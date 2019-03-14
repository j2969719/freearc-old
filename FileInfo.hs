----------------------------------------------------------------------------------------------------
---- Получение и хранение информации о файлах, поиск файлов на диске.                           ----
----------------------------------------------------------------------------------------------------
module FileInfo where

import Prelude hiding (catch)
import Control.Exception
import Control.Monad
import Data.Char
import Data.HashTable as Hash
import Data.Int
import Data.IORef
import Data.List
import Data.Maybe
import Data.Word
import Foreign.C
import System.Directory
import System.IO.Unsafe
import System.Posix.Internals

import Utils
import Process
import Files
import Errors
#ifdef FREEARC_PACKED_STRINGS
import UTF8Z
#endif
#if defined(FREEARC_WIN)
import Win32Files
import System.Win32.File
#endif


----------------------------------------------------------------------------------------------------
---- Компактное представление имени файла ----------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Хранение имён файлов в компактном виде с представлением быстрого доступа
-- к имени каталога, имени файла без каталога и расширению файла
data PackedFilePath = PackedFilePath
  { fpPackedDirectory       :: !MyPackedString     -- Имя каталога
  , fpPackedBasename        :: !MyPackedString     -- Имя файла без каталога, но с расширением
  , fpLCExtension           :: !String             -- Расширение, переведённое в нижний регистр
  , fpHash   :: {-# UNPACK #-} !Int32              -- Хеш от имени файла
  , fpParent                :: !PackedFilePath     -- Структура PackedFilePath родительского каталога
  }
  | RootDir

instance Eq PackedFilePath where
  (==)  =  map2eq$ map3 (fpHash,fpPackedBasename,fpPackedDirectory)

#ifdef FREEARC_PACKED_STRINGS
-- Использование упакованных строк уменьшает расход памяти в 2 раза
type MyPackedString = PackedString
myPackStr           = packString
myUnpackStr         = unpackPS

-- |Заменяет повторения одинаковых расширений одной и той же строкой
packext ext = unsafePerformIO$ do
  found <- Hash.lookup extsHash ext
  case found of
    Nothing      -> do Hash.insert extsHash ext ext
                       return ext
    Just oldext  -> return oldext

extsHash = unsafePerformIO$ Hash.new (==) (filenameHash 0)

#else
type MyPackedString = String
myPackStr           = id
myUnpackStr         = id
packext             = id
#endif

fpDirectory  =  myUnpackStr.fpPackedDirectory
fpBasename   =  myUnpackStr.fpPackedBasename

-- |Виртуальное поле: полное имя файла, включая каталог и расширение
fpFullname fp  =  fpDirectory fp </> fpBasename fp

-- |Ускоренное вычисление упакованного полного имени
fpPackedFullname fp  =  if fpPackedDirectory fp == myPackStr ""
                          then fpPackedBasename fp
                          else myPackStr (fpFullname fp)


-- |Создание упакованного представления из имени файла
packFilePath parent fullname  =  packFilePath2 parent dir name
  where (dir,name) = splitDirFilename fullname

-- |Создание упакованного представления из имени каталога и имени файла без каталога
packFilePath2       parent dir        name  =  packFilePathPacked2 parent (myPackStr dir) name
packFilePathPacked2 parent packed_dir name  =  packFilePathPacked3 parent packed_dir name (packext$ filenameLower$ getFileSuffix name)

-- |Создание упакованного представления из имени каталога, имени файла без каталога и расширения
packFilePath3 parent dir name lcext              =  packFilePathPacked3 parent (myPackStr dir) name lcext
packFilePathPacked3 parent packed_dir name lcext =
  PackedFilePath { fpPackedDirectory    =  packed_dir
                 , fpPackedBasename     =  myPackStr name
                 , fpLCExtension        =  lcext
                 , fpHash               =  filenameHash (fpHash parent) name
                 , fpParent             =  parent
                 }

-- |Создать структуру для базового каталога при поиске файлов
packParentDirPath dir  =
  PackedFilePath { fpPackedDirectory    =  myPackStr ""   -- Чтобы не тратить зря время,
                 , fpPackedBasename     =  myPackStr dir  -- помещаем имя каталога целиком в Basename
                 , fpLCExtension        =  ""
                 , fpHash               =  filenameHash 0 (filter (not.isPathSeparator) dir)
                 , fpParent             =  RootDir
                 }

-- |Хеш по полному имени файла (без разделителей каталога!).
-- Для ускорения его вычисления используется `dirhash` - хеш имени каталога, содержащего файл,
-- и `basename` - имя файла без имени каталога
filenameHash {-dirhash basename-}  =  foldl (\h c -> h*37+i(ord c))

{-# INLINE filenameHash #-}


----------------------------------------------------------------------------------------------------
---- Сопоставление имён файлов с регулярными выражениями -------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Сопоставить имя файла с маской `filespec`.
-- Маски "*", "*.ext" или имя файла без каталога - обрабатываются особо
match_FP getName filespec =
  if filespec==reANY_FILE  then const True  else
    case (splitFilename3 filespec) of
      ("", "*", ext) -> match  (filenameLower ext)      . fpLCExtension
      ("", _,   _  ) -> match  (filenameLower filespec) . filenameLower . getName
      _              -> match  (filenameLower filespec) . filenameLower . fpFullname

-- |Соответствует ли путь к файлу `filepath` хоть одной из масок `filespecs`?
match_filespecs getName {-filespecs filepath-}  =  anyf . map (match_FP getName)

-- |Маска, которой соответствует любое имя файла
reANY_FILE = "*"


----------------------------------------------------------------------------------------------------
---- Информация о файле ----------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- Типы данных для..
type FileCount = Int              -- количества файлов
type FileSize  = Integer          -- размера файла или позиции чтения/записи в нём
type FileTime  = CTime            -- времени создания/модификации/чтения файла
type FileAttr  = FileAttributes   -- досовских атрибутов файла

-- |Структура, хранящая всю необходимую нам информацию о файле
data FileInfo = FileInfo
  { fiFilteredName         :: !PackedFilePath  -- Имя файла, сопоставляемое с указанными в командной строке
  , fiDiskName             :: !PackedFilePath  -- "Внешнее" имя файла - для чтения/записи файлов на диске
  , fiStoredName           :: !PackedFilePath  -- "Внутреннее" имя файла - сохраняемое в оглавлении архива
  , fiSize  :: {-# UNPACK #-} !FileSize        -- Размер файла (0 для каталогов)
  , fiTime  :: {-# UNPACK #-} !FileTime        -- Дата/время создания файла
  , fiAttr  :: {-# UNPACK #-} !FileAttr        -- Досовские атрибуты файла
  , fiIsDir :: {-# UNPACK #-} !Bool            -- Это каталог?
  }

-- |Преобразовать FileInfo в имя файла на диске
diskName     = fpFullname.fiDiskName
storedName   = fpFullname.fiStoredName
filteredName = fpFullname.fiFilteredName

-- |Специальные файлы (каталоги, симлинки и тому подобное) не требуют упаковки
fiSpecialFile = fiIsDir

-- |Создать структуру FileInfo для каталога с заданным именем
createParentDirFileInfo fiFilteredName fiDiskName fiStoredName =
  FileInfo { fiFilteredName  =  packParentDirPath fiFilteredName
           , fiDiskName      =  packParentDirPath fiDiskName
           , fiStoredName    =  packParentDirPath fiStoredName
           , fiSize          =  0
           , fiTime          =  0
           , fiAttr          =  0
           , fiIsDir         =  True
           }

-- |Перечитать информацию о файле после его открытия (на случай, если файл успел измениться)
rereadFileInfo fi file = do
  getFileInfo (fiFilteredName fi) (fiDiskName fi) (fiStoredName fi)

-- |Создать структуру FileInfo с информацией о заданном файле
getFileInfo fiFilteredName fiDiskName fiStoredName  =
    let filename = fpFullname fiDiskName in do
    fileWithStatus "getFileInfo" filename $ \p_stat -> do
      fiIsDir  <-  stat_mode  p_stat  >>==  s_isdir
      fiTime   <-  stat_mtime p_stat
      fiSize   <-  if fiIsDir then return 0
                              else stat_size p_stat
      return$ Just$ FileInfo fiFilteredName fiDiskName fiStoredName fiSize fiTime 0 fiIsDir
  `catch`
    \e -> do registerWarning$ CANT_GET_FILEINFO filename
             return Nothing  -- В случае ошибки при выполнении stat возвращаем Nothing

-- |Restore date/time/attrs saved in FileInfo structure
setFileDateTimeAttr filename fileinfo  =  setFileDateTime filename (fiTime fileinfo)

{-# NOINLINE getFileInfo #-}


----------------------------------------------------------------------------------------------------
---- Процесс поиска файлов на диске ----------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Настройки для процесса поиска файлов на диске
data FindFiles = FindFiles
    { ff_disk_eq_filtered   :: Bool
    , ff_stored_eq_filtered :: Bool
    , ff_recursive          :: Bool
    , ff_parent_or_root     :: (FileInfo -> FileInfo)
    , ff_accept_f           :: (FileInfo -> Bool)
    , ff_process_f          :: ([FileInfo] -> IO ())
    }


-- |Вернуть FileInfo файлов и каталогов (исключая "." и ".."), находящихся в каталоге `parent`
getDirectoryContents_FileInfo ff parent{-родительская структура FileInfo-} = do
  let -- Полное дисковое имя род. каталога
      diskDirName = fpFullname$ fiDiskName parent
      -- Упакованные строки с дисковым, фильтруемым и запоминаемым именем род. каталога
      -- Эти имена могут совпадать при отсутствии -ap/-dp, что позволяет нам экономить память в этих случаях
      packedDisk  = myPackStr diskDirName
      packedFiltered = if ff.$ff_disk_eq_filtered
                          then packedDisk
                          else myPackStr$ fpFullname$ fiFilteredName parent
      packedStored   = if ff.$ff_stored_eq_filtered
                          then packedFiltered
                          else myPackStr$ fpFullname$ fiStoredName   parent_or_root
      -- Выбрать parent или root в качестве родительской записи (последнее - только при -ep0)
      parent_or_root = (ff.$ff_parent_or_root) parent

      -- Вызвать функцию f, передав ей объекты фильтруемого, дискового и запомненного имени
      make_names f name = f (packFilePathPacked3 (fiFilteredName parent)          packedFiltered  name lcext)
                            (packFilePathPacked3 (fiDiskName     parent)          packedDisk      name lcext)
                            (packFilePathPacked3 (fiStoredName   parent_or_root)  packedStored    name lcext)
                          where lcext  =  packext$ filenameLower$ getFileSuffix name

#if !defined(FREEARC_WIN)
  (dirList (diskDirName|||".")) .$handleFindErrors diskDirName  -- Получим список файлов в каталоге, обрабатывая ошибки чтения каталога,
    >>== filter exclude_special_names                           -- Исключим из списка "." и ".."
    >>= (mapMaybeM $! make_names getFileInfo)                   -- Превратим имена файлов в структуры FileInfo и уберём из списка файлы, на которых споткнулся `stat`
#else
  withList $ \list -> do
    handleFindErrors diskDirName $ do
      wfindfiles (diskDirName </> "*") $ \find -> do
        name <- w_find_name find
        when (exclude_special_names name) $ do
          fiAttr  <- w_find_attrib     find
          fiSize  <- w_find_size       find
          fiTime  <- w_find_time_write find
          fiIsDir <- w_find_isDir      find
          (list <<=) $! make_names FileInfo name fiSize fiTime fiAttr fiIsDir
#endif


-- |Добавить exception handler, вызываемый при ошибках получения списка файлов в каталоге
handleFindErrors dir =
  handleJust ioErrors $ \e -> do
    -- Сообщение об ошибке не печатается для каталогов "/System Volume Information"
    d <- canonicalizePath dir
    unless (stripRoot d `strLowerEq` "System Volume Information") $ do
      registerWarning$ CANT_READ_DIRECTORY dir
    return defaultValue

-- |Создать список файлов в `dir`, удовлетворяющих `accept_f` и отослать результат в `process_f`.
-- Если recursive==True - повторить эти действия рекурсивно в каждом найденном подкаталоге
findFiles_FileInfo dir ff@FindFiles{ff_accept_f=accept_f, ff_process_f=process_f, ff_recursive=recursive} = do
  if recursive  then recursiveM processDir dir  else do processDir dir; return ()
    where processDir dir = do
            dirContents  <-  getDirectoryContents_FileInfo ff dir
            process_f `unlessNull` (filter accept_f dirContents)   -- Обработать отфильтрованные файлы, если их список непуст
            return                 (filter fiIsDir  dirContents)   -- Возвратить список подкаталогов для рекурсивной обработки

{-# NOINLINE getDirectoryContents_FileInfo #-}
{-# NOINLINE findFiles_FileInfo #-}


----------------------------------------------------------------------------------------------------
---- Поиск и обработка файлов, удовлетворяющих заданным критериям ----------------------------------
----------------------------------------------------------------------------------------------------

-- |Условия поиска файлов на диске
data FileFind = FileFind
    { ff_ep             :: Int
    , ff_scan_subdirs   :: Bool
    , ff_include_dirs   :: Maybe Bool
    , ff_no_nst_filters :: Bool
    , ff_filter_f       :: FileInfo -> Bool
    , ff_arc_basedir    :: String
    , ff_disk_basedir   :: String
    }

-- |Найти [рекурсивно] все файлы, удовлетворяющие маске `filespec`, и вернуть их список
find_files scan_subdirs filespec  =  find_and_filter_files [filespec] $
    FileFind { ff_ep             = -1
             , ff_scan_subdirs   = scan_subdirs
             , ff_include_dirs   = Just False
             , ff_no_nst_filters = True
             , ff_filter_f       = const True
             , ff_arc_basedir    = ""
             , ff_disk_basedir   = ""
             }

-- |Найти все файлы, удовлетворяющие критерию отбора `ff`,
-- и вернуть их список
find_and_filter_files filespecs ff = do
  concat ==<< withList   -- Сконкатенировать списки файлов, найденных в каждом подкаталоге
    (\list -> find_filter_and_process_files filespecs (list<<=) ff)

-- |Найти все файлы, удовлетворяющие критерию отбора `ff`,
-- и послать их список по частям в выходной канал процесса
find_and_filter_files_PROCESS filespecs ff pipe = do
  find_filter_and_process_files filespecs (sendP pipe) ff
  sendP pipe []  -- сигнал "А кино-то уже кончилось!" :)


-- |Найти [рекурсивно] все файлы, описываемые масками `filespecs` и критерием отбора `filter_f`,
-- и выполнить над каждым списком файлов, найденных в одном каталоге, операцию `process_f`
find_filter_and_process_files filespecs process_f ff@FileFind{ ff_ep=ep, ff_scan_subdirs=scan_subdirs, ff_include_dirs=include_dirs, ff_filter_f=filter_f, ff_arc_basedir=arc_basedir, ff_disk_basedir=disk_basedir, ff_no_nst_filters=no_nst_filters}

  -- Сгруппировать маски по имени каталога, и обработать каждую из этих групп отдельно
  = do curdir  <-  getCurrentDirectory >>== translatePath
       mapM_ (find_files_in_one_dir curdir) $ sort_and_groupOn (filenameLower.takeDirectory) filespecs where

    -- Обработать группу масок, относящихся к одному каталогу
    find_files_in_one_dir curdir filespecs = do
      findFiles_FileInfo root FindFiles{ff_process_f=process_f, ff_recursive=recursive, ff_disk_eq_filtered=disk_eq_filtered, ff_stored_eq_filtered=stored_eq_filtered, ff_parent_or_root=parent_or_root, ff_accept_f=accept_f}

      where dirname  =  takeDirectory (head filespecs)  -- Общий для всех масок каталог
            masks    =  map takeFileName filespecs      -- Маски без этого имени каталога
            root     =  createParentDirFileInfo         -- Базовый FileInfo для этого поиска:
                            dirname                     --   базовый каталог для фильтрации файлов
                            diskdir                     --   базовый каталог на диске
                            arcdir                      --   базовый каталог в архиве

            -- Базовый каталог на диске
            diskdir       =  disk_basedir </> dirname
            -- Имена файлов на диске и в ком. строке совпадают?
            disk_eq_filtered  =  diskdir==dirname
            -- Полный путь к базовому каталогу на диске для -ep2/-ep3
            full_dirname  =  curdir </> diskdir

            -- Базовый каталог в архиве
            arcdir  =  arc_basedir </> case ep of
               0 -> ""                        -- -ep:  exclude any paths from names
               1 -> ""                        -- -ep1: exclude base dir from names
               2 -> full_dirname.$stripRoot   -- -ep2: full absolute path without "d:\"
               3 -> full_dirname              -- -ep3: full absolute path with "d:\"
               _ -> dirname.$stripRoot        -- Default: full relative path
            -- Выбирает parent или root каталог в зависимости от опции -ep
            parent_or_root      =  if ep==0  then const root  else id
            -- Имена файлов внутри архива и в ком. строке совпадают?
            stored_eq_filtered  =  arcdir==dirname && ep/=0

            -- Одно из имён указано как "dir/"?
            dir_slash    =  dirname>"" && masks `contains` ""
            -- Сканировать подкаталоги если указана опция "-r" или одно из имён указано как "dir/"
            recursive    =  scan_subdirs || dir_slash
            -- Включить в список все файлы/каталоги, если одно из имён указано как "dir/" или "*" или "dir/*"
            include_all  =  dir_slash || masks `contains` reANY_FILE
            -- Предикат, определяющий какие файлы и каталоги будут включены в создаваемый список:
            --   для каталогов это зависит от опций --[no]dirs, by default - при условии "[dir/]* -r" || "dir/" и отсутствии фильтров отбора файлов -n/-s../-t..
            --   для файлов проверяется соответствие предикату `filter_f` и одной из масок
            accept_f fi | fiIsDir fi  =  include_dirs `defaultVal` (no_nst_filters && recursive && include_all)
                        | otherwise   =  filter_f fi && (include_all || match_filespecs fpBasename masks (fiFilteredName fi))

{-# NOINLINE find_files #-}
{-# NOINLINE find_and_filter_files #-}
{-# NOINLINE find_and_filter_files_PROCESS #-}
{-# NOINLINE find_filter_and_process_files #-}
