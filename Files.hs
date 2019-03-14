----------------------------------------------------------------------------------------------------
---- Операции с именами файлов, манипуляции с файлами на диске, ввод/вывод.                     ----
----------------------------------------------------------------------------------------------------
module Files (module Files, module FilePath) where

import Prelude hiding (catch)
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.Char
import Data.List
import Foreign
import Foreign.C
import Foreign.Marshal.Alloc
import System.Directory
import System.Posix.Internals
import System.Posix.Types
import System.IO
import System.IO.Error
import System.IO.Unsafe
import System.Locale
import System.Time
import System.Process

import Utils
import FilePath
#if defined(FREEARC_WIN)
import Win32Files
import System.Win32.File
import System.Win32.Info
import System.Win32.Types
#endif

-- |Размер одного буфера, используемый в различных операциях
aBUFFER_SIZE = 64*kb

-- |Количество байт, которые должны читаться/записываться за один раз в быстрых методах и при распаковке асимметричных алгоритмов
aLARGE_BUFFER_SIZE = 256*kb

-- |Количество байт, которые должны читаться/записываться за один раз в очень быстрых методах (storing, tornado и тому подобное)
-- Этот объём минимизирует потери на disk seek operations - при условии, что одновременно не происходит в/в в другом потоке ;)
aHUGE_BUFFER_SIZE = 8*mb


----------------------------------------------------------------------------------------------------
---- Filename manipulations ------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |True, если file находится в каталоге `dir`, одном из его подкаталогов, или совпадает с ним
dir `isParentDirOf` file =
  case (startFrom dir file) of
    Just ""    -> True
    Just (x:_) -> isPathSeparator x
    Nothing    -> False

#if defined(FREEARC_WIN)
-- |Для case-insensitive файловых систем
filenameLower = strLower
#else
-- |Для case-sensitive файловых систем
filenameLower = id
#endif

-- |Return False for special filenames like "." and ".." - used to filtering results of getDirContents
exclude_special_names s  =  (s/=".")  &&  (s/="..")

-- Strip "drive:/" at the beginning of absolute filename
stripRoot = dropDrive

-- |Replace all '\' with '/'
translatePath = map (\c -> if isPathSeparator c  then '/'  else c)

-- |Filename extension, "dir/name.ext" -> "ext"
getFileSuffix = snd . splitFilenameSuffix

splitFilenameSuffix str  =  (name, drop 1 ext)
                               where (name, ext) = splitExtension str

-- "foo/bar/xyzzy.ext" -> ("foo/bar", "xyzzy.ext")
splitDirFilename :: String -> (String,String)
splitDirFilename str  =  case splitFileName str of
                           x@([d,':',s], name) -> x   -- оставляем ("c:\", name)
                           (dir, name)         -> (dropTrailingPathSeparator dir, name)

-- "foo/bar/xyzzy.ext" -> ("foo/bar", "xyzzy", "ext")
splitFilename3 :: String -> (String,String,String)
splitFilename3 str
   = let (dir, rest) = splitDirFilename str
         (name, ext) = splitFilenameSuffix rest
     in  (dir, name, ext)

-- | Modify the base name.
updateBaseName :: (String->String) -> FilePath -> FilePath
updateBaseName f pth  =  dir </> f name <.> ext
    where
          (dir, name, ext) = splitFilename3 pth


----------------------------------------------------------------------------------------------------
---- Поиск конфиг-файлов программы и SFX модулей ---------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Найти конфиг-файл с заданным именем или возвратить ""
findFile possibleFilePlaces cfgfilename = do
  found <- possibleFilePlaces cfgfilename >>= Utils.filterM fileExist
  case found of
    x:xs -> return x
    []   -> return ""


#if defined(FREEARC_WIN)
-- Под Windows все дополнительныйе файлы по умолчанию лежат в одном каталоге с программой
libraryFilePlaces = configFilePlaces
configFilePlaces filename  =  do exe <- getExeName
                                 return [takeDirectory exe </> filename]

-- |Имя исполняемого файла программы
getExeName = c_GetExeName >>= peekCWFilePath
foreign import ccall unsafe "Environment.h GetExeName"
  c_GetExeName :: IO CWFilePath

#else
-- |Места для поиска конфиг-файлов
configFilePlaces  filename  =  return ["/etc"           </> filename]

-- |Места для поиска sfx-модулей
libraryFilePlaces filename  =  return ["/usr/lib"       </> filename
                                      ,"/usr/local/lib" </> filename]
#endif


----------------------------------------------------------------------------------------------------
---- Запуск внешних программ -----------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Запустить команду через shell и возвратить её stdout
runProgram cmd = do
    (_, stdout, stderr, ph) <- runInteractiveCommand cmd
    forkIO (hGetContents stderr >>= evaluate.length >> return ())
    result <- hGetContents stdout
    evaluate (length result)
    waitForProcess ph
    return result


----------------------------------------------------------------------------------------------------
---- Операции с неоткрытыми файлами и каталогами ---------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Create a hierarchy of directories
createDirectoryHierarchy :: FilePath -> IO ()
createDirectoryHierarchy dir = do
  let d = stripRoot dir
  when (d/= "" && exclude_special_names d) $ do
    unlessM (dirExist dir) $ do
      createDirectoryHierarchy (takeDirectory dir)
      dirCreate dir

-- |Создать недостающие каталоги на пути к файлу
buildPathTo filename  =  createDirectoryHierarchy (takeDirectory filename)


#if defined(FREEARC_WIN)
-- |Clear file's Archive bit
clearArchiveBit filename = do
    attr <- getFileAttributes filename
    when (attr.&.fILE_ATTRIBUTE_ARCHIVE /= 0) $ do
        setFileAttributes filename (attr - fILE_ATTRIBUTE_ARCHIVE)
#else
clearArchiveBit _ = return ()
#endif


-- |Минимальное datetime, которое только может быть у файла. Соответствует 1 января 1970 г.
aMINIMAL_POSSIBLE_DATETIME = 0 :: CTime

-- |Get file's date/time
getFileDateTime filename  =  fileWithStatus "getFileDateTime" filename stat_mtime

-- |Set file's date/time
setFileDateTime filename datetime  =  withCFilePath filename (`c_SetFileDateTime` datetime)

foreign import ccall unsafe "Environment.h SetFileDateTime"
   c_SetFileDateTime :: CFilePath -> CTime -> IO ()

-- |Пребразование CTime в ClockTime. Используется информация о внутреннем представлении ClockTime в GHC!!!
convert_CTime_to_ClockTime ctime = TOD (realToInteger ctime) 0
  where realToInteger = round . realToFrac :: Real a => a -> Integer

-- |Пребразование ClockTime в CTime
convert_ClockTime_to_CTime (TOD secs _) = i secs

-- |Текстовое представление времени
showtime format t = formatCalendarTime defaultTimeLocale format (unsafePerformIO (toCalendarTime t))


----------------------------------------------------------------------------------------------------
---- Операции с открытыми файлами ------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

--withMVar  mvar action     =  bracket (takeMVar mvar) (putMVar mvar) action
liftMVar1  action mvar     =  withMVar mvar action
liftMVar2  action mvar x   =  withMVar mvar (\a -> action a x)
liftMVar3  action mvar x y =  withMVar mvar (\a -> action a x y)
returnMVar action          =  action >>= newMVar

-- |Архивный файл, заворачивается в MVar для реализации параллельного доступа из разных тредов ко входным архивам
data Archive = Archive { archiveName :: FilePath
                       , archiveFile :: MVar File
                       }
archiveOpen     name = do file <- fileOpen name >>= newMVar; return (Archive name file)
archiveCreate   name = do file <- fileCreate name >>= newMVar; return (Archive name file)
archiveCreateRW name = do file <- fileCreateRW name >>= newMVar; return (Archive name file)
archiveGetPos        = liftMVar1 fileGetPos   . archiveFile
archiveGetSize       = liftMVar1 fileGetSize  . archiveFile
archiveSeek          = liftMVar2 fileSeek     . archiveFile
archiveRead          = liftMVar2 fileRead     . archiveFile
archiveReadBuf       = liftMVar3 fileReadBuf  . archiveFile
archiveWrite         = liftMVar2 fileWrite    . archiveFile
archiveWriteBuf      = liftMVar3 fileWriteBuf . archiveFile
archiveClose         = liftMVar1 fileClose    . archiveFile

-- |Скопировать данные из одного архива в другой и затем восстановить позицию в исходном архиве
archiveCopyData srcarc pos size dstarc = do
  withMVar (archiveFile srcarc) $ \srcfile ->
    withMVar (archiveFile dstarc) $ \dstfile -> do
      restorePos <- fileGetPos srcfile
      fileSeek      srcfile pos
      fileCopyBytes srcfile size dstfile
      fileSeek      srcfile restorePos

-- |При работе с одним физическим диском (наиболее частый вариант)
-- нет смысла выполнять несколько I/O операций параллельно,
-- поэтому мы их все проводим через "угольное ушко" одной-единственной MVar
oneIOAtTime = unsafePerformIO$ newMVar "oneIOAtTime value"
fileReadBuf  file buf size = withMVar oneIOAtTime $ \_ -> fileReadBufSimple  file buf size
fileWriteBuf file buf size = withMVar oneIOAtTime $ \_ -> fileWriteBufSimple file buf size


----------------------------------------------------------------------------------------------------
---- URL access ------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

data File = FileOnDisk FileOnDisk | URL URL

fileOpen           = choose0 fOpen           url_open
fileCreate         = choose0 fCreate         (\_ -> err "url_create")
fileCreateRW       = choose0 fCreateRW       (\_ -> err "url_create_rw")
fileAppendText     = choose0 fAppendText     (\_ -> err "url_append_text")
fileGetPos         = choose  fGetPos         (url_pos  .>>==i)
fileGetSize        = choose  fGetSize        (url_size .>>==i)
fileSeek           = choose  fSeek           (\f p -> url_seek f (i p))
fileReadBufSimple  = choose  fReadBufSimple  url_read
fileWriteBufSimple = choose  fWriteBufSimple (\_ _ _ -> err "url_write")
fileFlush          = choose  fFlush          (\_     -> err "url_flush")
fileClose          = choose  fClose          url_close


isURL url = "://" `isInfixOf` url

{-# NOINLINE choose0 #-}
choose0 onfile onurl name | isURL name = do url <- withCString name onurl
                                            when (url==nullPtr) $ do
                                              fail$ "Can't open url "++name
                                            return (URL url)
                          | otherwise  = onfile name >>== FileOnDisk

choose _ onurl  (URL        url)   = onurl  url
choose onfile _ (FileOnDisk file)  = onfile file

{-# NOINLINE err #-}
err s  =  fail$ s++" isn't implemented" --registerError$ GENERAL_ERROR


type URL = Ptr ()
foreign import ccall safe "URL.h"  url_setup_proxy         :: Ptr CChar -> IO ()
foreign import ccall safe "URL.h"  url_setup_bypass_list   :: Ptr CChar -> IO ()
foreign import ccall safe "URL.h"  url_open   :: Ptr CChar -> IO URL
foreign import ccall safe "URL.h"  url_pos    :: URL -> IO Int64
foreign import ccall safe "URL.h"  url_size   :: URL -> IO Int64
foreign import ccall safe "URL.h"  url_seek   :: URL -> Int64 -> IO ()
foreign import ccall safe "URL.h"  url_read   :: URL -> Ptr a -> Int -> IO Int
foreign import ccall safe "URL.h"  url_close  :: URL -> IO ()


----------------------------------------------------------------------------------------------------
---- Под Windows мне пришлось реализовать библиотеку в/в самому для поддержки файлов >4Gb и Unicode имён файлов
----------------------------------------------------------------------------------------------------
#if defined(FREEARC_WIN)

type FileOnDisk      = FD
type CFilePath       = CWFilePath
type FileAttributes  = FileAttributeOrFlag
withCFilePath        = withCWFilePath
fOpen       name     = wopen name (read_flags  .|. o_BINARY) 0o666
fCreate     name     = wopen name (write_flags .|. o_BINARY .|. o_TRUNC) 0o666
fCreateRW   name     = wopen name (rw_flags    .|. o_BINARY .|. o_TRUNC) 0o666
fAppendText name     = wopen name (append_flags) 0o666
fGetPos              = wtell
fGetSize             = wfilelength
fSeek   file pos     = wseek file pos sEEK_SET
fReadBufSimple       = wread
fWriteBufSimple      = wwrite
fFlush  file         = return ()
fClose               = wclose
fileExist            = wDoesFileExist
fileRemove           = wunlink
fileRename           = wrename
fileWithStatus       = wWithFileStatus
fileStdin            = 0
stat_mode            = wst_mode
stat_size            = wst_size
stat_mtime           = wst_mtime
dirCreate            = wmkdir
dirExist             = wDoesDirectoryExist
dirRemove            = wrmdir

#else

type FileOnDisk      = Handle
type CFilePath       = CString
type FileAttributes  = Int
withCFilePath        = withCString   . str2filesystem
fOpen       name     = openBinaryFile (str2filesystem name) ReadMode
fCreate     name     = openBinaryFile (str2filesystem name) WriteMode
fCreateRW   name     = openBinaryFile (str2filesystem name) ReadWriteMode
fAppendText name     = openFile       (str2filesystem name) AppendMode
fGetPos              = hTell
fGetSize             = hFileSize
fSeek   file         = hSeek file AbsoluteSeek
fReadBufSimple       = hGetBuf
fWriteBufSimple      = hPutBuf
fFlush               = hFlush
fClose               = hClose
fileExist            = doesFileExist  . str2filesystem
fileRemove           = removeFile     . str2filesystem
fileRename a b       = renameFile      (str2filesystem a) (str2filesystem b)
fileSetSize          = hSetFileSize
fileStdin            = stdin
stat_mode            = st_mode
stat_size            = st_size  .>>== i
stat_mtime           = st_mtime
dirCreate            = createDirectory     . str2filesystem
dirExist             = doesDirectoryExist  . str2filesystem
dirRemove            = removeDirectory     . str2filesystem
dirList dir          = getDirectoryContents (str2filesystem dir)  >>==  map filesystem2str

-- kidnapped from System.Directory :)))
fileWithStatus :: String -> FilePath -> (Ptr CStat -> IO a) -> IO a
fileWithStatus loc name f = do
  modifyIOError (`ioeSetFileName` name) $
    allocaBytes sizeof_stat $ \p ->
      withCFilePath name $ \s -> do
        throwErrnoIfMinus1Retry_ loc (c_stat s p)
	f p

#endif

fileRead      file size = allocaBytes size $ \buf -> do fileReadBuf file buf size; peekCStringLen (buf,size)
fileWrite     file str  = withCStringLen str $ \(buf,size) -> fileWriteBuf file buf size
fileGetBinary name      = bracket (fileOpen   name) fileClose (\file -> fileGetSize file >>= fileRead file.i)
filePutBinary name str  = bracket (fileCreate name) fileClose (`fileWrite` str)

-- |Скопировать заданное количество байт из одного открытого файла в другой
fileCopyBytes srcfile size dstfile = do
  allocaBytes aHUGE_BUFFER_SIZE $ \buf -> do        -- используем `alloca`, чтобы автоматически освободить выделенный буфер при выходе
    doChunks size aHUGE_BUFFER_SIZE $ \bytes -> do  -- Скопировать size байт кусками по aHUGE_BUFFER_SIZE
      bytes <- fileReadBuf srcfile buf bytes        -- Проверим, что прочитано ровно столько байт, сколько затребовано
      fileWriteBuf dstfile buf bytes


---------------------------------------------------------------------------------------------------
---- Парсер опции командной строки -sc/--charset --------------------------------------------------
---------------------------------------------------------------------------------------------------

-- |Обработать список опций --charset/-sc, возвратив таблицу кодировок
-- и процедуры чтения/записи файлов с её учётом
parse_charset_option optionsList = (charsets, parseFile, unParseFile)
  where
    -- Таблица кодировок
    charsets = foldl f aCHARSET_DEFAULTS optionsList
    -- Функция обработки опций --charset
    f value "--"      =  aCHARSET_DEFAULTS      -- -sc-- означает восстановить значения по умолчанию
    f value ('s':cs)  =  _7zToRAR value "l" cs  -- -scs... устанавливает кодировку для листфайлов
    f value ('l':cs)  =  _7zToRAR value "l" cs  -- -scl... does the same
    f value ('c':cs)  =  _7zToRAR value "c" cs  -- -scs... устанавливает кодировку для комментфайлов
    f value ('f':cs)  =  _7zToRAR value "f" cs  -- -scf... устанавливает кодировку для файловой системы
    f value ('d':cs)  =  _7zToRAR value "d" cs  -- -scd... устанавливает кодировку для каталога архива
    f value ('t':cs)  =  _7zToRAR value "t" cs  -- -sct... устанавливает кодировку для терминала (консоли)
    f value ('p':cs)  =  _7zToRAR value "p" cs  -- -scp... устанавливает кодировку для параметров ком. строки
    f value ('i':cs)  =  _7zToRAR value "i" cs  -- -sci... устанавливает кодировку для ini-файлов (arc.ini/arc.groups)
    f value (x:cs)    =  foldl Utils.update value [(c,x) | c<-cs|||"cl"]  -- установить в `x` элементы списка, перечисленные в cs (по умолчанию 'c' и 'l')
    -- Вспомогательные функции, преобразующие 7zip-овский формат опций в RAR'овский
    _7zToRAR value typ cs  =  f value (g (strLower cs):typ)
    g "utf-8"  = '8';  g "win"  = 'a'
    g "utf8"   = '8';  g "ansi" = 'a'
    g "utf-16" = 'u';  g "dos"  = 'o'
    g "utf16"  = 'u';  g "oem"  = 'o'

    -- Процедура чтения файла, транслирующая его кодировку и разбивающая на отдельные строки
    parseFile domain file = fileGetBinary file
                                >>== aTRANSLATE_INPUT (translation charsets domain)
                                >>== linesCRLF

    -- Процедура записи файла, транслирующая данные в его кодировку
    unParseFile domain file = filePutBinary file
                                  . aTRANSLATE_OUTPUT (translation charsets domain)


-- |Разбиение на строки файла, ипользующего любое представление конца строки (CR, LF, CR+LF)
linesCRLF = recursive oneline  -- oneline "abc\n..." = ("abc","...")
              where oneline ('\r':'\n':s)  =  ("",'\xFEFF':s)
                    oneline ('\r':s)       =  ("",'\xFEFF':s)
                    oneline ('\n':s)       =  ("",'\xFEFF':s)
                    oneline ('\xFEFF':s)   =  oneline s
                    oneline (c:s)          =  (c:s0,s1)  where (s0,s1) = oneline s
                    oneline ""             =  ("","")


---------------------------------------------------------------------------------------------------
---- Поддержка различных кодировок для ввода/вывода -----------------------------------------------
---------------------------------------------------------------------------------------------------

-- |Транслировать входные данные из области domain (листфайлы, конфигфайлы, коммент-файлы...),
-- используя charset, заданный для неё в domainСharsets
translation domainCharsets domain  =  translators
  where charset     = lookup domain  domainCharsets `defaultVal` error ("Unknown charset domain "++quote [domain])
        translators = lookup charset aCHARSETS      `defaultVal` error ("Unknown charset "++quote [charset])

-- |Translate filename from internal to terminal encoding
str2terminal   = aTRANSLATE_OUTPUT (translation aCHARSET_DEFAULTS 't')
-- |Translate filename from filelist to internal encoding
filelist2str   = aTRANSLATE_INPUT  (translation aCHARSET_DEFAULTS 'l')
-- |Translate filename from cmdline to internal encoding
cmdline2str    = aTRANSLATE_INPUT  (translation aCHARSET_DEFAULTS 'p')
-- |Translate filename from filesystem to internal encoding
filesystem2str = aTRANSLATE_INPUT  (translation aCHARSET_DEFAULTS 'f')
-- |Translate filename from internal to filesystem encoding
str2filesystem = aTRANSLATE_OUTPUT (translation aCHARSET_DEFAULTS 'f')

-- Типы, используемые для представления domain и charset
type Domain  = Char
type Charset = Char

-- |Each charset is represented by pair of functions: input translation (byte sequence into Unicode String) and output translation
data TRANSLATION = TRANSLATION {aTRANSLATE_INPUT, aTRANSLATE_OUTPUT :: String->String}

-- |Character sets and functions to translate texts from/to these charsets
aCHARSETS = [ ('0', TRANSLATION id               id)
            , ('8', TRANSLATION utf8_to_unicode  unicode2utf8)
            , ('u', TRANSLATION utf16_to_unicode unicode2utf16)
            ] ++ aLOCAL_CHARSETS


#ifdef FREEARC_UNIX

aLOCAL_CHARSETS = []

-- |Default charsets for various domains
aCHARSET_DEFAULTS = [ ('f','8')  -- filenames in filesystem: UTF-8
                    , ('d','8')  -- filenames in archive directory: UTF-8
                    , ('l','8')  -- filelists: UTF-8
                    , ('c','8')  -- comment files: UTF-8
                    , ('t','8')  -- terminal: UTF-8
                    , ('p','8')  -- program arguments: UTF-8
                    , ('i','8')  -- ini/group files: UTF-8
                    ]

#else

-- |Windows-specific charsets
aLOCAL_CHARSETS = [ ('o', TRANSLATION oem2unicode  unicode2oem)
                  , ('a', TRANSLATION ansi2unicode unicode2ansi)
                  ]

-- |Default charsets for various domains
aCHARSET_DEFAULTS = [ ('f','u')  -- filenames in filesystem: UTF-16
                    , ('d','8')  -- filenames in archive directory: UTF-8
                    , ('l','o')  -- filelists: OEM
                    , ('c','o')  -- comment files: OEM
                    , ('t','o')  -- terminal: OEM
                    , ('p','a')  -- program arguments: ANSI
                    , ('i','o')  -- ini/group files: OEM
                    ]

---------------------------------------------------------------------------------------------------
---- Windows-specific codecs ----------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

-- |Преобразовать виндовые коды символов \r и \n в человеческий вид
iHateWindows = replace (chr 9834) '\r' . replace (chr 9689) '\n'

-- |Translate string from Unicode to OEM encoding
unicode2oem s =
  if all isAscii s
    then s
    else unsafePerformIO $ do
           withCWStringLen s $ \(wstr,len) -> do
             allocaBytes len $ \cstr -> do
               c_WideToOemBuff wstr cstr (i len)
               peekCStringLen (cstr,len)

-- |Translate string from OEM encoding to Unicode
oem2unicode s =
  if all isAscii s
    then s
    else iHateWindows $
         unsafePerformIO $ do
           withCStringLen s $ \(cstr,len) -> do
             allocaBytes (len*2) $ \wstr -> do
               c_OemToWideBuff cstr wstr (i len)
               peekCWStringLen (wstr,len)

-- |Translate string from Unicode to ANSI encoding
unicode2ansi s =
  if all isAscii s
    then s
    else unsafePerformIO $ do
           withCWStringLen s $ \(wstr,len) -> do
             allocaBytes len $ \cstr -> do
               c_WideToOemBuff wstr cstr (i len)
               c_OemToAnsiBuff cstr cstr (i len)
               peekCStringLen (cstr,len)

-- |Translate string from ANSI encoding to Unicode
ansi2unicode s =
  if all isAscii s
    then s
    else iHateWindows $
         unsafePerformIO $ do
           withCStringLen s $ \(cstr,len) -> do
             allocaBytes (len*2) $ \wstr -> do
               c_AnsiToOemBuff cstr cstr (i len)
               c_OemToWideBuff cstr wstr (i len)
               peekCWStringLen (wstr,len)

foreign import stdcall unsafe "winuser.h CharToOemBuffW"
  c_WideToOemBuff :: CWString -> CString -> DWORD -> IO Bool

foreign import stdcall unsafe "winuser.h OemToCharBuffW"
  c_OemToWideBuff :: CString -> CWString -> DWORD -> IO Bool

foreign import stdcall unsafe "winuser.h OemToCharBuffA"
  c_OemToAnsiBuff :: CString -> CString -> DWORD -> IO Bool

foreign import stdcall unsafe "winuser.h CharToOemBuffA"
  c_AnsiToOemBuff :: CString -> CString -> DWORD -> IO Bool

#endif


---------------------------------------------------------------------------------------------------
---- UTF-8, UTF-16 codecs -------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

-- |Translate string from UTF-16 encoding to Unicode
utf16_to_unicode = tryToSkip [chr 0xFEFF] . map chr . fromUTF16 . map ord
 where
  fromUTF16 (c1:c2:c3:c4:wcs)
    | 0xd8<=c2 && c2<=0xdb  &&  0xdc<=c4 && c4<=0xdf =
      ((c1+c2*256 - 0xd800)*0x400 + (c3+c4*256 - 0xdc00) + 0x10000) : fromUTF16 wcs
  fromUTF16 (c1:c2:wcs) = c1+c2*256 : fromUTF16 wcs
  fromUTF16 [] = []

-- |Translate string from Unicode to UTF-16 encoding
unicode2utf16 = map chr . foldr utf16Char [] . map ord
 where
  utf16Char c wcs
    | c < 0x10000 = c `mod` 256 : c `div` 256 : wcs
    | otherwise   = let c' = c - 0x10000 in
                    ((c' `div` 0x400) .&. 0xFF) :
                    (c' `div` 0x40000 + 0xd8) :
                    (c' .&. 0xFF) :
                    (((c' `mod` 0x400) `div` 256) + 0xdc) : wcs

-- |Translate string from UTF-8 encoding to Unicode
utf8_to_unicode s =
  if all isAscii s
    then s
    else (tryToSkip [chr 0xFEFF] . fromUTF' . map ord) s  where
            fromUTF' [] = []
            fromUTF' (all@(x:xs))
                | x<=0x7F = chr x : fromUTF' xs
                | x<=0xBF = err
                | x<=0xDF = twoBytes all
                | x<=0xEF = threeBytes all
                | x<=0xFF = fourBytes all
                | otherwise = err
            twoBytes (x1:x2:xs) = chr  ((((x1 .&. 0x1F) `shift` 6) .|.
                                          (x2 .&. 0x3F))):fromUTF' xs
            twoBytes _ = error "fromUTF: illegal two byte sequence"

            threeBytes (x1:x2:x3:xs) = chr ((((x1 .&. 0x0F) `shift` 12) .|.
                                             ((x2 .&. 0x3F) `shift` 6) .|.
                                              (x3 .&. 0x3F))):fromUTF' xs
            threeBytes _ = error "fromUTF: illegal three byte sequence"

            fourBytes (x1:x2:x3:x4:xs) = chr ((((x1 .&. 0x0F) `shift` 18) .|.
                                               ((x2 .&. 0x3F) `shift` 12) .|.
                                               ((x3 .&. 0x3F) `shift` 6) .|.
                                                (x4 .&. 0x3F))):fromUTF' xs
            fourBytes _ = error "fromUTF: illegal four byte sequence"

            err = error "fromUTF: illegal UTF-8 character"

-- |Translate string from Unicode to UTF-8 encoding
unicode2utf8 s =
  if all isAscii s
    then s
    else go s
      where go [] = []
            go (x:xs) | ord x<=0x007f = chr (ord x) : go xs
                      | ord x<=0x07ff = chr (0xC0 .|. ((ord x `shiftR` 6) .&. 0x1F)):
                                        chr (0x80 .|. ( ord x .&. 0x3F)):
                                        go xs
                      | ord x<=0xffff = chr (0xE0 .|. ((ord x `shiftR` 12) .&. 0x0F)):
                                        chr (0x80 .|. ((ord x `shiftR`  6) .&. 0x3F)):
                                        chr (0x80 .|. ( ord x .&. 0x3F)):
                                        go xs
                      | otherwise     = chr (0xF0 .|. ( ord x `shiftR` 18)) :
                                        chr (0x80 .|. ((ord x `shiftR` 12) .&. 0x3F)) :
                                        chr (0x80 .|. ((ord x `shiftR`  6) .&. 0x3F)) :
                                        chr (0x80 .|. ( ord x .&. 0x3F)) :
                                        go xs


---------------------------------------------------------------------------------------------------
---- Utility functions ----------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

foreign import ccall unsafe "string.h"
    memset :: Ptr a -> Int -> CSize -> IO ()

foreign import ccall unsafe "Environment.h memxor"
    memxor :: Ptr a -> Ptr a -> Int -> IO ()

