---------------------------------------------------------------------------------------------------
---- Превращение командной строки в набор команд/опций на выполнение.                          ----
---------------------------------------------------------------------------------------------------
module Cmdline where

import Prelude hiding (catch)
import Control.Exception
import Control.Monad
import Control.Concurrent
import Data.Bits
import Data.Char
import Data.IORef
import Data.List
import Data.Maybe
import Foreign.C
import Foreign.C.Types
import System.Environment
import System.IO.Unsafe
import System.Time

import CompressionLib hiding (b,kb,mb,gb)
import Utils
import Files
import Errors
import FileInfo
import Compression
#if defined(FREEARC_WIN)
import System.Win32.File
#endif


-- |Описание выполняемой команды
data Command = Command {
    cmd_command_text         :: !String             -- Полный текст команды
  , cmd_additional_args      :: !String             -- Дополнительные опции, прочитанные из переменной среды и конфиг-файла
  , cmd_name                 :: !String             -- Название команды
  , cmd_arcspec              ::  String             -- Маска архивов
  , cmd_arclist              ::  [FilePath]         --   Имена всех найденных по этой маске (и возможно, рекурсивно) архивов
  , cmd_arcname              ::  FilePath           --   Имя обрабатываемого архива (из одной команды с wildcards в cmd_arcspec формируется несколько команд с конкретным именем обрабатываемого архива)
  , cmd_archive_filter       :: (FileInfo -> Bool)  -- Предикат отбора файлов из существующих архивов
  , cmd_filespecs            :: ![String]           -- Спецификации добавляемых архивов или файлов
  , cmd_added_arcnames       :: !(IO [FilePath])    --   Вычисление, возвращающее имена добавляемых архивов (команда "j")
  , cmd_diskfiles            :: !(IO [FileInfo])    --   Вычисление, возвращающее имена добавляемых файлов  (остальные команды создания/обновления архивов)
  , cmd_subcommand           :: !Bool               -- Подкоманда? (например, тестирование после архивации)
  , cmd_setup_command        :: !(IO ())            -- Действия, которые надо выполнить непосредственно перед началом отработки этой команды (один раз на все архивы)
                                                    -- Опции:
  , opt_scan_subdirs         :: !Bool               --   рекурсивный поиск файлов?
  , opt_add_dir              :: !Bool               --   добавить имя архива к имени каталога, куда происходит распаковка?
  , opt_add_exclude_path     :: !Int                --   исключить имя базового каталога / сохранить абсолютный путь (при ПОИСКЕ архивируемых файлов на диске)
  , opt_dir_exclude_path     :: !Int                --   исключить имя базового каталога / сохранить абсолютный путь (при чтении КАТАЛОГА АРХИВА)
  , opt_arc_basedir          :: !String             --   базовый каталог внутри архива
  , opt_disk_basedir         :: !String             --   базовый каталог на диске
  , opt_group_dir            :: ![Grouping]         --   группировка файлов для каталога архива
  , opt_group_data           :: ![Grouping]         --   группировка файлов для солид-блока
  , opt_data_compressor      :: !UserCompressor     --   методы сжатия для данных
  , opt_dir_compressor       :: !Compressor         --   метод сжатия для блоков каталога
  , opt_arccmt_file          :: !String             --   файл, из которого читается (в который пишется) комментарий к архиву
  , opt_include_dirs         :: !(Maybe Bool)       --   включить каталоги в обработку? (Да/Нет/По обстоятельствам)
  , opt_indicator            :: !String             --   тип индикатора прогресса ("0" - отсутствует, "1" - индикатор по умолчанию, "2" - вывод в отдельной строке имени каждого обрабатываемого файла)
  , opt_display              :: !String             --   внутренняя опция, описывающая какие строки выводить на экран
  , opt_overwrite            :: !(IORef String)     --   состояние запроса к пользователю о перезаписи файлов ("a" - перезаписывать все, "s" - пропускать все, любые другие - задавать вопросы)
  , opt_sfx                  :: !String             --   имя SFX-модуля, который надо присоединить к архиву ("-" - отсоединить, если уже есть, "--" - скопировать существующий)
  , opt_keep_time            :: !Bool               --   сохранить mtime архива после обновления содержимого?
  , opt_time_to_last         :: !Bool               --   установить mtime архива на mtime самого свежего файла в нём?
  , opt_keep_broken          :: !Bool               --   не удалять файлы, распакованные с ошибками?
  , opt_test                 :: !Bool               --   протестировать архив после упаковки?
  , opt_pretest              :: !Int                --   режим тестирования архивов _перед_ выполнением операции (0 - нет, 1 - только recovery info, 2 - recovery или full, 3 - full testing)
  , opt_lock_archive         :: !Bool               --   закрыть создаваемый архив от дальнейших изменений?
  , opt_match_with           :: !(PackedFilePath -> FilePath)  -- сопоставлять при фильтрации маски с fpBasename или fpFullname
  , opt_append               :: !Bool               --   добавлять новые файлы только в конец архива?
  , opt_recompress           :: !Bool               --   принудительно перепаковать все файлы?
  , opt_keep_original        :: !Bool               --   не перепаковывать ни одного файла?
  , opt_noarcext             :: !Bool               --   не добавлять стандартное расширение к имени архива?
  , opt_nodir                :: !Bool               --   не записывать в архив его оглавление (для бенчмарков)?
  , opt_update_type          :: !Char               --   алгоритм обновления файлов (a/f/u/s)
  , opt_x_include_dirs       :: !Bool               --   включить каталоги в обработку (для команд листинга/распаковки)?
  , opt_no_nst_filters       :: !Bool               --   TRUE, если в команде отсутствуют опции отбора файлов по имени/размеру/времени (-n/-s../-t..)
  , opt_file_filter          :: !(FileInfo -> Bool) --   сформированный опциями предикат отбора файлов по атрибутам/размеру/времени/имени (всё, кроме filespecs)
  , opt_sort_order           :: !String             --   порядок сортировки файлов в архиве
  , opt_reorder              :: !Bool               --   переупорядочить файлы после сортировки (поместив рядом одинаковые/близкие файлы)?
  , opt_find_group           :: !(FileInfo -> Int)  --   функция, определяющая по FileInfo к какой группе (из arc.groups) относится данный файл
  , opt_groups_count         :: !Int                --   количество групп (`opt_find_group` возвращает результаты в диапазоне 0..opt_groups_count-1)
  , opt_find_type            :: !(FileInfo -> Int)  --   функция, определяющая по FileInfo к какому типу данных (из перечисленных в `opt_data_compressor`) относится данный файл
  , opt_types_count          :: !Int                --   количество типов файлов (`opt_find_type` возвращает результаты в диапазоне 0..opt_types_count-1)
  , opt_logfile              :: !String             --   имя лог-файла или ""
  , opt_delete_files         :: !DelOptions         --   удалить файлы/каталоги после успешной архивации?
  , opt_workdir              :: !String             --   каталог для временных файлов или ""
  , opt_clear_archive_bit    :: !Bool               --   сбросить атрибут Archive у успешно упакованных файлов (и файлов, которые уже есть в архиве)
  , opt_recovery             :: !String             --   величина Recovery блока (в процентах, байтах или секторах)
  , opt_broken_archive       :: !String             --   обрабатывать неисправный архив, полностью сканируя его в поисках оставшихся исправными блоков
  , opt_original             :: !String             --   перезагружать с указанного URL сбойные части архива
  , opt_save_bad_ranges      :: !String             --   записать в заданный файл список неисправных частей архива для их перевыкачки
  , opt_cache                :: !Int                --   размер буфера упреждающего чтения
  , opt_limit_compression_memory   :: !MemSize      --   ограничение памяти при упаковке, байт
  , opt_limit_decompression_memory :: !MemSize      --   ограничение памяти при распаковке, байт

                                                    -- Настройки шифрования:
  , opt_encryption_algorithm :: !String             --   алгоритм шифрования
  , opt_cook_passwords                              --   подготавливает команду к использованию шифрования, заправшивая у пользователя пароль и считывая keyfile (не должно выполняться прежде, чем начнётся выполнение самой команды, поэтому не может быть выполнено в parseCmdline)
                             :: !(Command -> (IO String, IO String) -> IO Command)
  , opt_data_password        :: String              --   пароль, используемый для шифрования данных (включает в себя ввод с клавиатуры и содержимое keyfiles). "" - паролирование не нужно
  , opt_headers_password     :: String              --   пароль, используемый для шифрования заголовков (ditto)
  , opt_decryption_info                             --   информация, используемая процедурой подбора ключа дешифрации:
                             :: ( Bool              --     не запрашивать у пользователя новый пароль, даже если все известные для распаковки данных не подходят?
                                , MVar [String]     --     список "старых паролей", которыми мы пытаемся расшифровать распаковываемые данные
                                , [String]          --     содержимое keyfiles, добавляемых к паролям
                                , IO String)        --     ask_decryption_password

  -- Операции чтения/записи файлов в кодировке, настраиваемый опцией -sc
  , opt_parseFile   :: !(Domain -> FilePath -> IO [String])      -- процедура парсинга файла с настраиваемой в -sc кодировкой и ОС-независимым разбиением на строки
  , opt_unParseFile :: !(Domain -> FilePath -> String -> IO ())  -- процедура записи файла с настраиваемой в -sc кодировкой
  }

-- |Виртуальная опция --debug
opt_debug cmd = cmd.$opt_display.$(`contains_one_of` "$#")


-- |Список опций, поддерживаемых программой
optionsList = sortOn (\(OPTION a b _) -> (a|||"zzz",b))
   [OPTION "--"    ""                   "stop processing options"
   ,OPTION "cfg"   "config"            ("use config FILE (default: " ++ aCONFIG_FILE ++ ")")
   ,OPTION "env"   ""                  ("read default options from environment VAR (default: " ++ aCONFIG_ENV_VAR ++ ")")
   ,OPTION "r"     "recursive"          "recursively collect files"
   ,OPTION "f"     "freshen"            "freshen files"
   ,OPTION "u"     "update"             "update files"
   ,OPTION ""      "sync"               "synchronize archive and disk contents"
   ,OPTION "o"     "overwrite"          "existing files overwrite MODE (+/-/p)"
   ,OPTION "y"     "yes"                "answer Yes to all queries"
   ,OPTION "x"     "exclude"            "exclude FILESPECS from operation"
   ,OPTION "n"     "include"            "include only files matching FILESPECS"
   ,OPTION "ep"    "ExcludePath"        "Exclude/expand path MODE"
   ,OPTION "ap"    "arcpath"            "base DIR in archive"
   ,OPTION "dp"    "diskpath"           "base DIR on disk"
   ,OPTION "m"     "method"             "compression METHOD"
   ,OPTION "dm"    "dirmethod"          "compression METHOD for archive directory"
   ,OPTION "md"    "dictionary"         "set compression dictionary to N bytes"
   ,OPTION "mm"    "multimedia"         "set multimedia compression to MODE"
   ,OPTION "ms"    "StoreCompressed"    "store already compressed files"
   ,OPTION "mt"    "MultiThreaded"      "number of compression THREADS"
   ,OPTION "mc"    ""                   "disable compression algorithms (-mcd-, -mc-rep...)"
   ,OPTION "mx"    ""                   "maximum internal compression mode"
   ,OPTION "max"   ""                   "maximum compression mode involving external compressors"
   ,OPTION "ds"    "sort"               "sort files in ORDER"                      -- to do: сделать эту опцию OptArg
   ,OPTION ""      "groups"             "name of groups FILE"                      -- to do: сделать эту опцию OptArg
   ,OPTION "s"     "solid"              "GROUPING for solid compression"           -- to do: сделать эту опцию OptArg
   ,OPTION "p"     "password"           "encrypt/decrypt compressed data using PASSWORD"
   ,OPTION "hp"    "HeadersPassword"    "encrypt/decrypt archive headers and data using PASSWORD"
   ,OPTION "ae"    "encryption"         "encryption ALGORITHM (aes, blowfish, serpent, twofish)"
   ,OPTION "kf"    "keyfile"            "encrypt/decrypt using KEYFILE"
   ,OPTION "op"    "OldPassword"        "old PASSWORD used only for decryption"
   ,OPTION "okf"   "OldKeyfile"         "old KEYFILE used only for decryption"
   ,OPTION "w"     "workdir"            "DIRECTORY for temporary files"
   ,OPTION "sc"    "charset"            "CHARSETS used for listfiles and comment files"
   ,OPTION "tp"    "pretest"            "test archive before operation using MODE"
   ,OPTION "t"     "test"               "test archive after operation"
   ,OPTION "d"     "delete"             "delete files & dirs after successful archiving"
   ,OPTION "df"    "delfiles"           "delete only files after successful archiving"
   ,OPTION "kb"    "keepbroken"         "keep broken extracted files"
   ,OPTION "ba"    "BrokenArchive"      "deal with badly broken archive using MODE"
#if defined(FREEARC_WIN)
   ,OPTION "ac"    "ClearArchiveBit"    "clear Archive bit on files succesfully (de)archived"
   ,OPTION "ao"    "SelectArchiveBit"   "select only files with Archive bit set"
#endif
   ,OPTION "sm"    "SizeMore"           "select files larger than SIZE"
   ,OPTION "sl"    "SizeLess"           "select files smaller than SIZE"
   ,OPTION "tb"    "TimeBefore"         "select files modified before specified TIME"
   ,OPTION "ta"    "TimeAfter"          "select files modified after specified TIME"
   ,OPTION "tn"    "TimeNewer"          "select files newer than specified time PERIOD"
   ,OPTION "to"    "TimeOlder"          "select files older than specified time PERIOD"
   ,OPTION "k"     "lock"               "lock archive"
   ,OPTION "rr"    "recovery"           "add recovery information of specified SIZE to archive"
   ,OPTION "sfx"   ""                  ("add sfx MODULE (\""++aDEFAULT_SFX++"\" by default)")  -- to do: сделать эту опцию OptArg
   ,OPTION "z"     "arccmt"             "read archive comment from FILE or stdin"  -- to do: сделать эту опцию OptArg
   ,OPTION "i"     "indicator"          "select progress indicator TYPE (0/1/2)"   -- to do: сделать эту опцию OptArg
   ,OPTION "ad"    "adddir"             "add arcname to extraction path"
   ,OPTION "ag"    "autogenerate"       "autogenerate archive name with FMT"       -- to do: сделать эту опцию OptArg
   ,OPTION ""      "noarcext"           "don't add default extension to archive name"
   ,OPTION "tk"    "keeptime"           "keep original archive time"
   ,OPTION "tl"    "timetolast"         "set archive time to latest file"
   ,OPTION "fn"    "fullnames"          "match with full names"
   ,OPTION ""      "append"             "add new files to the end of archive"
   ,OPTION ""      "recompress"         "recompress archive contents"
   ,OPTION ""      "dirs"               "add empty dirs to archive"
   ,OPTION "ed"    "nodirs"             "don't add empty dirs to archive"
   ,OPTION ""      "cache"              "use N bytes for read-ahead cache"
   ,OPTION "lc"    "LimitCompMem"       "limit memory usage for compression to N bytes"
   ,OPTION "ld"    "LimitDecompMem"     "limit memory usage for decompression to N bytes"
   ,OPTION ""      "nodir"              "don't write archive directories"
   ,OPTION ""      "nodata"             "don't store data in archive"
   ,OPTION ""      "crconly"            "save/check CRC, but don't store data"
   ,OPTION "di"    "display"           ("control AMOUNT of information displayed: ["++aDISPLAY_ALL++"]*")
   ,OPTION ""      "logfile"            "duplicate all information displayed to this FILE"
   ,OPTION ""      "print-config"       "display built-in definitions of compression methods"
   ,OPTION ""      "proxy"              "setups proxy(s) for URL access"
   ,OPTION ""      "bypass"             "setups proxy bypass list for URL access"
   ,OPTION ""      "original"           "redownload broken parts of archive from the URL"
   ,OPTION ""      "save-bad-ranges"    "save list of broken archive parts to the FILE"
   ]

-- |Список опций, которым надо отдавать предпочтение при возникновении коллизий в разборе командной строки
aPREFFERED_OPTIONS = words "method sfx charset SizeMore SizeLess overwrite"

-- |Опции из предыдущего списка, имеющий максимальный приоритет :)
aSUPER_PREFFERED_OPTIONS = words "OldKeyfile"


-- |Описание команд, поддерживаемых программой
commandsList = [
    "a        add files to archive"
  , "c        add comment to archive"
  , "ch       modify archive (recompress, encrypt and so on)"
  , "create   create new archive"
  , "cw       write archive comment to file"
  , "d        delete files from archive"
  , "e        extract files from archive ignoring pathnames"
  , "f        freshen archive"
  , "j        join archives"
  , "k        lock archive"
  , "l        list files in archive"
  , "lb       bare list of files in archive"
  , "m        move files and dirs to archive"
  , "mf       move files to archive"
  , "r        recover archive using recovery record"
  , "rr       add recovery record to archive"
  , "s        convert archive to SFX"
  , "t        test archive integrity"
  , "u        update files in archive"
  , "v        verbosely list files in archive"
  , "x        extract files from archive"
  ]

-- |Список команд, поддерживаемых программой
aLL_COMMANDS = map (head.words) commandsList

-- |Список команд, которые просто копируют архив
cOPYING_COMMANDS = words "ch s k c rr"

-- |Классификация всех команд по четырём типам: команды упаковки, распаковки, тестирования и листинга
data CmdType = ADD_CMD | EXTRACT_CMD | TEST_CMD | LIST_CMD | RECOVER_CMD  deriving (Eq)
cmdType "t"  = TEST_CMD
cmdType "e"  = EXTRACT_CMD
cmdType "x"  = EXTRACT_CMD
cmdType "cw" = EXTRACT_CMD
cmdType "l"  = LIST_CMD
cmdType "lb" = LIST_CMD
cmdType "v"  = LIST_CMD
cmdType "r"  = RECOVER_CMD
cmdType  _   = ADD_CMD
{-# NOINLINE cmdType #-}

-- |Версия архиватора, записываемая в HEADER BLOCK
aARCHIVE_VERSION = make4byte 0 0 4 0

-- |Краткое наименование программы, выводимое в начале работы
aARC_HEADER = "ARC 0.40 "
{-# NOINLINE aARC_HEADER #-}

-- |HELP, выводимый при вызове программы без параметров
aHELP = aARC_HEADER++" http://freearc.sourceforge.net  Jan 1 2008\n"++
        "Usage: Arc command [options...] archive [files... @listfiles...]\n" ++
        joinWith "\n  " ("Commands:":commandsList) ++ "\nOptions:\n" ++ optionsHelp

-- |Способы группировки файлов для солид-блока или оглавления архива
data Grouping = GroupNone                   -- каждый файл отдельно
                                            -- группировка по:
              | GroupByExt                  --   одинаковому расширению
              | GroupBySize      FileSize   --   минимальному объёму блока данных
              | GroupByBlockSize MemSize    --   максимальному объёму блока данных (для блочно-ориентированных алгоритмов, таких как BWT и ST)
              | GroupByNumber    FileCount  --   количеству файлов
              | GroupAll                    -- все файлы вместе

-- |Значение опции -d[f]: не удалять, удалять только файлы, удалять файлы и каталоги
data DelOptions = NO_DELETE | DEL_FILES | DEL_FILES_AND_DIRS  deriving (Eq)


---------------------------------------------------------------------------------------------------
-- ЗНАЧЕНИЯ, ИСПОЛЬЗУЕМЫЕ ПО УМОЛЧАНИЮ ------------------------------------------------------------
---------------------------------------------------------------------------------------------------

-- |Метод сжатия данных
aDEFAULT_COMPRESSOR = "4"

-- |Метод сжатия каталога архива
aDEFAULT_DIR_COMPRESSION = "lzma:1m"

-- |Размер солид-блоков
aDEFAULT_DATA_GROUPING  =  "1gb"

-- |Группировка для каталогов
aDEFAULT_DIR_GROUPING  =  GroupByNumber (20*1000)

-- |Алгоритм сжатия данных, используемый по умолчанию
aDEFAULT_ENCRYPTION_ALGORITHM = "aes"

-- |Если в командной строке не указаны имена обрабатываемых файлов - обрабатывать все, т.е. "*"
aDEFAULT_FILESPECS = [reANY_FILE]

-- |Расширение архивных файлов
aDEFAULT_ARC_EXTENSION = ".arc"

-- |Расширение SFX архивных файлов
aDEFAULT_SFX_EXTENSION = ".exe"

-- |Файл с описанием порядка сортировки имён файлов при "-og"
aDEFAULT_GROUPS_FILE = "arc.groups"

-- |SFX-модуль, используемый по умолчанию
aDEFAULT_SFX = "arc.sfx"

-- |Файл конфигурации (хранящий опции, используемые по умолчанию)
aCONFIG_FILE = "arc.ini"

-- |Переменная среды, содержащая опции, используемые по умолчанию
aCONFIG_ENV_VAR = "FREEARC"

-- |Порядок сортировки, используемый при solid-сжатии (для увеличения сжатия)
aDEFAULT_SOLID_SORT_ORDER = "gerpn"

-- |Объём информации, выводимой на экран - по умолчанию и при использовании опции "--display" без параметра.
-- По умолчанию на экран не выводятся "cmo" - доп. опции, режим сжатия и используемая память
aDISPLAY_DEFAULT = "hanwrftsek"
aDISPLAY_ALL     = "hoacmnwrfdtsek"

-- Секции arc.ini
compressionMethods = "[Compression methods]"
defaultOptions     = "[Default options]"
externalCompressor = "[External compressor:*]"

-- |Приведение имени секции к стандартизованной форме
cleanupSectionName  =  strLower . filter (not.isSpace)

-- |Проверка того, что это - заголовок секции
selectSectionHeadings  =  ("["==) . take 1 . trim


-- |Разбирает командную строку и возвращает список заданных в ней команд в виде структур Command.
-- Каждая команда содержит имя команды, спецификацию архивов, список спецификаций файлов и опции.
-- Команды разделяются " ; ", например "a archive -r ; t archive ; x archive"
parseCmdline cmdline  =  (`mapMaybeM` split ";" (map cmdline2str cmdline)) $ \args -> do
  display_option' =: aDISPLAY_DEFAULT  -- установить display_option в значение по умолчанию, поскольку другого случая может не представиться
  let options = takeWhile (/="--") $ filter (match "-*") args
  -- Если командная строка не содержит ничего кроме опций - напечатать help/конфигурацию и выйти
  if args==options then do
      putStr $ if options `contains` "--print-config"
                 then unlines ("":";You can insert these lines into ARC.INI":compressionMethods:builtinMethodSubsts)
                 else aHELP
      return Nothing
    else do

  -- Прочитаем опции из переменной среды FREEARC или заданной в опции -env
  (o0, _) <- parseOptions options [] []
  let no_configs = findReqList o0 "config" `contains` "-"
  env_options <- case (findReqArg o0 "env" "--") of
                    "--" | no_configs -> return ""  -- Опция -cfg- в командной строке отключает использование И arc.ini, И %FREEARC
                         | otherwise  -> getEnv aCONFIG_ENV_VAR  `catch`  (\e -> return "")
                    "-"               -> return ""
                    env               -> getEnv env

  -- Прочитаем конфиг-файл arc.ini или указанный опцией -cfg
  (o1, _)  <- parseOptions (words env_options++options) [] []   -- опция -cfg может быть задана в командной строке или в переменной среды
  cfgfile <- case (findReqArg o1 "config" "--") of
               "--" -> findFile configFilePlaces aCONFIG_FILE
               "-"  -> return ""
               cfg  -> return cfg
  -- Обработаем опцию --charset/-sc, чтобы определить кодировку для чтения конфиг-файла
  let (_, parseFile1, _)  =  parse_charset_option (findReqList o1 "charset")
  -- Прочитаем опции из конфиг-файла, если он есть, и удалим из него пустые строки и комментарии
  config  <-  cfgfile  &&&  parseFile1 'i' cfgfile >>== map trim >>== deleteIfs [null, match ";*"]

  -- Эти определения превращают содержимое конфиг-файла в набор секций,
  -- содержимое которых может быть запрошено функцией configSection.
  -- К примеру, configSection "[Compression methods]" - список строк в секции "[Compression methods]"
  let configSections = map makeSection $ makeGroups selectSectionHeadings config
      makeSection (x:xs) = (cleanupSectionName x, xs)
      configSection name = lookup (cleanupSectionName name) configSections `defaultVal` []
      -- Декодировать метод сжатия/доп. алгоритмы, используя настройки из секции "[Compression methods]"
      decode_compression_method = decode_method (configSection compressionMethods)
      decode_methods s = ("0/"++s).$decode_compression_method.$lastElems (length (elemIndices '/' s) + 1)

  -- А эти определения позволяют вытащить из секции элемент с заданным именем,
  -- включая случаи, когда левая сторона определения содержит несколько слов,
  -- которым даётся одно и то же определение,
  -- и когда определение повторяется (в этом случае надо слить все строки).
  -- Пример:
  --   a create j = -m4x -ms
  --   a = --display
  -- В этом случае (configElement section "a") возвратит "-m4x -ms --display"
  let sectionElement name = unwords . map snd
                              . filter (strLowerEq name . fst)
                              . concatMap (\line -> let (a,b)  =  split2 '=' line
                                                    in  map (\w->(w,trim b)) (words$ trim a))
      configElement section element  =  configSection section .$ sectionElement element

  -- Если первая значащая строка конфиг-файла не является заголовком секции, то
  -- она описывает опции, общие для всех команд
  let config_1st_line  =  case (head1 config) of
                              '[' : _  -> ""    -- это заголовок секции
                              str      -> str

  -- Имя команды: "a", "create" и так далее. Опции по умолчанию для этой команды, заданные в конфиг-файле
  let cmd = head1$ filter (not.match "-*") args
      default_cmd_options = configElement defaultOptions cmd

  -- Добавим в начало командной строки опции по умолчанию для всех команд,
  -- опции по умолчанию для этой команды и содержимое переменной среды
  let additional_args  =  concatMap words [config_1st_line, default_cmd_options, env_options]

  -- Разберём командную строку, получив набор опций и список "свободных аргументов"
  (o, freeArgs)  <-  parseOptions (additional_args++args) [] []
  -- Сообщить об ошибке, если "свободных аргументов" меньше двух - отсутствует команда или имя архива
  case freeArgs of
    []     ->  registerError$ CMDLINE_NO_COMMAND args
    [cmd]  ->  registerError$ CMDLINE_NO_ARCSPEC args
    otherwise -> return ()
  let (cmd:pure_arcspec:pure_filespecs) = freeArgs
                               -- Аргументы:  название опции и значение по умолчанию
  let grouping              =  findReqArg   o "solid" aDEFAULT_DATA_GROUPING .$ parseSolidOption
      group_dir             =  fst3 grouping
      group_data            =  snd3 grouping
      defaultDirCompressor  =  thd3 grouping ||| aDEFAULT_DIR_COMPRESSION
      orig_dir_compressor   =  findReqArg   o "dirmethod"  defaultDirCompressor .$ decode_compression_method
      compression_options   =  findReqList  o "method"
      orig_sort_order       =  findMaybeArg o "sort"
      yes                   =  findNoArg    o "yes"
      autogenerate_arcname  =  findOptArg   o "autogenerate"  "--" ||| "%Y%m%d%H%M%S"
      indicator             =  findOptArg   o "indicator"     "1"  ||| "0"   -- по умолчанию -i1; -i эквивалентно -i0
      arccmt_file           =  findOptArg   o "arccmt"        (if cmd=="c"  then ""  else "--")   -- команда "c" эквивалентна команде "ch -z"
      recovery              =  findOptArg   o "recovery"      (if take 2 cmd=="rr"  then drop 2 cmd  else "--")   -- команда "rr..." эквивалентна команде "ch -rr..."
      orig_workdir          =  findOptArg   o "workdir" "--"   ||| "%TEMP"
      pretest               =  findOptArg   o "pretest"       "1" .$  changeTo [("-","0"), ("+","2"), ("","2")]
      broken_archive        =  findReqArg   o "BrokenArchive" "-"  ||| "0"
      noarcext              =  findNoArg    o "noarcext"
      crconly               =  findNoArg    o "crconly"
      nodata                =  findNoArg    o "nodata"
      url_proxy             =  findOptArg   o "proxy"         "--"
      url_bypass            =  findOptArg   o "bypass"        ""
      exclude_path          =  findOptArg   o "ExcludePath"   "--"

      add_exclude_path  =  exclude_path .$ changeTo [("--", "9"), ("", "0")] .$ readInt
      dir_exclude_path  =  if        cmd=="e"                  then 0
                             else if cmdType cmd==EXTRACT_CMD  then add_exclude_path
                             else                                   3

  -- Список действий, которые надо выполнить непосредственно перед началом выполнения команды
  setup_command <- newList
  setup_command <<= (url_setup_proxy      .$ withCString (replace ',' ' ' url_proxy))
  setup_command <<= (url_setup_bypass_list.$ withCString (replace ',' ' ' url_bypass))

  -- Вручную раскидать опции -o/-op
  let (op, o_rest) = partition is_op_option (findReqList o "overwrite")
      op_opt       = map  (tryToSkip "p") op
      overwrite    = last ("p":o_rest)
      is_op_option ('p':_:_) = True
      is_op_option _         = False

  -- Проверить, что опции принимают одно из допустимых значений
  testOption "overwrite"     "o"  overwrite      (words "+ - p")
  testOption "indicator"     "i"  indicator      (words "0 1 2")
  testOption "pretest"       "pt" pretest        (words "0 1 2 3")
  testOption "BrokenArchive" "ba" broken_archive (words "- 0 1")
  testOption "ExcludePath"   "ep" exclude_path   ([""]++words "1 2 3 --")

  -- Определить имя SFX-модуля, который будет добавлен в начало архива
  sfx <- case (findOptArg o "sfx" (if cmd=="s" then "" else "--"))   -- команда "s" эквивалентна команде "ch -sfx"
          of ""  -> findFile libraryFilePlaces aDEFAULT_SFX    -- использовать модуль SFX по умолчанию (arc.sfx из каталога, где находится программа)
             sfx -> return sfx
  when (sfx=="") $
    registerError$ GENERAL_ERROR$ "SFX module "++aDEFAULT_SFX++" is not found"

  -- Добавим к базовому имени архива штамп даты/времени, если указана опция -ag
  current_time <- getClockTime
  let add_ag  =  case autogenerate_arcname of
                   "--" -> id
                   _    -> updateBaseName (++ showtime autogenerate_arcname current_time)

  -- Добавим к имени архива расширение по умолчанию, если нет другого расширения и не используется опция --noarcext
  let arcspec  =  addArcExtension noarcext$ add_ag pure_arcspec

  -- Обработать список опций --charset/-sc, возвратив таблицу кодировок
  -- и процедуры чтения/записи файлов с её учётом
  let (charsets, parseFile, unParseFile)  =  parse_charset_option (findReqList o "charset")

  -- Вручную обработать список опций --display
  let orig_display = foldl f aDISPLAY_DEFAULT (findReqList o "display")
      -- Функция обработки опций --display
      f value ""       =  aDISPLAY_ALL     -- -di без параметров означает включить вывод всей информации
      f value "--"     =  aDISPLAY_DEFAULT -- -di-- означает восстановить значение по умолчанию
      f value ('+':x)  =  nub (value++x)   -- -di+x означает добавить x к флагам
      f value ('-':x)  =  nub value \\ x   -- -di-x означает убрать x из флагов
      f value x        =  nub x            -- иначе просто скопируем параметр в значение опции

  -- Для команды "lb" полностью отключать вывод доп. информации на экран,
  -- для прочих команд листинга включать вывод имени архива в принудительном порядке
  let display = case () of
                  _ | cmd=="lb"              ->  ""
                    | cmdType cmd==LIST_CMD  ->  orig_display++"a"
                    | otherwise              ->  orig_display
  -- Установить display_option, поскольку она нам может понадобиться при выводе warning о содержимом external compressor section
  display_option' =: display
  -- Перед началом выполнения команды восстановить значение display_option, поскольку оно могло быть изменено при парсинге/выполнении других команд
  setup_command <<= (display_option' =: display)

  -- Зарегистрируем описания внешних упаковщиков из секций [External compressor:...]
  let externalSections = filter (matchExternalCompressor.head) $ makeGroups selectSectionHeadings config
      matchExternalCompressor s = and[ head externalCompressor          ==    head s
                                     , init (tail externalCompressor) `match` init (tail s)
                                     , last externalCompressor          ==    last s]
  results <- mapM (addExternalCompressor.unlines) externalSections
  for (zip results externalSections) $ \ (result,section) -> do
      when (result/=1) $ do
          registerWarning (BAD_CFG_SECTION cfgfile section)


---------------------------------------------------------------------------------------------------
-- ОПРЕДЕЛЕНИЕ АЛГОРИТМА СЖАТИЯ -------------------------------------------------------------------
  -- Парсер объёмов памяти, воспринимающий записи типа "75%" (от объёма ОЗУ)
  -- Объём памяти округляется до величины, кратной 4 мб, чтобы исключить получение некруглых величин в результате действия различных Shadow BIOS options
  let parsePhysMem = parseMemWithPercents (toInteger getPhysicalMemory `roundTo` (4*mb))

  -- Парсер опции -md
  let parseDict dictionary  =  case dictionary of
          [c]       | isAlpha c     ->  Just$ 2^(16 + ord c - ord 'a')   -- опция задана одной буквой, -mda..-mdz
          s@(c:_)   | isDigit c     ->  Just$ parsePhysMem s             -- опция начинается c цифры: -md8, -md8m, -md10%
          otherwise                 ->  Nothing                          -- иначе - это не опция -md, а опция -m, начинающаяся с -md...

  -- Цикл, вручную обрабатывающий различные опции, начинающиеся на "-m"
  method <- new "";    methods <- new "";  mc' <- newList;  dict <- new 0;
  mm'    <- new "--";  threads <- new 0
  for compression_options $ \option ->
    case option of
      -- Опция -mс позволяет быстро отключить отдельные алгоритмы сжатия (-mcd-, -mc-rep)
      'c':rest  | anyf [beginWith "-", endWith "-"] rest
                    ->  mc' <<= rest.$tryToSkip "-".$tryToSkipAtEnd "-"
                                    .$changeTo [("d","delta"), ("e","exe"),  ("l","lzp")
                                               ,("r","rep"),   ("z","dict")
                                               ,("a","$wav"),  ("c","$bmp"), ("t","$text")
                                               ]
      -- Опция -md устанавливает размер словаря как в старом добром RAR :)
      'd':rest  | Just md <- parseDict rest ->  dict =: md
      -- Опция -mm выбирает режим мультимедиа-сжатия
      'm':rest  | mmflag <- rest.$tryToSkip "=",
                  mmflag `elem` ["","--","+","-","max","fast"]  ->  mm' =: mmflag
      -- Опция -ms задаёт использование быстрого метода сжатия для уже сжатых файлов
      "s"  ->  methods ++= "/$compressed=rep:8mb:128+tor:2:64k"
      -- Опция -mt включает/выключает многотредовость и устанавливает количество тредов
      't':rest  | n <- rest.$changeTo [("-","1"), ("+","0"), ("","0"), ("--","0")],
                  all isDigit n  ->  threads =: readInt n
      -- Опции -m$type=method устанавливают алгоритмы сжатия для отдельных типов файлов
      '$':_ -> case (break (`elem` "=:.") option) of
                 (_type, '=':method) -> methods ++= '/':option                      -- -m$type=method: архивировать файлы этого типа заданным компрессором
                 -- (_type, ':':names)  -> types  ++= split ':' names               -- -m$type:name1:name2: добавить в список файлов этого типа заданные маски
                 -- (_type, ',':exts)   -> types  ++= map ("*."++) $ split '.' exts -- -m$type.ext1.ext2: добавить расширения в список типа
                 otherwise -> registerError$ CMDLINE_BAD_OPTION_FORMAT ("-m"++option)
      -- Все остальные опции, начинающиеся на -m0= или просто -m, задают основной метод сжатия
      m  ->  method =: m.$tryToSkip "0="
  -- Прочитаем окончательные значения переменных
  dictionary  <- val dict       -- размер словаря (-md)
  cthreads    <- val threads    -- количество compression threads (-mt)
  mainMethod  <- val method     -- основной метод сжатия
  userMethods <- val methods    -- дополнительные методы для конкретных типов файлов (-m$/-ms)
  mm          <- val mm'        -- мультимедиа-сжатие
  mc          <- listVal mc'    -- список алгоритмов сжатия, которые требуется отключить

  -- Перед началом выполнения команды передать в библиотеку упаковки количество тредов, которое она должна использовать
  setup_command <<= (setCompressionThreads$  cthreads ||| i getProcessorsCount)   -- By default, use number of threads equal to amount of available processors/cores

  -- Ограничения на память при упаковке/распаковке
  let climit = parseLimit$ findReqArg o "LimitCompMem"   "--"
      dlimit = parseLimit$ findReqArg o "LimitDecompMem" "--"
      parseLimit x = case x of
        "--" -> parsePhysMem "75%"  -- По умолчанию: ограничить использование памяти 75% её физического объёма
        "-"  -> maxBound::MemSize   -- Не ограничивать использование памяти
        s    -> parsePhysMem s      -- Ограничить использование памяти заданным объёмом

  -- Управление мультимедиа-сжатием
  let multimedia mm = case mm of
        "-"    -> filter ((`notElem` words "$wav $bmp").fst)    -- удалим группы $wav и $bmp из списка методов сжатия
        "fast" -> (++decode_methods "$wav=wavfast/$bmp=bmpfast") . multimedia "-"
        "max"  -> (++decode_methods "$wav=wav/$bmp=bmp")         . multimedia "-"
        "+"    -> \m -> case () of
                          _ | m.$isFastDecompression  -> m.$multimedia "fast"
                            | otherwise               -> m.$multimedia "max"
        ""     -> multimedia "+"
        "--"   -> id

  -- Удаление заданного алгоритма сжатия
  let method_change mc x = case mc of
        '$':_  -> -- удалим группу mc (например, "$bmp") из списка методов сжатия
                  x.$ filter ((/=mc).fst)
        _      -> -- удалим группы, в которых mc - последний алгоритм сжатия (например -mc-tta приведёт к удалению групп, цепочки сжатия которых заканчиваются алгоритмом tta)
                  x.$ (\(x:xs) -> x:(xs.$ filter ((/=mc).method_name.last1.snd)))   -- Не трогаем основную группу сжатия (голову списка)
                  -- удалим алгоритм mc из остальных цепочек сжатия
                   .$ map (mapSnd$ filter ((/=mc).method_name))

  -- Если задана опция "--nodata", то симулировать сжатие данных.
  -- Если задана опция "--crconly", то ограничиться подсчётом CRC архивируемых файлов.
  -- В противном случае обработать выбранные основной и дополнительные алгоритмы сжатия,
  -- настроив мультимедиа-сжатие и размер словаря, удалив отключенные алгоритмы,
  -- и ограничив потребление памяти
  let data_compressor = if      nodata   then [("", [aFAKE_COMPRESSION])]
                        else if crconly  then [("", [aCRC_ONLY_COMPRESSION])]
                        else ((mainMethod ||| aDEFAULT_COMPRESSOR) ++ userMethods)
                               .$ decode_compression_method
                               .$ multimedia mm
                               .$ applyAll (map method_change mc)
                               .$ compressorSetDictionary dictionary
                               .$ compressorLimitCompressionMem   climit
                               .$ compressorLimitDecompressionMem dlimit

  -- Ограничить сжатие каталога одним методом в цепочке и наличным объёмом памяти
  let dir_compressor = orig_dir_compressor.$ compressorLimitCompressionMem   climit
                                          .$ compressorLimitDecompressionMem dlimit
                                          .$ getMainCompressor
                                          .$ last .$ (:[])

  -- Макс. размер блока в используемых блочных компрессорах или 0
  let maxBlockSize = compressorGetBlockSize data_compressor
  -- Память, требуемая для алгоритма сжатия
  let compressionMem = compressorGetCompressionMem data_compressor

  -- Вычислить, сколько памяти нужно использовать под буфер упреждающего чтения файлов.
  -- Если размер кеша не задан явно опцией --cache, мы используем от 1 мб до 16 мб,
  -- стараясь сделать так, чтобы общее потребление памяти программой не превосходило
  -- половины от её физического объёма (не считая памяти, необходимой для распаковки данных
  -- в обновляемых архивах). Разумеется, при наличии параллельно выполняющихся memory-intensive
  -- tasks (и в частности, параллельно работающих копиях FreeArc) эта тактика не очень удачна.
  -- Лучше было бы смотреть на объём *свободного* физического ОЗУ в момент запуска программы
  let minCache  =  1*mb                             -- Мин. размер кеша  - 1  мб
      maxCache  =  (16*mb) `atLeast` maxBlockSize   -- Макс. размер кеша - 16 мб или размер блока для поблочных алгоритов (lzp/grzip/dict)
      availMem  =  if parsePhysMem "50%" >= compressionMem      -- "Свободно памяти" = 50% ОЗУ минус память, требуемая для сжатия
                       then parsePhysMem "50%" - compressionMem
                       else 0
      cache     =  clipToMaxInt $ atLeast aBUFFER_SIZE $  -- Кеш должен включать как минимум один буфер
                       case (findReqArg o "cache" "--") of
                           "--" -> availMem.$clipTo minCache maxCache
                           "-"  -> aBUFFER_SIZE
                           s    -> parsePhysMem s

  -- Автоматически включить опцию --recompress для команд, копирующих архив,
  -- если указаны опции -m../--nodata/--crconly
  let recompress = findNoArg o "recompress"
                   || (cmd `elem` cOPYING_COMMANDS && (mainMethod>"" || nodata || crconly))
  -- Не перепаковывать существующие солид-блоки в архиве при --append
  -- и в командах копирования архива, если нет явных причин
  let keep_original = findNoArg o "append"
                      || (cmd `elem` cOPYING_COMMANDS  &&  not recompress)


---------------------------------------------------------------------------------------------------
-- ПРЕДИКАТЫ ДЛЯ ОПРЕДЕЛЕНИЯ НОМЕРА ГРУППЫ (find_group) И ТИПА ФАЙЛА (find_type) ------------------
  -- Определить, какой файл со списком групп (типа arc.groups) будет использоваться
  actual_group_file <- case (findReqArg o "groups" "--") of
      "--" -> findFile configFilePlaces aDEFAULT_GROUPS_FILE  -- использовать файл групп по умолчанию (arc.groups из каталога, где находится программа)
      "-"  -> return ""      -- файл групп отключен опцией    --groups-
      x    -> return x       -- файл групп указан явно опцией --groups=FILENAME

  -- Прочитать список групп из файла групп
  group_strings  <-  if actual_group_file > ""
                         then parseFile 'i' actual_group_file      -- распарсить файл групп с учётом кодировки символов и разделителей строк
                                >>== map translatePath             -- превратить все '\' в '/'
                                >>== deleteIfs [match ";*", null]  -- удалить строки комментариев и пустые
                         else return [reANY_FILE]     -- если файл групп не используется, то все файлы принадлежат одной общей группе
  -- Список предикатов, проверяющих вхождение в каждую группу
  let group_predicates  =  map (match_FP fpBasename) group_strings
  -- Группа по умолчанию, куда попадают все файлы, не совпадающие ни с одной из масок.
  -- Указывается псевдо-маской "$default", при её отсутствии считается, что эта маска добавлена в конец списка
  let lower_group_strings = (map strLower group_strings) ++ ["$default"]
      default_group = "$default" `elemIndex` lower_group_strings .$ fromJust
  -- Функция "PackedFilePath -> номер группы из arc.groups"
  let find_group    = findGroup group_predicates default_group

  -- Список типов файлов ($text, $exe и так далее), соответствующих каждой группе из arc.groups
  let group_type_names = go "$binary" lower_group_strings  -- начальная группа - "$binary"
      go t []     = []           -- пройти по списку групп, заменяя маски файлов
      go t (x:xs) = case x of    --   на предшествующие им имена типов файлов ("$text", "$rgb" и так далее)
                      '$':_ | x/="$default" -> x : go x xs
                      _                     -> t : go t xs
  -- Список номеров методов сжатия из списка `data_compressor`, соответствующих каждой группе из arc.groups
  let group_types =  map typeNum group_type_names
      typeNum t   =  t `elemIndex` (map fst data_compressor) `defaultVal` 0
  -- Список предикатов, проверяющих что файл принадлежит одному из типов, перечисленных в `data_compressor`
  let type_predicates  =  const False : map match_type [1..maximum group_types]
      match_type t     =  any_function$ concat$ zipWith (\a b->if a==t then [b] else []) group_types group_predicates
  -- Функция "PackedFilePath -> номер компрессора в списке `data_compressor`"
  let find_type  =  findGroup type_predicates 0


-------------------------------------------------------------------------------------
-- ФИЛЬТР ФАЙЛОВ
  let match_with            =  findNoArg    o "fullnames"          .$bool fpBasename fpFullname
      orig_include_list     =  findReqList  o "include"
      orig_exclude_list     =  findReqList  o "exclude"
      include_dirs          =  findNoArgs   o "dirs" "nodirs"
      clear_archive_bit     =  findNoArg    o "ClearArchiveBit"
      select_archive_bit    =  findNoArg    o "SelectArchiveBit"
      filesize_greater_than =  findReqArg   o "SizeMore"           "--"
      filesize_less_than    =  findReqArg   o "SizeLess"           "--"
      time_before           =  findReqArg   o "TimeBefore"         "--"
      time_after            =  findReqArg   o "TimeAfter"          "--"
      time_newer            =  findReqArg   o "TimeNewer"          "--"
      time_older            =  findReqArg   o "TimeOlder"          "--"

  -- Заменим ссылки на лист-файлы (@listfile/-n@listfile/-x@listfile) их содержимым
  listed_filespecs <- pure_filespecs   .$ replace_list_files parseFile >>== map translatePath
  include_list     <- orig_include_list.$ replace_list_files parseFile >>== map translatePath
  exclude_list     <- orig_exclude_list.$ replace_list_files parseFile >>== map translatePath

  -- Предикаты отбора включаемых (-n) и исключаемых (-x) файлов. Для -n проверяем orig_include_list, поскольку при пустом листфайле ни один файл не должен проходить фильтр
  let match_included  =  orig_include_list &&& [match_filespecs match_with include_list]
      match_excluded  =  exclude_list      &&& [match_filespecs match_with exclude_list]

#if defined(FREEARC_WIN)
  -- Отбор файлов по атрибутам
  let attrib_filter | select_archive_bit = [\attr -> attr.&.fILE_ATTRIBUTE_ARCHIVE /= 0]
                    | otherwise          = []
#else
  let attrib_filter = []
#endif

  -- Отбор файлов по размеру
  let size_filter _  "--"   = []
      size_filter op option = [`op` parseSize option]

  -- Отбор файлов по времени модификации, time в формате YYYYMMDDHHMMSS
  let time_filter _  "--" = []
      time_filter op time = [`op` (time.$makeCalendarTime.$toClockTime.$convert_ClockTime_to_CTime)]
      -- Преобразует строчку вида YYYY-MM-DD_HH:MM:SS в CalendarTime и выставляет корректное ctTZ в зависимости от её времени года (для этого toCalendarTime.toClockTime делается дважды)
      makeCalendarTime str = ct {ctTZ = ctTZ$ unsafePerformIO$ toCalendarTime$ toClockTime ct2}
          where        ct2 = ct {ctTZ = ctTZ$ unsafePerformIO$ toCalendarTime$ toClockTime ct}
                       ct = CalendarTime
                            { ctYear    = readInt (take 4 s)
                            , ctMonth   = readInt (take 2 $ drop 4 s) .$ (\x->max(x-1)0) .$ toEnum
                            , ctDay     = readInt (take 2 $ drop 6 s)
                            , ctHour    = readInt (take 2 $ drop 8 s)
                            , ctMin     = readInt (take 2 $ drop 10 s)
                            , ctSec     = readInt (take 2 $ drop 12 s)
                            , ctPicosec = 0
                            , ctWDay    = error "ctWDay"
                            , ctYDay    = error "ctYDay"
                            , ctTZName  = error "ctTZName"
                            , ctTZ      = 0
                            , ctIsDST   = error "ctIsDST"
                            }
                       s = filter isDigit str ++ repeat '0'

  -- Отбор файлов по "старости", time в формате [<ndays>d][<nhours>h][<nminutes>m][<nseconds>s]
  let oldness_filter _  "--" = []
      oldness_filter op time = [`op` (time.$calcDiff.$(`addToClockTime` current_time).$convert_ClockTime_to_CTime)]

      calcDiff  =  foldl updateTD noTimeDiff . recursive (spanBreak isDigit)
      updateTD td x = case (last x) of
                        'd' -> td {tdDay  = -readInt (init x)}
                        'h' -> td {tdHour = -readInt (init x)}
                        'm' -> td {tdMin  = -readInt (init x)}
                        's' -> td {tdSec  = -readInt (init x)}
                        _   -> td {tdDay  = -readInt x}

  -- Фильтр отбора файлов, включающий все критерии отбора,
  -- указанные в командной строке, кроме отбора по filespecs.
  -- Для последних используется отдельная функция,
  -- потому что они по-разному используются в командах разного типа.
  let file_filter = all_functions$
                      concat [                     attrib_filter          .$map (.fiAttr)
                             , map (not.)          match_excluded         .$map (.fiFilteredName)
                             , nst_filters
                             ]
      nst_filters =   concat [                     match_included         .$map (.fiFilteredName)
                             , size_filter    (>)  filesize_greater_than  .$map (.fiSize)
                             , size_filter    (<)  filesize_less_than     .$map (.fiSize)
                             , time_filter    (>=) time_after             .$map (.fiTime)
                             , time_filter    (<)  time_before            .$map (.fiTime)
                             , oldness_filter (>=) time_newer             .$map (.fiTime)
                             , oldness_filter (<)  time_older             .$map (.fiTime)
                             ]

  -- Если имена обрабатываемых файлов не указаны и команда не cw/d, то обрабатывать все файлы
  filespecs <- case listed_filespecs of
      [] | cmd `elem` (words "cw d")  ->  registerError$ CMDLINE_NO_FILENAMES args
         | otherwise                  ->  return aDEFAULT_FILESPECS
      _                               ->  return listed_filespecs

  -- Включать каталоги в обработку? Эта переменная используется только при листинге/распаковке
  let x_include_dirs  =  case include_dirs of
           Just x  -> x   -- в соответствии с оциями --dirs/--nodirs
           _       -> -- ДА, если обрабатываются все файлы, нет фильтров -n/-s*/-t* и команда не "e"
                      filespecs==aDEFAULT_FILESPECS && null nst_filters && cmd/="e"


-------------------------------------------------------------------------------------
-- ШИФРОВАНИЕ
  -- Алгоритм шифрования; проверка валидности и приведение к каноническому виду ("aes" -> "aes-256/ctr")
  let ea = findReqArg o "encryption" aDEFAULT_ENCRYPTION_ALGORITHM
  encryptionAlgorithm <- join_compressor ==<< (foreach (split_compressor ea) $ \algorithm -> do
    unless (compressionIs "encryption?" algorithm) $ do
      registerError$ CMDLINE_GENERAL$ algorithm++" - error in encryption algorithm name or parameters"
    return$ canonizeCompressionMethod algorithm)

  -- Пароли для данных и заголовка архива
  let (dpwd,hpwd) = case (findReqArg o "password"        "--" .$changeTo [("-", "--")]
                         ,findReqArg o "HeadersPassword" "--" .$changeTo [("-", "--")])
                    of
                       (p,    "--")  ->  (p,  "--")    --  -p...
                       ("--", p   )  ->  (p,  p   )    --  -hp..,
                       (p,    ""  )  ->  (p,  p   )    --  -p[PWD] -hp
                       ("",   p   )  ->  (p,  p   )    --  -p -hpPWD
                       (p1,   p2  )  ->  (p1, p2  )    --  -pPWD1 -hpPWD2

  -- Запретить запрос паролей, необходимых для распаковки, если указано -op-/-p-/-hp-
  let dont_ask_passwords  =  last ("":op_opt) == "-" || findReqArg o "OldPassword" "" == "-"  ||  findReqArg o "password" "" == "-"  ||  findReqArg o "HeadersPassword" "" == "-"
  -- Список паролей, используемых при распаковке
  mvar_unpack_passwords  <-  newMVar$ deleteIfs [=="",=="?",=="-",=="--"]$ op_opt ++ findReqList o "OldPassword" ++ findReqList o "password" ++ findReqList o "HeadersPassword"
  -- Содержимое ключевых файлов, используемых при распаковке
  oldKeyfileContents     <-  mapM fileGetBinary (findReqList o "OldKeyfile" ++ findReqList o "keyfile")
  -- Содержимое ключевого файл, используемого при упаковке
  keyfileContents        <-  unlessNull fileGetBinary (findReqArg o "keyfile" "")
  -- Требуется ввод пароля с клавиатуры при -p? и при -p, если нет ключевого файла
  let askPwd pwd          =  pwd=="?" || (pwd=="" && keyfileContents=="")
  -- Рецепт подготовки команды к использованию шифрования, или Nothing до создания рецепта
  receipt                <-  newMVar Nothing

  -- Подготавливает command к использованию шифрования, при необходимости
  -- запрашивая пароль у пользователя и считывая keyfiles
  let cookPasswords command (ask_encryption_password, ask_decryption_password) = do
        modifyMVar receipt $ \x -> do
          f <- x.$maybe makeReceipt return   -- создать рецепт подготовки команды к шифрованию, если его ещё нет
          return (Just f, f command)         -- применить рецепт к command и запомнить его для последующих применений
       where
        makeReceipt = do
          -- Запросим у пользователя пароль, если он потребуется нам дальше
          let ask_password | cmdType cmd==ADD_CMD = ask_encryption_password
                           | otherwise            = ask_decryption_password
          asked_password  <-  any askPwd [dpwd,hpwd]  &&&  ask_password
          -- Добавим в список паролей распаковки введённый пользователем пароль и пустой пароль, если для расшифровки может быть использован keyfile
          asked_password      &&&  modifyMVar_ mvar_unpack_passwords (return.(asked_password:))
          oldKeyfileContents  &&&  modifyMVar_ mvar_unpack_passwords (return.("":))
          -- Добавить к паролю содержимое keyfile и заменить обозначения "--"/"?"
          let cook "--"             = ""                                -- шифрование отключено
              cook pwd | askPwd pwd = asked_password++keyfileContents   -- пароль, введёный с клавиатуры + содержимое keyfile
                       | otherwise  = pwd++keyfileContents              -- пароль из командной строки + содержимое keyfile
          return$ \command ->
                   command { opt_data_password    = cook dpwd
                           , opt_headers_password = cook hpwd
                           , opt_decryption_info  = (dont_ask_passwords, mvar_unpack_passwords, oldKeyfileContents, ask_decryption_password)}


-------------------------------------------------------------------------------------
-- ПРОЧЕЕ ПО МЕЛОЧИ
  -- Алгоритм обновления архива
  let update_type = case cmd of
        "f"                       -> 'f'  -- команда f: обновить файлы более свежими версиями, новых файлов не добавлять
        "u"                       -> 'u'  -- команда u: обновить файлы более свежими версиями и добавить новые файлы
        _ | findNoArg o "freshen" -> 'f'  -- опция  -f: см. выше
          | findNoArg o "update"  -> 'u'  -- опция  -u: см. выше
          | findNoArg o "sync"    -> 's'  -- опция --sync: привести файлы в архиве в соответствие с файлами на диске
          | otherwise             -> 'a'  -- иначе: заменить файлы в архиве на взятые с диска и добавить новые файлы

  -- Закрыть архив от изменений, если использована опция "-k" или команда "k"
  let lock_archive  =  findNoArg o "lock" || cmd=="k"

  -- Удалить архивируемые файлы, если использована опция "-d[f]" или команда "m[f]"
  delete_files  <-  case (findNoArg o "delete"   || cmd=="m"
                         ,findNoArg o "delfiles" || cmd=="mf")
                      of
                         (False, False) -> return NO_DELETE
                         (False, True ) -> return DEL_FILES
                         (True , False) -> return DEL_FILES_AND_DIRS
                         (True , True ) -> registerError$ CMDLINE_GENERAL "options m/-d and mf/-df can't be used together"

  -- Запретим использование несовместимых опций
  when (clear_archive_bit && delete_files/=NO_DELETE) $
      registerError$ CMDLINE_GENERAL "options m[f]/-d[f] and -ac can't be used together"

  -- Каталог для временных файлов - может быть указан явно или через переменную среды
  workdir <- case orig_workdir of
               "--"       -> return ""    -- По умолчанию (означает создавать файлы сразу в выходном каталоге)
               '%':envvar -> getEnv envvar
               dir        -> return dir

  -- Определить порядок сортировки файлов в архиве
  let sort_order  =  case (orig_sort_order, group_data) of
        (Just "-", _)  -> ""                    -- Если порядок сортировки задан как "-", то отключить сортировку
        (Just  x,  _)  -> x                     -- Если порядок сортировки был явно указан, то использовать его
        (_, [GroupNone]) -> ""                  -- Если не используется solid-сжатие - отключить сортировку
        _  -> if getMainCompressor data_compressor
                 .$anyf [==aNO_COMPRESSION, isFakeCompressor, isVeryFastCompressor]
                then ""                         -- Если -m0/--nodata/--crconly/tor:1..4/lzp:h13..15 - также отключить сортировку
                else aDEFAULT_SOLID_SORT_ORDER  -- Иначе - использовать стандартный порядок сортировки для solid-архивов

  -- Проверим, что опция "-rr" принимает одно из допустимых значений
  let rr_ok = recovery `elem` ["","-","--"]
              || snd(parseNumber recovery 'b') `elem` ['b','%','p']
              || ';' `elem` recovery
              || '*' `elem` recovery
  unless rr_ok $ do
    registerError$ INVALID_OPTION_VALUE "recovery" "rr" ["MEM", "N", "N%", "MEM;SS", "N%;SS", "N*SS", "-", ""]

  -- Состояние запроса к пользователю о перезаписи файлов
  ref_overwrite  <-  newIORef$ case (yes,   overwrite) of
                                    (_,     "+")  ->  "a"
                                    (_,     "-")  ->  "s"
                                    (True,  _  )  ->  "a"
                                    (False, "p")  ->  " "

  -- Список действий, которые надо выполнить непосредственно перед началом выполнения команды
  setup_command'  <-  listVal setup_command >>== sequence_


------------------------------------------------------------------------------------------------
-- Занесём всё это в структуру, представляющую выполняемую команду в последующей части программы
  return$ Just$ Command {
      cmd_command_text         = unwords ("arc":args)
    , cmd_additional_args      = unwords additional_args
    , cmd_name                 = cmd
    , cmd_arcspec              = arcspec
    , cmd_arclist              = error "Using uninitialized cmd_arclist"
    , cmd_arcname              = error "Using uninitialized cmd_arcname"
    , cmd_archive_filter       = error "Using uninitialized cmd_archive_filter"
    , cmd_filespecs            = filespecs
    , cmd_added_arcnames       = return []
    , cmd_diskfiles            = return []
    , cmd_subcommand           = False
    , cmd_setup_command        = setup_command'

    , opt_scan_subdirs         = findNoArg    o "recursive"
    , opt_add_dir              = findNoArg    o "adddir"
    , opt_add_exclude_path     = add_exclude_path
    , opt_dir_exclude_path     = dir_exclude_path
    , opt_arc_basedir          = findReqArg   o "arcpath"   "" .$ translatePath .$ dropTrailingPathSeparator
    , opt_disk_basedir         = findReqArg   o "diskpath"  "" .$ translatePath .$ dropTrailingPathSeparator
    , opt_no_nst_filters       = null nst_filters
    , opt_file_filter          = file_filter
    , opt_group_dir            = group_dir
    , opt_group_data           = group_data
    , opt_data_compressor      = data_compressor
    , opt_dir_compressor       = dir_compressor
    , opt_include_dirs         = include_dirs
    , opt_indicator            = indicator
    , opt_display              = display
    , opt_overwrite            = ref_overwrite
    , opt_keep_time            = findNoArg    o "keeptime"
    , opt_time_to_last         = findNoArg    o "timetolast"
    , opt_test                 = findNoArg    o "test"
    , opt_pretest              = readInt pretest
    , opt_keep_broken          = findNoArg    o "keepbroken"
    , opt_match_with           = match_with
    , opt_append               = findNoArg    o "append"
    , opt_recompress           = recompress
    , opt_keep_original        = keep_original
    , opt_noarcext             = noarcext
    , opt_nodir                = findNoArg    o "nodir"
    , opt_cache                = cache
    , opt_update_type          = update_type
    , opt_x_include_dirs       = x_include_dirs
    , opt_sort_order           = sort_order
    , opt_reorder              = False
    , opt_find_group           = find_group . fiFilteredName
    , opt_groups_count         = length group_strings
    , opt_find_type            = find_type  . fiFilteredName
    , opt_types_count          = maximum group_types + 1
    , opt_arccmt_file          = arccmt_file
    , opt_lock_archive         = lock_archive
    , opt_sfx                  = sfx
    , opt_logfile              = findReqArg   o "logfile"      ""
    , opt_delete_files         = delete_files
    , opt_workdir              = workdir
    , opt_clear_archive_bit    = clear_archive_bit
    , opt_recovery             = recovery
    , opt_broken_archive       = broken_archive
    , opt_original             = findOptArg   o "original"          "--"
    , opt_save_bad_ranges      = findReqArg   o "save-bad-ranges"   ""
    , opt_limit_compression_memory   = climit
    , opt_limit_decompression_memory = dlimit

    , opt_encryption_algorithm = encryptionAlgorithm
    , opt_cook_passwords       = cookPasswords
    , opt_data_password        = error "opt_data_password used before cookPasswords!"
    , opt_headers_password     = error "opt_headers_password used before cookPasswords!"
    , opt_decryption_info      = error "opt_decryption_info used before cookPasswords!"

    , opt_parseFile            = parseFile
    , opt_unParseFile          = unParseFile
    }


{-# NOINLINE testOption #-}
-- |Проверить, что опция принимает одно из разрешённых значений
testOption fullname shortname option valid_values = do
  unless (option `elem` valid_values) $ do
    registerError$ INVALID_OPTION_VALUE fullname shortname valid_values

{-# NOINLINE addArcExtension #-}
-- |Если имя архива не содержит расширения и не используется опция --noarcext,
-- то добавить к нему расширение по умолчанию
addArcExtension noarcext filespec =
  case (hasExtension filespec, noarcext) of
    (False, False)  ->  filespec ++ aDEFAULT_ARC_EXTENSION
    _               ->  filespec

{-# NOINLINE replace_list_files #-}
-- |Заменить ссылки на лист-файлы ("@listfile") их содержимым
replace_list_files parseFile  =  concatMapM $ \filespec ->
  case (startFrom "@" filespec) of
    Just listfile  ->  parseFile 'l' listfile >>== deleteIf null
    _              ->  return [filespec]

-- |Разбор параметров опции "-s"
parseSolidOption opt =
  case (split ';' opt) of
    []        ->  ([aDEFAULT_DIR_GROUPING], [GroupAll], "")   -- "-s" включает общий солид-блок для всех файлов в одном каталоге архива
    ["-"]     ->  ([aDEFAULT_DIR_GROUPING], [GroupNone], "")  -- "-s-" отключает солид-сжатие, для каталогов используется стандартная группировка
    ["7z"]    ->  ([GroupAll],  [GroupAll], "")               -- "-s=7z"  делает общий сжатый каталог и один солид-блок для всех файлов в архиве
    ["cab"]   ->  ([GroupAll],  [GroupAll],  "0")   --  -dm0  -- "-s=cab" делает общий несжатый каталог и один солид-блок для всех файлов в архиве
    ["zip"]   ->  ([GroupAll],  [GroupNone], "0")   --  -dm0  -- "-s=zip" делает отдельный солид-блок для каждого файла в архиве, и общий несжатый каталог
    ["arj"]   ->  ([GroupNone], [GroupNone], "0")   --  -dm0  -- "-s=arj" делает отдельный солид-блок и каталог для каждого файла в архиве
    [dat]     ->  ([aDEFAULT_DIR_GROUPING], parse dat, "")    -- "-sXXX" задаёт группировку только для солид-блоков, каталоги группируются стандартно
    [dir,dat] ->  (parse dir, parse dat, "")                  -- "-sXXX;YYY" задаёт группировку и для каталогов, и для солид-блоков
  where
    -- Разборщик описания группировки файлов:
    --   "-s/-se/-s10m/-s100f" - группировать все/по расширению/по 10 мб/по 100 файлов, соответственно.
    -- `parse1` обрабатывает одно описание группировки,
    -- а `parse` - их последовательность, например -se100f10m
    parse = map parse1 . recursive split
      where split ('e':xs) = ("e",xs)
            split xs       = spanBreak (anyf [isDigit, =='e']) xs
    parse1 s = case s of
                ""  -> GroupAll
                "e" -> GroupByExt
                _   -> case (parseNumber s 'f') of
                         (num, 'b') -> GroupBySize (i num)
                         (1,   'f') -> GroupNone
                         (num, 'f') -> GroupByNumber (i num)


----------------------------------------------------------------------------------------------------
---- Описания опций и поиск в списке найденных опций -----------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Описание опции - краткое имя, длинное имя, печатаемое описание
data Option = OPTION String String String

-- |Тип опции - короткие имеют префикс "-", длинные - префикс "--"
data OptType  =  SHORT | LONG

-- |Наличие параметра в опции: нет/обязательно/опционально
data ParamType  =  ParamNo | ParamReq | ParamOpt

-- |"Словарь" опций, содержащий их в удобной для разбора командной строки форме
optionsDict  =  concatMap compileOption optionsList
  where compileOption (OPTION short long description)  =  compile short ++ compile ('-':long)
          where -- Добавить в список описание опции с именем `name`, если оно непустое
                compile name  =  case (name, paramName description) of
                    ("",  _      )  ->  []                                -- нет имени - нет и опции :)
                    ("-", _      )  ->  []                                -- нет имени - нет и опции :)
                    (_,   Nothing)  ->  [(name, long|||short, ParamNo )]  -- опция без параметра
                    (_,   Just _ )  ->  [(name, long|||short, ParamReq)]  -- опция с параметром

-- |Описание опций для пользователя
optionsHelp  =  init$ unlines table
  where (ss,ls,ds)     = (unzip3 . map fmtOpt) optionsList
        table          = zipWith3 paste (sameLen ss) (sameLen ls) ds
        paste x y z    = "  " ++ x ++ "  " ++ y ++ "  " ++ z
        sameLen xs     = flushLeft ((maximum . map length) xs) xs
        flushLeft n    = map (left_justify n)
          -- Возвращает формат "короткой опции", "длинной опции", и их описание
        fmtOpt (OPTION short long description)  =  (format short "" description, format ('-':long) "=" description, description)
          -- Возвращает формат опции `name` с учётом наличия у неё имени и параметра
        format name delim description  =  case (name, paramName description) of
                                            ("",   _         )  ->  ""
                                            ("-",  _         )  ->  ""
                                            ("--", _         )  ->  "--"
                                            (_,    Nothing   )  ->  "-"++name
                                            (_,    Just aWORD)  ->  "-"++name++delim++aWORD

-- |Возвращает имя параметра опции, извлекая его из строки её описания
paramName descr =
  case filter (all isUpper) (words descr)
    of []      -> Nothing      -- Описание не содержит UPPERCASED слов
       [aWORD] -> Just aWORD   -- Описание включает UPPERCASED слово, обозначающее параметр опции
       _       -> error$ "option description \""++descr++"\" contains more than one uppercased word"

-- |Разбор командной строки, возвращающий список опций и список "свободных аргументов"
parseOptions []          options freeArgs  =  return (reverse options, reverse freeArgs)
parseOptions ("--":args) options freeArgs  =  return (reverse options, reverse freeArgs ++ args)

parseOptions (('-':option):args) options freeArgs = do
  let check (prefix, _, ParamNo)  =  (option==prefix)
      check (prefix, _, _)        =  (startFrom prefix option /= Nothing)
  let accept (prefix, name, haveParam)  =  return (name, tryToSkip "=" (tryToSkip prefix option))
      unknown                           =  registerError$ CMDLINE_UNKNOWN_OPTION ('-':option)
      ambiguous variants                =  registerError$ CMDLINE_AMBIGUOUS_OPTION ('-':option) (map (('-':).fst3) variants)
  newopt <- case (filter check optionsDict) of
              [opt] -> accept opt  -- принять опцию
              []    -> unknown     -- неизвестная опция
              xs    -> -- При неоднозначности в разборе опции посмотрим на список предпочтительных опций
                       case (filter ((`elem` aPREFFERED_OPTIONS++aSUPER_PREFFERED_OPTIONS) . snd3) xs) of
                         [opt] -> accept opt        -- принять опцию
                         []    -> ambiguous xs      -- неоднозначная опция, которой нет в списке предпочтений
                         xs    -> -- Повторим трюк! :)
                                  case (filter ((`elem` aSUPER_PREFFERED_OPTIONS) . snd3) xs) of
                                    [opt] -> accept opt        -- принять опцию
                                    []    -> ambiguous xs      -- неоднозначная опция, которой нет в списке предпочтений
                                    xs    -> ambiguous xs      -- неоднозначный разбор даже в списке предпочтений!

  parseOptions args (newopt:options) freeArgs

parseOptions (arg:args) options freeArgs   =  parseOptions args options (arg:freeArgs)


-- |Вернуть список значений опции с названием `flag`. Пример вызова: findReqList opts "exclude"
findReqList ((name, param):flags) flag  | name==flag  =  param: findReqList flags flag
findReqList (_:flags) flag                            =  findReqList flags flag
findReqList [] flag                                   =  []

-- |Вернуть значение опции с названием `flag`, если её нет - значение по умолчанию `deflt`
findReqArg options flag deflt  =  last (deflt : findReqList options flag)

-- |Вернуть значение опции с необязательным параметром
findOptArg = findReqArg

-- |Вернуть значение опции с названием `flag`, если её нет - Nothing
findMaybeArg options flag  =  case findReqList options flag
                                of [] -> Nothing
                                   xs -> Just (last xs)

-- |Вернуть True, если в списке опций есть опция с названием `flag`
findNoArg options flag  =  case findReqList options flag
                                of [] -> False
                                   _  -> True

-- |Вернуть Just True, если в списке опций есть опция с названием `flag1`,
--          Just False, если в списке опций есть опция с названием `flag2`,
--          Nothing, если нет ни той, ни другой
findNoArgs options flag1 flag2  =  case filter (\(o,_) -> o==flag1||o==flag2) options
                                     of [] -> Nothing
                                        xs -> Just (fst (last xs) == flag1)

{-# NOINLINE optionsDict #-}
{-# NOINLINE optionsHelp #-}
{-# NOINLINE parseOptions #-}
{-# NOINLINE findReqList #-}
{-# NOINLINE findReqArg #-}
{-# NOINLINE findMaybeArg #-}
{-# NOINLINE findNoArg #-}
{-# NOINLINE findNoArgs #-}

----------------------------------------------------------------------------------------------------
---- System information ----------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Number of physical processors/cores in the system. Determines number of heavy-computations thread runned
foreign import ccall unsafe "Environment.h GetProcessorsCount"
  getProcessorsCount :: CInt

-- |Size of physical computer memory in bytes
foreign import ccall unsafe "Environment.h GetPhysicalMemory"
  getPhysicalMemory :: CUInt

-- |Size of physical computer memory that is currently unused
foreign import ccall unsafe "Environment.h GetAvailablePhysicalMemory"
  getAvailablePhysicalMemory :: CUInt

