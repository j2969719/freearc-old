---------------------------------------------------------------------------------------------------
---- ����������� ��������� ������ � ����� ������/����� �� ����������.                          ----
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


-- |�������� ����������� �������
data Command = Command {
    cmd_command_text         :: !String             -- ������ ����� �������
  , cmd_additional_args      :: !String             -- �������������� �����, ����������� �� ���������� ����� � ������-�����
  , cmd_name                 :: !String             -- �������� �������
  , cmd_arcspec              ::  String             -- ����� �������
  , cmd_arclist              ::  [FilePath]         --   ����� ���� ��������� �� ���� ����� (� ��������, ����������) �������
  , cmd_arcname              ::  FilePath           --   ��� ��������������� ������ (�� ����� ������� � wildcards � cmd_arcspec ����������� ��������� ������ � ���������� ������ ��������������� ������)
  , cmd_archive_filter       :: (FileInfo -> Bool)  -- �������� ������ ������ �� ������������ �������
  , cmd_filespecs            :: ![String]           -- ������������ ����������� ������� ��� ������
  , cmd_added_arcnames       :: !(IO [FilePath])    --   ����������, ������������ ����� ����������� ������� (������� "j")
  , cmd_diskfiles            :: !(IO [FileInfo])    --   ����������, ������������ ����� ����������� ������  (��������� ������� ��������/���������� �������)
  , cmd_subcommand           :: !Bool               -- ����������? (��������, ������������ ����� ���������)
  , cmd_setup_command        :: !(IO ())            -- ��������, ������� ���� ��������� ��������������� ����� ������� ��������� ���� ������� (���� ��� �� ��� ������)
                                                    -- �����:
  , opt_scan_subdirs         :: !Bool               --   ����������� ����� ������?
  , opt_add_dir              :: !Bool               --   �������� ��� ������ � ����� ��������, ���� ���������� ����������?
  , opt_add_exclude_path     :: !Int                --   ��������� ��� �������� �������� / ��������� ���������� ���� (��� ������ ������������ ������ �� �����)
  , opt_dir_exclude_path     :: !Int                --   ��������� ��� �������� �������� / ��������� ���������� ���� (��� ������ �������� ������)
  , opt_arc_basedir          :: !String             --   ������� ������� ������ ������
  , opt_disk_basedir         :: !String             --   ������� ������� �� �����
  , opt_group_dir            :: ![Grouping]         --   ����������� ������ ��� �������� ������
  , opt_group_data           :: ![Grouping]         --   ����������� ������ ��� �����-�����
  , opt_data_compressor      :: !UserCompressor     --   ������ ������ ��� ������
  , opt_dir_compressor       :: !Compressor         --   ����� ������ ��� ������ ��������
  , opt_arccmt_file          :: !String             --   ����, �� �������� �������� (� ������� �������) ����������� � ������
  , opt_include_dirs         :: !(Maybe Bool)       --   �������� �������� � ���������? (��/���/�� ���������������)
  , opt_indicator            :: !String             --   ��� ���������� ��������� ("0" - �����������, "1" - ��������� �� ���������, "2" - ����� � ��������� ������ ����� ������� ��������������� �����)
  , opt_display              :: !String             --   ���������� �����, ����������� ����� ������ �������� �� �����
  , opt_overwrite            :: !(IORef String)     --   ��������� ������� � ������������ � ���������� ������ ("a" - �������������� ���, "s" - ���������� ���, ����� ������ - �������� �������)
  , opt_sfx                  :: !String             --   ��� SFX-������, ������� ���� ������������ � ������ ("-" - �����������, ���� ��� ����, "--" - ����������� ������������)
  , opt_keep_time            :: !Bool               --   ��������� mtime ������ ����� ���������� �����������?
  , opt_time_to_last         :: !Bool               --   ���������� mtime ������ �� mtime ������ ������� ����� � ��?
  , opt_keep_broken          :: !Bool               --   �� ������� �����, ������������� � ��������?
  , opt_test                 :: !Bool               --   �������������� ����� ����� ��������?
  , opt_pretest              :: !Int                --   ����� ������������ ������� _�����_ ����������� �������� (0 - ���, 1 - ������ recovery info, 2 - recovery ��� full, 3 - full testing)
  , opt_lock_archive         :: !Bool               --   ������� ����������� ����� �� ���������� ���������?
  , opt_match_with           :: !(PackedFilePath -> FilePath)  -- ������������ ��� ���������� ����� � fpBasename ��� fpFullname
  , opt_append               :: !Bool               --   ��������� ����� ����� ������ � ����� ������?
  , opt_recompress           :: !Bool               --   ������������� ������������ ��� �����?
  , opt_keep_original        :: !Bool               --   �� �������������� �� ������ �����?
  , opt_noarcext             :: !Bool               --   �� ��������� ����������� ���������� � ����� ������?
  , opt_nodir                :: !Bool               --   �� ���������� � ����� ��� ���������� (��� ����������)?
  , opt_update_type          :: !Char               --   �������� ���������� ������ (a/f/u/s)
  , opt_x_include_dirs       :: !Bool               --   �������� �������� � ��������� (��� ������ ��������/����������)?
  , opt_no_nst_filters       :: !Bool               --   TRUE, ���� � ������� ����������� ����� ������ ������ �� �����/�������/������� (-n/-s../-t..)
  , opt_file_filter          :: !(FileInfo -> Bool) --   �������������� ������� �������� ������ ������ �� ���������/�������/�������/����� (��, ����� filespecs)
  , opt_sort_order           :: !String             --   ������� ���������� ������ � ������
  , opt_reorder              :: !Bool               --   ��������������� ����� ����� ���������� (�������� ����� ����������/������� �����)?
  , opt_find_group           :: !(FileInfo -> Int)  --   �������, ������������ �� FileInfo � ����� ������ (�� arc.groups) ��������� ������ ����
  , opt_groups_count         :: !Int                --   ���������� ����� (`opt_find_group` ���������� ���������� � ��������� 0..opt_groups_count-1)
  , opt_find_type            :: !(FileInfo -> Int)  --   �������, ������������ �� FileInfo � ������ ���� ������ (�� ������������� � `opt_data_compressor`) ��������� ������ ����
  , opt_types_count          :: !Int                --   ���������� ����� ������ (`opt_find_type` ���������� ���������� � ��������� 0..opt_types_count-1)
  , opt_logfile              :: !String             --   ��� ���-����� ��� ""
  , opt_delete_files         :: !DelOptions         --   ������� �����/�������� ����� �������� ���������?
  , opt_workdir              :: !String             --   ������� ��� ��������� ������ ��� ""
  , opt_clear_archive_bit    :: !Bool               --   �������� ������� Archive � ������� ����������� ������ (� ������, ������� ��� ���� � ������)
  , opt_recovery             :: !String             --   �������� Recovery ����� (� ���������, ������ ��� ��������)
  , opt_broken_archive       :: !String             --   ������������ ����������� �����, ��������� �������� ��� � ������� ���������� ���������� ������
  , opt_original             :: !String             --   ������������� � ���������� URL ������� ����� ������
  , opt_save_bad_ranges      :: !String             --   �������� � �������� ���� ������ ����������� ������ ������ ��� �� �����������
  , opt_cache                :: !Int                --   ������ ������ ������������ ������
  , opt_limit_compression_memory   :: !MemSize      --   ����������� ������ ��� ��������, ����
  , opt_limit_decompression_memory :: !MemSize      --   ����������� ������ ��� ����������, ����

                                                    -- ��������� ����������:
  , opt_encryption_algorithm :: !String             --   �������� ����������
  , opt_cook_passwords                              --   �������������� ������� � ������������� ����������, ����������� � ������������ ������ � �������� keyfile (�� ������ ����������� ������, ��� ������� ���������� ����� �������, ������� �� ����� ���� ��������� � parseCmdline)
                             :: !(Command -> (IO String, IO String) -> IO Command)
  , opt_data_password        :: String              --   ������, ������������ ��� ���������� ������ (�������� � ���� ���� � ���������� � ���������� keyfiles). "" - ������������� �� �����
  , opt_headers_password     :: String              --   ������, ������������ ��� ���������� ���������� (ditto)
  , opt_decryption_info                             --   ����������, ������������ ���������� ������� ����� ����������:
                             :: ( Bool              --     �� ����������� � ������������ ����� ������, ���� ���� ��� ��������� ��� ���������� ������ �� ��������?
                                , MVar [String]     --     ������ "������ �������", �������� �� �������� ������������ ��������������� ������
                                , [String]          --     ���������� keyfiles, ����������� � �������
                                , IO String)        --     ask_decryption_password

  -- �������� ������/������ ������ � ���������, ������������� ������ -sc
  , opt_parseFile   :: !(Domain -> FilePath -> IO [String])      -- ��������� �������� ����� � ������������� � -sc ���������� � ��-����������� ���������� �� ������
  , opt_unParseFile :: !(Domain -> FilePath -> String -> IO ())  -- ��������� ������ ����� � ������������� � -sc ����������
  }

-- |����������� ����� --debug
opt_debug cmd = cmd.$opt_display.$(`contains_one_of` "$#")


-- |������ �����, �������������� ����������
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
   ,OPTION "ds"    "sort"               "sort files in ORDER"                      -- to do: ������� ��� ����� OptArg
   ,OPTION ""      "groups"             "name of groups FILE"                      -- to do: ������� ��� ����� OptArg
   ,OPTION "s"     "solid"              "GROUPING for solid compression"           -- to do: ������� ��� ����� OptArg
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
   ,OPTION "sfx"   ""                  ("add sfx MODULE (\""++aDEFAULT_SFX++"\" by default)")  -- to do: ������� ��� ����� OptArg
   ,OPTION "z"     "arccmt"             "read archive comment from FILE or stdin"  -- to do: ������� ��� ����� OptArg
   ,OPTION "i"     "indicator"          "select progress indicator TYPE (0/1/2)"   -- to do: ������� ��� ����� OptArg
   ,OPTION "ad"    "adddir"             "add arcname to extraction path"
   ,OPTION "ag"    "autogenerate"       "autogenerate archive name with FMT"       -- to do: ������� ��� ����� OptArg
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

-- |������ �����, ������� ���� �������� ������������ ��� ������������� �������� � ������� ��������� ������
aPREFFERED_OPTIONS = words "method sfx charset SizeMore SizeLess overwrite"

-- |����� �� ����������� ������, ������� ������������ ��������� :)
aSUPER_PREFFERED_OPTIONS = words "OldKeyfile"


-- |�������� ������, �������������� ����������
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

-- |������ ������, �������������� ����������
aLL_COMMANDS = map (head.words) commandsList

-- |������ ������, ������� ������ �������� �����
cOPYING_COMMANDS = words "ch s k c rr"

-- |������������� ���� ������ �� ������ �����: ������� ��������, ����������, ������������ � ��������
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

-- |������ ����������, ������������ � HEADER BLOCK
aARCHIVE_VERSION = make4byte 0 0 4 0

-- |������� ������������ ���������, ��������� � ������ ������
aARC_HEADER = "ARC 0.40 "
{-# NOINLINE aARC_HEADER #-}

-- |HELP, ��������� ��� ������ ��������� ��� ����������
aHELP = aARC_HEADER++" http://freearc.sourceforge.net  Jan 1 2008\n"++
        "Usage: Arc command [options...] archive [files... @listfiles...]\n" ++
        joinWith "\n  " ("Commands:":commandsList) ++ "\nOptions:\n" ++ optionsHelp

-- |������� ����������� ������ ��� �����-����� ��� ���������� ������
data Grouping = GroupNone                   -- ������ ���� ��������
                                            -- ����������� ��:
              | GroupByExt                  --   ����������� ����������
              | GroupBySize      FileSize   --   ������������ ������ ����� ������
              | GroupByBlockSize MemSize    --   ������������� ������ ����� ������ (��� ������-��������������� ����������, ����� ��� BWT � ST)
              | GroupByNumber    FileCount  --   ���������� ������
              | GroupAll                    -- ��� ����� ������

-- |�������� ����� -d[f]: �� �������, ������� ������ �����, ������� ����� � ��������
data DelOptions = NO_DELETE | DEL_FILES | DEL_FILES_AND_DIRS  deriving (Eq)


---------------------------------------------------------------------------------------------------
-- ��������, ������������ �� ��������� ------------------------------------------------------------
---------------------------------------------------------------------------------------------------

-- |����� ������ ������
aDEFAULT_COMPRESSOR = "4"

-- |����� ������ �������� ������
aDEFAULT_DIR_COMPRESSION = "lzma:1m"

-- |������ �����-������
aDEFAULT_DATA_GROUPING  =  "1gb"

-- |����������� ��� ���������
aDEFAULT_DIR_GROUPING  =  GroupByNumber (20*1000)

-- |�������� ������ ������, ������������ �� ���������
aDEFAULT_ENCRYPTION_ALGORITHM = "aes"

-- |���� � ��������� ������ �� ������� ����� �������������� ������ - ������������ ���, �.�. "*"
aDEFAULT_FILESPECS = [reANY_FILE]

-- |���������� �������� ������
aDEFAULT_ARC_EXTENSION = ".arc"

-- |���������� SFX �������� ������
aDEFAULT_SFX_EXTENSION = ".exe"

-- |���� � ��������� ������� ���������� ��� ������ ��� "-og"
aDEFAULT_GROUPS_FILE = "arc.groups"

-- |SFX-������, ������������ �� ���������
aDEFAULT_SFX = "arc.sfx"

-- |���� ������������ (�������� �����, ������������ �� ���������)
aCONFIG_FILE = "arc.ini"

-- |���������� �����, ���������� �����, ������������ �� ���������
aCONFIG_ENV_VAR = "FREEARC"

-- |������� ����������, ������������ ��� solid-������ (��� ���������� ������)
aDEFAULT_SOLID_SORT_ORDER = "gerpn"

-- |����� ����������, ��������� �� ����� - �� ��������� � ��� ������������� ����� "--display" ��� ���������.
-- �� ��������� �� ����� �� ��������� "cmo" - ���. �����, ����� ������ � ������������ ������
aDISPLAY_DEFAULT = "hanwrftsek"
aDISPLAY_ALL     = "hoacmnwrfdtsek"

-- ������ arc.ini
compressionMethods = "[Compression methods]"
defaultOptions     = "[Default options]"
externalCompressor = "[External compressor:*]"

-- |���������� ����� ������ � ����������������� �����
cleanupSectionName  =  strLower . filter (not.isSpace)

-- |�������� ����, ��� ��� - ��������� ������
selectSectionHeadings  =  ("["==) . take 1 . trim


-- |��������� ��������� ������ � ���������� ������ �������� � ��� ������ � ���� �������� Command.
-- ������ ������� �������� ��� �������, ������������ �������, ������ ������������ ������ � �����.
-- ������� ����������� " ; ", �������� "a archive -r ; t archive ; x archive"
parseCmdline cmdline  =  (`mapMaybeM` split ";" (map cmdline2str cmdline)) $ \args -> do
  display_option' =: aDISPLAY_DEFAULT  -- ���������� display_option � �������� �� ���������, ��������� ������� ������ ����� �� �������������
  let options = takeWhile (/="--") $ filter (match "-*") args
  -- ���� ��������� ������ �� �������� ������ ����� ����� - ���������� help/������������ � �����
  if args==options then do
      putStr $ if options `contains` "--print-config"
                 then unlines ("":";You can insert these lines into ARC.INI":compressionMethods:builtinMethodSubsts)
                 else aHELP
      return Nothing
    else do

  -- ��������� ����� �� ���������� ����� FREEARC ��� �������� � ����� -env
  (o0, _) <- parseOptions options [] []
  let no_configs = findReqList o0 "config" `contains` "-"
  env_options <- case (findReqArg o0 "env" "--") of
                    "--" | no_configs -> return ""  -- ����� -cfg- � ��������� ������ ��������� ������������� � arc.ini, � %FREEARC
                         | otherwise  -> getEnv aCONFIG_ENV_VAR  `catch`  (\e -> return "")
                    "-"               -> return ""
                    env               -> getEnv env

  -- ��������� ������-���� arc.ini ��� ��������� ������ -cfg
  (o1, _)  <- parseOptions (words env_options++options) [] []   -- ����� -cfg ����� ���� ������ � ��������� ������ ��� � ���������� �����
  cfgfile <- case (findReqArg o1 "config" "--") of
               "--" -> findFile configFilePlaces aCONFIG_FILE
               "-"  -> return ""
               cfg  -> return cfg
  -- ���������� ����� --charset/-sc, ����� ���������� ��������� ��� ������ ������-�����
  let (_, parseFile1, _)  =  parse_charset_option (findReqList o1 "charset")
  -- ��������� ����� �� ������-�����, ���� �� ����, � ������ �� ���� ������ ������ � �����������
  config  <-  cfgfile  &&&  parseFile1 'i' cfgfile >>== map trim >>== deleteIfs [null, match ";*"]

  -- ��� ����������� ���������� ���������� ������-����� � ����� ������,
  -- ���������� ������� ����� ���� ��������� �������� configSection.
  -- � �������, configSection "[Compression methods]" - ������ ����� � ������ "[Compression methods]"
  let configSections = map makeSection $ makeGroups selectSectionHeadings config
      makeSection (x:xs) = (cleanupSectionName x, xs)
      configSection name = lookup (cleanupSectionName name) configSections `defaultVal` []
      -- ������������ ����� ������/���. ���������, ��������� ��������� �� ������ "[Compression methods]"
      decode_compression_method = decode_method (configSection compressionMethods)
      decode_methods s = ("0/"++s).$decode_compression_method.$lastElems (length (elemIndices '/' s) + 1)

  -- � ��� ����������� ��������� �������� �� ������ ������� � �������� ������,
  -- ������� ������, ����� ����� ������� ����������� �������� ��������� ����,
  -- ������� ����� ���� � �� �� �����������,
  -- � ����� ����������� ����������� (� ���� ������ ���� ����� ��� ������).
  -- ������:
  --   a create j = -m4x -ms
  --   a = --display
  -- � ���� ������ (configElement section "a") ��������� "-m4x -ms --display"
  let sectionElement name = unwords . map snd
                              . filter (strLowerEq name . fst)
                              . concatMap (\line -> let (a,b)  =  split2 '=' line
                                                    in  map (\w->(w,trim b)) (words$ trim a))
      configElement section element  =  configSection section .$ sectionElement element

  -- ���� ������ �������� ������ ������-����� �� �������� ���������� ������, ��
  -- ��� ��������� �����, ����� ��� ���� ������
  let config_1st_line  =  case (head1 config) of
                              '[' : _  -> ""    -- ��� ��������� ������
                              str      -> str

  -- ��� �������: "a", "create" � ��� �����. ����� �� ��������� ��� ���� �������, �������� � ������-�����
  let cmd = head1$ filter (not.match "-*") args
      default_cmd_options = configElement defaultOptions cmd

  -- ������� � ������ ��������� ������ ����� �� ��������� ��� ���� ������,
  -- ����� �� ��������� ��� ���� ������� � ���������� ���������� �����
  let additional_args  =  concatMap words [config_1st_line, default_cmd_options, env_options]

  -- ������� ��������� ������, ������� ����� ����� � ������ "��������� ����������"
  (o, freeArgs)  <-  parseOptions (additional_args++args) [] []
  -- �������� �� ������, ���� "��������� ����������" ������ ���� - ����������� ������� ��� ��� ������
  case freeArgs of
    []     ->  registerError$ CMDLINE_NO_COMMAND args
    [cmd]  ->  registerError$ CMDLINE_NO_ARCSPEC args
    otherwise -> return ()
  let (cmd:pure_arcspec:pure_filespecs) = freeArgs
                               -- ���������:  �������� ����� � �������� �� ���������
  let grouping              =  findReqArg   o "solid" aDEFAULT_DATA_GROUPING .$ parseSolidOption
      group_dir             =  fst3 grouping
      group_data            =  snd3 grouping
      defaultDirCompressor  =  thd3 grouping ||| aDEFAULT_DIR_COMPRESSION
      orig_dir_compressor   =  findReqArg   o "dirmethod"  defaultDirCompressor .$ decode_compression_method
      compression_options   =  findReqList  o "method"
      orig_sort_order       =  findMaybeArg o "sort"
      yes                   =  findNoArg    o "yes"
      autogenerate_arcname  =  findOptArg   o "autogenerate"  "--" ||| "%Y%m%d%H%M%S"
      indicator             =  findOptArg   o "indicator"     "1"  ||| "0"   -- �� ��������� -i1; -i ������������ -i0
      arccmt_file           =  findOptArg   o "arccmt"        (if cmd=="c"  then ""  else "--")   -- ������� "c" ������������ ������� "ch -z"
      recovery              =  findOptArg   o "recovery"      (if take 2 cmd=="rr"  then drop 2 cmd  else "--")   -- ������� "rr..." ������������ ������� "ch -rr..."
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

  -- ������ ��������, ������� ���� ��������� ��������������� ����� ������� ���������� �������
  setup_command <- newList
  setup_command <<= (url_setup_proxy      .$ withCString (replace ',' ' ' url_proxy))
  setup_command <<= (url_setup_bypass_list.$ withCString (replace ',' ' ' url_bypass))

  -- ������� ��������� ����� -o/-op
  let (op, o_rest) = partition is_op_option (findReqList o "overwrite")
      op_opt       = map  (tryToSkip "p") op
      overwrite    = last ("p":o_rest)
      is_op_option ('p':_:_) = True
      is_op_option _         = False

  -- ���������, ��� ����� ��������� ���� �� ���������� ��������
  testOption "overwrite"     "o"  overwrite      (words "+ - p")
  testOption "indicator"     "i"  indicator      (words "0 1 2")
  testOption "pretest"       "pt" pretest        (words "0 1 2 3")
  testOption "BrokenArchive" "ba" broken_archive (words "- 0 1")
  testOption "ExcludePath"   "ep" exclude_path   ([""]++words "1 2 3 --")

  -- ���������� ��� SFX-������, ������� ����� �������� � ������ ������
  sfx <- case (findOptArg o "sfx" (if cmd=="s" then "" else "--"))   -- ������� "s" ������������ ������� "ch -sfx"
          of ""  -> findFile libraryFilePlaces aDEFAULT_SFX    -- ������������ ������ SFX �� ��������� (arc.sfx �� ��������, ��� ��������� ���������)
             sfx -> return sfx
  when (sfx=="") $
    registerError$ GENERAL_ERROR$ "SFX module "++aDEFAULT_SFX++" is not found"

  -- ������� � �������� ����� ������ ����� ����/�������, ���� ������� ����� -ag
  current_time <- getClockTime
  let add_ag  =  case autogenerate_arcname of
                   "--" -> id
                   _    -> updateBaseName (++ showtime autogenerate_arcname current_time)

  -- ������� � ����� ������ ���������� �� ���������, ���� ��� ������� ���������� � �� ������������ ����� --noarcext
  let arcspec  =  addArcExtension noarcext$ add_ag pure_arcspec

  -- ���������� ������ ����� --charset/-sc, ��������� ������� ���������
  -- � ��������� ������/������ ������ � � ������
  let (charsets, parseFile, unParseFile)  =  parse_charset_option (findReqList o "charset")

  -- ������� ���������� ������ ����� --display
  let orig_display = foldl f aDISPLAY_DEFAULT (findReqList o "display")
      -- ������� ��������� ����� --display
      f value ""       =  aDISPLAY_ALL     -- -di ��� ���������� �������� �������� ����� ���� ����������
      f value "--"     =  aDISPLAY_DEFAULT -- -di-- �������� ������������ �������� �� ���������
      f value ('+':x)  =  nub (value++x)   -- -di+x �������� �������� x � ������
      f value ('-':x)  =  nub value \\ x   -- -di-x �������� ������ x �� ������
      f value x        =  nub x            -- ����� ������ ��������� �������� � �������� �����

  -- ��� ������� "lb" ��������� ��������� ����� ���. ���������� �� �����,
  -- ��� ������ ������ �������� �������� ����� ����� ������ � �������������� �������
  let display = case () of
                  _ | cmd=="lb"              ->  ""
                    | cmdType cmd==LIST_CMD  ->  orig_display++"a"
                    | otherwise              ->  orig_display
  -- ���������� display_option, ��������� ��� ��� ����� ������������ ��� ������ warning � ���������� external compressor section
  display_option' =: display
  -- ����� ������� ���������� ������� ������������ �������� display_option, ��������� ��� ����� ���� �������� ��� ��������/���������� ������ ������
  setup_command <<= (display_option' =: display)

  -- �������������� �������� ������� ����������� �� ������ [External compressor:...]
  let externalSections = filter (matchExternalCompressor.head) $ makeGroups selectSectionHeadings config
      matchExternalCompressor s = and[ head externalCompressor          ==    head s
                                     , init (tail externalCompressor) `match` init (tail s)
                                     , last externalCompressor          ==    last s]
  results <- mapM (addExternalCompressor.unlines) externalSections
  for (zip results externalSections) $ \ (result,section) -> do
      when (result/=1) $ do
          registerWarning (BAD_CFG_SECTION cfgfile section)


---------------------------------------------------------------------------------------------------
-- ����������� ��������� ������ -------------------------------------------------------------------
  -- ������ ������� ������, �������������� ������ ���� "75%" (�� ������ ���)
  -- ����� ������ ����������� �� ��������, ������� 4 ��, ����� ��������� ��������� ��������� ������� � ���������� �������� ��������� Shadow BIOS options
  let parsePhysMem = parseMemWithPercents (toInteger getPhysicalMemory `roundTo` (4*mb))

  -- ������ ����� -md
  let parseDict dictionary  =  case dictionary of
          [c]       | isAlpha c     ->  Just$ 2^(16 + ord c - ord 'a')   -- ����� ������ ����� ������, -mda..-mdz
          s@(c:_)   | isDigit c     ->  Just$ parsePhysMem s             -- ����� ���������� c �����: -md8, -md8m, -md10%
          otherwise                 ->  Nothing                          -- ����� - ��� �� ����� -md, � ����� -m, ������������ � -md...

  -- ����, ������� �������������� ��������� �����, ������������ �� "-m"
  method <- new "";    methods <- new "";  mc' <- newList;  dict <- new 0;
  mm'    <- new "--";  threads <- new 0
  for compression_options $ \option ->
    case option of
      -- ����� -m� ��������� ������ ��������� ��������� ��������� ������ (-mcd-, -mc-rep)
      'c':rest  | anyf [beginWith "-", endWith "-"] rest
                    ->  mc' <<= rest.$tryToSkip "-".$tryToSkipAtEnd "-"
                                    .$changeTo [("d","delta"), ("e","exe"),  ("l","lzp")
                                               ,("r","rep"),   ("z","dict")
                                               ,("a","$wav"),  ("c","$bmp"), ("t","$text")
                                               ]
      -- ����� -md ������������� ������ ������� ��� � ������ ������ RAR :)
      'd':rest  | Just md <- parseDict rest ->  dict =: md
      -- ����� -mm �������� ����� �����������-������
      'm':rest  | mmflag <- rest.$tryToSkip "=",
                  mmflag `elem` ["","--","+","-","max","fast"]  ->  mm' =: mmflag
      -- ����� -ms ����� ������������� �������� ������ ������ ��� ��� ������ ������
      "s"  ->  methods ++= "/$compressed=rep:8mb:128+tor:2:64k"
      -- ����� -mt ��������/��������� ��������������� � ������������� ���������� ������
      't':rest  | n <- rest.$changeTo [("-","1"), ("+","0"), ("","0"), ("--","0")],
                  all isDigit n  ->  threads =: readInt n
      -- ����� -m$type=method ������������� ��������� ������ ��� ��������� ����� ������
      '$':_ -> case (break (`elem` "=:.") option) of
                 (_type, '=':method) -> methods ++= '/':option                      -- -m$type=method: ������������ ����� ����� ���� �������� ������������
                 -- (_type, ':':names)  -> types  ++= split ':' names               -- -m$type:name1:name2: �������� � ������ ������ ����� ���� �������� �����
                 -- (_type, ',':exts)   -> types  ++= map ("*."++) $ split '.' exts -- -m$type.ext1.ext2: �������� ���������� � ������ ����
                 otherwise -> registerError$ CMDLINE_BAD_OPTION_FORMAT ("-m"++option)
      -- ��� ��������� �����, ������������ �� -m0= ��� ������ -m, ������ �������� ����� ������
      m  ->  method =: m.$tryToSkip "0="
  -- ��������� ������������� �������� ����������
  dictionary  <- val dict       -- ������ ������� (-md)
  cthreads    <- val threads    -- ���������� compression threads (-mt)
  mainMethod  <- val method     -- �������� ����� ������
  userMethods <- val methods    -- �������������� ������ ��� ���������� ����� ������ (-m$/-ms)
  mm          <- val mm'        -- �����������-������
  mc          <- listVal mc'    -- ������ ���������� ������, ������� ��������� ���������

  -- ����� ������� ���������� ������� �������� � ���������� �������� ���������� ������, ������� ��� ������ ������������
  setup_command <<= (setCompressionThreads$  cthreads ||| i getProcessorsCount)   -- By default, use number of threads equal to amount of available processors/cores

  -- ����������� �� ������ ��� ��������/����������
  let climit = parseLimit$ findReqArg o "LimitCompMem"   "--"
      dlimit = parseLimit$ findReqArg o "LimitDecompMem" "--"
      parseLimit x = case x of
        "--" -> parsePhysMem "75%"  -- �� ���������: ���������� ������������� ������ 75% � ����������� ������
        "-"  -> maxBound::MemSize   -- �� ������������ ������������� ������
        s    -> parsePhysMem s      -- ���������� ������������� ������ �������� �������

  -- ���������� �����������-�������
  let multimedia mm = case mm of
        "-"    -> filter ((`notElem` words "$wav $bmp").fst)    -- ������ ������ $wav � $bmp �� ������ ������� ������
        "fast" -> (++decode_methods "$wav=wavfast/$bmp=bmpfast") . multimedia "-"
        "max"  -> (++decode_methods "$wav=wav/$bmp=bmp")         . multimedia "-"
        "+"    -> \m -> case () of
                          _ | m.$isFastDecompression  -> m.$multimedia "fast"
                            | otherwise               -> m.$multimedia "max"
        ""     -> multimedia "+"
        "--"   -> id

  -- �������� ��������� ��������� ������
  let method_change mc x = case mc of
        '$':_  -> -- ������ ������ mc (��������, "$bmp") �� ������ ������� ������
                  x.$ filter ((/=mc).fst)
        _      -> -- ������ ������, � ������� mc - ��������� �������� ������ (�������� -mc-tta ������� � �������� �����, ������� ������ ������� ������������� ���������� tta)
                  x.$ (\(x:xs) -> x:(xs.$ filter ((/=mc).method_name.last1.snd)))   -- �� ������� �������� ������ ������ (������ ������)
                  -- ������ �������� mc �� ��������� ������� ������
                   .$ map (mapSnd$ filter ((/=mc).method_name))

  -- ���� ������ ����� "--nodata", �� ������������ ������ ������.
  -- ���� ������ ����� "--crconly", �� ������������ ��������� CRC ������������ ������.
  -- � ��������� ������ ���������� ��������� �������� � �������������� ��������� ������,
  -- �������� �����������-������ � ������ �������, ������ ����������� ���������,
  -- � ��������� ����������� ������
  let data_compressor = if      nodata   then [("", [aFAKE_COMPRESSION])]
                        else if crconly  then [("", [aCRC_ONLY_COMPRESSION])]
                        else ((mainMethod ||| aDEFAULT_COMPRESSOR) ++ userMethods)
                               .$ decode_compression_method
                               .$ multimedia mm
                               .$ applyAll (map method_change mc)
                               .$ compressorSetDictionary dictionary
                               .$ compressorLimitCompressionMem   climit
                               .$ compressorLimitDecompressionMem dlimit

  -- ���������� ������ �������� ����� ������� � ������� � �������� ������� ������
  let dir_compressor = orig_dir_compressor.$ compressorLimitCompressionMem   climit
                                          .$ compressorLimitDecompressionMem dlimit
                                          .$ getMainCompressor
                                          .$ last .$ (:[])

  -- ����. ������ ����� � ������������ ������� ������������ ��� 0
  let maxBlockSize = compressorGetBlockSize data_compressor
  -- ������, ��������� ��� ��������� ������
  let compressionMem = compressorGetCompressionMem data_compressor

  -- ���������, ������� ������ ����� ������������ ��� ����� ������������ ������ ������.
  -- ���� ������ ���� �� ����� ���� ������ --cache, �� ���������� �� 1 �� �� 16 ��,
  -- �������� ������� ���, ����� ����� ����������� ������ ���������� �� ������������
  -- �������� �� � ����������� ������ (�� ������ ������, ����������� ��� ���������� ������
  -- � ����������� �������). ����������, ��� ������� ����������� ������������� memory-intensive
  -- tasks (� � ���������, ����������� ���������� ������ FreeArc) ��� ������� �� ����� ������.
  -- ����� ���� �� �������� �� ����� *����������* ����������� ��� � ������ ������� ���������
  let minCache  =  1*mb                             -- ���. ������ ����  - 1  ��
      maxCache  =  (16*mb) `atLeast` maxBlockSize   -- ����. ������ ���� - 16 �� ��� ������ ����� ��� ��������� ��������� (lzp/grzip/dict)
      availMem  =  if parsePhysMem "50%" >= compressionMem      -- "�������� ������" = 50% ��� ����� ������, ��������� ��� ������
                       then parsePhysMem "50%" - compressionMem
                       else 0
      cache     =  clipToMaxInt $ atLeast aBUFFER_SIZE $  -- ��� ������ �������� ��� ������� ���� �����
                       case (findReqArg o "cache" "--") of
                           "--" -> availMem.$clipTo minCache maxCache
                           "-"  -> aBUFFER_SIZE
                           s    -> parsePhysMem s

  -- ������������� �������� ����� --recompress ��� ������, ���������� �����,
  -- ���� ������� ����� -m../--nodata/--crconly
  let recompress = findNoArg o "recompress"
                   || (cmd `elem` cOPYING_COMMANDS && (mainMethod>"" || nodata || crconly))
  -- �� �������������� ������������ �����-����� � ������ ��� --append
  -- � � �������� ����������� ������, ���� ��� ����� ������
  let keep_original = findNoArg o "append"
                      || (cmd `elem` cOPYING_COMMANDS  &&  not recompress)


---------------------------------------------------------------------------------------------------
-- ��������� ��� ����������� ������ ������ (find_group) � ���� ����� (find_type) ------------------
  -- ����������, ����� ���� �� ������� ����� (���� arc.groups) ����� ��������������
  actual_group_file <- case (findReqArg o "groups" "--") of
      "--" -> findFile configFilePlaces aDEFAULT_GROUPS_FILE  -- ������������ ���� ����� �� ��������� (arc.groups �� ��������, ��� ��������� ���������)
      "-"  -> return ""      -- ���� ����� �������� ������    --groups-
      x    -> return x       -- ���� ����� ������ ���� ������ --groups=FILENAME

  -- ��������� ������ ����� �� ����� �����
  group_strings  <-  if actual_group_file > ""
                         then parseFile 'i' actual_group_file      -- ���������� ���� ����� � ������ ��������� �������� � ������������ �����
                                >>== map translatePath             -- ���������� ��� '\' � '/'
                                >>== deleteIfs [match ";*", null]  -- ������� ������ ������������ � ������
                         else return [reANY_FILE]     -- ���� ���� ����� �� ������������, �� ��� ����� ����������� ����� ����� ������
  -- ������ ����������, ����������� ��������� � ������ ������
  let group_predicates  =  map (match_FP fpBasename) group_strings
  -- ������ �� ���������, ���� �������� ��� �����, �� ����������� �� � ����� �� �����.
  -- ����������� ������-������ "$default", ��� � ���������� ���������, ��� ��� ����� ��������� � ����� ������
  let lower_group_strings = (map strLower group_strings) ++ ["$default"]
      default_group = "$default" `elemIndex` lower_group_strings .$ fromJust
  -- ������� "PackedFilePath -> ����� ������ �� arc.groups"
  let find_group    = findGroup group_predicates default_group

  -- ������ ����� ������ ($text, $exe � ��� �����), ��������������� ������ ������ �� arc.groups
  let group_type_names = go "$binary" lower_group_strings  -- ��������� ������ - "$binary"
      go t []     = []           -- ������ �� ������ �����, ������� ����� ������
      go t (x:xs) = case x of    --   �� �������������� �� ����� ����� ������ ("$text", "$rgb" � ��� �����)
                      '$':_ | x/="$default" -> x : go x xs
                      _                     -> t : go t xs
  -- ������ ������� ������� ������ �� ������ `data_compressor`, ��������������� ������ ������ �� arc.groups
  let group_types =  map typeNum group_type_names
      typeNum t   =  t `elemIndex` (map fst data_compressor) `defaultVal` 0
  -- ������ ����������, ����������� ��� ���� ����������� ������ �� �����, ������������� � `data_compressor`
  let type_predicates  =  const False : map match_type [1..maximum group_types]
      match_type t     =  any_function$ concat$ zipWith (\a b->if a==t then [b] else []) group_types group_predicates
  -- ������� "PackedFilePath -> ����� ����������� � ������ `data_compressor`"
  let find_type  =  findGroup type_predicates 0


-------------------------------------------------------------------------------------
-- ������ ������
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

  -- ������� ������ �� ����-����� (@listfile/-n@listfile/-x@listfile) �� ����������
  listed_filespecs <- pure_filespecs   .$ replace_list_files parseFile >>== map translatePath
  include_list     <- orig_include_list.$ replace_list_files parseFile >>== map translatePath
  exclude_list     <- orig_exclude_list.$ replace_list_files parseFile >>== map translatePath

  -- ��������� ������ ���������� (-n) � ����������� (-x) ������. ��� -n ��������� orig_include_list, ��������� ��� ������ ��������� �� ���� ���� �� ������ ��������� ������
  let match_included  =  orig_include_list &&& [match_filespecs match_with include_list]
      match_excluded  =  exclude_list      &&& [match_filespecs match_with exclude_list]

#if defined(FREEARC_WIN)
  -- ����� ������ �� ���������
  let attrib_filter | select_archive_bit = [\attr -> attr.&.fILE_ATTRIBUTE_ARCHIVE /= 0]
                    | otherwise          = []
#else
  let attrib_filter = []
#endif

  -- ����� ������ �� �������
  let size_filter _  "--"   = []
      size_filter op option = [`op` parseSize option]

  -- ����� ������ �� ������� �����������, time � ������� YYYYMMDDHHMMSS
  let time_filter _  "--" = []
      time_filter op time = [`op` (time.$makeCalendarTime.$toClockTime.$convert_ClockTime_to_CTime)]
      -- ����������� ������� ���� YYYY-MM-DD_HH:MM:SS � CalendarTime � ���������� ���������� ctTZ � ����������� �� � ������� ���� (��� ����� toCalendarTime.toClockTime �������� ������)
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

  -- ����� ������ �� "��������", time � ������� [<ndays>d][<nhours>h][<nminutes>m][<nseconds>s]
  let oldness_filter _  "--" = []
      oldness_filter op time = [`op` (time.$calcDiff.$(`addToClockTime` current_time).$convert_ClockTime_to_CTime)]

      calcDiff  =  foldl updateTD noTimeDiff . recursive (spanBreak isDigit)
      updateTD td x = case (last x) of
                        'd' -> td {tdDay  = -readInt (init x)}
                        'h' -> td {tdHour = -readInt (init x)}
                        'm' -> td {tdMin  = -readInt (init x)}
                        's' -> td {tdSec  = -readInt (init x)}
                        _   -> td {tdDay  = -readInt x}

  -- ������ ������ ������, ���������� ��� �������� ������,
  -- ��������� � ��������� ������, ����� ������ �� filespecs.
  -- ��� ��������� ������������ ��������� �������,
  -- ������ ��� ��� ��-������� ������������ � �������� ������� ����.
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

  -- ���� ����� �������������� ������ �� ������� � ������� �� cw/d, �� ������������ ��� �����
  filespecs <- case listed_filespecs of
      [] | cmd `elem` (words "cw d")  ->  registerError$ CMDLINE_NO_FILENAMES args
         | otherwise                  ->  return aDEFAULT_FILESPECS
      _                               ->  return listed_filespecs

  -- �������� �������� � ���������? ��� ���������� ������������ ������ ��� ��������/����������
  let x_include_dirs  =  case include_dirs of
           Just x  -> x   -- � ������������ � ������ --dirs/--nodirs
           _       -> -- ��, ���� �������������� ��� �����, ��� �������� -n/-s*/-t* � ������� �� "e"
                      filespecs==aDEFAULT_FILESPECS && null nst_filters && cmd/="e"


-------------------------------------------------------------------------------------
-- ����������
  -- �������� ����������; �������� ���������� � ���������� � ������������� ���� ("aes" -> "aes-256/ctr")
  let ea = findReqArg o "encryption" aDEFAULT_ENCRYPTION_ALGORITHM
  encryptionAlgorithm <- join_compressor ==<< (foreach (split_compressor ea) $ \algorithm -> do
    unless (compressionIs "encryption?" algorithm) $ do
      registerError$ CMDLINE_GENERAL$ algorithm++" - error in encryption algorithm name or parameters"
    return$ canonizeCompressionMethod algorithm)

  -- ������ ��� ������ � ��������� ������
  let (dpwd,hpwd) = case (findReqArg o "password"        "--" .$changeTo [("-", "--")]
                         ,findReqArg o "HeadersPassword" "--" .$changeTo [("-", "--")])
                    of
                       (p,    "--")  ->  (p,  "--")    --  -p...
                       ("--", p   )  ->  (p,  p   )    --  -hp..,
                       (p,    ""  )  ->  (p,  p   )    --  -p[PWD] -hp
                       ("",   p   )  ->  (p,  p   )    --  -p -hpPWD
                       (p1,   p2  )  ->  (p1, p2  )    --  -pPWD1 -hpPWD2

  -- ��������� ������ �������, ����������� ��� ����������, ���� ������� -op-/-p-/-hp-
  let dont_ask_passwords  =  last ("":op_opt) == "-" || findReqArg o "OldPassword" "" == "-"  ||  findReqArg o "password" "" == "-"  ||  findReqArg o "HeadersPassword" "" == "-"
  -- ������ �������, ������������ ��� ����������
  mvar_unpack_passwords  <-  newMVar$ deleteIfs [=="",=="?",=="-",=="--"]$ op_opt ++ findReqList o "OldPassword" ++ findReqList o "password" ++ findReqList o "HeadersPassword"
  -- ���������� �������� ������, ������������ ��� ����������
  oldKeyfileContents     <-  mapM fileGetBinary (findReqList o "OldKeyfile" ++ findReqList o "keyfile")
  -- ���������� ��������� ����, ������������� ��� ��������
  keyfileContents        <-  unlessNull fileGetBinary (findReqArg o "keyfile" "")
  -- ��������� ���� ������ � ���������� ��� -p? � ��� -p, ���� ��� ��������� �����
  let askPwd pwd          =  pwd=="?" || (pwd=="" && keyfileContents=="")
  -- ������ ���������� ������� � ������������� ����������, ��� Nothing �� �������� �������
  receipt                <-  newMVar Nothing

  -- �������������� command � ������������� ����������, ��� �������������
  -- ���������� ������ � ������������ � �������� keyfiles
  let cookPasswords command (ask_encryption_password, ask_decryption_password) = do
        modifyMVar receipt $ \x -> do
          f <- x.$maybe makeReceipt return   -- ������� ������ ���������� ������� � ����������, ���� ��� ��� ���
          return (Just f, f command)         -- ��������� ������ � command � ��������� ��� ��� ����������� ����������
       where
        makeReceipt = do
          -- �������� � ������������ ������, ���� �� ����������� ��� ������
          let ask_password | cmdType cmd==ADD_CMD = ask_encryption_password
                           | otherwise            = ask_decryption_password
          asked_password  <-  any askPwd [dpwd,hpwd]  &&&  ask_password
          -- ������� � ������ ������� ���������� �������� ������������� ������ � ������ ������, ���� ��� ����������� ����� ���� ����������� keyfile
          asked_password      &&&  modifyMVar_ mvar_unpack_passwords (return.(asked_password:))
          oldKeyfileContents  &&&  modifyMVar_ mvar_unpack_passwords (return.("":))
          -- �������� � ������ ���������� keyfile � �������� ����������� "--"/"?"
          let cook "--"             = ""                                -- ���������� ���������
              cook pwd | askPwd pwd = asked_password++keyfileContents   -- ������, ������� � ���������� + ���������� keyfile
                       | otherwise  = pwd++keyfileContents              -- ������ �� ��������� ������ + ���������� keyfile
          return$ \command ->
                   command { opt_data_password    = cook dpwd
                           , opt_headers_password = cook hpwd
                           , opt_decryption_info  = (dont_ask_passwords, mvar_unpack_passwords, oldKeyfileContents, ask_decryption_password)}


-------------------------------------------------------------------------------------
-- ������ �� ������
  -- �������� ���������� ������
  let update_type = case cmd of
        "f"                       -> 'f'  -- ������� f: �������� ����� ����� ������� ��������, ����� ������ �� ���������
        "u"                       -> 'u'  -- ������� u: �������� ����� ����� ������� �������� � �������� ����� �����
        _ | findNoArg o "freshen" -> 'f'  -- �����  -f: ��. ����
          | findNoArg o "update"  -> 'u'  -- �����  -u: ��. ����
          | findNoArg o "sync"    -> 's'  -- ����� --sync: �������� ����� � ������ � ������������ � ������� �� �����
          | otherwise             -> 'a'  -- �����: �������� ����� � ������ �� ������ � ����� � �������� ����� �����

  -- ������� ����� �� ���������, ���� ������������ ����� "-k" ��� ������� "k"
  let lock_archive  =  findNoArg o "lock" || cmd=="k"

  -- ������� ������������ �����, ���� ������������ ����� "-d[f]" ��� ������� "m[f]"
  delete_files  <-  case (findNoArg o "delete"   || cmd=="m"
                         ,findNoArg o "delfiles" || cmd=="mf")
                      of
                         (False, False) -> return NO_DELETE
                         (False, True ) -> return DEL_FILES
                         (True , False) -> return DEL_FILES_AND_DIRS
                         (True , True ) -> registerError$ CMDLINE_GENERAL "options m/-d and mf/-df can't be used together"

  -- �������� ������������� ������������� �����
  when (clear_archive_bit && delete_files/=NO_DELETE) $
      registerError$ CMDLINE_GENERAL "options m[f]/-d[f] and -ac can't be used together"

  -- ������� ��� ��������� ������ - ����� ���� ������ ���� ��� ����� ���������� �����
  workdir <- case orig_workdir of
               "--"       -> return ""    -- �� ��������� (�������� ��������� ����� ����� � �������� ��������)
               '%':envvar -> getEnv envvar
               dir        -> return dir

  -- ���������� ������� ���������� ������ � ������
  let sort_order  =  case (orig_sort_order, group_data) of
        (Just "-", _)  -> ""                    -- ���� ������� ���������� ����� ��� "-", �� ��������� ����������
        (Just  x,  _)  -> x                     -- ���� ������� ���������� ��� ���� ������, �� ������������ ���
        (_, [GroupNone]) -> ""                  -- ���� �� ������������ solid-������ - ��������� ����������
        _  -> if getMainCompressor data_compressor
                 .$anyf [==aNO_COMPRESSION, isFakeCompressor, isVeryFastCompressor]
                then ""                         -- ���� -m0/--nodata/--crconly/tor:1..4/lzp:h13..15 - ����� ��������� ����������
                else aDEFAULT_SOLID_SORT_ORDER  -- ����� - ������������ ����������� ������� ���������� ��� solid-�������

  -- ��������, ��� ����� "-rr" ��������� ���� �� ���������� ��������
  let rr_ok = recovery `elem` ["","-","--"]
              || snd(parseNumber recovery 'b') `elem` ['b','%','p']
              || ';' `elem` recovery
              || '*' `elem` recovery
  unless rr_ok $ do
    registerError$ INVALID_OPTION_VALUE "recovery" "rr" ["MEM", "N", "N%", "MEM;SS", "N%;SS", "N*SS", "-", ""]

  -- ��������� ������� � ������������ � ���������� ������
  ref_overwrite  <-  newIORef$ case (yes,   overwrite) of
                                    (_,     "+")  ->  "a"
                                    (_,     "-")  ->  "s"
                                    (True,  _  )  ->  "a"
                                    (False, "p")  ->  " "

  -- ������ ��������, ������� ���� ��������� ��������������� ����� ������� ���������� �������
  setup_command'  <-  listVal setup_command >>== sequence_


------------------------------------------------------------------------------------------------
-- ������ �� ��� � ���������, �������������� ����������� ������� � ����������� ����� ���������
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
-- |���������, ��� ����� ��������� ���� �� ����������� ��������
testOption fullname shortname option valid_values = do
  unless (option `elem` valid_values) $ do
    registerError$ INVALID_OPTION_VALUE fullname shortname valid_values

{-# NOINLINE addArcExtension #-}
-- |���� ��� ������ �� �������� ���������� � �� ������������ ����� --noarcext,
-- �� �������� � ���� ���������� �� ���������
addArcExtension noarcext filespec =
  case (hasExtension filespec, noarcext) of
    (False, False)  ->  filespec ++ aDEFAULT_ARC_EXTENSION
    _               ->  filespec

{-# NOINLINE replace_list_files #-}
-- |�������� ������ �� ����-����� ("@listfile") �� ����������
replace_list_files parseFile  =  concatMapM $ \filespec ->
  case (startFrom "@" filespec) of
    Just listfile  ->  parseFile 'l' listfile >>== deleteIf null
    _              ->  return [filespec]

-- |������ ���������� ����� "-s"
parseSolidOption opt =
  case (split ';' opt) of
    []        ->  ([aDEFAULT_DIR_GROUPING], [GroupAll], "")   -- "-s" �������� ����� �����-���� ��� ���� ������ � ����� �������� ������
    ["-"]     ->  ([aDEFAULT_DIR_GROUPING], [GroupNone], "")  -- "-s-" ��������� �����-������, ��� ��������� ������������ ����������� �����������
    ["7z"]    ->  ([GroupAll],  [GroupAll], "")               -- "-s=7z"  ������ ����� ������ ������� � ���� �����-���� ��� ���� ������ � ������
    ["cab"]   ->  ([GroupAll],  [GroupAll],  "0")   --  -dm0  -- "-s=cab" ������ ����� �������� ������� � ���� �����-���� ��� ���� ������ � ������
    ["zip"]   ->  ([GroupAll],  [GroupNone], "0")   --  -dm0  -- "-s=zip" ������ ��������� �����-���� ��� ������� ����� � ������, � ����� �������� �������
    ["arj"]   ->  ([GroupNone], [GroupNone], "0")   --  -dm0  -- "-s=arj" ������ ��������� �����-���� � ������� ��� ������� ����� � ������
    [dat]     ->  ([aDEFAULT_DIR_GROUPING], parse dat, "")    -- "-sXXX" ����� ����������� ������ ��� �����-������, �������� ������������ ����������
    [dir,dat] ->  (parse dir, parse dat, "")                  -- "-sXXX;YYY" ����� ����������� � ��� ���������, � ��� �����-������
  where
    -- ��������� �������� ����������� ������:
    --   "-s/-se/-s10m/-s100f" - ������������ ���/�� ����������/�� 10 ��/�� 100 ������, ��������������.
    -- `parse1` ������������ ���� �������� �����������,
    -- � `parse` - �� ������������������, �������� -se100f10m
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
---- �������� ����� � ����� � ������ ��������� ����� -----------------------------------------------
----------------------------------------------------------------------------------------------------

-- |�������� ����� - ������� ���, ������� ���, ���������� ��������
data Option = OPTION String String String

-- |��� ����� - �������� ����� ������� "-", ������� - ������� "--"
data OptType  =  SHORT | LONG

-- |������� ��������� � �����: ���/�����������/�����������
data ParamType  =  ParamNo | ParamReq | ParamOpt

-- |"�������" �����, ���������� �� � ������� ��� ������� ��������� ������ �����
optionsDict  =  concatMap compileOption optionsList
  where compileOption (OPTION short long description)  =  compile short ++ compile ('-':long)
          where -- �������� � ������ �������� ����� � ������ `name`, ���� ��� ��������
                compile name  =  case (name, paramName description) of
                    ("",  _      )  ->  []                                -- ��� ����� - ��� � ����� :)
                    ("-", _      )  ->  []                                -- ��� ����� - ��� � ����� :)
                    (_,   Nothing)  ->  [(name, long|||short, ParamNo )]  -- ����� ��� ���������
                    (_,   Just _ )  ->  [(name, long|||short, ParamReq)]  -- ����� � ����������

-- |�������� ����� ��� ������������
optionsHelp  =  init$ unlines table
  where (ss,ls,ds)     = (unzip3 . map fmtOpt) optionsList
        table          = zipWith3 paste (sameLen ss) (sameLen ls) ds
        paste x y z    = "  " ++ x ++ "  " ++ y ++ "  " ++ z
        sameLen xs     = flushLeft ((maximum . map length) xs) xs
        flushLeft n    = map (left_justify n)
          -- ���������� ������ "�������� �����", "������� �����", � �� ��������
        fmtOpt (OPTION short long description)  =  (format short "" description, format ('-':long) "=" description, description)
          -- ���������� ������ ����� `name` � ������ ������� � �� ����� � ���������
        format name delim description  =  case (name, paramName description) of
                                            ("",   _         )  ->  ""
                                            ("-",  _         )  ->  ""
                                            ("--", _         )  ->  "--"
                                            (_,    Nothing   )  ->  "-"++name
                                            (_,    Just aWORD)  ->  "-"++name++delim++aWORD

-- |���������� ��� ��������� �����, �������� ��� �� ������ � ��������
paramName descr =
  case filter (all isUpper) (words descr)
    of []      -> Nothing      -- �������� �� �������� UPPERCASED ����
       [aWORD] -> Just aWORD   -- �������� �������� UPPERCASED �����, ������������ �������� �����
       _       -> error$ "option description \""++descr++"\" contains more than one uppercased word"

-- |������ ��������� ������, ������������ ������ ����� � ������ "��������� ����������"
parseOptions []          options freeArgs  =  return (reverse options, reverse freeArgs)
parseOptions ("--":args) options freeArgs  =  return (reverse options, reverse freeArgs ++ args)

parseOptions (('-':option):args) options freeArgs = do
  let check (prefix, _, ParamNo)  =  (option==prefix)
      check (prefix, _, _)        =  (startFrom prefix option /= Nothing)
  let accept (prefix, name, haveParam)  =  return (name, tryToSkip "=" (tryToSkip prefix option))
      unknown                           =  registerError$ CMDLINE_UNKNOWN_OPTION ('-':option)
      ambiguous variants                =  registerError$ CMDLINE_AMBIGUOUS_OPTION ('-':option) (map (('-':).fst3) variants)
  newopt <- case (filter check optionsDict) of
              [opt] -> accept opt  -- ������� �����
              []    -> unknown     -- ����������� �����
              xs    -> -- ��� ��������������� � ������� ����� ��������� �� ������ ���������������� �����
                       case (filter ((`elem` aPREFFERED_OPTIONS++aSUPER_PREFFERED_OPTIONS) . snd3) xs) of
                         [opt] -> accept opt        -- ������� �����
                         []    -> ambiguous xs      -- ������������� �����, ������� ��� � ������ ������������
                         xs    -> -- �������� ����! :)
                                  case (filter ((`elem` aSUPER_PREFFERED_OPTIONS) . snd3) xs) of
                                    [opt] -> accept opt        -- ������� �����
                                    []    -> ambiguous xs      -- ������������� �����, ������� ��� � ������ ������������
                                    xs    -> ambiguous xs      -- ������������� ������ ���� � ������ ������������!

  parseOptions args (newopt:options) freeArgs

parseOptions (arg:args) options freeArgs   =  parseOptions args options (arg:freeArgs)


-- |������� ������ �������� ����� � ��������� `flag`. ������ ������: findReqList opts "exclude"
findReqList ((name, param):flags) flag  | name==flag  =  param: findReqList flags flag
findReqList (_:flags) flag                            =  findReqList flags flag
findReqList [] flag                                   =  []

-- |������� �������� ����� � ��������� `flag`, ���� � ��� - �������� �� ��������� `deflt`
findReqArg options flag deflt  =  last (deflt : findReqList options flag)

-- |������� �������� ����� � �������������� ����������
findOptArg = findReqArg

-- |������� �������� ����� � ��������� `flag`, ���� � ��� - Nothing
findMaybeArg options flag  =  case findReqList options flag
                                of [] -> Nothing
                                   xs -> Just (last xs)

-- |������� True, ���� � ������ ����� ���� ����� � ��������� `flag`
findNoArg options flag  =  case findReqList options flag
                                of [] -> False
                                   _  -> True

-- |������� Just True, ���� � ������ ����� ���� ����� � ��������� `flag1`,
--          Just False, ���� � ������ ����� ���� ����� � ��������� `flag2`,
--          Nothing, ���� ��� �� ���, �� ������
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

