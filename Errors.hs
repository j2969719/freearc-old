---------------------------------------------------------------------------------------------------
---- ����������� ������/�������������� � ������ ��������� � ���. ----------------------------------
---------------------------------------------------------------------------------------------------
module Errors where

import Prelude hiding (catch)
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Char
import Data.IORef
import System.Exit
import System.IO
import System.IO.Unsafe
#if defined(FREEARC_WIN)
import GHC.ConsoleHandler
#else
import System.Posix.Signals
#endif

import CompressionLib hiding (b,kb,mb,gb)
import Utils
import Files

-- |���� �������� ���������
aEXIT_CODE_SUCCESS     = 0
aEXIT_CODE_WARNINGS    = 1
aEXIT_CODE_FATAL_ERROR = 2
aEXIT_CODE_USER_BREAK  = 255

-- |��� ��������� ���� ������ � ��������������
data ErrorTypes = GENERAL_ERROR               String
                | CMDLINE_NO_COMMAND          [String]
                | CMDLINE_NO_ARCSPEC          [String]
                | CMDLINE_NO_FILENAMES        [String]
                | UNKNOWN_CMD                 String [String]
                | CMDLINE_UNKNOWN_OPTION      String
                | CMDLINE_AMBIGUOUS_OPTION    String [String]
                | CMDLINE_BAD_OPTION_FORMAT   String
                | INVALID_OPTION_VALUE        String String [String]
                | CMDLINE_GENERAL             String
                | CANT_READ_DIRECTORY         String
                | CANT_GET_FILEINFO           String
                | CANT_OPEN_FILE              String
                | BAD_CRC                     String
                | BAD_CFG_SECTION             String [String]
                | TERMINATED
                | NOFILES
                | SKIPPED_FAKE_FILES          Int
                | BROKEN_ARCHIVE              FilePath String
                | INTERNAL_ERROR              String

--foreign import "&errCounter" :: Ptr Int
{-
data SqliteException = SqliteException Int String
  deriving (Typeable)

catchSqlite :: IO a -> (SqliteException -> IO a) -> IO a
catchSqlite = catchDyn

throwSqlite :: SqliteException -> a
throwSqlite = throwDyn
-}

---------------------------------------------------------------------------------------------------
---- ��������� Ctrl-Break, Close � �.�. ������� ������� -------------------------------------------
---------------------------------------------------------------------------------------------------

setCtrlBreakHandler action = do
  --myThread <- myThreadId
  -- ��� ������ ��� ������������� ���������� ����������� ���������� ���������� �������
#if defined(FREEARC_WIN)
  bracket (installHandler$ Catch onBreak) (installHandler) $  \oldHandler -> do
    action
#else
  let catchSignals a  =  installHandler sigINT (CatchOnce$ onBreak undefined) Nothing
  bracket (catchSignals (CatchOnce$ onBreak (error "onBreak"))) (catchSignals) $  \oldHandler -> do
    action
#endif

-- |��������� Ctrl-Break �������� � ���������� ������������� �
-- ��������� ����. �����, ������� ����������� ����������, ����������� �� ��
onBreak event  =  shutdown "^Break!" aEXIT_CODE_USER_BREAK

shutdown msg exitCode = do
    separator' =: ("","\n")
    w <- val warnings
    case w of
      0 -> when (exitCode==aEXIT_CODE_SUCCESS) $ condPrintLineLn "k"$ "All OK"
      _ -> condPrintLineLn "n"$ "There were "++show w++" warning(s)"
    ignoreErrors (msg &&& condPrintLineLn "n" msg)
    programTerminated =: True
    fin <- val finalizers
    for fin (ignoreErrors.snd)
    compressionLib_cleanup
    condPrintLineLn "e" ""
#if !defined(FREEARC_WIN)
    putStrLn ""  -- � Unix ����������� �������������� ������� ������ � ��������� �� ���������� ���������
#endif
    ignoreErrors$ closeLogFile
    ignoreErrors$ hFlush stdout
    ignoreErrors$ hFlush stderr
    --killThread myThread
    exit (exitCode  |||  (w &&& aEXIT_CODE_WARNINGS))
    --exitWith$ ExitFailure (exitCode  |||  (w &&& aEXIT_CODE_WARNINGS))
    return undefined

-- |"bracket" � ����������� "close" ����� ��� ^Break
bracketCtrlBreak init close action = do
    id <- newId
    bracket (do x<-init; addFinalizer id (close x); return x)
            (\x -> do removeFinalizer id; close x)
            action

-- |��������� close-�������� �� ���������� action
ensureCtrlBreak close action  =  bracketCtrlBreak (return ()) (\_->close) (\_->action)

-- |"handle" � ����������� "onException" ����� ��� ^Break
handleCtrlBreak onException action = do
    id <- newId
    handle (\e -> do onException; throwIO e) $ do
        bracket (addFinalizer id onException) (\_ -> removeFinalizer id) $ \_ -> do
            action

-- ��������/������� finalizer � ������
addFinalizer id action  =  finalizers .= ((id,action):)
removeFinalizer id      =  finalizers .= filter ((/=id).fst)
newId                   =  do curId+=1; id<-val curId; return id

-- |���������� �����
curId :: IORef Int
curId = unsafePerformIO (ref 0)
{-# NOINLINE curId #-}

-- |������ ��������, ������� ���� ��������� ����� ������� �� ^Break
finalizers :: IORef [(Int, IO ())]
finalizers = unsafePerformIO (ref [])
{-# NOINLINE finalizers #-}

-- |���� ���� ��������������� ����� ����, ��� ������������ ����� Ctrl-Break
programTerminated = unsafePerformIO (ref False)
{-# NOINLINE programTerminated #-}


---------------------------------------------------------------------------------------------------
---- ������ ��������� � ��������� ����� ������. ���������� ������ ��� �������������� --------------
---------------------------------------------------------------------------------------------------

errormsg (GENERAL_ERROR str) =
  str

errormsg (UNKNOWN_CMD cmd known_cmds) =
  "command "++quote cmd++" is unknown. Supported commands are:\n" ++ joinWith ", " known_cmds

errormsg (CMDLINE_UNKNOWN_OPTION option) =
  "unknown option " ++ quote option

errormsg (CMDLINE_GENERAL option) =
  option

errormsg (CMDLINE_AMBIGUOUS_OPTION option variants) =
  "ambiguous option " ++ quote option ++ ": is that "++enumerate "or" variants++"?"

errormsg (CMDLINE_BAD_OPTION_FORMAT option) =
  "option " ++ quote option ++ " have illegal format"

errormsg (INVALID_OPTION_VALUE fullname shortname valid_values) =
  fullname++" option must be one of: "++ enumerate "or" (map (('-':shortname)++) valid_values)

errormsg (CMDLINE_NO_COMMAND args) =
  "no command name in command: " ++ quote (unwords args)

errormsg (CMDLINE_NO_ARCSPEC args) =
  "no archive name in command: " ++ quote (unwords args)

errormsg (CMDLINE_NO_FILENAMES args) =
  "no filenames in command: " ++ quote (unwords args)

errormsg (CANT_READ_DIRECTORY dir) =
  "error while reading directory " ++ quote dir

errormsg (CANT_GET_FILEINFO filename) =
  "can't get info about file " ++ quote filename

errormsg (CANT_OPEN_FILE filename) =
  "can't open file " ++ quote filename

errormsg (BAD_CRC filename) =
  "CRC error in " ++ filename

errormsg (BAD_CFG_SECTION cfgfile section) =
  "Bad section " ++ head section ++ " in "++cfgfile

errormsg (TERMINATED) =
  "Program terminated!"

errormsg (NOFILES) =
  "No files, erasing empty archive"

errormsg (SKIPPED_FAKE_FILES n) =
  "skipped "++show n++" fake files"

errormsg (BROKEN_ARCHIVE arcname msg) =
  "Archive "++arcname++" corrupt: "++msg++". Please recover it using 'r' command or use -tp- switch to ignore RR"

errormsg (INTERNAL_ERROR msg) =
  "FreeArc internal error: "++msg

-- |����������� ������ ��������
enumerate s list  =  joinWith2 ", " (" "++s++" ") (map quote list)

{-# NOINLINE errormsg #-}


----------------------------------------------------------------------------------------------------
---- ������ � ��������� � ���������� ������� ������ �� ����� � ������������ � ������ --display -----
----------------------------------------------------------------------------------------------------

-- ���������� �������� ������, ������� � ��� ������������� �� ���������� �������/������������� ������
-- ����� ����, ������ ����� ���������� ������ ����������� � ������ �������,
-- ���� ��� ���������� ��������������� ����� ��������� ���������
printLine = printLineC ""
printLineC c str = do
  (oldc,separator) <- val separator'
  let makeLower (x:y:zs) | isLower y  =  toLower x:y:zs
      makeLower xs                    =  xs
  let handle "w" = stderr
      handle _   = stdout
  hPutStr (handle oldc)$ separator
  hPutStr (handle c)   $ (oldc=="h" &&& makeLower) str
  hFlush  (handle c)
  separator' =: (c,"")

-- |���������� ������ � ������������ ����� ����� ��
printLineLn str = do
  printLine str
  printLineNeedSeparator "\n"

-- �������� ����������� ����� �������� �������. �� ������� ��� ������ �����,
-- ��������� �������� ������������ ������ ����� � �� ���� :)))
printLineNeedSeparator str = do
  separator' =: ("",str)

-- �������� ������ � �������.
-- ������� � �� ����� ��� �������, ��� � ����� �� �������� ������ --display
condPrintLine c line = do
  display_option <- val display_option'
  when (c/="$" || (display_option `contains` '#')) $ do
      printLog line
  when (display_option `contains_one_of` c) $ do
      printLineC c line

-- |���������� ������ � ������������ ����� ����� ��
condPrintLineLn c line = do
  condPrintLine c line
  condPrintLineNeedSeparator c "\n"

-- �������� ����������� ����� �������� ������� ��� ������� ���������� ������ ������ c
condPrintLineNeedSeparator c str = do
  display_option <- val display_option'
  when (c/="$" || (display_option `contains` '#')) $ do
      log_separator' =: str
  when (c=="" || (display_option `contains_one_of` c)) $ do
      separator' =: (c,str)

-- ������� �������
openLogFile logfilename = do
  closeLogFile  -- ������� ����������, ���� ���
  logfile <- case logfilename of
                 ""  -> return Nothing
                 log -> fileAppendText log >>== Just
  logfile' =: logfile

-- ������� ������ � �������
printLog line = do
  separator <- val log_separator'
  whenJustM_ (val logfile') $ \log -> do
      fileWrite log (separator ++ line); fileFlush log
      log_separator' =: ""

-- ������� �������
closeLogFile = do
  whenJustM_ (val logfile') fileClose
  logfile' =: Nothing

-- ����������, �������� Handle ��������
logfile'        = unsafePerformIO$ newIORef Nothing
-- ����������, ������������ ��� ��������� ������
separator'      = unsafePerformIO$ newIORef ("","") :: IORef (String,String)
log_separator'  = unsafePerformIO$ newIORef "\n"    :: IORef String
display_option' = unsafePerformIO$ newIORef$ error "undefined display_option"

{-# NOINLINE printLine #-}
{-# NOINLINE printLineNeedSeparator #-}
{-# NOINLINE condPrintLine #-}
{-# NOINLINE condPrintLineNeedSeparator #-}
{-# NOINLINE separator' #-}
{-# NOINLINE log_separator' #-}
{-# NOINLINE display_option' #-}

----------------------------------------------------------------------------------------------------
---- ������ ��������� �� ������� � ��������������
----------------------------------------------------------------------------------------------------

-- |������ ��������� �� ������ � ������� � ��������� ���������� ��������� � ���� ����������
registerError err = do
  let msg = str2terminal$ "ERROR: "++errormsg err
  shutdown msg aEXIT_CODE_FATAL_ERROR

-- |������ �������������� � ������� � ����� ��� �� �����
registerWarning warn = do
  warnings += 1
  let msg = str2terminal$ "WARNING: "++errormsg warn
  condPrintLineLn "w" msg

-- |��������� ��������� � ���������� ���������� ��������� ��� ���� warning'��
count_warnings action = do
  w0 <- val warnings
  action
  w  <- val warnings
  return (w-w0)

-- |������� ������, ��������� � ���� ������ ���������
warnings = unsafePerformIO$ newIORef 0 :: IORef Int

{-# NOINLINE registerError #-}
{-# NOINLINE registerWarning #-}
{-# NOINLINE warnings #-}

----------------------------------------------------------------------------------------------------
---- ������ � �������
----------------------------------------------------------------------------------------------------

-- |���������� Nothing � ���������� ��������� �� ������, ���� ���� �� ������� �������
tryOpen filename = catchJust ioErrors
                     (fileOpen filename >>== Just)
                     (\e -> do registerWarning$ CANT_OPEN_FILE filename; return Nothing)

-- |����������� ����
fileCopy srcname dstname = do
  bracketCtrlBreak (fileOpen srcname) (fileClose) $ \srcfile -> do
    handleCtrlBreak (ignoreErrors$ fileRemove dstname) $ do
      bracketCtrlBreak (fileCreate dstname) (fileClose) $ \dstfile -> do
        size <- fileGetSize srcfile
        fileCopyBytes srcfile size dstfile


----------------------------------------------------------------------------------------------------
----- External functions ---------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Stop program execution
foreign import ccall unsafe "stdlib.h exit"
  exit :: Int -> IO ()

