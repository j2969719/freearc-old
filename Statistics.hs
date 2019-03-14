----------------------------------------------------------------------------------------------------
---- ���� � ����������� ���������� ������ ��������� (����� ������������ ������, �������� � �.�.) ---
----------------------------------------------------------------------------------------------------
module Statistics where

import Prelude hiding (catch)
import Control.Monad
import Control.Concurrent
import Data.IORef
import Numeric           (showFFloat)
import System.CPUTime    (getCPUTime)
import System.Directory  (getCurrentDirectory)
import System.IO
import System.IO.Unsafe
import System.Time

import Utils
import Errors
import Files
import FileInfo
import Compression (showMem, compressorGetCompressionMem, compressorGetDecompressionMem)
import Cmdline
import CUI

-- |����� �������� ��� ���������� � ������� � �������� � ����������, ��������� ��� �����������
-- ���������� ��������� � ������ ��������� ����������
data UI_State = UI_State {
    command         :: !Command     -- ����������� ������ �������
  , total_files     :: !FileCount   -- ���-�� ������, ������� ��� ������ ����������
  , total_bytes     :: !FileSize    -- ����� ����� ���� ������ (� ������������� ����)
  , datatype        ::  DataType    -- �������������� � ������ ������ ����� ������: ����/�������/��������� ������
  -- � ����������� �� ����, ����� ����� ������ ������ ��������������, ���������� ���������
  -- ���� �� ���� ������:
  ,    files        :: !FileCount   -- ���-�� ��� ������������ ������
  ,    bytes        :: !FileSize    -- ����� ��� ������������ ������ � ������������� ����
  ,    cbytes       :: !FileSize    -- ����� ��� ������������ ������ � ����������� ����
  -- ���� �� ���� ��������� (��������� ���������� �� ��������������):
  ,    dirs         :: !FileCount   -- ���-�� ��������� ��������� � ������ ��������� ������
  ,    dir_bytes    :: !FileSize    -- ����� ��� ������������ ������ � ������������� ����
  ,    dir_cbytes   :: !FileSize    -- ����� ��� ������������ ������ � ����������� ����
  -- ����� ����, �� ����������, ����� ����� �� ���� ������ - �� ����� ���� �� ������������� (��� ������� ��� ����������� �������� �������� ��������):
  ,    fake_files   :: !FileCount   -- ���-�� ��� ������������ ������
  ,    fake_bytes   :: !FileSize    -- ����� ��� ������������ ������ � ������������� ����
  ,    fake_cbytes  :: !FileSize    -- ����� ��� ������������ ������ � ����������� ����
  -- ���������� � ������� �����-�����
  ,    algorithmsCount :: Int       -- ���-�� ���������� � �������
  ,    rw_ops       :: [[UI_RW FileSize]] -- ������������������ �������� ������/������ � ��������� �� ��������� ����������
  ,    r_bytes      :: FileSize     -- ����� ��� ������������ ������ �� ����� ������� ��������� ������
  ,    rnum_bytes   :: FileSize     -- ����� ��� ������������ ������ �� ����� ���������� ��������� ������
  }

-- |�������������� � ������ ������ ����� ������: ����/�������/��������� ������
data DataType = File | Dir | CData   deriving (Eq)

ref_command               =  unsafePerformIO$ newIORef$ error "undefined CUI::ref_command"
refStartArchiveTime       =  unsafePerformIO$ newIORef$ error "undefined CUI::refStartArchiveTime"
refArchiveProcessingTime  =  unsafePerformIO$ newIORef$ error "undefined CUI::refArchiveProcessingTime"  :: IORef Double
ref_ui_state              =  unsafePerformIO$ newIORef$ error "undefined CUI::ref_ui_state"
putHeader                 =  unsafePerformIO$ init_once


-- |�������� ������ ���������� ���������
uiStartProgram = do
  runIndicators

-- |�������� ������ ���������� �������
uiStartCommand command = do
  ref_command =: command
  display_option' =: opt_display command
  refStartArchiveTime =:: getClockTime
  -- ������� ������� � ������� � ���� ����������� �������
  openLogFile (opt_logfile command)
  curdir <- getCurrentDirectory
  printLog (curdir++">"++cmd_command_text command++"\n")
  -- �������� ������ ���������� � ������������ �������������� �����
  let addArgs = cmd_additional_args command
  once putHeader$ condPrintLine "h" aARC_HEADER
  condPrintLine "o" (addArgs &&& "Using additional options: "++addArgs++"\n")
  hFlush stdout

-- |�������� ������ ���������� ����������
uiStartSubCommand command subCommand = do
  ref_command =: subCommand
  display_option' =: opt_display subCommand

-- |�������� ������ ��������� ���������� ������
uiStartArchive command @ Command {
                 opt_data_compressor = compressor
               , opt_cache           = cache
               }
               method
               limited_compressor = do
  -- ��������� ����� ������ ��������� ������ � ����������� �������
  refStartArchiveTime =:: getClockTime
  ref_command =: command
  display_option' =: opt_display command

  -- ������� ��������� �� ����� ���������, ���� ��� ���-������� (��������, ������������ ����� ���������)
  if cmd_subcommand command
    then do condPrintLineNeedSeparator "" "\n"
    else do

  -- ������� ��������� ���� "Testing archive ..."
  let cmd = cmd_name command
      arcname = cmd_arcname command
  exist <- fileExist arcname
  condPrintLine "a"  $ (msgStart cmd exist) ++ (str2terminal arcname)
  condPrintLine "c"  $ (method &&& " using "++method)
  condPrintLine "ac" $ "\n"
  when (cmdType cmd == ADD_CMD) $ do
      condPrintLineLn "m" $
          "Memory for compression "++showMem (compressorGetCompressionMem   limited_compressor)
          ++", decompression "     ++showMem (compressorGetDecompressionMem limited_compressor)
          ++", cache "             ++showMem cache

-- |�������� ������ �������� ��� ���������� ������
uiStartProcessing filelist = do
  refArchiveProcessingTime =: 0
  command <- val ref_command
  let cmd = cmd_name command
      total_files' = i$ length filelist
      total_bytes' = sum (map fiSize filelist)
      ui_state = UI_State {
          command         = command
        , total_files     = total_files'
        , total_bytes     = total_bytes'
        , datatype        = error "internal CUI error: datatype not initialized"
        , files           = 0
        , bytes           = 0
        , cbytes          = 0
        , dirs            = 0
        , dir_bytes       = 0
        , dir_cbytes      = 0
        , fake_files      = 0
        , fake_bytes      = 0
        , fake_cbytes     = 0
        , algorithmsCount = error "internal CUI error: algorithmsCount not initialized"
        , rw_ops          = error "internal CUI error: rw_ops not initialized"
        , r_bytes         = error "internal CUI error: r_bytes not initialized"
        , rnum_bytes      = error "internal CUI error: rnum_bytes not initialized"
        }
  ref_ui_state =: ui_state
  printLine$ msgDo cmd ++ show_files3 total_files' ++ ", "
                       ++ show_bytes3 total_bytes'
  -- ����� ����� "�����������" �������� �������� �� ������ ������� � ������� �����������
  printLineNeedSeparator $ "\r"++replicate 75 ' '++"\r"
  when (opt_indicator command == "1") $ do
    putStr$ ". Processed "
  -- ������� progress indicator ��������� ����� ������� ������������ ������, ��� ��������� ��� �������� �������
  -- ������ � ������� ����������� �� �������: �������� �������� 1��/�, ����� �������� ����� - 10 ����
  let current bytes = do ui_state <- val ref_ui_state
                         return$ bytes + (bytes_per_sec `div` 100)*i (files ui_state)
      total = do ui_state <- val ref_ui_state
                 return$ total_bytes ui_state + (bytes_per_sec `div` 100)*i (total_files ui_state)
  uiStartProgressIndicator command current total
  hFlush stdout


-- |�������� ������ ��������/���������� ������
uiStartFiles count = do
  syncUI $ do
  modifyIORef ref_ui_state $ \ui_state ->
    ui_state { datatype        = File
             , algorithmsCount = count
             , rw_ops          = replicate count []
             , r_bytes         = 0
             , rnum_bytes      = 0
             }

-- |�������� ������ ��������/���������� �������� ������
uiStartDirectory = do
  syncUI $ do
  modifyIORef ref_ui_state $ \ui_state ->
    ui_state { datatype        = Dir
             , dirs            = dirs ui_state + 1
             , algorithmsCount = 0 }

-- |�������� ������ ��������/���������� ��������� ���������� ������
uiStartControlData = do
  syncUI $ do
  modifyIORef ref_ui_state $ \ui_state ->
    ui_state { datatype        = CData
             , algorithmsCount = 0 }

-- |�������� ������ ��������/���������� �����
uiStartFile fileinfo = do
  syncUI $ do
  modifyIORefIO ref_ui_state $ \ui_state -> do
    let cmd           =  command ui_state
        filename      =  str2terminal$ fpFullname$ fiStoredName$ fileinfo
        printed_name  =  filename ++ (fiIsDir fileinfo &&& "/")
    when (opt_indicator cmd == "2") $ do
      uiSuspendProgressIndicator
      putStrLn   ""
      putStr$    left_justify 72 (msgFile(cmd_name cmd) ++ printed_name)
      uiResumeProgressIndicator
      hFlush     stdout
    return ui_state {
        datatype = File
      , files    = files ui_state + 1
      }

-- |���������������� total_bytes � ui_state
uiCorrectTotal files bytes = do
  syncUI $ do
  when (files/=0 || bytes/=0) $ do
    modifyIORef ref_ui_state $ \ui_state ->
      ui_state { total_files = total_files ui_state + files
               , total_bytes = total_bytes ui_state + bytes }

-- |�������� �������� ��������� ������ �������� ������������ ������
uiFakeFiles filelist compsize = do
  let origsize  =  sum (map fiSize filelist)
  syncUI $ do
    modifyIORef ref_ui_state $ \ui_state ->
      ui_state { datatype    = File
               , files       = (files       ui_state) + (i$ length filelist)
               , fake_files  = (fake_files  ui_state) + (i$ length filelist)
               , fake_bytes  = (fake_bytes  ui_state) + origsize
               , fake_cbytes = (fake_cbytes ui_state) + compsize
               }
  uiUnpackedBytes           origsize
  uiCompressedBytes         compsize
  uiUpdateProgressIndicator origsize

-- |��������, ��� ���� ���������� �������-�� ���� ������ ������ (�������, ���������
-- �� ��� ��������, ������� ������ ��� ���������� ��� ������ ����������� ������, ����������
-- �� ������� ������ � ����� ��� �����-���� �����������)
uiCompressedBytes len = do
  syncUI $ do
  modifyIORef ref_ui_state $ \ui_state ->
    case (datatype ui_state) of
      File  ->  ui_state {     cbytes =     cbytes ui_state + len }
      Dir   ->  ui_state { dir_cbytes = dir_cbytes ui_state + len }
      CData ->  ui_state

-- |��������, ��� ���� ���������� �������-�� ���� ������������� ������ (���� ���� �������
-- ��� ����� ����� � ����� �� �����, ��������� ��� ���� �������� � ������ ���� �� ������ � �����)
uiUnpackedBytes len = do
  syncUI $ do
  modifyIORef ref_ui_state $ \ui_state ->
    case (datatype ui_state) of
      File  ->  ui_state {     bytes =     bytes ui_state + len }
      Dir   ->  ui_state { dir_bytes = dir_bytes ui_state + len }
      CData ->  ui_state


----------------------------------------------------------------------------------------------------
---- ������� �������� r/w, �� ������� ��� �������� ����������� ��������� ��������� -----------------
----------------------------------------------------------------------------------------------------

-- �������� ������ � ������ � ������ ��������
data UI_RW a = UI_Read a | UI_Write a deriving Show

-- �������� �������� ������/������ � ������ ������, ������ �������� ������ ����
add_Read  a (UI_Write 0:UI_Read  0:ops) = (UI_Read a:ops)  -- ���������� �� useless ���� r0+w0
add_Read  a (UI_Read  b:ops) = (UI_Read (a+b):ops)
add_Read  a             ops  = (UI_Read  a   :ops)

add_Write a (UI_Write b:ops) = (UI_Write(a+b):ops)
add_Write a             ops  = (UI_Write a   :ops)

-- |�������� ����� num � ������� ������� �������� bytes ����, ��������������� ���������� ����� �����������
-- ������ (��� �������� "�������� ������" ��������� ������������ ���������� ��������� ���������)
uiQuasiWriteData num bytes = do
  -- ���������� �������� ���, ��� ��������� ����������� ������ �������������� bytes ���������� ������,
  -- �� ��� ���� ����� ������ ���������� ������ �� ���������� �� �� ���� ;)
  uiWriteData num bytes
  uiReadData  num 0
  uiWriteData num (-bytes)

-- |�������� ����� num � ������� ������� bytes ����
uiWriteData num bytes = do
  UI_State {algorithmsCount=count, datatype=datatype} <- val ref_ui_state
  when (datatype == File) $ do
  -- ��������� � ������ �������� �/� �������� ������
  when (num>=1 && num<count) $ do
    syncUI $ do
    modifyIORef ref_ui_state $ \ui_state @ UI_State {rw_ops=rw_ops} ->
      ui_state {rw_ops = updateAt num (add_Write bytes) rw_ops}

-- |�������� ����� num � ������� �������� bytes ����
uiReadData num bytes = do
  UI_State {algorithmsCount=count, datatype=datatype} <- val ref_ui_state
  when (datatype == File) $ do
  -- ��������� � ������ �������� �/� �������� ������
  when (num>=1 && num<count) $ do
    syncUI $ do
    modifyIORef ref_ui_state $ \ui_state @ UI_State {rw_ops=rw_ops} ->
      ui_state {rw_ops = updateAt num (add_Read bytes) rw_ops}
  -- �������� ��������� ���������, ���� ��� ��������� �������� ������ � �������
  when (num>=1 && num==count) $ do
    unpBytes <- syncUI $ do
      -- ��������� �� ��������� ���� ����
      ui_state @ UI_State {r_bytes=r_bytes0, rnum_bytes=rnum_bytes0, rw_ops=rw_ops0}  <-  val ref_ui_state
      -- � ������ �� ����� ��������� num ����������� bytes ����,
      -- ����������� ���������� ���� �� ����� ������� ��������� ���� ���� ���� �� ����� �� ������ ��������� (bytes>16)
      let rnum_bytes = rnum_bytes0+bytes
          (r_bytes, rw_ops) = if bytes>16
                                 then calc num (reverse rw_ops0) [] rnum_bytes
                                 else (r_bytes0, rw_ops0)
      ref_ui_state =: ui_state {r_bytes=r_bytes, rnum_bytes=rnum_bytes, rw_ops=rw_ops}
      --for rw_ops $ \x -> print (reverse x)
      --print (rnum_bytes0, bytes, r_bytes0, r_bytes-r_bytes0)
      -- ���������� ���������� ���� �� ����� ������� ��������� ������������ ����������� �������� ���� ��������
      return (r_bytes-r_bytes0)
    uiUpdateProgressIndicator ((unpBytes*9) `div` 10)
  when (num==1) $ do  -- 90% �� ��������� �������� � ������� � 10% �� ������ (����� �������� ����� ��� external compression and so on)
    uiUpdateProgressIndicator (bytes `div` 10)

 where
  -- ���������� ����������� bytes ���� �� ����� ��������� num � ���������� ���� �� ����� ��������� 1
  -- ������ �� �������� ������� ��������, ������������� �������� �������������� ������� ����� ��������
  calc 1   _                new_ops bytes = (bytes, []:new_ops)
  calc num (old_op:old_ops) new_ops bytes =
    -- ����������� bytes ���� �� ������ ��������� num-1 � ����� �� ��� �����
    let (new_bytes, new_op) = go 0 bytes (0,0) (reverse old_op)
    in calc (num-1) old_ops (reverse new_op:new_ops) new_bytes

  -- ������������� ���������� ����� (restW) � ����������� (totalR) �������� ������������������ �������� �����/������
  go totalR restW (rsum,wsum) ops@(UI_Read r:UI_Write w:rest_ops)
       -- ���� ��������� ����� ����������� ������ ������ ������ �������, �� ����������� totalR �� ���� � �������� ������
       | w<restW   = go (totalR+r) (restW-w) (rsum+r,wsum+w) rest_ops
       -- ����� ����� ��� ��������������� (r/w * restW) � ��������� � totalR
       | otherwise = (totalR + ((r*restW) `div` max w 1), UI_Read rsum:UI_Write wsum:ops)
  -- ��� ������ ��������
  go totalR _ (rsum,wsum) ops  =  (totalR, UI_Read rsum:UI_Write wsum:ops)


----------------------------------------------------------------------------------------------------
---- ���������� ���������� ��������� ---------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |���������� ����������, �������� ��������� ���������� ���������
aProgressIndicatorState  =  unsafePerformIO$ newIORef$ error "undefined CUI::aProgressIndicatorState"

-- |���������������� ��������� ���������
uiStartProgressIndicator command bytes' total' = do
  bytes <- bytes' 0;  total <- total'
  let cmd        =  cmd_name command
      direction  =  if (cmdType cmd == ADD_CMD)  then " => "  else " <= "
      arcname    =  str2terminal$ cmd_arcname command
      indicator  =  select_indicator command total
  aProgressIndicatorState =: (indicator, arcname, direction, 0, bytes', total')
  putStr$ percents indicator bytes total

-- |������� �� ����� � � ��������� ���� ��������� ��������� (������� ��������� ������ ��� ����������)
uiUpdateProgressIndicator add_b = when (add_b/=0) $ do
  -- ��������� ��������� ��������: ��� ������� ���������� ����� �����-���� ����������
  -- ������. ��� ���� �� �������, ��� ���������� ������ � ������� ������� ��� ���������� �
  -- ��������� �� ����. ����� �� ������ ������ ����������� � ��������, �� �� ������
  -- �� ��������� ������ ����������. ��� ����� ��� ������� � ����� ������� :)
  syncUI $ do
  aProgramTerminated <- val programTerminated
  (indicator, arcname, direction, b, bytes', total') <- val aProgressIndicatorState
  when (indicator /= NoIndicator  &&  not aProgramTerminated) $ do
    bytes <- bytes' b;  total <- total'
    let p = percents indicator bytes total
    indicator_state =: Just (back_percents indicator ++ p)
    title_state     =: Just (trimLeft p ++ direction ++ arcname)
    aProgressIndicatorState =: (indicator, arcname, direction, b+add_b, bytes', total')

-- |������������� ����� ���������� ��������� � ������� ��� �����
uiSuspendProgressIndicator = do
  indicator_state =: Nothing
  title_state     =: Nothing
  (indicator, arcname, direction, b, bytes', total') <- val aProgressIndicatorState
  putStr$ clear_percents indicator
  hFlush stdout

-- |����������� ����� ���������� ��������� � ������� ��� ������� ��������
uiResumeProgressIndicator = do
  (indicator, arcname, direction, b, bytes', total') <- val aProgressIndicatorState
  bytes <- bytes' b;  total <- total'
  putStr$ percents indicator bytes total
  hFlush stdout

-- |��������� ����� ���������� ���������
uiDoneProgressIndicator = do
  uiSuspendProgressIndicator
  aProgressIndicatorState =: (NoIndicator, undefined, undefined, undefined, undefined, undefined)

-- |��������� ����� ���������� ���������
uiWithProgressIndicator command arcsize action = do
  uiStartProgressIndicator command return (return arcsize)
  ensureCtrlBreak uiDoneProgressIndicator action


----------------------------------------------------------------------------------------------------
---- ����������� ���������� ��������� --------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- ���������� ��������� � ������� � � ���������
indicator_state = unsafePerformIO$ newIORef Nothing
title_state     = unsafePerformIO$ newIORef Nothing

-- |��������� ��� ����� ��� ������ ����������� ���������
runIndicators = do
  -- ��������� ��������� ��������� ��� � 0.5 �������
  indicatorThread 0.5 indicator_state (putStr .>> hFlush stdout)
  -- ��������� ��������� ���� ��� � 0.5 �������
  indicatorThread 0.5 title_state     setConsoleTitle

-- |����, �������� �� indicator, � ��������� ����� �� ������� ��� ���������� ��������
indicatorThread secs indicator output = forkIO $ do
  last_i' <- ref Nothing
  forever $ do
    threadDelay (round$ secs*1000000)
    syncUI $ do
      i <- val indicator
      last_i <- val last_i'
      last_i' =: i
      when (i/=last_i) $ do
        case i of
          Just i  -> output i
          Nothing -> return undefined


----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |�������� ������ �������� ��� ���������� �����-�����
uiStartDeCompression deCompression = do
  x <- getCPUTime
  newMVar (x,deCompression,[])

-- |�������� � ������ ����� ������ ������ �� ���������� � �������
-- (����������� � ������ ����� ����� ������ ����������/������������)
uiDeCompressionTime times t =  do
  modifyMVar_ times (\(x,y,ts) -> return (x, y, ts++[t]))

-- |��������/���������� �����-����� ��������� - �������������� ����� ������ ���� ������
-- ��� ������������ wall clock time, ���� ���� �� ���� �� ������������ ����� == -1
uiFinishDeCompression times = do
  (timeStarted, deCompression, results) <- takeMVar times
  timeFinished <- getCPUTime
  let deCompressionTimes  =  map snd3 results
  refArchiveProcessingTime +=  if (all (>=0) deCompressionTimes)
                                 then sum deCompressionTimes
                                 else i(timeFinished - timeStarted) / 1e12
  let total_times = if (all (>=0) deCompressionTimes)
                                 then " ("++showFFloat (Just 3) (sum deCompressionTimes) ""++" seconds)"
                                 else ""
  when (results>[]) $ do
    debugLog$ "  Solid block "++deCompression++" results"++total_times
    for results $ \(method,time,size) -> do
        debugLog$ "    "++method++": "++show3 size++" bytes in "++showFFloat (Just 3) time ""++" seconds"

-- |��������� ���������� ������ ��������� -> ���������� ���������� � ������� � ���������� ���������
uiDoneArchive = do
  ui_state @ UI_State { command       = command
                      , total_files   = total_files
                      , total_bytes   = total_bytes
                      , files         = files
                      , bytes         = bytes
                      , cbytes        = cbytes
                      , dirs          = dirs
                      , dir_bytes     = dir_bytes
                      , dir_cbytes    = dir_cbytes
                      , fake_files    = fake_files
                      , fake_bytes    = fake_bytes
                      , fake_cbytes   = fake_cbytes }  <-  readIORef ref_ui_state
  let cmd = cmd_name command
  uiDoneProgressIndicator
  when (opt_indicator command=="2" && files-fake_files>0) $ do
    putStrLn ""
    printLineNeedSeparator ""  -- ����� � ����������� ����� ������� ��������� ����� �������

  -- ���������� ������ (�� ��������� ��� ���-������, ��������� ����� ����� �� ���������� ��� ���������� �������� �������)
  unless (cmd_subcommand command) $ do
    condPrintLineLn "f" $ left_justify 75 $    -- ��� �������������� �������� ����� �� ������������ ��������� ���������� ������
      msgDone cmd ++ show_files3 files ++ ", " ++ show_ratio cmd bytes cbytes
    -- ���������� ���������� �� �������� ������ ������ ���� �� ���������� �����
    when (dir_bytes>10^4) $ do
      condPrintLine   "d" $ "Directory " ++ (dirs>1 &&& "has " ++ show3 dirs ++ " chunks, ")
      condPrintLineLn "d" $                 show_ratio cmd dir_bytes dir_cbytes

  -- ���������� � ������� ������ � �������� ��������/����������
  secs <- val refArchiveProcessingTime   -- �����, ����������� ��������������� �� ��������/����������
  real_secs <- return_real_secs          -- ������ ����� ���������� ������� ��� ������� �������
  condPrintLine                     "t" $ msgStat cmd ++ "time " ++ showTime  secs
  when (secs>=0.01) $ condPrintLine "t" $ ", speed " ++ showSpeed (bytes-fake_bytes) secs
  condPrintLine                     "t" $ ". Total " ++ showTime  real_secs

  condPrintLineNeedSeparator "rdt" "\n"
  hFlush stdout
  resetConsoleTitle
  return (1,files,bytes,cbytes)

-- |���������� ����� ���� ��������������� �������� (���������� recovery info, ������������)
uiDoneArchive2 = do
  command <- val ref_command
  unless (cmd_subcommand command) $ do
    condPrintLineNeedSeparator "" "\n\n"

-- |���������� ���������� ���������
uiDoneSubCommand command subCommand results = do
  ref_command =: command
  display_option' =: opt_display command

-- |���������� ������� ���������, ���������� ��������� ���������� �� ���� ������������ �������
uiDoneCommand Command{cmd_name=cmd} totals = do
  let sum4 (a0,b0,c0,d0) (a,b,c,d)   =  (a0+a,b0+b,c0+c,d0+d)
      (counts, files, bytes, cbytes) =  foldl sum4 (0,0,0,0) totals
  when (counts>1) $ do
    condPrintLine "s" $ "Total: "++show_archives3 counts++", "
                                 ++show_files3    files ++", "
                                 ++if (cbytes>=0)
                                     then show_ratio cmd bytes cbytes
                                     else show_bytes3 bytes
    condPrintLineNeedSeparator "s" "\n\n\n"

-- |��������� ���������� ���������
uiDoneProgram = do
  condPrintLineNeedSeparator "" "\n"


{-# NOINLINE uiStartArchive #-}
{-# NOINLINE uiStartProcessing #-}
{-# NOINLINE uiStartFile #-}
{-# NOINLINE uiCorrectTotal #-}
{-# NOINLINE uiUnpackedBytes #-}
{-# NOINLINE uiCompressedBytes #-}
{-# NOINLINE uiUpdateProgressIndicator #-}
{-# NOINLINE uiDoneArchive #-}
{-# NOINLINE uiDoneCommand #-}


----------------------------------------------------------------------------------------------------
---- ����� ���������, ��������������� ����������� ������� ------------------------------------------
----------------------------------------------------------------------------------------------------

msgStart cmd arcExist =
                case (cmdType cmd, arcExist) of
                  (ADD_CMD,     False)  ->  "Creating archive: "
                  (ADD_CMD,     True)   ->  "Updating archive: "
                  (LIST_CMD,    _)      ->  "Listing archive: "
                  (TEST_CMD,    _)      ->  "Testing archive: "
                  (EXTRACT_CMD, _)      ->  "Extracting archive: "

msgDo cmd    =  case (cmdType cmd) of
                  ADD_CMD     -> "Compressing "
                  TEST_CMD    -> "Testing "
                  EXTRACT_CMD -> "Extracting "

msgFile      =  ("  "++).msgDo

msgDone cmd  =  case (cmdType cmd) of
                  ADD_CMD     -> "Compressed "
                  TEST_CMD    -> "Tested "
                  EXTRACT_CMD -> "Extracted "

msgStat cmd  =  case (cmdType cmd) of
                  ADD_CMD     -> "Compression "
                  TEST_CMD    -> "Testing "
                  EXTRACT_CMD -> "Extraction "

-- |���������� "file" ��� "files", � ����������� �� ���-��
show_files3 1 = "1 file"
show_files3 n = show3 n ++ " files"

-- |���������� "archive" ��� "archives", � ����������� �� ���-��
show_archives3 1 = "1 archive"
show_archives3 n = show3 n ++ " archives"

-- |���������� "byte" ��� "bytes", � ����������� �� ���-��
show_bytes3 1 = "1 byte"
show_bytes3 n = show3 n ++ " bytes"


----------------------------------------------------------------------------------------------------
---- ��������� ��������� ---------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |���� ���������� ��������� (����������, ��������, ������� ��������)
data Indicator = NoIndicator | ShortIndicator | LongIndicator   deriving (Eq)

bytes_per_sec = 1*mb_  -- Typical (de)compression speed

-- |������� ��������� ���������, ����������� �� ���������� ���������� :)
select_indicator command total_bytes  =  case (opt_indicator command)
  of "0"                                    ->  NoIndicator      -- ����� "-i" - ��������� ���������!
     _ | i total_bytes < bytes_per_sec*100  ->  ShortIndicator   -- ��������� � ���������, ���� ����� ����� ������ ������ 100 �� (��� ���� � ������� �������������� ������ ������ �������� ������)
       | otherwise                          ->  LongIndicator    -- ��������� � ������� ����� ��������, ���� ������ ������ 100 ��

-- |������� ��������� ��������� � ������������ � ��������� ���������
percents NoIndicator    current total  =  ""
percents ShortIndicator current total  =  right_justify 3 (ratio2 current total) ++ "%"
percents LongIndicator  current total  =  right_justify 5 (ratio3 current total) ++ "%"

-- |������� ����� ��� ���������� ���������
open_percents     =  flip replicate ' '  . indicator_len
-- |��������� ����� �� ������� ��������, ������� �������� ��������� ���������
back_percents     =  flip replicate '\b' . indicator_len
-- |���������� ������� ������ ����������������� ���������� ���������
clear_percents i  =  back_percents i ++ open_percents i

-- |������ ���������� ��������� � ��������
indicator_len NoIndicator    = 0
indicator_len ShortIndicator = 4
indicator_len LongIndicator  = 6


----------------------------------------------------------------------------------------------------
---- ��������������� ������� ��� �������������� �����/����� � ������ � �������� --------------------
----------------------------------------------------------------------------------------------------

-- |������� ����� ����� ��������� � �������� - ���������� ����������� ����������� �������������!!!
diffTimes (TOD sa pa) (TOD sb pb)  =  i(sa - sb) + (i(pa-pb) / 1e12)

-- |������� �����, ������� ������, �������� � �.�.: "1.234.567"
show3 :: (Show a) => a -> [Char]
show3 = reverse.xxx.reverse.show
          where xxx (a:b:c:d:e) = a:b:c:'.': xxx (d:e)
                xxx a = a

-- |Format percent ratio with 2 digits
ratio2 count 0     =  "0"
ratio2 count total =  show$ count*100 `div` total

-- |Format percent ratio with 2+1 digits
ratio3 count 0     =  "0.0"
ratio3 count total =  case (show$ count*1000 `div` total) of
                        [digit]  -> "0." ++ [digit]
                        digits   -> init digits ++ ['.', last digits]

-- |���������� ����� �������� � ����������� ������, � ������� ������
show_ratio cmd bytes cbytes =
  ""        ++ show3       (if (cmdType cmd == ADD_CMD) then bytes else cbytes) ++
   " => "   ++ show_bytes3 (if (cmdType cmd == ADD_CMD) then cbytes else bytes) ++ ". " ++
   "Ratio " ++ ratio3 cbytes bytes ++ "%"

-- |���������� ������, ����������� �������� �����
showTime secs  =  showFFloat (Just 2) secs " secs"

-- |���������� ������, ����������� �������� ��������
showSpeed bytes secs  =  show3(round$ i bytes/1000/secs) ++ " kb/s"


-- |�������� �����, ����� ���� ���������� ����������� ����� ��������� (����� ��� ���������� ����������)
debugLog label = do
  condPrintLine   "$" $   label   -- �������� label � ���������� � ��������
  real_secs <- return_real_secs
  condPrintLineLn "$" $  ": " ++ showTime real_secs

-- |�����, ������� ��������� � ������ ���������� ������� ��� ������� �������
return_real_secs = do
  start_time    <- val refStartArchiveTime
  current_time  <- getClockTime
  return$ diffTimes current_time start_time

{-# NOINLINE diffTimes #-}
{-# NOINLINE show3 #-}
{-# NOINLINE ratio2 #-}
{-# NOINLINE ratio3 #-}
{-# NOINLINE show_ratio #-}
{-# NOINLINE debugLog #-}


----------------------------------------------------------------------------------------------------
---- ���������� �������� �� ���� ��������� ������� ������� -----------------------------------------
----------------------------------------------------------------------------------------------------

-- |��������� ���������� ������ �������� - ��������� ����� � ��������� ��������, � �������� ��� ���� �������
data ActionState = ActionState ClockTime String

-- |��������� �������� `action` ��� ���������� `param` ���� � ������� � ���������� ����������
-- ������ �� ����� `maxdiff` ������ � `param` ����� ����������
do_if old_state new_time maxdiff action new_param = do
  let refresh  =  case old_state of
        Nothing                               -> True
        Just (ActionState old_time old_param) -> and [ new_param /= old_param
                                                     , diffTimes new_time old_time >= maxdiff ]
  if refresh
    then do action new_param
            return$ Just$ ActionState new_time new_param
    else    return$ old_state

{-# NOINLINE do_if #-}

