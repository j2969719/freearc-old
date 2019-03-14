----------------------------------------------------------------------------------------------------
---- Сбор и отображение статистики работы программы (объём обработанных данных, скорость и т.д.) ---
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

-- |Здесь хранится вся информация о команде и процессе её выполнения, требуемая для отображения
-- индикатора прогресса и вывода финальной статистики
data UI_State = UI_State {
    command         :: !Command     -- Выполняемая сейчас команда
  , total_files     :: !FileCount   -- Кол-во файлов, которые она должна обработать
  , total_bytes     :: !FileSize    -- Общий объём этих файлов (в распакованном виде)
  , datatype        ::  DataType    -- Обрабатываемая в данный момент часть архива: файл/каталог/служебные данные
  -- В зависимости от того, какая часть архива сейчас обрабатывается, статистика заносится
  -- либо на счёт файлов:
  ,    files        :: !FileCount   -- Кол-во уже обработанных файлов
  ,    bytes        :: !FileSize    -- Объём уже обработанных данных в распакованном виде
  ,    cbytes       :: !FileSize    -- Объём уже обработанных данных в упакованном виде
  -- либо на счёт каталогов (служебная информация не подсчитывается):
  ,    dirs         :: !FileCount   -- Кол-во созданных каталогов и других служебных блоков
  ,    dir_bytes    :: !FileSize    -- Объём уже обработанных данных в распакованном виде
  ,    dir_cbytes   :: !FileSize    -- Объём уже обработанных данных в упакованном виде
  -- Кроме того, мы запоминаем, какая часть из этих данных - на самом деле не упаковывалась (это полезно для определения реальной скорости упаковки):
  ,    fake_files   :: !FileCount   -- Кол-во уже обработанных файлов
  ,    fake_bytes   :: !FileSize    -- Объём уже обработанных данных в распакованном виде
  ,    fake_cbytes  :: !FileSize    -- Объём уже обработанных данных в упакованном виде
  -- Информация о текущем солид-блоке
  ,    algorithmsCount :: Int       -- Кол-во алгоритмов в цепочке
  ,    rw_ops       :: [[UI_RW FileSize]] -- Последовательность операций чтения/записи с разбивкой по отдельным алгоритмам
  ,    r_bytes      :: FileSize     -- Объём уже обработанных данных на входе первого алгоритма сжатия
  ,    rnum_bytes   :: FileSize     -- Объём уже обработанных данных на входе последнего алгоритма сжатия
  }

-- |Обрабатываемая в данный момент часть архива: файл/каталог/служебные данные
data DataType = File | Dir | CData   deriving (Eq)

ref_command               =  unsafePerformIO$ newIORef$ error "undefined CUI::ref_command"
refStartArchiveTime       =  unsafePerformIO$ newIORef$ error "undefined CUI::refStartArchiveTime"
refArchiveProcessingTime  =  unsafePerformIO$ newIORef$ error "undefined CUI::refArchiveProcessingTime"  :: IORef Double
ref_ui_state              =  unsafePerformIO$ newIORef$ error "undefined CUI::ref_ui_state"
putHeader                 =  unsafePerformIO$ init_once


-- |Отметить начало выполнения программы
uiStartProgram = do
  runIndicators

-- |Отметить начало выполнения команды
uiStartCommand command = do
  ref_command =: command
  display_option' =: opt_display command
  refStartArchiveTime =:: getClockTime
  -- Открыть логфайл и вывести в него выполняемую команду
  openLogFile (opt_logfile command)
  curdir <- getCurrentDirectory
  printLog (curdir++">"++cmd_command_text command++"\n")
  -- Вывеедем версию архиватора и используемые дополнительные опции
  let addArgs = cmd_additional_args command
  once putHeader$ condPrintLine "h" aARC_HEADER
  condPrintLine "o" (addArgs &&& "Using additional options: "++addArgs++"\n")
  hFlush stdout

-- |Отметить начало выполнения подкоманды
uiStartSubCommand command subCommand = do
  ref_command =: subCommand
  display_option' =: opt_display subCommand

-- |Отметить начало обработки очередного архива
uiStartArchive command @ Command {
                 opt_data_compressor = compressor
               , opt_cache           = cache
               }
               method
               limited_compressor = do
  -- Запомнить время начала обработки архива и выполняемую команду
  refStartArchiveTime =:: getClockTime
  ref_command =: command
  display_option' =: opt_display command

  -- Остаток процедуры не нужно выполнять, если это под-команда (например, тестирование после архивации)
  if cmd_subcommand command
    then do condPrintLineNeedSeparator "" "\n"
    else do

  -- Вывести сообщение типа "Testing archive ..."
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

-- |Отметить начало упаковки или распаковки данных
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
  -- Вывод этого "разделителя" позволит затереть на экране строчку с текущей статистикой
  printLineNeedSeparator $ "\r"++replicate 75 ' '++"\r"
  when (opt_indicator command == "1") $ do
    putStr$ ". Processed "
  -- Сложный progress indicator учитывает также процент обработанных файлов, что позволяет ему работать плавнее
  -- Данные с файлами смешиваются из расчёта: скорость упаковки 1мб/с, время открытия файла - 10 мсек
  let current bytes = do ui_state <- val ref_ui_state
                         return$ bytes + (bytes_per_sec `div` 100)*i (files ui_state)
      total = do ui_state <- val ref_ui_state
                 return$ total_bytes ui_state + (bytes_per_sec `div` 100)*i (total_files ui_state)
  uiStartProgressIndicator command current total
  hFlush stdout


-- |Отметить начало упаковки/распаковки файлов
uiStartFiles count = do
  syncUI $ do
  modifyIORef ref_ui_state $ \ui_state ->
    ui_state { datatype        = File
             , algorithmsCount = count
             , rw_ops          = replicate count []
             , r_bytes         = 0
             , rnum_bytes      = 0
             }

-- |Отметить начало упаковки/распаковки каталога архива
uiStartDirectory = do
  syncUI $ do
  modifyIORef ref_ui_state $ \ui_state ->
    ui_state { datatype        = Dir
             , dirs            = dirs ui_state + 1
             , algorithmsCount = 0 }

-- |Отметить начало упаковки/распаковки служебной информации архива
uiStartControlData = do
  syncUI $ do
  modifyIORef ref_ui_state $ \ui_state ->
    ui_state { datatype        = CData
             , algorithmsCount = 0 }

-- |Отметить начало упаковки/распаковки файла
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

-- |Откорректировать total_bytes в ui_state
uiCorrectTotal files bytes = do
  syncUI $ do
  when (files/=0 || bytes/=0) $ do
    modifyIORef ref_ui_state $ \ui_state ->
      ui_state { total_files = total_files ui_state + files
               , total_bytes = total_bytes ui_state + bytes }

-- |Отметить имитацию обработки файлов согласно прилагаемому списку
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

-- |Отметить, что было обработано столько-то байт сжатых данных (неважно, результат
-- ли это упаковки, входные данные для распаковки или просто упакованные данные, переданные
-- из старого архива в новый без какой-либо перепаковки)
uiCompressedBytes len = do
  syncUI $ do
  modifyIORef ref_ui_state $ \ui_state ->
    case (datatype ui_state) of
      File  ->  ui_state {     cbytes =     cbytes ui_state + len }
      Dir   ->  ui_state { dir_cbytes = dir_cbytes ui_state + len }
      CData ->  ui_state

-- |Отметить, что было обработано столько-то байт распакованных данных (даже если реально
-- эти байты никто в глаза не видел, поскольку они были переданы в сжатом виде из архива в архив)
uiUnpackedBytes len = do
  syncUI $ do
  modifyIORef ref_ui_state $ \ui_state ->
    case (datatype ui_state) of
      File  ->  ui_state {     bytes =     bytes ui_state + len }
      Dir   ->  ui_state { dir_bytes = dir_bytes ui_state + len }
      CData ->  ui_state


----------------------------------------------------------------------------------------------------
---- Очередь операций r/w, по которым при упаковке вычисляется индикатор прогресса -----------------
----------------------------------------------------------------------------------------------------

-- Операции чтения и записи в списке операций
data UI_RW a = UI_Read a | UI_Write a deriving Show

-- Добавить операцию чтения/записи в голову списка, сливая операции одного типа
add_Read  a (UI_Write 0:UI_Read  0:ops) = (UI_Read a:ops)  -- избавиться от useless пары r0+w0
add_Read  a (UI_Read  b:ops) = (UI_Read (a+b):ops)
add_Read  a             ops  = (UI_Read  a   :ops)

add_Write a (UI_Write b:ops) = (UI_Write(a+b):ops)
add_Write a             ops  = (UI_Write a   :ops)

-- |Алгоритм номер num в цепочке обещает записать bytes байт, соответствующих последнему блоку прочитанных
-- данных (эта операция "обещания записи" позволяет поддерживать аккуратный индикатор прогресса)
uiQuasiWriteData num bytes = do
  -- Реализация устроена так, что последним прочитанным данным сопоставляются bytes записанных данных,
  -- но при этом общий размер записанных данных не изменяется ни на йоту ;)
  uiWriteData num bytes
  uiReadData  num 0
  uiWriteData num (-bytes)

-- |Алгоритм номер num в цепочке записал bytes байт
uiWriteData num bytes = do
  UI_State {algorithmsCount=count, datatype=datatype} <- val ref_ui_state
  when (datatype == File) $ do
  -- Сохранить в список операций в/в операцию чтения
  when (num>=1 && num<count) $ do
    syncUI $ do
    modifyIORef ref_ui_state $ \ui_state @ UI_State {rw_ops=rw_ops} ->
      ui_state {rw_ops = updateAt num (add_Write bytes) rw_ops}

-- |Алгоритм номер num в цепочке прочитал bytes байт
uiReadData num bytes = do
  UI_State {algorithmsCount=count, datatype=datatype} <- val ref_ui_state
  when (datatype == File) $ do
  -- Сохранить в список операций в/в операцию записи
  when (num>=1 && num<count) $ do
    syncUI $ do
    modifyIORef ref_ui_state $ \ui_state @ UI_State {rw_ops=rw_ops} ->
      ui_state {rw_ops = updateAt num (add_Read bytes) rw_ops}
  -- Обновить индикатор прогресса, если это последний алгоритм сжатия в цепочке
  when (num>=1 && num==count) $ do
    unpBytes <- syncUI $ do
      -- Состояние до обработки этих байт
      ui_state @ UI_State {r_bytes=r_bytes0, rnum_bytes=rnum_bytes0, rw_ops=rw_ops0}  <-  val ref_ui_state
      -- К байтам на входе алгоритма num добавляется bytes байт,
      -- высчитываем количество байт на входе первого алгоритма если этот блок не похож на просто заголовок (bytes>16)
      let rnum_bytes = rnum_bytes0+bytes
          (r_bytes, rw_ops) = if bytes>16
                                 then calc num (reverse rw_ops0) [] rnum_bytes
                                 else (r_bytes0, rw_ops0)
      ref_ui_state =: ui_state {r_bytes=r_bytes, rnum_bytes=rnum_bytes, rw_ops=rw_ops}
      --for rw_ops $ \x -> print (reverse x)
      --print (rnum_bytes0, bytes, r_bytes0, r_bytes-r_bytes0)
      -- Возвращаем количество байт на входе первого алгоритма относительно предыдущего значения этой величины
      return (r_bytes-r_bytes0)
    uiUpdateProgressIndicator ((unpBytes*9) `div` 10)
  when (num==1) $ do  -- 90% на последний алгоритм в цепочке и 10% на первый (чтобы сгладить вывод для external compression and so on)
    uiUpdateProgressIndicator (bytes `div` 10)

 where
  -- Рекурсивно пересчитать bytes байт на входе алгоритма num в количество байт на входе алгоритма 1
  -- Заодно уж обновить очередь операций, просуммировав операции предшествуюшие текущей точке интереса
  calc 1   _                new_ops bytes = (bytes, []:new_ops)
  calc num (old_op:old_ops) new_ops bytes =
    -- Пересчитать bytes байт на выходе алгоритма num-1 в байты на его входе
    let (new_bytes, new_op) = go 0 bytes (0,0) (reverse old_op)
    in calc (num-1) old_ops (reverse new_op:new_ops) new_bytes

  -- Пересчитывает записанные байты (restW) в прочитанные (totalR) согласно последовательности операций ввода/вывода
  go totalR restW (rsum,wsum) ops@(UI_Read r:UI_Write w:rest_ops)
       -- Если следующий кусок упакованных данных больше нашего остатка, то увеличиваем totalR на него и движемся дальше
       | w<restW   = go (totalR+r) (restW-w) (rsum+r,wsum+w) rest_ops
       -- Иначе делим его пропорционально (r/w * restW) и добавляем к totalR
       | otherwise = (totalR + ((r*restW) `div` max w 1), UI_Read rsum:UI_Write wsum:ops)
  -- Все прочие варианты
  go totalR _ (rsum,wsum) ops  =  (totalR, UI_Read rsum:UI_Write wsum:ops)


----------------------------------------------------------------------------------------------------
---- Вычисление индикатора прогресса ---------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Глобальная переменная, хранящая состояние индикатора прогресса
aProgressIndicatorState  =  unsafePerformIO$ newIORef$ error "undefined CUI::aProgressIndicatorState"

-- |Инициализировать индикатор прогресса
uiStartProgressIndicator command bytes' total' = do
  bytes <- bytes' 0;  total <- total'
  let cmd        =  cmd_name command
      direction  =  if (cmdType cmd == ADD_CMD)  then " => "  else " <= "
      arcname    =  str2terminal$ cmd_arcname command
      indicator  =  select_indicator command total
  aProgressIndicatorState =: (indicator, arcname, direction, 0, bytes', total')
  putStr$ percents indicator bytes total

-- |Вывести на экран и в заголовок окна индикатор прогресса (сколько процентов данных уже обработано)
uiUpdateProgressIndicator add_b = when (add_b/=0) $ do
  -- Маленькая индейская хитрость: эта функция вызывается ПЕРЕД какой-либо обработкой
  -- данных. При этом мы считаем, что предыдущие данные к данному моменту уже обработаны и
  -- рапортуем об этом. Новые же данные только добавляются к счётчику, но не влияют
  -- на выводимую СЕЙЧАС статистику. Вот такие вот приколы в нашем городке :)
  syncUI $ do
  aProgramTerminated <- val programTerminated
  (indicator, arcname, direction, b, bytes', total') <- val aProgressIndicatorState
  when (indicator /= NoIndicator  &&  not aProgramTerminated) $ do
    bytes <- bytes' b;  total <- total'
    let p = percents indicator bytes total
    indicator_state =: Just (back_percents indicator ++ p)
    title_state     =: Just (trimLeft p ++ direction ++ arcname)
    aProgressIndicatorState =: (indicator, arcname, direction, b+add_b, bytes', total')

-- |Приостановить вывод индикатора прогресса и стереть его следы
uiSuspendProgressIndicator = do
  indicator_state =: Nothing
  title_state     =: Nothing
  (indicator, arcname, direction, b, bytes', total') <- val aProgressIndicatorState
  putStr$ clear_percents indicator
  hFlush stdout

-- |Возобновить вывод индикатора прогресса и вывести его текущее значение
uiResumeProgressIndicator = do
  (indicator, arcname, direction, b, bytes', total') <- val aProgressIndicatorState
  bytes <- bytes' b;  total <- total'
  putStr$ percents indicator bytes total
  hFlush stdout

-- |Завершить вывод индикатора прогресса
uiDoneProgressIndicator = do
  uiSuspendProgressIndicator
  aProgressIndicatorState =: (NoIndicator, undefined, undefined, undefined, undefined, undefined)

-- |Завершить вывод индикатора прогресса
uiWithProgressIndicator command arcsize action = do
  uiStartProgressIndicator command return (return arcsize)
  ensureCtrlBreak uiDoneProgressIndicator action


----------------------------------------------------------------------------------------------------
---- Отображение индикатора прогресса --------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- Индикаторы прогресса в консоли и её заголовке
indicator_state = unsafePerformIO$ newIORef Nothing
title_state     = unsafePerformIO$ newIORef Nothing

-- |Запускает два треда для вывода индикаторов прогресса
runIndicators = do
  -- Обновляем индикатор прогресса раз в 0.5 секунды
  indicatorThread 0.5 indicator_state (putStr .>> hFlush stdout)
  -- Обновляем заголовок окна раз в 0.5 секунды
  indicatorThread 0.5 title_state     setConsoleTitle

-- |Тред, следящий за indicator, и выводящий время от времени его обновлённые значения
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

-- |Отметить начало упаковки или распаковки солид-блока
uiStartDeCompression deCompression = do
  x <- getCPUTime
  newMVar (x,deCompression,[])

-- |Добавить в список время работы одного из алгоритмов в цепочке
-- (вычисленное в сишном треде время работы упаковщика/распаковщика)
uiDeCompressionTime times t =  do
  modifyMVar_ times (\(x,y,ts) -> return (x, y, ts++[t]))

-- |Упаковка/распаковка солид-блока завершена - просуммировать время работы всех тредов
-- или использовать wall clock time, если хотя бы одно из возвращённых времён == -1
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

-- |Обработка очередного архива завершена -> напечатать статистику и вернуть её вызывающей процедуре
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
    printLineNeedSeparator ""  -- нужда в разделителе перед выводом следующих строк исчезла

  -- Статистика сжатия (не выводится для суб-команд, поскольку точно такую же статистику уже напечатала основная команда)
  unless (cmd_subcommand command) $ do
    condPrintLineLn "f" $ left_justify 75 $    -- без дополнительных пробелов может не переписаться полностью предыдущая строка
      msgDone cmd ++ show_files3 files ++ ", " ++ show_ratio cmd bytes cbytes
    -- Напечатать статистику по каталогу архива только если он достаточно велик
    when (dir_bytes>10^4) $ do
      condPrintLine   "d" $ "Directory " ++ (dirs>1 &&& "has " ++ show3 dirs ++ " chunks, ")
      condPrintLineLn "d" $                 show_ratio cmd dir_bytes dir_cbytes

  -- Информация о времени работы и скорости упаковки/распаковки
  secs <- val refArchiveProcessingTime   -- время, затраченное непосредственно на упаковку/распаковку
  real_secs <- return_real_secs          -- полное время выполнения команды над текущим архивом
  condPrintLine                     "t" $ msgStat cmd ++ "time " ++ showTime  secs
  when (secs>=0.01) $ condPrintLine "t" $ ", speed " ++ showSpeed (bytes-fake_bytes) secs
  condPrintLine                     "t" $ ". Total " ++ showTime  real_secs

  condPrintLineNeedSeparator "rdt" "\n"
  hFlush stdout
  resetConsoleTitle
  return (1,files,bytes,cbytes)

-- |Вызывается после всех вспомогательных операций (добавление recovery info, тестирование)
uiDoneArchive2 = do
  command <- val ref_command
  unless (cmd_subcommand command) $ do
    condPrintLineNeedSeparator "" "\n\n"

-- |Выполнение подкоманды завершено
uiDoneSubCommand command subCommand results = do
  ref_command =: command
  display_option' =: opt_display command

-- |Выполнение команды завершено, напечатать суммарную статистику по всем обработанным архивам
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

-- |Завершить выполнение программы
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
---- Выбор сообщений, соответствующих выполняемой команде ------------------------------------------
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

-- |Напечатать "file" или "files", в зависимости от кол-ва
show_files3 1 = "1 file"
show_files3 n = show3 n ++ " files"

-- |Напечатать "archive" или "archives", в зависимости от кол-ва
show_archives3 1 = "1 archive"
show_archives3 n = show3 n ++ " archives"

-- |Напечатать "byte" или "bytes", в зависимости от кол-ва
show_bytes3 1 = "1 byte"
show_bytes3 n = show3 n ++ " bytes"


----------------------------------------------------------------------------------------------------
---- Индикатор прогресса ---------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Типы индикатора прогресса (молчаливый, проценты, десятые процента)
data Indicator = NoIndicator | ShortIndicator | LongIndicator   deriving (Eq)

bytes_per_sec = 1*mb_  -- Typical (de)compression speed

-- |Выбрать индикатор прогресса, основываясь на показаниях свидетелей :)
select_indicator command total_bytes  =  case (opt_indicator command)
  of "0"                                    ->  NoIndicator      -- опция "-i" - отключить индикатор!
     _ | i total_bytes < bytes_per_sec*100  ->  ShortIndicator   -- индикатор в процентах, если общий объём данных меньше 100 мб (при этом в секунду обрабатывается больше одного процента данных)
       | otherwise                          ->  LongIndicator    -- индикатор в десятых долях процента, если данных больше 100 мб

-- |Вывести индикатор прогресса в соответствии с выбранной точностью
percents NoIndicator    current total  =  ""
percents ShortIndicator current total  =  right_justify 3 (ratio2 current total) ++ "%"
percents LongIndicator  current total  =  right_justify 5 (ratio3 current total) ++ "%"

-- |Создать место для индикатора прогресса
open_percents     =  flip replicate ' '  . indicator_len
-- |Вернуться назад на столько символов, сколько занимает индикатор прогресса
back_percents     =  flip replicate '\b' . indicator_len
-- |Напечатать пробелы поверх использовавшегося индикатора прогресса
clear_percents i  =  back_percents i ++ open_percents i

-- |Размер индикатора прогресса в символах
indicator_len NoIndicator    = 0
indicator_len ShortIndicator = 4
indicator_len LongIndicator  = 6


----------------------------------------------------------------------------------------------------
---- Вспомогательные функции для форматирования чисел/строк и работы с временем --------------------
----------------------------------------------------------------------------------------------------

-- |Разница между двумя временами в секундах - использует особенности внутреннего представления!!!
diffTimes (TOD sa pa) (TOD sb pb)  =  i(sa - sb) + (i(pa-pb) / 1e12)

-- |Вывести число, отделяя тысячи, миллионы и т.д.: "1.234.567"
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

-- |Напечатать объём исходных и упакованных данных, и степень сжатия
show_ratio cmd bytes cbytes =
  ""        ++ show3       (if (cmdType cmd == ADD_CMD) then bytes else cbytes) ++
   " => "   ++ show_bytes3 (if (cmdType cmd == ADD_CMD) then cbytes else bytes) ++ ". " ++
   "Ratio " ++ ratio3 cbytes bytes ++ "%"

-- |Возвратить строку, описывающую заданное время
showTime secs  =  showFFloat (Just 2) secs " secs"

-- |Возвратить строку, описывающую заданную скорость
showSpeed bytes secs  =  show3(round$ i bytes/1000/secs) ++ " kb/s"


-- |Отметить время, когда была достигнута определённая точка программы (чисто для внутренних бенчмарков)
debugLog label = do
  condPrintLine   "$" $   label   -- вычислим label и напечатаем её значение
  real_secs <- return_real_secs
  condPrintLineLn "$" $  ": " ++ showTime real_secs

-- |Время, реально прошедшее с начала выполнения команды над текущим архивом
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
---- Выполнение операции не чаще заданного периода времени -----------------------------------------
----------------------------------------------------------------------------------------------------

-- |Состояние выполнения некоей операции - последнее время и последний параметр, с которыми она была вызвана
data ActionState = ActionState ClockTime String

-- |Выполнить операцию `action` над параметром `param` если с момента её последнего выполнения
-- прошло не менее `maxdiff` секунд и `param` успел измениться
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

