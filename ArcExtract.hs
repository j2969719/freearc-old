----------------------------------------------------------------------------------------------------
---- Реализация команд распаковки и получения листинга архива                                   ----
----------------------------------------------------------------------------------------------------
module ArcExtract ( runArchiveExtract
                  , runArchiveList
                  , runCommentWrite
                  ) where

import Prelude hiding (catch)
import Control.Exception
import Control.Monad
import Data.List
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc
import Numeric

import Utils
import Files
import Process
import Errors
import FileInfo
import Compression         (aINIT_CRC, updateCRC, finishCRC)
import Cmdline
import CUI
import Statistics
import ArhiveStructure     (ftComment)
import ArhiveDirectory
import ArcvProcessExtract

-- |Обобщённая команда распаковки архива
runArchiveExtract pretestArchive
                  command@Command{ cmd_arcname        = arcname
                                 , cmd_archive_filter = archive_filter
                                 , opt_arc_basedir    = arc_basedir
                                 , opt_disk_basedir   = disk_basedir
                                 , opt_arccmt_file    = arccmt_file
                                 , opt_unParseFile    = unParseFile
                                 } = do
    -- Суперэкономия памяти: find_archives -> buffer 10_000 -> read_dir -> buffer 10_000 -> arcExtract
  doFinally uiDoneArchive2 $ do
  uiStartArchive command "" undefined  -- сообщить пользователю о начале обработки очередного архива
  command <- (command.$ opt_cook_passwords) command ask_passwords  -- подготовить пароли в команде к использованию
  let openArchive = archiveReadInfo command arc_basedir disk_basedir archive_filter (pretestArchive command)
  bracketCtrlBreak (openArchive arcname) (arcClose)$ \archive -> do
    uiPrintArcComment (arcComment archive)           -- Напечатать комментарий
    when (arccmt_file/="-" && arccmt_file/="--") $   -- и записать его в файл, указанный опцией -z
      unParseFile 'c' arccmt_file (arcComment archive)
    arcExtract command archive
  uiDoneArchive  -- напечатать и вернуть в вызывающую процедуру статистику выполнения команды

-- |Распаковка архива
arcExtract command arcinfo = do
  -- Процедура, используемая для обработки каждого файла
  let process_file = case (cmd_name command) of
                       "t"  -> test_file
                       _    -> extract_file (fpFullname.fiDiskName) command
  -- Отобразить в UI общий объём распаковываемых файлов и объём уже распакованного каталога архива
  uiStartProcessing (map cfFileInfo (arcDirectory arcinfo))
  uiStartDirectory
  uiUnpackedBytes   (arcDirBytes  arcinfo)
  uiCompressedBytes (arcDirCBytes arcinfo)
  uiStartFiles 0
  -- Создать процесс для распаковки файлов
  decompress_pipe <- runAsyncP$ decompress_PROCESS command (uiCompressedBytes.i)
  -- Распаковать каждый распаковываемый файл и выругаться на нераспаковываемые
  let (filesToSkip, filesToExtract)  =  partition isCompressedFake (arcDirectory arcinfo)
  for filesToExtract (process_file decompress_pipe)   -- runP$ enum_files |> decompress |> write_files
  unless (null filesToSkip)$  do registerWarning$ SKIPPED_FAKE_FILES (length filesToSkip)
  -- Завершить работу подпроцесса распаковки
  sendP decompress_pipe Nothing
  joinP decompress_pipe

-- |Тестирование одного файла из архива
test_file decompress_pipe compressed_file = do
  uiStartFile (cfFileInfo compressed_file)
  run_decompress decompress_pipe compressed_file (\buf size -> return ())
  return ()

-- |Распаковка одного файла из архива
extract_file filename_func command decompress_pipe compressed_file = do
  let fileinfo  = cfFileInfo compressed_file
      filename  = filename_func fileinfo
  if (fiIsDir fileinfo)
    then do uiStartFile fileinfo
            createDirectoryHierarchy filename
    else do
  -- Продолжить при условии, что этот файл позволено распаковать
  whenM (can_be_extracted command filename fileinfo)$ do
    uiStartFile fileinfo
    buildPathTo filename
    outfile  <- fileCreate filename
    let closeOutfile ok = do   -- Процедура, выполняемая после распаковки файла или при выходе по ^Break
          fileClose outfile                              -- to do: если используется fileSetSize, то изменить размер файла в соответствии с количеством реально распакованных байт
          if ok || opt_keep_broken command
            then do setFileDateTimeAttr filename fileinfo   -- Распаковано успешно или нужно сохранять даже файлы, распакованные с ошибками
                    when (opt_clear_archive_bit command) $ do
                        clearArchiveBit filename            -- Опция -ac - очистить атрибут Archive после распаковки
            else fileRemove filename                     -- Удалить файл, распакованный с ошибками
    do  --fileSetSize outfile (fiSize fileinfo)  -- Приличная ОС при этом выделит на диске место для файла одним куском
        handleCtrlBreak (closeOutfile False) $ do
          ok <- run_decompress decompress_pipe compressed_file (fileWriteBuf outfile)
          closeOutfile ok


-- |Эта функция определяет - можно ли извлечь файл из архива?
-- Ответ зависит от 1) использованных опций (-u/-f/-sync)
--                  2) наличия на диске предыдущего файла
--                  3) того, какой из файлов свежее - на диске или в архиве
--                  4) значения опций "-o" и "y"
--                  5) ответа пользователя на запрос о перезаписи файла
--
can_be_extracted cmd filename arcfile = do
  diskfile_exist <- fileExist filename
  if not diskfile_exist                         -- Если файл на диске не существует
    then return (opt_update_type cmd /= 'f')    -- то извлечь файл из архива можно во всех случаях, кроме '-f'
    else do
  diskfile_time     <-  getFileDateTime filename
  let arcfile_newer  =  fiTime arcfile > diskfile_time   -- файл в архиве свежее, чем на диске?
  let overwrite = case (opt_update_type cmd) of
                    'f' -> arcfile_newer
                    'u' -> arcfile_newer
                    's' -> error "--sync can't be used on extract"
                    'a' -> True
  if overwrite
    then askOverwrite filename (opt_overwrite cmd) arcfile_newer
    else return False

{-# NOINLINE run_decompress #-}
-- |Распаковка файла из архива с проверкой CRC
run_decompress decompress_pipe compressed_file write_data = do
  crc <- ref aINIT_CRC                        -- Инициализируем значение CRC
  let writer buf len = do
        uiUnpackedBytes  (i len)              -- Информируем пользователя о ходе распаковки
        uiUpdateProgressIndicator (i len)     -- -.-
        crc          .<- updateCRC buf len    -- Обновим CRC содержимым буфера
        write_data       buf len              -- Запишем данные в файл
        send_backP       decompress_pipe ()   -- И возвратим использованный буфер
  decompress_file decompress_pipe compressed_file writer
  acrc  <-  val crc >>== finishCRC            -- Вычислим окончательное значение CRC
  when (cfCRC compressed_file /= acrc) $ do
    registerWarning$ BAD_CRC (fpFullname$ fiStoredName$ cfFileInfo compressed_file)
  return (cfCRC compressed_file == acrc)      -- Возвратить True, если всё ок


----------------------------------------------------------------------------------------------------
---- Запись комментария к архиву в файл (команда "cw")                                          ----
----------------------------------------------------------------------------------------------------

-- |Реализация команды "cw" - запись комментария к архиву в файл
runCommentWrite command@Command{ cmd_filespecs   = filespecs
                               , cmd_arcname     = arcname
                               , opt_unParseFile = unParseFile
                               } = do
  doFinally uiDoneArchive2 $ do
  when (length filespecs /= 1) $
    registerError$ CMDLINE_GENERAL "command syntax is \"cw archive outfile\""
  let [outfile] = filespecs
  command <- (command.$ opt_cook_passwords) command ask_passwords  -- подготовить пароли в команде к использованию
  printLineLn$ "Writing archive comment of "++arcname++" to "++outfile
  bracket (archiveReadFooter command arcname) (archiveClose.fst) $ \(_,footer) -> do
    unParseFile 'c' outfile (ftComment footer)
  return (0,0,0,0)


----------------------------------------------------------------------------------------------------
---- Печать листинга архива:                                                                    ----
----    - для пользователя (команда "l")                                                        ----
----    - для создания файл-листов (команда "lb")                                               ----
----    - для других программ (команда "v")                                                     ----
---------------------------------------------------------------------------------------------------

-- |Обобщённая команда получения листинга архива
runArchiveList pretestArchive
               command@Command{ cmd_arclist        = arclist
                              , cmd_arcname        = arcname
                              , opt_arc_basedir    = arc_basedir
                              , cmd_archive_filter = archive_filter
                              } = do
  command <- (command.$ opt_cook_passwords) command ask_passwords  -- подготовить пароли в команде к использованию
  bracket (archiveReadInfo command arc_basedir "" archive_filter (pretestArchive command) arcname) (arcClose) $
      archiveList command (length arclist==1)

-- |Листинг архива
archiveList command @ Command{ cmd_name = cmd, cmd_arcname = arcname }
            show_empty
            arc @ ArchiveInfo{ arcDirectory = directory } = do
  let files = length directory
      bytes = sum$ map (fiSize.cfFileInfo) directory
  when (files>0 || show_empty) $ do
    doFinally uiDoneArchive2 $ do
    uiStartArchive command "" undefined -- Сообщить пользователю о начале обработки очередного архива
    case cmd of
      "l" -> do uiPrintArcComment (arcComment arc)
                putStrLn$ "Date/time                  Size Filename"
                putStrLn$ "----------------------------------------"
                mapM_ terse_list directory
                putStrLn$ "----------------------------------------"
                putStr$   show3 files ++ " files, " ++ show3 bytes ++ " bytes"

      "lb"-> do putStr$ joinWith "\n"$ map filename directory

      "v" -> do uiPrintArcComment (arcComment arc)
                putStrLn$ "Date/time              Attr            Size          Packed      CRC Filename"
                putStrLn$ "-----------------------------------------------------------------------------"
                mapM_ verbose_list directory
                putStrLn$ "-----------------------------------------------------------------------------"
                putStr$   show3 files ++ " files, " ++ show3 bytes ++ " bytes"

  return (1, files, bytes, -1)

-- |Имя файла
filename = str2terminal . fpFullname . fiStoredName . cfFileInfo

-- |Однострочный простой листинг файла
terse_list direntry = do
  let fi = cfFileInfo direntry
  timestr <- formatDateTime (fiTime fi)
  putStrLn$        timestr
         ++ " " ++ right_justify 11 (if (fiIsDir fi) then ("-dir-") else (show3$ fiSize fi))
         ++ " " ++ filename direntry

-- |Однострочный подробный листинг файла
verbose_list direntry = do
  let fi = cfFileInfo direntry
  timestr <- formatDateTime (fiTime fi)
  putStrLn$        timestr
         ++ " " ++ (if (fiIsDir fi)  then ".D....."  else ".......")
         ++ " " ++ right_justify 15 (show$ fiSize fi)
         ++ " " ++ right_justify 15 "0"
         ++ " " ++ left_fill  '0' 8 (showHex (cfCRC direntry) "")
         ++ " " ++ filename direntry

{-
-- |Многострочный технический листинг файла
technical_list direntry = do
  let fi = (cfFileInfo direntry)
  timestr <- formatDateTime (fiTime fi)
  putStrLn$ ""
  putStrLn$ "Filename: "  ++ (str2terminal$ fpFullname$ fiStoredName fi)
  putStrLn$ "Size: "      ++ (show$ fiSize fi)
  putStrLn$ "Date/time: " ++ timestr
  putStrLn$ "CRC: "       ++ showHex (cfCRC direntry) ""
  putStrLn$ "Type: "      ++ if (fiIsDir fi) then "directory" else "file"
-}

-- |Отформатировать CTime в строку с форматом "%Y-%m-%d %H:%M:%S"
formatDateTime t  =  allocaBytes 100 $ \buf -> do
                       c_FormatDateTime buf 100 t
                       peekCString buf

foreign import ccall unsafe "Environment.h FormatDateTime"
  c_FormatDateTime :: CString -> CInt -> CTime -> IO ()

