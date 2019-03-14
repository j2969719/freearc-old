----------------------------------------------------------------------------------------------------
---- ���������� ������ ���������� � ��������� �������� ������                                   ----
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

-- |���������� ������� ���������� ������
runArchiveExtract pretestArchive
                  command@Command{ cmd_arcname        = arcname
                                 , cmd_archive_filter = archive_filter
                                 , opt_arc_basedir    = arc_basedir
                                 , opt_disk_basedir   = disk_basedir
                                 , opt_arccmt_file    = arccmt_file
                                 , opt_unParseFile    = unParseFile
                                 } = do
    -- ������������� ������: find_archives -> buffer 10_000 -> read_dir -> buffer 10_000 -> arcExtract
  doFinally uiDoneArchive2 $ do
  uiStartArchive command "" undefined  -- �������� ������������ � ������ ��������� ���������� ������
  command <- (command.$ opt_cook_passwords) command ask_passwords  -- ����������� ������ � ������� � �������������
  let openArchive = archiveReadInfo command arc_basedir disk_basedir archive_filter (pretestArchive command)
  bracketCtrlBreak (openArchive arcname) (arcClose)$ \archive -> do
    uiPrintArcComment (arcComment archive)           -- ���������� �����������
    when (arccmt_file/="-" && arccmt_file/="--") $   -- � �������� ��� � ����, ��������� ������ -z
      unParseFile 'c' arccmt_file (arcComment archive)
    arcExtract command archive
  uiDoneArchive  -- ���������� � ������� � ���������� ��������� ���������� ���������� �������

-- |���������� ������
arcExtract command arcinfo = do
  -- ���������, ������������ ��� ��������� ������� �����
  let process_file = case (cmd_name command) of
                       "t"  -> test_file
                       _    -> extract_file (fpFullname.fiDiskName) command
  -- ���������� � UI ����� ����� ��������������� ������ � ����� ��� �������������� �������� ������
  uiStartProcessing (map cfFileInfo (arcDirectory arcinfo))
  uiStartDirectory
  uiUnpackedBytes   (arcDirBytes  arcinfo)
  uiCompressedBytes (arcDirCBytes arcinfo)
  uiStartFiles 0
  -- ������� ������� ��� ���������� ������
  decompress_pipe <- runAsyncP$ decompress_PROCESS command (uiCompressedBytes.i)
  -- ����������� ������ ��������������� ���� � ���������� �� �����������������
  let (filesToSkip, filesToExtract)  =  partition isCompressedFake (arcDirectory arcinfo)
  for filesToExtract (process_file decompress_pipe)   -- runP$ enum_files |> decompress |> write_files
  unless (null filesToSkip)$  do registerWarning$ SKIPPED_FAKE_FILES (length filesToSkip)
  -- ��������� ������ ����������� ����������
  sendP decompress_pipe Nothing
  joinP decompress_pipe

-- |������������ ������ ����� �� ������
test_file decompress_pipe compressed_file = do
  uiStartFile (cfFileInfo compressed_file)
  run_decompress decompress_pipe compressed_file (\buf size -> return ())
  return ()

-- |���������� ������ ����� �� ������
extract_file filename_func command decompress_pipe compressed_file = do
  let fileinfo  = cfFileInfo compressed_file
      filename  = filename_func fileinfo
  if (fiIsDir fileinfo)
    then do uiStartFile fileinfo
            createDirectoryHierarchy filename
    else do
  -- ���������� ��� �������, ��� ���� ���� ��������� �����������
  whenM (can_be_extracted command filename fileinfo)$ do
    uiStartFile fileinfo
    buildPathTo filename
    outfile  <- fileCreate filename
    let closeOutfile ok = do   -- ���������, ����������� ����� ���������� ����� ��� ��� ������ �� ^Break
          fileClose outfile                              -- to do: ���� ������������ fileSetSize, �� �������� ������ ����� � ������������ � ����������� ������� ������������� ����
          if ok || opt_keep_broken command
            then do setFileDateTimeAttr filename fileinfo   -- ����������� ������� ��� ����� ��������� ���� �����, ������������� � ��������
                    when (opt_clear_archive_bit command) $ do
                        clearArchiveBit filename            -- ����� -ac - �������� ������� Archive ����� ����������
            else fileRemove filename                     -- ������� ����, ������������� � ��������
    do  --fileSetSize outfile (fiSize fileinfo)  -- ��������� �� ��� ���� ������� �� ����� ����� ��� ����� ����� ������
        handleCtrlBreak (closeOutfile False) $ do
          ok <- run_decompress decompress_pipe compressed_file (fileWriteBuf outfile)
          closeOutfile ok


-- |��� ������� ���������� - ����� �� ������� ���� �� ������?
-- ����� ������� �� 1) �������������� ����� (-u/-f/-sync)
--                  2) ������� �� ����� ����������� �����
--                  3) ����, ����� �� ������ ������ - �� ����� ��� � ������
--                  4) �������� ����� "-o" � "y"
--                  5) ������ ������������ �� ������ � ���������� �����
--
can_be_extracted cmd filename arcfile = do
  diskfile_exist <- fileExist filename
  if not diskfile_exist                         -- ���� ���� �� ����� �� ����������
    then return (opt_update_type cmd /= 'f')    -- �� ������� ���� �� ������ ����� �� ���� �������, ����� '-f'
    else do
  diskfile_time     <-  getFileDateTime filename
  let arcfile_newer  =  fiTime arcfile > diskfile_time   -- ���� � ������ ������, ��� �� �����?
  let overwrite = case (opt_update_type cmd) of
                    'f' -> arcfile_newer
                    'u' -> arcfile_newer
                    's' -> error "--sync can't be used on extract"
                    'a' -> True
  if overwrite
    then askOverwrite filename (opt_overwrite cmd) arcfile_newer
    else return False

{-# NOINLINE run_decompress #-}
-- |���������� ����� �� ������ � ��������� CRC
run_decompress decompress_pipe compressed_file write_data = do
  crc <- ref aINIT_CRC                        -- �������������� �������� CRC
  let writer buf len = do
        uiUnpackedBytes  (i len)              -- ����������� ������������ � ���� ����������
        uiUpdateProgressIndicator (i len)     -- -.-
        crc          .<- updateCRC buf len    -- ������� CRC ���������� ������
        write_data       buf len              -- ������� ������ � ����
        send_backP       decompress_pipe ()   -- � ��������� �������������� �����
  decompress_file decompress_pipe compressed_file writer
  acrc  <-  val crc >>== finishCRC            -- �������� ������������� �������� CRC
  when (cfCRC compressed_file /= acrc) $ do
    registerWarning$ BAD_CRC (fpFullname$ fiStoredName$ cfFileInfo compressed_file)
  return (cfCRC compressed_file == acrc)      -- ���������� True, ���� �� ��


----------------------------------------------------------------------------------------------------
---- ������ ����������� � ������ � ���� (������� "cw")                                          ----
----------------------------------------------------------------------------------------------------

-- |���������� ������� "cw" - ������ ����������� � ������ � ����
runCommentWrite command@Command{ cmd_filespecs   = filespecs
                               , cmd_arcname     = arcname
                               , opt_unParseFile = unParseFile
                               } = do
  doFinally uiDoneArchive2 $ do
  when (length filespecs /= 1) $
    registerError$ CMDLINE_GENERAL "command syntax is \"cw archive outfile\""
  let [outfile] = filespecs
  command <- (command.$ opt_cook_passwords) command ask_passwords  -- ����������� ������ � ������� � �������������
  printLineLn$ "Writing archive comment of "++arcname++" to "++outfile
  bracket (archiveReadFooter command arcname) (archiveClose.fst) $ \(_,footer) -> do
    unParseFile 'c' outfile (ftComment footer)
  return (0,0,0,0)


----------------------------------------------------------------------------------------------------
---- ������ �������� ������:                                                                    ----
----    - ��� ������������ (������� "l")                                                        ----
----    - ��� �������� ����-������ (������� "lb")                                               ----
----    - ��� ������ �������� (������� "v")                                                     ----
---------------------------------------------------------------------------------------------------

-- |���������� ������� ��������� �������� ������
runArchiveList pretestArchive
               command@Command{ cmd_arclist        = arclist
                              , cmd_arcname        = arcname
                              , opt_arc_basedir    = arc_basedir
                              , cmd_archive_filter = archive_filter
                              } = do
  command <- (command.$ opt_cook_passwords) command ask_passwords  -- ����������� ������ � ������� � �������������
  bracket (archiveReadInfo command arc_basedir "" archive_filter (pretestArchive command) arcname) (arcClose) $
      archiveList command (length arclist==1)

-- |������� ������
archiveList command @ Command{ cmd_name = cmd, cmd_arcname = arcname }
            show_empty
            arc @ ArchiveInfo{ arcDirectory = directory } = do
  let files = length directory
      bytes = sum$ map (fiSize.cfFileInfo) directory
  when (files>0 || show_empty) $ do
    doFinally uiDoneArchive2 $ do
    uiStartArchive command "" undefined -- �������� ������������ � ������ ��������� ���������� ������
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

-- |��� �����
filename = str2terminal . fpFullname . fiStoredName . cfFileInfo

-- |������������ ������� ������� �����
terse_list direntry = do
  let fi = cfFileInfo direntry
  timestr <- formatDateTime (fiTime fi)
  putStrLn$        timestr
         ++ " " ++ right_justify 11 (if (fiIsDir fi) then ("-dir-") else (show3$ fiSize fi))
         ++ " " ++ filename direntry

-- |������������ ��������� ������� �����
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
-- |������������� ����������� ������� �����
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

-- |��������������� CTime � ������ � �������� "%Y-%m-%d %H:%M:%S"
formatDateTime t  =  allocaBytes 100 $ \buf -> do
                       c_FormatDateTime buf 100 t
                       peekCString buf

foreign import ccall unsafe "Environment.h FormatDateTime"
  c_FormatDateTime :: CString -> CInt -> CTime -> IO ()

