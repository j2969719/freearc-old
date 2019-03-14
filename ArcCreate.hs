----------------------------------------------------------------------------------------------------
---- �������� � ��������� �������.                                                              ----
---- ����� �������������� ��� ������� �������� � ����������� �������:                           ----
----   create/a/f/m/u/ch/c/d/k/s/rr/j                                                           ----
---- ��������� runArchiveCreate ������ ������ ������, ������� ������ ������� � �������� �����, ----
----   ����� ��������� �������� �������� ��������� ��������� ������, ������ ������� ������,     ----
----   �������� � ������ ������ � �������� �����.                                               ----
---- ��� �������� ������� � ArcvProcessRead.hs � ArcvProcessCompress.hs                         ----
----------------------------------------------------------------------------------------------------
module ArcCreate where

import Prelude hiding (catch)
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.IORef
import Data.List
import System.Mem
import System.IO

import Utils
import Files
import Process
import Errors
import CompressionLib
import ByteStream
import FileInfo
import Compression           (compressorLimitCompressionMemoryUsage, encode_method)
import Cmdline
import CUI
import Statistics
import ArhiveStructure
import ArhiveDirectory
import ArhiveFileList
import ArcExtract
import ArcvProcessRead
import ArcvProcessExtract
import ArcvProcessCompress


-- |���������� ������� ��������/��������� ������
runArchiveCreate pretestArchive
                 writeRecoveryBlocks
                 command @ Command {             -- ������ � ����������� �������:
      cmd_name            = cmd                  --   �������� �������
    , cmd_arcname         = arcname              --   �������� �����, ������� ������������ ����������
    , cmd_archive_filter  = archive_filter       --   �������� ������ �������������� ������ �� �������
    , cmd_added_arcnames  = find_added_arcnames  --   �������������� ������� ������
    , cmd_diskfiles       = find_diskfiles       --   �����, ������� ����� �������� � �����
    , opt_arccmt_file     = arccmt_file          --   ����, �� �������� �������� ����� ����������� � ������
    , opt_data_compressor = compressor           --   �������� ������
    } = do
  -- ������� ������ tempfile, ���� �������� �� ������� � ������
  let limited_compressor =  compressor.$compressorLimitCompressionMemoryUsage (opt_limit_compression_memory command)
  -- ��� ������������� ������: find_files |> buffer 10_000 |> write_to_archive
  -- ������� "create" ������ ������ ����� � ����
  when (cmd=="create")$  do ignoreErrors$ fileRemove arcname
  -- �������� ������������ � ������ ��������� ������ � ��������� ������ ���������, ���� ����������
  uiStartArchive command (encode_method limited_compressor) limited_compressor
  command <- (command.$ opt_cook_passwords) command ask_passwords  -- ����������� ������ � ������� � �������������
  debugLog "Started"

  -- ����� ����������� ����� (� ����������� ������ - ��� ������� "j") �� �����
  diskfiles      <- find_diskfiles
  added_arcnames <- find_added_arcnames
  debugLog$ "Found "++show (length diskfiles     )++" files, "
                    ++show (length added_arcnames)++" archives"

  -- ��������� ��������� ���������� ������� �������, ������� �� ��������.
  -- �����, ���� �������� (�����������) ����� �������, � ���������� ������ ���� "������", ���� �� ������ ����� �����.
  -- ����� �����, ���� ����� �� ���� ������� �������� recovery info � ��������.
  let abort_on_locked_archive archive footer = do
          when (ftLocked footer) $
              registerError$ GENERAL_ERROR "can't modify archive locked with -k"
          pretestArchive command archive footer

  updatingArchive <- fileExist arcname
  main_archive    <- if updatingArchive
                       then archiveReadInfo command "" "" archive_filter abort_on_locked_archive arcname
                       else return phantomArc
  added_archives  <- foreach added_arcnames (archiveReadInfo command "" "" archive_filter (pretestArchive command))
  let input_archives = main_archive:added_archives      -- ������ ���� ������� �������
      closeInputArchives = for input_archives arcClose  -- �������� �������� ���� ������� �������

  -- �������� ����������� � ������
  arcComment <- getArcComment arccmt_file input_archives (opt_parseFile command)

  -- ������������� ������ ������ � �����
  sorted_diskfiles <- (opt_reorder command &&& reorder) (sort_files command diskfiles)
  debugLog$ "Sorted "++ show (length sorted_diskfiles)++" files"

  -- �������� ������ ������, ������� ������ ������� � �������� �����, ���� �����������
  -- ������ ������ �� ������������ ������, ������ ������ �� ����������� (�������� "j")
  -- � ���� �������, � ������ � �����. �������������� ��� ������ ���������� �� ����������.
  files_to_archive <- join_lists main_archive added_archives sorted_diskfiles command
  debugLog$ "Joined filelists"

  if null files_to_archive                    -- ���� �������� ����� �� �������� �� ������ �����
    then do registerWarning NOFILES           -- �� �������� �� ���� ������������
            closeInputArchives                --    ������� ������� ������
            ignoreErrors$ fileRemove arcname  --    ������� �����, ���� �� ����������� ����� ��������� (��������, � ������ ������� "arc d archive *")
            return (1,0,0,0)
    else do

  -- �������, ����������� �������������� (-d[f], -ac) ������ ���� ��� ������������ ���������� ������ �� ���� �� ������ warning'�
  postProcess_wrapper command $ \postProcess_processDir deleteFiles -> do

  -- ������ ��� �������� ����������� ������ ������� � ���������� ���������
  results <- ref (error "runArchiveCreate:results undefined")

  -- ��������� mtime ������ ��� ����� -tk
  old_arc_exist <- fileExist arcname
  arc_time <- if old_arc_exist  then getFileDateTime arcname  else return (error "runArchiveCreate:arc_time undefined")

  -- ��� ���������� ����� -tl �� ������ �������� ������ ���� ������������ � ����� ������ � ����� ����� ������ �� ���.
  --   ��� ����� � create_archive_structure_PROCESS ��������� ��������� `find_last_time`.
  --   �� �������� �� ������ ������ ������, ������������ � �����, � ��� ����������� ����� ������ �� ���.
  --   ���� ����� ����� ������������ ����� ����� ��������� ���������.
  last_time <- ref aMINIMAL_POSSIBLE_DATETIME
  let find_last_time dir  =  last_time .= (\time -> maximum$ time : map (fiTime.fwFileInfo) dir)
  let processDir dir      =  do when (opt_time_to_last command) (find_last_time dir)
                                postProcess_processDir dir  -- ������� ��������������� ���� ������ �������� ������ ������� ��������������� ������

  -- �������� ������������ � ������ �������� ������
  uiStartProcessing (map cfFileInfo files_to_archive)
  performGC   -- ��������� ����� ����� ���������� ��� ����� ������ ������ ��� ���������� ������ ������

  -- ������� �� ���������� ���������� ������ �� ��������� ���� � ���� �����, ��� ������ ��������� - ��������������� ���
  tempfile_wrapper arcname command deleteFiles pretestArchive $ \temp_arcname -> do
    bracketCtrlBreak (archiveCreateRW temp_arcname) (archiveClose) $ \archive -> do
      writeSFX (opt_sfx command) archive main_archive    -- ����� �������� ������ � ������ SFX-������
      -- �������� ������ - ������������������ ��������� ���������, ���������� ������ ���� �����:
      --   �������� ���������� ��������� ������ � ������ ������������� ������
      --   �������� �������� � ������ ������ ������ � �������� ����
      -- ����� ���� �������� ������� �������������� ����� (|>>>), ��� ��������� ������������ read-ahead ��������� ������
      let read_files          =  create_archive_structure_AND_read_files_PROCESS command archive main_archive files_to_archive processDir arcComment writeRecoveryBlocks results
          compress_AND_write  =  compress_AND_write_to_archive_PROCESS archive command
      backdoor <- newChan  -- ���� ����� ������������ ��� ����������� ���������� � ��������� ������ ������
      runP (read_files backdoor |>>> compress_AND_write backdoor)
    closeInputArchives                       -- ������� ������� ������
    --debugLog "Archive written"

  when (opt_keep_time command && old_arc_exist) $ do   -- ���� ������������ ����� -tk � ��� ���� ���������� ������������� ������
    setFileDateTime arcname arc_time                   --   �� ������������ mtime ������
  when (opt_time_to_last command) $ do                 -- ���� ������������ ����� -tl
    setFileDateTime arcname =<< val last_time          --   �� ���������� �����&���� ����������� ������ �� �����&���� ����������� ������ ������� ����� � ��
  renameArchiveAsSFX arcname command                   -- ����������� �����, ���� � ���� ��� �������� ��� �� ���� ����� SFX-������
  val results                                          -- ��������� ���������� ���������� �������


----------------------------------------------------------------------------------------------------
---- ������������� ���������� ����� ��� �������� ������ --------------------------------------------
----------------------------------------------------------------------------------------------------

-- |������� � ������� ��� ����������� ��������� ������
temparc_prefix = "$$temparc$$"
temparc_suffix = ".tmp"

-- |��������� `action` � ������ ���������� ����� � ����� ������������� ���
tempfile_wrapper filename command deleteFiles pretestArchive action  =  find 0 >>= doit
  where -- ����� ��������� ��� ��� ���������� �����
        find n = do let tempname = (opt_workdir command ||| takeDirectory filename)
                                   </> (temparc_prefix++show n++temparc_suffix)
                    found <- fileExist tempname
                    case found of
                        True  | n==999    -> registerError$ GENERAL_ERROR "can't create temporary file"
                              | otherwise -> find (n+1)
                        False             -> return tempname

        -- ��������� ��������, ��������� ��������� ��� �����, �������������� � ����� ������������� ������������� �����
        doit tempname = do old_file <- fileExist filename      -- �� ��������� ���������� ������������� ������?
                           handleCtrlBreak (ignoreErrors$ fileRemove tempname) $ do
                             -- ��������� ���������
                             action tempname
                             -- ���� ������� ����� "-t", �� ������������ ������ ��� ��������� �����
                             when (opt_test command) $ do
                                 test_archive tempname (opt_keep_broken command)
                           handleCtrlBreak (condPrintLineLn "n"$ "Keeping temporary archive "++tempname) $ do
                             -- ������� ��������������� �����, ���� ������������ ����� -d
                             deleteFiles
                             -- �������� ������ ����� �����
                             if old_file
                                 then fileRemove filename   -- ������ �� ���������, ��� ��� �� ��� ��� ����� ����
                                 else whenM (fileExist filename) $ do  -- ���� ���� � ������ ��������� ������ ������� �� ����� ���������, �� �������� �� ������
                                          registerError$ GENERAL_ERROR$ "output archive already exists, keeping temporary file "++tempname
                             fileRename tempname filename
                                 `catch` (\_-> do condPrintLineLn "n"$ "Copying temporary archive "++tempname++" to "++filename
                                                  fileCopy tempname filename; fileRemove tempname)
                           -- ���� ������� ����� "-t" � "-w", �� ��� ��� ������������ ������������� �����
                           when (opt_test command && opt_workdir command/="") $ do
                               test_archive filename (opt_keep_broken command || opt_delete_files command /= NO_DELETE)

        -- �������������� ����� � �����, ������ ���, ���� ��� ���� �������� ��������
        test_archive arcname keep_broken_archive = do
            w <- count_warnings $ do
                     testArchive command arcname pretestArchive
            -- ���������� ������ ������ ��� ���������� warning'��
            when (w/=0) $ do
                unless keep_broken_archive (ignoreErrors$ fileRemove arcname)
                registerError$ GENERAL_ERROR$ if keep_broken_archive
                                                 then "archive broken, keeping temporary file "++arcname
                                                 else "archive broken, deleting"


----------------------------------------------------------------------------------------------------
---- ��������������, ����������� ������ ���� ��������� ������ ������� ------------------------------
----------------------------------------------------------------------------------------------------

-- |��������������, ����������� ������ ���� ��������� ������ �������:
--    ������� ������� ��������������� �����, ���� ������ ����� -d[f]
--    �������� � ��� �������� Archive, ���� ������ ����� -ac
postProcess_wrapper command archiving = do
  doFinally uiDoneArchive2 $ do
  case (opt_delete_files command/=NO_DELETE || opt_clear_archive_bit command) of
      False -> archiving (\dir->return()) (return())  -- ���� ����� ������� �� �����, �� ������ �������� archiving

      _ -> do files2delete <- ref []   -- ������ ������, ������� �� ������ �������
              dirs2delete  <- ref []   -- ������ ���������, ������� �� ������ �������
              let -- ���� ��������� �� ������ ��������� ������ ������� ��������������� ������ � ���������,
                  -- � ��� ���������� �� ��� � ���, ����� ����� ��������� ��������� ��������� ������� ��
                  processDir filelist0  =  do
                      let filelist = map fwFileInfo$ filter isFILE_ON_DISK filelist0
                          (dirs,files)  =  partition fiIsDir filelist
                      evalList files  `seq`  (files2delete ++= files)
                      evalList dirs   `seq`  (dirs2delete  ++= dirs )
                  -- ������� ��������������� ����� � ��������
                  deleteFiles = when (opt_delete_files command /= NO_DELETE) $ do
                                    -- �������� ������
                                    condPrintLineLn "n"$ "Deleting successfully archived files"
                                    files <- val files2delete
                                    --print$ map diskName files   -- debugging tool :)
                                    for files $ \fi -> do
                                        whenM (check_that_file_was_not_changed fi) $ do
                                            ignoreErrors.fileRemove.fpFullname.fiDiskName$ fi
                                    -- �������� ���������
                                    when (opt_delete_files command == DEL_FILES_AND_DIRS) $ do
                                        dirs <- val dirs2delete
                                        for (reverse dirs) (ignoreErrors.dirRemove.fpFullname.fiDiskName)   -- �������� ������ ����������� � ������� ������, �� ���� ������������ ������� � ������ ������ ��������. ��� ��� reverse ��������� ������� ������� �������� ��������

              -- ��������� ���������, ������ ������� ��������������� ����� � ������ files2delete � dirs2delete.
              -- ������� ����� ����� ���������, ���� ������ ����� -d[f]
              result <- archiving processDir deleteFiles
              -- �������� ������� "������������" � ������� ����������� ������, ���� ������ ����� -ac
              when (opt_clear_archive_bit command) $ do
                  condPrintLineLn "n"$ "Clearing Archive attribute of successfully archived files"
                  files <- val files2delete
                  for files $ \fi -> do
                      whenM (check_that_file_was_not_changed fi) $ do
                          clearArchiveBit.fpFullname.fiDiskName$ fi
              return result

-- |���������, ��� ���� �� ��������� � ������� ���������
check_that_file_was_not_changed fi = do
    fileWithStatus "check_that_file_was_not_changed" (fpFullname.fiDiskName$ fi) $ \p_stat -> do
        size <- stat_size  p_stat
        time <- stat_mtime p_stat
        return (size==fiSize fi  &&  time==fiTime fi)


----------------------------------------------------------------------------------------------------
---- ��������������� �������� ----------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |�������� ����������� ��������� ������ �� �����, ���������� ������ -z,
-- ��� ������������� ������������ ������� �������, � ������� ��� �� �����
getArcComment arccmt_file input_archives parseFile = do
  case arccmt_file of           -- � ����������� �� �������� ����� "-z":
    ""   -> uiInputArcComment   --   ������ ����������� � stdin
    "-"  -> return ""           --   ������� ������ �����������
    "--" ->                     --   ����������� ������������ ����������� (�� ���������):
            do let comment = joinWith "\n\n" $ filter (not.null) $ map arcComment input_archives
               uiPrintArcComment comment
               return comment
    _    ->                     --   ��������� ����� ����������� �� ���������� �����:
            do parseFile 'c' arccmt_file >>== joinWith "\n"

-- |�������� SFX-������ � ������ ������������ ������
writeSFX sfxname archive old_archive = do
  let oldArchive = arcArchive old_archive
      oldSFXSize = ftSFXSize (arcFooter old_archive)
  case sfxname of                                      -- � ����������� �� �������� ����� "-sfx":
    "-"      -> return ()                              --   ������� ������ sfx-������
    "--"     -> unless (arcPhantom old_archive) $ do   --   ����������� sfx �� ��������� ������ (�� ���������)
                  archiveCopyData oldArchive 0 oldSFXSize archive
    filename -> bracket (archiveOpen sfxname              --   ��������� ������ sfx �� ���������� �����
                          `catch` (\e -> registerError$ GENERAL_ERROR$ "can't open SFX module "++sfxname))
                        (archiveClose)
                        (\sfxfile -> do size <- archiveGetSize sfxfile
                                        archiveCopyData sfxfile 0 size archive)

-- |������������� ����� � ������������ � ���, ��� �� �������� ��� �������� ������ �� ���� SFX-������
renameArchiveAsSFX arcname command = do
  let exe = aDEFAULT_SFX_EXTENSION
      arc = aDEFAULT_ARC_EXTENSION
  let changeExt = case (opt_noarcext command, opt_sfx command) of
        (True, _)     -> id                     -- �� ������ ����������, ���� ������� ����� --noarcext
        (_   , "--")  -> id                     --   ��� �� ������� ����� "-sfx"
        (_   , "-")   -> replaceAtEnd exe arc   -- ��� "-sfx-" ���������� �������� �� ".arc"
        _             -> replaceAtEnd arc exe   -- ��� "-sfx..." ���������� �������� �� ".exe"
  let newname = changeExt arcname
  when (newname/=arcname) $ do
    condPrintLineLn "n"$ "Renaming "++arcname++" to "++newname
    fileRename arcname newname

-- |�������������� ������ ��� ��������� �����, ����������� � ����� �� ����� `temp_arcname`
testArchive command temp_arcname pretestArchive = do
  let test_command = command{ cmd_name           = "t"           -- ���������
                            , cmd_arcname        = temp_arcname  -- � ��������� ������
                            , opt_arc_basedir    = ""            -- ��� �����
                            , opt_disk_basedir   = ""            -- ...
                            , cmd_archive_filter = const True    -- ...
                            , cmd_subcommand     = True          -- ��� ���������� (������������ ������ ��������)
                            , opt_pretest        = 1             -- �� ����� ��������� ������������ ����� �������������, �� recovery info ��������� ���� :)
                            }
  uiStartSubCommand command test_command
  results <- runArchiveExtract pretestArchive test_command
  uiDoneSubCommand command test_command [results]

