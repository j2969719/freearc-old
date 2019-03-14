----------------------------------------------------------------------------------------------------
---- �������� ������ ���������.                                                                 ----
---- �������� parseCmdline �� ������ Cmdline ��� ������� ��������� ������ � ��������� ������    ----
----   ���������� �������.                                                                      ----
---- ���� ������� ������ ���������� ��������� �������, �� find_archives ��������� �            ----
----   ��� ������� �� ���.                                                                      ----
---- ����� ������ ������� �������� � ���������� ����� �� ��������� �����:                       ----
---- * ��������� ������  � �������  runArchiveCreate   �� ������ ArcCreate   (������� a/f/m/u/j/d/ch/c/k/rr)
---- * ���������� ������         -  runArchiveExtract  -         ArcExtract  (������� t/e/x)    ----
---- * ��������� �������� ������ -  runArchiveList     -         ArcList     (������� l/v)      ----
---- * �������������� ������     -  runArchiveRecovery -         ArcRecover  (������� r)        ----
---- ������� ���������� ��������� � ������������ �� ���������� ���������� ����������� �������.  ----
----                                                                                            ----
---- ��� ��������� � ���� ������� ����� ��� �������� ���������� � �������:                      ----
----   ArhiveFileList   - ��� ������ �� �������� ������������ ������                            ----
----   ArhiveDirectory  - ��� ������/������ ���������� ������                                   ----
----   ArhiveStructure  - ��� ������ �� ���������� ������                                       ----
----   ByteStream       - ��� ����������� �������� ������ � ������������������ ������           ----
----   Compression      - ��� ������ ���������� ��������, ���������� � ���������� CRC           ----
----   Statistics       - ��� �������������� ������������ � ���� ����������� ����� :)           ----
----   Errors           - ��� ������������ � ��������� ������� � ������ � �������               ----
----   FileInfo         - ��� ������ ������ �� ����� � ��������� ���������� � ���               ----
----   Files            - ��� ���� �������� � ������� �� ����� � ������� ������                 ----
----   Process          - ��� ���������� ��������� �� ������������ ����������������� ��������   ----
----   Utils            - ��� ���� ��������� ��������������� �������                            ----
----------------------------------------------------------------------------------------------------
module Main where

import Prelude hiding (catch)
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.List
import System.Environment
import System.Mem
import System.IO

import Utils
import Process
import Errors
import Files
import FileInfo
import CUI
import Statistics
import Cmdline
import ArcCreate
import ArcExtract
import ArcRecover


-- |������� ������� ���������
main         =  (doMain =<< getArgs) >> shutdown "" aEXIT_CODE_SUCCESS
-- |����������� ������� ������� ��� ������������� �������
arc cmdline  =  doMain (words cmdline)

-- |���������� ��������� ������ � ����� ������ � ��������� ��
doMain args  =  do
  setUncaughtExceptionHandler handler
  setCtrlBreakHandler $ do          -- ���������� ��������� ^Break
  ensureCtrlBreak (resetConsoleTitle) $ do
  uiStartProgram                    -- ������� UI
  commands <- parseCmdline args     -- ���������� ��������� ������ � ������ ������ �� ����������
  mapM_ run commands                -- ��������� ������ ���������� �������
  uiDoneProgram                     -- ������� UI
 where
  handler ex  =  registerError$ GENERAL_ERROR$
      case ex of
        Deadlock    -> "no threads to run: infinite loop or deadlock?"
        ErrorCall s -> s
        other       -> showsPrec 0 other ""


-- |�������������� ������� � ���������� � ���������� ��� ������� ����������� ������
run command @ Command
                { cmd_name            = cmd
                , cmd_setup_command   = setup_command
                , opt_scan_subdirs    = scan_subdirs
                } = do
  performGC       -- ��������� ����� ����� ��������� ���������� ������
  setup_command   -- ��������� ���������, ����������� ����� ������� ���������� �������
  case (cmd) of
    "create" -> find_archives  False           run_add     command
    "a"      -> find_archives  False           run_add     command
    "f"      -> find_archives  False           run_add     command
    "m"      -> find_archives  False           run_add     command
    "mf"     -> find_archives  False           run_add     command
    "u"      -> find_archives  False           run_add     command
    "j"      -> find_archives  False           run_join    command
    "cw"     -> find_archives  False           run_cw      command
    "ch"     -> find_archives  scan_subdirs    run_copy    command
    "s"      -> find_archives  scan_subdirs    run_copy    command
    "c"      -> find_archives  scan_subdirs    run_copy    command
    "k"      -> find_archives  scan_subdirs    run_copy    command
    'r':'r':_-> find_archives  scan_subdirs    run_copy    command
    "r"      -> find_archives  scan_subdirs    run_recover command
    "d"      -> find_archives  scan_subdirs    run_delete  command
    "e"      -> find_archives  scan_subdirs    run_extract command
    "x"      -> find_archives  scan_subdirs    run_extract command
    "t"      -> find_archives  scan_subdirs    run_test    command
    "l"      -> find_archives  scan_subdirs    run_list    command
    "lb"     -> find_archives  scan_subdirs    run_list    command
    "v"      -> find_archives  scan_subdirs    run_list    command
    _ -> registerError$ UNKNOWN_CMD cmd aLL_COMMANDS


-- |���� ������, ���������� ��� ����� arcspec, � ��������� �������� ������� �� ������ �� ���
find_archives scan_subdirs   -- ������ ������ � � ������������?
              run_command    -- ���������, ������� ����� ��������� �� ������ ��������� ������
              command @ Command {cmd_arcspec = arcspec} = do
  uiStartCommand command   -- ������� ������ ���������� �������
  arclist <- if scan_subdirs || is_wildcard arcspec
               then find_files scan_subdirs arcspec >>== map diskName
               else return [arcspec]
  results <- foreach arclist $ \arcname -> do
    performGC   -- ��������� ����� ����� ��������� ���������� �������
    -- ���� ������� ����� -ad, �� �������� � �������� �������� �� ����� ��� ������ (��� ����������)
    let add_dir  =  opt_add_dir command  &&&  (</> takeBaseName arcname)
    run_command command { cmd_arcspec      = error "find_archives:cmd_arcspec undefined"  -- cmd_arcspec ��� ������ �� �����������
                        , cmd_arclist      = arclist
                        , cmd_arcname      = arcname
                        , opt_disk_basedir = add_dir (opt_disk_basedir command)
                        }
  uiDoneCommand command results   -- �������� � ����������� ���������� ������� ��� ����� ��������


-- |������� ���������� � �����: create, a, f, m, u
run_add cmd = do
  let diskfiles =  find_and_filter_files (cmd_filespecs cmd) find_criteria
      find_criteria  =  FileFind{ ff_ep             = opt_add_exclude_path cmd
                                , ff_scan_subdirs   = opt_scan_subdirs     cmd
                                , ff_include_dirs   = opt_include_dirs     cmd
                                , ff_no_nst_filters = opt_no_nst_filters   cmd
                                , ff_filter_f       = add_file_filter      cmd
                                , ff_arc_basedir    = opt_arc_basedir      cmd
                                , ff_disk_basedir   = opt_disk_basedir     cmd}
  runArchiveAdd cmd{ cmd_diskfiles      = diskfiles     -- �����, ������� ����� �������� � �����
                   , cmd_archive_filter = const True }  -- ������ ������ ������ �� ����������� �������


-- |������� ������� �������: j
run_join cmd @ Command { cmd_filespecs = filespecs
                       , opt_noarcext  = noarcext
                       } = do
  let arcspecs  =  map (addArcExtension noarcext) filespecs   -- ������� � ������ ���������� �� ��������� (".arc")
      arcnames  =  map diskName ==<< find_and_filter_files arcspecs find_criteria
      find_criteria  =  FileFind{ ff_ep             = opt_add_exclude_path cmd
                                , ff_scan_subdirs   = opt_scan_subdirs     cmd
                                , ff_include_dirs   = Just False
                                , ff_no_nst_filters = opt_no_nst_filters   cmd
                                , ff_filter_f       = add_file_filter      cmd
                                , ff_arc_basedir    = ""
                                , ff_disk_basedir   = opt_disk_basedir     cmd}
  runArchiveAdd cmd{ cmd_added_arcnames = arcnames      -- �������������� ������� ������
                   , cmd_archive_filter = const True }  -- ������ ������ ������ �� ����������� �������


-- |������� ����������� ������ � ��������� ���������: ch, c, k. s, rr
run_copy    = runArchiveAdd                    . setArcFilter full_file_filter
-- |������� �������� �� ������: d
run_delete  = runArchiveAdd                    . setArcFilter ((not.).full_file_filter)
-- |������� ���������� �� ������: e, x
run_extract = runArchiveExtract pretestArchive . setArcFilter (test_dirs extract_file_filter)
-- |������� ������������ ������: t
run_test    = runArchiveExtract pretestArchive . setArcFilter (test_dirs full_file_filter)
-- |������� ��������� �������� ������: l, v
run_list    = runArchiveList pretestArchive    . setArcFilter (test_dirs full_file_filter)
-- |������� ������ ��������� ����������� � ����: cw
run_cw      = runCommentWrite
-- |������� �������������� ������: r
run_recover = runArchiveRecovery

-- |Just shortcut
runArchiveAdd  =  runArchiveCreate pretestArchive writeRecoveryBlocks

{-# NOINLINE find_archives #-}
{-# NOINLINE run_add #-}
{-# NOINLINE run_join #-}
{-# NOINLINE run_copy #-}
{-# NOINLINE run_delete #-}
{-# NOINLINE run_extract #-}
{-# NOINLINE run_test #-}
{-# NOINLINE run_list #-}


----------------------------------------------------------------------------------------------------
---- �������� ������ ������, ���������� ���������, ��� ��������� ����� ������ ----------------------
----------------------------------------------------------------------------------------------------

-- |���������� � cmd �������� ������ �� ������ �������������� ������
setArcFilter filter cmd  =  cmd {cmd_archive_filter = filter cmd}

-- |�������� ����� � ������������ � �������� opt_file_filter, �� �����������
-- �������������� ���� �������� ������� � ��������� ������, ����������� ��� ���������
add_file_filter cmd      =  all_functions [opt_file_filter cmd, not.overwrite_f cmd]

-- |�������� ����� � ������������ � �������� full_file_filter, �� �����������
-- �������������� ���� �������� ������� � ��������� ������, ����������� ��� ���������
extract_file_filter cmd  =  all_functions [full_file_filter cmd, not.overwrite_f cmd]

-- |�������� ����� ������, ����� ������� ������� � ��������� ������,
-- ��������������� ������� opt_file_filter
full_file_filter cmd  =  all_functions
                           [  match_filespecs (opt_match_with cmd) (cmd_filespecs cmd) . fiFilteredName
                           ,  opt_file_filter cmd
                           ]

-- |�������� �������������� ������ � ��������� �����, ����������� ��� ���������,
-- � ����� �����, ������� ����� �� ������������ ��� ����������
overwrite_f cmd  =  in_arclist_or_temparc . fiDiskName
  where in_arclist_or_temparc filename =
            fpFullname filename `elem` cmd_arclist cmd
            || all_functions [(temparc_prefix `isPrefixOf`), (temparc_suffix `isSuffixOf`)]
                             (fpBasename filename)

-- |�������� � ������ ������ ������ `filter_f` ����� ��������� � ������������ � ������� ������� `cmd`
test_dirs filter_f cmd fi  =  if fiIsDir fi
                                then opt_x_include_dirs cmd
                                else filter_f cmd fi

