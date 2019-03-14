{-# OPTIONS_GHC -cpp #-}
----------------------------------------------------------------------------------------------------
---- FreeArc archive manager: Extract/ArcInfo/Settings dialogs                                ------
----------------------------------------------------------------------------------------------------
module FileManDialogs where

import Prelude hiding (catch)
import Control.Concurrent
import Control.OldException
import Control.Monad
import Control.Monad.Fix
import Data.Char
import Data.IORef
import Data.List
import Data.Maybe
import System.IO.Unsafe
import System.Cmd
#if defined(FREEARC_WIN)
import System.Win32
#endif

import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as New

import Utils
import Errors
import Files
import FileInfo
import Charsets
import Compression
import Encryption
import Options
import UIBase
import UI
import ArhiveStructure
import Arhive7zLib
import ArcExtract
import FileManPanel
import FileManUtils

----------------------------------------------------------------------------------------------------
---- ������ ���������� ������ ----------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

extractDialog fm' exec winoptions cmd arcnames arcdir files = do
  fm <- val fm'
  title <- i18n$ case (cmd, files, arcnames) of
                   ("t", [],     [])        -> "0157 Test all archives"
                   ("t", [],     [arcname]) -> "0152 Test %3"
                   ("t", [file], [arcname]) -> "0153 Test %1 from %3"
                   ("t", files,  [arcname]) -> "0154 Test %2 files from %3"
                   ("t", files,  arcnames)  -> "0155 Test %4 archives"
                   (_,   [],     [])        -> "0158 Extract all archives"
                   (_,   [],     [arcname]) -> "0024 Extract files from %3"
                   (_,   [file], [arcname]) -> "0025 Extract %1 from %3"
                   (_,   files,  [arcname]) -> "0026 Extract %2 files from %3"
                   (_,   files,  arcnames)  -> "0027 Extract files from %4 archives"
  let wintitle  =  formatn title [head files, show3$ length files, takeFileName$ head arcnames, show3$ length arcnames]
  -- �������� ������ �� ������������ �������� OK/Cancel
  fmDialog fm' wintitle winoptions $ \(dialog,okButton) -> do
    upbox <- dialogGetUpper dialog

    ; outFrame <- frameNew
    ; boxPackStart upbox outFrame           PackNatural 5         `on_` cmd/="t"
    ;   vbox <- vBoxNew False 0
    ;   set outFrame [containerChild := vbox, containerBorderWidth := 5]
    (hbox, _, dir) <- fmFileBox fm' dialog
                                "dir" FileChooserActionSelectFolder
                         (label "0004 Output directory:")
                                "0021 Select output directory"
                                aANYFILE_FILTER
                                (const$ return True)
                                (fmCanonicalizeDiskPath fm')
    ; boxPackStart vbox hbox                      PackNatural 0
    addDirButton <- checkBox "0014 Append archive name to the output directory"
    ; boxPackStart vbox (widget addDirButton)     PackNatural 0
    openOutDirButton <- checkBox "0468 Open output directory in Explorer"
    ; boxPackStart vbox (widget openOutDirButton) PackNatural 0

    overwrite <- radioFrame "0005 Overwrite mode"
                            [ "0001 Ask before overwrite",
                              "0002 Overwrite without prompt",
                              "0003 Update old files",
                              "0051 Skip existing files" ]
    ; boxPackStart upbox (widget overwrite) PackNatural 5         `on_` cmd/="t"

    (decryption, decryptionOnOK) <- decryptionBox fm' dialog   -- ��������� �����������
    ; boxPackStart upbox decryption           PackNatural 5

    keepBrokenButton <- fmCheckButtonWithHistory fm' "KeepBroken" False "0425 Keep broken extracted files"
    ; boxPackStart upbox (widget keepBrokenButton) PackNatural 0  `on_` cmd/="t"

    shutdown <- checkBox shutdown_msg
    ; boxPackStart upbox (widget shutdown)    PackNatural 0

    (hbox, options, optionsStr)  <- fmCheckedEntryWithHistory fm' "xoptions" "0072 Additional options:"
    ; boxPackStart upbox hbox                 PackNatural 0


    -- ��������� �������� ������� � �������� �� ���������
    case arcnames of
      [arcname] -> do dir =:: fmCanonicalizeDiskPath fm' (takeBaseName arcname)
      _         -> do dir =:: fmCanonicalizeDiskPath fm' "."; addDirButton=:True


    widgetShowAll upbox
    showTestDialog <- fmGetHistoryBool fm' "ShowTestDialog" False
    choice <- if cmd/="t" || showTestDialog
                then fmDialogRun fm' dialog (if cmd/="t" then "ExtractDialog" else "TestDialog")
                else return ResponseOk
    when (choice `elem` [ResponseOk, aResponseDetach]) $ do
      overwriteOption    <- val overwrite
      dir'               <- val dir;         saveHistory dir
      isAddDir           <- val addDirButton
      isOpenOutDir       <- val openOutDirButton
      decryptionOptions  <- decryptionOnOK
      keepBroken         <- val keepBrokenButton
      shutdown'          <- val shutdown
      optionsEnabled     <- val options
      ; optionsStr'        <- val optionsStr;  saveHistory optionsStr  `on_`  optionsEnabled
      let outdir = dir' .$ (isAddDir &&& length(arcnames)==1 &&& (</> takeBaseName(head arcnames)))
      exec (choice == aResponseDetach)                                               -- ��������� ������� � ��������� ����� FreeArc?
           (\ok -> when isOpenOutDir (runFile (outdir++[pathSeparator]) "" False))   -- post-operation action: open outdir in Explorer
           ((arcnames ||| ["*"]) .$map (\arcname ->
                [cmd]++
                (cmd/="t" &&& (
                    ["-dp"++clear dir']++
                    (isAddDir &&& ["-ad"])++
                    (arcdir &&& files &&& ["-ap"++clear arcdir])++
                    (keepBroken &&& ["-kb"])++
                    (overwriteOption  `select`  ",-o+,-u -o+,-o-")))++
                decryptionOptions++
                ["--fullnames"]++
                ["--noarcext"]++
                (shutdown'        &&&  ["--shutdown"])++
                (optionsEnabled   &&&  words (clear optionsStr'))++
                ["--", clear arcname]++files))


----------------------------------------------------------------------------------------------------
---- ������ ���������� �� ������ -------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

arcinfoDialog fm' exec mode arcnames arcdir files = do
  handle (\e -> fmErrorMsg fm' "0013 There are no archives selected!") $ do
  fm <- val fm'
  let arcname = head arcnames
  fm_arc <- case () of _ | isFM_Archive fm -> return (subfm fm)
                         | otherwise       -> with' (newFMArc fm' arcname "") (return) (\_ -> closeFMArc fm')
  let archive    = subfm_archive fm_arc
      footer     = arcFooter archive
      dataBlocks = arcDataBlocks archive   -- ������ �����-������
      dirs_and_files = [("0173 Directories:", show3$ ftDirs$  subfm_filetree fm_arc)
                       ,("0088 Files:",       show3$ ftFiles$ subfm_filetree fm_arc)]

  title <- i18n"0085 All about %1"
  let wintitle  =  format title (takeFileName arcname)
  -- �������� ������ �� ������������ �������� OK/Cancel
  fmDialog fm' wintitle [] $ \(dialog,okButton) -> do
    (nb,newPage) <- startNotebook dialog
------ ������� �������� ----------------------------------------------------------------------------
    vbox <- newPage "0174 Main";  let pack n makeControl = do control <- makeControl
                                                              boxPackStart vbox control PackNatural n
    tables <- arcGetTechinfo archive dirs_and_files
    for (zip [10,0,10,0,10] tables) $ \(n,table) -> do
      pack n (twoColumnTable table)

------ �������� � ���������� �����-������ ----------------------------------------------------------
    vBox <- newPage "0449 Solid blocks"
    let columnTitles = ["0450 Position", "0451 Size", "0452 Compressed", "0453 Files", "0454 Method"]
        n = map i18no columnTitles
    s <- i18ns columnTitles
    let compressor = join_compressor.blCompressor
    (listUI, listView, listModel, listSelection, columns, onColumnTitleClicked) <-
        createListView compressor [(n!!0, s!!0, (show3.blPos),      [New.cellTextEditable := True, cellXAlign := 1]),
                                   (n!!1, s!!1, (show3.blOrigSize), [New.cellTextEditable := True, cellXAlign := 1]),
                                   (n!!2, s!!2, (show3.blCompSize), [New.cellTextEditable := True, cellXAlign := 1]),
                                   (n!!3, s!!3, (show3.blFiles),    [New.cellTextEditable := True, cellXAlign := 1]),
                                   (n!!4, s!!4, (compressor),       [New.cellTextEditable := True])]
    boxPackStart vBox listUI PackGrow 0
    changeList listModel listSelection dataBlocks
    -- ��� �������� ������� �������� ������� � ������ �������, ��� �������� ����������� ��
    restoreColumnsOrderAndWidths fm' "SolidBlocks" listView columns


------ �������� ����������� ������ -----------------------------------------------------------------
    vbox <- newPage "0199 Comment"

    comment <- scrollableTextView (ftComment footer) []
    boxPackStart vbox (widget comment) PackGrow 0

    widgetShowAll dialog
    notebookSetCurrentPage nb 1    `on_` mode==CommentMode
    choice <- fmDialogRun fm' dialog "ArcInfoDialog"
    saveColumnsOrderAndWidths fm' "SolidBlocks" listView columns
    when (choice==ResponseOk) $ do
      newComment <- val comment
      when (newComment /= ftComment footer) $ do
        exec False doNothing [["ch"
                              ,"--noarcext"
                              ,newComment &&& ("--archive-comment="++newComment)
                                          ||| "-z-"
                              ,"--"
                              ,arcname]]


----------------------------------------------------------------------------------------------------
---- ������ �������� ��������� ---------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

settingsDialog fm' = do
  fm <- val fm'
  fmDialog fm' "0067 Settings" [] $ \(dialog,okButton) -> do
    (nb,newPage) <- startNotebook dialog
------ ������� �������� ----------------------------------------------------------------------
    vbox <- newPage "0174 Main";  let pack x = boxPackStart vbox x PackNatural 1
    aboutLabel         <- labelNewWithMnemonic aARC_HEADER_WITH_DATE
    langLabel          <- label "0068 Language:"
    langComboBox       <- New.comboBoxNewText
    editLangButton     <- button "0069 Edit"
    convertLangButton  <- button "0070 Import"
    -- �������
    (logfileBox, _, logfile) <- fmFileBox fm' dialog
                                          "logfile" FileChooserActionSave
                                   (label "0166 Logfile:")
                                          "0167 Select logfile"
                                          aANYFILE_FILTER
                                          (const$ return True)
                                          (fmCanonicalizeDiskPath fm')
    ; viewLogfileButton <- button "0292 View"
    -- ������� ��� ��������� ������
    (tempdirBox, _, tempdir) <- fmFileBox fm' dialog
                                          "tempdir" FileChooserActionSelectFolder
                                   (label "0447 Temporary directory:")
                                          "0448 Select directory for temporary files"
                                          aANYFILE_FILTER
                                          (const$ return True)
                                          (fmCanonicalizeDiskPath fm')
    -- ������
    checkNewsButton <- fmCheckButtonWithHistory fm' "CheckNews"       True "0370 Watch for new versions via Internet"
    notes           <- label . joinWith "\n" =<<
      i18ns["0168 You should restart "++aFreeArc++" in order for a language settings to take effect.",
            "0169 Passwords need to be entered again after restart."]

-----------------------------------------------------------------------------------------------
    -- ���������� � ������� ����� �����������
    langTable <- tableNew 2 2 False
    let dataset = [("0170 Full name:", "0000 English"), ("0171 Copyright:", "0159 ")]
    labels <- foreach [0..1] $ \y -> do
      -- ������ �������
      label1 <- labelNew Nothing;  let x=0
      tableAttach langTable label1 (x+0) (x+1) y (y+1) [Fill] [Fill] 5 5
      miscSetAlignment label1 0 0
      -- ������ �������
      label2 <- labelNew Nothing
      tableAttach langTable label2 (x+1) (x+2) y (y+1) [Expand, Fill] [Expand, Fill] 5 5
      set label2 [labelSelectable := True]
      miscSetAlignment label2 0 0
      return (label1, label2)
    --
    let showLang i18n = do
          for (zip labels dataset) $ \((l1,l2),(s1,s2)) -> do
            labelSetTextWithMnemonic l1      =<< i18n s1
            labelSetMarkup           l2.bold =<< i18n s2
    --
    showLang i18n

    -- ���� �������� �����������
    langFile <- fmGetHistory1 fm' aINITAG_LANGUAGE ""

    -- ��������� ������ ������ ������� ������ � �������� arc.languages � ������� �������� ����
    langDir   <- findDir libraryFilePlaces aLANG_DIR
    langFiles <- langDir &&& (dir_list langDir >>== map baseName >>== sort >>== filter (match "arc.*.txt"))
    -- ��������� ����� � 5 ��������, � ����������� �� ��������
    let cols = 5
        rows = (length langFiles) `divRoundUp` cols;  add = rows*cols - length langFiles
        sortOnColumn x  =  r*cols+c  where (c,r) = x `divMod` rows  -- ����������� �� ������������ ������� � ����������
    ;   langFiles <- return$ map snd $ sort $ zip (map sortOnColumn [0..]) (langFiles ++ replicate add "")
    --
    for langFiles (New.comboBoxAppendText langComboBox . mapHead toUpper . replace '_' ' ' . dropEnd 4 . drop 4)
    langComboBox  `New.comboBoxSetWrapWidth`  cols
    whenJust_ (elemIndex (takeFileName langFile) langFiles)
              (New.comboBoxSetActive langComboBox)

    -- ���������� ���� �����������, ��������������� ���������� � ���������� �����
    let getCurrentLangFile = do
          lang <- New.comboBoxGetActive langComboBox
          case lang of
            -1   -> return ""
            lang -> myCanonicalizePath (langDir </> (langFiles !! lang))

    -- ��� ������ ������� ����� ����������� ������� ���������� � ��
    on langComboBox changed $ do
      choice <- New.comboBoxGetActive langComboBox
      when (choice /= -1) $ do
        langFile   <- getCurrentLangFile
        localeInfo <- parseLocaleFile langFile
        showLang (i18n_general (return localeInfo) .>>== fst)

    -- �������������� �������� ����� �����������/��������
    editLangButton    `onClick` (runEditCommand =<< getCurrentLangFile)
    viewLogfileButton `onClick` (runViewCommand =<< val logfile)

    ; langFrame <- frameNew
    ;   vbox1 <- vBoxNew False 0
    ;   set langFrame [containerChild := vbox1, containerBorderWidth := 5]
    ;     langbox <- hBoxNew False 0
    boxPackStart langbox    (widget  langLabel)          PackNatural 0
    boxPackStart langbox             langComboBox        PackGrow    5
    boxPackStart langbox    (widget  editLangButton)     PackNatural 5
    --boxPackStart langbox    (widget  convertLangButton)  PackNatural 5
    boxPackStart vbox1               langbox             PackNatural 5
    boxPackStart vbox1               langTable           PackNatural 5
    boxPackStart logfileBox (widget  viewLogfileButton)  PackNatural 5
    boxPackStart vbox                aboutLabel          PackNatural 5
    boxPackStart vbox                langFrame           PackNatural 5
    boxPackStart vbox                logfileBox          PackNatural 5
    boxPackStart vbox                tempdirBox          PackNatural 5
    boxPackStart vbox       (widget  checkNewsButton)    PackNatural 5
    boxPackStart vbox       (widget  notes)              PackNatural 5

------ �������� �������� ���������� -----------------------------------------------------------
    vbox <- newPage "0466 Interface";  let pack x = boxPackStart vbox x PackNatural 1

    toolbarTextButton       <- fmCheckButtonWithHistory  fm' "ToolbarCaptions" True  "0361 Add captions to toolbar buttons";            pack (widget toolbarTextButton)
    showTestDialogButton    <- fmCheckButtonWithHistory  fm' "ShowTestDialog"  False "0469 Show \"Test archive\" dialog";               pack (widget showTestDialogButton)
    targzButton             <- fmCheckButtonWithHistory  fm' "QuickOpenTarGz"  True  "0485 Open .tar.gz-like archives in single step";  pack (widget targzButton)
    (hbox, extract_all_for) <- fmLabeledEntryWithHistory fm' "ExtractAllFor"         "0467 Unpack whole archive when running:";         pack hbox

------ �������� ���������� � Explorer ---------------------------------------------------------
#if defined(FREEARC_WIN)
    vbox <- newPage "0421 Explorer integration";  let pack x = boxPackStart vbox x PackNatural 1

    associateArcButton <- fmCheckButtonWithHistory fm' "Settings.Associate"             False  ("0172 Associate "++aFreeArc++" with .arc files")
    associateZipButton <- fmCheckButtonWithHistory fm' "Settings.AssociateNonArc"       False  ("0471 Associate "++aFreeArc++" with other archives")
    contextMenuButton  <- fmCheckButtonWithHistory fm' "Settings.ContextMenu"           False   "0422 Enable context menu in Explorer"
    cascadedButton     <- fmCheckButtonWithHistory fm' "Settings.ContextMenu.Cascaded"  True    "0423 Make it cascaded"
    empty              <- label ""
    mapM_ (pack.widget) [associateArcButton, associateZipButton, empty]

    frame <- frameNew;  frameSetLabelWidget frame (widget contextMenuButton)
    boxPackStart vbox frame PackGrow 1

    hbox <- hBoxNew False 0;  containerAdd frame hbox

    let show_or_hide = widgetSetSensitivity hbox =<< val contextMenuButton
    show_or_hide
    show_or_hide .$ setOnUpdate contextMenuButton
    oldContextMenu <- val contextMenuButton

    vbox <- vBoxNew False 0;  boxPackStart hbox vbox PackGrow 10
    let pack x = boxPackStart vbox x PackNatural 1

    empty <- label ""
    notes <- label =<< i18n"0424 Enable individual commands:"
    mapM_ (pack.widget) [cascadedButton, empty, notes]

    -- Put all subsequent checkboxes to scrolled window
    scrolledWindow <- scrolledWindowNew Nothing Nothing
    boxPackStart vbox scrolledWindow PackGrow 1
    vbox <- vBoxNew False 0
    scrolledWindowAddWithViewport scrolledWindow vbox
    scrolledWindowSetPolicy scrolledWindow PolicyAutomatic PolicyAutomatic
    Just viewport <- binGetChild scrolledWindow
    viewportSetShadowType (castToViewport viewport) ShadowNone
    let pack x = boxPackStart vbox x PackNatural 1

    let makeButton ("",_,_)             = do pack =<< hSeparatorNew; return []
        makeButton (cmdname,itext,imsg) = do
          button <- fmCheckButtonWithHistory fm' ("Settings.ContextMenu.Command."++cmdname) True imsg
          pack (widget button)
          return [button]

    commands <- getExplorerCommands >>= concatMapM makeButton
#endif

------ �������� ������ ------------------------------------------------------------------------
    (_, saveCompressionHistories) <- compressionPage fm' =<< newPage "0106 Compression"

------ �������� ���������� --------------------------------------------------------------------
    (_, saveEncryptionHistories)  <-  encryptionPage fm' dialog okButton =<< newPage "0119 Encryption"

------ �������� ���������� � ������� ----------------------------------------------------------
    vbox <- newPage "0388 Info";  let pack n makeControl = do control <- makeControl
                                                              boxPackStart vbox control PackNatural n

    maxBlock <- getMaxMemToAlloc
    pack 10 $twoColumnTable [("0461 Largest address space block:", showMem (maxBlock `roundDown` mb))]


-----------------------------------------------------------------------------------------------
    widgetShowAll dialog
    choice <- fmDialogRun fm' dialog "SettingsDialog"
    when (choice==ResponseOk) $ do
      -- ��������� ��������� � keyfile � INI-����, ������ - � ����. ����������
      langFile <- getCurrentLangFile
      fmReplaceHistory fm' aINITAG_LANGUAGE (takeFileName langFile)
      loadTranslation
      saveHistory `mapM_` [logfile, tempdir, extract_all_for]
      saveHistory `mapM_` [checkNewsButton, toolbarTextButton, showTestDialogButton, targzButton]
      saveCompressionHistories "partial"
      saveEncryptionHistories  "partial" ""
#if defined(FREEARC_WIN)
      saveHistory `mapM_` ([associateArcButton, associateZipButton, contextMenuButton, cascadedButton] ++ commands)
      registerShellExtensions' (fm_history fm) (Just oldContextMenu)
#endif
      return ()


----------------------------------------------------------------------------------------------------
---- (��)����������� shell extension � ���������� FreeArc � ��������� ������� ----------------------
----------------------------------------------------------------------------------------------------

#if defined(FREEARC_WIN)
-- |�����������/�������� ����������� ����� ��������� ������
changeRegisterShellExtensions action = do
  hf' <- openHistoryFile
  when (action==["--unregister"]) $ do
    hfReplaceHistoryBool hf' "Settings.Associate"        False
    hfReplaceHistoryBool hf' "Settings.AssociateNonArc"  False
    hfReplaceHistoryBool hf' "Settings.ContextMenu"      False
  registerShellExtensions' hf' Nothing

-- |��������� �������� ���������� � Explorer
registerShellExtensions' hf' oldContextMenu = do
  hfCacheConfigFile hf' $ do
  associateArc <- hfGetHistoryBool hf' "Settings.Associate"            True
  associateZip <- hfGetHistoryBool hf' "Settings.AssociateNonArc"      True
  contextMenu  <- hfGetHistoryBool hf' "Settings.ContextMenu"          True

  exe <- getExeName                                -- Name of FreeArc.exe file
  let ico   =  exe `replaceExtension` ".ico"       -- Name of FreeArc.ico file
      dir   =  exe.$takeDirectory                  -- FreeArc.exe directory
      shext =  dir </> "ArcShellExt"               -- Shell extension directory
      empty =  dir </> "empty.arc"                 -- Name of empty archive file
      version  = aARC_VERSION_WITH_DATE
      reg   = registryGetStr hKEY_CLASSES_ROOT
  oldAssociateArc <- and `fmap` sequence [(Just exe==)                `fmap` reg (aFreeArc++".arc") "Owner"   -- ���������� � .arc ��������?
                                         ,(Just (aFreeArc++".arc")==) `fmap` reg (".arc") ""]
  oldAssociateZip <- and `fmap` sequence [(Just exe==)                `fmap` reg (aFreeArc++".zip") "Owner"   -- ���������� � .zip ��������?
                                         ,(Just (aFreeArc++".zip")==) `fmap` reg (".zip") ""]
  old_shext   <- hfGetHistory1 hf' "Settings.ContextMenu.Directory" ""
  hfReplaceHistory             hf' "Settings.ContextMenu.Directory" shext
  old_version <- hfGetHistory1 hf' "Settings.ContextMenu.Version"   ""
  hfReplaceHistory             hf' "Settings.ContextMenu.Version"   version

  -- UAC-compatibility: instead of modifying registry directly, we are calling external executables that have admin privileges
  reglist32 <- newList;  reglist64 <- newList
  let add_to_list x  =  for [reglist32, reglist64] (<<=x)
  let register      key name value  =  mapM_ add_to_list ["RegistryCreateKey", key, name, value]
      regDeleteTree key             =  mapM_ add_to_list ["RegistryDeleteTree", key]
      runDll32      dll func        =  mapM_ (reglist32 <<=) ["RunDll", dll, func]
      runDll64      dll func        =  mapM_ (reglist64 <<=) ["RunDll", dll, func]

  -- (Un)registering ArcShellExt dlls - performed only if settings were changed
  let dll_register mode = when ((oldContextMenu,old_shext,old_version) /= (Just contextMenu,shext,version)) $ do
                            runDll32 (shext </> "ArcShellExt.dll")    (if mode=="" then "DllRegisterServer" else "DllUnregisterServer")
                            runDll64 (shext </> "ArcShellExt-64.dll") (if mode=="" then "DllRegisterServer" else "DllUnregisterServer")

  -- First, unregister any old version if required
  dll_register "/u"
  when (associateArc/=oldAssociateArc  ||  oldAssociateArc && version/=old_version) $ do
    regDeleteTree ("*\\shell\\"++aFreeArc)               -- these registry entries were used
    regDeleteTree ("Directory\\shell\\"++aFreeArc)       --   for Explorer integration in FreeArc 0.50
    for [aFreeArcExt] $ \ext -> do
      regDeleteTree ("."++ext)
      regDeleteTree (aFreeArc++"."++ext)
  when (associateZip/=oldAssociateZip  ||  associateZip && version/=old_version) $ do
    for (words other_archive_extensions) $ \ext -> do
      regDeleteTree ("."++ext)
      regDeleteTree (aFreeArc++"."++ext)

  -- Second, associate FreeArc with .arc and archives if it was not associated before or program was updated
  for ((associateArc  &&&  not (oldAssociateArc && version==old_version)  &&&  [aFreeArcExt]) ++
       (associateZip  &&&  not (oldAssociateZip && version==old_version)  &&&  words other_archive_extensions)) $ \ext -> do
    register  (aFreeArc++"."++ext)                            ""          ((if ext==aFreeArcExt then aFreeArc else map toUpper ext)++" archive")
    register  (aFreeArc++"."++ext)                            "Owner"     exe   -- used to check in oldAssociateArc/Zip that exension is already associated with this program installation
    register  (aFreeArc++"."++ext++"\\DefaultIcon")           ""          (ico++",0")
    register  (aFreeArc++"."++ext++"\\shell")                 ""          ("open")
    register  (aFreeArc++"."++ext++"\\shell\\open\\command")  ""          ("\""++exe++"\" \"%1\"")
    register  ("."++ext)                                      ""          (aFreeArc++"."++ext)
  --register  (".arc\\ShellNew")                              "FileName"  (empty)   -- disabled because we don't yet support Drag&Drop

  -- Third, add items to Explorer's right-click menu and register DLL
  when contextMenu $ do
    writeShellExtScript hf'
    dll_register ""

  -- Run external executables with admin privileges that make actual changes to the registry
  list32 <- listVal reglist32;  list64 <- listVal reglist64
  list32 &&& runCommand (unparseCommand$ (shext</>("Manager of FreeArc integration settings.exe"         )):"0.60":list32) "." True
  list64 &&& runCommand (unparseCommand$ (shext</>("Manager of FreeArc integration settings (64-bit).exe")):"0.60":list64) "." True

#else
changeRegisterShellExtensions = doNothing
#endif


----------------------------------------------------------------------------------------------------
---- �������� ������ -------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

compressionPage fm' vbox = do
  let pack x = boxPackStart vbox x PackNatural 1
  -- �������� ������.
  (hbox, cmethod) <- fmLabeledEntryWithHistory fm' "compression" "0175 Compression profile:";  pack hbox
  ; save <- button "0178 Save";  boxPackStart hbox (widget save) PackNatural 5

  -- ��������� ��������� ������.
  hbox <- hBoxNew False 0;  pack hbox
  ; method     <- radioFrame "0107 Compression level" levels;    boxPackStart hbox (widget method)  PackNatural 0
  ; methodText <- labelNew Nothing;                              boxPackStart hbox methodText       PackGrow 0
  xMethod    <- checkBox "0113 Fast, low-memory decompression";  pack (widget xMethod)
  autodetect <- checkBox "0176 Filetype auto-detection"       ;  pack (widget autodetect)

  -- ��������� ������� �����-�����
  vbox1 <- vBoxNew False 0;  let pack1 x = boxPackStart vbox1 x PackNatural 1
  ; (hbox, solidBytesOn, solidBytes) <- fmCheckedEntryWithHistory fm' "bytes" "0138 Bytes, no more than:";  pack1 hbox
  ; (hbox, solidFilesOn, solidFiles) <- fmCheckedEntryWithHistory fm' "files" "0139 Files, no more than:";  pack1 hbox
  ; solidByExtension                 <- checkBox                              "0140 Split by extension" ;  pack1 (widget solidByExtension)
  solidBlocksFrame <- frameNew;  pack solidBlocksFrame;  s <- i18n"0177 Limit solid blocks"
  set solidBlocksFrame [containerChild := vbox1, frameLabel := s, containerBorderWidth := 5]

  -- ������������� �����
  let m=4; x=False
  method  =: (6-m) .$ clipTo 0 5
  xMethod =: x
  autodetect =: True

  -- ������������ �������� ������������� ���������� ������ ������ � ��������� ��� ��� ���������� ��������
  let parsePhysMem = parseMemWithPercents (toInteger getPhysicalMemory `roundTo` (4*mb))
  let getSimpleMethod = do
        m <- val method
        x <- val xMethod
        return$ show(if m==0 then 9 else 6-m)++(x.$bool "" "x")
  let describeMethod = do
        m <- val method
        x <- val xMethod
        methodName <- getSimpleMethod
        let compressor = methodName.$ decode_method 1 []
                                   .$ limitCompressionMem   (parsePhysMem "75%")
                                   .$ limitDecompressionMem (1*gb)
            cmem = compressor.$ compressorGetShrinkedCompressionMem
            dmem = compressor.$ compressorGetShrinkedDecompressionMem
        let level  =         "      ccm     uharc     7-zip        rar       bzip2      zip"
            cspeed = x.$bool "    3mb/s     3mb/s     5mb/s     10mb/s      20mb/s   50mb/s" --m9,m5..m1
                             "  2.5mb/s   2.5mb/s     5mb/s     10mb/s      20mb/s   50mb/s" --m9x,m5x..m1x
            dspeed = x.$bool " 3-40mb/s  3-40mb/s  5-60mb/s  10-60mb/s  15-100mb/s  120mb/s" --m9,m5..m1
                             "   40mb/s    40mb/s    60mb/s     60mb/s     100mb/s  120mb/s" --m9x,m5x..m1x
        labelSetMarkup methodText . deleteIf (=='_') . unlines =<< mapM i18fmt
            [ ["0114 Compression level: %1",               bold((words level!!m).$replace '_' ' ')]
            , ["0115 Compression speed: %1, memory: %2",   bold(words cspeed!!m), bold(showMem cmem)]
            , ["0116 Decompression speed: %1, memory: %2", bold(words dspeed!!m), bold(showMem dmem)]
            , [""]
            , ["0390 All speeds were measured on 3GHz Core2 Duo"]]
        w1 <- i18n (levels!!m)
        w2 <- i18n "0226 (fast, low-memory decompression)"
        autodetect'   <- val autodetect
        solidBytesOn' <- val solidBytesOn
        solidBytes'   <- val solidBytes
        solidFilesOn' <- val solidFilesOn
        solidFiles'   <- val solidFiles
        solidByExtension' <- val solidByExtension
        let s = (solidBytesOn'     &&&  solidBytes')++
                (solidFilesOn'     &&& (solidFiles'++"f"))++
                (solidByExtension' &&&  "e")
        cmethod =: w1++(x&&&" "++w2)++": "++"-m"++(methodName.$changeTo [("9","x")])++(not autodetect' &&& " -ma-")++(s &&& " -s"++s)
  --
  describeMethod
  describeMethod .$ setOnUpdate xMethod
  describeMethod .$ setOnUpdate method
  describeMethod .$ setOnUpdate autodetect
  describeMethod .$ setOnUpdate solidBytesOn
  describeMethod .$ setOnUpdate solidBytes
  describeMethod .$ setOnUpdate solidFilesOn
  describeMethod .$ setOnUpdate solidFiles
  describeMethod .$ setOnUpdate solidByExtension

  -- ���������� ������� ��������� ����� � ��������� ������� �� Save
  let saveHistories full = do
        whenM (val solidBytesOn) $ do saveHistory solidBytes
        whenM (val solidFilesOn) $ do saveHistory solidFiles
        when (full=="full")      $ do saveHistory cmethod
  save `onClick` saveHistories "full"

  -- ��������� ����� ���������� ������� �� ��������� ��������� ������ � ���������, ����������� ��� ������� �� OK
  return (\act -> setOnUpdate cmethod (val cmethod >>= act), saveHistories)

{-
    let simpleMethod = initSetting "simpleMethod" `defaultVal` "4"
        m =  case (take 1 simpleMethod) of [d] | isDigit d -> digitToInt d
                                           _               -> 4
        x =  drop 1 simpleMethod == "x"

      ; solidBytes'        <- val solidBytes;  solidBytes' !~ "*b"   &&&   solidBytes =: solidBytes'++"b"
      simpleMethod' <- getSimpleMethod
-}

-- |Compression level names
levels = [ "0108 Maximum",
           "0109 High",
           "0110 Normal",
           "0111 Fast",
           "0112 Very fast",
           "0127 HDD-speed"]


----------------------------------------------------------------------------------------------------
---- �������� ���������� -------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

encryptionPage fm' dialog okButton vbox = do
  let pack x = boxPackStart vbox x PackNatural 1
  (hbox, pwds)  <-  pwdBox 2;  pack hbox   -- ������ ������� � ������ ��� ����� ���� �������

  -- ����� ����������.
  vbox1 <- vBoxNew False 0
  frame <- frameNew;  s <- i18n"0119 Encryption"
  set frame [containerChild := vbox1, frameLabel := s, containerBorderWidth := 5]
  let pack1 x = boxPackStart vbox1 x PackNatural 1
  boxPackStart vbox frame        PackNatural 10

  -- �������� ����������.
  (hbox, method) <- fmLabeledEntryWithHistory fm' "encryption" "0179 Encryption profile:";  pack1 hbox
  ; save <- button "0180 Save";  boxPackStart hbox (widget save) PackNatural 0

  -- ��������� ����������.
  encryptHeaders <- checkBox "0120 Encrypt archive directory";  pack1 (widget encryptHeaders)
  usePwd <- checkBox "0181 Use password";  pack1 (widget usePwd)
  (hbox, keyfileOn, keyfile) <- fmFileBox fm' dialog
                                          "akeyfile" FileChooserActionOpen
                                (checkBox "0123 Keyfile:")
                                          "0124 Select keyfile"
                                          aANYFILE_FILTER
                                          (const$ return True)
                                          (fmCanonicalizeDiskPath fm')
  ; createKeyfile <- button "0125 Create"
  ; boxPackStart hbox (widget createKeyfile) PackNatural 0;  pack1 hbox
  (hbox, encAlg) <- fmLabeledEntryWithHistory fm' "encryptor" "0121 Encryption algorithm:";  pack1 hbox

  -- ��������� �����������
  (decryption, decryptionOnOK) <- decryptionBox fm' dialog
  ; boxPackStart vbox decryption        PackNatural 10

  -- ��������� ������ OK ������ ���� ��� �������� ������ ���������
  let [pwd1,pwd2] = pwds
  for pwds $ flip afterKeyRelease $ \e -> do
    [pwd1', pwd2'] <- mapM val pwds
    okButton `widgetSetSensitivity` (pwd1'==pwd2')
    return False

  -- ������� ����� ����-����, ������� ���������������� ��������� ������ � ��������� ������������� ����
  createKeyfile `onClick` do
    let default_keyfile = do fm <- val fm'; return$ fm_curdir fm </> "new.key"
    chooseFile dialog FileChooserActionSave "0126 Create new keyfile" aANYFILE_FILTER default_keyfile $ \filename -> do
      --to do: fileChooserSetDoOverwriteConfirmation chooserDialog True
      filePutBinary filename =<< generateRandomBytes 1024
      keyfile   =: filename
      keyfileOn =: True

  -- �������������: ��������� ������ �� ���������� ����������
  pwd1 =:: val encryptionPassword
  pwd2 =:: val encryptionPassword

  -- ���������� ������� ��������� ����� � ��������� ������� �� Save
  let saveHistories full = do
        whenM (val keyfileOn) $ do saveHistory keyfile
        when (full=="full")   $ do saveHistory method
        saveHistory encAlg
  save `onClick` saveHistories "full"

  -- ��������, ����������� ��� ������� �� OK. ���������� �����, ������� ����� �������� � ��������� ������
  let onOK full encryption = do
        saveHistories full
        pwd' <- val pwd1;  encryptionPassword =: pwd'
        decryptionOptions <- decryptionOnOK
        return$ decryptionOptions ++ ((words encryption `contains` "-p?") &&& pwd' &&& ["-p"++pwd'])

  -- ��������� ������� ���������� � ���������� ��� ��������� ����� ����� � ���� ������
  let makeProfile = do
        usePwd'         <- val usePwd
        keyfileOn'      <- val keyfileOn
        keyfile'        <- val keyfile
        encAlg'         <- val encAlg
        encryptHeaders' <- val encryptHeaders
        method =: unwords( (encryptHeaders' &&& ["-hp"])++
                           (usePwd'         &&& ["-p?"])++
                                                ["--encryption="++clear encAlg']++
                           (keyfileOn'      &&& ["--keyfile="   ++clear keyfile']))
  --
  makeProfile
  makeProfile .$ setOnUpdate usePwd
  makeProfile .$ setOnUpdate keyfileOn
  makeProfile .$ setOnUpdate keyfile
  makeProfile .$ setOnUpdate encAlg
  makeProfile .$ setOnUpdate encryptHeaders

  -- ��������� ����� ���������� ������� �� ��������� �������� ���������� � ���������, ����������� ��� ������� �� OK
  return (\act -> setOnUpdate method (val method >>= act), onOK)


----------------------------------------------------------------------------------------------------
---- ����� ����������� -----------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

decryptionBox fm' dialog = do
  vbox <- vBoxNew False 0
  decryptionFrame <- frameNew;  s <- i18n"0144 Decryption"
  set decryptionFrame [containerChild := vbox, frameLabel := s, containerBorderWidth := 5]

  lbl <- label "0074 Enter password:"
  pwd <- entryNew   --newTextViewWithText
  ; set pwd [entryVisibility := False, entryActivatesDefault := True]
  (keyfileBox, _, keyfile) <- fmFileBox fm' dialog
                                        "keyfile" FileChooserActionOpen
                                 (label "0123 Keyfile:")
                                        "0124 Select keyfile"
                                        aANYFILE_FILTER
                                        (const$ return True)
                                        (fmCanonicalizeDiskPath fm')
  hbox <- hBoxNew False 0
  ; boxPackStart hbox (widget lbl) PackNatural 0
  ; boxPackStart hbox pwd          PackGrow    5
  boxPackStart vbox hbox       PackNatural 0
  boxPackStart vbox keyfileBox PackNatural 5
  -- ��������� ������ �� ���������� ����������
  pwd =:: val decryptionPassword
  -- ��������, ����������� ��� ������� �� OK. ���������� �����, ������� ����� �������� � ��������� ������
  let onOK = do decryptionPassword =:: val pwd
                saveHistory keyfile
                fmGetDecryptionOptions fm'
  --
  return (decryptionFrame, onOK)


----------------------------------------------------------------------------------------------------
---- ��������������� ����������� -------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |������ ��������
data DialogMode = EncryptionMode | ProtectionMode | RecompressMode | CommentMode | MakeSFXMode | NoMode  deriving Eq

-- |���������� ��, ��� ����� ��������� ������������� � �������
addCmdFiles dirname =  [dirname++[pathSeparator]]
xCmdFiles   dirname =  [dirname++[pathSeparator]++"*"]
dCmdFiles   dirname =  [dirname, dirname++[pathSeparator]++"*"]

-- |��������� �������� ��� ������� �������/����� ����������� ������� �� �����
compressionOperation fm' action exec winoptions cmd mode = do
  fm <- val fm'
  files <- if isFM_Archive fm then return [fm_arcname fm]
                              else getSelection fm' addCmdFiles  -- todo: j/ch ����� Selection �������� ��������
  action fm' exec winoptions cmd files mode

-- |��������� �������� ��� ���������� ������� � ������/����� ������� � ��������� �������
archiveOperation fm' action = do
  fm <- val fm'
  files <- getSelection fm' (if isFM_Archive fm  then xCmdFiles  else const [])
  if isFM_Archive fm
    then action [fm_arcname fm] (fm_arcdir fm) files
    else do fullnames <- mapM (fmCanonicalizePath fm') files
            action fullnames "" []

-- |��������� ��������, ������� ����� �������� ������ ����� �������
multiArchiveOperation fm' action = do
  fm <- val fm'
  if isFM_Archive fm
    then action [fm_arcname fm]
    else do files <- getSelection fm' (const [])
            fullnames <- mapM (fmCanonicalizePath fm') files
            action fullnames

-- |�������� ���������� ������ ����-��������� ����������� �������
refreshCommand fm' = do
  fm <- val fm'
  curfile <- fmGetCursor fm'
  selected <- getSelection fm' (:[])
  -- ������� ���������� ��������/������ � ����������� ������� ���� � ������ ����������
  closeFMArc fm'
  fmChdir fm' (fm_current fm)
  when (selected>[]) $ do
    fmSetCursor fm' curfile
  fmUnselectAll fm'
  fmSelectFilenames fm' ((`elem` selected).fmname)

-- |����������� ����
runViewCommand = runEditCommand

-- |������������� ����
runEditCommand filename = run (iif isWindows "notepad" "gedit") [filename]
  where run cmd params = forkIO (rawSystem cmd params >> return ()) >> return ()
  -- edit filename | isWindows && takeExtension filename == "txt"  =  todo: direct shell open command

-- �������� ��� �������� � ����������� notebook � ��������� ��������� �������� ����� ������� � ��
startNotebook dialog = do
  upbox <- dialogGetUpper dialog
  nb <- notebookNew;  boxPackStart upbox nb PackGrow 0
  let newPage name = do hbox <- hBoxNew False 0; notebookAppendPage nb hbox =<< i18n name
                        vbox <- vBoxNew False 0; boxPackStart hbox vbox PackGrow 5
                        return vbox
  return (nb,newPage)

-- |��������� �������� � �������������� ���������� �����, ���� ������������ ������ contents
withTempFile contents = withTemporary (`filePutBinary` contents) fileRemove

-- |��������� �������� � �������������� ���������� ��������
withTempDir = withTemporary createDirectoryHierarchy dirRemoveRecursive

-- |��������� �������� � �������������� ���������� �����/��������
withTemporary preAction postAction action = do
  tempDir <- getTempDir
  createDirectoryHierarchy tempDir
  fix $ \tryNext -> do n <- generateRandomBytes 4 >>== encode16
                       let tempname = tempDir </> ("freearc"++n++".tmp")
                       e <- fileOrDirExist tempname
                       if e then tryNext
                            else do preAction tempname
                                    ensureCtrlBreak "remove temporary files" (ignoreErrors$ postAction tempname) $ do
                                      action tempname

