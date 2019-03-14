{-# OPTIONS_GHC -cpp #-}
----------------------------------------------------------------------------------------------------
---- Работа с архивами через 7z.dll.                                                          ------
---- Этот модуль содержит процедуры для:                                                      ------
----   * чтения структуры входного архива (т.е. каталогов и других служебных блоков)          ------
----   * распаковки архивов                                                                   ------
----------------------------------------------------------------------------------------------------
module Arhive7zLib where

import Prelude hiding (catch)
import Control.Concurrent
import Control.Monad
import Control.OldException
import Data.HashTable as Hash
import Data.Char
import Data.Int
import Data.List
import Data.Maybe
import Data.Word
import Foreign.C
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import System.Mem

import GHC.PArr

import TABI
import Utils
import Errors
import Files
import qualified ByteStream
import Charsets
import FileInfo
import CompressionLib   hiding (getDictionary)
import Compression
import Encryption       (generateSzDecryption)
import UI
import Options
import ArhiveStructure

----------------------------------------------------------------------------------------------------
---- Описание структуры входного архива ------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Вся необходимая информация о входном архиве
data ArchiveInfo = ArchiveInfo
         { arcArchive     :: !(Maybe Archive)    -- открытый файл архива .arc
         , arcFooter      :: FooterBlock         -- FOOTER BLOCK архива
         , arcDirectory   :: [CompressedFile]    -- файлы, содержащиеся в архиве
         , arcDataBlocks  :: [ArchiveBlock]      -- список солид-блоков
         , arcDirBytes    :: FileSize            -- размер служебных блоков в распакованном виде
         , arcDirCBytes   :: FileSize            -- размер служебных блоков в упакованном виде
         , arcDataBytes   :: FileSize            -- размер данных в распакованном виде
         , arcDataCBytes  :: FileSize            -- размер данных в упакованном виде
         , arcSzArchive   :: !(Maybe SzArchive)  -- Хендл архива из 7z.dll
         , arcArchiveType :: String              -- Тип архива
         }

-- |Проверка, что это архив, поддерживаемый самим FreeArc / через 7z.dll
isArcArchive  =  isJust . arcArchive
isSzArchive   =  isJust . arcSzArchive

-- |True, если архива на самом деле нет (используется для main_archive)
arcPhantom = not . any_function [isArcArchive, isSzArchive]

-- Процедуры, упрощающие работу с архивами
arcGetPos  = archiveGetPos . fromJust . arcArchive
arcSeek    = archiveSeek   . fromJust . arcArchive
arcComment = ftComment . arcFooter

-- |Фантомный, несуществующий архив, необходимый для применения в некоторых операциях
-- (слияние списков файлов, закрытие входных архивов)
phantomArc  =  dirlessArchive Nothing (FooterBlock [] False "" "" 0)

-- |Архив без каталога файлов - используется только для вызова writeSFX из runArchiveRecovery
dirlessArchive archive footer   =   ArchiveInfo archive footer [] [] (error "emptyArchive:arcDirBytes") (error "emptyArchive:arcDirCBytes") (error "emptyArchive:arcDataBytes") (error "emptyArchive:arcDataCBytes") Nothing "-"

-- |Закрыть архивный файл, если только это не фантомный архив
arcClose arc  =  do whenJust (arcArchive arc) archiveClose
                    whenJust (arcSzArchive arc) szArcClose
                    return ()


----------------------------------------------------------------------------------------------------
---- Получение тех. информации об архиве в виде текстового блока -----------------------------------
----------------------------------------------------------------------------------------------------

arcGetTechinfo archive dirs_and_files = do
    let filelist    = map (cfFileInfo)$ arcDirectory archive
        footer      = arcFooter archive
        dataBlocks  = arcDataBlocks archive        -- список солид-блоков
        numOfBlocks = length dataBlocks
        empty       = "-"
    ;   yes        <- i18n"0101 Yes" >>== replaceAll "_" ""

    let origsize = arcDataBytes  archive  -- суммарный объём файлов в распакованном виде
        compsize = arcDataCBytes archive  -- суммарный объём файлов в упакованном виде
        getCompressors = partition isEncryption.blCompressor  -- разделить алг-мы шифрования и сжатия для блока
        (encryptors, compressors) = unzip$ map getCompressors dataBlocks  -- список алг. шифрования и сжатия.
        header_encryptors = deleteIf null$ map (fst.getCompressors) (ftBlocks footer)  -- алгоритмы шифрования служебных блоков
        all_encryptors = deleteIf null encryptors ++ header_encryptors   -- а теперь все вместе :)
        ciphers = joinWith "\n"$ removeDups$ map (join_compressor.map method_name) all_encryptors   -- имена алг. шифрования.
        formatMem s  =  x++" "++y  where (x,y) = span isDigit$ showMem s

    let -- Максимальные словари основных и вспомогательных алгоритмов
        dicts = compressors.$ map (splitAt 1.reverse.map getDictionary)  -- [([mainDict],[auxDict1,auxDict2..])...]
                           .$ map (\([x],ys) -> (x, maximum(0:ys)))      -- [(mainDict,maxAuxDict)...]
                           .$ ((0,0):) .$ sort .$ last                   -- Выбираем строчку с макс. основным и вспом. словарём
        dictionaries  =  case dicts of
                           (0,0)                     -> empty
                           (maxMainDict, 0)          -> formatMem maxMainDict
                           (maxMainDict, maxAuxDict) -> showMem maxAuxDict++" + "++showMem maxMainDict

        ciphers_text = [("0097 Encryption algorithms:",  ciphers ||| empty)]

    return$[[("0465 Archive type:",     arcArchiveType archive)]
            ++ dirs_and_files ++
            [("0089 Total bytes:",      show3$ origsize)
            ,("0090 Compressed bytes:", show3$ compsize)
            ,("0091 Ratio:",            ratio3 compsize origsize++"%")]

           ,[("0104 Directory blocks:", show3$ length$ filter ((DIR_BLOCK==).blType) (ftBlocks footer))
            ,("0463 Directory, bytes:", show3$ arcDirBytes archive)
            ,("0464 Directory, compressed:", show3$ arcDirCBytes archive)
            ,("0092 Solid blocks:",     show3$ numOfBlocks)
            ,("0093 Avg. blocksize:",   formatMem$ origsize `div` i(max numOfBlocks 1))]

           ,[("0099 Compression memory:",    formatMem$ maximum$ 0: map compressionGetShrinkedCompressionMem   compressors)
            ,("0100 Decompression memory:",  formatMem$ maximum$ 0: map compressionGetShrinkedDecompressionMem compressors)
            ,("0105 Dictionary:",            dictionaries)]

           ,[("0094 Archive locked:",    ftLocked footer   &&& yes ||| empty)
            ,("0098 Archive comment:",   ftComment footer  &&& yes ||| empty)
            ,("0095 Recovery info:",     ftRecovery footer ||| empty)
            ,("0096 SFX size:",          ftSFXSize footer .$show3 .$changeTo [("0", empty)])
            ,("0156 Headers encrypted:", header_encryptors &&& yes ||| empty)]
            ++ (if ciphers>""   then []   else ciphers_text)
           ] ++ (ciphers &&& [ciphers_text])


----------------------------------------------------------------------------------------------------
---- Чтение структуры входного архива --------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Open archive
szOpenArchive command arcname = do
#if !defined(FREEARC_WIN)
  sz_so <- findOrCreateFile libraryFilePlaces "7z.so"
  mySetEnv "P7ZIP_HOME_DIR" (dropFileName sz_so) True      -- set directory to search for 7z.so (system-wide 7z.so may be incompatible with our program!!!)
#endif
  withCWString arcname $ \c_arcname -> do
  alloca $ \archive -> do
  szCallback <- createSzCallback command (error "szOpenArchive: undefined arcinfo") (error "szOpenArchive: undefined can_be_extracted")
  szCheckedTABI c_szOpenArchive [ Pair "archive"   archive
                                , Pair "arcname"   c_arcname
                                , Pair "callback"  (szCallback :: TABI.FUNCTION)
                                ]
  archive <- peek archive
  comment     <- szArcGetStrProperty archive (-1) kpidComment
  sfxSize     <- szArcGetIntProperty archive (-1) kpidOffset
  let footer = FooterBlock [] False comment "" sfxSize
  return (archive,footer)


-- |Read archive directory
szReadInfo archive footer filter_f processFooterInfo arcname = do
  processFooterInfo Nothing footer
  dirCBytes   <- szArcGetIntProperty archive (-1) kpidHeadersSize
  archiveType <- szArcGetStrProperty archive (-1) kpidType
  numFiles    <- szArcItems archive
  files       <- foreach [0..numFiles-1] (szArcGetItem filter_f archive archiveType arcname)  >>==  catMaybes
  return ArchiveInfo { arcArchive     = Nothing
                     , arcFooter      = footer
                     , arcDirectory   = files
                     , arcDataBlocks  = []
                     , arcDirBytes    = 0
                     , arcDirCBytes   = dirCBytes
                     , arcDataBytes   = sum (map (fiSize.cfFileInfo) files)
                     , arcDataCBytes  = sum (mapMaybe cfCompsize files)
                     , arcSzArchive   = Just archive
                     , arcArchiveType = archiveType
                     }

-- |Get info about one file in archive, return Nothing if it was filtered out by filter_f
szArcGetItem filter_f archive archiveType arcname item = do
  fi        <- szArcGetItemFileInfo archive archiveType arcname item
  if not (filter_f fi)  then return Nothing  else do
  fiCRC     <- szArcGetIntProperty  archive item kpidCRC
  compsize  <- szArcGetIntProperty  archive item kpidPackSize
  encrypted <- szArcGetBoolProperty archive item kpidEncrypted
  return$ Just$ CompressedFile fi
                               (error "szArcGetItem: undefined cfArcBlock")
                               (error "szArcGetItem: undefined cfPos")
                               fiCRC
                               (Just$ SzData compsize encrypted (i item))

-- |Get info about one file in archive
szArcGetItemFileInfo archive archiveType arcname item = do
  fullname  <- szArcGetStrProperty  archive item kpidPath  >>== changeTo [("", takeBaseName arcname)]
  fiSize    <- szArcGetIntProperty  archive item kpidSize
  fiTime    <- case archiveType of
                 "bzip2" -> getFileDateTime arcname
                 _       -> szArcGetTimeProperty archive item kpidMTime
  fiAttr    <- szArcGetIntProperty  archive item kpidAttrib
  fiIsDir   <- szArcGetBoolProperty archive item kpidIsDir
  let root = packParentDirPath ""
      fp   = packFilePath root fullname
  return$ FileInfo fp fp fp fiSize fiTime fiAttr fiIsDir fiUndefinedGroup


----------------------------------------------------------------------------------------------------
---- Распаковка архива -----------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Test/extract archive contents
szExtract command arcinfo can_be_extracted = do
  withCWString  (opt_disk_basedir command) $ \c_disk_basedir -> do
  withArrayLen  (map (szIndex.fromJust.cfSzData) (arcDirectory arcinfo)) $ \files c_filelist -> do
  szCallback <- createSzCallback command arcinfo can_be_extracted
  szCheckedTABI c_szExtract [ Pair "cmd"         (cmd_name command)
                            , Pair "archive"     (fromJust$ arcSzArchive arcinfo)
                            , Pair "OutputDir"   c_disk_basedir
                            , Pair "callback"    (szCallback :: TABI.FUNCTION)
                            , Pair "filelist"    (c_filelist :: Ptr Word32)
                            , Pair "files"       files
                            , Pair "keepBroken?" (opt_keep_broken command)
                            ]
  return ()


-- |Создаёт Callback для распаковки архивов через 7z.dll
createSzCallback command arcinfo can_be_extracted = do
  saved_filename <- ref ""
  return $ \p -> do
    --TABI.dump p
    service <- TABI.required p "request"
    case service of
      -- Можно ли извлечь этот файл?
      "can_be_extracted?" -> do
                       let Just archive = arcSzArchive arcinfo
                       c_outname <- TABI.required p "outname"
                       index     <- TABI.required p "index"
                       outname <- peekCWString c_outname
                       b <- can_be_extracted command outname (szArcGetItemFileInfo archive (arcArchiveType arcinfo) (cmd_arcname command) index)
                       return$ iif b 1 0
      -- Запрос пароля
      "ask_password" -> do
                       password_buf  <- TABI.required p "password_buf"
                       password_size <- TABI.required p "password_size"
                       password <- generateSzDecryption (opt_decryption_info command)
                       when (length password < password_size) $ do
                         pokeArray0 0 password_buf (map (fromIntegral.ord) password :: [CWchar])
                       return aFREEARC_OK
      -- Начало распаковки нового файла
      "filename" -> do c_filename <- TABI.required p "filename"
                       is_folder  <- TABI.required p "is_folder?"
                       mode       <- TABI.required p "mode"
                       filename   <- peekCWString c_filename
                       uiStartFile (mode==kSkip &&& msgSkipping)
                                   (Left (filename, is_folder))
                       saved_filename =: filename
                       return aFREEARC_OK
      -- Окончание распаковки файла
      "filedone" -> do operationResult <- TABI.required p "operationResult"
                       when (operationResult /= kOK) $ do
                         encrypted     <- TABI.required p "encrypted?"
                         filename      <- val saved_filename
                         let err = case () of
                                     _ | operationResult==kUnSupportedMethod          -> UNSUPPORTED_METHOD
                                       | operationResult==kDataError && not encrypted -> DATA_ERROR
                                       | operationResult==kDataError &&     encrypted -> DATA_ERROR_ENCRYPTED
                                       | operationResult==kCRCError  && not encrypted -> BAD_CRC
                                       | operationResult==kCRCError  &&     encrypted -> BAD_CRC_ENCRYPTED
                                       | otherwise                                    -> UNKNOWN_ERROR
                         registerWarning$ err filename
                       return aFREEARC_OK
      -- Информируем пользователя о ходе распаковки
      "progress" -> do original   <- TABI.required p "original"
                       compressed <- TABI.required p "compressed"
                       uiUnpackedBytes           original
                       uiUpdateProgressIndicator original
                       uiCompressedBytes         compressed
                       return aFREEARC_OK
      -- Прочие (неподдерживаемые) callbacks
      _          -> do return aFREEARC_ERRCODE_NOT_IMPLEMENTED


----------------------------------------------------------------------------------------------------
---- Интерфейс к сишной части реализации -----------------------------------------------------------
----------------------------------------------------------------------------------------------------

type SzArchive  = Ptr ()
type SzItem     = Int32
type SzProperty = Int32
type SzErrCode  = Int32
type SzErrMsg   = CString

foreign import ccall   safe ""   c_szOpenArchive :: TABI.C_FUNCTION
foreign import ccall   safe ""   c_szArcClose    :: SzArchive -> SzErrMsg -> IO SzErrCode
foreign import ccall   safe ""   c_szArcItems    :: SzArchive -> Ptr SzItem -> SzErrMsg -> IO SzErrCode
foreign import ccall unsafe ""   c_szArcGetInt64Property :: SzArchive -> SzItem -> SzProperty -> Ptr Int64 -> SzErrMsg -> IO SzErrCode
foreign import ccall unsafe ""   c_szArcGetTimeProperty  :: SzArchive -> SzItem -> SzProperty -> Ptr Int32 -> SzErrMsg -> IO SzErrCode
foreign import ccall unsafe ""   c_szArcGetBoolProperty  :: SzArchive -> SzItem -> SzProperty -> Ptr Int32 -> SzErrMsg -> IO SzErrCode
foreign import ccall unsafe ""   c_szArcGetStrProperty   :: SzArchive -> SzItem -> SzProperty -> CWString -> Int32 -> SzErrMsg -> IO SzErrCode
foreign import ccall   safe ""   c_szExtract     :: TABI.C_FUNCTION

szArcClose           archive                =  szChecked (c_szArcClose archive)
szArcItems           archive                =  withTemp  (c_szArcItems archive)
szArcGetIntProperty  archive item property  =  withTemp  (c_szArcGetInt64Property archive item property) >>== i
szArcGetTimeProperty archive item property  =  withTemp  (c_szArcGetTimeProperty  archive item property) >>== i
szArcGetBoolProperty archive item property  =  withTemp  (c_szArcGetBoolProperty  archive item property) >>== (/=0)
szArcGetStrProperty  archive item property  =  withTempCWString (c_szArcGetStrProperty archive item property)


withTemp action = alloca $ \ptr -> do
                    szChecked$ action ptr
                    peek ptr

withTempCWString action = allocaBytes (long_path_size*4) $ \ptr -> do
                             szChecked$ action ptr (i long_path_size)
                             peekCWString ptr

szChecked action = allocaBytes 1000 $ \ptr -> do
                     poke ptr (toEnum 0)
                     result <- action ptr
                     when (result/=0) $ do
                       peekCString ptr >>= throwIO . ErrorCall

szCheckedTABI action p = do
                     result <- TABI.call action p
                     when (result/=0) $ do
                       throwIO$ ErrorCall "szCheckedTABI: error"

-- |NArchive::NExtract::NAskMode
kExtract:
  kTest:
  kSkip:_ = [0..] :: [Int]

-- |NArchive::NExtract::NOperationResult
kOK:
  kUnSupportedMethod:
  kDataError:
  kCRCError:_ = [0..] :: [Int]

-- |7-zip property IDs
kpidNoProperty:
  kpidMainSubfile:
  kpidHandlerItemIndex:
  kpidPath:
  kpidName:
  kpidExtension:
  kpidIsDir:
  kpidSize:
  kpidPackSize:
  kpidAttrib:
  kpidCTime:
  kpidATime:
  kpidMTime:
  kpidSolid:
  kpidCommented:
  kpidEncrypted:
  kpidSplitBefore:
  kpidSplitAfter:
  kpidDictionarySize:
  kpidCRC:
  kpidType:
  kpidIsAnti:
  kpidMethod:
  kpidHostOS:
  kpidFileSystem:
  kpidUser:
  kpidGroup:
  kpidBlock:
  kpidComment:
  kpidPosition:
  kpidPrefix:
  kpidNumSubDirs:
  kpidNumSubFiles:
  kpidUnpackVer:
  kpidVolume:
  kpidIsVolume:
  kpidOffset:
  kpidLinks:
  kpidNumBlocks:
  kpidNumVolumes:
  kpidTimeType:
  kpidBit64:
  kpidBigEndian:
  kpidCpu:
  kpidPhySize:
  kpidHeadersSize:
  kpidChecksum:
  kpidCharacts:
  kpidVa:
  kpidId:
  kpidShortName:
  kpidCreatorApp:
  kpidSectorSize:
  kpidPosixAttrib:
  kpidLink:_ = [0..]


----------------------------------------------------------------------------------------------------
---- Упаковываемый файл (или с диска, или из уже существующего архива) -----------------------------
----------------------------------------------------------------------------------------------------

-- |File to compress: either file on disk or compressed file in existing archive
data FileToCompress
  = DiskFile
      { cfFileInfo           :: !FileInfo
      }
  | CompressedFile
      { cfFileInfo           :: !FileInfo
      , cfArcBlock           ::  ArchiveBlock   -- Archive datablock which contains file data
      , cfPos                ::  FileSize       -- Starting byte of file data in datablock
      , cfCRC :: {-# UNPACK #-} !CRC            -- File's CRC
      , cfSzData             :: !(Maybe SzData) -- Only for 7z.dll-handled archives
      }

-- |Additional data for files in archives handled by 7z.dll
data SzData = SzData { szCompsize  :: !FileSize
                     , szEncrypted :: !Bool
                     , szIndex     :: !Word32
                     }

-- |Assign type synonym because variant label can't be used in another types declarations
type CompressedFile = FileToCompress


-- |Проверка того, что упаковываемый файл - из уже существующего архива, а не с диска
isCompressedFile CompressedFile{} = True
isCompressedFile DiskFile{}       = False

-- |Проверка что упакованный файл принадлежит архиву, поддерживаемому 7z.dll
isCfSz = isJust.cfSzData

-- |Алгоритм сжатия, использованный для данного (сжатого) файла
cfCompressor cf | isCfSz cf = ["lzma"]
                | otherwise = blCompressor (cfArcBlock cf)

-- |Это сжатый файл, использующий фейковый метод компрессии?
isCompressedFake file  =  isCompressedFile file  &&  isFakeCompressor (cfCompressor file)

-- |Сжатый размер файл, если доступен
cfCompsize = fmap szCompsize . cfSzData

-- |Это запаролированный файл?
cfIsEncrypted cf  =  case cfSzData cf of
                       Just szData  ->  szEncrypted szData              -- данные получены из 7z.dll
                       Nothing      ->  blIsEncrypted (cfArcBlock cf)   -- определяем по методу сжатия солид-блока

-- |Определить тип файла по группе, если она не проставлена - вычислить по имени
cfType command file | group/=fiUndefinedGroup  =  opt_group2type command group
                    | otherwise                =  opt_find_type command fi
                                                    where fi    = cfFileInfo file
                                                          group = fiGroup fi


----------------------------------------------------------------------------------------------------
---- Файл и его CRC - используется для передачи результатов упаковки -------------------------------
----------------------------------------------------------------------------------------------------

-- |File and it's CRC
data FileWithCRC = FileWithCRC { fwCRC  :: {-# UNPACK #-} !CRC
                               , fwType :: {-# UNPACK #-} !FileType
                               , fwFileInfo            :: !FileInfo
                               }

data FileType = FILE_ON_DISK | FILE_IN_ARCHIVE  deriving (Eq)

-- |Проверка того, что упакованный файл - из исходного архива, а не с диска
isFILE_ON_DISK fw  =  fwType fw == FILE_ON_DISK

-- |Convert FileToCompress to FileWithCRC
fileWithCRC (DiskFile       fi)            =  FileWithCRC 0   FILE_ON_DISK    fi
fileWithCRC (CompressedFile fi _ _ crc _)  =  FileWithCRC crc FILE_IN_ARCHIVE fi

