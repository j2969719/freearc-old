----------------------------------------------------------------------------------------------------
---- ������� ������������ ��������� ������ � ������ ������������� ������.                       ----
---- ���������� �� ArcCreate.hs                                                                 ----
----------------------------------------------------------------------------------------------------
module ArcvProcessRead where

import Prelude hiding (catch)
import Control.Exception
import Control.Monad
import Data.IORef
import Foreign.Ptr
import Foreign.C.Types
import Foreign.Marshal.Pool
import Foreign.Marshal.Utils

import Utils
import Files
import Process
import Errors
import FileInfo
import Compression
import Cmdline
import Statistics
import ArhiveStructure
import ArhiveDirectory
import ArhiveFileList
import ArcvProcessExtract


-- |����������, ���������� ��������� ������ ������� ������ �������� ��������
data Instruction
  =   DebugLog String                         --   ����� ����������� ���������
  |   CompressData BlockType Compressor Compressor Bool
                                              --   ������ ����� ������
  |     FileStart FileInfo                    --     ������ ���������� �����
  |       DataChunk (Ptr CChar) Int           --       ��������� ������ ������������� ������
  |     CorrectTotals FileCount FileSize      --     ������������� Total Files/Bytes ��� UI
  |     FakeFiles [FileToCompress]            --     ������������� ������ ��� ����������� ������ ��� UI
  |   CopySolidBlock [CompressedFile]         --   ����������� �����-����� ������� �� ������������� ������
  |   DataEnd                                 --   ����� ����� ������
  |   Directory [FileWithCRC]                 --   ������ �� ��������� ��������� ������ � ��������� ��������� ����� ������
  | TheEnd                                    -- �������� ������ ���������

-- |��������� ��� ������������� � ������: ��������� �� ����� ����� ������, ��������� �� ����� ������
notDataEnd DataEnd = False
notDataEnd _       = True
notTheEnd  TheEnd  = False
notTheEnd  _       = True


-- |�������, ��������� ��������� ������ - �� ��������� �����
--   �� ��������� ����� ������, ��������� ������ ������ ������ � �����-������.
-- ���� �� ������� �������� ������� ������ ��� �������� - ������ ����� � ����� �
--   ������������� ������ �� ������� �������.
create_archive_structure_AND_read_files_PROCESS command archive oldarc files processDir arcComment writeRecoveryBlocks results backdoor pipe = do
  initPos <- archiveGetPos archive
  -- �������� ������� ��� ���������� ������ �� ������� �������
  decompress_pipe <- runAsyncP$ decompress_PROCESS command doNothing
  -- �������� ��� ��� ������������ ������ ������������ ������
  withPool $ \pool -> do
  bufOps <- makeFileCache (opt_cache command) pool pipe
  -- ��������� ��� writeControlBlock
  let params = (command,bufOps,pipe,backdoor)

  -- ������� ���� ��������� (HEADER_BLOCK) � ������ ������
  header_block  <-  writeControlBlock HEADER_BLOCK aNO_COMPRESSION params $ do
                      archiveWriteHeaderBlock bufOps

  -- ������������ ������, �������� ����� �� dir-������
  directory_blocks <- foreach (splitToDirBlocks command files)
                              (createDirBlock archive processDir decompress_pipe params)
                      -- ��� ������������� ������ ��������� ���� ��� ���������� ������ c_compress()
                      `catch` (\e -> do programTerminated =: True; throwIO e)

  -- ������� ��������� ���� (FOOTER_BLOCK), ���������� ������� ��������� ������ ������ � ����������� ������
  let write_footer_block blocks arcRecovery = do
          footerPos <- archiveGetPos archive
          writeControlBlock FOOTER_BLOCK (dir_compressor command) params $ do
            let lock_archive = opt_lock_archive command   -- ������� ����������� ����� �� ���������?
            archiveWriteFooterBlock blocks lock_archive arcComment arcRecovery footerPos bufOps
          return ()
  write_footer_block (header_block:directory_blocks) ""

  -- �������� ������ ����������� ����������
  sendP decompress_pipe Nothing
  joinP decompress_pipe
  -- ���������� ���������� ���������� ������� � �������� � ��� �������� � ���������� ���������
  uiDoneArchive  >>=  writeIORef results

  -- ���� ������ RECOVERY ���������� �������� - ������� RECOVERY ����� � �������� FOOTER ����
  (recovery_blocks,recovery) <- writeRecoveryBlocks archive oldarc initPos command params bufOps
  unless (null recovery_blocks) $ do
    write_footer_block (header_block:directory_blocks++recovery_blocks) recovery

  -- �������� ������� ������ � �����, ��� �������� ������ ���������
  sendP pipe TheEnd


-- |�������� � ����� ���������� ����� � dir-���� � �� ���������
createDirBlock archive processDir decompress_pipe params@(command,bufOps,pipe,backdoor) files = do
  -- ������� ����� �� �����-������ � ���������� ������ ��������� ��������. ��� ������� - mapM (print.map (fpFullname.fiDiskName.cfFileInfo)) (splitToSolidBlocks files)
  solidBlocks <- foreach (splitToSolidBlocks command files)
                         (createSolidBlock command processDir bufOps pipe decompress_pipe)
  -- �������� �� �������� write_to_archive ���������� � ��������� �����-������ � ������������ � ��� ������.
  -- ���������� ���� ������� ��������� ���������� �������� ���� ����� ��������� ������ � ������ ����������� ������ � �����...
  blocks_info  <-  replicateM (length solidBlocks) (getP backdoor)
  -- ... ����� ���� ����� ���� ���������, ��� ������� ������� � ������ - ��� �������, ��� �������� ���� ��������
  dirPos <- archiveGetPos archive
  -- �������� ���� �������� � ���������� ���������� � ��� ��� ������������ �������� ���������
  writeControlBlock DIR_BLOCK (dir_compressor command) params $ do
    archiveWriteDir blocks_info dirPos bufOps


-- |������� �����-����, ���������� ������ �� ���������� ������
createSolidBlock command processDir bufOps pipe decompress_pipe files = do
  let -- ������� �������� ������ ��� ����� �����-�����
      -- � �������� ������� ��� ����������, ����� ��� ������ ������ ������ � �����;
      compressor = if copy_solid_block
                     then cfCompressor (head files)
                     else data_compressor command files
                          .$compressionLimitDictionary (clipToMaxInt$ roundMem$ totalBytes+512)
      -- ������� ������������ ���������� ���������� �� ������������� � ��������� �����
      -- �������� "tempfile", ������������ ����� ������� ������������ �����������
      real_compressor = compressor.$compressionLimitMemoryUsage freearcGetCompressionMem (opt_limit_compression_memory command)
      -- ����� ����� ������ � �����-�����
      totalBytes = sum$ map (fiSize.cfFileInfo) files
      -- True, ���� ��� ����� �����-���� �� �������� ������, ������� ����� ����������� ��� ���������
      copy_solid_block = not(opt_recompress command)  &&  isWholeSolidBlock files

  -- ����� �����-���� ������ � �������� � ��������� ������� ������ ���������� � ���� ������
  unless (null files) $ do
  printDebugInfo command pipe files totalBytes copy_solid_block compressor real_compressor
  writeBlock pipe DATA_BLOCK compressor real_compressor copy_solid_block $ do
    dir <- -- ���� �����-���� ��������� �� ������ � ����� �������, �� ������ �������� ��������� �����������
           if copy_solid_block then do
             sendP pipe (CopySolidBlock files)
             return$ map fileWithCRC files
           -- ���� ������������ --nodata, �� ������ ������ ������� ������
           else if (compressor==[aFAKE_COMPRESSION]) then do
             sendP pipe (FakeFiles files)
             return$ map fileWithCRC files
           -- ������� ������ ������ ��� ���� ������ (����� ��������) �������
           else do
             mapMaybeM (read_file command pipe bufOps decompress_pipe) files
    processDir dir   -- ���� ���������, ���������� ������, �������� ������ ��������������� ������ (������������ ��� ���������� ����� -tl, -ac, -d[f])
    return dir


-- |���������� ���������� ����������
printDebugInfo command pipe files totalBytes copy_solid_block compressor real_compressor = do
  --print (clipToMaxInt totalBytes, compressor)
  --print$ map (diskName.cfFileInfo) files   -- debugging tool :)
  when (opt_debug command) $ do
    sendP pipe$ DebugLog$ "Compressing "++show_files3(length files)++" of "++show_bytes3 totalBytes
    sendP pipe$ DebugLog$ if copy_solid_block then "  Copying "++join_compressor compressor  else "  Using "++join_compressor real_compressor
    unless (copy_solid_block) $ do
      sendP pipe$ DebugLog$ "  Memory for compression "++showMem (calcMem freearcGetCompressionMem   real_compressor)
                                   ++", decompression "++showMem (calcMem freearcGetDecompressionMem real_compressor)


---------------------------------------------------------------------------------------------------
---- ��������� ������ ������ �������������� ����� -------------------------------------------------
---------------------------------------------------------------------------------------------------

{-# NOINLINE read_file #-}
-- ���� ��� �������, �� ���������� ������ ������
read_file command pipe _ _ file  | fi<-cfFileInfo file, fiIsDir fi = do
  sendP pipe (FileStart fi)
  return$ Just$ fileWithCRC file

-- ���� ��� ���� �� �����, �� ��������� ��� �� ������, ��������� ����������� ����� �� ��������
read_file _ pipe (receiveBuf, sendBuf) _ (DiskFile old_fi) = do
  -- �������� �������������� ���������� �������� �� ��������� �������/���������� ������, ������� �� ������ �������� � UI
  let correctTotals files bytes  =  when (files/=0 || bytes/=0) (sendP pipe (CorrectTotals files bytes)) >> return Nothing
  -- ��������� ����������� ������� ���� - �� ����� ���� ������� ��� ��� �� ��� ����� ����� ����������� ������� :)
  tryOpen (diskName old_fi)  >>=  maybe (correctTotals (-1) (-fiSize old_fi))  (\file -> do
  -- ���������� ���������� � ����� �� ������, ���� �� ����� ����������
  rereadFileInfo old_fi file >>=  maybe (correctTotals (-1) (-fiSize old_fi))  (\fi -> do
  correctTotals 0 (fiSize fi - fiSize old_fi) -- �������������� ��������� UI, ���� ������ ����� ����� ����������
  sendP pipe (FileStart fi)                   -- �������������� ������������ � ������ �������� �����
  let readFile crc bytes = do    -- ��������� � ����� ����, ��������� ����������� ����� �� ��������:
        (buf, size) <- receiveBuf                -- ������� ��������� ����� �� ������� �������
        len         <- fileReadBuf file buf size -- ��������� � ���� ��������� ������ ������ �� �����
        newcrc      <- updateCRC buf len crc     -- ������� CRC ���������� ������
        sendBuf        buf size len              -- ������ ������ �������� ��������
        if len>0
          then readFile newcrc $! bytes+i len    -- ������� ������� ����������� ����
          else return (finishCRC newcrc, bytes)  -- ������ �� �����, ���� ���� ���������
  (crc,bytesRead) <- readFile aINIT_CRC 0     -- ��������� ����, ������� ��� CRC � ������
  fileClose file
  correctTotals 0 (bytesRead - fiSize fi)     -- �������������� ��������� UI, ���� ������ ����� ���������� �� ������������� getFileInfo
  return$ Just$ FileWithCRC crc FILE_ON_DISK fi{fiSize=bytesRead} ))

-- ���� ��� ���� �� ��� ������������� ������, �� ��������� ���, ��������� ������������� ����� �� ��������
read_file _ pipe (receiveBuf, sendBuf) decompress_pipe compressed_file = do
  crc  <-  ref aINIT_CRC                       -- �������������� �������� CRC
  -- �������� "������" ������������� ������ ���� ����������� �� � ����������� ������
  -- � ������� ���� ������� �� ����������� ���������
  let writer inbuf 0 = send_backP decompress_pipe ()  -- ������� ������������, ��� ������ ����� ��������
      writer inbuf insize = do
        (buf, size) <- receiveBuf              -- ������� ��������� ����� �� ������� �������
        let len  = min insize size             -- ��������� ������� ������ �� ����� ����������
        crc    .<- updateCRC inbuf len         -- ������� CRC ���������� ������
        copyBytes  buf inbuf len               -- ��������� ������ � ���������� �����
        sendBuf    buf size len                -- ����� �� ���������� �������� � �����������
        writer     (inbuf+:len) (insize-len)   -- ���������� ���������� ������, ���� ����
  let fi  =  cfFileInfo compressed_file
  sendP pipe (FileStart fi)                    -- �������������� ������������ � ������ ����������� �����
  decompress_file decompress_pipe compressed_file writer   -- ����������� ���� � ��������� �����
  crc'  <-  val crc >>== finishCRC            -- �������� ������������� �������� CRC
  if cfCRC compressed_file == crc'            -- ���� CRC � �������
    then return$ Just$ fileWithCRC compressed_file  -- �� ��������� ���������� � �����
    else registerError$ BAD_CRC$ diskName fi        -- ����� ��������� � �������


---------------------------------------------------------------------------------------------------
---- ��������������� ����������� ------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

-- |������� ��� ��� ������������ ������ � ���������� ��������� receiveBuf � sendBuf
-- ��� ��������� ���������� ������ �� ���� � ������������ ��������������� ������, ��������������
makeFileCache cache_size pool pipe = do
  -- ������ �������, �� ������� ����� ������ ���� ���
  let onebuf | cache_size>=aLARGE_BUFFER_SIZE*16  =  aLARGE_BUFFER_SIZE
             | otherwise                          =  aBUFFER_SIZE
  -- �������� ������ ��� ��� � ��������� memoryAllocator �� ���������� ���� ������
  heap                     <-  pooledMallocBytes pool cache_size
  (getBlock, shrinkBlock)  <-  memoryAllocator   heap cache_size onebuf 256 (receive_backP pipe)
  let -- �������� ��������� ���������� ������ � ����������� ������������ ������ ���������� ��������
      receiveBuf            =  do buf <- getBlock; return (buf, onebuf)
      sendBuf buf size len  =  do shrinkBlock buf len
                                  when (len>0)$  do sendP pipe (DataChunk buf len)
  return (receiveBuf, sendBuf)

{-# NOINLINE writeBlock #-}
-- |�������� � ����� ���� ������/���������/���������� �����
writeBlock pipe blockType compressor real_compressor just_copy action = do
  sendP pipe (CompressData blockType compressor real_compressor just_copy)
  directory <- action
  sendP pipe  DataEnd
  sendP pipe (Directory directory)

{-# NOINLINE writeControlBlock #-}
-- �������� � ����� ��������� ���� ������ � ��� ������������ � ���������� ���������� �� ���� �����
writeControlBlock blockType compressor (command,bufOps,pipe,backdoor) action = do
  if (opt_nodir command)   -- ����� "--nodir" ��������� ������ � ����� ���� ��������� ������ - �������� ������ ���� ������ ������
    then return (error "Attempt to use value returned by writeControlBlock when \"--nodir\"")
    else do
  writeBlock pipe blockType compressor compressor False $ do  -- ������� � ����� ���� ��������
    action; return []
  (thisBlock, [])  <-  getP backdoor                      -- ������� ��� ����������
  writeBlock pipe DESCR_BLOCK aNO_COMPRESSION aNO_COMPRESSION False $ do  -- ������� ���� ���������� � �����
    archiveWriteBlockDescriptor thisBlock bufOps; return []
  (_, [])  <-  getP backdoor                              -- ���������� �������� ���������� �����������
  return thisBlock                                        -- ��������� ���������� ����� ��������
