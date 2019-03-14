----------------------------------------------------------------------------------------------------
---- ������� ���������� ������� �������.                                                        ----
---- ���������� �� ArcExtract.hs � ArcCreate.hs (��� ���������� � ������� �������).             ----
----------------------------------------------------------------------------------------------------
module ArcvProcessExtract where

import Prelude hiding (catch)
import Control.Exception
import Control.Monad
import Data.Int
import Data.IORef
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Utils
import Foreign.Storable

import Utils
import Process
import FileInfo
import CompressionLib
import Compression
import Encryption
import Cmdline
import Statistics
import ArhiveStructure
import ArhiveDirectory

-- |���������� ����� �� ������ � �������������� ����������� �������� �������������
-- � ������� ������������� ������ � ������� ������� `writer`
decompress_file decompress_pipe compressed_file writer = do
  -- �� �������� ����������� ��������/������ ����� � ����� ��� ������, ��������� ������� ��������� 0 ������ - �������� �������� ������� ;)
  when (fiSize(cfFileInfo compressed_file) > 0  &&  not (isCompressedFake compressed_file)) $ do
    sendP decompress_pipe (Just compressed_file)
    repeat_whileM (receiveP decompress_pipe) ((>=0).snd) (uncurry writer)
    return ()

-- |�������, ��������������� ����� �� �������
decompress_PROCESS command count_cbytes pipe = do
  cmd <- receiveP pipe
  case cmd of
    Nothing     -> return ()
    Just cfile' -> do
      cfile <- ref cfile'
      state <- ref (error "Decompression state is not initialized!")
      repeat_untilM $ do
        decompress_block command cfile state count_cbytes pipe
        (x,_,_) <- val state
        return (x == aSTOP_DECOMPRESS_THREAD)

-- |����������� ���� �����-����
decompress_block command cfile state count_cbytes pipe = mdo
  cfile' <- val cfile
  let size        =  fiSize      (cfFileInfo cfile')
      pos         =  cfPos        cfile'
      block       =  cfArcBlock   cfile'
      compressor  =  blCompressor block .$ compressionLimitMemoryUsage freearcGetDecompressionMem (opt_limit_decompression_memory command)
      startPos  | compressor==aNO_COMPRESSION  =  pos  -- ��� -m0 �������� ������ �������� � ������ ������� � �����
                | otherwise                    =  0
  state =: (startPos, pos, size)
  archiveBlockSeek block startPos
  bytesLeft <- ref (blCompSize block - startPos)

  let reader buf size  =  do aBytesLeft <- val bytesLeft
                             let bytes   = minI size aBytesLeft
                             len        <- archiveBlockReadBuf block buf bytes
                             bytesLeft  -= i len
                             count_cbytes  len
                             return len

  let writer (DataBuf buf len)  =  decompress_step cfile state pipe buf len
      writer  NoMoreData        =  return 0

  -- �������� ���� � ������ ��������� ������������
  keyed_compressor <- generateDecryption compressor (opt_decryption_info command)

  -- ��������� ������ ������� ������/���������� � �������� ��������� ����������
  let decompress1 = decompress_p reader times                    -- ������ ������� � ���������
      decompressN = de_compress_PROCESS freearcDecompress times  -- ����������� �������� � ���������
      decompressa [p]     = decompress1 p
      decompressa [p1,p2] = decompress1 p2        |> decompressN p1 0
      decompressa (p1:ps) = decompress1 (last ps) |> foldl1 (|>) (map (\x->decompressN x 0) (reverse$ init ps)) |> decompressN p1 0

  -- � ������� ��������� ����������
  times <- uiStartDeCompression "decompression"  -- ������� ��������� ��� ����� ������� ����������
  ; result <- ref 0   -- ���������� ����, ���������� � ��������� ������ writer
  ; runFuncP (decompressa keyed_compressor) (undefined) (undefined) (writer .>>= writeIORef result) (val result)
  uiFinishDeCompression times                    -- ������ � UI ������ ����� ��������

decompress_p reader times comprMethod pipe = do
  total' <- ref ( 0 :: FileSize)
  time'  <- ref (-1 :: Double)
  let writer buf size = do total'+=i size; resend_data pipe (DataBuf buf size)
      callback "quasiwrite" _ _  =  return aFREEARC_OK
      callback "time" ptr 0 = do t <- peek (castPtr ptr::Ptr CDouble) >>==realToFrac
                                 time' =: t
                                 return aFREEARC_OK
  -- ���������� ����������
  result <- freearcDecompress 0 comprMethod reader writer callback
  -- ����������
  total <- val total'
  time  <- val time'
  uiDeCompressionTime times (comprMethod,time,total)
  resend_data pipe NoMoreData
  return ()

-- |���������, ������������ ��� �������� ������ ���������� �������� ��������/����������
data CompressionData = DataBuf (Ptr CChar) Int
                     | NoMoreData

-- |��������� �������� �������� ������ ����������/������������ ��������� ��������� � �������
resend_data pipe x@DataBuf{}   =  sendP pipe x  >>  receive_backP pipe  -- ���������� ���������� ����������� ����, ������������ �� ��������-�����������
resend_data pipe x@NoMoreData  =  sendP pipe x  >>  return 0


-- |��������������� ������� �������������� ������ �� ������� �������� ������
-- �� ������� ������ ��������� ��������/����������
--   comprMethod - ������ ������ ������ � �����������, ���� "ppmd:o10:m48m"
--   num - ����� �������� � ������� ��������� ��������
--   count - ����� ���������� ��������� �������� (��� ����������)
de_compress_PROCESS de_compress times comprMethod num pipe = do
  -- ���������� �� ������� ������, ���������� �� ����������� ��������, �� ��� �� ������������ �� ��������/����������
  remains <- ref$ Just (error "undefined remains:buf0", error "undefined remains:srcbuf", 0)
  let
    -- ��������� "������" ������� ������. �����, ����� ������ ����� � dstlen=0 �� ��������� ���������� ���� �� �������� ���� �� ���� ���� ������ �� ����������� ��������
    read_data prevlen  -- ������� ������ ��� ���������
              dstbuf   -- �����, ���� ����� ��������� ������� ������
              dstlen   -- ������ ������
              = do     -- -> ��������� ������ ���������� ���������� ����������� ���� ��� 0, ���� ������ �����������
      remains' <- val remains
      case remains' of
        Just (buf0, srcbuf, srclen)                   -- ���� ��� ���� ������, ���������� �� ����������� ��������
         | srclen>0  ->  copyData buf0 srcbuf srclen  --  �� �������� �� ����������/������������
         | otherwise ->  processNextInstruction       --  ����� �������� �����
        Nothing      ->  return prevlen               -- ���� solid-���� ����������, ������ ������ ���
      where
        -- ����������� ������ �� srcbuf � dstbuf � ���������� ������ ������������� ������
        copyData buf0 srcbuf srclen = do
          let len = srclen `min` dstlen    -- ���������� - ������� ������ �� ����� ���������
          copyBytes dstbuf srcbuf len
          uiReadData num (i len)           -- �������� ��������� ���������
          remains =: Just (buf0, srcbuf+:len, srclen-len)
          case () of
           _ | len==srclen -> do send_backP pipe (srcbuf-:buf0+srclen)               -- ���������� ������ ������, ��������� ��� ������ �� ���� ��� �������� ����������/������������
                                 read_data (prevlen+len) (dstbuf+:len) (dstlen-len)  -- ��������� ��������� ����������
             | len==dstlen -> return (prevlen+len)                                 -- ����� ���������� ��������
             | otherwise   -> read_data (prevlen+len) (dstbuf+:len) (dstlen-len)   -- �������� ������� ������ ���������� ��������� ������

        -- �������� ��������� ���������� �� ������ ������� ������ � ���������� �
        processNextInstruction = do
          instr <- receiveP pipe
          case instr of
            DataBuf srcbuf srclen  ->  copyData srcbuf srcbuf srclen
            NoMoreData             ->  do remains =: Nothing;  return prevlen

  -- ��������� ������ ������� ������ �������� ��������/���������� (���������� ���� �������, � ������� �� read_data)
  let reader  =  read_data 0

  -- ��������� ������ �������� ������ �������� ��������/����������
  total' <- ref ( 0 :: FileSize)
  time'  <- ref (-1 :: Double)
  let writer buf size = do total' += i size
                           uiWriteData num (i size)
                           resend_data pipe (DataBuf buf size)

  -- ��� ��������� ������� �������� (���)������� �������������� �����
      -- "�����������" ������ ������������� ������� ������ ����� �������� � ���������� ������
      -- ��� ����������� ������. �������� ��������� ����� int64* ptr
  let callback "quasiwrite" ptr size = do bytes <- peek (castPtr ptr::Ptr Int64) >>==i
                                          uiQuasiWriteData num bytes
                                          return aFREEARC_OK
      -- ���������� � ������ ������� ���������� ��������/����������
      callback "time" ptr 0 = do t <- peek (castPtr ptr::Ptr CDouble) >>==realToFrac
                                 time' =: t
                                 return aFREEARC_OK

  -- ���������� �������� ��� ����������
  result <- de_compress num comprMethod reader writer callback
  -- ����������
  total <- val total'
  time  <- val time'
  uiDeCompressionTime times (comprMethod,time,total)
  -- ������� ����������� ��������, ��� ������ ������ �� �����, � ���������� - ��� ������ ������ ���
  send_backP  pipe aFREEARC_ERRCODE_NO_MORE_DATA_REQUIRED
  resend_data pipe NoMoreData
  return ()


-- |��������� ��������� ������ ������������� ������ (writer ��� ������������).
-- ��������� (�������� �� ������ state) ��������:
--   1) block_pos - ������� ������� � ����� ������
--   2) pos       - �������, � ������� ���������� ���� (��� ��� ���������� �����)
--   3) size      - ������ ����� (��� ��� ���������� �����)
-- ��������������, ������� �� ������������ ������ �� ������ buf ������ len, �� ������:
--   1) ���������� � ������ ������ ������, �������������� ���������������� ����� (���� ����)
--   2) �������� �� ����� ������, ����������� � ����� ����� (���� ����)
--   3) �������� ��������� - ������� � ����� ���������� �� ������ ����������� ������,
--        � ������� � ������ ���������� ������ ����� - �� ������ ���������� �� ����� ������
--   4) ���� ���� ���������� ��������� - ���� ��������� �� ���� ����������� �������
--        � �������� ��������� ������� �� ����������
--   5) ���� ��������� ��������������� ���� �������� � ������ ����� ��� � ��� ��������� �����
--        �������� ����� - ���� �������� ���������� ����� ����� � ���, ����� decompress_block
--        ������� � ���������� ����, ��� ����� (�� ������ ��� ������ �� cfile)
--
decompress_step cfile state pipe buf len = do
  (block_pos, pos, size) <- val state
  if block_pos<0   -- ������, ��� ����������� �� ������� ��������, ��� �� ����� ������� � ������� ����� ������
    then return aFREEARC_ERRCODE_NO_MORE_DATA_REQUIRED   -- ������, ��������, ���� �� �����������. ������������: fail$ "Block isn't changed!!!"
    else do
  let skip_bytes = min (pos-block_pos) (i len)   -- ���������� ������ ���������� ������ � ������ ������
      data_start = buf +: skip_bytes             -- ������ ������, ������������� ���������������� �����
      data_size  = min size (i len-skip_bytes)   -- ���-�� ����, ������������� ���������������� �����
      block_end  = block_pos+i len               -- ������� � �����-�����, ��������������� ����� ����������� ������
  when (data_size>0) $ do    -- ���� � ������ ������� ������, ������������� ���������������� �����
    sendP pipe (data_start, i data_size)  -- �� ������� ��� ������ �� ������ ����� �����������
    receive_backP pipe                    -- �������� ������������� ����, ��� ������ ���� ������������
  state =: (block_end, pos+data_size, size-data_size)
  if data_size<size     -- ���� ���� ��� �� ���������� ���������
    then return len     -- �� ���������� ���������� �����
    else do             -- ����� ��������� � ���������� ������� �� ����������
  sendP pipe (error "End of decompressed data", aFREEARC_ERRCODE_NO_MORE_DATA_REQUIRED)
  old_block  <-  cfArcBlock ==<< val cfile
  cmd <- receiveP pipe
  case cmd of
    Nothing -> do  -- ��� ��������� ��������, ��� ������ ������� ������ �� ����� ���������� �� ��������� � �� ������ ���� ��������
      state =: (aSTOP_DECOMPRESS_THREAD, error "undefined state.pos", error "undefined state.size")
      cfile =: error "undefined cfile"
      return aFREEARC_ERRCODE_NO_MORE_DATA_REQUIRED

    Just cfile' -> do
      cfile =: cfile'
      let size   =  fiSize (cfFileInfo cfile')
          pos    =  cfPos      cfile'
          block  =  cfArcBlock cfile'
      if block/=old_block || pos<block_pos  -- ���� ����� ���� ��������� � ������ ����� ��� � ����, �� ������
           || (pos>block_end && blCompressor block==aNO_COMPRESSION)   -- ��� �� ������������� ����, ������ � -m0, � � ��� ���� ����������� ���������� ����� ������
        then do state =: (-1, error "undefined state.pos", error "undefined state.size")
                return aFREEARC_ERRCODE_NO_MORE_DATA_REQUIRED   -- ������� ����, ��� ����� ��������� ���������� ����� �����
        else do state =: (block_pos, pos, size)            -- ����� ���������� ���������� �����,
                decompress_step cfile state pipe buf len   -- ��� � ��������� ���������� ������ �����

-- |������, ��������� ���������� ������ ����� ����������
aSTOP_DECOMPRESS_THREAD = -99

{-# NOINLINE decompress_PROCESS #-}
{-# NOINLINE decompress_p #-}
{-# NOINLINE resend_data #-}
{-# NOINLINE de_compress_PROCESS #-}
