----------------------------------------------------------------------------------------------------
---- �������� � ���������� ������.                                                              ----
---- ��������� � ����������� �� �� �����������, ������������ ��� �������� ������.               ----
----------------------------------------------------------------------------------------------------
module CompressionLib where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Bits
import Data.Char
import Data.Int
import Data.List
import Data.Maybe
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Pool
import Foreign.Storable
import Foreign.Ptr
import System.IO.Unsafe

----------------------------------------------------------------------------------------------------
----- Error codes ----------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

aFREEARC_OK                            =  0   --  ALL RIGHT
aFREEARC_ERRCODE_GENERAL               = -1   --  Some error when (de)compressing
aFREEARC_ERRCODE_INVALID_COMPRESSOR    = -2   --  Invalid compression method or parameters
aFREEARC_ERRCODE_ONLY_DECOMPRESS       = -3   --  Program builded with FREEARC_DECOMPRESS_ONLY, so don't try to use compress
aFREEARC_ERRCODE_OUTBLOCK_TOO_SMALL    = -4   --  Output block size in (de)compressMem is not enough for all output data
aFREEARC_ERRCODE_NOT_ENOUGH_MEMORY     = -5   --  Can't allocate memory needed for (de)compression
aFREEARC_ERRCODE_IO                    = -6   --  Error when reading or writing data
aFREEARC_ERRCODE_BAD_COMPRESSED_DATA   = -7   --  Data can't be decompressed
aFREEARC_ERRCODE_NOT_IMPLEMENTED       = -8   --  Requested feature isn't supported
aFREEARC_ERRCODE_NO_MORE_DATA_REQUIRED = -9   --  Required part of data was already decompressed


----------------------------------------------------------------------------------------------------
----- Main exported definitions --------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Useful definitions for kilobytes, megabytes and so on
b:kb:mb:gb:_  = iterate (1024*) 1 :: [Int]

-- |Compress using callbacks
compress             = runWithMethod c_compress
-- |Decompress using callbacks
decompress           = runWithMethod c_decompress
-- |Compress using callbacks and save method name in compressed output
compressWithHeader   = runWithMethod c_CompressWithHeader
-- |Decompress data compressed with compressWithHeader (method name is read from compressed stream)
decompressWithHeader = run           c_DecompressWithHeader

-- |Compress memory block
compressMem             :: Method -> Ptr CChar -> Int -> Ptr CChar -> Int -> IO Int
compressMem             = withMethod c_CompressMem
-- |Decompress memory block
decompressMem           :: Method -> Ptr CChar -> Int -> Ptr CChar -> Int -> IO Int
decompressMem           = withMethod c_DecompressMem
-- |Compress memory block and save method name in compressed output
compressMemWithHeader   :: Method -> Ptr CChar -> Int -> Ptr CChar -> Int -> IO Int
compressMemWithHeader   = withMethod c_CompressMemWithHeader
-- |Decompress memory block compressed with compressMemWithHeader (method name is read from compressed data)
decompressMemWithHeader ::           Ptr CChar -> Int -> Ptr CChar -> Int -> IO Int
decompressMemWithHeader a b c d = c_DecompressMemWithHeader a b c d

-- |Return canonical representation of compression method
canonizeCompressionMethod :: Method -> Method
canonizeCompressionMethod = doWithMethod c_CanonizeCompressionMethod

-- |Returns memory used to compress/decompress, dictionary or block size of method given
getCompressionMem, getDecompressionMem, getDictionary, getBlockSize :: Method -> MemSize
getCompressionMem      =  getWithMethod c_GetCompressionMem
getDecompressionMem    =  getWithMethod c_GetDecompressionMem
getDictionary          =  getWithMethod c_GetDictionary
getBlockSize           =  getWithMethod c_GetBlockSize

-- |Set memory used to compress/decompress, dictionary or block size of method given
setCompressionMem, setDecompressionMem, setDictionary, setBlockSize :: MemSize -> Method -> Method
setCompressionMem      =  setWithMethod c_SetCompressionMem
setDecompressionMem    =  setWithMethod c_SetDecompressionMem
setDictionary          =  setWithMethod c_SetDictionary
setBlockSize           =  setWithMethod c_SetBlockSize

-- |Put upper limit to memory used to compress/decompress, dictionary or block size of method given
limitCompressionMem, limitDecompressionMem, limitDictionary, limitBlockSize :: MemSize -> Method -> Method
limitCompressionMem    =  setWithMethod c_LimitCompressionMem
limitDecompressionMem  =  setWithMethod c_LimitDecompressionMem
limitDictionary        =  setWithMethod c_LimitDictionary
limitBlockSize         =  setWithMethod c_LimitBlockSize

-- |Adds new external compresion method to the table
addExternalCompressor description =
  withCString description $ \c_description -> do
    c_AddExternalCompressor c_description

----------------------------------------------------------------------------------------------------
----- General compression services -----------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Check boolean property of compression method, returning FALSE if it's not implemented
compressionIs :: Parameter -> Method -> Bool
compressionIs param method  =  compressionQuery (const 0) param method /= 0

-- |Get value of compression method parameter or raise error if this parameter isn't supported
compressionGet :: Parameter -> Method -> Int
compressionGet param method  =  compressionQuery (\e -> error$ "Error "++show e++" when querying "++method++":"++param) param method

-- |Query compression method for some parameter
compressionQuery :: (Int->Int) -> Parameter -> Method -> Int
compressionQuery defaultVal param method =
    case unsafePerformIO$ compressionService method param 0 nullPtr (error$ method++":"++param++" - callback undefined")
      of x | x<0 -> defaultVal x
         x       -> x

-- |General function to call compression services
compressionService :: Method -> String -> Int -> VoidPtr -> CALLBACK_FUNC -> IO Int
compressionService method what param dat callback = do
  withCString method $ \c_method -> do
    withCString what $ \c_what -> do
      bracket (mkCALL_BACK callback) (freeHaskellFunPtr)$ \c_callback -> do
        res <- c_CompressionService c_method c_what (ii param) dat c_callback
        return (ii res)

----------------------------------------------------------------------------------------------------
----- Encryption routines --------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

#ifndef FREEARC_NO_ENCRYPTION
-- |Query encryption method for some parameter
encryptionGet = compressionGet

-- |Generates key based on password and salt using given number of hashing iterations
pbkdf2Hmac :: String -> String -> Int -> Int -> String
pbkdf2Hmac password salt iterations keySize = unsafePerformIO $
  withCStringLen   password $ \(c_password, c_password_len) -> do
    withCStringLen salt     $ \(c_salt,     c_salt_len) -> do
    allocaBytes    keySize  $ \c_key -> do
      c_Pbkdf2Hmac c_password (ii c_password_len) c_salt (ii c_salt_len) (ii iterations) c_key (ii keySize)
      peekCStringLen (c_key, keySize)
#endif

----------------------------------------------------------------------------------------------------
----- Internal auxiliary functions -----------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Run specified action with Haskell arguments converted to C equivalents
runWithMethod action method callback = do
  withCString method $ \c_method -> do
    run (action c_method) callback

-- |Execute C (de)compression routine `action` using read/write callbacks `read_f` & `write_f`
run action callback = do
  let callback2 cwhat buf size auxdata = do what <- peekCString cwhat
                                            callback what buf (ii size) auxdata >>=return.ii
  bracket (mkCALL_BACK callback2) (freeHaskellFunPtr)$ \c_callback -> do   -- convert Haskell routine to C-callable routine
    action c_callback c_callback

withMethod action method inp insize outp outsize = do
  withCString method $ \c_method -> do
    action c_method inp insize outp outsize


getWithMethod c_get method  =  unsafePerformIO$ withCString method c_get

setWithMethod c_action bytes = doWithMethod (\x y -> c_action x bytes y)

doWithMethod c_action method = do
  unsafePerformIO $ generalDoWithMethod "compression" c_action method

generalDoWithMethod mType c_action method = do
  withCString method $ \c_method -> do
    allocaBytes aMAX_METHOD_STRLEN $ \c_out_method -> do
      ret <- c_action c_method c_out_method
      case ret of
        0 -> peekCString c_out_method
        _ -> fail$ "Unsupported "++mType++" method or error in parameters: "++method

-- |Universal integral types conversion routine
ii x = fromIntegral x

-- |Typeless pointer
type VoidPtr = Ptr ()

{-# NOINLINE run #-}
{-# NOINLINE doWithMethod #-}

----------------------------------------------------------------------------------------------------
----- High-level compression/decompression routines working with callbacks -------------------------
----------------------------------------------------------------------------------------------------

-- |Compress using callbacks
foreign import ccall threadsafe  "Compression.h compress"
   c_compress             :: CMethod -> FunPtr CALLBACK_FUNC -> FunPtr CALLBACK_FUNC -> IO Int

-- |Decompress using callbacks
foreign import ccall threadsafe  "Compression.h decompress"
   c_decompress           :: CMethod -> FunPtr CALLBACK_FUNC -> FunPtr CALLBACK_FUNC -> IO Int

-- |Compress using callbacks and save method name in compressed output
foreign import ccall threadsafe  "Compression.h CompressWithHeader"
   c_CompressWithHeader   :: CMethod -> FunPtr CALLBACK_FUNC -> FunPtr CALLBACK_FUNC -> IO Int

-- |Decompress data compressed with c_CompressWithHeader (method name is read from compressed stream)
foreign import ccall threadsafe  "Compression.h DecompressWithHeader"
   c_DecompressWithHeader ::            FunPtr CALLBACK_FUNC -> FunPtr CALLBACK_FUNC -> IO Int


----------------------------------------------------------------------------------------------------
----- High-level compression/decompression routines working with memory buffers --------------------
----------------------------------------------------------------------------------------------------

-- |Compress memory block
foreign import ccall threadsafe  "Compression.h CompressMem"
   c_CompressMem             :: CMethod -> Ptr CChar -> Int -> Ptr CChar -> Int -> IO Int

-- |Decompress memory block
foreign import ccall threadsafe  "Compression.h DecompressMem"
   c_DecompressMem           :: CMethod -> Ptr CChar -> Int -> Ptr CChar -> Int -> IO Int

-- |Compress memory block and save method name in compressed output
foreign import ccall threadsafe  "Compression.h CompressMemWithHeader"
   c_CompressMemWithHeader   :: CMethod -> Ptr CChar -> Int -> Ptr CChar -> Int -> IO Int

-- |Decompress memory block compressed with c_CompressMemWithHeader (method name is read from compressed data)
foreign import ccall threadsafe  "Compression.h DecompressMemWithHeader"
   c_DecompressMemWithHeader ::            Ptr CChar -> Int -> Ptr CChar -> Int -> IO Int


----------------------------------------------------------------------------------------------------
----- Functions to add/query/modify compression methods --------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Get/set number of threads used for (de)compression
foreign import ccall unsafe  "Compression.h  GetCompressionThreads"
   getCompressionThreads :: IO Int
foreign import ccall unsafe  "Compression.h  SetCompressionThreads"
   setCompressionThreads :: Int -> IO ()

-- |Adds new external compresion method to the table
foreign import ccall unsafe  "External/C_External.h  AddExternalCompressor"
   c_AddExternalCompressor   :: CString -> IO Int

-- |Returns canonical representation of compression method or error code
foreign import ccall unsafe  "Compression.h  CanonizeCompressionMethod"
   c_CanonizeCompressionMethod   :: CMethod -> CMethod -> IO Int

foreign import ccall unsafe  "Compression.h CompressionService"
   c_CompressionService :: CMethod -> CString -> CInt -> VoidPtr -> FunPtr CALLBACK_FUNC -> IO CInt

foreign import ccall unsafe  "Compression.h compressionLib_cleanup"
   compressionLib_cleanup :: IO ()

-- |Returns memory used to compress/decompress, dictionary or block size of method given
foreign import ccall unsafe  "Compression.h GetCompressionMem"   c_GetCompressionMem   :: CMethod -> IO MemSize
foreign import ccall unsafe  "Compression.h GetDecompressionMem" c_GetDecompressionMem :: CMethod -> IO MemSize
foreign import ccall unsafe  "Compression.h GetDictionary"       c_GetDictionary       :: CMethod -> IO MemSize
foreign import ccall unsafe  "Compression.h GetBlockSize"        c_GetBlockSize        :: CMethod -> IO MemSize

-- |Set memory used to compress/decompress, dictionary or block size of method given
foreign import ccall unsafe  "Compression.h SetCompressionMem"   c_SetCompressionMem   :: CMethod -> MemSize -> CMethod -> IO Int
foreign import ccall unsafe  "Compression.h SetDecompressionMem" c_SetDecompressionMem :: CMethod -> MemSize -> CMethod -> IO Int
foreign import ccall unsafe  "Compression.h SetDictionary"       c_SetDictionary       :: CMethod -> MemSize -> CMethod -> IO Int
foreign import ccall unsafe  "Compression.h SetBlockSize"        c_SetBlockSize        :: CMethod -> MemSize -> CMethod -> IO Int

-- |Put upper limit to memory used to compress/decompress, dictionary or block size of method given
foreign import ccall unsafe  "Compression.h LimitCompressionMem"   c_LimitCompressionMem   :: CMethod -> MemSize -> CMethod -> IO Int
foreign import ccall unsafe  "Compression.h LimitDecompressionMem" c_LimitDecompressionMem :: CMethod -> MemSize -> CMethod -> IO Int
foreign import ccall unsafe  "Compression.h LimitDictionary"       c_LimitDictionary       :: CMethod -> MemSize -> CMethod -> IO Int
foreign import ccall unsafe  "Compression.h LimitBlockSize"        c_LimitBlockSize        :: CMethod -> MemSize -> CMethod -> IO Int

----------------------------------------------------------------------------------------------------
----- Encryption/PRNG routines ---------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

#ifndef FREEARC_NO_ENCRYPTION
-- |Generates key based on password and salt using given number of hashing iterations
foreign import ccall unsafe  "Compression.h  Pbkdf2Hmac"
   c_Pbkdf2Hmac :: Ptr CChar -> CInt -> Ptr CChar -> CInt -> CInt -> Ptr CChar -> CInt -> IO ()

-- PRNG
foreign import ccall unsafe  "Compression.h  fortuna_size"
   prng_size :: CInt
foreign import ccall unsafe  "Compression.h  fortuna_start"
   prng_start :: Ptr CChar -> IO CInt
foreign import ccall unsafe  "Compression.h  fortuna_add_entropy"
   prng_add_entropy :: Ptr CChar -> CULong -> Ptr CChar -> IO CInt
foreign import ccall unsafe  "Compression.h  fortuna_ready"
   prng_ready :: Ptr CChar -> IO CInt
foreign import ccall unsafe  "Compression.h  fortuna_read"
   prng_read :: Ptr CChar -> CULong -> Ptr CChar -> IO CULong
#endif

----------------------------------------------------------------------------------------------------
----- Direct calls to (de)compression routines -----------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |PPMD compression
foreign import ccall threadsafe  "PPMD/C_PPMD.h ppmd_compress"
   ppmd_compress   :: Int -> Int -> Int -> FunPtr INOUT_FUNC -> FunPtr INOUT_FUNC -> IO Int

-- |PPMD decompression
foreign import ccall threadsafe  "PPMD/C_PPMD.h ppmd_decompress"
   ppmd_decompress :: Int -> Int -> Int -> FunPtr INOUT_FUNC -> FunPtr INOUT_FUNC -> IO Int

-- |LZP compression
foreign import ccall threadsafe  "LZP/C_LZP.h lzp_compress"
   lzp_compress    :: Int -> Int -> Int -> Int -> Int -> Int -> FunPtr INOUT_FUNC -> FunPtr INOUT_FUNC -> IO Int

-- |LZP decompression
foreign import ccall threadsafe  "LZP/C_LZP.h lzp_decompress"
   lzp_decompress  :: Int -> Int -> Int -> Int -> Int -> Int -> FunPtr INOUT_FUNC -> FunPtr INOUT_FUNC -> IO Int

-- |LZMA compression
foreign import ccall threadsafe  "LZMA/C_LZMA.h lzma_compress"
   lzma_compress   :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> FunPtr INOUT_FUNC -> FunPtr INOUT_FUNC -> IO Int

-- |LZMA decompression
foreign import ccall threadsafe  "LZMA/C_LZMA.h lzma_decompress"
   lzma_decompress :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> FunPtr INOUT_FUNC -> FunPtr INOUT_FUNC -> IO Int

-- |BCJ-x86 preprocessing - encoder
foreign import ccall threadsafe  "LZMA/C_BCJ.h bcj_x86_compress"
   bcj_x86_compress    :: FunPtr INOUT_FUNC -> FunPtr INOUT_FUNC -> IO Int

-- |BCJ-x86 preprocessing - decoder
foreign import ccall threadsafe  "LZMA/C_BCJ.h bcj_x86_decompress"
   bcj_x86_decompress  :: FunPtr INOUT_FUNC -> FunPtr INOUT_FUNC -> IO Int

-- |GRZip compression
foreign import ccall threadsafe  "GRZip/C_GRZip.h grzip_compress"
   grzip_compress  :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> FunPtr INOUT_FUNC -> FunPtr INOUT_FUNC -> IO Int

-- |GRZip decompression
foreign import ccall threadsafe  "GRZip/C_GRZip.h grzip_decompress"
   grzip_decompress:: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> FunPtr INOUT_FUNC -> FunPtr INOUT_FUNC -> IO Int


-- |I/O callback function type
type INOUT_FUNC  =  Ptr CChar -> Int -> IO Int
foreign import ccall threadsafe "wrapper"
   mkInOutFunc :: INOUT_FUNC -> IO (FunPtr INOUT_FUNC)

-- |General callback function type
type CALLBACK_FUNC  =  CString -> Ptr CChar -> CInt -> VoidPtr -> IO CInt
foreign import ccall threadsafe "wrapper"
   mkCALL_BACK :: CALLBACK_FUNC -> IO (FunPtr CALLBACK_FUNC)

-- |Maximum length of string representing compression/encryption method
aMAX_METHOD_STRLEN = 2048

-- |Compression method representation
type Method = String

-- |Compression method representation in C
type CMethod = CString

-- |Compression method parameter
type Parameter = String

-- |Memory sizes
type MemSize = CUInt

