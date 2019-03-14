#include "Compression.h"
#include "MultiThreading.h"

// Распаковать данные заданным методом сжатия и возвратить время работы в секундах
int timed_decompress (COMPRESSION_METHOD *compressor, CALLBACK_FUNC *callback, void *auxdata)
{
  //SET_JMP_POINT( FREEARC_ERRCODE_GENERAL);
  double time0 = GetThreadCPUTime();
  int result = compressor->decompress (callback, auxdata);
  double time1 = GetThreadCPUTime(), t;
  if (time0>=0 && time1>=0 && compressor->addtime>=0)
    t = compressor->addtime + time1 - time0;
  else
    t = -1;
  callback ("time", &t, 0, auxdata);
  return result;
}

// Распаковать данные заданным методом сжатия
int Decompress (char *method, CALLBACK_FUNC *callback, void *auxdata)
{
  COMPRESSION_METHOD *compressor = ParseCompressionMethod (method);
  if (compressor){
    int result = timed_decompress (compressor, callback, auxdata);
    delete compressor;
    return result;}
  else
    return FREEARC_ERRCODE_INVALID_COMPRESSOR;
}

// Прочитать из входного потока обозначение метода сжатия и распаковать данные этим методом
int DecompressWithHeader (CALLBACK_FUNC *callback, void *auxdata)
{
  char method [MAX_METHOD_STRLEN];
  for (int i=0; i<MAX_METHOD_STRLEN; i++)
  {
    // Посимвольно читаем входные данные, пока не прочтём символ конца строки
    callback ("read", &method[i], 1, auxdata);
    if (method[i]=='\0')
      return Decompress (method, callback, auxdata);
  }
  return FREEARC_ERRCODE_INVALID_COMPRESSOR;  // Сюда мы попадаем, если в первых MAX_METHOD_STRLEN символах входных данных не нашлось символа '\0'
}

// Callback-функция чтения/записи для (рас)паковки в памяти
void *readPtr;    // текущая позиция читаемых данных
int   readLeft;   // сколько байт ещё осталось во входном буфере
void *writePtr;   // текущая позиция записываемых данных
int   writeLeft;  // сколько байт ещё осталось в выходном буфере
int ReadWriteMem (const char *what, void *buf, int size, void *callback)
{
  if (strequ(what,"read")) {
    int read_bytes = readLeft<size ? readLeft : size;
    memcpy (buf, readPtr, read_bytes);
    readPtr   = (uint8*)readPtr+read_bytes;
    readLeft -= read_bytes;
    return read_bytes;
  } else if (strequ(what,"write")) {
    if (size>writeLeft)  return FREEARC_ERRCODE_OUTBLOCK_TOO_SMALL;
    memcpy (writePtr, buf, size);
    writePtr   = (uint8*)writePtr+size;
    writeLeft -= size;
    return size;
  } else {
    return FREEARC_ERRCODE_NOT_IMPLEMENTED;
  }
}

// Распаковать данные в памяти, записав в выходной буфер не более outputSize байт.
// Возвращает код ошибки или количество байт, записанных в выходной буфер
int DecompressMem (char *method, void *input, int inputSize, void *output, int outputSize)
{
  readPtr=input, readLeft=inputSize, writePtr=output, writeLeft=outputSize;
  int result = Decompress (method, ReadWriteMem, 0);
  return result<0 ? result : outputSize-writeLeft;
}

// Распаковать данные в памяти, записав в выходной буфер не более outputSize байт.
// Возвращает код ошибки или количество байт, записанных в выходной буфер
int DecompressMemWithHeader (void *input, int inputSize, void *output, int outputSize)
{
  readPtr=input, readLeft=inputSize, writePtr=output, writeLeft=outputSize;
  int result = DecompressWithHeader (ReadWriteMem, 0);
  return result<0 ? result : outputSize-writeLeft;
}

#ifndef FREEARC_DECOMPRESS_ONLY

// Упаковать данные заданным методом сжатия и возвратить время работы в секундах
int timed_compress (COMPRESSION_METHOD *compressor, CALLBACK_FUNC *callback, void *auxdata)
{
  //SET_JMP_POINT( FREEARC_ERRCODE_GENERAL);
  double time0 = GetThreadCPUTime();
  int result = compressor->compress (callback, auxdata);
  double time1 = GetThreadCPUTime(), t;
  if (time0>=0 && time1>=0 && compressor->addtime>=0)
    t = compressor->addtime + time1 - time0;
  else
    t = -1;
  callback ("time", &t, 0, auxdata);
  return result;
}

// Упаковать данные заданным методом сжатия
int Compress (char *method, CALLBACK_FUNC *callback, void *auxdata)
{
  COMPRESSION_METHOD *compressor = ParseCompressionMethod (method);
  if (compressor){
    int result = timed_compress (compressor, callback, auxdata);
    delete compressor;
    return result;}
  else
    return FREEARC_ERRCODE_INVALID_COMPRESSOR;
}

// Записать в выходной поток обозначение метода сжатия и упаковать данные этим методом
int CompressWithHeader (char *method, CALLBACK_FUNC *callback, void *auxdata)
{
  COMPRESSION_METHOD *compressor = ParseCompressionMethod (method);
  if (compressor){
    char canonical_method [MAX_METHOD_STRLEN];
    compressor->ShowCompressionMethod (canonical_method, FALSE);
    int result = callback ("write", canonical_method, strlen(canonical_method)+1, auxdata);
    if (result>=0) result = timed_compress (compressor, callback, auxdata);
    delete compressor;
    return result;}
  else
    return FREEARC_ERRCODE_INVALID_COMPRESSOR;
}

// Упаковать данные в памяти, записав в выходной буфер не более outputSize байт.
// Возвращает код ошибки или количество байт, записанных в выходной буфер
int CompressMem (char *method, void *input, int inputSize, void *output, int outputSize)
{
  readPtr=input, readLeft=inputSize, writePtr=output, writeLeft=outputSize;
  int result = Compress (method, ReadWriteMem, 0);
  return result<0 ? result : outputSize-writeLeft;
}

// Упаковать данные в памяти, записав в выходной буфер не более outputSize байт.
// Возвращает код ошибки или количество байт, записанных в выходной буфер
int CompressMemWithHeader (char *method, void *input, int inputSize, void *output, int outputSize)
{
  readPtr=input, readLeft=inputSize, writePtr=output, writeLeft=outputSize;
  int result = CompressWithHeader (method, ReadWriteMem, 0);
  return result<0 ? result : outputSize-writeLeft;
}

// Вывести в canonical_method каноническое представление метода сжатия in_method
int CanonizeCompressionMethod (char *method, char *canonical_method, int purify)
{
  COMPRESSION_METHOD *compressor = ParseCompressionMethod (method);
  if (compressor){
    compressor->ShowCompressionMethod (canonical_method, purify!=0);
    delete compressor;
    return FREEARC_OK;}
  else
    return FREEARC_ERRCODE_INVALID_COMPRESSOR;
}


#define Generate_Getter(GETTER)                                              \
  MemSize GETTER (char *method)                                              \
  {                                                                          \
    COMPRESSION_METHOD *compressor = ParseCompressionMethod (method);        \
    if (compressor){                                                         \
      MemSize bytes = compressor->GETTER();                                  \
      delete compressor;                                                     \
      return bytes;}                                                         \
    else                                                                     \
      return (MemSize)FREEARC_ERRCODE_INVALID_COMPRESSOR;                    \
  }                                                                          \

#define Generate_Setter(SETTER)                                              \
  int SETTER (char *in_method, MemSize bytes, char *out_method)              \
  {                                                                          \
    COMPRESSION_METHOD *compressor = ParseCompressionMethod (in_method);     \
    if (compressor){                                                         \
      compressor->SETTER (bytes);                                            \
      compressor->ShowCompressionMethod (out_method, FALSE);                 \
      delete compressor;                                                     \
      return FREEARC_OK;}                                                    \
    else                                                                     \
      return FREEARC_ERRCODE_INVALID_COMPRESSOR;                             \
  }                                                                          \

// Информация о памяти, необходимой для упаковки/распаковки, размере словаря и размере блока.
Generate_Getter(GetCompressionMem)
Generate_Getter(GetDictionary)
Generate_Getter(GetBlockSize)

// Возвратить в out_method новый метод сжатия, настроенный на использование
// соответствующего количества памяти при упаковке/распаковке или словаря/размера блока
Generate_Setter(SetCompressionMem)
Generate_Setter(SetDecompressionMem)
Generate_Setter(SetDictionary)
Generate_Setter(SetBlockSize)

// Возвратить в out_method новый метод сжатия, уменьшив, если необходимо,
// используемую алгоритмом память / его словарь / размер блока
Generate_Setter(LimitCompressionMem)
Generate_Setter(LimitDecompressionMem)
Generate_Setter(LimitDictionary)
Generate_Setter(LimitBlockSize)

#endif  // !defined (FREEARC_DECOMPRESS_ONLY)

// Информация о памяти, необходимой для распаковки
MemSize GetDecompressionMem (char *method)
{
  COMPRESSION_METHOD *compressor = ParseCompressionMethod (method);
  if (compressor){
    MemSize bytes = compressor->GetDecompressionMem();
    delete compressor;
    return bytes;}
  else
    return (MemSize)FREEARC_ERRCODE_INVALID_COMPRESSOR;
}


// Универсальный метод. Параметры:
//   what: "compress", "decompress", "setCompressionMem", "limitDictionary"...
//   data: данные для операции в формате, зависящем от конкретной выполняемой операции
//   param&result: простой числовой параметр, что достаточно для многих информационных операций
// Неиспользуемые параметры устанавливаются в NULL/0. result<0 - код ошибки
int COMPRESSION_METHOD::doit (char *what, int param, void *data, CALLBACK_FUNC *callback)
{
       if (strequ (what, "encryption?"))           return 0;        // Это алгоритм шифрования?
  else if (strequ (what, "GetCompressionMem"))     return 0;        // Объём памяти, необходимый для упаковки
  else if (strequ (what, "GetDecompressionMem"))   return 0;        // Объём памяти, необходимый для распаковки
  else                                             return FREEARC_ERRCODE_NOT_IMPLEMENTED;
}


// ****************************************************************************************************************************
// Распаковка данных, сжатых цепочкой методов                                                                                 *
// ****************************************************************************************************************************

// Локальные данные одного метода
struct Params
{
  Thread              thread;         // OS thread executing this (de)compression algorithm
  int                 thread_num;     // Number of method in chain (0..N-1)
  int                 threads_total;  // Total amount of methods in chain (N)
  CMETHOD             method;         // String denoting (de)compression method with its parameters
  CALLBACK_FUNC*      callback;       // Original callback (function that reads data in first method and write data in last one)
  void*               auxdata;        // Original callback parameter
  BYTE*               buf;            // Buffer that points to data sent from i'th thread to i+1'th
  int                 size;           // Amount of data in the buf
  ManualEvent*        done;           // Set when (de)compression is finished or error was found
  int*                retcode;        // Overall multi_decompress return code
  Mutex*              retcode_cs;     // Ensure single-threaded access to retcode
  Semaphore           read;
  Semaphore           write;

  // Abort multi_decompress and set its exit code
  void SetExitCode (int code)
  {
    Lock _(*retcode_cs);
    if (*retcode == 0)  *retcode = code;   // Save into retcode first error code signalled (subsequent error codes may be sequels of the first one)
    done->Signal();
  }
};
static THREAD_FUNC_RET_TYPE THREAD_FUNC_CALL_TYPE multi_decompress_thread (void *paramPtr);
static int multi_decompress_callback (const char *what, void *buf, int size, void *paramPtr);


// Распаковать данные, сжатые цепочкой методов
int MultiDecompress (char *_method, CALLBACK_FUNC *callback, void *auxdata)
{
  char method [MAX_METHOD_STRLEN];
  strncopy (method, _method, sizeof (method));

  // Разобьём компрессор на отдельные алгоритмы и запустим для каждого из них отдельный тред
  CMETHOD cm[MAX_METHODS_IN_COMPRESSOR];
  int N = split (method, COMPRESSION_METHODS_DELIMITER, cm, MAX_METHODS_IN_COMPRESSOR);
  if (N==1)  return Decompress (method, callback, auxdata);  // multi-threading isn't required
  Params param[MAX_METHODS_IN_COMPRESSOR];

  ManualEvent        done;           // Set when (de)compression is finished or error was found
  int                retcode = 0;    // multi_decompress return code
  Mutex              retcode_cs;     // Ensure single-threaded access to retcode

  // Create semaphores for inter-thread communication
  for (int i=0; i<N; i++)
  {
    param[i].read .Create(0,1);
    param[i].write.Create(0,1);
  }
  // Start N threads
  for (int i=0; i<N; i++)
  {
    param[i].thread_num    = i;
    param[i].threads_total = N;
    param[i].method        = cm[N-1-i];    // при распаковке пускаем кино задом наперёд :D
    param[i].callback      = callback;
    param[i].auxdata       = auxdata;
    param[i].done          = &done;
    param[i].retcode       = &retcode;
    param[i].retcode_cs    = &retcode_cs;
    param[i].thread.Create (multi_decompress_thread, &param[i]);
  }

  done.Wait();    // wait for error or finish of last thread

  // Wait until all threads will be finished and return errcode or 0 at success
  for (int i=0; i<N; i++)
    param[i].thread.Wait();
    //printf("\nreleased %d    ", i);
  return retcode;
}


// Один тред распаковки в multi_decompress
static THREAD_FUNC_RET_TYPE THREAD_FUNC_CALL_TYPE multi_decompress_thread (void *paramPtr)
{
  Params *param = (Params*) paramPtr;
  // Не запускать этот thread, пока не начался вывод из предыдущего (для экономии памяти)
  if (param->thread_num > 0)
    param->read.Wait(),           // ожидаем разрешения на чтение (появления данных в буфере)
    param->read.Release();        // возвращаем разрешение на чтение
  //printf("\nstarted %d    ", param->thread_num);
  int old_priority = BeginCompressionThreadPriority();  // понизить приоритет треда на время выполнения распаковки
  int ret = Decompress (param->method, multi_decompress_callback, param);
  EndCompressionThreadPriority (old_priority);
  // Abort multi_decompress if decompress() returned error code
  if (ret<0)
    param->SetExitCode (ret);
  // Tell the previous thread that no more data required
  if (param->thread_num > 0)
    param[-1].size = -1,
    param[-1].write.Release();
  // Tell the next thread that no more data will be supplied to it
  if (param->thread_num < param->threads_total-1)
    param->size = -1,
    param[+1].read.Release();
  // If the last thread finished then no more data will be output, so we can finish multi_decompress
  if (param->thread_num == param->threads_total-1)
    param->SetExitCode(0);
  //printf("\nfinished %d    ", param->thread_num);
  return 0;
}


// Callback-функция чтения/записи для multi_decompress_thread
static int multi_decompress_callback (const char *what, void *_buf, int size, void *paramPtr)
{
  Params *param = (Params*) paramPtr;
  BYTE *buf = (BYTE*)_buf;
  //printf("\n%s %d........  ", what, param->thread_num);

  // Запись данных в следующий тред
  if (strequ(what,"write")  &&  param->thread_num < param->threads_total-1)
  {
    param->buf  = buf;
    param->size = size;
    param[+1].read.Release();   // даём разрешение на чтение (в буфере появились данные)
    param->write.Wait();        // ожидаем разрешения на выход (после того, как все данные будут прочитаны)
    //printf("\n%s %d -> %d  ", what, param->thread_num, param->size<0? -1 : size);
    return param->size<0? FREEARC_ERRCODE_NO_MORE_DATA_REQUIRED : size;
  }

  // Чтение данных из предыдущего треда
  else if (strequ(what,"read")  &&  param->thread_num > 0)
  {
    int prev=0;
loop:
    //if (size==0)  return prev;
    param->read.Wait();             // ожидаем разрешения на чтение (появления данных в буфере)
    if (param[-1].size < 0)         // данных больше не будет - предыдущий тред завершён
    {
      param->read.Release();        // возвращаем разрешение на чтение
      //printf("\n%s %d -> %d  ", what, param->thread_num, prev);
      return prev;
    }
    else if (size <= param[-1].size)
    {
      memcpy (buf, param[-1].buf, size);
      param[-1].buf  += size;
      param[-1].size -= size;
      param->read.Release();        // возвращаем разрешение на чтение
      //printf("\n%s %d -> %d  ", what, param->thread_num, prev+size);
      return prev+size;
    }
    else // param[-1].size < size
    {
      memcpy (buf, param[-1].buf, param[-1].size);
      buf  += param[-1].size;
      size -= param[-1].size;
      prev += param[-1].size;
      param[-1].write.Release();    // даём разрешение на выход из записи (буфер пуст)
      goto loop;
    }
  }

  // Чтение в первом треде, запись в последнем,
  // а также все неизвестные науке запросы передаются на выполнение в оригинальный callback
  else
  {
    int n = param->callback (what, buf, size, param->auxdata);
    //printf("\n%s %d -> %d  ", what, param->thread_num, n);
    return n;
  }
}


// ****************************************************************************************************************************
// ВЫЧИСЛЕНИЕ CRC-32                                                                                                          *
// ****************************************************************************************************************************

#define kCrcPoly 0xEDB88320

#ifndef FREEARC_DLL

uint32 CRCTab[256];

void InitCRC()
{
  for (int I=0;I<256;I++)
  {
    uint C=I;
    for (int J=0;J<8;J++)
      C=(C & 1) ? (C>>1)^kCrcPoly : (C>>1);
    CRCTab[I]=C;
  }
}

uint32 UpdateCRC (void *Addr, uint Size, uint32 crc)
{
  static FARPROC f  =  LoadFromDLL ("UpdateCRC");
  if (f)   return ((uint32 (__cdecl *)(void*, uint, uint32)) f) (Addr, Size, crc);

  if (CRCTab[1]==0)
    InitCRC();
  uint8 *Data = (uint8 *)Addr;
#if defined(FREEARC_INTEL_BYTE_ORDER)
  while (Size>=8)
  {
    crc ^= *(uint32*)Data;
    crc = CRCTab [(uint8)crc] ^ (crc>>8);
    crc = CRCTab [(uint8)crc] ^ (crc>>8);
    crc = CRCTab [(uint8)crc] ^ (crc>>8);
    crc = CRCTab [(uint8)crc] ^ (crc>>8);
    crc ^= *(uint32*)(Data+4);
    crc = CRCTab [(uint8)crc] ^ (crc>>8);
    crc = CRCTab [(uint8)crc] ^ (crc>>8);
    crc = CRCTab [(uint8)crc] ^ (crc>>8);
    crc = CRCTab [(uint8)crc] ^ (crc>>8);
    Data+=8;
    Size-=8;
  }
#endif
  for (int I=0;I<Size;I++)
    crc=CRCTab[(uint8)(crc^Data[I])]^(crc>>8);
  return crc;
}

#else

#define CRC_NUM_TABLES 8

uint32 g_CrcTable[256 * CRC_NUM_TABLES];

void CrcGenerateTable()
{
  uint32 i;
  for (i = 0; i < 256; i++)
  {
    uint32 r = i;
    int j;
    for (j = 0; j < 8; j++)
      r = (r >> 1) ^ (kCrcPoly & ~((r & 1) - 1));
    g_CrcTable[i] = r;
  }
  #if CRC_NUM_TABLES > 1
  for (; i < 256 * CRC_NUM_TABLES; i++)
  {
    uint32 r = g_CrcTable[i - 256];
    g_CrcTable[i] = g_CrcTable[r & 0xFF] ^ (r >> 8);
  }
  #endif
}

extern "C" uint32 __fastcall CrcUpdateT8(uint32 StartCRC, void *data, uint size, const uint32 *table);

uint32 UpdateCRC (void *Addr, uint Size, uint32 StartCRC)
{
  if (g_CrcTable[1]==0)
    CrcGenerateTable();
  return CrcUpdateT8 (StartCRC, Addr, Size, g_CrcTable);
}

#endif


// Вычислить CRC блока данных
uint32 CalcCRC (void *Addr, uint Size)
{
  return UpdateCRC(Addr, Size, INIT_CRC) ^ INIT_CRC;
}



// ****************************************************************************************************************************
// УТИЛИТЫ                                                                                                                    *
// ****************************************************************************************************************************

// Разбить COMPRESSOR на отдельные алгоритмы сжатия/шифрования
//void splitCompressor (COMPRESSOR c, ARRAY<CMETHOD> &cm)

// Запросить сервис what метода сжатия method
int CompressionService (char *method, char *what, int param, void *data, CALLBACK_FUNC *callback)
{
  COMPRESSION_METHOD *compressor = ParseCompressionMethod (method);
  if (compressor){
    int result = compressor->doit (what, param, data, callback);
    delete compressor;
    return result;}
  else
    return FREEARC_ERRCODE_INVALID_COMPRESSOR;
}

// Проверить, что данный компрессор включает алгоритм шифрования
int compressorIsEncrypted (COMPRESSOR c)
{
  // Разобьём компрессор на отдельные алгоритмы и поищем среди них алгоритм шифрования
  CMETHOD arr[MAX_METHODS_IN_COMPRESSOR];
  split (c, COMPRESSION_METHODS_DELIMITER, arr, MAX_METHODS_IN_COMPRESSOR);
  for (CMETHOD *cm=arr; *cm; cm++)
    if (CompressionService (*cm, "encryption?") == 1)  return TRUE;
  return FALSE;
}

// Вычислить, сколько памяти нужно для распаковки данных, сжатых этим компрессором
MemSize compressorGetDecompressionMem (COMPRESSOR c)
{
  // Разобьём компрессор на отдельные алгоритмы и просуммируем их требования к памяти
  CMETHOD arr[MAX_METHODS_IN_COMPRESSOR];
  split (c, COMPRESSION_METHODS_DELIMITER, arr, MAX_METHODS_IN_COMPRESSOR);
  MemSize sum=0;
  for (CMETHOD *cm=arr; *cm; cm++)
    sum += CompressionService (*cm, "GetDecompressionMem");
  return sum;
}


// Get/set number of threads used for (de)compression. 0 means "autodetect"
static int CompressionThreads = 1;
int  __cdecl GetCompressionThreads (void)         {return CompressionThreads;}
void __cdecl SetCompressionThreads (int threads)
{
  CompressionThreads = threads==0? 1 : threads;
#ifndef FREEARC_DLL
  static FARPROC f  = LoadFromDLL ("SetCompressionThreads");
  static FARPROC f2 = LoadFromDLL ("SetCompressionThreads", TRUE);
  if (f)   ((void (__cdecl *)(int)) f)  (threads);
  if (f2)  ((void (__cdecl *)(int)) f2) (threads);
#endif
}


// Used in 4x4 only: read entire input buffer before compression begins, allocate output buffer large enough to hold entire compressed output
int compress_all_at_once = 0;
void __cdecl Set_compress_all_at_once (int n)
{
  compress_all_at_once = n;
#ifndef FREEARC_DLL
  static FARPROC f  = LoadFromDLL ("Set_compress_all_at_once");
  static FARPROC f2 = LoadFromDLL ("Set_compress_all_at_once", TRUE);
  if (f)   ((void (__cdecl *)(int)) f)  (n);
  if (f2)  ((void (__cdecl *)(int)) f2) (n);
#endif
}


// Load accelerated function either from facompress.dll or facompress_mt.dll
FARPROC LoadFromDLL (char *funcname, int only_facompress_mt)
{
#ifdef FREEARC_WIN  // Non-Windows platforms aren't yet supported
  static bool loaded = FALSE;
  static HMODULE dll = NULL,  dll2 = NULL;

  if (!loaded)
  {
    loaded = TRUE;

    // Get program's executable filename
    wchar_t path[MY_FILENAME_MAX];
    GetModuleFileNameW (NULL, path, MY_FILENAME_MAX);

    // Load facompress.dll from the same directory as executable
    wchar_t *basename = _tcsrchr (path,L'\\')+1;
    _tcscpy (basename, L"facompress.dll");
    dll = LoadLibraryW(path);

    // Load facompress_mt.dll from the same directory as executable
    _tcscpy (basename, L"facompress_mt.dll");
    dll2 = LoadLibraryW(path);
  }

  FARPROC f = GetProcAddress (dll, funcname);
  return f && !only_facompress_mt? f : GetProcAddress (dll2, funcname);
#else
  return NULL;
#endif
}

// This function cleans up the Compression Library
void compressionLib_cleanup (void)
{
  removeTemporaryFiles();
}


// ****************************************************************************************************************************
// ПОДДЕРЖКА ТАБЛИЦЫ ЗАРЕГИСТРИРОВАННЫХ РАЗБОРЩИКОВ МЕТОДОВ СЖАТИЯ И ПОИСК В ЭТОЙ ТАБЛИЦЕ РЕАЛИЗАЦИИ ЧИСТО КОНКРЕТНОГО МЕТОДА *
// ****************************************************************************************************************************

template <class PARSER>
struct Parser
{
  PARSER  parser;
  void*   data;
};


int cmCount = 0;                                       // Кол-во зарегистрированных методов сжатия
Parser<CM_PARSER>  cmTable[MAX_COMPRESSION_METHODS];   // Таблица, в которую записываются все зарегистрированные парсеры методов сжатия

// Добавить новый метод в список поддерживаемых методов сжатия
int AddCompressionMethod (CM_PARSER parser)
{
  CHECK (cmCount < elements(cmTable), (s,"INTERNAL ERROR: Overflow of compression methods table"));
  cmTable[cmCount++].parser = parser;
  return 0;
}


int cmExternalCount = 0;                                       // Кол-во зарегистрированных внешних методов сжатия
Parser<CM_PARSER2> cmExternalTable[MAX_COMPRESSION_METHODS];   // Таблица, в которую записываются все зарегистрированные парсеры внешних методов сжатия

// Очистить таблицу внешних упаковщиков
void ClearExternalCompressorsTable (void)
{
  static int builtins = -1;  if (builtins<0)  builtins=cmExternalCount;
  cmExternalCount = builtins;  // Оставим только встроенные описания внешних упаковщиков
}

// Добавить парсер метода с дополнительным параметром, который должен быть передан этому парсеру
int AddExternalCompressionMethod (CM_PARSER2 parser, void *data)
{
  CHECK (cmExternalCount < elements(cmExternalTable), (s,"INTERNAL ERROR: Overflow of external compression methods table"));
  cmExternalTable[cmExternalCount].parser = parser;
  cmExternalTable[cmExternalCount].data   = data;
  cmExternalCount++;
  return 0;
}


// Сконструировать объект класса COMPRESSION_METHOD, реализующий метод, заданный в виде строки `method`
COMPRESSION_METHOD *ParseCompressionMethod (char* method)
{
  // Превратим строку метода сжатия в массив строк `parameters`, хранящий его название и параметры
  char* parameters [MAX_PARAMETERS];
  char  local_method [MAX_METHOD_STRLEN];
  strncopy (local_method, method, sizeof (local_method));
  split (local_method, COMPRESSION_METHOD_PARAMETERS_DELIMITER, parameters, MAX_PARAMETERS);

  // Переберём все зарегистрированные парсеры методов сжатия и найдём тот, который сможет опознать `parameters`
  iterate_var (i, cmExternalCount)  {
     COMPRESSION_METHOD *m = (*cmExternalTable[i].parser) (parameters, cmExternalTable[i].data);
     if (m)  return m;
  }
  iterate_var (i, cmCount)  {
     COMPRESSION_METHOD *m = (*cmTable[i].parser) (parameters);
     if (m)  return m;
  }
  return NULL;   // Полученный метод сжатия не опознан ни одним из парсеров
}


// ***********************************************************************************************************************
// Реализация класса STORING_METHOD                                                                                      *
// ***********************************************************************************************************************

// Функция "(рас)паковки", копирующая данные один в один
int copy_data (CALLBACK_FUNC *callback, void *auxdata)
{
  char buf[BUFFER_SIZE]; int len;
  while ((len = callback ("read", buf, BUFFER_SIZE, auxdata)) > 0) {
    if ((len = callback ("write", buf, len, auxdata)) < 0)  break;
  }
  return len;
}

// Функция распаковки
int STORING_METHOD::decompress (CALLBACK_FUNC *callback, void *auxdata)
{
  return copy_data (callback, auxdata);
}

#ifndef FREEARC_DECOMPRESS_ONLY

// Функция упаковки
int STORING_METHOD::compress (CALLBACK_FUNC *callback, void *auxdata)
{
  return copy_data (callback, auxdata);
}

// Записать в buf[MAX_METHOD_STRLEN] строку, описывающую метод сжатия (функция, обратная к parse_STORING)
void STORING_METHOD::ShowCompressionMethod (char *buf, bool purify)
{
  sprintf (buf, "storing");
}

#endif  // !defined (FREEARC_DECOMPRESS_ONLY)

// Конструирует объект типа STORING_METHOD или возвращает NULL, если это другой метод сжатия
COMPRESSION_METHOD* parse_STORING (char** parameters)
{
  if (strcmp (parameters[0], "storing") == 0
      &&  parameters[1]==NULL )
    // Если название метода - "storing" и параметров у него нет, то это наш метод
    return new STORING_METHOD;
  else
    return NULL;   // Это не метод storing
}

static int STORING_x = AddCompressionMethod (parse_STORING);   // Зарегистрируем парсер метода STORING_METHOD

// ***********************************************************************************************************************
// Реализация класса CRC_METHOD                                                                                          *
// ***********************************************************************************************************************

#ifndef FREEARC_DECOMPRESS_ONLY
// Функция упаковки, просто "съедающая" входные жанные
int CRC_METHOD::compress (CALLBACK_FUNC *callback, void *auxdata)
{
  char buf[BUFFER_SIZE]; int len;
  while ((len = callback ("read", buf, BUFFER_SIZE, auxdata)) > 0);
  return len;
}
#endif  // !defined (FREEARC_DECOMPRESS_ONLY)

// Конструирует объект типа CRC_METHOD или возвращает NULL, если это другой метод сжатия
COMPRESSION_METHOD* parse_CRC (char** parameters)
{
  if (strcmp (parameters[0], "crc") == 0
      &&  parameters[1]==NULL )
    // Если название метода - "storing" и параметров у него нет, то это наш метод
    return new CRC_METHOD;
  else
    return NULL;   // Это не метод storing
}

static int CRC_x = AddCompressionMethod (parse_CRC);   // Зарегистрируем парсер метода CRC_METHOD

