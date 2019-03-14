// C_LZMA.cpp - интерфейс FreeArc к алгоритму сжатия LZMA

#ifdef WIN32
#include <windows.h>
#include <initguid.h>
#else
#define INITGUID
#endif

#ifdef FREEARC_WIN
// Enable multithreading support
#define COMPRESS_MF_MT
#endif

extern "C" {
#include "C_LZMA.h"
}

// Включим в один .o файл все необходимые подпрограммы
#include "Common/Alloc.cpp"
#include "7zip/Common/InBuffer.cpp"
#include "7zip/Compress/LZ/LZOutWindow.cpp"
#include "7zip/Compress/LZMA/LZMADecoder.cpp"
#include "7zip/Common/OutBuffer.cpp"
#ifdef FREEARC_DECOMPRESS_ONLY
// Фиктивные значения для констант, используемых только при упаковке
#define kHC4 0
#define kBT4 0
#define FindMatchFinder(_) 0
#else
#include "Common/CRC.cpp"
#include "7zip/Compress/LZ/LZInWindow.cpp"
#ifdef FREEARC_WIN
#include "Windows/Synchronization.cpp"
#include "7zip/Compress/LZ/MT/MT.cpp"
#endif
#include "7zip/Compress/RangeCoder/RangeCoderBit.cpp"
#include "7zip/Compress/LZMA/LZMAEncoder.cpp"
#endif  // !defined (FREEARC_DECOMPRESS_ONLY)

using namespace NCompress::NLZMA;


class CallbackInStream:
  public ISequentialInStream,
  public CMyUnknownImp
{
  CALLBACK_FUNC *callback;
  VOID_FUNC     *auxdata;
public:
  int errcode;
  STDMETHOD_(ULONG, AddRef)()  { return 1; }
  STDMETHOD_(ULONG, Release)() { return 1; }
  STDMETHOD(QueryInterface)(REFGUID, void **) { return E_NOINTERFACE; }

  CallbackInStream(CALLBACK_FUNC *_callback, VOID_FUNC *_auxdata) : callback(_callback), auxdata(_auxdata), errcode(0) {}
  virtual ~CallbackInStream() {}
  STDMETHOD(Read)(void *data, UInt32 size, UInt32 *processedSize);
  STDMETHOD(ReadPart)(void *data, UInt32 size, UInt32 *processedSize);
};

STDMETHODIMP CallbackInStream::Read(void *data, UInt32 size, UInt32 *processedSize)
{
  if(processedSize != NULL)
    *processedSize = 0;
  ssize_t res = callback ("read", data, mymin( (size_t) size, BUFFER_SIZE), auxdata);
  if (res < 0) {
    errcode = res;
    return E_FAIL;
  }
  if(processedSize != NULL)
    *processedSize = (UInt32)res;
  return S_OK;
}

STDMETHODIMP CallbackInStream::ReadPart(void *data, UInt32 size, UInt32 *processedSize)
{
  return Read(data, size, processedSize);
}



class CallbackOutStream:
  public ISequentialOutStream,
  public CMyUnknownImp
{
  CALLBACK_FUNC *callback;
  VOID_FUNC     *auxdata;
public:
  int errcode;
  STDMETHOD_(ULONG, AddRef)()  { return 1; }
  STDMETHOD_(ULONG, Release)() { return 1; }
  STDMETHOD(QueryInterface)(REFGUID, void **) { return E_NOINTERFACE; }

  CallbackOutStream(CALLBACK_FUNC *_callback, VOID_FUNC *_auxdata) : callback(_callback), auxdata(_auxdata), errcode(0) {}
  virtual ~CallbackOutStream() {}
  STDMETHOD(Write)(const void *data, UInt32 size, UInt32 *processedSize);
  STDMETHOD(WritePart)(const void *data, UInt32 size, UInt32 *processedSize);
};

STDMETHODIMP CallbackOutStream::Write(const void *data, UInt32 size, UInt32 *processedSize)
{
  if(processedSize != NULL)
    *processedSize = 0;
  ssize_t res = callback ("write", (void*)data, (size_t)size, auxdata);
  if (res < 0) {
    errcode = res;
    return E_FAIL;
  }
  if(processedSize != NULL)
    *processedSize = (UInt32)size;
  return S_OK;
}

STDMETHODIMP CallbackOutStream::WritePart(const void *data, UInt32 size, UInt32 *processedSize)
{
  return Write(data, size, processedSize);
}


#ifndef FREEARC_DECOMPRESS_ONLY

int lzma_compress  ( int dictionarySize,
                     int algorithm,
                     int numFastBytes,
                     int matchFinder,
                     int matchFinderCycles,
                     int posStateBits,
                     int litContextBits,
                     int litPosBits,
                     CALLBACK_FUNC *callback,
                     VOID_FUNC *auxdata )
{
  CallbackInStream  inStream  (callback, auxdata);
  CallbackOutStream outStream (callback, auxdata);
  NCompress::NLZMA::CEncoder* encoder = new NCompress::NLZMA::CEncoder;

  bool eos = (1==1);  // use End-Of-Stream marker because we don't know filesize apriori

  if (encoder->SetupProperties (dictionarySize,
                                posStateBits,
                                litContextBits,
                                litPosBits,
                                algorithm,
                                numFastBytes,
                                matchFinder,
                                matchFinderCycles,
                                GetCompressionThreads() > 1,
                                eos) != S_OK)
  {
    delete encoder;
    return FREEARC_ERRCODE_INVALID_COMPRESSOR;
  }

  HRESULT result = encoder->Code (&inStream, &outStream, 0, 0, 0);
  delete encoder;
  if (inStream.errcode)
    return inStream.errcode;
  if (outStream.errcode)
    return outStream.errcode;
  if (result == E_OUTOFMEMORY)
  {
    return FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;
  }
  else if (result != S_OK)
  {
    //fprintf(stderr, "\nEncoder error = %X\n", (unsigned int)result);
    return FREEARC_ERRCODE_GENERAL;
  }
  return 0;
}

#endif  // !defined (FREEARC_DECOMPRESS_ONLY)

int lzma_decompress( int dictionarySize,
                     int algorithm,
                     int numFastBytes,
                     int matchFinder,
                     int matchFinderCycles,
                     int posStateBits,
                     int litContextBits,
                     int litPosBits,
                     CALLBACK_FUNC *callback,
                     VOID_FUNC *auxdata )
{
  CallbackInStream  inStream  (callback, auxdata);
  CallbackOutStream outStream (callback, auxdata);
  NCompress::NLZMA::CDecoder* decoder = new NCompress::NLZMA::CDecoder;

  if (decoder->SetupProperties(dictionarySize, posStateBits, litContextBits, litPosBits) != S_OK)
  {
    delete decoder;
    return 1;
  }

  UInt64 fileSize = (UInt64)-1;

  HRESULT result = decoder->Code(&inStream, &outStream, 0, &fileSize, 0);
  delete decoder;
  if (inStream.errcode)
    return inStream.errcode;
  if (outStream.errcode)
    return outStream.errcode;
  if (result == E_OUTOFMEMORY)
  {
    return FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;
  }
  if (result != S_OK)
  {
    //fprintf(stderr, "\nDecoder error = %X\n", (unsigned int)result);
    return FREEARC_ERRCODE_GENERAL;
  }
  return 0;
}



/*-------------------------------------------------*/
/* Реализация класса LZMA_METHOD                  */
/*-------------------------------------------------*/

// Если строка str начинается со start, то возвратить адрес остатка, иначе - NULL
char* start_from (char* str, char* start)
{
  while (*start && *str==*start)  str++, start++;
  return *start? NULL : str;
}

// Конструктор, присваивающий параметрам метода сжатия значения по умолчанию
LZMA_METHOD::LZMA_METHOD()
{
  dictionarySize    = 8*mb;
  algorithm         = 1;
  numFastBytes      = 32;
  matchFinder       = kBT4;
  matchFinderCycles = 0;    // библиотека LZMA определит количество циклов автоматически, исходя из matchFinder и numFastBytes
  posStateBits      = 2;
  litContextBits    = 3;
  litPosBits        = 0;
}

// Функция распаковки
int LZMA_METHOD::decompress (CALLBACK_FUNC *callback, VOID_FUNC *auxdata)
{
  return lzma_decompress (dictionarySize,
                          algorithm,
                          numFastBytes,
                          matchFinder,
                          matchFinderCycles,
                          posStateBits,
                          litContextBits,
                          litPosBits,
                          callback,
                          auxdata);
}

#ifndef FREEARC_DECOMPRESS_ONLY

// Функция упаковки
int LZMA_METHOD::compress (CALLBACK_FUNC *callback, VOID_FUNC *auxdata)
{
  // Если LZMA будет использовать multithreading matchfinder,
  // то нет смысла считать время работы по основному треду - вместо этого
  // следует использовать wall clock time всего процесса упаковки
  if ((algorithm || matchFinder!=kHC4) && GetCompressionThreads()>1)
      addtime = -1;   // это сигнал на использование wall clock time
  return lzma_compress   (dictionarySize,
                          algorithm,
                          numFastBytes,
                          matchFinder,
                          matchFinderCycles,
                          posStateBits,
                          litContextBits,
                          litPosBits,
                          callback,
                          auxdata);
}

// Записать в buf[MAX_METHOD_STRLEN] строку, описывающую метод сжатия и его параметры (функция, обратная к parse_LZMA)
void LZMA_METHOD::ShowCompressionMethod (char *buf)
{
  char DictionaryStr[100], fcStr[100], pbStr[100], lcStr[100], lpStr[100], mfStr[100];
  showMem (dictionarySize, DictionaryStr);
  LZMA_METHOD defaults;
  sprintf (fcStr, matchFinderCycles!=defaults.matchFinderCycles? ":mc%d" : "", matchFinderCycles);
  sprintf (pbStr, posStateBits     !=defaults.posStateBits     ? ":pb%d" : "", posStateBits);
  sprintf (lcStr, litContextBits   !=defaults.litContextBits   ? ":lc%d" : "", litContextBits);
  sprintf (lpStr, litPosBits       !=defaults.litPosBits       ? ":lp%d" : "", litPosBits);
  strcpy (mfStr, kMatchFinderIDs [matchFinder]);
  for (char *p=mfStr; *p; p++)   *p = tolower(*p);  // strlwr(mfStr);
  sprintf (buf, "lzma:%s:%s:%s:%d%s%s%s%s",
                      DictionaryStr,
                      algorithm==0? "fast": (algorithm==1? "normal":"max"),
                      mfStr,
                      numFastBytes,
                      fcStr,
                      pbStr,
                      lcStr,
                      lpStr);
}

// Посчитать, сколько памяти требуется для упаковки заданным методом
MemSize LZMA_METHOD::GetCompressionMem (void)
{
  switch (matchFinder) {
    case kBT2:    return dictionarySize*9 + dictionarySize/2 + 4*mb;
    case kBT3:    return dictionarySize*11+ dictionarySize/2 + 4*mb;
    case kBT4:    return dictionarySize*11+ dictionarySize/2 + 4*mb;
    case kHC4:    return dictionarySize*7 + dictionarySize/2 + 4*mb;
    default:      return 0;
  }
}

// Ограничить использование памяти при упаковке
void LZMA_METHOD::SetCompressionMem (MemSize mem)
{
  if (mem<=0)  return;
  double mem4 = mymax (double(mem)-4*mb, 0);
  switch (matchFinder) {
    case kBT2:    dictionarySize = (int)floor(mem4/9.5);     break;
    case kBT3:    dictionarySize = (int)floor(mem4/11.5);    break;
    case kBT4:    dictionarySize = (int)floor(mem4/11.5);    break;
    case kHC4:    dictionarySize = (int)floor(mem4/7.5);     break;
  }

  if (dictionarySize<32*kb) {

  // Если словарь получился слишком маленьким - переключимся на самый простой алгоритм сжатия
  //   (на настоящий момент, при одинаковых минимимальных требованиях к памяти
  //     у всех оставшихся алгоритмов, это не имеет никакого смысла):
  //  if (matchFinder!=HC4)
  //    matchFinder = HC4,  SetCompressionMem (mem);
  //  else
      dictionarySize = 32*kb;
  }
  // Округлим словарь вниз до 32 kb, 48 kb, 64 kb...
  uint t = 1 << lb(dictionarySize);
  if (t/2*3 <= dictionarySize)
        dictionarySize = t/2*3;
  else  dictionarySize = t;
}

// Ограничить использование памяти при распаковке
void LZMA_METHOD::SetDecompressionMem (MemSize mem)
{
  if (mem<=0)  return;
  dictionarySize = mem;
  // Округлим словарь вниз до 32 kb, 48 kb, 64 kb...
  uint t = 1 << lb(dictionarySize);
  if (t/2*3 <= dictionarySize)
        dictionarySize = t/2*3;
  else  dictionarySize = t;
}

// Установить размер словаря
void LZMA_METHOD::SetDictionary (MemSize mem)
{
  if (mem<=0)  return;
  dictionarySize = mem;
  // Округлим словарь вверх до 32 kb, 48 kb, 64 kb...
  uint t = 1 << lb(dictionarySize-1);
  if (t/2*3 <= dictionarySize)
        dictionarySize = t*2;
  else  dictionarySize = t/2*3;

  dictionarySize = mymin (dictionarySize, 256*mb);
}

#endif  // !defined (FREEARC_DECOMPRESS_ONLY)

// Конструирует объект типа LZMA_METHOD с заданными параметрами упаковки
// или возвращает NULL, если это другой метод сжатия или допущена ошибка в параметрах
COMPRESSION_METHOD* parse_LZMA (char** parameters)
{
  if (strcmp (parameters[0], "lzma") == 0) {
    // Если название метода (нулевой параметр) - "lzma", то разберём остальные параметры

    LZMA_METHOD *p = new LZMA_METHOD;
    int error = 0;  // Признак того, что при разборе параметров произошла ошибка

    // Переберём все параметры метода (или выйдем раньше при возникновении ошибки при разборе очередного параметра)
    while (*++parameters && !error && p->matchFinder>=0) {
      char *param = *parameters;
           if (start_from (param, "d"))    p->dictionarySize    = parseMem (param+1, &error);
      else if (start_from (param, "a"))    p->algorithm         = parseInt (param+1, &error);
      else if (start_from (param, "fb"))   p->numFastBytes      = parseInt (param+2, &error);
      else if (start_from (param, "mc"))   p->matchFinderCycles = parseInt (param+2, &error);
      else if (start_from (param, "lc"))   p->litContextBits    = parseInt (param+2, &error);
      else if (start_from (param, "lp"))   p->litPosBits        = parseInt (param+2, &error);
      else if (start_from (param, "pb"))   p->posStateBits      = parseInt (param+2, &error);
      else if (start_from (param, "mf"))   p->matchFinder       = FindMatchFinder (param[2]=='='? param+3 : param+2),
                                           p->matchFinder<0 && (error=1);
      else if (strequ (param, "fastest"))  p->algorithm = 0,  p->matchFinder = kHC4,  p->numFastBytes = 6,    p->matchFinderCycles = 1;
      else if (strequ (param, "fast"))     p->algorithm = 0,  p->matchFinder = kHC4,  p->numFastBytes = 32,   p->matchFinderCycles = 0;
      else if (strequ (param, "normal"))   p->algorithm = 1,  p->matchFinder = kBT4,  p->numFastBytes = 32,   p->matchFinderCycles = 0;
      else if (strequ (param, "max"))      p->algorithm = 2,  p->matchFinder = kBT4,  p->numFastBytes = 128,  p->matchFinderCycles = 0;
      else if (strequ (param, "ultra"))    p->algorithm = 2,  p->matchFinder = kBT4,  p->numFastBytes = 128,  p->matchFinderCycles = 10000;
      else {
        // Сюда мы попадаем, если в параметре опущено его наименование
        // Это может быть строка - имя MatchFinder'а, целое число - значение numFastBytes,
        // или обозначение памяти - значение dictionarySize
        int n = FindMatchFinder (param);
        if (n>=0)
          p->matchFinder = n;
        else {
          n = parseInt (param, &error);
          if (!error)  p->numFastBytes = n;
          else         error=0, p->dictionarySize = parseMem (param, &error);
        }
      }
    }
    p->dictionarySize = mymin (p->dictionarySize, 256*mb);
    if (error)  {delete p; return NULL;}  // Ошибка при парсинге параметров метода
    return p;
  } else
    return NULL;   // Это не метод lzma
}

static int LZMA_x = AddCompressionMethod (parse_LZMA);   // Зарегистрируем парсер метода LZMA
