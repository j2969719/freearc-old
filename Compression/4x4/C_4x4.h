#include "../Compression.h"

#ifdef __cplusplus

// Реализация стандартного интерфейса методов сжатия COMPRESSION_METHOD
class _4x4_METHOD : public COMPRESSION_METHOD
{
public:
  // Параметры этого метода сжатия
  bool    ENCODE;                           // True for compression, False - decompression
  char    Method[MAX_METHOD_STRLEN];        // Compression method used for every block
  MemSize BlockSize;                        // Size of chunks input split to
  int     NumCompressionThreads;            // Max. number of threads doing compression
  int     NumIOThreads;                     // Max. number of threads doing I/O

  // Конструктор, присваивающий параметрам метода сжатия значения по умолчанию
  _4x4_METHOD();
  int  TotalThreads();                      // Total amount of threads
  int  GetNumCompressionThreads();          // Number of compression of threads

  // Универсальный метод, отвечает на запрос "has_progress?"
  virtual int doit (char *what, int param, void *data, CALLBACK_FUNC *callback);
  // Функции распаковки и упаковки
  virtual int decompress (CALLBACK_FUNC *callback, void *auxdata);
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress   (CALLBACK_FUNC *callback, void *auxdata);

  // Записать в buf[MAX_METHOD_STRLEN] строку, описывающую метод сжатия и его параметры (функция, обратная к parse_X)
  virtual void ShowCompressionMethod (char *buf, bool purify);

  // Получить/установить объём памяти, используемой при упаковке/распаковке, размер словаря или размер блока
  virtual MemSize GetCompressionMem     (void)         {Set_compress_all_at_once(1); uint64 t = TotalThreads(); MemSize mem =  mymin(t*::GetCompressionMem(Method), MEMSIZE_MAX); Set_compress_all_at_once(0); return mem;}
  virtual MemSize GetDictionary         (void)         {return ::GetDictionary(Method);}
  virtual MemSize GetBlockSize          (void)         {return 0;}
  virtual void    SetCompressionMem     (MemSize mem);
  virtual void    SetDecompressionMem   (MemSize mem);
  virtual void    SetDictionary         (MemSize dict) {::SetDictionary (Method, dict, Method);}
  virtual void    SetBlockSize          (MemSize bs)   {};
#endif
  virtual MemSize GetDecompressionMem   (void)         {Set_compress_all_at_once(1); uint64 t = TotalThreads(); MemSize mem =  mymin(t*::GetDecompressionMem(Method), MEMSIZE_MAX); Set_compress_all_at_once(0); return mem;}
};

// Разборщик строки метода сжатия 4x4
COMPRESSION_METHOD* parse_4x4 (char** parameters);

#endif  // __cplusplus
