#include "Compression.h"

// Распаковать данные заданным методом сжатия и возвратить время работы в секундах
int timed_decompress (COMPRESSION_METHOD *compressor, CALLBACK_FUNC *callback, VOID_FUNC *auxdata)
{
  double time0 = getThreadCPUTime();
  int result = compressor->decompress (callback, auxdata);
  double time1 = getThreadCPUTime(), t;
  if (time0>=0 && time1>=0 && compressor->addtime>=0)
    t = compressor->addtime + time1 - time0;
  else
    t = -1;
  callback ("time", &t, 0, auxdata);
  return result;
}

// Распаковать данные заданным методом сжатия
int decompress (char *method, CALLBACK_FUNC *callback, VOID_FUNC *auxdata)
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
int DecompressWithHeader (CALLBACK_FUNC *callback, VOID_FUNC *auxdata)
{
  char method [MAX_METHOD_STRLEN];
  for (int i=0; i<MAX_METHOD_STRLEN; i++)
  {
    // Посимвольно читаем входные данные, пока не прочтём символ конца строки
    callback ("read", &method[i], 1, auxdata);
    if (method[i]=='\0')
      return decompress (method, callback, auxdata);
  }
  return FREEARC_ERRCODE_INVALID_COMPRESSOR;  // Сюда мы попадаем, если в первых MAX_METHOD_STRLEN символах входных данных не нашлось символа '\0'
}

// Callback-функция чтения/записи для (рас)паковки в памяти
void *readPtr;    // текущая позиция читаемых данных
int   readLeft;   // сколько байт ещё осталось во входном буфере
void *writePtr;   // текущая позиция записываемых данных
int   writeLeft;  // сколько байт ещё осталось в выходном буфере
int ReadWriteMem (char *what, void *buf, int size, VOID_FUNC *callback)
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
  int result = decompress (method, ReadWriteMem, 0);
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
int timed_compress (COMPRESSION_METHOD *compressor, CALLBACK_FUNC *callback, VOID_FUNC *auxdata)
{
  double time0 = getThreadCPUTime();
  int result = compressor->compress (callback, auxdata);
  double time1 = getThreadCPUTime(), t;
  if (time0>=0 && time1>=0 && compressor->addtime>=0)
    t = compressor->addtime + time1 - time0;
  else
    t = -1;
  callback ("time", &t, 0, auxdata);
  return result;
}

// Упаковать данные заданным методом сжатия
int compress (char *method, CALLBACK_FUNC *callback, VOID_FUNC *auxdata)
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
int CompressWithHeader (char *method, CALLBACK_FUNC *callback, VOID_FUNC *auxdata)
{
  COMPRESSION_METHOD *compressor = ParseCompressionMethod (method);
  if (compressor){
    char canonical_method [MAX_METHOD_STRLEN];
    compressor->ShowCompressionMethod (canonical_method);
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
  int result = compress (method, ReadWriteMem, 0);
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
int CanonizeCompressionMethod (char *method, char *canonical_method)
{
  COMPRESSION_METHOD *compressor = ParseCompressionMethod (method);
  if (compressor){
    compressor->ShowCompressionMethod (canonical_method);
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
      compressor->ShowCompressionMethod (out_method);                        \
      delete compressor;                                                     \
      return FREEARC_OK;}                                                    \
    else                                                                     \
      return FREEARC_ERRCODE_INVALID_COMPRESSOR;                             \
  }                                                                          \

// Информация о памяти, необходимой для упаковки/распаковки, размере словаря и размере блока.
Generate_Getter(GetCompressionMem)
Generate_Getter(GetDecompressionMem)
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
static int CompressionThreads;
int  GetCompressionThreads (void)         {return CompressionThreads;}
void SetCompressionThreads (int threads)  {CompressionThreads = threads==0? 2 : threads;}


// Table of temporary files that should be deleted on ^Break
static int TemporaryFilesCount=0;
static struct {char *name; FILE* file;}  TemporaryFiles[10];

void registerTemporaryFile (char *name, FILE* file)
{
  unregisterTemporaryFile (name);  // First, delete all existing registrations of the same file
  TemporaryFiles[TemporaryFilesCount].name = name;
  TemporaryFiles[TemporaryFilesCount].file = file;
  TemporaryFilesCount++;
}

void unregisterTemporaryFile (char *name)
{
  iterate_var(i,TemporaryFilesCount)
    if (strequ (TemporaryFiles[i].name, name))
    {
      memmove (TemporaryFiles+i, TemporaryFiles+i+1, (TemporaryFilesCount-(i+1)) * sizeof(TemporaryFiles[i]));
      TemporaryFilesCount--;
      return;
    }
}

// This function cleans up the Compression Library
void compressionLib_cleanup (void)
{
  iterate_var(i,TemporaryFilesCount)
    TemporaryFiles[i].file!=NULL  &&  fclose (TemporaryFiles[i].file),
    remove (TemporaryFiles[i].name);
}


// ****************************************************************************************************************************
// ПОДДЕРЖКА ТАБЛИЦЫ ЗАРЕГИСТРИРОВАННЫХ РАЗБОРЩИКОВ МЕТОДОВ СЖАТИЯ И ПОИСК В ЭТОЙ ТАБЛИЦЕ РЕАЛИЗАЦИИ ЧИСТО КОНКРЕТНОГО МЕТОДА *
// ****************************************************************************************************************************

struct Parser
{
  CM_PARSER  parser;
  CM_PARSER2 parser2;
  void*      data;
};

Parser cmTable[MAX_COMPRESSION_METHODS];   // Таблица, в которую записываются все зарегистрированные парсеры методов сжатия
int cmCount = 0;                           // Кол-во зарегистрированных методов сжатия

// Добавить новый метод в список поддерживаемых методов сжатия
int AddCompressionMethod (CM_PARSER parser)
{
  cmTable[cmCount++].parser = parser;
  CHECK (cmCount <= elements(cmTable), ("\nINTERNAL ERROR: Overflow of compression methods table"));
  return 0;
}

// Добавить парсер метода с дополнительным параметром, который должен быть передан этому парсеру
int AddCompressionMethod2 (CM_PARSER2 parser2, void *data)
{
  cmTable[cmCount].parser  = NULL;
  cmTable[cmCount].parser2 = parser2;
  cmTable[cmCount].data    = data;
  cmCount++;
  CHECK (cmCount <= elements(cmTable), ("\nINTERNAL ERROR: Overflow of compression methods table"));
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
  iterate_var (i, cmCount)  {
     COMPRESSION_METHOD *m = cmTable[i].parser?  (*cmTable[i].parser) (parameters)
                                              :  (*cmTable[i].parser2) (parameters, cmTable[i].data);
     if (m)  return m;
  }
  return NULL;   // Полученный метод сжатия не опознан ни одним из парсеров
}


// ***********************************************************************************************************************
// ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ, УПРОЩАЮЩИЕ ВАМ РЕАЛИЗАЦИЮ ПАРСЕРОВ И ПОКАЗУШНИКОВ РАЗЛИЧНЫХ МЕТОДОВ СЖАТИЯ
// ***********************************************************************************************************************

// Если строка param содержит целое число - возвратить его, иначе установить error=1
MemSize parseInt (char *param, int *error)
{
  MemSize n=0;
  char c = *param=='='? *++param : *param;
  while (c>='0' && c<='9')  n=n*10+c-'0', c=*++param;
  if (c!='\0') *error=1;
  return n;
}

// Аналогично readInt, только строка param может содержать суффиксы b/k/m/g/^, что означает соответствующие единицы памяти (по умолчанию - '^', т.е. степень двойки)
MemSize parseMem (char *param, int *error)
{
  MemSize n=0;
  char c = *param=='='? *++param : *param;
  while (c>='0' && c<='9')  n=n*10+c-'0', c=*++param;
  switch (c)
  {
    case 'b':  return n;
    case 'k':  return n*kb;
    case 'm':  return n*mb;
    case 'g':  return n*gb;
    case '^':
    case '\0':  return 1<<n;
  }
  *error=1; return 0;
}

// Возвращает текстовое описание объёма памяти
void showMem (MemSize mem, char *result)
{
       if (mem%gb==0) sprintf (result, "%dgb", mem/gb);
  else if (mem%mb==0) sprintf (result, "%dmb", mem/mb);
  else if (mem%kb==0) sprintf (result, "%dkb", mem/kb);
  else                sprintf (result, "%ub",  mem);
}

// Копирует строчку from в to, но не более len символов
void strncopy( char *to, char *from, int len ) {
  for (int i = len; --i && *from; )     *to++ = *from++;
  *to = '\0';
}

// Разбить строку str на подстроки, разделённые символом splitter.
// Результат - в строке str splitter заменяется на '\0'
//   и массив result заполняется ссылками на выделенные в str подстроки + NULL (аналогично argv)
// Возвращает число найденных подстрок
int split (char *str, char splitter, char **result_base, int result_size)
{
  char **result      = result_base;
  char **result_last = result_base+result_size-1;
  *result++ = str;
  while (*str && result < result_last)
  {
    while (*str && *str!=splitter) str++;
    if (*str) {
      *str++ = '\0';
      *result++ = str;
    }
  }
  *result = NULL;
  return result-result_base;
}

// Заменяет в строке original все вхождения from на to,
// возвращая вновь выделенную new строку и освобождая оригинал, если была хоть одна замена
char *subst (char *original, char *from, char *to)
{
  while(1) {
    char *p = strstr (original, from);
    if (!p)  return original;
    char *newstr = new char[strlen(original)+strlen(to)-strlen(from)+1];
    memcpy (newstr, original, p-original);
    strcpy (newstr+(p-original), to);
    strcat (newstr+(p-original), p+strlen(from));
    delete (original);
    original = newstr;
  }
}


// ***********************************************************************************************************************
// Реализация класса STORING                                                                                             *
// ***********************************************************************************************************************

// Функция "(рас)паковки", копирующая данные один в один
int copy_data (CALLBACK_FUNC *callback, VOID_FUNC *auxdata)
{
  char buf[BUFFER_SIZE]; int len;
  while ((len = callback ("read", buf, BUFFER_SIZE, auxdata)) > 0) {
    if ((len = callback ("write", buf, len, auxdata)) < 0)  break;
  }
  return len;
}

// Функция распаковки
int STORING_METHOD::decompress (CALLBACK_FUNC *callback, VOID_FUNC *auxdata)
{
  return copy_data (callback, auxdata);
}

#ifndef FREEARC_DECOMPRESS_ONLY

// Функция упаковки
int STORING_METHOD::compress (CALLBACK_FUNC *callback, VOID_FUNC *auxdata)
{
  return copy_data (callback, auxdata);
}

// Записать в buf[MAX_METHOD_STRLEN] строку, описывающую метод сжатия (функция, обратная к parse_STORING)
void STORING_METHOD::ShowCompressionMethod (char *buf)
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

