// Common definitions, used by various parts of FreeArc project
#ifndef FREEARC_COMMON_H
#define FREEARC_COMMON_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>
#include <math.h>
#include <time.h>

#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifdef FREEARC_WIN
#include <windows.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

/******************************************************************************
** Базовые определения FREEARC ************************************************
******************************************************************************/
#if !defined(FREEARC_WIN) && !defined(FREEARC_UNIX)
#error "You must define OS!"
#endif

#if !defined(FREEARC_INTEL_BYTE_ORDER) && !defined(FREEARC_MOTOROLA_BYTE_ORDER)
#error "You must define byte order!"
#endif

#ifdef FREEARC_WIN
#define UNSUPPORTED_PATH_DELIMITERS   "/"
#define PATH_DELIMITER                '\\'
#define STR_PATH_DELIMITER            "\\"
#else
#define UNSUPPORTED_PATH_DELIMITERS   ":\\"
#define PATH_DELIMITER                '/'
#define STR_PATH_DELIMITER            "/"
#endif

#define  DIRECTORY_DELIMITERS         "/\\"
#define  ALL_PATH_DELIMITERS          ":/\\"


/******************************************************************************
** Синонимы для простых типов, используемых в программе ***********************
******************************************************************************/
typedef unsigned int         uint,   UINT;
typedef unsigned long        uint32, ulong;
typedef unsigned short int   uint16, ushort;
typedef unsigned char        uint8,  uchar, byte, BYTE;
typedef   signed long        sint32, int32;
typedef   signed short int   sint16, int16;
typedef   signed char        sint8,  int8;

#ifdef __GNUC__
typedef          long long   sint64, int64;
typedef unsigned long long   uint64;
#elif _MSC_EXTENSIONS || _VISUALC || __INTEL_COMPILER || __BORLANDC__ || __WATCOMC__
typedef          __int64     sint64, int64;
typedef unsigned __int64     uint64;
#else
typedef          long long   sint64, int64;
typedef unsigned long long   uint64;
#endif

typedef unsigned             MemSize;          // объём памяти
typedef char*                FILENAME;         // имя файла
#ifdef FREEARC_LARGE_FILES
typedef uint64               FILESIZE;         // размер файла
#else
typedef uint                 FILESIZE;
#endif


/******************************************************************************
** Стандартные определения ****************************************************
******************************************************************************/
#define make4byte(a,b,c,d)       ((a)+256*((b)+256*((c)+256*(((uint32)d)))))
#define iterate(num, statement)  {for( int i=0; i<(num); i++) {statement;}}
#define iterate_var(i, num)      for( int i=0; i<(num); i++)
#define iterate_array(i, array)  for( int i=0; i<(array).size; i++)
#define TRUE                     1
#define FALSE                    0

#define PATH_CHARS               ":/\\"
#define is_path_char(c)          in_set(c, PATH_CHARS)
#define in_set(c, set)           (strchr (set, c ) != NULL)
#define in_set0(c, set)          (memchr (set, c, sizeof(set) ) != 0)
#define str_end(str)             (strchr (str,'\0'))
#define last_char(str)           (str_end(str) [-1])
#define strequ(a,b)              (strcmp((a),(b))==EQUAL)
#define namecmp                  strcasecmp
#define nameequ(s1,s2)           (namecmp(s1,s2)==EQUAL)
#define start_with(str,with)     (strncmp (str, with, strlen(with))==EQUAL)
#define end_with(str,with)       (nameequ (str_end(str)-strlen(with), with))
#define strdup_msg(s)            (strcpy (new char[strlen(s)+1], (s)))
#define find_extension(str)      (find_extension_in_entry (parse_path(str)))
#define mymax(a,b)               ((a)>(b)? (a) : (b))
#define mymin(a,b)               ((a)<(b)? (a) : (b))
#define inrange(x,a,b)           ((a)<=(x) && (x)<(b))
#define elements(arr)            (sizeof(arr)/sizeof(*arr))
#define endof(arr)               ((arr)+elements(arr))
#define zeroArray(arr)           (memset (arr, 0, sizeof(arr)))
#define EQUAL                    0   /* result of strcmp/memcmp for equal strings */

// File operations
#define file_exists(name)        (access((name),0) == 0)

// Read unsigned 16/24/32-bit value at given address
#define value16(p)               (*(uint*)(p) & 0xffff)
#define value24(p)               (*(uint*)(p) & 0xffffff)
#define value32(p)               (*(uint32*)(p))

// Write unsigned 24-bit value to given address
#define setvalue24(p,x)          (*(uint32*)(p) = ((x)&0xffffff)+(*(uint*)(p)&0xff000000))

#ifndef FreeAndNil
#define FreeAndNil(p)            ((p) && (free(p), (p)=NULL))
#endif

#ifndef CHECK
#define CHECK(a,b)               {if (!(a))  printf b, abort();}
#endif

// Include statements marked as debug(..)  only if we enabled debugging
#ifdef DEBUG
#define debug(stmt)  stmt
#else
#define debug(stmt)  ((void)0)
#endif

// Include statements marked as stat(..)  only if we enabled gathering stats
#ifdef STAT
#define stat(stmt)  stmt
#else
#define stat(stmt)  ((void)0)
#endif

// Define default parameter value only when compiled as C++
#ifdef __cplusplus
#define DEFAULT(x,n) x=n
#else
#define DEFAULT(x,n) x
#endif

#define then


// ****************************************************************************************************************************
// FILE I/O *******************************************************************************************************************
// ****************************************************************************************************************************

#if defined(WIN32) || defined(OS2) || defined(MSDOS)
#include <io.h>
#include <fcntl.h>
#define create_dir(name)                        mkdir(name)
#define set_flen(stream,new_size)               chsize( file_no(stream), new_size )
#define get_flen(stream)                        (_filelengthi64(fileno(stream)))
#define myeof(file)                             (feof(file))
#define get_ftime(stream,tstamp)                getftime( file_no(stream), (struct ftime *) &tstamp )
#define set_ftime(stream,tstamp)                setftime( file_no(stream), (struct ftime *) &tstamp )
#define set_binary_mode(file)                   setmode(fileno(file),O_BINARY)
#else
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#define create_dir(name)                        mkdir(name,0777)
#define get_flen(stream)                        (myfilelength( fileno (stream)))
#define set_binary_mode(file)
#define myeof(file)                             (get_flen(file) == ftell(file))
static inline off_t myfilelength (int h)
{
  off_t saved = lseek (h, 0, SEEK_CUR);
  off_t size  = lseek (h, 0, SEEK_END);
  lseek (h, saved, SEEK_SET);
  return size;
}
#endif

#define file_exists(name)                       (access((name),0) == 0)
#define delete_file(name)                       remove(name)
#define rename_file(oldname,newname)            rename(oldname,newname)
#define delete_dir(name)                        rmdir(name)
#define read(file, buf, size)                   fread  (buf, 1, size, file)
#define write(file, buf, size)                  fwrite (buf, 1, size, file)


/******************************************************************************
** A few commonly used functions **********************************************
******************************************************************************/

MemSize parseInt (char *param, int *error);      // Если строка param содержит целое число - возвратить его, иначе установить error=1
MemSize parseMem (char *param, int *error);      // Аналогично, только строка param может содержать суффиксы b/k/m/g/^, что означает соответствующие единицы памяти (по умолчанию - '^', т.е. степень двойки)
void showMem (MemSize mem, char *result);        // Возвращает текстовое описание объёма памяти
void strncopy (char *to, char *from, int len);   // Копирует строчку from в to, но не более len символов
int  split (char *str, char splitter, char **result, int result_size);  // Разбить строку str на подстроки, разделённые символом splitter
char*subst (char *original, char *from, char *to);  // Заменяет в строке original все вхождения from на to

// Round first number *down* to divisible by second one
static inline MemSize roundDown (MemSize a, MemSize b)
{
  return b>1? a/b*b : a;
}

// Round first number *up* to divisible by second one
static inline MemSize roundUp (MemSize a, MemSize b)
{
  return b>1? roundDown(a-1,b)+b : a;
}


// Whole part of number's binary logarithm (please ensure that n>0)
static inline MemSize lb (MemSize n)
{
  MemSize i;
  for (i=0; n>1; i++, n/=2);
  return i;
}

// Эта процедура округляет число к ближайшей сверху степени
// базы, например f(13,2)=16
static inline MemSize roundup_to_power_of (MemSize n, MemSize base)
{
    MemSize result;
    if (n==1)  return 1;
    for (result=base, n--; (n/=base) != 0; result *= base);
    return result;
}

// Заменить символы из множества from на символ to
static inline char *replace (char *str, char* from, char to)
{
  char *p;
  for (p=str; *p; p++)
    if (in_set (*p, from))
      *p = to;
  return str;
}

// Возращает числовое значение символа, рассматриваемого как шестнадцатеричная цифра
static inline int char2int(char c) {return isdigit(c)? c-'0' : tolower(c)-'a';}

#ifdef FREEARC_WIN
// Converts UTF-8 string to UTF-16
static inline void utf8_to_utf16 (char *utf8, WCHAR *utf16)
{
  do {
    BYTE c = utf8[0];   UINT c32;
         if (c<=0x7F)   c32 = c;
    else if (c<=0xBF)   c32 = '?';
    else if (c<=0xDF)   c32 = ((c&0x1F) << 6) +  (utf8[1]&0x3F),  utf8++;
    else if (c<=0xEF)   c32 = ((c&0x0F) <<12) + ((utf8[1]&0x3F) << 6) +  (utf8[2]&0x3F),  utf8+=2;
    else                c32 = ((c&0x0F) <<18) + ((utf8[1]&0x3F) <<12) + ((utf8[2]&0x3F) << 6) + (utf8[3]&0x3F),  utf8+=3;
    // Now c32 represents full 32-bit Unicode char
    if (c32 <= 0xFFFF)  *utf16++ = c32;
    else                c32-=0x10000, *utf16++ = c32/0x400 + 0xd800, *utf16++ = c32%0x400 + 0xdc00;

  } while (*utf8++);
}
#endif

/******************************************************************************
** System dependent high resolution timer functions. **************************
******************************************************************************/

#ifdef FREEARC_WIN

static inline double getGlobalTime (void)      // Returns the number of wall-clock seconds since some moment
{
  __int64 freq, t0;
  if( QueryPerformanceFrequency ((LARGE_INTEGER*)&freq) ) {
    QueryPerformanceCounter ((LARGE_INTEGER*)&t0);
    return ((double)t0)/freq;
  } else {
    return ((double)GetTickCount())/1000;
  }
}

static inline double getThreadCPUTime (void)   // Returns the number of seconds spent in this thread
{
    FILETIME kt, ut, x, y;
    int ok = GetThreadTimes(GetCurrentThread(),&x,&y,&kt,&ut);
    return !ok? -1 : ((double) (((long long)(ut.dwHighDateTime) << 32) + ut.dwLowDateTime)) / 10000000;
}
#endif // Windows


#ifdef FREEARC_UNIX
static inline double getGlobalTime (void)
{
    struct timespec ts;
    int res = clock_gettime(CLOCK_REALTIME, &ts);
    return res? -1 : (ts.tv_sec + ((double)ts.tv_nsec) / 1000000000);
}

static inline double getThreadCPUTime (void)
{
    // clock_gettime() gives us per-thread CPU time.  It isn't
    // reliable on Linux, but it's the best we have.
    struct timespec ts;
    int res = clock_gettime(CLOCK_THREAD_CPUTIME_ID, &ts);
    return res? -1 : (ts.tv_sec + ((double)ts.tv_nsec) / 1000000000);
}
#endif // Unix

#ifdef __cplusplus
}       // extern "C"
#endif

/******************************************************************************
** Bounds-checked arrays ******************************************************
******************************************************************************/

#ifdef DEBUG
#define ARRAYD(type,name,size)  Array<type> name
template <class ELEM>
struct Array
{
    int n;
    ELEM *p;
    Array(int _n)             {n=_n; p=(ELEM*)malloc(sizeof(ELEM)*n);}
    ~Array()                  {free(p);}
    ELEM& operator [](int i)  {CHECK( 0<=i && i<n, ("\n INDEXING ERROR: %d instead of [0,%d)\n", i, n));
                               return p[n];}
    operator void*()          {return p;}
};
#else
#define ARRAYD(type,name,size)  type name[size]
#endif


/******************************************************************************
** END. ***********************************************************************
******************************************************************************/

#endif  // FREEARC_COMMON_H
