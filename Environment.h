#include <time.h>
#include "Compression/Common.h"

#define PRESENT_INT32

#ifdef  __cplusplus
extern "C" {
#endif

#define INIT_CRC 0xffffffff

#ifdef FREEARC_WIN
typedef wchar_t* CFILENAME;
#else
typedef char* CFILENAME;
#endif

#define MY_FILENAME_MAX 4096                               /* maximum length of filename */

void SetFileDateTime (const CFILENAME Filename, time_t t); // Установить время/дату модификации файла
void FormatDateTime (char *buf, int bufsize, time_t t);    // Отформатировать время/дату для команды листинга
void EnvSetConsoleTitle (char *title);                     // Установить заголовок консольного окна
void EnvResetConsoleTitle (void);                          // Восстановить тот заголовок, который был в начале работы программы
CFILENAME GetExeName (void);                               // Вернуть исполняемый файл самой программы
unsigned GetPhysicalMemory (void);                         // Объём физической памяти компьютера
unsigned GetAvailablePhysicalMemory (void);                // Объём свободной физической памяти компьютера
int GetProcessorsCount (void);                             // Общее количество процессоров (точнее, физических ядер) в системе. Используется для определения того, сколько "тяжёлых" вычислительных потоков целесообразно запустить в программе
uint UpdateCRC (void *Addr, uint Size, uint StartCRC);     // Обновить CRC содержимым блока данных
uint CalcCRC (void *Addr, uint Size);                      // Вычислить CRC блока данных
void memxor (char *dest, char *src, uint size);            // От-xor-ить два блока данных
int systemRandomData (char *rand_buf, int rand_size);
void BuildPathTo (FILENAME name);                          // Создать каталоги на пути к name

#ifdef  __cplusplus
}
#endif
