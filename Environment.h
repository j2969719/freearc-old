#include <time.h>
#include "Compression/Common.h"

#ifdef  __cplusplus
extern "C" {
#endif

// Environment.cpp
int long_path_size (void);                                 // Максимальная длина имени файла
void FormatDateTime (char *buf, int bufsize, time_t t);    // Отформатировать время/дату для команды листинга
CFILENAME GetExeName (CFILENAME buf, int bufsize);         // Вернуть имя исполняемого файла программы
int MyGetAppUserDataDirectory (CFILENAME buf);             // Вернуть %APPDATA%
uint64   GetPhysicalMemory (void);                         // Объём физической памяти компьютера
unsigned GetMaxMemToAlloc (void);                          // Макс. объём памяти который мы можем выделить в адресном пространстве нашего процесса
unsigned GetTotalMemoryToAlloc (void);                     // Общий объём памяти который мы можем выделить в адресном пространстве нашего процесса
unsigned GetAvailablePhysicalMemory (void);                // Объём свободной физической памяти компьютера
void TestMalloc (void);                                    // Печатает статистику свободной памяти
int GetProcessorsCount (void);                             // Общее количество процессоров (точнее, физических ядер) в системе. Используется для определения того, сколько "тяжёлых" вычислительных потоков целесообразно запустить в программе
int PowerOffComputer();                                    // Инициировать выключение компьютера
void memxor (char *dest, char *src, uint size);            // От-xor-ить два блока данных
int systemRandomData (char *rand_buf, int rand_size);

#ifdef FREEARC_WIN
DWORD RegistryDeleteTree(HKEY hStartKey, LPTSTR pKeyName);  // Delete entrire subtree from Windows Registry
#endif

// GuiEnvironment.cpp
int BrowseForFolder(TCHAR *prompt, TCHAR *in_filename, TCHAR *out_filename);                      // Дать пользователю выбрать каталог
int BrowseForFile(TCHAR *prompt, TCHAR *filters, TCHAR *in_filename, TCHAR *out_filename);        // Дать пользователю выбрать файл
void GuiFormatDateTime (time_t t, char *buf, int bufsize, char *date_format, char *time_format);  // Превратить время/дату файла в строку в соответствии с настройками locale или заданными форматами времени и даты

#ifdef  __cplusplus
}
#endif
