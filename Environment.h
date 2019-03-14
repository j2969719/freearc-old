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

void SetFileDateTime (const CFILENAME Filename, time_t t); // ���������� �����/���� ����������� �����
void FormatDateTime (char *buf, int bufsize, time_t t);    // ��������������� �����/���� ��� ������� ��������
void EnvSetConsoleTitle (char *title);                     // ���������� ��������� ����������� ����
void EnvResetConsoleTitle (void);                          // ������������ ��� ���������, ������� ��� � ������ ������ ���������
CFILENAME GetExeName (void);                               // ������� ����������� ���� ����� ���������
unsigned GetPhysicalMemory (void);                         // ����� ���������� ������ ����������
unsigned GetAvailablePhysicalMemory (void);                // ����� ��������� ���������� ������ ����������
int GetProcessorsCount (void);                             // ����� ���������� ����������� (������, ���������� ����) � �������. ������������ ��� ����������� ����, ������� "������" �������������� ������� ������������� ��������� � ���������
uint UpdateCRC (void *Addr, uint Size, uint StartCRC);     // �������� CRC ���������� ����� ������
uint CalcCRC (void *Addr, uint Size);                      // ��������� CRC ����� ������
void memxor (char *dest, char *src, uint size);            // ��-xor-��� ��� ����� ������
int systemRandomData (char *rand_buf, int rand_size);
void BuildPathTo (FILENAME name);                          // ������� �������� �� ���� � name

#ifdef  __cplusplus
}
#endif
