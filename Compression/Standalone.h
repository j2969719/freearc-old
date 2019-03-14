// These definitions are used for compilation of standalone executables
#ifndef STANDALONE_COMPRESSION_H
#define STANDALONE_COMPRESSION_H

/******************************************************************************
** ���������� ������� ������ ��������� ****************************************
******************************************************************************/
#if defined(WIN32) || defined(OS2) || defined(MSDOS)
#include <windows.h>

static LARGE_INTEGER Frequency, PerformanceCountStart, PerformanceCountEnd;
static inline void init_timer (void)
{
    QueryPerformanceFrequency (&Frequency);
    QueryPerformanceCounter (&PerformanceCountStart);
}

static inline double timer (void)
{
    QueryPerformanceCounter (&PerformanceCountEnd);
    return double(PerformanceCountEnd.QuadPart - PerformanceCountStart.QuadPart)/Frequency.QuadPart;
}
#else
#endif

/******************************************************************************
** �������������� ������� *****************************************************
******************************************************************************/
// ���� ������ param �������� ����� ����� - ���������� ���, ����� ���������� error=1
MemSize parseInt (char *param, int *error)
{
  MemSize n=0;
  char c=*param;
  while (c>='0' && c<='9')  n=n*10+c-'0', c=*++param;
  if (c!='\0') *error=1;
  return n;
}

// ���������� readInt, ������ ������ param ����� ��������� �������� b/k/m/g/^, ��� �������� ��������������� ������� ������ (�� ��������� - '^', �.�. ������� ������)
MemSize parseMem (char *param, int *error)
{
  MemSize n=0;
  char c=*param;
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

// ���������� ��������� �������� ������ ������
void showMem (MemSize mem, char *result)
{
       if (mem%gb==0) sprintf (result, "%dgb", mem/gb);
  else if (mem%mb==0) sprintf (result, "%dmb", mem/mb);
  else if (mem%kb==0) sprintf (result, "%dkb", mem/kb);
  else                sprintf (result, "%ub",  mem);
}

#endif
