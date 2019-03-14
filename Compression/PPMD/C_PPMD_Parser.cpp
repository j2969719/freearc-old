#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>

extern "C" {
#include "C_PPMD.h"
}

/*-------------------------------------------------*/
/* ���������� ������ PPMD_METHOD                  */
/*-------------------------------------------------*/

// �����������, ������������� ���������� ������ ������ �������� �� ���������
PPMD_METHOD::PPMD_METHOD()
{
  order    = 10;
  mem      = 48*mb;
  MRMethod = 0;
}

// ������� ����������
int PPMD_METHOD::decompress (CALLBACK_FUNC *callback, VOID_FUNC *auxdata)
{
  return ppmd_decompress (order, mem, MRMethod, callback, auxdata);
}

#ifndef FREEARC_DECOMPRESS_ONLY

// ������� ��������
int PPMD_METHOD::compress (CALLBACK_FUNC *callback, VOID_FUNC *auxdata)
{
  return ppmd_compress   (order, mem, MRMethod, callback, auxdata);
}

// �������� � buf[MAX_METHOD_STRLEN] ������, ����������� ����� ������ � ��� ��������� (�������, �������� � parse_PPMD)
void PPMD_METHOD::ShowCompressionMethod (char *buf)
{
  char MemStr[100];
  showMem (mem, MemStr);
  sprintf (buf, "ppmd:%d:%s%s", order, MemStr, MRMethod==2? ":r2": (MRMethod==1? ":r":""));
}

// �������� ����������� � ������, ������ ������������ order
void PPMD_METHOD::SetCompressionMem (MemSize _mem)
{
  if (_mem==0)  return;
  order  +=  int (trunc (log(double(_mem)/mem) / log(2) * 4));
  mem = _mem;
}


#endif  // !defined (FREEARC_DECOMPRESS_ONLY)

// ������������ ������ ���� PPMD_METHOD � ��������� ����������� ��������
// ��� ���������� NULL, ���� ��� ������ ����� ������ ��� �������� ������ � ����������
COMPRESSION_METHOD* parse_PPMD (char** parameters)
{
  if (strcmp (parameters[0], "ppmd") == 0) {
    // ���� �������� ������ (������� ��������) - "ppmd", �� ������� ��������� ���������

    PPMD_METHOD *p = new PPMD_METHOD;
    int error = 0;  // ������� ����, ��� ��� ������� ���������� ��������� ������

    // �������� ��� ��������� ������ (��� ������ ������ ��� ������������� ������ ��� ������� ���������� ���������)
    while (*++parameters && !error)
    {
      char *param = *parameters;
      if (start_with (param, "mem")) {
        param+=2;  // ���������� "mem..." ��� "m..."
      }
      if (strlen(param)==1) switch (*param) {    // ������������� ���������
        case 'r':  p->MRMethod = 1; continue;
      }
      else switch (*param) {                    // ���������, ���������� ��������
        case 'm':  p->mem      = parseMem (param+1, &error); continue;
        case 'o':  p->order    = parseInt (param+1, &error); continue;
        case 'r':  p->MRMethod = parseInt (param+1, &error); continue;
      }
      // ���� �� ��������, ���� � ��������� �� ������� ��� ��������
      // ���� ���� �������� ������� ��������� ��� ����� ����� (�.�. � �� - ������ �����),
      // �� �������� ��� �������� ���� order, ����� ��������� ��������� ��� ��� mem
      int n = parseInt (param, &error);
      if (!error) p->order = n;
      else        error=0, p->mem = parseMem (param, &error);
    }
    if (error)  {delete p; return NULL;}  // ������ ��� �������� ���������� ������
    return p;
  } else
    return NULL;   // ��� �� ����� ppmd
}

static int PPMD_x = AddCompressionMethod (parse_PPMD);   // �������������� ������ ������ PPMD
