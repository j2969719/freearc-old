#include "../Compression.h"

#ifdef __cplusplus

// ���������� ������������ ���������� ������� ������ COMPRESSION_METHOD
class DISPACK_METHOD : public COMPRESSION_METHOD
{
public:
  // ��������� ����� ������ ������
  MemSize BlockSize;        // ������ ����� ������, �������������� �� ���� ���
  int     ExtendedTables;   // ������ ������� � �������� ��������, �������� �� 2/4

  // �����������, ������������� ���������� ������ ������ �������� �� ���������
  DISPACK_METHOD();

  // ������� ���������� � ��������
  virtual int decompress (CALLBACK_FUNC *callback, void *auxdata);
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress   (CALLBACK_FUNC *callback, void *auxdata);

  // �������� � buf[MAX_METHOD_STRLEN] ������, ����������� ����� ������ � ��� ��������� (�������, �������� � parse_DISPACK)
  virtual void ShowCompressionMethod (char *buf, bool purify);

  // ��������/���������� ����� ������, ������������ ��� ��������/����������, ������ ������� ��� ������ �����
  virtual MemSize GetCompressionMem     (void)         {return 3*BlockSize+BlockSize/4+1024;}
  virtual MemSize GetDictionary         (void)         {return 0;}
  virtual MemSize GetBlockSize          (void)         {return 0;}
  virtual void    SetCompressionMem     (MemSize mem)  {if (mem>0)   BlockSize = mymax(mem/13*4,64*kb);}
  virtual void    SetDecompressionMem   (MemSize mem)  {if (mem>0)   BlockSize = mymax(mem/ 9*4,64*kb);}
  virtual void    SetDictionary         (MemSize dict) {}
  virtual void    SetBlockSize          (MemSize bs)   {}
#endif
  virtual MemSize GetDecompressionMem   (void)         {return 2*BlockSize+BlockSize/4+1024;}
};

// ��������� ������ ������ ������ DISPACK
COMPRESSION_METHOD* parse_DISPACK (char** parameters);

#endif  // __cplusplus
