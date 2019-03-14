#include "../Compression.h"

int lzma_compress   (int dictionarySize,
                     int algorithm,
                     int numFastBytes,
                     int matchFinder,
                     int matchFinderCycles,
                     int posStateBits,
                     int litContextBits,
                     int litPosBits,
                     CALLBACK_FUNC *callback,
                     VOID_FUNC *auxdata);

int lzma_decompress (int dictionarySize,
                     int algorithm,
                     int numFastBytes,
                     int matchFinder,
                     int matchFinderCycles,
                     int posStateBits,
                     int litContextBits,
                     int litPosBits,
                     CALLBACK_FUNC *callback,
                     VOID_FUNC *auxdata);


#ifdef __cplusplus

// ���������� ������������ ���������� ������� ������ COMPRESSION_METHOD
class LZMA_METHOD : public COMPRESSION_METHOD
{
public:
  // ��������� ����� ������ ������
  MemSize dictionarySize;
  int     algorithm;
  int     numFastBytes;
  int     matchFinder;
  int     matchFinderCycles;
  int     posStateBits;
  int     litContextBits;
  int     litPosBits;

  // �����������, ������������� ���������� ������ ������ �������� �� ���������
  LZMA_METHOD();

  // ������� ���������� � ��������
  virtual int decompress (CALLBACK_FUNC *callback, VOID_FUNC *auxdata);
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress (CALLBACK_FUNC *callback, VOID_FUNC *auxdata);

  // �������� � buf[MAX_METHOD_STRLEN] ������, ����������� ����� ������ � ��� ��������� (�������, �������� � parse_LZMA)
  virtual void ShowCompressionMethod (char *buf);

  // ��������/���������� ����� ������, ������������ ��� ��������/����������, ������ ������� ��� ������ �����
  virtual MemSize GetCompressionMem     (void);
  virtual MemSize GetDecompressionMem   (void)         {return dictionarySize;}
  virtual MemSize GetDictionary         (void)         {return dictionarySize;}
  virtual MemSize GetBlockSize          (void)         {return 0;}
  virtual void    SetCompressionMem     (MemSize mem);
  virtual void    SetDecompressionMem   (MemSize mem);
  virtual void    SetDictionary         (MemSize dict);
  virtual void    SetBlockSize          (MemSize)      {}
#endif
};

// ��������� ������ ������ ������ LZMA
COMPRESSION_METHOD* parse_LZMA (char** parameters);

#endif  // __cplusplus
