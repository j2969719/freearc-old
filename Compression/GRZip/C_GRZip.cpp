/*-------------------------------------------------*/
/* GRZipII/libGRZip compressor          libGRZip.c */
/* libGRZip Compression(Decompression) Functions   */
/*-------------------------------------------------*/

/*--
  This file is a part of GRZipII and/or libGRZip, a program
  and library for lossless, block-sorting data compression.

  Copyright (C) 2002-2004 Grebnov Ilya. All rights reserved.

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
  Lesser General Public License for more details.

  Grebnov Ilya, Ivanovo, Russian Federation.
  Ilya.Grebnov@magicssoft.ru, http://magicssoft.ru/

  This program is based on (at least) the work of:
  Juergen Abel, Jon L. Bentley, Edgar Binder,
  Charles Bloom, Mike Burrows, Andrey Cadach,
  Damien Debin, Sebastian Deorowicz, Peter Fenwick,
  George Plechanov, Michael Schindler, Robert Sedgewick,
  Julian Seward, David Wheeler, Vadim Yoockin.

  Normal compression mode:
    Compression     memory use : [7-9]*BlockLen  + 1Mb
    Decompression   memory use : 5*BlockLen      + 1Mb
  Fast compression mode:
    Compression     memory use : 5*BlockLen      + 1Mb
    Decompression   memory use : 5.125*BlockLen  + 1Mb

  For more information on these sources, see the manual.
--*/

#include <math.h>
#include <stdlib.h>
#include <string.h>

extern "C" {
#include "C_GRZip.h"
}
#include "libGRZip.h"
#include "LZP.c"
#include "BWT.c"
#include "ST4.c"
#include "MTF_Ari.c"
#include "WFC_Ari.c"
#include "Rec_Flt.c"

sint32 RESERVED = 0;  // неиспользуемые байты в заголовке заполняются этим значением

#ifndef FREEARC_DECOMPRESS_ONLY

sint32 GRZip_StoreBlock(uint8 * Input ,sint32 Size,
                        uint8 * Output,sint32 Mode)
{
  *(sint32 *)(Output+4)=-1;
  *(sint32 *)(Output+8)=DisableAllButLZP(Mode);
  *(sint32 *)(Output+12)=0;
  *(sint32 *)(Output+16)=Size;
  memcpy(Output+28,Input,Size);
  *(sint32 *)(Output+20)=RESERVED;
  *(sint32 *)(Output+24)=RESERVED;
  return (Size+28);
}

sint32 GRZip_CompressBlock(uint8 * Input ,sint32 Size,
                           uint8 * Output,sint32 Mode)
{
  sint32 SSize=Size;

  *(sint32 *)Output=Size;

  if ((Size<32)||(Size>GRZ_MaxBlockSize))
    return(GRZip_StoreBlock(Input,Size,Output,0));

  if (Size<1024) Mode|=GRZ_Compression_ST4;

  if ((Size>1024)&&((Mode&GRZ_Disable_DeltaFlt)==0))
  {
    sint32 RecMode=GRZip_Rec_Test(Input,Size);
    if (RecMode)
    {
      sint32 NewSize;
      uint8 * Buffer=(uint8 *)malloc(Size+1024);
      if (Buffer==NULL) return(GRZip_StoreBlock(Input,Size,Output,0));
      GRZip_Rec_Encode(Input,Size,Buffer,RecMode); Mode+=GRZ_Disable_DeltaFlt;
      if ((RecMode&1)==1)
      {
        sint32 PartSize=(Size>>1);
        sint32 Result=GRZip_CompressBlock(Buffer,PartSize,Output+28,Mode);
        if (Result<0) {free(Buffer);return(GRZip_StoreBlock(Input,Size,Output,0));}
        NewSize=Result;
        Result=GRZip_CompressBlock(Buffer+PartSize,Size-PartSize,Output+28+NewSize,Mode);
        if (Result<0) {free(Buffer);return(GRZip_StoreBlock(Input,Size,Output,0));}
        NewSize+=Result;
      }
      if ((RecMode&1)==0)
      {
        sint32 PartSize=(Size>>2);
        sint32 Result=GRZip_CompressBlock(Buffer,PartSize,Output+28,Mode);
        if (Result<0) {free(Buffer);return(GRZip_StoreBlock(Input,Size,Output,0));}
        NewSize=Result;
        Result=GRZip_CompressBlock(Buffer+PartSize,PartSize,Output+28+NewSize,Mode);
        if (Result<0) {free(Buffer);return(GRZip_StoreBlock(Input,Size,Output,0));}
        NewSize+=Result;
        Result=GRZip_CompressBlock(Buffer+2*PartSize,PartSize,Output+28+NewSize,Mode);
        if (Result<0) {free(Buffer);return(GRZip_StoreBlock(Input,Size,Output,0));}
        NewSize+=Result;
        Result=GRZip_CompressBlock(Buffer+3*PartSize,Size-3*PartSize,Output+28+NewSize,Mode);
        if (Result<0) {free(Buffer);return(GRZip_StoreBlock(Input,Size,Output,0));}
        NewSize+=Result;
      }
      free(Buffer);

      if (NewSize>=Size) return(GRZip_StoreBlock(Input,Size,Output,0));

      *(sint32 *)(Output+4)=-2;
      *(sint32 *)(Output+8)=RecMode;
      *(sint32 *)(Output+16)=NewSize;
      *(sint32 *)(Output+20)=RESERVED;
      *(sint32 *)(Output+24)=RESERVED;

      return (NewSize+28);
    }
  }

  uint8 * LZPBuffer=(uint8 *)malloc(Size+1024);
  if (LZPBuffer==NULL) return(GRZip_StoreBlock(Input,Size,Output,0));

  if (LZP_Enabled(Mode))
  {
    sint32 Result=GRZip_LZP_Encode(Input,Size,LZPBuffer,Get_LZP_MinMatchLen(Mode),Get_LZP_HT_Size(Mode));
    if (Result==GRZ_NOT_ENOUGH_MEMORY)
    {
      free(LZPBuffer);
      return(GRZip_StoreBlock(Input,Size,Output,0));
    };
    if (Result==GRZ_NOT_COMPRESSIBLE)
    {
      Mode=Disable_LZP(Mode);
      memcpy(LZPBuffer,Input,Size);
      *(sint32 *)(Output+8)=Size;
    }
    else
     { *(sint32 *)(Output+8)=Result,Size=Result;}
  }
  else
  {
    memcpy(LZPBuffer,Input,Size);
    *(sint32 *)(Output+8)=Size;
  }
  sint32 Result;

  for (Result=0;Result<8;Result++) LZPBuffer[Result+Size]=0;
  Size=(Size+7)&(~7);

  if (Mode&GRZ_Compression_ST4)
    Result=GRZip_ST4_Encode(LZPBuffer,Size,LZPBuffer);
  else
    Result=GRZip_BWT_Encode(LZPBuffer,Size,LZPBuffer,Mode&GRZ_BWTSorting_Fast);

  if (Result==GRZ_NOT_ENOUGH_MEMORY)
  {
    if (LZP_Enabled(Mode))
    {
      sint32 Result=GRZip_LZP_Encode(Input,SSize,LZPBuffer,Get_LZP_MinMatchLen(Mode),Get_LZP_HT_Size(Mode));
      if (Result==GRZ_NOT_ENOUGH_MEMORY)
      {
        free(LZPBuffer);
        return(GRZip_StoreBlock(Input,SSize,Output,0));
      };
      Result=GRZip_StoreBlock(LZPBuffer,Result,Output,Mode);
      free(LZPBuffer);
      return (Result);
    }
    free(LZPBuffer);
    return(GRZip_StoreBlock(Input,SSize,Output,0));
  };

  *(sint32 *)(Output+12)=Result;

  if (Mode&GRZ_Compression_MTF)
    Result=GRZip_MTF_Ari_Encode(LZPBuffer,Size,Output+28);
  else
    Result=GRZip_WFC_Ari_Encode(LZPBuffer,Size,Output+28);

  if ((Result==GRZ_NOT_ENOUGH_MEMORY)||(Result==GRZ_NOT_COMPRESSIBLE))
  {
    if (LZP_Enabled(Mode))
    {
      sint32 Result=GRZip_LZP_Encode(Input,SSize,LZPBuffer,Get_LZP_MinMatchLen(Mode),Get_LZP_HT_Size(Mode));
      if (Result==GRZ_NOT_ENOUGH_MEMORY)
      {
        free(LZPBuffer);
        return(GRZip_StoreBlock(Input,SSize,Output,0));
      };
      Result=GRZip_StoreBlock(LZPBuffer,Result,Output,Mode);
      free(LZPBuffer);
      return (Result);
    }
    free(LZPBuffer);
    return(GRZip_StoreBlock(Input,SSize,Output,0));
  };

  *(sint32 *)(Output+4)=Mode;
  *(sint32 *)(Output+16)=Result;
  *(sint32 *)(Output+20)=RESERVED;
  *(sint32 *)(Output+24)=RESERVED;

  free(LZPBuffer);
  return (Result+28);
}

#endif  // !defined (FREEARC_DECOMPRESS_ONLY)

sint32 GRZip_CheckBlockSign(uint8 * Input,sint32 Size)
{
  if (Size<28) return (GRZ_UNEXPECTED_EOF);
  if ((*(sint32 *)(Input+24))!=RESERVED)
    return (GRZ_CRC_ERROR);
  return (GRZ_NO_ERROR);
}

sint32 GRZip_DecompressBlock(uint8 * Input,sint32 Size,uint8 * Output)
{
  if (Size<28) return (GRZ_UNEXPECTED_EOF);
  if ((*(sint32 *)(Input+24))!=RESERVED)
    return (GRZ_CRC_ERROR);
  if ((*(sint32 *)(Input+16))+28>Size) return (GRZ_UNEXPECTED_EOF);
  if ((*(sint32 *)(Input+20))!=RESERVED)
    return (GRZ_CRC_ERROR);
  sint32 Mode=*(sint32 *)(Input+4);
  sint32 Result=*(sint32 *)(Input+16);
  if (Mode==-1)
  {
    Mode=*(sint32 *)(Input+8);
    if (Mode==0)
    {
      memcpy(Output,Input+28,Result);
      return (Result);
    }
    Result=GRZip_LZP_Decode(Input+28,Result,Output,Get_LZP_MinMatchLen(Mode),Get_LZP_HT_Size(Mode));
    return (Result);
  }

  if (Mode==-2)
  {
    sint32 RecMode=*(sint32 *)(Input+8);
              Size=*(sint32 *)(Input);

    uint8 * Buffer=(uint8 *)malloc(Size+1024);
    if (Buffer==NULL) return(GRZ_NOT_ENOUGH_MEMORY);

    uint8 * Tmp=(Input+28);
    sint32  OutputPos=0;

    if ((RecMode&1)==1)
    {
      Result=GRZip_DecompressBlock(Tmp,(*(sint32 *)(Tmp+16))+28,Buffer+OutputPos);
      if (Result<0) {free(Buffer);return Result;};
      OutputPos+=Result;
      Tmp=Tmp+(*(sint32 *)(Tmp+16))+28;
      Result=GRZip_DecompressBlock(Tmp,(*(sint32 *)(Tmp+16))+28,Buffer+OutputPos);
      if (Result<0) {free(Buffer);return Result;};
    }
    if ((RecMode&1)==0)
    {
      Result=GRZip_DecompressBlock(Tmp,(*(sint32 *)(Tmp+16))+28,Buffer+OutputPos);
      if (Result<0) {free(Buffer);return Result;};
      OutputPos+=Result;
      Tmp=Tmp+(*(sint32 *)(Tmp+16))+28;
      Result=GRZip_DecompressBlock(Tmp,(*(sint32 *)(Tmp+16))+28,Buffer+OutputPos);
      if (Result<0) {free(Buffer);return Result;};
      OutputPos+=Result;
      Tmp=Tmp+(*(sint32 *)(Tmp+16))+28;
      Result=GRZip_DecompressBlock(Tmp,(*(sint32 *)(Tmp+16))+28,Buffer+OutputPos);
      if (Result<0) {free(Buffer);return Result;};
      OutputPos+=Result;
      Tmp=Tmp+(*(sint32 *)(Tmp+16))+28;
      Result=GRZip_DecompressBlock(Tmp,(*(sint32 *)(Tmp+16))+28,Buffer+OutputPos);
      if (Result<0) {free(Buffer);return Result;};
    }
    GRZip_Rec_Decode(Buffer,Size,Output,RecMode);
    free(Buffer);
    return (Size);
  }

  uint8 * LZPBuffer=(uint8 *)malloc(*(sint32 *)(Input+8)+1024);
  if (LZPBuffer==NULL) return(GRZ_NOT_ENOUGH_MEMORY);

  sint32 TSize;

  if (Mode&GRZ_Compression_MTF)
    TSize=GRZip_MTF_Ari_Decode(Input+28,LZPBuffer);
  else
    TSize=GRZip_WFC_Ari_Decode(Input+28,(*(sint32 *)(Input+8)),LZPBuffer);

  if (Result==GRZ_NOT_ENOUGH_MEMORY)
  {
    free(LZPBuffer);
    return(GRZ_NOT_ENOUGH_MEMORY);
  };

  Result=*(sint32 *)(Input+12);

  if (Mode&GRZ_Compression_ST4)
    Result=GRZip_ST4_Decode(LZPBuffer,TSize,Result);
  else
    Result=GRZip_BWT_Decode(LZPBuffer,TSize,Result);

  if (Result==GRZ_NOT_ENOUGH_MEMORY)
  {
    free(LZPBuffer);
    return(GRZ_NOT_ENOUGH_MEMORY);
  };

  TSize=*(sint32 *)(Input+8);

  if (LZP_Enabled(Mode))
  {
    sint32 Result=GRZip_LZP_Decode(LZPBuffer,TSize,Output,Get_LZP_MinMatchLen(Mode),Get_LZP_HT_Size(Mode));
    if (Result==GRZ_NOT_ENOUGH_MEMORY)
    {
      free(LZPBuffer);
      return(GRZ_NOT_ENOUGH_MEMORY);
    };
  }
  else
    memcpy(Output,LZPBuffer,TSize);

  free(LZPBuffer);
  return (*(sint32 *)Input);
}

#ifndef FREEARC_DECOMPRESS_ONLY

#define ABS_MaxByte      256
#define ABS_MinBlockSize 24*1024

sint32 GRZip_GetAdaptiveBlockSize(uint8 * Input,sint32 Size)
{
  sint32  TotFreq[ABS_MaxByte];
  sint32     Freq[ABS_MaxByte];

  if (Size<=ABS_MinBlockSize) return Size;

  memset(TotFreq,0,ABS_MaxByte*sizeof(sint32));

  uint8 * SInput=Input;
  uint8 * InputEnd=Input+ABS_MinBlockSize;
  while  (Input<InputEnd) TotFreq[*Input++]++;

  sint32 Pos=ABS_MinBlockSize,BlockSize=ABS_MinBlockSize/2;

  while (Pos+BlockSize<Size)
  {
    memset(Freq,0,ABS_MaxByte*sizeof(sint32));

    sint32 i=0,Sum=BlockSize+(Pos>>1);

    uint8 * Ptr=SInput+Pos;
    uint8 * PtrEnd=Ptr+BlockSize;
    while (Ptr<PtrEnd) Freq[*Ptr++]++;

    double AvgSize=0,RealSize=0;
    for (i=0;i<ABS_MaxByte;i++)
      if (Freq[i])
      {
        sint32 Fr=Freq[i];
        RealSize-=Fr*log10((double)Fr/BlockSize);
         AvgSize-=Fr*log10((double)(Fr+(TotFreq[i]>>1))/Sum);
      }

    if (AvgSize>1.25*RealSize)
       if (BlockSize<256)
         return Pos;
       else
         {BlockSize>>=1;continue;}

    for (i=0;i<ABS_MaxByte;i++) TotFreq[i]+=Freq[i];
    Pos+=BlockSize;
  }
  return Size;
}

#undef ABS_MaxByte
#undef ABS_MinBlockSize


int grzip_compress  ( int Method,
                      int BlockSize,
                      int EnableLZP,
                      int MinMatchLen,
                      int HashSizeLog,
                      int AlternativeBWTSort,
                      int AdaptiveBlockSize,       // использовать переменный размер блока
                      int DeltaFilter,
                      CALLBACK_FUNC *callback,
                      VOID_FUNC *auxdata )
{
  sint32 Mode;
  switch (Method)
  {
    case 1:{Mode=GRZ_Compression_BWT+GRZ_Compression_WFC;break;}
    case 2:{Mode=GRZ_Compression_BWT+GRZ_Compression_MTF;break;}
    case 3:{Mode=GRZ_Compression_ST4+GRZ_Compression_WFC;break;}
    case 4:{Mode=GRZ_Compression_ST4+GRZ_Compression_MTF;break;}
    default: return FREEARC_ERRCODE_INVALID_COMPRESSOR;
  }
  Mode += EnableLZP? Encode_LZP_HT_Size(HashSizeLog)+Encode_LZP_MinMatchLen(MinMatchLen) : GRZ_Disable_LZP;
  Mode += AlternativeBWTSort? GRZ_BWTSorting_Strong : GRZ_BWTSorting_Fast;
  Mode += DeltaFilter? GRZ_Enable_DeltaFlt : GRZ_Disable_DeltaFlt;

  if (BlockSize>GRZ_MaxBlockSize) BlockSize=GRZ_MaxBlockSize;
  uint8* In  = (uint8 *)malloc(BlockSize+1024);    // место для входных данных
  uint8* Out = (uint8 *)malloc(BlockSize+1024);    // место для выходных данных
  if (In==NULL || Out==NULL)   {free(Out); free(In); return FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;}
  int InSize;                                      // количество байт во входном блоке или код ошибки
  uint8* RemainderPos=In; int RemainderSize=0;     // остаток данных с предыдущего раза - адрес и количество
  while ( (InSize = callback ("read", In+RemainderSize, BlockSize-RemainderSize, auxdata)) >= 0 )
  {
    if ((InSize+=RemainderSize)==0)     break;  // Данных больше нет
    RemainderPos=In; RemainderSize=0;
    if (AdaptiveBlockSize)
    {  // Пошукаем статистику прочитанных данных - может, нет смысла сжимать их общим блоком
       sint32 NewSize = GRZip_GetAdaptiveBlockSize(In,InSize);
       // Принято решение сжать только первые NewSize байт. Остальное оставим на следующий раз
       RemainderPos=In+NewSize; RemainderSize=InSize-NewSize; InSize=NewSize;
    }
    int OutSize=GRZip_CompressBlock(In,InSize,Out,Mode);
    // Записать сжатый блок и выйти, если при записи произошла ошибка/больше данных не нужно
    if ((InSize=callback("write",Out,OutSize,auxdata)) < 0)   break;
    // Перенесём необработанный остаток данных в начало буфера
    if (RemainderSize>0)   memmove(In,RemainderPos,RemainderSize);
  }
  free(Out); free(In); return InSize;  // освободим буфера и возвратим код ошибки или 0
}

#endif  // !defined (FREEARC_DECOMPRESS_ONLY)

int grzip_decompress( int Method,
                      int BlockSize,
                      int EnableLZP,
                      int MinMatchLen,
                      int HashSizeLog,
                      int AlternativeBWTSort,
                      int AdaptiveBlockSize,
                      int DeltaFilter,
                      CALLBACK_FUNC *callback,
                      VOID_FUNC *auxdata )
{
  uint8 BlockSign[28];
  while (1)
  {
    sint32 NumRead=callback("read",BlockSign,28,auxdata);
    if (NumRead==0)                                          return FREEARC_OK;    // Конец данных
    if (NumRead!=28)                                         return NumRead<0?NumRead:FREEARC_ERRCODE_BAD_COMPRESSED_DATA;
    if (GRZip_CheckBlockSign(BlockSign,28)!=GRZ_NO_ERROR)    return FREEARC_ERRCODE_BAD_COMPRESSED_DATA;
    uint8 * Input=(uint8 *)malloc(  *(sint32 *)(BlockSign+16) + 1024);
    if (Input==NULL)                                         return FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;
    memcpy(Input,BlockSign,28);
    uint8 * Output=(uint8 *)malloc( *(sint32 *)Input + 1024);
    if (Output==NULL)                                        {free(Input); return FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;}
    NumRead=callback("read",Input+28,*(sint32 *)(Input+16),auxdata);
    if (NumRead!=*(sint32 *)(Input+16))                      {free(Input); free(Output); return NumRead<0?NumRead:FREEARC_ERRCODE_BAD_COMPRESSED_DATA;}
    sint32 Size=GRZip_DecompressBlock(Input,NumRead+28,Output);
    if (Size==GRZ_NOT_ENOUGH_MEMORY)                         {free(Input); free(Output); return FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;}
    if (Size<0)                                              {free(Input); free(Output); return FREEARC_ERRCODE_GENERAL;}
    int x = callback("write",Output,Size,auxdata);  if (x<0) {free(Input); free(Output); return x;}
    free(Input); free(Output);
  }
}


/*-------------------------------------------------*/
/* Реализация класса GRZIP_METHOD                  */
/*-------------------------------------------------*/
// Конструктор, присваивающий параметрам метода сжатия значения по умолчанию
GRZIP_METHOD::GRZIP_METHOD()
{
  Method              = 1;
  BlockSize           = 8*mb;
  EnableLZP           = 1;
  MinMatchLen         = 32;
  HashSizeLog         = 15;
  AlternativeBWTSort  = 0;
  AdaptiveBlockSize   = 0;
  DeltaFilter         = 0;
}

// Функция распаковки
int GRZIP_METHOD::decompress (CALLBACK_FUNC *callback, VOID_FUNC *auxdata)
{
  return grzip_decompress (Method,
                           BlockSize,
                           EnableLZP,
                           MinMatchLen,
                           HashSizeLog,
                           AlternativeBWTSort,
                           AdaptiveBlockSize,
                           DeltaFilter,
                           callback,
                           auxdata);
}

#ifndef FREEARC_DECOMPRESS_ONLY

// Функция упаковки
int GRZIP_METHOD::compress (CALLBACK_FUNC *callback, VOID_FUNC *auxdata)
{
  return grzip_compress (Method,
                         BlockSize,
                         EnableLZP,
                         MinMatchLen,
                         HashSizeLog,
                         AlternativeBWTSort,
                         AdaptiveBlockSize,
                         DeltaFilter,
                         callback,
                         auxdata);
}

// Установить размер блока и уменьшить размер хэша, если он слишком велик для такого маленького блока
void GRZIP_METHOD::SetBlockSize (MemSize bs)
{
  if (bs>0) {
    BlockSize   = mymin (bs, GRZ_MaxBlockSize);
    HashSizeLog = mymin (HashSizeLog, 1+lb(BlockSize-1));
  }
}

// Записать в buf[MAX_METHOD_STRLEN] строку, описывающую метод сжатия и его параметры (функция, обратная к parse_GRZIP)
void GRZIP_METHOD::ShowCompressionMethod (char *buf)
{
  char LZP_Str[100], BlockSizeStr[100];
  sprintf (LZP_Str, "l%d:h%d", MinMatchLen, HashSizeLog);
  showMem (BlockSize, BlockSizeStr);
  sprintf (buf, "grzip:%s:m%d:%s%s%s%s", BlockSizeStr,
                                         Method,
                                         EnableLZP?          LZP_Str : "l",
                                         AlternativeBWTSort? ":s" : "",
                                         AdaptiveBlockSize?  ":a" : "",
                                         DeltaFilter?        ":d" : "");
}

#endif  // !defined (FREEARC_DECOMPRESS_ONLY)

// Конструирует объект типа GRZIP_METHOD с заданными параметрами упаковки
// или возвращает NULL, если это другой метод сжатия или допущена ошибка при задании параметров
COMPRESSION_METHOD* parse_GRZIP (char** parameters)
{
  if (strcmp (parameters[0], "grzip") == 0) {
    // Если название метода (нулевой параметр) - "grzip", то разберём остальные параметры

    GRZIP_METHOD *p = new GRZIP_METHOD;
    int error = 0;  // Признак того, что при разборе параметров произошла ошибка

    while (!error && *++parameters)  // Переберём все параметры метода
    {
      char *param = *parameters;
      if (strlen(param)==1) switch (*param) {    // Однобуквенные параметры
        case 's':  p->AlternativeBWTSort  = 1; continue;
        case 'a':  p->AdaptiveBlockSize   = 1; continue;
        case 'l':  p->EnableLZP           = 0; continue;
        case 'd':  p->DeltaFilter         = 1; continue;
        case 'p':  p->AdaptiveBlockSize=0; p->EnableLZP=0; p->DeltaFilter=1; continue;
      }
      else switch (*param) {                    // Параметры, содержащие значения
        case 'm':  p->Method      = parseInt (param+1, &error); continue;
        case 'b':  p->BlockSize   = parseMem (param+1, &error); continue;
        case 'l':  p->MinMatchLen = parseInt (param+1, &error); continue;
        case 'h':  p->HashSizeLog = parseInt (param+1, &error); continue;
      }
      // Сюда мы попадаем, если в параметре не указано его название
      // Если этот параметр удастся разобрать как целое число (т.е. в нём - только цифры),
      // то присвоим его значение полю MinMatchLen, иначе попробуем разобрать его как BlockSize
      int n = parseInt (param, &error);
      if (!error) p->MinMatchLen = n;
      else        error=0, p->BlockSize = parseMem (param, &error);
    }
    if (error)  {delete p; return NULL;}  // Ошибка при парсинге параметров метода
    return p;
  } else
    return NULL;   // Это не метод grzip
}

static int GRZIP_x = AddCompressionMethod (parse_GRZIP);   // Зарегистрируем парсер метода GRZIP

/*-------------------------------------------------*/
/* End                                  libGRZip.c */
/*-------------------------------------------------*/
