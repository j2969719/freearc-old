/*
    REP is an LZ77-family algorithm, i.e. it founds matches and outputs them as
    (len,offset) pairs. It is oriented toward very fast compression and small
    memory overhead (1/4 of buffer size), but limited to rather large values of
    mimimal match length (say, 32), and don't search for optimum match. It's
    intended to preprocess data before using full-fledged compressors. and in
    this area it beats RZIP and, to some degree, LZP preprocessors. Small
    memory overhead means that RZIP/LZP/REP are capable to find matches at very
    long distances and this algorithm does it much better than RZIP and LZP.
    The algorithm implemented in functions REPEncode() and REPDecode().

    Main differences comparing to RZIP:
    1) Sliding window which slides at 1/16 of buffer size each time
    2) Almost ideal hash function (see update_hash)
    3) Direct hashing without hash chains which 1.5x cuts memory requirements
    4) Tags are not saved in hashtable, which again halves memory requirements.
         Instead, a few lower bits of hash table entry are used to save a few
         bits of tag (see chksum)
    5) Hash size is proportional to buffer size (which is equal to the maximum
         search distance) and by default limited to 1/4 of buffer size
    6) In order to find strings of length >=MinLen, blocks of length L=MinLen/2
         are indexed via hash. Of all those possible blocks, only 1/sqrt(L) are
         indexed and only 1/sqrt(L) are searched. It is alternative to solution
         described in RZIP paper where 1/L of blocks are indexed and each block
         searched. This means that logb(sqrt(L)) lower bits of hash entry are
         zeroes which allows to use trick 4.


References for RZIP algorithm explanation and implementations:
http://samba.org/~tridge/phd_thesis.pdf
http://rzip.samba.org/ftp/rzip/rzip-2.1.tar.gz
http://ck.kolivas.org/apps/lrzip/lrzip-0.18.tar.bz2
http://www.edcassa-ict.nl/lrzip.zip
http://www.edcassa-ict.nl/rzip21.zip

TAYLOR, R., JANA, R., AND GRIGG, M. 1997. Checksum testing of remote
synchronisation tool. Technical Report 0627 (November), Defence Science and
Technology Organisation, Canberra, Australia. (p.72)


References for LZP algorithm implementations:
http://magicssoft.ru/content/download/GRZipII/GRZipIISRC.zip
http://www.compression.ru/ds/lzp.rar


** Detailed algorithm description in Russian **********************************************

    ���� �������� �������� �������������� LZ77, �.�. �� ������� �������������
    ������ �� ������� ������, � �������� �� ��� (len,offset). ��� ������������
    �������� ���������� �� ����� ���������� ���������� ������� ����� �� �������
    ����������. ������� �� ������ ���������� ���������� ������ - ��� �������,
    ��� �������� ������ ��������� �� ����� 25% �� ������� ���� ������. ��� ����
    �� ������� ����������� ��� ���������� ���� ����������� ����� (MinLen)
    ������� ����� - 512 ����, � ������� 98% - � ����� ��� ������������ ��
    ����� ���������� � ������ �� 32 ����. �� �������� ���� �������� ������� ��
    ������������� � �������� �������������, ������������ ������������ �����
    �/��� ���������� ��������� �� ����� ����������, ������� ����������
    ��������� ��������� ��������, � � ���� �������� �� ����������� � ������
    �����������, ��� LZP by Ilya Grebnev � RZIP. ��� ����, ��� ����������
    ������������, ��� ������������� ����������� �������� ����������� �������
    ������ ��������� ������ � ���� �������� - 32-512 ����. ���� ��������
    ������� ���� ������ ����������, ��� LZP/RZIP, � ����� ����, ���
    �������� ������ ������������� ��� ���������� MinLen.

    �������� ����������� ��������� REPEncode() � REPDecode(), � ����������
    ��������� ���� �� LZP, RZIP � ���� �����������. ����� ���������� ������ �
    ���������� ���� - ������� ������ ����������� ������� �� 1/16 �� �������
    ������, � ��� �������� ��� � ����� ������ ������� ��� ������� 15/16 ������
    �������� ���������� ������, ������� ����������� � ������� ����������. ���
    ��������� ��������� �� ������� �����, �� ���������� �� ����� ����������
    ������� ������.

    ��� ������, ��� ������ ����� � ������ �� MinLen � ������� ����� �����
    ������ MinLen ����������� ����� ����������� ����� (��), ������� ��������� �
    ���-�������. ��������� �������� ������������ �� ������� �������� MinLen,
    ������� ���������� �� �� ������ ����� ����� �������� ���������. ���
    �������� �������� �������������� "���������� ��", �� ���� �����, �������
    ����� ������ ����������� ��� ���������� ������ ����� � ����� ����� �
    �������� ������ ����� � ������ (��. update_hash).

    ������ ��������� ������� ��� ����������� ����������� ��� ���������
    ������������. � ����� ������ ������� ������� hash = p[-1] + PRIME*p[-2] +
    PRIME*PRIME*p[-3] + ..., ��� PRIME - ������� �����, ��������� ����� �������
    � ������ ������ ����������� �������������. ����������, ��� ���������� ����
    �� ������ 1<<32, ������� ���������������� ��� ����������� :)

    �����, ���� ������������ �������������� ���� ��� ���������� ���������� �
    ������ � ���������� ��������. ���������� � ������� ������ ��������� ���
    MinLen=512. ��������� ����� 512-������� ���� �������� � ���� 256-�������
    ����, ������������ � �������, ������� 256, �� ��� ���������� ��������� �
    ���-������� ������ ������ �� ��� ����� � ������ ���������� ������ � ����.
    ����������, ��� �������� ���������� �� �� �������������� � �������� 256
    �������, � �������� ���������� ��� ��� ����� ������ � ��� �������. ������
    ��� � ��������� ����������� ��������� ������ ������ ��� ���������������
    ���������� ����� ���� ���������� - �� ������ ������, ����� MinLen
    ���������� ������.

    ������ ����� ����� ��� ������ - ������ ����, ����� ��������� � ���-�������
    ������ 256-� ����, �� ������ ������-������, �� ����� �������� ���������
    ������ 32-�, � ������ ������ 8-�, ��� ��������� ������ 2-�, � ������ ������
    128-�. ����������, ��������� ����� ��������� � ������ ������ 16-� ����.
    ������ ������, ����� ��������� ���� ���� ����� ������ 16 ����, � ������
    ������ 16 ������ �� ������ 256, �� ���� ��������� �����, ������������ �
    ������� 0, 16, 32..., � ���� �����, ������������ � ������� 0, 1, 2..., 15,
    256. 257... ����� �������, ��� MinLen=512 ����������� 8-������� ���������
    ������ (�� ���� 8-�������� ���������� ���������� ��������� � ������) ��
    ��������� � ������������� ����������� - ������, �� ���� ����������
    ���������� � ������ (� 1/64 ������� ������ �� 1/4, ��� �� ��� ������ ������
    ���������).

    �������, ��������� ������ �������� ������������� ������� ����� ������ �
    ���-������� ��� �������� ���������� ��� �� �������� ���-������� (chksum) -
    ����������, ���, ������� �� �������� ������ ������� � ���-�������. ���
    ��������� ������� ������� ����� ������ ����������, �� ��������� ����������
    ������, � ��� ����� ��������� ���������� ��������� � ������ � ��� ������
    �������� ������ ���������.

    � ��������� ������������ ����������� � ������ ����������, ��� ����������
    �����������, ��� ������ ���������� ����� �������. �������� ���-�������
    �� 256-�������� ����� (� ����� ������ ������ ����� ����� - L=MinLen/2)
    ������������ ��� ������ � ���-������� (hasharr[hash&HashMask]), ���
    ��������� ����� ���� ������ �������� ����� ������. �� �������� ���
    (�����������) �� ���� � ���������� ����������. ��� ��� ���������, ���
    ���� ��������, � ������� �� ����������� LZ77 ����������, ���� ��
    ����������� (����� �������) ����������, � ��������� ���� ���� ������ - ��
    ��������� ����, ������� ����� ���� ���-����, � ��� ��, �������������,
    ���������������� ��������� � �� �������� �����.

    ������ ���� (HashSize): ��� ���������� ��������� � �����������, ��� ��
    ������ ���� � 2-4 ���� ������ ���������� ���������, ������� � ���� �������
    ���������. ������ �� �������� ���������, ��� ������ ���������� ����� �� ��
    ����� ���-�� ������, � ��� MinLen=32 - ���� �������� (!) �������. �� ����,
    ��������, ��� 32 �� ����� ��� MinLen=512 � ��� ����������� ������ 16-�
    256-������� ���� � ����� ���������� ����������� ��������� - 32���/16=2���,
    �.�. 8 ��, � ��� �������� ������ ������ �������. ��� MinLen=32 �����
    ���������� ��������� 32���/4=8���, �� �� ������ ���-������� ��������
    ������, �� ���� ���������� �� �� ����� 8 ��. ����� �������, �����������
    ���������� ������������� ������ ���-������� ������� �� ����������� 1/4
    ������� �������� ������. ���� �� ������ ���������� ������ �������� - ��
    ����������� �������� HashBits (����� -h). ���������� HashSize ��� ���������
    MinLen �������� ������� ��������� ������� ������.

    Amplifier: ��� ���� ������� ����, ��� ������ ����������� ������ �����
    ������, ������� �� � ��������� ������� ��� ���������� ���� ����� � ������
    >=MinLen - ���� � ��� ��������� �����������. ������ ���� �����������
    ����������, � ����� ������������� ����� ��-�� ����� ��������. ��������
    Amplifier (����� -a) ��������� ����������� ������������ �������� �����
    ������ (� ��� ����� Amplifier ���). ����� �������, ��� �����������
    ����������� ������ ����� ������ ���������� Amplifier � ���������� �������
    ��������, ������ 99. ����������, ��� ��������� �������� � ���� ���������
    ����������� ������.

    Barrier � SmallestLen: ��������� ���������, � ��������� ppmd, ����������,
    ���� ������������ ���������� ������� �������� MinLen ��� �������
    ���������. ��� ��� ��������� ��������� ���������� ��������������� �������
    ������ ����������, �������� "� ������ ��������� - MinLen=128, �����
    MinLen=32" ������� ����� MinLen=128, Barrier=1<<20, SmallestLen=32
    (����� -l128 -d1048576 -s32). ��� ���� ����� ����� �������������, ��-��,
    �� ���������� ����� � ������ �� SmallestLen ������ MinLen.


** Benchmarks using 1GHz processor ****************************************************************

Test results for 26mb:
        Compression time   Compressed size
-l8192  0.5 seconds
 -l512  1.1
 -l128  1.4
  -l32  2.5                12.7 mb
lrzip   2.6                14.1
lzp:h20 6.5                13.1
lzp:h13 3.0                20.6

Compression speed on incompressible data:
-l8192  52 mb/sec
 -l512  25 mb/sec
 -l128  17 mb/sec
  -l32   8 mb/sec
lrzip    8 mb/sec

*/


// �������� ************************************************************************
#include "../Compression.h"


#ifdef REP_LIBRARY
#define stat1(nextmsg,size)
#else
void stat1 (char *nextmsg, int Size);
#endif


// ����� ��������� ������ **********************************************************************
#ifndef REP_LIBRARY
// ����� ����������, ���������� �� stdout
//   0   ������ ������
//   1   ����� ����������
//   2   ��������� ���������� � ��������
static int verbose = 0;

// �������� ����� ���������� ������� ���� ���������
static int print_timings = 0;
#endif


// ��������������� ������� *********************************************************************

// ���������� � �������
static unsigned power (unsigned base, unsigned n)
{
    int result;
    for (result=1; n != 0; result *= base, n--);
    return result;
}

// ���������� ������� base, �� ������������� sqrt(n),
// �������� sqrtb(36,2) = 4
static unsigned sqrtb (unsigned n, unsigned base = 2)
{
    int result;
    for (result=1; (n/=base*base) != 0; result *= base);
    return result;
}

// ������� ����� ������ ����������, ��� ����� �� *p � *q
static inline byte* find_match_start (byte* p, byte* q, byte* start)
{
    while (q>start)   if (*--p != *--q)  return q+1;
    return q;
}

// ������� ����� ������� �������������� �����, ��� ����� �� *p � *q
static inline byte* find_match_end (byte* p, byte* q, byte* end)
{
    while (q<end && *p==*q) p++,q++;
    return q;
}

// �������� ������ �� ������ � �����, ��� � ������� ����������� �������
// (��� �����, ��������� ������ ����� ������������ � � ���� ������ �����
// ���������� ������������ ������)
static inline void copymem (byte* p, byte* q, unsigned len)
{
    if (len)
    do *p++ = *q++;
    while (--len);
}


// �����, ������������ ��� ����������� ���������� ����������� ������� ������
// � ���������. ����� ����� ���������� � ���� 32-��������� �����. �������
// ���������� ������ ������������ � �������� �����.
// ������������� ����� ������������ ������ ����� ���������� � ���� ������.
// ����� ���������� ����� ������ - ��� max(p,end), ��� p - ������� ���������,
// � end - ������������ ������� ����� ���������� ������.
// �������� ������������ �� ������������, ��������� �������� �����������,
// ��� ������������ �� ���������.
struct Buffer
{
    byte*  buf;                 // ����� ����������� ������
    byte*  p;                   // ������� ��������� ������/������ ������ ����� ������
    byte*  end;                 // ����� ����� ����� �����������/���������� ������
    byte*  bufend;              // ����� ����������� ������
    int    len()                { return mymax(p,end)-buf; }
    Buffer (int size)           { buf=p=end= (byte*) malloc(size); bufend=buf+size;}
    void   free ()              { ::free(buf); buf=p=end=NULL; }
    void   put32(int x)         { *(int32*)p = x; p+= sizeof(int32); }
    void   put(void *b, int n)  { memcpy(p,b,n); p+= n; }
// ��� ������ ������
    void   rewind()             { end=mymax(p,end); p=buf; }
    int    get32()              { int x = *(int32*)p; p+= sizeof(int32); return x; }
    bool   eof()                { return p>=end; }
// ��� FWRITE
    int    remainingSpace()     { return bufend-p; }
    void   empty()              { p=end=buf; }
};

// �������� 32-������ ����� � �������� �����
#define Put32(x)                                           \
{                                                          \
    Buffer header(sizeof(int32));                          \
    header.put32 (x);                                      \
    FWRITE (header.buf, header.len());                     \
    header.free();                                         \
}


// �������� �������� *********************************************************************

/*
    ��� ���������� ���������� ������ �� MinLen ���� ����� �������� � ��� ��������
    ����������� ������� �� ������ ������ L = MinLen/2 ���� � �������� k = sqrt(L) ����.
    ������ � ���� ���-������� ���������� ��� ������, ������������ � ������ test=k ������
    �� ������� ����� ������ L ����.
*/

#define update_hash(sub,add)                        \
{                                                   \
    hash = hash*PRIME + add - sub*cPOWER_PRIME_L;   \
}

#define chksum         ((hash>>28)&k1)
#define PRIME          153191           /* or any other prime number */
#define POWER_PRIME_L  power(PRIME,L)

int rep_compress (unsigned BlockSize, int MinCompression, int MinMatchLen, int Barrier, int SmallestLen, int HashBits, int Amplifier, CALLBACK_FUNC *callback, VOID_FUNC *auxdata)
{
    // ��������� ���������� ���������
    if (SmallestLen>MinMatchLen)  SmallestLen=MinMatchLen;
    int L = roundup_to_power_of (SmallestLen/2, 2);  // ������ ������, �� ������� ��������� � ���
    int k = sqrtb(L*2), k1=k-1, test=mymin(k*Amplifier,L), cPOWER_PRIME_L = POWER_PRIME_L;
    int HashSize, HashMask=0, *hasharr=NULL, hash=0;  int errcode=FREEARC_OK;
    int Base=0, last_i=0, last_match=0;    // last_match points to the end of last match written, we shouldn't start new match before it
#ifdef DEBUG
    int matches=0, total=0, lit=0;
#endif
    byte *buf = (byte*) malloc(BlockSize);   // �����, ���� ����� ���������� ������� ������
    if (buf==NULL)  return FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;    // Error: not enough memory
    FOPEN();

    // ������ �������� ����� ����� ������, ������������ � ���������� ���� ���� ������
    // �������� � min(1/8 ������,8��). ��� ������������ ��������� ���� sliding window,
    // �� ���� ����������� ������ ���������� � ����������� ������� ����� �� ��� ����� ������
    for (int FirstTime=1; ; FirstTime=0) {

        // ������ ������� ������
        int Size = callback ("read", buf+Base, mymin (BlockSize-Base, mymin (BlockSize/8, 8*mb)), auxdata);
        if (Size < 0)  {errcode=Size; goto finished;}   // Error: can't read input data
        if (FirstTime) {
            HashSize = HashBits>0? (1<<HashBits) : roundup_to_power_of(BlockSize,2) / mymax(k,16); // ������ ���� ������ ��������������� ���������� ��������. ������� �� ����� � ���� �������, �� �� ��������� �������� �� ������� ������ / ������ ������� ������ (Size/16*sizeof(int)==Size/4)
            HashMask = HashSize-1;
            hasharr  = (int *) calloc (HashSize, sizeof(int));
            if (HashSize && hasharr==NULL)  {errcode=FREEARC_ERRCODE_NOT_ENOUGH_MEMORY; goto finished;}   // Error: not enough memory
            debug (verbose>0 && MinMatchLen==SmallestLen && printf(" Buf %d mb, MinLen %d, Hash %d mb, Amplifier %d\n", ((Size-1)>>20)+1, MinMatchLen, (HashSize*sizeof(int))>>20, test/k));
            debug (verbose>0 && MinMatchLen!=SmallestLen && printf(" Buf %d mb, MinLen %d, Barrier %d, Smallest Len %d, Hash %d mb, Amplifier %d\n", ((Size-1)>>20)+1, MinMatchLen, Barrier, SmallestLen, (HashSize*sizeof(int))>>20, test/k));
            Put32 (BlockSize);   // ������� ������ ������� � �������� �����
            stat1 ("Compression",0);
        }
        if (Size == 0) break;  // No more input data
        debug (verbose>0 && printf(" Bytes read: %u\n", Size));
        if (Base==0)  {   // � ������ ��� ��� ����� �������� ����� ������� ������
            hash=0;  for (int i=0; i < mymin(L,Size); i++)  update_hash (0, buf[i]);  // ��������� �������� hash - �� �� ������ L ���� ������
        }
        int literals=0, bsize = (Size/SmallestLen+1) * sizeof(int32);    // ����. ����� ������, ������� ����� ���� ������� � ����� ���� ��� ��������
        Buffer lens(bsize), offsets(bsize), datalens(bsize), dataOffsets(bsize);  // ������ ��� ���������� �������� ����, �������� ����������, ���� �������� ������ � ����� ���� ������. ��� ����������� ��������� ��������� �������� ������� ������

        // �������� ����, ��������� ������������� ������ �� ������� ������
        for (int i=last_i; i+L*2 < Base+Size; last_i=i) {   // ������������ �� L ���� �� ���� �������� ����� + ���� ����� L ���� lookahead

            // ���� ���������� � ������ test ������ ����� ����� L
            for (int j=0; j<test; j++, i++) {
                if (i>=last_match) {   // ��������� ���������� ������ ���� ���������� ��������� ���������� ��� ���������
                    int match = hasharr[hash&HashMask];
                    if (match && chksum==(match&k1)) {  // ������� ���� �������� match ������ ����������� ����� chksum. Ÿ �������� ��������� ���������� ����������� ��������� ������ � ������ ���-�������� (������������� ������ �������� hasharray ��� ������ ��������� hash)
                        match &= ~k1;   // ����� �� �� match. ������ i � match - ������ ���������������� ����������� ������ ����� L
                        if (match>=i && match<Base+Size)  goto no_match;  // match �������� �� ��� �� ������������ ������, �� ���� �� �������� �������
                        // ����������/���������� ��������, ������� ����� ��������� ��� ������
                        // ������ ������������ �� i, ����� ������ ������������ �� match,
                        // �� ����� �� ������� ������ � �� �������� � ������� ������
                        int LowBound  = match<i? i-match : match-(Base+Size)>i? 0 : i - (match-(Base+Size));
                        int HighBound = BlockSize - match + i;
                        // ����� �������� ������ � ����� ����������, ��������� ����� � ����� �� buf[i] <=> buf[match]
                        // i ���������� ����� � ������ ���������� last_match � Base+Size, ��������������
                        int start = find_match_start (buf+match, buf+i, buf+mymax(last_match,LowBound)) - buf;
                        int end   = find_match_end   (buf+match, buf+i, buf+mymin(Base+Size,HighBound)) - buf;
                        // start � end - ������� ���������� ������ i. ��������, ��� ��������� ���������� ����� ����� >=MinMatchLen (��� SmallestLen, ���� ��������� >Barrier)
                        if (end-start >= (i-match<Barrier? MinMatchLen : SmallestLen) ) {
                            int offset = i-match;  if (offset<0)  offset+=BlockSize;
                            // ���������� �������! ������� ���������� � �� � �������� ������
                            dataOffsets.put32 (last_match);         // ����� ����������� ������
                               datalens.put32 (start-last_match);   // ����� ����������� ������
                                offsets.put32 (offset);             // �������� match'�
                                   lens.put32 (end-start);          // ����� match'�
                            // ��������� ������� ����� ���������� ���������� � ������� ���������� ����������
                            debug ((matches++, total += end-start, lit += start-last_match));
                            debug (verbose>1 && printf ("Match %d %d %d  (lit %d)\n", -offset, start, end-start, start-last_match));
                            literals += start-last_match;  last_match=end;
                        }
                    }
                }
      no_match: // ������� � ������� ����� ����� ����� ������ k ����. ���� Amplifier=1, �� ��� ������� ����������� ������ ��� j=0, � ��������� ����� ������������� � ��������� �����
                if ((i&k1) == 0)  hasharr[hash&HashMask] = i + chksum;
                update_hash (buf[i], buf[i+L]);  // ������� sliding hash, ����� � ���� buf[i+L] � ������ buf[i]
            }
            // NB! ����������� �� ������� k �������!

            // ������� � ������� ����� ����� ����� ������ k ���� �� ����� �������� ����� ����� L
            while ((i&(L-1)) != 0) {
                hasharr[hash&HashMask] = i + chksum;
                for (int j=0; j<k; j++, i++)   update_hash (buf[i], buf[i+L]);
            }
        }

        // ����� ������ ������ � �������� ����� � ���������� � ��������� ��������� ������ ������
        Base += Size;
        if (Base==BlockSize) {  // ���� ���������� ������� ����� ������� ������
            // �������� � �������� ������ ������� ������ ����� ���������� ���������� ����������
            dataOffsets.put32 (last_match);        // ����� ������� ������
               datalens.put32 (Base-last_match);   // ����� ������� ������
            literals += Base-last_match;
            Base=last_match=last_i=0;  // ��! ������ ��������� ����� � ������!
        } else {
            if (lens.len()==0)  goto skip_output;  // There is no data to write, so let's skip this block entirely
            datalens.put32 (0);  // ����� ���������� ������� � ������ datalens � ����� ������ ���� ����� �� ���� ������, ��� � lens/offsets
        }
        // �������� ������ ������ ������ � ���������� ��������� ���������� � �����
       {int outsize = sizeof(int32)*2+lens.len()+offsets.len()+datalens.len()+literals;
        QUASIWRITE (outsize);
        Buffer header(2*sizeof(int32));  header.put32 (outsize-sizeof(int32));  header.put32 (lens.len()/sizeof(int32));
        // ������� ���������� ������� � �������� ������ � �������� �����
        FWRITE (  header.buf,   header.len());
        FWRITE (    lens.buf,     lens.len());
        FWRITE ( offsets.buf,  offsets.len());
        FWRITE (datalens.buf, datalens.len());
        dataOffsets.rewind(); datalens.rewind();
        while (!dataOffsets.eof()) {
            FWRITE (buf + dataOffsets.get32(), datalens.get32());
        }
        FFLUSH();
        header.free();
        // ���������� ����������
        debug (verbose>0 && printf(" Total %d bytes in %d matches (%d + %d = %d)\n", total, matches, header.len()+lens.len()+offsets.len()+datalens.len(), lit, header.len()+lens.len()+offsets.len()+datalens.len()+lit));
        stat1 ("Compression", Size);}
     skip_output: lens.free(); offsets.free(); datalens.free(); dataOffsets.free();
    }

    // �������� ��������� ����, ���������� ����������� ������� ������, � 0 - ������� ����� ������
   {int datalen = Base-last_match;
    Put32 (sizeof(int32)*2 + datalen);
    Put32 (0);                    // 0 matches in this block
    Put32 (datalen);              // ����� ������� ������
    FWRITE (buf+last_match, datalen); // ���� ��� ������
    Put32 (0);}                   // EOF flag (see below)
finished:
    FCLOSE();
    free(hasharr);
    free(buf);
    return errcode;
}


#define ReturnErrorCode(n)  { free(data0); free(buf0); return (n); }

// Classical LZ77 decoder with sliding window
int rep_decompress (unsigned BlockSize, int MinCompression, int MinMatchLen, int Barrier, int SmallestLen, int HashBits, int Amplifier, CALLBACK_FUNC *callback, VOID_FUNC *auxdata)
{
    // ����������� ������ ������ ������� �� ������� ������
    if (callback ("read", &BlockSize, sizeof(int32), auxdata) != sizeof(int32))   return FREEARC_ERRCODE_IO;  // Error: can't read input data
    byte *data = (byte*) malloc (BlockSize), *data0=data;
    if (data==NULL)  return -1;                             // Error: not enough memory
    stat1 ("Decompression",0);

    // ����, ������ �������� �������� ������������ ���� ���� ������ ������
    for (byte *last_data=data; ; last_data=data) {

        // ��������� ���� ���� ������ ������
        int32 ComprSize;
        if (callback ("read", &ComprSize, sizeof(int32), auxdata) != sizeof(int32))   return FREEARC_ERRCODE_IO;  // Error: can't read input data
        if (ComprSize == 0)  break;    // EOF flag (see above)

        byte *buf = (byte*) malloc(ComprSize), *buf0=buf;   // �����, ���� ����� �������� ������� ������
        if (buf==NULL)           ReturnErrorCode (FREEARC_ERRCODE_NOT_ENOUGH_MEMORY);       // Error: not enough memory
        int Size = callback ("read", buf, ComprSize, auxdata);
        if (Size != ComprSize)   ReturnErrorCode (FREEARC_ERRCODE_IO);       // Error: can't read input data

        // ��������� ����� �������� ������ ������ lens/offsets/datalens; ����� ���� ���� ��� ������� � ������� ����������� ������
        int         num = *(int32*)buf;  buf += sizeof(int32);           // ���������� ���������� (= ���������� ������� � �������� lens/offsets/datalens)
        int32*     lens =  (int32*)buf;  buf += num*sizeof(int32);
        int32*  offsets =  (int32*)buf;  buf += num*sizeof(int32);
        int32* datalens =  (int32*)buf;  buf += (num+1)*sizeof(int32);   // ������, datalens �������� num+1 �������

        // ������ �������� ����� ����� �������� ���� ���� �������� ������ � ���� match, ������� interleaved � ����� ���������� ������� ��������
        for (int i=0; i<num; i++) {
            memcpy (data, buf, datalens[i]);  buf += datalens[i];  data += datalens[i];
            debug (verbose>1 && printf ("Match %d %d %d\n", -offsets[i], data-data0, lens[i]));
            // ���� �������� �������� �� ������ ������, �� ������� �� ���� BlockSize, ����� "����������" ������ ������� ������
            int offset = offsets[i] <= data-data0 ?  offsets[i] : offsets[i]-BlockSize;
            copymem (data, data-offset, lens[i]);  data += lens[i];
        }
        // ���� ��� ���� ���� ����������� ������ � ����� ����� (��������, ������� �����)
        memcpy (data, buf, datalens[num]);  buf += datalens[num];  data += datalens[num];

        // ����� ������������� ������, ������ ���������� ���������� � ���������� � ��������� �������� �����
        int x = callback ("write", last_data, data-last_data, auxdata);  if (x<0)  ReturnErrorCode(x);
        debug (verbose>0 && printf( " Decompressed: %u => %u bytes\n", ComprSize+sizeof(int32), data-last_data) );
        stat1 ("Decompression", data-last_data);
        if (data==data0+BlockSize)  data=data0;
        free(buf0);
        // NB! check that buf==buf0+Size, data==data0+UncomprSize, and add buffer overflowing checks inside cycle
    }
    free(data0);
    return FREEARC_OK;
}


#ifndef REP_LIBRARY
// �������� ************************************************************************
#include "../Standalone.h"

// ������� ����� ���������� ���������� ���� ��������� � ��������� �������� ���������� ����
void stat1 (char *nextmsg, int Size)
{
    if (! print_timings) return;

    static char *msg = NULL;
    static clock_t StartClock, PreviousClock;

    if (msg == NULL) {
        StartClock = clock();
    } else {
        double seconds = (clock()-PreviousClock) / (double)CLOCKS_PER_SEC;
        printf( "%s: %.3lf seconds, speed %.3lf mb/sec\n", msg, seconds, Size/seconds/1000/1000);
        if (nextmsg==NULL) {
            double seconds = (clock()-StartClock) / (double)CLOCKS_PER_SEC;
            printf( "Total %.3lf seconds, speed %.3lf mb/sec\n", seconds, Size/seconds/1000/1000);
        }
    }

    msg = nextmsg;
    PreviousClock = clock();
}

FILE *fin, *fout;
int readFILE (/*void* param,*/ void* buf, int size)
{
    //FILE *fin = (FILE*)param;
    return  read (fin, buf, size);
}

int writeFILE (/*void* param,*/ void* buf, int size)
{
    //FILE *fout = (FILE*)param;
    if (fout)  write (fout, buf, size);
    return 0;
}

// ������ ��������� ������ � ����� rep_compress/rep_decompress � ���������������� �����������
int main (int argc, char **argv)
{
    // ���������� ������ ��������?
    int unpack = 0;

    int bufsize=1<<27, mincompr=100, minlen=512, small_len=0, barrier=8<<20, hashbits=0, amplifier=1;

    while (argv[1] && argv[1][0] == '-') {
        switch( tolower(argv[1][1]) ) {
            case 'v':   verbose++;                   break;
            case 't':   print_timings++;             break;
            case 'd':   if (argv[1][2])  barrier=atoi(argv[1]+2);  else unpack++;  break;
            case 'b':   bufsize   = atoi(argv[1]+2)*(1<<20); break;
            case 'l':   minlen    = atoi(argv[1]+2); break;
            case 's':   small_len = atoi(argv[1]+2); break;
            case 'h':   hashbits  = atoi(argv[1]+2); break;
            case 'a':   amplifier = atoi(argv[1]+2); break;
            default :   printf( "\n Unknown option '%s'\n", argv[1]);
                        exit(1);
        }
        argv++, argc--;
    }
    if (!small_len)  small_len = minlen;

    // ����� �����, � ��������� ������ ������ ���� ����� 1 ��� 2 ���������
    // (������� � ����������� �������� �����)
    if (argc != 2  &&  argc != 3) {
        printf( "\n Usage: rep [options] original-file [packed-file]");
        printf( "\n   -bN --  use N mb for sliding window (recommended: half of total RAM)");
        printf( "\n   -lN --  minimal match len");
        printf( "\n   -dN --  barrier for smaller matches");
        printf( "\n   -sN --  minimal match len after barrier");
        printf( "\n   -hN --  hash bits");
        printf( "\n   -aN --  coefficient of search \"amplification\"");
#ifdef DEBUG
        printf( "\n   -v  --  increment verbosity level (0 - default, 2 - maximum)");
#else
        printf( "\n   -v  --  verbosity level (you should recompile program with -DDEBUG to enable this option)");
#endif
        printf( "\n   -t  --  print operation timings");
        printf( "\n" );
        printf( "\n For decompress: rep -d [-v -t] packed-file [unpacked-file]");
        printf( "\n" );
        exit(2);
    }

    // ������� ������� ����
    fin = fopen (argv[1], "rb");
    if (fin == NULL) {
        printf( "\n Can't open %s for read\n", argv[1]);
        exit(3);
    }

    // ������� �������� ����, ���� �� ����� � ��������� ������
    fout = NULL;
    if (argc == 3) {
        fout = fopen (argv[2], "wb");
        if (fout == NULL) {
            printf( "\n Can't open %s for write\n", argv[2]);
            exit(4);
        }
    }

    if (!unpack) {
        rep_compress   (bufsize, mincompr, minlen, barrier, small_len, hashbits, amplifier, readFILE, fin, writeFILE, fout);
    } else {
        rep_decompress (bufsize, mincompr, minlen, barrier, small_len, hashbits, amplifier, readFILE, fin, writeFILE, fout);
    }

    fclose(fin);  if (fout)  fclose(fout);
    return 0;
}

#endif

/* to do:
+1. sliding window, In() function to read data
+2. ������������ ������, ���. ��� �������
+3. save pointers to unmatched blocks instead of copying data
4. ���������, ��� ���� ����� ����������, � �������� ��� ����� ���������.
     ������, ���������� ������� �������� last_match � �������� ��� �������� ������
5. last_small_match - ���� ��������� match ������ �� ��������� ���������� (<Barrier),
     �� ������������ ��������� ����� �� ������� ���������� (>Barrier) ���� ���� �� ��������.
     ��� �������� ��� ��������� �������� ���� � ������� ����� :)
6. -l8192 -s512
7. buffer data for Out() in 256k blocks

Fixed bugs:
1. �������� ������ �� ������� ������: offset<data-data0 ������ <=
2. last_match �� ��������� ��� ������ �� ����� ��� Base=0 � Size=0
*/
