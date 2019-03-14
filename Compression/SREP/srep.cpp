/*
SuperREP: large-distance LZ77 compression algorithm
(c) Bulat.Zignashin@gmail.com
homepage: http://freearc.org/research/SREP.aspx

User reports: http://encode.dreamhosters.com/showpost.php?p=10092&postcount=174
              http://forum.ru-board.com/topic.cgi?forum=5&topic=31386&start=680#10
              http://forum.ru-board.com/topic.cgi?forum=5&topic=31386&start=766&limit=1&m=1#1

When compressing, memory usage for hash: filesize/L*20 + filesize/L*4 + roundup(filesize/L*4 * 5/4) + roundup(filesize/(L*8) * 4)
where roundup() rounds up to the next power of 2 and first memory block (i.e. *20) used only in -m1 mode.
So, overall, hash includes 3 or 4 chunks. Besides hash, compression uses 3*8mb buffers.

Decompression uses only 2*8mb buffers and no hash. Repeated data copied directly from output file, though,
so you need to have enough RAM used for disk cache in order to make decompression fast.


Versions history:

srep 0.5 (November 23 2009):
    * first version that was able to compress and extract data

srep 0.6 (November 23 2009):
    * fixed 64-bit version, now it properly handles files >2gb
    * fixed decompresion with non-default -l
    * -s prints stats after each block

srep 0.7 (November 23 2009):
    * reduced memory usage down to 6-8% of filesize. For example, 24gb file needs 256+256+960 mb memory chunks
    * now hash keeps address of the last chunk with the same contents
    * hashing improved a little
    * fixed WinXP crashing bug

srep 0.8 (November 24 2009):
    * better compression due to improved hashing and compressed format
    * faster compression on files <1gb
    * MD5 integrity checking on decompressed data
    * first 8 bytes of compressed file now contains SREP signature, helping programs like unix magic
    * exit code == 0 on success

srep 1.0 (December 15 2009):
    * -delete option that deletes source file after successful (de)compression
    * checking of -l value

srep 1.5 (May 11 2010):
    * -m1: old method (compression memory = 6-7% of filesize, check matches by SHA1 digest)
    * -m2: new, default method (compression memory = 2-3% of filesize, check matches by rereading old data)
    * -index option - keep index of compressed data in separate file in order to improve compression ratio
    * 64-bit executable that's still 100% compatible but faster than 32-bit one

to do:
check success of every read/seek/write operation (When you run out of space, the file gets cut and srep gives no warning)
digestarr[chunk] may be not filled at all! in particular, for L!=2^n
put into HashTable chunks split between two input blocks (situation that's almost impossible when L==2^n)
ACCELERATOR: add 4 hashes for every 512-byte block to bitsarr and check every 4th with find_match in order to reduce amount of L2 misses (1/4 per byte)
like REP, allow to use larger MinMatchLen for small distances
store (dist,len) sorted by source position so we can keep data that will be used later
-l and -c options to separately specify min. match length and chunk size, respectively
*/

#include <stdio.h>
#include <math.h>
#include <malloc.h>
#include <stdarg.h>

#include "../Common.h"
#include "../Compression.h"

const uint SREP_SIGNATURE      = 0x50455253;
const uint SREP_FORMAT_VERSION1 = 1;
const uint SREP_FORMAT_VERSION2 = 2;
enum SREP_METHOD {SREP_METHOD1=1, SREP_METHOD2, SREP_METHOD3};
typedef uint32 STAT;


// LibTomCrypt: sha1 *********************************************************************

extern "C" {
#define LTC_NO_CIPHERS
#define   LTC_BLOWFISH
#define   LTC_RIJNDAEL
#define     ENCRYPT_ONLY
#define   LTC_TWOFISH
#define   LTC_SERPENT
#define LTC_NO_MODES
#define   LTC_CFB_MODE
#define   LTC_CTR_MODE
#define LTC_NO_HASHES
#define   LTC_MD5
#define   LTC_SHA1
#define LTC_NO_MACS
#define LTC_NO_MATH
#define LTC_NO_PK
#define   LTC_MECC
#define LTC_NO_TEST
#include "../_Encryption/crypt/crypt_argchk.c"
#include "../_Encryption/hashes/md5.c"
#include "../_Encryption/hashes/sha1.c"
}

typedef unsigned char Digest[20];

void compute_digest (void *buf, int size, Digest &result)
{
  hash_state md;
  sha1_init(&md);
  sha1_process(&md, (unsigned char*)buf, (unsigned long)size);
  sha1_done(&md, result);
}


typedef unsigned char Checksum[16];

void compute_checksum (void *buf, int size, void *result)
{
  hash_state md;
  md5_init(&md);
  md5_process(&md, (unsigned char*)buf, (unsigned long)size);
  md5_done(&md, (unsigned char*)result);
}



// ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ *********************************************************************

// Возведение в степень
template <class T>
inline static T power (T base, unsigned n)
{
    T result;
    for (result=1; n != 0; result *= base, n--);
    return result;
}

// Копирует данные из буфера в буфер, идя в порядке возрастания адресов
// (это важно, поскольку буфера могут пересекаться и в этом случае нужно
// размножить существующие данные)
static inline void memcpy_lz_match (void* _p, void* _q, unsigned len)
{
    byte *p = (byte*)_p, *q = (byte*)_q;
    if (len)
    do *p++ = *q++;
    while (--len);
}



// Hash table **********************************************************************************

typedef uint64 HashValue;            // Hash of L-byte block, used as first step to find the match
typedef uint32 StoredHashValue;      // Hash value stored in hasharr[]
typedef uint32 Chunk;                // Uncompressed file are splitted into L-byte chunks, it's the number of chunk in the file
typedef uint64 Offset;               // Filesize or position inside file
const Chunk  MAX_CHUNK = Chunk(-1),  NOT_FOUND = 0;
const int MAX_HASH_CHAIN = 12;
#define min_hash_size(n)   (((n)/4+1)*5)                   /* Minimum size of hash for storing n elements */
#define stored_hash(hash)  (StoredHashValue((hash)>>32))   /* Value saved in hasharr[] - take note that lowest 16-32 bits are used to find index, so we should use other bits to check */
const unsigned ACCELERATOR = 1;

struct HashTable
{
  SREP_METHOD method;
  unsigned L;
  FILE *fin;
  Offset filesize;
  Offset hs;
  unsigned hashsize, hashsize1, hash_shift;
  Digest          *digestarr;
  StoredHashValue *hasharr;
  Chunk           *chunkarr;

  // bitarr[] used for fast probing of hash values - it helps to detect whether we ever seen such hash value before
  bool    bitarrEnabled;
  int     bitarrsize;
  int     bitshift;
  BYTE   *bitarr;

  HashTable (SREP_METHOD _method, unsigned _L, FILE *_fin, Offset _filesize)
  {
    method = _method;  L = _L;  fin = _fin;  filesize = mymax(_filesize,L);  bitarrEnabled = FALSE;
    digestarr = NULL;  hasharr = NULL;  chunkarr = NULL;  bitarr = NULL;
    hs = roundup_to_power_of (min_hash_size(filesize/L), 2);
    if (hs > UINT_MAX  ||  filesize/L > MAX_CHUNK)  return;
    hashsize = hs,  hashsize1 = hashsize-1,  hash_shift = sizeof(HashValue)*CHAR_BIT - lb(hashsize);
    if (method == SREP_METHOD1)
      digestarr = (Digest*)       BigAlloc (filesize/L * sizeof(Digest));
    hasharr  = (StoredHashValue*) BigAlloc (filesize/L * sizeof(StoredHashValue));
    chunkarr = (Chunk*)           BigAlloc (hashsize   * sizeof(Chunk));
    if (chunkarr)  for (int i=0; i<hashsize; i++)  chunkarr[i] = NOT_FOUND;

    bitarrEnabled = TRUE;
    if (bitarrEnabled) {
      bitarrsize = roundup_to_power_of (filesize/L/CHAR_BIT * 4 + 2, 2);   // bit array checking works fine until 1/4 of bitarr[] is filled
      bitshift   = sizeof(HashValue)*CHAR_BIT - lb(bitarrsize);            // bitarrsize should be >=2, otherwise hash>>bitshift == hash>>64 == hash>>0 and indexing panics
      bitarr     = (BYTE*)  BigAlloc (bitarrsize);
      if (bitarr)  for (int i=0; i<bitarrsize; i++)  bitarr[i]=0;
    }
  }
  ~HashTable() {BigFree(bitarr); BigFree(hasharr); BigFree(chunkarr); BigFree(digestarr);}

  // Return errcode if any
  int error()  {return (method==SREP_METHOD1 && !digestarr) || !chunkarr || !hasharr || bitarrEnabled && !bitarr? FREEARC_ERRCODE_NOT_ENOUGH_MEMORY : FREEARC_OK;}

  // How much memory required for hash tables with given file and compression method settings
  Offset memreq() {return hs * sizeof(*chunkarr)
                        + filesize/L * (sizeof(*hasharr) + (method==SREP_METHOD1? sizeof(*digestarr) : 0))
                        + (bitarrEnabled? bitarrsize : 0);}

  // Compute chunkarr[] index for given hash value h
  unsigned index (unsigned h)  {return h&hashsize1;}   // don't use higher bits of h since they may be lost here (unsigned != HashValue)

  // Add chunk pointed by p to hash
  void add_hash (void *p, Offset offset, HashValue hash)
  {
    Chunk curchunk = offset/L;
    hasharr[curchunk] = stored_hash(hash);         // save hash of chunk for primary, quick match checking
    if (method == SREP_METHOD1)
      compute_digest (p, L, digestarr[curchunk]);  // -m1: save chunk digest for secondary, reliable match checking
    if (curchunk == NOT_FOUND)  return;            // it's impossible to hash the chunk having such offset since it's used as signal value

    if (bitarrEnabled) {
      bitarrEnabled = (curchunk/CHAR_BIT < bitarrsize/4);   // bit array checking works fine until 1/4 of bitarr[] is filled
      bitarr[hash>>bitshift] |= 1<<(unsigned(hash)&(CHAR_BIT-1));
    }

    unsigned h = unsigned(hash);  int limit = MAX_HASH_CHAIN;
    for (Chunk chunk;  (chunk = chunkarr[index(h)]) != NOT_FOUND  &&  --limit;  )
    {
      // Replace in hash chunk with the same digest (for -m1) or hash value (for -m2), supposing that it has the same contents
      if (hasharr[chunk] == stored_hash(hash))
        if (method != SREP_METHOD1  ||  0==memcmp(digestarr[chunk], digestarr[curchunk], sizeof(*digestarr)))
          break;
      h++, ((limit&3)==0) && (h=h*123456791+462782913);  // compute next hash slot
    }
    chunkarr[index(h)] = curchunk;
  }

  // Find previous L-byte chunk with the same contents
  Chunk find_match (void *p, HashValue hash)
  {
    if (bitarrEnabled && !(bitarr[hash>>bitshift] & (1<<(unsigned(hash)&(CHAR_BIT-1)))))
      return NOT_FOUND;

    unsigned h = unsigned(hash);  int limit = MAX_HASH_CHAIN;
    for (Chunk chunk;  (chunk = chunkarr[index(h)]) != NOT_FOUND  &&  --limit;  )
    {
      // If hash value is the same...
      if (hasharr[chunk] == stored_hash(hash))
      {
        // ... we either suppose that chunks are the same (for -m2)
        if (method != SREP_METHOD1)
          return chunk;

        // ... or compare their digests (for -m1)
        Digest dig;
        compute_digest (p, L, dig);
        if (0==memcmp(dig, digestarr[chunk], sizeof(dig)))
          return chunk;
      }
      h++, ((limit&3)==0) && (h=h*123456791+462782913);  // compute next hash slot
    }
    return NOT_FOUND;
  }

  // Length of match, in bytes
  unsigned match_len (Chunk start_chunk, char *min_p, char *start_p, char *last_p, Offset offset, char *buf, unsigned *add_len)
  {
    // Comparing with data before the current block
    Offset old_offset = Offset(start_chunk)*L;
    char *p = start_p;  *add_len = 0;
    if (method == SREP_METHOD1)
    {
      // -m1: check match by using saved digests of old chunks
      while (p += L, (old_offset += L) < offset)        // Skip checking first chunk since it was already done in find_match()
      {
        if (last_p-p < L)                               // We have no L-byte chunk to digest
          goto stop;

        Digest dig;                                     // Compare two L-byte blocks by comparison of their digests
        compute_digest (p, L, dig);
        if (0!=memcmp(dig, digestarr[old_offset/L], sizeof(dig)))
          goto stop;
      }
    }
    else if (old_offset < offset)
    { // -m2/-m3: check match by rereading old data from infile

      // -m3: First, compare bytes before match start (which is rounded to L-byte chunk boundary)
      int n = mymin (old_offset, mymin(L, start_p-min_p));   // how much bytes we can check
      if (n > 0  &&  method == SREP_METHOD3)
      {
        // Compare n bytes before start_p
        file_seek (fin, old_offset-n);
        char *old = (char*) alloca(n);
        int len = file_read (fin, old, n), i;
        if (len != n)  goto stop;
        for (i=1;  i <= n  &&  start_p[-i] == old[n-i];  i++);
        *add_len = i-1;
      } else
        file_seek (fin, old_offset);

      // Second, compare bytes after match start
      const int BUFSIZE = 4096;
      for (;  old_offset < offset;  old_offset += BUFSIZE)
      {
        char old[BUFSIZE];                              // Buffer for old data
        int len = file_read (fin, old, BUFSIZE);        // Read old data from file
        if (len != BUFSIZE)  goto stop;                 // If there was any problem reading entire buf
        for (char *q = old;  q < old+len;  p++, q++)
          if (p==last_p  ||  *p != *q)  goto stop;      // Exit function once we've found end of match
      }
    }
    else if (method == SREP_METHOD3)
    {
      // -m3: compare bytes (that are present in current block) before match start
      int i, n = mymin (old_offset-offset, mymin(L, start_p-min_p));   // how much bytes we can check
      for (i=1;  i <= n  &&  start_p[-i] == (buf+(old_offset-offset))[-i];  i++);
      *add_len = i-1;
    }

    // Comparing with data in the current block
    for (char *q = buf+(old_offset-offset);
         p < last_p  &&  *p == *q;
         p++,q++);

    stop:  return p-start_p + *add_len;
  }
};


// *********************************************************************************************

/*
    Для нахождения выравненных совпадений длины кратной L нужно заносить в хеш значения контрольной функции
      от блоков длиной L в позициях, кратных L, и искать в этой хеш-таблице совпадения для всех блоков длиной L байт.
    hash содержит значение хеш-функции от последних L обработанных байт, для удобства обновления используется скользящая хеш-функция.
    update_hash(sub,add) выносит из хеша байт sub и добавляет байт add.
*/

const HashValue PRIME = 153191;       // or any other prime number
#define update_hash(sub,add)                                       \
{                                                                  \
    hash = hash*PRIME + BYTE(add) - value_to_subtract[BYTE(sub)];  \
}


// Compress buf[] and return compressed data in outbuf[] and statbuf[]
void compress (bool ROUND_MATCHES, unsigned L, unsigned MIN_MATCH, Offset offset, HashTable &h, char *buf, int block_size, char *outbuf, STAT *statbuf, char *&out, STAT *&stat)
{
  out  = outbuf;
  stat = statbuf;

  HashValue value_to_subtract[256];                     // used to optimize update_hash() by replacing i*power(PRIME,L) with vts[i]
  for (int i=0; i<256; i++)
    value_to_subtract[i]  =  i * power(PRIME,L);

  HashValue hash=0;  for (int i=0; i<L; i++)  update_hash (0, buf[i])
  unsigned last_match = 0;

  for (unsigned i=0, i1=1; i+L<=block_size; i++)
  {
    if (i%ACCELERATOR==0 && i>=last_match)              // Let's check for match if previous match is over
    {
      Chunk k = h.find_match (&buf[i], hash);           // Number of previous chunk with the same contents as current one
      if (k != NOT_FOUND)                               // If match was found, and ...
      {
        unsigned add_len, match_len = h.match_len (k, &buf[last_match], &buf[i], buf+block_size, offset, buf, &add_len);
        if (match_len >= MIN_MATCH)                     // ... it's large enough, then ...
        {
          unsigned match_start = i-add_len;
          unsigned L1 = (ROUND_MATCHES? L : 1);         // match_len should be divisible by L in -m1 and -m2 modes
          if (ROUND_MATCHES)
            match_len = match_len/L*L;

          memcpy(out, &buf[last_match], match_start-last_match);  // ... copy literal bytes after the last match and ...
          out += match_start-last_match;

          Offset match_offset = (offset+i-Offset(k)*L)/L1;        // ... save triple (literal_len, match_offset, match_len)
          *stat++ = match_start-last_match;
          *stat++ = match_offset;  if (!ROUND_MATCHES)  *stat++ = match_offset >> 32;
          *stat++ = (match_len-L)/L1;
          last_match = match_start+match_len;
        }
      }
    }
    if (--i1 == 0)  h.add_hash (&buf[i], offset+i, hash), i1=L;   // Заносим в индекс h очередной блок данных через каждые L байт
    update_hash (buf[i], buf[i+L]);  // Обновим sliding hash, внеся в него buf[i+L] и вынеся buf[i]
  }

  memcpy(out, &buf[last_match], block_size-last_match);
  out += block_size-last_match;
}


// Decompress data using stat[] and in[] and return original data in outbuf[]. Returns TRUE on successful decompression
bool decompress (bool ROUND_MATCHES, unsigned L, FILE *fout, Offset offset, STAT *stat, char *in, char *inend, char *outbuf, char *outend)
{
  STAT *statend = (STAT*)in;
  char  *out = outbuf;
  unsigned L1 = (ROUND_MATCHES? L : 1);

  while (statend-stat >= (ROUND_MATCHES? 3:4))
  {
    // Like in original LZ77, LZ matches and literals are interleaved
    unsigned lit_len         = *stat++;                     // length of literal (copied from in[])
    Offset   lz_match_offset = *stat++;                     // LZ match start, relative to current position (and divided by L when ROUND_MATCHES==TRUE)
    if (!ROUND_MATCHES)  lz_match_offset += Offset(*stat++) << 32;
    unsigned lz_match_len    =(*stat++)*L1 + L;             // LZ match length
    if (lit_len+lz_match_len > outend-out)  return FALSE;   // data format error: output buffer overflow

    // First, copy literal data
    memcpy (out, in, lit_len);
    in  += lit_len;
    out += lit_len;

    // Absolute address (relative to start of outfile) of LZ match start
    Offset lz_offset = ((offset+(out-outbuf))/L1 - lz_match_offset) * L1;

    // Second, copy LZ match data from previous blocks
    if (lz_offset < offset)
    {
      uint bytes = mymin (lz_match_len, offset-lz_offset);
      file_seek(fout, lz_offset);
      file_read(fout, out, bytes);
      lz_match_len -= bytes;
      lz_offset    += bytes;
      out          += bytes;
    }

    // Third, copy LZ match data from the current block
    memcpy_lz_match (out, outbuf + (lz_offset-offset), lz_match_len);
    out += lz_match_len;
  }
  memcpy(out, in, inend-in);
  out += inend-in;
  return out==outend;
}


// Exit on error
enum { NO_ERRORS         = 0
     , WARNINGS          = 1
     , ERROR_CMDLINE     = 2
     , ERROR_IO          = 3
     , ERROR_COMPRESSION = 4
     , ERROR_MEMORY      = 5
     };

void error (int ExitCode, char *ErrmsgFormat...)
{
  va_list argp;
  va_start(argp, ErrmsgFormat);
  fprintf  (stderr, "\n  ERROR! ");
  vfprintf (stderr, ErrmsgFormat, argp);
  fprintf  (stderr, "\n");
  va_end(argp);

  exit(ExitCode);
}

int main (int argc, char **argv)
{
  SREP_METHOD method = SREP_METHOD2;
  unsigned L = 0, min_match = 0;
  bool compression = TRUE;
  bool delete_input_files = FALSE;
  char *index_file = "";
  char *newline = "  \b\b";
  int errcode = 0, warnings = 0;

  while(argv[1])
  {
    if (strequ(argv[1],"-d")) {
      compression = FALSE;
    } else if (strequ(argv[1],"-delete")) {
      delete_input_files = TRUE;
    } else if (strequ(argv[1],"-s")) {
      newline = "\n";
    } else if (strequ(argv[1],"-m1")) {
      method = SREP_METHOD1;
    } else if (strequ(argv[1],"-m2")) {
      method = SREP_METHOD2;
    } else if (start_with(argv[1],"-index=")) {
      index_file = argv[1]+7;
    } else if (compression && start_with(argv[1],"-c")) {
      L = parseInt (argv[1]+2, &errcode);
    } else if (compression && start_with(argv[1],"-l")) {
      min_match = parseInt (argv[1]+2, &errcode);
    } else break;
    if (errcode)
      error (ERROR_CMDLINE, "Invalid option: %s", argv[1]);
    argv++, argc--;
  }

  bool ROUND_MATCHES = (method <= SREP_METHOD2);
  if (!L && !min_match)  min_match = 512;
  if (!L)                L = ROUND_MATCHES? min_match : rounddown_to_power_of(min_match+1,2)/2;
  if (!min_match)        min_match = L;
  if (L != roundup_to_power_of(L,2)) {
    fprintf (stderr, "Warning: -l parameter should be power of 2, otherwise compressed file may be corrupt\n");
    warnings++;
  }

  if (argc!=3) {
    fprintf (stderr, "SuperREP 1.5: lz77 preprocessor that consumes only 2-3%% of filesize for compression\n"
                     "  May 11, 2010   homepage: http://freearc.org/research/SREP.aspx\n"
                     "\n"
                     "  Usage: SREP [options] infile outfile\n"
                     "     infile/outfile may be denoted as \"-\" that means stdin/stdout\n"
                     "  Options are:\n"
                     "     -m1: check matches by SHA1 digest (compression memory = 6-7%% of filesize)\n"
                     "     -m2: check matches by rereading old data (compression memory = 2-3%% of filesize)\n"
                     "     -d: decompression (requires only 16 mb of memory besides of OS I/O buffers)\n"
                     "     -delete: delete source file after successful (de)compression\n"
                     "     -index=FILENAME: read/write index of compressed data into separate file\n"
                     "     -l: minimum LZ match length, default 512\n"
                     "     -s: print stats\n");
    exit (ERROR_CMDLINE);
  }

  char *finame = argv[1];
  char *foutname = argv[2];
  if ( compression && method>=SREP_METHOD2 && strequ(finame,"-"))     error (ERROR_IO, "Reading data to compress from stdin isn't supported for this method");
  if (!compression &&                         strequ(foutname,"-"))   error (ERROR_IO, "Writing decompressed data to stdout isn't supported");
  if (!strequ(finame,"-") && strequ(finame,foutname))                 error (ERROR_IO, "Input and output files should have different names");

  FILE *fin = strequ (finame, "-")? stdin : fopen (finame, "rb");
  if (fin == NULL)  error (ERROR_IO, "Can't open %s for read", finame);
  set_binary_mode (fin);

  FILE *fout = strequ (foutname, "-")? stdout : fopen (foutname, compression? "wb" : "w+b");
  if (fout == NULL)  error (ERROR_IO, "Can't open %s for write", foutname);
  set_binary_mode (fout);

  FILE *fstat = *index_file? fopen (index_file, compression? "wb" : "rb") : (compression? fout : fin);
  if (fstat == NULL)  error (ERROR_IO, "Can't open %s for write", index_file);
  set_binary_mode (fstat);


  STAT header[100];  int bufsize = 8<<20,  header_size = sizeof(*header)*3 + sizeof(Checksum);
  Offset filesize = get_flen(fin), origsize=0, compsize=sizeof(*header)*4;
  double cpu0 = GetThreadCPUTime(), real0 = GetGlobalTime();

  if (compression)
  {
    char *buf     = new char[bufsize+1];
    char *outbuf  = new char[bufsize], *out;
    STAT *statbuf = new STAT[bufsize/4], *stat;

    HashTable h(method, L, fin, filesize);
    if (h.error())  error (ERROR_MEMORY, "Can't allocate memory: %.0lf mb required", double(h.memreq()/mb));
    fprintf (stderr, "%.0lf mb used for hash\n", double(h.memreq()/mb));
    //fprintf (stderr, "%.0lf mb used for hash, -m%d -l%d -c%d\n", double(h.memreq()/mb), method, min_match, L);

    // Write compressed file header
    header[0] = BULAT_ZIGANSHIN_SIGNATURE;
    header[1] = SREP_SIGNATURE;
    header[2] = ROUND_MATCHES? SREP_FORMAT_VERSION1 : SREP_FORMAT_VERSION2;
    header[3] = L;
    file_write (fout, header, sizeof(*header)*4);

    // Compress data by 8mb (bufsize) blocks until EOF
    for(;;)
    {
      // Read and compress next block
      file_seek (fin, origsize);
      int len = file_read (fin, buf, bufsize);
      if (len==0) break;
      compress(ROUND_MATCHES,L,min_match,origsize,h, buf,len,outbuf,statbuf,out,stat);

      // Fill block header and compute block checksum
      header[0] = out-outbuf;
      header[1] = len;
      header[2] = (char*)stat - (char*)statbuf;
      compute_checksum (buf,len, header+3);

      // Write compressed data to output file
      file_write (fout,  header,  header_size);
      file_write (fstat, statbuf, (char*)stat - (char*)statbuf);
      file_write (fout,  outbuf,  out-outbuf);

      // Update statistics
      compsize += header[0] + header[2] + header_size;
      origsize += header[1];
      fprintf (stderr, "\rCompression ratio: %.0lf -> %.0lf: %5.2lf%%. Cpu %.3lf mb/sec, real %.3lf mb/sec%s", double(origsize), double(compsize), double(compsize)*100/origsize, origsize/mymax(GetThreadCPUTime()-cpu0,0.001)/1000000, origsize/mymax(GetGlobalTime()-real0,0.001)/1000000, newline);
    }
  }
  else  // Decompression
  {
    STAT *statbuf = new STAT[bufsize/4+1024];
    char *out     = new char[bufsize];

    // Check header of compressed file
    int len = file_read (fin, header, sizeof(*header)*4);
    if (len != sizeof(*header)*4
     || header[0] != BULAT_ZIGANSHIN_SIGNATURE
     || header[1] != SREP_SIGNATURE)                error (ERROR_COMPRESSION, "Not an SREP compressed file: %s", finame);
    if (header[2] < SREP_FORMAT_VERSION1
     || header[2] > SREP_FORMAT_VERSION2)           error (ERROR_COMPRESSION, "Incompatible compressed data format: v%d (this program supports only v%d..v%d) in file %s", header[2], SREP_FORMAT_VERSION1, SREP_FORMAT_VERSION2, finame);
    L = header[3];
    bool ROUND_MATCHES = (header[2] == SREP_FORMAT_VERSION1);

    // Decompress data by blocks until EOF
    for(;;)
    {
      // Read block header
      int len = file_read (fin, header, header_size);
      if (len==0) break;
      int compsize1 = header[0]+header[2],
          origsize1 = header[1],
          statsize1 = header[2];
      if (len!=header_size  ||  compsize1>bufsize+1024  ||  origsize1>bufsize)   error (ERROR_COMPRESSION, "Decompression problem");

      // Read compressed data
      char *buf = (char*)statbuf;
      len  = file_read (fstat, buf, statsize1);
      len += file_read (fin,   buf+statsize1, compsize1-statsize1);
      if (len!=compsize1)   error (ERROR_COMPRESSION, "Decompression problem: unexpected end of input file or I/O error");

      // Perform decompression
      bool ok = decompress(ROUND_MATCHES, L, fout, origsize, statbuf, buf+statsize1, buf+compsize1, out, out+origsize1);
      if (!ok)   error (ERROR_COMPRESSION, "Decompression problem: broken compressed data");

      // Check MD5 sum of decompressed data
      Checksum chks;
      compute_checksum (out,origsize1, &chks);
      if (0!=memcmp(chks, header+3, sizeof(Checksum)))   error (ERROR_COMPRESSION, "Checksum of decompressed data are different to checksum of original data");

      // Write decompressed data to output file
      file_seek  (fout, origsize);
      file_write (fout, out, origsize1);

      // Update statistics
      compsize += header_size + compsize1;
      origsize += origsize1;
      fprintf (stderr, "\rCompression ratio: %.0lf -> %.0lf: %5.2lf%%. Cpu %.3lf mb/sec, real %.3lf mb/sec%s", double(origsize), double(compsize), double(compsize)*100/origsize, origsize/mymax(GetThreadCPUTime()-cpu0,0.001)/1000000, origsize/mymax(GetGlobalTime()-real0,0.001)/1000000, newline);
    }
  }

  fprintf (stderr, "\n");
  fclose(fin);
  fclose(fout);
  if (fstat!=fin && fstat!=fout)
    fclose(fstat);
  if (warnings==0 && delete_input_files && !strequ(finame,"-"))
    remove(finame);
  return warnings? WARNINGS : NO_ERRORS;
}

