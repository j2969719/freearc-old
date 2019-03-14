// GPL'ed code of Tornado - fast LZ77 compression algorithm.
// (c) Bulat Ziganshin <Bulat.Ziganshin@gmail.com>
#include "../Compression.h"
#include "MatchFinder.cpp"
#include "EntropyCoder.cpp"
#include "LZ77_Coder.cpp"
#include "DataTables.cpp"

// Compression method parameters
struct PackMethod
{
    int  number;            // Preset number
    int  encoding_method;   // Coder (0 - storing, 1 - bytecoder, 2 - bitcoder, 3 - huffman, 4 - arithmetic)
    int  hash_row_width;    // Length of hash row
    uint hashsize;          // Hash size
    int  caching_finder;    // Force/prohibit using caching match finder
    uint buffer;            // Buffer (dictionary) size
    int  match_parser;      // Match parser (1 - greedy, 2 - lazy, 3 - flexible, 4 - optimal, 5 - even better)
    int  hash3log;          // 3-byte hash size log
    int  shift;             // How much bytes to shift out/keep when window slides
    int  update_step;       // How much bytes are skipped in mf.update()
};

extern "C" {
// Main compression and decompression routines
int tor_compress   (PackMethod m, CALLBACK_FUNC *callback, VOID_FUNC *auxdata);
int tor_decompress (CALLBACK_FUNC *callback, VOID_FUNC *auxdata);
}

enum { STORING=0, BYTECODER=1, BITCODER=2, HUFCODER=3, ARICODER=4 };
enum { GREEDY=1, LAZY=2 };

// Preconfigured compression modes
PackMethod std_Tornado_method[] =
    //                 row hashsize  caching buffer parser  hash3 shift update
    { {  0, STORING,     0,       0, 0,      1*mb,  0     , 0 ,     0,  999 }
    , {  1, BYTECODER,   1,   16*kb, 0,      1*mb,  GREEDY, 0 ,     0,  999 }
    , {  2, HUFCODER,    1,   16*kb, 0,      2*mb,  GREEDY, 0 ,     0,  999 }
    , {  3, HUFCODER,    2,   64*kb, 0,      4*mb,  GREEDY, 0 ,     0,  999 }
    , {  4, HUFCODER,    2,    4*mb, 1,      8*mb,  GREEDY, 0 ,     0,  999 }
    , {  5, ARICODER,    4,    8*mb, 1,     16*mb,  LAZY  , 12,     0,  999 }
    , {  6, ARICODER,    8,   16*mb, 1,     32*mb,  LAZY  , 13,     0,  999 }
    , {  7, ARICODER,   16,   32*mb, 1,     64*mb,  LAZY  , 14,     0,    4 }
    , {  8, ARICODER,   32,   64*mb, 1,    128*mb,  LAZY  , 15,     0,    3 }
    , {  9, ARICODER,   64,  128*mb, 1,    256*mb,  LAZY  , 16,     0,    2 }
    , { 10, ARICODER,  128,  256*mb, 1,    512*mb,  LAZY  , 17,     0,    2 }
    , { 11, ARICODER,  256,  512*mb, 1,   1024*mb,  LAZY  , 18,     0,    1 }
    , { 12, ARICODER,  512, 1024*mb, 1,    768*mb,  LAZY  , 19,     0,    1 }
    };

// Default compression parameters are equivalent to option -5
int default_Tornado_method = 5;

int table_dist=256*1024, table_shift=128;

// Minimum lookahead for next match which compressor tries to guarantee.
// Also minimum amount of allocated space after end of buf (this allows to use things like p[11] without additional checks)
#define LOOKAHEAD 256


// Read next datachunk into buffer, shifting old contents if required
template <class MatchFinder, class Coder>
int read_next_chunk (PackMethod &m, CALLBACK_FUNC *callback, VOID_FUNC *auxdata, MatchFinder &mf, Coder &coder, byte *&p, byte *buf, BYTE *&bufend, BYTE *&table_end, BYTE *&last_found, BYTE *&read_point, int &bytes, int &shift, int &chunk, uint64 &offset)
{
    if (bytes==0)  return 0;     // All input data was successfully compressed
    // If we can't provide 256 byte lookahead then shift data toward buffer beginning,
    // freeing space at buffer end for the new data
    if (bufend-buf > m.buffer-LOOKAHEAD) {
        int sh;
        if (shift==-1) {
            sh = p-(buf+2);  // p should become buf+2 after this shift
            memcpy (buf, buf+sh, bufend-(buf+sh));
            mf.clear_hash (buf);
        } else {
            sh = shift>0? shift : bufend-buf+shift;
            memcpy (buf, buf+sh, bufend-(buf+sh));
            mf.shift (buf, sh);
        }
        p      -= sh;
        bufend -= sh;
        offset += sh;
        if (coder.support_tables)
            table_end  = table_end >buf+sh? table_end -sh : buf,
            last_found = last_found>buf+sh? last_found-sh : buf;
        iterate_var(i,64)  iterate_var(j,64)  last_checked[i][j] = buf;
        mf.invalidate_match();  // invalidate match stored in lazy MF; otherwise it may fuck up the NEXT REPCHAR checking
        coder.shift_occurs();   // Tell to the coder what shift occurs
        debug (printf ("==== SHIFT %08x: p=%08x ====\n", sh, p-buf));
    }
    bytes = callback ("read", bufend, mymin (chunk, buf+m.buffer-bufend), auxdata);
    if (bytes<0)  return bytes;    // Return errcode on error
    bufend += bytes;
    read_point = bytes==0? bufend:bufend-LOOKAHEAD;
    coder.flush();          // Sometimes data should be written to disk :)
    return p<bufend? 1 : 0; // Result code: 1 if we still have bytes to compress, 0 otherwise
}


// Check for data table with N byte elements at current pos
#define CHECK_FOR_DATA_TABLE(N)                                                                     \
{                                                                                                   \
    if (p[-1]==p[N-1]                                                                               \
    &&  uint(p[  N-1] - p[2*N-1] + 4) <= 2*4                                                        \
    &&  uint(p[2*N-1] - p[3*N-1] + 4) <= 2*4                                                        \
    &&  *(int32*)(p+2*N-4) != *(int32*)(p+N-4))                                                     \
    {                                                                                               \
        int type, items;                                                                            \
        if (check_for_data_table (N, type, items, p, bufend, table_end, buf, offset)) {             \
            coder.encode_table (type, items);                                                       \
            /* If data table was diffed, we should invalidate match cached by lazy match finder */  \
            mf.invalidate_match();                                                                  \
            goto found;                                                                             \
        }                                                                                           \
    }                                                                                               \
}


// tor_compress template parameterized by MatchFinder and Coder
template <class MatchFinder, class Coder>
int tor_compress0 (PackMethod m, CALLBACK_FUNC *callback, VOID_FUNC *auxdata)
{
    // Make buffer at least 32kb long and round its size up to 4096 bytes chunk
    m.buffer = ((mymax (m.buffer, 32*kb)-1) & ~4095) + 4096;
    // Buffer for input data
    byte *buf = (byte*) malloc (m.buffer+LOOKAHEAD);    if (!buf)  return FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;
    // >0: shift data in these chunks, <0: how many old bytes should be kept when buf shifts,
    // -1: don't slide buffer, fill it with new data instead
    int shift = m.shift?  m.shift  :  (m.hash_row_width>4? m.buffer/4   :
                                       m.hash_row_width>2? m.buffer/2   :
                                       m.hashsize>=512*kb? m.buffer/4*3 :
                                                           -1);
    // Read data in these chunks
    int chunk = mymin (shift>0? shift:m.buffer, LARGE_BUFFER_SIZE);
    uint64 offset = 0;                        // Current offset of buf[] contents relative to file (increased with each shift() operation)
    int bytes = callback ("read", buf, chunk, auxdata);   // Number of bytes read by last "read" call
    if (bytes<0)  {free(buf); return bytes;}
    BYTE *bufend = buf + bytes;               // Current end of real data in buf[]
    BYTE *matchend = bufend - mymin (MAX_HASHED_BYTES, bufend-buf);   // Maximum pos where match may finish (less than bufend in order to simplify hash updating)
    BYTE *read_point = bufend-mymin(LOOKAHEAD,bytes); // Next point where next chunk of data should be read to buf
    iterate_var(i,64)  iterate_var(j,64)  last_checked[i][j] = buf;
    // If hash is too large - make it smaller
    if (m.hashsize/4>m.buffer)  m.hashsize = 1<<lb(m.buffer*4);
    // Match finder will search strings similar to current one in previous data
    MatchFinder mf (buf, m.hashsize, m.hash_row_width);
    // Coder will encode LZ output down to bits and put them to outstream
    Coder coder (callback, auxdata, HUGE_BUFFER_SIZE, chunk*2);   // Data should be written in HUGE_BUFFER_SIZE chunks (at least) plus chunk*2 bytes should be allocated to ensure that no buffer overflow may occur (because we flush() data only after processing each 'chunk' input bytes)
    // If MatchFinder or Coder was not properly initialized, then return their error code
    if (mf.error()    != FREEARC_OK)  {free(buf); return mf.error();}
    if (coder.error() != FREEARC_OK)  {free(buf); return coder.error();}
    BYTE *table_end  = coder.support_tables? buf : buf+m.buffer+LOOKAHEAD;    // The end of last data table processed
    BYTE *last_found = buf;                    // Last position where data table was found
    // Use first output bytes to store encoding_method, minlen and buffer size
    coder.put8 (m.encoding_method);
    coder.put8 (mf.min_length());
    coder.put32(m.buffer);
    // Encode first four bytes directly (at least 2 bytes should be saved directly in order to avoid problems with using p-2 in MatchFinder.update())
    for (BYTE *p=buf; p<buf+4; p++) {
        if (p>=bufend)  goto finished;
        coder.encode (0, p, buf, mf.min_length());
    }

    // ========================================================================
    // MAIN CYCLE: FIND AND ENCODE MATCHES UNTIL DATA END
    for (BYTE *p=buf+4; TRUE; ) {
        // Read next chunk of data if all data up to read_point was already processed
        if (p >= read_point) {
            byte *p1=p;  // This trick allows to not take address of p and this buys us a bit better program optimization
            int res = read_next_chunk (m, callback, auxdata, mf, coder, p1, buf, bufend, table_end, last_found, read_point, bytes, shift, chunk, offset);
            p=p1, matchend = bufend - mymin (MAX_HASHED_BYTES, bufend-buf);
            if (res==0)  goto finished;            // All input data was successfully compressed
            if (res<0)   {free(buf); return res;}  // Error occured while reading data
        }

        // Check for data table which may be subtracted to improve compression
        if (p > table_end) {
            CHECK_FOR_DATA_TABLE (2);
            CHECK_FOR_DATA_TABLE (4);
/*            if (p > table_end2) {
            CHECK_FOR_DATA_TABLE (3);
            CHECK_FOR_DATA_TABLE (5);
            CHECK_FOR_DATA_TABLE (6);
            CHECK_FOR_DATA_TABLE (7);
            CHECK_FOR_DATA_TABLE (8);
            CHECK_FOR_DATA_TABLE (9);
            CHECK_FOR_DATA_TABLE (10);
            CHECK_FOR_DATA_TABLE (11);
            CHECK_FOR_DATA_TABLE (12);
            CHECK_FOR_DATA_TABLE (13);
            CHECK_FOR_DATA_TABLE (14);
            CHECK_FOR_DATA_TABLE (15);
            CHECK_FOR_DATA_TABLE (16);
            CHECK_FOR_DATA_TABLE (20);
            CHECK_FOR_DATA_TABLE (24);
            CHECK_FOR_DATA_TABLE (32);
            }*/
            if (p-last_found > table_dist)  table_end = p + table_shift;
            goto not_found;
            found: last_found=table_end;
            not_found:;
        }

        // Find match length and position
        UINT len = mf.find_matchlen (p, matchend, 0);
        BYTE *q  = mf.get_matchptr();
        // Encode either match or literal
        if (!coder.encode (len, p, q, mf.min_length())) {      // literal encoded
            print_literal (p-buf+offset, *p); p++;
        } else {                                               // match encoded
            // Update hash and skip matched data
            check_match (p, q, len);
            print_match (p-buf+offset, len, p-q);
            mf.update_hash (p, len, m.update_step);
            p += len;
        }
    }
    // END OF MAIN CYCLE
    // ========================================================================

finished:
    stat (printf("\nTables %d * %d = %d bytes\n", int(table_count), int(table_sumlen/mymax(table_count,1)), int(table_sumlen)));
    // Return mf/coder error code or mark data end and flush coder
    if (mf.error()   !=FREEARC_OK)   {free(buf); return mf.error();}
    if (coder.error()!=FREEARC_OK)   {free(buf); return coder.error();}
    coder.encode (IMPOSSIBLE_LEN, buf, buf-IMPOSSIBLE_DIST, mf.min_length());
    coder.finish();
    free(buf);
    return coder.error();
}


// tor_compress template parameterized by MatchFinder and Coder
template <class MatchFinder, class Coder>
int tor_compress4 (PackMethod m, CALLBACK_FUNC *callback, VOID_FUNC *auxdata)
{
    switch (m.match_parser) {
    case GREEDY: return tor_compress0 <             MatchFinder,  Coder> (m, callback, auxdata);
    case LAZY:   return tor_compress0 <LazyMatching<MatchFinder>, Coder> (m, callback, auxdata);
    }
}

// tor_compress template parameterized by MatchFinder and Coder
template <class MatchFinder, class Coder>
int tor_compress3 (PackMethod m, CALLBACK_FUNC *callback, VOID_FUNC *auxdata)
{
    switch (m.hash3log) {
    case 0:  return tor_compress4 <      MatchFinder,  Coder> (m, callback, auxdata);
    default: return tor_compress4 <Hash3<MatchFinder>, Coder> (m, callback, auxdata);
    }
}

// tor_compress template parameterized by Coder
template <class Coder>
int tor_compress2 (PackMethod m, CALLBACK_FUNC *callback, VOID_FUNC *auxdata)
{
    if (m.caching_finder)   // Use one caching match finder for any hash_row_width
        return tor_compress3 <CachingMatchFinderN, Coder> (m, callback, auxdata);
    else switch (m.hash_row_width) {
    case 1:    return tor_compress3 <MatchFinder1, Coder> (m, callback, auxdata);
    case 2:    return tor_compress3 <MatchFinder2, Coder> (m, callback, auxdata);
    default:   return tor_compress3 <MatchFinderN, Coder> (m, callback, auxdata);
    }
}

// Compress data from buf to outbuf using compression method m
int tor_compress (PackMethod m, CALLBACK_FUNC *callback, VOID_FUNC *auxdata)
{
// When FULL_COMPILE is defined, we compile all 64 possible coder variants
// Otherwise. we compile only 5 variants actually used by -0..-12 predefined modes
#ifdef FULL_COMPILE
    switch (m.encoding_method) {
    case STORING:   // Storing - go to any tor_compress2 call
    case BYTECODER: // Byte-aligned encoding
                    return tor_compress2 <LZ77_ByteCoder>  (m, callback, auxdata);
    case BITCODER:  // Bit-precise encoding
                    return tor_compress2 <LZ77_BitCoder>   (m, callback, auxdata);
    case HUFCODER:  // Huffman encoding
                    return tor_compress2 <LZ77_Coder <HuffmanEncoder<EOB_CODE> > > (m, callback, auxdata);
    case ARICODER:  // Arithmetic encoding
                    return tor_compress2 <LZ77_Coder <ArithCoder<EOB_CODE> >     > (m, callback, auxdata);
    }
#else
    if (m.encoding_method==BYTECODER && m.hash_row_width==1 && m.hash3log==0 && !m.caching_finder && m.match_parser==GREEDY ||
        m.encoding_method==STORING ) {
        return tor_compress0 <MatchFinder1, LZ77_ByteCoder> (m, callback, auxdata);
    } else if (m.encoding_method==HUFCODER && m.hash_row_width==1 && m.hash3log==0 && !m.caching_finder && m.match_parser==GREEDY ) {
        return tor_compress0 <MatchFinder1, LZ77_Coder<HuffmanEncoder<EOB_CODE> > > (m, callback, auxdata);
    } else if (m.encoding_method==HUFCODER && m.hash_row_width==2 && m.hash3log==0 && !m.caching_finder && m.match_parser==GREEDY ) {
        return tor_compress0 <MatchFinder2, LZ77_Coder<HuffmanEncoder<EOB_CODE> > > (m, callback, auxdata);
    } else if (m.encoding_method==HUFCODER && m.hash_row_width>=2 && m.hash3log==0 && m.caching_finder && m.match_parser==GREEDY ) {
        return tor_compress0 <CachingMatchFinderN, LZ77_Coder<HuffmanEncoder<EOB_CODE> > > (m, callback, auxdata);
    } else if (m.encoding_method==HUFCODER && m.hash_row_width>=2 && m.hash3log>0  &&  m.caching_finder && m.match_parser==LAZY ) {
        return tor_compress0 <LazyMatching<Hash3<CachingMatchFinderN> >, LZ77_Coder<HuffmanEncoder<EOB_CODE> > > (m, callback, auxdata);
    } else if (m.encoding_method==ARICODER && m.hash_row_width>=2 && m.hash3log>0  &&  m.caching_finder && m.match_parser==LAZY ) {
        return tor_compress0 <LazyMatching<Hash3<CachingMatchFinderN> >, LZ77_Coder<ArithCoder<EOB_CODE> > > (m, callback, auxdata);
    } else {
        return FREEARC_ERRCODE_INVALID_COMPRESSOR;
    }
#endif
}


// LZ77 decompressor ******************************************************************************

// If condition is true, write data to outstream
#define WRITE_DATA_IF(condition)                                                                  \
{                                                                                                 \
    if (condition) {                                                                              \
        if (decoder.error() != FREEARC_OK)  goto finished;                                        \
        tables.undiff_tables (write_start, output);                                               \
        WRITE (write_start, output-write_start);                                                  \
        tables.diff_tables (output);                                                              \
        write_start = output;  /* next time we should start writing from this pos */              \
                                                                                                  \
        /* Check that we should shift the output pointer to start of buffer */                    \
        if (output >= outbuf + bufsize) {                                                         \
            offset      += output-outbuf;                                                         \
            write_start -= output-outbuf;                                                         \
            write_end   -= output-outbuf;                                                         \
            tables.shift (output,outbuf);                                                         \
            output      -= output-outbuf;  /* output = outbuf; */                                 \
        }                                                                                         \
                                                                                                  \
        /* If we wrote data because write_end was reached (not because */                         \
        /* table list was filled), then set write_end into its next position */                   \
        if (write_start >= write_end) {                                                           \
            /* Set up next write chunk to HUGE_BUFFER_SIZE or until buffer end - whatever is smaller */ \
            write_end = write_start + mymin (outbuf+bufsize-write_start, HUGE_BUFFER_SIZE);       \
        }                                                                                         \
    }                                                                                             \
}


template <class Decoder>
int tor_decompress0 (CALLBACK_FUNC *callback, VOID_FUNC *auxdata, int bufsize, int minlen)
{
    int errcode = FREEARC_OK;                              // Error code of last "write" call
    Decoder decoder (callback, auxdata);                   // LZ77 decoder parses raw input bitstream and returns literals&matches
    if (decoder.error() != FREEARC_OK)  return decoder.error();
    bufsize = mymax (bufsize, HUGE_BUFFER_SIZE);          // Make sure that outbuf is at least 8mb in order to avoid excessive disk seeks
    BYTE *outbuf = (byte*) malloc (bufsize+512*2);        // Circular buffer for decompressed data
    if (!outbuf)  return FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;
    outbuf += 512;                        // We need at least 512 bytes available before and after outbuf in order to simplify datatables undiffing
    BYTE *output      = outbuf;           // Output pointer
    BYTE *write_start = outbuf;           // Data up to this point was already writen to outsream
    BYTE *write_end   = outbuf + mymin (bufsize, HUGE_BUFFER_SIZE); // Flush buffer when output pointer reaches this point
    uint64 offset = 0;                    // Current outfile position corresponding to beginning of outbuf
    DataTables tables;                    // Info about data tables that should be undiffed
    for (;;) {
        // Check whether next input element is a literal or a match
        if (decoder.is_literal()) {
            // Decode it as a literal
            BYTE c = decoder.getchar();
            print_literal (output-outbuf+offset, c);
            *output++ = c;
            WRITE_DATA_IF (output >= write_end);  // Write next data chunk to outstream if required

        } else {
            // Decode it as a match
            UINT len  = decoder.getlen(minlen);
            UINT dist = decoder.getdist();
            print_match (output-outbuf+offset, len, dist);

            // Check for good match (i.e. match not requiring any special handling, >99% of matches are belong to this class)
            if (output-outbuf>=dist && write_end-output>len) {
                BYTE *p = output-dist;
                do   *output++ = *p++;
                while (--len);

            // Check that it's a proper match
            } else if (len<IMPOSSIBLE_LEN) {
                // Slow match-copyoing route for cases when output-dist points before buffer beginning,
                // or p may wrap at buffer end, or output pointer may run over write point
                BYTE *p  =  output-outbuf>=dist? output-dist : output-dist+bufsize;
                do {
                    *output++ = *p++;
                    if (p==outbuf+bufsize)  p=outbuf;
                    WRITE_DATA_IF (output >= write_end);
                } while (--len);

            // Check for special len/dist code used to encode EOF
            } else if (len==IMPOSSIBLE_LEN && dist==IMPOSSIBLE_DIST) {
                goto done;

            // Otherwise it's a special code used to represent info about diffed data tables
            } else {
                // Add new table to list: len-IMPOSSIBLE_LEN encodes type of table and dist encodes its length
                tables.add (len-IMPOSSIBLE_LEN, output, dist);
                stat (printf ("\n%d: Start %x, end %x, length %d      ", len-IMPOSSIBLE_LEN, int(output-outbuf+offset), int(output-outbuf+offset+(len-IMPOSSIBLE_LEN)*dist), (len-IMPOSSIBLE_LEN)*dist));
                // If list of data tables is full then flush it by preprocessing
                // and writing to outstream already filled part of outbuf
                WRITE_DATA_IF (tables.filled());
            }
        }
    }
done:  // Flush outbuf
    WRITE_DATA_IF (TRUE);
finished:
    free(outbuf-512);
    // Return errcode, decoder error code or FREEARC_OK
    return errcode>=0? decoder.error() : errcode;
}


int tor_decompress (CALLBACK_FUNC *callback, VOID_FUNC *auxdata)
{
    int errcode;
    // First 6 bytes of compressed data are encoding method, minimum match length and buffer size
    BYTE buf[2];          READ (buf, 2);
    uint encoding_method; encoding_method = buf[0];
    uint minlen;          minlen          = buf[1];
    uint bufsize;         READ4 (bufsize);

    switch (encoding_method) {
    case BYTECODER:
            return tor_decompress0 <LZ77_ByteDecoder> (callback, auxdata, bufsize, minlen);

    case BITCODER:
            return tor_decompress0 <LZ77_BitDecoder>  (callback, auxdata, bufsize, minlen);

    case HUFCODER:
            return tor_decompress0 <LZ77_Decoder <HuffmanDecoder<EOB_CODE> > > (callback, auxdata, bufsize, minlen);

    case ARICODER:
            return tor_decompress0 <LZ77_Decoder <ArithDecoder<EOB_CODE> >   > (callback, auxdata, bufsize, minlen);
    }
finished: return errcode;
}

