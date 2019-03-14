// Tornado - fast LZ77-based compression algorithm.
// This module contains driver that uses Tornado library to compress files.
// (c) Bulat Ziganshin <Bulat.Ziganshin@gmail.com>
#include "Tornado.cpp"
#include "../Standalone.h"

// Codec and parser names for humans
const char *codec_name[]  = {"storing", "bytecoder", "bitcoder", "hufcoder", "aricoder"};
const char *parser_name[] = {"", "greedy", "lazy", "flexible", "optimal"};

// Returns human-readable method description
char *name (PackMethod method)
{
    static char namebuf[100], h[100], b[100], u[100];
    int c  = method.encoding_method;
    int l  = method.hash_row_width;
    showMem (method.hashsize, h);
    showMem (method.buffer,   b);
    int x  = method.caching_finder;
    int p  = method.match_parser;
    int h3 = method.hash3log;
    sprintf (u, method.update_step<999? " u%d":"", method.update_step);
    sprintf (namebuf, c==STORING? codec_name[c] : "%s %s hash%d%s%s%s, buffer %s, %s",
             parser_name[p], h, l, x?"x":"", h3?"+3byte":"", u, b, codec_name[c]);
    return namebuf;
}

// Structure for recording compression statistics and zero record of this type
struct Results {uint64 insize, outsize; double time;} r0;

// Input and output files business
FILE *fin, *fout;
uint64 filesize;  // Size of input file
Results r;        // Accumulator for compression statistics
int read_infile (void *buf, int size)
{
    r.time -= getGlobalTime();
    int n = read (fin, buf, size);
    r.insize += n;
    if (r.insize!=filesize)  printf("\b\b\b%2d%%", int(double(r.insize)*100/filesize));
    r.time += getGlobalTime();
    return n;
}
int write_outfile (void *buf, int size)
{
    r.time -= getGlobalTime();
    write (fout, buf, size);
    r.outsize += size;
    r.time += getGlobalTime();
    return size;
}


int main (int argc, char **argv)
{
    // Operation mode
    enum MODE {AUTO, COMPRESS, DECOMPRESS, BENCHMARK, HELP}  mode, global_mode=AUTO;

    // Default compression parameters are equivalent to option -5
    PackMethod method = std_Tornado_method [default_Tornado_method];

    // Count of files to process
    int fcount=0;

    // Output path/filename
    const char *output_filename = NULL;

    // Process options until "--"
    // 1. First, process -0..-12 option if any
    for (char **argv_ptr = argv; *++argv_ptr!=NULL; ) {
        char *param = *argv_ptr;
        if (*param == '-') {
            param++;
                 if (strcasecmp(param,"-")==0)      break;
            else if (isdigit(*param))            method = std_Tornado_method [mymin(atoi(param),elements(std_Tornado_method)-1)];
        }
    }
    // 2. Second, process rest of options
    for (char **argv_ptr = argv; *++argv_ptr!=NULL; ) {
        char *param = *argv_ptr;
        if (*param != '-') {
            fcount++;
        } else { param++;  int error=0;
                 if (strcasecmp(param,"-")==0)      break;
            else if (strcasecmp(param,"z")==0)      global_mode=COMPRESS;
            else if (strcasecmp(param,"d")==0)      global_mode=DECOMPRESS;
            else if (strcasecmp(param,"h")==0)      global_mode=HELP;
            else if (strcasecmp(param,"b")==0)      method.buffer=INT_MAX/2+1;
            else if (strcasecmp(param,"x")==0)      method.caching_finder = 1;
            else if (strcasecmp(param,"x+")==0)     method.caching_finder = 1;
            else if (strcasecmp(param,"x-")==0)     method.caching_finder = 0;
            else if (strcasecmp(param,"s")==0)      method.hash3log = 1;
            else if (strcasecmp(param,"s+")==0)     method.hash3log = 1;
            else if (strcasecmp(param,"s-")==0)     method.hash3log = 0;
            else if (isdigit(*param))            ; // -0..-12 option is already processed :)
            else switch( tolower(*param++) ) {
                case 'c': method.encoding_method = parseInt (param, &error); break;
                case 'l': method.hash_row_width  = parseInt (param, &error); break;
                case 'b': method.buffer          = parseMem (param, &error); break;
                case 'd': table_dist             = parseMem (param, &error); break;
                case 's': table_shift            = parseMem (param, &error); break;
                case 'p': method.match_parser    = parseInt (param, &error); break;
                case 'o': output_filename        = *param? param : "nul"; break;
                case 'h': method.hashsize        = parseMem (param, &error); break;
                case 'u': method.update_step     = parseInt (param, &error); break;
                default : printf( "\n Unknown option '%s'\n", param-2);
                          exit(1);
            }
            if (error) {
                printf( "\n Bad format of option value: '%s'\n", param-2);
                exit(1);
            }
        }
    }
    if (global_mode==HELP || fcount==0) {
        char h[100], b[100];
        showMem (method.hashsize, h);
        showMem (method.buffer,   b);
        printf( "Tornado compressor v0.3 (c) Bulat.Ziganshin@gmail.com  7.11.2007");
        printf( "\n" );
        printf( "\n Usage: tor [options and files in any order]");
        printf( "\n   -#     -- compression level (0..%d), default %d", elements(std_Tornado_method)-1, default_Tornado_method);
        printf( "\n   -z     -- force compression" );
        printf( "\n   -d     -- force decompression" );
        printf( "\n   -oNAME -- output filename/directory (default .tor/.untor)" );
        printf( "\n   -h     -- display this help" );
        printf( "\n   --     -- stop flags processing" );
        printf( "\n" );
        printf( "\n Advanced compression parameters:" );
        printf( "\n   -b#    -- buffer size, default %s", b);
        printf( "\n   -h#    -- hash size, default %s", h);
        printf( "\n   -l#    -- length of hash row (1..9999), default %d", method.hash_row_width);
        printf( "\n   -u#    -- update step (1..9999), default %d", method.update_step);
#ifdef FULL_COMPILE
        printf( "\n   -c#    -- coder (1-bytes,2-bits,3-huf,4-arith), default %d", method.encoding_method);
        printf( "\n   -p#    -- parser (1-greedy,2-lazy), default %d", method.match_parser);
        printf( "\n   -s[-]  -- enable/disable 3-byte hash, default %s", method.hash3log? "-s+":"-s-");
        printf( "\n   -x[-]  -- enable/disable caching match finder, default %s", method.caching_finder? "-x+":"-x-");
#endif
        printf( "\n" );
        exit(1);
    }



    // (De)compress all files given on cmdline
    bool parse_options=TRUE;  // options will be parsed until "--"
    for (char **filename = argv; *++filename!=NULL; )
    {
        // If options are still parsed and this argument starts with "-" - it's an option
        if (parse_options && **filename=='-') {
            if (strequ(*filename,"--"))  parse_options=FALSE;
            continue;
        }

        fin = fopen( *filename, "rb" );
        if (fin == NULL) {
            printf( "\n Can't open %s for read\n", *filename);
            exit(2);
        }

        // Select operation mode if it is not defined in cmdline
        mode = global_mode;
        if (mode==AUTO) {
            if (end_with (*filename, ".tor"))
                mode=DECOMPRESS;
            else
                mode=COMPRESS;
        }

        // Construct output filename if no one is given on cmdline:
        // on compressing - add .tor
        // on unpacking   - remove .tor (and add .untor if file already exists)
        char outname[10000];
        if (mode==BENCHMARK) {
           strcpy (outname, "nul");
        } else if (output_filename) {
            if (is_path_char (last_char (output_filename)))
                sprintf(outname, "%s%s.tor", output_filename, *filename);   // PROBLEM! .untor too
            else
                strcpy (outname, output_filename);
        } else {
            if (mode==COMPRESS) {
                sprintf(outname, "%s.tor", *filename);
            } else {
                strcpy (outname, *filename);
                if (end_with (outname, ".tor"))
                    outname [strlen(outname)-4] = '\0';
                if (file_exists (outname))
                    strcat(outname, ".untor");
            }
        }

        // Open output file
        fout = fopen (outname, "wb");
        if (fout == NULL) {
            printf( "\n Can't open %s for write\n", outname);
            exit(5);
        }

        // Prepare to (de)compression
        filesize = get_flen(fin);
        r = r0;
        r.time -= getGlobalTime();
        char title[200];
        int result;

        // Perform actual (de)compression
        switch (mode) {
        case COMPRESS: {
            printf ("%s: compressing %.0lf kb:  0%%", name(method), double(filesize/1000));
            PackMethod m = method;
            m.buffer = mymin (method.buffer, filesize+LOOKAHEAD*2);
            result = tor_compress (m, read_infile, write_outfile);
            sprintf (title, "\r%s: compressed %.0lf -> %.0lf kb (%3.1lf%%)", name(method), double(r.insize/1000), double(r.outsize/1000), (double)r.outsize*100/r.insize);
//            sprintf (title, "\r%s: compressed %.0lf -> %.0lf b (%3.1lf%%)", name(method), double(r.insize), double(r.outsize), (double)r.outsize*100/r.insize);
            break; }

        case DECOMPRESS: {
            printf("Decompressing %.0lf kb:  0%%", double(filesize/1000));
            result = tor_decompress (read_infile, write_outfile);
            sprintf (title, "\rDecompressed %.0lf -> %.0lf kb (%3.1lf%%)", double(r.insize/1000), double(r.outsize/1000), (double)r.insize*100/r.outsize);
            break; }
        }

        // Finish (de)compression
        r.time += getGlobalTime();
        fclose (fin);
        fclose (fout);

        if (result == FREEARC_OK)  {
            printf( "%s: %.3lf sec\n", title, r.time);
        } else {
            delete_file(outname);
            switch (result) {
            case FREEARC_ERRCODE_INVALID_COMPRESSOR:
                printf("\nThis compression mode isn't supported by small Tornado version, use full version instead!");
                break;
            case FREEARC_ERRCODE_NOT_ENOUGH_MEMORY:
                printf("\nNot enough memory for (de)compression!");
                break;
            default:
                printf("\n(De)compression failed with error code %d!", result);
                break;
            }
            exit(11);
        }

        // going to next file...
    }

    return 0;
}


/*
+l8... - �������� 1 ������ ������� �� ��������� ������ 280 ��
+compare with ideal hash function crc+crc+..
    (((CRCTab[(x)&255] ^ _rotr(CRCTab[((x)>>8)&255],8) ^ _rotr(CRCTab[((x)>>16)&255],16) ^ _rotr(CRCTab[((x)>>24)&255],24)) >> HashShift) & HashMask)
+store unused hash bits + a few more chars in hash   (1.5x speedup)
    491->367 (340 for hash4x), 91->68, 51->43 secs
    +������������ ������ ���� ��� ��� 4� ������
    +��������� ����� ��� len=3,4,5,6
    +��������� t, ������ ��������� ����� ����� �� 7 � ������ len3..5 � ��� �������� ������ ������
    ��������� ������ ����� ���������� ����� � ���-�������
+fast arithmetics! total=2^n
    ��������� ����� ��� ������ ������� �����; ��� ����� bits+arith � ����� ������ ������
+lazy matches                                        (+3.5% compression)
    unsuccessfully tried:
      ush good_length; - reduce lazy search above this match length
      ush max_lazy;    - do not perform lazy search above this match length
      ush nice_length; - quit search above this match length
+arith / huffman / bitio                         (+10% compresion for bit i/o, +20% for huffman)
    byte i/o -> class: +0.3 sec on !all
+3-byte strings
+���������� �������� ������ ������
    +����� �������� ������ �� 0.3% ���� ���������� ��� � 6-�������� ������
+better hash multiplier
-5% less compression of src (l4 h22) compared to mmdet. strange?
-several encoding tables: after char, after small string, large string
-add custom MF for l=4/8 (3/6?) what means -1 sec. on !all
    don't have much meaning because caching MF isn't any worser
+FIXED: MatchFinder2 ����������� � 3-��������� ������� / lazy matching (update_hash ���������� �� ���������� ��� ������� � 3 �����)
+FAST_COMPILE - only 4 models actually used by -1..-12
+������� hash_row_width ������ ������ MatchFinder
+FIXED: caching MF - �������� ����� ������ ������������������ ���������� ������ ������
+sliding window for higher modes (-4/-5 - m.buffer/2, -6 and up - m.buffer/4)
+write data to outstreams in 16mb chunks
+64k-1m non-sliding window for -1..-3
+improved caching MF - memory accesses only for matches>=7 or last check
-max_lazy may improve speed/ratio for -4..-6 modes
-don't check more than one real string (option? only for 2-element hash?)
    -skip checking second string if first is large enough
+[almost] full hash_update for highest modes
+IMPOSSIBLE_LEN/IMPOSSIBLE_DIST for EOF encoding, encode() for first 2 chars
+FIXED: -s- -p2 problem (was returning len==0 instead of MINLEN-1)
-��� lazy ������ ��������� ����� ����. �����, ��������� 3-�������� � ����� 4-��������� ������
+TOO_FAR checks moved into caching MF
+output buffer now flushed only when reading next input chunk
+tor_(de)compress - returns error code or FREEARC_OK
+freearc: ����������� ���� ������ ��� ������ ������
+7z's lazy heuristic
+2-byte strings, +repdist, -repboth, +repchar
+��������� ��������� ������!
+������������ bytecoder
  +large len - a few bytes representation to ensure no overflows
+auto-decrease hash (and buf) for small files
+�������� ����� next match � lazy matcher
-repdistN+-delta - 0.4% �� �������
+HuffmanEncoder::encode2
+fixed: ������������� � �������� �� REPCHAR ������������������ �������� repdist0=1
        ������������� ��������������� �� MMx ��� �������� �� REPCHAR (����: ������� ������ ����� �� �� ������� ��������� ���������)
        ������� diffed table ����� ����� ������
        ������������� p->table_len ������ ����������� len
        write_end ��� �������� �� ������� ������
        read_next_chunk ������ ���������� 0 ���� ������ ������� ������ (��������� ���� ����� �� ����� ��� ����������� ������ � ����� �������� �� �������)
        101..104 �� ������ ��������� ������������� ��� data table codes
-context-based char encoding
  separate coder table after \0 or after \0..\31
split caching hash into two parts - pointers and data
  cyclic hash for large N
��� ���������� ������� � ������ ����� ���������� ��� �� ���� � �������������, ��� ������� ������ ��� ��������� �������
  +������ ����� ��������, ����� ����� ������ ������ (����������� ��� ������������ CMF)
block-static arithmetic coder - may improve compression by 1-2%
? caching MF ��� -l2
? 5/6-byte main hash for highest modes (-7 and up)
hash3+lazy - �������������� � ������ �������, ��������� ��� ������ ������ 3-�������� ������ ����� �����?
��������� ����� ������ ���������� ������� � ������ �������� p+len<bufend
���������� ������������ ��������� � -1/-2/-3? ����� �� �������� �� ������ ����
rolz 1+2+3+4
minor thoughts:
  small outbuf for -5 and higher modes
  increase HUFBLOCKSIZE for -2/-3  (100k - -0.2sec)
  -�������� �������� p+len<=bufend ����� � compress0()

text files -5/-6: disable 2/3-byte searching, repchar and use encode(..., MINLEN=4), switch to hufcoder(?)
hufcoder: disable REPDIST, fast qsort<>
huf&ari: EOB, check for text-like stats, switch into text mode

use only one bit for flag in bytecoder
bitcoder: 30-bit length encoding - make it a part of 8-bit encoding
huf/ari - improve "first block" encoding, adaptation (currently, up to 1/64 of codespace is wasted),
  +EOB code
? �������� ������ �������, ���������������� ������� chunks, storing ����������� ������
    header = 1 byte flags + 3 bytes len
+diffing tables
use REPBOTH for very fast detection of tables of any size
-repboth, repchar1..3
check matches at repdist distances
����� ���������������� disttables ��� ��������� len
��� ������ ������ - if newlen=len+1 and newdist>dist*8 - ignore it
-1,-2,-3?: no MM, no REP*
huf/ari: ������ cnt++ ������ cnt+=10 - ������ ��������� �������� �����������


+-h1mb in cmdline
+-z/-d options, by default auto depending on file extension
+-h1m -9 == -9 -h1m (��������� ������� ����� �������, ����� ���������� ��� �����)
+-odir/ -odir\ -od:
+64-bit insize/outsize
+-b128k, m.hashsize ������ hashlog, print block/hashsize in help with k/m suffix
+CHECK mallocs
dir_exists=file_exists(dir\.) || end_with(:/\)
-t, -f force overwrite, -d delete src files, stdin->stdout by default
make non-inline as much functions as possible (optimize .exe size): +MatchFinder.cpp +LZ77_Coder.cpp
****Tornado 0.2 compressing VC, 41243 kb     --noheader option disables this
****-1: 1kb hash1...: done 5%
****-1: 1kb hash1...: 17876 kb (12.7%), 23.333 sec, 88.6 mb/s
.tor signature, version, flags, crc
? ���������� ������ ������ ����� ������� ���������� chunk � ������������ storing ��� ���������� ������ (�������� huf/ari-table)
? ��������� ��� ����� ����� (������� ��������� ������ �� ������ ������, 200-300 kb �� all)
print predefined methods definitions in help screen
-mem ������ ��������������� ������ ������ �� -1 �� -9?  -bench ��� ���� ���������� ������
tor_compress: ��� ������ ����� ==buffer ���������� ������ ������� ������ ����� ���, ��� �������� 0 ���� :)

Changes in 0.2:
    lazy parsing
    3-byte matches
    huffman coder
    sliding window

Changes in 0.3:
    repdist&repchar0 codes
    2-byte matches
    optimized lz parsing
    table preprocessing
    gzip-like cmdline interface?

    -1 thor e1, quicklz
    -2 thor e2, slug
    -3 thor e3, gzip -1
    -4 gzip, rar -m1
    -5 thor, 7zip -mx1
    -6 uharc -mz
    -7 bzip2, rar -m2
*/
