// LZ77 model *************************************************************************************
// Already tried:
//     no lz if len small and dist large: don't have much sense with our MINLEN=4
//     hash4+3: only 1% gain even on ghc.exe
//     hash5+4: 48->46.7 mb but 2x slower (22->46sec: 240mb compressed using 16mb hash)
// To do:
//     -0x65a8e9b4 for hash
//     +combined len+dist encoding a-la cabarc - will make decoding a bit faster, but who cares? :)
//     -save into hash records unused part of hash value in order to make
//         fast check of usability of this hash slot (like it is already
//         done in REP); would be especially helpful on larger hashes
//     -save into hash record 4 bits of p[5] - would be useful to skip trying second..fourth hash records
//     +save into hash record 4 bytes of data
//     +lazy search (and return of 3-byte strings) for highest compression mode
//     ST4/BWT sorting for exhaustive string searching

// Maximum number of bytes used for hashing in any match finder.
// If this value will be smaller than real, we can hash bytes in buf that are not yet read
#define MAX_HASHED_BYTES 4


#ifdef DEBUG
void check_match (BYTE *p, BYTE *q, int len)
{
    if (p==q || memcmp(p,q,len))   printf("Bad match:  ");
}
void print_literal (int pos, BYTE c)
{
    printf (isprint(c)? "%08x: '%c'\n" : "%08x: \\0x%02x\n", pos, c);
}
void print_match (int pos, int len, int dist)
{
    printf ("%08x: %3d %6d\n", pos, len, -dist);
}
#else
#define check_match(p,q,len)         (0)
#define print_literal(pos,c)         (0)
#define print_match(pos,len,dist)    (0)
#endif


// Settings for 4-byte hashing
#define value(p)     value32(p)
// Just for readability
#define MINLEN       min_length()

int alloced = 0;     // Number of currently allocated hashtables, should be 0 at program exit

// Abstract Match Finder class which defines codebase common for all concrete Match Finders
struct BaseMatchFinder
{
    UINT HashSize, HashShift, HashMask;
    BYTE **HTable, *q;
    int hash_row_width;

    // This contructor is common for several Match Finder classes.
    BaseMatchFinder (BYTE *buf, int hashsize, int _hash_row_width);
    // Returns error code if there is any problem in MF work
    int error();
    // Called on initialization and when next data chunk read into NON-SLIDING buffer
    void clear_hash (BYTE *buf);
    // Called after data was shifted 'shift' bytes backwards in SLIDING WINDOW buf[]
    void shift (BYTE *buf, int shift);
    // Minimal length of matches returned by this Match Finder
    uint min_length()     {return 4;}
    // Returns pointer of last match found (length is returned by find_matchlen() itself)
    byte *get_matchptr()  {return q;}
    // hash function
    uint hash (uint x)    {return ((x*123456791) >> HashShift) & HashMask;}
    // Invalidate previously found match
    void invalidate_match ()   {}
};

// This contructor is common for several match finder classes.
// It allocs and inits HTable and inits Hash* fields
BaseMatchFinder::BaseMatchFinder (BYTE *buf, int hashsize, int _hash_row_width)
{
    hash_row_width = _hash_row_width;
    HashSize  = (1<<lb(hashsize)) / sizeof(*HTable);
    HashShift = 32-lb(HashSize);
    HashMask  = (HashSize-1) & ~(roundup_to_power_of(hash_row_width,2)-1);
    HTable    = (BYTE**) malloc (sizeof(BYTE*) * HashSize);
    clear_hash (buf);
}

// Returns error code if there is any problem in MF work
int BaseMatchFinder::error()
{
    return HTable==NULL?  FREEARC_ERRCODE_NOT_ENOUGH_MEMORY : FREEARC_OK;
}

// Called when next data chunk read into NON-SLIDING buffer
void BaseMatchFinder::clear_hash (BYTE *buf)
{
    if (HTable)  iterate_var(i,HashSize)  HTable[i]=buf+1;
}

// Called after data was shifted 'shift' bytes backwards in SLIDING WINDOW buf[]
// We should make appropriate corrections in HTable. We set HTable entries minimum
// to buf+1 because with lazy matching we can extend match found one byte backward
void BaseMatchFinder::shift (BYTE *buf, int shift)
{
    iterate_var(i,HashSize)  HTable[i] = HTable[i]>buf+shift? HTable[i]-shift : buf+1;
}



// 1-way Match Finder which holds only one entry for each hash value
struct MatchFinder1 : BaseMatchFinder
{
    MatchFinder1 (BYTE *buf, int hashsize, int hash_row_width)
        : BaseMatchFinder (buf, hashsize, hash_row_width) {alloced++;}
    ~MatchFinder1 () { alloced--; free(HTable); }

    uint find_matchlen (byte *p, void *bufend, UINT prevlen)
    {
        UINT h = hash(value(p));
        q = HTable[h];  HTable[h] = p;
        if (value(p) == value(q)) {
            UINT len;
            for (len=MINLEN-1; p+len+4<bufend && value32(p+len)==value32(q+len);  len+=4);
            for (            ; p+len<bufend   && p[len]==q[len];                  len++);
            return len;
        } else {
            return MINLEN-1;
        }
    }
    void update_hash (BYTE *p, UINT len, UINT step)
    {
// No hash update in fastest mode!
/*        uint h;
        h = hash(value(p+1)),  HTable[h] = p+1;
        p += len;
        h = hash(value(p-2)),  HTable[h] = p-2;
        h = hash(value(p-1)),  HTable[h] = p-1;
*/    }
};


// 2-way Match Finder
struct MatchFinder2 : BaseMatchFinder
{
    MatchFinder2 (BYTE *buf, int hashsize, int hash_row_width)
        : BaseMatchFinder (buf, hashsize, hash_row_width) {alloced++;}
    ~MatchFinder2 () { alloced--; free(HTable); }

    uint find_matchlen (byte *p, void *bufend, UINT prevlen)
    {
        UINT len;
        UINT h = hash(value(p));
        q = HTable[h]; BYTE *q1 = HTable[h+1];
        HTable[h+1] = q, HTable[h] = p;
        if (value(p) == value(q)) {
            for (len=MINLEN-1;  p+len+4<bufend && value32(p+len)==value32(q+len);  len+=4);
            for (            ;  p+len<bufend && p[len]==q[len];                    len++);

            if (p[len] == q1[len]) {
                UINT len1;
                for (len1=0; p+len1<bufend && p[len1]==q1[len1]; len1++);
                if (len1>len)  len=len1, q=q1;
            }
            return len;
        } else if (value(p) == value(q1)) {
            q=q1;
            for (len=MINLEN-1;  p+len+4<bufend && value32(p+len)==value32(q+len);  len+=4);
            for (            ;  p+len<bufend && p[len]==q[len];                    len++);
            return len;
        } else {
            return MINLEN-1;
        }
    }
    void update_hash (BYTE *p, UINT len, UINT step)
    {   // len may be as low as 2 if LazyMatching and Hash3 are used together
        UINT h;
        h = hash(value(p+1)),  HTable[h+1] = HTable[h],  HTable[h] = p+1;
        p += len;
        h = hash(value(p-2)),  HTable[h+1] = HTable[h],  HTable[h] = p-2;
        h = hash(value(p-1)),  HTable[h+1] = HTable[h],  HTable[h] = p-1;
    }
};


// N-way Match Finder
struct MatchFinderN : BaseMatchFinder
{
    MatchFinderN (BYTE *buf, int hashsize, int hash_row_width)
        : BaseMatchFinder (buf, hashsize, hash_row_width) {alloced++;}
    ~MatchFinderN () { alloced--; free(HTable); }

    uint find_matchlen (byte *p, void *bufend, UINT prevlen)
    {
        UINT h = hash(value(p));
        UINT len = MINLEN-1;
        BYTE *q1, *q0 = q = HTable[h];  HTable[h] = p;
        // Start with checking first element of hash row
        if (value(p) == value(q)) {
            for (len=MINLEN-1;  p+len+4<bufend && value32(p+len)==value32(q+len);  len+=4);
            for (            ;  p+len<bufend && p[len]==q[len];                    len++);
        }
        // Check remaining elements, searching for longer match,
        // and shift entire row toward end
        for (int j=1; j<hash_row_width; j++, q0=q1) {
            q1=HTable[h+j];  HTable[h+j]=q0;
            if (value(p+len+1-MINLEN) == value(q1+len+1-MINLEN)) {
                UINT len1;
                for (len1=0;  p+len1<bufend && p[len1]==q1[len1];  len1++);
                if (len1>len)  len=len1, q=q1;
            }
        }
        return len;
    }

    void update_hash1 (BYTE *p)
    {
        UINT h = hash(value(p));
        for (int j=hash_row_width; --j; HTable[h+j] = HTable[h+j-1]);  HTable[h] = p;
    }
    void update_hash (BYTE *p, UINT len, UINT step)
    {
        update_hash1 (p+1);
        for (int i=2; i<len-1; i+=step)
            update_hash1 (p+i);
        if (len>3) update_hash1 (p+len-1);
    }
};


// This matchfinder caches 4 bytes of string (p[3..6]) in hash itself,
// providing faster search in case of highly-populated hash rows.
// For efficiency reasons I suggest to use it only for hash_row_width>=4,
// buffer>=8mb and hashsize>=2mb.
// This class is compatible only with MINLEN=4 and sizeof(void*)=4
// There are two possible implementation strategies:
//    a) guarantee that caching hash entries contains valid data and use this fact to cut off some tests
//    b) don't make such guarantees and make strict tests of actual data in buf[]
// I have not yet decided which strategy to use :)
struct CachingMatchFinderN : BaseMatchFinder
{
    CachingMatchFinderN (BYTE *buf, int hashsize, int _hash_row_width)
        : BaseMatchFinder (buf, hashsize, _hash_row_width*2)
    {
        alloced++;
        hash_row_width = _hash_row_width;
    }
    ~CachingMatchFinderN() { alloced--; free(HTable); }

    void shift (BYTE *buf, int shift)
    {
        for (int i=0; i<HashSize; i+=2)
            HTable[i]  =  HTable[i] > buf+shift?  HTable[i]-shift  :  buf+1;
    }
    // Key value stored in hash for each string
    uint key (BYTE *p)
    {
        return value32(p+3);
    }
    uint find_matchlen (byte *p, void *bufend, UINT prevlen)
    {
        UINT len;                  // Length of largest string already found
        UINT h = hash(value(p));   // Hash key of string for which we perform match search
        // Pointers to the current hash element and end of hash row
        BYTE **table = HTable+h, **tabend = table + hash_row_width*2;
        // q1/v1 keeps last match candidate and its key (we init them with values which would be saved in the first hash slot)
        BYTE *q1 = p;
        UINT  v1 = key(p);

        // Check hash elements, searching for longest match,
        // while shifting entire row contents towards end
        // (there are five loops here - one used before any match is found,
        //  three are used when a match of size 4/5/6 already found,
        //  and one used when match of size 7+ already found)
len0:
        while (table<tabend) {
            // Read next ptr/key from hash and save here previous pair (shifted from previous position)
            BYTE *q0=q1;  q1 = *table;         *table++ = q0;
            UINT  v0=v1;  v1 = (UINT) *table;  *table++ = (BYTE*)v0;
            UINT t = v1 ^ key(p);
            if ((t&0xff) == 0) {
                     if (t==0)        goto len7;
                else if (t&0xff00)    goto len4;
                else if (t&0xff0000)  goto len5;
                else                  goto len6;
            }
        }
        return MINLEN-1;

len4:   q=q1;
        while (table<tabend) {
            BYTE *q0=q1;  q1 = *table;         *table++ = q0;
            UINT  v0=v1;  v1 = (UINT) *table;  *table++ = (BYTE*)v0;
            UINT t = v1 ^ key(p);
            if ((t&0xffff) == 0) {
                     if (t==0)        goto len7;
                else if (t&0xff0000)  goto len5;
                else                  goto len6;
            }
        }
        return p-q<48*kb && p+4<=bufend && value32(p)==value32(q)?  4 : MINLEN-1;

len5:   q=q1;
        while (table<tabend) {
            BYTE *q0=q1;  q1 = *table;         *table++ = q0;
            UINT  v0=v1;  v1 = (UINT) *table;  *table++ = (BYTE*)v0;
            UINT t = v1 ^ key(p);
            if ((t&0xffffff) == 0) {
                     if (t==0)        goto len7;
                else                  goto len6;
            }
        }
        return p-q<192*kb && p+5<=bufend && value32(p)==value32(q) && p[4]==q[4]?  5 : MINLEN-1;

len6:   q=q1;
        while (table<tabend) {
            BYTE *q0=q1;  q1 = *table;         *table++ = q0;
            UINT  v0=v1;  v1 = (UINT) *table;  *table++ = (BYTE*)v0;
            UINT t = v1 ^ key(p);
            if (t == 0)                goto len7;
        }
        return p-q<1*mb && p+6<=bufend && value32(p)==value32(q) && value16(p+4)==value16(q+4)?  6 : MINLEN-1;

len7:   len = MINLEN-1;
        if (value32(p)==value32(q1)) {
            q=q1;
            for (len=MINLEN-1;  p+len+4<bufend && value32(p+len)==value32(q1+len);  len+=4);
            for (            ;  p+len<bufend   && p[len]==q1[len];                  len++);
        }

        while (table<tabend) {
            BYTE *q0=q1;  q1 = *table;         *table++ = q0;
            UINT  v0=v1;  v1 = (UINT) *table;  *table++ = (BYTE*)v0;
            UINT t = v1 ^ key(p);
            if (t == 0  &&  p[len]==q1[len]  &&  value32(p)==value32(q1)) {
                UINT len1;
                for (len1=MINLEN-1;  p+len1+4<bufend && value32(p+len1)==value32(q1+len1);  len1+=4);
                for (             ;  p+len1<bufend   && p[len1]==q1[len1];                  len1++);
                if (len1>len)  len=len1, q=q1;
            }
        }
        return len;
    }

    // Update half of hash row corresponding to string pointed by p
    // (hash updated via this procedure only when skipping match contents)
    void update_hash1 (BYTE *p)
    {
        UINT h = hash(value(p));
        for (int j=hash_row_width; j-=2; )
            HTable[h+j]   = HTable[h+j-2],
            HTable[h+j+1] = HTable[h+j-1];
        HTable[h]   = p;
        HTable[h+1] = (BYTE*) key(p);
    }
    // Skip match starting from p with length len and update hash with strings using given step
    void update_hash (BYTE *p, UINT len, UINT step)
    {
        // Update hash with strings at the start and end of match plus part of strings inside match
        if (len>1) update_hash1 (p+1);
        for (int i=2; i<len-1; i+=step)
            update_hash1 (p+i);
        if (len>3) update_hash1 (p+len-1);
    }
};


// Checks that bigDist/64 is bigger than smallDist
static inline bool ChangePair (uint smallDist, uint bigDist)
{
    return ((bigDist >> 6) > smallDist);
}

// This is MF transformer which makes lazy MF from ordinary one
template <class MatchFinder>
struct LazyMatching
{
    MatchFinder mf;
    uint nextlen;
    BYTE *prevq, *nextq;

    LazyMatching (BYTE *buf, int hashsize, int hash_row_width)
        : mf (buf, hashsize, hash_row_width)
    {
        nextlen = 0;
    }
    void clear_hash (BYTE *buf)
    {
        mf.clear_hash (buf);
        nextlen = 0;
    }

    void shift (BYTE *buf, int shift)
    {
        mf.shift (buf, shift);
        nextq -= shift;
    }

    int  error()          {return mf.error();}
    uint min_length()     {return mf.min_length();}
    byte *get_matchptr()  {return prevq;}

    uint find_matchlen (byte *p, void *bufend, UINT _prevlen)
    {
        // Find first match at new position
        if (!nextlen) {
            nextlen = mf.find_matchlen (p, bufend, _prevlen);
            nextq   = mf.get_matchptr();
        }
        // Copy "next match" into current one
        uint prevlen = nextlen;
             prevq   = nextq;
        // Find match at next position
        nextlen = mf.find_matchlen (p+1, bufend, prevlen);
        nextq   = mf.get_matchptr();

        uint nextdist = p+1-nextq;
        uint prevdist = p-prevq;

        // Extend match at p+1 one char backward if it's better than match at p
        if (nextlen>=MINLEN && nextq[-1]==*p &&
               (nextlen+1 >= prevlen && nextdist < prevdist ||
                nextlen+1 == prevlen + 1 && !ChangePair(prevdist, nextdist) ||
                nextlen+1 > prevlen + 1 ||
                nextlen+1 + 1 >= prevlen && prevlen >= MINLEN && ChangePair(nextdist, prevdist)))
        {
            debug (printf ("Extending %d:%d -> %d:%d\n",  nextlen, nextq-p, nextlen+1, nextq-1-p));
            prevlen = nextlen+1;
            prevq   = nextq-1;
            return prevlen;
        }

        // Truncate current match if match at next position will be better (LZMA's algorithm)
        if (nextlen >= prevlen && nextdist < prevdist ||
            nextlen == prevlen + 1 && !ChangePair(prevdist, nextdist) ||
            nextlen > prevlen + 1 ||
            nextlen + 1 >= prevlen && prevlen >= MINLEN && ChangePair(nextdist, prevdist))
        {
             return MINLEN-1;
        } else {
             return prevlen;
        }
    }

    void update_hash (BYTE *p, UINT len, UINT step)
    {
        mf.update_hash (p+1, len-1, step);
        nextlen = 0;
    }

    // Invalidate previously found match
    void invalidate_match ()
    {
        mf.invalidate_match();
        nextlen = MINLEN-1;     // We can't assign 0 because pointer to p already inserted into hash - find_match may return q==p
    }
};



// This is MF transformer which adds separate small hashes for searching 2/3-byte strings
template <class MatchFinder>
struct Hash3
{
    MatchFinder mf;
    UINT HashSize, HashSize2;
    BYTE **HTable, **HTable2, *q;

    Hash3 (BYTE *buf, int hashsize, int hash_row_width);
    ~Hash3();
    int  error();
    void clear_hash (BYTE *buf);
    void shift (BYTE *buf, int shift);

    uint min_length()     {return 2;}
    byte *get_matchptr()  {return q;}
    uint hash (uint x)    {return (x*234567913) >> 20;}
    uint hash2(uint x)    {return (x*123456791) >> 22;}

    uint find_matchlen (byte *p, void *bufend, UINT prevlen)
    {
        // Find long match
        UINT len = mf.find_matchlen (p, bufend, prevlen);
               q = mf.get_matchptr();
        // If long match was not found - try to find 3-byte one
        if (/*prevlen<MINLEN && */  len<mf.MINLEN) {
            UINT h = hash(value24(p));
            q = HTable[h];  HTable[h] = p;
            if (p-q<6*kb && p+3<=bufend && value24(p)==value24(q)) {
                UINT h = hash2(value16(p));  HTable2[h] = p;
                return 3;
            } else {
                UINT h = hash2(value16(p));
                q = HTable2[h];  HTable2[h] = p;
                if (p-q<256 && p+2<=bufend && value16(p)==value16(q)) {
                    return 2;
                } else {
                    return MINLEN-1;
                }
            }
        }
        // Update 3-byte hash
        UINT h = hash(value24(p));  HTable[h] = p;
             h = hash2(value16(p)); HTable2[h] = p;
        // And return 4+-byte match
        return len;
    }

    void update_hash1 (BYTE *p)
    {
        UINT h = hash(value24(p));  HTable[h] = p;
        //   h = hash2(value16(p)); HTable2[h] = p;   - HTable2 not updated here
    }
    void update_hash (BYTE *p, UINT len, UINT step)
    {
        mf.update_hash (p, len, step);
        if (len>1) update_hash1 (p+1);
        if (len>3) update_hash1 (p+len-1);
    }
    // Invalidate previously found match
    void invalidate_match ()   {mf.invalidate_match();}
};

template <class MatchFinder>
Hash3<MatchFinder> :: Hash3 (BYTE *buf, int hashsize, int hash_row_width)
    : mf (buf, hashsize, hash_row_width)
{
    HashSize = 4*kb;
    HashSize2= 1*kb;
    HTable = (BYTE**) malloc (sizeof(BYTE*) * HashSize); alloced++;
    HTable2= (BYTE**) malloc (sizeof(BYTE*) * HashSize2);
    clear_hash (buf);
}

template <class MatchFinder>
Hash3<MatchFinder> :: ~Hash3()
{
    alloced--; free(HTable); free(HTable2);
}

template <class MatchFinder>
int Hash3<MatchFinder> :: error()
{
    return HTable==NULL || HTable2==NULL?  FREEARC_ERRCODE_NOT_ENOUGH_MEMORY : mf.error();
}

template <class MatchFinder>
void Hash3<MatchFinder> :: clear_hash (BYTE *buf)
{
    mf.clear_hash (buf);
    if (HTable)  iterate_var(i,HashSize)   HTable[i] =buf+1;
    if (HTable2) iterate_var(i,HashSize2)  HTable2[i]=buf+1;
}

template <class MatchFinder>
void Hash3<MatchFinder> :: shift (BYTE *buf, int shift)
{
    mf.shift (buf, shift);
    iterate_var(i,HashSize)  HTable[i] = HTable[i] >buf+shift? HTable [i]-shift : buf+1;
    iterate_var(i,HashSize2) HTable2[i]= HTable2[i]>buf+shift? HTable2[i]-shift : buf+1;
}


#undef MINLEN
