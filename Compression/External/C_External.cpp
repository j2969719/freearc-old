#include <stdio.h>
#include <string.h>
extern "C" {
#include "C_External.h"
}


int external_program (bool IsCompressing, CALLBACK_FUNC *callback, VOID_FUNC *auxdata, char *infile, char *outfile, char *cmd, char *name, int MinCompression, double *addtime)
{
    BYTE* Buf = (BYTE*) malloc(LARGE_BUFFER_SIZE);  // �����, ������������ ��� ������/������ ������
    int x;                                          // ���, ������������ ��������� ��������� ������/������

    // TRUE, ���� � ������ ������� ������ ������������ 0/1 - ������ �������/�����
    bool useHeader = !strequ(name,"tempfile");

    // ��������� ������� ������ �� ��������� ����
    remove (infile);
    remove (outfile);
    FILE *f = NULL;
    int bytes = 0;
    BYTE runCmd = 1;
    if (!IsCompressing && useHeader)  checked_read (&runCmd, 1);
    while ( (x = callback ("read", Buf, LARGE_BUFFER_SIZE, auxdata)) > 0 )
    {
        if (f==NULL)  {f = fopen (infile, "wb");  // �� ��������� ���� ���� �� ������ ���� �������-������ ������ (��� ������� ������� � ������������ �����-������)
                       registerTemporaryFile (infile,f);}
        if (runCmd!=0 && runCmd!=1) {            // ��� ������������� �� ������� �������� FreeArc, ������� �� ��������� 1 ����� ������� ������� (������ �� FreeArc 0.50!)
            outfile = "data7777";
            write (f, &runCmd, 1);   bytes+=1;
            runCmd=1;
        }
        write (f, Buf, x);   bytes+=x;
    }
    FreeAndNil(Buf);
    unregisterTemporaryFile (infile);
    fclose (f);
    if (x)  {remove (infile); return x;}   // ���� ��� ������ ��������� ������ - �������


    // ���� cmd ����� - ���� ������������ ������ ��� ����������� ������ ����� ���������� �������.
    // ���� runCmd==0 - ������ ���� ����������� ��� ������
    registerTemporaryFile (infile);
    registerTemporaryFile (outfile);
    if (*cmd && runCmd) {
        printf ("\n%s %d bytes with ", IsCompressing? "Compressing":"Unpacking", bytes);
        double time0 = getGlobalTime();
        system (cmd);
        if (addtime)  *addtime += getGlobalTime() - time0;
    } else {
        rename (infile, outfile);
    }


    // ��������� �������� ������ �� ���������� �����
    f = fopen (outfile, "rb" );
    if (f) {
        registerTemporaryFile (outfile,f);
        unregisterTemporaryFile (infile);
        remove (infile);
        BYTE compressed[1] = {1};
        if (IsCompressing && useHeader)     checked_write(compressed,1);
    } else {
        unregisterTemporaryFile (outfile);
        unregisterTemporaryFile (infile);
        if (IsCompressing && !useHeader)    {remove (infile); return FREEARC_ERRCODE_GENERAL;}
        outfile = infile;
        f = fopen (outfile, "rb" );
        if (!f)                             {remove (outfile); return FREEARC_ERRCODE_GENERAL;}
        registerTemporaryFile (outfile,f);
        BYTE uncompressed[1] = {0};
        if (IsCompressing)                  checked_write(uncompressed,1);
    }
    QUASIWRITE (get_flen(f));
    Buf = (BYTE*) malloc(LARGE_BUFFER_SIZE);
    while ((x = read (f, Buf, LARGE_BUFFER_SIZE)) > 0)
    {
        checked_write (Buf, x);
    }
finished:
    unregisterTemporaryFile (outfile);
    fclose (f);
    remove (outfile);
    FreeAndNil(Buf);
    return x;         // 0, ���� �� � �������, � ��� ������ �����
}


#ifndef FREEARC_DECOMPRESS_ONLY
int external_compress (char *name, char *packcmd, char *unpackcmd, char *datafile, char *packedfile, CALLBACK_FUNC *callback, VOID_FUNC *auxdata, double *addtime)
{
    return external_program (TRUE, callback, auxdata, datafile, packedfile, packcmd, name, 0, addtime);
}
#endif  // !defined (FREEARC_DECOMPRESS_ONLY)

int external_decompress (char *name, char *packcmd, char *unpackcmd, char *datafile, char *packedfile, CALLBACK_FUNC *callback, VOID_FUNC *auxdata, double *addtime)
{
    return external_program (FALSE, callback, auxdata, packedfile, datafile, unpackcmd, name, 0, addtime);
}


/*-------------------------------------------------*/
/* ���������� ������ EXTERNAL_METHOD               */
/*-------------------------------------------------*/

// ������� ����������
int EXTERNAL_METHOD::decompress (CALLBACK_FUNC *callback, VOID_FUNC *auxdata)
{
    return external_decompress (name, packcmd, unpackcmd, datafile, packedfile, callback, auxdata, &addtime);
}

#ifndef FREEARC_DECOMPRESS_ONLY

// ������� ��������
int EXTERNAL_METHOD::compress (CALLBACK_FUNC *callback, VOID_FUNC *auxdata)
{
    return external_compress (name, packcmd, unpackcmd, datafile, packedfile, callback, auxdata, &addtime);
}

// �������� � buf[MAX_METHOD_STRLEN] ������, ����������� ����� ������ � ��� ��������� (�������, �������� � parse_EXTERNAL)
void EXTERNAL_METHOD::ShowCompressionMethod (char *buf)
{
    if (strequ (name, "pmm")) {
        char MemStr[100];
        showMem (cmem, MemStr);
        sprintf (buf, "pmm:%d:%s%s", order, MemStr, MRMethod==2? ":r2": (MRMethod==0? ":r0":""));
    } else {
        strcpy (buf, name);
    }
}

// �������� ����������� � ������, ������ ������������ order
void EXTERNAL_METHOD::SetCompressionMem (MemSize _mem)
{
  if (can_set_mem && _mem>0) {
    order  +=  int (trunc (log(double(_mem)/cmem) / log(2) * 4));
    cmem=dmem=_mem;
  }
}

#endif  // !defined (FREEARC_DECOMPRESS_ONLY)


// ������������ ������ ���� EXTERNAL_METHOD/PPMonstr � ��������� ����������� ��������
// ��� ���������� NULL, ���� ��� ������ ����� ������ ��� �������� ������ � ����������
COMPRESSION_METHOD* parse_PPMONSTR (char** parameters)
{
  // ���� �������� ������ (������� ��������) - "pmm", �� ������� ��������� ���������
  if (strcmp (parameters[0], "pmm") == 0) {
    // ��������� �������� ���������� ��� ������ ������ PPMonstr
    EXTERNAL_METHOD *p = new EXTERNAL_METHOD;
    p->name           = "pmm";
    p->MinCompression = 100;
    p->can_set_mem    = TRUE;
    p->order          = 16;
    p->cmem           = 192*mb;
    p->dmem           = 192*mb;
    p->MRMethod       = 1;
    p->datafile       = "$$arcdatafile$$.tmp";
    p->packedfile     = "$$arcdatafile$$.pmm";

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
        case 'm':  p->cmem = p->dmem = parseMem (param+1, &error); continue;
        case 'o':  p->order          = parseInt (param+1, &error); continue;
        case 'r':  p->MRMethod       = parseInt (param+1, &error); continue;
      }
      // ���� �� ��������, ���� � ��������� �� ������� ��� ��������
      // ���� ���� �������� ������� ��������� ��� ����� ����� (�.�. � ��� - ������ �����),
      // �� �������� ��� �������� ���� order, ����� ��������� ��������� ��� ��� mem
      int n = parseInt (param, &error);
      if (!error) p->order = n;
      else        error=0, p->cmem = p->dmem = parseMem (param, &error);
    }
    if (error)  {delete p; return NULL;}  // ������ ��� �������� ���������� ������

    // ������ packcmd/unpackcmd ��� PPMonstr
    char cmd[100];
    sprintf (cmd, "ppmonstr e -o%d -m%d -r%d %s", p->order, p->cmem>>20, p->MRMethod, p->datafile);
    p->packcmd = strdup_msg(cmd);
    sprintf (cmd, "ppmonstr d %s", p->packedfile);
    p->unpackcmd = strdup_msg(cmd);

    return p;
  } else {
    return NULL;   // ��� �� ����� PPMONSTR
  }
}

static int PPMONSTR_x = AddCompressionMethod (parse_PPMONSTR);   // �������������� ������ ������ PPMONSTR




// ��������� ������������ ������� ����������� **********************************************************************

// ������������ ������ ���� EXTERNAL_METHOD � ��������� ����������� ��������
// ��� ���������� NULL, ���� ��� ������ ����� ������ ��� �������� ������ � ����������
COMPRESSION_METHOD* parse_EXTERNAL (char** parameters, void *method_template)
{
  if (strequ (parameters[0], ((EXTERNAL_METHOD*)method_template)->name)) {
    // ���� �������� ������ (������� ��������) ������������� �������� ������������ EXTERNAL ������, �� ������� ��������� ���������

    EXTERNAL_METHOD *p = new EXTERNAL_METHOD (*(EXTERNAL_METHOD*)method_template);
    int error = 0;  // ������� ����, ��� ��� ������� ���������� ��������� ������
    // ... �� ������� ���������� ���� ��� � ��� :)
    if (error)  {delete p; return NULL;}  // ������ ��� �������� ���������� ������
    return p;
  } else {
    return NULL;   // ��� �� ����� EXTERNAL
  }
}


// �������� � ������� ������� ������ ��������� ������������� � arc.ini ������� ���������.
// params �������� �������� ���������� �� arc.ini. ���������� 1, ���� �������� ���������.
// ������ ��������:
//   [External compressor:ccm123,ccmx123,ccm125,ccmx125]
//   mem = 276
//   packcmd   = {compressor} c $$arcdatafile$$.tmp $$arcpackedfile$$.tmp
//   unpackcmd = {compressor} d $$arcpackedfile$$.tmp $$arcdatafile$$.tmp
//   datafile   = $$arcdatafile$$.tmp
//   packedfile = $$arcpackedfile$$.tmp
//
int AddExternalCompressor (char *params)
{
    // �������� �������� ������ ������ �� ��������� ������, �������� ��� ��������� � ���������
    char  local_method [MAX_EXTERNAL_COMPRESSOR_SECTION_LENGTH];
    strncopy (local_method, params, MAX_METHOD_STRLEN);
    char* parameters [MAX_PARAMETERS];
    split (local_method, '\n', parameters, MAX_PARAMETERS);

    // ��������, ��� ������ ������ - ��������� ������ [External compressor]
    if (last_char(parameters[0])=='\r')  last_char(parameters[0]) = '\0';
    if (! (start_with (parameters[0], "[External compressor:")
           && end_with (parameters[0], "]")))
      return 0;

    // �������� �� ��������� ������ ����� ������ ���������
    char *versions_list = strdup_msg (strchr(parameters[0],':')+1);
    last_char(versions_list) = '\0';
    char* version_name [MAX_COMPRESSION_METHODS];
    int versions_count = split (versions_list, ',', version_name, MAX_COMPRESSION_METHODS);

    // ��� ������ ������ ������ ��������� ������ EXTERNAL_METHOD
    EXTERNAL_METHOD *version  =  new EXTERNAL_METHOD[versions_count];
    for (int i=0; i<versions_count; i++) {
        // �������������� ������ EXTERNAL_METHOD ������ ��������� ������ � ����������� �� ���������
        version[i].name           = version_name[i];
        version[i].MinCompression = 100;
        version[i].can_set_mem    = FALSE;
        version[i].cmem           = 0;
        version[i].dmem           = 0;
        version[i].datafile       = "$$arcdatafile$$.tmp";
        version[i].packedfile     = "$$arcpackedfile$$.tmp";
        version[i].packcmd        = "";
        version[i].unpackcmd      = "";
    }


    // ������ �������� ��� ������� �� �������� ����������, ���������������� �������������
    // (������� ��������/����������, ���������� � ������ � ��� �����).
    for (char **param=parameters;  *++param; ) {
        // ���������� ������ ��������, ������ � �� ����� ����� �� '='
        // c ��������� ��������� � ������ ����� � ��� ���������
        char *s = *param;
        if (last_char(s)=='\r')  last_char(s) = '\0';  // �� ������ ��������� ����� � '\r\n' �������������
        if (*s=='\0' || *s==';')  continue;  // ��������� ������� ������ ������ / ������ ������������
        while (*s && isspace(*s))  s++;   // ��������� ��������� ������� � ������
        char *left = s;                   // �������� ������ ����� ����� (�����) ���������
        while (*s && !isspace(*s) && *s!='=')  s++;   // ����� ����� �����
        if (*s=='\0')  return 0;
        if (*s!='=') {                         // ��������� ������� ����� �����, ���� �����
            *s++ = '\0';
            while (*s && isspace(*s))  s++;
            if (*s!='=')  return 0;
        }
        *s++ = '\0';                           // �������� '\0' ����� �����
        while (*s && isspace(*s))  s++;        // ��������� ������� � ������ ������ ����� (��������)
        if (*s=='\0')  return 0;
        char *right = s;                       // �������� ������ ��������

        // ������ left �������� ����� ����� ������ (�� '=') ��� ��������,
        // � right - ������ ����� ��� ��������� ��������.
        // �������� ��� ������ ����������� � ������� � ��� ��������������� ����
        for (int i=0; i<versions_count; i++) {
            int error = 0;  // ������� ����, ��� ��� ������� ���������� ��������� ������
                 if (strequ (left, "mem"))         version[i].cmem = version[i].dmem = parseInt (right,&error)*mb;
            else if (strequ (left, "cmem"))        version[i].cmem        = parseInt (right,&error)*mb;
            else if (strequ (left, "dmem"))        version[i].dmem        = parseInt (right,&error)*mb;
            else if (strequ (left, "packcmd"))     version[i].packcmd     = subst (strdup_msg(right), "{compressor}", version[i].name);
            else if (strequ (left, "unpackcmd"))   version[i].unpackcmd   = subst (strdup_msg(right), "{compressor}", version[i].name);
            else if (strequ (left, "datafile"))    version[i].datafile    = subst (strdup_msg(right), "{compressor}", version[i].name);
            else if (strequ (left, "packedfile"))  version[i].packedfile  = subst (strdup_msg(right), "{compressor}", version[i].name);
            else                                   error=1;

            if (error)  return 0;
        }
    }


    // �������, �������������� ������ EXTERNAL ������ ������, ������������ ��� �������
    // ��� ������������� ����� ������� ������ � ��������� ���� ����������� ��������
    // � ���, ����� ������� ����� �������� ��� ��� ����������, ����� ����� �����
    // ���������� ������ � �.�.
    for (int i=0; i<versions_count; i++) {
        AddCompressionMethod2 (parse_EXTERNAL, &version[i]);
    }
    return 1;
}


// ������-����� ������, ������������ ��� ���������� �� ������ � ����, � ����� ����������� ���.
// ������������� ����������� ����� ������� ����� ������ �����������, �������� REP � LZMA
static int TEMPFILE_x = AddExternalCompressor ("[External compressor:tempfile]");   // �������������� ������ ������ TEMPFILE
