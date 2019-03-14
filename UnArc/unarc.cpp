// to do: ����� ������ �� ������ ("name" ��� "dir/name"),
// ������ �� ����������, ������������ ������/���������
// ���������� ".arc", listfiles/-ap/-kb/--noarcext/--
#include <stdlib.h>
#include <stdio.h>
#include <dir.h>
#include <io.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>

#include "ArcStructure.h"

#define HEADER1        "FreeArc 0.40 "

#ifdef FREEARC_SFX
#define NAME           "SFX"
#else
#define NAME           "extractor"
#endif

#define HEADER2        "  by Bulat.Ziganshin@gmail.com  2007-11-17\n"


/******************************************************************************
** ���������� � ����������� ������������� ������� *****************************
******************************************************************************/
class COMMAND
{
public:
  char cmd;             // ����������� �������
  FILENAME arcname;     // ��� ��������������� �������� ������
  FILENAME *filenames;  // ����� �������������� �������� ������ �� ������
  BOOL ok;              // ������� ����������� �������?
  BOOL noarcext;        // ����� --noarcext
  BOOL nooptions;       // ����� "--"

  bool list_cmd() {return cmd=='l' || cmd=='v';}   // True, ���� ��� ������� ��������� �������� ������

  // ������ ��������� ������
  COMMAND::COMMAND (int argc, char *argv[])
  {
    noarcext  = FALSE;
    nooptions = FALSE;
#ifdef FREEARC_SFX
#define ExpandProgname(name) name
#define basename(name) name
    arcname =  ExpandProgname (argv[0]);
    ok      =  argc==1 || argv[1][0]!='-' || strlen(argv[1])==2;  // ���� ������ �������� ���������� � '-', �� ����� ���� ������ ���� ������������� ������� ��� '-'
    if (ok)
    {
      cmd = 'x';
      if (argc>1 && argv[1][0]=='-')
        cmd = argv[1][1], argv++;
      filenames = argv+1;
      ok = in_set (cmd, "lvtex");
      if (ok) return;
    }
    printf(HEADER2
           "Usage: %s [-command] [filenames...]\n"
           "Available commands: l, v, e, x, t\n"
           "Default command: x\n", basename(arcname));
#else
    cmd     = ' ';
    arcname = NULL;
    for (ok=TRUE; ok && *++argv; )
    {
      if (argv[0][0]=='-')
      {
        if (strequ(argv[0],"--noarcext"))   noarcext =TRUE;
        else if (strequ(argv[0],"--"))      nooptions=TRUE;
        else ok=FALSE;
      }
      else if (cmd==' ')   cmd = argv[0][0], ok = ok && strlen(argv[0])==1;
      else if (!arcname)   arcname = argv[0];
      else break;
    }

    filenames = argv;
    ok = ok && strchr("lvtex",cmd) && arcname;
    if (!ok)
    {
      printf(HEADER2
             "Usage: unarc command [options] archive[.arc] [filenames...]\n"
             "Available commands: l, v, e, x, t\n"
             "Available options:  --noarcext, --\n");
    }
#endif
  }

  // TRUE, ���� i-� ���� �������� dirblock ������� �������� � ���������
  BOOL accept_file (DIRECTORY_BLOCK &dirblock, int i)
  {
    if (!*filenames)  return TRUE;            // � ��������� ������ �� ������� �� ������ ����� ����� - ������, ����� ������������ ����� ����
    for (FILENAME *f=filenames; *f; f++) {
      if (strequ (dirblock.name[i], *f))
        return TRUE;                          // �! �������!
    }
    return FALSE;                             // ������������ ����� �� �������
  }
};


/******************************************************************************
** ���������� ������� ��������� �������� ������ *******************************
******************************************************************************/
unsigned long long total_files, total_bytes;

void ListHeader (COMMAND &command)
{
  if (command.cmd=='l')
      printf ("Date/time                  Size Filename\n"
              "----------------------------------------\n");
  else
      printf ("Date/time              Attr            Size          Packed      CRC Filename\n"
              "-----------------------------------------------------------------------------\n");

  total_files=total_bytes=0;
}

void ListFooter (COMMAND &command)
{
  if (command.cmd=='l')
      printf ("----------------------------------------\n"
              "%.0lf files, %.0lf bytes", double(total_files), double(total_bytes));
  else
      printf ("-----------------------------------------------------------------------------\n"
              "%.0lf files, %.0lf bytes", double(total_files), double(total_bytes));
}

void ListFiles (DIRECTORY_BLOCK &dirblock, COMMAND &command)
{
  iterate_var (i, dirblock.total_files) {
    if (command.accept_file (dirblock, i)) { //   ���� ���� ���� ��������� ����������
      unsigned long long filesize = dirblock.size[i];
      char timestr[100];  FormatDateTime (timestr, 100, dirblock.time[i]);

      if (command.cmd=='l')
          printf (dirblock.isdir[i]? "%s       -dir-" : "%s %11.0lf", timestr, double(filesize));
      else
          printf ("%s %s %15.0lf %15.0lf %08lx", timestr, dirblock.isdir[i]? ".D.....":".......", double(filesize), double(0), dirblock.crc[i]);

#ifdef FREEARC_WIN
      // Convert filename from UTF-8 to OEM encoding before printing
      char utf8name[MY_FILENAME_MAX*4], oemname[MY_FILENAME_MAX];  WCHAR utf16name[MY_FILENAME_MAX];
      dirblock.fullname (i, utf8name);
      utf8_to_utf16 (utf8name, utf16name);
      CharToOemW (utf16name, oemname);
      printf (" %s\n", oemname);
#else
      // Print UTF-8 encoded filename
      char filename[MY_FILENAME_MAX];
      dirblock.fullname (i, filename);
      printf (" %s\n", filename);
#endif

      total_files ++;
      total_bytes += filesize;
    }
  }
}


/******************************************************************************
** ���������� ������ ���������� � ������������ ������� ************************
******************************************************************************/

// Callback-������� ������ ��� ������������ � � ���������� ���������
MYFILE *infile;          // ���� ������, �� �������� ��� ������
FILESIZE bytes_left;     // ���-�� ����, ������� �������� ��������� �� ���������� ����������� ������ ����� �����-�����
int read_func (void *buf, int size)
{
  int read_bytes = mymin (bytes_left, size);
  if (read_bytes==0) return 0;
  int len = infile->tryRead (buf, read_bytes);
  if (len>0)  bytes_left -= len;
  return len;
}


// ����������, ���������� ��������� �������� ������
COMMAND *cmd;             // ����������� �������
DIRECTORY_BLOCK *dir;     // �������, �������� ����������� ��������������� �����
int curfile;              //   ����� � �������� �������� ���������������� �����
BOOL included;            //   ������� ���� ������� � ��������� ��� �� ������ ���������� ���?
int extractUntil;         //   ����� ���������� �����, ������� ����� ������� �� ����� �����-�����
MYFILE outfile;           // ����, ����������� �� ������
char fullname[MY_FILENAME_MAX]; // ������ ��� ���������������� ������ �����
FILESIZE bytes_to_write;  // ������� ���� � ������� ����� �������� ��������
CRC crc;                  // CRC ������, ���������� � ����
enum PASS {FIRST_PASS, SECOND_PASS};  // ������/������ ������ �� �����-����� (������ - ���������� ��������� � ������ ������, ������ - ���� ���������)

// * ������������� ��������� ������������� ����������� �������� ������ � ������� �������� ������,
// * ������� ����� ������, ��� �������� ������ e/x/t, �������� ����� ���������� � �������,
// * � ��, ��� ����� ������ ����� ���� ��������� �� ���������

// ������� ��������� �������� ���� � ���������� ��������� � ��� ����������
void outfile_open (PASS pass)
{
  crc = INIT_CRC;
  bytes_to_write = dir->size[curfile];
  included = cmd->accept_file (*dir, curfile);
  dir->fullname (curfile, fullname);         //unicode
  if (pass==FIRST_PASS || dir->size[curfile]>0)   // �� ������ �������� � ���������� ���������/������ ������
    if (!(dir->isdir[curfile] && cmd->cmd=='t'))  // �� �������� � ������������ ��������� ;)
      printf (dir->isdir[curfile]?  "%s %s" STR_PATH_DELIMITER "\n"  :  "%s %s (%.0lf bytes)\n",
              included? (cmd->cmd=='t'? "Testing":"Extracting"):"Skipping", fullname, double(bytes_to_write));
  if (included && cmd->cmd!='t')
    if (dir->isdir[curfile])
      {if (cmd->cmd!='e')  mkdir(fullname);}
    else {
      FILENAME outname  =  cmd->cmd=='e'? dir->name[curfile] : fullname;
      BuildPathTo (outname);                 //unicode
      outfile.open (outname, WRITE_MODE);}   //unicode
}

// �������� ������ � �������� ����
void outfile_write (void *buf, int size)
{
  crc = UpdateCRC (buf, size, crc);
  if (included && cmd->cmd!='t' && !dir->isdir[curfile])
    outfile.write(buf,size);
}

// ������� �������� ����
void outfile_close()
{
  if (included)
  {
    CHECK ((crc^INIT_CRC) == dir->crc[curfile], ("\nERROR: file %s failed CRC check", fullname));
    if (cmd->cmd!='t' && !dir->isdir[curfile])
      outfile.close();
      //SetFileDateTime (fullname, dir->time[curfile]);  //unicode
  }
  included = FALSE;
}

// Callback-������� ������ ��� ������������
int write_func (void *buf, int size)
{
  int origsize = size;
  if (curfile > extractUntil)  return -1;         // ��� ������� ����� �����������, �� ��������� ��������� ���������� �� ���������� :(
  while (size>0) {
    int n = mymin (bytes_to_write, size);         // ���������� ������� �������� �� ����� ����� ���
    outfile_write (buf,n);                        // ������� �������� ������ � ������ - ������ ��� ������
    bytes_to_write -= n;
    if (bytes_to_write==0) {                      // ���� ���� ������� �� ����� - ������� � ����������
      outfile_close();
      if (++curfile > extractUntil)  return -1;   // ���� ��� �����, ������� �� ������ ����������� �� ����� �����, ��� ���������, �� ��������� ����������� ��������� ����������
      outfile_open(SECOND_PASS);
    }
    buf=(uint8*)buf+n; size-=n;
  }
  return origsize;     // ��������������� �������� ������ � ��������� ���������� ����������
}

// ����������� ��� �������������� ����� �� �����-����� � ������� block_num �������� dirblock
void ExtractFiles (DIRECTORY_BLOCK &dirblock, int block_num, COMMAND &command)
{
  cmd = &command;
  dir = &dirblock;
  BLOCK& data_block (dirblock.data_block [block_num]);
  extractUntil = -1;                        // � ��� ���������� ����� ������� ����� ���������� ����� � �����-�����, ������� ����� ����������
  // �������� ��� ����� � ���� �����
  for (curfile = dirblock.block_start(block_num); curfile < dirblock.block_end(block_num); curfile++) {
    if (command.accept_file (dirblock, curfile))           // ���� ���� ���� ��������� ����������
    {
      if (dir->isdir[curfile] || dir->size[curfile]==0) {  // �� ���� ��� ������� ��� ������ ���� - ������� ��� �� �����
        outfile_open (FIRST_PASS);
        outfile_close(); }
      else
        extractUntil = curfile;                            // � ����� - ��������, ��� ����� ����������� ���� ��� ������� �� ����� �����
    }
  }
  if (extractUntil >= 0) {                       // ���� � ���� ����� ������� ��� ������������� - ������, ���������! :)
    infile = &dirblock.arcfile;                  //   �������� ����
    infile->seek (data_block.pos);               //   ������ ������ �����-����� � ������
    bytes_left = data_block.compsize;            //   ������ ����������� ������ � �����-�����
    curfile = dirblock.block_start (block_num);  // ����� ������� ����� � ���� �����-�����
    outfile_open (SECOND_PASS);                  // ������� ������ �������� ����
    int result = decompress (data_block.compressor, read_func, write_func, NULL);
    CHECK (result!=FREEARC_ERRCODE_INVALID_COMPRESSOR, ("\nERROR: unsupported compression method \"%s\"", data_block.compressor));
    CHECK (result>=0, ("\nERROR: archive data corrupted (decompression fails)"));
    outfile_close();                             // ������� ��������� �������� ����
  }
}


/******************************************************************************
** �������� ��������� *********************************************************
******************************************************************************/

// ����������� � ������������ ���������� �� ���������� SFX
bool AllowProcessing (COMMAND &command)
{
#ifdef FREEARC_SFX
  // � SFX ���������� ��������� �������� ������������ ����� ������� ����������
  if (command.cmd=='e' || command.cmd=='x')
  {
    char answer[256];
    printf ("Continue extraction? ");
    gets (answer);
    strlwr (answer);
    if (! (strequ(answer,"y") || strequ(answer,"yes")))
    {
      puts ("Extraction aborted!");
      command.ok = FALSE;
      return FALSE;
    }
    puts("");
  }
#endif
  return TRUE;
}

// ������ ��������� ������ � �������� � ����������� �� ����������� �������
// ListFiles ��� ������� ����� �������� ��� ExtractFiles ��� ������� �����-�����
void ProcessArchive (COMMAND &command)
{
  ARCHIVE arcinfo (command.arcname);
  arcinfo.read_structure();                                           // ��������� ��������� ������
  printf (". %s archive: %s\n",                                       // ������� ��� ��������������� ������
    command.list_cmd()? "Listing" : command.cmd=='t' ? "Testing" : "Extracting", command.arcname);
  if (arcinfo.arcComment.size>0)                                      // ������� �������� �����������
    printf("%*.*s\n", arcinfo.arcComment.size, arcinfo.arcComment.size, &arcinfo.arcComment[0]);
  if (!AllowProcessing(command)) return;                              // ������� � ������������ ���������� �� ���������� SFX

  if (command.list_cmd())  ListHeader (command);
  iterate_array (i, arcinfo.control_blocks_descriptors) {             // �������� ��� ��������� ����� � ������...
    BLOCK& block_descriptor = arcinfo.control_blocks_descriptors[i];
    if (block_descriptor.type == DIR_BLOCK) {                         // ... � ������ �� ��� ����� ��������
      DIRECTORY_BLOCK dirblock (arcinfo, block_descriptor);           // ��������� ���� ��������
      if (command.list_cmd())                                         // ���� ��� ������� ��������� ��������
        ListFiles (dirblock, command);                                //   �� �������� �
      else
        iterate_array (i, dirblock.data_block)                        //   ����� - �������� ��� �����-����� � ��������
          ExtractFiles (dirblock, i, command);                        //     � ��� ������� �� ��� �������� ��������� ������������/����������
    }
  }
  if (command.list_cmd())  ListFooter (command);
}

int main (int argc, char *argv[])
{
  printf (HEADER1 NAME);
  COMMAND command (argc, argv);    // ���������� �������
  if (command.ok)                  // ���� ������� ��� ������ � ����� ��������� �������
    ProcessArchive (command);      //   ��������� ����������� �������
  return command.ok? EXIT_SUCCESS : EXIT_FAILURE;
}

