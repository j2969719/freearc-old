/******************************************************************************
** ������� ���������� ������� *************************************************
******************************************************************************/
class PROCESS
{
public:
  COMMAND *cmd;            // ����������� �������
  BASEUI  *UI;
  ARCHIVE arcinfo;

  // ����������, ���������� ��������� �������� ������ ������� ������
  MYFILE *infile;          // ���� ������, �� �������� ��� ������
  FILESIZE bytes_left;     // ���-�� ����, ������� �������� ��������� �� ���������� ����������� ������ ����� �����-�����

  // ����������, ���������� ��������� �������� ������ ������������� ������
  DIRECTORY_BLOCK *dir;     // �������, �������� ����������� ��������������� �����
  int curfile;              //   ����� � �������� �������� ���������������� �����
  BOOL included;            //   ������� ���� ������� � ��������� ��� �� ������ ���������� ���?
  int extractUntil;         //   ����� ���������� �����, ������� ����� ������� �� ����� �����-�����
  MYFILE outfile;           // ����, ����������� �� ������
  MYFILE auxfile;           // ��������������� ���������� ��� �������� ����� ����� � ��� ���� ��� ��� ��������� �� �����
  char fullname[MY_FILENAME_MAX*4]; // ������ ��� ���������������� ������ �����
  FILESIZE bytes_to_write;  // ������� ���� � ������� ����� �������� ��������
  FILESIZE writtenBytes;    // ������� ���� ����� ���� ����������� � ������� ������
  FILESIZE archive_pos;     // ������� ������� � ������
  CRC crc;                  // CRC ������, ���������� � ����
  enum PASS {FIRST_PASS, SECOND_PASS};  // ������/������ ������ �� �����-����� (������ - ���������� ��������� � ������ ������, ������ - ���� ���������)

  // ������
  void outfile_open (PASS pass);                         // ������� ��������� �������� ���� � ���������� ��������� � ��� ����������
  void outfile_write (void *buf, int size);              // �������� ������ � �������� ����
  void outfile_close();                                  // ������� �������� ����
  int  DecompressCallback (const char *what, void *buf, int size);  // Callback-������� ������/������ ��� ������������

  // ����������� ��� �������������� ����� �� �����-����� � ������� block_num �������� dirblock
  void ExtractFiles (DIRECTORY_BLOCK *dirblock, int block_num);

  // �������� ���������� �� ������
  PROCESS (COMMAND* _cmd, BASEUI* _UI, uint64 &total_files, uint64 &origsize, uint64 &compsize);

  // ������ ��������� ������ � �������� � ����������� �� ����������� �������
  // ListFiles ��� ������� ����� �������� ��� ExtractFiles ��� ������� �����-�����
  PROCESS(COMMAND* _cmd, BASEUI* _UI);
  void OpenArchive();

  // ��������� ����������� ������
  void quit(int errcode);
} *CurrentProcess;


/*************************************************************************************************
** ������������� ��������� ������������� ����������� �������� ������ � ������� �������� ������, **
** ���������� ����� ������, ��� �������� ������ e/x/t, �������� ����� ���������� � �������,     **
** � ��, ��� ����� ������ ����� ���� ��������� �� ���������                                     **
*************************************************************************************************/

// ������� ��������� �������� ���� � ���������� ��������� � ��� ����������
void PROCESS::outfile_open (PASS pass)
{
  crc = INIT_CRC;
  bytes_to_write = dir->size[curfile];
  if (pass==SECOND_PASS && bytes_to_write==0)
    return;  // Directories and empty files were extracted in first pass
  included = cmd->accept_file (dir, curfile);
  // ��� ��������� ����� (������ ��������, ���������� � -dp)
  char *xname = cmd->cmd=='e'? dir->name[curfile]
                             : dir->fullname (curfile, fullname)  +  (strequ(cmd->arc_base_dir,"")?  0  :  strlen (cmd->arc_base_dir)+1);
  outfile.setname (xname);

  if (included && cmd->cmd!='t')
    if (dir->isdir[curfile])
      {if (cmd->cmd!='e')  BuildPathTo (outfile.filename), create_dir (outfile.filename);}
    else
      {bool outfile_exists = outfile.exists();
       if (outfile_exists)
       {
         if (cmd->no)  included = FALSE;
         else if (!cmd->yes)
         {
           char answer = UI->AskOverwrite (outfile.displayname(), dir->size[curfile], dir->time[curfile]);
           switch (answer)
           {
             case 'y': break;
             case 'n': included = FALSE;  break;
             case 'a': cmd->yes = TRUE;   break;
             case 's': cmd->no  = TRUE;   included = FALSE;  break;
             case 'q': quit(FREEARC_ERRCODE_OPERATION_TERMINATED);
           }
         }
       }
       if (included)  {if(outfile_exists)  outfile.remove_readonly_attrib();  outfile.open (WRITE_MODE);}
      }

  if (pass==FIRST_PASS || dir->size[curfile]>0)   // �� ������ �������� � ���������� ���������/������ ������
    if (!(dir->isdir[curfile] && cmd->cmd!='x'))  // �� �������� � ������������ ��������� ;)
    {
      auxfile.setname (dir->fullname (curfile, fullname));
      if (!UI->ProgressFile (dir->isdir[curfile], included? (cmd->cmd=='t'? "Testing":"Extracting"):"Skipping", auxfile.displayname(), bytes_to_write))
        quit(FREEARC_ERRCODE_OPERATION_TERMINATED);
    }
}

// �������� ������ � �������� ����
void PROCESS::outfile_write (void *buf, int size)
{
  crc = UpdateCRC (buf, size, crc);
  if (included && cmd->cmd!='t' && size)
    outfile.write(buf,size);
  if (!UI->ProgressWrite (writtenBytes += size))  quit(FREEARC_ERRCODE_OPERATION_TERMINATED);
}

// ������� �������� ����
void PROCESS::outfile_close()
{
  if (included)
  {
    CHECK ((crc^INIT_CRC) == dir->crc[curfile], (s,"ERROR: file %s failed CRC check", outfile.utf8name));
    if (cmd->cmd!='t' && !dir->isdir[curfile])
      outfile.close();
      outfile.SetFileDateTime (dir->time[curfile]);
  }
  included = FALSE;
}


/******************************************************************************
** ���������� ������ ���������� � ������������ ������� ************************
******************************************************************************/

// Callback-������� ������/������ ��� ������������
int PROCESS::DecompressCallback (const char *what, void *buf, int size)
{
  if (strequ (what, "read")) {
    int read_bytes = mymin (bytes_left, size);
    if (read_bytes==0)  return 0;
    if (!UI->ProgressRead (archive_pos))  quit(FREEARC_ERRCODE_OPERATION_TERMINATED);
    int len = infile->tryRead (buf, read_bytes);
    if (len>0)  bytes_left -= len,  archive_pos += len;
    return len;

  } else if (strequ (what, "write")) {
    int origsize = size;
    if (curfile > extractUntil)  return FREEARC_ERRCODE_NO_MORE_DATA_REQUIRED;   // ��� ������� ����� �����������, �� ��������� ��������� ���������� �� ���������� :(
    while (size>0) {
      int n = mymin (bytes_to_write, size);   // ���������� ������� �������� �� ����� ����� ���
      outfile_write (buf,n);                  // ������� �������� ������ � ������ - ������ ��� ������
      bytes_to_write -= n;
      if (bytes_to_write==0) {                // ���� ���� ������� �� ����� - ������� � ����������
        outfile_close();
        if (++curfile > extractUntil)  return FREEARC_ERRCODE_NO_MORE_DATA_REQUIRED;   // ���� ��� �����, ������� �� ������ ����������� �� ����� �����, ��� ���������, �� ��������� ����������� ��������� ����������
        outfile_open(SECOND_PASS);
      }
      buf=(uint8*)buf+n; size-=n;
    }
    return origsize;     // ��������������� �������� ������ � ��������� ���������� ����������

  } else return FREEARC_ERRCODE_NOT_IMPLEMENTED;
}

// Add "tempfile" to compressors chain if required
// ��� ���� ������������ ���������� ���������� ����������� ����� REP � ������ ��������� � SparseDecompression
//   (��������� ��������� ������ ��� ���������� �� ��������� �����) �������������� ���� ����� ������� ������, � ��������� ��������� -
//   �������� ����������� ������������ ���������� �����. � ���������, GetTotalMemoryToAlloc() ���������� ����� �������� ������ 10 ��.
//   �� �������� ������� ������ ����� ��������� �� ����� ���. � ����������� ������� �� ����� �������� ����������.
char *AddTempfile (char *compressor, COMMAND* cmd)
{
  // ���� compressor ������� ����� �� ������ ������ ������, �� "tempfile" � ���� ����� �� �������� :)
  // noLimitMem ��������� ���� ���� ��������, �������� ��������������� �� ������������
  if (!strchr (compressor,COMPRESSION_METHODS_DELIMITER)  ||  cmd->noLimitMem)
    return NULL;

  char *c = (compressor = strdup_msg(compressor));
  int  compressor_len = strlen(compressor);
  char *BUFFERING = "tempfile";
  char PLUS[] = {COMPRESSION_METHODS_DELIMITER, '\0'};

  // �������� ���������� �� ��������� ��������� � ��������� ������ ������
  CMETHOD  cm[MAX_METHODS_IN_COMPRESSOR];
  uint64 memi[MAX_METHODS_IN_COMPRESSOR];
  int   solid[MAX_METHODS_IN_COMPRESSOR];
  int N = split (compressor, COMPRESSION_METHODS_DELIMITER, cm, MAX_METHODS_IN_COMPRESSOR);
  uint64 mem = 0, block = 0;
  for (int i=0; i<N; i++)
  {
    COMPRESSION_METHOD *method = ParseCompressionMethod (cm[i]);
    if (!method)  goto abort;
    mem += memi[i] = method->GetDecompressionMem();
    solid[i] = method->doit ("SparseDecompression?", 0, NULL, NULL)<=0;  // ����� ������������� ��� "not implemented"
    if (solid[i])  block += memi[i];
    free(method);
  }

  {
    // Maximum memory allowed to use and largest contiguous memory block
    uint64 maxmem   = cmd->limitMem? cmd->limitMem  :  mymin (GetPhysicalMemory()/4*3, GetTotalMemoryToAlloc()-30*mb);
    uint64 maxblock = cmd->limitMem? cmd->limitMem  :  GetMaxMemToAlloc();

    // If memreqs are too large - add "tempfile" between methods
    if (mem > maxmem  ||  block > maxblock)
    {
      compressor = (char*) malloc_msg (compressor_len + (strlen(BUFFERING)+strlen(PLUS))*(N-1) + 1);

      strcpy(compressor, cm[0]);
      mem = memi[0];
      block = solid[0]? memi[0] : 0;

      for (int i=1; i<N; i++)
      {
        // If total memreqs of methods after last tempfile >maxmem (or total reqs of methods requiring solid memory blocks >maxblock),
        //   then add one more tempfile occurence
        if (mem>0 && (mem+memi[i]>maxmem || (solid[i] && block+memi[i]>maxblock)))
        {
          strcat (compressor, PLUS);
          strcat (compressor, BUFFERING);
          mem = block = 0;
        }
        strcat (compressor, PLUS);
        strcat (compressor, cm[i]);
        mem += memi[i];
        if (solid[i])  block += memi[i];
      }
      free(c);  // we can't free c earlier since its space used by cm[i]
      return compressor;
    }
  }

abort:
  free(c);
  return NULL;
}

int global_callback (const char *what, void *buf, int size, void *auxdata)
{
  return ((PROCESS*)auxdata) -> DecompressCallback (what, buf, size);
}

// ����������� ��� �������������� ����� �� �����-����� � ������� block_num �������� dirblock
void PROCESS::ExtractFiles (DIRECTORY_BLOCK *dirblock, int block_num)
{
  dir = dirblock;
  BLOCK& data_block (dirblock->data_block [block_num]);
  extractUntil = -1;                        // � ��� ���������� ����� ������� ����� ���������� ����� � �����-�����, ������� ����� ����������
  // �������� ��� ����� � ���� �����
  for (curfile = dirblock->block_start(block_num); curfile < dirblock->block_end(block_num); curfile++) {
    if (cmd->accept_file (dirblock, curfile))    // ���� ���� ���� ��������� ����������
    {
      if (dir->size[curfile]==0) {               //   �� ���� ��� ������� ��� ������ ���� - ������� ��� �����
        outfile_open (FIRST_PASS);
        outfile_close(); }
      else
        extractUntil = curfile;                  //   � ����� - ��������, ��� ����� ����������� ���� ��� ������� �� ����� �����
    }
  }
  if (extractUntil >= 0) {                       // ���� � ���� ����� ������� ��� ������������� - ������, ���������! :)
    infile = &dirblock->arcfile;                 //   �������� ����
    infile->seek (archive_pos = data_block.pos); //   ������ ������ �����-����� � ������
    bytes_left = data_block.compsize;            //   ������ ����������� ������ � �����-�����
    curfile = dirblock->block_start (block_num); // ����� ������� ����� � ���� �����-�����
    outfile_open (SECOND_PASS);                  // ������� ������ �������� ����
    char *compressor = AddTempfile (data_block.compressor, cmd);  // ������� "tempfile" ����� ������������� ���� �� ������� ������ ��� ����������
    int result = MultiDecompress (compressor? compressor : data_block.compressor, global_callback, this);
    CHECK (result!=FREEARC_ERRCODE_INVALID_COMPRESSOR, (s,"ERROR: unsupported compression method %s", data_block.compressor));
    CHECK (result>=0 || result==FREEARC_ERRCODE_NO_MORE_DATA_REQUIRED, (s,"ERROR: archive data corrupted (decompression fails)"));
    free (compressor);
    outfile_close();                             // ������� ��������� �������� ����
  }
}


/******************************************************************************
** �������� ���������� �� ������ **********************************************
******************************************************************************/

PROCESS::PROCESS (COMMAND* _cmd, BASEUI* _UI, uint64 &total_files, uint64 &origsize, uint64 &compsize) : cmd(_cmd), UI(_UI)
{
  CurrentProcess = this;
  SetCompressionThreads (GetProcessorsCount());
  OpenArchive();                                                      // ������� ���� ������ � ��������� ��������� ������
  total_files = origsize = compsize = 0;

  iterate_array (i, arcinfo.control_blocks_descriptors) {             // �������� ��� ��������� ����� � ������...
    BLOCK& block_descriptor = arcinfo.control_blocks_descriptors[i];
    if (block_descriptor.type == DIR_BLOCK) {                         // ... � ������ �� ��� ����� ��������
      DIRECTORY_BLOCK dirblock (arcinfo, block_descriptor);           // ��������� ���� ��������
      iterate_var (i, dirblock.total_files)                           // �������� ��� ����� � ��������
      	origsize += dirblock.size[i];
      iterate_array (i, dirblock.data_block)                          // �������� ��� �����-����� � ��������
      	compsize += dirblock.data_block[i].compsize;
      total_files += dirblock.total_files;
    }
  }
}


/******************************************************************************
** �������� ��������� ���������� ������� ��� ������� **************************
******************************************************************************/

// ������� ���� ������ � ��������� ��������� ������
// ��� SFX ������� ��������������� .arc ���� ���� SFX-������ �� �������� ������
void PROCESS::OpenArchive()
{
#ifdef FREEARC_SFX
  SET_JMP_POINT_GOTO(try_arc);
try_exe:
#endif
  arcinfo.arcfile.open (cmd->arcname, READ_MODE);         // ������� ���� ������
  arcinfo.read_structure();                               // ��������� ��������� ������
#ifdef FREEARC_SFX
  RESET_JMP_POINT();
  return;

try_arc:                                                  // ������� ���� � ��� �� ������ ��� SFX, �� ����������� .arc
  SET_JMP_POINT_GOTO(try_exe);                            // ��� ������ � .arc ����� ���������� ������� .exe ����� ������� ��������� �� ������ � .exe
  arcinfo.arcfile.setname (cmd->arcname);
  arcinfo.arcfile.change_executable_ext (FREEARC_FILE_EXTENSION);
  arcinfo.arcfile.open (READ_MODE);
  arcinfo.read_structure();
  RESET_JMP_POINT();
#endif
}


// ������ ��������� ������ � �������� � ����������� �� ����������� �������
// ListFiles ��� ������� ����� �������� ��� ExtractFiles ��� ������� �����-�����
PROCESS::PROCESS (COMMAND* _cmd, BASEUI* _UI) : cmd(_cmd), UI(_UI)
{
  CurrentProcess = this;
  cmd->Prepare();
  OpenArchive();                                                      // ������� ���� ������ � ��������� ��������� ������

  // ������� ��������� �������� �� ����� � �������� � ������������ ���������� �� ���������� SFX
  if (!UI->AllowProcessing (cmd->cmd, cmd->silent, MYFILE(cmd->arcname).displayname(), &arcinfo.arcComment[0], arcinfo.arcComment.size, cmd->outpath.utf8name)) {
    cmd->ok = FALSE;  return;
  }
  if (cmd->cmd!='t')       outfile.SetBaseDir (UI->GetOutDir());

  writtenBytes = 0;
  if (cmd->list_cmd())     UI->ListHeader (*cmd);
  else                     UI->BeginProgress (arcinfo.arcfile.size());
  iterate_array (i, arcinfo.control_blocks_descriptors) {             // �������� ��� ��������� ����� � ������...
    BLOCK& block_descriptor = arcinfo.control_blocks_descriptors[i];
    if (block_descriptor.type == DIR_BLOCK) {                         // ... � ������ �� ��� ����� ��������
      DIRECTORY_BLOCK dirblock (arcinfo, block_descriptor);           // ��������� ���� ��������
      if (cmd->list_cmd())                                            // ���� ��� ������� ��������� ��������
        UI->ListFiles (&dirblock, *cmd);                              //   �� �������� �
      else
        iterate_array (i, dirblock.data_block)                        //   ����� - �������� ��� �����-����� � ��������
          ExtractFiles (&dirblock, i);                                //     � ��� ������� �� ��� �������� ��������� ������������/����������
    }
  }
  if (cmd->list_cmd())  UI->ListFooter (*cmd);
  else                  UI->EndProgress (cmd);
}


// ��������� ����������� ������
void PROCESS::quit(int errcode)
{
  cmd->ok = FALSE;
  if (outfile.isopen())  outfile.close(), outfile.remove();
  arcinfo.arcfile.tryClose();
  compressionLib_cleanup();
  UI->Abort (cmd, errcode);
}


