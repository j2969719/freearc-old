// to do: отбор файлов по именам ("name" или "dir/name"),
// запрос на перезапись, дешифрование данных/заголовка
// добавление ".arc", listfiles/-ap/-kb/--noarcext/--
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
** Информация о выполняемой деархиватором команде *****************************
******************************************************************************/
class COMMAND
{
public:
  char cmd;             // Выполняемая команда
  FILENAME arcname;     // Имя обрабатываемого командой архива
  FILENAME *filenames;  // Имена обрабатываемых командой файлов из архива
  BOOL ok;              // Команда выполняется успешно?
  BOOL noarcext;        // Опция --noarcext
  BOOL nooptions;       // Опция "--"

  bool list_cmd() {return cmd=='l' || cmd=='v';}   // True, если это команда получения листинга архива

  // Разбор командной строки
  COMMAND::COMMAND (int argc, char *argv[])
  {
    noarcext  = FALSE;
    nooptions = FALSE;
#ifdef FREEARC_SFX
#define ExpandProgname(name) name
#define basename(name) name
    arcname =  ExpandProgname (argv[0]);
    ok      =  argc==1 || argv[1][0]!='-' || strlen(argv[1])==2;  // если первый аргумент начинается с '-', то после него долюна быть однобуквенная команда или '-'
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

  // TRUE, если i-й файл каталога dirblock следует включить в обработку
  BOOL accept_file (DIRECTORY_BLOCK &dirblock, int i)
  {
    if (!*filenames)  return TRUE;            // В командной строке не указано ни одного имени файла - значит, нужно обрабатывать любой файл
    for (FILENAME *f=filenames; *f; f++) {
      if (strequ (dirblock.name[i], *f))
        return TRUE;                          // О! Совпало!
    }
    return FALSE;                             // Совпадающего имени не найдено
  }
};


/******************************************************************************
** Реализация команды получения листинга архива *******************************
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
    if (command.accept_file (dirblock, i)) { //   Если этот файл требуется обработать
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
** Реализация команд распаковки и тестирования архивов ************************
******************************************************************************/

// Callback-функция чтения для распаковщика и её переменные состояния
MYFILE *infile;          // Файл архива, из которого идёт чтение
FILESIZE bytes_left;     // Кол-во байт, которое осталось прочитать до исчерпания упакованных данных этого солид-блока
int read_func (void *buf, int size)
{
  int read_bytes = mymin (bytes_left, size);
  if (read_bytes==0) return 0;
  int len = infile->tryRead (buf, read_bytes);
  if (len>0)  bytes_left -= len;
  return len;
}


// Переменные, отражающие состояние процесса записи
COMMAND *cmd;             // Выполняемая команда
DIRECTORY_BLOCK *dir;     // Каталог, которому принадлежат распаковываемые файлы
int curfile;              //   Номер в каталоге текущего распаковываемого файла
BOOL included;            //   Текущий файл включён в обработку или мы просто пропускаем его?
int extractUntil;         //   Номер последнего файла, который нужно извлечь из этого солид-блока
MYFILE outfile;           // Файл, извлекаемый из архива
char fullname[MY_FILENAME_MAX]; // Полное имя распаковываемого сейчас файла
FILESIZE bytes_to_write;  // Сколько байт в текущем файле осталось записать
CRC crc;                  // CRC данных, записанных в файл
enum PASS {FIRST_PASS, SECOND_PASS};  // Первый/второй проход по солид-блоку (первый - распаковка каталогов и пустых файлов, второй - всех остальных)

// * Нижеследующие процедуры предоставляют абстрактные средства работы с текущим выходным файлом,
// * скрывая такие детали, как различия команд e/x/t, различие между каталогами и файлами,
// * и то, что часть файлов может быть исключена из обработки

// Открыть очередной выходной файл и напечатать сообщение о его распаковке
void outfile_open (PASS pass)
{
  crc = INIT_CRC;
  bytes_to_write = dir->size[curfile];
  included = cmd->accept_file (*dir, curfile);
  dir->fullname (curfile, fullname);         //unicode
  if (pass==FIRST_PASS || dir->size[curfile]>0)   // Не писать повторно о распаковке каталогов/пустых файлов
    if (!(dir->isdir[curfile] && cmd->cmd=='t'))  // Не сообщать о тестировании каталогов ;)
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

// Записать данные в выходной файл
void outfile_write (void *buf, int size)
{
  crc = UpdateCRC (buf, size, crc);
  if (included && cmd->cmd!='t' && !dir->isdir[curfile])
    outfile.write(buf,size);
}

// Закрыть выходной файл
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

// Callback-функция записи для распаковщика
int write_func (void *buf, int size)
{
  int origsize = size;
  if (curfile > extractUntil)  return -1;         // Нам попался тупой распаковщик, не способный завершить распаковку по требованию :(
  while (size>0) {
    int n = mymin (bytes_to_write, size);         // Записываем сколько осталось до конца файла или
    outfile_write (buf,n);                        // сколько осталось данных в буфере - смотря что меньше
    bytes_to_write -= n;
    if (bytes_to_write==0) {                      // Если файл записан до конца - перейдём к следующему
      outfile_close();
      if (++curfile > extractUntil)  return -1;   // Если все файлы, которые мы должны распаковать из этого блока, уже извлечены, то попросить распаковщик завершить распаковку
      outfile_open(SECOND_PASS);
    }
    buf=(uint8*)buf+n; size-=n;
  }
  return origsize;     // Сигнализировать успешную запись и попросить продолжить распаковку
}

// Распаковать или протестировать файлы из солид-блока с номером block_num каталога dirblock
void ExtractFiles (DIRECTORY_BLOCK &dirblock, int block_num, COMMAND &command)
{
  cmd = &command;
  dir = &dirblock;
  BLOCK& data_block (dirblock.data_block [block_num]);
  extractUntil = -1;                        // В эту переменную будет записан номер последнего файла в солид-блоке, который нужно обработать
  // Переберём все файлы в этом блоке
  for (curfile = dirblock.block_start(block_num); curfile < dirblock.block_end(block_num); curfile++) {
    if (command.accept_file (dirblock, curfile))           // Если этот файл требуется обработать
    {
      if (dir->isdir[curfile] || dir->size[curfile]==0) {  // то если это каталог или пустой файл - сделаем это на месте
        outfile_open (FIRST_PASS);
        outfile_close(); }
      else
        extractUntil = curfile;                            // а иначе - запомним, что нужно распаковать блок как минимум до этого файла
    }
  }
  if (extractUntil >= 0) {                       // Если в этом блоке нашлось что распаковывать - значит, распакуем! :)
    infile = &dirblock.arcfile;                  //   Архивный файл
    infile->seek (data_block.pos);               //   Начало данных солид-блока в архиве
    bytes_left = data_block.compsize;            //   Размер упакованных данных в солид-блоке
    curfile = dirblock.block_start (block_num);  // Номер первого файла в этом солид-блоке
    outfile_open (SECOND_PASS);                  // Откроем первый выходной файл
    int result = decompress (data_block.compressor, read_func, write_func, NULL);
    CHECK (result!=FREEARC_ERRCODE_INVALID_COMPRESSOR, ("\nERROR: unsupported compression method \"%s\"", data_block.compressor));
    CHECK (result>=0, ("\nERROR: archive data corrupted (decompression fails)"));
    outfile_close();                             // Закроем последний выходной файл
  }
}


/******************************************************************************
** Основная программа *********************************************************
******************************************************************************/

// Запрашивает у пользователя разрешение на распаковку SFX
bool AllowProcessing (COMMAND &command)
{
#ifdef FREEARC_SFX
  // В SFX необходимо запросить согласие пользователя перед началом распаковки
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

// Читает структуру архива и вызывает в зависимости от выполняемой команды
// ListFiles для каждого блока каталога или ExtractFiles для каждого солид-блока
void ProcessArchive (COMMAND &command)
{
  ARCHIVE arcinfo (command.arcname);
  arcinfo.read_structure();                                           // Прочитаем структуру архива
  printf (". %s archive: %s\n",                                       // Выведем имя обрабатываемого архива
    command.list_cmd()? "Listing" : command.cmd=='t' ? "Testing" : "Extracting", command.arcname);
  if (arcinfo.arcComment.size>0)                                      // Выведем архивный комментарий
    printf("%*.*s\n", arcinfo.arcComment.size, arcinfo.arcComment.size, &arcinfo.arcComment[0]);
  if (!AllowProcessing(command)) return;                              // Спросим у пользователя разрешение на распаковку SFX

  if (command.list_cmd())  ListHeader (command);
  iterate_array (i, arcinfo.control_blocks_descriptors) {             // Переберём все служебные блоки в архиве...
    BLOCK& block_descriptor = arcinfo.control_blocks_descriptors[i];
    if (block_descriptor.type == DIR_BLOCK) {                         // ... и отберём из них блоки каталога
      DIRECTORY_BLOCK dirblock (arcinfo, block_descriptor);           // Прочитаем блок каталога
      if (command.list_cmd())                                         // Если это команда получения листинга
        ListFiles (dirblock, command);                                //   то выполним её
      else
        iterate_array (i, dirblock.data_block)                        //   иначе - переберём все солид-блоки в каталоге
          ExtractFiles (dirblock, i, command);                        //     и для каждого из них выполним процедуру тестирования/распаковки
    }
  }
  if (command.list_cmd())  ListFooter (command);
}

int main (int argc, char *argv[])
{
  printf (HEADER1 NAME);
  COMMAND command (argc, argv);    // Распарсить команду
  if (command.ok)                  // Если парсинг был удачен и можно выполнить команду
    ProcessArchive (command);      //   Выполнить разобранную команду
  return command.ok? EXIT_SUCCESS : EXIT_FAILURE;
}

