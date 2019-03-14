// (c) Bulat Ziganshin <Bulat.Ziganshin@gmail.com>
// 4x4: multithreaded compression with overlapped I/O

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "C_4x4.h"
#include "../Compression.h"
#include "../MultiThreading.h"
#define Lock Wait

#define _4x4_VERSION 0  /* version of compressed data format */


struct DeCompressionMachine;
enum ENUM_STATE {STATE_EMPTY, STATE_IO, STATE_QUEUED, STATE_COMPRESSION, STATE_DONE};

// Compression job
struct Job
{
    DeCompressionMachine *parent;             // Compression operation owning the job
    Event  Finished;                          // Signals that job was finished
    Event  Compressed;                        // Signals that (de)compression part of the job was finished
    Event  Saved;                             // Signals that writing part of the job was finished
    int    insize;                            // Amount of data in inbuf
    int    outsize;                           // Amount of data in outbuf
    int    reported_insize;                   // Number of in/out bytes processed already reported to host
    int    reported_outsize;                  // -.-
    void  *outbuf;                            // Buffer containing output (de)compressed data
    enum ENUM_STATE state;
    volatile int Errcode;
};

// (De)compression machine
struct DeCompressionMachine
{
    bool    ENCODE;                           // True for compression, False - decompression
    char    Method[MAX_METHOD_STRLEN];        // Compression method used for every block
    MemSize BlockSize;                        // Size of chunks input split to
    int     NumCompressionThreads;            // Max. number of threads doing compression
    int     NumIOThreads;                     // Max. number of threads doing I/O
    SyncQueue<Job*> WriterJobs;               // Queue of jobs for Writer thread, ordered by input chunks
    Semaphore AllowCompression;               // Limits number of threads doing compression
    Semaphore IO;                             // Prevents simultaneous execution of I/O operations from different threads
    CALLBACK_FUNC *callback; void *auxdata;   // Interface to read/write callbacks
    volatile int Errcode;

    int  Callback (const char *what, void *data, int size, Job *job);
    void DeCompressionThread (Job *job);      // Read and (de)compress data by chunks from input stream
    void WriterThread (int NumThreads);       // Send compressed data to output stream
    int  Run (bool _ENCODE, _4x4_METHOD *method, CALLBACK_FUNC *_callback, void *_auxdata);  // Create (De)compression Machine and perform (de)compression

    void SetErrcode (int errcode)             // Set global error code, if not yet set
    {
        IO.Lock();
        if (Errcode >= 0)  Errcode = errcode;
        IO.Release();
    }
};


// *************************************************************************************************
// *** (De)compression machine *********************************************************************
// *************************************************************************************************

int _4x4_callback (const char *what, void *data, int size, void *param)
{
    Job *job = (Job*) param;
    return job->parent->Callback (what, data, size, job);
}

// Perform read/write operation inside compression thread
int DeCompressionMachine::Callback (const char *what, void *data, int size, Job *job)
{
    int errcode = FREEARC_OK;

    if (strequ(what,"read") && ENCODE)
    {
        if (job->state > STATE_IO)  return FREEARC_ERRCODE_GENERAL;
        IO.Lock();                     // Lock guarantees that jobs will be queued to WriterJobs in the same order as blocks are read from input stream
        job->state = STATE_IO;
        if (Errcode <= 0) {
            job->Errcode = Errcode;
            job->insize  = Errcode;
        } else {
            READ_LEN(job->insize, data, mymin(size,BlockSize));
            if (job->insize == 0)   {job->Errcode = Errcode = 0;}  // EOF
        }
        WriterJobs.Put(job);           // Put job to the Writer ordering queue
        IO.Release();
        job->state = STATE_QUEUED;
        if (job->insize > 0)
            AllowCompression.Lock(),   // Wait for compression slot
            job->state = STATE_COMPRESSION;
        return job->insize;
    }
    else if (strequ(what,"read") && !ENCODE)
    {
        if (job->insize==0)  return 0;
        int len;
        READ_LEN_OR_EOF(len, data, mymin(size,job->insize));
        job->insize -= len;
        if (job->insize == 0)
        {
            WriterJobs.Put(job);       // Put job to the Writer ordering queue
            IO.Release();
            AllowCompression.Lock();   // Wait for decompression slot
            job->state = STATE_COMPRESSION;
        }
        return len;
    }
    else if (strequ(what,"write"))
    {
        switch (job->state)
        {
        case STATE_EMPTY:       WriterJobs.Put(job);  break;
    	case STATE_IO:          WriterJobs.Put(job);  IO.Release(); break;
    	case STATE_QUEUED:      break;
        case STATE_COMPRESSION: AllowCompression.Release(); break;         // Release (de)compression slot
        }
        job->state = STATE_DONE;

        job->outbuf  = data;
        job->outsize = size;
        job->Compressed.Signal();         // Send signal to Writer thread

        job->Saved.Wait();                // Pause execution until (de)compressed data will be written
        return size;
    }
    else if (strequ(what,"progress"))
    {
        job->reported_insize  += ((int64*)data)[0];
        job->reported_outsize += ((int64*)data)[1];
        errcode = callback ("progress", data, size, auxdata);
        if (errcode==FREEARC_OK)  return errcode;
        else                      goto finished;
    }
    else return FREEARC_ERRCODE_NOT_IMPLEMENTED;

finished:
    job->Errcode = Errcode = (errcode==FREEARC_OK? FREEARC_ERRCODE_READ : errcode);
    return errcode;
}


// Read and (de)compress data by chunks from input stream
void DeCompressionMachine::DeCompressionThread (Job *job)
{
    BeginCompressionThreadPriority();  // понизить приоритет треда (рас)паковки чтоб не завешивать машину
    job->Errcode = 1;
    int errcode = FREEARC_OK;
    while (job->Errcode > 0)
    {
        job->state = STATE_EMPTY;
        job->reported_insize  = 0;
        job->reported_outsize = 0;
        if (ENCODE)
        {
#ifndef FREEARC_DECOMPRESS_ONLY
            errcode = Compress (Method, _4x4_callback, job);
#endif
        }
        else
        {
            IO.Lock();
            job->state = STATE_IO;
            if (Errcode <= 0)  {job->Errcode = Errcode; break;}
            READ4_OR_EOF(job->outsize);
            READ4(job->insize);
            if (int32(job->outsize) == -1) {  // stored block
                int len;
                job->outsize = job->insize;
                job->outbuf = malloc_msg(job->outsize);
                READ_LEN_OR_EOF(len, job->outbuf, job->outsize);
                WriterJobs.Put(job);
                IO.Release();
                job->state = STATE_DONE;
                job->Compressed.Signal();
                job->Saved.Wait();
                free(job->outbuf);
            } else {
                ////char method[MAX_METHOD_STRLEN];
                ////fgets0(method, sizeof(method), infile);
                ////job->insize -= strlen(method)+1;
                errcode = Decompress (Method, _4x4_callback, job);
            }
        }
        if (errcode==0)  continue;

finished:
        if (job->Errcode > 0)
            job->state==STATE_DONE && (job->state = STATE_EMPTY),
            job->Errcode = errcode;
    }

    switch (job->state)
    {
    case STATE_EMPTY:       WriterJobs.Put(job);  break;
    case STATE_IO:          WriterJobs.Put(job);  IO.Release();  break;
    case STATE_QUEUED:      break;
    case STATE_COMPRESSION: AllowCompression.Release(); break;
    case STATE_DONE:        break;
    }
    SetErrcode (job->Errcode);
    job->Compressed.Signal();         // Send signal to Writer thread
    job->Finished.Signal();
}


// Global function for (de)compression thread
static THREAD_FUNC_RET_TYPE THREAD_FUNC_CALL_TYPE GlobalDeCompressionThread (void *paramPtr)
{
    Job *job = (Job*) paramPtr;
    job->parent->DeCompressionThread(job);
    return 0;
}


// Send compressed data to output stream
void DeCompressionMachine::WriterThread (int NumThreads)
{
    for(;;)
    {
        // Acquire next writer job
        Job *job = WriterJobs.Get();

        // Wait until (de)compression will be finished
        job->Compressed.Wait();

        // Break on EOF/error
        if (job->Errcode <= 0)
        {
            job->Saved.Signal();
            job->Finished.Wait();
            NumThreads--;
            if (NumThreads <= 0)
                break;
        } else {

            int errcode = FREEARC_OK;
            //IO.Lock();               // Prevent simultaneous read and write
            if (ENCODE)
            {
                // Save compressed block header: original/compressed blocksize and compression method
                WRITE4(job->insize);
                WRITE4(job->outsize);
                ////WRITE4(job->outsize + strlen(method)+1);
                ////mywrite(method, strlen(method)+1);
            }
            // Save (de)compressed data
            WRITE(job->outbuf, job->outsize);
            // Update progress indicators
            PROGRESS((ENCODE? 0:2*4) + job->insize - job->reported_insize,  (ENCODE? 2*4:0) + job->outsize - job->reported_outsize);

finished:   if (errcode)  SetErrcode (errcode);
            //IO.Release();            // Prevent simultaneous read and write

            // Signal that we've saved (de)compressed data
            job->Saved.Signal();
        }
    }
}

int DeCompressionMachine::Run (bool _ENCODE, _4x4_METHOD *method, CALLBACK_FUNC *_callback, void *_auxdata)
{
    // Copy compression parameters into class fields
    ENCODE = _ENCODE;  strcpy(Method, method->Method);  callback = _callback;  auxdata = _auxdata;
#ifndef FREEARC_DECOMPRESS_ONLY
    BlockSize              =  method->BlockSize?              method->BlockSize              :  ::GetDictionary(Method);
#endif  // !defined (FREEARC_DECOMPRESS_ONLY)
    NumCompressionThreads  =  method->NumCompressionThreads?  method->NumCompressionThreads  :  GetCompressionThreads();
    NumIOThreads           =  method->NumIOThreads>=0?        method->NumIOThreads           :  mymax(NumCompressionThreads,2);

    // Save/check version of data format used
    Errcode = 1;
    int errcode = FREEARC_OK;
    if (ENCODE)  {WRITE4(_4x4_VERSION);}
    else         {int version; READ4_OR_EOF(version); if (version != _4x4_VERSION)  return FREEARC_ERRCODE_BAD_COMPRESSED_DATA;}

   {int NumThreads = NumCompressionThreads + NumIOThreads;                   // Number of threads performing compression and I/O
    AllowCompression.Create(NumCompressionThreads, NumCompressionThreads);   // Semaphore limiting number of threads simultaneously performing (de)compression
    IO.Create(1,1);                                                          // Semaphore limiting number of threads doing I/O
    WriterJobs.SetSize(NumThreads);                                          // Queue of jobs for Writer thread

    Set_compress_all_at_once(1);

    // Create threads for I/O and compression
    Job* job = new Job[NumThreads];
    for (int i=0; i < NumThreads; i++)
    {
        job[i].parent = this;
        Thread t; t.Create (GlobalDeCompressionThread, &job[i]);
    }

    // Perform (de)compression and write (de)compressed data to outstream
    WriterThread(NumThreads);
    PROGRESS(ENCODE?0:4,ENCODE?4:0);  // Count 4 header bytes
    Set_compress_all_at_once(0);

    // Clean up
    delete [] job;  AllowCompression.Close();  IO.Close();  WriterJobs.Close();}
finished: return Errcode<=0? Errcode : errcode;
}


/*-------------------------------------------------*/
/* Реализация класса _4x4_METHOD                   */
/*-------------------------------------------------*/
// Конструктор, присваивающий параметрам метода сжатия значения по умолчанию
_4x4_METHOD::_4x4_METHOD()
{
  strcpy(Method, "tor:3:1mb");
  NumCompressionThreads = 0;
  NumIOThreads          = -1;
  BlockSize             = 0;
}

// Total amount of threads
int _4x4_METHOD::TotalThreads()
{
  int ct = GetNumCompressionThreads();
  return ct + (NumIOThreads>=0? NumIOThreads : mymax(ct,2));
}

// Number of compression of threads
int _4x4_METHOD::GetNumCompressionThreads()
{
  return NumCompressionThreads? NumCompressionThreads : GetCompressionThreads();
}

// Универсальный метод, отвечает на запрос "has_progress?"
int _4x4_METHOD::doit (char *what, int param, void *data, CALLBACK_FUNC *callback)
{
    if (strequ (what, "has_progress?"))  return 1;               // Да, этот алгоритм поддерживает отчёт о прогрессе упаковки
    else                                 return COMPRESSION_METHOD::doit (what, param, data, callback);  // Передать остальные вызовы родительской процедуре
}

// Функция распаковки
int _4x4_METHOD::decompress (CALLBACK_FUNC *callback, void *auxdata)
{
  DeCompressionMachine m;
  return m.Run (FALSE, this, callback, auxdata);
}

#ifndef FREEARC_DECOMPRESS_ONLY

// Функция упаковки
int _4x4_METHOD::compress (CALLBACK_FUNC *callback, void *auxdata)
{
  DeCompressionMachine m;
  return m.Run (TRUE, this, callback, auxdata);
}

// Записать в buf[MAX_METHOD_STRLEN] строку, описывающую метод сжатия и его параметры (функция, обратная к parse_X)
void _4x4_METHOD::ShowCompressionMethod (char *buf, bool purify)
{
  char ThreadsStr[100], IOThreadsStr[100], BlockSizeStr[100];
  sprintf (ThreadsStr,   ":t%d", NumCompressionThreads);
  sprintf (IOThreadsStr, ":i%d", NumIOThreads);
  showMem (BlockSize, BlockSizeStr);
  sprintf (buf, "4x4%s%s%s%s:%s", !purify && NumCompressionThreads? ThreadsStr  :"",
                                  !purify && NumIOThreads>=0      ? IOThreadsStr:"",
                                  BlockSize? ":b":"",  BlockSize? BlockSizeStr:"",
                                  Method);
}

// Set amount of memory used for compression
void _4x4_METHOD::SetCompressionMem (MemSize mem)
{
  if (mem > 0)
  {
    Set_compress_all_at_once(1);

    // Посчитаем количество тредов, которое мы сможем себе позволить
    int threads = mem / ::GetCompressionMem(Method);
    if (threads >= TotalThreads()) {
      // Памяти достаточно - "расширим" алгоритм сжатия
      //::SetCompressionMem (Method, mem/TotalThreads(), Method);

    } else if (threads >= GetNumCompressionThreads()) {
      // Памяти достаточно для всех тредов сжатия, уменьшаем число тредов I/O
      NumIOThreads          = threads - GetNumCompressionThreads();

    } else if (threads >= 1) {
      // Памяти достаточно хотя бы для одного треда сжатия - уменьшаем число тредов сжатия и отключаем треды I/O
      NumCompressionThreads = threads;
      NumIOThreads          = 0;

    } else {
      // Памяти не хватило даже для одного треда сжатия - оставим только один и ещё подожмём его
      NumCompressionThreads = 1;
      NumIOThreads          = 0;
      ::SetCompressionMem (Method, mem, Method);
    }
    Set_compress_all_at_once(0);
  }
}

// Set amount of memory used for decompression
void _4x4_METHOD::SetDecompressionMem (MemSize mem)
{
  if (mem > 0)
  {
    Set_compress_all_at_once(1);

    // Посчитаем количество тредов, которое мы сможем себе позволить
    int threads = mem / ::GetDecompressionMem(Method);
    if (threads >= TotalThreads()) {
      // Памяти достаточно - "расширим" алгоритм сжатия
      //::SetDecompressionMem (Method, mem/TotalThreads(), Method);

    } else if (threads >= GetNumCompressionThreads()) {
      // Памяти достаточно для всех тредов сжатия, уменьшаем число тредов I/O
      NumIOThreads          = threads - GetNumCompressionThreads();

    } else if (threads >= 1) {
      // Памяти достаточно хотя бы для одного треда сжатия - уменьшаем число тредов сжатия и отключаем треды I/O
      NumCompressionThreads = threads;
      NumIOThreads          = 0;

    } else {
      // Памяти не хватило даже для одного треда сжатия - оставим только один и ещё подожмём его
      NumCompressionThreads = 1;
      NumIOThreads          = 0;
      ::SetDecompressionMem (Method, mem, Method);
    }
    Set_compress_all_at_once(0);
  }
}

#endif  // !defined (FREEARC_DECOMPRESS_ONLY)

// Конструирует объект типа _4x4_METHOD с заданными параметрами упаковки
// или возвращает NULL, если это другой метод сжатия или допущена ошибка при задании параметров
COMPRESSION_METHOD* parse_4x4 (char** parameters)
{
  if (strcmp (parameters[0], "4x4") == 0) {
    // Если название метода (нулевой параметр) - "4x4", то разберём остальные параметры

    _4x4_METHOD *p = new _4x4_METHOD;
    int error = 0;  // Признак того, что при разборе параметров произошла ошибка

    while (!error && *++parameters)  // Переберём все параметры метода
    {
      char *param = *parameters;

      // Если параметр начинается не с цифры или буквы плюс цифры - это начало описания метода сжатия
      if (!(isdigit(param[0]) || isdigit(param[1])))  {join (parameters, ':', p->Method, sizeof(p->Method)); break;}

      if (strlen(param)==1) switch (*param) {   // Однобуквенные параметры
      }
      else switch (*param) {                    // Параметры, содержащие значения
        case 'b':  p->BlockSize              =  parseMem (param+1, &error); continue;
        case 't':  p->NumCompressionThreads  =  parseInt (param+1, &error); continue;
        case 'i':  p->NumIOThreads           =  parseInt (param+1, &error); continue;
      }

      // Сюда мы попадаем, если в параметре не указано его название
      // Если этот параметр удастся разобрать как целое число (т.е. в нём - только цифры),
      // то присвоим его значение полю Threads, иначе попробуем разобрать его как BlockSize
      int n = parseInt (param, &error);
      if (!error) p->NumCompressionThreads = n;
      else        error=0, p->BlockSize = parseMem (param, &error);
    }
    if (error)  {delete p; return NULL;}  // Ошибка при парсинге параметров метода
    return p;
  } else
    return NULL;   // Это не метод 4x4
}

static int _4x4_x = AddCompressionMethod (parse_4x4);   // Зарегистрируем парсер метода 4x4

