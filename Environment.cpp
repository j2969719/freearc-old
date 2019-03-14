#include <stdio.h>
#include <sys/stat.h>
#include <utime.h>
#include <limits.h>
#include <memory.h>
#include "Environment.h"
#include "Compression/Compression.h"

// Изменим настройки RTS, включив compacting GC начиная с 20 mb: "-c1 -M2000m" (отключено до исправления ошибки в compacting GC)
char *ghc_rts_opts = "-c1 -M2000m";

#ifdef FREEARC_WIN

#include <windows.h>
#include <wininet.h>
#include <stdio.h>
#include <conio.h>
#include <time.h>

/*
void SetDateTimeAttr(const char* Filename, time_t t)
{
    struct tm* t2 = gmtime(&t);

    SYSTEMTIME t3;
    t3.wYear         = t2->tm_year+1900;
    t3.wMonth        = t2->tm_mon+1;
    t3.wDay          = t2->tm_mday;
    t3.wHour         = t2->tm_hour;
    t3.wMinute       = t2->tm_min;
    t3.wSecond       = t2->tm_sec;
    t3.wMilliseconds = 0;

    FILETIME ft;
    SystemTimeToFileTime(&t3, &ft);

    HANDLE hndl=CreateFile(Filename,GENERIC_WRITE,0,NULL,OPEN_EXISTING,0,0);
    SetFileTime(hndl,NULL,NULL,&ft);  //creation, last access, modification times
    CloseHandle(hndl);
    //SetFileAttributes(Filename,ai.attrib);
}
*/


#define LARGE_STRING 1024

char Saved_Title[LARGE_STRING];
bool Saved = FALSE;

void EnvSetConsoleTitle (char *title)
{
  if (!Saved) {
    GetConsoleTitle (Saved_Title, sizeof(Saved_Title));
    Saved = TRUE;
  }
  SetConsoleTitle (title);
}

void EnvResetConsoleTitle (void)
{
  if (Saved) {
    SetConsoleTitle (Saved_Title);
    Saved = FALSE;
  }
}

CFILENAME GetExeName (void)
{
  static char buf[LARGE_STRING];
  GetModuleFileNameW (NULL, (CFILENAME)buf, LARGE_STRING);
  return (CFILENAME)buf;
}

unsigned GetPhysicalMemory (void)
{
  MEMORYSTATUS buf;
    GlobalMemoryStatus (&buf);
  return buf.dwTotalPhys;
}

unsigned GetAvailablePhysicalMemory (void)
{
  MEMORYSTATUS buf;
    GlobalMemoryStatus (&buf);
  return buf.dwAvailPhys;
}

int GetProcessorsCount (void)
{
  SYSTEM_INFO si;
    GetSystemInfo (&si);
  return si.dwNumberOfProcessors;
}

void SetFileDateTime(const CFILENAME Filename, time_t mtime)
{
  struct _stat st;
    _wstat (Filename, &st);
  struct _utimbuf times;
    times.actime  = st.st_atime;
    times.modtime = mtime;
  _wutime (Filename, &times);
}


#else // For Unix:


#include <unistd.h>
#include <sys/sysinfo.h>

void EnvSetConsoleTitle    (char *)      {}
void EnvResetConsoleTitle  (void)        {}

unsigned GetPhysicalMemory (void)
{
  struct sysinfo si;
    sysinfo(&si);
  return si.totalram*si.mem_unit;
}

unsigned GetAvailablePhysicalMemory (void)
{
  struct sysinfo si;
    sysinfo(&si);
  return (si.freeram+si.bufferram)*si.mem_unit;
}

int GetProcessorsCount (void)
{
  return get_nprocs();
}

void SetFileDateTime(const CFILENAME Filename, time_t mtime)
{
#undef stat
  struct stat st;
    stat (Filename, &st);
  struct utimbuf times;
    times.actime  = st.st_atime;
    times.modtime = mtime;
  utime (Filename, &times);
}

#endif // Windows/Unix


void FormatDateTime (char *buf, int bufsize, time_t t)
{
  struct tm *p;
  if (t==-1)  t=0;  // Иначе получим вылет :(
  p = localtime(&t);
  strftime( buf, bufsize, "%Y-%m-%d %H:%M:%S", p);
}


/************************************************************************
 ************* CRC-32 subroutines ***************************************
 ************************************************************************/

uint CRCTab[256];

void InitCRC()
{
  for (int I=0;I<256;I++)
  {
    uint C=I;
    for (int J=0;J<8;J++)
      C=(C & 1) ? (C>>1)^0xEDB88320L : (C>>1);
    CRCTab[I]=C;
  }
}

uint UpdateCRC( void *Addr, uint Size, uint StartCRC)
{
  if (CRCTab[1]==0)
    InitCRC();
  uint8 *Data=(uint8 *)Addr;
#if defined(FREEARC_INTEL_BYTE_ORDER) && defined(PRESENT_UINT32)
  while (Size>=8)
  {
    StartCRC^=*(uint32 *)Data;
    StartCRC=CRCTab[(uint8)StartCRC]^(StartCRC>>8);
    StartCRC=CRCTab[(uint8)StartCRC]^(StartCRC>>8);
    StartCRC=CRCTab[(uint8)StartCRC]^(StartCRC>>8);
    StartCRC=CRCTab[(uint8)StartCRC]^(StartCRC>>8);
    StartCRC^=*(uint32 *)(Data+4);
    StartCRC=CRCTab[(uint8)StartCRC]^(StartCRC>>8);
    StartCRC=CRCTab[(uint8)StartCRC]^(StartCRC>>8);
    StartCRC=CRCTab[(uint8)StartCRC]^(StartCRC>>8);
    StartCRC=CRCTab[(uint8)StartCRC]^(StartCRC>>8);
    Data+=8;
    Size-=8;
  }
#endif
  for (int I=0;I<Size;I++)
    StartCRC=CRCTab[(uint8)(StartCRC^Data[I])]^(StartCRC>>8);
  return(StartCRC);
}

// Вычислить CRC блока данных
uint CalcCRC( void *Addr, uint Size)
{
  return UpdateCRC (Addr, Size, INIT_CRC) ^ INIT_CRC;
}



// От-xor-ить два блока данных
void memxor (char *dest, char *src, uint size)
{
  if (size) do
      *dest++ ^= *src++;
  while (--size);
}

// Вернуть имя файла без имени каталога
FILENAME basename (FILENAME fullname)
{
  char *basename = fullname;
  for (char* p=fullname; *p; p++)
    if (in_set (*p, ALL_PATH_DELIMITERS))
      basename = p+1;
  return basename;
}

// Создать каталоги на пути к name
void BuildPathTo (FILENAME name)
{
  char *path_ptr = NULL;
  for (char *p = str_end(name); --p >= name;)
    if (in_set (*p, DIRECTORY_DELIMITERS))
      {path_ptr=p; break;}
  if (path_ptr==NULL)  return;

  char oldc = *path_ptr;
  *path_ptr = '\0';

  if (! file_exists (name))
  {
    BuildPathTo (name);
    create_dir  (name);
  }
  *path_ptr = oldc;
}


/* ***************************************************************************
*																			*
* Random system values collection routine from CryptLib by Peter Gutmann
* [ftp://ftp.franken.de/pub/crypt/cryptlib/cl331.zip]							
*																			*
****************************************************************************/

/* The size of the intermediate buffer used to accumulate polled data */
#define RANDOM_BUFSIZE	4096

// Handling random data buffer
#define initRandomData(rand_buf, rand_size)  \
                                 char *rand_ptr=(rand_buf), *rand_end=(rand_buf)+(rand_size)
#define addRandomData(ptr,size)  (memcpy (rand_ptr, (ptr), mymin((size),rand_end-rand_ptr)), rand_ptr+=mymin((size),rand_end-rand_ptr))
#define addRandomLong(value)     {long n=(value); addRandomData(&n, sizeof(long));}
#define addRandomValue(value)    addRandomLong((long) value)


/* Map a value that may be 32 or 64 bits depending on the platform to a long */
#if defined( _MSC_VER ) && ( _MSC_VER >= 1400 )
  #define addRandomHandle( handle ) \
		  addRandomLong( PtrToUlong( handle ) )
#else
  #define addRandomHandle	addRandomValue
#endif /* 32- vs. 64-bit VC++ */


// This routine fills buffer with system-generated pseudo-random data
// and returns number of bytes filled
int systemRandomData (char *rand_buf, int rand_size)
{
#ifdef FREEARC_WIN

	FILETIME  creationTime, exitTime, kernelTime, userTime;
	DWORD minimumWorkingSetSize, maximumWorkingSetSize;
	LARGE_INTEGER performanceCount;
	MEMORYSTATUS memoryStatus;
	HANDLE handle;
	POINT point;

	initRandomData (rand_buf, rand_size);

	/* Get various basic pieces of system information: Handle of active
	   window, handle of window with mouse capture, handle of clipboard owner
	   handle of start of clpboard viewer list, pseudohandle of current
	   process, current process ID, pseudohandle of current thread, current
	   thread ID, handle of desktop window, handle  of window with keyboard
	   focus, whether system queue has any events, cursor position for last
	   message, 1 ms time for last message, handle of window with clipboard
	   open, handle of process heap, handle of procs window station, types of
	   events in input queue, and milliseconds since Windows was started.
	   Since a HWND/HANDLE can be a 64-bit value on a 64-bit platform, we
	   have to use a mapping macro that discards the high 32 bits (which
	   presumably won't be of much interest anyway) */
	addRandomHandle( GetActiveWindow() );
	addRandomHandle( GetCapture() );
	addRandomHandle( GetClipboardOwner() );
	addRandomHandle( GetClipboardViewer() );
	addRandomHandle( GetCurrentProcess() );
	addRandomValue( GetCurrentProcessId() );
	addRandomHandle( GetCurrentThread() );
	addRandomValue( GetCurrentThreadId() );
	addRandomHandle( GetDesktopWindow() );
	addRandomHandle( GetFocus() );
	addRandomValue( GetInputState() );
	addRandomValue( GetMessagePos() );
	addRandomValue( GetMessageTime() );
	addRandomHandle( GetOpenClipboardWindow() );
	addRandomHandle( GetProcessHeap() );
	addRandomHandle( GetProcessWindowStation() );
	addRandomValue( GetTickCount() );

	/* Get multiword system information: Current caret position, current
	   mouse cursor position */
	GetCaretPos( &point );
	addRandomData( &point, sizeof( POINT ) );
	GetCursorPos( &point );
	addRandomData( &point, sizeof( POINT ) );

	/* Get percent of memory in use, bytes of physical memory, bytes of free
	   physical memory, bytes in paging file, free bytes in paging file, user
	   bytes of address space, and free user bytes */
	memoryStatus.dwLength = sizeof( MEMORYSTATUS );
	GlobalMemoryStatus( &memoryStatus );
	addRandomData( &memoryStatus, sizeof( MEMORYSTATUS ) );

	/* Get thread and process creation time, exit time, time in kernel mode,
	   and time in user mode in 100ns intervals */
	handle = GetCurrentThread();
	GetThreadTimes( handle, &creationTime, &exitTime, &kernelTime, &userTime );
	addRandomData( &creationTime, sizeof( FILETIME ) );
	addRandomData( &exitTime, sizeof( FILETIME ) );
	addRandomData( &kernelTime, sizeof( FILETIME ) );
	addRandomData( &userTime, sizeof( FILETIME ) );
	handle = GetCurrentProcess();
	GetProcessTimes( handle, &creationTime, &exitTime, &kernelTime, &userTime );
	addRandomData( &creationTime, sizeof( FILETIME ) );
	addRandomData( &exitTime, sizeof( FILETIME ) );
	addRandomData( &kernelTime, sizeof( FILETIME ) );
	addRandomData( &userTime, sizeof( FILETIME ) );

	/* Get the minimum and maximum working set size for the current process */
	GetProcessWorkingSetSize( handle, &minimumWorkingSetSize, &maximumWorkingSetSize );
	addRandomValue( minimumWorkingSetSize );
	addRandomValue( maximumWorkingSetSize );

	/* The following are fixed for the lifetime of the process */
       	/* Get name of desktop, console window title, new window position and
       	   size, window flags, and handles for stdin, stdout, and stderr */
       	STARTUPINFO startupInfo;
       	startupInfo.cb = sizeof( STARTUPINFO );
       	GetStartupInfo( &startupInfo );
       	addRandomData( &startupInfo, sizeof( STARTUPINFO ) );

	/* The performance of QPC varies depending on the architecture it's
	   running on and on the OS, the MS documentation is vague about the
	   details because it varies so much.  Under Win9x/ME it reads the
	   1.193180 MHz PIC timer.  Under NT/Win2K/XP it may or may not read the
	   64-bit TSC depending on the HAL and assorted other circumstances,
	   generally on machines with a uniprocessor HAL
	   KeQueryPerformanceCounter() uses a 3.579545MHz timer and on machines
	   with a multiprocessor or APIC HAL it uses the TSC (the exact time
	   source is controlled by the HalpUse8254 flag in the kernel).  That
	   choice of time sources is somewhat peculiar because on a
	   multiprocessor machine it's theoretically possible to get completely
	   different TSC readings depending on which CPU you're currently
	   running on, while for uniprocessor machines it's not a problem.
	   However, the kernel appears to synchronise the TSCs across CPUs at
	   boot time (it resets the TSC as part of its system init), so this
	   shouldn't really be a problem.  Under WinCE it's completely platform-
	   dependant, if there's no hardware performance counter available, it
	   uses the 1ms system timer.

	   Another feature of the TSC (although it doesn't really affect us here)
	   is that mobile CPUs will turn off the TSC when they idle, Pentiums
	   will change the rate of the counter when they clock-throttle (to
	   match the current CPU speed), and hyperthreading Pentiums will turn
	   it off when both threads are idle (this more or less makes sense,
	   since the CPU will be in the halted state and not executing any
	   instructions to count).

	   To make things unambiguous, we detect a CPU new enough to call RDTSC
	   directly by checking for CPUID capabilities, and fall back to QPC if
	   this isn't present */
       	if( QueryPerformanceCounter( &performanceCount ) )
       		addRandomData( &performanceCount,
       					   sizeof( LARGE_INTEGER ) );
       	else
       		/* Millisecond accuracy at best... */
       		addRandomValue( GetTickCount() );

        return rand_ptr-rand_buf;

#else // For Unix:

	FILE *f = fopen ("/dev/urandom", "rb");

	if (f == NULL)
	{
		perror ("Cannot open /dev/urandom");
		return 0;
	}

	if (read (f, rand_buf, rand_size) != rand_size)
	{
		perror ("Read from /dev/urandom failed");
		fclose (f);
		return 0;
	}

	fclose (f);
	return rand_size;

#endif // Windows/Unix

}

/****************************************************************************
*
*                                           Random system values collection *
*
****************************************************************************/

