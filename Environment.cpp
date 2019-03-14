#define _WIN32_WINNT 0x0501
#include <stdio.h>
#include <sys/stat.h>
#include <utime.h>
#include <limits.h>
#include <memory.h>
#include "Environment.h"
#include "Compression/Compression.h"

// Изменим настройки RTS, включив compacting GC начиная с 40 mb:
char *ghc_rts_opts = "-c1 -M4000m -K80m                       ";


/* ********************************************************************************************************
*  Find largest contiguous memory block available and dump information about all available memory blocks
***********************************************************************************************************/

void memstat(void);

struct LargestMemoryBlock
{
  void   *p;
  size_t size;
  LargestMemoryBlock();
  ~LargestMemoryBlock()         {free();}
  size_t total();
  void alloc(size_t n);
  void free();
  void test();
};

LargestMemoryBlock::LargestMemoryBlock() : p(NULL)
{
  size_t a=0, b=UINT_MAX;
  while (b-a>1) {
    free();
    size_t c=(a+b)/2;
    alloc(c);
    if(p) a=c;  else b=c;
  }
}

size_t LargestMemoryBlock::total()
{
  if (size >= 10*mb) {               // Don't count too small memory blocks
    LargestMemoryBlock next;
    return size + next.total();
  } else {
    return 0;
  }
}

void LargestMemoryBlock::test()
{
  if ((size>>20)>0) {
    printf("Allocated %4d mb, addr=%p\n", size>>20, p);
    LargestMemoryBlock next;
    next.test();
  } else {
    memstat();
  }
}

void TestMalloc (void)
{
  memstat();
  printf("\n");
  LargestMemoryBlock m;
  m.test();
}


#ifdef FREEARC_WIN

#include <../HsFFI.h>
#include <io.h>
#include <wchar.h>
#include <sys/stat.h>

extern "C" HsInt    __w_find_sizeof       ( void ) { return sizeof(struct _wfinddatai64_t); };
extern "C" unsigned __w_find_attrib       ( struct _wfinddatai64_t* st ) { return st->attrib;      }
extern "C" time_t   __w_find_time_create  ( struct _wfinddatai64_t* st ) { return st->time_create; }
extern "C" time_t   __w_find_time_access  ( struct _wfinddatai64_t* st ) { return st->time_access; }
extern "C" time_t   __w_find_time_write   ( struct _wfinddatai64_t* st ) { return st->time_write;  }
extern "C" __int64  __w_find_size         ( struct _wfinddatai64_t* st ) { return st->size;        }
extern "C" wchar_t* __w_find_name         ( struct _wfinddatai64_t* st ) { return st->name;        }

extern "C" HsInt          __w_stat_sizeof ( void ) { return sizeof(struct _stati64); }
extern "C" unsigned short __w_stat_mode   ( struct _stati64* st ) { return st->st_mode;  }
extern "C" time_t         __w_stat_ctime  ( struct _stati64* st ) { return st->st_ctime; }
extern "C" time_t         __w_stat_atime  ( struct _stati64* st ) { return st->st_atime; }
extern "C" time_t         __w_stat_mtime  ( struct _stati64* st ) { return st->st_mtime; }
extern "C" __int64        __w_stat_size   ( struct _stati64* st ) { return st->st_size;  }


#include <windows.h>
#include <reason.h>
#include <shlobj.h>
#include <stdio.h>
#include <conio.h>
#include <time.h>

// Provide VirtualAlloc operations for testing
void LargestMemoryBlock::alloc(size_t n) {p = VirtualAlloc (0, size=n, MEM_RESERVE, PAGE_READWRITE);};
void LargestMemoryBlock::free ()         {VirtualFree (p, 0, MEM_RELEASE); p=NULL;};


// Use to convert bytes to MB
#define DIV (1024*1024)

// Specify the width of the field in which to print the numbers.
// The asterisk in the format specifier "%*I64d" takes an integer
// argument and uses it to pad and right justify the number.
#define WIDTH 4

void memstat (void)
{
  MEMORYSTATUSEX statex;

  statex.dwLength = sizeof (statex);

  GlobalMemoryStatusEx (&statex);

  printf ("There is  %*ld percent of memory in use.\n",
          WIDTH, statex.dwMemoryLoad);
  printf ("There are %*I64d total Mbytes of physical memory.\n",
          WIDTH, statex.ullTotalPhys/DIV);
  printf ("There are %*I64d free Mbytes of physical memory.\n",
          WIDTH, statex.ullAvailPhys/DIV);
  printf ("There are %*I64d total Mbytes of paging file.\n",
          WIDTH, statex.ullTotalPageFile/DIV);
  printf ("There are %*I64d free Mbytes of paging file.\n",
          WIDTH, statex.ullAvailPageFile/DIV);
  printf ("There are %*I64d total Mbytes of virtual memory.\n",
          WIDTH, statex.ullTotalVirtual/DIV);
  printf ("There are %*I64d free Mbytes of virtual memory.\n",
          WIDTH, statex.ullAvailVirtual/DIV);

  // Show the amount of extended memory available.

  printf ("There are %*I64d free Mbytes of extended memory.\n",
          WIDTH, statex.ullAvailExtendedVirtual/DIV);
}

#else

// Provide malloc operations for testing
void LargestMemoryBlock::alloc(size_t n) {p=malloc(size=n);};
void LargestMemoryBlock::free ()         {::free(p); p=NULL;};

void memstat (void)
{
}

#endif


#ifdef FREEARC_WIN

CFILENAME GetExeName (CFILENAME buf, int bufsize)
{
  GetModuleFileNameW (NULL, buf, bufsize);
  return buf;
}


#if !defined(_WIN64) && defined(__GNUC__)

typedef struct _MY_MEMORYSTATUSEX {
  DWORD dwLength;
  DWORD dwMemoryLoad;
  DWORDLONG ullTotalPhys;
  DWORDLONG ullAvailPhys;
  DWORDLONG ullTotalPageFile;
  DWORDLONG ullAvailPageFile;
  DWORDLONG ullTotalVirtual;
  DWORDLONG ullAvailVirtual;
  DWORDLONG ullAvailExtendedVirtual;
} MY_MEMORYSTATUSEX, *MY_LPMEMORYSTATUSEX;

#else

#define MY_MEMORYSTATUSEX MEMORYSTATUSEX
#define MY_LPMEMORYSTATUSEX LPMEMORYSTATUSEX

#endif

typedef BOOL (WINAPI *GlobalMemoryStatusExP)(MY_LPMEMORYSTATUSEX lpBuffer);

uint64 GetPhysicalMemory (void)
{
  MY_MEMORYSTATUSEX statx;
  statx.dwLength = sizeof(statx);
  GlobalMemoryStatusExP globalMemoryStatusEx = (GlobalMemoryStatusExP) GetProcAddress(GetModuleHandle(TEXT("kernel32.dll")), "GlobalMemoryStatusEx");
  if (globalMemoryStatusEx != 0 && globalMemoryStatusEx(&statx))
    return statx.ullTotalPhys;

  MEMORYSTATUS stat;
  stat.dwLength = sizeof(stat);
  GlobalMemoryStatus(&stat);
  return stat.dwTotalPhys;
}

unsigned GetMaxMemToAlloc (void)
{
  MEMORYSTATUS stat;
  stat.dwLength = sizeof(stat);
  GlobalMemoryStatus(&stat);

  LargestMemoryBlock block;
  return mymin(block.size, stat.dwAvailPageFile) - 5*mb;
}

unsigned GetTotalMemoryToAlloc (void)
{
  MEMORYSTATUS stat;
  stat.dwLength = sizeof(stat);
  GlobalMemoryStatus(&stat);

  LargestMemoryBlock block;
  return mymin(block.total(), stat.dwAvailPageFile) - 5*mb;
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

// Delete entrire subtree from Windows Registry
DWORD RegistryDeleteTree(HKEY hStartKey, LPTSTR pKeyName)
{
   const int MAX_KEY_LENGTH = MAX_PATH;
   DWORD   dwRtn, dwSubKeyLength;
   TCHAR   szSubKey[MAX_KEY_LENGTH];
   HKEY    hKey;

   // Do not allow NULL or empty key name
   if (pKeyName && lstrlen(pKeyName))
   {
      if((dwRtn = RegOpenKeyEx (hStartKey, pKeyName, 0, KEY_ENUMERATE_SUB_KEYS | DELETE, &hKey))  ==  ERROR_SUCCESS)
      {
         while (dwRtn == ERROR_SUCCESS )
         {
            dwSubKeyLength = MAX_KEY_LENGTH;
            dwRtn=RegEnumKeyEx(
                           hKey,
                           0,       // always index zero
                           szSubKey,
                           &dwSubKeyLength,
                           NULL,
                           NULL,
                           NULL,
                           NULL
                         );

            if(dwRtn == ERROR_NO_MORE_ITEMS)
            {
               dwRtn = RegDeleteKey(hStartKey, pKeyName);
               break;
            }
            else if(dwRtn == ERROR_SUCCESS)
               dwRtn=RegistryDeleteTree(hKey, szSubKey);
         }
         RegCloseKey(hKey);
         // Do not save return code because error
         // has already occurred
      }
   }
   else
      dwRtn = ERROR_BADKEY;

   return dwRtn;
}

int MyGetAppUserDataDirectory (CFILENAME buf)
{
  return SHGetFolderPathW(NULL, CSIDL_APPDATA, NULL, 0 /*SHGFP_TYPE_CURRENT*/, buf);
}

// Инициировать выключение компьютера
int PowerOffComputer()
{
   HANDLE hToken;
   TOKEN_PRIVILEGES tkp;

   // Get a token for this process.

   if (!OpenProcessToken(GetCurrentProcess(),
        TOKEN_ADJUST_PRIVILEGES | TOKEN_QUERY, &hToken))
      return( FALSE );

   // Get the LUID for the shutdown privilege.

   LookupPrivilegeValue(NULL, SE_SHUTDOWN_NAME,
        &tkp.Privileges[0].Luid);

   tkp.PrivilegeCount = 1;  // one privilege to set
   tkp.Privileges[0].Attributes = SE_PRIVILEGE_ENABLED;

   // Get the shutdown privilege for this process.

   AdjustTokenPrivileges(hToken, FALSE, &tkp, 0,
        (PTOKEN_PRIVILEGES)NULL, 0);

   if (GetLastError() != ERROR_SUCCESS)
      return FALSE;

   // Shut down the system and force all applications to close.

   if (!ExitWindowsEx(EWX_POWEROFF, SHTDN_REASON_MAJOR_APPLICATION | SHTDN_REASON_MINOR_MAINTENANCE | SHTDN_REASON_FLAG_PLANNED))
      return FALSE;

   //shutdown was successful
   return TRUE;
}


#else // For Unix:


#include <unistd.h>
#include <sys/sysinfo.h>

CFILENAME GetExeName (CFILENAME buf, int bufsize)
{
  int len = readlink("/proc/self/exe", buf, bufsize-1);
  if (len<0)  len=0;
  buf[len] = '\0';
  return buf;
}

uint64 GetPhysicalMemory (void)
{
  struct sysinfo si;
    sysinfo(&si);
  return uint64(si.totalram)*si.mem_unit;
}

unsigned GetTotalMemoryToAlloc (void)
{
  return INT_MAX;
}

unsigned GetMaxMemToAlloc (void)
{
  //struct sysinfo si;
  //  sysinfo(&si);
  return INT_MAX;
}

unsigned GetAvailablePhysicalMemory (void)
{
  struct sysinfo si;
    sysinfo(&si);
  return si.freeram*si.mem_unit;
}

int GetProcessorsCount (void)
{
  return get_nprocs();
}

// Инициировать выключение компьютера
int PowerOffComputer()
{
  system ("shutdown now");
  return TRUE;
}

#endif // Windows/Unix


void FormatDateTime (char *buf, int bufsize, time_t t)
{
  if (t<0)  t=INT_MAX;  // Иначе получаем вылет :(
  struct tm *p;
  p = localtime(&t);
  strftime (buf, bufsize, "%Y-%m-%d %H:%M:%S", p);
}

// Максимальная длина имени файла
int long_path_size (void)
{
  return MY_FILENAME_MAX;
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

// От-xor-ить два блока данных
void memxor (char *dest, char *src, uint size)
{
  if (size) do
      *dest++ ^= *src++;
  while (--size);
}


/* ***************************************************************************
*                                                                            *
* Random system values collection routine from CryptLib by Peter Gutmann     *
* [ftp://ftp.franken.de/pub/crypt/cryptlib/cl331.zip]                        *
*                                                                            *
*****************************************************************************/

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

	if (file_read (f, rand_buf, rand_size) != rand_size)
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

