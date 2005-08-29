#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/callback.h>
#include <caml/fail.h>

#include "../../utils/lib/os_stubs.h"

#ifdef HAS_SIGNALS_H
#include <signals.h>
#endif

#ifndef INVALID_SET_FILE_POINTER
#define INVALID_SET_FILE_POINTER (-1)
#endif

#define UNIX_BUFFER_SIZE 16384

extern void enter_blocking_section(); 
extern void leave_blocking_section();

extern ssize_t os_read(OS_FD fd, char *buf, size_t len)
{
  DWORD numread;
  BOOL ret;

  if (len > UNIX_BUFFER_SIZE) len = UNIX_BUFFER_SIZE;

  enter_blocking_section();
  ret = ReadFile(fd, buf, len, &numread, NULL);
  leave_blocking_section();
  if (! ret) {
    win32_maperr(GetLastError());
    uerror("os_read", Nothing);
  }
  return numread;
}

#include <winioctl.h>

void os_ftruncate(OS_FD fd, OFF_T size)
{
  uint curpos;
  long ofs_low = (long) size;
  long ofs_high = (long) (size >> 32);

	DWORD dw;
	BOOL bRet = DeviceIoControl(fd, FSCTL_SET_SPARSE, NULL, 0, NULL, 0, &dw, NULL);
	if (!bRet) {
		// No sparse files for you, sucker...
		// DWORD err = GetLastError();
	}
  curpos = SetFilePointer (fd, 0, NULL, FILE_CURRENT);
  if (curpos == 0xFFFFFFFF
      || SetFilePointer (fd, ofs_low, &ofs_high, FILE_BEGIN) == 0xFFFFFFFF
      || !SetEndOfFile (fd))
    {
      long err = GetLastError();
      if (err != NO_ERROR) {
	win32_maperr(err);
	uerror("os_ftruncate", Nothing);
      }
    }
}

int os_getdtablesize()
{
  return 32767;
}

int64 os_getfdsize(OS_FD fd)
{
  long len_high;
  int64 ret;

  ret = GetFileSize(fd, &len_high);
  return ((int64) len_high << 32 | ret);
}

int64 os_getfilesize(char *path)
{
  OS_FD fd = CreateFile(path, GENERIC_READ, FILE_SHARE_READ,
			NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL,
			NULL);
  long len_high;
  long ret;
  if (fd != INVALID_HANDLE_VALUE){
    ret = GetFileSize(fd, &len_high);
    CloseHandle(fd);
    return  ((int64) len_high << 32 | ret);
  } else {
    long err = GetLastError();
    if (err != NO_ERROR) {
	win32_maperr(err);
	uerror("os_getfilesize", Nothing);
    }
  }
}

OFF_T os_lseek(OS_FD fd, OFF_T ofs, int cmd)
{
  long ret;
  long ofs_low = ofs;
  long ofs_high = (long) (ofs >> 32);
  long err;

  ret = SetFilePointer(fd, ofs_low, &ofs_high, cmd);
  if (ret == INVALID_SET_FILE_POINTER) {
    err = GetLastError();
    if (err != NO_ERROR) {
      win32_maperr(err);
      uerror("os_lseek", Nothing);
    }
  }
  return ((OFF_T) ofs_high << 32 | ret);
}

#include <winsock2.h>

void os_set_nonblock(OS_SOCKET fd)
{
  u_long optval = 1;

  if( ioctlsocket(fd, FIONBIO, &optval) != 0){
    long err = GetLastError();
    if (err != NO_ERROR) {
      win32_maperr(err);
      uerror("os_set_nonblock", Nothing);
    }
  }
}


//http://lists.gnu.org/archive/html/bug-gnu-chess/2004-01/msg00020.html
void gettimeofday(struct timeval* p, void* tz /* IGNORED */){
   union {
     long long ns100; /*time since 1 Jan 1601 in 100ns units */
     FILETIME ft;
   } _now;

   GetSystemTimeAsFileTime( &(_now.ft) );
   p->tv_usec=(long)((_now.ns100 / 10LL) % 1000000LL );
   p->tv_sec= (long)((_now.ns100-(116444736000000000LL))/10000000LL);
   return;
}

