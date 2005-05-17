#include "../../utils/lib/os_stubs.h"

#ifdef HAS_SIGNALS_H
#include <signals.h>
#endif

#ifndef INVALID_SET_FILE_POINTER
#define INVALID_SET_FILE_POINTER (-1)
#endif

#define UNIX_BUFFER_SIZE 16384

extern int os_read(OS_FD fd, char *buf, int len)
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

void os_ftruncate(OS_FD fd, int64 size)
{
  uint curpos;
  long ofs_low = (long) size;
  long ofs_high = (long) (size >> 32);

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
  return 256;
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

int64 os_lseek(OS_FD fd, int64 ofs, int cmd)
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
  return ((int64) ofs_high << 32 | ret);
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

