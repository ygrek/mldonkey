#include "../../utils/lib/os_stubs.h"

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

void os_ftruncate(OS_FD fd, int size)
{
  uint curpos;

  curpos = SetFilePointer (fd, 0, NULL, FILE_CURRENT);
  if (curpos == 0xFFFFFFFF
      || SetFilePointer (fd, size, NULL, FILE_BEGIN) == 0xFFFFFFFF
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

int os_getfdsize(OS_FD fd)
{
  long len_high;

  return GetFileSize(fd, &len_high);
}

int os_getfilesize(char *path)
{
  OS_FD fd = CreateFile(path, GENERIC_READ, FILE_SHARE_READ,
			NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL,
			NULL);
  if (fd != INVALID_HANDLE_VALUE){
    int size = GetFileSize(fd, NULL);
    CloseHandle(fd);
  } else {
    long err = GetLastError();
    if (err != NO_ERROR) {
	win32_maperr(err);
	uerror("os_getfilesize", Nothing);
    }
  }
}

int os_lseek(OS_FD fd, int ofs, int cmd)
{
  long ret;
  long ofs_low = ofs;
  long ofs_high = ofs_low >= 0 ? 0 : -1;
  long err;

  ret = SetFilePointer(fd, ofs_low, &ofs_high, cmd);
  if (ret == INVALID_SET_FILE_POINTER) {
    err = GetLastError();
    if (err != NO_ERROR) {
      win32_maperr(err);
      uerror("os_lseek", Nothing);
    }
  }
  if (ofs_high != 0 || ret > Max_long) {
    win32_maperr(ERROR_ARITHMETIC_OVERFLOW);
    uerror("os_lseek", Nothing);
  }
  return ret;
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


