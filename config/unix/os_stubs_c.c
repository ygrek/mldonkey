#include "../config.h"
#include "../../lib/os_stubs.h"



#include "caml/mlvalues.h"
#include "caml/fail.h"
#include "caml/alloc.h"
#include "caml/memory.h"


#include <errno.h>
#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <signal.h>

#ifdef HAS_SIGNALS_H
#include <signals.h>
#endif

#include <sys/types.h>
#include <sys/time.h>

#ifdef HAS_SYS_SELECT_H
#include <sys/select.h>
#endif

#include <stdio.h>
#include <sys/stat.h>
#include <fcntl.h>

#ifdef HAS_UNISTD
#include <unistd.h>
#else
#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2
#endif

#if defined(__OpenBSD__) || defined(__FreeBSD__)
#include <string.h>
#endif

#if !defined(__MINGW32__)
#include <sys/mman.h>
#endif

int os_lseek(OS_FD fd, int pos, int dir)
{
  return lseek(fd, pos, dir);
}

int os_read(OS_FD fd, char *buf, int len)
{
  return read(fd, buf, len);
}
 
void os_ftruncate(OS_FD fd, int len)
{
  unsigned long cursize;
  if(!fd) failwith("ftruncate32: file is closed");
  
  cursize = os_getfdsize(fd);
  if(cursize < len){
    int zero = 0;
    lseek(fd, len-1, SEEK_SET);
    write(fd, &zero, 1);
  } else
    if((cursize != len) && (ftruncate(fd, len) < 0)) {
      fprintf(stderr, "ftruncate(%d,%d)\n", fd, len);
      uerror("ml_truncate32: error in ftruncate",Nothing);
    }
}

int os_getdtablesize()
{
  return getdtablesize();
}

int os_getfdsize(OS_FD fd)
{
  struct stat buf;

  if(fstat(fd, &buf) < 0)
    uerror("fstat: error in fstat",Nothing);

  return buf.st_size;
}

int os_getfilesize(char *path)
{
  struct stat buf;

  if(stat(path, &buf) < 0)
    uerror("fstat: error in fstat",Nothing);

  return buf.st_size;
}

