

#include <errno.h>
#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/callback.h>
#include <caml/fail.h>
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

#if !defined(__MINGW32__) && !defined(__BEOS__)
#include <sys/mman.h>
#endif

#include "../../../config/config.h"
#include "../../utils/lib/os_stubs.h"

OFF_T os_lseek(OS_FD fd, OFF_T pos, int dir)
{
  OFF_T result =  lseek(fd, pos, dir);

  if(result < 0) unix_error(errno, "os_lseek", Nothing);

  return result;
}

ssize_t os_read(OS_FD fd, char *buf, size_t len)
{
  ssize_t result = read(fd, buf, len);

  if(result < 0) unix_error(errno, "os_read", Nothing);

  return result;
}
 

// TODO: write whole file if sparse disabled
void os_ftruncate(OS_FD fd, OFF_T len, /* bool */ int sparse)
{
  int64 cursize;
  if(!fd) failwith("ftruncate32: file is closed");
  
  cursize = os_getfdsize(fd);
  if(cursize < len){
    int zero = 0;
    OFF_T result = lseek(fd, len-1, SEEK_SET);
    if(result < 0) unix_error(errno, "os_ftruncate", Nothing);

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

int64 os_getfdsize(OS_FD fd)
{
  struct stat buf;

  if(fstat(fd, &buf) < 0)
    uerror("fstat: error in fstat",Nothing);

  return buf.st_size;
}

int64 os_getfilesize(char *path)
{
  struct stat buf;

  if(stat(path, &buf) < 0)
    uerror("fstat: error in fstat",Nothing);

  return buf.st_size;
}

void os_set_nonblock(OS_SOCKET fd)
{

}

value glibc_version(void)
{
  CAMLparam0 ();
  CAMLlocal1 (v);
#ifdef HAVE_GNU_LIBC_VERSION_H
#  include <gnu/libc-version.h>
  v = copy_string (gnu_get_libc_version());
  CAMLreturn (v);
#else
  raise_constant(*(value *)caml_named_value("not supported"));
#endif
}

#ifdef HAVE_SYS_UTSNAME_H
#include <sys/utsname.h>
void os_uname(char buf[]) {
	struct utsname uts;
  uname(&uts);
	sprintf(buf, "%s %s %s %s %s\0", 
   uts.sysname, uts.nodename, uts.release, uts.version, uts.machine);
}
#else
void os_uname(char buf[]) {
// Do nothing to buf
}
#endif
