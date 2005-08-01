

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

#if !defined(__MINGW32__)
#include <sys/mman.h>
#endif

#include "../../../config/config.h"
#include "../../utils/lib/os_stubs.h"

int64 os_lseek(OS_FD fd, off_t pos, int dir)
{
  off_t result =  lseek(fd, pos, dir);

  if(result < 0) unix_error(errno, "os_lseek", Nothing);

  return result;
}

int os_read(OS_FD fd, char *buf, int len)
{
  int result = read(fd, buf, len);

  if(result < 0) unix_error(errno, "os_read", Nothing);

  return result;
}
 
void os_ftruncate(OS_FD fd, off_t len)
{
  int64 cursize;
  if(!fd) failwith("ftruncate32: file is closed");
  
  cursize = os_getfdsize(fd);
  if(cursize < len){
    int zero = 0;
    off_t result = lseek(fd, len-1, SEEK_SET);
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

#ifdef HAVE_SYS_PARAM_H
#  include <sys/param.h>
#  if (defined(__FreeBSD__) && __FreeBSD_version >= 503001) || defined(__OpenBSD__) || defined(__NetBSD__) || defined(__APPLE__)
#    include <sys/mount.h>
#    define HAVE_STATS 1
#  endif
#endif
#ifdef HAVE_SYS_VFS_H
#  include <sys/vfs.h>
#  define HAVE_STATS 1
#endif

#ifdef HAVE_STATS
static value
#if ((defined (sun) || defined (__sun__)))
copy_statfs (struct statvfs *buf)
#else
copy_statfs (struct statfs *buf)
#endif
{
  CAMLparam0 ();
  CAMLlocal2 (bufv, v);
  bufv = caml_alloc (11, 0);
#if ((defined (sun) || defined (__sun__))) || (defined(__FreeBSD__) && __FreeBSD_version >= 503001) || defined(__OpenBSD__) || defined(__NetBSD__)
  v = copy_int64 (-1); caml_modify (&Field (bufv, 0), v);
#else
  v = copy_int64 (buf->f_type); caml_modify (&Field (bufv, 0), v);
#endif /* ((defined (sun) || defined (__sun__))) || (defined(__FreeBSD__) && __FreeBSD_version >= 503001) || defined(__OpenBSD__) || defined(__NetBSD__) */
  v = copy_int64 (buf->f_bsize); caml_modify (&Field (bufv, 1), v);
  v = copy_int64 (buf->f_blocks); caml_modify (&Field (bufv, 2), v);
  v = copy_int64 (buf->f_bfree); caml_modify (&Field (bufv, 3), v);
  v = copy_int64 (buf->f_bavail); caml_modify (&Field (bufv, 4), v);
  v = copy_int64 (buf->f_files); caml_modify (&Field (bufv, 5), v);
  v = copy_int64 (buf->f_ffree); caml_modify (&Field (bufv, 6), v);
#if ((defined (sun) || defined (__sun__)))
  v = copy_int64 (-1); caml_modify (&Field (bufv, 7), v);
  v = copy_int64 (buf->f_namemax); caml_modify (&Field (bufv, 8), v);
  v = copy_string (buf->f_basetype); caml_modify (&Field (bufv, 9), v);
  v = copy_int64 (buf->f_frsize); caml_modify (&Field (bufv, 10), v);
#else
#if (defined(__FreeBSD__) && __FreeBSD_version >= 503001) || defined(__OpenBSD__) || defined(__NetBSD__) || defined(__APPLE__)
#  if defined(__OpenBSD__) || defined(__NetBSD__)
#    include <sys/syslimits.h>
     v = copy_int64 (NAME_MAX); caml_modify (&Field (bufv, 8), v);
#  else
#    if defined(__APPLE__)
#      include <unistd.h>
       v = copy_int64 (_PC_NAME_MAX); caml_modify (&Field (bufv ,8 ), v);
#    else
       v = copy_int64 (buf->f_namemax); caml_modify (&Field (bufv, 8), v);
#    endif /* (__APPLE__) */
#  endif /* (__OpenBSD__) || defined(__NetBSD__) */
  v = copy_string (buf->f_fstypename); caml_modify (&Field (bufv, 9), v);
#else
  v = copy_int64 (buf->f_namelen); caml_modify (&Field (bufv, 8), v);
  v = copy_string ("-1"); caml_modify (&Field (bufv, 9), v);
#endif /* (defined(__FreeBSD__) && __FreeBSD_version >= 503001) || defined(__OpenBSD__) || defined(__NetBSD__) || defined(__APPLE__) */
  caml_modify (&Field (bufv, 7), Val_unit);
  v = copy_int64 (-1); caml_modify (&Field (bufv, 10), v);
#endif /*  ((defined (sun) || defined (__sun__))) */
  CAMLreturn (bufv);
}
#endif

CAMLprim value
statfs_statfs (value pathv)
{
#ifdef HAVE_STATS
  CAMLparam1 (pathv);
  CAMLlocal1 (bufv);
  const char *path = String_val (pathv);
#if ((defined (sun) || defined (__sun__)))
  struct statvfs buf;
  if (statvfs (path, &buf) == -1)
#else
  struct statfs buf;
  if (statfs (path, &buf) == -1)
#endif
    raise_constant(*(value *)caml_named_value("error"));
  bufv = copy_statfs (&buf);
  CAMLreturn (bufv);
#else
  raise_constant(*(value *)caml_named_value("not supported"));
#endif
}

CAMLprim value
statfs_fstatfs (value fdv)
{
#ifdef HAVE_STATS
  CAMLparam1 (fdv);
  CAMLlocal1 (bufv);
  int fd = Int_val (fdv);
#if ((defined (sun) || defined (__sun__)))
  struct statvfs buf;
  if (fstatvfs (fd, &buf) == -1)
#else
  struct statfs buf;
  if (fstatfs (fd, &buf) == -1)
#endif
    raise_constant(*(value *)caml_named_value("error"));
  bufv = copy_statfs (&buf);
  CAMLreturn (bufv);
#else
  raise_constant(*(value *)caml_named_value("not supported"));
#endif
}

int glibc_version(void)
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
