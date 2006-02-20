

#include "../../utils/lib/os_stubs.h"



/*******************************************************************


                         os_lseek


*******************************************************************/

OFF_T os_lseek(OS_FD fd, OFF_T pos, int dir)
{
  OFF_T result =  lseek(fd, pos, dir);

  if(result < 0) unix_error(errno, "os_lseek", Nothing);

  return result;
}

/*******************************************************************


                         os_read


*******************************************************************/

ssize_t os_read(OS_FD fd, char *buf, size_t len)
{
  ssize_t result = read(fd, buf, len);

  if(result < 0) unix_error(errno, "os_read", Nothing);

  return result;
}
 
/*******************************************************************


                         os_ftruncate


*******************************************************************/

/* TODO: write whole file if sparse disabled */
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
      fprintf(stderr, "ftruncate(%d,%Ld)\n", fd, len);
      uerror("ml_truncate32: error in ftruncate",Nothing);
    }
}

/*******************************************************************


                         os_getdtablesize


*******************************************************************/

int os_getdtablesize()
{
  return getdtablesize();
}

/*******************************************************************


                         os_getfdsize


*******************************************************************/

int64 os_getfdsize(OS_FD fd)
{
  struct stat buf;

  if(fstat(fd, &buf) < 0)
    uerror("fstat: error in fstat",Nothing);

  return buf.st_size;
}

/*******************************************************************


                         os_getfilesize


*******************************************************************/

int64 os_getfilesize(char *path)
{
  struct stat buf;

  if(stat(path, &buf) < 0)
    uerror("fstat: error in fstat",Nothing);

  return buf.st_size;
}

/*******************************************************************


                         os_set_nonblock


*******************************************************************/

void os_set_nonblock(OS_SOCKET fd)
{

}

/*******************************************************************


                         glibc_version


*******************************************************************/

value glibc_version(void)
{
  CAMLparam0 ();
  CAMLlocal1 (v);

#ifdef HAVE_GNU_LIBC_VERSION_H
#include <gnu/libc-version.h>

  v = copy_string (gnu_get_libc_version());
  CAMLreturn (v);
  
#else

  raise_constant(*(value *)caml_named_value("not supported"));
  
#endif
}

/*******************************************************************


                         os_uname


*******************************************************************/

#ifdef HAVE_SYS_UTSNAME_H
#include <sys/utsname.h>

void os_uname(char buf[]) 
{
	struct utsname uts;
  uname(&uts);
	sprintf(buf, "%s %s %s %s %s", 
   uts.sysname, uts.nodename, uts.release, uts.version, uts.machine);
}

#else

void os_uname(char buf[]) 
{
/* Do nothing to buf */
}

#endif

/*******************************************************************


                         os_os_supported


*******************************************************************/

int os_os_supported()
{
	  return 1;  /* return always 1 to expect an supported os */
}
