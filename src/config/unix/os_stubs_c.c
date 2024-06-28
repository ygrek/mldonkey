/* Copyright 2001, 2002 b8_bavard, b8_fee_carabine, INRIA */
/*
    This file is part of mldonkey.

    mldonkey is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    mldonkey is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with mldonkey; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*/


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

void os_ftruncate(OS_FD fd, OFF_T len, /* bool */ int sparse)
{
  int64_t cursize;
  if(!fd) failwith("ftruncate32: file is closed");
  
  cursize = os_getfdsize(fd);
#ifdef HAVE_POSIX_FALLOCATE 
  if (!sparse)
    if (posix_fallocate(fd,cursize,len-cursize) == 0)
      return;
#endif
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

int64_t os_getfdsize(OS_FD fd)
{
  struct stat buf;

  if(fstat(fd, &buf) < 0)
    uerror("fstat: error in fstat",Nothing);

  return buf.st_size;
}

/*******************************************************************


                         os_getfilesize


*******************************************************************/

int64_t os_getfilesize(const char *path)
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

int os_fsync(OS_FD fd)
{
   return fsync(fd);
}

