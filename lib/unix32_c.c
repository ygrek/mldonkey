
#include "caml/memory.h"
#include "caml/fail.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/mman.h>
#include <string.h>

#ifdef HAS_UNISTD
#include <unistd.h>
#endif

#include "caml/alloc.h"
/* #include <mlvalues.h> */
/* #include "unixsupport.h" */

#define Nothing ((value) 0)

extern void unix_error (int errcode, char * cmdname, value arg) Noreturn;
extern void uerror (char * cmdname, value arg) Noreturn;

#ifdef HAS_UNISTD
#include <unistd.h>
#else
#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2
#endif

static int seek_command_table[] = {
  SEEK_SET, SEEK_CUR, SEEK_END
};

#include <stdio.h>

value ml_lseek32(value fd, value ofs, value cmd)
{
  long ret;
  long pos = Int32_val(ofs);
  ret = lseek(Int_val(fd), pos,
                       seek_command_table[Int_val(cmd)]);
  if (ret == -1) uerror("lseek", Nothing);
  return copy_int32(ret);
}

#include <sys/types.h>
#include <sys/stat.h>

value ml_getsize32(value path)
{
  int ret;
  struct stat buf;
  ret = stat(String_val(path), &buf);
  if (ret == -1) uerror("stat", path);
  return copy_int32(buf.st_size);
}


value ml_truncate32(value fd_v, value len_v)
{
  long len = Int32_val(len_v);
  int fd = Int_val(fd_v);  
  if(!fd)
    failwith("ftruncate32: file is closed");

  if(ftruncate(fd, len) < 0){

    uerror("ml_truncate32: error in ftruncate",Nothing);
  }
  return Val_unit;
}
