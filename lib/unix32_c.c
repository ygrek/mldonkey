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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

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

#define ZEROS_LEN 1024
value ml_truncate32(value fd_v, value len_v)
{
  unsigned long len = Int32_val(len_v);
  int fd = Int_val(fd_v);  
  struct stat buf;
  unsigned long cursize;
  if(!fd)
    failwith("ftruncate32: file is closed");

  
  if(fstat(fd, &buf) < 0)
    uerror("ml_truncate32: error in fstat",Nothing);

  cursize = buf.st_size;
  if(cursize < len){
    int zero = 0;
    lseek(fd, len-1, SEEK_SET);
    write(fd, &zero, 1);
  } else
    if(ftruncate(fd, len) < 0) 
      uerror("ml_truncate32: error in ftruncate",Nothing);
      
  return Val_unit;
}

value ml_strstr(value s_v, value sub_v)
{
  char *s = String_val(s_v);
  char *sub = String_val(sub_v);

  if(strstr(s, sub) == NULL) {
    return Val_false;
  }
  return Val_true;
}

