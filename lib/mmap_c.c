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
#include "md4.h"

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

#define Nothing ((value) 0)

extern void unix_error (int errcode, char * cmdname, value arg) Noreturn;
extern void uerror (char * cmdname, value arg) Noreturn;

#define UNIX_BUFFER_SIZE 16384

#define T_FD 0
#define T_ADDR 1
#define T_FILE 2
#define T_LEN 3
#define T_POS 4

value mmap_mmap(value t_v)
{
  char *filename = String_val(Field(t_v, T_FILE));
  long len = Int32_val(Field(t_v, T_LEN));
  long pos = Int32_val(Field(t_v, T_POS));
  int fd = Int_val(Field(t_v, T_FD));
  char * addr;
  
/*
  if(ftruncate(fd, pos + len) < 0){
    uerror("mmap_create: error in ftruncate %d", Field(t_v, T_FILE));
  }
*/
  
  if( (addr = mmap((void*)NULL, len, 
          PROT_READ /* | PROT_WRITE */ , MAP_SHARED, fd,pos)
      )
      == MAP_FAILED) {
    uerror("mmap_create: error in mmap", Field(t_v, T_FILE));   
  }

/*  
  printf("*********mapped at %ld <-> %ld [%ld] for %ld-%ld\n", (long)addr, (long)(addr + len), len, pos, pos+len);
*/

  Field(t_v,T_FD) = Val_int(fd);
  modify(&Field(t_v,T_ADDR), (value)(addr));

  return Val_unit;
}

value mmap_truncate(value t_v, value len_v)
{
  long len = Int32_val(len_v);
  int fd = Int_val(Field(t_v, T_FD));  
  if(!fd)
    uerror("mmap_truncate: file is closed", Field(t_v, T_FILE));

  if(ftruncate(fd, len) < 0){
    Field(t_v, T_FD) = Val_int(0);
    uerror("mmap_truncate: error in ftruncate", Field(t_v, T_FILE));
  }

  Field(t_v, T_LEN) = len_v;

  return Val_unit;
}

value mmap_blit_from_string(value str_v, value spos_v, value t_v, value tpos_v, value len_v)
{
  unsigned long tpos = Int32_val(tpos_v);
  unsigned long spos = Int32_val(spos_v);
  unsigned long len = Int32_val(len_v);
  char *src = String_val(str_v);
  char *dst = (char*) Field(t_v, T_ADDR);
  unsigned long max_len = Int32_val(Field(t_v, T_LEN));
  unsigned long real_pos = Int32_val(Field(t_v, T_POS));

/*
  printf("move %lx-%lx (%ld)[%ld]\n", (unsigned long)dst, 
    (unsigned long)(dst+max_len), max_len, real_pos);
*/

  int fd = Int_val(Field(t_v, T_FD));  
  if(!fd)
    uerror("mmap_blit_from_string: file is closed", Field(t_v, T_FILE));


  src += spos;
  dst += tpos;

/*
  printf("mmap move %lx -> %lx [%ld]\n", (long)src, (long)dst, len); 
*/

  memcpy(dst, src, len);  

  return Val_unit;
}

value mmap_blit_to_string(value t_v, value tpos_v, value str_v, value spos_v, value len_v)
{
  unsigned long tpos = Int32_val(tpos_v);
  unsigned long spos = Int32_val(spos_v);
  unsigned long len = Int32_val(len_v);
  char *src = String_val(str_v);
  char *dst = (char*) Field(t_v, T_ADDR);

  int fd = Int_val(Field(t_v, T_FD));  
  if(!fd)
    uerror("mmap_blit_to_string: file is closed", Field(t_v, T_FILE));

  src += spos;
  dst += tpos;

  memcpy(src, dst, len);  

  return Val_unit;
}

value mmap_msync(value t_v)
{
  value addr_v = Field(t_v,T_ADDR);
  char *addr = ((char*)addr_v);

  int fd = Int_val(Field(t_v, T_FD));  
  if(!fd)
    uerror("mmap_sync: file is closed", Field(t_v, T_FILE));

  Assert(fd > 0);
  if(msync(addr, Int32_val(Field(t_v, T_LEN)) ,MS_SYNC)<0){
    uerror("mmap_sync: msync", Nothing);
  }
  return Val_unit;
}

value mmap_munmap(value t_v)
{
  value addr_v = Field(t_v,T_ADDR);
  char *addr = ((char*)addr_v);

  int fd = Int_val(Field(t_v, T_FD));  
  if(!fd)
    uerror("mmap_close: file is closed", Field(t_v, T_FILE));

  if(munmap(addr, Int32_val(Field(t_v, T_LEN)))<0){
    uerror("mmap_unmap: munmap", Nothing);
  }
  Field(t_v, T_FD) = Val_int(0);

  return Val_unit;
}

value mmap_md4_sub(value t_v, value pos_v, value len_v, value digest_v)
{
  long pos = Int32_val(pos_v);
  value addr_v = Field(t_v,T_ADDR);
  char *addr = ((char*)addr_v);
  long len = Int32_val(len_v);
  char *digest = String_val(digest_v);
  MD4_CTX context;

  int fd = Int_val(Field(t_v, T_FD));  
  if(!fd)
    uerror("mmap_md4_sub: file is closed", Field(t_v, T_FILE));

  addr += pos;

  MD4Init (&context);
  MD4Update (&context, addr, len);
  MD4Final (digest, &context);

  return Val_unit;
}
