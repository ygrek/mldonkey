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

#include "caml/mlvalues.h"
#include "caml/fail.h"
#include "caml/alloc.h"

value md4_unsafe_string(value digest_v, value string_v, value len_v)
{
  unsigned char *digest = String_val(digest_v);
  unsigned char *string = String_val(string_v);
  long len = Long_val(len_v);
  MD4_CTX context;

  MD4Init (&context);
  MD4Update (&context, string, len);
  MD4Final (digest, &context);
 
  return Val_unit;
}

#include <stdio.h>

#define BUFFER_LEN 8192

value md4_unsafe_file (value digest_v, value filename_v)
{
  char *filename  = String_val(filename_v);
  unsigned char *digest = String_val(digest_v);
  FILE *file;
  MD4_CTX context;
  int len;
  unsigned char buffer[BUFFER_LEN];

  if ((file = fopen (filename, "rb")) == NULL)
    raise_not_found();

  else {
    MD4Init (&context);
    while ((len = fread (buffer, 1, BUFFER_LEN, file)))
      MD4Update (&context, buffer, len);
    MD4Final (digest, &context);

    fclose (file);
  }
  return Val_unit;
}

#ifdef HAS_UNISTD
#include <unistd.h>
#else
#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2
#endif

#include <sys/types.h>
#include <sys/stat.h>

value md4_unsafe_fd (value digest_v, value fd_v, value pos_v, value len_v)
{
  int fd = Int_val(fd_v);
  long pos = Int32_val(pos_v);
  long len = Int32_val(len_v);
  unsigned char *digest = String_val(digest_v);
  MD4_CTX context;
  int nread;
  unsigned char buffer[BUFFER_LEN];

  MD4Init (&context);
  lseek(fd, pos, SEEK_SET);

  while (len!=0){
    int max_nread = BUFFER_LEN > len ? len : BUFFER_LEN;

    nread = read (fd, buffer, max_nread);

    if(nread == 0){
      MD4Final (digest, &context);

      return Val_unit;
    }

    MD4Update (&context, buffer, nread);
    len -= nread;
  }
  MD4Final (digest, &context);

  return Val_unit;
}

value md4_xor(value m1_v, value m2_v, value m3_v) 
{
  char *m1 = String_val(m1_v);
  char *m2 = String_val(m2_v);
  char *m3 = String_val(m3_v);
  int i;

  for(i = 0; i<16; i++) m3[i] = m1[i] ^ m2[i];

  return Val_unit;
}

