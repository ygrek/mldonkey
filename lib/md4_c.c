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

value md4_unsafe_file (value digest_v, value filename_v)
{
  char *filename  = String_val(filename_v);
  unsigned char *digest = String_val(digest_v);
  FILE *file;
  MD4_CTX context;
  int len;

  if ((file = fopen (filename, "rb")) == NULL)
    raise_not_found();

  else {
    MD4Init (&context);
    while ((len = fread (hash_buffer, 1, HASH_BUFFER_LEN, file)))
      MD4Update (&context, hash_buffer, len);
    MD4Final (digest, &context);

    fclose (file);
  }
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

