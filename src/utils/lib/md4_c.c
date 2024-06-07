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

#include "os_stubs.h"
#include "md4.h"

#include "caml/mlvalues.h"
#include "caml/fail.h"
#include "caml/alloc.h"

#include <stdio.h>

value md4_xor(value m1_v, value m2_v, value m3_v) 
{
  int len = caml_string_length(m1_v);
  const char *m1 = String_val(m1_v);
  const char *m2 = String_val(m2_v);
  char *m3 = Bytes_val(m3_v);
  int i;

  for(i = 0; i<len; i++) m3[i] = m1[i] ^ m2[i];

  return Val_unit;
}

