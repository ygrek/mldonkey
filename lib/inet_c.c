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

#include "caml/mlvalues.h"
#include "caml/fail.h"
#include "caml/alloc.h"

#include <sys/types.h>
#include <stdio.h>

value ints_of_string(value s_v)
{
  char *s = String_val(s_v);
  uint a1,a2,a3,a4;
  value res;
  char *curs = s;
  char *first;
  char c;

  first = curs;
  while(isdigit(*curs)) curs++;
  if(*curs != '.' || curs == first || curs - first > 3) goto error;
  *curs = 0;
  a1 = atoi(first);
  *curs++ = '.';

  first = curs;
  while(isdigit(*curs)) curs++;
  if(*curs != '.' || curs == first || curs - first > 3) goto error;
  *curs = 0;
  a2 = atoi(first);
  *curs++ = '.';

  first = curs;
  while(isdigit(*curs)) curs++;
  if(*curs != '.' || curs == first || curs - first > 3) goto error;
  *curs = 0;
  a3 = atoi(first);
  *curs++ = '.';

  first = curs;
  while(isdigit(*curs)) curs++;
  if(curs == first || curs - first > 3) goto error;
  c = *curs;  *curs = 0;
  a4 = atoi(first);
  *curs++ = c;

/*   sscanf(s, "%d.%d.%d.%d", &a1, &a2, &a3, &a4); */
  goto ok;

  error:
  a1 = a2 = a3 = a4 = 0;

  ok:
  res = alloc(4,0);
  Field(res, 0) = Val_int(a1);
  Field(res, 1) = Val_int(a2);
  Field(res, 2) = Val_int(a3);
  Field(res, 3) = Val_int(a4);
  return res;
}
