
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

  sscanf(s, "%d.%d.%d.%d", &a1, &a2, &a3, &a4);

  res = alloc(4,0);
  Field(res, 0) = Val_int(a1);
  Field(res, 1) = Val_int(a2);
  Field(res, 2) = Val_int(a3);
  Field(res, 3) = Val_int(a4);
  return res;
}
