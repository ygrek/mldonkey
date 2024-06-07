/* File: magic.ml

   Copyright (C) 2005

     Christophe Troestler
     email: Christophe.Troestler@umh.ac.be
     WWW: http://www.umh.ac.be/math/an/software/

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation, with the
   special exception on linking described in file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details.
*/
/* 	$Id$	 */

#include "../../../config/config.h"
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/callback.h>

#include <magic.h>
#include <errno.h>
#define _XOPEN_SOURCE 600
#include <string.h>
#include <stdio.h>

/* Outputs an error message and terminates the program. */
#define DEBUG(...) \
  fprintf(stderr, "DEBUG magic_stub: " __VA_ARGS__);  \
  printf("\n")

#define CAML_MAGIC_VERSION "0.2"

/*
 * Failure
 */

static void raise_magic_failure(const char * msg)
{
  static const value* exn = NULL;
  if (!exn) caml_named_value("Magiclib.Failure");
  raise_with_string(*exn, (char *) msg);
}

/* [fname] is the function name. */
static void raise_on_error(const char* fname, magic_t cookie)
{
  const char *err_magic;
  char *errmsg; /* For thread safety of error messages */
  int flen;
  
  flen = strlen(fname);
  if ((err_magic = magic_error(cookie)) != NULL) {
    if ((errmsg = malloc(flen + strlen(err_magic) + 1)) == NULL)
      raise_out_of_memory();
    strcpy(errmsg, fname);
    strcpy(errmsg + flen, err_magic);
    raise_magic_failure(errmsg);
  }
  else {
    int len = 80;  /* Initial buffer length */
    int err;
  
    /* Allocate buffer [errmsg] until there is enough space for the
     * error message. */
    err = magic_errno(cookie);
    if ((errmsg = malloc(len)) == NULL) raise_out_of_memory();
    strcpy(errmsg, fname);
#ifdef HAVE_STRERROR_R
    while (strerror_r(err, errmsg + flen, len - flen) < 0) {
      len *= 2;
      errmsg = realloc(errmsg, len);
      if (errmsg == NULL) raise_out_of_memory();
    }
#else
      strcat (errmsg, strerror(err));        
#endif
    raise_sys_error(copy_string(errmsg));
  }
}


/*
 * magic_t
 */

/* magic_t is a pointer on 'struct magic_set' so one can set it to NULL */
#define COOKIE_VAL(v) (* ((magic_t *) Data_custom_val(v)))

/* If the cookie has not been forcibly closed with [magic_close], free it. */
static void free_cookie(value c)
{
  magic_t cookie = COOKIE_VAL(c);
  if (cookie != NULL) {
    magic_close(cookie);
    COOKIE_VAL(c) = NULL;
  }
}

/* compare magic_t pointers (=> total order) */
static int compare_cookie(value c1, value c2)
{
  magic_t cookie1 = COOKIE_VAL(c1), cookie2 = COOKIE_VAL(c2);
  
  if (cookie1 == cookie2)      return 0;
  else if (cookie1 < cookie2)  return -1;
  else return 1;
}

static struct custom_operations cookie_ops = {
    /* identifier */ "be.ac.umh.math/magic.cookie." CAML_MAGIC_VERSION,
    /* finalize */ free_cookie,
    /* compare */ compare_cookie,
    /* hash */ custom_hash_default,
    /* serialize */ custom_serialize_default,
    /* deserialize */ custom_deserialize_default
};

#define ALLOC_COOKIE alloc_custom(&cookie_ops, sizeof(magic_t), \
                     sizeof(magic_t), 40 * sizeof(magic_t))

/*
 * Stubs
 */

CAMLprim value ocaml_magic_open(value flags)
{
  CAMLparam1(flags);
  CAMLlocal1(c);
  char *errmsg;
  int len = 80;
  
  c = ALLOC_COOKIE;
  if ((COOKIE_VAL(c) = magic_open(Int_val(flags) | MAGIC_ERROR)) == NULL) {
    if (errno == EINVAL)
      /* An unsupported value for flags was given */
      raise_magic_failure("Magiclib.create: Preserve_atime not supported");
    else {
      /* No cookie yet, so one cannot use the above generic err fun */
      if ((errmsg = malloc(len)) == NULL) raise_out_of_memory();
      strcpy(errmsg, "Magiclib.create: "); /* 14 chars */
#ifdef HAVE_STRERROR_R
      while (strerror_r(errno, errmsg + 14, len - 14) < 0) {
        len *= 2;
        if ((errmsg = realloc(errmsg, len)) == NULL) raise_out_of_memory();
      }
#else
      strcat (errmsg, strerror(errno));
#endif
      raise_sys_error(copy_string(errmsg));
    }
  }
  CAMLreturn(c);
}

CAMLprim value ocaml_magic_close(value c)
{
  CAMLparam1(c);
  magic_t cookie = COOKIE_VAL(c);
  if (cookie != NULL) /* if first time it is called */
    magic_close(cookie);
  COOKIE_VAL(c) = NULL; /* For the finalization function & multiple calls */
  CAMLreturn(Val_unit);
}


CAMLprim value ocaml_magic_file(value c, value fname)
{
  CAMLparam2(c, fname);
  const char * ans;
  magic_t cookie = COOKIE_VAL(c);

  if (cookie == NULL) invalid_argument("Magiclib.file");
  if ((ans = magic_file(cookie, String_val(fname))) == NULL) {
    raise_on_error("Magiclib.file: ", cookie);
  }
  CAMLreturn(copy_string(ans));
}

CAMLprim value ocaml_magic_buffer(value c, value buf, value len)
{
  CAMLparam3(c, buf, len);
  const char * ans;
  magic_t cookie = COOKIE_VAL(c);

  if (cookie == NULL) caml_invalid_argument("Magiclib.buffer");
  if ((ans = magic_buffer(cookie, String_val(buf), Int_val(len)))
      == NULL)
    raise_on_error("Magiclib.buffer: ", cookie);
  CAMLreturn(copy_string(ans));
}


CAMLprim value ocaml_magic_setflags(value c, value flags)
{
  CAMLparam2(c, flags);
  magic_t cookie = COOKIE_VAL(c);

  if (cookie == NULL) caml_invalid_argument("Magiclib.setflags");
  if (magic_setflags(cookie, Int_val(flags)) < 0)
    raise_magic_failure("Magiclib.setflags: Preserve_atime not supported");
  CAMLreturn(Val_unit);
}



#define CHECK(fname) \
  magic_t cookie = COOKIE_VAL(c); \
  \
  if (cookie == NULL) caml_invalid_argument("Magiclib.check"); \
  if (magic_check(cookie, fname) < 0) \
    CAMLreturn(Val_false); \
  else \
    CAMLreturn(Val_true)

CAMLprim value ocaml_magic_check_default(value c)
{
  CAMLparam1(c);
  CHECK(NULL);
}
CAMLprim value ocaml_magic_check(value c, value filenames)
{
  CAMLparam2(c, filenames);
  CHECK(String_val(filenames));
}



#define COMPILE(fname) \
  magic_t cookie = COOKIE_VAL(c); \
  \
  if (cookie == NULL) caml_invalid_argument("Magiclib.compile"); \
  if (magic_compile(cookie, fname) < 0) \
    raise_on_error("Magiclib.compile: ", cookie);  \
  CAMLreturn(Val_unit)

CAMLprim value ocaml_magic_compile_default(value c)
{
  CAMLparam1(c);
  COMPILE(NULL);
}

CAMLprim value ocaml_magic_compile(value c, value filenames)
{
  CAMLparam2(c, filenames);
  COMPILE(String_val(filenames));
}



#define LOAD(fname) \
  magic_t cookie = COOKIE_VAL(c); \
  \
  if (cookie == NULL) caml_invalid_argument("Magiclib.load"); \
  if (magic_load(cookie, fname) < 0) \
    raise_on_error("Magiclib.load: ", cookie);  \
  CAMLreturn(Val_unit)

CAMLprim
value ocaml_magic_load_default(value c)
{
  CAMLparam1(c);
  LOAD(NULL);
}

CAMLprim
value ocaml_magic_load(value c, value filenames)
{
  CAMLparam2(c, filenames);
  LOAD(String_val(filenames));
}

