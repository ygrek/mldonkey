/* Copyright 2005 b8_bavard, INRIA */
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

/* Stub code to interface with libiconv
 *
 * taken and modified from
 *   glib-2.4.7/glib/gconvert.c
 *   glib-2.4.7/glib/gutf8.c
 *   lablgtk-2.4.0/src/ml_glib.c
 *   libiconv-1.9.1/src/iconv.c
 *
 */

#include "../../../config/config.h"
#include <stdio.h>
#include <string.h>
#include <errno.h>

#ifdef LOCALECHARSET
  #include <localcharset.h>
#else
  #include <locale.h>
  #include <langinfo.h>
#endif
#include <iconv.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>

#define FALSE 0
#define TRUE  1

#define SizedString_val(x) String_val(x), string_length(x)

#define UTF8_COMPUTE(Char, Mask, Len)					      \
  if (Char < 128)							      \
    {									      \
      Len = 1;								      \
      Mask = 0x7f;							      \
    }									      \
  else if ((Char & 0xe0) == 0xc0)					      \
    {									      \
      Len = 2;								      \
      Mask = 0x1f;							      \
    }									      \
  else if ((Char & 0xf0) == 0xe0)					      \
    {									      \
      Len = 3;								      \
      Mask = 0x0f;							      \
    }									      \
  else if ((Char & 0xf8) == 0xf0)					      \
    {									      \
      Len = 4;								      \
      Mask = 0x07;							      \
    }									      \
  else if ((Char & 0xfc) == 0xf8)					      \
    {									      \
      Len = 5;								      \
      Mask = 0x03;							      \
    }									      \
  else if ((Char & 0xfe) == 0xfc)					      \
    {									      \
      Len = 6;								      \
      Mask = 0x01;							      \
    }									      \
  else									      \
    Len = -1;

#define UTF8_LENGTH(Char)              \
  ((Char) < 0x80 ? 1 :                 \
   ((Char) < 0x800 ? 2 :               \
    ((Char) < 0x10000 ? 3 :            \
     ((Char) < 0x200000 ? 4 :          \
      ((Char) < 0x4000000 ? 5 : 6)))))
   

#define UTF8_GET(Result, Chars, Count, Mask, Len)			      \
  (Result) = (Chars)[0] & (Mask);					      \
  for ((Count) = 1; (Count) < (Len); ++(Count))				      \
    {									      \
      if (((Chars)[(Count)] & 0xc0) != 0x80)				      \
	{								      \
	  (Result) = -1;						      \
	  break;							      \
	}								      \
      (Result) <<= 6;							      \
      (Result) |= ((Chars)[(Count)] & 0x3f);				      \
    }

#define UNICODE_VALID(Char)                   \
    ((Char) < 0x110000 &&                     \
     (((Char) & 0xFFFFF800) != 0xD800) &&     \
     ((Char) < 0xFDD0 || (Char) > 0xFDEF) &&  \
     ((Char) & 0xFFFE) != 0xFFFE)

void
raise_error(void)
{
  static value * closure_f = NULL;
  if (closure_f == NULL) {
    /* First time around, look up by name */
    closure_f = caml_named_value("charset_error");
  }
  raise_constant(*closure_f);
}

int
utf8_validate (const char  *str,
               size_t      max_len,    
               const char  **end)
{

  const char *p;

  if (str == NULL)
    return FALSE;

  if (end)
    *end = str;
  
  p = str;
  
  while ((max_len < 0 || (p - str) < max_len) && *p)
    {
      int i, mask = 0, len;
      unsigned int result;
      unsigned char c = (unsigned char) *p;
      
      UTF8_COMPUTE (c, mask, len);

      if (len == -1)
        break;

      /* check that the expected number of bytes exists in str */
      if (max_len >= 0 &&
          ((max_len - (p - str)) < len))
        break;
        
      UTF8_GET (result, p, i, mask, len);

      if (UTF8_LENGTH (result) != len) /* Check for overlong UTF-8 */
	break;

      if (result == (unsigned int)-1)
        break;

      if (!UNICODE_VALID (result))
	break;
      
      p += len;
    }

  if (end)
    *end = p;

  /* See that we covered the entire length if a length was
   * passed in, or that we ended on a nul if not
   */
  if (max_len >= 0 &&
      p != (str + max_len))
    return FALSE;
  else if (max_len < 0 &&
           *p != '\0')
    return FALSE;
  else
    return TRUE;
}

size_t 
ml_iconv (iconv_t cd,
          char    **inbuf,
          size_t  *inbytes_left,
          char    **outbuf,
          size_t  *outbytes_left)
{
  return iconv (cd, inbuf, inbytes_left, outbuf, outbytes_left);
}

char*
ml_convert_with_iconv (const char *str,
                       size_t     len,
                       iconv_t    cd,
                       size_t     *bytes_read, 
                       size_t     *bytes_written)
{
  char *dest;
  char *outp;
  const char *p;
  size_t inbytes_remaining;
  size_t outbytes_remaining;
  size_t outbuf_size;
  size_t err;
  int have_error = FALSE;

  if (len < 0)
    len = strlen (str);

  p = str;
  inbytes_remaining = len;
  outbuf_size = len + 1; /* + 1 for nul in case len == 1 */
  
  outbytes_remaining = outbuf_size - 1; /* -1 for nul */
  outp = dest = malloc (outbuf_size);

 again:
  
  err = ml_iconv (cd, (char **)&p, &inbytes_remaining, &outp, &outbytes_remaining);

  if (err == (size_t) -1)
    {
      switch (errno)
	{
	case EINVAL:
	  /* Incomplete text, do not report an error */
	  break;
	case E2BIG:
	  {
	    size_t used = outp - dest;

	    outbuf_size *= 2;
	    dest = realloc (dest, outbuf_size);
		
	    outp = dest + used;
	    outbytes_remaining = outbuf_size - used - 1; /* -1 for nul */

	    goto again;
	  }
	case EILSEQ:
	  have_error = TRUE;
	  break;
	default:
	  have_error = TRUE;
	  break;
	}
    }

  *outp = '\0';
  
  if (bytes_read)
    *bytes_read = p - str;
  else
    {
      if ((p - str) != len) 
	{
          if (!have_error)
            {
              have_error = TRUE;
            }
	}
    }

  if (bytes_written)
    *bytes_written = outp - dest;	/* Doesn't include '\0' */

  if (have_error)
    {
      free (dest);
      return NULL;
    }
  else
    return dest;
}

char*
ml_convert (const char *str,
            size_t     len,  
            const char *to_codeset,
            const char *from_codeset,
            size_t     *bytes_read, 
	    size_t     *bytes_written)
{
  char *res;
  iconv_t cd;

  cd = iconv_open (to_codeset, from_codeset);

  if (cd == (iconv_t) -1)
    {
      if (bytes_read)
        *bytes_read = 0;
      
      if (bytes_written)
        *bytes_written = 0;
      
      return NULL;
    }

  res = ml_convert_with_iconv (str, len, cd,
			      bytes_read, bytes_written);
  
  iconv_close (cd);

  return res;
}

static value
ml_copy_string_len_and_free (char *str, size_t len)
{
  value v;

  if (!str)
    raise_error ();

  v = alloc_string (len);
  memcpy (String_val(v), str, len);
  free (str);
  return v;
}

value ml_convert_string(value str, value to, value from)
{
  size_t bw = 0;
  char* c_res;

  c_res = ml_convert(String_val(str),string_length(str),
                    String_val(to),String_val(from),
                    NULL,&bw);

  return ml_copy_string_len_and_free (c_res, bw);
}

value ml_utf8_validate(value s)
{
  return Val_bool(utf8_validate(SizedString_val(s),NULL));
}

value ml_locale_charset(void)
{

  const char *str;

  #ifdef LOCALECHARSET
    str = locale_charset ();
  #else
    setlocale(LC_CTYPE, "");
    str = nl_langinfo(CODESET);
  #endif

  if (!str)
    raise_error ();

  return (copy_string ((char*) str));
}
