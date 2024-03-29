/*
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with this library; if not, write to the
Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
Boston, MA  02110-1301, USA.
*/

#include "../../../config/config.h"

#ifdef USE_BZIP2
#include <bzlib.h>
#endif

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>

/* Bzip2 interface code */

#define BZStream_val(v) ((bz_stream *) (v))

static const value * camlzip_bzerror_exn = NULL;

#ifdef USE_BZIP2
static void camlzip_bzerror(char * fn, int err)
{
  char * msg;
  value s1 = Val_unit, s2 = Val_unit, bucket = Val_unit;

  if (camlzip_bzerror_exn == NULL) {
    camlzip_bzerror_exn = caml_named_value("Bzlib.Error");
    if (camlzip_bzerror_exn == NULL)
      invalid_argument("Exception Bzlib.Error not initialized");
  }
  Begin_roots3(s1, s2, bucket);
    s1 = copy_string(fn);
    switch (err) {
    case BZ_CONFIG_ERROR:
      s2 = Val_int(0);
      break;
    case BZ_SEQUENCE_ERROR:
      s2 = Val_int(1);
      break;
    case BZ_PARAM_ERROR:
      s2 = Val_int(2);
      break;
    case BZ_MEM_ERROR:
      s2 = Val_int(3);
      break;
    case BZ_DATA_ERROR:
      s2 = Val_int(4);
      break;
    case BZ_DATA_ERROR_MAGIC:
      s2 = Val_int(5);
      break;
    default:
      s2 = Val_int(6);
    }
    bucket = alloc_small(3, 0);
    Field(bucket, 0) = *camlzip_bzerror_exn;
    Field(bucket, 1) = s1;
    Field(bucket, 2) = s2;
  End_roots();
  mlraise(bucket);
}

static value camlzip_new_bzstream(void)
{
  bz_stream * bzs = (bz_stream *) malloc(sizeof(bz_stream));
  bzs->bzalloc = NULL;
  bzs->bzfree = NULL;
  bzs->opaque = NULL;
  bzs->next_in = NULL;
  bzs->next_out = NULL;
  return (value) bzs;
}

int camlzip_action_table[] = { BZ_RUN, BZ_FLUSH, BZ_FINISH };
#endif


value camlzip_bzCompressInit(value blockSize100k, value verbosity, value workFactor) {
#ifdef USE_BZIP2
  int err;
  value vbzs = camlzip_new_bzstream();
  if ((err = BZ2_bzCompressInit(BZStream_val(vbzs),
			 Int_val(blockSize100k),
			 Int_val(verbosity),
			 Int_val(workFactor))) != BZ_OK)
    camlzip_bzerror("Zlib.deflateInit", err);
  return vbzs;
#else
  failwith("Bzip2 compression not supported.");
#endif
}

value camlzip_bzCompress(value vzs, value srcbuf, value srcpos, value srclen,
                      value dstbuf, value dstpos, value dstlen,
                      value vflush)
{
#ifdef USE_BZIP2
  bz_stream * zs = BZStream_val(vzs);
  int retcode;
  long used_in, used_out;
  value res;

  zs->next_in = &Byte(srcbuf, Long_val(srcpos));
  zs->avail_in = Long_val(srclen);
  zs->next_out = &Byte(dstbuf, Long_val(dstpos));
  zs->avail_out = Long_val(dstlen);
  retcode = BZ2_bzCompress(zs, camlzip_action_table[Int_val(vflush)]);
  if (retcode < 0) camlzip_bzerror("Bzlib.compress", retcode);
  used_in = Long_val(srclen) - zs->avail_in;
  used_out = Long_val(dstlen) - zs->avail_out;
  zs->next_in = NULL;         /* not required, but cleaner */
  zs->next_out = NULL;        /* (avoid dangling pointers into Caml heap) */
  res = alloc_small(3, 0);
  Field(res, 0) = Val_bool(retcode == BZ_STREAM_END);
  Field(res, 1) = Val_int(used_in);
  Field(res, 2) = Val_int(used_out);
  return res;
#else
  failwith("Bzip2 compression not supported");
#endif
}

value camlzip_bzCompress_bytecode(value * arg, int nargs)
{
  return camlzip_bzCompress(arg[0], arg[1], arg[2], arg[3],
                         arg[4], arg[5], arg[6], arg[7]);
}

value camlzip_bzCompressEnd(value stream) {
#ifdef USE_BZIP2
  int err;
  if ((err = BZ2_bzCompressEnd(BZStream_val(stream))) != BZ_OK)
    camlzip_bzerror("Bzlib.compress_end", err);
  free(BZStream_val(stream));
#else
  failwith("Bzip2 compression not supported");
#endif
  return Val_unit;
}

value camlzip_bzDecompressInit(value verbosity, value small)
{
#ifdef USE_BZIP2
  int err;
  value vzs = camlzip_new_bzstream();
  if ((err = BZ2_bzDecompressInit(BZStream_val(vzs), Int_val(verbosity), Bool_val(small))) != BZ_OK)
    camlzip_bzerror("Bzlib.decompress_init", err);
  return vzs;
#else
  failwith("Bzip2 compression not supported");
#endif
}

value camlzip_bzDecompress(value vzs, value srcbuf, value srcpos, value srclen,
			   value dstbuf, value dstpos, value dstlen)
{
#ifdef USE_BZIP2
  bz_stream * zs = BZStream_val(vzs);
  int retcode;
  long used_in, used_out;
  value res;

  zs->next_in = &Byte(srcbuf, Long_val(srcpos));
  zs->avail_in = Long_val(srclen);
  zs->next_out = &Byte(dstbuf, Long_val(dstpos));
  zs->avail_out = Long_val(dstlen);
  retcode = BZ2_bzDecompress(zs);
  if (retcode < 0)
    camlzip_bzerror("Bzlib.decompress", retcode);
  used_in = Long_val(srclen) - zs->avail_in;
  used_out = Long_val(dstlen) - zs->avail_out;
  zs->next_in = NULL;           /* not required, but cleaner */
  zs->next_out = NULL;          /* (avoid dangling pointers into Caml heap) */
  res = alloc_small(3, 0);
  Field(res, 0) = Val_bool(retcode == BZ_STREAM_END);
  Field(res, 1) = Val_int(used_in);
  Field(res, 2) = Val_int(used_out);
  return res;
#else
  failwith("Bzip2 compression not supported");
#endif
}

value camlzip_bzDecompress_bytecode(value * arg, int nargs)
{
  return camlzip_bzDecompress(arg[0], arg[1], arg[2], arg[3],
                         arg[4], arg[5], arg[6]);
}

value camlzip_bzDecompressEnd(value stream) {
#ifdef USE_BZIP2
  int err;
  if ((err = BZ2_bzDecompressEnd(BZStream_val(stream))) != BZ_OK)
    camlzip_bzerror("Bzlib.decompressEnd", err);
  free(BZStream_val(stream));
#else
  failwith("Bzip2 compression not supported");
#endif
  return Val_unit;
}

int camlzip_bzlibversion(void)
{
  CAMLparam0 ();
  CAMLlocal1 (v);
#ifdef HAVE_BZLIBVERSION
  v = copy_string (BZ2_bzlibVersion());
  CAMLreturn (v);
#else
  failwith("bzlibVersion not found");
#endif
}
