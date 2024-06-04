/*
 * Bitstring library.
 *
 * Copyright (C) 2008-2016 Red Hat Inc., Richard W.M. Jones
 * Copyright (C) 2016 Red Hat Inc, Richard W.M. Jones, Xavier R. Guerin.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version,
 * with the OCaml linking exception described in COPYING.LIB.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

/* This file contains hand-coded, optimized C implementations of
 * certain very frequently used functions.
 */

#if defined(__APPLE__)
#include <machine/endian.h>
#elif defined(__FreeBSD__)
#include <sys/endian.h>
#elif defined(__MINGW32__)
#include <sys/param.h>
#elif defined(_WIN32) && defined(_MSC_VER) && (defined(_M_X64) || defined (_M_IX86))
#define BIG_ENDIAN      4321
#define LITTLE_ENDIAN   1234
#define BYTE_ORDER      LITTLE_ENDIAN
#else
#include <endian.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <byteswap.h>
#include <string.h>

#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/alloc.h>

/*
 * Prefix fastpath functions.
 */

static char prefix_mask_lookup[8] = {
  0x00, 0x80, 0xC0, 0xE0,
  0xF0, 0xF8, 0xFC, 0xFE
};

static
int match_partial_left(int len, char source, char prefix)
{
  register char mask = ~prefix_mask_lookup[len];
  return (source & mask) == (prefix & mask);
}

static
int match_partial_right(int len, char source, char prefix)
{
  register char mask = prefix_mask_lookup[len];
  return (source & mask) == (prefix & mask);
}

CAMLprim value
ocaml_bitstring_is_prefix_fastpath(value b1, value o1, value b2, value o2, value l2)
{
  CAMLparam5 (b1, o1, b2, o2, l2);
  int il2 = Int_val(l2);
  /*
   * Find the beginning of the bitstrings.
   */
  int bo1 = Int_val(o1) >> 3;
  int bo2 = Int_val(o2) >> 3;
  char * ptr1 = &((char *)String_val(b1))[bo1];
  char * ptr2 = &((char *)String_val(b2))[bo2];
  /*
   * Compute the left partial match if the offset mod 8 != 0.
   */
  int sh = Int_val(o2) & 0x7;
  if (sh != 0) {
    if (!match_partial_left(sh, *ptr1, *ptr2)) {
      CAMLreturn (Val_false);
    }
    il2 -= 8 - sh;
    ptr1++, ptr2++;
  }
  /*
   * Check the part of the prefix that fits in bytes using memcmp.
   */
  int bl2 = il2 >> 3;
  if (memcmp(ptr1, ptr2, bl2) != 0) {
    CAMLreturn (Val_false);
  }
  /*
   * Check the remainder of the prefix if there is any.
   */
  int rem = il2 & 0x7;
  if (rem) {
    int res = match_partial_right(rem, ptr1[bl2], ptr2[bl2]);
    CAMLreturn (Val_bool(res));
  }
  /*
   * The prefix exists.
   */
  CAMLreturn (Val_true);
}

/*
 * Extract fastpath functions.
 *
 * These are used in the common case for reading ints where the following
 * conditions are known to be true:
 * (a) the int size is a whole number of bytes (eg. 16, 24, 32, etc bits)
 * (b) the access in the match is byte-aligned
 * (c) the access in the underlying bitstring is byte-aligned
 *
 * These functions used to all be "noalloc" meaning they must not perform any
 * OCaml allocations.  However starting with OCaml 4.02, a compiler optimization
 * means that unforunately we now have to use ordinary alloc functions in some
 * cases.
 *
 * The final offset in the string is calculated by the OCaml (caller) code. All
 * we need to do is to read the string+offset and byteswap, sign-extend as
 * necessary.
 *
 * There is one function for every combination of:
 * (i) int size: 16, 32, 64 bits
 * (ii) endian: bigendian, littleendian, nativeendian
 * (iii) signed and unsigned
 *
 * XXX Future work: Expand this to 24, 40, 48, 56 bits.  This
 * requires some extra work because sign-extension won't "just happen".
 */

#if BYTE_ORDER == BIG_ENDIAN
#define swap_be(size,v)
#define swap_le(size,v) v = bswap_##size (v)
#define swap_ne(size,v)
#else
#define swap_be(size,v) v = bswap_##size (v)
#define swap_le(size,v)
#define swap_ne(size,v)
#endif

#define extract_fastpath_zero_copy(size, endian, sign, type)      \
  CAMLprim value                                                  \
  ocaml_bitstring_extract_fastpath_int##size##_##endian##_##sign  \
  (value strv, value offv)                                        \
{                                                                 \
  CAMLparam2 (strv, offv);                                        \
  type *ptr = (type *)((char *)String_val(strv) + Int_val(offv)); \
  type r;                                                         \
  memcpy(&r, ptr, sizeof(r));                                     \
  swap_##endian(size,r);                                          \
  CAMLreturn (Val_int(r));                                        \
}

#define extract_fastpath_with_copy(size, endian, sign, type)      \
  CAMLprim value                                                  \
  ocaml_bitstring_extract_fastpath_int##size##_##endian##_##sign  \
  (value strv, value offv)                                        \
{                                                                 \
  CAMLparam2 (strv, offv);                                        \
  CAMLlocal1 (rv);                                                \
  type *ptr = (type *)((char *)String_val(strv) + Int_val(offv)); \
  type r;                                                         \
  memcpy(&r, ptr, sizeof(r));                                     \
  swap_##endian(size,r);                                          \
  rv = caml_copy_int##size(r);                                    \
  CAMLreturn(rv);                                                 \
}

extract_fastpath_zero_copy(16, be, unsigned, uint16_t)
extract_fastpath_zero_copy(16, le, unsigned, uint16_t)
extract_fastpath_zero_copy(16, ne, unsigned, uint16_t)
extract_fastpath_zero_copy(16, be, signed  , int16_t )
extract_fastpath_zero_copy(16, le, signed  , int16_t )
extract_fastpath_zero_copy(16, ne, signed  , int16_t )

extract_fastpath_with_copy(32, be, unsigned, uint32_t)
extract_fastpath_with_copy(32, le, unsigned, uint32_t)
extract_fastpath_with_copy(32, ne, unsigned, uint32_t)
extract_fastpath_with_copy(32, be, signed  , int32_t )
extract_fastpath_with_copy(32, le, signed  , int32_t )
extract_fastpath_with_copy(32, ne, signed  , int32_t )

extract_fastpath_with_copy(64, be, unsigned, uint64_t)
extract_fastpath_with_copy(64, le, unsigned, uint64_t)
extract_fastpath_with_copy(64, ne, unsigned, uint64_t)
extract_fastpath_with_copy(64, be, signed  , int64_t )
extract_fastpath_with_copy(64, le, signed  , int64_t )
extract_fastpath_with_copy(64, ne, signed  , int64_t )

// vim: ts=2:sts=2:sw=2:et