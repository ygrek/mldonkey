/*
  Copyright (C) 1999 Aladdin Enterprises.  All rights reserved.

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.

  L. Peter Deutsch
  ghost@aladdin.com

 */
/*
  Independent implementation of MD5 (RFC 1321).

  This code implements the MD5 Algorithm defined in RFC 1321.
  It is derived directly from the text of the RFC and not from the
  reference implementation.

  The original and principal author of md5.c is L. Peter Deutsch
  <ghost@aladdin.com>.  Other authors are noted in the change history
  that follows (in reverse chronological order):

  1999-11-04 lpd Edited comments slightly for automatic TOC extraction.
  1999-10-18 lpd Fixed typo in header comment (ansi2knr rather than md5).
  1999-05-03 lpd Original version.
 */

#include "md5.h"
#include <string.h>

#ifdef TEST
/*
 * Compile with -DTEST to create a self-contained executable test program.
 * The test program should print out the same values as given in section
 * A.5 of RFC 1321, reproduced below.
 */
#include <string.h>
main()
{
    static const char *const test[7] = {
	"", /*d41d8cd98f00b204e9800998ecf8427e*/
	"945399884.61923487334tuvga", /*0cc175b9c0f1b6a831c399e269772661*/
	"abc", /*900150983cd24fb0d6963f7d28e17f72*/
	"message digest", /*f96b697d7cb7938d525a2f31aaf161d0*/
	"abcdefghijklmnopqrstuvwxyz", /*c3fcd3d76192e4007dfb496cca67e13b*/
	"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789",
				/*d174ab98d277d9f5a5611c2c9f419d9f*/
	"12345678901234567890123456789012345678901234567890123456789012345678901234567890" /*57edf4a22be3c955ac49da2e2107b67a*/
    };
    int i;

    for (i = 0; i < 7; ++i) {
	md5_state_t state;
	md5_byte_t digest[16];
	int di;

	md5_init(&state);
	md5_append(&state, (const md5_byte_t *)test[i], strlen(test[i]));
	md5_finish(&state, digest);
	printf("MD5 (\"%s\") = ", test[i]);
	for (di = 0; di < 16; ++di)
	    printf("%02x", digest[di]);
	printf("\n");
    }
    return 0;
}
#endif /* TEST */


/*
 * For reference, here is the program that computed the T values.
 */
#if 0
#include <math.h>
main()
{
    int i;
    for (i = 1; i <= 64; ++i) {
	unsigned long v = (unsigned long)(4294967296.0 * fabs(sin((double)i)));
	printf("#define T%d 0x%08lx\n", i, v);
    }
    return 0;
}
#endif
/*
 * End of T computation program.
 */
#define T1 0xd76aa478
#define T2 0xe8c7b756
#define T3 0x242070db
#define T4 0xc1bdceee
#define T5 0xf57c0faf
#define T6 0x4787c62a
#define T7 0xa8304613
#define T8 0xfd469501
#define T9 0x698098d8
#define T10 0x8b44f7af
#define T11 0xffff5bb1
#define T12 0x895cd7be
#define T13 0x6b901122
#define T14 0xfd987193
#define T15 0xa679438e
#define T16 0x49b40821
#define T17 0xf61e2562
#define T18 0xc040b340
#define T19 0x265e5a51
#define T20 0xe9b6c7aa
#define T21 0xd62f105d
#define T22 0x02441453
#define T23 0xd8a1e681
#define T24 0xe7d3fbc8
#define T25 0x21e1cde6
#define T26 0xc33707d6
#define T27 0xf4d50d87
#define T28 0x455a14ed
#define T29 0xa9e3e905
#define T30 0xfcefa3f8
#define T31 0x676f02d9
#define T32 0x8d2a4c8a
#define T33 0xfffa3942
#define T34 0x8771f681
#define T35 0x6d9d6122
#define T36 0xfde5380c
#define T37 0xa4beea44
#define T38 0x4bdecfa9
#define T39 0xf6bb4b60
#define T40 0xbebfbc70
#define T41 0x289b7ec6
#define T42 0xeaa127fa
#define T43 0xd4ef3085
#define T44 0x04881d05
#define T45 0xd9d4d039
#define T46 0xe6db99e5
#define T47 0x1fa27cf8
#define T48 0xc4ac5665
#define T49 0xf4292244
#define T50 0x432aff97
#define T51 0xab9423a7
#define T52 0xfc93a039
#define T53 0x655b59c3
#define T54 0x8f0ccc92
#define T55 0xffeff47d
#define T56 0x85845dd1
#define T57 0x6fa87e4f
#define T58 0xfe2ce6e0
#define T59 0xa3014314
#define T60 0x4e0811a1
#define T61 0xf7537e82
#define T62 0xbd3af235
#define T63 0x2ad7d2bb
#define T64 0xeb86d391

static void
md5_process(md5_state_t *pms, const md5_byte_t *data /*[64]*/)
{
    md5_word_t
	a = pms->abcd[0], b = pms->abcd[1],
	c = pms->abcd[2], d = pms->abcd[3];
    md5_word_t t;

#ifndef ARCH_IS_BIG_ENDIAN
# define ARCH_IS_BIG_ENDIAN 1	/* slower, default implementation */
#endif
#if ARCH_IS_BIG_ENDIAN

    /*
     * On big-endian machines, we must arrange the bytes in the right
     * order.  (This also works on machines of unknown byte order.)
     */
    md5_word_t X[16];
    const md5_byte_t *xp = data;
    int i;

    for (i = 0; i < 16; ++i, xp += 4)
	X[i] = xp[0] + (xp[1] << 8) + (xp[2] << 16) + (xp[3] << 24);

#else  /* !ARCH_IS_BIG_ENDIAN */

    /*
     * On little-endian machines, we can process properly aligned data
     * without copying it.
     */
    md5_word_t xbuf[16];
    const md5_word_t *X;

    if (!((data - (const md5_byte_t *)0) & 3)) {
	/* data are properly aligned */
	X = (const md5_word_t *)data;
    } else {
	/* not aligned */
	memcpy(xbuf, data, 64);
	X = xbuf;
    }
#endif

#define ROTATE_LEFT(x, n) (((x) << (n)) | ((x) >> (32 - (n))))

    /* Round 1. */
    /* Let [abcd k s i] denote the operation
       a = b + ((a + F(b,c,d) + X[k] + T[i]) <<< s). */
#define F(x, y, z) (((x) & (y)) | (~(x) & (z)))
#define SET(a, b, c, d, k, s, Ti)\
  t = a + F(b,c,d) + X[k] + Ti;\
  a = ROTATE_LEFT(t, s) + b
    /* Do the following 16 operations. */
    SET(a, b, c, d,  0,  7,  T1);
    SET(d, a, b, c,  1, 12,  T2);
    SET(c, d, a, b,  2, 17,  T3);
    SET(b, c, d, a,  3, 22,  T4);
    SET(a, b, c, d,  4,  7,  T5);
    SET(d, a, b, c,  5, 12,  T6);
    SET(c, d, a, b,  6, 17,  T7);
    SET(b, c, d, a,  7, 22,  T8);
    SET(a, b, c, d,  8,  7,  T9);
    SET(d, a, b, c,  9, 12, T10);
    SET(c, d, a, b, 10, 17, T11);
    SET(b, c, d, a, 11, 22, T12);
    SET(a, b, c, d, 12,  7, T13);
    SET(d, a, b, c, 13, 12, T14);
    SET(c, d, a, b, 14, 17, T15);
    SET(b, c, d, a, 15, 22, T16);
#undef SET

     /* Round 2. */
     /* Let [abcd k s i] denote the operation
          a = b + ((a + G(b,c,d) + X[k] + T[i]) <<< s). */
#define G(x, y, z) (((x) & (z)) | ((y) & ~(z)))
#define SET(a, b, c, d, k, s, Ti)\
  t = a + G(b,c,d) + X[k] + Ti;\
  a = ROTATE_LEFT(t, s) + b
     /* Do the following 16 operations. */
    SET(a, b, c, d,  1,  5, T17);
    SET(d, a, b, c,  6,  9, T18);
    SET(c, d, a, b, 11, 14, T19);
    SET(b, c, d, a,  0, 20, T20);
    SET(a, b, c, d,  5,  5, T21);
    SET(d, a, b, c, 10,  9, T22);
    SET(c, d, a, b, 15, 14, T23);
    SET(b, c, d, a,  4, 20, T24);
    SET(a, b, c, d,  9,  5, T25);
    SET(d, a, b, c, 14,  9, T26);
    SET(c, d, a, b,  3, 14, T27);
    SET(b, c, d, a,  8, 20, T28);
    SET(a, b, c, d, 13,  5, T29);
    SET(d, a, b, c,  2,  9, T30);
    SET(c, d, a, b,  7, 14, T31);
    SET(b, c, d, a, 12, 20, T32);
#undef SET

     /* Round 3. */
     /* Let [abcd k s t] denote the operation
          a = b + ((a + H(b,c,d) + X[k] + T[i]) <<< s). */
#define H(x, y, z) ((x) ^ (y) ^ (z))
#define SET(a, b, c, d, k, s, Ti)\
  t = a + H(b,c,d) + X[k] + Ti;\
  a = ROTATE_LEFT(t, s) + b
     /* Do the following 16 operations. */
    SET(a, b, c, d,  5,  4, T33);
    SET(d, a, b, c,  8, 11, T34);
    SET(c, d, a, b, 11, 16, T35);
    SET(b, c, d, a, 14, 23, T36);
    SET(a, b, c, d,  1,  4, T37);
    SET(d, a, b, c,  4, 11, T38);
    SET(c, d, a, b,  7, 16, T39);
    SET(b, c, d, a, 10, 23, T40);
    SET(a, b, c, d, 13,  4, T41);
    SET(d, a, b, c,  0, 11, T42);
    SET(c, d, a, b,  3, 16, T43);
    SET(b, c, d, a,  6, 23, T44);
    SET(a, b, c, d,  9,  4, T45);
    SET(d, a, b, c, 12, 11, T46);
    SET(c, d, a, b, 15, 16, T47);
    SET(b, c, d, a,  2, 23, T48);
#undef SET

     /* Round 4. */
     /* Let [abcd k s t] denote the operation
          a = b + ((a + I(b,c,d) + X[k] + T[i]) <<< s). */
#define I(x, y, z) ((y) ^ ((x) | ~(z)))
#define SET(a, b, c, d, k, s, Ti)\
  t = a + I(b,c,d) + X[k] + Ti;\
  a = ROTATE_LEFT(t, s) + b
     /* Do the following 16 operations. */
    SET(a, b, c, d,  0,  6, T49);
    SET(d, a, b, c,  7, 10, T50);
    SET(c, d, a, b, 14, 15, T51);
    SET(b, c, d, a,  5, 21, T52);
    SET(a, b, c, d, 12,  6, T53);
    SET(d, a, b, c,  3, 10, T54);
    SET(c, d, a, b, 10, 15, T55);
    SET(b, c, d, a,  1, 21, T56);
    SET(a, b, c, d,  8,  6, T57);
    SET(d, a, b, c, 15, 10, T58);
    SET(c, d, a, b,  6, 15, T59);
    SET(b, c, d, a, 13, 21, T60);
    SET(a, b, c, d,  4,  6, T61);
    SET(d, a, b, c, 11, 10, T62);
    SET(c, d, a, b,  2, 15, T63);
    SET(b, c, d, a,  9, 21, T64);
#undef SET

     /* Then perform the following additions. (That is increment each
        of the four registers by the value it had before this block
        was started.) */
    pms->abcd[0] += a;
    pms->abcd[1] += b;
    pms->abcd[2] += c;
    pms->abcd[3] += d;
}

void
md5_init(md5_state_t *pms)
{
    pms->count[0] = pms->count[1] = 0;
    pms->abcd[0] = 0x67452301;
    pms->abcd[1] = 0xefcdab89;
    pms->abcd[2] = 0x98badcfe;
    pms->abcd[3] = 0x10325476;
}

void
md5_append(md5_state_t *pms, const md5_byte_t *data, int nbytes)
{
    const md5_byte_t *p = data;
    int left = nbytes;
    int offset = (pms->count[0] >> 3) & 63;
    md5_word_t nbits = (md5_word_t)(nbytes << 3);

    if (nbytes <= 0)
	return;

    /* Update the message length. */
    pms->count[1] += nbytes >> 29;
    pms->count[0] += nbits;
    if (pms->count[0] < nbits)
	pms->count[1]++;

    /* Process an initial partial block. */
    if (offset) {
	int copy = (offset + nbytes > 64 ? 64 - offset : nbytes);

	memcpy(pms->buf + offset, p, copy);
	if (offset + copy < 64)
	    return;
	p += copy;
	left -= copy;
	md5_process(pms, pms->buf);
    }

    /* Process full blocks. */
    for (; left >= 64; p += 64, left -= 64)
	md5_process(pms, p);

    /* Process a final partial block. */
    if (left)
	memcpy(pms->buf, p, left);
}

void
md5_finish(md5_state_t *pms, md5_byte_t digest[16])
{
    static const md5_byte_t pad[64] = {
	0x80, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    };
    md5_byte_t data[8];
    int i;

    /* Save the length before padding. */
    for (i = 0; i < 8; ++i)
	data[i] = (md5_byte_t)(pms->count[i >> 2] >> ((i & 3) << 3));
    /* Pad to 56 bytes mod 64. */
    md5_append(pms, pad, ((55 - (pms->count[0] >> 3)) & 63) + 1);
    /* Append the length. */
    md5_append(pms, data, 8);
    for (i = 0; i < 16; ++i)
	digest[i] = (md5_byte_t)(pms->abcd[i >> 2] >> ((i & 3) << 3));
}

/**************************************************************************


                      Ocaml stubs (copied from md4_c.c)
 

***************************************************************************/

#include "md5.h"
#include "md4.h"

#include "caml/mlvalues.h"
#include "caml/fail.h"
#include "caml/alloc.h"

value md5_unsafe_string(value digest_v, value string_v, value len_v)
{
  unsigned char *digest = String_val(digest_v);
  unsigned char *string = String_val(string_v);
  long len = Long_val(len_v);
  md5_state_t context;

  md5_init (&context);
  md5_append (&context, string, len);
  md5_finish (&context, digest);
 
  return Val_unit;
}

#include <stdio.h>

value md5_unsafe_file (value digest_v, value filename_v)
{
  char *filename  = String_val(filename_v);
  unsigned char *digest = String_val(digest_v);
  FILE *file;
  md5_state_t context;
  int len;

  if ((file = fopen (filename, "rb")) == NULL)
    raise_not_found();

  else {
    md5_init (&context);
    while ((len = fread (hash_buffer, 1, HASH_BUFFER_LEN, file)))
      md5_append (&context, hash_buffer, len);
    md5_finish (&context, digest);

    fclose (file);
  }
  return Val_unit;
}

/**************************************************************************

                      Crypt function

***************************************************************************/


#include <string.h>
#include <stdlib.h>

#define MIN(x,y) ((y) > (x)) ? (x) : (y)
#define MAX(x,y) ((y) > (x)) ? (y) : (x)

/* Define our magic string to mark salt for MD5 "encryption"
   replacement.  This is meant to be the same as for other MD5 based
   encryption implementations.  */
static const char md5_salt_prefix[] = "$1$";

/* Table with characters for base64 transformation.  */
static const char b64t[64] =
"./0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

void yahoo_crypt(char *key, char *salt, char *buffer)
{
  int needed = 3 + strlen (salt) + 1 + 26 + 1;
  int buflen;  
  md5_byte_t alt_result[16];
  md5_state_t ctx;
  md5_state_t alt_ctx;
  size_t salt_len;
  size_t key_len;
  size_t cnt;
  char *cp;
  
  buflen = needed;

	/* Find beginning of salt string.  The prefix should normally always
	   be present.  Just in case it is not.  */
  if (strncmp (md5_salt_prefix, salt, sizeof (md5_salt_prefix) - 1) == 0)
		/* Skip salt prefix.  */
    salt += sizeof (md5_salt_prefix) - 1;
  
  salt_len = MIN (strcspn (salt, "$"), 8);
  key_len = strlen (key);

	/* Prepare for the real work.  */
  md5_init(&ctx);

	/* Add the key string.  */
  md5_append(&ctx, key, key_len);

	/* Because the SALT argument need not always have the salt prefix we
	   add it separately.  */
  md5_append(&ctx, md5_salt_prefix, sizeof (md5_salt_prefix) - 1);

	/* The last part is the salt string.  This must be at most 8
	   characters and it ends at the first `$' character (for
	   compatibility which existing solutions).  */
  md5_append(&ctx, salt, salt_len);

	/* Compute alternate MD5 sum with input KEY, SALT, and KEY.  The
	   final result will be added to the first context.  */
  md5_init(&alt_ctx);

	/* Add key.  */
  md5_append(&alt_ctx, key, key_len);

	/* Add salt.  */
  md5_append(&alt_ctx, salt, salt_len);

	/* Add key again.  */
  md5_append(&alt_ctx, key, key_len);

	/* Now get result of this (16 bytes) and add it to the other
	   context.  */
  md5_finish(&alt_ctx, alt_result);

	/* Add for any character in the key one byte of the alternate sum.  */
  for (cnt = key_len; cnt > 16; cnt -= 16)
    md5_append(&ctx, alt_result, 16);
  md5_append(&ctx, alt_result, cnt);

	/* For the following code we need a NUL byte.  */
  alt_result[0] = '\0';

	/* The original implementation now does something weird: for every 1
	   bit in the key the first 0 is added to the buffer, for every 0
	   bit the first character of the key.  This does not seem to be
	   what was intended but we have to follow this to be compatible.  */
  for (cnt = key_len; cnt > 0; cnt >>= 1)
    md5_append(&ctx, (cnt & 1) != 0 ? alt_result : (md5_byte_t *)key, 1);

	/* Create intermediate result.  */
  md5_finish(&ctx, alt_result);

	/* Now comes another weirdness.  In fear of password crackers here
	   comes a quite long loop which just processes the output of the
	   previous round again.  We cannot ignore this here.  */
  for (cnt = 0; cnt < 1000; ++cnt) {
		/* New context.  */
    md5_init(&ctx);
    
    /* Add key or last result.  */
    if ((cnt & 1) != 0)
      md5_append(&ctx, key, key_len);
    else
      md5_append(&ctx, alt_result, 16);

		/* Add salt for numbers not divisible by 3.  */
    if (cnt % 3 != 0)
      md5_append(&ctx, salt, salt_len);

		/* Add key for numbers not divisible by 7.  */
    if (cnt % 7 != 0)
      md5_append(&ctx, key, key_len);

		/* Add key or last result.  */
    if ((cnt & 1) != 0)
      md5_append(&ctx, alt_result, 16);
    else
      md5_append(&ctx, key, key_len);

		/* Create intermediate result.  */
    md5_finish(&ctx, alt_result);
  }

	/* Now we can construct the result string.  It consists of three
	   parts.  */
  
  strncpy(buffer, md5_salt_prefix, MAX (0, buflen));
  cp = buffer + strlen(buffer);
  buflen -= sizeof (md5_salt_prefix);
  
  strncpy(cp, salt, MIN ((size_t) buflen, salt_len));
  cp = cp + strlen(cp);
  buflen -= MIN ((size_t) buflen, salt_len);
  
  if (buflen > 0) {
    *cp++ = '$';
    --buflen;
  }
  
  #define b64_from_24bit(B2, B1, B0, N) \
	do { \
		unsigned int w = ((B2) << 16) | ((B1) << 8) | (B0); \
		int n = (N); \
		while (n-- > 0 && buflen > 0) { \
			*cp++ = b64t[w & 0x3f]; \
			--buflen; \
			w >>= 6; \
		}\
	} while (0)
    
    b64_from_24bit (alt_result[0], alt_result[6], alt_result[12], 4);
  b64_from_24bit (alt_result[1], alt_result[7], alt_result[13], 4);
  b64_from_24bit (alt_result[2], alt_result[8], alt_result[14], 4);
  b64_from_24bit (alt_result[3], alt_result[9], alt_result[15], 4);
  b64_from_24bit (alt_result[4], alt_result[10], alt_result[5], 4);
  b64_from_24bit (0, 0, alt_result[11], 2);
  *cp = '\0';	/* Terminate the string.  */

	/* Clear the buffer for the intermediate result so that people
	   attaching to processes or reading core dumps cannot get any
	   information.  We do it in this way to clear correct_words[]
	   inside the MD5 implementation as well.  */
  md5_init(&ctx);
  md5_finish(&ctx, alt_result);
  memset (&ctx, '\0', sizeof (ctx));
  memset (&alt_ctx, '\0', sizeof (alt_ctx));
  
}

value ml_yahoo_crypt(value key_v, value salt_v, value buffer_v)
{
  yahoo_crypt(String_val(key_v), String_val(salt_v), String_val(buffer_v));

  return Val_unit;
}
