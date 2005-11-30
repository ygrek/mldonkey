/*
 ---------------------------------------------------------------------------
 Copyright (c) 2002, Dr Brian Gladman, Worcester, UK.   All rights reserved.

 LICENSE TERMS

 The free distribution and use of this software in both source and binary
 form is allowed (with or without changes) provided that:

   1. distributions of this source code include the above copyright
      notice, this list of conditions and the following disclaimer;

   2. distributions in binary form include the above copyright
      notice, this list of conditions and the following disclaimer
      in the documentation and/or other associated materials;

   3. the copyright holder's name is not used to endorse products
      built using this software without specific written permission.

 ALTERNATIVELY, provided that this notice is retained in full, this product
 may be distributed under the terms of the GNU General Public License (GPL),
 in which case the provisions of the GPL apply INSTEAD OF those given above.

 DISCLAIMER

 This software is provided 'as is' with no explicit or implied warranties
 in respect of its properties, including, but not limited to, correctness
 and/or fitness for purpose.
 ---------------------------------------------------------------------------
 Issue Date: 26/08/2003
*/

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

/*swap order of arguments in sha1_hash and sha1_end to 'fit' mldonkey*/

#ifndef _SHA1_H
#define _SHA1_H

#include <limits.h>

#define SHA1_BLOCK_SIZE  64
#define SHA1_DIGEST_SIZE 20

#if defined(__cplusplus)
extern "C"
{
#endif

/* define an unsigned 32-bit type */

#if defined(_MSC_VER)
  typedef   unsigned long    sha1_32t;
#elif defined(ULONG_MAX) && ULONG_MAX == 0xfffffffful
  typedef   unsigned long    sha1_32t;
#elif defined(UINT_MAX) && UINT_MAX == 0xffffffff
  typedef   unsigned int     sha1_32t;
#else
#  error Please define sha1_32t as an unsigned 32 bit type in sha1.h
#endif


/* type to hold the SHA256 context  */

typedef struct sha1_context
{   sha1_32t count[2];
    sha1_32t hash[5];
    sha1_32t wbuf[16];
} sha1_context;

typedef struct sha1_context SHA1_CTX;

/* Note that these prototypes are the same for both bit and */
/* byte oriented implementations. However the length fields */
/* are in bytes or bits as appropriate for the version used */
/* and bit sequences are input as arrays of bytes in which  */
/* bit sequences run from the most to the least significant */
/* end of each byte                                         */

void sha1_compile(sha1_context ctx[1]);

/*
void sha1_begin(sha1_ctx ctx[1]);
void sha1_hash(sha1_ctx ctx[1], const unsigned char data[], unsigned long len);
void sha1_end(sha1_ctx ctx[1], unsigned char hval[]);
*/

void sha1(unsigned char hval[], const unsigned char data[], unsigned long len);

int sha1_begin(SHA1_CTX*);
int sha1_hash(SHA1_CTX*, const unsigned char [], unsigned long);
int sha1_end(SHA1_CTX*, unsigned char hval[]);

#if defined(__cplusplus)
}
#endif

#endif
