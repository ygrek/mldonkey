/*
 *
 * Copyright (C) 2003 giFT-FastTrack project
 * http://developer.berlios.de/projects/gift-fasttrack
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 */

#ifndef _CRYPT_H_
#define _CRYPT_H_




#define FST_DBG(fmt)
#define FST_DBG_1(fmt,a)
#define FST_DBG_2(fmt,a,b)
#define FST_DBG_3(fmt,a,b,c)
#define FST_DBG_4(fmt,a,b,c,d)
#define FST_DBG_5(fmt,a,b,c,d,e)

# define FST_HEAVY_DBG(fmt)
# define FST_HEAVY_DBG_1(fmt,a)
# define FST_HEAVY_DBG_2(fmt,a,b)
# define FST_HEAVY_DBG_3(fmt,a,b,c)
# define FST_HEAVY_DBG_4(fmt,a,b,c,d)
# define FST_HEAVY_DBG_5(fmt,a,b,c,d,e)

#define FST_WARN(fmt)
#define FST_WARN_1(fmt,a)
#define FST_WARN_2(fmt,a,b)
#define FST_WARN_3(fmt,a,b,c)

#define FST_ERR(fmt)
#define FST_ERR_1(fmt,a)
#define FST_ERR_2(fmt,a,b)
#define FST_ERR_3(fmt,a,b,c)

#define TRUE 1
#define FALSE 0

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*****************************************************************************/

typedef struct
{
	int pos;
	unsigned int wrapcount;
	unsigned int add_to_lookup;
	unsigned int enc_type;
	unsigned char pad[63];
	unsigned char lookup[256];
	unsigned int pad16[20];
	unsigned int seed;
} FSTCipher;

/*****************************************************************************/

/* allocate and init cipher */
FSTCipher *fst_cipher_create();

/* free cipher */
void fst_cipher_free (FSTCipher *cipher);

/* initialize cipher state */
/* returns FALSE if enc_type is not supported, TRUE otherwise */
int fst_cipher_init (FSTCipher *cipher, unsigned int seed, unsigned int enc_type);

/* encrypt / decrypt a block of data with cipher */
void fst_cipher_crypt (FSTCipher *cipher, unsigned char *data, int len);

/*****************************************************************************/

/* returns encrypted enc_type */
unsigned int fst_cipher_encode_enc_type (unsigned int seed, unsigned int enc_type);

/* returns decrypted enc_type */
unsigned int fst_cipher_decode_enc_type (unsigned int seed, unsigned int crypted_enc_type);

/*****************************************************************************/

#endif
