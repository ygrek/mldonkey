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

/* encrypt / decrypt a byte of data with cipher */
unsigned char fst_cipher_clock (FSTCipher *cipher);

/* encrypt / decrypt a block of data with cipher */
void fst_cipher_crypt (FSTCipher *cipher, unsigned char *data, int len);

/*****************************************************************************/

/* returns encrypted or decrypted enc_type */
unsigned int fst_cipher_mangle_enc_type (unsigned int seed, unsigned int enc_type);

/*****************************************************************************/

#endif
