/*
 * Copyright (C) 2003 Markus Kern (mkern@users.sourceforge.net)
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

#define FST_HEAVY_DBG(x)
#define FST_DBG(x)
#define FST_DBG_2(x,y,z)

/*****************************************************************************/

typedef struct
{
	int pos;
	unsigned int wrapcount;
	unsigned int add_to_lookup;
	unsigned int enc_type;
	unsigned char pad[63];
	unsigned char lookup[256];
	unsigned int seed;
	
	unsigned char buf[80];	// used in clock_cipher?
} FSTCipher;

/*****************************************************************************/

// allocate and init cipher
FSTCipher *fst_cipher_create();

// free cipher
void fst_cipher_free(FSTCipher *cipher);

// initialize cipher state
void fst_cipher_init(FSTCipher *cipher, unsigned int seed, unsigned int enc_type);

// encrypt / decrypt a block of data with cipher
void fst_cipher_crypt(FSTCipher *cipher, unsigned char *data, int len);

/*****************************************************************************/

// returns encrypted enc_type
unsigned int fst_cipher_encode_enc_type(unsigned int seed, unsigned int enc_type);

// returns decrypted enc_type
unsigned int fst_cipher_decode_enc_type(unsigned int seed, unsigned int crypted_enc_type);

/*****************************************************************************/

#endif
