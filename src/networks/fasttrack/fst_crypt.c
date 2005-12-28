/*
 *
 * Copyright (C) 2003 giFT-FastTrack project
 * Portions Copyright (C) 2001 Shtirlitz <shtirlitz@unixwarez.net>
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

#include "fst_crypt.h"
#include "../../utils/lib/md5.h"

#define FALSE 0
#define TRUE 1

#define FST_WARN(fmt)
#define FST_WARN_1(fmt,a)
#define FST_WARN_2(fmt,a,b)
#define FST_WARN_3(fmt,a,b,c)

#define FST_ERR(fmt)
#define FST_ERR_1(fmt,a)
#define FST_ERR_2(fmt,a,b)
#define FST_ERR_3(fmt,a,b,c)

#define FST_DBG(fmt)
#define FST_DBG_1(fmt,a)
#define FST_DBG_2(fmt,a,b)
#define FST_DBG_3(fmt,a,b,c)
#define FST_DBG_4(fmt,a,b,c,d)
#define FST_DBG_5(fmt,a,b,c,d,e)

#define FST_HEAVY_DBG(fmt)
#define FST_HEAVY_DBG_1(fmt,a)
#define FST_HEAVY_DBG_2(fmt,a,b)
#define FST_HEAVY_DBG_3(fmt,a,b,c)
#define FST_HEAVY_DBG_4(fmt,a,b,c,d)
#define FST_HEAVY_DBG_5(fmt,a,b,c,d,e)

/*****************************************************************************/

/* crypt functions */
void enc_type_1 (unsigned char *out_key, unsigned char *in_key);
void enc_type_2 (unsigned int *key, unsigned int seed);
void enc_type_20 (unsigned int *key, unsigned int seed);
void enc_type_80 (unsigned int *key, unsigned int seed);

/*****************************************************************************/

static int pad_init (unsigned int *pseed, unsigned int enc_type, unsigned char* pad, unsigned int pad_size);
static int calculate_num (unsigned int *num, int val);
static unsigned int seed_step (unsigned int seed);
static int qsort_cmp_func (const void *ap, const void *bp);
static void reverse_bytes (unsigned int *buf, unsigned int longs);

/*****************************************************************************/

/* allocate and init cipher */

FSTCipher *fst_cipher_create()
{
	FSTCipher *cipher;

	cipher = malloc (sizeof(FSTCipher));
	memset (cipher, 0, sizeof(FSTCipher));

	return cipher;
}

/* free cipher */

void fst_cipher_free (FSTCipher *cipher)
{
	if (cipher)
		free (cipher);
}

/* encrypt / decrypt a block of data with cipher */

void fst_cipher_crypt (FSTCipher *cipher, unsigned char *data, int len)
{
	for ( ; len > 0; len--, data++)
	{
		*data ^= fst_cipher_clock (cipher); 
	}
}

/* initialize cipher state */
/* returns FALSE if enc_type is not supported, TRUE otherwise */

int fst_cipher_init (FSTCipher *cipher, unsigned int seed, unsigned int enc_type)
{
	unsigned int j, i, temp, sortpos;
	unsigned char c;

	cipher->enc_type = enc_type;
	cipher->wrapcount = 0;
	cipher->add_to_lookup = 0;
	cipher->seed = seed;

	FST_HEAVY_DBG_2 ("init_cipher: seed = 0x%08x, enc_type = 0x%02x", seed, enc_type);

	if (!pad_init (&seed, enc_type, cipher->pad, sizeof (cipher->pad)))
		return FALSE;

	/* adjust pad */
	c = 0;
	for (i = 0; i < sizeof (cipher->pad); i++)
		c = c | cipher->pad[i];
	if (!(c & 1))
		cipher->pad[0] = cipher->pad[0] | 0x71;

	/* init cipher->pos */
	temp = seed_step (seed);
	temp = temp >> 16;
	cipher->pos = ( (temp << 6) - temp) >> 16;

	/* init cipher->lookup */
	for(i = 0; i <sizeof (cipher->lookup); i++)
		cipher->lookup[i] = (unsigned char)i;

	if (enc_type & 0x08)
	{
		ml_MD5Context ctx;
		unsigned char md5[ml_MD5_HASH_LEN];

		FST_HEAVY_DBG ("init_cipher: enc_type & 0x08");

		ml_MD5Init (&ctx);
		ml_MD5Update (&ctx, cipher->pad, sizeof(cipher->pad));
		ml_MD5Final (md5, &ctx);

		/* correct md5 byte order on big-endian since it's converted to (unsigned int*) below */
		reverse_bytes ( (unsigned int*)&md5, 4);

		/* modify cipher->lookup */
		for (i = 0; i < sizeof (cipher->lookup); i++)
		{
			if ( (j = calculate_num( (unsigned int*) &md5, 0x100 - i) + i) != i)
			{
				unsigned char a = cipher->lookup[j];
				unsigned char b = cipher->lookup[i];
				cipher->lookup[i] = a;
				cipher->lookup[j] = b;
			}
		}
	}


	if(enc_type & 0x10)
	{
		FST_HEAVY_DBG ("init_cipher: enc_type & 0x10");

		for (seed = cipher->pos, i=0; i < 20; i++)
		{
			seed = seed_step (seed);
			cipher->pad16[i] = seed;
		}		
		
		seed = seed_step (seed);

		/* CHECKME: endianess? */
		enc_type_2 (cipher->pad16, seed);
	}


	/* sort cipher->pad */
	sortpos = ( (cipher->pos * cipher->pos) + 2) % (sizeof(cipher->pad)-4);
	qsort (cipher->pad + sortpos, 5, 1, qsort_cmp_func);

	/* modify every third byte of cipher->pad */
	for (i = 5; i < sizeof (cipher->pad); i += 3) 
	{
		c = cipher->pad[i];
		c = ~c + i;
		cipher->pad[i] = c | 1;
	}

//	print_bin_data(cipher->pad, sizeof(cipher->pad));
//	print_bin_data(cipher->lookup, sizeof(cipher->lookup));

	return TRUE;
}

/*****************************************************************************/

/* returns encrypted or decrypted enc_type */
unsigned int fst_cipher_mangle_enc_type (unsigned int seed, unsigned int enc_type)
{
	unsigned int key_80[20];
	int i;

	for (i = 0; i < 20; i++)
	{
		seed = seed_step (seed);
		key_80[i] = seed;
	}

	seed = seed_step (seed);
	enc_type_2 (key_80, seed);

	return enc_type ^ key_80[7];
}

/*****************************************************************************/

static int pad_init (unsigned int *pseed, unsigned int enc_type, unsigned char* pad, unsigned int pad_size)
{
	unsigned int temp, i, seed = *pseed;
	
	memset (pad, 0, pad_size);

	if ( (enc_type & 1) || !(enc_type & 0x1E7))
	{
		unsigned char key_256_in[256];
		unsigned char key_256_out[256];

		FST_HEAVY_DBG ("pad_init: enc_type & 1");

		for (i = 0; i < 0xFF; i++)
		{
			seed = seed_step (seed);
			temp = seed >> 0x11;
			key_256_in[i] = (unsigned char) (temp % 0xE0);
		}

		enc_type_1 (key_256_out, key_256_in);

		/* merge with pad */
		for (i = 0; i < pad_size; i++)
			pad[i] ^= key_256_out[i];

	}

	if (enc_type & 0x1E6)
	{
		unsigned int key_80[20];

		FST_HEAVY_DBG ("pad_init: enc_type & 0x1E6");

		for(i = 0; i < 20; i++)
		{
			seed = seed_step (seed);
			key_80[i] = seed;
		}

		if (enc_type & 0x02)
		{
			FST_HEAVY_DBG ("pad_init: enc_type & 0x02");

			seed = seed_step (seed);
			enc_type_2 (key_80, seed);
		}

		if (enc_type & 0x04)
		{
//			FST_WARN ("pad_init: enc_type & 0x04 not implemented");

			seed = seed_step (seed);
			return FALSE;
		}

		if (enc_type & 0x20)
		{
			FST_HEAVY_DBG ("pad_init: enc_type & 0x20");

			seed = seed_step (seed);
			enc_type_20 (key_80, seed);
		}

		if (enc_type & 0x80)
		{
			FST_HEAVY_DBG ("pad_init: enc_type & 0x80");

			seed = seed_step (seed);
			enc_type_80 (key_80, seed);
		}

		if (enc_type & 0x100)
		{
//			FST_WARN ("pad_init: enc_type & 0x100 not implemented");

			seed = seed_step (seed);
			return FALSE;
		}

		/* correct byte order on big-endian before merging */
		reverse_bytes (key_80, 20);

		/* merge with pad */
		for (i = 0; i < pad_size; i++)
			pad[i] ^= ( (unsigned char*)key_80)[i];
	}

	*pseed = seed;

	return TRUE;
}

/*
 * Update the state of the cipher, and output the keystream byte which
 * will be XOR'ed with the plaintext to produce the ciphertext, or with
 * the ciphertext to produce the plaintext.
 */
unsigned char fst_cipher_clock (FSTCipher *cipher)
{
	unsigned char xor;
	unsigned char temp;

	/* Get the previous position of the state, wrapping around if
	 * necessary. */
	unsigned char lastpos = (cipher->pos > 0 ? cipher->pos-1 : sizeof(cipher->pad)-1);

	/* Add the value at the previous position to the one at the current
	 * position.  This creates a "running total". */
	cipher->pad[cipher->pos] += cipher->pad[lastpos];

	/* We're going to output the current value of the "running total". */
	xor = cipher->pad[cipher->pos];

	/* But before we output it, see if we should mangle the internal
	 * state a bit.  We do this with probability 1/8 (the high three
	 * bits of the xor byte are all 0), every PADSIZE output bytes.
	 * The mangling involves two steps:
	 * - Sort a particular 5 bytes of the state according to the above
	 *   rekey_cmp comparison function.
	 * - Modify the value of every third entry (starting with entry 5)
	 */
	if (cipher->pos == 7 && ((xor & 0x70) == 0))
	{
		unsigned int i;
		/* Which 5 elements should we sort?  We calculate this in a
		 * pretty odd manner. */
		int sortpos = xor + cipher->pad[2];
		sortpos = ( (sortpos * sortpos) + 2) % (sizeof (cipher->pad) - 4);

		/* Sort those 5 elements according to the qsort_cmp_func comparison
		* function. */
		qsort (cipher->pad + sortpos, 5, 1, qsort_cmp_func);
		
		/* Now modify every third byte of the state in a simple way. */
		for (i = 5; i < sizeof (cipher->pad); i += 3) 
		{
			unsigned char val = cipher->pad[i];
			val = ~val + i;
			cipher->pad[i] = val|1;
		}
	}	

	cipher->pos++;

	/* Increment the current position of the state, wrapping around if
	 * necessary. */
	if (cipher->pos == 0x3f) 
	{
		cipher->pos = 0;
		cipher->wrapcount++;

		if (cipher->enc_type & 0x08)
		{
			cipher->add_to_lookup++;
		}

		if (cipher->enc_type & 0x10)
		{
			int i;
			unsigned int val;
			unsigned char shift_factor = cipher->pad[0x18] & 0x0f;
			unsigned char pad_offset = cipher->pad[0x19] & 0x1f;

			for (i = 0; i < 6; i++)
			{
				val = cipher->pad16[i] >> shift_factor;
				cipher->pad[pad_offset + i] ^= (unsigned char)val;
			}

			cipher->pad[pad_offset + 4] |= (unsigned char)1 << (cipher->pad[0x0A] & 7);

			if ( (cipher->wrapcount & 15) == 0)
			{
				unsigned int seed = cipher->wrapcount;

				for (i = 0; i < 20; i++)
				{
					seed = seed_step (seed);
					cipher->pad16[i] = seed;
				}			
				seed = seed_step (seed);
				/* CHECKME: endianess? */
				enc_type_2 (cipher->pad16, seed);
			}
		}
	}

	temp = cipher->add_to_lookup + xor;
	xor = cipher->lookup[temp];
	return xor;
}


static int calculate_num (unsigned int *num, int val)
{
	if (!(val > 0x10001))
	{
		*num = seed_step(*num);

		return ((int) (*num >> 16) * val) >> 16;
	}
	else
	{
		FST_WARN ("calculate_num: (val <= 0x10001) not implemented");
	}

	if (val > 0xff)
	{
		FST_WARN ("calculate_num: (val > 0xff) not implemented");
	}
	return val;
}


static unsigned int seed_step (unsigned int seed)
{
	return 0x10dcd * seed + 0x4271;
}


/*
 * The comparison routine used in the "sort" portion of the state update
 * function (see below).  It's pretty much just an unsigned comparison
 * of bytes, except that bit 5 of each byte is inverted before
 * comparison for some reason.
 */

static int qsort_cmp_func (const void *ap, const void *bp)
{
    int a = (int)*(unsigned char *)ap;
    int b = (int)*(unsigned char *)bp;

    return ( (a^0x20) - (b^0x20));
/*
	int a = ( (int)*(unsigned char*)ap) << 1;
	int b = ( (int)*(unsigned char*)bp) << 1;

	return (a^0x41) - (b^0x41);
*/
}

/*
 * simple byte reversal function for endianess correction
 * this is a noop on little-endian
 */

static void reverse_bytes (unsigned int *buf, unsigned int longs)
{
	unsigned char *cbuf = (unsigned char*)buf;

	for ( ; longs; longs--, buf++, cbuf += 4)
	{
		*buf = ( (unsigned int) cbuf[3] << 8 | cbuf[2]) << 16 |
			   ( (unsigned int) cbuf[1] << 8 | cbuf[0]);
	}
}
