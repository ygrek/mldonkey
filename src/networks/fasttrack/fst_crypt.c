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

// #include "fst_fasttrack.h"

#include "../../utils/lib/md5.h"
#include "fst_crypt.h"

/*****************************************************************************/

// crypt functions
void enc_type_1 (unsigned char *out_key, unsigned char *in_key);
void enc_type_2 (unsigned char *key, unsigned int seed);
void enc_type_20 (unsigned char *key, unsigned int seed);


/*****************************************************************************/

static int pad_init(unsigned int seed, unsigned int enc_type, unsigned char* pad, unsigned int pad_size);
static unsigned char clock_cipher(FSTCipher *cipher);
static unsigned int calculate_num_xor(unsigned int seed);
static int calculate_num(unsigned int *num, int val);
static unsigned int seed_step(unsigned int seed);
static int qsort_cmp_func(const void *ap, const void *bp);

/*****************************************************************************/

// allocate and init cipher
FSTCipher *fst_cipher_create()
{
	FSTCipher *cipher;

	cipher = malloc(sizeof(FSTCipher));
	memset(cipher, 0, sizeof(FSTCipher));

	return cipher;
}

// free cipher
void fst_cipher_free(FSTCipher *cipher)
{
	if(cipher)
		free(cipher);
}

// encrypt / decrypt a block of data with cipher
void fst_cipher_crypt(FSTCipher *cipher, unsigned char *data, int len)
{
	for(; len>0; len--, data++)
	{
		*data ^= clock_cipher(cipher); 
    }
}

// initialize cipher state
void fst_cipher_init(FSTCipher *cipher, unsigned int seed, unsigned int enc_type)
{
	int i,j;
	unsigned int temp;
	unsigned int sortpos;
	unsigned char c;

	cipher->enc_type = enc_type;
	cipher->wrapcount = 0;
	cipher->add_to_lookup = 0;
	cipher->seed = seed;

	FST_DBG_2 ("init_cipher: seed = 0x%08x, enc_type = 0x%02x", seed, enc_type);

	seed = pad_init(seed, enc_type, cipher->pad, sizeof(cipher->pad));

	// adjust pad
	c = 0;
	for(i=0; i<sizeof(cipher->pad); i++)
		c = c | cipher->pad[i];
	if(!(c & 1))
		cipher->pad[0] = cipher->pad[0] | 0x71;

	// init cipher->pos
	temp = seed_step(seed);
	temp = temp >> 16;
	cipher->pos = ((temp << 6) - temp) >> 16;

	// init cipher->lookup
	for(i=0; i<sizeof(cipher->lookup); i++)
		cipher->lookup[i] = (unsigned char)i;


	if(enc_type & 0x08)
	{
		MD5Context ctx;
		unsigned char md5[MD5_HASH_LEN];

		FST_HEAVY_DBG ("init_cipher: enc_type & 0x08");

		MD5Init(&ctx);
		MD5Update(&ctx, cipher->pad, sizeof(cipher->pad));
		MD5Final(md5, &ctx);

		// modify cipher->lookup
		for(i=0; i<sizeof(cipher->lookup); i++)
		{

		  /* BIG ENDIAN */
			if( (j = calculate_num((unsigned int*) &md5, 0x100 - i) + i) != i)
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
		FST_DBG ("init_cipher: enc_type & 0x10, WARNING: not implemented");
	}


	// sort cipher->pad
	sortpos = ((cipher->pos * cipher->pos) + 2) % (sizeof(cipher->pad)-4);
	qsort(cipher->pad + sortpos, 5, 1, qsort_cmp_func);
	
	// modify every third byte of cipher->pad
	for(i=5; i<sizeof(cipher->pad); i+=3) 
	{
		c = cipher->pad[i];
		c = ~c + i;
		cipher->pad[i] = c | 1;
	}

	//print_bin_data(cipher->lookup, sizeof(cipher->lookup));
}

/*****************************************************************************/

// returns encrypted enc_type
unsigned int fst_cipher_encode_enc_type(unsigned int seed, unsigned int enc_type)
{
	return enc_type ^ calculate_num_xor(seed);
}

// returns decrypted enc_type
unsigned int fst_cipher_decode_enc_type(unsigned int seed, unsigned int crypted_enc_type)
{
	return crypted_enc_type ^ calculate_num_xor(seed);
}

/*****************************************************************************/

static int pad_init(unsigned int seed, unsigned int enc_type, unsigned char* pad, unsigned int pad_size)
{
	int i;
	unsigned int temp;
	
	memset(pad, 0, pad_size);

	if((enc_type & 1) || !(enc_type & 0x1E7))
	{
		unsigned char key_256[256];
		unsigned char key3_256[256];
//		unsigned int *p_table = cipher_table_1;
//		unsigned char *p_key = key_256;
//		unsigned char *p_key3 = key3_256;

		FST_HEAVY_DBG ("pad_init: enc_type & 1");

		for(i=0; i<0xFF; i++)
		{
			seed = seed_step(seed);
			temp = seed >> 0x11;
			key_256[i] = (unsigned char) (temp % 0xE0);
		}
/*
#ifdef WIN32
		__asm push p_key
		__asm push p_key3
		__asm mov ecx, p_table
		__asm call enc_type_1
#else

		__asm__(
			"push %0\n
			push %2\n
			mov %1,%%ecx\n
			call enc_type_1"
			:
			:"g"(p_key), "g"(p_table), "g"(p_key3));
#endif
*/
		enc_type_1 (key3_256, key_256);

		// merge with pad
		for(i=0; i<pad_size; i++)
//			pad[i] ^= p_key3[i];
			pad[i] ^= key3_256[i];

	}

	if(enc_type & 0x1E6)
	{
		  /* BIG ENDIAN */
		unsigned int key_80[20];
		unsigned char *p_key = (unsigned char*)key_80;

		FST_HEAVY_DBG ("pad_init: enc_type & 0x1E6");

		for(i=0; i<20; i++)
		{
			seed = seed_step(seed);
			key_80[i] = seed;
		}

		if(enc_type & 0x02)
		{
			FST_HEAVY_DBG ("pad_init: enc_type & 0x02");
			seed = seed_step(seed);

			enc_type_2 (p_key, seed);
		}

		if(enc_type & 0x04)
		{
			FST_DBG ("pad_init: enc_type & 0x04, WARNING: not implemented");
			seed = seed_step(seed);

/*
#ifdef WIN32
			__asm mov ecx, p_key
			__asm mov edx, seed
			__asm call enc_type_4
#else
			__asm__(
				"mov %0,%%ecx\n
				mov %1,%%edx\n
				call enc_type_4"
				:
				:"g"(p_key), "g"(seed));
#endif
*/
		}

		if(enc_type & 0x20)
		{
			FST_HEAVY_DBG ("pad_init: enc_type & 0x20");
			seed = seed_step(seed);

			enc_type_20 (p_key, seed);
		}

		if(enc_type & 0x80)
		{
			FST_DBG ("pad_init: enc_type & 0x80, WARNING: not implemented");
			seed = seed_step(seed);

/*
#ifdef WIN32
			__asm mov ecx, p_key
			__asm mov edx, seed
			__asm call enc_type_80
#else
			__asm__(
				"mov %0,%%ecx\n
				mov %1,%%edx\n
				call enc_type_80"
				:
				:"g"(p_key), "g"(seed));
#endif
*/
		}

		if(enc_type & 0x100)
		{
			FST_DBG ("pad_init: enc_type & 0x100, WARNING: not implemented");
			seed = seed_step(seed);

/*
#ifdef WIN32
			__asm mov ecx, p_key
			__asm mov edx, seed
			__asm call enc_type_100
#else
			__asm__(
				"mov %0,%%ecx\n
				mov %1,%%edx\n
				call enc_type_100"
				:
				:"g"(p_key), "g"(seed));
#endif
*/
		}

		// merge with pad
		for(i=0; i<pad_size; i++)
			pad[i] ^= p_key[i];

	}
	return seed;
}

/**
 * Update the state of the cipher, and output the keystream byte which
 * will be XOR'ed with the plaintext to produce the ciphertext, or with
 * the ciphertext to produce the plaintext.
 */
static unsigned char clock_cipher(FSTCipher *cipher)
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
		int i;
		/* Which 5 elements should we sort?  We calculate this in a
		* pretty odd manner. */
		int sortpos = xor + cipher->pad[2];
		sortpos = ((sortpos * sortpos) + 2) % (sizeof(cipher->pad)-4);

		FST_HEAVY_DBG ("clock_cipher: sorting pad");
		
		/* Sort those 5 elements according to the qsort_cmp_func comparison
		* function. */
		qsort(cipher->pad + sortpos, 5, 1, qsort_cmp_func);
		
		/* Now modify every third byte of the state in a simple way. */
		for(i=5; i<sizeof(cipher->pad); i+=3) 
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

/*
		if (cipher->enc_type & 0x10)
		{
			unsigned int val;
			char i;
			unsigned char temp2;
			char pointer = cipher->pad[0x20] & 0x1f;
			temp = cipher->pad[0x1f] & 0x0f;

			FST_DBG ("clock_cipher: check me 1");

			for (i = 0; i < 6; i++)
			{
				val = ((unsigned int *)(cipher->buf))[i];
				val = val >> temp;
				temp2 = cipher->pad[i + pointer];
				temp2 = temp2 ^ (unsigned char)val;
				cipher->pad[i+pointer] = temp2;
			}
			temp = cipher->pad[0x0A] & 7;
			temp2 = cipher->pad[pointer + 4];
			temp = 1 << temp;
			temp2 = temp2 | temp;
			cipher->pad[pointer + 4] = temp2;

			if (!(cipher->wrapcount & 15))
			{
				unsigned int seed = cipher->wrapcount;
				unsigned char *p_key = cipher->buf;

				FST_DBG ("clock_cipher: improve me 2");
				for(i=0; i<20; i++)
				{
					seed = seed_step(seed);
					((unsigned int *)cipher->buf)[i] = seed;
				}
				
				seed = seed_step(seed);

				enc_type_2 (p_key, seed);
			}
			// recalculate
		}
*/
	}
	temp = cipher->add_to_lookup + xor;
	xor = cipher->lookup[temp];
	return xor;
}

static unsigned int calculate_num_xor(unsigned int seed)
{
	unsigned int key_80[20];
	int i;

	for(i=0; i<20; i++)
	{
		seed = seed_step(seed);
		key_80[i] = seed;
	}

	seed = seed_step(seed);
	enc_type_2 ((unsigned char *)key_80, seed);

	return key_80[7];
}


static int calculate_num(unsigned int *num, int val)
{
	unsigned int temp = *num;
	int	temp2;

	if (!(val > 0x10001))
	{
		temp = temp * 5;
		temp = temp << 8;
		temp = temp - (*num);
		temp = temp * 9;
		temp = (*num) + temp * 2;
		temp = temp * 3 + 0x4271;
		*num = temp;
		temp = temp >> 16;
		temp2 = (int) temp;
		temp2 = temp2 * val;
		temp2 = temp2 >> 16;
		return temp2;
	}
	else
	{
		FST_DBG ("calculate_num: (val <= 0x10001), WARNING: not implemented");
	}

	if (val > 0xff)
	{
		FST_DBG ("calculate_num: (val > 0xff), WARNING: not implemented");
	}
	return val;
}


static unsigned int seed_step(unsigned int seed)
{
	unsigned int temp;

	temp = seed * 5;
	temp = temp << 8;
	temp = temp - seed;
	temp = temp * 9;
	seed = seed + temp * 2;
	seed = seed * 3 + 0x4271;
	
	return seed;
}


/**
 * The comparison routine used in the "sort" portion of the state update
 * function (see below).  It's pretty much just an unsigned comparison
 * of bytes, except that bit 5 of each byte is inverted before
 * comparison for some reason.
 */
static int qsort_cmp_func(const void *ap, const void *bp)
{

    int a = (int)*(unsigned char *)ap;
    int b = (int)*(unsigned char *)bp;

    return ((a^0x20)-(b^0x20));
/*
	int a = ((int)*(unsigned char*)ap) << 1;
	int b = ((int)*(unsigned char*)bp) << 1;

	return (a^0x41) - (b^0x41);
*/
}

/************************************************************************/


/*                    Functions for MLdonkey                            */


/************************************************************************/


#include <netinet/in.h>
#include "caml/mlvalues.h"
#include "caml/fail.h"

value ml_create_cipher(value unit)
{
  return (value) fst_cipher_create();
}

value ml_apply_cipher(value cipher_v, value s_v, value pos_v, value len_v)
{
  FSTCipher* cipher = (FSTCipher*) cipher_v;
  char *s = String_val(s_v);
  int pos = Int_val(pos_v);
  int len = Int_val(len_v);

/*  printf("Apply cipher %X on %d [from %d]\n", cipher, len, pos); */

  fst_cipher_crypt(cipher, s+pos, len);

  return Val_unit;
}

value ml_init_cipher(value cipher_v, value seed_v, value encode_v)
{
  FSTCipher* cipher = (FSTCipher*) cipher_v;
  unsigned int seed = Int32_val(seed_v);
  unsigned int encode = Int_val(encode_v);

  fst_cipher_init(cipher, seed, encode);

  return Val_unit;
}

value ml_set_cipher(value cipher_v, value seed_v, value encode_v)
{
  FSTCipher* cipher = (FSTCipher*) cipher_v;
  unsigned int seed = Int32_val(seed_v);
  unsigned int encode = Int_val(encode_v);

/*  printf("ml_set_cipher %X seed:%X enc_type: %X\n", cipher, seed, encode); */

  cipher->enc_type = encode;
  cipher->seed = seed;

  return Val_unit;
}

value ml_cipher_packet_get(value s_v, value pos_v, 
  value in_cipher_v, value out_cipher_v)
{
  FSTCipher* in_cipher = (FSTCipher*) in_cipher_v;
  FSTCipher* out_cipher = (FSTCipher*) out_cipher_v;
  char *s = String_val(s_v);
  int pos = Int_val(pos_v);
  unsigned int seed;
  unsigned int enc_type;

/*
  printf("ml_cipher_packet_get IN:%X OUT:%X pos %d\n", in_cipher, out_cipher, pos);

  printf("out seed:%X enc_type: %X\n", out_cipher->seed, out_cipher->enc_type);
*/

  seed = htonl  (((unsigned int*)(s+pos))[0]);
  enc_type = htonl (((unsigned int*)(s+pos+4))[0]);
  enc_type = fst_cipher_decode_enc_type (seed, enc_type);

  printf("in seed:%X enc_type: %X\n", seed, out_cipher->enc_type); 

  if(enc_type > 0x29)    failwith ("ERROR: unsupported encryption");

  out_cipher->seed ^= seed; /* xor send cipher with received seed */
  fst_cipher_init(out_cipher, out_cipher->seed, out_cipher->enc_type);
  fst_cipher_init(in_cipher, seed, enc_type);

  return Val_unit;
}

value ml_cipher_packet_set(value cipher_v, value s_v, value pos_v)
{
  FSTCipher* cipher = (FSTCipher*) cipher_v;
  char *s = String_val(s_v);
  int pos = Int_val(pos_v);

/*
  printf("ml_cipher_packet_set %X seed:%X enc_type:%X\n", cipher,
    cipher->seed, cipher->enc_type
  );
*/

/* in OCAML: "\250\000\182\043" */
  ((unsigned int*)(s+pos))[0] = 0x02BB600FA; /* random number? */
  ((unsigned int*)(s+pos+4))[0] = htonl(cipher->seed);
  ((unsigned int*)(s+pos+8))[0] = htonl(
    fst_cipher_encode_enc_type(cipher->seed, cipher->enc_type));

  return Val_unit;
}

value ml_cipher_free(value cipher_v)
{
  FSTCipher* cipher = (FSTCipher*) cipher_v;
  fst_cipher_free(cipher);

  return Val_unit;
}
