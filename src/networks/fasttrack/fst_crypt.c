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

#include <netinet/in.h>
#include "caml/mlvalues.h"
#include "caml/fail.h"

/* These functions can be used to layout the integers stored in memory
as on a 32-bits LittleEndian computer. They have to be called everytime
an integer as to be stored or loaded, in this file, and in the
enc_type_*.c files.
*/
void leint_to_string(int x, char *s)
{
  s[0] = x & 0xff;
  s[1] = (x >> 8) & 0xff;
  s[2] = (x >> 16) & 0xff;
  s[3] = (x >> 24) & 0xff;
}

unsigned int leint_of_string(unsigned char *s)
{
  return s[0] | ((s[1] | ((s[2] | (s[3] << 8)) << 8)) << 8);
}


void string_to_ints(char *s, int n)
{
  uint32 *ints = (uint32*)s;
  int i;
  
  for(i=0; i<n; i++)
    ints[i] = leint_of_string(s+4*i);
}

void ints_to_string(char *s, int n)
{
  uint32 *ints = (uint32*)s;
  int i;
  
  for(i=0; i<n; i++)
    leint_to_string(ints[i], s+4*i);
}

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

/*	printf("seed after pad_init: %ld\n", seed); */

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
	for(i=0; i<sizeof(cipher->lookup); i++){
		cipher->lookup[i] = (unsigned char)i;
/*	    printf("%02X", cipher->lookup[i]); */
	}
/*
	printf("\n");
	{
	  int k;
	  for(k=0; k<sizeof(cipher->pad);k++)
	    printf("%02X", cipher->pad[k]);
	  printf("\n");
	} */

	if(enc_type & 0x08)
	{
		MD5Context ctx;
		unsigned char md5[MD5_HASH_LEN];
                unsigned int md5_header;

		FST_HEAVY_DBG ("init_cipher: enc_type & 0x08");

		MD5Init(&ctx);
		MD5Update(&ctx, cipher->pad, sizeof(cipher->pad));
		MD5Final(md5, &ctx);

                md5_header = leint_of_string(md5);

		// modify cipher->lookup
		for(i=0; i<sizeof(cipher->lookup); i++)
		{

		  /* BIG ENDIAN */
			if( (j = calculate_num(&md5_header, 0x100 - i) + i) != i)
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

		FST_HEAVY_DBG ("pad_init: enc_type & 1");
/*		printf("\nkey_256="); */
		for(i=0; i<0xFF; i++)
		{
			seed = seed_step(seed);
			temp = seed >> 0x11;
			key_256[i] = (unsigned char) (temp % 0xE0);
/*			printf("%02X", key_256[i]); */
		}
/* 		printf("\n"); */

                  string_to_ints(key_256, 32); 
/*		printf("\nkey_256="); 
		for(i=0; i<0xFF; i++)
			printf("%02X", key_256[i]);
		printf("\n"); */

		enc_type_1 (key3_256, key_256);

/*		printf("\nkey3_256=");
		for(i=0; i<0xFF; i++)
			printf("%02X", key3_256[i]);
		printf("\n");
*/
                  ints_to_string(key3_256, 32); 

/*
		printf("\nkey3_256=");
		for(i=0; i<0xFF; i++)
			printf("%02X", key3_256[i]);
		printf("\n");
		printf("\npad=");
*/

		// merge with pad
		for(i=0; i<pad_size; i++){
			pad[i] ^= key3_256[i];
/*			printf("%02X", pad[i]); */
		}
/*		printf("\n"); */

	}

	if(enc_type & 0x1E6)
	{
		unsigned char p_key[80];

		FST_HEAVY_DBG ("pad_init: enc_type & 0x1E6");

		for(i=0; i<20; i++)
		{
			seed = seed_step(seed);
			leint_to_string(seed, p_key+i*4);
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

		}

		if(enc_type & 0x100)
		{
			FST_DBG ("pad_init: enc_type & 0x100, WARNING: not implemented");
			seed = seed_step(seed);

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
	}
	temp = cipher->add_to_lookup + xor;
	xor = cipher->lookup[temp];
	return xor;
}

static unsigned int calculate_num_xor(unsigned int seed)
{
	unsigned char key_80[80];
	int i;

	for(i=0; i<20; i++)
	{
		seed = seed_step(seed);
		leint_to_string(seed, key_80+i*4);
	}

	seed = seed_step(seed);
	enc_type_2 (key_80, seed);

	return leint_of_string(key_80+7*4);
}


static int calculate_num(unsigned int *num, int val)
{
	unsigned int temp = *num;
	int	temp2;

	if (!(val > 0x10001))
	{
/*	  printf("*num = %d\n", *num); */

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

/*	  printf("*num = %d, temp2 = %d\n", *num, temp2); */
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

/*  printf("in seed:%X enc_type: %X\n", seed, out_cipher->enc_type);  */

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
  /* ((unsigned int*)(s+pos))[0] = 0x02BB600FA; random number? */
  s[pos] = 250;
  s[pos+1] = 0;
  s[pos+2] = 182;
  s[pos+3] = 43;
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
