/*
 * $Id$
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

/*
 * This is the most recent pad mingling code for FastTrack as of 03/06/01
 * Used for encryption version 0x80
 */

typedef unsigned int u32;
typedef unsigned char u8;

/* our not so structured exception handling */

typedef int THROWS_RET;
#define THROW return -1;
#define TRY(x) { if((x) == -1) return 0; }
#define RETURN return 0;

/* some helper funcs */

#ifndef __GNUC__
#define __attribute__(x)
#endif

/* my_cos() and my_sin() are equal to cos()<0 and sin()<0. */
static int __attribute__ ((const)) my_cos (unsigned char i)
{
	return (i * 39 + 61) % 245 > 122;
}

/* It works because of fmod(i, 2*M_PI) > M_PI. */
static int __attribute__ ((const)) my_sin (unsigned char i)
{
	return (i * 46) % 289 > 144;
}

/* this is (unsigned int) floor(sqrt(((double)(((unsigned char)(i))))+1) + 0.001). */
static int __attribute__ ((const)) my_sqrt (unsigned char i)
{
	int j, k;

	for (j = 0, k = 0; j++ <= i; j += ++k << 1);
	return k;
}


/* macro for easier access to the key */

#define KEY(x) (*((u32*)(((u8*)key)+(x))))

/* some constants and helper funcs */ 

static u32 ROR(u32 value, u32 count)
{
  count = (count & 0xff) % 32;
  return (value >> count) | (value << (32 - count));
}

static u32 ROL(u32 value, u32 count)
{
  count = (count & 0xff) % 32;
  return (value << count) | (value >> (32 - count));
}

/* the entry point of this mess */
/* this all works on unsigned ints so endianess is not an issue */

static THROWS_RET enc_80_mix (u32 *key, u32 seed);

void enc_type_80 (u32 *key, u32 seed)
{
	enc_80_mix (key, seed);
}

/* major functions which make calls to other funcs */

static THROWS_RET enc_80_major_49B330 (u32 *key, u32 seed);
static THROWS_RET enc_80_major_49E930 (u32 *key, u32 seed);
static THROWS_RET enc_80_major_49ED30 (u32 *key, u32 seed);
static THROWS_RET enc_80_major_4A1BB0 (u32 *key, u32 seed);
static THROWS_RET enc_80_major_4AC300 (u32 *key, u32 seed);
static THROWS_RET enc_80_major_4AC560 (u32 *key, u32 seed);
static THROWS_RET enc_80_major_4AE780 (u32 *key, u32 seed);
static THROWS_RET enc_80_major_4B2FD0 (u32 *key, u32 seed);
static THROWS_RET enc_80_major_4BB410 (u32 *key, u32 seed);
static THROWS_RET enc_80_major_4C1A00 (u32 *key, u32 seed);
static THROWS_RET enc_80_major_4C52A0 (u32 *key, u32 seed);
static THROWS_RET enc_80_major_4CFE70 (u32 *key, u32 seed);
static THROWS_RET enc_80_major_4D2500 (u32 *key, u32 seed);
static THROWS_RET enc_80_major_4D2900 (u32 *key, u32 seed);
static THROWS_RET enc_80_major_4D7AE0 (u32 *key, u32 seed);
static THROWS_RET enc_80_major_4DB520 (u32 *key, u32 seed);
static THROWS_RET enc_80_major_4E5F90 (u32 *key, u32 seed);
static THROWS_RET enc_80_major_4E86B0 (u32 *key, u32 seed);
static THROWS_RET enc_80_major_4ECD20 (u32 *key, u32 seed);
static THROWS_RET enc_80_major_4EF430 (u32 *key, u32 seed);
static THROWS_RET enc_80_major_4F3220 (u32 *key, u32 seed);
static THROWS_RET enc_80_mix (u32 *key, u32 seed);

/* functions which throw exceptions */

static THROWS_RET enc_80_49E8E0 (u32 *key, u32 seed);
static THROWS_RET enc_80_4AC2B0 (u32 *key, u32 seed);
static THROWS_RET enc_80_4AEC50 (u32 *key, u32 seed);
static THROWS_RET enc_80_4B2BC0 (u32 *key, u32 seed);
static THROWS_RET enc_80_4B2C60 (u32 *key, u32 seed);
static THROWS_RET enc_80_4C5090 (u32 *key, u32 seed);
static THROWS_RET enc_80_4D5760 (u32 *key, u32 seed);
static THROWS_RET enc_80_4D57E0 (u32 *key, u32 seed);
static THROWS_RET enc_80_4D5830 (u32 *key, u32 seed);
static THROWS_RET enc_80_4D7A40 (u32 *key, u32 seed);
static THROWS_RET enc_80_4D7A90 (u32 *key, u32 seed);
static THROWS_RET enc_80_4DB090 (u32 *key, u32 seed);
static THROWS_RET enc_80_4DB3D0 (u32 *key, u32 seed);
static THROWS_RET enc_80_4E5EE0 (u32 *key, u32 seed);
static THROWS_RET enc_80_4ECBE0 (u32 *key, u32 seed);
static THROWS_RET enc_80_4EF230 (u32 *key, u32 seed);
static THROWS_RET enc_80_4EF350 (u32 *key, u32 seed);
static THROWS_RET enc_80_4EF3B0 (u32 *key, u32 seed);
static THROWS_RET enc_80_4F31A0 (u32 *key, u32 seed);

/* simple functions which modify key */

static void enc_80_49B280 (u32 *key, u32 seed);
static void enc_80_49B2E0 (u32 *key, u32 seed);
static void enc_80_49B310 (u32 *key, u32 seed);
static void enc_80_49B600 (u32 *key, u32 seed);
static void enc_80_49B630 (u32 *key, u32 seed);
static void enc_80_49ECD0 (u32 *key, u32 seed);
static void enc_80_4A1B20 (u32 *key, u32 seed);
static void enc_80_4A1B50 (u32 *key, u32 seed);
static void enc_80_4A1B80 (u32 *key, u32 seed);
static void enc_80_4AECA0 (u32 *key, u32 seed);
static void enc_80_4B2CB0 (u32 *key, u32 seed);
static void enc_80_4B2CE0 (u32 *key, u32 seed);
static void enc_80_4C19B0 (u32 *key, u32 seed);
static void enc_80_4C19E0 (u32 *key, u32 seed);
static void enc_80_4C5060 (u32 *key, u32 seed);
static void enc_80_4C50E0 (u32 *key, u32 seed);
static void enc_80_4C5240 (u32 *key, u32 seed);
static void enc_80_4C5270 (u32 *key, u32 seed);
static void enc_80_4D57B0 (u32 *key, u32 seed);
static void enc_80_4D7940 (u32 *key, u32 seed);
static void enc_80_4D7980 (u32 *key, u32 seed);
static void enc_80_4D79B0 (u32 *key, u32 seed);
static void enc_80_4D79E0 (u32 *key, u32 seed);
static void enc_80_4D7A10 (u32 *key, u32 seed);
static void enc_80_4DB000 (u32 *key, u32 seed);
static void enc_80_4DB030 (u32 *key, u32 seed);
static void enc_80_4DB240 (u32 *key, u32 seed);
static void enc_80_4DB270 (u32 *key, u32 seed);
static void enc_80_4DB2E0 (u32 *key, u32 seed);
static void enc_80_4DB480 (u32 *key, u32 seed);
static void enc_80_4E5D40 (u32 *key, u32 seed);
static void enc_80_4E5D70 (u32 *key, u32 seed);
static void enc_80_4E5DA0 (u32 *key, u32 seed);
static void enc_80_4E5DD0 (u32 *key, u32 seed);
static void enc_80_4E5E00 (u32 *key, u32 seed);
static void enc_80_4E5E60 (u32 *key, u32 seed);
static void enc_80_4E5EB0 (u32 *key, u32 seed);
static void enc_80_4E5F30 (u32 *key, u32 seed);
static void enc_80_4E8500 (u32 *key, u32 seed);
static void enc_80_4E8530 (u32 *key, u32 seed);
static void enc_80_4E8590 (u32 *key, u32 seed);
static void enc_80_4E85C0 (u32 *key, u32 seed);
static void enc_80_4E8620 (u32 *key, u32 seed);
static void enc_80_4E8680 (u32 *key, u32 seed);
static void enc_80_4ECC60 (u32 *key, u32 seed);
static void enc_80_4ECC90 (u32 *key, u32 seed);
static void enc_80_4ECCC0 (u32 *key, u32 seed);
static void enc_80_4ECCF0 (u32 *key, u32 seed);
static void enc_80_4EF200 (u32 *key, u32 seed);
static void enc_80_4EF280 (u32 *key, u32 seed);
static void enc_80_4EF2A0 (u32 *key, u32 seed);
static void enc_80_4EF300 (u32 *key, u32 seed);
static void enc_80_4EF400 (u32 *key, u32 seed);

/* and so it begins... */

void enc_80_4ECCF0 (u32 *key, u32 seed)
{
	KEY(0x14) += KEY(0x10) * 73;
}

void enc_80_4E8590 (u32 *key, u32 seed)
{
	KEY(0x00) += KEY(0x14) & 0x1B65B2C8;
}

void enc_80_4ECCC0 (u32 *key, u32 seed)
{
	KEY(0x24) += KEY(0x18) + 0x124D08A0;
}

void enc_80_4ECC60 (u32 *key, u32 seed)
{
	KEY(0x40) ^= (KEY(0x00) * 41) << 1;
}

void enc_80_4E8680 (u32 *key, u32 seed)
{
	KEY(0x38) ^= KEY(0x28) - 0x403483CE;
}

void enc_80_4ECC90 (u32 *key, u32 seed)
{	
	KEY(0x0C) ^= (KEY(0x2C) * 41) << 1;
}

void enc_80_4E8500 (u32 *key, u32 seed)
{
	KEY(0x28) = ROL(KEY(0x28), (seed * 15) << 2);
}

void enc_80_49B2E0 (u32 *key, u32 seed)
{
	KEY(0x04) = ROR(KEY(0x04), (KEY(0x20) * 7) << 4);
}

void enc_80_49B310 (u32 *key, u32 seed)
{
	KEY(0x28) *= ROL(KEY(0x04), 0xCC);
}

void enc_80_49B600 (u32 *key, u32 seed)
{
	KEY(0x48) = ROL(KEY(0x48), seed - 0xE066BF0);
}

void enc_80_4A1B20 (u32 *key, u32 seed)
{
	KEY(0x48) += (0 - ((seed << 4) - seed)) * 5;
}

void enc_80_4A1B50 (u32 *key, u32 seed)
{
	KEY(0x40) |= KEY(0x48) | 0xB25175E;
}

void enc_80_4A1B80 (u32 *key, u32 seed)
{
	KEY(0x08) |= KEY(0x3C) - 0x1886D6A;
}

void enc_80_4AECA0 (u32 *key, u32 seed)
{
	KEY(0x40) -= ROL(KEY(0x0C), 0xBB);
}

void enc_80_4B2CB0 (u32 *key, u32 seed)
{
	KEY(0x2C) |= KEY(0x34) * 9;
}

void enc_80_4B2CE0 (u32 *key, u32 seed)
{
	KEY(0x18) &= KEY(0x4C) ^ 0x1FAF0F41;
}

void enc_80_4C19B0 (u32 *key, u32 seed)
{
	KEY(0x10) = ROR(KEY(0x10), (KEY(0x44) * 11) << 3);
}

void enc_80_4C19E0 (u32 *key, u32 seed)
{
	KEY(0x44) *= ROL(KEY(0x18), 0xC0);
}

void enc_80_4C5060 (u32 *key, u32 seed)
{
	KEY(0x0C) ^= KEY(0x24) + 0x5B1A81FD;
}

void enc_80_4C5240 (u32 *key, u32 seed)
{
	KEY(0x44) *= ROR(KEY(0x28), 0x9A);
}

void enc_80_4C5270 (u32 *key, u32 seed)
{
	KEY(0x4C) ^= KEY(0x0C) ^ 0x19859C46;
}

void enc_80_4D57B0 (u32 *key, u32 seed)
{
	KEY(0x40) += KEY(0x04) + 0x5EDB78DA;
}

void enc_80_4D7940 (u32 *key, u32 seed)
{
	KEY(0x30) = ROL(KEY(0x30), (KEY(0x0C) * 43) << 1);
}

void enc_80_4D7980 (u32 *key, u32 seed)
{
	KEY(0x44) *= KEY(0x34) - 0x68C0E272;
}

void enc_80_4D79B0 (u32 *key, u32 seed)
{
	KEY(0x34) += KEY(0x34) | 0x5E919E06;
}

void enc_80_4D79E0 (u32 *key, u32 seed)
{
	KEY(0x40) = (KEY(0x24) * KEY(0x40) * 37) << 1;
}

void enc_80_4D7A10 (u32 *key, u32 seed)
{
	KEY(0x08) |= KEY(0x3C) + 0x44B04775;
}

void enc_80_4DB240 (u32 *key, u32 seed)
{
	KEY(0x20) = (KEY(0x20) * KEY(0x20) * 13) << 2;
}

void enc_80_4DB000 (u32 *key, u32 seed)
{
	KEY(0x44) = ROR(KEY(0x44), seed + 0x451498EC);
}

void enc_80_4DB480 (u32 *key, u32 seed)
{
	KEY(0x28) = ((KEY(0x28) * seed * 27) << 2) - (seed * KEY(0x28));
}

void enc_80_4E5D40 (u32 *key, u32 seed)
{
	KEY(0x20) += KEY(0x44) + 0x4E0679BE;
}

void enc_80_4E5D70 (u32 *key, u32 seed)
{
	KEY(0x28) ^= KEY(0x18) ^ 0x2BE68205;
}

void enc_80_4E5DA0 (u32 *key, u32 seed)
{
	KEY(0x1C) += (0 - (((seed * 3) << 3) - seed)) << 2;
}

void enc_80_4E5EB0 (u32 *key, u32 seed)
{
	KEY(0x34) &= ROL(seed, 0x30);
}

void enc_80_4EF200 (u32 *key, u32 seed)
{
	KEY(0x24) &= seed * 75;
}

void enc_80_4EF280 (u32 *key, u32 seed)
{
	KEY(0x00) += seed * 25;
}

void enc_80_4EF400 (u32 *key, u32 seed)
{
	KEY(0x20) *= KEY(0x18) ^ 0x377C08D2;
}


void enc_80_4E5DD0 (u32 *key, u32 seed)
{
	KEY(0x48) -=  KEY(0x28) < 0xB6C6C3E ? KEY(0x28) : KEY(0x34);
}

void enc_80_4E8530 (u32 *key, u32 seed)
{
	KEY(0x4C) += my_sqrt(KEY(0x48));
}

void enc_80_4EF2A0 (u32 *key, u32 seed)
{
	KEY(0x2C) ^= my_sqrt(KEY(0x10) );
}

void enc_80_49B280 (u32 *key, u32 seed)
{
	KEY(0x44) ^= my_sqrt(KEY(0x40) );
}

void enc_80_4E85C0 (u32 *key, u32 seed)
{
	KEY(0x24) += my_sqrt(KEY(0x34) );
}


void enc_80_4E8620 (u32 *key, u32 seed)
{
	KEY(0x28) -= my_cos(seed) ? 0x19C6C6E : KEY(0x1C);
}

void enc_80_4EF300 (u32 *key, u32 seed)
{
	KEY(0x28) -= my_cos(KEY(0x28)) ? 0x5F18F01 : KEY(0x34);
}

void enc_80_4E5E00 (u32 *key, u32 seed)
{
	KEY(0x44) = ROL(KEY(0x44), my_sin(KEY(0x1C)) ? 0x4262ED6B : seed);
}

void enc_80_4E5E60 (u32 *key, u32 seed)
{
	KEY(0x24) += my_cos(KEY(0x18)) ? 0x14F5046C : KEY(0x2C);
}

void enc_80_4E5F30 (u32 *key, u32 seed)
{
	KEY(0x38) ^= my_cos(KEY(0x2C)) ? 0x562482FA : KEY(0x38);
}

void enc_80_49ECD0 (u32 *key, u32 seed)
{
	KEY(0x0C) -= my_cos(KEY(0x38)) ? 0x39702EDD : KEY(0x18);
}

void enc_80_4DB030 (u32 *key, u32 seed)
{
	KEY(0x18) ^= my_cos(KEY(0x28)) ? 0x53236223 : KEY(0x1C);
}

void enc_80_4DB270 (u32 *key, u32 seed)
{
	KEY(0x04) |= my_sin(seed) ? 0x1BE7FECF : KEY(0x40);
}

void enc_80_4DB2E0 (u32 *key, u32 seed)
{
	KEY(0x44) *= my_cos(KEY(0x40)) ? 0x2C15B485 : KEY(0x0C);
}


THROWS_RET enc_80_4F31A0 (u32 *key, u32 seed)
{
	KEY(0x4C) += my_sqrt(KEY(0x00) );

	if (KEY(0x4C) & 1)
		THROW;

	RETURN;
}

THROWS_RET enc_80_4ECBE0 (u32 *key, u32 seed)
{
	KEY(0x1C) = ROL(KEY(0x1C), my_sqrt(KEY(0x04) ));

	if (KEY(0x1C) & 1)
		THROW;

	RETURN;
}

THROWS_RET enc_80_4DB3D0 (u32 *key, u32 seed)
{
	KEY(0x10) += my_cos(KEY(0x04)) ? 0x890AFEF : KEY(0x10);

	if (KEY(0x10) & 1)
		THROW;

	RETURN;
}

THROWS_RET enc_80_4B2BC0 (u32 *key, u32 seed)
{
	KEY(0x1C) = ROL(KEY(0x1C), my_sin(KEY(0x48)) ? 0x14D1DE3D : seed );

	if (KEY(0x1C) & 1)
		THROW;

	RETURN;
}

THROWS_RET enc_80_4EF350 (u32 *key, u32 seed)
{
	KEY(0x28) &=  KEY(0x04) < 0x1F2DD61 ? KEY(0x04) : KEY(0x28);

	if (KEY(0x28) & 1)
		THROW;

	RETURN;
}

THROWS_RET enc_80_4D57E0 (u32 *key, u32 seed)
{
	KEY(0x00) |= ((KEY(0x28) * 3) << 4) + KEY(0x28);

	if (KEY(0x00) & 1)
		THROW;

	RETURN;
}

THROWS_RET enc_80_4DB090 (u32 *key, u32 seed)
{
	KEY(0x24) ^=  KEY(0x04) < 0xB01609F ? KEY(0x04) : KEY(0x40);

	if (KEY(0x24) & 1)
		THROW;

	RETURN;
}

THROWS_RET enc_80_4C5090 (u32 *key, u32 seed)
{
	KEY(0x34) = ROR(KEY(0x34), (seed * 9) << 1);

	if (KEY(0x34) & 1)
		THROW;

	RETURN;
}


THROWS_RET enc_80_49E8E0 (u32 *key, u32 seed)
{
	KEY(0x1C) &= seed + 0x28495DA2;

	if (KEY(0x1C) & 1)
		THROW;

	RETURN;
}

THROWS_RET enc_80_4AC2B0 (u32 *key, u32 seed)
{
	KEY(0x2C) += ROL(KEY(0x14), 0x14);

	if (KEY(0x2C) & 1)
		THROW;

	RETURN;
}

THROWS_RET enc_80_4AEC50 (u32 *key, u32 seed)
{
	KEY(0x14) ^= seed + 0x391B56A;

	if (KEY(0x14) & 1)
		THROW;

	RETURN;
}

THROWS_RET enc_80_4B2C60 (u32 *key, u32 seed)
{
	KEY(0x0C) ^= KEY(0x38) * 0x711881F7;

	if (KEY(0x0C) & 1)
		THROW;

	RETURN;
}

THROWS_RET enc_80_4D5760 (u32 *key, u32 seed)
{
	KEY(0x18) -= KEY(0x48) ^ 0x4402CAF;

	if (KEY(0x18) & 1)
		THROW;

	RETURN;
}

THROWS_RET enc_80_4D5830 (u32 *key, u32 seed)
{
	KEY(0x08) += ROR(KEY(0x38), 0xD4);

	if (KEY(0x08) & 1)
		THROW;

	RETURN;
}

THROWS_RET enc_80_4D7A40 (u32 *key, u32 seed)
{
	KEY(0x18) += 0x287735D1 - KEY(0x0C);

	if (KEY(0x18) & 1)
		THROW;

	RETURN;
}

THROWS_RET enc_80_4D7A90 (u32 *key, u32 seed)
{
	KEY(0x0C) += 0x247B4DE9 - KEY(0x04);

	if (KEY(0x0C) & 1)
		THROW;

	RETURN;
}

THROWS_RET enc_80_4E5EE0 (u32 *key, u32 seed)
{
	KEY(0x2C) += KEY(0x18) * 0xE8C67004;

	if (KEY(0x2C) & 1)
		THROW;

	RETURN;
}


THROWS_RET enc_80_4EF230 (u32 *key, u32 seed)
{
	KEY(0x3C) |= KEY(0x04) ^ 0x37B6FE35;

	if (KEY(0x3C) & 1)
		THROW;

	RETURN;
}

THROWS_RET enc_80_4EF3B0 (u32 *key, u32 seed)
{
	KEY(0x38) = ROR(KEY(0x38), KEY(0x34) | 0x3433BE6);

	if (KEY(0x38) & 1)
		THROW;

	RETURN;
}


void enc_80_49B630 (u32 *key, u32 seed)
{
	u32 var_magic = 0x5008C0A3;

	var_magic |= 0x4E75FE;
	KEY(0x28) *= my_cos(KEY(0x0C)) ? 0x1DD34A4 : KEY(0x08);

	var_magic += 0x4F4BA2;
	KEY(0x30) |= (KEY(0x40) * 11) << 2;
	KEY(0x00) ^= KEY(0x4C) ^ var_magic;

	var_magic += 0x4F01BD;
	KEY(0x34) += (0 - (((KEY(0x24) * 15) << 1) - KEY(0x24))) << 2;

	var_magic += 0x47203F;
	seed -= my_cos(KEY(0x08)) ? 0xD7A79F4 : KEY(0x14);
	KEY(0x08) -= my_sin(KEY(0x30)) ? var_magic : KEY(0x04);

	var_magic |= 0x46235B;
	seed ^= my_sin(KEY(0x10)) ? 0x241147A3 : KEY(0x34);

	var_magic |= 0x40FE18;
	KEY(0x0C) += my_sqrt(seed);
	KEY(0x00) -= my_sin(seed) ? var_magic : KEY(0x3C);

	var_magic &= 0x442339;
	KEY(0x0C) += (0 - (((KEY(0x24) * 15) << 1) - KEY(0x24))) * 3;

	var_magic ^= 0x45AB8E;
	KEY(0x38) |=  KEY(0x08) < var_magic ? KEY(0x08) : KEY(0x28);
	KEY(0x34) ^= var_magic + seed;
}

void enc_80_4C50E0 (u32 *key, u32 seed)
{
	u32 var_magic = 0x33859641;

	var_magic &= 0x476CA7;
	KEY(0x34) -= KEY(0x08);

	var_magic |= 0x431C95;
	KEY(0x4C) += var_magic & KEY(0x30);
	KEY(0x38) +=  KEY(0x1C) < var_magic ? KEY(0x1C) : KEY(0x08);

	var_magic |= 0x4B8F45;

	var_magic &= 0x44E6C0;
	KEY(0x30) -= KEY(0x2C) ^ 0x5E823762;
	KEY(0x3C) &= (KEY(0x00) * 13) << 2;

	var_magic -= 0x4BB60F;
	KEY(0x44) ^= KEY(0x20) * 15;

	var_magic ^= 0x4FCB21;
	KEY(0x1C) += KEY(0x10) & 0x3996FD51;
	
	var_magic -= 0x484488;
	KEY(0x08) += my_sin(KEY(0x4C)) ? 0x34311111 : KEY(0x14);
}


THROWS_RET enc_80_major_4E5F90 (u32 *key, u32 seed)
{
	u32 type = (KEY(0x20) ^ KEY(0x40) ^ seed) % 5;
	u32 var_magic = 0x32EF0625;

	var_magic &= 0x4D21A3;
	seed *= my_sqrt(seed);
	var_magic ^= 0x4B7FA9;
	KEY(0x10) |=  seed < var_magic ? seed : KEY(0x0C);

	if (type == 1)
	{
		enc_80_4D57B0 (key, 0xBBA60924);
		enc_80_49B280 (key, 0xEA88164E);
		enc_80_4C50E0 (key, seed);
	}

	KEY(0x1C) = ROR(KEY(0x1C), KEY(0x44) & var_magic);
	var_magic &= 0x47D41A;
	KEY(0x00) += ROR(KEY(0x18), var_magic);

	if (type == 0)
	{
		enc_80_4A1B80 (key, 0x12264B76);
		enc_80_4E8500 (key, 0xB899C92B);
		enc_80_49B630 (key, seed);
	}

	var_magic -= 0x40E6F2;
	seed -= KEY(0x38) ^ var_magic;
	seed &= my_sqrt(KEY(0x04));

	if (type == 2)
	{
		enc_80_4DB270 (key, 0xF9FFE780);
		TRY(enc_80_4D7A40 (key, 0xA9195A7));
		TRY(enc_80_major_4AC300 (key, KEY(0x18)));
	}

	var_magic |= 0x4AA991;
	seed = ROL(seed, KEY(0x34) + 0x8D810DF);
	var_magic |= 0x4423C1;
	KEY(0x1C) ^= (seed * 11) << 1;

	if (type == 3)
	{
		enc_80_4A1B20 (key, 0x7ADA2000);
		enc_80_4DB2E0 (key, 0x2EFB1490);
		TRY(enc_80_major_4D7AE0 (key, KEY(0x20)));
	}

	KEY(0x20) ^= my_cos(KEY(0x40)) ? 0x12DA5B58 : KEY(0x08);
	var_magic ^= 0x43759A;
	KEY(0x38) ^= KEY(0x34) - var_magic;

	if (type == 4)
	{
		enc_80_4AECA0 (key, 0xFC6CAD0D);
		enc_80_4E5F30 (key, 0x4E2726D6);
		TRY(enc_80_major_49B330 (key, seed));
	}

	var_magic ^= 0x4B2F34;
	KEY(0x18) *=  KEY(0x44) < 0x9D9106A ? KEY(0x44) : KEY(0x20);

	RETURN;
}

THROWS_RET enc_80_major_4E86B0 (u32 *key, u32 seed)
{
	u32 type = (KEY(0x28) ^ KEY(0x00) ^ KEY(0x34)) % 0x0E;
	u32 var_magic = 0x21647591;

	if (type == 3)
	{
		TRY(enc_80_4D5830 (key, 0x13C075FC));
		enc_80_4D7980 (key, 0x42C03032);
		TRY(enc_80_major_4AC300 (key, KEY(0x28)));
	}

	var_magic |= 0x478232;
	KEY(0x4C) = ROL(KEY(0x4C), seed * 41);

	if (type == 0x0D)
	{
		TRY(enc_80_4EF230 (key, 0x2811B665));
		enc_80_4A1B80 (key, 0x156F97E2);
		TRY(enc_80_major_4D7AE0 (key, seed));
	}

	if (type == 2)
	{
		enc_80_4AECA0 (key, 0x4F05483D);
		enc_80_4DB2E0 (key, 0x5C118568);
		TRY(enc_80_major_49B330 (key, KEY(0x4)));
	}

	var_magic -= 0x445085;
	KEY(0x30) &= seed & 0x162E075D;

	if (type == 0)
	{
		enc_80_4EF2A0 (key, 0x4BE3988);
		enc_80_4DB000 (key, 0xCC7693F);
		TRY(enc_80_major_4E5F90 (key, KEY(0x0C)));
	}

	KEY(0x1C) += 0xC0CF1E75 - KEY(0x48);

	if (type == 0)
	{
		enc_80_4E5DD0 (key, 0x4DA506DB);
		enc_80_4E5DA0 (key, 0x3E303464);
		TRY(enc_80_major_4EF430 (key, KEY(0x34)));
	}

	if (type == 0x0B)
	{
		TRY(enc_80_4B2C60 (key, 0x2169D1B5));
		enc_80_49ECD0 (key, 0x202D78B2);
		TRY(enc_80_major_4C52A0 (key, seed));
	}
	
	var_magic |= 0x4D9F2D;
	KEY(0x28) *= seed + 0x15A0944D;

	if (type == 7)
	{
		enc_80_4E5D70 (key, 0x4E33147D);
		TRY(enc_80_4B2BC0 (key, 0xE636D969));
		TRY(enc_80_major_49E930 (key, KEY(0x38)));
	}

	var_magic ^= 0x458260;
	KEY(0x34) += KEY(0x00) - 0x3B1C8FB8;

	if (type == 8)
	{
		enc_80_4EF400 (key, 0xE6F5A7A);
		enc_80_4E5D40 (key, 0x5913D7B4);
		TRY(enc_80_major_49ED30 (key, KEY(0x14)));
	}

	if (type == 6)
	{
		TRY(enc_80_4AEC50 (key, 0x34472A0D));
		enc_80_4E8530 (key, 0x75648676);
		TRY(enc_80_major_4D2500 (key, KEY(0x4)));
	}

	KEY(0x48) *=  KEY(0x08) < 0xF9B0FAE ? KEY(0x08) : KEY(0x4C);

	if (type == 0x0C)
	{
		TRY(enc_80_4B2BC0 (key, 0xDF3D4CFC));
		TRY(enc_80_4E5EE0 (key, 0xDF21CAA9));
		TRY(enc_80_major_4AE780 (key, seed));
	}

	var_magic &= 0x414390;
	KEY(0x04) += (((KEY(0x14) * 3) << 3) - KEY(0x14)) << 1;

	if (type == 1)
	{
		enc_80_4E5E60 (key, 0xA31A3358);
		enc_80_4EF280 (key, 0x288A7CDC);
		TRY(enc_80_major_4F3220 (key, KEY(0x44)));
	}

	if (type == 4)
	{
		enc_80_4E5E60 (key, 0x3C826AC);
		enc_80_4E5F30 (key, 0x78837248);
		TRY(enc_80_major_4C1A00 (key, seed));
	}

	var_magic -= 0x4D6148;
	KEY(0x2C) ^= var_magic | KEY(0x34);

	if (type == 0x0A)
	{
		TRY(enc_80_4AC2B0 (key, 0x57634797));
		enc_80_4C19E0 (key, 0xA5A21972);
		TRY(enc_80_major_4AC300 (key, KEY(0x2C)));
	}

	KEY(0x34) ^= seed ^ 0x265916C9;

	if (type == 5)
	{
		enc_80_4C19B0 (key, 0xCA64A98F);
		TRY(enc_80_4EF350 (key, 0x5065055D));
		TRY(enc_80_major_4D7AE0 (key, KEY(0x2C)));
	}

	if (type == 9)
	{
		enc_80_4C5060 (key, 0x126C34B0);
		enc_80_4ECC90 (key, 0x18FD3B9A);
		TRY(enc_80_major_49B330 (key, KEY(0x4)));
	}

	KEY(0x08) *= KEY(0x14) * 105;

	RETURN;
}

THROWS_RET enc_80_major_4ECD20 (u32 *key, u32 seed)
{
	u32 type = (KEY(0x0C) ^ KEY(0x38) ^ seed) & 0x0F;
	u32 var_magic = 0xBD471AA;

	if (type == 0x0C)
	{
		enc_80_4D7980 (key, 0x4582E75A);
		enc_80_4DB270 (key, 0xE7C0DE4B);
		TRY(enc_80_major_49E930 (key, seed));
	}

	if (type == 4)
	{
		enc_80_4ECCF0 (key, 0x53609549);
		enc_80_4D79B0 (key, 0x299F8140);
		TRY(enc_80_major_49ED30 (key, KEY(0x4C)));
	}

	var_magic += 0x4EDDC5;
	seed += (((0 - KEY(0x14)) << 3) - KEY(0x14)) * 2;

	if (type == 8)
	{
		TRY(enc_80_49E8E0 (key, 0x60C57D12));
		enc_80_49B310 (key, 0xCB2A0C8);
		TRY(enc_80_major_4D2500 (key, seed));
	}

	if (type == 0x0E)
	{
		enc_80_4D7980 (key, 0x7EC193CC);
		enc_80_4C5060 (key, 0x3034C76A);
		TRY(enc_80_major_4AE780 (key, KEY(0x10)));
	}

	var_magic -= 0x4A9A10;
	KEY(0x4C) ^= var_magic ^ KEY(0x2C);

	if (type == 0x0B)
	{
		enc_80_4E5EB0 (key, 0x20AA3ABD);
		TRY(enc_80_4AEC50 (key, 0x5D894D2));
		TRY(enc_80_major_4F3220 (key, KEY(0x44)));
	}

	if (type == 4)
	{
		enc_80_4E5F30 (key, 0x5EC2A8BD);
		enc_80_4E5DD0 (key, 0x22CA241A);
		TRY(enc_80_major_4C1A00 (key, seed));
	}

	KEY(0x40) += (0 - (((KEY(0x0C) * 3) << 2) - KEY(0x0C))) << 1; 

	if (type == 1)
	{
		enc_80_4C5270 (key, 0xC779F5F8);
		enc_80_4DB480 (key, 0xDA9100);
		TRY(enc_80_major_4E86B0 (key, seed));
	}

	if (type == 9)
	{
		enc_80_4D7940 (key, 0x21C13AA6);
		enc_80_4D7A10 (key, 0x73F1E788);
		TRY(enc_80_major_4AC560 (key, KEY(0x24)));
	}

	var_magic &= 0x444707;
	KEY(0x38) *= KEY(0x14) | 0x132A8FBD;

	if (type == 0x0A)
	{
		enc_80_4DB000 (key, 0x3D008A13);
		enc_80_4EF200 (key, 0x2B5507BC);
		TRY(enc_80_major_4BB410 (key, seed));
	}

	if (type == 0x0D)
	{
		enc_80_4A1B80 (key, 0xF0AD3C52);
		enc_80_4C5240 (key, 0xFCA654);
		TRY(enc_80_major_4B2FD0 (key, seed));
	}

	if (type == 2)
	{
		TRY(enc_80_4F31A0 (key, 0xA45135B));
		enc_80_4E5EB0 (key, 0xFE2E49B0);
		TRY(enc_80_major_4D2900 (key, KEY(0x44)));
	}

	var_magic += 0x4B8F8E;
	KEY(0x30) += ((0 - (((seed * 5) << 2) - seed)) * 3) << 1;

	if (type == 1)
	{
		TRY(enc_80_4C5090 (key, 0x667591C7));
		enc_80_4C5240 (key, 0x58E1CA1);
		TRY(enc_80_major_4CFE70 (key, KEY(0x38)));
	}

	if (type == 2)
	{
		enc_80_4E5D70 (key, 0x156B1B77);
		TRY(enc_80_4DB3D0 (key, 0xF357B339));
		TRY(enc_80_major_49E930 (key, seed));
	}

	KEY(0x40) *= my_cos(seed) ? 0x40E92E8A : KEY(0x24);

	if (type == 0)
	{
		enc_80_4C19B0 (key, 0xD54A22A2);
		TRY(enc_80_4D5830 (key, 0x490E6975));
		TRY(enc_80_major_49ED30 (key, KEY(0x14)));
		enc_80_4D57B0 (key, 0x227ACE1D);
		enc_80_4E5E60 (key, 0x81AE2D0E);
		TRY(enc_80_major_4D2500 (key, KEY(0x28)));
	}

	var_magic += 0x451A1D;
	seed *= KEY(0x08) ^ 0xE5C69EA;

	if (type == 5)
	{
		TRY(enc_80_4C5090 (key, 0x9240545D));
		enc_80_4D79B0 (key, 0x2F58FEFB);
		TRY(enc_80_major_4AE780 (key, KEY(0x24)));
	}

	if (type == 7)
	{
		TRY(enc_80_4D57E0 (key, 0x926330D3));
		enc_80_49B2E0 (key, 0x552B1599);
		TRY(enc_80_major_4F3220 (key, KEY(0x00)));
	}

	var_magic &= 0x4F2085;
	KEY(0x20) += seed - 0x1DBCEFC2;

	if (type == 3)
	{
		enc_80_4C5270 (key, 0xF1307962);
		enc_80_4ECCF0 (key, 0x5CDFEC0);
		TRY(enc_80_major_4C1A00 (key, KEY(0x38)));
	}

	if (type == 6)
	{
		enc_80_4D79B0 (key, 0x332616DD);
		enc_80_4E5DA0 (key, 0x907C5B26);
		TRY(enc_80_major_4E86B0 (key, seed));
	}

	seed += KEY(0x3C) - 0x3206A0CA;

	if (type == 3)
	{
		enc_80_49B310 (key, 0x55EDF250);
		enc_80_4A1B80 (key, 0x168C2659);
		TRY(enc_80_major_4AC560 (key, seed));
	}

	if (type == 0x0F)
	{
		TRY(enc_80_4D7A40 (key, 0x47E3EB0F));
		enc_80_4ECC90 (key, 0x51D7D487);
		TRY(enc_80_major_4BB410 (key, KEY(0x4)));
	}

	var_magic ^= 0x47F2F1;
	KEY(0x38) ^= KEY(0x20) & var_magic;

	RETURN;
}

THROWS_RET enc_80_major_4EF430 (u32 *key, u32 seed)
{
	u32 type = (KEY(0x30) ^ KEY(0x2C) ^ seed) % 0x06;
	u32 var_magic = 0x12FF768;

	var_magic &= 0x465B85;
	KEY(0x2C) *= var_magic | seed;

	if (type == 2)
	{
		enc_80_4C19E0 (key, 0xAD3C3D8B);
		enc_80_4A1B50 (key, 0x850F3164);
		enc_80_4C50E0 (key, KEY(0x14));
	}

	var_magic -= 0x4631B0;
	seed += 0 - (((KEY(0x18) * 5) << 4) - KEY(0x18));
	KEY(0x20) = ROL(KEY(0x20), my_sqrt(KEY(0x24)));

	if (type == 0)
	{
		TRY(enc_80_4DB090 (key, 0x24387A66));
		enc_80_4ECC60 (key, 0x7782AEBD);
		enc_80_49B630 (key, KEY(0x10));
	}

	var_magic -= 0x426A8F;
	KEY(0x00) -= KEY(0x38) ^ var_magic;
	var_magic += 0x43515B;
	KEY(0x18) = ROL(KEY(0x18), KEY(0x2C) ^ var_magic);

	if (type == 5)
	{
		enc_80_4AECA0 (key, 0x6B384E2B);
		enc_80_4AECA0 (key, 0x6D2E96B9);
		TRY(enc_80_major_4AC300 (key, KEY(0x1C)));
	}

	seed += (0 - (KEY(0x2C) * 3)) << 4;

	if (type == 0)
	{
		enc_80_49B310 (key, 0xAF07482C);
		enc_80_4AECA0 (key, 0x10D5A571);
		TRY(enc_80_major_4D7AE0 (key, seed));
	}

	var_magic ^= 0x491D10;
	KEY(0x34) |= ROR(KEY(0x40), var_magic);
	var_magic ^= 0x47DEE2;
	KEY(0x0C) |= KEY(0x24) + 0x27C4C44E;

	if (type == 1)
	{
		enc_80_4DB000 (key, 0xE8890374);
		enc_80_4E8680 (key, 0x88B9E352);
		TRY(enc_80_major_49B330 (key, seed));
	}

	KEY(0x34) += (0 - (((KEY(0x18) * 5) << 3) - KEY(0x18))) << 1;
	var_magic += 0x454F3C;
	KEY(0x28) ^= ROL(KEY(0x2C), 0xA4);

	if (type == 3)
	{
		enc_80_49B280 (key, 0x4E9B95C5);
		TRY(enc_80_4D57E0 (key, 0x926BA8BC));
		TRY(enc_80_major_4E5F90 (key, KEY(0x28)));
	}

	RETURN;
}


THROWS_RET enc_80_major_4F3220 (u32 *key, u32 seed)
{
	u32 type = (KEY(0x38) ^ KEY(0x08) ^ KEY(0x4C)) % 0x0C;
	u32 var_magic = 0x189D772E;

	if (type == 4)
	{
		enc_80_4C19E0 (key, 0xFEF21601);
		TRY(enc_80_4ECBE0 (key, 0x4D63C273));
		enc_80_4C50E0 (key, KEY(0x20));
	}

	var_magic += 0x467A65;
	seed |= KEY(0x2C) ^ 0x4E05B048;

	if (type == 0x0A)
	{
		enc_80_4E85C0 (key, 0x201892BA);
		enc_80_4E5F30 (key, 0x170AA5EF);
		enc_80_49B630 (key, KEY(0x4));
	}

	var_magic &= 0x486071;
	seed += KEY(0x08) * 50;

	if (type == 1)
	{
		TRY(enc_80_4D57E0 (key, 0x6DC87084));
		enc_80_4D79B0 (key, 0xEF511AE);
		TRY(enc_80_major_4AC300 (key, seed));
	}

	seed = (seed * KEY(0x0C) * 13) << 1;

	if (type == 3)
	{
		enc_80_4D79B0 (key, 0x2A37FAB6);
		enc_80_4D57B0 (key, 0xB6ACE620);
		TRY(enc_80_major_4D7AE0 (key, KEY(0x1C)));
	}

	var_magic &= 0x49DC0D;
	KEY(0x28) |= KEY(0x4C) & 0x42A403D;

	if (type == 6)
	{
		enc_80_4D7940 (key, 0xCE7594C1);
		TRY(enc_80_4D7A40 (key, 0xFE4A5930));
		TRY(enc_80_major_49B330 (key, seed));
	}

	var_magic |= 0x482C59;
	KEY(0x10) = ROR(KEY(0x10), KEY(0x48) + var_magic);

	if (type == 2)
	{
		TRY(enc_80_4D57E0 (key, 0xE9FAF4B7));
		enc_80_4E5DA0 (key, 0xBCD76101);
		TRY(enc_80_major_4E5F90 (key, KEY(0x10)));
	}

	KEY(0x2C) += my_sin(seed) ? 0x44ACFBD : KEY(0x24);

	if (type == 9)
	{
		enc_80_4C5240 (key, 0x1B20454A);
		enc_80_4E8620 (key, 0xE0A4F875);
		TRY(enc_80_major_4EF430 (key, KEY(0x30)));
	}

	var_magic ^= 0x42CA85;
	seed = ROR(seed, seed < var_magic ? seed : KEY(0x44));

	if (type == 0)
	{
		enc_80_4C19E0 (key, 0xEEFC3B7A);
		TRY(enc_80_4DB3D0 (key, 0x2923012F));
		TRY(enc_80_major_4C52A0 (key, KEY(0x3C)));
	}

	var_magic -= 0x489D0D;
	KEY(0x40) *= var_magic ^ seed;

	if (type == 0)
	{
		enc_80_4EF400 (key, 0xB2A7AAD2);
		enc_80_4DB480 (key, 0xF0F77B83);
		TRY(enc_80_major_49E930 (key, KEY(0x14)));
	}

	KEY(0x10) += KEY(0x44) * 0xE9F05570;

	if (type == 5)
	{
		enc_80_4E8530 (key, 0x28701FEB);
		enc_80_4D79B0 (key, 0x26DCF794);
		TRY(enc_80_major_49ED30 (key, KEY(0x10)));
	}

	var_magic &= 0x4065EA;
	KEY(0x34) *= KEY(0x20) ^ var_magic;

	if (type == 7)
	{
		enc_80_4C5060 (key, 0x3BAE0AE1);
		enc_80_4E5EB0 (key, 0x1D1451DE);
		TRY(enc_80_major_4D2500 (key, KEY(0x4)));
	}

	if (type == 8)
	{
		enc_80_4C5060 (key, 0x41E6E51C);
		enc_80_4ECC90 (key, 0x13E30902);
		TRY(enc_80_major_4AE780 (key, KEY(0x2C)));
	}

	var_magic |= 0x4462F7;
	KEY(0x34) &= KEY(0x24) * var_magic;

	if (type == 0x0B)
	{
		enc_80_4C5240 (key, 0x641482BD);
		enc_80_4A1B50 (key, 0x46C97C9);
		enc_80_4C50E0 (key, KEY(0x34));
	}

	RETURN;
}

THROWS_RET enc_80_major_49B330 (u32 *key, u32 seed)
{
	u32 type = (KEY(0x24) ^ KEY(0x14) ^ KEY(0x18)) & 0x03;
	u32 var_magic = 0x1214C499;

	var_magic += 0x4D1304;
	KEY(0x30) += my_sqrt(KEY(0x04));
	var_magic -= 0x438C04;
	KEY(0x48) += KEY(0x4C) + 0x2B7FD413;

	if (type == 0)
	{
		enc_80_4C19B0 (key, 0xD7C23153);
		enc_80_4C5270 (key, 0xE28F3639);
		enc_80_4C50E0 (key, KEY(0x8));
	}

	seed = ROR(seed, KEY(0x10) - 0xA4E29DD);
	var_magic -= 0x4C74DD;
	seed ^= ROR(KEY(0x44), var_magic);

	if (type == 0)
	{
		TRY(enc_80_4D5760 (key, 0xC37795E2));
		enc_80_4C19E0 (key, 0x45B63F72);
		enc_80_49B630 (key, KEY(0x0C));
	}

	var_magic &= 0x4FF7E0;
	KEY(0x14) += ((seed * 27) << 1) - seed;
	KEY(0x48) += KEY(0x18) - 0x24A58A0;

	if (type == 3)
	{
		enc_80_4DB030 (key, 0xDDA667E);
		enc_80_4E8500 (key, 0x38209953);
		TRY(enc_80_major_4AC300 (key, KEY(0x48)));
	}

	var_magic ^= 0x4EC99D;
	KEY(0x28) &= my_sin(KEY(0x44)) ? var_magic : KEY(0x34);

	var_magic ^= 0x40354F;
	KEY(0x4C) |= my_sin(KEY(0x48)) ? var_magic : KEY(0x10);
	KEY(0x34) *= ROR(KEY(0x30), var_magic);

	if (type == 2)
	{
		TRY(enc_80_4EF3B0 (key, 0xFF98A941));
		enc_80_4A1B20 (key, 0xE13AFBA1);
		TRY(enc_80_major_4D7AE0 (key, KEY(0x8)));
	}

	var_magic |= 0x455BFA;
	KEY(0x18) += (KEY(0x04) << 6) + KEY(0x04);

	RETURN;
}


THROWS_RET enc_80_major_49E930 (u32 *key, u32 seed)
{
	u32 type = KEY(0x24) & 0x07;
	u32 var_magic = 0x64B961D1;

	var_magic -= 0x4A835A;
	seed -= var_magic & KEY(0x1C);

	if (type == 0)
	{
		enc_80_4B2CE0 (key, 0x553225DD);
		TRY(enc_80_4AC2B0 (key, 0x4DEF5392));
		enc_80_4C50E0 (key, KEY(0x1C));
	}

	var_magic &= 0x4C84C4;
	seed = ROR(seed, (seed * 59) << 1);

	if (type == 6)
	{
		enc_80_4C19E0 (key, 0x50161C22);
		enc_80_4DB000 (key, 0x1D050D83);
		enc_80_49B630 (key, KEY(0x10));
	}

	KEY(0x0C) += my_sin(seed) ? var_magic : KEY(0x20);

	if (type == 5)
	{
		enc_80_4E8530 (key, 0x3EB9AB52);
		enc_80_49B600 (key, 0x2AB2E7E2);
		TRY(enc_80_major_4AC300 (key, KEY(0x48)));
	}

	var_magic += 0x4D3285;
	KEY(0x08) = ROL(KEY(0x08), (((KEY(0x10) * 3) << 3) - KEY(0x10)) * 3);

	if (type == 3)
	{
		enc_80_4C5240 (key, 0x109F1113);
		enc_80_4E5EB0 (key, 0xD1FC123);
		TRY(enc_80_major_4D7AE0 (key, seed));
	}

	var_magic ^= 0x439DC1;
	KEY(0x00) += my_sin(KEY(0x10)) ? var_magic : seed;

	if (type == 2)
	{
		TRY(enc_80_4E5EE0 (key, 0xB4D46208));
		enc_80_4EF200 (key, 0x2F40BB4D);
		TRY(enc_80_major_49B330 (key, KEY(0x24)));
	}

	seed ^= seed * 0x11;

	if (type == 4)
	{
		enc_80_4E8680 (key, 0x8AA7909A);
		enc_80_4AECA0 (key, 0x85FDED7);
		TRY(enc_80_major_4E5F90 (key, KEY(0x28)));
	}

	var_magic |= 0x468D03;
	KEY(0x10) *= var_magic ^ KEY(0x08);

	if (type == 1)
	{
		enc_80_4ECCC0 (key, 0x1979981D);
		TRY(enc_80_4EF230 (key, 0x234F4D82));
		TRY(enc_80_major_4EF430 (key, seed));
	}

	var_magic |= 0x4A2982;
	KEY(0x24) = ((KEY(0x2C) * KEY(0x24) * 5) << 3) - KEY(0x2C) * KEY(0x24);
	KEY(0x04) += ((KEY(0x28) << 5) + KEY(0x28)) * 3;

	if (type == 0)
	{
		TRY(enc_80_4E5EE0 (key, 0xE5B87865));
		TRY(enc_80_4EF350 (key, 0x65B98EB3));
		TRY(enc_80_major_4C52A0 (key, KEY(0x00)));
	}

	RETURN;
}

THROWS_RET enc_80_major_49ED30 (u32 *key, u32 seed)
{
	u32 type = (KEY(0x10) ^ KEY(0x34) ^ KEY(0x44)) % 0x09;
	u32 var_magic = 0x16B6929D;
	
	var_magic -= 0x40E4F7;
	seed ^= ROR(seed, var_magic);

	if (type == 0)
	{
		TRY(enc_80_4AEC50 (key, 0x2816580B));
		enc_80_4B2CE0 (key, 0xCF0054E6);
		enc_80_4C50E0 (key, KEY(0x10));
	}

	var_magic &= 0x46C533;
	KEY(0x08) &= KEY(0x40) ^ var_magic;

	if (type == 8)
	{
		TRY(enc_80_4B2C60 (key, 0x31A2A97C));
		TRY(enc_80_4EF3B0 (key, 0x54B4093D));
		enc_80_49B630 (key, KEY(0x3C));
	}

	KEY(0x1C) &= my_sin(seed) ? 0x5228985F : KEY(0x14);

	if (type == 3)
	{
		TRY(enc_80_4DB3D0 (key, 0xBAFAEEE));
		enc_80_4A1B20 (key, 0x9312E9EA);
		TRY(enc_80_major_4AC300 (key, KEY(0x48)));
	}

	var_magic += 0x4F7544;
	seed *= KEY(0x0C) ^ var_magic;

	if (type == 7)
	{
		TRY(enc_80_4D57E0 (key, 0xB64F3C2B));
		enc_80_4EF280 (key, 0x142A13B1);
		TRY(enc_80_major_4D7AE0 (key, KEY(0x34)));
	}

	var_magic &= 0x43B25F;
	KEY(0x1C) += 0 - (((seed * 5) << 2) - seed);

	if (type == 4)
	{
		enc_80_4E5DA0 (key, 0x96D075FF);
		enc_80_4D79B0 (key, 0x302C41);
		TRY(enc_80_major_49B330 (key, KEY(0x34)));
	}

	KEY(0x30) = ROL(KEY(0x30), (((seed << 4) + seed) * 3) << 1);

	if (type == 6)
	{
		enc_80_4DB270 (key, 0xB554145);
		TRY(enc_80_4B2C60 (key, 0x19E465B6));
		TRY(enc_80_major_4E5F90 (key, KEY(0x14)));
	}

	var_magic -= 0x448E9C;
	KEY(0x4C) = ROL(KEY(0x4C), seed + 0x4685D5BE);

	if (type == 1)
	{
		TRY(enc_80_4DB090 (key, 0x30C99E0C));
		enc_80_4C19E0 (key, 0x81405E4);
		TRY(enc_80_major_4EF430 (key, KEY(0x38)));
	}

	var_magic |= 0x49DF33;
	KEY(0x14) += KEY(0x18) | 0x301DE279;

	if (type == 2)
	{
		enc_80_4ECC90 (key, 0x1CF6BE1A);
		enc_80_4E8530 (key, 0xC3C3534);
		TRY(enc_80_major_4C52A0 (key, KEY(0x44)));
	}

	KEY(0x18) *= KEY(0x00) ^ var_magic;

	if (type == 0)
	{
		enc_80_4ECCF0 (key, 0xC00205F0);
		enc_80_49B600 (key, 0x11B2399C);
		TRY(enc_80_major_49E930 (key, KEY(0x38)));
	}

	var_magic ^= 0x4C5428;
	KEY(0x44) |= my_sqrt(KEY(0x30));

	RETURN;
}

THROWS_RET enc_80_major_4A1BB0 (u32 *key, u32 seed)
{
	u32 type = KEY(0x24) & 0x0F;
	u32 var_magic = 0x4A19FE59;

	if (type == 0x0D)
	{
		enc_80_4E5D40 (key, 0x5E203202);
		enc_80_4AECA0 (key, 0x2942B196);
		TRY(enc_80_major_4D2500 (key, KEY(0x34)));
	}

	if (type == 2)
	{
		TRY(enc_80_4EF3B0 (key, 0x9FEDBF40));
		enc_80_4C19E0 (key, 0xC1527D22);
		TRY(enc_80_major_4AE780 (key, KEY(0x28)));
	}

	var_magic -= 0x499521;
	KEY(0x04) -= var_magic ^ KEY(0x1C);

	if (type == 6)
	{
		TRY(enc_80_4EF350 (key, 0x973D0841));
		enc_80_49B310 (key, 0x6FB848B1);
		TRY(enc_80_major_4F3220 (key, seed));
	}

	if (type == 0x0E)
	{
		enc_80_4DB000 (key, 0x3A1AC951);
		TRY(enc_80_4DB3D0 (key, 0x3CCA96B3));
		TRY(enc_80_major_4C1A00 (key, seed));
	}

	var_magic &= 0x4EF618;
	KEY(0x34) *= my_cos(seed) ? 0x46223265 : KEY(0x48);

	if (type == 6)
	{
		TRY(enc_80_4F31A0 (key, 0x13EDE9AD));
		TRY(enc_80_4D57E0 (key, 0x4E3DE42F));
		TRY(enc_80_major_4E86B0 (key, KEY(0x0C)));
	}

	if (type == 4)
	{
		enc_80_4D7980 (key, 0x65583F39);
		enc_80_4E5D40 (key, 0x781F3012);
		TRY(enc_80_major_4AC560 (key, seed));
	}

	KEY(0x18) += (seed * 3) << 3;

	if (type == 4)
	{
		TRY(enc_80_4AC2B0 (key, 0x4E2BAC7E));
		enc_80_4E8620 (key, 0x177D7BB6);
		TRY(enc_80_major_4BB410 (key, KEY(0x38)));
	}

	if (type == 2)
	{
		enc_80_4EF280 (key, 0x32FB8883);
		TRY(enc_80_4ECBE0 (key, 0x6CDE15FE));
		TRY(enc_80_major_4B2FD0 (key, KEY(0x00)));
	}

	var_magic |= 0x479E9B;
	seed ^= my_sin(KEY(0x2C)) ? 0x2F24FB19 : KEY(0x40);

	if (type == 0x0A)
	{
		enc_80_4C19E0 (key, 0x505E448C);
		enc_80_49ECD0 (key, 0x35CC377C);
		TRY(enc_80_major_4D2900 (key, KEY(0x1C)));
	}

	if (type == 0x0B)
	{
		enc_80_4DB240 (key, 0x1D24B37E);
		enc_80_4DB270 (key, 0xEBCD1E05);
		TRY(enc_80_major_4CFE70 (key, KEY(0x24)));
	}

	if (type == 5)
	{
		enc_80_4DB480 (key, 0xFDB8F3CF);
		enc_80_4AECA0 (key, 0x78A78E8);
		TRY(enc_80_major_4ECD20 (key, seed));
	}

	var_magic += 0x4AC738;
	KEY(0x48) *= KEY(0x08) + var_magic;

	if (type == 1)
	{
		enc_80_4A1B80 (key, 0xB2BF1EF);
		enc_80_4E85C0 (key, 0x1E9F3C2C);
		TRY(enc_80_major_4DB520 (key, KEY(0x1C)));
	}

	if (type == 5)
	{
		TRY(enc_80_4D7A40 (key, 0x36140218));
		enc_80_4E5E60 (key, 0x4883E81C);
		TRY(enc_80_major_4D2500 (key, seed));
	}

	KEY(0x18) -= KEY(0x20) & 0x42D2AFE6;

	if (type == 0)
	{
		enc_80_4D7980 (key, 0x4E54AFAF);
		TRY(enc_80_4EF350 (key, 0x5E99A42B));
		TRY(enc_80_major_4AE780 (key, KEY(0x24)));
	}

	if (type == 7)
	{
		enc_80_4ECC60 (key, 0x7DADF492);
		enc_80_4E85C0 (key, 0x203B3608);
		TRY(enc_80_major_4F3220 (key, KEY(0x0C)));
	}

	var_magic += 0x40BAF2;
	KEY(0x38) = ROR(KEY(0x38), KEY(0x0C) + 0x44060020);

	if (type == 3)
	{
		enc_80_4A1B80 (key, 0x193B54D7);
		enc_80_4EF400 (key, 0xD855D7F);
		TRY(enc_80_major_4C1A00 (key, KEY(0x8)));
	}

	if (type == 9)
	{
		enc_80_4C19B0 (key, 0xF8EE4D07);
		enc_80_4DB000 (key, 0x2B886022);
		TRY(enc_80_major_4E86B0 (key, seed));
	}

	var_magic |= 0x45836C;
	KEY(0x2C) &= seed ^ 0x26649BD;

	if (type == 0x0C)
	{
		enc_80_4C5270 (key, 0xBCE0098D);
		enc_80_4C19E0 (key, 0x51285563);
		TRY(enc_80_major_4AC560 (key, seed));
	}

	if (type == 0x0F)
	{
		enc_80_4A1B20 (key, 0x3B290E5);
		enc_80_49B310 (key, 0x51504261);
		TRY(enc_80_major_4BB410 (key, KEY(0x44)));
	}

	KEY(0x0C) = ROR(KEY(0x0C), var_magic & KEY(0x2C));

	if (type == 1)
	{
		TRY(enc_80_4D57E0 (key, 0x945B5EA3));
		enc_80_4ECC90 (key, 0x28CDD746);
		TRY(enc_80_major_4B2FD0 (key, KEY(0x34)));
	}

	if (type == 8)
	{
		enc_80_4ECC90 (key, 0x38F1FD0B);
		enc_80_4C19E0 (key, 0xCD12217B);
		TRY(enc_80_major_4D2900 (key, KEY(0x2C)));
	}

	if (type == 0)
	{
		TRY(enc_80_4DB090 (key, 0x425A78C1));
		enc_80_4A1B80 (key, 0x462E367);
		TRY(enc_80_major_4CFE70 (key, seed));
	}

	var_magic |= 0x495B98;
	KEY(0x28) += my_cos(KEY(0x00)) ? 0xF1B21FE : seed;

	if (type == 3)
	{
		enc_80_4E5DD0 (key, 0x5B545418);
		TRY(enc_80_4B2C60 (key, 0x1B747BD4));
		TRY(enc_80_major_4ECD20 (key, KEY(0x4)));
	}

	var_magic -= 0x4BD9D9;
	KEY(0x4C) *= KEY(0x48) * 73;

	RETURN;
}

THROWS_RET enc_80_major_4AC300 (u32 *key, u32 seed)
{
	u32 type = (KEY(0x1C) ^ KEY(0x0C) ^ seed) & 0x01;
	u32 var_magic = 0x71A82B0;

	var_magic |= 0x4E6D00;
	KEY(0x28) &= var_magic ^ seed;
	
	var_magic -= 0x4AEEF9;
	KEY(0x2C) ^= my_cos(KEY(0x04)) ? 0x2E0A5BE7 : KEY(0x18);
	KEY(0x00) += seed + 0x1FE76B44;

	var_magic += 0x4D7317;
	KEY(0x34) += var_magic - seed;

	if (type == 1)
	{
		enc_80_4E5EB0 (key, 0xCD17011);
		enc_80_4D79E0 (key, 0xFAFEF17E);
		enc_80_4C50E0 (key, KEY(0x0C));
	}

	var_magic += 0x488687;
	KEY(0x40) -= KEY(0x2C) < 0x56C0185B ? KEY(0x2C) : KEY(0x4C);
	KEY(0x00) ^= my_sin(KEY(0x40)) ? 0x5A271260 : seed;

	var_magic -= 0x4D591C;
	KEY(0x18) += KEY(0x2C) | 0x58E035D2;

	var_magic -= 0x432296;
	KEY(0x38) = ROL(KEY(0x38), ROL(KEY(0x04), 0xC8));
	KEY(0x34) ^= KEY(0x3C) * 91;

	if (type == 0)
	{
		enc_80_4EF200 (key, 0x77783699);
		TRY(enc_80_4AC2B0 (key, 0x8D6B4CEA));
		enc_80_49B630 (key, KEY(0x38));
	}

	RETURN;
}

THROWS_RET enc_80_major_4AC560 (u32 *key, u32 seed)
{
	u32 type = KEY(0x38) % 0x0F;
	u32 var_magic = 0xAE0D0C5;

	if (type == 0x0A)
	{
		enc_80_4E5DD0 (key, 0xBEABFC6);
		enc_80_4D7A10 (key, 0x6297E786);
		TRY(enc_80_major_4D7AE0 (key, KEY(0x4C)));
	}

	var_magic -= 0x4D4B8C;
	KEY(0x2C) = ROR(KEY(0x2C), var_magic & KEY(0x14));

	if (type == 3)
	{
		enc_80_4E5D40 (key, 0x4AC2DD72);
		TRY(enc_80_4C5090 (key, 0x7643972C));
		TRY(enc_80_major_49B330 (key, seed));
	}

	if (type == 0)
	{
		enc_80_49ECD0 (key, 0x17737461);
		TRY(enc_80_4D5760 (key, 0xF069FB95));
		TRY(enc_80_major_4E5F90 (key, KEY(0x30)));
	}

	var_magic += 0x4C8417;
	KEY(0x1C) *= my_cos(KEY(0x10)) ? 0xA45B84A : KEY(0x34);

	if (type == 4)
	{
		TRY(enc_80_4ECBE0 (key, 0x2DCB251));
		enc_80_4D57B0 (key, 0x60FAED63);
		TRY(enc_80_major_4EF430 (key, KEY(0x00)));
	}

	seed |= my_sin(seed) ? var_magic : KEY(0x28);

	if (type == 0x0D)
	{
		TRY(enc_80_4C5090 (key, 0x4611946));
		enc_80_4EF300 (key, 0xFA9EC86C);
		TRY(enc_80_major_4C52A0 (key, KEY(0x1C)));
	}

	if (type == 0)
	{
		enc_80_4ECCF0 (key, 0xA62631C9);
		enc_80_49ECD0 (key, 0x397DA4E9);
		TRY(enc_80_major_49E930 (key, seed));
	}

	var_magic |= 0x4E808D;
	KEY(0x3C) += KEY(0x34) + 0x519A438A;

	if (type == 1)
	{
		TRY(enc_80_4AC2B0 (key, 0x4042FF18));
		enc_80_4C5060 (key, 0xB3A258E);
		TRY(enc_80_major_49ED30 (key, KEY(0x10)));
	}

	if (type == 2)
	{
		enc_80_4ECCF0 (key, 0x82D23A86);
		enc_80_4D7980 (key, 0x56DF19F6);
		TRY(enc_80_major_4D2500 (key, KEY(0x3C)));
	}

	var_magic ^= 0x4885D9;
	KEY(0x24) -= my_cos(seed) ? var_magic : KEY(0x3C);

	if (type == 0x0C)
	{
		enc_80_4ECC60 (key, 0xFCE8B47F);
		enc_80_4E8590 (key, 0x12DD985C);
		TRY(enc_80_major_4AE780 (key, KEY(0x1C)));
	}

	if (type == 5)
	{
		TRY(enc_80_4AC2B0 (key, 0x47C80724));
		enc_80_4E5D40 (key, 0x5677CA06);
		TRY(enc_80_major_4F3220 (key, KEY(0x3C)));
	}

	KEY(0x20) += ROR(KEY(0x4C), var_magic);

	if (type == 0x0B)
	{
		enc_80_49B310 (key, 0x86F4A013);
		enc_80_4B2CE0 (key, 0x29C236F1);
		TRY(enc_80_major_4C1A00 (key, KEY(0x10)));
	}

	var_magic += 0x4FF798;
	KEY(0x1C) -= my_sin(KEY(0x14)) ? var_magic : KEY(0x30);

	if (type == 6)
	{
		enc_80_49ECD0 (key, 0x5E2C3E1F);
		enc_80_4E5E00 (key, 0xB27F04F);
		TRY(enc_80_major_4E86B0 (key, seed));
	}

	if (type == 0x0E)
	{
		enc_80_4D79E0 (key, 0x670094B);
		TRY(enc_80_4DB3D0 (key, 0xF82CAD07));
		TRY(enc_80_major_4D7AE0 (key, seed));
	}

	var_magic &= 0x473790;
	KEY(0x14) *= my_cos(seed) ? 0x5DF8323 : seed;

	if (type == 8)
	{
		enc_80_4DB480 (key, 0x1CAA72D);
		enc_80_4E8590 (key, 0x2ED240F8);
		TRY(enc_80_major_49B330 (key, seed));
	}

	if (type == 9)
	{
		enc_80_4E5D40 (key, 0x3A4CFAB1);
		enc_80_4D7980 (key, 0x26F982C7);
		TRY(enc_80_major_4E5F90 (key, KEY(0x38)));
	}

	KEY(0x1C) ^= KEY(0x3C) ^ var_magic;

	if (type == 7)
	{
		enc_80_4ECCC0 (key, 0x822F564);
		enc_80_4D57B0 (key, 0x4E48EE4D);
		TRY(enc_80_major_4EF430 (key, seed));
	}

	var_magic &= 0x423C0D;
	KEY(0x4C) ^= ROL(KEY(0x4C), var_magic);

	RETURN;
}

THROWS_RET enc_80_major_4AE780 (u32 *key, u32 seed)
{
	u32 type = KEY(0x3C) % 0x0B;
	u32 var_magic = 0xB4D36;
	
	if (type == 5)
	{
		TRY(enc_80_4D5830 (key, 0x50FEB0CF));
		enc_80_4EF2A0 (key, 0xFED8AB55);
		enc_80_4C50E0 (key, KEY(0x34));
	}

	var_magic -= 0x475C26;
	KEY(0x10) -= my_cos(KEY(0x38)) ? 0x76F737A3 : seed;

	if (type == 0x0A)
	{
		enc_80_4C5240 (key, 0x4CEEE050);
		enc_80_4E8620 (key, 0xEA2E6DBA);
		enc_80_49B630 (key, seed);
	}

	var_magic -= 0x4CE4BB;
	KEY(0x0C) &= KEY(0x28) ^ 0x20773F85;

	if (type == 4)
	{
		enc_80_4D79B0 (key, 0x7B8E385);
		enc_80_4ECC90 (key, 0x1A6B248E);
		TRY(enc_80_major_4AC300 (key, KEY(0x38)));
	}

	KEY(0x30) = ROL(KEY(0x30), KEY(0x1C) * 91);

	if (type == 9)
	{
		enc_80_4B2CB0 (key, 0xE1B47DED);
		enc_80_4DB2E0 (key, 0xA73FCF8F);
		TRY(enc_80_major_4D7AE0 (key, seed));
	}

	var_magic += 0x467B06;
	seed += 0x1FB9BCB1- KEY(0x08);

	if (type == 2)
	{
		enc_80_4E5E00 (key, 0xE8B01B9B);
		enc_80_4E5D70 (key, 0x5434B47);
		TRY(enc_80_major_49B330 (key, KEY(0x20)));
	}

	var_magic |= 0x44CA0D;
	seed *= seed;
	KEY(0x0C) += ROL(KEY(0x2C), 0x1D);

	if (type == 6)
	{
		enc_80_49ECD0 (key, 0x6064A99B);
		enc_80_4ECCF0 (key, 0x4132071F);
		TRY(enc_80_major_4E5F90 (key, KEY(0x00)));
	}

	seed ^= my_cos(seed) ? 0x30851F11 : KEY(0x28);

	if (type == 8)
	{
		enc_80_4E8500 (key, 0x8E0D3DC1);
		enc_80_4A1B50 (key, 0x54854D39);
		TRY(enc_80_major_4EF430 (key, KEY(0x30)));
	}

	var_magic ^= 0x428D8D;
	KEY(0x24) *= KEY(0x00) < 0xED7837 ? KEY(0x00) : KEY(0x3C);

	if (type == 3)
	{
		TRY(enc_80_4AEC50 (key, 0x19068E93));
		enc_80_4ECCC0 (key, 0x14181C79);
		TRY(enc_80_major_4C52A0 (key, KEY(0x3C)));
	}

	var_magic ^= 0x48393D;
	seed = ROL(seed, var_magic ^ seed);

	if (type == 0)
	{
		enc_80_4B2CB0 (key, 0x6F32EC9E);
		enc_80_4D7940 (key, 0xC040D97F);
		TRY(enc_80_major_49E930 (key, KEY(0x00)));
	}

	KEY(0x40) ^= KEY(0x18) ^ 0x28AA2736;

	if (type == 1)
	{
		enc_80_4EF400 (key, 0x182AA887);
		enc_80_4B2CB0 (key, 0xE9844061);
		TRY(enc_80_major_49ED30 (key, KEY(0x8)));
	}

	if (type == 0)
	{
		TRY(enc_80_4D7A90 (key, 0x153CACBE));
		enc_80_4D79E0 (key, 0x14718854);
		TRY(enc_80_major_4D2500 (key, KEY(0x18)));
	}

	var_magic ^= 0x4B27FE;
	KEY(0x08) ^= KEY(0x4C) * 101;

	if (type == 7)
	{
		enc_80_49B600 (key, 0x2A4F7758);
		enc_80_4C19B0 (key, 0xC0F12DEF);
		enc_80_4C50E0 (key, KEY(0x1C));
	}

	var_magic ^= 0x4EEA25;
	KEY(0x00) &= my_cos(KEY(0x00)) ? var_magic : KEY(0x14);

	RETURN;
}


THROWS_RET enc_80_major_4B2FD0 (u32 *key, u32 seed)
{
	u32 type = (KEY(0x04) ^ KEY(0x40) ^ KEY(0x48)) & 0x0F;
	u32 var_magic = 0xF145B6E;

	if (type == 1)
	{
		enc_80_4E5E60 (key, 0x695C49D0);
		enc_80_4E8500 (key, 0x22072F2B);
		TRY(enc_80_major_4E5F90 (key, KEY(0x0C)));
	}

	var_magic |= 0x43899E;
	KEY(0x14) += 0x37C9C771 - KEY(0x48);

	if (type == 0)
	{
		enc_80_4E5F30 (key, 0x4798ED37);
		enc_80_4C19B0 (key, 0xDBB8DC69);
		TRY(enc_80_major_4EF430 (key, KEY(0x4)));
	}

	if (type == 8)
	{
		enc_80_4EF300 (key, 0xCF0BFF8);
		TRY(enc_80_4D7A40 (key, 0x4751863F));
		TRY(enc_80_major_4C52A0 (key, KEY(0x1C)));
	}

	var_magic -= 0x475639;
	KEY(0x00) -= my_sin(seed) ? 0xE2D0D9 : seed;

	if (type == 7)
	{
		enc_80_4EF280 (key, 0x6609D8FC);
		TRY(enc_80_4D7A90 (key, 0x10E095B7));
		TRY(enc_80_major_49E930 (key, KEY(0x00)));
	}

	if (type == 6)
	{
		enc_80_4C5060 (key, 0x113E3AD2);
		TRY(enc_80_4EF350 (key, 0x6272A245));
		TRY(enc_80_major_49ED30 (key, KEY(0x00)));
	}

	KEY(0x2C) = ROR(KEY(0x2C), my_cos(seed) ? var_magic : KEY(0x2C));

	if (type == 1)
	{
		enc_80_4E8500 (key, 0xEA082B69);
		enc_80_4E5EB0 (key, 0x2CDA662B);
		TRY(enc_80_major_4D2500 (key, KEY(0x0C)));
	}

	if (type == 0x0F)
	{
		TRY(enc_80_4D7A90 (key, 0xD3363518));
		enc_80_4C5270 (key, 0xC237392);
		TRY(enc_80_major_4AE780 (key, KEY(0x48)));
	}

	var_magic ^= 0x4DB5DD;
	KEY(0x44) -= ROL(KEY(0x2C), var_magic);

	if (type == 2)
	{
		enc_80_49ECD0 (key, 0x339DBC7B);
		enc_80_4E85C0 (key, 0x56A1533);
		TRY(enc_80_major_4F3220 (key, KEY(0x44)));
	}

	if (type == 9)
	{
		enc_80_49ECD0 (key, 0x4ABD70C6);
		enc_80_49B280 (key, 0x75C1A78D);
		TRY(enc_80_major_4C1A00 (key, seed));
	}

	var_magic -= 0x4C4FE4;
	seed |= my_sin(KEY(0x20)) ? var_magic : KEY(0x8);

	if (type == 0x0A)
	{
		enc_80_49ECD0 (key, 0x2BFD5B87);
		enc_80_4EF280 (key, 0x27F49EA2);
		TRY(enc_80_major_4E86B0 (key, KEY(0x14)));
	}

	if (type == 0x0E)
	{
		TRY(enc_80_4DB3D0 (key, 0xF5D594C2));
		enc_80_4C5240 (key, 0x12F8065E);
		TRY(enc_80_major_4AC560 (key, KEY(0x00)));
	}

	seed ^= KEY(0x0C) & var_magic;

	if (type == 0x0C)
	{
		TRY(enc_80_4AC2B0 (key, 0x42F02A72));
		enc_80_4C5060 (key, 0x10CD847E);
		TRY(enc_80_major_4BB410 (key, KEY(0x48)));
	}

	if (type == 3)
	{
		enc_80_4C5270 (key, 0xF9A117B2);
		enc_80_4A1B20 (key, 0xEDBBBAEC);
		TRY(enc_80_major_4E5F90 (key, KEY(0x30)));
	}

	var_magic &= 0x4C374B;
	KEY(0x38) ^= KEY(0x10) | 0xC893A6;

	if (type == 0x0B)
	{
		enc_80_4D7940 (key, 0xB1A7309D);
		enc_80_4A1B20 (key, 0x1FAC3747);
		TRY(enc_80_major_4EF430 (key, KEY(0x30)));
	}

	if (type == 0)
	{
		TRY(enc_80_4AEC50 (key, 0x398C59E0));
		TRY(enc_80_4D5760 (key, 0xFC26EBC7));
		TRY(enc_80_major_4C52A0 (key, KEY(0x3C)));
	}

	var_magic += 0x4D352E;
	KEY(0x4C) ^= my_cos(KEY(0x38)) ? var_magic : KEY(0x28);

	if (type == 0x0D)
	{
		enc_80_4A1B80 (key, 0x15327396);
		enc_80_4DB000 (key, 0x15091EA6);
		TRY(enc_80_major_49E930 (key, KEY(0x8)));
	}

	KEY(0x34) = ((KEY(0x34) * 27) << 1) - KEY(0x34);

	if (type == 4)
	{
		TRY(enc_80_4B2BC0 (key, 0xD48681AE));
		enc_80_4DB000 (key, 0x19716A46);
		TRY(enc_80_major_49ED30 (key, KEY(0x18)));
	}

	if (type == 5)
	{
		TRY(enc_80_4E5EE0 (key, 0x89B618E));
		enc_80_4E8500 (key, 0xCC56F70A);
		TRY(enc_80_major_4D2500 (key, KEY(0x40)));
	}

	var_magic |= 0x45607A;
	KEY(0x20) ^= KEY(0x28) + var_magic;

	RETURN;
}

THROWS_RET enc_80_major_4BB410 (u32 *key, u32 seed)
{
	u32 type = (KEY(0x30) ^ KEY(0x08) ^ KEY(0x4C)) & 0x0F;
	u32 var_magic = 0x2D45C061;

	if (type == 0x0F)
	{
		TRY(enc_80_4DB3D0 (key, 0xF0B688AB));
		enc_80_4ECC60 (key, 0xF453954D);
		TRY(enc_80_major_49B330 (key, KEY(0x4C)));
	}

	var_magic |= 0x4AEECD;
	KEY(0x0C) += ((seed * 3) << 4) + seed;

	if (type == 9)
	{
		TRY(enc_80_49E8E0 (key, 0x4AE8ED5A));
		enc_80_4E5F30 (key, 0x497A4274);
		TRY(enc_80_major_4E5F90 (key, seed));
	}

	if (type == 3)
	{
		enc_80_4E5D40 (key, 0x5F346943);
		enc_80_4D57B0 (key, 0x5DB2BC1B);
		TRY(enc_80_major_4EF430 (key, seed));
	}

	var_magic &= 0x4C2BC6;
	KEY(0x24) -= KEY(0x08) & 0x8742C2;

	if (type == 6)
	{
		TRY(enc_80_4EF3B0 (key, 0xDF797762));
		enc_80_4E5E00 (key, 0xE37DF00A);
		TRY(enc_80_major_4C52A0 (key, seed));
	}

	seed ^= KEY(0x18) < var_magic ? KEY(0x18) : KEY(0x14);

	if (type == 0x0C)
	{
		enc_80_4EF280 (key, 0xF579E970);
		enc_80_4D79E0 (key, 0x454B31AB);
		TRY(enc_80_major_49E930 (key, seed));
	}

	if (type == 0x0E)
	{
		enc_80_4EF200 (key, 0x39259610);
		enc_80_4E5F30 (key, 0x76E8EB9F);
		enc_80_major_49ED30 (key, seed);
	}

	var_magic ^= 0x42FD8B;
	KEY(0x20) ^= my_sqrt(KEY(0x44));

	if (type == 4)
	{
		TRY(enc_80_4B2BC0 (key, 0xCAD836AF));
		enc_80_4ECCC0 (key, 0xF43AA4E);
		TRY(enc_80_major_4D2500 (key, KEY(0x40)));
	}

	var_magic ^= 0x47AF50;
	KEY(0x04) += 0x904F9E8D - KEY(0x14);

	if (type == 2)
	{
		TRY(enc_80_4DB3D0 (key, 0xEC6EB312));
		TRY(enc_80_4B2C60 (key, 0x51874720));
		TRY(enc_80_major_4AE780 (key, KEY(0x8)));
	}

	if (type == 0)
	{
		enc_80_4A1B20 (key, 0xBDE6C765);
		enc_80_4B2CE0 (key, 0x37730ECF);
		TRY(enc_80_major_4F3220 (key, KEY(0x44)));
	}

	KEY(0x14) += KEY(0x18) - var_magic;

	if (type == 5)
	{
		TRY(enc_80_4ECBE0 (key, 0x4B1E5360));
		enc_80_4EF2A0 (key, 0x400787D6);
		TRY(enc_80_major_4C1A00 (key, KEY(0x20)));
	}

	if (type == 0x0D)
	{
		enc_80_4C5270 (key, 0xE926E21);
		TRY(enc_80_49E8E0 (key, 0x73D5887D));
		TRY(enc_80_major_4E86B0 (key, seed));
	}

	var_magic ^= 0x47F906;
	KEY(0x00) ^= KEY(0x18) + 0x356DCF35;

	if (type == 1)
	{
		TRY(enc_80_4B2C60 (key, 0x31858645));
		enc_80_4E8500 (key, 0xCA4F0A68);
		TRY(enc_80_major_4AC560 (key, KEY(0x00)));
	}

	var_magic ^= 0x458008;
	KEY(0x20) ^= ROL(KEY(0x20), 0x1D);

	if (type == 0x0B)
	{
		enc_80_4EF2A0 (key, 0xFC96B0AF);
		enc_80_4ECC60 (key, 0xFCA3153B);
		TRY(enc_80_major_49B330 (key, KEY(0x24)));
	}

	if (type == 8)
	{
		enc_80_4C5240 (key, 0x380E354);
		enc_80_4A1B20 (key, 0xED213C2A);
		TRY(enc_80_major_4E5F90 (key, seed));
	}

	KEY(0x4C) |= KEY(0x10) * 73;

	if (type == 0x0A)
	{
		enc_80_4ECC60 (key, 0x4BE2174);
		TRY(enc_80_4DB3D0 (key, 0xEC89D1F3));
		TRY(enc_80_major_4EF430 (key, KEY(0x8)));
	}

	var_magic ^= 0x40899C;
	KEY(0x2C) += var_magic - KEY(0x28);

	if (type == 7)
	{
		enc_80_4ECCF0 (key, 0x9AD60B24);
		enc_80_4ECC60 (key, 0xB98A2F);
		TRY(enc_80_major_4C52A0 (key, seed));
	}

	if (type == 0)
	{
		TRY(enc_80_4D5830 (key, 0xC26C0CF));
		TRY(enc_80_4C5090 (key, 0x14496A43));
		TRY(enc_80_major_49E930 (key, KEY(0x4C)));
	}

	var_magic &= 0x44CA08;
	KEY(0x44) ^= (KEY(0x0C) * 5) << 3;

	RETURN;
}

THROWS_RET enc_80_major_4C1A00 (u32 *key, u32 seed)
{
	u32 type = (KEY(0x00) ^ KEY(0x0C) ^ seed) % 0x0D;
	u32 var_magic = 0x1295CAC6;

	if (type == 0)
	{
		enc_80_4C19E0 (key, 0xF43C0967);
		enc_80_4A1B80 (key, 0xC6396A22);
		enc_80_49B630 (key, KEY(0x48));
	}

	var_magic -= 0x4D6248;
	KEY(0x48) *= seed * 25;

	if (type == 1)
	{
		TRY(enc_80_4D7A90 (key, 0xCC24E346));
		enc_80_4E8680 (key, 0x68A46DDD);
		TRY(enc_80_major_4AC300 (key, KEY(0x4C)));
	}

	var_magic += 0x4403AB;
	KEY(0x30) &= KEY(0x20) ^ 0x3038E4EB;

	if (type == 3)
	{
		enc_80_4E8530 (key, 0x4196599);
		TRY(enc_80_4ECBE0 (key, 0x3DDD096E));
		TRY(enc_80_major_4D7AE0 (key, KEY(0x18)));
	}

	if (type == 6)
	{
		TRY(enc_80_4ECBE0 (key, 0x606621E0));
		enc_80_4ECC60 (key, 0x856DAAE3);
		TRY(enc_80_major_49B330 (key, KEY(0x30)));
	}

	seed ^= ROR(KEY(0x00), var_magic);

	if (type == 2)
	{
		enc_80_4EF2A0 (key, 0xECAB3260);
		enc_80_4DB2E0 (key, 0x1DC6F5B9);
		TRY(enc_80_major_4E5F90 (key, KEY(0x00)));
	}

	var_magic |= 0x4E9FE4;
	KEY(0x20) -= ROR(KEY(0x18), 0xBC);

	if (type == 0x0B)
	{
		TRY(enc_80_4AC2B0 (key, 0x5548FD79));
		TRY(enc_80_4D5830 (key, 0xEBE7C7B));
		TRY(enc_80_major_4EF430 (key, KEY(0x20)));
	}

	if (type == 8)
	{
		enc_80_4C5240 (key, 0x2ED739C1);
		TRY(enc_80_4DB090 (key, 0x29536264));
		TRY(enc_80_major_4C52A0 (key, seed));
	}

	var_magic ^= 0x467CC0;
	KEY(0x44) ^= KEY(0x08) + 0x187F79E8;

	if (type == 0)
	{
		enc_80_4DB030 (key, 0x32179577);
		TRY(enc_80_4D7A90 (key, 0xFCDDF393));
		TRY(enc_80_major_49E930 (key, KEY(0x30)));
	}

	KEY(0x34) ^= (seed * 25) << 2;

	if (type == 9)
	{
		TRY(enc_80_4AC2B0 (key, 0x48D2F6A4));
		enc_80_49B280 (key, 0x6C7C0586);
		TRY(enc_80_major_49ED30 (key, KEY(0x0C)));
	}

	if (type == 0x0A)
	{
		TRY(enc_80_4E5EE0 (key, 0xED896657));
		enc_80_4EF280 (key, 0xC2B1F58);
		TRY(enc_80_major_4D2500 (key, KEY(0x24)));
	}

	var_magic ^= 0x483F36;
	KEY(0x20) = ROR(KEY(0x20), KEY(0x44) - var_magic);

	if (type == 0x0C)
	{
		TRY(enc_80_4DB090 (key, 0x37807C23));
		TRY(enc_80_4EF3B0 (key, 0x61623FEE));
		TRY(enc_80_major_4AE780 (key, KEY(0x48)));
	}

	var_magic &= 0x43D4A6;
	KEY(0x2C) ^= KEY(0x30) - var_magic;

	if (type == 7)
	{
		enc_80_4E5D40 (key, 0x42DD6B4D);
		enc_80_4E8590 (key, 0x3AA5B40C);
		TRY(enc_80_major_4F3220 (key, seed));
	}

	KEY(0x10) = ROR(KEY(0x10), KEY(0x18) - var_magic);

	if (type == 5)
	{
		enc_80_4D57B0 (key, 0xAA1E999C);
		enc_80_49B600 (key, 0x52374450);
		enc_80_49B630 (key, KEY(0x4C));
	}

	if (type == 4)
	{
		enc_80_4EF280 (key, 0x29B6CBD2);
		enc_80_4EF300 (key, 0xCC69C7B);
		TRY(enc_80_major_4AC300 (key, KEY(0x4C)));
	}

	var_magic &= 0x496BD4;
	KEY(0x0C) ^= KEY(0x18) - var_magic;

	RETURN;
}

THROWS_RET enc_80_major_4C52A0 (u32 *key, u32 seed)
{
	u32 type = KEY(0x14) % 0x07;
	u32 var_magic = 0x24366FFF;

	var_magic ^= 0x41D20B;
	KEY(0x08) -= seed ^ 0x1E9E3A1C;

	if (type == 4)
	{
		enc_80_4B2CE0 (key, 0xCDA38D04);
		enc_80_4D79E0 (key, 0x3983248);
		enc_80_4C50E0 (key, KEY(0x34));
	}

	var_magic &= 0x4EB16A;
	KEY(0x24) &= KEY(0x10) ^ var_magic;

	if (type == 5)
	{
		enc_80_4C5270 (key, 0xFB335E13);
		enc_80_4EF2A0 (key, 0x1F2411CD);
		enc_80_49B630 (key, KEY(0x2C));
	}

	seed += KEY(0x44) ^ var_magic;

	if (type == 1)
	{
		TRY(enc_80_4B2BC0 (key, 0xC6D7E2F7));
		enc_80_49B2E0 (key, 0x66062DB5);
		TRY(enc_80_major_4AC300 (key, seed));
	}

	var_magic &= 0x4E33E9;
	KEY(0x34) |= my_sqrt(KEY(0x4C));

	var_magic += 0x49D53B;
	KEY(0x1C) -= my_cos(KEY(0x28)) ? var_magic : KEY(0x44);

	if (type == 0)
	{
		TRY(enc_80_4D5760 (key, 0xFE4CF79F));
		enc_80_4D7940 (key, 0xBAD669FF);
		TRY(enc_80_major_4D7AE0 (key, KEY(0x1C)));
	}

	KEY(0x3C) -= var_magic | seed;

	if (type == 6)
	{
		enc_80_4C19B0 (key, 0xD1A4DF47);
		enc_80_4E8500 (key, 0xCE673B32);
		TRY(enc_80_major_49B330 (key, KEY(0x4)));
	}

	var_magic ^= 0x4E7D6C;
	seed += KEY(0x18) & 0x44782589;

	if (type == 3)
	{
		enc_80_4E5D40 (key, 0x6CEA05F6);
		TRY(enc_80_4D5760 (key, 0xF81058F1));
		TRY(enc_80_major_4E5F90 (key, seed));
	}

	var_magic += 0x45642F;
	KEY(0x20) += KEY(0x2C) & 0x2645009B;
	seed = ROR(seed, KEY(0x00) < var_magic ? KEY(0x00) : KEY(0x38));

	if (type == 0)
	{
		enc_80_4E8500 (key, 0xC827DEB1);
		enc_80_4E85C0 (key, 0x14A73A7A);
		TRY(enc_80_major_4EF430 (key, KEY(0x28)));
	}

	var_magic &= 0x463769;
	KEY(0x20) *= KEY(0x40) ^ 0x2666A4CB;

	RETURN;
}

THROWS_RET enc_80_major_4CFE70 (u32 *key, u32 seed)
{
	u32 type = (KEY(0x08) ^ KEY(0x18) ^ seed) & 0x0F;
	u32 var_magic = 0xCECA967;

	if (type == 4)
	{
		enc_80_4DB270 (key, 0x21186FAA);
		enc_80_4C5270 (key, 0xEA423E68);
		TRY(enc_80_major_4C52A0 (key, KEY(0x18)));
	}

	if (type == 0x0A)
	{
		enc_80_49B310 (key, 0x24A2D81A);
		enc_80_4DB270 (key, 0x4AFBD7FD);
		TRY(enc_80_major_49E930 (key, KEY(0x00)));
	}

	var_magic ^= 0x4A0FC2;
	KEY(0x3C) += ROR(KEY(0x40), var_magic);

	if (type == 2)
	{
		enc_80_4D7A10 (key, 0x4C4FEE5F);
		enc_80_49ECD0 (key, 0x2157DCB7);
		TRY(enc_80_major_49ED30 (key, KEY(0x10)));
	}

	if (type == 9)
	{
		enc_80_4EF200 (key, 0x659B54D7);
		enc_80_4DB240 (key, 0x3B0726EB);
		TRY(enc_80_major_4D2500 (key, seed));
	}

	var_magic -= 0x49B1E7;
	seed = (KEY(0x0C) * seed * 19) << 1;

	if (type == 8)
	{
		enc_80_4C19B0 (key, 0xC5E5AFCD);
		enc_80_4DB030 (key, 0x1DD18054);
		TRY(enc_80_major_4AE780 (key, KEY(0x20)));
	}

	if (type == 3)
	{
		enc_80_4EF280 (key, 0x15993072);
		enc_80_4EF2A0 (key, 0xEEBBE15F);
		TRY(enc_80_major_4F3220 (key, KEY(0x38)));
	}

	KEY(0x2C) *= seed + 0x19948E09;

	if (type == 3)
	{
		enc_80_4D7A10 (key, 0x6595C89C);
		enc_80_4DB2E0 (key, 0x790D280);
		TRY(enc_80_major_4C1A00 (key, seed));
	}

	if (type == 0x0E)
	{
		TRY(enc_80_49E8E0 (key, 0x1B85EB94));
		TRY(enc_80_49E8E0 (key, 0xA3F69A0B));
		TRY(enc_80_major_4E86B0 (key, seed));
	}

	var_magic |= 0x4EA584;
	KEY(0x08) = ROR(KEY(0x08), seed - var_magic);

	if (type == 0)
	{
		enc_80_4ECC60 (key, 0x73A9ABFD);
		enc_80_4E8590 (key, 0xAE0AACE);
		TRY(enc_80_major_4AC560 (key, seed));
	}

	if (type == 0x0F)
	{
		enc_80_4E5F30 (key, 0x7232319D);
		enc_80_4E8530 (key, 0x5A52FAC);
		TRY(enc_80_major_4BB410 (key, seed));
	}

	var_magic += 0x4CAD15;
	KEY(0x30) -= my_cos(seed) ? var_magic : KEY(0x24);

	if (type == 0x0B)
	{
		enc_80_4E5EB0 (key, 0x27BA9331);
		enc_80_4E8500 (key, 0xB888054B);
		TRY(enc_80_major_4B2FD0 (key, seed));
	}

	if (type == 7)
	{
		enc_80_4A1B50 (key, 0x416C02F);
		enc_80_4DB480 (key, 0xE89F6224);
		TRY(enc_80_major_4D2900 (key, KEY(0x4)));
	}

	KEY(0x44) -= KEY(0x44) < var_magic ? KEY(0x44) : KEY(0x48);

	if (type == 0)
	{
		enc_80_4D57B0 (key, 0x9290BA3C);
		enc_80_4D79E0 (key, 0x42E2FDD0);
		TRY(enc_80_major_4C52A0 (key, KEY(0x28)));
	}

	if (type == 1)
	{
		enc_80_4A1B20 (key, 0x4C87F8E5);
		enc_80_4C5270 (key, 0xF4EC2AF1);
		TRY(enc_80_major_49E930 (key, KEY(0x44)));
	}

	var_magic &= 0x49CD75;
	KEY(0x18) += KEY(0x00) - 0xD6249F6;

	if (type == 0x0C)
	{
		TRY(enc_80_4EF3B0 (key, 0xC7589B9A));
		TRY(enc_80_4ECBE0 (key, 0x50F8EBD6));
		TRY(enc_80_major_49ED30 (key, KEY(0x18)));
	}

	if (type == 1)
	{
		enc_80_49ECD0 (key, 0x3EDF3873);
		enc_80_4EF200 (key, 0x200D9DB1);
		TRY(enc_80_major_4D2500 (key, seed));
	}

	var_magic &= 0x460AB3;
	KEY(0x14) += ((KEY(0x40) * 7) << 4) + KEY(0x40);

	if (type == 6)
	{
		enc_80_4DB240 (key, 0xF87BB298);
		TRY(enc_80_4D7A40 (key, 0x207E3A68));
		TRY(enc_80_major_4AE780 (key, KEY(0x20)));
	}

	if (type == 2)
	{
		TRY(enc_80_4B2C60 (key, 0xAEDD7B8));
		TRY(enc_80_4EF230 (key, 0xF556C814));
		TRY(enc_80_major_4F3220 (key, KEY(0x3C)));
	}

	KEY(0x2C) |= KEY(0x34) * var_magic;

	if (type == 0x0D)
	{
		enc_80_49B600 (key, 0x1CF92975);
		TRY(enc_80_4AEC50 (key, 0x2F50247E));
		TRY(enc_80_major_4C1A00 (key, KEY(0x2C)));
	}

	if (type == 5)
	{
		enc_80_4EF200 (key, 0x36559050);
		enc_80_4A1B50 (key, 0xCCBA4E8F);
		TRY(enc_80_major_4E86B0 (key, seed));
	}

	var_magic += 0x48A521;
	KEY(0x28) += KEY(0x28) ^ 0x53350A5E;

	RETURN;
}

THROWS_RET enc_80_major_4D2500 (u32 *key, u32 seed)
{
	u32 type = (KEY(0x3C) ^ KEY(0x04) ^ KEY(0x48)) % 0x0A;
	u32 var_magic = 0x2441DB49;

	var_magic += 0x486DE3;
	KEY(0x38) *=  KEY(0x48) - 0x3438A280;

	if (type == 9)
	{
		TRY(enc_80_4B2BC0 (key, 0xDC6C3D21));
		enc_80_4ECC60 (key, 0x8835AEB0);
		enc_80_4C50E0 (key, KEY(0x40));
	}

	var_magic -= 0x47E04D;
	KEY(0x08) += KEY(0x0C) & var_magic;

	if (type == 4)
	{
		enc_80_4EF300 (key, 0x306465D3);
		enc_80_4DB000 (key, 0xFA8D40AB);
		enc_80_49B630 (key, seed);
	}

	KEY(0x48) *= ROL(seed, var_magic);

	if (type == 5)
	{
		enc_80_4DB480 (key, 0x44A427);
		TRY(enc_80_4D5760 (key, 0xF1BEA435));
		TRY(enc_80_major_4AC300 (key, KEY(0x38)));
	}

	var_magic ^= 0x4B5CAD;
	KEY(0x18) += KEY(0x28) + 0x5308B1DB;

	if (type == 1)
	{
		enc_80_4C19B0 (key, 0x760B9BB);
		enc_80_4AECA0 (key, 0x4309F0C9);
		TRY(enc_80_major_4D7AE0 (key, seed));
	}

	var_magic |= 0x4F2B4A;
	KEY(0x14) += ROR(KEY(0x20), 0x39);

	if (type == 0)
	{
		TRY(enc_80_4E5EE0 (key, 0xAFB2F69));
		enc_80_4A1B50 (key, 0x340C5B01);
		TRY(enc_80_major_49B330 (key, KEY(0x14)));
	}

	KEY(0x24) |= seed & 0x27BE8251;

	if (type == 2)
	{
		TRY(enc_80_4B2BC0 (key, 0x9DBE27AD));
		TRY(enc_80_4AC2B0 (key, 0x42B24BA3));
		TRY(enc_80_major_4E5F90 (key, KEY(0x24)));
	}

	var_magic |= 0x4D9192;
	KEY(0x30) ^= KEY(0x08) * 73;

	if (type == 8)
	{
		enc_80_4EF400 (key, 0xFC1F1C1);
		enc_80_4E8590 (key, 0x320EFF97);
		TRY(enc_80_major_4EF430 (key, seed));
	}

	var_magic &= 0x4C3ADB;
	KEY(0x38) &= seed - 0x7413C08;

	if (type == 6)
	{
		enc_80_4D79B0 (key, 0x5D42041);
		TRY(enc_80_4AEC50 (key, 0x2725C3FB));
		TRY(enc_80_major_4C52A0 (key, KEY(0x44)));
	}

	KEY(0x24) += var_magic - seed;

	if (type == 3)
	{
		enc_80_4C5060 (key, 0x360E1D91);
		enc_80_4D7A10 (key, 0x1BBA7CBF);
		TRY(enc_80_major_49E930 (key, seed));
	}

	var_magic ^= 0x44D0B9;
	seed += var_magic | KEY(0x48);

	if (type == 0)
	{
		enc_80_4C5060 (key, 0x43E9D7F5);
		enc_80_4E8530 (key, 0x45359C51);
		TRY(enc_80_major_49ED30 (key, KEY(0x4C)));
	}

	var_magic |= 0x46DC29;
	KEY(0x0C) += seed < var_magic ? seed : KEY(0x10);

	RETURN;
}

THROWS_RET enc_80_major_4D2900 (u32 *key, u32 seed)
{
	u32 type = (KEY(0x2C) ^ KEY(0x18) ^ KEY(0x4C)) & 0x0F;
	u32 var_magic = 0x50782365;

	if (type == 7)
	{
		TRY(enc_80_4D57E0 (key, 0x58B010AD));
		enc_80_4A1B50 (key, 0xACA68F94);
		TRY(enc_80_major_4EF430 (key, seed));
	}

	var_magic += 0x430FFD;
	KEY(0x48) = ROR(KEY(0x48), KEY(0x18) + var_magic);

	if (type == 0x0D)
	{
		enc_80_4ECC60 (key, 0x9B3F91AC);
		TRY(enc_80_4D57E0 (key, 0xF97424E2));
		TRY(enc_80_major_4C52A0 (key, KEY(0x38)));
	}

	if (type == 0x0F)
	{
		TRY(enc_80_4B2BC0 (key, 0xCE7048A1));
		enc_80_4E5D40 (key, 0x3FA8F221);
		TRY(enc_80_major_49E930 (key, KEY(0x00)));
	}

	var_magic ^= 0x44568F;
	KEY(0x18) -= my_cos(KEY(0x38)) ? var_magic : KEY(0x24);

	if (type == 2)
	{
		enc_80_4EF200 (key, 0x1F00A68E);
		enc_80_4E5E60 (key, 0x6B66ECC7);
		TRY(enc_80_major_49ED30 (key, KEY(0x34)));
	}

	if (type == 8)
	{
		enc_80_4ECC60 (key, 0x7D8AEE44);
		enc_80_4D7A10 (key, 0x6A6D34AA);
		TRY(enc_80_major_4D2500 (key, seed));
	}

	seed ^= ROR(seed, var_magic);

	if (type == 0x0A)
	{
		enc_80_4E8620 (key, 0xDF91CB30);
		enc_80_4B2CE0 (key, 0x11D2ED95);
		TRY(enc_80_major_4AE780 (key, seed));
	}

	if (type == 5)
	{
		enc_80_4E5DA0 (key, 0x7D8CAF0B);
		enc_80_4D7940 (key, 0xF8774F77);
		TRY(enc_80_major_4F3220 (key, KEY(0x28)));
	}

	var_magic |= 0x4DE205;
	KEY(0x44) ^= KEY(0x10) | var_magic;

	if (type == 3)
	{
		enc_80_49B2E0 (key, 0x9E7B6025);
		enc_80_4EF400 (key, 0x224A0F90);
		TRY(enc_80_major_4C1A00 (key, KEY(0x44)));
	}

	if (type == 0)
	{
		enc_80_4ECCC0 (key, 0x76727E9);
		TRY(enc_80_4B2C60 (key, 0x31017341));
		TRY(enc_80_major_4E86B0 (key, KEY(0x20)));
	}

	var_magic += 0x49CDDC;
	seed ^= seed < var_magic ? seed : KEY(0x14);

	if (type == 0x0B)
	{
		enc_80_4D7980 (key, 0x4A88EBB8);
		enc_80_4EF280 (key, 0x3337FB6C);
		TRY(enc_80_major_4AC560 (key, seed));
	}

	if (type == 2)
	{
		TRY(enc_80_4DB090 (key, 0x2118E5D0));
		TRY(enc_80_4B2C60 (key, 0x4249CBC));
		TRY(enc_80_major_4BB410 (key, KEY(0x4)));
	}

	KEY(0x24) += KEY(0x1C) ^ var_magic;

	if (type == 0x0E)
	{
		enc_80_4DB000 (key, 0x21CDDA75);
		enc_80_49B280 (key, 0x46E80D07);
		TRY(enc_80_major_4B2FD0 (key, KEY(0x18)));
	}

	if (type == 1)
	{
		enc_80_4DB030 (key, 0x3A946C4C);
		enc_80_4A1B50 (key, 0xB14F56D6);
		TRY(enc_80_major_4EF430 (key, KEY(0x4C)));
	}

	var_magic |= 0x46C836;
	KEY(0x44) |= KEY(0x28) - var_magic;

	if (type == 6)
	{
		enc_80_4EF2A0 (key, 0x363944C);
		enc_80_4D57B0 (key, 0x8D0AACAA);
		TRY(enc_80_major_4C52A0 (key, KEY(0x20)));
	}

	if (type == 9)
	{
		enc_80_4EF280 (key, 0x1D7942BA);
		TRY(enc_80_4C5090 (key, 0xF46A752));
		TRY(enc_80_major_49E930 (key, KEY(0x8)));
	}

	var_magic &= 0x4E1DEB;
	KEY(0x18) ^= my_cos(KEY(0x34)) ? 0xB29627F : seed;

	if (type == 0x0C)
	{
		enc_80_49ECD0 (key, 0x286766C5);
		enc_80_49B2E0 (key, 0x759CF99F);
		TRY(enc_80_major_49ED30 (key, KEY(0x14)));
	}

	if (type == 0)
	{
		enc_80_4EF400 (key, 0x19ECF73D);
		TRY(enc_80_4AEC50 (key, 0x664D4285));
		TRY(enc_80_major_4D2500 (key, KEY(0x38)));
	}

	seed &= (KEY(0x34) * 13) << 1;

	if (type == 4)
	{
		TRY(enc_80_4AEC50 (key, 0x281B56A6));
		TRY(enc_80_4D7A90 (key, 0xF7120246));
		TRY(enc_80_major_4AE780 (key, KEY(0x48)));
	}

	if (type == 1)
	{
		enc_80_4ECCC0 (key, 0xC032CB66);
		enc_80_4E8590 (key, 0x41D2F9DE);
		TRY(enc_80_major_4F3220 (key, KEY(0x10)));
	}

	var_magic -= 0x4A749E;
	KEY(0x40) *= KEY(0x0C) * var_magic;

	RETURN;
}


THROWS_RET enc_80_major_4D7AE0 (u32 *key, u32 seed)
{
	u32 type = (KEY(0x34) ^ KEY(0x20) ^ KEY(0x44)) % 0x03;
	u32 var_magic = 0x39F69C3A;

	var_magic -= 0x4C5E86;
	seed ^= var_magic ^ KEY(0x44);

	var_magic ^= 0x4A79DF;
	KEY(0x3C) = ROL(KEY(0x3C), my_sin(seed) ? 0x236D13F3 : KEY(0x4C));
	KEY(0x40) ^= my_sqrt(seed);

	if (type == 0)
	{
		enc_80_49ECD0 (key, 0x3EAED654);
		enc_80_4EF300 (key, 0x3C4F15DD);
		enc_80_4C50E0 (key, seed);
	}

	var_magic -= 0x46E975;
	seed = (seed * seed * 5) << 4;

	var_magic -= 0x4D3A0F;
	KEY(0x48) |= KEY(0x20) * 0x1E4B94EC;
	KEY(0x0C) += seed & 0x2708964F;

	if (type == 0)
	{
		enc_80_4D7940 (key, 0x1ADC1DAA);
		enc_80_4E5D40 (key, 0x6A9FA0EA);
		enc_80_49B630 (key, seed);
	}

	var_magic ^= 0x46E1BE;
	KEY(0x00) -= KEY(0x28) | var_magic;

	var_magic &= 0x484A3B;
	KEY(0x20) -= my_cos(KEY(0x18)) ? var_magic : KEY(0x28);
	KEY(0x44) &= var_magic | seed;

	var_magic |= 0x43BE36;
	KEY(0x38) *= ROL(KEY(0x08), var_magic);

	if (type == 1)
	{
		TRY(enc_80_4D7A90 (key, 0xCD92AAF));
		enc_80_4E5DD0 (key, 0x1D57420D);
		TRY(enc_80_major_4AC300 (key, KEY(0x1C)));
	}

	var_magic -= 0x46EC2B;
	KEY(0x20) *= KEY(0x3C) - var_magic;

	RETURN;
}

THROWS_RET enc_80_major_4DB520 (u32 *key, u32 seed)
{
	u32 type = (KEY(0x3C) ^ KEY(0x10) ^ KEY(0x4C)) & 0x0F;
	u32 var_magic = 0x1999922A;

	if (type == 8)
	{
		enc_80_4C19B0 (key, 0xD0848E5B);
		TRY(enc_80_4D7A40 (key, 0x629A58BE));
		TRY(enc_80_major_49ED30 (key, KEY(0x30)));
	}

	var_magic ^= 0x47481A;
	KEY(0x40) += var_magic - KEY(0x04);

	if (type == 2)
	{
		enc_80_4EF280 (key, 0x60A53BB0);
		TRY(enc_80_4F31A0 (key, 0xE4C83A4));
		TRY(enc_80_major_4D2500 (key, seed));
	}

	if (type == 3)
	{
		enc_80_49B310 (key, 0x944DA846);
		enc_80_4EF280 (key, 0x56DE6466);
		TRY(enc_80_major_4AE780 (key, seed));
	}

	var_magic ^= 0x47A890;
	seed -= my_cos(KEY(0x28)) ? var_magic : seed;

	if (type == 4)
	{
		enc_80_49B2E0 (key, 0x550CAD01);
		enc_80_4DB270 (key, 0x46E1A196);
		TRY(enc_80_major_4F3220 (key, seed));
	}

	if (type == 9)
	{
		enc_80_4EF400 (key, 0x1CF0C29A);
		TRY(enc_80_4EF230 (key, 0x2847A5B4));
		TRY(enc_80_major_4C1A00 (key, KEY(0x40)));
	}

	KEY(0x40) *= my_sqrt(KEY(0x08));

	if (type == 0x0E)
	{
		enc_80_4E5EB0 (key, 0x103EC068);
		enc_80_4C5240 (key, 0x71C9FA4);
		TRY(enc_80_major_4E86B0 (key, KEY(0x20)));
	}

	if (type == 5)
	{
		enc_80_4D79E0 (key, 0x3CE6167);
		enc_80_4A1B50 (key, 0xBC47B55F);
		TRY(enc_80_major_4AC560 (key, KEY(0x24)));
	}

	var_magic -= 0x4A065E;
	KEY(0x48) -= my_sqrt(KEY(0x08));

	if (type == 6)
	{
		enc_80_4E5E00 (key, 0x903B6F4);
		enc_80_4B2CB0 (key, 0x96974932);
		TRY(enc_80_major_4BB410 (key, seed));
	}

	if (type == 1)
	{
		enc_80_4C5270 (key, 0xA5BBF43);
		enc_80_4DB2E0 (key, 0xFBD29DCF);
		TRY(enc_80_major_4B2FD0 (key, KEY(0x34)));
	}

	var_magic ^= 0x444C3D;
	seed += KEY(0x20) < 0x2B940916 ? KEY(0x20) : seed;

	if (type == 3)
	{
		enc_80_4E5DA0 (key, 0xE7559324);
		TRY(enc_80_4EF230 (key, 0x1773E584));
		TRY(enc_80_major_4D2900 (key, seed));
	}

	if (type == 2)
	{
		enc_80_4E8500 (key, 0x445B7A34);
		TRY(enc_80_4D7A90 (key, 0xAA0BDC3));
		TRY(enc_80_major_4CFE70 (key, KEY(0x14)));
	}

	seed *= KEY(0x14) + 0x1D13557B;

	if (type == 0x0B)
	{
		TRY(enc_80_4B2BC0 (key, 0xF4F5ADCC));
		enc_80_49ECD0 (key, 0x4D0BB323);
		TRY(enc_80_major_4ECD20 (key, KEY(0x44)));
	}

	if (type == 0x0A)
	{
		enc_80_4E5E60 (key, 0xA399EF87);
		TRY(enc_80_4ECBE0 (key, 0x77D04C87));
		enc_80_major_49ED30 (key, seed);
	}

	var_magic &= 0x409751;
	KEY(0x00) -= my_sqrt(seed);

	if (type == 0x0D)
	{
		enc_80_4A1B80 (key, 0x26F1D60B);
		TRY(enc_80_4EF350 (key, 0x45A48224));
		TRY(enc_80_major_4D2500 (key, KEY(0x44)));
	}

	if (type == 0)
	{
		TRY(enc_80_4EF230 (key, 0xBB4EDC7));
		TRY(enc_80_4D7A40 (key, 0x2293B42A));
		TRY(enc_80_major_4AE780 (key, KEY(0x4C)));
	}

	var_magic -= 0x46AC59;
	KEY(0x48) += var_magic + seed;

	if (type == 0x0F)
	{
		TRY(enc_80_4EF350 (key, 0x69B30067));
		enc_80_4E5F30 (key, 0x7A07A671);
		TRY(enc_80_major_4F3220 (key, seed));
	}

	if (type == 1)
	{
		TRY(enc_80_4E5EE0 (key, 0xF7D9807));
		enc_80_4D79B0 (key, 0x3FDAD28A);
		TRY(enc_80_major_4C1A00 (key, KEY(0x20)));
	}

	KEY(0x0C) ^= my_cos(seed) ? var_magic : KEY(0x10);

	if (type == 7)
	{
		enc_80_49B2E0 (key, 0x1C978F85);
		enc_80_4ECC90 (key, 0x2D581FC9);
		TRY(enc_80_major_4E86B0 (key, KEY(0x4)));
	}

	if (type == 0)
	{
		enc_80_4ECC90 (key, 0x77E6FB5D);
		enc_80_49ECD0 (key, 0x20710AEB);
		TRY(enc_80_major_4AC560 (key, KEY(0x20)));
	}

	var_magic ^= 0x41D40C;
	KEY(0x04) ^= KEY(0x0C) * 0x620312F0;

	if (type == 0x0C)
	{
		enc_80_49B280 (key, 0x8F75E201);
		enc_80_4E8620 (key, 0xB7C094D3);
		TRY(enc_80_major_4BB410 (key, KEY(0x30)));
	}

	var_magic |= 0x429937;
	KEY(0x1C) ^= my_sqrt(KEY(0x28));

	if (type == 4)
	{
		enc_80_4B2CE0 (key, 0xA37BD2C9);
		enc_80_4E5DD0 (key, 0xFD6562FC);
		TRY(enc_80_major_4B2FD0 (key, KEY(0x10)));
	}

	if (type == 5)
	{
		enc_80_4D57B0 (key, 0x716EA3BC);
		enc_80_4ECCF0 (key, 0xE39E92E9);
		TRY(enc_80_major_4D2900 (key, seed));
	}

	KEY(0x48) ^= KEY(0x38) + 0x9AD3708;

	RETURN;
}

THROWS_RET enc_80_mix (u32 *key, u32 seed)
{
	u32 type = (KEY(0x1C) ^ KEY(0x2C) ^ KEY(0x48)) & 0x0F;
	u32 var_magic = 0x2D6913E5;

	if (type == 0x0E)
	{
		enc_80_4A1B20 (key, 0xD2CA38AA);
		enc_80_4A1B50 (key, 0x8BF73297);
		TRY(enc_80_major_4AE780 (key, seed));
	}

	if (type == 9)
	{
		enc_80_4C5060 (key, 0x15CCADEE);
		enc_80_49ECD0 (key, 0x16DBC359);
		TRY(enc_80_major_4F3220 (key, KEY(0x1C)));
	}

	var_magic ^= 0x4C7B92;
	KEY(0x2C) += 0 - (((KEY(0x48) * 5) << 4) - KEY(0x48));

	if (type == 0x0C)
	{
		enc_80_4C5270 (key, 0x3BDCF76);
		enc_80_4E8680 (key, 0xCA2860B6);
		TRY(enc_80_major_4C1A00 (key, KEY(0x8)));
	}

	if (type == 3)
	{
		enc_80_4E8500 (key, 0x32372936);
		TRY(enc_80_4EF230 (key, 0x10B3A8DB));
		TRY(enc_80_major_4E86B0 (key, KEY(0x1C)));
	}

	if (type == 0x0B)
	{
		enc_80_49ECD0 (key, 0x512B3D5A);
		enc_80_4A1B80 (key, 0xFD3ABC53);
		TRY(enc_80_major_4AC560 (key, KEY(0x44)));
	}

	var_magic &= 0x41AD21;
	KEY(0x4C) ^= ROR(KEY(0x30), 0xFD);

	if (type == 4)
	{
		TRY(enc_80_4D57E0 (key, 0x1EE7589E));
		enc_80_4B2CB0 (key, 0xEBF5637C);
		TRY(enc_80_major_4BB410 (key, KEY(0x4)));
	}

	if (type == 2)
	{
		TRY(enc_80_4D5830 (key, 0x58CDF32));
		TRY(enc_80_4EF3B0 (key, 0x854DA745));
		TRY(enc_80_major_4B2FD0 (key, KEY(0x8)));
	}

	seed ^= KEY(0x38) - var_magic;

	if (type == 4)
	{
		enc_80_4A1B20 (key, 0x556A5BA2);
		enc_80_4C5240 (key, 0x58D59195);
		TRY(enc_80_major_4D2900 (key, seed));
	}

	if (type == 1)
	{
		TRY(enc_80_4AC2B0 (key, 0x55CB9027));
		enc_80_4ECC90 (key, 0x2CF16B8C);
		TRY(enc_80_major_4CFE70 (key, seed));
	}

	if (type == 0)
	{
		enc_80_4D79E0 (key, 0x474CE589);
		enc_80_4EF2A0 (key, 0x278BAB07);
		TRY(enc_80_major_4ECD20 (key, KEY(0x2C)));
	}

	var_magic |= 0x462C86;
	KEY(0x10) |= var_magic ^ KEY(0x10); // 0x10468B00

	if (type == 0x0D)
	{
		enc_80_4D7980 (key, 0x6A80B098);
		TRY(enc_80_4AC2B0 (key, 0x63EB6F7B));
		TRY(enc_80_major_4DB520 (key, KEY(0x34)));
	}

	if (type == 0x0F)
	{
		enc_80_4DB2E0 (key, 0x1463C4A1);
		enc_80_4C5270 (key, 0xE474D9D8);
		TRY(enc_80_major_4A1BB0 (key, KEY(0x34)));
	}

	var_magic -= 0x4327C5;
	KEY(0x34) -= KEY(0x40) < 0x97CBA53 ? KEY(0x40) : seed;

	if (type == 2)
	{
		enc_80_4E8590 (key, 0xFE3E1E71);
		TRY(enc_80_4C5090 (key, 0xD1006F43));
		TRY(enc_80_major_4AE780 (key, KEY(0x44)));
	}

	if (type == 6)
	{
		TRY(enc_80_4EF230 (key, 0xCE0C6C5));
		TRY(enc_80_4F31A0 (key, 0x244FAD8));
		TRY(enc_80_major_4F3220 (key, KEY(0x3C)));
	}

	if (type == 7)
	{
		enc_80_4DB270 (key, 0xAD4B36B);
		enc_80_4DB240 (key, 0xD585E7CE);
		TRY(enc_80_major_4C1A00 (key, KEY(0x20)));
	}

	KEY(0x28) ^= ((KEY(0x08) << 5) - KEY(0x08)) << 1;

	if (type == 6)
	{
		TRY(enc_80_49E8E0 (key, 0x171DFEAC));
		enc_80_4C5060 (key, 0x2891588E);
		TRY(enc_80_major_4E86B0 (key, KEY(0x40)));
	}

	if (type == 5)
	{
		enc_80_4D7A10 (key, 0x69E794CC);
		enc_80_4E5EB0 (key, 0x87C73F);
		TRY(enc_80_major_4AC560 (key, KEY(0x3C)));
	}

	var_magic |= 0x433F26;
	KEY(0x34) &= KEY(0x24) ^ var_magic;

	if (type == 7)
	{
		enc_80_4EF2A0 (key, 0x2474EC0D);
		enc_80_4ECC60 (key, 0x9FFE3AE);
		TRY(enc_80_major_4BB410 (key, seed));
	}

	if (type == 1)
	{
		TRY(enc_80_4F31A0 (key, 0xF433F152));
		TRY(enc_80_4D5760 (key, 0xED7971FE));
		TRY(enc_80_major_4B2FD0 (key, KEY(0x14)));
	}

	if (type == 0)
	{
		enc_80_4ECCC0 (key, 0x198ADCAD);
		enc_80_49B2E0 (key, 0xAAF84F9D);
		TRY(enc_80_major_4D2900 (key, KEY(0x44)));
	}

	var_magic -= 0x46D37A;
	KEY(0x04) -= ROL(KEY(0x1C), 0xEC);

	if (type == 8)
	{
		enc_80_4E8620 (key, 0xC80536A9);
		enc_80_4E8680 (key, 0x20A3AF35);
		TRY(enc_80_major_4CFE70 (key, KEY(0x8)));
	}

	if (type == 0x0A)
	{
		enc_80_4E5E00 (key, 0xF611CB26);
		enc_80_4E8500 (key, 0x8A2C966F);
		TRY(enc_80_major_4ECD20 (key, KEY(0x34)));
	}

	KEY(0x28) -= KEY(0x00) < var_magic ? KEY(0x00) : KEY(0x14);

	if (type == 3)
	{
		enc_80_4A1B20 (key, 0x6795F8E2);
		enc_80_4D7A10 (key, 0x1CA54873);
		TRY(enc_80_major_4DB520 (key, KEY(0x00)));
	}

	if (type == 5)
	{
		enc_80_4E8680 (key, 0x3EA6D0A7);
		enc_80_4B2CE0 (key, 0x246B2703);
		TRY(enc_80_major_4A1BB0 (key, KEY(0x40)));
	}

	var_magic |= 0x4509D2;
	KEY(0x14) += ((seed << 5) + seed) * 3;

	RETURN;
}
