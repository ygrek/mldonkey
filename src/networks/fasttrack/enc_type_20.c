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

/*
 * This is the most recent pad mingling code for FastTrack as of 03/04/28
 * Used for encryption version 0x20
 */

#include <math.h>	// for floating point stuff

/* our crude SEH replacement */

typedef int THROWS_RET;
#define THROW return -1;
#define TRY(x) { if((x) == -1) return 0; }
#define RETURN return 0;

/* a macro for easier access to the key */

#define KEY(x) (*((unsigned int*)(key+x)))

/* some constants and helper funcs */ 

double math_const_1 = 1.e-003;
double math_const_2 = 0;

static unsigned int ROR(unsigned int value, unsigned int count)
{
  count = (count & 0xff) % 32;
  return (value >> count) | (value << (32 - count));
}

static unsigned int ROL(unsigned int value, unsigned int count)
{
  count = (count & 0xff) % 32;
  return (value << count) | (value >> (32 - count));
}

static unsigned int my_ftol (double var)
{
	return (unsigned int)var;
}

/* the entry point of this mess */

THROWS_RET enc_20_mix (unsigned char *key, unsigned int seed);

void enc_type_20 (unsigned char *key, unsigned int seed)
{
	enc_20_mix (key, seed);
}

/* major functions which make calls to other funcs */

static THROWS_RET enc_20_major_4EECA0 (unsigned char *key, unsigned int seed);
static THROWS_RET enc_20_major_4F2B40 (unsigned char *key, unsigned int seed);
static THROWS_RET enc_20_major_49ADF0 (unsigned char *key, unsigned int seed);
static THROWS_RET enc_20_major_4A16B0 (unsigned char *key, unsigned int seed);
static THROWS_RET enc_20_major_4ABC60 (unsigned char *key, unsigned int seed);
static THROWS_RET enc_20_major_4B19A0 (unsigned char *key, unsigned int seed);
static THROWS_RET enc_20_major_4BAC60 (unsigned char *key, unsigned int seed);
static THROWS_RET enc_20_major_4C1240 (unsigned char *key, unsigned int seed);
static THROWS_RET enc_20_major_4C4AC0 (unsigned char *key, unsigned int seed);
static THROWS_RET enc_20_major_4CF650 (unsigned char *key, unsigned int seed);
static THROWS_RET enc_20_major_4D1E70 (unsigned char *key, unsigned int seed);
static THROWS_RET enc_20_major_4D4910 (unsigned char *key, unsigned int seed);
static THROWS_RET enc_20_major_4D4FB0 (unsigned char *key, unsigned int seed);
static THROWS_RET enc_20_major_4DA520 (unsigned char *key, unsigned int seed);
static THROWS_RET enc_20_major_4DF3C0 (unsigned char *key, unsigned int seed);
static THROWS_RET enc_20_major_4E01F0 (unsigned char *key, unsigned int seed);
static THROWS_RET enc_20_major_4E55C0 (unsigned char *key, unsigned int seed);
static THROWS_RET enc_20_major_4E5960 (unsigned char *key, unsigned int seed);
static THROWS_RET enc_20_major_4E7CE0 (unsigned char *key, unsigned int seed);
static THROWS_RET enc_20_major_4E7FD0 (unsigned char *key, unsigned int seed);
static THROWS_RET enc_20_major_4EB610 (unsigned char *key, unsigned int seed);
static THROWS_RET enc_20_major_4AE190 (unsigned char *key, unsigned int seed);
static THROWS_RET enc_20_major_4AE490 (unsigned char *key, unsigned int seed);
static THROWS_RET enc_20_major_4B24B0 (unsigned char *key, unsigned int seed);

/* functions which throw exceptions */

static THROWS_RET enc_20_4AE420 (unsigned char *key, unsigned int seed);
static THROWS_RET enc_20_4BAC10 (unsigned char *key, unsigned int seed);
static THROWS_RET enc_20_4D56B0 (unsigned char *key, unsigned int seed);
static THROWS_RET enc_20_4D7900 (unsigned char *key, unsigned int seed);
static THROWS_RET enc_20_4DA120 (unsigned char *key, unsigned int seed);
static THROWS_RET enc_20_4DA200 (unsigned char *key, unsigned int seed);
static THROWS_RET enc_20_4DA4D0 (unsigned char *key, unsigned int seed);
static THROWS_RET enc_20_4EEC50 (unsigned char *key, unsigned int seed);
static THROWS_RET enc_20_49AA90 (unsigned char *key, unsigned int seed);
static THROWS_RET enc_20_4D76F0 (unsigned char *key, unsigned int seed);
static THROWS_RET enc_20_49B1F0 (unsigned char *key, unsigned int seed);
static THROWS_RET enc_20_4A1640 (unsigned char *key, unsigned int seed);
static THROWS_RET enc_20_4E7C50 (unsigned char *key, unsigned int seed);

/* simple key manipulation functions */

static void enc_20_4EE700 (unsigned char *key, unsigned int seed);
static void enc_20_49AAE0 (unsigned char *key, unsigned int seed);
static void enc_20_49AB20 (unsigned char *key, unsigned int seed);
static void enc_20_49ADD0 (unsigned char *key, unsigned int seed);
static void enc_20_49B240 (unsigned char *key, unsigned int seed);
static void enc_20_49B260 (unsigned char *key, unsigned int seed);
static void enc_20_49CF30 (unsigned char *key, unsigned int seed);
static void enc_20_4A1AC0 (unsigned char *key, unsigned int seed);
static void enc_20_4A1AE0 (unsigned char *key, unsigned int seed);
static void enc_20_4A1B00 (unsigned char *key, unsigned int seed);
static void enc_20_4AB3B0 (unsigned char *key, unsigned int seed);
static void enc_20_4ABC20 (unsigned char *key, unsigned int seed);
static void enc_20_4ABC40 (unsigned char *key, unsigned int seed);
static void enc_20_4ADBC0 (unsigned char *key, unsigned int seed);
static void enc_20_4AE170 (unsigned char *key, unsigned int seed);
static void enc_20_4D70D0 (unsigned char *key, unsigned int seed);
static void enc_20_4D7050 (unsigned char *key, unsigned int seed);
static void enc_20_4D7660 (unsigned char *key, unsigned int seed);
static void enc_20_4D7680 (unsigned char *key, unsigned int seed);
static void enc_20_4F2B20 (unsigned char *key, unsigned int seed);
static void enc_20_4E7CC0 (unsigned char *key, unsigned int seed);
static void enc_20_4DA1E0 (unsigned char *key, unsigned int seed);
static void enc_20_4E4F80 (unsigned char *key, unsigned int seed);
static void enc_20_4E7FB0 (unsigned char *key, unsigned int seed);
static void enc_20_4EB2A0 (unsigned char *key, unsigned int seed);
static void enc_20_4D9AD0 (unsigned char *key, unsigned int seed);
static void enc_20_4F2AF0 (unsigned char *key, unsigned int seed);
static void enc_20_4B0380 (unsigned char *key, unsigned int seed);
static void enc_20_4BABE0 (unsigned char *key, unsigned int seed);
static void enc_20_4C47B0 (unsigned char *key, unsigned int seed);
static void enc_20_4C47D0 (unsigned char *key, unsigned int seed);
static void enc_20_4C4A60 (unsigned char *key, unsigned int seed);
static void enc_20_4C5040 (unsigned char *key, unsigned int seed);
static void enc_20_4CE2E0 (unsigned char *key, unsigned int seed);
static void enc_20_4CE300 (unsigned char *key, unsigned int seed);
static void enc_20_4D1E50 (unsigned char *key, unsigned int seed);
static void enc_20_4D4150 (unsigned char *key, unsigned int seed);
static void enc_20_4D4170 (unsigned char *key, unsigned int seed);
static void enc_20_4D5720 (unsigned char *key, unsigned int seed);

static void enc_20_4C0780 (unsigned char *key, unsigned int seed);
static void enc_20_4C07D0 (unsigned char *key, unsigned int seed);
static void enc_20_4D4100 (unsigned char *key, unsigned int seed);
static void enc_20_4A12B0 (unsigned char *key, unsigned int seed);
static void enc_20_4D6FB0 (unsigned char *key, unsigned int seed);
static void enc_20_4D7000 (unsigned char *key, unsigned int seed);
static void enc_20_4D7080 (unsigned char *key, unsigned int seed);
static void enc_20_4D76A0 (unsigned char *key, unsigned int seed);
static void enc_20_4D7740 (unsigned char *key, unsigned int seed);
static void enc_20_4E4F30 (unsigned char *key, unsigned int seed);
static void enc_20_4E7460 (unsigned char *key, unsigned int seed);
static void enc_20_4F2AA0 (unsigned char *key, unsigned int seed);
static void enc_20_4EB2C0 (unsigned char *key, unsigned int seed);
static void enc_20_4AE3C0 (unsigned char *key, unsigned int seed);
static void enc_20_4F2A40 (unsigned char *key, unsigned int seed);
static void enc_20_49CED0 (unsigned char *key, unsigned int seed);
static void enc_20_4D56F0 (unsigned char *key, unsigned int seed);
static void enc_20_4D5740 (unsigned char *key, unsigned int seed);
static void enc_20_4D7790 (unsigned char *key, unsigned int seed);
static void enc_20_4EB310 (unsigned char *key, unsigned int seed);


/* minor implementation details below this line ;) */

void enc_20_4EE700 (unsigned char *key, unsigned int seed)
{
	KEY(0x48) -=  KEY(0x34) ^ 0x154ABCDF;
}

void enc_20_49AAE0 (unsigned char *key, unsigned int seed)
{
	KEY(0x1C) |= seed + 0x013DADA0;
}

void enc_20_49AB20 (unsigned char *key, unsigned int seed)
{
	KEY(0x0C) -= KEY(0x00) ^ 0x185F3B0D;
}

void enc_20_49ADD0 (unsigned char *key, unsigned int seed)
{
	KEY(0x28) *= KEY(0x28) - 0x05EAE6BF;
}

void enc_20_49B240 (unsigned char *key, unsigned int seed)
{
	KEY(0x24) |= KEY(0x1C) ^ 0x2A19119F;
}

void enc_20_49B260 (unsigned char *key, unsigned int seed)
{
	KEY(0x10) = ROR(KEY(0x10), (unsigned char)((seed << 5) - seed));
}

void enc_20_49CF30 (unsigned char *key, unsigned int seed)
{
	KEY(0x08) *= KEY(0x0C) + 0x0D6863A6;
}

void enc_20_4A1AC0 (unsigned char *key, unsigned int seed)
{
	KEY(0x10) ^= ROR(seed, 0x85);
}

void enc_20_4A1AE0 (unsigned char *key, unsigned int seed)
{
	KEY(0x18) += KEY(0x4C) - 0x3F5675D6;
}

void enc_20_4A1B00 (unsigned char *key, unsigned int seed)
{
	KEY(0x40) += seed + 0x0D2C3EBC;
}

void enc_20_4AB3B0 (unsigned char *key, unsigned int seed)
{
	KEY(0x34) *= seed + 0x0601F603;
}

void enc_20_4ABC20 (unsigned char *key, unsigned int seed)
{
	KEY(0x30) += KEY(0x18) + 0x21D7BF61;
}

void enc_20_4ABC40 (unsigned char *key, unsigned int seed)
{
	KEY(0x40) += seed * 73;
}

void enc_20_4ADBC0 (unsigned char *key, unsigned int seed)
{
	KEY(0x10) -= KEY(0x44) ^ 0x2217CF47;
}

void enc_20_4AE170 (unsigned char *key, unsigned int seed)
{
	KEY(0x38) |= KEY(0x0C) ^ 0x04345732;
}

void enc_20_4D70D0 (unsigned char *key, unsigned int seed)
{
	KEY(0x4C) ^= (((KEY(0x1C) * 8) - KEY(0x1C)) * 4 + KEY(0x1C)) << 1;
}

void enc_20_4D7050 (unsigned char *key, unsigned int seed)
{
	KEY(0x4C) += 0x12B9E29D - KEY(0x30);
}

void enc_20_4D7660 (unsigned char *key, unsigned int seed)
{
	KEY(0x24) ^= seed ^ 0x334EC044;
}

void enc_20_4D7680 (unsigned char *key, unsigned int seed)
{
	KEY(0x34) *= ROR(KEY(0x0C), 0xA5);
}

void enc_20_4F2B20 (unsigned char *key, unsigned int seed)
{
	KEY(0x00) += (KEY(0x18) * 15) << 2;
}

void enc_20_4E7CC0 (unsigned char *key, unsigned int seed)
{
	KEY(0x18) = ROL(KEY(0x18), (unsigned char)ROR(KEY(0x20), 0xCE));
}

void enc_20_4DA1E0 (unsigned char *key, unsigned int seed)
{
	KEY(0x00) += KEY(0x48) ^ 0x4AC16B8D;
}

void enc_20_4E4F80 (unsigned char *key, unsigned int seed)
{
	KEY(0x08) ^= KEY(0x3C) << 5;
}

void enc_20_4E7FB0 (unsigned char *key, unsigned int seed)
{
	KEY(0x00) &= ROR(KEY(0x48), 0xDF);
}

void enc_20_4EB2A0 (unsigned char *key, unsigned int seed)
{
	KEY(0x24) &= seed - 0x2507B6E9;
}

void enc_20_4D9AD0 (unsigned char *key, unsigned int seed)
{
	KEY(0x30) ^= KEY(0x3C) - 0x0F5CFDE0;
}

void enc_20_4F2AF0 (unsigned char *key, unsigned int seed)
{
	KEY(0x0C) = ROR(KEY(0x0C), (unsigned char)(KEY(0x2C) ^ 0x0BBEA527));
}

void enc_20_4B0380 (unsigned char *key, unsigned int seed)
{
	KEY(0x1C) &= KEY(0x34) ^ 0x21AAF758;
}

void enc_20_4BABE0 (unsigned char *key, unsigned int seed)
{
	KEY(0x1C) &= (seed * 8 - seed) * 15;
}

void enc_20_4C47B0 (unsigned char *key, unsigned int seed)
{
	KEY(0x4C) ^= KEY(0x3C) ^ 0x03574ED3;
}

void enc_20_4C47D0 (unsigned char *key, unsigned int seed)
{
	KEY(0x14) += KEY(0x00) ^ 0x3E17ADD3;
}

void enc_20_4C4A60 (unsigned char *key, unsigned int seed)
{
	KEY(0x30) += seed - 0x075D8F4F;
}

void enc_20_4C5040 (unsigned char *key, unsigned int seed)
{
	KEY(0x24) += ROL(KEY(0x10), 0xE9);
}

void enc_20_4CE2E0 (unsigned char *key, unsigned int seed)
{
	KEY(0x30) += KEY(0x18) ^ 0x211F5E40;
}

void enc_20_4CE300 (unsigned char *key, unsigned int seed)
{
	KEY(0x24) ^= ((KEY(0x1C) << 4) + KEY(0x1C)) << 2;
}

void enc_20_4D1E50 (unsigned char *key, unsigned int seed)
{
	KEY(0x48) *= KEY(0x28) + 0x466E09CF;
}

void enc_20_4D4150 (unsigned char *key, unsigned int seed)
{
	KEY(0x14) -= KEY(0x3C);
}

void enc_20_4D4170 (unsigned char *key, unsigned int seed)
{
	KEY(0x18) += 0xFE07AF0E - KEY(0x0C);
}

void enc_20_4D5720 (unsigned char *key, unsigned int seed)
{
	KEY(0x18) ^= seed + 0x25283A4A;
}


THROWS_RET enc_20_4AE420 (unsigned char *key, unsigned int seed)
{
	double var = sin((double)((unsigned char)seed));

	if(var < math_const_2)
		KEY(0x20) += 0x04F0CF8D;
	else
		KEY(0x20) += seed;

	if(KEY(0x20) & 1)
		THROW;

	RETURN;
}

THROWS_RET enc_20_4BAC10 (unsigned char *key, unsigned int seed)
{
	if(KEY(0x08) < 0x36DEF3E1)
		KEY(0x08) += KEY(0x08);
	else
		KEY(0x08) += seed;

	if(KEY(0x08) & 1)
		THROW;

	RETURN;
}

THROWS_RET enc_20_4D56B0 (unsigned char *key, unsigned int seed)
{
	KEY(0x28) ^= ROL(KEY(0x04), 0x34);

	if(KEY(0x28) & 1)
		THROW;

	RETURN;
}

THROWS_RET enc_20_4D7900 (unsigned char *key, unsigned int seed)
{
	KEY(0x40) -= KEY(0x18);

	if(KEY(0x40) & 1)
		THROW;

	RETURN;
}

THROWS_RET enc_20_4DA120 (unsigned char *key, unsigned int seed)
{
	KEY(0x28) += (((0-KEY(0x24)) << 4) - KEY(0x24)) * 5;

	if(KEY(0x28) & 1)
		THROW;

	RETURN;
}

THROWS_RET enc_20_4DA200 (unsigned char *key, unsigned int seed)
{
	KEY(0x00) = ROL(KEY(0x00), (unsigned char)(KEY(0x4C) ^ 0x0290626C));

	if(KEY(0x00) & 1)
		THROW;

	RETURN;
}

THROWS_RET enc_20_4DA4D0 (unsigned char *key, unsigned int seed)
{
	KEY(0x44) += KEY(0x20) * 0xF6084C92;

	if(KEY(0x44) & 1)
		THROW;

	RETURN;
}

THROWS_RET enc_20_4EEC50 (unsigned char *key, unsigned int seed)
{
	KEY(0x30) ^= KEY(0x28) & 0x28ACEC82;

	if(KEY(0x30) & 1)
		THROW;

	RETURN;
}

THROWS_RET enc_20_49AA90 (unsigned char *key, unsigned int seed)
{
	if(KEY(0x30) < 0x012D7BED)
		KEY(0x30) *=	KEY(0x30);
	else
		KEY(0x30) *= seed;

	if(KEY(0x30) & 1)
		THROW;

	RETURN;
}

THROWS_RET enc_20_4D76F0 (unsigned char *key, unsigned int seed)
{
	if(KEY(0x14) < 0x0FD0AA3F)
		KEY(0x48) += KEY(0x14);
	else
		KEY(0x48) += seed;

	if(KEY(0x48) & 1)
		THROW;

	RETURN;
}

THROWS_RET enc_20_49B1F0 (unsigned char *key, unsigned int seed)
{
	KEY(0x04) = ROR(KEY(0x04), (unsigned char)(seed * 0x1592D04));

	if(KEY(0x04) & 1)
		THROW;

	RETURN;
}

THROWS_RET enc_20_4A1640 (unsigned char *key, unsigned int seed)
{
	double var = cos((double)((unsigned char)KEY(0x48)));

	if(var < math_const_2)
		KEY(0x2C) &= 0x146A49CC;
	else
		KEY(0x2C) &= seed;

	if(KEY(0x2C) & 1)
		THROW;

	RETURN;
}

THROWS_RET enc_20_4E7C50 (unsigned char *key, unsigned int seed)
{
	double var = cos((double)((unsigned char)KEY(0x08)));

	if(var < math_const_2)
		KEY(0x08) &= 0x07EBBFDE;
	else
		KEY(0x08) &= KEY(0x2C);

	if(KEY(0x08) & 1)
		THROW;

	RETURN;
}


void enc_20_4C0780 (unsigned char *key, unsigned int seed)
{
	double var = cos((double)((unsigned char)seed));

	if(var < math_const_2)
		KEY(0x34) *= 0x0A02FE00;
	else
		KEY(0x34) *= seed;
}

void enc_20_4C07D0 (unsigned char *key, unsigned int seed)
{
	double var = cos((double)((unsigned char)KEY(0x04)));

	if(var < math_const_2)
		KEY(0x00) |= 0x056E0E99;
	else
		KEY(0x00) |= KEY(0x20);
}

void enc_20_4D4100 (unsigned char *key, unsigned int seed)
{
	double var = cos((double)((unsigned char)KEY(0x3C)));

	if(var < math_const_2)
		KEY(0x48) += 0x10D11D00;
	else
		KEY(0x48) += KEY(0x24);
}

void enc_20_4A12B0 (unsigned char *key, unsigned int seed)
{
	double var = sin((double)((unsigned char)KEY(0x34)));

	if(var < math_const_2)
		KEY(0x08) ^= 0x0FD08092;
	else
		KEY(0x08) ^= KEY(0x28);
}

void enc_20_4D6FB0 (unsigned char *key, unsigned int seed)
{
	double var = cos((double)((unsigned char)KEY(0x3C)));

	if(var < math_const_2)
		KEY(0x28) -= 0x268CCA84;
	else
		KEY(0x28) -= KEY(0x24);
}

void enc_20_4D7000 (unsigned char *key, unsigned int seed)
{
	double var = cos((double)((unsigned char)KEY(0x18)));

	if(var < math_const_2)
		KEY(0x0C) -= 0x2031618A;
	else
		KEY(0x0C) -= KEY(0x20);
}

void enc_20_4D7080 (unsigned char *key, unsigned int seed)
{
	double var = sin((double)((unsigned char)KEY(0x14)));

	if(var < math_const_2)
		KEY(0x04) = ROL(KEY(0x04), (unsigned char)0x1D726264);
	else
		KEY(0x04) = ROL(KEY(0x04), (unsigned char)KEY(0x18));
}

void enc_20_4D76A0 (unsigned char *key, unsigned int seed)
{
	double var = cos((double)((unsigned char)seed));

	if(var < math_const_2)
		KEY(0x30) *= 0x0F44CB55;
	else
		KEY(0x30) *= KEY(0x30);
}

void enc_20_4D7740 (unsigned char *key, unsigned int seed)
{
	double var = cos((double)((unsigned char)seed));

	if(var < math_const_2)
		KEY(0x28) += 0x08958821;
	else
		KEY(0x28) += seed;
}

void enc_20_4E4F30 (unsigned char *key, unsigned int seed)
{
	double var = sin((double)((unsigned char)KEY(0x18)));

	if(var < math_const_2)
		KEY(0x44) = ROR(KEY(0x44), (unsigned char)0x16267C1D);
	else
		KEY(0x44) = ROR(KEY(0x44), (unsigned char)seed);
}

void enc_20_4E7460 (unsigned char *key, unsigned int seed)
{
	double var = cos((double)((unsigned char)KEY(0x24)));

	if(var < math_const_2)
		KEY(0x4C) ^= 0x057337B8;
	else
		KEY(0x4C) ^= KEY(0x38);
}

void enc_20_4F2AA0 (unsigned char *key, unsigned int seed)
{
	double var = cos((double)((unsigned char)seed));

	if(var < math_const_2)
		KEY(0x1C) ^= 0x414517EA;
	else
		KEY(0x1C) ^= seed;
}

void enc_20_4EB2C0 (unsigned char *key, unsigned int seed)
{
	double var = sin((double)((unsigned char)KEY(0x38)));

	if(var < math_const_2)
		KEY(0x3C) ^= 0x40A33FD4;
	else
		KEY(0x3C) ^= seed;
}

void enc_20_4AE3C0 (unsigned char *key, unsigned int seed)
{
	double dvar = sqrt(((double)(((unsigned char)(seed))))+1) + math_const_1;

	KEY(0x24) = ROR(KEY(0x24), (unsigned char)my_ftol(floor(dvar)));
}

void enc_20_4F2A40 (unsigned char *key, unsigned int seed)
{
	double dvar = sqrt(((double)(((unsigned char)(KEY(0x2C)))))+1) + math_const_1;

	KEY(0x1C) ^= my_ftol(floor(dvar));
}

void enc_20_49CED0 (unsigned char *key, unsigned int seed)
{
	double dvar = sqrt((double)(((unsigned char)(KEY(0x1C))))+1) + math_const_1;

	KEY(0x14) += my_ftol(floor(dvar));
}

void enc_20_4D56F0 (unsigned char *key, unsigned int seed)
{
	if(KEY(0x3C) < 0x137BFFEB)
		KEY(0x34) +=	KEY(0x3C);
	else
		KEY(0x34) += KEY(0x2C);
}

void enc_20_4D5740 (unsigned char *key, unsigned int seed)
{
	if(seed < 0x1515FEBD)
		KEY(0x34) -=	seed;
	else
		KEY(0x34) -= KEY(0x04);
}


void enc_20_4D7790 (unsigned char *key, unsigned int seed)
{
	unsigned int edi = 0;

	seed = 0xAE850DE;
	seed += 0x51CE37 - 0x7BD8639;

	KEY(0x40) = ROR(KEY(0x40), (unsigned char)(KEY(0x40) & 0x1BEEB131));

	KEY(0x3C) *= KEY(0x04) ^ 0xD89B4A;

	seed += 0x51CE37 - 0x14DBF2EE;

	KEY(0x0C) ^= KEY(0x2C) * seed;

	seed += 0x51CE37 - 0x6FB4E20;

	if(KEY(0x14) < seed)
		KEY(0x24) = ROL(KEY(0x24), (unsigned char)(KEY(0x14)));
	else
		KEY(0x24) = ROL(KEY(0x24), (unsigned char)(edi));

	edi += KEY(0x24) - seed;

	seed += 0x51CE37 - 0x47C201A4;

	KEY(0x48) *= KEY(0x4C) + seed;

	KEY(0x08) ^= KEY(0x14) < seed ? KEY(0x14) : edi;

	seed += 0x51CE37 - 0x22C6287B;

	KEY(0x0C) += my_ftol(floor(sqrt((double)(((unsigned char)KEY(0x00)))+1) + math_const_1));

	KEY(0x30) = ROL(KEY(0x30), (unsigned char)(ROL(edi, (unsigned char)seed)));

	seed += 0x51CE37 - 0x17512CB4;

	KEY(0x1C) *=  KEY(0x00) < seed ? KEY(0x00) : KEY(0x48);
}

void enc_20_4EB310 (unsigned char *key, unsigned int seed)
{
	unsigned int edi;
	
	seed = 0x3396EF39;
	seed += 0x51CE37 - 0x23940AC;

	KEY(0x10) -= KEY(0x10) ^ 0x692C9EF9;
	KEY(0x48) += KEY(0x00) ^ 0x3CF1856 ;

	seed += 0x51CE37 - 0x30DBAD40;

	KEY(0x44) ^= KEY(0x44) - seed;
	KEY(0x08) = ROL(KEY(0x08), (unsigned char)(KEY(0x1C) + seed));

	seed += 0x51CE37 - 0x3305F2B1;

	KEY(0x08) ^= KEY(0x24) * 0x7941955;

	edi = 0 - seed;
	seed += 0x51CE37 - 0xA87986D;

	KEY(0x28) += KEY(0x04) ^ seed;
	KEY(0x30) *= KEY(0x1C) - seed;

	seed += 0x51CE37 - 0x2D7466CD;

	KEY(0x44) = ROR(KEY(0x44), (unsigned char)(edi - seed));
	KEY(0x34) ^= my_ftol(floor(sqrt((double)(((unsigned char)edi))+1) + math_const_1));

	seed += 0x51CE37 - 0x38C46529;

	if(sin((double)((unsigned char)KEY(0x4C))) < math_const_2)
		KEY(0x0C) *= seed;
	else
		KEY(0x0C) *= KEY(0x14);

	KEY(0x08) -= edi ^ 0x48E12610;
}


THROWS_RET enc_20_major_4EECA0 (unsigned char *key, unsigned int seed)
{
	unsigned int type = (KEY(0x44) ^ KEY(0x10) ^ KEY(0x34)) % 0x0D;
	unsigned int var_18 = 0x3DBD1F8E;

	if(type == 0x09)
	{
		enc_20_49AAE0 (key, 0x3D35976D);
		enc_20_4E4F30 (key, 0x499C9229);
		enc_20_4D7790 (key, KEY(0x4C));
	}

	var_18 += 0x51CE37 - 0x87DF00A;
	KEY(0x2C) &= KEY(0x4C) & 0x170B54ED;

	if(type == 0x0A)
	{
		enc_20_4D9AD0 (key, 0x698084AC);
		enc_20_4D4100 (key, 0x3621E02B);
		TRY(enc_20_major_4AE190 (key, KEY(0x20)));
	}

	if(KEY(0x38) < 0x164D8D96)
		KEY(0x04) = ROR(KEY(0x04), (unsigned char)KEY(0x38));
	else
		KEY(0x04) = ROR(KEY(0x04), (unsigned char)KEY(0x10));

	if(type == 0x0C)
	{
		TRY(enc_20_4AE420 (key, 0xC0948CF0));
		enc_20_4D6FB0 (key, 0x62894845);
		TRY(enc_20_major_4AE490 (key, KEY(0x48)));
	}

	if(type == 0x00)
	{
		enc_20_4D9AD0 (key, 0x36A70C0E);
		enc_20_4E4F30 (key, 0x047FEEB5);
		TRY(enc_20_major_4E7CE0 (key, KEY(0x30)));
	}

	var_18 += 0x51CE37 - 0x2F310ECB;
	KEY(0x18) = ROL(KEY(0x18), (unsigned char)(KEY(0x34) ^ 0x0B31DDE2));

	if(type == 0x06)
	{
		TRY(enc_20_49B1F0 (key, 0x0DA426A1));
		enc_20_4CE300 (key, 0xB9533931);
		TRY(enc_20_major_4B24B0 (key, seed));
	}

	if(sin((double)((unsigned char)seed)) < math_const_2)
		seed += 0x160DF35D;
	else
		seed += seed;

	if(type == 0x03)
	{
		enc_20_4A12B0 (key, 0x1CA7E3E4);
		enc_20_4D7680 (key, 0xDB97EE80);
		TRY(enc_20_major_4E55C0 (key, KEY(0x3C)));
	}

	if(type == 0x00)
	{
		enc_20_4E7CC0 (key, 0xEDEEC7A2);
		enc_20_4D7680 (key, 0x520BD522);
		TRY(enc_20_major_4A16B0 (key, KEY(0x20)));
	}

	var_18 += 0x51CE37 - 0x26FD0898;
	seed &= KEY(0x4C) | var_18;

	if(type == 0x01)
	{
		enc_20_4B0380 (key, 0x40DB13FE);
		enc_20_49B240 (key, 0x45B12337);
		TRY(enc_20_major_4E5960 (key, KEY(0x30)));
	}

	if(sin((double)((unsigned char)seed)) < math_const_2)
		KEY(0x40) += var_18;
	else
		KEY(0x40) += KEY(0x1C);

	if(type == 0x02)
	{
		enc_20_4D56F0 (key, 0x445733FF);
		enc_20_4E7460 (key, 0x2A138AD9);
		TRY(enc_20_major_4DF3C0 (key, seed));
	}

	if(type == 0x07)
	{
		enc_20_4ADBC0 (key, 0x8872E233);
		enc_20_4D7680 (key, 0xE4ABFF42);
		TRY(enc_20_major_49ADF0 (key, KEY(0x38)));
	}

	var_18 += 0x51CE37 - 0x86D479C;
	seed += KEY(0x3C) ^ 0x1777BC26;

	if(type == 0x04)
	{
		enc_20_4E7460 (key, 0xF48F4DF1);
		enc_20_4F2A40 (key, 0x76C2807B);
		TRY(enc_20_major_4E7FD0 (key, KEY(0x48)));
	}

	KEY(0x14) *= my_ftol(floor(sqrt((double)(((unsigned char)KEY(0x24)))+1) + math_const_1));

	if(type == 0x0B)
	{
		enc_20_49B260 (key, 0xCFB45C3E);
		enc_20_4EB2C0 (key, 0x8517AE30);
		TRY(enc_20_major_4E01F0 (key, KEY(0x10)));
	}

	var_18 += 0x51CE37 - 0x8BAD95B;
	KEY(0x34) &= KEY(0x48) - 0x0EB6DEE4;

	if(type == 0x05)
	{
		enc_20_4D7080 (key, 0xDDCAFCBD);
		TRY(enc_20_4D56B0 (key, 0x12F0BCA1));
		enc_20_4D7790 (key, seed);
	}

	if(type == 0x08)
	{
		enc_20_49AAE0 (key, 0x7CABB74D);
		TRY(enc_20_4D7900 (key, 0xAA2C683C));
		TRY(enc_20_major_4AE190 (key, KEY(0x0C)));
	}

	RETURN;
}


THROWS_RET enc_20_major_4F2B40 (unsigned char *key, unsigned int seed)
{
	unsigned int type = KEY(0x28) & 0x0F;
	unsigned int var_18 = 0x0B112A28;

	if(type == 0x05)
	{
		enc_20_4D4100 (key, 0x37339B96);
		enc_20_4B0380 (key, 0x444DAE11);
		TRY(enc_20_major_4B24B0 (key, KEY(0x00)));
	}

	var_18 += 0x51CE37 - 0x883B1A1;
	KEY(0x00) += ((0 - (seed * 3)) * 9) << 1;

	if(type == 0x0D)
	{
		enc_20_49CF30 (key, 0xCA5E7D24);
		enc_20_4D4170 (key, 0x4ACC2885);
		TRY(enc_20_major_4E55C0 (key, seed));
	}

	if(type == 0x0C)
	{
		enc_20_4D5720 (key, 0x6E4C891E);
		enc_20_4BABE0 (key, 0x8CD76CBE);
		TRY(enc_20_major_4A16B0 (key, KEY(0x38)));
	}

	KEY(0x1C) -= KEY(0x20) | 0x1A1A9407;

	if(type == 0x6)
	{
		enc_20_4AE3C0 (key, 0x63EDE696);
		enc_20_49AB20 (key, 0x5C73625D);
		TRY(enc_20_major_4E5960 (key, KEY(0x38)));
	}

	var_18 += 0x51CE37 - 0x19EBB20;
	KEY(0x08) += KEY(0x00) + var_18;

	if(type == 0x8)
	{
		enc_20_4CE300 (key, 0x79943EEC);
		enc_20_4E4F80 (key, 0x0C4E46709);
		TRY(enc_20_major_4DF3C0 (key, seed));
	}

	if(type == 0x0B)
	{
		enc_20_4F2A40 (key, 0x196CCF25);
		enc_20_49AB20 (key, 0x2F105FA3);
		TRY(enc_20_major_49ADF0 (key, KEY(0x3C)));
	}

	KEY(0x40) &= seed -0x1BADCB5;

	if(type == 0x0F)
	{
		enc_20_4AE170 (key, 0x1B1F5B08);
		enc_20_49B260 (key, 0x2A42F19F);
		TRY(enc_20_major_4E7FD0 (key, KEY(0x0C)));
	}

	var_18 += 0x51CE37 - 0x2166EDD;

	if(cos((double)((unsigned char)KEY(0x10))) < math_const_2)
		KEY(0x14) -= var_18;
	else
		KEY(0x14) -= KEY(0x38);

	if(type == 0x1)
	{
		enc_20_4D5740 (key, 0x92ED31DA);
		enc_20_49AAE0 (key, 0x44A3D725);
		TRY(enc_20_major_4E01F0 (key, KEY(0x24)));
		TRY(enc_20_4AE420 (key, 0x149A97A0));
		TRY(enc_20_4D76F0 (key, 0x0D87D888E));
		TRY(enc_20_major_4EECA0 (key, KEY(0x24)));
	}

	KEY(0x14) *= KEY(0x20) + var_18;

	if(type == 0x4)
	{
		TRY(enc_20_4D76F0 (key, 0x130AA218));
		enc_20_4D7680 (key, 0x0E019C2D2);
		TRY(enc_20_major_4DA520 (key, KEY(0x18)));
	}

	var_18 += 0x51CE37 - 0x425B930B;

	if(KEY(0x3C) < var_18)
		KEY(0x04) = ROL(KEY(0x04), (unsigned char)KEY(0x3C));
	else
		KEY(0x04) = ROL(KEY(0x04), (unsigned char)KEY(0x24));

	if(type == 0x0E)
	{
		enc_20_4D7080 (key, 0x6E0C4FC0);
		enc_20_4AB3B0 (key, 0x790B68AA);
		TRY(enc_20_major_4B19A0 (key, KEY(0x14)));
	}

	if(type == 0x0)
	{
		enc_20_4A1AE0 (key, 0x1897846A);
		TRY(enc_20_4DA120 (key, 0x2C09E10B));
		TRY(enc_20_major_4C4AC0 (key, seed));
	}

	KEY(0x18) += KEY(0x0C) * 121;

	if(type == 0x9)
	{
		enc_20_4EB2A0 (key, 0x63BCA480);
		enc_20_4D7080 (key, 0x0D093CCF3);
		TRY(enc_20_major_4B24B0 (key, KEY(0x18)));
	}

	var_18 += 0x51CE37 - 0x52A4B65;

	if(cos((double)((unsigned char)KEY(0x1C))) < math_const_2)
		KEY(0x40) ^= 0x2D36F243;
	else
		KEY(0x40) ^= KEY(0x34);

	if(type == 0x0)
	{
		enc_20_4DA1E0 (key, 0x19A85EB1);
		enc_20_4D6FB0 (key, 0x6193F5BD);
		TRY(enc_20_major_4E55C0 (key, KEY(0x8)));
	}

	if(type == 0x7)
	{
		enc_20_4D7740 (key, 0x63208F9F);
		TRY(enc_20_4AE420 (key, 0x115E64D4));
		TRY(enc_20_major_4A16B0 (key, KEY(0x4C)));
	}

	KEY(0x38) &= KEY(0x0C) ^ var_18;

	if(type == 0x0A)
	{
		enc_20_4D5740 (key, 0x5EE26DDE);
		enc_20_4E4F80 (key, 0x282244A8);
		TRY(enc_20_major_4E5960 (key, KEY(0x20)));
	}

	var_18 += 0x51CE37 - 0x22679FC0;
	KEY(0x04) = ROR(KEY(0x04), (unsigned char)(KEY(0x30) * 101));

	if(type == 0x3)
	{
		enc_20_4A12B0 (key, 0x1545E592);
		enc_20_4A12B0 (key, 0x662CEFC1);
		TRY(enc_20_major_4DF3C0 (key, KEY(0x3C)));
	}

	if(type == 0x2)
	{
		enc_20_4BABE0 (key, 0x785A1B8A);
		enc_20_4C0780 (key, 0x0C0D70A49);
		TRY(enc_20_major_49ADF0 (key, KEY(0x38)));
	}

	if(sin((double)((unsigned char)KEY(0x00))) < math_const_2)
		KEY(0x30) ^= var_18;
	else
		KEY(0x30) ^= KEY(0x14);

	RETURN;
}


THROWS_RET enc_20_major_49ADF0 (unsigned char *key, unsigned int seed)
{
	unsigned int type = (KEY(0x14) ^ seed ^ KEY(0x30)) % 0x0A;
	unsigned int var_14 = 0x3074A456;
    
	var_14 += 0x51CE37 - 0x18D0A343;
	seed *= KEY(0x18) | 0x04723B25;

	if(type == 0x0)
	{
		enc_20_4F2AF0 (key, 0x46147EFF);
		TRY(enc_20_4DA120 (key, 0x41F46707));
		enc_20_4EB310 (key, KEY(0x18));
	}

	KEY(0x08) += 0 - (((KEY(0x10) * 8 - KEY(0x10)) << 1) - KEY(0x10));

	if(type == 0x5)
	{
		enc_20_4F2AA0 (key, 0x18518B22);
		enc_20_4D7000 (key, 0x0C664EC68);
		enc_20_4D7790 (key, KEY(0x0C));
	}

	var_14 += 0x51CE37 - 0x0C736D91;
	seed = (KEY(0x30) * 5 + seed) + (KEY(0x30) * 20);

	if(type == 0x1)
	{
		enc_20_4D56F0 (key, 0x5A606CDE);
		enc_20_4E7CC0 (key, 0x0A9C4C3E2);
		TRY(enc_20_major_4AE190 (key, seed));
	}

	seed += KEY(0x1C) + var_14;

	if(type == 0x2)
	{
		enc_20_4D7000 (key, 0x53DCBD4D);
		enc_20_4A1B00 (key, 0x122ECD9D);
		TRY(enc_20_major_4AE490 (key, seed));
	}

	var_14 += 0x51CE37 - 0x6C5479A8;
	KEY(0x3C) -= KEY(0x00) ^ 0x16BEE8C4;

	if(type == 0x4)
	{
		TRY(enc_20_4DA4D0 (key, 0x8B6772));
		enc_20_4D6FB0 (key, 0x56EE72CC);
		TRY(enc_20_major_4E7CE0 (key, seed));
	}

	KEY(0x48) ^= KEY(0x2C) + var_14;

	if(type == 0x6)
	{
		enc_20_4A1AE0 (key, 0x56945DD9);
		enc_20_4A1AE0 (key, 0x67A46509);
		TRY(enc_20_major_4B24B0 (key, seed));
	}

	var_14 += 0x51CE37 - 0x1BF2C739;
	KEY(0x38) = ROL(KEY(0x38), (unsigned char)(KEY(0x4C)-0x8C1CE40));

	if(type == 0x8)
	{
		enc_20_4D7080 (key, 0x0E07FD40E);
		enc_20_4ABC20 (key, 0x0EA6B51A8);
		TRY(enc_20_major_4E55C0 (key, seed));
	}

	KEY(0x00) = ROR(KEY(0x00), (unsigned char)(KEY(0x34) * 83));

	if(type == 0x9)
	{
		TRY(enc_20_4BAC10 (key, 0x70DA1D6F));
		enc_20_4D7000 (key, 0x516D164);
		TRY(enc_20_major_4A16B0 (key, seed));
	}

	var_14 += 0x51CE37 - 0x31D3FEFD;

	if(KEY(0x40) < 0x33671DE9)
		seed ^= KEY(0x40);
	else
		seed ^= KEY(0x44);

	if(type == 0x7)
	{
		enc_20_4F2AF0 (key, 0x70D19E4B);
		TRY(enc_20_4D56B0 (key, 0x0ABF1EA65));
		TRY(enc_20_major_4E5960 (key, KEY(0x14)));
	}

	seed &= seed << 6;

	if(type == 0x3)
	{
		enc_20_4D70D0 (key, 0x80B6D40E);
		enc_20_4E4F80 (key, 0x2CFE2CC2);
		TRY(enc_20_major_4DF3C0 (key, KEY(0x4C)));
	}

	RETURN;
}


THROWS_RET enc_20_major_4A16B0 (unsigned char *key, unsigned int seed)
{
	unsigned int type = KEY(0x18) % 7;
	unsigned int var_14 = 0x8A39523;

	var_14 += 0x51CE37 - 0x17F6642;
	seed ^= ROL(KEY(0x0C), 0x72);

	if(type == 0x6)
	{
		enc_20_4A1AE0 (key, 0x0A788E3DC);
		TRY(enc_20_4DA120 (key, 0x2059B605));
		enc_20_4EB310 (key, KEY(0x1C));
	}

	KEY(0x3C) += (seed * 25) << 1;

	var_14 += 0x51CE37 - 0x12202C25;
	KEY(0x14) += 0x0C93495E4 - KEY(0x38);

	if(type == 0x2)
	{
		TRY(enc_20_4D76F0 (key, 0x10DB4A9D));
		enc_20_4D4170 (key, 0x3D54F7F4);
		enc_20_4D7790 (key, KEY(0x00));
	}

	if(cos((double)((unsigned char)KEY(0x38))) < math_const_2)
		KEY(0x30) *= var_14;
	else
		KEY(0x30) *= KEY(0x44);

	if(type == 0x0)
	{
		enc_20_4D70D0 (key, 0x818E9FE0);
		enc_20_49B240 (key, 0x31D0CADA);
		TRY(enc_20_major_4AE190 (key, KEY(0x20)));
	}

	var_14 += 0x51CE37 - 0x15CE1247;
	KEY(0x18) &= KEY(0x1C) | var_14;

	if(cos((double)((unsigned char)KEY(0x00))) < math_const_2)
		KEY(0x2C) ^= 0x3A2C762B;
	else
		KEY(0x2C) ^= seed;

	if(type == 0x4)
	{
		TRY(enc_20_4D56B0 (key, 0x1080EF45));
		TRY(enc_20_49B1F0 (key, 0x44BF5D5F));
		TRY(enc_20_major_4AE490 (key, seed));
	}

	var_14 += 0x51CE37 - 0x128DFD61;
	KEY(0x0C) -= my_ftol(floor(sqrt((double)(((unsigned char)KEY(0x24)))+1) + math_const_1));

	if(type == 0x5)
	{
		enc_20_4D5720 (key, 0x0DF523ED5);
		enc_20_4DA1E0 (key, 0x1B51F3D0);
		TRY(enc_20_major_4E7CE0 (key, KEY(0x48)));
	}

	if(cos((double)((unsigned char)KEY(0x1C))) < math_const_2)
		seed &= var_14;
	else
		seed &= KEY(0x0C);

	var_14 += 0x51CE37 - 0x3DE94AC6;
	KEY(0x00) += 0 - ((((KEY(0x3C) << 4) + KEY(0x3C)) << 2) - KEY(0x3C));

	if(type == 0x1)
	{
		enc_20_4E7CC0 (key, 0x8F58F140);
		enc_20_4D5720 (key, 0x1D251133);
		TRY(enc_20_major_4B24B0 (key, KEY(0x0C)));
	}

	KEY(0x04) -=  ROR(KEY(0x48), 0x33);
	var_14 += 0x51CE37 - 0x26BE8A9B;

	if(sin((double)((unsigned char)KEY(0x38))) < math_const_2)
		KEY(0x44) ^= var_14;
	else
		KEY(0x44) ^= KEY(0x40);

	if(type == 0x0)
	{
		enc_20_49AB20 (key, 0x209DDB7B);
		enc_20_49CF30 (key, 0x217BBC5B);
		TRY(enc_20_major_4E55C0 (key, KEY(0x38)));
	}

	RETURN;
}


THROWS_RET enc_20_major_4ABC60 (unsigned char *key, unsigned int seed)
{
	unsigned int type = (KEY(0x34) ^ KEY(0x18) ^ KEY(0x40)) & 0x0F;
	unsigned int var_18 = 0x0FE1D15;

	if(type == 0x7)
	{
		enc_20_4E7FB0 (key, 0x21B598E);
		enc_20_4C5040 (key, 0x4AC1E03F);
		TRY(enc_20_major_4E55C0 (key, KEY(0x3C)));
	}

	var_18 += 0x51CE37 - 0x20B0889D;
	KEY(0x08) ^= KEY(0x3C) - var_18;

	if(type == 0x0F)
	{
		enc_20_4E4F30 (key, 0x0C7337307);
		enc_20_49B240 (key, 0x8901D037);
		TRY(enc_20_major_4A16B0 (key, KEY(0x28)));
	}

	if(type == 0x0E)
	{
		enc_20_4D7660 (key, 0x32D5440A);
		enc_20_4D4150 (key, 0x754F1FBD);
		TRY(enc_20_major_4E5960 (key, seed));
	}

	KEY(0x14) += KEY(0x20) * 73;

	if(type == 0x4)
	{
		enc_20_4AE170 (key, 0x92B46703);
		TRY(enc_20_4EEC50 (key, 0x0F89E7111));
		TRY(enc_20_major_4DF3C0 (key, KEY(0x4C)));
	}

	if(type == 0x1)
	{
		enc_20_4D4100 (key, 0x10224DAA);
		enc_20_4D70D0 (key, 0x817539A8);
		TRY(enc_20_major_49ADF0 (key, KEY(0x10)));
	}

	var_18 += 0x51CE37 - 0x0B2F7E4D;

	if(KEY(0x40) < 0x4DFE57F8)
		seed += KEY(0x40);
	else
		seed += KEY(0x44);

	if(type == 0x2)
	{
		enc_20_4AB3B0 (key, 0x74E11AD9);
		enc_20_4D9AD0 (key, 0x74004C0A);
		TRY(enc_20_major_4E7FD0 (key, KEY(0x2C)));
		enc_20_4D7680 (key, 0x93B24D42);
		enc_20_4E7FB0 (key, 0x1CBDFBFC);
		TRY(enc_20_major_4E01F0 (key, KEY(0x28)));
	}

	KEY(0x14) += KEY(0x18) + var_18;

	if(type == 0x0A)
	{
		enc_20_4E7460 (key, 0x4147E424);
		enc_20_49B240 (key, 0x2AE05891);
		TRY(enc_20_major_4EECA0 (key, KEY(0x28)));
	}

	if(type == 0x0C)
	{
		enc_20_4A1B00 (key, 0x1313A094);
		enc_20_4E7FB0 (key, 0x7CBD66C);
		TRY(enc_20_major_4DA520 (key, KEY(0x20)));
	}

	var_18 += 0x51CE37 - 0x1B56493C;

	if(sin((double)((unsigned char)seed)) < math_const_2)
		KEY(0x04) += var_18;
	else
		KEY(0x04) += seed;

	if(type == 0x6)
	{
		enc_20_4A1AC0 (key, 0x51CF3579);
		enc_20_4D5740 (key, 0x8E701496);
		TRY(enc_20_major_4B19A0 (key, KEY(0x38)));
	}

	if(type == 0x3)
	{
		enc_20_4D56F0 (key, 0x4A27019E);
		enc_20_4E4F80 (key, 0x0D32078E1);
		TRY(enc_20_major_4C4AC0 (key, seed));
	}

	if(cos((double)((unsigned char)KEY(0x0C))) < math_const_2)
		KEY(0x0C) |= var_18;
	else
		KEY(0x0C) |= KEY(0x44);

	if(type == 0x9)
	{
		enc_20_4D70D0 (key, 0x52922622);
		enc_20_4D4150 (key, 0x0F6E01421);
		TRY(enc_20_major_4F2B40 (key, seed));
	}

	if(type == 0x0D)
	{
		enc_20_49B260 (key, 0x78EBCA1E);
		enc_20_4ADBC0 (key, 0x70AA0B53);
		TRY(enc_20_major_4E55C0 (key, seed));
	}

	var_18 += 0x51CE37 - 0x8B3397E;
	seed ^= KEY(0x08) * var_18;

	if(type == 0x0B)
	{
		enc_20_4D7050 (key, 0x40FCBC29);
		enc_20_4B0380 (key, 0x403D2702);
		TRY(enc_20_major_4A16B0 (key, KEY(0x44)));
	}

	if(type == 0x0)
	{
		enc_20_4D9AD0 (key, 0x29770C45);
		enc_20_4E4F30 (key, 0x4FF6E927);
		TRY(enc_20_major_4E5960 (key, seed));
	}

	KEY(0x20) += 0xF1030E9C - KEY(0x30);

	if(type == 0x5)
	{
		enc_20_4D5720 (key, 0x0C571A70B);
		enc_20_4D7050 (key, 0x421EBAD1);
		TRY(enc_20_major_4DF3C0 (key, seed));
	}

	if(type == 0x8)
	{
		enc_20_4BABE0 (key, 0x7D6B64A8);
		enc_20_49CF30 (key, 0x0CBF6F6FA);
		TRY(enc_20_major_49ADF0 (key, KEY(0x44)));
	}

	var_18 += 0x51CE37 - 0x9BA58BD;
	KEY(0x3C) += var_18 - KEY(0x04);

	if(type == 0x0)
	{
		enc_20_49CF30 (key, 0x47A3A769);
		enc_20_49AB20 (key, 0x57BC6749);
		TRY(enc_20_major_4E7FD0 (key, seed));
	}

	if(type == 0x1)
	{
		TRY(enc_20_4D56B0 (key, 0x44A7AB9D));
		enc_20_4D76A0 (key, 0x824340CE);
		TRY(enc_20_major_4E01F0 (key, KEY(0x30)));
	}

	KEY(0x18) *= KEY(0x14) * 29;

	RETURN;
}


THROWS_RET enc_20_major_4B19A0 (unsigned char *key, unsigned int seed)
{
	unsigned int type = KEY(0x44) % 0x0F;
	unsigned int var_18 = 0x10572198;

	if(type == 0x00)
	{
		enc_20_4D56F0 (key, 0x0A849904);
		enc_20_4AB3B0 (key, 0x1CDB9F1C);
		TRY(enc_20_major_4AE490 (key, KEY(0x20)));
	}

	var_18 += 0x51CE37 - 0x2EA3BDA3;

	if(sin((double)((unsigned char)KEY(0x24))) < math_const_2)
		KEY(0x2C) -= var_18;
	else
		KEY(0x2C) -= KEY(0x1C);

	if(type == 0x0D)
	{
		TRY(enc_20_49B1F0 (key, 0x422E82A9));
		enc_20_4D9AD0 (key, 0x0C11A1416);
		TRY(enc_20_major_4E7CE0 (key, KEY(0x00)));
	}

	KEY(0x28) -= KEY(0x18) ^ 0x1289DE2;

	if(type == 0x8)
	{
		enc_20_4AE3C0 (key, 0x8FED4B73);
		TRY(enc_20_4E7C50 (key, 0x692C266A));
		TRY(enc_20_major_4B24B0 (key, KEY(0x10)));
	}

	if(type == 0x5)
	{
		enc_20_4AB3B0 (key, 0x6492D146);
		enc_20_4EE700 (key, 0x7C148C81);
		TRY(enc_20_major_4E55C0 (key, seed));
	}

	var_18 += 0x51CE37 - 0x1A21C37A;
	KEY(0x40) = ROL(KEY(0x40), (unsigned char)my_ftol(floor(sqrt((double)(((unsigned char)KEY(0x44)))+1) + math_const_1)));

	if(type == 0x2)
	{
		enc_20_4A1B00 (key, 0x1C73063B);
		enc_20_4A1B00 (key, 0x848F08E);
		TRY(enc_20_major_4A16B0 (key, KEY(0x40)));
	}

	KEY(0x24) += my_ftol(floor(sqrt((double)(((unsigned char)KEY(0x0C)))+1) + math_const_1));

	if(type == 0x0E)
	{
		enc_20_4AE3C0 (key, 0xA3D7A7FC);
		enc_20_4D5740 (key, 0x27AA3A54);
		TRY(enc_20_major_4E5960 (key, seed));
	}

	var_18 += 0x51CE37 - 0x5B8499EE;
	seed = KEY(0x18) ^ seed ^ 0x202AB323;

	if(type == 0x9)
	{
		enc_20_4C47D0 (key, 0x7C52976B);
		enc_20_4ADBC0 (key, 0x9ECB80E1);
		TRY(enc_20_major_4DF3C0 (key, KEY(0x20)));
	}

	if(type == 0x6)
	{
		enc_20_49CF30 (key, 0x0C09FA75F);
		enc_20_4A1AE0 (key, 0x75D1B938);
		TRY(enc_20_major_49ADF0 (key, KEY(0x40)));
	}

	KEY(0x3C) ^= my_ftol(floor(sqrt((double)(((unsigned char)KEY(0x28)))+1) + math_const_1));

	if(type == 0x1)
	{
		TRY(enc_20_4BAC10 (key, 0x0B30D40D0));
		enc_20_49ADD0 (key, 0x74E6601F);
		TRY(enc_20_major_4E7FD0 (key, KEY(0x34)));
	}

	var_18 += 0x51CE37 - 0x1DDA929B;
	KEY(0x00) -= KEY(0x2C) ^ 0x1284AF29;

	if(type == 0x4)
	{
		enc_20_4C47D0 (key, 0x0D16FD674);
		enc_20_4D7000 (key, 0x48FFB1AB);
		TRY(enc_20_major_4E01F0 (key, KEY(0x44)));
	}

	seed = ROL(seed, (unsigned char)((KEY(0x2C) * 8 - KEY(0x2C)) << 4));

	if(type == 0x0B)
	{
		enc_20_4D7660 (key, 0x2EC1F3E2);
		TRY(enc_20_49AA90 (key, 0x13EE15C3));
		TRY(enc_20_major_4EECA0 (key, KEY(0x4C)));
	}

	if(type == 0x0)
	{
		TRY(enc_20_4D56B0 (key, 0x91A8A09D));
		enc_20_4E7460 (key, 0x4817AA6D);
		TRY(enc_20_major_4DA520 (key, KEY(0x40)));
	}

	var_18 += 0x51CE37 - 0x5A8079C;
	KEY(0x24) |= KEY(0x24) ^ 0x2AD7629;

	if(type == 0x0A)
	{
		TRY(enc_20_49B1F0 (key, 0x4CDAADC3));
		TRY(enc_20_49AA90 (key, 0x0E8869877));
		TRY(enc_20_major_4AE490 (key, seed));
	}

	KEY(0x10) *= KEY(0x30) * var_18;

	if(type == 0x0C)
	{
		enc_20_4C5040 (key, 0x13C4504A);
		TRY(enc_20_4DA4D0 (key, 0x0A89CC1F8));
		TRY(enc_20_major_4E7CE0 (key, KEY(0x14)));
	}

	if(type == 0x7)
	{
		enc_20_4AE170 (key, 0x1B0B87FF);
		TRY(enc_20_49AA90 (key, 0x0DD1CA541));
		TRY(enc_20_major_4B24B0 (key, KEY(0x4)));
	}

	var_18 += 0x51CE37 - 0x2DE8F3D8;
	seed *= KEY(0x10) + 0x76E5A087;

	if(type == 0x3)
	{
		TRY(enc_20_4DA120 (key, 0x330EDC53));
		TRY(enc_20_4AE420 (key, 0x62F4D3C4));
		TRY(enc_20_major_4E55C0 (key, seed));
	}

	RETURN;
}


THROWS_RET enc_20_major_4BAC60 (unsigned char *key, unsigned int seed)
{
	unsigned int type = (KEY(0x28) ^ KEY(0x2C) ^ KEY(0x48)) & 0x0F;
	unsigned int var_18 = 0x29E4785;

	if(type == 0x3)
	{
		enc_20_4D4100 (key, 0x54251ED6);
		TRY(enc_20_4BAC10 (key, 0x54BCDE17));
		TRY(enc_20_major_4EECA0 (key, KEY(0x38)));
	}

	if(type == 0x9)
	{
		enc_20_4B0380 (key, 0x6268FE39);
		enc_20_4D5740 (key, 0x849633D3);
		TRY(enc_20_major_4DA520 (key, KEY(0x30)));
	}

	var_18 += 0x51CE37 - 0x3C00B73C;
	KEY(0x20) |= var_18 + KEY(0x04);

	if(type == 0x2)
	{
		enc_20_4A1AE0 (key, 0x72FA0344);
		enc_20_4D4170 (key, 0x3875F5B9);
		TRY(enc_20_major_4B19A0 (key, KEY(0x40)));
	}

	if(type == 0x5)
	{
		enc_20_4D4150 (key, 0x0D553467F);
		enc_20_4A1B00 (key, 0x32779D73);
		TRY(enc_20_major_4C4AC0 (key, KEY(0x14)));
	}

	if(type == 0x1)
	{
		enc_20_4D56F0 (key, 0x4F92773B);
		enc_20_4AE170 (key, 0x0EA665860);
		TRY(enc_20_major_4F2B40 (key, KEY(0x0C)));
	}

	KEY(0x3C) -= var_18 + KEY(0x4C);

	if(type == 0x4)
	{
		enc_20_4E4F80 (key, 0x703EED12);
		enc_20_4D6FB0 (key, 0x68EE704A);
		TRY(enc_20_major_4ABC60 (key, KEY(0x1C)));
	}

	if(type == 0x1)
	{
		enc_20_4F2B20 (key, 0x0F48B6D0F);
		enc_20_4E4F30 (key, 0x0CA4795D5);
		TRY(enc_20_major_4D4910 (key, KEY(0x40)));
	}

	if(type == 0x9)
	{
		enc_20_4C47D0 (key, 0x0B4225230);
		enc_20_4EB2A0 (key, 0x70E051EB);
		TRY(enc_20_major_4D1E70 (key, KEY(0x4C)));
	}

	var_18 += 0x51CE37 - 0x28A8FE0;
	seed -= KEY(0x00) ^ 0x3B61016B;

	if(type == 0x4)
	{
		enc_20_4D4150 (key, 0x0F72B306F);
		TRY(enc_20_49AA90 (key, 0x1984A749));
		TRY(enc_20_major_4D4FB0 (key, KEY(0x0C)));
	}

	if(type == 0x3)
	{
		enc_20_4C5040 (key, 0x42129AEB);
		enc_20_4D6FB0 (key, 0x7C864AA1);
		TRY(enc_20_major_4EB610 (key, KEY(0x10)));
	}

	if(type == 0x7)
	{
		TRY(enc_20_4D56B0 (key, 0x572869D1));
		enc_20_4E4F30 (key, 0x1C91FACD);
		TRY(enc_20_major_4C1240 (key, KEY(0x14)));
	}

	KEY(0x2C) = ROL(KEY(0x2C), (unsigned char)(KEY(0x28) ^ 0x469FB6FA));

	if(type == 0x8)
	{
		enc_20_4EE700 (key, 0x0DB7F42BA);
		enc_20_4D7050 (key, 0x2EEEE6EE);
		TRY(enc_20_major_4CF650 (key, seed));
	}

	if(type == 0x0E)
	{
		enc_20_4D7050 (key, 0x3EEA9094);
		enc_20_4AE3C0 (key, 0x0BFD9F90B);
		TRY(enc_20_major_4EECA0 (key, seed));
	}

	var_18 += 0x51CE37 - 0x4DA526F;
	seed -= KEY(0x38) * var_18;

	if(type == 0x0)
	{
		enc_20_4D5720 (key, 0x6FC267C3);
		enc_20_4D4100 (key, 0x1B511ED4);
		TRY(enc_20_major_4DA520 (key, KEY(0x18)));
	}

	if(type == 0x0D)
	{
		enc_20_4C4A60 (key, 0x32230D49);
		enc_20_4E4F80 (key, 0x7D753B3A);
		TRY(enc_20_major_4B19A0 (key, KEY(0x10)));
	}

	if(type == 0x8)
	{
		enc_20_4D1E50 (key, 0x0BA143EFE);
		enc_20_4D5740 (key, 0x2F19769B);
		TRY(enc_20_major_4C4AC0 (key, seed));
	}

	KEY(0x10) += 0xA207344D - seed;

	if(type == 0x0C)
	{
		TRY(enc_20_4BAC10 (key, 0x80A1DA17));
		enc_20_4C0780 (key, 0x0E8630EDE);
		TRY(enc_20_major_4F2B40 (key, KEY(0x0C)));
	}

	if(type == 0x2)
	{
		enc_20_4AB3B0 (key, 0x64A3D689);
		enc_20_4D5720 (key, 0x0E5C778D8);
		TRY(enc_20_major_4ABC60 (key, KEY(0x24)));
	}

	if(type == 0x0)
	{
		enc_20_4E4F30 (key, 0x4A5DE3AB);
		enc_20_4C47D0 (key, 0x1D5FBD04);
		TRY(enc_20_major_4D4910 (key, KEY(0x14)));
	}

	var_18 += 0x51CE37 - 0x352F1985;
	seed ^= KEY(0x48) ^ 0xE6830C9;

	if(type == 0x6)
	{
		enc_20_49B240 (key, 0x0DCF14366);
		enc_20_4B0380 (key, 0x590DA5F9);
		TRY(enc_20_major_4D1E70 (key, seed));
	}

	if(type == 0x7)
	{
		enc_20_49AB20 (key, 0x65C7C287);
		TRY(enc_20_4BAC10 (key, 0x0B11DA063));
		TRY(enc_20_major_4D4FB0 (key, KEY(0x0C)));
	}

	if(type == 0x0F)
	{
		enc_20_4F2A40 (key, 0x0AC8747EC);
		enc_20_4A1AC0 (key, 0x3CC69EC8);
		TRY(enc_20_major_4EB610 (key, KEY(0x44)));
	}

	if(sin((double)((unsigned char)seed)) < math_const_2)
		KEY(0x00) ^= var_18;
	else
		KEY(0x00) ^= KEY(0x20);


	if(type == 0x0B)
	{
		enc_20_4D6FB0 (key, 0x72BA7A8C);
		enc_20_4E4F80 (key, 0x1BB3C88C);
		TRY(enc_20_major_4C1240 (key, seed));
	}

	if(type == 0x5)
	{
		enc_20_4C5040 (key, 0x56D54F31);
		enc_20_4B0380 (key, 0x5AF9BA99);
		TRY(enc_20_major_4CF650 (key, KEY(0x40)));
	}

	var_18 += 0x51CE37 - 0x50402411;
	seed += ((KEY(0x04) << 5) - KEY(0x04)) * 2;

	if(type == 0x6)
	{
		enc_20_4F2A40 (key, 0x0DB8A89D8);
		enc_20_49B260 (key, 0x39DB8DF7);
		TRY(enc_20_major_4EECA0 (key, KEY(0x30)));
	}

	if(type == 0x0A)
	{
		enc_20_4F2AF0 (key, 0x4C0E5D7F);
		enc_20_4D9AD0 (key, 0x0F8184179);
		TRY(enc_20_major_4DA520 (key, seed));
	}

	KEY(0x04) ^= KEY(0x08) & var_18;

	RETURN;
}


THROWS_RET enc_20_major_4C1240 (unsigned char *key, unsigned int seed)
{
	unsigned int type = (KEY(0x08) ^ seed ^ KEY(0x44)) & 0x0F;
	unsigned int var_18 = 0x16332817;

	if(type == 0x7)
	{
		enc_20_4D5740 (key, 0x0BBE903B1);
		enc_20_4C07D0 (key, 0x0E9B6FDB);
		TRY(enc_20_major_4E7FD0 (key, seed));
	}

	if(type == 0x0)
	{
		enc_20_49CED0 (key, 0x0D1F2733B);
		TRY(enc_20_4BAC10 (key, 0x0E0B52E33));
		TRY(enc_20_major_4E01F0 (key, KEY(0x3C)));
	}

	var_18 += 0x51CE37 - 0x330C4928;
	seed -= ROR(KEY(0x08), (unsigned char)var_18);

	if(type == 0x5)
	{
		enc_20_4A1AC0 (key, 0x355281E1);
		enc_20_4C47D0 (key, 0x0DE72F125);
		TRY(enc_20_major_4EECA0 (key, KEY(0x30)));
	}

	if(type == 0x1)
	{
		enc_20_4F2AF0 (key, 0x40D80BF7);
		enc_20_4D70D0 (key, 0x86A8CA76);
		TRY(enc_20_major_4DA520 (key, KEY(0x4)));
	}

	KEY(0x30) += (0 - (((KEY(0x44) * 15) << 1) - KEY(0x44))) << 2;

	if(type == 0x1)
	{
		enc_20_4ABC20 (key, 0x20D3630);
		enc_20_4D7680 (key, 0x2D8C0262);
		TRY(enc_20_major_4B19A0 (key, KEY(0x14)));
	}

	if(type == 0x0E)
	{
		enc_20_4A1AC0 (key, 0x35880FD2);
		TRY(enc_20_4DA4D0 (key, 0x0C0074301));
		TRY(enc_20_major_4C4AC0 (key, seed));
	}

	var_18 += 0x51CE37 - 0x1A275E87;
	KEY(0x0C) ^= KEY(0x1C) + 0x137C9F7D;

	if(type == 0x0)
	{
		enc_20_4B0380 (key, 0x542636C4);
		enc_20_4E4F80 (key, 0x78B17F64);
		TRY(enc_20_major_4F2B40 (key, KEY(0x14)));
	}

	if(type == 0x4)
	{
		enc_20_4D7740 (key, 0x0AF2FBD41);
		enc_20_4A1B00 (key, 0x2FFD0A28);
		TRY(enc_20_major_4ABC60 (key, KEY(0x10)));
	}

	if(type == 0x3)
	{
		enc_20_49AB20 (key, 0x2BF3386F);
		enc_20_49B240 (key, 0x0BEF9A336);
		TRY(enc_20_major_4D4910 (key, KEY(0x30)));
	}

	if(KEY(0x44) < var_18)
		KEY(0x2C) += KEY(0x44);
	else
		KEY(0x2C) += KEY(0x10);
		
	if(type == 0x4)
	{
		enc_20_4D7660 (key, 0x2F26A2DC);
		enc_20_4D9AD0 (key, 0x164AA05D);
		TRY(enc_20_major_4D1E70 (key, KEY(0x24)));
	}

	if(type == 0x0D)
	{
		enc_20_49AAE0 (key, 0x364F8AC9);
		TRY(enc_20_4E7C50 (key, 0x6D071F3));
		TRY(enc_20_major_4D4FB0 (key, KEY(0x34)));
	}

	var_18 += 0x51CE37 - 0xC5AF489;
	seed *= KEY(0x30) + var_18;

	if(type == 0x7)
	{
		enc_20_4A1AE0 (key, 0x1B717676);
		enc_20_4E7460 (key, 0x4807D3BA);
		TRY(enc_20_major_4EB610 (key, seed));
	}

	if(type == 0x8)
	{
		enc_20_4AB3B0 (key, 0x17CCEE88);
		enc_20_4D7080 (key, 0x0E0885C3);
		TRY(enc_20_major_4E7FD0 (key, seed));
	}

	KEY(0x28) += 0xAA3373FC - KEY(0x18); 

	if(type == 0x5)
	{
		enc_20_4E4F30 (key, 0x0AE768CE);
		TRY(enc_20_4AE420 (key, 0x0BC90D50));
		TRY(enc_20_major_4E01F0 (key, seed));
	}

	if(type == 0x0F)
	{
		enc_20_49B240 (key, 0x3808522D);
		enc_20_4DA1E0 (key, 0x9E44B41);
		TRY(enc_20_major_4EECA0 (key, seed));
	}

	if(type == 0x6)
	{
		enc_20_4F2A40 (key, 0x4904087);
		enc_20_4A12B0 (key, 0x16B508FA);
		TRY(enc_20_major_4DA520 (key, seed));
	}

	var_18 += 0x51CE37 - 0x190E1B61;

	if(KEY(0x2C) < var_18)
		seed ^= KEY(0x2C);
	else
		seed ^= KEY(0x08);

	if(type == 0x6)
	{
		enc_20_4A1B00 (key, 0x0D0A586F);
		enc_20_4D70D0 (key, 0x0C615F9A3);
		TRY(enc_20_major_4B19A0 (key, KEY(0x14)));
	}

	if(type == 0x0C)
	{
		TRY(enc_20_4A1640 (key, 0x0B6571D3F));
		enc_20_4F2B20 (key, 0x797DB408);
		TRY(enc_20_major_4C4AC0 (key, KEY(0x24)));
	}

	seed *= KEY(0x38) + 0x9BAA8DB;

	if(type == 0x9)
	{
		TRY(enc_20_49AA90 (key, 0x0E378A0ED));
		enc_20_4C47D0 (key, 0x0BD2055E8);
		TRY(enc_20_major_4F2B40 (key, KEY(0x20)));
	}

	if(type == 0x0B)
	{
		TRY(enc_20_4D76F0 (key, 0x0BD149BD9));
		enc_20_4EB2C0 (key, 0x6476F303);
		TRY(enc_20_major_4ABC60 (key, seed));
	}

	var_18 += 0x51CE37 - 0x11E8B829;
	KEY(0x44) += my_ftol(floor(sqrt((double)(((unsigned char)KEY(0x30)))+1) + math_const_1));

	if(type == 0x0A)
	{
		enc_20_4C0780 (key, 0x5C95FB35);
		enc_20_4ABC20 (key, 0x0D4EE8E0E);
		TRY(enc_20_major_4D4910 (key, KEY(0x4C)));
	}

	if(type == 0x3)
	{
		enc_20_4AB3B0 (key, 0x0B198ECA);
		enc_20_4D7080 (key, 0x7134D6C7);
		TRY(enc_20_major_4D1E70 (key, KEY(0x8)));
	}

	KEY(0x1C) ^= KEY(0x24) * 0x27219096;

	if(type == 0x2)
	{
		enc_20_4A1AE0 (key, 0x724A25E9);
		enc_20_4ABC40 (key, 0x88BE6100);
		TRY(enc_20_major_4D4FB0 (key, KEY(0x3C)));
		TRY(enc_20_4DA200 (key, 0x4F8F5B12));
		enc_20_4E7460 (key, 0x4592496C);
		TRY(enc_20_major_4EB610 (key, KEY(0x4C)));
	}

	var_18 += 0x51CE37 - 0x2790F4EC;
	KEY(0x08) = ROL(KEY(0x08), (unsigned char)(var_18 ^ seed));

	RETURN;
}


THROWS_RET enc_20_major_4C4AC0 (unsigned char *key, unsigned int seed)
{
	unsigned int type = KEY(0x20) & 0x0F;
	unsigned int var_18 = 0x3A6D8FF;

	if(type == 0x0A)
	{
		enc_20_4D7000 (key, 0x4E507E0);
		enc_20_4B0380 (key, 0x6304BEAE);
		TRY(enc_20_major_4E7CE0 (key, KEY(0x00)));
	}

	var_18 += 0x51CE37 - 0x20E20A59;
	seed |= seed + 0x20029BC7;

	if(type == 0x3)
	{
		enc_20_4A1B00 (key, 0x38BC4AA5);
		TRY(enc_20_4EEC50 (key, 0x21B5174));
		TRY(enc_20_major_4B24B0 (key, KEY(0x3C)));
	}

	if(type == 0x8)
	{
		TRY(enc_20_4DA200 (key, 0x37CD0534));
		enc_20_4F2AF0 (key, 0x47183404);
		TRY(enc_20_major_4E55C0 (key, KEY(0x8)));
	}

	KEY(0x20) |= (((KEY(0x24) * 27) << 1) - KEY(0x24)) << 1;

	if(type == 0x0)
	{
		enc_20_4BABE0 (key, 0x7F3CF484);
		enc_20_4CE300 (key, 0x9968EDD0);
		TRY(enc_20_major_4A16B0 (key, KEY(0x2C)));
	}

	if(type == 0x0E)
	{
		enc_20_4D56F0 (key, 0x0B5DCD82);
		TRY(enc_20_4DA4D0 (key, 0x86C6A2AB));
		TRY(enc_20_major_4E5960 (key, KEY(0x34)));
	}

	var_18 += 0x51CE37 - 0x10983F4F;
	KEY(0x28) &= KEY(0x18) - 0x1286A10;

	if(type == 0x0C)
	{
		enc_20_4C5040 (key, 0x53865B91);
		TRY(enc_20_4D7900 (key, 0x0B1EAF26C));
		TRY(enc_20_major_4DF3C0 (key, KEY(0x44)));
	}

	if(type == 0x2)
	{
		TRY(enc_20_4EEC50 (key, 0x0D8692E58));
		enc_20_4E7FB0 (key, 0x6DCE7FF0);
		TRY(enc_20_major_49ADF0 (key, KEY(0x34)));
	}

	KEY(0x38) = ROR(KEY(0x38), (unsigned char)(ROL(seed, 0x48)));

	if(type == 0x9)
	{
		enc_20_4B0380 (key, 0x41D6F740);
		enc_20_4C07D0 (key, 0x75F056D9);
		TRY(enc_20_major_4E7FD0 (key, KEY(0x14)));
	}

	var_18 += 0x51CE37 - 0x5CA8FE0;
	seed += 0x176CF052 - KEY(0x30);

	if(type == 0x0F)
	{
		enc_20_4D9AD0 (key, 0x2B2612F);
		enc_20_4C0780 (key, 0x48A7587B);
		TRY(enc_20_major_4E01F0 (key, seed));
	}

	if(type == 0x1)
	{
		enc_20_4D7680 (key, 0x4F288946);
		enc_20_4E7460 (key, 0x36B40520);
		TRY(enc_20_major_4EECA0 (key, KEY(0x44)));
	}

	KEY(0x20) = ROL(KEY(0x20), (unsigned char)(KEY(0x10) | 0x702AAAF));

	if(type == 0x5)
	{
		enc_20_4CE300 (key, 0x4A8BADCA);
		enc_20_4BABE0 (key, 0x77C07A82);
		TRY(enc_20_major_4DA520 (key, KEY(0x18)));
	}

	if(type == 0x4)
	{
		enc_20_4D4150 (key, 0x0F0C0009F);
		enc_20_4D5720 (key, 0x5FEA0895);
		TRY(enc_20_major_4B19A0 (key, seed));
	}

	var_18 += 0x51CE37 - 0xECDFFDA;
	KEY(0x34) *= KEY(0x08) * 101;

	if(type == 0x0)
	{
		enc_20_4AE170 (key, 0x0CB6C5D03);
		enc_20_4D56F0 (key, 0x8DAB903F);
		TRY(enc_20_major_4E7CE0 (key, KEY(0x28)));
	}

	if(type == 0x6)
	{
		enc_20_4D7000 (key, 0x44A4726A);
		enc_20_4F2AF0 (key, 0x4942B0D3);
		TRY(enc_20_major_4B24B0 (key, seed));
	}

	KEY(0x2C) |= ROR(KEY(0x44), 0xFD);

	if(type == 0x0D)
	{
		enc_20_49ADD0 (key, 0x3B05DFE2);
		enc_20_4ABC40 (key, 0x981510F3);
		TRY(enc_20_major_4E55C0 (key, KEY(0x48)));
	}

	var_18 += 0x51CE37 - 0x146D65D3;
	KEY(0x44) &= (seed * 3) << 4;

	if(type == 0x7)
	{
		enc_20_4D4100 (key, 0x1EF961E0);
		TRY(enc_20_4A1640 (key, 0x65EC261));
		TRY(enc_20_major_4A16B0 (key, KEY(0x00)));
	}

	if(type == 0x0B)
	{
		enc_20_4AE170 (key, 0x0C337552D);
		enc_20_4DA1E0 (key, 0x1563D4B7);
		TRY(enc_20_major_4E5960 (key, KEY(0x40)));
	}

	KEY(0x34) |= ((KEY(0x0C) << 5) - KEY(0x0C)) << 1;

	RETURN;
}


THROWS_RET enc_20_major_4CF650 (unsigned char *key, unsigned int seed)
{
	unsigned int type = (KEY(0x10) ^ KEY(0x30) ^ KEY(0x44)) & 0x0F;
	unsigned int var_18 = 0x7A66DF8;

	if(type == 0x9)
	{
		TRY(enc_20_4A1640 (key, 0x9FEBCD24));
		enc_20_4BABE0 (key, 0x7CB379C8);
		TRY(enc_20_major_4E01F0 (key, KEY(0x38)));
	}

	if(type == 0x4)
	{
		enc_20_4D5720 (key, 0x82BEBF6F);
		enc_20_49ADD0 (key, 0x459DEA1E);
		TRY(enc_20_major_4EECA0 (key, KEY(0x8)));
	}

	var_18 += 0x51CE37 - 0x4204423;
	KEY(0x24) += KEY(0x2C) < var_18 ? KEY(0x2C) : KEY(0x24);

	if(type == 0x6)
	{
		TRY(enc_20_4D76F0 (key, 0x0ECE6BFA0));
		enc_20_4AE170 (key, 0x0B30AC2F5);
		TRY(enc_20_major_4DA520 (key, seed));
	}

	if(type == 0x8)
	{
		TRY(enc_20_4DA4D0 (key, 0x0E88A81AF));
		enc_20_4D1E50 (key, 0x509C8E6);
		TRY(enc_20_major_4B19A0 (key, KEY(0x30)));
	}

	if(type == 0x2)
	{
		enc_20_4ABC20 (key, 0x0CE236AA1);
		enc_20_4AE170 (key, 0x0C293C411);
		TRY(enc_20_major_4C4AC0 (key, KEY(0x4)));
	}

	if(cos((double)((unsigned char)seed)) < math_const_2)
		KEY(0x28) *= 0x16B578EE;
	else
		KEY(0x28) *= KEY(0x08);

	if(type == 0x6)
	{
		enc_20_4E7CC0 (key, 0x0AC998661);
		enc_20_4D7000 (key, 0x4076396);
		TRY(enc_20_major_4F2B40 (key, KEY(0x28)));
	}

	if(type == 0x0D)
	{
		enc_20_4CE300 (key, 0x49B53039);
		enc_20_49CED0 (key, 0x0BBE07CA0);
		TRY(enc_20_major_4ABC60 (key, KEY(0x28)));
	}

	var_18 += 0x51CE37 - 0x1472F876;
	KEY(0x44) += seed * 77;

	if(type == 0x1)
	{
		enc_20_4C07D0 (key, 0x6509B7B6);
		enc_20_4E7460 (key, 0x526BFFE9);
		TRY(enc_20_major_4D4910 (key, seed));
	}

	if(type == 0x0E)
	{
		TRY(enc_20_4EEC50 (key, 0x0F4ECA545));
		enc_20_4E4F30 (key, 0x9A35E7);
		TRY(enc_20_major_4D1E70 (key, KEY(0x4C)));
	}

	if(type == 0x1)
	{
		enc_20_4C47B0 (key, 0x98420A17);
		enc_20_4D7740 (key, 0x9F2550BD);
		TRY(enc_20_major_4D4FB0 (key, seed));
	}

	seed = ROL(seed, (unsigned char)(KEY(0x1C) * 0xD46040D));

	if(type == 0x0C)
	{
		enc_20_4EB2A0 (key, 0x542B84AF);
		enc_20_49ADD0 (key, 0x469B4D07);
		TRY(enc_20_major_4EB610 (key, KEY(0x8)));
	}

	if(type == 0x3)
	{
		enc_20_4D6FB0 (key, 0x49F3B340);
		enc_20_4C07D0 (key, 0x4D1104E9);
		TRY(enc_20_major_4C1240 (key, KEY(0x10)));
		enc_20_4D76A0 (key, 0x6574F12);
		TRY(enc_20_4DA200 (key, 0x45C39F73));
		TRY(enc_20_major_4E01F0 (key, KEY(0x48)));
	}

	var_18 += 0x51CE37 - 0xC2920FC;

	if(sin((double)((unsigned char)KEY(0x00))) < math_const_2)
		KEY(0x10) += 0x1873296;
	else
		KEY(0x10) += KEY(0x04);

	if(type == 0x0A)
	{
		enc_20_4C07D0 (key, 0x458D9A1);
		TRY(enc_20_4E7C50 (key, 0x0F12C4D71));
		TRY(enc_20_major_4EECA0 (key, KEY(0x20)));
	}

	if(type == 0x4)
	{
		enc_20_4D76A0 (key, 0x9B5513C);
		enc_20_4A1B00 (key, 0x687C2613);
		TRY(enc_20_major_4DA520 (key, KEY(0x10)));
	}

	KEY(0x30) += 0x1C0BD6DB - KEY(0x2C);

	if(type == 0x7)
	{
		TRY(enc_20_4DA4D0 (key, 0x0C067CB50));
		enc_20_4C07D0 (key, 0x641A66D);
		TRY(enc_20_major_4B19A0 (key, KEY(0x38)));
	}

	if(type == 0x2)
	{
		enc_20_4AB3B0 (key, 0x11B24BD8);
		TRY(enc_20_4A1640 (key, 0x8951503F));
		TRY(enc_20_major_4C4AC0 (key, KEY(0x4C)));
	}

	if(type == 0x0B)
	{
		enc_20_4F2B20 (key, 0x8B252905);
		enc_20_4ADBC0 (key, 0x91BAB634);
		TRY(enc_20_major_4F2B40 (key, KEY(0x20)));
	}

	var_18 += 0x51CE37 - 0x64E3C4EE;
	KEY(0x48) += (0 - (((KEY(0x18) * 3) << 2) - KEY(0x18))) << 2;

	if(type == 0x7)
	{
		enc_20_4D1E50 (key, 0x0B41BD474);
		enc_20_4D7000 (key, 0x0D09F6E51);
		TRY(enc_20_major_4ABC60 (key, KEY(0x3C)));
	}

	if(type == 0x0)
	{
		enc_20_4AB3B0 (key, 0x1253B4B9);
		TRY(enc_20_4D56B0 (key, 0x2D54DA39));
		TRY(enc_20_major_4D4910 (key, seed));
	}

	if(type == 0x5)
	{
		enc_20_4D1E50 (key, 0x179C9602);
		enc_20_4CE300 (key, 0x7B414571);
		TRY(enc_20_major_4D1E70 (key, KEY(0x28)));
	}

	KEY(0x18) ^= KEY(0x40) ^ 0x354E354D;

	if(type == 0x0F)
	{
		enc_20_49B240 (key, 0x4FE0409F);
		enc_20_4D6FB0 (key, 0x0BBDFBB0E);
		TRY(enc_20_major_4D4FB0 (key, seed));
	}

	if(type == 0x8)
	{
		enc_20_4AE170 (key, 0x0C974ACB8);
		enc_20_4EB2C0 (key, 0x8A0E1AD7);
		TRY(enc_20_major_4EB610 (key, KEY(0x20)));
	}

	var_18 += 0x51CE37 - 0x43F58A7F;
	seed += var_18 ^ KEY(0x44);

	if(type == 0x0)
	{
		TRY(enc_20_4EEC50 (key, 0x0F8F4B821));
		enc_20_4F2AF0 (key, 0x63F74ECA);
		TRY(enc_20_major_4C1240 (key, KEY(0x20)));
	}

	if(type == 0x5)
	{
		enc_20_4D4170 (key, 0x46B67ABA);
		enc_20_4C0780 (key, 0x5CC0DB7A);
		TRY(enc_20_major_4E01F0 (key, KEY(0x28)));
	}

	KEY(0x0C) += KEY(0x34) + var_18;

	RETURN;	  
}


THROWS_RET enc_20_major_4D1E70 (unsigned char *key, unsigned int seed)
{
	unsigned int type = (KEY(0x18) ^ seed ^ KEY(0x38)) & 0x0F;
	unsigned int var_18 = 0x1E171745;

	if(type == 0x2)
	{
		enc_20_4C47B0 (key, 0x0F24FFCB1);
		enc_20_4D5740 (key, 0x2E753345);
		TRY(enc_20_major_4E5960 (key, KEY(0x00)));
	}

	if(type == 0x0)
	{
		enc_20_4F2AA0 (key, 0x4CB7C10);
		TRY(enc_20_4D7900 (key, 0x6B6BD558));
		TRY(enc_20_major_4DF3C0 (key, KEY(0x34)));
	}

	var_18 += 0x51CE37 - 0x1A8B0B2;
	KEY(0x38) &= (seed << 6) - seed;

	if(type == 0x0A)
	{
		enc_20_4E7CC0 (key, 0x7DB354E2);
		enc_20_4EB2A0 (key, 0x5D0DEC41);
		TRY(enc_20_major_49ADF0 (key, KEY(0x28)));
	}

	if(type == 0x0F)
	{
		enc_20_4F2A40 (key, 0x43FE3FC4);
		enc_20_4C4A60 (key, 0x6372112C);
		TRY(enc_20_major_4E7FD0 (key, KEY(0x2C)));
	}

	KEY(0x28) = ROR(KEY(0x28), (unsigned char)(KEY(0x38) * 83));

	if(type == 0x8)
	{
		enc_20_4E7460 (key, 0x39100F43);
		enc_20_4D9AD0 (key, 0x3B3040D);
		TRY(enc_20_major_4E01F0 (key, seed));
	}

	if(type == 0x4)
	{
		enc_20_49AB20 (key, 0x3ED2D98A);
		enc_20_4D5720 (key, 0x82E916DD);
		TRY(enc_20_major_4EECA0 (key, KEY(0x14)));
	}

	var_18 += 0x51CE37 - 0x141DA8E2;
	KEY(0x2C) ^= seed - 0x3C17609C;

	if(type == 0x1)
	{
		enc_20_4CE2E0 (key, 0x756125B2);
		enc_20_4D7000 (key, 0x0A7D686B);
		TRY(enc_20_major_4DA520 (key, seed));
	}

	if(type == 0x7)
	{
		enc_20_4AE3C0 (key, 0x0B7D63681);
		TRY(enc_20_4D56B0 (key, 0x401D08FD));
		TRY(enc_20_major_4B19A0 (key, KEY(0x2C)));
	}

	if(sin((double)((unsigned char)KEY(0x24))) < math_const_2)
		KEY(0x38) += 0x2D3F1771;
	else
		KEY(0x38) += KEY(0x2C);

	if(type == 0x5)
	{
		TRY(enc_20_4EEC50 (key, 0x0DAED7ED5));
		enc_20_4DA1E0 (key, 0x0E20B1F2);
		TRY(enc_20_major_4C4AC0 (key, KEY(0x24)));
	}

	if(type == 0x2)
	{
		enc_20_4D76A0 (key, 0x187F89F9);
		enc_20_4E4F30 (key, 0x87959F1F);
		TRY(enc_20_major_4F2B40 (key, KEY(0x34)));
	}

	if(type == 0x1)
	{
		enc_20_4EB2A0 (key, 0x643C9851);
		enc_20_4D1E50 (key, 0x0FCA5C817);
		TRY(enc_20_major_4ABC60 (key, seed));
	}

	var_18 += 0x51CE37 - 0x287AE3DA;
	KEY(0x48) &= KEY(0x44) + 0x21012257;

	if(type == 0x0E)
	{
		enc_20_4D4170 (key, 0x1DA0662D);
		TRY(enc_20_4BAC10 (key, 0x51F9A91A));
		TRY(enc_20_major_4D4910 (key, KEY(0x38)));
	}

	if(type == 0x0C)
	{
		enc_20_4AE170 (key, 0x3B002118);
		enc_20_4A1B00 (key, 0x3B304A6F);
		TRY(enc_20_major_4E5960 (key, KEY(0x30)));
	}

	KEY(0x4C) &= KEY(0x28) ^ 0x6FC516D5;

	if(type == 0x6)
	{
		TRY(enc_20_4DA120 (key, 0x49E40A75));
		enc_20_4CE2E0 (key, 0x91C7C273);
		TRY(enc_20_major_4DF3C0 (key, KEY(0x34)));
	}

	if(type == 0x0B)
	{
		TRY(enc_20_4DA200 (key, 0x1351FB6B));
		enc_20_4D6FB0 (key, 0x76237FFD);
		TRY(enc_20_major_49ADF0 (key, seed));
	}

	var_18 += 0x51CE37 - 0x5875981F;
	KEY(0x20) ^= KEY(0x2C) * 41 * 3;

	if(type == 0x4)
	{
		enc_20_49CF30 (key, 0x8A0D7D04);
		enc_20_4C07D0 (key, 0x74F06A1);
		TRY(enc_20_major_4E7FD0 (key, KEY(0x10)));
	}

	if(type == 0x3)
	{
		enc_20_4C47B0 (key, 0x0C55FC12);
		enc_20_4D70D0 (key, 0x51541025);
		TRY(enc_20_major_4E01F0 (key, KEY(0x24)));
	}

	KEY(0x00) += KEY(0x34) + var_18;

	if(type == 0x0)
	{
		TRY(enc_20_4BAC10 (key, 0x0F10F9D87));
		enc_20_4D9AD0 (key, 0x0AFABF1ED);
		TRY(enc_20_major_4EECA0 (key, KEY(0x3C)));
	}

	if(type == 0x9)
	{
		enc_20_4D5740 (key, 0x0B01F2752);
		enc_20_4A12B0 (key, 0x31784913);
		TRY(enc_20_major_4DA520 (key, KEY(0x48)));
	}

	var_18 += 0x51CE37 - 0x3809EB5D;
	seed *= KEY(0x20) - 0x44260E37;

	if(type == 0x3)
	{
		enc_20_4C07D0 (key, 0x3842537E);
		enc_20_4D7740 (key, 0x671C01B3);
		TRY(enc_20_major_4B19A0 (key, KEY(0x20)));
	}

	if(type == 0x0D)
	{
		enc_20_4DA1E0 (key, 0x4E19BD3);
		enc_20_4F2AA0 (key, 0x129D6C5E);
		TRY(enc_20_major_4C4AC0 (key, seed));
	}

	KEY(0x08) &= ROL(KEY(0x4C), (unsigned char)var_18);

	RETURN;
}


THROWS_RET enc_20_major_4D4910 (unsigned char *key, unsigned int seed)
{
	unsigned int type = (KEY(0x1C) ^ seed ^ KEY(0x48)) & 0x0F;
	unsigned int var_18 = 0x9E24650;

	if(type == 0x0F)
	{
		TRY(enc_20_4DA4D0 (key, 0x9BCA873C));
		enc_20_4D56F0 (key, 0x8B90B1BB);
		TRY(enc_20_major_4A16B0 (key, KEY(0x44)));
	}

	var_18 += 0x51CE37 - 0x28E92C0D;
	KEY(0x20) |= seed + 0xE43FC6B;

	if(type == 0x1)
	{
		enc_20_4EB2C0 (key, 0x979304F6);
		enc_20_4C07D0 (key, 0x156749AD);
		TRY(enc_20_major_4E5960 (key, KEY(0x18)));
	}

	if(type == 0x8)
	{
		enc_20_4E7FB0 (key, 0x3965913E);
		TRY(enc_20_4A1640 (key, 0x0F7131053));
		TRY(enc_20_major_4DF3C0 (key, KEY(0x34)));
	}

	KEY(0x4C) ^= seed * 75;

	if(type == 0x1)
	{
		enc_20_4AB3B0 (key, 0x7F675F82);
		enc_20_4C0780 (key, 0x2C9514D7);
		TRY(enc_20_major_49ADF0 (key, KEY(0x44)));
	}

	if(type == 0x0A)
	{
		enc_20_49B240 (key, 0x0ADD8566C);
		TRY(enc_20_4AE420 (key, 0x433A0094));
		TRY(enc_20_major_4E7FD0 (key, KEY(0x20)));
	}

	var_18 += 0x51CE37 - 0x5763A27F;
	KEY(0x04) ^= (KEY(0x38) * 11) << 1;

	if(type == 0x0)
	{
		enc_20_4AB3B0 (key, 0x0D644EBB);
		TRY(enc_20_4DA200 (key, 0x67513FF1));
		TRY(enc_20_major_4E01F0 (key, KEY(0x38)));
	}

	KEY(0x1C) |= seed ^ 0xE857063;

	if(type == 0x4)
	{
		enc_20_4EE700 (key, 0x0F3115E0);
		enc_20_4C4A60 (key, 0x0FFF1F0B9);
		TRY(enc_20_major_4EECA0 (key, KEY(0x18)));
	}

	if(type == 0x9)
	{
		TRY(enc_20_4D56B0 (key, 0x9C7B6BE9));
		enc_20_4BABE0 (key, 0x6E480136);
		TRY(enc_20_major_4DA520 (key, KEY(0x1C)));
	}

	var_18 += 0x51CE37 - 0x676F0B3;
	KEY(0x18) = ROR(KEY(0x18), (unsigned char)(KEY(0x24) * var_18));

	if(type == 0x3)
	{
		TRY(enc_20_49AA90 (key, 0x0CD88EA76));
		TRY(enc_20_4EEC50 (key, 0x0EEBE11BC));
		TRY(enc_20_major_4B19A0 (key, KEY(0x4C)));
	}

	if(type == 0x0D)
	{
		enc_20_4AE3C0 (key, 0x6E062619);
		TRY(enc_20_49B1F0 (key, 0x408B95D2));
		TRY(enc_20_major_4C4AC0 (key, KEY(0x4)));
	}

	KEY(0x18) -= KEY(0x44) < 0x417E2F7B ? KEY(0x44) : KEY(0x4C);

	if(type == 0x5)
	{
		enc_20_49B240 (key, 0x0EAE87E5C);
		enc_20_4E7CC0 (key, 0x886005C3);
		TRY(enc_20_major_4F2B40 (key, KEY(0x4)));
	}

	if(type == 0x0C)
	{
		enc_20_4D5740 (key, 0x26A13171);
		enc_20_4F2B20 (key, 0x0BF17C103);
		TRY(enc_20_major_4ABC60 (key, KEY(0x1C)));
	}

	var_18 += 0x51CE37 - 0x4496C1CE;
	KEY(0x18) |= my_ftol(floor(sqrt((double)(((unsigned char)seed))+1) + math_const_1));

	if(type == 0x2)
	{
		enc_20_4E4F80 (key, 0x7E5651A5);
		enc_20_4B0380 (key, 0x26142E52);
		TRY(enc_20_major_4A16B0 (key, seed));
	}

	KEY(0x08) ^= KEY(0x20) + 0x3E85747B;

	if(type == 0x0)
	{
		enc_20_4E7FB0 (key, 0x2715398E);
		enc_20_4EB2A0 (key, 0x7F696176);
		TRY(enc_20_major_4E5960 (key, KEY(0x38)));
	}

	if(type == 0x0B)
	{
		enc_20_4D4170 (key, 0x463E7360);
		enc_20_4EB2C0 (key, 0x0BF47F027);
		TRY(enc_20_major_4DF3C0 (key, KEY(0x0C)));
	}

	var_18 += 0x51CE37 - 0x345F167D;
	KEY(0x08) &= seed;

	if(type == 0x3)
	{
		enc_20_4AB3B0 (key, 0x69AFA76);
		enc_20_4EE700 (key, 0x0FB192798);
		TRY(enc_20_major_49ADF0 (key, KEY(0x10)));
	}

	if(type == 0x7)
	{
		enc_20_4D9AD0 (key, 0x39F63C40);
		TRY(enc_20_4D7900 (key, 0x0AA198808));
		TRY(enc_20_major_4E7FD0 (key, KEY(0x38)));
	}

	seed += KEY(0x24) - var_18;

	if(type == 0x0E)
	{
		TRY(enc_20_4EEC50 (key, 0x0E402CD4C));
		enc_20_4CE300 (key, 0x0F97D5746);
		TRY(enc_20_major_4E01F0 (key, seed));
	}

	var_18 += 0x51CE37 - 0x2393ADEE;
	KEY(0x48) += KEY(0x2C) * 91;

	if(type == 0x2)
	{
		enc_20_4B0380 (key, 0x4410A5B0);
		enc_20_4D70D0 (key, 0x84BFCA8F);
		TRY(enc_20_major_4EECA0 (key, KEY(0x4)));
	}

	if(type == 0x6)
	{
		enc_20_4E4F80 (key, 0x7B9755F6);
		enc_20_4EE700 (key, 0x8D62C7CD);
		TRY(enc_20_major_4DA520 (key, seed));
	}

	KEY(0x10) ^= KEY(0x10) - var_18;

	RETURN;
}


THROWS_RET enc_20_major_4D4FB0 (unsigned char *key, unsigned int seed)
{
	unsigned int type = (KEY(0x10) ^ seed ^ KEY(0x48)) & 0x0F;
	unsigned int var_18 = 0x47631948;

	if(type == 0x0C)
	{
		enc_20_4D7000 (key, 0x43DFEE1A);
		enc_20_49B240 (key, 0x0D010B07F);
		TRY(enc_20_major_4DF3C0 (key, KEY(0x2C)));
	}

	if(type == 0x4)
	{
		enc_20_4AE3C0 (key, 0x0A095474);
		enc_20_4D9AD0 (key, 0x0BED26946);
		TRY(enc_20_major_49ADF0 (key, seed));
	}

	var_18 += 0x51CE37 - 0x21819C6E;
	seed ^= (KEY(0x04) * 27) << 2;

	if(type == 0x1)
	{
		TRY(enc_20_4A1640 (key, 0x0AD86172C));
		enc_20_4D9AD0 (key, 0x0E5D9856);
		TRY(enc_20_major_4E7FD0 (key, KEY(0x1C)));
	}

	if(type == 0x9)
	{
		enc_20_4F2B20 (key, 0x0FCB25212);
		enc_20_4EB2A0 (key, 0x0C7D3C53A);
		TRY(enc_20_major_4E01F0 (key, KEY(0x28)));
	}

	KEY(0x2C) += KEY(0x20) - 0xEF3B680;

	if(type == 0x5)
	{
		enc_20_4E7FB0 (key, 0x2ABFF9EA);
		enc_20_4A12B0 (key, 0x13E5A61B);
		TRY(enc_20_major_4EECA0 (key, seed));
	}

	if(type == 0x2)
	{
		enc_20_4C4A60 (key, 0x26995743);
		enc_20_4C5040 (key, 0x57DEA73C);
		TRY(enc_20_major_4DA520 (key, KEY(0x0C)));
	}

	var_18 += 0x51CE37 - 0x3B6D1B57;
	KEY(0x4C) -= seed ^ 0x42B04005;

	if(type == 0x8)
	{
		TRY(enc_20_4E7C50 (key, 0x8CF062FD));
		enc_20_4DA1E0 (key, 0x0DC0FFA2);
		TRY(enc_20_major_4B19A0 (key, KEY(0x8)));
	}

	if(type == 0x3)
	{
		TRY(enc_20_4E7C50 (key, 0x0B7169A5C));
		enc_20_4D70D0 (key, 0x71BEF0);
		TRY(enc_20_major_4C4AC0 (key, seed));
	}

	KEY(0x00) += my_ftol(floor(sqrt((double)(((unsigned char)KEY(0x40)))+1) + math_const_1));

	if(type == 0x0B)
	{
		enc_20_4D9AD0 (key, 0x46885C39);
		enc_20_49B260 (key, 0x3D2FA7C7);
		TRY(enc_20_major_4F2B40 (key, KEY(0x4C)));
	}

	if(type == 0x0D)
	{
		TRY(enc_20_4D7900 (key, 0x2FC054CA));
		enc_20_4C07D0 (key, 0x7215ED91);
		TRY(enc_20_major_4ABC60 (key, KEY(0x1C)));
	}

	var_18 += 0x51CE37 - 0x921FDE;
	seed += KEY(0x44) | var_18;

	if(type == 0x1)
	{
		TRY(enc_20_4AE420 (key, 0x0D3280A0));
		enc_20_4F2AA0 (key, 0x3EB9D37);
		TRY(enc_20_major_4D4910 (key, KEY(0x30)));
	}

	if(type == 0x0E)
	{
		enc_20_4D6FB0 (key, 0x5675456D);
		enc_20_4E4F30 (key, 0x994A9D7F);
		TRY(enc_20_major_4D1E70 (key, seed));
	}

	KEY(0x08) = ROR(KEY(0x08), (unsigned char)(KEY(0x3C) < 0x3F2998C ? KEY(0x3C) : seed));

	if(type == 0x5)
	{
		TRY(enc_20_4A1640 (key, 0x2DD0E73));
		enc_20_4C07D0 (key, 0x56566DA);
		TRY(enc_20_major_4DF3C0 (key, seed));
	}

	if(type == 0x3)
	{
		TRY(enc_20_49AA90 (key, 0x21602B81));
		enc_20_4AB3B0 (key, 0x4CF85493);
		TRY(enc_20_major_49ADF0 (key, seed));
	}

	var_18 += 0x51CE37 - 0x1979BCBA;
	KEY(0x10) += KEY(0x08) ^ 0x1579499;

	if(type == 0x0A)
	{
		TRY(enc_20_4DA4D0 (key, 0x0C3082BA));
		enc_20_4C47D0 (key, 0x256FD55F);
		TRY(enc_20_major_4E7FD0 (key, KEY(0x14)));
	}

	if(type == 0x0)
	{
		TRY(enc_20_4DA120 (key, 0x0E755B1F1));
		enc_20_4F2B20 (key, 0x0EEA23D2F);
		TRY(enc_20_major_4E01F0 (key, KEY(0x20)));
	}
	
	seed += (0 - (((KEY(0x08) * 15) << 1) - KEY(0x08))) * 4;

	if(type == 0x0)
	{
		enc_20_4D5740 (key, 0x99A730A6);
		enc_20_4C47B0 (key, 0x0B6CA2013);
		TRY(enc_20_major_4EECA0 (key, seed));
	}

	if(type == 0x0F)
	{
		enc_20_4EB2A0 (key, 0x58549EB8);
		TRY(enc_20_4D76F0 (key, 0x0BCBC7BB));
		TRY(enc_20_major_4DA520 (key, seed));
	}

	var_18 += 0x51CE37 - 0x23629E3F;
	KEY(0x28) -= KEY(0x28) | var_18;

	if(type == 0x6)
	{
		TRY(enc_20_4D56B0 (key, 0x0FAFD833D));
		TRY(enc_20_4DA200 (key, 0x0CA0F21A3));
		TRY(enc_20_major_4B19A0 (key, KEY(0x2C)));
	}

	if(type == 0x4)
	{
		enc_20_4A12B0 (key, 0x259D354C);
		enc_20_4AB3B0 (key, 0x3F5EC9D);
		TRY(enc_20_major_4C4AC0 (key, KEY(0x4C)));
	}

	seed += KEY(0x44) ^ var_18;

	if(type == 0x7)
	{
		TRY(enc_20_4EEC50 (key, 0x1EB4212));
		enc_20_4D1E50 (key, 0x0ED2C157);
		TRY(enc_20_major_4F2B40 (key, KEY(0x20)));
	}

	if(type == 0x2)
	{
		TRY(enc_20_4D56B0 (key, 0x3CBDD354));
		enc_20_49B240 (key, 0x24792F0C);
		TRY(enc_20_major_4ABC60 (key, KEY(0x48)));
	}

	var_18 += 0x51CE37 - 0xB836399;
	KEY(0x40) -= KEY(0x2C) < 0x1E7D86EE ? KEY(0x2C) : seed;

	RETURN;
}


THROWS_RET enc_20_major_4DA520 (unsigned char *key, unsigned int seed)
{
	unsigned int type = (KEY(0x20) ^ seed ^ KEY(0x2C)) % 0x0E;
	unsigned int var_18 = 0x13E63894;

	if(type == 0x0)
	{
		TRY(enc_20_4AE420 (key, 0x0E32BDCA0));
		enc_20_4D7080 (key, 0x253BAEBA);
		TRY(enc_20_major_4AE190 (key, KEY(0x4C)));
	}

	var_18 += 0x51CE37 - 0x1CF3B5CC;
	seed -= seed ^ var_18;
	
	if(type == 0x1)
	{
		enc_20_4EB2C0 (key, 0x788C78A4);
		enc_20_4D5740 (key, 0x0EAAD3709);
		TRY(enc_20_major_4AE490 (key, seed));
	}

	if(cos((double)((unsigned char)KEY(0x0C))) < math_const_2)
		KEY(0x34) -= var_18;
	else
		KEY(0x34) -= KEY(0x10);

	if(type == 0x9)
	{
		enc_20_49B240 (key, 0x54F2C066);
		enc_20_4AE3C0 (key, 0x79D06B80);
		TRY(enc_20_major_4E7CE0 (key, seed));
	}

	var_18 += 0x51CE37 - 0x2E870A32;
	KEY(0x24) ^= ((KEY(0x18) * 45) << 1) - KEY(0x18);

	if(type == 0x7)
	{
		enc_20_4C07D0 (key, 0x0FCE88D4);
		enc_20_4D4170 (key, 0x7A7F43A);
		TRY(enc_20_major_4B24B0 (key, KEY(0x2C)));
	}

	if(type == 0x8)
	{
		enc_20_4D5740 (key, 0x69881ABE);
		enc_20_4A1AC0 (key, 0x292E3197);
		TRY(enc_20_major_4E55C0 (key, KEY(0x1C)));
	}

	if(sin((double)((unsigned char)seed)) < math_const_2)
		KEY(0x04) ^= var_18;
	else
		KEY(0x04) ^= KEY(0x44);

	if(type == 0x0D)
	{
		enc_20_4EB2A0 (key, 0x7EDBE9A7);
		enc_20_4EE700 (key, 0x9B35CDF7);
		TRY(enc_20_major_4A16B0 (key, KEY(0x10)));
	}

	var_18 += 0x51CE37 - 0x2E50FA12;
	KEY(0x44) += KEY(0x34) < 0xAC24EB8 ? KEY(0x34) : KEY(0x24);

	if(type == 0x5)
	{
		enc_20_4A1AC0 (key, 0x798A34E7);
		enc_20_4C4A60 (key, 0x56245E85);
		TRY(enc_20_major_4E5960 (key, KEY(0x4)));
	}

	seed |= ROR(KEY(0x48), 0xEB);

	if(type == 0x3)
	{
		enc_20_4E7FB0 (key, 0x773BD5D0);
		enc_20_4D4150 (key, 0x2D111A17);
		TRY(enc_20_major_4DF3C0 (key, KEY(0x00)));
	}

	var_18 += 0x51CE37 - 0x14D04E2A;
	KEY(0x10) += seed + 0xF65EFBD;

	if(type == 0x0A)
	{
		TRY(enc_20_4DA120 (key, 0x174A71C6));
		enc_20_49CF30 (key, 0x0A76E16F2);
		TRY(enc_20_major_49ADF0 (key, KEY(0x14)));
	}

	if(type == 0x0B)
	{
		enc_20_4BABE0 (key, 0x7E789968);
		TRY(enc_20_4DA200 (key, 0x0A2CBDC3F));
		TRY(enc_20_major_4E7FD0 (key, seed));
	}

	KEY(0x10) ^= ROL(KEY(0x20), (unsigned char)var_18);

	if(type == 0x6)
	{
		TRY(enc_20_4D76F0 (key, 0x0EC30BD82));
		enc_20_49CF30 (key, 0x0ACAA6688);
		TRY(enc_20_major_4E01F0 (key, KEY(0x34)));
	}

	var_18 += 0x51CE37 - 0x214227C5;
	seed *= KEY(0x18) + 0x6BBEB974;

	if(type == 0x2)
	{
		enc_20_4D1E50 (key, 0x0C61F202B);
		enc_20_49CF30 (key, 0x0AB67DFFA);
		TRY(enc_20_major_4EECA0 (key, KEY(0x18)));
	}

	KEY(0x40) -= KEY(0x08) * var_18;

	if(type == 0x0C)
	{
		enc_20_4C47B0 (key, 0x8272E2B8);
		enc_20_4AE3C0 (key, 0x7667AFED);
		TRY(enc_20_major_4AE190 (key, KEY(0x38)));
	}

	var_18 += 0x51CE37 - 0xAE30053;
	KEY(0x34) = ROR(KEY(0x34), (unsigned char)my_ftol(floor(sqrt((double)(((unsigned char)seed))+1) + math_const_1)));

	if(type == 0x4)
	{
		enc_20_4E7CC0 (key, 0x0DF62443F);
		enc_20_4D70D0 (key, 0x455D721C);
		TRY(enc_20_major_4AE490 (key, KEY(0x00)));
	}

	if(type == 0x0)
	{
		enc_20_4F2AA0 (key, 0x0C9D1F4A2);
		enc_20_4C07D0 (key, 0x16931016);
		TRY(enc_20_major_4E7CE0 (key, seed));
	}

	if(sin((double)((unsigned char)KEY(0x28))) < math_const_2)
		KEY(0x30) -= 0x2818AE3C;
	else
		KEY(0x30) -= seed;

	RETURN;
}


THROWS_RET enc_20_major_4DF3C0 (unsigned char *key, unsigned int seed)
{
	unsigned int type = (KEY(0x44) ^ seed ^ KEY(0x4C)) % 0x09;
	unsigned int var_14 = 0x2341AE11;

	var_14 += 0x51CE37 - 0x29FC10F7;
	KEY(0x4C) = ROR(KEY(0x4C), (unsigned char)(KEY(0x4C) + 0x222C438A));

	if(type == 0x4)
	{
		enc_20_4E7CC0 (key, 0x1611042);
		enc_20_4D5720 (key, 0x0CF99677E);
		enc_20_4EB310 (key, KEY(0x34));
	}
	
	KEY(0x14) ^= seed + 0x1FF8749D;

	if(type == 0x5)
	{
		enc_20_49B260 (key, 0x95649C47);
		enc_20_4C5040 (key, 0x1832D52);
		enc_20_4D7790 (key, KEY(0x48));
	}

	var_14 += 0x51CE37 - 0x4E1FF97A;
	KEY(0x34) ^= KEY(0x3C) + 0x19AD9D3;

	if(type == 0x0)
	{
		enc_20_4AE170 (key, 0x0D2B1CE9E);
		enc_20_4C07D0 (key, 0x715F7BB6);
		TRY(enc_20_major_4AE190 (key, KEY(0x34)));
	}

	KEY(0x0C) = ROR(KEY(0x0C), (unsigned char)my_ftol(floor(sqrt((double)(((unsigned char)KEY(0x24)))+1) + math_const_1)));

	if(type == 0x1)
	{
		enc_20_4A1B00 (key, 0x0B5EA8D3);
		enc_20_4E4F80 (key, 0x7B4A5B73);
		TRY(enc_20_major_4AE490 (key, KEY(0x30)));
	}

	var_14 += 0x51CE37 - 0x29D1BF9E;
	seed ^= KEY(0x30) ^ var_14;

	if(type == 0x0)
	{
		enc_20_4AE170 (key, 0x0BB927A66);
		enc_20_4BABE0 (key, 0x640D6589);
		TRY(enc_20_major_4E7CE0 (key, KEY(0x18)));
	}

	KEY(0x00) = ROL(KEY(0x00), (unsigned char)(var_14 & seed));

	if(type == 0x7)
	{
		enc_20_4E7FB0 (key, 0x595B6DE2);
		TRY(enc_20_49AA90 (key, 0x0D3D79CB4));
		TRY(enc_20_major_4B24B0 (key, KEY(0x18)));
	}

	var_14 += 0x51CE37 - 0x43D4EFBB;
	KEY(0x48) ^= KEY(0x24) - 0x5606038;

	if(type == 0x3)
	{
		enc_20_4D56F0 (key, 0x0C086F2C7);
		enc_20_4A1B00 (key, 0x5CDB6514);
		TRY(enc_20_major_4E55C0 (key, KEY(0x20)));
	}

	if(sin((double)((unsigned char)KEY(0x1C))) < math_const_2)
		KEY(0x24) |= var_14;
	else
		KEY(0x24) |= KEY(0x18);

	if(type == 0x2)
	{
		enc_20_4D1E50 (key, 0x27F82E25);
		TRY(enc_20_4DA200 (key, 0x0A3035B2F));
		TRY(enc_20_major_4A16B0 (key, KEY(0x4)));
	}

	var_14 += 0x51CE37 - 0x13C98081;
	seed -= my_ftol(floor(sqrt((double)(((unsigned char)seed))+1) + math_const_1));

	if(type == 0x6)
	{
		enc_20_4D4100 (key, 0x396122F5);
		enc_20_4F2AF0 (key, 0x7AB135BA);
		TRY(enc_20_major_4E5960 (key, KEY(0x00)));
	}

	RETURN;
}


THROWS_RET enc_20_major_4E01F0 (unsigned char *key, unsigned int seed)
{
	unsigned int type = (KEY(0x2C) ^ seed ^ KEY(0x14)) % 0x0C;
	unsigned int var_18 = 0x16BFB62C;

	if(type == 0x5)
	{
		enc_20_49CF30 (key, 0x0C7074230);
		enc_20_4F2AF0 (key, 0x6FB6D5B3);
		enc_20_4EB310 (key, seed);
	}

	var_18 += 0x51CE37 - 0xC1435B7;
	KEY(0x10) ^= seed - var_18;

	if(type == 0x2)
	{
		TRY(enc_20_4DA120 (key, 0x37DB88AD));
		enc_20_4DA1E0 (key, 0x15D9C7E8);
		enc_20_4D7790 (key, seed);
	}

	KEY(0x3C) -= var_18 ^ seed;

	if(type == 0x0)
	{
		TRY(enc_20_4BAC10 (key, 0x80E3E69E));
		enc_20_4AE3C0 (key, 0x8BD64F99);
		TRY(enc_20_major_4AE190 (key, KEY(0x10)));
	}

	var_18 += 0x51CE37 - 0x3E87DD87;
	KEY(0x20) ^= my_ftol(floor(sqrt((double)(((unsigned char)KEY(0x40)))+1) + math_const_1));

	if(type == 0x3)
	{
		enc_20_4D7660 (key, 0x3BA8DA0B);
		enc_20_4D5740 (key, 0x8AF43903);
		TRY(enc_20_major_4AE490 (key, seed));
	}

	if(type == 0x0A)
	{
		enc_20_4A1AE0 (key, 0x6B7B7C8C);
		enc_20_4C0780 (key, 0x0EC5DB78C);
		TRY(enc_20_major_4E7CE0 (key, KEY(0x18)));
	}

	seed -= seed & 0x179DA692;

	if(type == 0x4)
	{
		TRY(enc_20_4EEC50 (key, 0x13465A59));
		TRY(enc_20_4DA120 (key, 0x37B9DF6E));
		TRY(enc_20_major_4B24B0 (key, KEY(0x00)));
	}

	var_18 += 0x51CE37 - 0xE35570;
	KEY(0x20) ^= ((KEY(0x3C) * 3) << 5) - KEY(0x3C);

	if(type == 0x0)
	{
		enc_20_4EB2C0 (key, 0x6191EFEC);
		enc_20_4D56F0 (key, 0x0CB94A6AA);
		TRY(enc_20_major_4E55C0 (key, KEY(0x24)));
	}

	if(sin((double)((unsigned char)seed)) < math_const_2)
		KEY(0x18) &= var_18;
	else
		KEY(0x18) &= KEY(0x38);

	if(type == 0x9)
	{
		enc_20_4D4170 (key, 0x4DD960F0);
		enc_20_4DA1E0 (key, 0x1F79699C);
		TRY(enc_20_major_4A16B0 (key, KEY(0x00)));
	}

	if(type == 0x6)
	{
		enc_20_49AAE0 (key, 0x94AA2F9);
		enc_20_4D7660 (key, 0x3E9D8E2F);
		TRY(enc_20_major_4E5960 (key, seed));
	}

	var_18 += 0x51CE37 - 0x390A1A2;

	if(cos((double)((unsigned char)KEY(0x4C))) < math_const_2)
		seed -= 0xC818C81;
	else
		seed -= KEY(0x4C);

	if(type == 0x7)
	{
		enc_20_4C4A60 (key, 0x65CCD7B0);
		enc_20_4EE700 (key, 0x90FA8FD1);
		TRY(enc_20_major_4DF3C0 (key, KEY(0x38)));
	}

	KEY(0x28) += KEY(0x04) + 0x217F7A00;

	if(type == 0x1)
	{
		enc_20_4DA1E0 (key, 0x0F4FFD358);
		enc_20_4D4100 (key, 0x6C60312C);
		TRY(enc_20_major_49ADF0 (key, KEY(0x44)));
	}

	var_18 += 0x51CE37 - 0x3B888541;
	KEY(0x14) &= ROR(KEY(0x00), (unsigned char)var_18);

	if(type == 0x8)
	{
		enc_20_49CF30 (key, 0x67B2223D);
		enc_20_4CE300 (key, 0x39289995);
		TRY(enc_20_major_4E7FD0 (key, KEY(0x34)));
	}

	if(type == 0x0B)
	{
		enc_20_4D7080 (key, 0x5C9A9ABA);
		enc_20_4D56F0 (key, 0x0F05B039);
		enc_20_4EB310 (key, KEY(0x00));
	}

	KEY(0x30) |= ROL(KEY(0x1C), (unsigned char)var_18);
	
	RETURN;
}


THROWS_RET enc_20_major_4E55C0 (unsigned char *key, unsigned int seed)
{
	unsigned int type = (KEY(0x20) ^ KEY(0x1C) ^ KEY(0x30)) % 0x06;
	unsigned int var_14 = 0x8622A6D;

	var_14 += 0x51CE37 - 0x503595F9;
	KEY(0x04) |= KEY(0x10) ^ 0x10104D4;

	if(type == 0x3)
	{
		enc_20_4E7FB0 (key, 0x0FC354AE9);
		enc_20_4AE3C0 (key, 0x0A9EB159D);
		enc_20_4EB310 (key, KEY(0x40));
	}

	var_14 += 0x51CE37 - 0x3CB4EBF6;
	seed = ((seed ^ 0x1EA9DA8) + seed) * KEY(0x48) * 13;

	if(type == 0x0)
	{
		TRY(enc_20_4AE420 (key, 0x10381FF0));
		enc_20_49CF30 (key, 0x83A6C91A);
		enc_20_4D7790 (key, KEY(0x14));
	}
	
	KEY(0x38) += KEY(0x30) * 25;
	var_14 += 0x51CE37 - 0x14CF48EC;
	KEY(0x08) -= my_ftol(floor(sqrt((double)(((unsigned char)KEY(0x14)))+1) + math_const_1));

	if(type == 0x4)
	{
		enc_20_4ABC40 (key, 0x39E08612);
		enc_20_4CE300 (key, 0x3A648AAD);
		TRY(enc_20_major_4AE190 (key, seed));
	}

	KEY(0x18) &= KEY(0x10) - var_14;
	var_14 += 0x51CE37 - 0x2A19B6A4;
	KEY(0x04) ^= KEY(0x40) + 0x988DB31;

	if(type == 0x0)
	{
		enc_20_4F2AA0 (key, 0x0A98896DD);
		TRY(enc_20_4D56B0 (key, 0x55AF8F05));
		TRY(enc_20_major_4AE490 (key, KEY(0x18)));
	}

	KEY(0x18) += ROR(seed, (unsigned char)var_14);
	var_14 += 0x51CE37 - 0x143D834B;
	seed -= KEY(0x00) < var_14 ? KEY(0x00) : KEY(0x0C);

	if(type == 0x2)
	{
		enc_20_49CED0 (key, 0x0F1517841);
		enc_20_4D9AD0 (key, 0x60D84C1E);
		TRY(enc_20_major_4E7CE0 (key, seed));
	}

	seed *= my_ftol(floor(sqrt((double)(((unsigned char)seed))+1) + math_const_1));
	var_14 += 0x51CE37 - 0x9210725;

	if(cos((double)((unsigned char)seed)) < math_const_2)
		KEY(0x14) *= var_14;
	else
		KEY(0x14) *= KEY(0x4C);

	if(type == 0x5)
	{
		enc_20_4D6FB0 (key, 0x4CBA8E5C);
		enc_20_4C0780 (key, 0x0A0982F6E);
		TRY(enc_20_major_4B24B0 (key, KEY(0x34)));
	}

	RETURN;
}


THROWS_RET enc_20_major_4E5960 (unsigned char *key, unsigned int seed)
{
	unsigned int type = (KEY(0x38) ^ KEY(0x2C) ^ KEY(0x44)) & 0x07;
	unsigned int var_14 = 0x37690C43;

	var_14 += 0x51CE37 - 0x252EAF05;
	KEY(0x2C) ^= ROR(KEY(0x34), (unsigned char)var_14);

	if(type == 0x5)
	{
		enc_20_4A1AE0 (key, 0x4A451DB4);
		enc_20_4D7660 (key, 0x3A03C13B);
		enc_20_4EB310 (key, KEY(0x28));
	}

	KEY(0x0C) = ROR(KEY(0x0C), (unsigned char)(KEY(0x40) * 37 * 3));

	if(type == 0x2)
	{
		enc_20_4C47D0 (key, 0x4800158B);
		enc_20_4F2A40 (key, 0x255DC48D);
		enc_20_4D7790 (key, KEY(0x44));
	}

	var_14 += 0x51CE37 - 0x38937140;
	KEY(0x2C) -= my_ftol(floor(sqrt((double)(((unsigned char)KEY(0x24)))+1) + math_const_1));
	KEY(0x30) += 0x17267C5B - KEY(0x2C);

	if(type == 0x3)
	{
		enc_20_4E4F30 (key, 0x0CA8D3E2B);
		enc_20_4B0380 (key, 0x68E48D67);
		TRY(enc_20_major_4AE190 (key, KEY(0x00)));
	}

	var_14 += 0x51CE37 - 0x390FD83B;
	KEY(0x44) ^= seed ^ 0x35EDDEA4; 

	if(type == 0x0)
	{
		enc_20_4D7740 (key, 0x3409139C);
		enc_20_4E7460 (key, 0x21F88861);
		TRY(enc_20_major_4AE490 (key, KEY(0x18)));
	}

	KEY(0x18) *= KEY(0x44) + 0xB89B51C;

	if(type == 0x1)
	{
		enc_20_4D4170 (key, 0x4ADDC50D);
		TRY(enc_20_4BAC10 (key, 0x90254266));
		TRY(enc_20_major_4E7CE0 (key, KEY(0x18)));
	}

	var_14 += 0x51CE37 - 0x4A885C91;
	KEY(0x4C) ^= KEY(0x0C) < var_14 ? KEY(0x0C) : KEY(0x04);
	KEY(0x3C) ^= ((KEY(0x30) * 3) << 3) - KEY(0x30);

	if(type == 0x7)
	{
		enc_20_4D7680 (key, 0x4BDE6422);
		enc_20_4D7680 (key, 0x0DE379800);
		TRY(enc_20_major_4B24B0 (key, KEY(0x24)));
	}

	var_14 += 0x51CE37 - 0x47CA3B11;
	KEY(0x28) += 0x395F1D29 - seed;

	if(type == 0x0)
	{
		enc_20_4C4A60 (key, 0x29CFE0BE);
		enc_20_4AB3B0 (key, 0x42E1F1A9);
		TRY(enc_20_major_4E55C0 (key, KEY(0x40)));
	}

	KEY(0x04) = ROL(KEY(0x04), (unsigned char)ROL(KEY(0x20),(unsigned char)var_14));
	var_14 += 0x51CE37 - 0x466E93D6;
	seed -= KEY(0x24) ^ var_14;

	if(type == 0x6)
	{
		TRY(enc_20_4BAC10 (key, 0x10B4EAEF));
		enc_20_4C4A60 (key, 0x298D7844);
		TRY(enc_20_major_4A16B0 (key, seed));
	}

	KEY(0x48) = ROL(KEY(0x48), (unsigned char)(KEY(0x1C) & 0x34490731));

	RETURN;
}


THROWS_RET enc_20_major_4E7CE0 (unsigned char *key, unsigned int seed)
{
	unsigned int type = (KEY(0x48) ^ KEY(0x18) ^ KEY(0x3C)) & 0x03;
	unsigned int var_14 = 0x62369;

	var_14 += 0x51CE37 - 0x1C2B7803;
	{
		unsigned int var = KEY(0x3C) * seed * 0x3C02927;
		seed = ROR(var, (unsigned char)(((var * 9) << 3) - var));
	}

	if(type == 0x0)
	{
		enc_20_4CE2E0 (key, 0x45000889);
		enc_20_4D7660 (key, 0x58053BA7);
		enc_20_4EB310 (key, KEY(0x4));
	}

	var_14 += 0x51CE37 - 0x2B57AC24;
	seed ^= KEY(0x18) ^ 0xC1FCDA0;

	if(cos((double)((unsigned char)KEY(0x18))) < math_const_2)
		KEY(0x14) -= var_14;
	else
		KEY(0x14) -= KEY(0x28);

	if(type == 0x0)
	{
		enc_20_4D7660 (key, 0x4370ACC2);
		enc_20_4ABC40 (key, 0x56BB8205);
		enc_20_4D7790 (key, KEY(0x38));
	}

	var_14 += 0x51CE37 - 0x4EADE9E0;
	seed *= KEY(0x4C) + 0x11500E47;	
	KEY(0x0C) ^= ROL(KEY(0x10), 0x34);

	if(type == 0x3)
	{
		enc_20_4E4F80 (key, 0x29F1EEB);
		enc_20_4C47B0 (key, 0x5285FE67);
		TRY(enc_20_major_4AE190 (key, KEY(0x3C)));
	}

	var_14 += 0x51CE37 - 0x399B1DAD;
	KEY(0x34) -= my_ftol(floor(sqrt((double)(((unsigned char)seed))+1) + math_const_1));

	if(cos((double)((unsigned char)seed)) < math_const_2)
		seed = ROR(seed, (unsigned char)0x3BAE8C7);
	else
		seed = ROR(seed, (unsigned char)KEY(0x28));

	var_14 += 0x51CE37 - 0x1453067E;
	KEY(0x40) = (KEY(0x3C) * KEY(0x40) * 37) << 1;

	if(type == 0x1)
	{
		enc_20_4F2AA0 (key, 0x0B3BB63F);
		enc_20_4ADBC0 (key, 0x0BD7F0241);
		TRY(enc_20_major_4AE490 (key, seed));
	}

	RETURN;
}


THROWS_RET enc_20_major_4E7FD0 (unsigned char *key, unsigned int seed)
{
	unsigned int type = (KEY(0x04) ^ KEY(0x00) ^ KEY(0x40)) % 0x0B;
	unsigned int var_14 = 0x966CB19;

	if(type == 0x2)
	{
		enc_20_49CF30 (key, 0x2FA01766);
		enc_20_4D9AD0 (key, 0x0B05CA012);
		enc_20_4EB310 (key, KEY(0x18));
	}

	var_14 += 0x51CE37 - 0x3E9B48D4;
	KEY(0x14) -= seed;

	if(type == 0x8)
	{
		enc_20_4A1B00 (key, 0x1DD94C2C);
		enc_20_4D4170 (key, 0x55CE8D08);
		enc_20_4D7790 (key, KEY(0x4C));
	}

	KEY(0x44) ^=  ROL(KEY(0x48), (unsigned char)var_14);

	if(type == 0x4)
	{
		enc_20_49CF30 (key, 0x0EAD5B044);
		enc_20_4EB2C0 (key, 0x79FB5201);
		TRY(enc_20_major_4AE190 (key, KEY(0x1C)));
	}

	var_14 += 0x51CE37 - 0x419B6D78;
	KEY(0x00) ^= my_ftol(floor(sqrt((double)(((unsigned char)KEY(0x30)))+1) + math_const_1));

	if(type == 0x0)
	{
		enc_20_4C47B0 (key, 0x3C3D1DB0);
		TRY(enc_20_4DA120 (key, 0x1798B6E0));
		TRY(enc_20_major_4AE490 (key, KEY(0x8)));
	}

	KEY(0x28) ^= (seed * 27) << 2;

	if(type == 0x9)
	{
		enc_20_4A12B0 (key, 0x1F50271D);
		enc_20_4EB2C0 (key, 0x6DDF8C10);
		TRY(enc_20_major_4E7CE0 (key, KEY(0x28)));
	}

	var_14 += 0x51CE37 - 0x2CBEEE4;

	if(cos((double)((unsigned char)KEY(0x30))) < math_const_2)
		KEY(0x20) -= var_14;
	else
		KEY(0x20) -= seed;

	if(type == 0x7)
	{
		enc_20_4E7CC0 (key, 0x0F2EC0B09);
		enc_20_4D7000 (key, 0x68DA21A);
		TRY(enc_20_major_4B24B0 (key, KEY(0x4)));
	}

	if(sin((double)((unsigned char)seed)) < math_const_2)
		seed ^= 0x2C99FADE;
	else
		seed ^= KEY(0x38);

	if(type == 0x1)
	{
		TRY(enc_20_4A1640 (key, 0x3FCF3163));
		enc_20_4CE300 (key, 0x0B98E1E36);
		TRY(enc_20_major_4E55C0 (key, seed));
	}

	var_14 += 0x51CE37 - 0x6D17119;

	if(cos((double)((unsigned char)KEY(0x2C))) < math_const_2)
		KEY(0x3C) += 0x1BEC01F;
	else
		KEY(0x3C) += seed;

	if(type == 0x5)
	{
		enc_20_4AB3B0 (key, 0x15D31F7C);
		enc_20_4A1AE0 (key, 0x61034C9F);
		TRY(enc_20_major_4A16B0 (key, KEY(0x3C)));
	}

	KEY(0x04) = ROR(KEY(0x04), (unsigned char)(var_14 * KEY(0x40)));

	if(type == 0x0)
	{
		TRY(enc_20_4D76F0 (key, 0x0FDE30E03));
		enc_20_49B240 (key, 0x58581DDE);
		TRY(enc_20_major_4E5960 (key, seed));
	}

	var_14 += 0x51CE37 - 0x25D9C0CA;
	KEY(0x1C) &= KEY(0x3C) * 0xA8F285;

	if(type == 0x0A)
	{
		enc_20_4F2AA0 (key, 0x0EF011757);
		enc_20_4AE3C0 (key, 0x2412315A);
		TRY(enc_20_major_4DF3C0 (key, KEY(0x34)));
	}

	if(type == 0x3)
	{
		enc_20_4ABC20 (key, 0x8E08D3A3);
		enc_20_4A1AE0 (key, 0x1B9DF7E0);
		TRY(enc_20_major_49ADF0 (key, KEY(0x28)));
	}

	if(sin((double)((unsigned char)KEY(0x20))) < math_const_2)
		KEY(0x0C) *= var_14;
	else
		KEY(0x0C) *= KEY(0x08);

	if(type == 0x6)
	{
		enc_20_4C5040 (key, 0x4120170D);
		enc_20_4F2AF0 (key, 0x476980E4);
		enc_20_4EB310 (key, KEY(0x4C));
	}

	var_14 += 0x51CE37 - 0x39DB4818;
	KEY(0x2C) ^= ((KEY(0x44) << 4) + KEY(0x44)) << 2;

	RETURN;
}


THROWS_RET enc_20_major_4EB610 (unsigned char *key, unsigned int seed)
{
	unsigned int type = (KEY(0x14) ^ KEY(0x00) ^ seed) & 0x0F;
	unsigned int var_18 = 0x7C36F793;

	if(type == 0x3)
	{
		TRY(enc_20_4DA200 (key, 0x2A8F3B7B));
		enc_20_4E4F30 (key, 0x4BB6BBB3);
		TRY(enc_20_major_49ADF0 (key, seed));
	}

	if(type == 0x0)
	{
		enc_20_4D5720 (key, 0x3B3E4742);
		enc_20_4D5740 (key, 0x27A033A6);
		TRY(enc_20_major_4E7FD0 (key, KEY(0x8)));
	}

	var_18 += 0x51CE37 - 0x513E5C0;
	KEY(0x38) ^= ROL(KEY(0x40), 0x76);

	if(type == 0x0C)
	{
		enc_20_4D7740 (key, 0x830BA927);
		enc_20_4EB2C0 (key, 0x6F3A3876);
		TRY(enc_20_major_4E01F0 (key, KEY(0x20)));
	}

	if(type == 0x1)
	{
		enc_20_4F2A40 (key, 0x5B53432E);
		enc_20_4A1B00 (key, 0x0E9B79A5);
		TRY(enc_20_major_4EECA0 (key, KEY(0x18)));
	}

	KEY(0x30) ^= KEY(0x2C) < 0x521B2180 ? KEY(0x2C) : KEY(0x24);

	if(type == 0x1)
	{
		enc_20_4E4F30 (key, 0x42261FF2);
		enc_20_4F2B20 (key, 0x8E337807);
		TRY(enc_20_major_4DA520 (key, KEY(0x3C)));
	}

	if(type == 0x8)
	{
		TRY(enc_20_4DA120 (key, 0x0EB358CDB));
		enc_20_4D1E50 (key, 0x0B444924);
		TRY(enc_20_major_4B19A0 (key, KEY(0x34)));
	}

	if(type == 0x4)
	{
		enc_20_4C5040 (key, 0x4682FB69);
		enc_20_49CF30 (key, 0x67CCBC61);
		TRY(enc_20_major_4C4AC0 (key, KEY(0x40)));
	}

	var_18 += 0x51CE37 - 0x10AD1B5;
	KEY(0x48) &= my_ftol(floor(sqrt((double)(((unsigned char)KEY(0x24)))+1) + math_const_1));

	if(type == 0x5)
	{
		TRY(enc_20_4D7900 (key, 0x893D3AC1));
		enc_20_4E4F80 (key, 0x137579DF);
		TRY(enc_20_major_4F2B40 (key, KEY(0x8)));
	}

	if(type == 0x6)
	{
		enc_20_49CF30 (key, 0x0A7A5C640);
		enc_20_4D4170 (key, 0x51927A5A);
		TRY(enc_20_major_4ABC60 (key, KEY(0x38)));
	}

	KEY(0x48) += 0 - (((KEY(0x40) * 15) << 3) - KEY(0x40));

	if(type == 0x9)
	{
		enc_20_4A1AC0 (key, 0x12C33EF4);
		enc_20_49B260 (key, 0x827C0747);
		TRY(enc_20_major_4D4910 (key, KEY(0x28)));
	}

	if(type == 0x0A)
	{
		enc_20_4D9AD0 (key, 0x92FD0C8C);
		enc_20_4C0780 (key, 0x6CD0251E);
		TRY(enc_20_major_4D1E70 (key, KEY(0x00)));
	}

	if(type == 0x6)
	{
		enc_20_49CF30 (key, 0x0E0EF8B15);
		enc_20_49B260 (key, 0x9CBCA826);
		TRY(enc_20_major_4D4FB0 (key, seed));
	}

	var_18 += 0x51CE37 - 0x2DDCCCC0;
	KEY(0x34) ^= var_18 ^ seed;

	if(type == 0x2)
	{
		enc_20_4D7740 (key, 0x6467451);
		enc_20_4ADBC0 (key, 0x0B73CCFC5);
		TRY(enc_20_major_49ADF0 (key, KEY(0x8)));
	}

	if(type == 0x7)
	{
		enc_20_4ADBC0 (key, 0x0F636FBE1);
		enc_20_4DA1E0 (key, 0x155B7182);
		TRY(enc_20_major_4E7FD0 (key, KEY(0x00)));
	}

	KEY(0x18) -= my_ftol(floor(sqrt((double)(((unsigned char)KEY(0x28)))+1) + math_const_1));

	if(type == 0x3)
	{
		TRY(enc_20_49AA90 (key, 0x5B9D1F9));
		enc_20_4D7740 (key, 0x1119D06E);
		TRY(enc_20_major_4E01F0 (key, KEY(0x20)));
	}

	if(type == 0x4)
	{
		enc_20_4D5740 (key, 0x5E4B50C9);
		enc_20_4AB3B0 (key, 0x6C475699);
		TRY(enc_20_major_4EECA0 (key, seed));
	}

	if(type == 0x0D)
	{
		enc_20_4ABC20 (key, 0x0F8366C3C);
		enc_20_4D7680 (key, 0x1DCFE0A0);
		TRY(enc_20_major_4DA520 (key, KEY(0x8)));
	}

	var_18 += 0x51CE37 - 0xE3C05FB;
	seed -= ROR(KEY(0x20), 0x91);

	if(type == 0x0F)
	{
		enc_20_4D7050 (key, 0x3DF6E9B8);
		enc_20_4F2B20 (key, 0x26143568);
		TRY(enc_20_major_4B19A0 (key, KEY(0x10)));
	}

	if(type == 0x2)
	{
		enc_20_4E7460 (key, 0x51443CCD);
		enc_20_4E7FB0 (key, 0x7620E46);
		TRY(enc_20_major_4C4AC0 (key, KEY(0x40)));
	}

	KEY(0x38) = ROR(KEY(0x38), (unsigned char)(seed - var_18));

	if(type == 0x0B)
	{
		enc_20_4E7460 (key, 0x1D2252F);
		enc_20_4C5040 (key, 0x46BFF386);
		TRY(enc_20_major_4F2B40 (key, KEY(0x24)));
	}

	if(type == 0x5)
	{
		enc_20_4D7050 (key, 0x2002C1F2);
		TRY(enc_20_4D7900 (key, 0x37C6428B));
		TRY(enc_20_major_4ABC60 (key, KEY(0x18)));
	}

	if(type == 0x0)
	{
		enc_20_4E7FB0 (key, 0x0F9B35E7B);
		enc_20_4D5720 (key, 0x849F0F1F);
		TRY(enc_20_major_4D4910 (key, KEY(0x38)));
	}

	var_18 += 0x51CE37 - 0x31DBA030;
	KEY(0x20) ^= ROR(seed, (unsigned char)var_18);

	if(type == 0x0E)
	{
		enc_20_4C4A60 (key, 0x5159E8CF);
		enc_20_49AB20 (key, 0x3DC64268);
		TRY(enc_20_major_4D1E70 (key, seed));
	}

	if(sin((double)((unsigned char)KEY(0x00))) < math_const_2)
		KEY(0x00) += var_18;
	else
		KEY(0x00) += KEY(0x38);

	RETURN;
}


THROWS_RET enc_20_major_4AE190 (unsigned char *key, unsigned int seed)
{
	unsigned int type = seed & 0x01;
	unsigned int var_14 = 0x7F15D67;

	var_14 += 0x51CE37 - 0x533AFC9;
	seed |= KEY(0x44) - 0x1E97AEB;

	KEY(0x24) ^= KEY(0x08) * 11;

	var_14 += 0x51CE37 - 0xFAF6959;
	KEY(0x10) += KEY(0x20) - 0x16F911E4;

	if(KEY(0x38) < var_14)
		seed |= KEY(0x38);
	else
		seed |= KEY(0x14);

	var_14 += 0x51CE37 - 0x33F6A23B;

	if(KEY(0x1C) < 0x402226F)
		KEY(0x28) ^= KEY(0x1C);
	else
		KEY(0x28) ^= KEY(0x08);

	if(type == 0x00)
	{
		enc_20_4B0380 (key, 0x731311BB);
		enc_20_4EB2C0 (key, 0x640F077D);
		enc_20_4EB310 (key, KEY(0x04));
	}

	KEY(0x04) += ((((0 - KEY(0x4C)) << 2) - KEY(0x4C)) * 5) << 2;
	var_14 += 0x51CE37 - 0x5F978BAB;
	
	KEY(0x44) += ROL(KEY(0x30), (unsigned char)var_14);
	KEY(0x2C) ^=  ROL(KEY(0x08), 0xA9);

	var_14 += 0x51CE37 - 0x1C3B041A;

	KEY(0x04) += seed - 0x18D1B90;
	KEY(0x1C) -= KEY(0x0C) ^ var_14;

	if(type == 0)
	{
		TRY(enc_20_49AA90 (key, 0xDC306F47));
		enc_20_4CE300 (key, 0x9194790);
		enc_20_4D7790 (key, KEY(0x14));
	}

	var_14 += 0x51CE37 - 0x2F28976F;
	KEY(0x1C) = ROR(KEY(0x1C), (unsigned char)(KEY(0x34) + var_14));

	RETURN;
}


THROWS_RET enc_20_major_4AE490 (unsigned char *key, unsigned int seed)
{
	unsigned int type = (KEY(0x08) ^ seed ^ KEY(0x1C)) % 0x03;
	unsigned int var_14 = 0x47296061;

	var_14 += 0x51CE37 - 0x40BC9F04;

	if(cos((double)((unsigned char)seed)) < math_const_2)
		seed *= var_14;
	else
		seed *= seed;

	KEY(0x08) ^= KEY(0x08) + 0x3786364B;
	var_14 += 0x51CE37 - 0x7DC308F;
	KEY(0x44) = ROL(KEY(0x44), (unsigned char)(seed - 0x2A2F6EB0));

	if(type == 0)
	{
		enc_20_49CED0 (key, 0x52332BED);
		enc_20_4D4100 (key, 0x2515D847);
		enc_20_4EB310 (key, KEY(0x1C));
	}

	seed += KEY(0x0C) ^ var_14;
	var_14 += 0x51CE37 - 0x5E8B2C2;

	if(sin((double)((unsigned char)KEY(0x2C))) < math_const_2)
		seed = ROL(seed, (unsigned char)var_14);
	else
		seed = ROL(seed, (unsigned char)KEY(0x00));


	if(sin((double)((unsigned char)KEY(0x40))) < math_const_2)
		KEY(0x14) += 0x3AF2A8E2;
	else
		KEY(0x14) += KEY(0x40);
    
	if(type == 0x00)
	{
		TRY(enc_20_4DA120 (key, 0xE33083C7));
		enc_20_49CF30 (key, 0x65D7A9B4);
		enc_20_4D7790 (key, KEY(0x2C));
	}

	var_14 += 0x51CE37 - 0x359F93E;

	if(cos((double)((unsigned char)KEY(0x40))) < math_const_2)
		KEY(0x34) ^= var_14;
	else
		KEY(0x34) ^= KEY(0x04);

	seed += my_ftol(floor(sqrt((double)(((unsigned char)KEY(0x28)))+1) + math_const_1));

	var_14 += 0x51CE37 - 0x13E1F046;
	KEY(0x48) |= KEY(0x44) & 0x6361A322;

	if(type == 0x01)
	{
		enc_20_4D7680 (key, 0x3C017CE0);
		enc_20_4AB3B0 (key, 0xAC5ABC0C);
		TRY(enc_20_major_4AE190 (key, KEY(0x3C)));
	}

	RETURN;
}


THROWS_RET enc_20_major_4B24B0 (unsigned char *key, unsigned int seed)
{
	unsigned int type = (KEY(0x1C) ^ KEY(0x08) ^ seed) % 0x05;
	unsigned int var_14 = 0xEB2B07B;

	var_14 += 0x51CE37 - 0xBE8F461;
	KEY(0x08) -= var_14 & seed;

	if(type == 0x03)
	{
		enc_20_4A12B0 (key, 0x14C5815);
		enc_20_4EB2A0 (key, 0x6EAF97B0);
		enc_20_4EB310 (key, KEY(0x3C));
	}

	KEY(0x04) &= ROR(seed, 0xBD);
	var_14 += 0x51CE37 - 0x2489D1ED;

	if(cos((double)((unsigned char)KEY(0x04))) < math_const_2)
		KEY(0x30) = ROL(KEY(0x30), (unsigned char)var_14);
	else
		KEY(0x30) = ROL(KEY(0x30), (unsigned char)KEY(0x14));

	if(type == 0x02)
	{
		TRY(enc_20_4D7900 (key, 0x60A4946C));
		enc_20_4A12B0 (key, 0x3D835004);
		enc_20_4D7790 (key, seed);
	}
	
	seed = ROR(seed, (unsigned char)my_ftol(floor(sqrt((double)(((unsigned char)seed))+1) + math_const_1)));
	var_14 += 0x51CE37 - 0xC5F25EE;
	KEY(0x44) += (KEY(0x4C) * 61) << 1;

	if(type == 0x00)
	{
		enc_20_4D7740 (key, 0x132FE180);
		enc_20_4D1E50 (key, 0x1F0AAF01);
		TRY(enc_20_major_4AE190 (key, KEY(0x28)));
	}

	if(cos((double)((unsigned char)KEY(0x18))) < math_const_2)
		KEY(0x48) = ROR(KEY(0x48), (unsigned char)0x22210ED1);
	else
		KEY(0x48) = ROR(KEY(0x48), (unsigned char)KEY(0x04));

	var_14 += 0x51CE37 - 0xCEA86AA;
	seed ^= var_14 * seed;

	if(type == 0x04)
	{
		TRY(enc_20_4DA4D0 (key, 0xDFEDAEC1));
		enc_20_4D7660 (key, 0x3006CEAA);
		TRY(enc_20_major_4AE490 (key, seed));
	}

	KEY(0x28) -= my_ftol(floor(sqrt((double)(((unsigned char)seed))+1) + math_const_1));
	var_14 += 0x51CE37 - 0xA964DAF;
	KEY(0x2C) &= (seed << 6) - seed;

	if(type == 0x00)
	{
		enc_20_4D1E50 (key, 0x99ED391);
		enc_20_4AB3B0 (key, 0x69F5B967);
		TRY(enc_20_major_4E7CE0 (key, KEY(0x44)));
	}

	KEY(0x04) = ROL(KEY(0x04), (unsigned char)(KEY(0x3C) + var_14));

	RETURN;
}


THROWS_RET enc_20_mix (unsigned char *key, unsigned int seed)
{
	unsigned int type = (KEY(0x14) ^ KEY(0x24) ^ KEY(0x4C)) & 0x0F;
	unsigned int var_18 = 0x283EF81D;

	if(type == 0x9)
	{
		TRY(enc_20_4DA4D0 (key, 0x0A57E85CC));
		TRY(enc_20_4E7C50 (key, 0x0CEB224F2));
		TRY(enc_20_major_4DA520 (key, seed));
	}

	if(type == 0x5)
	{
		TRY(enc_20_4EEC50 (key, 0x0EA6A13F8));
		enc_20_4C4A60 (key, 0x3B1AD72C);
		TRY(enc_20_major_4B19A0 (key, KEY(0x24)));
	}

	var_18 += 0x51CE37 - 0x139AAE35;
	seed = 0x45835EB3;

	if(type == 0x8)
	{
		enc_20_4D6FB0 (key, 0x6A50F5DD);
		enc_20_4A1AE0 (key, 0x158B51AA);
		TRY(enc_20_major_4C4AC0 (key, KEY(0x28)));
	}

	if(type == 0x0A)
	{
		TRY(enc_20_4DA120 (key, 0x1C760834));
		enc_20_4D5720 (key, 0x0E8D8F62);
		TRY(enc_20_major_4F2B40 (key, KEY(0x00)));
	}

	if(type == 0x0)
	{
		enc_20_4A12B0 (key, 0x71E0BF51);
		TRY(enc_20_4DA120 (key, 0x3A331670));
		TRY(enc_20_major_4ABC60 (key, 0x45835EB3));
	}

	KEY(0x08) ^= KEY(0x18) + 0x1847DE17;

	if(type == 0x1)
	{
		enc_20_4D7660 (key, 0x2EBE9EE6);
		enc_20_4D7050 (key, 0x4783AEE5);
		TRY(enc_20_major_4D4910 (key, 0x45835EB3));
	}

	if(type == 0x0A)
	{
		enc_20_4D6FB0 (key, 0x79378FA0);
		enc_20_4D76A0 (key, 0x3AFC7FE8);
		TRY(enc_20_major_4D1E70 (key, KEY(0x18)));
	}

	var_18 += 0x51CE37 - 0x36493B6F;
	KEY(0x4C) += (KEY(0x30) * 13) << 3;

	if(type == 0x2)
	{
		enc_20_4D6FB0 (key, 0x6F278A7C);
		enc_20_4D9AD0 (key, 0x0AEC3F44A);
		TRY(enc_20_major_4D4FB0 (key, KEY(0x4C)));
		enc_20_4A1AE0 (key, 0x72B54F51);
		enc_20_4C4A60 (key, 0x678012FE);
		TRY(enc_20_major_4EB610 (key, KEY(0x00)));
	}

	if(type == 0x6)
	{
		TRY(enc_20_4AE420 (key, 0x706A6BC));
		TRY(enc_20_4BAC10 (key, 0x82B598A1));
		TRY(enc_20_major_4C1240 (key, 0x45835EB3));
	}

	KEY(0x1C) -= KEY(0x38) & 0x1ADA7FA;

	if(type == 0x4)
	{
		enc_20_4E7FB0 (key, 0x7B44A5DE);
		enc_20_4ABC20 (key, 0x0E4545CA5);
		TRY(enc_20_major_4CF650 (key, KEY(0x40)));
	}

	if(type == 0x6)
	{
		TRY(enc_20_4DA4D0 (key, 0x1DC5E29D));
		TRY(enc_20_4BAC10 (key, 0x0D2950F8C));
		TRY(enc_20_major_4BAC60 (key, 0x45835EB3));
	}

	var_18 += 0x51CE37 - 0x0EBDA763;
	seed = (KEY(0x00) + var_18) & 0x45835EB3;

	if(type == 0x4)
	{
		enc_20_4D5740 (key, 0x0F80C2802);
		enc_20_4D1E50 (key, 0x2973284);
		TRY(enc_20_major_4DA520 (key, KEY(0x14)));
	}

	if(type == 0x7)
	{
		TRY(enc_20_49AA90 (key, 0x6D32760));
		enc_20_4CE300 (key, 0x0EBC3F62D);
		TRY(enc_20_major_4B19A0 (key, KEY(0x20)));
	}

	if(type == 0x3)
	{
		TRY(enc_20_4DA120 (key, 0x0D6E6F79E));
		enc_20_4B0380 (key, 0x4DB66912);
		TRY(enc_20_major_4C4AC0 (key, KEY(0x44)));
	}

	KEY(0x20) = ROL(KEY(0x20), (unsigned char)(KEY(0x0C) ^ 0x4437EB26));

	if(type == 0x5)
	{
		enc_20_4F2AF0 (key, 0x8589D96B);
		enc_20_4D7000 (key, 0x8F7AF37A);
		TRY(enc_20_major_4F2B40 (key, KEY(0x4)));
	}

	if(type == 0x9)
	{
		enc_20_4D5740 (key, 0x1E36028E);
		TRY(enc_20_4DA4D0 (key, 0x5BD52248));
		TRY(enc_20_major_4ABC60 (key, KEY(0x8)));
	}

	if(type == 0x0)
	{
		enc_20_4B0380 (key, 0x638D174F);
		enc_20_4AE170 (key, 0x0A14D9558);
		TRY(enc_20_major_4D4910 (key, KEY(0x24)));
	}

	var_18 += 0x51CE37 - 0x14F2A403;
	KEY(0x24) *= KEY(0x38) | var_18;

	if(type == 0x0F)
	{
		enc_20_4CE2E0 (key, 0x0C23DB3DC);
		enc_20_4C47D0 (key, 0x0DBF1F593);
		TRY(enc_20_major_4D1E70 (key, KEY(0x20)));
	}

	if(type == 0x0C)
	{
		enc_20_4C47B0 (key, 0x402039DC);
		enc_20_4D7080 (key, 0x273D6EBA);
		TRY(enc_20_major_4D4FB0 (key, KEY(0x40)));
	}

	seed *= my_ftol(floor(sqrt((double)(((unsigned char)KEY(0x04)))+1) + math_const_1));

	if(type == 0x0B)
	{
		enc_20_4EB2C0 (key, 0x678AAE2C);
		enc_20_4F2AF0 (key, 0x806CB49B);
		TRY(enc_20_major_4EB610 (key, KEY(0x2C)));
	}

	if(type == 0x0E)
	{
		enc_20_4D7680 (key, 0x0A94E6A91);
		enc_20_49CED0 (key, 0x7CC192C9);
		TRY(enc_20_major_4C1240 (key, KEY(0x48)));
	}

	if(type == 0x7)
	{
		enc_20_49B260 (key, 0x0CDDA283E);
		enc_20_4AE3C0 (key, 0x3A13B215);
		TRY(enc_20_major_4CF650 (key, KEY(0x00)));
	}

	var_18 += 0x51CE37 - 0x0A85F4C6;
	KEY(0x0C) -= KEY(0x1C) ^ 0x4E46F05D;

	if(type == 0x3)
	{
		enc_20_4F2AA0 (key, 0x0EDA01E71);
		enc_20_4D5740 (key, 0x0D3803CED);
		TRY(enc_20_major_4BAC60 (key, KEY(0x2C)));
	}

	if(type == 0x8)
	{
		enc_20_4A12B0 (key, 0x1F6DE4C2);
		enc_20_4C5040 (key, 0x4F763BB2);
		TRY(enc_20_major_4DA520 (key, KEY(0x8)));
	}

	KEY(0x4C) ^= var_18 ^ seed;

	if(type == 0x0D)
	{
		enc_20_4D1E50 (key, 0x0F8C1BF93);
		enc_20_4E4F80 (key, 0x7CEED01C);
		TRY(enc_20_major_4B19A0 (key, KEY(0x3C)));
	}

	if(type == 0x1)
	{
		enc_20_4D9AD0 (key, 0x0B68D8CDD);
		enc_20_4D76A0 (key, 0x81A8EE7D);
		TRY(enc_20_major_4C4AC0 (key, KEY(0x14)));
	}

	var_18 += 0x51CE37 - 0x14953064;
	KEY(0x18) ^= my_ftol(floor(sqrt((double)(((unsigned char)KEY(0x14)))+1) + math_const_1));

	return 0;
}
