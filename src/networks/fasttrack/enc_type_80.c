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

#define TRY(x) if((x) & 1) return;

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

/* some constants and helper funcs */

static u32 ROR (u32 value, u32 count)
{
	count = (count & 0xff) % 32;
	return (value >> count) | (value << (32 - count));
}

static u32 ROL (u32 value, u32 count)
{
	count = (count & 0xff) % 32;
	return (value << count) | (value >> (32 - count));
}

/* the entry point of this mess */
/* this all works on unsigned ints so endianess is not an issue */

static void major_0 (u32 *pad, u32 seed);

void enc_type_80 (u32 *pad, u32 seed)
{
	major_0 (pad, seed);
}

/* major functions which make calls to other funcs */
static void major_1 (u32 *pad, u32 seed);
static void major_2 (u32 *pad, u32 seed);
static void major_3 (u32 *pad, u32 seed);
static void major_4 (u32 *pad, u32 seed);
static void major_5 (u32 *pad, u32 seed);
static void major_6 (u32 *pad, u32 seed);
static void major_7 (u32 *pad, u32 seed);
static void major_8 (u32 *pad, u32 seed);
static void major_9 (u32 *pad, u32 seed);
static void major_10 (u32 *pad, u32 seed);
static void major_11 (u32 *pad, u32 seed);
static void major_12 (u32 *pad, u32 seed);
static void major_13 (u32 *pad, u32 seed);
static void major_14 (u32 *pad, u32 seed);
static void major_15 (u32 *pad, u32 seed);
static void major_16 (u32 *pad, u32 seed);
static void major_17 (u32 *pad, u32 seed);
static void major_18 (u32 *pad, u32 seed);
static void major_19 (u32 *pad, u32 seed);
static void major_20 (u32 *pad, u32 seed);
static void major_21 (u32 *pad, u32 seed);

static void minor_74 (u32 *pad, u32 seed);
static void minor_75 (u32 *pad);

/* and so it begins... */

#define minor_23 pad[5] += pad[4] * 73
#define minor_24 pad[0] += pad[5] & 0x1B65B2C8
#define minor_25 pad[9] += pad[6] + 0x124D08A0
#define minor_26 pad[16] ^= (pad[0] * 41) << 1
#define minor_27 pad[14] ^= pad[10] - 0x403483CE
#define minor_28 pad[3] ^= (pad[11] * 41) << 1
#define minor_29(x) pad[10] = ROL (pad[10], x)
#define minor_30 pad[1] = ROR (pad[1], (pad[8] * 7) << 4)
#define minor_31 pad[10] *= ROL (pad[1], 0xc)
#define minor_32(x) pad[18] = ROL (pad[18], x - 0xE066BF0)
#define minor_33(x) pad[18] -= x
#define minor_34 pad[16] |= pad[18] | 0xB25175E
#define minor_35 pad[2] |= pad[15] - 0x1886D6A
#define minor_36 pad[16] -= ROL (pad[3], 0x1b)
#define minor_37 pad[11] |= pad[13] * 9
#define minor_38 pad[6] &= pad[19] ^ 0x1FAF0F41
#define minor_39 pad[4] = ROR (pad[4], (pad[17] * 11) << 3)
#define minor_40 pad[17] *= pad[6]
#define minor_41 pad[3] ^= pad[9] + 0x5B1A81FD
#define minor_42 pad[17] *= ROR (pad[10], 0x1a)
#define minor_43 pad[19] ^= pad[3] ^ 0x19859C46
#define minor_44 pad[16] += pad[1] + 0x5EDB78DA
#define minor_45 pad[12] = ROL (pad[12], (pad[3] * 43) << 1)
#define minor_46 pad[17] *= pad[13] - 0x68C0E272
#define minor_47 pad[13] += pad[13] | 0x5E919E06
#define minor_48 pad[16] = (pad[9] * pad[16] * 37) << 1
#define minor_49 pad[2] |= pad[15] + 0x44B04775
#define minor_50 pad[8] = (pad[8] * pad[8] * 13) << 2
#define minor_51(x) pad[17] = ROR (pad[17], x)
#define minor_52(x) pad[10] *= x
#define minor_53 pad[8] += pad[17] + 0x4E0679BE
#define minor_54 pad[10] ^= pad[6] ^ 0x2BE68205
#define minor_55(x) pad[7] -= x
#define minor_56(x) pad[13] &= ROL (x, 0x10)
#define minor_57(x) pad[9] &= x
#define minor_58(x) pad[0] += x
#define minor_59 pad[8] *= pad[6] ^ 0x377C08D2
#define minor_60 pad[18] -= pad[10] < 0xB6C6C3E ? pad[10] : pad[13]
#define minor_61 pad[19] += my_sqrt (pad[18])
#define minor_62 pad[11] ^= my_sqrt (pad[4])
#define minor_63 pad[17] ^= my_sqrt (pad[16])
#define minor_64 pad[9] += my_sqrt (pad[13])
#define minor_65(x) pad[10] -= my_cos (x) ? 0x19C6C6E : pad[7]
#define minor_66 pad[10] -= my_cos (pad[10]) ? 0x5F18F01 : pad[13]
#define minor_67(x) pad[17] = ROL (pad[17], my_sin (pad[7]) ? 0x4262ED6B : x)
#define minor_68 pad[9] += my_cos (pad[6]) ? 0x14F5046C : pad[11]
#define minor_69 pad[14] ^= my_cos (pad[11]) ? 0x562482FA : pad[14]
#define minor_70 pad[3] -= my_cos (pad[14]) ? 0x39702EDD : pad[6]
#define minor_71 pad[6] ^= my_cos (pad[10]) ? 0x53236223 : pad[7]
#define minor_72(x) pad[1] |= my_sin (x) ? 0x1BE7FECF : pad[16]
#define minor_73 pad[17] *= my_cos (pad[16]) ? 0x2C15B485 : pad[3]
#define minor_3 pad[19] += my_sqrt (pad[0])
#define minor_4 pad[7] = ROL (pad[7], my_sqrt (pad[1]))
#define minor_5 pad[4] += my_cos (pad[1]) ? 0x890AFEF : pad[4]
#define minor_6(x) pad[7] = ROL (pad[7], my_sin (pad[18]) ? 0x14D1DE3D : x)
#define minor_7 pad[10] &= pad[1] < 0x1F2DD61 ? pad[1] : pad[10]
#define minor_8 pad[0] |= ((pad[10] * 3) << 4) + pad[10]
#define minor_9 pad[9] ^= pad[1] < 0xB01609F ? pad[1] : pad[16]
#define minor_10(x) pad[13] = ROR (pad[13], x)
#define minor_11(x) pad[7] &= x
#define minor_12 pad[11] += ROL (pad[5], 0x14)
#define minor_13(x) pad[5] ^= x + 0x391B56A
#define minor_14 pad[3] ^= pad[14] * 0x711881F7
#define minor_15 pad[6] -= pad[18] ^ 0x4402CAF
#define minor_16 pad[2] += ROR (pad[14], 0x14)
#define minor_17 pad[6] += 0x287735D1 - pad[3]
#define minor_18 pad[3] += 0x247B4DE9 - pad[1]
#define minor_19 pad[11] += pad[6] * 0xE8C67004
#define minor_20 pad[15] |= pad[1] ^ 0x37B6FE35
#define minor_21 pad[14] = ROR (pad[14], pad[13] | 0x3433BE6)

void minor_74 (u32 *pad, u32 seed)
{
	pad[10] *= my_cos (pad[3]) ? 0x1DD34A4 : pad[2];
	pad[12] |= (pad[16] * 11) << 2;
	pad[0] ^= pad[19] ^ 0x509e41a1;
	pad[13] -= (((pad[9] * 15) << 1) - pad[9]) << 2;
	seed -= my_cos (pad[2]) ? 0xD7A79F4 : pad[5];
	pad[2] -= my_sin (pad[12]) ? 0x5134639d : pad[1];
	seed ^= my_sin (pad[4]) ? 0x241147A3 : pad[13];
	pad[3] += my_sqrt (seed);
	pad[0] -= my_sin (seed) ? 0x5176ffdf : pad[15];
	pad[3] -= (((pad[9] * 15) << 1) - pad[9]) * 3;
	pad[14] |= pad[2] < 0x18897 ? pad[2] : pad[10];
	pad[13] ^= 0x18897 + seed;
}

void minor_75 (u32 *pad)
{
	pad[13] -= pad[2];
	pad[19] += 0x471c95 & pad[12];
	pad[14] += pad[7] < 0x471c95 ? pad[7] : pad[2];
	pad[12] -= pad[11] ^ 0x5E823762;
	pad[15] &= (pad[0] * 13) << 2;
	pad[17] ^= pad[8] * 15;
	pad[7] += pad[4] & 0x3996FD51;
	pad[2] += my_sin (pad[19]) ? 0x34311111 : pad[5];
}

void major_1 (u32 *pad, u32 seed)
{
	u32 branch = (pad[8] ^ pad[16] ^ seed) % 5;

	seed *= my_sqrt (seed);
	pad[4] |= seed < 0x67f88 ? seed : pad[3];

	if (branch == 1)
	{
		minor_44;
		minor_63;
		minor_75 (pad);
	}

	pad[7] = ROR (pad[7], pad[17] & 0x67f88);
	pad[0] += ROR (pad[6], 0x8);

	if (branch == 0)
	{
		minor_35;
		minor_29 (20);
		minor_74 (pad, seed);
	}

	seed -= pad[14] ^ 0xffc56d16;
	seed &= my_sqrt (pad[1]);

	if (branch == 2)
	{
		minor_72 (128);
		TRY (minor_17);
		major_10 (pad, pad[6]);
	}

	seed = ROL (seed, pad[13] + 0x8D810DF);
	pad[7] ^= (seed * 11) << 1;

	if (branch == 3)
	{
		minor_33 (0xfde76000);
		minor_73;
		major_20 (pad, pad[8]);
	}

	pad[8] ^= my_cos (pad[16]) ? 0x12DA5B58 : pad[2];
	pad[14] ^= pad[13] - 0xff8c9a4d;

	if (branch == 4)
	{
		minor_36;
		minor_69;
		major_6 (pad, seed);
	}

	pad[6] *= pad[17] < 0x9D9106A ? pad[17] : pad[8];
}

void major_2 (u32 *pad, u32 seed)
{
	u32 branch = (pad[10] ^ pad[0] ^ pad[13]) % 0x0E;

	if (branch == 3)
	{
		TRY (minor_16);
		minor_46;
		major_10 (pad, pad[10]);
	}

	pad[19] = ROL (pad[19], seed * 41);

	if (branch == 0x0D)
	{
		TRY (minor_20);
		minor_35;
		major_20 (pad, seed);
	}

	if (branch == 2)
	{
		minor_36;
		minor_73;
		major_6 (pad, pad[1]);
	}

	pad[12] &= seed & 0x162E075D;

	if (branch == 0)
	{
		minor_62;
		minor_51 (11);
		major_1 (pad, pad[3]);
	}

	pad[7] += 0xC0CF1E75 - pad[18];

	if (branch == 0)
	{
		minor_60;
		minor_55 (0x5952d3f0);
		major_4 (pad, pad[13]);
	}

	if (branch == 0x0B)
	{
		TRY (minor_14);
		minor_70;
		major_16 (pad, seed);
	}

	pad[10] *= seed + 0x15A0944D;

	if (branch == 7)
	{
		minor_54;
		TRY (minor_6 (0xE636D969));
		major_7 (pad, pad[14]);
	}

	pad[13] += pad[0] - 0x3B1C8FB8;

	if (branch == 8)
	{
		minor_59;
		minor_53;
		major_8 (pad, pad[5]);
	}

	if (branch == 6)
	{
		TRY (minor_13 (0x34472A0D));
		minor_61;
		major_18 (pad, pad[1]);
	}

	pad[18] *= pad[2] < 0xF9B0FAE ? pad[2] : pad[19];

	if (branch == 0x0C)
	{
		TRY (minor_6 (0xDF3D4CFC));
		TRY (minor_19);
		major_12 (pad, seed);
	}

	pad[1] += (((pad[5] * 3) << 3) - pad[5]) << 1;

	if (branch == 1)
	{
		minor_68;
		minor_58 (0xf586317c);
		major_5 (pad, pad[17]);
	}

	if (branch == 4)
	{
		minor_68;
		minor_69;
		major_15 (pad, seed);
	}

	pad[11] ^= 0xffb29fb8 | pad[13];

	if (branch == 0x0A)
	{
		TRY (minor_12);
		minor_40;
		major_10 (pad, pad[11]);
	}

	pad[13] ^= seed ^ 0x265916C9;

	if (branch == 5)
	{
		minor_39;
		TRY (minor_7);
		major_20 (pad, pad[11]);
	}

	if (branch == 9)
	{
		minor_41;
		minor_28;
		major_6 (pad, pad[1]);
	}

	pad[2] *= pad[5] * 105;
}

void major_3 (u32 *pad, u32 seed)
{
	u32 branch = (pad[3] ^ pad[14] ^ seed) & 0x0F;

	if (branch == 0x0C)
	{
		minor_46;
		minor_72 (75);
		major_7 (pad, seed);
	}

	if (branch == 4)
	{
		minor_23;
		minor_47;
		major_8 (pad, pad[19]);
	}

	seed -= pad[5] * 18;

	if (branch == 8)
	{
		TRY (minor_11 (0x890edab4));
		minor_31;
		major_18 (pad, seed);
	}

	if (branch == 0x0E)
	{
		minor_46;
		minor_41;
		major_12 (pad, pad[4]);
	}

	pad[19] ^= 0xbd8b55f ^ pad[11];

	if (branch == 0x0B)
	{
		minor_56 (0x20AA3ABD);
		TRY (minor_13 (0x5D894D2));
		major_5 (pad, pad[17]);
	}

	if (branch == 4)
	{
		minor_69;
		minor_60;
		major_15 (pad, seed);
	}

	pad[16] -= (((pad[3] * 3) << 2) - pad[3]) << 1;

	if (branch == 1)
	{
		minor_43;
		minor_52 (0x5b5a9b00);
		major_2 (pad, seed);
	}

	if (branch == 9)
	{
		minor_45;
		minor_49;
		major_11 (pad, pad[9]);
	}

	pad[14] *= pad[5] | 0x132A8FBD;

	if (branch == 0x0A)
	{
		minor_51 (31);
		minor_57 (0xb1e94414);
		major_14 (pad, seed);
	}

	if (branch == 0x0D)
	{
		minor_35;
		minor_42;
		major_13 (pad, seed);
	}

	if (branch == 2)
	{
		TRY (minor_3);
		minor_56 (0xFE2E49B0);
		major_19 (pad, pad[17]);
	}

	pad[12] += ((-(((seed * 5) << 2) - seed)) * 3) << 1;

	if (branch == 1)
	{
		TRY (minor_10 (30));
		minor_42;
		major_17 (pad, pad[14]);
	}

	if (branch == 2)
	{
		minor_54;
		TRY (minor_5);
		major_7 (pad, seed);
	}

	pad[16] *= my_cos (seed) ? 0x40E92E8A : pad[9];

	if (branch == 0)
	{
		minor_39;
		TRY (minor_16);
		major_8 (pad, pad[5]);
		minor_44;
		minor_68;
		major_18 (pad, pad[10]);
	}

	seed *= pad[2] ^ 0xE5C69EA;

	if (branch == 5)
	{
		TRY (minor_10 (10));
		minor_47;
		major_12 (pad, pad[9]);
	}

	if (branch == 7)
	{
		TRY (minor_8);
		minor_30;
		major_5 (pad, pad[0]);
	}

	pad[8] += seed - 0x1DBCEFC2;

	if (branch == 3)
	{
		minor_43;
		minor_23;
		major_15 (pad, pad[14]);
	}

	if (branch == 6)
	{
		minor_47;
		minor_55 (0xecb0c1a8);
		major_2 (pad, seed);
	}

	seed += pad[15] - 0x3206A0CA;

	if (branch == 3)
	{
		minor_31;
		minor_35;
		major_11 (pad, seed);
	}

	if (branch == 0x0F)
	{
		TRY (minor_17);
		minor_28;
		major_14 (pad, pad[1]);
	}

	pad[14] ^= pad[8] & 0x7d271;
}

void major_4 (u32 *pad, u32 seed)
{
	u32 branch = (pad[12] ^ pad[11] ^ seed) % 0x06;

	pad[11] *= 0x65300 | seed;

	if (branch == 2)
	{
		minor_40;
		minor_34;
		minor_75 (pad);
	}

	seed += 0 - (((pad[6] * 5) << 4) - pad[6]);
	pad[8] = ROL (pad[8], my_sqrt (pad[9]));

	if (branch == 0)
	{
		TRY (minor_9);
		minor_26;
		minor_74 (pad, pad[4]);
	}

	pad[0] -= pad[14] ^ 0xff7db6c1;
	pad[6] = ROL (pad[6], pad[11] ^ 0xffc1081c);

	if (branch == 5)
	{
		minor_36;
		minor_36;
		major_10 (pad, pad[7]);
	}

	seed -= pad[11] * 3 << 4;

	if (branch == 0)
	{
		minor_31;
		minor_36;
		major_20 (pad, seed);
	}

	pad[13] |= ROR (pad[16], 0xc);
	pad[3] |= pad[9] + 0x27C4C44E;

	if (branch == 1)
	{
		minor_51 (0);
		minor_27;
		major_6 (pad, seed);
	}

	pad[13] -= (((pad[6] * 5) << 3) - pad[6]) << 1;
	pad[10] ^= ROL (pad[11], 0x4);

	if (branch == 3)
	{
		minor_63;
		TRY (minor_8);
		major_1 (pad, pad[10]);
	}
}

void major_5 (u32 *pad, u32 seed)
{
	u32 branch = (pad[14] ^ pad[2] ^ pad[19]) % 0x0C;

	if (branch == 4)
	{
		minor_40;
		TRY (minor_4);
		minor_75 (pad);
	}

	seed |= pad[11] ^ 0x4E05B048;

	if (branch == 0x0A)
	{
		minor_64;
		minor_69;
		minor_74 (pad, pad[1]);
	}

	seed += pad[2] * 50;

	if (branch == 1)
	{
		TRY (minor_8);
		minor_47;
		major_10 (pad, seed);
	}

	seed = (seed * pad[3] * 13) << 1;

	if (branch == 3)
	{
		minor_47;
		minor_44;
		major_20 (pad, pad[7]);
	}

	pad[10] |= pad[19] & 0x42A403D;

	if (branch == 6)
	{
		minor_45;
		TRY (minor_17);
		major_6 (pad, seed);
	}

	pad[4] = ROR (pad[4], pad[18] + 0x486c59);

	if (branch == 2)
	{
		TRY (minor_8);
		minor_55 (0xdd66dc5c);
		major_1 (pad, pad[4]);
	}

	pad[11] += my_sin (seed) ? 0x44ACFBD : pad[9];

	if (branch == 9)
	{
		minor_42;
		minor_65 (117);
		major_4 (pad, pad[12]);
	}

	seed = ROR (seed, seed < 0xaa6dc ? seed : pad[17]);

	if (branch == 0)
	{
		minor_40;
		TRY (minor_5);
		major_16 (pad, pad[15]);
	}

	pad[16] *= 0xffc209cf ^ seed;

	if (branch == 0)
	{
		minor_59;
		minor_52 (0xb7709fc1);
		major_7 (pad, pad[5]);
	}

	pad[4] += pad[17] * 0xE9F05570;

	if (branch == 5)
	{
		minor_61;
		minor_47;
		major_8 (pad, pad[4]);
	}

	pad[13] *= pad[8] ^ 0x4001ca;

	if (branch == 7)
	{
		minor_41;
		minor_56 (0x1D1451DE);
		major_18 (pad, pad[1]);
	}

	if (branch == 8)
	{
		minor_41;
		minor_28;
		major_12 (pad, pad[11]);
	}

	pad[13] &= pad[9] * 0x4463ff;

	if (branch == 0x0B)
	{
		minor_42;
		minor_34;
		minor_75 (pad);
	}
}

void major_6 (u32 *pad, u32 seed)
{
	u32 branch = (pad[9] ^ pad[5] ^ pad[6]) & 0x03;

	pad[12] += my_sqrt (pad[1]);
	pad[18] += pad[19] + 0x2B7FD413;

	if (branch == 0)
	{
		minor_39;
		minor_43;
		minor_75 (pad);
	}

	seed = ROR (seed, pad[4] - 0xA4E29DD);
	seed ^= ROR (pad[17], 0x1c);

	if (branch == 0)
	{
		TRY (minor_15);
		minor_40;
		minor_74 (pad, pad[3]);
	}

	pad[5] += ((seed * 27) << 1) - seed;
	pad[18] += pad[6] - 0x24A58A0;

	if (branch == 3)
	{
		minor_71;
		minor_29 (116);
		major_10 (pad, pad[18]);
	}

	pad[10] &= my_sin (pad[17]) ? 0xf1f3d : pad[13];

	pad[19] |= my_sin (pad[18]) ? 0x4f2a72 : pad[4];
	pad[13] *= ROR (pad[12], 0x12);

	if (branch == 2)
	{
		TRY (minor_21);
		minor_33 (0xfc47b82b);
		major_20 (pad, pad[2]);
	}

	pad[6] += (pad[1] << 6) + pad[1];
}

void major_7 (u32 *pad, u32 seed)
{
	u32 branch = pad[9] & 0x07;

	seed -= 0x646ede77 & pad[7];

	if (branch == 0)
	{
		minor_38;
		TRY (minor_12);
		minor_75 (pad);
	}

	seed = ROR (seed, (seed * 59) << 1);

	if (branch == 6)
	{
		minor_40;
		minor_51 (15);
		minor_74 (pad, pad[4]);
	}

	pad[3] += my_sin (seed) ? 0x4c8444 : pad[8];

	if (branch == 5)
	{
		minor_61;
		minor_32 (0x2AB2E7E2);
		major_10 (pad, pad[18]);
	}

	pad[2] = ROL (pad[2], (((pad[4] * 3) << 3) - pad[4]) * 3);

	if (branch == 3)
	{
		minor_42;
		minor_56 (0xD1FC123);
		major_20 (pad, seed);
	}

	pad[0] += my_sin (pad[4]) ? 0xda2b08 : seed;

	if (branch == 2)
	{
		TRY (minor_19);
		minor_57 (0xd7f6df8f);
		major_6 (pad, pad[9]);
	}

	seed ^= seed * 0x11;

	if (branch == 4)
	{
		minor_27;
		minor_36;
		major_1 (pad, pad[10]);
	}

	pad[4] *= 0xdeaf0b ^ pad[2];

	if (branch == 1)
	{
		minor_25;
		TRY (minor_20);
		major_4 (pad, seed);
	}

	pad[9] = ((pad[11] * pad[9] * 5) << 3) - pad[11] * pad[9];
	pad[1] += ((pad[10] << 5) + pad[10]) * 3;

	if (branch == 0)
	{
		TRY (minor_19);
		TRY (minor_7);
		major_16 (pad, pad[0]);
	}
}

void major_8 (u32 *pad, u32 seed)
{
	u32 branch = (pad[4] ^ pad[13] ^ pad[17]) % 0x09;

	seed ^= ROR (seed, 0x6);

	if (branch == 0)
	{
		TRY (minor_13 (0x2816580B));
		minor_38;
		minor_75 (pad);
	}

	pad[2] &= pad[16] ^ 0x448522;

	if (branch == 8)
	{
		TRY (minor_14);
		TRY (minor_21);
		minor_74 (pad, pad[15]);
	}

	pad[7] &= my_sin (seed) ? 0x5228985F : pad[5];

	if (branch == 3)
	{
		TRY (minor_5);
		minor_33 (0x168a878e);
		major_10 (pad, pad[18]);
	}

	seed *= pad[3] ^ 0x93fa66;

	if (branch == 7)
	{
		TRY (minor_8);
		minor_58 (0xf81bec49);
		major_20 (pad, pad[13]);
	}

	pad[7] -= ((seed * 5) << 2) - seed;

	if (branch == 4)
	{
		minor_55 (0x32ea67a4);
		minor_47;
		major_6 (pad, pad[13]);
	}

	pad[12] = ROL (pad[12], (((seed << 4) + seed) * 3) << 1);

	if (branch == 6)
	{
		minor_72 (69);
		TRY (minor_14);
		major_1 (pad, pad[5]);
	}

	pad[19] = ROL (pad[19], seed + 0x4685D5BE);

	if (branch == 1)
	{
		TRY (minor_9);
		minor_40;
		major_4 (pad, pad[14]);
	}

	pad[5] += pad[6] | 0x301DE279;

	if (branch == 2)
	{
		minor_28;
		minor_61;
		major_16 (pad, pad[17]);
	}

	pad[6] *= pad[0] ^ 0xffffffbb;

	if (branch == 0)
	{
		minor_23;
		minor_32 (0x11B2399C);
		major_7 (pad, pad[14]);
	}

	pad[17] |= my_sqrt (pad[12]);
}

void major_9 (u32 *pad, u32 seed)
{
	u32 branch = pad[9] & 0x0F;

	if (branch == 0x0D)
	{
		minor_53;
		minor_36;
		major_18 (pad, pad[13]);
	}

	if (branch == 2)
	{
		TRY (minor_21);
		minor_40;
		major_12 (pad, pad[10]);
	}

	pad[1] -= 0x49d06938 ^ pad[7];

	if (branch == 6)
	{
		TRY (minor_7);
		minor_31;
		major_5 (pad, seed);
	}

	if (branch == 0x0E)
	{
		minor_51 (29);
		TRY (minor_5);
		major_15 (pad, seed);
	}

	pad[13] *= my_cos (seed) ? 0x46223265 : pad[18];

	if (branch == 6)
	{
		TRY (minor_3);
		TRY (minor_8);
		major_2 (pad, pad[3]);
	}

	if (branch == 4)
	{
		minor_46;
		minor_53;
		major_11 (pad, seed);
	}

	pad[6] += (seed * 3) << 3;

	if (branch == 4)
	{
		TRY (minor_12);
		minor_65 (182);
		major_14 (pad, pad[14]);
	}

	if (branch == 2)
	{
		minor_58 (0xfa9054cb);
		TRY (minor_4);
		major_13 (pad, pad[0]);
	}

	seed ^= my_sin (pad[11]) ? 0x2F24FB19 : pad[16];

	if (branch == 0x0A)
	{
		minor_40;
		minor_70;
		major_19 (pad, pad[7]);
	}

	if (branch == 0x0B)
	{
		minor_50;
		minor_72 (5);
		major_17 (pad, pad[9]);
	}

	if (branch == 5)
	{
		minor_52 (0xc4de785);
		minor_36;
		major_3 (pad, seed);
	}

	pad[18] *= pad[2] + 0x92c5d3;

	if (branch == 1)
	{
		minor_35;
		minor_64;
		major_21 (pad, pad[7]);
	}

	if (branch == 5)
	{
		TRY (minor_17);
		minor_68;
		major_18 (pad, seed);
	}

	pad[6] -= pad[8] & 0x42D2AFE6;

	if (branch == 0)
	{
		minor_46;
		TRY (minor_7);
		major_12 (pad, pad[9]);
	}

	if (branch == 7)
	{
		minor_26;
		minor_64;
		major_5 (pad, pad[3]);
	}

	pad[14] = ROR (pad[14], pad[3] + 0x44060020);

	if (branch == 3)
	{
		minor_35;
		minor_59;
		major_15 (pad, pad[2]);
	}

	if (branch == 9)
	{
		minor_39;
		minor_51 (14);
		major_2 (pad, seed);
	}

	pad[11] &= seed ^ 0x26649BD;

	if (branch == 0x0C)
	{
		minor_43;
		minor_40;
		major_11 (pad, seed);
	}

	if (branch == 0x0F)
	{
		minor_33 (0x15507317);
		minor_31;
		major_14 (pad, pad[17]);
	}

	pad[3] = ROR (pad[3], 0xd783ed & pad[11]);

	if (branch == 1)
	{
		TRY (minor_8);
		minor_28;
		major_13 (pad, pad[13]);
	}

	if (branch == 8)
	{
		minor_28;
		minor_40;
		major_19 (pad, pad[11]);
	}

	if (branch == 0)
	{
		TRY (minor_9);
		minor_35;
		major_17 (pad, seed);
	}

	pad[10] += my_cos (pad[0]) ? 0xF1B21FE : seed;

	if (branch == 3)
	{
		minor_60;
		TRY (minor_14);
		major_3 (pad, pad[1]);
	}

	pad[19] *= pad[18] * 73;
}

void major_10 (u32 *pad, u32 seed)
{
	u32 branch = (pad[7] ^ pad[3] ^ seed) & 0x01;

	pad[10] &= 0x75eefb0 ^ seed;

	pad[11] ^= my_cos (pad[1]) ? 0x2E0A5BE7 : pad[6];
	pad[0] += seed + 0x1FE76B44;

	pad[13] += 0x76173ce - seed;

	if (branch == 1)
	{
		minor_56 (0xCD17011);
		minor_48;
		minor_75 (pad);
	}

	pad[16] -= pad[11] < 0x56C0185B ? pad[11] : pad[19];
	pad[0] ^= my_sin (pad[16]) ? 0x5A271260 : seed;

	pad[6] += pad[11] | 0x58E035D2;

	pad[14] = ROL (pad[14], ROL (pad[1], 0x8));
	pad[13] ^= pad[15] * 91;

	if (branch == 0)
	{
		minor_57 (0x37fed3);
		TRY (minor_12);
		minor_74 (pad, pad[14]);
	}
}

void major_11 (u32 *pad, u32 seed)
{
	u32 branch = pad[14] % 0x0F;

	if (branch == 0x0A)
	{
		minor_60;
		minor_49;
		major_20 (pad, pad[19]);
	}

	pad[11] = ROR (pad[11], 0xa938539 & pad[5]);

	if (branch == 3)
	{
		minor_53;
		TRY (minor_10 (24));
		major_6 (pad, seed);
	}

	if (branch == 0)
	{
		minor_70;
		TRY (minor_15);
		major_1 (pad, pad[12]);
	}

	pad[7] *= my_cos (pad[4]) ? 0xA45B84A : pad[13];

	if (branch == 4)
	{
		TRY (minor_4);
		minor_44;
		major_4 (pad, pad[0]);
	}

	seed |= my_sin (seed) ? 0xae00950 : pad[10];

	if (branch == 0x0D)
	{
		TRY (minor_10 (12));
		minor_66;
		major_16 (pad, pad[7]);
	}

	if (branch == 0)
	{
		minor_23;
		minor_70;
		major_7 (pad, seed);
	}

	pad[15] += pad[13] + 0x519A438A;

	if (branch == 1)
	{
		TRY (minor_12);
		minor_41;
		major_8 (pad, pad[4]);
	}

	if (branch == 2)
	{
		minor_23;
		minor_46;
		major_18 (pad, pad[15]);
	}

	pad[9] -= my_cos (seed) ? 0xaa60c04 : pad[15];

	if (branch == 0x0C)
	{
		minor_26;
		minor_24;
		major_12 (pad, pad[7]);
	}

	if (branch == 5)
	{
		TRY (minor_12);
		minor_53;
		major_5 (pad, pad[15]);
	}

	pad[8] += ROR (pad[19], 0x4);

	if (branch == 0x0B)
	{
		minor_31;
		minor_38;
		major_15 (pad, pad[4]);
	}

	pad[7] -= my_sin (pad[5]) ? 0xaf6039c : pad[12];

	if (branch == 6)
	{
		minor_70;
		minor_67 (0xB27F04F);
		major_2 (pad, seed);
	}

	if (branch == 0x0E)
	{
		minor_48;
		TRY (minor_5);
		major_20 (pad, seed);
	}

	pad[5] *= my_cos (seed) ? 0x5DF8323 : seed;

	if (branch == 8)
	{
		minor_52 (0xbfb3dfcf);
		minor_24;
		major_6 (pad, seed);
	}

	if (branch == 9)
	{
		minor_53;
		minor_46;
		major_1 (pad, pad[14]);
	}

	pad[7] ^= pad[15] ^ 0x460390;

	if (branch == 7)
	{
		minor_25;
		minor_44;
		major_4 (pad, seed);
	}

	pad[19] = 0;
}

void major_12 (u32 *pad, u32 seed)
{
	u32 branch = pad[15] % 0x0B;

	if (branch == 5)
	{
		TRY (minor_16);
		minor_62;
		minor_75 (pad);
	}

	pad[4] -= my_cos (pad[14]) ? 0x76F737A3 : seed;

	if (branch == 0x0A)
	{
		minor_42;
		minor_65 (186);
		minor_74 (pad, seed);
	}

	pad[3] &= pad[10] ^ 0x20773F85;

	if (branch == 4)
	{
		minor_47;
		minor_28;
		major_10 (pad, pad[14]);
	}

	pad[12] = ROL (pad[12], pad[7] * 91);

	if (branch == 9)
	{
		minor_37;
		minor_73;
		major_20 (pad, seed);
	}

	if (branch == 2)
	{
		minor_67 (0xE8B01B9B);
		minor_54;
		major_6 (pad, pad[8]);
	}

	pad[3] += ROL (pad[11], 0x1d);

	if (branch == 6)
	{
		minor_70;
		minor_23;
		major_1 (pad, pad[0]);
	}

	if (branch == 8)
	{
		minor_29 (60);
		minor_34;
		major_4 (pad, pad[12]);
	}

	pad[9] *= pad[0] < 0xED7837 ? pad[0] : pad[15];

	if (branch == 3)
	{
		TRY (minor_13 (0x19068E93));
		minor_25;
		major_16 (pad, pad[15]);
	}

	if (branch == 0)
	{
		minor_37;
		minor_45;
		major_7 (pad, pad[0]);
	}

	pad[16] ^= pad[6] ^ 0x28AA2736;

	if (branch == 1)
	{
		minor_59;
		minor_37;
		major_8 (pad, pad[2]);
	}

	if (branch == 0)
	{
		TRY (minor_18);
		minor_48;
		major_18 (pad, pad[6]);
	}

	pad[2] ^= pad[19] * 101;

	if (branch == 7)
	{
		minor_32 (0x2A4F7758);
		minor_39;
		minor_75 (pad);
	}

	pad[0] &= my_cos (pad[0]) ? 0xfff2b634 : pad[5];
}

void major_13 (u32 *pad, u32 seed)
{
	u32 branch = (pad[1] ^ pad[16] ^ pad[18]) & 0x0F;

	if (branch == 1)
	{
		minor_68;
		minor_29 (20);
		major_1 (pad, pad[3]);
	}

	pad[5] += 0x37C9C771 - pad[18];

	if (branch == 0)
	{
		minor_69;
		minor_39;
		major_4 (pad, pad[1]);
	}

	if (branch == 8)
	{
		minor_66;
		TRY (minor_17);
		major_16 (pad, pad[7]);
	}

	pad[0] -= my_sin (seed) ? 0xE2D0D9 : seed;

	if (branch == 7)
	{
		minor_58 (0xf6f6309c);
		TRY (minor_18);
		major_7 (pad, pad[0]);
	}

	if (branch == 6)
	{
		minor_41;
		TRY (minor_7);
		major_8 (pad, pad[0]);
	}

	pad[11] = ROR (pad[11], my_cos (seed) ? 0xf1085c5 : pad[11]);

	if (branch == 1)
	{
		minor_29 (156);
		minor_56 (0x2CDA662B);
		major_18 (pad, pad[3]);
	}

	if (branch == 0x0F)
	{
		TRY (minor_18);
		minor_43;
		major_12 (pad, pad[18]);
	}

	pad[17] -= ROL (pad[11], 0x18);

	if (branch == 2)
	{
		minor_70;
		minor_64;
		major_5 (pad, pad[17]);
	}

	if (branch == 9)
	{
		minor_70;
		minor_63;
		major_15 (pad, seed);
	}

	if (branch == 0x0A)
	{
		minor_70;
		minor_58 (0xe6e37dd2);
		major_2 (pad, pad[5]);
	}

	if (branch == 0x0E)
	{
		TRY (minor_5);
		minor_42;
		major_11 (pad, pad[0]);
	}

	if (branch == 0x0C)
	{
		TRY (minor_12);
		minor_41;
		major_14 (pad, pad[18]);
	}

	if (branch == 3)
	{
		minor_43;
		minor_33 (0xa5ffc324);
		major_1 (pad, pad[12]);
	}

	pad[14] ^= pad[4] | 0xC893A6;

	if (branch == 0x0B)
	{
		minor_45;
		minor_33 (0x477431cd);
		major_4 (pad, pad[12]);
	}

	if (branch == 0)
	{
		TRY (minor_13 (0x398C59E0));
		TRY (minor_15);
		major_16 (pad, pad[15]);
	}

	pad[19] ^= my_cos (pad[14]) ? 0x4d552e : pad[10];

	if (branch == 0x0D)
	{
		minor_35;
		minor_51 (18);
		major_7 (pad, pad[2]);
	}

	pad[13] = ((pad[13] * 27) << 1) - pad[13];

	if (branch == 4)
	{
		TRY (minor_6 (0xD48681AE));
		minor_51 (18);
		major_8 (pad, pad[6]);
	}

	if (branch == 5)
	{
		TRY (minor_19);
		minor_29 (88);
		major_18 (pad, pad[16]);
	}

	pad[8] ^= pad[10] + 0x4d757e;
}

void major_14 (u32 *pad, u32 seed)
{
	u32 branch = (pad[12] ^ pad[2] ^ pad[19]) & 0x0F;

	if (branch == 0x0F)
	{
		TRY (minor_5);
		minor_26;
		major_6 (pad, pad[19]);
	}

	pad[3] += ((seed * 3) << 4) + seed;

	if (branch == 9)
	{
		TRY (minor_11 (0x73324afc));
		minor_69;
		major_1 (pad, seed);
	}

	if (branch == 3)
	{
		minor_53;
		minor_44;
		major_4 (pad, seed);
	}

	pad[9] -= pad[2] & 0x8742C2;

	if (branch == 6)
	{
		TRY (minor_21);
		minor_67 (0xE37DF00A);
		major_16 (pad, seed);
	}

	seed ^= pad[6] < 0x4c2ac4 ? pad[6] : pad[5];

	if (branch == 0x0C)
	{
		minor_58 (0xf8e7cbf0);
		minor_48;
		major_7 (pad, seed);
	}

	if (branch == 0x0E)
	{
		minor_57 (0xbe02f6b0);
		minor_69;
		major_8 (pad, seed);
	}

	pad[8] ^= my_sqrt (pad[17]);

	if (branch == 4)
	{
		TRY (minor_6 (0xCAD836AF));
		minor_25;
		major_18 (pad, pad[16]);
	}

	pad[1] += 0x904F9E8D - pad[5];

	if (branch == 2)
	{
		TRY (minor_5);
		TRY (minor_14);
		major_12 (pad, pad[2]);
	}

	if (branch == 0)
	{
		minor_33 (0xa29c6a97);
		minor_38;
		major_5 (pad, pad[17]);
	}

	pad[5] += pad[6] - 0x49781f;

	if (branch == 5)
	{
		TRY (minor_4);
		minor_62;
		major_15 (pad, pad[8]);
	}

	if (branch == 0x0D)
	{
		minor_43;
		TRY (minor_11 (0x9c1ee61f));
		major_2 (pad, seed);
	}

	pad[0] ^= pad[6] + 0x356DCF35;

	if (branch == 1)
	{
		TRY (minor_14);
		minor_29 (96);
		major_11 (pad, pad[0]);
	}

	pad[8] ^= ROL (pad[8], 0x1d);

	if (branch == 0x0B)
	{
		minor_62;
		minor_26;
		major_6 (pad, pad[9]);
	}

	if (branch == 8)
	{
		minor_42;
		minor_33 (0x78bca04e);
		major_1 (pad, seed);
	}

	pad[19] |= pad[4] * 73;

	if (branch == 0x0A)
	{
		minor_26;
		TRY (minor_5);
		major_4 (pad, pad[2]);
	}

	pad[11] += 0xb888d - pad[10];

	if (branch == 7)
	{
		minor_23;
		minor_26;
		major_16 (pad, seed);
	}

	if (branch == 0)
	{
		TRY (minor_16);
		TRY (minor_10 (22));
		major_7 (pad, pad[19]);
	}

	pad[17] ^= (pad[3] * 5) << 3;
}

void major_15 (u32 *pad, u32 seed)
{
	u32 branch = (pad[0] ^ pad[3] ^ seed) % 0x0D;

	if (branch == 0)
	{
		minor_40;
		minor_35;
		minor_74 (pad, pad[18]);
	}

	pad[18] *= seed * 25;

	if (branch == 1)
	{
		TRY (minor_18);
		minor_27;
		major_10 (pad, pad[19]);
	}

	pad[12] &= pad[8] ^ 0x3038E4EB;

	if (branch == 3)
	{
		minor_61;
		TRY (minor_4);
		major_20 (pad, pad[6]);
	}

	if (branch == 6)
	{
		TRY (minor_4);
		minor_26;
		major_6 (pad, pad[12]);
	}

	seed ^= ROR (pad[0], 0x9);

	if (branch == 2)
	{
		minor_62;
		minor_73;
		major_1 (pad, pad[0]);
	}

	pad[8] -= ROR (pad[6], 0x1c);

	if (branch == 0x0B)
	{
		TRY (minor_12);
		TRY (minor_16);
		major_4 (pad, pad[8]);
	}

	if (branch == 8)
	{
		minor_42;
		TRY (minor_9);
		major_16 (pad, seed);
	}

	pad[17] ^= pad[2] + 0x187F79E8;

	if (branch == 0)
	{
		minor_71;
		TRY (minor_18);
		major_7 (pad, pad[12]);
	}

	pad[13] ^= (seed * 25) << 2;

	if (branch == 9)
	{
		TRY (minor_12);
		minor_63;
		major_8 (pad, pad[3]);
	}

	if (branch == 0x0A)
	{
		TRY (minor_19);
		minor_58 (0x30360f98);
		major_18 (pad, pad[9]);
	}

	pad[8] = ROR (pad[8], pad[17] - 0x12c0bc1b);

	if (branch == 0x0C)
	{
		TRY (minor_9);
		TRY (minor_21);
		major_12 (pad, pad[18]);
	}

	pad[11] ^= pad[12] - 0x409402;

	if (branch == 7)
	{
		minor_53;
		minor_24;
		major_5 (pad, seed);
	}

	pad[4] = ROR (pad[4], pad[6] - 0x409402);

	if (branch == 5)
	{
		minor_44;
		minor_32 (0x52374450);
		minor_74 (pad, pad[19]);
	}

	if (branch == 4)
	{
		minor_58 (0x12d9e782);
		minor_66;
		major_10 (pad, pad[19]);
	}

	pad[3] ^= pad[6] - 0x400000;
}

void major_16 (u32 *pad, u32 seed)
{
	u32 branch = pad[5] % 0x07;

	pad[2] -= seed ^ 0x1E9E3A1C;

	if (branch == 4)
	{
		minor_38;
		minor_48;
		minor_75 (pad);
	}

	pad[9] &= pad[4] ^ 0x46b160;

	if (branch == 5)
	{
		minor_43;
		minor_62;
		minor_74 (pad, pad[11]);
	}

	seed += pad[17] ^ 0x46b160;

	if (branch == 1)
	{
		TRY (minor_6 (0xC6D7E2F7));
		minor_30;
		major_10 (pad, seed);
	}

	pad[13] |= my_sqrt (pad[19]);

	pad[7] -= my_cos (pad[10]) ? 0x90069b : pad[17];

	if (branch == 0)
	{
		TRY (minor_15);
		minor_45;
		major_20 (pad, pad[7]);
	}

	pad[15] -= 0x90069b | seed;

	if (branch == 6)
	{
		minor_39;
		minor_29 (184);
		major_6 (pad, pad[1]);
	}

	seed += pad[6] & 0x44782589;

	if (branch == 3)
	{
		minor_53;
		TRY (minor_15);
		major_1 (pad, seed);
	}

	pad[8] += pad[11] & 0x2645009B;

	if (branch == 0)
	{
		minor_29 (124);
		minor_64;
		major_4 (pad, pad[10]);
	}

	pad[8] *= pad[16] ^ 0x2666A4CB;
}

void major_17 (u32 *pad, u32 seed)
{
	u32 branch = (pad[2] ^ pad[6] ^ seed) & 0x0F;

	if (branch == 4)
	{
		minor_72 (170);
		minor_43;
		major_16 (pad, pad[6]);
	}

	if (branch == 0x0A)
	{
		minor_31;
		minor_72 (253);
		major_7 (pad, pad[0]);
	}

	pad[15] += ROR (pad[16], 0x5);

	if (branch == 2)
	{
		minor_49;
		minor_70;
		major_8 (pad, pad[4]);
	}

	if (branch == 9)
	{
		minor_57 (0xc481dafd);
		minor_50;
		major_18 (pad, seed);
	}

	seed = (pad[3] * seed * 19) << 1;

	if (branch == 8)
	{
		minor_39;
		minor_71;
		major_12 (pad, pad[8]);
	}

	if (branch == 3)
	{
		minor_58 (0x1bf5bb22);
		minor_62;
		major_5 (pad, pad[14]);
	}

	pad[11] *= seed + 0x19948E09;

	if (branch == 3)
	{
		minor_49;
		minor_73;
		major_15 (pad, seed);
	}

	if (branch == 0x0E)
	{
		TRY (minor_11 (0x43cf4936));
		TRY (minor_11 (0xcc3ff7ad));
		major_2 (pad, seed);
	}

	pad[2] = ROR (pad[2], seed - 0xc5ef5be);

	if (branch == 0)
	{
		minor_26;
		minor_24;
		major_11 (pad, seed);
	}

	if (branch == 0x0F)
	{
		minor_69;
		minor_61;
		major_14 (pad, seed);
	}

	pad[12] -= my_cos (seed) ? 0xcaba2d3 : pad[9];

	if (branch == 0x0B)
	{
		minor_56 (0x27BA9331);
		minor_29 (148);
		major_13 (pad, seed);
	}

	if (branch == 7)
	{
		minor_34;
		minor_52 (0x3a9e050c);
		major_19 (pad, pad[1]);
	}

	pad[17] -= pad[17] < 0xcaba2d3 ? pad[17] : pad[18];

	if (branch == 0)
	{
		minor_44;
		minor_48;
		major_16 (pad, pad[10]);
	}

	if (branch == 1)
	{
		minor_33 (0x6bd5eb17);
		minor_43;
		major_7 (pad, pad[17]);
	}

	pad[6] += pad[0] - 0xD6249F6;

	if (branch == 0x0C)
	{
		TRY (minor_21);
		TRY (minor_4);
		major_8 (pad, pad[6]);
	}

	if (branch == 1)
	{
		minor_70;
		minor_57 (0x63fd32db);
		major_18 (pad, seed);
	}

	pad[5] += ((pad[16] * 7) << 4) + pad[16];

	if (branch == 6)
	{
		minor_50;
		TRY (minor_17);
		major_12 (pad, pad[8]);
	}

	if (branch == 2)
	{
		TRY (minor_14);
		TRY (minor_20);
		major_5 (pad, pad[15]);
	}

	pad[11] |= pad[13] * 0x11;

	if (branch == 0x0D)
	{
		minor_32 (0x1CF92975);
		TRY (minor_13 (0x2F50247E));
		major_15 (pad, pad[11]);
	}

	if (branch == 5)
	{
		minor_57 (0xeb114770);
		minor_34;
		major_2 (pad, seed);
	}

	pad[10] += pad[10] ^ 0x53350A5E;
}

void major_18 (u32 *pad, u32 seed)
{
	u32 branch = (pad[15] ^ pad[1] ^ pad[18]) % 0x0A;

	pad[14] *= pad[18] - 0x3438A280;

	if (branch == 9)
	{
		TRY (minor_6 (0xDC6C3D21));
		minor_26;
		minor_75 (pad);
	}

	pad[2] += pad[3] & 0x244268df;

	if (branch == 4)
	{
		minor_66;
		minor_51 (23);
		minor_74 (pad, seed);
	}

	pad[18] *= ROL (seed, 0x1f);

	if (branch == 5)
	{
		minor_52 (0x1cb09c4d);
		TRY (minor_15);
		major_10 (pad, pad[14]);
	}

	pad[6] += pad[10] + 0x5308B1DB;

	if (branch == 1)
	{
		minor_39;
		minor_36;
		major_20 (pad, seed);
	}

	pad[5] += ROR (pad[8], 0x19);

	if (branch == 0)
	{
		TRY (minor_19);
		minor_34;
		major_6 (pad, pad[5]);
	}

	pad[9] |= seed & 0x27BE8251;

	if (branch == 2)
	{
		TRY (minor_6 (0x9DBE27AD));
		TRY (minor_12);
		major_1 (pad, pad[9]);
	}

	pad[12] ^= pad[2] * 73;

	if (branch == 8)
	{
		minor_59;
		minor_24;
		major_4 (pad, seed);
	}

	pad[14] &= seed - 0x7413C08;

	if (branch == 6)
	{
		minor_47;
		TRY (minor_13 (0x2725C3FB));
		major_16 (pad, pad[17]);
	}

	pad[9] += 0x4c3ada - seed;

	if (branch == 3)
	{
		minor_41;
		minor_49;
		major_7 (pad, seed);
	}

	seed += 0x8ea63 | pad[18];

	if (branch == 0)
	{
		minor_41;
		minor_61;
		major_8 (pad, pad[19]);
	}

	pad[3] += seed < 0x4efe6b ? seed : pad[4];
}

void major_19 (u32 *pad, u32 seed)
{
	u32 branch = (pad[11] ^ pad[6] ^ pad[19]) & 0x0F;

	if (branch == 7)
	{
		TRY (minor_8);
		minor_34;
		major_4 (pad, seed);
	}

	pad[18] = ROR (pad[18], pad[6] + 0x50bb3362);

	if (branch == 0x0D)
	{
		minor_26;
		TRY (minor_8);
		major_16 (pad, pad[14]);
	}

	if (branch == 0x0F)
	{
		TRY (minor_6 (0xCE7048A1));
		minor_53;
		major_7 (pad, pad[0]);
	}

	pad[6] -= my_cos (pad[14]) ? 0x50ff65ed : pad[9];

	if (branch == 2)
	{
		minor_57 (0x1530cb9a);
		minor_68;
		major_8 (pad, pad[13]);
	}

	if (branch == 8)
	{
		minor_26;
		minor_49;
		major_18 (pad, seed);
	}

	seed ^= ROR (seed, 0xd);

	if (branch == 0x0A)
	{
		minor_65 (48);
		minor_38;
		major_12 (pad, seed);
	}

	if (branch == 5)
	{
		minor_55 (0x1e8ee7f4);
		minor_45;
		major_5 (pad, pad[10]);
	}

	pad[17] ^= pad[4] | 0x50ffe7ed;

	if (branch == 3)
	{
		minor_30;
		minor_59;
		major_15 (pad, pad[17]);
	}

	if (branch == 0)
	{
		minor_25;
		TRY (minor_14);
		major_2 (pad, pad[8]);
	}

	seed ^= seed < 0x5149b5c9 ? seed : pad[5];

	if (branch == 0x0B)
	{
		minor_46;
		minor_58 (0x778d8c);
		major_11 (pad, seed);
	}

	if (branch == 2)
	{
		TRY (minor_9);
		TRY (minor_14);
		major_14 (pad, pad[1]);
	}

	pad[9] += pad[7] ^ 0x5149b5c9;

	if (branch == 0x0E)
	{
		minor_51 (1);
		minor_63;
		major_13 (pad, pad[6]);
	}

	if (branch == 1)
	{
		minor_71;
		minor_34;
		major_4 (pad, pad[19]);
	}

	pad[17] |= pad[10] - 0x514ffdff;

	if (branch == 6)
	{
		minor_62;
		minor_44;
		major_16 (pad, pad[8]);
	}

	if (branch == 9)
	{
		minor_58 (0xe0d7842a);
		TRY (minor_10 (4));
		major_7 (pad, pad[2]);
	}

	pad[6] ^= my_cos (pad[13]) ? 0xB29627F : seed;

	if (branch == 0x0C)
	{
		minor_70;
		minor_30;
		major_8 (pad, pad[5]);
	}

	if (branch == 0)
	{
		minor_59;
		TRY (minor_13 (0x664D4285));
		major_18 (pad, pad[14]);
	}

	if (branch == 4)
	{
		TRY (minor_13 (0x281B56A6));
		TRY (minor_18);
		major_12 (pad, pad[18]);
	}

	if (branch == 1)
	{
		minor_25;
		minor_24;
		major_5 (pad, pad[4]);
	}

	pad[16] *= pad[3] * 0x3a94d;
}

void major_20 (u32 *pad, u32 seed)
{
	u32 branch = (pad[13] ^ pad[8] ^ pad[17]) % 0x03;

	seed ^= 0x39aa3db4 ^ pad[17];

	pad[15] = ROL (pad[15], my_sin (seed) ? 0x236D13F3 : pad[19]);
	pad[16] ^= my_sqrt (seed);

	if (branch == 0)
	{
		minor_70;
		minor_66;
		minor_75 (pad);
	}

	seed = (seed * seed * 5) << 4;

	pad[18] |= pad[8] * 0x1E4B94EC;
	pad[3] += seed & 0x2708964F;

	if (branch == 0)
	{
		minor_45;
		minor_53;
		minor_74 (pad, seed);
	}

	pad[0] -= pad[10] | 0x390ac159;

	pad[8] -= my_cos (pad[6]) ? 0x84019 : pad[10];
	pad[17] &= 0x84019 | seed;

	pad[14] *= ROL (pad[2], 0x1f);

	if (branch == 1)
	{
		TRY (minor_18);
		minor_60;
		major_10 (pad, pad[7]);
	}

	pad[8] *= pad[15] - 0x51214;
}

void major_21 (u32 *pad, u32 seed)
{
	u32 branch = (pad[15] ^ pad[4] ^ pad[19]) & 0x0F;

	if (branch == 8)
	{
		minor_39;
		TRY (minor_17);
		major_8 (pad, pad[12]);
	}

	pad[16] += 0x19deda30 - pad[1];

	if (branch == 2)
	{
		minor_58 (0x7022d430);
		TRY (minor_3);
		major_18 (pad, seed);
	}

	if (branch == 3)
	{
		minor_31;
		minor_58 (0x7bb7cdf6);
		major_12 (pad, seed);
	}

	seed -= my_cos (pad[10]) ? 0x199972a0 : seed;

	if (branch == 4)
	{
		minor_30;
		minor_72 (150);
		major_5 (pad, seed);
	}

	if (branch == 9)
	{
		minor_59;
		TRY (minor_20);
		major_15 (pad, pad[16]);
	}

	pad[16] *= my_sqrt (pad[2]);

	if (branch == 0x0E)
	{
		minor_56 (0x103EC068);
		minor_42;
		major_2 (pad, pad[8]);
	}

	if (branch == 5)
	{
		minor_48;
		minor_34;
		major_11 (pad, pad[9]);
	}

	pad[18] -= my_sqrt (pad[2]);

	if (branch == 6)
	{
		minor_67 (0x903B6F4);
		minor_37;
		major_14 (pad, seed);
	}

	if (branch == 1)
	{
		minor_43;
		minor_73;
		major_13 (pad, pad[13]);
	}

	seed += pad[8] < 0x2B940916 ? pad[8] : seed;

	if (branch == 3)
	{
		minor_55 (0x22c0e0f0);
		TRY (minor_20);
		major_19 (pad, seed);
	}

	if (branch == 2)
	{
		minor_29 (48);
		TRY (minor_18);
		major_17 (pad, pad[5]);
	}

	seed *= pad[5] + 0x1D13557B;

	if (branch == 0x0B)
	{
		TRY (minor_6 (0xF4F5ADCC));
		minor_70;
		major_3 (pad, pad[17]);
	}

	if (branch == 0x0A)
	{
		minor_68;
		TRY (minor_4);
		major_8 (pad, seed);
	}

	pad[0] -= my_sqrt (seed);

	if (branch == 0x0D)
	{
		minor_35;
		TRY (minor_7);
		major_18 (pad, pad[17]);
	}

	if (branch == 0)
	{
		TRY (minor_20);
		TRY (minor_17);
		major_12 (pad, pad[19]);
	}

	pad[18] += 0xffb953f8 + seed;

	if (branch == 0x0F)
	{
		TRY (minor_7);
		minor_69;
		major_5 (pad, seed);
	}

	if (branch == 1)
	{
		TRY (minor_19);
		minor_47;
		major_15 (pad, pad[8]);
	}

	pad[3] ^= my_cos (seed) ? 0xffb953f8 : pad[4];

	if (branch == 7)
	{
		minor_30;
		minor_28;
		major_2 (pad, pad[1]);
	}

	if (branch == 0)
	{
		minor_28;
		minor_70;
		major_11 (pad, pad[8]);
	}

	pad[1] ^= pad[3] * 0x620312F0;

	if (branch == 0x0C)
	{
		minor_63;
		minor_65 (211);
		major_14 (pad, pad[12]);
	}

	pad[7] ^= my_sqrt (pad[10]);

	if (branch == 4)
	{
		minor_38;
		minor_60;
		major_13 (pad, pad[4]);
	}

	if (branch == 5)
	{
		minor_44;
		minor_23;
		major_19 (pad, seed);
	}

	pad[18] ^= pad[14] + 0x9AD3708;
}

void major_0 (u32 *pad, u32 seed)
{
	u32 branch = (pad[7] ^ pad[11] ^ pad[18]) & 0x0F;

	if (branch == 0x0E)
	{
		minor_33 (0xc13e99ce);
		minor_34;
		major_12 (pad, seed);
	}

	if (branch == 9)
	{
		minor_41;
		minor_70;
		major_5 (pad, pad[7]);
	}

	pad[11] += 0 - (((pad[18] * 5) << 4) - pad[18]);

	if (branch == 0x0C)
	{
		minor_43;
		minor_27;
		major_15 (pad, pad[2]);
	}

	if (branch == 3)
	{
		minor_29 (168);
		TRY (minor_20);
		major_2 (pad, pad[7]);
	}

	if (branch == 0x0B)
	{
		minor_70;
		minor_35;
		major_11 (pad, pad[17]);
	}

	pad[19] ^= ROR (pad[12], 0x1d);

	if (branch == 4)
	{
		TRY (minor_8);
		minor_37;
		major_14 (pad, pad[1]);
	}

	if (branch == 2)
	{
		TRY (minor_16);
		TRY (minor_21);
		major_13 (pad, pad[2]);
	}

	seed ^= pad[14] - 0x12821;

	if (branch == 4)
	{
		minor_33 (0x628d876);
		minor_42;
		major_19 (pad, seed);
	}

	if (branch == 1)
	{
		TRY (minor_12);
		minor_28;
		major_17 (pad, seed);
	}

	if (branch == 0)
	{
		minor_48;
		minor_62;
		major_3 (pad, pad[11]);
	}

	pad[4] |= 0x472ca7 ^ pad[4];	// 0x10468B00

	if (branch == 0x0D)
	{
		minor_46;
		TRY (minor_12);
		major_21 (pad, pad[13]);
	}

	if (branch == 0x0F)
	{
		minor_73;
		minor_43;
		major_9 (pad, pad[13]);
	}

	pad[13] -= pad[16] < 0x97CBA53 ? pad[16] : seed;

	if (branch == 2)
	{
		minor_24;
		TRY (minor_10 (22));
		major_12 (pad, pad[17]);
	}

	if (branch == 6)
	{
		TRY (minor_20);
		TRY (minor_3);
		major_5 (pad, pad[15]);
	}

	if (branch == 7)
	{
		minor_72 (107);
		minor_50;
		major_15 (pad, pad[8]);
	}

	pad[10] ^= ((pad[2] << 5) - pad[2]) << 1;

	if (branch == 6)
	{
		TRY (minor_11 (0x3f675c4e));
		minor_41;
		major_2 (pad, pad[16]);
	}

	if (branch == 5)
	{
		minor_49;
		minor_56 (0x87C73F);
		major_11 (pad, pad[15]);
	}

	pad[13] &= pad[9] ^ 0x473fe6;

	if (branch == 7)
	{
		minor_62;
		minor_26;
		major_14 (pad, seed);
	}

	if (branch == 1)
	{
		TRY (minor_3);
		TRY (minor_15);
		major_13 (pad, pad[5]);
	}

	if (branch == 0)
	{
		minor_25;
		minor_30;
		major_19 (pad, pad[17]);
	}

	pad[1] -= ROL (pad[7], 0xc);

	if (branch == 8)
	{
		minor_65 (169);
		minor_27;
		major_17 (pad, pad[2]);
	}

	if (branch == 0x0A)
	{
		minor_67 (0xF611CB26);
		minor_29 (4);
		major_3 (pad, pad[13]);
	}

	pad[10] -= pad[0] < 0x6c6c ? pad[0] : pad[5];

	if (branch == 3)
	{
		minor_33 (0x58efea36);
		minor_49;
		major_21 (pad, pad[0]);
	}

	if (branch == 5)
	{
		minor_27;
		minor_38;
		major_9 (pad, pad[16]);
	}

	pad[5] += ((seed << 5) + seed) * 3;
}
