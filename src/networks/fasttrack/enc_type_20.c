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
 * This is the most recent pad mingling code for FastTrack as of 03/04/28
 * Used for encryption version 0x20
 */

typedef unsigned int u32;

/* our crude SEH replacement */

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

#define ROR(value, count) ((value) >> ((count) & 0x1f) | ((value) << (32 - (((count) & 0x1f)))))
#define ROL(value, count) ((value) << ((count) & 0x1f) | ((value) >> (32 - (((count) & 0x1f)))))

#define ROREQ(value, count) value = ROR(value, count)
#define ROLEQ(value, count) value = ROL(value, count)

/* the entry point of this mess */
/* this all works on unsigned ints so endianess is not an issue */

void mix (u32 *pad, u32 seed);

void enc_type_20 (u32 *pad, u32 seed)
{
	mix (pad, seed);
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
static void major_21 (u32 *pad, u32 seed);
static void major_22 (u32 *pad, u32 seed);
static void major_23 (u32 *pad, u32 seed);
static void major_24 (u32 *pad, u32 seed);
static void major_25 (u32 *pad, u32 seed);

/* simple pad manipulation functions */

static void minor_36 (u32 *pad);
static void minor_37 (u32 *pad);


/* minor_ implementation details below this line ;) */

#define minor_1(x) pad[8] += my_sin(x&255) ? 0x4f0cf8d : x
#define minor_2(x) pad[2] += pad[2] < 0x36def3e1 ? pad[2] : x
#define minor_3 pad[10] ^= ROL(pad[1], 20)
#define minor_4 pad[16] -= pad[6]
#define minor_5 pad[10] -= pad[9] * 0x55
#define minor_6 ROLEQ(pad[0], pad[19] ^ 0xc)
#define minor_7 pad[17] += pad[8] * 0xf6084c92
#define minor_8 pad[12] ^= pad[10] & 0x28acec82
#define minor_9(x) pad[12] *= pad[12] < 0x12d7bed ? pad[12] : x
#define minor_10(x) pad[18] += pad[5] < 0xfd0aa3f ? pad[5] : x
#define minor_12(x) pad[11] &= my_cos(pad[18]) ? 0x146a49cc : x
#define minor_13 pad[2] &= my_cos(pad[2]) ? 0x7ebbfde : pad[11]
#define minor_17 pad[19] ^= pad[7] * 0x3a
#define minor_19 ROLEQ(pad[6], ROR(pad[8], 14))
#define minor_20 pad[0] &= ROR(pad[18], 31)
#define minor_22 ROREQ(pad[3], pad[11] ^ 0x7)
#define minor_26 pad[0] |= my_cos(pad[1]) ? 0x56e0e99 : pad[8]
#define minor_27 pad[18] += my_cos(pad[15]) ? 0x10d11d00 : pad[9]
#define minor_28 pad[10] -= my_cos(pad[15]) ? 0x268cca84 : pad[9]
#define minor_29 pad[3] -= my_cos(pad[6]) ? 0x2031618a : pad[8]
#define minor_30 ROLEQ(pad[1], my_sin(pad[5]) ? 4 : pad[6])
#define minor_31(x) ROREQ(pad[17], my_sin(pad[6]) ? 29 : x)
#define minor_32(x) pad[15] ^= my_sin(pad[14]) ? 0x40a33fd4 : x
#define minor_34 pad[7] ^= my_sqrt(pad[11])
#define minor_35 pad[5] += my_sqrt(pad[7])

void minor_36 (u32 *pad)
{
	pad[3] ^= pad[11] * 0xeef27425;
	pad[3] += my_sqrt (pad[0]);
	pad[15] *= pad[1] ^ 0xd89b4a;
	ROREQ (pad[16], pad[16] & 0x11);
	pad[18] *= pad[19] + 0xa0d8c0cf;
	pad[7] *= pad[0] < 0x6765080e ? pad[0] : pad[18];

	if (pad[5] < 0xe848f43c)
		ROLEQ (pad[9], pad[5]);

	pad[2] ^= pad[5] < 0xa0d8c0cf ? pad[5] : pad[9] - 0xe848f43c;
	ROLEQ (pad[12], ROL (pad[9] - 0xe848f43c, 11));
}

void minor_37 (u32 *pad)
{
	ROLEQ (pad[2], pad[7] + 0x1b);
	pad[2] ^= pad[9] * 0x7941955;
	pad[2] -= 0x796fa0af;
	pad[3] *= my_sin (pad[19]) ? 0x5ea67f83 : pad[5];
	pad[4] -= pad[4] ^ 0x692c9ef9;
	pad[10] += pad[1] ^ 0xc43baf0b;
	pad[12] *= pad[7] - 0xc43baf0b;
	pad[13] ^= 0xd;
	pad[17] ^= pad[17] - 0x1259dbb;
	ROREQ (pad[17], 10);
	pad[18] += pad[0] ^ 0x3cf1856;
}

void major_1 (u32 *pad, u32 seed)
{
	int branch = (pad[17] ^ pad[4] ^ pad[13]) % 13;

	if (branch == 9)
	{
		pad[7] |= 0x3e73450d;
		minor_31 (0x9);
		minor_36 (pad);
	}

	pad[11] &= pad[19] & 0x170b54ed;

	if (branch == 10)
	{
		pad[12] ^= pad[15] - 0xf5cfde0;
		minor_27;
		major_23 (pad, pad[8]);
	}

	ROREQ (pad[1], pad[14] < 0x164d8d96 ? pad[14] : pad[4]);

	if (branch == 12)
	{
		TRY (minor_1 (0xc0948cf0));
		minor_28;
		major_24 (pad, pad[18]);
	}

	if (branch == 0)
	{
		pad[12] ^= pad[15] - 0xf5cfde0;
		minor_31 (0x15);
		major_19 (pad, pad[12]);
	}

	ROLEQ (pad[6], pad[13] ^ 0x2);

	if (branch == 6)
	{
		TRY (ROREQ (pad[1], 0x4));
		pad[9] ^= pad[7] * 0x44;
		major_25 (pad, seed);
	}

	if (branch == 3)
	{
		pad[2] ^= my_sin (pad[13]) ? 0xfd08092 : pad[10];
		pad[13] *= ROR (pad[3], 5);
		major_17 (pad, pad[15]);
	}

	if (branch == 0)
	{
		minor_19;
		pad[13] *= ROR (pad[3], 5);
		major_4 (pad, pad[8]);
	}

	seed += my_sin (seed) ? 0x160df35d : seed;
	seed &= pad[19] | 0xe00682c6;

	if (branch == 1)
	{
		pad[7] &= pad[13] ^ 0x21aaf758;
		pad[9] |= pad[7] ^ 0x2a19119f;
		major_18 (pad, pad[12]);
	}

	pad[16] += my_sin (seed) ? 0xe00682c6 : pad[7];

	if (branch == 2)
	{
		pad[13] += pad[15] < 0x137bffeb ? pad[15] : pad[11];
		pad[19] ^= my_cos (pad[9]) ? 0x57337b8 : pad[14];
		major_15 (pad, seed);
	}

	if (branch == 7)
	{
		pad[4] -= pad[17] ^ 0x2217cf47;
		pad[13] *= ROR (pad[3], 5);
		major_3 (pad, pad[14]);
	}

	if (branch == 4)
	{
		pad[19] ^= my_cos (pad[9]) ? 0x57337b8 : pad[14];
		minor_34;
		major_21 (pad, pad[18]);
	}

	pad[5] *= my_sqrt (pad[9]);

	if (branch == 11)
	{
		ROREQ (pad[4], 0x2);
		minor_32 (0x8517ae30);
		major_16 (pad, pad[4]);
	}

	pad[13] &= pad[18] - 0xeb6dee4;

	if (branch == 5)
	{
		minor_30;
		TRY (minor_3);
		minor_36 (pad);
	}

	if (branch == 8)
	{
		pad[7] |= 0x7de964ed;
		TRY (minor_4);
		major_23 (pad, pad[3]);
	}
}

void major_2 (u32 *pad, u32 seed)
{
	int branch = pad[10] & 15;

	if (branch == 5)
	{
		minor_27;
		pad[7] &= pad[13] ^ 0x21aaf758;
		major_25 (pad, pad[0]);
	}

	pad[0] -= seed * 0x36;

	if (branch == 13)
	{
		pad[2] *= pad[3] + 0xd6863a6;
		pad[6] += 0xfe07af0e - pad[3];
		major_17 (pad, seed);
	}

	if (branch == 12)
	{
		pad[6] ^= 0x9374c368;
		pad[7] &= 0xc45b99ee;
		major_4 (pad, pad[14]);
	}

	pad[7] -= pad[8] | 0x1a1a9407;

	if (branch == 6)
	{
		ROREQ (pad[9], 12);
		pad[3] -= pad[0] ^ 0x185f3b0d;
		major_18 (pad, pad[14]);
	}

	pad[2] += pad[0] + 0x19259d5;

	if (branch == 8)
	{
		pad[9] ^= pad[7] * 0x44;
		pad[2] ^= pad[15] << 5;
		major_15 (pad, seed);
	}

	if (branch == 11)
	{
		minor_34;
		pad[3] -= pad[0] ^ 0x185f3b0d;
		major_3 (pad, pad[15]);
	}

	pad[16] &= seed - 0x1badcb5;

	if (branch == 15)
	{
		pad[14] |= pad[3] ^ 0x4345732;
		ROREQ (pad[4], 0x1);
		major_21 (pad, pad[3]);
	}

	pad[5] -= my_cos (pad[4]) ? 0xffcdb92f : pad[14];

	if (branch == 1)
	{
		pad[13] -= pad[1];
		pad[7] |= 0x45e184c5;
		major_16 (pad, pad[9]);
		TRY (minor_1 (0x149a97a0));
		TRY (minor_10 (0xd87d888e));
		major_1 (pad, pad[9]);
	}

	pad[5] *= pad[8] + 0xffcdb92f;

	if (branch == 4)
	{
		TRY (minor_10 (0x130aa218));
		pad[13] *= ROR (pad[3], 5);
		major_14 (pad, pad[6]);
	}

	ROLEQ (pad[1], pad[15] < 0xbdc3f45b ? pad[15] : pad[9]);

	if (branch == 14)
	{
		minor_30;
		pad[13] *= 0x7f0d5ead;
		major_6 (pad, pad[5]);
	}

	if (branch == 0)
	{
		pad[6] += pad[19] - 0x3f5675d6;
		TRY (minor_5);
		major_9 (pad, seed);
	}

	pad[6] += pad[3] * 0x79;

	if (branch == 9)
	{
		pad[9] &= 0x3eb4ed97;
		minor_30;
		major_25 (pad, pad[6]);
	}

	pad[16] ^= my_cos (pad[7]) ? 0x2d36f243 : pad[13];

	if (branch == 0)
	{
		pad[0] += pad[18] ^ 0x4ac16b8d;
		minor_28;
		major_17 (pad, pad[2]);
	}

	if (branch == 7)
	{
		pad[10] += 0x8958821;
		TRY (minor_1 (0x115e64d4));
		major_4 (pad, pad[19]);
	}

	pad[14] &= pad[3] ^ 0xb8eb772d;

	if (branch == 10)
	{
		pad[13] -= pad[1];
		pad[2] ^= pad[15] << 5;
		major_18 (pad, pad[8]);
	}

	ROREQ (pad[1], pad[12] * 0x5);

	if (branch == 3)
	{
		pad[2] ^= my_sin (pad[13]) ? 0xfd08092 : pad[10];
		pad[2] ^= my_sin (pad[13]) ? 0xfd08092 : pad[10];
		major_15 (pad, pad[15]);
	}

	if (branch == 2)
	{
		pad[7] &= 0x5cf54b9a;
		pad[13] *= 0xa02fe00;
		major_3 (pad, pad[14]);
	}

	pad[12] ^= my_sin (pad[0]) ? 0x96d5a5a4 : pad[5];
}

void major_3 (u32 *pad, u32 seed)
{
	int branch = (pad[5] ^ seed ^ pad[12]) % 10;

	seed *= pad[6] | 0x4723b25;

	if (branch == 0)
	{
		minor_22;
		TRY (minor_5);
		minor_37 (pad);
	}

	pad[2] -= pad[4] * 0xd;

	if (branch == 5)
	{
		pad[7] ^= 0x414517ea;
		minor_29;
		minor_36 (pad);
	}

	seed += pad[12] * 0x19;

	if (branch == 1)
	{
		pad[13] += pad[15] < 0x137bffeb ? pad[15] : pad[11];
		minor_19;
		major_23 (pad, seed);
	}

	seed += pad[7] + 0xbd42ff0;

	if (branch == 2)
	{
		minor_29;
		pad[16] += 0x1f5b0c59;
		major_24 (pad, seed);
	}

	pad[15] -= pad[0] ^ 0x16bee8c4;

	if (branch == 4)
	{
		TRY (minor_7);
		minor_28;
		major_19 (pad, seed);
	}

	pad[18] ^= pad[11] + 0x9fd1847f;

	if (branch == 6)
	{
		pad[6] += pad[19] - 0x3f5675d6;
		pad[6] += pad[19] - 0x3f5675d6;
		major_25 (pad, seed);
	}

	ROLEQ (pad[14], pad[19]);

	if (branch == 8)
	{
		minor_30;
		pad[12] += pad[6] + 0x21d7bf61;
		major_17 (pad, seed);
	}

	ROREQ (pad[0], pad[13] * 0x13);

	if (branch == 9)
	{
		TRY (minor_2 (0x70da1d6f));
		minor_29;
		major_4 (pad, seed);
	}

	if (branch == 7)
	{
		minor_22;
		TRY (minor_3);
		major_18 (pad, pad[5]);
	}

	if (branch == 3)
	{
		minor_17;
		pad[2] ^= pad[15] << 5;
		major_15 (pad, pad[19]);
	}
}

void major_4 (u32 *pad, u32 seed)
{
	int branch = pad[6] % 7;

	seed ^= ROL (pad[3], 18);

	if (branch == 6)
	{
		pad[6] += pad[19] - 0x3f5675d6;
		TRY (minor_5);
		minor_37 (pad);
	}

	pad[15] += seed * 0x32;
	pad[5] += 0xc93495e4 - pad[14];

	if (branch == 2)
	{
		TRY (minor_10 (0x10db4a9d));
		pad[6] += 0xfe07af0e - pad[3];
		minor_36 (pad);
	}

	pad[12] *= my_cos (pad[14]) ? 0xf5a79f2a : pad[17];

	if (branch == 0)
	{
		minor_17;
		pad[9] |= pad[7] ^ 0x2a19119f;
		major_23 (pad, pad[8]);
	}

	pad[6] &= pad[7] | 0xe02b5b1a;
	pad[11] ^= my_cos (pad[0]) ? 0x3a2c762b : seed;

	if (branch == 4)
	{
		TRY (minor_3);
		TRY (ROREQ (pad[1], 0x1c));
		major_24 (pad, seed);
	}

	pad[3] -= my_sqrt (pad[9]);

	if (branch == 5)
	{
		pad[6] ^= 0x47a791f;
		pad[0] += pad[18] ^ 0x4ac16b8d;
		major_19 (pad, pad[18]);
	}

	seed &= my_cos (pad[7]) ? 0xcdef2bf0 : pad[3];
	pad[0] -= pad[15] * 0x43;

	if (branch == 1)
	{
		minor_19;
		pad[6] ^= 0x424d4b7d;
		major_25 (pad, pad[3]);
	}

	pad[1] -= ROR (pad[18], 19);
	pad[17] ^= my_sin (pad[14]) ? 0x69eaf2fd : pad[16];

	if (branch == 0)
	{
		pad[3] -= pad[0] ^ 0x185f3b0d;
		pad[2] *= pad[3] + 0xd6863a6;
		major_17 (pad, pad[14]);
	}
}

void major_5 (u32 *pad, u32 seed)
{
	int branch = (pad[13] ^ pad[6] ^ pad[16]) & 15;

	if (branch == 7)
	{
		minor_20;
		pad[9] += ROL (pad[4], 9);
		major_17 (pad, pad[15]);
	}

	pad[2] ^= pad[15] - 0xe09f62af;

	if (branch == 15)
	{
		minor_31 (0x7);
		pad[9] |= pad[7] ^ 0x2a19119f;
		major_4 (pad, pad[10]);
	}

	if (branch == 14)
	{
		pad[9] ^= 0x19b844e;
		pad[5] -= pad[15];
		major_18 (pad, seed);
	}

	pad[5] += pad[8] * 0x49;

	if (branch == 4)
	{
		pad[14] |= pad[3] ^ 0x4345732;
		TRY (minor_8);
		major_15 (pad, pad[19]);
	}

	if (branch == 1)
	{
		minor_27;
		minor_17;
		major_3 (pad, pad[4]);
	}

	seed += pad[16] < 0x4dfe57f8 ? pad[16] : pad[17];

	if (branch == 2)
	{
		pad[13] *= 0x7ae310dc;
		pad[12] ^= pad[15] - 0xf5cfde0;
		major_21 (pad, pad[11]);
		pad[13] *= ROR (pad[3], 5);
		minor_20;
		major_16 (pad, pad[10]);
	}

	pad[5] += pad[6] + 0xd5c1b299;

	if (branch == 10)
	{
		pad[19] ^= my_cos (pad[9]) ? 0x57337b8 : pad[14];
		pad[9] |= pad[7] ^ 0x2a19119f;
		major_1 (pad, pad[10]);
	}

	if (branch == 12)
	{
		pad[16] += 0x203fdf50;
		minor_20;
		major_14 (pad, pad[8]);
	}

	pad[1] += my_sin (seed) ? 0xbabd3794 : seed;

	if (branch == 6)
	{
		pad[4] ^= 0xca8e79ab;
		pad[13] -= pad[1];
		major_6 (pad, pad[14]);
	}

	if (branch == 3)
	{
		pad[13] += pad[15] < 0x137bffeb ? pad[15] : pad[11];
		pad[2] ^= pad[15] << 5;
		major_9 (pad, seed);
	}

	pad[3] |= my_cos (pad[3]) ? 0xbabd3794 : pad[17];

	if (branch == 9)
	{
		minor_17;
		pad[5] -= pad[15];
		major_2 (pad, seed);
	}

	if (branch == 13)
	{
		ROREQ (pad[4], 0x2);
		pad[4] -= pad[17] ^ 0x2217cf47;
		major_17 (pad, seed);
	}

	seed ^= pad[2] * 0xb25bcc4d;

	if (branch == 11)
	{
		pad[19] += 0x12b9e29d - pad[12];
		pad[7] &= pad[13] ^ 0x21aaf758;
		major_4 (pad, pad[17]);
	}

	if (branch == 0)
	{
		pad[12] ^= pad[15] - 0xf5cfde0;
		minor_31 (0x7);
		major_18 (pad, seed);
	}

	pad[8] += 0xf1030e9c - pad[12];

	if (branch == 5)
	{
		pad[6] ^= 0xea99e155;
		pad[19] += 0x12b9e29d - pad[12];
		major_15 (pad, seed);
	}

	if (branch == 8)
	{
		pad[7] &= 0x710c48e8;
		pad[2] *= pad[3] + 0xd6863a6;
		major_3 (pad, pad[17]);
	}

	pad[15] += 0xa8f341c7 - pad[1];

	if (branch == 0)
	{
		pad[2] *= pad[3] + 0xd6863a6;
		pad[3] -= pad[0] ^ 0x185f3b0d;
		major_21 (pad, seed);
	}

	if (branch == 1)
	{
		TRY (minor_3);
		pad[12] *= pad[12];
		major_16 (pad, pad[12]);
	}

	pad[6] *= pad[5] * 0x1d;
}

void major_6 (u32 *pad, u32 seed)
{
	int branch = pad[17] % 15;

	if (branch == 0)
	{
		pad[13] += pad[15] < 0x137bffeb ? pad[15] : pad[11];
		pad[13] *= 0x22dd951f;
		major_24 (pad, pad[8]);
	}

	pad[11] -= my_sin (pad[9]) ? 0xe205322c : pad[7];

	if (branch == 13)
	{
		TRY (ROREQ (pad[1], 0x4));
		pad[12] ^= pad[15] - 0xf5cfde0;
		major_19 (pad, pad[0]);
	}

	pad[10] -= pad[6] ^ 0x1289de2;

	if (branch == 8)
	{
		ROREQ (pad[9], 10);
		TRY (minor_13);
		major_25 (pad, pad[4]);
	}

	if (branch == 5)
	{
		pad[13] *= 0x6a94c749;
		pad[18] -= pad[13] ^ 0x154abcdf;
		major_17 (pad, seed);
	}

	ROLEQ (pad[16], my_sqrt (pad[17]));

	if (branch == 2)
	{
		pad[16] += 0x3f147441;
		major_4 (pad, pad[16]);
	}

	pad[9] += my_sqrt (pad[3]);

	if (branch == 14)
	{
		ROREQ (pad[9], 15);
		pad[13] -= pad[1];
		major_18 (pad, seed);
	}

	seed = pad[6] ^ seed ^ 0x202ab323;

	if (branch == 9)
	{
		pad[5] += pad[0] ^ 0x3e17add3;
		pad[4] -= pad[17] ^ 0x2217cf47;
		major_15 (pad, pad[8]);
	}

	if (branch == 6)
	{
		pad[2] *= pad[3] + 0xd6863a6;
		pad[6] += pad[19] - 0x3f5675d6;
		major_3 (pad, pad[16]);
	}

	pad[15] ^= my_sqrt (pad[10]);

	if (branch == 1)
	{
		TRY (minor_2 (0xb30d40d0));
		pad[10] *= pad[10] - 0x5eae6bf;
		major_21 (pad, pad[13]);
	}

	pad[0] -= pad[11] ^ 0x1284af29;

	if (branch == 4)
	{
		pad[5] += pad[0] ^ 0x3e17add3;
		minor_29;
		major_16 (pad, pad[17]);
	}

	ROLEQ (seed, pad[11] * 0x10);

	if (branch == 11)
	{
		pad[9] ^= 0x1d8f33a6;
		TRY (minor_9 (0x13ee15c3));
		major_1 (pad, pad[19]);
	}

	if (branch == 0)
	{
		TRY (minor_3);
		pad[19] ^= my_cos (pad[9]) ? 0x57337b8 : pad[14];
		major_14 (pad, pad[16]);
	}

	pad[9] |= pad[9] ^ 0x2ad7629;

	if (branch == 10)
	{
		TRY (ROREQ (pad[1], 0xc));
		TRY (minor_9 (0xe8869877));
		major_24 (pad, seed);
	}

	pad[4] *= pad[12] * 0x4a237369;

	if (branch == 12)
	{
		pad[9] += ROL (pad[4], 9);
		TRY (minor_7);
		major_19 (pad, pad[5]);
	}

	if (branch == 7)
	{
		pad[14] |= pad[3] ^ 0x4345732;
		TRY (minor_9 (0xdd1ca541));
		major_25 (pad, pad[1]);
	}

	seed *= pad[4] + 0x76e5a087;

	if (branch == 3)
	{
		TRY (minor_5);
		TRY (minor_1 (0x62f4d3c4));
		major_17 (pad, seed);
	}
}

void major_7 (u32 *pad, u32 seed)
{
	int branch = (pad[10] ^ pad[11] ^ pad[18]) & 15;

	if (branch == 3)
	{
		minor_27;
		TRY (minor_2 (0x54bcde17));
		major_1 (pad, pad[14]);
	}

	if (branch == 9)
	{
		pad[7] &= pad[13] ^ 0x21aaf758;
		pad[13] -= pad[1];
		major_14 (pad, pad[12]);
	}

	pad[8] |= 0xc6ef5e80 + pad[1];

	if (branch == 2)
	{
		pad[6] += pad[19] - 0x3f5675d6;
		pad[6] += 0xfe07af0e - pad[3];
		major_6 (pad, pad[16]);
	}

	if (branch == 5)
	{
		pad[5] -= pad[15];
		pad[16] += 0x3fa3dc2f;
		major_9 (pad, pad[5]);
	}

	if (branch == 1)
	{
		pad[13] += pad[15] < 0x137bffeb ? pad[15] : pad[11];
		pad[14] |= pad[3] ^ 0x4345732;
		major_2 (pad, pad[3]);
	}

	pad[15] -= 0xc6ef5e80 + pad[19];

	if (branch == 4)
	{
		pad[2] ^= pad[15] << 5;
		minor_28;
		major_5 (pad, pad[7]);
	}

	if (branch == 1)
	{
		pad[0] += pad[6] * 0x3c;
		minor_31 (0x15);
		major_12 (pad, pad[16]);
	}

	if (branch == 9)
	{
		pad[5] += pad[0] ^ 0x3e17add3;
		pad[9] &= 0x4bd89b02;
		major_11 (pad, pad[19]);
	}

	seed -= pad[0] ^ 0x3b61016b;

	if (branch == 4)
	{
		pad[5] -= pad[15];
		TRY (minor_9 (0x1984a749));
		major_13 (pad, pad[3]);
	}

	if (branch == 3)
	{
		pad[9] += ROL (pad[4], 9);
		minor_28;
		major_22 (pad, pad[4]);
	}

	if (branch == 7)
	{
		TRY (minor_3);
		minor_31 (0xd);
		major_8 (pad, pad[5]);
	}

	ROLEQ (pad[11], pad[10] ^ 0x1a);

	if (branch == 8)
	{
		pad[18] -= pad[13] ^ 0x154abcdf;
		pad[19] += 0x12b9e29d - pad[12];
		major_10 (pad, seed);
	}

	if (branch == 14)
	{
		pad[19] += 0x12b9e29d - pad[12];
		ROREQ (pad[9], 3);
		major_1 (pad, seed);
	}

	seed -= pad[14] * 0xc02e189f;

	if (branch == 0)
	{
		pad[6] ^= 0x94eaa20d;
		minor_27;
		major_14 (pad, pad[6]);
	}

	if (branch == 13)
	{
		pad[12] += 0x2ac57dfa;
		pad[2] ^= pad[15] << 5;
		major_6 (pad, pad[4]);
	}

	if (branch == 8)
	{
		pad[18] *= pad[10] + 0x466e09cf;
		pad[13] -= pad[1];
		major_9 (pad, seed);
	}

	pad[4] += 0xa207344d - seed;

	if (branch == 12)
	{
		TRY (minor_2 (0x80a1da17));
		pad[13] *= 0xa02fe00;
		major_2 (pad, pad[3]);
	}

	if (branch == 2)
	{
		pad[13] *= 0x6aa5cc8c;
		pad[6] ^= 0xaefb322;
		major_5 (pad, pad[9]);
	}

	if (branch == 0)
	{
		minor_31 (0xb);
		pad[5] += pad[0] ^ 0x3e17add3;
		major_12 (pad, pad[5]);
	}

	seed ^= pad[18] ^ 0xe6830c9;

	if (branch == 6)
	{
		pad[9] |= pad[7] ^ 0x2a19119f;
		pad[7] &= pad[13] ^ 0x21aaf758;
		major_11 (pad, seed);
	}

	if (branch == 7)
	{
		pad[3] -= pad[0] ^ 0x185f3b0d;
		TRY (minor_2 (0xb11da063));
		major_13 (pad, pad[3]);
	}

	if (branch == 15)
	{
		minor_34;
		pad[4] ^= 0x41e634f6;
		major_22 (pad, pad[17]);
	}

	pad[0] ^= my_sin (seed) ? 0x8b50cd51 : pad[8];

	if (branch == 11)
	{
		minor_28;
		pad[2] ^= pad[15] << 5;
		major_8 (pad, seed);
	}

	if (branch == 5)
	{
		pad[9] += ROL (pad[4], 9);
		pad[7] &= pad[13] ^ 0x21aaf758;
		major_10 (pad, pad[16]);
	}

	seed += pad[1] * 0x3e;

	if (branch == 6)
	{
		minor_34;
		ROREQ (pad[4], 0x9);
		major_1 (pad, pad[12]);
	}

	if (branch == 10)
	{
		minor_22;
		pad[12] ^= pad[15] - 0xf5cfde0;
		major_14 (pad, seed);
	}

	pad[1] ^= pad[2] & 0x3b627777;
}

void major_8 (u32 *pad, u32 seed)
{
	int branch = (pad[2] ^ seed ^ pad[17]) & 15;

	if (branch == 7)
	{
		pad[13] -= pad[1];
		minor_26;
		major_21 (pad, seed);
	}

	if (branch == 0)
	{
		minor_35;
		TRY (minor_2 (0xe0b52e33));
		major_16 (pad, pad[15]);
	}

	seed -= ROR (pad[2], 6);

	if (branch == 5)
	{
		pad[4] ^= 0x9aa940f;
		pad[5] += pad[0] ^ 0x3e17add3;
		major_1 (pad, pad[12]);
	}

	if (branch == 1)
	{
		minor_22;
		minor_17;
		major_14 (pad, pad[1]);
	}

	pad[12] -= pad[17] * 0x74;

	if (branch == 1)
	{
		pad[12] += pad[6] + 0x21d7bf61;
		pad[13] *= ROR (pad[3], 5);
		major_6 (pad, pad[5]);
	}

	if (branch == 14)
	{
		pad[4] ^= 0x91ac407e;
		TRY (minor_7);
		major_9 (pad, seed);
	}

	pad[3] ^= pad[7] + 0x137c9f7d;

	if (branch == 0)
	{
		pad[7] &= pad[13] ^ 0x21aaf758;
		pad[2] ^= pad[15] << 5;
		major_2 (pad, pad[5]);
	}

	if (branch == 4)
	{
		pad[10] += 0x8958821;
		pad[16] += 0x3d2948e4;
		major_5 (pad, pad[4]);
	}

	if (branch == 3)
	{
		pad[3] -= pad[0] ^ 0x185f3b0d;
		pad[9] |= pad[7] ^ 0x2a19119f;
		major_12 (pad, pad[12]);
	}

	pad[11] += pad[17] < 0xc9a31cd6 ? pad[17] : pad[4];

	if (branch == 4)
	{
		pad[9] ^= 0x1c686298;
		pad[12] ^= pad[15] - 0xf5cfde0;
		major_11 (pad, pad[9]);
	}

	if (branch == 13)
	{
		pad[7] |= 0x378d3869;
		TRY (minor_13);
		major_13 (pad, pad[13]);
	}

	seed *= pad[12] + 0xbd99f684;

	if (branch == 7)
	{
		pad[6] += pad[19] - 0x3f5675d6;
		pad[19] ^= my_cos (pad[9]) ? 0x57337b8 : pad[14];
		major_22 (pad, seed);
	}

	if (branch == 8)
	{
		pad[13] *= 0x1dcee48b;
		minor_30;
		major_21 (pad, seed);
	}

	pad[10] += 0xaa3373fc - pad[6];

	if (branch == 5)
	{
		minor_31 (0xe);
		TRY (minor_1 (0xbc90d50));
		major_16 (pad, seed);
	}

	if (branch == 15)
	{
		pad[9] |= pad[7] ^ 0x2a19119f;
		pad[0] += pad[18] ^ 0x4ac16b8d;
		major_1 (pad, seed);
	}

	if (branch == 6)
	{
		minor_34;
		pad[2] ^= my_sin (pad[13]) ? 0xfd08092 : pad[10];
		major_14 (pad, seed);
	}

	seed ^= pad[11] < 0xa4dda95a ? pad[11] : pad[2];

	if (branch == 6)
	{
		pad[16] += 0x1a36972b;
		minor_17;
		major_6 (pad, pad[5]);
	}

	if (branch == 12)
	{
		TRY (minor_12 (0xb6571d3f));
		pad[0] += pad[6] * 0x3c;
		major_9 (pad, pad[9]);
	}

	seed *= pad[14] + 0x9baa8db;

	if (branch == 9)
	{
		TRY (minor_9 (0xe378a0ed));
		pad[5] += pad[0] ^ 0x3e17add3;
		major_2 (pad, pad[8]);
	}

	if (branch == 11)
	{
		TRY (minor_10 (0xbd149bd9));
		minor_32 (0x6476f303);
		major_5 (pad, seed);
	}

	pad[17] += my_sqrt (pad[12]);

	if (branch == 10)
	{
		pad[13] *= 0xa02fe00;
		pad[12] += pad[6] + 0x21d7bf61;
		major_12 (pad, pad[19]);
	}

	if (branch == 3)
	{
		pad[13] *= 0x111b84cd;
		minor_30;
		major_11 (pad, pad[2]);
	}

	pad[7] ^= pad[9] * 0x27219096;

	if (branch == 2)
	{
		pad[6] += pad[19] - 0x3f5675d6;
		pad[16] += 0xfe49a900;
		major_13 (pad, pad[15]);
		TRY (minor_6);
		pad[19] ^= my_cos (pad[9]) ? 0x57337b8 : pad[14];
		major_22 (pad, pad[19]);
	}

	ROLEQ (pad[2], 0x6c0798b3 ^ seed);
}

void major_9 (u32 *pad, u32 seed)
{
	int branch = pad[8] & 15;

	if (branch == 10)
	{
		minor_29;
		pad[7] &= pad[13] ^ 0x21aaf758;
		major_19 (pad, pad[0]);
	}

	if (branch == 3)
	{
		pad[16] += 0x45e88961;
		TRY (minor_8);
		major_25 (pad, pad[15]);
	}

	if (branch == 8)
	{
		TRY (minor_6);
		minor_22;
		major_17 (pad, pad[2]);
	}

	pad[8] |= pad[9] * 0x6a;

	if (branch == 0)
	{
		pad[7] &= 0x30004a24;
		pad[9] ^= pad[7] * 0x44;
		major_4 (pad, pad[11]);
	}

	if (branch == 14)
	{
		pad[13] += pad[15] < 0x137bffeb ? pad[15] : pad[11];
		TRY (minor_7);
		major_18 (pad, pad[13]);
	}

	pad[10] &= pad[6] - 0x1286a10;

	if (branch == 12)
	{
		pad[9] += ROL (pad[4], 9);
		TRY (minor_4);
		major_15 (pad, pad[17]);
	}

	if (branch == 2)
	{
		TRY (minor_8);
		minor_20;
		major_3 (pad, pad[13]);
	}

	seed |= seed + 0x20029bc7;
	ROREQ (pad[14], ROL (seed, 8));

	if (branch == 9)
	{
		pad[7] &= pad[13] ^ 0x21aaf758;
		minor_26;
		major_21 (pad, pad[5]);
	}

	seed += 0x176cf052 - pad[12];

	if (branch == 15)
	{
		pad[12] ^= pad[15] - 0xf5cfde0;
		pad[13] *= 0xa02fe00;
		major_16 (pad, seed);
	}

	if (branch == 1)
	{
		pad[13] *= ROR (pad[3], 5);
		pad[19] ^= my_cos (pad[9]) ? 0x57337b8 : pad[14];
		major_1 (pad, pad[17]);
	}

	ROLEQ (pad[8], pad[4] | 0xf);

	if (branch == 5)
	{
		pad[9] ^= pad[7] * 0x44;
		pad[7] &= 0x1df23f52;
		major_14 (pad, pad[6]);
	}

	if (branch == 4)
	{
		pad[5] -= pad[15];
		pad[6] ^= 0x851242df;
		major_6 (pad, seed);
	}

	pad[13] *= pad[2] * 0x65;

	if (branch == 0)
	{
		pad[14] |= pad[3] ^ 0x4345732;
		pad[13] += pad[15] < 0x137bffeb ? pad[15] : pad[11];
		major_19 (pad, pad[10]);
	}

	if (branch == 6)
	{
		minor_29;
		minor_22;
		major_25 (pad, seed);
	}

	pad[11] |= ROR (pad[17], 29);

	if (branch == 13)
	{
		pad[10] *= pad[10] - 0x5eae6bf;
		pad[16] += 0x5e01d54b;
		major_17 (pad, pad[18]);
	}

	pad[17] &= seed * 0x30;

	if (branch == 7)
	{
		minor_27;
		TRY (minor_12 (0x65ec261));
		major_4 (pad, pad[0]);
	}

	if (branch == 11)
	{
		pad[14] |= pad[3] ^ 0x4345732;
		pad[0] += pad[18] ^ 0x4ac16b8d;
		major_18 (pad, pad[16]);
	}

	pad[13] |= pad[3] * 0x3e;
}

void major_10 (u32 *pad, u32 seed)
{
	int branch = (pad[4] ^ pad[12] ^ pad[17]) & 15;

	if (branch == 9)
	{
		TRY (minor_12 (0x9febcd24));
		pad[7] &= 0x259cf308;
		major_16 (pad, pad[14]);
	}

	if (branch == 4)
	{
		pad[6] ^= 0xa7e6f9b9;
		pad[10] *= pad[10] - 0x5eae6bf;
		major_1 (pad, pad[2]);
	}

	pad[9] += pad[11] < 0x3d7f80c ? pad[11] : pad[9];

	if (branch == 2)
	{
		pad[12] += pad[6] + 0x21d7bf61;
		pad[14] |= pad[3] ^ 0x4345732;
		major_9 (pad, pad[1]);
	}

	if (branch == 6)
	{
		TRY (minor_10 (0xece6bfa0));
		pad[14] |= pad[3] ^ 0x4345732;
		major_14 (pad, seed);
	}

	if (branch == 8)
	{
		TRY (minor_7);
		pad[18] *= pad[10] + 0x466e09cf;
		major_6 (pad, pad[12]);
	}

	pad[10] *= my_cos (seed) ? 0x16b578ee : pad[2];

	if (branch == 6)
	{
		minor_19;
		minor_29;
		major_2 (pad, pad[10]);
	}

	if (branch == 13)
	{
		pad[9] ^= pad[7] * 0x44;
		minor_35;
		major_5 (pad, pad[10]);
	}

	pad[17] += seed * 0x4d;

	if (branch == 1)
	{
		minor_26;
		pad[19] ^= my_cos (pad[9]) ? 0x57337b8 : pad[14];
		major_12 (pad, seed);
		pad[19] ^= pad[15] ^ 0x3574ed3;
		pad[10] += 0x9f2550bd;
		major_13 (pad, seed);
	}

	if (branch == 14)
	{
		TRY (minor_8);
		minor_31 (0x7);
		major_11 (pad, pad[19]);
	}

	ROLEQ (seed, pad[7] * 0xd);

	if (branch == 12)
	{
		pad[9] &= 0x2f23cdc6;
		pad[10] *= pad[10] - 0x5eae6bf;
		major_22 (pad, pad[2]);
	}

	if (branch == 3)
	{
		minor_28;
		minor_26;
		major_8 (pad, pad[4]);
		pad[12] *= pad[12];
		TRY (minor_6);
		major_16 (pad, pad[18]);
	}

	pad[4] += my_sin (pad[0]) ? 0x1873296 : pad[1];

	if (branch == 10)
	{
		minor_26;
		TRY (minor_13);
		major_1 (pad, pad[8]);
	}

	if (branch == 4)
	{
		pad[12] *= 0xf44cb55;
		pad[16] += 0x75a864cf;
		major_14 (pad, pad[4]);
	}

	pad[12] += 0x1c0bd6db - pad[11];

	if (branch == 2)
	{
		pad[13] *= 0x17b441db;
		TRY (minor_12 (0x8951503f));
		major_9 (pad, pad[19]);
	}

	if (branch == 7)
	{
		TRY (minor_7);
		minor_26;
		major_6 (pad, pad[14]);
	}

	if (branch == 11)
	{
		pad[0] += pad[6] * 0x3c;
		pad[4] -= pad[17] ^ 0x2217cf47;
		major_2 (pad, pad[8]);
	}

	pad[18] -= pad[6] * 0x2c;

	if (branch == 0)
	{
		pad[13] *= 0x1855aabc;
		TRY (minor_3);
		major_12 (pad, seed);
	}

	if (branch == 5)
	{
		pad[18] *= pad[10] + 0x466e09cf;
		pad[9] ^= pad[7] * 0x44;
		major_11 (pad, pad[10]);
	}

	if (branch == 7)
	{
		pad[18] *= pad[10] + 0x466e09cf;
		minor_29;
		major_5 (pad, pad[15]);
	}

	pad[6] ^= pad[16] ^ 0x354e354d;

	if (branch == 0)
	{
		TRY (minor_8);
		minor_22;
		major_8 (pad, pad[8]);
	}

	if (branch == 5)
	{
		pad[6] += 0xfe07af0e - pad[3];
		pad[13] *= 0xa02fe00;
		major_16 (pad, pad[10]);
	}

	if (branch == 8)
	{
		pad[14] |= pad[3] ^ 0x4345732;
		minor_32 (0x8a0e1ad7);
		major_22 (pad, pad[8]);
	}

	if (branch == 15)
	{
		pad[9] |= pad[7] ^ 0x2a19119f;
		minor_28;
		major_13 (pad, seed);
	}

	pad[3] += pad[13] + 0x3ba9c809;
}

void major_11 (u32 *pad, u32 seed)
{
	int branch = (pad[6] ^ seed ^ pad[14]) & 15;

	if (branch == 2)
	{
		pad[19] ^= pad[15] ^ 0x3574ed3;
		pad[13] -= pad[1];
		major_18 (pad, pad[0]);
	}

	if (branch == 0)
	{
		pad[7] ^= 0x414517ea;
		TRY (minor_4);
		major_15 (pad, pad[13]);
	}

	pad[14] &= seed * 0x3f;

	if (branch == 10)
	{
		minor_19;
		pad[9] &= 0x38063558;
		major_3 (pad, pad[10]);
	}

	if (branch == 15)
	{
		minor_34;
		pad[12] += 0x5c1481dd;
		major_21 (pad, pad[11]);
	}

	ROREQ (pad[10], pad[14] * 0x13);

	if (branch == 8)
	{
		pad[19] ^= my_cos (pad[9]) ? 0x57337b8 : pad[14];
		pad[12] ^= pad[15] - 0xf5cfde0;
		major_16 (pad, seed);
	}

	if (branch == 4)
	{
		pad[3] -= pad[0] ^ 0x185f3b0d;
		pad[6] ^= 0xa8115127;
		major_1 (pad, pad[5]);
	}

	pad[11] ^= seed - 0x3c17609c;

	if (branch == 1)
	{
		pad[12] += pad[6] ^ 0x211f5e40;
		minor_29;
		major_14 (pad, seed);
	}

	if (branch == 7)
	{
		ROREQ (pad[9], 11);
		TRY (minor_3);
		major_6 (pad, pad[11]);
	}

	pad[14] += my_sin (pad[9]) ? 0x2d3f1771 : pad[11];

	if (branch == 5)
	{
		TRY (minor_8);
		pad[0] += pad[18] ^ 0x4ac16b8d;
		major_9 (pad, pad[9]);
	}

	if (branch == 2)
	{
		pad[12] *= 0xf44cb55;
		minor_31 (0x1f);
		major_2 (pad, pad[13]);
	}

	if (branch == 1)
	{
		pad[9] &= 0x3f34e168;
		pad[18] *= pad[10] + 0x466e09cf;
		major_5 (pad, seed);
	}

	pad[18] &= pad[17] + 0x21012257;

	if (branch == 14)
	{
		pad[6] += 0xfe07af0e - pad[3];
		TRY (minor_2 (0x51f9a91a));
		major_12 (pad, pad[14]);
	}

	if (branch == 12)
	{
		pad[14] |= pad[3] ^ 0x4345732;
		pad[16] += 0x485c892b;
		major_18 (pad, pad[12]);
	}

	pad[19] &= pad[10] ^ 0x6fc516d5;

	if (branch == 6)
	{
		TRY (minor_5);
		pad[12] += pad[6] ^ 0x211f5e40;
		major_15 (pad, pad[13]);
	}

	if (branch == 11)
	{
		TRY (minor_6);
		minor_28;
		major_3 (pad, seed);
	}

	pad[8] ^= pad[11] * 0x7b;

	if (branch == 4)
	{
		pad[2] *= pad[3] + 0xd6863a6;
		minor_26;
		major_21 (pad, pad[4]);
	}

	if (branch == 3)
	{
		pad[19] ^= pad[15] ^ 0x3574ed3;
		minor_17;
		major_16 (pad, pad[9]);
	}

	pad[0] += pad[13] + 0x88a77a94;

	if (branch == 0)
	{
		TRY (minor_2 (0xf10f9d87));
		pad[12] ^= pad[15] - 0xf5cfde0;
		major_1 (pad, pad[15]);
	}

	if (branch == 9)
	{
		pad[13] -= pad[1];
		pad[2] ^= my_sin (pad[13]) ? 0xfd08092 : pad[10];
		major_14 (pad, pad[18]);
	}

	seed *= pad[8] - 0x44260e37;

	if (branch == 3)
	{
		minor_26;
		pad[10] += 0x8958821;
		major_6 (pad, pad[8]);
	}

	if (branch == 13)
	{
		pad[0] += pad[18] ^ 0x4ac16b8d;
		pad[7] ^= 0x129d6c5e;
		major_9 (pad, seed);
	}

	pad[2] &= ROL (pad[19], 14);
}

void major_12 (u32 *pad, u32 seed)
{
	int branch = (pad[7] ^ seed ^ pad[18]) & 15;

	if (branch == 15)
	{
		TRY (minor_7);
		pad[13] += pad[15] < 0x137bffeb ? pad[15] : pad[11];
		major_4 (pad, pad[17]);
	}

	pad[8] |= seed + 0xe43fc6b;

	if (branch == 1)
	{
		minor_32 (0x979304f6);
		minor_26;
		major_18 (pad, pad[6]);
	}

	if (branch == 8)
	{
		minor_20;
		TRY (minor_12 (0xf7131053));
		major_15 (pad, pad[13]);
	}

	pad[19] ^= seed * 0x4b;

	if (branch == 1)
	{
		pad[13] *= 0x85695585;
		pad[13] *= 0x2c9514d7;
		major_3 (pad, pad[17]);
	}

	if (branch == 10)
	{
		pad[9] |= pad[7] ^ 0x2a19119f;
		TRY (minor_1 (0x433a0094));
		major_21 (pad, pad[8]);
	}

	pad[1] ^= pad[14] * 0x16;

	if (branch == 0)
	{
		pad[13] *= 0x136644be;
		TRY (minor_6);
		major_16 (pad, pad[14]);
	}

	pad[7] |= seed ^ 0xe857063;

	if (branch == 4)
	{
		pad[18] -= pad[13] ^ 0x154abcdf;
		pad[12] += 0xf894616a;
		major_1 (pad, pad[6]);
	}

	if (branch == 9)
	{
		TRY (minor_3);
		pad[7] &= 0x3b887f26;
		major_14 (pad, pad[7]);
	}

	ROREQ (pad[6], pad[9] * 0x8413f1b6);

	if (branch == 3)
	{
		TRY (minor_9 (0xcd88ea76));
		TRY (minor_8);
		major_6 (pad, pad[19]);
	}

	if (branch == 13)
	{
		ROREQ (pad[9], 5);
		TRY (ROREQ (pad[1], 0x8));
		major_9 (pad, pad[1]);
	}

	pad[6] -= pad[17] < 0x417e2f7b ? pad[17] : pad[19];

	if (branch == 5)
	{
		pad[9] |= pad[7] ^ 0x2a19119f;
		minor_19;
		major_2 (pad, pad[1]);
	}

	if (branch == 12)
	{
		pad[13] -= pad[1];
		pad[0] += pad[6] * 0x3c;
		major_5 (pad, pad[7]);
	}

	pad[6] |= my_sqrt (seed);

	if (branch == 2)
	{
		pad[2] ^= pad[15] << 5;
		pad[7] &= pad[13] ^ 0x21aaf758;
		major_4 (pad, seed);
	}

	pad[2] ^= pad[8] + 0x3e85747b;

	if (branch == 0)
	{
		minor_20;
		pad[9] &= 0x5a61aa8d;
		major_18 (pad, pad[14]);
	}

	if (branch == 11)
	{
		pad[6] += 0xfe07af0e - pad[3];
		minor_32 (0xbf47f027);
		major_15 (pad, pad[3]);
	}

	pad[2] &= seed;

	if (branch == 3)
	{
		pad[13] *= 0xc9cf079;
		pad[18] -= pad[13] ^ 0x154abcdf;
		major_3 (pad, pad[4]);
	}

	if (branch == 7)
	{
		pad[12] ^= pad[15] - 0xf5cfde0;
		TRY (minor_4);
		major_21 (pad, pad[14]);
	}

	seed += pad[9] - 0xbc1b5d9;

	if (branch == 14)
	{
		TRY (minor_8);
		pad[9] ^= pad[7] * 0x44;
		major_16 (pad, seed);
	}

	pad[18] += pad[11] * 0x5b;

	if (branch == 2)
	{
		pad[7] &= pad[13] ^ 0x21aaf758;
		minor_17;
		major_1 (pad, pad[1]);
	}

	if (branch == 6)
	{
		pad[2] ^= pad[15] << 5;
		pad[18] -= pad[13] ^ 0x154abcdf;
		major_14 (pad, seed);
	}

	pad[4] ^= pad[4] - 0xe87fd622;
}

void major_13 (u32 *pad, u32 seed)
{
	int branch = (pad[4] ^ seed ^ pad[18]) & 15;

	if (branch == 12)
	{
		minor_29;
		pad[9] |= pad[7] ^ 0x2a19119f;
		major_15 (pad, pad[11]);
	}

	if (branch == 4)
	{
		ROREQ (pad[9], 10);
		pad[12] ^= pad[15] - 0xf5cfde0;
		major_3 (pad, seed);
	}

	seed ^= pad[1] * 0x6c;

	if (branch == 1)
	{
		TRY (minor_12 (0xad86172c));
		pad[12] ^= pad[15] - 0xf5cfde0;
		major_21 (pad, pad[7]);
	}

	if (branch == 9)
	{
		pad[0] += pad[6] * 0x3c;
		pad[9] &= 0xa2cc0e51;
		major_16 (pad, pad[10]);
	}

	pad[11] += pad[8] - 0xef3b680;

	if (branch == 5)
	{
		minor_20;
		pad[2] ^= my_sin (pad[13]) ? 0xfd08092 : pad[10];
		major_1 (pad, seed);
	}

	if (branch == 2)
	{
		pad[12] += 0x1f3bc7f4;
		pad[9] += ROL (pad[4], 9);
		major_14 (pad, pad[3]);
	}

	pad[19] -= seed ^ 0x42b04005;

	if (branch == 8)
	{
		TRY (minor_13);
		pad[0] += pad[18] ^ 0x4ac16b8d;
		major_6 (pad, pad[2]);
	}

	if (branch == 3)
	{
		TRY (minor_13);
		minor_17;
		major_9 (pad, seed);
	}

	pad[0] += my_sqrt (pad[16]);

	if (branch == 11)
	{
		pad[12] ^= pad[15] - 0xf5cfde0;
		ROREQ (pad[4], 0x19);
		major_2 (pad, pad[19]);
	}

	if (branch == 13)
	{
		TRY (minor_4);
		minor_26;
		major_5 (pad, pad[7]);
	}

	seed += pad[17] | 0xead7ac4a;

	if (branch == 1)
	{
		TRY (minor_1 (0xd3280a0));
		pad[7] ^= 0x3eb9d37;
		major_12 (pad, pad[12]);
	}

	if (branch == 14)
	{
		minor_28;
		minor_31 (0x1f);
		major_11 (pad, seed);
	}

	ROREQ (pad[2], pad[15] < 0x3f2998c ? pad[15] : seed);

	if (branch == 5)
	{
		TRY (minor_12 (0x2dd0e73));
		minor_26;
		major_15 (pad, seed);
	}

	if (branch == 3)
	{
		TRY (minor_9 (0x21602b81));
		pad[13] *= 0x52fa4a96;
		major_3 (pad, seed);
	}

	pad[4] += pad[2] ^ 0x1579499;

	if (branch == 10)
	{
		TRY (minor_7);
		pad[5] += pad[0] ^ 0x3e17add3;
		major_21 (pad, pad[5]);
	}

	if (branch == 0)
	{
		TRY (minor_5);
		pad[0] += pad[6] * 0x3c;
		major_16 (pad, pad[8]);
	}

	seed -= pad[2] * 0x74;

	if (branch == 0)
	{
		pad[13] -= pad[1];
		pad[19] ^= pad[15] ^ 0x3574ed3;
		major_1 (pad, seed);
	}

	if (branch == 15)
	{
		pad[9] &= 0x334ce7cf;
		TRY (minor_10 (0xbcbc7bb));
		major_14 (pad, seed);
	}

	pad[10] -= pad[10] | 0xae9eedbf;

	if (branch == 6)
	{
		TRY (minor_3);
		TRY (minor_6);
		major_6 (pad, pad[11]);
	}

	if (branch == 4)
	{
		pad[2] ^= my_sin (pad[13]) ? 0xfd08092 : pad[10];
		pad[13] *= 0x9f7e2a0;
		major_9 (pad, pad[19]);
	}

	seed += pad[17] ^ 0xae9eedbf;

	if (branch == 7)
	{
		TRY (minor_8);
		pad[18] *= pad[10] + 0x466e09cf;
		major_2 (pad, pad[8]);
	}

	if (branch == 2)
	{
		TRY (minor_3);
		pad[9] |= pad[7] ^ 0x2a19119f;
		major_5 (pad, pad[18]);
	}

	pad[16] -= pad[11] < 0x1e7d86ee ? pad[11] : seed;
}

void major_14 (u32 *pad, u32 seed)
{
	int branch = (pad[8] ^ seed ^ pad[11]) % 14;

	if (branch == 0)
	{
		TRY (minor_1 (0xe32bdca0));
		minor_30;
		major_23 (pad, pad[19]);
	}

	seed -= seed ^ 0xf74450ff;

	if (branch == 1)
	{
		minor_32 (0x788c78a4);
		pad[13] -= pad[1];
		major_24 (pad, seed);
	}

	pad[13] -= my_cos (pad[3]) ? 0xf74450ff : pad[4];

	if (branch == 9)
	{
		pad[9] |= pad[7] ^ 0x2a19119f;
		ROREQ (pad[9], 11);
		major_19 (pad, seed);
	}

	pad[9] ^= pad[6] * 0x59;

	if (branch == 7)
	{
		minor_26;
		pad[6] += 0xfe07af0e - pad[3];
		major_25 (pad, pad[11]);
	}

	if (branch == 8)
	{
		pad[13] -= pad[1];
		pad[4] ^= 0xb949718c;
		major_17 (pad, pad[7]);
	}

	pad[1] ^= my_sin (seed) ? 0xc90f1504 : pad[17];

	if (branch == 13)
	{
		pad[9] &= 0x59d432be;
		pad[18] -= pad[13] ^ 0x154abcdf;
		major_4 (pad, pad[4]);
	}

	pad[17] += pad[13] < 0xac24eb8 ? pad[13] : pad[9];

	if (branch == 5)
	{
		pad[4] ^= 0x3bcc51a7;
		pad[12] += 0x4ec6cf36;
		major_18 (pad, pad[1]);
	}

	seed |= ROR (pad[18], 11);

	if (branch == 3)
	{
		minor_20;
		pad[5] -= pad[15];
		major_15 (pad, pad[0]);
	}

	pad[4] += seed + 0xf65efbd;

	if (branch == 10)
	{
		TRY (minor_5);
		pad[2] *= pad[3] + 0xd6863a6;
		major_3 (pad, pad[5]);
	}

	if (branch == 11)
	{
		pad[7] &= 0xdf76eba8;
		TRY (minor_6);
		major_21 (pad, seed);
	}

	pad[4] ^= ROL (pad[8], 22);

	if (branch == 6)
	{
		TRY (minor_10 (0xec30bd82));
		pad[2] *= pad[3] + 0xd6863a6;
		major_16 (pad, pad[13]);
	}

	seed *= pad[6] + 0x6bbeb974;

	if (branch == 2)
	{
		pad[18] *= pad[10] + 0x466e09cf;
		pad[2] *= pad[3] + 0xd6863a6;
		major_1 (pad, pad[6]);
	}

	pad[16] -= pad[2] * 0x65a10fa8;

	if (branch == 12)
	{
		pad[19] ^= pad[15] ^ 0x3574ed3;
		ROREQ (pad[9], 15);
		major_23 (pad, pad[14]);
	}

	ROREQ (pad[13], my_sqrt (seed));

	if (branch == 4)
	{
		minor_19;
		minor_17;
		major_24 (pad, pad[0]);
	}

	if (branch == 0)
	{
		pad[7] ^= 0xc9d1f4a2;
		minor_26;
		major_19 (pad, seed);
	}

	pad[12] -= my_sin (pad[10]) ? 0x2818ae3c : seed;
}

void major_15 (u32 *pad, u32 seed)
{
	int branch = (pad[17] ^ seed ^ pad[19]) % 9;

	ROREQ (pad[19], pad[19] + 0xa);

	if (branch == 4)
	{
		minor_19;
		pad[6] ^= 0xf4c1a1c8;
		minor_37 (pad);
	}

	pad[5] ^= seed + 0x1ff8749d;

	if (branch == 5)
	{
		ROREQ (pad[4], 0x19);
		pad[9] += ROL (pad[4], 9);
		minor_36 (pad);
	}

	pad[13] ^= pad[15] + 0x19ad9d3;

	if (branch == 0)
	{
		pad[14] |= pad[3] ^ 0x4345732;
		minor_26;
		major_23 (pad, pad[13]);
	}

	ROREQ (pad[3], my_sqrt (pad[9]));

	if (branch == 1)
	{
		pad[16] += 0x188ae78f;
		pad[2] ^= pad[15] << 5;
		major_24 (pad, pad[12]);
	}

	seed ^= pad[12] ^ 0x82494ea7;

	if (branch == 0)
	{
		pad[14] |= pad[3] ^ 0x4345732;
		pad[7] &= 0x97ea531;
		major_19 (pad, pad[6]);
	}

	ROLEQ (pad[0], 0x82494ea7 & seed);

	if (branch == 7)
	{
		minor_20;
		TRY (minor_9 (0xd3d79cb4));
		major_25 (pad, pad[6]);
	}

	pad[18] ^= pad[9] - 0x5606038;

	if (branch == 3)
	{
		pad[13] += pad[15] < 0x137bffeb ? pad[15] : pad[11];
		pad[16] += 0x6a07a3d0;
		major_17 (pad, pad[8]);
	}

	pad[9] |= my_sin (pad[7]) ? 0x3ec62d23 : pad[6];

	if (branch == 2)
	{
		pad[18] *= pad[10] + 0x466e09cf;
		TRY (minor_6);
		major_4 (pad, pad[1]);
	}

	if (branch == 6)
	{
		minor_27;
		minor_22;
		major_18 (pad, pad[0]);
	}
}

void major_16 (u32 *pad, u32 seed)
{
	int branch = (pad[11] ^ seed ^ pad[5]) % 12;

	if (branch == 5)
	{
		pad[2] *= pad[3] + 0xd6863a6;
		minor_22;
		minor_37 (pad);
	}

	pad[4] ^= seed - 0xafd4eac;

	if (branch == 2)
	{
		TRY (minor_5);
		pad[0] += pad[18] ^ 0x4ac16b8d;
		minor_36 (pad);
	}

	pad[15] -= 0xafd4eac ^ seed;

	if (branch == 0)
	{
		TRY (minor_2 (0x80e3e69e));
		ROREQ (pad[9], 12);
		major_23 (pad, pad[4]);
	}

	pad[8] ^= my_sqrt (pad[16]);

	if (branch == 3)
	{
		pad[9] ^= 0x8e61a4f;
		pad[13] -= pad[1];
		major_24 (pad, seed);
	}

	if (branch == 10)
	{
		pad[6] += pad[19] - 0x3f5675d6;
		pad[13] *= 0xa02fe00;
		major_19 (pad, pad[6]);
	}

	if (branch == 4)
	{
		TRY (minor_8);
		TRY (minor_5);
		major_25 (pad, pad[0]);
	}

	pad[8] ^= pad[15] * 0x5f;

	if (branch == 0)
	{
		minor_32 (0x6191efec);
		pad[13] += pad[15] < 0x137bffeb ? pad[15] : pad[11];
		major_17 (pad, pad[9]);
	}

	seed -= seed & 0x179da692;
	pad[6] &= my_sin (seed) ? 0xcc35b823 : pad[14];

	if (branch == 9)
	{
		pad[6] += 0xfe07af0e - pad[3];
		pad[0] += pad[18] ^ 0x4ac16b8d;
		major_4 (pad, pad[0]);
	}

	if (branch == 6)
	{
		pad[7] |= 0xa885099;
		pad[9] ^= 0xdd34e6b;
		major_18 (pad, seed);
	}

	if (branch == 7)
	{
		pad[12] += 0x5e6f4861;
		pad[18] -= pad[13] ^ 0x154abcdf;
		major_15 (pad, pad[14]);
	}

	pad[10] += pad[1] + 0x217f7a00;

	if (branch == 1)
	{
		pad[0] += pad[18] ^ 0x4ac16b8d;
		minor_27;
		major_3 (pad, pad[17]);
	}

	pad[5] &= ROR (pad[0], 14);

	if (branch == 8)
	{
		pad[2] *= pad[3] + 0xd6863a6;
		pad[9] ^= pad[7] * 0x44;
		major_21 (pad, pad[13]);
	}

	if (branch == 11)
	{
		minor_30;
		pad[13] += pad[15] < 0x137bffeb ? pad[15] : pad[11];
		minor_37 (pad);
	}

	pad[12] |= ROL (pad[7], 14);
}

void major_17 (u32 *pad, u32 seed)
{
	int branch = (pad[8] ^ pad[7] ^ pad[12]) % 6;

	pad[1] |= pad[4] ^ 0x10104d4;

	if (branch == 3)
	{
		minor_20;
		ROREQ (pad[9], 12);
		minor_37 (pad);
	}

	seed = ((seed ^ 0x1ea9da8) + seed) * pad[18] * 0xd;

	if (branch == 0)
	{
		TRY (minor_1 (0x10381ff0));
		pad[2] *= pad[3] + 0xd6863a6;
		minor_36 (pad);
	}

	pad[14] += pad[12] * 0x19;
	pad[2] -= my_sqrt (pad[5]);

	if (branch == 4)
	{
		pad[16] += 0x81063b22;
		pad[9] ^= pad[7] * 0x44;
		major_23 (pad, seed);
	}

	pad[6] &= pad[4] - 0x679dca37;
	pad[1] ^= pad[16] + 0x988db31;

	if (branch == 0)
	{
		pad[7] ^= 0xa98896dd;
		TRY (minor_3);
		major_24 (pad, pad[6]);
	}

	pad[6] += ROR (seed, 10);
	seed -= pad[0] < 0x29ea2cb6 ? pad[0] : pad[3];

	if (branch == 2)
	{
		minor_35;
		pad[12] ^= pad[15] - 0xf5cfde0;
		major_19 (pad, seed);
	}

	seed *= my_sqrt (seed);
	pad[5] *= my_cos (seed) ? 0x211af3c8 : pad[19];

	if (branch == 5)
	{
		minor_28;
		pad[13] *= 0xa02fe00;
		major_25 (pad, pad[13]);
	}
}

void major_18 (u32 *pad, u32 seed)
{
	int branch = (pad[14] ^ pad[11] ^ pad[17]) & 7;

	pad[11] ^= ROR (pad[13], 21);

	if (branch == 5)
	{
		pad[6] += pad[19] - 0x3f5675d6;
		pad[9] ^= 0x94d017f;
		minor_37 (pad);
	}

	ROREQ (pad[3], pad[16] * 0xf);

	if (branch == 2)
	{
		pad[5] += pad[0] ^ 0x3e17add3;
		minor_34;
		minor_36 (pad);
	}

	pad[11] -= my_sqrt (pad[9]);
	pad[12] += 0x17267c5b - pad[11];

	if (branch == 3)
	{
		minor_31 (0xb);
		pad[7] &= pad[13] ^ 0x21aaf758;
		major_23 (pad, pad[0]);
	}

	pad[17] ^= seed ^ 0x35eddea4;

	if (branch == 0)
	{
		pad[10] += 0x3409139c;
		pad[19] ^= my_cos (pad[9]) ? 0x57337b8 : pad[14];
		major_24 (pad, pad[6]);
	}

	pad[6] *= pad[17] + 0xb89b51c;

	if (branch == 1)
	{
		pad[6] += 0xfe07af0e - pad[3];
		TRY (minor_2 (0x90254266));
		major_19 (pad, pad[6]);
	}

	pad[19] ^= pad[3] < 0x5755f00e ? pad[3] : pad[1];
	pad[15] ^= pad[12] * 0x17;

	if (branch == 7)
	{
		pad[13] *= ROR (pad[3], 5);
		pad[13] *= ROR (pad[3], 5);
		major_25 (pad, pad[9]);
	}

	pad[10] += 0x395f1d29 - seed;

	if (branch == 0)
	{
		pad[12] += 0x2272516f;
		pad[13] *= 0x48e3e7ac;
		major_17 (pad, pad[16]);
	}

	ROLEQ (pad[1], ROL (pad[8], 20));
	seed -= pad[9] ^ 0xc9c0bd95;

	if (branch == 6)
	{
		TRY (minor_2 (0x10b4eaef));
		pad[12] += 0x222fe8f5;
		major_4 (pad, seed);
	}

	ROLEQ (pad[18], pad[7] & 0x11);
}

void major_19 (u32 *pad, u32 seed)
{
	int branch = (pad[18] ^ pad[6] ^ pad[15]) & 3;

	seed *= pad[15] * 0x3c02927;
	ROREQ (seed, seed * 0x7);

	if (branch == 0)
	{
		pad[12] += pad[6] ^ 0x211f5e40;
		pad[9] ^= 0x6b4bfbe3;
		minor_37 (pad);
	}

	seed ^= pad[6] ^ 0xc1fcda0;
	pad[5] -= my_cos (pad[6]) ? 0xb9269bb0 : pad[10];

	if (branch == 0)
	{
		pad[9] ^= 0x703e6c86;
		pad[16] += 0xbb78136d;
		minor_36 (pad);
	}

	seed *= pad[19] + 0x11500e47;
	pad[3] ^= ROL (pad[4], 20);

	if (branch == 3)
	{
		pad[2] ^= pad[15] << 5;
		pad[19] ^= pad[15] ^ 0x3574ed3;
		major_23 (pad, pad[15]);
	}

	pad[13] -= my_sqrt (seed);
	ROREQ (seed, my_cos (seed) ? 7 : pad[10]);
	pad[16] = pad[15] * pad[16] * 0x4a;

	if (branch == 1)
	{
		pad[7] ^= 0xb3bb63f;
		pad[4] -= pad[17] ^ 0x2217cf47;
		major_24 (pad, seed);
	}
}

void major_21 (u32 *pad, u32 seed)
{
	int branch = (pad[1] ^ pad[0] ^ pad[16]) % 11;

	if (branch == 2)
	{
		pad[2] *= pad[3] + 0xd6863a6;
		pad[12] ^= pad[15] - 0xf5cfde0;
		minor_37 (pad);
	}

	pad[5] -= seed;

	if (branch == 8)
	{
		pad[16] += 0x2b058ae8;
		pad[6] += 0xfe07af0e - pad[3];
		minor_36 (pad);
	}

	pad[17] ^= ROL (pad[18], 28);

	if (branch == 4)
	{
		pad[2] *= pad[3] + 0xd6863a6;
		minor_32 (0x79fb5201);
		major_23 (pad, pad[7]);
	}

	pad[0] ^= my_sqrt (pad[12]);

	if (branch == 0)
	{
		pad[19] ^= pad[15] ^ 0x3574ed3;
		TRY (minor_5);
		major_24 (pad, pad[2]);
	}

	pad[10] ^= seed * 0x6c;

	if (branch == 9)
	{
		pad[2] ^= my_sin (pad[13]) ? 0xfd08092 : pad[10];
		minor_32 (0x6ddf8c10);
		major_19 (pad, pad[10]);
	}

	pad[8] -= my_cos (pad[12]) ? 0x8759908e : seed;

	if (branch == 7)
	{
		minor_19;
		minor_29;
		major_25 (pad, pad[1]);
	}

	seed ^= my_sin (seed) ? 0x2c99fade : pad[14];

	if (branch == 1)
	{
		TRY (minor_12 (0x3fcf3163));
		pad[9] ^= pad[7] * 0x44;
		major_17 (pad, seed);
	}

	pad[15] += my_cos (pad[11]) ? 0x1bec01f : seed;

	if (branch == 5)
	{
		pad[13] *= 0x1bd5157f;
		pad[6] += pad[19] - 0x3f5675d6;
		major_4 (pad, pad[15]);
	}

	ROREQ (pad[1], 0x80d9edac * pad[16]);

	if (branch == 0)
	{
		TRY (minor_10 (0xfde30e03));
		pad[9] |= pad[7] ^ 0x2a19119f;
		major_18 (pad, seed);
	}

	pad[7] &= pad[15] * 0xa8f285;

	if (branch == 10)
	{
		pad[7] ^= 0xef011757;
		ROREQ (pad[9], 9);
		major_15 (pad, pad[13]);
	}

	if (branch == 3)
	{
		pad[12] += pad[6] + 0x21d7bf61;
		pad[6] += pad[19] - 0x3f5675d6;
		major_3 (pad, pad[10]);
	}

	pad[3] *= my_sin (pad[8]) ? 0x5b51fb19 : pad[2];

	if (branch == 6)
	{
		pad[9] += ROL (pad[4], 9);
		minor_22;
		minor_37 (pad);
	}

	pad[11] ^= pad[17] * 0x44;
}

void major_22 (u32 *pad, u32 seed)
{
	int branch = (pad[5] ^ pad[0] ^ seed) & 15;

	if (branch == 3)
	{
		TRY (minor_6);
		minor_31 (0x13);
		major_3 (pad, seed);
	}

	if (branch == 0)
	{
		pad[6] ^= 0x6066818c;
		pad[13] -= pad[1];
		major_21 (pad, pad[2]);
	}

	pad[14] ^= ROL (pad[16], 22);

	if (branch == 12)
	{
		pad[10] += 0x830ba927;
		minor_32 (0x6f3a3876);
		major_16 (pad, pad[8]);
	}

	if (branch == 1)
	{
		minor_34;
		pad[16] += 0x1bc7b861;
		major_1 (pad, pad[6]);
	}

	pad[12] ^= pad[11] < 0x521b2180 ? pad[11] : pad[9];

	if (branch == 1)
	{
		minor_31 (0x12);
		pad[0] += pad[6] * 0x3c;
		major_14 (pad, pad[15]);
	}

	if (branch == 8)
	{
		TRY (minor_5);
		pad[18] *= pad[10] + 0x466e09cf;
		major_6 (pad, pad[13]);
	}

	if (branch == 4)
	{
		pad[9] += ROL (pad[4], 9);
		pad[2] *= pad[3] + 0xd6863a6;
		major_9 (pad, pad[16]);
	}

	pad[18] &= my_sqrt (pad[9]);

	if (branch == 5)
	{
		TRY (minor_4);
		pad[2] ^= pad[15] << 5;
		major_2 (pad, pad[2]);
	}

	if (branch == 6)
	{
		pad[2] *= pad[3] + 0xd6863a6;
		pad[6] += 0xfe07af0e - pad[3];
		major_5 (pad, pad[14]);
	}

	pad[18] -= pad[16] * 0x77;

	if (branch == 9)
	{
		pad[4] ^= 0xa09619f7;
		ROREQ (pad[4], 0x19);
		major_12 (pad, pad[10]);
	}

	if (branch == 10)
	{
		pad[12] ^= pad[15] - 0xf5cfde0;
		pad[13] *= 0x6cd0251e;
		major_11 (pad, pad[0]);
	}

	if (branch == 6)
	{
		pad[2] *= pad[3] + 0xd6863a6;
		ROREQ (pad[4], 0x1a);
		major_13 (pad, seed);
	}

	pad[13] ^= 0x4930de03 ^ seed;

	if (branch == 2)
	{
		pad[10] += 0x6467451;
		pad[4] -= pad[17] ^ 0x2217cf47;
		major_3 (pad, pad[2]);
	}

	if (branch == 7)
	{
		pad[4] -= pad[17] ^ 0x2217cf47;
		pad[0] += pad[18] ^ 0x4ac16b8d;
		major_21 (pad, pad[0]);
	}

	pad[6] -= my_sqrt (pad[10]);

	if (branch == 3)
	{
		TRY (minor_9 (0x5b9d1f9));
		pad[10] += 0x8958821;
		major_16 (pad, pad[8]);
	}

	if (branch == 4)
	{
		pad[13] -= pad[1];
		pad[13] *= 0x72494c9c;
		major_1 (pad, seed);
	}

	if (branch == 13)
	{
		pad[12] += pad[6] + 0x21d7bf61;
		pad[13] *= ROR (pad[3], 5);
		major_14 (pad, pad[2]);
	}

	seed -= ROR (pad[8], 17);

	if (branch == 15)
	{
		pad[19] += 0x12b9e29d - pad[12];
		pad[0] += pad[6] * 0x3c;
		major_6 (pad, pad[4]);
	}

	if (branch == 2)
	{
		pad[19] ^= my_cos (pad[9]) ? 0x57337b8 : pad[14];
		minor_20;
		major_9 (pad, pad[16]);
	}

	ROREQ (pad[14], seed - 0x3b46a63f);

	if (branch == 11)
	{
		pad[19] ^= my_cos (pad[9]) ? 0x57337b8 : pad[14];
		pad[9] += ROL (pad[4], 9);
		major_2 (pad, pad[9]);
	}

	if (branch == 5)
	{
		pad[19] += 0x12b9e29d - pad[12];
		TRY (minor_4);
		major_5 (pad, pad[6]);
	}

	if (branch == 0)
	{
		minor_20;
		pad[6] ^= 0xa9c74969;
		major_12 (pad, pad[14]);
	}

	pad[8] ^= ROR (seed, 6);

	if (branch == 14)
	{
		pad[12] += 0x49fc5980;
		pad[3] -= pad[0] ^ 0x185f3b0d;
		major_11 (pad, seed);
	}

	pad[0] += my_sin (pad[0]) ? 0x9bcd446 : pad[14];
}

void major_23 (u32 *pad, u32 seed)
{
	int branch = seed & 1;

	pad[4] += pad[8] - 0x16f911e4;
	pad[9] ^= pad[2] * 11;
	pad[10] ^= pad[7] < 0x402226f ? pad[7] : pad[2];
	seed |= pad[17] - 0x1e97aeb;
	seed |= pad[14] < 0xf3b1e0b3 ? pad[14] : pad[5];

	if (branch == 0)
	{
		pad[7] &= pad[13] ^ 0x21aaf758;
		minor_32 (0x640f077d);
		minor_37 (pad);
	}

	pad[1] -= pad[19] * 0x64;
	pad[1] += seed - 0x18d1b90;
	pad[7] -= pad[3] ^ 0x44de1958;
	pad[11] ^= ROL (pad[2], 9);
	pad[17] += ROL (pad[12], 27);

	if (branch == 0)
	{
		TRY (minor_9 (0xdc306f47));
		pad[9] ^= pad[7] * 0x44;
		minor_36 (pad);
	}

	ROREQ (pad[7], pad[13]);
}

void major_24 (u32 *pad, u32 seed)
{
	int branch = (pad[2] ^ seed ^ pad[7]) % 3;

	seed *= my_cos (seed) ? 0x6be8f94 : seed;
	pad[2] ^= pad[2] + 0x3786364b;
	ROLEQ (pad[17], seed - 0x10);

	if (branch == 0)
	{
		minor_35;
		minor_27;
		minor_37 (pad);
	}

	pad[5] += my_sin (pad[16]) ? 0x3af2a8e2 : pad[16];

	if (branch == 0)
	{
		TRY (minor_5);
		pad[2] *= pad[3] + 0xd6863a6;
		minor_36 (pad);
	}

	pad[13] ^= my_cos (pad[16]) ? 0xf6951daa : pad[1];
	pad[18] |= pad[17] & 0x6361a322;

	if (branch == 1)
	{
		pad[13] *= ROR (pad[3], 5) * 0xb25cb20f;
		major_23 (pad, pad[15]);
	}
}

void major_25 (u32 *pad, u32 seed)
{
	int branch = (pad[7] ^ pad[2] ^ seed) % 5;

	pad[2] -= 0x31b8a51 & seed;

	if (branch == 3)
	{
		pad[2] ^= my_sin (pad[13]) ? 0xfd08092 : pad[10];
		pad[9] &= 0x49a7e0c7;
		minor_37 (pad);
	}

	pad[1] &= ROR (seed, 29);
	ROLEQ (pad[12], my_cos (pad[1]) ? 27 : pad[5]);

	if (branch == 2)
	{
		TRY (minor_4);
		pad[2] ^= my_sin (pad[13]) ? 0xfd08092 : pad[10];
		minor_36 (pad);
	}

	ROREQ (seed, my_sqrt (seed));
	seed ^= 0xc63d7671 * seed;
	pad[17] += pad[19] * 0x7a;

	if (branch == 0)
	{
		pad[10] += 0x8958821;
		pad[18] *= pad[10] + 0x466e09cf;
		major_23 (pad, pad[10]);
	}

	ROREQ (pad[18], my_cos (pad[6]) ? 0x11 : pad[1]);

	if (branch == 4)
	{
		TRY (minor_7);
		pad[9] ^= 0x3480eee;
		major_24 (pad, seed);
	}

	pad[10] -= my_sqrt (seed);
	pad[11] &= seed * 0x3f;

	if (branch == 0)
	{
		pad[18] *= pad[10] + 0x466e09cf;
		pad[13] *= 0x6ff7af6a;
		major_19 (pad, pad[17]);
	}

	ROLEQ (pad[1], pad[15] + 0x19);
}

void mix (u32 *pad, u32 seed)
{
	int branch = (pad[5] ^ pad[9] ^ pad[19]) & 15;

	switch (branch)
	{
	case 0:
		pad[2] ^= my_sin (pad[13]) ? 0xfd08092 : pad[10];
		TRY (minor_5);
		major_5 (pad, 0x45835eb3);
		break;
	case 5:
		TRY (minor_8);
		pad[12] += 0x33bd47dd;
		major_6 (pad, pad[9]);
		break;
	case 8:
		minor_28;
		pad[6] += pad[19] - 0x3f5675d6;
		major_9 (pad, pad[10]);
		break;
	case 9:
		TRY (minor_7);
		TRY (minor_13);
		major_14 (pad, seed);
		break;
	case 10:
		TRY (minor_5);
		pad[6] ^= 0x33b5c9ac;
		major_2 (pad, pad[0]);
		break;
	}

	pad[2] ^= pad[6] + 0x1847de17;

	switch (branch)
	{
	case 1:
		pad[9] ^= 0x1df05ea2;
		pad[19] += 0x12b9e29d - pad[12];
		major_12 (pad, 0x45835eb3);
		break;
	case 10:
		minor_28;
		pad[12] *= pad[12];
		major_11 (pad, pad[6]);
		break;
	}

	pad[19] += pad[12] * 0x68;

	switch (branch)
	{
	case 2:
		minor_28;
		pad[12] ^= pad[15] - 0xf5cfde0;
		major_13 (pad, pad[19]);
		pad[6] += pad[19] - 0x3f5675d6;
		pad[12] += 0x602283af;
		major_22 (pad, pad[0]);
		break;
	case 6:
		TRY (minor_1 (0x706a6bc));
		TRY (minor_2 (0x82b598a1));
		major_8 (pad, 0x45835eb3);
		break;
	}

	pad[7] -= pad[14] & 0x1ada7fa;

	switch (branch)
	{
	case 4:
		minor_20;
		pad[12] += pad[6] + 0x21d7bf61;
		major_10 (pad, pad[16]);
		break;
	case 6:
		TRY (minor_7);
		TRY (minor_2 (0xd2950f8c));
		major_7 (pad, 0x45835eb3);
		break;
	}

	seed = (pad[0] + 0xd092d1bb) & 0x45835eb3;

	switch (branch)
	{
	case 3:
		TRY (minor_5);
		pad[7] &= pad[13] ^ 0x21aaf758;
		major_9 (pad, pad[17]);
		break;
	case 4:
		pad[13] -= pad[1];
		pad[18] *= pad[10] + 0x466e09cf;
		major_14 (pad, pad[5]);
		break;
	case 7:
		TRY (minor_9 (0x6d32760));
		pad[9] ^= pad[7] * 0x44;
		major_6 (pad, pad[8]);
		break;
	}

	ROLEQ (pad[8], pad[3] ^ 0x6);

	switch (branch)
	{
	case 0:
		pad[7] &= pad[13] ^ 0x21aaf758;
		pad[14] |= pad[3] ^ 0x4345732;
		major_12 (pad, pad[9]);
		break;
	case 5:
		minor_22;
		minor_29;
		major_2 (pad, pad[1]);
		break;
	case 9:
		pad[13] -= pad[1];
		TRY (minor_7);
		major_5 (pad, pad[2]);
		break;
	}

	pad[9] *= pad[14] | 0xbbf1fbef;

	switch (branch)
	{
	case 15:
		pad[12] += pad[6] ^ 0x211f5e40;
		pad[5] += pad[0] ^ 0x3e17add3;
		major_11 (pad, pad[8]);
		break;
	case 12:
		pad[19] ^= pad[15] ^ 0x3574ed3;
		minor_30;
		major_13 (pad, pad[16]);
		break;
	}

	seed *= my_sqrt (pad[1]);

	switch (branch)
	{
	case 11:
		minor_32 (0x678aae2c);
		minor_22;
		major_22 (pad, pad[11]);
		break;
	case 14:
		pad[13] *= ROR (pad[3], 5);
		minor_35;
		major_8 (pad, pad[18]);
		break;
	case 7:
		ROREQ (pad[4], 0x2);
		ROREQ (pad[9], 4);
		major_10 (pad, pad[0]);
		break;
	}

	pad[3] -= pad[7] ^ 0x4e46f05d;

	switch (branch)
	{
	case 3:
		pad[7] ^= 0xeda01e71;
		pad[13] -= pad[1];
		major_7 (pad, pad[11]);
		break;
	case 8:
		pad[2] ^= my_sin (pad[13]) ? 0xfd08092 : pad[10];
		pad[9] += ROL (pad[4], 9);
		major_14 (pad, pad[2]);
		break;
	}

	pad[19] ^= 0xb1bdd560 ^ seed;

	switch (branch)
	{
	case 1:
		pad[12] ^= pad[15] - 0xf5cfde0;
		pad[12] *= pad[12];
		major_9 (pad, pad[5]);
		break;
	case 13:
		pad[18] *= pad[10] + 0x466e09cf;
		pad[2] ^= pad[15] << 5;
		major_6 (pad, pad[15]);
		break;
	}

	pad[6] ^= my_sqrt (pad[5]);
}
