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

/*
 * This file was relayed to me and is originally from Raimar Falke.
 * I cleaned it up a bit to save bandwidth.
 * Used for encryption version 0x02
 * Then it was cleaned up a whole lot more...
 * And then Thingol cleaned it up even more, many thanks!
 */

typedef unsigned char u8;
typedef unsigned int u32;

static void mix_major0 (u32 *pad, u32 seed);

/* this all works on unsigned ints so endianess is not an issue */

void enc_type_2 (unsigned int *key, unsigned int seed)
{
	mix_major0 (key, seed);
}

static void mix_major0 (u32 *pad, u32 seed);
static void mix_major1 (u32 *pad, u32 seed);
static void mix_major2 (u32 *pad, u32 seed);
static void mix_major3 (u32 *pad, u32 seed);
static void mix_major4 (u32 *pad, u32 seed);
static void mix_major5 (u32 *pad, u32 seed);
static void mix_major6 (u32 *pad, u32 seed);
static void mix_major7 (u32 *pad, u32 seed);
static void mix_major8 (u32 *pad, u32 seed);
static void mix_major9 (u32 *pad, u32 seed);
static void mix_major10 (u32 *pad, u32 seed);
static void mix_major11 (u32 *pad, u32 seed);
static void mix_major12 (u32 *pad, u32 seed);
static void mix_major13 (u32 *pad, u32 seed);
static void mix_major14 (u32 *pad, u32 seed);
static void mix_major15 (u32 *pad, u32 seed);
static void mix_major16 (u32 *pad, u32 seed);
static void mix_major17 (u32 *pad, u32 seed);
static void mix_major18 (u32 *pad, u32 seed);
static void mix_major19 (u32 *pad, u32 seed);
static void mix_major20 (u32 *pad, u32 seed);
static void mix_major21 (u32 *pad, u32 seed);
static void mix_major22 (u32 *pad, u32 seed);
static void mix_major23 (u32 *pad, u32 seed);
static void mix_major24 (u32 *pad, u32 seed);

#define mix_minor20 ROREQ (pad[12], pad[14] * 0x3)
#define mix_minor21 pad[12] *= pad[1] * 0x4b4f2e1
#define mix_minor22 pad[2] *= pad[10] + 0xfa1f1e0b
#define mix_minor23 pad[19] += pad[19] ^ 0x43b6b05
#define mix_minor24 pad[18] -= ROR (pad[4], 18)
#define mix_minor25 pad[0] &= pad[10] + 0xfc9be92d
#define mix_minor26 pad[9] ^= pad[3] + 0xbe5fec7d
#define mix_minor27 pad[6] *= pad[15] | 0x46afede0
#define mix_minor28 pad[17] -= pad[6] * 0x1b677cc8
#define mix_minor29 pad[14] &= pad[15] + 0xfc471d2b
#define mix_minor30 pad[19] += pad[16] + 0x24a7d94d
#define mix_minor31 pad[15] *= pad[0] ^ 0x48ad05f2
#define mix_minor32 pad[16] -= pad[4] - 0xbb834311
#define mix_minor33 pad[8] += ROL (pad[4], 26)
#define mix_minor34 pad[13] *= pad[18] + 0xac048a2
#define mix_minor35 pad[16] &= pad[18] + 0xe832eb88
#define mix_minor36 pad[4] -= pad[1] - 0xe6f17893
#define mix_minor37 pad[6] *= pad[7] | 0x17b60bb5
#define mix_minor38 pad[15] += ROL (pad[12], 16)
#define mix_minor39 pad[6] &= pad[10] + 0xfd7af7e
#define mix_minor40 ROREQ (pad[7], pad[18] & 2)
#define mix_minor41 ROREQ (pad[17], pad[7] ^ 3)
#define mix_minor42 pad[0] ^= pad[8] + 0xeee530d5
#define mix_minor43 pad[10] += pad[1] + 0xc484cfa2
#define mix_minor44 pad[16] += pad[5] ^ 0x19a836dc
#define mix_minor45 pad[17] += pad[7] + 0xd68a11c3
#define mix_minor46 pad[17] += ROL (pad[7], 19)
#define mix_minor47 pad[18] -= pad[6] * 0x368eaf4e
#define mix_minor48 ROREQ (pad[2], pad[7] ^ 3)
#define mix_minor49 pad[19] |= pad[5] + 0xda7c6c8e
#define mix_minor50 pad[6] *= ROR (pad[2], 12)
#define mix_minor51 pad[14] += pad[18] + 0xf655a040
#define mix_minor52 pad[11] += pad[19] * 0x251df1bd
#define mix_minor53 pad[11] -= pad[0] ^ 0x51a859c
#define mix_minor54 pad[18] += pad[6] + 0xdcccfc5
#define mix_minor55 pad[16] -= pad[18] ^ 0x39848960
#define mix_minor56 pad[14] ^= pad[19] + 0x1a6f3b29
#define mix_minor57 pad[12] &= pad[5] + 0x4ef1335a
#define mix_minor58 pad[14] *= pad[13] + 0xdb61abf8
#define mix_minor59 pad[18] ^= pad[19] * 0x378f67
#define mix_minor60 pad[18] ^= pad[4] * 0x2dd2a2fe
#define mix_minor61 pad[16] -= pad[4] - 0xe357b476
#define mix_minor62 pad[6] *= pad[16] * 0x381203
#define mix_minor63 pad[10] |= ROR (pad[11], 24)
#define mix_minor64 pad[10] ^= pad[5] + 0x147c80d5
#define mix_minor65 pad[16] ^= pad[3] * 0x27139980
#define mix_minor66 ROREQ (pad[15], pad[17])
#define mix_minor67 pad[14] &= ROL (pad[19], 6)
#define mix_minor68 pad[8] *= pad[0] * 0x1a4c02dd
#define mix_minor69 pad[16] ^= pad[14] + 0xfddb63a2

#define ROR(value, count) ((value) >> ((count) & 0x1f) | ((value) << (32 - (((count) & 0x1f)))))
#define ROL(value, count) ((value) << ((count) & 0x1f) | ((value) >> (32 - (((count) & 0x1f)))))

#define ROREQ(value, count) value = ROR(value, count)
#define ROLEQ(value, count) value = ROL(value, count)

void mix_major0 (u32 *pad, u32 seed)
{
	int branch = (pad[15] ^ pad[19] ^ seed) % 11;

	pad[6] *= pad[8] * 0x1bb4a70d;
	pad[12] += pad[14] + 0xe087bd96;
	seed &= pad[13] | 0x39367989;

	if (branch == 7)
	{
		mix_minor30;
		mix_minor41;
		mix_minor45;
		mix_major3 (pad, pad[19]);
	}

	pad[14] -= ROR (seed, 31);
	pad[8] ^= seed & 0x8e30c76;
	pad[3] *= pad[12] ^ 0xd05f635;
	pad[10] += pad[10] + 0xa92dc43a;

	if (branch == 0)
	{
		pad[0] += 0xde3b3b9a;
		mix_minor46;
		pad[3] += 0x8600800;
		mix_major14 (pad, pad[5]);
	}

	seed += pad[17] + 0xff92b824;
	pad[1] += pad[3] ^ 0x62c448c0;
	pad[8] ^= pad[3] ^ 0x43c25efd;
	pad[9] ^= ROL (pad[9], 16);

	if (branch == 5)
	{
		mix_minor53;
		pad[3] += 0x8502040;
		mix_minor54;
		mix_major9 (pad, pad[5]);
	}

	pad[3] -= pad[2] - 0xef553b21;
	pad[18] += pad[13] + 0x3b26991e;

	if (branch == 4)
	{
		mix_minor39;
		mix_minor42;
		mix_minor35;
		mix_major10 (pad, pad[7]);
	}

	pad[12] += pad[11] & 0x4be050d;
	pad[17] ^= ROR (seed, 8);
	ROREQ (pad[8], pad[16] + 0x17);
	pad[12] *= pad[8] + 0xf3910fa;

	if (branch == 2)
	{
		mix_minor58;
		mix_minor59;
		mix_minor38;
		mix_major2 (pad, seed);
	}

	pad[8] += seed + 0x4088eb5f;
	pad[5] &= pad[7] ^ 0x1387a250;
	pad[2] |= pad[1] ^ 0x47f3a78b;
	pad[17] |= pad[10] * 0x1d208465;

	if (branch == 1)
	{
		mix_minor39;
		mix_minor49;
		mix_minor50;
		mix_major7 (pad, pad[9]);
	}

	pad[1] -= seed & 0x4be5deac;
	pad[4] += pad[15] & 0x3496b61a;

	if (branch == 10)
	{
		mix_minor55;
		mix_minor61;
		pad[8] += 0x82e5ca1;
		mix_major21 (pad, pad[8]);
	}

	ROREQ (seed, seed * 0x10);
	pad[13] &= pad[12] + 0x6b465da;

	if (branch == 3)
	{
		mix_minor27;
		pad[8] += 0x370c574;
		pad[0] += 0xc484fc90;
		mix_major13 (pad, pad[11]);
	}

	pad[16] |= pad[14] + 0xff7068bf;
	pad[7] &= pad[19] ^ 0x1e569f2b;
	pad[12] += pad[15] * 0x49f90b6a;

	if (branch == 6)
	{
		pad[17] ^= 0x8ade6faa;
		mix_minor26;
		mix_minor59;
		mix_major24 (pad, pad[7]);
	}

	pad[6] -= pad[18] * 0xb0223a7;
	pad[19] -= pad[4] * 0x4f4bc59;
	pad[17] += pad[3] + 0x19da7ccb;
	pad[17] -= seed & 0x3a423827;

	if (branch == 9)
	{
		mix_minor67;
		mix_minor29;
		pad[3] += 0x506840;
		mix_major23 (pad, seed);
	}

	seed += pad[11] + 0xea268d79;
	seed ^= pad[11] + 0x7b41453;

	if (branch == 8)
	{
		pad[11] += 0xe199e061;
		mix_minor34;
		mix_minor30;
		mix_major1 (pad, seed);
	}

	pad[0] ^= pad[2] ^ 0x361eddb9;
	pad[0] += seed + 0xc3201c46;
	ROREQ (pad[4], pad[4] + 0x19);
	pad[8] *= pad[16] + 0xf6c0ea7;
	ROREQ (pad[11], pad[18] * 0x13);
	pad[2] |= pad[4] | 0x5747f7c;
	seed ^= pad[3] * 0x336a3c4f;
	pad[9] ^= pad[8] + 0x5ff3732;
	pad[9] ^= seed + 0x2b702a62;
	pad[1] *= pad[1] + 0xfa4e2f52;
}

void mix_major1 (u32 *pad, u32 seed)
{
	int branch = (pad[6] ^ pad[9] ^ pad[12]) % 11;

	pad[5] += pad[3] & 0x24398ab;
	seed += pad[3] - pad[18] + 0x45e6c9d4;

	if (branch == 2)
	{
		mix_minor26;
		mix_minor58;
		mix_minor67;
		mix_major5 (pad, pad[19]);
	}

	seed ^= pad[14] + 0xc0fd80ba;
	pad[12] -= pad[1] * 0xe99b672;
	pad[15] ^= pad[0] + 0xca70bf60;

	if (branch == 1)
	{
		pad[4] ^= 0x15e7d1d6;
		mix_minor44;
		mix_minor26;
		mix_major3 (pad, pad[11]);
	}

	seed += seed ^ 0x17339c6;
	pad[15] += pad[7] * 0x15f0a011;
	pad[4] &= pad[17] + 0x1b597286;
	pad[17] *= pad[15] & 0x389e630b;

	if (branch == 3)
	{
		mix_minor44;
		mix_minor25;
		mix_minor63;
		mix_major14 (pad, pad[3]);
	}

	pad[18] ^= pad[19] ^ 0x31a138ce;
	pad[16] &= seed * 0x271fe1f1;
	ROREQ (pad[7], pad[16] ^ 0x9);

	if (branch == 5)
	{
		pad[12] += 0x108440;
		pad[14] -= 0xf9b7e88d;
		mix_minor64;
		mix_major9 (pad, seed);
	}

	pad[2] *= ROR (pad[6], 31);
	seed -= pad[14] - 0xfee822a8;
	seed *= pad[5] * 0x9dfbe4;
	seed += pad[13] + 0xfd2ead2f;

	if (branch == 9)
	{
		mix_minor60;
		mix_minor42;
		pad[14] += 0x723398ff;
		mix_major10 (pad, pad[18]);
	}

	pad[7] += pad[17] + 0x2b29baf9;
	ROREQ (pad[2], ROL (pad[0], 25));

	if (branch == 6)
	{
		mix_minor46;
		ROLEQ (pad[19], 18);
		mix_minor41;
		mix_major2 (pad, pad[4]);
	}

	pad[12] &= pad[16] + 0x2223fa4b;
	seed -= pad[5] * 0x282f40d5;
	seed &= ROR (pad[18], 16);

	if (branch == 8)
	{
		mix_minor46;
		pad[9] += 0xd0b27d9c;
		ROLEQ (pad[10], 22);
		mix_major7 (pad, seed);
	}

	pad[17] += pad[7] + 0xf9ac8515;
	pad[7] += pad[10] + 0xf9b69577;

	if (branch == 4)
	{
		pad[3] *= 0x2da1cfcf;
		mix_minor54;
		pad[12] += 0x80410;
		mix_major21 (pad, pad[13]);
	}

	pad[7] += pad[13] ^ 0x6d56f7f;
	pad[8] -= seed - 0x8c8d3d9c;

	if (branch == 7)
	{
		mix_minor24;
		mix_minor33;
		mix_minor42;
		mix_major13 (pad, pad[4]);
	}

	pad[5] -= pad[12] - 0x4d2bd380;
	pad[1] -= seed - 0xfcee8aad;
	pad[18] *= pad[1] * 0x696c0;
	pad[8] *= pad[4] + 0xdc2745dc;

	if (branch == 10)
	{
		mix_minor58;
		mix_minor25;
		mix_minor36;
		mix_major24 (pad, pad[2]);
	}

	pad[18] ^= pad[7] + 0xd9de0ed7;
	ROREQ (pad[11], pad[6] + 0x11);
	pad[19] += pad[18] + 0xb295dc;

	if (branch == 0)
	{
		mix_minor41;
		mix_minor30;
		mix_minor64;
		mix_major23 (pad, seed);
	}

	pad[15] -= seed | 0x58eafd;
	ROREQ (pad[5], pad[12] * 0x6);
	pad[2] += pad[19] + 0xf42fd441;
	pad[12] *= pad[2] | 0x10d913b8;
	pad[1] ^= pad[11] + 0x2039d1f9;
	pad[15] += ROR (pad[2], 3);
	pad[11] += pad[1] + 0x55f96491;
	pad[4] *= pad[15] & 0x864fe18;
	pad[18] *= pad[18] + 0xf5eb4571;
}

void mix_major2 (u32 *pad, u32 seed)
{
	int branch = pad[9] % 11;

	pad[0] |= seed | 0x4d9f89df;
	seed -= seed & 0x10691818;
	pad[15] &= ROR (pad[15], 18);

	if (branch == 2)
	{
		mix_minor63;
		mix_minor62;
		mix_minor43;
		mix_major12 (pad, pad[9]);
	}

	seed |= pad[7] ^ 0x1f11181f;
	ROREQ (pad[17], pad[18] + 0x18);
	pad[3] &= pad[18] + 0xc18379a4;
	pad[8] += pad[2] + 0x8845990;

	if (branch == 8)
	{
		ROLEQ (pad[10], 6);
		mix_minor64;
		mix_minor69;
		mix_major18 (pad, seed);
	}

	seed *= ROL (pad[7], 30);
	ROREQ (pad[14], seed ^ 0x1);
	pad[3] -= pad[3] ^ 0x1a11c1c;

	if (branch == 3)
	{
		mix_minor48;
		pad[3] -= 0x833e3d40;
		mix_minor61;
		mix_major6 (pad, seed);
	}

	pad[5] += seed + 0xbdf50793;
	seed -= pad[6] ^ 0x341c6ce5;
	seed ^= pad[14] | 0x11712ba;
	pad[4] -= seed - 0x1df0f08c;

	if (branch == 4)
	{
		mix_minor44;
		mix_minor41;
		mix_minor64;
		mix_major4 (pad, pad[2]);
	}

	seed *= pad[15] + 0xd8a810b1;
	pad[0] -= pad[7] - 0x8e4e3c5;
	pad[9] -= seed ^ 0x13f1a8da;

	if (branch == 7)
	{
		mix_minor20;
		mix_minor47;
		mix_minor57;
		mix_major11 (pad, pad[12]);
	}

	pad[14] ^= seed + 0xf2dd8a98;
	pad[14] |= pad[3] & 0xb51383c;

	if (branch == 1)
	{
		mix_minor36;
		mix_minor27;
		mix_minor47;
		mix_major22 (pad, pad[9]);
	}

	pad[0] -= pad[2] - 0x16bda446;
	pad[2] -= pad[0] ^ 0x3576dfb9;

	if (branch == 9)
	{
		mix_minor26;
		mix_minor32;
		mix_minor49;
		mix_major5 (pad, pad[9]);
	}

	pad[5] -= pad[6] | 0x1720cf3;
	pad[16] ^= pad[19] ^ 0x2dfed60;
	seed *= pad[12] + 0xffcf5d22;
	seed += pad[11] ^ 0x26b4296;

	if (branch == 6)
	{
		mix_minor55;
		ROLEQ (pad[19], 15);
		ROLEQ (pad[10], 26);
		mix_major3 (pad, seed);
	}

	ROREQ (seed, ROL (seed, 11));
	seed -= ROL (pad[17], 25);
	pad[4] += pad[3] ^ 0x125c14db;

	if (branch == 0)
	{
		pad[14] += 0x7de14a07;
		pad[4] *= 0x13ca26ac;
		mix_minor41;
		mix_major14 (pad, pad[9]);
	}

	pad[9] |= ROR (pad[15], 31);
	seed -= pad[19] - 0xfde54451;
	ROREQ (pad[9], pad[11] | 0x3);
	pad[18] ^= seed ^ 0x22da8ee3;

	if (branch == 10)
	{
		mix_minor46;
		mix_minor28;
		mix_minor69;
		mix_major9 (pad, pad[2]);
	}

	ROREQ (pad[2], pad[9] + 0xf);
	ROREQ (pad[5], pad[18] & 0x13);
	pad[5] -= pad[1] ^ 0x2822999;

	if (branch == 5)
	{
		pad[12] += 0x108072;
		pad[8] += 0xaf45f1d7;
		mix_minor61;
		mix_major10 (pad, pad[9]);
	}

	pad[1] += ROR (seed, 24);
	pad[6] += pad[4] | 0x161d3ea;
	pad[9] += seed + 0xc2e590c;
	seed -= seed ^ 0x125deacd;
	pad[7] &= pad[17] ^ 0x10b015bf;
	pad[17] = 0x1bb396c0;
	pad[9] *= pad[1] & 0x7a04e3e;
	seed += pad[16] | 0x16cf1fa2;
	seed *= pad[14] * 0x1d5ac40e;
	pad[4] ^= seed + 0xf27819a7;
}

void mix_major3 (u32 *pad, u32 seed)
{
	int branch = (pad[10] ^ pad[16] ^ seed) % 11;

	pad[12] *= pad[3] & 0x19997dc0;
	seed |= pad[0] + 0xd31e211;
	pad[14] -= pad[0] - 0x7cfa160;

	if (branch == 10)
	{
		mix_minor48;
		mix_minor46;
		pad[14] -= 0x7f80fb4a;
		mix_major17 (pad, pad[2]);
	}

	pad[13] = ROR (pad[13], seed + 0x6);
	pad[3] *= pad[12] + 0xfd1d773c;

	if (branch == 3)
	{
		mix_minor37;
		mix_minor39;
		pad[17] ^= 0x1d4f264d;
		mix_major16 (pad, pad[12]);
	}

	seed |= seed + 0xd10c7a44;
	seed += pad[0] + 0xf3754e81;
	seed ^= pad[16] ^ 0x21d2a427;

	if (branch == 1)
	{
		mix_minor38;
		pad[12] += 0x208846a;
		mix_minor31;
		mix_major15 (pad, pad[11]);
	}

	pad[16] |= pad[4] | 0x599c0b2;
	ROLEQ (seed, pad[0] + 0x1d);
	pad[3] &= pad[6] ^ 0x1d86d59a;
	pad[0] ^= pad[10] ^ 0x22d79e78;

	if (branch == 9)
	{
		mix_minor45;
		ROLEQ (pad[16], 14);
		mix_minor45;
		mix_major8 (pad, pad[16]);
	}

	ROLEQ (pad[15], pad[9] + 0x2);
	seed += ROL (seed, 13) + (pad[4] ^ 0x17568f8b);
	pad[3] -= pad[9] ^ 0x1b7d211b;

	if (branch == 7)
	{
		mix_minor35;
		pad[14] ^= 0x7adc7a3f;
		mix_minor64;
		mix_major12 (pad, seed);
	}

	pad[14] *= pad[10] ^ 0x25da4024;
	pad[3] += pad[19] ^ 0x195596e2;

	if (branch == 8)
	{
		mix_minor37;
		mix_minor60;
		pad[8] -= 0x75c7234e;
		mix_major18 (pad, seed);
	}

	pad[3] ^= ROR (pad[4], 11);
	pad[19] ^= pad[2] & 0x142c74fa;
	pad[7] = 0x3de4cf2b;
	seed ^= pad[5] * 0x1195dbf3;

	if (branch == 5)
	{
		mix_minor51;
		mix_minor51;
		mix_minor28;
		mix_major6 (pad, pad[9]);
	}

	pad[12] *= pad[14] * 0x25bf72d4;
	seed += ROL (pad[11], 2);

	if (branch == 0)
	{
		mix_minor28;
		mix_minor20;
		mix_minor41;
		mix_major4 (pad, seed);
	}

	pad[7] += seed + 0xfbd89057;
	pad[12] -= seed - 0xfec898a3;
	seed *= seed + 0xe6d9d0ce;
	pad[2] *= pad[0] * 0x25d5927e;

	if (branch == 6)
	{
		mix_minor41;
		mix_minor35;
		pad[0] += 0x8a388c73;
		mix_major11 (pad, pad[3]);
	}

	seed -= seed ^ 0x7951f14a;
	seed *= pad[19] ^ 0x159fa550;
	pad[9] -= seed * 0x1b0d12a6;

	if (branch == 4)
	{
		mix_minor58;
		mix_minor26;
		mix_minor26;
		mix_major22 (pad, seed);
	}

	pad[12] += pad[4] | 0xf2ff1db;
	pad[12] ^= pad[2] & 0xac8676c;
	pad[7] -= seed * 0x1a41598b;
	pad[17] *= pad[14] & 0x36ff2c0;

	if (branch == 2)
	{
		mix_minor60;
		mix_minor67;
		mix_minor44;
		mix_major5 (pad, pad[11]);
	}

	pad[19] ^= pad[11] + 0xe311654d;
	pad[18] ^= pad[16] * 0x1267cd78;
	pad[16] &= seed ^ 0x1c8b2015;
	pad[16] *= pad[16] | 0xc26f29a;
	pad[14] &= pad[10] + 0xcec46d19;
	pad[8] -= pad[8] ^ 0xc03874d;
}

void mix_major4 (u32 *pad, u32 seed)
{
	int branch = (pad[2] ^ pad[15] ^ seed) % 9;

	pad[14] += pad[15] + 0xd3892fe6;
	pad[2] -= seed - 0xe600fde6;
	pad[15] ^= pad[4] + 0x385e38e;
	pad[18] |= seed + 0xc6189f52;

	if (branch == 7)
	{
		mix_minor43;
		mix_minor24;
		pad[3] += 0x9302800;
		mix_major19 (pad, pad[1]);
	}

	pad[11] &= pad[14] + 0x8f6f81a9;
	ROLEQ (pad[12], ROR (pad[6], 14));
	seed -= ROL (pad[8], 14);
	ROREQ (pad[0], ROR (pad[11], 1));
	pad[0] += pad[11] ^ 0x43cd4d14;

	if (branch == 3)
	{
		mix_minor22;
		mix_minor48;
		pad[4] *= 0x2a2e8718;
		mix_major20 (pad, pad[14]);
	}

	pad[3] -= pad[8] ^ 0x155c464;
	pad[16] += pad[0] + 0xf8d647b6;
	pad[2] ^= pad[4] ^ 0x11e3788d;

	if (branch == 5)
	{
		pad[5] += 0xc4115253;
		mix_minor51;
		mix_minor55;
		mix_major17 (pad, pad[10]);
	}

	seed |= ROL (seed, 11);
	pad[5] &= seed ^ 0x16984b90;
	pad[16] += ROR (seed, 29);
	pad[0] += pad[15] + 0xc3e56f16;

	if (branch == 2)
	{
		mix_minor46;
		ROLEQ (pad[16], 7);
		mix_minor40;
		mix_major16 (pad, pad[1]);
	}

	pad[5] &= pad[11] + 0xe57356e7;
	pad[18] -= seed ^ 0x23f157f6;
	seed -= pad[18] & 0x155b7cc8;

	if (branch == 1)
	{
		mix_minor48;
		pad[5] += 0x6d08d06;
		mix_minor50;
		mix_major15 (pad, seed);
	}

	pad[8] |= pad[5] | 0x21496d22;
	seed -= pad[18] - 0x93b1543f;
	pad[14] *= seed * 0x1db47609;
	ROREQ (pad[7], pad[10] ^ 0x1a);
	ROLEQ (pad[7], pad[18] + 0x1c);

	if (branch == 0)
	{
		mix_minor46;
		ROLEQ (pad[10], 4);
		mix_minor59;
		mix_major8 (pad, seed);
	}

	pad[8] ^= ROL (pad[5], 3);
	pad[6] ^= seed ^ 0x2c8ca15;
	pad[13] += ROL (seed, 13);

	if (branch == 4)
	{
		mix_minor61;
		pad[3] *= 0x6c0de9fa;
		mix_minor34;
		mix_major12 (pad, pad[18]);
	}

	pad[17] ^= pad[2] & 0xa0962e5;
	pad[3] *= seed & 0xd505f52;
	seed -= pad[15] ^ 0x15284f42;

	if (branch == 8)
	{
		mix_minor37;
		mix_minor47;
		pad[12] += 0x2108058;
		mix_major18 (pad, pad[8]);
	}

	pad[7] &= pad[2] + 0xf8df2963;
	pad[6] *= seed * 0x256b9c9c;
	pad[10] += pad[1] | 0xda16d9b;
	pad[9] *= pad[5] ^ 0x28b62e0c;

	if (branch == 6)
	{
		mix_minor35;
		pad[14] ^= 0x8a0974b;
		mix_minor37;
		mix_major6 (pad, seed);
	}

	pad[12] ^= pad[5] * 0x23779c9e;
	pad[10] *= ROR (pad[19], 29);
	pad[0] ^= pad[10] ^ 0x38a5f94;
	seed += pad[15] + 0x1c82e95e;
	ROLEQ (pad[9], pad[5] ^ 0x1d);
	pad[12] += seed + 0xc0e4fa7d;
	pad[17] ^= pad[7] ^ 0x141bbf98;
	pad[9] ^= ROR (pad[18], 6);
	pad[4] -= pad[13] & 0x2373fe39;
	pad[19] += ROL (seed, 15);
}

void mix_major5 (u32 *pad, u32 seed)
{
	int branch = pad[18] % 11;

	pad[5] |= pad[17] * 0x2e7a089;
	pad[3] ^= pad[13] + 0x1fef7de0;
	seed -= pad[16] ^ 0x8338b85;

	if (branch == 0)
	{
		pad[3] += 0x1000800;
		pad[3] += 0x9102040;
		mix_minor30;
		mix_major20 (pad, pad[11]);
	}

	seed *= 0x1cd19bfb;
	pad[3] *= pad[12] + 0x15bdbb56;
	pad[11] ^= seed + 0x374580a7;
	pad[10] += seed | 0x86941f3;

	if (branch == 4)
	{
		mix_minor32;
		ROLEQ (pad[10], 25);
		mix_minor50;
		mix_major17 (pad, pad[18]);
	}

	pad[6] -= pad[16] ^ 0x11119dd6;
	pad[13] += pad[18] + 0xcb82c76c;
	pad[8] -= pad[1] ^ 0x3b98ae58;

	if (branch == 9)
	{
		mix_minor42;
		mix_minor64;
		mix_minor21;
		mix_major16 (pad, pad[1]);
	}

	pad[17] ^= pad[17] + 0xcfd5283;
	pad[5] &= pad[13] + 0x539ef62;
	pad[11] &= pad[14] ^ 0x639b87fe;

	if (branch == 8)
	{
		mix_minor22;
		mix_minor59;
		pad[14] += 0x73204792;
		mix_major15 (pad, pad[18]);
	}

	pad[12] -= seed | 0x369e02e;
	pad[6] *= pad[12] + 0xf0544c52;
	seed += pad[5] + 0x8dcb06;
	pad[12] -= seed & 0x632ffca;

	if (branch == 3)
	{
		pad[5] += 0xc6ac8583;
		mix_minor41;
		pad[3] += 0x9004000;
		mix_major8 (pad, pad[17]);
	}

	pad[16] -= pad[6] * 0x345114ef;
	ROREQ (pad[10], pad[11] * 0x10);
	pad[0] += pad[4] & 0x18b74e25;

	if (branch == 7)
	{
		pad[12] += 0x1a;
		mix_minor53;
		mix_minor47;
		mix_major12 (pad, pad[15]);
	}

	pad[2] -= pad[2] ^ 0x18f1b56;
	ROLEQ (pad[19], pad[13] + 0x6);

	if (branch == 6)
	{
		pad[3] *= 0x27d3a148;
		pad[4] *= 0xa24016a8;
		pad[14] -= 0x3a2c78cf;
		mix_major18 (pad, seed);
	}

	ROREQ (seed, pad[7] + 0x16);
	ROLEQ (seed, pad[14] + 0x11);

	if (branch == 5)
	{
		pad[3] *= 0x3713ed22;
		mix_minor29;
		mix_minor59;
		mix_major6 (pad, pad[7]);
	}

	seed -= pad[10] - 0xd26e6435;
	ROLEQ (pad[8], pad[13] ^ 0x15);
	pad[1] += pad[10] ^ 0x1da5a5e2;

	if (branch == 2)
	{
		mix_minor45;
		mix_minor55;
		ROLEQ (pad[16], 13);
		mix_major4 (pad, pad[10]);
	}

	pad[7] |= seed * 0x1665683f;
	pad[6] += pad[17] + 0xd3198985;
	seed &= pad[1] * 0xb2490cd;

	if (branch == 1)
	{
		mix_minor50;
		mix_minor45;
		pad[8] += 0x749a003b;
		mix_major11 (pad, pad[6]);
	}

	pad[13] -= pad[3] ^ 0x49caa386;
	pad[5] -= pad[7] - 0xca44ad;
	seed += pad[14] | 0xce2b27d;

	if (branch == 10)
	{
		mix_minor33;
		mix_minor22;
		pad[8] -= 0x7a3a25c3;
		mix_major22 (pad, pad[11]);
	}

	pad[15] += pad[6] + 0x9f72b74b;
	pad[16] -= pad[3] - 0xaa1914c0;
	seed -= ROL (seed, 18);
	pad[14] ^= pad[9] ^ 0x7a9f2d9;
	pad[19] &= ROL (pad[3], 10);
	seed *= pad[15] * 0xd49e9d9;
	pad[4] += pad[2] ^ 0xc52d715;
	pad[15] *= pad[11] * 0x300c07b6;
	pad[4] ^= seed * 0x59c5268;
	pad[7] -= seed - 0xf1ae26ce;
}

void mix_major6 (u32 *pad, u32 seed)
{
	int branch = (pad[3] ^ pad[5] ^ pad[18]) & 7;

	pad[7] ^= pad[5] ^ 0x3610ff4;
	pad[18] ^= ROL (pad[14], 19);
	ROREQ (pad[15], pad[10] + 0xe);
	seed ^= pad[1] + 0xa89a8207;
	seed &= 0xecc2fa7d;

	if (branch == 0)
	{
		mix_minor54;
		pad[4] *= 0x5141d713;
		mix_minor62;
		mix_major19 (pad, seed);
	}

	pad[15] ^= pad[0] * 0x19dd786;
	seed *= ROL (seed, 12);
	pad[17] &= seed | 0x1249d1c;
	pad[15] ^= pad[8] + 0x5e67551f;
	seed += pad[0] * 0x320ea6ec;
	seed ^= pad[19] + 0xee10c43d;

	if (branch == 1)
	{
		ROLEQ (pad[19], 6);
		pad[3] += 0x1600840;
		mix_minor41;
		mix_major20 (pad, pad[2]);
	}

	ROREQ (pad[15], seed ^ 0x7);
	pad[5] -= pad[14] * 0x54cc1685;
	pad[12] -= seed - 0xf7d8f2fa;
	pad[5] -= pad[10] - 0xf95da87e;
	seed ^= ROL (pad[8], 18);

	if (branch == 5)
	{
		mix_minor53;
		mix_minor23;
		mix_minor60;
		mix_major17 (pad, pad[16]);
	}

	pad[19] += pad[2] ^ 0x4983faaa;
	seed &= pad[6] & 0x911ab6a;
	pad[17] &= pad[2] + 0xfbb4acd7;
	pad[5] += pad[13] + 0xf96465d3;
	ROLEQ (seed, pad[2] | 0x19);
	pad[9] += pad[2] | 0x176f7fa2;

	if (branch == 7)
	{
		mix_minor53;
		mix_minor67;
		mix_minor38;
		mix_major16 (pad, seed);
	}

	ROREQ (pad[4], seed + 0x10);
	seed |= pad[6] ^ 0x1ae616e0;
	seed ^= pad[15] * 0x7f034;
	ROREQ (pad[14], pad[2] + 0x3);

	if (branch == 6)
	{
		pad[14] -= 0xa630c9b5;
		mix_minor33;
		pad[8] -= 0xa8a2e592;
		mix_major15 (pad, seed);
	}

	pad[12] -= pad[10] & 0x1311b0aa;
	pad[14] ^= seed + 0xf5736e40;
	pad[17] += ROR (pad[18], 15);
	seed ^= pad[11] + 0x25e8d98c;
	ROLEQ (pad[0], pad[14] | 0x8);
	pad[13] -= pad[3] ^ 0x2a68c40c;

	if (branch == 3)
	{
		mix_minor40;
		pad[14] ^= 0x4e96c3d9;
		pad[3] *= 0x7b9dddda;
		mix_major8 (pad, pad[3]);
	}

	pad[12] += ROR (seed, 12);
	ROLEQ (pad[7], pad[6] & 0x9);
	ROLEQ (seed, seed * 0x4);
	ROREQ (seed, pad[16] ^ 0x4);
	ROLEQ (pad[6], ROL (pad[1], 11));

	if (branch == 4)
	{
		mix_minor31;
		mix_minor31;
		ROLEQ (pad[16], 28);
		mix_major12 (pad, pad[13]);
	}

	pad[14] += ROR (pad[14], 9);
	pad[3] *= pad[13] & 0x24b1abab;
	pad[3] -= pad[12] - 0x10decc67;
	seed *= pad[15] ^ 0x194903b4;

	if (branch == 2)
	{
		mix_minor33;
		mix_minor48;
		mix_minor66;
		mix_major18 (pad, pad[14]);
	}

	pad[15] *= seed * 0x2ed0158e;
	pad[14] += pad[3] + 0xc4d28c7c;
	pad[11] -= pad[18] ^ 0x3e1bda7e;
	pad[2] *= ROL (pad[13], 24);
}

void mix_major7 (u32 *pad, u32 seed)
{
	int branch = (pad[3] ^ pad[6] ^ seed) % 11;

	pad[8] += seed * 0x25d21c70;
	seed += ROR (pad[13], 26);
	pad[15] += ROR (pad[0], 18);

	if (branch == 1)
	{
		mix_minor45;
		ROLEQ (pad[16], 24);
		ROLEQ (pad[16], 18);
		mix_major18 (pad, pad[9]);
	}

	seed += pad[4] ^ 0x214bbbb;
	ROLEQ (pad[5], seed * 0x1d);
	pad[17] -= pad[18] | 0x1102e01a;
	pad[19] += pad[12] + 0xf1e0cc5a;

	if (branch == 0)
	{
		pad[4] *= 0x73b12006;
		mix_minor49;
		mix_minor43;
		mix_major6 (pad, pad[13]);
	}

	pad[8] |= seed * 0x33ff2ce9;
	pad[4] *= seed + 0x2fe45acf;
	pad[3] ^= ROR (seed, 13);
	seed ^= pad[12] & 0x2e2ac892;

	if (branch == 7)
	{
		mix_minor59;
		mix_minor69;
		mix_minor41;
		mix_major4 (pad, pad[15]);
	}

	seed *= ROL (pad[14], 1);
	seed ^= seed + 0x7a3b4f0e;
	pad[5] += pad[11] ^ 0x5f050ce6;

	if (branch == 2)
	{
		mix_minor46;
		mix_minor45;
		mix_minor24;
		mix_major11 (pad, pad[17]);
	}

	pad[9] -= pad[11] & 0x524788df;
	seed += pad[3] + ROR (pad[17], 18) + 0x17b2d86;
	pad[12] |= pad[17] ^ 0xd2348b5;

	if (branch == 4)
	{
		mix_minor60;
		mix_minor56;
		mix_minor64;
		mix_major22 (pad, seed);
	}

	pad[4] += pad[0] ^ 0x3ca6760a;
	ROREQ (pad[10], pad[12] & 0x1e);
	pad[12] -= seed ^ 0x32b59495;
	pad[11] -= pad[7] ^ 0xcc6cef3;

	if (branch == 9)
	{
		mix_minor51;
		mix_minor51;
		mix_minor46;
		mix_major5 (pad, pad[6]);
	}

	pad[18] -= seed ^ 0x42ce4263;
	pad[8] ^= pad[15] + 0xfc1ccf0a;
	pad[4] *= pad[2] + 0xdc6ebf0;

	if (branch == 8)
	{
		mix_minor34;
		mix_minor33;
		mix_minor35;
		mix_major3 (pad, pad[19]);
	}

	pad[14] ^= pad[17] + 0x29e0bfe6;
	pad[2] ^= pad[0] + 0xc0a98770;
	pad[6] += ROL (pad[11], 15);

	if (branch == 10)
	{
		mix_minor43;
		mix_minor38;
		ROLEQ (pad[10], 8);
		mix_major14 (pad, seed);
	}

	seed += pad[18] - pad[5] + 0xff5138a0;

	if (branch == 6)
	{
		ROLEQ (pad[19], 22);
		mix_minor32;
		pad[14] ^= 0x3ccf037;
		mix_major9 (pad, seed);
	}

	ROREQ (pad[3], pad[17] & 0xa);
	pad[3] ^= pad[7] * 0x36e7ec8;

	if (branch == 3)
	{
		mix_minor41;
		pad[17] ^= 0xeeea146c;
		mix_minor64;
		mix_major10 (pad, seed);
	}

	ROREQ (pad[10], pad[19] * 0x19);
	pad[14] *= pad[12] + 0xd914afe4;

	if (branch == 5)
	{
		mix_minor22;
		mix_minor57;
		mix_minor44;
		mix_major2 (pad, pad[18]);
	}

	pad[8] -= pad[7] ^ 0x1609874e;
	pad[10] ^= pad[4] | 0x1e171635;
	pad[6] -= pad[16] - 0x19b93371;
}

void mix_major8 (u32 *pad, u32 seed)
{
	int branch = (pad[5] ^ pad[9] ^ pad[19]) % 5;

	pad[5] += pad[12] ^ 0xb6b4743;
	seed *= pad[12] + 0x221bed03;
	pad[3] *= pad[11] ^ 0x2663a394;
	seed |= pad[4] ^ 0x4f1894;
	pad[5] &= 0xad85e5da;
	pad[17] &= seed + 0xd191e790;
	seed += pad[1] * 0x1c634b75;

	if (branch == 2)
	{
		mix_minor34;
		mix_minor25;
		pad[8] -= 0x3dcc78c5;
		mix_major19 (pad, pad[1]);
	}

	pad[3] -= pad[6] ^ 0x1fdc8171;
	pad[15] ^= pad[14] * 0xdc63a30;
	ROLEQ (pad[7], seed + 0x8);
	seed ^= pad[8] + 0xe4fb2084;
	pad[6] -= seed - 0xb6a8bfd8;
	seed *= ROR (pad[6], 1);
	pad[13] *= ROR (pad[8], 31);
	pad[18] ^= pad[15] + 0xa969bc16;

	if (branch == 1)
	{
		mix_minor25;
		ROLEQ (pad[10], 14);
		mix_minor38;
		mix_major20 (pad, pad[2]);
	}

	ROREQ (seed, pad[1] & 0x6);
	pad[8] -= pad[17] - 0xeba05ea0;
	pad[16] += pad[19] + 0xe8427306;
	pad[16] ^= pad[7] + 0x35f9fb28;
	pad[13] += seed & 0x16076281;
	seed *= seed + 0xe43a6120;
	pad[1] -= pad[3] - 0xd94074d;

	if (branch == 3)
	{
		pad[4] *= 0xdccff951;
		mix_minor53;
		mix_minor41;
		mix_major17 (pad, pad[17]);
	}

	ROREQ (seed, pad[18] + 0x5);
	pad[6] += seed + 0x126c7192;
	pad[4] &= pad[9] ^ 0xe4c97d9;
	seed ^= seed + 0x5246092;
	pad[14] += pad[3] + 0x12466f7c;
	pad[7] -= pad[19] - 0xe724e487;
	seed -= pad[2] - 0xfffcc68a;
	pad[2] -= pad[12] * 0xf8b6e25;

	if (branch == 4)
	{
		mix_minor36;
		pad[17] ^= 0x5f26a27b;
		pad[14] ^= 0x77f49770;
		mix_major16 (pad, pad[12]);
	}

	ROLEQ (pad[3], pad[6] ^ 0x11);
	pad[4] += seed & 0x3dd7da06;
	pad[11] *= pad[8] + 0xb6484f2a;
	seed ^= pad[8] & 0x274e05b8;
	pad[18] ^= pad[5] + 0x263032a4;
	pad[16] ^= seed + 0x1a70ff38;

	if (branch == 0)
	{
		mix_minor37;
		mix_minor22;
		mix_minor45;
		mix_major15 (pad, pad[10]);
	}

	pad[4] += seed + 0x4a83a932;
	ROLEQ (pad[19], pad[2] + 0x10);
	ROLEQ (pad[0], pad[19]);
	seed += seed ^ 0x1bb7cdc3;
	seed -= pad[4] - 0xf1efd9b1;
	pad[11] ^= pad[1] | 0x64a30a;
	pad[1] -= pad[8] - 0x4cd3708;
	pad[0] += seed + 0xf6d388b6;
	pad[8] -= pad[1] - 0x4b8444f;
	ROLEQ (pad[7], ROL (seed, 9));
	ROREQ (pad[17], pad[10] + 0x1c);
}

void mix_major9 (u32 *pad, u32 seed)
{
	int branch = (pad[1] ^ pad[15] ^ pad[19]) % 11;

	pad[19] |= pad[18] + 0xe56713bc;
	pad[12] |= pad[8] + 0xefc639fe;

	if (branch == 2)
	{
		mix_minor40;
		mix_minor26;
		mix_minor65;
		mix_major15 (pad, seed);
	}

	pad[4] ^= pad[18] + 0xf20ff41d;
	ROLEQ (seed, seed + 0xb);

	if (branch == 5)
	{
		mix_minor28;
		mix_minor35;
		ROLEQ (pad[19], 20);
		mix_major8 (pad, seed);
	}

	pad[2] ^= ROR (seed, 1);
	pad[10] *= seed + 0x3842b736;

	if (branch == 4)
	{
		mix_minor21;
		mix_minor28;
		pad[3] -= 0x524e81e6;
		mix_major12 (pad, pad[13]);
	}

	pad[5] ^= pad[4] ^ 0x224deca3;
	pad[9] += pad[15] & 0xe43bfd6;
	pad[12] += pad[18] | 0x24e2f424;

	if (branch == 1)
	{
		mix_minor43;
		pad[8] += 0x6afab397;
		pad[11] += 0x573a6da7;
		mix_major18 (pad, pad[0]);
	}

	pad[11] *= pad[10] + 0xf0b1e409;
	pad[9] *= pad[5] + 0x13bcdf0b;
	pad[5] += 0x2961fc0;
	pad[6] *= pad[11] + 0xe91b219c;

	if (branch == 7)
	{
		mix_minor29;
		mix_minor41;
		mix_minor28;
		mix_major6 (pad, 0xefc5f81f);
	}

	pad[1] *= pad[1] + 0xff4abdb4;
	seed = 0x6f850fff;
	pad[13] += ROR (pad[10], 27);
	pad[10] += pad[3] + 0xea05fa03;

	if (branch == 10)
	{
		mix_minor24;
		mix_minor31;
		ROLEQ (pad[16], 20);
		mix_major4 (pad, seed);
	}

	pad[19] = (pad[19] + 0xe8b6d37d) - pad[2];
	seed ^= pad[12] * 0xa95c314;

	if (branch == 8)
	{
		mix_minor64;
		pad[4] ^= 0xa54ee16;
		mix_minor54;
		mix_major11 (pad, pad[11]);
	}

	seed += pad[11] & 0x346472bf;
	seed &= pad[15] * 0xbeb977c;
	seed += pad[2] ^ 0x33dd726a;
	pad[19] &= seed ^ 0x13220e;

	if (branch == 6)
	{
		mix_minor63;
		mix_minor54;
		mix_minor43;
		mix_major22 (pad, pad[2]);
	}

	seed *= seed + 0x13a371f7;
	ROLEQ (pad[0], seed * 0x2);
	ROLEQ (seed, pad[15] * 0xf);
	pad[12] += pad[11] | 0x15477725;

	if (branch == 3)
	{
		mix_minor56;
		pad[14] ^= 0x66bd03a9;
		mix_minor55;
		mix_major5 (pad, pad[9]);
	}

	pad[16] += pad[8] + 0xb2878320;
	pad[0] += pad[11] * 0x128142d3;
	ROREQ (pad[13], seed + 0x9);

	if (branch == 0)
	{
		mix_minor33;
		mix_minor69;
		mix_minor58;
		mix_major3 (pad, pad[17]);
	}

	ROREQ (pad[13], pad[4] + 0x1a);
	seed |= seed + 0xb401ddcd;
	ROREQ (seed, pad[16] + 0x17);
	seed += pad[11] ^ 0x14302fce;

	if (branch == 9)
	{
		mix_minor30;
		mix_minor47;
		pad[14] -= 0x979badcc;
		mix_major14 (pad, pad[17]);
	}

	pad[7] += pad[2] & 0x2104615d;
	pad[6] |= ROL (pad[4], 21);
	pad[16] -= seed * 0x144af0fa;
	seed ^= pad[9] * 0x1d7178c2;
	seed *= 0x3564b1fd;
	pad[16] -= ROR (seed, 11);
	pad[8] ^= pad[19] * 0x383ae479;
	pad[11] += seed + 0xc4759a85;
	pad[9] ^= pad[11] + 0x35e01882;
	pad[10] &= pad[0] ^ 0x105d6dd1;
}

void mix_major10 (u32 *pad, u32 seed)
{
	int branch = pad[5] % 11;

	pad[17] ^= seed + 0x2277a712;
	pad[19] *= pad[8] + 0xe6c6654e;
	ROREQ (pad[6], pad[1] ^ 0x1b);

	if (branch == 3)
	{
		pad[8] += 0x8c1d03c3;
		pad[4] ^= 0x112c3767;
		mix_minor43;
		mix_major8 (pad, pad[1]);
	}

	pad[0] *= seed + 0x22e5f53d;
	pad[6] -= pad[14] - 0xf7f0c308;

	if (branch == 6)
	{
		pad[1] &= 0x548aed34;
		mix_minor33;
		mix_minor28;
		mix_major12 (pad, pad[2]);
	}

	seed += pad[9] + 0xafa2e81;
	pad[15] *= pad[17] + 0xfd2839c0;
	pad[14] -= pad[6] - 0x30bd8dc6;
	pad[2] += pad[7] ^ 0x1edb75c4;

	if (branch == 4)
	{
		mix_minor67;
		mix_minor49;
		mix_minor29;
		mix_major18 (pad, seed);
	}

	pad[2] = 0x2cfa7327;
	pad[7] -= pad[8] - 0xf2bf5a7;
	ROREQ (pad[11], pad[6] | 0x15);
	pad[2] ^= ROL (pad[10], 24);

	if (branch == 2)
	{
		ROLEQ (pad[19], 19);
		mix_minor41;
		mix_minor26;
		mix_major6 (pad, seed);
	}

	pad[16] ^= ROL (pad[5], 29);
	ROLEQ (pad[8], ROL (pad[8], 19));

	if (branch == 0)
	{
		pad[8] += 0xabc0d876;
		pad[1] &= 0x2002d891;
		mix_minor51;
		mix_major4 (pad, seed);
	}

	pad[13] *= seed & 0x9aee05b;
	ROLEQ (pad[18], pad[0] + 0x9);

	if (branch == 5)
	{
		mix_minor37;
		mix_minor52;
		mix_minor65;
		mix_major11 (pad, pad[5]);
	}

	pad[16] += seed + 0x15c7f2a;
	pad[0] += pad[8] | 0xc568bd;
	seed += ROR (pad[11], 25);

	if (branch == 10)
	{
		mix_minor29;
		pad[14] += 0x7bef2ee1;
		mix_minor55;
		mix_major22 (pad, seed);
	}

	pad[11] &= pad[0] | 0x3c992378;
	seed ^= pad[2] ^ 0x1ebdf827;
	seed ^= pad[16] & 0x1a8092b;
	pad[4] ^= pad[2] + 0xf6a7c14d;

	if (branch == 7)
	{
		pad[3] += 0x706840;
		pad[3] += 0x1400840;
		mix_minor68;
		mix_major5 (pad, pad[5]);
	}

	seed |= pad[1] + 0xbd4eb37a;
	seed *= pad[15] ^ 0xe476c17;

	if (branch == 9)
	{
		mix_minor41;
		pad[14] += 0x52aaba85;
		mix_minor68;
		mix_major3 (pad, pad[19]);
	}

	pad[0] -= pad[4] & 0x55d63dde;
	pad[14] += pad[19] + 0xfa050d42;
	pad[12] &= pad[0] + 0x9ff4339;
	pad[15] ^= pad[12] + 0xccdc186;

	if (branch == 8)
	{
		mix_minor25;
		mix_minor31;
		mix_minor43;
		mix_major14 (pad, pad[12]);
	}

	ROREQ (pad[10], pad[11] + 0x1b);
	pad[5] ^= pad[15] + 0x130fea4;
	seed ^= pad[19] + 0xdf1438e7;

	if (branch == 1)
	{
		mix_minor20;
		mix_minor54;
		mix_minor35;
		mix_major9 (pad, seed);
	}

	pad[11] += pad[3] ^ 0x30f43d2;
	pad[13] -= pad[16] * 0x485950f;
	pad[15] *= pad[1] + 0xa295d0d;
	seed ^= pad[0] * 0x68f4b257;
	pad[12] &= pad[8] + 0xe49d7359;
	pad[7] -= pad[2] * 0x16a7a0b6;
	seed &= pad[13] + 0x18727e9f;
	pad[14] &= ROL (seed, 3);
	pad[19] -= pad[6] ^ 0x13892cf5;
}

void mix_major11 (u32 *pad, u32 seed)
{
	int branch = (pad[3] ^ pad[11] ^ pad[17]) % 10;

	pad[15] -= pad[0] & 0x201c33b4;
	pad[9] &= pad[4] ^ 0x4b5700f;
	pad[14] *= seed - (pad[15] | 0x1f564f3c) + 0xfe30d470;

	if (branch == 2)
	{
		mix_minor58;
		pad[1] &= 0xdc0e2e53;
		mix_minor65;
		mix_major19 (pad, pad[1]);
	}

	pad[3] ^= ROL (pad[7], 28);
	seed = 0xb2363254;
	pad[17] += 0x503fc4de;
	pad[18] += pad[1] * 0xf14c9c;

	if (branch == 6)
	{
		mix_minor21;
		mix_minor28;
		mix_minor54;
		mix_major20 (pad, pad[5]);
	}

	pad[3] *= pad[0] + 0xaf4b1f37;
	pad[11] *= pad[11] + 0x1d1cbc4e;
	pad[13] ^= pad[1] + 0xf6c6f628;
	pad[17] ^= pad[3] + 0x7f863fa;

	if (branch == 4)
	{
		pad[3] -= 0x7d6e042a;
		mix_minor31;
		pad[12] += 0x2048070;
		mix_major17 (pad, seed);
	}

	pad[11] += pad[4] | 0x3b62a700;
	pad[19] ^= 0xf3c3d3f0;
	ROREQ (seed, pad[10] + 0xe);
	pad[16] |= ROR (pad[16], 10);
	pad[7] *= pad[11] * 0x5053948;

	if (branch == 3)
	{
		mix_minor58;
		pad[3] *= 0x34797b50;
		mix_minor69;
		mix_major16 (pad, pad[4]);
	}

	pad[1] &= seed * 0x377e5649;
	pad[18] += pad[2] | 0x57a0b91;
	ROLEQ (pad[7], seed + 0x7);
	pad[4] -= ROL (pad[7], 2);

	if (branch == 0)
	{
		pad[1] &= 0x49102e08;
		pad[12] += 0x20e0400;
		mix_minor56;
		mix_major15 (pad, pad[18]);
	}

	seed *= seed + 0xfea6f980;
	pad[18] += pad[2] * 0x33aaef75;
	pad[2] ^= pad[12] + 0xda4bd31e;
	seed -= pad[6] | 0x107e370;
	pad[17] -= seed - 0x191504c;

	if (branch == 9)
	{
		mix_minor48;
		pad[4] ^= 0xccc8d5fc;
		mix_minor41;
		mix_major8 (pad, pad[14]);
	}

	pad[3] += ROL (pad[15], 7);
	pad[12] -= pad[10] - 0x18afd3db;
	pad[5] += pad[12] + 0x1392be9b;
	pad[5] -= pad[3] ^ 0xfd205d5;
	pad[8] ^= seed ^ 0x9000ce9;

	if (branch == 5)
	{
		mix_minor51;
		mix_minor24;
		mix_minor32;
		mix_major12 (pad, pad[19]);
	}

	ROREQ (pad[19], seed + 0x19);
	ROREQ (pad[11], pad[19] * 0x10);
	seed ^= pad[12] ^ 0x534576d7;
	ROLEQ (pad[11], pad[1] ^ 0x15);
	pad[19] += pad[9] * 0x12af9c5;

	if (branch == 8)
	{
		mix_minor42;
		mix_minor60;
		mix_major18 (pad, pad[0]);
	}

	ROLEQ (pad[10], seed * 0x14);
	pad[1] -= ROL (pad[14], 19);
	seed |= pad[16] + 0xed222733;
	pad[16] &= pad[3] * 0x532f53a;
	seed ^= pad[11] * 0x14718f9a;

	if (branch == 1)
	{
		mix_minor49;
		mix_minor63;
		pad[1] &= 0xc2c9d439;
		mix_major6 (pad, pad[13]);
	}

	pad[3] *= seed | 0x1739a522;
	seed *= pad[1] | 0x4b09e3e;
	pad[7] ^= pad[12] ^ 0x2a4ea48a;

	if (branch == 7)
	{
		mix_minor35;
		mix_minor35;
		pad[4] *= 0x9b2bcf2e;
		mix_major4 (pad, seed);
	}

	pad[19] -= seed - 0x1dc54aa;
}

void mix_major12 (u32 *pad, u32 seed)
{
	int branch = (pad[7] ^ pad[16] ^ seed) % 6;

	pad[18] &= pad[6] & 0x104394c4;
	seed *= seed + 0xe92519e2;
	pad[4] += pad[19] + 0x46d5ad23;
	pad[6] += pad[1] + 0x3fd0884;
	seed = pad[9] * (seed + 0xc46fe68);
	pad[9] = seed;

	if (branch == 5)
	{
		pad[8] += 0xb0568904;
		mix_minor55;
		mix_minor56;
		mix_major19 (pad, pad[4]);
	}

	pad[11] ^= pad[7] ^ 0x4453b1d7;
	pad[4] ^= pad[12] + 0x187596ce;
	pad[14] += pad[19] ^ 0x1ecd4347;
	pad[17] &= pad[6] + 0xaa504a66;
	pad[13] -= pad[7] - 0x2482f7ba;

	if (branch == 2)
	{
		ROLEQ (pad[16], 27);
		pad[3] += 0x8602040;
		mix_minor37;
		mix_major20 (pad, pad[18]);
	}

	pad[5] *= pad[17] | 0x14128b1f;
	pad[5] &= pad[9] | 0x8ae69ec;
	seed = (pad[5] | 0x25dcee2a) * 0xf7abca44;
	pad[12] += pad[10] * 0x2b5c108a;
	pad[19] -= pad[10] - 0x45d1e08;

	if (branch == 1)
	{
		mix_minor61;
		pad[3] += 0x1704000;
		pad[12] += 0x20e002a;
		mix_major17 (pad, pad[7]);
	}

	pad[5] -= pad[3] - 0x17a9626b;
	seed += pad[8] + 0x55003f14;
	pad[9] += ROL (pad[6], 31);
	pad[2] |= ROL (pad[19], 13);
	pad[19] ^= pad[15] ^ 0xfbf02d6;
	pad[3] |= pad[18] * 0x279ed38c;
	seed &= pad[19] ^ 0x234a2088;

	if (branch == 0)
	{
		mix_minor42;
		pad[12] += 0x68468;
		mix_minor55;
		mix_major16 (pad, pad[14]);
	}

	pad[4] += pad[9] + 0xd5555942;
	pad[6] += pad[0] + 0xf6a829d0;
	pad[2] += pad[17] * 0x6877a2b6;
	seed |= pad[11] + 0x4f92882e;
	pad[4] ^= seed + 0x2a0e1a7a;
	seed *= seed * 0xba88b94;

	if (branch == 3)
	{
		pad[14] += 0x5a9acc8f;
		mix_minor40;
		mix_minor33;
		mix_major15 (pad, seed);
	}

	pad[8] -= pad[19] ^ 0x88fae5c;
	seed -= seed ^ 0x6171e1a;
	seed *= pad[0] & 0x6369ab7c;
	pad[2] ^= pad[12] & 0x36b79ddb;
	seed ^= seed + 0xff3ba490;

	if (branch == 4)
	{
		mix_minor42;
		mix_minor39;
		pad[8] += 0x9cf399e7;
		mix_major8 (pad, pad[2]);
	}

	seed ^= pad[9] * 0x2a0582f6;
	pad[9] ^= pad[10] + 0xf71f2197;
	pad[17] |= seed + 0x417b0639;
	pad[6] ^= ROR (seed, 17);
	pad[15] -= pad[3] - 0x1935355;
	pad[13] += pad[5] + 0x25393a1;
}

void mix_major13 (u32 *pad, u32 seed)
{
	int branch = (pad[1] ^ pad[12] ^ pad[18]) % 11;

	pad[7] *= seed + 0xfd2296dd;
	seed *= pad[9] + 0x10ce1e6b;
	pad[13] |= pad[14] & 0xe7aa887;

	if (branch == 9)
	{
		mix_minor61;
		pad[3] += 0x1702840;
		mix_minor34;
		mix_major4 (pad, pad[15]);
	}

	pad[19] += pad[17] + 0x44864e65;
	pad[2] -= pad[10] - 0x456501d3;
	pad[11] ^= pad[17] + 0xe91158ed;

	if (branch == 6)
	{
		mix_minor41;
		mix_minor46;
		mix_minor27;
		mix_major11 (pad, pad[8]);
	}

	pad[13] -= seed - 0xffeafe84;
	pad[3] ^= pad[10] & 0x5898bbff;
	seed -= pad[17] ^ 0xb4b5ddd;
	pad[5] &= seed + 0xf2a69347;

	if (branch == 7)
	{
		pad[11] += 0x28b81;
		mix_minor55;
		mix_minor38;
		mix_major22 (pad, pad[19]);
	}

	pad[8] += pad[11] + 0x35a3f082;
	pad[15] &= seed + 0xf0918e1c;

	if (branch == 8)
	{
		pad[12] += 0x2180072;
		mix_minor48;
		mix_minor39;
		mix_major5 (pad, seed);
	}

	seed -= pad[12] - 0x1e87b29e;
	seed ^= pad[0] + 0x9b993250;
	pad[13] ^= pad[17] * 0xb083b2b;

	if (branch == 5)
	{
		mix_minor68;
		mix_minor58;
		mix_minor52;
		mix_major3 (pad, pad[14]);
	}

	ROLEQ (pad[1], pad[0] ^ 0x1a);
	pad[5] ^= pad[11] * 0x17321349;
	seed ^= pad[3] + 0xffce689b;
	pad[4] *= seed + 0x2570be6e;

	if (branch == 10)
	{
		pad[14] -= 0xb271fe0e;
		mix_minor57;
		mix_minor20;
		mix_major14 (pad, pad[6]);
	}

	pad[15] *= 0x2d42b937;
	pad[4] *= seed + 0xf544478e;
	pad[0] += pad[9] ^ 0x4dc36a;
	pad[0] -= seed - 0x10bb4f25;

	if (branch == 3)
	{
		mix_minor39;
		mix_minor26;
		pad[0] += 0x8fc063b5;
		mix_major9 (pad, pad[15]);
	}

	pad[19] &= ROR (pad[3], 14);
	pad[17] *= seed * 0x18575b09;
	pad[1] |= seed * 0x50ebe77;
	seed += pad[6] | 0x4d24003d;

	if (branch == 4)
	{
		pad[14] -= 0x3b677863;
		mix_minor29;
		mix_minor22;
		mix_major10 (pad, pad[9]);
	}

	pad[15] &= pad[0] + 0xf770857b;
	ROREQ (pad[0], seed * 0xd);
	seed -= seed | 0x2576a843;

	if (branch == 0)
	{
		mix_minor49;
		mix_minor41;
		pad[3] += 0x8306000;
		mix_major2 (pad, pad[8]);
	}

	pad[1] += seed * 0x2994c8c;
	pad[16] ^= pad[6] + 0xfe25a480;
	pad[3] *= pad[11] * 0x1e333f7b;
	ROREQ (pad[7], pad[17] ^ 0x1a);

	if (branch == 2)
	{
		mix_minor51;
		pad[8] -= 0xfbb3cb07;
		pad[4] ^= 0x214ff68b;
		mix_major7 (pad, pad[1]);
	}

	pad[13] ^= pad[18] + 0x149e5b40;
	pad[0] += pad[19] + 0x541a494;

	if (branch == 1)
	{
		mix_minor38;
		mix_minor28;
		mix_minor50;
		mix_major21 (pad, seed);
	}

	pad[2] -= seed - 0x16deeae;
	pad[9] -= pad[0] ^ 0x1120ce2d;
	pad[13] ^= pad[7] ^ 0x2a74ac2a;
	pad[12] &= pad[9] + 0xdab80c67;
	pad[14] -= seed * 0x2776477;
	pad[4] -= pad[19] * 0x2f2e21d0;
	pad[19] -= pad[3] - 0xe78ae13d;
	seed -= seed ^ 0x434c0d3a;
	seed -= pad[2] - 0x11f70706;
	ROREQ (seed, pad[16] + 0x9);
	pad[13] += seed * 0x2a0d21c3;
}

void mix_major14 (u32 *pad, u32 seed)
{
	int branch = (pad[6] ^ pad[8] ^ pad[15]) % 11;

	pad[14] &= seed ^ 0x1c0b5143;
	pad[17] *= pad[5] + 0x4ef38b53;
	pad[15] ^= ROR (pad[16], 8);

	if (branch == 4)
	{
		pad[4] ^= 0x82254dc0;
		mix_minor36;
		mix_minor47;
		mix_major16 (pad, pad[10]);
	}

	seed ^= pad[17] & 0x3b118c17;
	ROREQ (seed, pad[7] * 0xb);
	pad[5] -= ROR (pad[12], 5);

	if (branch == 10)
	{
		pad[14] -= 0x7b59f866;
		pad[3] -= 0x6bfee72c;
		pad[3] += 0x1704040;
		mix_major15 (pad, seed);
	}

	seed ^= pad[10] + 0xe81a232b;
	pad[18] |= pad[2] + 0xef9e8d77;
	pad[3] += pad[4] + 0xce3d3234;

	if (branch == 5)
	{
		mix_minor27;
		mix_minor38;
		mix_minor20;
		mix_major8 (pad, pad[0]);
	}

	seed *= ROR (seed, 15);
	seed &= pad[7] + 0x358107b;
	pad[12] += ROL (pad[3], 20);

	if (branch == 3)
	{
		mix_minor20;
		pad[17] ^= 0xde7b4629;
		pad[4] ^= 0x5cfc1b41;
		mix_major12 (pad, seed);
	}

	seed += seed + 0xddcb6fb3;
	seed ^= pad[4] * 0x2a5c35ea;
	pad[4] -= pad[3] - 0x3b4034a1;
	pad[11] &= pad[19] | 0x2856103;

	if (branch == 1)
	{
		mix_minor21;
		mix_minor56;
		mix_minor25;
		mix_major18 (pad, pad[16]);
	}

	pad[7] |= seed + 0x2d3d686;
	seed &= pad[15] & 0x316de5b2;

	if (branch == 7)
	{
		mix_minor20;
		mix_minor33;
		pad[14] ^= 0x1e127778;
		mix_major6 (pad, pad[15]);
	}

	seed ^= pad[17] ^ 0x3e8999a9;
	seed += seed + 0x4d77c09e;
	pad[6] *= pad[10] + 0xd1650ad7;
	pad[7] *= pad[3] & 0xade0835;

	if (branch == 0)
	{
		mix_minor26;
		mix_minor28;
		mix_minor67;
		mix_major4 (pad, pad[3]);
	}

	pad[9] -= pad[15] ^ 0x32bd1767;
	pad[12] ^= pad[3] + 0x74289e8a;
	pad[9] ^= pad[5] + 0xd55d1b86;
	seed &= pad[12] * 0x13b7b134;

	if (branch == 8)
	{
		mix_minor35;
		mix_minor30;
		mix_minor21;
		mix_major11 (pad, pad[2]);
	}

	seed += seed + 0xda1b9ad7;
	pad[6] -= pad[18] * 0x452ad09;
	pad[4] += seed ^ 0x4895c9e2;

	if (branch == 9)
	{
		mix_minor49;
		mix_minor49;
		mix_minor46;
		mix_major22 (pad, pad[16]);
	}

	seed ^= seed + 0xf8ecf928;
	ROREQ (pad[18], pad[5] + 0xd);

	if (branch == 6)
	{
		mix_minor55;
		mix_minor69;
		mix_minor67;
		mix_major5 (pad, pad[8]);
	}

	seed *= 0x34b70af0;
	pad[5] -= ROL (pad[19], 23);

	if (branch == 2)
	{
		mix_minor46;
		mix_minor47;
		mix_minor47;
		mix_major3 (pad, pad[18]);
	}

	pad[8] *= ROR (pad[5], 2);
	pad[17] += pad[8] & 0x15595f;
	ROREQ (pad[19], pad[7] + 0x1);
	pad[9] -= seed * 0x539f549;
	pad[0] *= pad[8] ^ 0x10549d01;
	pad[11] -= pad[4] ^ 0x1cd38676;
	pad[12] += ROR (seed, 16);
	pad[17] ^= pad[15] + 0x266b587;
	pad[17] -= ROR (pad[0], 29);
	pad[3] -= pad[13] - 0x2669d0a1;
}

void mix_major15 (u32 *pad, u32 seed)
{
	int branch = (pad[12] ^ pad[15] ^ seed) & 3;

	ROREQ (pad[6], pad[3] ^ 0x14);
	pad[12] += seed ^ 0x9a94557;
	pad[15] *= pad[6] ^ 0x2c63c7d7;
	pad[4] -= pad[17] - 0x1565237b;
	ROLEQ (seed, pad[11] * 0x19);
	seed -= pad[9] * 0x3471499e;
	seed ^= pad[3] ^ 0x34293622;
	pad[11] += seed + 0xbab1970a;
	pad[7] |= pad[18] & 0x2e7cbf50;

	if (branch == 2)
	{
		mix_minor28;
		mix_minor53;
		pad[8] += 0xabdd8689;
		mix_major19 (pad, pad[11]);
	}

	pad[16] &= pad[12] + 0xc178e538;
	pad[14] |= pad[6] * 0xf7a199;
	pad[9] += seed + 0x598a281;
	seed ^= pad[0] + 0xf6c67dcd;
	pad[14] += pad[12] * 0x2a688905;
	ROREQ (pad[16], seed | 0x9);
	pad[10] += seed | 0x4d8cb855;
	pad[19] -= pad[9] - 0x32b94292;
	ROREQ (seed, pad[9] * 0x9);

	if (branch == 1)
	{
		pad[1] &= 0xbe845151;
		mix_minor66;
		pad[14] -= 0x77ab88ea;
		mix_major20 (pad, seed);
	}

	pad[6] &= ROL (pad[10], 28);
	pad[16] += seed ^ 0x5aafcd4a;
	pad[12] &= seed ^ 0x1c22a3b7;
	pad[18] -= pad[4] * 0x358b021d;
	pad[16] ^= pad[13] + 0xac30f7a;
	ROLEQ (seed, pad[17] ^ 0xe);
	ROREQ (seed, pad[1] + 0x2);
	pad[18] -= seed - 0xee6e38da;

	if (branch == 0)
	{
		mix_minor31;
		mix_minor31;
		mix_minor52;
		mix_major17 (pad, pad[9]);
	}

	pad[2] += pad[16] | 0x5cbeb00;
	pad[7] -= ROR (seed, 22);
	pad[4] ^= seed + 0x1580fb54;
	pad[17] -= ROL (pad[12], 25);
	pad[16] += pad[8] ^ 0x1b3ea2;
	pad[5] -= seed - 0x193cf230;
	ROLEQ (pad[18], seed + 0x12);
	seed -= pad[17] & 0x66e0e812;
	pad[12] ^= ROL (pad[7], 18);
	pad[17] -= pad[13] - 0xb70d1a;

	if (branch == 3)
	{
		mix_minor46;
		pad[1] &= 0x24c41868;
		mix_minor24;
		mix_major16 (pad, pad[17]);
	}

	pad[6] += pad[1] + 0xdfef3914;
	seed += ROL (pad[5], 29);
	pad[18] -= pad[8] | 0x456bd4b;
	seed &= pad[13] + 0x123e07ad;
	pad[0] ^= seed * 0x22af60a0;
	pad[13] -= pad[12] - 0xf69f7aa2;
	ROREQ (pad[17], seed ^ 0x1c);
	pad[13] += pad[5] * 0x248bf14b;
	pad[2] ^= ROR (seed, 12);
}

void mix_major16 (u32 *pad, u32 seed)
{
	int branch = pad[12] % 3;

	pad[7] ^= pad[7] + 0x1256f342;
	pad[9] ^= ROL (pad[14], 9);
	pad[0] += pad[13] ^ 0x4a20925;
	ROREQ (pad[13], seed | 0xb);
	seed -= pad[10] - 0x2cd8307e;
	ROREQ (seed, pad[17] * 0x15);
	pad[8] += pad[15] | 0x11570bca;
	seed &= pad[3] ^ 0x4c404c71;
	seed += pad[10] ^ 0x85d82e;
	pad[11] *= pad[6] & 0xf076b8f;
	pad[11] += seed + 0x26d0f98c;

	if (branch == 0)
	{
		mix_minor29;
		mix_minor21;
		mix_minor33;
		mix_major19 (pad, pad[7]);
	}

	pad[1] ^= ROR (seed, 23);
	pad[9] += seed + 0xf24cc80b;
	ROLEQ (pad[3], pad[14] * 0x1d);
	pad[19] += seed + 0x64922cc;
	seed -= pad[0] - 0x1e0944e3;
	ROREQ (seed, seed * 0x1c);
	seed *= pad[15] + 0x8d90c5a3;
	pad[17] ^= seed & 0xdd9bf1a;
	pad[4] -= pad[6] - 0xd5bd8bc1;
	seed += seed + 0x1226f462;
	pad[17] ^= ROL (pad[13], 5);
	ROLEQ (pad[13], seed & 0x12);

	if (branch == 2)
	{
		mix_minor45;
		pad[8] -= 0x3e5f74f5;
		pad[11] += 0xee0e47c6;
		mix_major20 (pad, seed);
	}

	seed |= pad[9] | 0x10b9b57a;
	pad[0] += pad[10] + 0x477a65c2;
	pad[8] |= pad[7] ^ 0x1b348ba1;
	ROLEQ (pad[16], ROL (pad[1], 8));
	seed ^= pad[19] * 0xfa375c5;
	ROREQ (pad[11], ROR (pad[5], 13));
	pad[7] ^= pad[19] + 0x64bd3f85;
	pad[6] *= ROR (seed, 25);
	pad[5] += seed + 0xaeeb67de;
	pad[19] |= ROR (pad[5], 22);
	pad[0] += pad[6] + 0xe1f2872;

	if (branch == 1)
	{
		mix_minor53;
		mix_minor27;
		pad[9] += 0xd829ce84;
		mix_major17 (pad, seed);
	}

	seed |= pad[6] | 0x40c95dca;
	ROLEQ (seed, pad[12] ^ 0x1);
	pad[3] &= pad[8] + 0xed5ca98b;
	pad[4] += seed + 0x92abec6e;
	seed &= ROR (pad[13], 22);
	pad[2] += pad[15] * 0xff635ec;
	pad[6] ^= seed + 0x37343841;
	pad[9] += pad[14] + 0xf8e12c69;
	pad[14] -= ROL (pad[10], 20);
}

void mix_major17 (u32 *pad, u32 seed)
{
	int branch = seed & 1;

	pad[5] -= pad[18] - 0x34b87873;
	pad[17] -= pad[1] - 0x2051ec4;
	pad[6] ^= pad[16] ^ 0x5c80bc7;
	seed -= ROR (seed, 26);
	pad[5] *= pad[16] | 0x154e9813;
	pad[0] |= pad[5] + 0xbac2a47e;
	pad[13] *= pad[9] ^ 0xbf263a6;
	pad[9] |= ROL (pad[11], 23);
	pad[16] *= pad[1] & 0x1c28de84;
	pad[6] ^= ROR (pad[2], 11);
	pad[12] ^= ROR (pad[9], 24);
	seed += seed + 0x2c5a0200;
	pad[19] |= pad[12] + 0xa104f7f6;
	pad[17] ^= pad[11] + 0xf51e9043;
	pad[15] += seed + 0x37f1bc89;

	if (branch == 0)
	{
		pad[3] -= 0x2ae49a0;
		pad[9] += 0xde755696;
		mix_minor53;
		mix_major19 (pad, pad[4]);
	}

	seed += pad[5] | 0x79ba9a48;
	pad[4] -= pad[2] ^ 0x1ecdadba;
	seed ^= pad[10] + 0xf01ca4cf;
	seed ^= pad[8] + 0xf58222aa;
	pad[8] |= pad[7] * 0x59c62257;
	pad[7] ^= pad[7] | 0x2d2750f0;
	seed += pad[17] | 0x1719d4f;
	pad[19] *= pad[4] + 0xcec35bec;
	pad[18] ^= pad[2] + 0xdc17a237;
	pad[19] += pad[5] + 0xca0f8bc5;
	seed += seed + 0xff282d98;
	pad[0] += seed + 0x2a09f2a5;
	pad[11] ^= pad[2] + 0x30e437d6;
	pad[12] |= seed + 0xee36df26;
	pad[15] &= seed + 0xc95e1442;

	if (branch == 1)
	{
		mix_minor27;
		pad[9] += 0xd68c597b;
		pad[9] += 0xdcb2dc4d;
		mix_major20 (pad, pad[14]);
	}

	pad[7] -= pad[17] ^ 0x72eeed7;
	pad[17] *= pad[15] * 0x162a030d;
	pad[7] &= pad[14] + 0xf0dd3ef3;
	seed += pad[1];
	pad[2] ^= pad[13] ^ 0x2d9ceb17;
	pad[7] &= seed ^ 0x176b1b8e;
	pad[8] |= seed + 0xdab13e76;
	pad[16] -= pad[12] - 0x2a74b8d4;
	seed -= pad[4] - 0xcc1039a3;
	seed -= pad[5] * 0x1239378b;
	pad[0] ^= seed ^ 0xd9a5ac4;
	pad[10] -= pad[1] ^ 0x346ff630;
	pad[14] += pad[15] ^ 0x2f99340b;
	pad[11] |= pad[7] + 0xd5881b85;
	ROLEQ (pad[9], pad[16] * 0x19);
}

void mix_major18 (u32 *pad, u32 seed)
{
	int branch = (pad[13] ^ pad[16] ^ pad[17]) % 7;

	pad[2] -= pad[9] - 0xe7e9ac84;
	pad[7] &= seed + 0xd5e47036;
	pad[7] ^= pad[18] ^ 0x5d5e7006;
	seed += pad[6] ^ 0x16afd25f;
	ROREQ (pad[0], pad[18] | 0x1b);

	if (branch == 4)
	{
		mix_minor33;
		ROLEQ (pad[16], 12);
		mix_minor39;
		mix_major19 (pad, pad[17]);
	}

	pad[1] *= pad[0] * 0x927384a;
	seed ^= pad[6] * 0x2ac0b63c;
	seed ^= pad[5] * 0xef44412;
	seed -= ROL (pad[18], 22);

	if (branch == 1)
	{
		mix_minor37;
		mix_minor23;
		mix_minor35;
		mix_major20 (pad, seed);
	}

	pad[6] &= seed + 0x4d05da6a;
	pad[13] *= pad[18] ^ 0xe2ba11c;
	seed ^= pad[2] ^ 0x2e3d328f;
	seed *= pad[1] | 0x110c8a1;
	ROLEQ (pad[4], ROR (pad[6], 27));

	if (branch == 0)
	{
		pad[3] -= 0xab85f363;
		mix_minor47;
		ROLEQ (pad[10], 12);
		mix_major17 (pad, seed);
	}

	pad[19] &= ROR (seed, 8);
	seed |= ROR (pad[19], 12);
	seed += pad[14] * 0x2d8924b3;
	pad[10] ^= pad[15] + 0xdcba6126;
	seed += pad[16] & 0xf72e29a;
	pad[3] -= pad[18] | 0x7614cfb;

	if (branch == 6)
	{
		mix_minor60;
		pad[3] *= 0x23a0356c;
		mix_minor33;
		mix_major16 (pad, pad[9]);
	}

	pad[19] &= pad[4] + 0xfe6ea18f;
	pad[6] *= pad[7] & 0x226185b2;
	pad[0] += pad[4] ^ 0x35388017;
	seed ^= pad[14] * 0x268d6eae;

	if (branch == 3)
	{
		pad[14] += 0x72559385;
		pad[8] += 0xafa7ed31;
		mix_minor26;
		mix_major15 (pad, pad[0]);
	}

	pad[15] += seed ^ 0xbf3b8c0;
	ROREQ (pad[10], ROR (pad[18], 25));
	pad[19] |= seed ^ 0x61d2180;
	pad[4] &= pad[19] + 0x588d79a3;

	if (branch == 5)
	{
		pad[11] += 0xa26a5e66;
		pad[9] += 0xcdf889ea;
		mix_minor69;
		mix_major8 (pad, pad[8]);
	}

	pad[0] += seed + 0x19039f88;
	ROLEQ (seed, ROR (pad[7], 14));
	pad[6] += pad[8] ^ 0x1f3dce4;
	pad[17] *= pad[18] + 0x4f2cb877;
	pad[6] &= pad[15] * 0x177f5d63;
	ROLEQ (pad[12], ROL (pad[16], 1));

	if (branch == 2)
	{
		mix_minor23;
		mix_minor32;
		pad[9] += 0xc3b96ef0;
		mix_major12 (pad, pad[18]);
	}

	pad[19] += pad[12] + 0xbe9fd027;
	seed &= pad[2] * 0x3ec8c5cb;
	pad[8] += pad[4] & 0x48357b75;
	ROLEQ (pad[1], pad[6] + 0x14);
	pad[14] ^= pad[11] + 0x13c7dc0f;
	pad[4] += ROL (seed, 19);
	pad[12] -= pad[2] - 0x15ea2e80;
	pad[11] += pad[19] + 0xaff84c32;
	pad[2] ^= pad[5] * 0x278991a8;
	pad[14] += pad[2] + 0xf431b0d4;
}

void mix_major19 (u32 *pad, u32 seed)
{
	pad[3] ^= seed + 0xd2670e69;
	seed ^= pad[2] & 0x3bd91a6d;
	seed += pad[12] + 0xe162a863;
	pad[3] -= seed - 0x2f72a89a;
	pad[11] ^= pad[3] & 0x4053f57a;
	seed *= pad[12] + 0xfe64a9df;
	pad[6] ^= pad[14] ^ 0x6c235a3;
	ROLEQ (pad[11], pad[7] + 0x1b);
	pad[13] ^= pad[8] + 0xdf869976;
	ROLEQ (pad[4], pad[12] + 0x1);
	pad[9] += pad[13] ^ 0x3d475dc2;
	pad[14] += ROR (seed, 25);
	pad[10] += seed ^ 0x222fef6f;
	pad[12] ^= ROR (pad[19], 9);
	pad[4] += pad[8] + 0x56d964ed;
	pad[18] -= pad[7] - 0x132444b;
	pad[0] = 0xd35add1b;
	seed += pad[6] * 0x6fe2b2f;
	pad[14] *= pad[1] + 0xacf6925;
	pad[15] ^= ROR (seed, 19);
	pad[0] = 0xc0c8f110 - pad[15];
	seed ^= seed | 0x2a57ebeb;
	pad[12] -= ROL (seed, 13);
	seed |= pad[13] & 0x15a66bda;
	seed += pad[18] + 0x235ac102;
	ROLEQ (pad[8], seed + 0xd);
	pad[5] ^= pad[12] + 0x3bbb70fe;
	seed &= pad[1] + 0xec51134a;
	pad[19] ^= pad[11] * 0x87095a6;
	pad[0] -= pad[5] & 0xf43f6fb;
	pad[15] += pad[4] + 0xea66f8dc;
	pad[19] -= pad[7] - 0xd049cfd6;
	pad[13] -= pad[0] ^ 0x253c86f9;
	seed += pad[16] | 0x520e84ba;
	pad[10] -= ROL (pad[18], 23);
	pad[18] += seed * 0x2ee5918a;
	pad[7] &= pad[11] ^ 0xf0a32bc;
	pad[0] -= seed ^ 0xfa89177;
	pad[2] |= ROL (pad[9], 24);
	pad[14] *= 0x1cb1574a;
	ROREQ (pad[10], seed + 0xd);
	pad[1] &= pad[11] + 0xef291170;
	pad[6] += pad[19] & 0x259a6745;
	seed -= pad[2] ^ 0x10467b8;
	pad[18] *= pad[9] + 0xdbff9c2b;
	pad[16] += seed ^ 0x8d4c279;
	pad[5] -= ROR (pad[10], 24);
}

void mix_major20 (u32 *pad, u32 seed)
{
	ROLEQ (pad[14], seed & 0xe);
	seed += pad[15] ^ 0xbf446ce;
	seed += seed + 0xe227ea76;
	pad[19] *= pad[15] * 0x50ee813;
	ROREQ (pad[19], pad[10] + 0x1e);
	pad[16] -= seed & 0x372035b;
	pad[18] |= pad[5] + 0xd9d1da08;
	pad[5] += ROL (pad[11], 9);
	ROREQ (pad[8], pad[0] * 0x3);
	seed += pad[0] ^ 0x46d0b40;
	pad[14] ^= pad[1] + 0xe8684fc;
	seed -= pad[14] * 0x28f80128;
	seed ^= ROR (pad[1], 10);
	pad[19] *= pad[16] + 0xa0397f;
	pad[13] += ROL (seed, 17);
	pad[4] -= pad[6] - 0x95670090;
	ROLEQ (pad[14], 24);
	pad[0] *= 0xe6219a29;
	pad[3] += pad[11] + 0xfa61efff;
	pad[8] -= pad[10] - 0xda64c153;
	pad[9] -= pad[8] - 0x22a4da90;
	seed = ROL (0xf2eafbc6, pad[12] ^ 0x15);
	seed ^= pad[19] | 0x2cd48d0d;
	seed += pad[19] + 0xc6a5343a;
	pad[13] -= seed - 0xc3172899;
	pad[14] ^= ROL (pad[4], 11);
	pad[19] ^= pad[14] ^ 0x274bf2e7;
	pad[16] += pad[9] ^ 0x1448b87d;
	seed &= pad[19] ^ 0x7c5e8091;
	pad[15] -= seed - 0x2de973cc;
	pad[17] *= pad[0] + 0x5cd4018;
	pad[15] *= pad[7] ^ 0x1c718ec4;
	ROREQ (pad[4], ROL (pad[14], 31));
	pad[6] -= pad[1] * 0x11e6e4aa;
	pad[3] -= pad[14] - 0x20a45ef;
	pad[14] &= pad[11] ^ 0x79362e5;
}

void mix_major21 (u32 *pad, u32 seed)
{
	int branch = (pad[2] ^ pad[11] ^ pad[15]) % 11;

	ROREQ (pad[13], seed | 0x1e);
	seed -= pad[6] - 0x67e07c3f;
	seed ^= seed * 0x157052aa;

	if (branch == 1)
	{
		mix_minor22;
		mix_minor62;
		mix_minor63;
		mix_major6 (pad, seed);
	}

	ROLEQ (pad[6], ROR (pad[6], 11));
	pad[19] += seed * 0x2437b7c7;

	if (branch == 6)
	{
		pad[8] -= 0x383b7a6c;
		mix_minor28;
		pad[14] -= 0xd8799ad3;
		mix_major4 (pad, pad[2]);
	}

	pad[3] += pad[12] + 0xf9430940;
	pad[11] -= pad[6];

	if (branch == 5)
	{
		mix_minor40;
		mix_minor41;
		mix_minor66;
		mix_major11 (pad, pad[8]);
	}

	pad[0] += pad[14] | 0x27c78ea;
	pad[18] -= seed & 0x6b2cc678;
	ROREQ (pad[15], pad[11] & 0xa);

	if (branch == 4)
	{
		mix_minor67;
		mix_minor26;
		mix_minor21;
		mix_major22 (pad, seed);
	}

	seed ^= ROR (pad[18], 2);
	pad[10] += pad[15] * 0x42515298;
	pad[19] += pad[2] ^ 0x2a15668a;

	if (branch == 7)
	{
		ROLEQ (pad[19], 21);
		mix_minor30;
		mix_minor58;
		mix_major5 (pad, pad[11]);
	}

	pad[6] -= seed - 0xe28d6e07;
	pad[1] &= pad[3] + 0x8a7848d;
	pad[10] *= pad[17] + 0xf76061aa;

	if (branch == 0)
	{
		mix_minor29;
		pad[17] ^= 0x3d87b641;
		mix_minor60;
		mix_major3 (pad, pad[12]);
	}

	pad[6] += ROR (pad[1], 8);
	pad[1] *= pad[2] | 0x16a41bdf;

	if (branch == 8)
	{
		mix_minor68;
		pad[9] += 0xb8c1b4ce;
		pad[4] ^= 0x5c2840a0;
		mix_major14 (pad, pad[3]);
	}

	pad[0] -= pad[4] - 0x21889c31;
	seed *= seed ^ 0x14a9f943;
	pad[5] |= pad[13] + 0x5c58f04e;
	pad[19] ^= pad[14] + 0x49437c23;

	if (branch == 2)
	{
		mix_minor22;
		mix_minor30;
		mix_minor29;
		mix_major9 (pad, pad[13]);
	}

	pad[9] |= pad[6] ^ 0x360a1ff0;
	pad[13] &= pad[14] * 0x810027b;
	seed += seed + 0x3053624;

	if (branch == 3)
	{
		mix_minor20;
		mix_minor38;
		mix_minor27;
		mix_major10 (pad, pad[17]);
	}

	seed -= pad[1] - 0xc7af02f5;
	seed &= 0xc11a9b11;
	pad[6] ^= pad[12] + 0xac2e6058;
	pad[12] ^= pad[17] + 0xd87e9f50;

	if (branch == 10)
	{
		mix_minor37;
		mix_minor29;
		mix_minor31;
		mix_major2 (pad, pad[6]);
	}

	ROLEQ (pad[9], pad[7] ^ 0x1);
	seed += pad[14] ^ 0xff63c7c;

	if (branch == 9)
	{
		pad[14] ^= 0x491ed97d;
		mix_minor22;
		mix_minor40;
		mix_major7 (pad, pad[5]);
	}

	pad[9] ^= pad[6] ^ 0x132ee304;
	pad[12] *= pad[14] + 0x11e0a175;
	pad[14] -= seed ^ 0x267e2568;
	ROLEQ (pad[0], ROL (pad[3], 11));
	pad[8] ^= pad[6] ^ 0xe173238;
	pad[0] *= pad[6] + 0xee9e5b6a;
	pad[9] |= pad[15] * 0x1fe0f470;
	ROLEQ (pad[2], pad[2] + 0x9);
	pad[16] ^= pad[14] * 0x1b4bf87b;
	pad[16] &= pad[10] + 0x2383020a;
	pad[15] += pad[7] + 0xeb32d6f9;
	pad[15] ^= ROL (pad[16], 17);
	pad[16] += seed | 0x20914367;
}

void mix_major22 (u32 *pad, u32 seed)
{
	int branch = seed % 11;

	pad[12] += seed ^ 0xc3115e;
	pad[19] -= seed - 0x4f9d3712;
	pad[16] &= pad[11] * 0x37e68d12;

	if (branch == 1)
	{
		mix_minor57;
		pad[4] *= 0x6f2b88b5;
		mix_major19 (pad, pad[7]);
	}

	seed -= pad[18] ^ 0x4ea934da;
	pad[1] &= pad[18] ^ 0x18a1ba1a;

	if (branch == 0)
	{
		pad[3] += 0x9302840;
		pad[8] += 0x91520abe;
		mix_minor37;
		mix_major20 (pad, seed);
	}

	pad[18] += pad[17] * 0x3bf23dc7;
	pad[12] += pad[5] ^ 0x3537eae2;
	pad[9] += pad[5] + 0xf4d4e1ee;
	pad[11] -= ROL (pad[16], 22);

	if (branch == 2)
	{
		mix_minor67;
		mix_minor67;
		mix_minor37;
		mix_major17 (pad, pad[16]);
	}

	ROLEQ (pad[17], pad[1]);
	seed |= ROR (pad[5], 1);
	pad[11] += seed + 0xf0871714;

	if (branch == 3)
	{
		mix_minor35;
		mix_minor65;
		pad[8] -= 0x265c0434;
		mix_major16 (pad, pad[18]);
	}

	seed &= 0x1b54f10;
	pad[15] += pad[1] + 0xe9b29695;
	pad[9] ^= pad[19] + 0xf9850900;

	if (branch == 8)
	{
		ROLEQ (pad[10], 1);
		pad[4] *= 0xb27c0ecb;
		mix_minor61;
		mix_major15 (pad, pad[11]);
	}

	pad[0] += pad[6] + 0x224785;
	pad[1] -= pad[9] * 0x602a9ff;

	if (branch == 4)
	{
		mix_minor53;
		pad[3] += 0x9702000;
		mix_minor26;
		mix_major8 (pad, pad[10]);
	}

	pad[14] += ROL (pad[5], 30);
	pad[8] -= pad[12] * 0x223c8eff;
	pad[3] += pad[11] * 0xc99e9b5;

	if (branch == 7)
	{
		mix_minor57;
		mix_minor45;
		mix_minor49;
		mix_major12 (pad, pad[13]);
	}

	seed *= pad[3] ^ 0xf8e252d;
	pad[12] += seed & 0xa58c765;

	if (branch == 10)
	{
		mix_minor39;
		pad[14] ^= 0x4dfb7ee4;
		mix_minor61;
		mix_major18 (pad, pad[13]);
	}

	pad[11] -= pad[3] ^ 0x59507436;
	pad[10] ^= seed ^ 0x1082cbd7;

	if (branch == 9)
	{
		mix_minor50;
		mix_minor53;
		ROLEQ (pad[10], 6);
		mix_major6 (pad, pad[3]);
	}

	ROREQ (pad[8], seed + 0x1);
	pad[17] ^= pad[15] * 0x1627a9f4;

	if (branch == 6)
	{
		pad[0] += 0xc3649533;
		mix_minor30;
		mix_minor41;
		mix_major4 (pad, seed);
	}

	seed *= ROL (pad[11], 21);
	pad[3] ^= pad[11] + 0x27d2e810;
	pad[3] += pad[16] * 0x2bb9259f;

	if (branch == 5)
	{
		mix_minor40;
		mix_minor63;
		mix_minor61;
		mix_major11 (pad, seed);
	}

	pad[19] ^= pad[17] ^ 0x2b7f6e80;
	pad[7] ^= ROL (pad[0], 24);
	pad[4] ^= seed | 0x334e9536;
	pad[11] -= ROL (seed, 19);
	pad[12] ^= seed + 0xf8e5b64c;
	pad[11] += pad[4] + 0x661bc871;
	ROREQ (pad[19], pad[0] & 0x9);
	pad[15] += seed & 0x7b85306;
	pad[7] -= pad[12] - 0x1394a239;
	pad[17] ^= pad[3] + 0x4d2d2d3c;
	pad[12] -= pad[6] & 0x312a10;
	pad[13] |= pad[19] + 0xba345c89;
	pad[13] *= seed + 0x2098c7b4;
	pad[6] &= ROL (pad[10], 22);
	pad[6] -= pad[12] & 0x13175e3d;
}

void mix_major23 (u32 *pad, u32 seed)
{
	int branch = seed % 11;

	seed &= ROL (pad[5], 11);
	pad[18] -= ROR (seed, 23);
	pad[19] += seed + 0xb42a2f00;

	if (branch == 5)
	{
		pad[17] ^= 0x33db0465;
		mix_minor62;
		mix_minor33;
		mix_major22 (pad, pad[2]);
	}

	pad[0] += pad[12] + 0x71507fd7;
	seed += pad[19] + 0x9a68096;

	if (branch == 6)
	{
		mix_minor64;
		mix_minor47;
		mix_minor62;
		mix_major5 (pad, pad[10]);
	}

	pad[0] += pad[2] + 0x238788d8;
	ROREQ (pad[3], pad[15] + 0x16);
	pad[10] -= pad[9] - 0xdf1e2fab;

	if (branch == 3)
	{
		mix_minor50;
		pad[4] ^= 0x3f348b71;
		mix_minor44;
		mix_major3 (pad, seed);
	}

	seed ^= ROL (seed, 27);
	seed -= ROR (pad[11], 23);

	if (branch == 7)
	{
		mix_minor48;
		mix_minor51;
		pad[3] -= 0xf2da1eb7;
		mix_major14 (pad, pad[15]);
	}

	pad[18] += pad[10] + 0x13ba6066;
	pad[11] -= pad[10] - 0xd44a337d;
	pad[17] &= pad[3] + 0xad722336;

	if (branch == 4)
	{
		pad[3] *= 0x1e952879;
		mix_minor55;
		mix_minor52;
		mix_major9 (pad, pad[8]);
	}

	pad[7] -= ROL (seed, 15);
	seed |= pad[6] + 0x45d2e311;
	seed ^= pad[7] + 0xd196f18f;
	ROLEQ (pad[7], seed ^ 0x8);

	if (branch == 8)
	{
		mix_minor55;
		mix_minor64;
		pad[3] += 0x704000;
		mix_major10 (pad, pad[13]);
	}

	pad[6] += pad[18] * 0x413db8c1;
	pad[0] ^= pad[19] + 0x2be41642;
	pad[4] *= ROR (pad[9], 18);

	if (branch == 10)
	{
		mix_minor26;
		mix_minor47;
		mix_minor59;
		mix_major2 (pad, seed);
	}

	ROLEQ (seed, pad[6] * 0x13);
	pad[17] *= pad[3] & 0x9262077;
	pad[13] ^= pad[14] + 0xfa8ae5a0;

	if (branch == 1)
	{
		mix_minor65;
		mix_minor21;
		mix_minor24;
		mix_major7 (pad, pad[13]);
	}

	ROREQ (seed, ROL (pad[2], 17));
	pad[13] -= pad[8] - 0xffd58fe8;
	pad[8] += pad[6] ^ 0x1d606322;

	if (branch == 9)
	{
		mix_minor52;
		mix_minor26;
		pad[3] += 0x404840;
		mix_major21 (pad, pad[10]);
	}

	pad[16] += pad[19] + 0xe3a240f7;
	seed ^= ROR (pad[14], 3);

	if (branch == 0)
	{
		mix_minor43;
		pad[3] -= 0xa9fe8c6d;
		pad[0] += 0xe9a284bb;
		mix_major13 (pad, pad[4]);
	}

	pad[18] ^= pad[7] | 0x196e1a4c;
	seed += pad[18] ^ 0xffcac8f;
	pad[1] ^= pad[0] ^ 0xb09adec;

	if (branch == 2)
	{
		mix_minor53;
		mix_minor39;
		mix_minor67;
		mix_major24 (pad, pad[11]);
	}

	pad[14] *= pad[2] + 0x328852b1;
	pad[8] ^= pad[15] & 0x1e0a37a;
	pad[3] *= ROL (seed, 13);
	pad[6] ^= pad[18] + 0xc9c48b38;
	ROLEQ (pad[2], pad[14] + 0x1d);
	seed ^= ROR (pad[10], 13);
	pad[12] ^= pad[8] + 0xef774f5b;
	ROREQ (seed, pad[14] + 0x3);
	seed += seed ^ 0x58f00a07;
	pad[9] ^= seed ^ 0x5483deb2;
	pad[14] |= pad[0] * 0x2c63f116;
	pad[3] += pad[10] ^ 0xa051af;
	seed -= pad[0] - 0xfdb247f0;
	pad[2] -= seed - 0xf9432db1;
}

void mix_major24 (u32 *pad, u32 seed)
{
	int branch = (pad[17] ^ pad[8] ^ pad[10]) % 11;

	seed *= pad[7];
	seed ^= pad[0] ^ 0x13a77c41;
	ROLEQ (pad[2], pad[3] + 0x10);

	if (branch == 1)
	{
		mix_minor27;
		mix_minor59;
		mix_minor22;
		mix_major11 (pad, pad[12]);
	}

	seed ^= seed + 0xf4135aef;
	ROLEQ (seed, pad[6] + 0x9);
	pad[14] += ROL (pad[13], 25);
	pad[16] ^= pad[8] + 0x19454e81;

	if (branch == 10)
	{
		mix_minor63;
		mix_minor32;
		mix_minor29;
		mix_major22 (pad, pad[8]);
	}

	pad[3] *= seed + 0xcb4ea17e;
	ROLEQ (pad[17], pad[17] ^ 0x14);
	seed -= pad[11] * 0x2c0fd99b;

	if (branch == 3)
	{
		mix_minor57;
		mix_minor63;
		mix_minor25;
		mix_major5 (pad, seed);
	}

	pad[12] += pad[19] + 0x7e55995;
	pad[14] -= pad[13] * 0x3dd1a491;
	pad[4] |= pad[8] & 0x162b97ec;
	pad[8] += pad[3] + 0xc3000fb6;

	if (branch == 6)
	{
		mix_minor48;
		pad[8] += 0x9cd4867c;
		pad[14] += 0x79cdbac7;
		mix_major3 (pad, pad[9]);
	}

	pad[13] += pad[8] ^ 0x2a161224;
	pad[10] += pad[1] * 0xc693c6b;
	pad[4] *= pad[10] + 0xecde6b96;

	if (branch == 9)
	{
		mix_minor40;
		ROLEQ (pad[10], 18);
		mix_minor40;
		mix_major14 (pad, pad[13]);
	}

	pad[8] *= ROR (pad[13], 25);
	pad[17] ^= ROR (pad[14], 24);
	seed &= pad[4] + 0x1c938114;

	if (branch == 2)
	{
		pad[4] ^= 0xc25fdd85;
		mix_minor42;
		mix_minor27;
		mix_major9 (pad, seed);
	}

	pad[0] *= seed + 0xc328858;
	seed += pad[15] | 0x137d6d8;
	pad[3] -= pad[9] - 0xae4f0ae;

	if (branch == 0)
	{
		mix_minor45;
		mix_minor24;
		mix_minor51;
		mix_major10 (pad, pad[3]);
	}

	seed *= pad[10] + 0xe55615;
	pad[15] |= seed | 0x120d32e3;
	ROLEQ (seed, pad[15] ^ 0xc);
	ROREQ (pad[6], pad[7]);

	if (branch == 7)
	{
		mix_minor23;
		mix_minor57;
		mix_minor55;
		mix_major2 (pad, pad[17]);
	}

	pad[3] -= pad[4] | 0x2587388f;
	pad[2] += pad[4] + 0xffda87c9;
	seed -= ROR (pad[2], 17);
	pad[1] += pad[6] * 0x34aabe3a;

	if (branch == 4)
	{
		mix_minor30;
		mix_minor46;
		mix_minor57;
		mix_major7 (pad, pad[16]);
	}

	pad[17] ^= pad[13] ^ 0x3d17e55a;
	pad[15] *= pad[14] + 0xdaf5121;

	if (branch == 5)
	{
		mix_minor57;
		mix_minor60;
		mix_minor61;
		mix_major21 (pad, seed);
	}

	ROLEQ (pad[6], pad[17] * 0x14);
	pad[6] += pad[15] ^ 0x14819516;

	if (branch == 8)
	{
		pad[8] -= 0x7b22975e;
		mix_minor50;
		mix_minor56;
		mix_major13 (pad, pad[5]);
	}

	pad[8] |= pad[14] + 0xc735f228;
	ROREQ (pad[7], pad[17] + 0x1e);
	seed *= pad[10] * 0x340d3ff2;
	pad[16] *= pad[14] + 0x57a8d4b3;
	pad[6] -= pad[1] - 0x534be48e;
	pad[2] ^= pad[9] * 0xd695251;
	pad[12] ^= pad[7];
	pad[1] += pad[17] + 0xf022cb99;
	pad[4] += seed | 0x2954ac20;
	pad[7] *= seed ^ 0x1b904466;
	pad[2] -= seed * 0x31fef0e1;
}
