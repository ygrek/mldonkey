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
 * This is a probably the oldest pad mingling code used in FastTrack.
 * Used for encryption version 0x01.
 *
 * Thanks to weinholt for reverse engineering most parts of this file.
 * Many thanks to Thingol for cleaning up this file considerably.
 */

#include <string.h>				/* memcpy(), memset() */

#ifdef WIN32
typedef unsigned __int64 u64;
#else
typedef unsigned long long u64;
#endif							/* WIN32 */

typedef unsigned int u32;
typedef unsigned char u8;

static u32 modulus[] = {
	0x0eb96841d, 0x031c4a05e, 0x0193bbd8f, 0x0bf77b6d6, 0x096d7f927,
	0x0e569c5fc, 0x05efa55ff, 0x0ba1519a1, 0x01b32a36f, 0x0e84b25f8,
	0x08d5b5eb2, 0x0c11f00e3, 0x019d2974d, 0x0e7ee26ad, 0x0bc8c0457,
	0x091fd3a9e, 0x06e37192e, 0x0e475aa9e, 0x072f554fc, 0x0695b3ca2,
	0x0f97bb445, 0x069f29f9a, 0x0b15e2216, 0x0390a5a36, 0x02c0054b3,
	0x0363dc15e, 0x0d238b5b8, 0x0e957a9b5, 0x00032a3df, 0x08781ab21,
	0x00a7703d8, 0x05250e6fe, 0x0b42d2df0, 0x010a931f8, 0x01e4970e5,
	0x0c3741ca7, 0x08606342a, 0x0d61a7055, 0x018e4dd48, 0x00829ee6d,
	0x08443fd05, 0x0e9db6341, 0x0de3c6725, 0x0a4037cdb, 0x096e537a7,
	0x0ed64060a, 0x0da2d7944, 0x051be98b0, 0x0315d5780, 0x0b6a31787,
	0x04d23ffc1, 0x0dbd26bb0, 0x0de7aa2fe, 0x071a157ac, 0x019288512,
	0x00d7a0050, 0x0522f69d1, 0x0dd331dd5, 0x096e62735, 0x0c6f2dc0e,
	0x0ae08473d, 0x00a532f6a, 0x034eb20ca, 0x0d453ec0a
};

void enc_type_1 (u8 *out_key, u8 *in_key);

static void big_set (u32 *num, u32 val);
static void big_letoh (u32 *dst, u8 *src);
static void big_htole (u8 *dst, u32 *src, int cnt);
static void big_expmod (u32 *num, u32 *exp, u32 *mod);
static void big_mulmod (u32 *num1, u32 *num2, u32 *mod);
static void big_mul (int cnt, u32 *out, u32 *in1, u32 *in2);
static void big_mod (int cnt, u32 *out, u32 *in1, u32 *in2);

static int big_getbit (u32 *num, int i);
static int big_isless (int cnt, u32 *num1, u32 *num2);

void enc_type_1 (u8 *out_key, u8 *in_key)
{
	u32 exp[64], num1[64];
	u8 num2[256];

	big_set (exp, 3);

	memcpy (num2, in_key, 255);
	num2[255] = 1;

	big_letoh (num1, num2);

	if (big_getbit (modulus, 2047) != 0)	/* always true */
		big_expmod (num1, exp, modulus);

	big_htole (out_key, num1, 256);
}

void big_set (u32 *num, u32 val)
{
	memset (num, 0, 4 * 64);
	num[0] = val;
}

void big_letoh (u32 *dst, u8 *src)
{
	int i, j;
	u32 tmp;

	for (i = 0; i < 64; i++)
	{
		for (j = 0, tmp = 0; j < 32; j += 8)
			tmp |= ((u32) (*src++)) << j;
		dst[i] = tmp;
	}
}

void big_htole (u8 *dst, u32 *src, int cnt)
{
	int i;

	if (cnt == 0)
		return;

	for (i = 0; i < cnt; i++)
		dst[i] = (src[i >> 2] >> ((i & 3) << 3)) & 0xff;
}

int big_getbit (u32 *num, int i)
{
	return (num[i >> 5] >> (i & 0x1f)) & 1;
}

void big_expmod (u32 *num, u32 *exp, u32 *mod)
{
	u32 lnum[64];
	int i, j;

	for (i = 2048; i > 0; i--)
		if (big_getbit (exp, i - 1) != 0)
			break;

	big_set (lnum, 1);

	if (i > 0)
	{
		for (j = 0; j < i; j++)
		{
			if (big_getbit (exp, j) != 0)
				big_mulmod (lnum, num, mod);
			big_mulmod (num, num, mod);
		}
	}

	memcpy (num, lnum, 64 * 4);
}

void big_mulmod (u32 *num1, u32 *num2, u32 *mod)
{
	u32 lnum[128];
	int len = mod[63] ? 64 : 32;

	big_mul (len, lnum, num1, num2);
	big_mod (len, num1, lnum, mod);

	if (len == 32)
		memset (num1 + 32, 0, 32 * 4);
}

void big_mul (int cnt, u32 *out, u32 *in1, u32 *in2)
{
	int i, j;
	u64 k;

	if (cnt == 0)
		return;

	memset (out, 0, 2 * cnt * 4);

	for (i = 0; i < cnt; i++)
	{
		for (j = 0, k = 0; j < cnt; j++)
		{
			k += (u64) out[i + j] + (u64) in1[i] * (u64) in2[j];
			out[i + j] = k & 0xffffffff;
			k >>= 32;
		}
		out[i + j] = k;
	}
}

int big_isless (int cnt, u32 *num1, u32 *num2)
{
	if (cnt == 0)
		return 0;

	for (cnt--; cnt >= 0; cnt--)
	{
		if (num1[cnt] < num2[cnt])
			return 1;
		else if (num1[cnt] > num2[cnt])
			return 0;
	}

	return 0;
}

void big_mod (int cnt, u32 *out, u32 *in1, u32 *in2)
{
	u64 x;
	u32 k, l;
	int i, j;

	if (cnt == 0)
		return;

	for (i = cnt - 1; i >= 0; i--)
	{
		k = in1[cnt + i];
		if (in2[cnt - 1] != 0xffffffff)
		{
			x = (((u64) in1[cnt + i] << 32) +
				 (u64) in1[cnt + i - 1]) / ((u64) in2[cnt - 1] + 1);
			k = x;
		}

		for (j = 0, l = 0; j < cnt; j++)
		{
			x = (u64) k *(u64) in2[j] + l;

			l = x >> 32;
			if (in1[i + j] < (x & 0xffffffff))
				l++;
			in1[i + j] -= x;
		}
		in1[cnt + i] -= l;

		while (in1[cnt + i] != 0 || !big_isless (cnt, in1 + i, in2))
		{
			for (j = 0, l = 0; j < cnt; j++)
			{
				if (in1[i + j] != 0 || in2[j] == 0)
				{
					in1[i + j] -= l;
					l = (in1[i + j] < in2[j]);
				} else
				{
					in1[i + j] = 0xffffffff;
				}
				in1[i + j] -= in2[j];
			}
			in1[cnt + i] -= l;
		}
	}

	memcpy (out, in1, cnt * 4);
}
