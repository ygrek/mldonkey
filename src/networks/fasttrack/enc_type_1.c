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
 * This is a probably the oldest pad mingling code used in FastTrack.
 * Used for encryption version 0x01
 *
 * Thanks to weinholt for reverse engineering most parts of this file.
 */

#include <string.h>	// memcpy()

#ifdef WIN32
typedef unsigned __int64 u64;
#else
typedef unsigned long long u64;
#endif

#define addCarryOf(x,y)		(((x)+(y)) < (x) || ((x)+(y)) < (y))
#define subCarryOf(x,y)		((x) < (y))
#define negCarryOf(x)		((0-x) != 0)

static unsigned int cipher_table[] =
{
	0x0EB96841D, 0x031C4A05E, 0x0193BBD8F, 0x0BF77B6D6, 0x096D7F927,
	0x0E569C5FC, 0x05EFA55FF, 0x0BA1519A1, 0x01B32A36F, 0x0E84B25F8,
	0x08D5B5EB2, 0x0C11F00E3, 0x019D2974D, 0x0E7EE26AD, 0x0BC8C0457,
	0x091FD3A9E, 0x06E37192E, 0x0E475AA9E, 0x072F554FC, 0x0695B3CA2,
    0x0F97BB445, 0x069F29F9A, 0x0B15E2216, 0x0390A5A36, 0x02C0054B3,
    0x0363DC15E, 0x0D238B5B8, 0x0E957A9B5, 0x00032A3DF, 0x08781AB21,
    0x00A7703D8, 0x05250E6FE, 0x0B42D2DF0, 0x010A931F8, 0x01E4970E5,
    0x0C3741CA7, 0x08606342A, 0x0D61A7055, 0x018E4DD48, 0x00829EE6D,
    0x08443FD05, 0x0E9DB6341, 0x0DE3C6725, 0x0A4037CDB, 0x096E537A7,
    0x0ED64060A, 0x0DA2D7944, 0x051BE98B0, 0x0315D5780, 0x0B6A31787,
    0x04D23FFC1, 0x0DBD26BB0, 0x0DE7AA2FE, 0x071A157AC, 0x019288512,
    0x00D7A0050, 0x0522F69D1, 0x0DD331DD5, 0x096E62735, 0x0C6F2DC0E,
    0x0AE08473D, 0x00A532F6A, 0x034EB20CA, 0x0D453EC0A
};

void enc_type_1 (unsigned char *out_key/*stack1*/, unsigned char *in_key/*stack2*/);

static void enc_1_mix (unsigned int *table/*ecx*/, unsigned int *in_key/*stack1*/, unsigned int *out_key/*stack2*/, unsigned int cnt/*stack3*/, unsigned int *buf/*stack4*/);
static void enc_1_zero (unsigned int *buf/*ecx*/, unsigned int first/*stack1*/);
static void enc_1_sub1 (unsigned int *dst/*ecx*/, unsigned char *src/*stack1*/);
static unsigned int enc_1_sub2 (unsigned int *buf/*ecx*/, unsigned int i/*stack1*/);
static void enc_1_sub3 (unsigned int *buf1/*ecx*/, unsigned int *buf2/*stack*/, unsigned int *table/*stack2*/);
static void enc_1_sub4 (unsigned int *src/*ecx*/, unsigned char *dst/*stack1*/, unsigned int cnt/*stack2*/);
static void enc_1_sub5 (unsigned int *buf1/*ecx*/, unsigned int *buf2/*stack1*/, unsigned int *table/*stack2*/);
static void enc_1_sub6 (unsigned int cnt/*ecx*/, unsigned char *buf1/*edx*/, unsigned char *buf2/*stack1*/, unsigned char *buf3/*stack2*/);
static void enc_1_sub7 (unsigned int cnt/*ecx*/, unsigned char *buf1/*edx*/, unsigned char *buf2/*stack1*/, unsigned char *table/*stack2*/);
static unsigned int enc_1_sub8 (unsigned int *buf1/*ecx*/, unsigned int seed1/*edx*/, unsigned int seed2/*stack1*/, unsigned int seed3/*stack2*/);
static unsigned int enc_1_sub9 (unsigned int cnt /*ecx*/, unsigned int *buf1/*edx*/, unsigned int *buf2/*stack1*/);

static void my_allmul (unsigned int al, unsigned int ah, unsigned int bl, unsigned int bh, unsigned int *ol, unsigned int *oh)
{
	u64 a,b,o;

	a = (((u64)ah) << 32) | al;
	b = (((u64)bh) << 32) | bl;
	o = a * b;
	*ol = (unsigned int)(o & 0xFFFFFFFF);
	*oh = (unsigned int)(o >> 32);
}

static void my_aulldiv (unsigned int al, unsigned int ah, unsigned int bl, unsigned int bh, unsigned int *ol, unsigned int *oh)
{
	u64 a,b,o;

	a = (((u64)ah) << 32) | al;
	b = (((u64)bh) << 32) | bl;
	o = a / b;
	*ol = (unsigned int)(o & 0xFFFFFFFF);
	*oh = (unsigned int)(o >> 32);
}

static void my_exmul (unsigned int a, unsigned int b, unsigned int *eax, unsigned int *edx)
{
	my_allmul (a, 0, b, 0, eax, edx);
}


void enc_type_1 (unsigned char *out_key/*stack1*/, unsigned char *in_key/*stack2*/)
{
	unsigned int buf1[0x40], buf2[0x40];

	enc_1_zero (buf1, 3);

	enc_1_mix (cipher_table, (unsigned int*)in_key, buf2, 1, buf1);

	memcpy (out_key, buf2, 0xFF);
}

void enc_1_mix (unsigned int *table/*ecx*/, unsigned int *in_key/*stack1*/, unsigned int *out_key/*stack2*/, unsigned int cnt/*stack3*/, unsigned int *buf/*stack4*/)
{
	unsigned int buf1[0x40], buf2[0x40];

	memcpy(buf2, in_key, 0 - negCarryOf(cnt) + 0x100);		    

	if(cnt != 0)
		((unsigned char*)buf2)[0xFF] = 1;

	enc_1_sub1 (buf1, (unsigned char*)buf2);

	if(enc_1_sub2 (table, 0x7FF) != 0)
		enc_1_sub3 (buf1, buf, table);

	enc_1_sub4 (buf1, (unsigned char*)out_key, cnt == 0 ? 0xFF : 0x100);
}

void enc_1_zero (unsigned int *buf/*ecx*/, unsigned int first/*stack1*/)
{
	memset(buf, 0, 0x100);
	*buf = first;
}

void enc_1_sub1 (unsigned int *dst/*ecx*/, unsigned char *src/*stack1*/)
{
	unsigned int ebp, ecx, tmp;
	
	for(ebp=0x40; ebp > 0; ebp--, dst++) {
		for(ecx=0, tmp=0; ecx < 0x20; ecx+=8, src++)
			tmp |= ((unsigned int)(*src)) << ecx;	
		*dst = tmp;
	}
}

unsigned int enc_1_sub2 (unsigned int *buf/*ecx*/, unsigned int i/*stack1*/)
{
	return (buf[i >> 5] >> (i & 0x1F)) & 1;
}

void enc_1_sub3 (unsigned int *buf1/*ecx*/, unsigned int *buf2/*stack1*/, unsigned int *table/*stack2*/)
{
	unsigned int lbuf[0x40];
	unsigned int i,j;

	for(i=0x800; i>0; i--)
		if(enc_1_sub2(buf2, i-1) != 0)
			break;

	enc_1_zero (lbuf, 1);

	if(i > 0) {
		for(j=0;j<i;j++) {
			if(enc_1_sub2(buf2, j) != 0)
				enc_1_sub5 (lbuf, buf1, table);
			enc_1_sub5 (buf1, buf1, table);
		}
	}

	memcpy(buf1, lbuf, 0x100);
}

void enc_1_sub4 (unsigned int *src/*ecx*/, unsigned char *dst/*stack1*/, unsigned int cnt/*stack2*/)
{
	unsigned int i;

	if(cnt == 0)
		return;
     
	for(i=0; i<cnt; i++, dst++)
		*dst = (unsigned char)(src[i>>2] >> (unsigned char)((i & 3) << 3));
	
}

void enc_1_sub5 (unsigned int *buf1/*ecx*/, unsigned int *buf2/*stack1*/, unsigned int *table/*stack2*/)
{
	unsigned int lbuf[0x40*2];

	unsigned int esi = ((0 - negCarryOf(table[0x3F])) & 0x20) + 0x20;

	enc_1_sub6 (esi/*ecx*/, (unsigned char*)lbuf/*edx*/, (unsigned char*)buf1/*stack1*/, (unsigned char*)buf2/*stack2*/);

	enc_1_sub7 (esi/*ecx*/, (unsigned char*)buf1/*edx*/, (unsigned char*)lbuf/*stack1*/, (unsigned char*)table/*stack2*/);

	if(esi < 0x40)
		memset(&buf1[esi], 0, (0x40-esi) * 4);
}

void enc_1_sub6 (unsigned int cnt/*ecx*/, unsigned char *out_buf/*edx*/, unsigned char *buf1/*stack1*/, unsigned char *buf2/*stack2*/)
{
	unsigned int out_buf_ptr, buf1_delta;
	unsigned int i,j;
	unsigned int eax, esi, edi;

	memset(out_buf, 0, cnt * 8);

	if(cnt == 0)
		return;

	out_buf_ptr = (unsigned int)out_buf + cnt * 4;
	buf1_delta = (unsigned int)buf1 - (unsigned int)out_buf;

	for(i=cnt; i>0; i--) {
		edi = (unsigned int)out_buf;
		esi = (unsigned int)buf2;

		for (j=cnt, eax=0; j>0; j--, esi+=4, edi+=4)
			eax = enc_1_sub8 ((unsigned int*)edi, eax, *((unsigned int *)(buf1_delta + out_buf)), *((unsigned int *)esi));

		*((unsigned int *)out_buf_ptr) = eax;
		out_buf += 4;
		out_buf_ptr += 4;
	}

	return;
}

unsigned int enc_1_sub9 (unsigned int cnt /*ecx*/, unsigned int *buf1/*edx*/, unsigned int *buf2/*stack1*/)
{
	if(cnt == 0)
		return 0;

	for(cnt--; cnt>=0; cnt--) {
		if(buf1[cnt] < buf2[cnt])
			return 1;
		else if(buf1[cnt] > buf2[cnt])
			return 0;
	}

	return 0;
}

unsigned int enc_1_sub8 (unsigned int *buf1/*ecx*/, unsigned int seed1/*edx*/, unsigned int seed2/*stack1*/, unsigned int seed3/*stack2*/)
{
	unsigned int eax, edx;
	edx = seed1;

	my_exmul (seed2, seed3, &eax, &edx);

	edx += addCarryOf(eax,*buf1);
	eax += *buf1;

	edx += addCarryOf(eax,seed1);
	eax += seed1;

	*buf1 = eax;

	return edx;
}


void enc_1_sub7 (unsigned int cnt/*ecx*/, unsigned char *buf1/*edx*/, unsigned char *buf2/*stack1*/, unsigned char *table/*stack2*/)
{
	unsigned int steffi, sasha;
	unsigned int kobi, cori;
	unsigned int i, table_val;
	unsigned int eax, ebx, ecx, edx, esi, edi;

	esi = (unsigned int)table;

	table_val = *((unsigned int *)(esi + cnt * 4 - 4));

	if(cnt == 0)
		return;

	steffi = edx = (unsigned int)buf2 + cnt * 4 - 4;
	sasha = edi = (unsigned int)buf2 + (cnt * 2 - 1) * 4;

	for(i=cnt; i>0; i--) {
		ecx = table_val;
		kobi = eax = *((unsigned int *)edi);

		if(ecx != 0xFFFFFFFF) {
			edi = *((unsigned int *)(edi-4));
			ebx = 0;
			my_allmul (eax, ebx, ebx, 1, &eax, &edx);
			ebx += edx + addCarryOf(edi,eax);
			edi += eax;
			edx = table_val + 1;
			my_aulldiv (edi, ebx, edx, 0, &eax, &edx);
			edi = sasha;
			kobi = eax;	
		}

		ebx = 0;

		if(cnt != 0) {
			edi = esi;
			esi = steffi;

			for(cori=cnt; cori>0; cori--) {
				edx = kobi;
				ecx = *((unsigned int *)edi);
				my_allmul (ecx, 0, edx, 0, &eax, &edx);
				edx += addCarryOf(eax, ebx);
				eax += ebx;
				ecx = *((unsigned int *)esi);
				ebx = edx;

				if(ecx < eax)
					ebx++;

				ecx -= eax;
				*((unsigned int *)esi) = ecx;

				edi += 4;
				esi += 4;
			}
	
			esi = (unsigned int)table;
			edi = sasha;
		}

		*((unsigned int *)edi) -= ebx;

		while(1) {
			if(*((unsigned int *)edi) == 0) {
				if(enc_1_sub9 (cnt, (unsigned int*)steffi, (unsigned int*)esi) != 0)
					break;
			}

			edi = 0;

			if(cnt != 0) {
				for(ebx=cnt, eax=steffi; ebx>0; ebx--) {
					ecx = *((unsigned int *)eax);
					edx = *((unsigned int *)esi);
	
					if(ecx != 0 || edi == 0) {
						ecx -= edi;
						*((unsigned int*)eax) = ecx;
						edi = 0 - subCarryOf(ecx,edx);
						edi = 0 - edi; 
					} else {
						ecx |= 0xFFFFFFFF;
					}

					ecx -= edx;
					*((unsigned int *)eax) = ecx;
					esi += 4;
					eax += 4;
				}
	
				esi = (unsigned int)table;
			}

			eax = sasha;
			ecx = *((unsigned int *)eax);
			ecx -= edi;
			edi = eax;
			*((unsigned int *)eax) = ecx;
		}

		edi -= 4;
		sasha = edi;
		steffi -= 4;
	}

	if(cnt != 0)
		memcpy (buf1, buf2, cnt*4);

}
