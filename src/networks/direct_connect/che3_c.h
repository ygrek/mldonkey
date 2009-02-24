/* rewrite in C and caml stubs by b8_bavard (2002) */
/* rewrite to class without glib by Mathias Küster (2002) */

/* DCTC - a Direct Connect text clone for Linux
 * Copyright (C) 2001 Eric Prevoteau
 *
 * he3.c: Copyright (C) Eric Prevoteau <www@ac2i.tzo.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#ifndef __HE3_H__
#define __HE3_H__

typedef struct hufnode
{
	unsigned long occur;
	struct hufnode *left,*right;		/* son nodes (left and right are both either not null or either null) */
	unsigned char val;					/* if left&right!=NULL, val is the encoded character, else, it has no usage */
} HUFNODE;

typedef struct
{
	unsigned int bits_len;				/* number of bits in the bitfield used to encode the value */
	unsigned long bits;					/* bitfield containing the encoding pattern */
												/* a N bits_len pattern is stored inside bits from bit N-1 to bit 0 */
} HUFENCODE;

#endif
