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
