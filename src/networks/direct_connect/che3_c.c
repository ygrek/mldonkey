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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
#include <dclib/clist.h>
#include <dclib/cbytearray.h>
*/
#include "che3_c.h"

/******************************************************/
/* patch from David Toth for  */
/* File sharing list Windows (DC) compatibiliy bugfix */
/* (byte 4 meaning and computation).  */
/******************************************************/

/*********************************************************************/
/* When the file list of another user is retrieved, it is compressed */
/* The data format is described below.   
byte 0: 'H'
byte 1: 'E'
byte 2: '3'
byte 3: 0xD
byte 4: 8 bit parity of the decoded data (the result of all bytes "xor"-ed)
byte 5 - 8 : it is a number encoded in little endian. It it the number of bytes to produce.
byte 9 - 10: it is a number encoded in little endian. This number gives the number of couples.
 Let's name this number as N
byte 11 - (10+2*N): this is a list of couple. The first byte is a character (the one before compression),
 the second byte gives the length of the compressed pattern. Let's name this value L.
byte (11+2*N) - (11+2*N+ ((sum(L)+7)&~7): a bit barbare :) To simplify. Let's name K, the sum of all L values.
 Here, you find a string of K bits (byte aligned). The L0 first bits are the encoded value of 
 caracter of couple 0. The next Lx bits are the encoded value of caractere of couple x.
 Note: each byte are used from the lower bit to the upper bit.
after this string, you have the encoded string.
When data are finally decoded. The format is always:
directory name \xD\xA
\x9 filename | filesize \xD\xA
\x9 filename | filesize \xD\xA
\x9 filename | filesize \xD\xA
\x9 filename | filesize \xD\xA
\x9 filename | filesize \xD\xA

*/


/* ============= Decoding functions ================ */

/******************************************************/
/*get 1 bit from the current bit position inside data */
/******************************************************/
unsigned long get_bit(const unsigned char *data, unsigned long *cur_pos)
{
 unsigned long out;

out=((unsigned long)(data[(*cur_pos)/8]>>((*cur_pos)&7)))&1;

 (*cur_pos)++;

 return out;
}

/*********************************************************/
/* get nb_bits from the current bit position inside data */
/*********************************************************/
unsigned long get_bits(const unsigned char *data, unsigned long *cur_pos, int nb_bit)
{
 int i;
 unsigned long res=0;

 for(i=0;i<nb_bit;i++)
 {
  res=(res<<1)|get_bit(data,cur_pos);
 }
 return res;
}

/**************************************************/
/* decompress data compressed using HE3 algorithm */
/**********************************************************/
/* input: a GByteArray containing HE3 compressed data */
/* output: a GString containing uncompressed data or NULL */
/**********************************************************/
char *decode_he3_data(const char *data_string, int data_len, int* final_len)
{
  char *output_string = NULL;
  int output_len = 0;
  
  
  if((data_string[0]=='H')&&(data_string[1]=='E')&&(data_string[2]=='3')&&(data_string[3]==0xD))
  {
    char *decode_array=NULL;
    int pos;
    int nb_couple;
    int max_len=0;  /* max size of encoded pattern */
    int ttl_len=0;  /* total size of all encoded patterns */
    unsigned long offset_pattern;
    unsigned long offset_encoded;
    int nb_output;
    int l=0;

  /* compute the number of bytes to produce */
    nb_output=(((int)(data_string[8]))&255);
    nb_output<<=8;
    nb_output|=(((int)(data_string[7]))&255);
    nb_output<<=8;
    nb_output|=(((int)(data_string[6]))&255);
    nb_output<<=8;
    nb_output|=(((int)(data_string[5]))&255);

  /* compute the number of couples */
    nb_couple=data_string[9];
    nb_couple+=((((int)(data_string[10]))&255)<<8);
    
    for(pos=0;pos<nb_couple;pos++)
    {
      int v;
      
      v=(((int)(data_string[12+pos*2]))&255);
      if(v>max_len)
        max_len=v;
      ttl_len+=v;
    }
    
  //decode_array=g_byte_array_new();
  decode_array = NULL;
  /* theorytically, max_len could reach up to 255 */
  
  
  decode_array = malloc(1<<(max_len+1));

/* but really, a  value above 15 is not oftenly encountered */
                          /* thus the algorithm needs no more than ... 64KB */
                        /* raisonnable value is upto 23 (requires 8MB) */

  if(decode_array!=NULL)
  {
   /* clear the decode array */
   /* the decode array is technically a binary tree */
   /* if the depth of the tree is big (let's say more than 10), */
   /* storing the binary tree inside an array becomes very memory consumming */
   /* but I am too lazy to program all binary tree creation/addition/navigation/destruction functions :) */
   memset(decode_array,0,1<<(max_len+1));
   
   offset_pattern=8*(11+nb_couple*2);  /* position of the pattern block, it is just after the list of couples */
   offset_encoded=offset_pattern + ((ttl_len+7)&~7); /* the encoded data are just after */
                    /* the pattern block (rounded to upper full byte) */

   /* decode_array is a binary tree. byte 0 is the level 0 of the tree. byte 2-3, the level 1, byte 4-7, the level 2, */
   /* in decode array, a N bit length pattern having the value K is its data at the position: */
   /* 2^N + (K&((2^N)-1)) */
   /* due to the fact K has always N bit length, the formula can be simplified into: */
   /* 2^N + K */
   for(pos=0;pos<nb_couple;pos++)
   {
    unsigned int v_len;
    unsigned long value;

    v_len=(((int)(data_string[12+pos*2]))&255); /* the number of bit required */

    value=get_bits(data_string,&offset_pattern,v_len);
    decode_array[(1<<v_len)+ value]=data_string[11+pos*2];  /* the character */
   }

   /* now, its time to decode */
   //while(output->len!=nb_output)
      output_string = malloc(nb_output+1);
      output_string[nb_output] = 0;
   while(output_len != nb_output)
   //while(l!=nb_output)
   {
    unsigned long cur_val;
    unsigned int nb_bit_val;

    cur_val=get_bit(data_string,&offset_encoded);  /* get one bit */
    nb_bit_val=1;

    while(decode_array[(1<<nb_bit_val) + cur_val ]==0)
    {
     cur_val=(cur_val<<1)|get_bit(data_string,&offset_encoded);
     nb_bit_val++;
    }
    
    //output=g_string_append_c(output,decode_array->data[(1<<nb_bit_val) + cur_val ]);
    output_string[output_len++] = decode_array[(1<<nb_bit_val) + cur_val ];
    l++;

   }
   //g_byte_array_free(decode_array,TRUE);
   free(decode_array);
  }
 }

 {
  int i;
  unsigned char parity=0;

  for(i=0;i<output_len;i++)
   parity^=output_string[i];

 }

  *final_len = output_len;
 return output_string;
}


#if 0
/* ================= Encoding functions ======================== */

//static gint huf_insert_glist(gconstpointer a, gconstpointer b)
int huf_insert_glist(void * a, void * b)
{
 if(((const HUFNODE*)a)->occur<((const HUFNODE*)b)->occur)
  return -1;  /* a is must be before b */
 else if(((const HUFNODE*)a)->occur>((const HUFNODE*)b)->occur)
   return 1;  /* a is must be after b */

 /* if both have the same occurences, the one without son comes before */
 if( (((const HUFNODE*)a)->left==NULL) && (((const HUFNODE*)b)->left==NULL) )
  return -1;
 
 if(((const HUFNODE*)a)->left==NULL)
  return -1;

 return 1;
}
#endif

/**************************************************************************/
/* recursively scan all nodes of the huffman tree and fill encoding table */
/**************************************************************************/
void use_hufnode(HUFENCODE tbl_enc[256], HUFNODE *node,unsigned int bits_len, unsigned long bits)
{
 if(node->left!=NULL)
 { /* it is a node, right is always != NULL */
  use_hufnode(tbl_enc,node->left,bits_len+1,(bits<<1)|0);
  use_hufnode(tbl_enc,node->right,bits_len+1,(bits<<1)|1);
 }
 else
 { /* it is a leaf, right is always NULL */
  int idx=((int)node->val)&255;
  tbl_enc[idx].bits_len=bits_len;
  tbl_enc[idx].bits=bits;
//  printf("huf: bits_len: %u  pattern: %lu ",bits_len,bits);
//  printf("leaf: %d (%c)\n",idx,node->val);

//  if(bits_len>8*sizeof(unsigned long))  /* compared with the size of unsigned long in bits */
//   disp_msg(ERR_MSG,"user_hufnode","encoded value cannot fit into unsigned long",NULL);
 }

 return;
}

/*****************************************************************/
/* recursively free memory used by all nodes of the huffman tree */
/*****************************************************************/
void free_hufnode(HUFNODE *node)
{
// printf("free node %c\n",node->val);
 if(node==NULL)
  return;
 if(node->left!=NULL)
  free_hufnode(node->left);
 if(node->right!=NULL)
  free_hufnode(node->right);
 free(node);
}


void add_bit(char *data, unsigned long *bit_pos, unsigned char bit_value)
{
 if(((*bit_pos)&7)==0)  /* starting a new byte ? */
 {
  //data=g_byte_array_append(data,&v,1);
    data[(*bit_pos)/8]|= 0;
 }

 /* due to the fact we always add 0 as new byte, we don't have to set bit having 0 as value */
 /* but just 1 */
 if(bit_value!=0)
 {
  data[(*bit_pos)/8]|= (1<<((*bit_pos)&7));
 }

 (*bit_pos)++;
}

/**************************************/
/* append the pattern to data@bit_pos */
/**************************************/
void add_bits(char *data, unsigned long *bit_pos, unsigned long pattern, unsigned int pattern_length)
{
 unsigned long i;

 for(i=0;i<pattern_length;i++)
 {
    add_bit(data, bit_pos,(unsigned char)(pattern>>(pattern_length-1-i))&1 );
  /* use pattern from upper to lower bit */
 }
}


#if 0
static void disp_huf(GList *pre_tree)
{
  int i;
  HUFNODE *w;
  printf("---\n");
 //for(i=0;i<g_list_length(pre_tree);i++)
  for(i=0;i<pre_tree->size;i++)
  {
    w=g_list_nth_data(pre_tree,i);  /* get the first HUFNODE of the list */
    
    printf("occur: %ld, left=%p, right=%p, val=%02X\n",w->occur,w->left,w->right,w->val);
  }
}
#endif
/*****************************************************************************************/
/* compress data compressed using an Huffman algorithm and store it in HE3 usable format */
/*****************************************************************************************/
/* input: a GString containing a string to compress  */
/* output: a GByteArray containing compressed data or NULL */
/***********************************************************/
static HUFNODE *pre_tree[256];
static int pre_tree_len = 0;

void insert_node(HUFNODE *nw){
  
  int j;
  
  j=pre_tree_len++;
  while(j>0 && (pre_tree[j-1]->occur < nw->occur || (pre_tree[j-1]->occur == nw->occur && pre_tree[j-1]->left == NULL && nw->left != NULL))){
    pre_tree[j] = pre_tree[j-1];
//    printf("move %d -> %d\n", j- 1, j);
    j--;
  }
  pre_tree[j] = nw;
//  printf("insert at %d\n", j);
}      

HUFNODE *remove_node()
{
  pre_tree_len--;
//  printf("remove %d\n", pre_tree_len);
  return pre_tree[pre_tree_len];
}

char *encode_he3_data(const char *str, int len, int* final_len)
{
  unsigned long occur[256];
  HUFENCODE tbl_enc[256];
  
  long i;
  char *data = NULL;
  HUFNODE *root_huf=NULL;
  int nb_val=0;
  unsigned long bit_pos;
  char *pos;
  
  if((str==NULL)||(len==0)) {
    *final_len = 0;
    return NULL;
  }

 /* count the number of times each character appears */
  memset(occur,0,sizeof(occur));

  for(i=0;i<len;i++)
  {
    occur[((int)(str[i]))&255]++;
  }

 /* now, we will build the huffman tree */
  pre_tree_len = 0;

 /* stage 1: create all the leafs */
  for(i=0;i<256;i++)
  {
    if(occur[i]!=0) 
    {
      HUFNODE *nw;
      
      nw=(HUFNODE*)malloc(sizeof(HUFNODE));
      nw->occur=occur[i];
      nw->left=NULL;
      nw->right=NULL;
      nw->val=(unsigned char)i;
      insert_node(nw);
      nb_val++;
    }
  }

  while(pre_tree_len>1)
  {
    HUFNODE *nw;
    
    nw=(HUFNODE*)malloc(sizeof(HUFNODE));
    
    nw->left=remove_node();
    nw->right=remove_node ();
    
    nw->occur=nw->left->occur+nw->right->occur;
    nw->val=0;

    insert_node(nw);
  }
  #if 0
    disp_huf(pre_tree);
  #endif
 /* now, the huffman tree is done */
 //root_huf=g_list_nth_data(pre_tree,0);
  root_huf=pre_tree[0];
  pre_tree_len=0;
  
  memset(tbl_enc,0,sizeof(tbl_enc));

 /* now, we will compute encoding pattern from huffman tree */
  use_hufnode(tbl_enc,root_huf,0,0);

 /* well, encoding table is now done, we can encode the data */
 //data=g_byte_array_new();
  data = malloc(len + 256 * 10);
  pos=data;
 /* set the initial HE3 header */
  {
    unsigned char he3_header[]={'H','E','3',0xD,
      0,     /* parity: all uncompressed bytes xor'ed */
      0,0,0,0,   /* number of bytes to produce (little-endian) */
      0,0};    /* number of couples */

  /* patch from David Toth (byte 4: parity computation) */
    unsigned char parity=0;

    for(i=0;i<len;i++)
      parity^=str[i];
    he3_header[4]=(unsigned char)(parity&255);

  //he3_header[5]=str->len&255;
    he3_header[5]=(unsigned char)(len&255);
    he3_header[6]=(unsigned char)((len>>8)&255);
    he3_header[7]=(unsigned char)((len>>16)&255);
    he3_header[8]=(unsigned char)((len>>24)&255);
    he3_header[9]=(unsigned char)(nb_val&255);
    he3_header[10]=(unsigned char)((nb_val>>8)&255);

  //data=g_byte_array_append(data,he3_header,11);
    memmove(pos, he3_header, 11);
    pos+=11;
  }

 /* add the couple list (character, pattern length) */
  for(i=0;i<256;i++)
  {
    if(occur[i]!=0)
    {
      *pos++ = (unsigned char)i;
      *pos++ = (unsigned char)tbl_enc[i].bits_len;
    }
  }

/*  pos--; *//* increment pos when bit_pos mod 8 = 0. The semantics of pos
  change: instead of the next byte to write, it is the previous byte written. */

 /* and now, add all the patterns */
  bit_pos=(pos - data)*8;
  for(i=0;i<256;i++)
  {
    if(occur[i]!=0)
    {
      add_bits(data,&bit_pos,tbl_enc[i].bits, tbl_enc[i].bits_len);
    }
  }
  
  bit_pos=(bit_pos+7)&~7;  /* move to the beginning of the next byte */

 /* now, we encode the string */
  for(i=0;i<len;i++)
  {
    int idx=((int)str[i])&255;
    
    add_bits(data,&bit_pos,tbl_enc[idx].bits, tbl_enc[idx].bits_len);
  }

  bit_pos=(bit_pos+7)&~7;  /* move to the beginning of the next byte */
  *final_len = (bit_pos / 8)+1;

//  printf("final len %d\n", *final_len);

 /* free huffman tree */
  free_hufnode(root_huf);
  
  return data;
}



#include "caml/mlvalues.h"
#include "caml/fail.h"
#include "caml/alloc.h"


value ml_che3_decompress(value s_v)
{
  const char *s = String_val(s_v);
  int len = string_length(s_v);
  char *result;
  int final_len;
  
  result = decode_he3_data(s, len, &final_len);

  return caml_alloc_initialized_string(final_len, result);
}

value ml_che3_compress(value s_v)
{
  const char *s = String_val(s_v);
  int len = string_length(s_v);
  char *result;
  int final_len;
  
  result = encode_he3_data(s, len, &final_len);

  return caml_alloc_initialized_string(final_len, result);
}

#if 0

int main(int argc,char **argv)
{
  int final_len;
  char *out;
  char *in;
  unsigned char tst[54]={0x48,0x45,0x33,0x0D,0x12,0x24,0x00,0x00,0x00,0x0B,0x00,0x09,0x04,0x0A,0x03,0x0D,0x04,0x2E,0x04,0x32,0x04,0x61,0x02,0x62,0x03,0x65,0x05,0x73,0x05,0x74,0x03,0x7C,0x04,0xA6,0xF4,0xE2,0x21,0xAE,0x01,0x61,0x71,0x29,0xFB,0xFF,0xFF,0xBE,0x75,0xA5,0x0C,0x00,0xF0,0xAD,0x2B,0x05};

  out=decode_he3_data(tst, 54, &final_len);

  printf("LEN[%d] = [%s]\n",final_len, out);

  return 0;
}

int main(int argc,char **argv)
{
  int final_len;
  char *out;
  char *in;
  int start_len;
  int i;

  in= encode_he3_data(argv[1], strlen(argv[1]), &start_len);
  for(i=0; i<start_len;i++)
    printf("%4x,", in[i]);
  printf("\n");
  out=decode_he3_data(in, start_len, &final_len);

  printf("LEN[%d] = [%s]\n",final_len, out);

  return 0;
}
#endif
