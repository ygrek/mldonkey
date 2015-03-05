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

#include "./md5.h"

#include <stdio.h>
#include <stdlib.h>

static const unsigned int smalltable[256] = {
    0x00000000,0x77073096,0xEE0E612C,0x990951BA,
    0x076DC419,0x706AF48F,0xE963A535,0x9E6495A3,
    0x0EDB8832,0x79DCB8A4,0xE0D5E91E,0x97D2D988,
    0x09B64C2B,0x7EB17CBD,0xE7B82D07,0x90BF1D91,
    0x1DB71064,0x6AB020F2,0xF3B97148,0x84BE41DE,
    0x1ADAD47D,0x6DDDE4EB,0xF4D4B551,0x83D385C7,
    0x136C9856,0x646BA8C0,0xFD62F97A,0x8A65C9EC,
    0x14015C4F,0x63066CD9,0xFA0F3D63,0x8D080DF5,
    0x3B6E20C8,0x4C69105E,0xD56041E4,0xA2677172,
    0x3C03E4D1,0x4B04D447,0xD20D85FD,0xA50AB56B,
    0x35B5A8FA,0x42B2986C,0xDBBBC9D6,0xACBCF940,
    0x32D86CE3,0x45DF5C75,0xDCD60DCF,0xABD13D59,
    0x26D930AC,0x51DE003A,0xC8D75180,0xBFD06116,
    0x21B4F4B5,0x56B3C423,0xCFBA9599,0xB8BDA50F,
    0x2802B89E,0x5F058808,0xC60CD9B2,0xB10BE924,
    0x2F6F7C87,0x58684C11,0xC1611DAB,0xB6662D3D,
    0x76DC4190,0x01DB7106,0x98D220BC,0xEFD5102A,
    0x71B18589,0x06B6B51F,0x9FBFE4A5,0xE8B8D433,
    0x7807C9A2,0x0F00F934,0x9609A88E,0xE10E9818,
    0x7F6A0DBB,0x086D3D2D,0x91646C97,0xE6635C01,
    0x6B6B51F4,0x1C6C6162,0x856530D8,0xF262004E,
    0x6C0695ED,0x1B01A57B,0x8208F4C1,0xF50FC457,
    0x65B0D9C6,0x12B7E950,0x8BBEB8EA,0xFCB9887C,
    0x62DD1DDF,0x15DA2D49,0x8CD37CF3,0xFBD44C65,
    0x4DB26158,0x3AB551CE,0xA3BC0074,0xD4BB30E2,
    0x4ADFA541,0x3DD895D7,0xA4D1C46D,0xD3D6F4FB,
    0x4369E96A,0x346ED9FC,0xAD678846,0xDA60B8D0,
    0x44042D73,0x33031DE5,0xAA0A4C5F,0xDD0D7CC9,
    0x5005713C,0x270241AA,0xBE0B1010,0xC90C2086,
    0x5768B525,0x206F85B3,0xB966D409,0xCE61E49F,
    0x5EDEF90E,0x29D9C998,0xB0D09822,0xC7D7A8B4,
    0x59B33D17,0x2EB40D81,0xB7BD5C3B,0xC0BA6CAD,
    0xEDB88320,0x9ABFB3B6,0x03B6E20C,0x74B1D29A,
    0xEAD54739,0x9DD277AF,0x04DB2615,0x73DC1683,
    0xE3630B12,0x94643B84,0x0D6D6A3E,0x7A6A5AA8,
    0xE40ECF0B,0x9309FF9D,0x0A00AE27,0x7D079EB1,
    0xF00F9344,0x8708A3D2,0x1E01F268,0x6906C2FE,
    0xF762575D,0x806567CB,0x196C3671,0x6E6B06E7,
    0xFED41B76,0x89D32BE0,0x10DA7A5A,0x67DD4ACC,
    0xF9B9DF6F,0x8EBEEFF9,0x17B7BE43,0x60B08ED5,
    0xD6D6A3E8,0xA1D1937E,0x38D8C2C4,0x4FDFF252,
    0xD1BB67F1,0xA6BC5767,0x3FB506DD,0x48B2364B,
    0xD80D2BDA,0xAF0A1B4C,0x36034AF6,0x41047A60,
    0xDF60EFC3,0xA867DF55,0x316E8EEF,0x4669BE79,
    0xCB61B38C,0xBC66831A,0x256FD2A0,0x5268E236,
    0xCC0C7795,0xBB0B4703,0x220216B9,0x5505262F,
    0xC5BA3BBE,0xB2BD0B28,0x2BB45A92,0x5CB36A04,
    0xC2D7FFA7,0xB5D0CF31,0x2CD99E8B,0x5BDEAE1D,
    0x9B64C2B0,0xEC63F226,0x756AA39C,0x026D930A,
    0x9C0906A9,0xEB0E363F,0x72076785,0x05005713,
    0x95BF4A82,0xE2B87A14,0x7BB12BAE,0x0CB61B38,
    0x92D28E9B,0xE5D5BE0D,0x7CDCEFB7,0x0BDBDF21,
    0x86D3D2D4,0xF1D4E242,0x68DDB3F8,0x1FDA836E,
    0x81BE16CD,0xF6B9265B,0x6FB077E1,0x18B74777,
    0x88085AE6,0xFF0F6A70,0x66063BCA,0x11010B5C,
    0x8F659EFF,0xF862AE69,0x616BFFD3,0x166CCF45,
    0xA00AE278,0xD70DD2EE,0x4E048354,0x3903B3C2,
    0xA7672661,0xD06016F7,0x4969474D,0x3E6E77DB,
    0xAED16A4A,0xD9D65ADC,0x40DF0B66,0x37D83BF0,
    0xA9BCAE53,0xDEBB9EC5,0x47B2CF7F,0x30B5FFE9,
    0xBDBDF21C,0xCABAC28A,0x53B39330,0x24B4A3A6,
    0xBAD03605,0xCDD70693,0x54DE5729,0x23D967BF,
    0xB3667A2E,0xC4614AB8,0x5D681B02,0x2A6F2B94,
    0xB40BBE37,0xC30C8EA1,0x5A05DF1B,0x2D02EF8D
};

static const unsigned short checksumtable[256]={
    0x0000,0x1021,0x2042,0x3063,0x4084,0x50A5,0x60C6,0x70E7,
    0x8108,0x9129,0xA14A,0xB16B,0xC18C,0xD1AD,0xE1CE,0xF1EF,
    0x1231,0x0210,0x3273,0x2252,0x52B5,0x4294,0x72F7,0x62D6,
    0x9339,0x8318,0xB37B,0xA35A,0xD3BD,0xC39C,0xF3FF,0xE3DE,
    0x2462,0x3443,0x0420,0x1401,0x64E6,0x74C7,0x44A4,0x5485,
    0xA56A,0xB54B,0x8528,0x9509,0xE5EE,0xF5CF,0xC5AC,0xD58D,
    0x3653,0x2672,0x1611,0x0630,0x76D7,0x66F6,0x5695,0x46B4,
    0xB75B,0xA77A,0x9719,0x8738,0xF7DF,0xE7FE,0xD79D,0xC7BC,
    0x48C4,0x58E5,0x6886,0x78A7,0x0840,0x1861,0x2802,0x3823,
    0xC9CC,0xD9ED,0xE98E,0xF9AF,0x8948,0x9969,0xA90A,0xB92B,
    0x5AF5,0x4AD4,0x7AB7,0x6A96,0x1A71,0x0A50,0x3A33,0x2A12,
    0xDBFD,0xCBDC,0xFBBF,0xEB9E,0x9B79,0x8B58,0xBB3B,0xAB1A,
    0x6CA6,0x7C87,0x4CE4,0x5CC5,0x2C22,0x3C03,0x0C60,0x1C41,
    0xEDAE,0xFD8F,0xCDEC,0xDDCD,0xAD2A,0xBD0B,0x8D68,0x9D49,
    0x7E97,0x6EB6,0x5ED5,0x4EF4,0x3E13,0x2E32,0x1E51,0x0E70,
    0xFF9F,0xEFBE,0xDFDD,0xCFFC,0xBF1B,0xAF3A,0x9F59,0x8F78,
    0x9188,0x81A9,0xB1CA,0xA1EB,0xD10C,0xC12D,0xF14E,0xE16F,
    0x1080,0x00A1,0x30C2,0x20E3,0x5004,0x4025,0x7046,0x6067,
    0x83B9,0x9398,0xA3FB,0xB3DA,0xC33D,0xD31C,0xE37F,0xF35E,
    0x02B1,0x1290,0x22F3,0x32D2,0x4235,0x5214,0x6277,0x7256,
    0xB5EA,0xA5CB,0x95A8,0x8589,0xF56E,0xE54F,0xD52C,0xC50D,
    0x34E2,0x24C3,0x14A0,0x0481,0x7466,0x6447,0x5424,0x4405,
    0xA7DB,0xB7FA,0x8799,0x97B8,0xE75F,0xF77E,0xC71D,0xD73C,
    0x26D3,0x36F2,0x0691,0x16B0,0x6657,0x7676,0x4615,0x5634,
    0xD94C,0xC96D,0xF90E,0xE92F,0x99C8,0x89E9,0xB98A,0xA9AB,
    0x5844,0x4865,0x7806,0x6827,0x18C0,0x08E1,0x3882,0x28A3,
    0xCB7D,0xDB5C,0xEB3F,0xFB1E,0x8BF9,0x9BD8,0xABBB,0xBB9A,
    0x4A75,0x5A54,0x6A37,0x7A16,0x0AF1,0x1AD0,0x2AB3,0x3A92,
    0xFD2E,0xED0F,0xDD6C,0xCD4D,0xBDAA,0xAD8B,0x9DE8,0x8DC9,
    0x7C26,0x6C07,0x5C64,0x4C45,0x3CA2,0x2C83,0x1CE0,0x0CC1,
    0xEF1F,0xFF3E,0xCF5D,0xDF7C,0xAF9B,0xBFBA,0x8FD9,0x9FF8,
    0x6E17,0x7E36,0x4E55,0x5E74,0x2E93,0x3EB2,0x0ED1,0x1EF0
};


#if 0
typedef struct {
  MD5_CTX md5_context;
  int todo;
  unsigned int smallhash;
} FST_CTX;

void FSTInit(FST_CTX *);
void FSTUpdate (FST_CTX*, unsigned char*, unsigned int);
void FSTFinal(unsigned char [20], FST_CTX*);

void FSTInit(FST_CTX *context)
{
  ml_MD5Init(& (context->md5_context) );
  context->todo = 307200;
  context->smallhash = 0xffffffff;
}

void FSTFinal(unsigned char buffer[20], FST_CTX *context)
{
  ml_MD5Final(buffer, & (context->md5_context) );

	// and append it to md5 hash
  buffer[16] = context->smallhash & 0xff;
  buffer[17] = (context->smallhash >> 8) & 0xff;
  buffer[18] = (context->smallhash >> 16) & 0xff;
  buffer[19] = (context->smallhash >> 24) & 0xff;
}

void FSTUpdate (FST_CTX*context, unsigned char*buffer, unsigned int len)
{
  if(context->todo>0){
    int baselen = len;
    if (baselen > context->todo) baselen = context->todo;
    ml_MD5Update( &(context->md5_context), buffer, baselen);
    buffer += baselen;
    context->todo -= baselen;
    len -= baselen;
  }  

}

#endif
/*****************************************************************************/

// updates 4 byte small hash that is concatenated to the md5 of the first
// 307200 bytes of the file. set hash to 0xffffffff for first run
static unsigned int fst_hash_small (unsigned char* data, unsigned int len, unsigned int smallhash)
{
  unsigned int i;
  
  for(i=0; i<len; ++i)
    smallhash = smalltable[data[i] ^ (smallhash & 0xff)] ^ (smallhash >> 8);

  return smallhash;
}


#include "caml/mlvalues.h"

/* returns checksum of fzhash */
unsigned short fst_hash_checksum (unsigned char *hash)
{
        unsigned short sum = 0;
        int i;

        /* calculate 2 byte checksum used in the URL from 20 byte fthash */
        for (i = 0; i < 20; i++)
                sum = checksumtable[hash[i] ^ (sum >> 8)] ^ (sum << 8);

        return (sum & 0x3fff);
}

#define FST_HASH_CHUNK 307200
/*****************************************************************************/

// hash file
int fst_hash_file (unsigned char *fth, char *file, int64_t filesize)
{
  FILE *fp;
  unsigned char *buf;
  size_t len;
  ml_MD5Context md5_ctx;
  unsigned int smallhash;
    
  if((fp = fopen (file, "rb")) == NULL)
    return 0;

	// md5 first FST_HASH_CHUNK bytes of file
  buf = malloc (FST_HASH_CHUNK);
  
  len = fread (buf, 1, FST_HASH_CHUNK, fp);
  if(len == 0 && !feof(fp))
  {
    free (buf);
    fclose (fp);
    return 0;
  }
  
  ml_MD5Init(&md5_ctx);
  ml_MD5Update(&md5_ctx, buf, len);
  ml_MD5Final(fth, &md5_ctx);

	// calculate 4-byte small hash
  smallhash = 0xffffffff;
  
  if (filesize > FST_HASH_CHUNK)
  {
    size_t offset = 0x100000;
    size_t lastpos = FST_HASH_CHUNK;
    size_t endlen;
    while(offset+2*FST_HASH_CHUNK < filesize)
    {
      if (fseek (fp, offset, SEEK_SET) < 0 || fread (buf, 1, FST_HASH_CHUNK, fp) < FST_HASH_CHUNK)
      {
        free (buf);
        fclose (fp);
        return 0;
      }
      smallhash = fst_hash_small (buf, FST_HASH_CHUNK, smallhash);
      lastpos = offset + FST_HASH_CHUNK;
      offset <<= 1;
    }
    endlen = filesize - lastpos;
    if (endlen > FST_HASH_CHUNK)
      endlen = FST_HASH_CHUNK;
    if (fseek (fp, filesize - endlen, SEEK_SET) < 0 || fread (buf, 1, endlen, fp) < endlen)
    {
      free (buf);
      fclose (fp);
      return 0;
    }
    smallhash = fst_hash_small (buf, endlen, smallhash);
  }
	
	// xor smallhash with filesize
  smallhash ^= filesize;
  
	// and append it to md5 hash
    fth[16] = smallhash & 0xff;
    fth[17] = (smallhash >> 8) & 0xff;
    fth[18] = (smallhash >> 16) & 0xff;
    fth[19] = (smallhash >> 24) & 0xff;

    free (buf);
	fclose (fp);
	
  return 1;
}


void fst_hash_string (unsigned char *fth, unsigned char *file, int64_t filesize)
{
  unsigned char * buf = file;
  size_t len = filesize;
  ml_MD5Context md5_ctx;
  unsigned int smallhash;
  
  if (len > FST_HASH_CHUNK) len = FST_HASH_CHUNK;  
  
  ml_MD5Init(&md5_ctx);
  ml_MD5Update(&md5_ctx, buf, len);
  ml_MD5Final(fth, &md5_ctx);

	// calculate 4-byte small hash
  smallhash = 0xffffffff;
  
  if (filesize > FST_HASH_CHUNK)
  {
    size_t offset = 0x100000;
    size_t lastpos = FST_HASH_CHUNK;
    size_t endlen;
    while(offset+2*FST_HASH_CHUNK < filesize)
    {
      buf = file + offset;
      smallhash = fst_hash_small (buf, FST_HASH_CHUNK, smallhash);
      lastpos = offset + FST_HASH_CHUNK;
      offset <<= 1;
    }
    endlen = filesize - lastpos;
    if (endlen > FST_HASH_CHUNK)
      endlen = FST_HASH_CHUNK;
    buf = file + (filesize - endlen);
    smallhash = fst_hash_small (buf, endlen, smallhash);
  }
	
	// xor smallhash with filesize
  smallhash ^= filesize;
  
	// and append it to md5 hash
  fth[16] = smallhash & 0xff;
  fth[17] = (smallhash >> 8) & 0xff;
  fth[18] = (smallhash >> 16) & 0xff;
  fth[19] = (smallhash >> 24) & 0xff;
}




#include "caml/fail.h"

value fst_hash_file_ml(value digest, value filename, value filesize)
{
  if(fst_hash_file(String_val(digest), String_val(filename), 
        Int64_val(filesize))) return Val_unit;
  failwith("Exception during FST computation");
}

value fst_hash_string_ml(value digest, value s, value size)
{
  fst_hash_string(String_val(digest), String_val(s), Int_val(size));
  return Val_unit;
}

value fst_hash_checksum_ml(value digest)
{
  return Val_int(fst_hash_checksum(String_val(digest)));
}
