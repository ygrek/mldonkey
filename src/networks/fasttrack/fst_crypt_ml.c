/* Copyright 2001, 2002 b8_bavard, b8_fee_carabine, INRIA */
/*
    This file is part of mldonkey.

    mldonkey is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    mldonkey is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with mldonkey; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*/

#include "fst_crypt.h"

#include <sys/types.h>
#include <unistd.h>

#include "caml/mlvalues.h"
#include "caml/fail.h"

#if defined(__MINGW32__)
typedef unsigned int uint;
#include <winsock.h>
#else
#include <netinet/in.h>
#endif
/************************************************************************/


/*                    Functions for MLdonkey                            */


/************************************************************************/

value ml_create_cipher(value unit)
{
  return (value) fst_cipher_create();
}

value ml_apply_cipher(value cipher_v, value s_v, value pos_v, value len_v)
{
  FSTCipher* cipher = (FSTCipher*) cipher_v;
  char *s = Bytes_val(s_v);
  int pos = Int_val(pos_v);
  int len = Int_val(len_v);

/*  printf("Apply cipher %X on %d [from %d]\n", cipher, len, pos); */

  fst_cipher_crypt(cipher, s+pos, len);

  return Val_unit;
}

value ml_init_cipher(value cipher_v)
{
  FSTCipher* cipher = (FSTCipher*) cipher_v;
  fst_cipher_init(cipher, cipher->seed, cipher->enc_type);

  return Val_unit;
}

value ml_set_cipher(value cipher_v, value seed_v, value encode_v)
{
  FSTCipher* cipher = (FSTCipher*) cipher_v;
  unsigned int seed = Int32_val(seed_v);
  unsigned int encode = Int_val(encode_v);

/*  printf("ml_set_cipher %X seed:%X enc_type: %X\n", cipher, seed, encode); */

  cipher->enc_type = encode;
  cipher->seed = seed;

  return Val_unit;
}

value ml_cipher_packet_get(value s_v, value pos_v, value in_cipher_v)
{
  FSTCipher* in_cipher = (FSTCipher*) in_cipher_v;
  const char *s = String_val(s_v);
  int pos = Int_val(pos_v);
  unsigned int seed;
  unsigned int enc_type;

/*
  printf("ml_cipher_packet_get IN:%X OUT:%X pos %d\n", in_cipher, out_cipher, pos);

  printf("out seed:%X enc_type: %X\n", out_cipher->seed, out_cipher->enc_type);
*/

  seed = htonl  (((unsigned int*)(s+pos))[0]);
  enc_type = htonl (((unsigned int*)(s+pos+4))[0]);
  enc_type = fst_cipher_mangle_enc_type (seed, enc_type);

  in_cipher->seed = seed;
  in_cipher->enc_type = enc_type;
/*  printf("in seed:%X enc_type: %X\n", seed, out_cipher->enc_type);  */

/*  if(enc_type > 0x29)    failwith ("ERROR: unsupported encryption"); */

  return Val_unit;
}

value ml_xor_ciphers(value out_cipher_v, value in_cipher_v){
  FSTCipher* in_cipher = (FSTCipher*) in_cipher_v;
  FSTCipher* out_cipher = (FSTCipher*) out_cipher_v;

  out_cipher->seed ^= in_cipher->seed; /* xor send cipher with received seed */

  /* the correct behaviour here is to use the enc_type the supernode sent
   * us for out_cipher too.
   */
  out_cipher->enc_type = in_cipher->enc_type;

  return Val_unit;
}

value ml_xor_ciphers2(value out_cipher_v, value in_cipher_v){
  FSTCipher* in_cipher = (FSTCipher*) in_cipher_v;
  FSTCipher* out_cipher = (FSTCipher*) out_cipher_v;
  unsigned int seed = out_cipher->seed;

  out_cipher->seed ^= in_cipher->seed; /* xor send cipher with received seed */
  in_cipher->seed ^= seed;
  fst_cipher_init(out_cipher, out_cipher->seed, out_cipher->enc_type);

  return Val_unit;
}


value ml_cipher_packet_set(value cipher_v, value s_v, value pos_v)
{
  FSTCipher* cipher = (FSTCipher*) cipher_v;
  char *s = Bytes_val(s_v);
  int pos = Int_val(pos_v);

  ((unsigned int*)(s+pos))[0] = htonl(cipher->seed);
  ((unsigned int*)(s+pos+4))[0] = htonl(
    fst_cipher_mangle_enc_type(cipher->seed, cipher->enc_type));

  return Val_unit;
}

value ml_cipher_enc_type(value cipher_v)
{
  FSTCipher* cipher = (FSTCipher*) cipher_v;

  return Val_int(cipher->enc_type);
}

value ml_cipher_packet_set_xored(value cipher_v, value s_v, value pos_v, value xor_cipher_v)
{
  FSTCipher* cipher = (FSTCipher*) cipher_v;
  FSTCipher* xor_cipher = (FSTCipher*) xor_cipher_v;
  const char *s = String_val(s_v);
  int pos = Int_val(pos_v);
  unsigned int seed = cipher->seed;

  seed ^= xor_cipher->seed;
  ((unsigned int*)(s+pos))[0] = htonl(seed);
  ((unsigned int*)(s+pos+4))[0] = htonl(
    fst_cipher_mangle_enc_type(cipher->seed, cipher->enc_type));

  return Val_unit;
}


value ml_cipher_free(value cipher_v)
{
  FSTCipher* cipher = (FSTCipher*) cipher_v;
  fst_cipher_free(cipher);

  return Val_unit;
}
