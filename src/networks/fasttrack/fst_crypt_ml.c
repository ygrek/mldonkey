#include "fst_crypt.h"

#include <sys/types.h>
#include <unistd.h>
#include <netinet/in.h>
#include "caml/mlvalues.h"
#include "caml/fail.h"


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
  char *s = String_val(s_v);
  int pos = Int_val(pos_v);
  int len = Int_val(len_v);

/*  printf("Apply cipher %X on %d [from %d]\n", cipher, len, pos); */

  fst_cipher_crypt(cipher, s+pos, len);

  return Val_unit;
}

value ml_init_cipher(value cipher_v, value seed_v, value encode_v)
{
  FSTCipher* cipher = (FSTCipher*) cipher_v;
  unsigned int seed = Int32_val(seed_v);
  unsigned int encode = Int_val(encode_v);

  fst_cipher_init(cipher, seed, encode);

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

value ml_cipher_packet_get(value s_v, value pos_v, 
  value in_cipher_v, value out_cipher_v)
{
  FSTCipher* in_cipher = (FSTCipher*) in_cipher_v;
  FSTCipher* out_cipher = (FSTCipher*) out_cipher_v;
  char *s = String_val(s_v);
  int pos = Int_val(pos_v);
  unsigned int seed;
  unsigned int enc_type;

/*
  printf("ml_cipher_packet_get IN:%X OUT:%X pos %d\n", in_cipher, out_cipher, pos);

  printf("out seed:%X enc_type: %X\n", out_cipher->seed, out_cipher->enc_type);
*/

  seed = htonl  (((unsigned int*)(s+pos))[0]);
  enc_type = htonl (((unsigned int*)(s+pos+4))[0]);
  enc_type = fst_cipher_decode_enc_type (seed, enc_type);

/*  printf("in seed:%X enc_type: %X\n", seed, out_cipher->enc_type);  */

  if(enc_type > 0x29)    failwith ("ERROR: unsupported encryption");

  out_cipher->seed ^= seed; /* xor send cipher with received seed */
  fst_cipher_init(out_cipher, out_cipher->seed, out_cipher->enc_type);
  fst_cipher_init(in_cipher, seed, enc_type);

  return Val_unit;
}

value ml_cipher_packet_set(value cipher_v, value s_v, value pos_v)
{
  FSTCipher* cipher = (FSTCipher*) cipher_v;
  char *s = String_val(s_v);
  int pos = Int_val(pos_v);

/*
  printf("ml_cipher_packet_set %X seed:%X enc_type:%X\n", cipher,
    cipher->seed, cipher->enc_type
  );
*/

/* in OCAML: "\250\000\182\043" */
  /* ((unsigned int*)(s+pos))[0] = 0x02BB600FA; random number? */
  s[pos] = 250;
  s[pos+1] = 0;
  s[pos+2] = 182;
  s[pos+3] = 43;
  ((unsigned int*)(s+pos+4))[0] = htonl(cipher->seed);
  ((unsigned int*)(s+pos+8))[0] = htonl(
    fst_cipher_encode_enc_type(cipher->seed, cipher->enc_type));

  return Val_unit;
}

value ml_cipher_free(value cipher_v)
{
  FSTCipher* cipher = (FSTCipher*) cipher_v;
  fst_cipher_free(cipher);

  return Val_unit;
}
