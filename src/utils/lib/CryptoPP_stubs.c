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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#include <string.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>

#include "CryptoPP_stubs.h"



// return private key Base64Encoded
value 
ml_createKey() {
	char buf[4096];
	createKey(buf);
	return caml_copy_string(buf);
}

// return public key
value 
ml_loadKey(value privatekey) {
  char *s = String_val(privatekey);
	char buf[4096];
	unsigned long len =	loadKey(s, buf);

  value res;
  res = caml_alloc_string(len);
  memmove(String_val(res), buf, len);

	return res;
}

value
ml_createSignature(value m_key, value m_keyLen, value m_cInt, value m_ipType, value m_ip) {

	byte *key = (byte*) String_val(m_key);
	int keyLen = Int_val(m_keyLen);
	uint32_t cInt = Int64_val(m_cInt);
	int ipType = Int_val(m_ipType);
	uint32_t ip = Int64_val(m_ip);

	byte buf[4096];

	int len = createSignature(buf, 200, key, keyLen, cInt, ipType, ip);

  value res;
  res = caml_alloc_string(len);
  memmove(String_val(res), buf, len);

	return res;
}

value 
ml_verifySignature(value m_key, value m_keyLen, value m_sig, value m_sigLen, value m_cInt, value m_ipType, value m_ip) {

	byte* key = (byte*) String_val(m_key);
	int keyLen = Int_val(m_keyLen);
	byte* sig = (byte*) String_val(m_sig);
	int sigLen = Int_val(m_sigLen);
	uint32_t cInt = Int64_val(m_cInt);
	int ipType = Int_val(m_ipType);
	uint32_t ip = Int64_val(m_ip);
	
	return Val_bool( 
			verifySignature(key, keyLen, sig, sigLen, cInt, ipType, ip)
	);
}

value
ml_verifySignature_bytecode(value *argv, int argn) {
	return ml_verifySignature(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6]);
}
