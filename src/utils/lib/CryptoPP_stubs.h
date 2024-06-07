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


#include "../../../config/config.h"

#include <string.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/callback.h>

#if defined (HAVE_STDINT_H)
#include <stdint.h>
#endif
#if defined (HAVE_INTTYPES_H)
#include <inttypes.h>
#endif

typedef unsigned char byte;

void crypto_exit();
void createKey(char buf[]);
unsigned long loadKey(const char privateKeyBase64[], char buf[]);
int createSignature(byte *buf, int maxLen, const byte *key, int keyLen, uint32_t cInt, uint8_t ipType, uint32_t ip);
int verifySignature(byte *key, int keyLen, byte *sig, int sigLen, uint32_t cInt, uint8_t ipType, uint32_t ip);
