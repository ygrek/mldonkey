
#ifndef _SHA1_H
#define _SHA1_H

#include "../../../config/config.h"

#if defined(ENABLE_SHA1NEW)
#include "sha1new_c.h"
#else
#include "os_stubs.h"
#include "sha1old_c.h"
#endif

int sha1_begin(SHA1_CTX*);
int sha1_hash(SHA1_CTX*, const unsigned char [], unsigned long);
int sha1_end(SHA1_CTX*, unsigned char hval[]);

#endif

