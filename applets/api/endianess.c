/* Copyright 2002 b8_fange */
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


#include <stdio.h>
#include <stdlib.h>

#include "endianess.h"

int64 
buf_to_int(char *buf,
               int pos,
               int size)
{
    int i;
    int64 res = 0;
  
    for(i = 0; i < size; i++){
        res += (buf[pos + i] & 0xFF) << (8 * i);
    }
    return res;
}

void
int_to_buf(int64 i,
           char *buf,
           int pos,
           int size)
{
    int j;
    
    for(j = 0; j < size; j++){
        buf[pos + j] = (i & (-1)) >> (8 * j);
    }
}
