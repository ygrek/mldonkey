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

#ifndef _ENDIANESS_H
#define _ENDIANESS_H

typedef long long int64;

int64 buf_to_int(char *buf, int pos, int size);
void int_to_buf(int64 i, char *buf, int pos, int size);

#define BUF8_TO_INT(buf, pos) buf_to_int((buf), (pos), 1)
#define BUF16_TO_INT(buf, pos) buf_to_int((buf), (pos), 2)
#define BUF32_TO_INT(buf, pos) buf_to_int((buf), (pos), 4)
#define BUF64_TO_INT(buf, pos) buf_to_int((buf), (pos), 8)

#define INT_TO_BUF8(i, buf, pos) int_to_buf((i), (buf), (pos), 1)
#define INT_TO_BUF16(i, buf, pos) int_to_buf((i), (buf), (pos), 2)
#define INT_TO_BUF32(i, buf, pos) int_to_buf((i), (buf), (pos), 4)
#define INT_TO_BUF64(i, buf, pos) int_to_buf((i), (buf), (pos), 8)

#endif
