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

#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <signal.h>
#ifdef HAS_SIGNALS_H
#include <signals.h>
#endif

/* #include "../otherlibs/unix/unixsupport.h" */
#ifdef HAS_UNISTD
#include <unistd.h>
#endif

#define Nothing ((value) 0)

extern void unix_error (int errcode, char * cmdname, value arg) Noreturn;
extern void uerror (char * cmdname, value arg) Noreturn;

#define UNIX_BUFFER_SIZE 16384

/* END unixsupport.h */


#ifdef HAS_SELECT

#include <sys/types.h>
#include <sys/time.h>
#ifdef HAS_SYS_SELECT_H
#include <sys/select.h>
#endif

#ifdef __OpenBSD__
#include <string.h>
#endif

#define FD_TASK_FD 0
#define FD_TASK_FLAGS 1
#define FD_TASK_WLEN 2
#define FD_TASK_RLEN 3
#define FD_TASK_CLOSED 4

typedef fd_set file_descr_set;

value ml_select(value fdlist, value timeout) /* ML */
{
  file_descr_set read, write, except;
  double tm;
  struct timeval tv;
  struct timeval * tvp;
  int retcode;
  value res;
  value read_list = Val_unit, write_list = Val_unit, except_list = Val_unit;
  int notimeout;
  value l;  

  FD_ZERO(&read);
  FD_ZERO(&write);
  FD_ZERO(&except);
  for (l = fdlist; l != Val_int(0); l = Field(l, 1)) {
    value v = Field(l,0);
    if(Field(v, FD_TASK_CLOSED) == Val_false){
      int fd = Int_val(Field(v,FD_TASK_FD));
/*      fprintf(stderr, "FD in SELECT %d\n", fd); */
      if(Field(v, FD_TASK_RLEN) != Val_int(0)) FD_SET(fd, &read);
      if(Field(v, FD_TASK_WLEN) != Val_int(0)) FD_SET(fd, &write);
    }
  }
  tm = Double_val(timeout);
  if (tm < 0.0)
    tvp = (struct timeval *) NULL;
  else {
    tv.tv_sec = (int) tm;
    tv.tv_usec = (int) (1e6 * (tm - (int) tm));
    tvp = &tv;
  }
  enter_blocking_section();
  retcode = select(FD_SETSIZE, &read, &write, &except, tvp);
  leave_blocking_section();
  if (retcode < 0) {
    uerror("select", Nothing);
  }
  for (l = fdlist; l != Val_int(0); l = Field(l, 1)) {
    value v = Field(l,0);
    if(Field(v, FD_TASK_CLOSED) == Val_false){
      int fd = Int_val(Field(v,FD_TASK_FD));
      value flags = Val_int(0);
      if (FD_ISSET(fd, &read)) flags |= 2;
      if (FD_ISSET(fd, &write)) flags |= 4;
      Field(v,FD_TASK_FLAGS) = flags;
    }
  }
  return Val_unit;
}

#else

value unix_select(value readfds, value writefds, value exceptfds, value timeout)
{ invalid_argument("select not implemented"); }

#endif
