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

#include "../config/config.h"

#include <errno.h>
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
#define FD_TASK_POS 5
#define FD_TASK_READ_ALLOWED 6
#define FD_TASK_WRITE_ALLOWED 7

#if defined(HAVE_POLL) && defined(HAVE_SYS_POLL_H)

#include <sys/poll.h>

static value* pfds = NULL;
static struct pollfd* ufds = NULL;
static int ufds_size = 0;
value ml_getdtablesize(value unit)
{
  ufds_size = getdtablesize();
  return Val_int(ufds_size);
}

value ml_select(value fdlist, value timeout) /* ML */
{
  int tm = (int)(1e3 * (double)Double_val(timeout));
  int nfds = 0;
  int retcode;
  value res;
  int notimeout;
  value l;  
  int must_read;
  int must_write;
  int pos;
  
  if(ufds == NULL){
    ufds_size = getdtablesize();
    ufds = (struct pollfd*) malloc (sizeof(struct pollfd) * ufds_size);
    pfds = (value*) malloc (sizeof(value) * ufds_size);
  }
  
  
  for (l = fdlist; l != Val_int(0); l = Field(l, 1)) {
    value v = Field(l,0);
    if(Field(v, FD_TASK_CLOSED) == Val_false){
      
      must_read = ((Field(v, FD_TASK_RLEN) != Val_int(0)) &&
        (Field(Field(v, FD_TASK_READ_ALLOWED),0) == Val_true));
      must_write = ( (Field(v, FD_TASK_WLEN) != Val_int(0)) &&
        (Field(Field(v, FD_TASK_WRITE_ALLOWED),0) == Val_true));
      if(must_read || must_write){
        int fd = Int_val(Field(v,FD_TASK_FD));
/*        fprintf(stderr, "FD in POLL added %d\n", fd);  */
        ufds[nfds].fd = fd;
        ufds[nfds].events = (must_read? POLLIN : 0) | (must_write ? POLLOUT:0);
        ufds[nfds].revents = 0;
/*        printf("SETTING %d TO %d\n", fd, nfds); */
        pfds[nfds] = v;
        nfds++;
      } else
        ;
    }
  }
/*  printf("POLL: %d/%d\n", nfds, ufds_size); */
  enter_blocking_section();
  retcode = poll(ufds, nfds, tm);
  leave_blocking_section();
  if (retcode < 0) {
    uerror("poll", Nothing);
  }
  if(retcode > 0){
    for(pos=0; pos<nfds; pos++){
      if (ufds[pos].revents){
        value v = pfds[pos];
        int fd = Int_val(Field(v,FD_TASK_FD));
      /*  printf("TESTING %d AT %d\n", fd, pos); */
      /*  fprintf(stderr, "FOR FD in POLL %d[%d]\n", fd, ufds[pos].revents); */
        value flags = Val_int(0);
        if (ufds[pos].revents & POLLIN)  flags |= 2;
        if (ufds[pos].revents & POLLOUT) flags |= 4;
        /*        if (ufds[pos].revents & POLLNVAL) */
        /*        Field(v, FD_TASK_CLOSED) = Val_true; */
        Field(v,FD_TASK_FLAGS) = flags;
      }
    }
  }
  return Val_unit;
}

#else

value ml_select(value fdlist, value timeout) /* ML */
{
  fd_set read, write, except;
  double tm;
  struct timeval tv;
  struct timeval * tvp;
  int retcode;
  value res;
  int notimeout;
  value l;  
  int maxfd = 0 ;

  restart_select:

  FD_ZERO(&read);
  FD_ZERO(&write);
  FD_ZERO(&except);
  for (l = fdlist; l != Val_int(0); l = Field(l, 1)) {
    value v = Field(l,0);
    if(Field(v, FD_TASK_CLOSED) == Val_false){
      int fd = Int_val(Field(v,FD_TASK_FD));
/*      fprintf(stderr, "FD in SELECT %d\n", fd); */
      if( (Field(v, FD_TASK_RLEN) != Val_int(0)) &&
          (Field(Field(v, FD_TASK_READ_ALLOWED),0) == Val_true)
        ) {
        maxfd = maxfd < fd ? fd : maxfd;
        FD_SET(fd, &read);
      }
      if( (Field(v, FD_TASK_WLEN) != Val_int(0)) &&
          (Field(Field(v, FD_TASK_WRITE_ALLOWED),0) == Val_true)
        ) {
        maxfd = maxfd < fd ? fd : maxfd;
        FD_SET(fd, &write);
      }
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
  retcode = select(maxfd+1, &read, &write, &except, tvp);
  leave_blocking_section();

  if (retcode < 0) {
/*    if(errno == EINTR) goto restart_select; */
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

value ml_getdtablesize(value unit)
{
  int dtablesize = getdtablesize();
  int maxselectfds = FD_SETSIZE;

  int maxfd = dtablesize;

  if (maxselectfds < maxfd) {
    printf("Your shell allows %d file descriptors, but select only allows %d file descriptors\n", dtablesize, maxselectfds);
    maxfd = maxselectfds;
    printf("Limit has been set to %d\n", maxfd);
  }

  return Val_int(maxfd);
}

#endif

