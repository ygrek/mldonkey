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
#include "../lib/os_stubs.h"

#define lseek XXXXXXXXX
#define read XXXXXXXXX
#define ftruncate XXXXXXXXX

#define UNIX_BUFFER_SIZE 16384




/*******************************************************************


                         ml_select


*******************************************************************/

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

value try_poll(value fdlist, value timeout) /* ML */
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
    ufds_size = os_getdtablesize();
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
      Field(v,FD_TASK_FLAGS) = Val_int(0);
      if(must_read || must_write){
        int fd = Socket_val(Field(v,FD_TASK_FD));
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
    for(pos=0; pos<nfds && retcode > 0; pos++){
      if (ufds[pos].revents){
        value v = pfds[pos];
        int fd = Socket_val(Field(v,FD_TASK_FD));
      /*  printf("TESTING %d AT %d\n", fd, pos); */
      /*  fprintf(stderr, "FOR FD in POLL %d[%d]\n", fd, ufds[pos].revents); */
        value flags = Val_int(0);
        retcode--;
        if (ufds[pos].revents & (POLLIN|POLLERR|POLLHUP))  flags |= 2;
        if (ufds[pos].revents & POLLOUT) flags |= 4;
        /*        if (ufds[pos].revents & POLLNVAL) */
        /*        Field(v, FD_TASK_CLOSED) = Val_true; */
        Field(v,FD_TASK_FLAGS) = flags;
      }
    }
  }
  return Val_unit;
}

#endif

value try_select(value fdlist, value timeout) /* ML */
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
      int fd = Socket_val(Field(v,FD_TASK_FD));
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
      int fd = Socket_val(Field(v,FD_TASK_FD));
      value flags = Val_int(0);
      if (FD_ISSET(fd, &read)) flags |= 2;
      if (FD_ISSET(fd, &write)) flags |= 4;
      Field(v,FD_TASK_FLAGS) = flags;
    }
  }
  return Val_unit;
}


static int use_poll = 1;
static int must_use_poll = 0;

value ml_use_poll(value use)
{
  use_poll = (use != Val_int(0)) || must_use_poll;
  return Val_unit;
}


value ml_select(value fd_list, value timeout)
{

#if defined(HAVE_POLL) && defined(HAVE_SYS_POLL_H)
    if (use_poll) 
      return try_poll(fd_list, timeout);
    else
      return try_select(fd_list, timeout);
#else
    return try_select(fd_list, timeout);
#endif

}

/*******************************************************************


                         ml_getdtablesize


 *******************************************************************/

value ml_getdtablesize(value unit)
{
  int dtablesize = os_getdtablesize();
  int maxselectfds = FD_SETSIZE;

  int maxfd = dtablesize;

  if (maxselectfds < maxfd) {

#if defined(HAVE_POLL) && defined(HAVE_SYS_POLL_H)

     must_use_poll = 1;
     use_poll = 1;
      
#else

      printf("Your shell allows %d file descriptors, but select only allows %d file descriptors\n", dtablesize, maxselectfds);
      maxfd = maxselectfds;
      printf("Limit has been set to %d\n", maxfd);

#endif
  }

  return Val_int(maxfd);
}

/*******************************************************************


                         ml_get_fd_num


 *******************************************************************/

value ml_get_fd_num(value fd)
{
#if defined(__MINGW32__)
  return Int_val(Socket_val(fd));
#else
  return fd;
#endif
}

/*******************************************************************


                         ml_setsock_iptos_thoughtput


*******************************************************************/

#if defined(HAVE_NETINET_IP_H)

#include <sys/socket.h>
#include <netinet/in_systm.h>
#include <netinet/in.h>
#include <netinet/ip.h>

value ml_setsock_iptos_throughput(value sock_v)
{
  int sock = Socket_val(sock_v);
  int tos = IPTOS_THROUGHPUT /* IPTOS_MINCOST obsoleted by ECN */;
  return Val_int(setsockopt(sock,
      IPPROTO_IP, IP_TOS,
      &tos, sizeof(tos)));
}

#else

value ml_setsock_iptos_throughput(value sock_v)
{
  return Val_unit;
}

#endif


/*******************************************************************


                         ml_lseek64


*******************************************************************/

value ml_lseek64(value fd, value ofs, value cmd)
{
  long ret;
  long pos = Int64_val(ofs);
  ret = os_lseek(Fd_val(fd), pos,
                       seek_command_table[Int_val(cmd)]);
  if (ret == -1) uerror("lseek", Nothing);
  return copy_int64(ret);
}

/*******************************************************************


                         ml_getsize64


*******************************************************************/

value ml_getsize64(value path)
{
  int ret;

  return copy_int64(os_getfilesize(String_val(path)));
}


/*******************************************************************


                         ml_truncate64


*******************************************************************/

#define ZEROS_LEN 1024
value ml_truncate64(value fd_v, value len_v)
{
  unsigned long len = Int64_val(len_v);
  OS_FD fd = Fd_val(fd_v);  

  os_ftruncate(fd, len);
    
  return Val_unit;
}

/*******************************************************************


                         ml_strstr


*******************************************************************/

value ml_strstr(value s_v, value sub_v)
{
  char *s = String_val(s_v);
  char *sub = String_val(sub_v);

  if(strstr(s, sub) == NULL) {
    return Val_false;
  }
  return Val_true;
}

/*******************************************************************


                     ml_ints_of_string


*******************************************************************/


value ml_ints_of_string(value s_v)
{
  char *s = String_val(s_v);
  uint a1,a2,a3,a4;
  value res;
  char *curs = s;
  char *first;
  char c;

  first = curs;
  while(isdigit(*curs)) curs++;
  if(*curs != '.' || curs == first || curs - first > 3) goto error;
  *curs = 0;
  a1 = atoi(first);
  *curs++ = '.';

  first = curs;
  while(isdigit(*curs)) curs++;
  if(*curs != '.' || curs == first || curs - first > 3) goto error;
  *curs = 0;
  a2 = atoi(first);
  *curs++ = '.';

  first = curs;
  while(isdigit(*curs)) curs++;
  if(*curs != '.' || curs == first || curs - first > 3) goto error;
  *curs = 0;
  a3 = atoi(first);
  *curs++ = '.';

  first = curs;
  while(isdigit(*curs)) curs++;
  if(curs == first || curs - first > 3) goto error;
  c = *curs;  *curs = 0;
  a4 = atoi(first);
  *curs++ = c;

/*   sscanf(s, "%d.%d.%d.%d", &a1, &a2, &a3, &a4); */
  goto ok;

  error:
  a1 = a2 = a3 = a4 = 0;

  ok:
  res = alloc(4,0);
  Field(res, 0) = Val_int(a1);
  Field(res, 1) = Val_int(a2);
  Field(res, 2) = Val_int(a3);
  Field(res, 3) = Val_int(a4);
  return res;
}


#include "md4.h"

unsigned char hash_buffer[HASH_BUFFER_LEN];

value md4_unsafe64_fd (value digest_v, value fd_v, value pos_v, value len_v)
{
  OS_FD fd = Fd_val(fd_v);
  long pos = Int64_val(pos_v);
  long len = Int64_val(len_v);
  unsigned char *digest = String_val(digest_v);
  MD4_CTX context;
  int nread;

  MD4Init (&context);
  os_lseek(fd, pos, SEEK_SET);

  while (len!=0){
    int max_nread = HASH_BUFFER_LEN > len ? len : HASH_BUFFER_LEN;

    nread = os_read (fd, hash_buffer, max_nread);

    if(nread < 0) {
      unix_error(errno, "md4_safe_fd: Read", Nothing);
    }

    if(nread == 0){
      MD4Final (digest, &context);

      return Val_unit;
    }

    MD4Update (&context, hash_buffer, nread);
    len -= nread;
  }
  MD4Final (digest, &context);

  return Val_unit;
}

#include "md5.h"

value md5_unsafe64_fd (value digest_v, value fd_v, value pos_v, value len_v)
{
  OS_FD fd = Fd_val(fd_v);
  long pos = Int64_val(pos_v);
  long len = Int64_val(len_v);
  unsigned char *digest = String_val(digest_v);
  md5_state_t context;
  int nread;

  md5_init (&context);
  os_lseek(fd, pos, SEEK_SET);

  while (len!=0){
    int max_nread = HASH_BUFFER_LEN > len ? len : HASH_BUFFER_LEN;

    nread = os_read (fd, hash_buffer, max_nread);

    if(nread < 0) {
      unix_error(errno, "md4_safe_fd: Read", Nothing);
    }

    if(nread == 0){
      md5_finish (&context, digest);

      return Val_unit;
    }

    md5_append (&context, hash_buffer, nread);
    len -= nread;
  }
  md5_finish (&context, digest);

  return Val_unit;
}

#include <locale.h>

value ml_setlcnumeric(value no)
{
   setlocale(LC_NUMERIC, "C");
  return Val_unit;
}