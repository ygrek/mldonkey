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

#include "../../../config/config.h"
#include "../../utils/lib/os_stubs.h"

#include <string.h> 
#include <ctype.h>

#define lseek XXXXXXXXX
#define read XXXXXXXXX
#define ftruncate XXXXXXXXX

#define UNIX_BUFFER_SIZE 16384

extern void enter_blocking_section(); 
extern void leave_blocking_section();


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

value ml_set_nonblock(value fd_v)
{
  OS_SOCKET fd = Socket_val(fd_v);
  os_set_nonblock(fd);
  return Val_unit;
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

/*******************************************************************


                     md4


*******************************************************************/

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

/*******************************************************************


                     md5


*******************************************************************/
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
      unix_error(errno, "md5_safe_fd: Read", Nothing);
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



/*******************************************************************


                     sha1


*******************************************************************/
#include "sha1_c.h"

value sha1_unsafe64_fd (value digest_v, value fd_v, value pos_v, value len_v)
{
  OS_FD fd = Fd_val(fd_v);
  long pos = Int64_val(pos_v);
  long len = Int64_val(len_v);
  unsigned char *digest = String_val(digest_v);
  SHA1_CTX context;
  int nread;

  sha1_init (&context);
  os_lseek(fd, pos, SEEK_SET);

  while (len!=0){
    int max_nread = HASH_BUFFER_LEN > len ? len : HASH_BUFFER_LEN;

    nread = os_read (fd, hash_buffer, max_nread);

    if(nread < 0) {
      unix_error(errno, "sha1_safe_fd: Read", Nothing);
    }

    if(nread == 0){
      sha1_finish (&context, digest);

      return Val_unit;
    }

    sha1_append (&context, hash_buffer, nread);
    len -= nread;
  }
  sha1_finish (&context, digest);

  return Val_unit;
}


/*******************************************************************


                     tiger


*******************************************************************/
#include "tiger.h"


static char tiger_buffer[BLOCK_SIZE+1];

void tiger_tree_fd(OS_FD fd, int len, int pos, int block_size, char *digest)
{
  if(block_size == BLOCK_SIZE){
    int length = (len - pos > BLOCK_SIZE) ? BLOCK_SIZE : len - pos;
    char *s = tiger_buffer+1;
    int toread = length;
    char *curs = s;
      while (toread!=0){
      int max_nread = toread;
/* HASH_BUFFER_LEN > toread ? toread : HASH_BUFFER_LEN; */

      int nread = os_read (fd, curs, max_nread);

        if(nread <= 0) {
        unix_error(errno, "tiger_safe_fd: Read", Nothing);
      }
      curs += nread;
      toread -= nread;
    }

    tiger_hash(0, s, length, digest);
  } else {    
    if(pos+block_size/2 >=len){
      tiger_tree_fd(fd, len, pos, block_size/2, digest);
    } else {
      char digests_prefixed[1+DIGEST_LEN * 2];
      char *digests = digests_prefixed+1;
      tiger_tree_fd(fd, len, pos, block_size/2, digests);
      tiger_tree_fd(fd, len, pos+block_size/2, block_size/2, digests+DIGEST_LEN);
      tiger_hash(1,digests, 2*DIGEST_LEN, digest);
    }
  }
}

value tiger_unsafe64_fd (value digest_v, value fd_v, value pos_v, value len_v)
{
  OS_FD fd = Fd_val(fd_v);
  long pos = Int64_val(pos_v);
  long len = Int64_val(len_v);
  unsigned char *digest = String_val(digest_v);
  int nread;

  os_lseek(fd, pos, SEEK_SET);

  tiger_tree_fd(fd, len, 0, tiger_block_size(len), digest);

  return Val_unit;
}

/*******************************************************************


                     setlcnumeric


*******************************************************************/

#include <locale.h>

value ml_setlcnumeric(value no)
{
   setlocale(LC_NUMERIC, "C");
  return Val_unit;
}

/******************************************************************/


/*        Asynchronous resolution of DNS names in threads         */


/******************************************************************/








#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
///#include <signals.h>
//#include "unixsupport.h"

// #include "socketaddr.h"
#ifndef _WIN32
#include <sys/types.h>
#include <netdb.h>
#endif

#define NETDB_BUFFER_SIZE 10000

#ifdef _WIN32
#define GETHOSTBYADDR_IS_REENTRANT 1
#define GETHOSTBYNAME_IS_REENTRANT 1
#endif

static char volatile ip_job_result[256];
static int volatile job_naddresses = 0;
static int entry_h_length;


static void save_one_addr(char volatile *dest, char const *a)
{
  memmove ((char*)dest, (char*) a, entry_h_length);
}

static void save_host_entry(struct hostent *entry)
{
  char **ptrs = (char **)entry->h_addr_list;

  entry_h_length = entry->h_length;
#ifdef h_addr
  job_naddresses = 0;
  while(*ptrs != NULL){
    save_one_addr(ip_job_result + entry_h_length * job_naddresses++, *ptrs++);
  }
#else
  job_naddresses = 1;
  save_one_addr(ip_job_result, entry->h_addr);
#endif
}

static int ml_gethostbyname(char *hostname)
{
  struct hostent * hp;

#if HAS_GETHOSTBYNAME_R == 5
  {
    struct hostent h;
    char buffer[NETDB_BUFFER_SIZE];
    int h_errno;
    enter_blocking_section();
    hp = gethostbyname_r(hostname, &h, buffer, sizeof(buffer), &h_errno);
    leave_blocking_section();
  }
#elif HAS_GETHOSTBYNAME_R == 6
  {
    struct hostent h;
    char buffer[NETDB_BUFFER_SIZE];
    int h_errno, rc;
    enter_blocking_section();
    rc = gethostbyname_r(hostname, &h, buffer, sizeof(buffer), &hp, &h_errno);
    leave_blocking_section();
    if (rc != 0) hp = NULL;
  }
#else
#ifdef GETHOSTBYNAME_IS_REENTRANT
  enter_blocking_section();
#endif
  hp = gethostbyname(hostname);
#ifdef GETHOSTBYNAME_IS_REENTRANT
  leave_blocking_section();
#endif
#endif

  if (hp == (struct hostent *) NULL) return 0;

/*  printf("No error\n"); */
  save_host_entry(hp);
  return 1;
}

extern value alloc_inet_addr(uint32 a);

static value alloc_one_addr(char volatile *a)
{
  struct in_addr addr;
  memmove (&addr, (char*)a, entry_h_length);
  return alloc_inet_addr(addr.s_addr);
}

static void store_in_job(value job_v)
{
  value adr = Val_unit;
  value addr_list = Val_unit;
  int i;

/*  printf("store_in_job %d\n", job_naddresses); */
  Begin_roots3 (job_v, addr_list, adr);
#ifdef h_addr
  addr_list = alloc_small(job_naddresses, 0);
  for(i=0; i<job_naddresses; i++){
    adr = alloc_one_addr(ip_job_result + i * entry_h_length);
    modify(&Field(addr_list,i), adr);
  }
#else
  adr = alloc_one_addr(ip_job_result);
  addr_list = alloc_small(1, 0);
  Field(addr_list, 0) = adr;
#endif
  modify(&Field(job_v,1), addr_list);
  End_roots();
}

#if !defined(HAVE_LIBPTHREAD) || !(HAS_GETHOSTBYNAME_R || GETHOSTBYNAME_IS_REENTRANT)

value ml_ip_job_start(value job_v)
{
  char *hostname = String_val(Field(job_v,0));
  if(ml_gethostbyname(hostname)){
    Field(job_v, 2) = Val_false;
    store_in_job(job_v);
  } else {
    Field(job_v, 2) = Val_true;
  }
  return Val_unit;
}

value ml_ip_job_done(value job_v)
{
  return Val_true;
}


value ml_has_pthread(value unit)
{ return Val_false; }

#else

#include <string.h>
#include <pthread.h>
#include <signal.h>
#include <sys/time.h>

static int thread_started = 0;
static int volatile ip_job_done = 1;
static char volatile job_hostname[256];
static int volatile job_error = 0;

static pthread_t pthread;
static pthread_cond_t cond;
static pthread_mutex_t mutex;

value ml_ip_job_done(value job_v)
{
  if(ip_job_done){
    if(job_error){ 
      Field(job_v, 2) = Val_false;
      store_in_job(job_v);
    } else {
/*      printf("found error\n"); */
      Field(job_v, 2) = Val_true;
    }
    return Val_true;
  }

  return Val_false;
}

static void * hasher_thread(void * arg)
{
  struct timeval now;
  struct timespec timeout;
  sigset_t mask;

  /* Block all signals so that we don't try to execute a Caml signal handler */
  sigfillset(&mask);
  pthread_sigmask(SIG_BLOCK, &mask, NULL);
  
  nice(19);
  
  pthread_mutex_lock(&mutex);

  while(1){
    gettimeofday(&now, NULL);
    timeout.tv_sec = now.tv_sec + 10;
    timeout.tv_nsec = now.tv_usec * 1000;

/*    printf("waiting for next job\n");  */
    pthread_cond_timedwait(&cond, &mutex, &timeout);
    
    if(!ip_job_done){
      job_error = ml_gethostbyname((char*) job_hostname);

      ip_job_done = 1;
/*      printf("job finished %d\n", job_error); */
    }
  }
    
  return NULL;
}

value ml_ip_job_start(value job_v)
{
  strcpy( (char*) job_hostname, String_val(Field(job_v,0)));

  if(!thread_started){
    int retcode;

    pthread_attr_t attr;
    pthread_attr_init(&attr);
    pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);

    pthread_cond_init(&cond, NULL);
    pthread_mutex_init(&mutex, NULL);

    thread_started = 1;
    retcode = pthread_create(&pthread, &attr, hasher_thread, NULL);

    if(retcode){
      perror("Error while starting Hashing thread");
      exit(2);
    }
  }

  enter_blocking_section();
  pthread_mutex_lock(&mutex);
/*  printf("Starting job\n");  */
  ip_job_done = 0; /* Thread can run ... */
  pthread_cond_signal(&cond);  
  pthread_mutex_unlock(&mutex);
  leave_blocking_section ();

  return Val_unit;
}

value ml_has_pthread(value unit)
{ return Val_true; }

#endif
