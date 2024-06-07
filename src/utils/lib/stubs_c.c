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

#include "../../utils/lib/os_stubs.h"

#include <string.h> 
#include <ctype.h>


#ifdef __MORPHOS__
#include <inttypes.h>
#endif  /* __MORPHOS__ */

#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif /* HAVE_SYS_RESOURCE_H */

/* For proper FreeBSD version identification */
#if defined(HAVE_SYS_PARAM_H)
#include <sys/param.h>
#endif

#define lseek XXXXXXXXX
#define read XXXXXXXXX
#define ftruncate XXXXXXXXX



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

/* Stubs that could be used by epoll */

value ml_change_fd_event_setting(value task_v)
{
  int fd = Socket_val(Field(task_v,FD_TASK_FD));
  int must_read = ((Field(task_v, FD_TASK_RLEN) != Val_int(0)) &&
    (Field(Field(task_v, FD_TASK_READ_ALLOWED),0) == Val_true));
  int must_write = ( (Field(task_v, FD_TASK_WLEN) != Val_int(0)) &&
    (Field(Field(task_v, FD_TASK_WRITE_ALLOWED),0) == Val_true));
  
  return Val_unit;
}

value ml_add_fd_to_event_set(value task_v)
{
  int fd = Socket_val(Field(task_v,FD_TASK_FD));
  int must_read = ((Field(task_v, FD_TASK_RLEN) != Val_int(0)) &&
    (Field(Field(task_v, FD_TASK_READ_ALLOWED),0) == Val_true));
  int must_write = ( (Field(task_v, FD_TASK_WLEN) != Val_int(0)) &&
    (Field(Field(task_v, FD_TASK_WRITE_ALLOWED),0) == Val_true));

  return Val_unit;
}

value ml_remove_fd_from_event_set(value task_v)
{
  int fd = Socket_val(Field(task_v,FD_TASK_FD));

  return Val_unit;
}


#if defined(HAVE_POLL) && defined(HAVE_SYS_POLL_H) && !defined(__MINGW32__)

#include <sys/poll.h>

static value* pfds = NULL;
static struct pollfd* ufds = NULL;
static int ufds_size = 0;

value try_poll(value fdlist, value timeout) /* ML */
{
  int tm = (int)(1e3 * (double)Double_val(timeout));
  int nfds = 0;
  int retcode;
/*  value res; */
/*  int notimeout; */
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
      } // else ;
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
/*        int fd = Socket_val(Field(v,FD_TASK_FD)); */
/*        printf("TESTING %d AT %d\n", fd, pos); */
/*        fprintf(stderr, "FOR FD in POLL %d[%d]\n", fd, ufds[pos].revents); */
        value flags = Val_int(0);
        retcode--;
        if (ufds[pos].revents & (POLLIN|POLLERR|POLLHUP))  flags |= 2;
        if (ufds[pos].revents & POLLOUT) flags |= 4;
/*                if (ufds[pos].revents & POLLNVAL) */
/*                Field(v, FD_TASK_CLOSED) = Val_true; */
        Field(v,FD_TASK_FLAGS) = flags;
      }
    }
  }
  return Val_unit;
}

#endif /* defined(HAVE_POLL) && defined(HAVE_SYS_POLL_H) && !defined(__MINGW32__) */


value try_select(value fdlist, value timeout) /* ML */
{
  fd_set read, write, except;
  double tm;
  struct timeval tv;
  struct timeval * tvp;
  int retcode;
/*  value res; */
/*  int notimeout; */
  value l;  
  int maxfd = 0 ;

/*  restart_select: */

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
#if defined(HAVE_POLL) && defined(HAVE_SYS_POLL_H) && !defined(__MINGW32__)
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


********************************************************************/

value ml_getdtablesize(value unit)
{
  int dtablesize = os_getdtablesize();
  int maxselectfds = FD_SETSIZE;
  int maxfd = dtablesize;

  if (maxselectfds < maxfd) {

#if defined(HAVE_POLL) && defined(HAVE_SYS_POLL_H) && !defined(__MINGW32__)

     must_use_poll = 1;
     use_poll = 1;
      
#else

      maxfd = maxselectfds;
/*		   printf("[Info] File descriptor limit: MIN(process: %d, select(): %d) = %d\n", dtablesize, maxselectfds, maxfd); */

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

#if defined(HAVE_NETINET_IP_H) && !defined(__MINGW32__)

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

#endif /* defined(HAVE_NETINET_IP_H) && !defined(__MINGW32__) */


/*******************************************************************


                         ml_sizeofoff_t


*******************************************************************/

value ml_sizeofoff_t(value unit)
{
  return Val_int(sizeof(OFF_T));
}

/*******************************************************************


                         ml_getsize64


*******************************************************************/

value ml_getsize64(value path)
{
/*  int ret; */

  return copy_int64(os_getfilesize(String_val(path)));
}

/*******************************************************************


                         ml_getfdsize64


*******************************************************************/

value ml_getfdsize64(value fd_v)
{
/*  int ret; */
  OS_FD fd = Fd_val(fd_v);
  return copy_int64(os_getfdsize(fd));
}

/*******************************************************************


                         mld_ftruncate_64


*******************************************************************/

#define ZEROS_LEN 1024
value mld_ftruncate_64(value fd_v, value len_v, value sparse)
{
  OFF_T len = Int64_val(len_v);
  OS_FD fd = Fd_val(fd_v);  
	 int use_sparse = Bool_val(sparse);

  os_ftruncate(fd, len, use_sparse);
    
  return Val_unit;
}

/*******************************************************************


                         ml_strstr


*******************************************************************/

value ml_strstr(value s_v, value sub_v)
{
  const char *s = String_val(s_v);
  const char *sub = String_val(sub_v);

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
  char *s = Bytes_val(s_v);
  uint a1,a2,a3,a4;
  value res;
  char *curs = s;
  char *first;
  char c;

  while(*curs == ' ') curs++;
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
/*   printf("Error while parsing[%s]\n",s); */
  raise_not_found();
/*  a1 = a2 = a3 = a4 = 0; */

  ok:
  res = alloc(4,0);
  Field(res, 0) = Val_int(a1);
  Field(res, 1) = Val_int(a2);
  Field(res, 2) = Val_int(a3);
  Field(res, 3) = Val_int(a4);
  return res;
}

/*******************************************************************


                     hashes


*******************************************************************/

unsigned char hash_buffer[HASH_BUFFER_LEN];

#define ML_HASH(HASH_NAME,HASH_CONTEXT,HASH_INIT,HASH_APPEND,HASH_FINISH) \
value HASH_NAME##_unsafe64_fd (value digest_v, value fd_v, value pos_v, value len_v) \
{ \
  OS_FD fd = Fd_val(fd_v); \
  OFF_T pos = Int64_val(pos_v); \
  OFF_T len = Int64_val(len_v); \
  unsigned char *digest = Bytes_val(digest_v); \
  HASH_CONTEXT context; \
  ssize_t nread; \
 \
  HASH_INIT (&context); \
  os_lseek(fd, pos, SEEK_SET); \
 \
  while (len!=0){ \
    size_t max_nread = HASH_BUFFER_LEN > len ? len : HASH_BUFFER_LEN; \
 \
    nread = os_read (fd, hash_buffer, max_nread); \
 \
    if(nread < 0) { \
      unix_error(errno, "md4_safe_fd: Read", Nothing); \
    } \
 \
    if(nread == 0){ \
      HASH_FINISH (&context, digest); \
 \
      return Val_unit; \
    } \
 \
    HASH_APPEND (&context, hash_buffer, nread); \
    len -= nread; \
  } \
  HASH_FINISH (&context, digest); \
 \
  return Val_unit; \
} \
\
value HASH_NAME##_unsafe_string(value digest_v, value string_v, value len_v) \
{ \
  unsigned char *digest = Bytes_val(digest_v); \
  const unsigned char *string = String_val(string_v); \
  long len = Long_val(len_v); \
  HASH_CONTEXT context; \
 \
  HASH_INIT (&context); \
  HASH_APPEND (&context, string, len); \
  HASH_FINISH (&context, digest); \
  \
  return Val_unit; \
} \
 \
value HASH_NAME##_unsafe_file (value digest_v, value filename_v, value file_size) \
{ \
  const char *filename  = String_val(filename_v); \
  unsigned char *digest = Bytes_val(digest_v); \
  FILE *file; \
  HASH_CONTEXT context; \
  size_t len; \
 \
  if ((file = fopen (filename, "rb")) == NULL) \
    raise_not_found(); \
 \
  else { \
    HASH_INIT (&context); \
    while ((len = fread (hash_buffer, 1, HASH_BUFFER_LEN, file)) >0) \
      HASH_APPEND (&context, hash_buffer, len); \
    HASH_FINISH (&context, digest); \
 \
    fclose (file); \
  } \
  return Val_unit; \
} \


#include "md4.h"
#include "md5.h"
#include "sha1_c.h"

ML_HASH(sha1,SHA1_CTX,sha1_begin,sha1_hash, sha1_end)
ML_HASH(md5,md5_state_t,md5_init,md5_append,md5_finish)
ML_HASH(md4,MD4_CTX,MD4Init,MD4Update,md4_finish)

/*******************************************************************


                     tiger


*******************************************************************/
#include "tiger.h"

static void tiger_tree_fd(OS_FD fd, OFF_T len, OFF_T pos, OFF_T block_size, char *digest)
{
  static char tiger_buffer[BLOCK_SIZE+1];
  if(block_size == BLOCK_SIZE){
    OFF_T length = (len - pos > BLOCK_SIZE) ? BLOCK_SIZE : len - pos;
    char *s = tiger_buffer+1;
    size_t toread = length;
    char *curs = s;
      while (toread!=0){
      int max_nread = toread;
/* HASH_BUFFER_LEN > toread ? toread : HASH_BUFFER_LEN; */

      ssize_t nread = os_read (fd, curs, max_nread);

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

value tigertree_unsafe64_fd (value digest_v, value fd_v, value pos_v, value len_v)
{
  OS_FD fd = Fd_val(fd_v);
  OFF_T pos = Int64_val(pos_v);
  OFF_T len = Int64_val(len_v);
  unsigned char *digest = Bytes_val(digest_v);
/*  int nread; */

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

/*******************************************************************


          Asynchronous resolution of DNS names in threads         


*******************************************************************/

/*
#include <signals.h>
#include "unixsupport.h"

#include "socketaddr.h"
*/
#ifndef _WIN32
/* #include <sys/types.h> */
#include <netdb.h>
#endif /* ndef _WIN32 */

#define NETDB_BUFFER_SIZE 10000

#if defined(_WIN32) || ( defined(__FreeBSD_version) && ( ((__FreeBSD_version >= 504102) && (__FreeBSD_version < 600000)) || (__FreeBSD_version >= 600029) ) )
#define GETHOSTBYADDR_IS_REENTRANT 1
#define GETHOSTBYNAME_IS_REENTRANT 1
#endif /* _WIN32 */

#define IP_JOB_RESULT_SIZE 256
static char volatile ip_job_result[IP_JOB_RESULT_SIZE];
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
  while(*ptrs != NULL && (entry_h_length * (job_naddresses + 1) < IP_JOB_RESULT_SIZE)) {
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
#endif  /* GETHOSTBYNAME_IS_REENTRANT */
  hp = gethostbyname(hostname);
#ifdef GETHOSTBYNAME_IS_REENTRANT
  leave_blocking_section();
#endif /* GETHOSTBYNAME_IS_REENTRANT */
#endif /* HAS_GETHOSTBYNAME_R == 5 */

  if (hp == (struct hostent *) NULL) return 0;

/*  printf("No error\n"); */
  save_host_entry(hp);
  return 1;
}

// unix/socketaddr.c
extern value alloc_inet_addr(struct in_addr * a);

static value alloc_one_addr(char volatile *a)
{
  struct in_addr addr;
  memmove (&addr, (char*)a, entry_h_length);
  return alloc_inet_addr(&addr);
}

static value addr_list_of_job(void)
{
  CAMLparam0();
  CAMLlocal2(v_addr, v_addr_list);
  int i;

/*  printf("store_in_job %d\n", job_naddresses); */
  v_addr_list = caml_alloc_tuple(job_naddresses);
  for(i=0; i<job_naddresses; i++){
    v_addr = alloc_one_addr(ip_job_result + i * entry_h_length);
    Store_field(v_addr_list, i, v_addr);
  }
  CAMLreturn(v_addr_list);
}

#if !defined(HAVE_PTHREAD) || (!(HAS_GETHOSTBYNAME_R || GETHOSTBYNAME_IS_REENTRANT) && !defined(HAS_SIGWAIT))

value ml_ip_job_start(value job_v)
{
  CAMLparam1(job_v);

  char *hostname = String_val(Field(job_v,0));
  if(ml_gethostbyname(hostname)){
    Store_field(job_v, 1, addr_list_of_job())
    Store_field(job_v, 2, Val_false);
  } else {
    Store_field(job_v, 2, Val_true);
  }
  CAMLreturn(Val_unit);
}

value ml_ip_job_done(value job_v)
{
  return Val_true;
}


value ml_has_pthread(value unit)
{ 
  return Val_false; 
}

#else  /* !defined(HAVE_PTHREAD) || (!(HAS_GETHOSTBYNAME_R || GETHOSTBYNAME_IS_REENTRANT) && !defined(HAS_SIGWAIT)) */

#include <string.h>
#include <pthread.h>
#include <signal.h>
#include <sys/time.h>

static int thread_started = 0;
static int volatile ip_job_done = 1;
#define JOB_HOSTNAME_SIZE 256
static char volatile job_hostname[JOB_HOSTNAME_SIZE];
static int volatile job_result = 0;

static pthread_t pthread;
static pthread_cond_t cond;
static pthread_mutex_t mutex;

value ml_ip_job_done(value job_v)
{
  CAMLparam1(job_v);

  if(ip_job_done){
    if(job_result){ 
      Store_field(job_v, 1, addr_list_of_job());
      Store_field(job_v, 2, Val_false);
    } else {
/*      printf("found error\n"); */
      Store_field(job_v, 2, Val_true);
    }
    CAMLreturn(Val_true);
  }

  CAMLreturn(Val_false);
}

static void * dns_thread(void * arg)
{
  struct timeval now;
  struct timespec timeout;

#if !defined(PTW32_STATIC_LIB)	
  sigset_t mask;

/*   Block all signals so that we don't try to execute a Caml signal handler */
  sigfillset(&mask);
  pthread_sigmask(SIG_BLOCK, &mask, NULL);
  
  nice(19);
#endif  /* !defined(PTW32_STATIC_LIB) */
  
  pthread_mutex_lock(&mutex);

  while(1){
    gettimeofday(&now, NULL);
    timeout.tv_sec = now.tv_sec + 10;
    timeout.tv_nsec = now.tv_usec * 1000;

/*    printf("waiting for next job\n");  */
    pthread_cond_timedwait(&cond, &mutex, &timeout);
    
    if(!ip_job_done){
      job_result = ml_gethostbyname((char*) job_hostname);

      ip_job_done = 1;
/*      printf("job finished %d\n", job_result); */
    }
  }
    
  return NULL;
}

value ml_ip_job_start(value job_v)
{
  if (caml_string_length(Field(job_v, 0)) >= JOB_HOSTNAME_SIZE)
  {
    // hostname too long - fail immediately
    job_result = 1;
    ip_job_done = 1;
    return Val_unit;
  }

  strcpy( (char*) job_hostname, String_val(Field(job_v,0)));

  if(!thread_started){
    int retcode;

    pthread_attr_t attr;
    pthread_attr_init(&attr);
    pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);

    pthread_cond_init(&cond, NULL);
    pthread_mutex_init(&mutex, NULL);

    thread_started = 1;
    retcode = pthread_create(&pthread, &attr, dns_thread, NULL);

    if(retcode){
      perror("Error while starting DNS thread");
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
{
  return Val_true; 
}

#endif  /* !defined(HAVE_PTHREAD) || (!(HAS_GETHOSTBYNAME_R || GETHOSTBYNAME_IS_REENTRANT) && !defined(HAS_SIGWAIT)) */

/*******************************************************************


                         statfs


*******************************************************************/

#if defined(__MINGW32__)

/* taken from www.greatchief.plus.com/gpc/os-hacks.h */
#define _fullpath(res,path,size) \
  (GetFullPathNameW ((path), (size), (res), NULL) ? (res) : NULL)

#define realpath(path,resolved_path) _fullpath(resolved_path, path, MAX_PATH)

#define FAKED_BLOCK_SIZE 512 /* fake block size */

/* linux-compatible values for fs type */
#define MSDOS_SUPER_MAGIC     0x4d44
#define NTFS_SUPER_MAGIC      0x5346544E

struct statfs {
   unsigned __int64    f_type;     /* type of filesystem (see below) */
   unsigned __int64    f_bsize;    /* optimal transfer block size */
   unsigned __int64    f_blocks;   /* total data blocks in file system */
   unsigned __int64    f_bfree;    /* free blocks in fs */
   unsigned __int64    f_bavail;   /* free blocks avail to non-superuser */
   unsigned __int64    f_files;    /* total file nodes in file system */
   unsigned __int64    f_ffree;    /* free file nodes in fs */
   unsigned __int64    f_fsid;     /* file system id */
   unsigned __int64    f_namelen;  /* maximum length of filenames */
   unsigned __int64    f_spare[6]; /* spare for later */
};

static int statfs (const unsigned char *path, struct statfs *buf)
  {
    HINSTANCE h;
    FARPROC f;
    int retval = 0;
    WCHAR tmp [MAX_PATH], resolved_path [MAX_PATH];
    WCHAR * wpath = (WCHAR *)utf8_to_utf16(path);
    realpath(wpath, resolved_path);
    free(wpath);
    if (!resolved_path)
      retval = - 1;
    else
      {
        /* check whether GetDiskFreeSpaceExA is supported */
        h = LoadLibraryA ("kernel32.dll");
        if (h)
          f = GetProcAddress (h, "GetDiskFreeSpaceExW");
        else
          f = NULL;
        if (f)
          {
            ULARGE_INTEGER bytes_free, bytes_total, bytes_free2;
            if (!f (resolved_path, &bytes_free2, &bytes_total, &bytes_free))
              {
                errno = ENOENT;
                retval = - 1;
              }
            else
              {
                buf -> f_bsize = FAKED_BLOCK_SIZE;
                buf -> f_bfree = (bytes_free.QuadPart) / FAKED_BLOCK_SIZE;
                buf -> f_files = buf -> f_blocks = (bytes_total.QuadPart) / FAKED_BLOCK_SIZE;
                buf -> f_ffree = buf -> f_bavail = (bytes_free2.QuadPart) / FAKED_BLOCK_SIZE;
              }
          }
        else
          {
            DWORD sectors_per_cluster, bytes_per_sector;
            if (h) FreeLibrary (h);
            if (!GetDiskFreeSpaceW (resolved_path, &sectors_per_cluster,
                   &bytes_per_sector, &buf -> f_bavail, &buf -> f_blocks))
              {
                errno = ENOENT;
                retval = - 1;
              }
            else
              {
                buf -> f_bsize = sectors_per_cluster * bytes_per_sector;
                buf -> f_files = buf -> f_blocks;
                buf -> f_ffree = buf -> f_bavail;
                buf -> f_bfree = buf -> f_bavail;
              }
          }
        if (h) FreeLibrary (h);
      }

    /* get the FS volume information */
    if (wcsspn (L":", resolved_path) > 0) resolved_path [3] = '\0'; /* we want only the root */    
    if (GetVolumeInformationW (resolved_path, NULL, 0, &buf -> f_fsid, &buf -> f_namelen, NULL, tmp, MAX_PATH))
    /* http://msdn.microsoft.com/library/default.asp?url=/library/en-us/fileio/fs/getvolumeinformation.asp */
     {
     	if (_wcsicmp (L"NTFS", tmp) == 0)
     	 {
     	   buf -> f_type = NTFS_SUPER_MAGIC;
     	 }
     	else
     	 {
     	   buf -> f_type = MSDOS_SUPER_MAGIC;
     	 }
     }
    else
     {
       errno = ENOENT;
       retval = - 1;
     }
    return retval;
}


static value
copy_statfs (struct statfs *buf)
{
  CAMLparam0 ();
  CAMLlocal2 (bufv, v);
  bufv = caml_alloc (11, 0);
  v = copy_int64 (buf->f_type); caml_modify (&Field (bufv, 0), v);
  v = copy_int64 (buf->f_bsize); caml_modify (&Field (bufv, 1), v);
  v = copy_int64 (buf->f_blocks); caml_modify (&Field (bufv, 2), v);
  v = copy_int64 (buf->f_bfree); caml_modify (&Field (bufv, 3), v);
  v = copy_int64 (buf->f_bavail); caml_modify (&Field (bufv, 4), v);
  v = copy_int64 (buf->f_files); caml_modify (&Field (bufv, 5), v);
  v = copy_int64 (buf->f_ffree); caml_modify (&Field (bufv, 6), v);
  v = copy_int64 (buf->f_namelen); caml_modify (&Field (bufv, 8), v);
  v = copy_string ("-1"); caml_modify (&Field (bufv, 9), v);
  v = copy_int64 (-1); caml_modify (&Field (bufv, 10), v);
  CAMLreturn (bufv);
}

CAMLprim value
statfs_statfs (value pathv)
{
  CAMLparam1 (pathv);
  CAMLlocal1 (bufv);
  const unsigned char *path = String_val (pathv);
  struct statfs buf;
  if (statfs (path, &buf) == -1)
    raise_constant(*(value *)caml_named_value("error"));
  bufv = copy_statfs (&buf);
  CAMLreturn (bufv);
}

#else  /* defined(__MINGW32__) */

#if (defined HAVE_SYS_PARAM_H && HAVE_SYS_MOUNT_H)
#  include <sys/param.h>
#  include <sys/mount.h>
#  define HAVE_STATS 1
#endif  /* (defined HAVE_SYS_PARAM_H && HAVE_SYS_MOUNT_H) */

#ifdef HAVE_SYS_VFS_H
#  include <sys/vfs.h>
#  define HAVE_STATS 1
#endif  /* HAVE_SYS_VFS_H */

#ifdef HAVE_SYS_STATVFS_H
#  include <sys/statvfs.h>
#endif

#ifdef HAVE_STATS
static value
#if ((defined (sun) || defined (__sun__))) || (defined(__NetBSD__) && (__NetBSD_Version__ > 299000000)) || defined (__hpux__) || defined(__alpha__)
copy_statfs (struct statvfs *buf)
#else
copy_statfs (struct statfs *buf)
#endif  /* ((defined (sun) || defined (__sun__))) || (defined(__NetBSD__) && (__NetBSD_Version__ > 299000000)) || defined (__hpux__) */
{
  CAMLparam0 ();
  CAMLlocal2 (bufv, v);
  bufv = caml_alloc (11, 0);
#if ((defined (sun) || defined (__sun__))) || (defined(__FreeBSD__) && __FreeBSD_version >= 503001) || defined(__OpenBSD__) || defined(__NetBSD__) || defined(__alpha__)
  v = copy_int64 (-1); caml_modify (&Field (bufv, 0), v);
#else
  v = copy_int64 (buf->f_type); caml_modify (&Field (bufv, 0), v);
#endif  /* ((defined (sun) || defined (__sun__))) || (defined(__FreeBSD__) && __FreeBSD_version >= 503001) || defined(__OpenBSD__) || defined(__NetBSD__) */
  v = copy_int64 (buf->f_bsize); caml_modify (&Field (bufv, 1), v);
  v = copy_int64 (buf->f_blocks); caml_modify (&Field (bufv, 2), v);
  v = copy_int64 (buf->f_bfree); caml_modify (&Field (bufv, 3), v);
  v = copy_int64 (buf->f_bavail); caml_modify (&Field (bufv, 4), v);
  v = copy_int64 (buf->f_files); caml_modify (&Field (bufv, 5), v);
  v = copy_int64 (buf->f_ffree); caml_modify (&Field (bufv, 6), v);
#if ((defined (sun) || defined (__sun__))) || defined (__hpux__) || defined(__alpha__)
  v = copy_int64 (-1); caml_modify (&Field (bufv, 7), v);
  v = copy_int64 (buf->f_namemax); caml_modify (&Field (bufv, 8), v);
# if ! defined(__alpha__)
  v = copy_string (buf->f_basetype); caml_modify (&Field (bufv, 9), v);
# else
  v = copy_string ("-1"); caml_modify (&Field (bufv, 9), v);
# endif
  v = copy_int64 (buf->f_frsize); caml_modify (&Field (bufv, 10), v);
#else
#if defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__) || defined(__APPLE__) || defined(__DragonFly__) || defined(__FreeBSD_kernel__)
#  if defined(__OpenBSD__) || defined(__NetBSD__) || (defined(__FreeBSD__) && __FreeBSD_version < 502000) || defined(__DragonFly__) || defined(__APPLE__)
#    include <sys/syslimits.h>
     v = copy_int64 (NAME_MAX); caml_modify (&Field (bufv, 8), v);
#  else
     v = copy_int64 (buf->f_namemax); caml_modify (&Field (bufv, 8), v);
#  endif /* (__OpenBSD__) || defined(__NetBSD__) || (defined(__FreeBSD__) && __FreeBSD_version < 502000) */
  v = copy_string (buf->f_fstypename); caml_modify (&Field (bufv, 9), v);
#else
  v = copy_int64 (buf->f_namelen); caml_modify (&Field (bufv, 8), v);
  v = copy_string ("-1"); caml_modify (&Field (bufv, 9), v);
#endif /* defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__) || defined(__APPLE__) */
  caml_modify (&Field (bufv, 7), Val_unit);
  v = copy_int64 (-1); caml_modify (&Field (bufv, 10), v);
#endif /*  ((defined (sun) || defined (__sun__))) || defined (__hpux__) */
  CAMLreturn (bufv);
}
#endif /* HAVE_STATS */

CAMLprim value
statfs_statfs (value pathv)
{
#ifdef HAVE_STATS
  CAMLparam1 (pathv);
  CAMLlocal1 (bufv);
  const char *path = String_val (pathv);
#if ((defined (sun) || defined (__sun__))) || (defined(__NetBSD__) && (__NetBSD_Version__ > 299000000)) || defined (__hpux__) || defined(__alpha__)
  struct statvfs buf;
  if (statvfs (path, &buf) == -1)
#else
  struct statfs buf;
  if (statfs (path, &buf) == -1)
#endif  /* ((defined (sun) || defined (__sun__))) || (defined(__NetBSD__) && (__NetBSD_Version__ > 299000000)) || defined (__hpux__) */
    raise_constant(*(value *)caml_named_value("error"));
  bufv = copy_statfs (&buf);
  CAMLreturn (bufv);
#else
  raise_constant(*(value *)caml_named_value("not supported"));
#endif  /* HAVE_STATS */
}
#endif  /* defined(__MINGW32__) */

/*******************************************************************


                         external_start
                         external_exit


*******************************************************************/

#if defined(__MINGW32__)
static HWND myHWND = NULL;
#endif

value
external_start (void)
{

/* Disable close button on console */
#if defined(__MINGW32__)
  char *buf = "[MLDonkey TitleSearch]\0"; /* if multiple instances */
  SetConsoleTitle((LPCTSTR)buf);
  myHWND = FindWindowEx(NULL, NULL, NULL, (LPCTSTR)buf);

  if (myHWND != NULL) {
    HMENU hmenu = GetSystemMenu(myHWND, FALSE);
    MENUITEMINFO CloseItem;
    CloseItem.cbSize = sizeof(MENUITEMINFO);
    CloseItem.fMask = MIIM_ID;
    CloseItem.wID = SC_MINIMIZE+1;
    EnableMenuItem(hmenu, SC_CLOSE, MF_BYCOMMAND | MF_GRAYED);
    SetMenuItemInfo(hmenu, SC_CLOSE, FALSE, &CloseItem);
    DrawMenuBar(myHWND);
  }
#endif  /* defined(__MINGW32__) */

#if defined(HAVE_PTHREAD) && defined(PTW32_STATIC_LIB)
	pthread_win32_process_attach_np();
#endif
	return Val_unit;

}

value
external_exit (void) 
{
/* Revert console system menu */
#if defined(__MINGW32__)
  if (myHWND != NULL) {
    HMENU hmenu = GetSystemMenu(myHWND, FALSE);
    MENUITEMINFO CloseItem;
    CloseItem.cbSize = sizeof(MENUITEMINFO);
    CloseItem.fMask = MIIM_ID;
    CloseItem.wID = SC_CLOSE;
    SetMenuItemInfo(hmenu, SC_MINIMIZE+1, FALSE, &CloseItem);
    EnableMenuItem(hmenu, SC_CLOSE, MF_BYCOMMAND | MF_ENABLED);
    DrawMenuBar(myHWND);
  }
#endif  /* defined(__MINGW32__) */

#if defined(HAVE_PTHREAD) && defined(PTW32_STATIC_LIB)
	pthread_win32_process_detach_np();
#endif

#if defined(HAVE_CRYPTOPP)
/*	crypto_exit(); */
#endif
	return Val_unit;
}

/*******************************************************************


                         ml_uname


*******************************************************************/

value
ml_uname(void) 
{
	char buf[4096];
	buf[0] = '\0';
	os_uname(buf);
	return caml_copy_string(buf);
}

/*******************************************************************


                         ml_check_endianness


*******************************************************************/

value 
ml_check_endianness(void)
{
  CAMLparam0 ();
  CAMLlocal1 (v);
#ifdef ARCH_BIG_ENDIAN
  v = copy_string ("big endian");
#else
  v = copy_string ("little endian");
#endif
  CAMLreturn (v);
}

/*******************************************************************


                         rlimit


*******************************************************************/

value 
ml_getrlimit(value resource) 
{
#ifdef HAVE_GETRLIMIT
  CAMLparam1(resource);
  CAMLlocal1(retval);
  int r;
  struct rlimit lim;

  switch (Int_val(resource)) {
  case 0:
#ifdef RLIMIT_CPU
    r = RLIMIT_CPU;
#else
    r = -1;
#endif
    break;
  case 1:
#ifdef RLIMIT_FSIZE
    r = RLIMIT_FSIZE;
#else
    r = -1;
#endif
    break;
  case 2:
#ifdef RLIMIT_DATA
    r = RLIMIT_DATA;
#else
    r = -1;
#endif
    break;
  case 3:
#ifdef RLIMIT_STACK
    r = RLIMIT_STACK;
#else
    r = -1;
#endif
    break;
  case 4:
#ifdef RLIMIT_CORE
    r = RLIMIT_CORE;
#else
    r = -1;
#endif
    break;
  case 5:
#ifdef RLIMIT_RSS
    r = RLIMIT_RSS;
#else
    r = -1;
#endif
    break;
  case 6:
#ifdef RLIMIT_NPROC
    r = RLIMIT_NPROC;
#else
    r = -1;
#endif
    break;
  case 7:
#ifdef RLIMIT_NOFILE
    r = RLIMIT_NOFILE;
#elif RLIMIT_OFILE
    r = RLIMIT_OFILE:
#else
    r = -1;
#endif
    break;
  case 8:
#ifdef RLIMIT_MEMLOCK
    r = RLIMIT_MEMLOCK;
#else
    r = -1;
#endif
    break;
  case 9:
#ifdef RLIMIT_AS
    r = RLIMIT_AS;
#else
    r = -1;
#endif
    break;
  default:
    errno = EINVAL;
    uerror("getrlimit", Nothing);    
  }

  if (getrlimit(r, &lim) < 0) 
    uerror("getrlimit", Nothing);

  
  retval = alloc_tuple(2);
  Field(retval, 0) = Val_int(lim.rlim_cur);
  Field(retval, 1) = Val_int(lim.rlim_max);

  CAMLreturn(retval);

#else
  failwith("getrlimit unimplemented");
#endif
}

value 
ml_setrlimit(value resource, value rlimit) 
{
#ifdef HAVE_SETRLIMIT
  int r;
  struct rlimit lim;

  switch (Int_val(resource)) {
  case 0:
#ifdef RLIMIT_CPU
    r = RLIMIT_CPU;
#else
    r = -1;
#endif
    break;
  case 1:
#ifdef RLIMIT_FSIZE
    r = RLIMIT_FSIZE;
#else
    r = -1;
#endif
    break;
  case 2:
#ifdef RLIMIT_DATA
    r = RLIMIT_DATA;
#else
    r = -1;
#endif
    break;
  case 3:
#ifdef RLIMIT_STACK
    r = RLIMIT_STACK;
#else
    r = -1;
#endif
    break;
  case 4:
#ifdef RLIMIT_CORE
    r = RLIMIT_CORE;
#else
    r = -1;
#endif
    break;
  case 5:
#ifdef RLIMIT_RSS
    r = RLIMIT_RSS;
#else
    r = -1;
#endif
    break;
  case 6:
#ifdef RLIMIT_NPROC
    r = RLIMIT_NPROC;
#else
    r = -1;
#endif
    break;
  case 7:
#ifdef RLIMIT_NOFILE
    r = RLIMIT_NOFILE;
#elif RLIMIT_OFILE
    r = RLIMIT_OFILE:
#else
    r = -1;
#endif
    break;
  case 8:
#ifdef RLIMIT_MEMLOCK
    r = RLIMIT_MEMLOCK;
#else
    r = -1;
#endif
    break;
  case 9:
#ifdef RLIMIT_AS
    r = RLIMIT_AS;
#else
    r = -1;
#endif
    break;
  default:
    errno = EINVAL;
    uerror("getrlimit", Nothing);    
  }

  lim.rlim_cur = Int_val(Field(rlimit, 0));
  lim.rlim_max = Int_val(Field(rlimit, 1));

  if (setrlimit(r, &lim) < 0) 
    uerror("setrlimit", Nothing);

  return Val_unit;

#else
  failwith("getrlimit unimplemented");
#endif
}

/*******************************************************************


                         ml_os_supported


*******************************************************************/

value ml_os_supported(value unit)
{
	int buf = os_os_supported();
		
	if ( buf == 1 )
	  return Val_true;
	else
	  return Val_false;
}

/*
 * fsync
 */
CAMLprim value ml_fsync(value v_fd)
{
    CAMLparam1(v_fd);
    int r = 0;
    OS_FD fd;

    if (OS_IS_FD(v_fd)) 
      fd = Fd_val(v_fd); 
    else 
      caml_invalid_argument("fsync");

    caml_enter_blocking_section();
    r = os_fsync(fd);
    caml_leave_blocking_section();
    if (0 != r)
      uerror("fsync",Nothing);
    CAMLreturn(Val_unit);
}

