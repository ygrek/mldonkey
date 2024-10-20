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

#include <stdio.h>
#include <string.h>
#include <pthread.h>
#include <signal.h>
#include <sys/time.h>

#include "../../../config/config.h"
#include "../../utils/lib/os_stubs.h"

#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <caml/alloc.h>
#include <caml/signals.h>

#define METHOD_MD4      Val_int(0)
#define METHOD_MD5      Val_int(1)
#define METHOD_SHA1     Val_int(2)
#define METHOD_TIGER     Val_int(3)

#define JOB_BEGIN_POS   1
#define JOB_LEN         2
#define JOB_METHOD      3
#define JOB_RESULT      4
#define JOB_HANDLER     5
#define JOB_ERROR       6

#include "../../utils/lib/md4.h"
#include "../../utils/lib/md5.h"
#include "../../utils/lib/sha1_c.h"
#include "../../utils/lib/tiger.h"



/* Use a different buffer to avoid sharing it between the two threads !! */
static unsigned char local_hash_buffer[HASH_BUFFER_LEN];



#define COMPLETE_HASH(HASH_NAME,HASH_CONTEXT,HASH_INIT,HASH_APPEND,HASH_FINISH) \
static void HASH_NAME##_unsafe64_fd_direct (OS_FD fd, OFF_T pos, OFF_T len, \
  unsigned char *digest) \
{ \
  HASH_CONTEXT context; \
  ssize_t nread; \
 \
  HASH_INIT (&context); \
  os_lseek(fd, pos, SEEK_SET); \
 \
  while (len!=0){ \
    size_t max_nread = HASH_BUFFER_LEN > len ? len : HASH_BUFFER_LEN; \
 \
    nread = os_read (fd, local_hash_buffer, max_nread); \
 \
    if(nread <= 0) { \
        unix_error(errno, "HASH_NAME##unsafe64_fd_direct: Read", Nothing); \
    } \
    if(nread == 0){ \
      HASH_FINISH (&context, digest); \
 \
      return; \
    } \
 \
    HASH_APPEND (&context, local_hash_buffer, nread); \
    len -= nread; \
  } \
  HASH_FINISH (&context, digest); \
}

COMPLETE_HASH(sha1,SHA1_CTX,sha1_begin,sha1_hash, sha1_end)
COMPLETE_HASH(md5,md5_state_t,md5_init,md5_append,md5_finish)
COMPLETE_HASH(md4,MD4_CTX,MD4Init,MD4Update,md4_finish)

static void tiger_tree_fd(OS_FD fd, size_t len, OFF_T pos, 
  size_t block_size, char *digest)
{
  static char tiger_buffer[BLOCK_SIZE+1];

  if(block_size == BLOCK_SIZE){
    size_t length = (len - pos > BLOCK_SIZE) ? BLOCK_SIZE : len - pos;
    char *s = tiger_buffer+1;
    size_t toread = length;
    char *curs = s;

      while (toread>0){
      size_t max_nread = toread;
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

/* We use this shared variable for thread synchronization... yes, mutexes
and conditions would be better... wait for the patch */
static int volatile  job_done = 1;

/* We use these variables for thread communication */
static char volatile job_result[64];
static OS_FD volatile  job_fd = 0;
static OFF_T volatile job_begin_pos = 0;
static long volatile job_len = 0;
static int volatile job_method = 0;

static int thread_started = 0;

static pthread_t pthread;
static pthread_cond_t cond;
static pthread_mutex_t mutex;

static char *p_job_result = (char *)job_result;

value ml_job_done(value job_v)
{
  if(job_done){
    value result_v = Field(job_v, JOB_RESULT);
    char *result = String_val(result_v);
    int result_len = string_length(result_v);

/*    printf("job len done: %d\n", result_len);     */
    memcpy(result, p_job_result, result_len);
    return Val_true;
  }

  return Val_false;
}

static void * hasher_thread(void * arg)
{
  struct timeval now;
  struct timespec timeout;

#if !defined(PTW32_STATIC_LIB) 
  sigset_t mask;

  /* Block all signals so that we don't try to execute a Caml signal handler */
  sigfillset(&mask);
  pthread_sigmask(SIG_BLOCK, &mask, NULL);
  
  nice(19);
#endif
  
  pthread_mutex_lock(&mutex);
  
  while(1){
    gettimeofday(&now, NULL);
    timeout.tv_sec = now.tv_sec + 10;
    timeout.tv_nsec = now.tv_usec * 1000;

/*    fprintf(stderr,"waiting for next job\n");  */
    pthread_cond_timedwait(&cond, &mutex, &timeout);
    
    if(!job_done) {

/*      fprintf(stderr,"job started\n");   */
      
      long bsize;
      switch(job_method) {
      case METHOD_MD4:
        md4_unsafe64_fd_direct(job_fd, job_begin_pos, job_len, p_job_result);
      break;

      case METHOD_MD5:
        md5_unsafe64_fd_direct(job_fd, job_begin_pos, job_len, p_job_result);
      break;

      case METHOD_SHA1:
        sha1_unsafe64_fd_direct(job_fd, job_begin_pos, job_len, p_job_result);
      break;

      case METHOD_TIGER:
        bsize = tiger_block_size(job_len);
        os_lseek(job_fd, job_begin_pos, SEEK_SET);
        tiger_tree_fd(job_fd, job_len, 0, bsize, p_job_result);
      break;

      default:
      printf("commonHasher_c.c: method not implemented\n");
      exit(2);
      }
      /*        fprintf(stderr,"job finished\n");  */
      job_done = 1;
    }
  }
  
  return NULL;
}

value ml_job_start(value job_v, value fd_v)
{
  // job_fd = Int_val(fd_v);
	job_fd = Fd_val(fd_v);
  job_begin_pos = Int64_val(Field(job_v, JOB_BEGIN_POS));
  job_len = Int64_val(Field(job_v, JOB_LEN));
  job_method = Field(job_v, JOB_METHOD);

/*  fprintf(stderr,"ml_job_start\n"); */

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
/*  printf("Starting job\n"); */
  job_done = 0; /* Thread can run ... */
  pthread_cond_signal(&cond);  
  pthread_mutex_unlock(&mutex);
  leave_blocking_section ();

  return Val_unit;
}

