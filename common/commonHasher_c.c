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
#include "../config/config.h"
#include "../lib/os_stubs.h"

#include "caml/mlvalues.h"
#include "caml/fail.h"
#include "caml/alloc.h"

#define METHOD_MD4      Val_int(0)
#define METHOD_MD5      Val_int(1)
#define METHOD_SHA1     Val_int(2)

#define JOB_BEGIN_POS   1
#define JOB_LEN         2
#define JOB_METHOD      3
#define JOB_RESULT      4
#define JOB_HANDLER     5

extern void md4_unsafe64_fd_direct (OS_FD fd, long pos, long len, 
  unsigned char *digest);

extern void md5_unsafe64_fd_direct (OS_FD fd, long pos, long len, 
  unsigned char *digest);

#ifndef HAVE_LIBPTHREAD

value ml_job_start(value job_v, value fd_v)
{
  OS_FD fd = Fd_val(fd_v);
  long pos = Int64_val(Field(job_v, JOB_BEGIN_POS));
  long len = Int64_val(Field(job_v, JOB_LEN));
  unsigned char *digest = String_val(Field(job_v, JOB_RESULT));

  if(Field(job_v, JOB_METHOD) == METHOD_MD4)
    { md4_unsafe64_fd_direct(fd, pos, len, digest); return Val_unit; }

  if(Field(job_v, JOB_METHOD) == METHOD_MD5)
    { md5_unsafe64_fd_direct(fd, pos, len, digest); return Val_unit; }

  printf("commonHasher_c.c: method sha1 not implemented\n");
  exit(2);

  return Val_unit;
}

value ml_job_done(value job_v)
{
  return Val_true;
}

#else
#include <string.h>
#include <pthread.h>
#include <signal.h>
#include <sys/time.h>

/* We use this shared variable for thread synchronization... yes, mutexes
and conditions would be better... wait for the patch */
static int volatile  job_done = 1;

/* We use these variables for thread communication */
static char volatile job_result[64];
static OS_FD volatile  job_fd = 0;
static long volatile job_begin_pos = 0;
static long volatile job_len = 0;
static int volatile job_method = 0;

static int thread_started = 0;

static pthread_t pthread;
static pthread_cond_t cond;
static pthread_mutex_t mutex;

value ml_job_done(value job_v)
{
  if(job_done){
    value result_v = Field(job_v, JOB_RESULT);
    char *result = String_val(result_v);
    int result_len = string_length(result_v);

/*    printf("job done\n");     */
    memcpy(result, job_result, result_len);
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

/*    fprintf(stderr,"waiting for next job\n");  */
    pthread_cond_timedwait(&cond, &mutex, &timeout);
    
    if(!job_done){

/*      fprintf(stderr,"job started\n");  */
      
      if(job_method == METHOD_MD4)
        md4_unsafe64_fd_direct(job_fd, job_begin_pos, job_len, job_result);
      else
        if( job_method == METHOD_MD5)
          md5_unsafe64_fd_direct(job_fd, job_begin_pos, job_len, job_result);
        else {
          printf("commonHasher_c.c: method sha1 not implemented\n");
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
  job_fd = Int_val(fd_v);
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

#endif

