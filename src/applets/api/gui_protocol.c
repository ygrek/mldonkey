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
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <string.h>
#include <netdb.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include "gui_protocol.h"








#define MAX_MESSAGE_LEN 65000
static int write_pos = 0;
static char write_buf[MAX_MESSAGE_LEN];
static char read_buf[MAX_MESSAGE_LEN];
static int read_pos;


static int mldonkey_sock = -1;
static int mldonkey_state = MLDONKEY_DISCONNECTED;
static mldonkey_config *old_config = NULL;


/******** Write operations ********/

void init_message()
{ write_pos = 0; }

void write_int8(int code)
{
  write_buf[write_pos++] = code;
}

void write_opcode(int code)
{
  write_buf[write_pos++] = code;
}

void write_int16(int code)
{
  INT_TO_BUF16(code, write_buf, write_pos);
  write_pos += 2;
}

void write_int32(int code)
{
  INT_TO_BUF32(code, write_buf, write_pos);
  write_pos += 4;
}

void write_int64(int64 code)
{
  INT_TO_BUF64(code, write_buf, write_pos);
  write_pos += 8;
}

void write_string(char *str)
{
  if(str == NULL){
    write_int16(0); 
  } else {
    int len = strlen(str);
    write_int16(len);
    memcpy((void *) (write_buf + write_pos), (void *) str, (size_t) len);
    write_pos += len;
  }
}


int write_message(char *mtype)
{
  char header[4];

  printf("len msg %d\n", write_pos); 
  INT_TO_BUF32(write_pos, header, 0);
  if(4 != write(mldonkey_sock, header, 4) ||
      write_pos != write(mldonkey_sock,(void *)write_buf,(size_t)write_pos)){
    printf("Error\n");
    printf("Error in transmitting %s\n",mtype);
    write_pos = 0;

    /* Immediatly close the connection */
    close(mldonkey_sock);
    mldonkey_state = MLDONKEY_DISCONNECTED;
    mldonkey_sock = -1;
    return -1;
  } else {
    printf("Message sent\n");
    write_pos = 0;
    return 0;
  }
}


/********** Read operations ********/

int read_int8()
{
  return read_buf[read_pos++];
}

int read_int16()
{
  int i = BUF16_TO_INT(read_buf, read_pos);
  read_pos += 2;
  return i;
}

int read_int32()
{
  int i = BUF32_TO_INT(read_buf, read_pos);
  read_pos += 4;
  return i;
}

int64 read_int64()
{
  int64 i = BUF64_TO_INT(read_buf, read_pos);
  read_pos += 8;
  return i;
}

char* read_string()
{
  char *buf;
  int len;
  
  len = BUF16_TO_INT(read_buf, read_pos);
  read_pos += 2;
    
  buf = (char *) malloc((size_t) len+1);
  memmove(read_buf + read_pos, buf, len);
  buf[len] = 0;
  read_pos += len;

  return buf;
}

/***************************************************************/

extern void close_sock();

/* This function returns the number of messages read, 0 if it blocks, 
-1 on error. */
int cut_messages(int reinit)
{
  int nread;
  static int toread = 0;
  static int pos = 0;

  if(reinit){
    printf("reinit\n");
    toread = 0;
    pos = 0;
    read_pos = 0;
    return 0;
  }

  printf("cut_messages\n");

  while(1){
    if(toread == 0){
      nread = read(mldonkey_sock, read_buf+pos,4-pos);
      printf("HEADER nread %d\n", nread);
      if(nread <= 0){
        if(errno == EAGAIN) {
          printf("wait...\n");
          return 0; 
        } else { 
          close_sock();
          pos = 0; toread = 0; return -1; }
      }
      pos += nread;
      if(pos == 4){
        toread = BUF32_TO_INT(read_buf,0);
        pos = 0;
      }
    } else {
      nread = read(mldonkey_sock, read_buf+pos, toread - pos);
      printf("PAYLOAD nread %d\n", nread);
      if(nread <= 0){
	if(errno == EAGAIN) return 0; else { 
          pos = 0; toread = 0; 
          close_sock();
          return -1; }
      }
      pos += nread;
      if(pos == toread){
        /* We have one message !!! */
        int old_pos = pos;
        read_pos = 0;
        pos = 0;
        toread = 0;
        
        return old_pos;
      }
    }
  }
}


void close_sock()
{
  printf("close sock\n");

  old_config = NULL;
  if(mldonkey_sock >= 0) close(mldonkey_sock);
  mldonkey_sock = -1;
  mldonkey_state = MLDONKEY_DISCONNECTED;
  cut_messages(1);
}

int mldonkey_connect(mldonkey_config *config)
{
  
  if(config != old_config){
    struct sockaddr_in sa;    
    int retcode;
    close_sock();


    old_config=config;
    /* resolve hostname */
    memset(&sa, 0, sizeof(sa));
    
    if(config->mldonkey_hostname == NULL)
      config->mldonkey_hostname = "127.0.0.1";
    if(config->mldonkey_hostname[0] >= '0' && 
        config->mldonkey_hostname[0] <= '9'){
      #ifdef HAS_INET_ATON
      if (inet_aton(config->mldonkey_hostname, &sa.sin_addr) == 0) return -1;
      #else
      printf("host %s\n", config->mldonkey_hostname);

      sa.sin_addr.s_addr = inet_addr(config->mldonkey_hostname);
      printf("addr %lx\n", sa.sin_addr.s_addr);
      if (sa.sin_addr.s_addr == (unsigned int) -1) return -1;
      #endif
    
    } else {
      struct hostent * hp;
      hp = gethostbyname(config->mldonkey_hostname);
      if (hp == (struct hostent *) NULL) return -1;
      sa.sin_addr.s_addr = (unsigned int)hp->h_addr_list[0];
    }
    
    printf("port %d\n", config->mldonkey_port);
    sa.sin_port = htons(config->mldonkey_port); 
    sa.sin_family = AF_INET;
    
    if ((mldonkey_sock = socket(PF_INET, SOCK_STREAM, 0)) < 0) {
      perror("Opening socket");
      close_sock();
      return -1;
    }

    if( connect(mldonkey_sock, (struct sockaddr *) &sa, sizeof(sa)) < 0) {
      if (errno != EAGAIN && 
          errno != EINTR && 
          errno != EINPROGRESS && 
          errno != EWOULDBLOCK) {
        perror("Connection");
        close_sock();
        return -1;
      }
      printf("Waiting for connect\n");
    }
    printf("Connecting\n");

    
    retcode = fcntl(mldonkey_sock, F_GETFL, 0);
    if (retcode == -1 ||
        fcntl(mldonkey_sock, F_SETFL, retcode | O_NONBLOCK) == -1){
      return -1;
    }


    mldonkey_state = MLDONKEY_CONNECTING;
    return 0;
  }
  
  return 0;
}

int mldonkey_can_read()
{
  return cut_messages(0);
}

int mldonkey_info_message(mldonkey_info *info)
{
  int opcode = read_int16();
  printf("Message %d\n", opcode);
  
  switch(opcode){
    
    case CoreProtocol :
    printf("CoreProtocol\n");
    
    init_message();
    
    write_int16(0); /* GUI protocol */
    write_int32(10); /* Version 10 ! */
    write_message("GuiProtocol");
    
    printf("Message sent\n");
    
    
    write_int16(47); /* GUI protocol */
    
    write_int16(1);
    write_int32(1);
    write_int8(1);
    write_message("GuiExtensions");
    
    
    init_message();
    printf("Message sent 1\n");
    write_int16(5); /* Password */
    printf("Message sent 2: %s\n", old_config->mldonkey_password);
    write_string(old_config->mldonkey_password);
    printf("Message sent 3\n");
    write_message("Password");
    
    printf("Message sent\n");
    break;
    
    case BadPassword : 
    printf("******  BAD PASSWORD ******\n");
    close_sock();
    break;
    
    case Client_stats:
    case Client_stats_v2:
    case Client_stats_v3:
    printf("Client stats format too old...\n");
    break;
    
    case Client_stats_v4:
    mldonkey_state = MLDONKEY_CONNECTED;
    
    info->upload_counter = read_int64();
    info->download_counter = read_int64();
    info->shared_counter = read_int64();
    info->nshared_files = read_int32();
    info->tcp_upload_rate = read_int32();
    info->tcp_download_rate = read_int32();
    info->udp_upload_rate = read_int32();
    info->udp_download_rate = read_int32();
    info->ndownloading_files = read_int32();
    info->ndownloaded_files = read_int32();
    
    printf("Client stats !!!\n");
    break;

    default:
    printf("Nothing to do with opcode %d\n", opcode);
  }


  printf("Done with message\n");
  return 0;
}

int get_mldonkey_status(mldonkey_config *config, mldonkey_info *info)
{
  if( mldonkey_connect(config) >= 0){
    while(mldonkey_can_read() > 0){
      mldonkey_info_message(info);
    }
  } 
  return mldonkey_state; 
}

