/***********************************************************************/
/*                                                                     */
/*                             ____                                    */
/*                                                                     */
/*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       */
/*                                                                     */
/*  Copyright 1999 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

#include <sys/ioctl.h>
#include "caml/mlvalues.h"
#include <unistd.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/types.h>
#include <stdio.h>
#include <sys/socket.h>


#define GET_INET_ADDR(v) (*((uint32 *) (v)))

value setsock_multicast(value sock_v)
{
  int sock = Int_val(sock_v);
  char value;

  value = 64;   /* Set the time-to-live:  only within site. */
  printf("ttl %d\n", value);
  return Val_int(setsockopt(sock,
      IPPROTO_IP, IP_MULTICAST_TTL,
      &value, sizeof(value)));
}

value setsock_join(value sock_v, value group_v)
{
   struct ip_mreq mreq;
   int sock = Int_val(sock_v);
  
  mreq.imr_multiaddr.s_addr = GET_INET_ADDR(group_v);
  mreq.imr_interface.s_addr = INADDR_ANY;

  return Val_int(setsockopt(sock, 
      IPPROTO_IP, IP_ADD_MEMBERSHIP, 
      (void*)&mreq, sizeof(mreq)));
}

value setsock_leave(value sock_v, value group_v) 
{
  struct ip_mreq mreq;
  int sock = Int_val(sock_v);

  mreq.imr_multiaddr.s_addr = GET_INET_ADDR(group_v);
  mreq.imr_interface.s_addr = INADDR_ANY;

  return Val_int(setsockopt(sock,
      IPPROTO_IP, IP_DROP_MEMBERSHIP, 
      (void*)&mreq, sizeof(mreq)));
}



