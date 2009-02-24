/* Copyright (C) 1999, 2000, 2001, 2002, 2003 Simon Patarin, INRIA

This file is part of Pandora, the Flexible Monitoring Platform.

Pandora is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Pandora is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Pandora; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */


#include <libpandora/global.h>

#include "tcpprintcomponent.h" 
#include <pandora_components/ippacket.h> 
#include <pandora_components/tcppacket.h> 
#include <pandora_components/udppacket.h> 

component_export(TCPPrintComponent, TCPPacket+|UDPPacket+,);

int TCPPrintComponent::id = 0;

bool TCPPrintComponent::add(Packet *pkt) 
{
  int sport = 0, dport = 0;

  locatePacket0(TCPPacket, tcpp, pkt);
  locatePacket0(UDPPacket, udpp, pkt);

  if (tcpp != NULL) {
    sport = tcpp->sport;
    dport = tcpp->dport;
  } else if (udpp != NULL) {
    sport = udpp->sport;
    dport = udpp->dport;
  }

  locatePacket(IPPacket, ipp, pkt);

  if (ipp == NULL) {
    cleanPacket(pkt);
    return false;
  }

  int len = ipp->dlength();

  if (len == 0) {
    discard(pkt);
    return false;
  }

  cout << "new_packet ";

  char *data = ipp->data();
  if (tcpp != NULL) {
    cout << "TCP " << cnx;
  }   else {
    cout << "UDP " << cnx;
 }

  cout << " \"" << ipp->src << "\" " << ntohs(sport) << " \"" 
       << ipp->dst << "\" " << ntohs(dport) << "\n";

  cout << "\"";
  for (int i = 0; i < len; ++i) {
    int c = (int) (unsigned char)data[i];
    if (c == '\\') cout << "\\\\";
    else
    if (c == '"') cout << "\\\"";
    else
    if (c == '\n' || (c > 31 && c < 127))
      cout << data[i];
    else
      printf("\\%03d", c);
  }
  printf( "\";\n");

  cout << "\n(*----------------------------------------------------------------------*)\n";

  discard(pkt);
  return false;
} 


