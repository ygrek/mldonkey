/* ---*-C++-*---------------------------------------------------------------
Copyright (C) 1999, 2000, 2001, 2002, 2003 Simon Patarin, INRIA

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
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


#ifndef TCPPRINT_COMPONENT_H
#define TCPPRINT_COMPONENT_H

#include <libpandora/global.h>

#include <libpandora/outputcomponent.h>

class Packet;

class TCPPrintComponent : public OutputComponent { 
private:
  static int id;
  int cnx;

public:
  component_init(TCPPrintComponent, 1);
  TCPPrintComponent(void) :cnx(0)	{ }
  virtual ~TCPPrintComponent(void) 	{ }

  virtual bool add(Packet *p); 
  virtual void setup(void)   		{ cnx = ++id; }
  virtual void cleanup(void) 		{ cnx = -1; }
};

#endif /* TCPPRINT_COMPONENT_H */

