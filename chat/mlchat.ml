(**************************************************************************)
(*  Copyright 2003, 2002 b8_bavard, b8_zoggy, , b52_simon INRIA            *)
(*                                                                        *)
(*    This file is part of mldonkey.                                      *)
(*                                                                        *)
(*    mldonkey is free software; you can redistribute it and/or modify    *)
(*    it under the terms of the GNU General Public License as published   *)
(*    by the Free Software Foundation; either version 2 of the License,   *)
(*    or (at your option) any later version.                              *)
(*                                                                        *)
(*    mldonkey is distributed in the hope that it will be useful,         *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of      *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       *)
(*    GNU General Public License for more details.                        *)
(*                                                                        *)
(*    You should have received a copy of the GNU General Public License   *)
(*    along with mldonkey; if not, write to the Free Software             *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston,               *)
(*    MA  02111-1307  USA                                                 *)
(*                                                                        *)
(**************************************************************************)

class type config = Chat_config.config 

class file_config = Chat_config.config

type port = int
type host = string
type address = host * port
type message = string
type version = Chat_proto.version
type id = string
type source = version * id * address
type proto = Chat_proto.proto =
    HelloOk | Hello | Byebye | Message of message 
  | AddOpen of id * address
  | RoomMessage of id * (id * host * port) list * message
type packet = source * id * proto

let version () = Chat_messages.software_version

class type com = Chat_proto.com

let read_packet = Chat_proto.read_packet_buffer
let read_packet_channel = Chat_proto.read_packet_channel

let write_packet = Chat_proto.write_packet
let write_packet_channel = Chat_proto.write_packet_channel

class udp (conf : config) = Chat_proto.udp conf
class tcp (conf : config) = Chat_proto.tcp conf

let default_pred (i1,h1,p1) (i2,h2,p2) =
  h1=h2 && p1=p2

class app 
    ?(no_quit=false) 
    ?(pred=default_pred)
    (conf : config) (com : com) =
  let chat = new Chat_app.app pred ~no_quit conf com in
  object
    method box = chat#box
    method coerce = chat#coerce
    method init_window (w : GWindow.window) =
      w#add_accel_group chat#accelgroup
  end
