(***********************************************************************)
(*                               MLChat                                *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)


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
