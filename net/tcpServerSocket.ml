(* Copyright 2001, 2002 b8_bavard, b8_fee_carabine, INRIA *)
(*
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
*)

open BasicSocket

type event = 
  CONNECTION of Unix.file_descr * Unix.sockaddr
| BASIC_EVENT of BasicSocket.event

type t = {
    mutable sock : BasicSocket.t;
    mutable event_handler : handler;
  }
and handler = t -> event -> unit


let set_handler t event handler =
  let old_handler = t.event_handler in
  let handler t ev =
    if ev = event then
      handler t
    else
      old_handler t ev
  in
  t.event_handler <- handler

let sock t = t.sock
  
let closed t = closed t.sock
let close t = close t.sock
  
let tcp_handler t sock event = 
  match event with
  | CAN_READ 
  | CAN_WRITE ->
      let s,id = Unix.accept (fd sock) in
      t.event_handler t (CONNECTION (s,id))
  | _ -> t.event_handler t (BASIC_EVENT event)
      
let dummy_sock = Obj.magic 0  
  
let create name addr port handler =
  try
    let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.setsockopt fd Unix.SO_REUSEADDR true;
    Unix.set_close_on_exec fd;
    Unix.bind fd (Unix.ADDR_INET ((*Unix.inet_addr_any*) addr, port));
    Unix.listen fd 10;
    let t = {
        sock = dummy_sock;
        event_handler = handler;
      } in
    let sock = create name fd (tcp_handler t) in
    t.sock <- sock;
    t
  with e ->
      Printf.printf "Exception: %s at port %d" (Printexc.to_string e) port;
      print_newline ();
      raise e
      