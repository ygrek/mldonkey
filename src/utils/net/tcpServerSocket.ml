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

open Printf2
open BasicSocket

type event = 
  CONNECTION of Unix.file_descr * Unix.sockaddr
| BASIC_EVENT of BasicSocket.event

type connections_controler = {
    mutable cc_name : string;
    mutable nconnections_last_second : int;
    mutable accept_connections : (int -> int -> bool);
    mutable allow_accept : bool ref;
  }
  
and t = {
    mutable name : string;
    mutable closed : bool;
    mutable sock : BasicSocket.t;
    mutable accept_control : connections_controler option;
    mutable event_handler : handler;
  }
  
and handler = t -> event -> unit


let connections_controlers = ref []
let nconnections_last_second = ref 0
let debug = ref false

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
let close t reason = 
  if not t.closed then begin
      close t.sock reason;
      match t.accept_control with
        None -> () | Some _ -> t.accept_control <- None
    end
  
let tcp_handler t sock event = 
  match event with
  | CAN_READ 
  | CAN_WRITE ->
    begin
     try
      let s,id = Unix.accept (fd sock) in
      if !verbose_bandwidth > 1 then lprintf_nl "[BW2 %6d] accept on %s" (last_time ()) t.name;
      (match t.accept_control with
          None -> () | Some cc -> 
            cc.nconnections_last_second <- cc.nconnections_last_second + 1);
      incr nconnections_last_second;
      t.event_handler t (CONNECTION (s,id))
     with e ->
      if !debug then
        lprintf "Exception tcp_handler: %s\n" (Printexc2.to_string e);
      raise e
    end
  | _ -> t.event_handler t (BASIC_EVENT event)
      
let dummy_sock = Obj.magic 0  
  
let create name addr port ?(backlog = 3) handler =
  try
    let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.setsockopt fd Unix.SO_REUSEADDR true; 
    MlUnix.set_close_on_exec fd;  
    Unix.bind fd (Unix.ADDR_INET ((*Unix.inet_addr_any*) addr, port));
    Unix.listen fd backlog;
    let t = {
        name = name;
        sock = dummy_sock;
        event_handler = handler;
        accept_control = None;
        closed = false;
      } in
    let sock = create name fd (tcp_handler t) in
    prevent_close sock;
    t.sock <- sock;
    t
  with e ->
    lprint_newline ();
    lprintf "Exception: %s at port %d\n" (Printexc2.to_string e) port;
    match e with
      Unix.Unix_error (Unix.EADDRINUSE, _, _) -> 
        lprintf "This is normally caused by another application currently using this port.\n";
        lprintf "Close that application and restart MLDonkey, exiting...\n";
        Pervasives.exit 69
      | _ -> raise e
  
let create_connections_controler name f =
  let cc = {
      cc_name = name;
      nconnections_last_second = 0;
      allow_accept = ref true;
      accept_connections = f;    
    }  in
  connections_controlers := cc :: !connections_controlers;
  cc
      
let set_accept_controler t cc =
  t.accept_control <- Some cc;
  set_allow_read t.sock cc.allow_accept
  
let _ =
  add_bandwidth_second_timer (fun _ ->
      
      if !verbose_bandwidth > 0 then begin
          if !nconnections_last_second > 0 then
            lprintf_nl "[BW1 %6d] %d incoming connections last second:"
              (last_time ())  !nconnections_last_second;
          List.iter (fun cc ->
              if cc.nconnections_last_second > 0 then
                lprintf_nl "[BW1 %6d]     %20s: %d incoming connections"
                  (last_time ()) cc.cc_name cc.nconnections_last_second
          ) !connections_controlers
        end;
      
      List.iter (fun cc ->
          cc.nconnections_last_second <- 0;
          cc.allow_accept := true
      ) !connections_controlers;
      nconnections_last_second := 0
  );
  
  set_before_select_hook (fun _ ->
      List.iter (fun cc ->
          let old_value = !(cc.allow_accept) in
          cc.allow_accept := cc.accept_connections 
            !nconnections_last_second cc.nconnections_last_second;
          if !verbose_bandwidth > 2 && !(cc.allow_accept) <> old_value then
            lprintf "[BW3 %6d] %20s: stop accepting connections\n" (last_time ()) cc.cc_name
      ) !connections_controlers
  )
