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
  WRITE_DONE
| CAN_REFILL
| READ_DONE 
| BASIC_EVENT of BasicSocket.event

type udp_packet = {
    content: string;
    addr: Unix.sockaddr;
(*
    val sendto : Unix.file_descr -> string -> int -> int ->
       Unix.msg_flag MSG_DONTWAITlist -> Unix.sockaddr -> int
*)
  }
  
type t = {
    mutable sock : BasicSocket.t;
    mutable rlist : udp_packet list;
    mutable wlist : udp_packet list;
    mutable event_handler : handler;
  }
  
and handler = t -> event -> unit


let udp_uploaded_bytes = ref Int64.zero
let udp_downloaded_bytes = ref Int64.zero


let read t = 
  match t.rlist with
    [] -> raise Not_found
  | p :: l ->
      t.rlist <- l;
      p

let set_handler t event handler =
  let old_handler = t.event_handler in
  let handler t ev =
    if ev = event then
      handler t
    else
      old_handler t ev
  in
  t.event_handler <- handler

let set_refill t f =
  set_handler t CAN_REFILL f;
  match t.wlist with
    [] -> (try f t with _ -> ())
  | _ -> ()
    
let set_reader t f =
  set_handler t READ_DONE f;
  match t.rlist with
    [] -> ()
  | _ -> (try f t with _ -> ())

let sock t = t.sock
let closed t = closed t.sock
let close t = close t.sock
    
let write t s pos len addr =
  if not (closed t) then 
    match t.wlist with
      [] ->
        begin
          let sock = sock t in
          try
          

            let code = Unix.sendto (fd sock) s pos len [] addr in
            udp_uploaded_bytes := Int64.add !udp_uploaded_bytes (Int64.of_int len);
	    ()
            (*
            Printf.printf "UDP sent [%s]" (String.escaped
              (String.sub s pos len)); print_newline ();
*)
          with
            Unix.Unix_error (Unix.EWOULDBLOCK, _, _) -> 
              t.wlist <- {
                content = String.sub s  pos len;
                addr = addr;
              } :: t.wlist;
              must_write sock true;
          | e ->
              Printf.printf "Exception %s in sendto"
              (Printexc.to_string e);
              print_newline ();
              raise e
        end
    | _ -> 
        t.wlist <- {
          content = String.sub s  pos len;
          addr = addr;
        } :: t.wlist
      
let dummy_sock = Obj.magic 0

let buf = String.create 66000
  
let udp_handler t sock event = 
  match event with
  | CAN_READ ->
      let (len, addr) = Unix.recvfrom (fd sock) buf 0 66000 [] in
      udp_downloaded_bytes := Int64.add !udp_downloaded_bytes (Int64.of_int len);
      t.rlist <- {
        content = String.sub buf 0 len;
        addr = addr;
      } :: t.rlist;
      t.event_handler t READ_DONE
  
  | CAN_WRITE ->
      begin
        match t.wlist with
          [] -> ()
        | p :: l ->
            t.wlist <- l;
            try
              let len = String.length p.content in
              ignore (
                Unix.sendto (fd sock) p.content 0 len  [] p.addr);
              udp_uploaded_bytes := Int64.add !udp_uploaded_bytes (Int64.of_int len);
(*
  Printf.printf "UDP sent after [%s]" (String.escaped
p.content); print_newline ();
  *)
            with
              Unix.Unix_error (Unix.EWOULDBLOCK, _, _) -> 
                t.wlist <- p :: l
            | e ->
                Printf.printf "Exception %s in sendto next"
                  (Printexc.to_string e);
                print_newline ();
                raise e

      end;
      if not (closed t) then begin
          t.event_handler t CAN_REFILL;
          if t.wlist = [] then begin
              must_write t.sock false;
              t.event_handler t WRITE_DONE
            end
        end      
  | _ -> t.event_handler t (BASIC_EVENT event)

      
let create addr port handler =
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_DGRAM 0 in
  Unix.setsockopt fd Unix.SO_REUSEADDR true;
  Unix.bind fd (Unix.ADDR_INET ((*Unix.inet_addr_any*) addr, port));
  let t = {
      rlist = [];
      wlist = [];
      sock = dummy_sock;
      event_handler = handler;
    } in
  let sock = BasicSocket.create "udp_socket" fd (udp_handler t) in
  t.sock <- sock;
  t
  
let create_sendonly () =
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_DGRAM 0 in
  Unix.setsockopt fd Unix.SO_REUSEADDR true;
  let t = {
      rlist = [];
      wlist = [];
      sock = dummy_sock;
      event_handler = (fun _ _ -> ());
    } in
  let sock = BasicSocket.create "udp_handler_sendonly" fd (udp_handler t) in
  t.sock <- sock;
  t
    
let can_write t =
  t.wlist = []
  
