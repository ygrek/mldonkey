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

module PacketSet = Set.Make (struct
      type t = int * udp_packet
      let compare (t1,p1) (t2,p2) = 
        compare (t1, String.length p1.content,p1) (t2, String.length p2.content,p2)
    end)


  
type t = {
    mutable sock : BasicSocket.t;
    mutable rlist : udp_packet list;
    mutable wlist : PacketSet.t;
    mutable event_handler : handler;
    mutable write_controler : bandwidth_controler option;
  }

and bandwidth_controler = {
    mutable sockets : t list;
    mutable remaining_bytes : int;
    mutable total_bytes : int;
    mutable allow_io : bool ref;
    mutable count : int;
    mutable base_time : int;
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
  if PacketSet.is_empty t.wlist then
   (try f t with _ -> ())
    
let set_reader t f =
  set_handler t READ_DONE f;
  match t.rlist with
    [] -> ()
  | _ -> (try f t with _ -> ())

let sock t = t.sock
let closed t = closed t.sock
let close t = 
  begin
    match t.write_controler with
      None -> ()
    | Some bc ->
        bc.sockets <- List2.removeq t bc.sockets
  end;
  close t.sock

let print_addr addr =
  begin
    match addr with
      Unix.ADDR_INET(ip, port) ->
        Printf.printf "ADDR_INET (%s, %d)" (Unix.string_of_inet_addr ip) port
    | Unix.ADDR_UNIX s ->
        Printf.printf "ADDR_UNIX (%s)" s;
  end;
  print_newline ()

let max_delayed_send = 30
  
let write t s pos len addr =
  if not (closed t) then 
    match t.write_controler with
      None ->
        if not (PacketSet.is_empty t.wlist) then
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
                t.wlist <- PacketSet.add  (0, {
                    content = String.sub s  pos len;
                    addr = addr;
                  }) t.wlist;
                must_write sock true;
            | e ->
                Printf.printf "Exception %s in sendto"
                  (Printexc.to_string e);
                print_newline ();
                print_addr addr;
                raise e
          end
        else begin
            t.wlist <- PacketSet.add (0, {
                content = String.sub s  pos len;
              addr = addr;
              })  t.wlist;
            must_write t.sock true;
          end
    | Some bc ->
        begin
          t.wlist <- PacketSet.add (bc.base_time + max_delayed_send, {
              content = String.sub s  pos len;
              addr = addr;
            })  t.wlist;
          must_write t.sock true;
        end
    
let dummy_sock = Obj.magic 0

let buf = String.create 66000

let rec iter_write_no_bc t sock = 
    let (time,p) = PacketSet.min_elt t.wlist in
    t.wlist <- PacketSet.remove (time,p) t.wlist;
    let len = String.length p.content in
    begin try
        ignore (
          Unix.sendto (fd sock) p.content 0 len  [] p.addr);
        udp_uploaded_bytes := Int64.add !udp_uploaded_bytes (Int64.of_int len);
      with
        Unix.Unix_error (Unix.EWOULDBLOCK, _, _) as e -> raise e
      | e ->
          Printf.printf "Exception %s in sendto next"
            (Printexc.to_string e);
          print_newline ();
    end;
    iter_write_no_bc t sock
    
let iter_write_no_bc t sock = 
  try
    iter_write_no_bc t sock 
  with
    Unix.Unix_error (Unix.EWOULDBLOCK, _, _) as e -> 
      must_write t.sock true

let rec iter_write t sock bc = 
  if bc.total_bytes = 0 || bc.remaining_bytes > 0 then
    let _ = () in
    let (time,p) = PacketSet.min_elt t.wlist in
    t.wlist <- PacketSet.remove (time,p) t.wlist;
    if time < bc.base_time then begin
        if !debug then begin
            Printf.printf "[UDP DROPPED]"; flush stdout;
          end;
      iter_write t sock bc
      end else
    let len = String.length p.content in
    begin try
        
        ignore (
          Unix.sendto (fd sock) p.content 0 len  [] p.addr);
        udp_uploaded_bytes := Int64.add !udp_uploaded_bytes (Int64.of_int len);
        bc.remaining_bytes <- bc.remaining_bytes - len;
      with
        Unix.Unix_error (Unix.EWOULDBLOCK, _, _) as e -> raise e
      | e ->
          Printf.printf "Exception %s in sendto next"
            (Printexc.to_string e);
          print_newline ();
    end;
    iter_write t sock bc
  else
    bc.allow_io := false
    
let iter_write t sock bc = 
  try
    iter_write t sock bc    
  with
    Unix.Unix_error (Unix.EWOULDBLOCK, _, _) as e -> 
      must_write sock true
      
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
        try
          match t.write_controler with
            None ->
              iter_write_no_bc t sock
          | Some bc ->
              iter_write t sock bc
        with Not_found ->
            must_write t.sock false
      end;
      if not (closed t) then begin
          t.event_handler t CAN_REFILL;
          if PacketSet.is_empty t.wlist then begin
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
      wlist = PacketSet.empty;
      sock = dummy_sock;
      event_handler = handler;
      write_controler = None;
    } in
  let sock = BasicSocket.create "udp_socket" fd (udp_handler t) in
  prevent_close sock;
  t.sock <- sock;
  t
  
let create_sendonly () =
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_DGRAM 0 in
  Unix.setsockopt fd Unix.SO_REUSEADDR true;
  let t = {
      rlist = [];
      wlist = PacketSet.empty;
      sock = dummy_sock;
      event_handler = (fun _ _ -> ());
      write_controler = None;      
    } in
  let sock = BasicSocket.create "udp_handler_sendonly" fd (udp_handler t) in
  prevent_close sock;
  t.sock <- sock;
  t
    
let can_write t =
  PacketSet.is_empty t.wlist

let read_packets t f =
  List.iter (fun p ->
      try f p with _ -> ()
  ) t.rlist;
  t.rlist <- []
  
let set_write_controler s c =  
  s.write_controler <- Some c;
  c.sockets <- s :: c.sockets;
  set_allow_write s.sock c.allow_io

let new_bandwidth_controler tcp_bc =
  let udp_bc = {
      sockets = [];
      remaining_bytes = 0;
      total_bytes = 0;
      allow_io = ref false;
      count = 0;
      base_time = 0;
    } in
  let udp_user total n =
    if !BasicSocket.debug then  begin
      Printf.printf "udp_user %d" n; print_newline ();
      end;
    udp_bc.base_time <- udp_bc.base_time + 1;
    if udp_bc.count = 0 then begin
        udp_bc.count <- 10;
        udp_bc.remaining_bytes <- 0;
      end;
    udp_bc.count <- udp_bc.count - 1;
    udp_bc.total_bytes <- total;
    udp_bc.remaining_bytes <- udp_bc.remaining_bytes + n;
    if total <> 0 && udp_bc.remaining_bytes > total then
      udp_bc.remaining_bytes <- total;
    udp_bc.allow_io := udp_bc.remaining_bytes > 0;
  in
  TcpBufferedSocket.set_remaining_bytes_user tcp_bc udp_user;
  udp_bc
  
let remaining_bytes bc = 
  if bc.total_bytes = 0 then 1000000 else bc.remaining_bytes
  
let use_remaining_bytes bc n =
  bc.remaining_bytes <- bc.remaining_bytes - n
  
  