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
open AnyEndian
open LittleEndian

type event = 
  WRITE_DONE
| CAN_REFILL
| READ_DONE 
| BASIC_EVENT of BasicSocket.event

type udp_packet = {
    udp_content: string;
    udp_addr: Unix.sockaddr;
(*
    val sendto : Unix.file_descr -> string -> int -> int ->
       Unix.msg_flag MSG_DONTWAITlist -> Unix.sockaddr -> int
*)
  }

let max_wlist_size = ref 100000
  
let local_sendto a b c d e f = 
  Unix.sendto a b c d e f
   

module PacketSet = Set.Make (struct
      type t = int * udp_packet
      let compare (t1,p1) (t2,p2) = 
        compare (t1, String.length p1.udp_content,p1) (t2, String.length p2.udp_content,p2)
    end)

type socks_proxy = {
    socks_proxy_address : string;
    socks_proxy_port : int;
    socks_proxy_user : string;
    socks_proxy_password : string;
  }
  
type t = {
    mutable port : int;
    mutable sock : BasicSocket.t;
    mutable rlist : udp_packet list;
    mutable wlist : PacketSet.t;
    mutable wlist_size : int;
    mutable event_handler : handler;
    mutable write_controler : bandwidth_controler option;
    mutable socks_proxy : socks_proxy option;
    mutable socks_local : (Ip.t * int) option;
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
    
let buf = Buffer.create 2000


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
        lprintf "ADDR_INET (%s, %d)" (Unix.string_of_inet_addr ip) port
    | Unix.ADDR_UNIX s ->
        lprintf "ADDR_UNIX (%s)" s;
  end;
  lprint_newline ()

let max_delayed_send = 30
  
let write t s ip port =
  if not (closed t) && t.wlist_size < !max_wlist_size then 
    let s, addr = match t.socks_local with
      None -> s, Unix.ADDR_INET(Ip.to_inet_addr ip, port) 
    | Some (ip, port) -> 
	Buffer.clear buf;
	buf_int8 buf 0;
	buf_int8 buf 0;
	buf_int8 buf 0;
	buf_int8 buf 1;
	buf_ip buf ip;
	buf_int16 buf port;
	Buffer.add_string buf s;
	Buffer.contents buf,  Unix.ADDR_INET(Ip.to_inet_addr ip, port) 
    in
    match t.write_controler with
      None ->
        if not (PacketSet.is_empty t.wlist) then
          begin
            let sock = sock t in
            try
              let len = String.length s in

              let code = local_sendto (fd sock) s 0 len [] addr in
              udp_uploaded_bytes := Int64.add !udp_uploaded_bytes (Int64.of_int len);
              ()
(*
lprintf "UDP sent [%s]" (String.escaped
(String.sub s pos len)); lprint_newline ();
*)
            with
              Unix.Unix_error ((Unix.EWOULDBLOCK | Unix.ENOBUFS), _, _) -> 
                t.wlist <- PacketSet.add  (0, {
                    udp_content = s ;
                    udp_addr = addr;
                  }) t.wlist;
                t.wlist_size <- t.wlist_size + String.length s;
                must_write sock true;
            | e ->
                lprintf "Exception %s in sendto"
                  (Printexc2.to_string e);
                lprint_newline ();
                print_addr addr;
                raise e
          end
        else begin
            t.wlist <- PacketSet.add (0, {
                udp_content = s ;
                udp_addr = addr;
              })  t.wlist;
            t.wlist_size <- t.wlist_size + String.length s;
            must_write t.sock true;
          end
    | Some bc ->

        begin
          t.wlist <- PacketSet.add (bc.base_time + max_delayed_send, {
              udp_content = s;
              udp_addr = addr;
            })  t.wlist;
          t.wlist_size <- t.wlist_size + String.length s;
          must_write t.sock true;
        end
    
let dummy_sock = Obj.magic 0

let read_buf = String.create 66000

let rec iter_write_no_bc t sock = 
  let (time,p) = PacketSet.min_elt t.wlist in
  t.wlist <- PacketSet.remove (time,p) t.wlist;
  t.wlist_size <- t.wlist_size - String.length p.udp_content;
  let len = String.length p.udp_content in
  begin try
      ignore (
        local_sendto (fd sock) p.udp_content 0 len  [] p.udp_addr);
      udp_uploaded_bytes := Int64.add !udp_uploaded_bytes (Int64.of_int len);
    with
      Unix.Unix_error ((Unix.EWOULDBLOCK | Unix.ENOBUFS), _, _) as e -> raise e
    | e ->
        lprintf "Exception %s in sendto next"
          (Printexc2.to_string e);
        lprint_newline ();
  end;
  iter_write_no_bc t sock
  
let iter_write_no_bc t sock = 
  try
    iter_write_no_bc t sock 
  with
    Unix.Unix_error ((Unix.EWOULDBLOCK | Unix.ENOBUFS), _, _) as e -> 
      must_write t.sock true

let rec iter_write t sock bc = 
  if bc.total_bytes = 0 || bc.remaining_bytes > 0 then
    let _ = () in
    let (time,p) = PacketSet.min_elt t.wlist in
    t.wlist <- PacketSet.remove (time,p) t.wlist;
    t.wlist_size <- t.wlist_size - String.length p.udp_content;
    if time < bc.base_time then begin
        if !debug then begin
            lprintf "[UDP DROPPED]"; 
          end;
      iter_write t sock bc
      end else
    let len = String.length p.udp_content in
    begin try
        

        ignore (
          local_sendto (fd sock) p.udp_content 0 len  [] p.udp_addr);
        udp_uploaded_bytes := Int64.add !udp_uploaded_bytes (Int64.of_int len);
        bc.remaining_bytes <- bc.remaining_bytes - (len + !
          TcpBufferedSocket.ip_packet_size) ;
      with
        Unix.Unix_error ((Unix.EWOULDBLOCK | Unix.ENOBUFS), _, _) as e -> raise e
      | e ->
          lprintf "Exception %s in sendto next"
            (Printexc2.to_string e);
          lprint_newline ();
    end;
    iter_write t sock bc
  else
    bc.allow_io := false
    
let iter_write t sock bc = 
  try
    iter_write t sock bc    
  with
    Unix.Unix_error ((Unix.EWOULDBLOCK | Unix.ENOBUFS), _, _) as e -> 
      must_write sock true
      
let udp_handler t sock event = 
  match event with
  | CAN_READ ->
      let (len, addr) = Unix.recvfrom (fd sock) read_buf 0 66000 [] in
      let s, addr = match t.socks_proxy with
	None -> String.sub read_buf 0 len, addr
      | Some _ ->
	  String.sub read_buf 10 (len-10), 
	  Unix.ADDR_INET(Ip.to_inet_addr (get_ip read_buf 4), get_int16 read_buf 8)
      in
      udp_downloaded_bytes := Int64.add !udp_downloaded_bytes (Int64.of_int len);
      t.rlist <- {
        udp_content = s;
        udp_addr = addr;
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
  Unix.bind fd (Unix.ADDR_INET ( (*Unix.inet_addr_any*) addr, port));
  let port = match Unix.getsockname fd with
      Unix.ADDR_INET (ip, port) -> port
    |_ -> port in
  let t = {
      rlist = [];
      wlist = PacketSet.empty;
      wlist_size = 0;
      sock = dummy_sock;
      event_handler = handler;
      write_controler = None;
      socks_proxy= None;
      socks_local= None;
      port = port;
    } in
  let sock = BasicSocket.create "udp_socket" fd (udp_handler t) in
  prevent_close sock;
  t.sock <- sock;
  t
  
let create_sendonly () = create Unix.inet_addr_any 0 (fun _ _ -> ())

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
(*
    if !BasicSocket.debug then  begin
      lprintf "udp_user %d/%d" n total; lprint_newline ();
      end; *)
    let n = if total = 0 then 100000 else n in
    udp_bc.base_time <- udp_bc.base_time + 1;
    if udp_bc.count = 0 then begin
        udp_bc.count <- 10;
        TcpBufferedSocket.set_lost_bytes tcp_bc udp_bc.remaining_bytes 
          udp_bc.base_time;
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

(*
let set_socks_proxy t ss =
  try

    Buffer.clear buf;
    let s = read_buf in

    t.socks_proxy <- Some ss;
    
    let fd = fd t.sock in
    Unix.clear_nonblock fd;
    let socks_ip = Ip.from_name ss.socks_proxy_address in
    let proxy_addr = Unix.ADDR_INET(Ip.to_inet_addr socks_ip, ss.socks_proxy_port) in

    buf_int8 buf 5;
    buf_int8 buf 1;
    let auth = ss.socks_proxy_user <> "" || ss.socks_proxy_password <> "" in
    buf_int8 buf (if auth then 2 else 0);

    let send_and_wait () =
      let s = Buffer.contents buf in
      Buffer.clear buf;
      assert (local_sendto fd s 0 (String.length s) [] proxy_addr > 0);
      
      match Unix.select [fd] [] [] 30. with
	[],_,_ -> failwith "[SOCKS] timeout"  
      | _ -> ()
    in
    send_and_wait ();
    assert (fst (Unix.recvfrom fd s 0 2 []) = 2);
    assert (s.[0] = '\005');
    if auth then begin
      assert (s.[1] = '\002');

      buf_int8 buf 1;
      buf_string8 buf ss.socks_proxy_user;
      buf_string8 buf ss.socks_proxy_password;    
      send_and_wait ();

      assert (fst (Unix.recvfrom fd s 0 2 []) = 2);
      assert (s.[0] = '\005');
    end;
    assert (s.[1] = '\000');

    buf_int8 buf 5;
    buf_int8 buf 3;
    buf_int8 buf 0;
    buf_int8 buf 1;
    buf_int buf 0;
    buf_int16 buf t.port;

    send_and_wait ();
    assert (fst (Unix.recvfrom fd s 0 10 []) = 10);
    assert (s.[0] = '\005');
    assert (s.[1] = '\000');

    let ip = get_ip s 4 in
    let port = get_int16 s 8 in
    t.socks_local <- Some (ip, port);

    MlUnix.set_nonblock fd;
  with e -> 
    lprintf "[SOCKS] proxy error prevent creation of UDP socket: %s" 
      (Printexc2.to_string e); lprint_newline ();
    close t "socks proxy error"; raise e
*)
