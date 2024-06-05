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

open Int64ops
open Printf2
open BasicSocket
open AnyEndian
open LittleEndian

let log_prefix = "[udpSock]"

let lprintf_nl fmt =
  lprintf_nl2 log_prefix fmt

let lprintf_n fmt =
  lprintf2 log_prefix fmt

let exn_log name f x = 
  try 
    f x
  with e -> 
    lprintf_nl "%s : unexpected exn %s" name (Printexc2.to_string e)

type event = 
  WRITE_DONE
| CAN_REFILL
| READ_DONE 
| BASIC_EVENT of BasicSocket.event

type ping = {
    ping_ip : Ip.t;
    mutable ping_die_time : int;
    mutable ping_obsolete_time : int;
    mutable ping_time : float;
  }

let latencies = Hashtbl.create 2131
let pings_fifo = Fifo.create ()
let pings_hashtbl = Hashtbl.create 2131
  
let declare_ping ip = 
  
  try
    let ping = Hashtbl.find pings_hashtbl ip in
    ping.ping_obsolete_time <- 0; (* ping is void *)
    ping.ping_die_time <- last_time () + 50
  with _ ->
      let ping = {
          ping_ip = ip;
          ping_obsolete_time = last_time () + 50;
          ping_die_time = last_time () + 50;
          ping_time = current_time ();
        } in
      Hashtbl.add pings_hashtbl ip ping;
      Fifo.put pings_fifo (ping.ping_die_time, ping)
  
let declare_pong ip = 
    let ping = Hashtbl.find pings_hashtbl ip in
    if ping.ping_obsolete_time > last_time () then begin
        ping.ping_die_time <- 0;
        let time = ping.ping_time in
        let ip = ping.ping_ip in
        let delay = current_time () -. time in
        let delay = 1000. *. delay in
        let delay = int_of_float delay in
        let delay = if delay > 65000 then 65000 else delay in
        try
          let latency, samples = Hashtbl.find latencies ip in
          incr samples;
          if !latency > delay then latency := delay
        with _ -> 
            Hashtbl.add latencies ip (ref delay, ref 1)            
      end

let declare_pong = exn_log "declare_pong" declare_pong

type udp_packet = {
    udp_ping : bool;
    udp_content: bytes;
    udp_addr: Unix.sockaddr;
(*
    val sendto : Unix.file_descr -> string -> int -> int ->
       Unix.msg_flag MSG_DONTWAITlist -> Unix.sockaddr -> int
*)
  }

let max_wlist_size = ref 100000
  
let local_sendto sock p = 
  if p.udp_ping then begin
      match p.udp_addr with
        Unix.ADDR_INET(ip, _) ->
          let ip = Ip.of_inet_addr ip  in
          declare_ping ip
      | _ -> ()
  end;
  Unix.sendto sock p.udp_content 0 (Bytes.length p.udp_content) [] p.udp_addr
   

module PacketSet = Set.Make (struct
      type t = int * udp_packet
      let compare (t1,p1) (t2,p2) = 
        compare (t1, Bytes.length p1.udp_content,p1) (t2, Bytes.length p2.udp_content,p2)
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
    mutable tcp_bc : TcpBufferedSocket.bandwidth_controler;
  }
  
and handler = t -> event -> unit


let udp_uploaded_bytes = ref Int64.zero
let udp_downloaded_bytes = ref Int64.zero
    
let buf = Buffer.create 2000
let debug = ref false

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
    exn_log "immediate refill" f t

let set_reader t f =
  set_handler t READ_DONE f;
  match t.rlist with
    [] -> ()
  | _ -> exn_log "immediate read" f t

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
        lprintf_nl "ADDR_INET (%s, %d)" (Unix.string_of_inet_addr ip) port
    | Unix.ADDR_UNIX s ->
        lprintf_nl "ADDR_UNIX (%s)" s;
  end

let max_delayed_send = 1
  
let write t ping s ip port =
(*  lprintf_nl "UDP write to %s:%d" (Ip.to_string ip) port; *)
  if not (closed t) && t.wlist_size < !max_wlist_size then 
    let s, addr = match t.socks_local with
      None -> s, Unix.ADDR_INET(Ip.to_inet_addr ip, port) 
    | Some (ip, port) -> 
        Buffer.reset buf;
        buf_int8 buf 0;
        buf_int8 buf 0;
        buf_int8 buf 0;
        buf_int8 buf 1;
        buf_ip buf ip;
        buf_int16 buf port;
        Buffer.add_bytes buf s;
        Buffer.to_bytes buf,  Unix.ADDR_INET(Ip.to_inet_addr ip, port) 
    in
    match t.write_controler with
      None ->
        if not (PacketSet.is_empty t.wlist) then
          begin
            let sock = sock t in
            try
              let len = Bytes.length s in

              let _ =
                try
                  if ping then declare_ping ip;
                  ignore(Unix.sendto (fd sock) s 0 len [] addr);
                  if !verbose_bandwidth > 1 then begin
                      lprintf_nl "[BW2] direct send udp %d bytes (write)" len;
                    end;
                with e ->
                    lprintf_nl "Exception in sendto %s:%d" (Ip.to_string ip) port;
                    raise e
                    in
              udp_uploaded_bytes := !udp_uploaded_bytes ++ (Int64.of_int len);
              ()
(*
lprintf_nl "UDP sent [%s]" (String.escaped
(String.sub s pos len));
*)
            with
              Unix.Unix_error ((Unix.EWOULDBLOCK | Unix.ENOBUFS), _, _) -> 
                t.wlist <- PacketSet.add  (0, {
                    udp_ping = ping;
                    udp_content = s ;
                    udp_addr = addr;
                  }) t.wlist;
                t.wlist_size <- t.wlist_size + Bytes.length s;
                must_write sock true;
            | e ->
                lprintf_nl "Exception %s in sendto"
                  (Printexc2.to_string e);
                print_addr addr;
                raise e
          end
        else begin
            t.wlist <- PacketSet.add (0, {
                udp_ping = ping;
                udp_content = s ;
                udp_addr = addr;
              })  t.wlist;
            t.wlist_size <- t.wlist_size + Bytes.length s;
            must_write t.sock true;
          end
    | Some bc ->

        begin
          t.wlist <- PacketSet.add (bc.base_time + max_delayed_send, {
              udp_ping = ping;
              udp_content = s;
              udp_addr = addr;
            })  t.wlist;
          t.wlist_size <- t.wlist_size + Bytes.length s;
          must_write t.sock true;
        end
  else
    if !debug then begin
        lprintf_nl "UDP DROPPED in write";
    end
    
let dummy_sock = Obj.magic 0

let read_buf = String.create 66000

let rec iter_write_no_bc t sock = 
  let (time,p) = PacketSet.min_elt t.wlist in
  t.wlist <- PacketSet.remove (time,p) t.wlist;
  t.wlist_size <- t.wlist_size - Bytes.length p.udp_content;
  let len = Bytes.length p.udp_content in
  begin try
      ignore (local_sendto (fd sock) p);
      udp_uploaded_bytes := !udp_uploaded_bytes ++ (Int64.of_int len);
      if !verbose_bandwidth > 1 then begin
          lprintf_nl "[BW2] direct send udp %d bytes (iter_write_no_bc)" len;
      end
    with
      Unix.Unix_error ((Unix.EWOULDBLOCK | Unix.ENOBUFS), _, _) as e -> 
        lprintf_nl "Exception %s in sendto next" (Printexc2.to_string e);
        raise e
    | e ->
          if !debug then
            lprintf_nl "Exception %s in sendto next"
              (Printexc2.to_string e)
  end;
  iter_write_no_bc t sock
  
let iter_write_no_bc t sock = 
  try
    iter_write_no_bc t sock 
  with
    Unix.Unix_error ((Unix.EWOULDBLOCK | Unix.ENOBUFS), _, _) -> 
      must_write t.sock true

let rec iter_write t sock bc = 
  if bc.total_bytes = 0 || bc.remaining_bytes > 0 then
    let _ = () in
    let (time,p) = PacketSet.min_elt t.wlist in
    t.wlist <- PacketSet.remove (time,p) t.wlist;
    t.wlist_size <- t.wlist_size - Bytes.length p.udp_content;
    if time < bc.base_time then begin
        if !debug then begin
            lprintf_nl "UDP DROPPED in iter_write"; 
          end;
      iter_write t sock bc
      end else
    let len = Bytes.length p.udp_content in
    begin try
        ignore (local_sendto (fd sock) p);
        udp_uploaded_bytes := !udp_uploaded_bytes ++ (Int64.of_int len);
        bc.remaining_bytes <- bc.remaining_bytes - (len +
          !TcpBufferedSocket.ip_packet_size) ;
        TcpBufferedSocket.register_bytes (Some bc.tcp_bc) len;
        if !verbose_bandwidth > 1 then begin
            lprintf_nl "[BW2] bc send udp %d bytes" len;
          end;
      with
        Unix.Unix_error ((Unix.EWOULDBLOCK | Unix.ENOBUFS), _, _) as e -> 
          lprintf_nl "Exception %s in sendto next" (Printexc2.to_string e);
          raise e
      | e ->
          if !debug then
            lprintf_nl "Exception %s in sendto next"
              (Printexc2.to_string e)
    end;
    iter_write t sock bc
  else
    bc.allow_io := false
    
let iter_write t sock bc = 
  try
    iter_write t sock bc    
  with
    Unix.Unix_error ((Unix.EWOULDBLOCK | Unix.ENOBUFS), _, _) -> 
      must_write sock true
      
let udp_handler t sock event = 
  match event with
  | CAN_READ ->
      let (len, addr) = Unix.recvfrom (fd sock) read_buf 0 66000 [] in
      let s, addr = match t.socks_proxy with
        None -> Bytes.sub read_buf 0 len, addr
      | Some _ ->
          Bytes.sub read_buf 10 (len-10), 
          Unix.ADDR_INET(Ip.to_inet_addr (get_ip_bytes read_buf 4), get_int16_bytes read_buf 8)
      in
      udp_downloaded_bytes := !udp_downloaded_bytes ++ (Int64.of_int len);
      t.rlist <- {
        udp_content = s;
        udp_ping = false;
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
  List.iter (exn_log "read_packets" f) t.rlist;
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
      tcp_bc = tcp_bc;
    } in
  let udp_user total n =
    if !verbose_bandwidth > 0 then
      lprintf_nl "udp_user %d/%d" n total;
(*    let n = if total = 0 then 100000 else n in *)
    udp_bc.base_time <- udp_bc.base_time + 1;
    if udp_bc.count = 0 then begin
        udp_bc.count <- 10;
        TcpBufferedSocket.set_lost_bytes tcp_bc udp_bc.remaining_bytes 
          udp_bc.base_time;
      end;
    udp_bc.count <- udp_bc.count - 1;
    udp_bc.total_bytes <- total;
    udp_bc.remaining_bytes <- total / 2;
(*    udp_bc.remaining_bytes <- udp_bc.remaining_bytes + n; *)
    if total <> 0 && udp_bc.remaining_bytes > total then
      udp_bc.remaining_bytes <- total;
    udp_bc.allow_io := total = 0 || udp_bc.remaining_bytes > 0;
    if !verbose_bandwidth > 0 then
      lprintf_nl "udp_bc count:%d total_bytes:%d remaining_bytes:%d" 
      udp_bc.count udp_bc.total_bytes udp_bc.remaining_bytes;
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

    Buffer.reset buf;
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
      Buffer.reset buf;
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
    lprintf_nl "[SOCKS] proxy error prevent creation of UDP socket: %s" 
      (Printexc2.to_string e);
    close t "socks proxy error"; raise e
*)

  
let get_latencies verbose =
  let b = Buffer.create 300 in
  let counter = ref 0 in  
  Hashtbl.iter (fun ip (latency, samples) ->
      incr counter;
  ) latencies;
  LittleEndian.buf_int b !counter;
  Hashtbl.iter (fun ip (latency, samples) ->
      if !verbose then lprintf_nl "   Latency UDP: %s -> %d (%d samples)" (Ip.to_string ip) !latency !samples;
      LittleEndian.buf_ip b ip;
      LittleEndian.buf_int16 b !latency;
      LittleEndian.buf_int16 b !samples;
  ) latencies;
  Hashtbl.clear latencies;
  Buffer.contents b
      
let _ =
  Heap.add_memstat "udpSocket" (fun level buf ->
      Printf.bprintf buf "  %d latencies\n" (Hashtbl.length latencies);
      Printf.bprintf buf "  %d entries in pings_fifo\n" (Fifo.length pings_fifo);
      Printf.bprintf buf "  %d entries in pings_hashtbl\n" (Hashtbl.length pings_hashtbl);
  );

  add_infinite_timer 5. (fun _ ->
      try
        while true do
          let (die_time, ping) = Fifo.head pings_fifo in
          if die_time < last_time () then begin
              ignore (Fifo.take pings_fifo);
              if ping.ping_die_time > last_time () then
                Fifo.put pings_fifo (ping.ping_die_time, ping)
              else
                Hashtbl.remove pings_hashtbl ping.ping_ip
            end else raise Exit
        done
      with _ -> ()
  )

