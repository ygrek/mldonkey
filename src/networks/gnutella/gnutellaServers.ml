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

open CommonSwarming
open Printf2
open Md4
open CommonOptions
open CommonSearch
open CommonServer
open CommonComplexOptions
open CommonFile
open BasicSocket
open TcpBufferedSocket

open CommonTypes
open CommonGlobals
open Options
open GnutellaTypes
open GnutellaGlobals
open GnutellaOptions
open GnutellaProtocol
open GnutellaComplexOptions

module DG = CommonGlobals
module DO = CommonOptions
  
module GnutellaHandler = struct
    let init s sock gconn =
      (if s.server_gnutella2 then
          Gnutella2Handler.init 
        else
          Gnutella1Handler.init) s sock gconn
  end
  
let gnutella_proto = "GNUTELLA/"
let gnutella_proto_len = String.length gnutella_proto

let retry_and_fake = false
  
let rec server_parse_header with_accept retry_fake s gconn sock header =
(*   if !verbose_msg_servers then  LittleEndian.dump_ascii header;   *)
  try
    if not (String2.starts_with header gnutella_proto) then 
      failwith "Reply is not in gnutella protocol";
    let space_pos = String.index header ' ' in
    let proto = String.sub header gnutella_proto_len (
        space_pos - gnutella_proto_len) in
    let code = String.sub header (space_pos+1) 3 in
    let lines = Http_client.split_header header in
    let headers =        
      match lines with
        [] -> []
      | line :: headers ->
          let headers = Http_client.cut_headers headers in
          if !verbose_msg_servers then begin
              lprintf "HEADER: %s\n" line;
              List.iter (fun (header, (value,_)) ->
                  lprintf "   %s = %s\n" header value;
              ) headers;
              lprintf "\n\n"
            end; 
          headers          
    in
    
    let ultra_peer = ref false in
    let gnutella2 = ref false in
    let deflate = ref false in
    let keep_headers = ref [] in
    let content_type = ref "" in
    let x_ultrapeers = ref [] in
    List.iter (fun (header, (value, key)) ->
        try
          match (String.lowercase header) with
          
          | "user-agent" ->  
              s.server_agent <- value;
              keep_headers := (key, value) :: !keep_headers
          
          | "remote-ip" -> 
              last_high_id := Ip.of_string value;

(* Be careful with x-try when we will limit the number of ultra_peers *)
(*          | "x-try" *)
          | "x-try-ultrapeers" ->
              List.iter (fun s ->
                  try
                    let len = String.length s in
(*                    lprintf "NEW ULTRAPEER %s\n" s; *)
                    let pos = String.index s ':' in
                    let ip = String.sub s 0 pos in
                    let port = try
                        let space = String.index_from s pos ' ' in
                        String.sub s (pos+1) (space - pos -1)
                      with _ -> 
                          String.sub s (pos+1) (len - pos - 1) in
                    let ip = Ip.of_string ip in
                    let port = int_of_string port in
(*                    lprintf "ADDING UP %s:%d\n" (Ip.to_string ip) port;  *)
                    x_ultrapeers :=  (ip,port, true) :: !x_ultrapeers;
                  with _ -> ()
              ) (String2.split_simplify value ',')
          
          | "x-try" ->
              List.iter (fun s ->
                  try
                    let len = String.length s in
(*                    lprintf "NEW ULTRAPEER %s\n" s; *)
                    let pos = String.index s ':' in
                    let ip = String.sub s 0 pos in
                    let port = try
                        let space = String.index_from s pos ' ' in
                        String.sub s (pos+1) (space - pos -1)
                      with _ -> 
                          String.sub s (pos+1) (len - pos - 1) in
                    let ip = Ip.of_string ip in
                    let port = int_of_string port in
(*                    lprintf "ADDING UP %s:%d\n" (Ip.to_string ip) port;  *)
                    x_ultrapeers :=  (ip,port, false) :: !x_ultrapeers;
                  
                  with _ -> ()
              ) (String2.split_simplify value ',')
          
          | "accept" -> ()
          | "accept-encoding" -> ()
          | "x-ultrapeer-loaded" -> ()
          
          | "content-type" ->
              List.iter (fun s ->
                  content_type := s;
                  lprintf "Content-Type: %s\n" s;
                  match s with 
                    "application/x-gnutella2" -> gnutella2 := true
                  | _ -> ()
              ) (String2.split_simplify value ',')
          
          | "x-ultrapeer" ->
              if String.lowercase value = "true" then ultra_peer := true
          
          | "content-encoding" -> 
              if value = "deflate" then deflate := true
          
          | "listen-ip"
          | "x-my-address"
            -> (* value = ip:port *) ()
          | "servent-id" -> (* servent in hexa *) ()
          | "x-ultrapeer-needed" -> (* true/false *) ()
          | "fp-auth-challenge" -> (* used by BearShare for auth *) ()
          | "machine" -> () (* unknown usage *)
(* Swapper *)
          | "x-live-since" -> ()
(* Gtk-gnutella : try and try-ultrapeer on several lines *)
          | "x-token" -> ()
(* Gnucleus/MorpheusOS/MyNapster *)
          | "uptime" -> (*  0D 21H 01M *) ()

(* These ones should be kept *)
          | "x-query-routing"    (* current 0.1 *) 
          | "pong-caching"    (* version = 0.1 *) 
          | "ggep"    (* current is 0.5 *) 
          | "vendor-message"    (* current is 0.1 *) 
          | "bye-packet"    (* current is 0.1 *) 

(* LimeWire *)
          | "x-dynamic-querying"    (* current is 0.1 *) 
          | "x-version"    
          | "x-max-ttl"    (* often 4 *) 
          | "x-degree"    (* often 15 *)  
          | "x-ext-probes"    (* current is 0.1 *) 
          | "x-ultrapeer-query-routing"    (* current is 0.1 *) 
(* BearShare *)
          | "hops-flow"    
          | "bearchat"    
          | "x-guess" ->
              s.server_query_key <- GuessSupport
          | "x-leaf-max" -> 
              keep_headers := (key, value) :: !keep_headers
          
          | _ -> 
              if !verbose_unknown_messages then begin
                  lprintf "Unknown Option in HEADER:\n";
                  List.iter (fun (h, (v,_)) ->
                      if h = header then
                        lprintf ">>>>>>>   %s = %s\n" h v
                      else
                        lprintf "   %s = %s\n" h v;
                  ) headers;
                  lprintf "\n\n"
                end
        with _ -> ()
    ) headers;
    List.iter (fun (ip,port,ultrapeer) ->
        let queue = if !gnutella2 then 2 else 1 in
        let h = new_host ip port ultrapeer queue in
        ()
    ) !x_ultrapeers;    
    
    if not !ultra_peer then 
      failwith "DISCONNECT: Not an Ultrapeer";
    if code <> "200" then begin
        if retry_fake then begin
            Gnutella.disconnect_from_server s;
            connect_server with_accept false s !keep_headers
          end else
          failwith  (Printf.sprintf "Bad return code [%s]" code)
      
      end else
    if proto < "0.6" then
      failwith (Printf.sprintf "Bad protocol [%s]" proto)
    else
    if not (!gnutella2 || !!enable_gnutella1) then
      failwith "Protocol Gnutella2 not supported"
    else
    let msg = 
      let buf = Buffer.create 100 in
      Buffer.add_string buf "GNUTELLA/0.6 200 OK\r\n";
      if !content_type <> "" then
        Printf.bprintf  buf "Content-Type: %s\r\n"
          (if !gnutella2 then
            "application/x-gnutella2" else
            !content_type);
      Buffer.add_string buf "X-Ultrapeer: False\r\n";
(* Contribute: sending gnutella 1 or 2 ultrapeers to other peers
      Buffer.add_string "X-Try-Ultrapeers: ...\r\n"; *)
      if !deflate then
        Printf.bprintf buf "Content-Encoding: deflate\r\n";
      Buffer.add_string buf "\r\n";
      Buffer.contents buf
    in
    if !verbose_msg_servers then
      lprintf "CONNECT REQUEST: %s\n" (String.escaped msg); 
    write_string sock msg;

(*
    if not retry_fake then
      lprintf "******** CONNECTED BY FAKING ********\n";
*)
    
    set_rtimeout sock DG.half_day;
    set_server_state s (Connected (-1));
    s.server_connected <- int32_time ();
    server_must_update (as_server s.server_server);
    
    s.server_gnutella2 <-  !gnutella2;
    connected_servers := s :: !connected_servers;
    GnutellaHandler.init s sock gconn;
    Gnutella.recover_files_from_server s;    
  
  with
  | e -> 
      if !verbose_msg_servers then
        lprintf "DISCONNECT WITH EXCEPTION %s\n" (Printexc2.to_string e); 
      Gnutella.disconnect_from_server s

and connect_server with_accept retry_fake s headers =  
  match s.server_sock with
    ConnectionWaiting -> ()
  | ConnectionAborted -> s.server_sock <- ConnectionWaiting
  | Connection _ -> ()
  | NoConnection -> 
      incr nservers;
      s.server_sock <- ConnectionWaiting;
      add_pending_connection (fun _ ->
          decr nservers;
          match s.server_sock with
            ConnectionAborted -> s.server_sock <- NoConnection;
          | Connection _ | NoConnection -> ()
          | ConnectionWaiting ->
              try
                if not (Ip.valid s.server_host.host_ip) then
                  failwith "Invalid IP for server\n";
                let ip = s.server_host.host_ip in
                let port = s.server_host.host_port in
(*        if !verbose_msg_servers then begin
            lprintf "CONNECT TO %s:%d\n" (Ip.to_string ip) port;
end;  *)
                
                let sock = connect "gnutella to server"
                    (Ip.to_inet_addr ip) port
                    (fun sock event -> 
                      match event with
                        BASIC_EVENT (RTIMEOUT|LTIMEOUT) -> 
(*                  lprintf "RTIMEOUT\n"; *)
                          Gnutella.disconnect_from_server s
                      | _ -> ()
                  ) in
                TcpBufferedSocket.set_read_controler sock download_control;
                TcpBufferedSocket.set_write_controler sock upload_control;
                
                set_server_state s Connecting;
                s.server_sock <- Connection sock;
                incr nservers;
                set_gnutella_sock sock !verbose_msg_servers
                  (HttpHeader (server_parse_header true retry_fake s)
                
                );
                set_closer sock (fun _ error -> 
(*            lprintf "CLOSER %s\n" error; *)
                    Gnutella.disconnect_from_server s);
                set_rtimeout sock !!server_connection_timeout;
                
                let s = 
                  let buf = Buffer.create 100 in
(* Start by Gnutella2 headers *)
                  Buffer.add_string buf "GNUTELLA CONNECT/0.6\r\n";
                  Printf.bprintf buf "Listen-IP: %s:%d\r\n"
                    (Ip.to_string (client_ip (Connection sock))) !!client_port;
                  Printf.bprintf buf "Remote-IP: %s\r\n"
                    (Ip.to_string s.server_host.host_ip);
                  
                  if headers = [] then begin
                      Printf.bprintf buf "User-Agent: %s\r\n" user_agent;
                      if with_accept then
                        Printf.bprintf buf "Accept: %s%s\r\n"
                          (if !!enable_gnutella1 then "application/x-gnutella-packets,"
                          else "")
                        (if !!enable_gnutella2 then "application/x-gnutella2"
                          else "application/x-edonkey"); 
(* Contribute:  Packet compression is not yet supported...
Printf.bprintf buf "Accept-Encoding: deflate\r\n"; *)
(* Other Gnutella headers *)          
                      Printf.bprintf buf "X-Max-TTL: 4\r\n";
                      Printf.bprintf buf "Vendor-Message: 0.1\r\n";
                      Printf.bprintf buf "X-Query-Routing: 0.1\r\n";
(* We add these two headers for Limewire, we are not completely sure we
  can comply with, as we are not ultrapeers... *)
                      Printf.bprintf buf "X-Dynamic-Querying: 0.1\r\n";
                      Printf.bprintf buf "X-Ultrapeer-Query-Routing: 0.1\r\n";
(* Guess support will be enabled soon *)
                      Printf.bprintf buf "X-Guess: 0.1\r\n";
                      Printf.bprintf buf "Vendor-Message: 0.1\r\n";
(* We don't forward Pongs at all, so we have this header... *)
                      Printf.bprintf buf "Pong-Caching: 0.1\r\n";
                      Printf.bprintf buf "GGEP: 0.5\r\n"; 
(* We never forward messages, so all our messages have hops = 0 *)
                      Printf.bprintf buf "Hops-Flow: 0.1\r\n"; 
(* Don't really know what this header means, so we just put the number
of ultrapeers we want to connect to *)
                      Printf.bprintf buf "X-Degree: %d\r\n" !!max_ultrapeers; 
(*              Printf.bprintf buf "X-Guess: 0.1\r\n";  *)
                    end else begin
(*              lprintf "****** Retry and fake **********\n"; *)
                      List.iter (fun (key, value) ->
                          Printf.bprintf buf "%s: %s\r\n" key value
                      ) headers;
                    end;
                  Printf.bprintf buf "X-My-Address: %s:%d\r\n"
                    (Ip.to_string (client_ip (Connection sock))) !!client_port;
                  Printf.bprintf buf "X-Ultrapeer: False\r\n";
                  Printf.bprintf buf "X-Ultrapeer-Needed: True\r\n";
(* Finish with headers *)
                  Buffer.add_string buf "\r\n";
                  Buffer.contents buf
                in
                if !verbose_msg_servers then
                  lprintf "SENDING %s\n" (String.escaped s); 
                write_string sock s;
              with _ ->
                  Gnutella.disconnect_from_server s
      )
      
let rec find_ultrapeer queue =
  let (h,next) = Fifo.head queue in
  if next > last_time () then begin
(*      lprintf "not ready: %d s\n" (next - last_time ()); *)
      raise Not_found;
    end;
  ignore (queue_take queue);    
  try
    h.host_ip, h.host_port,
    
    match h.host_kind with
      1 -> if not !!enable_gnutella1 then raise Not_found; false
    | 2 -> if not !!enable_gnutella2 then raise Not_found; true
    | _ -> 
        not !!enable_gnutella1
with _ -> find_ultrapeer queue
          
let try_connect_ultrapeer () =
  let (ip,port, with_accept) = 
    try
      find_ultrapeer ultrapeers_waiting_queue
    with _ ->
(*        lprintf "not in waiting\n"; *)
        try
          find_ultrapeer ultrapeers_recent_queue
        with _ ->
(*            lprintf "not in recent\n"; *)
            try
              find_ultrapeer ultrapeers_old_queue
            with _ ->
(*                lprintf "not in old\n"; *)
                if !!enable_gnutella2 then Gnutella2Redirector.connect ();
                if not !redirector_connected then begin
                    if !!enable_gnutella1 then Gnutella1Redirector.connect ()
                  end;
                find_ultrapeer peers_waiting_queue
  in
  let s = new_server ip port in
  connect_server (with_accept || s.server_gnutella2) retry_and_fake s []

(*
MLdonkey:
=========
GNUTELLA CONNECT/0.6
Listen-IP: 81.100.86.143:6346
Remote-IP: 207.5.221.193
User-Agent: MLDonkey 2.4-rc6
X-Query-Routing: 0.1
Pong-Caching: 0.1
GGEP: 0.5
X-My-Address: 81.100.86.143:6346
X-Ultrapeer: False
X-Ultrapeer-Needed: True
  
Bearshare:
==========
GNUTELLA CONNECT/0.6
X-Ultrapeer: False                                    
User-Agent: BearShare 4.2.5
Machine: 1,13,510,1,1196             ??
Pong-Caching: 0.1
X-Query-Routing: 0.1
Hops-Flow: 1.0                       ??
Listen-IP: 0.0.0.0:6346
Remote-IP: 24.117.189.175
GGEP: 0.5
BearChat: 1.0                        ??
FP-Auth-Challenge: YZCOLR2IFSF7WMVLYY54IS4Y5SQY52PM  ??

Limewire:
========  
GNUTELLA CONNECT/0.6 
X-Max-TTL: 4
X-Dynamic-Querying: 0.1              ??
X-Version: 2.9.8                     ??
X-Query-Routing: 0.1 
User-Agent: LimeWire/2.9.8 
Vendor-Message: 0.1 
X-Ultrapeer-Query-Routing: 0.1       ??
GGEP: 0.5 
Listen-IP: 192.168.0.2:6346 
X-Guess: 0.1                         
X-Ultrapeer: False 
X-Degree: 15                         
Remote-IP: 207.5.238.35 

  *)
  
let connect_servers () =
  (*
  lprintf "connect_servers %d %d\n" !nservers !!max_ultrapeers; 
  *)
  for i = !nservers to !!max_ultrapeers - 1 do
    try_connect_ultrapeer ()
  done    

let get_file_from_source c file =
  if connection_can_try c.client_connection_control then begin
      connection_try c.client_connection_control;
      match c.client_user.user_kind with
        Indirect_location ("", uid) ->
          lprintf "++++++ ASKING FOR PUSH +++++++++\n";   

(* do as if connection failed. If it connects, connection will be set to OK *)
          connection_failed c.client_connection_control;
(*          let module P = Push in
          let t = PushReq {
              P.guid = uid;
              P.ip = DO.client_ip None;
              P.port = !!client_port;
              P.index = (match 
                 with
                  FileByIndex (index,_) -> index
                | _ -> assert false)
            } in
          let p = new_packet t in *)
          List.iter (fun s ->
              Gnutella.server_send_push s uid   
                (find_download file c.client_downloads).download_uri
          ) !connected_servers
      | _ ->
          GnutellaClients.connect_client c
    end
    
let download_file (r : result) =
  let file = new_file (Md4.random ()) 
    r.result_name r.result_size r.result_uids in
  lprintf "DOWNLOAD FILE %s\n" file.file_name; 
  if not (List.memq file !current_files) then begin
      current_files := file :: !current_files;
    end;
  List.iter (fun (user, index) ->
      let c = new_client user.user_kind in
      add_download file c index;
      get_file_from_source c file;
  ) r.result_sources;
  ()

let ask_for_files () =
  List.iter (fun file ->
      List.iter (fun c ->
          get_file_from_source c file
      ) file.file_clients
  ) !current_files;
  ()
    
let disconnect_server s =
  match s.server_sock with
  | Connection sock -> close sock "user disconnect"
  | ConnectionWaiting -> s.server_sock <- ConnectionAborted
  | _ -> ()
      
let _ =
(*  server_ops.op_server_connect <- connect_server; *)
  server_ops.op_server_disconnect <- disconnect_server;
(*
(*  server_ops.op_server_query_users <- (fun s -> *)
      match s.server_sock with
        None -> ()
      | Some sock ->
          server_send sock (GetNickListReq)
  );
(*  server_ops.op_server_users <- (fun s -> *)
      List2.tail_map (fun u -> as_user u.user_user) s.server_users
);
  *)
  server_ops.op_server_remove <- (fun s ->
      disconnect_server s;
      server_remove s
  )
  
  
(* gnutella2: use shareaza redirector:
http://cache.shareaza.com/cache.aspx?get=1&hostfile=1&net=gnutella2

Format:
h|10.111.2.6:6346|942
h|81.50.66.76:6346|752
h|147.134.33.137:1076|348
h|64.247.95.19:6346|931
u|http://www20.brinkster.com/dgc2/lynnx.asp


  *)
  
  
let udp_handler p =
  let (ip,port) = match p.UdpSocket.addr with
    | Unix.ADDR_INET(ip, port) -> Ip.of_inet_addr ip, port
    | _ -> raise Not_found
  in
  let buf = p.UdpSocket.content in
  let len = String.length buf in
  if !!enable_gnutella2 && len > 3 && String.sub buf 0 3 = "GND" then
    try
      Gnutella2Handler.udp_packet_handler ip port
        (Gnutella2.parse_udp_packet ip port buf)
    with Gnutella2.AckPacket -> ()
    
       
let send_pings () =
  List.iter (fun s ->
      Gnutella.server_send_ping s;
      match s.server_query_key with
      | NoUdpSupport -> 
          Gnutella.server_send_qkr s           
      | _ -> ()
  ) !connected_servers;
  Fifo.iter (fun (h,_) ->
      match h.host_server with
      | None -> () | Some s ->
          match s.server_query_key with
          | NoUdpSupport ->
              Gnutella.server_send_qkr s 
          | _ -> ()
  ) ultrapeers_recent_queue

  
