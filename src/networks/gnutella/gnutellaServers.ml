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

open Queues
open Printf2
open Md4
open Options

open BasicSocket
open TcpBufferedSocket
  
open CommonDownloads
open CommonOptions
open CommonSearch
open CommonServer
open CommonComplexOptions
open CommonFile
open CommonTypes
open CommonGlobals
open CommonHosts

open GnutellaNetwork
open GnutellaTypes
open GnutellaGlobals
open GnutellaOptions
open GnutellaProtocol
open GnutellaComplexOptions  
open GnutellaProto

let exit = Exit

(*************************************************************************)
(*                                                                       *)
(*                         Constants                                     *)
(*                                                                       *)
(*************************************************************************)
    
let gnutella_proto = "GNUTELLA/"
let gnutella_proto_len = String.length gnutella_proto

  
(*************************************************************************)
(*                                                                       *)
(*                         extend_query                                  *)
(*                                                                       *)
(*************************************************************************)
    
(* For Guess hosts ... to be done *)      
let extend_query f = 
  let send (_,h) =
    try
      match h.host_server with
        None -> ()
      | Some s ->
          match s.server_query_key with 
            | UdpQueryKey _ ->
                f s
            | _ -> ()
    with _ -> ()
  in
  Queue.iter send active_udp_queue
  
(*************************************************************************)
(*                                                                       *)
(*                         send_query                                    *)
(*                                                                       *)
(*************************************************************************)
  
let send_query ss =
  match ss.search_search with
    UserSearch (uid, words, xml_query) ->
      let f s =
        server_send_query ss.search_uid words xml_query s.server_sock s in
      List.iter f !connected_servers;
      extend_query f
  | _ -> failwith "send_query: not implemented"      
  
(*************************************************************************)
(*                                                                       *)
(*                         recover_file                                  *)
(*                                                                       *)
(*************************************************************************)
  
let really_recover_file file =
  
  List.iter (fun fuid ->
      try
        List.iter (fun s ->
            match s.search_search with
              FileUidSearch (ss,uid) when uid = fuid -> raise exit
            | _ -> ()
        ) file.file_searches;
        
        let s = {
            search_search = FileUidSearch (file, fuid);
            search_hosts = Intset.empty;
            search_uid = Md4.random ();          
          }  in
        
        Hashtbl.add searches_by_uid s.search_uid s;
        file.file_searches <- s :: file.file_searches
      with _ -> ()
  ) file.file_uids;
  
  let f s = server_recover_file file s.server_sock s in
  List.iter f !connected_servers;
  extend_query f
  
(*************************************************************************)
(*                                                                       *)
(*                         send_pings                                    *)
(*                                                                       *)
(*************************************************************************)
         
let send_pings () =
  if !new_shared_words then update_shared_words ();
  List.iter (fun s ->      
      server_send_ping s.server_sock s;
      GnutellaProto.send_qrt_sequence s false;
      
      match s.server_query_key with
      | NoUdpSupport -> 
          host_send_qkr s.server_host           
      | _ -> 
          lprintf "Udp Support present\n";
          ()
  ) !connected_servers;
  new_shared_words := false;
  Queue.iter (fun (_,h) ->
      match h.host_server with
      | None -> () | Some s -> ()
  ) active_udp_queue
  
(*************************************************************************)
(*                                                                       *)
(*                         find_ultrapeer                                *)
(*                                                                       *)
(*************************************************************************)

let rec find_ultrapeer queue =
  let (next,h) = Queue.head queue in
  if next > last_time () then begin
(*      lprintf "not ready: %d s\n" (next - last_time ()); *)
      raise Not_found;
    end;
  ignore (H.host_queue_take queue);    
  try
    h, true  
  with _ -> find_ultrapeer queue
      
  
(*************************************************************************)
(*                                                                       *)
(*                         try_connect_ultrapeer                         *)
(*                                                                       *)
(*************************************************************************)
      
let try_connect_ultrapeer connect =
  let (h, with_accept) = 
    try
      find_ultrapeer ultrapeers_waiting_queue
    with _ ->
(*        lprintf "not in waiting\n"; *)
(*                lprintf "not in old\n"; *)
            GnutellaRedirector.connect ();
            find_ultrapeer peers_waiting_queue
  in
  connect h
  
(*************************************************************************)
(*                                                                       *)
(*                         connect_servers                               *)
(*                                                                       *)
(*************************************************************************)
  
let connect_servers connect =
(*
  lprintf "connect_servers %d %d\n" !nservers !!max_ultrapeers; 
*)
  (if !!max_ultrapeers > List.length !connected_servers then
      try
        let to_connect = 3 * (!!max_ultrapeers - !nservers) in
        for i = 1 to to_connect do
          try_connect_ultrapeer connect
        done    
      with _ -> ());
  (try 
      for i = 0 to 3 do
        let h = H.host_queue_take waiting_udp_queue in
(*        lprintf "waiting_udp_queue\n"; *)
        if (
            match h.host_server with
              None -> true
            | Some s ->
                match s.server_query_key with
                  UdpQueryKey _ -> false
                | _ -> true
          ) then begin
(*            lprintf "host_send_qkr...\n"; *)
            H.set_request h Udp_Connect;
            host_send_qkr h
          end
      done
    with _ -> ())
  
(*************************************************************************)
(*                                                                       *)
(*                         server_parse_headers                          *)
(*                                                                       *)
(*************************************************************************)
  
let server_parse_headers first_line headers =
  
  if !verbose_unknown_messages then begin
      let unknown_header = ref false in
      List.iter (fun (header, _) ->
          unknown_header := !unknown_header || not (List.mem header GnutellaProto.known_supernode_headers)
      ) headers;
      if !unknown_header then begin
          lprintf "Gnutella DEVEL: Supernode Header contains unknown fields\n";
          lprintf "    %s\n" first_line;
          List.iter (fun (header, (value,header2)) ->
              lprintf "    [%s] = [%s](%s)\n" header value header2;
          ) headers;
          lprintf "Gnutella DEVEL: end of header\n";        
        end;
    end;
  
  let ultrapeer = ref false in
  let ultrapeer_needed = ref false in
  let gnutella2 = ref false in
  let accept_deflate = ref false in
  let content_deflate = ref false in
  let keep_headers = ref [] in
  let content_type = ref "" in
  let x_ultrapeers = ref [] in
  let user_agent = ref "" in
  let query_key = ref NoUdpSupport in
  List.iter (fun (header, (value, key)) ->
      try
        match (String.lowercase header) with
        
        | "user-agent" ->  
            user_agent := value;
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
                  let ip = Ip.addr_of_string ip in
                  let port = int_of_string port in
(*                    lprintf "ADDING UP %s:%d\n" (Ip.to_string ip) port;  *)
                  x_ultrapeers :=  (ip,port, Ultrapeer) :: !x_ultrapeers;
                with e -> 
                    lprintf "Could not parse x-try-ultrapeers (%s):\n"
                      (Printexc2.to_string e);
                    AnyEndian.dump s
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
                  let ip = Ip.addr_of_string ip in
                  let port = int_of_string port in
(*                    lprintf "ADDING UP %s:%d\n" (Ip.to_string ip) port;  *)
                  x_ultrapeers :=  (ip,port, Peer) :: !x_ultrapeers;
                
                with e -> 
                    lprintf "Could not parse x-try (%s):\n"
                      (Printexc2.to_string e);
                    AnyEndian.dump s
            ) (String2.split_simplify value ',')
        
        | "accept" -> ()
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
            if String.lowercase value = "true" then ultrapeer := true
        
        | "content-encoding" -> 
            if value = "deflate" then content_deflate := true
        | "accept-encoding" -> 
            if value = "deflate" then accept_deflate := true
        
        | "listen-ip"
        | "x-my-address"
          -> (* value = ip:port *) ()
        | "servent-id" -> (* servent in hexa *) ()
        | "x-ultrapeer-needed" -> 
            if String.lowercase value = "true" then ultrapeer_needed := true
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
            query_key := GuessSupport
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
  if !gnutella2 <> GnutellaProto.gnutella2_needed then
    failwith "Protocol not supported";
  List.iter (fun (ip,port,ultrapeer) ->
      try
        ignore (H.new_host ip port ultrapeer)
      with e ->
          if !verbose_msg_servers then begin
              lprintf "Exception %s while in H.new_host x_ultrapeers\n"
                (Printexc2.to_string e)
            end
  ) !x_ultrapeers;    
  
  {
    hsrpl_content_deflate = !content_deflate;
    hsrpl_accept_deflate = !accept_deflate;
    hsrpl_ultrapeer = !ultrapeer;
    hsrpl_ultrapeer_needed = !ultrapeer_needed;
    hsrpl_agent = !user_agent;
    hsrpl_query_key = !query_key;
    hsrpl_content_type = !content_type;
    hsrpl_gnutella2 = !gnutella2;
  }
  
(*************************************************************************)
(*                                                                       *)
(*                         execute_handshake                             *)
(*                                                                       *)
(*************************************************************************)
  
let execute_handshake s gconn sock (first_line, headers) =
  try
    
    H.connected s.server_host;
(* The reply should be  "GNUTELLA/0.6 200 OK" *)
    let space_pos = String.index first_line ' ' in
    let slash_pos = String.index first_line '/' in
    let proto = String.sub first_line (slash_pos+1) (space_pos - slash_pos -1) in
    let code = String.sub first_line (space_pos+1) 3 in
        
    let h = server_parse_headers first_line headers in
    
    if proto <> "0.6" then
      failwith (Printf.sprintf "Bad protocol [%s]" proto);

    if not h.hsrpl_ultrapeer then
      failwith "DISCONNECT: Not an Ultrapeer";
    s.server_agent <- h.hsrpl_agent;
    s.server_query_key <- h.hsrpl_query_key;
    server_must_update (as_server s.server_server);
    
    if code <> "200" then begin
        s.server_connected <- int64_time ();
        failwith  (Printf.sprintf "Bad return code [%s]" code)      
      end;
      
    let headers = [
        ("X-Ultrapeer", "False") ;
(* Contribute: sending gnutella 1 or 2 ultrapeers to other peers
Buffer.add_string "X-Try-Ultrapeers: ...\r\n"; *)        
      ] in
    let headers = 
      if h.hsrpl_content_type <> "" then
        ("Content-Type",
          if h.hsrpl_gnutella2 then
            "application/x-gnutella2" else
            h.hsrpl_content_type) :: headers
      else headers
    in
    let headers =
      if h.hsrpl_content_deflate then
        ("Content-Encoding", "deflate") :: headers
      else headers
    in
    let headers =
      if h.hsrpl_accept_deflate then
        ("Accept-Encoding", "deflate") :: headers
      else headers
    in

    let msg = make_http_header "GNUTELLA/0.6 200 OK" headers in
    if !verbose_msg_servers then
      lprintf "CONNECT REQUEST: %s\n" (String.escaped msg); 
    write_string sock msg;
    
    if h.hsrpl_content_deflate then deflate_connection sock;
    
    set_rtimeout sock CommonGlobals.half_day;
    set_server_state s (Connected (-1));
    s.server_connected <- int64_time ();    
    GnutellaHandler.init s sock gconn;
    ()
  with e ->      
      if !verbose_msg_servers then
        lprintf "DISCONNECT WITH EXCEPTION %s\n" (Printexc2.to_string e); 
      disconnect_from_server s (Closed_for_exception e)
  
(*************************************************************************)
(*                                                                       *)
(*                         make_handshake_request_headers                *)
(*                                                                       *)
(*************************************************************************)

let make_handshake_request_headers req =
  let headers = [
(* Other Gnutella headers *)          
      ("User-Agent", user_agent) ;
      ("X-Max-TTL", "4") ;
      ("Vendor-Message", "0.1") ;
      ("X-Query-Routing", "0.1") ;
(* We add these two headers for Limewire, we are not completely sure we
can comply with, as we are not ultrapeers... *)

(***********************************************
                        ("X-Dynamic-Querying", "0.1") ;
                        ("X-Ultrapeer-Query-Routing", "0.1") ;

(* Guess support will be enabled soon *)
                        ("X-Guess", "0.1") ;
                        ("Vendor-Message", "0.1") ;
(* We don't forward Pongs at all, so we have this header... *)
                        ("Pong-Caching", "0.1") ;
                        ("Bye-Packet", "0.1") ;
                        ("GGEP", "0.5") ;
(* We never forward messages, so all our messages have hops = 0 *)
                        ("Hops-Flow", "0.1") ;
(* Don't really know what this header means, so we just put the number
of ultrapeers we want to connect to *)
                        ("X-Degree", string_of_int !!g1_max_ultrapeers) ;
*****************************************************)
      ("X-Ultrapeer", 
        if req.hsreq_ultrapeer then "True" else "False") ;
      ("X-Ultrapeer-Needed", 
        if req.hsreq_ultrapeer_needed then "True" else "False") ;
      ("Listen-IP", req.hsreq_local_address) ;
      ("Remote-IP", req.hsreq_remote_address) ;        
      ("X-My-Address", req.hsreq_local_address);      
    ]
  in
  let headers = 
    if GnutellaNetwork.has_accept then
      ("Accept", GnutellaNetwork.accept_header) :: headers
    else headers
  in
  let headers =
    if req.hsreq_accept_deflate then ("Accept-Encoding", "deflate") :: headers
    else headers
  in  
  let headers =
    if req.hsreq_content_deflate then ("Content-Encoding", "deflate") :: headers
    else headers
  in  
  headers
  
(*************************************************************************)
(*                                                                       *)
(*                         connect_server                                *)
(*                                                                       *)
(*************************************************************************)
  
let connect_server h =  
  let s = match h.host_server with
      None -> 
        let s = new_server h.host_addr h.host_port in
        h.host_server <- Some s;
        s
    | Some s -> s
  in
  match s.server_sock with
  | NoConnection -> 
      incr nservers;
      
      GnutellaProto.server_send_ping s.server_sock s;
      
      let token =
        add_pending_connection connection_manager (fun token ->
            decr nservers;
            try
              let ip = Ip.ip_of_addr s.server_host.host_addr in
              if not (Ip.valid ip ) then
                failwith "Invalid IP for server\n";
              let port = s.server_host.host_port in
(*        if !verbose_msg_servers then begin
            lprintf "CONNECT TO %s:%d\n" (Ip.to_string ip) port;
end;  *)
              H.set_request h Tcp_Connect;
              H.try_connect h;
(*                h.host_tcp_request <- last_time (); *)
              let sock = connect token "gnutella to server"
                  (Ip.to_inet_addr ip) port
                  (fun sock event -> 
                    match event with
                      BASIC_EVENT RTIMEOUT -> 
                        disconnect_from_server s Closed_for_timeout
                    | BASIC_EVENT LTIMEOUT ->
                        disconnect_from_server s Closed_for_lifetime

                    | _ -> ()
                ) in
              TcpBufferedSocket.set_read_controler sock download_control;
              TcpBufferedSocket.set_write_controler sock upload_control;
              
              set_server_state s Connecting;
              s.server_sock <- Connection sock;
              incr nservers;
              set_gnutella_sock sock !verbose_msg_servers
                (HttpReader (gnutella_proto_len,
                  [gnutella_proto, execute_handshake s],
                  GnutellaFunctions.default_handler)
              
              );
              set_closer sock (fun _ error -> 
(*            lprintf "CLOSER %s\n" error; *)
                  disconnect_from_server s error);
              set_rtimeout sock !!server_connection_timeout;
              
              let req = {
                  hsreq_accept_deflate = !!deflate_connections;
                  hsreq_content_deflate = false;
                  hsreq_ultrapeer_needed = true;
                  hsreq_ultrapeer = false;
                  hsreq_remote_address = 
                  Ip.string_of_addr s.server_host.host_addr;
                  hsreq_local_address = Printf.sprintf "%s:%d"
                    (Ip.to_string (client_ip (Connection sock))) 
                  !!client_port
                }
              in  
              let s = make_http_header
                  "GNUTELLA CONNECT/0.6"
                  (make_handshake_request_headers req)
              in

                if !verbose_msg_servers then
                  lprintf "SENDING %s\n" (String.escaped s);
                write_string sock s;
              with e ->
                  disconnect_from_server s
                    (Closed_for_exception e)
        )
      in
      s.server_sock <- ConnectionWaiting token
  | _ -> ()
  
(*************************************************************************)
(*                                                                       *)
(*                         get_file_from_source                          *)
(*                                                                       *)
(*************************************************************************)
      
let get_file_from_source c file =
  try
    if connection_can_try c.client_connection_control then begin
        connection_try c.client_connection_control;
        match c.client_user.user_kind with
          Indirect_location ("", uid) ->
            lprintf "++++++ ASKING FOR PUSH +++++++++\n";   

(* do as if connection failed. If it connects, connection will be set to OK *)
            connection_failed c.client_connection_control;
            
            let uri = (find_download file c.client_downloads).download_uri in
            List.iter (fun s ->
                GnutellaProto.server_send_push s uid uri
            ) !connected_servers;
        | _ ->
            GnutellaClients.connect_client c
      end
  with e ->
      lprintf "get_file_from_source: exception %s\n" (Printexc2.to_string e)
      
  
(*************************************************************************)
(*                                                                       *)
(*                         download_file                                 *)
(*                                                                       *)
(*************************************************************************)
    
let really_download_file (r : result_info) =
  lprintf "download_file\n";
  let file_temp = match r.result_uids with
      [] -> assert false
    | uid :: _ -> Uid.to_string uid in
  let file = new_file file_temp
    (List.hd r.result_names) r.result_size r.result_uids in
  lprintf "DOWNLOAD FILE %s\n" file.file_name; 
  if not (List.memq file !current_files) then begin
      current_files := file :: !current_files;
    end;
  (try
      let sources = Hashtbl.find result_sources r.result_num in
      List.iter (fun ((user, index),_) ->
          let c = new_client user.user_kind in
          add_download file c index;
          get_file_from_source c file;
      ) !sources;
    with _ -> ());
  file

      (*

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
*)
  
(*************************************************************************)
(*                                                                       *)
(*                         disconnect_server                             *)
(*                                                                       *)
(*************************************************************************)
  
let disconnect_server s r =
  match s.server_sock with
  | Connection sock -> close sock r
  | ConnectionWaiting token -> 
      cancel_token token;
      s.server_sock <- NoConnection
  | _ -> ()
  
(*************************************************************************)
(*                                                                       *)
(*                         ask_for_files                                 *)
(*                                                                       *)
(*************************************************************************)
    
let ask_for_files () =
  List.iter (fun file ->
      List.iter (fun c ->
          get_file_from_source c file
      ) file.file_clients
  ) !current_files;
  ()
  
(*************************************************************************)
(*                                                                       *)
(*                         manage_hosts                                  *)
(*                                                                       *)
(*************************************************************************)
  
let manage_hosts () = H.manage_hosts ()
  
(*************************************************************************)
(*                                                                       *)
(*                         MAIN                                          *)
(*                                                                       *)
(*************************************************************************)
     
let _ =
  server_ops.op_server_disconnect <- 
  (fun s -> disconnect_server s Closed_by_user);
  server_ops.op_server_remove <- (fun s ->
      disconnect_server s Closed_by_user; 
  )
  
