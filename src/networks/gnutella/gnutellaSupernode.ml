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
open GnutellaServers

(*
This is a simple ultrapeer implementation without Bloom filters. It can
be used only to test publication of files and upload between MLdonkey clients.
Maybe if Bloom filters are correctly implemented, it could be used as a normal
Gnutella ultrapeer...
*)
  
(*************************************************************************)
(*                                                                       *)
(*                         Types                                         *)
(*                                                                       *)
(*************************************************************************)
  
type node = {
    mutable node_sock : tcp_connection; 
    mutable node_packets : Md4.t list;
    mutable node_former_packets : Md4.t list Fifo.t;
    mutable node_guids : Md4.t list;
  }


(*************************************************************************)
(*                                                                       *)
(*                         Constants                                     *)
(*                                                                       *)
(*************************************************************************)

let gnutella_connect_proto = "GNUTELLA CONNECT/"

(*************************************************************************)
(*                                                                       *)
(*                         Global values                                 *)
(*                                                                       *)
(*************************************************************************)
  
let gnutella_enabled = ref None
let supernode_started = ref false
let supernode_sock = ref None
let nodes = ref []
let sent_packets = Hashtbl.create 1111
let node_guids = Hashtbl.create 1111
  
(*************************************************************************)
(*                                                                       *)
(*                         Options                                       *)
(*                                                                       *)
(*************************************************************************)
  
  
let supernode_port = define_option gnutella_section 
  ["supernode_port"]
  "(only for development tests)"  
    int_option 6348
  
let supernode_degree = define_option gnutella_section 
  ["supernode_degree"]
  "(only for development tests)"  
    int_option 20
  
(* Maximal number of packets received from one client per minute *)
let supernode_max_activity = define_option gnutella_section 
    ["supernode_max_activity"]
  "(only for development tests)"
    int_option 20

(*************************************************************************)
(*                                                                       *)
(*                         remove_packets                                *)
(*                                                                       *)
(*************************************************************************)

let remove_packets list =
  List.iter (fun uid -> Hashtbl.remove sent_packets uid) list
  
(*************************************************************************)
(*                                                                       *)
(*                         disconnect_node                               *)
(*                                                                       *)
(*************************************************************************)
  
let disconnect_node node s = 
  match node.node_sock with
    Connection sock ->
      lprintf "DISCONNECTED FROM NODE %s\n" (string_of_reason s);
      close sock s;
      node.node_sock <- NoConnection;
      nodes := List2.removeq node !nodes;
      remove_packets node.node_packets;
      Fifo.iter remove_packets node.node_former_packets;
      List.iter (fun guid ->
          Hashtbl.remove node_guids guid
      ) node.node_guids
  | ConnectionWaiting token -> assert false
  | NoConnection -> ()

(*************************************************************************)
(*                                                                       *)
(*                         node_send                                     *)
(*                                                                       *)
(*************************************************************************)

let node_send node t =
  match node.node_sock with
    NoConnection | ConnectionWaiting _ -> ()
  | Connection sock ->
      let m = server_msg_to_string t in
      write_string sock m

(*************************************************************************)
(*                                                                       *)
(*                         update_route                                  *)
(*                                                                       *)
(*************************************************************************)

let update_route p node = 
  if not (Hashtbl.mem sent_packets p.pkt_uid) then begin
      Hashtbl.add sent_packets p.pkt_uid node;
      node.node_packets <- p.pkt_uid :: node.node_packets;
      true
    end else false
      
(*************************************************************************)
(*                                                                       *)
(*                         node_send_ping                                *)
(*                                                                       *)
(*************************************************************************)

let node_send_ping node =
  node_send node 
    (new_packet (PingReq Ping.SimplePing))
      
(*************************************************************************)
(*                                                                       *)
(*                         supernode_to_node                             *)
(*                                                                       *)
(*************************************************************************)

let supernode_to_node node p sock =
  set_lifetime sock 600.;
  if !verbose_msg_servers then begin
      lprintf "RECEIVED supernode_to_node:\n";
      print p;
    end;
  match p.pkt_payload with
  | PingReq t -> 
      if p.pkt_hops <= 3 then
        node_send node {
          p with 
          pkt_hops = 0;
          pkt_type = PONG;
          pkt_payload = (
            let module P = Pong in
            PongReq {
              P.ip = (client_ip (Connection sock));
              P.port = !!supernode_port;
(* TODO: change this *)
              P.nfiles = 10;
              P.nkb = 10;
              P.ggep = [];
(*
              [
                Cobs.GGEP_GUE_guess 1; 
                Cobs.GGEP_VC_vendor ("MLDK", 2,4)]; *)
            });
        };
  
  | QueryReq t -> 
      if p.pkt_hops < 2 && update_route p node then
        let p = { p with pkt_hops = p.pkt_hops + 1 } in
        List.iter (fun n -> if n != node then node_send n p) !nodes
  
  | QueryReplyReq t -> 
      if p.pkt_hops < 2 then
        begin
          let guid = t.QueryReply.guid in
          if not (Hashtbl.mem node_guids guid) then begin
              node.node_guids <- guid :: node.node_guids;
              Hashtbl.add node_guids guid node
            end;
          
          try
            
            let n = Hashtbl.find sent_packets p.pkt_uid in
            node_send n { p with pkt_hops = p.pkt_hops + 1 }
          with Not_found -> 
              lprintf "Unable to forward QueryReplyReq\n"
        end
        
  | PushReq t -> 
      if p.pkt_hops < 2 then
        begin
          try
            let n = Hashtbl.find node_guids t.Push.guid in
            node_send n { p with pkt_hops = p.pkt_hops + 1 }
          with Not_found ->
              lprintf "Unable to forward PushReq\n"
        end
      
(* TODO: We don't care about all these ones currently *)
  | QrtPatchReq _ -> ()
  | QrtResetReq _ -> ()
(* We don't care about all these ones currently *)
  | PongReq t -> ()
  | ByeReq _ -> ()
  | UnknownReq _ -> ()
  | VendorReq _ -> ()
      
(*************************************************************************)
(*                                                                       *)
(*                         supernode_handler2                            *)
(*                                                                       *)
(*************************************************************************)
      
let supernode_handler2 node_ref gconn sock (first_line, headers) = 
    lprintf "Entering supernode_handler2\n";
  if List.length !nodes >= !!supernode_degree then begin
(* TODO somebody arrived before the final ack... close and forget this client *)
      close sock Closed_by_user;
      raise Exit
    end;

(* The reply should be  "GNUTELLA/0.6 200 OK" *)
  let space_pos = String.index first_line ' ' in
  let slash_pos = String.index first_line '/' in
  let proto = String.sub first_line (slash_pos+1) (space_pos - slash_pos -1) in
  let code = String.sub first_line (space_pos+1) 3 in
  
  let h = server_parse_headers first_line headers in
  
  if proto <> "0.6" then
    failwith (Printf.sprintf "Bad protocol [%s]" proto);
  if code <> "200" then
    failwith  (Printf.sprintf "Bad return code [%s]" code);
  
  let node = { 
      node_sock = Connection sock;
      node_packets = [];
      node_former_packets = Fifo.create ();
      node_guids = [];
    } in
(* No packets were sent during the last 3 minutes *)
  Fifo.put node.node_former_packets [];
  Fifo.put node.node_former_packets [];
  Fifo.put node.node_former_packets [];
  
  node_ref := Some node;
  nodes := node :: !nodes;
  
  
  TcpBufferedSocket.set_rtimeout sock 300.;
  if h.hsrpl_content_deflate then deflate_connection sock;
  node_send_ping node;
  node_send node 
    { (new_packet (VendorReq (Vendor.Supported
            [
            "BEAR", 4,1;
            "BEAR", 7,1;
            "GTKG", 7,1;
          ]
        ))) with pkt_ttl = 1; };
  gconn.gconn_handler <- Reader
    (gnutella_handler parse (supernode_to_node node))
  

(*************************************************************************)
(*                                                                       *)
(*                         supernode_handler1                            *)
(*                                                                       *)
(*************************************************************************)

let gnutella_proto = "GNUTELLA/"
let gnutella_proto_len = String.length gnutella_proto
  
let supernode_handler1 node gconn sock (first_line, headers) = 
  lprintf "Entering supernode_handler1\n";
  if List.length !nodes >= !!supernode_degree then begin
(* TODO hum... we should not close the socket like that, but send an error
  reply... *)
      close sock Closed_by_user;
      raise Exit
    end;
  try

(* The request should be  "GNUTELLA CONNECT/0.6" *)
    if first_line <> "GNUTELLA CONNECT/0.6" then
      failwith "Bad connection protocol";
    
    let h = server_parse_headers first_line headers in
    
    if h.hsrpl_accept_deflate then 
      lprintf "ACCEPT DEFLATE\n";
    
    let req = {
        hsreq_content_deflate = !!deflate_connections && h.hsrpl_accept_deflate;
        hsreq_accept_deflate = false;
        hsreq_ultrapeer_needed = false;
        hsreq_ultrapeer = true;
        hsreq_remote_address = 
        Ip.to_string (fst (peer_addr sock));
        hsreq_local_address = Printf.sprintf "%s:%d"
          (Ip.to_string (client_ip (Connection sock))) 
        !!client_port
      }
    in  
    let msg = make_http_header
        "GNUTELLA/0.6 200 OK"
        (make_handshake_request_headers req)
    in
    if !verbose_msg_servers then
      lprintf "SENDING %s\n" (String.escaped msg);
    
    write_string sock msg;
    set_gnutella_sock sock !verbose_msg_servers
      (HttpReader (gnutella_proto_len,
        [gnutella_proto, supernode_handler2 node],
        GnutellaFunctions.default_handler));
    ()    
  with e ->
      if !verbose_msg_servers then
        lprintf "DISCONNECT WITH EXCEPTION %s\n" (Printexc2.to_string e); 
      close sock (Closed_for_exception e)      

(*************************************************************************)
(*                                                                       *)
(*                         start_supernode                               *)
(*                                                                       *)
(*************************************************************************)
      
let gnutella_connect = "GNUTELLA CONNECT/"  
let gnutella_connect_len = String.length gnutella_connect
  
let start_supernode enabler =
  if not !supernode_started then begin
      supernode_started := true;
      
      try
        let sock = TcpServerSocket.create "gnutella supernode" 
            Unix.inet_addr_any
            !!supernode_port
            (fun sock event ->
              match event with
                TcpServerSocket.CONNECTION (s, 
                  Unix.ADDR_INET(from_ip, from_port)) ->
                  lprintf "CONNECTION RECEIVED FROM %s FOR SUPERNODE\n"
                    (Ip.to_string (Ip.of_inet_addr from_ip))
                  ; 
                  
                  lprintf "*********** CONNECTION ***********\n";
                  
                  let token = create_token connection_manager in
                  let sock = TcpBufferedSocket.create token
                      "gnutella client connection" s 
                      (fun sock event -> 
                        match event with
                          BASIC_EVENT RTIMEOUT -> close sock Closed_for_timeout
                        | BASIC_EVENT LTIMEOUT -> close sock Closed_for_lifetime
                        | _ -> ()
                    )
                  in
                  TcpBufferedSocket.set_read_controler sock download_control;
                  TcpBufferedSocket.set_write_controler sock upload_control;
                  
                  let node = ref None in
                  TcpBufferedSocket.set_closer sock (fun _ s ->
                      match !node with
                        Some node ->  disconnect_node node s
                      | None -> ()
                  );
                  TcpBufferedSocket.set_rtimeout sock 30.;
                  set_gnutella_sock sock !verbose_msg_servers
                    (HttpReader (gnutella_connect_len, 
                      [gnutella_connect , supernode_handler1 node],
                      GnutellaFunctions.default_handler));
              | _ -> ()
          ) in
        add_session_timer enabler 60. (fun _ ->
            List.iter (fun node ->
                
                if List.length node.node_packets > !!supernode_max_activity then
                  disconnect_node node (Closed_for_error "Too active")
                else begin
                    
                    node_send_ping node;
(* Forget packets older than 3 minutes *)
                    remove_packets (Fifo.take node.node_former_packets);
                    Fifo.put node.node_former_packets node.node_packets;
                    node.node_packets <- [];
                  end
            ) !nodes;
        );
        supernode_sock := Some sock;
        ()
      with e ->
          lprintf "Exception %s while initializing gnutella supernode\n" 
            (Printexc2.to_string e)
          
    end

(*************************************************************************)
(*                                                                       *)
(*                         stop_supernode                                *)
(*                                                                       *)
(*************************************************************************)

let stop_supernode () =
  if !supernode_started then begin
      supernode_started := false;
      
      (match !supernode_sock with None -> ()
        | Some sock -> 
            supernode_sock := None;
            TcpServerSocket.close sock Closed_by_user);      
      
    end

(*************************************************************************)
(*                                                                       *)
(*                         enable                                        *)
(*                                                                       *)
(*************************************************************************)
    
let enable enabler = 
  gnutella_enabled := Some enabler;
  if !!supernode_enabled then start_supernode enabler

(*************************************************************************)
(*                                                                       *)
(*                         disable                                       *)
(*                                                                       *)
(*************************************************************************)
    
let disable () = 
  gnutella_enabled := None;
  stop_supernode ()

(*************************************************************************)
(*                                                                       *)
(*                         MAIN                                          *)
(*                                                                       *)
(*************************************************************************)
  
let _ = 
  option_hook supernode_enabled (fun _ ->
      match !gnutella_enabled with
        None -> ()
      | Some enabler ->
            if !!supernode_enabled then start_supernode enabler
            else stop_supernode ()
  );
  plugin_enable_hooks := enable :: !plugin_enable_hooks;
  plugin_disable_hooks := disable :: !plugin_disable_hooks
  