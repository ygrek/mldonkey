(* Copyright 2002 b8_bavard, b8_fee_carabine, INRIA *)
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
open TcpBufferedSocket
open Unix
open TcpBufferedSocket
open Mftp
open Options
open Mftp_comm
open ServerTypes  
open ServerOptions        
open ServerGlobals

let null_ip = Ip.of_int32 (Int32.of_int 0)

(*  
      let id = 
        try
          ignore (Hashtbl.find clients_by_id ip);
          incr client_counter;
          Ip.of_int32 (Int32.of_int !client_counter)
        with _ -> ip
        in
        Hashtbl.add clients_by_id id client;

*)

module P = Mftp_server
  
let reply_to_client_connection c =
  let loc = c.client_location in
  match c.client_sock with
    None -> ()
  | Some sock ->
      let id =
        try
          match c.client_kind with
            Firewalled_client -> raise Not_found
          | KnownLocation (ip,port) ->
              if Hashtbl.mem clients_by_id ip then raise Not_found;
              loc.loc_ip <- ip;
                loc.loc_port <- port;
                  ip
        with _ ->
            let id = Ip.of_int32 (Int32.of_int !client_counter) in
            loc.loc_ip <- id;
              loc.loc_port <- 0;
                incr client_counter;
            id
      in
      c.client_id <- id;
      Hashtbl.add clients_by_id id c;
      
      Printf.printf "SET ID"; print_newline ();
(* send ID back to client *)
      direct_server_send sock  (P.SetIDReq c.client_id);

(* send some messages *)
      List.iter (fun msg ->
          direct_server_send sock (P.MessageReq msg)) !!welcome_messages
      
  
let check_handler c port ok sock event = 
  if not !ok then
    match event with
      CAN_REFILL 
    | BASIC_EVENT CAN_WRITE ->
        ok := true;
        TcpBufferedSocket.close sock "connect ok";
        Printf.printf "CAN WRITE"; print_newline ();
        c.client_kind <- KnownLocation (c.client_conn_ip, port);
        reply_to_client_connection c 
    | BASIC_EVENT (CLOSED s) ->
        ok := true;
        Printf.printf "CLOSED %s" s; print_newline ();
        c.client_kind <- Firewalled_client;
        reply_to_client_connection c 
    | _ ->
        TcpBufferedSocket.close sock "connect ok";
        ok := true;
        Printf.printf "ERROR IN CONNECT"; print_newline ();
        c.client_kind <- Firewalled_client;
        reply_to_client_connection c
    
let check_client c port =
  let try_sock = TcpBufferedSocket.connect 
      (Ip.to_inet_addr c.client_conn_ip)
    port 
      (check_handler c port (ref false))
    (*server_msg_to_string*)
  in
  BasicSocket.set_wtimeout (TcpBufferedSocket.sock try_sock) 5.;
  Printf.printf "Checking client ID"; print_newline ();
  ()
  
let server_to_client c t sock =
  let module P = Mftp_server in
  Printf.printf "server_to_client"; print_newline ();
  P.print t;
  print_newline ();
  match t with
    P.ConnectReq t ->
      c.client_md4 <- t.P.Connect.md4;
      c.client_tags <- t.P.Connect.tags;      
      check_client c t.P.Connect.port;

  | P.AckIDReq _ -> ()
  
  | P.ShareReq list ->
      List.iter (fun t -> 
          ServerIndexer.add t;
          ServerLocate.add t.f_md4 c.client_location ) list;
  
  | P.QueryReq t ->
      let module R = P.Query in
      let q = ServerIndexer.query_to_query t in      
      
      
      let docs = ServerIndexer.find q in
      ()      
      
  | P.QueryLocationReq t ->
      let module R = P.QueryLocation in
      ()
      
  | _ -> ()
    

let remove_client c sock s = 
  Printf.printf "CLIENT DISCONNECTED"; print_newline ();
  Hashtbl.remove clients_by_id c.client_id;
  decr nconnected_clients
      
let handler t event =
  Printf.printf "CONNECTION"; print_newline ();
  match event with
    TcpServerSocket.CONNECTION (s, Unix.ADDR_INET (from_ip, from_port)) ->

      if !!max_clients >= !nconnected_clients then
        Unix.close s
      else
      let sock = TcpBufferedSocket.create s (fun _ _ -> ()) 
        (*server_msg_to_string*)
        in
      
      let ip = Ip.of_inet_addr from_ip in
      let client = {
          client_id = null_ip;
          client_conn_ip = ip;
          client_sock = Some sock;
          client_kind = Firewalled_client;
          client_files = [];
          client_tags = [];
          client_md4 = Md4.null;
          client_location = {
            loc_ip  = ip;
            loc_port = 0;
            loc_expired = 0.0;
          }
        } in

      incr nconnected_clients;
      TcpBufferedSocket.set_reader sock (
        Mftp_comm.server_handler (server_to_client client));
      TcpBufferedSocket.set_closer sock 
        (remove_client client)
  | _ -> 
      Printf.printf "???"; print_newline ();
      ()      
