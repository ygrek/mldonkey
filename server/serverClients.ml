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

open CommonTypes

open BasicSocket
open TcpBufferedSocket
open Unix
open DonkeyMftp
open Options
open Mftp_comm
open ServerTypes  
open ServerOptions        
open ServerGlobals
open ServerLog

module M = Mftp_server

  
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

let print_loc loc =  Printf.printf("localisation %s port %d valide to %f")
                     (Ip.to_string loc.loc_ip)
                     loc.loc_port
                     loc.loc_expired;
                     print_newline()

let print_files_shared files = 
        List.iter (fun md4 -> 
                Printf.printf(" file: %s") (Md4.to_string md4); 
                print_newline())
                files
                     
let rec print_client_stat c option =
        Printf.printf("Client %s ") (Ip.to_string c.client_id);
        match option with 
        'a' -> Printf.printf("not implémenté")
        | 'l' ->  print_loc c.client_location
        | 'f' -> print_files_shared c.client_files
        | _ -> print_client_stat c 'a'

let print table =
        try
           print_newline();
           Printf.printf("Liste des clients");
           print_newline();
           Hashtbl.iter (fun md4 c -> print_client_stat c 'l') table;
           Printf.printf("FIN liste des clients");
           print_newline();
        with _ -> Printf.printf " Liste des clients vide"; print_newline();

exception Already_use
        
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
              loc.loc_ip <- ip;
                loc.loc_port <- port;
                  if Hashtbl.mem clients_by_id ip then raise Already_use;
                    ip
        with Already_use ->
              (*Printf.printf ("IP deja utilisé comme identifiant");*)
                 let id = Ip.of_int32 (Int32.of_int !client_counter) in 
                     incr client_counter;
                        id
        |_ ->
            let id = Ip.of_int32 (Int32.of_int !client_counter) in
            loc.loc_ip <- id;
              loc.loc_port <- 0;
                incr client_counter;
                  id
      in
      (*Printf.printf ("Nouvel identifiant %s") (Ip.to_string id);
      print_newline();*)
      c.client_id <- id;
      Hashtbl.add clients_by_id id c;

      (*print clients_by_id;*)
      
      (*Printf.printf "SET ID"; print_newline ();*)
(* send ID back to client *)
      direct_server_send sock  (M.SetIDReq c.client_id)

(* send some messages 
      List.iter (fun msg ->
          direct_server_send sock (M.MessageReq msg)) !!welcome_messages*)
      
  
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
  let try_sock = TcpBufferedSocket.connect "server to client"
      (Ip.to_inet_addr c.client_conn_ip)
    port 
      (check_handler c port (ref false))
    (*server_msg_to_string*)
  in
  BasicSocket.set_wtimeout (TcpBufferedSocket.sock try_sock) 5.;
  Printf.printf "Checking client ID"; print_newline ();
  ()

(*send 200 results*)
let send_query_reply sock c =
  let list = ServerIndexer.get c.client_results 200 in
  (*if (!!save_log) then
     ServerLog.put_results list;*)
  if (!!save_log) then
          ServerLog.add_note  ("Number of Files:\n"^(string_of_int (List.length list))^"\n");
  direct_server_send sock (M.QueryReplyReq list);
  ()
  
(*remove in the location table all file share by a client*)   
let remove_md4_source c =
  let files_liste = c.client_files in
  let loc = {
            loc_ip=c.client_id;
            loc_port=c.client_location.loc_port;
            loc_expired=0.0
            } in
  List.iter (fun md4 -> ServerLocate.supp md4 loc)
  files_liste   
  

(* send nb clients and nb files on the server *)
let send_stat_to_clients timer =
        Hashtbl.iter ( fun k x ->
                 match x.client_sock with
                         None -> ()
                       | Some sock ->
                direct_server_send  sock (M.InfoReq
                (!nconnected_clients,!nshared_files))) clients_by_id

(*get A part of a listeof servers*)
let rec get_serverlist servers nb_servers =
  match servers with 
    [] -> []
    | hd :: tail ->if nb_servers = 0 then []
      else { M.ServerList.ip = hd.server_ip; M.ServerList.port = hd.server_port} :: get_serverlist tail (nb_servers-1)


(*message comming from a client*)
let server_to_client c t sock =
(*  Printf.printf "server_to_client"; print_newline ();
  M.print t;
  print_newline ();*)
  incr nb_tcp_req;
  if (!!save_log) then
     ServerLog.new_log_req c.client_location.loc_ip c.client_md4 t;
  (match t with
    M.ConnectReq t ->
      c.client_md4 <- t.M.Connect.md4;
      c.client_tags <- t.M.Connect.tags;      
      check_client c t.M.Connect.port
      
  | M.AckIDReq _ -> 
(* send messages to the client*) 
      List.iter (fun s -> direct_server_send sock (M.MessageReq s))
      !!welcome_messages;
(* send servers list *)
      direct_server_send sock (M.ServerListReq (get_serverlist !alive_servers 50));
			     
(* send info to client *)
      direct_server_send sock (M.ServerInfoReq 
          (let module SI = M.ServerInfo in          
          {
            SI.md4 = !!server_md4;
            SI.ip = !!server_ip;
            SI.port = !!server_port;
            SI.tags = [
              { tag_name = "name"; tag_value = String !!server_name };
              { tag_name = "description"; tag_value = String !!server_desc }
              ];
          }))
  
  | M.ShareReq list ->
      remove_md4_source c;                     
      List.iter (fun tmp ->
          ServerIndexer.add tmp;
          try 
           ServerLocate.add tmp.f_md4
           {loc_ip=c.client_id;loc_port=c.client_location.loc_port;loc_expired=c.client_location.loc_expired};
            c.client_files <- tmp.f_md4::c.client_files 
          with _ -> Printf.printf "To Much Files Shared\n"
                   
       ) list
       (*ServerLocate.print()*)
                           
  | M.QueryReq t ->
      let module R = M.Query in
      let q = ServerIndexer.query_to_query t in            
      let docs = ServerIndexer.find q in
(* send back QueryReplyReq *)
	
      c.client_results <- docs;
      send_query_reply sock c   
      
  | M.QueryMoreResultsReq ->
(* send back QueryReplyReq *)
       send_query_reply sock c
      
  | M.QueryIDReq t ->
(* send back
  QueryIDFailedReq if disconnected
  QueryIDReplyReq if connected
*)
      begin
        try
          let cc = Hashtbl.find clients_by_id t in
          match cc.client_kind, sock, c.client_kind, cc.client_sock with
          | KnownLocation (ip, port), sock, _, _
          | Firewalled_client, _, KnownLocation (ip, port), Some sock 
            ->
              let module QI = M.QueryIDReply in
              direct_server_send sock (M.QueryIDReplyReq {
                  QI.ip = ip;
                  QI.port = port;
                })
          | _ ->
              Printf.printf "QueryIDReq can't return reply"; 
              print_newline ();
              raise Not_found
        with Not_found ->
            direct_server_send sock (M.QueryIDFailedReq t)
         
      end;

(***************************************************************** TODO ***)
      
  | M.QueryLocationReq t ->
     begin
       try
      let module R = M.QueryLocationReply in
      let peer_list = ServerLocate.get t in
      (*M.QueryLocationReply.print peer_list;*)
(* send back QueryLocationReplyReq *)
       if (!!save_log) then
          ServerLog.add_note  ("Number of Location:\n"^(string_of_int (List.length peer_list.R.locs))^"\n");
      direct_server_send sock (M.QueryLocationReplyReq peer_list);  
      with Not_found -> ()
     end
      
  | M.QueryUsersReq t ->
(* send back QueryUsersReplyReq *)
      let clients_list = ref [] in
      Hashtbl.iter (fun k x ->
              clients_list := {
                    M.QueryUsersReply.md4 = x.client_md4;
                    M.QueryUsersReply.ip = x.client_location.loc_ip;                             
                    M.QueryUsersReply.port = x.client_location.loc_port;
                    M.QueryUsersReply.tags = [];
                 }:: !clients_list
      ) clients_by_id; 
      direct_server_send sock (M.QueryUsersReplyReq !clients_list)       
      
  | _ -> Printf.printf "UNKNOWN TCP REQ\n"; 
      ());
  if (!!save_log) then
     ServerLog.add_to_liste ();
   ()
      
(* every minute, send a InfoReq message *)

let remove_client c sock s = 
  (*Printf.printf ("CLIENT DISCONNECTED %s")
  (Ip.to_string c.client_id);
  print_newline ();*)
  if (!!save_log) then
    begin
      ServerLog.something_append  c.client_location.loc_ip c.client_md4 s;
      ServerLog.add_to_liste () 
    end;
  Hashtbl.remove clients_by_id c.client_id;
  remove_md4_source c;
  (*print clients_by_id;
  ServerLocate.print ();*)
  decr nconnected_clients
      
let handler t event =
  (*Printf.printf "CONNECTION"; print_newline ();*)
  match event with
    TcpServerSocket.CONNECTION (s, Unix.ADDR_INET (from_ip, from_port)) ->

      if !!max_clients <= !nconnected_clients then
	begin
	  Printf.printf "too much clients\n";
          Unix.close s
	end
      else
      let sock = TcpBufferedSocket.create "server client connection" s (fun _ _ -> ()) 
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
          };
          client_results = { docs = [||]; next_doc = 0; };
        } in

      incr nconnected_clients;
      TcpBufferedSocket.set_reader sock (
        Mftp_comm.cut_messages Mftp_server.parse (server_to_client client));
      TcpBufferedSocket.set_closer sock 
        (remove_client client)
  | _ -> 
      Printf.printf "???"; print_newline ();
      ()      
