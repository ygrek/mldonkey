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
open ServerServer
open CommonClient
open DonkeyTypes
open CommonTypes
open DonkeyMftp
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
open ServerSubscriptions

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
		     try
		       let tags = ServerIndexer.get_def md4 in
			 DonkeyMftp.print_tags tags.DonkeyMftp.f_tags
		     with _ ->
		       Printf.printf " NO file def for %s\n" (Md4.to_string md4)
				    
		  ) files
          
let rec print_client_stat c option =
        Printf.printf("Client %s ") (Ip.to_string c.client_id);
        match option with 
            'a' ->  print_loc c.client_location;
	      print_files_shared c.client_files
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
           print_newline()
        with _ -> Printf.printf " Liste des clients vide"; print_newline()
	  
	  
let bprint_loc buf loc =  
  Printf.bprintf buf "localisation %s port %d valide to %f\n"
    (Ip.to_string loc.loc_ip)
    loc.loc_port
    loc.loc_expired;
  ()

let bprint_files_shared buf files = 
        List.iter (fun md4 -> 
		     try
		       let tags = ServerIndexer.get_def md4 in
			 DonkeyMftp.bprint_tags buf tags.DonkeyMftp.f_tags
		     with _ ->
		       Printf.bprintf buf " NO file def for %s\n" (Md4.to_string md4)
		  ) files
          
let rec bprint_client_stat buf c option =
        Printf.bprintf buf ("Client %s\n") (Ip.to_string c.client_id);
        match option with 
            "a" ->  bprint_loc buf c.client_location;
	      bprint_files_shared buf c.client_files
        | "l" ->  bprint_loc buf c.client_location
        | "f" -> bprint_files_shared buf c.client_files
        | _ -> begin 
	    bprint_loc buf c.client_location;
	    bprint_files_shared buf c.client_files
	      end

let bprint buf table arg=
  Printf.bprintf buf ("Liste des clients\n");
  Hashtbl.iter (fun md4 c -> 
		  match c with
		      LocalClient c ->
                        Printf.bprintf buf "LocalClient\n";
			bprint_client_stat buf c arg
		    | _ -> Printf.bprintf buf "RemoteClient\n"
	       ) table
      



exception Already_use
        
let get_client_id ip =
  if Hashtbl.mem clients_by_id ip then
    begin
      let find = ref true in
	while !find do
	  if Hashtbl.mem clients_by_id (Ip.of_int32 (Int32.of_int !client_counter)) then
	      find := false
	  else
	    begin 
	      incr client_counter;
	      if (!client_counter > 2000) then
		client_counter := 0
	    end
	done;
	(Ip.of_int32 (Int32.of_int !client_counter))  
    end
  else
    ip


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
      Hashtbl.add clients_by_id id (LocalClient c);

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
        (*Printf.printf "CAN WRITE"; print_newline ();*)
        c.client_kind <- KnownLocation (c.client_conn_ip, port);
        reply_to_client_connection c 
    | BASIC_EVENT (CLOSED s) ->
        ok := true;
        (*Printf.printf "CLOSED %s" s; print_newline ();*)
        c.client_kind <- Firewalled_client;
        reply_to_client_connection c 
    | _ ->
        TcpBufferedSocket.close sock "connect ok";
        ok := true;
        (*Printf.printf "ERROR IN CONNECT"; print_newline ();*)
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
  (*Printf.printf "Checking client ID"; print_newline ();*)
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
  try
    let files_liste = c.client_files in
    let loc = {
      loc_ip=c.client_id;
      loc_port=c.client_location.loc_port;
            loc_expired=0.0
    } in
      List.iter (fun md4 -> 
		     ServerLocate.supp md4 loc;
                     (* ServerServer.supp_source_to_notify c md4 *)
                ) files_liste 
  with _->
    Printf.printf "Exception during remove source\n"
  

(* send the number of clients and the number of files on the server *)
let send_stat_to_clients timer =
        Hashtbl.iter ( fun k x ->
			   match x with 
			   LocalClient x ->
			     (match x.client_sock with
                              None -> ()
			      | Some sock ->
                              direct_server_send  sock (M.InfoReq
                                (!nconnected_clients,!nshared_md4)))
                           | RemoteClient x ->
                              ()
                     ) clients_by_id
                          

let rec check_and_remove lst file =
  match lst with 
      [] -> raise Not_found
    | hd :: tl -> (if file.f_md4 = hd then
		     tl 
		   else
		     hd :: check_and_remove tl file;)
	
let rec print_liste lst = 
  match lst with 
      [] -> ()
    | hd :: tl -> Printf.printf " el: %s\n" (Md4.to_string hd);
	print_liste tl
	  
let rec print_liste_2 lst = 
   match lst with 
    [] -> ()
  | hd :: tl -> Printf.printf " el: %s\n" (Md4.to_string hd.f_md4);
      print_liste_2 tl

(*message comming from a client*)
let server_to_client c t sock =
  (*Printf.printf "server_to_client"; print_newline ();
  M.print t;
  print_newline ();*)
  incr nb_tcp_req_count;
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
        direct_server_send sock (M.ServerListReq (
            List.map (fun s ->
                { 
                  M.ServerList.ip = s.DonkeyTypes.server_ip; 
                  M.ServerList.port = s.DonkeyTypes.server_port
                }
            ) !serverList));

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
	ServerLocate.print();
        let tmp = c.client_files in
        let removed_files = ref tmp in
        let added_files = ref [] in
        let current_files = ref [] in

          List.iter ( fun file ->
			try
			  removed_files := check_and_remove !removed_files file; 
                          current_files := file.f_md4 :: !current_files;
			with Not_found ->
			  added_files := file :: !added_files
		    ) list;
	  
	  (*Printf.printf "remove file\n";
	  print_liste !removed_files;
	  Printf.printf "\nadd file\n";
	  print_liste_2 !added_files;
	  Printf.printf "\ncurrent file\n";
	  print_liste !current_files;
	  Printf.printf "\n";*)

          (* DEBUG *)
          Printf.printf "MANU DEBUG\n";
          List.iter (fun file -> print_tags file.f_tags) list;
          print_newline ();

          (* Naive: try to notify for each new document *)
          (* List.iter (fun file -> 
                       Subs.notify (Subs.cook_field_list file.f_tags) 
                       (Subs.cook_pred_list file.f_tags))
            list; *)

          c.client_files <- !removed_files;
          remove_md4_source c;           
          c.client_files <- !current_files;
          begin
          try
          List.iter (fun tmp ->
		       ServerIndexer.add tmp; 
			 ServerLocate.add tmp.f_md4
			   {loc_ip=c.client_id;loc_port=c.client_location.loc_port;loc_expired=c.client_location.loc_expired};
                         c.client_files <- tmp.f_md4 :: c.client_files;
                       if !!relais_cooperation_protocol then
                          begin
                            Printf.printf "Notif add to list\n";
                            (* ServerServer.add_source_to_notify c tmp.f_md4; *)
                          end
		    )  !added_files;
          with _ -> (*Printf.printf "To Much Files Shared\n"*)
		    direct_server_send sock (M.MessageReq "Too much files index in my server, so I can't index yours")
          end;
	  ServerLocate.print()
    
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
            match cc with 
              LocalClient cc ->
                (match cc.client_kind, sock, c.client_kind, cc.client_sock with
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
                      raise Not_found)
            | RemoteClient cc ->
                Printf.printf "RemoteClient QueryIDREQ no implemented"
          with Not_found ->
              direct_server_send sock (M.QueryIDFailedReq t)
        
        end;
    
    | M.QueryLocationReq t ->

(********** rewritten by Fabrice **********)

(* First of all: do as if it is a normal client *)
        
        begin
          try
            let module R = M.QueryLocationReply in
            let peer_list = ServerLocate.get t in
(*M.QueryLocationReply.print peer_list;*)
(* send back QueryLocationReplyReq *)
            if (!!save_log) then
              ServerLog.add_note  ("Number of Location:\n"^(string_of_int (List.length peer_list.R.locs))^"\n");
            direct_server_send sock (M.QueryLocationReplyReq peer_list);  
          with Not_found -> 
(* what does the normal server reply when there are no source for a file ? *)
              ()
        end;

(* Now, check if it is a mldonkey client *)
        
        begin
          if c.client_mldonkey = 0 && t = Md4.null then
(* c.client_mldonkey = 0 ==> First location query *)
            c.client_mldonkey <- 1 + c.client_mldonkey
          else
(* c.client_mldonkey = 1 ==> Might be mldonkey client *)
          if c.client_mldonkey = 1 && t = Md4.one then begin
              c.client_mldonkey <- 1 + c.client_mldonkey;
              direct_server_send sock (M.Mldonkey_MldonkeyUserReplyReq)
            end else
(* It is a classical client *)
          let mess = "For best performance, you'd better use a mldonkey client (http://www.freesoftware.fsf.org/mldonkey)" in
          c.client_mldonkey <- (-1);
          direct_server_send sock (M.MessageReq mess); 
        end
    
    | M.QueryUsersReq t ->
(* send back QueryUsersReplyReq *)
        let clients_list = ref [] in
        Hashtbl.iter (fun k x ->
            match x with 
              LocalClient x ->
                clients_list := {
                  M.QueryUsersReply.md4 = x.client_md4;
                  M.QueryUsersReply.ip = x.client_location.loc_ip;                             
                  M.QueryUsersReply.port = x.client_location.loc_port;
                  M.QueryUsersReply.tags = [];
                }:: !clients_list
            | RemoteClient x -> ()
        ) clients_by_id; 
        direct_server_send sock (M.QueryUsersReplyReq !clients_list)       

    | M.Mldonkey_SubscribeReq (num,t) ->
        (* Fabrice version
        let q = ServerIndexer.query_to_query t in            
        let notif = {
            notif_client = c;
            notif_num = num;
            notif_query = q;
            notif_docs = Intmap.empty;           
          } in
        Hashtbl.add ServerGlobals.notifications (c.client_md4, num) notif;
        c.client_subscriptions <- notif :: c.client_subscriptions;
        notif.notif_docs <- ServerIndexer.find_map q ;        
        let list = ref [] in
        Intmap.iter (fun _ num ->
            list := (Store.get store num) :: !list
        ) notif.notif_docs;
        direct_server_send sock (M.Mldonkey_NotificationReq (num, !list));
        *)
        let module S = Subs in
        let q = S.translate_query t in
          ignore(S.query_to_subs (c.client_md4, num) q S.Empty 
                   (fun _ -> Printf.printf "SUBS %d from %s MATCH\n" num (Md4.to_string c.client_md4)))
        
    | M.Mldonkey_CloseSubscribeReq num ->
        (* Fabrice version
        begin
          try
            let notif = Hashtbl.find ServerGlobals.notifications  (c.client_md4, num)
            in
            Hashtbl.remove ServerGlobals.notifications  (c.client_md4, num);
            c.client_subscriptions <- List2.removeq notif c.client_subscriptions
          with _ -> ()
        end
        *)
        let module S = Subs in
          S.remove_subs_id (c.client_md4, num)
            
    | _ -> Printf.printf "UNKNOWN TCP REQ\n"; 
      ());
  if (!!save_log) then
     ServerLog.add_to_liste ();
   ()
      
(* every minute, send a InfoReq message *)

let remove_client c sock s = 
  Printf.printf ("CLIENT DISCONNECTED %s")
  (Ip.to_string c.client_id);
  print_newline ();
  try 
    TcpBufferedSocket.close sock "";
    if (!!save_log) then
      begin
	ServerLog.something_append  c.client_location.loc_ip c.client_md4 s;
	ServerLog.add_to_liste () 
      end;
    
    (*remave client of clients_dy_id*)
    Hashtbl.remove clients_by_id c.client_id;
    decr nconnected_clients;
    
    (*remove all subscription made by this cient*)
    List.iter (fun n ->
		 Hashtbl.remove notifications (c.client_md4, n.notif_num)
	      ) c.client_subscriptions;
    c.client_subscriptions <- [];
    
    (* remove all files shared by this clients *)
    remove_md4_source c;
   
  with _ -> 
    Printf.printf "Exception in remove client"
   
      
let handler t event =
  Printf.printf "Client CONNECTION"; print_newline ();
  match event with
    TcpServerSocket.CONNECTION (s, Unix.ADDR_INET (from_ip, from_port)) ->

      if !!max_clients <= !nconnected_clients  then
	begin
	  (*Printf.printf "too much clients\n";*)
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
	  client_mldonkey = 0;
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
          client_subscriptions = [];
        } in

      incr nconnected_clients;
      TcpBufferedSocket.set_reader sock (
        Mftp_comm.cut_messages Mftp_server.parse (server_to_client client));
      TcpBufferedSocket.set_closer sock 
        (remove_client client)
  | _ -> 
      Printf.printf "???"; print_newline ();
      ()      

(* Check every minute that no new results have been recorded for a given
  subscription. *)
      
let check_notifications () =
  Hashtbl.iter (fun _ notif ->
      let c =  notif.notif_client in
      match c.client_sock with
        None -> ()
      | Some sock ->
          let docs = ServerIndexer.find_map notif.notif_query in
          let list = ref [] in
          Intmap.iter (fun num doc ->
              if not (Intmap.mem  num notif.notif_docs) then begin
                  notif.notif_docs <- Intmap.add num doc  notif.notif_docs;
                  list := (Store.get store doc) :: !list
                end
          ) docs;
          if !list <> [] then
            direct_server_send sock (
              M.Mldonkey_NotificationReq (notif.notif_num, !list));
  ) notifications
