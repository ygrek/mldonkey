(* Fonction of server interation in the relais protocol *)
(* Copyright 2002 vernin, INRIA *)
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

(*open Mftp_server*)
open ServerClients
open CommonGlobals
open CommonTypes
open BasicSocket
open TcpBufferedSocket
open Unix
open TcpBufferedSocket
open DonkeyMftp
open Options
open Mftp_comm
open ServerTypes  
open ServerOptions        
open ServerGlobals
open ServerMessages

module M = ServerMessages
  
let null_ip = Ip.of_int32 (Int32.of_int 0)

exception NoSocket

let add_file s md4 ip port =
  s.peer_notifications <- {
    add = true;
    md4 = md4;
    source_ip = ip;
    source_port = port;
  } :: s.peer_notifications
    
let supp_file s md4 ip port =
  s.peer_notifications <- {
    add = false;
    md4 = md4;
    source_ip = ip;
    source_port = port;
  } :: s.peer_notifications

let rec get_end_liste lst size =
  match lst with
      [] -> [] 
    | hd :: tail ->(if size = 0 then
	               tail
                     else 
		       get_end_liste tail (size-1);)

let rec get_begin_liste lst size =
  match lst with
      [] -> [] 
    | hd :: tail ->(if size = 0 then
	               []
                     else 
		       hd :: get_end_liste tail (size-1);)



let get_notify_to_send lst nbr =
  let size = List.length lst in
    (if size < nbr then
      lst,size
    else
      (get_end_liste lst (size-nbr)),nbr;) 

module LN = M.LocateNotif
      
let rec filter lst notif =
  match lst with 
      [] -> {
	LN.add = notif.add;
	LN.source_ip = notif.source_ip;
	LN.source_port =  notif.source_port; 
      }:: []
    | hd::tl -> (if (notif.source_ip = hd.LN.source_ip) && (notif.source_port = hd.LN.source_port) && (notif.add <> hd.LN.add) then
		   tl
		 else
		   filter tl notif;)
	
let supp_ack_notifs lst nbr =
  let size = List.length lst in 
    if  size <= nbr then
      []
    else
      (get_begin_liste lst nbr) 
  


let send_notification s sock =
  let notifs,nbr = get_notify_to_send s.peer_notifications 200 in
  let nb_diff_md4 = ref 0 in
  let msg = Hashtbl.create nbr in
    List.iter (fun notif ->
		 try
		   let md4_sources = Hashtbl.find msg notif.md4 in
		   let md4_sources = filter md4_sources notif in
		     Hashtbl.replace msg notif.md4 md4_sources
		 with _ ->
		   incr nb_diff_md4; 
		   Hashtbl.add msg notif.md4 [{
		     LN.add = notif.add;
		     LN.source_ip = notif.source_ip;
		     LN.source_port = notif.source_port;
		   }]
	      ) notifs;
    direct_group_send sock (LocateNotifReq {
			 LN.nb_notifs = !nb_diff_md4;
			 LN.notifs= msg;
		       });
    (*TEMPORAIRE*)
    s.peer_notifications <- supp_ack_notifs s.peer_notifications nbr 
    

let send_notif s =
  match s.peer_sock with
      Some sock -> 
	 send_notification s sock 
    | None -> 
	raise NoSocket


let get_server_id ()=
  let find = ref true in
    while !find do
      if Hashtbl.mem servers_by_id !server_counter then
	begin 
	  find := false
	end
      else
	begin 
	  incr server_counter;
	  if (!server_counter > 1000) then
	    server_counter := 0
	end
    done;
    !server_counter


let reply_to_server_connection s =
  match s.peer_sock with
    None -> ()
  | Some sock ->
      let id = get_server_id () in
	Printf.printf ("Nouvel identifiant %d") id;
	print_newline();
	s.peer_id <- id;
	Hashtbl.add servers_by_id id s;

      (*print clients_by_id;*)
      
      (*Printf.printf "SET ID"; print_newline ();*)
(* send ID back to client *)
      direct_group_send sock  (M.ACKConnectReq (
				 let module AC = M.ACKConnect in
				   {
				     AC.group_id = !group_id;
				     AC.server_id = s.peer_id;
				     AC.tags = [
				       { tag_name = "name"; tag_value = String !!server_name };
				       { tag_name = "description"; tag_value = String !!server_desc }
				     ];
				   }))

(* send some messages 
      List.iter (fun msg ->
          direct_server_send sock (M.MessageReq msg)) !!welcome_messages*)
      



let check_handler s port ok sock event = 
  if not !ok then
    match event with
      CAN_REFILL 
    | BASIC_EVENT CAN_WRITE ->
        ok := true;
        TcpBufferedSocket.close sock "connect ok";
        Printf.printf "CAN WRITE"; print_newline ();
        reply_to_server_connection s 
    (*| BASIC_EVENT (CLOSED s) ->
        ok := true;
        Printf.printf "CLOSED %s" s; print_newline ();
        c.client_kind <- Firewalled_client;
        reply_to_client_connection c *)
    | _ ->
        TcpBufferedSocket.close sock "can't connect";
        ok := false;
        Printf.printf "ERROR IN CONNECT\n"
    
let check_server s port =
  let try_sock = TcpBufferedSocket.connect "server to server"
      (Ip.to_inet_addr s.peer_ip)
    port 
      (check_handler s port (ref false))
    (*server_msg_to_string*)
  in
  BasicSocket.set_wtimeout (TcpBufferedSocket.sock try_sock) 5.;
  Printf.printf "Checking sevrer ID\n";
  ()

let server_handler s sock event = 
  match event with
    BASIC_EVENT (CLOSED _) ->
      (*
            Printf.printf "%s:%d CLOSED received by server"
(Ip.to_string s.server_ip) s.server_port; print_newline ();
  *)
      (*connection_failed (s.peer_connection_control);*)
      Printf.printf "server_handler call for close\n";
      s.peer_sock <- None
      (*set_server_state s NotConnected;*)
      (*!server_is_disconnected_hook s*)
  | _ -> ()


let remove_server_locate s =
  Hashtbl.iter (fun remote_id local_id -> 
		  let c = Hashtbl.find clients_by_id local_id in 
		      List.iter ( fun md4 ->
				    ServerLocate.supp md4 c.client_location
				) c.client_files;
		      Hashtbl.remove clients_by_id local_id
	       ) s.peer_clients
    
    
let remove_server s_id =
  remove_server_locate (Hashtbl.find servers_by_id s_id);
  decr nconnected_servers;
  Hashtbl.remove servers_by_id s_id

let server_disconnect s sock msg =
  Printf.printf "server_disconnect call for close\n";
  s.peer_sock <- None;
  ()
 


let send_to server t =
  match server.peer_sock with
      Some sock -> 
	direct_group_send sock t
    | None -> 
	raise NoSocket
 
let broadcast_to_group t =
  Hashtbl.iter (fun server_id server ->
		  try
		    send_to server t
		  with NoSocket ->
		    ()
	       ) servers_by_id


let server_to_server s t sock = 
  Printf.printf "server_to_server"; print_newline ();
  M.print t;
  print_newline ();
  match t with
      M.ServerConnectReq t ->
	let module SC =  M.ServerConnect in 
	  if ((t.SC.max_clients + !ngroup_clients) > !!max_group_clients or (t.SC.max_files + !ngroup_files) > !!max_group_files) then
	    begin 
	      (*new server allowed to much files or clients for the goupe*)
	      Hashtbl.remove  servers_by_id s.peer_id;
	      TcpBufferedSocket.close sock "CONNECTION REFUSED: to much file or client on your server";
	      decr nconnected_servers
	    end
	  else
	    begin
	      (*server accepted*)
	      s.peer_md4 <- t.M.ServerConnect.md4;
	      s.peer_tags <- t.M.ServerConnect.tags;
	      check_server s s.peer_port;      
	      
	      let module SN = M.ServerNotification in
		broadcast_to_group (ServerNotificationReq {
				      SN.group_id = !group_id;
				      SN.server_id = s.peer_id;
				      SN.server_ip = s.peer_ip;
				      SN.server_port = s.peer_port;
				    }
				   )

	    end

    | M.ACKConnectReq t ->
	let module A = M.ACKConnect in
	  s.group_id <- t.A.group_id;
	  s.peer_master <- true ;
	  s.peer_id <- t.A.server_id;
	  s.peer_tags <- t.A.tags;

	  Hashtbl.add servers_by_id s.peer_id s;

	  ServerLocate.get_local_sources s;
	  ()
	  
    | M.ServerNotificationReq t ->
	if !!relais_master then
	  (*"I'm the law and ServerNotificationReq messages isn't for me"*)
	  ()
	else
	  (*new server in the group*)
	  if s.peer_master then
	    begin
	      
              let module SN = ServerNotification in
	      let new_server = {
		group_id = s.group_id;
		peer_master = false ;
		peer_id = t.SN.server_id;
		peer_md4 = Md4.null;
		peer_ip =  t.SN.server_ip;
		peer_port =  t.SN.server_port;
		peer_sock = None;
		peer_need_recovery = true;
		peer_notifications = [];
		peer_clients = Hashtbl.create 100;
		peer_tags = [];
	      } in
		Hashtbl.add servers_by_id new_server.peer_id new_server;
		
		(*connect to the new server*)
		(*connect_server new_server;*)

		(*put all local information in the buffer*)
		
	    end
	    
    | M.RecoveryReq t ->
	remove_server_locate s

    | M.ServerSuppReq t ->
	if !!relais_master then
	  (*"I'm the law and ServerNotificationReq messages isn't for me"*)
	  ()
	else 
	  begin
	    if s.peer_master then
	      begin
		try 
		  close sock "Removed of the group";
		  let module SS = M.ServerSupp in
		    remove_server t.SS.server_id;
		with _ -> ()
	      end
	  end
   
  
    | M.LocalisationInitReq t ->
	let module I = M.LocalisationInit in
	  List.iter (fun md4_locs ->
		       (*add new sources in the localisation table*)
		       ServerLocate.adds md4_locs.I.md4 md4_locs.I.sources;
		       List.iter ( fun client ->
				     try
				       begin
					 (*modify client's list of md4 shared*)
					 let local_id = Hashtbl.find s.peer_clients client.I.source_ip in 
					 let client = Hashtbl.find clients_by_id local_id in
					   client.client_files <- md4_locs.I.md4 :: client.client_files
				       end
				     with _ -> 
				       (*if client is not in the clients_by_id table*)
				       let new_id = get_client_id client.I.source_ip in
					 Hashtbl.add s.peer_clients client.I.source_ip new_id;
					 Hashtbl.add clients_by_id new_id {
					   client_id = new_id; 
					   client_mldonkey = 0;
					   client_conn_ip = null_ip;
					   client_md4 = Md4.null;
					   client_sock = None;
					   client_kind = ( if client.I.source_port = 0 then
							     Firewalled_client
							   else
							     KnownLocation (client.I.source_ip,client.I.source_port)
							 );
					   client_files = [md4_locs.I.md4];
					   client_tags = [];
					   client_location = {
					     loc_ip  = client.I.source_ip;
					     loc_port = client.I.source_port;
					     loc_expired = 0.0;
					   };
					   client_results = { docs = [||]; next_doc = 0; };
					 }
				 ) md4_locs.I.sources
		    ) t

    | M.LocalisationNotifReq t ->
	let module N = M.LocalisationNotif in 
	  List.iter (fun md4_locs ->
		       (*add new sources in the localisation table*)
		       ServerLocate.notifications md4_locs.N.md4 md4_locs.N.sources;
		       let md4 = md4_locs.N.md4 in
			 List.iter ( fun source ->
				       try
					 begin
					   (*modify client's list of md4 shared*)
					   let local_id = Hashtbl.find s.peer_clients source.N.source_ip in 
					   let client = Hashtbl.find clients_by_id local_id in
					     if source.N.add then
					       (* if the notification is a add*)
					       client.client_files <- md4_locs.N.md4 :: client.client_files
					     else
					       (* if the notification is a supp*)
					       begin
						 client.client_files <- List.filter (fun x ->
										       md4 <> x 
										    )  client.client_files;
						 if client.client_files = [] then
						   begin
						     Hashtbl.remove s.peer_clients source.N.source_ip;
						     Hashtbl.remove clients_by_id local_id 
						   end
					       end
					 end
				     with _ -> 
				       (*if client is not in the clients_by_id table*)
				       let new_id = get_client_id source.N.source_ip in
					 Hashtbl.add s.peer_clients source.N.source_ip new_id;
					 Hashtbl.add clients_by_id new_id {
					   client_id = new_id; 
					   client_mldonkey = 0;
					   client_conn_ip = null_ip;
					   client_md4 = Md4.null;				
					   client_sock = None;
					   client_kind = ( if source.N.source_port = 0 then
							     Firewalled_client
							   else
							     KnownLocation (source.N.source_ip,source.N.source_port)
							 );
					   client_files = [md4];
					   client_tags = [];
					   client_location = {
					     loc_ip  = source.N.source_ip;
					     loc_port = source.N.source_port;
					     loc_expired = 0.0;
					   };
					   client_results = { docs = [||]; next_doc = 0; };
					 }
				 ) md4_locs.N.sources
		  ) t
    | LocateNotifReq t ->
	let module LN = M.LocateNotif in 
	  Hashtbl.iter ( fun md4 sources_list -> 
			   (*ServerLocate.notifications md4 sources_list;*)
			    List.iter ( fun source ->
				       try
					 begin
					   (*modify client's list of md4 shared*)
					   let local_id = Hashtbl.find s.peer_clients source.LN.source_ip in 
					   let client = Hashtbl.find clients_by_id local_id in
					     if source.LN.add then
					       (* if the notification is a add*)
					       client.client_files <- md4 :: client.client_files
					     else
					       (* if the notification is a supp*)
					       begin
						 client.client_files <- List.filter (fun x ->
										       md4 <> x 
										    )  client.client_files;
						 if client.client_files = [] then
						   begin
						     Hashtbl.remove s.peer_clients source.LN.source_ip;
						     Hashtbl.remove clients_by_id local_id 
						   end
					       end
					 end
				     with _ -> 
				       (*if remote client is not in server's client list*)
				       let new_id = get_client_id source.LN.source_ip in
					 Hashtbl.add s.peer_clients source.LN.source_ip new_id;
					 Hashtbl.add clients_by_id new_id {
					   client_id = new_id; 
					   client_conn_ip = null_ip;
					   client_md4 = Md4.null;
					    client_mldonkey = 0;
					   client_sock = None;
					   client_kind = ( if source.LN.source_port = 0 then
							     Firewalled_client
							   else
							     KnownLocation (source.LN.source_ip,source.LN.source_port)
							 );
					   client_files = [md4];
					   client_tags = [];
					   client_location = {
					     loc_ip  = source.LN.source_ip;
					     loc_port = source.LN.source_port;
					     loc_expired = 0.0;
					   };
					   client_results = { docs = [||]; next_doc = 0; };
					 }
				      ) sources_list
		       ) t.LN.notifs
	  
	


    | QuitReq ->
	close sock "Removed of the group";
	remove_server s.peer_id;
	let module SS = ServerSupp in
	  broadcast_to_group (ServerSuppReq {
				SS.group_id = !group_id;
				SS.server_id = s.peer_id;
			      }
			     )
	  

    | _ -> ()
   
exception CantConnect
     
let connect_server s = 
  try
    (*                Printf.printf "CONNECTING ONE SERVER"; print_newline (); *)
    (*connection_try s.server_connection_control;*)
    incr nconnected_servers; 
    let sock = TcpBufferedSocket.connect 
          "server to server"
        (
          Ip.to_inet_addr s.peer_ip) s.peer_port 
          (server_handler s) (* Mftp_comm.server_msg_to_string*)  in
      verify_ip sock;
      (*set_server_state s Connecting;*)
      set_read_controler sock download_control;
      set_write_controler sock upload_control;
      
      set_reader sock (Mftp_comm.cut_messages ServerMessages.parse
          (server_to_server s));
      if !!relais_master then
	set_closer sock (server_disconnect s);
      (*set_rtimeout sock !!server_connection_timeout;*)
      set_handler sock (BASIC_EVENT RTIMEOUT) (fun s ->
          close s "timeout"  
      );
      
      s.peer_sock <- Some sock;
      
       (*direct_group_send sock (
        let module C = M.ConnectRocky in
        M.ConnectRockyReq {
          C.group_id = !group_id ;
	  C.server_id = !server_id;
          C.server_ip = !!server_ip;
          C.server_port = !!server_port;
          C.server_tags =  [
	    { tag_name = "name"; tag_value = String !!server_name };
	    { tag_name = "description"; tag_value = String !!server_desc }
	  ];
        }
       );*)


    with _ -> 
      Printf.printf "%s:%d IMMEDIAT DISCONNECT\n"
      (Ip.to_string s.peer_ip) s.peer_port;
(*      Printf.printf "DISCONNECTED IMMEDIATLY"; print_newline (); *)
        decr nconnected_servers;
        s.peer_sock <- None;
	raise CantConnect
        (*set_server_state s NotConnected;*)
        (*connection_failed s.server_connection_control*)

let join_a_group ip port =

  let server = {
    group_id = Md4.null;
    peer_master = false ;
    peer_id = 0;
    peer_md4 = Md4.null;
    peer_ip = ip;
    peer_port =  port;
    peer_sock = None;
    peer_need_recovery = true;
    peer_notifications = [];
    peer_clients = Hashtbl.create 100;
    peer_tags = [];
  } in
    
    try
      connect_server server;
    
      Hashtbl.add servers_by_id server.peer_id server;
      
      send_to server (ServerConnectReq 
			(let module CS = M.ServerConnect in
			   {
			     CS.md4 = !!server_md4;
			     CS.ip = !!server_ip;
			     CS.port = !!server_port;
			     CS.max_clients = !!max_group_clients;
			     CS.max_files = !!max_group_files;
			     CS.tags =  [
			       { tag_name = "name"; tag_value = String !!server_name };
			       { tag_name = "description"; tag_value = String !!server_desc }
			     ];
			   })
		     )
	
    with _->
      Printf.printf "Can't open socket to %s:%d" (Ip.to_string ip) port;
      Hashtbl.remove servers_by_id 0

   
   

  
let handler t event =
  (*Printf.printf "CONNECTION"; print_newline ();*)
  match event with
    TcpServerSocket.CONNECTION (s, Unix.ADDR_INET (from_ip, from_port)) ->

      if !!max_servers <= !nconnected_servers then
	  (*Printf.printf "too much clients\n";*)
          Unix.close s
      else
      let sock = TcpBufferedSocket.create "server server connection" s (fun _ _ -> ()) 
        (*server_msg_to_string*)
        in
      
      let ip = Ip.of_inet_addr from_ip in
      let server = {
	group_id = Md4.null;
	peer_master = false;
        peer_id = 0;
	peer_md4 = Md4.null;
        peer_ip = ip;
	peer_port = from_port;
	peer_need_recovery = true;
        peer_sock = Some sock;
	peer_notifications = [];
	peer_clients = Hashtbl.create 100; 
        peer_tags = []; 
        } in

      incr nconnected_servers;

      TcpBufferedSocket.set_reader sock (
        Mftp_comm.cut_messages ServerMessages.parse (server_to_server server));
      if !!relais_master then
	TcpBufferedSocket.set_closer sock 
          (server_disconnect server)
  | _ -> 
      Printf.printf "???"; print_newline ();
      ()  

    
let get_ride_of s =
  false

let get_local_sources s =
  List.iter ( fun c_id ->
		let c = Hashtbl.find clients_by_id c_id in
		  match c.client_kind with
		      Firewalled_client ->
			List.iter ( fun md4 ->
				      s.peer_notifications <- {
					add = true;
					md4 = md4;
					source_ip = c_id; 
					source_port = 0;
				      } :: s.peer_notifications
				  ) c.client_files
		    | KnownLocation (ip,port) ->
			List.iter ( fun md4 ->
				      s.peer_notifications <- {
					add = true;
					md4 = md4;
					source_ip =  ip;
					source_port = port;
				      } :: s.peer_notifications
				  ) c.client_files
	    )local_clients

let notify_server time =
  let dead_servers = ref [] in
    Hashtbl.iter (fun id s ->
		    match s.peer_sock with
			None ->
			  begin
			    try
			      connect_server s;
			      if s.peer_need_recovery then
				begin
				  let module R = M.Recovery in
				    send_to s (RecoveryReq 
						 {
						   R.group_id = !group_id;
						   R.server_id = !server_id;
						   R.server_ip = !!server_ip;
						   R.server_port = !!server_port;
						 });
				    s.peer_need_recovery <- false;
				    ServerLocate.get_local_sources s
				end;				     
				send_notif s
			    with _ -> 
			      if !!relais_master && (get_ride_of s) then 
				dead_servers := id :: !dead_servers;
			  end
		      | Some sock -> 
			  send_notification s sock
		 ) servers_by_id; 
    if !!relais_master && (!dead_servers = []) then 
      Hashtbl.iter (fun id s ->
		      List.iter (fun dead_server ->
				   send_to s (
				     let module SS = M.ServerSupp in
				       (ServerSuppReq 
					  {
					    SS.group_id = !group_id;
					    SS.server_id = id
					  }
				       )
				   )
				) !dead_servers
		   ) servers_by_id 
	
      
     
		    
			
