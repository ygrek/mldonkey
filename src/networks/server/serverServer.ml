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

open Md4
open CommonGlobals
open CommonTypes
open BasicSocket
open TcpBufferedSocket
open Unix
open TcpBufferedSocket
open DonkeyMftp
open Options
open DonkeyProtoCom
open ServerTypes  
open ServerOptions        
open ServerGlobals
open ServerMessages

module M = ServerMessages
  
let null_ip = Ip.null

exception CantConnect
exception NoSocket


let add_file s md4 id ip port =
  s.server_notifications <- {
    add = true;
    md4 = md4;
    source_id = id;
    source_ip = ip;
    source_port = port;
  } :: s.server_notifications



(*
let add_source_to_notify c md4 =
  let ip, port = (match c.client_kind with 
                    Firewalled_client -> c.client_id, 0
                    | KnownLocation (ip,port) -> ip, port) in
    Hashtbl.iter (fun id s ->
                    if not (s.server_need_recovery) then 
                      Queue.add  {
                        add = true;
                        md4=md4;
                        source_id=c.client_id; 
                        source_ip = ip;
                        source_port = port} s.server_notifications
                 ) servers_by_id
*)

let add_source_to_notify c md4 =
  let ip, port = (match c.client_kind with 
                      Firewalled_client -> c.client_id, 0
                    | KnownLocation (ip,port) -> ip, port) in
    Hashtbl.iter (fun id s ->
                    if not (s.server_need_recovery) then 
                      s.server_notifications <- {
                        add = true;
                        md4=md4;
                        source_id=c.client_id; 
                        source_ip = ip;
                        source_port = port} :: s.server_notifications
                 ) servers_by_id


    
let supp_file s md4 id ip port =
  s.server_notifications <- {
    add = false;
    md4 = md4;
    source_id = id;
    source_ip = ip;
    source_port = port;
  } :: s.server_notifications


(*
let supp_source_to_notify c md4 =
  let ip, port = ( match c.client_kind with 
                       Firewalled_client -> c.client_id, 0
                     | KnownLocation (ip,port) -> ip, port) in
    Hashtbl.iter (fun id s -> 
                    if not (s.server_need_recovery) then
                      Queue.add {
                        add = false;
                        md4=md4;
                        source_id=c.client_id; 
                        source_ip = ip;
                        source_port = port} s.server_notifications
                 ) servers_by_id
*)

let supp_source_to_notify c md4 =
  let ip, port = ( match c.client_kind with 
                       Firewalled_client -> c.client_id, 0
                     | KnownLocation (ip,port) -> ip, port) in
    Hashtbl.iter (fun id s -> 
                    if not (s.server_need_recovery) then
                      s.server_notifications <- {
                        add = false;
                        md4=md4;
                        source_id=c.client_id; 
                        source_ip = ip;
                        source_port = port} :: s.server_notifications
                 ) servers_by_id
    



let rec get_end_liste lst size =
  match lst with
      [] -> [] 
    | hd :: tail ->(if size = 0 then
                       hd::tail
                     else 
                       get_end_liste tail (size-1);)

let rec get_begin_liste lst size =
  match lst with
      [] -> [] 
    | hd :: tail ->(if size = 0 then
                       [hd]
                     else 
                       hd :: get_begin_liste tail (size-1);)



let get_notify_to_send lst nbr =
  let size = List.length lst in
    (if size <= nbr then
      lst,size
    else
      (get_end_liste lst (size-nbr)),nbr;) 


let get_end_queue lst nbr =
  let notifs = ref [] in
    for i = 1 to nbr do
      notifs := (Queue.peek lst) :: !notifs;
    done;
    !notifs

(*
let get_notify_to_send lst nbr =
  let notifs = ref [] in
  let size = Queue.length lst in
    if (size < nbr) then
      notifs := get_end_queue lst size
    else 
      notifs := get_end_queue lst nbr;
    (!notifs,(List.length !notifs))
*)


module LN = M.LocateNotif
      
let rec filter lst notif =
  match lst with 
      [] -> [{
        LN.add = notif.add;
        LN.source_id = notif.source_id;
        LN.source_ip = notif.source_ip;
        LN.source_port =  notif.source_port; 
      }]
    | hd::tl -> 
        (if (notif.source_id = hd.LN.source_id) && (notif.add <> hd.LN.add) then
          tl
        else
          hd :: filter tl notif;)

        
let supp_ack_notifs lst nbr =
  let size = List.length lst in 
    if  size <= nbr then
      []
    else
      begin 
        (*lprintf "I have to get %d element\n" (size-nbr);*)
        (get_begin_liste lst (size-nbr))
      end

(*
let supp_ack_notifs lst nbr =
  try
    for i = 0 to nbr do
      ignore(Queue.take lst);
    done
  with _ -> lprintf "WARNING: SUPP NO ELEMENT FROM SERVER NOTIFICATION QUEUE\n"
*)

let send_notification s sock =
 try
  (*lprintf "BEGIN CONSTRUCTION OF NOTIF PACKET\n";*)
  let id_msg = ref 0 in
  let nb_notifs = ref 200 in
  let last_id_msg,last_nb_notifs = s.server_last_message_send in
   (*lprintf "Last msg %d -> %d\n" last_id_msg last_nb_notifs;*)
   if last_nb_notifs = -1 then
      (if last_id_msg > 100 then
         id_msg := 0
      else  
        id_msg := last_id_msg + 1)
    else 
      begin
        nb_notifs := last_nb_notifs;
        id_msg := last_id_msg;
      end;
  (*lprintf "Liste of notif %d\n" (List.length s.server_noxstifications); *)           
  let notifs,nbr = get_notify_to_send s.server_notifications !nb_notifs in
    (*lprintf "Send notification %d with %d notifs in real %d\n" !id_msg nbr (List.length notifs);*)

  let nb_diff_md4 = ref 0 in
  let msg = Hashtbl.create nbr in
    List.iter (fun notif ->
                 try
                   let md4_sources = Hashtbl.find msg notif.md4 in
                     Hashtbl.replace msg notif.md4 (filter md4_sources notif)
                 with _ ->
                   incr nb_diff_md4; 
                   Hashtbl.add msg notif.md4 [{
                     LN.add = notif.add;
                     LN.source_id = notif.source_id;
                     LN.source_ip = notif.source_ip;
                     LN.source_port = notif.source_port;
                   }]
              ) (List.rev notifs);
   

    direct_group_send sock (LocateNotifReq {
                         LN.message_id = !id_msg;
                         LN.nb_notifs = !nb_diff_md4;
                         LN.ack = s.server_last_message_received_id;
                         LN.notifs= msg;
                       });
    s.server_last_message_send <- (!id_msg,nbr);
 with _ -> lprintf "CAN'T MAKE NOTIF PACKET\n" 
    

let send_notif s =
  match s.server_sock with
      Some sock -> 
         send_notification s sock 
    | None -> 
        raise NoSocket


let get_server_id ()=
  let find = ref true in
    while !find do
      if Hashtbl.mem servers_by_id !server_counter then
        begin
          incr server_counter;
          if !server_counter > 2000 then 
                         server_counter := 0; 
        end
      else
        find := false;
    done;
    !server_counter


let get_client_id ip =
  if Hashtbl.mem clients_by_id ip then
    begin
      let find = ref true in
        while !find do
          if Hashtbl.mem clients_by_id (Ip.of_int32 (Int32.of_int !client_counter)) then
            begin 
              incr client_counter;
              if (!client_counter > 2000) then
                client_counter := 1
            end
          else
            find := false
           
        done;
        (Ip.of_int32 (Int32.of_int !client_counter))  
    end
  else
    ip




let send_to server t =
  match server.server_sock with
      Some sock ->
        direct_group_send sock t
    | None -> 
        raise NoSocket
 
let broadcast_to_group id t =
  Hashtbl.iter (fun server_id server ->
                  try
                    if id <> server_id then
                       send_to server t
                  with NoSocket ->
                    ()
               ) servers_by_id

let get_local_sources s =
  Hashtbl.iter ( fun id c ->
                match c with 
                LocalClient c ->
                  (match c.client_kind with
                      Firewalled_client ->
                        List.iter ( fun md4 ->
                                      s.server_notifications <- {
                                        add = true;
                                        md4 = md4;
                                        source_id = c.client_id;
                                        source_ip = id; 
                                        source_port = 0;
                                      } :: s.server_notifications
                                  ) c.client_files
                     | KnownLocation (ip,port) ->
                         List.iter ( fun md4 ->
                                       s.server_notifications <- {
                                         add = true;
                                         md4 = md4;
                                         source_id = c.client_id;
                                         source_ip = ip;
                                         source_port = port;
                                       } :: s.server_notifications
                                  ) c.client_files)
                  | _ -> ()
            ) clients_by_id




let reply_to_server_connection s =
  match s.server_sock with
    None -> (*lprintf "Can't reply server connection\n";*)
      ()
            
  | Some sock ->
       try 
         let id = get_server_id () in
        (*lprintf ("Nouvel identifiant %d") id;
          lprint_newline();*)
           s.server_id <- id;
           Hashtbl.add servers_by_id id s;

      (*print clients_by_id;*)
      
      (*lprintf "SET ID"; lprint_newline ();*)
       (*Pervasives.flush Pervasives.stdout;*)
(* send ID back to client *)
           direct_group_send sock  (M.ACKConnectReq (
                                      let module AC = M.ACKConnect in
                                        {
                                          AC.group_id = !group_id;
                                          AC.server_master_id = !server_id;
                                          AC.server_id = s.server_id;
                                          AC.tags = [
                                            { tag_name = "name"; tag_value = String !!server_name };
                                            { tag_name = "description"; tag_value = String !!server_desc }
                                          ];
                                        }));
           
           
           get_local_sources s; 
           
           send_notification s sock; 
           
           
           (*lprintf "notif send, broadcast new server position\n";*)
           
           let module SN = M.ServerNotification in
             broadcast_to_group s.server_id (ServerNotificationReq {
                                               SN.group_id = !group_id;
                                               SN.server_id = s.server_id;
                                               SN.server_ip = s.server_ip;
                                               SN.server_port = s.server_port;
                                             }
                                            ); 
             
             
       with _ -> 
             lprintf "Failure in reply connection server\n"
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
        (*lprintf "CAN WRITE\n";*)
        (*(match s.server_sock with
         None -> ()
        | Some sock -> lprintf "Cool\n");*)
        reply_to_server_connection s 
    | _ ->
        TcpBufferedSocket.close sock "can't connect";
        ok := false;
        lprintf "ERROR IN CONNECT\n"
    
let check_server s port =
  let try_sock = TcpBufferedSocket.connect "server to server"
      (Ip.to_inet_addr s.server_ip)
    port 
      (check_handler s port (ref false))
    (*server_msg_to_string*)
  in
  BasicSocket.set_wtimeout (TcpBufferedSocket.sock try_sock) 5.;
  (*lprintf "Checking sevrer ID\n";*)
  ()

let server_handler s sock event = 
  match event with
    BASIC_EVENT (CLOSED _) ->
    lprintf "%s:%d CLOSED received by server"
(Ip.to_string s.server_ip) s.server_port; lprint_newline ();
 
      (*connection_failed (s.server_connection_control);*)
      lprintf "server_handler call for close\n";
      s.server_sock <- None
      (*set_server_state s NotConnected;*)
      (*!server_is_disconnected_hook s*)
  | _ -> ()


let remove_server_locate s =
  Hashtbl.iter (fun remote_id local_id -> 
                  decr nremote_clients;
                  let c = Hashtbl.find clients_by_id local_id in
                      match c with
                      RemoteClient c -> 
                      (match c.remote_client_kind with
                       Firewalled_client ->
                         List.iter ( fun md4 ->
                                     ServerLocate.remote_supp md4 {
                                       loc_ip = c.remote_client_local_id; 
                                       loc_port= 0;
                                       loc_expired=0.;
                                       loc_local = false;
                                     } 
                                ) c.remote_client_files
                      | KnownLocation (ip,port) ->
                          List.iter ( fun md4 ->
                                     ServerLocate.remote_supp md4 {
                                       loc_ip = c.remote_client_local_id; 
                                       loc_port= port;
                                       loc_expired=0.;
                                       loc_local = false;
                                     } 
                                ) c.remote_client_files);           
                      Hashtbl.remove clients_by_id local_id
                      | _ -> ()
               ) s.server_clients
    
    

let rec remove id lst =
  match lst with 
      [] -> []
    | hd :: tl -> if hd.server_id = id then
                     begin
                       remove_server_locate hd;
                       tl
                     end
                   else
                     remove id tl

 let remove_server s_id =
  try
    let s = Hashtbl.find servers_by_id s_id in
      remove_server_locate s;
      (match s.server_sock with
           Some sock ->
             close sock "Removed of the group";
             s.server_sock <- None
         | _ -> ());
      Hashtbl.remove servers_by_id  s_id; 
      to_connect := List.filter (fun id ->
                    id<>s_id) !to_connect;
    decr nconnected_servers
   with _ -> lprintf "Failure when remove a server of the group"
 

 let print_servers () =
   lprintf "Contenu de server_by_id:\n";
   Hashtbl.iter (fun id s -> 
                   lprintf "server %d " id )
     servers_by_id;
   lprint_newline();
   lprintf "Contenu de to_connect:\n";
   List.iter (fun s_id -> 
                lprintf "server %d " s_id )
     !to_connect;
   lprint_newline()  


   

let server_disconnect_of_master s sock msg =
  lprintf "DISCONNECT:server %d disconnect_of_master call for close\n" s.server_id;
  s.server_sock <- None;
  close sock "connection dead";

  if (Unix.time() -. s.server_last_message_received_time) > !!server_dead_time_out then
    begin
      lprintf "Server %d disconnected since %f so I remove it of the group\n" s.server_id (Unix.time() -. s.server_last_message_received_time);
      remove_server s.server_id;
      decr nconnected_servers;
      lprintf "Server supprimer du groupe\n";
                       let module SS = ServerSupp in
                         broadcast_to_group !server_id (ServerSuppReq 
                                                          {
                                                            SS.group_id = !group_id;
                                                            SS.server_id = s.server_id
                                                          }
                                                       )
   end
  else
     begin
      lprintf "Server %d add to_connect and not remove\n" s.server_id;
      to_connect := s.server_id :: !to_connect
  end

 

let server_disconnect s sock msg =
  lprintf "DISCONNECT:server %d disconnect call for close\n" s.server_id;
  s.server_sock <- None;
  close sock "connection dead";
  to_connect := s.server_id :: !to_connect;    
  ()
 


(* let send_to server t =
  match server.server_sock with
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
               ) servers_by_id*)


let rec check_and_remove lst id =
   match lst with
     [] -> []
   | hd::tl -> (if hd = id then
                  tl
                else
                  hd :: check_and_remove tl id;) 
 
let direct_connect_server f s = 
  try
    lprintf "CONNECTING TO ONE SERVER %s:%d\n" (Ip.to_string s.server_ip) (s.server_port+5) ; 
    (*connection_try s.server_connection_control;*)
    incr nconnected_servers; 
    let sock = TcpBufferedSocket.connect 
          "server to server"
        (Ip.to_inet_addr s.server_ip) (s.server_port+5) 
          (server_handler s) (* DonkeyProtoCom.server_msg_to_string*)  in
      (*set_server_state s Connecting;*)
      (*set_read_controler sock download_control;
      set_write_controler sock upload_control;*)
      
      set_reader sock (DonkeyProtoCom.cut_messages ServerMessages.parse
          (f s));
      if !!relais_master then
        set_closer sock (server_disconnect_of_master s) 
      else
        set_closer sock (server_disconnect s);
      set_rtimeout sock 60.;
      set_handler sock (BASIC_EVENT RTIMEOUT) (fun s ->
                                                 lprintf "Standart rtimeout";
                                                 close s "timeout"  
                                              );
      
      
      s.server_sock <- Some sock;


    with _ -> 
      lprintf "%s:%d IMMEDIAT DISCONNECT\n"
      (Ip.to_string s.server_ip) s.server_port;
(*      lprintf "DISCONNECTED IMMEDIATLY"; lprint_newline (); *)
        decr nconnected_servers;
        s.server_sock <- None;
        raise CantConnect
        (*set_server_state s NotConnected;*)
        (*connection_failed s.server_connection_control*)



let rec server_to_server s t sock = 
  (*lprintf "----------------\nMSG received in server_to_server from %d locate at %s:%d\n" s.server_id (Ip.to_string s.server_ip) s.server_port; 
  M.print t;*)
  s.server_last_message_received_time <- Unix.time();

  match t with
      M.ServerConnectReq t ->
        if !!relais_master then
          let module SC =  M.ServerConnect in 
            if ((t.SC.max_clients + !ngroup_clients) > !!max_group_clients or (t.SC.max_files + !ngroup_files) > !!max_group_files) then
              begin 
                (*new server allowed to much files or clients for the goupe*)
                direct_group_send sock (M.MessageReq ("too high clients and file bound\n"));
                TcpBufferedSocket.close sock "CONNECTION REFUSED: too many file or client on your server";
                decr nconnected_servers
              end
            else
              begin
                (*server accepted*)
                s.server_md4 <- t.M.ServerConnect.md4;
                s.server_port <- t.M.ServerConnect.port;
                s.server_tags <- t.M.ServerConnect.tags;
        
                check_server s s.server_port;      
              end;
        else
          begin
            Hashtbl.remove  servers_by_id s.server_id;
            TcpBufferedSocket.close sock "CONNECTION REFUSED: I'm not a master server";
            decr nconnected_servers
          end
              

    | M.ACKConnectReq t ->
        let module A = M.ACKConnect in
          s.server_group_id <- t.A.group_id;
          s.server_master <- true ;
          s.server_id <- t.A.server_master_id;
          s.server_tags <- t.A.tags;
        

          group_id := t.A.group_id;
          server_id := t.A.server_id;
          
          Hashtbl.add servers_by_id s.server_id s;

          get_local_sources s;

          lprintf "Server connect to %s\n" (Md4.to_string t.A.group_id); 

          ()
          
    | M.ServerNotificationReq t ->
        if !!relais_master then
          (*"I'm the law and ServerNotificationReq messages isn't for me"*)
          ()
        else
          (*new server in the group*)
          if s.server_master then
            begin
              
              let module SN = ServerNotification in
              let new_server = {
                server_group_id = t.SN.group_id;
                server_master = false ;
                server_id = t.SN.server_id;
                server_md4 = Md4.null;
                server_ip = t.SN.server_ip;
                server_port = t.SN.server_port;
                server_last_message_send = (0,0);
                server_last_message_received_id = -1; 
                server_last_message_received_time = 0.;
                server_sock = None;
                server_need_recovery = false;
                server_notifications = [];
                server_clients = Hashtbl.create 100;
                server_tags = [];
              } in
                
                

                (*to_connect := new_server.server_id :: !to_connect;*)
                
                Hashtbl.add servers_by_id new_server.server_id new_server;

                direct_connect_server server_to_server new_server;

                get_local_sources new_server;
        
                lprintf "Server %s:%d join the group\n" (Ip.to_string new_server.server_ip) new_server.server_port;

                let module CG = ConnectByGroup in
                send_to new_server (ConnectByGroupReq 
                                  {
                                    CG.group_id = !group_id; 
                                    CG.server_id = !server_id;
                                    CG.server_ip = !!server_ip;
                                    CG.server_port = !!server_port; 
                                  });
        
            end
        
    | M.ConnectByGroupReq t ->
        let module CG = ConnectByGroup in
          if (List.exists (fun s_id -> s_id = t.CG.server_id) !to_connect) then
            begin
              lprintf "Reconnection from %d\n" t.CG.server_id;
              try
                decr nconnected_servers;
                to_connect := check_and_remove !to_connect  t.CG.server_id;
                lprintf "liste of connection is trediuce\n";
                List.iter (fun s_id -> 
                             lprintf "add to connecte to %d\n" s_id
                          ) !to_connect;
                let server = ref (Hashtbl.find servers_by_id  t.CG.server_id) in
                  TcpBufferedSocket.set_reader sock (
                    DonkeyProtoCom.cut_messages ServerMessages.parse (server_to_server !server));
                  if !!relais_master then
                    TcpBufferedSocket.set_closer sock 
                      (server_disconnect_of_master !server)
                  else
                    TcpBufferedSocket.set_closer sock 
                      (server_disconnect !server);
                  !server.server_sock <- Some sock;
              with _ -> lprintf "PB in Reconenction process\n"
              end

          else
            begin
              lprintf "Connection from %d server in %s group\n" t.CG.server_id (Md4.to_string t.CG.group_id);
              s.server_group_id <- t.CG.group_id;
              s.server_id <- t.CG.server_id;
              s.server_ip <- t.CG.server_ip;
              s.server_port <- t.CG.server_port; 
              Hashtbl.add servers_by_id s.server_id s;
            end;
          ()
         
          

    
    | M.RecoveryReq t ->	
        remove_server_locate s;
        ()

    | M.ServerSuppReq t ->
        if !!relais_master then
          (*"I'm the law and ServerNotificationReq messages isn't for me"*)
          ()
        else 
          begin
            if s.server_master then
                try 
                  let module SS = M.ServerSupp in
                    remove_server t.SS.server_id;
                with _ -> ()
          end
   
    | AckNotifReq t ->
        let id_msg,nbr_notif = s.server_last_message_send in  
          if t = id_msg then
            begin
              (*lprintf "ACK of %d , supp %d notifs\n" id_msg nbr_notif;*)
              s.server_last_message_send <- (id_msg,-1);
              (*lprintf "Added To send  %d\n" (List.length s.server_notifications);*)
              s.server_notifications <- supp_ack_notifs s.server_notifications nbr_notif;
              (*lprintf "Have To send  %d\n" (List.length s.server_notifications);*)
            end;
          if (200 < (List.length s.server_notifications)) then
            send_notification s sock 
              

    | LocateNotifReq t ->
        let module LN = M.LocateNotif in
       
        let id_msg,nbr_notif = s.server_last_message_send in  
          
          (*Piggybacking*)
          if (t.LN.ack = id_msg) then
           begin
             (*lprintf "Piggybacking  of %d , supp %d notifs\n" id_msg nbr_notif;*)
             s.server_last_message_send <- (id_msg,-1);
             (*lprintf "Added To send  %d\n" (List.length s.server_notifications);*)
             s.server_notifications <- supp_ack_notifs s.server_notifications nbr_notif;
             (*lprintf "Have To send  %d\n" (List.length s.server_notifications);*)
           end;

          (*process the notifications*)
         if (t.LN.message_id <> s.server_last_message_received_id) then
           begin
             (*lprintf "->Received %d notif message\n" t.LN.message_id;*)
             s.server_last_message_received_id <- t.LN.message_id; 
             let unkown_md4 = ref [] in
             let count = ref 0 in
               Hashtbl.iter ( fun md4 sources_list -> 
                               (*lprintf "Add source for %s\n" (Md4.to_string md4);*)
                               (try
                                  let tags = ServerIndexer.get_def md4 in
                                    ()
                                with _ ->
                                  unkown_md4 := md4 :: !unkown_md4
                               ); 
                                let notifs = ref [] in
                                  List.iter ( fun source ->
                                                incr count;
                                                try
                                                  begin
                                                    (*modify client's list of md4 shared*)
                                                    let local_id = Hashtbl.find s.server_clients source.LN.source_id in
                                                      (*lprintf "Remote Client %s existe here %s \n" (Ip.to_string source.LN.source_ip) (Ip.to_string local_id); *)  
                                                    let client = Hashtbl.find clients_by_id local_id in
                                                      match client with
                                                          RemoteClient client ->
                                                            (
                                                              if source.LN.add then
                                                                (* if the notification is a add*)
                                                                begin
                                                                  notifs := (true,{
                                                                    loc_ip = local_id;
                                                                    loc_port = source.LN.source_port;
                                                                    loc_expired = 0.0;
                                                                    loc_local = false;
                                                                  }) :: !notifs;
                                                                  client.remote_client_files <- md4 :: client.remote_client_files
                                                                end
                                                              else
                                                                (* if the notification is a supp*)
                                                                begin
                                                                  if not (List.mem md4 client.remote_client_files) then
                                                                    lprintf "WARNING: supp md4 on client fail\n";
                                                                  notifs := (false,{
                                                                               loc_ip = local_id;
                                                                               loc_port = source.LN.source_port;
                                                                               loc_expired = 0.0;
                                                                               loc_local = false;
                                                                             }) :: !notifs;
                                                                  client.remote_client_files <- List.filter (fun x ->
                                                                                                               md4 <> x 
                                                                                                            )  client.remote_client_files;
                                                                  
                                                                  if client.remote_client_files = [] then
                                                                    begin
                                                                      try 
                                                                        Hashtbl.remove s.server_clients source.LN.source_id;
                                                                        Hashtbl.remove clients_by_id local_id ;
                                                                        decr nremote_clients;
                                                                      with _ -> lprintf "WARNING: Cant't remove remote clients\n"
                                                                    end
                                                                end)
                                                        | _ -> lprintf "ATTENTION: remote id coorespond to local client\n";
                                                            raise Not_found
                                                  end
                                                with _ -> 
                                                  (*if remote client is not in server's client list*)
                                                  (*lprintf "Remote Client %s dos'nt exist here\n" (Ip.to_string source.LN.source_ip);/*)
                                                  if not source.LN.add then
                                                    lprintf "WARNING: supp a new file\n";
                                                  let new_id = get_client_id source.LN.source_ip in
                                                    (*lprintf "New client Remote client %s\n" (Ip.to_string new_id);*)
                                                    Hashtbl.add s.server_clients source.LN.source_id new_id;
                                                    let c = {
                                                      remote_client_local_id = new_id; 
                                                      remote_client_remote_id = source.LN.source_id;
                                                      remote_client_server = s.server_id;
                                                      remote_client_md4 = Md4.null;
                                                      remote_client_kind = ( if source.LN.source_port = 0 then
                                                                               Firewalled_client
                                                                             else
                                                                               KnownLocation (source.LN.source_ip,source.LN.source_port)
                                                                           );
                                                      remote_client_files = [md4];
                                                    } in
                                                      Hashtbl.add clients_by_id new_id (RemoteClient c);
                                                      incr nremote_clients;
                                                      notifs := (true,{
                                                                   loc_ip = new_id;
                                                                   loc_port = source.LN.source_port;
                                                                   loc_expired = 0.0;
                                                                   loc_local = false;
                                                                 }) :: !notifs;
                                                      (*lprintf "finish add\n" *)
                                            ) sources_list;
                                  ServerLocate.notifications md4 !notifs
                            ) t.LN.notifs;

               nb_notifs :=  !nb_notifs + t.LN.nb_notifs;
               nb_info := !nb_info + (List.length !unkown_md4);
               
               lprintf "Process and apply %d notif\n" !count; 

               (*Ack for this notifications*)
               if (200 > (List.length s.server_notifications)) then
                 direct_group_send sock (AckNotifReq s.server_last_message_received_id);
               
               (*Ask for md4 definition*)
               (match !unkown_md4 with
                    [] -> ()
                  | _ ->
                      direct_group_send sock (QueryFileInfoReq !unkown_md4)
                   );
               
        (*ServerLocate.print()*)
          end
        else
          lprintf "Message %d already received with %d notif\n" t.LN.message_id nbr_notif
        
    | QueryFileInfoReq t ->
        let module QI = QueryFileInfoReply in
        let reply = List.map (fun md4 ->
                                let tags = ServerIndexer.get_def md4 in
                                  { QI.md4 = md4;
                                    QI.tags = tags.f_tags;
                                  }
                             ) t in
          direct_group_send sock (QueryFileInfoReplyReq reply)
        
    | QueryFileInfoReplyReq t ->
        let module QI = QueryFileInfoReply in
          List.iter (fun info ->
                       ServerIndexer.add {
                         f_md4 = info.QI.md4;
                         f_ip = Ip.null;
                         f_port = 0;
                         f_tags = info.QI.tags
                       }
                    ) t

                       
    | QueryUserConnectReq t ->
        begin
          try
            let module QU = QueryUserConnect in
            let c = Hashtbl.find clients_by_id t.QU.local_client_id in
              match c with
                  LocalClient c ->
                    (match c.client_sock with 
                         None -> ()
                       | Some sock -> 
                           let module QI = DonkeyProtoServer.QueryIDReply in
                             direct_server_send sock (DonkeyProtoServer.QueryIDReplyReq {
                                                        QI.ip = t.QU.client_ip;
                                                        QI.port = t.QU.client_port;
                                                      }))
                | _ -> raise Not_found
          with _ -> lprintf "WARNING: Fail in connect firewalled not implemented\n";
            ()
        end
        
    | MessageReq t ->
        if s.server_master then
          lprintf "From Server Master:\n"
        else
          lprintf "From Basic Server %d :\n" s.server_id;
        M.Message.print t


  
    | QuitReq ->
        close sock "Removed of the group";
        remove_server s.server_id;
        decr nconnected_servers;
        let module SS = ServerSupp in
          broadcast_to_group s.server_id (ServerSuppReq {
                                SS.group_id = !group_id;
                                SS.server_id = s.server_id;
                              }
                             )
          

    | _ -> lprintf "Unknown TCP requete in server_group from %d\n" s.server_id;
           ();
    lprint_newline() 
   
     
let connect_server s = 
  try
    lprintf "CONNECTING TO ONE SERVER %s:%d\n" (Ip.to_string s.server_ip) (s.server_port+5) ; 
    (*connection_try s.server_connection_control;*)
    incr nconnected_servers; 
    let sock = TcpBufferedSocket.connect 
          "server to server"
        (Ip.to_inet_addr s.server_ip) (s.server_port+5) 
          (server_handler s) (* DonkeyProtoCom.server_msg_to_string*)  in
      (*set_server_state s Connecting;*)
      (*set_read_controler sock download_control;
      set_write_controler sock upload_control;*)
      
      set_reader sock (DonkeyProtoCom.cut_messages ServerMessages.parse
          (server_to_server s));
      if !!relais_master then
        set_closer sock (server_disconnect_of_master s) 
      else
        set_closer sock (server_disconnect s);
      set_rtimeout sock 3600.;
      set_handler sock (BASIC_EVENT RTIMEOUT) (fun s ->
                                                 lprintf "Standart rtimeout";
                                                 close s "timeout"  
                                              );
      
      
      s.server_sock <- Some sock;


    with _ -> 
      lprintf "%s:%d IMMEDIAT DISCONNECT\n"
      (Ip.to_string s.server_ip) s.server_port;
(*      lprintf "DISCONNECTED IMMEDIATLY"; lprint_newline (); *)
        decr nconnected_servers;
        s.server_sock <- None;
        raise CantConnect
        (*set_server_state s NotConnected;*)
        (*connection_failed s.server_connection_control*)

let join_a_group ip port =

  let server = {
    server_group_id = Md4.null;
    server_master = false ;
    server_id = 0;
    server_md4 = Md4.null;
    server_ip = ip;
    server_port =  port;
    server_last_message_send = (0,0);
    server_last_message_received_id = -1 ;
    server_last_message_received_time = 0.;
    server_sock = None;
    server_need_recovery = false;
    server_notifications = [];
    server_clients = Hashtbl.create 100;
    server_tags = [];
  } in
    
    try
      connect_server server;
      
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
      lprintf "Can't open socket to %s:%d\n" (Ip.to_string ip) port
      (*Hashtbl.remove servers_by_id 0*)

let connect_a_group () = 
 List.iter (fun (ip,port) -> 
           join_a_group ip port
)!!known_master

let disconnect () = 
  Hashtbl.iter (fun id s ->
       if s.server_master then
          send_to s (QuitReq);
       remove_server id
               ) servers_by_id;
  group_id := Md4.null;
  server_id := 0

  
let handler t event =
  lprintf "Server CONNECTION"; lprint_newline ();
  match event with
    TcpServerSocket.CONNECTION (s, Unix.ADDR_INET (from_ip, from_port)) ->

      if !!max_servers <= !nconnected_servers then
      begin
          lprintf "too many servers\n";
          Unix.close s
      end
      else
      let sock = TcpBufferedSocket.create "server server connection" s (fun _ _ -> ()) 
        (*server_msg_to_string*)
        in
      
      let ip = Ip.of_inet_addr from_ip in
      let server = {
        server_group_id = Md4.null;
        server_master = false;
        server_id = 0;
        server_md4 = Md4.null;
        server_ip = ip;
        server_port = from_port;
        server_need_recovery = false;
        server_last_message_send = (0,0);
        server_last_message_received_id = -1 ;
        server_last_message_received_time = 0.;
        server_sock = Some sock;
        server_notifications = [];
        server_clients = Hashtbl.create 100; 
        server_tags = []; 
        } in

      incr nconnected_servers;

      TcpBufferedSocket.set_reader sock (
        DonkeyProtoCom.cut_messages ServerMessages.parse (server_to_server server));
      if !!relais_master then
        TcpBufferedSocket.set_closer sock 
          (server_disconnect_of_master server)
      else
        TcpBufferedSocket.set_closer sock 
          (server_disconnect server);
      set_rtimeout sock 3600.;
      set_handler sock (BASIC_EVENT RTIMEOUT) (fun s ->
                                                 lprintf "Standart rtimeout";
                                                 close s "timeout"  
                                              );
  | _ -> 
      lprintf "???"; lprint_newline ();
      ()  



let action_notify_servers time =
  (*lprintf "+++++++++++++++\nNotify Location Process\n";*)
  Hashtbl.iter (fun id s ->
                  (*lprintf "Send Notif to %d\n" id; *)
                  match s.server_sock with
                      None -> lprintf "CAN'T NOTIF CAUSE NO SOCKET";
                    | Some sock -> 
                        if s.server_need_recovery then
                          begin 
                            get_local_sources s;
                            let module R = M.Recovery in
                              send_to s (RecoveryReq 
                                           {
                                             R.group_id = !group_id;
                                             R.server_id = !server_id;
                                             R.server_ip = !!server_ip;
                                             R.server_port = !!server_port;
                                           });
                              s.server_need_recovery <- false;
                          end
                        else 
                          if ((List.length s.server_notifications) > 150) then
                        (*lprintf "Server send notif to server %d \n" id;*)
                            send_notification s sock;
               ) servers_by_id
    
    
let action_connect_servers time =
  (*lprintf "+++++++++++++++\nConnection Process\n"; *)
    List.iter (fun s_id ->
                 try
                   let s = Hashtbl.find servers_by_id s_id in
                   (*lprintf "->Try to connect to %d %s:%d\n" s.server_id (Ip.to_string s.server_ip) s.server_port; *)
                   connect_server s; 
                   (*lprintf "-> connection process\n";*)
                   let module CG = ConnectByGroup in
                     send_to s (ConnectByGroupReq 
                                  {
                                    CG.group_id = !group_id; 
                                    CG.server_id = !server_id;
                                    CG.server_ip = !!server_ip;
                                    CG.server_port = !!server_port; 
                                  });
                 with _ ->
                     lprintf "CAN't direct open a socket to server %d\n" s_id;
              ) !to_connect;
    to_connect := [];
    (*print_servers();*)
    ()
      
      
let bprint_clients_info buf option =
 Hashtbl.iter (fun id c ->
      match c with
      LocalClient c ->
        begin
           Printf.bprintf buf "LocalClient: %s\n" (Ip.to_string id);
           match option with
           _ -> ()
        end
      | RemoteClient c ->
        begin 
           Printf.bprintf buf "RemoteClient: %s\n" (Ip.to_string id);
            match option with
           _ -> ()
        end
        ) clients_by_id
                    

let bprint_server_info buf option =
  Hashtbl.iter (fun id s ->
                  Printf.bprintf buf "Server %d\n" id;
                  Printf.bprintf buf "->Number of clients: %d\n" (let nb_clients = ref 0 in 
                                                                    Hashtbl.iter (fun _ _ ->
                                                                                    incr nb_clients
                                                                                 ) s.server_clients;
                                                                    !nb_clients);
                  (*Hashtbl.iter (fun r_id l_id ->
                                  Printf.bprintf buf "%s correspond to %s\n" (Ip.to_string r_id) (Ip.to_string l_id)	     
                               ) s.server_clients;*)
                  Printf.bprintf buf "->Size of notif: %d\n" (List.length s.server_notifications);
                  (*Printf.bprintf buf "->Nb clients %d\n" (Hashtbl.length s.server_clients)*)
               ) servers_by_id
                        
