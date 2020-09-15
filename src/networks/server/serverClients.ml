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

open Md4
open CommonGlobals
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
open DonkeyProtoCom
open ServerTypes  
open ServerOptions        
open ServerGlobals
open ServerLog
open ServerSubscriptions

module M = DonkeyProtoServer
  
let null_ip = Ip.null

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

module P = DonkeyProtoServer

let print_loc loc =  lprintf("localisation %s port %d valide to %f")
                     (Ip.to_string loc.loc_ip)
                     loc.loc_port
                     loc.loc_expired;
                     lprint_newline()

let print_files_shared files = 
        List.iter (fun md4 -> 
                     try
                       let tags = ServerIndexer.get_def md4 in
                         print_tags tags.f_tags
                     with _ ->
                       lprintf " NO file def for %s\n" (Md4.to_string md4)
                                    
                  ) files


let rec print_client_stat c option =
        lprintf("Client %s ") (Ip.to_string c.client_id);
        match option with 
            'a' ->  print_loc c.client_location;
              print_files_shared c.client_files
          | 'l' ->  print_loc c.client_location
          | 'f' -> print_files_shared c.client_files
          | _ -> print_client_stat c 'a'

let print table =
        try
           lprint_newline();
           lprintf("Liste des clients");
           lprint_newline();
           Hashtbl.iter (fun md4 c -> print_client_stat c 'l') table;
           lprintf("FIN liste des clients");
           lprint_newline()
        with _ -> lprintf " Liste des clients vide"; lprint_newline()
          
          
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
                         bprint_tags buf tags.f_tags
                     with _ ->
                       Printf.bprintf buf " NO file def for %s\n" (Md4.to_string md4)
                  ) files

let bprint_nb_files_shared buf files =
  let nb = List.length files in
    Printf.bprintf buf "Number of files shared :%d\n" nb
 
          
let rec bprint_client_stat buf c option =
        Printf.bprintf buf ("Client %s\n") (Ip.to_string c.client_id);
        match option with 
            "a" ->  bprint_loc buf c.client_location;
              bprint_files_shared buf c.client_files
        | "l" ->  bprint_loc buf c.client_location
        | "f" -> bprint_files_shared buf c.client_files
        | "i" -> bprint_nb_files_shared buf c.client_files
        | _ -> begin 
            bprint_loc buf c.client_location;
            bprint_files_shared buf c.client_files
              end

let rec bprint_remote_client_stat buf c option =
  Printf.bprintf buf ("Client %s ") (Ip.to_string c.remote_client_local_id);
  Printf.bprintf buf ("correspond to %s on server %d\n") (Ip.to_string c.remote_client_remote_id) c.remote_client_server;
  bprint_nb_files_shared buf c.remote_client_files
    (*match option with 
        "a" ->  bprint_loc buf c.client_location;
          bprint_files_shared buf c.client_files
      | "l" ->  bprint_loc buf c.client_location
      | "f" -> bprint_files_shared buf c.client_files
      | "i" -> bprint_nb_files_shared buf c.client_files
      | _ -> begin 
          bprint_loc buf c.client_location;
          bprint_files_shared buf c.client_files
        end*)

let bprint buf table arg=
  Printf.bprintf buf ("Liste des clients\n");
  Hashtbl.iter (fun md4 c -> 
                  match c with
                      LocalClient c ->
                        Printf.bprintf buf "LocalClient\n";
                        bprint_client_stat buf c arg
                    | RemoteClient c -> Printf.bprintf buf "RemoteClient\n";
                        bprint_remote_client_stat buf c arg
               ) table
      


   

exception Already_use

let get_client_num () =
  incr client_counter;
  try
    while true do
      if not (Hashtbl.mem clients_by_id (
            Ip.of_int32 (Int32.of_int !client_counter))) then
        if !client_counter > 200000 then client_counter := 1 else
          incr client_counter
    done
  with _ -> !client_counter
        
let get_client_id ip =
  if Hashtbl.mem clients_by_id ip then
    get_client_num ()
  else ip
    

let reply_to_client_connection c =
  (match c.client_sock with
      None -> ()
    | Some sock ->
        (match c.client_kind with
            Firewalled_client ->
              begin
                try
                  if c.client_files = [] then
                    begin
                      Hashtbl.remove clients_by_id c.client_id;
                      c.client_id <- (get_client_num ());
                      Hashtbl.add clients_by_id c.client_id (LocalClient c);
                    end;
                  c.client_location.loc_ip <- c.client_id;
                  c.client_location.loc_port <- 0;
                     
                with _ -> lprintf "fuck nico\n"
              end
          | KnownLocation (ip,port) ->
              begin
                c.client_location.loc_ip <- ip;
                c.client_location.loc_port <- port
              end);
        direct_server_send sock  (M.SetIDReq c.client_id)
  )
                (*lprintf ("Nouvel identifiant %s") (Ip.to_string id);
                  lprint_newline()*)

      (*Hashtbl.add clients_by_id id (LocalClient c);*)

      (*print clients_by_id;*)
      
      (*lprintf "SET ID"; lprint_newline ();*)
(* send ID back to client *)
      

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
        (*lprintf "CAN WRITE"; lprint_newline ();*)
        c.client_kind <- KnownLocation (c.client_conn_ip, port);
        reply_to_client_connection c 
    | BASIC_EVENT (CLOSED s) ->
        ok := true;
        (*lprintf "CLOSED %s" s; lprint_newline ();*)
        c.client_kind <- Firewalled_client;
        reply_to_client_connection c 
    | _ ->
        TcpBufferedSocket.close sock "connect ok";
        ok := true;
        (*lprintf "ERROR IN CONNECT"; lprint_newline ();*)
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
  (*lprintf "Checking client ID"; lprint_newline ();*)
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
    let loc = {
      loc_ip=c.client_id;
      loc_port=c.client_location.loc_port;
      loc_expired=0.0;
      loc_local=true;
    } in
      List.iter (fun md4 -> 
                   ServerLocate.supp md4 loc;
                   if !!relais_cooperation_protocol then
                     ServerServer.supp_source_to_notify c md4 
                ) c.client_files;
      c.client_files <- []
  with _->
    lprintf "Exception during remove source\n"
  

(* send the number of clients and the number of files on the server *)
let send_stat_to_clients timer =
        Hashtbl.iter ( fun k x ->
                           match x with 
                           LocalClient x ->
                             (match x.client_sock with
                              None -> ()
                              | Some sock ->
                              direct_server_send  sock (M.InfoReq
                                (*(!nconnected_clients,!nshared_md4)*)
                                                       (243,1000)))
                           | RemoteClient x ->
                              ()
                     ) clients_by_id
                          

let rec check_and_remove lst md4 =
  match lst with 
      [] -> raise Not_found
    | hd :: tl -> (if md4 = hd then
                     tl 
                   else
                     hd :: check_and_remove tl md4;)
        
let rec print_liste lst = 
  match lst with 
      [] -> ()
    | hd :: tl -> lprintf " el: %s\n" (Md4.to_string hd);
        print_liste tl
          
let rec print_liste_2 lst = 
   match lst with 
    [] -> ()
  | hd :: tl -> lprintf " el: %s\n" (Md4.to_string hd.f_md4);
      print_liste_2 tl

let client_is_mldonkey2 c =
  let md4 = Md4.direct_to_string c.client_md4 in
    md4.[5] = 'M' && md4.[14] = 'L'

(*message comming from a client*)
let server_to_client c t sock =
  (*lprintf "server_to_client"; lprint_newline ();
  M.print t;
  lprint_newline ();*)
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
        (*ServerLocate.print();*)

        let tmp = c.client_files in

        let removed_files = ref tmp in
        let added_files = ref [] in
        let current_files = ref [] in

        (*let insert_unique e l =
          if (List.memq e l) then
            l
          else (e :: l) in*)
          
          List.iter ( fun file ->
                        try
                          removed_files := check_and_remove !removed_files file.f_md4; 
                          current_files := file.f_md4 :: !current_files;
                        with Not_found ->
                          if not (List.exists (fun old_file -> file.f_md4 = old_file.f_md4) !added_files) then
                            added_files := file :: !added_files;
                    ) list;
          (*
          lprintf "remove file\n";
          print_liste !removed_files;

          lprintf "From %s\n" (Ip.to_string c.client_id);
          lprintf "\nadd file\n";
          List.iter (fun file -> lprintf " %s" (Md4.to_string file.f_md4)) !added_files;
          lprintf "\nlast file\n";
          print_liste c.client_files;

          List.iter (fun file ->
                       let count = ref 0 in
                         List.iter (fun im ->
                                      if im.f_md4 = file.f_md4 then
                                        incr count;
                                      if !count > 1 then
                                        lprintf "Quel con!!\n";
                                   ) !added_files
                    ) !added_files;
          

          lprintf "\ncurrent file\n";
          print_liste !current_files;
          lprintf "\n";*)
          
          c.client_files <- !removed_files;
          remove_md4_source c;           
          c.client_files <- !current_files;
          
          begin
            try
              List.iter (fun tmp ->
                           ServerIndexer.add tmp; 
                           ServerLocate.add tmp.f_md4
                             {
                               loc_ip=c.client_id;
                               loc_port=c.client_location.loc_port;
                               loc_expired=c.client_location.loc_expired;
                               loc_local = true;
                             };
                         c.client_files <- tmp.f_md4 :: c.client_files;
                         if !!relais_cooperation_protocol then
                           begin
                             (*lprintf "Notif add to list\n";*)
                             ServerServer.add_source_to_notify c tmp.f_md4; 
                           end
                        ) !added_files;
            with _ -> (*lprintf "Too Many Files Shared\n"*)
              direct_server_send sock (M.MessageReq (String.concat "" ["Too many files index in my server, so I can index ";(string_of_int (List.length c.client_files));" of your files"]))
          end;
          
          (* Update list of known tag name *)
          (*List.iter (fun file ->
                       List.iter (fun tag ->
                                    tag_name_list := insert_unique tag.tag_name !tag_name_list)
                       file.f_tags)
            !added_files;*)
          
          (* SUBSCRIPTION : try to notify for each new document *)
          List.iter (fun file ->
                       (* Notify for this file *)
                       Subs.notify (Subs.cook_field_list file.f_tags)
                       (Subs.cook_pred_list file.f_tags);
                       
                       (* Collect matching notification *)
                       List.iter (fun (num, ip) ->
                                    (try
                                       let client = (Hashtbl.find clients_by_id ip) in
                                         (match client with
                                              LocalClient c ->
                                                (match c.client_sock with
                                                     Some s ->
                                                       direct_server_send 
                                                       s
                                                       (M.Mldonkey_NotificationReq 
                                                          (num, [file]))
                                                   | None ->
                                                       lprintf 
                                                       "Client not connected\n")
                                            | RemoteClient _ -> ())
                                     with Not_found -> lprintf "No client in ht\n"))
                         !subs_match;

                       (* Clean matching list *)
                       subs_match := [])
            !added_files;
          (*ServerLocate.print()*)
          ()

    
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
                      lprintf "QueryIDReq on local client can't return reply"; 
                      lprint_newline ();
                      raise Not_found)
            | RemoteClient cc ->
                (match cc.remote_client_kind, sock, c.client_kind with
                   | KnownLocation (ip, port), sock, _
                       ->
                       let module QI = M.QueryIDReply in
                         direct_server_send sock (M.QueryIDReplyReq {
                                                    QI.ip = ip;
                                                    QI.port = port;
                                                  })
                   | Firewalled_client, _, KnownLocation (ip, port) 
                       ->
                       let s = Hashtbl.find servers_by_id cc.remote_client_server in
                       let module QU = ServerMessages.QueryUserConnect in
                         ServerServer.send_to s ( ServerMessages.QueryUserConnectReq {
                                                    QU.local_client_id = cc.remote_client_remote_id;
                                                    QU.client_ip = ip;
                                                    QU.client_port = port;
                                                  })
                   | _ ->
                       lprintf "QueryIDReq on remote client can't return reply"; 
                       raise Not_found)
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
          if c.client_mldonkey = 0 || c.client_mldonkey = 1 then
            if c.client_mldonkey = 0 && t = Md4.null then
              (* c.client_mldonkey = 0 ==> First location query *)
              c.client_mldonkey <- 1
            else
              (* c.client_mldonkey = 1 ==> Might be mldonkey client *)
              if c.client_mldonkey = 1 && t = Md4.one then 
                begin
                  c.client_mldonkey <- 2;
(*		  direct_server_send sock (M.Mldonkey_MldonkeyUserReplyReq) *)
                end
              else
                if client_is_mldonkey2 c then
                begin
                  c.client_mldonkey <- 3;
                  direct_server_send sock (M.Mldonkey_MldonkeyUserReplyReq)
                end
              else
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

    | M.Mldonkey_SubscribeReq (num, lifetime, t) ->
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

        (* Communication is made via a global variable : NO GOOD.
           Maybe I can avoid this crap with optional parameter function *)
        let q = Subs.translate_query t and
          lifetime = float_of_int lifetime in
          if (lifetime <= !!max_subs_lifetime) then
            begin
              lprintf "New subs (%s, %d) with %f seconds lifetime"
                (Md4.to_string c.client_md4) num lifetime;
              lprint_newline ();
              Subs.add_subscription (c.client_md4, num) lifetime q 
                (fun _ -> subs_match := (num, c.client_id) :: !subs_match)
            end
          else
            begin
              lprintf "%f seconds too long subs lifetime for me"
                lifetime;
              lprint_newline ()
            end
    

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
        Subs.remove_subs_id (c.client_md4, num)
            
    | _ -> (*lprintf "UNKNOWN TCP REQ\n";*) 
      ());
  if (!!save_log) then
     ServerLog.add_to_liste ();
   ()
      
(* every minute, send a InfoReq message *)

let remove_client c sock s = 
(*  lprintf ("CLIENT DISCONNECTED %s")
  (Ip.to_string c.client_id);
  lprint_newline ();*)
  try 

    decr nconnected_clients;
    (* remove all files shared by this clients *)
    let t =  (Hashtbl.find clients_by_id c.client_id) in
      Hashtbl.remove clients_by_id c.client_id;
      
      
      remove_md4_source c;
      
      TcpBufferedSocket.close sock "";
    
      c.client_sock <- None;
      if (!!save_log) then
        begin
          ServerLog.something_append  c.client_location.loc_ip c.client_md4 s;
          ServerLog.add_to_liste () 
        end;
      
      (*remave client of clients_dy_id*)
      
      (*rverLocate.print();*)

      (*Pervasives.flush Pervasives.stdout;*)
      (*remove all subscription made by this cient*)
      List.iter (fun n ->
                   Hashtbl.remove notifications (c.client_md4, n.notif_num)
                ) c.client_subscriptions;
      c.client_subscriptions <- [];
      
  with _ -> 
    lprintf "Exception in remove client"
    
      
let handler t event =
  (*lprintf "Client CONNECTION"; lprint_newline ();*) 
  match event with
    TcpServerSocket.CONNECTION (s, Unix.ADDR_INET (from_ip, from_port)) ->

      if !!max_clients <= !nconnected_clients  then
        begin
          (*lprintf "too many clients\n";*)
          Unix.close s
        end
      else
      let sock = TcpBufferedSocket.create "server client connection" s (fun _ _ -> ()) 
        (*server_msg_to_string*)
        in
      
      let ip = Ip.of_inet_addr from_ip in
      let client = {
          client_id = get_client_id ip;
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
            loc_local = true;
          };
          client_results = { docs = [||]; next_doc = 0; };
          client_subscriptions = [];
        } in

      

      TcpBufferedSocket.set_reader sock (
        DonkeyProtoCom.cut_messages DonkeyProtoServer.parse (server_to_client client));
      TcpBufferedSocket.set_closer sock 
        (remove_client client);
      
      incr nconnected_clients;
      Hashtbl.add clients_by_id client.client_id (LocalClient client)

  | _ -> 
      lprintf "???"; lprint_newline ();
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
