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
open Md4
open Options
open BasicSocket
open TcpBufferedSocket

open CommonInteractive
open CommonUser
open CommonSearch
open CommonComplexOptions
open CommonServer
open CommonOptions
open CommonTypes  
open CommonOptions
open CommonGlobals
open CommonSources

open DonkeyMftp
open DonkeyImport
open DonkeyProtoCom
open DonkeyTypes
open DonkeyOptions
open DonkeyComplexOptions
open DonkeyGlobals


module Udp = DonkeyProtoUdp

(* Constants *)
let udp_requests_new  = 5
let udp_requests_old  = 3
let udp_requests_wait = 60
let udp_max_ask       = 3

(*
 * Functions to handle TCP Queries for Sources to Server
 *
 * add_query_location (file) (server) -> add file as head to Queue of server
 * fill_query_queue (server)          -> fill Queue with files to request
 * get_query_files (server) (number)  -> strip (number) files from Queue 
 *                                       and return them
 *)

let add_query_location file s =
  s.server_waiting_queries <- file :: s.server_waiting_queries


let fill_query_queue server =
  let downloading = List.filter ( fun f ->
    file_state f = FileDownloading
  ) !current_files in

  (* shuffle list so we don't ask all Servers for the same file at once *)
  let rand_prio = List.map (fun f -> 
    file_priority f + Random.int 100
  ) downloading in

  let joined = List.combine downloading rand_prio in
  let queries, _ = List.split ( 
    List.sort (
      fun (_,p1) (_,p2) -> p2 - p1
    ) joined
  ) in

  server.server_waiting_queries <- queries;;


let get_query_files server number =
  let rec iter n =
    if n>0 then
      begin
        match server.server_waiting_queries with
            [] -> 
              fill_query_queue server;
              []
              (* iter (mini n (List.length server.server_waiting_queries) ) *)
          | file :: files ->
              server.server_waiting_queries <- files;
              if files = [] then
                server.server_sent_all_queries <- true;
              
              if file_state file = FileDownloading
                && DonkeySources.need_new_sources file.file_sources
                then
                  file :: iter (n-1)
              else
                iter n
      end
    else []
  in
  iter number


(* called every 60 seconds *)
let query_locations_timer () =
  List.iter (
    fun server ->
      if server.server_queries_credit <= 0 then
        begin
          do_if_connected server.server_sock (
            fun sock ->
              List.iter (
                fun file -> 
                  if !verbose_location then
                    lprintf "donkeyServers: TCP: Query Location of %s\n" 
                      (file_best_name file);
                  let module M = DonkeyProtoServer in
                  server_send sock ( M.QueryLocationReq file.file_md4  ) 
              ) (get_query_files server files_queries_per_minute);
          )
        end
      else 
        server.server_queries_credit <- server.server_queries_credit - 1 

  )(connected_servers());;


(* 
 *
 * Function to handle UDP Queries for Sources to Server
 *
 *)
let udp_query_sources () =

  let old_servers = ref [] in
  let new_servers = ref [] in
  let nservers = ref 0 in
  while !nservers < udp_max_ask (* ask only (udp_max_ask) servers *)
    && (match !udp_servers_list with
            [] -> 
              udp_servers_list := Hashtbl2.to_list servers_by_key;
              false
          | s :: tail -> 
              udp_servers_list := tail;
              (match s.server_sock with
                   Connection _ -> ()
                 | _ -> 
                     if connection_last_conn s.server_connection_control + 3600*1 
                       > last_time () &&
                       s.server_next_udp <= last_time () then 
                         begin
                           if server_accept_multiple_getsources s then
                             new_servers := s :: !new_servers
                           else
                             old_servers := s :: !old_servers;
                           
                           incr nservers;
                         end
              );
              true 
       )
  do
    ()
  done;
  if !verbose_location then
    lprintf "donkeyServers: udp_query_source to: %2d old %2d new servers\n" 
      (List.length !old_servers) (List.length !new_servers);

  try
    (* query "new servers" ie servers which understand multiple filerequests *)
    List.iter (
      fun s ->
        let md4s = List.map (
          fun file -> 
            file.file_md4; 
        ) (get_query_files s udp_requests_new)
        in 
        udp_server_send s (Udp.QueryLocationUdpReq md4s);
        s.server_next_udp <- last_time () + udp_requests_wait;
    ) !new_servers;
    
    (* query "old servers", they need one packet per request *)
    List.iter (
      fun s -> 
        let list = get_query_files s udp_requests_old in
        List.iter ( 
          fun file -> 
            udp_server_send s (Udp.QueryLocationUdpReq [file.file_md4]);
            s.server_next_udp <- last_time () + udp_requests_wait
        ) list;
    ) !old_servers;

  with e ->
    lprintf "udp_query_sources: %s\n" (Printexc2.to_string e)


      

let disconnect_server s reason =
  match s.server_sock with
      NoConnection -> ()
    | ConnectionWaiting token ->
        decr nservers;
        cancel_token token;
        s.server_sock <- NoConnection
    | Connection sock ->
        decr nservers;
        TcpBufferedSocket.close sock reason;
        (*
          lprintf "%s:%d CLOSED received by server\n"
          (Ip.to_string s.server_ip) s.server_port; 
        *)
        connection_failed (s.server_connection_control);
        s.server_sock <- NoConnection;
        s.server_score <- s.server_score - 1;
        s.server_users <- [];
        set_server_state s (NotConnected (reason, -1));
        s.server_master <- false;
        (match !DonkeyGlobals.master_server with
          | Some ss when s == ss ->
              DonkeyGlobals.master_server := None;
          | _ -> ());
        s.server_banner <- "";
        s.server_sent_all_queries <- false;
        remove_connected_server s


let server_handler s sock event = 
  match event with
      BASIC_EVENT (CLOSED r) ->
        disconnect_server s r
    | BASIC_EVENT (LTIMEOUT | RTIMEOUT) ->
        close sock Closed_for_timeout
    | _ -> ()




      
let last_message_sender = ref (-1)
      
let client_to_server s t sock =
  let module M = DonkeyProtoServer in
  
  s.server_last_message <- last_time ();
  
  if !verbose_msg_servers then begin
    lprintf "Message from server:\n"; 
    DonkeyProtoServer.print t; lprintf "\n"
  end;
  
  match t with
      M.SetIDReq t ->
        s.server_has_zlib <- t.M.SetID.zlib;
        if low_id t.M.SetID.ip && !!force_high_id then
          disconnect_server s (Closed_for_error "Low ID")
        else begin
          s.server_cid <- Some t.M.SetID.ip;
	  s.server_realport <- t.M.SetID.port;
          (* disconnect after (connected_server_timeout) seconds of silence *)
          set_rtimeout sock !!connected_server_timeout; 
          set_server_state s Connected_initiating;
          s.server_score <- s.server_score + 5;
          connection_ok (s.server_connection_control);
          
          server_send sock (
            let module A = M.AckID in
            M.AckIDReq A.t
          );
          
          if not (low_id t.M.SetID.ip) && !!use_server_ip then
            last_high_id := t.M.SetID.ip;
        end
  
  | M.MessageReq msg ->
      if !last_message_sender <> server_num s then begin
          let server_header = Printf.sprintf "\n+-- From server %s [%s:%d] ------\n"
              s.server_name (Ip.to_string s.server_ip) s.server_port in
          CommonEvent.add_event (Console_message_event server_header);
          last_message_sender := server_num s
        end;
      s.server_banner <- s.server_banner ^ Printf.sprintf "%s\n" msg;
      let msg = Printf.sprintf "| %s\n" msg in
      CommonEvent.add_event (Console_message_event msg)
  
  | M.ServerListReq l ->
      if !verbose then lprintf "donkeyServers: Received serverlist\n";
      if !!update_server_list then
        let module Q = M.ServerList in
        List.iter (fun s ->
            safe_add_server s.Q.ip s.Q.port
        ) l
  
  | M.ServerInfoReq t ->
      s.server_score <- s.server_score + 1;
      s.server_tags <- t.M.ServerInfo.tags;
      List.iter (
        fun tag ->
          match tag with
              { tag_name = Field_UNKNOWN "name"; tag_value = String name } -> 
                s.server_name <- name
            | { tag_name = Field_UNKNOWN "description"; tag_value = String desc } ->
                s.server_description <- desc
            | _ -> ()
      ) s.server_tags;
      printf_char 'S';

      (* nice and ugly, but it doesn't require any new fields *)
      set_server_state s (Connected 
          ( match s.server_cid with
              Some t -> (match Ip.to_ints t with
                          | _, _, _, 0 -> (-1)
                          | _ -> (-2))
              | _ -> (-1)
          )
        );

      (* fill list with queries for the server *)
      fill_query_queue s;
      s.server_sent_all_queries <- false;
      s.server_queries_credit <-  0

  | M.InfoReq (users, files) ->
      s.server_nusers <- Int64.of_int users;
      s.server_nfiles <- Int64.of_int files;
      if (users < !!min_users_on_server) then
        begin
          lprintf "%s:%d remove server min_users_on_server limit hit!\n"
            (Ip.to_string s.server_ip) s.server_port;
          
          disconnect_server s Closed_for_timeout;
          server_remove (as_server s.server_server);
        end;
      server_must_update s
  
  | M.QueryIDReplyReq t -> 
      (* This can either be a reply to a QueryID or a indirect request for
         connection from another client. In this case, we should immediatly 
         connect. *)
      if !verbose then lprintf "donkeyServers: QueryIDReplyReq: received\n";
      let module Q = M.QueryIDReply in
      if Ip.valid t.Q.ip && ip_reachable t.Q.ip then begin
        try
          match Fifo.take s.server_id_requests with
              None -> raise Not_found
            | Some file ->
                if !verbose then
                  lprintf "donkeyServers: QueryIDReplyReq: This was a QueryID reply !?\n";
                let s = DonkeySources.find_source_by_uid 
                  (Direct_address (t.Q.ip, t.Q.port)) in
                DonkeySources.set_request_result s file.file_sources 
                  File_new_source
        with _ ->
          if !verbose then
            lprintf "donkeyServers: QueryIDReplyReq: Calling back to %s:%d\n"
              (Ip.to_string t.Q.ip) t.Q.port;
          let c = new_client (Direct_address (t.Q.ip, t.Q.port)) in
          DonkeyClient.reconnect_client c;
      end

  | M.QueryIDFailedReq t ->
      if !verbose then
        lprintf "donkeyServers: QueryIDFailedReq:\n";
      ignore (Fifo.take s.server_id_requests)
        
  | M.QueryReplyReq t ->
      let rec iter () =
        let search = try
          Fifo.take s.server_search_queries
        with _ -> failwith "No pending query"
        in
        try
          let nres = List.length t in
          if !xs_last_search = search.search_num && nres = 201 &&
            search.search_nresults < search.search_max_hits then
              begin
                server_send sock M.QueryMoreResultsReq;
                Fifo.put s.server_search_queries search      
              end;
          DonkeyUdp.search_handler search t
        with Already_done -> iter ()
      in
      iter ()
    
  | M.Mldonkey_NotificationReq (num, t) ->
      let s = search_find num in
      List.iter (
        fun f ->
          DonkeyOneFile.search_found false s f.f_md4 f.f_tags
      ) t
  
  | M.QueryUsersReplyReq t ->
      let module M = DonkeyProtoServer in
      let module Q = M.QueryUsersReply in
      let add_to_friend = try
          Fifo.take s.server_users_queries
        with _ -> failwith "No pending query"
      in

(* We MUST found a way to keep indirect friends even after a deconnexion.
Add a connection num to server. Use Indirect_location (server_num, conn_num)
and remove clients whose server is deconnected. *)
(*          lprintf "QueryUsersReply\n";  *)
      List.iter (fun cl ->
          let rec user = {
              user_user = user_impl;
              user_md4 = cl.Q.md4;
              user_name = "";
              user_ip = cl.Q.ip;
              user_port = cl.Q.port;
              user_tags = cl.Q.tags;
              user_server = s;                  
            } 
          and  user_impl = {
              dummy_user_impl with
              impl_user_val = user;
              impl_user_ops = user_ops;
            }
          in
          user_add user_impl;
          List.iter (fun tag ->
              match tag with
                { tag_name = Field_UNKNOWN "name"; tag_value = String s } -> 
                  user.user_name <- s
              | _ -> ()
          ) user.user_tags;
          
          if add_to_friend then DonkeyUdp.add_user_friend s user;
          
          s.server_users <- user :: s.server_users;
          (* lprintf "SERVER NEW USER\n"; *)
          server_new_user (as_server s.server_server) (as_user user.user_user);
      ) t;
      server_must_update s
  
  | M.QueryLocationReplyReq t -> 
      DonkeyClient.query_locations_reply s t
      
  | _ -> 
      ()
      
let connect_server s =
  if !!enable_servers && can_open_connection connection_manager then
    match s.server_sock with
    | NoConnection ->
        incr nservers;
        let token = add_pending_connection connection_manager (fun token ->
              decr nservers;
              s.server_sock <- NoConnection;
              try
                (* lprintf "CONNECTING ONE SERVER\n"; *)
                connection_try s.server_connection_control;
                printf_char 's'; 
                let sock = TcpBufferedSocket.connect token "donkey to server"
                    (Ip.to_inet_addr s.server_ip) s.server_port 
                    (server_handler s) (* DonkeyProtoCom.server_msg_to_string*)  in
                s.server_cid <- None (*client_ip (Some sock) *);
                set_server_state s Connecting;
                set_read_controler sock download_control;
                set_write_controler sock upload_control;
                
                set_reader sock (DonkeyProtoCom.cut_messages DonkeyProtoServer.parse
                    (client_to_server s));
                set_rtimeout sock !!server_connection_timeout;
                
                Fifo.clear s.server_id_requests;
                s.server_waiting_queries <- [];
                s.server_queries_credit <- 0;
                s.server_sock <- Connection sock;
                incr nservers;
                server_send sock (
                  let module M = DonkeyProtoServer in
                  let module C = M.Connect in
                  M.ConnectReq {
                    C.md4 = !!client_md4;
                    C.ip = client_ip (Some sock);
                    C.port = !!donkey_port;
                    C.tags = !client_to_server_tags;
                  }
                );
                add_connected_server s;
              with e -> 
                (*
                  lprintf "%s:%d IMMEDIAT DISCONNECT \n"
                  (Ip.to_string s.server_ip) s.server_port; 
                  lprintf "DISCONNECTED IMMEDIATLY\n";
                 *)
                disconnect_server s (Closed_for_exception e)
          )
          in 
          s.server_sock <- ConnectionWaiting token
      | _ -> ()        
        
let print_empty_list = ref true

(* [restart] prevents infinite looping when [servers_list] contains only
  uninteresting servers. *)
  
let rec connect_one_server restart =
  (*  lprintf "connect_one_server\n";  *)
  if can_open_connection connection_manager then
    match !servers_list with
	[] ->
          if restart then begin
            servers_list := [];
            Hashtbl.iter (fun _ s ->
                servers_list := s :: !servers_list
            ) servers_by_key;
            if !servers_list = [] then begin
              if !print_empty_list then begin
                print_empty_list := false;
                lprintf "Looks like you have no servers in your servers.ini\n";
                lprintf "You should either use the one provided with mldonkey\n";
                lprintf "or import one from the WEB\n";
              end;
                
              raise Not_found;
            end;

            (* sort the servers list so that last connected servers are 
               connected first (ie decreasing order of last connections)  *)
            servers_list := List.sort (fun s1 s2 ->
                compare 
                  (connection_last_conn s2.server_connection_control) 
                  (connection_last_conn s1.server_connection_control)
            ) !servers_list;
            
            connect_one_server false;
          end
      | s :: list ->
          servers_list := list;
          if connection_can_try s.server_connection_control then
            begin
              (* connect to server *)
              match s.server_sock with
                | NoConnection when s.server_score >= 0 -> 
                    connect_server s
                | _ -> 
                    connect_one_server restart
            end
          

let force_check_server_connections user =
  (*  lprintf "force_check_server_connections\n";  *)
  if user || !nservers < max_allowed_connected_servers ()  then 
    let rec iter n =
      try
      if n > 0 && can_open_connection connection_manager then begin
        connect_one_server true; (* raises Not_found if server_list is empty *)
        iter (n-1)
      end
      with Not_found -> ()
    in
    let num = ( if user
                  then !!max_connected_servers 
                else max_allowed_connected_servers () )  - !nservers 
    in
    iter num
      
    
let rec check_server_connections () =
  force_check_server_connections false


(* 
 * clean serverlist 
 *)
let remove_old_servers () =
  if !verbose then lprintf "REMOVE OLD SERVERS\n";  
  let t1 = Unix.gettimeofday () in
(*
The new tactic: we sort the servers (the more recently connected first,
the black-listed ones at the end), and we start checking from the last
position to the min_left_servers position.
*)

  let to_remove = ref [] in
  let to_keep = ref [] in
  Hashtbl.iter (fun _ s ->
      if not s.server_preferred &&
        (is_black_address s.server_ip s.server_port || s.server_port = 4662)
        then
          to_remove := s :: !to_remove
      else
        to_keep :=
          (connection_last_conn s.server_connection_control, s) :: !to_keep
  ) servers_by_key;
  let t2 = Unix.gettimeofday () in
  if !verbose then lprintf "Delay to detect black-listed servers: %2.2f\n" (t2 -. t1); 
  
  if List.length !to_keep > !!min_left_servers then begin
  let array = Array.of_list !to_keep in
  Array.sort (fun (ls1,_) (ls2,_) ->
      if ls1 = ls2 then 0 else if ls1 > ls2 then -1 else 1
  ) array;

  if !verbose then 
    for i = 0 to Array.length array - 1 do
      let ls, s = array.(i) in
      lprintf "server %d last_conn %d\n" (server_num s) ls;
      
    done;

  let min_last_conn =  last_time () - !!max_server_age * Date.day_in_secs in
  
  for i = Array.length array - 1 downto !!min_left_servers do
    let ls, s = array.(i) in
        if ls < min_last_conn && s.server_sock = NoConnection 
          && not s.server_preferred then begin
        if !verbose then begin
            lprintf "Server too old: %s:%d\n" 
              (Ip.to_string s.server_ip) s.server_port;
            
          end;
        to_remove := s :: !to_remove
      end
  done;
  end;
  let t3 = Unix.gettimeofday () in
  if !verbose then lprintf "Delay to detect old servers: %2.2f\n" (t3 -. t2); 

  List.iter (fun s ->
    remove_server s.server_ip s.server_port
  ) !to_remove;

  let t4 = Unix.gettimeofday () in
  if !verbose then lprintf "Delay to finally remove servers: %2.2f\n" (t4 -. t3);   
  if (List.length !to_remove) > 0 || !verbose then
    lprintf "Removed %d old edonkey servers.\n" (List.length !to_remove)

  
(* Keep connecting to servers in the background. Don't stay connected to 
  them , and don't send your shared files list *)
let walker_list = ref []
let delayed_list = ref []
let next_walker_start = ref 0
  
(* one call every 5 seconds, so 12/minute, 720/hour *)
let walker_timer () = 
  
  if !!servers_walking_period > 0 &&
    !nservers < max_allowed_connected_servers () + !!max_walker_servers then
      
      match !walker_list with
          [] ->
            if !delayed_list <> [] then 
              begin
                walker_list := !delayed_list;
                delayed_list := []
              end 
            else
              if last_time () > !next_walker_start then 
                begin
                  next_walker_start := 
                    last_time () + !!servers_walking_period * 3600;

                  Hashtbl.iter (
                    fun _ s ->
                      walker_list := s :: !walker_list
                  ) servers_by_key;

                end
        | s :: tail ->
            walker_list := tail;
            match s.server_sock with
                NoConnection -> 
                  if connection_can_try s.server_connection_control then
                    begin
                      if !verbose then
                        lprintf "WALKER: try connect %s\n"
                          (Ip.to_string s.server_ip);
                      connect_server s
                    end
                  else
                    begin
                      delayed_list := s :: !delayed_list;
                      if !verbose then
                        lprintf "WALKER: connect %s delayed\n"
                          (Ip.to_string s.server_ip);
                    end
              | _ -> ()
            
(* Keep connecting to servers in the background. Don't stay connected to 
  them , and don't send your shared files list *)
let udp_walker_list = ref []
let next_udp_walker_start = ref 0
  
(* one call every second, so 3600/hour, must wait one hour before
restarting 
Each client issues 1 packet/4hour, so 100000 clients means 25000/hour,
7 packets/second = 7 * 40 bytes = 280 B/s ...
*)
  
let udp_ping = String.create 6
  
let udp_walker_timer () = 
  match !udp_walker_list with
    [] ->
      if last_time () > !next_udp_walker_start then begin
          next_udp_walker_start := last_time () + 4*3600;
          Hashtbl.iter (fun _ s ->
              udp_walker_list := s :: !udp_walker_list
          ) servers_by_key;
        end
  | s :: tail ->
      udp_walker_list := tail;
      UdpSocket.write (get_udp_sock ())
                  true udp_ping s.server_ip (s.server_port + 4)

(* sort the servers by preferred first
   then users count with decreasing order
 *)
let compare_servers s2 s1 =
  let n = compare s1.server_preferred s2.server_preferred in
  if n = 0 then
    Int64.to_int (s1.server_nusers -- s2.server_nusers)
  else n
    
(* check connected servers *)
let update_master_servers _ =
  let server_list = List.sort compare_servers (connected_servers ()) in
  let masters = ref [] in
  List.iter (
    fun s ->
      if s.server_master then 
        match s.server_sock with
        | Connection _ -> 
            if !verbose then begin
                lprintf "MASTER: OLD MASTER %s\n" (Ip.to_string s.server_ip);
              end;
            masters := s :: !masters
        | _ -> s.server_master <- false
  ) server_list;
  let nmasters = ref (List.length !masters) in
  
  if !verbose then begin
      lprintf "MASTER: nmaster %d\n\n" !nmasters;
    end;
  
  let make_master s =
    (* normal servers don't have our SHARE, so send list if it becomes a master *)
    do_if_connected s.server_sock (fun sock ->
        if !verbose then begin
            lprintf "   MASTER: %s\n" (Ip.to_string s.server_ip); 
          end;
        s.server_master <- true;
        incr nmasters;
      
        (* Put the server in the list of servers, and update the list *)
        masters := s :: !masters;
        masters := List.rev (List.sort compare_servers !masters);
        
        server_send_share s.server_has_zlib sock
          (DonkeyShare.all_shared ())        
    )
  in
  
  let max_allowed_connected_servers = max_allowed_connected_servers () in
  let nconnected_servers = ref 0 in
  
  let disconnect_old_server s =
    (* disconnect a server we are connected to for too long if we have too many
       connections *)
    if !nconnected_servers > max_allowed_connected_servers then
      begin
        if !verbose then
            lprintf "MASTER: DISCONNECT %s\n" (Ip.to_string s.server_ip);
        nconnected_servers := !nconnected_servers - 3;
        do_if_connected  s.server_sock (fun sock ->
            (* We will disconnect from this server.
               Wait for 5 seconds to disconnect. *)
            set_lifetime sock 5.);
      end
  in
  
  List.iter
    (fun s ->
      do_if_connected  s.server_sock 
        (fun _ ->
          incr nconnected_servers;
          let connection_time =
            last_time ()
            - connection_last_conn s.server_connection_control
          in
          if !verbose then
              lprintf "MASTER: Checking ip:%s ct:%d\n" (Ip.to_string s.server_ip) connection_time;
          if not s.server_master 
            && (s.server_preferred
                 || connection_time > !!become_master_delay
                 || !!immediate_master
               )
            then
              begin
                if (!nmasters < max_allowed_connected_servers) then
                  begin
                    if !verbose then
                        lprintf "   MASTER: RAISING %s (%Ld)\n" 
                                  (Ip.to_string s.server_ip) s.server_nusers;
                    make_master s
                  end
                else if s.server_sent_all_queries then
                  match !masters with
                      [] -> disconnect_old_server s
                    | ss :: tail ->
                        (* check if the non-master has more users
                           or is a preferred one *)
                        if (s.server_preferred && not ss.server_preferred)
                          || (!!keep_best_server
                               && mini ((Int64.to_int ss.server_nusers) + 1000)
                                       ((Int64.to_int ss.server_nusers) * 5)
                                  < (Int64.to_int s.server_nusers)
                             )
                          then
                            begin
                              if !verbose then
                                lprintf
                                  "   MASTER: RAISING %s (%Ld) instead of %s (%Ld)\n"
                                  (Ip.to_string s.server_ip) s.server_nusers
                                  (Ip.to_string ss.server_ip) ss.server_nusers;
                              ss.server_master <- false;
                              masters := tail;
                              make_master s
                            end
                        else
                          if connection_time > !!walker_server_lifetime then
                            disconnect_old_server s
              end
        )
    ) server_list;
  if !verbose then
      lprintf "MASTER: clean %d connected %d masters\n" 
        !nconnected_servers !nmasters


open LittleEndian
    
let _ = 
  udp_ping.[0] <- char_of_int 0xe3;
  udp_ping.[1] <- char_of_int 0x96;
  udp_ping.[2] <- char_of_int (Random.int 256);
  udp_ping.[3] <- char_of_int (Random.int 256);
  udp_ping.[4] <- char_of_int 0xAA;
  udp_ping.[5] <- char_of_int 0x55;
  
  CommonWeb.add_redirector_info "DKSV" (fun buf ->
      buf_list (fun buf s ->
          buf_ip buf s.server_ip;
          buf_int16 buf s.server_port;
      ) buf (connected_servers ())
  );

  server_ops.op_server_sort <- ( fun s -> 
    (3600 * s.server_score) + connection_last_conn s.server_connection_control
  )

