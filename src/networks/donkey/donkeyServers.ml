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

open Printf2
open Md4
open CommonUser
open CommonSearch
open CommonComplexOptions
open CommonServer
open CommonOptions
open CommonTypes
open Options
open BasicSocket
open TcpBufferedSocket
open DonkeyMftp
open DonkeyImport
open DonkeyProtoCom
open DonkeyTypes
open DonkeyOptions
open CommonOptions
open DonkeyComplexOptions
open DonkeyGlobals
open CommonGlobals

let udp_send_if_possible sock addr msg =
  udp_send sock addr msg

  (*
let first_name file =
  match file.file_filenames with
    [] -> Filename.basename (file_disk_name file)
  | name :: _ -> name
*)    

(****************************************************)

  (*
let update_options () =  
  known_servers =:=  Sort.list (fun s1 s2 -> 
      s1.server_score > s2.server_score ||
      (s1.server_score = s2.server_score &&
        (connection_last_conn 
            s1.server_connection_control) 
        > (connection_last_conn s2.server_connection_control))
  ) !!known_servers
    *)


let query_location file sock =
  if !verbose_location then begin
      lprint_newline ();
      lprintf "Server: Query Location of %s" (file_best_name file);
    end;
  direct_server_send sock (
    let module M = DonkeyProtoServer in
    let module C = M.QueryLocation in
    M.QueryLocationReq file.file_md4
  )
          
let query_locations s n_per_round = 
  match s.server_sock with
    None -> ()
  | Some sock ->
      let rec iter n =
        if n>0 then
          match s.server_waiting_queries with
            [] ->
              begin
                
                let files = !current_files in
                let files = List.sort (fun f1 f2 ->
                      file_priority f2 - file_priority f1
                  ) files in
                
                s.server_waiting_queries <- files;
                if s.server_waiting_queries <> [] then
                  let nqueries = ref 0 in
 		  List.iter (fun file ->
 		    if file_state file = FileDownloading then
 			incr nqueries
 		  ) !current_files;
                   iter (mini n (!nqueries - (n_per_round - n)))
            end
          | file :: files ->
              s.server_waiting_queries <- files;
              if file_state file = FileDownloading then begin
                  query_location file sock;
                  iter (n-1)
                end else
                iter n
      in
      iter n_per_round

      
(* every 60 seconds *)
let query_locations_timer () =
  List.iter (fun s ->
(* During the first 20 minutes, don't send any localisation query
to the server *)
      if s.server_queries_credit <= 0 then
        query_locations s !!files_queries_per_minute
      else 
        s.server_queries_credit <- s.server_queries_credit - 1
  ) (connected_servers())
  
let _ =
  server_ops.op_server_sort <- (fun s ->
      (3600 * s.server_score) +
        connection_last_conn s.server_connection_control
  )

let disconnect_server s =
  match s.server_sock with
    None -> ()
  | Some sock ->
      decr nservers;
      TcpBufferedSocket.close sock "closed";
      (*
            lprintf "%s:%d CLOSED received by server"
(Ip.to_string s.server_ip) s.server_port; lprint_newline ();
  *)
      connection_failed (s.server_connection_control);
      s.server_sock <- None;
      s.server_score <- s.server_score - 1;
      s.server_users <- [];
      set_server_state s (NotConnected (-1));
      s.server_master <- false;
	  s.server_banner <- "";
      remove_connected_server s
      
let server_handler s sock event = 
  match event with
    BASIC_EVENT (CLOSED _) ->
      disconnect_server s
  | BASIC_EVENT (LTIMEOUT | RTIMEOUT) ->
          close sock "timeout"  

  | _ -> ()
      
let last_message_sender = ref (-1)
      
let client_to_server s t sock =
  let module M = DonkeyProtoServer in

  if !verbose_msg_servers then begin
      lprintf "Message from server:"; lprint_newline ();
      DonkeyProtoServer.print t; lprint_newline ();
    end;
  
  match t with
    M.SetIDReq t ->
      if not (Ip.valid t) && !!force_high_id then
	disconnect_server s
      else begin
	s.server_cid <- Some t;
	set_rtimeout sock !!connected_server_timeout; 
(* force deconnection after one hour if nothing  appends *)
	set_server_state s Connected_initiating;
	s.server_score <- s.server_score + 5;
	connection_ok (s.server_connection_control);
      
	direct_server_send sock (
          let module A = M.AckID in
          M.AckIDReq A.t
	);

	if Ip.valid t && !!use_server_id then
	  last_high_id := t;

(*
      server_send sock (M.ShareReq (make_tagged (
            if !nservers <=  max_allowed_connected_servers () then
              begin
                s.server_master <- true;
                let shared_files = all_shared () in
                shared_files
              end else
              []
          )));
*)
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
      if !!update_server_list then
        let module Q = M.ServerList in
        List.iter (fun s ->
            safe_add_server s.Q.ip s.Q.port
        ) l
  
  | M.ServerInfoReq t ->
      
      let module Q = M.ServerInfo in
(* query file locations *)
      s.server_score <- s.server_score + 1;
      s.server_tags <- t.Q.tags;
      List.iter (fun tag ->
          match tag with
            { tag_name = "name"; tag_value = String name } -> 
              s.server_name <- name
          | { tag_name = "description"; tag_value = String desc } ->
              s.server_description <- desc
          | _ -> ()
      ) s.server_tags;
      printf_char 'S';
      set_server_state s (Connected (-1));
      add_connected_server s;
      
(* Send localisation queries to the server. We are limited by the maximalù
number of queries that can be sent per minute (for lugdunum, it's one!).
Once the first queries have been sent, we must wait 20 minutes before next
queries. *)
      query_locations s (20 * !!files_queries_per_minute);
      s.server_queries_credit <- (* !!files_queries_initial_delay *) 0
(*      !server_is_connected_hook s sock *)
  
  | M.InfoReq (users, files) ->
      s.server_nusers <- users;
      s.server_nfiles <- files;
      server_must_update s
  
  | M.Mldonkey_MldonkeyUserReplyReq ->
      s.server_mldonkey <- true;
      printf_string "[MLDONKEY SERVER]"
        
  | M.QueryIDReplyReq t -> 
(* This can either be a reply to a QueryID or a indirect request for
connection from another client. In this case, we should immediatly connect.
*)
      
      let module Q = M.QueryIDReply in
      if Ip.valid t.Q.ip && Ip.reachable t.Q.ip then begin
          match Fifo.take s.server_id_requests with
            None -> 
              let c = new_client (Known_location (t.Q.ip, t.Q.port)) in
              DonkeyClient.reconnect_client c;
              friend_add c

          | Some file ->
              ignore (DonkeySources.new_source (t.Q.ip, t.Q.port) file)
        end

  | M.QueryIDFailedReq t ->
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
              direct_server_send sock M.QueryMoreResultsReq;
              Fifo.put s.server_search_queries search      
            end;
          DonkeyFiles.search_handler search t
        with Already_done -> iter ()
      in
      iter ()          
    
  | M.Mldonkey_NotificationReq (num, t) ->
      let s = search_find num in
      List.iter (fun f ->
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
(*          lprintf "QueryUsersReply"; lprint_newline (); *)
      List.iter (fun cl ->

(*              lprintf "NEW ONE"; lprint_newline (); *)
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
                { tag_name = "name"; tag_value = String s } -> 
                  user.user_name <- s
              | _ -> ()
          ) user.user_tags;
          
          if add_to_friend then DonkeyFiles.add_user_friend s user;
          
          s.server_users <- user :: s.server_users;
(*              lprintf "SERVER NEW USER"; lprint_newline (); *)
          server_new_user (as_server s.server_server) 
          (as_user user.user_user);
      ) t;
      server_must_update s
  
  | M.QueryLocationReplyReq t -> 
      DonkeyClient.query_locations_reply s t
      
  | _ -> 
(*      !received_from_server_hook s sock t *)
      ()
      
let connect_server s =
  if can_open_connection () then
    try
      match s.server_sock with
        Some _ -> printf_string "[e0]"
      | _ ->

(*                lprintf "CONNECTING ONE SERVER"; lprint_newline (); *)
          connection_try s.server_connection_control;
          incr nservers;
          printf_char 's'; 
          let sock = TcpBufferedSocket.connect 
              "donkey to server"
              (
              Ip.to_inet_addr s.server_ip) s.server_port 
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
          s.server_sock <- Some sock;
          direct_server_send sock (
            let module M = DonkeyProtoServer in
            let module C = M.Connect in
            M.ConnectReq {
              C.md4 = !!client_md4;
              C.ip = client_ip (Some sock);
              C.port = !client_port;
              C.tags = !client_tags;
            }
          );
    with _ -> 
(*
      lprintf "%s:%d IMMEDIAT DISCONNECT "
      (Ip.to_string s.server_ip) s.server_port; lprint_newline ();
*)
(*      lprintf "DISCONNECTED IMMEDIATLY"; lprint_newline (); *)
        disconnect_server s
        
let print_empty_list = ref true
        
let rec connect_one_server () =
(*  lprintf "connect_one_server"; lprint_newline (); *)
  if can_open_connection () then
    match !servers_list with
      [] ->
        
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
        connect_one_server ()
    | s :: list ->
        servers_list := list;
        if connection_can_try s.server_connection_control then
          begin
(* connect to server *)
            match s.server_sock with
              Some _ -> ()
            | None -> 
                if s.server_score < 0 then begin
(*                  lprintf "TOO BAD SCORE"; lprint_newline ();*)
                    connect_one_server ()
                  end
                else
                  connect_server s
          
          end
          

let force_check_server_connections user =
(*  lprintf "force_check_server_connections"; lprint_newline (); *)
  if user || !nservers < max_allowed_connected_servers ()  then 
    let rec iter n =
      if n > 0 && can_open_connection () then begin
          connect_one_server ();
          iter (n-1)
        end
    in
    iter (!!max_connected_servers - !nservers)
    
let rec check_server_connections () =
  force_check_server_connections false

let remove_old_servers () =
  lprintf "REMOVE OLD SERVERS";  lprint_newline ();
  let t1 = Unix.gettimeofday () in
(*
The new tactic: we sort the servers (the more recently connected first,
the black-listed ones at the end), and we start checking from the last
position to the min_left_servers position.
*)

  let to_remove = ref [] in
  let to_keep = ref [] in
  Hashtbl.iter (fun _ s ->
      if is_black_address s.server_ip s.server_port || s.server_port = 4662 then
        to_remove := s :: !to_remove
      else
        to_keep := (connection_last_conn s.server_connection_control, s) :: 
        !to_keep
  ) servers_by_key;
  let t2 = Unix.gettimeofday () in
  lprintf "Delay to detect black-listed servers: %2.2f" (t2 -. t1); lprint_newline ();
  
  let array = Array.of_list !to_keep in
  Array.sort (fun (ls1,_) (ls2,_) ->
      if ls1 = ls2 then 0 else if ls1 > ls2 then -1 else 1
  ) array;

  if !verbose then 
    for i = 0 to Array.length array - 1 do
      let ls, s = array.(i) in
      lprintf "server %d last_conn %d" (server_num s) ls;
      lprint_newline ()
    done;

  let min_last_conn =  last_time () - !!max_server_age * Date.day_in_secs in
  
  for i = Array.length array - 1 downto !!min_left_servers do
    let ls, s = array.(i) in
    if ls < min_last_conn && s.server_sock = None then begin
        if !verbose then begin
            lprintf "Server too old: %s:%d" 
              (Ip.to_string s.server_ip) s.server_port;
            lprint_newline ();
          end;
        to_remove := s :: !to_remove
      end
  done;
  let t3 = Unix.gettimeofday () in
  lprintf "Delay to detect old servers: %2.2f" (t3 -. t2); 
  lprint_newline ();

  List.iter (fun s ->
      server_remove (as_server s.server_server);
      Hashtbl.remove servers_by_key (s.server_ip, s.server_port))
  !to_remove;

  let t4 = Unix.gettimeofday () in
  lprintf "Delay to finally remove servers: %2.2f" (t4 -. t3); 
  lprint_newline ();
  
(*
  let removed_servers = ref [] in
  let servers_left = ref 0 in
  
  Hashtbl.iter (fun key s ->
      if 
        removed_servers := (key,s) :: !removed_servers
      else incr servers_left
  ) servers_by_key;
  if !servers_left > 200 then begin
      List.iter (fun (key,s) ->
      ) !removed_servers
    end else begin
      lprintf "Not enough remaining servers: %d" !servers_left;  
      lprint_newline ()
    end; *)
  lprintf "REMOVE %d OLD SERVERS DONE" (List.length !to_remove);  
  lprint_newline ()
  
(* Don't let more than max_allowed_connected_servers running for
more than 5 minutes *)
    
let update_master_servers _ =
(*  lprintf "update_master_servers"; lprint_newline (); *)
  let nmasters = ref 0 in
  List.iter (fun s ->
      if s.server_master then
        match s.server_sock with
          None -> ()
        | Some _ -> incr nmasters;
  ) (connected_servers ());
  let nconnected_servers = ref 0 in
  let list = List.sort (fun s2 s1 ->
        s1.server_nusers - s2.server_nusers
    ) (connected_servers ()) in
  List.iter (fun s ->
      incr nconnected_servers;
      if not s.server_master && s.server_cid <> None then
        if !nmasters <  max_allowed_connected_servers () &&
          s.server_nusers >= !!master_server_min_users
        then begin
            match s.server_sock with
              None -> 
              (*  lprintf "MASTER NOT CONNECTED"; lprint_newline ();  *)
                ()
            | Some sock ->                
(*                lprintf "NEW MASTER SERVER"; lprint_newline (); *)
                s.server_master <- true;
                incr nmasters;
                direct_server_send_share sock (DonkeyShare.all_shared ())
          end else
        if connection_last_conn s.server_connection_control 
            + 120 < last_time () &&
          !nconnected_servers > max_allowed_connected_servers ()  then begin
(* remove one third of the servers every 5 minutes *)
            nconnected_servers := !nconnected_servers - 3;
(*            lprintf "DISCONNECT FROM EXTRA SERVER %s:%d "
(Ip.to_string s.server_ip) s.server_port; lprint_newline ();
  *)
            (match s.server_sock with
                None ->                   
                  (*
                  lprintf "Not connected !"; 
lprint_newline ();
*)
                  ()

              | Some sock ->
  (*                lprintf "shutdown"; lprint_newline (); *)
                  (shutdown sock "max allowed"));
          end
  ) 
  (* reverse the list, so that first servers to connect are kept ... *)
  list
    
(* Keep connecting to servers in the background. Don't stay connected to 
  them , and don't send your shared files list *)
let walker_list = ref []
let delayed_list = ref []
let next_walker_start = ref 0
  
(* one call every 5 seconds, so 12/minute, 720/hour *)
let walker_timer () = 
  
  if !!servers_walking_period > 0 &&
    !nservers < max_allowed_connected_servers () + !!max_walker_servers 
  then
    match !walker_list with
      [] ->
        begin      
          if !delayed_list <> [] then begin
              walker_list := !delayed_list;
              delayed_list := []
            end else
          if last_time () > !next_walker_start then begin
              next_walker_start := last_time () + !!servers_walking_period * 3600;
              Hashtbl.iter (fun _ s ->
                  walker_list := s :: !walker_list
              ) servers_by_key;
            end
        end
    | s :: tail ->
        walker_list := tail;
        match s.server_sock with
          None -> 
            if connection_can_try s.server_connection_control then begin
                
                if !verbose then begin
                    lprintf "WALKER: try connect %s" 
                      (Ip.to_string s.server_ip);
                    lprint_newline ();
                  end;
                
                connect_server s
              end else begin
                
                delayed_list := s :: !delayed_list;
                if !verbose then begin
                    lprintf "WALKER: connect %s delayed"
                      (Ip.to_string s.server_ip);
                    lprint_newline ();
                  end;
              
              end
        | Some _ -> ()
            
(* one call per second *)

              
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
  
let _ = 
  udp_ping.[0] <- char_of_int 0xe3;
  udp_ping.[1] <- char_of_int 0x96;
  udp_ping.[2] <- char_of_int (Random.int 256);
  udp_ping.[3] <- char_of_int (Random.int 256);
  udp_ping.[4] <- char_of_int (Random.int 256);
  udp_ping.[5] <- char_of_int (Random.int 256)
  
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
      UdpSocket.write (get_udp_sock ()) udp_ping s.server_ip (s.server_port + 4)
      
      
      
      

      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
let update_master_servers _ =

  if !verbose then begin
      
      lprint_newline ();
      lprint_newline ();
    end;
  
  let list = List.sort (fun s2 s1 ->
        s1.server_nusers - s2.server_nusers
    ) (connected_servers ()) in

(* Now, the servers are sorted in 'list' so that the first ones have the more
  users. *)
  
  let masters = ref [] in
  List.iter (fun s ->
      if s.server_master then 
        match s.server_sock with
          None -> s.server_master <- false
        | _ -> 
            if !verbose then begin
                lprintf "MASTER: OLD MASTER %s" (Ip.to_string s.server_ip);
                lprint_newline ();
              end;
            masters := s :: !masters
  ) list;
  let nmasters = ref (List.length !masters) in
  
  if !verbose then begin
      lprintf "MASTER: nmaster %d" !nmasters;
      lprint_newline (); lprint_newline ();
    end;
(* The master servers are sorted in 'masters' so that the first ones have
the fewer users. *)  
  
  let make_master s =
    match s.server_sock with
      None -> assert false
    | Some sock ->                
        if !verbose then begin
            lprintf "   MASTER: %s" (Ip.to_string s.server_ip); 
            lprint_newline ();
          end;
        s.server_master <- true;
        incr nmasters;
        direct_server_send_share sock (DonkeyShare.all_shared ())        
  in

  let max_allowed_connected_servers = max_allowed_connected_servers () in
  let nconnected_servers = ref 0 in
  
  let disconnect_old_server s =
(* disconnect a server we are connected to for too long if we have too many
connections *)
    if !nconnected_servers > max_allowed_connected_servers then begin
        
        if !verbose then begin
            lprintf "MASTER:    DISCONNECT %s" (Ip.to_string s.server_ip);
            lprint_newline ();
          end;
        
        nconnected_servers := !nconnected_servers - 3;
        (match s.server_sock with
            None -> assert false
          | Some sock ->
              
(* We will disconnect from this server. Send the last queries and
  wait for 60 seconds to disconnect. *)
              
              List.iter (fun file ->
                  query_location file sock 
              ) s.server_waiting_queries;
              
              
              set_lifetime sock 60.);
      end
  in
  
  List.iter (fun s ->
      if s.server_sock <> None then begin
          incr nconnected_servers;
          
          if !verbose then begin
              lprintf "MASTER: EXAM %s %d" (Ip.to_string s.server_ip)
              (last_time () -  connection_last_conn s.server_connection_control)
              ; 
              lprint_newline ();
            end;
          
          if connection_last_conn s.server_connection_control 
              + 120 < last_time () && not s.server_master then begin
(* We have been connected for two minutes to this server, we can disconnect 
now if needed *)
              
              if !nmasters < max_allowed_connected_servers
(* I Should clearly remove this option now  
  &&
                s.server_nusers >= !!master_server_min_users *)
              then 
                make_master s
              else
                
              match !masters with 
                [] -> disconnect_old_server s
              | ss :: tail ->
                  if !!keep_best_server &&
                    mini (ss.server_nusers + 1000) (ss.server_nusers * 5)
                    < s.server_nusers then begin
                      
                      if !verbose then begin
                          lprintf
                            "   MASTER: RAISING %s (%d) instead of %s (%d)" 
                            (Ip.to_string s.server_ip) s.server_nusers 
                            (Ip.to_string ss.server_ip) ss.server_nusers
                          ; lprint_newline (); 
                        end;
                      
                      ss.server_master <- false;
                      masters := tail;
                      make_master s
                    end else
                    disconnect_old_server s
            end
        end)
  list;
  
  if !verbose then begin
      lprintf "MASTER: clean %d connected %d masters" 
        !nconnected_servers !nmasters; lprint_newline ();
    end;
      
