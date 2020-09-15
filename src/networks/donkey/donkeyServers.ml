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
open CommonServer
open CommonTypes
open CommonOptions
open CommonGlobals
open CommonSources
open CommonShared

open DonkeyProtoCom
open DonkeyTypes
open DonkeyOptions
open DonkeyComplexOptions
open DonkeyGlobals
open DonkeyUdp


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
                    lprintf_nl "TCP: Query Location of %s [%s] [%Ld] [%s]"
                      (file_best_name file) (Md4.to_string file.file_md4) (file_size file) (Ip.to_string (peer_ip sock));
                  let module M = DonkeyProtoServer in
                  let module E = M.QueryLocation in
                  server_send sock ( M.QueryLocationReq {
                     E.md4 = file.file_md4;
                    E.size = file_size file;
                  });
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
                           if s.server_has_get_sources then
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
    lprintf_nl "udp_query_source to: %2d old %2d new servers"
      (List.length !old_servers) (List.length !new_servers);

  try
    (* query "new servers" ie servers which understand multiple filerequests *)
    List.iter (
      fun s ->
        let l = List.map (
          fun file ->
            (file.file_md4,(file_size file));
        ) (get_query_files s udp_requests_new)
        in
        udp_server_send_query_location s l;
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
    lprintf_nl "udp_query_sources: %s" (Printexc2.to_string e)

let disconnect_server s reason =
  let choose_new_master_server s =
    match !DonkeyGlobals.master_server with
    | Some ss when s == ss ->
        DonkeyGlobals.master_server := None;
        (try
          DonkeyGlobals.master_server :=
            Some (List.find (fun s -> s.server_master) !servers_list);
          if !verbose_location then begin
            match !DonkeyGlobals.master_server with
            | Some ns -> 
                    lprintf_nl "changed main master server from %s (%s) to %s (%s)"
                      ss.server_name (string_of_server ss) ns.server_name (string_of_server ns)
            | _ -> ()
          end
        with Not_found -> ())
    | _ -> ();
  in
(* remove this server from the list of servers a file was published to *)
  let remove_server_from_shared_files s =
    List.iter (fun file ->
      shared_iter (fun sh ->
        let impl = as_shared_impl sh in
          impl.impl_shared_servers <-
            List2.removeq (as_server s.server_server) impl.impl_shared_servers)
    ) s.server_sent_shared;
    s.server_sent_shared <- []
  in
  choose_new_master_server s;
  remove_server_from_shared_files s;
  match s.server_sock with
      NoConnection -> ()
    | ConnectionWaiting token ->
        decr nservers;
        cancel_token token;
        s.server_sock <- NoConnection
    | Connection sock ->
        decr nservers;
        TcpBufferedSocket.close sock reason;
        connection_failed (s.server_connection_control);
        s.server_sock <- NoConnection;
        s.server_score <- s.server_score - 1;
        s.server_users <- [];
        if server_state s = Connecting && String2.subcontains s.server_banner "This server is full" then
          set_server_state s ServerFull
        else
          set_server_state s (NotConnected (reason, -1));
        s.server_master <- false;
        s.server_banner <- "";
        s.server_sent_all_queries <- false;
        remove_connecting_server s;
        remove_connected_server s

let server_handler s sock event =
  match event with
      BASIC_EVENT (CLOSED r) ->
        disconnect_server s r
    | BASIC_EVENT (LTIMEOUT | RTIMEOUT) ->
        close sock Closed_for_timeout
    | _ -> ()

let server_udp_ping_statreq s =
  s.server_last_ping <- Unix.gettimeofday ();
  s.server_failed_count <- s.server_failed_count + 1;
  s.server_descping_counter <- s.server_descping_counter + 1;
  if !verbose_msg_servers then
    lprintf_nl "send UDP server ping to %s, try %d"
      (string_of_server s) s.server_failed_count;
  let module M = DonkeyProtoUdp in
  let module E = M.ServerDescUdp in
  let c = Int64.of_int (Random.int 65535) in
  let challenge = 1437204480L ++ c in (* 0x55AA0000 *)
  s.server_udp_ping_challenge <- Some challenge;
(* next ping in 4,5h (16200s) - random value between 1s and 1h to avoid ping storms
   eMule Changelog: August, 30. 2005
   Unk: When a very large popular server come online,
   it experienced ping storms every 4 hours.. Fixed (Lug) *)
  s.server_next_ping <- s.server_last_ping +. 16200. -. (Random.float 3600.);
  udp_server_send_ping s (M.PingServerUdpReq challenge);
  if s.server_descping_counter < 2 then
    begin
      if !verbose_msg_servers then
        lprintf_nl "send UDP server description request to %s" (string_of_server s);
      let rand16 = Int64.of_int (Random.int 65535) in
      let challenge = (left64 rand16 16) ++ M.ServerDescUdp.invalid_len in
      s.server_udp_desc_challenge <- Some challenge;
      udp_server_send s (M.ServerDescUdpReq challenge);
    end
  else
    s.server_descping_counter <- 0

let last_message_sender = ref (-1)

let client_to_server s t sock =
  let module M = DonkeyProtoServer in
  s.server_failed_count <- 0;

  if !verbose_msg_servers then begin
    lprintf_nl "Message from server %s:" (string_of_server s);
    DonkeyProtoServer.print t; lprint_newline ()
  end;

  match t with
      M.SetIDReq t ->
        s.server_has_zlib <- t.M.SetID.zlib;
        s.server_has_newtags <- t.M.SetID.newtags;
        s.server_has_unicode <- t.M.SetID.unicode;
        s.server_has_related_search <- t.M.SetID.related_search;
        s.server_has_tag_integer <- t.M.SetID.tag_integer;
        s.server_has_largefiles <- t.M.SetID.largefiles;
        (match s.server_obfuscation_tcp with
          | None -> if t.M.SetID.tcp_obfuscation then s.server_obfuscation_tcp <- Some 0
          | Some p -> if not t.M.SetID.tcp_obfuscation then s.server_obfuscation_tcp <- None);
        (match s.server_obfuscation_udp with
          | None -> if t.M.SetID.udp_obfuscation then s.server_obfuscation_udp <- Some 0
          | Some p -> if not t.M.SetID.udp_obfuscation then s.server_obfuscation_udp <- None);
        if low_id t.M.SetID.ip && !!force_high_id then
          disconnect_server s (Closed_for_error "Low ID")
        else begin
          s.server_cid <- Some t.M.SetID.ip;
          s.server_realport <- t.M.SetID.port;
          (* disconnect after (connected_server_timeout) seconds of silence *)
          set_rtimeout sock !!connected_server_timeout;
          remove_connecting_server s;
          s.server_score <- s.server_score + 5;
          connection_ok (s.server_connection_control);

          if !!update_server_list_server then
          server_send sock (
            let module Q = M.QueryServerList in
            M.QueryServerListReq Q.t
          );

          if not (low_id t.M.SetID.ip) && !last_high_id <> t.M.SetID.ip && !!use_server_ip then
            last_high_id := t.M.SetID.ip;

          (* nice and ugly, but it doesn't require any new fields *)
          set_server_state s (Connected
          ( match s.server_cid with
              Some t -> if low_id t then (-1) else (-2) 
              | _ -> (-1)
          ));

          if s.server_next_ping = 0. then server_udp_ping_statreq s;

          (* fill list with queries for the server *)
          fill_query_queue s;
          s.server_sent_all_queries <- false;
          s.server_queries_credit <- 0
        end

  | M.MessageReq msg ->
      if msg <> "" then begin
      if !last_message_sender <> server_num s then begin
          let server_header = Printf.sprintf "+-- From server %s [%s] ------"
              s.server_name (string_of_server s) in
          CommonEvent.add_event (Console_message_event (Printf.sprintf "\n%s\n" server_header));
          if !CommonOptions.verbose_msg_servers then
            lprintf_nl "%s" server_header;
          last_message_sender := server_num s
        end;
      s.server_banner <- s.server_banner ^ Printf.sprintf "%s\n" msg;
      let msg = Printf.sprintf "| %s" msg in
      CommonEvent.add_event (Console_message_event (Printf.sprintf "%s\n" msg));
      if !CommonOptions.verbose_msg_servers then
        begin
          lprintf "%s: %s" (string_of_server s) msg;
          lprint_newline ()
        end
      end

  | M.ServerListReq l ->
      if !!update_server_list_server then begin
        if !verbose_msg_servers then lprintf_nl "Received %d servers from server" (List.length l);
        let module Q = M.ServerList in
        List.iter (fun s ->
            safe_add_server s.Q.ip s.Q.port
        ) l
        end
      else begin
        if !verbose_msg_servers then lprintf_nl "Ignored serverlist from server"
        end


  | M.ServerInfoReq t ->
      s.server_score <- s.server_score + 1;
      s.server_tags <- t.M.ServerInfo.tags;
      List.iter (
        fun tag ->
          match tag with
              { tag_name = Field_KNOWN "name"; tag_value = String name } ->
                s.server_name <- name
            | { tag_name = Field_KNOWN "description"; tag_value = String desc } ->
                s.server_description <- desc
            | _ -> lprintf_nl "parsing donkeyServers.ServerInfo, unknown field %s" (string_of_tag tag)
      ) s.server_tags

  | M.InfoReq (users, files) ->
      s.server_nusers <- Some (Int64.of_int users);
      s.server_nfiles <- Some (Int64.of_int files);
      if (users < !!min_users_on_server && not s.server_preferred) then
        begin
          lprintf_nl "%s remove server min_users_on_server limit hit!"
            (string_of_server s);
          server_remove (as_server s.server_server);
        end;
      server_must_update s

  | M.QueryIDReplyReq t ->
      (* This can either be a reply to a QueryID or a indirect request for
         connection from another client. In this case, we should immediatly
         connect. *)
      if !verbose then lprintf_nl "QueryIDReplyReq: received";
      let module Q = M.QueryIDReply in
      if Ip.usable t.Q.ip then begin
        try
          match Fifo.take s.server_id_requests with
              None -> raise Not_found
            | Some file ->
                if !verbose then
                  lprintf_nl "QueryIDReplyReq: This was a QueryID reply !?";
                let s = DonkeySources.create_source_by_uid
                  (Direct_address (t.Q.ip, t.Q.port)) None in
                DonkeySources.set_request_result s file.file_sources
                  File_new_source
        with _ ->
          if !verbose then
            lprintf_nl "QueryIDReplyReq: Calling back to %s:%d"
              (Ip.to_string t.Q.ip) t.Q.port;
          let c = new_client (Direct_address (t.Q.ip, t.Q.port)) None in
          DonkeyClient.reconnect_client c;
      end

  | M.QueryIDFailedReq t ->
      if !verbose then
        lprintf_nl "QueryIDFailedReq:";
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
                { tag_name = Field_KNOWN "name"; tag_value = String s } ->
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
  if !!enable_servers && not (!Ip.banned (s.server_ip, s.server_country_code) <> None)
    && (not !!connect_only_preferred_server || s.server_preferred)
  then
    match s.server_sock with
    | NoConnection ->
        incr nservers;
        let token = add_pending_connection connection_manager (fun token ->
              decr nservers;
              s.server_sock <- NoConnection;
              try
                (* lprintf "CONNECTING ONE SERVER\n"; *)
                connection_try s.server_connection_control;
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
                add_connecting_server s;
                add_connected_server s;
              with e ->
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
                lprintf_nl "There are no ED2K-servers in your servers.ini.";
                lprintf_nl "Please import servers from a server.met file.";
                lprintf_nl "Let MLDonkey use a file configured in web_infos";
                lprintf_nl "or enter this link into MLDonkey:";
                lprintf_nl "ed2k://|serverlist|http://www.gruk.org/server.met.gz|/"
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

let check_server_connections () =
  force_check_server_connections false


(*
 * clean serverlist
 *)
let remove_old_servers () =
  if last_time () - start_time > !!max_server_age * Date.day_in_secs then begin
  
  if !verbose then
    lprintf_nl "old servers: Start check, remove servers not connected for %d days"
      !!max_server_age;
(*
The new tactic: we sort the servers (the more recently connected first,
the black-listed ones at the end), and we start checking from the last
position to the min_left_servers position.
*)

  let to_remove = ref [] in
  let to_keep = ref [] in
  Hashtbl.iter (fun _ s ->
      if connection_was_tried s.server_connection_control then
        if not s.server_preferred &&
          (is_black_address s.server_ip s.server_port s.server_country_code || s.server_port = 4662)
          then
            to_remove := s :: !to_remove
          else
            to_keep :=
              (connection_last_conn s.server_connection_control, s) :: !to_keep
  ) servers_by_key;

  if List.length !to_keep > !!min_left_servers then begin
    let array = Array.of_list !to_keep in
    Array.sort (fun (ls1,_) (ls2,_) ->
      compare ls2 ls1
    ) array;

    if !verbose then
      Array.iteri (fun i (ls, s) ->
        lprintf_nl "old servers: server %d last_conn %d" (server_num s) ls) array;

    let min_last_conn =  last_time () - !!max_server_age * Date.day_in_secs in

    for i = Array.length array - 1 downto !!min_left_servers do
      let ls, s = array.(i) in
        if ls < min_last_conn && s.server_sock = NoConnection
          && not s.server_preferred then begin
          if !verbose then
            lprintf_nl "old servers: Server too old: %s" (string_of_server s);

          to_remove := s :: !to_remove
        end
    done
  end;

  List.iter (fun s ->
    DonkeyGlobals.remove_server s.server_ip s.server_port
  ) !to_remove;

  if !to_remove <> [] || !verbose then
    lprintf_nl "Removed %d old edonkey servers." (List.length !to_remove)

  end


(* Keep connecting to servers in the background. Don't stay connected to
  them , and don't send your shared files list *)
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
                        lprintf_nl "WALKER: try connect %s" (string_of_server s);
                      connect_server s
                    end
                  else
                    begin
                      delayed_list := s :: !delayed_list;
                      if !verbose then
                        lprintf_nl "WALKER: connect %s delayed" (string_of_server s);
                    end
              | _ -> ()

let udp_walker_timer () = (* called every 5s *)
  if !!enable_servers then
  let now = Unix.gettimeofday () in
  try
    Hashtbl.iter (fun _ s ->
      if not (server_blocked (as_server s.server_server)) then
        begin
          if s.server_failed_count > 9 then
            begin
              lprintf_nl "remove dead server %s after %d not answered UDP pings"
                (string_of_server s) s.server_failed_count;
              server_remove (as_server s.server_server);
              raise Exit
            end;
          if s.server_next_ping < now then
            begin
              server_udp_ping_statreq s;
              raise Exit
            end
        end
    ) servers_by_key
  with Exit -> ()

(* sort the servers by preferred first
   then users count with decreasing order
 *)
let compare_servers s2 s1 =
  let n = compare s1.server_preferred s2.server_preferred in
  if n = 0 then
    Int64.to_int
    ((match s1.server_nusers with None -> 0L | Some v -> v) --
     (match s2.server_nusers with None -> 0L | Some v -> v))
  else n

(* check connected servers *)
let update_master_servers _ =
  if !verbose_location then
    lprintf_nl "master servers: start re-computing";
  let server_list = List.sort compare_servers (connected_servers ()) in
  let masters = ref [] in
  let tag1 = ref true in
  List.iter (
    let tag2 = ref true in
    fun s ->
      if s.server_master then
        match s.server_sock with
        | Connection _ ->
            if !verbose_location then begin
              if !tag2 then begin
                lprintf_n "master servers (old):";
                tag1 := false;
                tag2 := false
              end;
              lprintf " %s" (string_of_server s)
            end;
            masters := s :: !masters
        | _ -> s.server_master <- false
  ) server_list;
  if not !tag1 then lprint_newline ();
  let nmasters = ref (List.length !masters) in

  let make_master s =
    (* normal servers don't have our SHARE, so send list if it becomes a master *)
    do_if_connected s.server_sock (fun sock ->
        s.server_master <- true;
        incr nmasters;

        (* Put the server in the list of servers, and update the list *)
        masters := s :: !masters;
        masters := List.rev (List.sort compare_servers !masters);

        DonkeyGlobals.master_server := Some s;
    )
  in

  let max_allowed_connected_servers = max_allowed_connected_servers () in
  let nconnected_servers = ref 0 in

  let disconnect_old_server s =
    (* disconnect a server we are connected to for too long if we have too many
       connections *)
    if !nconnected_servers > max_allowed_connected_servers then
      begin
        if !verbose_location then
            lprintf_nl "master servers: disconnect %s" (string_of_server s);
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
          if !verbose_location then
              lprintf_nl "master servers: Checking %s, users: %Ld, ct:%d"
                (string_of_server s)
                (match s.server_nusers with None -> 0L | Some v -> v)
                connection_time;
          if not s.server_master then
              begin
                if (!nmasters < max_allowed_connected_servers) then begin
                  if !verbose_location then
                    lprintf_nl "master servers: raising %s" (string_of_server s);
                    make_master s
                  end
                else if s.server_sent_all_queries then
                  match !masters with
                      [] -> disconnect_old_server s
                    | ss :: tail ->
                        let ss_nusers =
                          match ss.server_nusers with
                            None -> 0L
                          | Some v -> v in
                        let s_nusers =
                          match s.server_nusers with
                            None -> 0L
                          | Some v -> v in
                        (* check if the non-master has more users
                           or is a preferred one *)
                        if (s.server_preferred && not ss.server_preferred)
                          || (!!keep_best_server
                               && min ((Int64.to_int ss_nusers) + 1000)
                                       ((Int64.to_int ss_nusers) * 5)
                                  < (Int64.to_int s_nusers)
                             )
                          then
                            begin
                              if !verbose_location then
                                lprintf_nl
                                  "master servers: raising %s, disconnected %s"
                                  (string_of_server s) (string_of_server ss);
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
  if !verbose_location then
      lprintf_nl "master servers: %d connected %d masters - re-computing completed"
        !nconnected_servers !nmasters

let check_for_preferred_servers () =
  let found_preferred = ref false in
  Hashtbl.iter (fun _ s -> if s.server_preferred then found_preferred := true) servers_by_key;
  if not !found_preferred && !!connect_only_preferred_server then
    begin
      connect_only_preferred_server =:= false;
      let message = "Set connect_only_preferred_server to false because no preferred server was found" in
      lprintf_nl "%s" message;
      startup_message := !startup_message ^ message
    end

let _ =
  server_ops.op_server_sort <- ( fun s ->
    (3600 * s.server_score) + connection_last_conn s.server_connection_control
  )
