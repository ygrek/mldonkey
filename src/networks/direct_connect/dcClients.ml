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
open Int64ops
open Md4
open CommonUploads
open CommonInteractive

open CommonClient
open BasicSocket
open CommonOptions
open TcpBufferedSocket
open CommonGlobals
open CommonFile
open CommonTypes
open CommonShared
open Options

open DcTypes
open DcOptions
open DcGlobals
open DcProtocol

let log_prefix = "[dcCli]"

let lprintf_nl fmt =
  lprintf_nl2 log_prefix fmt

(* Check current client state and setup for future proceeding with client *)
let set_client_state_on_disconnect c =
  (match c.client_state with
  | DcDownloadWaiting file                     (* if client is downloading a file ...*)
  | DcDownload file
  | DcDownloadConnecting (file,_) ->
      (*if !verbose_download then 
        lprintf_nl "Client (%s) state is (DcDownload/-Waiting/-Connecting file) on closing socket" (clients_username c);*)
      c.client_state <- DcDownloadWaiting file; (* continue downloading later *)
      set_client_state c NewHost
  | DcUploadList file_fd                       (* if client was handling file lists... *)
  | DcDownloadList file_fd ->
      (*if !verbose_upload || !verbose_download then
        lprintf_nl "Client (%s) state is (DcUploadList/DcDownloadList) on closing socket" (clients_username c); *)
      c.client_state <- DcIdle;
      Unix32.close file_fd;                    (* close file descriptor and remove client *)
      remove_client c
  | DcUploadStarting (dcsh,_,_)
  | DcUpload (dcsh,_,_,_) ->                   (* if client was uploading file ... *)
      (*if !verbose_upload then 
        lprintf_nl "Client (%s) state is (DcUpload/UploadStarting) on closing socket" (clients_username c);*)
      if not (counts_as_minislot dcsh.dc_shared_size) then dc_remove_uploader (); (* check if we have to free a slot *)
      set_client_has_a_slot (as_client c.client_client) NoSlot; (* inform GUI *)
      (match c.client_state with
      | DcUpload (_,file_fd,_,_) -> Unix32.close file_fd
      | _ -> () );
      c.client_state <- DcIdle;
      remove_client c
  | DcDownloadListWaiting                      (* filelist downloading *)
  | DcDownloadListConnecting _ ->
      (*if !verbose_download then
        lprintf_nl "Client (%s) state is (DcDownloadListWaiting/-Connecting) on closing socket" (clients_username c);*)
      c.client_state <- DcDownloadListWaiting;
      set_client_state c NewHost;
  | DcConnectionStyle _ ->
      (*if !verbose_upload || !verbose_download then
        lprintf_nl "Client (%s) state is (DcConnectionStyle) on closing socket" (clients_username c);*)
      c.client_state <- DcIdle;
      remove_client c
  | DcUploadListStarting _
  | DcUploadDoneWaitingForMore
  | DcIdle ->
      (*if !verbose_upload then
        lprintf_nl "Client (%s) state is (DcUploadListStarting/DoneWaitingForMore) on closing socket" (clients_username c);*)
      c.client_state <- DcIdle;
      remove_client c );
  ()

(* check that file can be started and no other client is downloading it *)
let can_file_start_downloading f =
  (try
    (match (file_state f) with         (* check file state *)
    | FileDownloaded | FileShared | FileCancelled | FileAborted _ | FilePaused -> raise BreakIter
    | _ -> () );
    List.iter (fun c -> (* check files all other clients that they are not already possibly loading *)
      if is_client_blocking_downloading c then raise BreakIter 
    ) f.file_clients;
    true
  with _ -> false )

(* Try to find alternative client to file *)
let find_downloadable_client_for_file file = (* CHECK  possible user state also... *)
  (try
    List.iter (fun c -> (* chech all files sources *)
      (match c.client_user with
      | Some u ->
          if (can_user_start_downloading u) then begin
            if is_client_waiting c then begin
              (match c.client_error with
              | NoError | NoFreeSlots | UploadError -> raise (Found_client c)
              | FileNotAvailable | UserNotReachable | ClosedOnInit | ConnectionResetByPeer
              | UserDontReplyOnTime -> () )
            end
          end
      | None -> () )
    ) file.file_clients;
    None
  with
  | Found_client c -> Some c )

(* Disconnect client with proper even if no socket yet handling *)
let dc_disconnect_client c reason =
  (match c.client_sock with
  | Connection sock ->
      connection_failed c.client_connection_control;      
      dc_set_client_disconnected c reason;
      TcpBufferedSocket.close sock reason;
      c.client_sock <- NoConnection;
  | ConnectionWaiting token -> 
      cancel_token token;
      c.client_sock <- NoConnection;
  | _ -> () );
  set_client_state_on_disconnect c;
  (match c.client_user with
  | Some user ->
      user.user_state <- UserIdle; (* initialize also possible user *)
  | _ -> () )

(* Move file to last in global filelist *)
let move_file_to_last_in_files f =
  if List.length !current_files > 1 then begin
    let list = List2.removeq_first f !current_files in
    current_files := list @ [f];         
  end

(* Move client to first in files clientlist *)
let move_client_to_first_in_fileslist c =
  (match c.client_file with
  | Some f -> 
      if List.length f.file_clients > 1 then begin
        let list = List2.removeq_first c f.file_clients in
        f.file_clients <- c :: list;                         (* lets put this client on top of files clients *)
      end
  | _ -> () )
      
(* Move client to first in users clientlist *)
let move_client_to_first_in_userslist c =
  (match c.client_user with
  | Some u -> 
      if List.length u.user_clients > 1 then begin
        let list = List2.removeq_first c u.user_clients in
        u.user_clients <- c :: list;                         (* lets put this client on top of users clients *)
      end
  | _ -> () )
        
(* Move client to last in users clientlist *) 
let move_client_to_last_in_userslist c =
  (match c.client_user with
  | Some u ->
       if List.length u.user_clients > 1 then begin
         let list = List2.removeq_first c u.user_clients in  (* lets move this client to last in users list  *)
         u.user_clients <- list @ [c];
       end
  | _ -> () )

(* Move client to last in users clientlist *) 
let move_client_to_last_in_fileslist c =
  (match c.client_file with
  | Some f ->
       if List.length f.file_clients > 1 then begin
         let list = List2.removeq_first c f.file_clients in  (* lets move this client to last in files list  *)
         f.file_clients <- list @ [c]; 
       end
  | _ -> () )

(* Try to find next available source to this file and make it next in line, return true if found *)
let find_existing_source c =
  (match c.client_file with
  | Some f ->
      let alternative = find_downloadable_client_for_file f in
      (match alternative with 
      | Some ac ->                              (* we have a client that we can try to download *)
          move_client_to_first_in_fileslist ac; (* lets top this client to be next in line to be tried *)
          move_client_to_first_in_userslist ac;
          true
      | None -> false )
  | None -> false )

(* Send search request to server *)
let server_send_search s search filetype sname =
  if !verbose_msg_clients && (List.length !connected_servers) > 0 then
    lprintf_nl "Sending: $Search (%s) (%s)" (shorten_string s.server_name 20) sname;
  do_if_connected s.server_sock (fun sock ->
    let module S = DcProtocol.Search in
    let msg = DcProtocol.SearchReq {
      S.passive = !!firewalled;
      S.nick = if !!firewalled then s.server_last_nick else empty_string;
      S.ip = if !!firewalled then empty_string else Ip.to_string (CommonOptions.client_ip (Some sock));
      S.port = if !!firewalled then empty_string else (string_of_int !!dc_port);
      S.sizelimit = NoLimit;
      S.filetype = filetype;
      S.words_or_tth = sname;
    } in
    dc_send_msg sock msg;
    s.server_search <- Some search;
    s.server_search_timeout <- last_time () + !!search_timeout;
  )

(* Received SR from servers or by udp *)
let received_new_search_result s msg =
  if s.server_search_timeout < last_time () then s.server_search <- None;
  (match s.server_search with
  | None -> ()
  | Some q ->
      let module S = SR in
      let user = new_user (Some s) msg.S.owner in      (* create possibly new user *)
      let result = new_result user msg.S.tth msg.S.directory msg.S.filename msg.S.filesize in (* new or retrieve existing *)
                                                                              (* result dir,filename,size *)
      ignore (add_info_to_result result user msg.S.tth msg.S.directory);      (* add info to dc-fields *)
      CommonInteractive.search_add_result false q result;
      if !!autosearch_by_tth && (msg.S.tth <> empty_string) then begin
        (try
          let f = Hashtbl.find dc_files_by_unchecked_hash msg.S.tth in        (* if this hash is in downloads *)
          if (List.length f.file_clients < !!max_sources_file) then begin 
            (try 
               List.iter (fun c ->  (* check if some of users client is already on this files list *)
                 if (List.mem c f.file_clients) then raise BreakIter
               ) user.user_clients;
               let c = new_client_to_user_with_file user f in 
               c.client_state <- DcDownloadWaiting f; 
               if !verbose_msg_clients then lprintf_nl "New client (%s) created by tth search to file (%s)"
                 (clients_username c) f.file_name
            with _ -> () )            (* user has already client with this file *)
          end
        with _ -> () )                (* this file is in active downloads *) 
      end  )

(* Create new search automatically if possible *)
let create_new_search f =
  let query = QAnd (QHasField (Field_Type , "TTH") , (QHasWord f.file_unchecked_tiger_root)) in
  let search = CommonSearch.new_search (CommonUserDb.find_ui_user CommonUserDb.admin_user_name)
    (let module G = GuiTypes in
      { G.search_num = 0;
        G.search_query = query;
        G.search_max_hits = 1000;
        G.search_type = RemoteSearch;
        G.search_network = network.network_num;
      } )
  in
  dc_with_connected_servers (fun s -> (* iter all servers *)
    server_send_search s search 9 f.file_unchecked_tiger_root
  );
  (match !dc_last_autosearch with
  | Some s -> CommonSearch.search_forget (CommonUserDb.find_ui_user CommonUserDb.admin_user_name) s
  | _ -> () );
  dc_last_autosearch := Some search;
  dc_last_autosearch_time := current_time ();
  f.file_autosearch_count <- succ f.file_autosearch_count

let create_autosearch () =
  let s_time_out = float !!search_timeout in
  let c_time = current_time () in
  if (!dc_last_autosearch_time +. s_time_out) < c_time &&
    (!dc_last_manual_search +. s_time_out) < c_time && (List.length !connected_servers) > 0 then begin
    (*lprintf_nl "Created autosearch";*)
    (try
      List.iter (fun f ->
        (try
          List.iter (fun c ->             (* lets check that file is not already being downloaded *)
            if is_client_blocking_downloading c then raise BreakIter
          ) f.file_clients;
          if (List.length f.file_clients < !!max_sources_file) &&
             (is_valid_tiger_hash f.file_unchecked_tiger_root) then begin
            create_new_search f;
            raise (Found_file f);
          end 
        with
        | BreakIter -> () );
      ) !current_files;
      (*lprintf_nl "Autosearch end not found";  *)
    with Found_file f ->
      move_file_to_last_in_files f ) (* lets give next search to different file *)
      (*lprintf_nl "Autosearch end file found" )*)
  end

(* Memorize and take action on different client error situations *)
let new_client_error c error =
  let same_as_before = (error = c.client_error) in
  if not same_as_before then c.client_error_count <- 1
  else c.client_error_count <- succ c.client_error_count;
  (match error with
  | NoFreeSlots ->                                                (* MaxedOut *)
      move_client_to_last_in_userslist c;
      move_client_to_last_in_fileslist c;
      c.client_error <- NoFreeSlots;
  | FileNotAvailable ->
      c.client_error <- FileNotAvailable;
      (match c.client_state with
      | DcDownloadList _ -> (* on rare condition... eg. DC++ 0.401 *)(* TODO Try to load MyList.DcLst *)
          ()
      | _ ->
        move_client_to_last_in_userslist c;
        move_client_to_last_in_fileslist c;
      )
  | UserNotReachable ->
     c.client_error <- UserNotReachable;
  | ClosedOnInit ->
      if same_as_before then begin
        (* what to do if connection closes on init phase without clear reason *)
        if (is_even_to_tenths c.client_error_count) then begin
          if not (find_existing_source c) then begin (* if not available source *)
            if !verbose_msg_clients then 
              lprintf_nl "ClosedOnInit: (%s)" (clients_username c);
          end
        end
      end else begin
        c.client_error <- ClosedOnInit
      end
  | ConnectionResetByPeer ->
      if same_as_before then begin
        (* what to do if client resets connection without error messages and without reason *)
        if (c.client_error_count > 10) then begin
          if not (find_existing_source c) then begin (* if not available source *)
            if !verbose_msg_clients then
              lprintf_nl "ConnectionResetByPeer: (%s)" (clients_username c);
          end
        end
      end else begin
        c.client_error <- ConnectionResetByPeer
      end
  | UploadError ->
      c.client_error <- UploadError;
      if !verbose_msg_clients || !verbose_upload then 
        lprintf_nl "UploadError: (%s)" (clients_username c);
      (*ready_for_upload (as_client c.client_client);*) (* still try to continue upload, or what ? *)
  | UserDontReplyOnTime ->
      (* what to do if we have sent Rev/ConnectToMe to user but got no MyNick on time *)
      if same_as_before then begin
        if (is_even_to_twos c.client_error_count) then begin
          if not (find_existing_source c) then begin (* if not available source *)
            if !verbose_msg_clients then
              lprintf_nl "UserDontReplyOnTime: (%s)" (clients_username c);
          end;
          dc_disconnect_client c (Closed_for_error "User waiting timeout") (* disconnect connection anyway *)
        end
      end else begin
        c.client_error <- UserDontReplyOnTime
      end
  | NoError -> () );
  ()

(* Client connection closing handler *)
let client_disconnected sock reason c =
  (match c.client_sock with
  | Connection csock -> 
      if not (sock == csock) then 
        if !verbose_msg_clients || !verbose_unexpected_messages then
          lprintf_nl "  On (client_disconnected sock reason c) sock <> c.client_sock ?!?"
  | _ -> () );
  dc_disconnect_client c reason 

(* Try to send connection messages to client, return true if sent *)
let try_connect_client c =
  (*  if connection_can_try c.client_connection_control then begin *)
  (match c.client_user with
  | Some user ->
      (try
        List.iter (fun s ->              (* find first server we are connected to iter users servers *)
          (match s.server_sock with      (* send to only one server *) 
          | Connection sock ->           (* if we are connected to this server already *)
              if user.user_state = TryingToSendFirstContact then begin
                if !!firewalled then begin                     (* if we are in passive mode *)
                  user.user_state <- UserActiveUserInitiating; (* mark user to be connecting to/for us *)
                  dc_send_msg sock (
                    let module C = RevConnectToMe in
                    RevConnectToMeReq {
                      C.orig = s.server_last_nick;
                      C.dest = user.user_nick;
                    }
                  );
                end else begin                                 (* if we are in active mode *)
                  user.user_state <- UserActiveMeInitiating;   (* mark user to be connecting us directly *)
                  dc_send_msg sock (
                    let module C = ConnectToMe in
                    ConnectToMeReq {
                      C.nick = user.user_nick;
                      C.ip = CommonOptions.client_ip (Some sock);
                      C.port = !!dc_port;
  }
                  )
                end;
                raise BreakIter
              end
          | _ -> () ) (* do nothing if we are not already connected to this server *)
        ) user.user_servers;
      with _ -> () );
      
      if user.user_state = TryingToSendFirstContact then begin (* if no connection try was sent at all *)
        (match c.client_state with 
        | DcDownloadConnecting (f,_) -> c.client_state <- DcDownloadWaiting f
        | DcDownloadListConnecting _ -> c.client_state <- DcDownloadListWaiting
        | _ -> lprintf_nl "Wrong client state on trying to connect" );
        new_client_error c UserNotReachable;
        user.user_state <- UserIdle;                           (* go back to waiting *)
        false
      end else true
  | _ -> lprintf_nl "no user for client"; false )
  (*  end else begin
    lprintf_nl "c.client_connection_control denies connection to %s" c.client_name
  end *)

(* Ask all files sources for download activation *)
let ask_file_sources_for_download f =
  (try
    List.iter (fun c ->
      (match c.client_user with
      | Some u ->
          if (can_user_start_downloading u) then begin (* check if download can be started *)
            (match c.client_file with 
            | Some f ->
                if (can_file_start_downloading f) then begin
                  c.client_state <- DcDownloadConnecting (f,current_time ());
                  u.user_state <- TryingToSendFirstContact;
                  if try_connect_client c then raise (Found_client c)
                end
            | _ -> () ) 
          end else begin                               (* otherwise do clients timeout checkings here *)
            (match c.client_state with
            | DcDownloadListConnecting (_,_,time)
            | DcDownloadConnecting (_,time) ->
                if (current_time () -. time) > float_of_int !!client_timeout then begin (* if waiting timeout is reached *)
                  new_client_error c UserDontReplyOnTime;
                end
            | _ -> () )
          end
      | _ -> () )
    ) f.file_clients;
    None 
  with
  | Found_client c -> Some c
  | Not_found -> None )

(* Check a user pending downloads if they can be started, return client *)
let ask_user_for_download u = 
  (try
    if (can_user_start_downloading u) then begin (* check if download can be started *)
      List.iter (fun c ->                        (* that have clients ... *)
        if (is_client_waiting c) then begin
          (match c.client_file with
          | Some f ->   
              c.client_state <- DcDownloadConnecting (f,current_time ());
              u.user_state <- TryingToSendFirstContact;
              if try_connect_client c then raise (Found_client c)
              else raise Not_found
          | _ -> () )
        end else begin                          (* otherwise check possible clients timeout *)
          (match c.client_state with 
          | DcDownloadListConnecting (_,_,time)
          | DcDownloadConnecting (_,time) -> 
              if (current_time () -. time) > float_of_int !!client_timeout then begin (* if waiting timeout is reached *)
                new_client_error c UserDontReplyOnTime;
              end
          | _ -> () )
        end
      ) u.user_clients;
    end else begin (* Check users, that have sent RevConnectToMe and we have sent ConnectToMe and we are waiting *) 
      check_passive_user u
    end;
    None
  with
  | Found_client c -> Some c
  | Not_found -> None )

(* Check all users pending downloads if they can be started *)
let ask_all_users_for_files () =
  Hashtbl.iter (fun _ u ->                  (* with all users .. *)
    ignore (ask_user_for_download u);
  ) users_by_name;
  ()

(* Try to resume all files *)
let try_to_resume_files () =
  List.iter (fun f ->                       (* with all files *)

    ignore (ask_file_sources_for_download f)
  ) !current_files
  
(* Get clients server and send contact messages to client *)  
let init_connection c sock =   
  c.client_receiving <- Int64.zero;
  c.client_sock <- Connection sock;
  connection_ok c.client_connection_control;  
  (match c.client_state with
  | DcConnectionStyle (ClientActive _ ) -> 
      (* we are sending to unknown client and have to decide correct state later *) 
      let my_nick =
        (match c.client_user with
        | Some user ->
            (match user.user_servers with
            | [] -> local_login () 
            | s :: _ -> s.server_last_nick ) (* pick first servers nick that is known to both... *)
        | _ -> local_login () )
      in
      dc_send_msg sock (MyNickReq my_nick); (* send nick and lock requests to client *)
      dc_send_msg sock (LockReq {
          Lock.info = empty_string;
          Lock.key = DcKey.create_key;
          Lock.extended_protocol = true
      })
  | DcDownloadListConnecting _
  | DcConnectionStyle (MeActive _ ) -> ()
  | _ ->
      if !verbose_unexpected_messages then lprintf_nl "In (init_connection) c.client_state was invalid";
      raise Not_found )

let print_client_error sock txt =
  ignore (Printf.sprintf "  %s (%s)" txt
    (match find_sockets_client sock with
    | Some c -> (clients_username c)
    | _ -> "(Client not found)" )
  )
    
(* Client handler for if proper connection yet not exist *)
let client_handler sock event =
   (match event with
   | BASIC_EVENT LTIMEOUT ->
       if !verbose_msg_clients then
         print_client_error sock "BASIC_EVENT LTIMEOUT"; 
       close sock Closed_for_timeout
   | BASIC_EVENT RTIMEOUT ->
       if !verbose_msg_clients then
         print_client_error sock "BASIC_EVENT RTIMEOUT";
       close sock Closed_for_timeout
   (*| WRITE_DONE -> lprintf_nl "  Event: WRITE_DONE";
   | CAN_REFILL -> lprintf_nl "  Event: CAN_REFILL";
   | CONNECTED -> lprintf_nl "  Event: CONNECTED";*)
   | BUFFER_OVERFLOW -> 
       if !verbose_msg_clients then
         print_client_error sock "Event: BUFFER_OVERFLOW";
       close sock Closed_for_overflow
   (*| READ_DONE _ -> lprintf_nl "  Event: READ_DONE";*)
   | BASIC_EVENT (CLOSED reason) -> 
       (match find_sockets_client sock with
       | Some c ->
           if !verbose_msg_clients then
             lprintf_nl "BASIC_EVENT CLOSED: (%s) (%s)" (closing_reason_to_text reason) (clients_username c);
           new_client_error c ClosedOnInit;       
           dc_disconnect_client c reason;  
       | _ -> 
           if !verbose_msg_clients then
             lprintf_nl "BASIC_EVENT CLOSED: No client exists for socket on CLOSE" )
   | BASIC_EVENT WTIMEOUT -> 
       if !verbose_msg_clients then
         print_client_error sock "BASIC_EVENT WTIMEOUT";
       close sock Closed_for_timeout
   (* | BASIC_EVENT CAN_READ -> lprintf_nl "  Event: Other BASIC_EVENT CAN_READ"*)
   (* | BASIC_EVENT CAN_WRITE -> lprintf_nl "  Event: Other BASIC_EVENT CAN_WRITE"*)
   | _ -> () )

(* Get first message from totally new client, return new client *)
let read_first_message t sock =
  (match t with 
  | MyNickReq n ->                         (* if very first client to client message is $MyNick, then continue... *)
      let ip,port as peer_addr = TcpBufferedSocket.peer_addr sock in
      if !verbose_msg_clients then lprintf_nl "Received FIRST MyNick with name %S from %s:%u" n (Ip.to_string ip) port;
      (try
        let u = search_user_by_name n in   (* check if user with this name exists *)
        let c =
          (match u.user_state with
          | UserActiveMeInitiating ->      (* client already present, find the right one *)
              (try
                List.iter (fun fc ->
                  (match fc.client_state with
                  | DcDownloadListConnecting _ | DcDownloadConnecting _ -> raise (Found_client fc)
                  | _ -> () )
                ) u.user_clients;
                if !verbose_msg_clients || !verbose_unexpected_messages then 
                  lprintf_nl "In FIRST MyNick users client (%s) state not correct" u.user_nick;
            raise Not_found
              with
              | Found_client fc -> fc )
          | UserPassiveUserInitiating _ -> (* create new client *)
              let c = new_client () in
              c.client_name <- Some n;
              add_client_to_user c u;
              c
          | _ ->
              if !verbose_msg_clients || !verbose_unexpected_messages then
                  lprintf_nl "In FIRST MyNick user (%s) state not correct" n;
              raise Not_found ) 
        in
        set_client_state c (Connected 0);
        TcpBufferedSocket.set_closer sock (fun _ reason -> client_disconnected sock reason c);
        (match c.client_state with
        | DcDownloadListConnecting _ -> () 
        | _ -> 
            (match u.user_state with
            | UserPassiveUserInitiating _ ->
                c.client_state <- DcConnectionStyle (MeActive (Download 0)) (* level is set after $Directions *)
            | UserActiveMeInitiating ->
                c.client_state <- DcConnectionStyle (MeActive (Upload 0))
            | _ ->
                if !verbose_msg_clients || !verbose_unexpected_messages then
                  lprintf_nl "Should not happen: In FIRST MyNick user (%s)" n;
                raise Not_found ) );
        u.user_state <- UserIdle;          (* initialize user_state for later correct usage *)
        c.client_addr <- Some peer_addr;
        init_connection c sock;
        Some c                             (* return client *)
      with _ -> 
        close sock (Closed_for_error "Closed in FIRST MyNick");
        None )                            (* return no client *)
  | _ ->                                  (* all other first messages are ignored and connection is closed *)
      if !verbose_msg_clients then
        lprintf_nl "In FIRST message from client: not MyNick"; 
      close sock (Closed_for_error "First message not MyNick");
      None )                              (* return no client *)

(* Get combination on own and client supports *)
let get_client_supports c = (* return ( xmlbzlist , adc ,tthf )  xmlbzlist means also ugetblock *)  
  let xmlbzlist , adc , tthf =
    (match c.client_supports with
    | Some c_supports ->
        (mldonkey_dc_client_supports.xmlbzlist && c_supports.xmlbzlist), (* own support && clients support *)
        (mldonkey_dc_client_supports.adcget && c_supports.adcget),
        (mldonkey_dc_client_supports.tthf && c_supports.tthf) 
    | None -> false,false,false )
  in 
  xmlbzlist , adc, tthf

(* Send download commands to client *) 
let dc_send_download_command c sock =
  let xmlbzlist, adc, tthf = get_client_supports c in
  let name, from_pos =
    match c.client_state with
    | DcDownload file ->
        let separator = String2.of_char '/' in
        let fname = file.file_directory ^ separator ^ file.file_name in
        let preload_bytes =                                      (* calculate preread bytes position *) 
          let from_pos = file_downloaded file in
          if from_pos < int64_kbyte then begin                   (* if read under 1k bytes from client, start over *)
            c.client_pos <- Int64.zero;
            0
          end else begin
            c.client_pos <- from_pos;
            !dc_download_preread 
          end
        in
        c.client_preread_bytes_left <- preload_bytes;
        `Normal (fname, file.file_unchecked_tiger_root), c.client_pos -- (Int64.of_int preload_bytes)
    | _ ->
        c.client_pos <- Int64.zero;
        `List (if xmlbzlist then mylistxmlbz2 else mylist), c.client_pos
  in
  if !verbose_msg_clients || !verbose_download then
  begin
    let (fname,tth) = match name with `Normal (name,tth) -> name,tth | `List name -> name,"" in
    lprintf_nl "Sending $Get/$ADCGET: (%s)(%s)(%s)(%Ld)" (clients_username c) fname tth from_pos;
  end;
  let msg = match adc, tthf, name with
  | true, true, `Normal (_,tth) when tth <> "" ->
    AdcGetReq {
      AdcGet.adctype = AdcFile (NameTTH tth);
      start_pos = from_pos;
      bytes = Int64.minus_one;                        (* TODO load file from from_pos to anywhere *)
      zl = false;
    }
  | true, _, `List name ->
    AdcGetReq {
      AdcGet.adctype = AdcFile (NameSpecial name); (* FIXME AdcList *)
      start_pos = from_pos;
      bytes = Int64.minus_one;
      zl = false;
    }
  | _, _, (`Normal (name,_) | `List name) ->
    if xmlbzlist then (* if client supports ugetblock ...*)
    UGetBlockReq {
      UGetBlock.ufilename = name;
      UGetBlock.ubytes = Int64.minus_one;
      UGetBlock.upos = from_pos;
    }
    else (* else send normal GET *)
    GetReq {
      Get.filename = name;
      Get.pos = Int64.succ from_pos }
  in
  dc_send_msg sock msg

(* clients messages normal reader *) 
let rec client_reader c t sock =

  (match t with

  | DirectionReq t -> 
      (*if !verbose_msg_clients then lprintf_nl "Received $Direction (%s)" (clients_username c);*)
      (match c.client_state with
      | DcDownloadListConnecting (our_level,_,_)             (* We are downloading filelist *)
      | DcConnectionStyle (ClientActive (Upload our_level))  (* We are in passive mode *)
      | DcConnectionStyle (MeActive (Upload our_level)) ->   (* We are in active mode, client needs to upload) *)
           (match t.Direction.direction with
           | Download _ ->
               if !verbose_msg_clients then 
                 lprintf_nl "We have a conflict with (%s), both want to download..." (clients_username c);
               if (t.Direction.level > our_level) then begin (* client gets to start download first *)
                 if !verbose_msg_clients then lprintf_nl "  Client won the election...";
                 (match c.client_state with                  (* memorize list loading if that is the case *) 
                 | DcConnectionStyle _ ->                    (* if file was tried to download ... *) 
                     let nc = new_copy_client c in
                     nc.client_sock <- NoConnection;
                     nc.client_addr <- None;
                     (match c.client_file with
                     | Some file ->
                         add_client_to_file nc file;
                         (match c.client_user with
                         | Some user ->
                             add_client_to_user nc user;
                         | _ -> () ); 
                         nc.client_state <- DcDownloadWaiting file
                     | None -> () );  
                     remove_client_from_clients_file c
                 | _ -> (* DcDownloadListConnecting *)       (* if filelist was tried to download *)
                     let nc = new_copy_client c in
                     nc.client_sock <- NoConnection;
                     nc.client_addr <- None;
                     (match c.client_user with
                     | Some user -> add_client_to_user nc user
                     | _ -> () );
                     nc.client_state <- DcDownloadListWaiting );
                                                              (* we change our direction *)
                 (match c.client_state with                   (* check which one is the case *)
                 | DcConnectionStyle (ClientActive (Upload _)) -> (* if client was initiating *)
                     c.client_state <- DcConnectionStyle (ClientActive (Download 65535)) (* 65535 means to KeyReq that *)
                 | DcConnectionStyle (MeActive (Upload _))    (* direction is changed    *)
                 | DcDownloadListConnecting _ ->              (* if we were initiating *)
                     c.client_state <- DcConnectionStyle (MeActive (Download 65535))
                 | _ -> () );
                 (* we check in GetReq if we can start a new download immediately *)
                 
               end else if (t.Direction.level < our_level) then begin (* we win and start downloading *)
                 if !verbose_msg_clients then lprintf_nl "  We won the election..."
               end else                                               (* otherwise close connection *)       
                 if !verbose_msg_clients then
                   lprintf_nl "  Stalemate (levels are equal), closing";
                 close sock (Closed_for_error "Negotiation download: Stalemate" )
           | _ -> () ) (* Upload *)
      | DcConnectionStyle (MeActive (Download our_level))
      | DcConnectionStyle (ClientActive (Download our_level)) -> (* connection is ready for uploading             *)
          (match t.Direction.direction with
          | Upload level ->                          (* Active mode and client wants to upload too ?? *)
             if !verbose_msg_clients then lprintf_nl "We have a conflict, both want to upload...";
             (match c.client_state with
             | DcConnectionStyle MeActive _ ->
                 if !verbose_msg_clients then  
                   lprintf_nl "  and client (%s) is in passive mode" (clients_username c)
  | _ ->
                 if !verbose_msg_clients then 
                   lprintf_nl "  and client (%s) is in active mode" (clients_username c) );
             close sock (Closed_for_error "Negotiation upload: conflict" );
          | _ -> () ) (* Download *)
      |  _ ->
          if !verbose_msg_clients || !verbose_unexpected_messages then  
            lprintf_nl "In Direction: client state invalid";
          close sock (Closed_for_error "Negotiation: client state invalid" ) )
  
  | ErrorReq errortxt
  | FailedReq errortxt -> 
  if !verbose_msg_clients then begin
        (match t with
        | ErrorReq _ -> lprintf_nl "Received (%s) from (%s)" errortxt (clients_username c)
        | _ -> lprintf_nl "Received (%s) from (%s)" errortxt (clients_username c)) 
    end;
      (match String2.split_simplify errortxt ' ' with 
      | [ _ ; "File" ; txt1 ; txt2 ] -> 
          (* $Error File Not Available
             $Error File not available *)
          if (String.length txt1 = 3) && (txt2.[1] = 'v') then new_client_error c FileNotAvailable
      | _ -> lprintf_nl "New errortext: (%s) - make handling ??" errortxt );
      close sock (Closed_for_error (Printf.sprintf "From client (%s): (%s)" (clients_username c) errortxt) )
  
  | FileLengthReq _
  | AdcSndReq _ ->
      (try 
        if !verbose_msg_clients then begin
          (match t with 
          | FileLengthReq _ -> lprintf_nl "Received $FileLength from (%s)" (clients_username c)
          | _ -> lprintf_nl "Received $AdcSnd from (%s)" (clients_username c) )   (* AdcSnd *)
        end;
        TcpBufferedSocket.set_rtimeout sock (float !!client_read_timeout); 
        (match c.client_state with
        | DcDownload file ->
            let bytes =
              (match t with
              | FileLengthReq t -> t
              | AdcSndReq t ->                    (* check file current position with to be sended data position *)
                  let size = file_downloaded file in
                  if !verbose_download then 
                    lprintf_nl "AdcSnd: file_downloaded=(%Ld) preread=(%d) start_pos=(%Ld)"
                      size c.client_preread_bytes_left t.AdcSnd.start_pos;
                  if size -- (Int64.of_int c.client_preread_bytes_left) = t.AdcSnd.start_pos then begin
                    if t.AdcSnd.bytes = Int64.minus_one then file_size file else t.AdcSnd.bytes
                  end else begin
                    if !verbose_unexpected_messages || !verbose_download then
                      lprintf_nl "AdcSnd: Current file=(%s) size=(%Ld) don't match start_pos=(%Ld) for user=(%s)"
                        file.file_name size t.AdcSnd.start_pos (clients_username c);
          raise Not_found
                  end
              | _ -> raise Not_found )
            in
            c.client_receiving <- bytes;
            c.client_error <- NoError;
            file_add file.file_file FileDownloading;
            (match t with
            | FileLengthReq _ -> 
                dc_send_msg sock SendReq
            | _ -> () )                           (* AdcSnd *) 

        | DcDownloadListConnecting _ ->
            let filelist_name = Filename.concat filelist_directory (
              (match c.client_user with
              | Some u -> 
                (match c.client_supports with
                | Some c_supports ->
                    if c_supports.xmlbzlist then u.user_nick ^ mylistxmlbz2_ext
                    else u.user_nick ^ mylist_ext
                | None -> u.user_nick ^ mylist_ext )
              | None -> failwith "No User" )
            ) in
            if !verbose_msg_clients || !verbose_download then 
              lprintf_nl "Creating filelist with name: (%s)" filelist_name;  
            let filelist_fd = Unix32.create_rw filelist_name in
            let bytes =
              (match t with
              | FileLengthReq t -> t
              | AdcSndReq t ->              (* check that adc client send the size of file in here *)
                  if t.AdcSnd.bytes > Int64.zero then t.AdcSnd.bytes
                  else failwith "Wrong bytes in AdcSnd"
              | _ -> raise Not_found )
            in
            c.client_state <- DcDownloadList filelist_fd; 
            c.client_receiving <- bytes; 
            c.client_error <- NoError;
            (match t with
              | FileLengthReq _ -> 
                  dc_send_msg sock SendReq
              | _ -> () ) (* AdcSnd *)
        | _ ->
            failwith "Nothing to download" ) 
    with e ->
      if !verbose_unexpected_messages then
        lprintf_nl "Exception (%s) FileLength/AdcSnd:" (Printexc2.to_string e);
      close sock (Closed_for_error (Printexc2.to_string e)) )

  | AdcGetReq _
  | GetReq _ 
  | UGetBlockReq _ -> (* TODO downloading a section of file *) (* TODO state checking ? *)

      if (c.client_state = DcUploadDoneWaitingForMore) then begin (* if this is a continual loading *) 
        if !verbose_upload || !verbose_msg_clients then lprintf_nl "  Continuing upload/slot";
        TcpBufferedSocket.set_lifetime sock infinite_timeout;     (* restore connection lifetime *) 
      end;

      let direction_change =                                    (* memorize possible direction change *)
        (match c.client_state with
        | DcConnectionStyle MeActive Download 65535
        | DcConnectionStyle ClientActive Download 65535 -> true (* these mean direction change and we have lost *)
        | _ -> false );
      in

      begin try

      let req = 
        match t with
        | AdcGetReq { AdcGet.zl = true; _ } ->
            failwith "ZLib not yet supported"

        | AdcGetReq { AdcGet.adctype = AdcList (dir,re1); _ } -> `PartialList (dir,re1)

        | AdcGetReq { AdcGet.adctype = AdcFile (NameSpecial name); _ }
        | GetReq { Get.filename = name; _ }
        | UGetBlockReq { UGetBlock.ufilename = name; _ } 
            when name = mylist || name = mylistxmlbz2 -> `FullList name

        | AdcGetReq { AdcGet.adctype = AdcFile (NameSpecial name); _ } ->
            failwith ("ADCGET special name not supported : " ^ name)

        | AdcGetReq { AdcGet.adctype = AdcFile (NameTTH tth); start_pos=start; bytes=bytes; _ } ->
            `File (`TTH tth, start, bytes)

        | GetReq t ->
            let name = String2.replace t.Get.filename char92 "/" in
            `File (`Name name, Int64.pred t.Get.pos, Int64.minus_one)

        | UGetBlockReq t ->
            let name = String2.replace t.UGetBlock.ufilename char92 "/" in
            `File (`Name name, t.UGetBlock.upos, t.UGetBlock.ubytes)

        | _ -> failwith "Unexpected request"
      in
      match req with
      | `FullList name ->
        lprintf_nl "Client %S requested FullList %s" (clients_username c) name;

        let mylist_filename = Filename.concat directconnect_directory name in
        c.client_state <- DcUploadListStarting mylist_filename;
        c.client_pos <- Int64.zero;
        let size = Unix32.getsize mylist_filename in
        begin match t with
        | AdcGetReq t ->
            dc_send_msg sock (AdcSndReq {
              AdcSnd.adctype = t.AdcGet.adctype;
              AdcSnd.start_pos = 0L;
              AdcSnd.bytes = size;
              AdcSnd.zl = false; (* CHECK *)
            });
            client_reader c SendReq sock                 (* call ourselves again with send starting *)
        | _ ->                                           (* GetReq _ | UGetBlockReq _ *)
            dc_send_msg sock (FileLengthReq size)
        end

      | `PartialList (dir,_re) ->
          lprintf_nl "Client %s requested PartialList %s" (clients_username c) dir;

          let mylist = try DcShared.make_xml_mylist (DcShared.find_dir_exn dir) 
            with exn -> failwith (Printf.sprintf "PartialList %s : %s" dir (Printexc2.to_string exn))
          in 
          let filename = CommonFile.concat_file directconnect_directory
            (Printf.sprintf "mylist.%s.partial.xml.bz2" (clients_username c))
          in
          DcShared.buffer_to_bz2_to_file mylist filename;
          c.client_state <- DcUploadListStarting filename;
          c.client_pos <- Int64.zero;
          let size = Int64.of_int (Buffer.length mylist) in
          begin match t with
          | AdcGetReq t ->
              dc_send_msg sock (AdcSndReq {
                AdcSnd.adctype = t.AdcGet.adctype;
                AdcSnd.start_pos = 0L;
                AdcSnd.bytes = size;
                AdcSnd.zl = false; (* CHECK *)
              });
              client_reader c SendReq sock                 (* call ourselves again with send starting *)
          | _ ->                                           (* GetReq _ | UGetBlockReq _ *)
              assert false
          end

      | `File (name, start_pos, bytes) -> (* client wants normal file *) 
          let dcsh = match name with
            | `TTH tth ->
              (try                                       (* lets find file by tth       *)
                Hashtbl.find dc_shared_files_by_hash tth
              with _ ->
                failwith (Printf.sprintf "Shared file not found by tth %S" tth))
            | `Name fname ->
              (try                                       (* so lets find filename then     *)
                Hashtbl.find dc_shared_files_by_codedname fname 
              with _ ->
                failwith (Printf.sprintf "Shared file not found by codedname %S" fname))
          in
          lprintf_nl "Client %S wants to download %S (%s) %Ld bytes from pos: %Ld" (clients_username c) 
              dcsh.dc_shared_fullname dcsh.dc_shared_tiger_root bytes start_pos;
          (* check if upload still exists *)
          c.client_pos <- start_pos;
          let rem = dcsh.dc_shared_size -- c.client_pos in 
          if dc_can_upload () || (counts_as_minislot dcsh.dc_shared_size) then 
          begin   (* if free slots or file size *) 
            if not (counts_as_minislot dcsh.dc_shared_size) then dc_insert_uploader ();(* increase uploaders *)
            c.client_state <- DcUploadStarting (dcsh,start_pos,bytes);
            (match t with
            | AdcGetReq t ->
                dc_send_msg sock (AdcSndReq {
                  AdcSnd.adctype = t.AdcGet.adctype;
                  start_pos = start_pos;
                  bytes = bytes;
                  zl = false; (* CHECK *)
                } );
                client_reader c SendReq sock             (* call ourselves again with send starting *)
            | _ ->                                       (* GetReq _ | UGetBlockReq _ *)
                dc_send_msg sock (FileLengthReq rem) )

          end else begin
            (*lprintf_nl "Sending MaxedOut to (%s)" (clients_username c);*)
            dc_send_msg sock MaxedOutReq;
            close sock (Closed_for_error ("By us: Maxedout")) 
          end
      with exn ->
          if !verbose_upload then
            lprintf_nl "Error answering GET/ADCGET: %s" (Printexc2.to_string exn);
          let errortxt = "File Not Available" in 
          begin match t with
          | AdcGetReq _
          | GetReq _ ->
              dc_send_msg sock (ErrorReq errortxt) 
          | _ ->                                       (* UGetBlockReq _ *)
              dc_send_msg sock (FailedReq errortxt) 
          end; 
          close sock (Closed_for_error ("By us:" ^ errortxt))
      end;
      if direction_change then begin                   (* now the users clients states wont interfere this check *)
        (match c.client_user with                      (* we can check if we can start new download immediately  *)
        | Some user ->
            lprintf_nl "Because we lost conflict we now try to start new download from %s" user.user_nick;
            ignore (ask_user_for_download user)
        | _ -> () );
              end

  | GetListLenReq -> ()

  | KeyReq _ -> 
      (*lprintf_nl "Received $Key ... dumping it";*)
      (*lprintf_nl "Client state: %s" (client_state_to_string c);*)
      let level = Random.int 32767 in
      let send_downloading_command dir c = (* inside Key function ... *)
        (match dir with                 (* Send first $Get if necessary *)
        | Upload _ ->                   (* sent we want to download and client needs to be uploading part *)
            (match c.client_file with   (* here we set the downloading file back again to client state *)
            | None ->
                close sock (Closed_for_error "Nothing to download")
            | Some file ->
                c.client_state <- DcDownload file;
                dc_send_download_command c sock )
        | _ -> () )                     (* we are uploading and wait for $Get now *)
      in
      (match c.client_state with
      | DcDownloadListConnecting (_,passive,time) ->  
          (match passive with (* if we were/are in passive mode *)
          | true ->           
              (*lprintf_nl "Connection state is: DcDownloadListConnecting )"; *)
              (match c.client_supports with (* send $Supports if necessary *)
              | None -> ()
              | Some dc_client_supports -> 
                  dc_send_msg sock ( SupportsReq (ClientSupports mldonkey_dc_client_supports) ) );
              c.client_state <- DcDownloadListConnecting (level,true,time); (* memorise $Direction level *)
              dc_send_msg sock ( DirectionReq {
                Direction.direction = Download level; Direction.level = level } );
              dc_send_msg sock ( KeyReq { Key.key = DcKey.calculate_key c.client_lock })
          | _ -> () );
          dc_send_download_command c sock;

      | DcConnectionStyle ( ClientActive dir ) ->
          (match dir with                     (* check that direction was not changed on election *)
          | Download 65535 -> ()              (* if was, do nothing and wait the Get from client   *) 
        | _ ->
              (match c.client_supports with   (* send $Supports if necessary *)
              | None -> ()
              | Some dc_client_supports ->    (* if EXTENDEDPROTOCOL supported by client, send own $Supports *)
                  dc_send_msg sock ( SupportsReq (ClientSupports mldonkey_dc_client_supports) ) );

              (match dir with                 (* send $Direction *)
              | Upload _ ->                   (* client seems to be uploading so ... *) 
                  c.client_state <- DcConnectionStyle (ClientActive (Upload level)); (* set level *)
                  dc_send_msg sock ( DirectionReq {             (* we thank and send Download *)
                    Direction.direction = Download level; Direction.level = level } )
              | Download _ ->                 (* clients want to download from us ... *)
                                              (* we send possible no slot later *)  
                  c.client_state <- DcConnectionStyle (ClientActive (Download level));
                  dc_send_msg sock ( DirectionReq {           (* we prepare for uploading file *)
                    Direction.direction = Upload level; Direction.level = level } ) ); 

              dc_send_msg sock (                                
                KeyReq { Key.key = DcKey.calculate_key c.client_lock });

              send_downloading_command dir c )

      | DcConnectionStyle (MeActive dir ) -> 
          (match dir with                     (* check that direction was not changed on election *)
          | Download 65535 -> ()              (* if was, do nothing and wait the Get from client   *)
          | _ -> send_downloading_command dir c )
  
      | _ -> () )
      
  | LockReq lock ->
      (*lprintf_nl "Received $Lock";*)
      (*lprintf_nl "Client state: %s" (client_state_to_string c);*)
      c.client_lock <- lock.Lock.key;       (* save the clients lock for later use *)

      (match c.client_state with
      | DcDownloadListConnecting _
      | DcConnectionStyle ( MeActive _ ) -> (* we are answering to a connection initialized by passive client *)
          let dir =                         (* lets set dir to DcDownloadListConnecting also ... *)
            (match c.client_state with
            | DcDownloadListConnecting (level,_,_) -> Upload level
            | DcConnectionStyle ( MeActive dir ) -> dir
            | _ -> Upload 0 )               
          in
          let my_nick =
            (match c.client_user with
            | Some user ->
                (match user.user_servers with
                | [] -> local_login ()
                | s :: _ -> s.server_last_nick ) (* pick first servers nick that is known to both... *)
            | _ -> local_login () )
          in
          dc_send_msg sock (MyNickReq my_nick); (* send nick and lock requests to client *)
          dc_send_msg sock (LockReq {
              Lock.info = empty_string;
              Lock.key = DcKey.create_key;
              Lock.extended_protocol = true
            } );
          dc_send_msg sock ( SupportsReq (ClientSupports mldonkey_dc_client_supports) );
          let level = Random.int 32767 in
          (match dir with
          | Upload _ -> 
              (match c.client_state with
              | DcConnectionStyle _ -> c.client_state <- DcConnectionStyle ( MeActive (Upload level))
              | _ -> () (* DcDownloadListConnecting *) );
              dc_send_msg sock ( DirectionReq {
                Direction.direction = Download level; (* we are downloading *)
                Direction.level = level } )
          | Download _ -> (* we set level to 0 so that we lose possible conflict all the time purposely *)
              (match c.client_state with
              | DcConnectionStyle _ -> c.client_state <- DcConnectionStyle ( MeActive (Download 0))
              | _ -> () (* DcDownloadListConnecting *) );
              dc_send_msg sock ( DirectionReq {
                Direction.direction = Upload 0;   (* we are uploading *)
                Direction.level = level } ) );
          dc_send_msg sock (KeyReq { Key.key = DcKey.calculate_key c.client_lock } );
          if !verbose_msg_clients then 
            lprintf_nl "Sent answer to (%s) (MyNick,Lock,Supports,Direction,Key)" (clients_username c)
      | _ -> () )

  | MaxedOutReq ->
      (*lprintf_nl "Received MaxedOut";*)
      new_client_error c NoFreeSlots; 
      close sock (Closed_for_error "MaxedOut from client")      

  | MyNickReq n -> 
      if !verbose_msg_clients then
        lprintf_nl "Received Normal $MyNick with nick (%s)" n;
      (*lprintf_nl "Client state: %s" (client_state_to_string c);*)
      connection_ok c.client_connection_control;
      (try 
        let u = search_user_by_name n in   (* connect first correct user and client together *)
        (match u.user_state with
        | UserActiveUserInitiating ->      (* RevConnect sent, another client present already *)
                                           (* Now we have to swap clients info *)
            (try
              List.iter (fun fc ->
                (match c.client_state with
                | DcConnectionStyle ClientActive Upload 0 -> raise (Found_client fc)
                | _ -> () )
              ) u.user_clients;
              failwith "Not found client with correct state"
            with 
            | Found_client fc ->
                (match fc.client_file with
                | Some file -> add_client_to_file c file;
                | _ -> () );
                remove_client_from_clients_file fc;
                remove_client fc )
        | UserIdle -> ()
        | UserPassiveUserInitiating _ (* ConnectToMe sent as answer to RevConnect, should not hapen here *)
        | UserActiveMeInitiating      (* ConnectToMe sent, another client already present, should not happen in here *)
        | _ ->
            failwith "User state is wrong" );
              
        add_client_to_user c u;
        c.client_name <- Some u.user_nick;
        set_client_state c (Connected 0);
              
        (match c.client_state with         (* now decide correct state *)
        | DcDownloadListConnecting _ -> () (* if client state is filelist downloading... *)
        | _ -> 
            (match u.user_state with
            | UserActiveUserInitiating ->  (* we sent RevConnect ... *)
                c.client_state <- DcConnectionStyle (ClientActive (Upload 0)); (* level assigned later *)
            | UserIdle ->                  (* totally new connection initialized by client *)  
                c.client_state <- DcConnectionStyle (ClientActive (Download 0))
            | _ ->
                failwith "Invalid user state" ) 
        );
        u.user_state <- UserIdle;        (* not needed anymore *)
      with e -> 
        if !verbose_unexpected_messages || !verbose_msg_clients then
          lprintf_nl "In normal MyNick: (%s) when received nick=(%s)" (Printexc2.to_string e) n;
        close sock (Closed_for_error "Error in $MyNick") )
  
  | SendReq ->
      (*lprintf_nl "Received or commanded $Send";*)
      (try
        (match c.client_state with
        | DcUploadListStarting fname -> 
            let file_fd = Unix32.create_ro fname in
            c.client_state <- DcUploadList file_fd; 
            c.client_endpos <- Unix32.getsize64 file_fd;
            let file = new_upfile None file_fd (CommonUserDb.admin_user ()) in (* FIXME user? *)
            c.client_file <- Some file;
            set_clients_upload c (as_file file.file_file);
        | DcUploadStarting (dcsh,start_pos,bytes) -> 
            let endpos =
              if bytes = Int64.minus_one then dcsh.dc_shared_size
              else begin
                let client_wants = start_pos ++ bytes in    (* if client requests too much data *)
                if client_wants > dcsh.dc_shared_size then failwith "Start_pos + bytes > dcsh.dc_shared_size"
                else client_wants
                end 
                in
            let file_fd = Unix32.create_ro dcsh.dc_shared_fullname in
            c.client_state <- DcUpload (dcsh,file_fd,start_pos,bytes);
            c.client_endpos <- endpos;
            let file = new_upfile (Some dcsh) file_fd (CommonUserDb.admin_user ()) in (* FIXME user? *)
            c.client_file <- Some file;
            set_clients_upload c (as_file file.file_file);
        | _ -> failwith "Wrong client state in Send" );

        set_client_has_a_slot (as_client c.client_client) NormalSlot;
        (*client_enter_upload_queue (as_client c.client_client);*)
        TcpBufferedSocket.set_wtimeout sock (float !!client_write_timeout)

      with e ->
          lprintf_nl "Exception %s in upload creation" (Printexc2.to_string e);
          close sock (Closed_for_error "Error in upload creation");
          failwith "Error in upload creation"  )

  | SupportsReq t ->                                        (* After EXTENDEDPROTOCOL support list from client ... *)
      (*lprintf_nl "Received $Supports";*)
      (match t with
      | ClientSupports t -> c.client_supports <- Some t     (* Save supports into clientdata *)
      | _ -> () )

  | UnknownReq s ->
      if s <> "" then
        if !verbose_unexpected_messages || !verbose_msg_clients then begin
          let l = String.length s in
          let txt = Printf.sprintf "Unknown client message: (%s)" (clients_username c) in
          if l > 50 then lprintf_nl "%s (%s...%d chars)" txt (shorten_string s 50) l
          else lprintf_nl "%s (%s)" txt s
        end
        
  | _ ->
      lprintf_nl "--> Unhandled client message. Implement ?:";
      DcProtocol.dc_print t )

(* Find next download from this user/client *)
let find_next_client c =
  (match c.client_user with
  | None -> None
  | Some u -> 
      if !verbose_download then lprintf_nl "Trying to find next download to user (%s)" u.user_nick;
      (try
        List.iter (fun cl ->         (* check first if filelist is waiting ... *)
          (match cl.client_state with
          | DcDownloadListWaiting -> raise (Found_client cl)
          | _ -> () )
        ) u.user_clients;            (* then normal downloads ... *)
        List.iter (fun cl ->
          (match cl.client_state with
          | DcDownloadWaiting _ -> raise (Found_client cl)
          | _ -> () )
        ) u.user_clients;
        None                         (* return false to calling function that closes the socket *)
      with 
      | Found_client cl -> (Some cl) (* we have a next file with existing client to download *)
      | _ -> None )
  )

(* Start next download from user and if change, current client <-> pending client *)
(* Remove other client if not change                                              *)
let next_download change c sock cl = (* c is current connection, cl is the pending download *)
  (match cl.client_state with
  | DcDownloadWaiting file ->
      if change then begin           (* we need to change current download with pending one *)
        (match c.client_state with 
        | DcDownload f ->            (* here we exchange pending client to existing client socket *)
            c.client_state <- DcDownload file;
            cl.client_state <- DcDownloadWaiting f;
            remove_client_from_clients_file c;
            remove_client_from_clients_file cl;
            add_client_to_file c file;
            add_client_to_file cl f;
        | _ -> () )
      end else begin
        remove_client_from_clients_file c; (* because file commit removes the file <-> client connection also,     *)
                                           (* this has to be done before assigning new file to this reused client, *)
                                           (* so that file remove don't erase this clients file                    *)
        add_client_to_file c file;   (* no change needed         *)
        remove_client cl;            (* remove not needed client *)
        c.client_state <- DcDownload file;
        dc_send_download_command c sock  
      end
  | DcDownloadListWaiting -> 
      if not change then begin       (* filelists changing not currently possible *)
        remove_client cl;
        c.client_state <- DcDownloadListConnecting (0,!!firewalled,nan); 
        dc_send_download_command c sock  
      end
  | _ -> () )

(* File is finished downloading, so remove file from clients list and client from files list *)
let file_complete file = 
  if !verbose_download then lprintf_nl "File %s downloaded" file.file_name;
  file_completed (as_file file.file_file);   (* update_file_state impl FileDownloaded; *)
  List.iter (fun c ->                        (* remove this files clients except current connection       *)
    (match c.client_state with               (* because we use this connection possibly for next download *)
    | DcDownload f -> ()                     (* only one client should be in this state *) 
    | _ ->
        remove_client c )  
  ) file.file_clients
  
let closing_text = "All files downloaded" 
(* Continue downloading from client that we have initialized *)  
let client_downloaded c sock nread = (* TODO check tth while loading, abort if error *)
  if nread > 0 then begin
    (match c.client_state with
    | DcDownload file ->
        let b = TcpBufferedSocket.buf sock in
        let downloaded =
          if c.client_preread_bytes_left > 0 then begin                (* if precheck not yet done *)
            let check_bytes = min nread c.client_preread_bytes_left in (* which is smaller... *) 
            let check_buffer = String.create check_bytes in
            Unix32.read (file_fd file) (c.client_pos -- (Int64.of_int c.client_preread_bytes_left))
              check_buffer 0 check_bytes;
            let str2 = Bytes.sub b.buf b.pos check_bytes in
            if (Bytes.compare check_buffer str2) = 0 then begin      (* if downloaded is ok *) 
              c.client_preread_bytes_left <- c.client_preread_bytes_left - check_bytes;
              if c.client_preread_bytes_left = 0 then begin            (* if checked all preread bytes *)
                let downloaded = b.len - check_bytes in
                if downloaded > 0 then begin                           (* check if buffer has bytes to write to file *) 
                  Unix32.write (file_fd file) c.client_pos b.buf (b.pos+check_bytes) downloaded
                end;
                Int64.of_int downloaded           
              end else Int64.zero
            end else begin                                             (* if file check failed *)
              if !verbose_download then 
                lprintf_nl "Corrupted file (%s) download from (%s)" file.file_name (clients_username c);
              c.client_state <- DcIdle;                                (* now closing sock removes the client also *)
              close sock (Closed_for_error "Corrupted file");
              Int64.zero
            end
          end else begin (* precheck done, normal flow *)
        Unix32.write (file_fd file) c.client_pos  b.buf b.pos b.len;
            Int64.of_int b.len
          end
          in
        c.client_pos <- c.client_pos ++ downloaded;
        (match c.client_user with 
        | Some u -> u.user_downloaded <- u.user_downloaded ++ downloaded 
        | _ -> () );
        c.client_downloaded <- c.client_downloaded ++ downloaded;
        buf_used b b.len;
        if c.client_pos > (file_downloaded file) then (* update downloading state *) (* TODO check tth while loading *)
          add_file_downloaded (as_file file.file_file) (c.client_pos -- (file_downloaded file));
        if (file_downloaded file) = (file_size file) then begin
          file_complete file;
          c.client_receiving <- Int64.zero;           (* this marks client as receiving normal commands again *)
          c.client_pos <- Int64.zero;
          TcpBufferedSocket.set_rtimeout sock infinite_timeout;   (* back to normal *) 
          (* update myinfo ? *)
          (match (find_next_client c) with            (* try to continue slot *)
          | Some cl -> next_download false c sock cl  (* connected client , sock , client download_waiting *)
          | None ->
              c.client_state <- DcIdle;               (* now closing sock removes the client also *)   
              close sock (Closed_for_error closing_text) )
          end
          
    | DcDownloadList filelist_fd ->                   (* downloading file list *)
        let b = TcpBufferedSocket.buf sock in        
        let len = Int64.of_int b.len in
        Unix32.write filelist_fd c.client_pos b.buf b.pos b.len;
        c.client_pos <- c.client_pos ++ len;
        (match c.client_user with
        | Some u -> u.user_downloaded <- u.user_downloaded ++ len
        | _ -> () );
        c.client_downloaded <- c.client_downloaded ++ len;
        c.client_receiving <- c.client_receiving -- len;
        buf_used b b.len;
        if c.client_receiving = Int64.zero then begin
          Unix32.close filelist_fd;
          if !verbose_download then lprintf_nl "Received filelist from (%s)" (clients_username c);
          c.client_receiving <- Int64.zero;           (* this marks client as receiving commands again *)
          c.client_pos <- Int64.zero;
          TcpBufferedSocket.set_rtimeout sock infinite_timeout;
          (match (find_next_client c) with
          | Some cl -> 
              next_download false c sock cl (* connected client , sock , client download_waiting *)
          | None ->
              c.client_state <- DcIdle;
              close sock (Closed_for_error closing_text) )
        end
    | _ -> raise Not_found )
          end
  
(* initialize a new connection when nothing is known from client *)
let init_anon_client sock =
  TcpBufferedSocket.set_read_controler sock download_control;
  TcpBufferedSocket.set_write_controler sock upload_control;
  TcpBufferedSocket.set_rtimeout sock infinite_timeout; (* client timeouts *)
  TcpBufferedSocket.set_wtimeout sock infinite_timeout;
  TcpBufferedSocket.set_reader sock (dc_handler_client (ref (None))
    read_first_message client_reader client_downloaded)  

(* create listening socket for incoming connection, return socket or None *)
let create_tcp_socket () = 
  (try
    let sock = TcpServerSocket.create "DC client listening" (Ip.to_inet_addr !!client_bind_addr) !!dc_port
        (fun sock event ->
          match event with
        | TcpServerSocket.CONNECTION (s, Unix.ADDR_INET(from_ip, from_port)) ->
            (*lprintf_nl "Listen: connection received from %s:%d" 
              (Ip.to_string (Ip.of_inet_addr from_ip)) from_port; *)
              
            (* CHECK Allow this connection or not ? *)
              let token = create_token connection_manager in
              let sock = TcpBufferedSocket.create token
              "DC client connection" s client_handler(*(fun _ _ -> ())*) in
            init_anon_client sock 
        | _ -> () )
    in
    (*lprintf_nl "Created listening socket..." ;*)
    dc_tcp_listen_sock := Some sock;
    (match (Unix.getsockname (BasicSocket.fd (TcpServerSocket.sock sock))) with 
    | Unix.ADDR_INET (addr,port) -> Some sock
    | _ -> None )
  with e -> lprintf_nl "Exception %s while initializing DC listen socket" (Printexc2.to_string e);
            None )

(* UDP *)

(* Parse udp messages *)
let udp_parse buf sock =
  if !verbose_udp then lprintf_nl "UDP Receive: (%s)" buf; 
  let str = String2.splitn buf ' ' 1 in
  (match str with
  | [cmd; args] -> 
      let module S = SR in
      let msg = S.parse (String2.replace args '|' empty_string) in (* strip following '|' from message *)  
      if msg.S.filename = empty_string then ()
        (*lprintf_nl "This result seems to be directory result, we don't support it atm."*)
      else begin
        (try
          let s = Hashtbl.find servers_by_ip msg.S.server_ip in
          received_new_search_result s msg;
        with _ -> if !verbose_udp then 
            lprintf_nl "UDP: Not valid ip-address (%s) in $SR" msg.S.server_ip)
      end
  | [cmd] -> if !verbose_udp then lprintf_nl "UDP: Unknown command %s" cmd
  | _ -> if !verbose_udp then lprintf_nl "UDP: Unknown message %s" (String.escaped buf) )
   
(* Udp sending *)
let udp_send ip port m =
  (try
    Buffer.reset buf;
    dc_write buf m; 
    Buffer.add_char buf '|';
    let s = Buffer.to_bytes buf in
    (match !dc_udp_sock with
    | Some sock -> 
        (*if !verbose_udp || !verbose_msg_clients then lprintf_nl "UDP Send: (%s)" s;*)
        UdpSocket.write sock false s ip port
    | None -> failwith "No UDP socket" );
  with e ->
    if !verbose_udp || !verbose_msg_clients then
      lprintf_nl "Exception (%s) in UDP send" (Printexc2.to_string e) )

(* Udp event handling *)
let udp_handler sock event =
  match event with
  | UdpSocket.READ_DONE -> 
      UdpSocket.read_packets sock (fun p ->
        (try
          let pbuf = p.UdpSocket.udp_content in
          let len = Bytes.length pbuf in
          if len > 0 then
            udp_parse (Bytes.to_string pbuf) sock
        with e -> () ) 
      ) 
          | _ -> ()

(* create listening udp port *)
let create_udp_socket () =
  (try
    let sock = UdpSocket.create (Ip.to_inet_addr !!client_bind_addr) !!dc_port
               (fun sock event -> udp_handler sock event) 
    in
    dc_udp_sock := Some sock;
    UdpSocket.set_write_controler sock udp_write_controler;
    Some sock
  with e ->
    lprintf_nl "Exception %s while binding UDP socket" (Printexc2.to_string e);
    None )

(* Start a connection to client *)
let connect_client c =
  let token = 
    add_pending_connection connection_manager (fun token ->
        try
          match c.client_addr with
        | None -> ()
          | Some (ip,port) ->
              connection_try c.client_connection_control;      
            let sock = TcpBufferedSocket.connect token "client connection" (Ip.to_inet_addr ip) port 
                       client_handler (*(fun _ _ -> ())*)
              in
            TcpBufferedSocket.set_read_controler sock download_control; (* CommonGlobals.download_control *)
              TcpBufferedSocket.set_write_controler sock upload_control;
            TcpBufferedSocket.set_rtimeout sock infinite_timeout; (* client timeouts *)
            TcpBufferedSocket.set_wtimeout sock infinite_timeout;
            TcpBufferedSocket.set_closer sock (fun _ reason -> client_disconnected sock reason c);
            TcpBufferedSocket.set_reader sock (dc_handler_client (ref (Some c)) read_first_message
              client_reader client_downloaded);
            init_connection c sock; (* Send first answer messages to client *)
        with e ->
        lprintf_nl "Exception: %s, while connecting to client" (Printexc2.to_string e);
    ) in
  c.client_sock <- ConnectionWaiting token
          
(* Upload to client *)
let dc_upload c bytes =
  (match c.client_sock with 
  | Connection sock ->
      (try
        if (bytes > 0) && can_write_len sock bytes then begin
          (* update upload rate from len bytes *)
          (*Rate.update c.client_upload_rate  (float_of_int len);*)
          (* update stats *)
          (*ignore (
            count_filerequest c; *)
          let file_fd =
            (match c.client_state with
            | DcUpload (_,fd,_,_) -> fd
            | DcUploadList fd -> fd
            | _ -> failwith "No fd in upload" )
          in 
          let rlen = int64_min_int (c.client_endpos -- c.client_pos) bytes in
          CommonUploads.consume_bandwidth rlen;
          let upload_buffer = String.create rlen in
          Unix32.read file_fd c.client_pos upload_buffer 0 rlen;
          TcpBufferedSocket.write sock upload_buffer 0 rlen;
          (*lprintf_nl "  Wrote (%d) bytes" rlen;*)
          let uploaded = Int64.of_int rlen in 
          c.client_pos <- c.client_pos ++ uploaded;
          dc_total_uploaded := !dc_total_uploaded ++ uploaded;
          (match c.client_user with
          | Some u -> u.user_uploaded <- u.user_uploaded ++ uploaded
          | _ -> () );
          c.client_uploaded <- c.client_uploaded ++ uploaded;
          (match c.client_state with
          | DcUpload (dcsh,_,_,_) ->
              (try
                let sh = CommonUploads.find_by_name dcsh.dc_shared_codedname in
                sh.shared_impl.impl_shared_uploaded <- c.client_pos;
                shared_must_update (as_shared sh.shared_impl) 
              with _ -> () )
          | _ -> () );
          if c.client_pos = c.client_endpos then begin
            if !verbose_upload then lprintf_nl "Finished uploading to (%s)" (clients_username c);
            Unix32.close file_fd;
            set_refill sock (fun _ -> () );
            (match c.client_state with
            | DcUpload (dcsh,_,_,_) ->
                if not (counts_as_minislot dcsh.dc_shared_size) then dc_remove_uploader () (* slots *)
            | _ -> () );
            c.client_state <- DcUploadDoneWaitingForMore; 
            set_client_has_a_slot (as_client c.client_client) NoSlot; (* inform GUI *)
            TcpBufferedSocket.set_lifetime sock (float !!wait_for_next_upload); 
          end else begin
            ready_for_upload (as_client c.client_client);
          end
        end else begin (* HMMM Is it ok to bang this line over and over again ? *)
            ready_for_upload (as_client c.client_client)
        end
        with e ->
          if !verbose_upload then lprintf_nl "Exception (%s) in upload" (Printexc2.to_string e);
          new_client_error c UploadError )
  | _ -> 
      if !verbose_upload then
        lprintf_nl "Socket not connected in uploading to (%s)" (clients_username c);
      c.client_state <- DcIdle;
      set_client_has_a_slot (as_client c.client_client) NoSlot;
      dc_disconnect_client c (Closed_for_error "No socket in upload") )

module P = GuiTypes
          
(* register client operations *)
let _ =
  client_ops.op_client_info <- (fun c ->
    let name = clients_username c in
    let kind,total_downloaded,total_uploaded =
      let ip,port =
        (match c.client_addr with
        | Some (ip,port) -> ip,port
        | None -> Ip.null,0 )
      in
      (match c.client_user with
      | Some user ->
          let kind = 
            if (user_active user) then Known_location (ip,port)
            else Indirect_location (empty_string,Md4.null,ip,port)
          in
          kind,user.user_downloaded,user.user_uploaded
      | _ ->
          let kind = Indirect_location (empty_string,Md4.null,ip,port) in
          kind,Int64.zero,Int64.zero )
    in
    let software, version = 
      match c.client_user with
      | Some u -> u.user_myinfo.client_brand, u.user_myinfo.version
      | None -> empty_string, empty_string
    in
    let filename =
      (match c.client_file with
      | Some file -> file.file_name
      | _ -> "" )
    in
      { (impl_client_info c.client_client) with
        P.client_network = network.network_num;
        P.client_kind = kind;
        P.client_state = client_state (as_client c.client_client);
        P.client_type = client_type c;
        P.client_name = name;
        P.client_num = (client_num (as_client c.client_client));
        P.client_connect_time = c.client_connect_time;
        P.client_software = software;
        P.client_release = version;
        P.client_emulemod = empty_string;
        P.client_session_downloaded = c.client_downloaded;
        P.client_session_uploaded = c.client_uploaded;
        P.client_total_downloaded = total_downloaded;
        P.client_total_uploaded = total_uploaded;
        P.client_upload = Some filename;
        P.client_sui_verified = None; (* new 2.6.5 *)
(*      P.client_sock_addr = ""; *)
      }
  );
  client_ops.op_client_browse <- (fun _ _ -> lprintf_nl "Received (op_client_browse)" );
  client_ops.op_client_can_upload <- (fun c bytes -> dc_upload c bytes );
  client_ops.op_client_enter_upload_queue <- (fun c ->
      if !verbose_msg_clients || !verbose_upload then
        lprintf_nl "Client (%s) started to upload" (clients_username c);
      ready_for_upload (as_client c.client_client)
  )

(*
    mutable op_client_network : CommonTypes.network;
    mutable op_client_connect : 'a -> unit;
    mutable op_client_disconnect : 'a -> unit;
    mutable op_client_say : 'a -> string -> unit;
    mutable op_client_files : 'a -> (string * CommonTypes.result) list;
    mutable op_client_clear_files : 'a -> unit;
    mutable op_client_bprint : 'a -> Buffer.t -> unit;
    mutable op_client_dprint :
    'a -> CommonTypes.ui_conn -> CommonTypes.file -> unit;
    mutable op_client_dprint_html :
    'a -> CommonTypes.ui_conn -> CommonTypes.file -> string -> bool;
    mutable op_client_debug : 'a -> bool -> unit;

*)
