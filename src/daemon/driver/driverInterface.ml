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

open CommonInteractive
open Printf2
open UdpSocket
open CommonShared
open CommonEvent
open CommonRoom
open CommonTypes
open CommonComplexOptions
open CommonUser
open CommonSearch
open CommonNetwork
open CommonResult
open GuiTypes
open GuiProto
open CommonTypes
open CommonFile
open CommonClient
open CommonServer
open Options
open BasicSocket
open TcpBufferedSocket
open CommonOptions
open CommonGlobals
  
module P = GuiProto
  
let binary_gui_send gui t =
  match gui.gui_sock with
    None -> 
      Fifo.put core_gui_fifo t
  | Some sock ->
      try
        GuiEncoding.gui_send (GuiEncoding.to_gui gui.gui_proto_to_gui_version) 
        sock t
      with UnsupportedGuiMessage -> 
(* the message is probably not supported by this GUI *)

          ()
  
let gift_gui_send gui t =
  match gui.gui_sock with
    None -> 
      Fifo.put core_gui_fifo t
  | Some sock ->
      try
        GiftEncoding.gui_send gui sock t
      with UnsupportedGuiMessage -> 
(* the message is probably not supported by this GUI *)
          ()

let gui_send gui t =
  gui.gui_send gui t

let binary_result_handler gui num r =
  gui_send gui (P.Search_result (num, result_num r, None)) 

let gift_result_handler gui num r =
  gui.gui_id_counter <- gui.gui_id_counter + 1;
  gui_send gui (P.Search_result (num, gui.gui_id_counter, Some (result_info r))) 
          
let gui_can_write gui =
  match gui.gui_sock with
    None -> not !gui_reconnected
  | Some sock -> TcpBufferedSocket.can_write sock
          
let restart_gui_server = ref (fun _ -> ())

let update_events gui update user_num map =
  if not gui.gui_poll then
    let gui_num = gui.gui_num in
    if update = 0 then
      addevent map user_num true
    else
    if update > 0 then
      (if update < gui_num then
          addevent map user_num true)
    else
      (if - update < gui_num then
          addevent map user_num true
        else
          addevent map user_num false)      
  
let update_user_info user =
  let impl = as_user_impl user in
  let update = impl.impl_user_update in
  let user_num = impl.impl_user_num in
  if update < !gui_counter then begin
      with_guis (fun gui -> 
          update_events gui update user_num (gui.gui_events.gui_users)
      );
      impl.impl_user_update <- !gui_counter
    end
  
let update_client_info client =
  let impl = as_client_impl client in
  let update = impl.impl_client_update in
  let client_num = impl.impl_client_num in
  if update < !gui_counter then begin
      with_guis (fun gui -> 
          update_events gui update client_num (gui.gui_events.gui_clients)
      );
      impl.impl_client_update <- !gui_counter
    end
  
let update_server_info server =
  let impl = as_server_impl server in
  let update = impl.impl_server_update in
  let server_num = impl.impl_server_num in
  if update < !gui_counter then begin
      with_guis (fun gui -> 
          update_events gui update server_num ( gui.gui_events.gui_servers)
      );
      impl.impl_server_update <- !gui_counter
    end
  
let update_file_info file =
  let impl = as_file_impl file in
  let update = impl.impl_file_update in
  let file_num = impl.impl_file_num in
  if update < !gui_counter then begin
      with_guis (fun gui -> 
          update_events gui update file_num (gui.gui_events.gui_files)
      );
      impl.impl_file_update <- !gui_counter
    end
  
let update_room_info room =
  let impl = as_room_impl room in
  let update = impl.impl_room_update in
  let room_num = impl.impl_room_num in
  if update < !gui_counter then begin
      with_guis (fun gui -> 
          update_events gui update room_num (gui.gui_events.gui_rooms)
      );
      impl.impl_room_update <- !gui_counter
    end
  
let update_result_info result =
  let impl = as_result_impl result in
  let update = impl.impl_result_update in
  let result_num = impl.impl_result_num in
  if update < !gui_counter then begin
      with_guis (fun gui -> 
          update_events gui update result_num ( gui.gui_events.gui_results)
      );
      impl.impl_result_update <- !gui_counter
    end
  
let update_shared_info shared =
  let impl = as_shared_impl shared in
  let update = impl.impl_shared_update in
  let shared_num = impl.impl_shared_num in
  if update < !gui_counter then begin
      with_guis (fun gui -> 
          update_events gui update shared_num ( gui.gui_events.gui_shared_files)
      );
      impl.impl_shared_update <- !gui_counter
    end

  
let catch m f =
  try f () with e ->
      lprintf "Exception %s for message %s\n" (Printexc2.to_string e) m

let send_event gui ev = 
  match ev with
  | Room_add_user_event (room, user) ->
      gui_send gui (P.Room_add_user (room_num room, user_num user)) 
      
  | Room_remove_user_event (room, user) ->
      gui_send gui (P.Room_remove_user (room_num room, user_num user))
      
  | Room_message_event (_, room, msg) ->            
      gui_send gui (P.Room_message (room_num room, msg))
      
  | Client_new_file_event (c, dirname, r) ->
      gui_send gui (P.Client_file (client_num c, dirname, 
                    result_num r) )

  | File_add_source_event (f,c) ->
      gui_send gui (P.File_add_source (file_num f, client_num c));      

  | File_update_availability (f,c,avail) ->
      gui_send gui (P.File_update_availability (file_num f, client_num c,avail));      
      
  | File_remove_source_event (f,c) ->
      gui_send gui (P.File_remove_source (file_num f, client_num c));      
      
  | Server_new_user_event (s,u) ->
      gui_send gui (P.Server_user (server_num s, user_num u))
      
  | Search_new_result_event (for_gui, events, int, r) ->      
      for_gui int r
(*      gui_send gui (P.Search_result (int, result_num r)) *)
    
  | Console_message_event msg ->
      gui_send gui (P.Console msg)

  | _ ->  lprintf "Event not treated\n"
    
let send_update_file gui file_num update =
  let file = file_find file_num in
  let impl = as_file_impl file in
  let file_info = if update then
      P.File_info (file_info file) 
    else
      P.File_downloaded (impl.impl_file_num,
        impl.impl_file_downloaded,
        file_download_rate impl,
        impl.impl_file_last_seen)
  in
  gui_send gui file_info

let send_update_user gui user_num update =
  let user = user_find user_num in
  let user_info = P.User_info (user_info user) in
  gui_send gui user_info
  
let send_update_client gui client_num update =
  let client = client_find client_num in
  let impl = as_client_impl client in
  let client_info = if update then
      P.Client_info (client_info client) 
    else
      P.Client_state (
        impl.impl_client_num, 
        impl.impl_client_state) 
  in
  gui_send gui client_info
  
let send_update_server gui server_num update =
  let server = server_find server_num in
  let impl = as_server_impl server in
  let server_info = if update then
      P.Server_info (server_info server) 
    else
      P.Server_state (impl.impl_server_num, impl.impl_server_state)
  in
  gui_send gui server_info

let send_update_room gui room_num update =
  let room = room_find room_num in
  let room_info = P.Room_info (room_info room) in
  gui_send gui room_info

let send_update_result gui result_num update =
  let result = result_find result_num in
  let impl = as_result_impl result in
  let result_info = P.Result_info (result_info result) in
  gui_send gui result_info  
  
  let send_update_shared gui shfile_num update =
    let shared = CommonShared.shared_find shfile_num in
    let impl = as_shared_impl shared in
    let msg = if update then
        P.Shared_file_info (shared_info shared)
      else
        P.Shared_file_upload (
          impl.impl_shared_num,
          impl.impl_shared_uploaded, 
          impl.impl_shared_requests)
    in
    gui_send gui msg

let send_update_old gui =
  match gui.gui_events.gui_old_events with
  | ev :: tail ->
      gui.gui_events.gui_old_events <- tail;
      send_event gui ev;
      true
  | [] ->
      match gui.gui_events.gui_new_events with
        [] -> false
      | list ->
          gui.gui_events.gui_old_events <- List.rev list;
          gui.gui_events.gui_new_events <- [];
          true
          
let connecting_writer gui _ =
  try
    let rec iter list =
      if gui_can_write gui then
        match list with
          [] -> if send_update_old gui then iter []
        |  (events, f) :: tail ->
            match events.num_list with
              [] -> iter tail
            | num :: tail ->
                events.num_list <- tail;
                let update = Intmap.find num events.num_map in
                events.num_map <- Intmap.remove num events.num_map ;
                (try f gui num update with _ -> ());
                iter list
    in
    iter [
        (gui.gui_events.gui_files, send_update_file);
        (gui.gui_events.gui_users, send_update_user);
        (gui.gui_events.gui_clients, send_update_client);
        (gui.gui_events.gui_servers, send_update_server);
        (gui.gui_events.gui_rooms, send_update_room);
        (gui.gui_events.gui_results, send_update_result);
        (gui.gui_events.gui_shared_files, send_update_shared);      
      ]

                    
          
          (*
      getevents  gui
        [
        
        (gui.gui_files, send_update_file);
        
        (gui.gui_users, send_update_user);
        
        (gui.gui_clients, send_update_client);
        
        (gui.gui_servers, send_update_server);
        
        (gui.gui_rooms, send_update_room);
        
        (gui.gui_results, send_update_result);
        
        (gui.gui_shared_files, send_update_shared);
      
      ]
send_update_old        
  *)
  with _ -> ()

let console_messages = Fifo.create ()
        
let gui_closed gui sock  msg =
(*  lprintf "DISCONNECTED FROM GUI %s\n" msg; *)
  guis := List2.removeq gui !guis



let gui_initialize gui = 

  gui.gui_initialized <- true;
  let connecting = ref true in
  networks_iter_all (fun n ->
      gui_send gui (Network_info (network_info n)));
  gui_send gui (Console (DriverControlers.text_of_html !!motd_html));
  
  networks_iter_all (fun n ->
      gui_send gui (Network_info (network_info n)));
  gui_send gui (Console (DriverControlers.text_of_html !!motd_html));
  
  
  if gui.gui_poll then begin
      
      let gui = gui.gui_events in
      gui.gui_new_events <- [];
      gui.gui_old_events <- [];
      
      gui.gui_files <- create_events ();            
      gui.gui_clients <- create_events ();
      gui.gui_servers <- create_events ();
      gui.gui_rooms <- create_events ();
      gui.gui_users <- create_events ();
      gui.gui_results <- create_events ();
      gui.gui_shared_files <- create_events ();
    
    
    end else begin
      
      List.iter (fun c ->
          addevent gui.gui_events.gui_clients (client_num c) true
      ) !!friends;
      
      List.iter (fun file ->
          addevent gui.gui_events.gui_files (file_num file) true;
          let sources = file_active_sources file in
          List.iter (fun c ->
              addevent gui.gui_events.gui_clients (client_num c) true;
              gui.gui_events.gui_new_events <-
                (File_add_source_event (file,c))
              :: gui.gui_events.gui_new_events
          ) (file_active_sources file)
      ) !!files;
      
      List.iter (fun file ->
          addevent gui.gui_events.gui_files (file_num file) true;
      ) !!done_files;
      
      networks_iter_all (fun n ->
          List.iter (fun s ->
              addevent gui.gui_events.gui_servers (server_num s) true
          ) (n.op_network_connected_servers ())
      );
      
      server_iter (fun s ->
          addevent gui.gui_events.gui_servers (server_num s) true
      );
      
      rooms_iter (fun room ->
          if room_state room <> RoomClosed then begin
              addevent gui.gui_events.gui_rooms (room_num room) true;
              List.iter (fun user ->
                  lprintf "room add user\n"; 
                  addevent gui.gui_events.gui_users (user_num user) true;
                  gui.gui_events.gui_new_events <-
                    (Room_add_user_event (room,user))
                  :: gui.gui_events.gui_new_events
              ) (room_users room)
            
            end
      );
      
      shared_iter (fun s ->
          addevent gui.gui_events.gui_shared_files (shared_num s) true
      );
      
      Fifo.iter (fun ev ->
          gui.gui_events.gui_new_events <- ev :: gui.gui_events.gui_new_events
      ) console_messages;                
      
      gui_send gui (
        P.Options_info (simple_options "" downloads_ini));
      networks_iter_all (fun r ->
          List.iter (fun opfile ->
(*
lprintf "options for net %s\n" r.network_name; 
*)
              let prefix = r.network_prefix () in
              let args = simple_options prefix opfile in
(*
lprintf "Sending for %s\n" prefix; 
 *)
              gui_send gui (P.Options_info args)
(*                            lprintf "sent for %s\n" prefix; *)
          )
          r.network_config_file 
      );

(* Options panels defined in downloads.ini *)
      List.iter (fun s ->
          let section = section_name s in
          List.iter (fun o ->
              gui_send gui (
                P.Add_section_option (section, o))
          ) (strings_of_section_options "" s)
      ) (sections downloads_ini);

(* Options panels defined in each plugin *)
      networks_iter_all (fun r ->
          let prefix = r.network_prefix () in
          List.iter (fun file ->
              
              List.iter (fun s ->
                  let section = section_name s in
                  List.iter (fun o ->
                      gui_send gui (
                        P.Add_plugin_option (r.network_name, o)
                      )
                  ) (strings_of_section_options prefix s)
              ) (sections file)
          ) r.network_config_file
      );
      
      gui_send gui (P.DefineSearches (CommonComplexOptions.customized_queries()));
      match gui.gui_sock with
        None -> ()
      | Some sock ->
          TcpBufferedSocket.must_write sock true;
          set_handler sock WRITE_DONE (connecting_writer gui);
    end
  
let gui_reader (gui: gui_record) t _ =
  
  let module P = GuiProto in  
  try
    match t with    
    
    | GuiProtocol version ->
        let version = min GuiProto.best_gui_version version in
        gui.gui_proto_to_gui_version <- Array.create 
          (to_gui_last_opcode + 1) version;
        gui.gui_proto_from_gui_version <- Array.create 
          (from_gui_last_opcode + 1) version;
        lprintf "Using protocol %d for communications with the GUI\n" 
          version    
    
    | P.GuiExtensions list ->
        List.iter (fun (ext, bool) ->
            if ext = P.gui_extension_poll then
              (            
                lprintf "Extension POLL %s\n" (string_of_bool bool); 
                gui.gui_poll <- bool
              )
        ) list
    
    | P.Password (user, pass) ->
        begin
          match gui.gui_sock with
            Some sock when not (valid_password user pass) ->
              gui_send gui BadPassword;                  
              set_lifetime sock 5.;
              lprintf "BAD PASSWORD\n"; 
              TcpBufferedSocket.close sock (Closed_for_error "Bad Password")
          
          | _ ->
              guis := gui :: !guis;
              gui.gui_auth <- true;
              
              if not gui.gui_initialized then 
                gui_initialize gui;
        
        end
    | _ ->
        if gui.gui_auth then
          let _ =
            if not gui.gui_initialized then 
              gui_initialize gui;
          in
          begin
            match gui.gui_sock with
              None -> ()
            | Some sock ->
                TcpBufferedSocket.set_lifetime sock 3600.;
          end;
          match t with
          | P.Command cmd ->
              let o = gui.gui_conn in
              let buf = o.conn_buf in
              Buffer.clear buf;
              Buffer.add_string buf "\n----------------------------------\n";
              Printf.bprintf buf "Eval command: %s\n\n" cmd;
              DriverControlers.eval (ref true) cmd o;
              Buffer.add_string buf "\n\n";
              gui_send gui (P.Console (
                  DriverControlers.dollar_escape o false
                    (Buffer.contents buf)))
          
          | P.MessageVersions list ->
              List.iter (fun (opcode, from_guip, proto) ->
                  if from_guip then
                    (if opcode <= from_gui_last_opcode then
                        gui.gui_proto_from_gui_version.(opcode) <- proto)
                  else
                    (if opcode <= to_gui_last_opcode then
                        gui.gui_proto_to_gui_version.(opcode) <- proto)
              ) list
          
          | P.SetOption (name, value) ->
              CommonInteractive.set_fully_qualified_options name value
          
          | P.CloseSearch (num, forget) ->
              let s = List.assoc num gui.gui_searches in
              if forget then search_forget gui.gui_conn.conn_user s
              else search_close s
          
          | P.SendMessage (num, msg) ->
              begin
                try
                  let room = room_find num in
                  room_send_message room msg
                with _ ->
                    match msg with (* no room ... maybe a private message *)
                      PrivateMessage (num, s) -> 
                        client_say (client_find num) s
                    | _ -> assert false
              end
          
          | P.EnableNetwork (num, bool) ->
              let n = network_find_by_num num in
              if n.op_network_is_enabled () <> bool then
                (try
                    if bool then network_enable n else network_disable n;
                  with e ->
                      lprintf "Exception %s in network enable/disable\n" 
                        (Printexc2.to_string e);
                );
              gui_send gui (P.Network_info (network_info n))
          
          | P.ExtendedSearch (num, e) ->
              let user = gui.gui_conn.conn_user in
              let s = 
                if num = -1 then
                  match user.ui_user_searches with
                    [] -> raise Not_found
                  | s :: _ -> s
                else
                  List.assoc num gui.gui_searches in
              networks_iter (fun r -> 
                  if s.search_network = 0 ||
                    r.network_num = s.search_network
                  then
                    network_extend_search r s e)
          
          | P.KillServer -> 
              exit_properly 0
          
          | P.Search_query s ->
              
              let search_num = s.GuiTypes.search_num in
              add_timer 60. (fun _ ->
                  gui_send gui (Search_waiting (search_num, 0))
              );
              gui.gui_id_counter <- maxi gui.gui_id_counter search_num;
              
              let user = gui.gui_conn.conn_user in
              let query = 
                try CommonGlobals.simplify_query
                    (CommonSearch.mftp_query_of_query_entry 
                      s.GuiTypes.search_query)
                with Not_found ->
                    lprintf "Not_found in mftp_query_of_query_entry\n";
                    raise Not_found
              in
              let buf = Buffer.create 100 in
              let num = s.GuiTypes.search_num in
              let search = CommonSearch.new_search user
                  { s with GuiTypes.search_query = query} in
              gui.gui_search_nums <- num ::  gui.gui_search_nums;
              gui.gui_searches <- (num, search) :: gui.gui_searches;
              search.op_search_new_result_handlers <- (fun r ->
                  CommonEvent.add_event
                    (Search_new_result_event (
                      gui.gui_result_handler, gui.gui_events, num, r))
              ) :: search.op_search_new_result_handlers;
              networks_iter (fun r -> 
                  if search.search_network = 0 ||
                    r.network_num = search.search_network
                  then
                    
                    r.op_network_search search buf);
(*
        search.op_search_end_reply_handlers <- 
          (fun _ -> send_waiting gui num search.search_waiting) ::
search.op_search_end_reply_handlers;
  *)
          
          | P.Download_query (filenames, num, force) ->
              begin
                let r = result_find num in
                let file = result_download r filenames force in
                CommonInteractive.start_download file
              end
          
          | P.ConnectMore_query ->
              networks_iter network_connect_servers
          
          | P.Url url ->
              if not (networks_iter_until_true (fun n -> network_parse_url n url)) then
                begin
                  let file = File.to_string url in
                  let lines = String2.split_simplify file '\n' in
                  List.iter (fun line ->
                      ignore (networks_iter_until_true (fun n -> network_parse_url n line))
                  ) lines
                end
          | P.GetUploaders ->
              gui_send gui (P.Uploaders
                  (List2.tail_map (fun c -> client_num c) 
                  (Intmap.to_list !uploaders)))
          
          | P.GetPending ->
              gui_send gui (P.Pending (
                  List2.tail_map (fun c -> client_num c)
                  (Intmap.to_list !CommonUploads.pending_slots_map)))
          
          | P.RemoveServer_query num ->
              server_remove (server_find num)
          
          | P.SaveOptions_query list ->
              
              List.iter (fun (name, value) ->
                  CommonInteractive.set_fully_qualified_options name value) list;
              DriverInteractive.save_config ()
          
          | P.RemoveDownload_query num ->
              file_cancel (file_find num)
          
          | P.ViewUsers num -> 
              let s = server_find num in
              server_query_users s
          
          | P.ServerUsers_query num -> 
              let s = server_find num in
              server_query_users s
          
          | P.SetFilePriority (num, prio) ->
              let file = file_find num in
              set_file_priority file prio;
              CommonInteractive.force_download_quotas ()
          
          | P.SaveFile (num, name) ->
              let file = file_find num in
              set_file_best_name file name;
              (match file_state file with
                  FileDownloaded ->
                    file_save_as file name;
                    file_commit file
                | _ -> ())
          
          | P.Preview num ->
              begin
                let file = file_find num in
                file_preview file
              end
          
          | P.AddClientFriend num ->
              let c = client_find num in
              friend_add c
          
          | P.BrowseUser num ->
              let user = user_find num in
              user_browse_files user
          
          | P.GetClient_files num ->        
              let c = client_find num in
(*        lprintf "GetClient_files %d\n" num;  *)
              List.iter (fun (dirname, r) ->
(* delay sending to update *)
                  client_new_file c dirname r
              ) (client_files c)
          
          | P.GetClient_info num ->
              addevent gui.gui_events.gui_clients num true
          
          | P.GetUser_info num ->
              addevent gui.gui_events.gui_users num true
          
          | P.GetServer_users num ->    
              let s = server_find num in
              let users = server_users s in
              List.iter (fun user ->
                  server_new_user s user
              ) users
          
          | P.GetServer_info num ->
              addevent gui.gui_events.gui_servers num true
          
          | P.GetFile_locations num ->
              let file = file_find num in
              let clients = file_active_sources file in
              List.iter (fun c ->
                  file_add_source file c
              ) clients
          
          | P.GetFile_info num ->
              addevent gui.gui_events.gui_files num true
          
          | P.RenameFile (num, new_name) ->
              let file = file_find num in
              set_file_best_name file new_name;
              addevent gui.gui_events.gui_files num true
          
          | P.ConnectFriend num 
          | P.ConnectClient num
            ->
              let c = client_find num in
              client_connect c
          
          | P.DisconnectClient num
            ->
              let c = client_find num in
              client_disconnect c
          
          | P.ConnectServer num -> 
              server_connect (server_find num)
          
          | P.DisconnectServer num -> 
              server_disconnect (server_find num)
          
          | P.ConnectAll num -> 
              let file = file_find num in
              file_recover file        
          
          | P.QueryFormat num ->
              begin
                try
                  let file = file_find num in
                  let filename = file_disk_name file in
                  let format = CommonMultimedia.get_info filename in
                  set_file_format file format;
                  file_must_update file;
                with _ -> ()
              end
          
          | P.ModifyMp3Tags (num, tag) ->
              begin
                try 
                  let file = file_find num in
                  let filename = file_disk_name file in
                  Mp3tag.Id3v1.write tag filename;
                with 
                  _ -> ()
              end
          
          | P.SwitchDownload (num, resume) ->
              let file = file_find num in
              if resume then
                file_resume file          
              else
                file_pause file
          
          | P.FindFriend user -> 
              networks_iter (fun n ->
                  List.iter (fun s -> server_find_user s user) 
                  (network_connected_servers n))
          
          | P.RemoveFriend num -> 
              let c = client_find num in
              friend_remove c 
          
          | P.RemoveAllFriends ->
              List.iter (fun c ->
                  friend_remove c
              ) !!friends
          
          | P.CleanOldServers -> 
              networks_iter network_clean_servers
          
          | P.AddUserFriend num ->
              let user = user_find num in
              user_set_friend user
          
          | P.VerifyAllChunks num ->
              let file = file_find num in
              file_check file 
          
          | P.Password _ | P.GuiProtocol _ | P.GuiExtensions _ -> 
(* These messages are handled before, since they can be received without
  authentication *)
              assert false

(* version 3 *)
          
          | P.MessageToClient (num, mes) ->
              lprintf "MessageToClient(%d,%s)\n" num mes;
              let c = client_find num in
              client_say c mes
          
          | P.GetConnectedServers ->
              let list = ref [] in
              networks_iter (fun n ->
                  list := network_connected_servers n @ !list);
              gui_send gui (P.ConnectedServers (List2.tail_map
                    server_info !list))
          
          | P.GetDownloadedFiles ->
              gui_send gui (P.DownloadedFiles
                  (List2.tail_map file_info !!done_files))
          
          | P.GetDownloadFiles -> 
              gui_send gui (P.DownloadFiles
                  (List2.tail_map file_info !!files))              
          
          | GetSearches ->
              let user = gui.gui_conn.conn_user in
              let searches = user.ui_user_searches in
              List.iter (fun s ->
                  let module P = GuiTypes in
                  gui_send gui (Search {
                      P.search_num = s.search_num;
                      P.search_type = s.search_type;
                      P.search_query = s.search_string;
                      P.search_max_hits = s.search_max_hits;
                      P.search_network = s.search_network;
                    });                  
              ) searches
          
          | GetSearch num ->
              let user = gui.gui_conn.conn_user in
              let searches = user.ui_user_searches in
              (try
                  List.iter (fun s ->
                      if s.search_num = num then begin
(* Send the search description *)
                          let module P = GuiTypes in
                          gui_send gui (Search {
                              P.search_num = num;
                              P.search_type = s.search_type;
                              P.search_query = s.search_string;
                              P.search_max_hits = s.search_max_hits;
                              P.search_network = s.search_network;
                            });
                          
(* Send the results corresponding to this search *)
                          gui.gui_search_nums <- num ::  gui.gui_search_nums;
                          gui.gui_searches <- (num, s) :: gui.gui_searches;
                          s.op_search_new_result_handlers <- (fun r ->
                              CommonEvent.add_event
                                (Search_new_result_event (
                                  gui.gui_result_handler, gui.gui_events, num, r))
                          ) :: s.op_search_new_result_handlers;
                          
                          Intmap.iter (fun _ (_, r) ->
                              CommonEvent.add_event
                                (Search_new_result_event (
                                  gui.gui_result_handler, gui.gui_events, 
                                  num, r))                              
                          ) s.search_results
                          
                        end
                  ) searches                
                with
                | Exit -> ()
                | Not_found ->
                    failwith (Printf.sprintf "No such search %d" num)
              )
          | SetRoomState (num, state) ->
              if num > 0 then begin
                  let room = room_find num in
                  match state with
                    RoomOpened -> room_resume room
                  | RoomClosed -> room_close room
                  | RoomPaused -> room_pause room
                end
  
          | NetworkMessage (num, s) ->
              let n = network_find_by_num num in
              n.op_network_gui_message s
                
          | AddServer_query (num, ip, port) ->
              let n = network_find_by_num num in
(* [CONTRIBUTE:] change the GUI protocol to transfer an address (ip/name)
  instead of just an ip. *)
              
              let s = n.op_network_add_server (Ip.addr_of_ip ip) port in
              server_connect s
          | RefreshUploadStats ->
              
              shared_iter (fun s ->
                  update_shared_info s;
              ) 

          | P.GiftAttach _ ->
              gui_send gui (P.GiftServerAttach ("mldonkey", "1.1"))
          | P.GiftStats ->
              let list = ref [] in
              networks_iter (fun n ->
                  list := (
                    n.network_name, "0", "0", "0"
                  ) :: !list
              );
              gui_send gui (P.GiftServerStats !list)
  with 
    Failure s ->
      gui_send gui (Console (Printf.sprintf "Failure: %s\n" s))
  | e ->
      gui_send gui (Console (    
          Printf.sprintf "from_gui: exception %s for message %s\n" (
            Printexc2.to_string e) (GuiProto.from_gui_to_string t)))

      
let gui_events () = 
  {
      
      gui_new_events = [];
      gui_old_events = [];
      
      gui_files = create_events ();            
      gui_clients = create_events ();
      gui_servers = create_events ();
      gui_rooms = create_events ();
      gui_users = create_events ();
      gui_results = create_events ();
      gui_shared_files = create_events ();
    
  }
  
let new_gui gui_send gui_auth sock  =
  incr gui_counter;
  let gui = {
      gui_searches = [];
      gui_sock = sock;
      gui_search_nums = [];
      gui_events = gui_events ();
      gui_proto_to_gui_version = Array.create (to_gui_last_opcode+1) 0;
      gui_proto_from_gui_version = Array.create (from_gui_last_opcode+1) 0;
      gui_num = !gui_counter;
      gui_auth = gui_auth;
      gui_poll = false;
      gui_result_handler = (fun _ _ -> ());
      gui_id_counter = 33000;
      gui_identifiers = Hashtbl.create 1023;
      gui_identifiers_rev = Hashtbl.create 1023;
      gui_initialized = false;
      gui_conn = { 
        conn_output = TEXT; 
        conn_sortvd = BySize;
        conn_filter = (fun _ -> ()); 
        conn_buf = Buffer.create 100;
        conn_user = default_user;
        conn_width = 80; conn_height = 25;
      };
      gui_send = gui_send;
    } in
  gui_send gui (
    P.CoreProtocol 
      (
      GuiProto.best_gui_version,
      GuiProto.to_gui_last_opcode,
      GuiProto.from_gui_last_opcode));
  gui
  
let gui_handler t event = 
  match event with
    TcpServerSocket.CONNECTION (s, Unix.ADDR_INET (from_ip, from_port)) ->
      let from_ip = Ip.of_inet_addr from_ip in
      lprintf "CONNECTION FROM GUI\n"; 
      if Ip.matches from_ip !!allowed_ips then 
        
        let module P = GuiProto in
        let token = create_token unlimited_connection_manager in
        let sock = TcpBufferedSocket.create_simple token
            "gui connection"
            s in
        
        let gui = new_gui binary_gui_send 
            false (Some sock) in
        gui.gui_result_handler <- binary_result_handler gui;
        TcpBufferedSocket.set_max_output_buffer sock !!interface_buffer;
        TcpBufferedSocket.set_lifetime sock 30.;
        TcpBufferedSocket.set_reader sock (GuiDecoding.gui_cut_messages
            (fun opcode s ->
              try
                let m = GuiDecoding.from_gui gui.gui_proto_from_gui_version 
                  opcode s in
                gui_reader gui m sock;
              with GuiDecoding.FromGuiMessageNotImplemented -> ()
          ));
        TcpBufferedSocket.set_closer sock (gui_closed gui);
        TcpBufferedSocket.set_handler sock TcpBufferedSocket.BUFFER_OVERFLOW
          (fun _ -> 
            lprintf "BUFFER OVERFLOW\n"; 
            close sock Closed_for_overflow);
        (* sort GUIs in increasing order of their num *)
        
      else begin
          lprintf "Connection from that IP %s not allowed\n"
            (Ip.to_string from_ip);
          Unix.close s
        end
  | _ -> ()
        
let gift_handler t event = 
  match event with
    TcpServerSocket.CONNECTION (s, Unix.ADDR_INET (from_ip, from_port)) ->
      let from_ip = Ip.of_inet_addr from_ip in
      lprintf "CONNECTION FROM GUI\n"; 
      if Ip.matches from_ip !!allowed_ips then 
        
        let module P = GuiProto in
        let token = create_token unlimited_connection_manager in
        let sock = TcpBufferedSocket.create_simple token
            "gui connection"
            s in
        
        let gui = new_gui gift_gui_send true (Some sock) in
        gui.gui_result_handler <- gift_result_handler gui;
        guis := gui :: !guis;
        
        TcpBufferedSocket.prevent_close sock;
        TcpBufferedSocket.set_max_output_buffer sock !!interface_buffer;
        TcpBufferedSocket.set_reader sock (GiftDecoding.gui_cut_messages
            (fun s ->
              let m = GiftDecoding.from_gui gui s in
              gui_reader gui m sock;
              ));
        TcpBufferedSocket.set_closer sock (gui_closed gui);
        TcpBufferedSocket.set_handler sock TcpBufferedSocket.BUFFER_OVERFLOW
          (fun _ -> 
            lprintf "BUFFER OVERFLOW\n"; 
            close sock Closed_for_overflow);
        (* sort GUIs in increasing order of their num *)
        
      else begin
          lprintf "Connection from that IP %s not allowed\n"
            (Ip.to_string from_ip);
          Unix.close s
        end
  | _ -> ()

      
let add_gui_event ev =
  with_guis (fun gui ->
      gui.gui_events.gui_new_events <- ev :: gui.gui_events.gui_new_events
  )

let rec update_events list = 
  match list with
    [] -> ()
  | event :: tail ->
      (try
          match event with
            Room_info_event room -> 
              update_room_info room
              
          | Room_add_user_event (room, user) ->
              update_room_info room;
              update_user_info user;
              add_gui_event event
          
          | Room_remove_user_event (room, user) ->
              update_room_info room;
              update_user_info user;
              add_gui_event event

          | Room_message_event (_, room, msg) ->            
              update_room_info room;
              begin
                match msg with
                  ServerMessage _ -> ()
                | PrivateMessage (user_num, _) 
                | PublicMessage (user_num, _) ->
                    try
                      let user = user_find user_num  in
                      update_user_info user
                    with _ ->
                        lprintf "USER NOT FOUND FOR MESSAGE\n"
              end;
              add_gui_event event
              
          | Shared_info_event sh ->
              update_shared_info sh
              
          | Result_info_event r ->
              update_result_info r
              
          | Client_info_event c ->
              update_client_info c
              
          | Server_info_event s ->
              update_server_info s
              
          | File_info_event f ->
              update_file_info f
              
          | User_info_event u ->
              update_user_info u

          | Client_new_file_event (c,_,r) ->
              update_client_info c;
              update_result_info r;
              add_gui_event event
              
          | File_add_source_event (f,c)
          | File_update_availability (f,c,_) ->
              update_file_info f;
              update_client_info c;
              add_gui_event event
              
          | File_remove_source_event (f,c) ->
              update_file_info f;
              add_gui_event event
              
          | Server_new_user_event (s,u) ->
              update_server_info s;
              update_user_info u;
              add_gui_event event

          | Search_new_result_event (events, gui, int, r) ->
              update_result_info r;
              gui.gui_new_events <- event :: gui.gui_new_events
              
          | Console_message_event msg ->  
              Fifo.put console_messages event;
              if Fifo.length console_messages > 30 then begin
                  ignore (Fifo.take console_messages)
                end;
              add_gui_event event
              
        with _ -> ());
      update_events tail
      

(* We should probably only send "update" to the current state of
the info already sent to *)
let next_clean_table = ref (last_time () + 1800)
  
let update_gui_info () =
  begin
    if !next_clean_table < last_time () then begin
        next_clean_table := last_time () + 1800;
        let clients = CommonClient.clients_get_all () in
        let servers = CommonServer.servers_get_all () in
        with_guis (fun gui ->
            gui_send gui (CleanTables (clients, servers))  
        );
      end;
  end;
  
  
  
  let nets = ref [] in
  networks_iter_all (fun n -> 
      nets := 
        (n.network_num, List.length (network_connected_servers n)) :: !nets);
     
  let msg = (Client_stats {
        upload_counter = !upload_counter;
        download_counter = !download_counter;
        shared_counter = !shared_counter;
        nshared_files = !nshared_files;
        tcp_upload_rate = !control_upload_rate;
        tcp_download_rate = !control_download_rate;
        udp_upload_rate = !udp_upload_rate;
        udp_download_rate = !udp_download_rate;
        connected_networks = !nets;
        ndownloaded_files = List.length !!done_files;
        ndownloading_files = List.length !!files;
      }) in
  with_guis(fun gui -> 
      gui_send gui msg);       
  let events = List.rev !events_list in
  events_list := [];
  update_events events;
  with_guis (fun gui ->
      connecting_writer gui gui.gui_sock
  )
  
let install_hooks () = 
  iter_file (fun o ->
      option_hook o (fun _ ->
          with_guis (fun gui ->
              try
                let oo = strings_of_option o in
                gui_send gui (P.Options_info [oo])
              with _ -> ())
      ) 
  ) downloads_ini;
  networks_iter_all (fun r ->
      let prefix = r.network_prefix()  in
      List.iter (fun opfile ->
          iter_file (fun o ->
              try
                let oo = strings_of_option o in
                let oo = { oo with
                    option_name = Printf.sprintf "%s%s" prefix oo.option_name
                  } in
                option_hook o (fun _ ->
                    with_guis (fun gui ->
                        gui_send gui (P.Options_info [oo])))
              with _ -> ()
          ) opfile)
      r.network_config_file 
      )      

  ;
  
  private_room_ops.op_room_send_message <- (fun s msg ->
      match msg with
        PrivateMessage (c, s) ->
            let ci = client_find c in
            update_client_info ci;  (* send client info before message? *)
            with_guis (fun gui -> gui_send gui (P.MessageFromClient (c, s)))
      | _ -> assert false
  )

let local_gui = ref None
  
let _ =
  add_init_hook (fun _ ->
      if !gui_included then
        add_infinite_timer 0.1 (fun _ ->
            begin
              match !local_gui with
                None -> ()
              | Some gui ->
                  if !gui_reconnected then begin
                      lprintf "close gui ...\n"; 
                      local_gui := None;
                      gui_closed gui () "local"
                    end else
                    (try
                        while true do
                          let m = Fifo.take gui_core_fifo in
                          gui_reader gui m ()
                        done
                      with Fifo.Empty -> ()
                      | e ->
                          lprintf "Exception %s in handle gui message\n"
                            (Printexc2.to_string e); 
                          )
            end;
            if !gui_reconnected then begin
                lprintf "gui_reconnected !\n"; 
                gui_reconnected := false;
                let gui = new_gui binary_gui_send false None in
                gui.gui_result_handler <- binary_result_handler gui;
                local_gui := Some gui;
                ()
              end
        )
  )
  
