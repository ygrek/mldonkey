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

let gui_send gui t =
  try
    GuiEncoding.gui_send GuiEncoding.to_gui.(gui.gui_version) gui.gui_sock t    
  with UnsupportedGuiMessage -> 
(* the message is probably not supported by this GUI *)
      ()
  
let restart_gui_server = ref (fun _ -> ())

  
let send_server_info gui s =
  gui_send gui (P.Server_info (CommonServer.server_info s))

let send_client_info gui c =
  let module P = GuiProto in  
  gui_send gui (P.Client_info (CommonClient.client_info c))

let send_file_info gui file =
  let module P = GuiProto in  
  gui_send gui (P.File_info (CommonFile.file_info file))
  
let send_result gui num r =
  if List.mem num gui.gui_search_nums then begin
    let module P = GuiProto in
      gui_send gui (P.Result_info (result_info r));
      gui_send gui (P.Search_result (num, result_num r))
    end
  
let send_waiting gui num r =
  if List.mem    num gui.gui_search_nums then 
    let module P = GuiProto in
    gui_send gui (P.Search_waiting (num,r))

            
let update_user_info user =
  let impl = as_user_impl user in
  if impl.impl_user_update < !gui_counter then
    let user_info = P.User_info (user_info user) in
    with_gui (fun gui -> 
        if impl.impl_user_update < gui.gui_num then
          begin
            gui_send gui user_info;
            impl.impl_user_update <- gui.gui_num;                  
          end);
    impl.impl_user_update <- !gui_counter
  
let update_client_info client =
  let impl = as_client_impl client in
  if impl.impl_client_update < !gui_counter then
    let client_info = 
      match impl.impl_client_update with
      | -1 -> P.Client_state (impl.impl_client_num, impl.impl_client_state) 
      | _ ->  P.Client_info (client_info client) 
    in
    with_gui (fun gui -> 
        if impl.impl_client_update < gui.gui_num then
          begin
            gui_send gui client_info;
            impl.impl_client_update <- gui.gui_num;                  
          end);
    impl.impl_client_update <- !gui_counter

let update_server_info server =
  let impl = as_server_impl server in
  if impl.impl_server_update < !gui_counter then
    let server_info = 
      match impl.impl_server_update with
      | -1 ->  P.Server_state (impl.impl_server_num, impl.impl_server_state)
      | _ ->  P.Server_info (server_info server) 
    in
    with_gui (fun gui -> 
        if impl.impl_server_update < gui.gui_num then
          begin
            gui_send gui server_info;
            impl.impl_server_update <- gui.gui_num;                  
            end);
      impl.impl_server_update <- !gui_counter

let update_file_info file =
  let impl = as_file_impl file in
  if impl.impl_file_update < !gui_counter then
    let file_info = 
      match impl.impl_file_update with
        -1 -> P.File_downloaded (impl.impl_file_num,
            impl.impl_file_downloaded,
            file_download_rate impl)
      | _ -> P.File_info (file_info file) in
    with_gui (fun gui -> 
        if impl.impl_file_update < gui.gui_num then
          begin
            impl.impl_file_update <- gui.gui_num;                  
            gui_send gui file_info
          end);
    impl.impl_file_update <- !gui_counter


let update_room_info room =
  let impl = as_room_impl room in
  if impl.impl_room_update < !gui_counter then
    let room_info = P.Room_info (room_info room) in
    with_gui (fun gui -> 
        if impl.impl_room_update < gui.gui_num then
          begin
            gui_send gui room_info;
            impl.impl_room_update <- gui.gui_num;                  
          end);
    impl.impl_room_update <- !gui_counter

let update_shared_info shared =
  let impl = as_shared_impl shared in
  if impl.impl_shared_update < !gui_counter then
    let msg = match impl.impl_shared_update with
        -1 -> P.Shared_file_upload (impl.impl_shared_num,
            impl.impl_shared_uploaded, impl.impl_shared_requests)
      | _ -> P.Shared_file_info (shared_info shared) in
    with_gui (fun gui -> 
        if impl.impl_shared_update < gui.gui_num then
          begin
            gui_send gui msg;
            impl.impl_shared_update <- gui.gui_num;                  
          end);
    impl.impl_shared_update <- !gui_counter

let update_result_info result =
  let impl = as_result_impl result in
  if impl.impl_result_update < !gui_counter then
    let result_info = P.Result_info (result_info result) in
    with_gui (fun gui -> 
        if impl.impl_result_update < gui.gui_num then
          begin
            gui_send gui result_info;
            impl.impl_result_update <- gui.gui_num;                  
          end);
    impl.impl_result_update <- !gui_counter



let search_results_list = ref []  

  
let catch m f =
  try f () with e ->
      Printf.printf "Exception %s for message %s" (Printexc.to_string e) m;
      print_newline () 

let connecting_writer gui sock =
  if gui.gui_connecting then
    try
      while TcpBufferedSocket.can_write sock do
        match gui.gui_sources with
          Some (c :: clients, file) ->
            gui.gui_sources <- Some(clients, file);
            (try 
                send_client_info gui c;
                gui_send gui
                  (P.File_source (file_num file, client_num c))
              with _ -> ());
        
        
        | _ -> 
            gui.gui_sources <- None;
            match gui.gui_files with
              file :: files ->
                gui.gui_files <- files;
                
                (try
(*                    Printf.printf "send file info"; print_newline (); *)
                    update_file_info file;
                    gui.gui_sources <- Some (file_sources file, file)
                  with _ -> ())
            | _ -> 
                gui.gui_files <- [];
                match gui.gui_friends with
                  c :: friends ->
                    gui.gui_friends <- friends;
                    (try 
                        send_client_info gui c;
                        
                        List.iter (fun (dirname, r) ->
                            gui_send gui
                              (P.Result_info (result_info r));
                            gui_send gui
                              (P.Client_file (client_num c, 
                                dirname,
                                result_num r))
                        ) (client_files c)
                      with _ -> ());
                
                | [] ->
                    match gui.gui_servers with
                      s :: servers ->
                        gui.gui_servers <- servers;
                        (try send_server_info gui s with _ -> ())
                    | [] -> 
                        match gui.gui_rooms with
                          r :: tail ->
                            gui.gui_rooms <- tail;
                            (try
                                match room_state r with
                                  RoomClosed -> ()
                                | _ -> 
                                    gui_send gui
                                      (P.Room_info (room_info r));
                                    List.iter (fun user ->
                                        update_user_info user;
                                        gui_send gui (P.Room_add_user (room_num r, user_num user))
                                    ) (room_users r)
                              with _ -> ())
                        | [] ->  raise Not_found
      done;
    with _ -> 
        gui.gui_connecting <- false
        
let gui_reader (gui: gui_record) t sock =
  let module P = GuiProto in  
  try
    match t with
    
    
    | GuiProtocol version ->
        gui.gui_version <- min GuiEncoding.best_gui_version version;
        Printf.printf "Using protocol %d for communications with the GUI" 
          gui.gui_version;
        print_newline ();
    
    | P.GuiExtensions list ->
        List.iter (fun (ext, bool) ->
            if ext = P.gui_extension_poll then
              gui.gui_poll <- bool
        ) list
        
    | P.Password s ->
        if s = !!password then begin
            BasicSocket.must_write (TcpBufferedSocket.sock sock) true;
            let connecting = ref true in
            
            begin
              match !gui_option with
                None -> ()
              | Some gui -> close gui.gui_sock "new gui"
            end;
            gui_option := Some gui;
            
            
            gui.gui_auth <- true;
            gui_send gui (
              P.Options_info (simple_options downloads_ini));
            
            List.iter (fun (section, message, option, optype) ->
                gui_send gui (
                  P.Add_section_option (section, message, option,
                    match optype with
                    | "B" -> BoolEntry 
                    | "F" -> FileEntry 
                    | _ -> StringEntry
                  )))
            !! gui_options_panel;
            
            gui_send gui (P.DefineSearches !!CommonComplexOptions.customized_queries);
            if not gui.gui_poll then
              set_handler sock WRITE_DONE (connecting_writer gui);
          
          end else begin
            Printf.printf "BAD PASSWORD"; print_newline ();
            TcpBufferedSocket.close gui.gui_sock "bad password"
          end
    
    | _ ->
        if gui.gui_auth then
          match t with
          | P.Command cmd ->
              let buf = Buffer.create 1000 in
              Buffer.add_string buf "\n----------------------------------\n";
              Printf.bprintf buf "Eval command: %s\n\n" cmd;
              let options = { conn_output = TEXT; conn_sortvd = BySize;
                  conn_filter = (fun _ -> ()); conn_buf = Buffer.create 1000;
                } in
              let buf = options.conn_buf in
              DriverControlers.eval (ref true) cmd 
                options;
              Buffer.add_string buf "\n\n";
              gui_send gui (P.Console (Buffer.contents buf))
          
          | P.SetOption (name, value) ->
              CommonInteractive.set_fully_qualified_options name value
          
          | P.ForgetSearch num ->
              let s = search_find num in
              search_forget s
          
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
                      Printf.printf "Exception %s in network enable/disable" 
                        (Printexc.to_string e);
                      print_newline ());
              gui_send gui (P.Network_info (network_info n))
          
          | P.ExtendedSearch ->
              networks_iter network_extend_search
          
          | P.KillServer -> 
              exit_properly ()
          
          | P.Search_query s ->
              let query = 
                try CommonGlobals.simplify_query
                    (CommonSearch.mftp_query_of_query_entry 
                      s.GuiTypes.search_query)
                with Not_found ->
                    prerr_endline "Not_found in mftp_query_of_query_entry";
                    raise Not_found
              in
              let buf = Buffer.create 100 in
              let num = s.GuiTypes.search_num in
              let search = CommonSearch.new_search 
                  { s with GuiTypes.search_query = query} in
              gui.gui_search_nums <- num ::  gui.gui_search_nums;
              gui.gui_searches <- (num, search) :: gui.gui_searches;
              search.op_search_new_result_handlers <- (fun r ->
                  search_results_list := (gui, num, r) :: !search_results_list;
              ) :: search.op_search_new_result_handlers;
              networks_iter (fun r -> r.op_network_search search buf);

(*
        search.op_search_end_reply_handlers <- 
          (fun _ -> send_waiting gui num search.search_waiting) ::
search.op_search_end_reply_handlers;
  *)
          
          | P.Download_query (filenames, num) ->
              begin
                let r = result_find num in
                result_download r filenames
              end
          
          | P.ConnectMore_query ->
              networks_iter network_connect_servers
          
          | P.Url url ->
              ignore (networks_iter_until_true (fun n -> network_parse_url n url))
          
          | P.RemoveServer_query num ->
              server_remove (server_find num)
          
          | P.SaveOptions_query list ->
              
              List.iter (fun (name, value) ->
                  CommonInteractive.set_fully_qualified_options name value) list;
              DriverInteractive.save_config ()
          
          | P.RemoveDownload_query num ->
              file_cancel (file_find num)
          
          | P.ServerUsers_query num -> 
              let s = server_find num in
              server_query_users s
          
          | P.SaveFile (num, name) ->
              let file = file_find num in
              file_save_as file name;
              file_commit file
          
          | P.Preview num ->
              begin
                let file = file_find num in
                let cmd = Printf.sprintf "%s \"%s\" \"%s\"" !!previewer
                    (file_disk_name file) (file_best_name file) in
                ignore (Sys.command cmd)
              end
          
          | P.AddClientFriend num ->
              let c = client_find num in
              friend_add c
          
          | P.BrowseUser num ->
              let user = user_find num in
              user_browse_files user
          
          | P.GetClient_files num ->        
              let c = client_find num in
(*        Printf.printf "GetClient_files %d" num; print_newline (); *)
              List.iter (fun (dirname, r) ->
(* delay sending to update *)
                  client_new_file c dirname r
              ) (client_files c)
          
          | P.GetClient_info num ->
              client_must_update (client_find num)
          
          
          | P.GetUser_info num ->
              user_must_update (user_find num)
          
          | P.GetServer_users num ->    
              let s = server_find num in
              let users = server_users s in
              List.iter (fun user ->
                  server_new_user s user
              ) users
          
          | P.GetServer_info num ->
              server_must_update (server_find num);
          
          | P.GetFile_locations num ->
              let file = file_find num in
              let clients = file_sources file in
              List.iter (fun c ->
                  file_new_source file c
              ) clients
          
          | P.GetFile_info num ->
              file_must_update  (file_find num)
          
          | P.ConnectFriend num ->
              let c = client_find num in
              client_connect c
          
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
                  file_set_format file format;
                  with_gui (fun gui -> send_file_info gui file)
                with _ -> ()
              end
          
          | P.ModifyMp3Tags (num, tag) ->
              begin
                try 
                  let file = file_find num in
                  let filename = file_disk_name file in
                  Mp3tag.write tag filename;
                with 
                  _ -> ()
              end
          
          | P.SwitchDownload (num, resume) ->
              let file = file_find num in
              if resume then
                file_resume file          
              else
                file_pause file
          
          | P.ViewUsers num -> 
              let s = server_find num in
              server_query_users s
          
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
              Printf.printf "MessageToClient(%d,%s)" num mes;
              print_newline ();
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

          | SetRoomState (num, state) ->
              if num > 0 then begin
                  let room = room_find num in
                  match state with
                    RoomOpened -> room_resume room
                  | RoomClosed -> room_close room
                  | RoomPaused -> room_pause room
                end

          | RefreshUploadStats ->
              
              shared_iter (fun s ->
                  update_shared_info s;
              ) 
                
  with e ->
      Printf.printf "from_gui: exception %s for message %s" (
        Printexc.to_string e) (GuiProto.from_gui_to_string t);
      print_newline ()
      
let gui_closed gui sock  msg =
(*  Printf.printf "DISCONNECTED FROM GUI"; print_newline (); *)
  match !gui_option with
    Some g when g == gui -> gui_option := None
  | _ -> ()
  
let gui_handler t event = 
  match event with
    TcpServerSocket.CONNECTION (s, Unix.ADDR_INET (from_ip, from_port)) ->
      let from_ip = Ip.of_inet_addr from_ip in
      if Ip.matches from_ip !!allowed_ips then 
        
        let module P = GuiProto in
        let sock = TcpBufferedSocket.create_simple 
            "gui connection"
            s in
        incr gui_counter;
        let gui = {
            gui_searches = [];
            gui_sock = sock;
            gui_search_nums = [];
            
            gui_sources = None;
            gui_files = !!files @ !!CommonComplexOptions.done_files;
            gui_friends = !!friends;
            gui_servers = !!servers;
            gui_rooms = [];
            gui_version = 0;
            gui_num = !gui_counter;
            gui_auth = false;
            gui_poll = false;
            gui_connecting = true;
          } in
        rooms_iter (fun room ->
            if room_state room <> RoomClosed then
              gui.gui_rooms <- room :: gui.gui_rooms;
        ) ;
        shared_iter (fun s ->
            let impl = as_shared_impl s in
            shared_must_update s);
        TcpBufferedSocket.set_max_write_buffer sock !!interface_buffer;
        TcpBufferedSocket.set_reader sock (GuiDecoding.gui_cut_messages
            (fun opcode s ->
              let m = GuiDecoding.from_gui.(gui.gui_version) opcode s in
              gui_reader gui m sock));
        TcpBufferedSocket.set_closer sock (gui_closed gui);
        TcpBufferedSocket.set_handler sock TcpBufferedSocket.BUFFER_OVERFLOW
          (fun _ -> 
            Printf.printf "BUFFER OVERFLOW"; print_newline ();
            close sock "overflow");
        (* sort GUIs in increasing order of their num *)
        gui_send gui (P.CoreProtocol GuiEncoding.best_gui_version);
        networks_iter_all (fun n ->
            gui_send gui (Network_info (network_info n)));
        
      else 
        Unix.close s
  | _ -> ()
  
(*******
These functions are used to send new informations to the GUI:
- The functions are only applied if there is still some space to write on the 
    GUI buffer (to avoid buffer overfull)
- The messages are only sent if the update timestamp of the structure is smaller
    than the one of the GUI

********)
      
      
let must_wait () =
  match !gui_option with
    None -> false
  | Some gui -> not (can_fill gui.gui_sock)

let rec update old_list new_list sender =
  if must_wait () then
    old_list @ new_list
  else
  match old_list with
    v :: tail ->
      sender v;
      update tail new_list sender
  | [] ->
      match new_list with
        [] -> []
      | v :: tail ->
          sender v;
          update [] tail sender

          
let files_old_list = ref []
let update_files () =
  files_old_list := update !files_old_list !files_update_list 
    (fun file ->
      try
        update_file_info file
      with _ -> ()
  );
  files_update_list := []
      
let users_old_list = ref []
let update_users () =
  users_old_list := update !users_old_list !users_update_list 
      (fun user ->
      try
        update_user_info user
        with _ -> ()
  );
  users_update_list := []
          
let servers_old_list = ref []
let update_servers () =
(*  Printf.printf "update_servers"; print_newline (); *)
  servers_old_list := update !servers_old_list !servers_update_list 
      (fun server ->
      try
        update_server_info server
        with _ -> ()
  );
  servers_update_list := []

let update_searches () =
  let rec iter list = 
    if must_wait () then list else
    match list with
      [] -> []
    | (gui, num, r) :: tail ->
        send_result gui num r; 
        iter tail
  in
  search_results_list := iter !search_results_list

let clients_old_list = ref []
let update_clients () =
  clients_old_list := update !clients_old_list !clients_update_list 
      (fun client ->
      try
        update_client_info client
      with _ -> ()
  );
  clients_update_list := []

(*
Three update cases:
1) Room state is changed: room_update is < gui_num and room is in 
     room_update_list
2) Room users are added or removed
3) Room messages are received
*)

let handle_event gui event =
  try
    match event with
      Room_info_event room -> 
        Printf.printf "room info"; print_newline ();
        update_room_info room
    | Room_add_user_event (room, user) ->
        update_room_info room;
        update_user_info user;
        gui_send gui (P.Room_add_user (room_num room, user_num user))
    
    | Room_remove_user_event (room, user) ->
        update_room_info room;
        update_user_info user;
        gui_send gui (P.Room_remove_user (room_num room, user_num user))
    
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
                  Printf.printf "USER NOT FOUND FOR MESSAGE"; print_newline ();
        end;
        gui_send gui (P.Room_message (room_num room, msg))
  with _ -> ()

let discard_event event = 
  match event with
    Room_info_event room -> room_updated room
  | Room_remove_user_event _
  | Room_message_event _
  | Room_add_user_event _ -> ()
      
let discard_events list =
  List.iter discard_event list
      
let old_events = ref []
let update_events () =
  match !gui_option with
    None -> 
      if !old_events <> [] then 
        begin discard_events !old_events; old_events := []; end;
      if !events_list <> [] then
        begin  discard_events !events_list; events_list := [] end;
  | Some gui ->
      let update_old_events () =
        let rec iter events =
          match events with
            [] -> old_events := []
          | event :: tail ->
              if not (can_fill gui.gui_sock) then begin
                  old_events := events;
                  raise Exit;
                end;
              handle_event gui event;
              iter tail            
        in
        iter !old_events
      in
      
      try
        update_old_events ();
        old_events := List.rev !events_list;
        events_list := [];
        update_old_events ()
      with Exit -> ()
  
  (*
  rooms_update_list := [];
  room_old_messages := update !room_old_messages 
    (List.rev !room_new_messages)
  (fun  );
  room_new_messages := []
  
let update_room_users () =
  let rec iter list =
    if must_wait () then list else
    match list with
      [] -> []
    | (room, user) :: tail ->
        (try
            match room_state room with
              RoomOpened ->
                send_user_info user;
                send_room_info room;
                let msg = Room_user (room_num room, user_num user) in
                List.iter (fun gui -> gui_send gui msg) !guis;       
            | _ -> ()
          with _ -> ());
        iter tail
  in
  room_new_users := iter !room_new_users
*)
    
  
let update_file_sources () =
  let rec iter list =
    match list with
      [] -> []
    | (file, client) :: tail ->
        if must_wait () then list else begin
            begin
              try
                update_client_info client;
                update_file_info file;
                let msg = File_source (file_num file, client_num client) in
                with_gui (fun gui -> 
                    gui_send gui msg) ;       
              with _ -> ()
            end;
            iter tail
          end
  in
  file_new_sources := iter !file_new_sources
  
let update_server_users () =
  let rec iter list =
    match list with
      [] -> []
    | (server, user) :: tail ->
        if must_wait () then list else begin
            begin
              try
                update_server_info server;
                update_user_info user;
                let msg = Server_user (server_num server, user_num user) in
                with_gui (fun gui -> gui_send gui msg);       
              with _ -> ()
            end;
            iter tail
          end
  in
  server_new_users := iter !server_new_users
  
let update_client_files () =
(*  Printf.printf "update_client_files"; print_newline (); *)
  let rec iter list =
    match list with
      [] -> []
    | (client, dirname, r) :: tail ->
        if must_wait () then list else begin
            begin
              try
(*                Printf.printf "send new client file"; print_newline (); *)
                update_result_info r;
                let msg = Client_file (client_num client, dirname, 
                    result_num r) 
                in
                with_gui (
                  fun gui -> gui_send gui msg
                ) 
                ;       
              with _ -> ()
            end;
            iter tail
          end
  in
  client_new_files := iter !client_new_files
  
let update_functions = [
    "update_files", update_files;
    "update_servers",update_servers;
    "update_clients",update_clients;
    "update_users",update_users;
    "update_file_sources",update_file_sources;
    "update_searches",update_searches;
    "update_server_users",update_server_users;
    "update_client_files",update_client_files;
  ]
  
let rec last = function
    [x] -> x
  | _ :: l -> last l
  | _ -> (last_time (), 0.0, 0.0)
    
let trimto list =
  let (list, _) = List2.cut 20 list in
  list 
  
let bandwidth_samples = ref []
  
(* We should probably only send "update" to the current state of
the info already sent to *)
let update_gui_info () =

  let time = last_time () in
  let last_count_time, last_uploaded_bytes, last_downloaded_bytes = 
    last !bandwidth_samples in
  
  let delay = time -. last_count_time in

  let uploaded_bytes = Int64.to_float !uploaded_bytes in
  let downloaded_bytes = Int64.to_float !downloaded_bytes in
  
  let upload_rate = if delay > 0. then
      int_of_float ( (uploaded_bytes -. last_uploaded_bytes) /. delay )
    else 0 in
  
  let download_rate = if delay > 0. then
      int_of_float ( (downloaded_bytes -. last_downloaded_bytes) /. delay )
    else 0 in

  bandwidth_samples := trimto (
    (time, uploaded_bytes, downloaded_bytes) :: !bandwidth_samples);
  
  let msg = (Client_stats {
        upload_counter = !upload_counter;
        download_counter = !download_counter;
        shared_counter = !shared_counter;
        nshared_files = !nshared_files;
        upload_rate = upload_rate;
        download_rate = download_rate;
      }) in
  update_events ();
  with_gui(fun gui -> 
      gui_send gui msg);       
  let rec iter fs =
    match fs with 
      [] -> ()
    | (name, f) :: fs ->
        if  must_wait () then begin
          end else begin
(*            Printf.printf "APPLY %s" name; print_newline (); *)
            (try f () with _ -> ());
(*            Printf.printf "DONE"; print_newline ();  *)
            iter fs
          end
  in
  iter update_functions
  
  
let install_hooks () = 
  List.iter (fun (name,_) ->
      set_option_hook downloads_ini name (fun _ ->
          with_gui (fun gui ->
              gui_send gui (P.Options_info [name, 
                  get_simple_option downloads_ini name])
          )
      )
  ) (simple_options downloads_ini)
  ;
  private_room_ops.op_room_send_message <- (fun s msg ->
      match msg with
        PrivateMessage (c, s) ->
          with_gui (fun gui -> gui_send gui 
                (P.MessageFromClient (c, s)))
          
      | _ -> assert false
    )
      

  
