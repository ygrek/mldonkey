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

open CommonChatRoom
open CommonTypes
open CommonComplexOptions
open CommonUser
open CommonSearch
open CommonNetwork
open CommonResult
open Gui_proto
open CommonTypes
open CommonFile
open CommonClient
open CommonServer
open Options
open BasicSocket
open TcpBufferedSocket
open CommonOptions
open CommonGlobals
  
module P = Gui_proto
  
  
let gui_send gui t = value_send gui.gui_sock (t : Gui_proto.to_gui)

let restart_gui_server = ref (fun _ -> ())
  
let send_result gui num r =
  Printf.printf "GUI RESULT FOR SEARCH %d" num; print_newline ();
  if List.mem num gui.gui_search_nums then begin
    let module P = Gui_proto in
      gui_send gui (P.Result_info (result_info r));
      gui_send gui (P.Search_result (num, result_num r))
    end
  
let send_waiting gui num r =
  if List.mem    num gui.gui_search_nums then 
    let module P = Gui_proto in
    gui_send gui (P.Search_waiting (num,r))


let send_file_info gui file =
  let module P = Gui_proto in  
  gui_send gui (P.File_info (CommonFile.file_info file))

let search_results_list = ref []  
  
let send_server_info gui s =
  gui_send gui (P.Server_info (CommonServer.server_info s))

let send_client_info gui c =
  let module P = Gui_proto in  
  gui_send gui (P.Client_info (CommonClient.client_info c))

let gui_reader (gui: gui_record) t sock =
  
  try
    let module P = Gui_proto in
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
        Options.set_simple_option downloads_ini name value
    
    | P.ForgetSearch num ->
        let s = search_find num in
        search_forget s
    
    | P.SendMessage (num, msg) ->
        let room = room_find num in
        room_send_message room msg
    
    | P.ExtendedSearch ->
        networks_iter network_extend_search
    
    | P.Password (v,s) ->
        if v <> CommonTypes.version then begin
            Printf.printf "Bad GUI version"; print_newline ();
            TcpBufferedSocket.close sock "bad version";
          end;
        if s = !!password then begin
            BasicSocket.must_write (TcpBufferedSocket.sock sock) true;
            let connecting = ref true in
            
            gui_send gui (
              P.Options_info (simple_options downloads_ini));
            gui_send gui (P.DefineSearches !!CommonComplexOptions.customized_queries);
            
            set_handler sock WRITE_DONE (fun _ ->
                if !connecting then
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
                                  Printf.printf "send file info"; print_newline ();
                                  (try send_file_info gui file with _ -> ());
                                  gui.gui_sources <- Some (file_sources file, file)
                                with _ -> ())
                          | _ -> 
                              gui.gui_files <- [];
                              match gui.gui_friends with
                                c :: friends ->
                                  gui.gui_friends <- friends;
                                  (try 
                                      send_client_info gui c;
                                      
                                      List.iter (fun r ->
                                          gui_send gui
                                            (P.Result_info (result_info r));
                                          gui_send gui
                                            (P.Client_file (client_num c, 
                                              result_num r))
                                      ) (client_files c)
                                    with _ -> ());
                              
                              | [] ->
                                  match gui.gui_servers with
                                    s :: servers ->
                                      gui.gui_servers <- servers;
                                      (try send_server_info gui s with _ -> ())
                                  | [] -> 
                                      (*
                                      match gui.gui_rooms with
                                        r :: tail ->
                                          gui.gui_rooms <- tail;
                                          (try
                                              match room_state r with
                                                RoomClosed -> ()
                                              | _ -> 
                                                  gui_send gui
                                                    (P.Room_info (room_info r))
                                            with _ -> ())
                                      | [] ->  *) raise Not_found
                    done;
                  with _ -> 
                      gui_send gui (P.GuiConnected);
                      connecting := false
            );
          
          end else
          TcpBufferedSocket.close gui.gui_sock "bad password"
    
    | P.KillServer -> 
        exit_properly ()
    
    | P.Search_query (local,s) ->
        Printf.printf "GUI SEARCH QUERY %d" s.P.search_num; print_newline ();
        let query = 
          try CommonGlobals.simplify_query
            (CommonSearch.mftp_query_of_query_entry 
                s.P.search_query)
          with Not_found ->
              prerr_endline "Not_found in mftp_query_of_query_entry";
              raise Not_found
        in
        let buf = Buffer.create 100 in
        let search = CommonInteractive.start_search query buf in
        let num = s.P.search_num in
        gui.gui_search_nums <- num ::  gui.gui_search_nums;
        gui.gui_searches <- (num, search) :: gui.gui_searches;
        search.op_search_new_result_handlers <- (fun r ->
            search_results_list := (gui, num, r) :: !search_results_list;
        ) :: search.op_search_new_result_handlers;
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

    | P.AddServer_query (network, ip, port) ->
        let n = network_find_by_name network in
        network_add_server_id n ip port
        
    | P.RemoveServer_query num ->
        server_remove (server_find num)
       
    | P.SaveOptions_query list ->
        
        List.iter (fun (name, value) ->
            set_simple_option downloads_ini name value) list;
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
        
    | P.AddFriend num ->
        let c = client_find num in
        friend_add c
        
    | P.GetClient_files num ->        
        let c = client_find num in
        List.iter (fun r ->
(* delay sending to update *)
            client_new_file c r
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
        server_must_update (server_find num)
        
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
        
    | P.AddNewFriend (network, ip, port) ->
        let n = network_find_by_name network in
        network_add_friend_id n ip port
        
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
            List.iter (fun gui -> send_file_info gui file)
            !guis;
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
        
    | P.SwitchDownload num ->
        let file = file_find num in
        begin
          match file_state file with
            FilePaused ->
              file_resume file          
          | _ -> 
              file_pause file
        end
        
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
    | P.SayFriends (s, friend_list) ->
        List.iter (fun num ->
            let c = client_find num in
            client_say c s              
        ) friend_list
        
    | P.CleanOldServers -> 
        networks_iter network_clean_servers
        
    | P.AddUserFriend num ->
        let user = user_find num in
        user_set_friend user
        
    | P.VerifyAllChunks num ->
        let file = file_find num in
        file_check file 
        
    | P.SendMoreInfo (md4_list, num_list) ->
        (* NOT IMPLEMENTED
        List.iter (fun md4 ->
            try 
              let file = find_file md4 in
              if file.file_known_locations != Intmap.empty ||
                file.file_indirect_locations != Intmap.empty then
                send_full_file_info gui file with _ -> ()) md4_list;
        (* NOT IMPLEMENTED 
        List.iter (fun num ->
            let c = find_client num in
            if c.client_all_files != None then
              send_client_info gui c) num_list;        *)
*)
        ()
  with e ->
      Printf.printf "from_gui: exception %s" (Printexc.to_string e);
      print_newline ()
  
let gui_closed gui sock  msg =
  Printf.printf "DISCONNECTED FROM GUI"; print_newline ();
  guis := List2.removeq gui !guis;
  ()
  
let gui_handler t event = 
  match event with
    TcpServerSocket.CONNECTION (s, Unix.ADDR_INET (from_ip, from_port)) ->
      let from_ip = Ip.of_inet_addr from_ip in
      if Ip.matches from_ip !!allowed_ips then 
        
        let module P = Gui_proto in
        let sock = TcpBufferedSocket.create_simple 
            "gui connection"
          s in
        let gui = {
            gui_searches = [];
            gui_sock = sock;
            gui_search_nums = [];
            
            gui_sources = None;
            gui_files = !!files @ !!CommonComplexOptions.done_files;
            gui_friends = !!friends;
            gui_servers = !!servers;
            gui_rooms = [];
          } in
        Hashtbl.iter (fun _ room ->
            if room_state room <> RoomClosed then
              gui.gui_rooms <- room :: gui.gui_rooms;
        ) com_rooms_by_num;
        TcpBufferedSocket.set_max_write_buffer sock !!interface_buffer;
        TcpBufferedSocket.set_reader sock (value_handler 
            (gui_reader gui));
        TcpBufferedSocket.set_closer sock (gui_closed gui);
        TcpBufferedSocket.set_handler sock TcpBufferedSocket.BUFFER_OVERFLOW
          (fun _ -> 
            Printf.printf "BUFFER OVERFLOW"; print_newline ();
            close sock "overflow");
        guis := gui :: !guis;
        gui_send gui (P.Connected CommonTypes.version);
        networks_iter_all (fun n ->
            gui_send gui (P.Network_info {
                P.network_num = n.network_num;
                P.network_name = n.network_name;
                P.network_enabled = network_is_enabled n;
              })
        );
        Hashtbl.iter (fun _ room ->
            try
              if room_state room = RoomOpened then
                gui_send gui (P.Room_info (room_info room))
            with _ -> ()
        ) com_rooms_by_num;
        
      else 
        Unix.close s
  | _ -> ()
  

let must_wait () =
  List.exists (fun gui -> not (can_fill gui.gui_sock)) !guis

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
          let file_info = P.File_info (file_info file) in
          List.iter (fun gui -> gui_send gui file_info) !guis;
          (as_file_impl file).impl_file_update <- false;
        with _ -> ()
  );
  files_update_list := []
          
let users_old_list = ref []
let update_users () =
  users_old_list := update !users_old_list !users_update_list 
      (fun user ->
        try
          let user_info = P.User_info (user_info user) in
          List.iter (fun gui -> gui_send gui user_info) !guis;
          (as_user_impl user).impl_user_update <- false;
        with _ -> ()
  );
  users_update_list := []
          
let servers_old_list = ref []
let update_servers () =
  servers_old_list := update !servers_old_list !servers_update_list 
      (fun server ->
        try
          let server_info = P.Server_info (server_info server) in
          List.iter (fun gui -> gui_send gui server_info) !guis;
          (as_server_impl server).impl_server_update <- false;
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
          let client_info = P.Client_info (client_info client) in
          List.iter (fun gui -> gui_send gui client_info) !guis;
          (as_client_impl client).impl_client_update <- false;
        with _ -> ()
  );
  clients_update_list := []

let rooms_old_list = ref []
let update_rooms () =
  if !guis <> [] then 
    rooms_old_list := update !rooms_old_list !rooms_update_list 
      (fun room ->
        try
          match room_state room with
            RoomOpened ->
              let num = room_num room in
              let room_info = P.Room_info (room_info room) in
              let messages = room_messages room in
              List.iter (fun gui -> 
                  gui_send gui room_info;
                  List.iter (fun m -> 
                      gui_send gui (P.Room_message (num, m))
                  ) messages;
              ) !guis;
          | _ -> 
              ()
        with _ -> ()
    );
  rooms_update_list := []

let update_room_users () =
  let rec iter list =
    if must_wait () then list else
    match list with
      [] -> []
    | (room_num, user) :: tail ->
        (try
            let room = room_find room_num in
            match room_state room with
              RoomOpened ->
                let room_msg = Room_info (room_info room) in
                let user_msg = User_info (user_info user) in
                let msg = Room_user (room_num, user_num user) in
                List.iter (fun gui -> 
                    gui_send gui room_msg;
                    gui_send gui user_msg;
                    gui_send gui msg) !guis;       
            | _ -> ()
          with _ -> ());
        iter tail
  in
  room_new_users := iter !room_new_users
    
  
let update_file_sources () =
  let map = !file_new_sources in
  file_new_sources := [];
  List.iter (fun (file_num, client_num) ->
      try
        let client = client_find client_num in
        let client_msg = Client_info (client_info client) in
        let msg = File_source (file_num, client_num) in
        List.iter (fun gui -> 
            gui_send gui client_msg;
            gui_send gui msg) !guis;       
      with _ -> ()
  ) map
  
let update_server_users () =
  let map = !server_new_users in
  server_new_users := [];
  List.iter (fun (server_num, user) ->
      try
        let user_msg = User_info (user_info user) in
        let msg = Server_user (server_num, user_num user) in
        List.iter (fun gui -> 
            gui_send gui user_msg;
            gui_send gui msg) !guis;       
      with _ -> ()
  ) map
  
let update_client_files () =
  let map = !client_new_files in
  client_new_files := [];
  List.iter (fun (client_num, r) ->
      try
        let result_msg = Result_info (result_info r) in
        let msg = Client_file (client_num, result_num r) in
        List.iter (
          fun gui -> 
            gui_send gui result_msg;
            gui_send gui msg
        ) 
        !guis;       
      with _ -> ()
  ) map

  
let update_functions = [
    "update_files", update_files;
    "update_servers",update_servers;
    "update_clients",update_clients;
    "update_users",update_users;
    "update_rooms",update_rooms;
    "update_file_sources",update_file_sources;
    "update_searches",update_searches;
    "update_server_users",update_server_users;
    "update_client_files",update_client_files;
    "update_room_users",update_room_users;
  ]
  
(* We should probably only send "update" to the current state of
the info already sent to *)
let update_gui_info timer =
  reactivate_timer timer;
  let rec iter fs =
    match fs with 
      [] -> ()
    | (name, f) :: fs ->
        if  must_wait () then begin
            Printf.printf "Wait for some update functions"; print_newline ();
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
          List.iter (fun gui ->
              gui_send gui (P.Options_info [name, 
                  get_simple_option downloads_ini name])
          ) !guis
      )
  ) (simple_options downloads_ini)
