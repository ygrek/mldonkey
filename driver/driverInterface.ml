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
  if List.mem    num gui.gui_search_nums then begin
    let module P = Gui_proto in
      gui_send gui (P.Search_result (num, result_info r))
    end
  
let send_waiting gui num r =
  if List.mem    num gui.gui_search_nums then 
    let module P = Gui_proto in
    gui_send gui (P.Search_waiting (num,r))

let send_file_info gui file =
  let module P = Gui_proto in  
  gui_send gui (P.File_info (CommonFile.file_info file))

let send_full_file_info gui file =
  let module P = Gui_proto in  
  let file_info = file_info file in
  gui_send gui (P.File_info file_info)
  
  
  
  
let send_server_info gui s =
  gui_send gui (P.Server_info (CommonServer.server_info s))

let send_client_info gui c =
  let module P = Gui_proto in  
  gui_send gui (P.Client_info (CommonClient.client_info c))

let gui_reader (gui: gui_record) t sock =

(*
  if Obj.is_int (Obj.repr t) then
    Printf.printf "from gui: int %d" (Obj.magic t)
  else
    Printf.printf "from gui: %d" (Obj.tag (Obj.repr t)); 
print_newline ();
*)  
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
                      match gui.gui_files with
                        file :: files ->
                          gui.gui_files <- files;
                          
                          (try send_file_info gui file with _ -> ())
                      | [] -> 
                          match gui.gui_friends with
                            c :: friends ->
                              gui.gui_friends <- friends;
                              (try 
                                  send_client_info gui c;
                                  
                                  List.iter (fun r ->
                                      gui_send gui
                                      (P.Client_file (client_num c, result_info r))
                                      ) (client_files c)
                                with _ -> ());
                              
                          | [] ->
                              match gui.gui_servers with
                                s :: servers ->
                                  gui.gui_servers <- servers;
                                  (try send_server_info gui s with _ -> ())
                              | [] -> raise Not_found
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
            send_result gui num r 
        ) :: search.op_search_new_result_handlers;
        search.op_search_end_reply_handlers <- 
          (fun _ -> send_waiting gui num search.search_waiting) ::
        search.op_search_end_reply_handlers;
        
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
            Printf.printf "%s:%s" name value; print_newline ();
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
            gui_send gui (P.Client_file (num, result_info r))
        ) (client_files c)
        
    | P.GetClient_info num ->
        gui_send gui (P.Client_info (client_info (client_find num)))
        
    | P.GetUser_info num ->
        gui_send gui (P.User_info (user_info (user_find num)))
        
    | P.GetServer_users num ->
    
        let s = server_find num in
        let users = server_users s in
        List.iter (fun user ->
            gui_send gui (P.Server_user (num, user_num user));
        ) users
        
    | P.GetServer_info num ->
        gui_send gui (P.Server_info (server_info (server_find num)))
        
    | P.GetFile_locations num ->
        let file = file_find num in
        let clients = file_sources file in
        List.iter (fun c ->
            gui_send gui (P.File_source (num, client_num c))
        ) clients
                  
    | P.GetFile_info num ->
        gui_send gui (P.File_info (file_info (file_find num)))
        
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
                        
            gui_files = !!files @ !!CommonComplexOptions.done_files;
            gui_friends = !!friends;
            gui_servers = !!servers;
          } in
        TcpBufferedSocket.set_max_write_buffer sock !!interface_buffer;
        TcpBufferedSocket.set_reader sock (value_handler 
            (gui_reader gui));
        TcpBufferedSocket.set_closer sock (gui_closed gui);
        TcpBufferedSocket.set_handler sock TcpBufferedSocket.BUFFER_OVERFLOW
          (fun _ -> Printf.printf "BUFFER OVERFLOW"; print_newline () );
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

(* We should probably only send "update" to the current state of
the info already sent to *)
let update_gui_info timer =
  reactivate_timer timer;
  
  let map = !files_update_map in
  files_update_map := Intmap.empty;
  Intmap.iter (fun _ file ->
      try
        let file_info = P.File_info (file_info file) in
        List.iter (fun gui -> gui_send gui file_info) !guis;
      with _ -> ()
  ) map;
  
  let map = !servers_update_map in
  servers_update_map := Intmap.empty;
  Intmap.iter (fun _ server ->
      try
        let server_info = P.Server_info (server_info server) in
        List.iter (fun gui -> gui_send gui server_info) !guis;
      with _ -> ()
  ) map;
  
  let map = !clients_update_map in
  clients_update_map := Intmap.empty;
  Intmap.iter (fun _ client ->
      try
        let client_info = P.Client_info (client_info client) in
        List.iter (fun gui -> gui_send gui client_info) !guis;
      with _ -> ()
  ) map;
  
  let map = !users_update_map in
  users_update_map := Intmap.empty;
  Intmap.iter (fun _ user ->
      try
        let user_info = P.User_info (user_info user) in
        List.iter (fun gui -> gui_send gui user_info) !guis;
      with _ -> ()
  ) map;
  
  
  if !guis <> [] then begin
      let map = !rooms_update_map in
      rooms_update_map := Intmap.empty;
      Intmap.iter (fun num room ->
          try
            let room_info = P.Room_info (room_info room) in
            let messages = room_messages room in
            List.iter (fun gui -> 
                gui_send gui room_info;
                List.iter (fun m -> 
                    gui_send gui (P.Room_message (num, m))
                ) messages;
            ) !guis;
          with _ -> ()
      ) map;
    end;
  

  let map = !file_new_sources in
  file_new_sources := [];
  List.iter (fun (file_num, client_num) ->
      try
        let msg = File_source (file_num, client_num) in
        List.iter (fun gui -> gui_send gui msg) !guis;       
      with _ -> ()
  ) map;

  let map = !server_new_users in
  server_new_users := [];
  List.iter (fun (server_num, user) ->
      try
        let msg = Server_user (server_num, user_num user) in
        List.iter (fun gui -> gui_send gui msg) !guis;       
      with _ -> ()
  ) map;

  let map = !room_new_users in
  room_new_users := [];
  List.iter (fun (room_num, user) ->
      try
        let user_msg = User_info (user_info user) in
        let msg = Room_user (room_num, user_num user) in
        List.iter (fun gui -> 
            gui_send gui user_msg;
            gui_send gui msg) !guis;       
      with _ -> ()
  ) map;
  ()
  
  (*
  let rec last = function
      [x] -> x
    | _ :: l -> last l
    | _ -> Int32.zero
  in
  List.iter (fun client ->
      let time = last_time () -. file.file_last_time in
      let last_downloaded = last file.file_last_downloaded in
      let diff = Int32.sub file.file_downloaded last_downloaded in
      file.file_last_time <- last_time ();
      let rate = if time > 0.0 && diff > Int32.zero then begin
            (Int32.to_float diff) /. time;
          end else 0.0
      in
      if rate <> file.file_last_rate || 
        file.file_downloaded <> last_downloaded then begin
          file.file_last_rate <- rate;
          let m = P.File_downloaded (file.file_num, file.file_downloaded,
              rate) in
          List.iter (fun gui ->
              gui_send gui m) !guis;
        end;
      
      begin
        match file.file_changed with
          NoFileChange -> ()
        | FileAvailabilityChange ->
            let m = P.File_availability (file.file_num,
                file.file_all_chunks,
                String2.init file.file_nchunks (fun i ->
                    if file.file_available_chunks.(i) > 1 then '2' else
                    if file.file_available_chunks.(i) > 0 then '1' else
                      '0'))
            in
          List.iter (fun gui ->
              gui_send gui m) !guis;
            
        | FileInfoChange ->
            List.iter (fun gui -> send_file_info gui (as_file file.file_file)) !guis;
      end;
      file.file_changed <- NoFileChange;
      
      if file.file_new_locations then begin
          file.file_new_locations <- false;
          
          let m = file_locations file in
          List.iter (fun gui -> gui_send gui m) !guis;
          
        end
  ) !current_files;
  let msg = P.LocalInfo {
      P.upload_counter = !upload_counter;
      P.shared_files = !nshared_files;
    } in
  List.iter (fun gui -> gui_send gui msg) !guis

    *)

let install_hooks () = 
  List.iter (fun (name,_) ->
      set_option_hook downloads_ini name (fun _ ->
          List.iter (fun gui ->
              gui_send gui (P.Options_info [name, 
                  get_simple_option downloads_ini name])
          ) !guis
      )
  ) (simple_options downloads_ini)
