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

open CommonGlobals
open CommonTypes
open Options
open BasicSocket
open TcpBufferedSocket
open Unix
open GuiTypes
open GuiProto
open Gui2_options
module O = Gui2_options
module M = Gui2_messages
open MyCList
  
open Gui2

let results_by_num = Hashtbl.create 13
let find_result num = Hashtbl.find results_by_num num
let add_result r = if not (Hashtbl.mem results_by_num r.result_num) then
    Hashtbl.add results_by_num r.result_num r

let clients_by_num = Hashtbl.create 13
let find_client num = Hashtbl.find clients_by_num num
let add_client r = if not (Hashtbl.mem clients_by_num r.client_num) then
    Hashtbl.add clients_by_num r.client_num r

let files_by_num = Hashtbl.create 13
let find_file num = Hashtbl.find files_by_num num
let add_file r = if not (Hashtbl.mem files_by_num r.file_num) then
    Hashtbl.add files_by_num r.file_num r

let users_by_num = Hashtbl.create 13
let find_user num = Hashtbl.find users_by_num num
let add_user r = if not (Hashtbl.mem users_by_num r.user_num) then
    Hashtbl.add users_by_num r.user_num r
  
  
  
module Mes = M
  
let locations = Hashtbl.create 103
  
(* Check bindings *)
let _ = 
  if !!O.keymap_global = [] then
    (
     let a = O.add_binding O.keymap_global in
     a "A-s" M.a_page_servers;
     a "A-d" M.a_page_downloads;
     a "A-f" M.a_page_friends;
     a "A-q" M.a_page_queries;
     a "A-o" M.a_page_options;
     a "A-c" M.a_page_console;
     a "A-h" M.a_page_help;
     a "A-Left" M.a_previous_page;
     a "A-Right" M.a_next_page;
     a "C-r" M.a_reconnect;
     a "C-e" M.a_exit ;
    );
  if !!O.keymap_servers = [] then
    (
     let a = O.add_binding O.keymap_servers in
     a "C-c" M.a_connect;
     a "C-m" M.a_connect_more;
     a "C-a" M.a_select_all;
    );
  if !!O.keymap_downloads = [] then
    (
     let a = O.add_binding O.keymap_downloads in
     a "C-c" M.a_cancel_download;
     a "CS-s" M.a_save_all_files;
     a "C-s" M.a_menu_save_file;
     a "C-a" M.a_select_all;
    );
  if !!O.keymap_friends = [] then
    (
     let a = O.add_binding O.keymap_friends in
     a "C-d" M.a_download_selection;
     a "C-x" M.a_remove_friend;
     a "C-a" M.a_select_all;
    );
  if !!O.keymap_queries = [] then
    (
     let a = O.add_binding O.keymap_queries in
     ()
    );
  if !!O.keymap_console = [] then
    (
     let a = O.add_binding O.keymap_console in
     ()
    )

let current_page = ref 0

let short_name n =
  let len = String.length n in
  if len > 35 then
    Printf.sprintf "%s...%s" (String.sub n 0 27) (String.sub n (len-5) 5)
  else n

let server_key s = s.server_num

let first_name r = match r.result_names with
    [] -> assert false
  | name :: _ -> name
  
let gui =
  ignore (GMain.Main.init ()) ;
  new gui () 
      
let connection_sock = ref None
let search_counter = ref 0
  
exception NotConnectedToClient    
  
  let gui_protocol_used = ref 0

  
let gui_send t = 
  match !connection_sock with
    None -> 
      Printf.printf "Message not sent since not connected";
      print_newline ();
  | Some sock ->
      GuiEncoding.gui_send GuiEncoding.from_gui.(!gui_protocol_used) sock t
      
let _ = 
  (try Options.load mldonkey_gui_ini with
      e ->
        Printf.printf "Exception %s in load options" (Printexc.to_string e);
        print_newline ();
  );
  let args = Options.simple_args mldonkey_gui_ini in
  Arg.parse args (Arg.usage args)  "mldonkey_gui: the GUI to use with mldonkey"
      
let window = GWindow.window 
    ~title: "MLdonkey"
    ~width: !!gui_width ~height: !!gui_height
    ~allow_shrink: true
    () 

let tab_searches = gui#tab_searches
let tab_servers = gui#tab_servers
let tab_downloads = gui#tab_downloads
let tab_friends = gui#tab_friends
let tab_help = gui#tab_help
  

let is_connected state =
  match state with
  | Connected_initiating
  | Connected_busy
  | Connected_idle
  | Connected_queued -> true
  | NotConnected
  | Connecting
  | NewHost
  | RemovedHost -> false
  
  
let for_selection list f () =
  List.iter (fun file ->
      try f file with _ -> ()) (MyCList.selection list);
  MyCList.unselect_all list

let for_selection_keep_selected list f () =
  List.iter (fun file ->
      try f file with _ -> ()) (MyCList.selection list)

  
let string_of_tags tags =
  let buf = Buffer.create 100 in
      List.iter (fun t ->
          Buffer.add_string buf (Printf.sprintf "%-3s "
              (match t.tag_value with
                String s -> s
              | Uint32 i -> Int32.to_string i
              | Fint32 i -> Int32.to_string i
              | _ -> "???"
            ))
  ) tags;
  Buffer.contents buf

let string_of_state state =
  match state with
    NotConnected
  | NewHost -> ""
  | Connecting -> "Connecting"
  | Connected_initiating -> "Initiating"
  | Connected_busy
  | Connected_idle -> "Connected"
  | Connected_queued -> "Queued"
  | RemovedHost -> "Removed"

let color_of_state state =
  match state with
  | Connected_busy
  | Connected_idle -> Some !!color_connected 
  | Connecting -> Some !!color_connecting
  | NotConnected
  | NewHost
  | Connected_initiating
  | Connected_queued
  | RemovedHost -> Some !!color_not_connected

let color_of_server s = color_of_state s.server_state

let nconnected_servers = ref 0

  
let comment_item t get_name get_md4 = 
  ("Comment", for_selection t
      (fun x -> 
        let module C = Configwin in
        let comment = ref "" in
        match C.simple_get (Printf.sprintf "Comment %s" 
              (get_name x))
            [
              C.text ~f:(fun s -> comment := s)
		(Printf.sprintf "Comment on %s:" (get_name x))
		""
            ] 
	with
          C.Return_ok -> 
            if !comment <> "" then
              gui_send (Command (Printf.sprintf 
                    "comment %s \"%s\""
                    (Md4.to_string (get_md4 x))
                  (String.escaped !comment)))
        | _ -> ()
    )
  )

  
let (clist_servers : 
    (int, server_info) MyCList.t) = 
  MyCList.create gui gui#tab_servers#clist_servers 
    ~color: color_of_server
    [
(* IP & PORT *)      
    (fun s -> Printf.sprintf "%16s : %-5d" (
          string_of_addr  s.server_addr) s.server_port);
(* STATUS *)  
    (fun s -> string_of_state s.server_state);
(* NUSERS *)  
    (fun s -> 
        if s.server_nusers = 0 then "" else
        Printf.sprintf "%+5s" (string_of_int s.server_nusers));
(* NFILES *)  
    (fun s -> 
        if s.server_nfiles = 0 then "" else
          Printf.sprintf "%+7s"  (string_of_int s.server_nfiles));
(* NAME & DESC *)    
    (fun s -> 
        if s.server_name = "" then "" else
        Printf.sprintf "%s [%s]" 
        s.server_name s.server_description);
  ] 

let (clist_server_users : (int, user_info) MyCList.t) =
  MyCList.create gui gui#tab_servers#clist_users
  [
(* KIND *) (fun u -> if Ip.valid u.user_ip then "Direct" else "");
(* NAME *) (fun u -> u.user_name)
  ]

let current_server = ref (-1)

let server_make_menu t =
  (List.map 
      (fun (s,f) -> `I (s, f))
    ([("Resolve IP", for_selection clist_servers
            (fun s -> 
              if addr_is_ip s.server_addr then
                let name = Ip.resolve_one (ip_of_addr s.server_addr) in
                s.server_addr.addr_name <- name;
                MyCList.set_value clist_servers s.server_num s
              else
              let ip = ip_of_addr s.server_addr in
              s.server_addr.addr_name <- "";
              MyCList.set_value clist_servers s.server_num s
          )
        );

      ])
  )
  
let search_make_menu t =
  (List.map 
      (fun (s,f) -> `I (s, f))
    ([comment_item t first_name (fun r -> r.result_md4)])
  )
  
  
let nservers = ref 0
let nfriends = ref 0
let nresults = ref 0
let current_search = ref (-1)
  
let update_searches_label () =
  gui#label_searches_status#set_text (Printf.sprintf 
      "%d Friends %d Search Results" !nfriends !nresults)
  
let update_server_label () =
  gui#label_servers_status#set_text
    (Gui2_messages.connected_to_servers !nconnected_servers !nservers)
  
let _ =
  MyCList.set_can_select_all clist_servers;
  MyCList.set_can_select_all clist_server_users;
  MyCList.set_replace_value clist_servers (fun old_s new_s ->
      begin
        match old_s.server_users, new_s.server_users with
          Some _, None -> new_s.server_users <- old_s.server_users
        | _ -> ()
      end;
      new_s);
  MyCList.set_selected_callback clist_servers (fun _ s ->
      if !current_server <> s.server_num then begin
          current_server := s.server_num;
          match s.server_users with
            None -> 
              gui_send (GetServer_users (server_key s))
          | Some users -> 
              MyCList.clear clist_server_users;
        end;
      match s.server_users with
        None -> ()
      | Some users ->
          List.iter (fun u_num -> 
              MyCList.update clist_server_users u_num (find_user u_num)) 
          users;
  );
  MyCList.set_context_menu clist_servers server_make_menu;
  MyCList.set_size_callback clist_servers (fun n ->
      nservers := n;
      update_server_label ()
  )
  
let add_user_to_friends =
  for_selection  clist_server_users (fun u ->
      gui_send (AddUserFriend u.user_num))
  

  
let string_of_file_state state =
  match state with
    FileDownloading -> "Downloading"
  | FileCancelled -> "Cancelled"
  | FilePaused -> "Paused"
  | FileDownloaded -> "Done"
(*  | FileRemoved -> "Removed"*)
  | FileNew -> assert false
  | FileShared -> "Shared"
      
let some_is_available f =
  let b = ref false in
  let len = String.length f.file_availability in
  for i = 0 to len - 1 do
    b := !b or f.file_availability.[i] <> '0'
  done;
  !b

let color_opt_of_file f =
  if f.file_download_rate > 0. then
    Some !!color_downloading
  else if some_is_available f then
    Some !!color_available
  else
    Some !!color_not_available
 
let (clist_downloads : 
       (int, file_info) MyCList.t) = 
  MyCList.create gui gui#tab_downloads#clist_downloads
    ~color: color_opt_of_file
    [
(* FILENAME *)
    (fun f -> match f.file_names with
          [] -> "<unknown>"
        | name :: _ -> short_name name);
(* SIZE *)    
    (fun f -> Printf.sprintf "%+10s" (Int32.to_string f.file_size));
(* DOWNLOADED *)
    (fun f -> Printf.sprintf "%+10s" (Int32.to_string f.file_downloaded));
(* PERCENT *)
    (fun f -> 
        Printf.sprintf "%5.1f" (
          Int32.to_float f.file_downloaded /. Int32.to_float f.file_size
          *. 100.)
        );
(* RATE *)
    (fun f -> 
        if f.file_download_rate > 0. then
          Printf.sprintf "%5.1f" (
            (f.file_download_rate /. 1024.) )
        else "");
(* STATE *)
    (fun f -> string_of_file_state f.file_state);    
(* AVAIL *)
    (fun f -> 
        let len = String.length f.file_availability in
        let p = ref 0 in
        for i = 0 to len - 1 do
          if f.file_availability.[i] <> '0' then begin
              incr p
            end
        done;
        if len = 0 then "" else 
          Printf.sprintf "%5.1f" (float_of_int !p /. float_of_int len *. 100.)
    );
(* MD4 *)
    (fun f -> Md4.to_string f.file_md4);    
  ]

let color_of_location l = color_of_state l.client_state

let shorten_client_name c =
  let len = String.length c.client_name in
  if len <= !!max_client_name_len then c.client_name else
    String.sub c.client_name 0 !!max_client_name_len
  
let (clist_file_locations :
    (int, client_info) MyCList.t) =
  MyCList.create gui tab_downloads#clist_locations
    ~color: color_of_location
    [
    (fun c -> match c.client_kind with
          Known_location _ -> "Direct"
        | _ -> "");
    shorten_client_name;
    (fun c -> string_of_state c.client_state);
  ]

let add_friend_location =
  for_selection clist_file_locations (fun c ->
      gui_send (AddClientFriend c.client_num)
  )  

  
let get_file_md4  f = f.file_md4

let get_file_name file =
  match file.file_names with
    [] -> "<unknown>"
  | name :: _ -> short_name name
  
let menu_save_file t =
  match MyCList.selection t  with
  | [file] ->
      
      let save_as file () =
        let file_opt = GToolbox.input_string ~title: Gui2_messages.mes_save 
            Gui2_messages.mes_save in
        match file_opt with
          None -> ()
        | Some name -> 
            prerr_endline ("save as "^name); 
            gui_send (GuiProto.SaveFile (file.file_num, name))
      in
      List.map 
        (fun (s,f) -> `I (s, f))
      (      
        let items =
          ("Preview file", (fun _ ->
                gui_send (Preview file.file_num))) ::
          (comment_item t get_file_name get_file_md4) ::
          (Gui2_messages.mes_save, save_as file) ::
          
          (List.map (fun name ->
                name, (fun _ -> 
                    gui_send (GuiProto.SaveFile (file.file_num, name))
                )) file.file_names
          )
        in
        match file.file_format with
          Mp3 tag ->
            let edit_mp3tag file () = 
(*              Printf.printf "do it"; print_newline (); *)
              (
                Mp3_ui.edit_tag_v1 Gui2_messages.mes_edit_mp3 tag ;
                gui_send (GuiProto.ModifyMp3Tags (file.file_num, tag))
              )
            in
            (Gui2_messages.mes_edit_mp3, edit_mp3tag file) :: items
        
        | _ ->
            items)
  
  | _ -> []
      
let menu_downloads_file t =
  List.map 
    (fun (s,f) -> `I (s, f))
  [ 
    "Pause/Resume file(s)", for_selection clist_downloads (fun file -> 
        gui_send (SwitchDownload (file.file_num, 
            match file.file_state with
                FilePaused -> true
              | _ -> false)));
    "Verify all chunks file(s)",
    for_selection clist_downloads
      (fun file -> 
        gui_send (VerifyAllChunks file.file_num));
    "Preview file",
    for_selection clist_downloads
      (fun file ->
        gui_send (Preview file.file_num));
    comment_item t get_file_name get_file_md4;
    "Get Format Info",
    for_selection clist_downloads (fun file ->
        gui_send (QueryFormat file.file_num));
    "", (fun _ -> ());
    "Cancel file(s)", 
    for_selection clist_downloads (fun file -> 
        gui_send (RemoveDownload_query file.file_num));
  ]
      
let (clist_downloaded : 
    (int, file_info) MyCList.t) = 
  MyCList.create gui gui#tab_downloads#clist_downloaded
    ~color: (fun _ -> Some !!color_downloaded)
    [
    get_file_name;
    (fun f -> Printf.sprintf "%+10s" (Int32.to_string f.file_size));
    (fun f -> Printf.sprintf "%+10s" (Int32.to_string f.file_downloaded));
    (fun f -> string_of_file_state f.file_state);
    (fun f -> Md4.to_string f.file_md4);    
  ]

  
  
let current_file = ref None
  
let ndownloads = ref 0
let ndownloaded = ref 0

let nlocations = ref 0
let nclocations = ref 0
  
let set_clist_file_locations_file file =
  begin
    match !current_file with 
      Some f when f == file -> ()
    | _ ->
        current_file := Some file;
        nclocations := 0;
        nlocations := 0;
        match file.file_sources with
          None -> 
            gui_send (GetFile_locations file.file_num)
        | Some mi -> 
            MyCList.clear clist_file_locations;
  end;
  match file.file_sources with
    None -> ()
  | Some sources ->
      nclocations := 0;
      nlocations := 0;
      List.iter (fun num ->
          try
            let c = Hashtbl.find locations num in
            if is_connected c.client_state then incr nclocations;
            MyCList.update clist_file_locations c.client_num c
          with _ -> 
              gui_send (GetClient_info num)
      ) 
      sources

let update_locations_label () =
  tab_downloads#label_locations_status#set_text (Mes.connected_to_locations !nclocations !nlocations)
      
let update_download_label () =
  gui#label_download_status#set_text (Mes.downloaded_files !ndownloaded !ndownloads);
  tab_downloads#label_downloading#set_text (Mes.downloading_files !ndownloads);
  tab_downloads#label_downloaded#set_text (Mes.files_downloaded !ndownloaded)

let save_all_files _ =
  MyCList.iter clist_downloaded (fun _ file ->
      let name = 
        match file.file_names with
          [] -> Md4.to_string file.file_md4
        | name :: _ ->  name
      in
      gui_send (GuiProto.SaveFile (file.file_num, name)))
  
let current_file = ref None

let colorGreen = `NAME "green"
let colorRed   = `NAME "red"
let colorBlue  = `NAME "blue"
let colorWhite =`WHITE
let colorBlack = `BLACK

let drawing = ref (None :   [ `window] GDraw.drawable option)
  
let redraw_current _ =
  let drawing = match !drawing with
      None -> 
        
        let w = tab_downloads#draw_availability#misc#window in
        let d = new GDraw.drawable w in
        tab_downloads#draw_availability#misc#show ();
        drawing := Some d;
        d        
    | Some d -> d
  in

  let wx, wy = drawing#size in
  drawing#set_foreground colorWhite;
  drawing#rectangle ~filled: true ~x:0 ~y:0 ~width:wx ~height:wy ();
  match !current_file with
    None -> true
  | Some file ->
      let nchunks = String.length file.file_chunks in
      let dx = mini 3 (wx / nchunks) in
      let offset = (wx - nchunks * dx) / 2 in
      let offset = if offset < 0 then 0 else offset in
      for i = 0 to nchunks - 1 do
        drawing#set_foreground (
          if file.file_chunks.[i] = '1' then
            colorGreen
          else
          match  file.file_availability.[i] with
            '0' -> colorRed
          | '1' -> colorBlue
          | _ -> colorBlack);
        drawing#rectangle ~filled: true
        ~x:(offset + i*dx) ~y: 0 
          ~width: dx ~height:wy ()
      done;
      false

  
let update_current_file () =
  tab_downloads#label_file_info#set_text (
    match !current_file with
      None -> ""
    | Some file ->
        Printf.sprintf "NAME: %s SIZE: %s FORMAT: %s" (match file.file_names with
            [] -> Md4.to_string file.file_md4
          | name :: _ -> name)
        (Int32.to_string file.file_size) 
        (match file.file_format with
          AVI f ->
            Printf.sprintf "AVI: %s %dx%d %d fps %d bpf"
              f.avi_codec f.avi_width f.avi_height 
              f.avi_fps f.avi_rate
	| Mp3 tag ->
	    Printf.sprintf "MP3: %s - %s (%d): %s"
	      tag.Mp3tag.artist tag.Mp3tag.album 
	      tag.Mp3tag.tracknum tag.Mp3tag.title
        | _ -> "Unknown")
        ;
  );
  redraw_current ()
      
let _ =
  MyCList.set_can_select_all clist_downloads;  
  MyCList.set_can_select_all clist_downloaded;  
  MyCList.set_can_select_all clist_file_locations;  
  MyCList.set_size_callback clist_downloads (fun n ->
      ndownloads := n;
      update_download_label ();
  );
  MyCList.set_size_callback clist_file_locations (fun n ->
      nlocations := n;
      update_locations_label ();
  );
  MyCList.set_replace_value clist_downloads (fun old_file new_file ->
      begin
        match old_file.file_sources, new_file.file_sources with
          Some _, None -> new_file.file_sources <- old_file.file_sources
        | _ -> ()
      end;
      new_file);
  MyCList.set_selected_callback clist_downloads (fun clist_downloads file ->
      MyCList.unselect_all clist_downloaded;
      set_clist_file_locations_file file;
      current_file := Some file;
      ignore (update_current_file ())
  );
  MyCList.set_size_callback clist_downloaded (fun n ->
      ndownloaded := n;
      update_download_label ()
  );

  MyCList.set_context_menu clist_downloads  menu_downloads_file;
  MyCList.set_context_menu clist_downloaded menu_save_file;
  MyCList.set_selected_callback clist_downloaded (fun _ file ->
      MyCList.unselect_all clist_downloads;
      set_clist_file_locations_file file)

let color_of_friend f = 
  match f.client_files with
    None -> Some !!color_default
  | Some _ -> Some !!color_files_listed

let clist_friends = MyCList.create gui tab_friends#clist_friends
    ~color: color_of_friend
    [
    (fun c -> 
        match c.client_files with
          None -> string_of_state c.client_state
        | Some _ -> "Files listed");    
    (fun c -> let name = c.client_name in
        if name = "" then "<unknown>" else shorten_client_name c);
    (fun c -> match c.client_kind with
          Known_location _ -> "Direct"
        | _ -> "");
  ]
  
let clist_friend_files = MyCList.create gui tab_friends#clist_friends_files
    [
(* FILENAME *) 
    (fun r -> (*short_name*) (first_name r));
(* SIZE *)
    (fun r -> Printf.sprintf "%10s" (Int32.to_string r.result_size));
(* PROPERTIES *)
    (fun r -> string_of_tags r.result_tags);
(* MD4 *)
    (fun r -> Md4.to_string r.result_md4);
    ]

let _ =
  MyCList.set_can_select_all clist_friends;
  MyCList.set_can_select_all clist_friend_files

    
let friends_make_menu t =
  (List.map 
      (fun (s,f) -> `I (s, f))
    ([("Connect Friend", for_selection clist_friends
            (fun c -> 
              gui_send (ConnectFriend c.client_num)
              )
        );
        
      ])
  )

  
let _ =
  MyCList.set_size_callback clist_friends (fun n ->
      nfriends := n;
      update_searches_label ());
  MyCList.set_context_menu clist_friends friends_make_menu
  
let remove_friend =
  for_selection clist_friends (fun c ->
      gui_send (RemoveFriend c.client_num))
  
let find_friend _ =
  let friend_name = tab_friends#entry_find_friend#text in
  if friend_name <> "" then begin
      gui_send (FindFriend friend_name)
    end
  
let view_users =
  for_selection clist_servers (fun s ->
      gui_send (ViewUsers (server_key s)))

let connect_server =
  for_selection clist_servers (fun s ->
      gui_send (ConnectServer (server_key s)))

let disconnect_server =
  for_selection clist_servers (fun s ->
      gui_send (DisconnectServer (server_key s)))
  
let remove_old_servers _ =
  gui_send CleanOldServers
  
let connect_all =
  for_selection  clist_downloads
    (fun file -> gui_send (ConnectAll file.file_num))
  
let current_friend =  ref (-1) 
let _ =
  MyCList.set_replace_value clist_friends (fun old_c new_c ->
      begin
        match old_c.client_files, new_c.client_files with
          Some _, None -> 
            new_c.client_files <- old_c.client_files
        | _ -> ()
      end;
      new_c);
  MyCList.set_selected_callback clist_friends (fun _ c ->
      if !current_friend <> c.client_num then begin
          current_friend := c.client_num;
          MyCList.clear clist_friend_files;
          match c.client_files with
            None -> 
              gui_send (GetClient_files c.client_num)
          | Some files -> 
              MyCList.clear clist_friend_files;              
        end;
      match c.client_files with
        None -> ()
      | Some tree -> 
          List.iter (fun r -> 
              MyCList.update clist_friend_files r.result_md4 r) 
          (list_files tree);
  )

  
  
let (searches : ((int, (int, result_info) MyCList.t * GMisc.label) Hashtbl.t))
  = Hashtbl.create 127

let search_npages = ref 0
let (search_pages :  (int, result_info) MyCList.t option array) = Array.create 50 None
  
let add_search_page v =
  let n = !search_npages in
  search_pages.(n) <- Some v;
  incr search_npages;
  n
  
let get_search_page v =
  let rec iter n =
    match search_pages.(n) with
    | Some vv when v == vv -> n
    | _ -> iter (n+1)
  in
  iter 0
  
let remove_search_page v =
  let n = get_search_page v in
  decr search_npages;
  Array.blit search_pages (n+1) search_pages n (!search_npages - n);
  n
      
let download_cancel (gui:gui) () =
  let module P = GuiProto in
  for_selection clist_downloads (fun file ->
      gui_send (RemoveDownload_query file.file_num)
  ) ()
  
let search_stop clist_search gui num () =
  Hashtbl.remove searches num
  
let search_close clist_search gui num () =
  (try Hashtbl.remove searches num with _ -> ());
  gui_send (ForgetSearch num);
  let n = remove_search_page clist_search in
  tab_searches#notebook_results#remove_page n
      
      
let search_download clist_search gui () =
  let module P = GuiProto in
  for_selection  clist_search (fun r ->
      gui_send (Download_query (r.result_names, r.result_num))
  ) ()

let download_ed2k_url _ =
  let url = tab_downloads#entry_ed2k_url#text in
  gui_send (Url url)
  
let download_md4 _ =
  let md4 = tab_downloads#entry_md4#text in
  gui_send (Url
      (Printf.sprintf "ed2k://|file||0|%s|" md4));
  tab_downloads#entry_md4#set_text ""
  
let download_friend_files =
  let module P = GuiProto in
  for_selection clist_friend_files (fun r ->
      gui_send (Download_query (r.result_names, r.result_num))
  )   

  
let canon_client c =
  let c= 
    try
      let cc = Hashtbl.find locations c.client_num in
      
      let is_in_locations =
        try
          ignore (MyCList.find clist_file_locations c.client_num);
          true
        with _ -> false in
      if is_in_locations then
        begin
          match is_connected cc.client_state, is_connected c.client_state with
            false , false
          | true, true -> ()
          | false , _ -> 
              nclocations := !nclocations + 1;
              update_locations_label ()
          | _, false -> 
              nclocations := !nclocations - 1;
              update_locations_label ()
        end;
      if c.client_files <> None then cc.client_files <- c.client_files;
      cc.client_state <- c.client_state;
      cc.client_type <- c.client_type;
      cc.client_rating <- c.client_rating;
      cc.client_name <- c.client_name;

      cc.client_kind <- c.client_kind;
      cc.client_tags <- c.client_tags;
      
      if is_in_locations then
        MyCList.set_value clist_file_locations c.client_num cc;
      cc
    with _ ->
        Hashtbl.add locations c.client_num c;
        c
  in
  begin
    match c.client_type with
      FriendClient ->
        begin
          try
            ignore (MyCList.find clist_friends c.client_num);
            MyCList.set_value clist_friends c.client_num c
          with _ ->
              MyCList.add clist_friends c.client_num c
        end

    | _ ->
          (try MyCList.remove clist_friends c.client_num with _ -> ())
  end;
  c
  
let update_server key s os =
  begin
    match is_connected s.server_state, is_connected os.server_state with
      true, false -> incr nconnected_servers
    | false, true -> decr nconnected_servers
    | _ -> ()
  end;
  if s.server_state = RemovedHost then
    (try MyCList.remove clist_servers key with _ -> ())
  else
    MyCList.set_value clist_servers key s

let array_memq v tab =
  try
    for i = 0 to Array.length tab -1 do
      if tab.(i) == v then raise Exit
    done;
    false
  with Exit -> true
    
let update_file f =
  try
(*        Printf.printf "Download_file"; print_newline (); *)
    let clist = gui#tab_downloads#clist_downloads in
(* GUI2 REMOVED
    begin
      match f.file_sources with
        None -> ()
      | Some sources ->
          f.file_sources <- Some (List2.tail_map canon_client sources)
end;
  *)
    match f.file_state with
      FileCancelled ->
        MyCList.remove clist_downloads f.file_num 
    | FileShared ->
        MyCList.remove clist_downloaded f.file_num 
    | FileDownloaded ->
        begin
          (try MyCList.remove clist_downloads f.file_num  with _ -> ());
          (try
              MyCList.set_value clist_downloaded f.file_num f        
            with _ ->
                MyCList.add clist_downloaded f.file_num f
          )
        end
    | _ ->
        (try
            MyCList.set_value clist_downloads f.file_num f        
          with _ ->
              MyCList.add clist_downloads f.file_num f;
              gui_send (GetFile_locations f.file_num)              
        )
  with e ->
      Printf.printf "Exception %s in update file %s"
        (Printexc.to_string e) (Md4.to_string f.file_md4);
      print_newline () 
      
let value_reader (gui: gui) t sock =
  try
    let module P = GuiProto in
    match t with
    | Console text ->
        
        ignore (gui#tab_console#text#insert_text text 0)
    
    | CoreProtocol v -> 
        gui_protocol_used := min v GuiEncoding.best_gui_version;
        Printf.printf "Using protocol %d for communications" !gui_protocol_used;
        print_newline ();
        gui#label_connect_status#set_text "Connected";
        gui_send (Password (!!password))
    
    | P.Search_result (s_num, r_num) -> 
(*        Printf.printf "Search_result"; print_newline (); *)
        begin
          try
            let clist_search, label_query = 
              Hashtbl.find searches s_num in
            MyCList.add clist_search r_num (find_result r_num)
          with _ -> 
              ()
        end
    
    | P.Search_waiting (num,waiting) -> 
(*        Printf.printf "Search_waiting"; print_newline (); *)
        begin
          try
            let (_, label_waiting) = Hashtbl.find searches num in
            label_waiting#set_text (
              Printf.sprintf "Waiting for %d replies" waiting);
          with _ ->               ()
        end
    
    | File_downloaded (num, downloaded, rate) ->
        begin
          try 
            let f =
              try
                MyCList.find clist_downloads num
              with
                _ -> MyCList.find clist_downloaded num
            in
            update_file { f with
              file_downloaded = downloaded;
              file_download_rate = rate;
            } 
          with _ -> ()
        end
    
    | File_availability (num, chunks, avail) ->
        begin
          try 
            let f =
              try
                MyCList.find clist_downloads num
              with
                _ -> MyCList.find clist_downloaded num
            in
            update_file { f with
              file_chunks = chunks;
              file_availability = avail;
            }
          with _ -> ()
        end
    
    | File_info f ->
        begin try update_file f with _ -> () end
    
    | P.Server_info s ->
(*        Printf.printf "Server_info"; print_newline ();  *)
        let key = server_key s in
        begin
          try
            let os = MyCList.find clist_servers key in
            update_server key s os
          with _ ->
              if s.server_state <> RemovedHost then
                begin
                  if is_connected s.server_state then incr nconnected_servers;
                  MyCList.add clist_servers key s;
                end
        end;
        update_server_label ()
    
    | P.Server_state (key,state) ->
        begin
          try
            let os = MyCList.find clist_servers key in
            update_server key { os with server_state = state } os
          with _ -> (* should not happen ? *) 
              gui_send (GetServer_info key);
        end;
        update_server_label ()
    
    | P.Server_busy (key,nusers, nfiles) ->
        begin
          try
            let os = MyCList.find clist_servers key in
            update_server key { os with 
              server_nusers = nusers; server_nfiles = nfiles } os
          with _ -> (* should not happen ? *) 
              gui_send (GetServer_info key)
        end;
        update_server_label ()
    
    | P.Client_state (num, state) ->
        begin
          try
            let c = Hashtbl.find locations num in
            ignore (canon_client { c with client_state = state })
          with _ -> 
              gui_send (GetClient_info num)
        end
    
    | P.Client_friend (num, friend_kind) ->
        begin
          try
            let c = Hashtbl.find locations num in
            ignore (canon_client { c with client_type = friend_kind })
          with _ -> 
              gui_send (GetClient_info num)
        end
    
    | P.Client_info c -> 
(*        Printf.printf "Client_info"; print_newline (); *)
        begin try ignore (canon_client c) with _ -> () end;
        begin
          match !current_file with
            None -> ()
          | Some file ->
              let num = c.client_num in
              match file.file_sources with
                None -> ()
              | Some sources ->
                  if List.memq num sources then
                    let c = Hashtbl.find locations c.client_num in
                    if is_connected c.client_state then incr nclocations;
                    MyCList.update clist_file_locations c.client_num c
        end
    
    | User_info u -> add_user u
    | Result_info r -> add_result r
    
    | Client_file (c_num, dirname, r_num) ->
        begin
          try
            let r = find_result r_num in
            let c = Hashtbl.find locations c_num in
            let tree = match c.client_files with
                None -> { file_tree_list = []; file_tree_name = "" }
              | Some tree -> { tree with file_tree_list = tree.file_tree_list }
            in
            GuiTypes.add_file tree dirname r;
            ignore (canon_client { c with client_files = Some tree })
          with _ -> ()
        end
    
    | Server_user (s_num, u_num) ->
        begin
          try
            let os = MyCList.find clist_servers s_num in
            update_server s_num { os with server_users = Some 
                (match os.server_users with
                  None -> [u_num]
                | Some list ->
                    if List.memq u_num list then raise Exit;
                    u_num :: list)
            } os;
            update_server_label ()
          with _ -> ()
        end;
    
    | File_source (f_num, c_num) ->
        
        begin
          try 
            let f =
              try
                MyCList.find clist_downloads f_num
              with
                _ -> MyCList.find clist_downloaded f_num
            in
            let f = { f with
                file_sources = Some (
                  match f.file_sources with
                    None -> [c_num]
                  | Some list ->
                      if List.memq c_num list then raise Exit;
                      c_num :: list)
              }
            in
            update_file f
          with _ -> ()
        end
    
    
    | P.Options_info list ->
        let rec iter list =
          match list with
            [] -> ()
          | (name, value) :: tail ->
              (
                try
                  let reference = 
                    List.assoc name Gui2_options.client_options_assocs 
                  in
                  reference := value
                with _ -> 
                    ()
              );
              iter tail
        in
        iter list
        
    | Room_message (-1, PrivateMessage (c_num , s))
    | Room_message (_, PrivateMessage (c_num , s))
    | Room_message (0, PublicMessage (c_num , s))
    | MessageFromClient (c_num,s)
      ->
        let c = Hashtbl.find locations c_num in
        let name = c.client_name in
        let len = String.length name + 1 in
        let fg = `NAME "red" in
        ignore (gui#tab_friends#text_dialog#insert
            ~foreground:fg (name ^ ":"));
        ignore (gui#tab_friends#text_dialog#insert 
            (Printf.sprintf " <%s>\n" s));
      
    | Client_stats s ->
        gui#label_upload_status#set_text
          (Printf.sprintf "Upload %d/%s" 
            s.nshared_files (Int64.to_string s.upload_counter))

    | Room_info _
    | Room_add_user (_, _)
    | Room_remove_user (_, _)
    | Room_message (_, _)
    | Network_info _
    | DefineSearches _ -> (* not supported *) ()
    | (DownloadedFiles _|DownloadFiles _|ConnectedServers _)
    | (Shared_file_unshared _|Shared_file_upload (_, _, _)|Shared_file_info _)
      
    | Add_section_option _
    | Add_plugin_option _
      -> assert false
  with e ->
      Printf.printf "EXception %s in reader" (Printexc.to_string e);
      print_newline ()
