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

module O = Gui_options
module M = Gui_messages
module Com = Gui_com
module G = Gui_global
module Mi = Gui_misc

let (!!) = Options.(!!)

let _ = GMain.Main.init ()

let _ = 
  (try Options.load O.mldonkey_gui_ini with
      e ->
        Printf.printf "Exception %s in load options" (Printexc.to_string e);
        print_newline ();
  );
  let args = Options.simple_args O.mldonkey_gui_ini in
  Arg.parse args (Arg.usage args)  "mldonkey_gui: the GUI to use with mldonkey"

(* Check bindings *)
let _ = 
  if !!O.keymap_global = [] then
    (
     let a = O.add_binding O.keymap_global in
     a "A-s" M.a_page_servers;
     a "A-d" M.a_page_downloads;
     a "A-f" M.a_page_friends;
     a "A-q" M.a_page_queries;
     a "A-r" M.a_page_results;
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
  if !!O.keymap_results = [] then
    (
     let a = O.add_binding O.keymap_results in
     ()
    );
  if !!O.keymap_console = [] then
    (
     let a = O.add_binding O.keymap_console in
     ()
    )

(** {2 Handling core messages} *)

open Gui_proto
open CommonTypes
  
let canon_client gui c =
  let box_file_locs = gui#tab_downloads#box_locations in 
  let box_friends = gui#tab_friends#box_friends in
  let c = 
    try
      let cc = Hashtbl.find G.locations c.client_num in
      
      let is_in_locations =
        try
          ignore (box_file_locs#find_client c.client_num);
          true
        with _ -> false 
      in
      if is_in_locations then
        gui#tab_downloads#h_update_location c;
      
      if c.client_files <> None then begin
          cc.client_files <- c.client_files;
        end;
      cc.client_state <- c.client_state;
      begin
        if c.client_type <> cc.client_type then begin
            match c.client_type, cc.client_type with
            | NormalClient, _ ->
                box_friends#h_remove_friend c.client_num
            | _  ->
                box_friends#h_update_friend c
          end
      end;
      cc.client_type <- c.client_type;
      cc.client_rating <- c.client_rating;
      cc.client_name <- c.client_name;

      cc.client_kind <- c.client_kind;
      cc.client_tags <- c.client_tags;
      
      cc
    with _ ->
        Hashtbl.add G.locations c.client_num c;
        begin
          match c.client_type with
            NormalClient -> ()
          | _ -> box_friends#h_update_friend c;
        end;
        c
  in
  c
  
let value_reader gui t sock =
  try
    match t with
    | Console text ->
	gui#tab_console#insert text

    | Network_info n ->
        Hashtbl.add Gui_global.networks n.Gui_proto.network_num n
        
    | LocalInfo l ->
        gui#label_upload_status#set_text (
          Printf.sprintf "%s %d/%d" M.upload l.upload_counter l.shared_files)
        
    | Connected v -> 
        if v <> CommonTypes.version then 
	  (
           Printf.printf "Bad GUI version"; print_newline ();
           TcpBufferedSocket.close sock "bad version";
          )
        else 
	  (
(*        Printf.printf "Connected"; print_newline (); *)
           gui#label_connect_status#set_text M.connected;
           Com.send (Password (CommonTypes.version,!!O.password))
	  )
 
    | Search_result (num,r) -> 
        begin try
          let r = Hashtbl.find G.results r in
            gui#tab_queries#h_search_result num r
          with _ -> 
              Printf.printf "Exception in Search_result %d %d" num r;
              print_newline ();
        end

    | Search_waiting (num,waiting) -> 
	gui#tab_queries#h_search_waiting num waiting

    | File_source (num, src) -> 
	gui#tab_downloads#h_file_location num src

     | File_downloaded (num, downloaded, rate) ->
	gui#tab_downloads#h_file_downloaded num downloaded rate
      
    | File_availability (num, chunks, avail) ->
	gui#tab_downloads#h_file_availability num chunks avail;

    | File_info f ->
	gui#tab_downloads#h_file_info f;
            
    | Server_info s ->
        gui#tab_servers#h_server_info s;
        gui#update_server_label
    
    | Server_state (key,state) ->
        gui#tab_servers#h_server_state key state ;
        gui#update_server_label 
   
    | Server_busy (key,nusers, nfiles) ->
	gui#tab_servers#h_server_busy key nusers nfiles ;
        gui#update_server_label 
    
    | Server_user (key, user) ->
        if not (Hashtbl.mem G.users user) then begin
            Gui_com.send (GetUser_info user);
          end else begin
            gui#tab_servers#h_server_user key user;
            gui#update_server_label 
          end

    | Room_info room ->
(*        Printf.printf "Room info %d" room.room_num; print_newline (); *)
        let wnote = (gui#wnote_rooms :> GPack.notebook) in
        Gui_rooms.room_info wnote room
          
    | User_info user ->
        let user = try 
            let u = Hashtbl.find G.users user.user_num  in
            u.user_state <- user.user_state;
            u
          with Not_found ->
              Hashtbl.add G.users user.user_num user; 
              user
        in
        gui#tab_servers#h_server_user user.user_server user.user_num;
        Gui_rooms.user_info user;
        gui#update_server_label

    | Room_user (num, user_num) -> 
        
        begin try
            Gui_rooms.add_room_user num user_num
          with e ->
              Printf.printf "Exception in Room_user %d %d" num user_num;
              print_newline ();
        end

    | Room_message (num, msg) ->
        begin try
            Gui_rooms.add_room_message num msg
                  with e ->
              Printf.printf "Exception in Room_message %d" num;
              print_newline ();
        end
        
    | GuiConnected -> 
	()

    | Options_info list ->
(*        Printf.printf "Options_info"; print_newline ();*)
        let rec iter list =
          match list with
            [] -> ()
          | (name, value) :: tail ->
              (
               try
                 let reference = 
		   List.assoc name Gui_options.client_options_assocs 
		 in
                 reference := value
               with _ -> 
                 ()
	      );
              iter tail
        in
        iter list

    | DefineSearches l ->
	gui#tab_queries#h_define_searches l
        
    | Client_state (num, state) ->
(*
	Printf.printf "Client_state" ; print_newline ();
*)
	(
         try
           let c = Hashtbl.find G.locations num in
           ignore (canon_client gui { c with client_state = state })
         with _ -> 
           Com.send (GetClient_info num)
	)

    | Client_friend (num, friend_kind) ->
        (
	 try
           let c = Hashtbl.find G.locations num in
           ignore (canon_client gui { c with client_type = friend_kind });
         with _ -> 
           Com.send (GetClient_info num)
	)

    | Result_info r ->
        if not (Hashtbl.mem G.results r.result_num) then
          Hashtbl.add G.results r.result_num r
        
    | Client_file (num , file_num) ->
        (
          try
            let file = Hashtbl.find G.results file_num in
            try
              let c = Hashtbl.find G.locations num in
              let files = match c.client_files with
                  None -> []
                | Some files -> files in
              ignore (canon_client gui
                  { c with client_files = Some (file :: files) });
            with _ ->
                Com.send (GetClient_info num);
          with _ ->  ()
	)

    | Client_info c -> 
(*        Printf.printf "Client_info"; print_newline (); *)
        (
	 try
	   ignore (canon_client gui c) ;
	 with _ -> ()
	)
(* A VOIR : Ca sert à quoi le bouzin ci-dessous ?
ben, ca sert a mettre a jour la liste des locations affichees pour un
fichier selectionne. Si ca marche toujours dans ton interface, pas de
  probleme ...
        begin
          match !current_file with
            None -> ()
          | Some file ->
              let num = c.client_num in
              match file.file_more_info with
                None -> ()
              | Some fmi ->
                  if array_memq num fmi.file_known_locations ||
                    array_memq num fmi.file_indirect_locations then
                    let c = Hashtbl.find locations c.client_num in
                    if is_connected c.client_state then incr nclocations;
                    MyCList.update clist_file_locations c.client_num c
        end
*)


  with e ->
      Printf.printf "Exception %s in reader" (Printexc.to_string e);
      print_newline ()


let main () =
  let gui = new Gui_window.window () in
  let w = gui#window in
  let quit () = 
    (try
        Gui_misc.save_gui_options gui;
        Gui_com.disconnect ();
      with _ -> ());
    exit 0
  in
  Gui_config.update_toolbars_style gui;
  ignore (w#connect#destroy quit);

  (** menu actions *)
  ignore (gui#itemQuit#connect#activate w#destroy) ;
  ignore (gui#itemKill#connect#activate (fun () -> Com.send KillServer));
  ignore (gui#itemReconnect#connect#activate 
	    (fun () ->Com.reconnect gui (value_reader gui)));
  ignore (gui#itemDisconnect#connect#activate 
	    (fun () -> Com.disconnect ()));
  ignore (gui#itemServers#connect#activate (fun () -> gui#notebook#goto_page 0));
  ignore (gui#itemDownloads#connect#activate (fun () -> gui#notebook#goto_page 1));
  ignore (gui#itemFriends#connect#activate (fun () -> gui#notebook#goto_page 2));
  ignore (gui#itemSearches#connect#activate (fun () -> gui#notebook#goto_page 3));
  ignore (gui#itemResults#connect#activate (fun () -> gui#notebook#goto_page 4));
  ignore (gui#itemOptions#connect#activate (fun () -> Gui_config.edit_options gui));
  ignore (gui#itemConsole#connect#activate (fun () -> gui#notebook#goto_page 5));
  ignore (gui#itemHelp#connect#activate (fun () -> gui#notebook#goto_page 6));


  (** connection with core *)
  Com.reconnect gui (value_reader gui) ;
  
  let gtk_handler timer =
    BasicSocket.reactivate_timer timer;
    while Glib.Main.pending () do
      ignore (Glib.Main.iteration false)
    done;
  in
    
  BasicSocket.add_timer 0.1 gtk_handler;
(*  BasicSocket.add_timer 2.0 update_sizes;*)
  let never_connected = ref true in
  BasicSocket.add_timer 1.0 (fun timer ->
      if !never_connected then 
        match !Com.connection with
          None ->
            BasicSocket.reactivate_timer timer;
            Com.reconnect gui (value_reader gui)
        | _ -> 
            never_connected := false
  );

  BasicSocket.loop ()
;;

main ()
