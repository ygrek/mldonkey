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

open CommonTypes
open Options
open BasicSocket
open TcpClientSocket
open Unix
open Gui_proto
open Gui2_options
module O = Gui2_options
module M = Gui2_messages
open MyCList
open Gui2_handler  
open Gui2
open Gui2_misc
    
let _ =
  
  ignore (window#add gui#box#coerce) ;
  ignore (gui#box#connect#destroy (fun _ ->
        window#destroy ())) ;
    ignore (window#connect#destroy (fun _ -> 
        GMain.Main.quit ())) ;

(* Connect buttons to actions *)
  ignore (gui#itemQuit#connect#activate (fun _ -> 
        save_gui_options ();
        exit 0));
  ignore (gui#itemKill#connect#activate (fun _ -> 
        gui_send KillServer));
  ignore (gui#itemReconnect#connect#activate (fun _ -> reconnect gui));
  ignore (gui#itemDisconnect#connect#activate (fun _ -> disconnect gui));
  ignore (gui#itemServers#connect#activate (fun _ -> gui#notebook#goto_page 0));
  ignore (gui#itemDownloads#connect#activate (fun _ -> gui#notebook#goto_page 1));
  ignore (gui#itemFriends#connect#activate (fun _ -> gui#notebook#goto_page 2));
  ignore (gui#itemSearches#connect#activate (fun _ -> gui#notebook#goto_page 3));
  ignore (gui#itemOptions#connect#activate Gui2_config.edit_options);
  ignore (gui#itemConsole#connect#activate (fun _ -> gui#notebook#goto_page 5));
  ignore (gui#itemHelp#connect#activate (fun _ -> gui#notebook#goto_page 6));
  ignore (tab_searches#button_search_submit#connect#clicked (submit_search gui false));
  ignore (tab_searches#button_local_search#connect#clicked (submit_search gui true));  
  
  ignore (tab_searches#button_extended_search#connect#clicked 
      (fun _ -> gui_send ExtendedSearch));
  
  ignore (tab_searches#entry_search_words#connect#activate (submit_search gui false));
(*
  ignore (tab_searches#clist_search_results#connect#select_row (search_set_selection gui));
ignore (tab_searches#clist_search_results#connect#unselect_row (search_unset_selection gui));
  *)
(*
  ignore (gui#clist_download#connect#select_row (download_set_selection gui));
ignore (gui#clist_download#connect#unselect_row (download_unset_selection gui));
*)
  ignore (tab_downloads#button_download_cancel#connect#clicked
      (download_cancel gui));
  ignore (tab_servers#button_servers_add#connect#clicked
      (servers_addserver gui));  
  ignore (tab_servers#entry_servers_new_ip#connect#activate
      (servers_addserver gui));
  ignore (tab_friends#button_friends_add#connect#clicked 
      (friends_addfriend gui));
  ignore (tab_servers#button_servers_connect_more#connect#clicked (
      servers_connect_more gui));
  ignore (tab_servers#button_servers_remove#connect#clicked (
      servers_remove gui));
  ignore (gui#tab_console#entry_command#connect#activate (fun _ ->
        gui_send (Command gui#tab_console#entry_command#text);
        gui#tab_console#entry_command#set_text ""
    ));
  
  ignore (tab_friends#entry_dialog#connect#activate (fun _ ->
        let s = tab_friends#entry_dialog#text in
        List.iter (fun c ->
            gui_send (SendMessage (-1, PrivateMessage (c.client_num,s))))
        (MyCList.selection clist_friends);
        tab_friends#entry_dialog#set_text "";
    ));
  
  
  ignore (tab_downloads#button_downloaded_save#connect#clicked 
      save_all_files);
  
  ignore (tab_downloads#button_download_add_friend#connect#clicked
      add_friend_location);
  
  ignore (tab_friends#button_friends_download#connect#clicked
      download_friend_files);
  ignore (tab_friends#button_friends_remove#connect#clicked
      remove_friend);
  ignore (tab_friends#entry_find_friend#connect#activate find_friend);
  
  ignore (tab_servers#button_remove_old_servers#connect#clicked
      remove_old_servers);
  ignore (tab_servers#button_servers_view_users#connect#clicked
      view_users);
  ignore (tab_downloads#button_download_retry_connect#connect#clicked
      connect_all);
  ignore (tab_servers#button_servers_connect#connect#clicked
      connect_server);
  ignore (tab_servers#button_servers_disconnect#connect#clicked
      disconnect_server);
  ignore (tab_servers#button_add_to_friends#connect#clicked 
      add_user_to_friends);
  
  ignore (gui#tab_console#button_clear_console#connect#clicked
      (fun _ ->
        let text = gui#tab_console#text in
        text#delete_text 0 (text#length)));
  
  ignore (tab_help#text#insert_text Gui2_messages.help_text 0);
  
  ignore (tab_downloads#entry_ed2k_url#connect#activate
      download_ed2k_url);
  ignore (tab_downloads#entry_md4#connect#activate
      download_md4);
  
  ignore (tab_downloads#draw_availability#event#connect#expose
      ~callback:redraw_current);
  
  ignore (window#add_accel_group gui#accel_menubar);
  ignore (window#show ()) ;
  
  
  ignore (gui#notebook#connect#switch_page 
      (fun n -> current_page := n));

(* Keyboard shortcuts *)
  let add w ?(cond=(fun () -> true)) l ((mods, k), action) = 
    try
      let f = List.assoc action l in
      Okey.add ~cond w ~mods k f
    with
      Not_found ->
        prerr_endline (Gui2_messages.action_unknown action)
  in

(* Global shortcuts *)
  let global_actions = [
      M.a_page_servers, gui#itemServers#activate ;
      M.a_page_downloads, gui#itemDownloads#activate;
      M.a_page_friends, gui#itemFriends#activate;
      M.a_page_queries, gui#itemSearches#activate;
      M.a_page_options, gui#itemOptions#activate;
      M.a_page_console, gui#itemConsole#activate;
      M.a_page_help, gui#itemHelp#activate;
      M.a_next_page, gui#notebook#next_page;
      M.a_previous_page, gui#notebook#previous_page;
      M.a_reconnect, gui#itemReconnect#activate;
      M.a_exit, gui#itemQuit#activate;
    ] 
  in
  List.iter (add window global_actions) !!O.keymap_global;

(* Servers shortcuts *)
  let servers_actions = global_actions @
      [
      M.a_connect, gui#tab_servers#button_servers_connect#clicked;
      M.a_connect_more, gui#tab_servers#button_servers_connect_more#clicked;
      M.a_select_all, (fun () -> ignore (MyCList.select_all clist_servers)) ;
    ] 
  in
  List.iter
    (add window ~cond: (fun () -> !current_page = 0) servers_actions)
  !!O.keymap_servers;

(* Downloads shortcuts *)
  let downloads_actions = global_actions @ 
      [
      M.a_cancel_download, gui#tab_downloads#button_download_cancel#clicked ;
      M.a_save_all_files, (fun () -> ignore (save_all_files ()));
      M.a_menu_save_file, 
      (fun () -> GToolbox.popup_menu ~x: 0 ~y: 0 
            ~entries: (menu_save_file clist_downloaded)) ;
      M.a_select_all, (fun () -> ignore (MyCList.select_all clist_downloads)) ;
    ]
  in
  List.iter
    (add window ~cond: (fun () -> !current_page = 1) downloads_actions)
  !!O.keymap_downloads;

(* Friends shortcuts *)
  let friends_actions = global_actions @ 
      [
      M.a_download_selection, gui#tab_friends#button_friends_download#clicked ;
      M.a_remove_friend, gui#tab_friends#button_friends_remove#clicked ;
      M.a_select_all, (fun () -> ignore (MyCList.select_all clist_friends)) ;
    ]
  in
  List.iter
    (add window ~cond: (fun () -> !current_page = 2) friends_actions)
  !!O.keymap_friends;

(* Queries shortcuts *)
  let queries_actions = global_actions @ 
      [
    ]
  in
  List.iter
    (add window ~cond: (fun () -> !current_page = 3) queries_actions)
  !!O.keymap_queries;

(* Console shortcuts *)
  let console_actions = global_actions @ 
      [
    ]
  in
  List.iter
    (add window ~cond: (fun () -> !current_page = 4) console_actions)
  !!O.keymap_console;

(* End of keyboard shortcuts *)

(* set layout *)
  
  set_hpaned tab_servers#hpaned servers_hpane_left;
  get_hpaned tab_servers#hpaned servers_hpane_left;
  
  gui#notebook#goto_page 1;
  set_hpaned tab_downloads#hpaned downloads_hpane_left;
  get_hpaned tab_downloads#hpaned downloads_hpane_left;  
  set_vpaned tab_downloads#vpaned downloads_vpane_up;
  get_vpaned tab_downloads#vpaned downloads_vpane_up;
  
  gui#notebook#goto_page 2;
  set_hpaned tab_friends#hpaned friends_hpane_left;
  get_hpaned tab_friends#hpaned friends_hpane_left;
  set_vpaned tab_friends#vpaned friends_vpane_up;
  get_vpaned tab_friends#vpaned friends_vpane_up;
  
  gui#notebook#goto_page 3;
  set_hpaned tab_searches#hpaned searches_hpane_left;  
  get_hpaned tab_searches#hpaned searches_hpane_left;  
  gui#notebook#goto_page 0;

  
  save_gui_options ();
  
  reconnect gui; 
  
  let gtk_handler timer =
    reactivate_timer timer;
    while Glib.Main.pending () do
      ignore (Glib.Main.iteration false)
    done;
  in
    
  add_timer 0.1 gtk_handler;
  add_timer 2.0 update_sizes;
  let never_connected = ref true in
  add_timer 1.0 (fun timer ->
      if !never_connected then 
        match !connection_sock with
          None ->
            reactivate_timer timer;
            reconnect gui
        | _ -> 
            never_connected := false
  );

  loop ()
  