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

(** GUI main window. *)

open Printf2
open Gettext
open CommonTypes
open GuiProto

module M = Gui_messages
module O = Gui_options
module Com = Gui_com
module G = Gui_global
module Mi = Gui_misc

let (!!) = Options.(!!)


let is_connected state =
  match state with
  | Connected_initiating
  | Connected_downloading
  | Connected _ -> true
  | NotConnected _
  | Connecting
  | RemovedHost
  | BlackListedHost
  | NewHost-> false

class window () =
  
  object(self)
    inherit Gui_window_base.window ()

    val mutable current_page = 0
      
    method wnote_results = tab_queries#wnote_results

    method current_page = current_page

    method clear =
      tab_servers#clear;
      tab_downloads#clear;
      tab_friends#clear;
      tab_queries#clear;
      tab_rooms#clear;
      tab_graph#clear;
      tab_uploads#clear;
      label_connect_status#set_text (gettext M.not_connected);
      List.iter menu_display#remove menu_display#children;
      List.iter menu_networks#remove menu_networks#children
      
    initializer

      window#show ();
      
      tab_queries#set_wnote_results self#wnote_results;
      tab_queries#set_wnote_main notebook;

      ignore (tab_downloads#box_downloads#ask_clients#connect#clicked
        (fun () ->
          (* Printf.printf "Gui_window Ask_clients clicked\n";
          flush stdout; *)
          let c_num_list = tab_downloads#box_downloads#clients_list in
          let c_list = tab_uploads#retrieve_clients c_num_list in
          tab_downloads#box_downloads#expand_file c_list
        )
      );

      ignore (notebook#connect#switch_page ~callback:
        begin fun p ->
          begin
            (if p = 0 then
              tab_networks#is_visible true
              else tab_networks#is_visible false);
            if p = 2 then
              tab_downloads#is_visible true
              else tab_downloads#is_visible false;
            if p = 3 then
              tab_friends#is_visible true
              else tab_friends#is_visible false;
            if p = 6 then
              tab_uploads#is_visible true
              else tab_uploads#is_visible false;
            notebook#set_page ~tab_label:(Gui_window_base.tab_box p true
                                          Gui_window_base.main_nbk_data)#coerce
                                         (notebook#get_nth_page p)#coerce;
            let page = notebook#current_page in
            notebook#set_page ~tab_label:(Gui_window_base.tab_box page false
                                          Gui_window_base.main_nbk_data)#coerce
                                         (notebook#get_nth_page page)#coerce;
            current_page <- p
          end
        end );

      (* set the size of panes as they were the last time. *)
(*
      Mi.set_hpaned tab_servers#hpaned_servers O.servers_hpane_left;
      Mi.get_hpaned self tab_servers#hpaned_servers O.servers_hpane_left;
*)
        notebook#goto_page 1;
      Mi.set_vpaned tab_servers#vpaned_servers O.servers_vpane_up;
      Mi.get_vpaned self tab_servers#vpaned_servers O.servers_vpane_up;
(*
      notebook#goto_page 2;
      Mi.set_hpaned tab_downloads#hpaned O.downloads_hpane_left;
      Mi.get_hpaned self tab_downloads#hpaned O.downloads_hpane_left;  
      Mi.set_vpaned tab_downloads#vpaned O.downloads_vpane_up;
      Mi.get_vpaned self tab_downloads#vpaned O.downloads_vpane_up;
*)
(*
      Mi.set_vpaned tab_downloads#clients_wpane O.downloads_wpane_up;
      Mi.get_vpaned self tab_downloads#clients_wpane O.downloads_wpane_up;
*)
        notebook#goto_page 3;
      Mi.set_hpaned tab_friends#hpaned O.friends_hpane_left;
      Mi.get_hpaned self tab_friends#hpaned O.friends_hpane_left;
      Mi.set_vpaned tab_friends#vpaned O.friends_vpane_up;
      Mi.get_vpaned self tab_friends#vpaned O.friends_vpane_up;
      Mi.set_hpaned tab_friends#box_files#wpane O.friends_hpane_dirs;
      Mi.get_hpaned self tab_friends#box_files#wpane O.friends_hpane_dirs;

        notebook#goto_page 5;
      Mi.set_hpaned tab_rooms#hpaned O.rooms_hpane_left;
      Mi.get_hpaned self tab_rooms#hpaned O.rooms_hpane_left;
        Mi.set_hpaned tab_rooms#room_pane O.rooms_hpane2_left;
        Mi.get_hpaned self tab_rooms#room_pane O.rooms_hpane2_left;
        Mi.set_vpaned tab_rooms#rooms_pane O.rooms_vpane_up;
        Mi.get_vpaned self tab_rooms#rooms_pane O.rooms_vpane_up;

        notebook#goto_page 6;
        Mi.set_vpaned tab_uploads#vpaned O.uploads_vpane_up;
        Mi.get_vpaned self tab_uploads#vpaned O.uploads_vpane_up;
(*
      notebook#goto_page 3;
      Mi.set_hpaned tab_searches#hpaned O.searches_hpane_left;  
      Mi.get_hpaned self tab_searches#hpaned O.searches_hpane_left;  
*)
      notebook#goto_page 0;


      (* Keyboard shortcuts *)
      let add w ?(cond=(fun () -> true)) l ((mods, k), action) = 
	try
	  let f = List.assoc action l in
	  Okey.add ~cond w ~mods k f
	with
	  Not_found ->
            lprintf "%s\n" (Gui_messages.action_unknown action)
      in

      (* Global shortcuts *)

      let global_actions = [
	M.a_page_servers, itemServers#activate ;
	M.a_page_downloads, itemDownloads#activate;
	M.a_page_friends, itemFriends#activate;
	M.a_page_results, itemResults#activate;
	M.a_page_options, buttonOptions#clicked;
	M.a_page_console, itemConsole#activate;
	M.a_page_help, itemHelp#activate;
	M.a_next_page, notebook#next_page;
	M.a_previous_page, notebook#previous_page;
	M.a_reconnect, itemReconnect#activate;
	M.a_exit, buttonQuit#clicked;
      ] 
      in
      List.iter (add window global_actions) !!O.keymap_global;

      (* Servers shortcuts *)
      let bs = tab_servers#box_servers in
      let servers_actions = global_actions @
	[
	  M.a_connect, bs#connect ;
	  M.a_connect_more, bs#connect_more_servers ;
	  M.a_select_all, bs#wlist#select_all ;
	] 
      in
      List.iter
	(add window ~cond: (fun () -> current_page = 0) servers_actions)
	!!O.keymap_servers;

      (* Downloads shortcuts *)
      let bdls = tab_downloads#box_downloads in
      let downloads_actions = global_actions @
	[
	  M.a_cancel_download, bdls#cancel ;
	  M.a_select_all, bdls#wlist#select_all;
	]
      (*
      let bdled = tab_downloads#box_downloaded in
      let downloads_actions = global_actions @ 
	[
	  M.a_cancel_download, bdls#cancel ;
	  M.a_save_all_files, bdled#save_all ;
	  M.a_menu_save_file, bdled#save ;
	  M.a_select_all, bdls#wlist#select_all;
	]
      *)
      in
      List.iter
	(add window ~cond: (fun () -> current_page = 1) downloads_actions)
	!!O.keymap_downloads;

      (* Friends shortcuts *)
      let bf = tab_friends#box_friends in
      let bfiles = tab_friends#box_files in
      let friends_actions = global_actions @ 
	[
	  M.a_download_selection, bfiles#box_results#download ;
	  M.a_remove_friend, bf#remove ;
	  M.a_select_all, bf#wlist#select_all ;
	]
      in
      List.iter
	(add window ~cond: (fun () -> current_page = 2) friends_actions)
	!!O.keymap_friends;

      hbox_status#pack ~expand: true tab_servers#wl_status#coerce;
      hbox_status#pack ~expand: true tab_downloads#wl_status#coerce;
      hbox_status#pack ~expand: true tab_uploads#wl_status#coerce;

  end
