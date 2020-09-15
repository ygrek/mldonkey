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
  | Connected_downloading _
  | Connected _ -> true
  | NotConnected _
  | Connecting
  | ServerFull
  | RemovedHost
  | BlackListedHost
  | NewHost-> false



class window () =
  
  object(self)
    inherit Gui_window_base.window ()

    val mutable current_page = 0
      
    method clear =
      tab_servers#clear;
      tab_downloads#clear;
      tab_friends#clear;
      List.iter wnote_results#remove wnote_results#children;
      tab_rooms#clear;
      label_connect_status#set_text (gettext M.not_connected);
      List.iter menu_display#remove menu_display#children;
      List.iter menu_networks#remove menu_networks#children;

    method tab_queries = tab_servers#tab_queries
      
    initializer

      window#show ();
      tab_servers#tab_queries#set_wnote_results self#wnote_results;
      tab_servers#tab_queries#set_wnote_main notebook;
      
      ignore (notebook#connect#switch_page 
                (fun n -> current_page <- n));

      (* set the size of panes as they were the last time. *)
      Mi.set_hpaned tab_servers#hpaned_servers O.servers_hpane_left;
      Mi.get_hpaned self tab_servers#hpaned_servers O.servers_hpane_left;
      Mi.set_vpaned tab_servers#vpaned_servers O.servers_vpane_up;
      Mi.get_vpaned self tab_servers#vpaned_servers O.servers_vpane_up;

      tab_servers#box_servers#wlist#columns_autosize();
      tab_servers#box_users#wlist#columns_autosize();

      notebook#goto_page 1;
      Mi.set_hpaned tab_downloads#hpaned O.downloads_hpane_left;
      Mi.get_hpaned self tab_downloads#hpaned O.downloads_hpane_left;  
      Mi.set_vpaned tab_downloads#vpaned O.downloads_vpane_up;
      Mi.get_vpaned self tab_downloads#vpaned O.downloads_vpane_up;
      Mi.set_vpaned tab_downloads#clients_wpane O.downloads_wpane_up;
      Mi.get_vpaned self tab_downloads#clients_wpane O.downloads_wpane_up;

      tab_downloads#box_downloads#wlist#columns_autosize();
      tab_downloads#box_downloaded#wlist#columns_autosize();
      tab_downloads#box_locations#wlist#columns_autosize();

      notebook#goto_page 2;
      Mi.set_hpaned tab_friends#hpaned O.friends_hpane_left;
      Mi.get_hpaned self tab_friends#hpaned O.friends_hpane_left;

      Mi.set_vpaned tab_friends#vpaned O.friends_vpane_up;
      Mi.get_vpaned self tab_friends#vpaned O.friends_vpane_up;

      Mi.set_hpaned tab_friends#box_files#wpane O.friends_hpane_dirs;
      Mi.get_hpaned self tab_friends#box_files#wpane O.friends_hpane_dirs;

      tab_friends#box_friends#wlist#columns_autosize();
      tab_friends#box_files#box_results#wlist#columns_autosize();

      notebook#goto_page 4;
      Mi.set_hpaned tab_rooms#hpaned O.rooms_hpane_left;
      Mi.get_hpaned self tab_rooms#hpaned O.rooms_hpane_left;

      notebook#goto_page 5;
      tab_uploads#upstats_box#wlist#columns_autosize();
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
        M.a_page_options, itemOptions#activate;
        M.a_page_console, itemConsole#activate;
        M.a_page_help, itemHelp#activate;
        M.a_next_page, notebook#next_page;
        M.a_previous_page, notebook#previous_page;
        M.a_reconnect, itemReconnect#activate;
        M.a_exit, itemQuit#activate;
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
      let bdled = tab_downloads#box_downloaded in
      let downloads_actions = global_actions @ 
        [
          M.a_cancel_download, bdls#cancel ;
          M.a_save_all_files, bdled#save_all ;
          M.a_menu_save_file, bdled#save ;
          M.a_select_all, bdls#wlist#select_all;
        ]
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
