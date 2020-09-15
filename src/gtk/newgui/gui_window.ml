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

open Gui_global
  
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
  | ServerFull
  | Connecting
  | RemovedHost
  | BlackListedHost
  | NewHost-> false


class window () =
  
  object(self)
    inherit Gui_window_base.window ()

    val mutable current_page = G.networks_page
      
    method wnote_results = tab_queries#wnote_results

    method current_page = current_page

    method set_connect_status s =
      label_connect_status#set_text s
      
    method clear =
      tab_networks#clear;
      tab_servers#clear;
      tab_downloads#clear;
      tab_friends#clear;
      tab_queries#clear;
      tab_rooms#clear;
      tab_graph#clear;
      tab_uploads#clear;
      label_connect_status#set_text (M.mW_lb_not_connected)

    initializer

      window#show ();
      
(*      tab_queries#set_wnote_results self#wnote_results; *)
      tab_queries#set_wnote_main notebook;

      ignore (notebook#connect#switch_page ~callback:
        begin fun p ->
            begin

              displayed_page := p;

            (if p = networks_page then
              tab_networks#is_visible true
              else tab_networks#is_visible false);
            if p = downloads_page then
              tab_downloads#is_visible true
              else tab_downloads#is_visible false;
            if p = friends_page then
              tab_friends#is_visible true
              else tab_friends#is_visible false;
            if p = uploads_page then
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

      notebook#goto_page servers_page;
      Mi.set_vpaned tab_servers#vpaned_servers O.servers_vpane_up;
      Mi.get_vpaned self tab_servers#vpaned_servers O.servers_vpane_up;

      notebook#goto_page friends_page;
      Mi.set_hpaned tab_friends#hpaned O.friends_hpane_left;
      Mi.get_hpaned self tab_friends#hpaned O.friends_hpane_left;
      Mi.set_vpaned tab_friends#vpaned O.friends_vpane_up;
      Mi.get_vpaned self tab_friends#vpaned O.friends_vpane_up;
      Mi.set_hpaned tab_friends#box_files#wpane O.friends_hpane_dirs;
      Mi.get_hpaned self tab_friends#box_files#wpane O.friends_hpane_dirs;

      notebook#goto_page rooms_page;
      Mi.set_hpaned tab_rooms#hpaned O.rooms_hpane_left;
      Mi.get_hpaned self tab_rooms#hpaned O.rooms_hpane_left;
      Mi.set_hpaned tab_rooms#room_pane O.rooms_hpane2_left;
      Mi.get_hpaned self tab_rooms#room_pane O.rooms_hpane2_left;
      Mi.set_vpaned tab_rooms#rooms_pane O.rooms_vpane_up;
      Mi.get_vpaned self tab_rooms#rooms_pane O.rooms_vpane_up;

      notebook#goto_page uploads_page;
      Mi.set_vpaned tab_uploads#vpaned O.uploads_vpane_up;
      Mi.get_vpaned self tab_uploads#vpaned O.uploads_vpane_up;

      (* Keyboard shortcuts *)
      let add w ?(cond=(fun () -> true)) l ((mods, k), action) = 
        try
          let f = List.assoc action l in
          Okey.add ~cond w ~mods k f
        with
          Not_found ->
            lprintf "%s %s\n" (M.mW_tx_action_unknown) action
      in

      (* Global shortcuts *)
      
      let global_actions = [
        M.a_page_networks, (fun () -> notebook#goto_page 0);
        M.a_page_servers, (fun () -> notebook#goto_page 1);
        M.a_page_downloads, (fun () -> notebook#goto_page 2);
        M.a_page_friends, (fun () -> notebook#goto_page 3);
        M.a_page_searches, (fun () -> notebook#goto_page 4);
        M.a_page_rooms, (fun () -> notebook#goto_page 5);
        M.a_page_uploads, (fun () -> notebook#goto_page 6);
        M.a_page_console, (fun () -> notebook#goto_page 7);
        M.a_page_graph, (fun () -> notebook#goto_page 8);
        M.a_next_page, notebook#next_page;
        M.a_previous_page, notebook#previous_page;
        M.a_reconnect, buttonGui#clicked;
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
        (add window ~cond: (fun () -> 
            current_page = servers_page) servers_actions)
        !!O.keymap_servers;

      (* Downloads shortcuts *)
      let bdls = tab_downloads#box_downloads in
      let downloads_actions = global_actions @
        [
          M.a_cancel_download, bdls#cancel ;
          M.a_select_all, bdls#wlist#select_all;
          M.a_save_all_files, bdls#save_all ;
        ]
      in
      List.iter
        (add window ~cond: (fun () -> 
            current_page = downloads_page) downloads_actions)
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
        (add window ~cond: (fun () -> 
            current_page = friends_page) friends_actions)
        !!O.keymap_friends;

      hbox_status#pack ~expand: true tab_servers#wl_status#coerce;
      hbox_status#pack ~expand: true tab_downloads#wl_status#coerce;
      hbox_status#pack ~expand: true tab_uploads#wl_status#coerce;

  end
