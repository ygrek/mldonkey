(* Copyright 2004 b8_bavard, INRIA *)
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

open Options
open GuiTypes2
open CommonTypes
open GraphTypes

open GMain
open GtkBase
open Gdk


module O = GuiOptions
module M = GuiMessages
module G = GuiGlobal
module Mi = GuiMisc
module A = GuiArt
module U = GuiUtf8


let verbose = O.gtk_verbose_main

let lprintf' fmt =
  Printf2.lprintf ("GuiMain: " ^^ fmt)

(*************************************************************************)
(*                                                                       *)
(*                         chmod_config                                  *)
(*                                                                       *)
(*************************************************************************)

let chmod_config () =
  let base_config = 
    (Filename.concat M.gui_config_dir "mlgui.ini")
  in
  let save_config =
    base_config ^ ".old"
  in
  begin
    if Sys.file_exists base_config then
      Printexc2.catch2 "Unix.chmod" Unix.chmod base_config 0o600 
    else
      ()
  end;
  begin
    if Sys.file_exists save_config then
      Printexc2.catch2 "Unix.chmod" Unix.chmod save_config 0o600
    else
      ()
  end

(* {2 Handling core messages} *)

open CommonTypes
open GuiTypes
open GuiProto
  

(*************************************************************************)
(*                                                                       *)
(*                         value_reader                                  *)
(*                                                                       *)
(*************************************************************************)

let value_reader gui t =
  try

     begin
        if !!verbose then lprintf' "MESSAGE RECEIVED: %s\n" 
          (string_of_to_gui t);
      end;

    match t with

      (* The first message received from the core *)
    | CoreProtocol (v, _, _) -> 
        let version = min v GuiProto.best_gui_version in
        for i = 0 to to_gui_last_opcode do
          GuiCom.to_gui_protocol_used.(i) <- version;
        done;
        for i = 0 to from_gui_last_opcode do
          GuiCom.from_gui_protocol_used.(i) <- version;
        done;
        ( if !!verbose then lprintf' "Using protocol %d for communications\n" version);
        gui.set_corestatus Core_Connected;
        if version >= G.interested_in_sources_version
          then begin
            ( if !!verbose then lprintf' "-----  not interested in sources -----\n");
            G.use_interested_in_sources := true;
            GuiCom.send (InterestedInSources false);
            end;
        GuiCom.send (Password (!!O.gtk_client_login, !!O.gtk_client_password))

    | Console text ->
        begin
          (if gui.init.console
             then begin
               gui.set_splash_screen M.icon_menu_console (Mi.remove_ !M.mW_lb_console);
               gui.init.console <- false
             end
          );
          GuiConsole.insert text
        end

    | Network_info n ->
        begin
          (if gui.init.networks
             then begin
               gui.set_splash_screen M.icon_menu_networks (Mi.remove_ !M.mW_lb_networks);
               gui.init.networks <- false
             end
          );
          GuiNetworks.update_network n
        end

    | Client_stats s ->
        begin
          GuiStatusBar.update_updown s.tcp_upload_rate s.tcp_download_rate;
          GuiStatusBar.update_sharedfiles s.nshared_files s.upload_counter;
          GuiGraphBase.save_record s.tcp_download_rate GraphDownloads;
          GuiGraphBase.save_record s.tcp_upload_rate GraphUploads;
          if !G.is_docked
            then begin
              let up = float_of_int s.tcp_upload_rate /. 1024. in
              let down = float_of_int s.tcp_download_rate /. 1024. in
              let t = Printf.sprintf "MLDonkey %s\n%s %5.1f ko/s\n%s %5.1f ko/s"
                Autoconf.current_version !M.mW_lb_updload up !M.mW_lb_download down
              in
              G.tray.set_tooltip_tray t
            end
        end

    | Search_result (query_num, result_num, _) ->
        GuiQueries.h_search_result query_num result_num

    | Search_waiting (query_num, waiting) ->
        GuiQueries.h_search_waiting query_num waiting

    | File_add_source (file_num, source_num) ->
        begin
          (if gui.init.downloads
             then begin
               gui.set_splash_screen M.icon_menu_downloads (Mi.remove_ !M.mW_lb_downloads);
               gui.init.downloads <- false
             end
          );
          try
            let s = Hashtbl.find G.sources source_num in
            let files_requested =
              if not (List.mem file_num s.source_files_requested)
                then file_num :: s.source_files_requested
                else s.source_files_requested
            in
            let s_new = {s with source_files_requested = files_requested} in
            GuiDownloads.h_add_source s_new file_num;(* seen *)
            s.source_files_requested <- s_new.source_files_requested;(* seen *)
          with _ -> 
            GuiCom.send (GuiProto.GetClient_info source_num)
        end

    | File_remove_source (file_num, source_num) ->
        begin
          try
            let s = Hashtbl.find G.sources source_num in
            let files_requested = List.filter (fun num -> num <> file_num) s.source_files_requested in
            let s_new = {s with source_files_requested = files_requested} in
            GuiDownloads.h_remove_source s_new file_num;(* seen *)
            s.source_files_requested <- s_new.source_files_requested;(* seen *)
          with _ -> 
            GuiCom.send (GuiProto.GetClient_info source_num)
        end

    | File_downloaded (num, downloaded, rate, last_seen) ->
        GuiDownloads.file_downloaded (num, downloaded, rate, (BasicSocket.last_time () - last_seen))

    | File_update_availability (file_num, source_num, avail) ->
        begin
          (* no more availability sent by Bittorrent, Fasttrack & eDonkey ? *)
          (if !!verbose then lprintf' "update_availability  %d/%d %s\n" file_num source_num avail);
          try
            let s = Hashtbl.find G.sources source_num in
            let s_new =
              try
                let avails = List.remove_assoc file_num s.source_availability in
                {s with source_availability = (file_num, avail) :: avails}
              with _ -> {s with source_availability = (file_num, avail) :: s.source_availability}
            in
            GuiDownloads.h_update_source_availability s s_new file_num;(* seen *)
            s.source_availability <- s_new.source_availability(* seen *)
          with _ -> 
            GuiCom.send (GuiProto.GetClient_info source_num)
        end

    | File_info f ->
        begin
          (if gui.init.downloads
             then begin
               gui.set_splash_screen M.icon_menu_downloads (Mi.remove_ !M.mW_lb_downloads);
               gui.init.downloads <- false
             end
          );
          let file = Mi.file_info_to_g_file_info f in
          GuiDownloads.file_info file
        end

    | Server_info s ->
        begin
          (if gui.init.servers
             then begin
               gui.set_splash_screen M.icon_menu_servers (Mi.remove_ !M.mW_lb_servers);
               gui.init.servers <- false
             end
          );
          GuiServers.server_info s
        end

    | Server_state (num, state) ->
        GuiServers.h_server_update_state num state

    | Server_busy (num, nusers, nfiles) ->
        GuiServers.h_server_busy num nusers nfiles

    | Server_user (num, user) ->
        begin
          ( if !!verbose then lprintf' "Server_user (%d, %d)\n" num user);
          GuiServers.h_server_update_users num user
        end

    | Room_info room ->
        begin
          (if gui.init.rooms
             then begin
               gui.set_splash_screen M.icon_menu_rooms (Mi.remove_ !M.mW_lb_rooms);
               gui.init.rooms <- false
             end
          );
          GuiRooms.room_info room
        end

    | User_info user ->
        begin
          let user =
            try 
              let u = Hashtbl.find G.users user.user_num  in
              u.user_tags <- user.user_tags;
              u
            with Not_found ->
              Hashtbl.add G.users user.user_num user; 
              user
          in
          ( if !!verbose then lprintf' "user_info %s/%d\n" user.user_name user.user_server);
          GuiServers.h_server_update_users user.user_server user.user_num;
          GuiRooms.update_user_info user
        end

    | Room_add_user (num, user_num) ->
        GuiRooms.add_room_user num user_num

    | Room_remove_user (num, user_num) ->
        GuiRooms.remove_room_user num user_num

    | Options_info options_list ->
        GuiConfig.update_options options_list

    | Add_section_option (section, o) ->
        begin
          (if gui.init.settings
             then begin
               gui.set_splash_screen M.icon_menu_settings (Mi.remove_ !M.mW_lb_settings);
               gui.init.settings <- false
             end
          );
          GuiConfig.add_option (section, o)
        end

    | Add_plugin_option (section, o) ->
        begin
          (if gui.init.settings
             then begin
               gui.set_splash_screen M.icon_menu_settings (Mi.remove_ !M.mW_lb_settings);
               gui.init.settings <- false
             end
          );
          GuiConfig.add_option (section, o)
        end

    | DefineSearches l ->
        begin
          (if gui.init.queries
             then begin
               gui.set_splash_screen M.icon_menu_searches (Mi.remove_ !M.mW_lb_search);
               gui.init.queries <- false
             end
          );
          GuiQueries.h_define_searches l
        end

    | Client_state (source_num, state) ->
        begin
          try
            let s = Hashtbl.find G.sources source_num in
            let s_new = {s with source_state = state} in
            GuiDownloads.h_update_source s s_new;(* seen *)
            GuiUploads.h_update_uploader s s_new;(* seen *)
            GuiFriends.h_update_friend s s_new;(* seen *)
            s.source_state <- state(* seen *)
          with _ ->
            begin
              GuiCom.send (GuiProto.GetClient_info source_num)
            end
        end

    | Client_friend (source_num, friend_kind) ->
        begin
          try
            let s = Hashtbl.find G.sources source_num in
            let s_new = {s with source_type = friend_kind} in
            GuiDownloads.h_update_source s s_new;(* seen *)
            GuiUploads.h_update_uploader s s_new;(* seen *)
            GuiFriends.h_update_friend s s_new;(* seen *)
            s.source_type <- friend_kind;(* seen *)
          with _ ->
            begin
              GuiCom.send (GuiProto.GetClient_info source_num)
            end
        end

    | Result_info r ->
        GuiResults.result_info r

    | Client_file (source_num , dirname, file_num) ->
        GuiFriends.add_friend_files (source_num , dirname, file_num)(* seen *)

    | Client_info client ->
        begin
          (if gui.init.friends
             then begin
               gui.set_splash_screen M.icon_menu_friends (Mi.remove_ !M.mW_lb_friends);
               gui.init.friends <- false
             end
          );
          let source = Mi.client_to_source client in
          try
            let s = Hashtbl.find G.sources source.source_num in
            let s_new =
              { source with
                source_files = s.source_files;
                source_has_upload = s.source_has_upload;
                source_availability = s.source_availability;
                source_files_requested = s.source_files_requested;
                source_upload_rate = Mi.source_upload_rate source s;
                source_download_rate = Mi.source_download_rate source s;
              }
            in
            GuiDownloads.h_update_source s s_new;(* seen *)
            GuiUploads.h_update_uploader s s_new;(* seen *)
            GuiFriends.h_update_friend s s_new;(* seen *)
            G.hashtbl_update_sources s s_new;(* seen *)
          with _ ->
            begin
              GuiFriends.h_update_friend source source;(* seen *)
              Hashtbl.add G.sources source.source_num source(* seen *)
            end
        end

    | Room_message (_, PrivateMessage(num, mes) )
    | Room_message (0, PublicMessage(num, mes) )
    | MessageFromClient (num, mes) ->
        begin
          try
            GuiFriends.add_message_from_client num mes 
          with
            Not_found ->
              try
                match t with
                  Room_message (num, msg) ->
                    GuiRooms.add_room_message num msg
                | _ -> raise Not_found
              with Not_found ->
                  if !!verbose then lprintf' "Client %d not found in reader.MessageFromClient\n" num
        end

    | Room_message (num, msg) ->
        GuiRooms.add_room_message num msg

    | (DownloadedFiles _|DownloadFiles _|ConnectedServers _) -> assert false

    | CleanTables (clients, servers) ->
        begin
          let all_sources = Hashtbl2.to_list G.sources in
          (if !!verbose then lprintf' "Cleaning sources\n   sources table : %d\n   new sources   : %d\n"
              (List.length all_sources) (List.length clients));
          List.iter (fun s ->
            if not (List.mem s.source_num clients)
              then begin
                GuiDownloads.clean_sources_table s;
                GuiFriends.clean_friends_table s;
                GuiUploads.clean_uploaders_table s;
                Hashtbl.remove G.sources s.source_num
              end
          ) all_sources;
          if !!verbose then
            begin
              let l = Hashtbl2.to_list G.sources in
              lprintf' "   ----------------------------\n   sources table : %d\n" (List.length l)
            end;
          GuiServers.clean_servers_table servers;
        end

    | Shared_file_info s ->
        begin
          (if gui.init.uploads
             then begin
               gui.set_splash_screen M.icon_menu_uploads (Mi.remove_ !M.mW_lb_uploads);
               gui.init.uploads <- false
             end
          );
          GuiUploads.h_shared_file_info (Mi.shared_info_to_shared_file s)
        end

    | Shared_file_upload (num, size, requests) ->
        GuiUploads.h_shared_file_upload num size requests

    | Shared_file_unshared num ->
        GuiUploads.h_shared_file_unshared num

    | BadPassword ->
        GToolbox.message_box 
          ~title:(!M.pW_wt_bad_password)
          (!M.pW_lb_bad_password)

    | Uploaders l ->
        GuiUploads.h_update_uploaders l

    | Pending l ->
        GuiUploads.h_update_pending_slots l

    | GiftServerAttach _
    | GiftServerStats _ -> assert false
    | Search s -> ()
    | Version v -> ()
    | Stats (_, _) -> ()
        
  with e ->
      Printf2.lprintf "Exception %s in reader\n" (Printexc2.to_string e)

(*************************************************************************)
(*                                                                       *)
(*                         core_menu                                     *)
(*                                                                       *)
(*************************************************************************)

let generate_connect_menu gui menu =
  let add_item hostname port =
    let menu_item =
      let label = U.utf8_of (Printf.sprintf "%s:%d" hostname port) in
      GMenu.menu_item ~label
      ~packing:menu#add ()
    in
    ignore (menu_item#connect#activate ~callback:(fun _ ->
          O.gtk_client_hostname =:= hostname;
          O.gtk_client_port =:= port;
          GuiCom.reconnect gui value_reader gui BasicSocket.Closed_by_user
      ));
  in
  List.iter (fun child -> child#destroy ()) menu#children;
  List.iter (fun (h,port) -> add_item h port) !!O.gtk_client_history;
  List.iter (fun (h,port) -> add_item h port) !G.scanned_ports

let core_menu gui on_quit =
  let menu = GMenu.menu () in
  let accel_menu = GtkData.AccelGroup.create () in
  let _ = gui.window#add_accel_group accel_menu in
  let _ = menu#set_accel_group accel_menu in
  if not !CommonGlobals.core_included
    then begin
      let reconnect =
        GMenu.image_menu_item ~label:!M.mW_me_reconnect ~use_mnemonic:true
          ~image:(GMisc.image ~pixbuf:(A.get_icon ~icon:M.icon_menu_core_reconnect ~size:A.SMALL ()) ())
          ~packing:menu#add ()
      in
      let disconnect =
        GMenu.image_menu_item ~label:!M.mW_me_disconnect ~use_mnemonic:true
          ~image:(GMisc.image ~pixbuf:(A.get_icon ~icon:M.icon_menu_core_disconnect ~size:A.SMALL ()) ())
          ~packing:menu#add ()
      in
      let scanports =
        GMenu.image_menu_item ~label:!M.mW_me_scan_ports ~use_mnemonic:true
          ~image:(GMisc.image ~pixbuf:(A.get_icon ~icon:M.icon_menu_core_scanports ~size:A.SMALL ()) ())
          ~packing:menu#add ()
      in
      let reconnect_to =
        GMenu.image_menu_item ~label:!M.mW_me_reconnect_to ~use_mnemonic:true
          ~image:(GMisc.image ~pixbuf:(A.get_icon ~icon:M.icon_menu_core_connectto ~size:A.SMALL ()) ())
          ~packing:menu#add ()
      in
      let kill_core =
        GMenu.image_menu_item ~label:!M.mW_me_kill_core ~use_mnemonic:true
          ~image:(GMisc.image ~pixbuf:(A.get_icon ~icon:M.icon_menu_core_kill ~size:A.SMALL ()) ())
          ~packing:menu#add ()
      in
      let reconnect_to_menu = GMenu.menu ~packing:(reconnect_to#set_submenu) () in
      ignore (reconnect_to#connect#activate ~callback:
        (fun _ ->
           generate_connect_menu gui reconnect_to_menu
      ));
      ignore (GMenu.separator_item ~packing:menu#add ());
      ignore (reconnect#connect#activate 
        (fun _ -> GuiCom.reconnect gui value_reader gui BasicSocket.Closed_by_user
      ));
      ignore (disconnect#connect#activate
        (fun _ -> (try GuiCom.disconnect gui BasicSocket.Closed_by_user with _ -> ())
      ));
      ignore (scanports#connect#activate
        (fun _ -> GuiCom.scan_ports ()
      ));
      ignore (kill_core#connect#activate
        (fun _ -> GuiCom.send KillServer
      ));
      reconnect#add_accelerator ~group:accel_menu
        ~modi:[`CONTROL] ~flags:[`VISIBLE] GdkKeysyms._r;
      disconnect#add_accelerator ~group:accel_menu
        ~modi:[`CONTROL] ~flags:[`VISIBLE] GdkKeysyms._d;
      kill_core#add_accelerator ~group:accel_menu
        ~modi:[`CONTROL] ~flags:[`VISIBLE] GdkKeysyms._k;
    end;
  let settings =
    GMenu.image_menu_item ~label:!M.mW_me_settings ~use_mnemonic:true
      ~image:(GMisc.image ~pixbuf:(A.get_icon ~icon:M.icon_menu_settings ~size:A.SMALL ()) ())
      ~packing:menu#add ()
  in
  let quit =
    GMenu.image_menu_item ~label:!M.mW_me_quit ~use_mnemonic:true
      ~image:(GMisc.image ~pixbuf:(A.get_icon ~icon:M.icon_menu_quit ~size:A.SMALL ()) ())
      ~packing:menu#add ()
  in
  ignore (settings#connect#activate 
    (fun _ -> GuiWindow.display_settings gui value_reader ()
  ));
  ignore (quit#connect#activate 
    (fun _ -> on_quit ()
  ));
  settings#add_accelerator ~group:accel_menu
    ~modi:[`CONTROL] ~flags:[`VISIBLE] GdkKeysyms._o;
  quit#add_accelerator ~group:accel_menu
    ~modi:[`CONTROL] ~flags:[`VISIBLE] GdkKeysyms._w;
  menu

(*************************************************************************)
(*                                                                       *)
(*                         tray_menu                                     *)
(*                                                                       *)
(*************************************************************************)

let tray_menu gui on_quit =
  let menu = GMenu.menu () in
  if not !CommonGlobals.core_included
    then begin
      let reconnect =
        GMenu.image_menu_item ~label:!M.mW_me_reconnect ~use_mnemonic:true
          ~image:(GMisc.image ~pixbuf:(A.get_icon ~icon:M.icon_menu_core_reconnect ~size:A.SMALL ()) ())
          ~packing:menu#add ()
      in
      let disconnect =
        GMenu.image_menu_item ~label:!M.mW_me_disconnect ~use_mnemonic:true
          ~image:(GMisc.image ~pixbuf:(A.get_icon ~icon:M.icon_menu_core_disconnect ~size:A.SMALL ()) ())
          ~packing:menu#add ()
      in
      let scanports =
        GMenu.image_menu_item ~label:!M.mW_me_scan_ports ~use_mnemonic:true
          ~image:(GMisc.image ~pixbuf:(A.get_icon ~icon:M.icon_menu_core_scanports ~size:A.SMALL ()) ())
          ~packing:menu#add ()
      in
      let reconnect_to =
        GMenu.image_menu_item ~label:!M.mW_me_reconnect_to ~use_mnemonic:true
          ~image:(GMisc.image ~pixbuf:(A.get_icon ~icon:M.icon_menu_core_connectto ~size:A.SMALL ()) ())
          ~packing:menu#add ()
      in
      let kill_core =
        GMenu.image_menu_item ~label:!M.mW_me_kill_core ~use_mnemonic:true
          ~image:(GMisc.image ~pixbuf:(A.get_icon ~icon:M.icon_menu_core_kill ~size:A.SMALL ()) ())
          ~packing:menu#add ()
      in
      let reconnect_to_menu = GMenu.menu ~packing:(reconnect_to#set_submenu) () in
      generate_connect_menu gui reconnect_to_menu;
      ignore (GMenu.separator_item ~packing:menu#add ());
      ignore (reconnect#connect#activate 
        (fun _ -> GuiCom.reconnect gui value_reader gui BasicSocket.Closed_by_user
      ));
      ignore (disconnect#connect#activate 
        (fun _ -> (try GuiCom.disconnect gui BasicSocket.Closed_by_user with _ -> ())
      ));
      ignore (scanports#connect#activate
        (fun _ -> GuiCom.scan_ports ()
      ));
      ignore (kill_core#connect#activate
        (fun _ -> GuiCom.send KillServer
      ));
    end;
  let restore =
    GMenu.image_menu_item ~label:!M.mW_me_restore ~use_mnemonic:true
      ~image:(GMisc.image ~pixbuf:(A.get_icon ~icon:M.icon_menu_interfaces ~size:A.SMALL ()) ())
      ~packing:menu#add ()
  in
  let settings =
    GMenu.image_menu_item ~label:!M.mW_me_settings ~use_mnemonic:true
      ~image:(GMisc.image ~pixbuf:(A.get_icon ~icon:M.icon_menu_settings ~size:A.SMALL ()) ())
      ~packing:menu#add ()
  in
  let quit =
    GMenu.image_menu_item ~label:!M.mW_me_quit ~use_mnemonic:true
      ~image:(GMisc.image ~pixbuf:(A.get_icon ~icon:M.icon_menu_quit ~size:A.SMALL ()) ())
      ~packing:menu#add ()
  in
  ignore (restore#connect#activate 
    (fun _ ->
       gui.window#show ();
       G.tray.destroy_tray ();
  ));
  ignore (settings#connect#activate 
    (fun _ -> GuiWindow.display_settings gui value_reader ()
  ));
  ignore (quit#connect#activate 
    (fun _ -> on_quit ()
  ));
  menu

(*************************************************************************)
(*                                                                       *)
(*                         splash_screen_window                          *)
(*                                                                       *)
(*************************************************************************)

let spercent p =
  let v = int_of_float (p *. 100.) in
  U.simple_utf8_of (Printf.sprintf "%d%%" v)

let set_progress p pbar =
  let p = 
    if p >= 1. 
      then 1. 
      else p
  in
  pbar#set_fraction p;
  pbar#set_text (spercent p)

let splash_timerID = ref (GMain.Timeout.add ~ms:2000 ~callback:(fun _ -> true))

let splash_screen_window gui =
  let scrwidth = Gdk.Screen.width () in
  let _colormap = Gdk.Color.get_system_colormap () in
  let _size = scrwidth / 3 in
  let splash_screen_pb = A.get_icon ~icon:M.icon_splash_screen ~size:A.LARGE () in
  let width = GdkPixbuf.get_width splash_screen_pb in
  let height = GdkPixbuf.get_height splash_screen_pb in
  let (splash_screen_pix , _ ) = GdkPixbuf.create_pixmap splash_screen_pb in
  let wpix = `PIXMAP splash_screen_pix in
  let window =
    GWindow.window ~position:`CENTER_ALWAYS ~kind:`POPUP
      ~width ~height ()
  in
  Widget.realize window#as_widget ;
  window#misc#set_app_paintable true;
  Window.set_back_pixmap window#misc#window wpix;
(*
  ignore (window#event#add [`BUTTON_PRESS]);
*)
  let vbox =
    GPack.vbox ~homogeneous:false
      ~packing:(window#add) ()
  in
  let hbox =
    GPack.hbox ~homogeneous:false ~spacing:10 ~border_width:18
      ~packing:(vbox#pack ~fill:true ~expand:true) ()
  in

  let img =
    GMisc.image ~xalign:1. ~yalign:0.5 ~width:(width / 2)
      ~packing:(hbox#pack ~fill:true ~expand:false) ()
  in
  let status =
    GMisc.label ~xalign:0. ~yalign:0.5
      ~packing:(hbox#pack ~fill:true ~expand:false) ()
  in
  status#set_use_markup true;

  let _label =
    GMisc.label ~text:(Printf.sprintf "v. %s" Autoconf.current_version)
      ~xpad:10 ~ypad:10 ~xalign:0. ~yalign:1.
      ~packing:(vbox#pack ~fill:true ~expand:false) ()
  in

  let step = ref 0 in
  let pbar =
    GRange.progress_bar ~pulse_step:0.1
      ~packing:(vbox#pack ~fill:true ~expand:false) ()
  in
  set_progress 0. pbar;
(*
  ignore (window#event#connect#button_press
    ~callback:(fun e ->
      GdkEvent.get_type e = `BUTTON_PRESS &&
      (
         window#destroy ();
        true
      )
  ));
*)
  ignore (window#connect#destroy ~callback:
    (fun _ ->
       Timeout.remove (!splash_timerID);
       gui.init.networks <- false;
       gui.init.servers <- false;
       gui.init.downloads <- false;
       gui.init.friends <- false;
       gui.init.queries <- false;
       gui.init.rooms <- false;
       gui.init.uploads <- false;
       gui.init.settings <- false;
       gui.init.console <- false;
       gui.set_splash_screen <- (fun _ _ -> ());
       gui.switch_to_page gui.current_page;
       gui.window#show ()
  ));
  let countdown = ref 0 in
  splash_timerID := Timeout.add ~ms:500 ~callback:
    (fun _ ->
       if !countdown = 10 then window#destroy ();
       incr countdown;
       true
  );
  gui.set_splash_screen <-
   (fun icon text ->
      incr step;
      let pixbuf = A.get_icon ~icon ~size:A.LARGE () in
      img#set_pixbuf pixbuf;
      let markup = GuiTools.create_bold_markup
        (Printf.sprintf "%s %s" !M.mW_lb_initializing text) in
      status#set_label markup;
      (if !!verbose then lprintf' "Initializing %s\n" text);
      Timeout.remove (!splash_timerID);
      let count = ref 0 in
      splash_timerID := Timeout.add ~ms:700 ~callback:
        (fun _ ->
           (if !!verbose then lprintf' "Splash_timerID %d %d\n" !step !count);
           set_progress (float_of_int !step /. 9.) pbar;
           GMain.quit ();
           if !count = 10 then set_progress 1. pbar;
           if !step = 9 || !count = 11 then window#destroy ();
           incr count;
           true
      );
      GMain.main ();

  );
  window#show ()


(*************************************************************************)
(*                                                                       *)
(*                         main                                          *)
(*                                                                       *)
(*************************************************************************)

let main () =
  let gui = GuiWindow.window () in
  let w = gui.window in
  let locale = w#misc#pango_context#language in
  let list_of_locale = Array.of_list (String2.split locale '-') in
  let language = String.uppercase list_of_locale.(0) in
  let lang = U.string_to_lang language in
  U.set_default_codeset_list lang;
  G.get_metrics_from_gtk_font_list ();

  let quit () = 
    CommonGlobals.exit_properly 0
  in
  chmod_config ();

  splash_screen_window gui;

  ignore (w#event#connect#delete ~callback:
    (fun _ ->
       if Autoconf.system = "windows"
         then begin
           w#misc#hide ();
           let icon = A.get_icon ~icon:M.icon_type_source_normal ~size:A.MEDIUM () in
           G.tray.create_tray icon "MLDonkey";
         end else begin
           quit ()
         end;
         true
  ));

  let main_menu = core_menu gui quit in
  GuiStatusBar.menuitem#set_submenu (main_menu);

  G.console_message := (fun s -> GuiConsole.insert s);
  if Autoconf.system <> "windows"
    then begin
      let icon = A.get_icon ~icon:M.icon_type_source_normal ~size:A.MEDIUM () in
      G.tray.create_tray icon "MLDonkey";
    end;
  let win_hidden = ref false in
  !G.set_systray_callback (fun ev ->
    match ev with
        DOUBLE_CLICKED ->
          if Autoconf.system = "windows"
            then begin
             (if !!verbose then lprintf' "tray double clicked\n");
             G.tray.destroy_tray ();
             w#misc#show ();
             w#maximize ();
            end
      | RBUTTON_CLICKED ->
           begin
             (if !!verbose then lprintf' "tray right clicked\n");
             let menu = tray_menu gui quit in
             let w_opt = GWindow.toplevel menu in
             match w_opt with
                 None -> (if !!verbose then lprintf' "No toplevel window\n"; flush stdout)
               | Some win ->
                   begin
                     win#set_position `MOUSE;
                     win#show ()
                   end
           end
      | LBUTTON_CLICKED ->
          if Autoconf.system <> "windows"
            then begin
             (if !!verbose then lprintf' "tray left clicked\n");
             if !win_hidden
               then begin
                 w#misc#show ();
                 win_hidden := false
               end else begin
                 w#misc#hide ();
                 win_hidden := true
               end
           end
  );

  CommonGlobals.do_at_exit (fun _ ->
    chmod_config ();
    GuiMisc.save_gui_options gui;
    GuiCom.disconnect gui BasicSocket.Closed_by_user;
    if !G.is_docked then G.tray.destroy_tray ()
  );

  MlUnix.set_signal  Sys.sigint
    (Sys.Signal_handle (fun _ -> CommonGlobals.exit_properly 0));

  MlUnix.set_signal  Sys.sigterm
    (Sys.Signal_handle (fun _ -> CommonGlobals.exit_properly 0));

  (************ Some hooks ***************)


  (* connection with core *)

  GuiCom.reconnect gui value_reader gui BasicSocket.Closed_by_user
(*
  let never_connected = ref true in
  BasicSocket.add_timer 1.0 (fun timer ->
      if !never_connected && not (GuiCom.connected ()) then  begin
          BasicSocket.reactivate_timer timer;
          GuiCom.reconnect gui value_reader gui BasicSocket.Closed_by_user
        end else
        never_connected := false
  )
*)

let _ = 
  CommonGlobals.gui_included := true;
  main ()
