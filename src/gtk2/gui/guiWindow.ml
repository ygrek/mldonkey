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

(* Main window of the GUI. *)


open GuiTypes2
open GuiTools


module M = GuiMessages
module O = GuiOptions
module G = GuiGlobal
module A = GuiArt
module U = GuiUtf8

let (!!) = Options.(!!)
let (<:>) = GuiTools.(<:>)

(*************************************************************************)
(*                                                                       *)
(*                         clean                                         *)
(*                                                                       *)
(*************************************************************************)

let clean w =
  List.iter (fun w -> w#destroy ()) w#children

(*************************************************************************)
(*                                                                       *)
(*                         display_networks                              *)
(*                                                                       *)
(*************************************************************************)

let display_networks gui () =
  clean gui.vbox;
  let networks = GuiNetworks.networks_box gui in
  gui.vbox#add networks;
  gui.current_page <- 0

(*************************************************************************)
(*                                                                       *)
(*                         display_servers                               *)
(*                                                                       *)
(*************************************************************************)

let display_servers gui () =
  clean gui.vbox;
  let servers = GuiServers.servers_box gui in
  gui.vbox#add servers;
  gui.current_page <- 1

(*************************************************************************)
(*                                                                       *)
(*                         display_downloads                             *)
(*                                                                       *)
(*************************************************************************)

let display_downloads gui () =
  clean gui.vbox;
  let downloads = GuiDownloads.downloads_box gui in
  gui.vbox#add downloads;
  gui.current_page <- 2

(*************************************************************************)
(*                                                                       *)
(*                         display_friends                               *)
(*                                                                       *)
(*************************************************************************)

let display_friends gui () =
  clean gui.vbox;
  let friends = GuiFriends.friends_box gui in
  gui.vbox#add friends;
  gui.current_page <- 3

(*************************************************************************)
(*                                                                       *)
(*                         display_search                                *)
(*                                                                       *)
(*************************************************************************)

let display_search gui () =
  clean gui.vbox;
  let queries = GuiQueries.queries_box gui in
  gui.vbox#add queries;
  gui.current_page <- 4

(*************************************************************************)
(*                                                                       *)
(*                         display_rooms                                 *)
(*                                                                       *)
(*************************************************************************)

let display_rooms gui () =
  clean gui.vbox;
  let rooms = GuiRooms.rooms_box gui in
  gui.vbox#add rooms;
  gui.current_page <- 5

(*************************************************************************)
(*                                                                       *)
(*                         display_uploads                               *)
(*                                                                       *)
(*************************************************************************)

let display_uploads gui () =
  clean gui.vbox;
  let uploads = GuiUploads.uploads_box gui in
  gui.vbox#add uploads;
  gui.current_page <- 6

(*************************************************************************)
(*                                                                       *)
(*                         display_console                               *)
(*                                                                       *)
(*************************************************************************)

let display_console gui () =
  clean gui.vbox;
  let console = GuiConsole.console_box gui in
  gui.vbox#add console;
  gui.current_page <- 7

(*************************************************************************)
(*                                                                       *)
(*                         display_graph                                 *)
(*                                                                       *)
(*************************************************************************)

(* TODO *)

let display_graph gui () =
  clean gui.vbox;
  gui.current_page <- 8

(*************************************************************************)
(*                                                                       *)
(*                         display_im                                    *)
(*                                                                       *)
(*************************************************************************)

let display_im gui () =
  GuiIm.main_window ()

(*************************************************************************)
(*                                                                       *)
(*                         display_settings                              *)
(*                                                                       *)
(*************************************************************************)

let rec display_settings gui value_reader =
  GuiConfig.config_window gui value_reader fill_tool_bar

and fill_tool_bar gui =
  let bNetworks = gui.wtool#add_toggle_button
      ~style:!!O.gtk_look_toolbars_style
      ~markup:!M.mW_lb_networks
      ~icon:(A.get_icon ~icon:M.icon_menu_networks ~size:A.LARGE ())
      ~f:(display_networks gui) ()
  in
  let bServers = gui.wtool#add_toggle_button
      ~style:!!O.gtk_look_toolbars_style
      ~markup:!M.mW_lb_servers
      ~icon:(A.get_icon ~icon:M.icon_menu_servers ~size:A.LARGE ())
      ~f:(display_servers gui) ()
  in
  let bDownloads = gui.wtool#add_toggle_button
      ~style:!!O.gtk_look_toolbars_style
      ~markup:!M.mW_lb_downloads
      ~icon:(A.get_icon ~icon:M.icon_menu_downloads ~size:A.LARGE ())
      ~f:(display_downloads gui) ()
  in
  let bFriends = gui.wtool#add_toggle_button
      ~style:!!O.gtk_look_toolbars_style
      ~markup:!M.mW_lb_friends
      ~icon:(A.get_icon ~icon:M.icon_menu_friends ~size:A.LARGE ())
      ~f:(display_friends gui) ()
  in
  let bSearch = gui.wtool#add_toggle_button
      ~style:!!O.gtk_look_toolbars_style
      ~markup:!M.mW_lb_search
      ~icon:(A.get_icon ~icon:M.icon_menu_searches ~size:A.LARGE ())
      ~f:(display_search gui) ()
  in
  let bRooms = gui.wtool#add_toggle_button
      ~style:!!O.gtk_look_toolbars_style
      ~markup:!M.mW_lb_rooms
      ~icon:(A.get_icon ~icon:M.icon_menu_rooms ~size:A.LARGE ())
      ~f:(display_rooms gui) ()
  in
  let bUploads = gui.wtool#add_toggle_button
      ~style:!!O.gtk_look_toolbars_style
      ~markup:!M.mW_lb_uploads
      ~icon:(A.get_icon ~icon:M.icon_menu_uploads ~size:A.LARGE ())
      ~f:(display_uploads gui) ()
  in
  let bConsole = gui.wtool#add_toggle_button
      ~style:!!O.gtk_look_toolbars_style
      ~markup:!M.mW_lb_console
      ~icon:(A.get_icon ~icon:M.icon_menu_console ~size:A.LARGE ())
      ~f:(display_console gui) ()
  in
(*
 * TODO : make a Graph tab + some stats
 *
  let bGraph = gui.wtool#add_toggle_button
      ~style:!!O.gtk_look_toolbars_style
      ~markup:!M.mW_lb_graph
      ~icon:(A.get_icon ~icon:M.icon_menu_graph ~size:A.LARGE ())
      ~f:(display_graph gui) ()
  in
*)
(*
  let bIm = gui.wtool#add_button
      ~style:!!O.gtk_look_toolbars_style
      ~markup:!M.mW_lb_im
      ~icon:(A.get_icon ~icon:M.icon_menu_im ~size:A.LARGE ())
      ~f:(display_im gui) ()
  in
  let bSettings = gui.wtool#add_button
      ~style:!!O.gtk_look_toolbars_style
      ~markup:!M.mW_lb_settings
      ~icon:(A.get_icon ~icon:M.icon_menu_settings ~size:A.LARGE ())
      ~f:(display_settings gui) ()
  in
*)
  gui.switch_to_page <- (fun page ->
    match page with
        0 -> bNetworks#set_active true
      | 1 -> bServers#set_active true
      | 3 -> bFriends#set_active true
      | 4 -> bSearch#set_active true
      | 5 -> bRooms#set_active true
      | 6 -> bUploads#set_active true
      | 7 -> bConsole#set_active true
(*
      | 8 -> bGraph#set_active true
*)
      | _ -> bDownloads#set_active true
  );
  gui.update_current_page <- (fun _ ->
    match gui.current_page with
        0 -> display_networks gui ()
      | 1 -> display_servers gui ()
      | 3 -> display_friends gui ()
      | 4 -> display_search gui ()
      | 5 -> display_rooms gui ()
      | 6 -> display_uploads gui ()
      | 7 -> display_console gui ()
(*
      | 8 -> display_graph gui ()
*)
      | _ -> display_downloads gui ()
  )

(*************************************************************************)
(*                                                                       *)
(*                         window                                        *)
(*                                                                       *)
(*************************************************************************)

let window () =
  let win =
    GWindow.window 
      ~title:(!M.mW_wt_software)
      ~icon:(A.get_icon ~icon:M.icon_type_source_normal ~size:A.SMALL ())
      ~resizable:true ~modal:false ()
  in
  win#maximize ();
  let vbox =
    GPack.vbox ~homogeneous:false ~packing:(win#add) ()
  in
  let hbox = 
    GPack.hbox ~homogeneous:false
      ~packing:(vbox#pack ~expand:false ~fill:true) () in
  let wtool =
    tool_bar `HORIZONTAL
      ~layout:`START ~packing:hbox#add ()
  in
  let vbox_view = 
    GPack.vbox ~homogeneous:false
      ~packing:(vbox#pack ~expand:true ~fill:true) ()
  in
  let gui = 
    {
     window = win;
     vbox = vbox_view;
     wtool = wtool;
     init =
       {
        networks = true;
        servers = true;
        downloads = true;
        friends = true;
        queries = true;
        rooms = true;
        uploads = true;
        settings = true;
        console = true;
       };
     clear = (fun _ ->
                GuiStatusBar.clear ();
                GuiNetworks.clear ();
                GuiServers.clear ();
                GuiDownloads.clear ();
                GuiFriends.clear ();
                GuiQueries.clear ();
                GuiRooms.clear ();
                GuiUploads.clear ();
                GuiConsole.clear ();
                GuiConfig.clear ();
             );

     set_corestatus = GuiStatusBar.update_corestatus;
     current_page = !!O.last_tab;
     switch_to_page = (fun _ -> ());
     set_splash_screen = (fun _ _ -> ());
     update_current_page = (fun _ -> ());
    }
  in

  fill_tool_bar gui;

  let statusbar = GuiStatusBar.status_box () in
  vbox#pack ~expand:false ~fill:true statusbar;

  gui
      
