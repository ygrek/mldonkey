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

open Printf2
open Options
open Gettext
open Gui_global
open GMain
open GtkBase
open Gdk

module O = Gui_options
module M = Gui_messages
module Com = Gui_com
module G = Gui_global
module Mi = Gui_misc
module D = Gui_downloads
  
(*module Gui_rooms = Gui_rooms2*)
let chmod_config () =
  let base_config = 
    (Filename.concat CommonOptions.home_dir "mldonkey_newgui.ini")
  in
  let save_config =
    base_config^".old"
  in
  let chmod file =
    if Sys.file_exists file then
      try
        Unix.chmod file 0o600 
      with
        exn -> lprintf "Exception: chmod %s 0600 : %s\n" file (Printexc2.to_string exn)
  in
  chmod base_config;
  chmod save_config

let () = 
  (try Options.load O.mldonkey_gui_ini with
      Sys_error _ ->
        (try Options.save O.mldonkey_gui_ini with _ -> ())
     | e ->
        lprintf "Exception %s in load options %s\n" 
         (Printexc2.to_string e)
         (Options.options_file_name O.mldonkey_gui_ini);
  );
  let args = 
    ("-dump_msg", Arg.Unit (fun _ ->
          Options.save Gui_messages.message_file
      ), ": update internationalisation message file")::
    Options.simple_args "" O.mldonkey_gui_ini in
  Arg.parse args (Arg.usage args)  "mlgui: the GUI to use with mldonkey"
  
(* Check bindings *)
let () = 
  if !!O.keymap_global = [] then
    (
     let a = O.add_binding O.keymap_global in
     a "A-n" M.a_page_networks;
     a "A-s" M.a_page_servers;
     a "A-d" M.a_page_downloads;
     a "A-f" M.a_page_friends;
     a "A-q" M.a_page_searches;
     a "A-r" M.a_page_rooms ;
     a "A-u" M.a_page_uploads;
     a "A-c" M.a_page_console;
     a "A-g" M.a_page_graph;
     a "A-Left" M.a_previous_page;
     a "A-Right" M.a_next_page;
     a "C-r" M.a_reconnect;
     a "C-q" M.a_exit ;
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

open CommonTypes
open GuiTypes
open GuiProto
  

let verbose_gui_messages = ref false

let nbr_client_info = ref 0 
let nbr_add_source = ref 0

let value_reader gui t =
  try
    
    if !verbose_gui_messages then begin  
        lprintf "MESSAGE RECEIVED: %s\n" 
          (string_of_to_gui t);
      end;  
    
    
    match t with

      (* The first message received from the core *)
    | CoreProtocol (v,_,_) -> 
        
        
        let version = min v GuiProto.best_gui_version in
        for i = 0 to to_gui_last_opcode do
          Gui_com.to_gui_protocol_used.(i) <- version;
        done;
        for i = 0 to from_gui_last_opcode do
          Gui_com.from_gui_protocol_used.(i) <- version;
        done;
        lprintf "Using protocol %d for communications\n" version;
        gui#label_connect_status#set_text ( M.mW_lb_connected);
        
        if version >= D.interested_in_sources_version then begin
(*            lprintf "-----  not interested in sources -----\n"; *)
            Gui_downloads.use_interested_in_sources := true;
            Com.send (InterestedInSources false);
          end;
        Com.send (Password (!!O.login, !!O.password))

    | Console text ->
        gui#tab_console#insert text
    
    | Network_info n ->
        begin
          try
            let box_net = gui#tab_networks in
            let (box, i) = Gui_networks.retrieve_net_box n.network_netname box_net in
            begin
              try
                let nn = Hashtbl.find Gui_global.networks n.network_netnum in
                nn.net_enabled <- n.network_enabled;
                Gui_networks.fill_box box i nn.net_enabled nn.net_displayed true;
                gui#tab_queries#update_wcombos
              with _ ->
                  let nn = {
                      net_num = n.network_netnum;
                      net_name = n.network_netname;
                      net_enabled = n.network_enabled;
                      net_displayed = true;
                    } in
                  
                  (match n.network_netname with
                      "BitTorrent" -> bittorrent_network := n.network_netnum
                    | _ -> ());
                  
                  Gui_networks.fill_box box i nn.net_enabled nn.net_displayed true;
                   ignore (box#wtog_net#connect#toggled ~callback:(fun _ ->
                        Com.send (EnableNetwork (nn.net_num,
                            box#wtog_net#active)
                        )));
                  ignore (box#wchk_net#connect#toggled ~callback:(fun _ ->
                        nn.net_displayed <- not nn.net_displayed;
                        networks_filtered := (if nn.net_displayed then
                            List2.removeq nn.net_num !networks_filtered
                          else nn.net_num :: !networks_filtered);
                        gui#tab_servers#h_server_filter_networks;
                        gui#tab_queries#h_search_filter_networks;
                        Gui_networks.fill_box box i nn.net_enabled nn.net_displayed true
                    ));
                  Hashtbl.add Gui_global.networks n.network_netnum nn;
                  gui#tab_queries#update_wcombos;
                  ()
            end
          
          with _ -> ()
        
        end
    
    | Client_stats s ->
        gui#tab_uploads#wl_status#set_text  
          (Printf.sprintf Gui_messages.mW_sb_files_shared 
             s.nshared_files 
             (Gui_misc.size_of_int64 s.upload_counter)
             (s.tcp_upload_rate + s.udp_upload_rate) 
             s.udp_upload_rate
             (s.tcp_download_rate + s.udp_download_rate) 
             s.udp_download_rate
        );
        gui#tab_graph#set_upload_rate (s.tcp_upload_rate + s.udp_upload_rate);
        gui#tab_graph#set_download_rate (s.tcp_download_rate + s.udp_download_rate)
    
    | Search_result (num,r,_) -> 
        begin try
            let r = Hashtbl.find G.results r in
            gui#tab_queries#h_search_result num r
          with _ -> 
              lprintf "Exception in Search_result %d %d\n" num r;
        end
    
    | Search_waiting (num,waiting) -> 
        gui#tab_queries#h_search_waiting num waiting
    
    | File_add_source (num, src) -> 
(*        lprintf "File_add_source\n"; *)
(*        incr nbr_add_source;
        Printf.printf "File_add_source %3d %3d = %d" num src !nbr_add_source; print_newline (); *)
        gui#tab_downloads#h_file_location num src
    
    | File_remove_source (num, src) ->
(*        lprintf "File_remove_source\n";  *)
        gui#tab_downloads#h_file_remove_location num src
    
    | File_downloaded (num, downloaded, rate, last_seen) ->
        (* Printf.printf "File_downloaded\n"; flush stdout; *)
        gui#tab_downloads#h_file_downloaded num downloaded rate last_seen

    | File_update_availability (file_num, client_num, avail) ->
        (* Printf.printf "File_update_availability\n"; flush stdout; *)
        gui#tab_downloads#h_file_availability file_num client_num avail
    
    | File_info f ->
(*        lprintf "FILE INFO"; lprint_newline (); *)
        gui#tab_downloads#h_file_info f
    
    | Server_info s ->
(*        lprintf "server info"; lprint_newline (); *)
        gui#tab_servers#h_server_info s
    
    | Server_state (key,state) ->
        gui#tab_servers#h_server_state key state
    
    | Server_busy (key,nusers, nfiles) ->
        gui#tab_servers#h_server_busy key nusers nfiles
    
    | Server_user (key, user) ->
(*        lprintf "server user %d %d" key user; lprint_newline (); *)
        if not (Hashtbl.mem G.users user) then begin
(*            lprintf "Unknown user %d" user; lprint_newline ();*)
            Gui_com.send (GetUser_info user);
          end else 
          begin
            gui#tab_servers#h_server_user key user
          end
    
    | Room_info room ->
(*        lprintf "Room info %d" room.room_num; lprint_newline (); *)
        gui#tab_rooms#room_info room
    
    | User_info user ->
        let user = try 
            let u = Hashtbl.find G.users user.user_num  in
            u.user_tags <- user.user_tags;
            u
          with Not_found ->
              Hashtbl.add G.users user.user_num user; 
              user
        in
(*        lprintf "user_info %s/%d" user.user_name user.user_server; lprint_newline (); *)
        gui#tab_servers#h_server_user user.user_server user.user_num;
        Gui_rooms.user_info user
    
    | Room_add_user (num, user_num) -> 
        
        begin try
            gui#tab_rooms#add_room_user num user_num
          with e ->
              lprintf "Exception in Room_user %d %d" num user_num;
              lprint_newline ();
        end
    
    | Room_remove_user (num, user_num) -> 
        
        begin try
            gui#tab_rooms#remove_room_user num user_num
          with e ->
              lprintf "Exception in Room_user %d %d" num user_num;
              lprint_newline ();
        end
    
    | Options_info list ->
(*        lprintf "Options_info"; lprint_newline ();*)
        let module M = Options in
        let rec iter list =
          match list with
            [] -> ()
          | o :: tail ->
              (
                try
                  let reference = 
                    List.assoc o.M.option_name Gui_options.client_options_assocs 
                  in                  
                  reference := o.M.option_value;
                  Gui_config.add_option_value o.M.option_name reference
                with _ -> 
                    Gui_config.add_option_value o.M.option_name (ref o.M.option_value)
              );
              iter tail
        in
        iter list
        
    | Add_section_option (section, o) ->
        let optype = match o.option_type with
            "Bool" -> BoolEntry
          | "Filename" -> FileEntry
          | _ -> StringEntry in
        let line = o.option_desc, optype, o.option_name in
        (try
            let options = List.assoc section !client_sections in
            if not (List.mem line !options) then
              options := !options @ [line]
          with _ ->
              client_sections := !client_sections  @[section, ref [line]]
        )          
    
    | Add_plugin_option (section, o) ->
        let optype = match o.option_type with
            "Bool" -> BoolEntry
          | "Filename" -> FileEntry
          | _ -> StringEntry in
        let line = o.option_desc, optype, o.option_name in
        (try
            let options = List.assoc section !plugins_sections in
            if not (List.mem line !options) then
              options := !options @ [line]
          with _ ->
              plugins_sections := !plugins_sections  @[section, ref [line]]
        )          
    
    | DefineSearches l ->
        gui#tab_queries#h_define_searches l
    
    | Client_state (num, state) ->
(*        lprintf "Client_state\n" ; *)
        gui#tab_friends#h_update_friend_state (num , state);
        gui#tab_uploads#h_update_client_state (num , state);
        gui#tab_downloads#h_update_client_state (num , state)
        
    | Client_friend (num, friend_kind) ->
(*        lprintf "Client_friend\n" ; *)
        gui#tab_friends#h_update_friend_type (num , friend_kind);
        gui#tab_uploads#h_update_client_type (num , friend_kind);
        gui#tab_downloads#h_update_client_type (num , friend_kind)
    
    | Result_info r ->
        
        if not (Hashtbl.mem G.results r.result_num) then
          Hashtbl.add G.results r.result_num r
    
    | Client_file (num , dirname, file_num) ->
(* Here, the dirname is forgotten: it should be used to build a tree
  when possible... *)
        gui#tab_friends#h_add_friend_files (num , dirname, file_num)
    
    | Client_info c -> 
(*        lprintf "Client_info\n" ; *)
(*        incr nbr_client_info;
        Printf.printf  "Client_info %d, %d/%d" c.client_num !nbr_client_info !nbr_add_source; print_newline (); *)
        gui#tab_friends#h_update_friend c;
        gui#tab_uploads#h_update_client c;
        gui#tab_downloads#h_update_client c

    | Room_message (_, PrivateMessage(num, mes) )
    | Room_message (0, PublicMessage(num, mes) )
    | MessageFromClient (num, mes) ->
        (
          try
            let box_uploaders = gui#tab_uploads#box_uploaders in
            let (row , c ) = box_uploaders#find_client num in
            let d = gui#tab_friends#get_dialog c in
            d#handle_message mes
          with
            Not_found ->
              try
                match t with
                  Room_message (num, msg) ->
                    gui#tab_rooms#add_room_message num msg                
                | _ -> raise Not_found
              with Not_found -> ()
                  (* lprintf "Client %d not found in reader.MessageFromClient" num;
                  lprint_newline () *)
        )    
    
    | Room_message (num, msg) ->
        begin try
            gui#tab_rooms#add_room_message num msg
          with e -> ()
              (* lprintf "Exception in Room_message %d" num;
              lprint_newline ();*)
        end
    
    | (DownloadedFiles _|DownloadFiles _|ConnectedServers _) -> assert false
    
    | Shared_file_info si ->
        gui#tab_uploads#h_shared_file_info si
    
    | CleanTables (clients, servers) ->
        gui#tab_servers#clean_table servers;
        gui#tab_downloads#clean_table clients;
        gui#tab_uploads#clean_table clients
    
    | Shared_file_upload (num,size,requests) ->
        gui#tab_uploads#h_shared_file_upload num size requests
    | Shared_file_unshared _ ->  ()
    | BadPassword -> 
        GToolbox.message_box 
          ~title:(M.pW_wt_bad_password)
          (M.pW_lb_bad_password)
    
    | Uploaders l ->
        if !displayed_page = uploads_page then
          gui#tab_uploads#h_update_uploaders l

    | Pending l ->
        if !displayed_page = uploads_page then
          gui#tab_uploads#h_update_pending_slots l

    | GiftServerAttach _
    | GiftServerStats _ -> assert false
    | Version _
    | Stats (_, _)
    | Search _ -> ()
        
  with e ->
      lprintf "Exception %s in reader\n" (Printexc2.to_string e)

      
let generate_connect_menu gui menu =
  let add_item hostname port =
    let menu_item =
      let label = Printf.sprintf "%s:%d" hostname port in
      GMenu.menu_item ~label
      ~packing:menu#add ()
      in
    ignore (menu_item#connect#activate ~callback:(fun _ ->
          O.hostname =:= hostname;
          O.port =:= port;
          Com.reconnect (gui :> Com.gui) value_reader gui BasicSocket.Closed_by_user
      ));
  in
  List.iter (fun child -> child#destroy ()) menu#children;
  List.iter (fun (h,port) -> add_item h port) !!O.history;
  List.iter (fun (h,port) -> add_item h port) !G.scanned_ports

let gui_menu gui =
  let menu = GMenu.menu () in
  let reconnect =
    GMenu.menu_item ~label:(M.mW_me_reconnect)
      ~packing:(menu#add) ()
  in
  let disconnect =
    GMenu.menu_item ~label:(M.mW_me_disconnect)
      ~packing:(menu#add) ()
  in
  let scanports =
    GMenu.menu_item ~label:(M.mW_me_scan_ports) 
      ~packing:(menu#add) ()
  in
  let reconnect_to =
    GMenu.menu_item ~label:(M.mW_me_reconnect_to)
      ~packing:(menu#add) ()
  in
  let reconnect_to_menu = GMenu.menu ~packing:(reconnect_to#set_submenu) () in
  generate_connect_menu gui reconnect_to_menu; 
  ignore (reconnect#connect#activate 
      (fun () -> Com.reconnect (gui :> Com.gui) value_reader gui BasicSocket.Closed_by_user));
  ignore (disconnect#connect#activate 
      (fun () -> Com.disconnect (gui :> Com.gui) BasicSocket.Closed_by_user));
  ignore (scanports#connect#activate
      (fun _ -> Com.scan_ports ()));
  menu
      



let window_about _ =
  let window =
    GWindow.window ~position:`CENTER_ALWAYS ~kind:`POPUP
      ~width:375 ~height:275 ()
  in
  Widget.realize window#as_widget ;
  Widget.set_app_paintable window#as_widget true;
  let splash_screen = O.gdk_pix M.o_xpm_splash_screen in
  let wpix = `PIXMAP(splash_screen#pixmap) in
  Window.set_back_pixmap window#misc#window wpix;
  ignore (window#event#add [`BUTTON_PRESS]);
  let vbox =
    GPack.vbox ~homogeneous:false
      ~packing:(window#add) ()
  in
  let vbox_ =
    GPack.vbox ~homogeneous:false
      ~packing:(vbox#pack ~fill:true ~expand:true) ()
  in
  let hbox =
    GPack.hbox ~homogeneous:false
      ~packing:(vbox#pack ~fill:true ~expand:false) ()
  in
  let label =
    GMisc.label ~line_wrap:true ~text:(Printf.sprintf "v. %s" Autoconf.current_version)
      ~justify:`LEFT ~packing:(hbox#pack ~fill:true ~expand:false ~padding:10) ()
  in
  ignore (window#event#connect#button_press
    ~callback:(fun e ->
      GdkEvent.get_type e = `BUTTON_PRESS &&
      (
        window#destroy ();
        true
      )
  ));
  window#show ();
  window


let main () =
  let gui = new Gui_window.window () in
  let w = gui#window in
  let quit () = 
    chmod_config (); 
    CommonGlobals.exit_properly 0
  in
  chmod_config (); 
  let splash_window = window_about () in
  Gui_config.update_toolbars_style gui;
  Gui_config.update_list_bg gui;
  Gui_config.update_graphs gui;
  Gui_config.update_availability_column gui;
  Gui_config.update_icons gui;
  
  
  gui#notebook#goto_page !!O.last_tab;

  ignore (w#event#connect#delete (fun _ ->
                                     quit ();
                                     true)
  );

  console_message := (fun s -> gui#tab_console#insert s);

  CommonGlobals.do_at_exit (fun _ ->
      Gui_misc.save_gui_options gui;
      Gui_com.disconnect (gui :> Com.gui) BasicSocket.Closed_by_user);  

(** button actions *)

  ignore (gui#buttonQuit#connect#clicked 
      (fun () -> CommonGlobals.exit_properly 0)) ;

  ignore (gui#buttonKill#connect#clicked 
      (fun () -> Com.send KillServer));

  ignore (gui#buttonGui#connect#clicked 
      (fun () -> 
         let menu = gui_menu gui in
         menu#popup ~button:1 ~time:0
      ));

  ignore (gui#buttonOptions#connect#clicked 
      (fun () -> Gui_config.edit_options gui));

  ignore (gui#buttonAbout#connect#clicked 
      (fun () -> ignore (window_about ())));

  (************ Some hooks ***************)

  option_hook Gui_options.notebook_tab 
      (fun _ -> gui#notebook#set_tab_pos !!Gui_options.notebook_tab);
  
  (** close the splash window *)

  splash_window#destroy ();

  (** connection with core *)

  Com.reconnect (gui :> Com.gui) value_reader gui BasicSocket.Closed_by_user ;

  let never_connected = ref true in
  BasicSocket.add_timer 1.0 (fun timer ->
      if !never_connected && not (Com.connected ()) then  begin
          BasicSocket.reactivate_timer timer;
          Com.reconnect (gui :> Com.gui) value_reader gui BasicSocket.Closed_by_user
        end else
        never_connected := false
  )
  
let () = 
  CommonGlobals.gui_included := true;
  main ()
