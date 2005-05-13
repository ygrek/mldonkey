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

(* preference settings of MLDonkey. *)

open CommonTypes
open GuiProto
open GuiTypes2
open Options
open ConfigWindow

module M = GuiMessages
module Mi = GuiMisc
module G = GuiGlobal
module O = GuiOptions
module A = GuiArt
module U = GuiUtf8

let (!!) = Options.(!!)
let (<:>) = GuiTools.(<:>)

let verbose = O.gtk_verbose_settings

let lprintf' fmt =
  Printf2.lprintf ("GuiConfig: " ^^ fmt)

(*************************************************************************)
(*                                                                       *)
(*                         Global variables                              *)
(*                                                                       *)
(*************************************************************************)

let advanced = ref !!O.gtk_advanced_options

(*************************************************************************)
(*                                                                       *)
(*                         opt_type_to_pref_type                         *)
(*                                                                       *)
(*************************************************************************)

let opt_type_to_pref_type o =
    match o.option_type with
        "Bool" -> BBool
      | "Advanced" -> BAdvanced
      | "Filename" -> BFilename
      | "Path" -> BPath
      | "Password" -> BPassword
      | "Time" -> BTime
      | "Color" -> BColor
      | "Font" -> BFont
      | "Int" -> BInt
      | "Int32" -> BInt32
      | "Int64" -> BInt64
      | "Float" -> BFloat
      | "Scale" -> BScale
      | _ ->
          begin
            if !!verbose
              then begin
                if o.option_type <> "String"
                  then lprintf' "Parameter type: %s\n   name : %s\n   value : %s\n"
                         o.option_type o.option_name o.option_value;
              end;
            BString
          end

(*************************************************************************)
(*                                                                       *)
(*                         string_to_section                             *)
(*                                                                       *)
(*************************************************************************)

let string_to_section s =
  match s with
      "Main" -> Some Main
    | "Bandwidth" -> Some Main
    | "Network Config" -> Some Main
    | "Security" -> Some Main
    | "Interfaces" -> Some Interfaces
    | "HTML mods" -> Some Interfaces
    | "MLgui" -> Some Interfaces
    | "MLguiDebug" -> Some Interfaces
    | "MLChat" -> Some Mlchat
    | "Other" -> Some Other
    | "Paths" -> Some Tools
    | "Startup" -> Some Tools
    | "Download" -> Some Tools
    | "Mail" -> Some Tools
    | "Debug" -> Some Tools
    | "BitTorrent" -> Some Bittorrent
    | "Direct Connect" -> Some Direct_connect
    | "Donkey" -> Some Donkey
    | "Fasttrack" -> Some Fasttrack
    | "FileTP" -> Some Filetp
    | "Gnutella" -> Some Gnutella
    | "G2" -> Some Gnutella
    | "Open Napster" -> Some Open_napster
    | "Soulseek" -> Some Soulseek
    | _ -> None

(*************************************************************************)
(*                                                                       *)
(*                         string_to_subsection                          *)
(*                                                                       *)
(*************************************************************************)

let string_to_subsection s =
  match s with
      "Main" -> Some General
    | "Bandwidth" -> Some Bandwidth
    | "Network Config" -> Some Network_config
    | "Security" -> Some Security
    | "Interfaces" -> Some General
    | "HTML mods" -> Some Html_mods
    | "MLgui" -> Some Mlgui
    | "MLguiDebug" -> Some Debug
    | "MLChat" -> None
    | "Other" -> None
    | "Paths" -> Some Paths
    | "Startup" -> Some Startup
    | "Download" -> Some Download
    | "Mail" -> Some Mail
    | "Debug" -> Some Debug
    | "BitTorrent" -> None
    | "Direct Connect" -> None
    | "Donkey" -> None
    | "Fasttrack" -> None
    | "FileTP" -> None
    | "Gnutella" -> Some Gnutella1
    | "G2" -> Some Gnutella2
    | "Open Napster" -> None
    | "Soulseek" -> None
    | _ -> None


(*************************************************************************)
(*                                                                       *)
(*                         regexp                                        *)
(*                                                                       *)
(*************************************************************************)

let rate_reg = Str.regexp_case_fold ".*rate.*"
let connect_reg = Str.regexp_case_fold ".*connection.*"
let uploads_reg = Str.regexp_case_fold ".*slots.*"
let proxy_reg = Str.regexp_case_fold ".*proxy.*"
let client_reg = Str.regexp_case_fold ".*client.*"
let connect_param_reg = Str.regexp_case_fold ".*packet.*"
let gui_reg = Str.regexp_case_fold ".*gui.*"
let web_reg = Str.regexp_case_fold ".*http.*"
let telnet_reg = Str.regexp_case_fold ".*telnet.*"
let gift_reg = Str.regexp_case_fold ".*gift.*"
let peer_reg = Str.regexp_case_fold ".*peer.*"
let server_reg = Str.regexp_case_fold ".*server.*"
let mail_reg = Str.regexp_case_fold ".*smtp.*\\|mail"
let mlchat_server_reg = Str.regexp_case_fold ".*app.*"
let tracker_reg = Str.regexp_case_fold ".*tracker.*"
let overnet_reg = Str.regexp_case_fold ".*overnet.*"
let shared_reg = Str.regexp_case_fold ".*shared.*"
let display_reg = Str.regexp_case_fold ".*vd.*\\|.*show.*\\|.*availability.*"
let look_reg = Str.regexp_case_fold ".*theme.*\\|.*style.*\\|.*checkbox.*\\|.*human_readable.*\\|html_mods"
let mlchat_client_reg = Str.regexp_case_fold ".*addr.*\\|.*port.*"
let dc_client_reg = Str.regexp_case_fold ".*login.*\\|.*firewalled.*\\|.*client.*"
let dc_server_reg = Str.regexp_case_fold ".*hub.*\\|.*server*."
let ed2k_client_reg = Str.regexp_case_fold ".*md4.*\\|.*login.*\\|.*high_id.*\\|.*port.*\\|.*max_xs.*\\|.*max_udp.*"
let ed2k_upload_reg = Str.regexp_case_fold ".*upload.*\\|.*warning.*\\|.*queued.*"
let ed2k_download_reg = Str.regexp_case_fold ".*download.*\\|.*sources_per_chunk.*"
let ed2k_sources_reg = Str.regexp_case_fold ".*source.*\\|.*initial_score.*\\|.*ban.*\\|.*clients.*\\|.*client_connections_delay.*\\|.*client_rank.*\\|.*black_list.*\\|.*max_indirect.*"
let ed2k_server_reg = Str.regexp_case_fold ".*files_queries.*\\|.*connections_delay.*\\|.*protocol_version.*\\|.*server.*"
let nap_server_reg = Str.regexp_case_fold ".*napigator.*\\|.*server.*"
let slsk_client_reg = Str.regexp_case_fold ".*client_.*\\|.*login.*\\|.*password.*"
let slsk_sources_reg = Str.regexp_case_fold ".*token.*\\|.*clients.*\\|.*cache_.*"
let mlgui_client_reg = Str.regexp_case_fold ".*gtk_client_.*"
let mlgui_look_reg = Str.regexp_case_fold ".*gtk_look_.*"
let mlgui_color_reg = Str.regexp_case_fold ".*gtk_color_.*"
let mlgui_font_reg = Str.regexp_case_fold ".*gtk_font_.*"
let mlgui_graph_reg = Str.regexp_case_fold ".*gtk_graph_.*"
let html_mods_reg = Str.regexp_string "html_mods_"

let groups_reg =
  [
   (Some Main, Some Bandwidth),
        [(Some rate_reg, Some Rates); (Some connect_reg, Some Connections);
         (Some uploads_reg, Some Uploads); (None, Some Others)];
   (Some Main, Some Network_config),
        [(Some proxy_reg, Some Proxy); (Some client_reg, Some Client);
         (Some connect_param_reg, Some Connection_param); (None, Some Others)];
   (Some Interfaces, Some General),
        [(Some gui_reg, Some Gui); (Some web_reg, Some Web); (Some telnet_reg, Some Telnet);
         (Some gift_reg, Some Gift); (None, Some General_)];
   (Some Interfaces, Some Html_mods),
        [(Some display_reg, Some Display_conf); (Some look_reg, Some Look);
         (None, Some General_)];
   (Some Interfaces, Some Mlgui),
        [(Some mlgui_client_reg, Some Client); (Some mlgui_look_reg, Some Look);
         (Some mlgui_color_reg, Some Colors); (Some mlgui_font_reg, Some Fonts);
         (Some mlgui_graph_reg, Some Graph); (None, Some Others)];
   (Some Tools, Some Startup),
        [(Some gui_reg, Some Gui); (None, Some User)];
   (Some Tools, Some Mail),
        [(Some mail_reg, Some Mail_setup); (None, Some Others)];
   (Some Mlchat, None),
        [(Some mlchat_server_reg, Some Server); (Some mlchat_client_reg, Some Client);
         (None, Some General_)];
   (Some Bittorrent, None),
        [(Some client_reg, Some Client); (Some tracker_reg, Some Tracker);
         (None, Some Others)];
   (Some Direct_connect, None),
        [(Some dc_client_reg, Some Client); (Some dc_server_reg, Some Servers);
         (None, Some Others)];
   (Some Donkey, None),
        [(Some overnet_reg, Some Overnet); (Some ed2k_client_reg, Some Client);
         (Some ed2k_upload_reg, Some Uploads); (Some ed2k_download_reg, Some Downloads);
         (Some ed2k_sources_reg, Some Sources); (Some ed2k_server_reg, Some Servers);
         (None, Some Others)];
   (Some Fasttrack, None),
        [(Some client_reg, Some Client); (Some server_reg, Some Servers);
         (Some uploads_reg, Some Uploads); (Some peer_reg, Some Peers);
         (None, Some Others)];
   (Some Gnutella, Some Gnutella1),
        [(Some client_reg, Some Client); (Some uploads_reg, Some Uploads);
         (Some server_reg, Some Servers); (Some peer_reg, Some Peers);
         (None, Some Others)];
   (Some Gnutella, Some Gnutella2),
        [(Some client_reg, Some Client); (Some uploads_reg, Some Uploads);
         (Some server_reg, Some Servers); (Some peer_reg, Some Peers);
         (None, Some Others)];
   (Some Open_napster, None),
        [(Some client_reg, Some Client); (Some nap_server_reg, Some Servers);
         (Some shared_reg, Some Uploads); (None, Some Others)];
   (Some Soulseek, None),
        [(Some slsk_client_reg, Some Client); (Some slsk_sources_reg, Some Sources);
         (Some server_reg, Some Servers); (None, Some Others)]
 ]


let rec iter (tuple_regs : (Str.regexp option * gui_group option) list) name =
  match tuple_regs with
      [] -> None
    | tuple_reg :: tail ->
        match tuple_reg with
            (None, group) -> group
          | (Some reg, group) ->
              if Str.string_match reg name 0
                then group
                else iter tail name

(*************************************************************************)
(*                                                                       *)
(*                         define_group                                  *)
(*                                                                       *)
(*************************************************************************)

let define_group p =
  try
    let group_reg = List.assoc (p.pref_section, p.pref_subsection) groups_reg in
    iter group_reg p.pref_name
  with Not_found -> None

(*************************************************************************)
(*                                                                       *)
(*                         define_label                                  *)
(*                                                                       *)
(*************************************************************************)

let define_label p =
  let s = Str.replace_first html_mods_reg "" p.pref_name in
  let s = String2.replace s '_' " " in
  try
    let pos = String.index s '-' in
    let len = (String.length s) - (pos + 1) in
    let s = String.sub s (pos + 1) len in
    s
  with Not_found -> s

(*************************************************************************)
(*                                                                       *)
(*                         redefine_pref_type                            *)
(*                                                                       *)
(*************************************************************************)

let redefine_pref_type p =
match p.pref_subsection with
    Some Paths ->
      begin
        match p.pref_name with
            "incoming_directory"
          | "mldonkey_bin"
          | "mldonkey_gui"
          | "temp_directory" -> BPath
          | "previewer" -> BFilename
          | _ -> p.pref_type
      end
  | Some Html_mods ->
      begin
        match p.pref_name with
            "html_mods_theme" -> BPath
          | _ -> p.pref_type
      end
  | Some Mlgui ->
      begin
        match p.pref_name with
            "gtk_look_toolbars_style" ->
              (p.pref_option_list <- List.map (fun (s, t) -> s) O.tb_styles;
               BCombo)
          | "gtk_client_lang" ->
              (p.pref_option_list <- U.languages;
               BCombo)
          | _ -> p.pref_type
      end
  | _ -> p.pref_type

(*************************************************************************)
(*                                                                       *)
(*                         add_preference                                *)
(*                                                                       *)
(*************************************************************************)


let add_preference (section, o) pv =
  try
    let p = Hashtbl.find pv o.option_name in
    p.pref_name <- o.option_name;
    p.pref_value <- o.option_value;
    p.pref_new_value <- o.option_value
  with _ ->
    if section <> "Networks" then
    begin
      let pref =
        {
          pref_name = o.option_name;
          pref_label = U.utf8_of o.option_desc;
          pref_section = string_to_section section;
          opt_section = section;
          pref_subsection = string_to_subsection section;
          pref_group = None;
          pref_help = U.simple_utf8_of (Gettext._s section o.option_help);
          pref_advanced = o.option_advanced;
          pref_default = o.option_default;
          pref_type = opt_type_to_pref_type o;
          pref_option_list = [];
          pref_value = o.option_value;
          pref_new_value = o.option_value;
          pref_apply = (fun () -> ());
          pref_apply_default = (fun () -> ());
        }
      in
      pref.pref_type <- redefine_pref_type pref;
      pref.pref_group <- define_group pref;
      Hashtbl.add pv o.option_name pref
    end

(*************************************************************************)
(*                                                                       *)
(*                         update_preferences                            *)
(*                                                                       *)
(*************************************************************************)

let update_preferences list pv =
  let module M = Options in
  let rec iter list =
    match list with
        [] -> ()
      | o :: tail ->
          begin
            let _ =
              match o.M.option_name with
                  "client_name" -> G.client_name := o.M.option_value
                | _ -> ()
            in
            try
              let p = Hashtbl.find pv o.option_name in
              p.pref_name <- o.option_name;
              p.pref_value <- o.option_value;
              p.pref_new_value <- o.option_value;
            with _ -> ()
          end;
          iter tail
  in
  iter list

(*************************************************************************)
(*                                                                       *)
(*                         section_to_label                              *)
(*                                                                       *)
(*************************************************************************)

let section_to_label sec =
  match sec with
      Some Main -> !M.cW_lb_main
    | Some Interfaces -> !M.cW_lb_interfaces
    | Some Tools -> !M.cW_lb_tools
    | Some Mlchat -> !M.cW_lb_mlchat
    | Some Other -> !M.cW_lb_other
    | Some Bittorrent -> !M.cW_lb_bittorrent
    | Some Direct_connect -> !M.cW_lb_direct_connect
    | Some Donkey -> !M.cW_lb_donkey
    | Some Fasttrack -> !M.cW_lb_fasttrack
    | Some Filetp -> !M.cW_lb_filetp
    | Some Gnutella -> !M.cW_lb_gnutella
    | Some Open_napster -> !M.cW_lb_open_napster
    | Some Soulseek -> !M.cW_lb_soulseek
    | None -> ""

(*************************************************************************)
(*                                                                       *)
(*                         subsection_to_label                           *)
(*                                                                       *)
(*************************************************************************)

let subsection_to_label sub =
  match sub with
      Some General -> !M.cW_lb_general
    | Some Bandwidth -> !M.cW_lb_bandwidth
    | Some Network_config -> !M.cW_lb_network_config
    | Some Security -> !M.cW_lb_security
    | Some Mlgui -> !M.cW_lb_mlgui
    | Some Html_mods -> !M.cW_lb_html_mods
    | Some Paths -> !M.cW_lb_paths
    | Some Startup -> !M.cW_lb_startup
    | Some Download -> !M.cW_lb_download
    | Some Mail -> !M.cW_lb_mail
    | Some Debug -> !M.cW_lb_debug
    | Some Gnutella1 -> !M.cW_lb_gnutella1
    | Some Gnutella2 -> !M.cW_lb_gnutella2
    | None -> ""

(*************************************************************************)
(*                                                                       *)
(*                         group_to_label                                *)
(*                                                                       *)
(*************************************************************************)

let group_to_label grp =
  match grp with
      Some General_ -> !M.cW_lb_general_
    | Some Rates -> !M.cW_lb_rates
    | Some Connections -> !M.cW_lb_connections
    | Some Proxy -> !M.cW_lb_proxy
    | Some User -> !M.cW_lb_user
    | Some Gui -> !M.cW_lb_gui
    | Some Web -> !M.cW_lb_web
    | Some Telnet -> !M.cW_lb_telnet
    | Some Gift -> !M.cW_lb_gift
    | Some Colors -> !M.cW_lb_colors
    | Some Fonts -> !M.cW_lb_fonts
    | Some Graph -> !M.cW_lb_graph
    | Some Display_conf -> !M.cW_lb_display_conf
    | Some Look -> !M.cW_lb_look
    | Some Mail_setup -> !M.cW_lb_mail_setup
    | Some Client -> !M.cW_lb_client
    | Some Server ->  !M.cW_lb_server
    | Some Connection_param -> !M.cW_lb_connection_param
    | Some Uploads -> !M.cW_lb_uploads
    | Some Downloads -> !M.cW_lb_downloads
    | Some Sources -> !M.cW_lb_sources
    | Some Peers -> !M.cW_lb_peers
    | Some Servers -> !M.cW_lb_servers
    | Some Overnet -> !M.cW_lb_overnet
    | Some Tracker -> !M.cW_lb_tracker
    | Some Others -> !M.cW_lb_other_
    | None -> ""

(*************************************************************************)
(*                                                                       *)
(*                         icon_from_section                             *)
(*                                                                       *)
(*************************************************************************)

let icon_from_section section =
  match section with
      Some Main -> A.get_icon ~icon:M.icon_type_source_normal ~size:A.LARGE ()
    | Some Interfaces -> A.get_icon ~icon:M.icon_menu_interfaces ~size:A.LARGE ()
    | Some Mlchat -> A.get_icon ~icon:M.icon_menu_mlchat ~size:A.LARGE ()
    | Some Other -> A.get_icon ~icon:M.icon_menu_others ~size:A.LARGE ()
    | Some Tools -> A.get_icon ~icon:M.icon_menu_tools ~size:A.LARGE ()
    | Some Bittorrent -> A.get_icon ~icon:M.icon_net_bittorrent ~size:A.LARGE ()
    | Some Direct_connect -> A.get_icon ~icon:M.icon_net_dc ~size:A.LARGE ()
    | Some Donkey -> A.get_icon ~icon:M.icon_net_ed2k ~size:A.LARGE ()
    | Some Fasttrack -> A.get_icon ~icon:M.icon_net_fasttrack ~size:A.LARGE ()
    | Some Filetp -> A.get_icon ~icon:M.icon_net_filetp ~size:A.LARGE ()
    | Some Gnutella -> A.get_icon ~icon:M.icon_net_gnutella2 ~size:A.LARGE ()
    | Some Open_napster -> A.get_icon ~icon:M.icon_net_napster ~size:A.LARGE ()
    | Some Soulseek -> A.get_icon ~icon:M.icon_net_soulseek ~size:A.LARGE ()
    | _ -> A.get_icon ~icon:M.icon_mime_unknown ~size:A.LARGE ()

(*************************************************************************)
(*                                                                       *)
(*                         save_gui_options                              *)
(*                                                                       *)
(*************************************************************************)

let gui_pref_reg = Str.regexp_string "gtk_"


let save_gui_options list pv =
  let l = ref [] in
  List.iter (fun (name, v) ->
    try
      (set_simple_option O.gui_ini name v; raise Exit)
    with Exit ->
      (List.iter (fun opt_section ->
         let section = Options.section_name opt_section in
         try
           List.iter (fun o ->
             if o.option_name = name then
               (l := o :: !l; raise Exit) 
           ) (Options.strings_of_section_options "" opt_section)
         with _ -> ()
       ) (Options.sections O.gui_ini);
       if !!verbose then lprintf' "MLgui option saved\n   name: %s\n   value: %s\n" name v)
       | _ ->
      (if !!verbose then lprintf' "Exception in saving MLgui option\n   name: %s\n   value: %s" name v)
  ) list;
  update_preferences !l pv

(*************************************************************************)
(*                                                                       *)
(*                         save_core_options                              *)
(*                                                                       *)
(*************************************************************************)

let save_core_options list =
  let module P = GuiProto in
  try
    GuiCom.send (P.SaveOptions_query list)
  with _ ->
    if !!verbose then lprintf' "ERROR SAVING OPTIONS (but port/password/host correctly set for GUI)"

(*************************************************************************)
(*                                                                       *)
(*                         save_options                                  *)
(*                                                                       *)
(*************************************************************************)

let save_options list (pv : (string, (gui_section, gui_subsection, gui_group) preference) Hashtbl.t) =
  let core_list = ref [] in
  let gui_list = ref [] in
  List.iter (fun (name, v) ->
    if Str.string_match gui_pref_reg name 0
      then gui_list := (name, v) :: !gui_list
      else core_list := (name, v) :: !core_list
  ) list;
  save_core_options !core_list;
  save_gui_options !gui_list pv

(*************************************************************************)
(*                                                                       *)
(*                         Config module                                 *)
(*                                                                       *)
(*************************************************************************)

module Config = ConfigWindow.ConfigPanel (struct

      type section    = gui_section
      and  subsection = gui_subsection
      and  group      = gui_group

      let section_to_label    = section_to_label
      let subsection_to_label = subsection_to_label
      let group_to_label      = group_to_label
      let icon_from_section   = icon_from_section
      let advanced_mode       = advanced
      let save_options        = save_options

    end)


(*************************************************************************)
(*                                                                       *)
(*                         add_option                                    *)
(*                                                                       *)
(*************************************************************************)


let add_option (section, o) = add_preference (section, o) Config.preferences_values

(*************************************************************************)
(*                                                                       *)
(*                         update_options                                *)
(*                                                                       *)
(*************************************************************************)

let update_options options_list = update_preferences options_list Config.preferences_values

(*************************************************************************)
(*                                                                       *)
(*                         load_gui_options                              *)
(*                                                                       *)
(*************************************************************************)

let load_gui_options () =
  List.iter (fun opt_section ->
    let section = Options.section_name opt_section in
    List.iter (fun o ->
      add_option (section, o)
    ) (Options.strings_of_section_options "" opt_section)
  ) (Options.sections O.gui_ini)

(*************************************************************************)
(*                                                                       *)
(*                         clear                                         *)
(*                                                                       *)
(*************************************************************************)

let clear =
  (fun _ ->
     Config.clear ();
     load_gui_options ())

(*************************************************************************)
(*                                                                       *)
(*                         config_window                                 *)
(*                                                                       *)
(*************************************************************************)

let rec iter items =
  match items with
      [] -> ()
    | (GTreeDirectory tree) :: tail ->
        begin
          tree.g_file_tree_pixb <- GuiFriends.folder_closed ();
          iter tail
        end
    | (GTreeFile r) :: tail ->
        begin
          r.res_network_pixb <- Mi.network_pixb r.res_num ~size:A.SMALL ();
          r.res_name_pixb <- Mi.file_type_of_name r.res_name ~size:A.SMALL;
          iter tail
        end

let config_window gui value_reader f =
  let use_icons = !!O.gtk_look_use_icons in
  let use_avail_bars = !!O.gtk_look_graphical_availability in
  let language = !!O.gtk_client_lang in
  let saturation = !!O.gtk_look_icon_saturation in
  let small_icon = !!O.gtk_look_lists_icon_size in
  let medium_icon = !!O.gtk_look_toolbars_icon_size in
  let large_icon = !!O.gtk_look_main_toolbar_icon_size in
  let (hostname, port) = (!!O.gtk_client_hostname, !!O.gtk_client_port) in
  let avail_max = !!O.gtk_misc_availability_max in
  let use_avail_height = !!O.gtk_misc_use_availability_height in
  let on_ok () =
    G.get_metrics_from_gtk_font_list ();
    (if (hostname, port) <> (!!O.gtk_client_hostname, !!O.gtk_client_port)
       then GuiCom.reconnect gui value_reader gui BasicSocket.Closed_by_user);
    (if use_icons <> !!O.gtk_look_use_icons ||
        saturation <> !!O.gtk_look_icon_saturation ||
        small_icon <> !!O.gtk_look_lists_icon_size ||
        medium_icon <> !!O.gtk_look_toolbars_icon_size ||
        large_icon <> !!O.gtk_look_main_toolbar_icon_size
      then A.clean_icons ());
    (if use_avail_bars <> !!O.gtk_look_graphical_availability ||
        use_avail_height <> !!O.gtk_misc_use_availability_height
      then begin
        A.clean_avail_bars [];
        GuiDownloads.downloadstore#force_update_avail_bars ()
      end);
    (if avail_max <> !!O.gtk_misc_availability_max
      then begin
        A.clean_avail_bars [];
        A.create_color_blue_relative ();
        GuiDownloads.downloadstore#force_update_avail_bars ()
      end);
    (if use_icons <> !!O.gtk_look_use_icons ||
        small_icon <> !!O.gtk_look_lists_icon_size ||
        saturation <> !!O.gtk_look_icon_saturation
      then begin
        GuiDownloads.downloadstore#force_update_icons ();
        GuiServers.serverstore#force_update_icons ();
        GuiFriends.friendstore#force_update_icons ();
        Hashtbl.iter (fun _ qr ->
          qr.GuiQueries.g_query_result#force_update_icons ()
        ) GuiQueries.qresults;
        GuiRooms.roomstore#force_update_icons ();
        GuiUploads.uploadstore#force_update_icons ();
        GuiUploads.uploaderstore#force_update_icons ();
        Hashtbl.iter (fun _ r ->
          r.res_network_pixb <- Mi.network_pixb r.res_num ~size:A.SMALL ();
          r.res_name_pixb <- Mi.file_type_of_name r.res_name ~size:A.SMALL;
        ) GuiGlobal.results;
        Hashtbl.iter (fun _ s ->
          match s.source_files with
              None -> ()
            | Some ft ->
                begin
                  ft.g_file_tree_pixb <- GuiFriends.folder_closed ();
                  iter ft.g_file_tree_list
                end
        ) GuiGlobal.sources;
      end);
(*
    (if language <> !!O.gtk_client_lang
      then begin
        let s = U.lang_to_string !!O.gtk_client_lang in
        let ext = String.lowercase s in
        Printf.printf "gtk_client_lang %s\nlanguage %s\next %s\n"
          s (U.lang_to_string language) ext;
        flush stdout;
        Gettext.set_strings_file M.filename ~ext ();
        M.load_messages ();
        GuiStatusBar.update_labels ();
        Hashtbl.iter (fun _ p ->
          p.pref_help <- U.simple_utf8_of (Gettext._s p.opt_section p.pref_help)
        ) Config.preferences_values
      end);
*)
    gui.wtool#empty ();
    f gui;
    gui.update_current_page ();
  in
  Config.config_window ~on_ok


let _ =
  (try Options.load O.gui_ini with _ -> ());
  (try Options.save O.gui_ini with _ -> ());
  let args = Options.simple_args "" O.gui_ini in
  Arg.parse args (Arg.usage args)  "mlgui: the GUI to use with mldonkey";
  load_gui_options () (* ;
  match !!O.gtk_client_lang with
      U.EN -> ()
    | _ ->
      begin
        let s = U.lang_to_string !!O.gtk_client_lang in
        let ext = String.lowercase s in
        Gettext.set_strings_file M.filename ~ext ();
        M.load_messages ()
      end
*)
