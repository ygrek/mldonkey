(* Copyright 2004 b8_bavard INRIA *)
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

(* Options of the Gui. *)

open Gettext
open Options
open CommonGlobals
open GraphTypes

module U = GuiUtf8

let _s x = _s "GuiOptions" x
let _b x = _b "GuiOptions" x  
    
let define_option a b ?desc c d e = 
  match desc with
    None -> define_option a b (_s c) d e
  | Some desc -> define_option a b ~desc: (_s desc) (_s c) d e
let define_expert_option a b ?desc c d e = 
  match desc with
    None -> define_expert_option a b (_s c) d e
  | Some desc -> define_expert_option a b ~desc: (_s desc) (_s c) d e

module M = GuiMessages
module C = GuiColumns

let _ = Unix2.safe_mkdir GuiMessages.gui_config_dir

let gui_ini = create_options_file
    (Filename.concat GuiMessages.gui_config_dir "mlgui.ini")



let tb_styles = [ "both", `BOTH ;
                  "both horizontal", `BOTH_HORIZ ; 
                  "text", `TEXT ;
                  "icon", `ICONS ]

let tb_styles_rev = List.map (fun (a,b) -> (b,a)) tb_styles

let string_to_tbstyle s =
  try List.assoc (String.lowercase s) tb_styles
  with Not_found -> `BOTH

let tbstyle_to_string st = List.assoc st tb_styles_rev

let value_to_tbstyle v =
  match v with
    StringValue s -> string_to_tbstyle s
  | _ -> raise Not_found

let tbstyle_to_value (st:Gtk.Tags.toolbar_style) =
  StringValue (tbstyle_to_string st)

let (tbstyle_option : Gtk.Tags.toolbar_style option_class) = 
    define_option_class "Toolbar" 
    value_to_tbstyle tbstyle_to_value

let graphtime_to_string gt =
  match gt with
      GraphHour -> "hour"
    | GraphHalfDay -> "half_day"
    | GraphDay -> "day"
    | GraphWeek -> "week"
    | GraphMonth -> "month"
    | GraphYear -> "year"
    | _ -> "quarter"

let string_to_graphtime s =
  match s with
      "hour" -> GraphHour
    | "half_day" -> GraphHalfDay
    | "day" -> GraphDay
    | "week" -> GraphWeek
    | "month" -> GraphMonth
    | "year" -> GraphYear
    | _ -> GraphQuarter

let value_to_graphtime v =
  match v with
      StringValue s -> string_to_graphtime s
    | _ -> GraphQuarter

let graphtime_to_value (gt : graph_time) =
  StringValue (graphtime_to_string gt)

let (graphtime_option : graph_time option_class) =
    define_option_class "GraphTime" 
    value_to_graphtime graphtime_to_value

let time_option =
    define_option_class "Time"
    value_to_int int_to_value

let advanced_option =
    define_option_class "Advanced"
    value_to_bool bool_to_value

let password_option =
    define_option_class "Password"
    value_to_string string_to_value

let scale_option =
    define_option_class "Scale"
    value_to_float float_to_value

let main_section = file_section gui_ini ["Main"] "Main options"
let mlgui_section = file_section gui_ini ["MLgui"] "Options to control MLgui"
let mlgui_debug_section = file_section gui_ini ["MLguiDebug"] "Debug Options"

(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(*                                                                       *)
(*               Options displayed in the config panel                   *)
(*                                                                       *)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)

let current_section = main_section

let gtk_advanced_options = define_option current_section
    ["gtk_advanced_options"]
    ~desc:"Advanced options"
    "Set to true whether you want to access to the advanced options"
    advanced_option false


let current_section = mlgui_section

(* {Connection} *)

let gtk_connection_http_proxy_server = define_option current_section
    ["gtk_connection_http_proxy_server"]
    ~desc:"HTTP proxy server"
    "Direct HTTP queries to HTTP proxy"
    string_option ""

let gtk_connection_http_proxy_port = define_option current_section
    ["gtk_connection_http_proxy_port"]
    ~desc:"HTTP proxy server port"
    "Port of HTTP proxy"
    port_option 8080

let gtk_connection_http_use_proxy = define_option current_section
    ["gtk_connection_http_use_proxy"]
    ~desc:"Enable Proxy server"
    "Direct TCP connections to HTTP proxy (the proxy should support CONNECT)"
    bool_option false

let gtk_connection_http_proxy_login = define_option current_section
    ["gtk_connection_http_proxy_login"]
    "HTTP proxy login (leave empty if proxy doesn't require authentication)"
    string_option ""

let gtk_connection_http_proxy_password = define_option current_section
    ["gtk_connection_http_proxy_password"]
    "HTTP proxy password"
    string_option ""

(* {Client} *)

let gtk_client_login = define_option current_section
    ["gtk_client_login"]
    ~desc:"Login"
    "Your login name (default is admin)"
    string_option "admin"

let gtk_client_password = define_option current_section
    ["gtk_client_password"]
    ~desc:"Password"
    "The password to use when connecting to the server"
    password_option ""

let gtk_client_port = define_option current_section
    ["gtk_client_port"]
    ~desc:"MLgui port"
    "The server port to connect to"
    port_option 4001

let gtk_client_hostname = define_option current_section
    ["gtk_client_hostname"]
    ~desc:"Hostname"
    "The server hostname to connect to"
    string_option "localhost"

let gtk_client_lang = define_option current_section
    ["gtk_client_lang"]
    ~desc:"Language"
    "The language you want to use in MLgui"
    U.language_option U.EN

let gtk_client_history = define_expert_option current_section 
    ["gtk_client_history"]
    ~desc:"History"
    "History of connected cores"
    (list_option (tuple2_option (string_option, int_option))) []



(* {Look & Feel} *)

let gtk_look_use_size_suffixes = define_option current_section
    ["gtk_look_use_size_suffixes"]
    ~desc:"Use size suffixes (G, M, k)"
    "Whether sizes are printed using G(iga), M(ega) and k(ilo) suffixes." 
    bool_option true

let gtk_look_use_icons = define_option current_section 
    ["gtk_look_use_icons"]
    ~desc:"Use icons in the lists"
    "Whether icons are displayed in MLgui"
    bool_option true

let gtk_look_graphical_availability = define_option current_section 
    ["gtk_look_graphical_availability"]
    ~desc:"Use graphical represention for availability"
    "What is displayed in availability column : graphical or text"
    bool_option true

let gtk_look_icons_directory = define_option current_section
    ["gtk_look_icons_directory"]
    ~desc:"Icons themes"
    "The directory where mldonkey gui's icons are"
    path_option [""]

let gtk_look_main_toolbar_icon_size = define_option current_section
    ["gtk_look_main_toolbar_icon_size"]
    ~desc:"Icons size in the main toolbar"
    "The size of the icons in the main toolbar"
    int_option 32

let gtk_look_toolbars_icon_size = define_option current_section
    ["gtk_look_toolbars_icon_size"]
    ~desc:"Icons size in the other toolbars"
    "The size of the icons in the other toolbars"
    int_option 22

let gtk_look_lists_icon_size = define_option current_section
    ["gtk_look_lists_icon_size"]
    ~desc:"Icons size in the lists"
    "The size of the icons in the lists"
    int_option 16

let gtk_look_icon_saturation = define_option current_section
    ["gtk_look_icon_saturation"]
    ~desc:"Icons saturation"
    "The level of saturation when diplaying icons"
    scale_option 1.

let gtk_look_toolbars_style = define_option current_section
    ["gtk_look_toolbars_style"]
    ~desc:"Style of toolbars"
    "What is displayed in toolbar buttons : text, icon or both"
    tbstyle_option `BOTH



(* {Colors} *)

let gtk_color_default = define_option current_section
    ["gtk_color_default"]
    ~desc:"Default color"
    "Set the default color in MLgui"
    color_option "Black"

let gtk_color_state_not_available = define_option current_section
    ["gtk_color_state_not_available"]
    ~desc:"State not available"
    "Color for unavailable files"
    color_option "Red"

let gtk_color_state_files_listed = define_option current_section 
    ["gtk_color_state_files_listed"]
    ~desc:"State files listed"
    "Color for users whose list of files has been retrieved"
    color_option "Blue"

(* {Fonts} *)

let gtk_font_list = define_option current_section
    ["gtk_font_list"]
    ~desc:"Lists and trees font"
    "Font for the list and trees texts"
    font_option "sans 12"

let gtk_font_networks = define_option current_section
    ["gtk_font_network"]
    ~desc:"Networks labels font"
    "Font for the networks labels in the Networks Tab"
    font_option "sans 16"


(* {2 Graph Options} *)

let gtk_graph_time_downloads =  define_option current_section
    ["gtk_graph_time_downloads"]
    ~desc:"Time range to view the global downloads"
    "Set the time range for the downloads graph"
    graphtime_option GraphQuarter

let gtk_graph_time_uploads =  define_option current_section
    ["gtk_graph_time_uploads"]
    ~desc:"Time range to view the global uploads"
    "Set the time range for the uploads graph"
    graphtime_option GraphQuarter

let gtk_graph_time_file =  define_option current_section
    ["gtk_graph_time_file"]
    ~desc:"Time range to view one-file downloads and uploads"
    "Set the time range to display the uploads and downloads of one file in the graph tab"
    graphtime_option GraphQuarter

let gtk_graph_font = define_option current_section
    ["gtk_graph_font"]
    ~desc:"Font"
    "Set the font to display texts in both the uploads and downloads graphs" 
    font_option "sans 7"

let gtk_graph_background =  define_option current_section 
    ["gtk_graph_background"]
    ~desc:"Background color"
    "Set the background color for both the uploads and downloads graphs" 
    color_option "#000000"

let gtk_graph_grid =  define_option current_section 
    ["gtk_graph_grid"]
    ~desc:"Grid color"
    "Set the color of the grid for both the uploads and downloads graphs" 
    color_option "#484848"

let gtk_graph_download = define_option current_section 
    ["gtk_graph_download"]
    ~desc:"Downloads color"
    "Set the foreground color of the download rate" 
    color_option "#83afff"

let gtk_graph_upload = define_option current_section 
    ["gtk_graph_upload"]
    ~desc:"Uploads color"
    "Set the foreground color of the upload rate"
    color_option "#6eec8b"


(* {Misc} *)

let gtk_misc_relative_availability = define_option current_section 
    ["gtk_misc_relative_availability"]
    ~desc:"Use relative % availability"
    "Calculate % availability ignoring already present chunks"
    bool_option true

let gtk_misc_files_auto_expand_depth = define_option current_section 
    ["gtk_misc_files_auto_expand_depth"]
    ~desc:"Files auto-expand depth"
    "The depth to which the directories of a friend are automatically expanded"
    int_option 3

let gtk_misc_use_availability_height = define_option current_section 
    ["gtk_misc_use_availability_height"]
    ~desc:"Use height encoded availability"
    "Display the availability of a chunk as height or color coded bar"
    bool_option true

let gtk_misc_availability_max = define_expert_option current_section 
    ["gtk_misc_availability_max"]
    ~desc:"Max availability"
    "If use_availability_height is true, which availability corresponds to a full bar ?"
    int_option 10

let gtk_misc_compaction_overhead = define_expert_option current_section 
    ["gtk_misc_compaction_overhead"]
    ~desc:"Compaction overhead"
    "The percentage of free memory before a compaction is triggered"
    int_option 50

let gtk_misc_interface_buffer = define_expert_option current_section
    ["gtk_misc_interface_buffer"]
    ~desc:"Interface buffer"
    "The size of the buffer to the core"
    int_option 10000000

let gtk_misc_copy_messages = define_expert_option current_section
    ["gtk_misc_copy_messages"]
    ~desc:"Copy messages"
    "For bundle binaries, should we directly pass structures between the core and the GUI (faster), or copy them (fewer bugs)" 
    bool_option true

(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(*                                                                       *)
(*              Options not displayed in the config panel                *)
(*                                                                       *)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)

(* {Layout} *)

let servers_vpane_up = define_option current_section
    ["layout";"servers_vpane_up"]
    "Size in % of upper part of the servers hpane"
    float_option 0.50

let friends_hpane_left = define_option current_section
    ["layout"; "friends_hpane_left"]
    "Size in % of left part of the friends hpane"
    float_option 0.17

let friends_vpane_up = define_option current_section
    ["layout"; "friends_vpane_up"]
    "Size in % of up part of the friends vpane"
    float_option 0.69

let friends_hpane_dirs = define_option current_section
    ["layout"; "friends_hpane_dirs"]
    "Size in % of the directories part of the files box"
    float_option 0.13

let rooms_hpane_left = define_option current_section
    ["layout"; "rooms_hpane_left"]
    "Size in % of left part of the rooms hpane"
    float_option 0.30

let rooms_hpane2_left = define_option current_section
    ["layout"; "rooms_hpane2_left"]
    "Size in % of left part of the second rooms hpane"
    float_option 0.50

let queries_hpane_left = define_option current_section
    ["layout"; "queries_hpane_up"]
    "Size in % of left part of the queries hpane"
    float_option 0.20

let uploads_vpane_up = define_option current_section
    ["layout"; "uploads_vpane_up"]
    "Size in % of up part of the uploads vpane"
    float_option 0.50

let im_room_hpane = define_option current_section
    ["layout"; "im_room_hpane"]
    "Size in % of the left part of the identities hpane"
    float_option 0.80

let last_tab = define_option current_section
    ["layout"; "last_tab"]
    "The last tab opened before closing the GUI" 
    int_option 0

(* {List columns} *)

let downloads_columns = define_option current_section
    ["Colums"; "downloads_columns"] 
    "Columns for the files being downloaded"
    (list_option (tuple2_option (C.File.column_option, float_option)))
  [
   (C.Col_file_network      , 0.1);
   (C.Col_file_priority     , 0.1);
   (C.Col_file_name         , 0.1);
   (C.Col_file_availability , 0.1);
   (C.Col_file_size         , 0.1);
   (C.Col_file_downloaded   , 0.1);
   (C.Col_file_percent      , 0.1);
   (C.Col_file_rate         , 0.1);
   (C.Col_file_state        , 0.1);
   (C.Col_file_eta          , 0.1);
   (C.Col_file_age          , 0.1);
  ]

let friends_columns = define_option current_section
    ["Colums"; "friends_columns"] 
    "Columns for the friends"
    (list_option (tuple2_option (C.Friend.column_option, float_option)))
  [
   (C.Col_friend_network , 0.1);
   (C.Col_friend_name    , 0.1);
  ]

let friends_results_columns = define_option current_section
    ["Colums"; "friends_results_columns"] 
    "Columns for the results of friends files"
    (list_option (tuple2_option (C.Result.column_option, float_option)))
  [
   (C.Col_result_network      , 0.1);
   (C.Col_result_name         , 0.1);
   (C.Col_result_size         , 0.1);
   (C.Col_result_format       , 0.1);
   (C.Col_result_duration     , 0.1);
   (C.Col_result_codec        , 0.1);
   (C.Col_result_bitrate      , 0.1);
   (C.Col_result_availability , 0.1);
   (C.Col_result_comment      , 0.1);
  ]

let friends_dirs_columns = define_option current_section
    ["Colums"; "friends_dirs_columns"] 
    "Columns for the folders of friends files"
    (list_option (tuple2_option (C.Directory.column_option, float_option)))
  [
   (C.Col_dir_name , 0.1);
  ] 

let uploaders_columns = define_option current_section
    ["Colums"; "file_locations_columns"] 
    "Columns for the uploaders"
    (list_option (tuple2_option (C.Client.column_option, float_option)))
  [
   (C.Col_client_network      , 0.1);
   (C.Col_client_name         , 0.1);
   (C.Col_client_kind         , 0.1);
   (C.Col_client_state        , 0.1);
   (C.Col_client_rating       , 0.1);
   (C.Col_client_connect_time , 0.1);
   (C.Col_client_software     , 0.1);
   (C.Col_client_downloaded   , 0.1);
   (C.Col_client_uploaded     , 0.1);
   (C.Col_client_upload       , 0.1);
  ]

let rooms_columns = define_option current_section
    ["Colums"; "rooms_columns"] 
    "Columns of the room lists"
    (list_option (tuple2_option (C.Room.column_option, float_option)))
  [
   (C.Col_room_network , 0.1);
   (C.Col_room_name    , 0.1);
   (C.Col_room_nusers  , 0.1);
  ]

let rooms_users_columns = define_option current_section
    ["Colums"; "rooms_users_columns"]
    "Columns of the rooms users lists"
    (list_option (tuple2_option (C.User.column_option, float_option)))
  [
   (C.Col_user_name , 0.1);
   (C.Col_user_addr , 0.1);
   (C.Col_user_tags , 0.1);
   (C.Col_user_md4  , 0.1);
  ]

let servers_columns = define_option current_section
    ["Colums"; "server_columns"] 
    "Columns for the servers"
    (list_option (tuple2_option (C.Server.column_option, float_option)))
  [
   (C.Col_server_network  , 0.1);
   (C.Col_server_preferred, 0.1);
   (C.Col_server_name     , 0.1);
   (C.Col_server_address  , 0.1);
   (C.Col_server_state    , 0.1);
   (C.Col_server_users    , 0.1);
   (C.Col_server_files    , 0.1);
   (C.Col_server_desc     , 0.1);
  ]

let servers_users_columns = define_option current_section
    ["Colums"; "servers_users_columns"]
    "Columns of the servers users lists"
    (list_option (tuple2_option (C.User.column_option, float_option)))
  [
   (C.Col_user_name , 0.1);
   (C.Col_user_addr , 0.1);
   (C.Col_user_tags , 0.1);
   (C.Col_user_md4  , 0.1);
  ]

let results_columns = define_option current_section
    ["Colums"; "results_columns"] 
    "Columns for the results of searches and files of a friends"
    (list_option (tuple2_option (C.Result.column_option, float_option)))
  [
   (C.Col_result_network      , 0.1);
   (C.Col_result_name         , 0.1);
   (C.Col_result_size         , 0.1);
   (C.Col_result_format       , 0.1);
   (C.Col_result_duration     , 0.1);
   (C.Col_result_codec        , 0.1);
   (C.Col_result_bitrate      , 0.1);
   (C.Col_result_availability , 0.1);
   (C.Col_result_comment      , 0.1);
  ]

let shared_files_up_columns = define_option current_section
    ["Colums"; "shared_files_up_columns"] 
    "Columns for the list of shared files upload information"
    (list_option (tuple2_option (C.Shared_files_up.column_option, float_option)))
  [
   (C.Col_shared_network  , 0.1);
   (C.Col_shared_file     , 0.1);
   (C.Col_shared_size     , 0.1);
   (C.Col_shared_requests , 0.1);
   (C.Col_shared_upsize   , 0.1);
   (C.Col_shared_uid      , 0.1);
  ] 

let account_columns = define_option current_section
    ["Colums"; "account_columns"] 
    "Columns for the Accounts in the IM interface"
    (list_option (tuple2_option (C.IMAccount.column_option, float_option)))
  [
   (C.Col_account_name     , 0.1);
   (C.Col_account_status   , 0.1);
   (C.Col_account_protocol , 0.1);
  ]

let identities_columns = define_option current_section
    ["Colums"; "identities_columns"] 
    "Columns for the Identities in the IM interface"
    (list_option (tuple2_option (C.IMIdentities.column_option, float_option)))
  [
   (C.Col_identity_name , 0.1);
  ] 


(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(*                                                                       *)
(*              Debug Options                                            *)
(*                                                                       *)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)

let current_section = mlgui_debug_section

let gtk_verbose_gview = define_expert_option current_section
    ["gtk_verbose_gview"]
    ~desc:"verbose Gview"
    "Debug module GuiTemplates.Gview"
    bool_option false

let gtk_verbose_chat = define_expert_option current_section
    ["gtk_verbose_chat"]
    ~desc:"verbose Chat"
    "Debug module GuiTemplates.Chat"
    bool_option false

let gtk_verbose_tools = define_expert_option current_section
    ["gtk_verbose_tools"]
    ~desc:"verbose tools"
    "Debug module GuiTools"
    bool_option false

let gtk_verbose_configwin = define_expert_option current_section
    ["gtk_verbose_configwin"]
    ~desc:"verbose configwin"
    "Debug module ConfigWindow"
    bool_option false

let gtk_verbose_art = define_expert_option current_section
    ["gtk_verbose_art"]
    ~desc:"verbose art"
    "Debug module GuiArt"
    bool_option false

let gtk_verbose_main = define_expert_option current_section
    ["gtk_verbose_main"]
    ~desc:"verbose main"
    "Debug module GuiMain"
    bool_option false

let gtk_verbose_networks = define_expert_option current_section
    ["gtk_verbose_networks"]
    ~desc:"verbose networks"
    "Debug module GuiNetworks"
    bool_option false

let gtk_verbose_servers = define_expert_option current_section
    ["gtk_verbose_servers"]
    ~desc:"verbose servers"
    "Debug module GuiServers"
    bool_option false

let gtk_verbose_downloads = define_expert_option current_section
    ["gtk_verbose_downloads"]
    ~desc:"verbose downloads"
    "Debug module GuiDownloads"
    bool_option false

let gtk_verbose_friends = define_expert_option current_section
    ["gtk_verbose_friends"]
    ~desc:"verbose friends"
    "Debug module GuiFriends"
    bool_option false

let gtk_verbose_queries = define_expert_option current_section
    ["gtk_verbose_queries"]
    ~desc:"verbose queries"
    "Debug module GuiQueries"
    bool_option false

let gtk_verbose_rooms = define_expert_option current_section
    ["gtk_verbose_rooms"]
    ~desc:"verbose rooms"
    "Debug module GuiRooms"
    bool_option false

let gtk_verbose_uploads = define_expert_option current_section
    ["gtk_verbose_uploads"]
    ~desc:"verbose uploads"
    "Debug module GuiUploads"
    bool_option false

let gtk_verbose_console = define_expert_option current_section
    ["gtk_verbose_console"]
    ~desc:"verbose console"
    "Debug module GuiConsole"
    bool_option false

let gtk_verbose_graphbase = define_expert_option current_section
    ["gtk_verbose_graphbase"]
    ~desc:"verbose graphbase"
    "Debug module GuiGraphBase"
    bool_option false

let gtk_verbose_graph = define_expert_option current_section
    ["gtk_verbose_graph"]
    ~desc:"verbose graph"
    "Debug module GuiGraph"
    bool_option false

let gtk_verbose_im = define_expert_option current_section
    ["gtk_verbose_im"]
    ~desc:"verbose im"
    "Debug module GuiIm"
    bool_option false

let gtk_verbose_settings = define_expert_option current_section
    ["gtk_verbose_settings"]
    ~desc:"verbose settings"
    "Debug module GuiConfig"
    bool_option false



let _ =
  option_hook gtk_misc_compaction_overhead (fun _ ->
      let gc_control = Gc.get () in
      Gc.set { gc_control with Gc.max_overhead = !!gtk_misc_compaction_overhead };     
  )
