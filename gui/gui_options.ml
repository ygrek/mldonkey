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

(** Options of the Gui. *)

(*
  type modifier =
    [ `SHIFT|`LOCK|`CONTROL|`MOD1|`MOD2|`MOD3|`MOD4|`MOD5|`BUTTON1
     |`BUTTON2|`BUTTON3|`BUTTON4|`BUTTON5 ]
*)

open Options

module M = Gui_messages
module C = Gui_columns

let mldonkey_gui_ini = create_options_file 
    (Filename.concat Sysenv.home ".mldonkey_gui.ini")
  
module KeyOption = struct
    
    let name_to_keysym = 
      ("Button1", Gui_keys.xk_Pointer_Button1) ::
      ("Button2", Gui_keys.xk_Pointer_Button2) ::
      ("Button3", Gui_keys.xk_Pointer_Button3) ::
      ("Button4", Gui_keys.xk_Pointer_Button4) ::
      ("Button5", Gui_keys.xk_Pointer_Button5) ::
      Gui_keys.name_to_keysym
    
    let string_to_key s =
      let mask = ref [] in
      let key = try
          let pos = String.rindex s '-' in
          for i = 0 to pos - 1 do 
            let m = match s.[i] with
                'C' -> `CONTROL
              | 'S' -> `SHIFT
              | 'L' -> `LOCK 
              | 'M' -> `MOD1
              | 'A' -> `MOD1
              | '1' -> `MOD1
              | '2' -> `MOD2
              | '3' -> `MOD3
              | '4' -> `MOD4
              | '5' -> `MOD5
              | _ -> raise Not_found
            in
            mask := m :: !mask
          done;
          String.sub s (pos+1) (String.length s - pos - 1)
        with _ -> 
            s
      in
      !mask, List.assoc key name_to_keysym
      
    let key_to_string (m, k) =
      let s = List.assoc k Gui_keys.keysym_to_name in
      match m with
        [] -> s
      | _ ->
          let rec iter m s =
            match m with
              [] -> s
            | c :: m ->
                iter m ((
                    match c with
                      `CONTROL -> "C"
                    | `SHIFT -> "S"
                    | `LOCK -> "L"
                    | `MOD1 -> "A"
                    | `MOD2 -> "2"
                    | `MOD3 -> "3"
                    | `MOD4 -> "4"
                    | `MOD5 -> "5"
                    | _  -> raise Not_found
                  ) ^ s)
          in
          iter m ("-" ^ s)
    
    let value_to_key v =
      match v with
        StringValue s -> string_to_key s
      | _ -> raise Not_found
    
    let key_to_value k =
      StringValue (key_to_string k)
    
    let (t : (Gdk.Tags.modifier list * int) option_class) = 
      define_option_class "Key" value_to_key key_to_value
      
  end

(** {2 Key mappings} *)

let keymap_global = define_option mldonkey_gui_ini ["keymaps"; "global"]
    "Global key bindings" 
  (list_option (tuple2_option (KeyOption.t, string_option))) []

let keymap_servers = define_option mldonkey_gui_ini ["keymaps"; "servers"]
  "Key bindings for servers tab"   
  (list_option (tuple2_option (KeyOption.t, string_option))) []

let keymap_downloads = define_option mldonkey_gui_ini ["keymaps"; "downloads"]
  "Key bindings for downloads tab" 
  (list_option (tuple2_option (KeyOption.t, string_option))) []

let keymap_friends = define_option mldonkey_gui_ini ["keymaps"; "friends"]
  "Key bindings for friends tab" 
  (list_option (tuple2_option (KeyOption.t, string_option))) []

let keymap_queries = define_option mldonkey_gui_ini ["keymaps"; "queries"]
  "Key bindings for queries tab" 
  (list_option (tuple2_option (KeyOption.t, string_option))) []

let keymap_results = define_option mldonkey_gui_ini ["keymaps"; "results"]
  "Key bindings for results tab" 
  (list_option (tuple2_option (KeyOption.t, string_option))) []

let keymap_console = define_option mldonkey_gui_ini ["keymaps"; "console"]
  "Key bindings for console tab" 
    (list_option (tuple2_option (KeyOption.t, string_option))) []

let add_binding map binding action = 
  map =:= 
    (KeyOption.string_to_key binding, action) :: !!map
  
(** {2 Colors} *)

let color_default = define_option mldonkey_gui_ini ["colors"; "default"]
    M.h_col_default string_option "Black"
let color_downloaded =  define_option mldonkey_gui_ini ["colors"; "downloaded"]
    M.h_col_downloaded string_option "Blue"
let color_downloading =  define_option mldonkey_gui_ini ["colors"; "downloading"]
    M.h_col_downloading string_option "DarkGreen"
let color_available =  define_option mldonkey_gui_ini ["colors"; "available"]
    M.h_col_avail string_option "Orange"
let color_not_available =  define_option mldonkey_gui_ini ["colors"; "not_available"]
    M.h_col_not_avail string_option "Red"
let color_connected =  define_option mldonkey_gui_ini ["colors"; "connected"]
    M.h_col_connected string_option "DarkGreen"
let color_not_connected =  define_option mldonkey_gui_ini ["colors"; "not_connected"]
    M.h_col_not_connected string_option "Black"
let color_connecting =  define_option mldonkey_gui_ini ["colors"; "connecting"]
    M.h_col_connecting string_option "Orange"
let color_files_listed =  define_option mldonkey_gui_ini ["colors"; "files_listed"]
    M.h_col_files_listed string_option "Blue"

(** {2 Icons} *)

let xpm_label s = ["icons" ; s]
let xpm_remove = define_option mldonkey_gui_ini (xpm_label M.o_xpm_remove)
    "" filename_option ""
let xpm_cancel = define_option mldonkey_gui_ini (xpm_label M.o_xpm_cancel)
    "" filename_option ""
let xpm_connect = define_option mldonkey_gui_ini (xpm_label M.o_xpm_connect)
    "" filename_option ""
let xpm_disconnect = define_option mldonkey_gui_ini (xpm_label M.o_xpm_disconnect)
    "" filename_option ""
let xpm_view_users = define_option mldonkey_gui_ini (xpm_label M.o_xpm_view_users)
    "" filename_option ""
let xpm_connect_more_servers = define_option mldonkey_gui_ini (xpm_label M.o_xpm_connect_more_servers)
    "" filename_option ""
let xpm_toggle_display_all_servers = define_option mldonkey_gui_ini (xpm_label M.o_xpm_toggle_display_all_servers)
    "" filename_option ""
let xpm_remove_old_servers = define_option mldonkey_gui_ini (xpm_label M.o_xpm_remove_old_servers)
    "" filename_option ""
let xpm_save = define_option mldonkey_gui_ini (xpm_label M.o_xpm_save)
    "" filename_option ""
let xpm_save_all = define_option mldonkey_gui_ini (xpm_label M.o_xpm_save_all)
    "" filename_option ""
let xpm_save_as = define_option mldonkey_gui_ini (xpm_label M.o_xpm_save_as)
    "" filename_option ""
let xpm_edit_mp3 = define_option mldonkey_gui_ini (xpm_label M.o_xpm_edit_mp3)
    "" filename_option ""
let xpm_pause_resume = define_option mldonkey_gui_ini (xpm_label M.o_xpm_pause_resume)
    "" filename_option ""
let xpm_get_format = define_option mldonkey_gui_ini (xpm_label M.o_xpm_get_format)
    "" filename_option ""
let xpm_preview = define_option mldonkey_gui_ini (xpm_label M.o_xpm_preview)
    "" filename_option ""
let xpm_verify_chunks = define_option mldonkey_gui_ini (xpm_label M.o_xpm_verify_chunks)
    "" filename_option ""
let xpm_retry_connect = define_option mldonkey_gui_ini (xpm_label M.o_xpm_retry_connect)
    "" filename_option ""
let xpm_add_to_friends = define_option mldonkey_gui_ini (xpm_label M.o_xpm_add_to_friends)
    "" filename_option ""
let xpm_download = define_option mldonkey_gui_ini (xpm_label M.o_xpm_download)
    "" filename_option ""
let xpm_submit_search = define_option mldonkey_gui_ini (xpm_label M.o_xpm_submit_search)
    "" filename_option ""
let xpm_extend_search = define_option mldonkey_gui_ini (xpm_label M.o_xpm_extend_search)
    "" filename_option ""
let xpm_local_search = define_option mldonkey_gui_ini (xpm_label M.o_xpm_local_search)
    "" filename_option ""
let xpm_find_friend =  define_option mldonkey_gui_ini (xpm_label M.o_xpm_find_friend)
    "" filename_option ""
let xpm_remove_all_friends =  define_option mldonkey_gui_ini (xpm_label M.o_xpm_remove_all_friends)
    "" filename_option ""
let xpm_close_room =  define_option mldonkey_gui_ini (xpm_label M.o_xpm_close_room)
    "" filename_option ""
let xpm_refresh =  define_option mldonkey_gui_ini (xpm_label M.o_xpm_refresh)
    "" filename_option ""

(** {2 Toolbars style} *)

let tb_styles = [ "both", `BOTH ; 
		  "text", `TEXT ;
		  "icon", `ICONS ]

let tb_styles_rev = List.map
    (fun (a,b) -> (b,a)) tb_styles

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

let (class_tbstyle : Gtk.Tags.toolbar_style option_class) = 
      define_option_class "toolbar_style" 
    value_to_tbstyle tbstyle_to_value

let toolbars_style = define_option mldonkey_gui_ini
    ["toolbars_style"] M.h_toolbars_style
    class_tbstyle `BOTH

(** {2 Layout} *)

let servers_hpane_left = define_option mldonkey_gui_ini
    ["layout"; "servers_hpane_left"]
    "Size in % of left part of the servers hpane"
    int_option 80

let rooms_hpane_left = define_option mldonkey_gui_ini
    ["layout"; "rooms_hpane_left"]
    "Size in % of left part of the rooms hpane"
    int_option 80

let downloads_hpane_left = define_option mldonkey_gui_ini
    ["layout"; "downloads_hpane_left"]
  "Size in % of left part of the downloads hpane"
    int_option 80

let friends_hpane_left = define_option mldonkey_gui_ini
  ["layout"; "friends_hpane_left"]
  "Size in % of left part of the friends hpane"
    int_option 30

let searches_hpane_left = define_option mldonkey_gui_ini
    ["layout"; "searches_hpane_left"]
  "Size in % of left part of the searches hpane"
    int_option 40
  
let downloads_vpane_up = define_option mldonkey_gui_ini
    ["layout"; "downloads_vpane_up"]
  "Size in % of up part of the downloads vpane"
    int_option 40

let friends_vpane_up = define_option mldonkey_gui_ini
    ["layout"; "friends_vpane_up"]
    "Size in % of up part of the friends vpane"
    int_option 80

let friends_hpane_dirs = define_option mldonkey_gui_ini
    ["layout"; "friends_hpane_dirs"]
    "Size in % of the directories part of the files box"
    int_option 20

let gui_width = define_option mldonkey_gui_ini
    ["layout"; "width"]
    "Width of GUI window" int_option 600

let gui_height = define_option mldonkey_gui_ini
    ["layout"; "height"]
  "Height of GUI window" int_option 400



(** {2 List columns} *)

(** {3 Files} *)


let downloads_columns = define_option mldonkey_gui_ini
    ["downloads_columns"] M.h_downloads_columns
    (list_option C.File.class_column)
  [ C.Col_file_name ; C.Col_file_size ; C.Col_file_downloaded;
    C.Col_file_percent ;
      C.Col_file_rate ; C.Col_file_state ; 
    C.Col_file_availability ]

let downloaded_columns = define_option mldonkey_gui_ini
    ["downloaded_columns"] M.h_downloaded_columns
    (list_option C.File.class_column)
    [ C.Col_file_name ; C.Col_file_size ; 
      C.Col_file_format;]

(** {3 Clients} *)

let friends_columns = define_option mldonkey_gui_ini
    ["friends_columns"] M.h_friends_columns
    (list_option C.Client.class_column)
    [ C.Col_client_name ; C.Col_client_kind ; 
      C.Col_client_state; ]

let file_locations_columns = define_option mldonkey_gui_ini
    ["file_locations_columns"] M.h_file_locations_columns
    (list_option C.Client.class_column)
    [ C.Col_client_name ; C.Col_client_kind ; 
      C.Col_client_state; ]

(** {3 Users} *)

let users_columns = define_option mldonkey_gui_ini
    ["users_columns"] "Columns of the users lists"
    (list_option C.User.class_column)
  [ C.Col_user_name ; C.Col_user_kind ;     C.Col_user_tags; ]

let rooms_columns = define_option mldonkey_gui_ini
    ["rooms_columns"] "Columns of the room lists"
    (list_option C.Room.class_column)
  [ C.Col_room_name ; C.Col_room_nusers ]

(** {3 Servers} *)

let servers_columns = define_option mldonkey_gui_ini
    ["server_columns"] M.h_servers_columns
    (list_option C.Server.class_column)
    [ C.Col_server_address ; C.Col_server_state ; 
      C.Col_server_users ; C.Col_server_files ;
      C.Col_server_desc ]

(** {3 Results} *)

let results_columns = define_option mldonkey_gui_ini
    ["results_columns"] M.h_results_columns
    (list_option C.Result.class_column)
    [ C.Col_result_name ; 
      C.Col_result_size ; C.Col_result_format ;
      C.Col_result_props ; C.Col_result_comment ; ]


(** {3 Upload info} *)

let shared_files_up_columns = define_option mldonkey_gui_ini
    ["shared_files_up_columns"] M.h_shared_files_up_columns
    (list_option C.Shared_files_up.class_column)
    [ C.Col_shared_file ; C.Col_shared_size; C.Col_shared_requests ; 
      C.Col_shared_upsize ;
    ] 

(** {2 Others} *)

let password = define_option mldonkey_gui_ini ["password"] 
    M.h_gui_password string_option ""
let port = define_option mldonkey_gui_ini ["port"] 
    M.h_gui_port int_option 4001
let hostname = define_option mldonkey_gui_ini ["hostname"] 
    M.h_hostname string_option "localhost"

(** Profondeur max pour l'auto expand des arborescences des fichiers
   des friends. *)
let files_auto_expand_depth = define_option mldonkey_gui_ini 
    ["files_auto_expand_depth"]
    M.h_files_auto_expand_depth int_option 3

(** Whether we must print sizes in bytes or use G, M and k suffixes. *)
let use_size_suffixes = define_option mldonkey_gui_ini
    ["use_size_suffixes"]
    M.h_use_size_suffixes bool_option true

let max_client_name_len = define_option mldonkey_gui_ini
    ["max_client_name_len"] "Max length of a source name"
    int_option 18

let max_result_name_len = define_option mldonkey_gui_ini
    ["max_result_name_len"] "Max length of a source name"
    int_option 70
    
let compaction_overhead = define_option mldonkey_gui_ini 
    ["compaction_overhead"] 
    "The percentage of free memory before a compaction is triggered"
    int_option 50

(*
let auto_resize = define_option mldonkey_gui_ini
    ["auto_resize"]
  "auto resize columns" bool_option true
*)

let interface_buffer = define_option mldonkey_gui_ini
    ["interface_buffer"] "The size of the buffer to the core"
    int_option 10000000

  
let _ =
  option_hook compaction_overhead (fun _ ->
      let gc_control = Gc.get () in
      Gc.set { gc_control with Gc.max_overhead = !!compaction_overhead };     
  )
  
(** {2 Options du client} *)

let client_port = ref ""
let telnet_port = ref ""
let client_gui_port = ref ""
let save_options_delay = ref ""
let check_client_connections_delay = ref ""
let check_connections_delay = ref ""
let min_retry_delay = ref ""
let client_name = ref ""
let client_password = ref ""
let max_connected_servers = ref ""
let max_upload_rate = ref ""
let max_download_rate = ref ""
let server_connection_timeout = ref ""
let client_timeout = ref ""
let max_server_age = ref ""
let update_gui_delay = ref ""

let smtp_server = ref ""
let smtp_port = ref ""
let mail = ref ""

let temp_dir = ref ""
let incoming_dir = ref ""

let chat_app_port = ref ""
let chat_app_host = ref ""
let chat_port = ref ""
let chat_console_id = ref ""
let chat_warning_for_downloaded = ref ""

let client_options_assocs = [
  "http_port", client_port;
  "telnet_port", telnet_port;
  "gui_port",    client_gui_port;
  "save_options_delay", save_options_delay;
  "check_client_connections_delay", check_client_connections_delay;
  "check_connections_delay", check_connections_delay;
  "min_retry_delay", min_retry_delay;
  "client_name",   client_name;
  "password",  client_password;
  "max_connected_servers", max_connected_servers;
  "max_hard_upload_rate", max_upload_rate;
  "max_hard_download_rate", max_download_rate;
  "server_connection_timeout", server_connection_timeout;
  "client_timeout", client_timeout;
  "max_server_age", max_server_age;    
  "update_gui_delay", update_gui_delay;
  "smtp_server", smtp_server ;
  "smtp_port", smtp_port ;
  "mail", mail ;
  "temp_directory", temp_dir ;
  "incoming_directory", incoming_dir ;
  "chat_app_host", chat_app_host ;
  "chat_app_port", chat_app_port ;
  "chat_port", chat_port ;
  "chat_console_id", chat_console_id ;
  "chat_warning_for_downloaded", chat_warning_for_downloaded ;
  ]

let client_options_assocs_rev = 
  List.map 
    (fun (name,reference) -> reference, name) 
    client_options_assocs
  
  
