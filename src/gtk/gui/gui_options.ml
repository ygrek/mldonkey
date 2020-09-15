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

open Gettext
open Options

module M = Gui_messages
module C = Gui_columns

let _ = Unix2.safe_mkdir CommonOptions.home_dir
let mldonkey_gui_ini = create_options_file 
    (Filename.concat CommonOptions.home_dir "mldonkey_gui.ini")
  
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

module TabPosition = struct

    let values = [
        "left" ; "right"; "top"; "bottom"
      ]
    
    let string_to_pos v =
      match String.lowercase v with
      | "left" -> `LEFT
      | "right" -> `RIGHT
      | "top" -> `TOP
      | "bottom" -> `BOTTOM
      | _ -> `LEFT
    
    let pos_to_string pos = 
      match pos with
        `LEFT -> "left"
      | `RIGHT -> "right"
      | `TOP -> "top"
      | `BOTTOM -> "bottom"

    let value_to_pos v =
      match v with
      | StringValue "left" -> `LEFT
      | StringValue "right" -> `RIGHT
      | StringValue "top" -> `TOP
      | StringValue "bottom" -> `BOTTOM
      | _ -> `LEFT
          
    let pos_to_value pos = 
      StringValue (match pos with
          `LEFT -> "left"
        | `RIGHT -> "right"
        | `TOP -> "top"
        | `BOTTOM -> "bottom")
    
    let t = define_option_class "TabPosition" value_to_pos pos_to_value
  end
  
(** {2 Key mappings} *)

let mldonkey_gui_section = file_section mldonkey_gui_ini [] ""
  
let keymap_global = define_option mldonkey_gui_section ["keymaps"; "global"]
    "Global key bindings" 
  (list_option (tuple2_option (KeyOption.t, string_option))) []

let keymap_servers = define_option mldonkey_gui_section ["keymaps"; "servers"]
  "Key bindings for servers tab"   
  (list_option (tuple2_option (KeyOption.t, string_option))) []

let keymap_downloads = define_option mldonkey_gui_section ["keymaps"; "downloads"]
  "Key bindings for downloads tab" 
  (list_option (tuple2_option (KeyOption.t, string_option))) []

let keymap_friends = define_option mldonkey_gui_section ["keymaps"; "friends"]
  "Key bindings for friends tab" 
  (list_option (tuple2_option (KeyOption.t, string_option))) []

let keymap_queries = define_option mldonkey_gui_section ["keymaps"; "queries"]
  "Key bindings for queries tab" 
  (list_option (tuple2_option (KeyOption.t, string_option))) []

let keymap_results = define_option mldonkey_gui_section ["keymaps"; "results"]
  "Key bindings for results tab" 
  (list_option (tuple2_option (KeyOption.t, string_option))) []

let keymap_console = define_option mldonkey_gui_section ["keymaps"; "console"]
  "Key bindings for console tab" 
    (list_option (tuple2_option (KeyOption.t, string_option))) []

let add_binding map binding action = 
  map =:= 
    (KeyOption.string_to_key binding, action) :: !!map
  
(** {2 Colors} *)

let color_default = define_option mldonkey_gui_section ["colors"; "default"]
    (gettext M.h_col_default) string_option "Black"
let color_downloaded =  define_option mldonkey_gui_section ["colors"; "downloaded"]
    (gettext M.h_col_downloaded) string_option "Blue"
let color_downloading =  define_option mldonkey_gui_section ["colors"; "downloading"]
    (gettext M.h_col_downloading) string_option "DarkGreen"
let color_available =  define_option mldonkey_gui_section ["colors"; "available"]
    (gettext M.h_col_avail) string_option "Orange"
let color_not_available =  define_option mldonkey_gui_section ["colors"; "not_available"]
    (gettext M.h_col_not_avail) string_option "Red"
let color_connected =  define_option mldonkey_gui_section ["colors"; "connected"]
    (gettext M.h_col_connected) string_option "DarkGreen"
let color_not_connected =  define_option mldonkey_gui_section ["colors"; "not_connected"]
    (gettext M.h_col_not_connected) string_option "Black"
let color_connecting =  define_option mldonkey_gui_section ["colors"; "connecting"]
    M.h_col_connecting string_option "Orange"
let color_files_listed =  define_option mldonkey_gui_section ["colors"; "files_listed"]
    M.h_col_files_listed string_option "Blue"

(** {2 Icons} *)

let xpm_label s = ["icons" ; s]
let xpm_mini_label s = ["mini_icons" ; s]

let xpm_remove = define_option mldonkey_gui_section (xpm_label M.o_xpm_remove)
    "" filename_option ""
let xpm_cancel = define_option mldonkey_gui_section (xpm_label M.o_xpm_cancel)
    "" filename_option ""
let xpm_connect = define_option mldonkey_gui_section (xpm_label M.o_xpm_connect)
    "" filename_option ""
let xpm_disconnect = define_option mldonkey_gui_section (xpm_label M.o_xpm_disconnect)
    "" filename_option ""
let xpm_view_users = define_option mldonkey_gui_section (xpm_label M.o_xpm_view_users)
    "" filename_option ""
let xpm_connect_more_servers = define_option mldonkey_gui_section (xpm_label M.o_xpm_connect_more_servers)
    "" filename_option ""
let xpm_toggle_display_all_servers = define_option mldonkey_gui_section (xpm_label M.o_xpm_toggle_display_all_servers)
    "" filename_option ""
let xpm_remove_old_servers = define_option mldonkey_gui_section (xpm_label M.o_xpm_remove_old_servers)
    "" filename_option ""
let xpm_save = define_option mldonkey_gui_section (xpm_label M.o_xpm_save)
    "" filename_option ""
let xpm_save_all = define_option mldonkey_gui_section (xpm_label M.o_xpm_save_all)
    "" filename_option ""
let xpm_save_as = define_option mldonkey_gui_section (xpm_label M.o_xpm_save_as)
    "" filename_option ""
let xpm_edit_mp3 = define_option mldonkey_gui_section (xpm_label M.o_xpm_edit_mp3)
    "" filename_option ""
let xpm_pause_resume = define_option mldonkey_gui_section (xpm_label M.o_xpm_pause_resume)
    "" filename_option ""
let xpm_get_format = define_option mldonkey_gui_section (xpm_label M.o_xpm_get_format)
    "" filename_option ""
let xpm_preview = define_option mldonkey_gui_section (xpm_label M.o_xpm_preview)
    "" filename_option ""
let xpm_verify_chunks = define_option mldonkey_gui_section (xpm_label M.o_xpm_verify_chunks)
    "" filename_option ""
let xpm_retry_connect = define_option mldonkey_gui_section (xpm_label M.o_xpm_retry_connect)
    "" filename_option ""
let xpm_add_to_friends = define_option mldonkey_gui_section (xpm_label M.o_xpm_add_to_friends)
    "" filename_option ""
let xpm_download = define_option mldonkey_gui_section (xpm_label M.o_xpm_download)
    "" filename_option ""
let xpm_submit_search = define_option mldonkey_gui_section (xpm_label M.o_xpm_submit_search)
    "" filename_option ""
let xpm_extend_search = define_option mldonkey_gui_section (xpm_label M.o_xpm_extend_search)
    "" filename_option ""
let xpm_local_search = define_option mldonkey_gui_section (xpm_label M.o_xpm_local_search)
    "" filename_option ""
let xpm_find_friend =  define_option mldonkey_gui_section (xpm_label M.o_xpm_find_friend)
    "" filename_option ""
let xpm_remove_all_friends =  define_option mldonkey_gui_section (xpm_label M.o_xpm_remove_all_friends)
    "" filename_option ""
let xpm_close_room =  define_option mldonkey_gui_section (xpm_label M.o_xpm_close_room)
    "" filename_option ""
let xpm_refresh =  define_option mldonkey_gui_section (xpm_label M.o_xpm_refresh)
    "" filename_option ""

  
let xpm_mini_remove = define_option mldonkey_gui_section (xpm_mini_label M.o_xpm_mini_remove)
    "" filename_option ""
let xpm_mini_cancel = define_option mldonkey_gui_section (xpm_mini_label M.o_xpm_mini_cancel)
    "" filename_option ""
let xpm_mini_connect = define_option mldonkey_gui_section (xpm_mini_label M.o_xpm_mini_connect)
    "" filename_option ""
let xpm_mini_disconnect = define_option mldonkey_gui_section (xpm_mini_label M.o_xpm_mini_disconnect)
    "" filename_option ""
let xpm_mini_view_users = define_option mldonkey_gui_section (xpm_mini_label M.o_xpm_mini_view_users)
    "" filename_option ""
let xpm_mini_connect_more_servers = define_option mldonkey_gui_section (xpm_mini_label M.o_xpm_mini_connect_more_servers)
    "" filename_option ""
let xpm_mini_toggle_display_all_servers = define_option mldonkey_gui_section (xpm_mini_label M.o_xpm_mini_toggle_display_all_servers)
    "" filename_option ""
let xpm_mini_remove_old_servers = define_option mldonkey_gui_section (xpm_mini_label M.o_xpm_mini_remove_old_servers)
    "" filename_option ""
let xpm_mini_save = define_option mldonkey_gui_section (xpm_mini_label M.o_xpm_mini_save)
    "" filename_option ""
let xpm_mini_save_all = define_option mldonkey_gui_section (xpm_mini_label M.o_xpm_mini_save_all)
    "" filename_option ""
let xpm_mini_save_as = define_option mldonkey_gui_section (xpm_mini_label M.o_xpm_mini_save_as)
    "" filename_option ""
let xpm_mini_edit_mp3 = define_option mldonkey_gui_section (xpm_mini_label M.o_xpm_mini_edit_mp3)
    "" filename_option ""
let xpm_mini_pause_resume = define_option mldonkey_gui_section (xpm_mini_label M.o_xpm_mini_pause_resume)
    "" filename_option ""
let xpm_mini_get_format = define_option mldonkey_gui_section (xpm_mini_label M.o_xpm_mini_get_format)
    "" filename_option ""
let xpm_mini_preview = define_option mldonkey_gui_section (xpm_mini_label M.o_xpm_mini_preview)
    "" filename_option ""
let xpm_mini_verify_chunks = define_option mldonkey_gui_section (xpm_mini_label M.o_xpm_mini_verify_chunks)
    "" filename_option ""
let xpm_mini_retry_connect = define_option mldonkey_gui_section (xpm_mini_label M.o_xpm_mini_retry_connect)
    "" filename_option ""
let xpm_mini_add_to_friends = define_option mldonkey_gui_section (xpm_mini_label M.o_xpm_mini_add_to_friends)
    "" filename_option ""
let xpm_mini_download = define_option mldonkey_gui_section (xpm_mini_label M.o_xpm_mini_download)
    "" filename_option ""
let xpm_mini_submit_search = define_option mldonkey_gui_section (xpm_mini_label M.o_xpm_mini_submit_search)
    "" filename_option ""
let xpm_mini_extend_search = define_option mldonkey_gui_section (xpm_mini_label M.o_xpm_mini_extend_search)
    "" filename_option ""
let xpm_mini_local_search = define_option mldonkey_gui_section (xpm_mini_label M.o_xpm_mini_local_search)
    "" filename_option ""
let xpm_mini_find_friend =  define_option mldonkey_gui_section (xpm_mini_label M.o_xpm_mini_find_friend)
    "" filename_option ""
let xpm_mini_remove_all_friends =  define_option mldonkey_gui_section (xpm_mini_label M.o_xpm_mini_remove_all_friends)
    "" filename_option ""
let xpm_mini_close_room =  define_option mldonkey_gui_section (xpm_mini_label M.o_xpm_mini_close_room)
    "" filename_option ""
let xpm_mini_refresh =  define_option mldonkey_gui_section (xpm_mini_label M.o_xpm_mini_refresh)
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

let toolbars_style = define_option mldonkey_gui_section
    ["toolbars_style"] (gettext M.h_toolbars_style)
    class_tbstyle `BOTH

let mini_toolbars = define_option mldonkey_gui_section
    ["mini_toolbars"] "Use mini icons in toolbars"
    bool_option true

(** {2 Layout} *)

let servers_hpane_left = define_option mldonkey_gui_section
    ["layout"; "servers_hpane_left"]
    "Size in % of left part of the servers hpane"
    int_option 80

let servers_vpane_up = define_option mldonkey_gui_section
    ["layout"; "servers_vpane_up"]
  "Size in % of upper part of the servers hpane"
    int_option 39

let rooms_hpane_left = define_option mldonkey_gui_section
    ["layout"; "rooms_hpane_left"]
    "Size in % of left part of the rooms hpane"
    int_option 80

let downloads_hpane_left = define_option mldonkey_gui_section
    ["layout"; "downloads_hpane_left"]
  "Size in % of left part of the downloads hpane"
    int_option 80

let friends_hpane_left = define_option mldonkey_gui_section
  ["layout"; "friends_hpane_left"]
  "Size in % of left part of the friends hpane"
    int_option 30

let searches_hpane_left = define_option mldonkey_gui_section
    ["layout"; "searches_hpane_left"]
  "Size in % of left part of the searches hpane"
    int_option 40
  
let downloads_vpane_up = define_option mldonkey_gui_section
    ["layout"; "downloads_vpane_up"]
  "Size in % of up part of the downloads vpane (downloaded/downloading)"
    int_option 50

let downloads_wpane_up = define_option mldonkey_gui_section
    ["layout"; "downloads_wpane_up"]
  "Size in % of up part of the downloads vpane (locations/client info)"
    int_option 75

let friends_vpane_up = define_option mldonkey_gui_section
    ["layout"; "friends_vpane_up"]
    "Size in % of up part of the friends vpane"
    int_option 80

let friends_hpane_dirs = define_option mldonkey_gui_section
    ["layout"; "friends_hpane_dirs"]
    "Size in % of the directories part of the files box"
    int_option 20

let gui_width = define_option mldonkey_gui_section
    ["layout"; "width"]
    "Width of GUI window" int_option 600

let gui_height = define_option mldonkey_gui_section
    ["layout"; "height"]
  "Height of GUI window" int_option 400

let notebook_tab = define_option mldonkey_gui_section
    ["layout"; "tab_position"]
    "Position of tab (left, right, top, bottom)" TabPosition.t `TOP



(** {2 List columns} *)

(** {3 Files} *)


let downloads_columns = define_option mldonkey_gui_section
    ["downloads_columns"] (gettext M.h_downloads_columns)
    (list_option C.File.class_column)
  [ C.Col_file_name ; C.Col_file_size ; C.Col_file_downloaded;
    C.Col_file_percent ;
      C.Col_file_rate ; C.Col_file_state ; 
    C.Col_file_availability ]

let downloaded_columns = define_option mldonkey_gui_section
    ["downloaded_columns"] (gettext M.h_downloaded_columns)
    (list_option C.File.class_column)
    [ C.Col_file_name ; C.Col_file_size ; 
      C.Col_file_format;]

(** {3 Clients} *)

let friends_columns = define_option mldonkey_gui_section
    ["friends_columns"] (gettext M.h_friends_columns)
    (list_option C.Client.class_column)
    [ C.Col_client_name ; C.Col_client_kind ; 
      C.Col_client_state; ]

let file_locations_columns = define_option mldonkey_gui_section
    ["file_locations_columns"] (gettext M.h_file_locations_columns)
    (list_option C.Client.class_column)
    [ C.Col_client_name ; C.Col_client_kind ; 
      C.Col_client_state; ]

(** {3 Users} *)

let users_columns = define_option mldonkey_gui_section
    ["users_columns"] "Columns of the users lists"
    (list_option C.User.class_column)
  [ C.Col_user_name ; C.Col_user_kind ;     C.Col_user_tags; ]

let rooms_columns = define_option mldonkey_gui_section
    ["rooms_columns"] "Columns of the room lists"
    (list_option C.Room.class_column)
  [ C.Col_room_name ; C.Col_room_nusers ]

(** {3 Servers} *)

let servers_columns = define_option mldonkey_gui_section
    ["server_columns"] (gettext M.h_servers_columns)
    (list_option C.Server.class_column)
    [ C.Col_server_address ; C.Col_server_state ; 
      C.Col_server_users ; C.Col_server_files ;
      C.Col_server_desc ]

(** {3 Results} *)

let results_columns = define_option mldonkey_gui_section
    ["results_columns"] (gettext M.h_results_columns)
    (list_option C.Result.class_column)
    [ C.Col_result_name ; 
      C.Col_result_size ; C.Col_result_format ;
      C.Col_result_props ; C.Col_result_comment ; ]


(** {3 Upload info} *)

let shared_files_up_columns = define_option mldonkey_gui_section
    ["shared_files_up_columns"] M.h_shared_files_up_columns
    (list_option C.Shared_files_up.class_column)
    [ C.Col_shared_file ; C.Col_shared_size; C.Col_shared_requests ; 
      C.Col_shared_upsize ;
    ] 

(** {2 Others} *)

let login = define_option mldonkey_gui_section ["login"] 
    "Your login name (default is admin)" string_option "admin"
let password = define_option mldonkey_gui_section ["password"] 
    (gettext M.h_gui_password) string_option ""
let port = define_option mldonkey_gui_section ["port"] 
    (gettext M.h_gui_port) port_option 4001
let hostname = define_option mldonkey_gui_section ["hostname"] 
    (gettext M.h_hostname) string_option "localhost"

let history = define_option mldonkey_gui_section ["history"]
    "History of connected cores"
  (list_option (tuple2_option (string_option, int_option))) []
  
(** Profondeur max pour l'auto expand des arborescences des fichiers
   des friends. *)
let files_auto_expand_depth = define_option mldonkey_gui_section 
    ["files_auto_expand_depth"]
    (M.h_files_auto_expand_depth) int_option 3

(** Whether we must print sizes in bytes or use G, M and k suffixes. *)
let use_size_suffixes = define_option mldonkey_gui_section
    ["use_size_suffixes"]
    (M.h_use_size_suffixes) bool_option true

let use_availability_height = define_option mldonkey_gui_section ["availability_height"]
  "Display the availability of each chunk as the height of the colored bar"
    bool_option true

let availability_max = define_option mldonkey_gui_section ["availability_max"]
  "If use_availability_height is true, which availability corresponds to
  a full bar ?"
    int_option 10
  
let use_relative_availability = define_option mldonkey_gui_section ["relative_availability"]
  "Calculate the % avilability ignoring already present chunks"
    bool_option true

let max_client_name_len = define_option mldonkey_gui_section
    ["max_client_name_len"] "Max length of a source name"
    int_option 18

let max_result_name_len = define_option mldonkey_gui_section
    ["max_result_name_len"] "Max length of a source name"
    int_option 70
    
let compaction_overhead = define_option mldonkey_gui_section 
    ["compaction_overhead"] 
    "The percentage of free memory before a compaction is triggered"
    int_option 50

(*
let auto_resize = define_option mldonkey_gui_section
    ["auto_resize"]
  "auto resize columns" bool_option true
*)

let interface_buffer = define_option mldonkey_gui_section
    ["interface_buffer"] "The size of the buffer to the core"
    int_option 10000000

  
let _ =
  option_hook compaction_overhead (fun _ ->
      let gc_control = Gc.get () in
      Gc.set { gc_control with Gc.max_overhead = !!compaction_overhead };     
  )

(** {2 Options du client} *)

  
let client_name = ref ""

let client_options_assocs = [
    "client_name",   client_name;    
  ]


let table = [
  M.o_xpm_remove, (Trash_xpm.t, xpm_remove) ;
  M.o_xpm_cancel, (Cancel_xpm.t, xpm_cancel);
  M.o_xpm_connect, (Connect_xpm.t, xpm_connect);
  M.o_xpm_disconnect, (Disconnect_xpm.t, xpm_disconnect);
  M.o_xpm_view_users, (View_users_xpm.t, xpm_view_users);
  M.o_xpm_connect_more_servers, (Connect_more_xpm.t, xpm_connect_more_servers);
  M.o_xpm_remove_old_servers, (Trash_xpm.t, xpm_remove_old_servers);
    M.o_xpm_toggle_display_all_servers, (
      Get_format_xpm.t, xpm_toggle_display_all_servers);
  M.o_xpm_save, (Save_xpm.t, xpm_save);
  M.o_xpm_save_all, (Save_all_xpm.t, xpm_save_all);
  M.o_xpm_save_as, (Save_as_xpm.t, xpm_save_as);
  M.o_xpm_edit_mp3, (Edit_mp3_xpm.t, xpm_edit_mp3);
  M.o_xpm_pause_resume, (Pause_resume_xpm.t, xpm_pause_resume);
  M.o_xpm_get_format, (Get_format_xpm.t, xpm_get_format);
  M.o_xpm_preview, (Preview_xpm.t, xpm_preview);
  M.o_xpm_verify_chunks, (Verify_chunks_xpm.t, xpm_verify_chunks);
  M.o_xpm_retry_connect, (Connect_xpm.t, xpm_retry_connect); 
  M.o_xpm_add_to_friends, (Add_to_friends_xpm.t, xpm_add_to_friends);
  M.o_xpm_download, (Download_xpm.t, xpm_download);
   M.o_xpm_submit_search, (Get_format_xpm.t, xpm_submit_search); 
  M.o_xpm_extend_search, (Extend_search_xpm.t, xpm_extend_search);
  M.o_xpm_local_search, (Local_search_xpm.t, xpm_local_search);
  M.o_xpm_find_friend, (Get_format_xpm.t, xpm_find_friend) ;  
  M.o_xpm_remove_all_friends, (Remove_all_friends_xpm.t, xpm_remove_all_friends) ; 
  M.o_xpm_close_room, (Trash_xpm.t, xpm_close_room) ;
  M.o_xpm_refresh, (Refres_xpm.t, xpm_refresh) ;
  M.o_xpm_mini_remove, (Trash_xpm.mini, xpm_mini_remove) ;
  M.o_xpm_mini_cancel, (Cancel_xpm.mini, xpm_mini_cancel);
  M.o_xpm_mini_connect, (Connect_xpm.mini, xpm_mini_connect);
  M.o_xpm_mini_disconnect, (Disconnect_xpm.mini, xpm_mini_disconnect);
  M.o_xpm_mini_view_users, (View_users_xpm.mini, xpm_mini_view_users);
  M.o_xpm_mini_connect_more_servers, (Connect_more_xpm.mini, xpm_mini_connect_more_servers);
  M.o_xpm_mini_remove_old_servers, (Trash_xpm.mini, xpm_mini_remove_old_servers);
    M.o_xpm_mini_toggle_display_all_servers, (
      Get_format_xpm.mini, xpm_mini_toggle_display_all_servers);
  M.o_xpm_mini_save, (Save_xpm.mini, xpm_mini_save);
  M.o_xpm_mini_save_all, (Save_all_xpm.mini, xpm_mini_save_all);
  M.o_xpm_mini_save_as, (Save_as_xpm.mini, xpm_mini_save_as);
  M.o_xpm_mini_edit_mp3, (Edit_mp3_xpm.mini, xpm_mini_edit_mp3);
  M.o_xpm_mini_pause_resume, (Pause_resume_xpm.mini, xpm_mini_pause_resume);
  M.o_xpm_mini_get_format, (Get_format_xpm.mini, xpm_mini_get_format);
  M.o_xpm_mini_preview, (Preview_xpm.mini, xpm_mini_preview);
  M.o_xpm_mini_verify_chunks, (Verify_chunks_xpm.mini, xpm_mini_verify_chunks);
  M.o_xpm_mini_retry_connect, (Connect_xpm.mini, xpm_mini_retry_connect); 
  M.o_xpm_mini_add_to_friends, (Add_to_friends_xpm.mini, xpm_mini_add_to_friends);
  M.o_xpm_mini_download, (Download_xpm.mini, xpm_mini_download);
   M.o_xpm_mini_submit_search, (Get_format_xpm.mini, xpm_mini_submit_search); 
  M.o_xpm_mini_extend_search, (Extend_search_xpm.mini, xpm_mini_extend_search);
  M.o_xpm_mini_local_search, (Local_search_xpm.mini, xpm_mini_local_search);
  M.o_xpm_mini_find_friend, (Get_format_xpm.mini, xpm_mini_find_friend) ;  
  M.o_xpm_mini_remove_all_friends, (Remove_all_friends_xpm.mini, xpm_mini_remove_all_friends) ; 
  M.o_xpm_mini_close_room, (Trash_xpm.mini, xpm_mini_close_room) ;
  M.o_xpm_mini_refresh, (Refres_xpm.mini, xpm_mini_refresh) ;
] 

let gdk_pix i =
  try
    let (default, o) = List.assoc i table in
    match !!o with
      "" -> 
        GDraw.pixmap_from_xpm_d ~data: default
          ~colormap: (Gdk.Color.get_system_colormap ())
          () 
    | f ->
        try
          GDraw.pixmap_from_xpm ~file: f
            ~colormap: (Gdk.Color.get_system_colormap ())
            () 
        with
          _ ->
            GDraw.pixmap_from_xpm_d ~data: default
              ~colormap: (Gdk.Color.get_system_colormap ())
              () 
  with
    Not_found ->
      GDraw.pixmap_from_xpm_d ~data: Trash_xpm.t
        ~colormap: (Gdk.Color.get_system_colormap ())
        () 
      
let pixmap i = GMisc.pixmap (gdk_pix i) ()
      



let copy_messages = define_option mldonkey_gui_section
    ["copy_messages"]
  "For bundle binaries, should we directly pass structures between
    the core and the GUI (faster), or copy them (fewer bugs) " bool_option 
  true

let downloads_up = define_option mldonkey_gui_section
    ["downloads_up"]
  "Put the download frame up"
  bool_option true

let chunk_width = define_option mldonkey_gui_section
    ["chunk_width"]
    "The chunk width in pixel"
    int_option 3
  
