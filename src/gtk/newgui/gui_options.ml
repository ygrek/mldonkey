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

let _s x = _s "Gui_options" x
let _b x = _b "Gui_options" x  
    
let define_option a b ?desc c d e = 
  match desc with
    None -> define_option a b (_s c) d e
  | Some desc -> define_option a b ~desc: (_s desc) (_s c) d e
let define_expert_option a b ?desc c d e = 
  match desc with
    None -> define_expert_option a b (_s c) d e
  | Some desc -> define_expert_option a b ~desc: (_s desc) (_s c) d e

module M = Gui_messages
module C = Gui_columns

let _ = Unix2.safe_mkdir CommonOptions.home_dir
  
let mldonkey_gui_ini = create_options_file 
    (Filename.concat CommonOptions.home_dir "mldonkey_newgui.ini")
  
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

  
let mldonkey_gui_section = file_section mldonkey_gui_ini [] ""
  
(** {2 Key mappings} *)

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
    ( M.h_col_default) string_option "Black"
let color_downloaded =  define_option mldonkey_gui_section ["colors"; "downloaded"]
    ( M.h_col_downloaded) string_option "Blue"
let color_downloading =  define_option mldonkey_gui_section ["colors"; "downloading"]
    ( M.h_col_downloading) string_option "DarkGreen"
let color_available =  define_option mldonkey_gui_section ["colors"; "available"]
    ( M.h_col_avail) string_option "Orange"
let color_not_available =  define_option mldonkey_gui_section ["colors"; "not_available"]
    ( M.h_col_not_avail) string_option "Red"
let color_connected =  define_option mldonkey_gui_section ["colors"; "connected"]
    ( M.h_col_connected) string_option "DarkGreen"
let color_not_connected =  define_option mldonkey_gui_section ["colors"; "not_connected"]
    ( M.h_col_not_connected) string_option "Black"
let color_connecting =  define_option mldonkey_gui_section ["colors"; "connecting"]
    ( M.h_col_connecting) string_option "Orange"
let color_files_listed =  define_option mldonkey_gui_section ["colors"; "files_listed"]
    ( M.h_col_files_listed) string_option "Blue"
let color_files_result =  define_option mldonkey_gui_section ["colors"; "files_result"]
    ( M.h_col_files_listed) string_option "Blue"
let color_tab_selected =  define_option mldonkey_gui_section ["colors"; "tab_selected"]
    ( M.h_col_tab_selected) string_option "#ffffff"
let color_tab_not_selected =  define_option mldonkey_gui_section ["colors"; "tab_not_selected"]
    ( M.h_col_tab_not_selected) string_option "#636363"
let color_list_bg =  define_option mldonkey_gui_section ["colors"; "list_bg"]
    ( M.h_col_list_bg) string_option "#FFFFDB"
let color_network_enabled =  define_option mldonkey_gui_section ["colors"; "network_enabled"]
    ( M.h_col_network_enabled) string_option "#ffffff"
let color_network_disabled =  define_option mldonkey_gui_section ["colors"; "network_disabled"]
    ( M.h_col_network_disabled) string_option "#636363"

(** {2 Fonts} *)

let font_list =  define_option mldonkey_gui_section ["fonts"; "list"]
    ( M.h_font_list) string_option
     ( if Autoconf.windows then "-unknown-Tahoma-normal-r-normal-*-*-100-*-*-p-*-iso8859-1"
     else "-*-*-*-*-*-*-*-*-*-*-*-*-*-*" )

let font_main_tab =  define_option mldonkey_gui_section ["fonts"; "main_tab"]
    ( M.h_font_main_tab) string_option
     ( if Autoconf.windows then "-unknown-Tahoma-normal-r-normal-*-*-100-*-*-p-*-iso8859-1"
     else "-*-*-*-*-*-*-*-*-*-*-*-*-*-*" )

let font_networks =  define_option mldonkey_gui_section ["fonts"; "networks"]
    ( M.h_font_networks) string_option
     ( if Autoconf.windows then "-unknown-Tahoma-normal-r-normal-*-*-100-*-*-p-*-iso8859-1"
     else "-*-*-*-*-*-*-*-*-*-*-*-*-*-*" )

let font_graphic =  define_option mldonkey_gui_section ["fonts"; "graphic"]
    ( M.h_font_graphic)  string_option
     ( if Autoconf.windows then "-unknown-Tahoma-normal-r-normal-*-*-80-*-*-p-*-iso8859-1"
     else "-*-*-*-*-*-*-*-*-*-*-*-*-*-*" )

(*
let font_list =  define_option mldonkey_gui_section ["fonts"; "list"]
    ( M.h_font_list) string_option "-bitstream-bitstream vera sans-medium-r-normal-*-10-*-*-*-p-*-iso8859-1"
let font_main_tab =  define_option mldonkey_gui_section ["fonts"; "main_tab"]
    ( M.h_font_main_tab) string_option "-2rebels-andover-medium-r-normal-*-48-*-*-*-p-*-iso8859-1"
let font_networks =  define_option mldonkey_gui_section ["fonts"; "networks"]
    ( M.h_font_networks) string_option "-2rebels-andover-medium-r-normal-*-48-*-*-*-p-*-iso8859-1"
let font_graphic =  define_option mldonkey_gui_section ["fonts"; "graphic"]
    ( M.h_font_graphic) string_option "-bitstream-bitstream vera sans-medium-r-normal-*-10-*-*-*-p-*-iso8859-1"
*)

(** {2 Icons} *)

let xpm_label s = ["icons" ; s]
let xpm_mini_label s = ["mini_icons" ; s]

let xpm_toggle_display_all_servers = define_option mldonkey_gui_section (xpm_label M.o_xpm_toggle_display_all_servers)
    "" filename_option ""
let xpm_add_server = define_option mldonkey_gui_section (xpm_label M.o_xpm_add_server)
    "" filename_option ""
let xpm_submit_search = define_option mldonkey_gui_section (xpm_label M.o_xpm_submit_search)
    "" filename_option ""
let xpm_extend_search = define_option mldonkey_gui_section (xpm_label M.o_xpm_extend_search)
    "" filename_option ""
let xpm_local_search = define_option mldonkey_gui_section (xpm_label M.o_xpm_local_search)
    "" filename_option ""
let xpm_subscribe_search = define_option mldonkey_gui_section (xpm_label M.o_xpm_subscribe_search)
    "" filename_option ""
let xpm_stop_search = define_option mldonkey_gui_section (xpm_label M.o_xpm_stop_search)
    "" filename_option ""
let xpm_close_search = define_option mldonkey_gui_section (xpm_label M.o_xpm_close_search)
    "" filename_option ""
let xpm_close_room =  define_option mldonkey_gui_section (xpm_label M.o_xpm_close_room)
    "" filename_option ""
let xpm_add_shared_directory =  define_option mldonkey_gui_section (xpm_label M.o_xpm_add_shared_directory)
    "" filename_option ""
let xpm_download_directory =  define_option mldonkey_gui_section (xpm_label M.o_xpm_download_directory)
    "" filename_option ""
let xpm_view_pending_slots =  define_option mldonkey_gui_section (xpm_label M.o_xpm_view_pending_slots)
    "" filename_option ""

let xpm_mini_toggle_display_all_servers = define_option mldonkey_gui_section
    (xpm_mini_label M.o_xpm_mini_toggle_display_all_servers)
    "" filename_option ""
let xpm_mini_add_server = define_option mldonkey_gui_section (xpm_mini_label M.o_xpm_mini_add_server)
    "" filename_option ""
let xpm_mini_submit_search = define_option mldonkey_gui_section (xpm_mini_label M.o_xpm_mini_submit_search)
    "" filename_option ""
let xpm_mini_extend_search = define_option mldonkey_gui_section (xpm_mini_label M.o_xpm_mini_extend_search)
    "" filename_option ""
let xpm_mini_local_search = define_option mldonkey_gui_section (xpm_mini_label M.o_xpm_mini_local_search)
    "" filename_option ""
let xpm_mini_subscribe_search = define_option mldonkey_gui_section (xpm_label M.o_xpm_mini_subscribe_search)
    "" filename_option ""
let xpm_mini_stop_search = define_option mldonkey_gui_section (xpm_label M.o_xpm_mini_stop_search)
    "" filename_option ""
let xpm_mini_close_search = define_option mldonkey_gui_section (xpm_label M.o_xpm_mini_close_search)
    "" filename_option ""
let xpm_mini_close_room =  define_option mldonkey_gui_section (xpm_mini_label M.o_xpm_mini_close_room)
    "" filename_option ""
let xpm_mini_add_shared_directory =  define_option mldonkey_gui_section (xpm_label M.o_xpm_mini_add_shared_directory)
    "" filename_option ""
let xpm_mini_download_directory =  define_option mldonkey_gui_section (xpm_label M.o_xpm_mini_download_directory)
    "" filename_option ""
let xpm_mini_view_pending_slots =  define_option mldonkey_gui_section (xpm_label M.o_xpm_mini_view_pending_slots)
    "" filename_option ""

let xpm_nbk_networks_on = define_option mldonkey_gui_section (xpm_label M.o_xpm_nbk_networks_on)
    "" filename_option ""
let xpm_nbk_networks_off = define_option mldonkey_gui_section (xpm_label M.o_xpm_nbk_networks_off)
    "" filename_option ""
let xpm_nbk_networks_menu = define_option mldonkey_gui_section (xpm_label M.o_xpm_nbk_networks_menu)
    "" filename_option ""
let xpm_nbk_servers_on = define_option mldonkey_gui_section (xpm_label M.o_xpm_nbk_servers_on)
    "" filename_option ""
let xpm_nbk_servers_off = define_option mldonkey_gui_section (xpm_label M.o_xpm_nbk_servers_off)
    "" filename_option ""
let xpm_nbk_servers_menu = define_option mldonkey_gui_section (xpm_label M.o_xpm_nbk_servers_menu)
    "" filename_option ""
let xpm_nbk_downloads_on = define_option mldonkey_gui_section (xpm_label M.o_xpm_nbk_downloads_on)
    "" filename_option ""
let xpm_nbk_downloads_off = define_option mldonkey_gui_section (xpm_label M.o_xpm_nbk_downloads_off)
    "" filename_option ""
let xpm_nbk_downloads_menu = define_option mldonkey_gui_section (xpm_label M.o_xpm_nbk_downloads_menu)
    "" filename_option ""
let xpm_nbk_friends_on = define_option mldonkey_gui_section (xpm_label M.o_xpm_nbk_friends_on)
    "" filename_option ""
let xpm_nbk_friends_off = define_option mldonkey_gui_section (xpm_label M.o_xpm_nbk_friends_off)
    "" filename_option ""
let xpm_nbk_friends_menu = define_option mldonkey_gui_section (xpm_label M.o_xpm_nbk_friends_menu)
    "" filename_option ""
let xpm_nbk_search_on = define_option mldonkey_gui_section (xpm_label M.o_xpm_nbk_search_on)
    "" filename_option ""
let xpm_nbk_search_off = define_option mldonkey_gui_section (xpm_label M.o_xpm_nbk_search_off)
    "" filename_option ""
let xpm_nbk_search_menu = define_option mldonkey_gui_section (xpm_label M.o_xpm_nbk_search_menu)
    "" filename_option ""
let xpm_nbk_rooms_on = define_option mldonkey_gui_section (xpm_label M.o_xpm_nbk_rooms_on)
    "" filename_option ""
let xpm_nbk_rooms_off = define_option mldonkey_gui_section (xpm_label M.o_xpm_nbk_rooms_off)
    "" filename_option ""
let xpm_nbk_rooms_menu = define_option mldonkey_gui_section (xpm_label M.o_xpm_nbk_rooms_menu)
    "" filename_option ""
let xpm_nbk_uploads_on = define_option mldonkey_gui_section (xpm_label M.o_xpm_nbk_uploads_on)
    "" filename_option ""
let xpm_nbk_uploads_off = define_option mldonkey_gui_section (xpm_label M.o_xpm_nbk_uploads_off)
    "" filename_option ""
let xpm_nbk_uploads_menu = define_option mldonkey_gui_section (xpm_label M.o_xpm_nbk_uploads_menu)
    "" filename_option ""
let xpm_nbk_console_on = define_option mldonkey_gui_section (xpm_label M.o_xpm_nbk_console_on)
    "" filename_option ""
let xpm_nbk_console_off = define_option mldonkey_gui_section (xpm_label M.o_xpm_nbk_console_off)
    "" filename_option ""
let xpm_nbk_console_menu = define_option mldonkey_gui_section (xpm_label M.o_xpm_nbk_console_menu)
    "" filename_option ""
let xpm_nbk_graphs_on = define_option mldonkey_gui_section (xpm_label M.o_xpm_nbk_graphs_on)
    "" filename_option ""
let xpm_nbk_graphs_off = define_option mldonkey_gui_section (xpm_label M.o_xpm_nbk_graphs_off)
    "" filename_option ""
let xpm_nbk_graphs_menu = define_option mldonkey_gui_section (xpm_label M.o_xpm_nbk_graphs_menu)
    "" filename_option ""

let xpm_about = define_option mldonkey_gui_section (xpm_label M.o_xpm_about)
    "" filename_option ""
let xpm_settings = define_option mldonkey_gui_section (xpm_label M.o_xpm_settings)
    "" filename_option ""
let xpm_exit = define_option mldonkey_gui_section (xpm_label M.o_xpm_exit)
    "" filename_option ""
let xpm_gui = define_option mldonkey_gui_section (xpm_label M.o_xpm_gui)
    "" filename_option ""
let xpm_kill_core = define_option mldonkey_gui_section (xpm_label M.o_xpm_kill_core)
    "" filename_option ""
let xpm_splash_screen = define_option mldonkey_gui_section (xpm_label M.o_xpm_splash_screen)
    "" filename_option ""
  
let xpm_album_search = define_option mldonkey_gui_section (xpm_label M.o_xpm_album_search)
    "" filename_option ""
let xpm_movie_search = define_option mldonkey_gui_section (xpm_label M.o_xpm_movie_search)
    "" filename_option ""
let xpm_mp3_search = define_option mldonkey_gui_section (xpm_label M.o_xpm_mp3_search)
    "" filename_option ""
let xpm_complex_search = define_option mldonkey_gui_section (xpm_label M.o_xpm_complex_search)
    "" filename_option ""
let xpm_sharereactor_search = define_option mldonkey_gui_section (xpm_label M.o_xpm_sharereactor_search)
    "" filename_option ""
let xpm_jigle_search = define_option mldonkey_gui_section (xpm_label M.o_xpm_jigle_search)
    "" filename_option ""
let xpm_freedb_search = define_option mldonkey_gui_section (xpm_label M.o_xpm_freedb_search)
    "" filename_option ""
let xpm_imdb_search = define_option mldonkey_gui_section (xpm_label M.o_xpm_imdb_search)
    "" filename_option ""

let xpm_bt = define_option mldonkey_gui_section (xpm_label M.o_xpm_bt)
    "" filename_option ""
let xpm_dc = define_option mldonkey_gui_section (xpm_label M.o_xpm_dc)
    "" filename_option ""
let xpm_ed2k = define_option mldonkey_gui_section (xpm_label M.o_xpm_ed2k)
    "" filename_option ""
let xpm_fasttrack = define_option mldonkey_gui_section (xpm_label M.o_xpm_fasttrack)
    "" filename_option ""
let xpm_gnutella = define_option mldonkey_gui_section (xpm_label M.o_xpm_gnutella)
    "" filename_option ""
let xpm_napster = define_option mldonkey_gui_section (xpm_label M.o_xpm_napster)
    "" filename_option ""
let xpm_slsk = define_option mldonkey_gui_section (xpm_label M.o_xpm_slsk)
    "" filename_option ""
let xpm_unknown = define_option mldonkey_gui_section (xpm_label M.o_xpm_unknown)
    "" filename_option ""

let xpm_downloading = define_option mldonkey_gui_section (xpm_label M.o_xpm_downloading)
    "" filename_option ""
let xpm_connect_y = define_option mldonkey_gui_section (xpm_label M.o_xpm_connect_y)
    "" filename_option ""
let xpm_connect_m = define_option mldonkey_gui_section (xpm_label M.o_xpm_connect_m)
    "" filename_option ""
let xpm_connect_n = define_option mldonkey_gui_section (xpm_label M.o_xpm_connect_n)
    "" filename_option ""
let xpm_removedhost = define_option mldonkey_gui_section (xpm_label M.o_xpm_removedhost)
    "" filename_option ""
let xpm_blacklistedhost = define_option mldonkey_gui_section (xpm_label M.o_xpm_blacklistedhost)
    "" filename_option ""
let xpm_files_listed = define_option mldonkey_gui_section (xpm_label M.o_xpm_files_listed)
    "" filename_option ""
let xpm_server_c_low = define_option mldonkey_gui_section (xpm_label M.o_xpm_server_c_low)
    "" filename_option ""
let xpm_server_c_high = define_option mldonkey_gui_section (xpm_label M.o_xpm_server_c_high)
    "" filename_option ""
let xpm_server_ci = define_option mldonkey_gui_section (xpm_label M.o_xpm_server_ci)
    "" filename_option ""
let xpm_server_nc = define_option mldonkey_gui_section (xpm_label M.o_xpm_server_nc)
    "" filename_option ""

let xpm_friend_user = define_option mldonkey_gui_section (xpm_label M.o_xpm_friend_user)
    "" filename_option ""
let xpm_contact_user = define_option mldonkey_gui_section (xpm_label M.o_xpm_contact_user)
    "" filename_option ""
let xpm_normal_user = define_option mldonkey_gui_section (xpm_label M.o_xpm_normal_user)
    "" filename_option ""

let xpm_priority_0 = define_option mldonkey_gui_section (xpm_label M.o_xpm_priority_0)
    "" filename_option ""
let xpm_priority_1 = define_option mldonkey_gui_section (xpm_label M.o_xpm_priority_1)
    "" filename_option ""
let xpm_priority_2 = define_option mldonkey_gui_section (xpm_label M.o_xpm_priority_2)
    "" filename_option ""

let xpm_mimetype_binary = define_option mldonkey_gui_section (xpm_label M.o_xpm_mimetype_binary)
    "" filename_option ""
let xpm_mimetype_cdimage = define_option mldonkey_gui_section (xpm_label M.o_xpm_mimetype_cdimage)
    "" filename_option ""
let xpm_mimetype_debian = define_option mldonkey_gui_section (xpm_label M.o_xpm_mimetype_debian)
    "" filename_option ""
let xpm_mimetype_html = define_option mldonkey_gui_section (xpm_label M.o_xpm_mimetype_html)
    "" filename_option ""
let xpm_mimetype_images = define_option mldonkey_gui_section (xpm_label M.o_xpm_mimetype_images)
    "" filename_option ""
let xpm_mimetype_java = define_option mldonkey_gui_section (xpm_label M.o_xpm_mimetype_java)
    "" filename_option ""
let xpm_mimetype_pdf = define_option mldonkey_gui_section (xpm_label M.o_xpm_mimetype_pdf)
    "" filename_option ""
let xpm_mimetype_postscript = define_option mldonkey_gui_section (xpm_label M.o_xpm_mimetype_postscript)
    "" filename_option ""
let xpm_mimetype_real = define_option mldonkey_gui_section (xpm_label M.o_xpm_mimetype_real)
    "" filename_option ""
let xpm_mimetype_recycled = define_option mldonkey_gui_section (xpm_label M.o_xpm_mimetype_recycled)
    "" filename_option ""
let xpm_mimetype_rpm = define_option mldonkey_gui_section (xpm_label M.o_xpm_mimetype_rpm)
    "" filename_option ""
let xpm_mimetype_shellscript = define_option mldonkey_gui_section (xpm_label M.o_xpm_mimetype_shellscript)
    "" filename_option ""
let xpm_mimetype_soffice = define_option mldonkey_gui_section (xpm_label M.o_xpm_mimetype_soffice)
    "" filename_option ""
let xpm_mimetype_sound = define_option mldonkey_gui_section (xpm_label M.o_xpm_mimetype_sound)
    "" filename_option ""
let xpm_mimetype_source = define_option mldonkey_gui_section (xpm_label M.o_xpm_mimetype_source)
    "" filename_option ""
let xpm_mimetype_spreadsheet = define_option mldonkey_gui_section (xpm_label M.o_xpm_mimetype_spreadsheet)
    "" filename_option ""
let xpm_mimetype_tex = define_option mldonkey_gui_section (xpm_label M.o_xpm_mimetype_tex)
    "" filename_option ""
let xpm_mimetype_text = define_option mldonkey_gui_section (xpm_label M.o_xpm_mimetype_text)
    "" filename_option ""
let xpm_mimetype_tgz = define_option mldonkey_gui_section (xpm_label M.o_xpm_mimetype_tgz)
    "" filename_option ""
let xpm_mimetype_video = define_option mldonkey_gui_section (xpm_label M.o_xpm_mimetype_video)
    "" filename_option ""
let xpm_mimetype_wordprocessing = define_option mldonkey_gui_section (xpm_label M.o_xpm_mimetype_wordprocessing)
    "" filename_option ""
let xpm_mimetype_unknown = define_option mldonkey_gui_section (xpm_label M.o_xpm_mimetype_unknown)
    "" filename_option ""

let xpm_tree_closed = define_option mldonkey_gui_section (xpm_label M.o_xpm_tree_closed)
    "" filename_option ""
let xpm_tree_opened = define_option mldonkey_gui_section (xpm_label M.o_xpm_tree_opened)
    "" filename_option ""

let xpm_bt_net_on = define_option mldonkey_gui_section (xpm_label M.o_xpm_bt_net_on)
    "" filename_option ""
let xpm_dc_net_on = define_option mldonkey_gui_section (xpm_label M.o_xpm_dc_net_on)
    "" filename_option ""
let xpm_ed2k_net_on = define_option mldonkey_gui_section (xpm_label M.o_xpm_ed2k_net_on)
    "" filename_option ""
let xpm_ftt_net_on = define_option mldonkey_gui_section (xpm_label M.o_xpm_ftt_net_on)
    "" filename_option ""
let xpm_gnut_net_on = define_option mldonkey_gui_section (xpm_label M.o_xpm_gnut_net_on)
    "" filename_option ""
let xpm_nap_net_on = define_option mldonkey_gui_section (xpm_label M.o_xpm_nap_net_on)
    "" filename_option ""
let xpm_slsk_net_on = define_option mldonkey_gui_section (xpm_label M.o_xpm_slsk_net_on)
    "" filename_option ""
let xpm_mld_tux_on = define_option mldonkey_gui_section (xpm_label M.o_xpm_mld_tux_on)
    "" filename_option ""
let xpm_bt_net_off = define_option mldonkey_gui_section (xpm_label M.o_xpm_bt_net_off)
    "" filename_option ""
let xpm_dc_net_off = define_option mldonkey_gui_section (xpm_label M.o_xpm_dc_net_off)
    "" filename_option ""
let xpm_ed2k_net_off = define_option mldonkey_gui_section (xpm_label M.o_xpm_ed2k_net_off)
    "" filename_option ""
let xpm_ftt_net_off = define_option mldonkey_gui_section (xpm_label M.o_xpm_ftt_net_off)
    "" filename_option ""
let xpm_gnut_net_off = define_option mldonkey_gui_section (xpm_label M.o_xpm_gnut_net_off)
    "" filename_option ""
let xpm_nap_net_off = define_option mldonkey_gui_section (xpm_label M.o_xpm_nap_net_off)
    "" filename_option ""
let xpm_slsk_net_off = define_option mldonkey_gui_section (xpm_label M.o_xpm_slsk_net_off)
    "" filename_option ""
let xpm_mld_tux_off = define_option mldonkey_gui_section (xpm_label M.o_xpm_mld_tux_off)
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
    ["toolbars_style"] 
    ( M.h_toolbars_style)
    class_tbstyle `ICONS

let mini_toolbars = define_option mldonkey_gui_section
    ["mini_toolbars"] 
    ( M.h_mini_toolbars)
    bool_option true

(** {2 Layout} *)

let servers_vpane_up = define_option mldonkey_gui_section
    ["layout"; "servers_vpane_up"]
    ( M.h_servers_vpane_up)
    int_option 72

let friends_hpane_left = define_option mldonkey_gui_section
    ["layout"; "friends_hpane_left"]
    ( M.h_friends_hpane_left)
    int_option 17

let friends_vpane_up = define_option mldonkey_gui_section
    ["layout"; "friends_vpane_up"]
    ( M.h_friends_vpane_up)
    int_option 69

let friends_hpane_dirs = define_option mldonkey_gui_section
    ["layout"; "friends_hpane_dirs"]
    ( M.h_friends_hpane_dirs)
    int_option 13

let rooms_hpane_left = define_option mldonkey_gui_section
    ["layout"; "rooms_hpane_left"]
    ( M.h_rooms_hpane_left)
    int_option 30

let rooms_hpane2_left = define_option mldonkey_gui_section
    ["layout"; "rooms_hpane2_left"]
    ( M.h_rooms_hpane2_left)
    int_option 50

let rooms_vpane_up = define_option mldonkey_gui_section
    ["layout"; "rooms_vpane_up"]
    ( M.h_rooms_vpane_up)
    int_option 50

let uploads_vpane_up = define_option mldonkey_gui_section
    ["layout"; "uploads_vpane_up"]
    ( M.h_uploads_vpane_up)
    int_option 72

let gui_width = define_option mldonkey_gui_section
    ["layout"; "width"]
    ( M.h_gui_width)
    int_option 600

let gui_height = define_option mldonkey_gui_section
    ["layout"; "height"]
    ( M.h_gui_height)
    int_option 400

let last_tab = define_option mldonkey_gui_section
    ["layout"; "last_tab"]
    ( M.h_last_tab) 
    int_option 0

let notebook_tab = define_option mldonkey_gui_section
    ["layout"; "tab_position"]
    ( M.h_tab_position) 
    TabPosition.t `TOP

(** {2 List columns} *)

(** {3 Files} *)


let downloads_columns = define_option mldonkey_gui_section
    ["downloads_columns"] 
    ( M.h_downloads_columns)
    (list_option C.File.class_column)
  [
   C.Col_file_network; C.Col_file_priority ;
   C.Col_file_name ; C.Col_file_availability ;
   C.Col_file_size ; C.Col_file_downloaded ;
   C.Col_file_percent ; C.Col_file_rate ;
   C.Col_file_state ; C.Col_file_eta;
   C.Col_file_status;
  ]

(** {3 Clients} *)

let friends_columns = define_option mldonkey_gui_section
    ["friends_columns"] 
    ( M.h_friends_columns)
    (list_option C.Client.class_column)
  [
   C.Col_client_name ;
  ]

let file_locations_columns = define_option mldonkey_gui_section
    ["file_locations_columns"] 
    ( M.h_file_locations_columns)
    (list_option C.Client.class_column)
  [
   C.Col_client_network ; C.Col_client_name ;
   C.Col_client_kind ; C.Col_client_state;
   C.Col_client_rating ; C.Col_client_connect_time ;
   C.Col_client_software ;
   C.Col_client_downloaded ; C.Col_client_uploaded ;
   C.Col_client_upload ; C.Col_client_sock_addr ;
  ]

(** {3 Users} *)

let users_columns = define_option mldonkey_gui_section
    ["users_columns"] ( M.h_users_columns)
    (list_option C.User.class_column)
  [
   C.Col_user_name ; C.Col_user_kind ;
   C.Col_user_tags ;
  ]

let rooms_columns = define_option mldonkey_gui_section
    ["rooms_columns"] 
    ( M.h_rooms_columns)
    (list_option C.Room.class_column)
  [
   C.Col_room_network ; C.Col_room_name ;
   C.Col_room_nusers ;
  ]

(** {3 Servers} *)

let servers_columns = define_option mldonkey_gui_section
    ["server_columns"] 
    ( M.h_servers_columns)
    (list_option C.Server.class_column)
  [
   C.Col_server_network ; C.Col_server_name ;
   C.Col_server_address ; C.Col_server_state ;
   C.Col_server_users ; C.Col_server_files ;
   C.Col_server_desc ;
  ]

(** {3 Results} *)

let results_columns = define_option mldonkey_gui_section
    ["results_columns"] 
    ( M.h_results_columns)
    (list_option C.Result.class_column)
  [
   C.Col_result_network ; C.Col_result_name ;
   C.Col_result_size ; C.Col_result_format ;
   C.Col_result_duration ; C.Col_result_codec ;
   C.Col_result_bitrate ; C.Col_result_availability ;
   C.Col_result_completesources ; C.Col_result_comment ;
  ]


(** {3 Upload info} *)

let shared_files_up_columns = define_option mldonkey_gui_section
    ["shared_files_up_columns"] 
    ( M.h_shared_files_up_columns)
    (list_option C.Shared_files_up.class_column)
  [
   C.Col_shared_file ; C.Col_shared_size;
   C.Col_shared_requests ; C.Col_shared_upsize ;
  ] 

(** {2 Others} *)

let login = define_option mldonkey_gui_section 
    ["login"]
    ( M.h_login)
    string_option "admin"

let password = define_option mldonkey_gui_section 
    ["password"] 
    ( M.h_gui_password) 
    string_option ""

let port = define_option mldonkey_gui_section 
    ["port"] 
    ( M.h_gui_port) 
    port_option 4001

let hostname = define_option mldonkey_gui_section 
    ["hostname"] 
    ( M.h_hostname) 
    string_option "localhost"

let history = define_option mldonkey_gui_section 
    ["history"]
    ( M.h_history)
    (list_option (tuple2_option (string_option, int_option))) []
  
(** Max Depth used to auto expand the trees in the Friends Tab. *)
let files_auto_expand_depth = define_option mldonkey_gui_section 
    ["files_auto_expand_depth"]
    ( M.h_files_auto_expand_depth) 
    int_option 3

(** Whether we must print sizes in bytes or use G, M and k suffixes. *)
let use_size_suffixes = define_option mldonkey_gui_section
    ["use_size_suffixes"]
    ( M.h_use_size_suffixes) 
    bool_option true

let use_availability_height = define_option mldonkey_gui_section 
    ["availability_height"]
    ( M.h_use_availability_height)
    bool_option true

let availability_max = define_option mldonkey_gui_section 
    ["availability_max"]
    ( M.h_availability_max)
    int_option 10
  
let use_relative_availability = define_option mldonkey_gui_section 
    ["relative_availability"]
    ( M.h_use_relative_availability)
    bool_option true

let use_icons = define_option mldonkey_gui_section 
    ["use_icons"]
    ( M.h_use_icons)
    bool_option true

let use_graphical_availability = define_option mldonkey_gui_section 
    ["graphical_availability"]
    ( M.h_use_graphical_availability)
    bool_option true

let max_file_name_len = define_option mldonkey_gui_section 
    ["max_file_name_len"] 
    ( M.h_max_file_name_len)
    int_option 70

let max_client_name_len = define_option mldonkey_gui_section 
    ["max_client_name_len"]
    ( M.h_max_client_name_len)
    int_option 40

let max_result_name_len = define_option mldonkey_gui_section 
    ["max_result_name_len"]
    ( M.h_max_result_name_len)
    int_option 70
    
let compaction_overhead = define_option mldonkey_gui_section 
    ["compaction_overhead"]
    ( M.h_compaction_overhead)
    int_option 50

let interface_buffer = define_option mldonkey_gui_section
    ["interface_buffer"] 
    ( M.h_interface_buffer)
    int_option 10000000

  
let _ =
  option_hook compaction_overhead (fun _ ->
      let gc_control = Gc.get () in
      Gc.set { gc_control with Gc.max_overhead = !!compaction_overhead };     
  )

let copy_messages = define_option mldonkey_gui_section
    ["copy_messages"]
    ( M.h_copy_messages) 
    bool_option true

(** {2 Graph Options} *)
let max_download_rate =  define_option mldonkey_gui_section 
    ["max_download_rate"]
    ( M.h_max_download_rate) 
    int_option 64

let max_upload_rate =  define_option mldonkey_gui_section 
    ["max_upload_rate"]
    ( M.h_max_upload_rate) 
    int_option 16

let download_time_range =  define_option mldonkey_gui_section 
    ["download_time_range"]
    ( M.h_download_time_range) 
    string_option "6h"

let upload_time_range = define_option mldonkey_gui_section 
    ["upload_time_range"]
    ( M.h_upload_time_range) 
    string_option "6h"

let color_bg_download =  define_option mldonkey_gui_section 
    ["bg_download"]
    ( M.h_col_bg_download) 
    string_option "#454b55"

let color_bg_upload = define_option mldonkey_gui_section 
    ["bg_upload"]
    ( M.h_col_bg_upload) 
    string_option "#454b55"

let color_grid_download =  define_option mldonkey_gui_section 
    ["grid_download"]
    ( M.h_col_grid_download) 
    string_option "#fffcc6"

let color_grid_upload = define_option mldonkey_gui_section 
    ["grid_upload"]
    ( M.h_col_grid_upload) 
    string_option "#fffcc6"

let color_fg_download = define_option mldonkey_gui_section 
    ["fg_download"]
    ( M.h_col_fg_download) 
    string_option "#83afff"

let color_fg_upload = define_option mldonkey_gui_section 
    ["fg_upload"]
    ( M.h_col_fg_upload) 
    string_option "#6eec8b"

let color_fg_download_av = define_option mldonkey_gui_section 
    ["fg_download_av"]
    ( M.h_col_fg_download_av) 
    string_option "#2d62c1"

let color_fg_upload_av = define_option mldonkey_gui_section 
    ["fg_upload_av"]
    ( M.h_col_fg_upload_av) 
    string_option "#61b722"


(** {2 Options du client} *)
  
let client_name = ref ""

let client_options_assocs = [
    "client_name",   client_name;    
  ]


(* 2 desaturate functions for icons*)
let rec iter i s l =
  try
    let pos = String.index_from s i ' ' in
    let l = pos::l in
    iter (pos + 1) s l
  with _ -> List.rev l

let bw_of data =
  let array = Array.copy data in
  let pixmap_header = array.(0) in
  let l = iter 0 pixmap_header [] in
  let n1 = (List.nth l 1) + 1 in
  let n2 = (List.nth l 2) - n1 in
  let n_colors = int_of_string (String.sub pixmap_header n1 n2) in
  for i = 1 to n_colors do
    let s = array.(i) in
    let len = String.length s in
    let si = String.sub s 0 (len - 6) in
    let red = String.sub s (len - 6) 2 in
    let green = String.sub s (len - 4) 2 in
    let blue = String.sub s (len - 2) 2 in
    let col = min red (max green blue) in
    let sis = si ^ col ^ col ^ col in
    array.(i) <- sis
  done;
  array

let bw2_of data =
  let array = Array.copy data in
  let pixmap_header = array.(0) in
  let l = iter 0 pixmap_header [] in
  let n1 = (List.nth l 1) + 1 in
  let n2 = (List.nth l 2) - n1 in
  let n_colors = int_of_string (String.sub pixmap_header n1 n2) in
  for i = 1 to n_colors do
    let s = array.(i) in
    let len = String.length s in
    let si = String.sub s 0 (len - 6) in
    let red = String.sub s (len - 6) 2 in
    let green = String.sub s (len - 4) 2 in
    let blue = String.sub s (len - 2) 2 in
    let col = max red (min green blue) in
    let sis = si ^ col ^ col ^ col in
    array.(i) <- sis
  done;
  array

(* 1 very simple resize function for icons *)
let resize (pixmap : GDraw.pixmap) (scale : float) =
  let (width, height) = pixmap#size in
  let new_width = int_of_float (float_of_int width *. scale) in
  let new_height = int_of_float (float_of_int height *. scale) in
  let new_pixmap =
    GDraw.pixmap ~width:new_width ~height:new_height ~mask:true
      ~colormap:(Gdk.Color.get_system_colormap ()) ()
  in
  let img = Gdk.Image.get pixmap#pixmap ~x:0 ~y:0 ~width:width ~height:height in
  let new_img = Gdk.Image.get new_pixmap#pixmap ~x:0 ~y:0 ~width:new_width ~height:new_height in
  for i = 0 to (new_width - 1) do
    for j = 0 to (new_height - 1) do
      let new_i = min width (int_of_float (float_of_int i /. scale)) in
      let new_j = min height (int_of_float (float_of_int j /. scale)) in
      let pixel = Gdk.Image.get_pixel img ~x:new_i ~y:new_j in
      Gdk.Image.put_pixel new_img ~x:i ~y:j ~pixel;
    done
  done;
  new_pixmap#put_image ~x:0 ~y:0 ~xsrc:0 ~ysrc:0 ~width:new_width ~height:new_height new_img;
  new_pixmap



let table = [
  M.o_xpm_toggle_display_all_servers, (
      Toggle_display_all_servers_xpm.t, xpm_toggle_display_all_servers);
  M.o_xpm_add_server, (Add_server_xpm.t, xpm_add_server);
  M.o_xpm_submit_search, (Submit_search_xpm.t, xpm_submit_search);
  M.o_xpm_extend_search, (Extend_search_xpm.t, xpm_extend_search);
  M.o_xpm_local_search, (Local_search_xpm.t, xpm_local_search);
  M.o_xpm_subscribe_search, (Subscribe_search_xpm.t, xpm_subscribe_search);
  M.o_xpm_stop_search, (Stop_search_xpm.t, xpm_stop_search);
  M.o_xpm_close_search, (Close_search_xpm.t, xpm_close_search);
  M.o_xpm_close_room, (Trash_xpm.t, xpm_close_room) ;
  M.o_xpm_add_shared_directory, (Add_shared_directory_xpm.t, xpm_add_shared_directory);
  M.o_xpm_download_directory, (Download_directory_xpm.t, xpm_download_directory);
  M.o_xpm_view_pending_slots,(View_pending_slots_xpm.t, xpm_view_pending_slots); 

  M.o_xpm_mini_toggle_display_all_servers, (
      Toggle_display_all_servers_xpm.mini, xpm_mini_toggle_display_all_servers);
  M.o_xpm_mini_add_server, (Add_server_xpm.mini, xpm_mini_add_server);
  M.o_xpm_mini_submit_search, (Submit_search_xpm.mini, xpm_mini_submit_search);
  M.o_xpm_mini_extend_search, (Extend_search_xpm.mini, xpm_mini_extend_search);
  M.o_xpm_mini_local_search, (Local_search_xpm.mini, xpm_mini_local_search);
  M.o_xpm_mini_subscribe_search, (Subscribe_search_xpm.mini, xpm_mini_subscribe_search);
  M.o_xpm_mini_stop_search, (Stop_search_xpm.mini, xpm_mini_stop_search);
  M.o_xpm_mini_close_search, (Close_search_xpm.mini, xpm_mini_close_search);
  M.o_xpm_mini_close_room, (Trash_xpm.mini, xpm_mini_close_room) ;
  M.o_xpm_mini_add_shared_directory, (Add_shared_directory_xpm.mini, xpm_mini_add_shared_directory);
  M.o_xpm_mini_download_directory, (Download_directory_xpm.mini, xpm_mini_download_directory);
  M.o_xpm_mini_view_pending_slots,(View_pending_slots_xpm.mini, xpm_mini_view_pending_slots); 

  M.o_xpm_nbk_networks_on, (Nbk_networks_on_xpm.t, xpm_nbk_networks_on);
  M.o_xpm_nbk_networks_off, (bw_of Nbk_networks_on_xpm.t, xpm_nbk_networks_off);
  M.o_xpm_nbk_networks_menu, (Nbk_networks_menu_xpm.t, xpm_nbk_networks_menu);
  M.o_xpm_nbk_servers_on, (Nbk_servers_on_xpm.t, xpm_nbk_servers_on);
  M.o_xpm_nbk_servers_off, (bw_of Nbk_servers_on_xpm.t, xpm_nbk_servers_off);
  M.o_xpm_nbk_servers_menu, (Nbk_servers_menu_xpm.t, xpm_nbk_servers_menu);
  M.o_xpm_nbk_downloads_on, (Nbk_downloads_on_xpm.t, xpm_nbk_downloads_on);
  M.o_xpm_nbk_downloads_off, (bw_of Nbk_downloads_on_xpm.t, xpm_nbk_downloads_off);
  M.o_xpm_nbk_downloads_menu, (Nbk_downloads_menu_xpm.t, xpm_nbk_downloads_menu);
  M.o_xpm_nbk_friends_on, (Nbk_friends_on_xpm.t, xpm_nbk_friends_on);
  M.o_xpm_nbk_friends_off, (bw_of Nbk_friends_on_xpm.t, xpm_nbk_friends_off);
  M.o_xpm_nbk_friends_menu, (Nbk_friends_menu_xpm.t, xpm_nbk_friends_menu);
  M.o_xpm_nbk_search_on, (Nbk_search_on_xpm.t, xpm_nbk_search_on);
  M.o_xpm_nbk_search_off, (bw_of Nbk_search_on_xpm.t, xpm_nbk_search_off);
  M.o_xpm_nbk_search_menu, (Nbk_search_menu_xpm.t, xpm_nbk_search_menu);
  M.o_xpm_nbk_rooms_on, (Nbk_rooms_on_xpm.t, xpm_nbk_rooms_on);
  M.o_xpm_nbk_rooms_off, (bw_of Nbk_rooms_on_xpm.t, xpm_nbk_rooms_off);
  M.o_xpm_nbk_rooms_menu, (Nbk_rooms_menu_xpm.t, xpm_nbk_rooms_menu);
  M.o_xpm_nbk_uploads_on, (Nbk_uploads_on_xpm.t, xpm_nbk_uploads_on);
  M.o_xpm_nbk_uploads_off, (bw_of Nbk_uploads_on_xpm.t, xpm_nbk_uploads_off);
  M.o_xpm_nbk_uploads_menu, (Nbk_uploads_menu_xpm.t, xpm_nbk_uploads_menu);
  M.o_xpm_nbk_console_on, (Nbk_console_on_xpm.t, xpm_nbk_console_on);
  M.o_xpm_nbk_console_off, (bw_of Nbk_console_on_xpm.t, xpm_nbk_console_off);
  M.o_xpm_nbk_console_menu, (Nbk_console_menu_xpm.t, xpm_nbk_console_menu);
  M.o_xpm_nbk_graphs_on, (Nbk_graphs_on_xpm.t, xpm_nbk_graphs_on);
  M.o_xpm_nbk_graphs_off, (bw_of Nbk_graphs_on_xpm.t, xpm_nbk_graphs_off);
  M.o_xpm_nbk_graphs_menu, (Nbk_graphs_menu_xpm.t, xpm_nbk_graphs_menu);

  M.o_xpm_about, (About_xpm.t, xpm_about);
  M.o_xpm_settings, (Settings_xpm.t, xpm_settings);
  M.o_xpm_exit, (Exit_xpm.t, xpm_exit);
  M.o_xpm_gui, (Gui_xpm.t, xpm_gui);
  M.o_xpm_kill_core, (Kill_core_xpm.t, xpm_kill_core);
  M.o_xpm_splash_screen, (Splash_screen_xpm.t, xpm_splash_screen);

  M.o_xpm_album_search, (Album_search_xpm.t, xpm_album_search);
  M.o_xpm_movie_search, (Movie_search_xpm.t, xpm_movie_search);
  M.o_xpm_mp3_search, (Mp3_search_xpm.t, xpm_mp3_search);
  M.o_xpm_complex_search, (Complex_search_xpm.t, xpm_complex_search);
  M.o_xpm_sharereactor_search, (Sharereactor_search_xpm.t, xpm_sharereactor_search);
  M.o_xpm_jigle_search, (Jigle_search_xpm.t, xpm_jigle_search);
  M.o_xpm_freedb_search, (Freedb_search_xpm.t, xpm_freedb_search);
  M.o_xpm_imdb_search, (Imdb_search_xpm.t, xpm_imdb_search);

  M.o_xpm_bt, (Bt_xpm.t, xpm_bt);
  M.o_xpm_dc, (Dc_xpm.t, xpm_dc);
  M.o_xpm_ed2k, (Ed2k_xpm.t, xpm_ed2k);
  M.o_xpm_fasttrack, (Fasttrack_xpm.t, xpm_fasttrack);
  M.o_xpm_gnutella, (Gnutella_xpm.t, xpm_gnutella);
  M.o_xpm_napster, (Napster_xpm.t, xpm_napster);
  M.o_xpm_slsk, (Slsk_xpm.t, xpm_slsk);
  M.o_xpm_unknown, (Unknown_xpm.t, xpm_unknown);

  M.o_xpm_downloading, (Downloading_xpm.t, xpm_downloading);
  M.o_xpm_connect_y, (Connect_y_xpm.t, xpm_connect_y);
  M.o_xpm_connect_m, (Connect_m_xpm.t, xpm_connect_m);
  M.o_xpm_connect_n, (Connect_n_xpm.t, xpm_connect_n);
  M.o_xpm_removedhost, (Removedhost_xpm.t, xpm_removedhost);
  M.o_xpm_blacklistedhost, (Blacklistedhost_xpm.t, xpm_blacklistedhost);
  M.o_xpm_files_listed, (Files_listed_xpm.t, xpm_files_listed);
  
  M.o_xpm_server_c_low, (Server_c_low_xpm.t, xpm_server_c_low);
  M.o_xpm_server_c_high, (Server_c_high_xpm.t, xpm_server_c_high);
  M.o_xpm_server_ci, (Server_ci_xpm.t, xpm_server_ci);
  M.o_xpm_server_nc, (Server_nc_xpm.t, xpm_server_nc);

  M.o_xpm_friend_user, (Friend_user_xpm.t, xpm_friend_user);
  M.o_xpm_contact_user, (Contact_user_xpm.t, xpm_contact_user);
  M.o_xpm_normal_user, (Normal_user_xpm.t, xpm_normal_user);

  M.o_xpm_priority_0, (Priority_0_xpm.t, xpm_priority_0);
  M.o_xpm_priority_1, (Priority_1_xpm.t, xpm_priority_1);
  M.o_xpm_priority_2, (Priority_2_xpm.t, xpm_priority_2);

  M.o_xpm_mimetype_binary, (Mimetype_binary_xpm.t, xpm_mimetype_binary);
  M.o_xpm_mimetype_cdimage, (Mimetype_cdimage_xpm.t, xpm_mimetype_cdimage);
  M.o_xpm_mimetype_debian, (Mimetype_debian_xpm.t, xpm_mimetype_debian);
  M.o_xpm_mimetype_html, (Mimetype_html_xpm.t, xpm_mimetype_html);
  M.o_xpm_mimetype_images, (Mimetype_images_xpm.t, xpm_mimetype_images);
  M.o_xpm_mimetype_java, (Mimetype_java_xpm.t, xpm_mimetype_java);
  M.o_xpm_mimetype_pdf, (Mimetype_pdf_xpm.t, xpm_mimetype_pdf);
  M.o_xpm_mimetype_postscript, (Mimetype_postscript_xpm.t, xpm_mimetype_postscript);
  M.o_xpm_mimetype_real, (Mimetype_real_xpm.t, xpm_mimetype_real);
  M.o_xpm_mimetype_recycled, (Mimetype_recycled_xpm.t, xpm_mimetype_recycled);
  M.o_xpm_mimetype_rpm, (Mimetype_rpm_xpm.t, xpm_mimetype_rpm);
  M.o_xpm_mimetype_shellscript, (Mimetype_shellscript_xpm.t, xpm_mimetype_shellscript);
  M.o_xpm_mimetype_soffice, (Mimetype_soffice_xpm.t, xpm_mimetype_soffice);
  M.o_xpm_mimetype_sound, (Mimetype_sound_xpm.t, xpm_mimetype_sound);
  M.o_xpm_mimetype_source, (Mimetype_source_xpm.t, xpm_mimetype_source);
  M.o_xpm_mimetype_spreadsheet, (Mimetype_spreadsheet_xpm.t, xpm_mimetype_spreadsheet);
  M.o_xpm_mimetype_tex, (Mimetype_tex_xpm.t, xpm_mimetype_tex);
  M.o_xpm_mimetype_text, (Mimetype_text_xpm.t, xpm_mimetype_text);
  M.o_xpm_mimetype_tgz, (Mimetype_tgz_xpm.t, xpm_mimetype_tgz);
  M.o_xpm_mimetype_video, (Mimetype_video_xpm.t, xpm_mimetype_video);
  M.o_xpm_mimetype_wordprocessing, (Mimetype_wordprocessing_xpm.t, xpm_mimetype_wordprocessing);
  M.o_xpm_mimetype_unknown, (Mimetype_unknown_xpm.t, xpm_mimetype_unknown);

  M.o_xpm_tree_closed, (Tree_closed_xpm.t, xpm_tree_closed);
  M.o_xpm_tree_opened, (Tree_opened_xpm.t, xpm_tree_opened);

  M.o_xpm_bt_net_on, (Bt_net_on_xpm.t, xpm_bt_net_on);
  M.o_xpm_dc_net_on, (Dc_net_on_xpm.t, xpm_dc_net_on);
  M.o_xpm_ed2k_net_on, (Ed2k_net_on_xpm.t, xpm_ed2k_net_on);
  M.o_xpm_ftt_net_on, (Ftt_net_on_xpm.t, xpm_ftt_net_on);
  M.o_xpm_gnut_net_on, (Gnut_net_on_xpm.t, xpm_gnut_net_on);
  M.o_xpm_nap_net_on, (Nap_net_on_xpm.t, xpm_nap_net_on);
  M.o_xpm_slsk_net_on, (Slsk_net_on_xpm.t, xpm_slsk_net_on);
  M.o_xpm_mld_tux_on, (Mld_tux_on_xpm.t, xpm_mld_tux_on);
  M.o_xpm_bt_net_off, (bw_of Bt_net_on_xpm.t, xpm_bt_net_off);
  M.o_xpm_dc_net_off, (bw_of Dc_net_on_xpm.t, xpm_dc_net_off);
  M.o_xpm_ed2k_net_off, (bw_of Ed2k_net_on_xpm.t, xpm_ed2k_net_off);
  M.o_xpm_ftt_net_off, (bw_of Ftt_net_on_xpm.t, xpm_ftt_net_off);
  M.o_xpm_gnut_net_off, (bw_of Gnut_net_on_xpm.t, xpm_gnut_net_off);
  M.o_xpm_nap_net_off, (bw_of Nap_net_on_xpm.t, xpm_nap_net_off);
  M.o_xpm_slsk_net_off, (bw_of Slsk_net_on_xpm.t, xpm_slsk_net_off);
  M.o_xpm_mld_tux_off, (bw_of Mld_tux_on_xpm.t, xpm_mld_tux_off);

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

let network_pix net =
    match net with
      "BitTorrent" -> gdk_pix M.o_xpm_bt
    | "DC" -> gdk_pix M.o_xpm_dc
    | "eDK" -> gdk_pix M.o_xpm_ed2k
    | "NAP" -> gdk_pix M.o_xpm_napster
    | "GTL" -> gdk_pix M.o_xpm_gnutella
    | "FT" -> gdk_pix M.o_xpm_fasttrack
    | "SLSK" -> gdk_pix M.o_xpm_slsk
    | _ -> gdk_pix M.o_xpm_unknown


(* popup window for icons management *)
open GMain

let timerID = ref (Timeout.add ~ms:1000 ~callback:(fun _ -> true))

class pbar_window () =
  let window = GWindow.window ~border_width:10 ~kind:`POPUP
    ~position:`CENTER_ALWAYS () in
  let vbox = GPack.vbox ~homogeneous:true ~packing:window#add () in
  let label = GMisc.label
    ~packing:(vbox#pack ~fill:true ~expand:false) ()
  in
  let pbar =
    GRange.progress_bar ~bar_style:`CONTINUOUS ~discrete_blocks:20
      ~packing:(vbox#pack ~fill:true ~expand:true) ()
  in
  object
    val window = window
    val vbox = vbox
    val pbar = pbar
    val label = label
    method window = window
    method vbox = vbox
    method pbar = pbar
    method label = label
    method set_progress p =
      let p = if p >= 1. then 1. else p in
      pbar#set_percentage p
end


let generate_with_progress txt list f step =
  let w = new pbar_window () in
  w#label#set_text txt;
  w#pbar#set_show_text true;
  w#pbar#set_format_string "(%p%%)";
  w#set_progress 0.;
  w#window#show ();
  ignore (w#window#connect#destroy Main.quit);
  let len = List.length list in
  let rec iter (step : int) (l : 'a list) (li : 'a list list) =
    let le = List.length l in
    if le <= step then
      begin
        let li = l::li in
        li
      end
      else begin
        let lis = Array.to_list (Array.sub (Array.of_list l) 0 step) in
        let li = lis::li in
        iter step (Array.to_list (Array.sub (Array.of_list l) step (le - step))) li
      end
  in
  (* let step = max 1 (len / 10) in *)
  let new_l = iter step list [] in
  (* Printf.printf "new_l length %d\n" (List.length new_l);
  flush stdout;*)
  let c = ref 0 in
  let count = ref 0 in
  let flush_list _ =
    let li = List.nth new_l !c in
    if !c = (List.length new_l - 1) then
      begin
        (* Printf.printf "flush finished c = %d\n" !c;
        flush stdout; *)
        List.iter f li;
        Timeout.remove (!timerID);
        w#window#destroy ()
      end else
      begin
        (* Printf.printf "flush c = %d\n" !c;
        flush stdout; *)
        List.iter f li;
        incr (c)
      end
  in
  timerID :=
    (Timeout.add ~ms:10
           ~callback:(fun _ ->
             count := !count + List.length (List.nth new_l !c);
             (* Printf.printf "list length - partial : %d total : %d\n" (List.length (List.nth new_l !c)) len;
             flush stdout;
             Printf.printf "count = %d\n" !count;
             flush stdout;*)
             flush_list ();
             w#set_progress ((float_of_int !count) /. (float_of_int len +. 1.));
             true));
  GMain.Main.main ()
