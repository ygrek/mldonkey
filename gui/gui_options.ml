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

let keymap_options = define_option mldonkey_gui_ini ["keymaps"; "options"]
  "Key bindings for options tab" 
  (list_option (tuple2_option (KeyOption.t, string_option))) []

let keymap_console = define_option mldonkey_gui_ini ["keymaps"; "console"]
  "Key bindings for console tab" 
    (list_option (tuple2_option (KeyOption.t, string_option))) []
  
let add_binding map binding action = 
  map =:= 
    (KeyOption.string_to_key binding, action) :: !!map
  
(** {2 Colors} *)

let color_default = define_option mldonkey_gui_ini ["colors"; "default"]
    "" string_option "Black"
let color_downloaded =  define_option mldonkey_gui_ini ["colors"; "downloaded"]
    "" string_option "Blue"
let color_downloading =  define_option mldonkey_gui_ini 
  ["colors"; "downloading"]
    "" string_option "DarkGreen"
let color_available =  define_option mldonkey_gui_ini ["colors"; "available"]
    "" string_option "Orange"
let color_not_available =  define_option mldonkey_gui_ini ["colors"; "not_available"]
    "" string_option "Red"
let color_connected =  define_option mldonkey_gui_ini ["colors"; "connected"]
    "" string_option "DarkGreen"
let color_not_connected =  define_option mldonkey_gui_ini ["colors"; "not_connected"]
    "" string_option "Black"
let color_connecting =  define_option mldonkey_gui_ini ["colors"; "connecting"]
    "" string_option "Orange"
let color_files_listed =  define_option mldonkey_gui_ini ["colors"; "files_listed"]
    "" string_option "Blue"

(** {2 Layout} *)

let servers_hpane_left = define_option mldonkey_gui_ini
    ["layout"; "servers_hpane_left"]
    "Size in % of left part of the servers hpane"
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

let gui_width = define_option mldonkey_gui_ini
    ["layout"; "width"]
    "Width of GUI window" int_option 600

let gui_height = define_option mldonkey_gui_ini
    ["layout"; "height"]
  "Height of GUI window" int_option 400
  
(** {2 Others} *)

let password = define_option mldonkey_gui_ini ["password"] "" string_option ""
let port = define_option mldonkey_gui_ini ["port"] "" int_option 4001
let hostname = define_option mldonkey_gui_ini ["hostname"] "" string_option 
  "localhost"

  
  
let max_client_name_len = define_option mldonkey_gui_ini
    ["max_client_name_len"] "Max len of a source name"
    int_option 18
    
let compaction_overhead = define_option mldonkey_gui_ini 
    ["compaction_overhead"] 
    "The percentage of free memory before a compaction is triggered"
    int_option 50

let auto_resize = define_option mldonkey_gui_ini
    ["auto_resize"]
  "auto resize columns" bool_option true
  
let _ =
  option_hook compaction_overhead (fun _ ->
      let gc_control = Gc.get () in
      Gc.set { gc_control with Gc.max_overhead = !!compaction_overhead };     
  )
  
  