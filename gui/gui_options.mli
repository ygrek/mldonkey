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

val mldonkey_gui_ini : Options.options_file
  
val keymap_global :
  ((Gdk.Tags.modifier list * int) * string) list Options.option_record
val keymap_servers :
  ((Gdk.Tags.modifier list * int) * string) list Options.option_record
val keymap_downloads :
  ((Gdk.Tags.modifier list * int) * string) list Options.option_record
val keymap_friends :
  ((Gdk.Tags.modifier list * int) * string) list Options.option_record
val keymap_queries :
  ((Gdk.Tags.modifier list * int) * string) list Options.option_record
val keymap_options :
  ((Gdk.Tags.modifier list * int) * string) list Options.option_record
val keymap_console :
  ((Gdk.Tags.modifier list * int) * string) list Options.option_record

  
val add_binding :
  ((Gdk.Tags.modifier list * int) * string)
  list Options.option_record -> string -> string -> unit
  
val color_default : string Options.option_record
val color_downloaded : string Options.option_record
val color_downloading : string Options.option_record
val color_available : string Options.option_record
val color_not_available : string Options.option_record
val color_connected : string Options.option_record
val color_not_connected : string Options.option_record
val color_connecting : string Options.option_record
val color_files_listed : string Options.option_record

val servers_hpane_left : int Options.option_record
val  downloads_hpane_left : int Options.option_record
val  friends_hpane_left : int Options.option_record
val  searches_hpane_left : int Options.option_record  
val  downloads_vpane_up : int Options.option_record
val  friends_vpane_up : int Options.option_record
val gui_width : int Options.option_record
val gui_height : int Options.option_record

val password : string Options.option_record
val port : int Options.option_record
val hostname : string Options.option_record
