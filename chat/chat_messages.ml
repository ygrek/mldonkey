(**************************************************************************)
(*  Copyright 2003, 2002 b8_bavard, b8_zoggy, , b52_simon INRIA            *)
(*                                                                        *)
(*    This file is part of mldonkey.                                      *)
(*                                                                        *)
(*    mldonkey is free software; you can redistribute it and/or modify    *)
(*    it under the terms of the GNU General Public License as published   *)
(*    by the Free Software Foundation; either version 2 of the License,   *)
(*    or (at your option) any later version.                              *)
(*                                                                        *)
(*    mldonkey is distributed in the hope that it will be useful,         *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of      *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       *)
(*    GNU General Public License for more details.                        *)
(*                                                                        *)
(*    You should have received a copy of the GNU General Public License   *)
(*    along with mldonkey; if not, write to the Free Software             *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston,               *)
(*    MA  02111-1307  USA                                                 *)
(*                                                                        *)
(**************************************************************************)

(** Messages and string constants. *)

let software = "MLChat"
let software_version = "1.1"
let software_author = "Maxence Guesdon"
let software_author_mail = "Maxence.Guesdon@inria.fr"
let software_copyright = 
  "Copyright 2002 Institut National de Recherche en \n"^
  "Informatique et en Automatique. All rights reserved.\n"^
  "This software is distributed under the terms of the\n"^
  "GPL Public License version 2.0.\n"^
  "(see file LICENSE in the distribution)"

let software_about = 
  software^" version "^software_version^"\n\n"^
  software_author^"\n"^
  software_author_mail^"\n\n"^
  software_copyright

let home = 
  try Sys.getenv "HOME"
  with Not_found -> ""

let verbose_mode = ref false

(** {2 Command line messages} *)

let usage = "Usage : "^Sys.argv.(0)^" [options] \nwhere options are :"
let op_verbose = " verbose mode"
let op_config = "<file>  use <file> as configuration file instead of default ~/.mlchatrc"

(** Print the given string if we are in verbose mode.*)
let verbose s =
  if !verbose_mode then
    (print_string s ; print_newline ())

(** {2 Help messages} *)

let h_color_connected = "Color for connected people"
let h_color_connected_temp = "Color for connected temporary people"
let h_color_not_connected = "Color for not connected people"
let h_people = "People you know, list of (id, host, port)"
let h_port = "The port to listen to"
let h_timeout = "Timeout for listening on port (in ms)"
let h_id = "Your id"
let h_color_myself = "My color in dialog boxes"
let h_popup_all = "Popup for all incoming messages (true) or "^
  "for only people in your personal list"
let h_rooms = "The rooms you know, list of (name, list of (id, host, port))"

(** {2 Messages} *)

let people = "People"
let id = "Id"
let host = "Host"
let port = "Port"
let temporary = "Temporary"
let yes_or_no b = if b then "yes" else "no"
let incompatible_version = "Incompatible versions"
let dest_is_source = "Destination = source"
let options = "Options"
let connection = "Connection"
let colors = "Colors"
let popup_all = "Popup all"
let rooms = "Rooms"
let room_name = "Room name"
let show_hide_people = "Show/hide people"

(** {2 Menu labels} *)

let m_quit = "Quit"
let m_open_dialog_for_selected_people = "Open dialog"
let m_options = "Options"
let m_add_people = "Add people"
let m_toggle_temp = "Toggle temp flag for selected people"
let m_about = "About ..."
let m_remove_people = "Remove selected people"
let m_rooms = "Rooms"
