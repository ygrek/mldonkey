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

open Printf2
module O = GuiOptions

let (!!) = Options.(!!)
let (=:=) = Options.(=:=)

(*************************************************************************)
(*                                                                       *)
(*                         Global variables                              *)
(*                                                                       *)
(*************************************************************************)

(* The name of the client given by MLnet. *)
let client_name = ref ""

(* The max download rate of the client given by MLnet in ko/s. *)
let max_hard_download_rate = ref 64

(* The max upload rate of the client given by MLnet in ko/s. *)
let max_hard_upload_rate = ref 16

(* The time between updates of the client given by MLnet in ms. *)
let update_gui_delay = ref 1000

(* Number of known servers. *)
let nservers = ref 0

(* Number of connected servers. *)
let nconnected_servers = ref 0

(* Number of downloaded files *)
let ndownloaded = ref 0
  
(* Number of downloading files *)
let ndownloads = ref 0

(* Search counter. *)
let search_counter = ref 0

(* Core status *)
let core_status = ref GuiTypes2.Core_notconnected

let char_width = ref 0
let digit_width = ref 0

(* new protocol 27 InterestedInSources *)
let use_interested_in_sources = ref false
let interested_in_sources_version = 27

(*************************************************************************)
(*                                                                       *)
(*                         Global tables                                 *)
(*                                                                       *)
(*************************************************************************)

let networks_filtered = ref ([] : int list)

let (networks : (int, GuiTypes2.net_info) Hashtbl.t) = Hashtbl.create 13

let (files : (int, GuiTypes2.g_file_info) Hashtbl.t) = Hashtbl.create 27

let (servers : (int, GuiTypes.server_info) Hashtbl.t) = Hashtbl.create 207

let (rooms : (int, GuiTypes.room_info) Hashtbl.t) = Hashtbl.create 207

let (sources : (int, GuiTypes2.source_info) Hashtbl.t) = Hashtbl.create 1023

let (results : (int, GuiTypes2.res_info) Hashtbl.t) = Hashtbl.create 1023

let (users : (int, GuiTypes.user_info) Hashtbl.t) = Hashtbl.create 1023

let (shared_files : (int, GuiTypes2.shared_file) Hashtbl.t) = Hashtbl.create 103

let (file_by_uid : (CommonTypes.uid_type, string) Hashtbl.t) = Hashtbl.create 103

(*************************************************************************)
(*                                                                       *)
(*                         Global functions                              *)
(*                                                                       *)
(*************************************************************************)

let clear () =
(* variables *)
  nservers := 0;
  nconnected_servers := 0;
  ndownloads := 0;
  ndownloaded := 0;
  search_counter := 0;
  use_interested_in_sources := false;
(* tables    *)
  Hashtbl.clear networks;
  Hashtbl.clear files;
  Hashtbl.clear servers;
  Hashtbl.clear rooms;
  Hashtbl.clear sources;
  Hashtbl.clear results;
  Hashtbl.clear users;
  Hashtbl.clear shared_files;
  Hashtbl.clear file_by_uid;
  Gc.compact ()

let console_message = ref (fun s -> 
      lprintf "CONSOLE: %s" s;
      lprint_newline () )


let gtk_handler timer =
  while Glib.Main.pending () do
    ignore (Glib.Main.iteration false)
  done

external setncnumeric : unit -> unit = "ml_setlcnumeric" "noalloc"

let _ =
  ignore (GMain.Main.init ~setlocale: true ());
  setncnumeric ();
  BasicSocket.add_infinite_timer 0.1 gtk_handler

let scanned_ports = ref ([] : ((string * int) list))
let new_scanned_port = ref true

let get_metrics_from_gtk_font_list () =
  let label = GMisc.label () in
  label#misc#modify_font (Pango.Font.from_string !!O.gtk_font_list);
  let font_metrics = label#misc#pango_context#get_metrics () in
  char_width := GPango.to_pixels font_metrics#approx_char_width;
  digit_width := GPango.to_pixels font_metrics#approx_digit_width

let get_files list =
  let l = ref [] in
  List.iter (fun file_num ->
    try
      let f = Hashtbl.find files file_num in
      l := f :: !l
    with _ -> ()
  ) list;
  !l

let is_docked = ref false

let (tray : GuiTypes2.tray) =
  {
   GuiTypes2.create_tray = (fun _ _ -> ());
   GuiTypes2.destroy_tray = (fun _ -> ());
   GuiTypes2.set_icon_tray = (fun _ -> ());
   GuiTypes2.set_tooltip_tray = (fun _ -> ());
  }

let (set_systray_callback : ((GuiTypes2.button_types -> unit) -> unit) ref) = ref (fun _ -> ())

open GuiTypes2
open CommonTypes

let check_usefull_source s =
  let is_not_source = if s.source_files_requested = [] then true else false in
  let is_not_friend = if client_browsed_tag land s.source_type = 0 then false else true in
  let is_not_uploader = if source_only land s.source_has_upload = 1 then true else false in
  if is_not_source && is_not_friend && is_not_uploader
    then (Hashtbl.remove sources s.source_num; false)
    else true


let hashtbl_update_sources s s_new =
  s.source_kind <- s_new.source_kind;
  s.source_state <- s_new.source_state;
  s.source_type <- s_new.source_type;
  s.source_tags <- s_new.source_tags;
  s.source_name <- s_new.source_name;
  s.source_files <- s_new.source_files;
  s.source_rating <- s_new.source_rating;
  s.source_chat_port <- s_new.source_chat_port;
  s.source_connect_time <- s_new.source_connect_time;
  s.source_last_seen <- s_new.source_last_seen;
  s.source_software <- s_new.source_software;
  s.source_downloaded <- s_new.source_downloaded;
  s.source_uploaded <- s_new.source_uploaded;
  s.source_upload_rate <- s_new.source_upload_rate;
  s.source_download_rate <- s_new.source_download_rate;
  s.source_upload <- s_new.source_upload;
  s.source_has_upload <- s_new.source_has_upload;
  s.source_availability <- s_new.source_availability;
  s.source_files_requested <- s_new.source_files_requested
