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

(* Information on networks. *)
let networks_filtered = ref ([] : int list)

let (networks : (int, GuiTypes2.net_info) Hashtbl.t) = Hashtbl.create 13

(* Information on clients. *)
let (sources : (int, GuiTypes2.source_info) Hashtbl.t) = Hashtbl.create 1023

let (results : (int, GuiTypes2.res_info) Hashtbl.t) = Hashtbl.create 103


(* Information on users. *)
let (users : (int, GuiTypes.user_info) Hashtbl.t) = Hashtbl.create 1023

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
  Hashtbl.clear sources;
  Hashtbl.clear results;
  Hashtbl.clear users


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
  digit_width := GPango.to_pixels font_metrics#approx_digit_width;
  Printf.printf "char_width : %d digit_width : %d\n" !char_width !digit_width;
  flush stdout

let get_files = ref (fun (l : int list) -> ([] : GuiTypes.file_info list))

let is_docked = ref false

let (tray : GuiTypes2.tray) =
  {
   GuiTypes2.create_tray = (fun _ _ -> ());
   GuiTypes2.destroy_tray = (fun _ -> ());
   GuiTypes2.set_icon_tray = (fun _ -> ());
   GuiTypes2.set_tooltip_tray = (fun _ -> ());
  }

let (set_systray_callback : ((GuiTypes2.button_types -> unit) -> unit) ref) = ref (fun _ -> ())
