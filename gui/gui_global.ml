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

open Printf2
(** Global variables. *)

(** Number of known servers. *)
let nservers = ref 0

(** Number of connected servers. *)
let nconnected_servers = ref 0

(** Number of friends. *)
let nfriends = ref 0

(** Number of query results. *)
let nresults = ref 0

(** Number of locations for selected downloading file. *)
let nlocations = ref 0

(** Number of connected locations for selected downloading file. *)
let nclocations = ref 0

(** Number of downloaded files *)
let ndownloaded = ref 0
  
(** Number of downloading files *)
let ndownloads = ref 0

(** Information on locations. *)
let (locations : (int, GuiTypes.client_info) Hashtbl.t) = Hashtbl.create 103

(** Information on locations. *)
let (results : (int, CommonTypes.result_info) Hashtbl.t) = Hashtbl.create 103

(** Search counter. *)
let search_counter = ref 0

type net_info = {
    net_num : int;
    net_name : string;
    mutable net_enabled : bool;
    net_menu_item : GMenu.check_menu_item;
    mutable net_displayed : bool;
  }

let networks_filtered = ref ([] : int list)
  
open GuiTypes
  
let (networks : (int, net_info) Hashtbl.t) = Hashtbl.create 13
  
let network_name num = try (Hashtbl.find networks num).net_name
  with _ -> "?"
      
let (users : (int, GuiTypes.user_info) Hashtbl.t) = Hashtbl.create 1023

  
let (client_sections :   (string * (string * GuiTypes.option_widget * string) list ref) list ref) = ref []
let (plugins_sections :   (string * (string * GuiTypes.option_widget * string) list ref) list ref) = ref []
let (options_values : (string, string ref) Hashtbl.t) = Hashtbl.create 100

let (availabilities : (string * file_info) Intmap.t ref
    Intmap.t ref) = ref Intmap.empty

  
let clear () =
  nservers := 0;
  nconnected_servers := 0;
  nfriends := 0;
  nresults := 0;
  nlocations := 0;
  nclocations := 0;
  ndownloads := 0;
  ndownloaded := 0;
  Hashtbl.clear locations;
  Hashtbl.clear networks;
  Hashtbl.clear results;
  Hashtbl.clear users;
  search_counter := 0;
  Hashtbl.clear options_values;
  client_sections := [];
  plugins_sections := []

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

let top_menus = ref ([]: (string * (GMenu.menu -> unit)) list)
  
let scanned_ports = ref ([] : ((string * int) list))
let new_scanned_port = ref true
  