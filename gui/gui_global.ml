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

(** Information on locations. *)
let (locations : (int, Gui_proto.client_info) Hashtbl.t) = Hashtbl.create 103

(** Information on locations. *)
let (results : (int, CommonTypes.result_info) Hashtbl.t) = Hashtbl.create 103

(** Search counter. *)
let search_counter = ref 0

open Gui_proto
  
let (networks : (int, Gui_proto.network_info) Hashtbl.t) = Hashtbl.create 13
  
let network_name num = try (Hashtbl.find networks num).network_name
  with _ -> "?"
      
let (users : (int, Gui_proto.user_info) Hashtbl.t) = Hashtbl.create 1023