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

open GList

let clist = (clist_poly : ?columns:int ->
  ?titles:string list ->
  ?hadjustment:GData.adjustment ->
  ?vadjustment:GData.adjustment ->
  ?shadow_type:Gtk.Tags.shadow_type ->
  ?button_actions:(int * Gtk.Tags.button_action list) list ->
  ?selection_mode:Gtk.Tags.selection_mode ->
  ?reorderable:bool ->
  ?use_drag_icons:bool ->
  ?row_height:int ->
  ?titles_show:bool ->
  ?titles_active:bool ->
  ?auto_sort:bool ->
  ?sort_column:int ->
  ?sort_type:Gtk.Tags.sort_type ->
  ?border_width:int ->
  ?width:int ->
  ?height:int ->
  ?packing:(GObj.widget -> unit) -> ?show:bool -> unit -> int clist)
