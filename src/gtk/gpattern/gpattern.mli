(* Copyright 2001, 2002 Maxence Guesdon INRIA *)
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

(** Generic classes for LablGtk. *)

(** {2 Lists} *)

type content =
  | String of string 
  | Pixmap of GDraw.pixmap
  | Pixtext of string * GDraw.pixmap

type 'a ptree =
  {
   data : 'a;
   mutable children : 'a ptree list
  }

class virtual ['a] plist : 
    Gtk.Tags.selection_mode ->
      string list ->
        bool ->
    ('a -> int) ->
  object
    val mutable selection : 'a list
    val mutable current_sort : int
    method box : GObj.widget
    method wlist : 'a GList.clist
    method update_row : 'a -> int -> unit
    method insert : ?row: int -> 'a -> unit
    method update : unit
    method clear : unit
    method virtual content : 'a -> content list * GDraw.optcolor option
    method virtual compare : 'a -> 'a -> int
    method selection : 'a list
    method set_titles : string list -> unit
    method on_select : 'a -> unit
    method on_deselect : 'a -> unit
    method on_double_click : 'a -> unit
    method menu : GToolbox.menu_entry list
    method find : int -> int * 'a
    method get_data : int -> 'a
    method get_all_items : 'a list
    method has_changed_width : (int * int) list -> unit
    method add_item : 'a -> unit
    method size : int
    method reset_data : 'a list -> unit
    method remove_item : int -> 'a -> unit

    method filter : 'a -> bool
    method refresh_item : int -> 'a -> unit
    method refresh_filter : unit

    method resort_column : int -> unit -> unit
    method column_menu :  int -> GToolbox.menu_entry list
      
    method iter : ('a -> unit) -> unit
  end

class virtual ['a] filtered_plist : 
    Gtk.Tags.selection_mode ->
      string list ->
        bool ->
    ('a -> int) ->
  object
    val mutable selection : 'a list
    val mutable current_sort : int
    method box : GObj.widget
    method wlist : 'a GList.clist
    method update_row : 'a -> int -> unit
    method insert : ?row: int -> 'a -> unit
    method update : unit
    method clear : unit
    method virtual content : 'a -> content list * GDraw.optcolor option
    method virtual compare : 'a -> 'a -> int
    method selection : 'a list
    method set_titles : string list -> unit
    method on_select : 'a -> unit
    method on_deselect : 'a -> unit
    method on_double_click : 'a -> unit
    method menu : GToolbox.menu_entry list
    method find : int -> int * 'a
    method get_data : int -> 'a
    method get_all_items : 'a list
    method has_changed_width : (int * int) list -> unit
    method add_item : 'a -> unit
    method size : int
    method reset_data : 'a list -> unit
    method remove_item : int -> 'a -> unit

    method virtual filter : 'a -> bool
    method refresh_item : int -> 'a -> unit
    method refresh_filter : unit

    method resort_column : int -> unit -> unit
    method column_menu :  int -> GToolbox.menu_entry list
      
    method iter : ('a -> unit) -> unit
  end

class virtual ['a] filtered_ptree :
    Gtk.Tags.selection_mode ->
    string list ->
    bool ->
    ('a -> int list) ->
  object
    constraint 'a = 'b ptree
    val mutable selection : 'a list
    val mutable current_sort : int
    method is_expanded : (int list * int ) list
    method box : GObj.widget
    method wlist : 'a GList.clist
    method update_row : 'a -> int -> unit
    method insert : ?row: int -> 'a -> unit
    method expand : 'a -> bool
    method collapse : 'a -> bool
    method update : unit
    method clear : unit
    method virtual content : 'a -> content list * GDraw.optcolor option
    method virtual compare : 'a -> 'a -> int
    method selection : 'a list
    method set_titles : string list -> unit
    method on_select : 'a -> unit
    method on_deselect : 'a -> unit
    method on_double_click : 'a -> unit
    method menu : GToolbox.menu_entry list
    method find : int list -> int * 'a
    method get_data : int -> 'a
    method get_all_items : 'a list
    method has_changed_width : (int * int) list -> unit
    method add_item : 'a -> unit
    method size : int
    method reset_data : 'a list -> unit
    method remove_item : int -> 'a -> unit
    method virtual filter : 'a -> bool
    method refresh_item : int -> 'a -> unit
    method refresh_filter : unit
    method resort_column : int -> unit -> unit
    method column_menu :  int -> GToolbox.menu_entry list
    method iter : ('a -> unit) -> unit
  end
