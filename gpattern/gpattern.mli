(***********************************************************************)
(*                                GPattern                             *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** Generic classes for LablGtk. *)

(** {2 Lists} *)

type content =
  | String of string 
  | Pixmap of GDraw.pixmap

class virtual ['a] plist : 
    Gtk.Tags.selection_mode ->
      string list ->
	bool ->
  object
    val mutable data : 'a list
    val mutable selection : 'a list
    val mutable current_sort : int
    method box : GObj.widget
    method wlist : 'a GList.clist
    method update_row : 'a -> int -> unit
    method insert : ?row: int -> 'a -> unit
    method update_data : 'a list -> unit
    method update : unit
    method virtual content : 'a -> content list * GDraw.optcolor option
    method virtual compare : 'a -> 'a -> int
    method selection : 'a list
    method set_titles : string list -> unit
    method on_select : 'a -> unit
    method on_deselect : 'a -> unit
    method on_double_click : 'a -> unit
    method menu : GToolbox.menu_entry list
  end
