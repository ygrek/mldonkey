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


(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(*                                                                       *)
(*                         Gview                                         *)
(*                                                                       *)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)

module Gview :
  functor
    (V : sig

           module Column :
             sig

               type column

               val column_strings : (column * string ref * int) list
               val string_of_column : column -> string
               val int_of_column : column -> int

             end

           type item
           type key

           val columns     : (Column.column * float) list Options.option_record
           val get_key     : item -> key
           val module_name : string

         end) ->
    sig

      class type g_model =
        object
          inherit GTree.model
          method content : GTree.view_column -> V.Column.column -> unit
          method expanded_paths : int array list
          method get_item : Gtk.tree_iter -> V.item
          method sort : V.Column.column -> Gtk.Tags.sort_type option -> unit
          method set_view : g_view -> unit
          method unset_view : g_view -> unit
        end

      and g_view =
        object
          method expanded_paths : int array list
          method id : int
          method set_model : g_model -> unit
          method unset_model : unit -> unit
        end

      class virtual g_list : GTree.column_list ->
        object
          inherit GTree.model_filter
          val store : GTree.list_store
          method virtual from_item : Gtk.tree_iter -> V.item -> unit
          method virtual from_new_item : Gtk.tree_iter -> V.item -> V.item -> unit
          method virtual content : GTree.view_column -> V.Column.column -> unit
          method virtual sort_items : V.Column.column -> V.item -> V.item -> int
          method add_item : V.item -> Gtk.tree_iter
          method all_items : unit -> V.item list
          method clear : unit -> unit
          method expanded_paths : int array list
          method find_item : V.key -> (Gtk.tree_iter * V.item)
          method get_item : Gtk.tree_iter -> V.item
          method gmodel : g_model
          method nitems : int
          method refresh_filter : unit -> unit
          method remove_item : V.item -> unit
          method set_filter : (V.item -> bool) -> unit
          method set_view : g_view -> unit
          method sort : V.Column.column -> Gtk.Tags.sort_type option -> unit
          method unset_view : g_view -> unit
          method update_item : Gtk.tree_iter -> V.item -> V.item -> unit
        end

      class virtual g_tree : GTree.column_list ->
        object
          inherit GTree.model_filter
          val store : GTree.tree_store
          method virtual from_item : Gtk.tree_iter -> V.item -> unit
          method virtual from_new_item : Gtk.tree_iter -> V.item -> V.item -> unit
          method virtual content : GTree.view_column -> V.Column.column -> unit
          method virtual sort_items : V.Column.column -> V.item -> V.item -> int
          method add_item : V.item -> ?parent:Gtk.tree_iter -> unit -> Gtk.tree_iter
          method all_items : unit -> V.item list
          method clear : unit -> unit
          method expanded_paths : int array list
          method find_item : V.key -> (Gtk.tree_iter * V.item)
          method get_item : Gtk.tree_iter -> V.item
          method gmodel : g_model
          method nitems : int
          method refresh_filter : unit -> unit
          method remove_item : V.item -> unit
          method set_filter : (V.item -> bool) -> unit
          method set_view : g_view -> unit
          method sort : V.Column.column -> Gtk.Tags.sort_type option -> unit
          method unset_view : g_view -> unit
          method update_item : Gtk.tree_iter -> V.item -> V.item -> unit
        end

      class treeview : [> Gtk.box] Gtk.obj ->
        object
          inherit GPack.box
          method expanded_paths : int array list
          method gview : g_view
          method id : int
          method set_menu : (V.item list -> GToolbox.menu_entry list) -> unit
          method set_model : g_model -> unit
          method set_on_collapse : (Gtk.tree_path -> V.item -> bool) -> unit
          method set_on_collapsed : (Gtk.tree_path -> V.item -> unit) -> unit
          method set_on_double_click : (V.item -> unit) -> unit
          method set_on_expand : (Gtk.tree_path -> V.item -> bool) -> unit
          method set_on_expanded : (Gtk.tree_path -> V.item -> unit) -> unit
          method set_on_select : (V.item list -> unit) -> unit
          method set_selection_mode : Gtk.Tags.selection_mode -> unit
          method unset_model : unit -> unit
          method view : GTree.view
        end

      val treeview :
        ?mode:Gtk.Tags.selection_mode ->
        ?homogeneous:bool ->
        ?spacing:int ->
        ?border_width:int ->
        ?width:int ->
        ?height:int -> ?packing:(GObj.widget -> unit) -> ?show:bool -> unit -> treeview

    end

(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(*                                                                       *)
(*                         CHAT                                          *)
(*                                                                       *)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)

class type chat_buffer =
  object
    inherit GText.buffer
    method clear : unit -> unit
    method enc_list : GuiUtf8.codeset list
    method insert_text : string -> string -> ?priv : bool -> unit -> unit
    method maxlines : int
    method on_entry_return : (string -> unit)
    method set_enc_list : GuiUtf8.codeset list -> unit
    method set_maxlines : int -> unit
    method set_on_entry_return : (string -> unit) -> unit
    method set_smileys : bool -> unit
    method smileys : bool
  end

val chat_buffer :
  ?on_entry:(string -> unit) ->
  ?smileys:bool ->
  ?tag_table:GText.tag_table -> ?text:string -> unit -> chat_buffer

class type chat_view =
  object
    inherit GPack.box
    method buffer : chat_buffer option
    method clear : unit -> unit
    method extended : bool
    method set_buffer : chat_buffer -> unit
    method set_extended : bool -> unit
    method set_my_name : string -> unit
  end

val chat_view :
  ?extended:bool ->
  ?buffer:chat_buffer ->
  ?my_name:string ->
  ?homogeneous:bool ->
  ?spacing:int ->
  ?border_width:int ->
  ?width:int ->
  ?height:int -> ?packing:(GObj.widget -> unit) -> ?show:bool -> unit -> chat_view
