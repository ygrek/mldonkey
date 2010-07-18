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

(* Tools of the GUI. *)


open GMain

val (<:>) : 
  < add : GObj.widget -> unit; all_children : #GObj.widget list;
        as_widget : [> Gtk.widget] Gobject.obj; border_width : int;
        children : #GObj.widget list; coerce : #GObj.widget;
        destroy : unit -> unit; drag : #GObj.drag_ops;
        focus : #GContainer.focus; get_oid : int; misc : #GObj.misc_ops;
        remove : GObj.widget -> unit; resize_mode : [< Gtk.Tags.resize_mode];
        set_border_width : int -> unit;
        set_resize_mode : [> Gtk.Tags.resize_mode] -> unit; .. > -> GContainer.container

(**********************************************************************************)
(*                                                                                *)
(*                  Miscellaneous functions for a paned area                      *)
(*                                                                                *)
(**********************************************************************************)

val set_hpaned : GPack.paned -> float Options.option_record -> unit

val set_vpaned : GPack.paned -> float Options.option_record -> unit

val get_hpaned : GPack.paned -> float Options.option_record -> unit

val get_vpaned : GPack.paned -> float Options.option_record -> unit

(**
   [remove_key ~target ~sign] removes the signal [sign] for the
   given widget [target].

*)

val remove_key : 
      target : < event : GObj.event_ops;
                       misc : < disconnect : GtkSignal.id -> 'a; .. >; .. > -> 
      sign : GtkSignal.id -> unit

(**
   [add_key ~key ~target ~f] associates the [f] function to the event
   "key_press" with the given [key] for the given widget [target].

   @param source, the given key [key] will be removed automatically
   from the widget [target] when the given widget [source] is destroyed.
   If [source] is not specified [key] will be removed automatically
   from the widget [target] when [target] is destroyed.

   @param mods the list of modifiers. If not given, the default modifiers
   are used : [`MOD1].

*)

val add_key : 
      key : Gdk.keysym -> 
      target : < event : GObj.event_ops;
                       misc : < disconnect : GtkSignal.id -> 'a; .. >; .. > -> 
      f:(unit -> unit) -> ?mods : Gdk.Tags.modifier list -> 
      ?source : GObj.widget -> unit -> unit

(* **
   [get_access_key_from ~markup] parses the string [markup] and
   an underline in [markup] indicates the next character should be
   used for the mnemonic accelerator key.

   @param char, specifies which character shall used when parsing [markup]
   instead of an underline.

*)

val get_access_key_from : 
      markup : string -> ?char : Glib.unichar -> unit -> Gdk.keysym option

(* **
   [add_label ~cont ~markup] add a label to the widget [cont].
   Parses [markup] which is marked up with the Pango text markup 
   language, setting the label's text and attribute list based on
   the parse results.
   Especially using an underline in [markup] indicates the next
   character should be used for the mnemonic accelerator key. The 
   mnemonic accelerator key is automatically associated with the
   widget [cont]. Returns the label.

   @param mnemonic_widget, specifies the given widget [mnemonic_widget]
   that will be accelerated by mnemonic accelerator key of the label
   instead of [cont].
   Can be usefull when the target is a GtkEntry next to the label.

*)

val add_label : 
      cont : < add : GObj.widget -> unit; all_children : #GObj.widget list;
                   as_widget : [> Gtk.widget] Gobject.obj; border_width : int;
                   children : #GObj.widget list; coerce : #GObj.widget;
                   destroy : unit -> unit; drag : #GObj.drag_ops;
                   focus : #GContainer.focus; get_oid : int; misc : #GObj.misc_ops;
                   remove : GObj.widget -> unit; resize_mode : [< Gtk.Tags.resize_mode];
                   set_border_width : int -> unit;
                   set_resize_mode : [> Gtk.Tags.resize_mode] -> unit; .. > ->
      markup : string -> ?mnemonic_widget : GObj.widget -> unit -> GMisc.label

(* **
   [add_pixbuf ~cont ~pixpuf] add an image to the widget [cont].
   Returns the image.

*)

val add_pixbuf : 
      cont : < add : GObj.widget -> unit; all_children : #GObj.widget list;
                   as_widget : [> Gtk.widget] Gobject.obj; border_width : int;
                   children : #GObj.widget list; coerce : #GObj.widget;
                   destroy : unit -> unit; drag : #GObj.drag_ops;
                   focus : #GContainer.focus; get_oid : int; misc : #GObj.misc_ops;
                   remove : GObj.widget -> unit; resize_mode : [< Gtk.Tags.resize_mode];
                   set_border_width : int -> unit;
                   set_resize_mode : [> Gtk.Tags.resize_mode] -> unit; .. > -> 
      pixbuf : GdkPixbuf.pixbuf -> GMisc.image

(* **
   [add_complex_box ~cont ~style] add a GPack.vbox or a GPack.hbox
   according to [style] to the widget [cont].

   @param markup, add a label to the widget [cont] which is
   marked up with the Pango text markup language, setting the
   label's text and attribute list based on the parse results.
   Especially using an underline in [markup] indicates the next
   character should be used for the mnemonic accelerator key. The 
   mnemonic accelerator key is automatically associated with the
   widget [cont].
   The default handler for this signal will activate the widget
   [cont] if there are no mnemonic collisions and toggle focus
   between the colliding widgets otherwise.

   @param mnemonic_widget, specifies the given widget [mnemonic_widget]
   that will be accelerated by the mnemonic accelerator key of 
   the label instead of [cont].

   @param icon, add an image based on the [icon] to the widget [cont].

*)

val add_complex_box :
      cont : GContainer.container -> style : Gtk.Tags.toolbar_style ->
      ?markup : string -> ?icon : GdkPixbuf.pixbuf -> unit -> unit

type b_children =
  {
   oid : int;
   button : GButton.toggle_button;
   mutable active : bool;
 }


class tool_bar : ([> Gtk.box] as 'a) Gtk.obj ->
  object
    inherit GPack.box
    val obj : 'a Gtk.obj
    val mutable bb_children : b_children list
    method add_toggle_button : 
      style : Gtk.Tags.toolbar_style ->
      ?markup : string -> ?icon : GdkPixbuf.pixbuf -> ?f : (unit -> unit) -> unit -> GButton.toggle_button
    method add_button : 
      style : Gtk.Tags.toolbar_style ->
      ?markup : string -> ?icon : GdkPixbuf.pixbuf -> ?f : (unit -> unit) -> unit -> GButton.button
    method empty : unit -> unit
  end

val tool_bar :
      Gtk.Tags.orientation -> ?spacing : int ->
      ?child_width : int -> ?child_height : int ->
      ?child_ipadx : int -> ?child_ipady : int ->
      ?layout : GtkPack.BBox.bbox_style ->
      ?width : int -> ?height : int -> ?border_width : int ->
      ?packing : (GObj.widget -> unit) -> ?show : bool -> unit -> tool_bar

(* **
   [popup_progress ~label ~items ~f ~step] displays a popup window
   with a label [label] and a progress bar.
   the progress displayed corresponds to the progress of the
   function [f] applied to [step] items taken from the items
   list [items].
   
*)

val popup_progress :
      label : string -> items : 'a list -> f : ('a -> unit) -> step : int -> unit


(* **
   [create_bold_markup s] returns a string which is
   marked up with the Pango text markup language, setting the
   label's text as bold with a blue color, and converted
   to the utf8 format.

*)

val create_bold_markup : string -> string
val create_default_bold_markup : string -> string

(* **
   [create_markup s] returns a string which is
   marked up with the Pango text markup language, setting the
   label's text with the default color as set in the options,
   and converted to the utf8 format.

*)

val create_markup : string -> string

(* **
   [color_of_name s] returns a string of type GDraw.color.

*)

val color_of_name : string -> string

(* **
   [fit_string_to_pixels s ~context ~pixels] returns a new string
   from [s] that fits the given width [pixels] for the given pango [context].

*)

val fit_string_to_pixels : string -> context:GPango.context -> pixels:int -> string

val warning_box : warning_message:string -> text: string list -> ?title:string -> ?icon:GdkPixbuf.pixbuf -> ?on_ok:(unit -> unit) -> unit -> unit
