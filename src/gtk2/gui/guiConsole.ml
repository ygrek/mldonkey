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

(* console of the GUI. *)

(* TODO :
     - make some regexp to underline some
       part of the text
     - put some color accordingly
*)

open CommonTypes
open GuiProto

module M = GuiMessages
module O = GuiOptions
module Mi = GuiMisc
module U = GuiUtf8

let (!!) = Options.(!!)

let verbose = O.gtk_verbose_console

let lprintf' fmt =
  Printf2.lprintf ("GuiConsole: " ^^ fmt)


(*************************************************************************)
(*                                                                       *)
(*                         Global tables                                 *)
(*                                                                       *)
(*************************************************************************)

let history_cols = new GTree.column_list
let history_text = history_cols#add Gobject.Data.string
let history_store = GTree.list_store history_cols
let history_model = GTree.model_sort history_store

let _ =
  history_model#set_default_sort_func
    (fun model iter_a iter_b ->
       let a = model#get ~row:iter_a ~column:history_text in
       let b = model#get ~row:iter_b ~column:history_text in
       compare a b
  )

(*************************************************************************)
(*                                                                       *)
(*                         fill_history                                  *)
(*                                                                       *)
(*************************************************************************)

let fill_history s =
  let not_exist = ref true in
  history_model#foreach
    (fun _ row ->
      (let text = history_model#get ~row ~column:history_text in
       if text = s then not_exist := false);
      false
  );
  if !not_exist
    then begin
      let row = history_store#append () in
      history_store#set ~row ~column:history_text s;
    end

(*************************************************************************)
(*                                                                       *)
(*                         Text skeleton                                 *)
(*                                                                       *)
(*************************************************************************)

let text_buffer = GText.buffer ()

let _ =
  ignore (text_buffer#connect#changed ~callback:
    (fun _ ->
       let nlines = text_buffer#line_count in
       (* Printf.printf "Buffer modified %d\n" nlines;
       flush stdout; *)
       if nlines > 1000
         then begin
           let stop = text_buffer#get_iter `END in
           let lines = (nlines - 1000) / 100 in
           (if !!verbose then lprintf' "lines to remove %d\n" (lines * 100));
           let stop = stop#backward_lines (nlines - (lines * 100)) in
           text_buffer#delete
             ~start:text_buffer#start_iter
             ~stop
         end
  ))

(*************************************************************************)
(*                                                                       *)
(*                         on_entry_return                               *)
(*                                                                       *)
(*************************************************************************)

let on_entry_return entry () =
  match entry#text with
      "" -> ()
    | s ->
       begin
          GuiCom.send (GuiProto.Command s);
          fill_history s;
          entry#set_text ""
        end

(*************************************************************************)
(*                                                                       *)
(*                         clear                                         *)
(*                                                                       *)
(*************************************************************************)

let clear () =
  text_buffer#delete
    ~start:text_buffer#start_iter
    ~stop:text_buffer#end_iter

(*************************************************************************)
(*                                                                       *)
(*                         interface with the core                       *)
(*                                                                       *)
(*************************************************************************)

let insert t =
  let iter = text_buffer#get_iter `END in
  let s = (U.utf8_of t) ^ "\n" in
  text_buffer#insert ~iter s

(*************************************************************************)
(*                                                                       *)
(*                         console window                                *)
(*                                                                       *)
(*************************************************************************)

let console_box gui =
  let vbox = GPack.vbox ~homogeneous:false ~border_width:6 () in
  let scrolled_box =
    GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
      ~placement:`TOP_LEFT
      ~packing:(vbox#pack ~expand:true ~fill:true) ()
  in
  let text = 
    GText.view ~editable:false
      ~buffer:text_buffer
      ~wrap_mode:`WORD ~packing:scrolled_box#add ()
  in
  let hbox =
    GPack.hbox ~homogeneous:false ~border_width:6 ~spacing:3
      ~packing:(vbox#pack ~expand:false ~fill:true) ()
  in
  let _ =
    GMisc.label ~text:(!M.cT_lb_command)
      ~xalign:0. ~line_wrap:true
      ~packing:(hbox#pack ~expand:false ~fill:true) ()
  in
  let entry =
    GEdit.entry ~editable:true ~has_frame:true
      ~packing:(hbox#pack ~expand:true ~fill:true) ()
  in
  let c = GEdit.entry_completion ~model:history_model ~entry () in
  c#set_text_column history_text;

  let wb_clear_console =
    GButton.button
      ~packing:(hbox#pack ~expand:false ~fill:true ~padding:6) ()
  in
  let markup = GuiTools.create_markup !M.cT_lb_clear_console in
  let _ =
    GMisc.label ~markup ~use_underline:true
      ~justify:`RIGHT ~line_wrap:true
      ~packing:(wb_clear_console#add) ()
  in
  ignore (entry#event#connect#key_press ~callback:
    (fun ev ->
       GdkEvent.Key.keyval ev = GdkKeysyms._Return &&
      (on_entry_return entry ();
       true
      )
  ));
  ignore (text#misc#connect#size_allocate ~callback:
    (fun _ ->
       let iter = text_buffer#get_iter `END in
       ignore(text#scroll_to_iter ~use_align:true ~xalign:1. ~yalign:1. iter)
  ));
  ignore (wb_clear_console#connect#clicked
    (fun () -> 
       text#buffer#delete
         ~start:(text#buffer#get_iter `START)
         ~stop:(text#buffer#get_iter `END)
  ));
  let font = Pango.Font.from_string !!O.gtk_font_list in
  Pango.Font.set_family font "monospace";
  let col = `NAME !!O.gtk_color_default in
  text#misc#modify_font font;
  text#misc#modify_text [`NORMAL, col];
  text#set_left_margin 6;
  text#set_right_margin 6;
  vbox#coerce
