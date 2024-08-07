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

(* preference settings of MLDonkey. *)

open GuiTypes2

module M = GuiMessages
module O = GuiOptions
module A = GuiArt
module U = GuiUtf8

let (!!) = Options.(!!)
let (<:>) = GuiTools.(<:>)

let verbose = O.gtk_verbose_configwin

let lprintf' fmt =
  Printf2.lprintf ("GuiConfigWindow: " ^^ fmt)

type pref_box =
  BFilename
| BPath
| BPassword
| BString
| BCombo
| BTime
| BColor
| BFont
| BScale
| BFloat
| BInt
| BInt32
| BInt64
| BAdvanced
| BBool

type ('a, 'b, 'c) preference = 
  {
    pref_section : 'a option;
    opt_section : string;
    pref_subsection : 'b option;
    mutable pref_help : string;
    pref_advanced : bool;
    pref_default : string;

    mutable pref_name : string;
    mutable pref_label : string;
    mutable pref_group : 'c option;
    mutable pref_option_list : string list;    (* Used for combo_box *)
    mutable pref_value : string;
    mutable pref_new_value : string;           (* Internally used by ConfigWindow *)
    mutable pref_type : pref_box;
    mutable pref_apply : unit -> unit;         (* Becarefull overwritten by ConfigWindow *)
    mutable pref_apply_default : unit -> unit; (* Becarefull overwritten by ConfigWindow *)
  }

(*************************************************************************)
(*                                                                       *)
(*                         Global functions                              *)
(*                                                                       *)
(*************************************************************************)

let safe_int s = float_of_int (Options.value_to_int (Options.StringValue s))

let safe_int32 s = 
  try
    Int32.to_float (Int32.of_string s)
  with _ -> failwith "Options: not an int32 option"

let safe_int64 s = Int64.to_float (Options.value_to_int64 (Options.StringValue s))

let safe_float s = Options.value_to_float (Options.StringValue s)

let safe_bool s = Options.value_to_bool (Options.StringValue s)

let remove_ s =
  String2.replace s '_' ""

(*************************************************************************)
(*                                                                       *)
(*                         message_to_stem                               *)
(*                                                                       *)
(*************************************************************************)

let message_to_stem m context pixels =
  let pango_layout = Pango.Layout.create context in
  let m = String2.replace m '\n' " " in
  let sl = String2.split_simplify m ' ' in
  let m = 
    let s = ref "" in
    List.iter (fun t ->
      let str = match !s with "" -> t | _ -> !s ^ " " ^ t in
      Pango.Layout.set_text pango_layout str;
      if fst (Pango.Layout.get_pixel_size pango_layout) <= pixels
        then s := str
        else s := !s ^ "\n" ^ t
    ) sl;
    !s
  in
  (U.simple_utf8_of (Printf.sprintf "%s" m))

(*************************************************************************)
(*                                                                       *)
(*                         event_wrap_widget                             *)
(*                                                                       *)
(*************************************************************************)

let event_wrap_widget ~w ~p ?h_label () =
  match h_label with
      None -> w#coerce
    | Some label ->
        begin
          let evbox = GBin.event_box () in
          ignore (evbox#event#add [`ENTER_NOTIFY;`LEAVE_NOTIFY]);
          ignore (evbox#event#connect#enter_notify
            ~callback:
              (fun ev ->
                 if GdkEvent.get_type ev = `ENTER_NOTIFY 
                   then begin
                     let context = label#misc#pango_context#as_context in
                     let dw = new GDraw.drawable label#misc#window in
                     let width = (fst dw#size) * 7 / 10 in (* looks to be the good ratio with several themes. to see ... *)
                     (if !!verbose then lprintf' "Help box width %d\n" width);
                     label#set_label (message_to_stem p.pref_help context width);
                     true
                   end else false
          ));
          ignore (evbox#event#connect#leave_notify
            ~callback:
              (fun ev ->
                 if GdkEvent.get_type ev = `LEAVE_NOTIFY 
                   then (label#set_label ""; true)
                   else false
          ));
          evbox#add w#coerce;
          evbox#coerce
        end

(*************************************************************************)
(*                                                                       *)
(*                         add_string_param                              *)
(*                                                                       *)
(*************************************************************************)

let add_string_param ~p ~top ~(table : GPack.table) ?(h_label : GMisc.label option) () =
  let label =
    GMisc.label ~xalign:0. 
      ~markup:(U.simple_utf8_of p.pref_label) ()
  in
  let edit =
     GEdit.entry ~text:(U.simple_utf8_of p.pref_new_value)
       ~editable:true ~visibility:true ()
  in
(*
  ignore (table#misc#connect#size_allocate
      ~callback: (fun r ->
        lprintf' "Table width %d\n" r.Gtk.width;
  ));
*)
  table#attach ~left:0 ~top 
    ~xpadding:18 ~ypadding:0 
    ~expand:`X ~fill:`X
    (event_wrap_widget ~w:label ~p ?h_label ());
  table#attach ~left:1 ~top
    ~xpadding:0 ~ypadding:0
    ~expand:`X ~fill:`X
    (event_wrap_widget ~w:edit ~p ?h_label ());
  p.pref_apply <- (fun _ -> p.pref_new_value <- edit#text);
  p.pref_apply_default <- (fun _ -> edit#set_text (U.simple_utf8_of p.pref_default))

(*************************************************************************)
(*                                                                       *)
(*                         add_password_param                            *)
(*                                                                       *)
(*************************************************************************)

let add_password_param ~p ~top ~(table : GPack.table) ?(h_label : GMisc.label option) () =
  let label = GMisc.label ~xalign:0. ~markup:(U.simple_utf8_of p.pref_label) () in
  let hbox = GPack.hbox ~homogeneous:false ~spacing:6 () in
  let pixbuf = A.get_icon ~icon:M.icon_stock_password ~size:A.SMALL () in
  let _image = GMisc.image ~pixbuf ~packing:(hbox#pack ~expand:false ~fill:true) () in
  let edit =
    GEdit.entry ~text:(U.simple_utf8_of p.pref_new_value) ~editable:true ~visibility:false
      ~packing:(hbox#pack ~expand:true ~fill:true) ()
  in
  table#attach ~left:0 ~top 
    ~xpadding:18 ~ypadding:0
    ~expand:`X ~fill:`X
    (event_wrap_widget ~w:label ~p ?h_label ());
  table#attach ~left:1 ~top
    ~xpadding:0 ~ypadding:0
    ~expand:`X ~fill:`X
    (event_wrap_widget ~w:hbox ~p ?h_label ());
  p.pref_apply <- (fun _ -> p.pref_new_value <- edit#text);
  p.pref_apply_default <- (fun _ -> edit#set_text (U.simple_utf8_of p.pref_default))

(*************************************************************************)
(*                                                                       *)
(*                         add_scale_param                               *)
(*                                                                       *)
(*************************************************************************)

let add_scale_param ~p ~top ~(table : GPack.table) ?(h_label : GMisc.label option) () =
  let label = GMisc.label ~xalign:0. ~markup:(U.simple_utf8_of p.pref_label) () in
  let value = (safe_float p.pref_new_value) *. 100. in
  let range =
    GData.adjustment ~lower:0. ~upper:110.
      ~value ~step_incr:1. ()
  in
  let scale =
    GRange.scale `HORIZONTAL ~adjustment:range ~digits:0
      ~draw_value:true ~value_pos:`RIGHT ~update_policy:`CONTINUOUS ()
  in
  table#attach ~left:0 ~top 
    ~xpadding:18 ~ypadding:0
    ~expand:`X ~fill:`X
    (event_wrap_widget ~w:label ~p ?h_label ());
  table#attach ~left:1 ~top
    ~xpadding:0 ~ypadding:0
    ~expand:`X ~fill:`X
    (event_wrap_widget ~w:scale ~p ?h_label ());
  p.pref_apply <- (fun _ -> p.pref_new_value <- string_of_float (range#value /. 100.));
  p.pref_apply_default <- (fun _ -> range#set_value ((safe_float p.pref_default) *. 100.))

(*************************************************************************)
(*                                                                       *)
(*                         add_int_param                                 *)
(*                                                                       *)
(*************************************************************************)

let add_int_param ~p ~f ~top ~(table : GPack.table) ?(h_label : GMisc.label option) () =
  let value = f p.pref_new_value in
  let label = GMisc.label ~xalign:0. ~markup:(U.simple_utf8_of p.pref_label) () in
  let hbox = GPack.hbox ~homogeneous:false ~spacing:3 () in
  let range = GData.adjustment ~lower:0. ~upper:(float_of_int max_int) ~step_incr:1. () in
  let spin =
    GEdit.spin_button ~adjustment:range ~rate:1. ~digits:0 ~numeric:true
      ~snap_to_ticks:true ~update_policy:`IF_VALID ~value ~wrap:true
      ~packing:(hbox#pack ~expand:false ~fill:true) ()
  in
  let _a_box =
    GPack.hbox ~homogeneous:false
      ~packing:(hbox#pack ~expand:true ~fill:true) ()
  in
  table#attach ~left:0 ~top
    ~xpadding:18 ~ypadding:0
    ~expand:`X ~fill:`X
    (event_wrap_widget ~w:label ~p ?h_label ());
  table#attach ~left:1 ~top
    ~xpadding:0 ~ypadding:0
    ~expand:`X ~fill:`X
    (event_wrap_widget ~w:hbox ~p ?h_label ());
  p.pref_apply <- (fun _ -> p.pref_new_value <- string_of_int spin#value_as_int);
  p.pref_apply_default <- (fun _ -> spin#set_value (f p.pref_default))

(*************************************************************************)
(*                                                                       *)
(*                         add_float_param                               *)
(*                                                                       *)
(*************************************************************************)

let add_float_param ~p ~top ~(table : GPack.table) ?(h_label : GMisc.label option) () =
  let value = safe_float p.pref_new_value in
  let label = GMisc.label ~xalign:0. ~markup:(U.simple_utf8_of p.pref_label) () in
  let hbox = GPack.hbox ~homogeneous:false ~spacing:3 () in
  let range = GData.adjustment ~lower:0. ~upper:((float_of_int max_int) *. 1000.) ~step_incr:0.1 () in
  let spin =
    GEdit.spin_button ~adjustment:range ~rate:1. ~digits:1 ~numeric:true
      ~snap_to_ticks:true ~update_policy:`IF_VALID ~value ~wrap:true
      ~packing:(hbox#pack ~expand:false ~fill:true) ()
  in
  let _a_box =
    GPack.hbox ~homogeneous:false
      ~packing:(hbox#pack ~expand:true ~fill:true) ()
  in
  table#attach ~left:0 ~top
    ~xpadding:18 ~ypadding:0
    ~expand:`X ~fill:`X
    (event_wrap_widget ~w:label ~p ?h_label ());
  table#attach ~left:1 ~top
    ~xpadding:0 ~ypadding:0
    ~expand:`X ~fill:`X
    (event_wrap_widget ~w:hbox ~p ?h_label ());
  p.pref_apply <- (fun _ -> p.pref_new_value <- string_of_float spin#value);
  p.pref_apply_default <- (fun _ -> spin#set_value (float_of_string p.pref_default))

(*************************************************************************)
(*                                                                       *)
(*                         add_filename_param                            *)
(*                                                                       *)
(*************************************************************************)

let last_dir = ref ""

let add_filename_param ~p ~top ~(table : GPack.table) ~path ?(h_label : GMisc.label option) () =
  let label = GMisc.label ~xalign:0. ~markup:(U.simple_utf8_of p.pref_label) () in
  let hbox = GPack.hbox ~homogeneous:false ~spacing:6 () in
  let edit =
    GEdit.entry ~text:(U.simple_utf8_of p.pref_new_value) ~editable:true ~visibility:true
      ~packing:(hbox#pack ~expand:true ~fill:true) ()
  in
  let button = GButton.button ~packing:(hbox#pack ~expand:false ~fill:true) () in
  GuiTools.add_complex_box
    ~cont:((<:>) button)
    ~style:`BOTH_HORIZ
    ~markup:!M.cW_lb_browse
    ~icon:(A.get_icon ~icon:M.icon_stock_directory ~size:A.SMALL ()) ();
  table#attach ~left:0 ~top
    ~xpadding:18 ~ypadding:0
    ~expand:`X ~fill:`X
    (event_wrap_widget ~w:label ~p ?h_label ());
  table#attach ~left:1 ~top
    ~xpadding:0 ~ypadding:0
    ~expand:`X ~fill:`X
    (event_wrap_widget ~w:hbox ~p ?h_label ());
  let files = ref ([] : string list) in 
  let f_sel () =
    let dialog = GWindow.file_selection
      ~title:(U.simple_utf8_of p.pref_label)
      ~modal:false
      ~show:true ()
    in
    let wb_ok = dialog#ok_button in
    let wb_cancel = dialog#cancel_button in
    let _ = 
      match p.pref_new_value with
          "" ->
            if !last_dir <> ""
              then dialog#set_filename !last_dir
        | dir -> dialog#set_filename dir
    in
    ignore (wb_ok#connect#clicked ~callback:
      (fun () -> 
         files := [dialog#filename];
         let _ =
           match !files with
               []
             | [""] -> ()
             | l ->
                 last_dir := Filename.dirname (List.hd l);
                 if not path
                   then edit#set_text (U.simple_utf8_of (List.hd l))
                   else edit#set_text (U.simple_utf8_of !last_dir)
         in
         dialog#destroy ()
    ));
    ignore (wb_cancel#connect#clicked dialog#destroy)
  in
  ignore (button#connect#clicked f_sel);
  p.pref_apply <- (fun _ -> p.pref_new_value <- edit#text);
  p.pref_apply_default <- (fun _ -> edit#set_text (U.simple_utf8_of p.pref_default))

(*************************************************************************)
(*                                                                       *)
(*                         add_color_param                               *)
(*                                                                       *)
(*************************************************************************)

let add_color_param ~p ~top ~(table : GPack.table) ?(h_label : GMisc.label option) () =
  let label = GMisc.label ~xalign:0. ~markup:(U.simple_utf8_of p.pref_label) () in
  let hbox = GPack.hbox ~homogeneous:false () in
  let button = GButton.button ~packing:(hbox#pack ~expand:false ~fill:true) () in
  let box = GPack.hbox ~homogeneous:false ~spacing:6 ~packing:button#add () in
  GuiTools.add_complex_box
    ~cont:((<:>) box)
    ~style:`ICONS
    ~icon:(A.get_icon ~icon:M.icon_stock_color ~size:A.SMALL ()) ();
  let _separator =
    GMisc.separator `VERTICAL
    ~packing:(box#pack ~expand:false ~fill:true) ()
  in
  let pixbuf = GdkPixbuf.create ~width:100 ~height:!!O.gtk_look_lists_icon_size () in
  let colv = ref "" in
  let set_color col =
    try
      GdkPixbuf.fill pixbuf (Int32.of_string col)
    with _ -> ()
  in
  let string_of_int_of_col color = 
    let r = (Gdk.Color.red color) / 256 in
    let g = (Gdk.Color.green color) / 256 in
    let b = (Gdk.Color.blue color) / 256 in
    let s =
      let s = Bytes.unsafe_of_string @@ Printf.sprintf "%02X%02X%02X" r g b in
      (* FIXME why start from 1 not 0? also seems this code is not needed, %02X takes care *)
      for i = 1 to (Bytes.length s) - 1 do
        if Bytes.get s i = ' ' then Bytes.set s i '0'
      done;
      Bytes.unsafe_to_string s
    in
    colv := "#" ^ s; 
    ("0x" ^ s ^ "FF")
  in
  let col = GDraw.color (`NAME p.pref_new_value) in
  set_color (string_of_int_of_col col);    
  let _sample = GuiTools.add_pixbuf ~cont:((<:>) box) ~pixbuf in
  let _separator =
    GMisc.separator `VERTICAL
    ~packing:(box#pack ~expand:false ~fill:true) ()
  in
  let _a_box =
    GPack.hbox ~homogeneous:false
      ~packing:(hbox#pack ~expand:true ~fill:true) ()
  in
  table#attach ~left:0 ~top
    ~xpadding:18 ~ypadding:0
    ~expand:`X ~fill:`X
    (event_wrap_widget ~w:label ~p ?h_label ());
  table#attach ~left:1 ~top
    ~xpadding:0 ~ypadding:0
    ~expand:`X ~fill:`X
    (event_wrap_widget ~w:hbox ~p ?h_label ());
  let f_sel () =
    let dialog = GWindow.color_selection_dialog
        ~title:(U.simple_utf8_of p.pref_label)
        ~modal:false
        ~show:true ()
    in
    let color = GDraw.color (`NAME !colv) in
    dialog#colorsel#set_color color;
    let wb_ok = dialog#ok_button in
    let wb_cancel = dialog#cancel_button in
    let _ = wb_ok#connect#clicked
      (fun () -> 
         let color = dialog#colorsel#color in
         set_color (string_of_int_of_col color);
         dialog#destroy ()
      )
    in
    ignore (wb_cancel#connect#clicked dialog#destroy)
  in
  ignore (button#connect#clicked f_sel);
  p.pref_apply <- (fun _ -> p.pref_new_value <- !colv);
  p.pref_apply_default <- 
    (fun _ ->
       let color = GDraw.color (`NAME p.pref_default) in
       set_color (string_of_int_of_col color))

(*************************************************************************)
(*                                                                       *)
(*                         add_font_param                                *)
(*                                                                       *)
(*************************************************************************)

let add_font_param ~p ~top ~(table : GPack.table) ?(h_label : GMisc.label option) () =
  let label = GMisc.label ~xalign:0. ~markup:(U.simple_utf8_of p.pref_label) () in
  let hbox = GPack.hbox ~homogeneous:false () in
  let button = GButton.button ~packing:(hbox#pack ~expand:false ~fill:true) () in
  let box = GPack.hbox ~homogeneous:false ~spacing:6 ~packing:button#add () in
  GuiTools.add_complex_box
    ~cont:((<:>) box)
    ~style:`ICONS
    ~icon:(A.get_icon ~icon:M.icon_stock_font ~size:A.SMALL ()) ();
  let _separator =
    GMisc.separator `VERTICAL
    ~packing:(box#pack ~expand:false ~fill:true) ()
  in
  let sample =
    GMisc.label ~width:100 ~xalign:0.
      ~markup:!M.cW_lb_font_sample
      ~packing:(box#pack ~expand:false ~fill:true) ()
  in
  let fontv = ref "" in
  let set_font font =
    sample#misc#modify_font_by_name font;
    fontv := font
  in
  set_font p.pref_new_value;
  let _separator =
    GMisc.separator `VERTICAL
    ~packing:(box#pack ~expand:false ~fill:true) ()
  in
  let _a_box =
    GPack.hbox ~homogeneous:false
      ~packing:(hbox#pack ~expand:true ~fill:true) ()
  in
  table#attach ~left:0 ~top
    ~xpadding:18 ~ypadding:0
    ~expand:`X ~fill:`X
    (event_wrap_widget ~w:label ~p ?h_label ());
  table#attach ~left:1 ~top
    ~xpadding:0 ~ypadding:0
    ~expand:`X ~fill:`X
    (event_wrap_widget ~w:hbox ~p ?h_label ());
  let f_sel () =
    let dialog = GWindow.font_selection_dialog
        ~title:(U.simple_utf8_of p.pref_label)
        ~modal:false
        ~show:true ()
    in
    dialog#selection#set_font_name !fontv;
    let wb_ok = dialog#ok_button in
    let wb_cancel = dialog#cancel_button in
    let _ = wb_ok#connect#clicked
        (fun () -> 
          let font = dialog#selection#font_name in
          set_font font;
          dialog#destroy ()
        )
    in
    ignore (wb_cancel#connect#clicked dialog#destroy)
  in
  ignore (button#connect#clicked f_sel);
  p.pref_apply <- (fun _ -> p.pref_new_value <- !fontv);
  p.pref_apply_default <- (fun _ -> set_font p.pref_default)

(*************************************************************************)
(*                                                                       *)
(*                         add_bool                                      *)
(*                                                                       *)
(*************************************************************************)

let add_bool ~p ~top ~(table : GPack.table) ?(h_label : GMisc.label option) () =
  let active = safe_bool p.pref_new_value in
  let check = GButton.check_button ~active ~label:(U.simple_utf8_of p.pref_label) () in
  table#attach ~left:0 ~top
    ~xpadding:18 ~ypadding:0
    ~right:2 ~bottom:(top + 1)
    ~expand:`X ~fill:`X
    (event_wrap_widget ~w:check ~p ?h_label ());
  check

(*************************************************************************)
(*                                                                       *)
(*                         add_bool_param                                *)
(*                                                                       *)
(*************************************************************************)

let add_bool_param ~p ~top ~(table : GPack.table) ?(h_label : GMisc.label option) () =
  let check = add_bool ~p ~top ~table ?h_label () in
  p.pref_apply <- (fun _ -> p.pref_new_value <- string_of_bool check#active);
  p.pref_apply_default <- (fun _ -> check#set_active (bool_of_string p.pref_default))

(*************************************************************************)
(*                                                                       *)
(*                         add_advanced_param                            *)
(*                                                                       *)
(*************************************************************************)

let add_advanced_param ~p ~top ~(table : GPack.table) ?(h_label : GMisc.label option) ~advanced_mode () =
  let check = add_bool ~p ~top ~table ?h_label () in
  p.pref_apply <-
        (fun _ -> 
           begin
             advanced_mode := check#active;
             p.pref_new_value <- string_of_bool check#active
          end
        );
  p.pref_apply_default <-
        (fun _ ->
           begin
             advanced_mode := check#active;
             check#set_active (bool_of_string p.pref_default)
           end
        )

(*************************************************************************)
(*                                                                       *)
(*                         add_combo_param                               *)
(*                                                                       *)
(*************************************************************************)

let set_combobox_value (combobox : GEdit.combo_box) (column : string GTree.column) v =
  combobox#model#foreach
    (fun _ row ->
      let s = combobox#model#get ~row ~column in
      if s = v
        then begin
          combobox#set_active_iter (Some row);
          true
        end else false)

let get_combobox_value (combobox : GEdit.combo_box) (column : string GTree.column) default =
  match combobox#active_iter with
      None -> default
    | Some row -> combobox#model#get ~row ~column

let add_combo_param ~p ~top ~(table : GPack.table) ?(h_label : GMisc.label option) () =
  let label = GMisc.label ~xalign:0. ~markup:(U.simple_utf8_of p.pref_label) () in
  let (combobox, (_, column)) =
    GEdit.combo_box_text ~strings:(List.map U.simple_utf8_of p.pref_option_list) ()
  in
  set_combobox_value combobox column (U.simple_utf8_of p.pref_new_value);
  table#attach ~left:0 ~top
    ~xpadding:18 ~ypadding:0
     ~expand:`X ~fill:`X
    (event_wrap_widget ~w:label ~p ?h_label ());
  table#attach ~left:1 ~top
    ~xpadding:0 ~ypadding:0
    ~expand:`X ~fill:`X
    (event_wrap_widget ~w:combobox ~p ?h_label ());
  p.pref_apply <- (fun _ -> p.pref_new_value <- get_combobox_value combobox column p.pref_default);
  p.pref_apply_default <- (fun _ -> set_combobox_value combobox column (U.simple_utf8_of p.pref_default))

(*************************************************************************)
(*                                                                       *)
(*                         add_time_param                                *)
(*                                                                       *)
(*************************************************************************)

let add_time_param ~p ~top ~(table : GPack.table) ?(h_label : GMisc.label option) () =
  let label = GMisc.label ~xalign:0. ~markup:(U.simple_utf8_of p.pref_label) () in
  let hbox = GPack.hbox ~homogeneous:false ~spacing:3 () in
  let range_day = GData.adjustment ~lower:0. ~upper:365. ~step_incr:1. () in
  let spin_day =
    GEdit.spin_button ~adjustment:range_day ~rate:1. ~digits:0 ~numeric:true
      ~snap_to_ticks:true ~update_policy:`IF_VALID ~wrap:true
      ~packing:(hbox#pack ~expand:false ~fill:true) ()
  in
  let _label_day =
    GMisc.label ~xalign:0. ~markup:!M.cW_lb_day
      ~packing:(hbox#pack ~expand:false ~fill:true) ()
  in
  let range_hour = GData.adjustment ~lower:0. ~upper:23. ~step_incr:1. () in
  let spin_hour =
    GEdit.spin_button ~adjustment:range_hour ~rate:1. ~digits:0 ~numeric:true
      ~snap_to_ticks:true ~update_policy:`IF_VALID ~wrap:true
      ~packing:(hbox#pack ~expand:false ~fill:true) ()
  in
  let _label_hour =
    GMisc.label ~xalign:0. ~markup:!M.cW_lb_hour
      ~packing:(hbox#pack ~expand:false ~fill:true) ()
  in
  let range_minute = GData.adjustment ~lower:0. ~upper:59. ~step_incr:1. () in
  let spin_minute =
    GEdit.spin_button ~adjustment:range_minute ~rate:1. ~digits:0 ~numeric:true
      ~snap_to_ticks:true ~update_policy:`IF_VALID ~wrap:true
      ~packing:(hbox#pack ~expand:false ~fill:true) ()
  in
  let _label_minute =
    GMisc.label ~xalign:0. ~markup:!M.cW_lb_minute
      ~packing:(hbox#pack ~expand:false ~fill:true) ()
  in
  let range_second = GData.adjustment ~lower:0. ~upper:59. ~step_incr:1. () in
  let spin_second =
    GEdit.spin_button ~adjustment:range_second ~rate:1. ~digits:0 ~numeric:true
      ~snap_to_ticks:true ~update_policy:`IF_VALID ~wrap:true
      ~packing:(hbox#pack ~expand:false ~fill:true) ()
  in
  let _label_second =
    GMisc.label ~xalign:0. ~markup:!M.cW_lb_second
      ~packing:(hbox#pack ~expand:false ~fill:true) ()
  in
  let _a_box =
    GPack.hbox ~homogeneous:false
      ~packing:(hbox#pack ~expand:true ~fill:true) ()
  in
  let set_time v =
    let time = int_of_float (safe_int v)  in
    let days = time / 60 / 60 / 24 in
    let rest = time - days * 60 * 60 * 24 in
    let hours = rest / 60 / 60 in
    let rest = rest - hours * 60 * 60 in
    let minutes = rest / 60 in
    let seconds = rest - minutes * 60 in
    spin_day#set_value (float_of_int days);
    spin_hour#set_value (float_of_int hours);
    spin_minute#set_value (float_of_int minutes);
    spin_second#set_value (float_of_int seconds)
  in
  let get_time _ =
    let days = spin_day#value_as_int * 60 * 60 * 24 in
    let hours = spin_hour#value_as_int * 60 * 60 in
    let minutes = spin_minute#value_as_int * 60 in
    let seconds = spin_second#value_as_int in
    let time = seconds + minutes + hours + days in
    string_of_int time
  in
  set_time p.pref_new_value;
  table#attach ~left:0 ~top
    ~xpadding:18 ~ypadding:0
    ~expand:`X ~fill:`X
    (event_wrap_widget ~w:label ~p ?h_label ());
  table#attach ~left:1 ~top
    ~xpadding:0 ~ypadding:0
    ~expand:`X ~fill:`X
    (event_wrap_widget ~w:hbox ~p ?h_label ());
  p.pref_apply <- (fun _ -> p.pref_new_value <- get_time () );
  p.pref_apply_default <- (fun _ -> set_time p.pref_default)

(*************************************************************************)
(*                                                                       *)
(*                         add_pref                                      *)
(*                                                                       *)
(*************************************************************************)

let add_pref ?h_label ~table ~p ~top ~advanced_mode () =
    match p.pref_type with
        BBool -> add_bool_param ~p ~top ~table ?h_label ()
      | BAdvanced -> add_advanced_param ~p ~top ~table ?h_label ~advanced_mode ()
      | BFilename -> add_filename_param ~p ~top ~table ~path:false ?h_label ()
      | BPath -> add_filename_param ~p ~top ~table ~path:true ?h_label ()
      | BPassword -> add_password_param ~p ~top ~table ?h_label ()
      | BCombo -> add_combo_param ~p ~top ~table ?h_label ()
      | BTime -> add_time_param ~p ~top ~table ?h_label ()
      | BColor -> add_color_param ~p ~top ~table ?h_label ()
      | BFont -> add_font_param ~p ~top ~table ?h_label ()
      | BInt -> add_int_param ~p ~f:safe_int ~top ~table ?h_label ()
      | BInt32 -> add_int_param ~p ~f:safe_int32 ~top ~table ?h_label ()
      | BInt64 -> add_int_param ~p ~f:safe_int64 ~top ~table ?h_label ()
      | BFloat -> add_float_param ~p ~top ~table ?h_label ()
      | BScale -> add_scale_param ~p ~top ~table ?h_label ()
      | _ -> add_string_param ~p ~top ~table ?h_label ()


(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(*                                                                       *)
(*                         CONFIGPANEL                                   *)
(*                                                                       *)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)

module ConfigPanel (CW:

(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(*                                                                       *)
(*                         FUNCTOR Argument                              *)
(*                                                                       *)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)

    sig

      type section
      and  subsection
      and  group

      val section_to_label    : section option -> string
      val subsection_to_label : subsection option -> string
      val group_to_label      : group option -> string
      val icon_from_section   : section option -> GdkPixbuf.pixbuf
      val advanced_mode       : bool ref
      val save_options        : (string * string) list ->        (* (pref_name, pref_new_value) *)
                                  (string, (section, subsection, group) preference) Hashtbl.t ->
                                     unit

    end) = 
  (struct

    open CW

(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(*                                                                       *)
(*                         FUNCTOR Body                                  *)
(*                                                                       *)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)

(*************************************************************************)
(*                                                                       *)
(*                         Global tables                                 *)
(*                                                                       *)
(*************************************************************************)

    let (preferences_values : (string, (section, subsection, group) preference) Hashtbl.t) =
      Hashtbl.create 103

    let (sections : (section option * (subsection option *
                        (group option * (pref_box *
                            string list ref) list ref) list ref)
                                list ref) list ref ) = ref []

(*************************************************************************)
(*                                                                       *)
(*                         Global variables                              *)
(*                                                                       *)
(*************************************************************************)

    let (win : GWindow.window option ref) = ref None

    let wwidth = ref ((Gdk.Screen.width ()) * 2 / 3)
    let wheight = ref ((Gdk.Screen.height ()) * 3 / 4)

    let (current_section : section option ref) = ref None

(*************************************************************************)
(*                                                                       *)
(*                         build_sections                                *)
(*                                                                       *)
(*************************************************************************)

let build_sections _ =
  Hashtbl.iter (fun name p ->
    let t = (p.pref_section, ref [p.pref_subsection, ref [p.pref_group, ref [p.pref_type, ref [p.pref_name]]]]) in
    let list = sections in
    if not (List.mem_assoc (fst t) !list)
      then begin
        list := List.sort (fun t1 t2 -> compare t1 t2) (t::!list)
      end else begin
        let list = List.assoc (fst t) !list in
        let t = List.hd !(snd t) in
        if not (List.mem_assoc (fst t) !list)
          then begin
            list := List.sort (fun t1 t2 -> compare t1 t2) (t::!list)
          end else begin
            let list = List.assoc (fst t) !list in
            let t = List.hd !(snd t) in
            if not (List.mem_assoc (fst t) !list)
              then begin
                list := List.sort (fun t1 t2 -> compare t1 t2) (t::!list)
              end else begin
                let list = List.assoc (fst t) !list in
                let t = List.hd !(snd t) in
                if not (List.mem_assoc (fst t) !list)
                  then begin
                    list := List.sort (fun t1 t2 -> compare t1 t2) (t::!list)
                  end else begin
                    let list = List.assoc (fst t) !list in
                    let t = List.hd !(snd t) in
                    if not (List.mem t !list)
                      then begin
                        list := List.sort (fun t1 t2 -> compare t1 t2) (t::!list)
                      end
                  end
              end
          end
      end
  ) preferences_values

(*************************************************************************)
(*                                                                       *)
(*                         add_group_name                                *)
(*                                                                       *)
(*************************************************************************)

let add_group_name ~grp ~(table : GPack.table) ~top =
  if grp <> None
    then begin
      let s = group_to_label grp in
      let hbox = GPack.hbox ~homogeneous:false ~border_width:6 () in
      let markup =
        Printf.sprintf "<span foreground=\"%s\" weight=\"bold\">%s</span>" 
          !!O.gtk_color_default s
      in
      let _label =
        GMisc.label ~xalign:0. ~markup
          ~packing:(hbox#pack ~expand:false ~fill:true) ()
      in
      table#attach ~left:0 ~top
        ~right:2 ~bottom:(top + 1)
        ~expand:`X ~fill:`X
        hbox#coerce
    end

(*************************************************************************)
(*                                                                       *)
(*                         add_groups                                    *)
(*                                                                       *)
(*************************************************************************)

let add_groups h_label groups =
  let scroll_table_box =
    GBin.scrolled_window ~hpolicy:`NEVER ~vpolicy:`AUTOMATIC
      ~placement:`TOP_LEFT ()
  in
  let table =
    GPack.table ~columns:2 ~homogeneous:false
       ~row_spacings:6 ~col_spacings:6 ~border_width:6
       ~packing:scroll_table_box#add_with_viewport ()
  in
  let index = ref 0 in
  List.iter (fun (grp, prefs) ->
     let list = ref [] in
     List.iter (fun (box, names) ->
       List.iter (fun name ->
         let p = Hashtbl.find preferences_values name in
         if (not p.pref_advanced) || !advanced_mode
           then list := p :: !list
       ) !names
     ) !prefs;
     list := List.rev !list;
     if !list <> [] 
       then begin
         add_group_name ~grp ~table ~top:!index;
         incr (index)
       end;
     List.iter (fun p ->
       add_pref ~h_label ~table ~p ~top:!index ~advanced_mode ();
       incr (index)
     ) !list
  ) !groups;
  scroll_table_box#coerce

(*************************************************************************)
(*                                                                       *)
(*                         add_subsections                               *)
(*                                                                       *)
(*************************************************************************)

let rec iter (notebook : GPack.notebook) h_label subsections =
  match subsections with
      [] -> ()
    | (sub, groups) :: tail ->
        begin
          let w = add_groups h_label groups in
          let markup = subsection_to_label sub in
          ignore (notebook#append_page 
            ~tab_label:((GMisc.label ~use_underline:true ~markup ())#coerce)
            w);
          iter notebook h_label tail
        end

let add_subsections (box : GPack.box) h_label subsections =
  match !subsections with
      [] -> ()
    | [None, groups] ->
        begin
          let w = add_groups h_label groups in
          box#pack ~expand:true ~fill:true w
        end
    | _ ->
        begin
          let notebook = GPack.notebook ~homogeneous_tabs:true ~scrollable:true
                 ~packing:(box#pack ~expand:true ~fill:true) ()
          in
          iter notebook h_label !subsections
        end

(*************************************************************************)
(*                                                                       *)
(*                         add_subsection                                *)
(*                                                                       *)
(*************************************************************************)

let add_section menu box pref_label h_label section subsections =
  let item = GList.list_item () in
  let label = section_to_label section in
  let markup = 
    Printf.sprintf "<span foreground=\"%s\" weight=\"bold\">%s</span>" 
      !!O.gtk_color_default label
  in
  GuiTools.add_complex_box
    ~cont:((<:>) item) 
    ~markup
    ~style:`BOTH
    ~icon:(icon_from_section section) ();
  menu#append item;
  item#set_border_width 6;
  ignore (item#connect#select ~callback:
    (fun _ ->
      pref_label#set_label (remove_ markup);
      let list = 
        try
          List.assoc !current_section !sections
        with Not_found -> ref []
      in
      List.iter (fun (sub, groups) ->
        List.iter (fun (grp, prefs) ->
          List.iter (fun (box, names) ->
            List.iter (fun name ->
              try
                let p = Hashtbl.find preferences_values name in
                p.pref_apply ()
              with Not_found -> ()
            ) !names
          ) !prefs
        ) !groups
      ) !list;
      List.iter (fun w -> w#destroy ()) box#children;
      current_section := None;
      add_subsections box h_label subsections;
      current_section := section
  ))

(*************************************************************************)
(*                                                                       *)
(*                         insert_options                                *)
(*                                                                       *)
(*************************************************************************)

let h_pb = A.get_icon ~icon:M.icon_menu_help ~size:A.LARGE ()


let insert_options (menu : GList.liste) (opt_box : GPack.box) =
  let pref_evbox =
    GBin.event_box ~packing:(opt_box#pack ~expand:false ~fill:true) ()
  in
  pref_evbox#misc#modify_bg [(`NORMAL, (`NAME "#AFAFF4"))];
  let pref_label =
    GMisc.label ~xalign:0. ~yalign:0.
      ~xpad:3 ~ypad:3 ~packing:pref_evbox#add ()
  in
  pref_label#set_use_markup true;
  let box_table =
    GPack.vbox ~homogeneous:false
      ~packing:(opt_box#pack ~expand:true ~fill:true) ()
  in
  let frame =
    GBin.frame ~height:(!wheight / 7)
      ~packing:(opt_box#pack ~expand:false ~fill:true) ()
  in
  let h_hbox =
    GPack.hbox ~homogeneous:false ~spacing:6
      ~border_width:3 ~packing:frame#add ()
  in
  let _icon_h =
    GMisc.image ~pixbuf:h_pb 
      ~packing:(h_hbox#pack ~expand:false ~fill:true) ()
  in
  let  h_vbox =
    GPack.hbox ~homogeneous:false ~border_width:3
       ~packing:(h_hbox#pack ~expand:true ~fill:true) ()
  in
  let h_label =
    GMisc.label ~xalign:0. ~yalign:0.
      ~packing:(h_vbox#pack ~expand:true ~fill:true) ()
  in
  List.iter (fun (sec,subs) -> 
    add_section menu box_table pref_label h_label sec subs
  ) !sections;
  menu#select_item ~pos:0

(*************************************************************************)
(*                                                                       *)
(*                         save_all_prefs                                *)
(*                                                                       *)
(*************************************************************************)

let save_all_prefs _ =
  let list = ref [] in
  Hashtbl.iter (fun name p ->
    (if p.pref_section = !current_section
      then p.pref_apply ());
    if p.pref_value <> p.pref_new_value
      then begin
          list := (p.pref_name, p.pref_new_value) :: !list;
          p.pref_new_value <- p.pref_value
      end

  ) preferences_values;
  save_options !list preferences_values

(*************************************************************************)
(*                                                                       *)
(*                         save_section_prefs                            *)
(*                                                                       *)
(*************************************************************************)

let save_section_prefs _ =
  let list = ref [] in
  Hashtbl.iter (fun name p ->
    if p.pref_section = !current_section
      then begin
        p.pref_apply ();
        if p.pref_value <> p.pref_new_value
          then begin
            list := (p.pref_name, p.pref_new_value) :: !list;
            p.pref_new_value <- p.pref_value
            end
      end
  ) preferences_values;
  save_options !list preferences_values

(*************************************************************************)
(*                                                                       *)
(*                         clear                                         *)
(*                                                                       *)
(*************************************************************************)

let clear () =
  sections := [];
  Hashtbl.clear preferences_values

(*************************************************************************)
(*                                                                       *)
(*                         CONFIG WINDOW                                 *)
(*                                                                       *)
(*************************************************************************)


let create_config_window on_ok =
  let window =
    GWindow.window ~width:!wwidth ~height:!wheight
      ~title:(remove_ !M.mW_lb_settings)
      ~icon:(A.get_icon ~icon:M.icon_menu_settings ~size:A.SMALL ())
      ~position:`CENTER_ALWAYS
      ~kind:`TOPLEVEL
      ~resizable:true ~modal:false ()
  in
  window#set_skip_taskbar_hint false;
  window#set_skip_pager_hint false;
  ignore (window#connect#destroy ~callback:
    (fun _ ->
       win := None;
       current_section := None
  ));
  let vbox =
    GPack.vbox ~homogeneous:false
      ~packing:window#add ()
  in
  let hbox = 
    GPack.hbox ~homogeneous:false ~border_width:6
      ~packing:(vbox#pack ~expand:true ~fill:true) ()
  in
  let vbox_menu =
    GPack.vbox ~homogeneous:false ~border_width:6
      ~packing:(hbox#pack ~expand:false ~fill:true) ()
  in
  let vbox_opt =
    GPack.vbox ~homogeneous:false ~border_width:6 ~spacing:6
      ~packing:(hbox#pack ~expand:true ~fill:true) ()
  in
  let scroll_menu_box =
    GBin.scrolled_window ~hpolicy:`NEVER ~vpolicy:`ALWAYS
      ~placement:`TOP_LEFT
      ~packing:(vbox_menu#pack ~expand:true ~fill:true) ()
  in
  let menu_list =
    GList.liste ~selection_mode:`SINGLE
      ~packing:scroll_menu_box#add_with_viewport ()
  in
  let _separator =
    GMisc.separator `HORIZONTAL
    ~packing:(vbox#pack ~expand:false ~fill:true) ()
  in
  let hbox_button =
    GPack.hbox ~homogeneous:false ~border_width:6
      ~packing:(vbox#pack ~expand:false ~fill:true) ()
  in
  let bbox = 
    GuiTools.tool_bar `HORIZONTAL ~border_width:6 ~spacing:6
      ~layout:`END ~packing:hbox_button#add ()
  in
  ignore (bbox#add_button
    ~markup:!M.pW_lb_cancel
    ~style:`BOTH_HORIZ
    ~icon:(A.get_icon ~icon:M.icon_stock_close ~size:A.SMALL ())
    ~f:(window#destroy) ()
  );
  let f () =
    Hashtbl.iter (fun name p -> 
      if p.pref_section = !current_section
        then p.pref_apply_default ()
    ) preferences_values
  in
  ignore (bbox#add_button
    ~markup:!M.pW_lb_default
    ~style:`TEXT
    ~f ()
  );
  let f () =
    save_section_prefs ()
  in
  ignore (bbox#add_button
    ~markup:!M.pW_lb_apply
    ~style:`TEXT
    ~f ()
  );
  let f () =
    save_all_prefs ();
    on_ok ();
    window#destroy ()
  in
  ignore (bbox#add_button 
    ~markup:!M.pW_lb_ok
    ~style:`BOTH_HORIZ
    ~icon:(A.get_icon ~icon:M.icon_stock_ok ~size:A.SMALL ())
    ~f ()
  );
  insert_options menu_list vbox_opt;
  win := Some window;
  window#show ()




let config_window ?(on_ok = fun () -> ()) () =
  match !win with
      Some w ->
        w#present ()
    | None ->
       begin
         let _ =
            match !sections with
                [] -> build_sections ()
              | _ -> ()
          in
          create_config_window on_ok
        end

  end)

(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(*                                                                       *)
(*                         PANEL                                         *)
(*                                                                       *)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)

let panel ~(structure: (string * ('a, 'b, 'c) preference list) list)
  ?(title=(remove_ !M.mW_lb_settings))
  ?(icon=(A.get_icon ~icon:M.icon_menu_settings ~size:A.SMALL ())) 
  ?(advanced_mode=ref false)
  ?(on_ok = fun () -> ()) () =
  let window =
    GWindow.window
      ~title
      ~icon
      ~position:`CENTER_ALWAYS
      ~kind:`TOPLEVEL
      ~resizable:true ~modal:false ()
  in
  window#set_skip_taskbar_hint false;
  window#set_skip_pager_hint false;
  let vbox =
    GPack.vbox ~homogeneous:false
      ~packing:window#add ()
  in
  let notebook =
    GPack.notebook ~homogeneous_tabs:true ~scrollable:true
      ~packing:(vbox#pack ~expand:true ~fill:true) ()
  in
  let _separator =
    GMisc.separator `HORIZONTAL
    ~packing:(vbox#pack ~expand:false ~fill:true) ()
  in
  let hbox_button =
    GPack.hbox ~homogeneous:false ~border_width:6
      ~packing:(vbox#pack ~expand:false ~fill:true) ()
  in
  let bbox = 
    GuiTools.tool_bar `HORIZONTAL ~border_width:6 ~spacing:6
      ~layout:`END ~packing:hbox_button#add ()
  in
  ignore (bbox#add_button
    ~markup:!M.pW_lb_cancel
    ~style:`BOTH_HORIZ
    ~icon:(A.get_icon ~icon:M.icon_stock_close ~size:A.SMALL ())
    ~f:(window#destroy) ()
  );
  let f () =
    List.iter (fun (_, prefs) ->
      List.iter (fun p ->
        p.pref_apply_default ()
      ) prefs;
    ) structure
  in
  ignore (bbox#add_button
    ~markup:!M.pW_lb_default
    ~style:`TEXT
    ~f ()
  );
  let f () =
    List.iter (fun (_, prefs) ->
      List.iter (fun p ->
        p.pref_apply ();
      ) prefs;
    ) structure;
    on_ok ();
    window#destroy ()
  in
  ignore (bbox#add_button 
    ~markup:!M.pW_lb_ok
    ~style:`BOTH_HORIZ
    ~icon:(A.get_icon ~icon:M.icon_stock_ok ~size:A.SMALL ())
    ~f ()
  );
  List.iter (fun (s, prefs) ->
    let top = ref 0 in
    let markup = GuiTools.create_markup s in
    let scrolled_box =
      GBin.scrolled_window ~hpolicy:`NEVER ~vpolicy:`ALWAYS
        ~placement:`TOP_LEFT ()
    in
    let table =
      GPack.table ~columns:2 ~homogeneous:false
         ~row_spacings:6 ~col_spacings:6 ~border_width:6
         ~packing:scrolled_box#add_with_viewport ()
    in
    ignore (notebook#append_page 
      ~tab_label:((GMisc.label ~use_underline:true ~markup ())#coerce)
      scrolled_box#coerce);
    List.iter (fun p->
      add_pref ~table ~p ~top:!top ~advanced_mode ();
      incr top
    ) prefs;
  ) structure;
  window#show ()

(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(*                                                                       *)
(*                         SIMPLE_PANEL                                  *)
(*                                                                       *)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)

let simple_panel ~(prefs: (('a, 'b, 'c) preference) list) 
  ?(title=(remove_ !M.mW_lb_settings))
  ?(icon=(A.get_icon ~icon:M.icon_menu_settings ~size:A.SMALL ())) 
  ?(advanced_mode=ref false)
  ?(on_ok = fun () -> ()) () =
  let window =
    GWindow.window
      ~title
      ~icon
      ~position:`CENTER_ALWAYS
      ~kind:`TOPLEVEL
      ~resizable:true ~modal:false ()
  in
  window#set_skip_taskbar_hint false;
  window#set_skip_pager_hint false;
  let vbox =
    GPack.vbox ~homogeneous:false
      ~packing:window#add ()
  in
  let table =
    GPack.table ~columns:2 ~homogeneous:false
       ~row_spacings:6 ~col_spacings:6 ~border_width:6
       ~packing:vbox#add ()
  in
  let _separator =
    GMisc.separator `HORIZONTAL
    ~packing:(vbox#pack ~expand:false ~fill:true) ()
  in
  let hbox_button =
    GPack.hbox ~homogeneous:false ~border_width:6
      ~packing:(vbox#pack ~expand:false ~fill:true) ()
  in
  let bbox = 
    GuiTools.tool_bar `HORIZONTAL ~border_width:6 ~spacing:6
      ~layout:`END ~packing:hbox_button#add ()
  in
  ignore (bbox#add_button
    ~markup:!M.pW_lb_cancel
    ~style:`BOTH_HORIZ
    ~icon:(A.get_icon ~icon:M.icon_stock_close ~size:A.SMALL ())
    ~f:(window#destroy) ()
  );
  let f () =
    List.iter (fun p -> 
      p.pref_apply_default ()
    ) prefs
  in
  ignore (bbox#add_button
    ~markup:!M.pW_lb_default
    ~style:`TEXT
    ~f ()
  );
  let f () =
    List.iter (fun p -> 
      p.pref_apply ();
    ) prefs;
    on_ok ();
    window#destroy ()
  in
  ignore (bbox#add_button 
    ~markup:!M.pW_lb_ok
    ~style:`BOTH_HORIZ
    ~icon:(A.get_icon ~icon:M.icon_stock_ok ~size:A.SMALL ())
    ~f ()
  );
  let top = ref 0 in
  List.iter (fun p ->
    add_pref ~table ~p ~top:!top ~advanced_mode ();
    incr top
  ) prefs;
  window#show ()

(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(*                                                                       *)
(*                         INPUT_WINDOW                                  *)
(*                                                                       *)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)

let input_window ~(pref: ('a, 'b, 'c) preference) 
  ?(title=(remove_ !M.mW_lb_settings))
  ?(icon=(A.get_icon ~icon:M.icon_menu_settings ~size:A.SMALL ())) 
  ?(advanced_mode=ref false)
  ?(on_ok = fun () -> ()) () =
  let window =
    GWindow.window
      ~title
      ~icon
      ~position:`CENTER_ALWAYS
      ~kind:`TOPLEVEL
      ~resizable:true ~modal:false ()
  in
  window#set_skip_taskbar_hint false;
  window#set_skip_pager_hint false;
  let vbox =
    GPack.vbox ~homogeneous:false
      ~packing:window#add ()
  in
  let table =
    GPack.table ~columns:2 ~homogeneous:false
       ~row_spacings:6 ~col_spacings:6 ~border_width:6
       ~packing:vbox#add ()
  in
  let _separator =
    GMisc.separator `HORIZONTAL
    ~packing:(vbox#pack ~expand:false ~fill:true) ()
  in
  let hbox_button =
    GPack.hbox ~homogeneous:false ~border_width:6
      ~packing:(vbox#pack ~expand:false ~fill:true) ()
  in
  let bbox = 
    GuiTools.tool_bar `HORIZONTAL ~border_width:6 ~spacing:6
      ~layout:`END ~packing:hbox_button#add ()
  in
  ignore (bbox#add_button
    ~markup:!M.pW_lb_cancel
    ~style:`BOTH_HORIZ
    ~icon:(A.get_icon ~icon:M.icon_stock_close ~size:A.SMALL ())
    ~f:(window#destroy) ()
  );
  ignore (bbox#add_button
    ~markup:!M.pW_lb_default
    ~style:`TEXT
    ~f:(pref.pref_apply_default) ()
  );
  let f () =
    pref.pref_apply ();
    on_ok ();
    window#destroy ()
  in
  ignore (bbox#add_button 
    ~markup:!M.pW_lb_ok
    ~style:`BOTH_HORIZ
    ~icon:(A.get_icon ~icon:M.icon_stock_ok ~size:A.SMALL ())
    ~f ()
  );
  add_pref ~table ~p:pref ~top:0 ~advanced_mode ();
  window#show ()
