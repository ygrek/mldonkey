(* Copyright 2004 b8_bavard, b8_fee_carabine, INRIA *)
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

(* preference settings of MLchat. *)

module A = Chat_art

type pref_box =
  BFilename
| BPath
| BPassword
| BString
| BCombo
| BTime
| BColor
| BFont
| BFloat
| BInt
| BInt32
| BInt64
| BAdvanced
| BBool

type preference = 
  {
    pref_help : string;
    pref_advanced : bool;
    pref_default : string;

    mutable pref_name : string;
    mutable pref_label : string;
    mutable pref_option_list : string list;    (* Used for combo_box *)
    mutable pref_value : string;
    mutable pref_new_value : string;           (* Internally used *)
    mutable pref_type : pref_box;
    mutable pref_apply : unit -> unit;         (* Becarefull overwritten *)
    mutable pref_apply_default : unit -> unit; (* Becarefull overwritten *)
  }

let preference ?(help="") label v box_type () =
  {
    pref_help = help;
    pref_advanced = false;
    pref_default = v;

    pref_name = label;
    pref_label = label;
    pref_option_list = [];
    pref_value = v;
    pref_new_value = v;
    pref_type = box_type;
    pref_apply = (fun () -> ());
    pref_apply_default = (fun () -> ());
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
                   then (label#set_label p.pref_help; true)
                   else false
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
      ~markup:p.pref_label ()
  in
  let edit =
     GEdit.entry ~text:p.pref_new_value 
       ~editable:true ~visibility:true ()
  in
  table#attach ~left:0 ~top 
    ~xpadding:18 ~ypadding:0 
    ~expand:`X ~fill:`X
    (event_wrap_widget ~w:label ~p ?h_label ());
  table#attach ~left:1 ~top
    ~xpadding:0 ~ypadding:0
    ~expand:`X ~fill:`X
    (event_wrap_widget ~w:edit ~p ?h_label ());
  p.pref_apply <- (fun _ -> p.pref_new_value <- edit#text);
  p.pref_apply_default <- (fun _ -> edit#set_text p.pref_default)

(*************************************************************************)
(*                                                                       *)
(*                         add_password_param                            *)
(*                                                                       *)
(*************************************************************************)

let add_password_param ~p ~top ~(table : GPack.table) ?(h_label : GMisc.label option) () =
  let label = GMisc.label ~xalign:0. ~markup:p.pref_label () in
  let hbox = GPack.hbox ~homogeneous:false ~spacing:6 () in
  let pixbuf = A.get_icon ~icon:"icon_password" () in
  let image = GMisc.image ~pixbuf ~packing:(hbox#pack ~expand:false ~fill:true) () in
  let edit =
    GEdit.entry ~text:p.pref_new_value ~editable:true ~visibility:false
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
  p.pref_apply_default <- (fun _ -> edit#set_text p.pref_default)

(*************************************************************************)
(*                                                                       *)
(*                         add_int_param                                 *)
(*                                                                       *)
(*************************************************************************)

let add_int_param ~p ~f ~top ~(table : GPack.table) ?(h_label : GMisc.label option) () =
  let value = f p.pref_new_value in
  let label = GMisc.label ~xalign:0. ~markup:p.pref_label () in
  let hbox = GPack.hbox ~homogeneous:false ~spacing:3 () in
  let range = GData.adjustment ~lower:0. ~upper:(float_of_int max_int) ~step_incr:1. () in
  let spin =
    GEdit.spin_button ~adjustment:range ~rate:1. ~digits:0 ~numeric:true
      ~snap_to_ticks:true ~update_policy:`IF_VALID ~value ~wrap:true
      ~packing:(hbox#pack ~expand:false ~fill:true) ()
  in
  let a_box =
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
  let label = GMisc.label ~xalign:0. ~markup:p.pref_label () in
  let hbox = GPack.hbox ~homogeneous:false ~spacing:3 () in
  let range = GData.adjustment ~lower:0. ~upper:((float_of_int max_int) *. 1000.) ~step_incr:0.1 () in
  let spin =
    GEdit.spin_button ~adjustment:range ~rate:1. ~digits:1 ~numeric:true
      ~snap_to_ticks:true ~update_policy:`IF_VALID ~value ~wrap:true
      ~packing:(hbox#pack ~expand:false ~fill:true) ()
  in
  let a_box =
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
  let label = GMisc.label ~xalign:0. ~markup:p.pref_label () in
  let hbox = GPack.hbox ~homogeneous:false ~spacing:6 () in
  let edit =
    GEdit.entry ~text:p.pref_new_value ~editable:true ~visibility:true
      ~packing:(hbox#pack ~expand:true ~fill:true) ()
  in
  let button = GButton.button ~packing:(hbox#pack ~expand:false ~fill:true) () in
  let bbox = GPack.hbox ~packing:button#add () in
  let bimage =
    GMisc.image ~pixbuf:(A.get_icon ~icon:"icon_directory" ())
      ~packing:(bbox#pack ~expand:false ~fill:true) ()
  in
  let blabel =
    GMisc.label ~markup:" _Browse ..." ~use_underline:true
      ~packing:(bbox#pack ~expand:false ~fill:true) ()
  in
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
      ~title:p.pref_label
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
                   then edit#set_text (List.hd l)
                   else edit#set_text !last_dir
         in
         dialog#destroy ()
    ));
    ignore (wb_cancel#connect#clicked dialog#destroy)
  in
  ignore (button#connect#clicked f_sel);
  p.pref_apply <- (fun _ -> p.pref_new_value <- edit#text);
  p.pref_apply_default <- (fun _ -> edit#set_text p.pref_default)

(*************************************************************************)
(*                                                                       *)
(*                         add_color_param                               *)
(*                                                                       *)
(*************************************************************************)

let add_color_param ~p ~top ~(table : GPack.table) ?(h_label : GMisc.label option) () =
  let label = GMisc.label ~xalign:0. ~markup:p.pref_label () in
  let hbox = GPack.hbox ~homogeneous:false () in
  let button = GButton.button ~packing:(hbox#pack ~expand:false ~fill:true) () in
  let box = GPack.hbox ~homogeneous:false ~spacing:6 ~packing:button#add () in
  let bimage =
    GMisc.image ~pixbuf:(A.get_icon ~icon:"icon_color" ())
      ~packing:(box#pack ~expand:false ~fill:true) ()
  in
  let separator =
    GMisc.separator `VERTICAL
    ~packing:(box#pack ~expand:false ~fill:true) ()
  in
  let pixbuf = GdkPixbuf.create ~width:100 ~height:16 () in
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
    let s = Printf.sprintf "%02X%02X%02X" r g b in
    let _ =
      for i = 1 to (String.length s) - 1 do
        if s.[i] = ' ' then s.[i] <- '0'
      done
    in
    colv := "#" ^ s; 
    ("0x" ^ s ^ "FF")
  in
  let col = GDraw.color (`NAME p.pref_new_value) in
  set_color (string_of_int_of_col col);
  let sample = GMisc.image ~pixbuf ~packing:box#add () in
  let separator =
    GMisc.separator `VERTICAL
    ~packing:(box#pack ~expand:false ~fill:true) ()
  in
  let a_box =
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
        ~title:p.pref_label
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
  let label = GMisc.label ~xalign:0. ~markup:p.pref_label () in
  let hbox = GPack.hbox ~homogeneous:false () in
  let button = GButton.button ~packing:(hbox#pack ~expand:false ~fill:true) () in
  let box = GPack.hbox ~homogeneous:false ~spacing:6 ~packing:button#add () in
  let bimage =
    GMisc.image ~pixbuf:(A.get_icon ~icon:"icon_font" ())
      ~packing:(box#pack ~expand:false ~fill:true) ()
  in
  let separator =
    GMisc.separator `VERTICAL
    ~packing:(box#pack ~expand:false ~fill:true) ()
  in
  let sample =
    GMisc.label ~width:100 ~xalign:0.
      ~markup:"Font sample"
      ~packing:(box#pack ~expand:false ~fill:true) ()
  in
  let fontv = ref "" in
  let set_font font =
    sample#misc#modify_font_by_name font;
    fontv := font
  in
  set_font p.pref_new_value;
  let separator =
    GMisc.separator `VERTICAL
    ~packing:(box#pack ~expand:false ~fill:true) ()
  in
  let a_box =
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
	~title:p.pref_label
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
  let check = GButton.check_button ~active ~label:p.pref_label () in
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

let get_combobox_value (combobox : GEdit.combo_box) (column : string GTree.column) default_value =
  match combobox#active_iter with
      None -> default_value
    | Some row -> combobox#model#get ~row ~column

let add_combo_param ~p ~top ~(table : GPack.table) ?(h_label : GMisc.label option) () =
  let label = GMisc.label ~xalign:0. ~markup:p.pref_label () in
  let (combobox, (_, column)) =
    GEdit.combo_box_text ~strings:p.pref_option_list ()
  in
  set_combobox_value combobox column p.pref_new_value;
  table#attach ~left:0 ~top
    ~xpadding:18 ~ypadding:0
     ~expand:`X ~fill:`X
    (event_wrap_widget ~w:label ~p ?h_label ());
  table#attach ~left:1 ~top
    ~xpadding:0 ~ypadding:0
    ~expand:`X ~fill:`X
    (event_wrap_widget ~w:combobox ~p ?h_label ());
  p.pref_apply <- (fun _ -> p.pref_new_value <- get_combobox_value combobox column p.pref_default);
  p.pref_apply_default <- (fun _ -> set_combobox_value combobox column p.pref_default)

(*************************************************************************)
(*                                                                       *)
(*                         add_time_param                                *)
(*                                                                       *)
(*************************************************************************)

let add_time_param ~p ~top ~(table : GPack.table) ?(h_label : GMisc.label option) () =
  let label = GMisc.label ~xalign:0. ~markup:p.pref_label () in
  let hbox = GPack.hbox ~homogeneous:false ~spacing:3 () in
  let range_day = GData.adjustment ~lower:0. ~upper:365. ~step_incr:1. () in
  let spin_day =
    GEdit.spin_button ~adjustment:range_day ~rate:1. ~digits:0 ~numeric:true
      ~snap_to_ticks:true ~update_policy:`IF_VALID ~wrap:true
      ~packing:(hbox#pack ~expand:false ~fill:true) ()
  in
  let label_day =
    GMisc.label ~xalign:0. ~markup:"d"
      ~packing:(hbox#pack ~expand:false ~fill:true) ()
  in
  let range_hour = GData.adjustment ~lower:0. ~upper:23. ~step_incr:1. () in
  let spin_hour =
    GEdit.spin_button ~adjustment:range_hour ~rate:1. ~digits:0 ~numeric:true
      ~snap_to_ticks:true ~update_policy:`IF_VALID ~wrap:true
      ~packing:(hbox#pack ~expand:false ~fill:true) ()
  in
  let label_hour =
    GMisc.label ~xalign:0. ~markup:"h"
      ~packing:(hbox#pack ~expand:false ~fill:true) ()
  in
  let range_minute = GData.adjustment ~lower:0. ~upper:59. ~step_incr:1. () in
  let spin_minute =
    GEdit.spin_button ~adjustment:range_minute ~rate:1. ~digits:0 ~numeric:true
      ~snap_to_ticks:true ~update_policy:`IF_VALID ~wrap:true
      ~packing:(hbox#pack ~expand:false ~fill:true) ()
  in
  let label_minute =
    GMisc.label ~xalign:0. ~markup:"min"
      ~packing:(hbox#pack ~expand:false ~fill:true) ()
  in
  let range_second = GData.adjustment ~lower:0. ~upper:59. ~step_incr:1. () in
  let spin_second =
    GEdit.spin_button ~adjustment:range_second ~rate:1. ~digits:0 ~numeric:true
      ~snap_to_ticks:true ~update_policy:`IF_VALID ~wrap:true
      ~packing:(hbox#pack ~expand:false ~fill:true) ()
  in
  let label_second =
    GMisc.label ~xalign:0. ~markup:"s"
      ~packing:(hbox#pack ~expand:false ~fill:true) ()
  in
  let a_box =
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
      | _ -> add_string_param ~p ~top ~table ?h_label ()

(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(*                                                                       *)
(*                         PANEL                                         *)
(*                                                                       *)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)

let panel ~(structure: (string * preference list) list)
  ?(title=Chat_messages.software)
  ?(width=Gdk.Screen.width () * 2 / 5)
  ?(height=Gdk.Screen.height () * 2 / 5)
  ?(icon=(A.get_icon ~icon:"icon_settings" ())) 
  ?(advanced_mode=ref false)
  ?(on_ok = fun () -> ()) () =
  let window =
    GWindow.window ~width ~height
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
  let separator =
    GMisc.separator `HORIZONTAL
    ~packing:(vbox#pack ~expand:false ~fill:true) ()
  in
  let hbox_button =
    GPack.hbox ~homogeneous:false ~border_width:6
      ~packing:(vbox#pack ~expand:false ~fill:true) ()
  in
  let bbox = 
    GPack.button_box `HORIZONTAL ~border_width:6 ~spacing:6
      ~layout:`END ~packing:hbox_button#add ()
  in
  let b_close = GButton.button ~packing:bbox#add () in
  let box = GPack.hbox ~spacing:6 ~border_width:3 ~packing:b_close#add () in
  let bimage =
    GMisc.image ~pixbuf:(A.get_icon ~icon:"icon_close" ())
      ~packing:(box#pack ~expand:false ~fill:true) ()
  in
  let blabel =
    GMisc.label ~markup:"_Close" ~use_underline:true
      ~packing:(box#pack ~expand:false ~fill:true) ()
  in
  ignore (b_close#connect#clicked ~callback:
        (fun _ -> window#destroy ()
  ));
  let callback () =
    List.iter (fun (_, prefs) ->
      List.iter (fun p ->
        p.pref_apply_default ()
      ) prefs;
    ) structure
  in
  let b_default =
    GButton.button ~label:"_Default"
      ~use_mnemonic:true ~packing:bbox#add ()
  in
  ignore (b_close#connect#clicked ~callback);
  let callback () =
    List.iter (fun (_, prefs) ->
      List.iter (fun p ->
        p.pref_apply ();
      ) prefs;
    ) structure;
    on_ok ();
    window#destroy ()
  in
  let b_ok = GButton.button ~packing:bbox#add () in
  let box = GPack.hbox ~spacing:6 ~border_width:3 ~packing:b_ok#add () in
  let bimage =
    GMisc.image ~pixbuf:(A.get_icon ~icon:"icon_ok" ())
      ~packing:(box#pack ~expand:false ~fill:true) ()
  in
  let blabel =
    GMisc.label ~markup:"_Ok" ~use_underline:true
      ~packing:(box#pack ~expand:false ~fill:true) ()
  in
  ignore (b_ok#connect#clicked ~callback);
  List.iter (fun (s, prefs) ->
    let top = ref 0 in
    let scrolled_box =
      GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
        ~placement:`TOP_LEFT ()
    in
    let table =
      GPack.table ~columns:2 ~homogeneous:false
         ~row_spacings:6 ~col_spacings:6 ~border_width:6
         ~packing:scrolled_box#add_with_viewport ()
    in
    notebook#append_page 
      ~tab_label:((GMisc.label ~use_underline:true ~markup:s ())#coerce)
      scrolled_box#coerce;
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

let simple_panel ~(prefs: preference list) 
  ?(title=Chat_messages.software)
  ?(icon=(A.get_icon ~icon:"icon_mlchat" ()))
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
  let separator =
    GMisc.separator `HORIZONTAL
    ~packing:(vbox#pack ~expand:false ~fill:true) ()
  in
  let hbox_button =
    GPack.hbox ~homogeneous:false ~border_width:6
      ~packing:(vbox#pack ~expand:false ~fill:true) ()
  in
  let bbox = 
    GPack.button_box `HORIZONTAL ~border_width:6 ~spacing:6
      ~layout:`END ~packing:hbox_button#add ()
  in
  let b_close = GButton.button ~packing:bbox#add () in
  let box = GPack.hbox ~spacing:6 ~border_width:2 ~packing:b_close#add () in
  let bimage =
    GMisc.image ~pixbuf:(A.get_icon ~icon:"icon_close" ())
      ~packing:(box#pack ~expand:false ~fill:true) ()
  in
  let blabel =
    GMisc.label ~markup:"_Close" ~use_underline:true
      ~packing:(box#pack ~expand:false ~fill:true) ()
  in
  ignore (b_close#connect#clicked ~callback:
        (fun _ -> window#destroy ()
  ));
  let callback () =
    List.iter (fun p -> 
      p.pref_apply ();
    ) prefs;
    on_ok ();
    window#destroy ()
  in
  let b_ok = GButton.button ~packing:bbox#add () in
  let box = GPack.hbox ~spacing:6 ~border_width:3 ~packing:b_ok#add () in
  let bimage =
    GMisc.image ~pixbuf:(A.get_icon ~icon:"icon_ok" ())
      ~packing:(box#pack ~expand:false ~fill:true) ()
  in
  let blabel =
    GMisc.label ~markup:"_Ok" ~use_underline:true
      ~packing:(box#pack ~expand:false ~fill:true) ()
  in
  ignore (b_ok#connect#clicked ~callback);
  let top = ref 0 in
  List.iter (fun p ->
    add_pref ~table ~p ~top:!top ~advanced_mode ();
    incr top
  ) prefs;
  window#show ()
