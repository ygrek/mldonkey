(* Copyright 2005 b8_bavard, INRIA *)
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

(* the graph tab of the GUI. *)

open Options
open GraphTypes
open CommonTypes

module G  = GuiGlobal
module Gb = GuiGraphBase
module M  = GuiMessages
module O  = GuiOptions
module U  = GuiUtf8

let verbose = O.gtk_verbose_graph

let lprintf' fmt =
  Printf2.lprintf ("GuiGraph: " ^^ fmt)

(*************************************************************************)
(*                                                                       *)
(*                         Global variables                              *)
(*                                                                       *)
(*************************************************************************)

let g_width = ((Gdk.Screen.width ()) * 4 / 9)
let g_height = ((Gdk.Screen.height ()) * 1 / 3)

let last_file = ref NoUid

let graphtime_cols = new GTree.column_list
let graphtime_num = graphtime_cols#add Gobject.Data.int
let graphtime_text = graphtime_cols#add Gobject.Data.string
let graphtime_store = GTree.list_store graphtime_cols

let file_cols = new GTree.column_list
let file_id = file_cols#add Gobject.Data.caml
let file_name = file_cols#add Gobject.Data.string
let file_store = GTree.list_store file_cols

(*************************************************************************)
(*                                                                       *)
(*                         lighten                                       *)
(*                                                                       *)
(*************************************************************************)

let lighten col =
  let gdk_col = GDraw.color ~colormap:(Gdk.Color.get_system_colormap ()) col in
  let max_c = 255 * 256 in
  let col_step = 100 * 256 in
  let r = min max_c (Gdk.Color.red   gdk_col + col_step) in
  let g = min max_c (Gdk.Color.green gdk_col + col_step) in
  let b = min max_c (Gdk.Color.blue  gdk_col + col_step) in
  `RGB (r, g, b)

(*************************************************************************)
(*                                                                       *)
(*                         graphtime_to_label                            *)
(*                                                                       *)
(*************************************************************************)

let graph_times =
  [
   GraphQuarter;
   GraphHour;
   GraphHalfDay;
   GraphDay;
   GraphWeek;
   GraphMonth;
   GraphYear;
  ]

let graphtime_to_label g =
  match g with
      GraphQuarter -> !M.gT_lb_quarter
    | GraphHour -> !M.gT_lb_hour
    | GraphHalfDay -> !M.gT_lb_halfday
    | GraphDay -> !M.gT_lb_day
    | GraphWeek -> !M.gT_lb_week
    | GraphMonth -> !M.gT_lb_month
    | GraphYear -> !M.gT_lb_year

let _ =
  let n = ref 0 in
  List.iter (fun g ->
    let row = graphtime_store#append () in
    graphtime_store#set ~row ~column:graphtime_num !n;
    graphtime_store#set ~row ~column:graphtime_text (U.utf8_of (graphtime_to_label g));
    incr n
  ) graph_times

(*************************************************************************)
(*                                                                       *)
(*                         Module Curve                                  *)
(*                                                                       *)
(*************************************************************************)

module Curve(C:

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

      val graph_time       : graph_time option_record
      val graph_foreground : string option_record
      val graph_size       : (int * int)

      val vertical_flip    : bool

      val module_name      : string

    end) =
  (struct

  open C


let lprintf'' fmt =
  Printf2.lprintf ("GuiGraph - %s " ^^ fmt) module_name

(*************************************************************************)
(*                                                                       *)
(*                         Global variables                              *)
(*                                                                       *)
(*************************************************************************)

let graph_data = ref (Gb.dummy_graph ())

let graph_timerID = ref (GMain.Timeout.add ~ms:(8 * !G.update_gui_delay) ~callback:(fun _ -> true))

let (graph_width, graph_height) = graph_size

let scale_x = ref 1.
let scale_y = ref 1.

let abs_step = 70
let step = (max graph_width graph_height) / abs_step

let p =
  GDraw.pixmap ~width:graph_width ~height:graph_height ~mask:false
    ~colormap:(Gdk.Color.get_system_colormap ()) ()

let image = GMisc.image ()
let context = image#misc#pango_context
let layout = context#create_layout
let font = Pango.Font.from_string !!O.gtk_graph_font


(*************************************************************************)
(*                                                                       *)
(*                         get_stack                                     *)
(*                                                                       *)
(*************************************************************************)

let get_stack g =
  match !!graph_time with
    GraphQuarter -> g.quarter
  | GraphHour -> g.hour
  | GraphHalfDay -> g.halfday
  | GraphDay -> g.day
  | GraphWeek -> g.week
  | GraphMonth -> g.month
  | GraphYear -> g.year

(*************************************************************************)
(*                                                                       *)
(*                         ref_time                                      *)
(*                                                                       *)
(*************************************************************************)

let ref_time () =
  match !!graph_time with
    GraphQuarter -> Gb.quarter
  | GraphHour -> Gb.hour
  | GraphHalfDay -> Gb.halfday
  | GraphDay -> Gb.day
  | GraphWeek -> Gb.week
  | GraphMonth -> Gb.month
  | GraphYear -> Gb.year

(*************************************************************************)
(*                                                                       *)
(*                         scalex                                        *)
(*                                                                       *)
(*************************************************************************)

let scalex () =
  let t =
    match !!graph_time with
        GraphQuarter -> Gb.quarter
      | GraphHour -> Gb.hour
      | GraphHalfDay -> Gb.halfday
      | GraphDay -> Gb.day
      | GraphWeek -> Gb.week
      | GraphMonth -> Gb.month
      | GraphYear -> Gb.year
  in
  let r = float_of_int graph_width /. t in
  if r = 0.
  then 1.
  else r

(*************************************************************************)
(*                                                                       *)
(*                         scaley                                        *)
(*                                                                       *)
(*************************************************************************)

let max_from t =
  let m = GStack.fold_left (fun accu (_, r) -> max accu r) 0. t in
  max 2. m

let scaley () =
  let max_rate =
    match !!graph_time with
      GraphQuarter -> max_from !graph_data.quarter
    | GraphHour -> max_from !graph_data.hour
    | GraphHalfDay -> max_from !graph_data.halfday
    | GraphDay -> max_from !graph_data.day
    | GraphWeek -> max_from !graph_data.week
    | GraphMonth -> max_from !graph_data.month
    | GraphYear -> max_from !graph_data.year
  in
  let r = float_of_int graph_height /. (max_rate *. 1.2) in
  if r = 0.
  then 1.
  else r

(*************************************************************************)
(*                                                                       *)
(*                         compute_scales                                *)
(*                                                                       *)
(*************************************************************************)

let compute_scales () =
  scale_x := scalex ();
  scale_y := scaley ()


(*************************************************************************)
(*                                                                       *)
(*                         draw_bezier_curve                             *)
(*                                                                       *)
(*************************************************************************)

let draw_quadratic_curve (x1, y1) (x2, y2) (x3, y3) p =
  let fore_col = `NAME !!graph_foreground in
  let light_fore_col = lighten fore_col in
  let nbpts = floor ((x3 -. x1) *. !scale_x) in
  let nbpts_int = int_of_float nbpts in
  if nbpts_int > 0
  then begin
    let f i =
      let v = (nbpts -. i) in
      let v1 = v *. v in
      let v2 = 2. *. i *. v in
      let v3 = i *. i in
      (y1 *. v1 +. y2 *. v2 +. y3 *. v3) /. nbpts /. nbpts
    in
    let x1 = int_of_float (x1 *. !scale_x) in
    for n = 0 to nbpts_int do
      p#set_foreground fore_col;
      let y = !scale_y *. (f (float_of_int n)) in
      let y =
        if vertical_flip
          then int_of_float y
          else graph_height - int_of_float y
      in
      let y0 =
        if vertical_flip
        then 0
        else graph_height
      in
      let x = x1 + n in
      p#line ~x ~y ~x ~y:y0;
      p#set_foreground light_fore_col;
      p#point ~x ~y
    done
  end

(*************************************************************************)
(*                                                                       *)
(*                         normalize_vector                              *)
(*                                                                       *)
(*************************************************************************)

let normalize_vector v x0 =
  let (x, y) = v in
  (x -. x0, y)

(*************************************************************************)
(*                                                                       *)
(*                         draw_curve                                    *)
(*                                                                       *)
(*************************************************************************)

let draw_curve () =
  Pango.Context.set_font_description context#as_context font;
  compute_scales ();
  let grid_col = `NAME !!O.gtk_graph_grid in
  p#set_foreground (`NAME !!O.gtk_graph_background);
  p#rectangle ~x:0 ~y:0 ~width:graph_width ~height:graph_height ~filled:true ();
  p#set_line_attributes ~width:1 ~style:`SOLID ();
  let stack = get_stack !graph_data in
  let ind_opt = GStack.get_index_last stack in
  let x0 = Gb.last_time () -. ref_time () in
  let _ =
    match ind_opt with
      None -> ()
    | Some index ->
        begin
          let count = ref true in
          let l = ref [] in
          while !count do
            let (t, r) = normalize_vector (GStack.get_at_index index stack) x0 in
            l := (t, r) :: !l;
            if (List.length !l) = 3 then begin
              let v1 = List.nth !l 0 in
              let v2 = List.nth !l 1 in
              let v3 = List.nth !l 2 in
              draw_quadratic_curve v1 v2 v3 p;
              l := [v1]
            end;
            count := GStack.iter_next index stack && (t > 0.);
          done
        end
  in
  p#set_foreground grid_col;
  p#set_line_attributes ~width:1 ~style:`ON_OFF_DASH ();
  for i = 0 to step do
    let a = i * abs_step in
    p#line ~x:a ~y:0 ~x:a ~y:graph_height;
    let y = if vertical_flip then a else (graph_height - a - 1) in
    p#line ~x:0 ~y ~x:graph_width ~y;
    if i > 0
      then begin
        let t = Date.to_string (x0 +. (float_of_int a /. !scale_x)) in
        (if !!verbose then lprintf'' "  * time : %s\n" t);
        Pango.Layout.set_text layout t;
        let (offset_x, offset_y) = Pango.Layout.get_pixel_size layout in
        p#put_layout ~x:(a - offset_x / 2) ~y:(graph_height - offset_y * (2 - (i mod 2)))
                     ~fore:grid_col
                     layout;
        let t = Printf.sprintf "%5.1f ko/s" (float_of_int a /. !scale_y) in
        (if !!verbose then lprintf'' "  * rate : %s\n" t);
        Pango.Layout.set_text layout t;
        let (_, offset) = Pango.Layout.get_pixel_size layout in
        let y = if vertical_flip then (a - offset ) else (graph_height - a - offset) in
        p#put_layout ~x:0 ~y
                     ~fore:grid_col
                     layout
      end
  done;
  image#set_pixmap p

(*************************************************************************)
(*                                                                       *)
(*                         reset_graph_timer                             *)
(*                                                                       *)
(*************************************************************************)

let reset_graph_timer () =
  GMain.Timeout.remove (!graph_timerID);
  let s =
    match !!graph_time  with
        GraphQuarter -> Gb.quarter
      | GraphHour -> Gb.hour
      | GraphHalfDay -> Gb.halfday
      | GraphDay -> Gb.day
      | GraphWeek -> Gb.week
      | GraphMonth -> Gb.month
      | GraphYear -> Gb.year
  in
  let ms = int_of_float (1000. *. s /. Gb.max_points) in
  graph_timerID := GMain.Timeout.add ~ms ~callback:
    (fun _ ->
      (if !!verbose then lprintf'' "Update graph every %d ms -- time displayed = %s\n" ms (graphtime_to_label !!graph_time));
      draw_curve ();
      true);
  (if !!verbose then lprintf'' "Update graphs every %d ms -- time displayed = %s\n" ms (graphtime_to_label !!graph_time));
  draw_curve ()

(*************************************************************************)
(*                                                                       *)
(*                         set_graph_data                                *)
(*                                                                       *)
(*************************************************************************)

let set_graph_data g =
  graph_data := g;
  reset_graph_timer ()

(*************************************************************************)
(*                                                                       *)
(*                         initializer                                   *)
(*                                                                       *)
(*************************************************************************)

let _ =
  GMain.Timeout.remove (!graph_timerID);
  ignore (image#misc#connect#destroy ~callback:
    (fun _ ->
       GMain.Timeout.remove (!graph_timerID);
  ))

end)



(*************************************************************************)
(*                                                                       *)
(*                         Module DisplayCurve                           *)
(*                                                                       *)
(*************************************************************************)

module DisplayCurve(D:

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

      val w                 : GObj.widget
      val graph_time        : graph_time option_record
      val reset_graph_timer : unit -> unit
      val title             : string

    end) =
  (struct

  open D

(*************************************************************************)
(*                                                                       *)
(*                         Global variables                              *)
(*                                                                       *)
(*************************************************************************)

let vbox = GPack.vbox ~homogeneous:false ~spacing:6 ()
let hbox =
  GPack.hbox ~homogeneous:false ~spacing:6
    ~packing:(vbox#pack ~expand:true ~fill:true) ()



(*************************************************************************)
(*                                                                       *)
(*                         initializer                                   *)
(*                                                                       *)
(*************************************************************************)

let _ =
  let _label =
    GMisc.label ~text:!M.gT_lb_time_range
      ~packing:(hbox#pack ~expand:false ~fill:true) ()
  in
  let combo =
    GEdit.combo_box ~model:graphtime_store
      ~packing:(hbox#pack ~expand:false ~fill:true) ()
  in
  let str_renderer = GTree.cell_renderer_text [ `XPAD 6 ] in
  combo#pack str_renderer;
  combo#add_attribute str_renderer "text" graphtime_text;
  vbox#add w;
  let markup = GuiTools.create_bold_markup title in
  ignore (GMisc.label ~markup ~packing:(vbox#pack ~expand:true ~fill:true) ());
  let n = ref 0 in
  (try
     let gt = !!graph_time in
     List.iter (fun g ->
       if g = gt
         then assert false
         else incr n
     ) graph_times;
     combo#set_active 0
  with _ -> combo#set_active !n);
  ignore (combo#connect#changed
    (fun _ ->
       match combo#active_iter with
         | None -> ()
         | Some row ->
             begin
               let n = combo#model#get ~row ~column:graphtime_num in
               graph_time =:= (try List.nth graph_times n with _ -> GraphQuarter);
               reset_graph_timer ()
             end
  ))

end)

(*************************************************************************)
(*                                                                       *)
(*                        file_name_from_uid                             *)
(*                                                                       *)
(*************************************************************************)

let file_name_from_uid file_uid =
  try
    Hashtbl.find G.file_by_uid file_uid
  with _ ->
    begin
      GuiGraphBase.cancel_file file_uid;
      raise Not_found
    end

(*************************************************************************)
(*                                                                       *)
(*                         Graph window                                  *)
(*                                                                       *)
(*************************************************************************)

let ni = ref 0

let graph_box gui =
  incr ni;
  let scroll_table_box =
    GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
      ~placement:`TOP_LEFT ()
  in
  let table =
    GPack.table ~columns:2 ~rows:2 ~homogeneous:true
      ~row_spacings:6 ~col_spacings:6 ~border_width:6
      ~packing:scroll_table_box#add_with_viewport ()
  in

  let module MyCurveD = Curve (struct
      let graph_time       = O.gtk_graph_time_downloads
      let graph_foreground = O.gtk_graph_download
      let graph_size       = (g_width, g_height)
      let vertical_flip    = false
      let module_name      = Printf.sprintf "global_downloads_%d" !ni
    end)
  in
  let module MyCurveU = Curve (struct 
      let graph_time       = O.gtk_graph_time_uploads
      let graph_foreground = O.gtk_graph_upload
      let graph_size       = (g_width, g_height)
      let vertical_flip    = false
      let module_name      = Printf.sprintf "global_uploads_%d" !ni
    end)
  in
  let module MyCurveFD = Curve (struct
      let graph_time       = O.gtk_graph_time_file
      let graph_foreground = O.gtk_graph_download
      let graph_size       = (g_width, g_height / 2)
      let vertical_flip    = false
      let module_name      = Printf.sprintf "file_downloads_%d" !ni
    end)
  in
  let module MyCurveFU = Curve (struct 
      let graph_time       = O.gtk_graph_time_file
      let graph_foreground = O.gtk_graph_upload
      let graph_size       = (g_width, g_height / 2)
      let vertical_flip    = true
      let module_name      = Printf.sprintf "file_uploads_%d" !ni
    end)
  in

  let file_data =
    try
      List.assoc !last_file !!Gb.files_graph
    with _ -> 
      match !!Gb.files_graph with
          [] -> (Gb.dummy_graph (), Gb.dummy_graph ())
        | c :: _ -> snd c
  in
  MyCurveD.set_graph_data (fst !!Gb.global_graph);
  MyCurveU.set_graph_data (snd !!Gb.global_graph);
  MyCurveFD.set_graph_data (fst file_data);
  MyCurveFU.set_graph_data (snd file_data);

  let vbox = GPack.vbox ~homogeneous:false () in
  vbox#add MyCurveFD.image#coerce;
  vbox#add MyCurveFU.image#coerce;
  let reset_graph_timer () =
    MyCurveFD.reset_graph_timer ();
    MyCurveFU.reset_graph_timer ()
  in

  let module DisplayMyCurveD = DisplayCurve (struct
      let w                 = MyCurveD.image#coerce
      let graph_time        = O.gtk_graph_time_downloads
      let reset_graph_timer = MyCurveD.reset_graph_timer
      let title             = !M.gT_lb_global_downloads
    end)
  in
  let module DisplayMyCurveU = DisplayCurve (struct
      let w                 = MyCurveU.image#coerce
      let graph_time        = O.gtk_graph_time_uploads
      let reset_graph_timer = MyCurveU.reset_graph_timer
      let title             = !M.gT_lb_global_uploads
    end)
  in
  let module DisplayMyCurveF = DisplayCurve (struct
      let w                 = vbox#coerce
      let graph_time        = O.gtk_graph_time_file
      let reset_graph_timer = reset_graph_timer
      let title             = !M.gT_lb_file_down_up
    end)
  in


  List.iter (fun (left, top, box) ->
    table#attach ~left ~top ~expand:`X ~fill:`X box
  ) [
     0, 0, DisplayMyCurveD.vbox#coerce;
     0, 1, DisplayMyCurveU.vbox#coerce;
     1, 0, DisplayMyCurveF.vbox#coerce;
    ];

  let hbox = DisplayMyCurveF.hbox in
  file_store#clear ();
  List.iter (fun (file_uid, _) ->
    try
      let name = file_name_from_uid file_uid in
      let row = file_store#append () in
      file_store#set ~row ~column:file_id file_uid;
      file_store#set ~row ~column:file_name name;
    with _ -> ()
  ) !!Gb.files_graph;
  let combo =
    GEdit.combo_box ~model:file_store ~width:(g_width * 2 / 3)
      ~packing:(hbox#pack ~expand:false ~fill:true) ()
  in
  let str_renderer = GTree.cell_renderer_text [ `XPAD 6 ] in
  combo#pack str_renderer;
  combo#add_attribute str_renderer "text" file_name;
  let n = ref 0 in
  (try
     List.iter (fun (file_uid, _) ->
       if file_uid = !last_file
         then assert false
         else incr n
     ) !!Gb.files_graph;
     combo#set_active 0
   with _ -> combo#set_active !n);
  ignore (combo#connect#changed
    (fun _ ->
       match combo#active_iter with
           None -> ()
         | Some row ->
             begin
               let file_uid = combo#model#get ~row ~column:file_id in
               last_file := file_uid;
               try
                 let data = List.assoc file_uid !!Gb.files_graph in
                 MyCurveFD.set_graph_data (fst data);
                 MyCurveFU.set_graph_data (snd data);
               with _ -> ()
             end
    ));

  scroll_table_box#coerce


