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

(* The files window of MLgui *)


open GuiTypes2
open GuiTypes
open CommonTypes

open GuiTools
open GuiColumns
open Md4
open GuiProto

module M = GuiMessages
module Mi = GuiMisc
module O = GuiOptions
module G = GuiGlobal
module A = GuiArt
module U = GuiUtf8

let (!!) = Options.(!!)
let (=:=) = Options.(=:=)
let (<:>) = GuiTools.(<:>)

type line =
  Single of (string * string)
| Double of ((string * string) * (string * string))

let (win : GWindow.window option ref) = ref None

let wwidth = ref ((Gdk.Screen.width ()) * 1 / 2)
let wheight = ref ((Gdk.Screen.height ()) * 3 / 4)

(*************************************************************************)
(*                                                                       *)
(*                         attach                                        *)
(*                                                                       *)
(*************************************************************************)

let attach (table : GPack.table) left right top xpadding w =
  table#attach ~left ~top
    ~right ~bottom:(top + 1)
    ~xpadding ~ypadding:0 
    ~expand:`X ~fill:`X
    w#coerce

(*************************************************************************)
(*                                                                       *)
(*                         insert_data                                   *)
(*                                                                       *)
(*************************************************************************)

let insert_data (table : GPack.table) data top =
  let top = ref top in
  List.iter (fun string_list ->
    let len = List.length string_list in
    if len < 5 (* Because table has 4 columns *)
      then begin
        let left = ref 0 in
        let right = ref (if len > 1 then 1 else 4) in
        let xpadding = ref 18 in
        let rec iter list =
          match list with
              [] -> ()
            | s :: tail ->
                begin
                  let markup = GuiTools.create_markup s in
                  let label = GMisc.label ~xalign:0. ~yalign:0. ~markup () in
                  attach table !left !right !top !xpadding label;
                  left := !right;
                  right := 5 - List.length tail; (* Because table has 4 columns *)
                  xpadding := 0;
                  iter tail
                end
        in
        iter string_list;
        incr top;
      end
  ) data;
  (!top + 1)

(*************************************************************************)
(*                                                                       *)
(*                         item_name_to_string                           *)
(*                                                                       *)
(*************************************************************************)

let item_name_to_string item =
  match item with
      File f -> f.g_file_name
    | Source (s, files) -> s.source_name

(*************************************************************************)
(*                                                                       *)
(*                         pixbuf_of_item                                *)
(*                                                                       *)
(*************************************************************************)

let pixbuf_of_item item =
  match item with
      File f -> Mi.file_type_of_name f.g_file_name ~size:A.LARGE
    | Source (s, files) -> Mi.source_type_to_icon s.source_type ~size:A.LARGE

(*************************************************************************)
(*                                                                       *)
(*                         format_to_string                              *)
(*                                                                       *)
(*************************************************************************)

let format_to_string (table : GPack.table) format =
  let m = Mi.format_to_string format in
  let context = table#misc#pango_context#as_context in
  let pixels = !wwidth * 3 / 4 in
  ConfigWindow.message_to_stem m context pixels

(*************************************************************************)
(*                                                                       *)
(*                         item_info_of_item                             *)
(*                                                                       *)
(*************************************************************************)

let item_info_of_item (table : GPack.table) item =
  match item with
      File f ->
        let avail = Mi.main_availability_of f.g_file_network f.g_file_availability in
        [
         [!M.fW_lb_file_format; format_to_string table f.g_file_format];
         [!M.fW_lb_file_hash;   Mi.uid_list_to_string f.g_file_uids];
         [!M.fW_lb_file_size;   Mi.size_of_int64 f.g_file_size;
          !M.fW_lb_file_state;  Mi.string_of_file_state f.g_file_state f.g_file_download_rate];
         [!M.fW_lb_file_chunks; Mi.chunks_to_string f.g_file_chunks;
          !M.fW_lb_file_avail;  Mi.string_of_availability avail f.g_file_chunks];
         [!M.fW_lb_file_age;    Mi.time_to_string f.g_file_age;
          !M.fW_lb_file_prio;   Mi.priority_to_string f.g_file_priority]
        ]

    | Source (s, files) ->
        [
         [!M.fW_lb_source_hash;      Mi.hash_of_client s.source_kind];
         [!M.fW_lb_source_kind;      Mi.location_kind_to_string s.source_kind;
          !M.fW_lb_source_chat_port; string_of_int s.source_chat_port];
         [!M.fW_lb_source_software;  s.source_software];
        ]

(*************************************************************************)
(*                                                                       *)
(*                         transfer_info_of_item                         *)
(*                                                                       *)
(*************************************************************************)

let transfer_info_of_item item (table : GPack.table) =
  match item with
      File f ->
        [
         [!M.fW_lb_file_sources;         Mi.sources_to_string f.g_file_sources;
          !M.fW_lb_file_rate;            Mi.average_rate f.g_file_downloaded f.g_file_age];
         [!M.fW_lb_file_dled;            Mi.size_of_int64 f.g_file_downloaded;
          !M.fW_lb_file_complete_chunks; Mi.completed_chunks_to_string f.g_file_chunks];
         [!M.fW_lb_last_seen;            Mi.time_to_string f.g_file_last_seen;
          !M.fW_lb_file_eta;            (Mi.calc_eta_inst f.g_file_size f.g_file_downloaded f.g_file_download_rate) ^ " / " ^
                                        (Mi.calc_eta_average f.g_file_size f.g_file_downloaded f.g_file_age)]
        ]

    | Source (s, files) ->
        begin
          let context = table#misc#pango_context in
          let pixels = !wwidth * 1 / 2 in
          let upload = GuiTools.fit_string_to_pixels (Mi.upload_to_string s.source_upload) ~context ~pixels in
          [
           [!M.fW_lb_source_dlding;        upload];
           [!M.fW_lb_source_rating;        string_of_int s.source_rating;
            !M.fW_lb_source_connect_time;  Mi.time_to_string s.source_connect_time];
           [!M.fW_lb_source_dled;          Mi.size_of_int64 s.source_downloaded;
            !M.fW_lb_source_upled;         Mi.size_of_int64 s.source_uploaded];
           [!M.fW_lb_source_average_dled;  Mi.average_rate s.source_downloaded s.source_connect_time;
            !M.fW_lb_source_average_upled; Mi.average_rate s.source_uploaded s.source_connect_time];
          ]
        end

(*************************************************************************)
(*                                                                       *)
(*                         add_item                                      *)
(*                                                                       *)
(*************************************************************************)

let add_item (table : GPack.table) item top =
  let s =  item_name_to_string item in
  let pixb = pixbuf_of_item item in
  let hbox = GPack.hbox ~homogeneous:false ~spacing:12 () in
  let _ =
    match pixb with
        None -> ()
      | Some pixbuf -> ignore (GMisc.image ~pixbuf ~packing:(hbox#pack ~expand:false ~fill:true) ())
  in
  let markup = GuiTools.create_bold_markup s in
  let vbox =
    GPack.hbox ~homogeneous:false
      ~packing:(hbox#pack ~expand:true ~fill:true) ()
  in
  let _label =
    GMisc.label ~xalign:0. ~yalign:0. ~markup ~line_wrap:true
      ~packing:(vbox#pack ~expand:true ~fill:true) ()
  in
  table#attach ~left:0 ~top
    ~right:4 ~bottom:(top + 1)
    ~xpadding:0 ~ypadding:0 
    ~expand:`X ~fill:`X
    hbox#coerce;
  (top + 1)

(*************************************************************************)
(*                                                                       *)
(*                         add_group                                     *)
(*                                                                       *)
(*************************************************************************)

let add_group (table : GPack.table) s top =
  let hbox = GPack.hbox ~homogeneous:false () in
  let markup = GuiTools.create_bold_markup s in
  let _label =
    GMisc.label ~xalign:0. ~markup
      ~packing:(hbox#pack ~expand:false ~fill:true) ()
  in
  table#attach ~left:0 ~top
    ~right:4 ~bottom:(top + 1)
    ~xpadding:6 ~ypadding:0 
    ~expand:`X ~fill:`X
    hbox#coerce;
  (top + 1)

(*************************************************************************)
(*                                                                       *)
(*                         add_item_info                                 *)
(*                                                                       *)
(*************************************************************************)

let add_item_info (table : GPack.table) item top =
  let list = item_info_of_item table item in
  insert_data table list top

(*************************************************************************)
(*                                                                       *)
(*                         add_transfer_info                             *)
(*                                                                       *)
(*************************************************************************)

let add_transfer_info (table : GPack.table) item top =
  let list = transfer_info_of_item item table in
  insert_data table list top

(*************************************************************************)
(*                                                                       *)
(*                         add_avail_info                                *)
(*                                                                       *)
(*************************************************************************)

let add_avail_info (table : GPack.table) f top =
  let top = ref top in
  let width = !wwidth * 1 / 3 in
  let height = 16 in
  List.iter (fun (net_num, avail) ->
    let net_pixb = Mi.network_pixb net_num ~size:A.SMALL () in
    let avail_bar = Mi.availability_bar avail f.g_file_chunks true in
    let _ =
      match (net_pixb, avail_bar) with
          (Some pb, Some pixb) ->
            begin
              let img = GMisc.image ~pixbuf:pb () in
              table#attach ~left:0 ~top:!top
                ~right:1 ~bottom:(!top + 1)
                ~xpadding:0 ~ypadding:0 
                ~expand:`X ~fill:`X
                img#coerce;
              let w = GdkPixbuf.get_width pixb in
              let pixbuf = GdkPixbuf.create ~width ~height ~has_alpha:true () in
              let scale_x = (float_of_int width) /. (float_of_int w) in
              GdkPixbuf.scale
                ~dest:pixbuf ~dest_x:0 ~dest_y:0
                ~width ~height ~scale_x ~interp:`TILES
                pixb;
              let img = GMisc.image ~pixbuf () in
              table#attach ~left:1 ~top:!top
                ~right:3 ~bottom:(!top + 1)
                ~xpadding:0 ~ypadding:0 
                ~expand:`X ~fill:`X
                img#coerce
            end
      | _ -> ()
    in
    incr top
  ) f.g_file_availability;
  (!top + 1)

(*************************************************************************)
(*                                                                       *)
(*                         add_file_names                                *)
(*                                                                       *)
(*************************************************************************)

let sort_function column (model : #GTree.model) it_a it_b =
  let a = model#get ~row:it_a ~column in
  let b = model#get ~row:it_b ~column in
  compare a b

let add_file_names (table : GPack.table) f top =
  let height = !wheight * 1 / 4 in
  let file_name = U.utf8_of f.g_file_name in
  let cols = new GTree.column_list in
  let file_names = cols#add Gobject.Data.string in
  let file_names_pixb = cols#add Gobject.Data.gobject_option in
  let liststore = GTree.list_store cols in
  let listmodel = GTree.model_sort liststore in
  listmodel#set_sort_func 0 (sort_function file_names);
  List.iter (fun name ->
    let s = U.utf8_of name in
    let pixb = (Mi.file_type_of_name name ~size:A.SMALL) in
    let row = liststore#append () in
    liststore#set ~row ~column:file_names_pixb pixb;
    liststore#set ~row ~column:file_names s;
  ) f.g_file_names;
  let col_name =
    let col =  GTree.view_column ~title:!M.fW_lb_file_names_col () in
    col#set_resizable true;
    col#set_clickable true;
    col#set_sizing `FIXED;
    col#set_fixed_width (!wwidth * 5 / 8);
    let renderer = GTree.cell_renderer_pixbuf [`XALIGN 0.;`XPAD 4] in
    col#pack renderer;
    col#add_attribute renderer "pixbuf" file_names_pixb;
    let renderer = GTree.cell_renderer_text [`XALIGN 0.] in
    col#pack renderer;
    col#add_attribute renderer "text" file_names;
    col#set_sort_column_id 0;
    col
  in
  let vbox = GPack.vbox ~homogeneous:false ~spacing:6 () in
  let scrolled_box =
    GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
      ~placement:`TOP_LEFT ~shadow_type:`OUT
      ~packing:(vbox#pack ~expand:true ~fill:true) ()
  in
  let view = GTree.view ~model:listmodel ~height ~packing:scrolled_box#add () in
  ignore (view#append_column col_name);
  let hbox =
    GPack.hbox ~homogeneous:false ~spacing:6
      ~packing:(vbox#pack ~expand:true ~fill:true) ()
  in
  let entry =
    GEdit.entry ~text:file_name ~editable:true ~visibility:true
      ~packing:(hbox#pack ~expand:true ~fill:true) ()
  in
  let button =
    GButton.button ~label:!M.fW_lb_rename ~use_mnemonic:true
      ~packing:(hbox#pack ~expand:false ~fill:true) ()
  in
  ignore (button#connect#clicked ~callback:
    (fun _ ->
      if entry#text <> "" then
        match f.g_file_state with
            FileDownloaded  -> GuiCom.send (GuiProto.SaveFile (f.g_file_num, entry#text))
          | _  ->  GuiCom.send (GuiProto.RenameFile (f.g_file_num, entry#text))
  ));
  ignore (view#selection#connect#after#changed ~callback:
    (fun _ ->
      let path_list = view#selection#get_selected_rows in
      match path_list with
          path :: tail ->
            begin
              let p = listmodel#convert_path_to_child_path path in
              let row = liststore#get_iter p in
              let name = liststore#get ~row ~column:file_names in
              entry#set_text name
            end
        | _ -> ()
  ));
  table#attach ~left:0 ~top
    ~right:4 ~bottom:(top + 1)
    ~xpadding:18 ~ypadding:0 
    ~expand:`X ~fill:`X
    vbox#coerce;
  (top + 1)

(*************************************************************************)
(*                                                                       *)
(*                         add_files_requested                           *)
(*                                                                       *)
(*************************************************************************)

let add_files_requested (table : GPack.table) s top =
  let context = table#misc#pango_context in
  let pixels = !wwidth * 3 / 4 in
  let _list = ref [] in
  match s.source_files_requested with
      [] -> insert_data table [] top
    | files ->
        begin
          let file_list = G.get_files files in
          let l = ref [] in
          List.iter (fun f ->
            l := [GuiTools.fit_string_to_pixels (U.utf8_of f.g_file_name) ~context ~pixels] :: !l
          ) file_list;
          insert_data table !l top
        end

(*************************************************************************)
(*                                                                       *)
(*                         insert_item_details                           *)
(*                                                                       *)
(*************************************************************************)

let insert_item_details (table : GPack.table) item =
  match item with
      File f ->
        begin
          let v_pos = add_item table item 0 in
          let v_pos = add_group table !M.fW_lb_file_info v_pos in
          let v_pos = add_item_info table item v_pos in
          let v_pos = add_group table !M.fW_lb_transfer_info v_pos in
          let v_pos = add_transfer_info table item v_pos in
          let v_pos = add_group table !M.fW_lb_avail_info v_pos in
          let v_pos = add_avail_info table f v_pos in
          let v_pos = add_group table !M.fW_lb_file_names v_pos in
          let _v_pos = add_file_names table f v_pos in
          ()
        end
  | Source (s, _) ->
        begin
          let v_pos = add_item table item 0 in
          let v_pos = add_group table !M.fW_lb_source_info v_pos in
          let v_pos = add_item_info table item v_pos in
          let v_pos = add_group table !M.fW_lb_transfer_info v_pos in
          let v_pos = add_transfer_info table item v_pos in
          let v_pos = add_group table !M.fW_lb_files_requested v_pos in
          let _v_pos = add_files_requested table s v_pos in
          ()
        end


(*************************************************************************)
(*                                                                       *)
(*                         create_window                                 *)
(*                                                                       *)
(*************************************************************************)

let create_window item =
  let height =
    match item with
        File _ -> !wheight
      | Source _ -> !wheight * 2 / 3
  in
  let title =
    match item with
        File _ -> !M.fW_wt_show_file_details
      | Source _ -> !M.fW_wt_show_source_details
  in
  let window =
    GWindow.window ~width:!wwidth ~height
      ~title
      ~icon:(A.get_icon ~icon:M.icon_stock_info ~size:A.SMALL ())
      ~position:`CENTER_ALWAYS
      ~kind:`TOPLEVEL
      ~allow_grow:false ~allow_shrink:false
      ~resizable:false ~modal:false ()
  in
  window#set_skip_taskbar_hint false;
  window#set_skip_pager_hint false;
  ignore (window#connect#destroy ~callback:
    (fun _ ->
       win := None
  ));
  let vbox =
    GPack.vbox ~homogeneous:false
      ~packing:window#add ()
  in
  let scroll_table_box =
    GBin.scrolled_window ~hpolicy:`NEVER ~vpolicy:`AUTOMATIC
      ~placement:`TOP_LEFT ~shadow_type:`NONE
      ~packing:(vbox#pack ~expand:true ~fill:true) ()
  in
  let table =
    GPack.table ~columns:4 ~homogeneous:false
       ~row_spacings:6 ~col_spacings:6 ~border_width:6
       ~packing:scroll_table_box#add_with_viewport ()
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
    tool_bar `HORIZONTAL ~border_width:6 ~spacing:6
      ~layout:`END ~packing:hbox_button#add ()
  in
  ignore (bbox#add_button
    ~markup:!M.pW_lb_close
    ~style:`BOTH_HORIZ
    ~icon:(A.get_icon ~icon:M.icon_stock_close ~size:A.SMALL ())
    ~f:(window#destroy) ()
  );
  insert_item_details table item;
  win := Some window;
  window#show ()

(*************************************************************************)
(*                                                                       *)
(*                         window                                        *)
(*                                                                       *)
(*************************************************************************)

let window (item : GuiTypes2.item_info) () =
  match !win with
      Some w ->
        w#present ()
    | None ->
        create_window item

