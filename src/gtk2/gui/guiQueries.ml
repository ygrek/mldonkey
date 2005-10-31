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

(* The queries window of MLgui *)

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

let verbose = O.gtk_verbose_queries

let lprintf' fmt =
  Printf2.lprintf ("GuiQueries: " ^^ fmt)

(*************************************************************************)
(*                                                                       *)
(*                         Templates                                     *)
(*                                                                       *)
(*************************************************************************)

let (view_context : GPango.context option ref) = ref None

module QueriesResults = GuiResults.ResultList(struct

  let columns = O.results_columns
  let view_context = view_context
  let module_name = "QueriesResults"

end)

(*************************************************************************)
(*                                                                       *)
(*                         Types                                         *)
(*                                                                       *)
(*************************************************************************)

type g_query =
  {
    g_query_num : int;
    g_query_desc : string;
    g_query_result : QueriesResults.g_result;
    mutable g_query_waiting : int option;
    mutable g_query_label : GMisc.label option;
    mutable g_query_waiting_label : GMisc.label option;
  }

(*************************************************************************)
(*                                                                       *)
(*                         Global tables                                 *)
(*                                                                       *)
(*************************************************************************)

let (qresults : (int, g_query) Hashtbl.t) = Hashtbl.create 13

let (query_entries : (int * CommonTypes.query_entry) list ref) = ref []

let string_to_label_list =
  [
   ("Complex Search", !M.qT_lb_complex_searches );
   ("MP3 Search", !M.qT_lb_mp3_searches );
   ("Movie Search", !M.qT_lb_movie_searches );
   ("Album Search", !M.qT_lb_album_searches );
   ("And Not", !M.qT_lb_and_not );
   ("Audio", !M.qT_tx_audio );
   ("Video", !M.qT_tx_video );
   ("Program", !M.qT_tx_program );
   ("Image", !M.qT_tx_image );
   ("Documentation", !M.qT_tx_documentation );
   ("Collection", !M.qT_tx_collection );
   ("Keywords", !M.qT_lb_keywords );
   ("Media", !M.qT_lb_media );
   ("Format", !M.qT_lb_format );
   ("Min size", !M.qT_lb_min_size );
   ("Max size", !M.qT_lb_max_size );
   ("Min Bitrate", !M.qT_lb_min_bitrate );
   ("Title", !M.qT_lb_title );
   ("Number of results", !M.qT_lb_number_of_results );
   ("Sort by", !M.qT_lb_sort_by );
   ("Album", !M.qT_lb_album );
   ("Fields", !M.qT_lb_fields );
   ("Artist", !M.qT_lb_artist );
   ("Track/Title", !M.qT_lb_track_title );
   ("Track", !M.qT_lb_track );
   ("Rest", !M.qT_lb_rest );
   ("Categories", !M.qT_lb_categories );
   ("All", !M.qT_lb_all );
   ("Blues", !M.qT_lb_blues );
   ("Classical", !M.qT_lb_classical );
   ("Data", !M.qT_lb_data );
   ("Folk", !M.qT_lb_folk );
   ("Rock", !M.qT_lb_rock );
   ("Soundtrack", !M.qT_lb_soundtrack );
   ("Availability", !M.qT_lb_availability );
   ("Size", !M.qT_lb_size );
 ]

let label_to_string_list = List.map (fun (l, t) -> (t, l)) string_to_label_list

(*************************************************************************)
(*                                                                       *)
(*                         Global variables                              *)
(*                                                                       *)
(*************************************************************************)

let (wnote_results : GPack.notebook option ref) = ref None
let (current_form : GuiTypes2.query_form option ref) = ref None

let current_net = ref 0
let current_max_hits = ref 500
let show_hidden_fields = ref false
let current_query_entry = ref 0

let query_cols = new GTree.column_list
let query_num = query_cols#add Gobject.Data.int
let query_text = query_cols#add Gobject.Data.string
let query_icon = query_cols#add Gobject.Data.gobject_option
let query_store = GTree.list_store query_cols
let query_model = GTree.model_sort query_store

let net_cols = new GTree.column_list
let net_num = net_cols#add Gobject.Data.int
let net_text = net_cols#add Gobject.Data.string
let net_icon = net_cols#add Gobject.Data.gobject_option
let net_store = GTree.list_store net_cols
let net_model = GTree.model_sort net_store

let history_cols = new GTree.column_list
let history_text = history_cols#add Gobject.Data.string
let history_store = GTree.list_store history_cols
let history_model = GTree.model_sort history_store

let _ =
  query_model#set_default_sort_func
    (fun model iter_a iter_b ->
       let a = model#get ~row:iter_a ~column:query_text in
       let b = model#get ~row:iter_b ~column:query_text in
       compare a b
  );
  net_model#set_default_sort_func
    (fun model iter_a iter_b ->
       let a = model#get ~row:iter_a ~column:net_text in
       let b = model#get ~row:iter_b ~column:net_text in
       compare a b
  );
  history_model#set_default_sort_func
    (fun model iter_a iter_b ->
       let a = model#get ~row:iter_a ~column:history_text in
       let b = model#get ~row:iter_b ~column:history_text in
       compare a b
  )

(*************************************************************************)
(*                                                                       *)
(*                         message to the core                           *)
(*                                                                       *)
(*************************************************************************)

let extend_query query_num =
  GuiCom.send (GuiProto.ExtendedSearch (query_num, ExtendSearchRemotely))

let close_query query_num (forget : bool) =
  (if !!verbose then lprintf' "Close query %d %b\n" query_num forget);
  flush stdout;
  GuiCom.send (CloseSearch (query_num, forget))

let send_search (s : CommonTypes.query_entry GuiTypes.search_request) =
  GuiCom.send (Search_query s)

(*************************************************************************)
(*                                                                       *)
(*                         string_to_label                               *)
(*                                                                       *)
(*************************************************************************)

let string_to_label s =
  try
    List.assoc s string_to_label_list
  with Not_found -> U.utf8_of s

(*************************************************************************)
(*                                                                       *)
(*                         label_to_string                               *)
(*                                                                       *)
(*************************************************************************)

let label_to_string label = 
  try
    List.assoc label label_to_string_list
  with Not_found -> label

(*************************************************************************)
(*                                                                       *)
(*                         query_icon_of_string                          *)
(*                                                                       *)
(*************************************************************************)

let album_reg = Str.regexp_case_fold ".*album.*"
let mp3_reg = Str.regexp_case_fold ".*mp3.*"
let movie_reg = Str.regexp_case_fold ".*movie.*"

let query_icon_of_string s =
  if !!O.gtk_look_use_icons
    then begin
      (if !!verbose then lprintf' "LABEL: [%s]\n" s);
      let label = String.lowercase s in
      if Str.string_match album_reg label 0 
        then Some (A.get_icon ~icon:M.icon_menu_search_album ~size:A.SMALL ())
        else if Str.string_match movie_reg label 0 
          then Some (A.get_icon ~icon:M.icon_menu_search_movie ~size:A.SMALL ())
          else if Str.string_match mp3_reg label 0 
            then Some (A.get_icon ~icon:M.icon_menu_search_mp3 ~size:A.SMALL ())
            else Some (A.get_icon ~icon:M.icon_menu_search_complex ~size:A.SMALL ())
    end else None

(*************************************************************************)
(*                                                                       *)
(*                         build_query_type_menu                         *)
(*                                                                       *)
(*************************************************************************)

let build_query_type_menu s num =
  let row = query_store#append () in
  query_store#set ~row ~column:query_num num;
  query_store#set ~row ~column:query_text (string_to_label s);
  query_store#set ~row ~column:query_icon (query_icon_of_string s)

(*************************************************************************)
(*                                                                       *)
(*                         build_net_menu                                *)
(*                                                                       *)
(*************************************************************************)

let all_networks_icon () =
  if !!O.gtk_look_use_icons
    then Some (A.get_icon ~icon:M.icon_net_globalshare ~size:A.SMALL ())
    else None

let build_net_menu () =
  let row = net_store#append () in
  net_store#set ~row ~column:net_num 0;
  net_store#set ~row ~column:net_text !M.qT_lb_all_networks;
  net_store#set ~row ~column:net_icon (all_networks_icon ());
  Hashtbl.iter (fun num net ->
    if net.net_enabled && (Mi.net_has_search net)
      then begin
        let row = net_store#append () in
        net_store#set ~row ~column:net_num net.net_num;
        net_store#set ~row ~column:net_text (U.simple_utf8_of net.net_name);
        net_store#set ~row ~column:net_icon (Mi.network_pixb net.net_num ~size:A.SMALL ())
      end
  ) G.networks

(*************************************************************************)
(*                                                                       *)
(*                         fill_queries_history                          *)
(*                                                                       *)
(*************************************************************************)

let fill_queries_history s =
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
(*                         display_results                               *)
(*                                                                       *)
(*************************************************************************)

let display_results (notebook : GPack.notebook) (qr : g_query) =
  let main_evbox = GBin.event_box () in
  let vbox =
    GPack.vbox ~homogeneous:false
      ~packing:main_evbox#add ()
  in
  let resultview =
    QueriesResults.R.treeview ~mode:`MULTIPLE
      ~packing:(vbox#pack ~expand:true ~fill:true) ()
  in
  view_context := Some resultview#view#misc#pango_context;
  resultview#set_model qr.g_query_result#gmodel;
  resultview#set_menu GuiResults.result_menu;
  let hbox_b =
    GPack.hbox ~homogeneous:false
      ~packing:(vbox#pack ~fill:true ~expand:false) ()
  in
  let query_status =
    GMisc.label ~xalign:0. ~yalign:0.5 ~xpad:3 ~ypad:3 
      ~packing:(hbox_b#pack ~expand:false ~fill:true) ()
  in
  let _ =
    match qr.g_query_waiting with
        None -> ()
      | Some n ->
          begin
            let text = Printf.sprintf "%s %d" !M.qT_lb_waiting_for_replies n in
            query_status#set_text text
          end
  in
  let bbox = 
    GuiTools.tool_bar `HORIZONTAL ~border_width:6 ~spacing:6
      ~layout:`END ~packing:(hbox_b#pack ~fill:true ~expand:true) ()
  in
  ignore (bbox#add_button
    ~markup:!M.qT_lb_extend_search
    ~style:`BOTH_HORIZ
    ~icon:(A.get_icon ~icon:M.icon_stock_extend_search ~size:A.SMALL ())
    ~f:(fun _ -> extend_query qr.g_query_num) ()
  );
  ignore (bbox#add_button
    ~markup:!M.qT_lb_stop_search
    ~style:`BOTH_HORIZ
    ~icon:(A.get_icon ~icon:M.icon_stock_stop ~size:A.SMALL ())
    ~f:(fun _ -> close_query qr.g_query_num false) ()
  );

  let text = Printf.sprintf "%s (%d)" qr.g_query_desc qr.g_query_result#nitems in
  let hbox =
    GPack.hbox ~homogeneous:false ~spacing:3 () in
  let label =
    GMisc.label ~xalign:0. ~yalign:0.5 ~text
      ~packing:(hbox#pack ~expand:true ~fill:true) ()
  in
  let evbox = GBin.event_box ~packing:(hbox#pack ~expand:false ~fill:false) () in
  let pixbuf = A.get_icon ~icon:M.icon_stock_close_overlay ~size:A.SMALL () in
  let img = GMisc.image ~pixbuf ~xalign:1.0 ~yalign:0. ~packing:evbox#add () in
  evbox#misc#set_state `ACTIVE;
  qr.g_query_label <- Some label;
  qr.g_query_waiting_label <- Some query_status;
  let on_closure () =
    let results = GuiResults.keys_to_results (qr.g_query_result#all_items ()) in
    List.iter (fun r ->
      Hashtbl.remove G.results r.res_num;
    ) results;
    close_query qr.g_query_num true;
    qr.g_query_result#clear ();
    main_evbox#destroy ();
    Hashtbl.remove qresults qr.g_query_num;
  in
  GuiTools.add_key ~key:GdkKeysyms._w ~target:main_evbox ~f:on_closure ~mods:[`CONTROL] ();
  ignore (evbox#event#connect#button_press ~callback:
    (fun ev ->
      GdkEvent.get_type ev = `BUTTON_PRESS &&
      GdkEvent.Button.button ev = 1 &&
      (
       on_closure ();
       true
      )
  ));
  notebook#append_page ~tab_label:hbox#coerce main_evbox#coerce

(*************************************************************************)
(*                                                                       *)
(*                         entry_leaf                                    *)
(*                                                                       *)
(*************************************************************************)

let entry_leaf qf we =
  match qf with
  | QF_KEYWORDS _ -> Q_KEYWORDS ("", we#text)
  | QF_MP3_ARTIST _ -> Q_MP3_ARTIST ("", we#text)
  | QF_MP3_TITLE  _ -> Q_MP3_TITLE ("", we#text)
  | QF_MP3_ALBUM  _ -> Q_MP3_ALBUM ("", we#text)
  | _ -> assert false

(*************************************************************************)
(*                                                                       *)
(*                         entry_of_form                                 *)
(*                                                                       *)
(*************************************************************************)

let rec entry_of_form qf =
  match qf with
    QF_AND l ->
      Q_AND (List.map entry_of_form l)
  | QF_OR l -> 
      Q_OR (List.map entry_of_form l)
  | QF_ANDNOT (qf1, qf2) ->
      Q_ANDNOT (entry_of_form qf1, entry_of_form qf2) 
  | QF_MODULE qf ->
      Q_MODULE ("", entry_of_form qf)

  | QF_MINSIZE (we,we2) -> 
      let size = 
        try 
          Int32.to_string
            (Int32.mul (Int32.of_string we#text) (Mi.unit_of_string we2#text))
        with _ -> ""
      in
      Q_MINSIZE ("", size)
  | QF_MAXSIZE (we,we2) -> 
      let size = 
        try Int32.to_string
            (Int32.mul (Int32.of_string we#text) (Mi.unit_of_string we2#text))
        with _ -> ""
      in
      Q_MAXSIZE ("", size)
      
  | QF_COMBO s -> Q_COMBO ("", label_to_string !s, [])
      
  | QF_FORMAT we -> Q_FORMAT ("", we#text)
  | QF_MEDIA we -> Q_MEDIA ("", label_to_string we#text)
  | QF_MP3_BITRATE  we -> Q_MP3_BITRATE ("", we#text)

  | QF_KEYWORDS we
  | QF_MP3_ARTIST we
  | QF_MP3_TITLE we
  | QF_MP3_ALBUM we -> (fill_queries_history we#text; entry_leaf qf we)
  
  | QF_HIDDEN l ->
      Q_HIDDEN (List.map entry_of_form l)

(*************************************************************************)
(*                                                                       *)
(*                         submit_search                                 *)
(*                                                                       *)
(*************************************************************************)

let submit_search (s : CommonTypes.query_entry GuiTypes.search_request) =
  send_search s;
  let desc = GuiMisc.description_of_query s.GuiTypes.search_query in
  let result = new QueriesResults.g_result () in
  result#set_filter GuiResults.filter_result;
  let qr =
    {
      g_query_num = s.GuiTypes.search_num;
      g_query_desc = desc;
      g_query_result = result;
      g_query_waiting = None;
      g_query_label = None;
      g_query_waiting_label = None;
    }
  in
  Hashtbl.add qresults qr.g_query_num qr;
  match !wnote_results with
      Some w ->
        display_results w qr
    | _ -> ()

(*************************************************************************)
(*                                                                       *)
(*                         submit                                        *)
(*                                                                       *)
(*************************************************************************)

let submit ?(local=RemoteSearch) () =
  match !current_form with
      None -> ()
    | Some form ->
        begin
          let qe = entry_of_form form in
          let search = Mi.create_search qe !current_max_hits !current_net local in
          submit_search search
        end

(*************************************************************************)
(*                                                                       *)
(*                         attach                                        *)
(*                                                                       *)
(*************************************************************************)

let attach (table : GPack.table) top l =
  List.iter (fun (w, left, right) ->
    table#attach ~left ~top
      ~xpadding:0 ~ypadding:0
      ~right ~bottom:(top + 1)
      ~expand:`X ~fill:`X
      w
  ) l;
  (top +1)

(*************************************************************************)
(*                                                                       *)
(*                         make_separator                                *)
(*                                                                       *)
(*************************************************************************)

let make_separator ?text () =
  match text with
      None ->
        begin
          let sep = GMisc.separator `HORIZONTAL () in
          sep#coerce
        end
    | Some s ->
        begin
          let hbox = GPack.hbox ~homogeneous:false ~spacing:3 () in
          let markup = GuiTools.create_bold_markup s in
          let label = 
            GMisc.label ~markup ~xalign:0. ~yalign:0.5
              ~packing:(hbox#pack ~expand:false ~fill:true) ()
          in
          let sep =
            GMisc.separator `HORIZONTAL
              ~packing:(hbox#pack ~expand:true ~fill:true) ()
          in
          hbox#coerce
        end

(*************************************************************************)
(*                                                                       *)
(*                         combo_from_value                              *)
(*                                                                       *)
(*************************************************************************)

let combo_from_value s v args =
  let return = ref "" in
  let strings =
    List.sort (fun s1 s2 ->
      if s1 = v || s2 = v
        then (-1)
        else compare s1 s2
    ) args
  in
  let markup = string_to_label s in
  let wl = GMisc.label ~markup ~xalign:0. ~yalign:0.5 () in
  let (combo, (_, column)) = GEdit.combo_box_text ~strings ~width:90 () in
  ignore (combo#connect#changed
    (fun _ ->
      match combo#active_iter with
      | None -> ()
      | Some row -> return := combo#model#get ~row ~column
  ));
  combo#set_active 0;
  (wl, combo, return)

(*************************************************************************)
(*                                                                       *)
(*                         combo_entry_from_value                        *)
(*                                                                       *)
(*************************************************************************)

let combo_entry_from_value s v args =
  let markup = string_to_label s in
  let wl = GMisc.label ~markup ~xalign:0. ~yalign:0.5 () in
  let model, text_column = 
    GTree.store_of_list Gobject.Data.string (v :: args)
  in
  let combo = GEdit.combo_box_entry ~text_column ~model ~width:90 () in
  combo#set_active 0;
  (wl, combo)

(*************************************************************************)
(*                                                                       *)
(*                         double_combo_from_value                       *)
(*                                                                       *)
(*************************************************************************)

let double_combo_from_value s text args =
  let markup = string_to_label s in
  let wl = GMisc.label ~markup ~xalign:0. ~yalign:0.5 () in
  let model, text_column = 
    GTree.store_of_list Gobject.Data.string args
  in
  let combo = GEdit.combo_box_entry ~text_column ~model ~width:40 () in
  combo#set_active 0;
  let entry = GEdit.entry ~text ~editable:true ~has_frame:true ~width:90 () in
  (wl, entry, combo)

(*************************************************************************)
(*                                                                       *)
(*                         form_leaf                                     *)
(*                                                                       *)
(*************************************************************************)

let form_leaf qe we =
  match qe with
  | Q_KEYWORDS _ -> QF_KEYWORDS we
  | Q_MP3_ARTIST _ -> QF_MP3_ARTIST we
  | Q_MP3_TITLE  _ -> QF_MP3_TITLE we
  | Q_MP3_ALBUM  _ -> QF_MP3_ALBUM we
  | _ -> assert false

(*************************************************************************)
(*                                                                       *)
(*                         form_of_entry                                 *)
(*                                                                       *)
(*************************************************************************)

let rec form_of_entry (table : GPack.table) qe top =
  match qe with
    Q_AND le ->
      let rec iter l = 
        match l with
          [] -> []

        | qe :: tail ->
            let form = form_of_entry table qe top in
            form :: (iter tail)
      in
      QF_AND (iter le)

  | Q_OR le ->
      let sep = make_separator ~text:!M.qT_lb_or () in
      top := attach table !top [(sep#coerce,0,3)];
      let l = List.map (fun qe -> form_of_entry table qe top) le in
      QF_OR l

  | Q_ANDNOT (qe1, qe2) ->
      let sep1 = make_separator () in
      top := attach table !top [(sep1#coerce,0,3)];
      let form1 = form_of_entry table qe1 top in
      let sep2 = make_separator ~text:!M.qT_lb_and_not () in
      top := attach table !top [(sep2#coerce,0,3)];
      let form2 = form_of_entry table qe2 top in
      QF_ANDNOT (form1, form2)

  | Q_MODULE (s, qe) ->
      let sep = make_separator () in
      top := attach table !top [(sep,0,3)];
      let markup = GuiTools.create_bold_markup (string_to_label s) in
      let label = GMisc.label ~markup ~xalign:0. ~yalign:0.5 () in
      top := attach table !top [(label#coerce,0,3)];
      let form = form_of_entry table qe top in
      QF_MODULE form

  | Q_FORMAT (s, v) ->
      let args = ["avi";"mpg";"ogm";"qt";"mov"; "mp3"; "ogg"; "pcm"; "wav"] in
      let (label, combo) = combo_entry_from_value s v args in
      top := attach table !top [(label#coerce,0,1); (combo#coerce,1,3)];
      QF_FORMAT combo#entry

  | Q_MEDIA (s, v) ->
      let args =
        [
          !M.qT_tx_audio;
          !M.qT_tx_video;
          !M.qT_tx_program;
          !M.qT_tx_image;
          !M.qT_tx_documentation;
          !M.qT_tx_collection
        ]
      in
      let (label, combo) = combo_entry_from_value s v args in
      top := attach table !top [(label#coerce,0,1); (combo#coerce,1,3)];
      QF_MEDIA combo#entry

  | Q_COMBO (s, v, args) ->
      let (label, combo, return) = combo_from_value s v args in
      top := attach table !top [(label#coerce,0,1); (combo#coerce,1,3)];
      QF_COMBO return

  | Q_MP3_BITRATE (s, v) ->
      let args = ["64"; "96"; "128"; "160"; "192"] in
      let (label, combo) = combo_entry_from_value s v args in
      top := attach table !top [(label#coerce,0,1); (combo#coerce,1,3)];
      QF_MP3_BITRATE combo#entry

  | Q_MINSIZE (s, v) ->
      let args = ["" ; "Go"; "Mo"; "ko"] in
      let (label, entry, combo) = double_combo_from_value s v args in
      top := attach table !top [(label#coerce,0,1); (entry#coerce,1,2); (combo#coerce,2,3)];
      QF_MINSIZE (entry, combo#entry)

  | Q_MAXSIZE (s, v) ->
      let args = ["" ; "Go"; "Mo"; "ko"] in
      let (label, entry, combo) = double_combo_from_value s v args in
      top := attach table !top [(label#coerce,0,1); (entry#coerce,1,2); (combo#coerce,2,3)];
      QF_MAXSIZE (entry, combo#entry)

  | Q_KEYWORDS (s, v)
  | Q_MP3_ARTIST (s, v)
  | Q_MP3_TITLE (s, v)
  | Q_MP3_ALBUM (s, v) ->
      let markup = GuiTools.create_default_bold_markup (string_to_label s) in
      let wl = GMisc.label ~markup ~xalign:0. ~yalign:0.5 () in
      let entry = GEdit.entry ~text:v ~editable:true ~has_frame:true ~width:90 () in
      let c = GEdit.entry_completion ~model:history_model ~entry () in
      c#set_text_column history_text;
      ignore (entry#event#connect#key_press ~callback:
        (fun ev ->
           GdkEvent.Key.keyval ev = GdkKeysyms._Return &&
           (submit ();
            true)
      ));
      top := attach table !top [(wl#coerce,0,1); (entry#coerce,1,3)];
      form_leaf qe entry

  | Q_HIDDEN le ->
      if !show_hidden_fields
        then begin
          let l = List.map (fun qe -> form_of_entry table qe top) le in
          QF_HIDDEN  l
        end else QF_HIDDEN  []

(*************************************************************************)
(*                                                                       *)
(*                         renderer_pack_combobox                        *)
(*                                                                       *)
(*************************************************************************)

let renderer_pack_combobox (combobox : GEdit.combo_box)
     ((col_pixb : GdkPixbuf.pixbuf option GTree.column),
      (col_text : string GTree.column),
      (col_num  : int GTree.column)) (f : int -> unit) =
  if !!O.gtk_look_use_icons
    then begin
      let pixb_renderer = GTree.cell_renderer_pixbuf [] in
      combobox#pack pixb_renderer ;
      combobox#add_attribute pixb_renderer "pixbuf" col_pixb
    end;
  let str_renderer = GTree.cell_renderer_text [ `XPAD 6 ] in
  combobox#pack str_renderer;
  combobox#add_attribute str_renderer "text" col_text;
  ignore (combobox#connect#changed ~callback:
    (fun _ ->
      match combobox#active_iter with
          Some row -> 
            begin
	      let num = combobox#model#get ~row ~column:col_num in
              f num
            end
        | _ -> ()
  ))

(*************************************************************************)
(*                                                                       *)
(*                         on_net_select                                 *)
(*                                                                       *)
(*************************************************************************)

let on_net_select net_num =
  current_net := net_num

(*************************************************************************)
(*                                                                       *)
(*                         on_query_select                               *)
(*                                                                       *)
(*************************************************************************)

let on_query_select (table : GPack.table) qe_num =
  try
    let qe = List.assoc qe_num !query_entries in
    List.iter (fun w -> w#destroy ()) table#children;
    let top = ref 0 in
    let form = form_of_entry table qe top in
    current_form := Some form;
    current_query_entry := qe_num

  with _ -> ()

(*************************************************************************)
(*                                                                       *)
(*                         build_query_box                               *)
(*                                                                       *)
(*************************************************************************)

let build_query_box () =
  let scroll_table_box =
    GBin.scrolled_window ~hpolicy:`NEVER ~vpolicy:`AUTOMATIC
      ~placement:`TOP_LEFT ()
  in
  let table = GPack.table ~columns:2 ~homogeneous:false
    ~row_spacings:6 ~col_spacings:6 ~border_width:6
    ~packing:scroll_table_box#add_with_viewport ()
  in

  let local_search = GButton.button () in
  let submit_search = GButton.button () in
  let subscribe_search = GButton.button () in
  let customed_editor = GButton.button () in
  List.iter (fun (button, markup, icon) ->
    let hbox =
      GPack.hbox ~homogeneous:false ~spacing:6
        ~packing:button#add ()
    in
    let pixbuf = A.get_icon ~icon ~size:A.SMALL () in
    ignore (GMisc.image ~pixbuf ~xalign:0. ~yalign:0.5
      ~packing:(hbox#pack ~expand:false ~fill:true) ());
    ignore (GMisc.label ~markup ~use_underline:true
      ~mnemonic_widget:button#coerce ~xalign:0. ~yalign:0.5
      ~packing:(hbox#pack ~expand:false ~fill:true) ())
  ) [
     local_search, !M.qT_lb_local_search, M.icon_stock_local_search;
     submit_search, !M.qT_lb_submit_search, M.icon_stock_submit_search;
     subscribe_search, !M.qT_lb_subscribe_search, M.icon_stock_subscribe_search;
     customed_editor, !M.qT_lb_customed_search_editor, M.icon_menu_search_complex;
    ];

  let query_combo = GEdit.combo_box ~model:query_model () in
  let net_combo = GEdit.combo_box ~model:net_model () in
  let range = GData.adjustment ~lower:0. ~upper:(float_of_int max_int) ~step_incr:10. () in
  let markup = GuiTools.create_markup !M.qT_lb_max_hits in
  let max_hits_label = GMisc.label ~markup ~xalign:0. ~yalign:0.5 () in
  let max_hits_entry =
    GEdit.spin_button ~adjustment:range ~rate:1. ~digits:0
      ~numeric:true ~snap_to_ticks:true ~update_policy:`IF_VALID
      ~value:(float_of_int !current_max_hits) ~wrap:true ()
  in
  let show_fields =
    GButton.check_button ~active:!show_hidden_fields
      ~label:!M.qT_lb_extended_fields ()
  in
  let fields_table =
    GPack.table ~columns:3 ~homogeneous:false
      ~row_spacings:3 ~col_spacings:2 ()
  in
  List.iter (fun (w, left, right, top) ->
    table#attach ~left ~top
      ~xpadding:0 ~ypadding:0
      ~right ~bottom:(top + 1)
      ~expand:`X ~fill:`X
      w
  ) [
     local_search#coerce    , 0, 2, 0;
     submit_search#coerce   , 0, 2, 1;
     subscribe_search#coerce, 0, 2, 2;
     customed_editor#coerce , 0, 2, 3;
     query_combo#coerce     , 0, 2, 4;
     net_combo#coerce       , 0, 2, 5;
     max_hits_label#coerce  , 0, 1, 6;
     max_hits_entry#coerce  , 1, 2, 6;
     show_fields#coerce     , 0, 2, 7;
     fields_table#coerce    , 0, 2, 8;
    ];

  renderer_pack_combobox query_combo (query_icon, query_text, query_num)
    (on_query_select fields_table);
  renderer_pack_combobox net_combo (net_icon, net_text, net_num)
    (on_net_select);

  ignore (local_search#connect#clicked ~callback:
    (fun _ ->
       submit ~local:LocalSearch ()
  ));
  ignore (submit_search#connect#clicked ~callback:
    (fun _ ->
       submit ()
  ));
  ignore (subscribe_search#connect#clicked ~callback:
    (fun _ ->
       submit ~local:SubscribeSearch ()
  ));
  ignore (max_hits_entry#connect#value_changed ~callback:
    (fun _ ->
       current_max_hits := max_hits_entry#value_as_int
  ));
  ignore (show_fields#connect#toggled ~callback:
    (fun _ ->
       show_hidden_fields := show_fields#active;
       on_query_select fields_table !current_query_entry
  ));

  query_combo#set_active 0;
  net_combo#set_active 0;
  scroll_table_box#coerce

(*************************************************************************)
(*                                                                       *)
(*                         clear                                         *)
(*                                                                       *)
(*************************************************************************)

let clear () =
  Hashtbl.iter (fun _ qr ->
    qr.g_query_result#clear ()
  ) qresults;
  let _ =
    match !wnote_results with
        None -> ()
      | Some w -> List.iter (fun child -> child#destroy ()) w#children
  in
  view_context := None;
  Hashtbl.clear qresults;
  query_entries := [];
  query_store#clear ();
  net_store#clear ();
  current_form := None;
  current_query_entry := 0;
  current_net := 0


(*************************************************************************)
(*                                                                       *)
(*                         message from the core                         *)
(*                                                                       *)
(*************************************************************************)

let h_search_result query_num result_num =
  try
    let qr = Hashtbl.find qresults query_num in
    try
      let r = Hashtbl.find G.results result_num in
      ignore (qr.g_query_result#add_item r);
      let text = Printf.sprintf "%s (%d)" qr.g_query_desc qr.g_query_result#nitems in
      match qr.g_query_label with
          None -> ()
        | Some label -> label#set_text text
    with Not_found -> (if !!verbose then lprintf' "result doesn't exist for query %d %s\n" query_num qr.g_query_desc)
  with Not_found ->
    (Hashtbl.remove G.results result_num;  (* remove the result if the query does'nt exist *)
     if !!verbose then lprintf' "query %d doesn't exist\n" query_num)

let h_search_waiting query_num waiting =
  try
    let qr = Hashtbl.find qresults query_num in
    qr.g_query_waiting <- Some waiting;
    match qr.g_query_waiting_label with
        None -> ()
      | Some label ->
          begin
            let text = Printf.sprintf "%s %d" !M.qT_lb_waiting_for_replies waiting in
            label#set_text text
          end
  with _ -> ()

let h_define_searches (l : (string * CommonTypes.query_entry) list) =
  query_store#clear ();
  query_entries := [];
  let n = ref 0 in
  List.iter (fun (s, qe) ->
    incr n;
    query_entries := (!n, qe) :: !query_entries;
    build_query_type_menu s !n;
  ) l

(*************************************************************************)
(*                                                                       *)
(*                         message from GuiNetwoks                       *)
(*                                                                       *)
(*************************************************************************)

let reset_results_filter () =
  Hashtbl.iter (fun _ qr ->
    qr.g_query_result#refresh_filter ()
  ) qresults

(*************************************************************************)
(*                                                                       *)
(*                         queries window                                *)
(*                                                                       *)
(*************************************************************************)

open GMain

let queries_box gui =
  let hpaned_queries =
    GPack.paned `HORIZONTAL ()
  in
  let vbox_queries =
    GPack.vbox ~homogeneous:false  ~border_width:6
      ~packing:hpaned_queries#add1 ()
  in
  build_net_menu ();
  let queries = build_query_box () in
  vbox_queries#add queries;

  let frame =
    GBin.frame ~border_width:6 ~shadow_type:`ETCHED_IN
    ~packing:hpaned_queries#add2 ()
  in
  let note =
    GPack.notebook ~homogeneous_tabs:false ~show_border:true
      ~scrollable:true ~enable_popup:false
      ~packing:frame#add ()
  in
  Hashtbl.iter (fun _ qr ->
    display_results note qr
  ) qresults;

  GuiTools.set_hpaned hpaned_queries O.queries_hpane_left;
  GuiTools.get_hpaned hpaned_queries O.queries_hpane_left;

  wnote_results := Some note;

  note#goto_page 0;

  ignore (hpaned_queries#connect#destroy ~callback:
    (fun _ ->
       List.iter (fun child -> child#destroy ()) note#children; (* there is a bug here ?
                                                                 * the notebook pages have to be killed explicitly.
                                                                 * otherwise the #g_view are not killed ?!!!
                                                                 * consequently the timer of the #g_model (results) are
                                                                 * not killed.
                                                                 *)
       Hashtbl.iter (fun _ qr ->
         qr.g_query_label <- None;
         qr.g_query_waiting_label <- None;
       ) qresults;
       view_context := None;
       wnote_results := None;
       current_form := None;
       net_store#clear ();
       current_query_entry := 0;
       current_net := 0
  ));

  hpaned_queries#coerce
