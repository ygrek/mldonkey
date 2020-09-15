(* Copyright 2001, 2002 b8_bavard, b8_fee_carabine, INRIA *)
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

(* GUI for the lists of files. *)

open Gettext
open CommonTypes
open GuiProto
open Gui_columns
open Gui_global


module M = Gui_messages
module P = Gpattern
module O = Gui_options
module G = Gui_global

let (!!) = Options.(!!)

(* This is a bad idea. Customed searches are defined in "searches.ini", and
   so the labels are bad if a new search is added.
   Indeed it is. I didn't know about this. So to improve internationalization,
   keeping this in mind, that should work now ?
   Most of the users will not touch this file and I guess if someone changes 
   something in the "searches.ini" he can understand what he writes *)

let label_to_text_list =
  [
   ("Complex Search", M.qT_lb_complex_searches );
   ("MP3 Search", M.qT_lb_mp3_searches );
   ("Movie Search", M.qT_lb_movie_searches );
   ("Album Search", M.qT_lb_album_searches );
   ("And Not", M.qT_lb_and_not );
   ("Audio", M.qT_tx_audio );
   ("Video", M.qT_tx_video );
   ("Program", M.qT_tx_program );
   ("Image", M.qT_tx_image );
   ("Documentation", M.qT_tx_documentation );
   ("Collection", M.qT_tx_collection );
   ("Keywords", M.qT_lb_keywords );
   ("Media", M.qT_lb_media );
   ("Format", M.qT_lb_format );
   ("Min size", M.qT_lb_min_size );
   ("Max size", M.qT_lb_max_size );
   ("Min Bitrate", M.qT_lb_min_bitrate );
   ("Title", M.qT_lb_title );
   ("Number of results", M.qT_lb_number_of_results );
   ("Sort by", M.qT_lb_sort_by );
   ("Album", M.qT_lb_album );
   ("Fields", M.qT_lb_fields );
   ("Artist", M.qT_lb_artist );
   ("Track/Title", M.qT_lb_track_title );
   ("Track", M.qT_lb_track );
   ("Rest", M.qT_lb_rest );
   ("Categories", M.qT_lb_categories );
   ("All", M.qT_lb_all );
   ("Blues", M.qT_lb_blues );
   ("Classical", M.qT_lb_classical );
   ("Data", M.qT_lb_data );
   ("Folk", M.qT_lb_folk );
   ("Rock", M.qT_lb_rock );
   ("Soundtrack", M.qT_lb_soundtrack );
   ("Availability", M.qT_lb_availability );
   ("Completesources", M.qT_lb_completesources );
   ("Size", M.qT_lb_size );
   ("DVD Rips", M.qT_lb_dvd_rips );
   ("Screeners", M.qT_lb_screeners );
   ("PC Games", M.qT_lb_pc_games );
   ("Software", M.qT_lb_software );
   ("Anime", M.qT_lb_anime );
   ("Series", M.qT_lb_series);
   ("Funstuff", M.qT_lb_funstuff );
   ("Adult", M.qT_lb_adult );
   ("Consoles", M.qT_lb_consoles );
   ("Books", M.qT_lb_books );
   ("XBOX", M.qT_lb_xbox );
   ("Hentai", M.qT_lb_hentai );
   ("PS2", M.qT_lb_ps2 );
   ("Gay", M.qT_lb_gay);
 ]
 

let text_to_label_list = List.map (fun (l, t) -> (t, l)) label_to_text_list

let label_to_text label =
  try
    List.assoc label label_to_text_list
  with Not_found -> label

let text_to_label text = 
  try
    List.assoc text text_to_label_list
  with Not_found -> text

(* Add these regexps in gui_options.ini *)
let album_reg = Str.regexp_case_fold ".*album.*"
let mp3_reg = Str.regexp_case_fold ".*mp3.*"
let movie_reg = Str.regexp_case_fold ".*movie.*"

let string_to_icon s =
  Printf2.lprintf "LABEL: [%s]\n" s;
  let label = String.lowercase s in
  if Str.string_match album_reg label 0 
    then M.o_xpm_album_search 
    else if Str.string_match movie_reg label 0 
      then M.o_xpm_movie_search 
      else if Str.string_match mp3_reg label 0 
        then M.o_xpm_mp3_search 
        else M.o_xpm_complex_search
  
let tab_box icon wlabel =
  let hbox = GPack.hbox ~homogeneous:false ~spacing:2 () in
  let main_pix = Gui_options.gdk_pix icon in
  ignore (GMisc.pixmap main_pix ~packing:hbox#add ());
  let label = GMisc.label ~text:wlabel ~packing:hbox#add () in
  hbox

let menu_label_box icon menu_text =
  let hbox = GPack.hbox ~homogeneous:false ~spacing:2 () in
  let menu_pix = Gui_options.gdk_pix icon in
  ignore (GMisc.pixmap menu_pix ~packing:hbox#pack ());
  ignore (GMisc.label ~text:menu_text ~justify:`LEFT ~packing:hbox#pack ());
  hbox

let history = ref []

let fill_history label =
  if not (List.mem label !history) 
    then history := List.sort (fun l1 l2 -> compare l1 l2) (label::!history)

type ent = GEdit.entry

type query_form =
  QF_AND of query_form list
| QF_OR of query_form list
| QF_ANDNOT of query_form * query_form  
| QF_MODULE of query_form
  
| QF_KEYWORDS of ent
| QF_MINSIZE of ent * ent (** number and unit *)
| QF_MAXSIZE of ent * ent (** number and unit *)
| QF_FORMAT of ent
| QF_MEDIA of ent
  
| QF_MP3_ARTIST of ent
| QF_MP3_TITLE of ent
| QF_MP3_ALBUM of ent
| QF_MP3_BITRATE of ent

| QF_COMBO of ent
  
| QF_HIDDEN of GPack.box * query_form list

let form_leaf qe we =
  match qe with
  | Q_KEYWORDS _ -> QF_KEYWORDS we
  | Q_MP3_ARTIST _ -> QF_MP3_ARTIST we
  | Q_MP3_TITLE  _ -> QF_MP3_TITLE we
  | Q_MP3_ALBUM  _ -> QF_MP3_ALBUM we
  | _ -> assert false

let rec form_of_entry f_submit qe =
  match qe with
    Q_AND le ->
      
      let vbox = GPack.vbox ~spacing:2 () in
      
      let rec iter l = 
        match l with
          [] -> []
        | (Q_MODULE (s1, qe1) ) :: (Q_MODULE (s2, qe2) ) :: tail ->
            let hbox = GPack.hbox ~spacing:2 ~packing:vbox#add () in
            
            let wf1 = GBin.frame ~label:s1 ~border_width:2 () in
            let (w1,e1,f1) = form_of_entry f_submit qe1 in
            wf1#add w1;
            
            let wf2 = GBin.frame ~label:s2 ~border_width:2 () in
            let (w2,e2,f2) = form_of_entry f_submit qe2 in
            wf2#add w2;
            
            hbox#pack ~padding: 2 ~expand: e1 wf1#coerce;            
            hbox#pack ~padding: 2 ~expand: e2 wf2#coerce;
            
            (QF_MODULE f1) :: (QF_MODULE f2) :: (iter tail)
        
        | qe :: tail ->
            let (w, expand, form) = form_of_entry f_submit qe in
            vbox#pack ~padding: 2 ~expand w;
            form :: (iter tail)
      in
      let le = QF_AND (iter le) in

(*
      let l = List.map (form_of_entry f_submit) le in
      List.iter (fun (w,expand,form) -> vbox#pack ~padding: 2 ~expand w) l;
*)
      
      (vbox#coerce, false, le)
(*       QF_AND (List.map (fun (_,_,f) -> f) l)) *)

  | Q_OR le ->
      let wf = GBin.frame ~label: "||" () in
      let vbox = GPack.vbox ~packing: wf#add () in
      let l = List.map (form_of_entry f_submit) le in
      List.iter (fun (w,expand,form) -> vbox#pack ~padding: 2 ~expand w) l;
      (wf#coerce, false, 
       QF_OR (List.map (fun (_,_,f) -> f) l))
      
  | Q_ANDNOT (qe1, qe2) ->
      let wf = GBin.frame () in
      let vbox = GPack.vbox ~packing: wf#add () in
      let (w1,e1,f1) = form_of_entry f_submit qe1 in
      let (w2,e2,f2) = form_of_entry f_submit qe2 in
      vbox#pack ~padding: 2 ~expand: e1 w1;
      vbox#pack ~padding:2 ~expand:false (GMisc.label
        ~text:(M.qT_lb_and_not) ())#coerce;
      vbox#pack ~padding: 2 ~expand: e2 w2;
      (wf#coerce, false, QF_ANDNOT (f1, f2))

  | Q_MODULE (s, qe) ->
      let wf = GBin.frame ~label: s () in
      let (w,_,f) = form_of_entry f_submit qe in
      wf#add w;
      (wf#coerce, true, QF_MODULE f)
        
  
  | Q_FORMAT (label, v) ->
      let hbox = GPack.hbox () in
      let text = (label_to_text label) ^ " :" in
      let wl = GMisc.label ~text
          ~justify:`LEFT
          ~xalign:0.0  ~xpad:1 ~width:100
          ~packing: (hbox#pack ~padding:2 ~expand:false ~fill:false) ()
      in
      let wcombo = GEdit.combo
          ~popdown_strings: [""; "avi"; "mp3"]
          ~value_in_list: false
          ~ok_if_empty: true  
          ~packing: (hbox#pack ~padding: 2 ~expand: false) () 
      in
      wcombo#entry#set_text v;
      (hbox#coerce, false, QF_FORMAT wcombo#entry)

  | Q_MEDIA (label, v) ->
      let hbox = GPack.hbox () in
      let text = (label_to_text label) ^ " :" in
      let wl = GMisc.label ~text
          ~justify:`LEFT
          ~xalign:0.0  ~xpad:1 ~width:100
          ~packing: (hbox#pack ~padding:2) ()
      in
      let wcombo = GEdit.combo
          ~popdown_strings:
          (List.map (fun label -> label_to_text label)
          ["";
           "Audio";
           "Video";
           "Program";
           "Image";
           "Documentation";
           "Collection"])
          ~value_in_list: false
          ~ok_if_empty: true  
          ~packing: (hbox#pack ~padding: 2 ~expand: false) () 
      in
      wcombo#entry#set_text v;
      (hbox#coerce, false, QF_MEDIA wcombo#entry)

  | Q_COMBO (label, default, args) ->
      let hbox = GPack.hbox () in
      let text = (label_to_text label) ^ " :" in
      let popdown_strings = 
        List.map (fun label -> 
          label_to_text label) args
      in
      let wl = GMisc.label ~text
          ~justify:`LEFT
          ~xalign:0.0  ~xpad:1 ~width:100
          ~packing: (hbox#pack ~padding:2) ()
      in
      let wcombo = GEdit.combo
          ~popdown_strings
          ~value_in_list: true
          ~ok_if_empty: true  
          ~packing: (hbox#pack ~padding: 2 ~expand: false) () 
      in
      wcombo#entry#set_text default;
      (hbox#coerce, false, QF_COMBO wcombo#entry)

  | Q_MP3_BITRATE (label, v) ->
      let hbox = GPack.hbox () in
      let text = (label_to_text label) ^ " :" in
      let wl = GMisc.label ~text
          ~justify:`LEFT
          ~xalign:0.0  ~xpad:1 ~width:100
          ~packing: (hbox#pack ~padding:2) ()
      in
      let wcombo = GEdit.combo
          ~popdown_strings: [""; "64"; "96"; "128"; "160"; "192"]
          ~value_in_list: false
          ~ok_if_empty: true  
        ~width: 60
          ~packing: (hbox#pack ~padding: 2 ~expand: false) () 
      in
      wcombo#entry#set_text v;
      (hbox#coerce, false, QF_MP3_BITRATE wcombo#entry)
        
  | Q_MINSIZE (label, v) ->
      let hbox = GPack.hbox () in
      let text = (label_to_text label) ^ " :" in
      let wl = GMisc.label ~text
          ~justify:`LEFT
          ~xalign:0.0  ~xpad:1 ~width:100
          ~packing: (hbox#pack ~padding:2) ()
      in
      let we = GEdit.entry
          ~width:100
          ~packing:(hbox#pack ~padding:2 ~expand:false) () in
      let wcombo = GEdit.combo
          ~popdown_strings: ["" ; "Mo"; "ko"]
          ~value_in_list: false
          ~ok_if_empty: true
          ~width: 60
          ~packing: (hbox#pack ~padding:2 ~expand:false) ()
      in
      we#set_text v;
      (hbox#coerce, false, QF_MINSIZE (we, wcombo#entry))

  | Q_MAXSIZE (label, v) ->
      let hbox = GPack.hbox () in
      let text = (label_to_text label) ^ " :" in
      let wl = GMisc.label ~text
          ~justify:`LEFT
          ~xalign:0.0  ~xpad:1 ~width:100
          ~packing: (hbox#pack ~padding:2) ()
      in
      let we = GEdit.entry
          ~width:100
          ~packing:(hbox#pack ~padding:2 ~expand:false) () in
      let wcombo = GEdit.combo
          ~popdown_strings: ["" ; "Mo"; "ko"]
          ~value_in_list: false
          ~ok_if_empty: true
          ~width: 60
          ~packing: (hbox#pack ~padding:2 ~expand:false) ()
      in
      we#set_text v;
      (hbox#coerce, false, QF_MAXSIZE (we, wcombo#entry))

  | Q_KEYWORDS (label, v) ->
      let hbox = GPack.hbox () in
      let text = (label_to_text label) ^ " :" in
      let wl = GMisc.label ~text
          ~justify:`LEFT
          ~xalign:0.0 ~xpad:1 ~width:100
          ~packing: (hbox#pack ~padding:2) ()
      in
      let wcombo = GEdit.combo
          ~popdown_strings:[]
          ~value_in_list:false
          ~ok_if_empty:true  
          ~packing: (hbox#pack ~padding: 2 ~expand: false) () 
      in
      let we = wcombo#entry in
      Okey.add we ~mods: [] GdkKeysyms._Return 
        (fun () ->
           f_submit ();
           fill_history we#text;
           wcombo#set_popdown_strings !history);
      we#set_text v;
      (hbox#coerce, false, (form_leaf qe we))

  | Q_MP3_ARTIST (label, v)
  | Q_MP3_TITLE (label, v)
  | Q_MP3_ALBUM (label, v) ->
      let hbox = GPack.hbox () in
      let text = (label_to_text label) ^ " :" in
      let wl = GMisc.label ~text
          ~justify:`LEFT
          ~xalign:0.0  ~xpad:1 ~width:100
          ~packing: (hbox#pack ~padding:2) ()
      in
      let we = GEdit.entry
          ~width: 200
          ~packing:(hbox#pack ~padding:2 ~expand:false) () in
      Okey.add we ~mods: [] GdkKeysyms._Return f_submit;
      we#set_text v;
      (hbox#coerce, false, (form_leaf qe we))
        
  | Q_HIDDEN le ->
      let vbox = GPack.vbox () in
      vbox#misc#hide ();
      let l = List.map (form_of_entry f_submit) le in
      List.iter
        (fun (w, expand, form) -> vbox#pack ~padding: 2 ~expand w)
        l;
      (vbox#coerce, false, 
       QF_HIDDEN (vbox, List.map (fun (_,_,f) -> f) l))

let rec hide_or_show show qf =
  match qf with
    QF_AND l
  | QF_OR l -> 
      List.iter (hide_or_show show) l
  | QF_ANDNOT (qf1, qf2) ->
      hide_or_show show qf1;
      hide_or_show show qf2
  | QF_MODULE qf ->
      hide_or_show show qf
        
  | QF_KEYWORDS _
  | QF_MINSIZE _
  | QF_MAXSIZE _
  | QF_FORMAT _
  | QF_MEDIA _
  | QF_MP3_ARTIST _
  | QF_MP3_TITLE _
  | QF_MP3_ALBUM _
  | QF_COMBO _
  | QF_MP3_BITRATE  _ -> ()

  | QF_HIDDEN (b, l) ->
      if show then b#misc#show () else b#misc#hide () ;
      List.iter (hide_or_show show) l

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
  | QF_KEYWORDS we -> Q_KEYWORDS ("", we#text)
  | QF_MINSIZE (we,we2) -> 
      let size = 
        try 
          Int32.to_string
            (Int32.mul (Int32.of_string we#text) (Gui_misc.unit_of_string we2#text))
        with _ -> ""
      in
      Q_MINSIZE ("", size)
  | QF_MAXSIZE (we,we2) -> 
      let size = 
        try Int32.to_string
            (Int32.mul (Int32.of_string we#text) (Gui_misc.unit_of_string we2#text))
        with _ -> ""
      in
      Q_MAXSIZE ("", size)
      
  | QF_COMBO we -> Q_COMBO ("", text_to_label we#text, [])
      
  | QF_FORMAT we -> Q_FORMAT ("", we#text)
  | QF_MEDIA we -> Q_MEDIA ("", text_to_label we#text)
  | QF_MP3_ARTIST we -> Q_MP3_ARTIST ("", we#text)
  | QF_MP3_TITLE we -> Q_MP3_TITLE ("", we#text)
  | QF_MP3_ALBUM we -> Q_MP3_ALBUM ("", we#text)
  | QF_MP3_BITRATE  we -> Q_MP3_BITRATE ("", we#text)
  
  | QF_HIDDEN (b, l) ->
      Q_HIDDEN (List.map entry_of_form l)


class box submit_search query_entry =
  let f_submit = ref (fun () -> ()) in
  let (w,expand,form) = form_of_entry 
      (fun () -> !f_submit ()) 
    query_entry 
  in
  object(self)
    inherit Gui_queries_base.box () 
    
    method set_nets l = nets <- l

    method submit ?(local=RemoteSearch) () = 
      let qe = entry_of_form form in
      let max_hits =
        try int_of_string we_max_hits#text
        with _ -> 200
      in
      let net = List.assoc nets_wcombo#entry#text nets in 
      let search = Gui_misc.create_search qe max_hits net local in
      submit_search search
    
    method local () = self#submit ~local: LocalSearch ()
    method subscribe () = self#submit ~local: SubscribeSearch ()
    
    method set_tb_style st = 
        if Options.(!!) Gui_options.mini_toolbars then
          (wtool1#misc#hide (); wtool2#misc#show ()) else
          (wtool2#misc#hide (); wtool1#misc#show ());
      wtool1#set_style st;
      wtool2#set_style st
    
    initializer
      f_submit := (fun () -> self#submit ());
      box_fields#pack w;
      ignore (wchk_show#connect#clicked
          (fun () -> hide_or_show wchk_show#active form));

      Gui_misc.insert_buttons wtool1 wtool2 
        ~text: (M.qT_lb_submit)
        ~tooltip: (M.qT_ti_submit)
        ~icon: (M.o_xpm_submit_search)
        ~callback: self#submit
        ();
      Gui_misc.insert_buttons wtool1 wtool2
        ~text: (M.qT_lb_local_search)
        ~tooltip: (M.qT_ti_local_search)
        ~icon: (M.o_xpm_local_search)
        ~callback: self#local
        ();
      Gui_misc.insert_buttons wtool1 wtool2
        ~text: (M.qT_lb_subscribe)
        ~tooltip: (M.qT_ti_subscribe)
        ~icon: (M.o_xpm_subscribe_search)
        ~callback: self#subscribe
        ();

end

(*   let query_entry = Q_KEYWORDS ("Title", "") in *)

class url_box query_entry submit_search =
  let f_submit = ref (fun () -> ()) in
  let (w,expand,form) = form_of_entry 
      (fun () -> !f_submit ()) 
    query_entry 
  in
  object(self)
    inherit Gui_queries_base.box () 
    
    method submit () = 
      let qe = entry_of_form form in
      submit_search qe
      
    method set_tb_style st = 
      wtool1#set_style st;
      wtool2#set_style st;
        if Options.(!!) Gui_options.mini_toolbars then
        (wtool1#misc#hide (); wtool2#misc#show ()) else
        (wtool2#misc#hide (); wtool1#misc#show ())
    
    initializer

      f_submit := (fun () -> self#submit ());
      box_fields#pack w;
      ignore (wchk_show#connect#clicked
          (fun () -> hide_or_show wchk_show#active form));

      network_label#misc#hide ();
      nets_wcombo#misc#hide ();

      Gui_misc.insert_buttons wtool1 wtool2
        ~text: (M.qT_lb_submit)
        ~tooltip: ( M.qT_ti_submit)
        ~icon: (M.o_xpm_submit_search)
        ~callback: self#submit
        ()

end


class paned () =

  object(self)
    inherit Gui_queries_base.paned ()

(* Associations (number of the gui query, box displaying results) *)
    val mutable results = ([] : (int * Gui_results.search_result_box) list)

    val mutable queries_box = ([] : box list)
    val mutable static_queries_box = ([] : url_box list)
    val mutable static_results = ([] : Gui_cdget.url_results list)
      
    val mutable wnote_main = GPack.notebook () 

(*  val mutable wnote_results = GPack.notebook ()   
    method wnote_results = wnote_results  
    method set_wnote_results w = wnote_results <- w *)

    method set_wnote_main w = wnote_main <- w
    
    method set_list_bg bg font =
      List.iter (fun (_, b) -> b#set_list_bg bg font) results;
      List.iter (fun b -> b#set_list_bg bg font) static_results

    method c_update_icons b =
      List.iter (fun (_,(box : Gui_results.search_result_box)) ->
        box#update_icons b
      ) results

    method clear = 
      List.iter (fun box -> box#coerce#destroy ()) queries_box;
      queries_box <- [];
      Hashtbl.clear G.results;
      List.iter (fun (_, box_res) ->
        box_res#clear;
        box_res#coerce#destroy ()
      ) results;
      results <- []
    
    method close_query num forget () =
      try
        if forget then  begin
            let box_res = List.assoc num results in
            box_res#clear;
            box_res#coerce#destroy ();
            Hashtbl.remove G.results num;
            results <- List.filter (fun (n,_) -> n <> num) results;
          end;
        Gui_com.send (GuiProto.CloseSearch (num, forget))
      with
        Not_found ->
          ()
    
    method submit_search s =
      Gui_com.send (GuiProto.Search_query (s));
      let desc = Gui_misc.description_of_query s.GuiTypes.search_query in
      let wl = GMisc.label ~text: desc () in
      let box_res = new Gui_results.search_result_box s.GuiTypes.search_num () in
      ignore (Gui_misc.insert_buttons box_res#wtool1 box_res#wtool2
          ~text: ( M.qT_lb_close_search)
          ~tooltip: (M.qT_ti_close_search)
          ~icon: (M.o_xpm_close_search)
          ~callback:(self#close_query s.GuiTypes.search_num true)
          ());
      ignore (Gui_misc.insert_buttons box_res#wtool1 box_res#wtool2
          ~text: (M.qT_lb_stop_search)
          ~tooltip: (M.qT_ti_stop_search)
          ~icon: (M.o_xpm_stop_search)
          ~callback:(self#close_query s.GuiTypes.search_num false)
          ());
      wnote_results#insert_page ~tab_label:wl#coerce ~pos:0 box_res#coerce;
      wnote_results#goto_page 0;
      wnote_main#goto_page 4;
      box_res#set_tb_style !!Gui_options.toolbars_style;
      box_res#set_list_bg (`NAME !!Gui_options.color_list_bg) (Gdk.Font.load_fontset !!O.font_list);

(* only the last result box must have an "extended search" button *)
(*      List.iter (fun (_,(b,_)) -> b#remove_extend_search_button) results; *)

      results <- (s.GuiTypes.search_num, box_res) :: results      

    method submit_url_search result_box qe =
      let desc = Gui_misc.description_of_query qe in
      let wl = GMisc.label ~text: desc () in
      let box_res = result_box qe in
      static_results <- box_res :: static_results;
      ignore (Gui_misc.insert_buttons box_res#wtool1 box_res#wtool2
          ~text: (M.qT_lb_close_search)
          ~tooltip: (M.qT_ti_close_search)
          ~icon: (M.o_xpm_close_search)
          ~callback:(fun _ ->
            static_results <- List2.removeq box_res static_results;
            box_res#coerce#destroy ()
            )
          ());
      wnote_results#insert_page ~tab_label:wl#coerce ~pos:0 box_res#coerce;
      wnote_results#goto_page 0;
      wnote_main#goto_page 4

(* only the last result box must have an "extended search" button *)
(*      List.iter (fun (_,(b,_)) -> b#remove_extend_search_button) results; *)
    
    method set_tb_style st = 
      List.iter (fun b -> b#set_tb_style st) queries_box;
      List.iter (fun b -> b#set_tb_style st) static_queries_box;
      List.iter (fun (_, b) -> b#set_tb_style st) results;
      List.iter (fun b -> b#set_tb_style st) static_results

(* {2 Handling of core messages} *)
    
    method h_search_filter_networks =
      List.iter (fun (_, srbox) -> srbox#filter_networks) results
    
    method h_search_result num res =
      try
        let box_res = List.assoc num results in
        box_res#add_result res
      with
        Not_found ->
          ()
    
    method h_search_waiting num waiting =
      try
        let box_res = List.assoc num results in
        box_res#set_waiting waiting
      with
        Not_found ->
          ()
    
    method h_define_searches l =
      let f (label, qe) =
        let b = new box self#submit_search qe in
        let icon = string_to_icon label in
        let text = label_to_text label in
        wnote_queries#insert_page ~pos: 0
            ~tab_label:(tab_box icon text)#coerce
            ~menu_label:(menu_label_box icon text)#coerce
            b#coerce;
        wnote_queries#goto_page 0;
        b#set_tb_style !!Gui_options.toolbars_style;
        queries_box <- b :: queries_box
      in
      List.iter f l

   (* the combo box was not updated when the networks activated changed *)
    method update_wcombos =
      List.iter (fun (w : box) ->
        try
          begin
            let combo = w#nets_wcombo in
            let nets = ref [] in
            Hashtbl.iter (fun (num : int) (net : net_info) ->
              if net.net_enabled then
                nets := (net.net_name, num) :: !nets
            ) networks;
            let nets = ( M.qT_tx_all_networks, 0) :: !nets in
            combo#set_popdown_strings (List.map fst nets);
            w#set_nets nets
          end
        with _ -> ()
      ) queries_box

    initializer
    
    let b = new url_box Gui_cdget.ShareReactor.query
        (self#submit_url_search (new Gui_cdget.ShareReactor.results)) in
    static_queries_box <- b :: static_queries_box;
    wnote_queries#append_page
      ~tab_label:
        (tab_box M.o_xpm_sharereactor_search 
        (M.qT_lb_sharereactor_searches))#coerce
      ~menu_label:
        (menu_label_box M.o_xpm_sharereactor_search 
        (M.qT_lb_sharereactor_searches))#coerce
      b#coerce;
    self#set_list_bg (`NAME !!Gui_options.color_list_bg) 
      (Gdk.Font.load_fontset !!O.font_list);
    
    let b = new url_box Gui_cdget.Jigle.query
        (self#submit_url_search (new Gui_cdget.Jigle.results)) in
    static_queries_box <- b :: static_queries_box;
    wnote_queries#append_page
      ~tab_label:
        (tab_box M.o_xpm_jigle_search 
        (M.qT_lb_jigle_searches))#coerce
      ~menu_label:
        (menu_label_box M.o_xpm_jigle_search 
        (M.qT_lb_jigle_searches))#coerce
      b#coerce;
    self#set_list_bg (`NAME !!Gui_options.color_list_bg) 
      (Gdk.Font.load_fontset !!O.font_list);
    
    let b = new url_box Gui_cdget.FreeDB.query 
        (self#submit_url_search (new Gui_cdget.FreeDB.results)) in
    static_queries_box <- b :: static_queries_box;
    wnote_queries#append_page
      ~tab_label:
        (tab_box M.o_xpm_freedb_search 
        (M.qT_lb_freedb_searches))#coerce
      ~menu_label:
        (menu_label_box M.o_xpm_freedb_search 
        (M.qT_lb_freedb_searches))#coerce
      b#coerce;
    self#set_list_bg (`NAME !!Gui_options.color_list_bg) 
      (Gdk.Font.load_fontset !!O.font_list);
    
    let b = new url_box (Q_KEYWORDS ((Gui_messages.qT_lb_title), ""))
        (self#submit_url_search (new Gui_cdget.IMDB.results)) in
    static_queries_box <- b :: static_queries_box;
    wnote_queries#append_page
      ~tab_label:
        (tab_box M.o_xpm_imdb_search 
        (M.qT_lb_imdb_searches))#coerce
      ~menu_label:
        (menu_label_box M.o_xpm_imdb_search 
        (M.qT_lb_imdb_searches))#coerce
      b#coerce;
    self#set_list_bg (`NAME !!Gui_options.color_list_bg) 
      (Gdk.Font.load_fontset !!O.font_list);

  end
