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

(** GUI for the lists of files. *)

open CommonTypes
open Gui_proto
open Gui_columns



module M = Gui_messages
module P = Gpattern
module O = Gui_options

let (!!) = Options.(!!)


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
      let wf = GBin.frame ~label: "&" () in
      let vbox = GPack.vbox ~packing: wf#add () in
      let l = List.map (form_of_entry f_submit) le in
      List.iter (fun (w,expand,form) -> vbox#pack ~padding: 2 ~expand w) l;
      (wf#coerce, false, 
       QF_AND (List.map (fun (_,_,f) -> f) l))

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
      vbox#pack ~padding: 2 ~expand: false (GMisc.label ~text: "AND NOT" ())#coerce;
      vbox#pack ~padding: 2 ~expand: e2 w2;
      (wf#coerce, false, QF_ANDNOT (f1, f2))

  | Q_MODULE (s, qe) ->
      let wf = GBin.frame ~label: s () in
      let (w,_,f) = form_of_entry f_submit qe in
      wf#add w;
      (wf#coerce, true, QF_MODULE f)
	
  
  | Q_FORMAT (label, v) ->
      let hbox = GPack.hbox () in
      let wl = GMisc.label ~text: (label^":")
	  ~packing: (hbox#pack ~padding: 2 ~expand: false) ()
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
      let wl = GMisc.label ~text: (label^":")
	  ~packing: (hbox#pack ~padding: 2 ~expand: false) ()
      in
      let wcombo = GEdit.combo
	  ~popdown_strings: [""; "Audio"; "Video"; "Program"; 
			      "Image"; "Documentation"; "Collection"]
	  ~value_in_list: false
	  ~ok_if_empty: true  
	  ~packing: (hbox#pack ~padding: 2 ~expand: false) () 
      in
      wcombo#entry#set_text v;
      (hbox#coerce, false, QF_MEDIA wcombo#entry)

  | Q_MP3_BITRATE (label, v) ->
      let hbox = GPack.hbox () in
      let wl = GMisc.label ~text: (label^":")
	  ~packing: (hbox#pack ~padding: 2 ~expand: false) ()
      in
      let wcombo = GEdit.combo
	  ~popdown_strings: [""; "64"; "96"; "128"; "160"; "192"]
	  ~value_in_list: false
	  ~ok_if_empty: true  
	  ~packing: (hbox#pack ~padding: 2 ~expand: false) () 
      in
      wcombo#entry#set_text v;
      (hbox#coerce, false, QF_MP3_BITRATE wcombo#entry)
	
  | Q_MINSIZE (label, v) ->
      let hbox = GPack.hbox () in
      let wl = GMisc.label ~text: (label^":")
	  ~packing: (hbox#pack ~padding: 2 ~expand: false) ()
      in
      let we = GEdit.entry ~packing: (hbox#pack ~padding: 2 ~expand: false) () in
      let wcombo = GEdit.combo
	  ~popdown_strings: ["" ; "Mo"; "ko"]
	  ~value_in_list: false
	  ~ok_if_empty: true
	  ~width: 80
	  ~packing: (hbox#pack ~padding: 2 ~expand: false ~fill: false) () 
      in
      we#set_text v;
      (hbox#coerce, false, QF_MINSIZE (we, wcombo#entry))

  | Q_MAXSIZE (label, v) ->
      let hbox = GPack.hbox () in
      let wl = GMisc.label ~text: (label^":")
	  ~packing: (hbox#pack ~padding: 2 ~expand: false) ()
      in
      let we = GEdit.entry ~packing: (hbox#pack ~padding: 2 ~expand: false) () in
      let wcombo = GEdit.combo
	  ~popdown_strings: ["" ; "Mo"; "ko"]
	  ~value_in_list: false
	  ~ok_if_empty: true
	  ~width: 80
	  ~packing: (hbox#pack ~padding: 2 ~expand: false ~fill: false) () 
      in
      we#set_text v;
      (hbox#coerce, false, QF_MAXSIZE (we, wcombo#entry))

  | Q_KEYWORDS (label, v)
  | Q_MP3_ARTIST (label, v)
  | Q_MP3_TITLE (label, v)
  | Q_MP3_ALBUM (label, v) ->
      let hbox = GPack.hbox () in
      let wl = GMisc.label ~text: (label^":")
	  ~packing: (hbox#pack ~padding: 2 ~expand: false) ()
      in
      let we = GEdit.entry ~packing: (hbox#pack ~padding: 2 ~expand: false) () in
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
  | QF_FORMAT we -> Q_FORMAT ("", we#text)
  | QF_MEDIA we -> Q_MEDIA ("", we#text)
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
	
    method submit ?(local=false) () = 
      let qe = entry_of_form form in
      let max_hits =
	try int_of_string we_max_hits#text
	with _ -> 200
      in
      let search = Gui_misc.create_search qe max_hits in
      submit_search local search

    method local () = self#submit ~local: true ()

    method set_tb_style st = 
      wtool#set_style st;

    initializer
      f_submit := (fun () -> self#submit ());
      box_fields#pack w;
      ignore (wchk_show#connect#clicked
		(fun () -> hide_or_show wchk_show#active form));
      ignore
	(wtool#insert_button 
	   ~text: M.submit
	   ~tooltip: M.submit
	   ~icon: (Gui_icons.pixmap M.o_xpm_submit_search)#coerce
	   ~callback: self#submit
	   ()
	);
      ignore
	(wtool#insert_button 
	   ~text: M.local_search
	   ~tooltip: M.local_search
	   ~icon: (Gui_icons.pixmap M.o_xpm_local_search)#coerce
	   ~callback: self#local
	   ()
	);
      
  end


class paned () =
  object(self)
    inherit Gui_queries_base.paned ()

    (** Associations (numéro de query de gui, boite affichant les résultats) *)
    val mutable results = ([] : (int * (Gui_results.search_result_box * GPack.box)) list)

    val mutable queries_box = ([] : box list)

    val mutable wnote_main = GPack.notebook () 
    val mutable wnote_results = GPack.notebook () 
    method set_wnote_results w = wnote_results <- w
    method set_wnote_main w = wnote_main <- w

    method close_query num () =
      try
	let (box_res, vb) = List.assoc num results in
	vb#destroy ();
	results <- List.filter (fun (n,_) -> n <> num) results;
	Gui_com.send (Gui_proto.ForgetSearch num)
      with
	Not_found ->
	  ()

    method submit_search local (s : search_request) =
      Gui_com.send (Gui_proto.Search_query (local, s));
      let desc = Gui_misc.description_of_query s.Gui_proto.search_query in
      let wl = GMisc.label ~text: desc () in
      let vbox = GPack.vbox () in
      let box_res = new Gui_results.search_result_box () in
      vbox#pack ~expand: true box_res#coerce;
      let wb_close = GButton.button ~label: M.close_search
	  ~packing: (vbox#pack ~expand: false)
	  ()
      in
      ignore (wb_close#connect#clicked (self#close_query s.Gui_proto.search_num));
      wnote_results#insert_page ~tab_label: wl#coerce ~pos: 0 vbox#coerce;
      wnote_results#goto_page 0;
      wnote_main#goto_page 4;

      (* only the last result box must have an "extended search" button *)
      List.iter (fun (_,(b,_)) -> b#remove_extend_search_button) results;

      results <- (s.Gui_proto.search_num, (box_res, vbox)) :: results

    method set_tb_style st = 
      List.iter (fun b -> b#set_tb_style st) queries_box;
      List.iter (fun (_,(b,_)) -> b#set_tb_style st) results

    (** {2 Handling of core messages} *)

    method h_search_filter_networks =
      List.iter (fun (_, (srbox, _)) -> srbox#filter_networks) results
      
    method h_search_result num res =
      try
	let (box_res, vb) = List.assoc num results in
	box_res#add_result res
      with
	Not_found ->
	  ()

    method h_search_waiting num waiting =
      try
	let (box_res, vb) = List.assoc num results in
	box_res#set_waiting waiting
      with
	Not_found ->
	  ()

    method h_define_searches l =
      let f (label, qe) =
	let b = new box self#submit_search qe in
	let wl = GMisc.label ~text: label () in
	wnote_queries#append_page ~tab_label: wl#coerce b#coerce;
	queries_box <- b :: queries_box
      in
      List.iter f l

    initializer
      ()
  end
