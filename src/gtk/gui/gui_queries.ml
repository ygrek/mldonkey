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

open Gettext
open CommonTypes
open GuiProto
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
      let wf = GBin.frame ~label: "&" () in
      let vbox = GPack.vbox ~packing: wf#add () in
      
      let rec iter l = 
        match l with
          [] -> []
        | (Q_MODULE (s1, qe1) ) :: (Q_MODULE (s2, qe2) ) :: tail ->
            let hbox = GPack.hbox ~packing: vbox#add () in
            
            let wf1 = GBin.frame ~label: s1 () in
            let (w1,e1,f1) = form_of_entry f_submit qe1 in
            wf1#add w1;
            
            let wf2 = GBin.frame ~label: s2 () in
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
      
      (wf#coerce, false, le)
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

  | Q_COMBO (label, default, args) ->
      let hbox = GPack.hbox () in
      let wl = GMisc.label ~text: (label^":")
          ~packing: (hbox#pack ~padding: 2 ~expand: false) ()
      in
      let wcombo = GEdit.combo
          ~popdown_strings: args
          ~value_in_list: true
          ~ok_if_empty: true  
          ~packing: (hbox#pack ~padding: 2 ~expand: false) () 
      in
      wcombo#entry#set_text default;
      (hbox#coerce, false, QF_COMBO wcombo#entry)

  | Q_MP3_BITRATE (label, v) ->
      let hbox = GPack.hbox () in
      let wl = GMisc.label ~text: (label^":")
          ~packing: (hbox#pack ~padding: 2 ~expand: false) ()
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
      let wl = GMisc.label ~text: (label^":")
          ~packing: (hbox#pack ~padding: 2 ~expand: false) ()
      in
      let we = GEdit.entry ~width: 100 ~packing: (hbox#pack ~padding: 2 ~expand: false) () in
      let wcombo = GEdit.combo
          ~popdown_strings: ["" ; "Mo"; "ko"]
          ~value_in_list: false
          ~ok_if_empty: true
          ~width: 60
          ~packing: (hbox#pack ~padding: 2 ~expand: false ~fill: false) () 
      in
      we#set_text v;
      (hbox#coerce, false, QF_MINSIZE (we, wcombo#entry))

  | Q_MAXSIZE (label, v) ->
      let hbox = GPack.hbox () in
      let wl = GMisc.label ~text: (label^":")
          ~packing: (hbox#pack ~padding: 2 ~expand: false) ()
      in
      let we = GEdit.entry ~width: 100 ~packing: (hbox#pack ~padding: 2 ~expand: false) () in
      let wcombo = GEdit.combo
          ~popdown_strings: ["" ; "Mo"; "ko"]
          ~value_in_list: false
          ~ok_if_empty: true
          ~width: 60
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
          Int64.to_string
            (Int64.mul (Int64.of_string we#text) (Gui_misc.unit_of_string we2#text))
        with _ -> ""
      in
      Q_MINSIZE ("", size)
  | QF_MAXSIZE (we,we2) -> 
      let size = 
        try Int64.to_string
            (Int64.mul (Int64.of_string we#text) (Gui_misc.unit_of_string we2#text))
        with _ -> ""
      in
      Q_MAXSIZE ("", size)
      
  | QF_COMBO we -> Q_COMBO ("", we#text, [])
      
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
      wtool2#set_style st;
    
    initializer
      f_submit := (fun () -> self#submit ());
      box_fields#pack w;
      ignore (wchk_show#connect#clicked
          (fun () -> hide_or_show wchk_show#active form));

      Gui_misc.insert_buttons wtool1 wtool2 
        ~text: (gettext M.submit)
      ~tooltip: (gettext M.submit)
      ~icon: (M.o_xpm_submit_search)
      ~callback: self#submit
        ();
      Gui_misc.insert_buttons wtool1 wtool2
        ~text: (gettext M.local_search)
      ~tooltip: (gettext M.local_search)
      ~icon: (M.o_xpm_local_search)
          ~callback: self#local
        ();
      Gui_misc.insert_buttons wtool1 wtool2
          ~text: (gettext M.subscribe)
        ~tooltip: (gettext M.subscribe)
        ~icon: (M.o_xpm_download)
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
      Gui_misc.insert_buttons wtool1 wtool2
          ~text: (gettext M.submit)
        ~tooltip: (gettext M.submit)
      ~icon: (M.o_xpm_submit_search)
          ~callback: self#submit
          ()
      

end


class paned () =
  object(self)
    inherit Gui_queries_base.paned ()

(** Associations (numéro de query de gui, boite affichant les résultats) *)
    val mutable results = ([] : (int * 
          (Gui_results.search_result_box * GPack.box)) list)
    
    val mutable queries_box = ([] : box list)
    val mutable static_queries_box = ([] : url_box list)
    val mutable static_results = ([] : Gui_cdget.url_results list)
      
    val mutable wnote_main = GPack.notebook () 
    val mutable wnote_results = GPack.notebook () 
    method set_wnote_results w = wnote_results <- w
    method set_wnote_main w = wnote_main <- w
    
    method clear = 
      List.iter (fun box -> box#coerce#destroy ()) queries_box;
      queries_box <- [];
      results <- [] ;
    
    
    method close_query num forget () =
      try
        if forget then  begin
            let (box_res, vb) = List.assoc num results in
            vb#destroy ();
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
      let vbox = GPack.vbox () in
      let box_res = new Gui_results.search_result_box s.GuiTypes.search_num () in
      vbox#pack ~expand: true box_res#coerce;
      let wb_close = GButton.button ~label: (gettext M.close_search)
        ~packing: (vbox#pack ~expand: false)
        ()
      in
      let wb_stop = GButton.button ~label: (gettext M.stop_search)
        ~packing: (vbox#pack ~expand: false)
        ()
      in
      ignore (wb_close#connect#clicked (self#close_query s.GuiTypes.search_num true));
      ignore (wb_stop#connect#clicked (self#close_query s.GuiTypes.search_num false));
      wnote_results#insert_page ~tab_label: wl#coerce ~pos: 0 vbox#coerce;
      wnote_results#goto_page 0;
      wnote_main#goto_page 3;
      box_res#set_tb_style !!Gui_options.toolbars_style;

(* only the last result box must have an "extended search" button *)
(*      List.iter (fun (_,(b,_)) -> b#remove_extend_search_button) results; *)
      
      results <- (s.GuiTypes.search_num, (box_res, vbox)) :: results
          
    method submit_url_search result_box qe =
      let desc = Gui_misc.description_of_query qe in
      let wl = GMisc.label ~text: desc () in
      let vbox = GPack.vbox () in
      let box_res = result_box qe in
      static_results <- box_res :: static_results;
      vbox#pack ~expand: true box_res#coerce;
      let wb_close = GButton.button ~label: (gettext M.close_search)
        ~packing: (vbox#pack ~expand: false)
        ()
        in
      ignore (wb_close#connect#clicked (fun _ ->
            static_results <- List2.removeq box_res static_results;
            vbox#destroy ()
        ));
      wnote_results#insert_page ~tab_label: wl#coerce ~pos: 0 vbox#coerce;
      wnote_results#goto_page 0;
      wnote_main#goto_page 3;

(* only the last result box must have an "extended search" button *)
(*      List.iter (fun (_,(b,_)) -> b#remove_extend_search_button) results; *)
    
    method set_tb_style st = 
      List.iter (fun b -> b#set_tb_style st) queries_box;
      List.iter (fun b -> b#set_tb_style st) static_queries_box;
      List.iter (fun (_,(b,_)) -> b#set_tb_style st) results;
      List.iter (fun b -> b#set_tb_style st) static_results

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
        wnote_queries#insert_page ~pos: 0 ~tab_label: wl#coerce b#coerce;
        wnote_queries#goto_page 0;
        b#set_tb_style !!Gui_options.toolbars_style;
        queries_box <- b :: queries_box
      in
      List.iter f l

    initializer
    
    let b = new url_box Gui_cdget.ShareReactor.query
        (self#submit_url_search (new Gui_cdget.ShareReactor.results)) in
    static_queries_box <- b :: static_queries_box;
    let wl = GMisc.label ~text: "ShareReactor Search" () in
    wnote_queries#append_page ~tab_label: wl#coerce b#coerce;
    
    let b = new url_box Gui_cdget.Jigle.query
        (self#submit_url_search (new Gui_cdget.Jigle.results)) in
    static_queries_box <- b :: static_queries_box;
    let wl = GMisc.label ~text: "Jigle Search" () in
    wnote_queries#append_page ~tab_label: wl#coerce b#coerce;
    
    let b = new url_box Gui_cdget.FreeDB.query 
        (self#submit_url_search (new Gui_cdget.FreeDB.results)) in
    static_queries_box <- b :: static_queries_box;
    let wl = GMisc.label ~text: "FreeDB Search" () in
    wnote_queries#append_page ~tab_label: wl#coerce b#coerce;
    
    let b = new url_box (Q_KEYWORDS ("Title", ""))
        (self#submit_url_search (new Gui_cdget.IMDB.results)) in
    static_queries_box <- b :: static_queries_box;
    let wl = GMisc.label ~text: "IMDB Search" () in
    wnote_queries#append_page ~tab_label: wl#coerce b#coerce;

  end
