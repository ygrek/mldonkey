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

(* The users window of MLgui *)


open GuiTypes2
open CommonTypes

open GuiTools
open GuiGlobal
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


(*************************************************************************)
(*                                                                       *)
(*                         message from the core                         *)
(*                                                                       *)
(*************************************************************************)

let hashtbl_update_result r res =
  res.res_format <- U.simple_utf8_of r.result_format;
  res.res_type <- U.simple_utf8_of r.result_type;
  res.res_duration <- Mi.duration_of_tags r.result_tags;
  res.res_codec <- Mi.codec_of_tags r.result_tags;
  res.res_bitrate <- Mi.bitrate_of_tags r.result_tags;
  res.res_availability <- Mi.availability_of_tags r.result_tags;
  res.res_completesources <- Mi.completesources_of_tags r.result_tags;
  res.res_tags <- Mi.tags_to_string r.result_tags;
  res.res_comment <- U.utf8_of r.result_comment;
  res.res_color <- Mi.color_of_result (Mi.availability_of_tags r.result_tags) r.result_done

let result_info (r : result_info) =
  try
    let res = Hashtbl.find G.results r.result_num in
    hashtbl_update_result r res
  with _ ->
    begin
      let net_num = Mi.neworknum_from_uids r.result_uids in
      let name = Mi.result_first_name r.result_names in
      let res =
        {
         res_num             = r.result_num;
         res_network         = net_num;
         res_name            = name;
         res_uid             = Mi.uid_list_to_string r.result_uids;
         res_size            = r.result_size;
         res_format          = U.simple_utf8_of r.result_format;
         res_type            = U.simple_utf8_of r.result_type;
         res_duration        = Mi.duration_of_tags r.result_tags;
         res_codec           = Mi.codec_of_tags r.result_tags;
         res_bitrate         = Mi.bitrate_of_tags r.result_tags;
         res_availability    = Mi.availability_of_tags r.result_tags;
         res_completesources = Mi.completesources_of_tags r.result_tags;
         res_tags            = Mi.tags_to_string r.result_tags;
         res_comment         = U.utf8_of r.result_comment;
         res_color           = Mi.color_of_result (Mi.availability_of_tags r.result_tags) r.result_done;
         res_has_query       = [];
         res_network_pixb    = Mi.network_pixb net_num ~size:A.SMALL ();
         res_name_pixb       = Mi.file_type_of_name name ~size:A.SMALL;
        }
      in
      Hashtbl.add G.results res.res_num res;
    end

(*************************************************************************)
(*                                                                       *)
(*                         result_num                                    *)
(*                                                                       *)
(*************************************************************************)

let result_num key =
  try int_of_string key with _ -> raise Not_found

(*************************************************************************)
(*                                                                       *)
(*                         result_of_key                                 *)
(*                                                                       *)
(*************************************************************************)

let result_of_key key =
  try
    let num = result_num key in
    Hashtbl.find G.results num
  with _ -> raise Not_found

(*************************************************************************)
(*                                                                       *)
(*                         keys_to_results                               *)
(*                                                                       *)
(*************************************************************************)

let keys_to_results keys =
  let l = ref [] in
  List.iter (fun k ->
    try
      let s = result_of_key k in
      l := s :: !l
    with _ -> ()) keys;
  !l

(*************************************************************************)
(*                                                                       *)
(*                         result_key                                    *)
(*                                                                       *)
(*************************************************************************)

let result_key result_num =
  Printf.sprintf "%d" result_num

(*************************************************************************)
(*                                                                       *)
(*                         message to the core                           *)
(*                                                                       *)
(*************************************************************************)

let download sel () =
  let l = keys_to_results sel in
  List.iter (fun r -> 
    GuiCom.send (Download_query ([r.res_name], r.res_num, false))
  ) l

let force_download sel () =
  let l = keys_to_results sel in
  List.iter (fun r -> 
    GuiCom.send (Download_query ([r.res_name], r.res_num, true))
  ) l

(*************************************************************************)
(*                                                                       *)
(*                         result_menu                                   *)
(*                                                                       *)
(*************************************************************************)

let result_menu sel =
  match sel with
      [] -> []
    | _ ->
          [
           `I (!M.qT_me_download, download sel);
           `I (!M.qT_me_force_download, force_download sel);
          ]

(*************************************************************************)
(*                                                                       *)
(*                         filter_result                                 *)
(*                                                                       *)
(*************************************************************************)

let filter_result k =
  try
    let r = result_of_key k in
    not (List.memq r.res_network !G.networks_filtered)
  with _ -> true

module ResultList(Res:

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

      val columns      : (result_column * float) list Options.option_record
      val view_context : GPango.context option ref
      val module_name  : string

    end) = 
  (struct

(*************************************************************************)
(*                                                                       *)
(*                         Templates                                     *)
(*                                                                       *)
(*************************************************************************)

  module R = GuiTemplates.Gview(struct

    module Column = GuiColumns.Result

    type item = res_info

    let columns = Res.columns
    let get_key = (fun r -> result_key r.res_num)
    let module_name = Res.module_name

  end)

  class g_result () =
    let result_cols            = new GTree.column_list in
    let result_network_str     = result_cols#add Gobject.Data.string in
    let result_network_pixb    = result_cols#add Gobject.Data.gobject_option in
    let result_name            = result_cols#add Gobject.Data.string in
    let result_name_pixb       = result_cols#add Gobject.Data.gobject_option in
    let result_uid             = result_cols#add Gobject.Data.string in
    let result_size_str        = result_cols#add Gobject.Data.string in
    let result_format          = result_cols#add Gobject.Data.string in
    let result_type            = result_cols#add Gobject.Data.string in
    let result_duration        = result_cols#add Gobject.Data.string in
    let result_codec           = result_cols#add Gobject.Data.string in
    let result_bitrate         = result_cols#add Gobject.Data.int in
    let result_avail           = result_cols#add Gobject.Data.int in
    let result_completesources = result_cols#add Gobject.Data.int in
    let result_comment         = result_cols#add Gobject.Data.string in
    let result_tags            = result_cols#add Gobject.Data.string in
    let result_color           = result_cols#add Gobject.Data.string in
    object (self)

      inherit R.g_list result_cols


        val res_queue = Queue.create ()

(*************************************************************************)
(*                                                                       *)
(*                         from_item                                     *)
(*                                                                       *)
(*************************************************************************)

      method add_result_while_idle () =
        ignore (Glib.Idle.add (fun _ ->
          try
            let (res, f) = Queue.take res_queue in
            self#add_item res ?f ();
            true
          with Queue.Empty -> false))

      method add (r : res_info) ?f () =
        let is_empty = Queue.is_empty res_queue in
        Queue.add (r, f) res_queue;
        if is_empty then self#add_result_while_idle ()

(*************************************************************************)
(*                                                                       *)
(*                         from_item                                     *)
(*                                                                       *)
(*************************************************************************)

      method from_item row (r : res_info) =
        store#set ~row ~column:result_network_str (Mi.network_name r.res_network);
        store#set ~row ~column:result_network_pixb r.res_network_pixb;
        store#set ~row ~column:result_name r.res_name;
        store#set ~row ~column:result_name_pixb r.res_name_pixb;
        store#set ~row ~column:result_uid r.res_uid;
        store#set ~row ~column:result_size_str (Mi.size_of_int64 r.res_size);
        store#set ~row ~column:result_format r.res_format;
        store#set ~row ~column:result_type r.res_type;
        store#set ~row ~column:result_duration r.res_duration;
        store#set ~row ~column:result_codec r.res_codec;
        store#set ~row ~column:result_bitrate r.res_bitrate;
        store#set ~row ~column:result_avail r.res_availability;
        store#set ~row ~column:result_completesources r.res_completesources;
        store#set ~row ~column:result_comment r.res_comment;
        store#set ~row ~column:result_tags r.res_tags;
        store#set ~row ~column:result_color r.res_color

(*************************************************************************)
(*                                                                       *)
(*                         from_new_item                                 *)
(*                                                                       *)
(*************************************************************************)

      method from_new_item row r _ =
        self#from_item row r

(*************************************************************************)
(*                                                                       *)
(*                         content                                       *)
(*                                                                       *)
(*************************************************************************)

      method content (col : GTree.view_column) c =
        let autosize = match col#sizing with `AUTOSIZE -> true | _ -> false in
        match c with
            Col_result_name ->
              begin
                if !!O.gtk_look_use_icons
                  then begin
                    let renderer = GTree.cell_renderer_pixbuf [`XALIGN 0.; `XPAD 4] in
                    col#pack ~expand:false renderer;
                    col#add_attribute renderer "pixbuf" result_name_pixb
                  end;
                if autosize
                  then begin
                    let renderer = GTree.cell_renderer_text [`XALIGN 0.] in
                    col#pack ~expand:false renderer;
                    col#add_attribute renderer "text" result_name;
                    col#add_attribute renderer "foreground" result_color;
                    col#pack ~expand:false renderer;
                  end else begin
                    let renderer = GTree.cell_renderer_text [`XALIGN 0.] in
                    col#pack ~expand:false renderer;
                    col#set_cell_data_func renderer
                      (fun model row ->
                       match !Res.view_context with
                         Some context when col#width > 0 ->
                           begin
                             let width =
                               if !!O.gtk_look_use_icons
                                 then (col#width - 4 - !!O.gtk_look_lists_icon_size) - 4 * !G.char_width
                                 else col#width - 4 * !G.char_width
                             in
                             let name = model#get ~row ~column:result_name in
                             let s = GuiTools.fit_string_to_pixels name ~context ~pixels:width in
                             renderer#set_properties [ `TEXT s ]
                           end
                       | _ -> renderer#set_properties [ `TEXT "" ]
                      );
                    col#add_attribute renderer "foreground" result_color;
                  end
              end

          | Col_result_uid ->
              begin
                let renderer = GTree.cell_renderer_text [`XALIGN 0.] in
                col#pack renderer;
                col#add_attribute renderer "text" result_uid;
                col#add_attribute renderer "foreground" result_color
              end

          | Col_result_size ->
              begin
                let renderer = GTree.cell_renderer_text [`XALIGN 1.] in
                col#pack renderer;
                col#add_attribute renderer "text" result_size_str;
                col#add_attribute renderer "foreground" result_color
              end

          | Col_result_format ->
              begin
                let renderer = GTree.cell_renderer_text [`XALIGN 1.] in
                col#pack renderer;
                col#add_attribute renderer "text" result_format;
                col#add_attribute renderer "foreground" result_color
              end

          | Col_result_type ->
              begin
                let renderer = GTree.cell_renderer_text [`XALIGN 0.] in
                col#pack renderer;
                col#add_attribute renderer "text" result_type;
                col#add_attribute renderer "foreground" result_color
              end

          | Col_result_duration ->
              begin
                let renderer = GTree.cell_renderer_text [`XALIGN 1.] in
                col#pack renderer;
                col#add_attribute renderer "text" result_duration;
                col#add_attribute renderer "foreground" result_color
              end

          | Col_result_codec ->
              begin
                let renderer = GTree.cell_renderer_text [`XALIGN 1.] in
                col#pack renderer;
                col#add_attribute renderer "text" result_codec;
                col#add_attribute renderer "foreground" result_color
              end

          | Col_result_bitrate ->
              begin
                let renderer = GTree.cell_renderer_text [`XALIGN 1.] in
                col#pack renderer;
                col#add_attribute renderer "text" result_bitrate;
                col#add_attribute renderer "foreground" result_color
              end

          | Col_result_availability ->
              begin
                let renderer = GTree.cell_renderer_text [`XALIGN 1.] in
                col#pack renderer;
                col#add_attribute renderer "text" result_avail;
                col#add_attribute renderer "foreground" result_color
              end

          | Col_result_comment ->
              begin
                let renderer = GTree.cell_renderer_text [`XALIGN 0.] in
                col#pack renderer;
                col#add_attribute renderer "text" result_comment;
                col#add_attribute renderer "foreground" result_color
              end

          | Col_result_network ->
              begin
                if !!O.gtk_look_use_icons
                  then begin
                    let renderer = GTree.cell_renderer_pixbuf [`XALIGN 0.] in
                    col#pack renderer;
                    col#add_attribute renderer "pixbuf" result_network_pixb;
                  end else begin
                    let renderer = GTree.cell_renderer_text [] in
                    col#pack renderer;
                    col#add_attribute renderer "text" result_network_str;
                    col#add_attribute renderer "foreground" result_color
                  end
              end
          | Col_result_completesources -> 
              begin
                let renderer = GTree.cell_renderer_text [`XALIGN 1.] in
                col#pack renderer;
                col#add_attribute renderer "text" result_completesources;
                col#add_attribute renderer "foreground" result_color
              end

          | Col_result_tags ->
              begin
                let renderer = GTree.cell_renderer_text [`XALIGN 0.] in
                col#pack renderer;
                col#add_attribute renderer "text" result_tags;
                col#add_attribute renderer "foreground" result_color
              end

(*************************************************************************)
(*                                                                       *)
(*                         sort_items                                    *)
(*                                                                       *)
(*************************************************************************)

      method sort_items c k1 k2 =
        try
          let r1 = result_of_key k1 in
          let r2 = result_of_key k2 in
          match c with
            Col_result_name -> compare (String.lowercase r1.res_name) (String.lowercase r2.res_name)
          | Col_result_uid -> compare r1.res_uid r2.res_uid
          | Col_result_size -> compare r1.res_size r2.res_size
          | Col_result_format -> compare r1.res_format r2.res_format
          | Col_result_type -> compare r1.res_type r2.res_type
          | Col_result_tags -> compare r1.res_tags r2.res_tags
          | Col_result_duration -> compare r1.res_duration r2.res_duration
          | Col_result_codec -> compare r1.res_codec r2.res_codec
          | Col_result_bitrate -> compare r1.res_bitrate r2.res_bitrate
          | Col_result_availability -> compare r1.res_availability r2.res_availability
          | Col_result_comment -> compare r1.res_comment r2.res_comment
          | Col_result_network -> compare r1.res_network r2.res_network
          | Col_result_completesources -> compare r1.res_completesources r2.res_completesources
        with _ -> 0

(*************************************************************************)
(*                                                                       *)
(*                         force_update_icons                            *)
(*                                                                       *)
(*************************************************************************)

    method force_update_icons () =
      let f r row =
        r.res_network_pixb <- Mi.network_pixb r.res_num ~size:A.SMALL ();
        r.res_name_pixb <- Mi.file_type_of_name r.res_name ~size:A.SMALL;
        store#set ~row ~column:result_network_pixb r.res_network_pixb;
        store#set ~row ~column:result_name_pixb r.res_name_pixb;
      in
      List.iter (fun k ->
        try
          let r = result_of_key k in
          let row = self#find_row k in
          Gaux.may ~f:(f r) row
        with _ -> ()
      ) (self#all_items ())

    end

(*************************************************************************)
(*                                                                       *)
(*                         shortcut                                      *)
(*                                                                       *)
(*************************************************************************)

    let treeview = R.treeview

  end)

