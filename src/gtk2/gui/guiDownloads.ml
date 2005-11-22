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

(* The downloads window of MLgui *)

open GuiTypes2
open GuiTypes
open CommonTypes
open GraphTypes

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
module H = GuiHtml

let (!!) = Options.(!!)
let (=:=) = Options.(=:=)
let (<:>) = GuiTools.(<:>)

let verbose = O.gtk_verbose_downloads

let lprintf' fmt =
  Printf2.lprintf ("GuiDownloads: " ^^ fmt)


(*************************************************************************)
(*                                                                       *)
(*                         Global tables                                 *)
(*                                                                       *)
(*************************************************************************)

let (downloaders : int list ref) = ref []
let current_selection = ref []

(*************************************************************************)
(*                                                                       *)
(*                         Global variables                              *)
(*                                                                       *)
(*************************************************************************)

let downloaders_timerID = ref (GMain.Timeout.add ~ms:2000 ~callback:(fun _ -> true))
let (view_context : GPango.context option ref) = ref None
let (stat_boxes : (GPack.box * GPack.box * GMisc.label * GRange.progress_bar) option ref) = ref None
let interested_in_sources = ref false
let expanded_rows = ref 0

let dummy_source =
  {
     source_num = (-1);
     source_network = 0;
     source_kind = Indirect_location ("", Md4.null);
     source_state = RemovedHost;
     source_type = client_initialized_tag;
     source_tags = [];
     source_name = " ";
     source_files = None;
     source_rating = 0;
     source_chat_port = 0;
     source_connect_time = 0;
     source_last_seen = 0.;
     source_software = "";
     source_downloaded = Int64.zero;
     source_uploaded = Int64.zero;
     source_upload_rate = 0.;
     source_download_rate = 0.;
     source_upload = None;
     source_has_upload = source_only;
     source_availability = [];
     source_files_requested = [];
  }

(*************************************************************************)
(*                                                                       *)
(*                         is_file                                       *)
(*                                                                       *)
(*************************************************************************)

let is_file key = not (String.contains key ':')

(*************************************************************************)
(*                                                                       *)
(*                         is_source                                     *)
(*                                                                       *)
(*************************************************************************)

let is_source key = (snd (String2.cut_at key ':') <> "")

(*************************************************************************)
(*                                                                       *)
(*                         file_num                                      *)
(*                                                                       *)
(*************************************************************************)

let file_num key =
  if is_file key || is_source key
  then begin
    let t = fst (String2.cut_at key ':') in
    int_of_string t
  end else raise Not_found

(*************************************************************************)
(*                                                                       *)
(*                         source_num                                    *)
(*                                                                       *)
(*************************************************************************)

let source_num key =
  if is_source key
  then begin
    let t = snd (String2.cut_at key ':') in
    int_of_string t
  end else raise Not_found

(*************************************************************************)
(*                                                                       *)
(*                         file_of_key                                   *)
(*                                                                       *)
(*************************************************************************)

let file_of_key key =
  try
    let num = file_num key in
    Hashtbl.find G.files num
  with _ -> raise Not_found

(*************************************************************************)
(*                                                                       *)
(*                         source_of_key                                 *)
(*                                                                       *)
(*************************************************************************)

let source_of_key key =
  try
    let num = source_num key in
    Hashtbl.find G.sources num
  with _ -> raise Not_found

(*************************************************************************)
(*                                                                       *)
(*                         keys_to_files                                 *)
(*                                                                       *)
(*************************************************************************)

let keys_to_files keys =
  let l = ref [] in
  List.iter (fun k ->
    try
      if (is_file k) then
        let f = file_of_key k in
        if not (List.memq f !l) then
          l := f :: !l
    with _ -> ()) keys;
  !l

(*************************************************************************)
(*                                                                       *)
(*                         keys_to_sources                               *)
(*                                                                       *)
(*************************************************************************)

let keys_to_sources keys =
  let l = ref [] in
  List.iter (fun k ->
    try
      let s = source_of_key k in
      if not (List.memq s !l)
        then l := s :: !l
    with _ -> ()) keys;
  !l

(*************************************************************************)
(*                                                                       *)
(*                         file_key                                      *)
(*                                                                       *)
(*************************************************************************)

let file_key file_num =
  Printf.sprintf "%d" file_num

(*************************************************************************)
(*                                                                       *)
(*                         source_key                                    *)
(*                                                                       *)
(*************************************************************************)

let source_key file_num source_num =
  Printf.sprintf "%d:%d" file_num source_num

(*************************************************************************)
(*                                                                       *)
(*                         dummy_source_key                              *)
(*                                                                       *)
(*************************************************************************)

let dummy_source_key file_num =
  source_key file_num dummy_source.source_num

(*************************************************************************)
(*                                                                       *)
(*                         Templates                                     *)
(*                                                                       *)
(*************************************************************************)

let get_key =
  (fun i ->
     match i with
         File f -> (Printf.sprintf "%d" f.g_file_num)
       | Source (s, file_num) -> (Printf.sprintf "%d:%d" file_num s.source_num)
  )

module Downloads = GuiTemplates.Gview(struct

  module Column = GuiColumns.File

  type item = item_info

  let columns = O.downloads_columns
  let get_key = get_key
  let module_name = "Downloads"

end)

class g_download () =
  let download_cols              = new GTree.column_list in
  let download_network           = download_cols#add Gobject.Data.string in
  let download_network_pixb      = download_cols#add Gobject.Data.gobject_option in
  let download_name              = download_cols#add Gobject.Data.string in
  let download_name_pixb         = download_cols#add Gobject.Data.gobject_option in
  let download_uid               = download_cols#add Gobject.Data.string in
  let download_size              = download_cols#add Gobject.Data.string in
  let download_downloaded        = download_cols#add Gobject.Data.string in
  let download_percent           = download_cols#add Gobject.Data.string in
  let download_sources           = download_cols#add Gobject.Data.string in
  let download_state             = download_cols#add Gobject.Data.string in
  let download_availability      = download_cols#add Gobject.Data.string in
  let download_availability_pixb = download_cols#add Gobject.Data.gobject_option in
  let download_download_rate     = download_cols#add Gobject.Data.string in
  let download_format            = download_cols#add Gobject.Data.string in
  let download_age               = download_cols#add Gobject.Data.string in
  let download_last_seen         = download_cols#add Gobject.Data.string in
  let download_eta_inst          = download_cols#add Gobject.Data.string in
  let download_eta_average       = download_cols#add Gobject.Data.string in
  let download_priority          = download_cols#add Gobject.Data.string in
  let download_comment           = download_cols#add Gobject.Data.string in
  object (self)

    inherit Downloads.g_tree download_cols

(*************************************************************************)
(*                                                                       *)
(*                         from_item                                     *)
(*                                                                       *)
(*************************************************************************)

    method from_item row (i : GuiTypes2.item_info) =
      match i with
          File f ->
            begin
              store#set ~row ~column:download_network (Mi.network_name f.g_file_network);
              store#set ~row ~column:download_network_pixb (Mi.network_pixb f.g_file_network ~size:A.SMALL ());
              store#set ~row ~column:download_name (U.utf8_of f.g_file_name);
              store#set ~row ~column:download_name_pixb (Mi.file_type_of_name f.g_file_name ~size:A.SMALL);
              store#set ~row ~column:download_uid (Mi.uid_list_to_string f.g_file_uids);
              store#set ~row ~column:download_size (Mi.size_of_int64 f.g_file_size);
              store#set ~row ~column:download_downloaded (Mi.size_of_int64 f.g_file_downloaded);
              store#set ~row ~column:download_percent (Mi.get_percent_of f.g_file_downloaded f.g_file_size);
              store#set ~row ~column:download_sources (Printf.sprintf "(%d / %d)" f.g_file_active_sources f.g_file_all_sources);
              store#set ~row ~column:download_state (Mi.string_of_file_state f.g_file_state f.g_file_download_rate);
              let availability = Mi.main_availability_of f.g_file_network f.g_file_availability in
              store#set ~row ~column:download_availability (Mi.string_of_availability availability f.g_file_chunks);
              store#set ~row ~column:download_availability_pixb (Mi.availability_bar availability f.g_file_chunks true);
              store#set ~row ~column:download_download_rate (Mi.rate_to_string f.g_file_download_rate);
              store#set ~row ~column:download_format (Mi.format_to_string f.g_file_format);
              store#set ~row ~column:download_age (Mi.time_to_string f.g_file_age);
              store#set ~row ~column:download_last_seen (Mi.time_to_string f.g_file_last_seen);
              store#set ~row ~column:download_eta_inst (Mi.calc_eta_inst f.g_file_size f.g_file_downloaded f.g_file_download_rate);
              store#set ~row ~column:download_eta_average (Mi.calc_eta_average f.g_file_size f.g_file_downloaded f.g_file_age);
              store#set ~row ~column:download_priority (Mi.priority_to_string f.g_file_priority);
              store#set ~row ~column:download_comment (U.utf8_of f.g_file_comment)
            end
        | Source (s, file_num) ->
            try
              let f = Hashtbl.find G.files file_num in
              if s.source_num = (-1)
                then begin
                  store#set ~row ~column:download_name s.source_name;
                  store#set ~row ~column:download_availability_pixb None;
                end else begin
                  let availability =
                    try
                      List.assoc file_num s.source_availability
                    with _ -> ""
                  in
                  store#set ~row ~column:download_network (Mi.network_name s.source_network);
                  store#set ~row ~column:download_network_pixb (Mi.network_pixb s.source_network ~size:A.SMALL ());
                  store#set ~row ~column:download_name s.source_name;
                  store#set ~row ~column:download_name_pixb (Mi.source_type_to_icon s.source_type ~size:A.SMALL);
                  store#set ~row ~column:download_downloaded (Mi.size_of_int64 s.source_downloaded);
                  store#set ~row ~column:download_percent s.source_software;
                  store#set ~row ~column:download_state (Mi.string_of_state s.source_state file_num);
                  store#set ~row ~column:download_availability (Mi.string_of_availability availability f.g_file_chunks);
                  store#set ~row ~column:download_availability_pixb (Mi.availability_bar availability f.g_file_chunks false);
                  store#set ~row ~column:download_download_rate (Mi.rate_to_string s.source_download_rate);
                end
            with _ -> ()

(*************************************************************************)
(*                                                                       *)
(*                         from_new_item                                 *)
(*                                                                       *)
(*************************************************************************)

    method from_new_item (row : Gtk.tree_iter) (i : GuiTypes2.item_info) (i_new : GuiTypes2.item_info) =
      match (i, i_new) with
          (File f, File f_new) ->
            begin
              if f.g_file_comment <> f_new.g_file_comment
                then begin
                  store#set ~row ~column:download_comment (U.utf8_of f_new.g_file_comment)
                end;
              if f.g_file_name <> f_new.g_file_name
                then begin
                  store#set ~row ~column:download_name (U.utf8_of f_new.g_file_name);
                  if (Mi.extension_of f.g_file_name) <> (Mi.extension_of f_new.g_file_name)
                    then begin
                      store#set ~row ~column:download_name_pixb (Mi.file_type_of_name f_new.g_file_name ~size:A.SMALL);
                    end
                end;
              if f.g_file_uids <> f_new.g_file_uids
                then begin
                  store#set ~row ~column:download_uid (Mi.uid_list_to_string f.g_file_uids);
                end;
              if (f.g_file_size, f.g_file_downloaded, f.g_file_download_rate, f.g_file_age) <>
                 (f_new.g_file_size, f_new.g_file_downloaded, f_new.g_file_download_rate, f_new.g_file_age)
                then begin
                  store#set ~row ~column:download_downloaded (Mi.size_of_int64 f_new.g_file_downloaded);
                  store#set ~row ~column:download_percent (Mi.get_percent_of f_new.g_file_downloaded f_new.g_file_size);
                  store#set ~row ~column:download_size (Mi.size_of_int64 f_new.g_file_size);
                  store#set ~row ~column:download_age (Mi.time_to_string f_new.g_file_age);
                  store#set ~row ~column:download_download_rate (Mi.rate_to_string f_new.g_file_download_rate);
                  store#set ~row ~column:download_eta_inst (Mi.calc_eta_inst f_new.g_file_size f_new.g_file_downloaded
                                                                             f_new.g_file_download_rate);
                  store#set ~row ~column:download_eta_average (Mi.calc_eta_average f_new.g_file_size
                                                                                   f_new.g_file_downloaded f_new.g_file_age);
                end;
              if (f.g_file_state, f.g_file_download_rate) <> (f_new.g_file_state, f_new.g_file_download_rate)
                then begin
                  store#set ~row ~column:download_state (Mi.string_of_file_state f_new.g_file_state f_new.g_file_download_rate);
                end;
              let availability = Mi.main_availability_of f.g_file_network f.g_file_availability in
              let availability_new = Mi.main_availability_of f_new.g_file_network f_new.g_file_availability in
              if (availability, f.g_file_chunks) <> (availability_new, f_new.g_file_chunks)
                then begin
                  store#set ~row ~column:download_availability (Mi.string_of_availability availability_new f_new.g_file_chunks);
                  store#set ~row ~column:download_availability_pixb (Mi.availability_bar availability_new f_new.g_file_chunks true);
                end;
              if f.g_file_format <> f_new.g_file_format
                then begin
                  store#set ~row ~column:download_format (Mi.format_to_string f_new.g_file_format);
                end;
              if f.g_file_last_seen <> f_new.g_file_last_seen
                then begin
                  store#set ~row ~column:download_last_seen (Mi.time_to_string f_new.g_file_last_seen);
                end;
              if f.g_file_priority <> f_new.g_file_priority
                then begin
                  store#set ~row ~column:download_priority (Mi.priority_to_string f_new.g_file_priority);
                end;
              if (f.g_file_active_sources, f.g_file_all_sources) <> (f_new.g_file_active_sources, f_new.g_file_all_sources)
                then begin
                  store#set ~row ~column:download_sources (Printf.sprintf "(%d / %d)" f_new.g_file_active_sources f_new.g_file_all_sources);
                end
            end

        | (Source (s, file_num), Source (s_new, file_num_new)) when file_num = file_num_new ->
           (try
              let f = Hashtbl.find G.files file_num in
              if s.source_num <> (-1)
                then begin
                  if s.source_name <> s_new.source_name
                    then begin
                      store#set ~row ~column:download_name s_new.source_name;
                    end;
                  if s.source_type <> s_new.source_type
                    then begin
                      store#set ~row ~column:download_name_pixb (Mi.source_type_to_icon s_new.source_type ~size:A.SMALL);
                    end;
                  if s.source_downloaded <> s_new.source_downloaded
                    then begin
                      store#set ~row ~column:download_downloaded (Mi.size_of_int64 s_new.source_downloaded);
                    end;
                  if s.source_state <> s_new.source_state
                    then begin
                      store#set ~row ~column:download_state (Mi.string_of_state s_new.source_state file_num);
                    end;
                  if s.source_software <> s_new.source_software
                    then begin
                      store#set ~row ~column:download_percent s_new.source_software;
                    end;
                  let availability =
                    try
                      List.assoc file_num s.source_availability
                    with _ -> ""
                  in
                  let new_availability =
                    try
                      List.assoc file_num s_new.source_availability
                    with _ -> ""
                  in
                  if availability <> new_availability
                    then begin
                      store#set ~row ~column:download_availability (Mi.string_of_availability new_availability f.g_file_chunks);
                      store#set ~row ~column:download_availability_pixb (Mi.availability_bar new_availability f.g_file_chunks false)
                    end;
                  if s_new.source_download_rate <> s.source_download_rate
                    then begin
                      store#set ~row ~column:download_download_rate (Mi.rate_to_string s_new.source_download_rate);
                    end
                end
            with _ -> ())

        | _ -> ()

(*************************************************************************)
(*                                                                       *)
(*                         content                                       *)
(*                                                                       *)
(*************************************************************************)

    method content (col : GTree.view_column) c =
      let autosize = match col#sizing with `AUTOSIZE -> true | _ -> false in
      match c with
          Col_file_name ->
            begin
              if !!O.gtk_look_use_icons
                then begin
                  let renderer = GTree.cell_renderer_pixbuf [`XALIGN 0.;`XPAD 4] in
                  col#pack ~expand:false renderer;
                  col#add_attribute renderer "pixbuf" download_name_pixb
                end;
              let renderer = GTree.cell_renderer_text [`XALIGN 0.] in
              col#pack ~expand:false renderer;
              if autosize
                then col#add_attribute renderer "text" download_name
                else col#set_cell_data_func renderer
                  (fun model row ->
                     match !view_context with
                       Some context when col#width > 0 ->
                         begin
                           let width =
                             if !!O.gtk_look_use_icons
                               then (col#width - 4 - !!O.gtk_look_lists_icon_size) - 4 * !G.char_width
                               else col#width - 4 * !G.char_width
                           in
                           let name = model#get ~row ~column:download_name in
                           let s = GuiTools.fit_string_to_pixels name ~context ~pixels:width in
                           renderer#set_properties [ `TEXT s ];
                           try
                             let key = self#find_model_key row in
                             if (is_file key) then begin
                               renderer#set_properties [ `EDITABLE true ];
                               ignore (renderer#connect#edited ~callback:
                                 (fun path name ->
                                    try
                                      let iter = self#get_iter path in
                                      let k = self#find_model_key iter in
                                      if (is_file k) then begin
                                        let file = file_of_key k in
                                        (match file.g_file_state with
                                           FileDownloaded  -> GuiCom.send (SaveFile (file.g_file_num, name))
                                         | _  ->  GuiCom.send (RenameFile (file.g_file_num, name)));
                                        let store_iter = self#convert_iter_to_child_iter iter in
                                        store#set ~row:store_iter ~column:download_name file.g_file_name
                                      end
                                    with _ -> ()
                               ))
                             end else begin
                               renderer#set_properties [ `EDITABLE false ]
                             end
                           with _ ->
                             begin
                               renderer#set_properties [ `EDITABLE false ]
                             end
                         end

                      | _ -> renderer#set_properties [ `TEXT "" ]
                  )
            end

        | Col_file_size ->
            begin
              let renderer = GTree.cell_renderer_text [`XALIGN 1.] in
              col#pack renderer;
              col#add_attribute renderer "text" download_size
            end

        | Col_file_downloaded ->
            begin
              let renderer = GTree.cell_renderer_text [`XALIGN 1.] in
              col#pack renderer;
              col#add_attribute renderer "text" download_downloaded
            end

        | Col_file_percent ->
            begin
              let renderer = GTree.cell_renderer_text [`XALIGN 1.] in
              col#pack renderer;
              col#add_attribute renderer "text" download_percent
            end

        | Col_file_rate ->
            begin
              let renderer = GTree.cell_renderer_text [`XALIGN 1.] in
              col#pack renderer;
              col#add_attribute renderer "text" download_download_rate
            end

        | Col_file_state ->
            begin
              let renderer = GTree.cell_renderer_text [] in
              col#pack renderer;
              col#add_attribute renderer "text" download_state
            end

        | Col_file_availability ->
            begin
              if !!O.gtk_look_graphical_availability
                then begin
                  let renderer = GTree.cell_renderer_pixbuf [`XALIGN 0.] in
                  col#pack renderer;
                  col#set_cell_data_func renderer
                    (fun m row ->
                       (if !!verbose then lprintf' "rendering availability bar\n");
                       let height = 16 in
                       let width = col#width in
                       let avail = m#get ~row ~column:download_availability_pixb in
                       match avail with
                           Some pb when width > 0 ->
                             begin
                               let w = GdkPixbuf.get_width pb in
                               let pixb = GdkPixbuf.create ~width ~height ~has_alpha:true () in
                               let scale_x = (float_of_int width) /. (float_of_int w) in
                               (if !!verbose then lprintf' "Resizing pixbuf using scale %5.2f width:%d old_width:%d\n"
                                 scale_x width w);
                               GdkPixbuf.scale
                                 ~dest:pixb ~dest_x:0 ~dest_y:0
                                 ~width ~height ~scale_x ~interp:`TILES
                                 pb;
                               renderer#set_properties [ `PIXBUF pixb; `VISIBLE true ]
                             end
                         | _ -> renderer#set_properties [ `VISIBLE false ]
                    )
                end else begin
                  let renderer = GTree.cell_renderer_text [`XALIGN 1.] in
                  col#pack renderer;
                  col#add_attribute renderer "text" download_availability
                end
            end

        | Col_file_uid ->
            begin
              let renderer = GTree.cell_renderer_text [`XALIGN 0.] in
              col#pack renderer;
              col#add_attribute renderer "text" download_uid
            end

        | Col_file_format ->
            begin
              let renderer = GTree.cell_renderer_text [`XALIGN 0.] in
              col#pack renderer;
              col#add_attribute renderer "text" download_format
            end

        | Col_file_network ->
            begin
              if !!O.gtk_look_use_icons
                then begin
                  let renderer = GTree.cell_renderer_pixbuf [`XALIGN 0.] in
                  col#pack renderer;
                  col#add_attribute renderer "pixbuf" download_network_pixb
                end else begin
                  let renderer = GTree.cell_renderer_text [`XALIGN 0.] in
                  col#pack renderer;
                  col#add_attribute renderer "text" download_network
                end
              end

        | Col_file_age ->
            begin
              let renderer = GTree.cell_renderer_text [`XALIGN 1.] in
              col#pack renderer;
              col#add_attribute renderer "text" download_age
            end

        | Col_file_last_seen ->
            begin
              let renderer = GTree.cell_renderer_text [`XALIGN 1.] in
              col#pack renderer;
              col#add_attribute renderer "text" download_last_seen
            end

        | Col_file_eta ->
            begin
              let renderer = GTree.cell_renderer_text [`XALIGN 0.] in
              col#pack ~expand:true renderer;
              col#add_attribute renderer "text" download_eta_inst;
              let renderer = GTree.cell_renderer_text [`XALIGN 1.] in
              col#pack ~expand:false renderer;
              col#add_attribute renderer "text" download_eta_average;
            end

        | Col_file_priority ->
            begin
              let renderer = GTree.cell_renderer_text [`XALIGN 0.5] in
              col#pack renderer;
              col#add_attribute renderer "text" download_priority
            end

        | Col_file_comment ->
            begin
              let renderer = GTree.cell_renderer_text [`XALIGN 0.] in
              col#pack renderer;
              col#add_attribute renderer "text" download_comment
            end

        | Col_file_sources ->
            begin
              let renderer = GTree.cell_renderer_text [`XALIGN 0.5] in
              col#pack renderer;
              col#add_attribute renderer "text" download_sources
            end

(*************************************************************************)
(*                                                                       *)
(*                         sort_items                                    *)
(*                                                                       *)
(*************************************************************************)

    method sort_items c k1 k2 =
      match (is_file k1, is_file k2) with
          (true, true) ->
             begin try
               let f1 = file_of_key k1 in
               let f2 = file_of_key k2 in
               match c with
                   Col_file_name -> compare (String.lowercase f1.g_file_name) (String.lowercase f2.g_file_name)
                 | Col_file_size -> compare f1.g_file_size f2.g_file_size
                 | Col_file_downloaded -> compare f1.g_file_downloaded f2.g_file_downloaded
                 | Col_file_percent -> compare (float_of_string (Mi.get_percent_of f1.g_file_downloaded f1.g_file_size))
                                               (float_of_string (Mi.get_percent_of f2.g_file_downloaded f2.g_file_size))
                 | Col_file_rate -> compare f1.g_file_download_rate f2.g_file_download_rate
                 | Col_file_state -> compare (Mi.string_of_file_state f1.g_file_state f1.g_file_download_rate)
                                             (Mi.string_of_file_state f2.g_file_state f2.g_file_download_rate)
                 | Col_file_availability ->
                     begin
                       let av1 = Mi.main_availability_of f1.g_file_network f1.g_file_availability in
                       let av2 = Mi.main_availability_of f2.g_file_network f2.g_file_availability in
                       if !!O.gtk_look_graphical_availability
                         then begin
                           Mi.sort_availability_bar (av1, f1.g_file_chunks) (av2, f2.g_file_chunks) true
                         end else begin
                           let s1 = Mi.string_of_availability av1 f1.g_file_chunks in
                           let s2 = Mi.string_of_availability av2 f2.g_file_chunks in
                           compare (float_of_string s1) (float_of_string s2)
                         end
                     end
                 | Col_file_uid -> compare (Mi.uid_list_to_string f1.g_file_uids) (Mi.uid_list_to_string f2.g_file_uids)
                 | Col_file_format -> compare f1.g_file_format f2.g_file_format
                 | Col_file_network -> compare f1.g_file_network f2.g_file_network
                 | Col_file_age -> compare f1.g_file_age f2.g_file_age
                 | Col_file_last_seen -> compare f1.g_file_last_seen f2.g_file_last_seen
                 | Col_file_eta -> compare (Mi.calc_eta_inst f1.g_file_size f1.g_file_downloaded f1.g_file_download_rate)
                                           (Mi.calc_eta_inst f2.g_file_size f2.g_file_downloaded f2.g_file_download_rate)
                 | Col_file_priority -> compare f1.g_file_priority f2.g_file_priority
                 | Col_file_comment -> compare f1.g_file_comment f2.g_file_comment
                 | Col_file_sources ->
                     begin
                       let i = compare f1.g_file_active_sources f2.g_file_active_sources in
                       if i = 0
                         then compare f1.g_file_all_sources f2.g_file_all_sources
                         else i
                     end
             with _ -> 0 end

        | (false, false) when (try file_num k1 = file_num k2 with _ -> false) ->
             begin try
               let s1 = source_of_key k1 in
               let s2 = source_of_key k2 in
               let file_num = file_num k1 in
               match c with
                   Col_file_name -> compare (String.lowercase s1.source_name)
                                            (String.lowercase s2.source_name)
                 | Col_file_downloaded -> compare s1.source_downloaded s2.source_downloaded
                 | Col_file_percent -> compare s1.source_software s2.source_software
                 | Col_file_state -> compare (Mi.string_of_state s1.source_state file_num)
                                             (Mi.string_of_state s2.source_state file_num)
                 | Col_file_availability ->
                     begin
                       try
                         let f = Hashtbl.find G.files file_num in
                         let av_s1 = try List.assoc file_num s1.source_availability with _ -> "" in
                         let av_s2 = try List.assoc file_num s2.source_availability with _ -> "" in
                         if !!O.gtk_look_graphical_availability
                           then begin
                             Mi.sort_availability_bar (av_s1, f.g_file_chunks) (av_s2, f.g_file_chunks) false
                           end else begin
                             let av1 = Mi.string_of_availability av_s1 f.g_file_chunks in
                             let av2 = Mi.string_of_availability av_s2 f.g_file_chunks in
                             compare (float_of_string av1) (float_of_string av2)
                           end
                       with _ -> 0
                     end
                 | Col_file_network -> compare s1.source_network s2.source_network
                 | Col_file_rate -> compare s1.source_download_rate s2.source_download_rate
                 | _ -> 0
             with _ -> 0 end

        | _ -> 0

(*************************************************************************)
(*                                                                       *)
(*                         force_update_icons                            *)
(*                                                                       *)
(*************************************************************************)

    method force_update_icons () =
      let f k row =
        if (is_file k) then begin
          let f = file_of_key k in
          store#set ~row ~column:download_network_pixb (Mi.network_pixb f.g_file_network ~size:A.SMALL ());
          store#set ~row ~column:download_name_pixb (Mi.file_type_of_name f.g_file_name ~size:A.SMALL);
        end else begin
          let s = source_of_key k in
          store#set ~row ~column:download_network_pixb (Mi.network_pixb s.source_network ~size:A.SMALL ());
          store#set ~row ~column:download_name_pixb (Mi.source_type_to_icon s.source_type ~size:A.SMALL);
        end
      in
      List.iter (fun k ->
        try
          let row = self#find_row k in
          f k row
        with _ -> ()
      ) (self#all_items ())

(*************************************************************************)
(*                                                                       *)
(*                         force_update_avail_bars                       *)
(*                                                                       *)
(*************************************************************************)

    method force_update_avail_bars () =
      let f k row =
        if !!O.gtk_look_graphical_availability
          then if (is_file k) then begin
             let f = file_of_key k in
             let availability = Mi.main_availability_of f.g_file_network f.g_file_availability in
             store#set ~row ~column:download_availability_pixb (Mi.availability_bar availability f.g_file_chunks true);
          end else begin
             let f = file_of_key k in
             let s = source_of_key k in
             let availability =
               try
                 List.assoc (file_num k) s.source_availability
               with _ -> ""
             in
             store#set ~row ~column:download_availability_pixb (Mi.availability_bar availability f.g_file_chunks false);
          end else store#set ~row ~column:download_availability_pixb None
      in
      List.iter (fun k ->
        try
          let row = self#find_row k in
          f k row
        with _ -> ()
      ) (self#all_items ())

end

let downloadstore = new g_download ()

(*************************************************************************)
(*                                                                       *)
(*                         message to the core                           *)
(*                                                                       *)
(*************************************************************************)

let on_entry_return entry =
  match entry#text with
      "" -> ()
    | s ->
        begin
          GuiCom.send (Url s);
          entry#set_text ""
        end

let preview k () = try GuiCom.send (Preview (file_num k)) with _ -> ()

let show_details k () =
  try
    let item =
      if is_file k
      then File (file_of_key k)
      else Source ((source_of_key k), file_num k)
    in GuiInfoWindow.window item ()
  with _ -> ()

let pause_resume sel () =
  List.iter (fun k ->
    if is_file k
    then try
      let f = file_of_key k in
      let state =
        match f.g_file_state with
            FilePaused | FileAborted _ -> true
          | _ -> false
      in
      GuiCom.send (SwitchDownload (f.g_file_num, state))
    with _ -> ()
  ) sel

let retry_connect sel () =
  List.iter (fun k ->
    if is_file k
    then try
      GuiCom.send (ConnectAll (file_num k))
    with _ -> ()
  ) sel

let cancel sel () =
  let warning_message = !M.dT_lb_ask_cancel_download_files in
  let l = ref [] in
  List.iter (fun k ->
    if is_file k
    then try
      let f = file_of_key k in
      l := f.g_file_name :: !l
    with _ -> ()
  ) sel;
  let on_ok () =
    List.iter (fun k ->
    if is_file k
    then try
      GuiCom.send (RemoveDownload_query (file_num k))
    with _ -> ()
    ) sel
  in
  GuiTools.warning_box ~warning_message ~text:!l ~on_ok ()

let verify_chunks sel () =
  List.iter (fun k ->
    if is_file k
    then try
      GuiCom.send (VerifyAllChunks (file_num k))
    with _ -> ()
  ) sel

let set_priority sel prio () =
  List.iter (fun k ->
    if is_file k
    then try
      GuiCom.send (SetFilePriority (file_num k, prio))
    with _ -> ()
  ) sel

let preference label v box_type opt_list =
  let module C = ConfigWindow in
  {
    C.pref_section = None;
    C.opt_section = "";
    C.pref_subsection = None;
    C.pref_help = "";
    C.pref_advanced = false;
    C.pref_default = v;
    C.pref_name = label;
    C.pref_label = label;
    C.pref_group = None;
    C.pref_option_list = opt_list;
    C.pref_value = v;
    C.pref_new_value = v;
    C.pref_type = box_type;
    C.pref_apply = (fun () -> ());
    C.pref_apply_default = (fun () -> ());
  }

let edit_mp3_tags f () =
  match f.g_file_format with
      MP3 (tag,_) ->
        begin
          let module C = ConfigWindow in
          let module P = Mp3tag.Id3v1 in
          let title = preference !M.qT_lb_title tag.P.title C.BString [] in
          let artist = preference !M.qT_lb_artist tag.P.artist C.BString [] in
          let album = preference !M.qT_lb_album tag.P.album C.BString [] in
          let year = preference !M.qT_lb_year tag.P.year C.BString [] in
          let comment = preference !M.qT_lb_comment tag.P.comment C.BString [] in
          let tracknum = preference !M.qT_lb_soundtrack (string_of_int tag.P.tracknum) C.BInt [] in
          let genres = List.map (fun (_, g) -> g) Mp3_genres.genres in
          let genre = preference !M.qT_lb_genre (Mp3_misc.string_of_genre tag.P.genre) C.BCombo genres in
          let on_ok () =
            tag.P.title <- title.C.pref_new_value;
            tag.P.artist <- artist.C.pref_new_value;
            tag.P.album <- album.C.pref_new_value;
            tag.P.year <- year.C.pref_new_value;
            tag.P.comment <- comment.C.pref_new_value;
            tag.P.tracknum <- int_of_float (C.safe_int tracknum.C.pref_new_value);
            tag.P.genre <- Mp3_misc.genre_of_string genre.C.pref_new_value;
            GuiCom.send (ModifyMp3Tags (f.g_file_num, tag))
          in
          C.simple_panel
              ~prefs:[title;artist;album;year;comment;tracknum;genre]
              ~title:!M.dT_wt_edit_mp3
              ~icon:(A.get_icon ~icon:M.icon_menu_search_mp3 ~size:A.SMALL ())
              ~on_ok ()
        end
    | _ -> ()


let get_format sel () =
  List.iter (fun k ->
    if is_file k then try
      GuiCom.send (QueryFormat (file_num k))
    with _ -> ()
  ) sel

let save_all () =
  List.iter (fun k ->
    if is_file k
    then try
      let f = file_of_key k in
      match f.g_file_state with
          FileDownloaded -> GuiCom.send (SaveFile (f.g_file_num, f.g_file_name))
        | _ -> ()
    with _ -> ()
  ) (downloadstore#all_items ())

let add_to_friends sel () =
  List.iter (fun k ->
    if is_source k then try
      GuiCom.send (AddClientFriend (source_num k))
    with _ -> ()
  ) sel

let browse_files = add_to_friends

let update_downloaders _ =
  List.iter (fun source_num ->
    GuiCom.send (GetClient_info source_num)
  ) !downloaders;
  downloaders := []

let ask_for_sources () =
  if !G.use_interested_in_sources && not !interested_in_sources
    then begin
(*    if !!verbose then lprintf' "interested in sources !!!!\n"; *)
      interested_in_sources := true;
      GuiCom.send (InterestedInSources true)
    end

let update_downloading_files () =
  let keys = downloadstore#all_items () in
  let files = keys_to_files keys in
  List.iter (fun f ->
    GuiCom.send (GetFile_info f.g_file_num)
  ) files

let add_razorback_stats stat (png_box : GPack.box) (table : GPack.table) pos =
  try
    let filename = stat.stats_file_history in
    let pixb = GdkPixbuf.from_file filename in
    let razorback_history =
      GMisc.image ~pixbuf:pixb ~packing:(png_box#pack ~expand:false ~fill:true) ()
    in
    let label_razorback =
      GMisc.label ~xalign:0. ~yalign:0.
      ~markup:(GuiTools.create_default_bold_markup "http://stats.razorback2.com/ed2khistory") ()
    in
    let label_rate =
      GMisc.label ~xalign:0. ~yalign:0.
      ~markup:!M.dT_lb_stats_rate ()
    in
    let label_available =
      GMisc.label ~xalign:0. ~yalign:0.
      ~markup:!M.dT_lb_stats_available ()
    in
    let label_complete =
      GMisc.label ~xalign:0. ~yalign:0.
      ~markup:!M.dT_lb_stats_complete ()
    in
    let label_rating =
      GMisc.label ~xalign:1. ~yalign:0. ~line_wrap:true
      ~markup:(U.utf8_of stat.stats_file_rating) ()
    in
    let label_availability =
      GMisc.label ~xalign:1. ~yalign:0.
      ~markup:(Printf.sprintf "%d" stat.stats_file_availability) ()
    in
    let label_completed =
      GMisc.label ~xalign:1. ~yalign:0.
      ~markup:(Printf.sprintf "%d" stat.stats_file_completed) ()
    in
    List.iter (fun (w, left, right, top, expand) ->
      table#attach
        ~left ~top
        ~xpadding:0 ~ypadding:0
        ~right ~bottom:(top + 1)
        ~expand ~fill:`X
        w;
    ) [
       label_razorback#coerce   , 0, 2, !pos + 0, `NONE;
       label_rate#coerce        , 0, 1, !pos + 1, `NONE;
       label_rating#coerce      , 1, 2, !pos + 1, `NONE;
       label_available#coerce   , 0, 1, !pos + 2, `NONE;
       label_availability#coerce, 1, 2, !pos + 2, `NONE;
       label_complete#coerce    , 0, 1, !pos + 3, `NONE;
       label_completed#coerce   , 1, 2, !pos + 3, `NONE;
      ];
    pos := !pos + 6
  with _ -> ()

let add_filedonkey_stats stat (table : GPack.table) pos =
  try
    let label_filedonkey =
      GMisc.label ~xalign:0. ~yalign:0.
      ~markup:(GuiTools.create_default_bold_markup "http://www.filedonkey.com/") ()
    in
    let label_available =
      GMisc.label ~xalign:0. ~yalign:0.
      ~markup:!M.dT_lb_stats_available ()
    in
    let label_complete =
      GMisc.label ~xalign:0. ~yalign:0.
      ~markup:!M.dT_lb_stats_complete ()
    in
    let label_availability =
      GMisc.label ~xalign:1. ~yalign:0.
      ~markup:(Printf.sprintf "%d" stat.stats_file_availability) ()
    in
    let label_completed =
      GMisc.label ~xalign:1. ~yalign:0.
      ~markup:(Printf.sprintf "%d" stat.stats_file_completed) ()
    in
    List.iter (fun (w, left, right, top, expand) ->
      table#attach
        ~left ~top
        ~xpadding:0 ~ypadding:0
        ~right ~bottom:(top + 1)
        ~expand ~fill:`X
        w
    ) [
       label_filedonkey#coerce  , 0, 2, !pos + 0, `NONE;
       label_available#coerce   , 0, 1, !pos + 1, `NONE;
       label_availability#coerce, 1, 2, !pos + 1, `NONE;
       label_complete#coerce    , 0, 1, !pos + 2, `NONE;
       label_completed#coerce   , 1, 2, !pos + 2, `NONE;
      ];
    pos := !pos + 5
  with _ -> ()

let show_stats file (hbox_stats : GPack.box) (hbox_progress : GPack.box) =
  List.iter (fun w -> w#destroy ()) hbox_stats#children;
  List.iter (fun w -> w#misc#hide ()) hbox_progress#children;
  match file.g_file_stats with
    [] -> ()
  | _ ->
      begin
        let pos = ref 0 in
        let table_stats =
          GPack.table ~columns:2 ~homogeneous:false
          ~row_spacings:3 ~col_spacings:48 ~border_width:6 ()
        in
        begin
          try
            let stat = H.get_stat file RazorBack in
            add_razorback_stats stat hbox_stats table_stats pos
          with _ -> ()
        end;
        begin
          try
            let stat = H.get_stat file FileDonkey in
            add_filedonkey_stats stat table_stats pos
          with _ -> ()
        end;
        hbox_stats#add table_stats#coerce
      end

let razorback2_stats k () =
  try
    let file = file_of_key k in
    let on_completed () =
      match !current_selection with
        key :: _ when key = k ->
          begin
            match !stat_boxes with
              Some (hbox_stats, hbox_progress, label, pbar) ->
                begin
                  show_stats file hbox_stats hbox_progress
                end
            | None -> ()
          end
      | _ -> ()
    in
    let on_not_found md4 =
      H.remove_stat file RazorBack;
      match !stat_boxes with
        Some (hbox_stats, hbox_progress, label, pbar) ->
          begin
            show_stats file hbox_stats hbox_progress;
            let s = Printf.sprintf "Sorry, the file ed2k::%s was not found on http://stats.razorback2.com" md4 in
            label#set_label s;
            label#misc#show ()
          end
      | _ -> ()
    in
    let t1 = ref 0. in
    let t2 = ref 0. in
    let progress t md4 desc n m =
      let s = U.utf8_of (Printf.sprintf "%s %s:" desc md4) in
      let tt = !t in
      t := !t +. float_of_int n;
      let p =
        if m > 0
          then min 1. (!t /. float_of_int m)
          else if !t > 0. then tt /. !t else 0.
      in
      let v = int_of_float (p *. 100.) in
      let percent = U.simple_utf8_of (Printf.sprintf "%d%%" v) in
      (if !!verbose then lprintf' "%s: %s\n" s percent);
      match !stat_boxes with
        Some (hbox_stats, hbox_progress, label, pbar) ->
          begin
            label#set_label s;
            pbar#set_fraction p;
            pbar#set_text percent;
            label#misc#show ();
            pbar#misc#show ();
          end
      | _ -> ()
    in
    H.get_razorback2_stats file on_completed on_not_found (progress t1) (progress t2)
  with _ -> ()

let filedonkey_stats k () =
  try
    let file = file_of_key k in
    let on_completed () =
      match !current_selection with
        key :: _ when key = k ->
          begin
            match !stat_boxes with
              Some (hbox_stats, hbox_progress, label, pbar) ->
                begin
                  show_stats file hbox_stats hbox_progress
                end
            | None -> ()
          end
      | _ -> ()
    in
    let on_not_found md4 =
      H.remove_stat file FileDonkey;
      match !stat_boxes with
        Some (hbox_stats, hbox_progress, label, pbar) ->
          begin
            show_stats file hbox_stats hbox_progress;
            let s = Printf.sprintf "Sorry, the file ed2k::%s was not found on http://www.filedonkey.com/" md4 in
            label#set_label s;
            label#misc#show ()
          end
      | _ -> ()
    in
    let t = ref 0. in
    let progress md4 desc n m =
      let s = U.utf8_of (Printf.sprintf "%s %s:" desc md4) in
      let tt = !t in
      t := !t +. float_of_int n;
      let p =
        if m > 0
          then min 1. (!t /. float_of_int m)
          else if !t > 0. then tt /. !t else 0.
      in
      let v = int_of_float (p *. 100.) in
      let percent = U.simple_utf8_of (Printf.sprintf "%d%%" v) in
      (if !!verbose then lprintf' "%s: %s\n" s percent);
      match !stat_boxes with
        Some (hbox_stats, hbox_progress, label, pbar) ->
          begin
            label#set_label s;
            pbar#set_fraction p;
            pbar#set_text percent;
            label#misc#show ();
            pbar#misc#show ();
          end
      | _ -> ()
    in
    H.get_filedonkey_stats file on_completed on_not_found progress
  with _ -> ()

(*************************************************************************)
(*                                                                       *)
(*                         Download Menu                                 *)
(*                                                                       *)
(*************************************************************************)

let download_menu sel =
    match sel with
        [] -> []
      | k :: tail when (is_file k) ->
          (if tail = []
             then
               [
                `I ((!M.dT_me_show_file_details), show_details k) ;
                `I ((!M.dT_me_preview), preview k) ;
                `S ;
                `I ((!M.dT_me_razorback2_stats), razorback2_stats k);
                `I ((!M.dT_me_filedonkey_stats), filedonkey_stats k);
                `S ;
               ]
             else  [])
             @
                `I ((!M.dT_me_pause_resume_dl), pause_resume sel) ::
                `I ((!M.dT_me_retry_connect), retry_connect sel) ::
                `I ((!M.dT_me_cancel), cancel sel) ::
                `I ((!M.dT_me_verify_chunks), verify_chunks sel) ::
                `M ((!M.dT_me_set_priority),
                     [
                      `I ((!M.dT_me_set_priority_veryhigh), set_priority sel 20);
                      `I ((!M.dT_me_set_priority_high), set_priority sel 10);
                      `I ((!M.dT_me_set_priority_normal), set_priority sel 0);
                      `I ((!M.dT_me_set_priority_low), set_priority sel (-10));
                      `I ((!M.dT_me_set_priority_verylow), set_priority sel (-20));
                     ]) ::
                `S ::
                `I ((!M.dT_me_get_format), get_format sel) ::
             (try let f = file_of_key k in
              match (f.g_file_state, f.g_file_format) with
                  (FileDownloaded, MP3 _) ->
               [
                `I ((!M.dT_me_edit_mp3), edit_mp3_tags f)
               ]
                | _ ->
               [] with _ -> [])
             @
                `I ((!M.dT_me_save_all), save_all) :: [] (*
                (if tail = [] then
               [
                `I ((!M.dT_me_save_as), save_as f) ;
               ]
            else  []) *)

      | k :: tail when (is_source k) ->
          if (source_num k) = (-1)
            then []
            else
              (if tail = []
                 then
                   [
                    `I ((!M.dT_me_show_source_details), show_details k) ;
                   ]
                 else  [])
              @
                   [
                    `I (!M.dT_me_browse_files, browse_files sel);
                    `I (!M.dT_me_add_to_friends, add_to_friends sel)
                   ]
      | _ -> []

(*************************************************************************)
(*                                                                       *)
(*                         filter_download                               *)
(*                                                                       *)
(*************************************************************************)

let filter_download k = 
  if is_file k
  then begin
    try
      let f = file_of_key k in
      not (List.memq f.g_file_network !G.networks_filtered)
    with _ -> true
  end else begin
    try
      let s = source_of_key k in
      not (List.memq s.source_network !G.networks_filtered)
(*    && s.source_state <> NewHost && (s.source_name <> "" && s.source_software <> "unk") *)
    with _ -> true
  end

(*************************************************************************)
(*                                                                       *)
(*                         Templates initialization                      *)
(*                                                                       *)
(*************************************************************************)

let _ =
  downloadstore#set_filter filter_download

(*************************************************************************)
(*                                                                       *)
(*                         clean_avail_bars                              *)
(*                                                                       *)
(*************************************************************************)

let clean_avail_bars () =
  if !!O.gtk_look_graphical_availability
    then begin
      let list = ref [] in
      List.iter (fun k ->
        if is_file k
          then begin
            try
              let f = file_of_key k in
              let availability = Mi.main_availability_of f.g_file_network f.g_file_availability in
              let key = (availability, f.g_file_chunks, true) in
              if not (List.mem key !list)
                then begin list := key :: !list end;
              match f.g_file_sources with
                  None -> ()
                | Some sources ->
                    List.iter (fun source_num ->
                      let s = Hashtbl.find G.sources source_num in
                      List.iter (fun (_, avail) ->
                        let key = (avail, f.g_file_chunks, false) in
                        if not (List.mem key !list)
                          then list := key :: !list
                      ) s.source_availability;
                    ) sources
           with _ -> ()
          end
      ) (downloadstore#all_items ());
      A.clean_avail_bars !list
    end

(*************************************************************************)
(*                                                                       *)
(*                         clear                                         *)
(*                                                                       *)
(*************************************************************************)

let clear () =
  downloadstore#clear ();
  downloaders := [];
  current_selection := [];
  expanded_rows := 0;
  clean_avail_bars ()

(*************************************************************************)
(*                                                                       *)
(*                         message from GuiNetwoks                       *)
(*                                                                       *)
(*************************************************************************)

let reset_downloads_filter () =
  downloadstore#refresh_filter ()

(*************************************************************************)
(*                                                                       *)
(*                         message from the core                         *)
(*                                                                       *)
(*************************************************************************)

let hashtbl_update f f_new =
  f.g_file_comment <- f_new.g_file_comment;
  f.g_file_name <- f_new.g_file_name;
  f.g_file_names <- f_new.g_file_names;
  f.g_file_size <- f_new.g_file_size;
  f.g_file_downloaded <- f_new.g_file_downloaded;
  f.g_file_active_sources <- f_new.g_file_active_sources;
  f.g_file_all_sources <- f_new.g_file_all_sources;
  f.g_file_state <- f_new.g_file_state;
  f.g_file_chunks <- f_new.g_file_chunks;
  f.g_file_availability <- f_new.g_file_availability;
  f.g_file_sources <- f_new.g_file_sources;
  f.g_file_download_rate <- f_new.g_file_download_rate;
  f.g_file_format <- f_new.g_file_format;
  f.g_file_chunks_age <- f_new.g_file_chunks_age;
  f.g_file_age <- f_new.g_file_age;
  f.g_file_last_seen <- f_new.g_file_last_seen;
  f.g_file_priority <- f_new.g_file_priority;
  f.g_file_uids <- f_new.g_file_uids

let remove_dummy_source file_num =
  downloadstore#remove_item (dummy_source_key file_num)

let add_dummy_source file_num =
  try
    let parent = downloadstore#find_row (file_key file_num) in
    try
      let _ = downloadstore#find_row (dummy_source_key file_num) in ()
    with _ ->
       downloadstore#add_item (Source (dummy_source, file_num)) ~parent ();
  with _ -> ()

let h_cancelled file_num =
  try
    let fi = Hashtbl.find G.files file_num in
    downloadstore#remove_item (file_key file_num);
    Hashtbl.remove G.files file_num;
    decr G.ndownloads;
    (if fi.g_file_state = FileDownloaded
       then decr G.ndownloaded);
    GuiStatusBar.update_downloadedfiles ();
    let file_uid = Mi.to_uid_type fi.g_file_uids in
    GuiGraphBase.cancel_file file_uid;
    Hashtbl.remove G.file_by_uid file_uid;
  with _ -> ()

let h_paused f =
  let file_uid = Mi.to_uid_type f.g_file_uids in
  try
    let fi = Hashtbl.find G.files f.g_file_num in
    let row = downloadstore#find_row (file_key f.g_file_num) in
    (if f.g_file_state = FileDownloaded && f.g_file_state <> fi.g_file_state
       then incr G.ndownloaded);
    let f_new = {f with g_file_sources = fi.g_file_sources} in
    downloadstore#update_item row (File fi) (File f_new);
    hashtbl_update fi f_new;
    let rate = int_of_float f.g_file_download_rate in
    GuiGraphBase.save_record rate (GraphFile (file_uid, GraphDownloads));
    if f.g_file_name <> fi.g_file_name
      then Hashtbl.replace G.file_by_uid (file_uid) (U.utf8_of f.g_file_name);
  with _ ->
    begin
      downloadstore#add_item (File f) ();
      incr G.ndownloads;
      (if f.g_file_state = FileDownloaded
         then incr G.ndownloaded);
      GuiStatusBar.update_downloadedfiles ();
      Hashtbl.add G.files f.g_file_num f;
      GuiGraphBase.add_file file_uid;
      Hashtbl.add G.file_by_uid file_uid (U.utf8_of f.g_file_name);
      add_dummy_source f.g_file_num;
    end

let h_downloading = h_paused

let h_downloaded = h_paused

let h_removed = h_cancelled


let file_info f =
  match f.g_file_state with
      FileNew -> assert false
    | FileCancelled -> 
        h_cancelled f.g_file_num
    | FileDownloaded ->
        h_downloaded f
    | FileShared ->
        h_removed f.g_file_num
    | FilePaused | FileQueued | FileAborted _ -> 
        h_paused f
    | FileDownloading ->
        h_downloading f

let file_downloaded (file_num, downloaded, rate, last_seen) =
  try
    let f = Hashtbl.find G.files file_num in
    let row = downloadstore#find_row (file_key file_num) in
    let f_new = {f with g_file_downloaded = downloaded;
                        g_file_download_rate = rate;
                        g_file_last_seen = last_seen}
    in
    downloadstore#update_item row (File f) (File f_new);
    f.g_file_downloaded <- f_new.g_file_downloaded;
    f.g_file_download_rate <- f_new.g_file_download_rate;
    f.g_file_last_seen <- f_new.g_file_last_seen;
    let rate = int_of_float f.g_file_download_rate in
    GuiGraphBase.save_record rate (GraphFile ((Mi.to_uid_type f.g_file_uids), GraphDownloads));
  with _ -> ()

(* File_add_source and File_update_availability could be merged now that we use
 * InterestedInSources. Pb how to retrieve the chunks of a source ?
 *)
let h_add_source s file_num =
  try
    let f = Hashtbl.find G.files file_num in
    let parent = downloadstore#find_row (file_key file_num) in
    match f.g_file_sources with
        None ->
          begin
            f.g_file_sources <- Some [s.source_num];
            downloadstore#add_item (Source (s, file_num)) ~parent ();
            remove_dummy_source file_num
          end
      | Some sources ->
          begin
            if List.mem s.source_num sources
              then raise Exit
              else begin
                f.g_file_sources <- Some (s.source_num :: sources);
                downloadstore#add_item (Source (s, file_num)) ~parent ();
              end
          end
  with _ -> ()

let h_remove_source s file_num =
  try
    let f = Hashtbl.find G.files file_num in
    let _ =
      match f.g_file_sources with
            None -> add_dummy_source file_num
          | Some sources ->
              begin
                let l = List.filter (fun num -> num <> s.source_num) sources in
                match l with
                    [] ->
                      begin
                        add_dummy_source file_num;
                        f.g_file_sources <- None
                      end
                  | _ -> f.g_file_sources <- Some l
              end
    in
    downloadstore#remove_item (source_key file_num s.source_num);
  with _ -> ()

let h_update_source s s_new =
  List.iter (fun file_num ->
    try
      let row = downloadstore#find_row (source_key file_num s_new.source_num) in
      downloadstore#update_item row (Source (s, file_num)) (Source (s_new, file_num))
    with _ -> ()
  ) s_new.source_files_requested;
  match s_new.source_state with
      Connected_downloading _ ->
        if not (List.mem s_new.source_num !downloaders)
          then downloaders := s_new.source_num :: !downloaders
    | _ -> ()

let h_update_source_availability s s_new file_num =
   try
     let row = downloadstore#find_row (source_key file_num s_new.source_num) in
     downloadstore#update_item row (Source (s, file_num)) (Source (s_new, file_num))
   with _ -> ()

let clean_sources_table s_old =
  List.iter (fun file_num ->
    h_remove_source s_old file_num
  ) s_old.source_files_requested

(*************************************************************************)
(*                                                                       *)
(*                         downloads window                              *)
(*                                                                       *)
(*************************************************************************)

open GMain

let downloads_box gui =
  update_downloaders ();
  update_downloading_files ();
  let vbox = GPack.vbox ~homogeneous:false ~border_width:6 () in
  ignore (vbox#connect#destroy ~callback:
    (fun _ ->
       view_context := None;
       stat_boxes := None;
       current_selection := [];
       expanded_rows := 0;
       Timeout.remove (!downloaders_timerID)
  ));

  let hbox =
    GPack.hbox ~homogeneous:false ~border_width:6
      ~spacing:12
      ~packing:(vbox#pack ~expand:false ~fill:true) ()
  in
  let markup = create_markup !M.dT_lb_link in
  let _ =
    GMisc.label ~markup ~xalign:0. ~line_wrap:true
      ~packing:(hbox#pack ~expand:false ~fill:true) ()
  in
  let entry =
    GEdit.entry ~editable:true ~has_frame:true
       ~packing:(hbox#pack ~expand:true ~fill:true) ()
  in
  let downloadview =
    Downloads.treeview ~mode:`MULTIPLE
      ~packing:(vbox#pack ~expand:true ~fill:true) ()
  in
  let hbox_razorback_stats =
    GPack.hbox ~homogeneous:false ~border_width:6 ~spacing:12
      ~packing:(vbox#pack ~expand:false ~fill:true) ()
  in
  let hbox_razorback_progress =
    GPack.hbox ~homogeneous:false ~border_width:6 ~spacing:12
      ~packing:(vbox#pack ~expand:false ~fill:true) ()
  in
  let label_razorback_progress =
    GMisc.label ~xalign:0. ~yalign:0.5 ~show:false
      ~packing:(hbox_razorback_progress#pack ~expand:false ~fill:true) ()
  in
  let pbar_razorback_progress =
    GRange.progress_bar ~pulse_step:0.01 ~show:false
      ~packing:(hbox_razorback_progress#pack ~expand:false ~fill:true) ()
  in
  stat_boxes := Some (hbox_razorback_stats, hbox_razorback_progress,
                           label_razorback_progress, pbar_razorback_progress);
  let on_select_files keys =
    current_selection := keys;
    match keys with
      k :: _ when (is_file k) ->
        begin
          try
            let file = file_of_key k in
            show_stats file hbox_razorback_stats hbox_razorback_progress
          with _ -> ()
        end
    | _ -> ()
  in
  let on_expand_item path k =
    ask_for_sources ();
    false
  in
  let on_expanded_item path k =
    expanded_rows := List.length downloadview#expanded_paths;
    (if !!verbose then lprintf' "rows expanded %d\n" !expanded_rows)
  in
  let on_collapsed_item path k =
    expanded_rows := List.length downloadview#expanded_paths;
    (if !!verbose then lprintf' "rows expanded %d\n" !expanded_rows)
  in
  view_context := Some downloadview#view#misc#pango_context;
  downloadview#set_model downloadstore#gmodel;
  downloadview#set_menu download_menu;
  downloadview#set_on_expand on_expand_item;
  downloadview#set_on_expanded on_expanded_item;
  downloadview#set_on_collapsed on_collapsed_item;
  downloadview#set_on_select on_select_files;
  update_downloaders ();
  downloaders_timerID := Timeout.add ~ms:6000 ~callback:
    (fun _ ->
      ( if !!verbose then lprintf' "Update downloaders\n");
      update_downloaders ();
      update_downloading_files ();
      true);
  ignore (entry#event#connect#key_press ~callback:
    (fun ev ->
      (GdkEvent.Key.keyval ev = GdkKeysyms._Return ||
       GdkEvent.Key.keyval ev = GdkKeysyms._KP_Enter ||
       GdkEvent.Key.keyval ev = GdkKeysyms._ISO_Enter ||
       GdkEvent.Key.keyval ev = GdkKeysyms._3270_Enter) &&
      (on_entry_return entry;
       true
      )
  ));
  vbox#coerce

let _ =
  ignore (Timeout.add ~ms:1800000 ~callback:
    (fun _ ->
      clean_avail_bars ();
      true
  ));
  ignore (Timeout.add ~ms:30000 ~callback:
    (fun _ ->
       if !G.use_interested_in_sources &&
            !interested_in_sources && !expanded_rows = 0 then 
         begin
           ( if !!verbose then lprintf' "not interested in sources !!!!\n");
           interested_in_sources := false;
           GuiCom.send (InterestedInSources false);
           true
         end else true
  ))

