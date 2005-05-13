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

let verbose = O.gtk_verbose_downloads

let lprintf' fmt =
  Printf2.lprintf ("GuiDownloads: " ^^ fmt)


(*************************************************************************)
(*                                                                       *)
(*                         Global tables                                 *)
(*                                                                       *)
(*************************************************************************)

let (downloaders : int list ref) = ref []

(*************************************************************************)
(*                                                                       *)
(*                         Global variables                              *)
(*                                                                       *)
(*************************************************************************)

let downloaders_timerID = ref (GMain.Timeout.add ~ms:2000 ~callback:(fun _ -> true))
let (view_context : GPango.context option ref) = ref None
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
     source_has_upload = false;
     source_availability = [];
     source_files_requested = [];
  }

(*************************************************************************)
(*                                                                       *)
(*                         Templates                                     *)
(*                                                                       *)
(*************************************************************************)

let get_key =
  (fun i ->
     match i with
         File f -> File_num f.file_num
       | Source (s, file_num) -> Source_num (s.source_num, file_num)
  )

module Downloads = GuiTemplates.Gview(struct

  module Column = GuiColumns.File

  type item = item_info
  type key  = item_index

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
              store#set ~row ~column:download_network (Mi.network_name f.file_network);
              store#set ~row ~column:download_network_pixb (Mi.network_pixb f.file_network ~size:A.SMALL ());
              store#set ~row ~column:download_name (U.utf8_of f.file_name);
              store#set ~row ~column:download_name_pixb (Mi.file_type_of_name f.file_name ~size:A.SMALL);
              store#set ~row ~column:download_uid (Mi.uid_list_to_string f.file_uids);
              store#set ~row ~column:download_size (Mi.size_of_int64 f.file_size);
              store#set ~row ~column:download_downloaded (Mi.size_of_int64 f.file_downloaded);
              store#set ~row ~column:download_percent (Mi.get_percent_of f.file_downloaded f.file_size);
              store#set ~row ~column:download_sources (Printf.sprintf "(%d / %d)" f.file_active_sources f.file_all_sources);
              store#set ~row ~column:download_state (Mi.string_of_file_state f.file_state f.file_download_rate);
              let availability = Mi.main_availability_of f.file_network f.file_availability in
              store#set ~row ~column:download_availability (Mi.string_of_availability availability f.file_chunks);
              store#set ~row ~column:download_availability_pixb (Mi.availability_bar availability f.file_chunks true);
              store#set ~row ~column:download_download_rate (Mi.rate_to_string f.file_download_rate);
              store#set ~row ~column:download_format (Mi.format_to_string f.file_format);
              store#set ~row ~column:download_age (Mi.time_to_string f.file_age);
              store#set ~row ~column:download_last_seen (Mi.time_to_string f.file_last_seen);
              store#set ~row ~column:download_eta_inst (Mi.calc_eta_inst f.file_size f.file_downloaded f.file_download_rate);
              store#set ~row ~column:download_eta_average (Mi.calc_eta_average f.file_size f.file_downloaded f.file_age);
              store#set ~row ~column:download_priority (Mi.priority_to_string f.file_priority);
              store#set ~row ~column:download_comment (U.utf8_of f.file_comment)
            end
        | Source (s, file_num) ->
            try
              let (_, i) = self#find_item (File_num file_num) in
              let f = match i with File f -> f | _ -> raise Exit in
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
                  store#set ~row ~column:download_availability (Mi.string_of_availability availability f.file_chunks);
                  store#set ~row ~column:download_availability_pixb (Mi.availability_bar availability f.file_chunks false);
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
              if f.file_comment <> f_new.file_comment
                then begin
                  store#set ~row ~column:download_comment (U.utf8_of f_new.file_comment)
                end;
              if f.file_name <> f_new.file_name
                then begin
                  store#set ~row ~column:download_name (U.utf8_of f_new.file_name);
                  if (Mi.extension_of f.file_name) <> (Mi.extension_of f_new.file_name)
                    then begin
                      store#set ~row ~column:download_name_pixb (Mi.file_type_of_name f_new.file_name ~size:A.SMALL);
                    end
                end;
              if f.file_uids <> f_new.file_uids
                then begin
                  store#set ~row ~column:download_uid (Mi.uid_list_to_string f.file_uids);
                end;
              if (f.file_size, f.file_downloaded, f.file_download_rate, f.file_age) <>
                 (f_new.file_size, f_new.file_downloaded, f_new.file_download_rate, f_new.file_age)
                then begin
                  store#set ~row ~column:download_downloaded (Mi.size_of_int64 f_new.file_downloaded);
                  store#set ~row ~column:download_percent (Mi.get_percent_of f_new.file_downloaded f_new.file_size);
                  store#set ~row ~column:download_size (Mi.size_of_int64 f_new.file_size);
                  store#set ~row ~column:download_age (Mi.time_to_string f_new.file_age);
                  store#set ~row ~column:download_download_rate (Mi.rate_to_string f_new.file_download_rate);
                  store#set ~row ~column:download_eta_inst (Mi.calc_eta_inst f_new.file_size f_new.file_downloaded
                                                                             f_new.file_download_rate);
                  store#set ~row ~column:download_eta_average (Mi.calc_eta_average f_new.file_size
                                                                                   f_new.file_downloaded f_new.file_age);
                end;
              if (f.file_state, f.file_download_rate) <> (f_new.file_state, f_new.file_download_rate)
                then begin
                  store#set ~row ~column:download_state (Mi.string_of_file_state f_new.file_state f_new.file_download_rate);
                end;
              let availability = Mi.main_availability_of f.file_network f.file_availability in
              let availability_new = Mi.main_availability_of f_new.file_network f_new.file_availability in
              if (availability, f.file_chunks) <> (availability_new, f_new.file_chunks)
                then begin
                  store#set ~row ~column:download_availability (Mi.string_of_availability availability_new f_new.file_chunks);
                  store#set ~row ~column:download_availability_pixb (Mi.availability_bar availability_new f_new.file_chunks true);
                end;
              if f.file_format <> f_new.file_format
                then begin
                  store#set ~row ~column:download_format (Mi.format_to_string f_new.file_format);
                end;
              if f.file_last_seen <> f_new.file_last_seen
                then begin
                  store#set ~row ~column:download_last_seen (Mi.time_to_string f_new.file_last_seen);
                end;
              if f.file_priority <> f_new.file_priority
                then begin
                  store#set ~row ~column:download_priority (Mi.priority_to_string f_new.file_priority);
                end;
              if (f.file_active_sources, f.file_all_sources) <> (f_new.file_active_sources, f_new.file_all_sources)
                then begin
                  store#set ~row ~column:download_sources (Printf.sprintf "(%d / %d)" f_new.file_active_sources f_new.file_all_sources);
                end
            end

        | (Source (s, file_num), Source (s_new, file_num_new)) when file_num = file_num_new ->
           (try
              let (_, i) = self#find_item (File_num file_num) in
              let f = match i with File f -> f | _ -> raise Exit in
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
                      store#set ~row ~column:download_availability (Mi.string_of_availability new_availability f.file_chunks);
                      store#set ~row ~column:download_availability_pixb (Mi.availability_bar new_availability f.file_chunks false)
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
              col#set_cell_data_func renderer
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
                          let item = self#get_item row in
                          begin
                            match item with
                                File f ->
                                  renderer#set_properties [ `EDITABLE true ];
                                  ignore (renderer#connect#edited ~callback:
                                    (fun path name ->
                                       try
                                         let iter = self#get_iter path in
                                         let item = self#get_item iter in
                                         match item with
                                             File ff ->
                                               let child_row = self#convert_iter_to_child_iter iter in
                                               store#set ~row:child_row ~column:download_name ff.file_name;
                                               (match ff.file_state with
                                                   FileDownloaded  -> GuiCom.send (SaveFile (ff.file_num, name))
                                                 | _  ->  GuiCom.send (RenameFile (ff.file_num, name)))
                                           | _ -> ()

                                       with _ -> ()
                                  ))

                              | _ -> renderer#set_properties [ `EDITABLE false ]

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
                               let pixb = GdkPixbuf.create ~width ~height ~has_alpha:false () in
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

    method sort_items c i1 i2 =
      match (i1, i2) with
          (File f1, File f2) ->
             begin
               match c with
                   Col_file_name -> compare (String.lowercase f1.file_name) (String.lowercase f2.file_name)
                 | Col_file_size -> compare f1.file_size f2.file_size
                 | Col_file_downloaded -> compare f1.file_downloaded f2.file_downloaded
                 | Col_file_percent -> compare (float_of_string (Mi.get_percent_of f1.file_downloaded f1.file_size))
                                               (float_of_string (Mi.get_percent_of f2.file_downloaded f2.file_size))
                 | Col_file_rate -> compare f1.file_download_rate f2.file_download_rate
                 | Col_file_state -> compare (Mi.string_of_file_state f1.file_state f1.file_download_rate)
                                             (Mi.string_of_file_state f2.file_state f2.file_download_rate)
                 | Col_file_availability ->
                     begin
                       let av1 = Mi.main_availability_of f1.file_network f1.file_availability in
                       let av2 = Mi.main_availability_of f2.file_network f2.file_availability in
                       if !!O.gtk_look_graphical_availability
                         then begin
                           Mi.sort_availability_bar (av1, f1.file_chunks) (av2, f2.file_chunks) true
                         end else begin
                           let s1 = Mi.string_of_availability av1 f1.file_chunks in
                           let s2 = Mi.string_of_availability av2 f2.file_chunks in
                           compare (float_of_string s1) (float_of_string s2)
                         end
                     end
                 | Col_file_uid -> compare (Mi.uid_list_to_string f1.file_uids) (Mi.uid_list_to_string f2.file_uids)
                 | Col_file_format -> compare f1.file_format f2.file_format
                 | Col_file_network -> compare f1.file_network f2.file_network
                 | Col_file_age -> compare f1.file_age f2.file_age
                 | Col_file_last_seen -> compare f1.file_last_seen f2.file_last_seen
                 | Col_file_eta -> compare (Mi.calc_eta_inst f1.file_size f1.file_downloaded f1.file_download_rate)
                                           (Mi.calc_eta_inst f2.file_size f2.file_downloaded f2.file_download_rate)
                 | Col_file_priority -> compare f1.file_priority f2.file_priority
                 | Col_file_comment -> compare f1.file_comment f2.file_comment
                 | Col_file_sources ->
                     begin
                       let i = compare f1.file_active_sources f2.file_active_sources in
                       if i = 0
                         then compare f1.file_all_sources f2.file_all_sources
                         else i
                     end
             end

        | (Source (s1, file_num_a), Source (s2, file_num_b)) when file_num_a = file_num_b ->
             begin
               match c with
                   Col_file_name -> compare (String.lowercase s1.source_name)
                                            (String.lowercase s2.source_name)
                 | Col_file_downloaded -> compare s1.source_downloaded s2.source_downloaded
                 | Col_file_percent -> compare s1.source_software s2.source_software
                 | Col_file_state -> compare (Mi.string_of_state s1.source_state file_num_a)
                                             (Mi.string_of_state s2.source_state file_num_a)
                 | Col_file_availability ->
                     begin
                       try
                         let (_, i1) = self#find_item (File_num file_num_a) in
                         let (_ ,i2) = self#find_item (File_num file_num_b) in
                         let f1 = match i1 with File f -> f | _ -> raise Exit in
                         let f2 = match i2 with File f -> f | _ -> raise Exit in
                         let av_s1 = try List.assoc file_num_a s1.source_availability with _ -> "" in
                         let av_s2 = try List.assoc file_num_a s2.source_availability with _ -> "" in
                         if !!O.gtk_look_graphical_availability
                           then begin
                             Mi.sort_availability_bar (av_s1, f1.file_chunks) (av_s2, f2.file_chunks) false
                           end else begin
                             let av1 = Mi.string_of_availability av_s1 f1.file_chunks in
                             let av2 = Mi.string_of_availability av_s2 f2.file_chunks in
                             compare (float_of_string av1) (float_of_string av2)
                           end
                       with _ -> 0
                     end
                 | Col_file_network -> compare s1.source_network s2.source_network
                 | Col_file_rate -> compare s1.source_download_rate s2.source_download_rate
                 | _ -> 0
             end

        | _ -> 0

(*************************************************************************)
(*                                                                       *)
(*                         force_update_icons                            *)
(*                                                                       *)
(*************************************************************************)

    method force_update_icons () =
      List.iter (fun i ->
        try
          let (row, i_) = self#find_item (get_key i) in 
          match i with
              File f ->
                 begin
                   store#set ~row ~column:download_network_pixb (Mi.network_pixb f.file_network ~size:A.SMALL ());
                   store#set ~row ~column:download_name_pixb (Mi.file_type_of_name f.file_name ~size:A.SMALL);
                 end
            | Source (s, file_num) ->
                begin
                  store#set ~row ~column:download_network_pixb (Mi.network_pixb s.source_network ~size:A.SMALL ());
                  store#set ~row ~column:download_name_pixb (Mi.source_type_to_icon s.source_type ~size:A.SMALL);
                end
        with _ -> ()
      ) (self#all_items ())

(*************************************************************************)
(*                                                                       *)
(*                         force_update_avail_bars                       *)
(*                                                                       *)
(*************************************************************************)

    method force_update_avail_bars () =
      List.iter (fun i ->
        try
          let (row, _) = self#find_item (get_key i) in
          if !!O.gtk_look_graphical_availability
            then match i with
                     File f ->
                       begin
                         let availability = Mi.main_availability_of f.file_network f.file_availability in
                         store#set ~row ~column:download_availability_pixb (Mi.availability_bar availability f.file_chunks true);
                       end
                   | Source (s, file_num) ->
                       begin
                         let (_, i') = self#find_item (File_num file_num) in
                         let f = match i' with File f -> f | _ -> raise Exit in
                         let availability =
                           try
                             List.assoc file_num s.source_availability
                           with _ -> ""
                         in
                         store#set ~row ~column:download_availability_pixb (Mi.availability_bar availability f.file_chunks false);
                       end

            else store#set ~row ~column:download_availability_pixb None
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

let preview f () = GuiCom.send (Preview f.file_num)

let show_details item () = GuiInfoWindow.window item ()

let pause_resume sel () =
  List.iter (fun item ->
    match item with
        File f ->
           GuiCom.send (SwitchDownload (f.file_num,
              match f.file_state with
                FilePaused | FileAborted _ -> true
              | _ -> false
           ))
      | _ -> ()
  ) sel

let retry_connect sel () =
  List.iter (fun item ->
    match item with
        File f ->
           GuiCom.send (ConnectAll f.file_num)
      | _ -> ()
  ) sel

let cancel sel () =
  let warning_message = !M.dT_lb_ask_cancel_download_files in
  let l = ref [] in
  List.iter (fun item ->
    match item with
        File f ->
          l := f.file_name :: !l
      | _ -> ()
  ) sel;
  let on_ok () =
    List.iter (fun item ->
      match item with
          File f ->
            (* Printf.printf "ASK to Cancel file : %d\n" f.file_num;
            flush stdout; *)
            GuiCom.send (RemoveDownload_query f.file_num)
        | _ -> ()
    ) sel
  in
  GuiTools.warning_box ~warning_message ~text:!l ~on_ok ()

let verify_chunks sel () =
  List.iter (fun item ->
    match item with
        File f ->
           GuiCom.send (VerifyAllChunks f.file_num)
      | _ -> ()
  ) sel

let set_priority sel prio () =
  List.iter (fun item ->
    match item with
        File f ->
           GuiCom.send (SetFilePriority (f.file_num, prio))
      | _ -> ()
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
  match f.file_format with
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
            GuiCom.send (ModifyMp3Tags (f.file_num, tag))
          in
          C.simple_panel
              ~prefs:[title;artist;album;year;comment;tracknum;genre]
              ~title:!M.dT_wt_edit_mp3
              ~icon:(A.get_icon ~icon:M.icon_menu_search_mp3 ~size:A.SMALL ())
              ~on_ok ()
        end

    | _ -> ()

let save_all () =
  List.iter (fun i ->
    match i with
        File f ->
          (match f.file_state with
              FileDownloaded -> 
                GuiCom.send (SaveFile (f.file_num, f.file_name))
            | _ -> ())

      | _ -> ()

  ) (downloadstore#all_items ())

(*
let save_as f () =
  let file_opt = GToolbox.input_string ~title:(!M.dT_wt_save_as)
    ~text:(U.utf8_of f.file_name)
    (!M.dT_lb_save_as)
  in
  match file_opt with
    None -> ()
  | Some name -> 
      match f.file_state with
           FileDownloaded  -> GuiCom.send (SaveFile (f.file_num, name))
        | _  ->  GuiCom.send (RenameFile (f.file_num, name))
*)

let add_to_friends sel () =
  List.iter (fun item ->
    match item with
        Source (s, _) ->
           GuiCom.send (AddClientFriend s.source_num)
      | _ -> ()
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

let get_format sel () =
  List.iter (fun item ->
    match item with
        File f ->
           GuiCom.send (QueryFormat f.file_num)
      | _ -> ()
  ) sel

(*************************************************************************)
(*                                                                       *)
(*                         Download Menu                                 *)
(*                                                                       *)
(*************************************************************************)

let download_menu sel =
    match sel with
        [] -> []
      | (File f) :: tail ->
          (if tail = []
             then
               [
                `I ((!M.dT_me_show_file_details), show_details (File f)) ;
                `I ((!M.dT_me_preview), preview f) ;
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
             (match (f.file_state, f.file_format) with
                  (FileDownloaded, MP3 _) ->
               [
                `I ((!M.dT_me_edit_mp3), edit_mp3_tags f)
               ]
                | _ ->
               [])
             @
                `I ((!M.dT_me_save_all), save_all) :: [] (*
                (if tail = [] then
               [
                `I ((!M.dT_me_save_as), save_as f) ;
               ]
            else  []) *)

      | (Source (s, file_num)) :: tail ->
          if s.source_num = (-1)
            then []
            else
              (if tail = []
                 then
                   [
                    `I ((!M.dT_me_show_source_details), show_details (Source (s, file_num))) ;
                   ]
                 else  [])
              @
                   [
                    `I (!M.dT_me_browse_files, browse_files sel);
                    `I (!M.dT_me_add_to_friends, add_to_friends sel)
                   ]

(*************************************************************************)
(*                                                                       *)
(*                         filter_download                               *)
(*                                                                       *)
(*************************************************************************)

let filter_download i = 
  match i with
      File f -> not (List.memq f.file_network !G.networks_filtered)
    | Source (s, _) -> not (List.memq s.source_network !G.networks_filtered)(*  &&
                       s.source_state <> NewHost &&
                       (s.source_name <> "" && s.source_software <> "unk") *)

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
      List.iter (fun i ->
        match i with
            File f ->
              begin
                let availability = Mi.main_availability_of f.file_network f.file_availability in
                let key = (availability, f.file_chunks, true) in
                if not (List.mem key !list)
                  then begin list := key :: !list end;
                match f.file_sources with
                    None -> ()
                  | Some sources ->
                      List.iter (fun source_num ->
                        let s = Hashtbl.find G.sources source_num in
                        List.iter (fun (_, avail) ->
                          let key = (avail, f.file_chunks, false) in
                          if not (List.mem key !list)
                            then list := key :: !list
                        ) s.source_availability;
                      ) sources
              end

          | _ -> ()

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

let remove_dummy_source file_num =
  downloadstore#remove_item (Source (dummy_source, file_num))

let add_dummy_source parent file_num =
  try
    let _ = downloadstore#find_item (Source_num ((-1), file_num)) in
    ()
  with _ ->
    ignore (downloadstore#add_item (Source (dummy_source, file_num)) ~parent ())

let h_cancelled file_num =
  try
    let (_, i) = downloadstore#find_item (File_num file_num) in
    match i with
        File f ->
          begin
            downloadstore#remove_item (File f);
            decr G.ndownloads;
            (if f.file_state = FileDownloaded
               then decr G.ndownloaded);
            GuiStatusBar.update_downloadedfiles ()
          end
      | _ -> ()
  with _ -> ()

let h_paused f =
  try
    let (row, i) = downloadstore#find_item (File_num f.file_num) in
    match i with
        File fi ->
          begin
            (if f.file_state = FileDownloaded && f.file_state <> fi.file_state
               then incr G.ndownloaded);
            let f_new = {f with file_sources = fi.file_sources} in
            downloadstore#update_item row (File fi) (File f_new)
          end
        | _ -> ()
  with _ ->
    begin
      let parent = downloadstore#add_item (File f) () in
      incr G.ndownloads;
      (if f.file_state = FileDownloaded
         then incr G.ndownloaded);
      GuiStatusBar.update_downloadedfiles ();
      add_dummy_source parent f.file_num
    end

let h_downloading = h_paused

let h_downloaded = h_paused

let h_removed = h_cancelled


let file_info f =
  match f.file_state with
      FileNew -> assert false
    | FileCancelled -> 
        h_cancelled f.file_num
    | FileDownloaded ->
        h_downloaded f
    | FileShared ->
        h_removed f.file_num
    | FilePaused | FileQueued | FileAborted _ -> 
        h_paused f
    | FileDownloading ->
        h_downloading f

let file_downloaded (num, downloaded, rate, last_seen) =
  try
    let (row, i) = downloadstore#find_item (File_num num) in
    match i with
        File f ->
          begin
            let f_new = {f with file_downloaded = downloaded;
                                file_download_rate = rate;
                                file_last_seen = last_seen}
            in
            downloadstore#update_item row (File f) (File f_new)
          end
      | _ -> ()
  with _ -> ()

(* File_add_source and File_update_availability could be merged now that we use
 * InterestedInSources. Pb how to retrieve the chunks of a source ?
 *)
let h_add_source s file_num =
  try
    let (row, i) = downloadstore#find_item (File_num file_num) in
    match i with
        File f ->
          begin
            match f.file_sources with
                None ->
                  begin
                    f.file_sources <- Some [s.source_num];
                    ignore (downloadstore#add_item (Source (s, file_num)) ~parent:row ());
                    remove_dummy_source file_num
                  end
              | Some sources ->
                  begin
                    if List.mem s.source_num sources
                      then raise Exit
                      else begin
                        f.file_sources <- Some (s.source_num :: sources);
                        ignore (downloadstore#add_item (Source (s, file_num)) ~parent:row ());
                      end
                  end
          end
      | _ -> ()
  with _ -> ()

let h_remove_source s file_num =
  try
    let (row, i) = downloadstore#find_item (File_num file_num) in
    match i with
        File f ->
          begin
            let _ =
              match f.file_sources with
                  None -> add_dummy_source row file_num
                | Some sources ->
                    begin
                      f.file_sources <- Some (List.filter (fun num -> num <> s.source_num) sources)
                    end
            in
            (match f.file_sources with
                Some [] ->
                  begin
                    f.file_sources <- None;
                    add_dummy_source row file_num;
                  end
              | _ -> ());
            downloadstore#remove_item (Source (s, file_num));
          end
      | _ -> ()
  with _ -> ()

let h_update_source s_new =
  List.iter (fun file_num ->
    try
      let (row, i) = downloadstore#find_item (Source_num (s_new.source_num, file_num)) in
      match i with
          Source (s, num) ->
            downloadstore#update_item row (Source (s, num)) (Source (s_new, num))
        | _ -> ()
    with _ -> ()
  ) s_new.source_files_requested;
  match s_new.source_state with
      Connected_downloading _ ->
        if not (List.mem s_new.source_num !downloaders)
          then downloaders := s_new.source_num :: !downloaders
    | _ -> ()

let h_update_source_availability s_new file_num =
   try
     let (row, i) = downloadstore#find_item (Source_num (s_new.source_num, file_num)) in
     match i with
         Source (s, num) ->
           downloadstore#update_item row (Source (s, num)) (Source (s_new, num))
       | _ -> ()
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
  let vbox = GPack.vbox ~homogeneous:false ~border_width:6 () in
  ignore (vbox#connect#destroy ~callback:
    (fun _ ->
       view_context := None;
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
  let on_expand_item path (i : GuiTypes2.item_info) =
    ask_for_sources ();
    false
  in
  let on_expanded_item path (i : GuiTypes2.item_info) =
    expanded_rows := List.length downloadview#expanded_paths;
    ( if !!verbose then lprintf' "rows expanded %d\n" !expanded_rows)
  in
  let on_collapsed_item path (i : GuiTypes2.item_info) =
    expanded_rows := List.length downloadview#expanded_paths;
    ( if !!verbose then lprintf' "rows expanded %d\n" !expanded_rows)
  in
  view_context := Some downloadview#view#misc#pango_context;
  downloadview#set_model downloadstore#gmodel;
  downloadview#set_menu download_menu;
  downloadview#set_on_expand on_expand_item;
  downloadview#set_on_expanded on_expanded_item;
  downloadview#set_on_collapsed on_collapsed_item;
  update_downloaders ();
  downloaders_timerID := Timeout.add ~ms:6000 ~callback:
    (fun _ ->
      ( if !!verbose then lprintf' "Update downloaders\n");
      update_downloaders ();
      true);
  ignore (entry#event#connect#key_press ~callback:
    (fun ev ->
       GdkEvent.Key.keyval ev = GdkKeysyms._Return &&
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

