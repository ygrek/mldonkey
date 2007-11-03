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

(* The uploads window of MLgui *)

open GuiTypes2
open GuiTypes
open CommonTypes
open GraphTypes

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

let verbose = O.gtk_verbose_uploads

let lprintf' fmt =
  Printf2.lprintf ("GuiUploads: " ^^ fmt)

(*************************************************************************)
(*                                                                       *)
(*                         Global variables                              *)
(*                                                                       *)
(*************************************************************************)

let (upload_label : GMisc.label option ref) = ref None
let (uploader_label : GMisc.label option ref) = ref None
let nuploading = ref 0
let show_pending = ref false
let (view_context : GPango.context option ref) = ref None
let uploaders_n_pendings = ref []

(*************************************************************************)
(*                                                                       *)
(*                         shared_file_num                               *)
(*                                                                       *)
(*************************************************************************)

let shared_file_num key =
  try int_of_string key with _ -> raise Not_found

(*************************************************************************)
(*                                                                       *)
(*                         shared_file_of_key                            *)
(*                                                                       *)
(*************************************************************************)

let shared_file_of_key key =
  try
    let num = shared_file_num key in
    Hashtbl.find G.shared_files num
  with _ -> raise Not_found

(*************************************************************************)
(*                                                                       *)
(*                         keys_to_shared_files                          *)
(*                                                                       *)
(*************************************************************************)

let keys_to_shared_files keys =
  let l = ref [] in
  List.iter (fun k ->
    try
      let s = shared_file_of_key k in
      l := s :: !l
    with _ -> ()) keys;
  !l

(*************************************************************************)
(*                                                                       *)
(*                         shared_file_key                               *)
(*                                                                       *)
(*************************************************************************)

let shared_file_key shared_file_num =
  Printf.sprintf "%d" shared_file_num

(*************************************************************************)
(*                                                                       *)
(*                         uploader_num                                  *)
(*                                                                       *)
(*************************************************************************)

let uploader_num key =
  try int_of_string key with _ -> raise Not_found

(*************************************************************************)
(*                                                                       *)
(*                         uploader_of_key                               *)
(*                                                                       *)
(*************************************************************************)

let uploader_of_key key =
  try
    let num = uploader_num key in
    Hashtbl.find G.sources num
  with _ -> raise Not_found

(*************************************************************************)
(*                                                                       *)
(*                         keys_to_uploaders                             *)
(*                                                                       *)
(*************************************************************************)

let keys_to_uploaders keys =
  let l = ref [] in
  List.iter (fun k ->
    try
      let s = uploader_of_key k in
      l := s :: !l
    with _ -> ()) keys;
  !l

(*************************************************************************)
(*                                                                       *)
(*                         uploader_key                                  *)
(*                                                                       *)
(*************************************************************************)

let uploader_key uploader_num =
  Printf.sprintf "%d" uploader_num

(*************************************************************************)
(*                                                                       *)
(*                         Templates                                     *)
(*                                                                       *)
(*************************************************************************)

module Uploads = GuiTemplates.Gview(struct

  module Column = GuiColumns.Shared_files_up

  type item = shared_file

  let columns = O.shared_files_up_columns
  let get_key = (fun si -> (shared_file_key si.g_shared_num))
  let module_name = "Uploads"

end)

class g_shared () =
  let shared_cols         = new GTree.column_list in
  let shared_network_str  = shared_cols#add Gobject.Data.string in
  let shared_network_pixb = shared_cols#add Gobject.Data.gobject_option in
  let shared_name         = shared_cols#add Gobject.Data.string in
  let shared_name_pixb    = shared_cols#add Gobject.Data.gobject_option in
  let shared_size_str     = shared_cols#add Gobject.Data.string in
  let shared_uploaded_str = shared_cols#add Gobject.Data.string in
  let shared_requests     = shared_cols#add Gobject.Data.int in
  let shared_uid          = shared_cols#add Gobject.Data.string in
  object (self)

    inherit Uploads.g_list shared_cols

(*************************************************************************)
(*                                                                       *)
(*                         from_item                                     *)
(*                                                                       *)
(*************************************************************************)

    method from_item row (si : shared_file) =
      store#set ~row ~column:shared_network_str (Mi.network_name si.g_shared_network);
      store#set ~row ~column:shared_network_pixb (Mi.network_pixb si.g_shared_network ~size:A.SMALL ());
      store#set ~row ~column:shared_name (U.utf8_of si.g_shared_filename);
      store#set ~row ~column:shared_name_pixb (Mi.file_type_of_name si.g_shared_filename ~size:A.SMALL);
      store#set ~row ~column:shared_size_str (Mi.size_of_int64 si.g_shared_size);
      store#set ~row ~column:shared_uploaded_str (Mi.size_of_int64 si.g_shared_uploaded);
      store#set ~row ~column:shared_requests si.g_shared_requests;
      store#set ~row ~column:shared_uid (Mi.uid_list_to_string si.g_shared_uids)

(*************************************************************************)
(*                                                                       *)
(*                         from_new_item                                 *)
(*                                                                       *)
(*************************************************************************)

    method from_new_item (row : Gtk.tree_iter) (si : shared_file) (si_new : shared_file) =
      if si.g_shared_filename <> si_new.g_shared_filename
        then begin
          store#set ~row ~column:shared_name (U.utf8_of si_new.g_shared_filename);
          if (Mi.extension_of si.g_shared_filename) <> (Mi.extension_of si_new.g_shared_filename)
            then begin
              store#set ~row ~column:shared_name_pixb (Mi.file_type_of_name si_new.g_shared_filename ~size:A.SMALL);
            end
        end;
      if si.g_shared_size <> si_new.g_shared_size
        then begin
          store#set ~row ~column:shared_size_str (Mi.size_of_int64 si_new.g_shared_size);
        end;
      if si.g_shared_uploaded <> si_new.g_shared_uploaded
        then begin
          store#set ~row ~column:shared_uploaded_str (Mi.size_of_int64 si_new.g_shared_uploaded);
        end;
      if si.g_shared_requests <> si_new.g_shared_requests
        then begin
          store#set ~row ~column:shared_requests si_new.g_shared_requests;
        end;
      if si.g_shared_uids <> si_new.g_shared_uids
        then begin
          store#set ~row ~column:shared_uid (Mi.uid_list_to_string si_new.g_shared_uids)
        end

(*************************************************************************)
(*                                                                       *)
(*                         content                                       *)
(*                                                                       *)
(*************************************************************************)

    method content (col : GTree.view_column) c =
      let autosize = match col#sizing with `AUTOSIZE -> true | _ -> false in
      match c with
          Col_shared_file ->
            begin
              if !!O.gtk_look_use_icons
                then begin
                  let renderer = GTree.cell_renderer_pixbuf [`XALIGN 0.;`XPAD 4] in
                  col#pack ~expand:false renderer;
                  col#add_attribute renderer "pixbuf" shared_name_pixb
                end;
              let renderer = GTree.cell_renderer_text [`XALIGN 0.] in
              col#pack ~expand:false renderer;
              if autosize
                then col#add_attribute renderer "text" shared_name
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
                           let name = model#get ~row ~column:shared_name in
                           let s = GuiTools.fit_string_to_pixels name ~context ~pixels:width in
                           renderer#set_properties [ `TEXT s ]
                         end
                     | _ -> renderer#set_properties [ `TEXT "" ]
                  )
            end

        | Col_shared_upsize ->
            begin
              let renderer = GTree.cell_renderer_text [`XALIGN 1.] in
              col#pack renderer;
              col#add_attribute renderer "text" shared_uploaded_str
            end

        | Col_shared_requests ->
            begin
              let renderer = GTree.cell_renderer_text [`XALIGN 1.] in
              col#pack renderer;
              col#add_attribute renderer "text" shared_requests
            end

        | Col_shared_size ->
            begin
              let renderer = GTree.cell_renderer_text [`XALIGN 1.] in
              col#pack renderer;
              col#add_attribute renderer "text" shared_size_str
            end

        | Col_shared_uid ->
            begin
              let renderer = GTree.cell_renderer_text [`XALIGN 0.] in
              col#pack renderer;
              col#add_attribute renderer "text" shared_uid
            end

        | Col_shared_network ->
            begin
              if !!O.gtk_look_use_icons
                then begin
                  let renderer = GTree.cell_renderer_pixbuf [`XALIGN 0.] in
                  col#pack renderer;
                  col#add_attribute renderer "pixbuf" shared_network_pixb
                end else begin
                  let renderer = GTree.cell_renderer_text [`XALIGN 0.] in
                  col#pack renderer;
                  col#add_attribute renderer "text" shared_network_str
                end
            end

(*************************************************************************)
(*                                                                       *)
(*                         sort_items                                    *)
(*                                                                       *)
(*************************************************************************)

    method sort_items c k1 k2 =
      try
        let si1 = shared_file_of_key k1 in
        let si2 = shared_file_of_key k2 in
        match c with
          Col_shared_file -> compare si1.g_shared_filename si2.g_shared_filename
        | Col_shared_network -> compare si1.g_shared_network si2.g_shared_network
        | Col_shared_upsize -> compare si1.g_shared_uploaded si2.g_shared_uploaded
        | Col_shared_requests -> compare si1.g_shared_requests si2.g_shared_requests
        | Col_shared_size -> compare si1.g_shared_size si2.g_shared_size
        | Col_shared_uid -> compare (Mi.uid_list_to_string si1.g_shared_uids) (Mi.uid_list_to_string si2.g_shared_uids)
      with _ -> 0

(*************************************************************************)
(*                                                                       *)
(*                         force_update_icons                            *)
(*                                                                       *)
(*************************************************************************)

    method force_update_icons () =
      let f k row =
        let si = shared_file_of_key k in
        store#set ~row ~column:shared_network_pixb (Mi.network_pixb si.g_shared_network ~size:A.SMALL ());
        store#set ~row ~column:shared_name_pixb (Mi.file_type_of_name si.g_shared_filename ~size:A.SMALL);
      in
      List.iter (fun k ->
        try
          let row = self#find_row k in
          Gaux.may ~f:(f k) row
        with _ -> ()
      ) (self#all_items ())

  end

module Uploaders = GuiTemplates.Gview(struct

  module Column = GuiColumns.Client

  type item = source_info

  let columns = O.uploaders_columns
  let get_key = (fun s -> (uploader_key s.source_num))
  let module_name = "Uploaders"

end)

class g_uploader () =
  let uploaders_cols             = new GTree.column_list in
  let uploader_network_str       = uploaders_cols#add Gobject.Data.string in
  let uploader_network_pixb      = uploaders_cols#add Gobject.Data.gobject_option in
  let uploader_name              = uploaders_cols#add Gobject.Data.string in
  let uploader_name_pixb         = uploaders_cols#add Gobject.Data.gobject_option in
  let uploader_type              = uploaders_cols#add Gobject.Data.string in
  let uploader_state             = uploaders_cols#add Gobject.Data.string in
  let uploader_kind              = uploaders_cols#add Gobject.Data.string in
  let uploader_rating            = uploaders_cols#add Gobject.Data.int in
  let uploader_connect_time_str  = uploaders_cols#add Gobject.Data.string in
  let uploader_soft              = uploaders_cols#add Gobject.Data.string in
  let uploader_uploaded_str      = uploaders_cols#add Gobject.Data.string in
  let uploader_downloaded_str    = uploaders_cols#add Gobject.Data.string in
  let uploader_upload_rate_str   = uploaders_cols#add Gobject.Data.string in
  let uploader_download_rate_str = uploaders_cols#add Gobject.Data.string in
  let uploader_upload            = uploaders_cols#add Gobject.Data.string_option in
  object (self)

    inherit Uploaders.g_list uploaders_cols

(*************************************************************************)
(*                                                                       *)
(*                         from_item                                     *)
(*                                                                       *)
(*************************************************************************)

    method from_item row (s : source_info) =
      store#set ~row ~column:uploader_network_str (Mi.network_name s.source_network);
      store#set ~row ~column:uploader_network_pixb (Mi.network_pixb s.source_network ~size:A.SMALL ());
      store#set ~row ~column:uploader_name s.source_name;
      store#set ~row ~column:uploader_name_pixb (Mi.source_type_to_icon s.source_type ~size:A.SMALL);
      store#set ~row ~column:uploader_type (Mi.source_type_to_string s.source_type);
      store#set ~row ~column:uploader_state (Mi.uploader_state_to_string s.source_state s.source_has_upload);
      store#set ~row ~column:uploader_kind (Mi.location_kind_to_string s.source_kind);
      store#set ~row ~column:uploader_rating s.source_rating;
      store#set ~row ~column:uploader_connect_time_str (Mi.time_to_string s.source_connect_time);
      store#set ~row ~column:uploader_soft s.source_software;
      store#set ~row ~column:uploader_uploaded_str (Mi.size_of_int64 s.source_uploaded);
      store#set ~row ~column:uploader_downloaded_str (Mi.size_of_int64 s.source_downloaded);
      store#set ~row ~column:uploader_upload_rate_str (Mi.rate_to_string s.source_upload_rate);
      store#set ~row ~column:uploader_download_rate_str (Mi.rate_to_string s.source_download_rate);
      store#set ~row ~column:uploader_upload (match s.source_upload with None -> None | Some s -> Some (U.utf8_of s))

(*************************************************************************)
(*                                                                       *)
(*                         from_new_item                                 *)
(*                                                                       *)
(*************************************************************************)

    method from_new_item (row : Gtk.tree_iter) (s : source_info) (s_new : source_info) =
      if s.source_name <> s_new.source_name
        then begin
          store#set ~row ~column:uploader_name s_new.source_name;
        end;
      if s.source_type <> s_new.source_type
        then begin
          store#set ~row ~column:uploader_name_pixb (Mi.source_type_to_icon s_new.source_type ~size:A.SMALL);
          store#set ~row ~column:uploader_type (Mi.source_type_to_string s_new.source_type);
        end;
      if (s.source_state, s.source_has_upload) <> (s_new.source_state, s_new.source_has_upload)
        then begin
          store#set ~row ~column:uploader_state (Mi.uploader_state_to_string s_new.source_state s_new.source_has_upload);
        end;
      if s.source_kind <> s_new.source_kind
        then begin
          store#set ~row ~column:uploader_kind (Mi.location_kind_to_string s_new.source_kind);
        end;
      if s.source_rating <> s_new.source_rating
        then begin
          store#set ~row ~column:uploader_rating s_new.source_rating;
        end;
      if s.source_connect_time <> s_new.source_connect_time
        then begin
           store#set ~row ~column:uploader_connect_time_str (Mi.time_to_string s_new.source_connect_time);
        end;
      if s.source_software <> s_new.source_software
        then begin
          store#set ~row ~column:uploader_soft s_new.source_software;
        end;
      if s.source_uploaded <> s_new.source_uploaded
        then begin
          store#set ~row ~column:uploader_uploaded_str (Mi.size_of_int64 s_new.source_uploaded);
        end;
      if s.source_downloaded <> s_new.source_downloaded
        then begin
          store#set ~row ~column:uploader_downloaded_str (Mi.size_of_int64 s_new.source_downloaded);
        end;
      if s.source_upload_rate <> s_new.source_upload_rate
        then begin
          store#set ~row ~column:uploader_upload_rate_str (Mi.rate_to_string s_new.source_upload_rate);
        end;
      if s.source_download_rate <> s_new.source_download_rate
        then begin
          store#set ~row ~column:uploader_download_rate_str (Mi.rate_to_string s_new.source_download_rate);
        end;
      if s.source_upload <> s_new.source_upload
        then begin
          store#set ~row ~column:uploader_upload (match s_new.source_upload with None -> None | Some s -> Some (U.utf8_of s));
        end

(*************************************************************************)
(*                                                                       *)
(*                         content                                       *)
(*                                                                       *)
(*************************************************************************)

    method content (col : GTree.view_column) c =
      let autosize = match col#sizing with `AUTOSIZE -> true | _ -> false in
      match c with
          Col_client_name ->
            begin
              if !!O.gtk_look_use_icons
                then begin
                  let renderer = GTree.cell_renderer_pixbuf [`XALIGN 0.;`XPAD 4] in
                  col#pack ~expand:false renderer;
                  col#add_attribute renderer "pixbuf" uploader_name_pixb
                end;
              let renderer = GTree.cell_renderer_text [`XALIGN 0.] in
              col#pack ~expand:false renderer;
              if autosize
                then col#add_attribute renderer "text" uploader_name
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
                           let name = model#get ~row ~column:uploader_name in
                           let s = GuiTools.fit_string_to_pixels name ~context ~pixels:width in
                           renderer#set_properties [ `TEXT s ]
                         end
                     | _ -> renderer#set_properties [ `TEXT "" ]
                  )
            end

        | Col_client_state ->
            begin
              let renderer = GTree.cell_renderer_text [`XALIGN 0.] in
              col#pack renderer;
              col#add_attribute renderer "text" uploader_state
            end

        | Col_client_kind ->
            begin
              let renderer = GTree.cell_renderer_text [`XALIGN 0.] in
              col#pack renderer;
              col#add_attribute renderer "text" uploader_kind
            end

        | Col_client_type ->
            begin
              let renderer = GTree.cell_renderer_text [`XALIGN 0.] in
              col#pack renderer;
              col#add_attribute renderer "text" uploader_type
            end

        | Col_client_rating ->
            begin
              let renderer = GTree.cell_renderer_text [`XALIGN 1.] in
              col#pack renderer;
              col#add_attribute renderer "text" uploader_rating
            end

        | Col_client_connect_time ->
            begin
              let renderer = GTree.cell_renderer_text [`XALIGN 1.] in
              col#pack renderer;
              col#add_attribute renderer "text" uploader_connect_time_str
            end

        | Col_client_software ->
            begin
              let renderer = GTree.cell_renderer_text [`XALIGN 1.] in
              col#pack renderer;
              col#add_attribute renderer "text" uploader_soft
            end

        | Col_client_downloaded ->
            begin
              let renderer = GTree.cell_renderer_text [`XALIGN 1.] in
              col#pack renderer;
              col#add_attribute renderer "text" uploader_downloaded_str
            end

        | Col_client_uploaded ->
            begin
              let renderer = GTree.cell_renderer_text [`XALIGN 1.] in
              col#pack renderer;
              col#add_attribute renderer "text" uploader_uploaded_str
            end

        | Col_client_download_rate ->
            begin
              let renderer = GTree.cell_renderer_text [`XALIGN 1.] in
              col#pack renderer;
              col#add_attribute renderer "text" uploader_download_rate_str
            end

        | Col_client_upload_rate ->
            begin
              let renderer = GTree.cell_renderer_text [`XALIGN 1.] in
              col#pack renderer;
              col#add_attribute renderer "text" uploader_upload_rate_str
            end

        | Col_client_upload ->
            begin
              let renderer = GTree.cell_renderer_text [`XALIGN 0.] in
              col#pack renderer;
              if autosize
                then col#add_attribute renderer "text" uploader_upload
                else col#set_cell_data_func renderer
                  (fun model row ->
                     let upload = model#get ~row ~column:uploader_upload in
                     match (!view_context, upload) with
                       (Some context, Some file_name) when col#width > 0 ->
                         begin
                           let width = col#width - 4 * !G.char_width in
                           let s = GuiTools.fit_string_to_pixels file_name ~context ~pixels:width in
                           renderer#set_properties [ `TEXT s ]
                         end
                     | _ -> renderer#set_properties [ `TEXT "" ]
                  )
            end

        | Col_client_network ->
            begin
              if !!O.gtk_look_use_icons
                then begin
                  let renderer = GTree.cell_renderer_pixbuf [`XALIGN 0.] in
                  col#pack renderer;
                  col#add_attribute renderer "pixbuf" uploader_network_pixb
                end else begin
                  let renderer = GTree.cell_renderer_text [`XALIGN 0.] in
                  col#pack renderer;
                  col#add_attribute renderer "text" uploader_network_str
                end
            end

(*************************************************************************)
(*                                                                       *)
(*                         sort_items                                    *)
(*                                                                       *)
(*************************************************************************)

    method sort_items c k1 k2 =
      try
        let s1 = uploader_of_key k1 in
        let s2 = uploader_of_key k2 in
        match c with
          Col_client_name -> compare (String.lowercase s1.source_name) (String.lowercase s2.source_name)
        | Col_client_state -> compare s1.source_state s2.source_state
        | Col_client_kind -> compare s1.source_kind s2.source_kind
        | Col_client_network -> compare s1.source_network s2.source_network
        | Col_client_type -> compare s1.source_type s2.source_type
        | Col_client_rating -> compare s1.source_rating s2.source_rating
        | Col_client_connect_time -> compare s1.source_connect_time s2.source_connect_time
        | Col_client_software -> compare s1.source_software s2.source_software
        | Col_client_downloaded -> compare s1.source_downloaded s2.source_downloaded
        | Col_client_uploaded -> compare s2.source_uploaded s2.source_uploaded
        | Col_client_download_rate -> compare s1.source_download_rate s2.source_download_rate
        | Col_client_upload_rate -> compare s2.source_upload_rate s2.source_upload_rate
        | Col_client_upload -> compare s1.source_upload s2.source_upload
      with _ -> 0

(*************************************************************************)
(*                                                                       *)
(*                         force_update_icons                            *)
(*                                                                       *)
(*************************************************************************)

    method force_update_icons () =
      let f k row =
        let s = uploader_of_key k in
        store#set ~row ~column:uploader_network_pixb (Mi.network_pixb s.source_network ~size:A.SMALL ());
        store#set ~row ~column:uploader_name_pixb (Mi.source_type_to_icon s.source_type ~size:A.SMALL);
      in
      List.iter (fun k ->
        try
          let row = self#find_row k in
          Gaux.may ~f:(f k) row
        with _ -> ()
      ) (self#all_items ())

  end

let uploadstore = new g_shared ()
let uploaderstore = new g_uploader ()

(*************************************************************************)
(*                                                                       *)
(*                         update_uploads_label                          *)
(*                                                                       *)
(*************************************************************************)

let update_uploads_label () =
  match !upload_label with
      Some label ->
        begin
          let markup =
            create_default_bold_markup
             (Printf.sprintf "%s (%d)" !M.uT_lb_uploads uploadstore#nitems)
          in
          label#set_label markup
        end
    | _ -> ()

(*************************************************************************)
(*                                                                       *)
(*                         update_uploaders_label                        *)
(*                                                                       *)
(*************************************************************************)

let update_uploaders_label () =
  match !uploader_label with
      Some label ->
        begin
          let markup =
            create_default_bold_markup
             (Printf.sprintf "%s (%d / %d)" !M.uT_lb_uploaders !nuploading uploaderstore#nitems)
          in
          label#set_label markup
        end
    | _ -> ()




(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(*                                                                       *)
(*                         UPLOADS                                       *)
(*                                                                       *)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)

(*************************************************************************)
(*                                                                       *)
(*                         message to the core                           *)
(*                                                                       *)
(*************************************************************************)

let copy_ed2k_links sel () = 
  let buf = Buffer.create 100 in
  let l = keys_to_shared_files sel in
  List.iter (fun si ->
    match si.g_shared_uids with
        uid :: _ -> (
          match (Uid.to_uid uid) with
              Ed2k md4 ->
                begin
                  let link = Printf.sprintf "ed2k://|file|%s|%Ld|%s|" 
                             (Url.encode (Filename.basename si.g_shared_filename))
                             si.g_shared_size
                             (Md4.to_string md4)
                  in
                  Printf.bprintf buf "%s\n" link;
                end
            | _ -> ())

      | _ -> ()
  ) l;
  let link = Buffer.contents buf in
  GuiConsole.insert link;
  GMain.clipboard#clear ();
  GMain.clipboard#set_text link

let refresh_uploadstats () =
  GuiCom.send GuiProto.RefreshUploadStats

(*************************************************************************)
(*                                                                       *)
(*                         upload_menu                                   *)
(*                                                                       *)
(*************************************************************************)

let upload_menu sel =
  match sel with
      [] -> []
    | _ ->
          [ `I ((!M.uT_me_copy_ed2k), copy_ed2k_links sel) 
          ] 

(*************************************************************************)
(*                                                                       *)
(*                         filter_upload                                 *)
(*                                                                       *)
(*************************************************************************)

let filter_upload k =
  try
    let si = shared_file_of_key k in
    not (List.memq si.g_shared_network !G.networks_filtered)
  with _ -> true

(*************************************************************************)
(*                                                                       *)
(*                         Templates initialization                      *)
(*                                                                       *)
(*************************************************************************)

let _ =
  uploadstore#set_filter filter_upload

(*************************************************************************)
(*                                                                       *)
(*                         uploads_clear                                 *)
(*                                                                       *)
(*************************************************************************)

let uploads_clear () =
  uploadstore#clear ();
  update_uploads_label ()

(*************************************************************************)
(*                                                                       *)
(*                         message from the core                         *)
(*                                                                       *)
(*************************************************************************)

let hashtbl_update_shared_files si si_new =
  si.g_shared_filename <- si_new.g_shared_filename;
  si.g_shared_size <- si_new.g_shared_size;
  si.g_shared_uploaded <- si_new.g_shared_uploaded;
  si.g_shared_requests <- si_new.g_shared_requests;
  si.g_shared_uids <- si_new.g_shared_uids;
  si.g_shared_last_seen <- si_new.g_shared_last_seen

let add_upload si =
  uploadstore#add_item si ~f:update_uploads_label ();
  Hashtbl.add G.shared_files si.g_shared_num si;
  let si_uid = Mi.to_uid_type si.g_shared_uids in
  GuiGraphBase.add_file si_uid;
  Hashtbl.add G.file_by_uid si_uid (U.utf8_of si.g_shared_filename)

let remove_upload si =
  uploadstore#remove_item (shared_file_key si.g_shared_num);
  Hashtbl.remove G.shared_files si.g_shared_num;
  update_uploads_label ();
  let si_uid = Mi.to_uid_type si.g_shared_uids in
  GuiGraphBase.cancel_file si_uid;
  Hashtbl.remove G.file_by_uid si_uid

let h_shared_file_info si_new =
  try
    let si = Hashtbl.find G.shared_files si_new.g_shared_num in
    let rate = (Int64.to_float si_new.g_shared_uploaded -. Int64.to_float si.g_shared_uploaded) /.
               (si_new.g_shared_last_seen -. si.g_shared_last_seen)
    in
    let row = uploadstore#find_row (shared_file_key si_new.g_shared_num) in
    Gaux.may ~f:(fun r -> uploadstore#update_item r si si_new) row;
    hashtbl_update_shared_files si si_new;
    let rate = int_of_float rate in
    GuiGraphBase.save_record rate (GraphFile ((Mi.to_uid_type si.g_shared_uids), GraphUploads));
  with Not_found -> add_upload si_new

let h_shared_file_upload shared_num upsize requests =
  try
    let si = Hashtbl.find G.shared_files shared_num in
    let row = uploadstore#find_row (shared_file_key shared_num) in
    let si_new = {si with
                  g_shared_uploaded = upsize;
                  g_shared_requests = requests;
                  g_shared_last_seen = BasicSocket.current_time ()
    } in
    let rate = (Int64.to_float si_new.g_shared_uploaded -. Int64.to_float si.g_shared_uploaded) /.
               (si_new.g_shared_last_seen -. si.g_shared_last_seen)
    in
    let rate = int_of_float rate in
    Gaux.may ~f:(fun r -> uploadstore#update_item r si si_new) row;
    si.g_shared_size <- si_new.g_shared_size;
    si.g_shared_uploaded <- si_new.g_shared_uploaded;
    si.g_shared_requests <- si_new.g_shared_requests;
    si.g_shared_last_seen <- si_new.g_shared_last_seen;
    GuiGraphBase.save_record rate (GraphFile ((Mi.to_uid_type si.g_shared_uids), GraphUploads))
  with Not_found ->
    (if !!verbose then lprintf' "Shared file %d not found\n" shared_num)

let h_shared_file_unshared shared_num =
  try
    let si = Hashtbl.find G.shared_files shared_num in
    remove_upload si
  with Not_found ->
    (if !!verbose then lprintf' "Shared file %d not found" shared_num)

(*************************************************************************)
(*                                                                       *)
(*                         message from GuiNetwoks                       *)
(*                                                                       *)
(*************************************************************************)

let reset_uploads_filter () =
  uploadstore#refresh_filter ()



(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(*                                                                       *)
(*                         UPLOADERS                                     *)
(*                                                                       *)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)


(*************************************************************************)
(*                                                                       *)
(*                         message to the core                           *)
(*                                                                       *)
(*************************************************************************)

let add_to_friends sel () =
  let l = keys_to_uploaders sel in
  List.iter (fun s ->
    GuiCom.send (AddClientFriend s.source_num)
  ) l

let browse_files = add_to_friends

let show_details k () =
  try
    let s = uploader_of_key k in
    let item = Source (s, 0) in
    GuiInfoWindow.window item ()
  with _ -> ()

let update_all keys =
  let l = keys_to_uploaders keys in
  List.iter (fun s ->
    GuiCom.send (GetClient_info s.source_num)
  ) l;
  GuiCom.send GetUploaders;
  GuiCom.send GetPending

(*************************************************************************)
(*                                                                       *)
(*                         uploader_menu                                 *)
(*                                                                       *)
(*************************************************************************)

let uploader_menu sel =
  match sel with
      [] -> []
    | k :: tail ->
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

(*************************************************************************)
(*                                                                       *)
(*                         filter_uploader                               *)
(*                                                                       *)
(*************************************************************************)

let filter_uploader k =
  try
    let s = uploader_of_key k in
    not ((not !show_pending && (s.source_has_upload <> source_has_upload)) ||
          List.memq s.source_network !G.networks_filtered)
  with _ -> true

(*************************************************************************)
(*                                                                       *)
(*                         set UploadersList functions                   *)
(*                                                                       *)
(*************************************************************************)

let _ =
  uploaderstore#set_filter filter_uploader

(*************************************************************************)
(*                                                                       *)
(*                         uploaders_clear                               *)
(*                                                                       *)
(*************************************************************************)

let uploaders_clear () =
  uploaderstore#clear ();
  nuploading := 0;
  update_uploaders_label ()

(*************************************************************************)
(*                                                                       *)
(*                         message from the core                         *)
(*                                                                       *)
(*************************************************************************)

let remove_uploader s =
  uploaderstore#remove_item (uploader_key s.source_num);
  (if s.source_has_upload = source_has_upload then decr nuploading);
  s.source_has_upload <- source_only;
  update_uploaders_label ()

let update_uploaders () =
  let keys = uploaderstore#all_items () in
  let all_uploaders = keys_to_uploaders keys in
  List.iter (fun s ->
    if not (List.mem_assoc s.source_num !uploaders_n_pendings)
      then remove_uploader s
  ) all_uploaders;
  List.iter (fun (num, has_upload) ->
    try
      let s = Hashtbl.find G.sources num in
      let s_new = {s with source_has_upload = has_upload} in
      try
        let row = uploaderstore#find_row (uploader_key num) in
        Gaux.may ~f:(fun r -> uploaderstore#update_item r s s_new) row;
        begin
          if s_new.source_has_upload = source_has_upload && s.source_has_upload <> source_has_upload
            then incr nuploading
            else if s_new.source_has_upload <> source_has_upload && s.source_has_upload = source_has_upload
              then decr nuploading
        end;
        s.source_has_upload <- s_new.source_has_upload
      with _ ->
        begin
          s.source_has_upload <- s_new.source_has_upload;
          uploaderstore#add_item s ~f:update_uploaders_label ();
          (if s.source_has_upload = source_has_upload then incr nuploading)
        end
    with _ ->
      begin
        GuiCom.send (GetClient_info num)
      end
  ) !uploaders_n_pendings;
  uploaders_n_pendings := []

let h_update_uploader s s_new =
  try
    let row = uploaderstore#find_row (uploader_key s_new.source_num) in
    Gaux.may ~f:(fun r -> uploaderstore#update_item r s s_new) row
  with _ -> ()

let h_update_uploaders uploaders =
  List.iter (fun uploader_num ->
    uploaders_n_pendings := (uploader_num, source_has_upload) :: !uploaders_n_pendings
  ) uploaders

let h_update_pending_slots pending_slots =
  List.iter (fun uploader_num ->
    uploaders_n_pendings := (uploader_num, source_has_slot) :: !uploaders_n_pendings
  ) pending_slots

let clean_uploaders_table uploader =
  remove_uploader uploader

(*************************************************************************)
(*                                                                       *)
(*                         message from GuiNetwoks                       *)
(*                                                                       *)
(*************************************************************************)

let reset_uploaders_filter () =
  uploaderstore#refresh_filter ()

(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(*                                                                       *)
(*                         COMMON PART                                   *)
(*                                                                       *)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)

(*************************************************************************)
(*                                                                       *)
(*                         clear                                         *)
(*                                                                       *)
(*************************************************************************)

let clear () =
  uploads_clear ();
  uploaders_clear ()

(*************************************************************************)
(*                                                                       *)
(*                         uploads window                                *)
(*                                                                       *)
(*************************************************************************)

open GMain

let uploads_box gui =
  update_all (uploaderstore#all_items ());
  refresh_uploadstats ();

  let vpaned_uploads =
    GPack.paned `VERTICAL ~border_width:6 ()
  in
  ignore (vpaned_uploads#connect#destroy ~callback:
    (fun _ ->
      view_context := None;
      upload_label := None;
      uploader_label := None;
  ));
  let vbox_uploads =
    GPack.vbox ~homogeneous:false
      ~packing:vpaned_uploads#add1 ()
  in
  let vbox_uploaders = 
    GPack.vbox ~homogeneous:false
      ~packing:vpaned_uploads#add2 ()
  in

  let uploads_evbox =
    GBin.event_box ~packing:(vbox_uploads#pack ~expand:false ~fill:true) ()
  in
  uploads_evbox#misc#modify_bg [(`NORMAL, (`NAME "#AFAFF4"))];
  let uploads_label =
    GMisc.label ~xalign:0. ~yalign:0.
      ~xpad:3 ~ypad:3 ~packing:uploads_evbox#add ()
  in
  let uploaders_evbox =
    GBin.event_box ~packing:(vbox_uploaders#pack ~expand:false ~fill:true) ()
  in
  uploaders_evbox#misc#modify_bg [(`NORMAL, (`NAME "#AFAFF4"))];
  let hbox_uploaders =
    GPack.hbox ~homogeneous:false
      ~packing:uploaders_evbox#add ()
  in
  let uploaders_label =
    GMisc.label ~xalign:0. ~yalign:0. ~xpad:3 ~ypad:3 
      ~packing:(hbox_uploaders#pack ~expand:false ~fill:true) ()
  in

  let uploadview = 
    Uploads.treeview ~mode:`MULTIPLE 
      ~packing:(vbox_uploads#pack ~expand:true ~fill:true) ()
  in
  view_context := Some uploadview#view#misc#pango_context;
  uploadview#set_model uploadstore#gmodel;
  uploadview#set_menu upload_menu;
  let uploaderview = 
    Uploaders.treeview ~mode:`MULTIPLE
      ~packing:(vbox_uploaders#pack ~expand:true ~fill:true) ()
  in
  uploaderview#set_model uploaderstore#gmodel;
  uploaderview#set_menu uploader_menu;

  let wtool =
    GuiTools.tool_bar `HORIZONTAL ~layout:`END
      ~packing:(hbox_uploaders#pack ~expand:true ~fill:true) ()
  in
  let markup = create_markup !M.uT_lb_show_pending in
  let f () =
    show_pending := not !show_pending;
    uploaderstore#refresh_filter ()
  in
  let _bShow_pending = wtool#add_button
      ~style:`BOTH_HORIZ
      ~markup
      ~icon:(A.get_icon ~icon:M.icon_stock_pending_slots ~size:A.SMALL ())
      ~f ()
  in

  GuiTools.set_vpaned vpaned_uploads O.uploads_vpane_up;
  GuiTools.get_vpaned vpaned_uploads O.uploads_vpane_up;

  uploads_label#set_use_markup true;
  uploaders_label#set_use_markup true;
  upload_label := Some uploads_label;
  uploader_label := Some uploaders_label;
  update_uploads_label ();
  update_uploaders_label ();
  vpaned_uploads#coerce



let _ =
  ignore (Timeout.add ~ms:6000 ~callback:
    (fun _ ->
       update_uploaders ();
       update_all (uploaderstore#all_items ());
       refresh_uploadstats ();
       true
  ))


