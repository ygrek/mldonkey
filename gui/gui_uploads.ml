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

(** The box with uploads info *)

open Md4

open Gettext
open CommonTypes
open GuiTypes
open GuiProto


module M = Gui_messages
module P = Gpattern
module O = Gui_options
module C = Gui_columns

let (!!) = Options.(!!)



class box columns () =
  object (self)
    inherit [GuiTypes.shared_info] Gpattern.plist `SINGLE
      (List.map C.Shared_files_up.string_of_column columns)
    true  (fun si -> si.shared_num) as pl
      inherit Gui_uploads_base.box () as box
    
    val mutable columns = columns
    method set_columns l =
      columns <- l;
      self#set_titles 
        (List.map C.Shared_files_up.string_of_column columns);
      self#update
    
    method box = wf_upstats#coerce
    
    method compare_by_col col si1 si2 = 
      match col with
        C.Col_shared_file -> 
          compare si1.shared_filename si2.shared_filename
      |	C.Col_shared_requests ->
          compare si1.shared_requests si2.shared_requests
      |	C.Col_shared_upsize ->
          compare si1.shared_uploaded si2.shared_uploaded
      |	C.Col_shared_size ->
          compare si1.shared_size si2.shared_size
    
    method compare si1 si2 =
      let abs = if current_sort >= 0 then current_sort else - current_sort in
      let col = 
        try List.nth columns (abs - 1) 
        with _ -> C.Col_shared_file
      in
      let res = self#compare_by_col col si1 si2 in
      current_sort * res
    
    method content_by_col si col =
      match col with
        C.Col_shared_file -> si.shared_filename
      |	C.Col_shared_requests -> string_of_int si.shared_requests
      |	C.Col_shared_upsize -> 
          Gui_misc.size_of_int64 si.shared_uploaded
      |	C.Col_shared_size -> 
          Gui_misc.size_of_int64 si.shared_size
    
    method content si =
      let strings = List.map 
          (fun col -> P.String (self#content_by_col si col))
        columns 
      in
      (strings, None)

    method menu =
      
(* fuck the object oriented style: how do I copy something to the
console ???? *)
      
      let copy_ed2k_links list _ = 
        List.iter (fun si ->
            let link = Printf.sprintf "ed2k://|file|%s|%Ld|%s|" 
(* Why are some files prefixed by their path ?? *)
                (Filename.basename si.shared_filename)
              si.shared_size
              (Md4.to_string si.shared_id)
            in
            !Gui_global.console_message link
        ) list
      in
      
      match self#selection with
        [] -> []
      |	list ->
          [ `I (("Copy ed2k link to console"), copy_ed2k_links list)  ] 
    
    method find_file num = self#find num
    
    method h_shared_file_info si =
      try
        let _,s_old = self#find_file si.shared_num in
        s_old.shared_filename <- si.shared_filename
      with Not_found ->
        self#add_item si
        
    method h_shared_file_upload num upsize requests =
      try
	let (row, si) = self#find_file num in
	si.shared_uploaded <- upsize;
	si.shared_requests <- requests ;
	self#update_row si row
      with
        Not_found ->
	  Printf.printf "Shared file %d not found" num; print_newline ();

    initializer
      wf_upstats#add pl#box;

  end

class upstats_box () =
  let wl_status = GMisc.label ~text: "" ~show: true () in
  let upstats = new box !!O.shared_files_up_columns () in
  object (self)
    inherit Gui_uploads_base.upstats_box () as upsb

    method wl_status = wl_status
    method box = upsb#vbox
    method upstats_box= upstats

    method clear = 
      wl_status#set_text "";
      upstats#clear

    method refresh () =
      Gui_com.send GuiProto.RefreshUploadStats

    method h_shared_file_info =
      upstats#h_shared_file_info 

    method h_shared_file_upload =
      upstats#h_shared_file_upload 

    initializer
      vbox#pack ~expand: true ~padding: 2 upstats#box;

      ignore
	(wtool#insert_button 
	   ~text: (gettext M.refresh)
	   ~tooltip: (gettext M.refresh)
	   ~icon: (Gui_options.pixmap M.o_xpm_refresh)#coerce
	   ~callback: self#refresh
	   ()
	);
  end
