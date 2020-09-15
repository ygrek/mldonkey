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
open Printf2
open Options
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
      (List.map C.Shared_files_up.string_of_column !!columns)
    true  (fun si -> si.shared_num) as pl
      inherit Gui_uploads_base.box () 
    
    val mutable clipboard = ""
    val mutable columns = columns
    method set_columns l =
      columns <- l;
      self#set_titles 
        (List.map C.Shared_files_up.string_of_column !!columns);
      self#update
    
    method column_menu  i = 
      [
        `I ("Autosize", fun _ -> self#wlist#columns_autosize ());
        `I ("Sort", self#resort_column i);
        `I ("Remove Column",
          (fun _ -> 
              match !!columns with
                _ :: _ :: _ ->
                  
                  (let l = !!columns in
                    match List2.cut i l with
                      l1, _ :: l2 ->
                        columns =:= l1 @ l2;
                        self#set_columns columns
                    | _ -> ())
              | _ -> ()
          )
        );
        `M ("Add Column After", (
            List.map (fun (c,s) ->
                (`I (s, (fun _ -> 
                        let c1, c2 = List2.cut (i+1) !!columns in
                        columns =:= c1 @ [c] @ c2;
                        self#set_columns columns
                    )))
            ) Gui_columns.Shared_files_up.column_strings));
        `M ("Add Column Before", (
            List.map (fun (c,s) ->
                (`I (s, (fun _ -> 
                        let c1, c2 = List2.cut i !!columns in
                        columns =:= c1 @ [c] @ c2;
                        self#set_columns columns
                    )))
            ) Gui_columns.Shared_files_up.column_strings));
      ]
    
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
        try List.nth !!columns (abs - 1) 
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
        !!columns 
      in
      (strings, None)
    
    method menu =

(* fuck the object oriented style: how do I copy something to the
console ???? *)
      
      let copy_ed2k_links list _ = 
        let buf = Buffer.create 100 in
        List.iter (fun s ->
    match s.shared_uids with
        uid :: _ -> (
          match (Uid.to_uid uid) with
              Ed2k md4 ->
                begin
                  let link = Printf.sprintf "ed2k://|file|%s|%Ld|%s|"
                             (Url.encode (Filename.basename s.shared_filename))
                             s.shared_size
                             (Md4.to_string md4)
                  in
                  Printf.bprintf buf "%s\n" link;
                end
            | _ -> ())

      | _ -> ()
        ) list;
        let link = Buffer.contents buf in
        !Gui_global.console_message link;
        clipboard <- link;

(*
        ignore (self#misc#grab_selection `PRIMARY);
        self#misc#add_selection_target ~target:"string" `PRIMARY;
        ignore (self#misc#connect#selection_get (fun sel ~info ~time ->
              lprintf "request selection"; lprint_newline ();
              sel#return clipboard
          )); 
        ignore (self#event#connect#selection_clear (fun sel ~info ~time ->
              lprintf "request selection"; lprint_newline ();
              sel#return clipboard
          ));
*)      
      in
      match self#selection with
        [] -> []
      |	list ->
          [ `I (("Copy ed2k link to console/clipboard"), copy_ed2k_links list) 
          ] 
    
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
          lprintf "Shared file %d not found" num; lprint_newline ();
    
    initializer
      wf_upstats#add pl#box;

end

class upstats_box () =
  let wl_status = GMisc.label ~text: "" ~show: true () in
  let upstats = new box O.shared_files_up_columns () in
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
    
    method set_tb_style tb = 
      if Options.(!!) Gui_options.mini_toolbars then
        (wtool1#misc#hide (); wtool2#misc#show ()) else
        (wtool2#misc#hide (); wtool1#misc#show ());
      wtool1#set_style tb;
      wtool2#set_style tb
    
    initializer
      vbox#pack ~expand: true ~padding: 2 upstats#box;
      
      Gui_misc.insert_buttons wtool1 wtool2 
        ~text: (gettext M.refresh)
      ~tooltip: (gettext M.refresh)
      ~icon: M.o_xpm_refresh
        ~callback: self#refresh 
        ();

      (*
      Gui_misc.insert_buttons wtool1 wtool2 
        ~text: "Edit Shared Directories"
        ~tooltip: "Edit Shared Directories"
        ~icon: M.o_xpm_verify_chunks
        ~callback: (fun _ -> 
          let module C = Configwin in
          let params = [
              C.filenames ~f: (fun _ -> ()) "Shared Directories:" []] in
          match C.simple_edit "Add New Directory" params with
            C.Return_apply -> ()
          | C.Return_ok -> ()
          | C.Return_cancel -> ()
      )
      ()
*)

      Gui_misc.insert_buttons wtool1 wtool2 
        ~text: "Add Shared Directory"
        ~tooltip: "Add Shared Directory"
        ~icon: M.o_xpm_verify_chunks
        ~callback: (fun _ -> 
          let module C = Configwin in
          let prio = ref 0 in
          let dir = ref "" in
          let params = [
              C.string ~f: (fun p -> prio := int_of_string(p)) "Prio:" "0";
              C.filename ~f: (fun d -> dir := d) "Directory:" ""] in
          match C.simple_edit "Add New Directory" ~with_apply: false
            params with
            C.Return_apply -> 
              if !dir <> "" && !dir <> "/" then
                Gui_com.send (Command (Printf.sprintf "share %d '%s'" !prio !dir))
          | C.Return_ok -> ()
          | C.Return_cancel -> ()
      )
      ()
      
  end
