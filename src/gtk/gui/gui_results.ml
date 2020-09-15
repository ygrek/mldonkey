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

open Options
open Md4

open Gettext
open Gui_global
open CommonTypes
open GuiProto
open GuiTypes
open Gui_columns

module M = Gui_messages
module P = Gpattern
module O = Gui_options

let (!!) = Options.(!!)

let first_name r = 
  match r.result_names with
    [] -> gettext M.unknown
  | n :: _ -> n

let shorten_name s = CommonGlobals.shorten s !!O.max_result_name_len

let is_filtered r = false
(* TODO RESULT
  List.memq r.result_network !Gui_global.networks_filtered
*)

class box s_num columns () =
  let titles = List.map Gui_columns.Result.string_of_column !!columns in
  object (self)
    inherit [CommonTypes.result_info] Gpattern.filtered_plist `EXTENDED titles true (fun r -> r.result_num) as pl
      inherit Gui_results_base.box !!O.toolbars_style () as box 

(*    val mutable wb_extend_search = (None : GButton.button option) *)
    
    method filter = is_filtered
    
    val mutable columns = columns
    method set_columns l =
      columns <- l;
      self#set_titles (List.map Gui_columns.Result.string_of_column !!columns);
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
            ) Gui_columns.Result.column_strings));
        `M ("Add Column Before", (
            List.map (fun (c,s) ->
                (`I (s, (fun _ -> 
                        let c1, c2 = List2.cut i !!columns in
                        columns =:= c1 @ [c] @ c2;
                        self#set_columns columns
                    )))
            ) Gui_columns.Result.column_strings));
      ]
    
    method download () = 
      List.iter
        (fun r -> 
          Gui_com.send (Download_query (r.result_names, r.result_num, false)))
      self#selection
    
    method force_download () = 
      List.iter
        (fun r -> 
          Gui_com.send (Download_query (r.result_names, r.result_num, true)))
      self#selection
    
    method menu = 
      match self#selection with
        [] -> []
      |	_ -> [
            `I (gettext M.download, self#download);
            `I (gettext M.force_download, self#force_download);
          ]
    
    
    method compare_by_col col r1 r2 =
      match col with
        Col_result_name -> compare (first_name r1) (first_name r2)
      |	Col_result_uids -> compare r1.result_uids r2.result_uids
      |	Col_result_size -> compare r1.result_size r2.result_size
      |	Col_result_format -> compare r1.result_format r2.result_format
      |	Col_result_props -> compare (CommonGlobals.string_of_tags 
              r1.result_tags) (
            CommonGlobals.string_of_tags r2.result_tags)
      |	Col_result_comment -> compare r1.result_comment r2.result_comment
      | Col_result_network ->
            (* TODO RESULT compare r1.gresult_network r2.gresult_network *)
            0

    method compare r1 r2 =
      let abs = if current_sort >= 0 then current_sort else - current_sort in
      let col = 
        try List.nth !!columns (abs - 1) 
        with _ -> Col_result_name
      in
      let res = self#compare_by_col col r1 r2 in
      current_sort * res
    
    method content_by_col col r =
      match col with
        Col_result_name -> shorten_name (first_name r)
      |	Col_result_uids -> (Uid.to_string (List.hd r.result_uids))
      |	Col_result_size -> Gui_misc.size_of_int64 r.result_size
      |	Col_result_format -> r.result_format
      |	Col_result_props -> CommonGlobals.string_of_tags r.result_tags
      | Col_result_network ->
                (* TODO RESULT compare r1.gresult_network r2.gresult_network *)
                "--"
      |	Col_result_comment -> r.result_comment 
    
    method content r =
      let strings = List.map 
          (fun col -> P.String (self#content_by_col col r))
        !!columns
      in
      (strings, None)
    
    method set_tb_style tb = 
      if Options.(!!) Gui_options.mini_toolbars then
        (wtool1#misc#hide (); wtool2#misc#show ()) else
        (wtool2#misc#hide (); wtool1#misc#show ());
      wtool2#set_style tb;
      wtool1#set_style tb
    
    method extend_search () =
      Gui_com.send (GuiProto.ExtendedSearch (s_num, ExtendSearchRemotely))

(*
    method remove_extend_search_button =
      match wb_extend_search with
        None -> ()
      |	Some wb -> 
          wtool#remove wb#coerce;
          wb_extend_search <- None
*)
    
    initializer
      box#vbox#pack ~expand: true pl#box ;
      
      (Gui_misc.insert_buttons wtool1 wtool2 
          ~text: (gettext M.download)
        ~tooltip: (gettext M.download)
        ~icon: (M.o_xpm_download)
        ~callback: self#download
          ()
      );
      
      if s_num >= 0 then
(*	wb_extend_search <- Some *)
        Gui_misc.insert_buttons  wtool1 wtool2
          ~text: (gettext M.extended_search)
        ~tooltip: (gettext M.extended_search)
        ~icon: (M.o_xpm_extend_search)
        ~callback: self#extend_search
          ()


end

class search_result_box s_num () =
  let hbox_labels = GPack.hbox () in
  let wl_count = GMisc.label ~text: "" 
      ~packing: (hbox_labels#pack ~expand: false ~padding:3) 
    () 
  in
  let wl_wait = GMisc.label ~text: "" 
      ~packing: (hbox_labels#pack ~expand: false ~padding:3) 
    () 
  in
  object (self)
    inherit box s_num Gui_options.results_columns  () as box
    
    method filter res = is_filtered res
    
    method add_result (res : CommonTypes.result_info) =
      self#add_item res;
      wl_count#set_text  (gettext M.results self#size)
    
    method set_waiting n =
      wl_wait#set_text (M.waiting_for_replies n)
    
    method filter_networks = self#refresh_filter
    
    initializer
      box#vbox#pack ~expand: false hbox_labels#coerce ;

end


class box_dir_files () =
  let results = new box (-1) O.results_columns () in
  object (self)
    inherit Gui_results_base.files ()
    
    val mutable selected_dir = (None : GuiTypes.file_tree option)
    
    method update_tree file_tree_opt =
      self#clear ;
      selected_dir <- None;
      results#clear ;
      match file_tree_opt with
        None -> ()
      |	Some ft ->
          self#insert_dir wtree 0 ft;
    
    method clear =
      List.iter wtree#remove wtree#children;
    
    method box_results = results
    
    method set_tb_style st =
      if Options.(!!) Gui_options.mini_toolbars then
        (wtool1#misc#hide (); wtool2#misc#show ()) else
        (wtool2#misc#hide (); wtool1#misc#show ());
      results#set_tb_style st;
      wtool1#set_style st;
      wtool2#set_style st
    
    method separate_children ft =
      let rec iter acc_dirs acc_files = function
          [] -> (List.rev acc_dirs, List.rev acc_files)
        | (TreeDirectory child) :: q -> iter (child::acc_dirs) acc_files q
        | (TreeFile r) :: q -> 
            iter acc_dirs (r::acc_files) q
      in
      iter [] [] ft.file_tree_list
    
    method insert_dir wt depth ft =
      let item = GTree.tree_item ~label: ft.file_tree_name () in
      wt#append item;
      let (subdirs, result_list) = self#separate_children ft in
      
      ignore (item#connect#select
          (fun () -> selected_dir <- Some ft; results#reset_data result_list));
      ignore (item#connect#deselect
          (fun () -> selected_dir <- None; results#clear));
      (
        match subdirs with
          [] -> ()
        | l ->
            let wt_sub = GTree.tree () in
            item#set_subtree wt_sub;
            let expanded = ref false in
            ignore (item#connect#expand
                (fun _ ->
                  if !expanded then
                    ()
                  else
                    (
                      expanded := true;
                      List.iter (self#insert_dir wt_sub (depth+1)) subdirs
                    )
              )
            );
            if depth <= !!O.files_auto_expand_depth then
              item#expand ()
      );
      if depth = 0 then
        wt#select_item ~pos: 0
    
    method download_selected_dir () =
      match selected_dir with
        None -> ()
      |	Some ft ->
          let files = GuiTypes.list_files ft in
          let len = List.length files in
          match GToolbox.question_box
              (gettext M.download_selected_dir)
            [gettext M.ok ; gettext M.cancel]
              (M.confirm_download_dir ft.GuiTypes.file_tree_name len) 
          with
            1 ->
              List.iter
                (fun r -> 
                  Gui_com.send (
                    Download_query (r.result_names, r.result_num, false)))
              files
          | _ ->
              ()
    
    initializer
      wpane#add2 results#coerce;
      
      Gui_misc.insert_buttons wtool1 wtool2 
        ~text: (gettext M.download)
      ~tooltip: (gettext M.download_selected_dir)
      ~icon: (M.o_xpm_download)
      ~callback: self#download_selected_dir
        ()


end
