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

open Printf2
open Options
open Md4
open GMain

open Gettext
open Gui_global
open CommonTypes
open GuiProto
open GuiTypes
open Gui_types
open Gui_columns

module M = Gui_messages
module P = Gpattern
module O = Gui_options

let (!!) = Options.(!!)

let color_opt_of_file f = Some !!O.color_files_result

let first_name names =
  match names with
    [] -> gettext M.unknown
  | n :: _ -> n

let shorten_name s = Filename2.shorten !!O.max_result_name_len s

let is_filtered r =
  List.memq r.gresult_network !Gui_global.networks_filtered

let result_mapping names =
  let ext = String.lowercase (Filename2.last_extension (first_name names)) in
  match ext with
      ".exe"
    | ".py"
    | ".bat"
    | ".run"
    | ".com" -> O.gdk_pix M.o_xpm_mimetype_binary
    | ".cda"
    | ".iso"
    | ".cue"
    | ".img"
    | ".nrg"
    | ".bin"
    | ".bwt"
    | ".ccd"
    | ".cif" -> O.gdk_pix M.o_xpm_mimetype_cdimage
    | ".deb" -> O.gdk_pix M.o_xpm_mimetype_debian
    | ".url"
    | ".swf"
    | ".xhtml"
    | ".xml"
    | ".html"
    | ".wml"
    | ".php"
    | ".htm" -> O.gdk_pix M.o_xpm_mimetype_html
    | ".ai"
    | ".ag"
    | ".g3"
    | ".cgm"
    | ".eps"
    | ".epsi"
    | ".epsf"
    | ".gif"
    | ".xcf"
    | ".jng"
    | ".jpeg"
    | ".jpg"
    | ".msod"
    | ".pcx"
    | ".pcd"
    | ".png"
    | ".pbm"
    | ".pgm"
    | ".ppm"
    | ".pnm"
    | ".svg"
    | ".tif"
    | ".tiff"
    | ".tga"
    | ".bmp"
    | ".ico"
    | ".wmf"
    | ".xbm"
    | ".xpm"
    | ".fig"
    | ".dxf"
    | ".dwg" -> O.gdk_pix M.o_xpm_mimetype_images
    | ".jar"
    | ".class"
    | ".js"
    | ".java" -> O.gdk_pix M.o_xpm_mimetype_java
    | ".pdf" -> O.gdk_pix M.o_xpm_mimetype_pdf
    | ".ps" -> O.gdk_pix M.o_xpm_mimetype_postscript
    | ".ra"
    | ".ram"
    | ".rm"
    | ".rmvb"
    | ".rv9"
    | ".rt" -> O.gdk_pix M.o_xpm_mimetype_real
    | ".bak"
    | ".old"
    | ".sik" -> O.gdk_pix M.o_xpm_mimetype_recycled
    | ".rpm" -> O.gdk_pix M.o_xpm_mimetype_rpm
    | ".sh"
    | ".csh" -> O.gdk_pix M.o_xpm_mimetype_shellscript
    | ".xls"
    | ".xlc"
    | ".xll"
    | ".xlm"
    | ".xlw"
    | ".ppz"
    | ".ppt"
    | ".sdc"
    | ".sdd"
    | ".sdw"
    | ".doc"
    | ".rtf" -> O.gdk_pix M.o_xpm_mimetype_soffice
    | ".ape"
    | ".ogg"
    | ".smil"
    | ".smi"
    | ".aiff"
    | ".mod"
    | ".s3m"
    | ".stm"
    | ".ult"
    | ".uni"
    | ".xm"
    | ".m15"
    | ".mtm"
    | ".669"
    | ".it"
    | ".mid"
    | ".pls"
    | ".mp3"
    | ".pcm"
    | ".m3u"
    | ".au"
    | ".snd"
    | ".wav" -> O.gdk_pix M.o_xpm_mimetype_sound
    | ".o"
    | ".c"
    | ".cpp"
    | ".cxx"
    | ".cc"
    | ".h"
    | ".hh"
    | ".pl"
    | ".perl"
    | ".pm"
    | ".ml"
    | ".mli"
    | ".pyc"
    | ".p"
    | ".pas"
    | ".m" -> O.gdk_pix M.o_xpm_mimetype_source
    | ".as"
    | ".gnumeric"
    | ".ksp"
    | ".wb1"
    | ".wb2"
    | ".wb3" -> O.gdk_pix M.o_xpm_mimetype_spreadsheet
    | ".gf"
    | ".tex"
    | ".ltx"
    | ".sty"
    | ".cls"
    | ".bib" -> O.gdk_pix M.o_xpm_mimetype_tex
    | ".txt"
    | ".csv"
    | ".sgml"
    | ".lyx"
    | ".dif"
    | ".diff"
    | ".patch"
    | ".css"
    | ".sst"
    | ".srt"
    | ".mpl"
    | ".scr"
    | ".sub"
    | ".dks"
    | ".jss"
    | ".pjs"
    | ".psb"
    | ".ssa"
    | ".tts"
    | ".vsf"
    | ".nfo"
    | ".zeg" -> O.gdk_pix M.o_xpm_mimetype_text
    | ".a"
    | ".arj"
    | ".bz"
    | ".bz2"
    | ".z"
    | ".tgz"
    | ".gz"
    | ".zip"
    | ".ace"
    | ".lha"
    | ".lzh"
    | ".lzo"
    | ".tzo"
    | ".rar"
    | ".tar"
    | ".zoo" -> O.gdk_pix M.o_xpm_mimetype_tgz
    | ".ogm"
    | ".asf"
    | ".fli"
    | ".flc"
    | ".avi"
    | ".mng"
    | ".mp2"
    | ".mpe"
    | ".mpeg"
    | ".mpg"
    | ".qt"
    | ".movie"
    | ".mov"
    | ".moov"
    | ".qtvr"
    | ".wma"
    | ".wmv" -> O.gdk_pix M.o_xpm_mimetype_video
    | ".abw"
    | ".zabw"
    | ".bzabw"
    | ".aw"
    | ".hwp"
    | ".kwd"
    | ".kwt"
    | ".sam"
    | ".wpd"-> O.gdk_pix M.o_xpm_mimetype_wordprocessing
    | _ -> O.gdk_pix M.o_xpm_mimetype_unknown

class box s_num columns () =
  let titles = List.map Gui_columns.Result.string_of_column !!columns in
  object (self)
    inherit [gui_result_info] Gpattern.filtered_plist `EXTENDED titles true (fun r -> r.gresult_num) as pl
      inherit Gui_results_base.box !!O.toolbars_style () as box 

(*    val mutable wb_extend_search = (None : GButton.button option) *)
    
    method filter = is_filtered
    
    val mutable columns = columns
    val mutable icons_are_used = (!!O.use_icons : bool)

    method set_list_bg bg font =
      let wlist = self#wlist in
      let style = wlist#misc#style#copy in
      style#set_base [ (`NORMAL, bg)];
      style#set_font font;
      wlist#misc#set_style style;
      wlist#set_row_height 18;
      wlist#columns_autosize ()

    method set_columns l =
      columns <- l;
      self#set_titles (List.map Gui_columns.Result.string_of_column !!columns);
      self#update;
      self#set_list_bg (`NAME !!O.color_list_bg)
        (Gdk.Font.load_fontset !!O.font_list)
        
    method column_menu  i = 
      [
        `I (gettext M.mAutosize, fun _ -> self#wlist#columns_autosize ());
        `I (gettext M.mSort, self#resort_column i);
        `I (gettext M.mRemove_column,
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
        `M (gettext M.mAdd_column_after, (
            List.map (fun (c,s) ->
                (`I (s, (fun _ -> 
                        let c1, c2 = List2.cut (i+1) !!columns in
                        columns =:= c1 @ [c] @ c2;
                        self#set_columns columns
                    )))
            ) Gui_columns.Result.column_strings));
        `M (gettext M.mAdd_column_before, (
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
          Gui_com.send (Download_query (r.gresult_names, r.gresult_num, false)))
      self#selection
    
    method force_download () = 
      List.iter
        (fun r -> 
          Gui_com.send (Download_query (r.gresult_names, r.gresult_num, true)))
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
        Col_result_name -> compare (first_name r1.gresult_names) (first_name r2.gresult_names)
      |	Col_result_md4 -> compare (Md4.to_string r1.gresult_md4) (Md4.to_string r2.gresult_md4)
      |	Col_result_size -> compare r1.gresult_size r2.gresult_size
      |	Col_result_format -> compare r1.gresult_format r2.gresult_format
      |	Col_result_props -> compare (CommonGlobals.string_of_tags 
              r1.gresult_tags) (
            CommonGlobals.string_of_tags r2.gresult_tags)
      |	Col_result_comment -> compare r1.gresult_comment r2.gresult_comment
      | Col_result_network -> compare r1.gresult_network r2.gresult_network
          
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
        Col_result_name -> shorten_name (first_name r.gresult_names)
      |	Col_result_md4 -> Md4.to_string r.gresult_md4
      |	Col_result_size -> Gui_misc.size_of_int64 r.gresult_size
      |	Col_result_format -> r.gresult_format
      |	Col_result_props -> CommonGlobals.string_of_tags r.gresult_tags
      | Col_result_network -> Gui_global.network_name r.gresult_network
      |	Col_result_comment -> r.gresult_comment
    
    method content r =
      let strings = List.map 
      (* (fun col -> P.String (self#content_by_col col r)) *)
          (fun col -> match col with
               Col_result_name ->
                 (match r.gresult_pixmap with
                     Some pixmap -> P.Pixtext (self#content_by_col col r, pixmap)
                   | _ -> P.String (self#content_by_col col r))
             | Col_result_network ->
                 (match r.gresult_net_pixmap with
                     Some pixmap -> P.Pixmap (pixmap)
                   | _ -> P.String (self#content_by_col col r))
             | _ -> P.String (self#content_by_col col r))
        !!columns
      in
      let col_opt =
        match color_opt_of_file r with
          None -> Some `BLACK
        | Some c -> Some (`NAME c)
      in
      (strings, col_opt)
    
    method set_tb_style tb = 
      if Options.(!!) Gui_options.mini_toolbars then
        (wtool1#misc#hide (); wtool2#misc#show ()) else
        (wtool2#misc#hide (); wtool1#misc#show ());
      wtool2#set_style tb;
      wtool1#set_style tb
    
    method extend_search () =
      Gui_com.send (GuiProto.ExtendedSearch (s_num, ExtendSearchRemotely))

    method core_to_gui_result r =
      {
       gresult_num = r.result_num;
       gresult_network = r.result_network;
       gresult_names = r.result_names;
       gresult_md4 = r.result_md4;
       gresult_size = r.result_size;
       gresult_format = r.result_format;
       gresult_type = r.result_type;
       gresult_tags = r.result_tags;
       gresult_comment = r.result_comment;
       gresult_done = r.result_done;
       gresult_pixmap =
         if icons_are_used then
           Some (result_mapping r.result_names)
           else None;
       gresult_net_pixmap =
         if icons_are_used then
           Some (Gui_options.network_pix (Gui_global.network_name r.result_network))
           else None;
      }
(*
    method remove_extend_search_button =
      match wb_extend_search with
	None -> ()
      |	Some wb -> 
	  wtool#remove wb#coerce;
	  wb_extend_search <- None
*)
    
    method update_icons b =
      icons_are_used <- b;
      let (f, label, step) =
        if b then
          ((fun r ->
          r.gresult_net_pixmap <-
            Some (Gui_options.network_pix
                    (Gui_global.network_name r.gresult_network));
          r.gresult_pixmap <-
            Some (result_mapping r.gresult_names)
          ), gettext M.results_add_icons, 1)
          else
            ((fun r ->
            r.gresult_net_pixmap <- None;
            r.gresult_pixmap <- None;
            ), gettext M.results_remove_icons, 100)
      in
      Gui_options.generate_with_progress label self#get_all_items f step;
      self#update


    initializer
      box#vbox#pack ~expand: true pl#box ;
      
      let style = evbox#misc#style#copy in
      style#set_bg [ (`NORMAL, (`NAME "#494949"))];
      evbox#misc#set_style style;
(*
      (Gui_misc.insert_buttons wtool1 wtool2 
          ~text: (gettext M.download)
        ~tooltip: (gettext M.download)
        ~icon: (M.o_xpm_download)
        ~callback: self#download
          ()
      );
*)
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
      let r = self#core_to_gui_result res in
      self#add_item r;
      wl_count#set_text  (gettext M.results self#size)
    
    method set_waiting n =
      wl_wait#set_text (M.waiting_for_replies n)
    
    method filter_networks = self#refresh_filter
    
    initializer
      box#vbox#pack ~expand: false hbox_labels#coerce ;

end

let rec color_tree w bg font =
  List.iter (fun b ->
    match b#subtree with
        Some (wt: GTree.tree) ->
          begin
            try
              let style = wt#misc#style#copy in
              style#set_bg [ (`NORMAL, bg)];
              style#set_base [ (`NORMAL, bg)];
              style#set_font font;
              wt#misc#set_style style;
              color_tree wt bg font
            with _ -> ()
          end
       | _ ->
          begin 
            try
              List.iter (fun e ->
                try
                  let style = e#misc#style#copy in
                  style#set_base [ (`NORMAL, bg)];
                  style#set_bg [ (`NORMAL, bg)];
                  style#set_font font;
                  e#misc#set_style style
                with _ -> ()
              ) b#children
            with _ -> ()
          end
    ) w#children

let timerID = ref (Timeout.add ~ms:500
                              ~callback:(fun _ -> true))

class box_dir_files () =
  let results = new box (-1) O.results_columns () in
  object (self)
    inherit Gui_results_base.files ()
    
    val mutable selected_dir = (None : GuiTypes.file_tree option)
    
    method set_tree_bg bg font =
      let style = wtree#misc#style#copy in
      style#set_bg [ (`NORMAL, bg)];
      style#set_font font;
      wtree#misc#set_style style;
      color_tree wtree bg font

    method set_list_bg bg font =
      results#set_list_bg bg font;
      self#set_tree_bg bg font

(* (w : gui_client_info GList.clist) *)

    val mutable keep_this = ([] : file_tree option list)
    val mutable gdk_window = ( None : Gdk.window option)
    val mutable tree_is_built = (true : bool)

    method tree_is_built = tree_is_built

    method set_tree_state state =
      tree_is_built <- state
      (* Printf.printf "Gui_results set_tree_state tree_is_built %b\n" tree_is_built;
      flush stdout*)

    method update_gdk_window w =
      gdk_window <- Some w

    method update_tree file_tree_opt =
    (* here there is a segmentation fault when
    clearing the wtree to build a new one while
    the preceeding one is still building.
    It appears when you browse the friends list very
    fast and if one of the friends has a lot of dirs
    & files : see with soulseek clients for example.
    The fix is only a workaround. I can't find the right signal to catch. *)
    if tree_is_built then
      begin
        self#set_tree_state false;
        let curs = Gdk.Cursor.create `WATCH in
        (match gdk_window with
             Some w -> Gdk.Window.set_cursor w curs
           | None -> ());
        Gdk.Cursor.destroy curs;
      selected_dir <- None;
        keep_this <- [];
        timerID := (Timeout.add ~ms:2000
          ~callback:(fun _ ->
            (self#build_tree file_tree_opt;
             true)))
      end
      else
        begin
          keep_this <- file_tree_opt::keep_this;
          (* Printf.printf "Gui_results fill keep_this %b\n" (not tree_is_built);
          flush stdout *)
        end

    method build_tree file_tree_opt =
        self#clear;
      match file_tree_opt with
            None ->
              (Timeout.remove (!timerID);
              let curs = Gdk.Cursor.create `LEFT_PTR in
              (match gdk_window with
                  Some w -> Gdk.Window.set_cursor w curs
                | None -> ());
              Gdk.Cursor.destroy curs;
              self#set_tree_state true;
              (* Printf.printf "Gui_results file_tree_opt is None ? %b\n" tree_is_built;
              flush stdout; *)
              if keep_this <> [] then
                self#update_tree (List.hd keep_this))
      |	Some ft ->
              self#insert_dir wtree 0 ft

    
    method clear =
      List.iter wtree#remove wtree#children;
      results#clear
    
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
        | (TreeFile r) :: q -> iter acc_dirs (r::acc_files) q
      in
      iter [] [] ft.file_tree_list
    
    method insert_dir wt depth ft =
      let item = GTree.tree_item ~label: ft.file_tree_name () in
      wt#append item;
      let (subdirs, result_list) = self#separate_children ft in
      (try
        let style = wt#misc#style#copy in
        style#set_bg [ (`NORMAL, (`NAME !!O.color_list_bg))];
        style#set_font (Gdk.Font.load_fontset !!O.font_list);
        wt#misc#set_style style
       with _ -> ());
      ignore (item#connect#select
        (fun () ->
           selected_dir <- Some ft;
           let l = ref [] in
           List.iter (fun r ->
             let ri = results#core_to_gui_result r in
             l := ri::!l;
           ) result_list;
        results#reset_data !l));
      ignore (item#connect#deselect
        (fun () ->
           selected_dir <- None;
        results#clear));
      (
        match subdirs with
          [] -> ()
        | l ->
            let wt_sub = GTree.tree () in
            item#set_subtree wt_sub;
            (try
              let style = wt_sub#misc#style#copy in
              style#set_bg [ (`NORMAL, (`NAME !!O.color_list_bg))];
              style#set_font (Gdk.Font.load_fontset !!O.font_list);
              wt_sub#misc#set_style style;
             with _ -> ());
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
    
    method c_update_icons b =
      results#update_icons b


    initializer
      wpane#add2 results#coerce;
      

      ignore (wtree#connect#select_child
        ~callback:(fun _ ->
                   (* Printf.printf "child selected\n";
                   flush stdout; *)
                   Timeout.remove (!timerID);
                   let curs = Gdk.Cursor.create `LEFT_PTR in
                   (match gdk_window with
                       Some w -> Gdk.Window.set_cursor w curs
                     | None -> ());
                   Gdk.Cursor.destroy curs;
                   self#set_tree_state true;
                   if keep_this <> [] then
                     self#update_tree (List.hd keep_this)));


      Gui_misc.insert_buttons wtool1 wtool2 
        ~text: (gettext M.download)
      ~tooltip: (gettext M.download_selected_dir)
       ~icon: (M.o_xpm_download_directory)
      ~callback: self#download_selected_dir
       ();

      let style = evbox#misc#style#copy in
      style#set_bg [ (`NORMAL, (`NAME "#494949"))];
      evbox#misc#set_style style;


end
