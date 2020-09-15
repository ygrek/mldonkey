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

(** Configuration panel. *)

open Printf2
open Gettext
open Gui_global
module GO = Gui_options
open Configwin

module M = Gui_messages

let (!!) = Options.(!!)
let (=:=) = Options.(=:=)

let safe_int_of_string option s =
  try option =:= int_of_string s
  with _ -> ()

let use_graphical_availability_ref = ref !!GO.use_graphical_availability
let use_icons_ref = ref !!GO.use_icons

let create_gui_params () =
  (** Server options *)

  let gui_port = string 
      ~help: (M.h_gui_port)
      ~f: (fun s -> safe_int_of_string GO.port s) 
      (M.o_gui_port) (string_of_int !!GO.port)
  in
  let gui_hostname = string 
      ~help: (M.h_hostname)
      ~f: (fun s -> GO.hostname =:= s) 
      (M.o_hostname) !!GO.hostname 
  in
  let gui_login = string
      ~help: (M.h_login)
      ~f: (fun s -> GO.login =:= s)
      (M.o_login) !!GO.login
  in
  let gui_password = password
      ~help: (M.h_gui_password)
      ~f: (fun s -> GO.password =:= s)
      (M.o_password) !!GO.password
  in
  let server_options = Section
      ((M.o_gui_server),
       [
         gui_port ; gui_hostname ; gui_login ; gui_password ;
       ] 
      )
  in

  (** Colors *)
  let color_default = color 
      ~help: (M.h_col_default)
      ~f: (fun s -> GO.color_default =:= s)
      (M.o_col_default) !!GO.color_default 
  in
  let color_downloaded = color
      ~help: (M.h_col_downloaded)
      ~f: (fun s -> GO.color_downloaded =:= s)
      (M.o_col_downloaded) !!GO.color_downloaded
  in
  let color_downloading = color
      ~help: (M.h_col_downloading)
      ~f: (fun s -> GO.color_downloading =:= s)
      (M.o_col_downloading) !!GO.color_downloading
  in
  let color_available = color
      ~help: (M.h_col_avail)
      ~f: (fun s -> GO.color_available =:= s)
      (M.o_col_avail) !!GO.color_available
  in
  let color_not_available = color
      ~help: (M.h_col_not_avail)
      ~f: (fun s -> GO.color_not_available =:= s)
      (M.o_col_not_avail) !!GO.color_not_available 
  in
  let color_connected = color
      ~help: (M.h_col_connected)
      ~f: (fun s -> GO.color_connected =:= s)
      (M.o_col_connected) !!GO.color_connected
  in
  let color_not_connected = color
      ~help: (M.h_col_not_connected)
      ~f: (fun s -> GO.color_not_connected =:= s)
      (M.o_col_not_connected) !!GO.color_not_connected
  in
  let color_connecting = color
      ~help: (M.h_col_connecting)
      ~f: (fun s -> GO.color_connecting =:= s)
      (M.o_col_connecting) !!GO.color_connecting
  in
  let color_files_listed = color
      ~help: (M.h_col_files_listed)
      ~f: (fun s -> GO.color_files_listed =:= s)
      (M.o_col_files_listed) !!GO.color_files_listed
  in
  let color_files_result = color
      ~help: (M.h_col_files_result)
      ~f: (fun s -> GO.color_files_result =:= s)
      (M.o_col_files_result) !!GO.color_files_result
  in
  let color_tab_selected = color
      ~help: (M.h_col_tab_selected)
      ~f: (fun s -> GO.color_tab_selected =:= s)
      (M.o_col_tab_selected) !!GO.color_tab_selected
  in
  let color_tab_not_selected = color
      ~help: (M.h_col_tab_not_selected)
      ~f: (fun s -> GO.color_tab_not_selected =:= s)
      (M.o_col_tab_not_selected) !!GO.color_tab_not_selected
  in
  let color_list_bg = color
      ~help: (M.h_col_list_bg)
      ~f: (fun s -> GO.color_list_bg =:= s)
      (M.o_col_list_bg) !!GO.color_list_bg
  in
  let color_network_enabled = color
      ~help: (M.h_col_network_enabled)
      ~f: (fun s -> GO.color_network_enabled =:= s)
      (M.o_col_network_enabled) !!GO.color_network_enabled
  in
  let color_network_disabled = color
      ~help: (M.h_col_network_disabled)
      ~f: (fun s -> GO.color_network_disabled =:= s)
      (M.o_col_network_disabled) !!GO.color_network_disabled
  in
  let colors_options = Section
      ((M.o_colors),
       [
         color_default ; color_downloaded ;
         color_downloading ; color_available ;
         color_not_available ;
         color_connected ; color_not_connected ;
         color_connecting ; color_files_listed ;
         color_files_result ; color_tab_selected ;
         color_tab_not_selected ; color_list_bg ;
         color_network_enabled ; color_network_disabled ;
       ] 
      )
  in

  (** Layout options *)
  
  let tb_style = combo
      ~expand:false
      ~help: (M.h_toolbars_style) 
      ~f:(fun s -> GO.toolbars_style =:= GO.string_to_tbstyle s)
      ~new_allowed:false ~blank_allowed:false
      (M.o_toolbars_style) 
      (List.map fst GO.tb_styles)
      (GO.tbstyle_to_string !!GO.toolbars_style)
  in
  let tb_icons = bool
      ~help: (M.h_mini_toolbars)
      ~f: (fun b -> GO.mini_toolbars =:= b)
      (M.o_mini_toolbars) !!GO.mini_toolbars
  in

  let tab_pos = combo
      ~expand:false
      ~help: (M.h_tab_position)
    ~f:(fun s -> 
        GO.notebook_tab =:= GO.TabPosition.string_to_pos s
        )
      ~new_allowed:false ~blank_allowed:false
      (M.o_tab_position)
      GO.TabPosition.values
      (GO.TabPosition.pos_to_string !!GO.notebook_tab)
  in

  let layout_options = Section
      ((M.o_layout),
       [
        tb_style ;
        tb_icons;
        tab_pos;
       ] 
      )
  in

  let sel l f_string () =
    let menu = GMenu.menu () in
    let choice = ref None in
    let entries = List.map
        (fun ele -> 
          `I (f_string ele, fun () -> choice := Some ele))
        l
    in
    GToolbox.build_menu menu ~entries;
    ignore (menu#connect#deactivate GMain.Main.quit);
    menu#popup 0 0;
    GMain.Main.main ();
    match !choice with
      None -> []
    | Some c -> [c]
  in

  (** Columns options *)
  let servers_cols = list
      ~help: (M.h_servers_columns)
      ~f: (fun l -> GO.servers_columns =:= l)
      ~add: (sel 
               (List.map (fun (c,_,_) -> c ) Gui_columns.server_column_strings)
               Gui_columns.Server.string_of_column)
      ""
      (fun c -> [Gui_columns.Server.string_of_column c])
      !!GO.servers_columns
  in
  let dls_cols = list
      ~help: (M.h_downloads_columns)
      ~f: (fun l -> GO.downloads_columns =:= l)
      ~add: (sel 
               (List.map (fun (c,_,_) -> c ) Gui_columns.file_column_strings)
               Gui_columns.File.string_of_column)
      ""
      (fun c -> [Gui_columns.File.string_of_column c])
      !!GO.downloads_columns
  in
(*
  let dled_cols = list
      ~help: (M.h_downloaded_columns)
      ~f: (fun l -> GO.downloaded_columns =:= l)
      ~add: (sel 
               (List.map fst Gui_columns.file_column_strings)
               Gui_columns.File.string_of_column)
      ""
      (fun c -> [Gui_columns.File.string_of_column c])
      !!GO.downloaded_columns
  in
*)
  let friends_cols = list
      ~help: (M.h_friends_columns)
      ~f: (fun l -> GO.friends_columns =:= l)
      ~add: (sel 
               (List.map (fun (c,_,_) -> c ) Gui_columns.client_column_strings)
               Gui_columns.Client.string_of_column)
      ""
      (fun c -> [Gui_columns.Client.string_of_column c])
      !!GO.friends_columns
  in
  let file_locs_cols = list
      ~help: (M.h_file_locations_columns)
      ~f: (fun l -> GO.file_locations_columns =:= l)
      ~add: (sel 
               (List.map (fun (c,_,_) -> c ) Gui_columns.client_column_strings)
               Gui_columns.Client.string_of_column)
      ""
      (fun c -> [Gui_columns.Client.string_of_column c])
      !!GO.file_locations_columns
  in
  let results_cols = list
      ~help: (M.h_results_columns)
      ~f: (fun l -> GO.results_columns =:= l)
      ~add: (sel 
               (List.map (fun (c,_,_) -> c ) Gui_columns.result_column_strings)
               Gui_columns.Result.string_of_column)
      ""
      (fun c -> [Gui_columns.Result.string_of_column c])
      !!GO.results_columns
  in
  let shared_cols = list
      ~help: (M.h_shared_files_up_columns)
      ~f: (fun l -> GO.shared_files_up_columns =:= l)
      ~add: (sel 
               (List.map (fun (c,_,_) -> c ) Gui_columns.shared_file_up_column_strings)
               Gui_columns.Shared_files_up.string_of_column)
      ""
      (fun c -> [Gui_columns.Shared_files_up.string_of_column c])
      !!GO.shared_files_up_columns
  in
  let columns_options = Section_list
      ((M.o_columns),
       [
         Section ((M.o_servers_columns) ,[servers_cols]) ; 
         Section ((M.o_downloads_columns) ,[dls_cols]); 
         Section ((M.o_results_columns) ,[results_cols]);
         Section ((M.o_friends_columns) ,[friends_cols]) ;
         Section ((M.o_file_locations_columns) ,[file_locs_cols]) ;
         Section ((M.o_shared_files_up_colums) ,[shared_cols]) ;
       ]
      )
  in
(*
  let columns_options = Section_list
      ((M.o_columns),
       [
         Section ((M.o_servers_columns) ,[servers_cols]) ;
         Section ((M.o_downloads_columns) ,[dls_cols]);
         Section ((M.o_downloaded_columns) ,[dled_cols]); 
         Section ((M.o_results_columns) ,[results_cols]);
         Section ((M.o_friends_columns) ,[friends_cols]) ; 
         Section ((M.o_file_locations_columns) ,[file_locs_cols]) ;
         Section ((M.o_shared_files_up_colums) ,[shared_cols]) ;
       ] 
      )
  in
*)
  let files_auto_expand_depth = string
      ~f: (safe_int_of_string GO.files_auto_expand_depth)
      ~help: (M.h_files_auto_expand_depth)
      (M.o_files_auto_expand_depth)
      (string_of_int !!GO.files_auto_expand_depth)
  in
  let use_size_suffixes = bool
      ~f: (fun b -> GO.use_size_suffixes =:= b)
      ~help: (M.h_use_size_suffixes)
      (M.o_use_size_suffixes)
      !!GO.use_size_suffixes
  in
  let use_availability_height = bool
      ~f: (fun b -> GO.use_availability_height =:= b)
      ~help: (M.h_use_availability_height)
      (M.o_use_availability_height)
      !!GO.use_availability_height
  in
  let use_relative_availability = bool
      ~f: (fun b -> GO.use_relative_availability =:= b)
      ~help: (M.h_use_relative_availability)
      (M.o_use_relative_availability)
      !!GO.use_relative_availability
  in
  let use_icons = bool
      ~f: (fun b -> GO.use_icons =:= b)
      ~help: (M.h_use_icons)
      (M.o_use_icons)
      !!GO.use_icons
  in
  let use_graphical_availability = bool
      ~f: (fun b -> GO.use_graphical_availability =:= b)
      ~help: (M.h_use_graphical_availability)
      (M.o_use_graphical_availability)
      !!GO.use_graphical_availability
  in
  let max_file_name_len = string
      ~f: (safe_int_of_string GO.max_file_name_len)
      ~help: (M.h_max_file_name_len)
      (M.o_max_file_name_len)
      (string_of_int !!GO.max_file_name_len)
  in
  let max_client_name_len = string
      ~f: (safe_int_of_string GO.max_client_name_len)
      ~help: (M.h_max_client_name_len)
      (M.o_max_client_name_len)
      (string_of_int !!GO.max_client_name_len)
  in
  let max_result_name_len = string
      ~f: (safe_int_of_string GO.max_result_name_len)
      ~help: (M.h_max_result_name_len)
      (M.o_max_result_name_len)
      (string_of_int !!GO.max_result_name_len)
  in
  let misc_options = Section
      ((M.o_misc),
       [
         files_auto_expand_depth ;
         use_size_suffixes ;
         use_availability_height ;
         use_relative_availability ;
         use_icons;
         use_graphical_availability;
         max_file_name_len;
         max_client_name_len;
         max_result_name_len;
       ]
      )
  in

        (** Graph options *)
  let max_download_rate = string
      ~help: (M.h_max_download_rate)
      ~f: (safe_int_of_string GO.max_download_rate)
      (M.o_max_download_rate) (string_of_int !!GO.max_download_rate)
  in
  let max_upload_rate = string
      ~help: (M.h_max_upload_rate)
      ~f: (safe_int_of_string GO.max_upload_rate)
      (M.o_max_upload_rate) (string_of_int !!GO.max_upload_rate)
  in
  let download_time_range = string
      ~help: (M.h_download_time_range)
      ~f: (fun s -> GO.download_time_range =:= s)
      (M.o_download_time_range) (!!GO.download_time_range)
  in
  let upload_time_range = string
      ~help: (M.h_upload_time_range)
      ~f: (fun s -> GO.upload_time_range =:= s)
      (M.o_upload_time_range) (!!GO.upload_time_range)
  in
  let color_bg_download = color
      ~help: (M.h_col_bg_download)
      ~f: (fun s -> GO.color_bg_download =:= s)
      (M.o_col_bg_download) !!GO.color_bg_download
  in
  let color_bg_upload = color
      ~help: (M.h_col_bg_upload)
      ~f: (fun s -> GO.color_bg_upload =:= s)
      (M.o_col_bg_upload) !!GO.color_bg_upload
  in
  let color_grid_download = color
      ~help: (M.h_col_grid_download)
      ~f: (fun s -> GO.color_grid_download =:= s)
      (M.o_col_grid_download) !!GO.color_grid_download
  in
  let color_grid_upload = color
      ~help: (M.h_col_grid_upload)
      ~f: (fun s -> GO.color_grid_upload =:= s)
      (M.o_col_grid_upload) !!GO.color_grid_upload
  in
  let color_fg_download = color
      ~help: (M.h_col_fg_download)
      ~f: (fun s -> GO.color_fg_download =:= s)
      (M.o_col_fg_download) !!GO.color_fg_download
  in
  let color_fg_upload = color
      ~help: (M.h_col_fg_upload)
      ~f: (fun s -> GO.color_fg_upload =:= s)
      (M.o_col_fg_upload) !!GO.color_fg_upload
  in
  let color_fg_download_av = color
      ~help: (M.h_col_fg_download_av)
      ~f: (fun s -> GO.color_fg_download_av =:= s)
      (M.o_col_fg_download_av) !!GO.color_fg_download_av
  in
  let color_fg_upload_av = color
      ~help: (M.h_col_fg_upload_av)
      ~f: (fun s -> GO.color_fg_upload_av =:= s)
      (M.o_col_fg_upload_av) !!GO.color_fg_upload_av
  in
  let graph_options = Section
      ((M.o_graph),
       [
         max_download_rate ;
         max_upload_rate ;
         download_time_range ;
         upload_time_range ;
         color_bg_download ;
         color_bg_upload ;
         color_grid_download ;
         color_grid_upload ;
         color_fg_download ;
         color_fg_upload ;
         color_fg_download_av ;
         color_fg_upload_av ;
       ]
      )
  in

  (** Fonts *)
  let font_list = font
      ~help: (M.h_font_list)
      ~f: (fun s -> GO.font_list =:= s)
      (M.o_font_list) !!GO.font_list
  in
  let font_main_tab = font
      ~help: (M.h_font_main_tab)
      ~f: (fun s -> GO.font_main_tab =:= s)
      (M.o_font_main_tab) !!GO.font_main_tab
  in
  let font_networks = font
      ~help: (M.h_font_networks)
      ~f: (fun s -> GO.font_networks =:= s)
      (M.o_font_networks) !!GO.font_networks
  in
  let font_graphic = font
      ~help: (M.h_font_graphic)
      ~f: (fun s -> GO.font_graphic =:= s)
      (M.o_font_graphic) !!GO.font_graphic
  in
  let fonts_options = Section
      ((M.o_fonts),
       [
         font_list ;
         font_main_tab ;
         font_networks ;
         font_graphic ;
       ]
      )
  in
  [ server_options ; colors_options ; layout_options ; columns_options ; misc_options ; graph_options ; fonts_options ]
  
let create_string_option ?help label ref = string ?help ~f: (fun s -> ref := s) label !ref
  
let create_file_option ?help label ref = filename ?help ~f: (fun s -> ref := s) label !ref
  
let create_bool_option ?help label ref = bool ?help ~f: (fun s -> ref := string_of_bool s) label (bool_of_string !ref)
  
let add_option_value option value =
  try
    let o  = Hashtbl.find options_values option in
    o.option_value := !value;
    o.option_old_value <- !value;
    
  with _ ->
      Hashtbl.add options_values option {
        option_value = value;
        option_old_value = !value;
      }
  
let create_sections_params sections =
  List.map (fun (name, options) ->
      Section (name,
        List.fold_left (fun list (message, optype, option) ->
            try
              (match optype with
                | GuiTypes.StringEntry ->
                    create_string_option message
                      (Hashtbl.find options_values option).option_value
                | GuiTypes.BoolEntry ->                  
                    create_bool_option message
                    (Hashtbl.find options_values option).option_value
                | GuiTypes.FileEntry ->                  
                    create_file_option message
                    (Hashtbl.find options_values option).option_value
              ) :: list
            with Not_found ->
                lprintf "No option %s" option; lprint_newline ();
                list
        ) [] !options)
  ) sections
  
let update_toolbars_style gui =
  gui#tab_downloads#set_tb_style !!GO.toolbars_style;
  gui#tab_servers#set_tb_style !!GO.toolbars_style ;
  gui#tab_friends#set_tb_style !!GO.toolbars_style ;
  gui#tab_queries#set_tb_style !!GO.toolbars_style ;
  gui#tab_rooms#set_tb_style !!GO.toolbars_style ;
  gui#tab_uploads#set_tb_style !!GO.toolbars_style

    
let update_list_bg gui =
  gui#tab_servers#set_list_bg (`NAME !!GO.color_list_bg)
    (Gdk.Font.load_fontset !!GO.font_list) ;
  gui#tab_downloads#set_list_bg (`NAME !!GO.color_list_bg)
    (Gdk.Font.load_fontset !!GO.font_list) ;
  gui#tab_friends#set_list_bg (`NAME !!GO.color_list_bg)
    (Gdk.Font.load_fontset !!GO.font_list) ;
  gui#tab_queries#set_list_bg (`NAME !!GO.color_list_bg)
    (Gdk.Font.load_fontset !!GO.font_list) ;
  gui#tab_rooms#set_list_bg (`NAME !!GO.color_list_bg)
    (Gdk.Font.load_fontset !!GO.font_list) ;
  gui#tab_uploads#set_list_bg (`NAME !!GO.color_list_bg)
    (Gdk.Font.load_fontset !!GO.font_list) ;
  gui#tab_console#set_list_bg (`NAME !!GO.color_list_bg)
    (Gdk.Font.load_fontset !!GO.font_list)


let update_icons gui =
  if !!GO.use_icons <> !use_icons_ref then
    begin
      use_icons_ref := !!GO.use_icons;
      gui#tab_servers#c_update_icons !!GO.use_icons;
      gui#tab_downloads#c_update_icons !!GO.use_icons;
      gui#tab_friends#c_update_icons !!GO.use_icons;
      gui#tab_queries#c_update_icons !!GO.use_icons;
      gui#tab_rooms#c_update_icons !!GO.use_icons;
      gui#tab_uploads#c_update_icons !!GO.use_icons
    end

let update_availability_column gui =
  if !!GO.use_graphical_availability <> !use_graphical_availability_ref then
    begin
      use_graphical_availability_ref := !!GO.use_graphical_availability;
      gui#tab_downloads#c_update_availability_column !!GO.use_graphical_availability
    end

let update_graphs gui =
  gui#tab_graph#set_graph_properties
    !!GO.font_graphic
    !!GO.color_bg_download
    !!GO.color_bg_upload
    !!GO.color_grid_download
    !!GO.color_grid_upload
    !!GO.color_fg_download
    !!GO.color_fg_upload
    !!GO.color_fg_download_av
    !!GO.color_fg_upload_av
    !!GO.download_time_range
    !!GO.max_download_rate
    !!GO.upload_time_range
    !!GO.max_upload_rate

let save_options gui =
  let module P = GuiProto in

  try
    let list = ref [] in
    Hashtbl.iter (fun option o ->
        if !(o.option_value) <> o.option_old_value then begin
            o.option_old_value <- !(o.option_value);
            list := (option, o.option_old_value) :: !list;
          end) 
    options_values;   
    Gui_com.send (P.SaveOptions_query !list)
(*
    (List.map
                       (fun (name, r) -> (name, !r))
                       Gui_options.client_options_assocs
                    )
);
  *)
  with _ ->
    lprintf "ERROR SAVING OPTIONS (but port/password/host correctly set for GUI)"; lprint_newline ()

  
let edit_options gui =
  try
    lprintf "edit_options\n"; 
    let gui_params = create_gui_params () in 
    let client_params = create_sections_params !client_sections in
    let plugins_params = create_sections_params 
        (List.sort (fun (n1,_) (n2,_) ->
          compare (String.lowercase n1) (String.lowercase n2)
        ) !plugins_sections) in
    let structure = [
        Section_list ((M.o_gui), gui_params) ;
        Section_list ((M.o_client), client_params) ;
        Section_list ((M.o_plugins), plugins_params) ;
      ] 
    in
    match Configwin.get ~height:600 ~width:400 
        (M.o_options) structure 
    with
      Return_ok | Return_apply -> 
        Gui_misc.save_gui_options gui;      
        save_options gui ;
        (* update_icons is placed here because #update
           of GPattern will be called by #set_columns *)
        update_icons gui;
        gui#tab_servers#box_servers#set_columns
          GO.servers_columns;
        gui#tab_downloads#box_downloads#set_columns
          GO.downloads_columns;
        gui#tab_friends#box_friends#set_columns
          GO.friends_columns;
        gui#tab_friends#box_files#box_results#set_columns
          GO.results_columns;
        gui#tab_uploads#box_upstats#set_columns
          GO.shared_files_up_columns;
        gui#tab_uploads#box_uploaders#set_columns
          GO.file_locations_columns;
        let current = gui#current_page in
        gui#tab_networks#update_style;
        for i = 0 to 8 do
          gui#notebook#goto_page i
        done;
        gui#notebook#goto_page current;
        (* update_list_bg is placed here because the
           column_autosize is necessary after #set_columns *)
        update_list_bg gui;
        update_toolbars_style gui;
        update_graphs gui;
        update_availability_column gui;

        
    
    | Return_cancel -> ()
  with e ->
      lprintf "Exception %s in edit_options" (Printexc2.to_string e);
      lprint_newline ();
