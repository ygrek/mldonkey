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

module GO = Gui_options
open Configwin

module M = Gui_messages

let (!!) = Options.(!!)
let (=:=) = Options.(=:=)

let safe_int_of_string option s =
  try option =:= int_of_string s
  with _ -> ()



let create_gui_params () =
  (** Server options *)

  let gui_port = string 
      ~help: M.h_gui_port
      ~f: (fun s -> safe_int_of_string GO.port s) 
      M.o_gui_port (string_of_int !!GO.port)
  in
  let gui_hostname = string 
      ~help: M.h_hostname
      ~f: (fun s -> GO.hostname =:= s) 
      M.o_hostname !!GO.hostname 
  in
  let gui_password = string
      ~help: M.h_gui_password
      ~f: (fun s -> GO.password =:= s)
      M.o_password !!GO.password
  in
  let server_options = Section
      (M.o_gui_server,
       [
	 gui_port ; gui_hostname ; gui_password ;
       ] 
      )
  in

  (** Colors *)
  let color_default = color 
      ~help: M.h_col_default
      ~f: (fun s -> GO.color_default =:= s)
      M.o_col_default !!GO.color_default 
  in
  let color_downloaded = color
      ~help: M.h_col_downloaded
      ~f: (fun s -> GO.color_downloaded =:= s)
      M.o_col_downloaded !!GO.color_downloaded
  in
  let color_downloading = color
      ~help: M.h_col_downloading
      ~f: (fun s -> GO.color_downloading =:= s)
      M.o_col_downloading !!GO.color_downloading
  in
  let color_available = color
      ~help: M.h_col_avail
      ~f: (fun s -> GO.color_available =:= s)
      M.o_col_avail !!GO.color_available
  in
  let color_not_available = color
      ~help: M.h_col_not_avail
      ~f: (fun s -> GO.color_not_available =:= s)
      M.o_col_not_avail !!GO.color_not_available 
  in
  let color_connected = color
      ~help: M.h_col_connected
      ~f: (fun s -> GO.color_connected =:= s)
      M.o_col_connected !!GO.color_connected
  in
  let color_not_connected = color
      ~help: M.h_col_not_connected
      ~f: (fun s -> GO.color_not_connected =:= s)
      M.o_col_not_connected !!GO.color_not_connected
  in
  let color_connecting = color
      ~help: M.h_col_connecting
      ~f: (fun s -> GO.color_connecting =:= s)
      M.o_col_connecting !!GO.color_connecting
  in
  let color_files_listed = color
      ~help: M.h_col_files_listed
      ~f: (fun s -> GO.color_files_listed =:= s)
      M.o_col_files_listed !!GO.color_files_listed
  in
  let colors_options = Section
      (M.o_colors,
       [
	 color_default ; color_downloaded ;
	 color_downloading ; color_available ;
	 color_not_available ;
	 color_connected ; color_not_connected ;
	 color_connecting ; color_files_listed ;
       ] 
      )
  in

  (** Layout options *)
  
  let tb_style = combo
      ~expand:false
      ~help: M.h_toolbars_style 
      ~f:(fun s -> GO.toolbars_style =:= GO.string_to_tbstyle s)
      ~new_allowed:false ~blank_allowed:false
      M.o_toolbars_style 
      (List.map fst GO.tb_styles)
      (GO.tbstyle_to_string !!GO.toolbars_style)
  in
(*
  let auto_resize = bool
      ~help: M.h_auto_resize
      ~f: (fun b -> GO.auto_resize =:= b)
      M.o_auto_resize !!GO.auto_resize
  in
*)
  let layout_options = Section
      (M.o_layout,
       [
	 tb_style ;
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
      ~help: M.h_servers_columns
      ~f: (fun l -> GO.servers_columns =:= l)
      ~add: (sel 
	       (List.map fst Gui_columns.server_column_strings)
	       Gui_columns.string_of_server_column)
      M.o_servers_columns
      (fun c -> [Gui_columns.string_of_server_column c])
      !!GO.servers_columns
  in
  let dls_cols = list
      ~help: M.h_downloads_columns
      ~f: (fun l -> GO.downloads_columns =:= l)
      ~add: (sel 
	       (List.map fst Gui_columns.file_column_strings)
	       Gui_columns.string_of_file_column)
      M.o_downloads_columns
      (fun c -> [Gui_columns.string_of_file_column c])
      !!GO.downloads_columns
  in
  let dled_cols = list
      ~help: M.h_downloaded_columns
      ~f: (fun l -> GO.downloaded_columns =:= l)
      ~add: (sel 
	       (List.map fst Gui_columns.file_column_strings)
	       Gui_columns.string_of_file_column)
      M.o_downloaded_columns
      (fun c -> [Gui_columns.string_of_file_column c])
      !!GO.downloaded_columns
  in
  let friends_cols = list
      ~help: M.h_friends_columns
      ~f: (fun l -> GO.friends_columns =:= l)
      ~add: (sel 
	       (List.map fst Gui_columns.client_column_strings)
	       Gui_columns.string_of_client_column)
      M.o_friends_columns
      (fun c -> [Gui_columns.string_of_client_column c])
      !!GO.friends_columns
  in
  let file_locs_cols = list
      ~help: M.h_file_locations_columns
      ~f: (fun l -> GO.file_locations_columns =:= l)
      ~add: (sel 
	       (List.map fst Gui_columns.client_column_strings)
	       Gui_columns.string_of_client_column)
      M.o_file_locations_columns
      (fun c -> [Gui_columns.string_of_client_column c])
      !!GO.file_locations_columns
  in
  let results_cols = list
      ~help: M.h_results_columns
      ~f: (fun l -> GO.results_columns =:= l)
      ~add: (sel 
	       (List.map fst Gui_columns.result_column_strings)
	       Gui_columns.string_of_result_column)
      M.o_results_columns
      (fun c -> [Gui_columns.string_of_result_column c])
      !!GO.results_columns
  in
  let columns_options = Section
      (M.o_columns,
       [
	 servers_cols ; 
	 dls_cols ; dled_cols ; 
	 results_cols ;
	 friends_cols ; file_locs_cols ;
       ] 
      )
  in

  [ server_options ; colors_options ; layout_options ; columns_options  ]
  
let create_option ?help label ref = string ?help ~f: (fun s -> ref := s) label !ref

let create_client_params () =
  (** Name and authentification *)
  let client_name = create_option M.o_client_name GO.client_name in
  let client_password = create_option M.o_password GO.client_password in

  (** ports *)
  let port_param = create_option M.o_http_port GO.client_port in
  let telnet_port = create_option M.o_telnet_port GO.telnet_port in
  let gui_port = create_option M.o_gui_server_port GO.client_gui_port in

  (** chat *)
  let chat_app_port = create_option M.o_chat_app_port  GO.chat_app_port in
  let chat_app_host = create_option M.o_chat_app_host  GO.chat_app_host in
  let chat_port = create_option M.o_chat_port GO.chat_port in
  let chat_console_id = create_option M.o_chat_console_id GO.chat_console_id in
  let chat_warning_for_downloaded = create_option
      M.o_chat_warning_for_downloaded GO.chat_warning_for_downloaded
  in
  (** delays *)
  let save_op_delay = create_option M.o_save_options_delay GO.save_options_delay in
  let check_cl_delay = create_option M.o_check_client_cons_delay GO.check_client_connections_delay in
  let check_delay = create_option M.o_check_cons_delay GO.check_connections_delay in
  let min_retry = create_option M.o_min_delay GO.min_retry_delay in
(*  let medium_retry = create_option
      ~help: M.h_medium_delay
      M.o_medium_delay GO.medium_retry_delay 
  in
  let long_retry = create_option
      ~help: M.h_long_delay
      M.o_long_delay GO.long_retry_delay 
in
  *)
  let server_timeout = create_option M.o_server_connection_timeout GO.server_connection_timeout in
  let client_timeout = create_option M.o_client_timeout GO.client_timeout in
  let update_gui_delay = create_option M.o_update_gui_delay GO.update_gui_delay in
  let max_server_age = create_option M.o_max_server_age GO.max_server_age in

  (** mail *)
  let smtp_server = create_option M.o_smtp_server GO.smtp_server in
  let smtp_port = create_option M.o_smtp_port GO.smtp_port in
  let mail = create_option M.o_mail_address GO.mail in

  (** directories *)
  let temp_dir = create_option M.o_temp_dir GO.temp_dir in
  let incom_dir = create_option M.o_incom_dir GO.incoming_dir in

  (** misc *)
  let max_up_rate = create_option M.o_max_upload_rate GO.max_upload_rate in
  let max_dl_rate = create_option M.o_max_dl_rate GO.max_download_rate in
  let max_con_servs = create_option M.o_max_connected_servers GO.max_connected_servers in

  [ Section (M.o_name_auth,
	     [ client_name;  client_password ]) ;
    Section (M.o_ports, 
	     [ port_param ; telnet_port ; gui_port]) ;
    Section (M.o_chat,
	     [ chat_app_port ; chat_app_host ; 
	       chat_port ; chat_console_id ; chat_warning_for_downloaded] ) ;
    Section (M.o_delays,
	     [ save_op_delay ; check_cl_delay ; check_delay ;
                min_retry; server_timeout ; client_timeout ;
	       update_gui_delay ; max_server_age ;]) ;
    Section (M.o_directories,
	     [ temp_dir ; incom_dir]) ;
    Section (M.o_mail,
	     [ smtp_server ; smtp_port ; mail ]);
    Section (M.o_misc,
	     [max_up_rate ; max_con_servs ]) ;
  ] 

let update_toolbars_style gui =
  gui#tab_downloads#set_tb_style !!GO.toolbars_style;
  gui#tab_servers#set_tb_style !!GO.toolbars_style ;
  gui#tab_friends#set_tb_style !!GO.toolbars_style ;
  gui#tab_queries#set_tb_style !!GO.toolbars_style 

let edit_options gui =
  let gui_params = create_gui_params () in 
  let client_params = create_client_params () in
  let structure = [
    Section_list (M.o_gui, gui_params) ;
    Section_list (M.o_client, client_params) ;
  ] 
  in
  match Configwin.get ~height: 700 ~width: 500
      M.o_options structure 
  with
    Return_ok | Return_apply -> 
      Gui_misc.save_options gui ;
      gui#tab_servers#box_servers#set_columns
	!!GO.servers_columns;
      gui#tab_downloads#box_downloads#set_columns
	!!GO.downloads_columns;
      gui#tab_downloads#box_downloaded#set_columns
	!!GO.downloaded_columns;
      gui#tab_friends#box_friends#set_columns
	!!GO.friends_columns;
      gui#tab_downloads#box_locations#set_columns
	!!GO.file_locations_columns;
      gui#tab_friends#box_results#set_columns
	!!GO.results_columns;

      update_toolbars_style gui
      
  | Return_cancel -> ()
