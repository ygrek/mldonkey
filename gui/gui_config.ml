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

let (!!) = Options.(!!)
let (=:=) = Options.(=:=)

let safe_int_of_string option s =
  try option =:= int_of_string s
  with _ -> ()



let create_gui_params () =
  (** Server options *)

  let gui_port = string 
      ~f: (fun s -> safe_int_of_string GO.port s) 
      "GUI port" (string_of_int !!GO.port)
  in
  let gui_hostname = string 
      ~f: (fun s -> GO.hostname =:= s) 
      "Hostname" !!GO.hostname 
  in
  let gui_password = string
      ~f: (fun s -> GO.password =:= s)
      "Password" !!GO.password
  in
  let server_options = Section
      ("Server",
       [
	 gui_port ; gui_hostname ; gui_password ;
       ] 
      )
  in

  (** Colors *)
  let color_default = color 
      ~f: (fun s -> GO.color_default =:= s)
      "Default" !!GO.color_default 
  in
  let color_downloaded = color
      ~f: (fun s -> GO.color_downloaded =:= s)
      "Downloaded" !!GO.color_downloaded
  in
  let color_downloading = color
      ~f: (fun s -> GO.color_downloading =:= s)
      "Downloading" !!GO.color_downloading
  in
  let color_available = color
      ~f: (fun s -> GO.color_available =:= s)
      "Available" !!GO.color_available
  in
  let color_not_available = color
      ~f: (fun s -> GO.color_not_available =:= s)
      "Not available" !!GO.color_not_available 
  in
  let color_connected = color
      ~f: (fun s -> GO.color_connected =:= s)
      "Connected" !!GO.color_connected
  in
  let color_not_connected = color
      ~f: (fun s -> GO.color_not_connected =:= s)
      "Not connected" !!GO.color_not_connected
  in
  let color_connecting = color
      ~f: (fun s -> GO.color_connecting =:= s)
      "Connecting" !!GO.color_connecting
  in
  let color_files_listed = color
      ~f: (fun s -> GO.color_files_listed =:= s)
      "Files listed" !!GO.color_files_listed
  in
  let colors_options = Section
      ("Colors",
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
  let auto_resize = bool
      ~f: (fun b -> GO.auto_resize =:= b)
      "Auto resize" !!GO.auto_resize
  in
  let layout_options = Section
      ("Layout",
       [
	 auto_resize ;
       ] 
      )
  in

  [ server_options ; colors_options ; layout_options  ]
  
let create_option label ref = string ~f: (fun s -> ref := s) label !ref

let create_client_params () =
  (** Name and authentification *)
  let client_name = create_option "Client name" GO.client_name in
  let client_password = create_option "Password" GO.client_password in

  (** ports *)
  let port_param = create_option "Port" GO.client_port in
  let telnet_port = create_option "Telnet port" GO.telnet_port in
  let gui_port = create_option "Gui port" GO.client_gui_port in
  

  (** delays *)
  let save_op_delay = create_option "Save options delay" GO.save_options_delay in
  let check_cl_delay = create_option "Check client connections delay" GO.check_client_connections_delay in
  let check_delay = create_option "Check connections delay" GO.check_connections_delay in
  let small_retry = create_option "Small retry delay" GO.small_retry_delay in
  let medium_retry = create_option "Medium retry delay" GO.medium_retry_delay in
  let long_retry = create_option "Long retry delay" GO.long_retry_delay in
  let server_timeout = create_option "Server connection timeout" GO.server_connection_timeout in
  let client_timeout = create_option "Client timeout" GO.client_timeout in
  let update_gui_delay = create_option "Update GUI delay" GO.update_gui_delay in
  let max_server_age = create_option "Max server age" GO.max_server_age in

  (** mail *)
  let smtp_server = create_option "SMTP server" GO.smtp_server in
  let smtp_port = create_option "SMTP port" GO.smtp_port in
  let mail = create_option "Mail address" GO.mail in

  (** directories *)
  let temp_dir = create_option "Temp directory" GO.temp_dir in
  let incom_dir = create_option "Incoming directory" GO.incoming_dir in

  (** misc *)
  let max_up_rate = create_option "Max upload rate" GO.max_upload_rate in
  let max_con_servs = create_option "Max connected servers" GO.max_connected_servers in

  [ Section ("Name and authentification",
	     [ client_name;  client_password ]) ;
    Section ("Ports",
	     [ port_param ; telnet_port ; gui_port]) ;
    Section ("Delays",
	     [ save_op_delay ; check_cl_delay ; check_delay ;
	       small_retry ; medium_retry ; long_retry ;
	       server_timeout ; client_timeout ;
	       update_gui_delay ; max_server_age ;]) ;
    Section ("Directories",
	     [ temp_dir ; incom_dir]) ;
    Section ("Mail",
	     [ smtp_server ; smtp_port ; mail ]);
    Section ("Misc",
	     [max_up_rate ; max_con_servs ]) ;
  ] 

let edit_options () =
  let gui_params = create_gui_params () in 
  let client_params = create_client_params () in
  let structure = [
    Section_list ("Gui", gui_params) ;
    Section_list ("Client", client_params) ;
  ] 
  in
  match Configwin.get "Options" structure with
    Return_ok | Return_apply -> Gui_misc.save_options ()
  | Return_cancel -> ()
