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
val downloads_ini : Options.options_file
val shared_files_ini : Options.options_file
val servers_ini : Options.options_file
val files_ini : Options.options_file
val friends_ini : Options.options_file

val temp_directory : string Options.option_record
val incoming_directory : string Options.option_record
val client_name : string Options.option_record
val small_retry_delay : float Options.option_record
val medium_retry_delay : float Options.option_record
val long_retry_delay : float Options.option_record
val client_timeout : float Options.option_record
val gui_port : int Options.option_record
val update_gui_delay : float Options.option_record
val port : int Options.option_record
val save_options_delay : float Options.option_record
val check_client_connections_delay : float Options.option_record
val initial_score : int Options.option_record
val check_connections_delay : float Options.option_record
val max_connected_servers : int Options.option_record
val retry_delay : float Options.option_record
val server_connection_timeout : float Options.option_record
val telnet_port : int Options.option_record
val max_server_age : int Options.option_record
val interface_buffer : int Options.option_record
val max_upload_rate : int Options.option_record
val max_download_rate : int Options.option_record
val password : string Options.option_record
val features : string Options.option_record
val max_udp_sends : int Options.option_record
val max_xs_packets : int Options.option_record
val previewer : string Options.option_record
val max_dialog_history : int Options.option_record

val smtp_server : string Options.option_record
val smtp_port : int Options.option_record
val mail : string Options.option_record
  
val filters : string Options.option_record
  
val use_file_history : bool Options.option_record
val save_file_history : bool Options.option_record
  
val http_port : int Options.option_record
val http_login : string Options.option_record
val http_password : string Options.option_record
  
val initialized : bool Options.option_record
val max_allowed_connected_servers : unit -> int
  
val max_name_len : int Options.option_record
val shared_directories : string list Options.option_record
val verbose : bool Options.option_record
  
  
val max_opened_connections : int Options.option_record
  
val web_infos : (string * int * string) list Options.option_record