val value_to_addr : Options.option_value -> Ip.t * int
val value_to_md4 : Options.option_value -> Md4.t
  
val files : DownloadTypes.file list Options.option_record
val done_files : DownloadTypes.file list Options.option_record

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
val old_files : Md4.t list Options.option_record
val known_servers : DownloadTypes.server list Options.option_record
val initial_score : int Options.option_record
val check_connections_delay : float Options.option_record
val max_connected_servers : int Options.option_record
val retry_delay : float Options.option_record
val server_connection_timeout : float Options.option_record
val telnet_port : int Options.option_record
val known_friends : DownloadTypes.client list Options.option_record
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
  
val client_md4 : Md4.t Options.option_record

val use_file_history : bool Options.option_record
val save_file_history : bool Options.option_record
  
val http_port : int Options.option_record
val http_login : string Options.option_record
val http_password : string Options.option_record
  
val add_server : Ip.t -> int -> DownloadTypes.server
val remove_server : Ip.t -> int -> unit

