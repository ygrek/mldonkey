open Options

val allowed_ips : Ip.t list Options.option_record

val add_server : Ip.t -> int -> DownloadTypes.server
val remove_server : Ip.t -> int -> unit
  
val client_md4 : Md4.t Options.option_record
val known_shared_files : DownloadTypes.shared_file_info 
  list Options.option_record

val known_friends : DownloadTypes.client list Options.option_record
  
val old_files : Md4.t list Options.option_record
val known_servers : DownloadTypes.server list Options.option_record

  
val value_to_addr : Options.option_value -> Ip.t * int
val value_to_md4 : Options.option_value -> Md4.t
  
val files : DownloadTypes.file list Options.option_record
val done_files : DownloadTypes.file list Options.option_record
