val verbose : bool ref
  
val last_connected_server : unit -> DownloadTypes.server
val all_servers : unit -> DownloadTypes.server list
val connect_one_server : unit -> unit
val force_check_server_connections : bool -> unit
val check_server_connections : BasicSocket.timer -> unit
val connect_server : DownloadTypes.server -> unit
val make_tagged : DownloadTypes.file list -> Mftp.tagged_file list
val remove_old_servers : unit -> unit
val all_shared : unit -> DownloadTypes.file list
  
val remove_old_servers_timer : unit -> unit
  
val update_master_servers : BasicSocket.timer -> unit  
val update_options : unit -> unit
