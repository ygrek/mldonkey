val find_client_zone : DownloadTypes.client -> unit
val disconnected_from_client : DownloadTypes.client -> string -> unit
val update_zone : DownloadTypes.file -> 
  int32 -> int32 -> DownloadTypes.zone -> unit
val client_state : Gui_types.connection_state -> string
val set_client_state :
  DownloadTypes.client -> Gui_types.connection_state -> unit
val client_file : DownloadTypes.client -> DownloadTypes.file
val find_client_block : DownloadTypes.client -> unit
val verify_chunks : DownloadTypes.file -> unit 
val set_file_size : DownloadTypes.file -> int32 -> unit
val start_download : DownloadTypes.client -> unit
val check_files_md4s : BasicSocket.timer -> unit
val next_file : DownloadTypes.client -> unit
  
val remove_file : Md4.t -> unit

val update_options : DownloadTypes.file -> unit
val add_shared_files : string -> unit
val download_engine : unit -> unit