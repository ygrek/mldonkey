val query_download : string list -> int32 -> Md4.t -> int option
  -> string option -> (int32 * int32) list option -> unit
val save_file: Md4.t -> string -> unit
val forget_search : int -> unit
val check_shared_files : unit -> unit
val load_server_met : string -> unit

exception CommandCloseSocket
val eval : bool ref -> Buffer.t -> string -> DownloadTypes.output_type -> unit
  
val telnet_handler : TcpServerSocket.t -> TcpServerSocket.event -> unit
val create_http_handler : unit -> TcpServerSocket.t