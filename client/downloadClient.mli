val verbose : bool ref
  
val query_id_reply : Ip.t -> Mftp_server.QueryIDReply.t -> unit
  
val query_id :
  DownloadTypes.server -> TcpClientSocket.t -> Ip.t -> unit
  
val query_locations_reply : DownloadTypes.server ->
  Mftp_server.QueryLocationReply.t -> unit
  
val query_locations :
  DownloadTypes.file -> DownloadTypes.server -> TcpClientSocket.t -> unit

val connect_client : 
  Ip.t -> DownloadTypes.file list -> DownloadTypes.client -> unit
val client_connection_handler : 'a -> TcpServerSocket.event -> unit

val query_files : DownloadTypes.client ->
  TcpClientSocket.t -> DownloadTypes.file list -> unit
  
val udp_server_send : DownloadTypes.server -> Mftp_server.t -> unit
  
val client_wants_file : DownloadTypes.client -> Md4.t -> unit