(*
val search_found :
  DownloadTypes.search -> 'a -> Md4.t -> Mftp.tag list -> unit
*)
val force_check_locations : unit -> unit
val save_options : BasicSocket.timer -> unit
val check_locations : BasicSocket.timer -> unit
val search_handler :
  DownloadTypes.search ->  Mftp_server.QueryReply.t -> unit
val make_query : DownloadTypes.search -> Mftp_server.Query.t
val force_save_options : unit -> unit
val install_hooks : unit -> unit
val reset_upload_timer : BasicSocket.timer -> unit
val upload_timer : BasicSocket.timer -> unit
val upload_credit_timer : BasicSocket.timer -> unit
val udp_client_handler: Mftp_server.t -> UdpSocket.udp_packet -> unit 
val find_search : int -> DownloadTypes.search
val make_xs : DownloadTypes.search -> unit