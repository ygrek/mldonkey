(*
  val gui_send :
  DownloadTypes.gui_record -> DownloadTypes.Gui_proto.to_gui -> unit
val send_result :
  DownloadTypes.gui_record -> int -> DownloadTypes.result -> unit
val send_download : DownloadTypes.gui_record -> DownloadTypes.file -> unit
val send_server : DownloadTypes.gui_record -> DownloadTypes.server -> unit
val gui_reader :
  DownloadTypes.gui_record -> DownloadTypes.Gui_proto.from_gui -> 'a -> unit
val gui_closed : DownloadTypes.gui_record -> 'a -> 'b -> unit
val gui_server_change_hook :
  DownloadTypes.server -> DownloadTypes.gui_record -> unit
val gui_friend_change_hook : 'a -> 'b -> unit
val gui_file_change_hook :
  DownloadTypes.file -> DownloadTypes.gui_record -> unit
*)

  val gui_handler : 'a -> TcpServerSocket.event -> unit
  
val update_gui_info : BasicSocket.timer -> unit

val install_hooks : unit -> unit

val restart_gui_server : (unit -> unit) ref