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
val new_server_hook : (DownloadTypes.server -> unit) ref
val server_change_hook : (DownloadTypes.server -> unit) ref
val client_change_hook : (DownloadTypes.client -> unit) ref
val say_hook : (DownloadTypes.client option -> string -> unit) ref
val server_is_connected_hook :
  (DownloadTypes.server -> TcpClientSocket.t -> unit) ref
val received_from_server_hook :
  (DownloadTypes.server -> TcpClientSocket.t -> Mftp_server.t -> unit) ref
val server_is_disconnected_hook : (DownloadTypes.server -> unit) ref
val friend_change_hook : (DownloadTypes.client -> unit) ref
val file_change_hook : (DownloadTypes.file -> unit) ref
  
val page_size : int32

val client_ip : Ip.t ref
val client_port : int ref
val client_tags  : Mftp.tag list ref

val shared_files_info : (string, DownloadTypes.shared_file_info) Hashtbl.t

val servers_ini_changed : bool ref
val sleeping : bool ref
val dialog_history : (string * string) list ref  
val upload_credit : int ref
val has_upload : int ref
val interesting_clients : DownloadTypes.client list ref
val servers_list : DownloadTypes.server list ref  
val udp_servers_list : DownloadTypes.server list ref
val searches : DownloadTypes.search list ref  
val file_counter : int ref
val search_counter : int ref
val guis : DownloadTypes.gui_record list ref
val nservers : int ref
val servers_by_key : (Ip.t * int, DownloadTypes.server) Hashtbl2.t
val servers_by_num : (int, DownloadTypes.server) Hashtbl.t
val server_counter : int ref
val zone_size : int32
val block_size : int32
val queue_timeout : float ref
val nclients : int ref
val files_by_anon_client :
  (Ip.t * int * Ip.t, DownloadTypes.file * bool ref) Hashtbl.t
val connected_server_list : DownloadTypes.server list ref
val dbserver_sock : TcpClientSocket.t option ref
val user_socks : TcpClientSocket.t list ref
val client_counter : int ref
val gui_server_sock : TcpServerSocket.t option ref
val indirect_friends : (string * Md4.t, unit) Hashtbl.t
val upload_clients : DownloadTypes.client Fifo.t
val new_shared : DownloadTypes.file list ref
val shared_files : DownloadTypes.shared_file list ref
val files_by_md4 : (Md4.t, DownloadTypes.file) Hashtbl.t
val download_counter : int ref
val upload_counter : int ref
val download_credit : int ref
val nshared_files :  int ref
val udp_sock : UdpSocket.t option ref
val last_xs : int ref
val indirect_clients_by_md4 : (Md4.t, unit) Hashtbl.t
val clients_by_kind : (Gui_types.location_kind, DownloadTypes.client) Hashtbl.t
val clients_by_num : (int, DownloadTypes.client) Hashtbl.t
val find_file : Md4.t -> DownloadTypes.file  
val new_file : string -> Md4.t -> int32 -> DownloadTypes.file
val new_server : Ip.t -> int -> int -> DownloadTypes.server
val find_server : Ip.t -> int -> DownloadTypes.server
val new_client : Gui_types.location_kind -> DownloadTypes.client
  
val remove_server :  Ip.t -> int -> unit
val remove_client : DownloadTypes.client -> unit
val find_client : int -> DownloadTypes.client

val small_change_file : DownloadTypes.file -> unit
  
val remove_client_chunks : 
  DownloadTypes.file -> DownloadTypes.availability -> unit
val add_client_chunks :   
  DownloadTypes.file -> DownloadTypes.availability -> unit
  
val file_fd : DownloadTypes.file -> Unix.file_descr
val first_name : DownloadTypes.file -> string
  
val exit_properly : unit -> unit
val do_at_exit : (unit -> unit) -> unit
  
  
      
val new_connection_control : float -> DownloadTypes.connection_control
val connection_ok : DownloadTypes.connection_control -> unit
val connection_failed : DownloadTypes.connection_control -> unit
val connection_can_try : DownloadTypes.connection_control -> bool
val connection_must_try : DownloadTypes.connection_control -> unit
val connection_set_last_conn : DownloadTypes.connection_control -> float -> unit
val connection_last_conn :  DownloadTypes.connection_control -> float
val connection_try : DownloadTypes.connection_control -> unit