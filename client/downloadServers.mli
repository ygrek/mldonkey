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
val walker_timer : BasicSocket.timer -> unit