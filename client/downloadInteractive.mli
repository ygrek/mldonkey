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
val query_download : string list -> int32 -> Md4.t -> int option
  -> string option -> (int32 * int32) list option -> unit
val save_file: Md4.t -> string -> unit
val forget_search : int -> unit
val check_shared_files : unit -> unit
val load_server_met : string -> unit

exception CommandCloseSocket

val eval : bool ref -> Buffer.t -> string -> DownloadTypes.connection_options -> unit

  
val telnet_handler : TcpServerSocket.t -> TcpServerSocket.event -> unit
val create_http_handler : unit -> TcpServerSocket.t
  
val load_url : string -> string -> unit
  
val reconnect_all : DownloadTypes.file -> unit