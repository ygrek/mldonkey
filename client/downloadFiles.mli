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
(*
val search_found :
  DownloadTypes.search -> 'a -> Md4.t -> Mftp.tag list -> unit
*)
val force_check_locations : unit -> unit
val save_options : BasicSocket.timer -> unit
val check_locations : BasicSocket.timer -> unit
val search_handler :
  DownloadTypes.search ->  Mftp_server.QueryReply.t -> unit
val force_save_options : unit -> unit
val install_hooks : unit -> unit
val reset_upload_timer : unit -> unit
val upload_timer : BasicSocket.timer -> unit
val upload_credit_timer : unit -> unit
val udp_client_handler: Mftp_server.t -> UdpSocket.udp_packet -> unit 
val find_search : int -> DownloadTypes.search
val make_xs : DownloadTypes.search -> unit
  
val fill_clients_list : unit -> unit
val check_clients : unit -> unit
val remove_old_clients : unit -> unit
  