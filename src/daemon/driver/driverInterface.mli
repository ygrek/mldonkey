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
  val gui_send :
  DonkeyTypes.gui_record -> DonkeyTypes.GuiProto.to_gui -> unit
val send_result :
  DonkeyTypes.gui_record -> int -> DonkeyTypes.result -> unit
val send_download : DonkeyTypes.gui_record -> DonkeyTypes.file -> unit
val send_server : DonkeyTypes.gui_record -> DonkeyTypes.server -> unit
val gui_reader :
  DonkeyTypes.gui_record -> DonkeyTypes.GuiProto.from_gui -> 'a -> unit
val gui_closed : DonkeyTypes.gui_record -> 'a -> 'b -> unit
val gui_server_change_hook :
  DonkeyTypes.server -> DonkeyTypes.gui_record -> unit
val gui_friend_change_hook : 'a -> 'b -> unit
val gui_file_change_hook :
  DonkeyTypes.file -> DonkeyTypes.gui_record -> unit
*)

val gui_handler : 'a -> TcpServerSocket.event -> unit

val gift_handler : 'a -> TcpServerSocket.event -> unit
  
val update_gui_info : unit -> unit

val install_hooks : unit -> unit

val restart_gui_server : (unit -> unit) ref