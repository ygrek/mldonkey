(* Copyright 2004 b8_bavard INRIA *)
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


val disconnect : GuiTypes2.gui -> BasicSocket.close_reason -> unit
val send : GuiProto.from_gui -> unit
val reconnect : GuiTypes2.gui ->
    ('a -> GuiProto.to_gui -> unit) -> 'a -> BasicSocket.close_reason -> unit
val connected : unit -> bool
  
val to_gui_protocol_used : int array
val from_gui_protocol_used : int array
  
val scan_ports : unit -> unit
  
  
