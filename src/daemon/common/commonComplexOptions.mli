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
val load : unit -> unit
val save : unit -> unit
*)

val file_commit : CommonTypes.file -> unit
val file_cancel : CommonTypes.file -> unit
val file_completed : CommonTypes.file -> unit
val file_add : 'a CommonFile.file_impl -> CommonTypes.file_state -> unit
  
val server_add : 'a CommonServer.server_impl -> unit
val server_remove : CommonTypes.server -> unit
  
val friend_add : CommonTypes.client -> unit
val friend_remove : CommonTypes.client -> unit
  
val contact_add : CommonTypes.client -> unit
val contact_remove : CommonTypes.client -> unit

val done_files :  CommonTypes.file list Options.option_record
val files :  CommonTypes.file list Options.option_record

val servers : CommonTypes.server Intmap.t Options.option_record
val friends : CommonTypes.client list Options.option_record
val contacts : CommonTypes.client list ref
  
val customized_queries :     
  (string * CommonTypes.query_entry) list Options.option_record
  
  (*
val addr_to_value : Ip.addr -> Options.option_value
val value_to_addr : Options.option_value -> Ip.addr
  *)