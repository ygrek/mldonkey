(* Copyright 2002 b8_bavard, b8_fee_carabine, INRIA *)
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


open Options

val allowed_ips : Ip.t list Options.option_record

val add_server : Ip.t -> int -> DownloadTypes.server
val remove_server : Ip.t -> int -> unit
  
val client_md4 : Md4.t Options.option_record
val known_shared_files : DownloadTypes.shared_file_info 
  list Options.option_record

val known_friends : DownloadTypes.client list Options.option_record
  
val old_files : Md4.t list Options.option_record
val known_servers : DownloadTypes.server list Options.option_record

  
val value_to_addr : Options.option_value -> Ip.t * int
val value_to_md4 : Options.option_value -> Md4.t
  
val files : DownloadTypes.file list Options.option_record
val done_files : DownloadTypes.file list Options.option_record

val customized_queries :
  (string * DownloadTypes.query_entry) list Options.option_record
  
