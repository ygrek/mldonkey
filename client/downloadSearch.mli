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

val search_of_args : string list -> Mftp.query
  
val custom_query : Buffer.t -> string -> unit
val send_custom_query : Buffer.t -> string -> (string * string) list -> unit
  
val search_string : Mftp.query -> string
val new_search : Mftp.query -> DownloadTypes.search
val start_search : Mftp.query -> Buffer.t -> unit
  
val complex_search : Buffer.t -> unit
val send_search : DownloadTypes.search -> Mftp.query -> unit