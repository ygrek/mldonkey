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

open Options
open SlskOptions
open SlskTypes
open CommonTypes
  
let old_files = 
  define_option soulseek_section ["old_files"]
    "" (list_option (tuple2_option (string_option, int64_option))) []

let servers = 
  define_option soulseek_section ["servers"]
    "" (list_option (tuple2_option (string_option, int_option))) 
  [ ("server.slsknet.org", 2240) ]
  
let save_config () =
  servers =:= List.map (fun s ->
      let addr = s.server_addr in
      let port = s.server_port in
      let name = Ip.string_of_addr addr in
      (name, port)
  ) (Hashtbl2.to_list SlskGlobals.servers_by_addr)
  
  