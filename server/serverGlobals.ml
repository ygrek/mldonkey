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

open Unix
open TcpBufferedSocket
open Mftp
open Options
open Mftp_comm
open ServerTypes
  
let files_by_md4 = Hashtbl.create 1023
let clients_by_id = Hashtbl.create 101
  
let client_counter = ref 0  
  
let other_servers = ref []
  
let get_client id = Hashtbl.find clients_by_id id

let nconnected_clients = ref 0
  
  (*
let rec tag_find v tags =
  match tags with
    [] -> raise Not_found
  | { tag_name = tag_name; tag_value = tag_value } :: _ 
    when tag_name = v -> tag_value
  | _ :: tags -> tag_find v tags


let rec tags_find_size tags =
  match tags with
    [] -> raise Not_found
  | { tag_name = "length"; tag_value = Uint32 s } :: _ -> s
  | _ :: tags -> tags_find_size tags

let remove_file file =
  Hashtbl.remove files_by_md4 file.file_md4
      
let remove_client client sock msg =
(* remove the client from all tables *)
  Hashtbl.remove clients_by_id client.client_id;
  
(* remove the client from files *)
  List.iter (fun file -> 
      file.file_clients <- List2.removeq client file.file_clients;
      if file.file_clients = [] then
        remove_file file
  ) client.client_files;
  ()

*)