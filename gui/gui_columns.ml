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

(** Types to define columns. *)

module M = Gui_messages


(** The different columns which can be displayed for a file. *)
type file_column = 
  Col_file_name
| Col_file_md4
| Col_file_size
| Col_file_downloaded
| Col_file_percent
| Col_file_state
| Col_file_availability
| Col_file_rate
| Col_file_format
| Col_file_network
  
let file_column_strings = [
    Col_file_name, M.name ;
    Col_file_md4, M.md4 ;
    Col_file_size, M.size ;
    Col_file_downloaded, M.downloaded ;
    Col_file_percent, "%" ;
    Col_file_state, M.state ;
    Col_file_availability, M.availability ;
    Col_file_rate, M.rate ;
    Col_file_format, M.format ;
    Col_file_network, M.network;
] 

let strings_file_column = 
  List.map (fun (c,s) -> (s,c)) file_column_strings

let string_of_file_column c = 
  List.assoc c file_column_strings

let file_column_of_string s = 
  try List.assoc s strings_file_column
  with Not_found ->
    prerr_endline 
      (Printf.sprintf 
	 "incorrect file column : %s, using Col_file_name instead" s);
    Col_file_name


(** The different columns which can be displayed for a client/location/friend. *)
type client_column = 
  Col_client_name
| Col_client_state
| Col_client_kind
| Col_client_network
| Col_client_type

let client_column_strings = [
    Col_client_name, M.name ;
    Col_client_state, M.state ;
    Col_client_kind, M.kind ;
    Col_client_network, M.network;
    Col_client_type, M.client_type;
] 

let strings_client_column = 
  List.map (fun (c,s) -> (s,c)) client_column_strings

let string_of_client_column c = 
  List.assoc c client_column_strings

let client_column_of_string s = 
  try List.assoc s strings_client_column
  with Not_found ->
    prerr_endline 
      (Printf.sprintf 
	 "incorrect client column : %s, using Col_client_name instead" s);
    Col_client_name

(** The different columns which can be displayed for a server. *)
type server_column = 
  Col_server_address
| Col_server_state
| Col_server_users
| Col_server_files
| Col_server_desc
| Col_server_network
| Col_server_name
  
let server_column_strings = [
    Col_server_address, M.address ;
    Col_server_state, M.state ;
    Col_server_users, M.server_nusers ;
    Col_server_files, M.server_nfiles ;
    Col_server_desc, M.server_desc ;
    Col_server_network, M.network;
    Col_server_name, M.name;
  ] 
  
let strings_server_column = 
  List.map (fun (c,s) -> (s,c)) server_column_strings

let string_of_server_column c = 
  List.assoc c server_column_strings

let server_column_of_string s = 
  try List.assoc s strings_server_column
  with Not_found ->
    prerr_endline 
      (Printf.sprintf 
	 "incorrect server column : %s, using Col_server_address instead" s);
    Col_server_address

(** The different columns which can be displayed for a search result. *)
type result_column = 
  Col_result_name
| Col_result_md4
| Col_result_size
| Col_result_format
| Col_result_props
| Col_result_comment
| Col_result_network
  
let result_column_strings = [
    Col_result_name, M.name ;
    Col_result_md4, M.md4 ;
    Col_result_size, M.size ;
    Col_result_format, M.format ;
    Col_result_props, M.properties ;
    Col_result_comment, M.comment ;
    Col_result_network, M.network;
  ] 
  
let strings_result_column = 
  List.map (fun (c,s) -> (s,c)) result_column_strings

let string_of_result_column c = 
  List.assoc c result_column_strings

let result_column_of_string s = 
  try List.assoc s strings_result_column
  with Not_found ->
    prerr_endline 
      (Printf.sprintf 
	 "incorrect result column : %s, using Col_result_name instead" s);
    Col_result_name

      
(** The different columns which can be displayed for a user/location/friend. *)
type user_column = 
  Col_user_name
| Col_user_kind
| Col_user_tags

let user_column_strings = [
    Col_user_name, M.name ;
    Col_user_kind, M.kind ;
    Col_user_tags, M.comment;
] 

let strings_user_column = 
  List.map (fun (c,s) -> (s,c)) user_column_strings

let string_of_user_column c = 
  List.assoc c user_column_strings

let user_column_of_string s = 
  try List.assoc s strings_user_column
  with Not_found ->
    prerr_endline 
        (Printf.sprintf 
          "incorrect user column : %s, using Col_user_name instead" s);
      Col_user_name

      