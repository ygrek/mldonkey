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

open Printf2
open Gettext
open Options
module M = Gui_messages


module Make(M: sig
      
      type column

      val kind : string
      val column_strings : (column * string * int) list
    
    end) = struct

    (*
    let strings_column () = 
      List.map (fun (c,s) -> (s,c)) (M.column_strings ())
*)
    let column_strings = M.column_strings
    
    let string_of_column c = 
      let cs = List.map (fun (c,s,_) -> (c,s)) M.column_strings in
      List.assoc c cs
    
    let column_of_string s = 
      try
        let sc = List.map (fun (c,s,_) -> (s,c)) M.column_strings in
        List.assoc s sc 
      with Not_found ->
          match M.column_strings with
            [] -> assert false
          | (c,name,_)  :: _ ->
              lprintf 
                "incorrect column : %s, using %s instead\n" s name;
              c

    let int_of_column c =
      let ci = List.map (fun (c,_,i) -> (c,i)) M.column_strings in
      List.assoc c ci

    let column_of_int i =
      try
        let ic = List.map (fun (c,_,i) -> (i,c)) M.column_strings in
        List.assoc i ic 
      with Not_found ->
          match M.column_strings with
            [] -> assert false
          | (c,name,_)  :: _ ->
              lprintf 
                "incorrect column : %d, using %s instead\n" i name;
              c

    open Options
              
    let value_to_column v =
      match v with
        IntValue i -> column_of_int (Int64.to_int i)
      | _ -> raise Not_found
    
    let column_to_value k =
      IntValue (Int64.of_int (int_of_column k))
    
    let class_column  = define_option_class 
        (Printf.sprintf "%s_column" M.kind)
      value_to_column column_to_value
      
  end
  

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
| Col_file_status
| Col_file_eta
| Col_file_priority
  
let file_column_strings = [
    Col_file_name,  M.c_name, 0 ;
    Col_file_md4,   M.c_md4, 1 ;
    Col_file_size,   M.c_size, 2 ;
    Col_file_downloaded,   M.c_downloaded, 3 ;
    Col_file_percent,  M.c_percent, 4 ;
    Col_file_state,   M.c_state, 5 ;
    Col_file_availability,   M.c_avail, 6 ;
    Col_file_rate,  M.c_rate, 7 ;
    Col_file_format,  M.c_format, 8 ;
    Col_file_network,  M.c_network, 9 ;
    Col_file_status,  M.c_status, 10 ;
    Col_file_eta,  M.c_eta, 11 ;
    Col_file_priority,  M.c_priority, 12;
] 


module File = Make(struct 
      type column = file_column 
      let kind = "File"
      let column_strings = file_column_strings
    end)


(** The different columns which can be displayed for a client/location/friend. *)
type client_column = 
  Col_client_name
| Col_client_state
| Col_client_kind
| Col_client_network
| Col_client_type
| Col_client_rating
| Col_client_connect_time
| Col_client_software
| Col_client_emulemod
| Col_client_downloaded
| Col_client_uploaded
| Col_client_upload
| Col_client_sock_addr

let client_column_strings = [
    Col_client_name,  M.c_name, 0 ;
    Col_client_state,  M.c_state, 1 ;
    Col_client_kind,  M.c_kind, 2 ;
    Col_client_network,  M.c_network, 3 ;
    Col_client_type,  M.c_client_type, 4 ;
    Col_client_rating,  M.c_client_rating, 5 ;
    Col_client_connect_time,  M.c_client_connect_time, 6 ;
    Col_client_software,  M.c_client_software, 7 ;
    Col_client_emulemod,  M.c_client_emulemod, 8 ;
    Col_client_downloaded,  M.c_client_downloaded, 9 ;
    Col_client_uploaded,  M.c_client_uploaded, 10 ;
    Col_client_upload,  M.c_client_upload, 11 ;
    Col_client_sock_addr,  M.c_client_sock_addr, 12 ;
] 

module Client = Make(struct 
      type column = client_column 
      let kind = "Client"
      let column_strings = client_column_strings
    end)

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
    Col_server_address,  M.c_address, 0 ;
    Col_server_state,  M.c_state, 1 ;
    Col_server_users,  M.c_server_nusers, 2 ;
    Col_server_files,  M.c_server_nfiles, 3 ;
    Col_server_desc,  M.c_server_desc, 4 ;
    Col_server_network,  M.c_network, 5 ;
    Col_server_name,  M.c_name, 6 ;
  ] 

module Server = Make(struct 
      type column = server_column 
      let kind = "Server"
      let column_strings = server_column_strings
    end)
  
(** The different columns which can be displayed for a search result. *)
type result_column = 
  Col_result_name
| Col_result_md4
| Col_result_size
| Col_result_format
| Col_result_duration
| Col_result_codec
| Col_result_bitrate
| Col_result_availability
| Col_result_comment
| Col_result_network
  
let result_column_strings = [
    Col_result_name,  M.c_name, 0 ;
    Col_result_md4,  M.c_md4, 1 ;
    Col_result_size,  M.c_size, 2 ;
    Col_result_format,  M.c_format, 3 ;
    Col_result_duration,  M.c_duration, 4 ;
    Col_result_codec,  M.c_codec, 5 ;
    Col_result_bitrate,  M.c_bitrate, 6 ;
    Col_result_availability,  M.c_avail, 7 ;
    Col_result_comment,  M.c_comment, 8 ;
    Col_result_network,  M.c_network, 9 ;
  ] 

module Result = Make(struct 
      type column = result_column 
      let kind = "Result"
      let column_strings = result_column_strings
    end)

      
(** The different columns which can be displayed for a user/location/friend. *)
type user_column = 
  Col_user_name
| Col_user_kind
| Col_user_tags

let user_column_strings = [
    Col_user_name,  M.c_name, 0 ;
    Col_user_kind,  M.c_kind, 1 ;
    Col_user_tags,  M.c_comment, 2 ;
] 

module User = Make(struct 
      type column = user_column 
      let kind = "User"
      let column_strings = user_column_strings
    end)

      
(** The different columns which can be displayed for a room. *)
type room_column = 
  Col_room_name
| Col_room_network
| Col_room_nusers
  
let room_column_strings = [
    Col_room_name,  M.c_name, 0 ;
    Col_room_network,  M.c_network, 1 ;
    Col_room_nusers,  M.c_nusers, 2 ;
] 

module Room = Make(struct 
      type column = room_column 
      let kind = "Room"
      let column_strings = room_column_strings
    end)

(** The columns for the shared file upload info.*)
type shared_file_up_column =
  Col_shared_file
| Col_shared_upsize
| Col_shared_requests
| Col_shared_size

let shared_file_up_column_strings = [
    Col_shared_file,  M.c_filename, 0 ;
    Col_shared_upsize,  M.c_uploaded, 1 ;
    Col_shared_requests,  M.c_requests, 2 ;
    Col_shared_size,  M.c_size, 3 ;
  ] 
  
module Shared_files_up = Make
    (
     struct
       type column = shared_file_up_column
       let kind = "shared_file_upload"
       let column_strings = shared_file_up_column_strings
     end
    )
