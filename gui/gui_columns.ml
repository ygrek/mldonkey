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


module Make(M: sig
      
      type column

      val kind : string
      val column_strings : (column * string) list
    
    end) = struct
    
    let strings_column = 
      List.map (fun (c,s) -> (s,c)) M.column_strings
    
    let string_of_column c = 
      List.assoc c M.column_strings
    
    let column_of_string s = 
      try List.assoc s strings_column
      with Not_found ->
          match M.column_strings with
            [] -> assert false
          | (c,name)  :: _ ->
              prerr_endline 
                (Printf.sprintf 
                  "incorrect column : %s, using %s instead" s name);
              c
    
    open Options
              
    let value_to_column v =
      match v with
        StringValue s -> column_of_string s
      | _ -> raise Not_found
    
    let column_to_value k =
      StringValue (string_of_column k)
    
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

let client_column_strings = [
    Col_client_name, M.name ;
    Col_client_state, M.state ;
    Col_client_kind, M.kind ;
    Col_client_network, M.network;
    Col_client_type, M.client_type;
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
    Col_server_address, M.address ;
    Col_server_state, M.state ;
    Col_server_users, M.server_nusers ;
    Col_server_files, M.server_nfiles ;
    Col_server_desc, M.server_desc ;
    Col_server_network, M.network;
    Col_server_name, M.name;
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
    Col_user_name, M.name ;
    Col_user_kind, M.kind ;
    Col_user_tags, M.comment;
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
    Col_room_name, M.name ;
    Col_room_network, M.network ;
    Col_room_nusers, M.nusers;
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
  Col_shared_file, M.filename ;
  Col_shared_upsize, M.uploaded ;
    Col_shared_requests, M.requests ;
    Col_shared_size, M.size;
] 

module Shared_files_up = Make
    (
     struct
       type column = shared_file_up_column
       let kind = "shared_file_upload"
       let column_strings = shared_file_up_column_strings
     end
    )
