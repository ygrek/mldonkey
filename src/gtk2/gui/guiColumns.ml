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

(* Types to define columns. *)

open Printf2

open Gettext
open Options
module M = GuiMessages

module Column(C: sig
      
      type column

      val kind : string
      val column_strings : (column * string ref * int) list
    
    end) = struct

    type column = C.column
    let kind = C.kind
    let column_strings = C.column_strings
    
    let string_of_column c = 
      let cs = List.map (fun (c,s,_) -> (c,!s)) C.column_strings in
      List.assoc c cs
    
    let column_of_string s = 
      try
        let sc = List.map (fun (c,s,_) -> (!s,c)) C.column_strings in
        List.assoc s sc 
      with Not_found ->
          match C.column_strings with
            [] -> assert false
          | (c,name,_)  :: _ ->
              lprintf "incorrect column : %s, using %s instead\n" s !name;
              c

    let int_of_column c =
      let ci = List.map (fun (c,_,i) -> (c,i)) C.column_strings in
      List.assoc c ci

    let column_of_int i =
      try
        let ic = List.map (fun (c,_,i) -> (i,c)) C.column_strings in
        List.assoc i ic 
      with Not_found ->
          match C.column_strings with
            [] -> assert false
          | (c,name,_)  :: _ ->
              lprintf "incorrect column : %d, using %s instead\n" i !name;
              c

    open Options
              
    let value_to_column v =
      match v with
        IntValue i -> column_of_int (Int64.to_int i)
      | _ -> raise Not_found
    
    let column_to_value k =
      IntValue (Int64.of_int (int_of_column k))
    
    let column_option  = define_option_class 
        (Printf.sprintf "%s_column" C.kind)
      value_to_column column_to_value
      
  end
  

(* The different columns which can be displayed for a file. *)
type file_column = 
  Col_file_name
| Col_file_uid
| Col_file_size
| Col_file_downloaded
| Col_file_percent
| Col_file_state
| Col_file_availability
| Col_file_rate
| Col_file_format
| Col_file_network
| Col_file_age
| Col_file_last_seen
| Col_file_eta
| Col_file_priority
| Col_file_comment
| Col_file_sources

let file_column_strings = [
    Col_file_name         , M.c_name       , 0  ;
    Col_file_uid          , M.c_uid        , 1  ;
    Col_file_size         , M.c_size       , 2  ;
    Col_file_downloaded   , M.c_downloaded , 3  ;
    Col_file_percent      , M.c_percent    , 4  ;
    Col_file_state        , M.c_state      , 5  ;
    Col_file_availability , M.c_avail      , 6  ;
    Col_file_rate         , M.c_rate       , 7  ;
    Col_file_format       , M.c_format     , 8  ;
    Col_file_network      , M.c_network    , 9  ;
    Col_file_age          , M.c_age        , 10 ;
    Col_file_last_seen    , M.c_last_seen  , 11 ;
    Col_file_eta          , M.c_eta        , 12 ;
    Col_file_priority     , M.c_priority   , 13 ;
    Col_file_comment      , M.c_comment    , 14 ;
    Col_file_sources      , M.c_sources    , 15 ;
] 


module File = Column(struct 
      type column = file_column 
      let kind = "File"
      let column_strings = file_column_strings
    end)


(* The different columns which can be displayed for a client/location. *)
type client_column = 
  Col_client_name
| Col_client_state
| Col_client_kind
| Col_client_network
| Col_client_type
| Col_client_rating
| Col_client_connect_time
| Col_client_software
| Col_client_downloaded
| Col_client_uploaded
| Col_client_upload_rate
| Col_client_download_rate
| Col_client_upload

let client_column_strings = [
    Col_client_name          , M.c_name                 , 0  ;
    Col_client_state         , M.c_state                , 1  ;
    Col_client_kind          , M.c_kind                 , 2  ;
    Col_client_network       , M.c_network              , 3  ;
    Col_client_type          , M.c_client_type          , 4  ;
    Col_client_rating        , M.c_client_rating        , 5  ;
    Col_client_connect_time  , M.c_client_connect_time  , 6  ;
    Col_client_software      , M.c_client_software      , 7  ;
    Col_client_downloaded    , M.c_client_downloaded    , 8  ;
    Col_client_uploaded      , M.c_client_uploaded      , 9  ;
    Col_client_upload_rate   , M.c_client_upload_rate   , 10 ;
    Col_client_download_rate , M.c_client_download_rate , 11 ;
    Col_client_upload        , M.c_client_upload        , 12 ;
] 

module Client = Column(struct 
      type column = client_column 
      let kind = "Client"
      let column_strings = client_column_strings
    end)

(* The different columns which can be displayed for a friend. *)
type friend_column = 
  Col_friend_name
| Col_friend_network

let friend_column_strings = [
    Col_friend_name,  M.c_name, 0 ;
    Col_friend_network,  M.c_network, 1 ;
] 

module Friend = Column(struct 
      type column = friend_column 
      let kind = "Friend"
      let column_strings = friend_column_strings
    end)

(* The different columns which can be displayed for a server. *)
type server_column = 
  Col_server_address
| Col_server_state
| Col_server_users
| Col_server_files
| Col_server_desc
| Col_server_network
| Col_server_name
| Col_server_tags
| Col_server_preferred
  
let server_column_strings = [
    Col_server_address  , M.c_address       , 0 ;
    Col_server_state    , M.c_state         , 1 ;
    Col_server_users    , M.c_server_nusers , 2 ;
    Col_server_files    , M.c_server_nfiles , 3 ;
    Col_server_desc     , M.c_server_desc   , 4 ;
    Col_server_network  , M.c_network       , 5 ;
    Col_server_name     , M.c_name          , 6 ;
    Col_server_tags     , M.c_tags          , 7 ;
    Col_server_preferred, M.c_preferred     , 8 ;
  ] 

module Server = Column(struct 
      type column = server_column 
      let kind = "Server"
      let column_strings = server_column_strings
    end)
  
(* The different columns which can be displayed for a search result. *)
type result_column = 
  Col_result_name
| Col_result_uid
| Col_result_size
| Col_result_format
| Col_result_type
| Col_result_duration
| Col_result_codec
| Col_result_bitrate
| Col_result_availability
| Col_result_comment
| Col_result_network
| Col_result_tags
| Col_result_completesources
  
let result_column_strings = [
    Col_result_name           , M.c_name     , 0  ;
    Col_result_uid            , M.c_uid      , 1  ;
    Col_result_size           , M.c_size     , 2  ;
    Col_result_format         , M.c_format   , 3  ;
    Col_result_duration       , M.c_duration , 4  ;
    Col_result_codec          , M.c_codec    , 5  ;
    Col_result_bitrate        , M.c_bitrate  , 6  ;
    Col_result_availability   , M.c_avail    , 7  ;
    Col_result_comment        , M.c_comment  , 8  ;
    Col_result_network        , M.c_network  , 9  ;
    Col_result_tags           , M.c_tags     , 10 ;
    Col_result_type           , M.c_type     , 11 ;
    Col_result_completesources, M.c_complete , 12 ;
  ] 

module Result = Column(struct 
      type column = result_column 
      let kind = "Result"
      let column_strings = result_column_strings
    end)

      
(* The different columns which can be displayed for a user/location/friend. *)
type user_column = 
  Col_user_name
| Col_user_addr
| Col_user_tags
| Col_user_md4

let user_column_strings = [
    Col_user_name , M.c_name    , 0 ;
    Col_user_addr , M.c_address , 1 ;
    Col_user_tags , M.c_tags    , 2 ;
    Col_user_md4  , M.c_md4     , 3 ;
] 

module User = Column(struct 
      type column = user_column 
      let kind = "User"
      let column_strings = user_column_strings
    end)

      
(* The different columns which can be displayed for a room. *)
type room_column = 
  Col_room_name
| Col_room_network
| Col_room_nusers
| Col_room_state

let room_column_strings = [
    Col_room_name    , M.c_name    , 0 ;
    Col_room_network , M.c_network , 1 ;
    Col_room_nusers  , M.c_nusers  , 2 ;
    Col_room_state   , M.c_state   , 3 ;
] 

module Room = Column(struct 
      type column = room_column 
      let kind = "Room"
      let column_strings = room_column_strings
    end)

(* The columns for the shared file upload info.*)
type shared_file_up_column =
  Col_shared_file
| Col_shared_network
| Col_shared_upsize
| Col_shared_requests
| Col_shared_size
| Col_shared_uid

let shared_file_up_column_strings = [
    Col_shared_file     , M.c_filename , 0 ;
    Col_shared_network  , M.c_network  , 1 ;
    Col_shared_upsize   , M.c_uploaded , 2 ;
    Col_shared_requests , M.c_requests , 3 ;
    Col_shared_size     , M.c_size     , 4 ;
    Col_shared_uid      , M.c_uid      , 5 ;
  ] 
  
module Shared_files_up = Column
    (
     struct
       type column = shared_file_up_column
       let kind = "Shared_file_upload"
       let column_strings = shared_file_up_column_strings
     end
    )

(* The different columns which can be displayed for a folder. *)
type folder_column = 
  Col_dir_name

let folder_column_strings = [
    Col_dir_name,  M.c_folders, 0 ;
] 

module Directory = Column(struct 
      type column = folder_column 
      let kind = "Folder"
      let column_strings = folder_column_strings
    end)

(* The columns for the identities of IM.*)
type identity_column =
  Col_identity_name

let identity_column_strings = [
    Col_identity_name , M.c_name , 0 ;
  ] 

module IMIdentities = Column
    (
     struct
       type column = identity_column
       let kind = "IMIdentities"
       let column_strings = identity_column_strings
     end
    )

(* The columns for the accounts of IM.*)
type account_column =
  Col_account_name
| Col_account_status
| Col_account_protocol

let account_column_strings = [
    Col_account_name     , M.c_name     , 0 ;
    Col_account_status   , M.c_status   , 1 ;
    Col_account_protocol , M.c_protocol , 2 ;
  ] 

module IMAccount = Column
    (
     struct
       type column = account_column
       let kind = "IMAccounts"
       let column_strings = account_column_strings
     end
    )
