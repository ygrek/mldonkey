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

open CommonGlobals
open CommonTypes


type options = {
    mutable connection_port : int;
    mutable control_port : int;
    mutable gui_port : int;
    
    mutable save_options_delay : float;
    mutable check_client_connections_delay : float;
    mutable check_server_connections_delay : float;
    mutable small_retry_delay : float;
    mutable medium_retry_delay : float;
    mutable long_retry_delay : float;
    
    mutable name : string;
    mutable max_connected_servers : int;
    mutable upload_limit : int;
    mutable features : string;
    
    mutable server_timeout: float;
    mutable client_timeout: float;
    mutable max_server_age : int;
    mutable password : string;
  }
  
type 'a search_request = {
    mutable search_num : int;
    mutable search_type : search_type;
    mutable search_query : 'a; (* query_entry for the GUI *)
    mutable search_max_hits : int;
  }

  
type file_info = {
    file_num : int;    
    file_network : int;
    
    mutable file_name : string;
    mutable file_names : string list;
    mutable file_md4 : Md4.t;        
    mutable file_size : int32;
    mutable file_downloaded : int32; (* LOT OF CHANGES *)
    mutable file_nlocations : int; (* MANY CHANGES *)
    mutable file_nclients: int;

    mutable file_state : file_state;
    mutable file_chunks : string;
    mutable file_availability : string; (* MANY CHANGES *)
    mutable file_sources : int list option;
    mutable file_download_rate : float; (* LOT OF CHANGES *)
    mutable file_format : format;
    mutable file_chunks_age : float array;
    mutable file_age : float;
  }
  
type user_info = {
    user_num : int;
    user_md4 : Md4.t;
    user_name : string;
    user_ip : Ip.t;
    user_port : int;
    mutable user_tags : tag list;
    user_server : int;
  }

type server_info = {
    server_num : int;
    server_network : int;
    
    mutable server_addr : addr;
    mutable server_port : int;
    mutable server_score : int;
    mutable server_tags : CommonTypes.tag list;
    mutable server_nusers : int;
    mutable server_nfiles : int;
    mutable server_state : host_state;
    mutable server_name : string;
    mutable server_description : string;
    mutable server_users : int list option;
  } 

type room_info = {
    room_num : int;
    room_network : int;
    room_name : string;
    mutable room_state : room_state;
    mutable room_users : int list;
    mutable room_messages : room_message list;
    mutable room_nusers : int;
  }

type file_tree =
  {
    mutable file_tree_name : string;
    mutable file_tree_list : file_tree_item list;
  }
  
and file_tree_item =
  TreeDirectory of file_tree
| TreeFile of result_info
  
type client_info = {
    client_num : int;
    client_network : int;
    
    mutable client_kind : location_kind;
    mutable client_state : host_state;
    mutable client_type : client_type;
    mutable client_tags: CommonTypes.tag list;
    mutable client_name : string;
    mutable client_files:  file_tree option;
    mutable client_rating : int;
    mutable client_chat_port : int;
  }

type client_stats = {
    mutable upload_counter : int64;
    mutable download_counter : int64;
    mutable nshared_files : int;
    mutable shared_counter : int64;
    mutable tcp_upload_rate : int; (* bytes/second *)
    mutable tcp_download_rate : int; (* bytes/second *)
    mutable udp_upload_rate : int; (* bytes/second *)
    mutable udp_download_rate : int; (* bytes/second *)
  }

type shared_info = {
    shared_num : int;
    shared_network : int;
    shared_filename : string;
    shared_size : int32;
    mutable shared_uploaded : int64;
    mutable shared_requests : int;
  }
  
  
let add_file tree dirname r =
  let path = Filename2.path_of_filename dirname in
  
  let rec iter list tree =
    match list with
      [] ->
        let r = TreeFile r in
        (if not (List.mem r tree.file_tree_list) then
            tree.file_tree_list <- r :: tree.file_tree_list)
    | dirname :: tail ->
        iter2 tail tree dirname tree.file_tree_list
        
  and iter2 list tree dirname items =
    match items with
      [] ->
        let new_tree = { file_tree_name = dirname; file_tree_list = [] } in
        tree.file_tree_list <- (TreeDirectory new_tree) :: tree.file_tree_list;
        iter list new_tree
    | (TreeDirectory old_tree) :: items ->
        if old_tree.file_tree_name = dirname then
          iter list old_tree
        else
          iter2 list tree dirname items
    | _ :: items ->
        iter2 list tree dirname items
  in
  iter path tree    

let list_directory_files tree =
  let rec iter list items =
    match items with
      [] -> list
    | (TreeDirectory tree) :: items -> iter list items
    | (TreeFile r) :: items -> iter (r :: list) items        
  in
  iter [] tree.file_tree_list

let list_files tree =
  let rec iter list items =
    match items with
      [] -> list
    | (TreeDirectory tree) :: items -> 
        iter (iter list tree.file_tree_list) items
    | (TreeFile r) :: items -> iter (r :: list) items


        
  in
  iter [] tree.file_tree_list
  
type arg_handler =  connection_options -> string
type arg_kind = 
  Arg_none of arg_handler
| Arg_multiple of (string list -> arg_handler)
| Arg_one of (string -> arg_handler)
| Arg_two of (string -> string -> arg_handler)
| Arg_three of (string -> string -> string -> arg_handler)

type option_widget = 
  StringEntry
| BoolEntry
| FileEntry
  