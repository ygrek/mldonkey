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

open Md4
open CommonTypes

type ips_list = {
    mutable nips : int;
    mutable ips : Ip.t list;
  }

let noips () = { ips = []; nips = 0; }
  
type 'a search_request = {
    mutable search_num : int;
    mutable search_type : search_type;
    mutable search_query : 'a; (* query_entry for the GUI *)
    mutable search_max_hits : int;
    mutable search_network : int;
  }

module Fields_file_info = struct

(* To decrease the size of the "file_info" messages sent to the GUI, send 
only useful fields. The "op_file_info" function should take this bitmap
as an argument, and "land" it with the plugin bitmap (to remove fields that
are irrelevant for the plugin) and put that into the file_info.

In the future, it might also be interesting that the GUI tells the core
which fields it is interesting in (why send the availability when the
  GUI cannot display it ?).
  *)
    
    type t = {
        mutable file_network : bool;
        mutable file_comment : bool;
        mutable file_name : bool;
        mutable file_names : bool;
        mutable file_md4 : bool;        
        mutable file_size : bool;
        mutable file_downloaded : bool;
        mutable file_nlocations : bool;
        mutable file_nclients: bool;
        mutable file_state : bool;
        mutable file_chunks : bool;
        mutable file_availability : bool;
        mutable file_sources : bool;
        mutable file_download_rate : bool;
        mutable file_format : bool;
        mutable file_chunks_age : bool;
        mutable file_age : bool;
        mutable file_last_seen : bool;
        mutable file_priority : bool;
        mutable file_uids : bool;
      }
    
    let all = {
        file_network = true;
        file_comment = true;
        file_name = true;
        file_names = true;
        file_md4 = true;        
        file_size = true;
        file_downloaded = true;
        file_nlocations = true;
        file_nclients= true;
        file_state = true;
        file_chunks = true;
        file_availability = true;
        file_sources = true;
        file_download_rate = true;
        file_format = true;
        file_chunks_age = true;
        file_age = true;
        file_last_seen = true;
        file_priority = true;
        file_uids = true;
      }
    
    type file_info_fields =    
    | File_network of int
    | File_comment of string
    | File_name of string
    | File_names of (string * ips_list) list
    | File_md4 of Md4.t        
    | File_size of int64
    | File_downloaded of int64
    | File_nlocations of int
    | File_nclients of int
    | File_state of file_state
    | File_chunks of string
    | File_availability of (int * string) list
    | File_download_rate of float
    | File_format of format
    | File_chunks_age of int array
    | File_age of int
    | File_last_seen of int
    | File_priority of int
(*    | File_uids of Uid.t list   *)
(*    | File_sources of int list option *)
      
  end

  
type file_info = {
    file_num : int;    
    file_fields : Fields_file_info.t;
    
    file_network : int;
    mutable file_comment : string;
    mutable file_name : string;
    mutable file_names : (string * ips_list) list;
    mutable file_md4 : Md4.t;        
    mutable file_size : int64;
    mutable file_downloaded : int64; (* LOT OF CHANGES *)
    mutable file_active_sources : int; (* MANY CHANGES *)
    mutable file_all_sources : int;
    mutable file_state : file_state;
    mutable file_chunks : string;
    mutable file_availability : (int * string) list; (* MANY CHANGES *)
    mutable file_sources : int list option;
    mutable file_download_rate : float; (* LOT OF CHANGES *)
    mutable file_format : format;
    mutable file_chunks_age : int array;
    mutable file_age : int;
    mutable file_last_seen : int;
    mutable file_priority : int;
    mutable file_uids : Uid.t list;
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
    
    mutable server_addr : Ip.addr;
    mutable server_port : int;
    mutable server_realport : int;
    mutable server_score : int;
    mutable server_tags : CommonTypes.tag list;
    mutable server_nusers : int64;
    mutable server_nfiles : int64;
    mutable server_state : host_state;
    mutable server_name : string;
    mutable server_description : string;
    mutable server_users : int list option;
    mutable server_banner : string;
    mutable server_preferred : bool;
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
| TreeFile of  result_info
  
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
    mutable client_connect_time : int;
    mutable client_software : string;
    mutable client_release : string;
    mutable client_emulemod : string;
    mutable client_downloaded : int64;
    mutable client_uploaded : int64;
    mutable client_upload : string option;
(*  mutable client_sock_addr : string;  *)
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
    mutable connected_networks : (int * int) list; (* network/connected servers *)
    mutable ndownloaded_files : int;
    mutable ndownloading_files : int;
  }

type shared_info = {
    shared_num : int;
    shared_network : int;
    mutable shared_filename : string;
    mutable shared_size : int64;
    mutable shared_uploaded : int64;
    mutable shared_requests : int;
    mutable shared_uids : Uid.t list; (* net file UID *)
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
  
type option_widget = 
  StringEntry
| BoolEntry
| FileEntry

open BasicSocket
  
let file_info_test = 
  {
    file_fields = Fields_file_info.all;
    
    file_comment = "";
    file_name = "tratra";
    file_num = 356;
    file_network = 873;
    file_names = ["toto", noips(); "tutu", noips()];
    file_md4 = Md4.random ();
    file_size = Int64.of_string "68758765";
    file_active_sources = 12;
    file_all_sources = 18;
    file_state = FileDownloading;
    file_sources = None;
    file_download_rate = 2.2;
    file_chunks = "1010100";
    file_downloaded = Int64.of_string "68758764";
    file_availability = [0,""];
    file_format = FormatUnknown;
    file_chunks_age = [| last_time () - 2 |];
    file_age = last_time () - 3;
    file_last_seen = BasicSocket.last_time ();
    file_priority = 0;
   file_uids = [];
 } 
  
