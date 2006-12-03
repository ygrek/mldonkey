(* Copyright 2001, 2002 Simon, INRIA *)
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
open CommonTypes

type request_result = 
| File_possible   (* we asked, but didn't know *)
| File_not_found  (* we asked, the file is not there *)
| File_new_source (* we never asked, but we should *)
| File_found      (* the file was found *)
| File_chunk      (* the file has chunks we want *)
| File_upload     (* we uploaded from this client *)
(* | File_unknown     We don't know anything *)

(*
val initial_new_source_score : int
val new_source_score : int
val not_found_score : int
val found_score : int
val chunk_score : int
val upload_score   : int
*)
val possible_score : int

module Make(M: 


(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(*                                                                       *)
(*                         FUNCTOR Argument                              *)
(*                                                                       *)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
    
  sig
    val module_name : string

    type source_uid

    val dummy_source_uid : source_uid
    val source_uid_to_value: source_uid -> Options.option_value
    val value_to_source_uid: Options.option_value -> source_uid

    type source_brand

    val dummy_source_brand : source_brand
    val source_brand_to_value: source_brand -> Options.option_value
    val value_to_source_brand: Options.option_value -> source_brand
      
    val direct_source : source_uid -> bool    
    val indirect_source : source_uid -> bool
  end) : (
  sig

(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(*                                                                       *)
(*                         FUNCTOR Signature                             *)
(*                                                                       *)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
      
    type source = {
      source_uid : M.source_uid;
      mutable source_files : file_request list;
      mutable source_score : int;
      mutable source_num : int;
      mutable source_age : int;
      mutable source_last_attempt : int;
      mutable source_sock : tcp_connection;
      mutable source_brand : M.source_brand;
    }

    and file_request = {
      request_file : file_sources_manager;
      mutable request_queue : int;
      mutable request_time : int;
      mutable  request_score : int;
    }

    and file_sources_manager = {
      manager_uid : string;
      mutable manager_sources : source Queues.Queue.t array;
      mutable manager_active_sources : int;
      mutable manager_all_sources : int;
      mutable manager_file : (unit -> file);
    }

    and functions = {
      mutable function_connect: (M.source_uid -> unit);
      mutable function_query: (M.source_uid -> string -> unit);
      mutable function_string_to_manager: (string -> file_sources_manager);
      mutable function_max_connections_per_second : (unit -> int);
      mutable function_max_sources_per_file : (unit -> int);
      
      mutable function_add_location : 
        (M.source_uid -> string -> unit);
      mutable function_remove_location : 
        (M.source_uid -> string -> unit);
    }

    val functions : functions

    val create_file_sources_manager : string -> file_sources_manager
    val remove_file_sources_manager : file_sources_manager -> unit
(*
    val number_of_sources : file_sources_manager -> int
*)        
(* Find a given source *)

    val find_source_by_uid : M.source_uid -> source
(*
    val find_source_by_num : int -> source
*)
(* Feed-back on sources *)
    val source_connected : source -> unit
    val source_disconnected : source -> unit

    val add_request : source -> file_sources_manager  -> int -> file_request
    val set_request_result : 
      source -> file_sources_manager -> request_result -> unit
    val find_request : source -> file_sources_manager -> file_request
    val find_request_result : source -> file_sources_manager -> request_result

    val need_new_sources : file_sources_manager -> bool 

(* Connect sources every second *)
    val connect_sources : TcpBufferedSocket.connection_manager -> unit

    val attach_sources_to_file : Options.options_section -> (unit -> unit)

    val print : Buffer.t -> CommonTypes.output_type -> unit

    val indirect_connections : int ref

    val dummy_source : source        
(*  
    val query_file : source -> file_sources_manager -> unit
*)
    val query_files : source -> unit

    val clean_sources : unit -> unit

    val iter_all_sources : (source -> unit) -> file_sources_manager -> unit
    val iter_active_sources : (source -> unit) -> file_sources_manager -> unit
    val iter_qualified_sources : 
      (source -> unit) -> file_sources_manager -> unit
    val iter_relevant_sources : 
      (source -> unit) -> file_sources_manager -> unit

    val source_brand : source -> M.source_brand
    val set_source_brand : source -> M.source_brand -> unit
  end)

