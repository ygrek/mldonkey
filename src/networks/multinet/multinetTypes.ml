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

open Queues
open Printf2
open Options
open BasicSocket
open TcpBufferedSocket
  
open CommonClient
open CommonComplexOptions
open CommonTypes
open CommonFile

open CommonGlobals
open CommonOptions
open CommonSwarming

type bitzi_ticket =
| Bitzi_next_retry of int (* retry after this date *)
| Bitzi_not_found     (* don't retry *)
| Bitzi_ticket of string


type impl

type file = {        
    file_file : file CommonFile.file_impl;
    file_id : string;
    file_swarmer : Int64Swarmer.t;
    
    mutable file_uids : file_uid list;
    mutable file_filenames : string list;
    
    mutable file_locations : source Intmap.t; 
    mutable file_sources : source Queue.t array;    
    
    mutable file_impls : (impl network_file * impl) list;
    mutable file_files : (string * int64 * int64) list;
    mutable file_verified_partition : Int64Swarmer.partition;  
    mutable file_bitzi_ticket : bitzi_ticket;
  }

and 'a network_file = {
    mutable op_download_network : file_network;
    mutable op_download_finish : ('a -> unit); 
    mutable op_download_recover : ('a -> unit);
    mutable op_download_pause : ('a -> unit);
    mutable op_download_resume : ('a -> unit);
    mutable op_download_sources : ('a -> client list);
    mutable op_download_debug : ('a -> Buffer.t -> unit);
    mutable op_download_to_value : ('a -> (string * option_value) list);
  }

and file_network = {
    mutable op_download_netname : string;
    mutable op_download_start : (file -> unit);
    mutable op_download_of_value : (file ->
    (string * option_value) list -> unit);
    mutable op_source_fifo : source Fifo.t;
    mutable op_source_delay : int;
  }

and source = {
    source_num : int;                      (* client num *)
    source_addr : location_kind;           (* addr *)
    source_network : file_network;         (* network *)
    mutable source_age: int;               (* last successful connection *)
    mutable source_requests : file_request list; (* request *)
    mutable source_files : file list;      (* interesting files *)
    mutable source_retry: int;             (* next connection *)
    mutable source_score : int;            (* score *)
  }      

and file_request = {
    request_file : file;
    mutable request_time : int;
    mutable request_result : request_result;
  }

and request_result = 
| File_possible (* we asked, but didn't know *)
| File_not_found (* we asked, the file is not there *)
| File_expected (* we asked, because it was announced *)
| File_new_source (* we never asked *)
| File_found    (* the file was found *)
| File_chunk    (* the file has chunks we want *)
| File_upload   (* we uploaded from this client *)
  
  
let file_num file = file.file_file.impl_file_num
let file_size file = file.file_file.impl_file_size
let file_downloaded file = file_downloaded (as_file file.file_file)
let file_age file = file.file_file.impl_file_age
let file_fd file = file.file_file.impl_file_fd
let file_disk_name file = file_disk_name (as_file file.file_file)
let set_file_disk_name file = set_file_disk_name (as_file file.file_file)
let file_state file =  file_state (as_file file.file_file)      
let set_file_state file =  set_file_state (as_file file.file_file)      
let file_must_update file =
  file_must_update (as_file file.file_file)
let file_priority file = file.file_file.impl_file_priority
let file_best_name file = file_best_name (as_file file.file_file)
let set_file_best_name file = set_file_best_name (as_file file.file_file)
let file_add_source file = file_add_source (as_file file.file_file)      
let file_last_seen file = file.file_file.impl_file_last_seen
let set_file_last_seen file v = file.file_file.impl_file_last_seen <- v
let file_must_update_downloaded file = 
  file_must_update_downloaded (as_file  file.file_file)
let as_file file = as_file file.file_file
  
  


let add_uids file uids =
  List.iter (fun uid ->
      let uid = uid_of_uid uid in
      if not (List.mem uid file.file_uids) then
        file.file_uids <- uid :: file.file_uids
  ) uids

let sources_nqueues = 10
module SourcesQueueCreate = struct  
    
    let lifo = lifo 
    let fifo = fifo
    
    module SourcesSet = Set.Make (
        struct
          type t = int * source
          let compare (t1,s1) (t2,s2) = 
            if s1.source_num = s2.source_num then begin
                0 end else              
            let x = compare t1 t2 in
            if x = 0 then compare s1.source_addr s2.source_addr else x
        end
      )
    
    let oldest_first () = 
      let t = ref SourcesSet.empty in
      of_impl {
        head = (fun _ -> try SourcesSet.min_elt !t with _ -> raise Fifo.Empty);
        put = (fun x ->  t := SourcesSet.add x !t);
        length = (fun _ -> SourcesSet.cardinal !t);
        take = (fun _ ->
            try 
              let x = SourcesSet.min_elt !t in
              t := SourcesSet.remove x !t;
              x
            with _ -> raise Fifo.Empty);
        iter = (fun f ->
            SourcesSet.iter (fun (_,x) -> f x) !t);
        put_back = (fun e -> t := SourcesSet.add e !t);
      }
    
    let oldest_last () = 
      let t = ref SourcesSet.empty in
      of_impl {
        head = (fun _ ->
            try SourcesSet.max_elt !t with _ -> raise Fifo.Empty);
        put = (fun x ->  t := SourcesSet.add x !t);
        length = (fun _ -> SourcesSet.cardinal !t);
        take = (fun _ ->
            try
              let x = SourcesSet.max_elt !t in
              t := SourcesSet.remove x !t;
              x
            with _ -> raise Fifo.Empty);
        iter = (fun f ->
            SourcesSet.iter (fun (_,x) -> f x) !t);
        put_back = (fun e -> t := SourcesSet.add e !t);
      }
      
      
      
end