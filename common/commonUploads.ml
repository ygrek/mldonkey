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


open CommonInteractive
open CommonClient
open CommonComplexOptions
open CommonTypes
open CommonFile
open Options
open BasicSocket
open TcpBufferedSocket

open CommonGlobals
open CommonOptions

(* We should implement a common uploader too for all networks where
upload is done linearly. *)
  
(*******************************************************************

  
                         TYPES

  
*******************************************************************)
  
type shared_file = {
    shared_fullname : string;
    shared_codedname : string;
    shared_size : int64;
    shared_fd : Unix32.t;
    shared_id : int;
    shared_format : CommonTypes.format;
  }

and shared_tree =
  { 
    shared_dirname : string;
    mutable shared_files : shared_file list;
    mutable shared_dirs : (string * shared_tree) list;
  }

  
type upload = {
    upload_file : file; (* the file being uploaded *)
    upload_client : client;
    mutable upload_pos : int64; (* the position in the file *)
    mutable upload_end : int64; (* the last position in the file to upload *)
    mutable upload_sock : TcpBufferedSocket.t option; (* the socket for upload (often shared in client structure) *)
    mutable upload_on_close : (upload -> unit); (* function called when
    the socket is closed (Unix.close is already done) *)
    upload_subdir : string Options.option_record;
(* The subdir option for the commit *)
    upload_on_finish : (upload -> unit);
(* A function to call when upload is finished ! *)    
  }

    
(*******************************************************************

  
                      DATA STRUCTURES

  
*******************************************************************)
  
let shareds_counter = ref 0
let shared_counter = ref (Int64.zero)
let shared_files = Hashtbl.create 13 
  
module Document = struct
    type t = shared_file
      
    let num t = t.shared_id
    let filtered t = false
    let filter t bool = ()
  end

module DocIndexer = Indexer2.FullMake(Document)
  
let index = DocIndexer.create ()
let table = Hashtbl.create 1023     

let filesize_field = "size"
  
(*******************************************************************

  
                      INDEXER

  
*******************************************************************)

  
module Indexer = struct
        
let stem s =
  let s = String.lowercase (String.copy s) in
  for i = 0 to String.length s - 1 do
    let c = s.[i] in
    match c with
      'a'..'z' | '0' .. '9' -> ()
    | _ -> s.[i] <- ' ';
  done;
  String2.split_simplify s ' '

let name_bit = 1
(* "size" *)
(* "bitrate" *)
let artist_bit = 2 (* tag "Artiste" *)
let title_bit = 4  (* tag "Title" *)
let album_bit = 8 (* tag "Album" *)
let media_bit = 16 (* "type" *)
let format_bit = 32 (* "format" *)
      
let index_string doc s fields =
  let words = stem s in
  List.iter (fun s ->
(*      Printf.printf "ADD [%s] in index" s; print_newline (); *)
      DocIndexer.add  index s doc fields
  ) words 
  
let index_name doc name = 
  index_string doc name 1 (* general search have field 1 *)

(***********************************************)

let bit_of_field field =
  match field with
  | "filename" -> name_bit
  | "Artist" -> artist_bit
  | "Title" -> title_bit
  | "Album" -> album_bit
  | "format" -> format_bit 
  | "type" -> media_bit
  | _ -> raise Not_found
        
let rec query_to_query t = 
  match t with
  | QAnd (q1,q2) -> Indexer.And (query_to_query q1, query_to_query q2)
  | QOr (q1, q2) -> Indexer.Or (query_to_query q1, query_to_query q2)
  | QAndNot (q1,q2) -> Indexer.AndNot (query_to_query q1, query_to_query q2)
  | QHasWord w -> Indexer.HasWord w
  | QHasField (field, w) -> Indexer.HasField(bit_of_field field, w)
  | QHasMinVal (field, minval) -> 
      Indexer.Predicate (fun s -> 
          match field with 
          | "size" -> s.shared_size >= minval
          | _ -> true)
  | QHasMaxVal (field, maxval) -> 
      Indexer.Predicate (fun s -> 
          match field with 
          | "size" -> s.shared_size <= maxval
          | _ -> true)
    
      
  | _ -> failwith "Query not implemented by server"
      
(*
  
type query =
  QAnd of query * query
| QOr of query * query
| QAndNot of query * query
| QHasWord of string
| QHasField of string * string
| QHasMinVal of string * int32
| QHasMaxVal of string * int32
  
type 'a query =
  And of 'a query * 'a query
| Or of 'a query * 'a query
| AndNot of 'a query * 'a query
| HasWord of string
| HasField of int * string
| Predicate of ('a -> bool)

    *)

let index_file s =
  index_string s s.shared_codedname name_bit

  (*
  List.iter (fun tag ->
      match tag with
      | { tag_name = "filename"; tag_value = String s } -> 
          index_string doc s name_bit
      | { tag_name = "Artist"; tag_value = String s } -> 
          index_string doc s artist_bit
      | { tag_name = "Title"; tag_value = String s } -> 
          index_string doc s title_bit
      | { tag_name = "Album"; tag_value = String s } -> 
          index_string doc s album_bit
      | { tag_name = "format"; tag_value = String s } -> 
          index_string doc s format_bit
      | { tag_name = "type"; tag_value = String s } -> 
          index_string doc s media_bit
      | _ -> ()
  ) file.f_tags  
*)
  
(*
let add file = 
  try
    let doc = Hashtbl.find table file.f_md4 in
    let file2 = Store.get store doc in

    if file <> file2 then
      index_file doc file;
    (*Printf.printf "Must check files with same md4"; print_newline ();*)
    ()
  with _ ->
      let doc = Store.add store file in
      Hashtbl.add table file.f_md4 doc;
      index_file doc file;
      ()   

let get_def file_md4 =
  let doc = Hashtbl.find table file_md4 in
    (Store.get store doc)
  *)

let find query = DocIndexer.query index query 
        
let find_map query = 
  DocIndexer.query_map index query

(*
let get docs num = 
  let len = Array.length docs.docs in
  let rec iter pos num list =
    if num = 0 || pos >= len then
      begin
        docs.next_doc <- pos;
        if pos >= len then
          docs.docs <- [||];
        list
      end
    else
      iter (pos+1) (num-1) (Store.get store docs.docs.(pos) :: list)
  in
  iter docs.next_doc num []
*)
end
  
(*******************************************************************

  
                      FUNCTIONS

  
*******************************************************************)

  
let new_shared_dir dirname = {
    shared_dirname = dirname;
    shared_files = [];
    shared_dirs = [];
  }

let shared_tree = new_shared_dir ""

let rec add_shared_file node sh dir_list =
  match dir_list with
    [] -> assert false
  | [filename] ->
      node.shared_files <- sh :: node.shared_files
  | dirname :: dir_tail ->
      let node =
        try
          List.assoc dirname node.shared_dirs
        with _ ->
            let new_node = new_shared_dir dirname in
            node.shared_dirs <- (dirname, new_node) :: node.shared_dirs;
            new_node
      in
      add_shared_file node sh dir_tail
  
let add_shared full_name codedname size =
  if not( Hashtbl.mem shared_files codedname) then begin
      incr shareds_counter;
      let sh = {
          shared_fullname = full_name;
          shared_codedname = codedname;
          shared_size = size;
          shared_id = !shareds_counter;
          shared_fd=  Unix32.create full_name [Unix.O_RDONLY] 0o444;
          shared_format = CommonMultimedia.get_info full_name;
        } in
      Printf.printf "FILE ADDED: %s" codedname; print_newline ();
      Hashtbl.add table !shareds_counter sh;
      Hashtbl.add shared_files codedname sh;
      Indexer.index_file sh;
      add_shared_file shared_tree sh (String2.split codedname '/');
      shared_counter := Int64.add !shared_counter size;
      Printf.printf "Total shared : %s" (Int64.to_string !shared_counter);
      print_newline () 
    end
    
let query q = Indexer.find (Indexer.query_to_query q)
  
let find name = Hashtbl.find shared_files name
  
