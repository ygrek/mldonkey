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

open ServerTypes
open CommonOptions
open CommonTypes
open Unix
open TcpBufferedSocket
open DonkeyMftp
open Options
open DonkeyProtoCom
  
(*
  
type tagged_file =  {
    f_md4: Md4.t;
    f_ip: Ip.t;
    f_port: int;
    f_tags: tag list;
  }

    *)
  
module DocIndexer = Indexer2.FullMake(Document)
  
let index = DocIndexer.create ()
let table = Hashtbl.create 1023

  
(****************************************************)  
        
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
(*      lprintf "ADD [%s] in index" s; lprint_newline (); *)
      DocIndexer.add  index s doc fields
  ) words 
  
let index_name doc name = 
  index_string doc name 1 (* general search have field 1 *)

let index_file doc file = 
  
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
      Indexer.Predicate (fun index -> 
          let f = Store.get store index in
          List.exists (fun tag ->
              tag.tag_name = field &&
              match tag.tag_value with
                Uint32 v | Fint32 v -> v > minval
              | _ -> false
          ) f.f_tags
      )      
  | QHasMaxVal (field, maxval) -> 
      Indexer.Predicate (fun index -> 
          let f = Store.get store index in
          List.exists (fun tag ->
              tag.tag_name = field &&
              match tag.tag_value with
                Uint32 v | Fint32 v -> v < maxval
              | _ -> false
          ) f.f_tags
      )      
    
      
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
open ServerTypes
  
let add file = 
  try
    let doc = Hashtbl.find table file.f_md4 in
    let file2 = Store.get store doc in

    if file <> file2 then
      index_file doc file;
    (*lprintf "Must check files with same md4"; lprint_newline ();*)
    ()
  with _ ->
      let doc = Store.add store file in
      Hashtbl.add table file.f_md4 doc;
      index_file doc file;
      ()   

let get_def file_md4 =
  let doc = Hashtbl.find table file_md4 in
    (Store.get store doc)
        
let find query = 
  let docs = DocIndexer.query index query in
  { 
    docs = docs;
    next_doc = 0;
  }
        
let find_map query = 
  DocIndexer.query_map index query

  
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
  
