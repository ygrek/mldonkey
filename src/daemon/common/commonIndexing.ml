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

open Printf2
open CommonGlobals
open CommonTypes



exception EmptyQuery
  
let string_of_query q =
  let rec iter q =
    match q with
      QAnd (q1, q2) -> Printf.sprintf "(%s) AND (%s)" (iter q1) (iter q2)
    | QOr (q1, q2) ->  Printf.sprintf "(%s) OR (%s)" (iter q1) (iter q2)
    | QAndNot (q1, q2) ->  Printf.sprintf "(%s) AND NOT (%s)" (iter q1) (iter q2)
    | QHasWord s -> Printf.sprintf "CONTAINS[%s]" s
    | QHasField (f,s) -> Printf.sprintf "[%s]CONTAINS[%s]" (string_of_field f) s
    | QHasMinVal (f,v) -> Printf.sprintf "[%s]>%Ld" (string_of_field f) v
    | QHasMaxVal (f,v) -> Printf.sprintf "[%s]<%Ld" (string_of_field f) v
    | QNone ->
        lprintf "QNone in query\n";
        ""
  in
  iter q

let has_word s bit =
  match String2.stem s with
    [] -> raise EmptyQuery
  | s :: tail -> 
      List.fold_left (fun q s ->
          Indexer.And (q, (Indexer.HasField (bit, s)))
      ) (Indexer.HasField (bit, s)) tail
      
      
let rec rec_simplify_query q =
  match q with
    QAnd (q1, q2) ->
      (
       match (rec_simplify_query q1, rec_simplify_query q2) with
        QNone, QNone -> QNone
       | QNone, q2' -> q2'
       | q1', QNone -> q1'
       | q1', q2' -> QAnd (q1',q2')
      )
  | QOr (q1, q2) ->
      (
       match (rec_simplify_query q1, rec_simplify_query q2) with
         QNone, QNone -> QNone
       | QNone, q2' -> q2'
       | q1', QNone -> q1'
       | q1', q2' -> QOr (q1',q2')
      )
  | QAndNot (q1, q2) ->
      (
       match (rec_simplify_query q1, rec_simplify_query q2) with
         QNone, QNone -> QNone
       | QNone, q2' -> QNone
       | q1', QNone -> q1'
       | q1', q2' -> QAndNot (q1',q2')
      )
  | QHasWord _
  | QHasField _
  | QHasMinVal _
  | QHasMaxVal _
  | QNone -> q

let rec canonize_query q =
  let q1 = match q with
    
    | QAnd (q, QNone) | QAnd (QNone, q) -> q
    | QOr (q, QNone) | QOr (QNone,q) -> q
    | QAndNot (q, QNone) -> q
    
    | QAndNot ( QAndNot (q1,q2), q3 ) ->   QAndNot ( q1, QAnd(q2,q3))
    | QAndNot (q1, QAndNot(q2,q3)) ->      QAndNot (QAnd(q1,q2), q3)          
    | QAnd (q1, QAndNot (q2,q3)) ->        QAndNot (QAnd (q1,q2), q3)
    | QAnd (QAndNot (q1, q2), q3) ->       QAndNot (QAnd (q1, q3), q2)
    
    | QAnd (q1, q2) -> QAnd (canonize_query q1, canonize_query q2)
    | QOr (q1, q2) -> QOr (canonize_query q1, canonize_query q2)
    | QAndNot (q1, q2) -> QAndNot (canonize_query q1, canonize_query q2)
    | _ -> q
  in
  if q <> q1 then canonize_query q1 else q
    
let simplify_query q =
  let q = canonize_query q in
  match rec_simplify_query q with
    QNone -> QHasWord " "
  | q' -> q'
      
module Make(Stored : sig
      
      type search
      type result
      type stored_result
      
      val store_name : string
      val result_names : result -> string list
      val result_tags : result -> tag list
      val result_uids : result -> Uid.t list
      val result_size : result -> int64
      
      val result_index : stored_result -> Store.index
      
      val search_query : search -> CommonTypes.query
(*      val add_search_result : search -> result -> unit *)
    end) = struct
    
    let (store: Stored.result Store.t) = 
      Store.create Stored.store_name
    
    module Document = struct
        type t = Store.index
        
        let num t = Store.index t
        let filtered t = Store.get_attrib store t
        let filter t bool = Store.set_attrib store t bool
        let doc_value t = Store.get store t
      end
    
    let name_bit = 1 lsl 0
(* "size" *)
(* "bitrate" *)
    let artist_bit = 1 lsl 1 (* tag "Artiste" *)
    let title_bit = 1 lsl 2  (* tag "Title" *)
    let album_bit = 1 lsl 3 (* tag "Album" *)
    let media_bit = 1 lsl 4 (* "type" *)
    let format_bit = 1 lsl 5 (* "format" *)
    let uid_bit = 1 lsl 6  (* uid *)
    
    let index_result index_string r =
      
      List.iter (fun name ->
          index_string name name_bit
      ) (Stored.result_names r);
      
      List.iter (fun tag ->
          match tag with
          | { tag_name = Field_Artist; tag_value = String s } -> 
              index_string s artist_bit
          | { tag_name = Field_Title; tag_value = String s } -> 
              index_string s title_bit
          | { tag_name = Field_Album; tag_value = String s } -> 
              index_string s album_bit
(* we could directly use the fields of r *)
          | { tag_name = Field_Format; tag_value = String s } -> 
              index_string s format_bit
          | { tag_name = Field_Type; tag_value = String s } -> 
              index_string  s media_bit
          | { tag_name = Field_Length; tag_value = _ } -> ()
          | { tag_name = _; tag_value = String s } -> 
              index_string s name_bit
          | _ -> ()
      ) (Stored.result_tags r);
      
      List.iter (fun uid ->
          index_string (match Uid.to_uid uid with
              Ed2k _ -> "ed2k"
            | Md5Ext _ -> "ft"
            | TigerTree _ -> "ttr"
            | Md5 _ -> "md5"
            | Sha1 _ -> "sha1"
            | Bitprint _ -> "bp"
            | BTUrl _ -> "bt"
            | FileTP _ -> "filetp"
            | NoUid -> ""
          ) uid_bit
      ) (Stored.result_uids r);

(*
      if r.result_format <> "" then
        index_string r.result_format format_bit;
      if r.result_type <> "" then
        index_string r.result_type media_bit
*)    
      ()
      
    let query_to_indexer doc_value q =
      let rec iter q =
        match q with
        | QAnd (q1, q2) ->
            (try
                let e1 = iter q1 in
                try
                  let e2 = iter q2 in
                  Indexer.And (e1,e2)
                with 
                  EmptyQuery -> e1
(*                | InvertQuery e2 -> Indexer.AndNot(e1, e2) *)
              with 
                EmptyQuery -> iter q2
(*
                  | InvertQuery e1 ->
                  try
                    let e2 = iter q2 in
                    Indexer.AndNot (e2,e1)
                  with 
| InvertQuery e2 -> raise (InvertQuery (Indexer.And(e1, e2)))
  *)
            )
        | QOr  (q1, q2) ->
            (try
                let e1 = iter q1 in
                try
                  let e2 = iter q2 in
                  Indexer.Or (e1,e2)
                with _ -> e1
              with _ -> iter q2
            )
        
        | QAndNot (q1, q2) ->
            (
(*              try *)
              let e1 = iter q1 in
              try
                let e2 = iter q2 in
                Indexer.AndNot (e1,e2)
              with 
                EmptyQuery -> e1
(*                | InvertQuery e2 -> Indexer.And (e1,e2) 
              with EmptyQuery -> 
                  try
                    let e2 = iter q2 in
                    raise (PassInvertQuery e2)
                  with InvertQuery e2 -> e2
                  | PassInvertQuery e2 -> raise (InvertQuery e2) *)
            )
        
        | QHasWord s -> has_word s 0x7fffffff
        | QHasField (f, s) ->
            has_word s (
              match f with
                Field_Type -> media_bit 
              | Field_Format -> format_bit
              | Field_Title -> title_bit 
              | Field_Artist -> artist_bit 
              | Field_Album -> album_bit 
              | Field_Uid -> uid_bit
              | _ -> 0x7fffffff);
        | QHasMinVal (f,size) ->
            Indexer.Predicate
              (if f = Field_Size then
                (fun doc -> 
                    let r = doc_value doc in
                    (Stored.result_size r) >= size)
              else (fun doc -> true))
        
        | QHasMaxVal (f,size) ->
            Indexer.Predicate (
              if f = Field_Size then
                (fun doc -> 
                    let r = doc_value doc in
                    (Stored.result_size r) <= size)
              else (fun doc -> true))
        | QNone ->
            failwith "query_to_indexer: QNone in query"
      in
      iter q
    
    let query_to_indexer f q = 
      let q = simplify_query q in
(*      lprintf "query_to_indexer (%s): \n%s\n" Stored.store_name      
      (string_of_query q); *)
      query_to_indexer f q
      
    module DocIndexer = Indexer2.FullMake(Document)
    
    let get_result rs = Store.get store (Stored.result_index rs)
    let update_result rs r = Store.update store (Stored.result_index rs) r
    let add r = Store.add store r
    let remove_result rs = Store.remove store (Stored.result_index rs)
    
    module MakeIndex (FilterResult : sig
          
          val add_search_result : Stored.search -> Stored.result -> unit
        
        end) = struct      
        
        open FilterResult
        
        let index = DocIndexer.create ()
        
        let index_string doc s fields =
          let words = String2.stem s in
          List.iter (fun s ->
(*          lprintf "ADD %d: [%s] in index [field %d]\n"  
            doc.result_num s fields;  *)
              DocIndexer.add  index s doc fields
          ) words 
          
        let add rs = 
          let index = Stored.result_index rs in
          let r = get_result rs in
          index_result (index_string index) r
        
        let find s = 
          
          let ss = Stored.search_query s in
          let req = query_to_indexer Document.doc_value ss in  
          
          let docs = DocIndexer.query index req in
(*  lprintf "%d results\n" (List.length docs);  *)
          Array.iter (fun doc ->
              if DocIndexer.filtered doc then
                lprintf_nl "[cInd] doc filtered"
              else
              let r = Document.doc_value doc in
(*    merge_result s doc.num; *)
(*          lprintf "search_add_result: %d\n" r.result_num;  *)
              add_search_result s r
          ) docs
        
        let clear () =
          DocIndexer.clear index
        
        let stats () = DocIndexer.stats index
      
      end  
    
    let stats () = Store.stats store

    let _ =
      CommonGlobals.do_at_exit (fun _ ->
          Store.close store  
      ) 
      
  end
