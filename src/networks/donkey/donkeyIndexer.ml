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

open AnyEndian
open Printf2
open Md4
open LittleEndian
open CommonSearch
open GuiProto
open CommonTypes
open CommonResult
open BasicSocket
open DonkeyGlobals
open DonkeyMftp
open Options
open DonkeyTypes
open DonkeyOptions
open CommonOptions
open CommonGlobals

(* Use this latter to add comments to result. Comments can be downloaded
from WEB servers. *)

let update_comment_result md4 comment =
  try
    let rs = Hashtbl.find results_by_md4 md4 in
    let r = doc_value rs.result_index in
    r.result_comment <- comment;
    Store.update store rs.result_index r
  with _ -> ()
  
let add_comment md4 comment =
  try
    let old_comment = Hashtbl.find comments md4 in
    if not (String2.contains old_comment comment) then 
      let comment = Printf.sprintf "%s\n%s" old_comment comment in
      Hashtbl.remove comments md4;
      Hashtbl.add comments md4 comment;
      update_comment_result md4 comment
  with _ ->
      Hashtbl.add comments md4 comment;
      update_comment_result md4 comment
      
let load_comments filename = 
  try
    Unix2.tryopen_read filename (fun ic ->
    try
      while true do 
        let s = read_request ic in
        let md4 = get_md4 s 0 in
        let comment,_ = get_string s 16 in
        add_comment md4 comment
      done
    with 
      End_of_file -> ())
  with e ->
      lprintf "Error loading %s: %s" filename (Printexc2.to_string e);
      lprint_newline () 

let save_comments () =
  Unix2.tryopen_write filename (fun oc ->
  let buf = Buffer.create 256 in
  Hashtbl.iter (fun md4 comment ->
      Buffer.reset buf;
      buf_md4 buf md4;
      buf_string buf comment;
      output_request oc (Buffer.contents buf);
  ) comments)

let comment_result r doc = 
  try
    r.result_comment <- Hashtbl.find comments r.result_md4;
    Store.update store doc r
  with _ -> ()

let buf_tag b tag =
  buf_string b tag.tag_name;
  match tag.tag_value with
    String s -> buf_int8 b 0; buf_string b s
  | Uint64 i -> buf_int8 b 1; buf_int64_32 b i
  | Fint64 i -> buf_int8 b 2; buf_int64_32 b i
  | Addr ip ->  buf_int8 b 3; buf_ip b ip
  
let output_value oc h =
  let b = Buffer.create 100 in
  buf_list buf_string b h.hresult_names;
  buf_md4 b h.hresult_md4;
  buf_int64_32 b h.hresult_size;
  buf_list buf_tag b h.hresult_tags;
  let s = Buffer.contents b in
  output_request oc s

let get_tag s pos =
  let name, pos = get_string s pos in
  let t = get_uint8 s pos in
  let tag, pos =
    if t = 0 then
      let s, pos = get_string s (pos+1) in
      String s, pos
    else 
    if t = 1 then
      Uint64 (get_uint64_32 s (pos+1)), pos + 5
    else
    if t = 2 then
      Fint64 (get_uint64_32 s (pos+1)), pos + 5
    else
    let ip = get_ip s (pos+1) in
    Addr ip, pos + 5
  in
  { tag_name = name; tag_value = tag }, pos
  
let input_value ic =
  let s = read_request ic in
  let (names, pos) = get_list get_string s 0 in
  let md4 = get_md4 s pos in
  let size = get_uint64_32 s (pos + 16) in
  let pos = pos + 16 + 4 in
  let (tags, pos) = get_list get_tag s pos in
  {
    hresult_names = names;
    hresult_md4 = md4;
    hresult_size = size;
    hresult_tags = tags;
  }
  
let input_result ic = 
  let hresult = input_value ic in
  let info = {
      result_num = 0;
      result_network = network.network_num;
      result_names = hresult.hresult_names;
      result_md4 = hresult.hresult_md4;
      result_size = hresult.hresult_size;
      result_format = "";
      result_type = "";
      result_tags = hresult.hresult_tags;
      result_comment = "";
      result_done = false;
    } in
  List.iter (fun tag ->
      match tag with
        { tag_name = "format"; tag_value = String s } ->
          info.result_format <- s
      | { tag_name = "type"; tag_value = String s } ->
          info.result_type <- s
      | _ -> ()
  ) info.result_tags;
    info
    
    (*
      result_impl = {
      impl_result_num = 0;
      impl_result_val = file;
      impl_result_ops = result_ops;
    } in
  new_result result_impl;
  *)

let input_old_result ic = 
  let hresult = Pervasives.input_value ic in
  
  let o = Obj.repr hresult in
  lprintf "Type int: %s\n" (string_of_bool (Obj.is_int o));
  if not (Obj.is_int o) then lprintf "Size: %d\n" (Obj.size o);
    
  let info = {
      result_num = 0;
      result_network = network.network_num;
      result_names = hresult.hresult_names;
      result_md4 = hresult.hresult_md4;
      result_size = hresult.hresult_size;
      result_format = "";
      result_type = "";
      result_tags = hresult.hresult_tags;
      result_comment = "";
      result_done = false;
    } in
  List.iter (fun tag ->
      match tag with
        { tag_name = "format"; tag_value = String s } ->
          info.result_format <- s
      | { tag_name = "type"; tag_value = String s } ->
          info.result_type <- s
      | _ -> ()
  ) info.result_tags;
  info
  
let clear () =
  DocIndexer.clear index;
  Hashtbl.clear results_by_md4;
  match !history_file_oc with
    None -> ()
  | Some oc -> 
      close_out oc;
      history_file_oc := Some (open_out history_file) (* truncate !! *)

let close_history_oc () =
  match !history_file_oc with
    None -> ()
  | Some oc -> 
      close_out oc;
      history_file_oc := None

let history_file_oc () =
  match !history_file_oc with
    None ->
(*      lprintf "CREATE HISTORY CHANNEL"; lprint_newline (); *)
      let oc = open_out_gen [Open_binary; Open_append; Open_wronly;
          Open_creat] 0o666 history_file
      in
      history_file_oc := Some oc;
      oc
  | Some oc -> oc

          
let output_result result =
  if !!save_file_history then
  output_value (history_file_oc ()) 
  {
      hresult_names = result.result_names;
      hresult_md4 = result.result_md4;
      hresult_size = result.result_size;
      hresult_tags = result.result_tags;
    }

  
  
          
let index_string doc s fields =
  let words = String2.stem s in
  List.iter (fun s ->
(*      lprintf "ADD [%s] in index" s; lprint_newline (); *)
      DocIndexer.add  index s doc fields
  ) words 
  
let index_name r name = 
  index_string r name 1 (* general search have field 1 *)

let indexer = ref None

let add_to_local_index_queue = ref []

  (*
let add_to_local_index r =
  if !!local_index_add_cmd <> "" then 
    add_to_local_index_queue := r :: !add_to_local_index_queue
  
let refill_add_to_local_index t_out =
  if !add_to_local_index_queue = [] then
    TcpBufferedSocket.close t_out "finished"
  else
  let (before, after) = List2.cut 50 !add_to_local_index_queue in
  add_to_local_index_queue := after;
  
  let buf = Buffer.create 1000 in
  List.iter (fun r ->
      
      List.iter (fun name -> 
          Printf.bprintf  buf "name:%s\n" name
      ) r.result_names;
      Printf.bprintf buf "size:%s\n" (Int64.to_string r.result_size);
      Printf.bprintf buf "md4:%s\n" (Md4.to_string r.result_md4);
      if r.result_format <> "" then
        Printf.bprintf buf "format:%s\n" r.result_format;
      if r.result_type <> "" then
        Printf.bprintf buf "type:%s\n" r.result_type;
      List.iter (fun tag ->
          match tag.tag_value with
            String s ->
              Printf.bprintf buf "string_tag:%s:%s\n" tag.tag_name s
          | Uint64 i | Fint64 i ->
              Printf.bprintf buf "int_tag:%s:%s\n" tag.tag_name 
                (Int64.to_string i)
          | _ -> ()
      ) r.result_tags;
      Buffer.add_string buf "end result\n";
      
  ) before;
  
  let s = Buffer.contents buf in
  TcpBufferedSocket.write_string t_out s   
  
let add_to_local_index_timer _ =

  if !add_to_local_index_queue <> [] &&
    !indexer = None then begin
      try
        let t_out =
          match !indexer with
            None ->
              let (t_in, t_out) = TcpBufferedSocket.exec_command !!local_index_add_cmd [||] 
                  (fun sock ev -> ()) in
              indexer := Some (t_in, t_out);
              TcpBufferedSocket.set_closer t_in (fun _ _ ->
                  match !indexer with
                    None -> ()
                  | Some (t_in_old, t_out_old) ->
                      if t_out_old == t_out then
                        indexer := None);
              TcpBufferedSocket.set_closer t_out (fun _ _ ->
                  match !indexer with
                    None -> ()
                  | Some (t_in_old, t_out_old) ->
                      if t_out_old == t_out then
                        indexer := None);
              t_out
          | Some (t_in, t_out) -> t_out
        in
        TcpBufferedSocket.set_refill t_out refill_add_to_local_index        

      with e ->
          lprintf "Exception %s while starting local_index_add"
            (Printexc2.to_string e); lprint_newline ()
    
    end
      *)

let result_add_by_md4 r =
  
  let rec rs = {
      result_result = result_impl;
      result_index = Store.dummy_index;
    } and result_impl = {
      dummy_result_impl with
      impl_result_val = rs;
      impl_result_ops = result_ops;
    } in
  new_result result_impl;
  Hashtbl.add results_by_md4 r.result_md4 rs; 
  r.result_num <- result_impl.impl_result_num;
  rs
  
let index_result_no_filter r =
  try
(*    lprintf "RESULT %s" (Md4.to_string r.result_md4);
    lprint_newline (); *)
    let rs = Hashtbl.find results_by_md4 r.result_md4 in
    let rr = doc_value rs.result_index in
    List.iter (fun name ->
        if not (List.mem name rr.result_names) then begin
            rr.result_names <- name :: rr.result_names;
            index_name rs.result_index name
          end
    ) r.result_names;
    Store.update store rs.result_index r;
    rs
  with
    _ -> 
      
      if List.mem r.result_md4 !!DonkeyComplexOptions.old_files then
        r.result_done <- true
      else
      if Hashtbl.mem files_by_md4  r.result_md4 then
        r.result_done <- true;
      
      let rs = result_add_by_md4 r in
      
      let index = Store.add store r in

      rs.result_index <- index;
      
(*      (try add_to_local_index r with _ -> ()); *)
      
      if !!save_file_history then begin
          output_result r;
          flush (history_file_oc ());
        end;

      CommonSearch.Indexing.index_result (index_string rs.result_index) r;
      
      (*
      List.iter (fun name ->
          index_name rs.result_index name
      ) r.result_names;
      
      List.iter (fun tag ->
          match tag with
          | { tag_name = FT_FILE_ARTIST; tag_value = String s } -> 
              index_string rs.result_index s artist_bit
          | { tag_name = FT_FILE_ARTIST tag_value = String s } -> 
              index_string rs.result_index s title_bit
          | { tag_name = FT_FILE_ALBUM tag_value = String s } -> 
              index_string rs.result_index s album_bit
              (* we could directly use the fields of r *)
          | { tag_name = FT_FILE_FORMAT; tag_value = String s } -> 
              index_string rs.result_index s format_bit
          | { tag_name = FT_FILE_TYPE; tag_value = String s } -> 
              index_string rs.result_index s media_bit
          | { tag_value = String s } -> 
              index_name rs.result_index s
          | _ -> ()
) r.result_tags;
  *)
      rs

let index_result r =
(*    if not !!use_file_history then r else *)
    let rs = index_result_no_filter r in
    if DocIndexer.filtered rs.result_index then raise Not_found;
    rs

let add_name r file_name =
  if !!use_file_history then
    try
      let rs = Hashtbl.find results_by_md4 r.result_md4 in
      let rr = doc_value rs.result_index in
      if r != rr then raise Not_found;
      if not (List.mem file_name r.result_names) then begin
          r.result_names <- file_name :: r.result_names;
          Store.update store rs.result_index r;
          index_name rs.result_index file_name
        end
    with _ ->
        r.result_names <- file_name :: r.result_names;
        ignore (index_result_no_filter r)
  else begin
      r.result_names <- file_name :: r.result_names;      
    end

          (*

let has_word s bit =
  match String2.stem s with
    [] -> assert false
  | s :: tail -> 
      List.fold_left (fun q s ->
          Indexer.And (q, (Indexer.HasField (bit, s)))
      ) (Indexer.HasField (bit, s)) tail

let query_to_indexer q =
  let rec iter q =
    match q with
      QAnd (q1, q2) ->
        Indexer.And (iter q1, iter q2)  
    | QOr  (q1, q2) ->
        Indexer.Or (iter q1, iter q2)  
    | QAndNot (q1, q2) ->
        Indexer.AndNot (iter q1, iter q2)  
    | QHasWord s -> has_word s 0xffffffff
    | QHasField (f, s) ->
        has_word s (
          if f = "type" then media_bit else
          if f = "format" then  format_bit  else
          if f = "Title" then title_bit else
          if f = "Artist" then artist_bit else
          if f = "Album" then album_bit 
          else 0xffffffff);
    | QHasMinVal (f,size) ->
        Indexer.Predicate
          (if f = "size" then
            (fun doc -> 
                let r = doc_value doc in
                r.result_size >= size)
          else (fun doc -> true))

    | QHasMaxVal (f,size) ->
        Indexer.Predicate (
          if f = "size" then
            (fun doc -> 
                let r = doc_value doc in
                r.result_size <= size)
          else (fun doc -> true))
    | QNone ->
	failwith "query_to_indexer: QNone in query"
  in
  iter q
    *)

let find s = 
  if not !!use_file_history then () else
(*  Indexer.print index; *)
  let req = ref [] in
  let pred = ref (fun _ -> true) in
  
  let ss = s.search_query in
  let req = CommonSearch.Indexing.query_to_indexer doc_value ss in  
  
  let docs = DocIndexer.query index req in
(*  lprintf "%d results" (Array.length docs); lprint_newline (); *)
  Array.iter (fun doc ->
      if DocIndexer.filtered doc then
        lprintf "doc filtered\n"
      else
      let r = doc_value doc in
      
      let rs = try
          Hashtbl.find results_by_md4  r.result_md4
        with _ -> result_add_by_md4 r
      in
      comment_result r doc;

(*    merge_result s doc.num; *)
(*      lprintf "search_add_result"; lprint_newline (); *)
      search_add_result_in s rs.result_result
  ) docs
  

let load_old_history () =
  Unix2.tryopen_read "history.dat" (fun ic ->
  try
    while true do
      ignore (index_result_no_filter (input_old_result ic))
    done
  with End_of_file -> ())
  
let init () =
(* load history *)
  if !! save_file_history then
    begin
      (try
          save_file_history =:= false;
          lprintf  "Loading history file ..."; 
          let list = ref [] in
	  try
	    Unix2.tryopen_read history_file (fun ic ->
              try
		while true do
		  let file = input_result ic in
		  let rs = index_result_no_filter file in
		  list := doc_value rs.result_index :: !list;
		done
              with 
		  End_of_file -> 
		    lprintf "done\n")
	  with
              e -> (* some error *)
              lprintf "Error %s reading history file\n"
                (Printexc2.to_string e);
              lprintf "Generating new file\n";
              begin try
                  (try close_history_oc () with _ -> ());
                  (try Sys.remove "history.met" with _ -> ());
                  List.iter (fun file ->
                      output_result file
                  ) !list;
                  close_history_oc ();
                with e ->            
                    lprintf "Error %s generating new history file\n"
                      (Printexc2.to_string e)
              end
        with _ -> ());
      save_file_history =:= true;
      close_history_oc ()
    end
  
let index_result_no_filter r = 
  let rs = index_result_no_filter r in
  let r = doc_value rs.result_index in
  comment_result r rs.result_index;
  rs
  
let index_result r = 
  let rs = index_result r in
  let r = Store.get store rs.result_index in
  comment_result r rs.result_index;
  rs
  
let find_names md4 =
  try
    let rs = Hashtbl.find results_by_md4 md4 in
    let r = doc_value rs.result_index in
    r.result_names
  with _ -> []
      
      
let add_comment md4 comment =
  add_comment md4 comment;
  save_comments ()


  
let find_result m = Hashtbl.find results_by_md4 m

let save_history () =
  if !!save_file_history then begin
      close_history_oc ();
      (try Unix2.rename history_file (history_file ^ ".tmp") with _ -> ());
      (try Sys.remove history_file with _ -> ());
      Hashtbl.iter (fun _ rs ->
          let r = doc_value rs.result_index in
          output_result r
      ) results_by_md4;
      close_history_oc ();
(*          
          (try Unix2.rename history_file (history_file ^ ".old") with _ -> ());
(try Unix2.rename (history_file ^ ".tmp") history_file with _ -> ())
  *)
    end
    
let install_hooks () =
  
  Options.option_hook filters (fun _ ->
      
      try
(*        lprintf "CLEAR OLD FILTERS"; lprint_newline (); *)
        DocIndexer.clear_filter index;
(*        lprintf "SET NEW FILTERS"; lprint_newline (); *)
        DocIndexer.filter_words index (String2.stem !!filters)
      with e ->
          lprintf "Error %s in set filters\n" (Printexc2.to_string e)
  )
  
