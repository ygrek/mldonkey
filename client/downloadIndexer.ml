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

open DownloadGlobals
open Mftp
open Options
open DownloadTypes
open DownloadOptions
open Gui_types

(* Use this latter to add comments to result. Comments can be downloaded
from WEB servers. *)

let comments = Hashtbl.create 127

let comment_filename = "comments.met"

let name_bit = 1
(* "size" *)
(* "bitrate" *)
let artist_bit = 2 (* tag "Artiste" *)
let title_bit = 4  (* tag "Title" *)
let album_bit = 8 (* tag "Album" *)
let media_bit = 16 (* "type" *)
let format_bit = 32 (* "format" *)
  
let index = Indexer.create ()
  
let results = Hashtbl.create 1023
  
let history_file = "history.met"
let history_file_oc = ref None

let update_comment_result md4 comment =
  try
    let doc = Hashtbl.find results md4 in
    let r = Indexer.value doc in
    r.result_comment <- Some comment
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
    let ic = open_in filename in
    try
      while true do 
        let s = read_request ic in
        let md4 = get_md4 s 0 in
        let comment,_ = get_string s 16 in
        add_comment md4 comment
      done
    with 
      End_of_file -> close_in ic
    | e ->
        close_in ic;
        Printf.printf "Error loading %s: %s" filename (Printexc.to_string e);
        print_newline () 
  with e ->
      Printf.printf "Error loading %s: %s" filename (Printexc.to_string e);
      print_newline () 

let save_comments () =
  let oc = open_out comment_filename in
  let buf = Buffer.create 256 in
  Hashtbl.iter (fun md4 comment ->
      Buffer.clear buf;
      buf_md4 buf md4;
      buf_string buf comment;
      output_request oc (Buffer.contents buf);
  ) comments;
  close_out oc
  
let _ =
  load_comments comment_filename
  
let comment_result r = 
  match r.result_comment with
    Some _ -> ()
  | None ->
      try
        r.result_comment <- Some (Hashtbl.find comments r.result_md4)
      with _ -> ()

let buf_tag b tag =
  buf_string b tag.tag_name;
  match tag.tag_value with
    String s -> buf_int8 b 0; buf_string b s
  | Uint32 i -> buf_int8 b 1; buf_int32_32 b i
  | Fint32 i -> buf_int8 b 2; buf_int32_32 b i
  | Addr ip ->  buf_int8 b 3; buf_ip b ip
  
let output_value oc h =
  let b = Buffer.create 100 in
  buf_list buf_string b h.hresult_names;
  buf_md4 b h.hresult_md4;
  buf_int32_32 b h.hresult_size;
  buf_list buf_tag b h.hresult_tags;
  let s = Buffer.contents b in
  output_request oc s

let get_tag s pos =
  let name, pos = get_string s pos in
  let t = get_int8 s pos in
  let tag, pos =
    if t = 0 then
      let s, pos = get_string s (pos+1) in
      String s, pos
    else 
    if t = 1 then
      Uint32 (get_int32_32 s (pos+1)), pos + 5
    else
    if t = 2 then
      Fint32 (get_int32_32 s (pos+1)), pos + 5
    else
    let ip = get_ip s (pos+1) in
    Addr ip, pos + 5
  in
  { tag_name = name; tag_value = tag }, pos
  
let input_value ic =
  let s = read_request ic in
  let (names, pos) = get_list get_string s 0 in
  let md4 = get_md4 s pos in
  let size = get_int32_32 s (pos + 16) in
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
  let file = {
      result_names = hresult.hresult_names;
      result_md4 = hresult.hresult_md4;
      result_size = hresult.hresult_size;
      result_format = "";
      result_type = "";
      result_tags = hresult.hresult_tags;
      result_comment = None;
    }  in
  List.iter (fun tag ->
      match tag with
        { tag_name = "format"; tag_value = String s } ->
          file.result_format <- s
      | { tag_name = "type"; tag_value = String s } ->
          file.result_type <- s
      | _ -> ()
  ) file.result_tags;
  file
  
let input_old_result ic = 
  printf_char '<';
  let hresult = Pervasives.input_value ic in
  printf_char '>';
  
  let o = Obj.repr hresult in
  Printf.printf "Type int: %s" (string_of_bool (Obj.is_int o));
  print_newline ();
  if not (Obj.is_int o) then begin
      Printf.printf "Size: %d" (Obj.size o);
      print_newline ();
      
      
    end;
  
  
  let file = {
      result_names = hresult.hresult_names;
      result_md4 = hresult.hresult_md4;
      result_size = hresult.hresult_size;
      result_format = "";
      result_type = "";
      result_tags = hresult.hresult_tags;
      result_comment = None;
    }  in
  printf_char '!';
  List.iter (fun tag ->
      match tag with
        { tag_name = "format"; tag_value = String s } ->
          file.result_format <- s
      | { tag_name = "type"; tag_value = String s } ->
          file.result_type <- s
      | _ -> ()
  ) file.result_tags;
  file
  
let clear () =
  Indexer.clear index;
  Hashtbl.clear results;
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
(*      Printf.printf "CREATE HISTORY CHANNEL"; print_newline (); *)
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

  
  
      
let stem s =
  let s = String.lowercase (String.copy s) in
  for i = 0 to String.length s - 1 do
    let c = s.[i] in
    match c with
      'a'..'z' | '0' .. '9' -> ()
    | _ -> s.[i] <- ' ';
  done;
  String2.split_simplify s ' '
      
let index_string doc s fields =
  let words = stem s in
  List.iter (fun s ->
(*      Printf.printf "ADD [%s] in index" s; print_newline (); *)
      Indexer.add  index s doc fields
  ) words 
  
let index_name r name = 
  index_string r name 1 (* general search have field 1 *)

let indexer = ref None


let add_to_local_index r =
  
  if !!local_index_add_cmd <> "" then begin
      try
        let buf = Buffer.create 100 in
        
        List.iter (fun name -> 
            Printf.bprintf  buf "name:%s\n" name
        ) r.result_names;
        Printf.bprintf buf "size:%s\n" (Int32.to_string r.result_size);
        Printf.bprintf buf "md4:%s\n" (Md4.to_string r.result_md4);
        if r.result_format <> "" then
          Printf.bprintf buf "format:%s\n" r.result_format;
        if r.result_type <> "" then
          Printf.bprintf buf "type:%s\n" r.result_type;
        List.iter (fun tag ->
            match tag.tag_value with
              String s ->
                Printf.bprintf buf "string_tag:%s:%s\n" tag.tag_name s
            | Uint32 i | Fint32 i ->
                Printf.bprintf buf "int_tag:%s:%s\n" tag.tag_name 
                  (Int32.to_string i)
            | _ -> ()
        ) r.result_tags;
        Buffer.add_string buf "end result\n";
        
        let s = Buffer.contents buf in
        let t_out =
          match !indexer with
            None ->
              let (t_in, t_out) = TcpClientSocket.exec_command !!local_index_add_cmd [||] 
                  (fun sock ev -> ()) in
              indexer := Some (t_in, t_out);
              TcpClientSocket.set_closer t_out (fun _ _ ->
                  match !indexer with
                    None -> ()
                  | Some (t_in_old, t_out_old) ->
                      if t_out_old == t_out then
                        indexer := None);
              TcpClientSocket.set_closer t_out (fun _ _ ->
                  match !indexer with
                    None -> ()
                  | Some (t_in_old, t_out_old) ->
                      if t_out_old == t_out then
                        indexer := None);
              t_out
          | Some (t_in, t_out) -> t_out
        in
        TcpClientSocket.write_string t_out s
      with e ->
          Printf.printf "Exception %s while starting local_index_add"
            (Printexc.to_string e); print_newline ()
    
    end
  
  
let index_result_no_filter r =
  try
(*    Printf.printf "RESULT %s" (Md4.to_string r.result_md4);
    print_newline (); *)
    let doc = Hashtbl.find results r.result_md4 in
    let rr = Indexer.value doc in
    List.iter (fun name ->
        if not (List.mem name rr.result_names) then begin
            rr.result_names <- name :: rr.result_names;
            index_name doc name
          end
    ) r.result_names;
    doc
  with
    _ -> 
      let doc = Indexer.make_doc index r in
      Hashtbl.add results r.result_md4 doc;

      (try add_to_local_index r with _ -> ());
      
      if !!save_file_history then begin
          output_result r;
          flush (history_file_oc ());
        end;

      List.iter (fun name ->
          index_name doc name
      ) r.result_names;      
      List.iter (fun tag ->
          match tag with
          | { tag_name = "Artist"; tag_value = String s } -> 
              index_string doc s artist_bit
          | { tag_name = "Title"; tag_value = String s } -> 
              index_string doc s title_bit
          | { tag_name = "Album"; tag_value = String s } -> 
              index_string doc s album_bit
              (* we could directly use the fields of r *)
          | { tag_name = "format"; tag_value = String s } -> 
              index_string doc s format_bit
          | { tag_name = "type"; tag_value = String s } -> 
              index_string doc s media_bit
          | _ -> ()
      ) r.result_tags;
      doc

let index_result r =
    if not !!use_file_history then r else
    let doc = index_result_no_filter r in
    if Indexer.filtered doc then raise Not_found;
    Indexer.value doc

let add_name r file_name =
  if !!use_file_history then
    try
      let doc = Hashtbl.find results r.result_md4 in
      let rr = Indexer.value doc in
      if r != rr then raise Not_found;
      if not (List.mem file_name r.result_names) then begin
          r.result_names <- file_name :: r.result_names;
          index_name doc file_name
        end
    with _ ->
        r.result_names <- file_name :: r.result_names;
        ignore (index_result_no_filter r)
  else
    r.result_names <- file_name :: r.result_names

  
let merge_result search r =
  try
    let result, old_avail = Hashtbl.find search.search_files r.result_md4
    in
    if result != r then
      List.iter (fun name ->
          if not (List.mem name result.result_names) then begin
              add_name result name;
              result.result_names <- name :: result.result_names
            end
      ) r.result_names
  with _ ->
      try
        let result =  index_result r in      
        Hashtbl.add search.search_files r.result_md4 (result, ref 0);
        search.search_nresults <- search.search_nresults + 1;
        search.search_handler (Result result);
      with _ ->  (* the file was probably filtered *)
          ()

  
let find s = 
  if not !!use_file_history then () else
(*  Indexer.print index; *)
  let req = ref [] in
  let pred = ref (fun _ -> true) in

  let ss = s.search_query in
  
  List.iter (fun s -> 
      List.iter (fun s ->
          req := (s, 0xffffffff) :: !req) (stem s) 
  )  ss.search_words;

  let with_option o f =
    match o with 
      None -> ()
    | Some v -> f v 
  in

  let with_option_bit o bit =
    match o with 
      None -> ()
    | Some s -> 
        List.iter (fun s ->
            req := (s, bit) :: !req) (stem s)
  in
  
  with_option ss.search_minsize (fun size -> 
      let old_pred = !pred in
      pred := (fun doc ->
          let r = Indexer.value doc in
          r.result_size >= size && old_pred doc));
  with_option ss.search_maxsize (fun  size -> 
      let old_pred = !pred in
      pred := (fun doc ->
          let r = Indexer.value doc in
          r.result_size <= size && old_pred doc));
  with_option_bit ss.search_media media_bit;
  with_option_bit ss.search_format format_bit;
  with_option_bit ss.search_title title_bit;
  with_option_bit ss.search_artist artist_bit;
  with_option_bit ss.search_album album_bit;  
  
  let req = !req in
  
  let docs = Indexer.complex_request index req !pred in
(*  Printf.printf "%d results" (List.length docs); print_newline (); *)
  List.iter (fun doc ->
      let r = Indexer.value doc in
      comment_result r;
      merge_result s r
  ) docs


let load_old_history () =
  let ic = open_in "history.dat" in
  try
    while true do
      DownloadGlobals.printf_char '.';
      ignore (index_result_no_filter (input_old_result ic))
    done
  with _ -> close_in ic
  
let init () =
(* load history *)
  if !! save_file_history then
    try
      Printf.printf  "Loading history file ..."; flush stdout;
      let list = ref [] in
      let ic = open_in history_file in
      try
        while true do
          let file = input_result ic in
          list := (Indexer.value (index_result_no_filter file)) :: !list;
        done
      with 
        End_of_file -> 
          Printf.printf "done"; print_newline ();
          close_in ic
      | _ -> (* some error *)
          Printf.printf "Error reading history file"; print_newline ();
          close_in ic;
          Printf.printf "Generating new file"; print_newline ();
          begin try
            List.iter (fun file ->
                output_result file
              ) !list;
              close_history_oc ();
            with e ->            
                Printf.printf "Error %s generating new history file"
                  (Printexc.to_string e);
                print_newline () 
          end
    with _ -> ()
            
let _ =
  Options.option_hook filters (fun _ ->
      try
(*        Printf.printf "CLEAR OLD FILTERS"; print_newline (); *)
        Indexer.clear_filter index;
(*        Printf.printf "SET NEW FILTERS"; print_newline (); *)
        Indexer.filter_words index (stem !!filters)
      with e ->
          Printf.printf "Error %s in set filters" (Printexc.to_string e);
          print_newline ();
  );
  DownloadGlobals.do_at_exit (fun _ ->
      if !!save_file_history then begin
          Printf.printf "Saving history 1"; print_newline (); 
          close_history_oc ();
          
          Hashtbl.iter (fun _ doc ->
              let r = Indexer.value doc in
              output_result r
          ) results;
          close_history_oc ();
(*          
          (try Sys.rename history_file (history_file ^ ".old") with _ -> ());
(try Sys.rename (history_file ^ ".tmp") history_file with _ -> ())
  *)
        end
  )
  
let index_result_no_filter r = 
  let r = Indexer.value (index_result_no_filter r) in
  comment_result r;
  r
  
let index_result r = 
  let r = index_result r in
  comment_result r;
  r
  
let find_names md4 =
  try
    let doc = Hashtbl.find results md4 in
    let r = Indexer.value doc in
    r.result_names
  with _ -> []
      
      
let add_comment md4 comment =
  add_comment md4 comment;
  save_comments ()

let _ =
  BasicSocket.add_timer 10. (fun timer ->
      BasicSocket.reactivate_timer timer;
      match !indexer with
        None -> ()
      | Some (t_in, t_out) ->
          if TcpClientSocket.can_write t_out then begin
              TcpClientSocket.close t_out "timed";
              indexer := None;
            end)