open Mftp
open Options
open DownloadTypes
open DownloadOptions
open Gui_types

let input_result ic = 
  let hresult = input_value ic in
  let file = {
      result_names = hresult.hresult_names;
      result_md4 = hresult.hresult_md4;
      result_size = hresult.hresult_size;
      result_tags = hresult.hresult_tags;
      result_filtered_out = 0;
    }  in
  file
    
let output_result oc result =
  output_value oc 
  {
      hresult_names = result.result_names;
      hresult_md4 = result.result_md4;
      hresult_size = result.result_size;
      hresult_tags = result.result_tags;
    }
  
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
  
let history_file = "history.dat"
let history_file_oc = ref None
  
let clear () =
  Indexer.clear index;
  Hashtbl.clear results;
  match !history_file_oc with
    None -> ()
  | Some oc -> 
      close_out oc;
      history_file_oc := Some (open_out history_file) (* truncate !! *)

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
      output_result (history_file_oc ()) r;
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
      
let find s = 
  if not !!use_file_history then () else
(*  Indexer.print index; *)
  let req = ref [] in
  let pred = ref (fun _ -> true) in
  
  List.iter (fun s -> 
(*      Printf.printf "search for [%s]" s; print_newline (); *)
      List.iter (fun s ->
          req := (s, 0xffffffff) :: !req) (stem s) 
  )  s.search_words;
  
  begin
    match s.search_minsize with
      None -> ()
    | Some size -> 
        let old_pred = !pred in
        pred := (fun doc ->
            let r = Indexer.value doc in
            r.result_size >= size && old_pred doc);
  end;
  
  begin
    match s.search_maxsize with
      None -> ()
    | Some size -> 
        let old_pred = !pred in
        pred := (fun doc ->
            let r = Indexer.value doc in
            r.result_size <= size && old_pred doc);
  end;
  
  begin
    match s.search_media with
      None -> ()
    | Some s -> 
        List.iter (fun s ->
            req := (s, media_bit) :: !req
        ) (stem s)
  end;
  
  begin
    match s.search_media with
      None -> ()
    | Some s -> 
        List.iter (fun s ->
            req := (s, media_bit) :: !req
        ) (stem s)
  end;

  begin
    match s.search_format with
      None -> ()
    | Some s -> 
        List.iter (fun s ->
            req := (s, format_bit) :: !req
        ) (stem s)
  end;

  begin
    match s.search_title with
      None -> ()
    | Some s -> 
        List.iter (fun s ->
            req := (s, title_bit) :: !req
        ) (stem s)
  end;

  begin
    match s.search_artist with
      None -> ()
    | Some s -> 
        List.iter (fun s ->
            req := (s, artist_bit) :: !req
        ) (stem s)
  end;

  begin
    match s.search_album with
      None -> ()
    | Some s -> 
        List.iter (fun s ->
            req := (s, album_bit) :: !req
        ) (stem s)
  end;
  
  
  let req = !req in
  
  let docs = Indexer.complex_request index req !pred in
(*  Printf.printf "%d results" (List.length docs); print_newline (); *)
  List.iter (fun doc ->
      let r = Indexer.value doc in
      s.search_handler (Result r);
      Hashtbl.add s.search_files r.result_md4 (r, ref 0);
      s.search_nresults <- s.search_nresults + 1
  ) docs

  
let init () =
(* load history *)
  if !! save_file_history then
    try
      let list = ref [] in
      let ic = open_in history_file in
      try
        while true do
          let file = input_result ic in
          list := (Indexer.value (index_result_no_filter file)) :: !list;
        done
      with 
        End_of_file -> close_in ic
      | _ -> (* some error *)
          Printf.printf "Error reading history file"; print_newline ();
          close_in ic;
          Printf.printf "Generating new file"; print_newline ();
          begin try
            let oc = open_out history_file in
            List.iter (fun file ->
                output_result oc file
            ) !list;
            close_out oc
            with e ->            
                Printf.printf "Error %s generating new history file"
                  (Printexc.to_string e);
                print_newline () 
          end
    with _ -> ()
        
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
(*          Printf.printf "Saving history 1"; print_newline (); *)
          close_out (history_file_oc ());
          
(*          Printf.printf "Saving history 2"; print_newline (); *)
          let oc = open_out (history_file ^ ".tmp") in
(*          Printf.printf "Saving history 3"; print_newline (); *)
          Hashtbl.iter (fun _ doc ->
              let r = Indexer.value doc in
(*              Printf.printf "Saving history 4"; print_newline (); *)
              output_result oc r
          ) results;
(*          Printf.printf "Saving history 5"; print_newline (); *)
          close_out oc;
          
(*          Printf.printf "Saving history 6"; print_newline (); *)
          (try Sys.rename history_file (history_file ^ ".old") with _ -> ());
(*          Printf.printf "Saving history 7"; print_newline (); *)
          (try Sys.rename (history_file ^ ".tmp") history_file with _ -> ())
        end
  )
  
let index_result_no_filter r = Indexer.value (index_result_no_filter r)