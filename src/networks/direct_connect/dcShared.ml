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
open Int64ops
open Printf2
open Md4

open CommonOptions
open Options
open CommonTypes

open DcTypes
open DcGlobals

let log_prefix = "[dcSh]"

let lprintf_nl fmt =
  lprintf_nl2 log_prefix fmt

(* Share files tabs *)
let rec buf_tabs buf n =
  if n > 0 then begin
    Buffer.add_char buf '\t';
    buf_tabs buf (n-1)
  end

(* Create list of shared files *)
let make_mylist () =
  let buf = Buffer.create 1000 in
  let rec iter ntabs node =
    let dirname = node.shared_dirname in
    let ntabs =
      if dirname = "" then ntabs else begin
        buf_tabs buf ntabs;
        let dir = dirname in
        Printf.bprintf buf "%s\r\n" (DcProtocol.utf_to_dc dir);
        ntabs+1
      end
    in
    List.iter (fun dcsh ->
      buf_tabs buf ntabs;
      let fname = Filename2.basename dcsh.dc_shared_codedname in
      Printf.bprintf buf "%s|%Ld\r\n" (DcProtocol.utf_to_dc fname) dcsh.dc_shared_size
    ) node.shared_files;
    List.iter (fun (_, node) ->
        iter ntabs node
    ) node.shared_dirs
  in
  iter 0 dc_shared_tree;
  Buffer.contents buf

(* Create mylist of shared files in xml-format *)
let make_xml_mylist root = 
  let buf = Buffer.create 1000 in
  Printf.bprintf buf "<?xml version=\"1.0\" encoding=\"utf-8\" standalone=\"yes\"?>\r\n";
  Printf.bprintf buf "<FileListing Version=\"1\" CID=\"1,0,2,3,4,5,6\" Base=\"/\" Generator=\"MLDC-%s\">\r\n" (Xml.escape Autoconf.current_version);
  let rec iter ntabs node =
    buf_tabs buf ntabs;
    Printf.bprintf buf "<Directory Name=\"%s\">\r\n" (Xml.escape node.shared_dirname);
    List.iter (fun dcsh ->
      buf_tabs buf (ntabs + 1);
      let fname = Filename2.basename dcsh.dc_shared_codedname in
      Printf.bprintf buf "<File Name=\"%s\" Size=\"%Ld\" TTH=\"%s\"/>\r\n" (Xml.escape fname)
        dcsh.dc_shared_size (Xml.escape dcsh.dc_shared_tiger_root)
    ) node.shared_files;
    List.iter (fun (_, node) -> iter (ntabs+1) node) node.shared_dirs;
    buf_tabs buf ntabs;
    Printf.bprintf buf "</Directory>\r\n"
  in
  if root.shared_dirname = "" then 
    List.iter (fun (_,node) -> iter 0 node) root.shared_dirs
  else 
    iter 0 root;
  Printf.bprintf buf "</FileListing>\r\n";
  buf

let file_to_che3_to_string filename = 
  let buf = Buffer.create 8192 in
  let file_fd = Unix32.create_ro filename in
  let flen = Unix32.getsize64 file_fd in
  let slen = 4096 in
  (try
    let rec read pos =
      let rlen = int64_min_int (flen -- pos) slen in
      let npos = Int64.add pos (Int64.of_int rlen) in
      let str = Bytes.create slen in
      Unix32.read file_fd pos str 0 rlen;
      Buffer.add_bytes buf str;
      if npos < flen then read npos
    in
    read Int64.zero;
    Unix32.close file_fd;
    Che3.decompress (Buffer.contents buf)
  with e ->
    if !verbose_unexpected_messages then
      lprintf_nl "Exception (%s) in (file_to_che3_to_string)" (Printexc2.to_string e);
    raise e )

(* Compress string to Che3 and write to file *)
let string_to_che3_to_file str filename =
  (try
    let s = Che3.compress str in
    let wlen = 4096 in
    (*let str = String.create slen in*)
    let slen = String.length s in
    let file_fd = Unix32.create_rw filename in
    let rec write pos =
      let len =
        if (pos + wlen) > slen then slen-pos
        else wlen
      in
      let npos = pos + len in
      Unix32.write file_fd (Int64.of_int pos) s pos len;
      if npos < slen then write npos
    in 
    write 0;
    Unix32.close file_fd;
    ()
  with e ->
    if !verbose_unexpected_messages then
      lprintf_nl "Exception (%s) in (string_to_che3_to_file)" (Printexc2.to_string e) )

(* Open bz2 file and return opened data in buffer *)
let file_to_bz2_to_buffer filename = 
  let buf = Buffer.create 8192 in
  let ic = Bzip2.open_in filename in
  (try
    (*let rec getchar () =
      ignore (Bzip2.input_char ic);
      incr count;
      getchar ()
    in getchar ();*)
    let rec decompress () =
      let str = Bytes.create 4096 in 
      let n = Bzip2.input ic str 0 (Bytes.length str) in
      if n = 0 then ()
      else begin 
        (*let ss = (String.sub str 0 n) in*)
        Buffer.add_string buf (Bytes.sub_string str 0 n);
        (*lprintf_nl "(%s)" ss;*)
        decompress ()
      end
    in
    decompress ();
    Bzip2.close_in ic; 
    (*lprintf_nl "Size of bz2 buffer: %d" (Buffer.length buf);*)
    buf
  with
  | e ->
    if !verbose_unexpected_messages then
      lprintf_nl "Exception (%s) in (file_to_bz2_to_buffer)" (Printexc2.to_string e);
    Bzip2.close_in ic;
    raise e )

(* Compress buffer to bz2 and save to file *)    
let buffer_to_bz2_to_file buf filename =
  (try 
    let slen = 4096 in
    (*let str = String.create slen in*)
    let blen = Buffer.length buf in
    let oc = Bzip2.open_out filename in
    let rec compress pos =
      let len =
        if (pos + slen) > blen then blen-pos
        else slen
      in
      let npos = pos + len in
      let str = Bytes.unsafe_of_string @@ Buffer.sub buf pos len in      
      Bzip2.output oc str 0 len;
      if npos < blen then compress npos 
    in compress 0;
    Bzip2.close_out oc;
    ()
  with e ->
    if !verbose_unexpected_messages then
      lprintf_nl "Exception (%s) in (buffer_to_bz2_to_file)" (Printexc2.to_string e) )

(* Create xml and mylist filelist *)
let create_filelist () =
  buffer_to_bz2_to_file (make_xml_mylist dc_shared_tree) (Filename.concat directconnect_directory mylistxmlbz2);
  if !verbose_upload then lprintf_nl "Created mylist.xml file";
  string_to_che3_to_file (make_mylist () ) (Filename.concat directconnect_directory mylist);
  if !verbose_upload then lprintf_nl "Created mylist file";
  ()

let find_dir_exn name =
  let path = String2.split_simplify name '/' in
  let rec follow path node =
    match path with
    | [] -> node
    | x::xs -> follow xs (List.assoc x node.shared_dirs)
  in
  follow path dc_shared_tree

(*let dc_share_file dcsh = ()*)
(*  let magic =
    match Magic.M.magic_fileinfo dcsh.dc_shared_fullname false with
      None -> None
    | Some magic -> Some (HashMagic.merge files_magic magic)
  in *)
  (*let fd = Unix32.create_ro dcsh.dc_shared_fullname in
  let info, index = CommonUploads.new_info dcsh.dc_shared_fullname dcsh.dc_shared_size in *)
(*
  let rec impl = {
    impl_shared_update = 1;
    impl_shared_fullname = dcsh.dc_shared_fullname;
    impl_shared_codedname = dcsh.dc_shared_codedname;
    impl_shared_size = dcsh.dc_shared_size;
    impl_shared_id = Md4.null;
    impl_shared_num = 0;
    impl_shared_uploaded = Int64.zero;
    impl_shared_ops = shared_ops;
    impl_shared_val = 0;
    impl_shared_requests = 0;
    impl_shared_magic = magic;
  } 
  and sh = {
    shared_info = index;
    shared_codedname = dcsh.dc_shared_codedname;
    shared_fd = fd;
    shared_format = CommonMultimedia.get_info dcsh.dc_shared_fullname;
    shared_impl = impl;
    shared_uids_wanted = [];
  } in
  update_shared_num impl *)
  (*Hashtbl.add shareds_by_id info.shared_id sh;*)
  (*List.iter (fun uid -> add_by_uid uid sh) info.shared_uids;*)
  (*SharedFilesIndex.add sh.shared_info;*)
(*  add_shared_file shared_tree sh (String2.split dcsh.dc_shared_codedname '/');
  shared_counter := !shared_counter ++ dcsh.dc_shared_size *)

(* Compute (at most) one TigerTree chunk from unhashed shared files *)
let dc_check_hashed_files () =
  let module M = CommonHasher in
    match !dc_tiger_computing, !dc_files_to_hash with
    | _, [] | Some _, _ -> ()
    | None, dcsh :: files ->
      try
          if not (Sys.file_exists dcsh.dc_shared_fullname) then raise Not_found;
          if Unix32.getsize dcsh.dc_shared_fullname <> dcsh.dc_shared_size then
            raise (Wrong_file_size ((Unix32.getsize dcsh.dc_shared_fullname), dcsh.dc_shared_size));

          dc_tiger_computing := Some dcsh;
          let end_pos = dcsh.dc_shared_pos ++ CommonUploads.tiger_block_size in
          let end_pos = min end_pos dcsh.dc_shared_size in
          let len = end_pos -- dcsh.dc_shared_pos in
          (*lprintf_nl "Hashing chunk %d: %Ld-%Ld (%Ld) of %s"
            dcsh.dc_shared_chunks dcsh.dc_shared_pos end_pos len dcsh.dc_shared_fullname;*)

          M.compute_tiger dcsh.dc_shared_fullname dcsh.dc_shared_pos len
            (fun job ->
              if job.M.job_error then begin
                lprintf_nl "Error prevent hashing/sharing %s" dcsh.dc_shared_fullname;
                dc_files_to_hash := files;
              end else begin
                let new_tiger = job.M.job_result in
                (*lprintf_nl "  (%s)" (TigerTree.to_string new_tiger);*)
                dcsh.dc_shared_tiger_list <- new_tiger :: dcsh.dc_shared_tiger_list;
                dcsh.dc_shared_pos <- end_pos;
                dcsh.dc_shared_chunks <- dcsh.dc_shared_chunks + 1;

                if dcsh.dc_shared_chunks = dc_get_nchunks dcsh.dc_shared_size then begin
                  let array = Array.of_list (List.rev dcsh.dc_shared_tiger_list) in
                  let root = TigerTree.to_string (CommonUploads.tiger_of_array array) in
                  (*dcsh.dc_shared_tiger_array <- array;*)
                  dcsh.dc_shared_tiger_list <- [];
                  dcsh.dc_shared_tiger_root <- root;  
                  (*if !verbose_upload then*) lprintf_nl "New shared file (%s) (%s)" root dcsh.dc_shared_fullname;
                  dc_files_to_hash := files;
                  Hashtbl.add dc_shared_files_by_hash root dcsh;
                  DcComplexOptions.dc_saved_shared_files =:= dcsh :: !!DcComplexOptions.dc_saved_shared_files;
                end
              end;
              dc_tiger_computing := None
          )
        with
        | Wrong_file_size (real,computed) ->
            dc_files_to_hash := files;
            if !verbose_upload || !verbose_unexpected_messages then
              lprintf_nl "Computed filesize %Ld does not match physical filesize %Ld, %s not shared"
            computed real dcsh.dc_shared_fullname
        | e ->
            dc_files_to_hash := files;
            dc_tiger_computing := None;
            if !verbose_upload || !verbose_unexpected_messages then
              lprintf_nl "Exception %s prevents sharing of %s"
                (Printexc2.to_string e) dcsh.dc_shared_fullname

let dc_updatesharesize () =
  let dc_sharesize = ref Int64.zero in
  Hashtbl.iter (fun _ dcsh ->
    dc_sharesize := !dc_sharesize ++ dcsh.dc_shared_size) dc_shared_files_by_codedname;
  !dc_sharesize

let () =
  network.op_network_share <- (fun fullname codedname size -> (* this is called once/60s with all shared files *)
    (* file path in DC network should use '/' as separator, convert local path accordingly *)
    let codedname =
      match Filename2.slash with
      | '/' -> codedname
      | c -> String2.replace_char codedname c '/'
    in
    (try
      let dcsh = Hashtbl.find dc_shared_files_by_fullname fullname in
      if (dcsh.dc_shared_size = size) then begin            (* if size is correct... *)
        if dcsh.dc_shared_codedname <> codedname then begin (* if codedname is different *) 
          (*lprintf_nl "  Changing codedname (%s) to (%s)" dcsh.dc_shared_codedname codedname;*)
          let old_cname = dcsh.dc_shared_codedname in
          Hashtbl.remove dc_shared_files_by_codedname old_cname;
          dcsh.dc_shared_codedname <- codedname;
          Hashtbl.add dc_shared_files_by_codedname codedname dcsh;
          if !verbose_upload then lprintf_nl "Changed codedname from (%s) to (%s)" old_cname codedname;
        end
      end else raise Not_found                              (* create new shared *)
    with _ ->
      (* if file is moved it is recalculated *)
      let dcsh = {
        dc_shared_fullname = fullname;
        dc_shared_codedname = codedname;
        dc_shared_searchname = String.lowercase (List.nth (String2.splitn codedname '/' 1) 1);
        dc_shared_size = size;
        dc_shared_tiger_root = empty_string;
        (*dc_shared_tiger_array = [||];*)
        dc_shared_tiger_list = [];
        dc_shared_pos = Int64.zero;
        dc_shared_chunks = 0;
      } in
      (*lprintf_nl "New shared file (%s)" codedname;*)
      Hashtbl.add dc_shared_files_by_fullname fullname dcsh;
      Hashtbl.add dc_shared_files_by_codedname codedname dcsh;
      dc_add_shared_file dc_shared_tree dcsh (String2.split codedname '/');
      dc_files_to_hash := dcsh :: !dc_files_to_hash;
      ()
    );
    (*ignore (CommonUploads.add_shared fullname codedname size)*)
  )
