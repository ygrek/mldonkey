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
open CommonShared
open Printf2
open CommonClient
open CommonComplexOptions
open CommonTypes
open CommonFile
open Options
open BasicSocket
open TcpBufferedSocket

open GuiTypes
open CommonGlobals
open CommonOptions

  
    
module M = struct    
let shared_files_ini = create_options_file (
    Filename.concat file_basedir "files_shared.ini")


module SharedFileOption = struct
    
    let value_to_shinfo v =
      match v with
        Options.Module assocs ->
          let get_value name conv = conv (List.assoc name assocs) in
          let get_value_nil name conv = 
            try conv (List.assoc name assocs) with _ -> []
          in
          
          let sh_md4s = try
              value_to_list (fun v ->
                  Md4.of_string (value_to_string v)) (List.assoc "md4s" assocs)
            with _ -> []
          in
          let sh_tiger = try
              value_to_list (fun v ->
                  TigerTree.of_string (value_to_string v)) (List.assoc "tiger" assocs)
            with _ -> []
          in
          let sh_size = try
              value_to_int64 (List.assoc "size" assocs) 
            with _ -> failwith "Bad shared file size"
          in
          let sh_name = try
              value_to_filename (List.assoc "name" assocs)
            with _ -> failwith "Bad shared file name"
          in
          let sh_mtime = try
              value_to_float (List.assoc "mtime" assocs)
            with _ -> failwith "Bad shared file mtime"
          in
          let sh_uids = try
              value_to_list (fun v ->
                  uid_of_string (value_to_string v)  
              ) (List.assoc "hashes" assocs)
            with _ -> 
                lprintf "[WARNING]: Could not load hash for %s\n"
                  sh_name;
                []
          in
          { 
            sh_name = sh_name; 
            sh_mtime = sh_mtime;
            sh_size = sh_size; 
            sh_md4s = Array.of_list sh_md4s;
            sh_tiger = Array.of_list sh_tiger;
            sh_uids = sh_uids;
          }
          
      | _ -> failwith "Options: not a shared file info option"
          
    let shinfo_to_value sh =
      Options.Module [
        "name", filename_to_value sh.sh_name;
        "md4s", list_to_value "Shared Md4" (fun md4 ->
            string_to_value (Md4.to_string md4)) (Array.to_list sh.sh_md4s);
        "tiger", list_to_value "Shared Tiger" (fun md4 ->
            string_to_value (TigerTree.to_string md4)) (Array.to_list sh.sh_tiger);
        "mtime", float_to_value sh.sh_mtime;
        "size", int64_to_value sh.sh_size;
        "hashes", list_to_value "Hashes" (fun uid ->
            string_to_value (string_of_uid uid)
        ) sh.sh_uids;
      ]
    
    
    let t = define_option_class "SharedFile" value_to_shinfo shinfo_to_value
  end
  
let known_shared_files = define_option shared_files_ini 
    ["shared_files"] "" 
    (list_option SharedFileOption.t) []

    
let shared_files_info = (Hashtbl.create 127 :
    (string, shared_file_info) Hashtbl.t)

let find_shared_info fullname size =
  let s = Hashtbl.find shared_files_info fullname in
  let mtime = Unix32.mtime64 fullname in
  if s.sh_mtime = mtime && s.sh_size = size then begin
      if !verbose_share then begin
          lprintf "USING OLD MD4s for %s" fullname;
          lprint_newline (); 
        end;
      s
    end else begin
      if !verbose_share then begin                
          lprintf "Shared file %s has been modified" fullname;
          lprint_newline ();
        end;
      Hashtbl.remove shared_files_info fullname;
      known_shared_files =:= List2.removeq s !!known_shared_files;
      raise Not_found
    end

let put_shared_info name size md4s mtime uids =
  
  let s = {
      sh_name = name;
      sh_size = size;
      sh_md4s = Array.of_list md4s;
      sh_tiger = [||];
      sh_mtime = mtime (* Unix32.mtime64 name *);
      sh_uids = uids;
    } in
  lprintf "NEW SHARED FILE %s\n" name; 
  Hashtbl.add shared_files_info name s;
  known_shared_files =:= s :: !!known_shared_files;
  s      
  
let new_shared_info name size md4s mtime uids =
  try
    let s = find_shared_info name size in
    if md4s <> [] then
      s.sh_md4s <- Array.of_list md4s;
    List.iter (fun uid ->
        if not (List.mem uid s.sh_uids) then
          s.sh_uids <- uid :: s.sh_uids
    ) uids;
    s
  with _ ->
      put_shared_info name size md4s mtime uids
      
end

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
    shared_impl : shared_file shared_impl;
    shared_info : shared_file_info;
    mutable shared_uids_wanted : 
    (file_uid_id * (shared_file -> file_uid -> unit)) list;
  }

and shared_tree =
  { 
    shared_dirname : string;
    mutable shared_files : shared_file list;
    mutable shared_dirs : (string * shared_tree) list;
  }

  
let network = CommonNetwork.new_network "MultiNet"
    (fun _ -> "")
    (fun _ -> "")

let _ = 
  network.op_network_connected <- (fun _ -> false);
  network.op_network_is_enabled <- (fun _ -> raise IgnoreNetwork);
  network.op_network_info <- (fun _ -> raise Not_found)
  
let (shared_ops : shared_file CommonShared.shared_ops) = 
  CommonShared.new_shared_ops network
  
(*******************************************************************

  
                    HASHES COMPUTATION

  
*******************************************************************)
  
  
let waiting_shared_files = ref []

(* The shared files indexed by the strings (lowercase), corresponding to
their uids *)
let shareds_by_uid = Hashtbl.create 13
  
let current_job = ref None

let ed2k_block_size = Int64.of_int 9728000
let tiger_block_size = Int64.of_int (1024 * 1024)
  
let ask_for_uid sh uid f =
  sh.shared_uids_wanted <- (uid,f) :: sh.shared_uids_wanted;
  waiting_shared_files := sh :: !waiting_shared_files

let md4_of_list md4s =
  let len = List.length md4s in
  let s = String.create (len * 16) in
  let rec iter list i =
    match list with
      [] -> ()
    | md4 :: tail ->
        let md4 = Md4.direct_to_string md4 in
        String.blit md4 0 s i 16;
        iter tail (i+16)
  in
  iter md4s 0;
  Md4.string s

let rec tiger_of_array array pos block =
  if block = 1 then
    array.(pos)
  else
  let len = Array.length array in
  if pos + block / 2 >= len then
    tiger_of_array array pos (block/2)
  else
  let d1 = tiger_of_array array pos (block/2) in
  let d2 = tiger_of_array array (pos+block/2) (block/2) in
  let s = String.create (1 + Tiger.length * 2) in
  s.[0] <- '\001';
  String.blit (TigerTree.direct_to_string d1) 0 s 1 Tiger.length;
  String.blit (TigerTree.direct_to_string d2) 0 s (1+Tiger.length) Tiger.length;
  let t = Tiger.string s in
  let t = TigerTree.direct_of_string (Tiger.direct_to_string t) in
  t
  
let rec tiger_max_block_size block len =
  if block >= len then block
  else tiger_max_block_size (block*2) len
  
let tiger_of_array array =
  tiger_of_array array 0 (tiger_max_block_size  1 (Array.length array))

let rec tiger_pos nblocks =
  if nblocks < 2 then 0 else
  let half = nblocks / 2 in
  let acc = nblocks - 2 * half in
  let half = half + acc in
  half + tiger_pos half

let rec tiger_pos2 nblocks =
  if nblocks < 2 then 0, [] else
  let half = nblocks / 2 in
  let acc = nblocks - 2 * half in
  let half = half + acc in
  let pos, list = tiger_pos2 half in
  let list = (nblocks, pos) :: list in
  let pos = half + pos in
  pos, list

let tiger_node d1 d2 = 
  let s = String.create (1 + Tiger.length * 2) in
  s.[0] <- '\001';
  String.blit (TigerTree.direct_to_string d1) 0 s 1 Tiger.length;
  String.blit (TigerTree.direct_to_string d2) 0 s (1+Tiger.length) Tiger.length;
  let t = Tiger.string s in
  let t = TigerTree.direct_of_string (Tiger.direct_to_string t) in
  t
  
let rec tiger_tree s array pos block =
  if block = 1 then
    array.(pos)
  else
  let len = Array.length array in
  if pos + block / 2 >= len then
    tiger_tree s array pos (block/2)
  else
  let d1 = tiger_tree s array pos (block/2) in
  let d2 = tiger_tree s array (pos+block/2) (block/2) in
  tiger_node d1 d2
  
let rec fill_tiger_tree s list =
  match list with
    [] -> ()
  | (nblocks, pos) :: tail ->
(* nblocks: the number of blocks in the next level
   pos: the position of the blocks in to be created
*)
      let half = nblocks / 2 in
      let acc = nblocks - 2 * half in

      let next_pos = pos + half + acc in
      for i = 0 to half - 1 do
        let d1 = s.(next_pos+2*i) in
        let d2 = s.(next_pos+2*i+1) in
        s.(pos+i) <- tiger_node d1 d2;
      done;
      if acc = 1 then s.(pos+half) <- s.(next_pos+2*half);
      fill_tiger_tree s tail

let flatten_tiger_array array = 
  let len = Array.length array in
  let s = String.create ( len * TigerTree.length) in  
  for i = 0 to len - 1 do
    String.blit (TigerTree.direct_to_string array.(i)) 0
      s (i * TigerTree.length) TigerTree.length
  done;
  s

let unflatten_tiger_array s = 
  let len = String.length s / TigerTree.length in
  let array = Array.create len TigerTree.null in  
  for i = 0 to len - 1 do
    array.(i) <- TigerTree.direct_of_string 
      (String.sub s (i * TigerTree.length) TigerTree.length)
  done;
  array
  
let make_tiger_tree array =
  let len = Array.length array in
  let pos, list = tiger_pos2 len in
  let s = Array.create (pos + len) TigerTree.null in
  for i = 0 to len - 1 do
    s.(pos+i) <- array.(i)
  done;
  fill_tiger_tree s list;
  flatten_tiger_array s
  
let rec start_job_for sh (wanted_id, handler) = 
  try
    List.iter (fun id ->
        match wanted_id,id with
        | SHA1, Sha1 _
        | ED2K, Ed2k _
        | MD5, Md5 _ 
        | MD5EXT, Md5Ext _
        | BITPRINT, Bitprint _
        | TIGER, TigerTree _
          -> (try handler sh id with _ -> ()); raise Exit
        | _ -> ()
    ) sh.shared_info.sh_uids;
    
    match wanted_id with
      SHA1 -> 
        CommonHasher.compute_sha1 (Unix32.filename sh.shared_fd)
        zero sh.shared_size (fun job ->
            if job.CommonHasher.job_error then begin
                lprintf "Error during hashing of %s\n" sh.shared_fullname; 
                current_job := None;
              end else
              begin
                let sha1 = job.CommonHasher.job_result in
                let uid = uid_of_uid (Sha1 ("", sha1)) in
                let urn = string_of_uid uid in
                sh.shared_info.sh_uids <- uid :: sh.shared_info.sh_uids;
                Hashtbl.add shareds_by_uid (String.lowercase urn) sh;
                start_job_for sh (wanted_id, handler)  
              end
        );
    | BITPRINT ->
        let sha1 = ref None in
        let tiger = ref None in
        List.iter (fun id ->
            match id with
            | Sha1 (_, s) -> sha1 := Some s
            | TigerTree (_, s) -> tiger := Some s
            | _ -> ()
        ) sh.shared_info.sh_uids;
        begin
          match !sha1, !tiger with
            Some sha1, Some tiger ->
              let uid = uid_of_uid (Bitprint ("", sha1, tiger)) in
              let urn = string_of_uid uid in
              sh.shared_info.sh_uids <- uid :: sh.shared_info.sh_uids;
              Hashtbl.add shareds_by_uid (String.lowercase urn) sh;
              start_job_for sh (wanted_id, handler)
          
          | _ -> ()
(*
(* Not enough information to compute the bitprint. Ask for the corresponding
information. What happens if there is an error during SHA1 or TIGER
computation ??? *)
              ask_for_uid sh BITPRINT handler;
              (match !sha1 with
                  None ->  ask_for_uid sh SHA1 (fun _ _ -> ())
                | _ -> ());
              (match !tiger with
                  None ->  ask_for_uid sh TIGER (fun _ _ -> ())
| _ -> ());
  *)
        end
    
    | MD5EXT ->
        let md5ext = Md5Ext.file sh.shared_fullname in
        let uid = uid_of_uid (Md5Ext ("", md5ext)) in
        let urn = string_of_uid uid in
        sh.shared_info.sh_uids <- uid :: sh.shared_info.sh_uids;
        Hashtbl.add shareds_by_uid (String.lowercase urn) sh;
        start_job_for sh (wanted_id, handler)  
    
    | ED2K ->
        let size = sh.shared_size in
        let chunk_size = ed2k_block_size  in
        let nhashes = Int64.to_int (size // chunk_size) + 1 in
        let rec iter pos hashes =
          if pos < size then
            CommonHasher.compute_md4 sh.shared_fullname
              pos (min (size -- pos) chunk_size)
            (fun job ->
                iter (pos ++ chunk_size) (job.CommonHasher.job_result :: hashes))
          else
          let list = List.rev hashes in
          let ed2k = md4_of_list list in
          let uid = uid_of_uid (Ed2k ("", ed2k)) in
          let urn = string_of_uid uid in
          sh.shared_info.sh_md4s <- Array.of_list list;
          sh.shared_info.sh_uids <- uid :: sh.shared_info.sh_uids;
          Hashtbl.add shareds_by_uid (String.lowercase urn) sh;
          start_job_for sh (wanted_id, handler)                
        in
        iter zero []
    
    | TIGER -> 
        let size = sh.shared_size in
        let chunk_size = tiger_block_size in
        let nhashes = Int64.to_int (size // chunk_size) + 1 in
        let rec iter pos hashes =
          if pos < size then
            CommonHasher.compute_tiger sh.shared_fullname
              pos (min (size -- pos) chunk_size)
            (fun job ->
                iter (pos ++ chunk_size) (job.CommonHasher.job_result :: hashes))
          else
          let array = Array.of_list (List.rev hashes) in
          let tiger = tiger_of_array array in
          let uid = uid_of_uid (TigerTree ("", tiger)) in
          let urn = string_of_uid uid in
          sh.shared_info.sh_tiger <- array;
          sh.shared_info.sh_uids <- uid :: sh.shared_info.sh_uids;
          Hashtbl.add shareds_by_uid (String.lowercase urn) sh;
          start_job_for sh (wanted_id, handler)                
        in
        iter zero []
    
    | _ -> raise Exit
  
  with Exit -> 
      current_job := None
      
  
let shared_files_timer _ =
  match !current_job with
  | Some _ -> ()
  | None ->
      match !waiting_shared_files with
        [] -> ()
      | sh :: tail ->
          match sh.shared_uids_wanted with
            [] ->  waiting_shared_files := tail;
          | uid :: tail ->
              sh.shared_uids_wanted <- tail;
              current_job := Some sh;
              start_job_for sh uid
              
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
(*      lprintf "ADD [%s] in index" s; lprint_newline (); *)
      DocIndexer.add  index s doc fields
  ) words 
  
let index_name doc name = 
  index_string doc name 1 (* general search have field 1 *)

(***********************************************)

let bit_of_field field =
  match field with
  | Field_Filename -> name_bit
  | Field_Artist -> artist_bit
  | Field_Title -> title_bit
  | Field_Album -> album_bit
  | Field_Format -> format_bit 
  | Field_Type -> media_bit
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
          | Field_Size -> s.shared_size >= minval
          | _ -> true)
  | QHasMaxVal (field, maxval) -> 
      Indexer.Predicate (fun s -> 
          match field with 
          | Field_Size -> s.shared_size <= maxval
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
  try
    Hashtbl.find shared_files codedname
  with Not_found ->
      incr shareds_counter;
      
      let rec impl = {
          impl_shared_update = 1;
          impl_shared_fullname = full_name;
          impl_shared_codedname = codedname;
          impl_shared_size = size;
          impl_shared_id = Md4.random ();
          impl_shared_num = 0;
          impl_shared_uploaded = Int64.zero;
          impl_shared_ops = shared_ops;
          impl_shared_val = sh;
          impl_shared_requests = 0;
        } 
      and sh = {
          shared_fullname = full_name;
          shared_codedname = codedname;
          shared_size = size;
          shared_id = !shareds_counter;
          shared_fd=  Unix32.create full_name [Unix.O_RDONLY] 0o444;
          shared_format = CommonMultimedia.get_info full_name;
          shared_impl = impl;
          shared_uids_wanted = [];
          shared_info = M.new_shared_info full_name size [] 
            (Unix32.mtime64 full_name) [];
        } in
      
      update_shared_num impl;
      
      lprintf "FILE ADDED: %s" codedname; lprint_newline ();
      Hashtbl.add table !shareds_counter sh;
      Hashtbl.add shared_files codedname sh;
      Indexer.index_file sh;
      add_shared_file shared_tree sh (String2.split codedname '/');
      shared_counter := Int64.add !shared_counter size;
      lprintf "Total shared : %s" (Int64.to_string !shared_counter);
      lprint_newline () ;
      sh
      
let query q = Indexer.find (Indexer.query_to_query q)
  
let find_by_name name = Hashtbl.find shared_files name
  
let find_by_num num = Hashtbl.find table num

(**********************************************************************


                     UPLOAD SCHEDULER


***********************************************************************)

let client_is_connected c = is_connected (client_state c)
  

(* Move the uploaders and nu commands to driver *)







let upload_clients = (Fifo.create () : client Fifo.t)


let (pending_slots_map : client Intmap.t ref) = ref Intmap.empty
(* let (pending_slots_fifo : int Fifo.t)  = Fifo.create () *)

let remaining_bandwidth = ref 0    
let total_bandwidth = ref 0    
let complete_bandwidth = ref 0
let counter = ref 1    
let sent_bytes = Array.create 10 0
let has_upload = ref 0
let upload_credit = ref 0


let can_write_len sock len =
  can_write_len sock len && 
  (let upload_rate = 
      (if !!max_hard_upload_rate = 0 then 10000 else !!max_hard_upload_rate)
      * 1024 in
    lprintf "upload_rate %d\n" upload_rate;
    not_buffer_more sock (upload_rate * (1 + Fifo.length upload_clients)))

let upload_to_one_client () =
  if !remaining_bandwidth < 10000 then begin
      let c = Fifo.take upload_clients in
      client_can_upload c  !remaining_bandwidth
    end else
  let per_client = 
    let len = Fifo.length upload_clients in
    if len * 10000 < !remaining_bandwidth then
(* Each client in the Fifo can receive 10000 bytes.
Divide the bandwidth between the clients
*)
      (!remaining_bandwidth / 10000 / len) * 10000
    else mini 10000 !remaining_bandwidth in
  let c = Fifo.take upload_clients in
  client_can_upload c per_client

let rec fifo_uploads n =
  if n>0 && !remaining_bandwidth > 0 then
    begin
      upload_to_one_client ();
      fifo_uploads (n-1)
    end

let rec next_uploads () =
(*  lprintf "next_uploads %d\n" !remaining_bandwidth; *)
  let old_remaining_bandwidth = !remaining_bandwidth in
  let len = Fifo.length upload_clients in
  fifo_uploads len;
  if !remaining_bandwidth < old_remaining_bandwidth then
    next_uploads ()

let next_uploads () = 
  sent_bytes.(!counter-1) <- sent_bytes.(!counter-1) - !remaining_bandwidth;
  if !verbose_upload then begin
      lprintf "Left %d\n" !remaining_bandwidth; 
    end;
  complete_bandwidth := !complete_bandwidth + !remaining_bandwidth;
  incr counter;
  if !counter = 11 then begin
      counter := 1;
      total_bandwidth := 
      (if !!max_hard_upload_rate = 0 then 10000 * 1024
        else (maxi (!!max_hard_upload_rate - 1) 1) * 1024 );
      complete_bandwidth := !total_bandwidth;
      if !verbose_upload then begin
          lprintf "Init to %d\n" !total_bandwidth; 
        end;
      remaining_bandwidth := 0          
    end;
  
  let last_sec = ref 0 in
  for i = 0 to 9 do
    last_sec := !last_sec + sent_bytes.(i)
  done;
  
  if !verbose_upload then begin
      lprintf "last sec: %d/%d (left %d)" !last_sec !total_bandwidth
        (!total_bandwidth - !last_sec);
      lprint_newline (); (*
      for i = 0 to 9 do
        lprintf "    last[%d] = %d\n" i  sent_bytes.(i)
      done; *)
      
    end;
  
  remaining_bandwidth := mini (mini (mini 
        (maxi (!remaining_bandwidth + !total_bandwidth / 10) 10000) 
      !total_bandwidth) !complete_bandwidth) 
  (!total_bandwidth - !last_sec);
  complete_bandwidth := !complete_bandwidth - !remaining_bandwidth;
  if !verbose_upload then begin
      lprintf "Remaining %d[%d]\n" !remaining_bandwidth !complete_bandwidth; 
    end;
  sent_bytes.(!counter-1) <- !remaining_bandwidth;
  if !remaining_bandwidth > 0 then 
    next_uploads ()

let reset_upload_timer () = ()
    
let reset_upload_timer _ =
  reset_upload_timer ()
  
let upload_credit_timer _ =
  if !has_upload = 0 then 
    (if !upload_credit < 300 then incr upload_credit)
  else
    decr has_upload
    
let ready_for_upload c =
  Fifo.put upload_clients c
    
let add_pending_slot c =
  if client_has_a_slot c then begin
      lprintf "Avoided inserting an uploader in pending slots!\n";
    end 
  else 
  if not (Intmap.mem (client_num c) !pending_slots_map) then
    begin
(* This is useless since it is the goal of the pending_slots_map 
        else if Fifo.mem pending_slots_fifo (client_num c) then begin
	lprintf "Avoided inserting a client twice in pending slots";
	lprint_newline ()
      end else *)
      pending_slots_map := Intmap.add (client_num c) c !pending_slots_map;
    end
    
let remove_pending_slot c =
  if Intmap.mem (client_num c) !pending_slots_map then
    pending_slots_map := Intmap.remove (client_num c) !pending_slots_map
    
let rec give_a_slot c =
  remove_pending_slot c;
  if not (client_is_connected c) then begin
      find_pending_slot ()
    end
  else begin
      set_client_has_a_slot c true;
      client_enter_upload_queue c
    end
    
and find_pending_slot () =
  try
    let rec iter () =
      let c = Intmap.top !pending_slots_map in
      give_a_slot c
    in
    iter ()
  with _ -> ()

let add_pending_slot c =
  let csh = client_upload c in
  let cdir = shared_dir csh in
  let cprio = ref (shared_prio csh) in
  (* if cdir <> "" then
    lprintf "Testing cdir %s\n" cdir; *)
  Intmap.iter (fun _ c -> 
    let sh = client_upload c in
    if shared_dir sh = cdir then decr cprio
  ) !CommonClient.uploaders;
  (* if cdir <> "" then
    lprintf "Testing cprio %d\n" !cprio; *)
  if !cprio > 0 then begin
    remove_pending_slot c;
    if client_is_connected c then begin
      set_client_has_a_slot c true;
      client_enter_upload_queue c
    end
  end else
    add_pending_slot c

let static_refill_upload_slots () =
  let len = Intmap.length !CommonClient.uploaders in
  if len < !!max_upload_slots then find_pending_slot ()

(* Since dynamic slots allocation is based on feedback, it should not 
 * allocate new slots too fast, since connections need some time to reach 
 * a stable state. 
 * To compensate for that slow pace, slots are allocated quadratically
 * as long as the link is not saturated. 
 *)

let not_saturated_count = ref 0
let allocation_cluster = ref 1
  
let dynamic_refill_upload_slots () =
  let reset_state () =
    not_saturated_count := 0;
    allocation_cluster := 1 in

  let open_slots n =
    let i = ref n in
    if !verbose_upload then begin
      lprintf "try to allocate %d more slots" n;
      lprint_newline ()
    end;
    while !i > 0 do
      find_pending_slot ();
      decr i
    done in

  let slot_bw = 3072 in
  let min_upload_slots = 3 in
(*  let estimated_capacity = !!max_hard_upload_rate * 1024 in *)
  let estimated_capacity = detected_uplink_capacity () in
  if !verbose_upload then begin
    lprintf "usage: %d(%d) capacity: %d "
      (short_delay_upload_usage ()) 
      (upload_usage ()) 
      estimated_capacity;
    lprint_newline ()
  end;
  let len = Intmap.length !CommonClient.uploaders in
  if len < !!max_upload_slots then begin

(* enough free bw for another slot *)
    if short_delay_upload_usage () + slot_bw < estimated_capacity then begin
      if !verbose_upload then begin
	lprintf "uplink not fully used";
	lprint_newline ()
      end;
      incr not_saturated_count
    end else reset_state ();
          
    if len < min_upload_slots then begin
      if !verbose_upload then begin
	lprintf "too few upload slots";
	lprint_newline ()
      end;
      open_slots (min_upload_slots - len);
      reset_state ()
    end else if !not_saturated_count >= 2 then begin
      open_slots (min !allocation_cluster (!!max_upload_slots - len));
      incr allocation_cluster
    end
  end

let turn = ref (-1)

let refill_upload_slots () =
  incr turn;
  if !turn = 5 then
    turn := 0;
  if !!dynamic_slots then begin
    if !turn = 0 then
      (* call every 5s *)
      dynamic_refill_upload_slots ()
  end else
    (* call every 1s *)
    static_refill_upload_slots ();

  if !turn = 0 then
    (* call every 5s *)
    update_upload_history ()


let consume_bandwidth len =
  remaining_bandwidth := !remaining_bandwidth - len

let remaining_bandwidth () = !remaining_bandwidth
  
  

(**********************************************************************


                     DOWNLOAD SCHEDULER


***********************************************************************)

let download_credit = ref 0 
let download_fifo = Fifo.create ()

    
let download_engine () =
  if not (Fifo.empty download_fifo) then begin
      let credit = !!max_hard_download_rate in
      let credit = 2 * (if credit = 0 then 10000 else credit) in
      download_credit := !download_credit + credit;
      let rec iter () =
        if !download_credit > 0 && not (Fifo.empty download_fifo) then  
          begin
            (try
                let (f, len) = Fifo.take download_fifo in
                download_credit := !download_credit - (len / 1000 + 1);
                f ()
              with _ -> ());
            iter ()
          end
      in
      iter ()
    end

let queue_download_request f len =  
  if !!max_hard_download_rate = 0 then 
    f ()
  else
    Fifo.put download_fifo (f,len)    

    (*
  (* timer started every 1/10 seconds *)
let download_timer () =
  *)


  (* timer started every 1/10 seconds *)
let upload_download_timer () =
  (try download_engine () 
    with e -> 
        lprintf "Exception %s in download_engine\n"  (Printexc2.to_string e)
  );
  (try next_uploads ()
    with e ->  lprintf "exc %s in upload\n" (Printexc2.to_string e))

(*
    head.agent=MyAgentName/2.6
    head.version=S0.4
    bitprint=433IUJ4MB4TRY3X74SL4B6BTD3JLTD5C
    tag.file.length=2373194
    tag.file.first20=1F8B080028F2DE3E0003ECFD7B7FDB3696380ECF
    tag.filename.filename=mldonkey-2.5-3.sources.tar.gz
    tag.ed2k.ed2khash=fe0cefcf20e7106214b6546d85061432
    tag.uuhash.uuhash=hqdpK4JOn2/mplyBEfTafcO4+yA
*)


let make_bitzi_post_request args= 
  
  let module H = Http_client in
  let r = {
      H.basic_request with
      
      H.req_url = Url.of_string "http://bitzi.com/lookup" ~args:args;
      H.req_user_agent = Printf.sprintf "MLdonkey/%s" Autoconf.current_version;
      H.req_referer = None;
      H.req_request = H.POST;
    } in
  
  r
  
  