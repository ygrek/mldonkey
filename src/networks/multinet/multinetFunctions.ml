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
  
open MultinetTypes
open MultinetGlobals
  
let register_network f =
  networks := f :: !networks;
  ()

let new_network_file n = {
    op_download_finish = (fun _ -> ());
    op_download_sources = (fun _ -> []);
    op_download_recover = (fun _ -> ());
    op_download_pause = (fun _ -> ());
    op_download_resume = (fun _ -> ());
    op_download_to_value = (fun _ -> []);
    op_download_debug = (fun _ _ -> ());
    op_download_network = n;
  }

let new_file_network name = {
    op_download_netname = name;
    op_download_start = (fun _ -> ());
    op_download_of_value = (fun  _ _ -> ());
    op_source_fifo = Fifo.create ();
    op_source_delay = 600;
  }

let add_file_impl file (f : 'a network_file) (v : 'a) =
  file.file_impls <- (Obj.magic (f,v)) :: file.file_impls





(* We should have a Hashtbl keeping the files by their UID *)

let min_range = Int64.of_int (256 * 1024)
let maxparts = Int64.of_int 200

let update_best_file_name file name =
  set_file_best_name file name;
  file.file_filenames <- name ::
  (List2.removeq name file.file_filenames)

let new_download file_id filenames file_size old_file file_uids = 
  let file_id = match file_id with
      Some file_id -> file_id
    | None ->
        let file_id = ref None in
        List.iter (fun uid ->
            match uid with
              Ed2k _ | Sha1 _ | Md5Ext _ ->
                file_id := Some (string_of_uid uid)
            | _ -> ()
        ) file_uids;
        match !file_id with
          None -> Md4.to_string (Md4.random ())
        | Some file_id -> file_id
  in
  let file_temp = Filename.concat !!temp_directory 
      (Printf.sprintf "COMMON-%s" file_id) in
  
  (match old_file with
      None -> () 
    | Some old_file ->
        try
          Unix2.rename old_file file_temp;
          Unix.chmod file_temp 0o644;
        with e -> 
            lprintf "[ERROR] Exception %s while copying old file [%s]"
              (Printexc2.to_string e) old_file
  );
  let t = Unix32.create file_temp [Unix.O_RDWR; Unix.O_CREAT] 0o666 in
  let swarmer = Int64Swarmer.create () in
  let chunk_size = max min_range (file_size // maxparts) in
  let partition = fixed_partition swarmer (-1) chunk_size in
  let rec file = {
      file_file = file_impl;
      file_id = file_id;
      file_swarmer = swarmer;
      file_uids = expand_uids file_uids;
      file_filenames = filenames;
      file_impls = [];
      file_files = [];
      file_verified_partition = partition;
      file_bitzi_ticket = Bitzi_next_retry 0;
      
      file_locations = Intmap.empty;
      file_sources = Array.init sources_nqueues (fun _ ->
          SourcesQueueCreate.oldest_first ());

    } and file_impl =  {
      dummy_file_impl with
      impl_file_fd = t;
      impl_file_size = file_size;
      impl_file_downloaded = Int64.zero;
      impl_file_val = file;
      impl_file_ops = file_ops;
      impl_file_age = last_time ();          
      impl_file_best_name = 
      match filenames with
        [] -> file_id
      | name :: _ -> name;
    } 
  in
  Int64Swarmer.set_size swarmer file_size;  
  Int64Swarmer.set_writer swarmer (fun offset s pos len ->      
      
      if !!CommonOptions.buffer_writes then 
        Unix32.buffered_write_copy t offset s pos len
      else
        Unix32.write  t offset s pos len
  );
  current_files := file :: !current_files;
  file_add file_impl FileDownloading;
  Hashtbl.add files_by_num (file_num file) file;
(*      lprintf "ADD FILE TO DOWNLOAD LIST\n"; *)

(* We should probably recall this part every few minutes... *)
  
  List.iter (fun f -> try f.op_download_start file with _ -> ()) !networks;
  (try MultinetBitzi.query_bitzi_ticket file with _ -> ());
  file      

let new_filename file name = 
  if not (List.mem name file.file_filenames) then begin
      file.file_filenames <- file.file_filenames @ [name] ;
(*          update_best_name file*)
    end

let remove_file file =
  Hashtbl.remove files_by_num (file_num file);
  Unix32.close (file_fd file);
  current_files := List2.removeq file !current_files;
  List.iter (fun (n,v) -> n.op_download_finish v) file.file_impls
  
  
let download_is_finished file = 
  if List.memq file !current_files then begin
      lprintf "[DEBUG] file is finished...2\n";
      file_completed (as_file file);
      remove_file file;
      List.iter (fun (n,v) -> n.op_download_finish v) file.file_impls
    end    

let check_finished file = 
  try
    if Int64Swarmer.dirty file.file_swarmer then begin
        file_must_update file;
        Int64Swarmer.verify_file file.file_swarmer;
      end;
    if file_state file <> FileDownloaded &&
      file_size file = Int64Swarmer.downloaded file.file_swarmer then begin
        lprintf "[DEBUG] check file is finished...2\n";
        let partition = file.file_verified_partition in
        lprintf "[DEBUG] check file is finished...3\n";
        let bitmap = Int64Swarmer.verified_bitmap partition in
        let min = ref bitmap.[0] in
        for i = 1 to String.length bitmap - 1 do
          if bitmap.[i] < !min then min := bitmap.[i]
        done;  
        lprintf "[DEBUG] check file is finished...5\n";
        if !min = '3' ||
          (!min = '2' &&
(* It might not be possible to verify this file... *)
            (!!commit_unverified_files ||
              not (Int64Swarmer.is_file_verifiable file.file_swarmer)))
        then download_is_finished file;
      end
  with _ -> 
      lprintf "[DEBUG] check file is finished...1\n";
      ()

let check_finished_timer _ =
  List.iter check_finished !current_files

let first_verified_block p =
  let bitmap = Int64Swarmer.verified_bitmap p in
  let nverified = ref 0 in
  for i = 0 to String.length bitmap - 1 do
    if bitmap.[i] = '3' then incr nverified
  done;
  !nverified = 1

let downloaded file = Int64Swarmer.downloaded file.file_swarmer

let _ =
  network.op_network_connected_servers <- (fun _ -> [])
