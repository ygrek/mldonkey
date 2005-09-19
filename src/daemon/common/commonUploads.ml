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
open Md4
open CommonShared
open Printf2
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

(*
PROBLEMS: most of the time, users won't share their files on all networks.
We should provide a different directory than incoming/, where files
would be shared, per directory ?

We should move to a per-network approach. This module would be a functor,
and each network would be configured to share or not share different
directories. Moreover, we should have a different sharing strategy.

Default would be: share all files greater than 1 MB in incoming/ on Edonkey.

*)

(*******************************************************************

                         TYPES

*******************************************************************)

let ed2k_block_size = Int64.of_int 9728000
let tiger_block_size = Int64.of_int (1024 * 1024)

type shared_file = {
    shared_codedname : string;
    shared_info : Store.index;
    shared_fd : Unix32.t;
    shared_format : CommonTypes.format;
    shared_impl : shared_file shared_impl;
    mutable shared_uids_wanted :
    (file_uid_id * (shared_file -> Uid.t -> unit)) list;
  }

and shared_info = {
    shared_fullname : string;
    shared_size : int64;
    mutable shared_md4s : Md4.t array;
    mutable shared_tiger : TigerTree.t array;
    mutable shared_bitprint : Uid.t option;
    mutable shared_mtime : float;
    mutable shared_uids : Uid.t list;
    mutable shared_id : int;
  }

and shared_tree =
  {
    shared_dirname : string;
    mutable shared_files : shared_file list;
    mutable shared_dirs : (string * shared_tree) list;
  }

type local_search = {
    mutable local_search_results : (shared_file * shared_info) list;
    mutable local_search_query : query;
  }

module IndexingSharedFiles = struct

    let store_name = "shared_store"

    let search_query s = s.local_search_query

    type search = local_search
    type result = shared_info

    let result_names sh = [sh.shared_fullname]
    let result_size sh = sh.shared_size
    let result_uids sh = []
    let result_tags sh = []

(* We should probably directly use the Store.index here so that all
shared_infos are stored on disk. *)
    type stored_result = Store.index
    let result_index r = r

  end

module IndexedSharedFiles = CommonIndexing.Make(IndexingSharedFiles)

(*************************************************************************)
(*                                                                       *)
(*                         SAVED SHARED FILES                            *)
(*                                                                       *)
(*************************************************************************)

module SharedFileOption = struct

    let get_value assocs name conv =
      try conv (List.assoc name assocs)
      with _ -> failwith (Printf.sprintf "Bad shared file %s" name)

    let value_to_info v =
      match v with
        Options.Module assocs ->
          let sh_md4s = get_value assocs "md4s"
              (value_to_array (fun v ->
                  Md4.of_string (value_to_string v)))
          in
          let sh_ttr = get_value assocs "ttr"
              (value_to_array (fun v ->
                  TigerTree.of_string (value_to_string v)))
          in
          let sh_uids = get_value assocs "uids"
              (value_to_list (fun v ->
                  Uid.of_string (value_to_string v)))
          in

          let sh_size = get_value assocs "size"  value_to_int64 in
          let sh_name = get_value assocs "name" value_to_filename in
          let sh_mtime = get_value assocs "mtime" value_to_float in

          let sh_bitprint = ref None in
          List.iter (fun uid ->
              match Uid.to_uid uid with
                Bitprint _ -> sh_bitprint := Some uid;
              | _ -> ()
          ) sh_uids;

          { shared_fullname = sh_name;
            shared_mtime = sh_mtime;
            shared_size = sh_size;
            shared_md4s = sh_md4s;
            shared_tiger = sh_ttr;
            shared_bitprint = !sh_bitprint;
            shared_uids = sh_uids;
            shared_id = 0;
          }

      | _ -> failwith "Options: not a shared file info option"

    let info_to_value info =
      Options.Module [
        "name", filename_to_value info.shared_fullname;
        "md4s", array_to_value Md4.hash_to_value info.shared_md4s;
        "mtime", float_to_value info.shared_mtime;
        "size", int64_to_value info.shared_size;
        "ttr", array_to_value TigerTree.hash_to_value info.shared_tiger;
        "uids", list_to_value (fun v ->
            string_to_value (Uid.to_string v)) info.shared_uids;
      ]

    let t = define_option_class "SharedFile" value_to_info info_to_value
  end

let shared_ini = create_options_file "shared_files.ini"

let shared_section = file_section shared_ini [] ""

let old_shared_files = define_option shared_section
    ["shared_files"] ""
    (list_option SharedFileOption.t) []

let infos_by_name = Hashtbl.create 113

let _ =
  set_after_load_hook shared_ini (fun _ ->
      List.iter (fun info ->
          let index = IndexedSharedFiles.add info in
          Hashtbl.add infos_by_name info.shared_fullname index
      ) !!old_shared_files;
      old_shared_files =:= [];
  );
  set_before_save_hook shared_ini (fun _ ->
      Hashtbl.iter (fun _ index ->
          old_shared_files =:= (IndexedSharedFiles.get_result index)
          :: !!old_shared_files
      ) infos_by_name
  );
  set_after_save_hook shared_ini (fun _ -> old_shared_files =:= [])

let load () = try Options.load shared_ini with _ -> ()
let save () = Options.save shared_ini

(*************************************************************************)
(*                                                                       *)
(*                         NETWORK                                       *)
(*                                                                       *)
(*************************************************************************)

let network = CommonNetwork.new_network "GS" "Global Shares"
    [ VirtualNetwork ]

let _ =
  network.op_network_connected <- (fun _ -> false);
  network.op_network_is_enabled <- (fun _ -> raise IgnoreNetwork);
  network.op_network_info <- (fun _ -> raise Not_found);
  network.op_network_info <- (fun n ->
      {
        network_netnum = network.network_num;
        network_config_filename = (match network.network_config_file with
            [] -> "" | opfile :: _ -> options_file_name opfile);
        network_netname = network.network_name;
        network_netflags = network.network_flags;
        network_enabled = true;
        network_uploaded = Int64.zero;
        network_downloaded = Int64.zero;
        network_connected = 0;
      });
  network.op_network_connected_servers <- (fun _ -> [])

let (shared_ops : shared_file CommonShared.shared_ops) =
  CommonShared.new_shared_ops network

let waiting_shared_files = ref []
let shareds_by_uid = Hashtbl.create 13
let shareds_by_id = Hashtbl.create 13

let add_by_uid uid sh =
  let urn = Uid.to_string uid in
  Hashtbl.add shareds_by_uid uid sh

let find_by_uid uid =
  let urn = Uid.to_string uid in
  Hashtbl.find shareds_by_uid uid

module SharedFilesIndex = IndexedSharedFiles.MakeIndex (struct
      let add_search_result s sh =
        let r = Hashtbl.find shareds_by_id sh.shared_id in
        s.local_search_results <- (r, sh) :: s.local_search_results
    end)

let current_job = ref None

(*******************************************************************

                      DATA STRUCTURES

*******************************************************************)

let shareds_counter = ref 1
let shared_counter = ref (Int64.zero)
let shared_files = Hashtbl.create 13

let _ =
  Heap.add_memstat "CommonUploads" (fun level buf ->
   Printf.bprintf buf "  infos_by_name: %d\n" (Hashtbl.length infos_by_name);
   Printf.bprintf buf "  shareds_by_uid: %d\n" (Hashtbl.length shareds_by_uid);
   Printf.bprintf buf "  shareds_by_id: %d\n" (Hashtbl.length shareds_by_id);
   Printf.bprintf buf "  shared_files: %d\n" (Hashtbl.length shared_files);
  )


let new_shared_dir dirname = {
    shared_dirname = dirname;
    shared_files = [];
    shared_dirs = [];
  }

let shared_tree = new_shared_dir ""

(*******************************************************************

                    HASHES COMPUTATION

*******************************************************************)

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

let build_tiger_tree_file uid ttr =
  let s = make_tiger_tree ttr in
  Unix2.safe_mkdir "ttr";
  Unix2.can_write_to_directory "ttr";
  File.from_string (Filename.concat "ttr" (Uid.to_file_string uid)) s

let rec start_job_for sh (wanted_id, handler) =
  let info = IndexedSharedFiles.get_result sh.shared_info in
  try
    List.iter (fun id ->
        match wanted_id,Uid.to_uid id with
          BITPRINT, Bitprint _
        | SHA1, Sha1 _
        | ED2K, Ed2k _
        | MD5, Md5 _
        | MD5EXT, Md5Ext _
        | TIGER, TigerTree _
          -> (try handler sh id with _ -> ()); raise Exit
        | _ -> ()
    ) info.shared_uids;

    match wanted_id with
      SHA1 ->
        begin
          try
            CommonHasher.compute_sha1 (Unix32.filename sh.shared_fd)
            zero info.shared_size (fun job ->
                if job.CommonHasher.job_error then begin
                    lprintf_nl "[cUp] Error during hashing of %s" info.shared_fullname;
                    current_job := None;
                  end else
                  begin
                    let sha1 = job.CommonHasher.job_result
                    in
                    let uid = Uid.create (Sha1 sha1) in
                    info.shared_uids <- uid :: info.shared_uids;
                    IndexedSharedFiles.update_result sh.shared_info info;

                    add_by_uid uid sh;
                    start_job_for sh (wanted_id, handler)
                  end
            );
          with e ->
              current_job := None;
              raise e
        end

    | BITPRINT ->
        let sha1 = ref None in
        let tiger = ref None in
        List.iter (fun id ->
            match Uid.to_uid id with
            | Sha1 (s) -> sha1 := Some s
            | TigerTree (s) -> tiger := Some s
            | _ -> ()
        ) info.shared_uids;
        begin
          match !sha1, !tiger with
            Some sha1, Some tiger ->
              let uid = Uid.create (Bitprint (sha1, tiger)) in
              info.shared_uids <- uid :: info.shared_uids;
              info.shared_bitprint <- Some uid;
              IndexedSharedFiles.update_result sh.shared_info info;

              add_by_uid uid sh;

              build_tiger_tree_file uid info.shared_tiger;

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
        let md5ext =
          try
            let fd = Unix32.create_rw info.shared_fullname in
            let file_size = Unix32.getsize64 fd false in
            let len64 = min (Int64.of_int 307200) file_size in
            let len = Int64.to_int len64 in
            let s = String.create len in
            Unix32.read fd zero s 0 len;
            Md5Ext.string s
          with e ->
              current_job := None;
              raise e
        in
        let uid = Uid.create (Md5Ext (md5ext)) in
        info.shared_uids <- uid :: info.shared_uids;
        IndexedSharedFiles.update_result sh.shared_info info;

        add_by_uid uid sh;
        start_job_for sh (wanted_id, handler)

    | ED2K ->
        let size = info.shared_size in
        let chunk_size = ed2k_block_size  in
        let nhashes = Int64.to_int (size // chunk_size) + 1 in
        let rec iter pos hashes =
          if pos < size then
            try
              CommonHasher.compute_md4 info.shared_fullname
                pos (min (size -- pos) chunk_size)
              (fun job ->
                  if job.CommonHasher.job_error then begin
                      lprintf_nl "[cUp] Error during hashing of %s" info.shared_fullname;
                      current_job := None;
                    end else begin

                      iter (pos ++ chunk_size) (job.CommonHasher.job_result :: hashes)
                    end)
            with e ->
                current_job := None;
                raise e
          else
          let list = List.rev hashes in
          let ed2k = md4_of_list list in
          let uid = Uid.create (Ed2k (ed2k)) in
          let urn = Uid.to_string uid in
          info.shared_md4s <- Array.of_list list;
          info.shared_uids <- uid :: info.shared_uids;
          IndexedSharedFiles.update_result sh.shared_info info;

          add_by_uid uid sh;
          start_job_for sh (wanted_id, handler)
        in
        iter zero []

    | TIGER ->

        if TigerTree.enabled then

          let size = info.shared_size in
          let chunk_size = tiger_block_size in
          let nhashes = Int64.to_int (size // chunk_size) + 1 in
          let rec iter pos hashes =
            if pos < size then
                CommonHasher.compute_tiger info.shared_fullname
                  pos (min (size -- pos) chunk_size)
              (fun job ->
                    if job.CommonHasher.job_error then begin
                      lprintf_nl "[cUp] Error during hashing of %s"
                      info.shared_fullname;
                        current_job := None;
                      end else begin
                        iter (pos ++ chunk_size)
                        (job.CommonHasher.job_result :: hashes)
                      end)
            else
            let array = Array.of_list (List.rev hashes) in
            let tiger = tiger_of_array array in
            let uid = Uid.create (TigerTree (tiger)) in
            info.shared_tiger <- array;
            info.shared_uids <- uid :: info.shared_uids;
            IndexedSharedFiles.update_result sh.shared_info info;

            add_by_uid uid sh;
            start_job_for sh (wanted_id, handler)
          in
          iter zero []

    | _ -> raise Exit

  with Exit ->
      current_job := None
  | e -> current_job := None; raise e

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
              if !verbose_share then
                lprintf_nl "[cUp] shared_files_timer: starting job";
              sh.shared_uids_wanted <- tail;
              current_job := Some sh;
              start_job_for sh uid

let ask_for_uid sh uid f =
  sh.shared_uids_wanted <- (uid,f) :: sh.shared_uids_wanted;
  waiting_shared_files := sh :: !waiting_shared_files

(*******************************************************************

                      FUNCTIONS

*******************************************************************)

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

let new_info full_name size =
  incr shareds_counter;
  let fd = Unix32.create_ro full_name in
  let mtime = Unix32.mtime64 fd in
  try
    let index = Hashtbl.find infos_by_name full_name in
    let info = IndexedSharedFiles.get_result index in
    if info.shared_mtime <> mtime then begin
        Hashtbl.remove infos_by_name full_name;
        IndexedSharedFiles.remove_result index;
        raise Not_found;
      end;

    info.shared_id <- !shareds_counter;
    IndexedSharedFiles.update_result index info;
    info, index
  with Not_found ->
      let info = {
          shared_fullname = full_name;
          shared_uids = [];
          shared_mtime = mtime;
          shared_md4s = [||];
          shared_tiger = [||];
          shared_bitprint = None;
          shared_size = size;
          shared_id = !shareds_counter;
        } in
      let index =IndexedSharedFiles.add info in
      Hashtbl.add infos_by_name full_name index;
      info, index

let add_shared full_name codedname size =
  try
    Hashtbl.find shared_files codedname
  with Not_found ->

      let fd = Unix32.create_ro full_name in

      let info, index = new_info full_name size in

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
          shared_info = index;
          shared_codedname = codedname;
          shared_fd=  fd;
          shared_format = CommonMultimedia.get_info full_name;
          shared_impl = impl;
          shared_uids_wanted = [];
        } in

      update_shared_num impl;

(*      lprintf "FILE ADDED: %s\n" codedname;  *)
      Hashtbl.add shared_files codedname sh;
      Hashtbl.add shareds_by_id info.shared_id sh;

      List.iter (fun uid -> add_by_uid uid sh) info.shared_uids;

      SharedFilesIndex.add sh.shared_info;
      add_shared_file shared_tree sh (String2.split codedname '/');
      shared_counter := Int64.add !shared_counter size;
(*      lprintf "Total shared : %Ld\n" !shared_counter; *)
      sh

let iter f =
  Hashtbl.iter (fun _ sh ->
      f sh
  ) shared_files

let query q =
  let s = {
    local_search_query = q;
    local_search_results = []
    } in
  SharedFilesIndex.find s;
  s.local_search_results

let find_by_name name = Hashtbl.find shared_files name

(*let find_by_num num = Hashtbl.find table num *)

(**********************************************************************

                     UPLOAD SCHEDULER

***********************************************************************)

let client_is_connected c = is_connected (client_state c)

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
  let bool1 = can_write_len sock len in
  let upload_rate =
        (if !!max_hard_upload_rate = 0 then 10000 else !!max_hard_upload_rate)
    * 1024 in
  let bool2 =
    (
(*      lprintf "upload_rate %d -> %d\n" upload_rate
        (upload_rate * (Fifo.length upload_clients)); *)
(* changed 2.5.24
Don't put in a socket more than 10 seconds of upload.
  *)
    not_buffer_more sock (upload_rate * 10  (* * (Fifo.length upload_clients) *) ))
  in
(*  lprintf "can_write_len %b %b\n" bool1 bool2; *)
  let b = bool1 && bool2 in
(*  if not b then
lprintf "bool1 %b len %d bool2 %b upload %d\n" bool1 len bool2 upload_rate;
  *)
  b

let upload_to_one_client () =
(*  lprintf "upload_to_one_client %d %d\n" (Fifo.length upload_clients)
  !remaining_bandwidth;
Fifo.iter (fun c -> lprintf "   client %d\n" (client_num c))
upload_clients; *)
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
  (*
  if !verbose_upload then begin
      lprintf "Left %d\n" !remaining_bandwidth;
    end; *)
  complete_bandwidth := !complete_bandwidth + !remaining_bandwidth;
  incr counter;
  if !counter = 11 then begin
      counter := 1;
      total_bandwidth :=
      (if !!max_hard_upload_rate = 0 then 10000 * 1024
        else (maxi (!!max_hard_upload_rate - 1) 1) * 1024 );
      complete_bandwidth := !total_bandwidth;
(*      lprintf "Init to %d\n" !total_bandwidth;  *)
      remaining_bandwidth := 0
    end;

  let last_sec = ref 0 in
  for i = 0 to 9 do
    last_sec := !last_sec + sent_bytes.(i)
  done;

(*  if !verbose_upload then begin
      lprintf "last sec: %d/%d (left %d)\n" !last_sec !total_bandwidth
        (!total_bandwidth - !last_sec);
 (*
      for i = 0 to 9 do
        lprintf "    last[%d] = %d\n" i  sent_bytes.(i)
      done; *)

    end; *)
  remaining_bandwidth := mini (mini (mini
        (maxi (!remaining_bandwidth + !total_bandwidth / 10) 10000)
      !total_bandwidth) !complete_bandwidth)
  (!total_bandwidth - !last_sec);
  complete_bandwidth := !complete_bandwidth - !remaining_bandwidth;
(*  lprintf "Remaining %d[%d]\n" !remaining_bandwidth !complete_bandwidth;  *)
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
      if !verbose_upload then lprintf_nl "[cUp] Avoided inserting an uploader in pending slots!"
    end
  else
  if not (Intmap.mem (client_num c) !pending_slots_map) then
    begin
(* This is useless since it is the goal of the pending_slots_map
        else if Fifo.mem pending_slots_fifo (client_num c) then begin
	lprintf "Avoided inserting a client twice in pending slots\n";
	
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
      let c = Intmap.nth !pending_slots_map 
	(Random.int (Intmap.length !pending_slots_map)) in
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
    if !verbose_upload then
      lprintf_nl "[cUp] try to allocate %d more slots" n;
    while !i > 0 do
      find_pending_slot ();
      decr i
    done in

  let slot_bw = 3072 in
  let min_upload_slots = 3 in
(*  let estimated_capacity = !!max_hard_upload_rate * 1024 in *)
  let estimated_capacity = detected_uplink_capacity () in
  if !verbose_upload then
    lprintf_nl "[cUp] usage: %d(%d) capacity: %d"
      (short_delay_upload_usage ())
      (upload_usage ())
      estimated_capacity;
  let len = Intmap.length !CommonClient.uploaders in
  if len < !!max_upload_slots then begin

(* enough free bw for another slot *)
    if short_delay_upload_usage () + slot_bw < estimated_capacity then begin
      if !verbose_upload then
	lprintf_nl "[cUp] uplink not fully used";
      incr not_saturated_count
    end else reset_state ();

    if len < min_upload_slots then begin
      if !verbose_upload then
	lprintf_nl "[cUp] too few upload slots";
      open_slots (min_upload_slots - len);
      reset_state ()
    end else if !not_saturated_count >= 2 then begin
      open_slots (min !allocation_cluster (!!max_upload_slots - len));
      incr allocation_cluster
    end
  end

let turn = ref (-1)
let turn_h = ref (-1)

let refill_upload_slots () =
  incr turn;
  if !turn = 5 then
    turn := 0;
  if !turn_h = 360 then
    turn_h := 0;
  if !!dynamic_slots then begin
    if !turn = 0 then
      (* call every 5s *)
      dynamic_refill_upload_slots ()
  end else
    (* call every 1s *)
    static_refill_upload_slots ();

  if !turn = 0 then begin
    (* call every 5s *)
    incr turn_h;
    update_download_history ();
    update_upload_history ()
    end;

  if !turn_h = 0 then begin
    (* call every 720 * 5s *)
    update_h_download_history ();
    update_h_upload_history ()
    end

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

  (* timer started every 1/10 seconds *)
let upload_download_timer () =
  (try download_engine ()
    with e ->
        lprintf_nl "[cUp] Exception %s in download_engine" (Printexc2.to_string e)
  );
  (try next_uploads ()
  with e -> lprintf_nl "[cUp] exc %s in upload" (Printexc2.to_string e))

let words_of_filename =
  let extension_list = [
      "mp3" ; "avi" ; "jpg" ; "jpeg" ; "txt" ; "mov" ; "mpg" ; "ogm"
    ]
  in
  let rec remove_short list list2 =
    match list with
      [] -> List.rev list2
    | s :: list ->
        if List.mem s extension_list then
          remove_short list (s :: list2) else

        if String.length s < 5 then (* keywords should had list be 5 bytes *)
          remove_short list list2
        else
          remove_short list (s :: list2)
  in

  let get_name_keywords file_name =
    match remove_short (String2.stem file_name) [] with
      [] | [_] ->
        lprintf_nl "[cUp] Not enough keywords to recover %s" file_name;
        [file_name]
    | l -> l
  in
  get_name_keywords

open LittleEndian

let _ =
  CommonWeb.add_redirector_info "SHARED" (fun buf ->
      let module S = CommonShared in
      let total_shared = ref Int64.zero in
      let total_uploaded = ref Int64.zero in
      S.shared_iter (fun s ->
          let i = S.as_shared_impl s in
          total_uploaded :=
          Int64.add !total_uploaded i.S.impl_shared_uploaded;
          total_shared :=
          Int64.add !total_shared i.S.impl_shared_size
      );
      buf_int64 buf !total_shared;
      buf_int64 buf !total_uploaded;
  )
