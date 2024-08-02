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

open Gettext
open Md4
open LittleEndian
open Unix
open Printf2

let _s x = _s "Mld_hash" x
let _b x = _b "Mld_hash" x  

let zero = Int64.zero
let one = Int64.one
let (++) = Int64.add
let (--) = Int64.sub
let ( ** ) x y = Int64.mul x y
let ( // ) x y = Int64.div x y

let edk_block_size = 9728000L
let edk_zone_size = 180L ** 1024L
let tiger_block_size = Int64.of_int (1024 * 1024)
let partial = ref false
let partial_zone = ref false
let keep_file_after_check = ref false

(*************************************************************************)
(*                                                                       *)
(*                         tiger_of_array                                *)
(*                                                                       *)
(*************************************************************************)
    
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
  let s = Bytes.create (1 + Tiger.length * 2) in
  s.[0] <- '\001';
  String.blit (TigerTree.direct_to_string d1) 0 s 1 Tiger.length;
  String.blit (TigerTree.direct_to_string d2) 0 s (1+Tiger.length) Tiger.length;
  let t = Tiger.string @@ Bytes.unsafe_to_string s in
  let t = TigerTree.direct_of_string (Tiger.direct_to_string t) in
  t

(*************************************************************************)
(*                                                                       *)
(*                         tiger_max_block_size                          *)
(*                                                                       *)
(*************************************************************************)
  
let rec tiger_max_block_size block len =
  if block >= len then block
  else tiger_max_block_size (block*2) len

(*************************************************************************)
(*                                                                       *)
(*                         tiger_of_array                                *)
(*                                                                       *)
(*************************************************************************)
  
let tiger_of_array array =
  tiger_of_array array 0 (tiger_max_block_size  1 (Array.length array))
  

(*************************************************************************)
(*                                                                       *)
(*                         bitprint_file                                 *)
(*                                                                       *)
(*************************************************************************)

let bitprint_file fd file_size =
  lprintf "Calculating SHA1\n";
  let sha1 = Sha1.digest_subfile fd zero file_size in
  lprintf "Calculating TigerTree\n";
  let tiger = TigerTree.digest_subfile fd zero file_size in
  lprintf "urn:bitprint:%s.%s\n" (Sha1.to_string sha1) (TigerTree.to_string tiger);
  let file_size = Unix32.getsize64 fd in
  let nchunks = Int64.to_int (Int64.pred file_size // tiger_block_size) + 1 in
  let chunks = 
    let chunks = Array.make nchunks tiger in
    for i = 0 to nchunks - 1 do
      let begin_pos = tiger_block_size ** (Int64.of_int i) in
      let end_pos = begin_pos ++ tiger_block_size in
      let end_pos = min end_pos file_size in
      let len = end_pos -- begin_pos in
      let tiger = TigerTree.digest_subfile fd begin_pos len in
      if !partial then lprintf "  Partial %4d/%4d : %s\n" i nchunks (TigerTree.to_string tiger);
      chunks.(i) <- tiger
    done;
    chunks
  in
  let tiger2 = tiger_of_array chunks in
  sha1, tiger2

(*************************************************************************)
(*                                                                       *)
(*                         bitprint_filename                             *)
(*                                                                       *)
(*************************************************************************)
  
let bitprint_filename filename =
  let fd = Unix32.create_ro filename in
  let file_size = Unix32.getsize64 fd in
  let (sha1, tiger2) = bitprint_file fd file_size in
  lprintf "urn:bitprint:%s.%s\n" (Sha1.to_string sha1) (TigerTree.to_string tiger2);
  ()

  
(*************************************************************************)
(*                                                                       *)
(*                         ed2k_hash_file                                *)
(*                                                                       *)
(*************************************************************************)

let ed2k_hash_file fd file_size =
  (* See: DonkeyGlobals *)
  let nchunks = Int64.to_int (file_size // edk_block_size) + 1 in
  let nchunk_hashes = Int64.to_int (file_size // edk_block_size) in
  let nchunk_hashes = if nchunk_hashes <> 0 then nchunk_hashes + 1 else nchunk_hashes in
  let md4 = if nchunk_hashes = 0 then
      Md4.digest_subfile fd zero file_size        
    else
    let chunks = Bytes.create (nchunks*16) in
    for i = 0 to nchunks - 1 do
      let begin_pos = edk_block_size ** (Int64.of_int i) in
      let end_pos = begin_pos ++ edk_block_size in
      let end_pos = min end_pos file_size in
      let len = end_pos -- begin_pos in
      let md4 = Md4.digest_subfile fd begin_pos len in
      if !partial then lprintf "  Partial %4d/%4d (%Ld - %Ld): %s\n"
                         i nchunks begin_pos end_pos (Md4.to_string md4);
      let md4 = Md4.direct_to_string md4 in
      String.blit md4 0 chunks (i*16) 16;
    done;
    Md4.string @@ Bytes.unsafe_to_string chunks
  in
  md4

(*************************************************************************)
(*                                                                       *)
(*                         ed2k_hash_filename                            *)
(*                                                                       *)
(*************************************************************************)
  
let ed2k_hash_filename filename = 
  lprintf "Calculating ed2k of %s\n" filename;
  let fd = Unix32.create_ro filename in
  let file_size = Unix32.getsize64 fd in
  let md4 = ed2k_hash_file fd file_size in
  lprintf "ed2k://|file|%s|%Ld|%s|/\n" 
    (Url.encode (Filename.basename filename))
    file_size
    (Md4.to_string md4)

(*************************************************************************)
(*                                                                       *)
(*                         aich_hash_file                                *)
(*                                                                       *)
(*************************************************************************)

type side = Left | Right

let aich_hash side nchunks f_hashchunk =
  let combine_sha1 hash1 hash2 =
    Sha1.string (Sha1.direct_to_string hash1 ^ Sha1.direct_to_string hash2)
  in
  let build_tree n =
    let rec aux side n next_leaf cont =
      if n = 1L then cont (f_hashchunk side next_leaf) (next_leaf ++ 1L)
      else
        let p, q = n // 2L, Int64.rem n 2L in
        aux Left (if q = 0L || side = Right then p else p ++ 1L) next_leaf
          (fun left_hash next_leaf ->
             aux Right (if q = 0L || side = Left then p else p ++ 1L) next_leaf
               (fun right_hash next_leaf ->
                  cont (combine_sha1 left_hash right_hash) next_leaf)) in
    aux side n 0L (fun root_hash number_of_leaves -> root_hash)
  in
  build_tree nchunks

let aich_hash_chunk side fd offset len =
  let nzones = (Int64.pred len // edk_zone_size) ++ 1L in
  let compute_sha1_zone side nzone =
    let begin_pos = offset ++ edk_zone_size ** nzone in
    let end_pos = offset ++ (min (edk_zone_size ** (nzone ++ 1L)) len) in
    let len = end_pos -- begin_pos in
    if !partial_zone then
      lprintf_nl "compute SHA1 of zone %Ld/%Ld (%Ld - %Ld) len %Ld"
        nzone nzones begin_pos end_pos len;
    Sha1.digest_subfile fd begin_pos len
  in
  aich_hash side nzones compute_sha1_zone

let aich_hash_file fd file_size =
  let nchunks = (Int64.pred file_size // edk_block_size) ++ 1L in
  let compute_sha1_chunk side nchunk =
    let begin_pos = edk_block_size ** nchunk in
    let end_pos = min (begin_pos ++ edk_block_size) file_size in
    let len = end_pos -- begin_pos in
    if !partial then
      lprintf_nl "compute SHA1 of chunk %Ld/%Ld (%Ld - %Ld) len %Ld"
        nchunk nchunks begin_pos end_pos len;
    aich_hash_chunk side fd begin_pos len
  in
  aich_hash Left nchunks compute_sha1_chunk

(*************************************************************************)
(*                                                                       *)
(*                         aich_hash_filename                            *)
(*                                                                       *)
(*************************************************************************)
  
let aich_hash_filename filename = 
  lprintf "Calculating AICH of %s\n" filename;
  let fd = Unix32.create_ro filename in
  let file_size = Unix32.getsize64 fd in
  let aich = aich_hash_file fd file_size in
  lprintf "AICH of %s = %s\n" 
    (Url.encode (Filename.basename filename)) (Sha1.to_string aich)

(*************************************************************************)
(*                                                                       *)
(*                         sha1_hash_file                                *)
(*                                                                       *)
(*************************************************************************)

let sha1_hash_filename block_size filename =
  let fd = Unix32.create_ro filename in
  let file_size = Unix32.getsize64 fd in
  let nchunks = Int64.to_int (Int64.pred file_size // block_size) + 1 in
  for i = 0 to nchunks - 1 do
    let begin_pos = block_size ** (Int64.of_int i) in
    let end_pos = begin_pos ++ block_size in
    let end_pos = min end_pos file_size in
    let len = end_pos -- begin_pos in
    let md4 = Sha1.digest_subfile fd begin_pos len in
    if !partial then lprintf "  Partial %4d/%4d (%Ld-%Ld) : %s\n" i nchunks begin_pos end_pos
      (Sha1.to_string md4);
  done

(*************************************************************************)
(*                                                                       *)
(*                         sig2dat_hash_filename                         *)
(*                                                                       *)
(*************************************************************************)
  
let sig2dat_hash_filename filename =
  lprintf "Calculating sig2dat of %s\n" filename;
  let fd = Unix32.create_ro filename in
  let file_size = Unix32.getsize64 fd in
  let len64 = min 307200L file_size in
  let len = Int64.to_int len64 in
  let s = Bytes.create len in
  Unix32.read fd zero s 0 len;
  let md5ext = Md5Ext.string @@ Bytes.unsafe_to_string s in
  lprintf "sig2dat://|File: %s|Length: %Ld Bytes|UUHash: %s|/\n"
    (Url.encode (Filename.basename filename)) file_size (Md5Ext.to_string md5ext);
  lprintf "    Hash: %s\n" (Md5Ext.to_hexa_case false md5ext);
  ()

(*************************************************************************)
(*                                                                       *)
(*                         check_external_functions                      *)
(*                                                                       *)
(*************************************************************************)
  
let check_external_functions file_size = 
  partial := true;
  let test_string_len = 43676 in
  let dummy_string = "bonjourhello1" in
  
  let create_diskfile filename size =
    Unix32.create_diskfile filename true
  in
  let create_sparsefile filename size =
    Unix32.create_sparsefile filename true
  in
  let create_multifile filename size =    
    let rec iter pos size list =
      if size <> zero then
        let new_size = (size // (Int64.of_int 2)) ++ one in
        let filename = Printf.sprintf "%d-%Ld" pos new_size in
        iter (pos+1) (size -- new_size) 
        ((filename, new_size) :: list)
      else  list
    in
    let files = iter 0 size [] in
    lprintf "File %s will be:\n" filename;
    List.iter (fun (name, size) ->
        lprintf "    %-50s %Ld\n" name size;
    ) files;
    Unix32.create_multifile filename 
      true  files
  in    
  
  let (file_types : (string * (string -> int64 -> Unix32.t)
        * (string -> int64 -> Unix32.t) ) list) = 
    [
      "diskfile", create_diskfile, create_diskfile;
      "sparsefile", create_sparsefile, create_diskfile;
      "multifile", create_multifile, create_multifile;
    
    ] in
  
  let test_string = Bytes.create test_string_len in
  let rec iter pos =
    if pos < test_string_len then
      let end_pos = min test_string_len (2*pos) in
      let len = end_pos - pos in        
      Bytes.blit test_string 0 test_string pos len;
      iter end_pos
  in
  
  let dummy_string_len = String.length dummy_string in
  String.blit dummy_string 0 test_string 0 dummy_string_len;
  iter dummy_string_len;
  
  
  let test_string_len64 = Int64.of_int test_string_len in
  let rec iter pos waves =
    if pos < file_size then
      let end_pos = min file_size (pos ++ test_string_len64) in
      let len64 = end_pos -- pos in        
      let len = Int64.to_int len64 in
      iter end_pos
        ((pos, len) :: waves)
    else
      waves
  in
  let waves = iter zero [] in
  lprintf "\n";
  List.iter (fun (name,f,f') ->
      let filename = Printf.sprintf "test.%s.%Ld" name file_size in
      try
        lprintf "Creating file %s\n" filename;
        let file = f filename file_size in
        Unix32.ftruncate64 file file_size false;

(*
            lprintf "Computing ed2k hash of zeroed file\n";
            let md4 = ed2k_hash_file file file_size in
            lprintf "ed2k://|file|%s|%Ld|%s|\n" 
              filename
              file_size
              (Md4.to_string md4);
*)

        lprintf "Filling file\n";
        List.iter (fun (pos, len) ->
            Unix32.write_bytes file pos test_string 0 len;                
        ) waves;
        
        lprintf "Computing ed2k hash\n";
        let md4 = ed2k_hash_file file file_size in
        let aich = aich_hash_file file file_size in
        lprintf "ed2k://|file|%s|%Ld|%s|h=%s|/\n" 
          (Url.encode filename) file_size (Md4.to_string md4) (Sha1.to_string aich);
        
        lprintf "Computing bitprint hash\n";
        let (sha1, tiger2) = bitprint_file file file_size in
        lprintf "urn:bitprint:%s.%s\n" (Sha1.to_string sha1) (TigerTree.to_string tiger2);
        
        Unix32.close file;

        if not !keep_file_after_check then begin
          lprintf (_b "Renaming...\n");
          Unix32.rename file (filename ^ ".final");
          lprintf (_b "Removing %s\n") filename;
          (try Unix32.remove file with _ -> ());
          let file = f' (filename ^ ".final") file_size in
          Unix32.close file;
          Unix32.remove file;
        end;
        (try Sys.remove "zero_chunk" with _ -> ());

        lprintf "done\n"
      with e ->
          lprintf (_b "Exception %s in check_external_functions %s.%Ld\n")
            (Printexc2.to_string e) name file_size)
  file_types

let max_diff_size = 30000000L
  
let diff_chunk args =
  let filename1 = args.(0) in
  let filename2 = args.(1) in
  let begin_pos = Int64.of_string args.(2) in
  let end_pos = Int64.of_string args.(3) in
  
  let total = ref 0 in
  
  if end_pos -- begin_pos > max_diff_size then
    failwith (Printf.sprintf "Cannot diff chunk > %Ld bytes" max_diff_size);
  let len = Int64.to_int (end_pos -- begin_pos) in
  let read_string fd pos len =
    let s = Bytes.create len in
    Unix32.read fd pos s 0 len;
    Bytes.unsafe_to_string s
  in
  let fd1 = Unix32.create_ro filename1 in
  let fd2 = Unix32.create_ro filename2 in
  let s1 = read_string fd1 begin_pos len in
  let s2 = read_string fd2 begin_pos len in
  
  let rec iter_in old_pos pos =
    if pos < len then 
      if s1.[pos] <> s2.[pos] then begin
          lprintf "  common %Ld-%Ld %d\n"
            (begin_pos ++ Int64.of_int old_pos)
          (begin_pos ++ Int64.of_int (pos-1))
          (pos - old_pos);
          iter_out pos (pos+1)
        end else
        iter_in old_pos (pos+1)
    else
      lprintf "  common %Ld-%Ld %d\n"
        (begin_pos ++ Int64.of_int old_pos)
      (begin_pos ++ Int64.of_int (pos-1))
      (pos - old_pos)
      
  and iter_out old_pos pos =
    if pos < len then
      if s1.[pos] <> s2.[pos] then 
        iter_out old_pos (pos+1) 
      else
        begin
          lprintf "  diff   %Ld-%Ld %d\n"
            (begin_pos ++ Int64.of_int old_pos)
          (begin_pos ++ Int64.of_int (pos-1))
          (pos - old_pos);
          total := !total + (pos - old_pos);
          iter_in (pos-1) (pos+1)
        end
    else begin
        lprintf "  diff   %Ld-%Ld %d\n"
        (begin_pos ++ Int64.of_int old_pos)
        (begin_pos ++ Int64.of_int (pos-1))
        (pos - old_pos);    
        total := !total + (pos - old_pos);
        end
  in
  iter_in 0 0;
  lprintf "Diff Total: %d/%d bytes\n" !total len
  
  
  
(*************************************************************************)
(*                                                                       *)
(*                         MAIN                                          *)
(*                                                                       *)
(*************************************************************************)


let hash = ref ""
let chunk_size = ref zero
  
let _ =
  MlUnix.set_signal  Sys.sigint
    (Sys.Signal_handle (fun _ -> lprintf_nl "Received SIGINT, stopping mld_hash...";
        exit 0));

  MlUnix.set_signal  Sys.sigterm
    (Sys.Signal_handle (fun _ -> lprintf_nl "Received SIGTERM, stopping mld_hash...";
        exit 0));

  Arg2.parse2 [
    "-diff_chunk", Arg2.Array (4, diff_chunk), 
    "<filename1> <filename2> <begin_pos> <end_pos> : compute diff between the two files";
    "-hash", Arg2.String ( (:=) hash), _s " <hash> : Set hash type you want to compute (ed2k, aich, sha1, sig2dat, bp)";
    "-sha1", Arg2.String (fun size ->
        hash := "sha1";
        chunk_size := Int64.of_string size;
    ), " <chunk_size> : Set hash type to sha1 and chunk_size to <chunk_size>";
    "-partial", Arg2.Unit (fun _ -> partial := true), _s ": enable display of partial hash values";
    "-partial_zone", Arg2.Unit (fun _ -> partial_zone := true), _s ": enable display of zone AICH hash values";
    "-check_keep", Arg2.Unit (fun _ -> keep_file_after_check := true), _s ": keep files after checking functions";
    "-check", Arg2.Int64 check_external_functions, _s " <size of testfile in bytes>: check C file functions";
  ] (fun filename ->
      match !hash with
      | "ed2k" | "edk" -> ed2k_hash_filename filename
      | "aich" -> aich_hash_filename filename
      | "emule" -> ed2k_hash_filename filename; aich_hash_filename filename
      | "sha1" -> sha1_hash_filename !chunk_size filename
      | "sig2dat" -> sig2dat_hash_filename filename
      | "bp" -> bitprint_filename filename
      | _ -> 
          ed2k_hash_filename filename;
          aich_hash_filename filename;
          sig2dat_hash_filename filename;
          bitprint_filename filename
  ) (_s " <filenames> : compute hashes of filenames");
  exit 0
