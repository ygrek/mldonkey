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
open LittleEndian
open Unix
open Printf2

let zero = Int64.zero
let one = Int64.one
let (++) = Int64.add
let (--) = Int64.sub
let ( ** ) x y = Int64.mul x (Int64.of_int y)
let ( // ) x y = Int64.div x (Int64.of_int y)
  
let block_size = Int64.of_int 9728000

let tiger_block_size = Int64.of_int (1024 * 1024)

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
  let s = String.create (1 + Tiger.length * 2) in
  s.[0] <- '\001';
  String.blit (TigerTree.direct_to_string d1) 0 s 1 Tiger.length;
  String.blit (TigerTree.direct_to_string d2) 0 s (1+Tiger.length) Tiger.length;
  let t = Tiger.string s in
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
  let sha1 = Sha1.digest_subfile fd zero file_size in
  let tiger = TigerTree.digest_subfile fd zero file_size in
  lprintf "urn:bitprint:%s.%s\n" (Sha1.to_string sha1) (TigerTree.to_string tiger);
  let file_size = Unix32.getsize64 fd in
  let nchunks = Int64.to_int (Int64.div 
        (Int64.sub file_size Int64.one) tiger_block_size) + 1 in
  let chunks = 
    let chunks = Array.create nchunks tiger in
    for i = 0 to nchunks - 1 do
      let begin_pos = tiger_block_size ** i  in
      let end_pos = begin_pos ++ tiger_block_size in
      let end_pos = min end_pos file_size in
      let len = end_pos -- begin_pos in
      let tiger = TigerTree.digest_subfile fd begin_pos len in
      lprintf "  Partial %3d : %s\n" i (TigerTree.to_string tiger);
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
  let fd = Unix32.create_rw filename in
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
  let nchunks = Int64.to_int (Int64.div 
        (Int64.sub file_size Int64.one) block_size) + 1 in
  let md4 = if nchunks = 1 then
      Md4.digest_subfile fd zero file_size        
    else
    let chunks = String.create (nchunks*16) in
    for i = 0 to nchunks - 1 do
      let begin_pos = block_size ** i  in
      let end_pos = begin_pos ++ block_size in
      let end_pos = min end_pos file_size in
      let len = end_pos -- begin_pos in
      let md4 = Md4.digest_subfile fd begin_pos len in
      lprintf "  Partial %3d : %s\n" i (Md4.to_string md4);
      let md4 = Md4.direct_to_string md4 in
      String.blit md4 0 chunks (i*16) 16;
    done;
    Md4.string chunks
  in
  md4

(*************************************************************************)
(*                                                                       *)
(*                         ed2k_hash_filename                            *)
(*                                                                       *)
(*************************************************************************)
  
let ed2k_hash_filename filename = 
  let fd = Unix32.create_rw filename in
  let file_size = Unix32.getsize64 fd in
  let md4 = ed2k_hash_file fd file_size in
  lprintf "ed2k://|file|%s|%Ld|%s|\n" 
    filename
    file_size
    (Md4.to_string md4)

(*************************************************************************)
(*                                                                       *)
(*                         sig2dat_hash_filename                         *)
(*                                                                       *)
(*************************************************************************)
  
let sig2dat_hash_filename filename =
  let fd = Unix32.create_rw filename in
  let file_size = Unix32.getsize64 fd in
  let len64 = min (Int64.of_int 307200) file_size in
  let len = Int64.to_int len64 in
  let s = String.create len in
  Unix32.read fd zero s 0 len;
  let md5ext = Md5Ext.string s in
  lprintf "sig2dat://|File: %s|Length: %Ld Bytes|UUHash: %s|/\n"
    filename file_size (Md5Ext.to_string md5ext);
  lprintf "    Hash: %s\n" (Md5Ext.to_hexa_case false md5ext);
  ()

(*************************************************************************)
(*                                                                       *)
(*                         check_external_functions                      *)
(*                                                                       *)
(*************************************************************************)
  
let check_external_functions size = 
  let nchunks = 1024 in
(*
  let chunk_sizes = [
      10 (* 10 KB *); 1000 (* ~ 1 MB *) ; 
(*      50000 (* ~ 50 MB *) ;   *)
(*      100000 (* ~ 100 MB *); 3000000 (* ~ 3 GB *)  *)
    ]
  in *)
  let test_string_len = 43676 in
  let dummy_string = "bonjourhello1" in
  
  let create_diskfile filename size =
    Unix32.create_diskfile filename Unix32.rw_flag 0o066
  in
  let create_sparsefile filename size =
    Unix32.create_sparsefile filename
  in
  let create_multifile filename size =    
    let rec iter pos size list =
      if size <> zero then
        let new_size = (size // 2) ++ one in
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
      Unix32.rw_flag 0o066  files
  in    
  
  let (file_types : (string * (string -> int64 -> Unix32.t)
        * (string -> int64 -> Unix32.t) ) list) = 
    [
      "diskfile", create_diskfile, create_diskfile;
      
      "sparsefile", create_sparsefile, create_diskfile;
            
      "multifile", create_multifile, create_multifile;
    
    ] in
  
  let test_string = String.create test_string_len in
  let rec iter pos =
    if pos < test_string_len then
      let end_pos = min test_string_len (2*pos) in
      let len = end_pos - pos in        
      String.blit test_string 0 test_string pos len;
      iter end_pos
  in
  
  let dummy_string_len = String.length dummy_string in
  String.blit dummy_string 0 test_string 0 dummy_string_len;
  iter dummy_string_len;
  
  
  let test_string_len64 = Int64.of_int test_string_len in
  let file_size = (Int64.of_int size) ** 1024 in
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
  
  lprintf "Waves: \n";
  List.iter (fun (pos,len) ->
      lprintf " %Ld[%d]," pos len;
  ) waves;
  lprintf "\n";
  List.iter (fun (name,f,f') ->
      let filename = Printf.sprintf "test.%s.%d" name size in
      try
        lprintf "Creating file %s\n" filename;
        let file = f filename file_size in
        Unix32.ftruncate64 file file_size;

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
            Unix32.write file pos test_string 0 len;                
        ) waves;
        
        lprintf "Computing ed2k hash\n";
        let md4 = ed2k_hash_file file file_size in
        lprintf "ed2k://|file|%s|%Ld|%s|\n" 
          filename
          file_size
          (Md4.to_string md4);
        
        lprintf "Computing bitprint hash\n";
        let (sha1, tiger2) = bitprint_file file file_size in
        lprintf "urn:bitprint:%s.%s\n" (Sha1.to_string sha1) (TigerTree.to_string tiger2);
        
        lprintf "Renaming...\n";
        Unix32.rename file (filename ^ ".final");            
        
        lprintf "Removing %s\n" filename;
        Unix32.remove file;
        
        let file = f' (filename ^ ".final") file_size in
        Unix32.remove file;
        
        lprintf "done\n"
      with e ->
          lprintf "   **********    Exception %s in check_external_functions %s.%d KB\n"
            (Printexc2.to_string e) name size)
  file_types

(*************************************************************************)
(*                                                                       *)
(*                         MAIN                                          *)
(*                                                                       *)
(*************************************************************************)
  
let hash = ref ""
  
let _ =
  Arg.parse [
    "-hash", Arg.String ( (:=) hash), " <hash> : Set hash type you want to compute (ed2k, sig2dat,bp)";
    "-check", Arg.Int check_external_functions, " <nth size>: check C file functions";
  ] (fun filename ->
      match !hash with
      | "ed2k" -> ed2k_hash_filename filename
      | "sig2dat" -> sig2dat_hash_filename filename
      | "bp" -> bitprint_filename filename
      | _ -> 
          ed2k_hash_filename filename;
          sig2dat_hash_filename filename;
          bitprint_filename filename
  ) " <filenames> : compute hashes of filenames";
  