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
let ( // ) x y = Int64.div x y
  
let block_size = Int64.of_int 9728000

let tiger_block_size = Int64.of_int (1024 * 1024)
    
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
  
  
let bitprint_filename filename =
  let fd = Unix32.create filename  [Unix.O_RDONLY] 0o666 in
  let file_size = Unix32.getsize64 filename in
  let sha1 = Sha1.digest_subfile fd zero file_size in
  let tiger = TigerTree.digest_subfile fd zero file_size in
  lprintf "urn:bitprint:%s.%s\n" (Sha1.to_string sha1) (TigerTree.to_string tiger);
  let file_size = Unix32.getsize64 filename in
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
  lprintf "urn:bitprint:%s.%s\n" (Sha1.to_string sha1) (TigerTree.to_string tiger2);
  ()
  
let ed2k_hash_filename filename = 
  let fd = Unix32.create filename  [Unix.O_RDONLY] 0o666 in
  let file_size = Unix32.getsize64 filename in
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
  lprintf "ed2k://|file|%s|%Ld|%s|\n" 
    filename
    file_size
    (Md4.to_string md4)
  
let sig2dat_hash_filename filename =
  let md5ext = Md5Ext.file filename in
  let file_size = Unix32.getsize64 filename in
  lprintf "sig2dat://|File: %s|Length: %Ld Bytes|UUHash: %s|/\n"
    filename file_size (Md5Ext.to_string md5ext);
  lprintf "    Hash: %s\n" (Md5Ext.to_hexa_case false md5ext);
  ()
    
let hash = ref ""
  
let _ =
  Arg.parse [
    "-hash", Arg.String ( (:=) hash), " <hash> : Set hash type you want to compute (ed2k, sig2dat,bp)";
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
  