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

let _ =
  Arg.parse [] (fun filename ->
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
  ) " <filenames> : compute hashes of filenames"