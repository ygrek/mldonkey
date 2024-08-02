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

open Sys
open Int64ops
open Gettext
open Md4
open LittleEndian
open Unix
open Printf2

let _s x = _s "Get_range" x
let _b x = _b "Get_range" x  

(*************************************************************************)
(*                                                                       *)
(*                         tiger_of_array                                *)
(*                                                                       *)
(*************************************************************************)

  
  
(*************************************************************************)
(*                                                                       *)
(*                         MAIN                                          *)
(*                                                                       *)
(*************************************************************************)

let usage () =
  lprintf "Bad number of arguments: get_range [(range|rangex) <begin_pos> <end_pos> <file_num> | size] <filename>\n";
  exit 2
  
let _ =
  try  
    if Array.length Sys.argv < 2 then usage ();
    match Sys.argv.(1) with
      "size" -> 
        if Array.length Sys.argv <> 3 then usage ();
        let filename = argv.(2) in
        Printf.printf "[SIZE %Ld]\n" (Unix32.getsize filename)
        
    | "range" ->
        if Array.length Sys.argv <> 6 then usage ();
        let begin_pos = Int64.of_string argv.(2) in
        let end_pos = Int64.of_string argv.(3) in
        let file_num = argv.(4) in
        let filename = argv.(5) in
        let t = Unix32.create_ro filename in
        
        let segment = kilobytes 10 in
        let bufferlen = Int64.to_int segment in
        let buffer = String.create bufferlen in
        let rec iter begin_pos end_pos =
          let len = end_pos -- begin_pos in
          if len > zero then
            let len64 = min segment len in
            let len = Int64.to_int len64 in
            Unix32.read t begin_pos buffer 0 len;
            let encoded = Base64.encode_substring (Bytes.unsafe_to_string buffer) 0 len in
            Printf.printf "[SEGMENT %s %Ld %d %d]\n" file_num begin_pos len (
              String.length encoded);
            output_string Pervasives.stdout encoded;
            Printf.printf "\n[/SEGMENT]\n";
            iter (begin_pos ++ len64) end_pos
        in
        iter begin_pos end_pos
        
    | "rangex" ->
        if Array.length Sys.argv <> 6 then usage ();
        let begin_pos = Int64.of_string argv.(2) in
        let end_pos = Int64.of_string argv.(3) in
        let file_num = argv.(4) in
        let filename = argv.(5) in
        let t = Unix32.create_ro filename in
        
        let segment = kilobytes 10 in
        let bufferlen = Int64.to_int segment in
        let buffer = String.create bufferlen in
        let rec iter begin_pos end_pos =
          let len = end_pos -- begin_pos in
          if len > zero then
            let len64 = min segment len in
            let len = Int64.to_int len64 in
            Unix32.read t begin_pos buffer 0 len;
            let encoded = Bytes.sub_string buffer 0 len in
            Printf.printf "[SEGMENT 8bits %s %Ld %d %d]\n" file_num begin_pos len (
              String.length encoded);
            output_string Pervasives.stdout encoded;
            Printf.printf "[/SEGMENT]\n";
            iter (begin_pos ++ len64) end_pos
        in
        iter begin_pos end_pos
        
    | _ -> usage ()
  with e ->
      lprintf "Exception %s\n" (Printexc2.to_string e);
      exit 2
      
