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

open CommonSwarming
open Printf2
open Md4
open CommonOptions
open CommonSearch
open CommonServer
open CommonComplexOptions
open CommonFile
open BasicSocket
open TcpBufferedSocket

open CommonTypes
open CommonGlobals
open Options
open GnutellaTypes
open GnutellaGlobals
open GnutellaOptions
open GnutellaProtocol
open GnutellaComplexOptions

module DG = CommonGlobals
module DO = CommonOptions

(* Here, we put functions that, depending on the server type, call
either the corresponding Gnutella1 or Gnutella2 function
*)

let server_send_qrt_reset s = 
  if s.server_gnutella2 then
    Gnutella2.server_send_qrt_reset s
  else
    Gnutella1.server_send_qrt_reset s
    
let server_send_qrt_patch s = 
  if s.server_gnutella2 then
    Gnutella2.server_send_qrt_patch s
  else
    Gnutella1.server_send_qrt_patch s
    
let server_send_query s uid words xml_query = 
  if s.server_gnutella2 then
    Gnutella2.server_send_query s uid words xml_query
  else
    Gnutella1.server_send_query s uid words xml_query  
    
let server_send_ping s = 
  if s.server_gnutella2 then
    Gnutella2.server_send_ping s
  else
    Gnutella1.server_send_ping s
  
let server_send_push s uid uri = 
  if s.server_gnutella2 then
    Gnutella2.server_send_push s uid uri  
  else
    Gnutella1.server_send_push s uid uri  
    
let bloom_hash_magic = Int32.of_string  "0x4F1BBCDC"
let bloom_hash_magic_int64 =  Int32ops.int64_of_uint32 bloom_hash_magic

let bloom_hash_fast x bits =
  let xx = Int32ops.int64_of_uint32 x in
  let prod = Int64.mul xx bloom_hash_magic_int64 in
  let ret = Int64.shift_left prod  32 in     (* prod << 32 *)
  Int64.shift_right_logical ret (32 + (32 - bits))   (* ret >>> (32 + (32 - bits))  *)

let bloom_hash_full s pos len bits =
  let xor = ref Int32.zero in
  let j = ref 0 in
  for i = pos to len - 1 do
    let b = Int32.of_int (int_of_char (Char.lowercase s.[i])) in
    let b = Int32.shift_left b (!j * 8) in
    xor := Int32.logxor !xor b;
    j := (!j+1) mod 4;
  done;
  bloom_hash_fast !xor bits
  
let bloom_hash s bits = bloom_hash_full s 0 (String.length s) bits
  
let create_qrt_table words table_size =
  let infinity = 7 in
  let table_length = 1 lsl table_size in
  let old_array = Array.create table_length infinity in
  let array = Array.create table_length infinity in
  List.iter (fun w ->
      let pos = bloom_hash w table_size in
      lprintf "Position %Ld\n" pos;
      array.(Int64.to_int pos) <- 1;
  ) words;
  let string_size = table_length/2 in
  let table = String.create  string_size in
  for i = 0 to string_size - 1 do
    table.[i] <- char_of_int (
      (
        ((array.(i*2) - old_array.(i*2)) land 15) lsl 4) + 
      ((array.(i*2+1) - old_array.(i*2+1)) land 15))
  done;
  table
(*  
   create_qrt_table ["qrp"] 3;;
- : string = "\000\000\000\160"
   create_qrt_table ["test"] 3;;


*)

module WordSet = Set.Make(struct
      type t = string
      let compare = compare
    end)
  
let new_shared_words = ref false
let all_shared_words = ref []
let cached_qrt_table = ref ""
                
let extension_list = [
    "mp3" ; "avi" ; "jpg" ; "jpeg" ; "txt" ; "mov" ; "mpg" 
]
      
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

let stem s =
  let s = String.lowercase (String.copy s) in
  for i = 0 to String.length s - 1 do
    match s.[i] with
      'a'..'z' | '0' .. '9' -> ()
    | _ -> s.[i] <- ' '
  done;
  lprintf "STEM %s\n" s;
  remove_short (String2.split s ' ') []

let update_shared_words () = 
  all_shared_words := [];
  let module M = CommonUploads in
  let words = ref WordSet.empty in
  let register_words s = 
    let ws = stem s in
    List.iter (fun w ->
        words := WordSet.add w !words
    ) ws
  in
  let rec iter node =
    List.iter (fun sh ->
        lprintf "CODED name: %s\n" sh.M.shared_codedname;
        register_words sh.M.shared_codedname;
    ) node.M.shared_files;
    List.iter (fun (_,node) ->
        register_words node.M.shared_dirname;
        iter node
    ) node.M.shared_dirs;
  in
  iter M.shared_tree;
  WordSet.iter (fun s ->
      all_shared_words := s :: !all_shared_words
  ) !words;
  lprintf "SHARED WORDS: ";
  List.iter (fun s ->
      lprintf "%s " s
  ) !all_shared_words;
  lprint_newline ()
  
  
let send_qrt_sequence s =
  
  if !new_shared_words then begin
      update_shared_words ();
      new_shared_words := false;
    end;
  
  let table_size = 10 in
  let infinity = 7 in
  let table_length = 1 lsl table_size in
  server_send_qrt_reset s {
      QrtReset.table_length = table_length;
      QrtReset.infinity = infinity;
    };
  
  if !cached_qrt_table = "" then 
        cached_qrt_table := create_qrt_table !all_shared_words table_size;
  let table = !cached_qrt_table in
  
  let compressor, table =
    if Autoconf.has_zlib then
      1, Autoconf.zlib__compress_string table
    else
      0, table 
  in
  
  server_send_qrt_patch s {
      QrtPatch.seq_no = 1;
      QrtPatch.seq_size = 1;
      QrtPatch.compressor = compressor;
      QrtPatch.entry_bits = 4;
      QrtPatch.table = table;
    }
  
let send_query servers words xml_query =
  if !verbose_msg_servers then begin
      lprintf "sending query for <%s>\n" words;
    end;
  let uid = Md4.random () in
  List.iter (fun s ->
      server_send_query s uid words xml_query
  ) servers;
  uid

  
let get_name_keywords file_name =
  match stem file_name with 
    [] | [_] -> 
      lprintf "Not enough keywords to recover %s\n" file_name;
      [file_name]
  | l -> l
      
let gen_query file servers =
  if file.file_uids = [] then
    let keywords = get_name_keywords file.file_name in
    let words = String2.unsplit keywords ' ' in
    ignore (send_query servers words "urn:")
  else
    List.iter (fun uid ->
        let xml_query = 
          (string_of_uid uid)          
        in
        ignore (send_query servers "" xml_query)
    ) file.file_uids    
    
    
let recover_files () =
  List.iter (fun file ->
      gen_query file !connected_servers
  ) !current_files;
  ()
  
let recover_files_from_server s =
  if !verbose_msg_servers then begin
      lprintf "trying to recover files from server\n";
    end;
  List.iter (fun file ->
      gen_query file [s]
  ) !current_files;
  ()
            

      
let disconnect_from_server s =
  match s.server_sock with
    None -> ()
  | Some sock ->
(*
  lprintf "DISCONNECT FROM SERVER %s:%d\n" 
        (Ip.to_string s.server_ip) s.server_port;
  *)
      close sock "timeout";
      s.server_sock <- None;
      set_server_state s (NotConnected (-1));
      decr nservers;
      s.server_need_qrt <- true;
      if List.memq s !connected_servers then begin
          connected_servers := List2.removeq s !connected_servers;
        end;
      server_remove s
      
let find_header header headers default =
  try
    List.assoc header headers
  with Not_found -> default

let add_uid r uid =
  if not (List.mem uid r.result_uids) then
    r.result_uids <- uid :: r.result_uids
  
