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

open Printf2
open CommonOptions
open GnutellaOptions
open Options
open Md4
open CommonGlobals
open LittleEndian
open TcpBufferedSocket
open AnyEndian
open GnutellaTypes



type ghandler =
  HttpHeader of (gconn -> TcpBufferedSocket.t -> string -> unit)
| Reader of (gconn -> TcpBufferedSocket.t -> unit)

and gconn = {
    mutable gconn_handler : ghandler;
    mutable gconn_refill : (TcpBufferedSocket.t -> unit) list;
    mutable gconn_close_on_write : bool;

(* If the connection is compressed, the TcpBufferedSocket.buf buffer
only contains the data that has not been decompressed yet. gconn.gconn_zout
contains the part that has been decompressed. If the socket is not
decompressed, the two buffers point to the same buffer.
  *)
    mutable gconn_deflate : bool;
    mutable gconn_zout : TcpBufferedSocket.buf;
  }
  
module type GnutellaProtocol = sig
    val handler : gconn -> TcpBufferedSocket.t -> unit
  end


module QrtReset = struct
(*
    struct gnutella_qrp_reset {
        guchar table_length[4]; /* little endian */
        guchar infinity;
} __attribute__((__packed__));
*)
    
    type t = {
        table_length : int;
        infinity : int;
      }
    
    let parse s = 
      { table_length = get_int s 0;
        infinity = get_int8 s 4;
      }
    
    let print t = 
      lprintf "QRT Reset %d %d" t.table_length t.infinity
    
    let write buf t = 
      buf_int buf t.table_length;
      buf_int8 buf t.infinity
  
  end

module QrtPatch = struct
(*
struct gnutella_qrp_patch {
        guchar seq_no;
        guchar seq_size;
        guchar compressor;
        guchar entry_bits;
} __attribute__((__packed__));
*)
    
    type t = {
        seq_no : int; (* char *)
        seq_size : int; (* char *)
        compressor : int; (* char *)
        entry_bits : int; (* char *)
        table : string;
      }
    
    let parse s = 
      {
        seq_no = get_int8 s 0;
        seq_size = get_int8 s 1;
        compressor = get_int8 s 2;
        entry_bits = get_int8 s 3;
        table = String.sub s 4 (String.length s - 4);
      }
    
    let print t = 
      lprintf "QRT PATCH"
    
    let write buf t = 
      buf_int8 buf t.seq_no;
      buf_int8 buf t.seq_size;
      buf_int8 buf t.compressor;
      buf_int8 buf t.entry_bits;
      Buffer.add_string buf t.table
  
  end

let gnutella_ok = "GNUTELLA OK"     
let gnutella_200_ok = "GNUTELLA/0.6 200 OK"
let gnutella_503_shielded = "GNUTELLA/0.6 503 I am a shielded leaf node"

let add_header_fields header sock trailer =
  let buf = Buffer.create 100 in
  Printf.bprintf buf "%s" header;
  Printf.bprintf buf "User-Agent: %s\r\n" user_agent;
  Printf.bprintf buf "X-My-Address: %s:%d\r\n"
    (Ip.to_string (client_ip (Some sock))) !!client_port;
  Printf.bprintf buf "X-Ultrapeer: False\r\n";
  Printf.bprintf buf "X-Query-Routing: 0.1\r\n";
  Printf.bprintf buf "GGEP: 0.5\r\n";
  Printf.bprintf buf "%s" trailer;
  Buffer.contents buf

let handlers info gconn =
  let rec iter_read sock nread =
    let b = TcpBufferedSocket.buf sock in
    if b.len > 0 then
      match gconn.gconn_handler with
      | HttpHeader h ->
          let end_pos = b.pos + b.len in
          let begin_pos =  b.pos in
          let rec iter i n_read =
            if i < end_pos then
              if b.buf.[i] = '\r' then
                iter (i+1) n_read
              else
              if b.buf.[i] = '\n' then
                if n_read then begin
                    let header = String.sub b.buf b.pos (i - b.pos) in
(*                    if info then begin
                        lprintf "HEADER : ";
                        dump header; lprint_newline ();
                      end; *)
                    h gconn sock header;
                    if not (TcpBufferedSocket.closed sock) then begin
                        let nused = i - b.pos + 1 in
                        buf_used sock nused;
                        iter_read sock 0
                      end
                  end else
                  iter (i+1) true
              else
                iter (i+1) false
          in
          iter begin_pos false
      | Reader h -> 
          let len = b.len in
          h gconn sock;
          if b.len < len then iter_read sock 0
  in
  iter_read


let set_gnutella_sock sock info ghandler = 
  let gconn = {
      gconn_handler = ghandler;
      gconn_refill = [];
      gconn_close_on_write = false;
      gconn_deflate = false;
      gconn_zout = TcpBufferedSocket.buf sock;
    } in
  TcpBufferedSocket.set_reader sock (handlers info gconn);
  TcpBufferedSocket.set_refill sock (fun sock ->
      match gconn.gconn_refill with
        [] -> ()
      | refill :: _ -> refill sock
  );
  TcpBufferedSocket.set_handler sock TcpBufferedSocket.WRITE_DONE (
    fun sock ->
      match gconn.gconn_refill with
        [] -> ()
      | _ :: tail -> 
          gconn.gconn_refill <- tail;
          match tail with
            [] -> 
              if gconn.gconn_close_on_write then 
                set_lifetime sock 30.
(*                TcpBufferedSocket.close sock "write done" *)
          | refill :: _ -> refill sock)


let parse_range range =
  try
    let npos = (String.index range 'b')+6 in
    let dash_pos = try String.index range '-' with _ -> -10 in
    let slash_pos = try String.index range '/' with _ -> -20 in
    let star_pos = try String.index range '*' with _ -> -30 in
    if star_pos = slash_pos-1 then
      Int64.zero, None, None (* "bytes */X" *)
    else
    let len = String.length range in
    let x = Int64.of_string (
        String.sub range npos (dash_pos - npos) )
    in
    if len = dash_pos + 1 then
(* bytes x- *)
      x, None, None
    else
    let y = Int64.of_string (
        String.sub range (dash_pos+1) (slash_pos - dash_pos - 1))
    in
    if slash_pos = star_pos - 1 then 
      x, Some y, None (* "bytes x-y/*" *)
    else
(* bytes x-y/len *)
    
    let z = Int64.of_string (
        String.sub range (slash_pos+1) (len - slash_pos -1) )
    in
    x, Some y, Some z
  with 
  | e ->
      lprintf "Exception %s for range [%s]\n" 
        (Printexc2.to_string e) range;
      raise e

let parse_range range =
  let x, y, z = parse_range range in
  lprintf "Range parsed: %Ld-%s/%s" x
    (match y with None -> "" | Some y -> Int64.to_string y)    
  (match z with None -> "*" | Some y -> Int64.to_string y);
  x, y, z
    
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

  
module WordSet = Set.Make(struct
      type t = string
      let compare = compare
    end)
  
let new_shared_words = ref false
let all_shared_words = ref []
                
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
