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
open Options
open Md4
  
open LittleEndian
open TcpBufferedSocket
open AnyEndian

open CommonTypes
open CommonGlobals
open CommonOptions

open Gnutella2Options
open Gnutella2Types
open Gnutella2Globals


type ghandler =
  HttpHeader of (gconn -> TcpBufferedSocket.t -> string -> unit)
| Reader of (gconn -> TcpBufferedSocket.t -> unit)

and gconn = {
    mutable gconn_handler : ghandler;
    mutable gconn_refill : (tcp_connection -> unit) list;
    mutable gconn_close_on_write : bool;
    mutable gconn_sock : tcp_connection;
  }
  
module type Gnutella2Protocol = sig
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
    
    let patches = ref ""
    
    let print buf t = 
      Printf.bprintf buf "QRT PATCH:\n";
      Printf.bprintf buf "  seq_no: %d/%d\n" t.seq_no t.seq_size;
      Printf.bprintf buf "  compressor: %d\n" t.compressor;
      Printf.bprintf buf "  entry bits: %d\n" t.entry_bits;
      Printf.bprintf buf "  table: ";
      if t.seq_no = 1 then patches := t.table
      else patches := !patches ^ t.table;
      if t.seq_no = t.seq_size then begin
          if t.compressor < 2 then
            let table = 
              if t.compressor = 1 then
                Autoconf.zlib__uncompress_string2 !patches
              else
                !patches
            in
            let nbits = ref 0 in
            for i = 0 to String.length table - 1 do
              let c = int_of_char table.[i] in
              for j = 0 to 7 do
                if (1 lsl j) land c <> 0 then begin
                    incr nbits;
                    Printf.bprintf buf "(%d)" (i*8+j);
                  end
              done
            done;
            Printf.bprintf buf "  = %d bits\n" !nbits               
          else
            Printf.bprintf buf " (compressed) \n"
        end else
        Printf.bprintf buf " (partial) \n"            
        
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
    (Ip.to_string (client_ip (Connection sock))) !!client_port;
  Printf.bprintf buf "X-Ultrapeer: False\r\n";
  Printf.bprintf buf "X-Query-Routing: 0.1\r\n";
  Printf.bprintf buf "GGEP: 0.5\r\n";
  Printf.bprintf buf "%s" trailer;
  Buffer.contents buf

let handlers info gconn =
  let rec iter_read sock nread =
(*    lprintf "iter_read %d\n" nread; *)
    let b = CanBeCompressed.buf gconn.gconn_sock in
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
                    
                    (try h gconn sock header with
                        e -> close sock 
			(BasicSocket.Closed_for_exception e));
                    if not (TcpBufferedSocket.closed sock) then begin
                        let nused = i - b.pos + 1 in
(*                        lprintf "HEADER: buf_used %d\n" nused; *)
                        buf_used b nused;
                        iter_read sock 0
                      end
                  end else
                  iter (i+1) true
              else
                iter (i+1) false
          in
          iter begin_pos false
      | Reader h -> 
          lprintf "Reader\n";
          let len = b.len in
          h gconn sock;
          if b.len < len then iter_read sock 0
  in
  iter_read


let set_gnutella_sock conn info ghandler = 
  match conn with
    ConnectionAborted|ConnectionWaiting|NoConnection -> assert false
  | Connection sock | CompressedConnection (_,_,_,sock) ->
      let gconn = {
          gconn_handler = ghandler;
          gconn_refill = [];
          gconn_close_on_write = false;
          gconn_sock = conn;
        } in
      TcpBufferedSocket.set_reader sock (handlers info gconn);
      TcpBufferedSocket.set_refill sock (fun sock ->
          match gconn.gconn_refill with
            [] -> ()
          | refill :: _ -> 
(* BUG: here, we should immediatly flush the data, and not wait for
the timer in CanBeCompressed *)
              refill gconn.gconn_sock
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
              | refill :: _ -> refill gconn.gconn_sock)
      

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

  
let udp_handler f sock event =
  match event with
    UdpSocket.READ_DONE ->
      UdpSocket.read_packets sock (fun p -> 
          try
            let pbuf = p.UdpSocket.content in
            let len = String.length pbuf in
            f p
          with e ->
              lprintf "Error %s in udp_handler\n"
                (Printexc2.to_string e); 
      ) ;
  | _ -> ()

      