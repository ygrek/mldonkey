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
open BasicSocket
open Printf2
open Options
open Md4
open LittleEndian
open AnyEndian
open TcpBufferedSocket

open CommonOptions
open CommonGlobals

open FasttrackOptions
open FasttrackTypes
open FasttrackGlobals

type ghandler =
  HttpHeader of (gconn -> TcpBufferedSocket.t -> string -> unit)
| Reader of (gconn -> TcpBufferedSocket.t -> unit)
| CipherReader of (cipher * (gconn -> TcpBufferedSocket.t -> unit))

and gconn = {
    mutable gconn_handler : ghandler;
    mutable gconn_refill : (TcpBufferedSocket.t -> unit) list;
    mutable gconn_close_on_write : bool;
  }
  
module type FasttrackProtocol = sig
    val handler : gconn -> TcpBufferedSocket.t -> unit
  end

let handlers info gconn =
  let rec iter_read sock nread =
(*    lprintf "iter_read %d\n" nread;  *)
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
                    
                    (try h gconn sock header with
                        e -> close sock (Closed_for_exception e));
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
          let len = b.len in
          h gconn sock;
          if b.len < len then iter_read sock b.len
      | CipherReader (cipher, h) ->
(*          lprintf "CipherReader %d: [%s]\n" nread
            (String.escaped (String.sub b.buf b.pos b.len)); *)
          if nread > 0 then begin
(*              AnyEndian.dump_sub b.buf (b.pos + b.len - nread) nread; *)
              apply_cipher cipher b.buf (b.pos + b.len - nread) nread;
            end;
          let len = b.len in
          h gconn sock;
          if b.len < len then iter_read sock 0
          
  in
  iter_read

let set_fasttrack_sock sock info ghandler = 
  let gconn = {
      gconn_handler = ghandler;
      gconn_refill = [];
      gconn_close_on_write = false;
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
            let pbuf = p.UdpSocket.udp_content in
            let len = String.length pbuf in
            f p
          with e ->
              lprintf "Error %s in udp_handler\n"
                (Printexc2.to_string e); 
      ) ;
  | _ -> ()

let len5 = Int64.of_int (128*128*128*128)
let len4 = Int64.of_int (128*128*128)
let len3 = Int64.of_int (128*128)
let len2 = Int64.of_int (128)
  
let int64_7f = Int64.of_int 0x7f
let int64_80 = Int64.of_int 0x80
  
let buf_dynint b data =
  let buf = String.create 6 in

  let len = 
    if data > len5 then 5 else
    if data > len4 then 4 else
    if data > len3 then 3 else
    if data > len2 then 2 else 1
  in
  let i = len - 1 in

  (* last byte doesn't have high bit set *)
  buf.[i] <-  char_of_int (Int64.to_int (Int64.logand data int64_7f));
  let data = ref (Int64.shift_right_logical data  7) in
  
  for i = i - 1 downto 0 do
    buf.[i] <- char_of_int (Int64.to_int
        (Int64.logor int64_80 (Int64.logand !data int64_7f)));
    data := Int64.shift_right_logical !data  7;
  done;
  Buffer.add_string b (String.sub buf 0 len)

let dynint v =
  let b = Buffer.create 10 in
  buf_dynint b v;
  Buffer.contents b
  
  
let get_dynint s pos =
  let len = String.length s in
  let rec iter len pos ret =
    if pos < len then
      let i = int_of_char s.[pos] in
      let ret = Int64.logor (Int64.shift_left ret 7) 
        (Int64.of_int (i land 0x7f)) in
      if i land 0x80 <> 0 then 
        iter len (pos+1) ret
      else
        ret, pos+1
    else 
      ret, len
  in
  iter len pos zero
