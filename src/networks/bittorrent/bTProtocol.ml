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

open BigEndian
open TcpBufferedSocket
open AnyEndian
  
open CommonTypes
open CommonOptions
open CommonGlobals
open CommonClient
  
open BTTypes
open BTOptions
  
type ghandler =
  BTHeader of (gconn -> TcpBufferedSocket.t -> 
  (string * Sha1.t * Sha1.t) -> unit)
| Reader of (gconn -> TcpBufferedSocket.t -> unit)

and gconn = {
    mutable gconn_handler : ghandler;
    mutable gconn_refill : (TcpBufferedSocket.t -> unit) list;
    mutable gconn_close_on_write : bool;
  }

type msg = 
| Choke
| Unchoke
| Interested
| NotInterested
| Have of int64
| BitField of string
| Request of int * int64 * int64
| Piece of int * int64 * string * int * int
| Cancel of int64 * int64 * int64
| Ping  

  
let bt_print msg =
  match msg with
  | Choke -> lprintf "Choke\n"
  | Unchoke -> lprintf "Unchoke\n"
  | Interested -> lprintf "Interested\n"
  | NotInterested -> lprintf "NotInterested\n"
  | Have n -> lprintf "Have %Ld\n" n
  | BitField s -> lprintf "BitField %s\n" (String.escaped s)
  | Request (index, offset, len) -> 
      lprintf "Request %d %Ld[%Ld]\n" index offset len
  | Piece (index, offset, s, pos, len) -> 
      lprintf "Piece %d %Ld[%d]\n" index offset len
  | Cancel _ -> lprintf "Cancel\n"
  | Ping   -> lprintf "Ping\n"
    
let bt_parser opcode m = 
  match opcode with
    0 -> Choke
  | 1 -> Unchoke
  | 2 -> Interested
  | 3 -> NotInterested
  | 4 -> Have (get_int64_32 m 0)
  | 5 -> BitField m
  | 6 -> Request (get_int m 0, get_int64_32 m 4, get_int64_32 m 8)
  | 7 -> Piece (get_int m 0, get_int64_32 m 4, m, 8, String.length m - 8)
  | _ -> raise Not_found
  
let bt_handler parse_fun handler sock =
    try
      let b = TcpBufferedSocket.buf sock in
      (*
    lprintf "BT HANDLER\n";
dump (String.sub b.buf b.pos b.len);
  *)
    try
      while b.len >= 4 do
        let msg_len = get_int b.buf b.pos in
        if b.len >= 4 + msg_len then
          begin
              TcpBufferedSocket.buf_used b 4;
(*              lprintf "Message complete\n"; *)
            if msg_len > 0 then 
              let opcode = get_int8 b.buf b.pos in
              let payload = String.sub b.buf (b.pos+1) (msg_len-1) in
              TcpBufferedSocket.buf_used b msg_len;
(*              lprintf "Opcode %d\n" opcode; *)
                try
                  let p = parse_fun opcode payload in
(*              lprintf "Parsed, calling handler\n"; *)
                  handler sock p
                with Not_found -> ()
          end
        else raise Not_found
      done
    with 
    | Not_found -> ()
    with e ->
        lprintf "Exception %s in bt_handler\n"
          (Printexc2.to_string e)
        
let handlers info gconn =
  let rec iter_read sock nread =
(*    lprintf "iter_read %d\n" nread; *)
    let b = TcpBufferedSocket.buf sock in
    if b.len > 0 then
      match gconn.gconn_handler with
      | BTHeader h ->
          
(*          dump (String.sub b.buf b.pos (min b.len 100)); *)
          let slen = get_int8 b.buf b.pos in
          if slen + 49 <= b.len then
            let proto = String.sub b.buf (b.pos+1) slen in
            let file_id = Sha1.direct_of_string 
                (String.sub  b.buf (b.pos+9+slen) 20) in
            let peer_id = Sha1.direct_of_string 
                (String.sub  b.buf (b.pos+29+slen) 20) in
            let proto,pos = get_string8 b.buf b.pos in
            TcpBufferedSocket.buf_used b (slen+49);
            h gconn sock (proto, file_id, peer_id);
            if not (TcpBufferedSocket.closed sock) then 
              iter_read sock 0

      | Reader h -> 
          let len = b.len in
          h gconn sock;
          if b.len < len then iter_read sock 0
  in
  iter_read
  
let set_bt_sock sock info ghandler = 
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
              
      
  
(*  
No payload:
    * 0 - choke: you have been blocked
    * 1 - unchoke: you have been unblocked
    * 2 - interested: I'm interested in downloading this file now
    * 3 - not interested: I'm not interested in downloading this file now
With bencoded payload:
    * 4 - have
          int : index of new completed chunk          
    * 5 - bitfield: 
          string: a bitfield of bit 1 for downloaded chunks
          byte: bits are inverted 0....7 ---> 7 .... 0      
    * 6 - request
          int: index
          int: begin
          int: length (power of 2, 2 ^ 15)
    * 7 - piece
          int: index
          int: begin
          string: piece
    * 8 - cancel: cancel a requesu
          int: index
          int: begin
          int: length (power of 2, 2 ^ 15)
*)

let buf = Buffer.create 100
let send_client c msg =
  try
    match c.client_sock with 
      NoConnection | ConnectionWaiting | ConnectionAborted -> 
        failwith "Client is not connected\n"
(*      lprintf "send_client: not connected\n";        *) 
    | Connection sock ->
        Buffer.clear buf;
(*        lprintf "send_client\n";         *)
        begin
          buf_int buf 0;
          match msg with
          | Choke -> buf_int8 buf 0
          | Unchoke -> buf_int8 buf 1
          | Interested -> buf_int8 buf 2
          | NotInterested -> buf_int8 buf 3
          | Have i -> buf_int8 buf 4; buf_int64_32 buf i
          | BitField string -> buf_int8 buf 5; Buffer.add_string buf string
          | Request (index, pos, len) ->
              buf_int8 buf 6; 
              buf_int buf index; buf_int64_32 buf pos; buf_int64_32 buf len
          | Piece (num, index, s, pos, len) ->
              buf_int8 buf 7; 
              buf_int buf num;
              buf_int64_32 buf index; 
              Buffer.add_substring buf s pos len
          
          | Cancel _ -> ()
          | Ping -> ()          
        end;
        let s = Buffer.contents buf in
        str_int s 0 (String.length s - 4);
        if !verbose_msg_clients then begin
            lprintf "CLIENT %d: Sending " (client_num c);
            bt_print msg;
          end;
(*        dump s; *)
        write_string sock s
    | CompressedConnection (comp, rbuf, wbuf, sock) -> 
        lprintf "CompressedConnection not implemented\n";
        assert false
  
  with e ->
      lprintf "CLIENT %d: Error %s in send_client\n"  (client_num c)
        (Printexc2.to_string e)
      
      
let zero8 = String.make 8 '\000'
  
let send_init file c sock = 
  let buf = Buffer.create 100 in
  buf_string8 buf  "BitTorrent protocol";
  Buffer.add_string buf zero8;
  Buffer.add_string buf (Sha1.direct_to_string file.file_info.file_info_id);
  Buffer.add_string buf (Sha1.direct_to_string c.client_uid);
  let s = Buffer.contents buf in
  write_string sock s
  
