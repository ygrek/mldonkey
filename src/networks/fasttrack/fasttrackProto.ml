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

open BasicSocket
open AnyEndian
open Printf2
open Options
open Md4
open TcpBufferedSocket

open CommonGlobals
open CommonTypes
open CommonOptions
  
open FasttrackOptions
open FasttrackTypes
open FasttrackProtocol
open FasttrackGlobals

let server_crypt_and_send s out_cipher str =
  let str = String.copy str in
  apply_cipher out_cipher str 0 (String.length str);
  match s.server_sock with
  | Connection sock ->
      write_string sock str
  | _ -> assert false

let int64_3 = Int64.of_int 3
let int64_ffffffff = Int64.of_string "0xffffffff"
  
let server_send s msg_type m =
  match s.server_ciphers with
    None -> assert false
  | Some ciphers ->
      let size = String.length m in
      let lo_len = size land 0xff in
      let hi_len = (size lsr 8) land 0xff in
      
      let b = Buffer.create 100 in
      buf_int8 b 0x4B; (* 'K' *)
      
      let xtype = Int64.to_int (Int64.rem ciphers.out_xinu int64_3) in
      
      let _ = match xtype with
        
        | 0 ->
            buf_int8 b msg_type;
            buf_int8 b 0;
            buf_int8 b hi_len;
            buf_int8 b lo_len;
        | 1 ->
            buf_int8 b 0;
            buf_int8 b hi_len;
            buf_int8 b msg_type;
            buf_int8 b lo_len;
        | _ ->
            buf_int8 b 0;
            buf_int8 b lo_len;
            buf_int8 b hi_len;
            buf_int8 b msg_type;
      in
      
(* update xinu state *)
      ciphers.out_xinu <- Int64.logxor ciphers.out_xinu  
        (Int64.logand
          (Int64.lognot (Int64.of_int (size + msg_type))) 
        int64_ffffffff);

      Buffer.add_string b m;
      let m = Buffer.contents b in
      server_crypt_and_send s ciphers.out_cipher m

let server_send_ping s = 
  let m = "\080" in (* 0x50 = PING *)
  match s.server_ciphers with
    None -> assert false
  | Some ciphers ->
      lprintf "   ******* sending PING\n";
      server_crypt_and_send s ciphers.out_cipher m

let server_send_pong s = 
  let m = "\082" in (* 0x52 = PONG *)
  match s.server_ciphers with
    None -> assert false
  | Some ciphers ->
      lprintf "   ******* sending PONG\n";
      server_crypt_and_send s ciphers.out_cipher m
  
  
let server_send_query s ss = 
  match ss.search_search with
    UserSearch (sss, words, realm, tags) ->
      
      lprintf "UserSearch [%s] for %d\n" words ss.search_id;
      let b = Buffer.create 100 in
      Buffer.add_string b "\000\001";

(* max search results *)
      BigEndian.buf_int16 b 100;
(* search id *)
      BigEndian.buf_int16 b ss.search_id;
(* dunno what this is *)
      buf_int8 b 0x01;

(* realm is video/..., audio/..., and strings like that. Avoid them currently.*)
      buf_int8 b (match realm with
          "audio" -> 0x21
        | "video" -> 0x22
        | "image" -> 0x23
        | "text" -> 0x24
        | "application" -> 0x25
        | _ -> 0x3f);

(* number of search terms *)
      buf_int8 b (
        (if words <> "" then 1 else 0)
        + List.length tags);
      
      if words <> "" then begin
(* cmp type of first term *)
          buf_int8 b   0x05; (* QUERY_CMP_SUBSTRING *)
(* field to cmp of first term *)
          buf_int8 b 0;  (* FILE_TAG_ANY *)
(* length of query string *)
          buf_dynint b (Int64.of_int (String.length words));
(* query string *)
          Buffer.add_string b words;
        end;
      List.iter (fun term ->
          let code, tag, s =
            match term with
              AtMost (tag,v)  -> 2, tag, dynint v
            | AtLeast (tag, v) -> 4, tag, dynint v
            | Substring (tag, v) -> 5, tag, v
          in
          buf_int8 b code;
          buf_int8 b (List.assoc tag name_of_tag);
          buf_dynint b (Int64.of_int (String.length s));
          Buffer.add_string b s;
              
      ) tags;
      
      let m = Buffer.contents b in
      lprintf "Sending Query\n";
      dump m;
      server_send s 0x06 m
      
  | FileSearch file -> 
      
      lprintf "FileSearch [%s] for %d\n" file.file_name ss.search_id;
      let b = Buffer.create 100 in
      Buffer.add_string b "\000\001";

(* max search results *)
      BigEndian.buf_int16 b 100;
(* search id *)
      BigEndian.buf_int16 b ss.search_id;
(* dunno what this is *)
      buf_int8 b 0x01;

(* realm is video/..., audio/..., and strings like that. Avoid them currently.*)
      buf_int8 b 191; (* from one example ... *)
      
(* number of search terms *)
      buf_int8 b 0x02;

(* if(search->type == SearchTypeSearch) *)
(* cmp type of first term *)
      buf_int8 b   0; (* EQUAL *)
(* field to cmp of first term *)
      buf_int8 b 3;  (* HASH *)
(* length of query string *)
      buf_int8 b 20;
      Buffer.add_string b (Md5Ext.direct_to_string file.file_hash);
      Buffer.add_string b "\006\037\001\002"; (* ???? *)
      let m = Buffer.contents b in
      lprintf "Sending Query\n";
      dump m;
      server_send s 0x06 m
      
  | _ -> ()
