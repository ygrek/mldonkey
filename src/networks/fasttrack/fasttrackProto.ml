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
    UserSearch (sss, words, exclude, realm) ->
      
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
      buf_int8 b 0x01;

(* if(search->type == SearchTypeSearch) *)
(* cmp type of first term *)
      buf_int8 b   0x05; (* QUERY_CMP_SUBSTRING *)
(* field to cmp of first term *)
      buf_int8 b 0;  (* FILE_TAG_ANY *)
(* length of query string *)
      buf_dynint b (Int64.of_int (String.length words));
(* query string *)
      Buffer.add_string b words;

(*
	else if(search->type == SearchTypeLocate)
	{
		unsigned char hash[FST_HASH_LEN];
		// convert hash string to binary 
		if(fst_hash_set_string (hash, search->query) == FALSE)
		{
			fst_packet_free (packet);
			return FALSE;
		}

		// cmp type of first term
		fst_packet_put_uint8 (packet, (fst_uint8)QUERY_CMP_EQUALS);
		// field to cmp of first term
		fst_packet_put_uint8 (packet, (fst_uint8)FILE_TAG_HASH);
		// length of query string
		fst_packet_put_dynint (packet, FST_HASH_LEN);
		// query string
		fst_packet_put_ustr (packet, hash, FST_HASH_LEN);
	}
*)
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
      
      (*(match realm with
          "audio" -> 0x21
        | "video" -> 0x22
        | "image" -> 0x23
        | "text" -> 0x24
        | "application" -> 0x25
        | _ -> 0x3f);
*)
      
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

(*
  Sending Query
ascii: [(0)(1)(0) d(0)(0)(1) ?(1)(5)(0) a l b u m]
dec: [(0)(1)(0)(100)(0)(0)(1)(63)(1)(5)(0)(97)(108)(98)(117)(109)]
*)
      
(*
Query For 58412f398fa7cb2ce782213e66ab1838dbddfb28

  dec: [(0)(0)(0)(10)(93)(58)(1)(191)(2)(0)(3)(20)(88)(65)(47)(57)(143)(167)(203)(44)(231)(130)(33)(62)(102)(171)(24)(56)(219)(221)(251)(40)(6)(37)(1)(2)]

          
      Message 6:
SessMsgQuery (len 36)
ascii: [(0)(0)(0)(10) ] :(1)(191)(2)(0)(3)(20) X A / 9(143)(167)(203) ,(231)(130) ! > f(171)(24) 8(219)(221)(251) ((6) %(1)(2)]
dec: [
(0)(0) search header
(0)(10) max results
(93)(58) search number
(1)  unknown
(191) realm
(2) search terms
(0) comp QUERY_CMP_EQUALS
(3) tag  FILE_TAG_HASH
(20) len of string
(88)(65)(47)(57)(143)(167)(203)(44)(231)(130)(33)(62)(102)(171)(24)(56)(219)(221)(251)(40)
(6) comp
(37) tag
(1) len
(2) string
]
  ascii: [(0)(1)(0)(200)(211)(185)(1)(161)(3)(5)(0)(9) e l l i s   m p 3(4) !(3)(159)(160)(0)(6) %(1)(2)]

(0)(1)
(0)(200)
(211)(185)
(1)
(161)
(3) terms
(5) SUBSTRING
(0) FILE_TAG_ANY
(9) ellis mp3
(4) AT_LEAST
(33) FILE_TAG_SIZE
(3)(159)(160)(0) dynint = 512 kB
(6) ????????
(37) ??????
(1)
(2)
]

  ascii: [(0)(1)(0)(200) C a(1)(162)(3)(5)(0)(3) x x x(2) !(4)(132)(226)(173)(0)(6) %(1)(2)]
dec: 
[
(0)(1)
(0)(200)
(67)(97)
(1)
(162)
(3) terms
(5) SUBSTRING
(0)
(3) yyy
(2) AT MOST
(33) FILE_TAG_SIZE
(4)(132)(226)(173)(0) 
(6)(37)(1)(2)

*)