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

open CommonTypes
open LittleEndian
open CommonGlobals
open DonkeyMftp

type peer = 
  { 
    peer_md4 : Md4.t;
    peer_ip : Ip.t;
    peer_port : int;
    peer_kind : int;
  }
  
let buf_peer buf p =
  buf_md4 buf p.peer_md4;
  buf_ip buf p.peer_ip;
  buf_int16 buf p.peer_port;
  buf_int8 buf p.peer_kind
  
let get_peer s pos = 
  let md4 = get_md4 s pos in
  let ip = get_ip s (pos+16) in
  let port = get_int16 s (pos+20) in
  Printf.printf "PEER %s:%d" (Ip.to_string ip) port; print_newline ();
  let kind = get_int8 s (pos+22) in
  {
    peer_md4 = md4;
    peer_ip = ip;
    peer_port = port;
    peer_kind = kind;
  }, pos + 23
  
type t =
| OvernetConnect of 
(* client md4 *) Md4.t *
(* IP address *) Ip.t *
(* port *)       int *
(* kind *)       int

| OvernetConnectReply of 
(* The last peer of this list is the sender *)
  peer list

| OvernetSearch of 
(* ?? 2 is OK for most searches *) int * 
(* searched file or keyword *) Md4.t

| OvernetSearchReply of 
  Md4.t *
  peer list (* the two closest peers in the binary tree of md4s *)

(*

OK: SEARCH REPLY
17:54:05.411029 217.231.48.129.5509 > 192.168.0.2.5682:  udp 65
MESSAGE SIZE: 65
dec: [
(227)
(15)
(11)(198)(22)(129)(235)(182)(173)(78)(160)(117)(29)(147)(133)(82)(9)(66)
(2) npeers on which we can iter the request

(11)(199)(46)(146)(72)(115)(64)(95)(26)(146)(99)(178)(47)(103)(227)(118)
(217)(88)(209)(92)
(172)(46)
(2)
  
(11)(196)(75)(211)(89)(29)(190)(67)(234)(253)(153)(118)(62)(126)(231)(98)
(217)(231)(48)(129)
(133)(21)
(0)

  ]
*)

| OvernetGetSearchResults of 
  Md4.t * int * int * int

(*
OK: SEARCH GET REPLIES
17:54:15.309167 192.168.0.2.5682 > 217.231.48.129.5509:  udp 23 (DF)
MESSAGE SIZE: 23
dec: [
(227)
(16)
(11)(198)(22)(129)(235)(182)(173)(78)(160)(117)(29)(147)(133)(82)(9)(66)
(0)(0)(0)(100)(0)]
*)


| OvernetSearchResult of
(* query *)  Md4.t *
(* md4 result *) Md4.t *
(* tags *) tag list

(*
OK: ONE REPLY
17:54:15.862868 217.231.48.129.5509 > 192.168.0.2.5682:  udp 71
MESSAGE SIZE: 71
dec: [
(227)
(17)
(11)(198)(22)(129)(235)(182)(173)(78)(160)(117)(29)(147)(133)(82)(9)(66)
(25)(119)(145)(24)(179)(65)(171)(160)(216)(218)(6)(95)(109)(228)(202)(169)
(1)(0)(0)(0)
(2)
(3)(0)  loc
(25)(0) bcp://80.131.174.215:4662
]
*)
(*
OK: ONE REPLY
17:54:15.936396 217.231.48.129.5509 > 192.168.0.2.5682:  udp 70
dec: [
(227)
(17)
(11)(198)(22)(129)(235)(182)(173)(78)(160)(117)(29)(147)(133)(82)(9)(66)
(53)(213)(239)(225)(189)(192)(230)(24)(6)(92)(231)(203)(2)(102)(21)(168)
(1)(0)(0)(0)
(2)
(3)(0) l o c
(24)(0) b c p : / / 2 4 . 1 7 0 . 8 1 . 2 2 7 : 4 6 6 2]
  
OK: ONE REPLY
17:54:15.945381 217.231.48.129.5509 > 192.168.0.2.5682:  udp 103
MESSAGE SIZE: 103
dec: [
(227)
(17)
(11)(198)(22)(129)(235)(182)(173)(78)(160)(117)(29)(147)(133)(82)(9)(66)
(189)(53)(62)(130)(142)(23)(226)(254)(35)(118)(167)(74)(171)(219)(193)(93)
(1)(0)(0)(0)
(2)
(3)(0) l o c
(57)(0) b c p : / / b d 3 5 3 e 8 2 8 e 1 7 e 2 f e 2 3 7 6 a 7 4 a a b d b c 1 5 d : 8 0 . 3 4 . 1 2 7 . 2 1 5 : 6 3 2 4
  ]
*)


| OvernetUnknown of int * string

let names_of_tag =
  [
    1, "filename";
    2, "size";
    3, "type";
    4, "format";
  ]
  
let write buf t =
  match t with
  | OvernetConnect (md4, ip, port, kind) ->
      buf_int8 buf 10;
      buf_md4 buf md4;
      buf_ip buf ip;
      buf_int16 buf port;
      buf_int8 buf kind
  
  | OvernetConnectReply peers ->
      buf_int8 buf 11;
      buf_list16 buf_peer buf peers      
  
  | OvernetSearch (kind, md4) ->
      buf_int8 buf 14;
      buf_int8 buf kind;
      buf_md4 buf md4
  
  | OvernetSearchReply (md4, peers) ->
      buf_int8 buf 15;
      buf_list8 buf_peer buf peers
  
  | OvernetGetSearchResults (md4, kind, min, max) ->
      buf_int8 buf 16;
      buf_md4 buf md4;
      buf_int8 buf kind;
      buf_int16 buf min;
      buf_int16 buf max
      
  | OvernetSearchResult (md4, r_md4, r_tags) ->
      buf_int8 buf 17;
      buf_md4 buf md4;
      buf_md4 buf r_md4;
      buf_tags buf r_tags names_of_tag
      
  | OvernetUnknown (opcode, s) ->
      buf_int8 buf opcode;
      Buffer.add_string buf s
    
let parse opcode s =  
  try
    match opcode with  
    | 10 -> 
        Printf.printf "OK: CONNECT MESSAGE"; print_newline ();
        let md4 = get_md4 s 0 in
        let ip = get_ip s 16 in
        let port = get_int16 s 20 in
        let kind = get_int8 s 22 in
        OvernetConnect (md4, ip, port, kind)
    | 11 -> 
        Printf.printf "OK: CONNECT REPLY"; print_newline ();
        let peers, pos = get_list16 get_peer s 0 in
        OvernetConnectReply peers
    | 14 -> 
        Printf.printf "OK: SEARCH MESSAGE"; print_newline ();
        let kind = get_int8 s 0 in
        let md4 = get_md4 s 1 in
        OvernetSearch (kind, md4)
    | 15 -> 
        Printf.printf "OK: SEARCH REPLY"; print_newline ();
        let md4 = get_md4 s 0 in
        let peers, pos = get_list8 get_peer s 16 in
        OvernetSearchReply (md4, peers)
    
    | 16 ->
        Printf.printf "OK: SEARCH GET REPLIES"; print_newline ();
        let md4 = get_md4 s 0 in
        let kind = get_int8 s 16 in
        let min = get_int16 s 17 in
        let max = get_int16 s 19 in
        OvernetGetSearchResults (md4, kind, min, max)
    
    
    | 17 ->
        Printf.printf "OK: ONE REPLY"; print_newline ();
        let md4 = get_md4 s 0 in
        let r_md4 = get_md4 s 16 in
        let ntags = get_int s 32 in
        let r_tags, pos = get_tags s 36 ntags names_of_tag in
        OvernetSearchResult (md4, r_md4, r_tags)
    | _ ->
        Printf.printf "UNKNOWN: opcode %d" opcode; print_newline ();
        dump s;
        print_newline ();
        OvernetUnknown (opcode, s)
  with e ->
      Printf.printf "Error %s while parsing opcode %d" (Printexc.to_string e)
      opcode; print_newline ();
      dump s;
      print_newline ();
      OvernetUnknown (opcode, s)
      
let udp_handler f sock event =
  match event with
    UdpSocket.READ_DONE ->
      List.iter (fun p -> 
          try
            let pbuf = p.UdpSocket.content in
            let len = String.length pbuf in
            if len < 2 || 
              int_of_char pbuf.[0] <> 227 then begin
                Printf.printf "Received unknown UDP packet"; print_newline ();
                dump pbuf;
              end else begin
                let t = parse (int_of_char pbuf.[1]) 
                  (String.sub pbuf 2 (len-2)) in
(*              M.print t; *)
                f t p
              end
          with e ->
              Printf.printf "Error %s in udp_handler"
                (Printexc.to_string e); print_newline () 
      ) sock.UdpSocket.rlist;
      sock.UdpSocket.rlist <- []
  | _ -> ()
      