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

open CommonOptions
open LimewireOptions
open Options
open Md4
open CommonGlobals
open LittleEndian
open TcpBufferedSocket

  
let gnutella_ok = "GNUTELLA OK"     
let gnutella_200_ok = "GNUTELLA/0.6 200 OK"
let gnutella_503_shielded = "GNUTELLA/0.6 503 I am a shielded leaf node"

  
type packet_type =
  PING | PONG | PUSH | QUERY | QUERY_REPLY | UNKNOWN of int

type 'a packet = {
    pkt_uid : Md4.t;
    pkt_type : packet_type;
    pkt_ttl : int;
    pkt_hops : int;
    pkt_payload : 'a;
  }

let get_string s pos =
  let end_pos = String.index_from s pos '\000' in
  String.sub s pos (end_pos - pos), end_pos+1

let buf_string buf s =
  Buffer.add_string buf s;
  Buffer.add_char buf '\000'

  (*
let get_int16 s pos =
  let c1 = int_of_char s.[pos+1] in
  let c2 = int_of_char s.[pos] in
  c1 + c2 * 256

let buf_int16 buf i =
  let i = i land 65535 in
  Buffer.add_char buf (char_of_int (i / 256));
  Buffer.add_char buf (char_of_int (i mod 256))
  *)
  
module Ping = struct
(*
(202)(24)               PORT
(212)(198)(235)(220)    IP
(0)(0)(0)(0)            NFILES
(0)(0)(0)(0)            NKB
n o n e : 5 6 : f a l s e(0)

grouped? (none = true) : connection speed : ultrapeer possible
*)
    
    type t = 
      SimplePing
    | ComplexPing of complex_type
    
    and complex_type = {
        ip : Ip.t;
        port : int;
        nfiles : int64;
        nkb : int64;
        s : string;
      }
    
    let parse s =
      if s = "" then SimplePing else
      let port = get_int16 s 0 in
      let ip = get_ip s 2 in
      let nfiles = get_int64_32 s 6 in
      let nkb = get_int64_32 s 10 in
      let s = String.sub s 14 (String.length s - 15) in
      ComplexPing { ip = ip;
        port = port;
        nfiles = nfiles;
        nkb = nkb;
        s = s }
    
    let print t = 
      match t with
        SimplePing -> 
          Printf.printf "SIMPLE PING"; print_newline ();
      | ComplexPing t ->
          Printf.printf "PING FROM %s:%d" (Ip.to_string t.ip) t.port;
          print_newline () 
    
    let write buf t =
      match t with
        SimplePing -> ()
      | ComplexPing t ->
          buf_int16 buf t.port;
          buf_ip buf t.ip;
          buf_int64_32 buf t.nfiles;
          buf_int64_32 buf t.nkb;
          buf_string buf t.s
  end
  
module Pong = struct (* PONG *)
    
    type t = {
        ip : Ip.t;
        port : int;
        nfiles : int;
        nkb : int;
      }
    
    let parse s =
      let port = get_int16 s 0 in
      let ip = get_ip s 2 in
      let nfiles = get_int s 6 in
      let nkb = get_int s 10 in
      { ip = ip;
        port = port;
        nfiles = nfiles;
        nkb = nkb;
      }
      
    let print t = 
      Printf.printf "PONG FROM %s:%d" (Ip.to_string t.ip) t.port;
      print_newline () 
      
    let write buf t =
      buf_int16 buf t.port;
      buf_ip buf t.ip;
      buf_int buf t.nfiles;
      buf_int buf t.nkb;
    
  end
  
module Push = struct (* PUSH *)
    
    type t = {
        guid : Md4.t;
        index : int;
        ip : Ip.t;
        port : int;
      }
    
    let parse s =
      let guid = get_md4 s 0 in
      let index = get_int s 16 in
      let ip = get_ip s 20 in
      let port = get_int16 s 24 in
      { ip = ip;
        port = port;
        guid = guid;
        index = index;
      }
      
    let print t = 
      Printf.printf "PUSH TO %s:%d OF %d" 
        (Ip.to_string t.ip) t.port t.index;
      print_newline () 
      
    let write buf t =
      buf_md4 buf t.guid;
      buf_int buf t.index;
      buf_ip buf t.ip;
      buf_int16 buf t.port    
  end
  
module Query = struct (* QUERY *)

(*    
(17)(119)(46)(2)(0)(156)(202)(71)(255)(123)(244)(89)(248)(200)(172)(0)          UID
(128)                                                                           RECHERCHE
(7)                                                                             TTL
(0)                                                                             HOPS
(168)(0)(0)(0)                                                                  PAYLOAD
(0)(0)
f a r m e r   m y l e n e  (0) < ? x m l   v e r s i o n = " 1 . 0 " ? > < a u d i o s   x s i : n o N a m e s p a c e S c h e m a L o c a t i o n = " h t t p : / / w w w . l i m e w i r e . c o m / s c h e m a s / a u d i o . x s d " > < a u d i o   a r t i s t = " f a r m e r   m y l e n e " > < / a u d i o > < / a u d i o s >(0)

    *)

    type t = {
        min_speed : int;  (* kb/s *)
        keywords : string;
        xml_query : string;
      }
    
    let parse s =
      let min_speed = get_int16 s 0 in
      let keywords,pos = get_string s 2 in
      let xml_query, pos = get_string s pos in
      { 
        min_speed = min_speed;
        keywords = keywords;
        xml_query = xml_query;
      }
      
    let print t = 
      Printf.printf "QUERY FOR %s (%s)" t.keywords t.xml_query;
      print_newline () 
      
    let write buf t =
      buf_int16 buf t.min_speed;
      Buffer.add_string buf t.keywords;
      Buffer.add_string buf t.xml_query
    
  end
  
module QueryReply = struct (* QUERY_REPLY *)

(*
(17)(119)(46)(2)(0)(156)(202)(71)(255)(123)(244)(89)(248)(200)(172)(0)
(129)
(0)
(7)
(199)(1)(0)(0)
(2)              NFILES
(202)(24)        PORT
(192)(168)(0)(3) IP
(64)(5)(0)(0)    SPEED
  
(154)(1)(0)(0)     INDEX
(124)(26)(60)(0)   SIZE
M y l e n e   F a r m e r   -   M a m a n   a   T o 1 . m p 3(0)
(0)

(155)(1)(0)(0)
(206)(181)(114)(0)
M y l e n e   F a r m e r   -   M a m a n   A   T o r . m p 3
(0)(0)

  
L I M E
(4)
(28)
(25)
(80)(1) # XML SIZE
(0)     # SUPPORT CHAT
{ p l a i n t e x t } < ? x m l   v e r s i o n = " 1 . 0 " ? > < a u d i o s   n o N a m e s p a c e S c h e m a L o c a t i o n = " h t t
 p : / / w w w . l i m e w i r e . c o m / s c h e m a s / a u d i o . x s d " > < a u d i o       b i t r a t e = " 1 2 8 "   s e c o n d
s = " 2 4 6 "   i n d e x = " 0 "   / > < a u d i o       t i t l e = " M a m a n   A   T o r t "   a r t i s t = " M y l e n e   F a r m e
r "   a l b u m = " D a n c e   R e m i x e s   2 "   g e n r e = " P o p "   y e a r = " 2 0 0 0 "   c o m m e n t s = " ,   A G #   8 5 1
E A 3 B 7 "   b i t r a t e = " 1 6 0 "   s e c o n d s = " 3 7 5 " i n d e x = " 1 "   / > < / a u d i o s >(0) 
(122)(93)(63)(134)(108)(83)(239)(129)(255)(75)(235)(24)(150)(226)(201)(0)
*)
    
    type t = {
        ip : Ip.t;
        port : int;
        speed : int;
        files : file list;
        guid : Md4.t;        
        vendor : string;
        speed_measured : bool option;
        busy : bool option;
        stable : bool option;
        xml_reply : string;
        support_chat : bool;
        dont_connect : bool option;
      }
    
    and file = {
        index: int;
        size : int64;
        name : string;
        info : string;
      }
    
    let rec iter_files nfiles s pos list =
      if nfiles = 0 then List.rev list, pos else
      let index = get_int s pos in
      let size = get_int64_32 s (pos+4) in
      let name,pos = get_string s (pos+8) in
      let info,pos = get_string s pos in
      iter_files (nfiles-1) s pos (
        {
          index = index;
          size = size;
          name = name;
          info = info;
        } :: list)
    
    
    let parse s =
      let nfiles = get_int8 s 0 in
      let port = get_int16 s 1 in
      let ip = get_ip s 3 in
      let speed = get_int s 7 in
      let files, pos  = iter_files nfiles s 11 [] in
      
      let vendor = String.sub s pos 4 in
      let vendor_len = get_int8 s (pos+4) in
      let byte5 = get_int8 s (pos+5) in
      let byte6 = get_int8 s (pos+6) in
      
      let dont_connect = if byte6 land 1 = 0 then None else
          Some (byte5 land 1 <> 0)
      in
      let busy = if byte5 land 4 = 0 then None else
          Some (byte6 land 4 <> 0)
      in
      let stable = if byte5 land 8 = 0 then None else
          Some (byte6 land 8 <> 0)
      in
      let speed_measured = if byte5 land 16 = 0 then None else
          Some (byte6 land 16 <> 0)
      in
      let support_chat, xml_reply =
        try
          let support_chat = get_int8 s (pos+9) = 1 in
          let xml_reply,pos = get_string s (pos+10) in
          support_chat, xml_reply
        with _ -> false, ""
      in
      let guid = get_md4 s (String.length s - 16) in
      {
        ip  = ip;
        port = port;
        speed = speed;
        files = files;
        guid = guid;
        vendor = vendor;
        xml_reply = xml_reply;
        support_chat = support_chat;
        busy = busy;
        stable = stable;
        speed_measured = speed_measured;
        dont_connect = dont_connect;
      }
    
    let print t = 
      Printf.printf "QUERY REPLY FROM %s:%d" (Ip.to_string t.ip) t.port;
      print_newline ();
      List.iter (fun f ->
          Printf.printf "   FILE %s SIZE %s INDEX %d" f.name 
            (Int64.to_string f.size) f.index;
          print_newline ();
      ) t.files
    
    let rec write_files buf files =
      match files with
        [] -> ()
      | t :: list ->
          buf_int buf t.index;
          buf_int64_32 buf t.size;
          buf_string buf t.name;
          buf_string buf t.info;
          write_files buf list
    
    let write buf t =
      buf_int8 buf (List.length t.files);
      buf_int16 buf t.port;
      buf_ip buf t.ip;
      buf_int buf t.speed;
      write_files buf t.files;
      
      Buffer.add_string buf t.vendor;
      buf_int8 buf 4;
      buf_int8 buf (
        (match t.dont_connect with Some true -> 1 | _ -> 0) lor
          (2) lor
          (if t.busy = None then 0 else 4) lor
          (if t.stable = None then 0 else 8) lor
          (if t.speed_measured = None then 0 else 16)
      );
      buf_int8 buf (
        (if t.dont_connect = None then 0 else 1) lor          
          (match t.busy with Some true -> 4 | _ -> 0) lor
          (match t.stable with Some true -> 8 | _ -> 0) lor
          (match t.speed_measured with Some true -> 16 | _ -> 0)
      );
      buf_int16 buf (String.length t.xml_reply + 1);
      buf_int8 buf (if t.support_chat then 1 else 0);
      buf_string buf t.xml_reply;
      
      buf_md4 buf t.guid
      
  end
  
type t = 
| PingReq of Ping.t
| PongReq of Pong.t
| PushReq of Push.t
| QueryReq of Query.t
| QueryReplyReq of QueryReply.t
| UnknownReq of packet_type * string
  
let parse pkt = 
  try
    match pkt.pkt_type with
      PING ->
        { pkt with pkt_payload = PingReq (Ping.parse pkt.pkt_payload) }
    | PONG ->
        { pkt with pkt_payload = PongReq (Pong.parse pkt.pkt_payload) }
    | PUSH ->
        { pkt with pkt_payload = PushReq (Push.parse pkt.pkt_payload) }
    | QUERY ->
        { pkt with pkt_payload = QueryReq (Query.parse pkt.pkt_payload) }
    | QUERY_REPLY ->  
        { pkt with pkt_payload = QueryReplyReq
            (QueryReply.parse pkt.pkt_payload) }
    | UNKNOWN i ->  { pkt with pkt_payload = UnknownReq 
          (UNKNOWN i,pkt.pkt_payload) }
  with e ->
      Printf.printf "Exception in parse: %s" (Printexc2.to_string e);
      print_newline ();
      dump pkt.pkt_payload;
      { pkt with pkt_payload = UnknownReq (pkt.pkt_type,pkt.pkt_payload) }
        
let write buf t =
  match t with
  | PingReq t -> Ping.write buf t
  | PongReq t -> Pong.write buf t
  | PushReq t -> Push.write buf t
  | QueryReq t -> Query.write buf t
  | QueryReplyReq t -> QueryReply.write buf t
  | UnknownReq (i,s) -> Buffer.add_string buf s
        
let print p =
  match p.pkt_payload with
  | PingReq t -> Ping.print t
  | PongReq t -> Pong.print t
  | PushReq t -> Push.print t
  | QueryReq t -> Query.print t
  | QueryReplyReq t -> QueryReply.print t
  | UnknownReq (i,s) -> 
      Printf.printf "UNKNOWN message:"; print_newline ();
      dump s
      
let buf = Buffer.create 1000
      
let server_msg_to_string pkt = 
  Buffer.clear buf;
  buf_md4 buf pkt.pkt_uid;
  buf_int8 buf (match pkt.pkt_type with
      PING -> 0
    | PONG -> 1
    | PUSH -> 64
    | QUERY -> 128
    | QUERY_REPLY -> 129
    | UNKNOWN i -> i);
  buf_int8 buf pkt.pkt_ttl;
  buf_int8 buf pkt.pkt_hops;
  buf_int buf 0;
  write buf pkt.pkt_payload;
  let s = Buffer.contents buf in
  let len = String.length s - 23 in
  str_int s 19 len;
  s 

let new_packet t =
  { 
    pkt_uid = Md4.random ();
    pkt_type = (match t with
        PingReq _ -> PING
      | PongReq _ -> PONG
      | PushReq _ -> PUSH
      | QueryReq _ -> QUERY
      | QueryReplyReq _ -> QUERY_REPLY
      | UnknownReq (i,_) -> i);
    pkt_ttl = 7;
    pkt_hops = 0;
    pkt_payload  =t;
  }
          
let server_send_new sock t =
  write_string sock (server_msg_to_string (new_packet t))
  
let server_send sock t =
  write_string sock (server_msg_to_string t)
      
let gnutella_handler parse f sock nread =
  let b = TcpBufferedSocket.buf sock in
(*  Printf.printf "GNUTELLA HANDLER"; print_newline ();
dump (String.sub b.buf b.pos b.len);
  *)
  try
    while b.len >= 23 do
      let msg_len = get_int b.buf (b.pos+19) in
      if b.len >= 23 + msg_len then
        begin
          let pkt_uid = get_md4 b.buf b.pos in
          let pkt_type = match get_int8 b.buf (b.pos+16) with
              0 -> PING
            | 1 -> PONG
            | 64 -> PUSH
            | 128 -> QUERY
            | 129 -> QUERY_REPLY
            | n -> UNKNOWN n
          in
          let pkt_ttl = get_int8 b.buf  (b.pos+17) in
          let pkt_hops = get_int8 b.buf  (b.pos+18) in
          let data = String.sub b.buf (b.pos+23) msg_len in
          TcpBufferedSocket.buf_used sock (msg_len + 23);
          let pkt = {
              pkt_uid = pkt_uid;
              pkt_type = pkt_type;
              pkt_ttl = pkt_ttl;
              pkt_hops = pkt_hops;
              pkt_payload = data;
            } in
          let pkt = parse pkt in
          f pkt sock
        end
      else raise Not_found
    done
  with 
  | Not_found -> ()

let handler info header_handler body_handler =
  let header_done = ref false in
  fun sock nread ->
    try
      let b = TcpBufferedSocket.buf sock in
      if !header_done then body_handler sock nread else
      let end_pos = b.pos + b.len in
      let begin_pos = max b.pos (end_pos - nread - 3) in
      let rec iter i n_read =
        if i < end_pos then
          if b.buf.[i] = '\r' then
            iter (i+1) n_read
          else
          if b.buf.[i] = '\n' then
            if n_read then begin
                let header = String.sub b.buf b.pos (i - b.pos) in
                
                if info > 10 then begin
                    Printf.printf "HEADER : ";
                    dump header; print_newline ();
                  end;
                header_done := true;
                
                header_handler sock header;
                if not (TcpBufferedSocket.closed sock) then begin
                    let nused = i - b.pos + 1 in
                    buf_used sock nused;              
                    if nread - nused > 20 then begin
(*
                  Printf.printf "BEGINNING OF BLOC (6 bytes from header)";
                  print_newline ();
                  dump (String.sub b.buf (b.pos-6) (min 20 (b.len - b.pos + 6)));
Printf.printf "LEFT %d" (nread - nused); print_newline ();
*)
                        ()
                      end;
                    body_handler sock (nread - nused)
                  end
              end else
              iter (i+1) true
          else
            iter (i+1) false
        else begin
            if info > 0 then (
                Printf.printf "END OF HEADER WITHOUT END"; print_newline ();
                let header = String.sub b.buf b.pos b.len in
                LittleEndian.dump header;
              );
          end
      in
      iter begin_pos false
    with e ->
        Printf.printf "Exception %s in handler" (Printexc2.to_string e); 
        print_newline ();
        raise e
        
let handlers header_handlers body_handler =
  let headers = ref header_handlers in
  let rec iter_read  sock nread =
    let b = TcpBufferedSocket.buf sock in
    match !headers with
      [] -> body_handler sock nread
    | header_handler :: tail ->
        Printf.printf "header handler"; print_newline ();
        let end_pos = b.pos + b.len in
        let begin_pos = max b.pos (end_pos - nread - 3) in
        let rec iter i n_read =
          if i < end_pos then
            if b.buf.[i] = '\r' then
              iter (i+1) n_read
            else
            if b.buf.[i] = '\n' then
              if n_read then begin
                  let header = String.sub b.buf b.pos (i - b.pos) in
(*
                  Printf.printf "HEADER : ";
                  dump header; print_newline ();
*)
                  headers := tail;
                  header_handler sock header;
                  if not (TcpBufferedSocket.closed sock) then begin
                      let nused = i - b.pos + 1 in
                      buf_used sock nused;
                      iter_read sock (nread - nused)
                    end
                end else
                iter (i+1) true
            else
              iter (i+1) false
        in
        iter begin_pos false
  in
  iter_read

let vendors = [
    "ACQX", "Acquisition" ;
    "ARES", "Ares" ;
    "ATOM", "AtomWire" ;
    "BEAR", "BearShare" ;
    "COCO", "CocoGnut" ;
    "CULT", "Cultiv8r" ;
    "EVIL", "Suicide" ;
    "FIRE", "FireFly" ;
    "FISH", "PEERahna" ;
    "GNEW", "Gnewtellium" ;
    "GNOT", "Gnotella" ;
    "GNUC", "Gnucleus" ;
    "GNUT", "Gnut" ;
    "GTKG", "gtk-gnutella" ;
    "HSLG", "Hagelslag" ;
    "LIME", "Limewire" ;
    "MACT", "Mactella" ;
    "MLDK", "MLDonkey";
    "MMMM", "Morpheus-v2" ;
    "MNAP", "MyNapster" ;
    "MRPH", "Morpheus" ;
    "MUTE", "Mutella" ;
    "NAPS", "NapShare" ;
    "OCFG", "OpenCola" ;
    "OPRA", "Opera" ;
    "PHEX", "Phex" ;
    "QTEL", "Qtella" ;
    "RAZA", "Shareaza" ;
    "SNUT", "SwapNut" ;
    "SWAP", "Swapper" ;
    "SWFT", "Swift" ;
    "TOAD", "ToadNode" ;
    "XOLO", "Xolox" ;
    "XTLA", "Xtella" ;
    "ZIGA", "Ziga" 
  ]

let add_header_fields header sock trailer =
  Printf.sprintf "%sUser-Agent: %s\r\nX-My-Address: %s:%d\r\nX-Ultrapeer: False\r\nX-Query-Routing: 0.1\r\n%s"
    header
    !!user_agent
    (Ip.to_string (client_ip (Some sock))) !!client_port
    trailer
  
let add_simplified_header_fields header trailer =
  Printf.sprintf "%sUser-Agent: %s\r\n%s"
    header
    !!user_agent
    trailer
  