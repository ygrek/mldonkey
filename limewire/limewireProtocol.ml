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
  PING | PONG | QRP | VENDOR | STANDARD | 
  PUSH | QUERY | QUERY_REPLY | UNKNOWN of int

type 'a packet = {
    pkt_uid : Md4.t;
    pkt_type : packet_type;
    pkt_ttl : int;
    pkt_hops : int;
    pkt_payload : 'a;
  }

let get_string s pos =
  let len = String.length s in
  try
    if pos <= len then raise Exit;
    let end_pos = String.index_from s pos '\000' in
    String.sub s pos (end_pos - pos), end_pos+1
  with _ -> 
      String.sub s pos (len - pos), len
      
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
          lprintf "SIMPLE PING\n";
      | ComplexPing t ->
          lprintf "PING FROM %s:%d\n" (Ip.to_string t.ip) t.port
          
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
      lprintf "PONG FROM %s:%d\n" (Ip.to_string t.ip) t.port
      
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
      lprintf "PUSH TO %s:%d OF %d\n" 
        (Ip.to_string t.ip) t.port t.index
      
    let write buf t =
      buf_md4 buf t.guid;
      buf_int buf t.index;
      buf_ip buf t.ip;
      buf_int16 buf t.port    
  end
  
module Query = struct (* QUERY *)

    type t = {
        min_speed : int;  (* kb/s *)
        keywords : string;
        xml_query : string list;
      }
      
    let parse s =
      let min_speed = get_int16 s 0 in
      let keywords,pos = get_string s 2 in
      let xml_query, pos =  get_string s pos in
      { 
        min_speed = min_speed;
        keywords = keywords;
        xml_query = String2.split xml_query '\028'; (* 0x1c *)
      }
      
    let print t = 
      lprintf "QUERY FOR %s (%d exts)\n" t.keywords 
        (List.length t.xml_query)
      
    let write buf t =
      buf_int16 buf t.min_speed;
      Buffer.add_string buf t.keywords;
      Buffer.add_char buf '\000';
      Buffer.add_string buf (String2.unsplit t.xml_query '\028');
      Buffer.add_char buf '\000'
  end
  
module QueryReply = struct (* QUERY_REPLY *)
    
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
        info : string list;
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
          info = String2.split info '\028';
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
      lprintf "QUERY REPLY FROM %s:%d\n" (Ip.to_string t.ip) t.port;
      List.iter (fun f ->
          lprintf "   FILE %s SIZE %s INDEX %d\n" f.name 
            (Int64.to_string f.size) f.index;
      ) t.files
    
    let rec write_files buf files =
      match files with
        [] -> ()
      | t :: list ->
          buf_int buf t.index;
          buf_int64_32 buf t.size;
          buf_string buf t.name;
          buf_string buf (String2.unsplit t.info '\028');
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
  
type t = 
| PingReq of Ping.t
| PongReq of Pong.t
| PushReq of Push.t
| QueryReq of Query.t
| QueryReplyReq of QueryReply.t
| QrtPatchReq of QrtPatch.t
| QrtResetReq of QrtReset.t
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
    | QRP -> 
        { pkt with pkt_payload = 
          (if int_of_char pkt.pkt_payload.[0] = 0 then
              QrtResetReq (QrtReset.parse pkt.pkt_payload) 
            else
              QrtPatchReq (QrtPatch.parse pkt.pkt_payload))
        }
    | VENDOR ->  { pkt with pkt_payload = UnknownReq 
            (UNKNOWN 49,pkt.pkt_payload) }
    | STANDARD ->  { pkt with pkt_payload = UnknownReq 
          (UNKNOWN 50,pkt.pkt_payload) }
    | UNKNOWN i ->  { pkt with pkt_payload = UnknownReq 
          (UNKNOWN i,pkt.pkt_payload) }
  with e ->
      lprintf "Exception in parse: %s\n" (Printexc2.to_string e);
      dump pkt.pkt_payload;
      { pkt with pkt_payload = UnknownReq (pkt.pkt_type,pkt.pkt_payload) }
        
let write buf t =
  match t with
  | PingReq t -> Ping.write buf t
  | PongReq t -> Pong.write buf t
  | PushReq t -> Push.write buf t
  | QueryReq t -> Query.write buf t
  | QueryReplyReq t -> QueryReply.write buf t
  | QrtPatchReq t -> 
      buf_int8 buf 1;
      QrtPatch.write buf t
  | QrtResetReq t -> 
      buf_int8 buf 0;
      QrtReset.write buf t
  | UnknownReq (i,s) -> Buffer.add_string buf s
        
let print p =
  match p.pkt_payload with
  | PingReq t -> Ping.print t
  | PongReq t -> Pong.print t
  | PushReq t -> Push.print t
  | QrtResetReq t -> QrtReset.print t
  | QrtPatchReq t -> QrtPatch.print t
  | QueryReq t -> Query.print t
  | QueryReplyReq t -> QueryReply.print t
  | UnknownReq (i,s) -> 
      lprintf "UNKNOWN message:\n";
      dump s
      
let buf = Buffer.create 1000
      
let server_msg_to_string pkt = 
  Buffer.clear buf;
  buf_md4 buf pkt.pkt_uid;
  buf_int8 buf (match pkt.pkt_type with
    | PING -> 0
    | PONG -> 1
    | QRP -> 48 (* 0x30 *)
    | VENDOR -> 49 (* 0x31 *)
    | STANDARD -> 50 
    | PUSH -> 64
    | QUERY -> 128
    | QUERY_REPLY -> 129
    | UNKNOWN i -> i);
  buf_int8 buf pkt.pkt_ttl;
  buf_int8 buf 0 (* pkt.pkt_hops *); (* as a leaf node, we should always send
  a hops = 0 *)
  buf_int buf 0;
  write buf pkt.pkt_payload;
  let s = Buffer.contents buf in
  let len = String.length s - 23 in
  str_int s 19 len;
  if !verbose_msg_servers then begin
      lprintf "SENDING :\n";
      dump s;
    end;
  s 

let new_packet t =
  { 
    pkt_uid = Md4.random ();
    pkt_type = (match t with
        PingReq _ -> PING
      | PongReq _ -> PONG
      | PushReq _ -> PUSH
      | QueryReq _ -> QUERY
      | QrtResetReq _ | QrtPatchReq _ -> QRP
      | QueryReplyReq _ -> QUERY_REPLY
      | UnknownReq (i,_) -> i);
    pkt_ttl = (match t with
      | QrtPatchReq _ | QrtResetReq _ -> 1
      | _ -> 7);
    pkt_hops = 0;
    pkt_payload  =t;
  }
          
let server_send_new sock t =
  write_string sock (server_msg_to_string (new_packet t))
  
let server_send sock t =
  write_string sock (server_msg_to_string t)

  
type ghandler =
  HttpHeader of (gconn -> TcpBufferedSocket.t -> string -> unit)
| Reader of (gconn -> TcpBufferedSocket.t -> int -> unit)

and gconn = {
    mutable gconn_handler : ghandler;
    mutable gconn_refill : (TcpBufferedSocket.t -> unit) list;
    mutable gconn_close_on_write : bool;
  }
    
let gnutella_handler parse f handler sock nread =
  let b = TcpBufferedSocket.buf sock in
  lprintf "GNUTELLA HANDLER\n";
  dump (String.sub b.buf b.pos b.len);
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
      
      (*
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
                
                if info then begin
                    lprintf "HEADER : ";
                    dump header; lprint_newline ();
                  end;
                header_done := true;
                
                header_handler sock header;
                if not (TcpBufferedSocket.closed sock) then begin
                    let nused = i - b.pos + 1 in
                    buf_used sock nused;              
                    if nread - nused > 20 then begin
(*
                  lprintf "BEGINNING OF BLOC (6 bytes from header)\n";
                  dump (String.sub b.buf (b.pos-6) (min 20 (b.len - b.pos + 6)));
lprintf "LEFT %d\n" (nread - nused); 
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
            if info then (
                lprintf "END OF HEADER WITHOUT END\n";
                let header = String.sub b.buf b.pos b.len in
                LittleEndian.dump header;
              );
          end
      in
      iter begin_pos false
    with e ->
        lprintf "Exception %s in handler\n" (Printexc2.to_string e); 
        raise e
          *)
  
let handlers info gconn =
  let rec iter_read sock nread =
    let b = TcpBufferedSocket.buf sock in
    if b.len > 0 then
      match gconn.gconn_handler with
      | HttpHeader h ->
          lprintf "header handler\n"; 
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
                    if info then begin
                        lprintf "HEADER : ";
                        dump header; lprint_newline ();
                      end;
                    h gconn sock header;
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
      | Reader h -> h gconn sock nread
    else 
      lprintf "Nothing to read\n"
  in
  iter_read
  
let set_gnutella_sock sock info ghandler = 
  let gconn = {
      gconn_handler = ghandler;
      gconn_refill = [];
      gconn_close_on_write = false;
    } in
  TcpBufferedSocket.set_reader sock (handlers info gconn);
  lprintf "setting refill handler\n"; 
  TcpBufferedSocket.set_refill sock (fun sock ->
      lprintf "calling major with %d refill handlers\n"
        (List.length gconn.gconn_refill);
      match gconn.gconn_refill with
        [] -> ()
      | refill :: _ -> refill sock
  );
  TcpBufferedSocket.set_handler sock TcpBufferedSocket.WRITE_DONE (
    fun sock ->
      lprintf "write done...\n";
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
  
(* A standard session with gtk-gnutella as ultrapeer:

HEADER : ascii: [ G N U T E L L A / 0 . 6   2 0 0   O K(13)(10) U s e r - A g e n t :   g t k - g n u t e l l a / 0 . 9 2 u   ( 3 0 / 0 1 / 2 0 0 3 ;   X 1 1 ;   L i n u x   2 . 4 . 1 8 - 1 8 . 8 . 0   i 6 8 6 )(13)(10) P o n g - C a c h i n g :   0 . 1(13)(10) B y e - P a c k e t :   0 . 1(13)(10) G G E P :   0 . 5(13)(10) V e n d o r - M e s s a g e :   0 . 1(13)(10) R e m o t e - I P :   1 2 7 . 0 . 0 . 1(13)(10) A c c e p t - E n c o d i n g :   d e f l a t e(13)(10) X - U l t r a p e e r :   T r u e(13)(10) X - U l t r a p e e r - N e e d e d :   T r u e(13)(10) X - Q u e r y - R o u t i n g :   0 . 1(13)(10) X - L i v e - S i n c e :   S a t ,   2 2   M a r   2 0 0 3   0 8 : 2 1 : 3 5   + 0 0 0 0(13)(10)(13)]


dec: [(18)(211)(144)(220)(91)(118)(100)(204)(255)(248)(177)(92)(197)(27)(109)(3)(0)(1)(0)(0)(0)(0)(0)]
SIMPLE PING  

dec: [(6)(93)(144)(220)(17)(35)(129)(160)(226)(78)(124)(120)(59)(147)(241)(78)(128)(7)(0)(13)(0)(0)(0)(0)(0)(97)(103)(112)(114)(111)(116)(111)(99)(111)(108)(0)]
QUERY FOR agprotocol ()

  download:
PUSH HEADER: [GET /get/3/shared1/agProtocol.ml HTTP/1.1\013\nHost: 127.0.0.1:6550\013\nUser-Agent: gtk-gnutella/0.92u (30/01/2003; X11; Linux 2.4.18-18.8.0 i686)\013\n\013]
*)
  
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
  