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
  
  (*

module Gnutella1 : GnutellaProtocol = struct
        
    
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
        if pos >= len then raise Exit;
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
    
    let sock_send_new sock t =
      write_string sock (server_msg_to_string  (new_packet t))            
    
    let server_send s t =
      if s.server_gnutella2 then begin
          lprintf "server_send: try to send a gnutella1 packet to a gnutella2 server\n";
          raise Exit;
        end;
      match s.server_sock with
        None -> ()
      | Some sock ->
          write_string sock (server_msg_to_string t)          
    
    let server_send_new s t =
      server_send s (new_packet t)
    
    
        
    let gnutella_handler parse f handler sock =
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
    "LIME", "Gnutella" ;
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
  
(* A standard session with gtk-gnutella as ultrapeer:

HEADER : ascii: [ G N U T E L L A / 0 . 6   2 0 0   O K(13)(10) U s e r - A g e n t :   g t k - g n u t e l l a / 0 . 9 2 u   ( 3 0 / 0 1 / 2 0 0 3 ;   X 1 1 ;   L i n u x   2 . 4 . 1 8 - 1 8 . 8 . 0   i 6 8 6 )(13)(10) P o n g - C a c h i n g :   0 . 1(13)(10) B y e - P a c k e t :   0 . 1(13)(10) G G E P :   0 . 5(13)(10) V e n d o r - M e s s a g e :   0 . 1(13)(10) R e m o t e - I P :   1 2 7 . 0 . 0 . 1(13)(10) A c c e p t - E n c o d i n g :   d e f l a t e(13)(10) X - U l t r a p e e r :   T r u e(13)(10) X - U l t r a p e e r - N e e d e d :   T r u e(13)(10) X - Q u e r y - R o u t i n g :   0 . 1(13)(10) X - L i v e - S i n c e :   S a t ,   2 2   M a r   2 0 0 3   0 8 : 2 1 : 3 5   + 0 0 0 0(13)(10)(13)]


dec: [(18)(211)(144)(220)(91)(118)(100)(204)(255)(248)(177)(92)(197)(27)(109)(3)(0)(1)(0)(0)(0)(0)(0)]
SIMPLE PING  

dec: [(6)(93)(144)(220)(17)(35)(129)(160)(226)(78)(124)(120)(59)(147)(241)(78)(128)(7)(0)(13)(0)(0)(0)(0)(0)(97)(103)(112)(114)(111)(116)(111)(99)(111)(108)(0)]
QUERY FOR agprotocol ()

  download:
PUSH HEADER: [GET /get/3/shared1/agProtocol.ml HTTP/1.1\013\nHost: 127.0.0.1:6550\013\nUser-Agent: gtk-gnutella/0.92u (30/01/2003; X11; Linux 2.4.18-18.8.0 i686)\013\n\013]
*)
  
end

module Gnutella2 : GnutellaProtocol = struct
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
    
    
    open AnyEndian
    open Printf2
    open CommonOptions
    open GnutellaOptions
    open Options
    open Md4
    open CommonGlobals
    open TcpBufferedSocket
    open LittleEndian
    
    type addr = Ip.t * int
    
    type g2_packet =
    | PI
    | PI_RELAIS
    | PI_UDP of addr
    
    | PO
    | PO_RELAIS
    
    | LNI
    | LNI_NA of addr (* with or without the port ??? *)
    | LNI_GU of Md4.t (* [16] uid *)
    | LNI_V of string (* [4] vendor *)
    | LNI_LS of int32 (* files *) * int32 (* kB *)
    | LNI_HS of int (* [2] leaves *) * int (* [2] max_leaves *)
    
    | KHL 
    | KHL_TS of int32
    | KHL_NH of addr
    | KHL_NH_GU of Md4.t
    | KHL_NH_V of string (* [4] vendor *)
    | KHL_NH_LS of int32 (* files *) * int32 (* kB *)
    | KHL_NH_HS of int (* [2] leaves *) * int (* [2] max_leaves *)  
    | KHL_CH of addr * int32 (* last conn *)
    | KHL_CH_GU of Md4.t
    | KHL_CH_V of string (* [4] vendor *)
    | KHL_CH_LS of int32 (* files *) * int32 (* kB *)
    | KHL_CH_HS of int (* [2] leaves *) * int (* [2] max_leaves *)  
    
    | PUSH of addr
    
    | QHT_reset of GnutellaProtocol.QrtReset.t
    | QHT_patch of GnutellaProtocol.QrtPatch.t
    
    | QKR
    | QKR_RNA of addr
    | QKA
    | QKA_QK of int32
    | QKA_SNA of addr
    | QKA_QNA of addr
    
    | Q2 of Md4.t
    | Q2_UDP of addr * int32
    | Q2_URN of string
    | Q2_DN of string
    | Q2_MD of string
    | Q2_SZR of int32 * int32
    | Q2_I of string list
    
    | QA of Md4.t
    | QA_TS of int32
    | QA_D of addr * int (* [2] leaves number *)
    | QA_S of addr * int32
    | QA_FR of addr
    
    | QH2 of int (* [1] hop count *) * Md4.t
    | QH2_GU of Md4.t
    | QH2_NA of addr
    | QH2_NH
    | QH2_V of string (* [4] *)
    | QH2_BUP
    | QH2_HG of int (* [1] group id *)
    | QH2_HG_SS of int (* [2]queue *) * int (* [1]capacity *) * int (* [4]speed *)
    | QH2_H 
    | QH2_H_URN of string
    | QH2_H_URL of string
    | QH2_H_DN of int32 (* only of no SZ *) * string
    | QH2_H_SZ of int64 (* 32 or 64 bits *)
    | QH2_H_G of int (* [1] group id *)
    | QH2_H_ID of int32 (* object id *)
    | QH2_H_CSC of int (* [2] n cached sources *)
    | QH2_H_PART of int32 (* length got *)
    | QH2_H_COM of string 
    | QH2_H_PVU of string
    | QH2_MD of string
    | QH2_UPRO
    | QH2_UPRO_NICK of string
    
    | TO of Md4.t
    
    | Unknown of string list * bool * string
    
    type connection =
      UdpPacket of UdpSocket.udp_packet
    | TcpPacket of TcpBufferedSocket.t
    
    type packet = {
        g2_children : packet list;
        g2_payload : g2_packet;
      }
    
    module G2_LittleEndian = struct
        include LittleEndian
        
        let get_addr s pos len =
          let ip = get_ip s pos in
          let port = if pos + 6 <= len then get_int16 s (pos+4) else 0 in
          (ip, port)
        
        let buf_addr buf (ip,port) =
          buf_ip buf ip;
          buf_int16 buf port
        
        let get_string s pos len =
          try
            let end_pos = String.index_from s pos '\000' in
            String.sub s pos (end_pos - pos), end_pos+1
          with _ -> String.sub s pos (len-pos), len
      
      end
    
    module Print = struct
        
        module M = struct
            let buf_addr buf (ip,port) =
              Printf.bprintf buf " %s:%d " (Ip.to_string ip) port
            
            let buf_int64 buf v = Printf.bprintf buf " %Ld " v
            let buf_int32 buf v = Printf.bprintf buf " %ld " v
            let buf_int16 buf v = Printf.bprintf buf " %d " v
            let buf_int buf v = Printf.bprintf buf " %d " v
          end
        
        let buf = Buffer.create 100
        let print_payload msg = 
          Buffer.clear buf;
          let name = 
            match msg with 
            | TO md4 -> buf_md4 buf md4; "TO"
            | PI -> "PI"
            | PI_RELAIS ->  "RELAIS"
            | PI_UDP addr -> M.buf_addr buf addr;  "UDP"
            | PO -> "PO"
            | PO_RELAIS ->  "RELAIS"
            | LNI -> "LNI"        
            | LNI_NA addr -> M.buf_addr buf addr; "NA"
            | LNI_GU md4
            | KHL_NH_GU md4 
            | KHL_CH_GU md4 
              -> buf_md4 buf md4; "GU"
            | LNI_V s
            | KHL_NH_V s
            | KHL_CH_V s
              -> Buffer.add_string buf s; "V"
            | LNI_LS (a,b)
            | KHL_NH_LS (a,b)
            | KHL_CH_LS (a,b)
              -> M.buf_int32 buf a; M.buf_int32 buf b; "LS"
            | LNI_HS (a,b)
            | KHL_NH_HS (a,b) 
            | KHL_CH_HS (a,b) -> M.buf_int16 buf a; M.buf_int16 buf b; "HS"
            | KHL -> "KHL"
            | KHL_TS int32 -> M.buf_int32 buf int32; "TS"
            | KHL_NH addr -> M.buf_addr buf addr; "NH"
            
            | KHL_CH (addr, int32) -> 
                M.buf_addr buf addr; M.buf_int32 buf int32; "CH"
            | PUSH addr -> M.buf_addr buf addr; "PUSH"

(*  
| QHT_reset of GnutellaProtocol.QrtReset.t
| QHT_patch of GnutellaProtocol.QrtPatch.t
*)
            
            
            | QKR -> "QKR"
            | QKR_RNA addr -> M.buf_addr buf addr; "RNA" 
            | QKA -> "QKA"
            | QKA_SNA addr -> M.buf_addr buf addr; "SNA" 
            | QKA_QNA addr -> M.buf_addr buf addr; "QNA" 
            
            | Q2 md4 -> buf_md4 buf md4; "Q2"
            | Q2_UDP (addr, int32) -> M.buf_addr buf addr; M.buf_int32 buf int32; "UDP"
            | Q2_URN s -> Buffer.add_string buf s; "URN"
            | Q2_DN s -> Buffer.add_string buf s; "DN"
            | Q2_SZR (a32, b32) -> M.buf_int32 buf a32; M.buf_int32 buf b32; "SZR"
            | Q2_I list ->
                Buffer.add_string buf (String2.unsplit list '\000'); "I"
            
            | QA md4 -> buf_md4 buf md4; "QA"
            | QA_TS int32 -> M.buf_int32 buf int32; "TS"
            | QA_D (addr, int16) -> M.buf_addr buf addr; M.buf_int16 buf int16; "D"
            | QA_S (addr, int32) -> M.buf_addr buf addr; M.buf_int32 buf int32; "S"
            | QA_FR addr -> M.buf_addr buf addr; "FR"
            
            | QH2 (char, md4) -> buf_int8 buf char; buf_md4 buf md4; "QH2"
            | QH2_GU md4 -> buf_md4 buf md4; "GU"
            | QH2_V s -> Buffer.add_string buf s; "V"
            | QH2_NA addr -> M.buf_addr buf addr; "NA"
            | QH2_NH -> "NH"
            | QH2_BUP -> "BUP"
            | QH2_HG c -> buf_int8 buf c; "HG"
            | QH2_HG_SS (a16,b8,c) ->
                M.buf_int16 buf a16; buf_int8 buf b8; M.buf_int buf c; "SS"
            | QH2_H -> "H"
            
            | QH2_H_URN s -> Buffer.add_string buf s; "URN"
            | QH2_H_URL s -> Buffer.add_string buf s; "URL"
            | QH2_H_COM s -> Buffer.add_string buf s; "COM"
            | QH2_H_PVU s -> Buffer.add_string buf s; "PVU"
            | QH2_MD s -> Buffer.add_string buf s; "MD"
            | QH2_UPRO -> "UPRO"
            | QH2_UPRO_NICK s -> Buffer.add_string buf s; "NICK"
            | QH2_H_DN (int32, s) -> 
                M.buf_int32 buf int32;
                Buffer.add_string buf s; "DN"
            | QH2_H_SZ sz -> M.buf_int64 buf sz; "SZ"
            | QH2_H_G i8 -> buf_int8 buf i8; "G"
            | QH2_H_ID i32 -> M.buf_int32 buf i32; "ID"
            | QH2_H_CSC i16 -> M.buf_int16 buf i16; "CSC"
            | QH2_H_PART i32 -> M.buf_int32 buf i32; "PART"
            | _ ->
                lprintf "Message not implemented\n";
                "A"
          in  
          name, Buffer.contents buf
        
        let print p =
          let buf = Buffer.create 100 in
          let rec iter buf indent p =
            let name, content = print_payload p.g2_payload in
            Printf.bprintf buf "%s%s%s\n" indent name content;
            List.iter (iter buf (indent^"  ")) p.g2_children;
          in
          iter buf "" p;
          Buffer.contents buf        
      end  
    
    let buf = Buffer.create 50 
    
    let g2_encode_payload msg = 
      let  module M = G2_LittleEndian in
      Buffer.clear buf;
      let name = 
        match msg with 
        | TO md4 -> buf_md4 buf md4; "TO"
        | PI -> "PI"
        | PI_RELAIS ->  "RELAIS"
        | PI_UDP addr -> M.buf_addr buf addr;  "UDP"
        | PO -> "PO"
        | PO_RELAIS ->  "RELAIS"
        | LNI -> "LNI"        
        | LNI_NA addr -> M.buf_addr buf addr; "NA"
        | LNI_GU md4
        | KHL_NH_GU md4 
        | KHL_CH_GU md4 
          -> buf_md4 buf md4; "GU"
        | LNI_V s
        | KHL_NH_V s
        | KHL_CH_V s
          -> Buffer.add_string buf s; "V"
        | LNI_LS (a,b)
        | KHL_NH_LS (a,b)
        | KHL_CH_LS (a,b)
          -> M.buf_int32 buf a; M.buf_int32 buf b; "LS"
        | LNI_HS (a,b)
        | KHL_NH_HS (a,b) 
        | KHL_CH_HS (a,b) -> M.buf_int16 buf a; M.buf_int16 buf b; "HS"
        | KHL -> "KHL"
        | KHL_TS int32 -> M.buf_int32 buf int32; "TS"
        | KHL_NH addr -> M.buf_addr buf addr; "NH"
        
        | KHL_CH (addr, int32) -> 
            M.buf_addr buf addr; M.buf_int32 buf int32; "CH"
        | PUSH addr -> M.buf_addr buf addr; "PUSH"

(*  
| QHT_reset of GnutellaProtocol.QrtReset.t
| QHT_patch of GnutellaProtocol.QrtPatch.t
*)
        
        
        | QKR -> "QKR"
        | QKR_RNA addr -> M.buf_addr buf addr; "RNA" 
        | QKA -> "QKA"
        | QKA_SNA addr -> M.buf_addr buf addr; "SNA" 
        | QKA_QNA addr -> M.buf_addr buf addr; "QNA" 
        
        | Q2 md4 -> buf_md4 buf md4; "Q2"
        | Q2_UDP (addr, int32) -> M.buf_addr buf addr; M.buf_int32 buf int32; "UDP"
        | Q2_URN s -> Buffer.add_string buf s; "URN"
        | Q2_DN s -> Buffer.add_string buf s; "DN"
        | Q2_SZR (a32, b32) -> M.buf_int32 buf a32; M.buf_int32 buf b32; "SZR"
        | Q2_I list ->
            Buffer.add_string buf (String2.unsplit list '\000'); "I"
        
        | QA md4 -> buf_md4 buf md4; "QA"
        | QA_TS int32 -> M.buf_int32 buf int32; "TS"
        | QA_D (addr, int16) -> M.buf_addr buf addr; M.buf_int16 buf int16; "D"
        | QA_S (addr, int32) -> M.buf_addr buf addr; M.buf_int32 buf int32; "S"
        | QA_FR addr -> M.buf_addr buf addr; "FR"
        
        | QH2 (char, md4) -> buf_int8 buf char; buf_md4 buf md4; "QH2"
        | QH2_GU md4 -> buf_md4 buf md4; "GU"
        | QH2_V s -> Buffer.add_string buf s; "V"
        | QH2_NA addr -> M.buf_addr buf addr; "NA"
        | QH2_NH -> "NH"
        | QH2_BUP -> "BUP"
        | QH2_HG c -> buf_int8 buf c; "HG"
        | QH2_HG_SS (a16,b8,c) ->
            M.buf_int16 buf a16; buf_int8 buf b8; M.buf_int buf c; "SS"
        | QH2_H -> "H"
        
        | QH2_H_URN s -> Buffer.add_string buf s; "URN"
        | QH2_H_URL s -> Buffer.add_string buf s; "URL"
        | QH2_H_COM s -> Buffer.add_string buf s; "COM"
        | QH2_H_PVU s -> Buffer.add_string buf s; "PVU"
        | QH2_MD s -> Buffer.add_string buf s; "MD"
        | QH2_UPRO -> "UPRO"
        | QH2_UPRO_NICK s -> Buffer.add_string buf s; "NICK"
        | QH2_H_DN (int32, s) -> 
            M.buf_int32 buf int32;
            Buffer.add_string buf s; "DN"
        | QH2_H_SZ sz -> M.buf_int64 buf sz; "SZ"
        | QH2_H_G i8 -> buf_int8 buf i8; "G"
        | QH2_H_ID i32 -> M.buf_int32 buf i32; "ID"
        | QH2_H_CSC i16 -> M.buf_int16 buf i16; "CSC"
        | QH2_H_PART i32 -> M.buf_int32 buf i32; "PART"
        | _ ->
            lprintf "Message not implemented\n";
            "A"
      in  
      name, Buffer.contents buf
    
    let buf = ()
    
    let rec g2_encode pkt = 
      let children = List.map g2_encode pkt.g2_children in
      let name, payload = g2_encode_payload pkt.g2_payload in
      let name_len = String.length name in
      let size = ref (name_len + String.length payload) in
      if children <> [] then begin
          incr size;
          List.iter (fun c -> size := !size + String.length c) children
        end;
      let buf = Buffer.create 100 in
      (if !size = 0 then begin
            let cb = 
              if name_len = 1 then 4 else
                (name_len - 1) lsl 3
            in
            Buffer.add_char buf (char_of_int cb);
            Buffer.add_string buf name
          end else
        let len_len = 
          if !size < 256 then 1 else 
          if !size < 65536 then 2 else 3
        in
        let cb = 
          (if children <> [] then 4 else 0) lor
            (len_len lsl 6) lor ((name_len-1) lsl 3)
        in
        Buffer.add_char buf (char_of_int cb);
        if len_len = 1 then buf_int8 buf !size else
        if len_len = 2 then buf_int16 buf !size else
          LittleEndian.buf_int24 buf !size;
        
        Buffer.add_string buf name;
        List.iter (fun c -> Buffer.add_string buf c) children;
        if children <> [] then Buffer.add_char buf '\000';
        Buffer.add_string buf payload);
      Buffer.contents buf
    
    
    
    
    let g2_decode_payload names be s =
      try
        if be then 
          (lprintf "Big Endian not supported yet\n"; raise Exit);
        let  module M = G2_LittleEndian in
        match names with
        
        | "TO" :: _ -> TO (get_md4 s 0)
        
        | [ "PI" ] -> PI
        | [ "RELAIS"; "PI" ] -> PI_RELAIS
        | [ "UDP"; "PI" ] -> PI_UDP (M.get_addr s 0 (String.length s))
        
        | [ "PO" ] -> PO
        | [ "RELAIS" ; "PO" ] -> PO_RELAIS
        
        | [ "LNI" ] -> LNI
        | [ "NA"; "LNI" ] -> LNI_NA  (M.get_addr s 0 (String.length s))
        | [ "GU"; "LNI" ] -> LNI_GU (get_md4 s 0)
        | [ "V" ; "LNI" ] -> LNI_V (String.sub s 0 4)
        | [ "LS"; "LNI" ] -> LNI_LS (M.get_int32 s 0, M.get_int32 s 4)
        | [ "HS"; "LNI" ] -> LNI_HS (M.get_int16 s 0, M.get_int16 s 2)
        
        | [ "KHL" ] -> KHL
        | [ "TS"; "KHL" ] -> KHL_TS (M.get_int32 s 0)
        | [ "NH" ; "KHL" ] -> KHL_NH (M.get_addr s 0 (String.length s))
        | [ "GU"; "NH" ;"KHL" ] -> KHL_NH_GU (get_md4 s 0)
        | [ "V" ; "NH" ;"KHL" ] -> KHL_NH_V (String.sub s 0 4)
        | [ "LS"; "NH" ;"KHL" ] -> KHL_NH_LS (M.get_int32 s 0, M.get_int32 s 4)
        | [ "HS"; "NH" ;"KHL" ] -> KHL_NH_HS (M.get_int16 s 0, M.get_int16 s 2)
        | [ "CH" ; "KHL" ] -> 
            let len = String.length s in
            KHL_CH (M.get_addr s 0 (len - 4), M.get_int32 s (len-4))
        | [ "GU"; "CH" ;"KHL" ] -> KHL_CH_GU (get_md4 s 0)
        | [ "V" ; "CH" ;"KHL" ] -> KHL_CH_V (String.sub s 0 4)
        | [ "LS"; "CH" ;"KHL" ] -> KHL_CH_LS (M.get_int32 s 0, M.get_int32 s 4)
        | [ "HS"; "CH" ;"KHL" ] -> KHL_CH_HS (M.get_int16 s 0, M.get_int16 s 2)
        
        | [ "PUSH" ] -> PUSH (M.get_addr s 0 (String.length s))

(*  
| QHT_reset of GnutellaProtocol.QrtReset.t
| QHT_patch of GnutellaProtocol.QrtPatch.t
*)
        
        | [ "QKR" ] -> QKR
        | [ "RNA" ; "QKR" ] -> QKR_RNA  (M.get_addr s 0 (String.length s))
        | [ "QKA" ] -> QKA
        | [ "QK" ; "QKA" ] ->  QKA_QK  (M.get_int32 s 0)
        | [ "SNA" ; "QKA" ] -> QKA_SNA  (M.get_addr s 0 (String.length s))
        | [ "QNA" ; "QKA" ] -> QKA_QNA  (M.get_addr s 0 (String.length s))
        
        | [ "Q2" ] -> Q2 (get_md4 s 0)
        | [ "UDP"; "Q2" ] -> 
            let len = String.length s in
            Q2_UDP (M.get_addr s 0 (len - 4), M.get_int32 s (len-4))
        
        | [ "URN"; "Q2" ] -> 
            let s, pos = M.get_string s 0 (String.length s) in Q2_URN s
        | [ "DN"; "Q2" ] -> 
            let s, pos = M.get_string s 0 (String.length s) in Q2_DN s
        | [ "SZR"; "Q2" ] -> 
            Q2_SZR (M.get_int32 s 0, M.get_int32 s 4)
        | [ "I"; "Q2" ] -> 
            let len = String.length s in
            let rec iter list s pos len =
              if pos >= len then list else
              try
                let end_pos = String.index_from s pos '\000' in
                let str = String.sub s pos (end_pos - pos) in
                iter (str :: list) s (end_pos+1) len
              with _ -> 
                  let str = String.sub s pos (len-pos) in
                  if str = "" then list else str :: list
            in
            Q2_I (iter [] s 0 len)
        
        | [ "QA" ] -> QA (get_md4 s 0)
        | [ "TS"; "QA" ] -> QA_TS (M.get_int32 s 0)
        | [ "D"; "QA" ] -> QA_D (M.get_addr s 0 6, M.get_int16 s 6)
        | [ "S"; "QA" ] -> QA_S (M.get_addr s 0 6, M.get_int32 s 6)
        | [ "FR"; "QA" ] -> QA_FR (M.get_addr s 0 (String.length s))
        
        | [ "QH2" ] -> QH2 (get_int8 s 0, get_md4 s 1)
        | [ "GU"; "QH2" ] -> QH2_GU (get_md4 s 0)
        | [ "V" ; "QH2" ] -> QH2_V (String.sub s 0 4)
        | [ "NA"; "QH2" ] -> QH2_NA (M.get_addr s 0 (String.length s))
        | [ "NH"; "QH2" ] -> QH2_NH
        | [ "BUP"; "QH2" ] -> QH2_BUP
        
        | [ "HG"; "QH2" ] -> QH2_HG (get_int8 s 0)
        | [ "SS"; "HG"; "QH2" ] -> 
            QH2_HG_SS (M.get_int16 s 0, get_int8 s 2, M.get_int s 3)
        
        | [ "H"; "QH2" ] -> QH2_H
        | [ "URN"; "H"; "QH2" ] -> 
            let s, pos = M.get_string s 0 (String.length s) in QH2_H_URN s
        | [ "URL"; "H"; "QH2" ] -> 
            let s, pos = M.get_string s 0 (String.length s) in QH2_H_URL s
        | [ "COM"; "H"; "QH2" ] -> 
            let s, pos = M.get_string s 0 (String.length s) in QH2_H_COM s
        | [ "PVU" ;"H"; "QH2" ] -> 
            let s, pos = M.get_string s 0 (String.length s) in QH2_H_PVU s
        | [ "MD"; "QH2" ] -> 
            let s, pos = M.get_string s 0 (String.length s) in QH2_MD s
        | [ "UPRO"; "QH2" ] -> QH2_UPRO
        | [ "NICK"; "UPRO"; "QH2" ] -> 
            let s, pos = M.get_string s 0 (String.length s) in QH2_UPRO_NICK s
        | [ "DN"; "H"; "QH2" ] -> 
            let dn, pos = M.get_string s 4 (String.length s) in
            QH2_H_DN (M.get_int32 s 0, dn)
        | [ "SZ"; "H"; "QH2" ] -> 
            let len = String.length s in
            QH2_H_SZ (if len = 4 then  Int64.of_int32 (M.get_int32 s 0)
              else M.get_int64 s 0)
        | [ "G"; "H"; "QH2" ] -> QH2_H_G (get_int8 s 0)
        | [ "ID"; "H"; "QH2" ] -> QH2_H_ID (M.get_int32 s 0)
        | [ "CSC"; "H"; "QH2" ] -> QH2_H_CSC (M.get_int16 s 0)
        | [ "PART"; "H"; "QH2" ] -> QH2_H_PART (M.get_int32 s 0)
        
        | _ -> raise Not_found
      with _ ->
          lprintf "Cannot parse: ";
          List.iter (fun name -> lprintf "%s/" name) names;
          lprintf "\n";
          Unknown (names, be, s)
    
    let rec g2_parse name has_children bigendian s = 
      let len = String.length s in
      let rec iter_child pos children = 
        if pos >= len then children, len
        else
        let cb = get_int8 s pos in
        if cb = 0 then children, len else
        let len_len = (cb lsr 6) land 3 in
        if len < pos + 1 + len_len then
          failwith "Ill formed packet (len < pos + 1 + len_len)";
        let be = cb land 2 <> 0 in
        let packet, pos = g2_extract_packet name cb s be pos len in
        iter_child pos (packet :: children)
      in
      let children, pos = if has_children then 
          iter_child 0 [] else [], 0
      in
      {
        g2_children = children;
        g2_payload = g2_decode_payload name bigendian
          (String.sub s pos (len - pos));
      }
    
    and g2_extract_packet root_name cb s be pos len =
      let len_len = (cb lsr 6) land 3 in    
      let pkt_len, pkt_pos = 
        match len_len, be with
        | 1, true -> get_int8 s (pos+1), 2 
        | 2, true -> BigEndian.get_int16 s (pos+1), 3 
        | 3, true -> BigEndian.get_int24 s (pos+1), 4
        | 1, false -> get_int8 s (pos+1), 2 
        | 2, false -> LittleEndian.get_int16 s (pos+1), 3 
        | 3, false -> LittleEndian.get_int24 s (pos+1), 4
        | _ -> 0, 1
      in
      let name_len = ((cb lsr 3) land 7) + 1 in
      let msg_len = 1 + len_len + name_len + pkt_len in
      if len < pos + msg_len then 
        failwith "Ill formed packet (len < pos + msg_len)";
      
      lprintf "One gnutella2 subpacket received\n";
      let name = String.sub s (pos + pkt_pos) name_len in
      let packet = String.sub s (pos + pkt_pos + name_len) pkt_len in
      let has_children = cb land 4 <> 0 in
      g2_parse (name :: root_name) has_children be packet, pos + msg_len
    
    let g2_packet_handler connection p = 
      lprintf "Received packet: \n%s\n" (Print.print p);
      match p.g2_payload with 
      | PI -> ()
      | PO -> ()
      | LNI -> ()
      | KHL -> ()
      | QKR -> () (* unlikely *)
      | QKA -> ()
      | Q2 _ -> ()
      | QA _ -> ()
      | QH2 _ -> ()
(*  | UPROC -> () *)
      | _ -> 
          lprintf "g2_packet_handler: unexpected packet\n"
    
    let g2_handler gconn sock =
      let b = TcpBufferedSocket.buf sock in
      lprintf "GNUTELLA2 HANDLER\n";
      AnyEndian.dump (String.sub b.buf b.pos b.len);
      try
        while b.len >= 2 do
          let s = b.buf in
          lprintf "g2_tcp_packet_handler\n";
          let cb = get_int8 s b.pos in
          let len_len = (cb lsr 6) land 3 in
          if b.len < 1 + len_len then raise Not_found;
          let be = cb land 2 <> 0 in
          
          let len, pos = match len_len, be with
            | 1, true -> get_int8 s (b.pos+1), 2 
            | 2, true -> BigEndian.get_int16 s (b.pos+1), 3 
            | 3, true -> BigEndian.get_int24 s (b.pos+1), 4
            | 1, false -> get_int8 s (b.pos+1), 2 
            | 2, false -> LittleEndian.get_int16 s (b.pos+1), 3 
            | 3, false -> LittleEndian.get_int24 s (b.pos+1), 4
            | _ -> 0, 1
          in
          let name_len = ((cb lsr 3) land 7) + 1 in
          let msg_len = 1 + len_len + name_len + len in
          if b.len < msg_len then raise Not_found;
          
          lprintf "One gnutella2 packet received\n";
          let name = String.sub b.buf (b.pos + pos) name_len in
          let packet = String.sub b.buf (b.pos + pos + name_len) len in
          let has_children = cb land 4 <> 0 in
          TcpBufferedSocket.buf_used sock msg_len;
          g2_packet_handler (TcpPacket sock)
          (g2_parse [name] has_children be packet)
        done
      with 
      | Not_found -> ()
    
    let g2_connected_servers = ref ([] : GnutellaTypes.server list)
    
    let g2_recover_files_from_server sock = 
      lprintf "g2_recover_files_from_server not implemented\n"
    
    let g2_udp_handler p =
      lprintf "g2_udp_handler...\n";
      let buf = p.UdpSocket.content in
      let len = String.length buf in
      let pos = 8 in
      let nCount = get_int8 buf 7 in
      let nFlags = get_int8 buf 3 in
      let ack_me = nFlags land 2 <> 0 in
(* Contribute:
  - deflating
  - multi-parts message
  - acknowledgement
*)
      if nCount > 1 || nFlags land 1 <> 0 then
        lprintf "g2_udp_handler not implemented\n";
      let cb = get_int8 buf 8 in
      let len_len = (cb lsr 6) land 3 in
      let be = cb land 2 <> 0 in
      
      let pkt_len, pkt_pos = match len_len, be with
        | 1, true -> get_int8 buf (pos+1), 2 
        | 2, true -> BigEndian.get_int16 buf (pos+1), 3 
        | 3, true -> BigEndian.get_int24 buf (pos+1), 4
        | 1, false -> get_int8 buf (pos+1),  2 
        | 2, false -> LittleEndian.get_int16 buf (pos+1), 3 
        | 3, false -> LittleEndian.get_int24 buf (pos+1), 4
        | _ -> 0, 1
      in
      let name_len = ((cb lsr 3) land 7) + 1 in
      let msg_len = 1 + len_len + name_len + pkt_len in
      if len < 8 + msg_len then raise Not_found;
      
      lprintf "One gnutella2 packet received\n";
      let name = String.sub buf (pkt_pos + pos) name_len in
      let packet = String.sub buf (pkt_pos + pos + name_len) pkt_len in
      let has_children = cb land 4 <> 0 in
      g2_packet_handler (UdpPacket p) (g2_parse [name] has_children be packet)
    
    let udp_sock = ref None
    
    let udp_handler f sock event =
      match event with
        UdpSocket.READ_DONE ->
          UdpSocket.read_packets sock (fun p -> 
              try
                let pbuf = p.UdpSocket.content in
                let len = String.length pbuf in
                f p
              with e ->
                  lprintf "Error %s in udp_handler"
                    (Printexc2.to_string e); lprint_newline () 
          ) ;
      | _ -> ()
    
    let enable () =   
      let sock = (UdpSocket.create Unix.inet_addr_any
            !!client_port (udp_handler g2_udp_handler)) in
      udp_sock := Some sock;
      
      UdpSocket.set_write_controler sock udp_write_controler
    
    let disable () = 
      match !udp_sock with
        None -> ()
      | Some sock -> 
          udp_sock := None;
          UdpSocket.close sock "disabled"
  
  end
  *)  
