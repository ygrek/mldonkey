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

(* UDP connect back: if we are not guess capable, we can only send packets
  if we have been allowed to using GTKG/7 *)

open Int64ops
open Printf2
open Options
open Md4
open AnyEndian
open LittleEndian
open TcpBufferedSocket
open Xml_types
  
open CommonHosts
open CommonTypes
open CommonOptions
open CommonGlobals

open GnutellaNetwork
open GnutellaGlobals
open GnutellaTypes
open GnutellaOptions
open GnutellaProtocol

type packet_type =
  PING | PONG | BYE | QRP | VENDOR | STANDARD | 
  PUSH | QUERY | QUERY_REPLY | UNKNOWN of int

type 'a packet = {
    pkt_uid : Md4.t;
    pkt_type : packet_type;
    pkt_ttl : int;
    pkt_hops : int;
    pkt_payload : 'a;
  }

let gnutella2_needed = false
  
let get_string0 s pos =
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

module Ping = struct
    
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
      let nfiles = get_uint64_32 s 6 in
      let nkb = get_uint64_32 s 10 in
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
          lprintf "PING FROM %s:%d\n   %Ld %Ld [%s]\n" (Ip.to_string t.ip) t.port
            t.nfiles t.nkb (String.escaped t.s)
    
    let write buf t =
      match t with
        SimplePing -> ()
      | ComplexPing t ->
          buf_int16 buf t.port;
          buf_ip buf t.ip;
          buf_int64_32 buf t.nfiles;
          buf_int64_32 buf t.nkb;
          Buffer.add_string buf t.s
  end

module Pong = struct (* PONG *)
    
    type t = {
        ip : Ip.t;
        port : int;
        nfiles : int;
        nkb : int;
        ggep: Cobs.ggep list;
      }
    
    let parse s =
      let port = get_int16 s 0 in
      let ip = get_ip s 2 in
      let nfiles = get_int s 6 in
      let nkb = get_int s 10 in
      let s = Cobs.parse (String.sub s 14 (String.length s - 14)) in
      { ip = ip;
        port = port;
        nfiles = nfiles;
        nkb = nkb;
        ggep = s;
      }
    
    let print t = 
      lprintf "PONG FROM %s:%d\n   %d %d\n" (Ip.to_string t.ip) t.port
        t.nfiles t.nkb;
      Cobs.print t.ggep
    
    let write buf t =
      buf_int16 buf t.port;
      buf_ip buf t.ip;
      buf_int buf t.nfiles;
      buf_int buf t.nkb;
      Cobs.write buf t.ggep
  
  end

module Bye = struct
    type t = {
        code : int;
        reason : string;
      }
      
    let parse s = 
      let code = LittleEndian.get_int16 s 0 in
      let reason = String.sub s 2 (String.length s - 3) in
      { code = code; reason = reason }
      
    let print t =
      lprintf "BYE PACKET %d %s\n" t.code t.reason
      
    let write buf t =
      buf_int16 buf t.code;
      Buffer.add_string buf t.reason;
      buf_int8 buf 0
      
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

module Vendor = struct
    type t = 
      Supported of (string * int * int) list
    | Bear4_HopsFlow of int
    | Bear7_ConnectBack of int
    | Gtkg7_UdpConnectBack of int * Md4.t
    | VUnknown of string * int * int * string
    
    let parse s =
      let vendor = String.sub s 0 4 in
      if vendor = "\000\000\000\000" then
        let n  = LittleEndian.get_int16 s 8 in
        let list = ref [] in
        for i = 1 to n do
          list := (String.sub s (2+i*8) 4, get_int16 s (6+i*8),
            get_int16 s (8+i*8)) :: !list
        done;
        Supported (List.rev !list)
      else
      let num = LittleEndian.get_int16 s 4 in
      let version = LittleEndian.get_int16 s 6 in
      match vendor, num, version with
      | "BEAR", 4, 1 ->
          Bear4_HopsFlow (get_uint8 s 8)
      | "BEAR", 7, 1 ->
          Bear7_ConnectBack (get_int16 s 8)
      | "GTKG", 7, 1 ->
          Gtkg7_UdpConnectBack (get_int16 s 8, get_md4 s 10)
      | _ ->
          let s = String.sub s 8 (String.length s - 8) in          
          VUnknown (vendor, num, version, s)
    
    let print t = 
      match t with
        Supported list ->
          lprintf "Supported VENDOR Specific Messages:\n";
          List.iter (fun (s,num,version) ->
              lprintf "   %s/%d version %d\n" s num version
          ) list
      | Bear4_HopsFlow nhops ->
          lprintf "BEAR/4 HopsFlow: nhops %d\n" nhops
      | Bear7_ConnectBack port ->
          lprintf "BEAR/7 Connect Back: port %d\n" port
      | Gtkg7_UdpConnectBack (port, md4) ->
          lprintf "GTKG/7 UDP Connect Back: port %d uid %s\n"
            port (Md4.to_string md4)
      | VUnknown (vendor, num, version, s) ->
          lprintf "VENDOR MESSAGE %s/%d version %d\n   %s\n"
            vendor num version (String.escaped s)
    
    let write buf t =
      match t with
        Supported list ->
          Buffer.add_string buf "\000\000\000\000\000\000\000\000";
          buf_int16 buf (List.length list);
          List.iter (fun (s,num,version) ->
              Buffer.add_string buf s;
              buf_int16 buf num;
              buf_int16 buf version;
          ) list
      | Bear4_HopsFlow nhops ->
          Buffer.add_string buf "BEAR";
          buf_int16 buf 4;
          buf_int16 buf 1;
          buf_int8 buf nhops
          
      | Bear7_ConnectBack port ->
          Buffer.add_string buf "BEAR";
          buf_int16 buf 7;
          buf_int16 buf 1;
          buf_int16 buf port
          
      | Gtkg7_UdpConnectBack (port, md4) ->
          Buffer.add_string buf "GTKG";
          buf_int16 buf 7;
          buf_int16 buf 1;
          buf_int8 buf port;
          buf_md4 buf md4
          
      | VUnknown (vendor, num, version, s) ->
          Buffer.add_string buf vendor;
          LittleEndian.buf_int16 buf num;
          LittleEndian.buf_int16 buf version;
          Buffer.add_string buf s
  end
  
module Query = struct (* QUERY *)
    
    type t = {
        min_speed : int;  (* kb/s *)
        keywords : string;
        xml_query : string list;
        s : string;
      }
    
    let parse s =
      let min_speed = get_int16 s 0 in
      let keywords,pos = get_string0 s 2 in
      let xml_query, pos =  get_string0 s pos in
      let len = String.length s in
      let s = String.sub s pos (len-pos) in
      { 
        min_speed = min_speed;
        keywords = keywords;
        xml_query = String2.split xml_query '\028'; (* 0x1c *)
        s = s;
      }
    
    let print t = 
      lprintf "QUERY FOR %s (%d exts)\n" t.keywords 
        (List.length t.xml_query);
      List.iter (fun xml ->
          lprintf "          ext: %s\n" (String.escaped xml)
      ) t.xml_query;
      if t.s <> "" then
        lprintf "          more: %s\n" (String.escaped t.s)
    
    let write buf t =
      buf_int16 buf t.min_speed;
      Buffer.add_string buf t.keywords;
      Buffer.add_char buf '\000';
      Buffer.add_string buf (String2.unsplit t.xml_query '\028');
      Buffer.add_char buf '\000';
      Buffer.add_string buf t.s
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
      let size = get_uint64_32 s (pos+4) in
      let name,pos = get_string0 s (pos+8) in
      let info,pos = get_string0 s pos in
      iter_files (nfiles-1) s pos (
        {
          index = index;
          size = size;
          name = name;
          info = String2.split info '\028';
        } :: list)
    
    
    let parse s =
      let nfiles = get_uint8 s 0 in
      let port = get_int16 s 1 in
      let ip = get_ip s 3 in
      let speed = get_int s 7 in
      let files, pos  = iter_files nfiles s 11 [] in
      
      let vendor = String.sub s pos 4 in
(*      let vendor_len = get_uint8 s (pos+4) in *)
      let byte5 = get_uint8 s (pos+5) in
      let byte6 = get_uint8 s (pos+6) in
      
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
          let support_chat = get_uint8 s (pos+9) = 1 in
          let xml_reply,pos = get_string0 s (pos+10) in
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
          List.iter (fun more ->
              lprintf "        info: %s\n" (String.escaped more)
          ) f.info
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
| ByeReq of Bye.t
| PongReq of Pong.t
| PushReq of Push.t
| QueryReq of Query.t
| QueryReplyReq of QueryReply.t
| QrtPatchReq of QrtPatch.t
| QrtResetReq of QrtReset.t
| VendorReq of Vendor.t
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
    | BYE ->
        { pkt with pkt_payload = ByeReq (Bye.parse pkt.pkt_payload) }
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
    | VENDOR ->  { pkt with pkt_payload = 
          VendorReq (Vendor.parse pkt.pkt_payload) }
    | STANDARD ->  { pkt with pkt_payload = UnknownReq 
            (UNKNOWN 50,pkt.pkt_payload) }
    | UNKNOWN i ->  { pkt with pkt_payload = UnknownReq 
            (UNKNOWN i,pkt.pkt_payload) }
  with e ->
      if !verbose_unknown_messages then
        begin
          lprintf_nl "Exception in parse: %s" (Printexc2.to_string e);
          dump_hex pkt.pkt_payload;
        end;
      { pkt with pkt_payload = UnknownReq (pkt.pkt_type,pkt.pkt_payload) }

let write buf t =
  match t with
  | PingReq t -> Ping.write buf t
  | PongReq t -> Pong.write buf t
  | PushReq t -> Push.write buf t
  | ByeReq t -> Bye.write buf t
  | QueryReq t -> Query.write buf t
  | VendorReq t -> Vendor.write buf t
  | QueryReplyReq t -> QueryReply.write buf t
  | QrtPatchReq t -> 
      buf_int8 buf 1;
      QrtPatch.write buf t
  | QrtResetReq t -> 
      buf_int8 buf 0;
      QrtReset.write buf t
  | UnknownReq (i,s) -> Buffer.add_string buf s

      
let print p =
  lprintf "Packet (ttl %d, nhops %d, uid %s):\n" p.pkt_ttl p.pkt_hops
    (Md4.to_string p.pkt_uid);
  begin
    match p.pkt_payload with
    | PingReq t -> Ping.print t
    | PongReq t -> Pong.print t
    | PushReq t -> Push.print t
    | ByeReq t -> Bye.print t
    | QrtResetReq t -> QrtReset.print t
    | QrtPatchReq t -> 
        let buf = Buffer.create 100 in
        QrtPatch.print buf t;
        lprintf "%s" (Buffer.contents buf)
    | QueryReq t -> Query.print t
    | VendorReq t -> Vendor.print t
    | QueryReplyReq t -> QueryReply.print t
    | UnknownReq (UNKNOWN i,s) -> 
        lprintf "UNKNOWN message %d:\n" i;
        dump s
    | UnknownReq (_,s) -> 
        lprintf "Bad parsed UNKNOWN message:\n";
        dump s
  end;
  lprintf "\n"
  
let buf = Buffer.create 1000

let server_msg_to_string pkt = 
  
  if !verbose_msg_servers then begin
      lprintf "SENDING :\n";
      print pkt;
    end; 
  Buffer.reset buf;
  buf_md4 buf pkt.pkt_uid;
  buf_int8 buf (match pkt.pkt_type with
    | PING -> 0
    | PONG -> 1
    | BYE -> 2
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
  let s = Bytes.unsafe_of_string @@ Buffer.contents buf in
  let len = Bytes.length s - 23 in
  str_int s 19 len;
  Bytes.unsafe_to_string s

let new_packet t =
  { 
    pkt_uid = Md4.random ();
    pkt_type = (match t with
        PingReq _ -> PING
      | PongReq _ -> PONG
      | PushReq _ -> PUSH
      | ByeReq _ -> BYE
      | QueryReq _ -> QUERY
      | VendorReq _ -> VENDOR
      | QrtResetReq _ | QrtPatchReq _ -> QRP
      | QueryReplyReq _ -> QUERY_REPLY
      | UnknownReq (i,_) -> i);
    pkt_ttl = (match t with
      | PingReq _ | QueryReq _ -> 4
      | _ -> 1
    );
    pkt_hops = 0;
    pkt_payload  = t;
  }

let sock_send_new sock t =
  write_string sock (server_msg_to_string  (new_packet t))            
  
let udp_send ip port msg =
  match !GnutellaGlobals.udp_sock with
    None -> ()
  | Some sock ->
      try
        let s = server_msg_to_string msg in
        if !verbose_msg_servers then begin
            lprintf "Sending on UDP to %s:%d:\n%s\n"
              (Ip.to_string ip) port
            (String.escaped s);
          end;
        UdpSocket.write sock false (Bytes.unsafe_of_string s) ip port;
(*        UdpSocket.write sock s Ip.localhost !!client_port *)
      with e ->
          lprintf "Exception %s in udp_send\n" (Printexc2.to_string e)

let server_send s t =
(*  if s.server_gnutella2 then begin
      lprintf "server_send: try to send a gnutella1 packet to a gnutella2 server\n";
      raise Exit;
    end; *)
  match s.server_sock with
    NoConnection | ConnectionWaiting _ -> 
      begin
        match s.server_query_key with
          GuessSupport ->
            let h = s.server_host in
            let ip = Ip.ip_of_addr h.host_addr in
            udp_send ip h.host_port t
        | _ -> ()
      end
  | Connection sock ->
      let m = server_msg_to_string t in
      write_string sock m

let server_send_new s t =
  server_send s (new_packet t)


let gnutella_handler parse f handler sock =
  let b = TcpBufferedSocket.buf sock in
  let bbuf = Bytes.unsafe_to_string b.buf in
(*  lprintf "GNUTELLA HANDLER\n"; 
  dump (String.sub b.buf b.pos b.len); *)
  try
    while b.len >= 23 do
      let msg_len = get_int bbuf (b.pos+19) in
      if b.len >= 23 + msg_len then
        begin
          let pkt_uid = get_md4 bbuf b.pos in
          let pkt_type = match get_uint8 bbuf (b.pos+16) with
              0 -> PING
            | 1 -> PONG
            | 2 -> BYE
            | 48 -> QRP
            | 49 -> VENDOR
            | 64 -> PUSH
            | 128 -> QUERY
            | 129 -> QUERY_REPLY
            | n -> UNKNOWN n
          in
          let pkt_ttl = get_uint8 bbuf  (b.pos+17) in
          let pkt_hops = get_uint8 bbuf  (b.pos+18) in
          let data = String.sub bbuf (b.pos+23) msg_len in
          buf_used b (msg_len + 23);
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

let server_send_qrt_reset s m = 
  server_send_new s (QrtResetReq m)
  
let server_send_qrt_patch s m = 
  server_send_new s (QrtPatchReq m)
  
let server_send_query quid words xml_query sock s = 
  let module Q = Query in
  let t = QueryReq {
      Q.min_speed = 0;
      Q.keywords = words;
      Q.xml_query  = ["urn:"];
      Q.s = "" ;} in
  let pp = new_packet t in
  let p = { pp with 
      pkt_uid = quid;
      pkt_ttl = (match s.server_query_key with
          GuessSupport -> 1
        | _ -> pp.pkt_ttl);
    } in
  server_send s p

let server_ask_uid s quid fuid = 
  let module Q = Query in
  let t = QueryReq {
      Q.min_speed = 0;
      Q.keywords = "\\";
      Q.xml_query  = [Uid.to_string fuid];
      Q.s = "";
    } in
  let pp = new_packet t in
  let p = { pp with 
      pkt_uid = quid;
      pkt_ttl = (match s.server_query_key with
          GuessSupport -> 1
        | _ -> pp.pkt_ttl);
    } in
  server_send s p
  
let server_recover_file file sock s =
  List.iter (fun ss ->
      match ss.search_search with
        FileUidSearch (file, fuid) ->
          server_ask_uid s ss.search_uid fuid
(*      | FileWordSearch (file, words) ->
          server_send_query ss.search_uid words "" sock s;           *)
      | _ -> ()          
  ) file.file_searches
  

let on_send_pings () = ()

let server_send_ping sock s =
  match sock with
    Connection sock ->
      let pl =
        let module P = Ping in
        PingReq P.SimplePing
      in
      let p  = { (new_packet pl) with pkt_ttl = 1; } in
      s.server_nfiles <- s.server_nfiles_last;
      s.server_nkb <- s.server_nkb_last;
      s.server_ping_last <- p.pkt_uid;
      s.server_nfiles_last <- Int64.zero;
      s.server_nkb_last <- 0;
      server_send s p
  | _ -> ()
      
let server_send_push s uid uri =
  let module P = Push in
  let t = PushReq {
      P.guid = uid;
      P.ip = CommonOptions.client_ip None;
      P.port = !!client_port;
      P.index = (match uri with
          FileByIndex (index,_) -> index
        | _ -> assert false)
    } in
  let p = new_packet t in
  server_send s p

let create_qrt_table words table_size =
  if !verbose then
    lprintf "create_qrt_table\n";
  let infinity = 7 in
  let table_length = 1 lsl table_size in
  let old_array = Array.make table_length infinity in
  let array = Array.make table_length infinity in
  List.iter (fun w ->
      let pos = bloom_hash w table_size in
      if !verbose then
        lprintf "Position %Ld\n" pos;
      array.(Int64.to_int pos) <- 1;
  ) words;
  let string_size = table_length/2 in
  let table = String.create  string_size in
  for i = 0 to string_size - 1 do
    table.[i] <- char_of_int (
      (
        ((array.(i*2) - old_array.(i*2)) land 15) lsl 4) + 
      ((array.(i*2+1) - old_array.(i*2+1)) land 15))
  done;
  Bytes.unsafe_to_string table

let send_qrt_sequence s update_table =

  if update_table then cached_qrt_table := "";
  let table_size = 10 in
  let infinity = 7 in
  let table_length = 1 lsl table_size in
  server_send_qrt_reset s {
      QrtReset.table_length = table_length;
      QrtReset.infinity = infinity;
    };
  
  if !cached_qrt_table = "" then 
    cached_qrt_table := create_qrt_table !all_shared_words table_size;
  let table = !cached_qrt_table in
  
  let compressor, table =
      1, 
      let t = Zlib2.compress_string table in
      assert (Zlib2.uncompress_string2 t = table);
      t
  in
  
  server_send_qrt_patch s {
      QrtPatch.seq_no = 1;
      QrtPatch.seq_size = 1;
      QrtPatch.compressor = compressor;
      QrtPatch.entry_bits = 4;
      QrtPatch.table = table;
    }



let server_send_qkr _ = ()

  
  
  
let resend_udp_packets () = ()
  
let known_download_headers = []
let known_supernode_headers = []
let is_same_network gnutella2 = not gnutella2
  
let host_send_qkr h = ()
  
let check_primitives () = ()
let recover_files_delay = 3600.
  
  
let xml_to_string xml = 
  "<?xml version=\"1.0\"?>" ^ (  Xml.to_string xml)
      
let audio_schema tags = 
  Element ("audios",
    [("xsi:nonamespaceschemalocation",
        "http://www.limewire.com/schemas/audio.xsd")],
    [Element ("audio", tags, [])])

(*
[
("artist", "Tom Jones");
("album", "Mars Attacks Soundtrack");
("title", "It&apos;s Not Unusal");

("sampleRate", "44100"); 
("seconds", "239"); 
("index", "0");
("bitrate", "128")
("track", "1"); 
("description", "Tom Jones, hehe"); 
("genre", "Retro");
("year", "1997")
] 

*)
  
let translate_query q =

  let keywords = ref [] in
  let add_words w =
    keywords := (String2.split_simplify w ' ') @ !keywords
  in
  let audio = ref false in
  let tags = ref [] in  
  let rec iter q = 
    match q with
    | QOr (q1,q2) 
    | QAnd (q1, q2) -> iter q1; iter q2
    | QAndNot (q1,q2) -> iter q1 
    | QHasWord w ->  add_words w
    | QHasField(field, w) ->
        begin
          match field with
            Field_Type -> 
              begin
                match String.lowercase w with
                  "audio" -> audio := true
                | _ -> add_words w
              end
          | Field_Format ->
              begin
                match String.lowercase w with
                | "mp3" | "wav" -> 
                    add_words w;
                    audio := true
                | _ -> add_words w
              end
          | Field_Album -> tags := ("album", w) :: !tags; add_words w
          | Field_Artist -> tags := ("artist", w) :: !tags; add_words w
          | Field_Title -> tags := ("title", w) :: !tags; add_words w
          | _ -> add_words w
        end
    | QHasMinVal (field, value) -> ()
    | QHasMaxVal (field, value) -> ()
    | QNone ->  ()
  in
  iter q;
  !keywords, if !audio then xml_to_string (audio_schema !tags) else ""

let new_search_uid () = Md4.random ()

let cancel_recover_files file =
  List.iter (fun s ->
      Hashtbl.remove searches_by_uid s.search_uid
  ) file.file_searches
  
          
let parse_url url = 
  let (name, uids) = parse_magnet url in
  (name, zero, uids)
  
    
let ask_for_push uid =
  let module P = Push in
  let p = {
      P.guid = uid;
      P.index = 0;
      P.port = !!client_port;
      P.ip = client_ip NoConnection;
    } in
  let pkt = new_packet (PushReq p) in
  List.iter (fun s ->
      server_send s pkt
  ) !connected_servers
  
