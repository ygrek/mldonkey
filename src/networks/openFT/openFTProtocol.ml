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

open Md4

open OpenFTTypes
open CommonGlobals
open BigEndian
open TcpBufferedSocket

let buf_ip buf ip= 
  buf_int16 buf 4;
  LittleEndian.buf_ip buf ip;
  buf_int buf 0;
  buf_int buf 0;
  buf_int buf 0

let get_string s pos =
  try
    let end_pos = String.index_from s pos '\000' in
    String.sub s pos (end_pos - pos), end_pos+1
  with _ -> 
      lprintf "No ending zero !!!"; lprint_newline ();
      let len = String.length s in
      String.sub s pos (len - pos), len

let buf_string buf s = 
  Buffer.add_string buf s;
  buf_int8 buf 0
  
      
  
module Empty = functor(M: sig val msg : string end) -> 
    struct

      let parse s = ()
      
      let print t =
        lprintf "message %s" M.msg
        
      let write buf t = ()

    end
    
module NotImplemented = functor(M: sig val msg : string end) -> 
    struct

      type t
        
      let parse s = 
        lprintf "PARSE FOR Message %s not implemented" M.msg;
        lprint_newline ();
        assert false
      
      let print t =
        lprintf "message %s" M.msg
        
      let write buf t = 
        lprintf "WRITE FOR Message %s not implemented" M.msg;
        lprint_newline ();
        assert false
    end
    
module VersionReply = struct
    type t = {
        major_num : int;
        minor_num : int;
        micro_num : int;
      }
      
    let parse s = 
      let major_num = get_int16 s 0 in
      let minor_num = get_int16 s 2 in
      let micro_num = get_int16 s 4 in
      { major_num = major_num; minor_num = minor_num; micro_num = micro_num }
      
    let print t =
      lprintf "VersionReply %d.%d.%d" t.major_num t.minor_num t.micro_num
      
    let write buf t = 
      buf_int16 buf t.major_num;
      buf_int16 buf t.minor_num;
      buf_int16 buf t.micro_num

    end

module NodeInfoReply = struct 
    type t = {
        ip : Ip.t;
        port : int;
        http_port : int;
      }
      
    let parse s = 
      let ip = LittleEndian.get_ip s 2 in
      let port = get_int16 s 18 in
      let http_port = get_int16 s 20 in
      { 
        ip = ip;
        port = port; 
        http_port = http_port;
      }
      
    let print t =
      lprintf "NodeInfoReply %s:%d (http:%d)"
        (Ip.to_string t.ip) t.port t.http_port
      
    let write buf t = 
      buf_ip buf t.ip;
      buf_int16 buf t.port;
      buf_int16 buf t.http_port
      
  end

let string_of_node_type t = match t with
    User_node -> "User_node"
  | Search_node -> "Search_node"
  | Index_node -> "Index_node"
  
module NodeListReply = struct 
    type node = {
        ip : Ip.t;
        port : int;
        node_type : node_type;
      }
    
    type t = node option
    
    let parse s = 
      if s = "" then None else 
      let ip = LittleEndian.get_ip s 2 in
      let port = get_int16 s 18 in
      let node_type = get_int16 s 20 in
      let node_type =
        if node_type land 2 <> 0 then Search_node else 
        if node_type land 4 <> 0 then Index_node else User_node
      in
      Some { 
        ip = ip;
        port = port; 
        node_type = node_type;
      }
    
    let print t =
      match t with
        None -> lprintf "NodeListReply"
      | Some t ->
          lprintf "NodeInfoReply %s:%d type %s"
            (Ip.to_string t.ip) t.port 
            (string_of_node_type t.node_type)
    
    let write buf t = 
      match t with
        None -> ()
      | Some t ->
          buf_ip buf t.ip;
          buf_int16 buf t.port;
          buf_int16 buf (match t.node_type with
              User_node -> 1
            | Search_node -> 2
            | Index_node -> 3)
          
  end
  
module ClassReply = struct 

    type t = node_type
    
    let parse s = 
      let node_type = get_int16 s 0 in
      let node_type =
        if node_type land 2 <> 0 then Search_node else 
        if node_type land 4 <> 0 then Index_node else User_node
      in
      node_type

    let print t =
      lprintf "ClassReply %s"
        (string_of_node_type t)
    
    let write buf t = 
      buf_int16 buf (match t with
          User_node -> 1
        | Search_node -> 2
        | Index_node -> 3)
          
  end
  
  
module NodeCapReply = struct
    type t = string list
      
    let parse s = 
      let len = String.length s in
      let rec iter pos =
        let ok = get_int16 s pos in
        if ok <> 0 then
          let s, pos = get_string s (pos+2) in
          s :: (iter pos)
        else
          []
      in
      iter 0

    let print t= 
      lprintf "NodeCapReply:"; lprint_newline ();
      List.iter (fun s -> lprintf "%s " s) t
      
    let write buf t =
      List.iter (fun s ->
          buf_int16 buf 1;
          buf_string buf s
      ) t;
      buf_int16 buf 0
      
  end
  
module Child = struct
    type t = bool option
    
    let parse s = 
      if s = "" then None else
        Some (get_int16 s 0 = 1)

    let print t =
      lprintf "Child %s"
        (match t with None -> "" | Some true -> "OK" | _ -> "NO")
    
    let write buf t =
      match t with
        None -> ()
      | Some true ->  buf_int16 buf 1
      | Some false ->  buf_int16 buf 0
  end
    
module ChildReply = struct
    type t = bool
    
    let parse s = 
        get_int16 s 0 = 1

    let print t =
      lprintf "ChildReply %s"
        (match t with true -> "OK" | _ -> "NO")
    
    let write buf t =
      match t with
      | true ->  buf_int16 buf 1
      | false ->  buf_int16 buf 0
  end

(*
Sending on FileDescr 19 (len 512): FT_STATS_REQUEST
(0)(2)   len 
(0)(106) opcode
(0)(1)  /* retrieve info */ 2 = (submit digest)
*)
    
module Stats = struct
    type t = Retrieve_info | Submit_digest
    
    let parse s = 
      match get_int16 s 0  with
        2 -> Submit_digest
      | _ -> Retrieve_info

    let print t =
      lprintf "Stats %s"
        (match t with 
          Submit_digest -> "Submit_digest"
        | _ -> "Retrieve_info")
    
    let write buf t =
      match t with
      | Retrieve_info ->  buf_int16 buf 1
      | Submit_digest ->  buf_int16 buf 2
  end

(*
  Received from FileDescr 19 (len 12): FT_STATS_RESPONSE
Command : 107
dec:[
(0)(0)(1)(114)  USERS 
(0)(6)(73)(-76) SHARES
(0)(0)(12)(20)  SIZE  (MB)
]
*)
    
module StatsReply = struct
    type t = {
        nusers : int;
        nfiles : int;
        size : int;
      }
      
    let parse s = 
      let nusers = get_int16 s 0 in
      let nfiles = get_int16 s 2 in
      let size = get_int16 s 4 in
      { nusers = nusers; nfiles = nfiles; size = size }
      
    let print t =
      lprintf "StatsReply %d.%d.%d" t.nusers t.nfiles t.size
      
    let write buf t = 
      buf_int16 buf t.nusers;
      buf_int16 buf t.nfiles;
      buf_int16 buf t.size

    end

(*
  
(* SEARCH *)

int32 : id
int16 : type SEARCH_HIDDEN | SEARCH_MD5 | SEARCH_HOST

type: SEARCH_HIDDEN
  query: get_array 4
  exclude : ge_array 2
  query_str : "*hidden*"
  exclude_str : ""
else
  query : str
  exclude : str
fi

realm : str
size_min : int32
size_max : int32
kbps_min : int32
kbps_max : int32

  
  Search 2 for [mp3] without []
ascii: [(0)(28)(0) l(0)(0)(0)(2)(0)(1) m p 3(0)(0)(0)(0)(0)(0)(0)(0)(0)(0)(0)(0)(0)(0)(0)(0)(0)(0)(0)]
dec: [(0)(28)(0)(108)(0)(0)(0)(2)(0)(1)(109)(112)(51)(0)(0)(0)(0)(0)(0)(0)(0)(0)(0)(0)(0)(0)(0)(0)(0)(0)(0)(0)]
SENDING:
Search 2 for [mp3] without []
ascii: [(0)(28)(0) l(0)(0)(0)(2)(0)(1) m p 3(0)(0)(0)(0)(0)(0)(0)(0)(0)(0)(0)(0)(0)(0)(0)(0)(0)(0)(0)]
        dec: [
        (0)(28)(0)(108)(0)(0)(0)(2)(0)(1)(109)(112)(51)(0)(0)(0)(0)(0)(0)(0)(0)(0)(0)(0)(0)(0)(0)(0)(0)(0)(0)(0)]

  
dec:[
(0)(31)
(0)(200)
(0)(0)(0)(6) id
(0)(1)       type
(106)(111)(104)(110)(110)(121)(0) johnny
(0)          exclude
(0)          realm
(0)(0)(0)(0) size_min
(0)(0)(0)(0) size_max
(0)(0)(0)(0) kbps_min
(0)(0)(0)(0) kbps_max
]

        SEARCH_FILENAME = 0x01,
        SEARCH_MD5      = 0x02,
        SEARCH_HOST     = 0x04,
        SEARCH_LOCAL    = 0x08,
        SEARCH_HIDDEN   = 0x10  /* the HIDDEN flag indicates that the human
                                 * readable search string will be substituted
                                 * by the hashed/tokenized query...this is up to
                                 * the implementation how paranoid they wish to
                                 * be ;) */
               return;
*)

module Search = struct
    
    type search_type =
      Search_filename
    | Search_md5
    | Search_host
    | Search_local
    | Search_hidden
      
    type t = {
        id : int;
        search_type : search_type;
        words : string;
        exclude : string;
        realm : string;
        size_min : int64;
        size_max : int64;
        kbps_min : int64;
        kbps_max : int64;
      }
      
    let parse s = 
      let id = get_int s 0 in
      let stype = match get_int16 s 4 with
          1 -> Search_filename
        | 2 -> Search_md5
        | 3 -> Search_host
        | 4 -> Search_local
        | _ -> Search_hidden
      in
      if stype = Search_hidden then assert false;
      let words, pos = get_string s 6 in
      let exclude, pos = get_string s pos in
      let realm, pos = get_string s pos in
      let size_min = get_int64_32 s pos in
      let size_max = get_int64_32 s (pos+4) in
      let kbps_min = get_int64_32 s (pos+8) in
      let kbps_max = get_int64_32 s (pos+12) in
      {
        id = id;
        search_type = stype;
        words = words;
        exclude = exclude;
        realm = realm;
        size_min = size_min;
        size_max = size_max;
        kbps_min = kbps_min;
        kbps_max = kbps_max;
      }
      
    let print t =
      lprintf "Search %d for [%s] without [%s]" 
        t.id t.words t.exclude
      
    let write buf t = 
      buf_int buf t.id;
      buf_int16 buf (match t.search_type with
          Search_filename -> 1
        | Search_md5 -> 2
        | Search_host -> 3
        | Search_local -> 4
        | Search_hidden -> 5);
      buf_string buf t.words;
      buf_string buf t.exclude;
      buf_string buf t.realm;
      buf_int64_32 buf t.size_min;
      buf_int64_32 buf t.size_max;
      buf_int64_32 buf t.kbps_min;
      buf_int64_32 buf t.kbps_max

    end

module SearchReply = struct
    type t = {
        id : int;
        ip : Ip.t;
        port : int;
        http_port : int;
        avail : int;
        size : int64;
        md5 : Md4.t;
        filename : string;
      }

    let parse s = 
      let id = get_int s 0 in
      let ip = LittleEndian.get_ip  s 6 in
      let port = get_int16 s 22 in
      let http_port = get_int16 s 24 in
      let avail = get_int s 26 in
      let size = get_int64_32 s 30 in
      let md5,pos = get_string s 34 in
      let filename, pos = get_string s pos in
      {
        id = id;
        ip = ip;
        port = port;
        http_port = http_port;
        avail = avail;
        size = size;
        md5 = Md4.of_string md5;
        filename = filename
      }

    let print t =
      lprintf "SearchReply for %d : %s size %Ld"
        t.id t.filename t.size
      
    let write buf t =
      buf_int buf t.id;
      buf_ip buf t.ip;
      buf_int16 buf t.port;
      buf_int16 buf t.http_port;
      buf_int buf t.avail;
      buf_int64_32 buf t.size;
      buf_string buf (String.lowercase (Md4.to_string t.md5));
      buf_string buf t.filename
      
  end
  
module Share = NotImplemented(struct let msg = "Share" end)
module ShareReply = NotImplemented(struct let msg = "ShareReply" end)
module ModShare = NotImplemented(struct let msg = "ModShare" end)
module ModShareReply = NotImplemented(struct let msg = "ModShareReply" end)

module Push = NotImplemented(struct let msg = "Push" end)
module PushReply = NotImplemented(struct let msg = "PushReply" end)
    
type t =
  VersionReq
| VersionReplyReq of VersionReply.t
| ClassReq
| ClassReplyReq of ClassReply.t
| NodeInfoReq
| NodeInfoReplyReq of NodeInfoReply.t 
| NodeListReq
| NodeListReplyReq of NodeListReply.t
| NodeCapReq
| NodeCapReplyReq of NodeCapReply.t
| PingReq
| PingReplyReq
  
| ChildReq of Child.t
| ChildReplyReq of ChildReply.t
| ShareReq of Share.t
| ShareReplyReq of ShareReply.t
| ModShareReq of ModShare.t
| ModShareReplyReq of ModShareReply.t
| StatsReq of Stats.t
| StatsReplyReq of StatsReply.t
  
| SearchReq of Search.t
| SearchReplyReq of SearchReply.t
  
| PushReq of Push.t
| PushReplyReq of PushReply.t

| UnknownReq of int * string
  
let parse opcode s = 
  try
    match opcode with
      0 -> VersionReq
    | 1 -> VersionReplyReq (VersionReply.parse s)
    | 2 -> ClassReq
    | 3 -> ClassReplyReq (ClassReply.parse s)
    | 4 -> NodeInfoReq
    | 5 -> NodeInfoReplyReq (NodeInfoReply.parse s)
    | 6 -> NodeListReq
    | 7 -> NodeListReplyReq (NodeListReply.parse s)
    | 8 -> NodeCapReq
    | 9 -> NodeCapReplyReq (NodeCapReply.parse s)
    | 10 -> PingReq
    | 11 -> PingReplyReq
  
    | 100 -> ChildReq (Child.parse s)
    | 101 -> ChildReplyReq (ChildReply.parse s)
    | 102 -> ShareReq (Share.parse s)
    | 103 -> ShareReplyReq (ShareReply.parse s)
    | 104 -> ModShareReq (ModShare.parse s)
    | 105 -> ModShareReplyReq (ModShareReply.parse s)
    | 106 -> StatsReq (Stats.parse s)
    | 107 -> StatsReplyReq (StatsReply.parse s)
  
    | 200 -> SearchReq (Search.parse s)
    | 201 -> SearchReplyReq (SearchReply.parse s)
  
    | 300 -> PushReq (Push.parse s)
    | 301 -> PushReplyReq (PushReply.parse s)
    | _ -> raise Not_found

  with e ->
      lprintf "Exception in parse (OPCODE %d): %s" opcode (Printexc2.to_string e);
      lprint_newline ();
      LittleEndian.dump s;
      UnknownReq (opcode, s)
    
let write buf t =
  match t with
    VersionReq -> buf_int16 buf 0
  | VersionReplyReq t -> buf_int16 buf 1;  VersionReply.write buf t
  | ClassReq -> buf_int16 buf 2
  | ClassReplyReq t -> buf_int16 buf 3;  ClassReply.write buf t
  | NodeInfoReq -> buf_int16 buf 4
  | NodeInfoReplyReq t -> buf_int16 buf 5;  NodeInfoReply.write buf t
  | NodeListReq -> buf_int16 buf 6
  | NodeListReplyReq t -> buf_int16 buf 7;  NodeListReply.write buf t
  | NodeCapReq -> buf_int16 buf 8
  | NodeCapReplyReq t -> buf_int16 buf 9;  NodeCapReply.write buf t
  | PingReq -> buf_int16 buf 10
  | PingReplyReq -> buf_int16 buf 11
  
  | ChildReq t -> buf_int16 buf 100;  Child.write buf t
  | ChildReplyReq t -> buf_int16 buf 101;  ChildReply.write buf t
  | ShareReq t -> buf_int16 buf 102;  Share.write buf t
  | ShareReplyReq t -> buf_int16 buf 103;  ShareReply.write buf t
  | ModShareReq t -> buf_int16 buf 104;  ModShare.write buf t
  | ModShareReplyReq t -> buf_int16 buf 105;  ModShareReply.write buf t
  | StatsReq t -> buf_int16 buf 106;  Stats.write buf t
  | StatsReplyReq t -> buf_int16 buf 107;  StatsReply.write buf t
  
  | SearchReq t -> buf_int16 buf 200;  Search.write buf t
  | SearchReplyReq t -> buf_int16 buf 201;  SearchReply.write buf t
  
  | PushReq t -> buf_int16 buf 300;  Push.write buf t
  | PushReplyReq t -> buf_int16 buf 301;  PushReply.write buf t
  
  | UnknownReq (opcode, s) -> buf_int16 buf opcode; Buffer.add_string buf s


let print t = 
  begin
    match t with
      VersionReq -> lprintf "VersionReq"
    | VersionReplyReq t -> VersionReply.print t
    | ClassReq -> lprintf "ClassReq"
    | ClassReplyReq t -> ClassReply.print t
    | NodeInfoReq -> lprintf "NodeInfoReq"
    | NodeInfoReplyReq t -> NodeInfoReply.print t
    | NodeListReq -> lprintf "NodeListReq"
    | NodeListReplyReq t -> NodeListReply.print t
    | NodeCapReq -> lprintf "NodeCapReq"
    | NodeCapReplyReq t -> NodeCapReply.print t
    | PingReq -> lprintf "PingReq"
    | PingReplyReq -> lprintf "PingReplyReq"
    
    | ChildReq t -> Child.print t
    | ChildReplyReq t -> ChildReply.print t
    | ShareReq t -> Share.print t
    | ShareReplyReq t -> ShareReply.print t
    | ModShareReq t -> ModShare.print t
    | ModShareReplyReq t -> ModShareReply.print t
    | StatsReq t -> Stats.print t
    | StatsReplyReq t -> StatsReply.print t
    
    | SearchReq t -> Search.print t
    | SearchReplyReq t -> SearchReply.print t
    
    | PushReq t -> Push.print t
    | PushReplyReq t -> PushReply.print t
    
    | UnknownReq (opcode,s) ->
        lprintf "UNKNOWN %d" opcode; lprint_newline ();
        LittleEndian.dump s
  end;
  lprint_newline () 
  
let buf = Buffer.create 1000
      
let server_msg_to_string t = 
  Buffer.clear buf;
  buf_int16 buf 0;
  write buf t;
  let s = Buffer.contents buf in
  let len = String.length s - 4 in
  str_int16 s 0 len;
  s 

    
let server_send sock t =
(*
  lprintf "SENDING:"; lprint_newline ();
print t;
  *)
  let s = server_msg_to_string t in
(*  LittleEndian.dump s;*)
  write_string sock s
  
let cut_messages parse f sock nread =
  let b = TcpBufferedSocket.buf sock in
  try
    while b.len >= 4 do
      let msg_len = get_int16 b.buf b.pos in
      if b.len >= 4 + msg_len then
        begin
          let opcode = get_int16 b.buf (b.pos+2) in
          let s = String.sub b.buf (b.pos+4) msg_len in
          TcpBufferedSocket.buf_used sock  (msg_len + 4);
          let t = parse opcode s in
          f t sock
        end
      else raise Not_found
    done
  with Not_found -> ()
      
(*
      
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
      lprintf "UNKNOWN message:"; lprint_newline ();
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
      
let gnutella_handler parse f sock nread =
  let b = TcpBufferedSocket.buf sock in
(*  lprintf "GNUTELLA HANDLER"; lprint_newline ();
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
      *)

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
                    lprintf "HEADER : ";
                    LittleEndian.dump header; lprint_newline ();
                  end;
                header_done := true;
                
                header_handler sock header;
                let nused = i - b.pos + 1 in
                buf_used sock nused;              
                if nread - nused > 20 then begin
(*
                  lprintf "BEGINNING OF BLOC (6 bytes from header)";
                  lprint_newline ();
                  dump (String.sub b.buf (b.pos-6) (min 20 (b.len - b.pos + 6)));
lprintf "LEFT %d" (nread - nused); lprint_newline ();
*)
                    ()
                  end;
                body_handler sock (nread - nused)
              end else
              iter (i+1) true
          else
            iter (i+1) false
        else begin
            if info > 0 then (
                lprintf "END OF HEADER WITHOUT END"; lprint_newline ();
                let header = String.sub b.buf b.pos b.len in
                LittleEndian.dump header;
              );
          end
    in
    iter begin_pos false
    with e ->
        lprintf "Exception %s in handler" (Printexc2.to_string e); 
        lprint_newline ();
        raise e

let handlers header_handlers body_handler =
  let headers = ref header_handlers in
  let rec iter_read  sock nread =
    let b = TcpBufferedSocket.buf sock in
    match !headers with
      [] -> body_handler sock nread
    | header_handler :: tail ->
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
                  lprintf "HEADER : ";
                  dump header; lprint_newline ();
*)
                  headers := tail;
                  header_handler sock header;
                  let nused = i - b.pos + 1 in
                  buf_used sock nused;
                  iter_read sock (nread - nused)
                end else
                iter (i+1) true
            else
              iter (i+1) false
        in
        iter begin_pos false
  in
  iter_read
    