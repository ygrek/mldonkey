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

open BigEndian
open AnyEndian
open Printf2
open Md4
open TcpBufferedSocket

open CommonHosts
open CommonGlobals
open CommonTypes
open CommonOptions

open FasttrackNetwork
open FasttrackTypes
open FasttrackProtocol
open FasttrackGlobals

(*************************************************************************)
(*                                                                       *)
(*                         Constants                                     *)
(*                                                                       *)
(*************************************************************************)

let int64_3 = 3L
let int64_ffffffff = 0xffffffffL
let default_bandwidth = 0x68

external fst_hash_checksum : string -> int = "fst_hash_checksum_ml"

  (*
let known_ips = Hashtbl.create 113

let ip_to_string ip =
  let s = Ip.to_string ip in
  if not (Hashtbl.mem known_ips s) then Hashtbl.add known_ips s s;
  s
    *)

let ip_to_string ip = Ip.to_string ip

(*************************************************************************)
(*                                                                       *)
(*                         crypt_and_send                                *)
(*                                                                       *)
(*************************************************************************)

let crypt_and_send sock out_cipher str =
  if !verbose_msg_raw || monitored sock then
    lprintf "crypt_and_send: to send [%s]\n" (String.escaped str);
  let str = Bytes.of_string str in
  apply_cipher out_cipher str 0 (Bytes.length str);
  if !verbose_msg_raw || monitored sock then
    lprintf "crypt_and_send: [%s] sent\n" (Bytes.unsafe_to_string (Bytes.escaped str));
  write sock str 0 (Bytes.length str)

(*************************************************************************)
(*                                                                       *)
(*                         server_crypt_and_send                         *)
(*                                                                       *)
(*************************************************************************)

let server_crypt_and_send s out_cipher str =
  match s.server_sock with
  | Connection sock ->
      crypt_and_send sock out_cipher str
  | _ -> assert false



let bprint_ints b s =
  for i = 0 to String.length s - 1 do
    Printf.bprintf b "%d." (int_of_char s.[i])
  done

let bprint_chars b s =
  for i = 0 to String.length s - 1 do
    let c = s.[i] in
    let n = int_of_char c in
    if n > 31 && n < 127 then
      Printf.bprintf b "%c." c
    else
      Printf.bprintf b  "\\%03d." n
  done

let tag_of_tag tag s =
  match tag with
  | Field_KNOWN "any"
  | Field_Filename
  | Field_Uid
  | Field_Title
  | Field_KNOWN "time"
  | Field_Artist
  | Field_Album
  | Field_KNOWN "language"
  | Field_KNOWN "keywords"
  | Field_KNOWN "genre"
  | Field_KNOWN "OS"
  | Field_Type
  | Field_KNOWN "version"
  | Field_KNOWN "comment"
  | Field_Codec ->
      string_tag tag s
  | Field_KNOWN "bitdepth"
  | Field_KNOWN "year"
  | Field_KNOWN "rating"
  | Field_KNOWN "quality"
  | Field_Size ->
      let s, _ = get_dynint s 0 in
      int64_tag tag s
  | Field_KNOWN "resolution" ->
      let n1, pos = get_dynint s 0 in
      let n2, pos = get_dynint s pos in
      { tag_name =  tag; tag_value = Pair (n1, n2) }

  | Field_Completesources
  | Field_Availability
  | Field_Length
  | Field_Bitrate
  | Field_Format
  | Field_Filerating
  | Field_Lastseencomplete
  | Field_Mediacodec
  | Field_Medialength
  | Field_Size_Hi
  | Field_UNKNOWN _
  | Field_KNOWN _ ->
      string_tag tag s



let audio_realm = 0x21
let video_realm = 0x22
let image_realm = 0x23
let text_realm = 0x24
let application_realm = 0x25
let any_realm = 0x3f

(*************************************************************************)
(*                                                                       *)
(*                         TcpMessages                                   *)
(*                                                                       *)
(*************************************************************************)

module TcpMessages = struct


(*************************************************************************)
(*                                                                       *)
(*                         TYPES                                         *)
(*                                                                       *)
(*************************************************************************)

    type unicast_address = {
        unicast_source_ip : Ip.t;
        unicast_source_port : int;
        unicast_dest_ip : Ip.t;
        unicast_dest_port : int;
        unicast_hops : int;
      }

    type broadcast_address = {
        broadcast_source_ip : Ip.t;
        broadcast_source_port : int;
        broadcast_unknown : int;
        broadcast_hops : int;
      }

    type packet_path =
      DirectPacket
    | UnicastPacket of unicast_address
    | BroadcastPacket of broadcast_address

    type result_user = {
        user_ip : Ip.t;
        user_port : int;
        user_bandwidth : int;
        user_name : string;
        user_netname : string;
      }

    type result_meta = {
        meta_hash : Md5Ext.t;
        meta_checksum : int64;
        meta_size : int64;
        meta_tags : tag list;
      }

    type push = {
        push_id : int64;

        dest_ip : Ip.t;
        dest_port : int;

        pushing_ip : Ip.t;
        pushing_port : int;

        pushing_supernode_ip : Ip.t;
        pushing_supernode_port : int;

        pushing_name : string
      }

    type shared_file = {
        shared_type : int;
        shared_hash : Md5Ext.t;
        shared_checksum : int64;
        shared_size : int64;
        shared_tags : tag list;
      }

    type neighbour = {
        neighbour_ip : Ip.t;
        neighbour_port : int;
(*
the fourth byte is the same as the last byte of NodeInfoReq
*)
        neighbour_info : string;
        mutable neighbour_hops : int;
      }

    type stats = {
        nusers : int64;
        nfiles : int64;
        nkb : int;
      }

    type file_descr = {
        fd_realm : int;
        fd_unknown1 : string;
        fd_unknown2 : string;
        fd_artist : string;
        fd_title : string;
      }

    type query =
    | QueryFilesReq of string * int * query_term list
    | QueryLocationReq of Md5Ext.t

(* TODO: where do we publish the port where clients can connect us ?? *)
    type t =

(* 0x00 *)    | NodeListReq of (Ip.t * int * int * int) list
(* 0x01 *)    | DeclareNeighbours of neighbour list
(* 0x02 *)    | NodeInfoReq of Ip.t * int * int * string
(* 0x03 *)    | Unknown_03

(* 0x05 *)    | UnshareFileReq of shared_file
(* 0x06 *)    | SearchReq of int * int * query
(* 0x07 *)    | QueryReplyReq of
      (Ip.t * int) *
      int *
      (result_user * result_meta)  list
(* 0x08 *)    | QueryReplyEndReq of int
(* 0x09 *)    | NetworkStatsReq of stats list * string * int64
(* 0x0a *)    | SearchForwardReq of string * int * query
(* 0x0b *)    | SearchForward2Req of string * int * query

(* 0x0b ?? *)
(* 0x0c ?? *)

(* 0x0d *)    | PushRequestReq of push

(* 0x15 *)    | AskUDPConnectionReq of Ip.t * int

(* 0x16 ?? *)
(* 0x17 ?? *)

(* 0x1d *)    | NetworkNameReq of string

(* 0x1e ?? *) | Unknown_1e of int

(* 0x1f Network update *)

(* 0x20 *)    | RandomFilesReq of int * file_descr list

(* 0x21 Random file *)

(* 0x22 *)    | ShareFileReq of shared_file
(* 0x23 *)    | Unknown_23 of int

(* 0x24 *)    | NetworkGlobalStats of string * (string * int64) list

(* 0x26 *)    | ProtocolVersionReq of int

(*        | 0x2b -> Unknown packet [opcode = 0x2b, len=1]
MESSAGE 71 from 62.131.207.119:2354 time:97770972: DirectPacket
    Unknown packet [opcode = 0x2b, len=1]
ascii: [ ?]
dec: [(63)]
*)
(* 0x2b *)    | Unknown_2b of int

(* 0x2c *)    | ExternalIpReq of Ip.t

(* Direct Messages *)
    | PingReq
    | PongReq

(* Unknown Messages *)
    | UnknownReq of int * string
    | UnknownMessageReq of int * string

(*************************************************************************)
(*                                                                       *)
(*                         crypt (internal)                              *)
(*                                                                       *)
(*************************************************************************)

    let crypt ciphers msg_type addr m =
      let size = String.length m in

      let b = Buffer.create 100 in
      buf_int8 b 0x4b; (* 'K' *)

      let msg_type =
        match addr with
          DirectPacket -> msg_type

        | BroadcastPacket addr ->
            LittleEndian.buf_ip b addr.broadcast_source_ip;
            buf_int16 b addr.broadcast_source_port;
            buf_int16 b addr.broadcast_unknown;
            buf_int8 b addr.broadcast_hops;
            0xc000 lor msg_type
        | UnicastPacket addr ->
            LittleEndian.buf_ip b addr.unicast_source_ip;
            buf_int16 b addr.unicast_source_port;
            LittleEndian.buf_ip b addr.unicast_dest_ip;
            buf_int16 b addr.unicast_dest_port;
            buf_int8 b addr.unicast_hops;

            0x80 lor msg_type
      in

      let lo_len = size land 0xff in
      let hi_len = (size lsr 8) land 0xff in

      let lo_type = msg_type land 0xff in
      let hi_type = (msg_type lsr 8) land 0xff in

      let xtype = Int64.to_int (Int64.rem ciphers.out_xinu int64_3) in

      let _ = match xtype with

        | 0 ->
            buf_int8 b lo_type;
            buf_int8 b hi_type;
            buf_int8 b hi_len;
            buf_int8 b lo_len;
        | 1 ->
            buf_int8 b hi_type;
            buf_int8 b hi_len;
            buf_int8 b lo_type;
            buf_int8 b lo_len;
        | _ ->
            buf_int8 b hi_type;
            buf_int8 b lo_len;
            buf_int8 b hi_len;
            buf_int8 b lo_type;
      in

(* update xinu state *)
      ciphers.out_xinu <- Int64.logxor ciphers.out_xinu
        (Int64.logand
          (Int64.lognot (Int64.of_int (size + msg_type)))
        int64_ffffffff);

      Buffer.add_string b m;
      Buffer.contents b

(*************************************************************************)
(*                                                                       *)
(*                         buf_string                                    *)
(*                                                                       *)
(*************************************************************************)

    let buf_string b s =
      buf_dynint b (Int64.of_int (String.length s));
      Buffer.add_string b s

    let get_string s pos =
      let len, pos = get_dynint s pos in
      let len = Int64.to_int len in
      let s = String.sub s pos len in
      s, pos + len

    let get_string0 s pos =
      let n = String.index_from s pos '\000' in
      String.sub s pos (n-pos), n+1

(*************************************************************************)
(*                                                                       *)
(*                         buf_query                                     *)
(*                                                                       *)
(*************************************************************************)

    let buf_query b s_uid query =

(* search id *)
      buf_int16 b s_uid;
(* dunno what this is *)
      buf_int8 b 0x01;
      match query with

      | QueryFilesReq (words, realm, tags) ->

(*          lprintf "UserSearch [%s] for %d\n" words s_uid; *)

(* realm is video/..., audio/..., and strings like that. Avoid them currently.*)
          buf_int8 b realm;

          let tags =
            if words <> "" then
              (Substring, string_tag (Field_KNOWN "any") words) :: tags
            else tags in
          buf_int8 b (List.length tags);

          List.iter (fun (operator, tag) ->
              let s = match tag.tag_value with
                | Uint64 v | Fint64 v -> dynint v
                | String v -> v
                | Pair (n1,n2) -> (dynint n1) ^ (dynint n2)
                | Uint8 _ | Uint16 _ | Addr _
                  -> assert false
              in
              let code =
                match operator with
                | Equals -> 0
                | AtMost -> 2
                | Approx -> 3
                | AtLeast -> 4
                | Substring -> 5
              in
              let tag = tag.tag_name in
              buf_int8 b code;
              buf_int8 b (
                try List.assoc tag name_of_tag with
                  _ -> match tag with
                      Field_KNOWN n -> int_of_string n
                    | _ -> assert false);
              buf_string b s;
          ) tags;

      | QueryLocationReq file_hash ->

(* realm is video/..., audio/..., and strings like that. Avoid them currently.*)
          buf_int8 b 0xbf; (* 191 from one example ... *)

(* number of search terms *)
          buf_int8 b 0x02;

(* if(search->type == SearchTypeSearch) *)
(* cmp type of first term *)
          buf_int8 b   0; (* EQUAL *)
(* field to cmp of first term *)
          buf_int8 b 3;  (* HASH *)
(* length of query string *)
          buf_string b (Md5Ext.direct_to_string file_hash);
          Buffer.add_string b "\006\037\001\002" (* ???? *)

(*************************************************************************)
(*                                                                       *)
(*                         buf_tags                                      *)
(*                                                                       *)
(*************************************************************************)

    let buf_tags b tags =
      let ntags = ref 0 in
      let tags =
        let buf = Buffer.create 100 in
        List.iter (fun tag ->
            try
              let name = List.assoc tag.tag_name name_of_tag in
              incr ntags;
              buf_int8 buf name;
              buf_string buf (
                match tag.tag_value with
                  Uint64 v | Fint64 v ->
                    let b = Buffer.create 6 in
                    buf_dynint b v;
                    Buffer.contents b
                | String s -> s
                | _ -> assert false
              )
            with _ -> ()
        ) tags;

        Buffer.contents buf
      in

      buf_dynint b (Int64.of_int !ntags);
      Buffer.add_string b tags

(*************************************************************************)
(*                                                                       *)
(*                         write                                         *)
(*                                                                       *)
(*************************************************************************)

    let write ciphers addr t =
      match t with
        PingReq -> "\080"
      | PongReq -> "\082"

      | UnknownMessageReq (_,s) -> s

      | DeclareNeighbours neighbours ->
          let b = Buffer.create 100 in
          List.iter (fun n ->
              LittleEndian.buf_ip b n.neighbour_ip;
              buf_int16 b n.neighbour_port;
              assert (String.length n.neighbour_info = 19);
              Buffer.add_string b n.neighbour_info;
              buf_int8 b n.neighbour_hops
          ) neighbours;

          let m = Buffer.contents b in
          crypt ciphers 0x01 addr m

      | Unknown_1e v ->
          let b = Buffer.create 100 in
          buf_int b v;
          let m = Buffer.contents b in
          crypt ciphers 0x1e addr m

      | Unknown_23 v ->
          let b = Buffer.create 100 in
          buf_int b v;
          let m = Buffer.contents b in
          crypt ciphers 0x23 addr m

      | Unknown_2b v ->
          let b = Buffer.create 100 in
          buf_int8 b v;
          let m = Buffer.contents b in
          crypt ciphers 0x2b addr m

      | AskUDPConnectionReq (ip, port) ->
          let b = Buffer.create 100 in
          LittleEndian.buf_ip b ip;
          buf_int16 b port;
          let m = Buffer.contents b in
          crypt ciphers 0x15 addr m

      | Unknown_03 -> crypt ciphers 0x03 addr ""

      | SearchReq (max_results, s_uid, query) ->
          let b = Buffer.create 100 in
          Buffer.add_string b "\000\001";
(* max search results *)
          buf_int16 b max_results; (* Number of results for general queries *)
          buf_query b s_uid query;
          let m = Buffer.contents b in
          crypt ciphers 0x06 addr m

      | NetworkGlobalStats (s, nets) ->
          let b = Buffer.create 100 in
          Buffer.add_string b s;
          List.iter (fun (netname, nusers) ->
              Buffer.add_string b netname;
              buf_int8 b 0;
              buf_int64_32 b nusers
          ) nets;
          let m = Buffer.contents b in
          crypt ciphers 0x24 addr m

      | SearchForwardReq (s, s_uid, query) ->
          let b = Buffer.create 100 in
          Buffer.add_string b s;
          buf_query b s_uid query;
          let m = Buffer.contents b in
          crypt ciphers 0x0a addr m

      | SearchForward2Req (s, s_uid, query) ->
          let b = Buffer.create 100 in
          Buffer.add_string b s;
          buf_query b s_uid query;
          let m = Buffer.contents b in
          crypt ciphers 0x0b addr m

      | ShareFileReq sh ->
          let module U = CommonUploads in
          let buf = Buffer.create 100 in

          Buffer.add_string buf "\000";

          buf_int8 buf sh.shared_type; (* MEDIA_TYPE_UNKNOWN *)

(* unknown *)
          Buffer.add_string buf "\000\000";


(* fthash *)
          let hash = sh.shared_hash in
          let hash = Md5Ext.direct_to_string hash in
          Buffer.add_string buf hash;
          buf_dynint buf (Int64.of_int (fst_hash_checksum (hash)));

(* file size *)
          buf_dynint buf sh.shared_size;

          buf_tags buf sh.shared_tags;

          let m = Buffer.contents buf in
          crypt ciphers 0x22 addr m

      | ProtocolVersionReq version ->
          let b = Buffer.create 4 in
          buf_int b version;
          let m = Buffer.contents b in
          crypt ciphers 0x26 addr m

      | UnshareFileReq sh ->
          let buf = Buffer.create 100 in

(* fthash *)
          let hash = sh.shared_hash in
          let hash = Md5Ext.direct_to_string hash in
          Buffer.add_string buf hash;
          buf_dynint buf (Int64.of_int (fst_hash_checksum (hash)));

(* file size *)
          buf_dynint buf sh.shared_size;
          buf_tags buf sh.shared_tags;

          let m = Buffer.contents buf in
          crypt ciphers 0x05 addr m

      | NetworkNameReq network_name ->
          crypt ciphers 0x1d addr network_name

      | NodeInfoReq (my_ip, my_port, bandwidth, client_name) ->
          let b = Buffer.create 100 in

(* Hope it is the right order: ntohl(sa.sin_addr.s_addr) *)
          LittleEndian.buf_ip b my_ip;
          buf_int16 b my_port; (* client_port *)

(* This next byte represents the user's advertised bandwidth, on
* a logarithmic scale.  0xd1 represents "infinity" (actually,
* 1680 kbps).  The value is approximately 14*log_2(x)+59, where
* x is the bandwidth in kbps. *)
          buf_int8 b bandwidth;
(* 1 byte: dunno. *)
          buf_int8 b  0x00;

          Buffer.add_string b client_name; (* no ending 0 *)
          let m = Buffer.contents b in
          crypt ciphers 0x02 addr m

      | ExternalIpReq ip ->
          let b = Buffer.create 10 in
          LittleEndian.buf_ip b ip;
          let m = Buffer.contents b in
          crypt ciphers 0x2c addr m

      | UnknownReq (msg_type, m) ->
          crypt ciphers msg_type addr m

      | PushRequestReq push ->
          let b = Buffer.create 100 in

          buf_int64_32 b push.push_id;

          LittleEndian.buf_ip b push.dest_ip;
          buf_int16 b push.dest_port;

          LittleEndian.buf_ip b push.pushing_ip;
          buf_int16 b push.pushing_port;

          LittleEndian.buf_ip b push.pushing_supernode_ip;
          buf_int16 b push.pushing_supernode_port;

          Buffer.add_string b push.pushing_name;
          let m = Buffer.contents b in
          crypt ciphers 0x0d addr m

      | NetworkStatsReq (stats, network, nusers) ->
          let b = Buffer.create 100 in

          assert (List.length stats = 7);

          List.iter (fun stats ->
              buf_int64_32 b stats.nusers;
              buf_int64_32 b stats.nfiles;

              let up_nkb = (lnot 0) lsl 16 in

              let nkb = stats.nkb in
              if up_nkb land nkb = 0 then begin

                  buf_int16 b nkb;
                  buf_int16 b 30;

                end else begin
(* TODO: compute the exact exponent *)
                  buf_int16 b (nkb lsr 15);
                  buf_int16 b 15;

                end;
          ) stats;
          Buffer.add_string b network;
          buf_int8 b 0;
          buf_int64_32 b nusers;
          let m = Buffer.contents b in
          crypt ciphers 0x09 addr m

      | NodeListReq list ->

          let b = Buffer.create 100 in

          List.iter (fun (ip, port, last_seen, slots) ->
              LittleEndian.buf_ip b ip;
              buf_int16 b port;
              buf_int8 b last_seen;
              buf_int8 b slots
          )  list;

          let m = Buffer.contents b in
          crypt ciphers 0x00 addr m

      | QueryReplyEndReq sid ->

          let b = Buffer.create 100 in
          buf_int16 b sid;
          let m = Buffer.contents b in
          crypt ciphers 0x08 addr m

      | RandomFilesReq (sid, files) ->

          let b = Buffer.create 100 in
          buf_int16 b sid;
          buf_int8 b 50;
          buf_int8 b 2;
          buf_int8 b (List.length files);
          List.iter (fun file ->
              buf_int8 b file.fd_realm;
              Buffer.add_string b file.fd_unknown1; buf_int8 b 0;
              Buffer.add_string b file.fd_unknown2; buf_int8 b 0;
              Buffer.add_string b file.fd_artist; buf_int8 b 0;
              Buffer.add_string b file.fd_title; buf_int8 b 0;
          ) files;
          let m = Buffer.contents b in
          crypt ciphers 0x20 addr m

      | QueryReplyReq ( (s_ip, s_port), s_uid, results) ->

          let b = Buffer.create 100 in

          LittleEndian.buf_ip b s_ip;
          buf_int16 b s_port;
          buf_int16 b s_uid;

          buf_int16 b (List.length results);

          List.iter (fun (user, meta) ->
(* user *)
              LittleEndian.buf_ip b user.user_ip;
              buf_int16 b user.user_port;
              buf_int8 b user.user_bandwidth;
              Buffer.add_string b user.user_name;
              buf_int8 b 1;
              Buffer.add_string b user.user_netname;
              buf_int8 b 0;

(* hash *)
              Buffer.add_string b (Md5Ext.direct_to_string meta.meta_hash);
              buf_dynint b meta.meta_checksum;
              buf_dynint b meta.meta_size;
              buf_tags b meta.meta_tags;
          ) results;

          let m = Buffer.contents b in
          crypt ciphers 0x07 addr m



(*************************************************************************)
(*                                                                       *)
(*                         get_filename (internal)                       *)
(*                                                                       *)
(*************************************************************************)

    let get_filename tags =
      try
        match find_tag Field_Filename tags with
          String s -> s | _ -> raise Not_found with
        _ -> "Unknown"

(*************************************************************************)
(*                                                                       *)
(*                         get_tags (internal)                           *)
(*                                                                       *)
(*************************************************************************)

    let get_tags m pos =
      let ntags, pos = get_dynint m pos in
      let ntags = Int64.to_int ntags in
      let len = String.length m in

      let rec iter_tags pos n tags =
        if n > 0 && pos < len-2 then
          let tag, pos = get_dynint m pos in
(*          lprintf "tag: %Lx\n" tag; *)
          let tag = Int64.to_int tag in
          let tag_len, pos = get_dynint m pos in
(*          lprintf "tag len: %Ld\n" tag_len; *)
          let tag_len = Int64.to_int tag_len in
(*          lprintf "  value [%s]\n"
            (String.escaped (String.sub m pos tag_len)); *)
          let tagdata =
            match tag with
            | 1  (* 0x01 year *)
            | 5  (* 0x05 duration *)
            | 9  (* 0x09 ??? *)
            | 17 (* 0x11 bitdepth *)
            | 21 (* 0x15 quality/bitrate *)
            | 25 (* 0x19 ??? *)
            | 29 (* 0x1D rating *)
            | 53 (* 0x35 0, 1, 2, -1 *)
            | 33 (* 0x21 size *)
              ->
                let dynint, npos = get_dynint m pos in
                Uint64 dynint

            | 0x0d ->
                let n1, npos = get_dynint m pos in
                let n2, npos = get_dynint m pos in
                Pair (n1,n2)


(*
            | 5 -> time_of_sec (Int64.to_int dynint);
            | 21 -> Printf.sprintf "%Ld kbs" dynint;
            | 13 -> let dynint2, _ = get_dynint m npos in
                Printf.sprintf "%Ldx%Ld" dynint dynint2;
            | 1 | 17 -> Printf.sprintf "%Ld" dynint;
            | 29 -> (match (Int64.to_int dynint) with
                  | 0 -> "Very Poor"
                  | 1 -> "Poor"
                  | 2 -> "OK"
                  | 3 -> "Good"
                  | 4 -> "Excellent"
                  | _ -> "Unknown rating") *)
            | _ -> String (String.sub m pos tag_len)
          in
          let tag = try
              List2.assoc_inv tag name_of_tag
            with _ ->
                Field_KNOWN (string_of_int tag)
          in
          iter_tags (pos + tag_len) (n-1)
          ((new_tag tag tagdata) :: tags)
        else
          tags, pos
      in
      let tags, pos = iter_tags pos ntags [] in
      tags, pos

(*************************************************************************)
(*                                                                       *)
(*                         get_query (internal)                          *)
(*                                                                       *)
(*************************************************************************)

    let get_query m pos =
      let s_uid = get_int16 m pos in

      if m.[pos+2] <> '\001' then
        lprintf "WARNING: query : third byte is %d, not 1\n"
          (int_of_char m.[pos+2]);

      let realm = get_int8 m (pos+3) in
      let nterms = get_int8 m (pos+4) in

      let first_op = get_int8 m (pos+5)  in
      let first_tag = get_int8 m (pos+6) in

      s_uid,
      if  first_op = 0 && first_tag = 3 then begin
          let hash, pos = get_string m (pos+7) in
          let hash = Md5Ext.direct_of_string hash in
          QueryLocationReq hash

        end else begin

          let rec iter pos nterms =
            if nterms = 0 then [] else
            let code = get_int8 m pos in
            if code = 6 then [] else
            let tag = get_int8 m (pos+1) in
            let tag =
              try
                List2.assoc_inv tag name_of_tag
              with Not_found ->
                  lprintf "WARNING Unknown tag %d\n" tag;
                  Field_KNOWN (string_of_int tag)
            in
            let v, pos = get_string m (pos+2) in
            let tag = tag_of_tag tag v in
            let operator = match code with
              | 0 -> Equals
(* Not sure at all... 1 does not appear in PROTOCOL *)
              | 1 -> Approx
              | 2 -> AtMost
              | 3 -> Approx
              | 4 -> AtLeast
              | 5 -> Substring
              | _ -> failwith (Printf.sprintf "Unknown operator %d" code)
            in
            let term = (operator, tag) in
            term :: (iter pos (nterms - 1))
          in
          let terms = iter (pos+5) nterms in
          QueryFilesReq ("", realm, terms)
        end

    exception MessageNotUnderstood

(*************************************************************************)
(*                                                                       *)
(*                         parse_packet (internal)                       *)
(*                                                                       *)
(*************************************************************************)

    let parse_packet msg_type m =
      try
        match msg_type with

        | 0x00 ->

            let list =
              let n = String.length m / 8 in
              let rec iter i list =
                if i = n then List.rev list else
                let l_ip = LittleEndian.get_ip m (i*8) in
                let l_port = get_int16 m (i*8+4) in
                let seen = get_int8 m (i*8+6) in
                let slots = get_int8 m (i*8+7) in

                iter (i+1) ( (l_ip, l_port, seen, slots) :: list)
              in
              iter 0 []
            in
            NodeListReq list

        | 0x01 ->
            let rec iter pos neighbours =
              if pos = String.length m then List.rev neighbours else
              let ip = LittleEndian.get_ip m pos in
              let port = get_int16 m (pos+4) in
              let info = String.sub m (pos+6) 19 in
              let hops = get_int8 m (pos+25) in
              let n = {
                  neighbour_ip = ip;
                  neighbour_port = port;
                  neighbour_info = info;
                  neighbour_hops = hops;
                } in
              iter (pos+26) (n :: neighbours)
            in
            let neighbours = iter 0 [] in
            DeclareNeighbours neighbours

        | 0x02 ->

            let my_ip = LittleEndian.get_ip m 0 in
            let my_port = get_int16 m 4 in

            let bandwidth = get_int8 m 6 in
(*      let next_byte = get_int8 m 7 in *)
            let client_name = String.sub m 8 (String.length m - 8) in

            NodeInfoReq (my_ip, my_port, bandwidth, client_name)

        | 0x03 -> Unknown_03

        | 0x05 ->

            let shared_hash = String.sub m 0 20 in
            let shared_checksum, pos = get_dynint m 20 in
            let shared_size, pos = get_dynint m pos in
            let shared_tags, pos = get_tags m pos in

            let computed_checksum = 
              Int64.of_int (fst_hash_checksum shared_hash) in
            if computed_checksum <> shared_checksum && !verbose then begin
                lprintf "Bad COMPUTED checksum for hash\n";
              end;
            let shared_hash = Md5Ext.direct_of_string shared_hash in

            UnshareFileReq {
              shared_type = 0;
              shared_checksum = shared_checksum;
              shared_hash = shared_hash;
              shared_size = shared_size;
              shared_tags = shared_tags;
            }

        | 0x06 ->

            assert (m.[0] = '\000');

(* second byte is sometimes 0, often with a SID greater than 25000 *)
            if m.[1] <> '\001' then
              lprintf "WARNING: opcode 0x06 : second byte is %d, not 1\n"
                (int_of_char m.[1]);

            let max_results = get_int16 m 2 in
            let s_uid, query = get_query m 4 in
            SearchReq (max_results, s_uid, query)

        | 0x07 ->

(* probably supernode address *)
            let s_ip = LittleEndian.get_ip m 0 in
            let s_port = get_int16 m 4 in

            let id = get_int16 m 6 in

            let nresults = get_int16 m 8 in

            let len = String.length m in
            let rec iter pos n results =
              if n > 0 && pos + 32 < len then
                let user_ip = LittleEndian.get_ip m pos in
                let user_port = get_int16 m (pos+4) in
                let user_bandwidth = get_uint8 m (pos+6) in
                let pos = pos + 7 in
                let user_name, user_netname, pos =
                  if get_uint8 m pos = 2 then
                    "unknown", "unknown", pos+1
                  else
                  let end_name = String.index_from m pos '\001' in
                  let end_netname = String.index_from m end_name '\000' in
                  String.sub m pos (end_name - pos),
                  String.sub m (end_name+1) (end_netname - end_name -1),
                  end_netname + 1
                in

                let user = {
                    user_name = user_name;
                    user_ip = user_ip;
                    user_port = user_port;
                    user_bandwidth = user_bandwidth;
                    user_netname = user_netname;
                  } in

                let result_hash = String.sub m pos 20 in
                let result_checksum, pos = get_dynint m (pos+20) in
                let result_size, pos = get_dynint m pos in

                let computed_checksum = 
                  Int64.of_int (fst_hash_checksum result_hash) in
                if computed_checksum <> result_checksum && !verbose then begin
                    lprintf "Bad COMPUTED checksum for hash\n";
                  end;

                let result_hash = Md5Ext.direct_of_string result_hash in

                let result_tags, pos = get_tags m pos in

                let meta = {
                    meta_tags = result_tags;
                    meta_hash = result_hash;
                    meta_checksum = result_checksum;
                    meta_size = result_size;
                  } in

                iter pos (n-1) ((user, meta) :: results)
              else List.rev results
            in
            let results = iter 10 nresults [] in

            QueryReplyReq ( (s_ip, s_port), id, results)

        | 0x08 ->

            QueryReplyEndReq (get_int16 m 0)

        | 0x09 ->

            let rec iter i =
              if i = 7 then [] else
              let nusers = get_uint64_32 m (i * 12) in
              let nfiles = get_uint64_32 m (i * 12 + 4) in

              let nkb =
                let mantissa = get_int16 m (i * 12 + 8) in
                let exponent = get_int16 m (i * 12 + 10) in

                if exponent >= 30 then
                  (mantissa lsl (exponent-30))
                else
                  (mantissa lsl (30-exponent));
              in
              {
                nusers = nusers;
                nfiles = nfiles;
                nkb = nkb;
              } :: (iter (i+1))
            in
            let stats = iter 0 in
            let len = String.length m in
            let netname = String.sub m 84 (len - 89) in
            let nusers = get_uint64_32 m (len - 4) in
            NetworkStatsReq (stats, netname, nusers)

        | 0x0a ->
            let s = String.sub m 0 4 in
            let s_uid, query = get_query m 4 in
            SearchForwardReq (s, s_uid, query)

        | 0x0b ->
            let s = String.sub m 0 4 in
            let s_uid, query = get_query m 4 in
            SearchForward2Req (s, s_uid, query)

        | 0x0d ->
            let push_id = get_uint64_32 m 0 in

            let dest_ip = LittleEndian.get_ip m 4 in
            let dest_port = get_int16 m 8 in

            let pushing_ip = LittleEndian.get_ip m 10 in
            let pushing_port = get_int16 m 14 in

            let supernode_ip = LittleEndian.get_ip m 16 in
            let supernode_port = get_int16 m 20 in

            let pushing_name = String.sub m 22 (String.length m - 22) in

            let push = {
                push_id = push_id;

                dest_ip = dest_ip;
                dest_port = dest_port;

                pushing_ip = pushing_ip;
                pushing_port = pushing_port;

                pushing_supernode_ip = supernode_ip;
                pushing_supernode_port = supernode_port;

                pushing_name = pushing_name;
              } in

            PushRequestReq push

        | 0x15 ->
            AskUDPConnectionReq (LittleEndian.get_ip m 0, get_int16 m 4)

        | 0x1d ->
            NetworkNameReq m

        | 0x1e -> Unknown_1e (get_int m 0)

        | 0x20 ->
            let id = get_int16 m 0 in
            assert (m.[2] = '\050');
            assert (m.[3] = '\002');
            let nfiles = get_int8 m 4 in

            let rec iter nfiles pos list =
              if nfiles = 0 then List.rev list else
              let fd_realm = get_int8 m pos in
              let fd_unknown1, pos = get_string0 m (pos+1) in
              let fd_unknown2, pos = get_string0 m pos in
              let fd_artist, pos = get_string0 m (pos) in
              let fd_title, pos = get_string0 m (pos) in
              iter (nfiles-1) pos (
                {
                  fd_realm = fd_realm;
                  fd_unknown1 = fd_unknown1;
                  fd_unknown2 = fd_unknown2;
                  fd_artist = fd_artist;
                  fd_title = fd_title;
                } :: list)
            in
            let files = iter nfiles 5 [] in
            RandomFilesReq (id, files)

        | 0x23 -> Unknown_23 (get_int m 0)

        | 0x2b -> Unknown_2b (get_int8 m 0) (* always 63 *)

        | 0x2c ->
            let ip = LittleEndian.get_ip m 0 in
            ExternalIpReq ip

        | 0x26 ->
            let v = get_int m 0 in
            ProtocolVersionReq  v

        | 0x24 ->
            let v = String.sub m 0 60 in
            let rec iter pos =
              try
                let netname, pos = get_string0 m pos in
                let nusers = get_uint64_32 m pos in
                (netname, nusers) :: (iter pos)
              with _ -> []
            in
            let nets = iter 60 in
            NetworkGlobalStats (v, nets)

        | 0x22 ->
            if get_int8 m 0 <> 0 then
              lprintf "WARNING: opcode 0x22, byte 0 (%d) <> 0\n"
              (get_int8 m 0);

(* media type

        MEDIA_TYPE_UNKNOWN  = 0x00,
        MEDIA_TYPE_AUDIO    = 0x01,
        MEDIA_TYPE_VIDEO    = 0x02,
        MEDIA_TYPE_IMAGE    = 0x03,
        MEDIA_TYPE_DOCUMENT = 0x04,
        MEDIA_TYPE_SOFTWARE = 0x05
  *)
            let shared_type = get_int8 m 1 in
            if get_int8 m 2 <> 0 then
              lprintf "WARNING: opcode 0x22, byte 3 (%d)<> 0\n"
                (get_int8 m 2);

(* This byte is sometimes different from 0, in [1..10] *)
           if get_int8 m 3 <> 0 then
              lprintf "WARNING: opcode 0x22, byte 4 (%d) <> 0\n"
              (get_int8 m 3);

            let shared_hash = String.sub m 4 20 in
            let shared_checksum, pos = get_dynint m 24 in
            let shared_size, pos = get_dynint m pos in
            let shared_tags, pos = get_tags m pos in

            let computed_checksum = 
              Int64.of_int (fst_hash_checksum shared_hash) in
            if computed_checksum <> shared_checksum && !verbose then begin
                lprintf "Bad COMPUTED checksum for hash\n";
              end;
            let shared_hash = Md5Ext.direct_of_string shared_hash in

            ShareFileReq {
              shared_type = shared_type;
              shared_hash = shared_hash;
              shared_size = shared_size;
              shared_tags = shared_tags;
              shared_checksum = shared_checksum;
            }

(*        | 0x20 ->
    Unknown packet [opcode = 0x20, len=1038]
    Unknown packet [opcode = 0x20, len=1137]
    Unknown packet [opcode = 0x20, len=1244]
    Unknown packet [opcode = 0x20, len=1252]
    Unknown packet [opcode = 0x20, len=1339]
    Unknown packet [opcode = 0x20, len=1430]
    Unknown packet [opcode = 0x20, len=1466]
    Unknown packet [opcode = 0x20, len=1479]
    Unknown packet [opcode = 0x20, len=1640]
    Unknown packet [opcode = 0x20, len=1664]
    Unknown packet [opcode = 0x20, len=1792]
    Unknown packet [opcode = 0x20, len=1801]
    Unknown packet [opcode = 0x20, len=1905]
    Unknown packet [opcode = 0x20, len=218]
    Unknown packet [opcode = 0x20, len=2302]
    Unknown packet [opcode = 0x20, len=236]
    Unknown packet [opcode = 0x20, len=237]
    Unknown packet [opcode = 0x20, len=5]
    Unknown packet [opcode = 0x20, len=641]
    Unknown packet [opcode = 0x20, len=985]
    Unknown packet [opcode = 0x20, len=998]
    Unknown packet [opcode = 0x22, len=155]
*)

(*        | 0x24 ->
    Unknown packet [opcode = 0x24, len=60]
    Unknown packet [opcode = 0x24, len=97]
*)

(*        | 0x61    Unknown packet [opcode = 0x61, len=23105] *)

(*
        | 0x24 ->
            RECEIVED from supernode 213.93.116.47:1222: BroadcastPacket (69.136.60.160:3277, 11 hops)
Unknown packet [opcode = 0x24, len=97]
ascii: [(0) #(31)(147) H $ <(179)(137) ](0) ((0)(184) f(10)(138)(31)(0)   >(155)(247)(252)(136) E(0) %(5)(255)(31)(184)(236) R(0) '(1)(202) R(140)(145)(4)(0)(26)(0)(21)(174) x(204)(216)(0)(26)(0)(240)(189)(241)(206)(20)(0) ! K a Z a A(0)(0)  (178)(204) f i l e s h a r e(0)(0)(2) \ ) G r o k s t e r(0)(0)(0)(16)(158)]
dec: [(0)(35)(31)(147)(72)(36)(60)(179)(137)(93)(0)(40)(0)(184)(102)(10)(138)(31)(0)(32)(62)(155)(247)(252)(136)(69)(0)(37)(5)(255)(31)(184)(236)(82)(0)(39)(1)(202)(82)(140)(145)(4)(0)(26)(0)(21)(174)(120)(204)(216)(0)(26)(0)(240)(189)(241)(206)(20)(0)(33)(75)(97)(90)(97)(65)(0)(0)(32)(178)(204)(102)(105)(108)(101)(115)(104)(97)(114)(101)(0)(0)(2)(92)(41)(71)(114)(111)(107)(115)(116)(101)(114)(0)(0)(0)(16)(158)]
*)

        | _ -> raise MessageNotUnderstood
      with
        MessageNotUnderstood ->
          UnknownReq (msg_type, m)
      | e ->
          lprintf "WARNING: opcode %d raised %s\n" msg_type
            (Printexc2.to_string e);
          UnknownReq (msg_type, m)


(*************************************************************************)
(*                                                                       *)
(*                         get_xinu                                      *)
(*                                                                       *)
(*************************************************************************)

    let get_xinu s pos xtype =
      match xtype with
        0 ->
          let msg_lo = get_uint8 s (pos+1) in
          let msg_hi = get_uint8 s (pos+2) in
          let len_hi = get_uint8 s (pos+3) in
          let len_lo = get_uint8 s (pos+4) in
          (msg_hi lsl 8) lor msg_lo, (len_hi lsl 8) lor len_lo
      | 1 ->
          let msg_hi = get_uint8 s (pos+1) in
          let len_hi = get_uint8 s (pos+2) in
          let msg_lo = get_uint8 s (pos+3) in
          let len_lo = get_uint8 s (pos+4) in
          (msg_hi lsl 8) lor msg_lo, (len_hi lsl 8) lor len_lo
      | _ ->
          let msg_hi = get_uint8 s (pos+1) in
          let len_lo = get_uint8 s (pos+2) in
          let len_hi = get_uint8 s (pos+3) in
          let msg_lo = get_uint8 s (pos+4) in
          (msg_hi lsl 8) lor msg_lo, (len_hi lsl 8) lor len_lo

(*************************************************************************)
(*                                                                       *)
(*                         parse_head                                    *)
(*                                                                       *)
(*************************************************************************)

    let parse_head ciphers s pos =
      let xtype = Int64.to_int (Int64.rem ciphers.in_xinu int64_3) in
      get_xinu s pos xtype

(*************************************************************************)
(*                                                                       *)
(*                         packet_size                                   *)
(*                                                                       *)
(*************************************************************************)

    let packet_size ciphers s pos len =
      let ss = Bytes.to_string s in
      if len > 0 then
        match int_of_char (Bytes.get s pos) with
          0x50 -> Some 1
        | 0x52 -> Some 1
        | 0x4b ->
(*          lprintf "We have got a real packet\n"; *)
            if len > 4 then
(*                dump_sub s b.pos b.len; *)
              let msg_type, size = parse_head ciphers ss pos in
              Some (size + 5)
            else None

        | n ->
            lprintf "WARNING packet_size: packet not understood: %x (remaining %d bytes)\n" n (len-1);
            if len > 4 then begin
(*                dump_sub s b.pos b.len; *)
                lprintf "Trying to continue...\n";
                let msg_type, size = parse_head ciphers ss pos in
                Some (size + 5)
              end
            else None

      else None

(*************************************************************************)
(*                                                                       *)
(*                         string_of_path                                *)
(*                                                                       *)
(*************************************************************************)

    let string_of_path addr =
      match addr with
        DirectPacket -> "DirectPacket"
      | BroadcastPacket addr ->
          Printf.sprintf "BroadcastPacket (%s:%d, %d hops)"
            (ip_to_string addr.broadcast_source_ip)
          addr.broadcast_source_port
            addr.broadcast_hops
      | UnicastPacket addr ->
          Printf.sprintf "UnicastPacket (%s:%d -> %s:%d, %d hops)"
            (ip_to_string addr.unicast_source_ip)
          addr.unicast_source_port
            (ip_to_string addr.unicast_dest_ip)
          addr.unicast_dest_port
            addr.unicast_hops


(*************************************************************************)
(*                                                                       *)
(*                         parse                                         *)
(*                                                                       *)
(*************************************************************************)

    let parse ciphers s =
      match int_of_char s.[0] with
        0x50 -> DirectPacket, PingReq
      | 0x52 -> DirectPacket, PongReq
      | 0x4b ->
(*          lprintf "We have got a real packet\n"; *)
          let msg_type, size = parse_head ciphers s 0 in
(*                lprintf "Message to read: xtype %d type %d len %d\n"
                  xtype msg_type size; *)

          ciphers.in_xinu <- Int64.logxor ciphers.in_xinu
            (Int64.logand
              (Int64.lognot (Int64.of_int (size + msg_type)))
            int64_ffffffff);

          let msg_flags = (msg_type land 0xff00) lsr 8 in
          let msg_type = msg_type land 0xff in

          let pos, size, addr = match msg_flags with
            | 0x80 ->
                let source_ip = LittleEndian.get_ip s 5 in
                let source_port = get_int16  s 9 in
                let dest_ip = LittleEndian.get_ip s 11 in
                let dest_port = get_int16 s 15 in
                let hops = get_int8 s 17 in

                let addr = {
                    unicast_source_ip = source_ip;
                    unicast_source_port = source_port;
                    unicast_dest_ip = dest_ip;
                    unicast_dest_port = dest_port;
                    unicast_hops = hops;
                  } in

                5 + 13, size - 13, UnicastPacket addr
            | 0xc0 ->
                let source_ip = LittleEndian.get_ip s 5 in
                let source_port = get_int16  s 9 in
                let unknown = get_int16 s 11 in
                let hops = get_int8 s 13 in

                let addr = {
                    broadcast_source_ip = source_ip;
                    broadcast_source_port = source_port;
                    broadcast_unknown = unknown;
                    broadcast_hops = hops;
                  } in

                5 + 9, size - 9, BroadcastPacket addr
            | 0 ->
                5, size, DirectPacket

            | 0x61 (* This has been observed... *)
            | _ ->
                lprintf "WARNING:   MESSAGE HAS UNKNOWN FLAG %x\n" msg_flags;
                5, size, DirectPacket
          in
          let m = String.sub s pos size in
          addr, parse_packet msg_type m
      | n ->
          lprintf "WARNING parse: packet not understood: %d\n" n;
          dump s;
          DirectPacket, UnknownMessageReq (n, s)

(*************************************************************************)
(*                                                                       *)
(*                         bprint_tags                                   *)
(*                                                                       *)
(*************************************************************************)

    let bprint_tags b tags =
      List.iter (fun tag ->
          Printf.bprintf b "      Field: %s --> %s\n"
            (escaped_string_of_field tag)
          (string_of_tag_value tag.tag_value);
      ) tags

(*************************************************************************)
(*                                                                       *)
(*                         bprint_query                                  *)
(*                                                                       *)
(*************************************************************************)

    let bprint_query b s_uid query =
      match query with
      | QueryFilesReq (words, realm, terms) ->

          let realm = match realm with
              0x21 -> "audio"
            | 0x22 -> "video"
            | 0x23 -> "image"
            | 0x24 -> "text"
            | 0x25 -> "application"
            | 0x3f -> "any"
            | _ -> Printf.sprintf "realm=%d" realm
          in

          Printf.bprintf  b "QueryFiles (%d,%s,%s)\n"
            s_uid words realm;
          List.iter (fun (operator, tag) ->
              Printf.bprintf b "     %s %s\n"
                (match operator with
                | AtMost  -> "AtMost"
                | AtLeast -> "AtLeast"
                | Substring -> "Substring"
                | Equals -> "Equals"
                | Approx -> "Approx"
              ) (CommonGlobals.string_of_tag tag)
          ) terms;

      | QueryLocationReq hash ->
          Printf.bprintf b "QueryLocation (%d, %s)"
            s_uid (Md5Ext.to_string_case false hash)

(*************************************************************************)
(*                                                                       *)
(*                         to_string                                     *)
(*                                                                       *)
(*************************************************************************)

    let to_string t =
      match t with
      | NetworkNameReq m ->
          Printf.sprintf "NetworkName [%s]" m

      | NetworkGlobalStats (s, nets) ->
          let b = Buffer.create 100 in
          Printf.bprintf b "NetworkGlobalStats\n";
          bprint_ints b s;
          Printf.bprintf b "\n";
          List.iter (fun (netname, nusers) ->
              Printf.bprintf b "     %s: %Ld\n" netname nusers
          ) nets;
          Buffer.contents b

      | AskUDPConnectionReq (ip, port) ->
          Printf.sprintf "AskUDPConnection to %s:%d" (Ip.to_string ip) port

      | RandomFilesReq (sid, files) ->

          let b = Buffer.create 1000 in
          Printf.bprintf b "RandomFileReq (%d)\n" sid;
          List.iter (fun fd ->
              Printf.bprintf b "   title: %s\n" fd.fd_title;
              Printf.bprintf b "     realm: %d\n" fd.fd_realm;
              Printf.bprintf b "     artist: %s\n" fd.fd_artist;
              Printf.bprintf b "     unknown1: %s\n" fd.fd_unknown1;
              Printf.bprintf b "     unknown2: %s\n" fd.fd_unknown2;
          ) files;
          Buffer.contents b

      | UnshareFileReq sh ->

          let b = Buffer.create 1000 in
          Printf.bprintf b "UnshareFileReq (%d, %s, %Ld)\n"
            sh.shared_type
            (Md5Ext.to_string_case false sh.shared_hash)
          sh.shared_size;
          bprint_tags b sh.shared_tags;

          Buffer.contents b

      | ShareFileReq sh ->

          let b = Buffer.create 1000 in
          Printf.bprintf b "ShareFileReq (%d, %s, %Ld)\n"
            sh.shared_type
            (Md5Ext.to_string_case false sh.shared_hash)
          sh.shared_size;
          Printf.bprintf b "   %d tags\n" (List.length sh.shared_tags);
          bprint_tags b sh.shared_tags;

          Buffer.contents b

      | DeclareNeighbours neighbours ->
          let b = Buffer.create 1000 in
          Printf.bprintf b "DeclareNeighbours %d\n" (List.length neighbours);
          List.iter (fun n ->
              Printf.bprintf b "      x %s:%d (%d hops)          "
                (Ip.to_string n.neighbour_ip) n.neighbour_port
                n.neighbour_hops;
              bprint_ints b n.neighbour_info;
              Printf.bprintf b "\n"
          ) neighbours;
          Printf.bprintf b "\n";
          Buffer.contents b

      | Unknown_1e v ->  Printf.sprintf "Unknown_1e (%d)" v
      | Unknown_23 v ->  Printf.sprintf "Unknown_23 (%d)" v
      | Unknown_2b v ->  Printf.sprintf "Unknown_2b (%d)" v
      | Unknown_03 ->  Printf.sprintf "Unknown_03"

      | UnknownReq (opcode, m) ->
          let b = Buffer.create 1000 in
          let len = String.length m in
          Printf.bprintf b "Unknown packet [opcode = 0x%x, len=%d]\n" opcode
            len;
(*          let m = if len > 200 then String.sub m 0 200 else m in*)
          bdump b m;
          Buffer.contents b

      | UnknownMessageReq (opcode, m) ->
          let b = Buffer.create 1000 in
          let len = String.length m in
          Printf.bprintf b "Unknown message [header = 0x%x, len=%d]\n" opcode
            len;
          let m = if len > 200 then String.sub m 0 200 else m in
          bdump b m;
          Buffer.contents b

      | NodeListReq list ->
          let b = Buffer.create 1000 in
          Buffer.add_string b "NodeList\n";

          List.iter (fun (ip,port, seen, slots) ->
              try
                Printf.bprintf b  "      x %s:%d  seen:%d slots:%d\n"
                  (ip_to_string ip) port seen slots;
              with Not_found -> ()
          ) list;
          Buffer.contents b

      | QueryReplyReq ( (s_ip, s_port), id, results) ->
          let b = Buffer.create 1000 in
          Buffer.add_string b "QueryReply\n";
          Printf.bprintf b "  Supernode: %s:%d\n  Search: %d\n"
            (ip_to_string s_ip) s_port id;

          List.iter (fun (user, meta) ->
              Printf.bprintf b "   User %s@%s %s:%d\n"
                user.user_name
                user.user_netname
                (ip_to_string user.user_ip) user.user_port;

              Printf.bprintf b  "   Result %s size: %Ld tags: %d\n"
                (Md5Ext.to_string_case false meta.meta_hash)
              meta.meta_size (List.length meta.meta_tags);
              bprint_tags b meta.meta_tags

          ) results;

          Buffer.contents b

      | QueryReplyEndReq s_id -> Printf.sprintf "QueryReplyEnd %d" s_id
      | NetworkStatsReq (stats, netname, nusers) ->
          let b = Buffer.create 1000 in
          Printf.bprintf b "NetworkStats (%s, %Ld)\n" netname nusers;
          List.iter (fun stats ->
              Printf.bprintf b "     (nusers = %Ld, nfiles = %Ld, nkb = %d)\n"
                stats.nusers stats.nfiles stats.nkb
          ) stats;
          Buffer.contents b
      | PushRequestReq _ -> "PushRequest"
      | ExternalIpReq ip -> Printf.sprintf "ExternalIp %s" (ip_to_string ip)
      | PingReq -> "Ping"
      | PongReq -> "Pong"

      | SearchReq (max_results, s_uid, query) ->
          let b = Buffer.create 1000 in
          Printf.bprintf b "SearchReq (max_results = %d)" max_results;
          bprint_query b s_uid query;
          Buffer.contents b

      | SearchForwardReq (s, s_uid, query) ->
          let b = Buffer.create 1000 in
          Printf.bprintf  b "SearchForwardReq[%d,%d,%d,%d] "
            (int_of_char s.[0])
            (int_of_char s.[1])
            (int_of_char s.[2])
            (int_of_char s.[3]);
          bprint_query b s_uid query;
          Buffer.contents b

      | SearchForward2Req (s, s_uid, query) ->
          let b = Buffer.create 1000 in
          Printf.bprintf  b "SearchForward2Req[%d,%d,%d,%d] "
            (int_of_char s.[0])
            (int_of_char s.[1])
            (int_of_char s.[2])
            (int_of_char s.[3]);
          bprint_query b s_uid query;
          Buffer.contents b

      | NodeInfoReq (ip, port, bandwidth, name) ->
          Printf.sprintf "NodeInfo (%s:%d,%d,%s)" (ip_to_string ip) port bandwidth name
      | ProtocolVersionReq version ->
          Printf.sprintf "ProtocolVersion %d" version
  end (* module *)


(*************************************************************************)
(*                                                                       *)
(*                         UdpMessages                                   *)
(*                                                                       *)
(*************************************************************************)

module UdpMessages = struct

    type t =
      PingReq of int * string * string
    | SupernodePongReq of int * string * string
    | NodePongReq of int * string
    | UnknownReq of int * string
    let extract_string s pos =
      let end_pos = String.index_from s pos '\000' in
      String.sub s pos (end_pos - pos), pos + 1

    let parse p =
      match int_of_char p.[0] with
      | 0x27 ->
          let min_enc_type = get_int p 1 in
          let unknown = String.sub p 5 1 in
          let netname, pos = extract_string p 6 in

          PingReq (min_enc_type, unknown, netname)
      | 0x28 ->

          let min_enc_type = get_int p 1 in
          let unknown = String.sub p 5 6 in
          let netname, pos = extract_string p 11 in
          SupernodePongReq (min_enc_type, unknown, netname)

      | 0x29 ->
          let min_enc_type = get_int p 1 in
          let unknown = String.sub p 5 (String.length p - 5) in
          NodePongReq (min_enc_type, unknown)
      | n -> UnknownReq (n, p)

    let write p =
      let b = Buffer.create 100 in
      begin
        match p with
        | PingReq (min_enc_type, unknown, netname) ->
            buf_int8 b 0x27;
            buf_int b min_enc_type;
            Buffer.add_string b unknown;
            Buffer.add_string b netname;
            buf_int8 b 0x00
        | SupernodePongReq (min_enc_type, unknown, netname) ->
            buf_int8 b 0x28;
            buf_int b min_enc_type;
            Buffer.add_string b unknown;
            Buffer.add_string b netname;
            buf_int8 b 0x00
        | NodePongReq (min_enc_type, unknown) ->
            buf_int8 b 0x29;
            buf_int b min_enc_type;
            Buffer.add_string b unknown
        | UnknownReq (opcode, unknown) ->
            Buffer.add_string b unknown;
      end;
      Buffer.contents b

    let to_string p =
      let b = Buffer.create 100 in
      begin
        match p with
        | PingReq (min_enc_type, unknown, netname) ->
            Printf.bprintf b "Ping (%d, " min_enc_type;
            bprint_ints b unknown;
            Printf.bprintf b ", %s)" netname
        | SupernodePongReq (min_enc_type, unknown, netname) ->
            Printf.bprintf b "SupernodePong (%d, " min_enc_type;
            bprint_ints b unknown;
            Printf.bprintf b ", %s)" netname
        | NodePongReq (min_enc_type, unknown) ->
            Printf.bprintf b "NodePong (%d, " min_enc_type;
            bprint_ints b unknown;
            Printf.bprintf b ")"
        | UnknownReq (opcode, unknown) ->
            Printf.bprintf b "Unknown \n    ";
            bprint_ints b unknown;
            Printf.bprintf b  "\n    ";
            bprint_chars b unknown;
            Printf.bprintf b "\n"
      end;
      Buffer.contents b

    let udp_send t ip port ping msg =

      if !verbose_udp then begin
          lprintf "Message UDP to %s:%d\n%s\n" (Ip.to_string ip) port
            (to_string msg);
        end;

      try
        let s = write msg in
        UdpSocket.write t ping (Bytes.of_string s) ip port
      with e ->
          lprintf "FT: Exception %s in udp_send\n" (Printexc2.to_string e)

  end


(*************************************************************************)
(*                                                                       *)
(*                         server_send                                   *)
(*                                                                       *)
(*************************************************************************)

let server_send s addr t =
  match s.server_ciphers with
    None -> assert false
  | Some ciphers ->
      if !verbose_msg_servers then begin
          lprintf "\nSENDING node_to_supernode %s:%d: %s\n%s\n"
          (Ip.string_of_addr s.server_host.host_addr)
          s.server_host.host_port
          (TcpMessages.string_of_path addr)
          (TcpMessages.to_string t);
        end;

      let m = TcpMessages.write ciphers addr t in
      server_crypt_and_send s ciphers.out_cipher m


(*************************************************************************)
(*                                                                       *)
(*                         direct_server_send                            *)
(*                                                                       *)
(*************************************************************************)

let direct_server_send s t = server_send s TcpMessages.DirectPacket t

(*************************************************************************)
(*                                                                       *)
(*                         server_send_ping                              *)
(*                                                                       *)
(*************************************************************************)

let server_send_ping s =
  direct_server_send s TcpMessages.PingReq

(*************************************************************************)
(*                                                                       *)
(*                         server_send_pong                              *)
(*                                                                       *)
(*************************************************************************)

let server_send_pong s =
  server_send s TcpMessages.DirectPacket TcpMessages.PongReq

(*************************************************************************)
(*                                                                       *)
(*                         server_send_register_file                     *)
(*                                                                       *)
(*************************************************************************)

let server_send_register_file s sh =
  server_send s TcpMessages.DirectPacket (TcpMessages.ShareFileReq sh)

(*************************************************************************)
(*                                                                       *)
(*                         server_send_unregister_file                   *)
(*                                                                       *)
(*************************************************************************)

let server_send_unregister_file s sh =
  server_send s TcpMessages.DirectPacket (TcpMessages.UnshareFileReq sh)

(*************************************************************************)
(*                                                                       *)
(*                         OTHER PRIMITIVES                              *)
(*                                                                       *)
(*************************************************************************)

(* This function is used in Gnutella2 to resend non-acknowledged UDP packets *)
let resend_udp_packets _ = ()

(* TODO: this doesn't work properly on my computer !!! *)
let check_primitives () =
  (try
      let cipher = create_cipher () in
      set_cipher cipher 123456789l 0x29;
      init_cipher cipher;
      let s = Bytes.make 12 '0' in
      cipher_packet_set cipher s 0;
      assert (s = Bytes.of_string "\007\091\205\021\110\233\135\1870000"); 
      (* lprintf "cipher_packet_set s = \"%s\"\n" (String.escaped s); *)
      let s = Bytes.of_string "123456789abcdefghijklm\233\234\235" in
      apply_cipher cipher s 0 (Bytes.length s);
      assert (s = Bytes.of_string "\016\210\245\241\144Ug\028Z\229\1928\176\167\192\008\139\019\018Z\1937\226\250i"); 
      (* lprintf "apply_cipher s = \"%s\"\n" (String.escaped s); *)
      cipher_free cipher;
    with _ ->
  lprint_newline ();
        lprintf "The Fasttrack plugin will not work on your computer, since\n";
        lprintf "the encryption algorithm does not work correctly.\n";
        lprintf "You can try to solve this problem by hacking the C files in\n";
        lprintf "   mldonkey/src/networks/fasttrack/*.c \n")

let recover_files_delay = 600.

let translate_query q =
  let realm = ref "" in
  let keywords = ref [] in
  let add_words w =
    keywords := (String2.split_simplify w ' ') @ !keywords
  in
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
          | Field_Type -> realm := String.lowercase w
          | Field_Format ->
              begin
                match String.lowercase w with
                | "mp3" | "wav" ->
                    add_words w;
                    realm := "audio"
                | _ -> add_words w
              end
          | Field_Album
          | Field_Artist
          | Field_Title
          | Field_Codec
          | Field_KNOWN _
          | Field_UNKNOWN _
          | Field_Filename ->
              tags := (Substring, string_tag field w) :: !tags
          | Field_Bitrate
          | Field_Uid
          | Field_Completesources
          | Field_Length
          | Field_Availability
    | Field_Filerating
    | Field_Lastseencomplete
    | Field_Mediacodec
    | Field_Medialength
          | Field_Size_Hi
          | Field_Size -> ()
        end
    | QHasMinVal (field, value) ->
        begin
          match field with
          | Field_Size
          | Field_KNOWN _
            -> tags := (AtLeast, int64_tag field value) :: !tags
          | _ -> ()
        end
    | QHasMaxVal (field, value) ->
        begin
          match field with
          | Field_KNOWN _
          | Field_Size ->
              tags := (AtMost, int64_tag field value) :: !tags
          | _ -> ()
        end
    | QNone ->  ()
  in
  iter q;
  !keywords, (!realm, !tags)

let new_search_uid () =
  let s = !search_num in
  incr search_num;
  s

let cancel_recover_files file =
  List.iter (fun s ->
      Hashtbl.remove searches_by_uid s.search_uid
  ) file.file_searches

let parse_url url =

  match String2.split (String.escaped url) '|' with
  | "sig2dat://" :: file :: length :: uuhash :: _
  | "sig2dat:///" :: file :: length :: uuhash :: _ ->

      let filename =
        let len = String.length file in
        let rec iter1 pos =
          if pos = len then raise Exit;
          if file.[pos] = ':' then iter2 (pos+1)
          else iter1 (pos+1)
        and  iter2 pos =
          if pos = len then raise Exit;
          if file.[pos] = ' ' then iter2 (pos+1)
          else String.sub file pos (len - pos)
        in
        iter1 0
      in

      let size =

        let len = String.length length in
        let rec iter1 pos =
          if pos = len then raise Exit;
          if length.[pos] = ':' then iter2 (pos+1)
          else iter1 (pos+1)
        and  iter2 pos =
          if pos = len then raise Exit;
          if length.[pos] = ' ' then iter2 (pos+1)
          else iter3 pos (pos+1)
        and iter3 begin_pos pos =
          if pos = len then raise Exit;
          if length.[pos] = 'B' || length.[pos] = ' ' then
            String.sub length begin_pos (pos - begin_pos)
          else iter3 begin_pos (pos+1)
        in
        iter1 0

      in

      let hash =

        let len = String.length uuhash in
        let rec iter1 pos =
          if pos = len then raise Exit;
          if uuhash.[pos] = '=' then iter2 pos (pos+1)
          else iter1 (pos+1)
        and iter2 begin_pos pos =
          if pos = len then raise Exit;
          if uuhash.[pos] = '=' then
            String.sub uuhash begin_pos (pos+1 - begin_pos)
          else iter2 begin_pos (pos+1)
        in
        iter1 0

      in

      lprintf "sig2dat: [%s] [%s] [%s]\n" filename size hash;
      let size = Int64.of_string size in
      let hash = Md5Ext.of_string hash in
      file, size, [hash]
  | _ -> raise Not_found



let udp_send ip port m =
  let module M = UdpMessages in
  match !udp_sock with
    None -> failwith "No UDP socket !!!"
  | Some sock ->
      M.udp_send sock ip port m

let ask_for_push _ = ()
