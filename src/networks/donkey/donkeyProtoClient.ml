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

open Int64ops
open AnyEndian
open Printf2
open Md4
open CommonTypes
open LittleEndian
open CommonGlobals
open CommonOptions
open DonkeyTypes
open DonkeyMftp

let compatibleclient = ref 10

let get_emule_version () =
    (!compatibleclient lsl 24) lor
    (int_of_string(Autoconf.major_version) lsl 17) lor
    (int_of_string(Autoconf.minor_version) lsl 10) lor
    (int_of_string(Autoconf.sub_version) lsl 7)

let mldonkey_emule_proto = 
  {
    emule_version = get_emule_version (); 
    emule_release = "";
    emule_osinfosupport = 1;
    emule_features = 3;

(* emule_miscoptions1 *)
    received_miscoptions1 = false;
    emule_aich = 0;
    emule_unicode = 0;
    emule_udpver = 0;
    emule_compression = 1;
    emule_secident = 3; (* Emule uses v1 if advertising both, v2 if only advertising 2 *)
    emule_sourceexchange = 2; (* 2 : +client_md4 3 : +IdHybrid (emule Kademlia?)*)
    emule_extendedrequest = 1; (* 1: +file_status 2: +ncomplete_sources*)
    emule_comments = 1;
    emule_peercache = 0;
    emule_noviewshared = 0;
    emule_multipacket = 0;
    emule_supportpreview = 0;

(* emule_miscoptions2 *)
    received_miscoptions2 = false;
    emule_require_crypt = 0;
    emule_request_crypt = 0;
    emule_support_crypt = 0;
    emule_extmultipacket = 0;
    emule_largefiles = 1;
    emule_kad_version = 0;
    emule_support_captcha = 1;
  }

let emule_miscoptions1 m =
  let o =
    (m.emule_aich lsl 29) lor
    (m.emule_unicode lsl 28) lor
    (m.emule_udpver lsl 24) lor
    (m.emule_compression lsl 20) lor
    (m.emule_secident lsl 16) lor
    (m.emule_sourceexchange lsl 12) lor
    (m.emule_extendedrequest lsl 8) lor
    (m.emule_comments lsl 4) lor
    (m.emule_peercache lsl 3) lor
    (m.emule_noviewshared lsl 2) lor
    (m.emule_multipacket lsl 1) lor
    (m.emule_supportpreview lsl 0)
  in
  Int64.of_int o

let update_emule_proto_from_miscoptions1 m o =
  let o = Int64.to_int o in
  m.emule_aich            <- (o lsr 29) land 0x7;
  m.emule_unicode         <- (o lsr 28) land 0xf;
  m.emule_udpver          <- (o lsr 24) land 0xf;
  m.emule_compression     <- (o lsr 20) land 0xf;
  m.emule_secident        <- (o lsr 16) land 0xf;
  m.emule_sourceexchange  <- (o lsr 12) land 0xf;
  m.emule_extendedrequest <- (o lsr  8) land 0xf;
  m.emule_comments        <- (o lsr  4) land 0xf;
  m.emule_peercache       <- (o lsr  3) land 0x1;
  m.emule_noviewshared    <- (o lsr  2) land 0x1;
  m.emule_multipacket     <- (o lsr  1) land 0x1;
  m.emule_supportpreview  <- (o lsr  0) land 0x1

let print_emule_proto_miscoptions1 m =
  let buf = Buffer.create 50 in
  if m.emule_aich <> 0 then Printf.bprintf buf " aich %d\n" m.emule_aich;
  if m.emule_unicode <> 0 then Printf.bprintf buf " unicode %d\n" m.emule_unicode;
  if m.emule_udpver <> 0 then Printf.bprintf buf " udpver %d\n" m.emule_udpver;
  if m.emule_compression <> 0 then Printf.bprintf buf " compression %d\n" m.emule_compression;
  if m.emule_secident <> 0 then Printf.bprintf buf " secident %d\n" m.emule_secident;
  if m.emule_sourceexchange <> 0 then Printf.bprintf buf " sourceexchange %d\n" m.emule_sourceexchange;
  if m.emule_extendedrequest <> 0 then Printf.bprintf buf " extendedrequest %d\n" m.emule_extendedrequest;
  if m.emule_comments <> 0 then Printf.bprintf buf " comments %d\n" m.emule_comments;
  if m.emule_peercache <> 0 then Printf.bprintf buf " peercache %d\n" m.emule_peercache;
  if m.emule_noviewshared <> 0 then Printf.bprintf buf " noviewshared %d\n" m.emule_noviewshared;
  if m.emule_multipacket <> 0 then Printf.bprintf buf " multipacket %d\n" m.emule_multipacket;
  if m.emule_supportpreview <> 0 then Printf.bprintf buf " supportpreview %d\n" m.emule_supportpreview;
  Buffer.contents buf

let emule_miscoptions2 m =
  let o =
    (m.emule_support_captcha lsl 11) lor
    (m.emule_largefiles lsl 4)
  in
  Int64.of_int o

let update_emule_proto_from_miscoptions2 m o =
  let o = Int64.to_int o in
  m.emule_support_captcha <- (o lsr 11) land 0x1;
  m.emule_require_crypt   <- (o lsr 9)  land 0x1;
  m.emule_request_crypt   <- (o lsr 8)  land 0x1;
  m.emule_support_crypt   <- (o lsr 7)  land 0x1;
  m.emule_extmultipacket  <- (o lsr 5)  land 0x1;
  m.emule_largefiles      <- (o lsr 4)  land 0x1;
  m.emule_kad_version     <- (o lsr 0)  land 0xf

let print_emule_proto_miscoptions2 m =
  let buf = Buffer.create 50 in
  if m.emule_require_crypt <> 0 then Printf.bprintf buf " require_crypt %d\n" m.emule_require_crypt;
  if m.emule_request_crypt <> 0 then Printf.bprintf buf " request_crypt %d\n" m.emule_request_crypt;
  if m.emule_support_crypt <> 0 then Printf.bprintf buf " support_crypt %d\n" m.emule_support_crypt;
  if m.emule_extmultipacket <> 0 then Printf.bprintf buf " extmultipacket %d\n" m.emule_extmultipacket;
  if m.emule_largefiles <> 0 then Printf.bprintf buf " largefiles %d\n" m.emule_largefiles;
  if m.emule_kad_version <> 0 then Printf.bprintf buf " kad_version %d\n" m.emule_kad_version;
  if m.emule_support_captcha <> 0 then Printf.bprintf buf " support_captcha %d\n" m.emule_support_captcha;
  Buffer.contents buf

let emule_compatoptions m =
  (m.emule_osinfosupport lsl 0)

let update_emule_proto_from_compatoptions m o =
  m.emule_osinfosupport <- (o lsr 0) land 0x1

let extendedrequest e =
  min e.emule_extendedrequest mldonkey_emule_proto.emule_extendedrequest

let sourceexchange e =
  min e.emule_sourceexchange mldonkey_emule_proto.emule_sourceexchange

(*
BAD MESSAGE FROM CONNECTING CLIENT
UnknownReq:
ascii: [(1)(16)(231)(129)(131)(26) O(247)(154)(145)(251)(253)(167) G }(207) j(146)(140) { l(139) F(18)(0)(0)(0)(0)(0)(0)(0)(0)(0)(0)]
dec: [
(1)
(16)
(231)(129)(131)(26)(79)(247)(154)(145)(251)(253)(167)(71)(125)(207)(106)(146)
(140)(123)(108)(139)
(70)(18)
(0)(0)(0)(0)
(0)(0)(0)(0)(0)(0)
]
  *)
let rec lbprint_tags buf tags =
  match tags with
  [] -> Printf.bprintf buf ""
  | tag :: tags ->
   Printf.bprintf buf " (%s)=(%s)" (escaped_string_of_field tag)
   (string_of_tag_value tag.tag_value);
    lbprint_tags buf tags

module Connect  = struct
    type t = {
        hash_len : int;
        md4 : Md4.t;
        ip: Ip.t;
        port: int;
        tags : tag list;
        server_info : (Ip.t * int) option;
        left_bytes : string;
      }

    let names_of_tag = client_common_tags

    let names_of_tag =
      List.map (fun (v, name) -> (v, Field_KNOWN name)) names_of_tag

    let parse reply len s =
      let hash_len, pos = if not reply then get_uint8 s 1, 2 else -1, 1 in
      let md4 = get_md4 s pos in
      let ip = get_ip s (pos+16) in
      let port = get_port s (pos+20) in
      let tags, pos = get_tags s (pos+22) names_of_tag in
      let server_info = Some (get_ip s pos, get_port s (pos+4)) in
      let left_bytes = String.sub s (pos+6) (String.length s - pos - 6) in
      {
        hash_len = hash_len;
        md4 = md4;
        ip = ip;
        port = port;
        tags = tags;
        server_info = server_info;
        left_bytes = left_bytes;
      }


    let print t =
      let b1 = Buffer.create 50 in
      let b2 = Buffer.create 5 in
      lbprint_tags b1 t.tags;
      String.iter (fun c -> Printf.bprintf b2 "(%d)" (int_of_char c)) t.left_bytes;
      lprintf_nl "Connect [hl: %d] [md4: %s] [ip: %s:%d] [server: %s] [left: %s] [tags:%s]" 
        t.hash_len
        (Md4.to_string t.md4)
        (Ip.to_string t.ip) t.port
        (match t.server_info with
          None -> "None"  
        | Some (ip, port) -> Printf.sprintf "%s:%d" (Ip.to_string ip) port)
        (if String.length t.left_bytes <> 0 then (Buffer.contents b2) else "None")
        (Buffer.contents b1)


    let write reply buf t =
      if not reply then
        buf_int8 buf 16;

      buf_md4 buf t.md4;
      buf_ip buf t.ip;
      buf_port buf t.port;
      buf_tags buf t.tags names_of_tag;
      begin
        match t.server_info with
          None ->
            buf_ip buf Ip.null;
            buf_port buf 0
        | Some (ip, port) ->
            buf_ip buf ip;
            buf_port buf port;
      end

  end

module Say = struct
    type t = string

    let parse len s =
      let (s, p) = get_string s 1 in
      s

    let print t =
      lprintf_nl "SAY %s" t

    let write buf t =
      buf_string buf t
  end

module OneMd4 = functor(M: sig val m : string end) -> (struct
    type t = Md4.t

    let parse len s =
      get_md4 s 1

    let print t =
          lprintf_nl "OneMd4: %s OF %s" M.m (Md4.to_string t)

    let write buf t =
      buf_md4 buf t
      
  end)

module JoinQueue = struct
    type t = Md4.t option

    let parse len s =
      if len >= 17 then
        Some (get_md4 s 1)
      else None

    let print t =
      lprintf_nl "JOIN QUEUE %s"
      (match t with None -> "" | Some md4 ->
            Printf.sprintf "OF %s" (Md4.to_string md4))

    let write emule buf t =
      if extendedrequest emule > 0 then
        match t with
          None -> ()
        | Some md4 ->
            buf_md4 buf md4
  end
    (*
      : sig
        type t
          val parse : int -> string  -> t
          val print : t -> unit
          val write : Buffer.t -> t  -> unit
          val t :t
          end
      )
*)

(* In Emule, this message contains much more information, and will probably
remove the need for QueryChunks. *)

let get_bitmap s pos =
  let nchunks = get_int16 s pos in
  let chunks, pos =
    if nchunks = 0 then (Bitv.create 0 false), pos+2 else
    let pos = pos + 2 in
    let chunks = (Bitv.create nchunks false) in
    for i = 0 to (nchunks-1) / 8 do
      let m = get_uint8 s (pos + i) in
      for j = 0 to 7 do
        let n = i * 8 + j in
        if n < nchunks then
          Bitv.set chunks n ((m land (1 lsl j)) <> 0);
      done;
    done;
    let pos = pos + (nchunks-1)/8 + 1 in
    chunks, pos
  in
  chunks, pos

let print_bitmap chunks =
  lprintf "\n%s\n" (Bitv.to_string chunks)

let write_bitmap buf chunks =
  let nchunks = Bitv.length chunks in
  buf_int16 buf nchunks;
  if nchunks > 0 then
  for i = 0 to (nchunks-1) / 8 do
    let m = ref 0 in
    for j = 0 to 7 do
      let n = i * 8 + j in
      if n < nchunks then
        if (Bitv.get chunks n) then
          m :=  !m lor (1 lsl j);
    done;
    buf_int8 buf !m
  done

module QueryFile  = struct
    type t = {
        md4 : Md4.t;
        emule_extension : (Bitv.t * int) option;
      }

    let parse emule len s =
(*      lprintf "Query File: emule version %d len %d"
      (extendedrequest emule) len;
      print_newline (); *)
      let md4 = get_md4 s 1 in
      let emule_extension =
        try
          if len < 18 || extendedrequest emule = 0 then None else
          let chunks, pos = get_bitmap s 17 in
          let ncompletesources =
            if extendedrequest emule > 1 && len > pos+1 then get_int16 s pos
            else -1 in
          Some (chunks, ncompletesources)
        with _ -> None
      in
      { md4 = md4;
        emule_extension = emule_extension }

    let print t =
      lprintf_nl "QUERY FILE OF %s" (Md4.to_string t.md4);
      match t.emule_extension with
        None -> ()
      | Some (bitmap, ncompletesources) ->
          print_bitmap bitmap;
          lprint_newline ();
          if ncompletesources >= 0 then
            lprintf_nl "Complete sources: %d" ncompletesources

    let write emule buf t =
      buf_md4 buf t.md4;
      match t.emule_extension with
        None -> ()
      | Some (chunks, ncompletesources) ->
          if extendedrequest emule > 0 then begin
              write_bitmap buf chunks;
              if extendedrequest emule > 1 && ncompletesources >= 0 then
                buf_int16 buf ncompletesources
            end
  end

module QueryChunks  = OneMd4(struct let m = "QUERY CHUNKS" end)
  (* Request 79 *)

module QueryChunkMd4  = OneMd4(struct let m = "QUERY CHUNKS MD4" end)
module EndOfDownload  = OneMd4(struct let m = "END OF DOWNLOAD MD4" end)
module NoSuchFile  = OneMd4(struct let m = "NO SUCH FILE" end)

module QueryChunksReply = struct (* Request 80 *)

    type t = {
        md4 : Md4.t;
        chunks: Bitv.t;
      }

    let parse len s =
      let md4 = get_md4 s 1 in
      let chunks, pos = get_bitmap s 17 in
      {
        md4 = md4;
        chunks = chunks;
      }

    let print t =
      lprintf_nl "CHUNKS for %s" (Md4.to_string t.md4);
      lprintf_nl "%s\n" (Bitv.to_string t.chunks)

    let write buf t =
      buf_md4 buf t.md4;
      write_bitmap buf t.chunks;
      if Bitv.length t.chunks = 0 then buf_int8 buf 0
  end
(*
dec: [(96)(215)(1)(0)(0)(0)(0)(0)(0)(0)(0)(0)(0)]

OP_QUEUERANKING: int16
*)

module QueryChunkMd4Reply = struct (* Request 80 *)

    type t = {
        md4 : Md4.t;
        chunks: Md4.t array;
      }

    let parse len s =
      let md4 = get_md4 s 1 in
      let nchunks = get_int16 s 17 in
(*      lprintf "nchunks : %d" nchunks; lprint_newline (); *)
      let chunks = Array.make nchunks md4  in
      for i = 0 to nchunks - 1 do
        chunks.(i) <- get_md4 s (19 + i * 16)
      done;
      {
        md4 = md4;
        chunks = chunks;
      }

    let print t =
      lprintf_nl "CHUNKSMd4 for %s" (Md4.to_string t.md4);
      lprint_string "   ";
      Array.iter (fun b ->
          lprintf "  %s" (Md4.to_string b))
      t.chunks;
      lprint_newline ()

    let write buf t =
      buf_md4 buf t.md4;
      let nchunks = Array.length t.chunks in
      buf_int16 buf nchunks;
      for i = 0 to nchunks - 1 do
        buf_md4 buf t.chunks.(i)
      done
  end

module QueryFileReply  = struct
    type t = {
        md4 : Md4.t;
        name : string;
      }

    let parse len s =
      let name, _ = get_string s 17 in
      { md4 = get_md4 s 1;
        name =  name;
      }

    let print t =
      lprintf_nl "QUERY FILE REPLY OF %s : \"%s\"" (Md4.to_string t.md4) t.name

    let write buf t =
      buf_md4 buf t.md4;
      buf_string buf t.name
  end

module Bloc  = struct
    type t = {
        md4 : Md4.t;
        usesixtyfour : bool;
        start_pos : int64;
        end_pos: int64;
        bloc_str: bytes;
        bloc_begin : int;
        bloc_len : int;
      }

    let parse usesixtyfour len s =
      {
        md4 = get_md4 s 1;
        usesixtyfour = usesixtyfour;
        start_pos = if usesixtyfour then get_int64 s 17 else get_uint64_32 s 17;
        end_pos   = if usesixtyfour then get_int64 s 25 else get_uint64_32 s 21;
        bloc_str = (Bytes.of_string s);
        bloc_begin = if usesixtyfour then 33 else 25;
        bloc_len = if usesixtyfour then len - 33 else len - 25;
      }

    let print t =
      lprintf_nl "BLOC OF %s len %Ld [%Ld - %Ld] " (Md4.to_string t.md4)
      (t.end_pos -- t.start_pos)
      t.start_pos
        t.end_pos

    let write buf t =
      buf_md4 buf t.md4;
      if t.usesixtyfour then buf_int64 buf t.start_pos else buf_int64_32 buf t.start_pos;
      if t.usesixtyfour then buf_int64 buf t.end_pos else buf_int64_32 buf t.end_pos;
      Buffer.add_subbytes buf t.bloc_str t.bloc_begin t.bloc_len
  end

module QueryBloc  = struct
    type t = {
        md4 : Md4.t;
        usesixtyfour : bool;
        start_pos1 : int64; (* 180 ko *)
        end_pos1: int64;
        start_pos2 : int64;
        end_pos2: int64;
        start_pos3 : int64;
        end_pos3: int64;
      }

    let parse usesixtyfour len s =
      {
        md4 = get_md4 s 1;
        usesixtyfour = usesixtyfour;
        start_pos1 = if usesixtyfour then get_int64 s 17 else get_uint64_32 s 17;
        end_pos1   = if usesixtyfour then get_int64 s 41 else get_uint64_32 s 29;
        start_pos2 = if usesixtyfour then get_int64 s 25 else get_uint64_32 s 21;
        end_pos2   = if usesixtyfour then get_int64 s 49 else get_uint64_32 s 33;
        start_pos3 = if usesixtyfour then get_int64 s 33 else get_uint64_32 s 25;
        end_pos3   = if usesixtyfour then get_int64 s 57 else get_uint64_32 s 37;
      }

    let print t =
      lprintf_nl "QUERY BLOCS OF %s [%s - %s] [%s - %s] [%s - %s]"
      (Md4.to_string t.md4)
      (Int64.to_string t.start_pos1) (Int64.to_string t.end_pos1)
      (Int64.to_string t.start_pos2) (Int64.to_string t.end_pos2)
      (Int64.to_string t.start_pos3) (Int64.to_string t.end_pos3)

    let write buf t =
      buf_md4 buf t.md4;
      if t.usesixtyfour then buf_int64 buf t.start_pos1 else buf_int64_32 buf t.start_pos1;
      if t.usesixtyfour then buf_int64 buf t.start_pos2 else buf_int64_32 buf t.start_pos2;
      if t.usesixtyfour then buf_int64 buf t.start_pos3 else buf_int64_32 buf t.start_pos3;
      if t.usesixtyfour then buf_int64 buf t.end_pos1 else buf_int64_32 buf t.end_pos1;
      if t.usesixtyfour then buf_int64 buf t.end_pos2 else buf_int64_32 buf t.end_pos2;
      if t.usesixtyfour then buf_int64 buf t.end_pos3 else buf_int64_32 buf t.end_pos3
  end

let unit = ()
module NoArg = functor(M: sig val m : string end) -> (struct
        type t = unit

        let parse len s = ()

        let print t =
          lprintf_nl "%s:" M.m

        let write (buf: Buffer.t) (t: t) = unit

        let t = (() : t)
      end : sig
        type t
          val parse : int -> string  -> t
          val print : t -> unit
          val write : Buffer.t -> t  -> unit
          val t :t
          end
      )

module AvailableSlot = NoArg(struct let m = "AvailableSlot" end)
module ReleaseSlot = NoArg(struct let m = "ReleaseSlot" end)
module OutOfParts = NoArg(struct let m = "OutOfParts" end)
module ViewFiles = NoArg(struct let m = "VIEW FILES" end)
module ViewDirs = NoArg(struct let m = "VIEW DIRS" end)

module ViewFilesReply = struct

    type file = {
        md4: Md4.t;
        ip: Ip.t;
        port: int;
        tags:  tag list;
      }

    type t = tagged_file list

    let names_of_tag = file_common_tags

    let rec get_files  s pos n =
      if n = 0 then [], pos else
      let md4 = get_md4 s pos in
      let ip = get_ip s (pos + 16) in
      let port = get_port s (pos + 20) in
      let tags, pos = get_tags s (pos+22) names_of_tag in
      let file = {
          f_md4 = md4;
          f_ip = ip;
          f_port = port;
          f_tags = tags;
        } in
      let files, pos =  get_files s pos (n-1) in
      file :: files, pos

    let parse len s =
      let n = get_int s 1 in
      let files, pos = get_files s 5 n in
      files

    let print t =
      lprintf_nl "VIEW FILES REPLY:";
      List.iter (fun t ->
          lprintf_nl "FILE:";
          lprintf_nl "  MD4: %s" (Md4.to_string t.f_md4);
          lprintf_nl "  ip: %s" (Ip.to_string t.f_ip);
          lprintf_nl "  port: %d" t.f_port;
          lprintf "  tags: ";
          print_tags t.f_tags;
          lprint_newline ();) t

    let rec write_files buf files =
      match files with
        [] -> ()
      | file :: files ->
          buf_md4 buf file.f_md4;
          buf_ip buf file.f_ip;
          buf_port buf file.f_port;
          buf_tags buf file.f_tags names_of_tag;
          write_files buf files

    let write buf t =
      buf_int buf (List.length t);
      write_files buf t

    let rec write_files_max buf files nfiles max_len =
      let prev_len = Buffer.length buf in
      match files with
        [] -> nfiles, prev_len
      | file :: files ->
          buf_md4 buf file.f_md4;
          buf_ip buf file.f_ip;
          buf_port buf file.f_port;
          buf_tags buf file.f_tags names_of_tag;
          if Buffer.length buf < max_len then
            write_files_max buf files (nfiles+1) max_len
          else
            nfiles, prev_len
  end

module ViewDirsReply = struct

    type t = string list

    let rec get_dirs s pos n =
      if n = 0 then [], pos else
      let dir, pos = get_string16 s pos in
      let dirs, pos = get_dirs s pos (n-1) in
      dir :: dirs, pos

    let parse len s =
      let dirs, pos = get_dirs s 2 (get_int16 s 0) in
      dirs

    let print t =
      lprintf_nl "VIEW DIRS REPLY:";
      List.iter (fun dir ->
          lprintf_nl "DIR: %s" dir;) t

    let write buf t =
      buf_int buf (List.length t);
      List.iter (fun dir ->
        buf_string buf dir;) t

  end

module ViewFilesDir = struct

    type t = string

    let print t =
      lprintf_nl "VIEW FILES DIR: %s" t

    let parse len s =
      let dir, pos = get_string s 1 in
      dir

    let write buf t =
      buf_string buf t

  end

module ViewFilesDirReply = struct

    type t = string * tagged_file list

    let names_of_tag = file_common_tags

    let parse len s =
      let dir, pos = get_string s 1 in
      let n = get_int s (pos+1) in
      let files, pos = ViewFilesReply.get_files s (pos+5) n in
      dir, files

    let print t =
      lprintf_nl "VIEW FILES DIR REPLY:";
      let dir, files = t in begin
        lprintf_nl "DIR: %s" dir;
        List.iter (fun file ->
            lprintf_nl "FILE:";
            lprintf_nl "  MD4: %s" (Md4.to_string file.f_md4);
            lprintf_nl "  ip: %s" (Ip.to_string file.f_ip);
            lprintf_nl "  port: %d" file.f_port;
            lprintf "  tags: ";
            print_tags file.f_tags;
            lprint_newline ();) files
      end

    let write buf t =
      let dir, files = t in begin
        buf_string buf dir;
        buf_int buf (List.length files);
        ViewFilesReply.write_files buf files
      end

  end

module OtherLocations = struct

    type t = Ip.t list

    let parse len s =
      let list = ref [] in
      for i = 0 to len / 4 - 1 do
        list := (get_ip s (i*4+1)) :: !list;
      done;
      !list

    let print t =
      lprintf_nl "OTHER LOCATIONS:\n";
      List.iter (fun ip ->
          lprintf_nl "  ip: %s" (Ip.to_string ip);) t

    let write buf t =
      List.iter (buf_ip buf) t
  end

module NewUserID = struct

    type t = Ip.t * Ip.t

    let parse len s =
      get_ip s 1, get_ip s 5

    let print (ip1,ip2) =
      lprintf_nl "NEW USER ID: %s -> %s" (Ip.to_string ip1)
      (Ip.to_string ip2)

    let write buf (ip1,ip2) =
      buf_ip buf ip1;
      buf_ip buf ip2
  end


module Sources = struct

    type t = {
        md4: Md4.t;
        sources : (Ip.t * int * Ip.t) list;
      }

    let parse len s =
      let len = get_int16 s 1 in
      let md4 = get_md4 s 3 in
      let list = ref [] in
(*      let pos = 19 in *)
      for i = 0 to len - 1 do
        list := (get_ip s (19 + 10 * i), get_port s (23 + 10 * i),
          get_ip  s (25 + 10 * i)) :: !list;
      done;
      { md4 = md4;
        sources = !list;
      }

    let print t =
      lprintf_nl "SOURCES for %s:" (Md4.to_string t.md4);
      List.iter (fun (ip1, port, ip2) ->
          lprintf_nl "  %s:%d:%s" (Ip.to_string ip1) port(Ip.to_string ip2)) t.sources

    let write buf t =
      buf_int16 buf (List.length t.sources);
      buf_md4 buf t.md4;
      List.iter (fun (ip1, port, ip2) ->
          buf_ip buf ip1;
          buf_port buf port;
          buf_ip buf ip2) t.sources
  end

module EmuleClientInfo = struct

    type t = {
        version : int; (* CURRENT_VERSION_SHORT = !!emule_protocol_version *)
        protversion : int; (* EMULE_PROTOCOL_VERSION = 0x1 *)
        mutable tags : tag list;
      }

    let names_of_tag = client_common_tags

    let names_of_tag =
      List.map (fun (v, name) -> (v, Field_KNOWN name)) names_of_tag

    let parse len s =
      let version = get_uint8 s 1 in
      let protversion = get_uint8 s 2 in
      let tags,_ = get_tags s 3 names_of_tag in
      {
        version = version;
        protversion = protversion;
        tags = tags;
      }

    let print m t =
      let b1 = Buffer.create 50 in
      lbprint_tags b1 t.tags;
      lprintf_nl "%s: [version: %d] [protversion: %d] [tags:%s]" m t.version t.protversion (Buffer.contents b1)

    let write buf t =
      buf_int8 buf t.version;
      buf_int8 buf t.protversion;
      buf_tags buf t.tags names_of_tag;

  end

module EmuleQueueRanking = struct

    type t = int

    let parse len s = get_int16 s 1
    let print t =
      lprintf_nl "EmuleQueueRanking: %d" t

    let string_null10 = String.make 10 (char_of_int 0)

    let write buf t =
      buf_int16 buf t;
      Buffer.add_string buf string_null10

  end

module QueueRank = struct

    type t = int

    let parse len s = get_int s 1
    let print t =
      lprintf_nl "QueueRank: %d" t

    let write buf t =
      buf_int buf t

  end

module EmuleRequestSources = struct

    type t =  Md4.t

    let parse len s =
      get_md4 s 1

    let print t =
      lprintf_nl "EmuleRequestSources: %s" (Md4.to_string t)

    let write buf t =
      buf_md4 buf t

  end


let buf_estring buf s =
  let len = String.length s in
  buf_int8 buf len;
  Buffer.add_string buf s

module EmuleSignatureReq = struct

    type t = {
        signature : string;
        ip_type : int;
      }

    let print t =
      lprintf_nl "EmuleSignatureReq [type %d] [sig(%d): %s]" t.ip_type (String.length t.signature) (String.escaped t.signature)

    let parse len s =
      let mlen = get_uint8 s 1 in
      let slen = String.length s in
      let signature = String.sub s 2 mlen in
      let ip_type = if mlen = (slen-2) then 0 else get_uint8 s (2 + mlen) in
      {
        signature = signature;
        ip_type = ip_type;
      }

    let write buf t =
      buf_estring buf t.signature;
      if (t.ip_type <> 0) then
           buf_int8 buf t.ip_type;

  end


module EmulePublicKeyReq = struct

    type t = string

    let print t =
      lprintf_nl "EmulePublicKeyReq [key(%d): %s]" (String.length t) (String.escaped t)

    let parse len s =
      let len = get_uint8 s 1 in
      String.sub s 2 len

    let write buf t =
      buf_estring buf t

  end


module EmuleCaptchaReq = struct

    type t = string

    let print t =
      lprintf_nl "EmuleCaptchaReq [CAPTCHA BMP length=%d bytedata=%s]" (String.length t) (String.escaped t)

    let parse len s =
      String.sub s 2 (len - 2)

    let write buf t =
      buf_estring buf t

  end


module EmuleCaptchaRes = struct

    type t = int

    let print t =
      lprintf_nl "EmuleCaptchaRes RESPONSE=%d" t

    let parse s =
      get_uint8 s 1

    let write buf t =
      buf_int8 buf t

  end


module EmuleSecIdentStateReq = struct

    type t = {
        state : int;
        challenge : int64;
      }

    let print t =
      lprintf_nl "EmuleSecIdentStateReq [state: %d] [challenge: %Ld]" t.state t.challenge

    let parse len s =
      let state = get_uint8 s 1 in
      let challenge = get_uint64_32 s 2 in
      {
        state = state;
        challenge = challenge;
      }

    let write buf t =
      buf_int8 buf t.state;
      buf_int64_32 buf t.challenge

  end

module EmuleRequestSourcesReply = struct

    type source = {
        src_ip : Ip.t;
        src_port : int;
        mutable src_server_ip : Ip.t;
        mutable src_server_port : int;
        mutable src_md4 : Md4.t;
        mutable src_cc : int option;
      }

    type t = {
        md4 : Md4.t;
        sources : source array;
      }

    let dummy_source = {
        src_ip = Ip.null;
        src_port = 0;
        src_server_ip = Ip.null;
        src_server_port = 0;
        src_md4 = Md4.null;
        src_cc = None;
      }

    let parse e len s =
      let md4 = get_md4 s 1 in
      let ncount = get_int16 s 17 in

      let sources =
        if ncount = 0 then [||] else
        let slen = (len - 19) / ncount in
(*        lprintf "PER SOURCES LEN: %d\n" slen; *)
        let sources = Array.make ncount dummy_source in
        let rec iter pos i =
          if i < ncount then
            let ss =
              let ip = get_ip s pos in
              {
                dummy_source with
                src_ip = ip;
                src_port = get_int16 s (pos+4);
                src_cc = Geoip.get_country_code_option ip
              } in
            let pos =
              if slen > 6 then begin
                  ss.src_server_ip <- get_ip s (pos+6);
                  ss.src_server_port <- get_int16 s (pos+10);
                  if slen > 12 && (sourceexchange e > 1) then begin
                      ss.src_md4 <- get_md4 s (pos+12);
                      pos + 28
                    end else
                    pos + 12
                end
              else pos + 6
            in
            sources.(i) <- ss;
            iter pos (i+1)
        in
        iter 19 0;
        sources
      in
      {
        md4 = md4;
        sources = sources;
      }

    let print t =
      let ncount = Array.length t.sources in
      lprintf_nl "EMULE SOURCES REPLY: %d sources for %s"
        ncount (Md4.to_string t.md4);
       for i = 0 to ncount - 1 do
        let s = t.sources.(i) in
          lprintf_nl "%s %s"
          (if Ip.valid s.src_ip then
              Printf.sprintf "%s:%d" (Ip.to_string s.src_ip) s.src_port
          else
              Printf.sprintf  "%s:%d (Indirect)" (Ip.to_string s.src_server_ip) s.src_server_port)
          (if s.src_md4 != Md4.null then
              Printf.sprintf "MD4: %s" (Md4.to_string s.src_md4)
          else "")
      done

    let write e buf t =
      buf_md4 buf t.md4;
      let ncount = Array.length t.sources in
      buf_int16 buf ncount;

      for i = 0 to ncount - 1 do
        let s = t.sources.(i) in
          buf_ip buf s.src_ip;
          buf_port buf s.src_port;
          if sourceexchange e > 0 then begin
              buf_ip buf s.src_server_ip;
              buf_port buf s.src_server_port;
              if sourceexchange e > 1 then
                buf_md4 buf s.src_md4
            end
      done

  end
module EmuleFileDesc = struct

    type t = {
        rating : int;
        comment : string;
      }

    let parse len s =
      let rating = get_uint8 s 1 in
      let (comment, _) = get_string32 s 2 in
      {
        rating = rating;
        comment = comment;
      }

    let print t =
      lprintf_nl "EmuleFileDesc [%d][%s]" t.rating t.comment

    let write buf t =
      buf_int8 buf t.rating;
      buf_string buf t.comment
  end

module EmuleCompressedPart = struct

    type t = {
        md4 : Md4.t;
        usesixtyfour : bool;
        statpos : int64;
        newsize : int64;
        bloc : string;
      }

    let parse usesixtyfour len s =
      {
        md4 = get_md4 s 1;
        usesixtyfour = usesixtyfour;
        statpos = if usesixtyfour then get_int64 s 17 else get_uint64_32 s 17;
        newsize = if usesixtyfour then get_uint64_32 s 25 else get_uint64_32 s 21;
        bloc = if usesixtyfour then String.sub s 29 (len-29) else String.sub s 25 (len-25)
      }

    let print t =
      lprintf_nl "EmuleCompressedPart for %s %Ld %Ld len %d"
        (Md4.to_string t.md4) t.statpos t.newsize (String.length t.bloc)

    let write buf t =
      buf_md4 buf t.md4;
      if t.usesixtyfour then buf_int64 buf t.statpos else buf_int64_32 buf t.statpos;
      buf_int64_32 buf t.newsize;
      Buffer.add_string buf t.bloc
  end

module EmulePortTestReq = struct

    type t = string

    let print s =
      lprintf_nl "Emule porttest request %s" (String.escaped s)

    let parse s = s

    let write buf =
      buf_int8 buf 0x12

  end

type t =
| ConnectReq of Connect.t
| ConnectReplyReq of Connect.t
| QueryFileReq of QueryFile.t
| QueryFileReplyReq of QueryFileReply.t
| BlocReq of Bloc.t
| QueryBlocReq of QueryBloc.t
| JoinQueueReq of JoinQueue.t (* sent before queryBloc *)
| AvailableSlotReq of AvailableSlot.t
| ReleaseSlotReq of ReleaseSlot.t
| OutOfPartsReq of OutOfParts.t
| QueryChunksReq of QueryChunks.t
| QueryChunksReplyReq of QueryChunksReply.t
| QueryChunkMd4Req of QueryChunkMd4.t
| QueryChunkMd4ReplyReq of QueryChunkMd4Reply.t
| ViewFilesReq of ViewFiles.t
| ViewFilesReplyReq of ViewFilesReply.t
| ViewDirsReq of ViewDirs.t
| ViewDirsReplyReq of ViewDirsReply.t
| ViewFilesDirReq of ViewFilesDir.t
| ViewFilesDirReplyReq of ViewFilesDirReply.t
| QueueReq of OtherLocations.t
| UnknownReq of int * string
| OtherLocationsReq of OtherLocations.t
| SayReq of Say.t
| SourcesReq of Sources.t
| EndOfDownloadReq of EndOfDownload.t
| NewUserIDReq of NewUserID.t
| NoSuchFileReq of NoSuchFile.t
| QueueRankReq of QueueRank.t

| EmuleClientInfoReq of EmuleClientInfo.t
| EmuleClientInfoReplyReq of EmuleClientInfo.t
| EmuleQueueRankingReq of EmuleQueueRanking.t
| EmuleRequestSourcesReq of EmuleRequestSources.t
| EmuleRequestSourcesReplyReq of EmuleRequestSourcesReply.t
| EmuleFileDescReq of EmuleFileDesc.t 
| EmulePublicKeyReq of EmulePublicKeyReq.t
| EmuleSignatureReq of EmuleSignatureReq.t
| EmuleSecIdentStateReq  of EmuleSecIdentStateReq.t
| EmuleMultiPacketReq of Md4.t * t list
| EmuleMultiPacketAnswerReq of Md4.t * t list
| EmuleCompressedPart of EmuleCompressedPart.t
| EmulePortTestReq of EmulePortTestReq.t
| EmuleCaptchaReq of EmuleCaptchaReq.t
| EmuleCaptchaRes of EmuleCaptchaRes.t

let rec print t =
  begin
    match t with
    | ConnectReq t -> Connect.print t
    | ConnectReplyReq t -> Connect.print t
    | QueryFileReq t -> QueryFile.print t
    | QueryFileReplyReq t -> QueryFileReply.print t
    | BlocReq t -> Bloc.print t
    | QueryBlocReq t -> QueryBloc.print t
    | JoinQueueReq t -> JoinQueue.print t
    | AvailableSlotReq t -> AvailableSlot.print t
    | ReleaseSlotReq t -> ReleaseSlot.print t
    | OutOfPartsReq t -> OutOfParts.print t
    | QueryChunksReq t -> QueryChunks.print t
    | QueryChunksReplyReq t -> QueryChunksReply.print t
    | QueryChunkMd4Req t -> QueryChunkMd4.print t
    | QueryChunkMd4ReplyReq t -> QueryChunkMd4Reply.print t
    | ViewFilesReplyReq t -> ViewFilesReply.print t
    | ViewFilesReq t -> ViewFiles.print t
    | ViewDirsReq t -> ViewDirs.print t
    | ViewDirsReplyReq t -> ViewDirsReply.print t
    | ViewFilesDirReq t -> ViewFilesDir.print t
    | ViewFilesDirReplyReq t -> ViewFilesDirReply.print t
    | QueueReq t -> OtherLocations.print t
    | OtherLocationsReq t  -> OtherLocations.print t
    | SayReq t -> Say.print t
    | SourcesReq t -> Sources.print t
    | EndOfDownloadReq t -> EndOfDownload.print t
    | NewUserIDReq t -> NewUserID.print t
    | NoSuchFileReq t -> NoSuchFile.print t
    | QueueRankReq t ->
        QueueRank.print t

    | EmuleClientInfoReq t ->
        EmuleClientInfo.print "EmuleClientInfo"  t
    | EmuleClientInfoReplyReq t ->
        EmuleClientInfo.print "EmuleClientInfoReply" t
    | EmuleQueueRankingReq t ->
        EmuleQueueRanking.print t
    | EmuleRequestSourcesReq t ->
        EmuleRequestSources.print  t
    | EmuleRequestSourcesReplyReq t ->
        EmuleRequestSourcesReply.print t

    | EmuleFileDescReq t ->
        EmuleFileDesc.print t

    | EmuleMultiPacketReq (md4, list) ->
        lprintf_nl "EmuleMultiPacket for %s:" (Md4.to_string md4);
        List.iter (fun t ->
            lprintf "  ";
            print t
        ) list

    | EmuleMultiPacketAnswerReq (md4, list) ->
        lprintf_nl "EmuleMultiPacketAnswer for %s:" (Md4.to_string md4);
        List.iter (fun t ->
            lprintf "  ";
            print t
        ) list
    | EmuleSecIdentStateReq t ->
        EmuleSecIdentStateReq.print t
    | EmuleSignatureReq t ->
        EmuleSignatureReq.print t
    | EmulePublicKeyReq t ->
        EmulePublicKeyReq.print t
    | EmuleCompressedPart t ->
        EmuleCompressedPart.print t
    | EmulePortTestReq t ->
        EmulePortTestReq.print t
    | EmuleCaptchaReq t ->
        EmuleCaptchaReq.print t
    | EmuleCaptchaRes t ->
        EmuleCaptchaRes.print t
    | UnknownReq (opcode, s) ->
        let len = String.length s in
        lprintf_nl "UnknownReq: magic (%d), opcode (%d) len (%d)" opcode 
        (int_of_char s.[0])
        (String.length s);
        lprintf "ascii: [";
        for i = 0 to len - 1 do
          let c = s.[i] in
          let n = int_of_char c in
          if n > 31 && n < 127 then
            lprintf " %c" c
          else
            lprintf "(%d)" n
        done;
        lprintf "]\n";
        lprintf "dec: [";
        for i = 0 to len - 1 do
          let c = s.[i] in
          let n = int_of_char c in
          lprintf "(%d)" n
        done;
        lprintf "]\n"
  end

let rec parse_emule_packet emule opcode len s =
(*
  lprintf "Emule magic: %d opcode %d:" magic opcode; lprint_newline ();
          dump s; lprint_newline ();
  *)
  let t = match opcode with
    | 1 -> EmuleClientInfoReq (EmuleClientInfo.parse len s)
    | 2 -> EmuleClientInfoReplyReq (EmuleClientInfo.parse len s)

    | 0x60 (* 96 *) -> EmuleQueueRankingReq (EmuleQueueRanking.parse len s)

    | 0x61 (* 97 *) -> EmuleFileDescReq (EmuleFileDesc.parse len s)

    | 0x81 (* 129 *) -> EmuleRequestSourcesReq (EmuleRequestSources.parse len s)
    | 0x82 (* 130 *) ->
        EmuleRequestSourcesReplyReq (
          EmuleRequestSourcesReply.parse emule len s)

    | 0x40 (* 64 *) ->
(* OP_COMPRESSEDPART *)
        EmuleCompressedPart (EmuleCompressedPart.parse false len s)

    | 0x85 (* 133 *) ->
        EmulePublicKeyReq(EmulePublicKeyReq.parse len s)

    | 0x86 (* 134 *) ->
        EmuleSignatureReq(EmuleSignatureReq.parse len s)

    | 0x87 (* 135 *) ->
        EmuleSecIdentStateReq (EmuleSecIdentStateReq.parse len s)

(*     | 0x90 (* 144 *) -> RequestPreview *)
(*    | 0x91 (* 145 *) -> PreviewAnswer *)
    | 0x92 (* 146 *) ->
        let md4 = get_md4 s 1 in

(*        lprintf "MULTI EMULE VERSION %d"
          (extendedrequest emule); print_newline (); *)
(*        let pos = 17 in *)
        let rec iter pos =
          if pos < len then
            let opcode = get_uint8 s pos in
            match opcode with
              0x58 (* 88 *) ->
                let bitmap, pos = get_bitmap s (pos+1) in
                let ncompletesources, pos =
                  if extendedrequest emule > 1 then
                    get_int16 s pos, pos+2
                  else -1, pos
                in
                (QueryFileReq {
                    QueryFile.md4 = md4;
                    QueryFile.emule_extension = Some (bitmap, ncompletesources);
                  }) :: (iter pos)
            | 0x4F (* 79 *) ->
                (QueryChunksReq md4) :: iter (pos+1)
            | 0x81 (* 129 *) ->
                (EmuleRequestSourcesReq md4) :: iter (pos+1)
            | _ ->
                lprintf_nl "Unknown short emule packet %d" opcode;
                raise Not_found
          else
            []
        in
        EmuleMultiPacketReq (md4, iter 17)

    | 0x93 (* 147 *) ->
        if String.length s < 16 then begin
          if !verbose_unknown_messages then lprintf_nl "EmuleMultiPacketAnswer: incomplete request";
          raise Not_found
        end;
        let md4 = get_md4 s 1 in

(*        lprintf "MULTI EMULE VERSION %d"
          (extendedrequest emule); print_newline (); *)
        let rec iter s pos len =
          if pos < len then
            let opcode = get_uint8 s pos in
            match opcode with
            | 89 ->
                let module Q = QueryFileReply in
                let name, pos = get_string s (pos+1) in
                let q = {
                    Q.md4 = md4;
                    Q.name =  name;
                  } in
                (QueryFileReplyReq q) :: (iter s pos len)
            | 80 ->
                let module Q = QueryChunksReply in
                let chunks, pos = get_bitmap s (pos+1) in
                let q = {
                    Q.md4 = md4;
                    Q.chunks = chunks;
                  } in
                (QueryChunksReplyReq q) :: (iter s pos len)
            | _ ->
                lprintf_nl "Unknown packet in emule multipacket 0x93: %d" opcode;
                raise Not_found
          else
            []
        in
        EmuleMultiPacketAnswerReq (md4, iter s 17 len)

    | 0xa1 (* 161 *) -> (* OP_COMPRESSEDPART_I64 *)
        EmuleCompressedPart (EmuleCompressedPart.parse true len s)
    | 0xa2 -> BlocReq (Bloc.parse true len s) (* OP_SENDINGPART_I64 *)
    | 0xa3 -> QueryBlocReq (QueryBloc.parse true len s) (*OP_REQUESTPARTS_I64 *)
    | 0xa5 (* 165 *) -> EmuleCaptchaReq (EmuleCaptchaReq.parse len s) (* OP_CHATCAPTCHAREQ *)
    | 0xa6 (* 166 *) -> EmuleCaptchaRes (EmuleCaptchaRes.parse s) (* OP_CHATCAPTCHARES *)
    | 0xfe (* 254 *) -> EmulePortTestReq s

    | code ->
        if !CommonOptions.verbose_unknown_messages then
          lprintf_nl "EDK: unknown eMule message %d" code;
        raise Not_found
  in
(*
          lprintf "EMULE MESSAGE: "; lprint_newline ();
          print t;
          lprint_newline (); *)
  t

and parse emule_version magic s =
  try
    let len = String.length s in
    if len = 0 then raise Not_found;
    let opcode = int_of_char (s.[0]) in
(*lprintf "opcode: %d" opcode; lprint_newline (); *)
    match magic with
      227 ->
        begin
          match opcode with
          | 1 -> ConnectReq (Connect.parse false len s)
          | 70 -> BlocReq (Bloc.parse false len s)
          | 71 -> QueryBlocReq (QueryBloc.parse false len s)
          | 72 -> NoSuchFileReq (NoSuchFile.parse len s)
          | 73 -> EndOfDownloadReq (EndOfDownload.parse len s)
          | 74 -> ViewFilesReq (ViewFiles.parse len s)
          | 75 -> ViewFilesReplyReq (ViewFilesReply.parse len s)
          | 76 -> ConnectReplyReq (Connect.parse true len s)
          | 77 -> NewUserIDReq (NewUserID.parse len s)
          | 78 -> SayReq (Say.parse len s)
          | 79 -> QueryChunksReq (QueryChunks.parse len s)
          | 80 -> QueryChunksReplyReq (QueryChunksReply.parse len s)
          | 81 -> QueryChunkMd4Req (QueryChunkMd4.parse len s)
          | 82 -> QueryChunkMd4ReplyReq (QueryChunkMd4Reply.parse len s)
(* JoinQueue: the sender wants to join the upload queue *)
          | 84 -> JoinQueueReq (JoinQueue.parse len s)
(* AvailableSlot: there is an available slot in upload queue *)
          | 85 -> AvailableSlotReq (AvailableSlot.parse len s)
(* ReleaseSlot: the upload is finished *)
          | 86 -> ReleaseSlotReq (ReleaseSlot.parse len s)
(* OutOfParts: the upload slot is not available *)
          | 87 -> OutOfPartsReq (OutOfParts.parse len s)
          | 88 -> QueryFileReq (QueryFile.parse emule_version len s)
          | 89 -> QueryFileReplyReq (QueryFileReply.parse len s)
          | 92 -> QueueRankReq (QueueRank.parse len s)
          | 93 -> ViewDirsReq (ViewDirs.parse len s)
          | 94 -> ViewFilesDirReq (ViewFilesDir.parse len s)
          (*
          | 95 -> ViewDirsReplyReq (ViewDirsReply.parse len s)
          | 96 -> ViewFilesDirReplyReq (ViewFilesDirReply.parse len s)
          *)
          | 250 -> SourcesReq (Sources.parse len s)

          | _ -> raise Not_found
        end

    | 0xc5  -> (* 197: emule extended protocol *)
        parse_emule_packet emule_version opcode len s

(* Compressed packet, probably sent by cDonkey ? *)

    | 0xD4 -> (* 212 *)

          let s = Zlib2.uncompress_string2 (Bytes.of_string (String.sub s 1 (len-1))) in
          let s = Printf.sprintf "%c%s" (char_of_int opcode) (Bytes.unsafe_to_string s) in
    begin try
            parse_emule_packet emule_version opcode (String.length s) s
          with
          | e ->
              if !CommonOptions.verbose_unknown_messages then begin
                  lprintf_nl "Unknown message From client: %s (magic %d)"
                    (Printexc2.to_string e) magic;
                  let tmp_file = Filename2.temp_file "comp" "pak" in
                  File.from_string tmp_file s;
                  lprintf_nl "Saved unknown packet %s" tmp_file;
                  dump s;
                  lprint_newline ();
                end;
              UnknownReq (magic,s)
    end

    | _ ->
        if !CommonOptions.verbose_unknown_messages then
            lprintf_nl "Strange magic: %d" magic;
        raise Not_found
  with
  | e ->
      if !CommonOptions.verbose_unknown_messages then begin
          lprintf_nl "Unknown message From client: %s (magic %d)"
              (Printexc2.to_string e) magic;
               let tmp_file = Filename2.temp_file "comp" "pak" in
       File.from_string tmp_file s;
       lprintf_nl "Saved unknown packet %s" tmp_file;

          dump s;
          lprint_newline ();
        end;
      UnknownReq (magic,s)

let write emule buf t =
  let magic = match t with
      EmuleMultiPacketAnswerReq _
    | EmuleMultiPacketReq _
    | EmuleSecIdentStateReq _
    | EmuleSignatureReq _
    | EmulePublicKeyReq _
    | EmuleRequestSourcesReplyReq _
    | EmuleRequestSourcesReq _
    | EmuleClientInfoReplyReq _
    | EmuleClientInfoReq _
    | EmuleFileDescReq _
    | EmuleQueueRankingReq _
    | EmuleCaptchaReq _
    | EmuleCaptchaRes _
    | EmuleCompressedPart _
      -> 0xC5
    | QueryBlocReq t when t.QueryBloc.usesixtyfour -> 0xC5
    | BlocReq t when t.Bloc.usesixtyfour -> 0xC5
    | _
      ->  227
  in
  begin
    match t with
    | ConnectReq t ->
        buf_int8 buf 1;
        Connect.write false buf t
    | ConnectReplyReq t ->
        buf_int8 buf 76;
        Connect.write true buf t
    | QueryFileReq t ->
        buf_int8 buf 88;
        QueryFile.write emule buf t
    | QueryFileReplyReq t ->
        buf_int8 buf 89;
        QueryFileReply.write buf t
    | QueueReq t ->
        buf_int8 buf 77;
        OtherLocations.write buf t
    | QueryBlocReq t ->
        buf_int8 buf (if t.QueryBloc.usesixtyfour then 0xa3 else 71);
        QueryBloc.write buf t
    | BlocReq t ->
        buf_int8 buf (if t.Bloc.usesixtyfour then 0xa2 else 70);
        Bloc.write buf t
    | JoinQueueReq t ->
        buf_int8 buf 84;
        JoinQueue.write emule buf t
    | QueryChunksReq t ->
        buf_int8 buf 79;
        QueryChunks.write buf t
    | QueryChunksReplyReq t ->
        buf_int8 buf 80;
        QueryChunksReply.write buf t
    | QueryChunkMd4Req t ->
        buf_int8 buf 81;
        QueryChunkMd4.write buf t
    | QueryChunkMd4ReplyReq t ->
        buf_int8 buf 82;
        QueryChunkMd4Reply.write buf t
    | AvailableSlotReq t ->
        buf_int8 buf 85;
        AvailableSlot.write buf t
    | ReleaseSlotReq t ->
        buf_int8 buf 86;
        ReleaseSlot.write buf t
    | OutOfPartsReq t ->
        buf_int8 buf 87;
        OutOfParts.write buf t
    | ViewFilesReq t ->
        buf_int8 buf 74;
        ViewFiles.write buf t
    | ViewFilesReplyReq t ->
        buf_int8 buf 75;
        ViewFilesReply.write buf t
    | ViewDirsReq t ->
        buf_int8 buf 93;
        ViewDirs.write buf t
    | ViewDirsReplyReq t ->
        buf_int8 buf 95;
        ViewDirsReply.write buf t
    | ViewFilesDirReq t ->
        buf_int8 buf 94;
        ViewFilesDir.write buf t
    | ViewFilesDirReplyReq t ->
        buf_int8 buf 96;
        ViewFilesDirReply.write buf t
    | OtherLocationsReq t ->
        buf_int8 buf 72;
        OtherLocations.write buf t
    | SayReq t ->
        buf_int8 buf 78;
        Say.write buf t
    | SourcesReq t ->
        buf_int8 buf 250;
        Sources.write buf t
    | NewUserIDReq t ->
        buf_int8 buf 77;
        NewUserID.write buf t
    | EndOfDownloadReq t ->
        buf_int8 buf 73;
        EndOfDownload.write buf t
    | NoSuchFileReq t ->
        buf_int8 buf 72;
        NoSuchFile.write buf t
    | QueueRankReq t ->
        buf_int8 buf 92;
        QueueRank.write buf t

    | EmuleClientInfoReq t ->
        buf_int8 buf 1;
        EmuleClientInfo.write buf t
    | EmuleClientInfoReplyReq t ->
        buf_int8 buf 2;
        EmuleClientInfo.write buf t
    | EmuleQueueRankingReq t ->
        buf_int8 buf 0x60;
        EmuleQueueRanking.write buf t
    | EmuleRequestSourcesReq t ->
        buf_int8 buf 0x81;
        EmuleRequestSources.write buf t
    | EmuleRequestSourcesReplyReq t ->
        buf_int8 buf 0x82;
        EmuleRequestSourcesReply.write emule buf t
    | EmuleFileDescReq t ->
        buf_int8 buf 0x61;
        EmuleFileDesc.write buf t
    | EmuleCompressedPart t ->
        buf_int8 buf (if t.EmuleCompressedPart.usesixtyfour then 0xa1 else 0x40);
        EmuleCompressedPart.write buf t
    | EmuleMultiPacketReq (md4, list) ->
        buf_int8 buf 0x92;
        buf_md4 buf md4;
        List.iter (fun t ->
            match t with
              QueryFileReq t ->
                buf_int8 buf 0x58;
                (match t.QueryFile.emule_extension with
                    None -> ()
                  | Some (bitmap, ncompletesources) ->
                      write_bitmap buf bitmap;
                      if ncompletesources >= 0 && extendedrequest emule > 1 then
                        buf_int16 buf ncompletesources)
            | QueryChunksReq _ ->
                buf_int8 buf 0x4F
            | EmuleRequestSourcesReq _ ->
                buf_int8 buf 0x81
            | _ ->
                lprintf_nl "WARNING: Don't know how to write short packet:";
                print t;
                print_newline ();
        ) list

    | EmuleMultiPacketAnswerReq (md4, list) ->
        buf_int8 buf 0x93;
        buf_md4 buf md4;
        List.iter (fun t ->
            match t with
              QueryFileReplyReq t ->
                buf_int8 buf 89;
                buf_string buf t.QueryFileReply.name
            | QueryChunksReplyReq t ->
                buf_int8 buf 80;
                write_bitmap buf t.QueryChunksReply.chunks
            | _ ->
                lprintf_nl "WARNING: Don't know how to write short packet:";
                print t;
                print_newline ();
        ) list

    | EmuleSecIdentStateReq t ->
        buf_int8 buf 0x87;
        EmuleSecIdentStateReq.write buf t

    | EmuleSignatureReq t ->
        buf_int8 buf 0x86;
        EmuleSignatureReq.write buf t

    | EmulePublicKeyReq t ->
        buf_int8 buf 0x85;
        EmulePublicKeyReq.write buf t

    | EmulePortTestReq t ->
        buf_int8 buf 0xfe;
        EmulePortTestReq.write buf

    | EmuleCaptchaReq t ->
        buf_int8 buf 0xa5;
        EmuleCaptchaReq.write buf t

    | EmuleCaptchaRes t ->
        buf_int8 buf 0xa6;
        EmuleCaptchaRes.write buf t

    | UnknownReq (opcode, s) ->
        Buffer.add_string buf s

  end;
  magic

(*
------------------------------------------------------
1044008574.297 192.168.0.3:37522 -> 80.26.114.12:13842 of len 6
? Become Friend ? ping ?

(227)(1)(0)(0)(0)
(98)

------------------------------------------------------
1044008576.274 80.26.114.12:13842 -> 192.168.0.3:37522 of len 6
? OK ? pong ?

(227)(1)(0)(0)(0)(99)]

------------------------------------------------------
1044008687.977 192.168.0.3:37522 -> 80.26.114.12:13842 of len 6
Browse Main Dir

(227)(1)(0)(0)(0)
(93)

------------------------------------------------------
1044008690.832 80.26.114.12:13842 -> 192.168.0.3:37522 of len 43
Browse Main Dir Reply
(227)(38)(0)(0)(0)
(95)
(2)(0)(0)(0) --------> 2 directories:
(12)(0) C : \ D o w n l o a d s
(17)(0) ! I n c o m p l e t e   F i l e s


------------------------------------------------------
1044008766.137 192.168.0.3:37522 -> 80.26.114.12:13842 of len 20
Browse directory

(227)(15)(0)(0)(0)
(94)
(12)(0) C : \ D o w n l o a d s

------------------------------------------------------
1044008769.045 80.26.114.12:13842 -> 192.168.0.3:37522 of len 300
(227) p(8)(0)(0) `(12)(0) C : \ D o w n l o a d s(21)(0)(0)(0)(152) 2(229)(158)(218)(141)(217)(138) n(181) 6 ( ) h V(179)(0)(0)(0)(0)(0)(0)(3)(0)(0)(0)(2)(1)(0)(1)(11)(0) d e s k t o p . i n i(3)(1)(0)(2)(180)(0)(0)(0)(3)(1)(0)(19)(0)(0)(0)(0) y(16)(15) 9 O Z(219) i e(200)(10) |(29)(27) F(128)(0)(0)(0)(0)(0)(0)(5)(0)(0)(0)(2)(1)(0)(1)(15)(0) u t b o n u s p a c k . z i p(3)(1)(0)(2) J(16)(221)(0)(2)(1)(0)(3)(3)(0) P r o(2)(1)(0)(4)(3)(0) z i p(3)(1)(0)(19)(0)(0)(0)(0)(178)(145)(161)(146) P(199)(228)(249) K a :(9)(237)(246)(233) v(0)(0)(0)(0)(0)(0)(5)(0)(0)(0)(2)(1)(0)(1)(11)(0) c t f m a p s . z i p(3)(1)(0)(2)(236)(239)(23)(0)(2)(1)(0)(3)(3)(0) P r o(2)(1)(0)(4)(3)(0) z i p(3)(1)(0)(19)(0)(0)(0)(0) a n(251)(225) ^ g(205)(133)(25)(12) # ' J A(221) `(0)(0)(0)(0)(0)(0)(5)(0)(0)(0)(2)(1)(0)(1)(23)(0) u t i n o x x p a c k - n o - u m o d . z i p(3)(1)(0)(2)]
(227)(112)(8)(0)(0)

(96)
(12)(0) C : \ D o w n l o a d s
(21)(0)(0)(0) 21 files

(152)(50)(229)(158)(218)(141)(217)(138)(110)(181)(54)(40)(41)(104)(86)(179)
(0)(0)(0)(0)
(0)(0)
(3)(0)(0)(0)
(2)
(1)(0)(1)
(11)(0)  d e s k t o p . i n i
(3)
(1)(0)(2)
(180)(0)(0)(0)
(3)
(1)(0)(19)
(0)(0)(0)(0)

(121)(16)(15)(57)(79)(90)(219)(105)(101)(200)(10)(124)(29)(27)(70)(128)
(0)(0)(0)(0)
(0)(0)
(5)(0)(0)(0)
(2)
(1)(0)(1)
(15)(0) u t b o n u s p a c k . z i p
(3)
(1)(0)(2)
(74)(16)(221)(0)
(2)
(1)(0)(3)
(3)(0) Pro
(2)
(1)(0)(4)
(3)(0) zip
(3)
(1)(0)(19)
(0)(0)(0)(0)
....

*)

(* 92: Queue Rank *)
