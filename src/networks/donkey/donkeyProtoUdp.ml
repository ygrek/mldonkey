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
open Md4
open LittleEndian
open AnyEndian

open CommonTypes
open CommonGlobals
open CommonOptions

open DonkeyTypes
open DonkeyMftp
open Int64ops

module QueryReplyUdp  = struct

    type t = tagged_file list

    let names_of_tag = file_common_tags

    let get_file  s pos =
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
      file, pos

    let parse len s =
      let rec iter pos list =
  if pos < len then
          let file, pos = get_file s pos in
          let pos = pos + 2 in
    iter pos (file :: list)
  else List.rev list
      in
      iter 1 []

    let bprint oc t =
      Printf.bprintf oc "FOUND:\n";
      List.iter (fun t ->
  Printf.bprintf oc "%s\n" (Md4.to_string t.f_md4);
  Printf.bprintf oc "%s\n" (Ip.to_string t.f_ip);
  Printf.bprintf oc "%d\n" t.f_port;
  Printf.bprintf oc "TAGS:\n";
  bprint_tags oc t.f_tags;
         Printf.bprintf oc "\n"
      ) t

    let write buf t =
      List.iter (fun file ->
  buf_md4 buf file.f_md4;
  buf_ip buf file.f_ip;
  buf_port buf file.f_port;
  buf_tags buf file.f_tags names_of_tag
      ) t

  end

module QueryCallUdp  = struct
    type t = {
        ip : Ip.t;
        port : int;
        id : int64;
      }

    let parse len s =
      let ip = get_ip s 1 in
      let port = get_port s 5 in
      let id = id_of_ip (get_ip s 7) in
      { ip = ip; port = port; id = id; }

    let bprint oc t =
      Printf.bprintf oc "QueryCall %s : %d --> %Ld\n" (Ip.to_string t.ip)
      t.port  t.id

    let write buf t =
      buf_ip buf t.ip;
      buf_port buf t.port;
      buf_ip buf (ip_of_id t.id)

  end

module PingServerUdp = struct (* client -> serveur pour identification ? *)
    type t = int64

    let parse len s =
      try
        get_uint64_32 s 1 (*, get_int8 s 2, get_int8 s 3*)
      with _ -> 0L

    let bprint oc t =
      Printf.bprintf oc "PING %s\n" (Int64.to_string t)

    let write buf t =
      buf_int64_32 buf t

    (* let bprint oc (t1,t2,t3) =
      Printf.bprintf oc "MESSAGE 150 UDP %d %d %d\n" t1 t2 t3*)

    (*let write buf (t1,t2,t3) =
      buf_int8 buf t1;
      buf_int8 buf t2;
      buf_int8 buf t3;*)

  end

module PingServerReplyUdp = struct (* reponse du serveur a 150 *)

    type t = {
        challenge  : int64;
        users      : int64;
        files      : int64;
        soft_limit : int64 option;
        hard_limit : int64 option;
        max_users  : int64 option;
        lowid_users : int64 option;
        get_sources : bool;
        get_files : bool;
        newtags : bool;
        unicode : bool;
        get_sources2 : bool;
        largefiles : bool;
        udp_obfuscation : bool;
        tcp_obfuscation : bool;
      }

    let parse len s =
      let challenge = get_uint64_32 s 1 in
      let users = get_uint64_32 s 5 in
      let files = get_uint64_32 s 9 in
      let max_users  = if len >= 17 then Some (get_uint64_32 s 13) else None in
      let soft_limit = if len >= 21 then Some (get_uint64_32 s 17) else None in
      let hard_limit = if len >= 25 then Some (get_uint64_32 s 21) else None in
      let flags      = if len >= 29 then get_int s 25 else 0 in
      let lowid_users = if len >= 33 then Some (get_uint64_32 s 29) else None in

      {
        challenge = challenge;
        users = users;
        files = files;
        soft_limit = soft_limit;
        hard_limit = hard_limit;
        max_users = max_users;
        lowid_users = lowid_users;
        get_sources = 0x01 land flags = 0x01;
        get_files = 0x02 land flags = 0x02;
        newtags = 0x08 land flags = 0x08;
        unicode = 0x10 land flags = 0x10;
        get_sources2 = 0x20 land flags = 0x20;
        largefiles = 0x100 land flags = 0x100;
        udp_obfuscation = 0x200 land flags = 0x200;
        tcp_obfuscation = 0x200 land flags = 0x200;
      }

    let bprint oc t =
      Printf.bprintf oc "PING REPLY\n";
      Printf.bprintf oc "   %Ld users %Ld files\n" t.users t.files;
      (match t.soft_limit with Some x -> Printf.bprintf oc "   Soft limit: %Ld\n" x | None -> ());
      (match t.hard_limit with Some x -> Printf.bprintf oc "   Hard limit: %Ld\n" x | None -> ());
      (match t.max_users with Some x -> Printf.bprintf oc "   Max nusers: %Ld\n" x | None -> ());
      (match t.lowid_users with Some x -> Printf.bprintf oc "   LowId nusers: %Ld\n" x | None -> ());
      Printf.bprintf oc "   get_sources %b, get_files %b, newtags %b, unicode %b, get_sources2 %b, largefiles %b, udp_obfuscation %b, tcp_obfuscation %b"
        t.get_sources t.get_files t.newtags t.unicode t.get_sources2 t.largefiles t.udp_obfuscation t.tcp_obfuscation

    let write buf t =
      buf_int64_32 buf t.challenge;
      buf_int64_32 buf t.users;
      buf_int64_32 buf t.files;
      (match t.soft_limit, t.hard_limit, t.max_users with
          None, None, None -> ()
        | _ ->
            buf_int64_32 buf (
              match t.soft_limit with Some x -> x | None -> 0L);
            buf_int64_32 buf (
              match t.hard_limit with Some x -> x | None -> 0L);
            buf_int64_32 buf (
              match t.max_users with Some x -> x | None -> 0L)
      )
  end

module ServerDescUdp = struct
  type t = int64

  let invalid_len = Int64.of_int 0xF0FF

  let parse len s = Int64.of_string s

  let bprint b t =
    Printf.bprintf b "ServerDescUdpReq\n"

(*
// eserver 16.45+ supports a new OP_SERVER_DESC_RES answer, if the OP_SERVER_DESC_REQ contains a uint32
// challenge, the server returns additional info with OP_SERVER_DESC_RES. To properly distinguish the
// old and new OP_SERVER_DESC_RES answer, the challenge has to be selected carefully. The first 2 bytes
// of the challenge (in network byte order) MUST NOT be a valid string-len-int16!
*)

  let write buf t =
    buf_int64_32 buf t

end

module ServerDescReplyUdp = struct
  type t = {
    name : string;
    desc : string;
    tags : tag list;
    challenge : int64;
  }

  let names_of_tag = [
    "\001", Field_KNOWN "servername";
    "\011", Field_KNOWN "description";
    "\012", Field_KNOWN "ping";
    "\013", Field_KNOWN "fail";
    "\014", Field_KNOWN "preference";
    "\015", Field_KNOWN "port";
    "\016", Field_KNOWN "ip";
    "\133", Field_KNOWN "dynip";
    "\135", Field_KNOWN "maxusers";
    "\136", Field_KNOWN "softfiles";
    "\137", Field_KNOWN "hardfiles";
    "\144", Field_KNOWN "lastping";
    "\145", Field_KNOWN "version";
    "\146", Field_KNOWN "udpflags";
    "\147", Field_KNOWN "auxportslist";
    "\148", Field_KNOWN "lowidusers";
  ]

  let parse1 len s challenge =
    let name, pos = get_string s 1 in
    let desc, pos = get_string s pos in
     {
       tags = [];
       name = name;
       desc = desc;
       challenge = challenge;
     }

  let parse2 len s challenge =
    let stags,pos = get_tags s 5 names_of_tag in
    let name = ref "" in
    let desc = ref "" in
    List.iter (fun tag ->
      match tag with
      | { tag_name = Field_KNOWN "servername"; tag_value = String v } ->
            name := v
      | { tag_name = Field_KNOWN "description"; tag_value = String v } ->
            desc := v
      | _ -> ()
    ) stags;
    { 
      tags = stags;
      name = !name;
      desc = !desc;
      challenge = challenge;
    }

  let parse len s =
    let challenge = get_uint64_32 s 1 in
    let test = right64 (left64 challenge 48) 48 in
    let f = if test = ServerDescUdp.invalid_len then parse2 else parse1 in
    f len s challenge

  let bprint b t =
    Printf.bprintf b  "ServerDescReplyUdpReq\n";
    Printf.bprintf b "name : %s\n" t.name;
    Printf.bprintf b "desc : %s\n" t.desc

  let write buf t =
    buf_string buf t.name;
    buf_string buf t.desc

end

module ServerListUdp = struct
  type t = {
    ip : Ip.t;
  }

  let parse len s =
    try
      let ip = get_ip s 1 in
  {
    ip = ip;
  }
    with _ ->
      {
  ip = Ip.null
      }
  
  let bprint b t =
    Printf.bprintf b  "ServerListUdp %s\n" (Ip.to_string t.ip)

  let write buf t =
    buf_ip buf t.ip

end

module QueryServersUdp = DonkeyProtoServer.QueryServers
module QueryServersReplyUdp = DonkeyProtoServer.QueryServersReply
module QueryLocationUdp = struct

    type t = Md4.t list

  let parse len s =
    let rec iter pos list =
      if pos < len then
  iter (pos+16) (get_md4 s pos :: list)
      else
  List.rev list
    in
    iter 1 []

  let bprint b t =
    Printf.bprintf b "UDP QUERY LOCATIONS: ";
    List.iter (fun md4 -> Printf.bprintf b "%s " (Md4.to_string md4)) t

  let write buf t =
    List.iter (fun md4 -> buf_md4 buf md4) t
end

module QueryLocationUdpReq2 = struct

  type t = (Md4.t * Int64.t) list

  (* We never parse this anyway, it is outgoing only *)
  let parse len s =
    let rec iter pos list =
      if pos < len then
  iter (pos+20) ( (get_md4 s pos, get_uint64_32 s (pos+16)) :: list)
      else
  List.rev list
    in
    iter 1 []

  let bprint b t =
    Printf.bprintf b "UDP QUERY LOCATIONS2: ";
    List.iter (fun (md4,size) -> Printf.bprintf b "%s|%Ld " (Md4.to_string md4) size) t;
    Printf.bprintf b "\n"

  let write buf t =
    List.iter (fun (md4,size) -> buf_md4 buf md4; buf_int64_32 buf size) t

end

module QueryLocationReplyUdp = struct
  open DonkeyProtoServer.QueryLocationReply

    type t = DonkeyProtoServer.QueryLocationReply.t list

    let parse len s =
      let rec iter_len pos list =
        if pos < len then
          let md4 = get_md4 s pos in
          let n = get_uint8 s (pos+16) in
          let rec iter i  =
            if i = n then [] else
            let ip = get_ip s (pos+17 + i * 6) in
            let port = get_port s (pos+21+ i * 6) in
            { ip = ip; port = port; } :: (iter (i+1))
          in
          let locs = iter 0 in
          let pos = pos+17+6*n + 2 in
          iter_len pos ({ locs =locs; md4 = md4 } :: list)
        else
          List.rev list
      in
      iter_len 1 []

  let bprint b t =
    Printf.bprintf b "UDP LOCATION: %d\n" (List.length t);
    List.iter (fun t ->
      Printf.bprintf b "    of %s:\n" (Md4.to_string t.md4);
      List.iter (fun l ->
          Printf.bprintf b "%s:%d " (Ip.to_string l.ip) l.port;
      ) t.locs;
      Printf.bprintf b "\n") t

  let write buf t =
    List.iter (fun t ->
      buf_md4 buf t.md4;
      buf_int8 buf (List.length t.locs);
      List.iter (fun l ->
        buf_ip buf l.ip;
        buf_port buf l.port;
      ) t.locs
    ) t

end


module QueryUdp = DonkeyProtoServer.Query

(*
  let parse len s =
    let rec iter list pos =
      if len > pos then
  let t, pos = parse_query s pos in
  iter (t :: list) pos
      else List.rev list
    in
    iter [] 1

  let bprint b t =
    Printf.bprintf b "UDP QUERY: %d\n" (List.length t);
    List.iter (bprint_query b) t

  let write buf t =
    List.iter write t

  end
*)

module QueryIDReplyUdp = DonkeyProtoServer.QueryIDReply

type t =
| QueryServersUdpReq of QueryServersUdp.t
| QueryServersReplyUdpReq of QueryServersReplyUdp.t

| PingServerUdpReq of PingServerUdp.t
| PingServerReplyUdpReq of PingServerReplyUdp.t

| QueryLocationUdpReq2 of QueryLocationUdpReq2.t
| QueryLocationUdpReq of QueryLocationUdp.t
| QueryLocationReplyUdpReq of QueryLocationReplyUdp.t

| QueryReplyUdpReq of QueryReplyUdp.t
| QueryUdpReq of CommonTypes.query
| QueryMultipleUdpReq of CommonTypes.query
| QueryCallUdpReq of QueryCallUdp.t
| QueryIDReplyUdpReq of QueryIDReplyUdp.t
| FileGroupInfoUdpReq of QueryLocationReplyUdp.t
| ServerDescUdpReq of ServerDescUdp.t
| ServerDescReplyUdpReq of ServerDescReplyUdp.t
| ServerListUdpReq of ServerListUdp.t

| EmuleReaskFilePingUdpReq of Md4.t
| EmuleReaskAckUdpReq of Md4.t
| EmuleFileNotFoundUdpReq
| EmuleQueueFullUdpReq
| EmulePortTestReq

| UnknownUdpReq of int * string

let parse magic s =
  try
    let len = String.length s in
    if len = 0 then raise Not_found;
    let opcode = int_of_char (s.[0]) in
(*    lprintf "opcode: %d" opcode; lprint_newline (); *)
    match opcode with
    | 150 -> PingServerUdpReq (PingServerUdp.parse len s)
    | 151 -> PingServerReplyUdpReq (PingServerReplyUdp.parse len s)

    | 146 -> QueryMultipleUdpReq (QueryUdp.parse len s)
    | 152 -> QueryUdpReq (QueryUdp.parse len s)
    | 153 -> QueryReplyUdpReq (QueryReplyUdp.parse len s)
    | 154 -> QueryLocationUdpReq (QueryLocationUdp.parse len s)
    | 155 -> QueryLocationReplyUdpReq (QueryLocationReplyUdp.parse len s)
    | 156 -> QueryCallUdpReq (QueryCallUdp.parse len s)
    | 160 -> QueryServersUdpReq (QueryServersUdp.parse len s)
    | 161 -> QueryServersReplyUdpReq (QueryServersReplyUdp.parse len s)
    | 162 -> ServerDescUdpReq (ServerDescUdp.parse len s)
    | 163 -> ServerDescReplyUdpReq (ServerDescReplyUdp.parse len s)
    | 164 -> ServerListUdpReq (ServerListUdp.parse len s)

    | 144 -> EmuleReaskFilePingUdpReq (get_md4 s 1)
    | 145 -> EmuleReaskAckUdpReq (get_md4 s 1)
(*    | 146 -> EmuleFileNotFoundUdpReq *)
    | 147 -> EmuleQueueFullUdpReq
    | 254 -> EmulePortTestReq

    | _ -> raise Exit
  with
    e ->
      if !verbose_unknown_messages then begin lprintf "Unknown UDP request:\n"; dump s end;
      UnknownUdpReq (magic, s)

let print t =
  let b = Buffer.create 100 in
  begin
    match t with

    | QueryUdpReq t -> QueryUdp.bprint b t
    | QueryMultipleUdpReq t -> QueryUdp.bprint b t
    | QueryReplyUdpReq t -> QueryReplyUdp.bprint b t
    | QueryLocationUdpReq2 t -> QueryLocationUdpReq2.bprint b t
    | QueryLocationUdpReq t -> QueryLocationUdp.bprint b t
    | QueryLocationReplyUdpReq t
    | FileGroupInfoUdpReq t -> QueryLocationReplyUdp.bprint b t
    | QueryCallUdpReq t -> QueryCallUdp.bprint b t

    | QueryServersUdpReq t -> QueryServersUdp.bprint b t
    | QueryServersReplyUdpReq t -> QueryServersReplyUdp.bprint b t
    | QueryIDReplyUdpReq t -> QueryIDReplyUdp.bprint b t

    | PingServerUdpReq t -> PingServerUdp.bprint b t
    | PingServerReplyUdpReq t -> PingServerReplyUdp.bprint b t
    | ServerDescUdpReq t -> ServerDescUdp.bprint b t
    | ServerDescReplyUdpReq t -> ServerDescReplyUdp.bprint b t
    | ServerListUdpReq t -> ServerListUdp.bprint b t

    | EmuleReaskFilePingUdpReq md4 ->
        Printf.bprintf b  "EmuleReaskFilePingUdpReq %s" (Md4.to_string md4)
    | EmuleReaskAckUdpReq md4 ->
        Printf.bprintf b "EmuleReaskAckUdpReq %s" (Md4.to_string md4)
    | EmuleFileNotFoundUdpReq ->
        Printf.bprintf b "EmuleFileNotFoundUdpReq"
    | EmuleQueueFullUdpReq ->
        Printf.bprintf b "EmuleQueueFullUdpReq"
    | EmulePortTestReq ->
        Printf.bprintf b "EmulePortTestReq"

    | UnknownUdpReq (magic, s) ->
        Printf.bprintf b "UnknownReq magic %d\n" magic;
        bdump b s;
  end;
  Printf.bprintf b "\n";
  Buffer.contents b

let write buf t =
  match t with

  | UnknownUdpReq (magic, s) ->
      buf_int8 buf magic;
      Buffer.add_string buf s

  | EmuleReaskFilePingUdpReq md4 ->
      buf_int8 buf 197;
      buf_int8 buf 145;
      buf_md4 buf md4

  | EmuleReaskAckUdpReq md4 ->
      buf_int8 buf 197;
      buf_int8 buf 145;
      buf_md4 buf md4

  | EmuleFileNotFoundUdpReq ->
      buf_int8 buf 197;
      buf_int8 buf 146

  | EmuleQueueFullUdpReq ->
      buf_int8 buf 197;
      buf_int8 buf 147

  | _ ->
      buf_int8 buf 227;
      match t with
      | QueryServersUdpReq t ->
          buf_int8 buf 160;
          QueryServersUdp.write buf t
      | QueryServersReplyUdpReq t ->
          buf_int8 buf 161;
          QueryServersReplyUdp.write buf t

      | ServerDescUdpReq t ->
          buf_int8 buf 162;
          ServerDescUdp.write buf t
      | ServerDescReplyUdpReq t ->
          buf_int8 buf 163;
          ServerDescReplyUdp.write buf t
      | ServerListUdpReq t ->
          buf_int8 buf 164;
          ServerListUdp.write buf t

      | PingServerUdpReq t ->
          buf_int8 buf 150;
          PingServerUdp.write buf t
      | PingServerReplyUdpReq t ->
          buf_int8 buf 151;
          PingServerReplyUdp.write buf t

      | QueryLocationUdpReq t ->
          buf_int8 buf 154;
          QueryLocationUdp.write buf t
      | QueryLocationUdpReq2 t ->
          buf_int8 buf 148;
          QueryLocationUdpReq2.write buf t
      | QueryLocationReplyUdpReq t ->
          buf_int8 buf 155;
          QueryLocationReplyUdp.write buf t
      | QueryUdpReq t ->
          buf_int8 buf 152;
          QueryUdp.write buf t
      | QueryMultipleUdpReq t ->
          buf_int8 buf 146;
          QueryUdp.write buf t
      | QueryReplyUdpReq t ->
          buf_int8 buf 153;
          QueryReplyUdp.write buf t
      | QueryCallUdpReq t ->
          buf_int8 buf 156;
          QueryCallUdp.write buf t
      | FileGroupInfoUdpReq t ->
          buf_int8 buf 251;
          QueryLocationReplyUdp.write buf t

      | QueryIDReplyUdpReq t ->
          buf_int8 buf 53;
          QueryIDReplyUdp.write buf t

      | EmulePortTestReq ->
          buf_int8 buf 2;
          buf_int8 buf 0;
          buf_int8 buf 0;
          buf_int8 buf 0;
          buf_int8 buf 0xfe;
          buf_int8 buf 0x31

      | EmuleQueueFullUdpReq
      | EmuleFileNotFoundUdpReq
      | EmuleReaskAckUdpReq _
      | EmuleReaskFilePingUdpReq _
      | UnknownUdpReq _ -> assert false
