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
open CommonTypes
open LittleEndian
open CommonGlobals
open DonkeyMftp

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
  
module Connect  = struct
    type t = {
        md4 : Md4.t;
        version : int;
        ip: Ip.t;
        port: int;
        tags : tag list;
        server_info : (Ip.t * int) option;
        left_bytes: string;
      }
    
    let names_of_tag =
      [
        1, "name";
        17, "version";
        15, "port";
        31, "server_udp";
      ]
    
    let parse len s =
      let version = get_int8 s 1 in
      let md4 = get_md4 s 2 in
      let ip = get_ip s 18 in
      let port = get_port s 22 in
(*      Printf.printf "port: %d" port; print_newline (); *)
      let tags, pos = get_tags s 24 names_of_tag in
      let len = String.length s in
      let server_info = 
        Some (get_ip s pos, get_port s (pos+4)) 
      in
      let left_bytes = String.sub s (pos+6) (String.length s - pos - 6) in
      {
        md4 = md4;
        version = version;
        ip = ip;
        port = port;
        tags = tags;
        server_info = server_info;
        left_bytes = left_bytes;
      }
    
    let print t = 
      Printf.printf "CONNECT:\n";
      Printf.printf "version: %d" t.version; print_newline ();
      Printf.printf "MD4: %s\n" (Md4.to_string t.md4); 
      Printf.printf "ip: %s\n" (Ip.to_string t.ip);
      Printf.printf "port: %d\n" t.port;
      Printf.printf "tags: ";
      print_tags t.tags;
      print_newline ();
      (match t.server_info with
          None -> ()
        | Some (ip, port) ->
            Printf.printf "ip_server: %s\n" (Ip.to_string ip);
            Printf.printf "port_server: %d\n" port);
      String.iter (fun c -> Printf.printf "(%d)" (int_of_char c)) 
      t.left_bytes;
      Printf.printf "\n"
    
    let write buf t =
      buf_int8 buf t.version;
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
      end;
      Buffer.add_string buf t.left_bytes

  end

module ConnectReply  = struct
    type t = {
        md4 : Md4.t;
        ip: Ip.t;
        port: int;
        tags : tag list;
        server_info : (Ip.t * int) option;
        left_bytes : string;
      }
    
    let names_of_tag =
      [
        1, "name";
        17, "version";
        15, "port";
      ]
    
    let parse len s =
      let md4 = get_md4 s 1 in
      let ip = get_ip s 17 in
      let port = get_port s 21 in
(*      Printf.printf "port: %d" port; print_newline (); *)
      let tags, pos = get_tags s 23 names_of_tag in
      let server_info =  Some (get_ip s pos, get_port s (pos+4)) in
      let left_bytes = String.sub s (pos+6) (String.length s - pos - 6) in
      {
        md4 = md4;
        ip = ip;
        port = port;
        tags = tags;
        server_info = server_info;
        left_bytes = left_bytes;
      }
    
    let print t = 
      Printf.printf "CONNECT REPLY:\n";
      Printf.printf "MD4: %s\n" (Md4.to_string t.md4); 
      Printf.printf "ip: %s\n" (Ip.to_string t.ip);
      Printf.printf "port: %d\n" t.port;
      Printf.printf "tags: ";
      print_tags t.tags;
      print_newline ();
      (match t.server_info with
          None -> ()
        | Some (ip, port) ->
            Printf.printf "ip_server: %s\n" (Ip.to_string ip);
            Printf.printf "port_server: %d\n" port);
      String.iter (fun c -> Printf.printf "(%d)" (int_of_char c)) 
      t.left_bytes;
      Printf.printf "\n"
      
    let write buf t =
      buf_md4 buf t.md4;
      buf_ip buf t.ip;
      buf_port buf t.port;
      buf_tags buf t.tags names_of_tag;
      begin
        match t.server_info with
          None -> ()
        | Some (ip, port) ->
            buf_ip buf ip;
            buf_port buf port;
      end;
      Buffer.add_string buf t.left_bytes
  end

module Say = struct
    type t = string
      
    let parse len s =
      let (s, p) = get_string s 1 in
      s
      
    let print t =
      Printf.printf "SAY %s" t
      
    let write buf t =
      buf_string buf t
  end

module EmuleFileDesc = struct
    type t = string
      
    let parse len s =
      let (s, p) = get_string s 1 in
      s
      
    let print t =
      Printf.printf "EMULE FILE DESC %s" t
      
    let write buf t =
      buf_string buf t
  end
  
module OneMd4 = functor(M: sig val m : string end) -> (struct 
    type t = Md4.t
      
    let parse len s = 
      get_md4 s 1
      
    let print t = 
          Printf.printf "%s OF %s" M.m (Md4.to_string t)
          
    let write buf t = 
      buf_md4 buf t
      end)
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
    
module QueryFile  = OneMd4(struct let m = "QUERY FILE" end)
module QueryChunks  = OneMd4(struct let m = "QUERY CHUNKS" end)
  (* Request 79 *)
    
module QueryChunkMd4  = OneMd4(struct let m = "QUERY CHUNKS MD4" end)
module EndOfDownload  = OneMd4(struct let m = "END OF DOWNLOAD MD4" end)
module NoSuchFile  = OneMd4(struct let m = "NO SUCH FILE" end)

module QueryChunksReply = struct (* Request 80 *)
        
    type t = {
        md4 : Md4.t;
        chunks: bool array;
      }
    
    let parse len s = 
      let md4 = get_md4 s 1 in
      let nchunks = get_int16 s 17 in
(*      Printf.printf "nchunks : %d" nchunks; print_newline ();*)
      let chunks = 
        if nchunks = 0 then [||] else
        let chunks = Array.create nchunks false  in
        for i = 0 to (nchunks-1) / 8 do
          let m = get_int8 s (19+i) in
          for j = 0 to 7 do
            let n = i * 8 + j in
            if n < nchunks then
              chunks.(n) <- (m land (1 lsl j)) <> 0;
          done;
        done;
        chunks
      in
      {
        md4 = md4;
        chunks = chunks;
      }
    
    let print t =
      Printf.printf "CHUNKS for %s" (Md4.to_string t.md4);
      print_newline ();
      print_string "   ";
      Array.iter (fun b -> 
          if b then Printf.printf "1" else Printf.printf "0") t.chunks;
      print_newline ()
    
    let write buf t =
      buf_md4 buf t.md4;
      let nchunks = Array.length t.chunks in
      (*
      try
        for i = 0 to nchunks - 1 do
          if not t.chunks.(i) then raise Not_found
        done;
        buf_int16 buf 0
      with _ ->
  *)
          buf_int16 buf nchunks;
          for i = 0 to (nchunks-1) / 8 do
            let m = ref 0 in
            for j = 0 to 7 do
              let n = i * 8 + j in
              if n < nchunks then
                if t.chunks.(n) then
                  m :=  !m lor (1 lsl j);
            done;
            buf_int8 buf !m
          done
          
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
(*      Printf.printf "nchunks : %d" nchunks; print_newline (); *)
      let chunks = Array.create nchunks md4  in
      for i = 0 to nchunks - 1 do
        chunks.(i) <- get_md4 s (19 + i * 16)        
      done;
      {
        md4 = md4;
        chunks = chunks;
      }

    let print t =
      Printf.printf "CHUNKS for %s" (Md4.to_string t.md4);
      print_newline ();
      print_string "   ";
      Array.iter (fun b -> 
          Printf.printf "  %s" (Md4.to_string b))
      t.chunks;
      print_newline ()
      
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
      Printf.printf "QUERY FILE REPLY OF %s\n" (Md4.to_string t.md4);
      Printf.printf "  name = \"%s\"\n" t.name
      
    let write buf t = 
      buf_md4 buf t.md4;
      buf_string buf t.name
  end
    
module Bloc  = struct 
    type t = {
        md4 : Md4.t;
        start_pos : int64;
        end_pos: int64;
        bloc_str: string;
        bloc_begin : int;
        bloc_len : int;
      }
      
    let parse len s = 
      {
        md4 = get_md4 s 1;
        start_pos = get_int64_32 s 17;
        end_pos = get_int64_32 s 21;
        bloc_str = s;
        bloc_begin = 25;
        bloc_len = len - 25;
      }
      
    let print t = 
      Printf.printf "BLOC OF %s [%s - %s]" (Md4.to_string t.md4)
      (Int64.to_string t.start_pos)
      (Int64.to_string t.end_pos)
      
    let write buf t = 
      buf_md4 buf t.md4;
      buf_int64_32 buf t.start_pos;
      buf_int64_32 buf t.end_pos;
      Buffer.add_substring buf t.bloc_str t.bloc_begin t.bloc_len
  end
    
module QueryBloc  = struct 
    type t = {
        md4 : Md4.t;
        start_pos1 : int64; (* 180 ko *)
        end_pos1: int64;
        start_pos2 : int64;
        end_pos2: int64;
        start_pos3 : int64;
        end_pos3: int64;
      }
      
    let parse len s = 
      {
        md4 = get_md4 s 1;
        start_pos1 = get_int64_32 s 17;
        end_pos1 = get_int64_32 s 29;
        start_pos2 = get_int64_32 s 21;
        end_pos2 = get_int64_32 s 33;
        start_pos3 = get_int64_32 s 25;
        end_pos3 = get_int64_32 s 37;
      }
      
    let print t = 
      Printf.printf "QUERY BLOCS OF %s [%s - %s] [%s - %s] [%s - %s]"
      (Md4.to_string t.md4)
      (Int64.to_string t.start_pos1) (Int64.to_string t.end_pos1)
      (Int64.to_string t.start_pos2) (Int64.to_string t.end_pos2)
      (Int64.to_string t.start_pos3) (Int64.to_string t.end_pos3)
      
    let write buf t = 
      buf_md4 buf t.md4;
      buf_int64_32 buf t.start_pos1;
      buf_int64_32 buf t.start_pos2;
      buf_int64_32 buf t.start_pos3;
      buf_int64_32 buf t.end_pos1;
      buf_int64_32 buf t.end_pos2;
      buf_int64_32 buf t.end_pos3
      
  end

  
let unit = ()
module NoArg = functor(M: sig val m : string end) -> (struct 
        type t = unit
        
        let parse len s = ()
        
        let print t = 
          Printf.printf "%s:\n" M.m
        
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
    
module JoinQueue = NoArg(struct let m = "JoinQueue" end)
module AvailableSlot = NoArg(struct let m = "AvailableSlot" end)
module ReleaseSlot = NoArg(struct let m = "ReleaseSlot" end)
module CloseSlot = NoArg(struct let m = "CloseSlot" end)
module ViewFiles = NoArg(struct let m = "VIEW FILES" end)

  
module ViewFilesReply = struct 
    
    type file = {
        md4: Md4.t;
        ip: Ip.t;
        port: int;
        tags: tag list;
      }
    
    type t = tagged_file list
    
    let names_of_tag =
      [
        1, "filename";
        2, "size";
        3, "type";
        4, "format";
      ]
    
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
      Printf.printf "VIEW FILES REPLY:\n";
      List.iter (fun t ->
          Printf.printf "FILE:\n";
          Printf.printf "  MD4: %s\n" (Md4.to_string t.f_md4);
          Printf.printf "  ip: %s\n" (Ip.to_string t.f_ip);
          Printf.printf "  port: %d\n" t.f_port;
          Printf.printf "  tags: ";
          print_tags t.f_tags;
          print_newline ();) t
    
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
  
module OtherLocations = struct 

    type t = Ip.t list
      
    let parse len s =
      let list = ref [] in
      for i = 0 to len / 4 - 1 do
        list := (get_ip s (i*4+1)) :: !list;
      done;
      !list
    
    let print t = 
      Printf.printf "OTHER LOCATIONS:\n";
      List.iter (fun ip ->
          Printf.printf "  ip: %s\n" (Ip.to_string ip);
          print_newline ();) t
        
    let write buf t = 
      List.iter (buf_ip buf) t
  end
  
module NewUserID = struct 

    type t = Ip.t * Ip.t
      
    let parse len s =
      get_ip s 1, get_ip s 5
    
    let print (ip1,ip2) = 
      Printf.printf "NEW USER ID: %s -> %s\n" (Ip.to_string ip1)
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
      let pos = 19 in
      for i = 0 to len - 1 do
        list := (get_ip s (19 + 10 * i), get_port s (23 + 10 * i),
          get_ip  s (25 + 10 * i)) :: !list;
      done;
      { md4 = md4;
        sources = !list;
      }
    
    let print t = 
      Printf.printf "SOURCES for %s:\n" (Md4.to_string t.md4);
      print_newline ();
      List.iter (fun (ip1, port, ip2) ->
          Printf.printf "  %s:%d:%s\n" (Ip.to_string ip1) port(Ip.to_string ip2);
          print_newline ();) t.sources
        
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
        version : int; (* CURRENT_VERSION_SHORT = 0x24*)
        protversion : int; (* EMULE_PROTOCOL_VERSION = 0x1 *)
        tags : tag list;
      }
      
    let names_of_tag =
      [
        0x20, "compression"; (* ET_COMPRESSION *)
        0x21, "udp_port"     (* ET_UDPPORT *)
      ]
      
    let parse len s =
      let version = get_int8 s 1 in
      let protversion = get_int8 s 2 in
      let tags,_ = get_tags s 3 names_of_tag in
      {
        version = version; 
        protversion = protversion;
        tags = tags;
      }
      
    let print m t = 
      Printf.printf "%s:\n" m;
      Printf.printf "  version: %d\n" t.version;
      Printf.printf "  protversion: %d\n" t.version;
      Printf.printf "  tags: ???"; print_newline ()
        
    let write buf t = 
      buf_int8 buf t.version;
      buf_int8 buf t.protversion;
      buf_tags buf t.tags names_of_tag;
      
  end
      
module EmuleQueueRanking = struct 

    type t = int
      
    let parse len s = get_int16 s 1      
    let print t = 
      Printf.printf "QUEUE RANKING: %d" t; print_newline ()

    let string_null10 = String.make 10 (char_of_int 0)
      
    let write buf t = 
      buf_int16 buf t;
      Buffer.add_string buf string_null10
            
  end

module QueueRanking = struct 

    type t = int
      
    let parse len s = get_int s 1      
    let print t = 
      Printf.printf "QUEUE RANKING: %d" t; print_newline ()

    let write buf t = 
      buf_int buf t
            
  end
      
module EmuleRequestSources = struct 

    type t =  Md4.t
      
    let parse len s = 
      get_md4 s 1
      
    let print t = 
      Printf.printf "EMULE REQUEST SOURCES: %s" (Md4.to_string t);
      print_newline ()

    let write buf t = 
      buf_md4 buf t 
            
  end
      
module EmuleRequestSourcesReply = struct 
    
    type source = {
        ip : Ip.t;
        port : int;
        server_ip : Ip.t;
        server_port : int;
      }
    
    type t = {
        md4 : Md4.t;
        sources : source array;        
      }
    
    let dummy_source = {
        ip = Ip.null;
        port = 0;
        server_ip = Ip.null;
        server_port = 0;
      }
    
    let parse len s = 
      let md4 = get_md4 s 1 in
      let ncount = get_int16 s 17 in
      let sources =
        if len = 1 + 16 + ncount * 6 then
          let nsources = 2 * ncount in
          let sources = Array.create nsources dummy_source in
          for i = 0 to nsources-1 do
            let pos = 19 + i * 6 in
            sources.(i) <- {
              ip = get_ip s pos;
              port = get_int16 s (pos+4);
              server_ip = Ip.null;
              server_port = 0;
            }
          done;
          sources
        else
        let nsources = ncount in
        let sources = Array.create nsources dummy_source in
        for i = 0 to nsources-1 do
          let pos = 19 + i *12 in
          sources.(i) <- {
            ip = get_ip s pos;
            port = get_int16 s (pos+4);
            server_ip = get_ip s (pos+6);
            server_port = get_int16 s (pos+10);
          }
        done;
        sources
      in
      {
        md4 = md4;
        sources = sources;
      }
    
    let print t = 
      Printf.printf "EMULE SOURCES REPLY: %d sources for %s" 
        (Array.length t.sources)
      (Md4.to_string t.md4); 
      print_newline ();
      Array.iter (fun s ->
          if Ip.valid s.ip then
            Printf.printf "  %s:%d" (Ip.to_string s.ip) s.port
          else 
            Printf.printf "  Indirect from %s:%d"
              (Ip.to_string s.server_ip) s.server_port;
          print_newline ();
      ) t.sources

    let write buf t = 
      buf_md4 buf t.md4;
      buf_int16 buf (Array.length t.sources);
      ()
            
  end

  
type t = 
| ConnectReq of Connect.t
| ConnectReplyReq of ConnectReply.t
| QueryFileReq of QueryFile.t
| QueryFileReplyReq of QueryFileReply.t
| BlocReq of Bloc.t
| QueryBlocReq of QueryBloc.t
| JoinQueueReq of JoinQueue.t (* sent before queryBloc *)
| AvailableSlotReq of AvailableSlot.t
| ReleaseSlotReq of ReleaseSlot.t
| CloseSlotReq of CloseSlot.t
| QueryChunksReq of QueryChunks.t
| QueryChunksReplyReq of QueryChunksReply.t
| QueryChunkMd4Req of QueryChunkMd4.t
| QueryChunkMd4ReplyReq of QueryChunkMd4Reply.t
| ViewFilesReq of ViewFiles.t
| ViewFilesReplyReq of ViewFilesReply.t
| QueueReq of OtherLocations.t
| UnknownReq of string
| OtherLocationsReq of OtherLocations.t
| SayReq of Say.t
| SourcesReq of Sources.t
| EndOfDownloadReq of EndOfDownload.t
| NewUserIDReq of NewUserID.t
| NoSuchFileReq of NoSuchFile.t  
| QueueRankingReq of QueueRanking.t
  
  
| EmuleClientInfoReq of EmuleClientInfo.t
| EmuleClientInfoReplyReq of EmuleClientInfo.t
| EmuleQueueRankingReq of EmuleQueueRanking.t
| EmuleRequestSourcesReq of EmuleRequestSources.t
| EmuleRequestSourcesReplyReq of EmuleRequestSourcesReply.t
| EmuleFileDescReq of EmuleFileDesc.t
        
let print t =
  begin
    match t with
    | ConnectReq t -> Connect.print t
    | ConnectReplyReq t -> ConnectReply.print t
    | QueryFileReq t -> QueryFile.print t
    | QueryFileReplyReq t -> QueryFileReply.print t
    | BlocReq t -> Bloc.print t
    | QueryBlocReq t -> QueryBloc.print t
    | JoinQueueReq t -> JoinQueue.print t
    | AvailableSlotReq t -> AvailableSlot.print t
    | ReleaseSlotReq t -> ReleaseSlot.print t
    | CloseSlotReq t -> CloseSlot.print t    
    | QueryChunksReq t -> QueryChunks.print t    
    | QueryChunksReplyReq t -> QueryChunksReply.print t    
    | QueryChunkMd4Req t -> QueryChunkMd4.print t    
    | QueryChunkMd4ReplyReq t -> QueryChunkMd4Reply.print t    
    | ViewFilesReplyReq t -> ViewFilesReply.print t
    | ViewFilesReq t -> ViewFiles.print t    
    | QueueReq t -> OtherLocations.print t    
    | OtherLocationsReq t  -> OtherLocations.print t
    | SayReq t -> Say.print t
    | SourcesReq t -> Sources.print t
    | EndOfDownloadReq t -> EndOfDownload.print t
    | NewUserIDReq t -> NewUserID.print t
    | NoSuchFileReq t -> NoSuchFile.print t
    | QueueRankingReq t -> 
        QueueRanking.print t
        
    | EmuleClientInfoReq t -> 
        EmuleClientInfo.print "EMULE CLIENT INFO"  t
    | EmuleClientInfoReplyReq t -> 
        EmuleClientInfo.print "EMULE CLIENT INFO REPLY" t
    | EmuleQueueRankingReq t -> 
        EmuleQueueRanking.print t
    | EmuleRequestSourcesReq t -> 
        EmuleRequestSources.print  t
    | EmuleRequestSourcesReplyReq t -> 
        EmuleRequestSourcesReply.print t
        
    | EmuleFileDescReq t -> EmuleFileDesc.print t
        
    | UnknownReq s ->  
        let len = String.length s in
        Printf.printf "UnknownReq:\n";
        Printf.printf "ascii: [";
        for i = 0 to len - 1 do
          let c = s.[i] in
          let n = int_of_char c in
          if n > 31 && n < 127 then
            Printf.printf " %c" c
          else
            Printf.printf "(%d)" n
        done;
        Printf.printf "]\n";
        Printf.printf "dec: [";
        for i = 0 to len - 1 do
          let c = s.[i] in
          let n = int_of_char c in
          Printf.printf "(%d)" n            
        done;
        Printf.printf "]\n"
  end

let parse_emule_packet opcode len s =
(*
  Printf.printf "Emule magic: %d opcode %d:" magic opcode; print_newline ();
          dump s; print_newline ();
  *)        
  let t = match opcode with
    | 1 -> EmuleClientInfoReq (EmuleClientInfo.parse len s)
    | 2 -> EmuleClientInfoReplyReq (EmuleClientInfo.parse len s)
    | 0x60 -> EmuleQueueRankingReq (EmuleQueueRanking.parse len s)
    | 0x81 -> EmuleRequestSourcesReq (EmuleRequestSources.parse len s)
    | 0x82 -> EmuleRequestSourcesReplyReq (
          EmuleRequestSourcesReply.parse len s)
    | 0x61 -> EmuleFileDescReq (EmuleFileDesc.parse len s)

(*
#define OP_COMPRESSEDPART       0x40
#define OP_FILEDESC             0x61
#define OP_VERIFYUPSREQ         0x71
#define OP_VERIFYUPSANSWER      0x72
#define OP_UDPVERIFYUPREQ       0x73
#define OP_UDPVERIFYUPA         0x74
*)
    
    | _ -> raise Not_found
  in
(*
          Printf.printf "EMULE MESSAGE: "; print_newline ();
          print t;
          print_newline (); *)
  t
  
  
let parse magic s =
  try 
    let len = String.length s in
    if len = 0 then raise Not_found;
    let opcode = int_of_char (s.[0]) in
(*Printf.printf "opcode: %d" opcode; print_newline (); *)
    match magic with
      227 ->
        begin
          match opcode with 
          | 1 -> ConnectReq (Connect.parse len s)
          | 70 -> BlocReq (Bloc.parse len s)
          | 71 -> QueryBlocReq (QueryBloc.parse len s)
          | 72 -> NoSuchFileReq (NoSuchFile.parse len s)
          | 73 -> EndOfDownloadReq (EndOfDownload.parse len s)
          | 74 -> ViewFilesReq (ViewFiles.parse len s)
          | 75 -> ViewFilesReplyReq (ViewFilesReply.parse len s)
          | 76 -> ConnectReplyReq (ConnectReply.parse len s)
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
(* CloseSlot: the upload slot is not available *)
          | 87 -> CloseSlotReq (CloseSlot.parse len s)
          | 88 -> QueryFileReq (QueryFile.parse len s)
          | 89 -> QueryFileReplyReq (QueryFileReply.parse len s)
          | 92 -> QueueRankingReq (QueueRanking.parse len s)
          | 250 -> SourcesReq (Sources.parse len s)        
              
          | _ -> raise Not_found
        end 

    | 0xc5  -> (* emule extended protocol *)
        parse_emule_packet opcode len s

(* Compressed packet, probably sent by cDonkey ? *)
        
    | 0xD4 ->
        
        let s = Autoconf.zlib_uncompress (String.sub s 1 (len-1)) in
        let s = Printf.sprintf "%c%s" (char_of_int opcode) s in
        parse_emule_packet opcode (String.length s) s
        
        (*
        Printf.printf "Compressed message decompressed with opcode %d" opcode; print_newline ();
        if !CommonOptions.verbose_unknown_messages then begin       
            let tmp_file = Filename.temp_file "comp" "unpak" in
            File.from_string tmp_file s;
            Printf.printf "Saved compressed packet %s" tmp_file; print_newline ();
          end;	   
        UnknownReq s        *)
    | _ -> 
        if !CommonOptions.verbose_unknown_messages then begin
            Printf.printf "Strange magic: %d" magic; print_newline ();
          end;
        raise Not_found
  with
  | e -> 
      if !CommonOptions.verbose_unknown_messages then begin
          Printf.printf "Unknown message From client: %s (magic %d)"
              (Printexc2.to_string e) magic; print_newline ();
	      	     let tmp_file = Filename.temp_file "comp" "pak" in
	     File.from_string tmp_file s;
	     Printf.printf "Saved unknown packet %s" tmp_file; print_newline ();

          dump s;
          print_newline ();
        end;
      UnknownReq s
  
let write buf t =
  match t with
  | ConnectReq t -> 
      buf_int8 buf 1;
      Connect.write buf t
  | ConnectReplyReq t -> 
      buf_int8 buf 76;
      ConnectReply.write buf t
  | QueryFileReq t -> 
      buf_int8 buf 88;
      QueryFile.write buf t
  | QueryFileReplyReq t -> 
      buf_int8 buf 89;
      QueryFileReply.write buf t
  | QueueReq t ->
      buf_int8 buf 77;
      OtherLocations.write buf t
  | QueryBlocReq t ->
      buf_int8 buf 71;
      QueryBloc.write buf t
  | BlocReq t -> 
      buf_int8 buf 70;
      Bloc.write buf t
  | JoinQueueReq t -> 
      buf_int8 buf 84;
      JoinQueue.write buf t
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
  | CloseSlotReq t -> 
      buf_int8 buf 87;
      CloseSlot.write buf t
  | ViewFilesReq t -> 
      buf_int8 buf 74;
      ViewFiles.write buf t
  | ViewFilesReplyReq t -> 
      buf_int8 buf 75;
      ViewFilesReply.write buf t
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
  | QueueRankingReq t ->
      buf_int8 buf 92;
      QueueRanking.write buf t
      
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
      EmuleRequestSourcesReply.write buf t
  | EmuleFileDescReq t ->
      buf_int8 buf 0x61;
      EmuleFileDesc.write buf t
      
      
  | UnknownReq s ->
      Buffer.add_string buf s

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