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
open Options
open Md4
open TcpBufferedSocket

open CommonGlobals
open CommonTypes
open CommonOptions
  
open GnutellaOptions
open GnutellaTypes
open GnutellaProtocol
open GnutellaGlobals
  
type addr = Ip.t * int
  
type g2_packet =
| PI
| PI_RELAY
| PI_UDP of addr
  
| PO
| PO_RELAY
  
| LNI
| LNI_NA of addr (* with or without the port ??? *)
| LNI_GU of Md4.t (* [16] uid *)
| LNI_V of string (* [4] vendor *)
| LNI_LS of int32 (* files *) * int32 (* kB *)
| LNI_HS of int (* [2] leaves *) * int (* [2] max_leaves *)
  
| KHL 
| KHL_TS of int64
| KHL_NH of addr
| KHL_NH_GU of Md4.t
| KHL_NH_V of string (* [4] vendor *)
| KHL_NH_LS of int32 (* files *) * int32 (* kB *)
| KHL_NH_HS of int (* [2] leaves *) * int (* [2] max_leaves *)  
| KHL_CH of (addr * int32) (* last conn *)
| KHL_CH_GU of Md4.t
| KHL_CH_V of string (* [4] vendor *)
| KHL_CH_LS of int32 (* files *) * int32 (* kB *)
| KHL_CH_HS of int (* [2] leaves *) * int (* [2] max_leaves *)  

| PUSH of addr
  
| QHT_RESET
| QHT_PATCH of GnutellaProtocol.QrtPatch.t
  
| QKR
| QKR_RNA of addr
| QKA
| QKA_QK of int32
| QKA_SNA of addr
| QKA_QNA of addr
  
| Q2 of Md4.t
| Q2_UDP of addr * int32 option
| Q2_URN of CommonTypes.file_uid
| Q2_DN of string
| Q2_MD of string
| Q2_SZR of int32 * int32
| Q2_I of string list
  
| QA of Md4.t
| QA_TS of int32
| QA_D of addr * int (* [2] leaves number *)
| QA_S of addr * int32 option
| QA_FR of addr
  
| QH2 of int (* [1] hop count *) * Md4.t
| QH2_GU of Md4.t
| QH2_NA of addr
| QH2_NH of addr
| QH2_V of string (* [4] *)
| QH2_BUP
(*  | QH2_SS ??? *)
| QH2_PCH (* chat *)
| QH2_BH  (* browse  host *)
| QH2_HG of int (* [1] group id *)
| QH2_SS of int (* [2]queue *) * int (* [1]capacity *) * int (* [4]speed *)
| QH2_HG_SS of int (* [2]queue *) * int (* [1]capacity *) * int (* [4]speed *)
| QH2_H 
| QH2_H_URN of file_uid
| QH2_H_URL of string
| QH2_H_DN of int64 (* only of no SZ *) * string * string
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

| UPROD
| UPROD_XML of string
| UPROC  
| Unknown of string list * bool * string
  
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

    let get_uid s pos len =
      let f, pos = get_string s pos len in
      match f with
      | "ed2k" -> 
          let ed2k = String.sub s pos 16 in
          let ed2k = Md4.direct_of_string ed2k in
          Ed2k (Printf.sprintf "urn:ed2k:%s" (Md4.to_string ed2k), ed2k),
          pos+16
      | "bitprint" | "bp" -> 
          let sha1 = String.sub s pos 20 in
          let ttr = String.sub s (pos+20) 24 in
          let sha1 = Sha1.direct_of_string sha1 in
          let ttr = Tiger.direct_of_string ttr in
          Bitprint (Printf.sprintf "urn:bitprint:%s.%s" 
              (Sha1.to_string sha1)
            (Tiger.to_string ttr), sha1, ttr),
          pos+44
      | "sha1" -> 
          let sha1 = String.sub s pos 20 in
          let sha1 = Sha1.direct_of_string sha1 in
          Sha1 (Printf.sprintf "urn:sha1:%s" 
              (Sha1.to_string sha1),  sha1), pos + 20
          
      | "tree:tiger" | "ttr" -> 
          let ttr = String.sub s pos 24 in
          let ttr = Tiger.direct_of_string ttr in
          TigerTree (Printf.sprintf "urn:ttr:%s" 
              (Tiger.to_string ttr), ttr), pos + 24
      | "md5" -> 
          let ed2k = String.sub s pos 16 in
          let ed2k = Md5.direct_of_string ed2k in
          Md5 (Printf.sprintf "urn:md5:%s" (Md5.to_string ed2k), ed2k),
          pos + 16
      | _ -> 
          failwith (Printf.sprintf 
              "Unknown Uniq Ressource Identifier: %s" f), pos
      
  end
  
module Print = struct
    
    module M = struct
        let buf_addr buf (ip,port) =
          Printf.bprintf buf " %s:%d " (Ip.to_string ip) port
        
        let buf_int64_32 buf v = Printf.bprintf buf " %Ld " v
        let buf_int64 buf v = Printf.bprintf buf " %Ld " v
        let buf_int32 buf v = Printf.bprintf buf " %ld " v
        let buf_int16 buf v = Printf.bprintf buf " %d " v
        let buf_int8 buf v = Printf.bprintf buf " %d " v
        let buf_int buf v = Printf.bprintf buf " %d " v
      end
    
    module Buffer2 = struct
        let add_string buf s = Printf.bprintf buf " '%s'" s
      end
    
    let buf_md4 buf v = 
      Printf.bprintf buf " %s" (Md4.to_string v)
    
    let buf = Buffer.create 100
    let print_payload msg = 
      Buffer.clear buf;
      let name = 
        let module Buffer = Buffer2 in
        match msg with 
        | TO md4 -> buf_md4 buf md4; "TO"
        | PI -> "PI"
        | PI_RELAY ->  "RELAY"
        | PI_UDP addr -> M.buf_addr buf addr;  "UDP"
        | PO -> "PO"
        | PO_RELAY ->  "RELAY"
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
        | KHL_NH_HS (a,b) -> M.buf_int8 buf a; M.buf_int8 buf b; "HS"
        | KHL_CH_HS (a,b) -> M.buf_int16 buf a; M.buf_int16 buf b; "HS"
        | KHL -> "KHL"
        | KHL_TS int32 -> M.buf_int64_32 buf int32; "TS"
        | KHL_NH addr -> M.buf_addr buf addr; "NH"
        
        | KHL_CH (addr, int32) -> 
            M.buf_addr buf addr; M.buf_int32 buf int32; "CH"
        
        | PUSH addr -> M.buf_addr buf addr; "PUSH"
        
        
        | QKR -> "QKR"
        | QKR_RNA addr -> M.buf_addr buf addr; "RNA" 
        | QKA -> "QKA"
        | QKA_SNA addr -> M.buf_addr buf addr; "SNA" 
        | QKA_QNA addr -> M.buf_addr buf addr; "QNA" 
        
        | Q2 md4 -> buf_md4 buf md4; "Q2"
        | Q2_UDP (addr, Some int32) -> M.buf_addr buf addr; M.buf_int32 buf int32; "UDP"
        | Q2_UDP (addr, None) -> M.buf_addr buf addr; "UDP"
        | Q2_URN s -> Buffer.add_string buf (string_of_uid s);   "URN"
        | Q2_DN s -> Buffer.add_string buf s; "DN"
        | Q2_SZR (a32, b32) -> M.buf_int32 buf a32; M.buf_int32 buf b32; "SZR"
        | Q2_I list ->
            Buffer.add_string buf (String2.unsplit list '+'); "I"
        
        | QA md4 -> buf_md4 buf md4; "QA"
        | QA_TS int32 -> M.buf_int32 buf int32; "TS"
        | QA_D (addr, int16) -> M.buf_addr buf addr; M.buf_int16 buf int16; "D"
        | QA_S (addr, Some int32) -> M.buf_addr buf addr; M.buf_int32 buf int32; "S"
        | QA_S (addr, None) -> M.buf_addr buf addr; "S"
        | QA_FR addr -> M.buf_addr buf addr; "FR"
        
        | QH2 (char, md4) -> M.buf_int8 buf char; buf_md4 buf md4; "QH2"
        | QH2_GU md4 -> buf_md4 buf md4; "GU"
        | QH2_V s -> Buffer.add_string buf s; "V"
        | QH2_NA addr -> M.buf_addr buf addr; "NA"
        | QH2_NH addr -> M.buf_addr buf addr; "NH"
        | QH2_BUP -> "BUP"
        | QH2_PCH -> "PCH"
        | QH2_BH -> "BH"
        | QH2_HG c -> M.buf_int8 buf c; "HG"
        | QH2_SS (a16,b8,c) ->
            M.buf_int16 buf a16; M.buf_int8 buf b8; M.buf_int buf c; "SS"
        | QH2_HG_SS (a16,b8,c) ->
            M.buf_int16 buf a16; M.buf_int8 buf b8; M.buf_int buf c; "SS"
        | QH2_H -> "H"
        
        | QH2_H_URN s -> Buffer.add_string buf (string_of_uid s); "URN"
        | QH2_H_URL s -> Buffer.add_string buf s; "URL"
        | QH2_H_COM s -> Buffer.add_string buf s; "COM"
        | QH2_H_PVU s -> Buffer.add_string buf s; "PVU"
        | QH2_MD s -> Buffer.add_string buf s; "MD"
        | Q2_MD s -> Buffer.add_string buf s; "MD"
        | QH2_UPRO -> "UPRO"
        | QH2_UPRO_NICK s -> Buffer.add_string buf s; "NICK"
        | QH2_H_DN (int32, s1,s2) -> 
            Buffer.add_string buf s2; "DN"
        | QH2_H_SZ sz -> M.buf_int64 buf sz; "SZ"
        | QH2_H_G i8 -> M.buf_int8 buf i8; "G"
        | QH2_H_ID i32 -> M.buf_int32 buf i32; "ID"
        | QH2_H_CSC i16 -> M.buf_int16 buf i16; "CSC"
        | QH2_H_PART i32 -> M.buf_int32 buf i32; "PART"
        | QHT_PATCH p -> 
            "QHT_PATCH"
        | QHT_RESET -> "QHT_RESET"
        
        | UPROC -> "UPROC"
        | UPROD -> "UPROD"
        | UPROD_XML s -> Buffer.add_string buf s; "XML"
        
        | Unknown (names, be,s) ->
            (List.iter (fun s ->
                  Printf.bprintf buf "/%s" s) names;
              Printf.bprintf buf "\n DUMP: %s" (AnyEndian.sdump s));
            "UNKNOWN"
        | QKA_QK i32 -> M.buf_int32 buf i32; "QKA_QK"
      
      
      in  
      name, Buffer.contents buf
    
    let print p =
      try
        let buf = Buffer.create 100 in
        let rec iter buf indent p =
          let name, content = print_payload p.g2_payload in
          Printf.bprintf buf "%s%s%s\n" indent name content;
          List.iter (iter buf (indent^"  ")) p.g2_children;
        in
        iter buf "" p;
        Buffer.contents buf        
      with e ->
          lprintf "Exception %s in Print.print\n"
            (Printexc2.to_string e);
          raise e
  end  
  
let buf = Buffer.create 50 
  
let buf_uid buf s = match s with
  | Bitprint (_,sha1, ttr) ->
      Buffer.add_string buf "bp\000"; 
      Buffer.add_string buf (Sha1.direct_to_string sha1);
      Buffer.add_string buf (Tiger.direct_to_string ttr);
  | Sha1 (_,sha1) ->
      Buffer.add_string buf "sha1\000"; 
      Buffer.add_string buf (Sha1.direct_to_string sha1)
  | Ed2k (_,ed2k) ->
      Buffer.add_string buf "ed2k\000"; 
      Buffer.add_string buf (Md4.direct_to_string ed2k)
  | Md5 (_, md5) ->
      Buffer.add_string buf "md5\000"; 
      Buffer.add_string buf (Md5.direct_to_string md5)
  | TigerTree (_, ttr) ->
      Buffer.add_string buf "ttr\000"; 
      Buffer.add_string buf (Tiger.direct_to_string ttr)

let g2_encode_payload msg = 
  let module M = G2_LittleEndian in
  Buffer.clear buf;
  let name = 
    match msg with 
    | TO md4 -> buf_md4 buf md4; "TO"
    | PI -> "PI"
    | PI_RELAY ->  "RELAY"
    | PI_UDP addr -> M.buf_addr buf addr;  "UDP"
    | PO -> "PO"
    | PO_RELAY ->  "RELAY"
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
    | KHL_NH_HS (a,b)  -> buf_int8 buf a; buf_int8 buf b; "HS"
    | LNI_HS (a,b)
    | KHL_CH_HS (a,b) -> M.buf_int16 buf a; M.buf_int16 buf b; "HS"
    | KHL -> "KHL"
    | KHL_TS int32 -> M.buf_int64_32 buf int32; "TS"
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
    | Q2_UDP (addr, Some int32) -> M.buf_addr buf addr; M.buf_int32 buf int32; "UDP"
    | Q2_UDP (addr, None) -> M.buf_addr buf addr; "UDP"
    | Q2_URN s -> buf_uid buf s; "URN"
    | Q2_DN s -> Buffer.add_string buf s; "DN"
    | Q2_MD s -> Buffer.add_string buf s; "MD"
    | Q2_SZR (a32, b32) -> M.buf_int32 buf a32; M.buf_int32 buf b32; "SZR"
    | Q2_I list ->
        Buffer.add_string buf (String2.unsplit list '\000'); "I"
    
    | QA md4 -> buf_md4 buf md4; "QA"
    | QA_TS int32 -> M.buf_int32 buf int32; "TS"
    | QA_D (addr, int16) -> M.buf_addr buf addr; M.buf_int16 buf int16; "D"
    | QA_S (addr, Some int32) -> M.buf_addr buf addr; M.buf_int32 buf int32; "S"
    | QA_S (addr, None) -> M.buf_addr buf addr; "S"
        
    | QA_FR addr -> M.buf_addr buf addr; "FR"
    
    | QH2 (char, md4) -> buf_int8 buf char; buf_md4 buf md4; "QH2"
    | QH2_GU md4 -> buf_md4 buf md4; "GU"
    | QH2_V s -> Buffer.add_string buf s; "V"
    | QH2_NA addr -> M.buf_addr buf addr; "NA"
    | QH2_NH addr ->  M.buf_addr buf addr; "NH"
    | QH2_BUP -> "BUP"
    | QH2_PCH -> "PCH"
    | QH2_BH -> "BH"
    | QH2_HG c -> buf_int8 buf c; "HG"
    | QH2_SS (a16,b8,c) ->
        M.buf_int16 buf a16; buf_int8 buf b8; M.buf_int buf c; "SS"
    | QH2_HG_SS (a16,b8,c) ->
        M.buf_int16 buf a16; buf_int8 buf b8; M.buf_int buf c; "SS"
    | QH2_H -> "H"
    
    | QH2_H_URN s -> buf_uid buf s; "URN"
    | QH2_H_URL s -> Buffer.add_string buf s; "URL"
    | QH2_H_COM s -> Buffer.add_string buf s; "COM"
    | QH2_H_PVU s -> Buffer.add_string buf s; "PVU"
    | QH2_MD s -> Buffer.add_string buf s; "MD"
    | QH2_UPRO -> "UPRO"
    | QH2_UPRO_NICK s -> Buffer.add_string buf s; "NICK"
    | QH2_H_DN (int32, s1,s2) -> 
        Buffer.add_string buf s2; "DN"
    | QH2_H_SZ sz -> M.buf_int64 buf sz; "SZ"
    | QH2_H_G i8 -> buf_int8 buf i8; "G"
    | QH2_H_ID i32 -> M.buf_int32 buf i32; "ID"
    | QH2_H_CSC i16 -> M.buf_int16 buf i16; "CSC"
    | QH2_H_PART i32 -> M.buf_int32 buf i32; "PART"
    | UPROC -> "UPROC"        
    | UPROD -> "UPROD"
    | UPROD_XML s -> Buffer.add_string buf s; "XML"
    | QHT_PATCH p -> 
        buf_int8 buf 1;
        buf_int8 buf p.QrtPatch.seq_no;
        buf_int8 buf p.QrtPatch.seq_size;
        buf_int8 buf p.QrtPatch.compressor;
        buf_int8 buf p.QrtPatch.entry_bits;
        Buffer.add_string buf p.QrtPatch.table;
        "QHT"
        
    | QHT_RESET -> 
(*
0: reset
0 0 16 0: table size
1 : infinity

*)
        
        Buffer.add_string buf "\000\000\000\016\000\001"; "QHT"
      
    | QKA_QK i32 -> M.buf_int32 buf i32; "QKA_QK"
    | Unknown _ -> failwith "Unknown packet"
  in  
  name, Buffer.contents buf

let buf = ()

let rec g2_encode header pkt =   
  let children = List.map (g2_encode "") pkt.g2_children in
  let name, payload = g2_encode_payload pkt.g2_payload in
  let name_len = String.length name in
  let size = ref (String.length payload) in
  if children <> [] then begin
      if payload <> "" then incr size;
      List.iter (fun c -> size := !size + String.length c) children
    end;
  let buf = Buffer.create 100 in
  Buffer.add_string buf header;
  let len_len = 
    if !size < 256 then 1 else 
    if !size < 65536 then 2 else 3
  in
  let cb = 
    (if children <> [] then 4 else 0) lor
      (len_len lsl 6) lor ((name_len-1) lsl 3)
  in
  let cb = if cb = 0 then 4 else cb in
(*    lprintf "encode: cb = %d size = %d len_len= %d\n" cb !size len_len; *)
  
  Buffer.add_char buf (char_of_int cb);
  if len_len = 1 then 
    buf_int8 buf !size else
  if len_len = 2 then 
    LittleEndian.buf_int16 buf !size else
    LittleEndian.buf_int24 buf !size;
  
  Buffer.add_string buf name;
  List.iter (fun c -> Buffer.add_string buf c) children;
  if children <> [] && payload <> "" then Buffer.add_char buf '\000';
  Buffer.add_string buf payload;
  Buffer.contents buf
  

   
  
let g2_decode_payload names be s =
(*
  lprintf "names:"; List.iter (fun s -> lprintf "/%s" s) names;
  lprintf "\n";
  dump s;
lprintf "\n";
  *)
  try
    if be then 
      (lprintf "Big Endian not supported yet\n"; raise Exit);
    let module M = G2_LittleEndian in
    match names with
    
    | "TO" :: _ -> TO (get_md4 s 0)
    
    | [ "PI" ] -> PI
    | [ "RELAY"; "PI" ] -> PI_RELAY
    | [ "UDP"; "PI" ] -> PI_UDP (M.get_addr s 0 (String.length s))
    
    | [ "PO" ] -> PO
    | [ "RELAY" ; "PO" ] -> PO_RELAY
    
    | [ "LNI" ] -> LNI
    | [ "NA"; "LNI" ] -> LNI_NA  (M.get_addr s 0 (String.length s))
    | [ "GU"; "LNI" ] -> LNI_GU (get_md4 s 0)
    | [ "V" ; "LNI" ] -> LNI_V (String.sub s 0 4)
    | [ "LS"; "LNI" ] -> LNI_LS (M.get_int32 s 0, M.get_int32 s 4)
    | [ "HS"; "LNI" ] -> LNI_HS (M.get_int16 s 0, M.get_int16 s 2)
    
    | [ "KHL" ] -> KHL
    | [ "TS"; "KHL" ] -> KHL_TS (M.get_int64_32 s 0)
    | [ "NH" ; "KHL" ] -> 
        KHL_NH (M.get_addr s 0 (String.length s))
    | [ "GU"; "NH" ;"KHL" ] -> KHL_NH_GU (get_md4 s 0)
    | [ "V" ; "NH" ;"KHL" ] -> KHL_NH_V (String.sub s 0 4)
    | [ "LS"; "NH" ;"KHL" ] -> KHL_NH_LS (M.get_int32 s 0, M.get_int32 s 4)
    | [ "HS"; "NH" ;"KHL" ] -> 
        assert (String.length s = 2);
        KHL_NH_HS (get_int8 s 0, get_int8 s 1)
    | [ "CH" ; "KHL" ] -> 
        let len = String.length s in
        KHL_CH (M.get_addr s 0 (len - 4), M.get_int32 s (len-4))
    | [ "GU"; "CH" ;"KHL" ] -> KHL_CH_GU (get_md4 s 0)
    | [ "V" ; "CH" ;"KHL" ] -> KHL_CH_V (String.sub s 0 4)
    | [ "LS"; "CH" ;"KHL" ] -> KHL_CH_LS (M.get_int32 s 0, M.get_int32 s 4)
    | [ "HS"; "CH" ;"KHL" ] -> KHL_CH_HS (M.get_int16 s 0, M.get_int16 s 2)
    
    | [ "PUSH" ] -> PUSH (M.get_addr s 0 (String.length s))
    
    | [ "QKR" ] -> QKR
    | [ "RNA" ; "QKR" ] -> QKR_RNA  (M.get_addr s 0 (String.length s))
    | [ "QKA" ] -> QKA
    | [ "QK" ; "QKA" ] ->  QKA_QK  (M.get_int32 s 0)
    | [ "SNA" ; "QKA" ] -> QKA_SNA  (M.get_addr s 0 (String.length s))
    | [ "QNA" ; "QKA" ] -> QKA_QNA  (M.get_addr s 0 (String.length s))
    
    | [ "Q2" ] -> Q2 (get_md4 s 0)
    | [ "UDP"; "Q2" ] -> 
        let len = String.length s in
        if len > 6 then
          Q2_UDP (M.get_addr s 0 (len - 4), Some (M.get_int32 s (len-4)))
        else
          Q2_UDP (M.get_addr s 0 len, None)
    | [ "MD"; "Q2" ] -> 
        let s, pos = M.get_string s 0 (String.length s) in Q2_MD s
    
    | [ "URN"; "Q2" ] -> 
        let s, pos = M.get_uid s 0 (String.length s) in Q2_URN s
    | [ "DN"; "Q2" ] -> 
        let s, pos = M.get_string s 0 (String.length s) in Q2_DN s
    | [ "SZR"; "Q2" ] -> 
        Q2_SZR (M.get_int32 s 0, M.get_int32 s 4)
    | [ "I"; "Q2" ] -> 
        Q2_I (String2.split s '\000')
    
    | [ "QA" ] -> QA (get_md4 s 0)
    | [ "TS"; "QA" ] -> QA_TS (M.get_int32 s 0)
    | [ "D"; "QA" ] -> QA_D (M.get_addr s 0 6, M.get_int16 s 6)
    | [ "S"; "QA" ] -> 
        let len = String.length s in
        if len > 6 then
          QA_S (M.get_addr s 0 (len - 4), Some (M.get_int32 s (len-4)))
        else
          QA_S (M.get_addr s 0 len, None)
    | [ "FR"; "QA" ] -> QA_FR (M.get_addr s 0 (String.length s))
    
    | [ "QH2" ] -> QH2 (get_int8 s 0, get_md4 s 1)
    | [ "GU"; "QH2" ] -> QH2_GU (get_md4 s 0)
    | [ "V" ; "QH2" ] -> QH2_V (String.sub s 0 4)
    | [ "NA"; "QH2" ] -> QH2_NA (M.get_addr s 0 (String.length s))
    | [ "NH"; "QH2" ] -> QH2_NH (M.get_addr s 0 (String.length s))
    | [ "BUP"; "QH2" ] -> QH2_BUP
    | [ "BH"; "QH2" ] -> QH2_BH
    | [ "PCH"; "QH2" ] -> QH2_PCH
    
    | [ "HG"; "QH2" ] -> QH2_HG (get_int8 s 0)
    | [ "SS"; "HG"; "QH2" ] -> 
        QH2_HG_SS (M.get_int16 s 0, get_int8 s 2, M.get_int s 3)
    | [ "SS"; "QH2" ] -> 
        QH2_SS (M.get_int16 s 0, get_int8 s 2, M.get_int s 3)
    
    | [ "H"; "QH2" ] -> QH2_H
    | [ "URN"; "H"; "QH2" ] -> 
        let s, pos = M.get_uid s 0 (String.length s) in QH2_H_URN s
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
        let len = String.length s in
        if len >= 4 then
          let dn, pos = M.get_string s 4 len in
          QH2_H_DN (M.get_int64_32 s 0, dn,s)
        else
          QH2_H_DN (Int64.zero, s,s)
    | [ "SZ"; "H"; "QH2" ] -> 
        let len = String.length s in
        QH2_H_SZ (if len = 4 then  Int64.of_int32 (M.get_int32 s 0)
          else M.get_int64 s 0)
    | [ "G"; "H"; "QH2" ] -> QH2_H_G (get_int8 s 0)
    | [ "ID"; "H"; "QH2" ] -> QH2_H_ID (M.get_int32 s 0)
    | [ "CSC"; "H"; "QH2" ] -> QH2_H_CSC (M.get_int16 s 0)
    | [ "PART"; "H"; "QH2" ] -> QH2_H_PART (M.get_int32 s 0)
    
    | [ "QHT" ] ->
        if get_int8 s 0 = 1 then
          let p = {
              QrtPatch.seq_no = get_int8 s 1; 
              QrtPatch.seq_size = get_int8 s 2; 
              QrtPatch.compressor = get_int8 s 3; 
              QrtPatch.entry_bits = get_int8 s 4; 
              QrtPatch.table = String.sub s 5 (String.length s - 5);
            } in 
          if p.QrtPatch.compressor = 1 then
            (try
                lprintf "Decompressing\n";
                let s = Autoconf.zlib__uncompress_string2 p.QrtPatch.table in
                lprintf "Size %d: %s\n" (String.length s)
                (String.escaped (String.sub s 0 40));
              with e ->
                  lprintf "Exception in uncompress %s\n"
                    (Printexc2.to_string e)
                  
            );
          QHT_PATCH p
        else begin
(*            lprintf "RESET %d\n" (M.get_int s 1);
            dump s; *)
            QHT_RESET
          end
    | [ "UPROC" ] -> UPROC
    | [ "UPROD" ] -> UPROD
    | [ "XML"; "UPROD" ] -> 
        let xml, pos = M.get_string s 0 (String.length s) in
        UPROD_XML xml
    | _ -> raise Not_found
  with e ->
      lprintf "Cannot parse: %s\n   " (Printexc2.to_string e);
      List.iter (fun name -> lprintf "%s/" name) names;
      lprintf "\n%s\n" (sdump s);
      Unknown (names, be, s)
  
let rec g2_parse name has_children bigendian s = 
  (*
  lprintf "g2_parse:"; List.iter (fun s -> lprintf "/%s" s) name;
  dump s;
*)  
  
  let len = String.length s in
  let rec iter_child pos children = 
    if pos >= len then children, len
    else
    let cb = get_int8 s pos in
    if cb = 0 then children, (pos+1) else
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
    g2_children = List.rev children;
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
  
(*  lprintf "One gnutella2 subpacket received\n";*)
  let name = String.sub s (pos + pkt_pos) name_len in
  let packet = String.sub s (pos + pkt_pos + name_len) pkt_len in
  let has_children = cb land 4 <> 0 in
  g2_parse (name :: root_name) has_children be packet, pos + msg_len

let socket_send sock p =
  let m = g2_encode "" p in
  (*
  lprintf "DUMP SENT: \n";
  dump m; *)
  write_string sock m
  
  
  
let udp_counter = ref 0
let udp_header () = 
  incr udp_counter;
  Printf.sprintf "GND\002%c%c\001\001" 
    (char_of_int (!udp_counter land 0xff))
    (char_of_int ((!udp_counter lsr 8) land 0xff))
  
let udp_send ip port msg =
  match !udp_sock with
    None -> ()
  | Some sock ->
      try
        let s = g2_encode (udp_header ()) msg in
        let len = String.length s in
        if !verbose_udp then begin
            lprintf "Sending on UDP(%d) to %s:%d:\n%s\n%s\n"
              !udp_counter
              (Ip.to_string ip) port
              (Print.print msg)
            (String.escaped s);
          end;
        UdpSocket.write sock s ip port;
(*        UdpSocket.write sock s Ip.localhost !!client_port *)
      with e ->
          lprintf "Exception %s in udp_send" (Printexc2.to_string e);
          lprint_newline () 
 
let udp_send_ack ip port counter =
  match !udp_sock with
    None -> ()
  | Some sock ->
      try
        let s =   Printf.sprintf "GND\000%c%c\001\000" 
          (char_of_int (counter land 0xff))
          (char_of_int ((counter lsr 8) land 0xff))
        in
        let len = String.length s in
        lprintf "ack sent\n";
        UdpSocket.write sock s ip port
      with e ->
          lprintf "Exception %s in udp_send" (Printexc2.to_string e);
          lprint_newline () 

          
let server_send sock s p = 
  let h = s.server_host in
  let ip = h.host_ip in
  let port = h.host_port in
  match sock with
  | Connection sock ->
      lprintf "Sending on TCP to %s:%d: \n%s\n" 
        (Ip.to_string ip) port (Print.print p);
      socket_send sock p
  | _ -> 
      udp_send ip port p

let packet p list = { g2_children = list; g2_payload = p }

let g2_handler f gconn sock  =
  let b = TcpBufferedSocket.buf sock in
(*
  lprintf "GNUTELLA2 HANDLER\n";
AnyEndian.dump (String.sub b.buf b.pos b.len);
  *)
  try
    while b.len >= 2 do
      let s = b.buf in
(*      lprintf "g2_tcp_packet_handler\n"; *)
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
      
(*      lprintf "One gnutella2 packet received\n"; *)
      let name = String.sub b.buf (b.pos + pos) name_len in
      let packet = String.sub b.buf (b.pos + pos + name_len) len in
      let has_children = cb land 4 <> 0 in
      TcpBufferedSocket.buf_used sock msg_len;
      f gconn (g2_parse [name] has_children be packet)
    done
  with 
  | Not_found -> ()

exception AckPacket

let udp_fragmented_packets = Fifo.create ()
  
let parse_udp_packet ip port buf =
  
  lprintf "\n\nNEW UDP PACKET   \n%s\n" (String.escaped buf);
  let len = String.length buf in
  
  if len < 8 then 
    failwith "Not a GNUTELLA2 packet (too short)\n"
  else
  if String.sub buf 0 3 <> "GND" then
    failwith "Not a GNUTELLA2 packet (no GND)\n"
  else
  let nSequence = LittleEndian.get_int16 buf 4 in
  let nCount = get_int8 buf 7 in
  let nFlags = get_int8 buf 3 in
  let ack_me = nFlags land 2 <> 0 in
(* Contribute:
  - deflating
  - multi-parts message
  - acknowledgement
*)
  if nFlags land 2 <> 0 then begin
      udp_send_ack ip port nSequence;
      lprintf "Need ack\n";
    end;
  
  if nCount = 0 then begin
      lprintf "ACK PACKET (%d)\n" nSequence;
      raise AckPacket
    end
  else
  
  let buf = 
    if nCount > 1 then begin
        let nPart = get_int8 buf 6 in
        let needed = Array.create nCount None in
        Fifo.iter (fun (p_ip,p_port,nSeq,nPart, nFlags, buf) ->
            if p_port = port && nSeq = nSequence
              && p_ip = ip then needed.(nPart) <- Some buf
        ) udp_fragmented_packets;
        if needed.(nPart) <> None then
          failwith "Fragmented packet already present\n";
        needed.(nPart) <- Some buf;
        let b = Buffer.create 100 in
        for i = 0 to nCount - 1 do
          match needed.(nPart) with
            None ->
              Fifo.put udp_fragmented_packets
                (ip,port,nSequence, nPart, nFlags, buf);
              if Fifo.length udp_fragmented_packets > 50 then
                ignore (Fifo.take udp_fragmented_packets);
              failwith "Fragmented packet part not present\n";
          | Some buf ->
              
              Buffer.add_string b (if i = 0 then buf else
                  String.sub buf 8 (String.length buf - 8))
        done;
(* All needed packets are present *)
        lprintf "Fragmented packet built";
        Buffer.contents b

        
        
      end else buf
  in
  try
    let buf,pos = if nFlags land 1 <> 0 then
        let trailer = String.sub buf 8 (len - 8) in
        lprintf "Uncompress\n";
        Autoconf.zlib__uncompress_string2 trailer, 0
      else buf, 8
    in
    let len = String.length buf in
    try
      let cb = get_int8 buf pos in
      let len_len = (cb lsr 6) land 3 in
      let be = cb land 2 <> 0 in
      
      let pkt_len, pkt_pos = match len_len, be with
        | 1, true -> get_int8 buf (pos+1), 2 
        | 2, true -> BigEndian.get_int16 buf (pos+1), 3 
        | 3, true -> BigEndian.get_int24 buf (pos+1), 4
        | 1, false -> get_int8 buf (pos+1),  2 
        | 2, false -> LittleEndian.get_int16 buf (pos+1), 3 
        | 3, false -> LittleEndian.get_int24 buf (pos+1), 4
        | _ ->
            lprintf "no correct pkt_len, pkt_pos\n";
            0, 1
      in
      let name_len = ((cb lsr 3) land 7) + 1 in
      let msg_len = 1 + len_len + name_len + pkt_len in
      if len > pos + msg_len then 
        (lprintf "STRANGE: %d > %d + %d (1 + %d + %d + %d)\n" len pos msg_len
            len_len name_len pkt_len
          ;
        );
      let msg_len = 
        if len < pos + msg_len then 
          (lprintf "RECOVER: %d < %d + %d (1 + %d + %d + %d)\n" len pos msg_len
              len_len name_len pkt_len
            ;
            len - pos)
        else msg_len in

(*  lprintf "One gnutella2 packet received\n"; *)
      let name = String.sub buf (pkt_pos + pos) name_len in
      let packet = String.sub buf (pkt_pos + pos + name_len) pkt_len in
      let has_children = cb land 4 <> 0 in
      let p = g2_parse [name] has_children be packet in
      lprintf "PACKET: %s\n" (Print.print p);
      p      
    
    with
    | AckPacket as e -> raise e
    | e ->
        lprintf "EXCEPTION AFTER DECOMP: %s\n" (Printexc2.to_string e);
        dump buf;
        raise e

  with 
  | AckPacket as e -> raise e
  | e ->
      lprintf "EXCEPTION: %s\n" (Printexc2.to_string e);
      dump buf;
      raise e

      
      (*
  let len = String.length buf in
  let nSequence = get_int16 buf 4 in
  let nCount = get_int8 buf 7 in
  let nFlags = get_int8 buf 3 in
  let ack_me = nFlags land 2 <> 0 in
(* Contribute:
  - deflating
  - multi-parts message
  - acknowledgement
*)
  if nCount > 1 then
    lprintf "g2_udp_handler not implemented\n";
  let buf,pos = if nFlags land 1 <> 0 then
      let trailer = String.sub buf 8 (len - 8) in
      Autoconf.zlib__uncompress_string2 trailer, 0
    else buf, 8
  in
  if nFlags land 2 <> 0 then begin
      lprintf "need ack\n";
      udp_send_ack ip port nSequence;
    end;
  let cb = get_int8 buf pos in
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
  if len < pos + msg_len then raise Not_found;
  
(*  lprintf "One gnutella2 packet received\n"; *)
  let name = String.sub buf (pkt_pos + pos) name_len in
  let packet = String.sub buf (pkt_pos + pos + name_len) pkt_len in
  let has_children = cb land 4 <> 0 in
g2_parse [name] has_children be packet
  
*)
      
(*

let s = "\120\156\242\225\014\240\012\096\011\117\009\056\044\038\212\200\198\002\000\000\000\255\255\000";;
let s2 = String.make 50 '\000';;
let f = Z_SYNC_FLUSH;;
let len = String.length s;;
let z = inflate_init true;;
let pos = 0;;
inflate z s pos (len-pos) s2 0 50 f;;
s2;;

let z = inflate_init true;;
let buf = ref ""
let pos = ref 0
  
let decompress s = 
  let s2 = String.make 100000 '\000' in
  let f = Z_SYNC_FLUSH in
  let len = String.length s in
  buf := !buf ^ s;;
  let (_,used_in, used_out) = inflate z !buf !pos len s2 0 100000 f in
  pos := !pos + len;
  
  Printf.printf "\n----------NEW PAQUET (%d/%d)----------------------------------------\n" used_in len;  
  for i = 0 to used_out - 1 do
    let c = s2.[i] in
    let x = int_of_char c in
    if x > 31 && x < 127 then 
      print_char c
    else
      Printf.printf "(%d)" x;
  done;
  Printf.printf "\n------------------------------------------------------------\n";  
  for i = 0 to used_out - 1 do
    let c = s2.[i] in
    let x = int_of_char c in
    Printf.printf "(%d)" x;
  done;
  Printf.printf "\n------------------------------------------------------------\n";
*)

(*
let parse s s_pos =
  let cb = get_int8 s s_pos in
  let len_len = (cb lsr 6) land 3 in
  if String.length s - s_pos < 1 + len_len then raise Not_found;
  let be = cb land 2 <> 0 in
  
  let len, pos = match len_len, be with
    | 1, true -> get_int8 s (s_pos+1), 2 
    | 2, true -> BigEndian.get_int16 s (s_pos+1), 3 
    | 3, true -> BigEndian.get_int24 s (s_pos+1), 4
    | 1, false -> get_int8 s (s_pos+1), 2 
    | 2, false -> LittleEndian.get_int16 s (s_pos+1), 3 
    | 3, false -> LittleEndian.get_int24 s (s_pos+1), 4
    | _ -> 0, 1
  in
  let name_len = ((cb lsr 3) land 7) + 1 in
  let msg_len = 1 + len_len + name_len + len in
  if String.length s - s_pos < msg_len then raise Not_found;

(*  lprintf "One gnutella2 packet received\n"; *)
  let name = String.sub s (s_pos + pos) name_len in
  let packet = String.sub s (s_pos + pos + name_len) len in
  let has_children = cb land 4 <> 0 in

(*
  lprintf "PACKET: \n";
  dump (String.sub s s_pos msg_len);
  lprintf "\n";
*)  
  let p = g2_parse [name] has_children be packet in
  
  p, String.sub s s_pos msg_len, s_pos + msg_len

open Zlib
let _ = 
  for i = 1 to 8 do
    let file = Printf.sprintf "flow.%d" i in
    try
      try
        let s = File.to_string file in
        let z = inflate_init true in
        let s =  
          let s2 = String.make 100000 '\000' in
          let f = Z_SYNC_FLUSH in
          let len = String.length s in
          let (_,used_in, used_out) = inflate z s 0 len s2 0 100000 f in
          String.sub s2 0 used_out
        in
        lprintf "%s loaded:\n" file;
        let rec iter pos =
(*          lprintf "iter %d\n" pos; *)
          if pos < String.length s then
            let p, decoded, pos = parse s pos in

            lprintf "decoded:\n";
            dump decoded;

            (try
                let encoded = g2_encode "" p in
                (*
                lprintf "encoded:\n";
dump encoded;
  *)
                let pp, _, _ = parse encoded 0 in
                
                if encoded <> decoded then begin
                    lprintf "ENCODER / DECODER ERROR:\n";
                    lprintf "CORRECT:\n";
                    dump decoded;
                    lprintf "INCORRECT:\n";
                    dump encoded;
                    lprintf "______________________\n";
                  end;
              with e ->
                  lprintf "Exception %s in Encoding\n" 
(Printexc2.to_string e));
  
            lprintf "Packet: \n%s\n" (Print.print p);
            iter pos
        in
        iter 0
      with e ->
          lprintf "Exception %s in reading flow.%d\n"
            (Printexc2.to_string e) i
    with e ->
        lprintf "Exception %s in reading flow.%d\n"
          (Printexc2.to_string e) i
  done
;;
  (*
  Packet: 
UPROD
  XML '<?xml version="1.0"?><gProfile xmlns="http://www.shareaza.com/schemas/GProfile.xsd"><gnutella guid="0BD7E4AC-1DD6-221B-6D60-D9FB555B097A"/><identity><handle pri
mary="fab"/></identity></gProfile>'
*)
*)  
  
        
let server_send_qrt_reset s = 
  server_send s.server_sock s (packet QHT_RESET [])
    
let server_send_qrt_patch s m = 
  server_send s.server_sock s (packet (QHT_PATCH m) [])
    
let server_ask_query sock s uid words xml_query = 
  lprintf "*********8 server_ask_query *********\n";
  
  let xml_query = 
    "<?xml version=\"1.0\"?><audios xsi:noNamespaceSchemaLocation=\"http://www.limewire.com/schemas/audio.xsd\"><audio/></audios>"
  in
  let args = 
        [
      packet (Q2_DN words) []; 
      packet (Q2_MD xml_query) []; 
      packet (Q2_I ["URL"; "DN"; "MD"; "COM"; "PFS"]) [];
    ]
  in
  let args = match sock, s.server_query_key with
      _, NoUdpSupport -> args
    | _  ->
        (packet (Q2_UDP ((client_ip sock, !!client_port), 
              (match s.server_query_key with
                | UdpQueryKey k -> Some k
                | _ -> Some Int32.zero
                    ))) []) :: args
  in
  server_send sock s (packet (Q2 uid) args)
    
let server_ask_uid sock s uid words fuid = 
  server_send sock s (packet (Q2 uid)
    [
      packet (Q2_UDP ((client_ip sock, !!client_port), 
          (match s.server_query_key with
              UdpQueryKey k -> Some k 
            | _ -> None))) [];
      packet (Q2_DN words) []; 
      packet (Q2_URN fuid) []; 
      packet (Q2_I ["URL"; "DN"; "MD"; "COM"; "PFS"]) [];
    ]
  )
  
let server_send_ping sock s = 
  server_send sock s
    (packet PI [
      packet (PI_UDP (client_ip NoConnection, !!client_port))[]])
        
let server_send_push s uid uri = ()

    
let create_qrt_table words table_size =
  let table_length = 1 lsl (table_size-3) in
  lprintf "table_length %d\n" table_length;
  let array = Array.create table_length 0 in
  List.iter (fun w ->
      let pos = bloom_hash w table_size in
      let pos = Int64.to_int pos in
      lprintf "ADDING WORD %d\n" pos; 
      let index = pos / 8 in
      let bit = (1 lsl (pos land 7)) in
      array.(pos) <- array.(pos) lor bit;
  ) words;
  let string_size = table_length in
  let table = String.create  string_size in
  for i = 0 to string_size - 1 do
    table.[i] <- char_of_int array.(i)
  done;
  table
  
let cached_qrt_table = ref ""
  
let send_qrt_sequence s update_table =
  
  if update_table then cached_qrt_table := "";
  let table_size = 20 in
  let infinity = 7 in
  let table_length = 1 lsl table_size in
  server_send_qrt_reset s;
  
  if !cached_qrt_table = "" then 
    cached_qrt_table := create_qrt_table !all_shared_words table_size;
  let table = !cached_qrt_table in
  
  let compressor, table =
    if Autoconf.has_zlib then
      1, Autoconf.zlib__compress_string table
    else
      0, table 
  in
  
  server_send_qrt_patch s {
      QrtPatch.seq_no = 1;
      QrtPatch.seq_size = 1;
      QrtPatch.compressor = compressor;
      QrtPatch.entry_bits = 1;
      QrtPatch.table = table;
    }

(* let packets = Hashtbl.create 100 *)
  
let print_string s buf =  
  lprintf "\n\nNEW UDP PACKET   %s" s;
  let len = String.length buf in
  
  if len < 8 then 
    lprintf "Not a GNUTELLA2 packet (too short)\n"
  else
  if String.sub buf 0 3 <> "GND" then
    lprintf "Not a GNUTELLA2 packet (no GND)\n"
  else
  let nSequence = LittleEndian.get_int16 buf 4 in
  let nCount = get_int8 buf 7 in
  let nFlags = get_int8 buf 3 in
  let ack_me = nFlags land 2 <> 0 in
(* Contribute:
  - deflating
  - multi-parts message
  - acknowledgement
*)
  
  if nCount > 1 then begin
    lprintf "Fragmented packet\n";
      
  end else
  if nCount = 0 then
    lprintf "ACK PACKET\n"
  else
  try
    let buf,pos = if nFlags land 1 <> 0 then
        let trailer = String.sub buf 8 (len - 8) in
        lprintf "Uncompress\n";
        Autoconf.zlib__uncompress_string2 trailer, 0
      else buf, 8
    in
    try
      if nFlags land 2 <> 0 then
        lprintf "Need ack\n";
      let cb = get_int8 buf pos in
      let len_len = (cb lsr 6) land 3 in
      let be = cb land 2 <> 0 in
      
      let pkt_len, pkt_pos = match len_len, be with
        | 1, true -> get_int8 buf (pos+1), 2 
        | 2, true -> BigEndian.get_int16 buf (pos+1), 3 
        | 3, true -> BigEndian.get_int24 buf (pos+1), 4
        | 1, false -> get_int8 buf (pos+1),  2 
        | 2, false -> LittleEndian.get_int16 buf (pos+1), 3 
        | 3, false -> LittleEndian.get_int24 buf (pos+1), 4
        | _ ->
            lprintf "no correct pkt_len, pkt_pos\n";
            0, 1
      in
      let name_len = ((cb lsr 3) land 7) + 1 in
      let msg_len = 1 + len_len + name_len + pkt_len in
      if len > pos + msg_len then 
        (lprintf "STRANGE: %d > %d + %d (1 + %d + %d + %d)\n" len pos msg_len
            len_len name_len pkt_len
          ;
        );
      let msg_len = 
        if len < pos + msg_len then 
          (lprintf "RECOVER: %d < %d + %d (1 + %d + %d + %d)\n" len pos msg_len
            len_len name_len pkt_len
            ;
            len - pos)
        else msg_len in

(*  lprintf "One gnutella2 packet received\n"; *)
      let name = String.sub buf (pkt_pos + pos) name_len in
      let packet = String.sub buf (pkt_pos + pos + name_len) pkt_len in
      let has_children = cb land 4 <> 0 in
      let p = g2_parse [name] has_children be packet in
      lprintf "PACKET: %s\n" (Print.print p);
      dump buf
      
  with e ->
      lprintf "EXCEPTION: %s\n" (Printexc2.to_string e);
      dump buf

  with e ->
      lprintf "EXCEPTION: %s\n" (Printexc2.to_string e);
      dump buf

let server_send_qkr s =
  server_send NoConnection s 
    (packet QKR
      [
      (packet (QKR_RNA (client_ip NoConnection, !!client_port)) [])
    ])

(* These functions could go in Gnutella when Guess support is enabled *)
  
let extend_ask_query uid words xml_query = 
  let send (h,_) =
    try
      match h.host_server with
        None -> ()
      | Some s ->
          match s.server_query_key with 
          | UdpQueryKey _ ->
              server_ask_query NoConnection s uid words xml_query
          | _ -> ()
    with _ -> ()
  in
  lprintf "From Recent queue:\n";
  Fifo.iter send ultrapeers_recent_queue;
  lprintf "From Old queue:\n";
  Fifo.iter send ultrapeers_old_queue;
  lprintf "From Waiting queue:\n";
  Fifo.iter send ultrapeers_waiting_queue;
  ()
  
let extend_ask_uid  uid words fuid = ()
  