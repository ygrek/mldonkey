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
  let module M = G2_LittleEndian in
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
    let module M = G2_LittleEndian in
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

(*
2) Implement connection to a gnutella2 ultra-peer (send PI and LNI, receive KHL)
3) Support PUSH request
4) Query Key Negociation
5) Query and QueryHits
  
  
Connection to a server: 
  Send PI
  Send LNI


  
*)

      
let server_send_qrt_reset s m = ()
    
let server_send_qrt_patch s m = ()
    
let server_send_query s uid words xml_query = ()
    
let server_send_ping s = ()
  
let server_send_push s uid uri = ()
  
