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

let calcDecodedLength psrc srclen =
  let rec iter codepos len =
    if codepos < srclen then
      let code = psrc.[codepos] in
      let code = int_of_char code in
(*      lprintf "code : %d\n" code; *)
      iter (codepos+code) (len+code-
        (if code = 255 then 1 else 0))
    else
      (len-1)
  in
  iter 0 0 	

let decodeData pdest psrc srclen =
  let rec iter srcpos dstpos =
     if srcpos<srclen then
        let code = int_of_char psrc.[srcpos] in
        let srcpos = srcpos + 1 in
        for i = 0 to code - 2 do
           pdest.[dstpos+i] <- psrc.[srcpos+i]
        done;
        let srcpos = srcpos + code - 1 in
        let dstpos = dstpos + code - 1 in
        iter srcpos (if code<255 && srcpos<srclen  then begin
           pdest.[dstpos] <- '\000';
           dstpos+1
        end else dstpos)
  in
  iter 0 0

let calcEncodedLength psrc srclen =
  let rec iter srcpos len code =
    if srcpos < srclen then
      let c = psrc.[srcpos] in
      let srcpos = srcpos + 1 in
      if c = '\000' then
        iter srcpos (len+code) 1
      else
      let code = code + 1 in
      if code = 255 then
        iter srcpos (len+code) 1
      else 
        iter srcpos len code
    else
      (len+code)
  in
  iter 0 0 1

let encodeData pdest psrc srclen =
  let rec iter dstpos srcpos codepos code =
    if srcpos < srclen then
      let c = psrc.[srcpos] in
      if c = '\000' then
        let srcpos = srcpos + 1 in
        pdest.[codepos] <- char_of_int code;
        let codepos = dstpos in
        let dstpos = dstpos + 1 in
        iter dstpos srcpos codepos 1
      else
      let code = code + 1 in
      pdest.[dstpos] <- c;
      let dstpos = dstpos+1 in
      let srcpos = srcpos+1 in
      if code = 255 then begin
          pdest.[codepos] <- char_of_int code;
          let codepos = dstpos in
          let dstpos = dstpos+1 in
          iter dstpos srcpos codepos 1
        end
      else
        iter dstpos srcpos codepos code
    else
      pdest.[codepos] <- char_of_int code;
  in
  iter 1 0 0 1

let decode psrc =
  let srclen = String.length psrc in 
  let dstlen = calcDecodedLength psrc srclen in
  let pdest = String.create dstlen in
  decodeData pdest psrc srclen;
  (Bytes.to_string pdest)
  
let encode psrc =
  let srclen = String.length psrc in 
  let dstlen = calcEncodedLength psrc srclen in
  let pdest = String.create dstlen in
  encodeData pdest psrc srclen;
  pdest

(*
ggep:

magic: 1 byte 195
flags: 1 byte 
  bit 7 : last extension
  bit 6 : encoding (1 = cobs)
  bit 5 : compression (1 = deflate)
  bit 4 : reserved
  bit 3-0: : id len (never 0)
  
id: 1-15 bytes
len: 1-3 bytes
  MLxxxxxxx
  M : not last one, L : last one
data: remaining
  
*)
open AnyEndian

module GGEP = struct
  
type ggep_block = 
  UnknownBlock of string
| GGEP of string * string

let rec get_len s pos len =
  let c = (int_of_char s.[pos]) land 255 in
  let again = c land (1 lsl 7) <> 0 in
  let len = (len lsl 6) lor (c land 0x3f) in
  if again then get_len s (pos+1) len 
  else (len, pos+1)
    
let rec iter_block s pos len list = 
  if pos = len then list
  else
  let flags = int_of_char s.[pos] in
(*  let last = (flags land (1 lsl 7)) <> 0 in *)
  let cobs_encoded = (flags land (1 lsl 6)) <> 0 in
(*  lprintf "cobs_encoded %b\n" cobs_encoded; *)
(*  let compressed = (flags land (1 lsl 5)) <> 0 in *)
(*  lprintf "compressed %b\n" compressed; *)
  let id_len = flags land 7 in
  let id = String.sub s (pos+1) id_len in
  let data_len, pos = get_len s (pos+1+id_len) 0 in
(*  lprintf "data_len %d,pos %d, len %d\n" data_len pos len; *)
  let data = String.sub s pos data_len in
  let data = 
    if cobs_encoded then decode data else data in
  iter_block s (pos+data_len) len ((GGEP (id,data)) :: list)
  
let parse_block s list =
  let len = String.length s in
  if len > 0 then
    try
(*      lprintf "Block len: %d\n" len; *)
      if s.[0] <> '\195' then raise Not_found;
      iter_block s 1 len list
    with e ->
        lprintf "Exception %s in parse_block\n" (Printexc2.to_string e);
        (UnknownBlock s) :: list
  else list
    
(*
 "\195\002DUB\232\030\003GUEA\001\002UPC\001\000\000\130VCELIME'"
    GGEP Block:
    DU: \232\030
    GUE: \001      
    UP: \001\000\000
    VC: LIME'

messages/GGEP.java:    GGEP_HEADER_BROWSE_HOST = "BH";
messages/GGEP.java:    GGEP_HEADER_UP_SUPPORT = "UP";        Ultrapeer byte[1]=version(guess), byte[1] =free leaf slots, byte[1] = free non leaf 
messages/GGEP.java:    GGEP_HEADER_QUERY_KEY_SUPPORT = "QK";
messages/GGEP.java:    GGEP_HEADER_MULTICAST_RESPONSE = "MCAST";

  
*)

      (*
let parse s =
  let len = String.length s in
  let rec iter s pos len list =
    if pos >= len then List.rev list else
    try
      let end_pos = String.index_from s pos '\000' in
      lprintf "First block\n";
      let block = String.sub s pos (end_pos-pos) in
      let list = parse_block block list in
      iter s (end_pos+1) len list
    with Not_found ->
        parse_block (String.sub s pos (len-pos)) list
  in
  iter s 0 len []
    *)

let parse s = List.rev (parse_block s [])
  
let print_block s = 
  lprintf "New block\n";
  let list = parse s in
  List.iter (fun b ->
      match b with
        UnknownBlock s -> 
          lprintf "Unknown Block: %s\n" (String.escaped s)
      | GGEP (id,data) ->
          lprintf "GGEP Block:\n";
          lprintf "    %s: %s\n" id (String.escaped data)
  ) list;
  list

  (*
let _ =
  List.iter print_block    
    [
    "\195\002DUA\023\130VCELIME'";
    "\195\002DUB\232\030\003GUEA\001\002UPC\001\000\000\130VCELIME'";
  ]  
    
let _ =
  let s = "1234\0005677840438903985\123\1560\000\200" in
  let slen = String.length s in
  lprintf "calcEncodedLength...\n";
  let blen = calcEncodedLength s slen in
  lprintf "encoded: %d/%d\n" blen slen;
  let buf = String.create blen in
  lprintf "encodeData...\n";
  encodeData buf s slen;
  lprintf "calcDecodedLength...\n";
  let olen = calcDecodedLength buf blen in
  assert (olen = slen);
  let os = String.create olen in
  lprintf "decodeData...\n";
  decodeData os buf blen;
  assert (os = s)

  *)
  
let rec put_len buf last_byte len =
  if len > 63 then put_len buf false (len lsr 6);
  let len = len land 0x3f in
  let len = len lor (if last_byte then 0x40 else 0x80) in
  buf_int8 buf len    
  
let write_ggep buf put_magic last_block b =
  match b with
  | UnknownBlock s ->
      Buffer.add_string buf s
  | GGEP (id, data) ->
      let id_len = String.length id in
      if put_magic then Buffer.add_char buf '\195';
      let cobs_encoded = String.contains data '\000' in
      let data = if cobs_encoded then encode data else (Bytes.of_string data) in
      let flags = id_len in
      let flags  = if cobs_encoded then flags lor (1 lsl 6) else flags in
      let flags  = if last_block then flags lor (1 lsl 7) else flags in
      buf_int8 buf flags;
      Buffer.add_string buf id;
      let data_len = Bytes.length data in
      put_len buf true data_len;
      Buffer.add_bytes buf data
      
let write_block buf list =
  let rec iter put_magic list =
    match list with
      [] -> ()
    | [b] -> write_ggep buf put_magic true b
    | b :: tail ->
        write_ggep buf put_magic false b;
        iter (match b with GGEP _ -> false | _ -> true) tail
  in
  iter true list

  (*
let _ =
  List.iter (fun s ->
      let list = print_block s in
      let b = Buffer.create 100 in
      write_block b list;
      let ss = Buffer.contents b in
      lprintf "Encoding: \n%s\n%s\n" (String.escaped s) (String.escaped ss);
      ignore (print_block ss)
  )
  
  [
    "\195\002DUA\023\130VCELIME'";
    "\195\002DUB\232\030\003GUEA\001\002UPC\001\000\000\130VCELIME'";
  ]  
    *)

end

type ggep =
  GGEP_block of string * string
| GGEP_unknown of string
| GGEP_GUE_guess of int 
| GGEP_VC_vendor of string * int * int
| GGEP_DU_uptime of int
  
let parse s =
  List.map (fun x ->
      try
        match x with 
          GGEP.UnknownBlock s -> GGEP_unknown s
        | GGEP.GGEP ("GUE", s) -> GGEP_GUE_guess (get_uint8 s 0)
        | GGEP.GGEP ("VC", s) ->
            let vendor = String.sub s 0 4 in
            let version = get_uint8 s 4 in
            let version1 = (version lsr 4) land 0xf in
            let version2 = version land 0xf in
            GGEP_VC_vendor (vendor, version1, version2)
        | GGEP.GGEP ("DU", s) ->
            let du = match String.length s with
                0 -> 0
              | 1 -> get_uint8 s 0 
              | 2 -> LittleEndian.get_int16 s 0 
              | 3 -> LittleEndian.get_int24 s 0 
              | _ -> LittleEndian.get_int s 0 in
            GGEP_DU_uptime du
        | GGEP.GGEP (id,data) -> GGEP_block (id,data)
      with e -> 
(*          lprintf "Exception %s in GGEP parse\n" (Printexc2.to_string e); *)
          GGEP_unknown "Bad GGEP Block"
  ) (GGEP.parse s)
  
let write buf list =
  let list = 
    List.map (fun x ->
        match x with
          GGEP_unknown s -> GGEP.UnknownBlock s
        | GGEP_block (id,data) -> GGEP.GGEP (id,data)
        | GGEP_GUE_guess version -> 
            GGEP.GGEP ("GUE", String.make 1 (char_of_int version))
        | GGEP_VC_vendor (vendor, version1, version2) ->
            let s = Printf.sprintf "MLDK%c" 
                (char_of_int ((version1 lsl 4) lor version2)) in
            GGEP.GGEP ("VC", s)
        | GGEP_DU_uptime up ->
            let s = 
              if up land 0xff = up then
                String.make 1 (char_of_int up)
              else
              if up land 0xffff = up then
                let s = String.create 2 in
                LittleEndian.str_int16 s 0 up;
                (Bytes.to_string s)
              else if up land 0xffffff = up then
                let s = String.create 3 in
                LittleEndian.str_int24 s 0 up;
                (Bytes.to_string s)
              else
              let s = String.create 4 in
              LittleEndian.str_int s 0 up;
              (Bytes.to_string s)
            in
            GGEP.GGEP ("DU", s)
    ) list
  in
  GGEP.write_block buf list
  
let print list =
  lprintf "GGEP block:\n";
  List.iter (fun x -> 
      match x with
        GGEP_block (id, data) ->
          lprintf "   Block %s : %s\n" id (String.escaped data)
      | GGEP_unknown s ->
          lprintf "   Unknown : %s\n" (String.escaped s)
      | GGEP_GUE_guess v ->
          lprintf "   Guess version %d\n" v
      | GGEP_VC_vendor (vendor, v1, v2) ->
          lprintf "   Vendor %s %d.%d\n" vendor v1 v2
      | GGEP_DU_uptime up ->
          lprintf "   Uptime %d seconds\n" up
  ) list
  
