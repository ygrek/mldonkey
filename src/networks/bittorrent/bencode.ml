(* Copyright 2001, 2002 b52_simon :), b8_bavard, b8_fee_carabine, INRIA *)
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

(*
 {
 "announce" = "http://sucs.org:6969/announce";
 "info" =  {
 "files" =  [
  {
  "length" =  682164224;
  "path" =  [
  "Mandrake91-cd1-inst.i586.iso";
   ]
   ;
    }
    ;
     {
     "length" =  681279488;
     "path" =  [
     "Mandrake91-cd2-ext.i586.iso";
      ]
      ;
       }
       ;
        {
	"length" =  681574400;
	"path" =  [
	"Mandrake91-cd3-i18n.i586.iso";
	 ]
	 ;
	  }
	  ;
	   ]
	   ;
	   "name" = "mandrake9.1";
	   "piece length" =  262144;
	   "pieces" =  "[EAd\155ã´gÛ ÓþËf\134Ê«\025\016ôÍµ,1U\150À
\132\147îª\n%ù\\é,\012ÿC\008GÈÓd!æ¾öuL!\134Ô\016\152&\017¾\008³¢d\029Ë3\031Ï\134#»×\025\137¡=¢.®\019§´\138î.ñ\151O\137Ùÿ,£ç&\019ÀÛ¢Ã§\156.ù\150<Eªª\153\018\145\149d\147[+J=º\155l\139Î\028¡dVÉ\000-\017°Å¤\013\154¼>A¹Ã5ïIt\007\020©ãÚÀÈÈ\014O®ô1\152UÄ\026K\021^ãúì5Í¿ü \026\149\131q\024\015¸]Òþ£\027&\148\\ã-©\028WMÂ5...";
 }
 ;
  }

*)
type value =
  String of string
  | Int of int64
  | List of value list
  | Dictionary of (value * value) list

let decode s =
  let len = String.length s in
  let rec decode s pos len =
    if pos >= len then assert false;
    match s.[pos] with
     '0'..'9' ->
        let rec iter_i s pos len =
           if pos = len then pos else
           match s.[pos] with
             '0' .. '9' -> iter_i s (pos+1) len
            | ':' -> pos
	    | _ -> assert false
	in
	let end_pos = iter_i s (pos+1) len in
	let size = int_of_string (String.sub s pos (end_pos-pos)) in
	String (String.sub s (end_pos+1) size), (end_pos+1+size)
   | 'i' ->
        let rec iter_i s pos len =
           if pos = len then assert false;
           match s.[pos] with
            | 'e' -> pos
	    | _ -> iter_i s (pos+1) len
	in
	let end_pos = iter_i s (pos+1) len in
	(Int (Int64.of_string (String.sub s (pos+1) (end_pos-pos-1)))),
	  (end_pos+1)
   | 'l' ->
        let rec iter s pos len list =
	  if pos = len then assert false;
          match s.[pos] with
            | 'e' -> List (List.rev list), (pos+1)
	    | _ ->
	       let v, pos = decode s pos len in
	       iter s pos len (v :: list)
	in
	iter s (pos+1) len []
   | 'd' ->

        let rec iter s pos len list =
	  if pos = len then assert false;
          match s.[pos] with
            | 'e' -> Dictionary (List.rev list), (pos+1)
	    | _ ->
	       let key, pos = decode s pos len in
	       let v, pos = decode s pos len in
	       iter s pos len ((key,v) :: list)
	in
	iter s (pos+1) len []
   | _ -> assert false
  in
  let (v,pos) = decode s 0 len in
  v

let encode v =
  let buf = Buffer.create 100 in
  let rec encode v =
    match v with
    | String s -> Printf.bprintf buf "%d:%s" (String.length s) s
    | Int i -> Printf.bprintf buf "i%Lde" i
    | List list ->
       Buffer.add_char buf 'l';
       List.iter encode list;
       Buffer.add_char buf 'e'
    | Dictionary list ->
       Buffer.add_char buf 'd';
       List.iter (fun (key,v) -> encode key; encode v) list;
       Buffer.add_char buf 'e'
  in
  encode v;
  Buffer.contents buf

let print b =
  let buf = Buffer.create 100 in
  let rec print v =
    match v with
    | String s ->
       if String.length s > 200 then
          Printf.bprintf buf " \"%s...\"" (String.escaped (String.sub s 0 200))
       else
          Printf.bprintf buf "\"%s\"" (String.escaped s)
    | Int i -> Printf.bprintf buf " %Ld" i
    | List list ->
       Printf.bprintf buf " [\n";
       List.iter (fun v -> print v; Printf.bprintf buf ";\n")  list;
       Printf.bprintf buf " ]\n";
    | Dictionary list ->
       Printf.bprintf buf " {\n";
       List.iter (fun (key,v) -> print key; Buffer.add_string buf " = "; print v; Printf.bprintf buf ";\n")  list;
       Printf.bprintf buf " }\n";
  in
  print b;
  Buffer.contents buf
