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

open CommonTypes
open BigEndian
open Int32ops
open TcpBufferedSocket
  
let ports = ref []

      
type tagged_file =  {
    f_md4: Md4.t;
    f_ip: Ip.t;
    f_port: int;
    f_tags: tag list;
  }
  
let rec print_tags tags =
  match tags with
    [] -> ()
  | tag :: tags ->
      Printf.printf "  \"%s\" = " (String.escaped tag.tag_name);
      begin
        match tag.tag_value with
        | Uint32 n -> Printf.printf "%s" (Int32.to_string n)
        | Fint32 n -> Printf.printf "%s" (Int32.to_string n)
        | Addr ip -> Printf.printf "%s" (Ip.to_string ip)
        | String s -> Printf.printf "\"%s\"" 
              (String.escaped s)
      end;
      print_tags tags

let rec fprint_tags oc tags =
  match tags with
    [] -> Printf.fprintf oc "\n";
          ()
  | tag :: tags ->
      Printf.fprintf oc "%s = " (String.escaped tag.tag_name);
      begin
        match tag.tag_value with
        | Uint32 n -> Printf.fprintf oc "%s" (Int32.to_string n)
        | Fint32 n -> Printf.fprintf oc "%s" (Int32.to_string n)
        | Addr ip -> Printf.fprintf oc "%s" (Ip.to_string ip)
        | String s -> Printf.fprintf oc "\"%s\"" 
              (String.escaped s)
      end;
      fprint_tags oc tags

let rec bprint_tags buf tags =
  match tags with
    [] -> Printf.bprintf buf "\n";
          ()
  | tag :: tags ->
      Printf.bprintf buf "%s = " (String.escaped tag.tag_name);
      begin
        match tag.tag_value with
        | Uint32 n -> Printf.bprintf buf "%s" (Int32.to_string n)
        | Fint32 n -> Printf.bprintf buf "%s" (Int32.to_string n)
        | Addr ip -> Printf.bprintf buf "%s" (Ip.to_string ip)
        | String s -> Printf.bprintf buf "\"%s\"" 
              (String.escaped s)
      end;
      bprint_tags buf tags
      
let const_int32_255 = Int32.of_int 255
let output_int32_8 oc i =
  output_char oc (char_of_int (Int32.to_int (
        Int32.logand i const_int32_255)))

let output_int32_32 oc i =
  output_int32_8 oc i;
  output_int32_8 oc (right32 i  8);
  output_int32_8 oc (right32 i  16);
  output_int32_8 oc (right32 i  24)

let output_int8 oc i =
  output_char oc (char_of_int (i land 255))
  
let output_int oc i =
  output_int8 oc i;
  output_int8 oc (i lsr 8);
  output_int8 oc (i lsr 16);
  output_int8 oc (i lsr 24)
        
(*  
let buf_int buf int =
  buf_int32_32 buf (Int32.of_int int)
  *)

let rec rev_assoc x = function
    [] -> raise Not_found
  | (b,a)::l -> if a = x then b else rev_assoc x l

let buf_string buf s =
  buf_int16 buf (String.length s);
  Buffer.add_string buf s

let buf_port buf port =
  buf_int16 buf port
        

let buf_peer buf (ip,port) =
  buf_ip buf ip;
  buf_port buf port
  
let buf_list buf_item b list =
  let len = List.length list in
  buf_int b len;
  List.iter (buf_item b) list
  
let rec buf_tags buf tags names_of_tag =
  match tags with
    [] -> ()
  | tag :: tags ->
      let name = tag.tag_name in
      let name = try
          let i = rev_assoc name names_of_tag in
          String.make 1 (char_of_int i)
        with _ -> name in
      begin
        match tag.tag_value with
        | Uint32 n -> 
            buf_int8 buf 3;
            buf_string buf name;
            buf_int32_32 buf n
        | Fint32 n -> 
            buf_int8 buf 4;
            buf_string buf name;
            buf_int32_32 buf n
        | Addr ip -> assert false
        | String s -> 
            buf_int8 buf 2;
            buf_string buf name;
            buf_string buf s

      end;
      buf_tags buf tags names_of_tag
  
let read_uint8 ic =
  Int32.of_int (int_of_char (input_char ic))
  
let read_uint32 ic =
  let a0 = read_uint8 ic in
  let a1 = read_uint8 ic in
  let a2 = read_uint8 ic in
  let a3 = read_uint8 ic in
  a0 +. (left32 a1  8) +. (left32 a2 16) +. (left32 a3 24) 

let read_request ic =
  let c = int_of_char (input_char ic) in
  assert (c = 227);
  let len32 = read_uint32 ic in
  let len = Int32.to_int len32 in
  let s = String.create len in
  really_input ic s 0 len;
  (*
  Printf.printf "read_request %d [%s]" len (String.escaped s); 
print_newline ();
  *)
  s

let output_request oc s =
  output_char oc (char_of_int 227);
  let len = String.length s in
  (*
  Printf.printf "output_request %d [%s]" len (String.escaped s); 
print_newline ();
  *)
  output_int oc len;
  output_string oc s

  
let translate_port port =
  try
    List.assoc port !ports
  with _ -> port
  
let get_port s pos =
  let port = get_int16 s pos in
  translate_port port

let get_string s pos =
  let len = get_int16 s pos in
  String.sub s (pos+2) len, pos+2+len


let rec get_tags s pos ntags names_of_tag =
  if ntags = 0 then [], pos else
  let t = get_int8 s pos in
  let name, pos2 = get_string s (pos+1) in
(*  Printf.printf "tag name = %s" (String.escaped name);   *)
  let name = if String.length name = 1 then
      try
        List.assoc (get_int8 name 0) names_of_tag
      with _ -> name 
    else name in
  let v, pos = match t with
    | 2 -> let v, pos = get_string s pos2 in
        String v, pos
    | 1|3 -> let v = get_int32_32 s pos2 in
        Uint32 
          (if name = "port" then
            let port = Int32.to_int v in
            Int32.of_int (translate_port port)
          else v
        ), pos2+4
    | 4 -> let v = get_int32_32 s pos2 in
        Fint32 
          (if name = "port" then
            let port = Int32.to_int v in
            Int32.of_int (translate_port port)
          else v
        ), pos2+4
    | _ -> 
        Printf.printf "get_tags: unknown tag %d at pos %d" t pos;
        print_newline ();
        raise Not_found
  in
  let tag = {
      tag_name = name;
      tag_value = v
    } in
  let tags, pos = get_tags s pos (ntags-1) names_of_tag in
  (tag :: tags), pos


let rec get_list_rec get_item s pos len left =
  if len = 0 then List.rev left, pos else
  let (item,pos) = get_item s pos in
  get_list_rec get_item s pos (len-1) (item :: left)
  
let get_list get_item s pos =
  let len = get_int s pos in
  get_list_rec get_item s (pos+4) len []

let get_peer s pos =
  let ip = get_ip s pos in
  let port = get_port s (pos+4) in
  (ip,port), pos+6
    
module type Request = sig
    type t
    val parse: int -> string -> t
    val print: t -> unit
    val write: Buffer.t -> t -> unit
  end
  
let rec find_tag v tags =
  match tags with
    [] -> raise Not_found
  | { tag_name = tag_name; tag_value = tag_value } :: _ 
    when tag_name = v -> tag_value
  | _ :: tags -> find_tag v tags





