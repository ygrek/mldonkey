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
open Int32ops
open TcpClientSocket
  
let ports = ref []


type tag_value =
| Uint32 of int32
| Fint32 of int32
| String of string
| Addr of Ip.t
  
type tag = {
    mutable tag_name : string;
    mutable tag_value : tag_value;
  }
      
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

      
      


      
      
let const_int32_255 = Int32.of_int 255
let output_int32_8 oc i =
  output_char oc (char_of_int (Int32.to_int (
        Int32.logand i const_int32_255)))

let output_int32_32 oc i =
  output_int32_8 oc i;
  output_int32_8 oc (right32 i  8);
  output_int32_8 oc (right32 i  16);
  output_int32_8 oc (right32 i  24)
      
let buf_int32_8 buf i =
  Buffer.add_char buf (char_of_int (Int32.to_int (
        Int32.logand i const_int32_255)))

let buf_int8 buf i =
  Buffer.add_char buf (char_of_int i)

let buf_int32_32 oc i =
  buf_int32_8 oc i;
  buf_int32_8 oc (right32 i  8);
  buf_int32_8 oc (right32 i  16);
  buf_int32_8 oc (right32 i  24)
  
let buf_int16 buf i =
  Buffer.add_char buf (char_of_int (i mod 256));
  Buffer.add_char buf (char_of_int (i / 256))
  
let buf_int buf int =
  buf_int32_32 buf (Int32.of_int int)
  
let buf_ip buf ip =
  let (ip0,ip1,ip2,ip3) = Ip.to_ints ip in
  buf_int8 buf ip0;
  buf_int8 buf ip1;
  buf_int8 buf ip2;
  buf_int8 buf ip3

let rec rev_assoc x = function
    [] -> raise Not_found
  | (b,a)::l -> if a = x then b else rev_assoc x l

let buf_string buf s =
  buf_int16 buf (String.length s);
  Buffer.add_string buf s

let buf_port buf port =
  buf_int16 buf port

let buf_md4 buf s = Buffer.add_string buf (Md4.direct_to_string s)
        
  
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
  s

  
let get_md4 s pos =
  try Md4.direct_of_string (String.sub s pos 16)  with e ->
      Printf.printf "exception in get_md4 %d" pos; print_newline ();
      raise e

let get_ip s pos =
  let c1 = int_of_char s.[pos] in
  let c2 = int_of_char s.[pos+1] in
  let c3 = int_of_char s.[pos+2] in
  let c4 = int_of_char s.[pos+3] in
  Ip.of_ints (c1, c2, c3, c4)

let get_int8 s pos = 
  int_of_char s.[pos]

let get_int16 s pos =
  let c1 = int_of_char s.[pos] in
  let c2 = int_of_char s.[pos+1] in
  c1 + c2 * 256

let translate_port port =
  try
    List.assoc port !ports
  with _ -> port
  
let get_port s pos =
  let port = get_int16 s pos in
  translate_port port
      
let get_int32_8 s pos =
  Int32.of_int (int_of_char s.[pos])

let get_int32_32 s pos = 
  let c1 = get_int32_8 s pos in
  let c2 = get_int32_8 s (pos+1) in
  let c3 = get_int32_8 s (pos+2) in
  let c4 = get_int32_8 s (pos+3) in
  c1 +. (left32 c2 8) +. (left32 c3 16) +. (left32 c4 24)           

let get_int s pos =
  Int32.to_int (get_int32_32 s pos)

let get_string s pos =
  let len = get_int16 s pos in
  try
    String.sub s (pos+2) len, pos+2+len
  with e ->
      Printf.printf "exception in get_string %d(%d)" pos len; print_newline ();
      raise e


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


let dump s =
  let len = String.length s in
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

let dump_sub s pos len =
  Printf.printf "dec: [";
  for i = 0 to len - 1 do
    let c = s.[pos+i] in
    let n = int_of_char c in
    Printf.printf "(%d)" n            
  done;
  Printf.printf "]\n";
  print_newline ()
  
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



          
let internal_buf = Buffer.create 17000

let simple_send_buf buf sock =
  let s = Buffer.contents buf in
  Buffer.clear buf;
  buf_int8 buf 228;
  let len = String.length s in
  buf_int buf len;
  write sock (Buffer.contents buf) 0 5;
  write sock s 0 len

let value_send sock m =
  Buffer.clear internal_buf;
  Buffer.add_string internal_buf (Marshal.to_string m []);
  simple_send_buf internal_buf sock

let value_handler f sock nread =
  let b = buf sock in
  try
    while b.len >= 5 do
      let msg_len = get_int b.buf (b.pos+1) in
      if b.len >= 5 + msg_len then
        begin
          let s = String.sub b.buf (b.pos+5) msg_len in
          let t = Marshal.from_string  s 0 in
          buf_used sock  (msg_len + 5);
          f t sock;
          ()
        end
      else raise Not_found
    done
  with Not_found -> ()
