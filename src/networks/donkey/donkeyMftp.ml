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
open CommonTypes
open LittleEndian
open Int32ops
open TcpBufferedSocket

      
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
        

let buf_addr buf (ip,port) =
  buf_ip buf ip;
  buf_port buf port
  
let rec buf_tags buf tags names_of_tag =
  buf_int buf (List.length tags);
  let rec iter_tags tags =
    match tags with
      [] -> ()
    | tag :: tags ->
        let name = try rev_assoc tag.tag_name names_of_tag 
            with _ -> tag.tag_name
        in
(* try
            let i = rev_assoc name names_of_tag in
            String.make 1 (char_of_int i)
          with _ -> name *) 
        begin
          match tag.tag_value with
          | Uint64 n -> 
              buf_int8 buf 3;
              buf_string buf name;
              buf_int64_32 buf n
          | Fint64 n -> 
              buf_int8 buf 4;
              buf_string buf name;
              buf_int64_32 buf n
          | Addr ip -> assert false
          | String s -> 
              buf_int8 buf 2;
              buf_string buf name;
              buf_string buf s
        
        end;
        iter_tags tags
  in
  iter_tags tags
  
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
  lprintf "read_request %d [%s]" len (String.escaped s); 
lprint_newline ();
  *)
  s

let output_request oc s =
  output_char oc (char_of_int 227);
  let len = String.length s in
  (*
  lprintf "output_request %d [%s]" len (String.escaped s); 
lprint_newline ();
  *)
  output_int oc len;
  output_string oc s
  
let get_port s pos =
  get_int16 s pos

let get_string = get_string16
  
let get_tag s pos names_of_tag =
  let t = get_int8 s pos in
  let name, pos2 = get_string s (pos+1) in
(*  lprintf "tag name = %s" (String.escaped name);   *)
  let v, pos = match t with
    | 2 -> let v, pos = get_string s pos2 in
        String v, pos
    | 1|3 -> let v = get_int64_32 s pos2 in
        Uint64 v, pos2+4
    | 4 -> let v = get_int64_32 s pos2 in
        Fint64 v, pos2+4
    | _ -> 
        lprintf "get_tags: unknown tag %d at pos %d\n" t pos;
        raise Not_found
  in
  {
    tag_name = (try
        List.assoc name names_of_tag
      with Not_found ->
(*          lprintf "Unknown tag \"%s\"\n" (String.escaped name); *)
          name);
    tag_value = v
  }, pos
  
let get_tags s pos names_of_tag =
  let rec iter_tags ntags pos tags =
    if ntags = 0 then List.rev tags, pos else
    let tag, pos = get_tag s pos names_of_tag in
    iter_tags (ntags-1) pos (tag :: tags) 
  in
  iter_tags (get_int s pos) (pos+4) []
  
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





