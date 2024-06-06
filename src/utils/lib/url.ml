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

open Buffer

type url = {
    proto : string;
    server : string;
    port : int;
    full_file : string;
    short_file : string;
    user : string;
    passwd : string;
    args : (string*string) list;
    
    string : string;
  }

(* encode using RFC 1738 form *)
let encode s =
  let pos = ref 0 in
  let len = String.length s in
  let res = String.create (3*len) in
  let hexa_digit x =
    if x >= 10 then Char.chr (Char.code 'A' + x - 10)
    else Char.chr (Char.code '0' + x) in
  for i=0 to len-1 do
    match s.[i] with
    | 'a'..'z' | 'A'..'Z' | '0'..'9' | '.' | '-' | '*' | '_' | '\''| '(' | ')'->
        res.[!pos] <- s.[i]; incr pos
    | c ->
        res.[!pos] <- '%';
        res.[!pos+1] <- hexa_digit (Char.code c / 16);
        res.[!pos+2] <- hexa_digit (Char.code c mod 16);
        pos := !pos + 3
  done;
  Bytes.sub res 0 !pos

let encode_to_string s =
  Bytes.to_string (encode s)

(** decodes a sting according RFC 1738
or x-www-form-urlencoded ('+' with ' ')
  @param raw true use RFC 1738
  @param string string to decode
*)
let decode ?(raw=true) s =
  let len = String.length s in
  let r = Buffer.create len in
  let rec iter i =
    if i < len then
      match s.[i] with
      | '%' ->
          let n = 
            try
              (* int_of_string with leading "0x", string is read hexadecimal *)
              Buffer.add_char r (char_of_int (int_of_string ("0x" ^ (String.sub s (i+1) 2))));
              3
            with _ ->
                Buffer.add_char r '%';
                1
          in
          iter (i+n)

      (* if not raw decode '+' -> ' ' else don't change char *)
      | '+' ->  let c = if raw then '+' else ' ' in
                Buffer.add_char r c; iter (i+1)

      | c -> Buffer.add_char r c; iter (i+1)
  in
  iter 0;
  Buffer.contents r


let to_string url =
  let res = Buffer.create 80 in
  add_string res url.proto;
  add_string res "://";
  if url.user <> "" || url.passwd <> "" then begin
      add_string res url.user;
      add_string res ":";
      add_string res url.passwd;
      add_string res "@";
    end;
  add_string res url.server;
    (match url.proto, url.port with
      "http", 80 
    | "ftp", 21
    | "ssh", 22 -> ()
    | ("http" | "ftp" | "ssh"), _ ->
        (add_char res ':'; add_string res (string_of_int url.port));
    | _, port when port <> 0 ->
        (add_char res ':'; add_string res (string_of_int url.port));
    | _ -> ());
  add_string res url.full_file;
  contents res

let cut_args url_end =
  if url_end = "" then []
  else
  let args = String2.split url_end '&' in
  List.map (fun s -> 
        let (name, value) = String2.cut_at s '=' in
      decode ~raw:false name, decode ~raw:false value
    ) args 

let create proto user passwd server port full_file =
  let short_file, args = String2.cut_at full_file '?' in
  let args = cut_args args in

  (*
  let user, passw, server = 
    let userpass, server = String2.cut_at server '@' in
    if server = "" then "", "", server
    else
    let user, pass = String2.cut_at userpass ':' in
    user, pass, server
  in
*)
  
  let url = 
    {
      proto = proto;
      server = server;
      port = port;
      full_file = full_file;
      short_file = short_file;
      user = user;
      passwd = passwd;
      args = args;
      
      string = "";
    }
  in
  { url with string = to_string url }
  
  (*
  let port = if proto = "ftp" && port = 80 then 21 else port in
  let url = { proto=proto; server=server; port=port; full_file=file;
      user=user; passwd=pass; file = short_file; args = args; string = "" } in
  { url with string = to_string url }
    *)

let put_args s args =
  if args = [] then s else
  let res = Buffer.create 256 in
  Buffer.add_string res s;
  Buffer.add_char res (if String.contains s '?' then '&' else '?');
  let rec manage_args = function
    | [] -> assert false
    | [a, ""] ->
        Buffer.add_bytes res (encode a)
    | [a, b] ->
        Buffer.add_bytes res (encode a); Buffer.add_char res '='; Buffer.add_bytes res 
          (encode b)
    | (a,b)::l ->
        Buffer.add_bytes res (encode a); Buffer.add_char res '='; Buffer.add_bytes res 
          (encode b);
        Buffer.add_char res '&'; manage_args l in
(*  lprintf "len args %d" (List.length args); lprint_newline ();*)
  manage_args args;
  Buffer.contents res  
  
let of_string ?(args=[]) s =
  let remove_leading_slashes s =
    let len = String.length s in
    let left =
      let rec aux i =
        if i < len && s.[i] = '/' then aux (i+1) else i in
      aux 0 in
    if left = 0 then s
    else
      String.sub s left (len - left) in

  (* redefine s to remove all leading slashes *)
  let s = remove_leading_slashes s in

  let s = put_args s args in
  let url =
    let get_two init_pos =
      let pos = ref init_pos in
      while s.[!pos] <> ':' && s.[!pos] <> '/' && s.[!pos] <> '@' do
        incr pos
      done;
      let first = String.sub s init_pos (!pos - init_pos) in
      if s.[!pos] = ':'
      then
        (let deb = !pos+1 in
          while s.[!pos] <> '@' && s.[!pos] <> '/' do
            incr pos
          done;
          (first, String.sub s deb (!pos-deb), !pos))
      else
        (first, "", !pos) in
    let cut init_pos default_port =
      let stra, strb, new_pos = get_two init_pos in
      let user, pass, host, port, end_pos =
        if s.[new_pos] = '@'
        then
          (let host, port_str, end_pos = get_two (new_pos+1) in
            let port =
              if port_str="" then default_port else int_of_string port_str in
            stra, strb, host, port, end_pos)
        else
          (let port = if strb="" then default_port else int_of_string strb in
            "", "", stra, port, new_pos) in
      let len = String.length s in
      let file = String.sub s end_pos (len - end_pos) in
      host, port, file, user, pass in
    try
      let colon = String.index s ':' in
      let len = String.length s  in
      if len > colon + 2 &&
        s.[colon+1] = '/' &&
        s.[colon+2] = '/' then
        let proto =  String.sub s 0 colon in
        let port = match proto with
            "http" -> 80
          | "ftp" -> 21
          | "ssh" -> 22
          | _ -> 0
        in
        let host, port, full_file, user, pass = cut (colon+3) port in
        create proto user pass host port full_file
        
      else
        raise Not_found 
    with Not_found ->
        let short_file, args = String2.cut_at s '?' in
        let args = cut_args args in
        {
          proto = "file";
          server = "";
          port = 0;
          full_file = s;
          short_file = short_file;
          user = "";
          passwd = "";
          args = args;
          
          string = s;
        }

        (*
    if String2.check_prefix s "http://"
    then
      try
      with _ -> raise (Invalid_argument "this string is not a valid http url")
    else if String2.check_prefix s "ftp://"
    then
      try
        let host, port, file, user, pass = cut 6 21 in
        create "ftp" user pass host port full_file
      with _ -> raise (Invalid_argument "this string is not a valid ftp url")
    else if String2.check_prefix s "ssh://"
    then
      try
        let host, port, file, user, pass = cut 6 22 in
        create "ssh" user pass host port full_file
      with _ -> raise (Invalid_argument "this string is not a valid ssh url")
    else

    let file = s in
    Printf2.lprintf "NEW URL FOR %s\n" file;
create "file"~proto: "file"  file
  *)
  in
  url 
        
let to_string url = url.string
  
  
let to_string_no_args url =
  let res = Buffer.create 80 in
  add_string res url.proto;
  add_string res "://";
  add_string res url.server;
  (match url.proto, url.port with
      "http", 80 
    | "ftp", 21
    | "ssh", 22 -> ()
    | ("http" | "ftp" | "ssh"), _ ->
        (add_char res ':'; add_string res (string_of_int url.port));
    | _, port when port <> 0 ->
        (add_char res ':'; add_string res (string_of_int url.port));
    | _ -> ());
  add_string res url.short_file;
  contents res

open Options
let option = 
  define_option_class "URL"
    (fun v -> of_string (value_to_string v))
  (fun url -> string_to_value (to_string url))
