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

type token =
    Kwd of string
  | Ident of string
  | Int of int64
  | Float of float
  | String of string
  | Char of char


(* The string buffering machinery *)

let initial_buffer = Buffer.create 32

let buffer = ref initial_buffer
let bufpos = ref 0

let reset_buffer () = buffer := initial_buffer; bufpos := 0

let store c =
  Buffer.add_char !buffer c;
  incr bufpos

let get_string () =
  let s = Buffer.sub !buffer 0 !bufpos in
  Buffer.clear !buffer;
  bufpos := 0;
  s

(* The lexer *)

let make_lexer keywords =
  let kwd_table = Hashtbl.create 17 in
  List.iter (fun s -> Hashtbl.add kwd_table s (Kwd s)) keywords;
  let ident_or_keyword id =
    try Hashtbl.find kwd_table id with
      Not_found -> Ident id
  and keyword_or_error c =
    let s = String.make 1 c in
    try Hashtbl.find kwd_table s with
      Not_found -> raise (Stream.Error ("Illegal character " ^ s))
  in
  let rec next_token (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
      Some (' ' | '\010' | '\013' | '\009' | '\026' | '\012') ->
        Stream.junk strm__; next_token strm__
    | Some ('A'..'Z' | 'a'..'z' | '\192'..'\255' as c) ->
        Stream.junk strm__;
        let s = strm__ in reset_buffer (); store c; ident s
    | Some
        ('!' | '%' | '&' | '$' | '#' | '+' | '/' | ':' | '<' | '=' | '>' |
         '?' | '@' | '\\' | '~' | '^' | '|' | '*' as c) ->
        Stream.junk strm__;
        let s = strm__ in reset_buffer (); store c; ident2 s
    | Some ('0'..'9' as c) ->
        Stream.junk strm__;
        let s = strm__ in reset_buffer (); store c; number s
    | Some '\'' ->
        Stream.junk strm__;
        let c =
          try char strm__ with
            Stream.Failure -> raise (Stream.Error "")
        in
        begin match Stream.peek strm__ with
          Some '\'' -> Stream.junk strm__; Some (Char c)
        | _ -> raise (Stream.Error "")
        end
    | Some '"' ->
        Stream.junk strm__;
        let s = strm__ in reset_buffer (); Some (String (string s))
    | Some '-' -> Stream.junk strm__; neg_number strm__
    | Some '(' -> Stream.junk strm__; maybe_comment strm__
    | Some c -> Stream.junk strm__; Some (keyword_or_error c)
    | _ -> None
  and ident (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
      Some
        ('A'..'Z' | 'a'..'z' | '\192'..'\255' | '0'..'9' | '_' | '\'' as c) ->
        Stream.junk strm__; let s = strm__ in store c; ident s
    | _ -> Some (ident_or_keyword (get_string ()))
  and ident2 (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
      Some
        ('!' | '%' | '&' | '$' | '#' | '+' | '-' | '/' | ':' | '<' | '=' |
         '>' | '?' | '@' | '\\' | '~' | '^' | '|' | '*' as c) ->
        Stream.junk strm__; let s = strm__ in store c; ident2 s
    | _ -> Some (ident_or_keyword (get_string ()))
  and neg_number (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
      Some ('0'..'9' as c) ->
        Stream.junk strm__;
        let s = strm__ in reset_buffer (); store '-'; store c; number s
    | _ -> let s = strm__ in reset_buffer (); store '-'; ident2 s
  and number (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
      Some ('0'..'9' as c) ->
        Stream.junk strm__; let s = strm__ in store c; number s
    | Some '.' ->
        Stream.junk strm__; let s = strm__ in store '.'; decimal_part s
    | Some ('e' | 'E') ->
        Stream.junk strm__; let s = strm__ in store 'E'; exponent_part s
    | _ -> Some (Int (Int64.of_string (get_string ())))
  and decimal_part (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
      Some ('0'..'9' as c) ->
        Stream.junk strm__; let s = strm__ in store c; decimal_part s
    | Some ('e' | 'E') ->
        Stream.junk strm__; let s = strm__ in store 'E'; exponent_part s
    | _ -> Some (Float (float_of_string (get_string ())))
  and exponent_part (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
      Some ('+' | '-' as c) ->
        Stream.junk strm__; let s = strm__ in store c; end_exponent_part s
    | _ -> end_exponent_part strm__
  and end_exponent_part (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
      Some ('0'..'9' as c) ->
        Stream.junk strm__; let s = strm__ in store c; end_exponent_part s
    | _ -> Some (Float (float_of_string (get_string ())))
  and string (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
      Some '"' -> Stream.junk strm__; get_string ()
    | Some '\\' ->
        Stream.junk strm__;
        let c =
          try escape strm__ with
            Stream.Failure -> raise (Stream.Error "")
        in
        let s = strm__ in store c; string s
    | Some c -> Stream.junk strm__; let s = strm__ in store c; string s
    | _ -> raise Stream.Failure
  and char (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
      Some '\\' ->
        Stream.junk strm__;
        begin try escape strm__ with
          Stream.Failure -> raise (Stream.Error "")
        end
    | Some c -> Stream.junk strm__; c
    | _ -> raise Stream.Failure
  and escape (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
      Some 'n' -> Stream.junk strm__; '\n'
    | Some 'r' -> Stream.junk strm__; '\r'
    | Some 't' -> Stream.junk strm__; '\t'
    | Some ('0'..'9' as c1) ->
        Stream.junk strm__;
        begin match Stream.peek strm__ with
          Some ('0'..'9' as c2) ->
            Stream.junk strm__;
            begin match Stream.peek strm__ with
              Some ('0'..'9' as c3) ->
                Stream.junk strm__;
                Char.chr
                  ((Char.code c1 - 48) * 100 + (Char.code c2 - 48) * 10 +
                     (Char.code c3 - 48))
            | _ -> raise (Stream.Error "")
            end
        | _ -> raise (Stream.Error "")
        end
    | Some c -> Stream.junk strm__; c
    | _ -> raise Stream.Failure
  and maybe_comment (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
      Some '*' ->
        Stream.junk strm__; let s = strm__ in comment s; next_token s
    | _ -> Some (keyword_or_error '(')
  and comment (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
      Some '(' -> Stream.junk strm__; maybe_nested_comment strm__
    | Some '*' -> Stream.junk strm__; maybe_end_comment strm__
    | Some _ -> Stream.junk strm__; comment strm__
    | _ -> raise Stream.Failure
  and maybe_nested_comment (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
      Some '*' -> Stream.junk strm__; let s = strm__ in comment s; comment s
    | Some _ -> Stream.junk strm__; comment strm__
    | _ -> raise Stream.Failure
  and maybe_end_comment (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
      Some ')' -> Stream.junk strm__; ()
    | Some '*' -> Stream.junk strm__; maybe_end_comment strm__
    | Some _ -> Stream.junk strm__; comment strm__
    | _ -> raise Stream.Failure
  in
  fun input -> Stream.from (fun _count -> next_token input)
