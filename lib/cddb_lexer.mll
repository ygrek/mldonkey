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

{
open Printf2
  
type cddb_info =  
  EOF
| TRACKPOS of string
| DISCLENGTH of string
| DISCID of string
| DTITLE of string
| DYEAR of string
| DGENRE of string
| EXTD of string
| PLAYORDER of string

| TTITLE of int * string
| EXTT of int * string

} 

rule line = parse
    '#' { comment lexbuf }
  | eof { EOF }
  | ' '* '\n' { line lexbuf }
  | "DISCID=" { DISCID (end_of_line lexbuf) }
  | "DTITLE=" { DTITLE (end_of_line lexbuf) }
  | "DYEAR="  { DYEAR  (end_of_line lexbuf) }
  | "DGENRE=" { DGENRE (end_of_line lexbuf) }
  | "EXTD=" { EXTD (end_of_line lexbuf) }
  | "PLAYORDER=" { PLAYORDER (end_of_line lexbuf) }
  
  | "TTITLE" [ '0' - '9' ]+ "=" { 
      let s = Lexing.lexeme lexbuf in
      let len = String.length s in
      let len_title = String.length "TTITLE" in
      let pos = int_of_string (String.sub s len_title (len - len_title - 1)) in
      TTITLE (pos, end_of_line lexbuf) }
  
  | "EXTT" [ '0' - '9' ]+ "=" { 
      let s = Lexing.lexeme lexbuf in
      let len = String.length s in
      let len_title = String.length "EXTT" in
      let pos = int_of_string (String.sub s len_title (len - len_title - 1)) in
      EXTT (pos, end_of_line lexbuf) }
    
  | [ 'a'-'z' 'A'-'Z' '0'-'9'] + '=' { 
      let name = Lexing.lexeme lexbuf in
      let name = String.sub name 0 (String.length name - 1) in
      let value = end_of_line lexbuf in
      lprintf "UNKNOWN KEY %s" name;
      lprint_newline (); 
      line lexbuf
    }
    
and comment = parse
    [' '  '\t'] + { comment lexbuf }
  | '\n' { line lexbuf }
  | ['0'-'9'] + { 
      let v = Lexing.lexeme lexbuf in
      let _ = end_of_line lexbuf in
      TRACKPOS  v
    }
  | "Disc length:" { 
      get_length lexbuf }
  | _ { let s = end_of_line lexbuf in
(*      lprintf "DISCARD [%s]" s; lprint_newline (); *)
      line lexbuf }
    
and get_length = parse
  | ['0'-'9'] + { 
      let v = Lexing.lexeme lexbuf in
      let _ = end_of_line lexbuf in
  DISCLENGTH v
    }
  |  [' '  '\t'] + { get_length lexbuf }
    
and end_of_line = parse
    [ ^ '\n' ]* { let v = Lexing.lexeme lexbuf in
    let _ = get_end_of_line lexbuf in
    v }

  
and get_end_of_line = parse 
    '\n' { () }
  | eof { () }
    
