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

open Autoconf

type expected_types =
  Type_int
| Type_char
| Type_string
| Type_float  
| Type_bool
| Type_int32
| Type_int64
| Type_nativeint
  
let ty_arrow x y = x :: y

(* Taken from ocaml-3.04, typing/typecore.ml *)
  
let type_format fmt =
  let len = String.length fmt in
  let incomplete i =
    failwith (Printf.sprintf "Incomplete format %s" (String.sub fmt i (len - i)))
  in
  let bad_format i j=
    failwith (Printf.sprintf "Bad format %s" (String.sub fmt i (j - i + 1)))
  
  in
  let ty_result = [] in
  let rec scan_format i =
    if i >= len then ty_result else
    match fmt.[i] with
    | '%' -> scan_flags i (i+1)
    | _ -> scan_format (i+1)
  and scan_flags i j =
    if j >= len then incomplete i else
    match fmt.[j] with
    | '#' | '0' | '-' | ' ' | '+' -> scan_flags i (j+1)
    | _ -> scan_width i j
  and scan_width i j =
    if j >= len then incomplete i else
    match fmt.[j] with
    | '*' -> ty_arrow Type_int (scan_dot i (j+1))
    | '.' -> scan_precision i (j+1)
    | _ -> scan_fixed_width i j
  and scan_fixed_width i j =
    if j >= len then incomplete i else
    match fmt.[j] with
    | '0' .. '9' | '-' | '+' -> scan_fixed_width i (j+1)
    | '.' -> scan_precision i (j+1)
    | _ -> scan_conversion i j
  and scan_dot i j =
    if j >= len then incomplete i else
    match fmt.[j] with
    | '.' -> scan_precision i (j+1)
    | _ -> scan_conversion i j
  and scan_precision i j =
    if j >= len then incomplete i else
    match fmt.[j] with
    | '*' -> ty_arrow Type_int (scan_conversion i (j+1))
    | _ -> scan_fixed_precision i j
  and scan_fixed_precision i j =
    if j >= len then incomplete i else
    match fmt.[j] with
    | '0' .. '9' | '-' | '+' -> scan_fixed_precision i (j+1)
    | _ -> scan_conversion i j
  and scan_conversion i j =
    if j >= len then incomplete i else
    match fmt.[j] with
    | '%' -> scan_format (j+1)
    | 's' | 'S' | '[' ->
        ty_arrow Type_string (scan_format (j+1))
    | 'c' | 'C' ->
        ty_arrow Type_char (scan_format (j+1))
    | 'd' | 'i' | 'o' | 'x' | 'X' | 'u' | 'N' ->
        ty_arrow Type_int (scan_format (j+1))
    | 'f' | 'e' | 'E' | 'g' | 'G' ->
        ty_arrow Type_float (scan_format (j+1))
    | 'b' ->
        ty_arrow Type_bool (scan_format (j+1))
    | 'a' ->
        bad_format i j
    | 't' ->
        bad_format i j
    | 'l' ->
        if j+1 >= len then incomplete i else begin
            match fmt.[j+1] with
            | 'd' | 'i' | 'o' | 'x' | 'X' | 'u' ->
                ty_arrow Type_int32 (scan_format (j+2))
            | c ->
                bad_format i j
          end
    | 'n' ->
        if j+1 >= len then incomplete i else begin
            match fmt.[j+1] with
            | 'd' | 'i' | 'o' | 'x' | 'X' | 'u' ->
                ty_arrow Type_nativeint (scan_format (j+2))
            | c ->
                bad_format i j
        end
    | 'L' ->
        if j+1 >= len then incomplete i else begin
          match fmt.[j+1] with
          | 'd' | 'i' | 'o' | 'x' | 'X' | 'u' ->
              ty_arrow Type_int64 (scan_format (j+2))
          | c ->
              bad_format i j
        end
    | c ->
        bad_format i j
  in
  scan_format 0
  
type 'a variable
type 'a arrow 


let arrow_add_variable
  (x : 'a variable)
  (y : 'b arrow) = 
  let x = Obj.magic x in
  let y = Obj.magic y in
  (Obj.magic (x :: y) : ('a -> 'b) arrow)


  
open Options
  
let value_to_text (expected_type : 'a arrow) v =
  let s = value_to_string v in
  let expected_type = Obj.magic expected_type in
  let format_type = type_format s in
  if format_type = expected_type then 
    (Obj.magic s : ('a, unit, string) format) else
    failwith "Bad format"

let text_to_value v = 
  let v = Obj.magic v in
  string_to_value v
    
let text_option (expected_type : 'a arrow)
  = 
  define_option_class "Text" 
    (value_to_text expected_type) 
  text_to_value

let gettext v = Printf.sprintf !!v
  
let buftext buf (v : ('a, Buffer.t, unit) format Options.option_record) = 
  Printf.bprintf buf !!v
  
module T = struct
    let int x = arrow_add_variable (Obj.magic Type_int : int variable) x
    let char x = arrow_add_variable (Obj.magic Type_char : char variable) x
    let string x = arrow_add_variable (Obj.magic Type_string : string variable) x
    let float x = arrow_add_variable (Obj.magic Type_float : float variable) x
    let bool x = arrow_add_variable (Obj.magic Type_bool : bool variable) x
    let int32 x = arrow_add_variable (Obj.magic Type_int32 : int32 variable) x
    let int64 x = arrow_add_variable (Obj.magic Type_int64 : int64 variable) x
    let nativeint x = arrow_add_variable (Obj.magic Type_nativeint : nativeint variable) x
    let format = (Obj.magic [] : string arrow)
    let bformat = (Obj.magic [] : unit arrow)
    let option = text_option
    let boption x = (Obj.magic text_option) x
  end


(********* Some tests ************)

(*
let option_file = create_options_file "test.ini"
  
let nshared = define_option option_file
  ["nshared"] "Text for Nshared option"
    (text_option 
      (T.int (T.int32 T.format))) 
  "Shared: %d/%ld"
  
let _ =
  try 
    load option_file
  with Sys_error _ ->
      save_with_help option_file
      
let _ =
  lprint_string (Printf.sprintf !! nshared 23 (Int32.one));
  lprint_newline ();
  *)

module NEW = struct 
    
    type 'a message = {
        name : string;
        index : int;
      }
    
    let strings = Hashtbl.create 1111
    let next_slot = ref 0
    let translation = ref [||]
    let default = ref [||]
    let requests = ref [||]
    let message_file = ref ""    
    
    let save_message_file = ref false
    
    let no_translation = ""
    
    let register x =
      try
        let index = Hashtbl.find strings x in
        { name = x; index = index }        
      with Not_found ->
          
          save_message_file := true;
          
          if !next_slot = Array.length !translation then begin
              
              let new_array = Array.create (2 * !next_slot+ 1) no_translation
              in
              Array.blit !translation 0 new_array 0 !next_slot;
              translation := new_array;
              
              let new_array = Array.create (2 * !next_slot+ 1) 0 
              in
              Array.blit !requests 0 new_array 0 !next_slot;
              requests := new_array;
              
              let new_array = Array.create (2 * !next_slot+ 1) no_translation
              in
              Array.blit !default 0 new_array 0 !next_slot;
              default := new_array;
            
            end;
          let index = !next_slot in
          incr next_slot;
          !default.(index ) <- x;
          { name = x ; index = index }        
    
    let b_ x = 
      let y = (Obj.magic x : string) in
      Obj.magic (register y : string message)
    
    let s_ x = register x
    
    let p_ m = 
      let index = m.index in
      !requests.(index) <- !requests.(index) + 1;
      let translation = !translation.(index) in
      let s= if translation == no_translation then
          !default.(index)
        else translation
      in
      Obj.magic (s : string)
    
    
    let save_messages () =
      if !message_file <> "" then
        let oc = open_out !message_file in

(* Untranslated strings first *)        
        for i = 0 to Array.length !default do
          if !translation.(i) == no_translation then
          Printf.fprintf oc "\"%s\" = \"\"\n"
            (String.escaped !default.(i)) 
        done;

(* Translated strings second *)
        for i = 0 to Array.length !default do
          if !translation.(i) != no_translation then
            Printf.fprintf oc "\"%s\" = \"%s\"\n"
              (String.escaped !default.(i)) 
            (String.escaped !translation.(i))
        done;
        
        close_out oc;
        save_message_file := false
        
    let save_message_file () =
      if !save_message_file then
        save_messages ()
    
    
    let set_message_file filename = 
(* If the file exists, load it. Check that '%' formats are the same
in the default and in the translation. *)

      save_message_file ()
      
        
  end