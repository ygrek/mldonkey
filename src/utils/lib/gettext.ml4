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

let log_prefix = "[Gettext]"

let lprintf_nl fmt =
  lprintf_nl2 log_prefix fmt

let lprintf_n fmt =
  lprintf2 log_prefix fmt

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
  *)

type 'a _string = {
    name : string;
    index : int;
  }

let strings = Hashtbl.create 1111
let next_slot = ref 0
let translation = ref [||]
let verified = ref [||]
let default = ref [||]
let requests = ref [||]
let strings_file = ref None    

let strings_file_error = ref false
let save_strings_file = ref false

let no_translation = "   "
let modules = Hashtbl.create 11
  
let register modname x =
  try
    Hashtbl.find strings x
  with Not_found ->

(*          lprintf "New message [%s]\n" x;  *)
      save_strings_file := true;
      
      if !next_slot = Array.length !translation then begin

(*              lprintf "Incrementing size\n"; *)
          let new_array = Array.make (2 * !next_slot+ 1) false
          in
          Array.blit !verified 0 new_array 0 !next_slot;
          verified := new_array;
          
          let new_array = Array.make (2 * !next_slot+ 1) no_translation
          in
          Array.blit !translation 0 new_array 0 !next_slot;
          translation := new_array;
          
          let new_array = Array.make (2 * !next_slot+ 1) 0 
          in
          Array.blit !requests 0 new_array 0 !next_slot;
          requests := new_array;
          
          let new_array = Array.make (2 * !next_slot+ 1) no_translation
          in
          Array.blit !default 0 new_array 0 !next_slot;
          default := new_array;
        
        end;
      let index = !next_slot in
      let m = { name = x ; index = index } in
      ( try
          let names = Hashtbl.find modules modname in
          names := index :: !names
        with _ -> 
            Hashtbl.add modules modname (ref [index]));
      Hashtbl.add strings x m;
      incr next_slot;
      !default.(index ) <- x;
      m

let translate modname s t =
  if t <> "" && t <> s then
    begin
(*          lprintf "Register\n"; *)
      let m = register modname s in
(*          lprintf "Translation: %s = %s\n" s t; *)
      save_strings_file := true;
      !translation.(m.index) <- t
    end
(*
    let  x = 
      let y = (Obj.magic x : string) in
      Obj.magic (register y : string message)

    let s_ x = register x
*)


let verify index translated = 
  let index_type = type_format !default.(index) in
  let translated_type = type_format translated in
  if index_type = translated_type then begin
      !verified.(index) <- true;
      true
    end else begin
      lprintf_nl "Bad format for %s\n" translated;
      save_strings_file := true;
      !translation.(index) <- no_translation;
      false
    end

let ss_ modname (x : string) = register modname x
let _ss m = 
  let index = m.index in
  !requests.(index) <- !requests.(index) + 1;
  let translation = !translation.(index) in
  let s= if translation == no_translation then
      !default.(index)
    else 
      translation
  in
  s 

let _s modname (x : string) = _ss (ss_ modname x)

let bb_ : string -> ('a, 'b, 'c, 'd) format4 -> ('a, 'b, 'c, 'd) format4 _string = fun modname -> Obj.magic (ss_ modname)
let _bb : ('a, 'b, 'c, 'd) format4 _string -> ('a, 'b, 'c, 'd) format4 = fun m ->
  let index = m.index in
  !requests.(index) <- !requests.(index) + 1;
  let translation = !translation.(index) in
  let s= if translation == no_translation then
      !default.(index)
    else 
    if !verified.(index) || verify index translation then
      translation
    else !default.(index)
  in
  Obj.magic s 

let _b modname x = _bb (bb_ modname x)
  
  
let save_strings () =
  match !strings_file with 
    None -> ()
  | Some filename ->
      if !save_strings_file && not !strings_file_error then	
        try
          Unix2.tryopen_write filename (fun oc ->
            
            Hashtbl.iter (fun modname names ->

                Printf.fprintf oc "(************************************)\n";
                Printf.fprintf oc "         module \"%s\"\n" (String.escaped modname);
                Printf.fprintf oc "(************************************)\n\n";

                
                List.iter (fun i ->
                    
                    Printf.fprintf oc "\"%s\" = \"%s\"\n\n"
                      (String.escaped !default.(i)) 
                    (String.escaped 
                        (if !translation.(i) != no_translation then
                          !translation.(i)
                        else
                          !default.(i)))
                      
                ) !names;
                
            ) modules;
            
            save_strings_file := false)
        with e ->
            lprintf_nl "save_strings: Error %s"
              (Printexc2.to_string e)
open Genlex2

let lexer = make_lexer [ "=" ; "module" ]

let current_modname = ref ""
  
let rec parse_file = (parser
  |   [< 'String s0; 'Kwd "="; 'String s1; strm >] ->
(*      lprintf "trans\n"; *)
  translate !current_modname s0 s1; parse_file strm
  |   [< 'Kwd "module"; 'String modname; strm >] ->
  current_modname := modname;
  parse_file  strm
  |   [< >] -> (* lprintf "done\n" *) ())
  
let set_strings_file filename =   
  let filename =
    let extension = try
        Unix.getenv "LANG"
      with _ -> (Charset.Locale.default_language ^ "_" ^ Charset.Locale.locale_string)
    in
    Printf.sprintf "%s.%s" filename extension
  in
  
  (match !strings_file with Some _ -> ()
    | None -> 
        Pervasives.at_exit (fun _ -> try save_strings () with _ -> ()));
  strings_file := Some filename;

(* If the file exists, load it. Check that '%' formats are the same
in the default and in the translation. *)
(*lprintf "Loading...\n"; *)
  (try
    Unix2.tryopen_read filename (fun ic ->
      lprintf_nl "Loading language resource %s" filename;
      let s = Stream.of_channel ic in
      try
        let stream = lexer s in
(*        lprintf "x\n"; *)
        current_modname := "general";
        parse_file stream
      with e -> 
          strings_file_error := true;
        lprintf_nl "set_strings_file: Exception %s in %s at pos %d"
          (Printexc2.to_string e) filename (Stream.count s))
    with e -> 
        save_strings_file := true);
  save_strings ()


let _ =
  try
    let file = Sys.getenv "GETTEXT_FILE" in
    let ext = Sys.getenv "LANG" in
    let f1 = Printf.sprintf "%s.en" file in
    let f2 = Printf.sprintf "%s.%s" file ext in
    let strings = Hashtbl.create 111 in
    
let translate1 s0 s1 =
  lprintf_nl "translate0 %s" s0;
  Hashtbl.add strings s0 s1
in

    
let rec parse_file = (parser
  |   [< 'Ident s0; 'Kwd "="; s1 = parse_next; strm >] ->
  translate1 s0 s1; parse_file strm
  |   [< >] -> (* lprintf "done\n" *) ())
  
and parse_next = parser
      |   [< 'Ident s1 >] -> s1
      |   [< 'String s1 >] -> s1
in

(try
  Unix2.tryopen_read f1(fun ic ->
    let s = Stream.of_channel ic in
    try
      let stream = lexer s in
(*        lprintf "x\n"; *)
      parse_file stream 
    with e -> 
        strings_file_error := true;
        lprintf_nl "set_strings_file: Exception %s in %s at pos %d"
           (Printexc2.to_string e) f1 (Stream.count s))
  with e -> 
    save_strings_file := true;
    lprintf_nl "set_strings_file: no message file found. Creating one");

let translate2 s0 s1 =
  try
    lprintf_nl "translate2 %s" s0;
    let s0 = Hashtbl.find strings s0 in
    translate "Former Translation" s0 s1
  with _ -> lprintf_nl "No translation for %s" s0
in
    
let rec parse_file = (parser
  |   [< 'Ident s0; 'Kwd "="; s1 = parse_next; strm >] ->
  translate2 s0 s1; parse_file strm
  |   [< >] -> (* lprintf "done\n" *) ())
  
and parse_next = parser
      |   [< 'Ident s1 >] -> s1
      |   [< 'String s1 >] -> s1
in

      try
        Unix2.tryopen_read f2 (fun ic ->
       let s = Stream.of_channel ic in
        try
          let stream = lexer s in
(*        lprintf "x\n"; *)
          parse_file stream 
        with e -> 
            strings_file_error := true; 
            lprintf_nl "set_strings_file: Exception %s in %s at pos %d"
               (Printexc2.to_string e) f2 (Stream.count s))
     with e -> 
       save_strings_file := true;
       lprintf_nl "set_strings_file: no message file found. Creating one"
    
    
  with _ -> ()
      
