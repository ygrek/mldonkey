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

open Printf;;

let locfmt =
  Obj.magic (match Sys.os_type with
  | "MacOS" -> "File \"%s\"; line %d; characters %d to %d ### %s"
  | _ -> "File \"%s\", line %d, characters %d-%d: %s")
;;

let field x i =
  let f = Obj.field x i in
  if not (Obj.is_block f) then
    sprintf "%d" (Obj.magic f : int)           (* can also be a char *)
  else if Obj.tag f = Obj.string_tag then
    sprintf "\"%s\"" (String.escaped (Obj.magic f : string))
  else if Obj.tag f = Obj.double_tag then
    string_of_float (Obj.magic f : float)
  else
    "_"
;;
let rec other_fields x i =
  if i >= Obj.size x then ""
  else sprintf ", %s%s" (field x i) (other_fields x (i+1))
;;
let fields x =
  match Obj.size x with
  | 0 -> ""
  | 1 -> ""
  | 2 -> sprintf "(%s)" (field x 1)
  | n -> sprintf "(%s%s)" (field x 1) (other_fields x 2)
;;

let printers = ref []

let rec check exn printers =
  match printers with
    [] -> raise Not_found
  | printer :: printers ->
      try printer exn with _ ->
          check exn printers

let to_string = function
  | Out_of_memory -> "Out of memory";
  | Stack_overflow -> "Stack overflow";
  | Match_failure(file, first_char, last_char) ->
      sprintf locfmt file 0 first_char last_char "Pattern matching failed";
  | Assert_failure(file, first_char, last_char) ->
      sprintf locfmt file 0 first_char last_char "Assertion failed";
  | x ->
      try
        check x !printers
      with _ ->
      let x = Obj.repr x in
      let constructor = (Obj.magic(Obj.field (Obj.field x 0) 0) : string) in
      constructor ^ (fields x)
;;

let print fct arg =
  try
    fct arg
  with x ->
    eprintf "Uncaught exception: %s\n" (to_string x);
    flush stderr;
    raise x

let catch fct arg =
  try
    fct arg
  with x ->
    flush stdout;
    eprintf "Uncaught exception: %s\n" (to_string x);
    exit 2

      
let register_exn f = printers := f :: !printers
  
let catchexn s f =
  try f () with
    e -> 
      Printf.printf "Uncaught exception in %s: %s" s (to_string e);
      print_newline () 
  
let vcatchexn s f =
  try Some (f ()) with
    e -> 
      Printf.printf "Uncaught exception in %s: %s" s (to_string e);
      print_newline ();
      None

let _ =
  register_exn (fun e ->
      match e with
        Unix.Unix_error (e, f, arg) ->
          Printf.sprintf "%s failed%s: %s" f (if arg = "" then "" else 
              "on " ^ arg) (Unix.error_message e)
          | _ -> raise e
  )
