
open Printf;;

let locfmt =
  match Sys.os_type with
  | "MacOS" -> ("File \"%s\"; line %d; characters %d to %d ### %s"
                : ('a, 'b, 'c) format)
  | _ -> ("File \"%s\", line %d, characters %d-%d: %s" : ('a, 'b, 'c) format)
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
