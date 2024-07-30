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


    (* Simple options:
  This will enable very simple configuration, by a mouse-based configurator.
  Options will be defined by a special function, which will also check
  if a value has been provided  by the user in its .gwmlrc file.
  The .gwmlrc will be created by a dedicated tool, which could be used
  to generate both .gwmlrc and .efunsrc files.

*)

open Printf2

type option_value =
    Module of option_module
  | StringValue of string
  | IntValue of int64
  | FloatValue of float
  | List of option_value list
  | SmallList of option_value list
  | OnceValue of option_value
  | DelayedValue of (out_channel -> string -> unit)
and option_module = (string * option_value) list

exception SideEffectOption
exception OptionNotFound
  
type 'a option_class =
  { class_name : string;
    from_value : option_value -> 'a;
    to_value : 'a -> option_value;
    mutable class_hooks : ('a option_record -> unit) list;
    mutable string_wrappers : (('a -> string) * (string -> 'a)) option;
  }
and 'a option_record =
  { 
    option_class : 'a option_class;
    mutable option_value : 'a;
    option_name : string list;
    mutable option_desc : string;
    option_help : string;
    option_default : 'a;
    mutable option_hooks : (unit -> unit) list;
    option_section : options_section;
    option_advanced : bool;    
    option_restart : bool;
    option_public : bool;
    option_internal : bool;
    }
and options_file =
  { mutable file_name : string;
    mutable file_sections : options_section list;
    mutable file_rc : option_module;
    mutable file_pruned : bool;
    
    mutable file_before_save_hook : (unit -> unit);
    mutable file_after_save_hook : (unit -> unit);
    mutable file_after_load_hook : (unit -> unit);
    }
and options_section = {
    section_name : string list;
    section_help : string;
    section_file : options_file;
    mutable section_options : Obj.t option_record list;
  }
  
let file_section file section_name section_help =
  let s = {
      section_name = section_name;
      section_help = section_help;
      section_file = file;
      section_options = [];
    }
  in
  file.file_sections <- file.file_sections @ [s];
  s
  
let create_options_file name =
  let file =
    { 
      file_name = name; 
      file_sections = [];
      file_rc = []; 
      file_pruned = false;
      
      file_before_save_hook = (fun _ -> ());
      file_after_save_hook = (fun _ -> ());
      file_after_load_hook = (fun _ -> ());
      }
  in
  ignore (file_section file ["Header"] "These options must be read first");
  file
  
let set_options_file opfile name = opfile.file_name <- name
let print_options_not_found = ref false

let define_option_class
  (class_name : string) (from_value : option_value -> 'a)
    (to_value : 'a -> option_value) =
  let c =
    {class_name = class_name; from_value = from_value; to_value = to_value;
     class_hooks = []; string_wrappers = None;}
  in
  c  


let rec find_value list m =
  match list with
    [] -> raise Not_found
  | name :: tail ->
      let m = List.assoc name m in
      match m, tail with
        _, [] -> m
      | Module m, _ :: _ -> find_value tail m
      | _ -> raise Not_found

let find_value list m =
  try find_value list m with
    _ -> raise OptionNotFound

let prune_file file = file.file_pruned <- true

let define_simple_option
    normalp (section : options_section) (option_name : string list) desc
  restart public internal
  (option_help : string) (option_class : 'a option_class)
  (default_value : 'a) (advanced : bool) =
  let desc = match desc with None -> "" | Some s -> s in
  let o =
    { option_name = option_name; option_help = option_help;
      option_class = option_class; option_value = default_value;
      option_default = default_value; 
      option_hooks = []; option_section = section;
      option_restart = (match restart with None -> false | Some v -> v);
      option_public = (match public with None -> false | Some v -> v);
      option_internal = (match internal with None -> false | Some v -> v);
      option_advanced = advanced; option_desc = desc; }
  in
  section.section_options <- 
    section.section_options @ [ (Obj.magic o : Obj.t option_record) ];
  o.option_value <-
    begin try
      o.option_class.from_value (
        find_value option_name section.section_file.file_rc)
    with
      OptionNotFound -> default_value
    | e ->
        lprintf "Options.define_option, for option %s: "
          (match option_name with
            [] -> "???"
          | name :: _ -> name);
        lprintf "%s\n" (Printexc2.to_string e);
        default_value
  end;
  o
  
let define_header_option
  opfile option_name option_help option_class default_value =
  define_simple_option false (List.hd opfile.file_sections)
  option_name None None None None option_help option_class
    default_value false

let define_option opfile option_name ?desc ?restart ?public ?internal option_help option_class default_value =
  define_simple_option true opfile option_name desc restart public internal option_help option_class
    default_value false

let define_expert_option
  opfile option_name ?desc ?restart ?public ?internal option_help option_class default_value =
  define_simple_option true opfile option_name desc restart public internal option_help option_class
    default_value true

  
open Genlex2

let once_values = Hashtbl.create 13
let once_values_counter = ref 0
let once_values_rev = Hashtbl.create 13

let lexer = make_lexer ["="; "{"; "}"; "["; "]"; ";"; "("; ")"; ","; "."; "@"]
      
let rec parse_gwmlrc = parser
| [< id = parse_id; 'Kwd "="; v = parse_option ; 
      eof = parse_gwmlrc >] -> (id, v) :: eof
| [< >] -> []
    
and parse_option = parser
| [< 'Kwd "{"; v = parse_gwmlrc; 'Kwd "}" >] -> Module v
| [< 'Ident s >] -> StringValue s
| [< 'String s >] -> StringValue s
| [< 'Int i >] -> IntValue i
| [< 'Float f >] -> FloatValue  f
| [< 'Kwd "@"; 'Int i; v = parse_once_value i >] -> OnceValue v
| [< 'Char c >] -> StringValue (String.make 1 c)
| [< 'Kwd "["; v = parse_list [] >] -> List v
| [< 'Kwd "("; v = parse_list [] >] -> List v

and parse_once_value i = parser
    [< 'Kwd "@" >] -> 
    begin
      try Hashtbl.find once_values i with Not_found ->
          lprintf "Error in saved file: @%Ld@ not defined\n" i;
          exit 70
    end
|  [< 'Kwd "="; v = parse_option >] ->
    begin
      Hashtbl.add once_values i v;
      v
    end
    
and parse_id = parser
    [< 'Ident s >] -> s
|   [< 'String s >] -> s

and parse_list list = parser
    [< 'Kwd ";"; strm >] -> parse_list (list) strm
|   [< 'Kwd ","; strm >] -> parse_list (list) strm
|   [< 'Kwd "."; strm >] -> parse_list (list) strm
|   [< v = parse_option; strm >] -> parse_list (v :: list) strm
|   [< 'Kwd "]" >] -> List.rev list
|   [< 'Kwd ")" >] -> List.rev list

  
let exec_hooks o =
  List.iter
    (fun f ->
       try f () with
         _ -> ())
    o.option_hooks  

let exec_chooks o =
  List.iter
    (fun f ->
       try f o with
         _ -> ())
    o.option_class.class_hooks  

let count_lines ic pos =
  seek_in ic 0;
  let prev = ref 0 in
  let lines = ref 0 in
  while pos_in ic < pos do
    prev := pos_in ic;
    incr lines;
    ignore (input_line ic);
  done;
  !lines, pos - !prev

let really_load filename sections =
  let temp_file = filename ^ ".tmp" in
  if Sys.file_exists temp_file then
    begin
      lprintf "File %s exists\n" temp_file;
      lprintf "An error may have occurred during previous configuration save.\n";
      lprintf "Please, check your configurations files, and rename/remove this file\n";
      lprintf "before restarting\n";
      exit 70
    end;
  Unix2.tryopen_read filename (fun ic ->
    let s = Stream.of_channel ic in
    try
      let stream = lexer s in
      Hashtbl.clear once_values;
      let list =
        try 
          parse_gwmlrc stream 
        with e ->
          let (line,col) = count_lines ic (Stream.count s) in
          lprintf "Syntax error while parsing file %s at position %d:%d: %s\n"
            filename line col (Printexc2.to_string e);
          lprintf "it seems that %s is corrupt,\n" filename;
          lprintf "try to use a backup from %s\n"
            (Filename.concat (Sys.getcwd ()) "old_config");
          exit 70 in
      Hashtbl.clear once_values;
      let affect_option o =
        try
          (try
            o.option_value <-
              o.option_class.from_value (find_value o.option_name list)
          with SideEffectOption -> ());
          exec_chooks o;
          exec_hooks o
        with
          | SideEffectOption -> ()
          | OptionNotFound ->
              if !print_options_not_found then
                begin
                  lprintf "Option ";
                  List.iter (fun s -> lprintf "%s " s) o.option_name;
                  lprintf "not found in %s\n" filename;
                end
          | e ->
              lprintf "Exception: %s while handling option:"
                (Printexc2.to_string e);
              List.iter (fun s -> lprintf "%s " s) o.option_name;
              lprintf "\n";
              lprintf "  in %s\n" filename;
              lprintf "Aborting\n.";
              exit 70
      in
      
(* The options are affected by sections, from the first defined one to
the last defined one ("defined" in the order of the program execution).
  Don't change this. *)
      List.iter (fun s ->
          List.iter affect_option s.section_options) sections;
      list
    with e ->
      lprintf "Error %s in %s\n" (Printexc2.to_string e) filename;
      [])


let exit_exn = Exit

let safe_string s =
  if s = "" then "\"\""
  else
    try
      match s.[0] with
        'a'..'z' | 'A'..'Z' ->
          for i = 1 to String.length s - 1 do
            match s.[i] with
              'a'..'z' | 'A'..'Z' | '_' | '0'..'9' -> ()
            | _ -> raise exit_exn
          done;
          s
      | _ ->
          if Int64.to_string (Int64.of_string s) = s ||
             string_of_float (float_of_string s) = s
          then
            s
          else raise exit_exn
    with
      _ -> Printf.sprintf "\"%s\"" (String.escaped s)

let with_help = ref false
let save_private = ref false

let tabulate s = String2.replace s '\n' "\n\t"

let rec save_module indent oc list =
  let subm = ref [] in
  List.iter
    (fun (name, help, restart, internal, value) ->
       match name with
         [] -> assert false
       | [name] ->
           if !with_help && help <> "" then
             Printf.fprintf oc "\n\t(* %s *)\n" (tabulate help);
           if restart then
             Printf.fprintf oc "\t(* changing this option requires restart of MLDonkey core *)\n";
           if internal then
             Printf.fprintf oc "\t(* Do not change this option, internal use only! *)\n";
           Printf.fprintf oc "%s %s = " indent (safe_string name);
           save_value indent oc value;
           Printf.fprintf oc "\n"
       | m :: tail ->
           let p =
             try List.assoc m !subm with
               e -> let p = ref [] in subm := (m, p) :: !subm; p
           in
           p := (tail, help, restart, internal, value) :: !p)
    list;
  List.iter
    (fun (m, p) ->
       Printf.fprintf oc "%s %s = {\n" indent (safe_string m);
       save_module (indent ^ "  ") oc !p;
       Printf.fprintf oc "%s}\n" indent)
    !subm
and save_list indent oc list =
  match list with
    [] -> ()
  | [v] -> save_value indent oc v
  | v :: tail ->
      save_value indent oc v; Printf.fprintf oc ", "; save_list indent oc tail
and save_list_nl indent oc list =
  match list with
    [] -> ()
  | [v] -> Printf.fprintf oc "\n%s" indent; save_value indent oc v
  | v :: tail ->
      Printf.fprintf oc "\n%s" indent;
      save_value indent oc v;
      Printf.fprintf oc ";";
      save_list_nl indent oc tail
and save_value indent oc v =
  match v with
    StringValue s -> Printf.fprintf oc "%s" (safe_string s)
  | IntValue i -> Printf.fprintf oc "%s" (Int64.to_string i)
  | FloatValue f -> Printf.fprintf oc "%F" f
  | List l ->
      Printf.fprintf oc "[";
      save_list_nl (indent ^ "  ") oc l;
      Printf.fprintf oc "]"
  | DelayedValue f -> f oc indent
  | SmallList l ->
      Printf.fprintf oc "(";
      save_list (indent ^ "  ") oc l;
      Printf.fprintf oc ")"
  | Module m ->
      Printf.fprintf oc "{";
      save_module_fields (indent ^ "  ") oc m;
      Printf.fprintf oc "}"
  | OnceValue v ->
      try
        let i = Hashtbl.find once_values_rev v in Printf.fprintf oc "@%Ld@" i
      with
        Not_found ->
          incr once_values_counter;
          let i = Int64.of_int !once_values_counter in
          Hashtbl.add once_values_rev v i;
          Printf.fprintf oc "@%Ld = " i;
          save_value indent oc v
and save_module_fields indent oc m =
  match m with
    [] -> ()
  | (name, v) :: tail ->
      Printf.fprintf oc "%s %s = " indent (safe_string name);
      save_value indent oc v;
      Printf.fprintf oc "\n";
      save_module_fields indent oc tail

let options_file_name f = f.file_name

let load opfile =
  (try
    opfile.file_rc <-
      really_load opfile.file_name opfile.file_sections;
  with
  | Not_found | Sys_error _ -> ());
  opfile.file_after_load_hook ()
      
let append opfile filename =
  try
    opfile.file_rc <-
      really_load filename opfile.file_sections @
        opfile.file_rc
  with
    Not_found -> lprintf "No %s found\n" filename
      
let ( !! ) o = o.option_value
let ( =:= ) o v = o.option_value <- v; exec_chooks o; exec_hooks o
    
let value_to_stringoption v =
  match v with
    StringValue s -> if s = "None" then None else Some s
  | _ -> failwith "Not a string option"

let stringoption_to_value v =
  match v with
    None -> StringValue "None"
  | Some s -> StringValue s

let rec value_to_string v =
  match v with
    StringValue s -> s
  | IntValue i -> Int64.to_string i
  | FloatValue f -> string_of_float f
  | OnceValue v -> value_to_string v
  | _ -> failwith "Not a string option"

let safe_value_to_string v =
  match v with
    StringValue s -> s
  | IntValue i -> Int64.to_string i
  | FloatValue f -> string_of_float f
  | OnceValue v -> value_to_string v
  | _ -> "NaS"
      
let string_to_value s = StringValue s
  
let rec value_to_int64 v =
  match v with
    StringValue s -> Int64.of_string s
  | IntValue i -> i
  | FloatValue i -> Int64.of_float i
  | OnceValue v -> value_to_int64 v
  | _ -> failwith "Options: not an int option"

let value_to_int v = Int64.to_int (value_to_int64 v)
let int_to_value i = IntValue (Int64.of_int i)
let int64_to_value i = IntValue i

let percent_to_value i = IntValue (Int64.of_int i)
let value_to_percent v =
  match Int64.to_int (value_to_int64 v) with
    v when v < 0 -> 0
  | v when v > 100 -> 100
  | v -> v

let port_to_value i = IntValue (Int64.of_int i)
let value_to_port v =
  match Int64.to_int (value_to_int64 v) with
  | v when v < 0 -> 2000 + Random.int 40000
  | v when v > 65535 -> 2000 + Random.int 40000
  | v -> v

(* The Pervasives version is too restrictive *)
let bool_of_string s =
  match String.lowercase s with
    "true" -> true
  | "false" -> false
  | "yes" -> true
  | "no" -> false
  | "y" -> true
  | "n" -> false
  | _ -> invalid_arg "bool_of_string"

let rec value_to_bool v =
  match v with
    StringValue s -> bool_of_string s
  | IntValue v when v = Int64.zero -> false
  | IntValue v when v = Int64.one -> true
  | OnceValue v -> value_to_bool v
  | _ -> failwith "Options: not a bool option"
let bool_to_value i = StringValue (string_of_bool i)

let rec value_to_float v =
  match v with
    StringValue s -> float_of_string s
  | FloatValue f -> f
  | OnceValue v -> value_to_float v
  | _ -> failwith "Options: not a float option" 

let float_to_value i = FloatValue i

let rec value_to_string2 v =
  match v with
    List [s1; s2] | SmallList [s1; s2] ->
      value_to_string s1, value_to_string s2
  | OnceValue v -> value_to_string2 v
  | _ -> failwith "Options: not a string2 option"

let string2_to_value (s1, s2) = SmallList [StringValue s1; StringValue s2]

let rec value_to_list v2c v =
  match v with
    List l | SmallList l -> List.rev (List.rev_map v2c l)
  | OnceValue v -> value_to_list v2c v
  | StringValue s ->
      failwith
        (Printf.sprintf "Options: not a list option (StringValue [%s])" s)
  | FloatValue _ -> failwith "Options: not a list option (FloatValue)"
  | IntValue _ -> failwith "Options: not a list option (IntValue)"
  | Module _ -> failwith "Options: not a list option (Module)"
  | DelayedValue _ -> failwith "Options: not a list option (Delayed)"

let rec value_to_hasharray v2c v =
  match v with
    List l ->
      let hash = Array.init 256 (fun _ -> Hashtbl.create 10) in
      List.iter
        (fun a ->
           let (num, md4, peer) = v2c a in Hashtbl.add hash.(num) md4 peer)
        (List.rev l);
      hash
  | OnceValue v -> value_to_hasharray v2c v
  | _ -> failwith (Printf.sprintf "Options: not a list option for list2")

let rec value_to_safelist v2c v =
  match v with
    List l | SmallList l ->
      let rec iter list left =
        match left with
          [] -> list
        | x :: tail ->
            let list =
              try v2c x :: list with
                _ -> list
            in
            iter list tail
      in
      List.rev (iter [] (List.rev l))
  | OnceValue v -> value_to_safelist v2c v
  | StringValue s ->
      failwith
        (Printf.sprintf "Options: not a list option (StringValue [%s])" s)
  | FloatValue _ -> failwith "Options: not a list option (FloatValue)"
  | IntValue _ -> failwith "Options: not a list option (IntValue)"
  | Module _ -> failwith "Options: not a list option (Module)"
  | DelayedValue _ -> failwith "Options: not a list option (Delayed)"

let rec value_to_intmap f v2c v =
  match v with
    List l | SmallList l ->
      let rec iter map left =
        match left with
          [] -> map
        | x :: tail ->
            let map =
              try let v = v2c x in let num = f v in Intmap.add num v map with
                _ -> map
            in
            iter map tail
      in
      iter Intmap.empty l
  | OnceValue v -> value_to_intmap f v2c v
  | StringValue s ->
      failwith
        (Printf.sprintf "Options: not a list option (StringValue [%s])" s)
  | FloatValue _ -> failwith "Options: not a list option (FloatValue)"
  | IntValue _ -> failwith "Options: not a list option (IntValue)"
  | Module _ -> failwith "Options: not a list option (Module)"
  | DelayedValue _ -> failwith "Options: not a list option (Delayed)"

let rec value_to_listiter v2c v =
  match v with
    List l | SmallList l ->
      List.iter
        (fun v ->
           try ignore (v2c v) with
             SideEffectOption -> ())
        l;
      raise SideEffectOption
  | OnceValue v -> value_to_listiter v2c v
  | StringValue s ->
      failwith
        (Printf.sprintf "Options: not a list option (StringValue [%s])" s)
  | FloatValue _ -> failwith "Options: not a list option (FloatValue)"
  | IntValue _ -> failwith "Options: not a list option (IntValue)"
  | Module _ -> failwith "Options: not a list option (Module)"
  | DelayedValue _ -> failwith "Options: not a list option (Delayed)"

let rec convert_list c2v l res =
  match l with
    [] -> List.rev res
  | v :: list ->
      match
        try Some (c2v v) with
          e ->
            lprintf "Exception %s in Options.convert_list\n"
              (Printexc2.to_string e);
            None
      with
        None -> convert_list c2v list res
      | Some v -> convert_list c2v list (v :: res)

let option_to_value c2v o =
  match o with
    None -> StringValue ""
  | Some c -> c2v c

let rec value_to_option v2c v =
  match v with
    StringValue "" -> None
  | OnceValue v -> value_to_option v2c v
  | _ -> Some (v2c v)

let save_delayed_list_value oc indent c2v =
  let indent = indent ^ "  " in
  fun v ->
    try
      let v = c2v v in
      Printf.fprintf oc "\n%s" indent;
      save_value indent oc v;
      Printf.fprintf oc ";"
    with _ -> ()  
        
let list_to_value c2v l =
  DelayedValue
    (fun oc indent ->
       Printf.fprintf oc "[";
       List.iter (save_delayed_list_value oc indent c2v) l;
       Printf.fprintf oc "]")

let intmap_to_value name c2v map =
  DelayedValue
    (fun oc indent ->
       let save = save_delayed_list_value oc indent c2v in
       Printf.fprintf oc "[";
       Intmap.iter (fun _ v -> save v) map;
       Printf.fprintf oc "]")
  
let hasharray_to_value x c2v l =
  DelayedValue
    (fun oc indent ->
       Printf.fprintf oc "[";
       let save = save_delayed_list_value oc indent c2v in
       for i = 0 to Array.length l - 1 do
         Hashtbl.iter (fun a b -> save (0, x, b)) l.(i)
       done;
       Printf.fprintf oc "]")

let smalllist_to_value c2v l = SmallList (convert_list c2v l [])

let value_to_path v =
  List.map Filename2.from_string
    (let rec iter v =
       match v with
         StringValue s -> Filepath.string_to_colonpath s
       | OnceValue v -> iter v
       | List l ->
           List.map
             (fun v ->
                match v with
                  StringValue s -> Filename2.from_string s
                | _ -> failwith "Options: not a path option")
             l
       | _ -> failwith "Options: not path bool option"
     in
     iter v)
  
let path_to_value list =
  StringValue (Filepath.colonpath_to_string (List.map Filename2.to_string list))

let string_option =
  define_option_class "String" value_to_string string_to_value
let color_option = define_option_class "Color" value_to_string string_to_value
let font_option = define_option_class "Font" value_to_string string_to_value
  
let int_option = define_option_class "Int" value_to_int int_to_value
let int64_option = define_option_class "Int64" value_to_int64 int64_to_value
let percent_option = define_option_class "Int" value_to_percent percent_to_value
let port_option = define_option_class "Int" value_to_port port_to_value
  
let bool_option = define_option_class "Bool" value_to_bool bool_to_value
let float_option = define_option_class "Float" value_to_float float_to_value
let path_option = define_option_class "Path" value_to_path path_to_value

let string2_option =
  define_option_class "String2" value_to_string2 string2_to_value

let option_option cl =
  define_option_class (cl.class_name ^ " Option")
    (value_to_option cl.from_value) (option_to_value cl.to_value)

let list_option cl =
  define_option_class (cl.class_name ^ " List") (value_to_list cl.from_value)
    (list_to_value cl.to_value)

let value_to_array from_value a =
  Array.of_list (value_to_list from_value a)
let array_to_value to_value v =
  list_to_value to_value (Array.to_list v)
  
let array_option cl =
  define_option_class (cl.class_name ^ " Array")
    (fun v -> Array.of_list (value_to_list cl.from_value v))
    (fun v -> list_to_value cl.to_value (Array.to_list v))

let hasharray_option x cl =
  define_option_class "Hashtable array" (value_to_hasharray cl.from_value)
    (hasharray_to_value x cl.to_value)

let safelist_option cl =
  define_option_class (cl.class_name ^ " List")
    (value_to_safelist cl.from_value)
    (list_to_value cl.to_value)

let intmap_option f cl =
  define_option_class (cl.class_name ^ " Intmap")
    (value_to_intmap f cl.from_value)
    (intmap_to_value cl.class_name cl.to_value)

let listiter_option cl =
  define_option_class (cl.class_name ^ " List")
    (value_to_listiter cl.from_value)
    (list_to_value cl.to_value)

let smalllist_option cl =
  define_option_class (cl.class_name ^ " List") (value_to_list cl.from_value)
    (smalllist_to_value cl.to_value)

let to_value cl = cl.to_value
let from_value cl = cl.from_value
  
let rec value_to_sum l v =
  match v with
    StringValue s -> List.assoc s l
  | OnceValue v -> value_to_sum l v
  | _ -> failwith "Options: not a sum option"
  
let sum_to_value l v = StringValue (List.assq v l)
  
let sum_option l =
  let ll = List.map (fun (a1, a2) -> a2, a1) l in
  define_option_class "Sum" (value_to_sum l) (sum_to_value ll)

let option_to_value o =
  o.option_name, o.option_help,
  o.option_restart, o.option_internal,
  (try o.option_class.to_value o.option_value with
     e ->
       lprintf "Error while saving option \"%s\": %s\n"
         (try List.hd o.option_name with
            _ -> "???")
         (Printexc2.to_string e);
       StringValue "")

let string_of_string_list list =
  let rec iter s list =
    match list with 
      [] -> s
    | ss :: tail -> 
        iter (Printf.sprintf "%s.%s" s ss) tail
  in
  match list with
    [] -> ""
  | s :: tail -> iter s tail
  
let title_opfile = ref true;;
  
let save opfile =
  opfile.file_before_save_hook ();

  let old_config_dir = "old_config" in
  if not (Sys.file_exists old_config_dir) then Unix.mkdir old_config_dir 0o755;

  let filename = opfile.file_name in
  let old_file = Filename.concat old_config_dir filename in

  try
    Unix2.with_remove (filename ^ ".tmp") begin fun temp_file ->
    Unix2.tryopen_write temp_file (fun oc ->
      (* race! *)
      if !save_private then (try Unix.chmod temp_file 0o600 with _ -> ());
      once_values_counter := 0;
      title_opfile := true;
      Hashtbl.clear once_values_rev;
      let advanced = ref false in
      List.iter (fun s ->
        let options = List.filter (fun o -> 
          if o.option_advanced then advanced := true; 
          not o.option_advanced) s.section_options in
        if options <> [] then begin
          if s.section_name <> [] then begin
            Printf.fprintf oc "\n\n";
            Printf.fprintf oc "    (************************************)\n";
            if !title_opfile then begin
              Printf.fprintf oc "    (*   Never edit options files when  *)\n";
              Printf.fprintf oc "    (*       the daemon is running      *)\n";
              Printf.fprintf oc "    (************************************)\n";
              title_opfile := false;
            end;
            Printf.fprintf oc "    (* SECTION : %s *)\n" (string_of_string_list s.section_name);
            Printf.fprintf oc "    (* %s *)\n" s.section_help;
            Printf.fprintf oc "    (************************************)\n";
            Printf.fprintf oc "\n\n";
          end;
          save_module "" oc (List.map option_to_value options)
        end
      ) opfile.file_sections;
      if !advanced then begin
        Printf.fprintf oc "\n\n\n";
        Printf.fprintf oc "(*****************************************************************)\n";
        Printf.fprintf oc "(*                                                               *)\n";
        Printf.fprintf oc "(*                       ADVANCED OPTIONS                        *)\n";
        Printf.fprintf oc "(*                                                               *)\n";
        Printf.fprintf oc "(*        All the options after this line are for the expert     *)\n";
        Printf.fprintf oc "(*        user. Do not modify them if you are not   sure.        *)\n";
        Printf.fprintf oc "(*                                                               *)\n";
        Printf.fprintf oc "(*****************************************************************)\n";
        Printf.fprintf oc "\n\n\n";
        List.iter (fun s ->
          let options = List.filter (fun o -> o.option_advanced)  
            s.section_options in
          if options = [] then () else let _ = () in
          Printf.fprintf oc "\n\n";
          Printf.fprintf oc "    (************************************)\n";
          
          Printf.fprintf oc "    (* SECTION : %s FOR EXPERTS *)\n" (string_of_string_list s.section_name);
          Printf.fprintf oc "    (* %s *)\n" s.section_help;
          Printf.fprintf oc "    (************************************)\n";
          Printf.fprintf oc "\n\n";
          save_module "" oc (List.map option_to_value options)
        ) opfile.file_sections;
      end;
      if not opfile.file_pruned then
        begin
          let rem = ref [] in
          Printf.fprintf oc "\n(*\n The following options are not used (errors, obsolete, ...) \n*)\n";
          List.iter
            (fun (name, value) ->
              try
                List.iter
                  (fun s ->
                    List.iter
                      (fun o ->
                        match o.option_name with
                            n :: _ -> if n = name then raise Exit
                          | _ -> ())
                      s.section_options)
                  opfile.file_sections;
                rem := (name, value) :: !rem;
                Printf.fprintf oc "%s = " (safe_string name);
                save_value "  " oc value;
                Printf.fprintf oc "\n"
              with
                | Exit -> ()
                | e ->
                    lprintf "Exception %s in Options.save\n"
                      (Printexc2.to_string e);
            )
            opfile.file_rc;
          opfile.file_rc <- !rem
        end;
      flush oc;
      Unix2.fsync (Unix.descr_of_out_channel oc);
      Hashtbl.clear once_values_rev);
    (try
      begin
        try
          Unix2.rename filename old_file
        with Unix.Unix_error(Unix.ENOENT, _, _) -> ();
      end;
      Unix2.rename temp_file filename
     with e ->
        lprintf_nl "[Opt] exception %s while saving %s" (Printexc2.to_string e) filename
    );
    end; (* remove temp_file *)
    opfile.file_after_save_hook ();
  with e -> 
    opfile.file_after_save_hook ();
    raise e
      
let save_with_help opfile =
  with_help := true;
  ( try save opfile with _ -> () );
  with_help := false

let save_with_help_private opfile =
  with_help := true;
  save_private := true;
  begin try save opfile with
    _ -> ()
  end;
  with_help := false;
  save_private := false
  
let option_hook option f = option.option_hooks <- f :: option.option_hooks
  
let class_hook option_class f =
  option_class.class_hooks <- f :: option_class.class_hooks

let rec iter_order f list =
  match list with
    [] -> ()
  | v :: tail -> f v; iter_order f tail
  
let help oc opfile =
  List.iter (fun s ->
      List.iter
        (fun o ->
          Printf.fprintf oc "OPTION \"";
          begin match o.option_name with
              [] -> Printf.fprintf oc "???"
            | [name] -> Printf.fprintf oc "%s" name
            | name :: tail ->
                Printf.fprintf oc "%s" name;
                iter_order (fun name -> Printf.fprintf oc ":%s" name) o.option_name
          end;
          Printf.fprintf oc "\" (TYPE \"%s\"): %s\n   CURRENT: \n"
            o.option_class.class_name o.option_help;
          begin try
              once_values_counter := 0;
              Hashtbl.clear once_values_rev;
              save_value "" oc (o.option_class.to_value o.option_value)
            with
              _ -> ()
          end;
          Printf.fprintf oc "\n")
      s.section_options;
  ) opfile.file_sections;
  flush oc
  
    
let tuple2_to_value (c1, c2) (a1, a2) =
  SmallList [to_value c1 a1; to_value c2 a2]
  
let rec value_to_tuple2 (c1, c2 as cs) v =
  match v with
    List [v1; v2] -> from_value c1 v1, from_value c2 v2
  | SmallList [v1; v2] -> from_value c1 v1, from_value c2 v2
  | OnceValue v -> value_to_tuple2 cs v
  | List l | SmallList l ->
      lprintf "list of %d\n" (List.length l);
      failwith "Options: not a tuple2 list option"
  | _ -> failwith "Options: not a tuple2 option"
  
let tuple2_option p =
  define_option_class "tuple2_option" (value_to_tuple2 p) (tuple2_to_value p)
  
let tuple3_to_value (c1, c2, c3) (a1, a2, a3) =
  SmallList [to_value c1 a1; to_value c2 a2; to_value c3 a3]
let rec value_to_tuple3 (c1, c2, c3 as cs) v =
  match v with
    List [v1; v2; v3] -> from_value c1 v1, from_value c2 v2, from_value c3 v3
  | SmallList [v1; v2; v3] ->
      from_value c1 v1, from_value c2 v2, from_value c3 v3
  | OnceValue v -> value_to_tuple3 cs v
  | _ -> failwith "Options: not a tuple3 option"
      
let tuple3_option p =
  define_option_class "tuple3_option" (value_to_tuple3 p) (tuple3_to_value p)

let tuple4_to_value (c1, c2, c3, c4) (a1, a2, a3, a4) =
  SmallList [to_value c1 a1; to_value c2 a2; to_value c3 a3; to_value c4 a4]
let rec value_to_tuple4 (c1, c2, c3, c4 as cs) v =
  match v with
    List [v1; v2; v3; v4] | SmallList [v1; v2; v3; v4] ->
      from_value c1 v1, from_value c2 v2, from_value c3 v3, from_value c4 v4
  | OnceValue v -> value_to_tuple4 cs v
  | _ -> failwith "Options: not a tuple4 option"
      
let tuple4_option p =
  define_option_class "tuple4_option" (value_to_tuple4 p) (tuple4_to_value p)

      
let value_to_filename v =
  Filename2.from_string
    (match v with
       StringValue s -> s
     | _ -> failwith "Options: not a filename option")
  
let filename_to_value v = StringValue (Filename2.to_string v)
      
let filename_option =
  define_option_class "Filename" value_to_filename filename_to_value

let shortname o = String.concat ":" o.option_name
let get_class o = o.option_class
let get_help o =
  let help = o.option_help in if help = "" then "No Help Available" else help
let advanced o = o.option_advanced

let get_option opfile name =
(*  lprintf "get_option [%s]\n" name;*)
  let rec iter name list sections =
    match list with
    | o :: list -> if o.option_name = name then o else 
          iter name list sections
    | [] ->
        match sections with 
          [] ->
            prerr_endline
              (Printf.sprintf "option [%s] not_found in %s"
                (String.concat ";" name) opfile.file_name);
            raise Not_found
        | s :: tail ->
            iter name s.section_options tail
  in
  iter [name] [] opfile.file_sections
  
  
let set_simple_option opfile name v =
  let o = get_option opfile name in
  begin match o.option_class.string_wrappers with
    None -> o.option_value <- o.option_class.from_value (string_to_value v)
  | Some (_, from_string) -> o.option_value <- from_string v
  end;
  exec_chooks o;
  exec_hooks o
    
let get_simple_option opfile name =
  let o = get_option opfile name in
  match o.option_class.string_wrappers with
    None -> safe_value_to_string (o.option_class.to_value o.option_value)
  | Some (to_string, _) -> to_string o.option_value
  
let set_string_wrappers o to_string from_string =
  o.string_wrappers <- Some (to_string, from_string)

let option_type o = (get_class o).class_name

let once_value v = OnceValue v

let restore_default o = 
  o =:= o.option_default
let set_option_desc o s =
  o.option_desc <- s 
  
module M = struct
    
    type option_info = {
        option_name : string;
        option_shortname : string;
        option_desc : string;
        option_value : string;
        option_help : string;
        option_advanced : bool;
        option_default : string;
        option_type : string;
        option_restart : bool;
        option_public : bool;
        option_internal : bool;
      }
  
  end

let string_of_option_value o v =
  match o.option_class.string_wrappers with
    None ->
      value_to_string (o.option_class.to_value v)
  | Some (to_string, _) -> to_string v

let tuple2_to_value f x =
  let (v1, v2) = f x in
  SmallList [v1; v2]
  
let value_to_tuple2 f x =
  match value_to_list (fun id -> id) x with
    [v1;v2] -> f (v1, v2)
  | _ -> assert false
      
let strings_of_option prefix o =
  match o.option_name with
    [] | _ :: _ :: _ -> failwith "Complex option"
  | [name] ->
      let desc = if o.option_desc = "" then name else o.option_desc in
      {
        M.option_name = Printf.sprintf "%s%s" prefix name;
        M.option_shortname = name;
        M.option_desc = desc;
        M.option_value = string_of_option_value o o.option_value;
        M.option_default = string_of_option_value o o.option_default;
        M.option_advanced = o.option_advanced;
        M.option_help = o.option_help;
        M.option_type = o.option_class.class_name;
        M.option_restart = o.option_restart;
        M.option_public = o.option_public;
        M.option_internal = o.option_internal;
      }
  
let simple_options prefix opfile admin =
  let list = ref [] in
  List.iter (fun s ->
      List.iter
        (fun o ->
          if admin || o.option_public then
          try list := strings_of_option prefix o :: !list  with _ -> ())
      s.section_options)
  opfile.file_sections;
  List.rev !list
  
let simple_args prefix opfile =
  List2.tail_map
    (fun oi ->
       "-" ^ oi.M.option_name,
       Arg.String
         (fun s ->
            lprintf_nl "Setting option %s" oi.M.option_name;
            let name =
              match String2.split oi.M.option_name '-' with
              | [ _ (* ^ "-" = prefix *); name ] -> name (* opfile is corresponding to prefix *)
              | _ -> oi.M.option_name
            in
            set_simple_option opfile name s),
       Printf.sprintf "<string> : \t%s (current: %s)"
         oi.M.option_help oi.M.option_value)
    (simple_options prefix opfile true)

let prefixed_args prefix file =
  List.map
    (fun (s, f, h) ->
       let s = String.sub s 1 (String.length s - 1) in
       Printf.sprintf "-%s:%s" prefix s, f, h)
    (simple_args "" file)
  
let sections file = file.file_sections
let section_name s = string_of_string_list s.section_name
  
let strings_of_section_options prefix s =
  let list = ref [] in
  List.iter
  (fun o ->
      try list := strings_of_option prefix o :: !list  with _ -> ())
  s.section_options;
  List.rev !list

type option_info = M.option_info = {
    option_name : string;
    option_shortname : string;
    option_desc : string;
    option_value : string;
    option_help : string;
    option_advanced : bool;
    option_default : string;
    option_type : string;
    option_restart : bool;
    option_public : bool;
    option_internal : bool;
  }
  
let iter_section f s =
  List.iter f s.section_options
  
let iter_file f file =
  List.iter (iter_section f) file.file_sections
  
let strings_of_option o = strings_of_option "" o
  
let set_after_load_hook file f =
  file.file_after_load_hook <- f
  
let set_after_save_hook file f =
  file.file_after_save_hook <- f
  
let set_before_save_hook file f =
  file.file_before_save_hook <- f
  
