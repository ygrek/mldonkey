
(*d This module implements a simple mechanism to handle program options files.
  An options file is defined as a set of [variable = value] lines,
  where value can be a simple string, a list of values (between brackets
or parentheses) or a set of [variable = value] lines between braces.
The option file is automatically loaded and saved, and options are
manipulated inside the program as easily as references. *)

type 'a option_class
(*d The abstract type for a class of options. A class is a set of options
which use the same conversion functions from loading and saving.*)
  
type 'a option_record
(*d The abstract type for an option *)

(*4 Operations on option files *)
  
val filename : string ref
(*d The name of the option file. *)
  
val load : unit -> unit
(*d [load ()] loads the option file. All options whose value is specified
in the option file are updated. *)
  
val append : string -> unit
(*d [append filename] loads the specified option file. All options whose 
value is specified in this file are updated. *)
  
val save : unit -> unit
(*d [save ()] saves all the options values to the option file. *)
val save_with_help : unit -> unit
(*d [save_with_help ()] saves all the options values to the option file,
with the help provided for each option. *)
  
(*4 Creating options *)  
val define_option :
  string list ->  string -> 'a option_class -> 'a -> 'a option_record
val option_hook : 'a option_record -> (unit -> unit) -> unit
    
val string_option : string option_class
val color_option : string option_class
val font_option : string option_class
val int_option : int option_class
val int32_option : int32 option_class
val bool_option : bool option_class
val float_option : float option_class
val path_option : string list option_class
val string2_option : (string * string) option_class
val filename_option : string option_class
  
  (* parameterized options *)
val list_option : 'a option_class -> 'a list option_class
val smalllist_option : 'a option_class -> 'a list option_class
val sum_option : (string * 'a) list -> 'a option_class
val tuple2_option :  
  'a option_class * 'b option_class -> ('a * 'b) option_class
val tuple3_option : 'a option_class * 'b option_class * 'c option_class ->
  ('a * 'b * 'c) option_class

(*4 Using options *)
  
val ( !! ) : 'a option_record -> 'a
val ( =:= ) : 'a option_record -> 'a -> unit

val shortname : 'a option_record -> string
val get_help : 'a option_record -> string  
  
(*4 Creating new option classes *)

val get_class : 'a option_record -> 'a option_class

val class_hook : 'a option_class -> ('a option_record -> unit) -> unit

type option_value =
  Module of option_module
| StringValue of  string
| IntValue of int32
| FloatValue of float
| List of option_value list
| SmallList of option_value list
  
and option_module =
  (string * option_value) list

val define_option_class :
  string -> (option_value -> 'a) -> ('a -> option_value) -> 'a option_class

val to_value : 'a option_class -> 'a -> option_value
val from_value : 'a option_class -> option_value -> 'a
  
val value_to_string : option_value -> string
val string_to_value : string -> option_value
val value_to_int : option_value -> int
val int_to_value : int -> option_value
val value_to_int32 : option_value -> int32
val int32_to_value : int32 -> option_value
val bool_of_string : string -> bool
val value_to_bool : option_value -> bool
val bool_to_value : bool -> option_value
val value_to_float : option_value -> float
val float_to_value : float -> option_value
val value_to_string2 : option_value -> string * string
val string2_to_value : string * string -> option_value
val value_to_list : (option_value -> 'a) -> option_value -> 'a list
val list_to_value : ('a -> option_value) -> 'a list -> option_value
val smalllist_to_value : ('a -> option_value) -> 'a list -> option_value
val value_to_path : option_value -> string list
val path_to_value : string list -> option_value

val set_simple_option : string -> string -> unit
val simple_options : unit -> (string * string) list
val get_simple_option : string -> string
val set_option_hook : string -> (unit -> unit) -> unit