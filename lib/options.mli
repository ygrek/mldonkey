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

type options_file

exception SideEffectOption

val create_options_file : string -> options_file
val options_file_name : options_file -> string
val set_options_file : options_file -> string -> unit
val prune_file : options_file -> unit  
(*4 Operations on option files *)
  
val load : options_file -> unit
(*d [load ()] loads the option file. All options whose value is specified
in the option file are updated. *)
  
val append : options_file -> string -> unit
(*d [append filename] loads the specified option file. All options whose 
value is specified in this file are updated. *)
  
val save : options_file -> unit
(*d [save ()] saves all the options values to the option file. *)
val save_with_help : options_file -> unit
(*d [save_with_help ()] saves all the options values to the option file,
with the help provided for each option. *)
  
(*4 Creating options *)  
val define_option : options_file ->
  string list ->  string -> 'a option_class -> 'a -> 'a option_record
val define_header_option : options_file ->
  string list ->  string -> 'a option_class -> 'a -> 'a option_record
val option_hook : 'a option_record -> (unit -> unit) -> unit
    
val string_option : string option_class
val color_option : string option_class
val font_option : string option_class
val int_option : int option_class
val int64_option : int64 option_class
val bool_option : bool option_class
val float_option : float option_class
val path_option : string list option_class
val string2_option : (string * string) option_class
val filename_option : string option_class
  
  (* parameterized options *)
val list_option : 'a option_class -> 'a list option_class
val array_option : 'a option_class -> 'a array option_class
val hasharray_option : 'a -> (int * 'a * 'b) option_class -> ('a, 'b) Hashtbl.t array option_class
val safelist_option : 'a option_class -> 'a list option_class
val intmap_option : ('a -> int) -> 'a option_class -> 'a Intmap.t option_class
val listiter_option : 'a option_class -> 'a list option_class
val option_option : 'a option_class -> 'a option option_class
val smalllist_option : 'a option_class -> 'a list option_class
val sum_option : (string * 'a) list -> 'a option_class
val tuple2_option :  
  'a option_class * 'b option_class -> ('a * 'b) option_class
val tuple3_option : 'a option_class * 'b option_class * 'c option_class ->
  ('a * 'b * 'c) option_class
val tuple4_option : 'a option_class * 'b option_class * 'c option_class
   * 'd option_class->
  ('a * 'b * 'c * 'd) option_class

(*4 Using options *)
  
val ( !! ) : 'a option_record -> 'a
val ( =:= ) : 'a option_record -> 'a -> unit

val shortname : 'a option_record -> string
val option_type : 'a option_record -> string
val get_help : 'a option_record -> string  
  
(*4 Creating new option classes *)

val get_class : 'a option_record -> 'a option_class

val class_hook : 'a option_class -> ('a option_record -> unit) -> unit

type option_value =
  Module of option_module
| StringValue of  string
| IntValue of int64
| FloatValue of float
| List of option_value list
| SmallList of option_value list
| OnceValue of option_value 
  
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
val value_to_int64 : option_value -> int64
val int64_to_value : int64 -> option_value
val bool_of_string : string -> bool
val value_to_bool : option_value -> bool
val bool_to_value : bool -> option_value
val value_to_float : option_value -> float
val float_to_value : float -> option_value
val value_to_string2 : option_value -> string * string
val string2_to_value : string * string -> option_value
val value_to_list : (option_value -> 'a) -> option_value -> 'a list
val list_to_value : string -> ('a -> option_value) -> 'a list -> option_value
val smalllist_to_value : string -> ('a -> option_value) -> 'a list -> option_value
val value_to_path : option_value -> string list
val path_to_value : string list -> option_value

val filename_to_value : string -> option_value
val value_to_filename : option_value -> string
  
val set_simple_option : options_file -> string -> string -> unit
val simple_options : options_file -> (string * string) list
val simple_options_html : options_file -> (string * string * string * string) list
val get_simple_option : options_file -> string -> string
val set_option_hook : options_file -> string -> (unit -> unit) -> unit
  
val set_string_wrappers : 'a option_record -> 
  ('a -> string) -> (string -> 'a) -> unit
  
val simple_args : options_file -> (string * Arg.spec * string) list
  
val prefixed_args : 
  string -> options_file -> (string * Arg.spec * string) list

val once_value : option_value -> option_value
val strings_of_option : 'a option_record -> string * string
val strings_of_option_html : 'a option_record  -> string * string * string * string
  
