(* xml light - (c)2002 Nicolas Cannasse		*)
(* http://warplayer.free.fr					*)
(* read LICENCE for additional informations *)

(* the full type description : *)
type xml = XML of string * (string * string) list * xml list

(* error on parsing *)
exception Xml_error of string

val parse_file : string -> xml
val parse_string : string -> xml

(* convenient tools *)
val get_name : xml -> string
val get_attrib : string -> xml -> string (* or raise Not_found *)
val get_childs : xml -> xml list

(* compact text *)
val to_string : xml -> string

