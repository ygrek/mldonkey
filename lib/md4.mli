type t
  
val null : t
val one : t
val to_string : t -> string
val of_string : string -> t
  
val string : string -> t
val file : string -> t
val create : unit -> t
val direct_of_string : string -> t
val direct_to_string : t -> string
val random : unit -> t
  
val digest_subfile : Unix.file_descr -> int32 -> int32 -> t