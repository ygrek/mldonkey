(* The interface *)

type t

val create : string -> int64 -> t
val get_chunk : t -> int64 -> int64 -> (Unix.file_descr * int64)
val build : t -> unit
