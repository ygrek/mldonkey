type t

val mmap : string -> Unix.file_descr -> int32 -> int32 -> t
val truncate : t -> int32 -> unit
val blit_from_string : string -> int32 -> t -> int32 -> int32 -> unit
val blit_to_string :  t -> int32 -> string -> int32 -> int32 -> unit
val munmap : t -> unit
val md4_sub : t -> int32 -> int32 -> Md4.t
val msync: t -> unit

external seek32 : Unix.file_descr -> int32 -> Unix.seek_command -> int32 =
  "ml_lseek32"
external getsize32 : string -> int32 = "ml_getsize32"