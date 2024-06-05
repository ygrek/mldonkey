
val uncompress_string : string -> string
val uncompress_string2 : bytes -> bytes
val compress_string : ?level:int -> bytes -> bytes
val gzip_string : ?level:int -> bytes -> string

val zlib_version_num : unit -> string
