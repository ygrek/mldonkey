  
external seek32 : Unix.file_descr -> int32 -> Unix.seek_command -> int32 =
  "ml_lseek32"
external getsize32 : string -> int32 = "ml_getsize32"
external ftruncate32 : Unix.file_descr -> int32 -> unit = "ml_truncate32"
  
