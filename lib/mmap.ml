
type t = {
    mutable fd : Unix.file_descr;
    mutable addr: int option;
    mutable filename : string;
    mutable len : int32;
    mutable pos : int32;
  }

external mmap_open_file : t -> unit = "mmap_mmap"
external truncate : t -> int32 -> unit = "mmap_truncate"
external blit_from_string : string -> int32 -> t -> int32 -> int32 -> unit = "mmap_blit_from_string"
external blit_to_string :  t -> int32 -> string -> int32 -> int32 -> unit = "mmap_blit_to_string"
external munmap : t -> unit = "mmap_munmap"
external mmap_md4_sub : t -> int32 -> int32 -> Md4.t -> unit = "mmap_md4_sub"
external msync : t -> unit = "mmap_msync"
  
let mmap filename fd pos len =
  let t = { fd = fd; 
      addr = None; 
      filename = filename; 
      len = len;
      pos = pos;
      } in
  mmap_open_file t;
  t
  
let md4_sub t pos len =
  let md4 = Md4.create () in
  mmap_md4_sub t pos len md4;
  md4
  
external seek32 : Unix.file_descr -> int32 -> Unix.seek_command -> int32 =
  "ml_lseek32"
external getsize32 : string -> int32 = "ml_getsize32"