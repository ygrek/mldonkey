type event =
    WRITE_DONE
  | CAN_REFILL
  | BUFFER_OVERFLOW
  | READ_DONE of int
  | BASIC_EVENT of BasicSocket.event

and buf = {
  mutable buf : string;
  mutable pos : int;
  mutable len : int;
  mutable max_buf_size : int;
  } 
  
type t
  
and handler = t -> event -> unit

val sock: t -> BasicSocket.t
val create : Unix.file_descr -> handler -> t
val create_simple : Unix.file_descr -> t
val create_blocking : Unix.file_descr -> handler -> t
val buf : t -> buf
val set_reader : t -> (t -> int -> unit) -> unit
val buf_used : t -> int -> unit
val set_handler : t -> event -> (t -> unit) -> unit
val set_refill : t -> (t -> unit) -> unit
val write: t -> string -> int -> int -> unit
val write_string: t -> string -> unit
val connect: Unix.inet_addr -> int -> handler -> t
val close : t -> string -> unit
val shutdown : t -> string -> unit
val error: t -> string
val tcp_handler: t -> BasicSocket.t -> BasicSocket.event -> unit
val set_closer : t -> (t -> string -> unit) -> unit
val nread : t -> int
val set_max_write_buffer : t -> int -> unit  
val can_write : t -> bool  
  
val close_after_write : t -> unit
