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
val buf : t -> buf
val set_reader : t -> (t -> int -> unit) -> unit
val buf_used : t -> int -> unit
val set_handler : t -> event -> (t -> unit) -> unit
val set_refill : t -> (t -> unit) -> unit
val write: t -> string -> int -> int -> unit