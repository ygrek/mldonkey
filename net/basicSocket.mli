type event = CLOSED of string | RTIMEOUT | WTIMEOUT | LTIMEOUT | CAN_READ | CAN_WRITE

type t
type handler = t -> event -> unit

val infinite_timeout : float
val last_time : unit -> float

  
val fd : t -> Unix.file_descr
val must_write : t -> bool -> unit
val set_rtimeout : t -> float -> unit
val set_wtimeout : t -> float -> unit
val set_lifetime : t -> float -> unit
val set_handler : t -> handler -> unit
val handler : t -> handler
val closed : t -> bool
  
  
val create : Unix.file_descr -> handler -> t
val create_blocking : Unix.file_descr -> handler -> t
val close : t -> string -> unit
val shutdown : t -> string -> unit

  
(* val add_timer: float -> (unit -> unit) -> unit *)
  
type timer
val add_timer : float -> (timer -> unit) -> unit
val reactivate_timer : timer -> unit
val loop : unit -> unit
