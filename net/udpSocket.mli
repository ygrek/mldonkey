type event =
    WRITE_DONE
  | CAN_REFILL
  | READ_DONE
  | BASIC_EVENT of BasicSocket.event
type udp_packet = { content : string; addr : Unix.sockaddr; } 
type t = {
  mutable sock : BasicSocket.t;
  mutable rlist : udp_packet list;
  mutable wlist : udp_packet list;
  mutable event_handler : handler;
} 
and handler = t -> event -> unit
val read : t -> udp_packet
val set_handler : t -> event -> (t -> unit) -> unit
val set_refill : t -> (t -> unit) -> unit
val set_reader : t -> (t -> unit) -> unit
val sock : t -> BasicSocket.t
val closed : t -> bool
val close : t -> string -> unit
val write : t -> string -> int -> int -> Unix.sockaddr -> unit
val create : int -> handler -> t
