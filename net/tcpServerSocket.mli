type event =
  CONNECTION of Unix.file_descr * Unix.sockaddr
| BASIC_EVENT of BasicSocket.event
  
type t
  
and handler = t -> event -> unit

val sock : t -> BasicSocket.t
val create : int -> handler -> t
val close : t -> string -> unit
  