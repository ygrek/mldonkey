type t
  
val local_ip : Unix.inet_addr
val local_host : t
val from_name : string -> t
val from_ints : int -> int -> int -> int -> t
val from_ip : Unix.inet_addr -> t
val name : t -> string
val ip : t -> Unix.inet_addr
