type t
  
val of_inet_addr : Unix.inet_addr -> t
val of_string : string -> t
val of_ints : int * int * int * int -> t

val to_inet_addr : t -> Unix.inet_addr
val to_string : t -> string
val to_ints : t -> int * int * int * int

val to_fixed_string : t -> string

val valid : t -> bool

val resolve_one : t -> string
  