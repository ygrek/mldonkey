type 'a t
  
val create: int -> 'a t
val set: 'a t -> int -> 'a -> unit
val get: 'a t -> int -> 'a
  
  