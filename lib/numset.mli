type 'a t
val create : unit -> 'a t
val add : 'a t -> 'a -> int
val get : 'a t -> int -> 'a
val free : 'a t -> int -> unit
