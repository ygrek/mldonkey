exception Empty
type 'a t
val create : unit -> 'a t
val put : 'a t -> 'a -> unit
val take : 'a t -> 'a
val clear : 'a t -> unit
val read : 'a t -> 'a
val empty : 'a t -> bool
val to_list : 'a t -> 'a list
val length : 'a t -> int
val put_back_ele : 'a t -> 'a -> unit
val put_back : 'a t -> 'a list -> unit
