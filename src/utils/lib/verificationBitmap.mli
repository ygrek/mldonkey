type t
type part_state = 
    State_missing | State_partial | State_complete | State_verified

val init : int -> (int -> part_state) -> t
val create : int -> part_state -> t
val get : t -> int -> part_state
val set : t -> int -> part_state -> unit
val length : t -> int
  
val iteri : (int -> part_state -> unit) -> t -> unit
val mapi : (int -> part_state -> 'a) -> t -> 'a array
val fold_lefti : ('a -> int -> part_state -> 'a) -> 'a -> t -> 'a
val existsi : (int -> part_state -> bool) -> t -> bool
val for_all : (part_state -> bool) -> t -> bool
  
val to_string : t -> string
val of_string : string -> t
val state_to_char : part_state -> char
