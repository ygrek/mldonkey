
val removeq :  'a -> 'a list -> 'a list
(*d [removeq ele list] returns a copy of [list] where all memory occurences
of [ele] have been removed. *)
  
val remove :  'a -> 'a list -> 'a list
(*d [remove ele list] returns a copy of [list] where all structural occurences
of [ele] have been removed. *)

val removeq_first :  'a -> 'a list -> 'a list
(*d [removeq_first ele list] returns a copy of [list] where the first memory
  occurence of [ele] has been removed. *)

val remove_first : 'a ->  'a list -> 'a list
(*d [remove_first ele list] returns a copy of [list] where the first
structural occurence of [ele] has been removed. *)

val cut: int -> 'a list -> 'a list * 'a list
  