type 'a  index
and 'a doc

val print : 'a index -> unit
val create : unit -> 'a index
val find_docs : 'a index -> string -> unit
val make_doc : 'a index -> 'a -> 'a doc
type request = (string * int) list
val complex_request :
  'a index -> request -> 'a doc list

  
    
val add : 'a index -> string -> 'a doc -> int -> unit
val value : 'a doc -> 'a

val clear : 'a index -> unit
 