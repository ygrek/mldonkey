  
open Indexer
  
module FullMake (Doc : Doc) : sig
    
    type   index
    val create : unit ->  index
    
    val add :  index -> string ->  Doc.t -> int -> unit
    
    val clear :  index -> unit
    
    val filter_words :  index -> string list -> unit
    val clear_filter :  index -> unit
    val filtered :  Doc.t -> bool
    val query : index -> Doc.t query -> Doc.t array          
  end
