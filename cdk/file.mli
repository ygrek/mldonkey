
val from_string : string -> string -> unit
(*d [input_file filename str] creates a file named [filename] whose content is 
  [str]. *)

val to_string : string -> string
(*d [to_string filename] returns in a string the content of 
 the file [filename]. *)
 
val iter : (string -> unit) -> string -> unit
(*d [iter f filename] read [filename] line by line, applying [f] to each 
one. *)
  
val to_value : string -> 'a
(*d [to_value filename] read a value from [filename] using [input_value] *)
  
val from_value : string -> 'a -> unit
(*d [to_value filename v] write a value [v] to [filename] using
  [output_value] *)
  