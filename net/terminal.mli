
type command =
  | Message of string
  | Command of string

 val init : unit -> unit
 val set_prompt : string -> unit 
 val set_header : string list -> unit  (* the first lines of the terminal *)
 val set_trailer : string list -> unit      (* the last line before the entry point *)
 val set_reader : (command -> unit) -> unit 
   (* param: what has been typed, return: what should remain *)
val print : string -> unit
val update : unit -> unit

