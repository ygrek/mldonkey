external dump_heap_c: unit -> unit = "heap_dump"
external set_tag : 'a -> int -> unit = "heap_set_tag"

  
let memstat_functions = ref []
  
let add_memstat m f = memstat_functions := (m,f) :: !memstat_functions

let print_memstats buf =  
  Gc.compact ();
  Gc.compact ();

  Printf.bprintf buf "Memory Debug Stats:\n";
  let list = List.rev !memstat_functions in
  List.iter (fun (m,f) ->   
      Printf.bprintf buf "Module %s:\n" m ;      
      f buf) list;
  
    dump_heap_c ()

