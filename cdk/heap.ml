external dump_heap_c: unit -> unit = "heap_dump"
external set_tag : 'a -> int -> unit = "heap_set_tag"
  
let dumpers = ref []
  
let register_dumper m f = 
  dumpers := (m,f) :: !dumpers
  
let dump_usage () =
  Gc.compact ();
  Gc.compact ();
  List.iter (fun (m,f) ->
      Printf.printf "Module %s:" m; print_newline ();
      f ()
  ) !dumpers
    
let dump_heap () =
  dump_usage ();
  dump_heap_c ()
  