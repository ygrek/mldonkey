external dump_heap_c: unit -> unit = "heap_dump"
external set_tag : 'a -> int -> unit = "heap_set_tag"
  
let dumpers = ref []
  
let register_dumper m f = 
  dumpers := (m,f) :: !dumpers
  
let dump_heap () =
  Gc.compact ();
  Gc.compact ();
  dump_heap_c ();
  List.iter (fun (m,f) ->
      Printf.printf "Module %s:" m; print_newline ();
      f ()
  ) !dumpers
  