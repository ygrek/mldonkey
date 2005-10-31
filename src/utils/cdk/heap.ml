external dump_heap_c: unit -> unit = "heap_dump"
external set_tag : 'a -> int -> unit = "heap_set_tag"

open Printf2

let memstat_functions = ref []

let add_memstat m f = memstat_functions := (m,f) :: !memstat_functions

let _ = 
  add_memstat "Gc" (fun level buf ->
    let stat = if level > 0 then Gc.stat () else Gc.quick_stat () in
    let ab = (stat.Gc.minor_words +. stat.Gc.major_words -. stat.Gc.promoted_words) 
      *. float_of_int (Sys.word_size / 8) in
    Printf.bprintf buf "minor_words: %.0f\n" stat.Gc.minor_words;
    Printf.bprintf buf "promoted_words: %.0f\n" stat.Gc.promoted_words;
    Printf.bprintf buf "major_words: %.0f\n" stat.Gc.major_words;
    Printf.bprintf buf "minor_collections: %d\n" stat.Gc.minor_collections;
    Printf.bprintf buf "major_collections: %d\n" stat.Gc.major_collections;
    Printf.bprintf buf "heap_words: %d\n" stat.Gc.heap_words;
    Printf.bprintf buf "heap_chunks: %d\n" stat.Gc.heap_chunks;
    if stat.Gc.live_words > 0 then
      Printf.bprintf buf "live_words: %d\n" stat.Gc.live_words;
    if stat.Gc.live_blocks > 0 then
      Printf.bprintf buf "live_blocks: %d\n" stat.Gc.live_blocks;
    if stat.Gc.free_words > 0 then
      Printf.bprintf buf "free_words: %d\n" stat.Gc.free_words;
    if stat.Gc.free_blocks > 0 then
      Printf.bprintf buf "free_blocks: %d\n" stat.Gc.free_blocks;
    if stat.Gc.largest_free > 0 then
      Printf.bprintf buf "largest_free: %d\n" stat.Gc.largest_free;
    if stat.Gc.fragments > 0 then
      Printf.bprintf buf "fragments: %d\n" stat.Gc.fragments;
    Printf.bprintf buf "compactions: %d\n" stat.Gc.compactions;
    Printf.bprintf buf "top_heap_words: %d\n" stat.Gc.top_heap_words;
    Printf.bprintf buf "allocated_bytes: %.0f\n" ab;
  )

let print_memstats (level : int) buf use_html_mods =
  let level = if level < 0 then begin
        Gc.compact (); -level
      end else level in

  let memstat_list = List.rev !memstat_functions in

  if use_html_mods then
    begin

      html_mods_table_header buf "memstats" "memstats" [];
      html_mods_cntr_init ();

      List.iter (fun (m,f) ->

         Printf.bprintf buf "\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr());
         html_mods_td buf [ ("", "srh", m); ];
         Printf.bprintf buf "\\</tr\\>";

         html_mods_cntr_init ();

          let buftmp = Buffer.create 100 in
          f level buftmp;
          let listtmp =  Str.split (Str.regexp "\n") (Buffer.contents buftmp) in
          (List.iter (fun s ->

              Printf.bprintf buf "\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr());
              html_mods_td buf [ ("", "sr", s); ];
              Printf.bprintf buf "\\</tr\\>";

              ) listtmp);

          ) memstat_list;
      Printf.bprintf buf "\\</table\\>\\</div\\>\n";

    end

  else

    begin
      Printf.bprintf buf "Memory Debug Stats:\n";
      List.iter (fun (m,f) ->
          Printf.bprintf buf "\n----------------------------------\n";
          Printf.bprintf buf "  Module %s:\n" m ;      
          Printf.bprintf buf "----------------------------------\n";
          f level buf
      ) memstat_list;
    end;

      dump_heap_c ()
