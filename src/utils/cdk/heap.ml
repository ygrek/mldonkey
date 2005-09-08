external dump_heap_c: unit -> unit = "heap_dump"
external set_tag : 'a -> int -> unit = "heap_set_tag"

open Printf2

let memstat_functions = ref []

let add_memstat m f = memstat_functions := (m,f) :: !memstat_functions

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
