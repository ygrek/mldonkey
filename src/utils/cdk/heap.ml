external dump_heap_c: unit -> unit = "heap_dump"
external set_tag : 'a -> int -> unit = "heap_set_tag"

(* open CommonGlobals *)
(* open CommonTypes *)

let memstat_functions = ref []

let add_memstat m f = memstat_functions := (m,f) :: !memstat_functions

let print_memstats (level : int) buf output_type =
  let level = if level < 0 then begin
        Gc.compact (); -level
      end else level in

  if output_type = true then
    begin
      Printf.bprintf buf "\\<div class=results\\>";
(*
      html_mods_table_header buf "memstatsTable" "memstats" [];
      html_mods_td buf [
        ("", "srh", "Memory Debug Stats"); ];
*)
      Printf.bprintf buf "\\<div class=\\\"results\\\"\\>
\\<table id=\\\"memstatsTable\\\" name=\\\"memstatsTable\\\" class=\\\"results\\\" cellspacing=0 cellpadding=0\\>
\\<tr\\>\\<td class=\\\"srh\\\" \\>Memory Debug Stats\\</td\\>";
      Printf.bprintf buf "\\</tr\\>\\</table\\>\\</div\\>\n"
    end
  else
  Printf.bprintf buf "Memory Debug Stats:\n";
  let memstat_list = List.rev !memstat_functions in
  if output_type = true then
    begin
      let split_l s c =
        let len = String.length s in
        let rec iter pos =
          try
            if pos = len then [""] else
            let pos2 = String.index_from s pos c in
            if pos2 = pos then "" :: iter (pos+1) else
              (String.sub s pos (pos2-pos)) :: (iter (pos2+1))
          with _ -> [String.sub s pos (len-pos)]
        in
        iter 0
      in
(*
      let split_string c str =
        let rec aux s acc =
          try  let ind=String.index s c in
                 aux (String.sub s (ind+1) ((String.length s) - ind -1 ))
                     ((String.sub s 0 ind)::acc)
          with Not_found -> List.rev (s::acc)
          in aux str []; in
      let split_lines = split_string '\n' in
 *)
      List.iter (fun (m,f) ->
(*
          html_mods_table_header buf "memstatsTable" "memstats" [
            ( "0", "srh", "Module", m ); ];
*)
          Printf.bprintf buf "\\<div class=\\\"results\\\"\n\\>\\<table id=\\\"memstatsTable\\\" name=\\\"memstatsTable\\\" class=\\\"results\\\" cellspacing=0 cellpadding=0
\\>\\<tr\\>\\<td class=\\\"srh\\\" \\>Module %s\\</td\\>\\</tr\\>" m;
          let buftmp = Buffer.create 100 in
          f level buftmp;
          let listtmp = split_l (Buffer.contents buftmp) '\n' in
          (List.iter (fun s ->
(*
              html_mods_td buf [
                ("", "srh", s); ];
*)
              Printf.bprintf buf "\\<tr class=\\\"dl-1\\\"\\>\\<td class=\\\"sr\\\"\\>";
              Printf.bprintf buf "%s" s;
              Printf.bprintf buf "\\</td\\>\\</tr\\>";
              ) listtmp);

          Printf.bprintf buf "\\</table\\>\\</div\\>\n";
          ) memstat_list;

      Printf.bprintf buf "\\</div\\>\n";
      dump_heap_c ()
    end
  else
    begin
      List.iter (fun (m,f) ->
          Printf.bprintf buf "\n----------------------------------\n";
          Printf.bprintf buf "  Module %s:\n" m ;      
          Printf.bprintf buf "----------------------------------\n";
          f level buf) memstat_list;

      dump_heap_c ()
    end;
