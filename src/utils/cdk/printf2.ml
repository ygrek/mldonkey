(* Copyright 2001, 2002 b8_bavard, b8_fee_carabine, INRIA *)
(*
    This file is part of mldonkey.

    mldonkey is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    mldonkey is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with mldonkey; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)


open Autoconf
open Syslog

let syslog_oc = ref None

external format_int: string -> int -> string = "caml_format_int"
external format_int32: string -> int32 -> string = "caml_int32_format"
external format_nativeint: string -> nativeint -> string = "caml_nativeint_format"
external format_int64: string -> int64 -> string = "caml_int64_format"
external format_float: string -> float -> string = "caml_format_float"

let log_time () =
let t = Unix.localtime (Unix.time ()) in
  let { Unix.tm_year = tm_year; Unix.tm_mon = tm_mon; Unix.tm_mday = tm_mday;
        Unix.tm_hour = tm_hour; Unix.tm_min = tm_min; Unix.tm_sec = tm_sec } = t in
    Printf.sprintf "%4d/%02d/%02d %02d:%02d:%02d " (tm_year+1900) (tm_mon+1) tm_mday tm_hour tm_min tm_sec

let bad_format fmt pos =
  invalid_arg
    ("printf: bad format " ^ String.sub fmt pos (String.length fmt - pos))

(* Format a string given a %s format, e.g. %40s or %-20s.
   To do: ignore other flags (#, +, etc)? *)

let format_string format s =
  let rec parse_format neg i =
    if i >= String.length format then (0, neg) else
    match String.unsafe_get format i with
    | '1'..'9' ->
        (int_of_string (String.sub format i (String.length format - i - 1)),
         neg)
    | '-' ->
        parse_format true (succ i)
    | _ ->
        parse_format neg (succ i) in
  let (p, neg) =
    try parse_format false 1 with Failure _ -> bad_format format 0 in
  if String.length s < p then begin
    let res = String.make p ' ' in
    if neg
    then String.blit s 0 res 0 (String.length s)
    else String.blit s 0 res (p - String.length s) (String.length s);
    res
  end else
    s

(* Extract a %format from [fmt] between [start] and [stop] inclusive.
   '*' in the format are replaced by integers taken from the [widths] list.
   The function is somewhat optimized for the "no *" case. *)

let extract_format fmt start stop widths =
  match widths with
  | [] -> String.sub fmt start (stop - start + 1)
  | _  ->
      let b = Buffer.create (stop - start + 10) in
      let rec fill_format i w =
        if i > stop then Buffer.contents b else
          match (String.unsafe_get fmt i, w) with
            ('*', h::t) ->
              Buffer.add_string b (string_of_int h); fill_format (succ i) t
          | ('*', []) ->
              bad_format fmt start (* should not happen *)
          | (c, _) ->
              Buffer.add_char b c; fill_format (succ i) w
      in fill_format start (List.rev widths)

(* Decode a %format and act on it.
   [fmt] is the printf format style, and [pos] points to a [%] character.
   After consuming the appropriate number of arguments and formatting
   them, one of the three continuations is called:
   [cont_s] for outputting a string (args: string, next pos)
   [cont_a] for performing a %a action (args: fn, arg, next pos)
   [cont_t] for performing a %t action (args: fn, next pos)
   "next pos" is the position in [fmt] of the first character following
   the %format in [fmt]. *)

(* Note: here, rather than test explicitly against [String.length fmt]
   to detect the end of the format, we use [String.unsafe_get] and
   rely on the fact that we'll get a "nul" character if we access
   one past the end of the string.  These "nul" characters are then
   caught by the [_ -> bad_format] clauses below.
   Don't do this at home, kids. *)

let scan_format fmt pos cont_s cont_a cont_t =
  let rec scan_flags widths i =
    match String.unsafe_get fmt i with
    | '*' ->
        Obj.magic(fun w -> scan_flags (w :: widths) (succ i))
    | '0'..'9' | '.' | '#' | '-' | ' ' | '+' -> scan_flags widths (succ i)
    | _ -> scan_conv widths i
  and scan_conv widths i =
    match String.unsafe_get fmt i with
    | '%' ->
        cont_s "%" (succ i)
    | 's' | 'S' as conv ->
        Obj.magic (fun (s: string) ->
          let s = if conv = 's' then s else "\"" ^ String.escaped s ^ "\"" in
          if i = succ pos (* optimize for common case %s *)
          then cont_s s (succ i)
          else cont_s (format_string (extract_format fmt pos i widths) s)
                      (succ i))
    | 'c' | 'C' as conv ->
        Obj.magic (fun (c: char) ->
          if conv = 'c'
          then cont_s (String.make 1 c) (succ i)
          else cont_s ("'" ^ Char.escaped c ^ "'") (succ i))
    | 'd' | 'i' | 'o' | 'x' | 'X' | 'u' ->
        Obj.magic(fun (n: int) ->
          cont_s (format_int (extract_format fmt pos i widths) n) (succ i))
    | 'f' | 'e' | 'E' | 'g' | 'G' ->
        Obj.magic(fun (f: float) ->
          cont_s (format_float (extract_format fmt pos i widths) f) (succ i))
    | 'b' | 'B' ->
        Obj.magic(fun (b: bool) ->
          cont_s (string_of_bool b) (succ i))
    | 'a' ->
        Obj.magic (fun printer arg ->
          cont_a printer arg (succ i))
    | 't' ->
        Obj.magic (fun printer ->
          cont_t printer (succ i))
    | 'l' ->
        begin match String.unsafe_get fmt (succ i) with
        | 'd' | 'i' | 'o' | 'x' | 'X' | 'u' ->
            Obj.magic(fun (n: int32) ->
              cont_s (format_int32 (extract_format fmt pos (succ i) widths) n)
                     (i + 2))
        | _ ->
            bad_format fmt pos
        end
    | 'n' ->
        begin match String.unsafe_get fmt (succ i) with
        | 'd' | 'i' | 'o' | 'x' | 'X' | 'u' ->
            Obj.magic(fun (n: nativeint) ->
              cont_s (format_nativeint
                         (extract_format fmt pos (succ i) widths)
                         n)
                     (i + 2))
        | _ ->
            bad_format fmt pos
        end
    | 'L' ->
        begin match String.unsafe_get fmt (succ i) with
        | 'd' | 'i' | 'o' | 'x' | 'X' | 'u' ->
            Obj.magic(fun (n: int64) ->
              cont_s (format_int64 (extract_format fmt pos (succ i) widths) n)
                     (i + 2))
        | _ ->
            bad_format fmt pos
        end
    | _ ->
        bad_format fmt pos
  in scan_flags [] (pos + 1)

let cprintf kont fmt =
  let fmt = (Obj.magic fmt : string) in
  let len = String.length fmt in
  let dest = Buffer.create (len + 16) in
  let rec doprn i =
    if i >= len then begin
      let res = Buffer.contents dest in
      Buffer.reset dest;  (* just in case kprintf is partially applied *)
      Obj.magic (kont res)
    end else
    match String.unsafe_get fmt i with
    | '%' -> scan_format fmt i cont_s cont_a cont_t
    |  c  -> Buffer.add_char dest c; doprn (succ i)
  and cont_s s i =
    Buffer.add_string dest s; doprn i
  and cont_a printer arg i =
    Buffer.add_string dest (printer () arg); doprn i
  and cont_t printer i =
    Buffer.add_string dest (printer ()); doprn i
  in doprn 0

let lprintf_handler = ref (fun s time ->
      Printf.printf "%sMessage [%s] discarded\n" time s;
  )

let lprintf fmt =
  cprintf (fun s -> try !lprintf_handler "" s with _ -> ())
  (fmt : ('a,unit, unit) format )

let lprintf2 m fmt =
  cprintf (fun s -> try !lprintf_handler (log_time ()) (m^" "^s) with _ -> ())
  (fmt : ('a,unit, unit) format )

let lprintf_nl fmt =
  cprintf (fun s -> try !lprintf_handler (log_time ()) (s^"\n") with _ -> ())
  (fmt : ('a,unit, unit) format )

let lprintf_nl2 m fmt =
  cprintf (fun s -> try !lprintf_handler (log_time ()) (m^" "^s^"\n") with _ -> ())
  (fmt : ('a,unit, unit) format )

let lprint_newline () = lprintf "\n"
let lprint_char = lprintf "%c"
let lprint_int = lprintf "%d"
let lprint_string = lprintf "%s"

let set_lprintf_handler f =
  lprintf_handler := f

let lprintf_max_size = ref 100
let lprintf_size = ref 0
let lprintf_fifo = Fifo.create ()
let lprintf_to_channel = ref true

let lprintf_output = ref (Some stderr)
let lprintf_original_output = ref None

let keep_console_output () =
  match !lprintf_original_output with
    Some c when c = stderr || c = stdout -> true
  | _ -> false

let _ =
  set_lprintf_handler (fun time s ->
      (match !syslog_oc with
	None -> ()
      | Some oc -> Syslog.syslog oc `LOG_INFO s);
      match !lprintf_output with
        Some out when !lprintf_to_channel ->
          Printf.fprintf out "%s" (time ^ s); flush out
      | _ ->
          if !lprintf_size >= !lprintf_max_size then
            ignore (Fifo.take lprintf_fifo)
          else
            incr lprintf_size;
          Fifo.put lprintf_fifo (time ^ s)
  )

let detach () =
  match !lprintf_output with
    Some oc when oc == Pervasives.stdout -> lprintf_output := None
  | _ -> ()

let close_log () =
  lprintf_to_channel := false;
  match !lprintf_output with
    None -> ()
  | Some oc ->
      if oc != stderr && oc != stdout then
        close_out oc;
      lprintf_output := None

let log_to_file oc =
  close_log ();
  lprintf_output := Some oc;
  lprintf_to_channel := true

let log_to_buffer buf =
  try
    while true do
      let s = Fifo.take lprintf_fifo in
      decr lprintf_size;
      Buffer.add_string buf s
    done
  with _ -> ()

(* html_mods *)

let html_mods_commands buf n c l =
    (* Name Class List *)
    Printf.bprintf buf "\\<div class=\\\"%s\\\"\\>\\<table id=\\\"%s\\\"
    name=\\\"%s\\\" class=\\\"%s\\\" cellspacing=0 cellpadding=0
    width=\\\"100pc\\\"\\>\\<tbody\\>\\<tr\\>"
    c n n c;
    List.iter (fun (w,x,y,z)  ->
     (* Class Title Onclick Value *)
     Printf.bprintf buf "\\<td class=\\\"%s\\\"
     title=\\\"%s\\\" onMouseOver=\\\"mOvr(this,'mOvr1');\\\" onMouseOut=\\\"mOut(this);\\\"
     onClick=\\\"%s\\\" \\>%s\\</td\\>"
     w x y z;
    ) l;
    Printf.bprintf buf "\\</tr\\>\\</tbody\\>\\</table\\>\\</div\\>"

(*
  html_mods_commands buf "commandsTable" "commands" [
    ( "bu bbig", "Extend search to more servers and view results", "mSub('output','vr');", "Extend search" ) ;
*)

type sort_kind = Num (* numeric, parse size suffixes (kMGT) *) | Str (* plain string *)

let html_mods_table_header buf ?(total = "0") n c l =
    (* Name Class List *)
    Printf.bprintf buf "\\<div class=\\\"%s\\\"\\>\\<table id=\\\"%s\\\" name=\\\"%s\\\" class=\\\"%s\\\" cellspacing=0 cellpadding=0\\>"
    c n n c;
    if List.length l > 0 then begin
        Printf.bprintf buf "\\<tr\\>";
        List.iter (fun (w,x,y,z)  ->
         let sort = match w with Num -> "1" | Str -> "0" in
         (* Sort Class Title Value *)
         Printf.bprintf buf "\\<td onClick=\\\"_tabSort(this,%s,%s);\\\" class=\\\"%s\\\" title=\\\"%s\\\"\\>%s\\</td\\>"
         sort total x y z;
        ) l;
        Printf.bprintf buf "\\</tr\\>"
      end
(* Add colspan functionality to html_mods_table_header *)

let html_mods_table_header_colspan buf ?(total="0") n c l =
    (* Name Class List *)
    Printf.bprintf buf "\\<div class=\\\"%s\\\"\\>\\<table id=\\\"%s\\\" name=\\\"%s\\\" class=\\\"%s\\\" cellspacing=0 cellpadding=0\\>\\<tr\\>"
    c n n c;
    List.iter (fun (v,w,x,y,z)  ->
     (* Sort Class Title Value *)
     Printf.bprintf buf "\\<td colspan=%s onClick=\\\"_tabSort(this,%s,%s);\\\" class=\\\"%s\\\" title=\\\"%s\\\"\\>%s\\</td\\>"
     v w total x y z;
    ) l;
    Printf.bprintf buf "\\</tr\\>"

let html_mods_table_no_header buf n c l =
    (* 1 row * n cols *)
    (* Name Class List *)
    Printf.bprintf buf "\\<div class=\\\"%s\\\"\\>\\<table id=\\\"%s\\\" name=\\\"%s\\\" class=\\\"%s\\\" cellspacing=0 cellpadding=0\\>\\<tr\\>"
    c n n c;
    List.iter (fun (t,c,d)  ->
    (* Title Class Value *)
     Printf.bprintf buf "\\<td class=\\\"%s\\\" %s\\>%s\\</td\\>"
     c (if t <> "" then "title=\\\"" ^ t ^ "\\\"" else "") d;
    ) l;
    Printf.bprintf buf "\\</tr\\>\\</table\\>\\</div\\>"

let html_mods_table_one_row buf n c l =
    (* 1 row * n cols *)
    (* Name Class List *)
    Printf.bprintf buf "\\<div class=\\\"%s\\\"\\>\\<table id=\\\"%s\\\" name=\\\"%s\\\" class=\\\"%s\\\" cellspacing=0 cellpadding=0\\>\\<tr\\>"
    c n n c;
    List.iter (fun (t,c,d)  ->
    (* Title Class Value *)
     Printf.bprintf buf "\\<td class=\\\"%s\\\" %s\\>%s\\</td\\>"
     c (if t <> "" then "title=\\\"" ^ t ^ "\\\"" else "") d;
    ) l;
    Printf.bprintf buf "\\</tr\\>\\</table\\>\\</div\\>"

let html_mods_table_one_col buf n c l =
    (* n rows * 1 col *)
    (* Name Class List *)
    Printf.bprintf buf "\\<div class=\\\"%s\\\"\\>\\<table id=\\\"%s\\\" name=\\\"%s\\\" class=\\\"%s\\\" cellspacing=0 cellpadding=0\\>\\<tr\\>"
    c n n c;
    List.iter (fun (t,c,d)  ->
    (* Title Class Value *)
     Printf.bprintf buf "\\<tr\\>\\<td class=\\\"%s\\\" %s\\>%s\\</td\\>\\</tr\\>"
     c (if t <> "" then "title=\\\"" ^ t ^ "\\\"" else "") d;
    ) l;
    Printf.bprintf buf "\\</tr\\>\\</table\\>\\</div\\>"

let html_mods_td buf l =
    (* List *)
    List.iter (fun (t,c,d)  ->
    (* Title Class Value *)
     Printf.bprintf buf "\\<td class=\\\"%s\\\" %s\\>%s\\</td\\>"
     c (if t <> "" then "title=\\\"" ^ t ^ "\\\"" else "") d;
    ) l


let html_mods_big_header_start buf c l =
  Printf.bprintf buf "\\<div class=\\\"%s\\\"\\>\\<table class=main border=0 cellspacing=0 cellpadding=0\\>\\<tr\\>\\<td\\>" c;
  Printf.bprintf buf "\\<table cellspacing=0 cellpadding=0 width=100%%\\>\\<tr\\>";
  Printf.bprintf buf "\\<td width=100%%\\>\\</td\\>";
  let len = List.length l in
  let cnt = ref 0 in
  List.iter (fun s ->
    incr cnt;
    Printf.bprintf buf "\\<td nowrap class=\\\"fbig%s\\\"\\>%s\\</td\\>" (if !cnt = len then " pr" else "") s;
  ) l;
  Printf.bprintf buf "\\</tr\\>\\</table\\>\\</td\\>\\</tr\\>\\<tr\\>\\<td\\>"

let html_mods_big_header_end buf =
  Printf.bprintf buf "\\</td\\>\\</tr\\>\\</table\\>\\</div\\>"

let html_mods_counter = ref true

let html_mods_cntr () =
    html_mods_counter := not !html_mods_counter;
    if !html_mods_counter then 1 else 2

let html_mods_cntr_init () =
  html_mods_counter := true

let print_plural_s v =
  if v > 1 then "s" else ""
