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


let syslog_oc = ref None

let log_time () =
  let t = Unix.localtime (Unix.time ()) in
  let { Unix.tm_year; tm_mon; tm_mday; tm_hour; tm_min; tm_sec; _ } = t in
  Printf.sprintf "%4d/%02d/%02d %02d:%02d:%02d " (tm_year+1900) (tm_mon+1) tm_mday tm_hour tm_min tm_sec

let lprintf_handler = ref (fun s time ->
      Printf.printf "%sMessage [%s] discarded\n" time s;
  )

let show_exn exn s =
  match exn with 
  | None -> s^"\n"
  | Some exn -> Printf.sprintf "Exception in %s : %s\n" s (Printexc2.to_string exn)

let lprintf fmt = Printf.ksprintf (fun s -> try !lprintf_handler "" s with _ -> ()) fmt
let lprintf2 m fmt = Printf.ksprintf (fun s -> try !lprintf_handler (log_time ()) (m^" "^s) with _ -> ()) fmt
let lprintf_nl ?exn fmt =
  Printf.ksprintf (fun s -> try !lprintf_handler (log_time ()) (show_exn exn s) with _ -> ()) fmt
let lprintf_nl2 ?exn m fmt =
  Printf.ksprintf (fun s -> try !lprintf_handler (log_time ()) (m^" "^show_exn exn s) with _ -> ()) fmt

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

let () =
  set_lprintf_handler (fun time s ->
      (match !syslog_oc with
      | None -> ()
      | Some oc -> Syslog.syslog oc `LOG_INFO s);
      match !lprintf_output with
      | Some out when !lprintf_to_channel ->
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
    Printf.bprintf buf "\\<div class=\\\"%s\\\"\\>\\<table id=\\\"%s\\\" \
    name=\\\"%s\\\" class=\\\"%s\\\" cellspacing=0 cellpadding=0 \
    width=\\\"100pc\\\"\\>\\<tbody\\>\\<tr\\>"
    c n n c;
    List.iter (fun (w,x,y,z)  ->
     (* Class Title Onclick Value *)
     Printf.bprintf buf "\\<td class=\\\"%s\\\" \
     title=\\\"%s\\\" onMouseOver=\\\"mOvr(this,'mOvr1');\\\" onMouseOut=\\\"mOut(this);\\\" \
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
