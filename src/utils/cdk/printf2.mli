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

val syslog_oc : Syslog.t option ref

val lprintf_original_output : out_channel option ref
val keep_console_output : unit -> bool

val log_time : unit -> string

val lprintf :  ('a, unit, string, unit) format4 -> 'a
val lprintf2 :  string -> ('a, unit, string, unit) format4 -> 'a
val lprintf_nl :  ?exn:exn -> ('a, unit, string, unit) format4 -> 'a
val lprintf_nl2 : ?exn:exn -> string -> ('a, unit, string, unit) format4 -> 'a
val lprint_newline : unit -> unit
val lprint_char : char -> unit
val lprint_string : string -> unit
val lprint_int : int -> unit
val lprintf_max_size : int ref

val detach : unit -> unit
val log_to_file : out_channel -> unit
val log_to_buffer : Buffer.t -> unit
val close_log : unit -> unit

type sort_kind = Num (* numeric, parse size suffixes (kMGT) *) | Str (* plain string *)

val html_mods_big_header_start : Buffer.t -> string -> string list -> unit
val html_mods_big_header_end : Buffer.t -> unit
val html_mods_commands : Buffer.t -> string -> string -> (string * string * string * string) list -> unit
val html_mods_table_header : Buffer.t -> ?total:string -> string -> string -> (sort_kind * string * string * string) list -> unit
val html_mods_table_header_colspan : Buffer.t -> ?total:string -> string -> string -> (string * string * string * string * string) list -> unit
val html_mods_table_no_header : Buffer.t -> string -> string -> (string * string * string) list -> unit
val html_mods_table_one_row : Buffer.t -> string -> string -> (string * string * string) list -> unit
val html_mods_table_one_col : Buffer.t -> string -> string -> (string * string * string) list -> unit
val html_mods_td : Buffer.t -> (string * string * string) list -> unit
val html_mods_cntr_init : unit -> unit
val html_mods_cntr : unit -> int
val print_plural_s : int -> string
