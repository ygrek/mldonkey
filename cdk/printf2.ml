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

let cprintf kont fmt =
  let fmt = (Obj.magic fmt : string) in
  let len = String.length fmt in
  let dest = Buffer.create (len + 16) in
  let rec doprn i =
    if i >= len then begin
      let res = Buffer.contents dest in
      Buffer.clear dest;  (* just in case kprintf is partially applied *)
      Obj.magic (kont res)
    end else
    match String.unsafe_get fmt i with
    | '%' -> Printf.scan_format fmt i cont_s cont_a cont_t
    |  c  -> Buffer.add_char dest c; doprn (succ i)
  and cont_s s i =
    Buffer.add_string dest s; doprn i
  and cont_a printer arg i =
    Buffer.add_string dest (printer () arg); doprn i
  and cont_t printer i =
    Buffer.add_string dest (printer ()); doprn i
  in doprn 0
  
let lprintf_handler = ref (fun s -> ())
  
let lprintf fmt = 
  cprintf (fun s -> !lprintf_handler s)
  (fmt : ('a,unit, unit) format )
  
let lprintf_nl fmt = 
  cprintf (fun s -> !lprintf_handler (s^"\n"))
  (fmt : ('a,unit, unit) format )

let lprint_newline () = lprintf "\n"
let lprint_char c = lprintf "%c" c
let lprint_int c = lprintf "%d" c
let lprint_string c = lprintf "%s" c
  
let set_lprintf_handler f =
  lprintf_handler := f
  
let lprintf_max_size = ref 100
let lprintf_size = ref 0
let lprintf_fifo = Fifo.create ()
let lprintf_to_stdout = ref true
  
let _ =
  set_lprintf_handler (fun s -> 
      if !lprintf_to_stdout then begin
          Printf.printf "%s" s; flush stdout;
        end else begin
          if !lprintf_size >= !lprintf_max_size then
            ignore (Fifo.take lprintf_fifo)
          else
            incr lprintf_size;
          Fifo.put lprintf_fifo s 
        end
  )
  
  