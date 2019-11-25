(* Copyright 2005 beedauchon *)
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

open CommonOptions
open Printf2

module type Graphics =
sig
  val do_draw_pic :
    string -> string -> string -> int Fifo.t -> int Fifo.t -> unit
  val do_draw_down_pic :
    string -> string -> string -> string -> int Fifo.t -> unit
  val do_draw_up_pic :
    string -> string -> string -> string -> int Fifo.t -> unit
  val do_draw_h_pic :
    string -> string -> string -> int Fifo.t -> int Fifo.t -> unit
  val do_draw_down_h_pic :
    string -> string -> string -> string -> int Fifo.t -> unit
  val do_draw_up_h_pic :
    string -> string -> string -> string -> int Fifo.t -> unit
  val do_draw_tag : string -> int Fifo.t -> int Fifo.t -> unit
  val really_remove_files : unit -> unit
  val remove_files : unit -> unit
  val png_version_num : unit -> string
end


module Graphics : Graphics = struct

let show_nogd_debug_msg () =
  if !verbose then lprintf_nl "Warning: Gd support was not compiled (install libgd-dev to enable it)"

let remove_files () =
(
  show_nogd_debug_msg ()
)

let really_remove_files () = ()

(* main *)

let do_draw_pic ttl vl hl gdown gup =
(
  show_nogd_debug_msg ()
)

let do_draw_down_pic ttl top_title vl hl gdown =
(
  show_nogd_debug_msg ()
)

let do_draw_up_pic ttl top_title vl hl gup =
(
  show_nogd_debug_msg ()
)

let do_draw_h_pic ttl vl hl gdown gup =
(
  show_nogd_debug_msg ()
)

let do_draw_down_h_pic ttl top_title vl hl gdown =
(
  show_nogd_debug_msg ()
)

let do_draw_up_h_pic ttl top_title vl hl gup =
(
  show_nogd_debug_msg ()
)

let do_draw_tag title gdown gup =
(
  show_nogd_debug_msg ()
)

let png_version_num () = ""

end
