(* Copyright 2004 b8_bavard, INRIA *)
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
open CommonTypes

module GStack :
  sig

    type t
    type index

    exception Empty

    val make : int -> t
    val clear : t -> unit
    val put : float * float -> t -> unit
    val add : float -> t -> unit
    val take : t -> float * float
    val peek : t -> float * float
    val last : t -> float * float
    val copy : t -> t
    val is_empty : t -> bool
    val length : t -> int
    val iter : (float * float -> 'a) -> t -> unit
    val rev_iter : (float * float -> 'a) -> t -> unit
    val iteri : (int -> float * float -> 'a) -> t -> unit
    val rev_iteri : (int -> float * float -> 'a) -> t -> unit
    val map : (float * float -> 'a) -> t -> 'a array
    val rev_map : (float * float -> 'a) -> t -> 'a array
    val mapi : (int -> float * float -> 'a) -> t -> 'a array
    val rev_mapi : (int -> float * float -> 'a) -> t -> 'a array
    val fold_left : ('a -> float * float -> 'a) -> 'a -> t -> 'a
    val fold_right : ('a -> float * float -> 'a) -> 'a -> t -> 'a
    val to_list_lif : t -> (float * float) list
    val to_list_fif : t -> (float * float) list
    val from_list : int -> (float * float) list -> t
    val get_index_first : t -> index option
    val get_index_last : t -> index option
    val iter_next : index -> t -> bool
    val get_at_index : index -> t -> float * float
  end


type graph = {
  mutable quarter : GStack.t;
  mutable hour    : GStack.t;
  mutable halfday : GStack.t;
  mutable day     : GStack.t;
  mutable week    : GStack.t;
  mutable month   : GStack.t;
  mutable year    : GStack.t;
}

type graph_record =
  GraphDownloads
| GraphUploads
| GraphFile of (uid_type * graph_record)

type graph_time =
  GraphQuarter
| GraphHour
| GraphHalfDay
| GraphDay
| GraphWeek
| GraphMonth
| GraphYear
