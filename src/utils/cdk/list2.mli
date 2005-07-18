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

val removeq :  'a -> 'a list -> 'a list
(*d [removeq ele list] returns a copy of [list] where all memory occurences
of [ele] have been removed. *)
  
val remove :  'a -> 'a list -> 'a list
(*d [remove ele list] returns a copy of [list] where all structural occurences
of [ele] have been removed. *)

val removeq_first :  'a -> 'a list -> 'a list
(*d [removeq_first ele list] returns a copy of [list] where the first memory
  occurence of [ele] has been removed. *)

val remove_first : 'a ->  'a list -> 'a list
(*d [remove_first ele list] returns a copy of [list] where the first
structural occurence of [ele] has been removed. *)

val cut: int -> 'a list -> 'a list * 'a list
  
val tail_map : ('a -> 'b) -> 'a list -> 'b list
  
val assoc_inv : 'a -> ('b * 'a) list -> 'b
  
val safe_iter : ('a -> unit) -> 'a list -> unit
  
val min : 'a list -> 'a
val max : 'a list -> 'a

val shuffle: 'a list -> 'a list