(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(** Sets over int type. *)

type t
(** The type of sets. *)

val empty: t
(** The empty set. *)

val is_empty: t -> bool
(** Test whether a set is empty or not. *)

val mem: int -> t -> bool
(** [mem x s] tests whether [x] belongs to the set [s]. *)

val add: int -> t -> t
(** [add x s] returns a set containing all elements of [s],
       plus [x]. If [x] was already in [s], [s] is returned unchanged. *)

val singleton: int -> t
(** [singleton x] returns the one-element set containing only [x]. *)

val remove: int -> t -> t
(** [remove x s] returns a set containing all elements of [s],
       except [x]. If [x] was not in [s], [s] is returned unchanged. *)

val union: t -> t -> t
(** Set union. *)

val inter: t -> t -> t
(** Set intersection. *)

(** Set difference. *)
val diff: t -> t -> t

val compare: t -> t -> int
(** Total ordering between sets. Can be used as the ordering function
       for doing sets of sets. *)

val equal: t -> t -> bool
(** [equal s1 s2] tests whether the sets [s1] and [s2] are
       equal, that is, contain equal elements. *)

val subset: t -> t -> bool
(** [subset s1 s2] tests whether the set [s1] is a subset of
       the set [s2]. *)

val iter: (int -> unit) -> t -> unit
(** [iter f s] applies [f] in turn to all elements of [s].
       The order in which the elements of [s] are presented to [f]
       is unspecified. *)

val fold: (int -> 'a -> 'a) -> t -> 'a -> 'a
(** [fold f s a] computes [(f xN ... (f x2 (f x1 a))...)],
       where [x1 ... xN] are the elements of [s].
       The order in which elements of [s] are presented to [f] is
       unspecified. *)

val for_all: (int -> bool) -> t -> bool
(** [for_all p s] checks if all elements of the set
       satisfy the predicate [p]. *)

val exists: (int -> bool) -> t -> bool
(** [exists p s] checks if at least one element of
       the set satisfies the predicate [p]. *)

val filter: (int -> bool) -> t -> t
(** [filter p s] returns the set of all elements in [s]
       that satisfy predicate [p]. *)

val partition: (int -> bool) -> t -> t * t
(** [partition p s] returns a pair of sets [(s1, s2)], where
       [s1] is the set of all the elements of [s] that satisfy the
       predicate [p], and [s2] is the set of all the elements of
       [s] that do not satisfy [p]. *)

val cardinal: t -> int
(** Return the number of elements of a set. *)

val elements: t -> int list
(** Return the list of all elements of the given set.
       The returned list is sorted in increasing order with respect
       to the ordering [Ord.compare], where [Ord] is the argument
       given to {!Set.Make}. *)

val min_elt: t -> int
(** Return the smallest element of the given set
       (with respect to the [Ord.compare] ordering), or raise
       [Not_found] if the set is empty. *)

val max_elt: t -> int
(** Same as {!Set.S.min_elt}, but returns the largest element of the
       given set. *)

val choose: t -> int
(** Return one element of the given set, or raise [Not_found] if
       the set is empty. Which element is chosen is unspecified,
       but equal elements will be chosen for equal sets. *)
  