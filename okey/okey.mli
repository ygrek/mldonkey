(***********************************************************************)
(*                               Okey                                  *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** Okey interface. *)

type modifier = Gdk.Tags.modifier

(** Set the default modifier list. The first default value is [[]].*)
val set_default_modifiers : modifier list -> unit

(** [add widget key callback] associates the [callback] function to the event
   "key_press" with the given [key] for the given [widget].

   @param remove when true, the previous handlers for the given key and modifier
   list are not kept.
   @param cond this function is a guard: the [callback] function is not called
   if the [cond] function returns [false]. 
   The default [cond] function always returns [true].

   @param mods the list of modifiers. If not given, the default modifiers
   are used. 
   You can set the default modifiers with function {!Okey.set_default_modifiers}.
*)
val add : < event : GObj.event_ops ; get_id : int ; ..> -> 
  ?cond: (unit -> bool) -> 
    ?mods: modifier list -> 
      Gdk.keysym -> 
	(unit -> unit) -> 
	  unit

(** It calls {!Okey.add} for each given key.*)
val add_list : < event : GObj.event_ops ; get_id : int ; ..> -> 
  ?cond: (unit -> bool) -> 
    ?mods: modifier list -> 
      Gdk.keysym list -> 
	(unit -> unit) -> 
	  unit

(** Like {!Okey.add} but the previous handlers for the
   given modifiers and key are not kept.*)
val set : < event : GObj.event_ops ; get_id : int ; ..> -> 
  ?cond: (unit -> bool) -> 
    ?mods: modifier list -> 
      Gdk.keysym -> 
	(unit -> unit) -> 
	  unit

(** It calls {!Okey.set} for each given key.*)
val set_list : < event : GObj.event_ops ; get_id : int ; ..> -> 
  ?cond: (unit -> bool) -> 
    ?mods: modifier list -> 
      Gdk.keysym list -> 
	(unit -> unit) -> 
	  unit
