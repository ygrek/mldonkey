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

type modifier = Gdk.Tags.modifier

type handler = {
    cond : (unit -> bool) ;
    cback : (unit -> unit) ;
  } 

type handler_spec = int * Gdk.keysym

let table = Hashtbl.create 13

let int_of_modifier = function
    `SHIFT -> 1
  | `LOCK -> 2
  | `CONTROL -> 4
  | `MOD1 -> 8
  | `MOD2 -> 16
  | `MOD3 -> 32
  | `MOD4 -> 64
  | `MOD5 -> 128
  | `BUTTON1 -> 256
  | `BUTTON2 -> 512
  | `BUTTON3 -> 1024
  | `BUTTON4 -> 2028
  | `BUTTON5 -> 4056
  
let find_handlers t mods key = Hashtbl.find t (mods, key)

let int_of_modifiers l =
  List.fold_left (fun acc -> fun m -> acc + (int_of_modifier m)) 0 l

let key_press w ev =
  let key = GdkEvent.Key.keyval ev in
  let modifiers = GdkEvent.Key.state ev in
  try
    let t = Hashtbl.find table w#get_id in
    let l = find_handlers t (int_of_modifiers modifiers) key in
    let b = ref true in
    List.iter
      (fun h ->
	if h.cond () then
	  (h.cback () ; b := false)
	else
	  ()
      )
      l;
    !b
  with
    Not_found ->
      true

let associate_key_press w = 
  ignore ((w#event#connect#key_press ~callback: (key_press w)) : GtkSignal.id)

let default_modifiers = ref ([] : modifier list)

let set_default_modifiers l = default_modifiers := l

let add1 ?(remove=false) (w : < event : GObj.event_ops ; get_id : int ; ..>) 
    ?(cond=(fun () -> true)) 
    ?(mods= !default_modifiers)
    k callback =
  let t =
    try Hashtbl.find table w#get_id
    with Not_found -> 
      let t = Hashtbl.create 13 in
      Hashtbl.add table w#get_id t;
      associate_key_press w;
      t
  in
  let n_mods = int_of_modifiers mods in
  let new_h = { cond = cond ; cback = callback } in
  let new_l = 
    if remove then 
      (
       Hashtbl.remove t (n_mods, k);
       [new_h]
      )
    else
      try
	let l = find_handlers t n_mods k in
	l @ [new_h]
      with
	Not_found -> [new_h]
  in
  Hashtbl.add t (n_mods, k) new_l

let add (w : < event : GObj.event_ops ; get_id : int ; ..>) 
    ?(cond=(fun () -> true)) 
    ?(mods= !default_modifiers)
    k callback =
  add1 w ~cond ~mods k callback

let add_list (w : < event : GObj.event_ops ; get_id : int ; ..>) 
    ?(cond=(fun () -> true)) 
    ?(mods= !default_modifiers)
    k_list callback =
  List.iter (fun k -> add w ~cond ~mods k callback) k_list

let set (w : < event : GObj.event_ops ; get_id : int ; ..>) 
    ?(cond=(fun () -> true)) 
    ?(mods= !default_modifiers)
    k callback =
  add1 ~remove: true w ~cond ~mods k callback

let set_list (w : < event : GObj.event_ops ; get_id : int ; ..>) 
    ?(cond=(fun () -> true)) 
    ?(mods= !default_modifiers)
    k_list callback =
  List.iter (fun k -> set w ~cond ~mods k callback) k_list
