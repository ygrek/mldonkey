(***********************************************************************)
(*                               MLChat                                *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** A class for the state. *)

open Chat_types
open Chat_proto

let (!!) = Chat_options.(!!)


class data pred (conf : Chat_config.config) (com : Chat_proto.com) =
  object
    val mutable people = ([] : (id * host * port * state * bool) list)
	(** list of known people : 
	   id * host * port * connected/not_connected * temporary or friend *)

    method people = people

    method pred = pred

    (** merge current people list with [conf#people].*)
    method update_people =
      let l = conf#people in
      let rec iter = function
	  [] -> []
	| (id,host,port,s,t) :: q ->
	    let b = List.exists (pred (id,host,port)) l in
	    (id,host,port,s,not b) :: (iter q)
      in
      let l2 = iter people in
      let rec iter2 = function
	  [] -> []
	| (id,host,port) :: q ->
	    if List.exists 
		(fun(i,h,p,_,_) -> pred (id,host,port) (i,h,p))
		l2
	    then iter2 q
	    else (id,host,port,Not_connected,false) :: (iter2 q)
      in
      people <- l2 @ (iter2 l)

    method set_connected id host port =
      let rec iter = function
	  [] -> [id, host, port, Connected, true]
	| ((i,h,p,s,t) as a) :: q ->
	    if pred (id,host,port) (i,h,p) then
	      (id,h,port,Connected,t) :: q
	    else
	      a :: (iter q)
      in
      people <- iter people

    method set_not_connected id host port =
      let rec iter = function
	  [] -> []
	| ((i,h,p,s,t) as a) :: q ->
	    if pred (id,host,port) (i,h,p) then
	      (id,h,port,Not_connected,t) :: q
	    else
	      a :: (iter q)
      in
      people <- iter people

    method add_people id host port =
      if List.exists (pred (id,host,port)) conf#people
      then
	()
      else
	(
	 let rec iter = function
	    [] -> []
	  | ((i,h,p,s,t) as a) :: q ->
	      if pred (id,host,port) (i,h,p) then
		(id,h,port,s,false) :: q
	      else
		a :: (iter q)
	 in
	 people <- iter people;
	 conf#set_people (conf#people @ [id,host,port]);
	 conf#save
	)

    method com = com
    method conf = conf

    initializer
      people <- List.map (fun (i,h,p) -> (i,h,p,Not_connected,false)) 
	  conf#people
  end
