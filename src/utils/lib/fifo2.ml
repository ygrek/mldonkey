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

exception Empty;;

type 'a t = { mutable inlist : 'a list; mutable outlist : 'a list };;

let create () = {inlist = []; outlist = []};;
let put t e = 
  t.inlist <- e :: t.inlist

let rec take t =
  match t.outlist with
    e :: queue -> t.outlist <- queue; e
  | [] ->
      t.outlist <- List.rev t.inlist;
      t.inlist <- [];
      match t.outlist with
        e :: queue -> t.outlist <- queue; e
      | [] -> raise Empty
let clear t = 
  t.inlist <- [];
  t.outlist <- []

let read t =
  match t.outlist with
    e :: queue -> e
  | [] ->
      t.outlist <- List.rev t.inlist;
      t.inlist <- [];
      match t.outlist with
        e :: queue -> e
      | [] -> raise Empty

let empty t = (t.inlist == [] && t.outlist == [])

let to_list t =
  t.outlist <- t.outlist @ (List.rev t.inlist);
  t.inlist <- [];
  t.outlist
  
let length t = List.length t.inlist + List.length t.outlist
let put_back t l = t.outlist <- l@t.outlist
