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

(*
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
  
    *)

exception Empty;;

type 'a t = { 
    mutable empty : bool;
    mutable inpos : int;
    mutable outpos : int;
    mutable array : 'a array;
    mutable size : int; (* bit Mask *)
  }
  

let create () = {
    empty = true;
    inpos = 0;
    outpos = 0;
    array = Array.create 4 (Obj.magic ());
    size = 3;
  }

let iter f t =
  if not t.empty then 
    if t.inpos > t.outpos then 
      for i = t.outpos to t.inpos - 1 do
        f t.array.(i)
      done
    else begin
        for i = t.outpos to t.size do
          f t.array.(i)
        done;
        for i = 0 to t.inpos - 1 do
          f t.array.(i)
        done        
      end

let mem t v =
  try
    if not t.empty then 
      if t.inpos > t.outpos then 
        for i = t.outpos to t.inpos - 1 do
          if t.array.(i) = v then raise Exit;
        done
      else begin
          for i = t.outpos to t.size do
            if t.array.(i) = v then raise Exit
          done;
          for i = 0 to t.inpos - 1 do
            if t.array.(i) = v then raise Exit
          done        
        end;
      false
  with _ -> true
  
let realloc t =
  let len = Array.length t.array in
  let tab = Array.create (2*len) t.array.(0) in
  let start = len - t.inpos in
  Array.blit t.array t.inpos tab 0 start;
  Array.blit t.array 0 tab start (len - start);
  t.array <- tab;
  t.outpos <- 0;
  t.inpos <- len;
  t.size <- t.size * 2 + 1
  
let put t e = 
(*  lprintf "FIFO PUT"; lprint_newline (); *)
  if t.inpos = t.outpos && not t.empty then realloc t;
  t.array.(t.inpos) <- e;
  t.inpos <- (t.inpos + 1) land t.size;
  t.empty <- false;
(*
lprintf "FIFO NOT EMPTY %s" (string_of_bool t.empty); lprint_newline ();
*)
  ()

let take t =
(*  lprintf "FIFO TAKE"; lprint_newline (); *)
  if t.empty then raise Empty;
  let e = t.array.(t.outpos) in
  t.outpos <- (t.outpos + 1) land t.size;
  t.empty <- (t.outpos = t.inpos);
  e
  
let clear t = 
(*  lprintf "FIFO CLEAR"; lprint_newline (); *)
  t.empty <- true;
  t.inpos <- 0;
  t.outpos <- 0

let head t =
  if t.empty then raise Empty;
  t.array.(t.outpos)

let empty t = 
(*  lprintf "FIFO EMPTY %s" (string_of_bool t.empty); lprint_newline (); *)
  t.empty

let to_list t =
  if t.empty then [] else
  if t.inpos > t.outpos then
    let len = t.inpos - t.outpos in
    let tab = Array.create len t.array.(0) in
    Array.blit t.array t.outpos tab 0 len;
    Array.to_list tab
  else
  let s = Array.length t.array in
  let len = s + t.inpos - t.outpos in
  let tab = Array.create len t.array.(0) in
  Array.blit t.array t.outpos tab 0 (s - t.outpos);
  Array.blit t.array 0 tab (s - t.outpos) t.inpos;
  Array.to_list tab

let to_array t =
  if t.empty then [||] else
  if t.inpos > t.outpos then
    let len = t.inpos - t.outpos in
    let tab = Array.create len t.array.(0) in
    Array.blit t.array t.outpos tab 0 len;
    tab
  else
  let s = Array.length t.array in
  let len = s + t.inpos - t.outpos in
  let tab = Array.create len t.array.(0) in
  Array.blit t.array t.outpos tab 0 (s - t.outpos);
  Array.blit t.array 0 tab (s - t.outpos) t.inpos;
  tab

let length t = 
(*  lprintf "FIFO LEN"; lprint_newline (); *)
  if t.empty then 0 else
  if t.inpos > t.outpos then t.inpos - t.outpos else
  let s = Array.length t.array in
  s + t.inpos - t.outpos 
  
let put_back_ele t e = 
  if t.inpos = t.outpos && not t.empty then realloc t;
  t.outpos <- (t.outpos - 1) land t.size;
  t.array.(t.outpos) <- e;
  t.empty <- false
  
  
let rec put_back t list = 
  match list with
    [] -> ()
  | ele :: tail ->
      put_back t tail; put_back_ele t ele

let reformat t =
  if not t.empty then begin
      let s = Array.length t.array in
      let len = s + t.inpos - t.outpos in
      let tab = Array.create s t.array.(0) in
      Array.blit t.array t.outpos tab 0 (s - t.outpos);
      Array.blit t.array 0 tab (s - t.outpos) t.inpos;
      t.array <- tab;
      t.inpos <- len;
      t.outpos <- 0;
    end
    
let remove t e =
  if not t.empty then begin
      if t.outpos >= t.inpos then reformat t;
      let rec iter t i j =
(*        Printf.printf "i=%d j=%d inpos=%d outpos=%d\n"
          i j t.inpos t.outpos; print_newline (); *)
        if i >= t.inpos then
          (if i > j then begin
                t.inpos <- j;
                t.empty <- (t.inpos = t.outpos);
              end)
        else
        let ee = t.array.(i) in
        if e = ee then
          iter t (i+1) j
        else begin
            if i > j then begin
(*                Printf.printf "Move i=%d at j=%d" i j; print_newline ();  *)
                t.array.(j) <- ee;
              end;
            iter t (i+1) (j+1)
          end
      in
      iter t t.outpos t.outpos
    end

(* TEST SUITE
  
let t = Fifo.create ();;

for i = 0 to 100 do
  Fifo.put t i
done;;

for i = 0 to 80 do
  Fifo.put t (Fifo.take t)
done;;

Fifo.length t;;

for i = 56 to 76 do
  Fifo.remove t i
done
;;

while true do
  Printf.printf "%d\n" (Fifo.take t)
done;;

*)