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

module Queue = struct
    type 'a t = {
        head : (unit -> int * 'a);
        put : (int * 'a -> unit);
        length : (unit -> int);
        take : (unit -> int * 'a);
        iter : ( (int * 'a -> unit) -> unit);
        put_back : (int * 'a -> unit);
        remove : ( (int * 'a) -> unit);
      }
    
    let head t = t.head ()
    let put t x = t.put x
    let iter f t = t.iter f
    let length t = t.length ()
    let take t = t.take ()
    let put_back t e = t.put_back e
    let remove t e = t.remove e
  end

open Queue
type 'a lifo = {
    mutable lifo_size : int;
    mutable lifo_list : 'a list;
  }

let fifo () = 
  let t = Fifo.create () in
  {
    head = (fun _ -> Fifo.head t);
    put = (fun x -> Fifo.put t x);
    length = (fun _ -> Fifo.length t);
    take = (fun _ -> Fifo.take t);
    iter = (fun f -> Fifo.iter (fun x -> f x) t);
    put_back = (fun e -> Fifo.put_back_ele t e);
    remove = (fun e -> Fifo.remove t e);
  }

let lifo () = 
  let t = { lifo_size = 0; lifo_list = [] } in
  {
    head = (fun _ -> match t.lifo_list with 
          [] -> raise Fifo.Empty | x :: _ -> x);
    put = (fun x -> 
        t.lifo_list <- x :: t.lifo_list;
        t.lifo_size <- t.lifo_size +1
    );
    length = (fun _ -> t.lifo_size);
    take = (fun _ -> match t.lifo_list with 
          [] -> raise Fifo.Empty | x :: tail -> 
            t.lifo_list <- tail; t.lifo_size <- t.lifo_size - 1; x); 
    iter = (fun f -> List.iter (fun x -> f x) t.lifo_list);
    put_back = (fun e -> 
        t.lifo_list <- e :: t.lifo_list;
        t.lifo_size <- 1+ t.lifo_size
    );
    remove = (fun e ->
        let removed = ref 0 in
        t.lifo_list <- List.filter (fun ee -> 
            if e = ee then begin
                incr removed;
                false
              end else true) t.lifo_list;
        if !removed > 0 then t.lifo_size <- t.lifo_size - !removed
    );
  }      

module Make(M: sig
      type t 
      val compare : t -> t -> int
    end) = struct
    
    module InsidesSet = Set2.Make (
        struct
          type t = int * M.t
          let compare (t1,s1) (t2,s2) = 
            if s1 == s2 then begin
                0 end else              
            let x = compare t1 t2 in
            if x = 0 then M.compare s1 s2 else x
        end
      )

    let lifo = lifo
    let fifo = fifo
      
    let oldest_first () = 
      let t = ref InsidesSet.empty in
      {
        head = (fun _ -> try InsidesSet.min_elt !t with _ -> raise Fifo.Empty);
        put = (fun x ->  t := InsidesSet.add x !t);
        length = (fun _ -> InsidesSet.cardinal !t);
        take = (fun _ ->
            try 
              let x = InsidesSet.min_elt !t in
              t := InsidesSet.remove x !t;
              x
            with _ -> raise Fifo.Empty);
        iter = (fun f ->
            InsidesSet.iter (fun x -> f x) !t);
        put_back = (fun e -> t := InsidesSet.add e !t);
        remove = (fun e -> t := InsidesSet.remove e !t);
      }
    
    let oldest_last () = 
      let t = ref InsidesSet.empty in
      {
        head = (fun _ ->
            try InsidesSet.max_elt !t with _ -> raise Fifo.Empty);
        put = (fun x ->  t := InsidesSet.add x !t);
        length = (fun _ -> InsidesSet.cardinal !t);
        take = (fun _ ->
            try
              let x = InsidesSet.max_elt !t in
              t := InsidesSet.remove x !t;
              x
            with _ -> raise Fifo.Empty);
        iter = (fun f ->
            InsidesSet.iter (fun x -> f x) !t);
        put_back = (fun e -> t := InsidesSet.add e !t);
        remove = (fun e -> t := InsidesSet.remove e !t);
      }

      (*
    let max_first compare =
      let module InsideSet = Set2.Make(struct
            type t = source
            let compare = compare
          end) in
      let t = ref InsidesSet.empty in
      {
        head = (fun _ -> InsidesSet.max_elt !t);
        put = (fun x ->  t := InsidesSet.add x !t);
        length = (fun _ -> InsidesSet.cardinal !t);
        take = (fun _ ->
            let x = InsidesSet.max_elt !t in
            t := InsidesSet.remove x !t;
            x);
        iter = (fun f ->
            InsidesSet.iter f !t);
        put_back = (fun e -> t := InsidesSet.add e !t);
        }

    let min_first compare =
      let module InsideSet = Set2.Make(struct
            type t = source
            let compare = compare
          end) in
      let t = ref InsidesSet.empty in
      {
        head = (fun _ -> InsidesSet.min_elt !t);
        put = (fun x ->  t := InsidesSet.add x !t);
        length = (fun _ -> InsidesSet.cardinal !t);
        take = (fun _ ->
            let x = InsidesSet.min_elt !t in
            t := InsidesSet.remove x !t;
            x);
        iter = (fun f ->
            InsidesSet.iter f !t);
        put_back = (fun e -> t := InsidesSet.add e !t);
}
  *)
  end

type 'a impl = 'a t = {
    head : unit -> int * 'a;
    put : int * 'a -> unit;
    length : unit -> int;
    take : unit -> int * 'a;
    iter : (int * 'a -> unit) -> unit;
    put_back : int * 'a -> unit;
    remove : (int * 'a -> unit);
  } 
let of_impl x = x

module Workflow = struct 
    
    type 'a workflow = {
        fifo: 'a Queue.t;
        lifo: 'a Queue.t;
        delayed : (int -> bool);
      }
    
    let head t =
      try
        Queue.head t.lifo
      with _ ->
          let (time,x) = Queue.head t.fifo in
          if t.delayed time then raise Not_found;
          time,x
    
    let put t ((time,_) as x)=
      Queue.put (
        if time = 0 then t.lifo else t.fifo) x
    
    let length t = Queue.length t.fifo + Queue.length t.lifo 
    
    let take t  = 
      try
        Queue.take t.lifo
      with _ ->
          let (time,x) = Queue.head t.fifo in
          if t.delayed time then begin
(*              Printf2.lprintf "not ready\n";*)
              raise Not_found;
            end;
          ignore (Queue.take t.fifo);
          time,x
          
    let iter f t =
      Queue.iter f t.lifo;
      Queue.iter f t.fifo

    let put_back = put

    let remove t e =
      Queue.remove t.fifo e;      
      Queue.remove t.lifo e
      
      
  end
  
open Workflow
  
let workflow delayed =
  let t = {
      fifo = fifo ();
      lifo = lifo ();
      delayed = delayed;
    } in
  {
    head = (fun _ -> head t);
    put = (fun x -> put t x);
    length = (fun _ -> length t);
    take = (fun _ -> take t);
    iter = (fun f -> iter f t);
    put_back = (fun x -> put_back t x);
    remove = (fun x -> remove t x);
  }
