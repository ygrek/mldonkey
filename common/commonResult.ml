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


open Options
open CommonTypes

let (dummy_result : result) = Obj.magic 0

type 'a result_impl = {
    mutable impl_result_num : int;
    mutable impl_result_val : 'a;
    mutable impl_result_ops : 'a result_ops;
  }
  
and 'a result_ops = {
    mutable op_result_network : network;
    mutable op_result_print : ('a -> int -> CommonTypes.connection_options -> unit);
    mutable op_result_download : ('a -> string list -> unit);
    mutable op_result_info : ('a -> CommonTypes.result_info);
  }
  
let result_counter = ref 0
  
module H = Weak2.Make(struct
      type t = int * result
      let hash (x,_) = Hashtbl.hash x
      
      let equal (x,_) (y,_) = x = y        
    end)
  
let results_by_num = H.create 1027
  
let new_result (result : 'a result_impl) =
  incr result_counter;
  result.impl_result_num <- !result_counter;
  let (result : result) = Obj.magic result in
  H.add results_by_num (!result_counter, result)

  
let ni n m = 
  let s = Printf.sprintf "Result.%s not implemented by %s" 
      m n.network_name in
  print_string s; print_newline ();
  s
  
let fni n m =  failwith (ni n m)
let ni_ok n m = ignore (ni n m)

  
let as_result  (result : 'a result_impl) =
  let (result : result) = Obj.magic result in
  result
  
let as_result_impl  (result : result) =
  let (result : 'a result_impl) = Obj.magic result in
  result

let result_num r =
  (as_result_impl r).impl_result_num
  
let result_info (result : result) =
  let result = as_result_impl result in
  result.impl_result_ops.op_result_info result.impl_result_val
  
let result_download (result : result) =
  let result = as_result_impl result in
  result.impl_result_ops.op_result_download result.impl_result_val
  
let new_result_ops network = {
    op_result_network =  network;
    op_result_download = (fun _ _ -> ni_ok network "result_download");
    op_result_print = (fun _ _ _ -> ni_ok network "result_print");
    op_result_info = (fun _ -> fni network "result_info");
  }

let result_find num = snd (H.find results_by_num (num, dummy_result))

  
let result_print (result : result) buf count =
  ()
  
let results_iter f =
  H.iter (fun (n,r) -> f n r) results_by_num
  