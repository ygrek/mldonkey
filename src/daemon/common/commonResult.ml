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

open Printf2
open Options
open CommonTypes

let (dummy_result : result) = Obj.magic 0

type 'a result_impl = {
    mutable impl_result_update : int;
    mutable impl_result_num : int;
    mutable impl_result_val : 'a;
    mutable impl_result_ops : 'a result_ops;
  }
  
and 'a result_ops = {
    mutable op_result_network : network;
    mutable op_result_download : ('a -> string list -> bool -> unit);
    mutable op_result_info : ('a -> CommonTypes.result_info);
  }
  
let result_counter = ref 0

let as_result  (result : 'a result_impl) =
  let (result : result) = Obj.magic result in
  result
  
let as_result_impl  (result : result) =
  let (result : 'a result_impl) = Obj.magic result in
  result

let result_num r =
  (as_result_impl r).impl_result_num

let dummy_result_impl = {
    impl_result_update = 1;
    impl_result_num = 0;
    impl_result_val = 0;
    impl_result_ops = Obj.magic None;
  }
  
let dummy_result = as_result dummy_result_impl
  
module H = Weak2.Make(struct
      type t = result
      let hash x = Hashtbl.hash (result_num x)
      
      let equal x y = (result_num x) = (result_num y)
    end)
  
let results_by_num = H.create 1027
    
let _ = 
  Heap.add_memstat "CommonResult" (fun buf ->
      let counter = ref 0 in
      H.iter (fun _ -> incr counter) results_by_num;
      Printf.bprintf buf "  results: %d\n" !counter;
  )

let new_result (result : 'a result_impl) =
  incr result_counter;
  result.impl_result_num <- !result_counter;
  let (result : result) = Obj.magic result in
  H.add results_by_num result

  
let ni n m = 
  let s = Printf.sprintf "Result.%s not implemented by %s" 
      m n.network_name in
  lprint_string s; lprint_newline ();
  s
  
let fni n m =  failwith (ni n m)
let ni_ok n m = ignore (ni n m)

    
let result_info (result : result) =
  let result = as_result_impl result in
  result.impl_result_ops.op_result_info result.impl_result_val
  
let result_download (result : result) list force =
  let result = as_result_impl result in
  result.impl_result_ops.op_result_download result.impl_result_val list force

let results_ops = ref []
  
let new_result_ops network = 
  let r = {
      op_result_network =  network;
      op_result_download = (fun _ _ _ -> ni_ok network "result_download");
      op_result_info = (fun _ -> fni network "result_info");
    }
  in
  let rr = (Obj.magic r : int result_ops) in
  results_ops := (rr, { rr with op_result_network = r.op_result_network })
  :: ! results_ops;
  r
  
let check_result_implementations () =
  lprintf "\n---- Methods not implemented for CommonResult ----\n";
  lprint_newline ();
  List.iter (fun (c, cc) ->
      let n = c.op_result_network.network_name in
      lprintf "\n  Network %s\n" n; lprint_newline ();
      if c.op_result_download == cc.op_result_download then 
        lprintf "op_result_download\n";
      if c.op_result_info == cc.op_result_info then
        lprintf "op_result_info\n";
  ) !results_ops;
  lprint_newline () 

  
let result_find num = 
  H.find results_by_num (as_result { dummy_result_impl with
      impl_result_num = num })

  
let result_print (result : result) buf count =
  ()
  
let results_iter f =
  H.iter (fun r -> f (result_num r) r) results_by_num
  