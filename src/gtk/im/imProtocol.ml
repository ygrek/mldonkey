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
open ImTypes

type 'a protocol_impl = {
    mutable impl_protocol_num : int;
    mutable impl_protocol_val : 'a;
    mutable impl_protocol_name : string; 
    mutable op_protocol_account_from_option : 'a -> 
    (string * Options.option_value) list -> account;
    mutable op_protocol_new_account : 'a -> account;
    mutable op_protocol_available_status : 'a -> online_status list;
  }
  
  
let as_protocol  (protocol : 'a protocol_impl) =
  let (protocol : protocol) = Obj.magic protocol in
  protocol
  
let as_protocol_impl  (protocol : protocol) =
  let (protocol : 'a protocol_impl) = Obj.magic protocol in
  protocol
  
let protocol_num protocol =
  let impl = as_protocol_impl  protocol in
  impl.impl_protocol_num
  
let protocol_name protocol  =
  let protocol = as_protocol_impl protocol in
  protocol.impl_protocol_name

let dummy_protocol_impl = {
    impl_protocol_num = 0;
    impl_protocol_val = 0;
    impl_protocol_name = ""; 
    op_protocol_new_account = (fun _ -> raise Exit);
    op_protocol_account_from_option = (fun _ -> raise Exit);
    op_protocol_available_status = (fun _ -> [Online_available]);
  }
  
let dummy_protocol = as_protocol dummy_protocol_impl  

module Hnum = Weak2.Make(struct
      type t = protocol
      let hash protocol = Hashtbl.hash (protocol_num protocol)
      
      let equal x y  = (protocol_num x) = (protocol_num y)
    end)

module Hname = Weak2.Make(struct
      type t = protocol
      let hash protocol = Hashtbl.hash (protocol_name protocol)
      
      let equal x y  =  (protocol_name x) = (protocol_name y)
    end)

let protocols_by_name = Hname.create 1027
let protocols_by_num = Hnum.create 1027
let counter = ref 0

let protocol_account_from_option protocol info =
  let protocol = as_protocol_impl protocol in
  protocol.op_protocol_account_from_option protocol.impl_protocol_val info

let protocol_new_account protocol =
  let protocol = as_protocol_impl protocol in
  protocol.op_protocol_new_account protocol.impl_protocol_val 

let protocol_available_status protocol =
  let protocol = as_protocol_impl protocol in
  protocol.op_protocol_available_status protocol.impl_protocol_val 
    
let new_protocol name v = 
  incr counter;  
  let p = {
      dummy_protocol with
      impl_protocol_name = name;
      impl_protocol_val = v;
      impl_protocol_num = !counter;
      op_protocol_account_from_option = (fun s -> 
        failwith "op_protocol_account_from_option not implemented for %s" name);
      op_protocol_new_account = (fun s -> 
        failwith "op_protocol_new_account not implemented for %s" name);
      op_protocol_available_status = (fun s -> 
          [Online_available])
    }  in
  let p = as_protocol p in
  Hname.add protocols_by_name p;
  lprintf "Registered protocol %s" name; lprint_newline ();
  Hnum.add protocols_by_num p;
  p
  
let ni protocol name _ =
  lprintf "Method %s not implemented in %s" name 
    (protocol_name protocol);
  lprint_newline ()
  
let fni protocol name x =
  ni protocol name x;
  raise Not_found
  
let ni2 protocol name _ _ =
  lprintf "Method %s not implemented in %s" name 
    (protocol_name protocol); lprint_newline ()
  
  
let fni2 protocol name x y =
  ni2 protocol name x y;
  raise Not_found
  

let protocol_find_by_name name = 
  Hname.find protocols_by_name
    (as_protocol { dummy_protocol_impl with impl_protocol_name = name })
  
  
let iter f =
  Hname.iter (fun p -> try f p with _ -> ())  protocols_by_name
  