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

open ImTypes
open ImProtocol
  
type 'a identity_impl = {
    mutable impl_identity_num : int;
    mutable impl_identity_val : 'a;
    mutable impl_identity_ops : 'a identity_ops;
  }
  
and 'a identity_ops = {
    mutable op_identity_protocol : protocol; 
    
(* Send a message to this identity *)
    mutable op_identity_send : ('a -> string -> unit); 
    
(* Remove this identity from my contacts *)
    mutable op_identity_remove : ('a -> unit);
    
(* Get the name of this identity *)
    mutable op_identity_name : ('a -> string);
    
(* Is this identity online ? *)
    mutable op_identity_status : ('a -> bool);

    mutable op_identity_account : ('a -> account);
    
    mutable op_identity_config_record : ('a -> config_record);
    
    mutable op_identity_open_chat : ('a -> unit);
  }
  
  
let as_identity  (identity : 'a identity_impl) =
  let (identity : identity) = Obj.magic identity in
  identity
  
let as_identity_impl  (identity : identity) =
  let (identity : 'a identity_impl) = Obj.magic identity in
  identity
  
let identity_num identity =
  let impl = as_identity_impl  identity in
  impl.impl_identity_num

let dummy_identity_impl = {
    impl_identity_num = 0;
    impl_identity_val = 0;
    impl_identity_ops = Obj.magic 0;
  }
  
let dummy_identity = as_identity dummy_identity_impl  

module H = Weak2.Make(struct
      type t = identity
      let hash identity = Hashtbl.hash (identity_num identity)
      
      let equal x y  = (identity_num x) = (identity_num y)
    end)

let identity_counter = ref 0
let identitys_by_num = H.create 1027
  
let update_identity_num impl =
  if impl.impl_identity_num = 0 then begin
      incr identity_counter;
      impl.impl_identity_num <- !identity_counter;
      H.add identitys_by_num (as_identity impl);
    end

let identity_send identity msg =
  let identity = as_identity_impl identity in
  identity.impl_identity_ops.op_identity_send identity.impl_identity_val msg

let identity_remove identity =
  let identity = as_identity_impl identity in
  identity.impl_identity_ops.op_identity_remove identity.impl_identity_val

let identity_name identity =
  let identity = as_identity_impl identity in
  identity.impl_identity_ops.op_identity_name identity.impl_identity_val

let identity_status identity =
  let identity = as_identity_impl identity in
  identity.impl_identity_ops.op_identity_status identity.impl_identity_val

let identity_account identity =
  let impl = as_identity_impl identity in
  impl.impl_identity_ops.op_identity_account identity

let identity_config_record identity =
  let identity = as_identity_impl identity in
  identity.impl_identity_ops.op_identity_config_record identity.impl_identity_val

  
let identity_open_chat identity =
  let identity = as_identity_impl identity in
  identity.impl_identity_ops.op_identity_open_chat identity.impl_identity_val
  
let new_identity_ops protocol = {
    op_identity_protocol = protocol; 
    op_identity_send = fni2 protocol "op_identity_send";
    op_identity_remove = fni protocol "op_identity_remove";
    op_identity_name = fni protocol "op_identity_name";
    op_identity_status = fni protocol "op_identity_status";
    op_identity_config_record = fni protocol "op_identity_config_record";
    op_identity_open_chat = fni protocol "op_identity_open_chat";
    op_identity_account = fni protocol "op_identity_account";
   }
  