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
open ImEvent
  
type 'a account_impl = {
    mutable impl_account_num : int;
    mutable impl_account_val : 'a;
    mutable impl_account_ops : 'a account_ops;
    mutable impl_account_status : status;
  }
  
and 'a account_ops = {
    mutable op_account_protocol : protocol; 
    
(* Send a message to this account *)
    mutable op_account_send : ('a -> string -> unit); 
    
(* Remove this account from my contacts *)
    mutable op_account_logout : ('a -> unit);

    mutable op_account_contacts : ('a -> identity list);
    
(* Remove this account from my contacts *)
    mutable op_account_login : ('a -> unit);
    
(* Get the name of this account *)
    mutable op_account_name : ('a -> string);

    mutable op_account_set_status : ('a -> status -> unit);
    
(* How to save infos on this account ? *)
    mutable op_account_to_option : ('a -> (string * Options.option_value) list);

(* This function is called every minute *)
    mutable op_account_keepalive : ('a -> unit);
    
    mutable op_account_config_record : ('a -> config_record);
    
    mutable op_account_new_identity : ('a -> identity);
    
    mutable op_account_open_chat : ('a -> identity list -> unit);
    
    mutable op_account_join_room : ('a -> string -> unit);
    
    mutable op_account_has_rooms : ('a -> bool);
    
    mutable op_account_prefered_rooms : ('a -> string list);
  }
  
  
let as_account  (account : 'a account_impl) =
  let (account : account) = Obj.magic account in
  account
  
let as_account_impl  (account : account) =
  let (account : 'a account_impl) = Obj.magic account in
  account
  
let account_num account =
  let impl = as_account_impl  account in
  impl.impl_account_num

let dummy_account_impl = {
    impl_account_num = 0;
    impl_account_val = 0;
    impl_account_ops = Obj.magic 0;
    impl_account_status = Status_offline;
  }
  
let dummy_account = as_account dummy_account_impl  

module H = Weak2.Make(struct
      type t = account
      let hash account = Hashtbl.hash (account_num account)
      
      let equal x y  = (account_num x) = (account_num y)
    end)

let account_counter = ref 0
let accounts_by_num = H.create 1027
  
let update_account_num impl =
  if impl.impl_account_num = 0 then begin
      incr account_counter;
      impl.impl_account_num <- !account_counter;
      H.add accounts_by_num (as_account impl);
      add_event (Account_event (as_account impl));
    end

let account_send account msg =
  let account = as_account_impl account in
  account.impl_account_ops.op_account_send account.impl_account_val msg

let account_login account =
  let account = as_account_impl account in
  account.impl_account_ops.op_account_login account.impl_account_val

let account_logout account =
  let account = as_account_impl account in
  account.impl_account_ops.op_account_logout account.impl_account_val

let account_keepalive account =
  let account = as_account_impl account in
  account.impl_account_ops.op_account_keepalive account.impl_account_val

let account_has_rooms account =
  let account = as_account_impl account in
  account.impl_account_ops.op_account_has_rooms account.impl_account_val

let account_join_room account room_name =
  let account = as_account_impl account in
  account.impl_account_ops.op_account_join_room account.impl_account_val room_name

let account_status account =
  let account = as_account_impl account in
  account.impl_account_status

let set_account_status account status =
  Printf.printf "set_account_status"; print_newline ();
  let impl = as_account_impl account in
  if impl.impl_account_status <> status then begin
      impl.impl_account_status <- status;
      Printf.printf "Add event account"; print_newline ();
      add_event (Account_event account);
    end

let account_name account =
  let account = as_account_impl account in
  account.impl_account_ops.op_account_name account.impl_account_val

let account_status account =
  let account = as_account_impl account in
  account.impl_account_status

let account_config_record account =
  let account = as_account_impl account in
  account.impl_account_ops.op_account_config_record account.impl_account_val

let account_to_option account =
  let account = as_account_impl account in
  account.impl_account_ops.op_account_to_option account.impl_account_val

let account_new_identity account =
  let account = as_account_impl account in
  account.impl_account_ops.op_account_new_identity account.impl_account_val

let account_set_status account status =
  let account = as_account_impl account in
  account.impl_account_ops.op_account_set_status account.impl_account_val status

let account_protocol account =
  let account = as_account_impl account in
  account.impl_account_ops.op_account_protocol

let account_open_chat account =
  let account = as_account_impl account in
  account.impl_account_ops.op_account_open_chat account.impl_account_val

let account_prefered_rooms account =
  let account = as_account_impl account in
  account.impl_account_ops.op_account_prefered_rooms account.impl_account_val
  
  
let new_account_ops protocol = {
    op_account_protocol = protocol; 
    op_account_send = fni2 protocol "op_account_send";
    op_account_login = fni protocol "op_account_login";
    op_account_logout = fni protocol "op_account_logout";
    op_account_name = fni protocol "op_account_name";
    op_account_to_option = fni protocol "op_account_to_option";
    op_account_keepalive = fni protocol "op_account_keepalive";
    op_account_config_record = fni protocol "op_account_config_record";
    op_account_new_identity = fni protocol "op_account_new_identity";
    op_account_set_status = fni protocol "op_account_set_status";
    op_account_contacts = fni protocol "op_account_contacts";
    op_account_open_chat = fni protocol "op_account_open_chat";
    op_account_has_rooms = (fun x -> ni protocol "op_account_has_rooms" x; false);
    op_account_prefered_rooms = (fun x -> ni protocol "op_account_prefered_rooms" x; []);
    op_account_join_room = fni protocol "op_account_open_chat";
  }
