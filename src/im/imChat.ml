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
  
type 'a chat_impl = {
    mutable impl_chat_num : int;
    mutable impl_chat_val : 'a;
    mutable impl_chat_ops : 'a chat_ops;
    mutable impl_chat_account : account;
  }
  
and 'a chat_ops = {
    op_chat_protocol : protocol; 
    
(* Send a message to this chat *)
    mutable op_chat_send : ('a -> string -> unit); 
    
    mutable op_chat_close : ('a -> unit);
    
(* Get the name of this chat *)
    mutable op_chat_name : ('a -> string);
  }
  
  
let as_chat  (chat : 'a chat_impl) =
  let (chat : chat) = Obj.magic chat in
  chat
  
let as_chat_impl  (chat : chat) =
  let (chat : 'a chat_impl) = Obj.magic chat in
  chat
  
let chat_num chat =
  let impl = as_chat_impl  chat in
  impl.impl_chat_num

let dummy_chat_impl = {
    impl_chat_num = 0;
    impl_chat_val = 0;
    impl_chat_ops = Obj.magic 0;
    impl_chat_account = Obj.magic 0;
  }
  
let dummy_chat = as_chat dummy_chat_impl  

module H = Weak2.Make(struct
      type t = chat
      let hash chat = Hashtbl.hash (chat_num chat)
      
      let equal x y  = (chat_num x) = (chat_num y)
    end)

let chat_counter = ref 0
let chats_by_num = H.create 1027
  
let update_chat_num impl =
  if impl.impl_chat_num = 0 then begin
      incr chat_counter;
      impl.impl_chat_num <- !chat_counter;
      H.add chats_by_num (as_chat impl);
    end

let chat_send chat msg =
  let chat = as_chat_impl chat in
  chat.impl_chat_ops.op_chat_send chat.impl_chat_val msg

let chat_close chat =
  let chat = as_chat_impl chat in
  chat.impl_chat_ops.op_chat_close chat.impl_chat_val

let chat_name chat =
  let chat = as_chat_impl chat in
  chat.impl_chat_ops.op_chat_name chat.impl_chat_val

let chat_account chat =
  let chat = as_chat_impl chat in
  chat.impl_chat_account
    
let new_chat_ops protocol = {
    op_chat_protocol = protocol; 
    op_chat_send = fni2 protocol "op_chat_send";
    op_chat_close = fni protocol "op_chat_close";
    op_chat_name = fni protocol "op_chat_name";
  }
