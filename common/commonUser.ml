(* Copyright 2001, 2002 b8_bavard, b8_fee_carabine, INRIA *)
(*
    This client is part of mldonkey.

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
  
type 'a user_impl = {
    mutable impl_user_update : bool;
    mutable impl_user_state : host_state;
    mutable impl_user_num : int;
    mutable impl_user_val : 'a;
    mutable impl_user_ops : 'a user_ops;
  }
  
and 'a user_ops = {
    mutable op_user_network : network;
    mutable op_user_commit : ('a -> unit);
    mutable op_user_save_as : ('a -> string -> unit);
    mutable op_user_print : ('a -> CommonTypes.connection_options -> unit);
    mutable op_user_to_option : ('a -> (string * option_value) list);
    mutable op_user_remove : ('a -> unit);
    mutable op_user_info : ('a -> Gui_proto.user_info);
    mutable op_user_set_friend : ('a -> unit);
    mutable op_user_browse_files : ('a -> unit);
  }
  
let user_counter = ref 1
let users_by_num = Hashtbl.create 1027
  
let as_user  (user : 'a user_impl) =
  let (user : user) = Obj.magic user in
  user
  
let as_user_impl  (user : user) =
  let (user : 'a user_impl) = Obj.magic user in
  user

    
let users_update_list = ref []

let user_must_update user =
  let impl = as_user_impl user in
  if not impl.impl_user_update then
    begin
      impl.impl_user_update <- true;
      users_update_list := user :: !users_update_list
    end
      
let user_add (user : 'a user_impl) =
  incr user_counter;
  user.impl_user_num <- !user_counter;
  let (user : user) = Obj.magic user in
  user_must_update user;
  Hashtbl.add users_by_num !user_counter user

let user_state c =
  let impl = as_user_impl c in
  impl.impl_user_state
  
let set_user_state c state =
  let impl = as_user_impl c in
  if impl.impl_user_state <> state then begin
      impl.impl_user_state <- state;
      user_must_update c
    end

let user_network (user : user) =
  let user = as_user_impl user in
  user.impl_user_ops.op_user_network

let user_to_option (user : user) =
  let user = as_user_impl user in
  user.impl_user_ops.op_user_to_option user.impl_user_val
  
let user_remove (user : user) =
  let impl = as_user_impl user in
  if impl.impl_user_state = NewHost then begin 
      impl.impl_user_state <- RemovedHost;
      user_must_update user;
      impl.impl_user_ops.op_user_remove impl.impl_user_val;
      Hashtbl.remove users_by_num impl.impl_user_num
    end
  
let user_commit (user : user) =
  let user = as_user_impl user in
  user.impl_user_ops.op_user_commit user.impl_user_val;
  Hashtbl.remove users_by_num user.impl_user_num
  
let user_print (user : user) buf =
  let user = as_user_impl user in
  user.impl_user_ops.op_user_print user.impl_user_val buf
  
let user_save_as (user : user) name =
  let user = as_user_impl user in
  user.impl_user_ops.op_user_save_as user.impl_user_val name

let user_network (user : user) =
  let user = as_user_impl user in
  user.impl_user_ops.op_user_network

let user_info (user : user) =
  let user = as_user_impl user in
  user.impl_user_ops.op_user_info user.impl_user_val

let user_set_friend u =
  let u = as_user_impl u in
  u.impl_user_ops.op_user_set_friend u.impl_user_val

let user_browse_files u =
  let u = as_user_impl u in
  u.impl_user_ops.op_user_browse_files u.impl_user_val

let ni n m = 
  let s = Printf.sprintf "User.%s not implemented by %s" 
      m n.network_name in
  print_string s; print_newline ();
  s
  
let fni n m =  failwith (ni n m)
let ni_ok n m = ignore (ni n m)
  
let new_user_ops network = {
    op_user_network =  network;
    op_user_commit = (fun _ -> ni_ok network "user_commit");
    op_user_save_as = (fun _ _ -> ni_ok network "user_save_as");
    op_user_print = (fun _ _ -> ni_ok network "user_print");
    op_user_to_option = (fun _ -> fni network "user_to_option");
    op_user_remove = (fun _ -> ni_ok network "user_remove");
    op_user_info = (fun _ -> fni network "user_info");
    op_user_set_friend = (fun _ -> ni_ok network "user_set_friend");
    op_user_browse_files = (fun _ -> ni_ok network "user_browse_files");
  }

let user_find num = Hashtbl.find users_by_num num
    
let user_num c = 
  let c = as_user_impl c in
  c.impl_user_num

  