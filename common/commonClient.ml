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
  
type 'a client_impl = {
    mutable impl_client_type : client_type;
    mutable impl_client_state : host_state;
    mutable impl_client_num : int;
    mutable impl_client_val : 'a;
    mutable impl_client_ops : 'a client_ops;
  }
  
and 'a client_ops = {
    mutable op_client_network : network;
    mutable op_client_commit : ('a -> unit);
    mutable op_client_connect : ('a -> unit);
    mutable op_client_save_as : ('a -> string -> unit);
    mutable op_client_print : ('a -> CommonTypes.connection_options -> unit);
    mutable op_client_to_option : ('a -> (string * option_value) list);
    mutable op_client_cancel : ('a -> unit);
    mutable op_client_info : ('a -> Gui_proto.client_info);
    mutable op_client_say : ('a -> string -> unit);
    mutable op_client_files : ('a -> result list);
    mutable op_client_set_friend : ('a -> unit);
    mutable op_client_remove_friend : ('a -> unit);
  }
  
let client_counter = CommonUser.user_counter
let clients_by_num = Hashtbl.create 1027
  
let as_client  (client : 'a client_impl) =
  let (client : client) = Obj.magic client in
  client
  
let as_client_impl  (client : client) =
  let (client : 'a client_impl) = Obj.magic client in
  client

let client_num c = 
  let c = as_client_impl c in
  c.impl_client_num
  
let client_network (client : client) =
  let client = as_client_impl client in
  client.impl_client_ops.op_client_network

let client_to_option (client : client) =
  let client = as_client_impl client in
  client.impl_client_ops.op_client_to_option client.impl_client_val
  
let client_cancel (client : client) =
  let client = as_client_impl client in
  client.impl_client_ops.op_client_cancel client.impl_client_val;
  Hashtbl.remove clients_by_num client.impl_client_num
  
let client_commit (client : client) =
  let client = as_client_impl client in
  client.impl_client_ops.op_client_commit client.impl_client_val;
  Hashtbl.remove clients_by_num client.impl_client_num
  
let client_print (client : client) buf =
  let client = as_client_impl client in
  client.impl_client_ops.op_client_print client.impl_client_val buf
  
let client_save_as (client : client) name =
  let client = as_client_impl client in
  client.impl_client_ops.op_client_save_as client.impl_client_val name

let client_network (client : client) =
  let client = as_client_impl client in
  client.impl_client_ops.op_client_network

let client_info (client : client) =
  let client = as_client_impl client in
  client.impl_client_ops.op_client_info client.impl_client_val

let client_say (client : client) s =
  let client = as_client_impl client in
  client.impl_client_ops.op_client_say client.impl_client_val s

let client_files client=
  let client = as_client_impl client in
  client.impl_client_ops.op_client_files client.impl_client_val

let client_connect client=
  let client = as_client_impl client in
  client.impl_client_ops.op_client_connect client.impl_client_val
  
let ni n m = 
  let s = Printf.sprintf "Client.%s not implemented by %s" 
      m n.network_name in
  print_string s; print_newline ();
  s
  
let fni n m =   failwith (ni n m)
  let ni_ok n m = ignore (ni n m)

let new_client_ops network = {
    op_client_network =  network;
    op_client_commit = (fun _ -> ni_ok network "client_commit");
    op_client_save_as = (fun _ _ -> ni_ok network "client_save_as");
    op_client_print = (fun _ _ -> ni_ok network "client_print");
    op_client_to_option = (fun _ -> fni network "client_to_option");
    op_client_cancel = (fun _ -> ni_ok network "client_cancel");
    op_client_info = (fun _ -> fni network "client_info");
    op_client_say = (fun _ _ -> ni_ok network "client_say");
    op_client_files = (fun _ -> fni network "client_files");
    op_client_set_friend = (fun _ -> ni_ok network "client_set_friend");
    op_client_remove_friend = (fun _ -> ni_ok network "client_remove_friend");
    op_client_connect  = (fun _ -> ni_ok network "client_connect");
  }

let client_find num = Hashtbl.find clients_by_num num
    
let clients_update_map = ref Intmap.empty

let client_must_update s =
  if not (Intmap.mem (as_client_impl s).impl_client_num !clients_update_map) 
  then
    clients_update_map := Intmap.add (as_client_impl s).impl_client_num 
      s !clients_update_map

let client_state c =
  let impl = as_client_impl c in
  impl.impl_client_state
  
let set_client_state c state =
  let impl = as_client_impl c in
  if impl.impl_client_state <> state then begin
      impl.impl_client_state <- state;
      client_must_update c
    end
  
let new_client (client : 'a client_impl) =
  incr client_counter;
  client.impl_client_num <- !client_counter;
  let (client : client) = Obj.magic client in
  Hashtbl.add clients_by_num !client_counter client;
  client_must_update client
  
  
let client_remove c =
  set_client_state c RemovedHost;
  let impl = as_client_impl c in
  Hashtbl.remove clients_by_num impl.impl_client_num

let client_type c =
  let impl = as_client_impl c in
  impl.impl_client_type
  
let clients_by_num = ()
  

let client_new_files = ref []
    
let client_new_file client c =
  let key = (client_num client, (c : result)) in
  if not (List.mem key !client_new_files) then  
    client_new_files := key :: !client_new_files  
