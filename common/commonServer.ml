(* Copyright 2001, 2002 b8_bavard, b8_fee_carabine, INRIA *)
(*
    This server is part of mldonkey.

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
  
type 'a server_impl = {
    mutable impl_server_state : CommonTypes.host_state;
    mutable impl_server_num : int;
    mutable impl_server_sort : float;
    mutable impl_server_val : 'a;
    mutable impl_server_ops : 'a server_ops;
  }

and 'a server_ops = {
    mutable op_server_network : network;
    mutable op_server_to_option : ('a -> (string * option_value) list);
    mutable op_server_remove : ('a -> unit);
    mutable op_server_print : ('a -> CommonTypes.connection_options -> unit);
    mutable op_server_info : ('a -> Gui_proto.server_info);
    mutable op_server_sort : ('a -> float);
    mutable op_server_connect : ('a -> unit);
    mutable op_server_disconnect : ('a -> unit);
    mutable op_server_users : ('a -> user list);
    mutable op_server_query_users : ('a -> unit);
    mutable op_server_find_user : ('a -> string -> unit);
    
(* line number X user num X message  *)
    mutable op_server_new_messages : (unit -> (int * int * string) list);
  }

let ni n m = 
  let s = Printf.sprintf "Server.%s not implemented by %s" 
      m n.network_name in
  print_string s; print_newline ();
  s
  
let fni n m =  failwith (ni n m)
let ni_ok n m = ignore (ni n m)

let server_num = ref 0
let servers_by_num = Hashtbl.create 1027

let as_server  (server : 'a server_impl) =
  let (server : server) = Obj.magic server in
  server

let as_server_impl  (server : server) =
  let (server : 'a server_impl) = Obj.magic server in
  server

let servers_update_map = ref Intmap.empty
  
let server_must_update s =
  if not (Intmap.mem (as_server_impl s).impl_server_num !servers_update_map) then
    servers_update_map := Intmap.add (as_server_impl s).impl_server_num 
      s !servers_update_map

let server_update_num impl =
  let server = as_server impl in
  incr server_num;
  impl.impl_server_num <- !server_num;
  server_must_update server;
  Hashtbl.add servers_by_num !server_num server

let server_to_option (server : server) =
  let server = as_server_impl server in
  server.impl_server_ops.op_server_to_option server.impl_server_val

let server_network (server : server) =
  let server = as_server_impl server in
  server.impl_server_ops.op_server_network

let server_to_option server =
  let server = as_server_impl server in
  server.impl_server_ops.op_server_to_option server.impl_server_val

let server_print server buf =
  let server = as_server_impl server in
  server.impl_server_ops.op_server_print server.impl_server_val buf

let server_info (server : server) =
  let server = as_server_impl server in
  server.impl_server_ops.op_server_info server.impl_server_val

let server_find_user s u =
  let s = as_server_impl s in
  s.impl_server_ops.op_server_find_user s.impl_server_val u

let server_query_users s =
  let s = as_server_impl s in
  s.impl_server_ops.op_server_query_users s.impl_server_val

let server_users s =
  let s = as_server_impl s in
  s.impl_server_ops.op_server_users s.impl_server_val

let new_server_ops network = {
    op_server_network =  network;
    op_server_remove = (fun _ -> ni_ok network "server_remove");
    op_server_print = (fun _ _ -> ni_ok network "server_print");
    op_server_to_option = (fun _ -> fni network "server_to_option");
    op_server_info = (fun _ -> fni network "server_info");
    op_server_sort = (fun _ -> ni_ok network "server_sort"; 0.0);
    op_server_connect = (fun _ -> ni_ok network "server_connect");
    op_server_disconnect = (fun _ -> ni_ok network "server_disconnect");
    op_server_find_user = (fun _ -> fni network "find_user");
    op_server_query_users = (fun _ -> ni_ok network "query_users");
    op_server_users = (fun _ -> fni network "users");
    op_server_new_messages = (fun _ -> fni network "new_messages");
  }
  
let server_new_messages s =
  let s = as_server_impl s in
  s.impl_server_ops.op_server_new_messages s.impl_server_val
  
  (*
let remove_connected_server network s =
  network.connected_servers <- List2.removeq s network.connected_servers

let add_connected_server network s =
  network.connected_servers <- s :: network.connected_servers
*)



let server_find (num : int) = 
  (Hashtbl.find servers_by_num num : server)

let server_num s =
  let s = as_server_impl s in
  s.impl_server_num
  
let server_connect s =
  let server = as_server_impl s in
  server.impl_server_ops.op_server_connect server.impl_server_val
  
let server_disconnect s =
  let server = as_server_impl s in
  server.impl_server_ops.op_server_disconnect server.impl_server_val


let server_state c =
  let impl = as_server_impl c in
  impl.impl_server_state

let server_num c =
  let impl = as_server_impl c in
  impl.impl_server_num
  
let set_server_state c state =
  let impl = as_server_impl c in
  if impl.impl_server_state <> state then begin
      impl.impl_server_state <- state;
      server_must_update c
    end

let server_sort () = 
  let list = ref [] in
  Hashtbl.iter (fun _ s ->
      list := s :: !list;
      let s = as_server_impl s in
      s.impl_server_sort <- s.impl_server_ops.op_server_sort s.impl_server_val;
  ) servers_by_num;
  Sort.list (fun s1 s2 -> 
      (as_server_impl s1).impl_server_sort >= (as_server_impl s2).impl_server_sort
  ) !list

let com_servers_by_num = servers_by_num
  
  
let server_new_users = ref []
    
let server_new_user server c =
  let key = (server_num server, (c : user)) in
  if not (List.mem key !server_new_users) then
    server_new_users := key :: !server_new_users  


let servers_by_num = ()
  
    