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
  

let ni n m = 
  let s = Printf.sprintf "Network.%s not implemented by %s" 
      m n in
  print_string s; print_newline ();
  s
  
let fni n m =  failwith (ni n m)
let ni_ok n m = ignore (ni n m)
  
let network_uid =
  let counter = ref 0 in
  fun () ->
    incr counter;
    !counter
    
let networks = ref []
let networks_by_name = Hashtbl.create 11
let networks_by_num = Hashtbl.create 11

let new_network name = 
  let r =
    {
      network_name = name;
      network_num = network_uid ();
      op_network_connected_servers = (fun _ -> fni name "connected_servers");
      op_network_config_file = (fun _ -> fni name "config_file");
      op_network_is_enabled =  (fun _ -> fni name "is_enabled");
      op_network_save_simple_options =  (fun _ -> ni_ok name "save_simple_options");
      op_network_load_simple_options =  (fun _ -> ni_ok name "load_simple_options");
      op_network_save_complex_options =  (fun _ -> ni_ok name "save_complex_options");
      op_network_load_complex_options =  (fun _ -> ni_ok name "load_complex_options");
      op_network_enable =  (fun _ -> ni_ok name "enable");
      op_network_disable =  (fun _ -> ni_ok name "disable");
      op_network_add_server =  (fun _ -> fni name "add_server");
      op_network_add_file =  (fun _ _ -> fni name "add_file");
      op_network_add_client =  (fun _ -> fni name "add_client");
      op_network_prefixed_args =  (fun _ -> fni name "prefixed_args");
      op_network_search = (fun _ _ -> ni_ok name "search");
      op_network_share = (fun _ -> ni_ok name "share");
      op_network_private_message = (fun _ _ -> ni_ok name "prvate message");
      op_network_add_server_id = (fun _ _ -> ni_ok name "add_server_id");
      op_network_connect_servers = (fun _ -> ni_ok name "connect_servers");
      op_network_forget_search = (fun _ -> ni_ok name "forget_search");
      op_network_close_search = (fun _ -> ni_ok name "close_search");
      op_network_extend_search = (fun _ -> ni_ok name "extend search");
      op_network_clean_servers = (fun _ -> ni_ok name "clean servers");
      op_network_add_friend_id = (fun _ _ -> ni_ok name "add_friend_id");
    }
  in
  networks := r :: !networks;
  Hashtbl.add networks_by_name r.network_name r;
  Hashtbl.add networks_by_num r.network_num r;
  Printf.printf "Network %s registered" r.network_name;
  print_newline ();
  r
  
let network_connected_servers n = n.op_network_connected_servers ()
let network_config_file n = n.op_network_config_file ()
let network_is_enabled n = n.op_network_is_enabled ()
let network_save_simple_options n = n.op_network_save_simple_options ()
let network_load_simple_options n = n.op_network_load_simple_options ()
let network_save_complex_options n = n.op_network_save_complex_options ()
let network_load_complex_options n = n.op_network_load_complex_options ()
let network_enable n = n.op_network_enable ()
let network_disable n = n.op_network_disable ()
let network_share n s = n.op_network_share s
let network_add_server n s = n.op_network_add_server s
let network_add_file n f = n.op_network_add_file f
let network_add_client n f = n.op_network_add_client f
let network_prefixed_args n = n.op_network_prefixed_args ()
let network_add_friend_id n ip port = n.op_network_add_friend_id ip port

  
let networks_iter f =
  List.iter (fun r ->
      try
        if network_is_enabled r then f r
      with e ->
          Printf.printf "Exception %s in Network.iter for %s"
            (Printexc.to_string e) r.network_name;
          print_newline ()
  ) !networks
  
let networks_iter_all f =
  List.iter (fun r ->
      try f r  with e ->
          Printf.printf "Exception %s in Network.iter for %s"
            (Printexc.to_string e) r.network_name;
          print_newline ()
  ) !networks

let network_find_by_name name =
  Hashtbl.find networks_by_name name
  
let network_find_by_num num =
  Hashtbl.find networks_by_num num

(* we could replace that by a [32..127] array mapping to functions. it would 
only take 100*4 bytes ... *)
  
let network_escape_chars = ref []
  
let register_escape_char (c : char) (f : unit -> string) =
  network_escape_chars := (c,f) :: !network_escape_chars
  
let escape_char c = (List.assq c !network_escape_chars) ()
  
let network_commands = ref ([] : (string * Gui_proto.arg_kind * string) list)
  
let register_commands list = 
  network_commands := list @ !network_commands
  
let network_connect_servers n = n.op_network_connect_servers ()
let network_add_server_id n ip port = n.op_network_add_server_id ip port
let network_forget_search n s = n.op_network_forget_search s
let network_close_search n s = n.op_network_close_search s  
let network_private_message n id s = n.op_network_private_message id s
  
let network_extend_search n = n.op_network_extend_search ()
  
let network_clean_servers r = r.op_network_clean_servers ()
  