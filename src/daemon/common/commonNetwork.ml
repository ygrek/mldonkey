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
open CommonGlobals
open Options
open CommonTypes

    

let ni n m = 
  let s = Printf.sprintf "Network.%s not implemented by %s" 
      m n in
  lprintf "%s\n" s; 
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

let networks_ops = ref []

let check_network_implementations () =
  lprintf "\n---- Methods not implemented for CommonNetwork ----\n\n";
  List.iter (fun (c, cc) ->
      let n = c.network_name in
      lprintf "\n  Network %s\n\n" n; 
      if c.network_config_file == cc.network_config_file then 
        lprintf "network_config_file\n";
      if c.op_network_connected_servers == cc.op_network_connected_servers then 
        lprintf "op_network_connected_servers\n";
      if c.op_network_is_enabled == cc.op_network_is_enabled then 
        lprintf "op_network_is_enabled\n";
      if c.op_network_save_complex_options == cc.op_network_save_complex_options
        then 
        lprintf "op_network_save_complex_options\n";
      if c.op_network_load_complex_options == cc.op_network_load_complex_options
        then 
        lprintf "op_network_load_complex_options\n";
      if c.op_network_enable == cc.op_network_enable then 
        lprintf "op_network_enable\n";
      if c.op_network_add_server == cc.op_network_add_server then 
        lprintf "op_network_add_server\n";
      if c.op_network_server_of_option == cc.op_network_server_of_option then 
        lprintf "op_network_server_of_option";
      if c.op_network_file_of_option == cc.op_network_file_of_option then 
        lprintf "op_network_file_of_option";
      if c.op_network_client_of_option == cc.op_network_client_of_option then 
        lprintf "op_network_client_of_option";
      if c.op_network_recover_temp == cc.op_network_recover_temp then 
        lprintf "op_network_recover_temp\n";
      if c.op_network_search == cc.op_network_search then 
        lprintf "op_network_search\n";
      if c.op_network_share == cc.op_network_share then 
        lprintf "op_network_share\n";
      if c.op_network_private_message == cc.op_network_private_message then 
        lprintf "op_network_private_message\n";
      if c.op_network_parse_url == cc.op_network_parse_url then 
        lprintf "op_network_parse_url\n";
      if c.op_network_connect_servers == cc.op_network_connect_servers then 
        lprintf "op_network_connect_servers\n";
      if c.op_network_forget_search == cc.op_network_forget_search then 
        lprintf "op_network_forget_search\n";
      if c.op_network_close_search == cc.op_network_close_search then 
        lprintf "op_network_close_search\n";
      if c.op_network_extend_search == cc.op_network_extend_search then 
        lprintf "op_network_extend_search\n";
      if c.op_network_clean_servers == cc.op_network_clean_servers then 
        lprintf "op_network_clean_servers\n";
      if c.op_network_display_stats == cc.op_network_display_stats then 
        lprintf "op_network_display_stats\n";
      if c.op_network_info == cc.op_network_info then 
        lprintf "op_network_info\n";
  ) !networks_ops;
  lprint_newline () 
  
let network_connected_servers n = n.op_network_connected_servers ()
let network_is_enabled n = n.op_network_is_enabled ()
let network_save_complex_options n = n.op_network_save_complex_options ()
let network_load_complex_options n = n.op_network_load_complex_options ()
let network_enable n =  n.op_network_enable ()
let network_disable n = n.op_network_disable () 
let network_share n s = n.op_network_share s
let network_recover_temp n = n.op_network_recover_temp ()
let network_add_server n s = n.op_network_add_server s
let network_server_of_option n s = n.op_network_server_of_option s
let network_file_of_option n f = n.op_network_file_of_option f 
let network_client_of_option n f = n.op_network_client_of_option f

  
let networks_iter f =
  List.iter (fun r ->
      try
        if network_is_enabled r then f r
      with
      | IgnoreNetwork -> ()
      | e ->
          lprintf "Exception %s in Network.iter for %s\n"
            (Printexc2.to_string e) r.network_name;
  ) !networks
  
let networks_iter_until_true f =
  List.exists (fun r ->
      try
        network_is_enabled r && f r
      with 
        IgnoreNetwork -> false
      | e ->
          lprintf "Exception %s in Network.iter for %s\n"
            (Printexc2.to_string e) r.network_name;
          false
  ) !networks
  
let networks_iter_all f =
  List.iter (fun r ->
      try f r  
      with
      | IgnoreNetwork -> ()
      | e ->
          lprintf "Exception %s in Network.iter for %s\n"
            (Printexc2.to_string e) r.network_name;
  ) !networks
  
let networks_iter_all_until_true f =
  List.exists (fun r ->
      try f r  
      with
        IgnoreNetwork -> false
      | e ->
          lprintf "Exception %s in Network.iter for %s\n"
            (Printexc2.to_string e) r.network_name;
          false
  ) !networks

let network_find_by_name name =
  Hashtbl.find networks_by_name name
  
let network_find_by_num num =
  Hashtbl.find networks_by_num num

  (*
(* we could replace that by a [32..127] array mapping to functions. it would 
only take 100*4 bytes ... *)
  
let network_escape_chars = ref []
  
let register_escape_char (c : char) (f : unit -> string) =
  network_escape_chars := (c,f) :: !network_escape_chars
  
let escape_char c = (List.assq c !network_escape_chars) ()
*)
  
let network_commands = ref ([] : (string * string * CommonTypes.arg_kind * string) list)

let commands_by_kind = Hashtbl.create 11
  
let register_commands list = 
  List.iter (fun (s, n, f, h) ->
      try
        let ss = Hashtbl.find commands_by_kind n in
        ss := (s,h) :: !ss
      with _ ->
          Hashtbl.add commands_by_kind n (ref [s,h])
  ) list;
  network_commands := list @ !network_commands
  
let network_connect_servers n = n.op_network_connect_servers ()
let network_forget_search n s = n.op_network_forget_search s
let network_close_search n s = n.op_network_close_search s  
let network_private_message n id s = n.op_network_private_message id s
  
let network_extend_search n e = n.op_network_extend_search e
let network_connected n = n.op_network_connected ()
  
let network_clean_servers r = r.op_network_clean_servers ()

let network_parse_url n url = 
  let url = try Url.decode url with _ -> url in
  n.op_network_parse_url url
    
let network_info n = n.op_network_info ()

let new_network shortname name flags = 
  let manager = TcpBufferedSocket.create_connection_manager name in
  let r =
    {
      network_name = name;
      network_num = network_uid ();
      network_shortname = shortname;
      network_flags = flags;
      network_config_file = [];
      network_connection_manager = manager;
      op_network_connected_servers = (fun _ -> fni name "connected_servers");
      op_network_is_enabled =  (fun _ -> fni name "is_enabled");
      op_network_save_complex_options =  (fun _ -> ni_ok name "save_complex_options");
      op_network_load_complex_options =  (fun _ -> ni_ok name "load_complex_options");
      op_network_enable =  (fun _ -> ni_ok name "enable");
      op_network_disable =  (fun _ -> ni_ok name "disable");
      op_network_server_of_option =  (fun _ -> fni name "op_network_server_of_option");
      op_network_file_of_option =  (fun _ _ -> fni name "op_network_file_of_option");
      op_network_client_of_option =  (fun _ -> fni name "op_network_client_of_option");
      op_network_recover_temp = (fun _ -> ni_ok name "recover_temp");
      op_network_search = (fun _ _ -> ni_ok name "search");
      op_network_share = (fun _ _ _ -> ni_ok name "share");
      op_network_private_message = (fun _ _ -> ni_ok name "private message");
      op_network_connect_servers = (fun _ -> ni_ok name "connect_servers");
      op_network_forget_search = (fun _ -> ni_ok name "forget_search");
      op_network_close_search = (fun _ -> ni_ok name "close_search");
      op_network_extend_search = (fun _ _ -> ni_ok name "extend search");
      op_network_clean_servers = (fun _ -> ni_ok name "clean servers");
      op_network_parse_url = (fun _ -> ni_ok name "parse_url"; false);
      op_network_info = (fun _ -> fni name "network_info");
      op_network_connected = (fun _ -> ni_ok name "connected"; false);
      op_network_add_server = (fun _ -> fni name "op_network_add_server");
      op_network_gui_message = (fun _ -> ni_ok name "gui_message");
      op_network_download = (fun _ -> raise IgnoreNetwork);
      op_network_display_stats = (fun _ _ -> ni_ok name "display_stats");
    }
  in
  let rr = (Obj.magic r: network) in
  networks_ops := (rr, { rr with network_name = rr.network_name })
  :: !networks_ops;
  networks := r :: !networks;
  Hashtbl.add networks_by_name r.network_name r;
  Hashtbl.add networks_by_num r.network_num r;
  if !networks_string = "" then
    networks_string := r.network_name
  else
    networks_string := Printf.sprintf "%s  %s" !networks_string r.network_name;
  lprintf "Network %s registered\n" r.network_name;
  r
  
