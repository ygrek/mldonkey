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


open CommonOptions
open Options
open CommonTypes
  
type 'a client_impl = {
    mutable impl_client_type : client_type;
    mutable impl_client_state : host_state;
    mutable impl_client_update : int;
    mutable impl_client_num : int;
    mutable impl_client_val : 'a;
    mutable impl_client_ops : 'a client_ops;
  }
  
and 'a client_ops = {
    mutable op_client_network : network;
    
(* force connection to the client. *)
    mutable op_client_connect : ('a -> unit);
    
(* convert a client structure to be stored in the option file *)
    mutable op_client_to_option : ('a -> (string * option_value) list);
    
(* convert a client to an info structure used in the interfaces *)
    mutable op_client_info : ('a -> GuiTypes.client_info);
    
(* send a message to a given client *)
    mutable op_client_say : ('a -> string -> unit);
    
(* ask a client for its list of files. The boolean argument is used to
decide whether to connect immediatly or not. *)
    mutable op_client_browse : ('a -> bool -> unit);
    
(* returns the list of files of a client as already known *)
    mutable op_client_files : ('a -> (string * result) list);
    
(* used to clear the file list a client *)
    mutable op_client_clear_files : ('a -> unit);
    
    mutable op_client_bprint : (Buffer.t -> 'a -> unit);
    
    mutable op_client_bprint_html : (Buffer.t -> 'a -> 
    CommonTypes.file -> unit);
  }
  
let client_counter = CommonUser.user_counter
  
  
let as_client  (client : 'a client_impl) =
  let (client : client) = Obj.magic client in
  client
  
let as_client_impl  (client : client) =
  let (client : 'a client_impl) = Obj.magic client in
  client

let client_num c = 
  let c = as_client_impl c in
  c.impl_client_num
  
let dummy_client_impl = {
    impl_client_type = NormalClient;
    impl_client_state = NewHost;
    impl_client_update = 1;
    impl_client_num = 0;
    impl_client_val = 0;
    impl_client_ops = Obj.magic None;
  }

let dummy_client = as_client dummy_client_impl
  
module H = Weak2.Make(struct
      type t = client
      let hash c = Hashtbl.hash (client_num c)
      
      let equal x y = (client_num x) = (client_num y)
    end)

let clients_by_num = H.create 1027
  
let client_network (client : client) =
  let client = as_client_impl client in
  client.impl_client_ops.op_client_network

let client_to_option (client : client) =
  let client = as_client_impl client in
  client.impl_client_ops.op_client_to_option client.impl_client_val

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

let client_bprint buf client =
  let client = as_client_impl client in
  client.impl_client_ops.op_client_bprint client.impl_client_val buf

let client_bprint_html buf client file =
  let client = as_client_impl client in
  client.impl_client_ops.op_client_bprint_html client.impl_client_val buf file
  
let client_connect client=
  let client = as_client_impl client in
  client.impl_client_ops.op_client_connect client.impl_client_val

let client_clear_files client=
  let client = as_client_impl client in
  client.impl_client_ops.op_client_clear_files client.impl_client_val

let client_browse client immediate =
  let client = as_client_impl client in
  client.impl_client_ops.op_client_browse client.impl_client_val immediate
  
let ni n m = 
  let s = Printf.sprintf "Client.%s not implemented by %s" 
      m n.network_name in
  print_string s; print_newline ();
  s
  
let fni n m =   failwith (ni n m)
  let ni_ok n m = ignore (ni n m)

let clients_ops = ref []
  
let new_client_ops network = 
  let c = {
      op_client_network =  network;
      op_client_to_option = (fun _ -> fni network "client_to_option");
      op_client_info = (fun _ -> fni network "client_info");
      op_client_say = (fun _ _ -> ni_ok network "client_say");
      op_client_files = (fun _ -> fni network "client_files");
      op_client_connect  = (fun _ -> ni_ok network "client_connect");
      op_client_clear_files = (fun _ -> ni_ok network "client_clear_files");
      op_client_browse = (fun _ _ -> ni_ok network "client_browse");
      op_client_bprint = (fun _ _ -> ni_ok network "client_bprint");
      op_client_bprint_html = (fun _ _ _ -> ni_ok network "client_bprint_html");
    } in
  let cc = (Obj.magic c : int client_ops) in
  clients_ops := (cc, { cc with op_client_network = c.op_client_network })
  :: ! clients_ops;
  c

let check_client_implementations () =
  Printf.printf "\n---- Methods not implemented for CommonClient ----\n";
  print_newline ();
  List.iter (fun (c, cc) ->
      let n = c.op_client_network.network_name in
      Printf.printf "\n  Network %s\n" n; print_newline ();
      if c.op_client_to_option == cc.op_client_to_option then 
        Printf.printf "op_client_to_option\n";
      if c.op_client_info == cc.op_client_info then
        Printf.printf "op_client_info\n";
      if c.op_client_say == cc.op_client_say then
        Printf.printf "op_client_say\n";
      if c.op_client_files == cc.op_client_files then
        Printf.printf "op_client_files\n";
      if c.op_client_connect == cc.op_client_connect then
        Printf.printf "op_client_connect\n";
      if c.op_client_clear_files == cc.op_client_clear_files then
        Printf.printf "op_client_clear_files\n";
      if c.op_client_browse == cc.op_client_browse then
        Printf.printf "op_client_browse\n";
  ) !clients_ops;
  print_newline () 
  
let client_find num = 
  H.find clients_by_num (as_client { dummy_client_impl with
      impl_client_num = num })
    
let client_must_update client =
  let impl = as_client_impl client in
  if impl.impl_client_update <> 0 then
    CommonEvent.add_event (Client_info_event client);
  impl.impl_client_update <- 0
  
let client_must_update_state client =
  let impl = as_client_impl client in
  if impl.impl_client_update > 0 then
    begin
      impl.impl_client_update <- - impl.impl_client_update;
      CommonEvent.add_event (Client_info_event client);
    end

let client_state c =
  let impl = as_client_impl c in
  impl.impl_client_state
  
let set_client_state c state =
  let impl = as_client_impl c in
  if impl.impl_client_state <> state then begin
      impl.impl_client_state <- state;
      client_must_update_state c
    end

let set_client_disconnected c =
  let impl = as_client_impl c in
  match impl.impl_client_state with
    Connected n -> set_client_state c (NotConnected n)
  | _ ->  set_client_state c (NotConnected (-1))
    
let new_client (client : 'a client_impl) =
  incr client_counter;
  client.impl_client_num <- !client_counter;
  let (client : client) = Obj.magic client in
  H.add clients_by_num client;
  client_must_update client

let book_client_num () = 
  incr client_counter;
  !client_counter
  
let new_client_with_num (client : 'a client_impl) num =
  client.impl_client_num <- num;
  let (client : client) = Obj.magic client in
  H.add clients_by_num client;
  client_must_update client
  
let new_client (client : 'a client_impl) =
  new_client_with_num client (book_client_num ())
  
let client_remove c =
  set_client_state c RemovedHost
  
let client_type c =
  let impl = as_client_impl c in
  impl.impl_client_type

let set_client_type c t =
  let impl = as_client_impl c in
  if impl.impl_client_type <> t then begin
      impl.impl_client_type <- t;
      client_must_update c
    end

let _ = 
  CommonGlobals.add_memstat "CommonClient" (fun buf ->
      let counter = ref 0 in
      H.iter (fun _ -> incr counter) clients_by_num;
      Printf.bprintf buf "  clients: %d\n" !counter;
  )

let clients_get_all () =
  let list = ref [] in
  H.iter (fun c ->
      list := (client_num c) :: !list) clients_by_num;
  !list
    
let clients_by_num = ()
  
let client_new_file (client :client) (dirname:string) r =
  CommonEvent.add_event (Client_new_file_event 
    (client, dirname, (r : result)))

module G = GuiTypes
  
let client_print c o =
  let impl = as_client_impl c in
  let i = client_info c in
  let n = impl.impl_client_ops.op_client_network in
  let info = client_info c in
  let buf = o.conn_buf in
  if use_html_mods o then begin
      Printf.bprintf buf "\\<td class=\\\"sr\\\"\\>%s\\</td\\>
    \\<td class=\\\"sr\\\"\\>%s\\</td\\>
    \\<td class=\\\"sr\\\"\\>%s\\</td\\>"
        n.network_name 
        (match info.G.client_kind with
          Indirect_location (name, _) -> Printf.sprintf "I"
        | Known_location (ip, port) -> Printf.sprintf "D"
      ) 
      info.G.client_name      
    end
  else begin
      Printf.bprintf buf "[%s %-5d] %d %23s %-20s"
        n.network_name
        (client_num c)
      i.GuiTypes.client_rating
        (match info.G.client_kind with
          Indirect_location (name, _) -> name
        | Known_location (ip, port) ->
            Printf.sprintf "%s:%d" (Ip.to_string ip) port
      )
      info.G.client_name
    end

