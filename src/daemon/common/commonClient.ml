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


open Printf2
open CommonOptions
open Options
open CommonTypes
open CommonGlobals

type 'a client_impl = {
    mutable impl_client_type : client_type;
    mutable impl_client_state : host_state;
    mutable impl_client_update : int;
    mutable impl_client_has_slot : bool;
    mutable impl_client_upload : shared option;
    mutable impl_client_num : int;
    mutable impl_client_val : 'a;
    mutable impl_client_ops : 'a client_ops;
  }
  
and 'a client_ops = {
    mutable op_client_network : network;
       
(* force connection to the client. *)
    mutable op_client_connect : ('a -> unit);
    
(* force connection to the client. *)
    mutable op_client_disconnect : ('a -> unit);
 
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
    
    mutable op_client_bprint : ('a -> Buffer.t -> unit);
    
    mutable op_client_bprint_html : ('a -> Buffer.t -> 
    CommonTypes.file -> unit);

    mutable op_client_dprint : ('a -> CommonTypes.ui_conn -> 
	CommonTypes.file -> unit);

    mutable op_client_dprint_html : ('a -> CommonTypes.ui_conn ->
    CommonTypes.file -> string -> bool);
    
    mutable op_client_debug : ('a -> bool -> unit);
    
    mutable op_client_can_upload : ('a -> int -> unit);
    
    mutable op_client_enter_upload_queue : ('a -> unit);
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
    impl_client_type = 0;
    impl_client_state = NewHost;
    impl_client_update = 1;
    impl_client_has_slot = false;
    impl_client_upload = None;
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

let client_can_upload (client : client) s =
  let client = as_client_impl client in
  client.impl_client_ops.op_client_can_upload client.impl_client_val s

let client_debug (client : client) s =
  let client = as_client_impl client in
  client.impl_client_ops.op_client_debug client.impl_client_val s

let client_files client=
  let client = as_client_impl client in
  client.impl_client_ops.op_client_files client.impl_client_val

let client_bprint (client: client) buf =
  let client = as_client_impl client in
  client.impl_client_ops.op_client_bprint client.impl_client_val buf

let client_bprint_html (client: client) buf file =
  let client = as_client_impl client in
  client.impl_client_ops.op_client_bprint_html client.impl_client_val buf file

let client_dprint (client: client) o file =
  let client = as_client_impl client in
  client.impl_client_ops.op_client_dprint client.impl_client_val o file

let client_dprint_html (client: client) o file str =
  let client = as_client_impl client in
  client.impl_client_ops.op_client_dprint_html client.impl_client_val o file str
  
let client_connect client=
  let client = as_client_impl client in
  client.impl_client_ops.op_client_connect client.impl_client_val

let client_disconnect client=
  let client = as_client_impl client in
  client.impl_client_ops.op_client_disconnect client.impl_client_val

let client_clear_files client=
  let client = as_client_impl client in
  client.impl_client_ops.op_client_clear_files client.impl_client_val

let client_browse client immediate =
  let client = as_client_impl client in
  client.impl_client_ops.op_client_browse client.impl_client_val immediate

let client_enter_upload_queue client =
  let client = as_client_impl client in
  client.impl_client_ops.op_client_enter_upload_queue client.impl_client_val 
  
let ni n m = 
  let s = Printf.sprintf "Client.%s not implemented by %s" 
      m n.network_name in
  lprintf "%s\n" s;
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
      op_client_debug = (fun _ _ -> ni_ok network "client_debug");
      op_client_files = (fun _ -> fni network "client_files");
      op_client_connect  = (fun _ -> ni_ok network "client_connect");
      op_client_disconnect  = (fun _ -> ni_ok network "client_disconnect");
      op_client_clear_files = (fun _ -> ni_ok network "client_clear_files");
      op_client_browse = (fun _ _ -> ni_ok network "client_browse");
      op_client_bprint = (fun _ _ -> ni_ok network "client_bprint");
      op_client_bprint_html = (fun _ _ _ -> ni_ok network "client_bprint_html");
      op_client_dprint = (fun _ _ _ -> ni_ok network "client_dprint");
      op_client_dprint_html = (fun _ _ _ _ -> fni network "client_dprint_html");
      op_client_can_upload = (fun _ _ -> ni_ok network "client_can_upload");
      op_client_enter_upload_queue = (fun _ -> ni_ok network "client_enter_upload_queue");
    } in
  let cc = (Obj.magic c : int client_ops) in
  clients_ops := (cc, { cc with op_client_network = c.op_client_network })
  :: ! clients_ops;
  c

let check_client_implementations () =
  lprintf "\n---- Methods not implemented for CommonClient ----\n\n";
  List.iter (fun (c, cc) ->
      let n = c.op_client_network.network_name in
      lprintf "\n  Network %s\n\n" n; 
      if c.op_client_to_option == cc.op_client_to_option then 
        lprintf "op_client_to_option\n";
      if c.op_client_info == cc.op_client_info then
        lprintf "op_client_info\n";
      if c.op_client_say == cc.op_client_say then
        lprintf "op_client_say\n";
      if c.op_client_files == cc.op_client_files then
        lprintf "op_client_files\n";
      if c.op_client_disconnect == cc.op_client_disconnect then
        lprintf "op_client_disconnect\n";
      if c.op_client_connect == cc.op_client_connect then
        lprintf "op_client_connect\n";
      if c.op_client_clear_files == cc.op_client_clear_files then
        lprintf "op_client_clear_files\n";
      if c.op_client_browse == cc.op_client_browse then
        lprintf "op_client_browse\n";
  ) !clients_ops;
  lprint_newline () 
  
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

let uploaders = ref Intmap.empty

let client_has_a_slot c = 
  (as_client_impl c).impl_client_has_slot

let client_upload c =
  (as_client_impl c).impl_client_upload

let set_client_upload c sh =
  (as_client_impl c).impl_client_upload <- sh;
  client_must_update c
  
let set_client_has_a_slot c b = 
  let impl = as_client_impl c in
  if not b && impl.impl_client_has_slot then begin
      impl.impl_client_has_slot <- false;
      uploaders := Intmap.remove (client_num c) !uploaders;
      client_must_update c
    end
  else
  if b && not impl.impl_client_has_slot then  begin
      uploaders := Intmap.add (client_num c) c !uploaders;
      impl.impl_client_has_slot <- b;
      client_must_update c
    end
    
let set_client_disconnected c reason =
  let impl = as_client_impl c in
  set_client_has_a_slot c false;
  
  match impl.impl_client_state with
    Connected n -> set_client_state c (NotConnected (reason, n))
  | _ ->  set_client_state c (NotConnected (reason, -1))
    
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
  H.remove clients_by_num c;
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
  Heap.add_memstat "CommonClient" (fun level buf ->
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

let client_print_html c o =
  let impl = as_client_impl c in
  let i = client_info c in
  let n = impl.impl_client_ops.op_client_network in
  let info = client_info c in
  let buf = o.conn_buf in
	html_mods_td buf [
	("", "sr", n.network_name);
	("", "sr", (match info.G.client_kind with
          Indirect_location (name, _) -> Printf.sprintf "I"
        | Known_location (ip, port) -> Printf.sprintf "D") );
	(String.escaped info.G.client_name, "sr", client_short_name info.G.client_name); ]

let client_print c o =
  let impl = as_client_impl c in
  let i = client_info c in
  let n = impl.impl_client_ops.op_client_network in
  let info = client_info c in
  let buf = o.conn_buf in
  if use_html_mods o then begin
	html_mods_td buf [
	("", "sr", Printf.sprintf "%d" (client_num c));
	("", "sr", n.network_name);
	("", "sr", (try match info.G.client_kind with 
             Known_location (ip,port) -> Printf.sprintf "%s" (Ip.to_string ip)
           | Indirect_location _ -> Printf.sprintf "None"
           with _ -> ""));
	(String.escaped info.G.client_name, "sr", client_short_name info.G.client_name); ];
    end
  else begin
      Printf.bprintf buf "[%s%6d] Name  : %-27s Rating  : %-10d  IP   : %-20s"
        n.network_name
        (client_num c)
        (shorten info.G.client_name 20)
        i.GuiTypes.client_rating
        (match info.G.client_kind with
            Indirect_location (name, _) -> "firewalled"
            | Known_location (ip, port) ->
            Printf.sprintf "%s:%d" (Ip.to_string ip) port)
    end


let is_friend c =
  (client_type c) land client_friend_tag <> 0
  
let is_contact c = 
  (client_type c) land client_contact_tag <> 0
  
let set_friend c =
  set_client_type c (client_type c lor client_friend_tag)
  
let set_contact c =
  set_client_type c (client_type c lor client_contact_tag)
  
let set_not_friend c =
  set_client_type c (client_type c land (lnot client_friend_tag))
  
let set_not_contact c =
  set_client_type c (client_type c land (lnot client_contact_tag))
  
let is_nolimit c =
  (client_type c) land client_nolimit_tag <> 0
  
let set_nolimit c =
  set_client_type c (client_type c lor client_nolimit_tag)
  
let is_initialized c =
  client_type c land client_initialized_tag <> 0
    
let set_initialized c =
  set_client_type c (client_type c lor client_initialized_tag)
