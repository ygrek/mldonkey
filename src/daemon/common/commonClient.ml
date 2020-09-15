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

let log_prefix = "[cCl]"

let lprintf_nl fmt =
  lprintf_nl2 log_prefix fmt

type 'a client_impl = {
    mutable impl_client_type : client_type;
    mutable impl_client_state : host_state;
    mutable impl_client_update : int;
    mutable impl_client_slot : slot_kind;
    mutable impl_client_upload : file option;
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

    mutable op_client_dprint : ('a -> CommonTypes.ui_conn ->
        CommonTypes.file -> unit);

    mutable op_client_dprint_html : ('a -> CommonTypes.ui_conn ->
    CommonTypes.file -> string -> bool);

(* used to print network specific client infos *)
    mutable op_client_print_info : ('a -> CommonTypes.ui_conn -> unit);

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
    impl_client_slot = NoSlot;
    impl_client_upload = None;
    impl_client_num = 0;
    impl_client_val = 0;
    impl_client_ops = Obj.magic None;
  }

let dummy_client = as_client dummy_client_impl

module H = Weak.Make(struct
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

let client_dprint (client: client) o file =
  let client = as_client_impl client in
  client.impl_client_ops.op_client_dprint client.impl_client_val o file

let client_dprint_html (client: client) o file str =
  let client = as_client_impl client in
  client.impl_client_ops.op_client_dprint_html client.impl_client_val o file str

let client_print_info (client: client) o =
  let c = as_client_impl client in
  c.impl_client_ops.op_client_print_info c.impl_client_val o

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
  if CommonNetwork.network_is_enabled
     ((as_client_impl client).impl_client_ops.op_client_network) then
  let client = as_client_impl client in
  client.impl_client_ops.op_client_browse client.impl_client_val immediate

let client_enter_upload_queue client =
  let client = as_client_impl client in
  client.impl_client_ops.op_client_enter_upload_queue client.impl_client_val

let ni n m =
  let s = Printf.sprintf "Client.%s not implemented by %s"
      m n.network_name in
  lprintf_nl "%s" s;
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
      op_client_dprint = (fun _ _ _ -> ni_ok network "client_dprint");
      op_client_dprint_html = (fun _ _ _ _ -> fni network "client_dprint_html");
      op_client_print_info = (fun _ _ -> fni network "client_print_info");
      op_client_can_upload = (fun _ _ -> ni_ok network "client_can_upload");
      op_client_enter_upload_queue = (fun _ -> ni_ok network "client_enter_upload_queue");
    } in
  let cc = (Obj.magic c : int client_ops) in
  clients_ops := (cc, { cc with op_client_network = c.op_client_network })
  :: ! clients_ops;
  c

let check_client_implementations () =
  lprintf_nl "\n---- Methods not implemented for CommonClient ----\n";
  List.iter (fun (c, cc) ->
      let n = c.op_client_network.network_name in
      lprintf_nl "\n  Network %s\n" n;
      if c.op_client_to_option == cc.op_client_to_option then
        lprintf_nl "op_client_to_option";
      if c.op_client_info == cc.op_client_info then
        lprintf_nl "op_client_info";
      if c.op_client_say == cc.op_client_say then
        lprintf_nl "op_client_say";
      if c.op_client_files == cc.op_client_files then
        lprintf_nl "op_client_files";
      if c.op_client_disconnect == cc.op_client_disconnect then
        lprintf_nl "op_client_disconnect";
      if c.op_client_connect == cc.op_client_connect then
        lprintf_nl "op_client_connect";
      if c.op_client_clear_files == cc.op_client_clear_files then
        lprintf_nl "op_client_clear_files";
      if c.op_client_browse == cc.op_client_browse then
        lprintf_nl "op_client_browse";
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

let client_type c =
  let impl = as_client_impl c in
  impl.impl_client_type

let set_client_type c t =
  let impl = as_client_impl c in
  if impl.impl_client_type <> t then begin
      impl.impl_client_type <- t;
      client_must_update c
    end

let is_friend c =
  (client_type c) land client_friend_tag <> 0

let is_contact c =
  (client_type c) land client_contact_tag <> 0

let is_must_browse c =
  (client_type c) land client_must_browse_tag <> 0

let set_friend c =
  set_client_type c (client_type c lor client_friend_tag)

let set_contact c =
  set_client_type c (client_type c lor client_contact_tag)

let set_must_browse c =
  set_client_type c (client_type c lor client_must_browse_tag)

let set_not_friend c =
  set_client_type c (client_type c land (lnot client_friend_tag))

let set_not_contact c =
  set_client_type c (client_type c land (lnot client_contact_tag))

let set_not_must_browse c =
  set_client_type c (client_type c land (lnot client_must_browse_tag))

let is_nolimit c =
  (client_type c) land client_nolimit_tag <> 0

let set_nolimit c =
  set_client_type c (client_type c lor client_nolimit_tag)

let is_initialized c =
  client_type c land client_initialized_tag <> 0

let set_initialized c =
  set_client_type c (client_type c lor client_initialized_tag)

let client_slot c =
  (as_client_impl c).impl_client_slot

let client_has_a_slot c =
  match (as_client_impl c).impl_client_slot with
    NoSlot -> false
  | _ -> true

let client_upload c =
  (as_client_impl c).impl_client_upload

let set_client_upload c sh =
  (as_client_impl c).impl_client_upload <- Some sh;
  client_must_update c

let set_client_has_a_slot c slot =
  let impl = as_client_impl c in
  match slot with
    NoSlot -> if client_has_a_slot c then begin
      impl.impl_client_slot <- NoSlot;
      uploaders := Intmap.remove (client_num c) !uploaders;
      client_must_update c;
(*
TODO
If an upload slot is closed the previously uploaded file
is still kept open. There should be a check if other users
are currently uploading that file, if not close it.
Until this is coded all files are closed, it does not harm
the work of the core but avoids locking files which makes
them unaccessable on Windows.
*)
      Unix32.close_all ()
    end
  | slot -> if not (client_has_a_slot c) then begin
      uploaders := Intmap.add (client_num c) c !uploaders;
      impl.impl_client_slot <- slot;
      client_must_update c
    end

let set_client_disconnected c reason =
  let impl = as_client_impl c in
  set_client_has_a_slot c NoSlot;

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

let disconnect_all_clients () =
  H.iter (fun c -> client_disconnect c) clients_by_num;
  Intmap.iter (fun _ c -> client_disconnect c) !uploaders

let _ =
  Heap.add_memstat "CommonClient" (fun level buf ->
      let counter = ref 0 in
      H.iter (fun _ -> incr counter) clients_by_num;
      Printf.bprintf buf "  clients: %d\n" !counter;
      Printf.bprintf buf "  uploaders: %d\n" (Intmap.length !uploaders);
  )

let clients_get_all () =
  let list = ref [] in
  H.iter (fun c ->
      list := (client_num c) :: !list) clients_by_num;
  !list

(* the next line is to prevent any code after it from
   directly accessing the hashtable.
 *)
let clients_by_num = ()

let client_new_file (client :client) (dirname:string) r =
  CommonEvent.add_event (Client_new_file_event
    (client, dirname, (r : result)))

module G = GuiTypes

let client_print_html c o =
  let impl = as_client_impl c in
  let n = impl.impl_client_ops.op_client_network in
  let info = client_info c in
  let buf = o.conn_buf in
        html_mods_td buf [
        ("", "sr", n.network_name);
        ("", "sr", (match info.G.client_kind with
          Indirect_location (_ , _, _, _) -> Printf.sprintf "I"
        | Known_location (ip, port) -> Printf.sprintf "D") );
        (String.escaped info.G.client_name, "sr", client_short_name info.G.client_name); ]

let client_print c o =
  let impl = as_client_impl c in
  let n = impl.impl_client_ops.op_client_network in
  let info = client_info c in
  let buf = o.conn_buf in
  let ips,cc,cn = string_of_kind_geo info.G.client_kind info.G.client_country_code in
  if use_html_mods o then begin
        html_mods_td buf ([
        ("", "sr", Printf.sprintf "%d" (client_num c));
        ("", "sr", n.network_name);
        ("", "sr", ips);
        ] @ (if Geoip.active () then [(cn, "sr", CommonPictures.flag_html cc)] else []) @ [
        (String.escaped info.G.client_name, "sr", client_short_name info.G.client_name); ]);
    end
  else begin
      Printf.bprintf buf "[%s%6d] Name  : %-27s Rating  : %-10d  IP   : %-20s"
        n.network_name
        (client_num c)
        (shorten info.G.client_name 20)
        info.GuiTypes.client_rating
        (match info.G.client_kind with
              Indirect_location (_, _, ip, port)
            | Known_location (ip, port) ->
                Printf.sprintf "%s:%d" (Ip.to_string ip) port)
    end


let client_has_bitmap (client : client) (file : file) bitmap =
  CommonEvent.add_event (File_update_availability (file, client, bitmap))

let clear_upload_slots () =
  if !CommonGlobals.has_upload = 0 then
  Intmap.iter (fun _ c ->
    try
      let i = client_info c in
      let ctime = ((BasicSocket.last_time ()) - i.GuiTypes.client_connect_time) / 60 in
      if i.GuiTypes.client_session_uploaded = Int64.zero && ctime > 1 then
        begin
          client_disconnect c;
          if !verbose then lprintf_nl "disconnected client %d: [%s %s] %s after %d minute%s of silence."
            (client_num c)
            (GuiTypes.client_software i.GuiTypes.client_software i.GuiTypes.client_os)
            i.GuiTypes.client_release
            i.GuiTypes.client_name
            ctime (Printf2.print_plural_s ctime)
        end
    with _ -> ()
  ) !uploaders
  else
  Intmap.iter (fun _ c ->
    let i = client_info c in
    client_disconnect c;
    if !verbose then lprintf_nl "disconnected client %d: [%s %s] %s after user disabled upload."
      (client_num c)
      (GuiTypes.client_software i.GuiTypes.client_software i.GuiTypes.client_os)
      i.GuiTypes.client_release i.GuiTypes.client_name
  ) !uploaders

let impl_client_info impl =
  let module T = GuiTypes in
  {
     T.client_num = impl.impl_client_num;
     T.client_state = impl.impl_client_state;
     T.client_type = impl.impl_client_type;
  
     T.client_tags = [];
     T.client_name = "";
     T.client_country_code = None;
     T.client_network = 0;
     T.client_rating = 0;
     T.client_chat_port = 0;
     T.client_connect_time = BasicSocket.last_time ();
     T.client_kind = Indirect_location ("", Md4.Md4.null, Ip.null, 0);
     T.client_software = "";
     T.client_os = None;
     T.client_release = "";
     T.client_emulemod = "";
     T.client_total_downloaded = 0L;
     T.client_total_uploaded = 0L;
     T.client_session_downloaded = 0L;
     T.client_session_uploaded = 0L;
     T.client_upload = None;
     T.client_sui_verified = None;
     T.client_file_queue = [];
  }
