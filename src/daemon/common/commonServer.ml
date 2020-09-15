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

open Printf2
open CommonOptions
open CommonNetwork
open Options
open CommonUser
open CommonTypes

let log_prefix = "[cSe]"

let lprintf_nl fmt =
  lprintf_nl2 log_prefix fmt

let lprintf_n fmt =
  lprintf2 log_prefix fmt

module G = GuiTypes

type 'a server_impl = {
    mutable impl_server_update : int;
    mutable impl_server_state : CommonTypes.host_state;
    mutable impl_server_num : int;
    mutable impl_server_sort : int;
    mutable impl_server_val : 'a;
    mutable impl_server_ops : 'a server_ops;
  }

and 'a server_ops = {
    mutable op_server_network : network;
    mutable op_server_to_option : ('a -> (string * option_value) list);
    mutable op_server_remove : ('a -> unit);
    mutable op_server_info : ('a -> GuiTypes.server_info);
    mutable op_server_sort : ('a -> int);
    mutable op_server_connect : ('a -> unit);
    mutable op_server_disconnect : ('a -> unit);
    mutable op_server_users : ('a -> user list);
    mutable op_server_published : ('a -> file list);
    mutable op_server_query_users : ('a -> unit);
    mutable op_server_find_user : ('a -> string -> unit);
    mutable op_server_cid : ('a -> Ip.t);
    mutable op_server_low_id : ('a -> bool);
    mutable op_server_set_preferred : ('a -> bool -> unit);
    mutable op_server_rename : ('a -> string -> unit);
  }

let ni n m =
  let s = Printf.sprintf "Server.%s not implemented by %s"
      m n.network_name in
  lprintf_nl "%s" s;
  s

let fni n m =  failwith (ni n m)
let ni_ok n m = ignore (ni n m)

let as_server  (server : 'a server_impl) =
  let (server : server) = Obj.magic server in
  server

let as_server_impl  (server : server) =
  let (server : 'a server_impl) = Obj.magic server in
  server

let dummy_server_impl = {
    impl_server_update = 1;
    impl_server_state = NewHost;
    impl_server_num = 0;
    impl_server_sort = 0;
    impl_server_val = 0;
    impl_server_ops = Obj.magic None;
  }

let dummy_server = as_server dummy_server_impl

let impl_server_info impl =
  let module T = GuiTypes in
  {
    T.server_num = impl.impl_server_num;
    T.server_state = impl.impl_server_state;

    T.server_network = 0;
    T.server_addr = Ip.addr_of_ip Ip.null;
    T.server_port = 0;
    T.server_realport = 0;
    T.server_country_code = None;
    T.server_score = 0;
    T.server_tags = [];
    T.server_nusers = 0L;
    T.server_nfiles = 0L;
    T.server_name = "";
    T.server_description = "";
    T.server_users = None;
    T.server_banner = "";
    T.server_preferred = false;
    T.server_master = false;
    T.server_version = "";
    T.server_max_users = 0L;
    T.server_soft_limit = 0L;
    T.server_hard_limit = 0L;
    T.server_lowid_users = 0L;
    T.server_ping = 0;
    T.server_published_files = 0;
    T.server_features = None;
  }

let server_num s =
  let s = as_server_impl s in
  s.impl_server_num

module H = Weak.Make(struct
      type t = server
      let hash s = Hashtbl.hash (server_num s)

      let equal x y =
        (server_num x) = (server_num y)
    end)

let server_counter = ref 0
let servers_by_num = H.create 1027

let _ =
  Heap.add_memstat "CommonServer" (fun level buf ->
      let counter = ref 0 in
      H.iter (fun _ -> incr counter) servers_by_num;
      Printf.bprintf buf "  servers: %d\n" !counter;
  )

let server_must_update s =
  let impl = as_server_impl s in
  if impl.impl_server_update <> 0 then
    CommonEvent.add_event (Server_info_event s);
  impl.impl_server_update <- 0

let server_must_update_state s =
  let impl = as_server_impl s in
  if impl.impl_server_update > 0 then
    begin
      impl.impl_server_update <- - impl.impl_server_update;
      CommonEvent.add_event (Server_info_event s);
    end

let server_update_num impl =
  let server = as_server impl in
  incr server_counter;
  impl.impl_server_num <- !server_counter;
  server_must_update server;
  H.add servers_by_num server

let server_to_option (server : server) =
  let server = as_server_impl server in
  server.impl_server_ops.op_server_to_option server.impl_server_val

let server_network (server : server) =
  let server = as_server_impl server in
  server.impl_server_ops.op_server_network

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

let server_published s =
  let s = as_server_impl s in
  s.impl_server_ops.op_server_published s.impl_server_val

let server_cid s =
  let s = as_server_impl s in
  s.impl_server_ops.op_server_cid s.impl_server_val

let server_low_id s =
  let s = as_server_impl s in
  s.impl_server_ops.op_server_low_id s.impl_server_val

let server_set_preferred s b =
  let s = as_server_impl s in
  s.impl_server_ops.op_server_set_preferred s.impl_server_val b

let server_rename s name =
  let s = as_server_impl s in
  s.impl_server_ops.op_server_rename s.impl_server_val name

let servers_ops = ref []
let new_server_ops network =
  let s = {
      op_server_network =  network;
      op_server_remove = (fun _ -> ni_ok network "server_remove");
(*    op_server_print = (fun _ _ -> ni_ok network "server_print"); *)
      op_server_to_option = (fun _ -> fni network "server_to_option");
      op_server_info = (fun _ -> fni network "server_info");
      op_server_sort = (fun _ -> ni_ok network "server_sort"; 0);
      op_server_connect = (fun _ -> ni_ok network "server_connect");
      op_server_disconnect = (fun _ -> ni_ok network "server_disconnect");
      op_server_find_user = (fun _ -> fni network "find_user");
      op_server_query_users = (fun _ -> ni_ok network "query_users");
      op_server_published = (fun _ -> fni network "published");
      op_server_users = (fun _ -> fni network "users");
      op_server_cid = (fun _ -> fni network "cid");
      op_server_low_id = (fun _ -> fni network "low_id");
      op_server_set_preferred = (fun _ _ -> fni network "server_set_preferred");
      op_server_rename = (fun _ _ -> fni network "server_rename");
    } in
  let ss = (Obj.magic s : int server_ops) in
  servers_ops := (ss, { ss with op_server_network = s.op_server_network })
  :: ! servers_ops;
  s

let check_server_implementations () =
  lprintf_nl "----- Methods not implemented for CommonServer ----";
  List.iter (fun (c, cc) ->
      let n = c.op_server_network.network_name in
      lprintf_nl "  Network %s" n;
      if c.op_server_remove == cc.op_server_remove then
        lprintf_nl "op_server_remove";
      if c.op_server_to_option == cc.op_server_to_option then
        lprintf_nl "op_server_to_option";
      if c.op_server_info == cc.op_server_info then
        lprintf_nl "op_server_info";
      if c.op_server_sort == cc.op_server_sort then
        lprintf_nl "op_server_sort";
      if c.op_server_connect == cc.op_server_connect then
        lprintf_nl "op_server_connect";
      if c.op_server_disconnect == cc.op_server_disconnect then
        lprintf_nl "op_server_disconnect";
      if c.op_server_find_user == cc.op_server_find_user then
        lprintf_nl "op_server_find_user";
      if c.op_server_query_users == cc.op_server_query_users then
        lprintf_nl "op_server_query_users";
      if c.op_server_users == cc.op_server_users then
        lprintf_nl "op_server_users";
      if c.op_server_cid == cc.op_server_cid then
        lprintf_nl "op_server_cid";
      if c.op_server_low_id == cc.op_server_low_id then
        lprintf_nl "op_server_low_id";
      if c.op_server_rename == cc.op_server_rename then
        lprintf_nl "op_server_rename";
      if c.op_server_set_preferred == cc.op_server_set_preferred then
        lprintf_nl "op_server_set_preferred";
  ) !servers_ops;
  lprint_newline ()

let server_find (num : int) =
  H.find servers_by_num  (as_server { dummy_server_impl with
      impl_server_num = num })

let server_blocked s =
  let info = server_info s in
  !Ip.banned (Ip.ip_of_addr info.G.server_addr, info.G.server_country_code) <> None

let server_connect s =
  if not (server_blocked s) then
  let server = as_server_impl s in
  server.impl_server_ops.op_server_connect server.impl_server_val

let server_disconnect s =
  let server = as_server_impl s in
  server.impl_server_ops.op_server_disconnect server.impl_server_val

let server_state c =
  let impl = as_server_impl c in
  impl.impl_server_state

let set_server_state c state =
  let impl = as_server_impl c in
  if impl.impl_server_state <> state then begin
      impl.impl_server_state <- state;
      server_must_update_state c
    end

let server_sort () =
  let list = ref [] in
  H.iter (fun s ->
      let impl = as_server_impl s in
      match impl.impl_server_state with
        RemovedHost -> ()
      | _ ->
          list := s :: !list;
          impl.impl_server_sort <-
            (try impl.impl_server_ops.op_server_sort impl.impl_server_val
            with _ -> 0);
  ) servers_by_num;
  List.sort (fun s1 s2 ->
    compare (as_server_impl s2).impl_server_sort (as_server_impl s1).impl_server_sort
  ) !list

let server_iter f =
  H.iter f servers_by_num

let com_servers_by_num = servers_by_num

let server_new_user server user =
  user_must_update user;
  CommonEvent.add_event (Server_new_user_event (server, user))

let servers_get_all () =
  let list = ref [] in
  H.iter (fun c ->
      list := (server_num c) :: !list) servers_by_num;
  !list

let servers_by_num = ()

let check_blocked_servers () =
  try
    server_iter (fun s ->
      if server_blocked s then
        begin
          let impl = as_server_impl s in
          let info = server_info s in
          (match impl.impl_server_state with
             NotConnected _ -> ()
         | _ -> server_disconnect s;
              lprintf_nl "Disconnected server %s (%s:%d), IP is now blocked"
                info.G.server_name
                (Ip.string_of_addr info.G.server_addr)
                info.G.server_port);
      end;
      server_must_update s)
  with
    Not_found -> ()
  | e -> lprintf_nl "Exception in check_blocked_servers: %s" (Printexc2.to_string e)

let disconnect_all_servers () =
  try
    server_iter (fun s ->
     (match (as_server_impl s).impl_server_state with
        NotConnected _ -> ()
      | _ -> server_disconnect s; server_must_update s))
  with
    Not_found -> ()
  | e -> lprintf_nl "Exception in disconnect_all_servers: %s" (Printexc2.to_string e)

let server_must_update_all () =
  try
    server_iter (fun s ->
      server_must_update s)
  with e ->
    lprintf_nl "Exception in server_must_update_all: %s" (Printexc2.to_string e)

let server_banner s o =
  let buf = o.conn_buf in
  let info = server_info s in
  Printf.bprintf buf "%s"
      info.G.server_banner

let server_print_html_header buf ext =
     if !!html_mods_use_js_tooltips then Printf.bprintf buf
"\\<div id=\\\"object1\\\" style=\\\"position:absolute; background-color:#FFFFDD;color:black;border-color:black;border-width:20px;font-size:8pt; visibility:visible; left:25px; top:
-100px; z-index:+1\\\" onmouseover=\\\"overdiv=1;\\\"  onmouseout=\\\"overdiv=0; setTimeout(\\\'hideLayer()\\\',1000)\\\"\\>\\&nbsp;\\</div\\>";

    html_mods_table_header buf "serversTable" (Printf.sprintf "servers%s" ext) ([
    ( Num, "srh", "Server number", "#" ) ;
    ( Str, "srh", "Connect|Disconnect", "C/D" ) ;
    ( Str, "srh", "Remove", "Rem" ) ;
    ( Str, "srh", "Preferred", "P" ) ;
    ( Str, "srh", "Master servers", "M" ) ;
    ( Str, "srh", "[Hi]gh or [Lo]w ID", "ID" ) ;
    ( Str, "srh", "Network name", "Network" ) ;
    ( Str, "srh", "Connection status", "Status" ) ;
    ( Str, "srh br", "IP address", "IP address" ) ;
    ] @ (if Geoip.active () then [( Str, "srh br", "Country Code/Name", "CC" )] else []) @ [
    ( Num, "srh ar", "Number of connected users", "Users" ) ;
    ( Num, "srh ar br", "Max number of users", "MaxUsers" ) ;
    ( Num, "srh ar br", "LowID users", "LowID" ) ;
    ( Num, "srh ar", "Number of files indexed on server", "Files" );
    ( Num, "srh ar br", "Number of published files on server", "Publ" );
    ( Num, "srh ar", "Soft file limit", "Soft" ) ;
    ( Num, "srh ar br", "Hard file limit", "Hard" ) ;
    ( Num, "srh ar br", "Ping (ms)", "Ping" ) ;
    ( Str, "srh br", "Server version", "Version" ) ;
    ( Str, "srh", "Server name", "Name" ) ;
    ( Str, "srh", "Server details", "Details" ) ])

let server_print s o =
  let impl = as_server_impl s in
  let n = impl.impl_server_ops.op_server_network in
  if network_is_enabled n then
  try
    let info =
      try server_info s with e ->
          lprintf_nl "Exception %s in server_info (%s)\n"
            (Printexc2.to_string e) n.network_name;
          raise e in
    let cc,cn = Geoip.get_country_code_name info.G.server_country_code in
    let buf = o.conn_buf in
  
  if use_html_mods o then begin
    let snum = (server_num s) in
    let ip_port_string = 
      Printf.sprintf "%s:%s%s" 
      (Ip.string_of_addr info.G.server_addr)
       (string_of_int info.G.server_port)
       (if info.G.server_realport <> 0 
          then "(" ^ (string_of_int info.G.server_realport) ^ ")" 
            else ""
        )
    in

    Printf.bprintf buf "\\<tr class=\\\"dl-%d\\\""
    (html_mods_cntr ());

    if !!html_mods_use_js_tooltips then
       Printf.bprintf buf " onMouseOver=\\\"mOvr(this);setTimeout('popLayer(\\\\\'%s %s<br>%s\\\\\')',%d);setTimeout('hideLayer()',%d);return true;\\\" onMouseOut=\\\"mOut(this);hideLayer();setTimeout('hideLayer()',%d)\\\"\\>"
        info.G.server_name ip_port_string 
        (match info.G.server_features with
        | None -> ""
        | Some f -> "server features: " ^ f)
       !!html_mods_js_tooltips_wait
       !!html_mods_js_tooltips_timeout
       !!html_mods_js_tooltips_wait
    else Printf.bprintf buf "\\>";

    Printf.bprintf buf " \\<td class=\\\"srb\\\" %s \\>%d\\</td\\> %s %s %s"
    (match impl.impl_server_state with
        Connected _ -> 
            Printf.sprintf "title=\\\"Server Banner\\\"
            onClick=\\\"location.href='submit?q=server_banner+%d'\\\"" 
            snum
        | _ -> "")
    snum
      (
     let not_connected =
        match impl.impl_server_state with
        | ServerFull
        | NotConnected _ -> true
        | _ -> false 
      in 
      if server_blocked s && not_connected 
        then "\\<td class=\\\"srb\\\"\\>blk\\</td\\>" 
        else Printf.sprintf
        "\\<td class=\\\"srb\\\" title=\\\"Connect|Disconnect\\\"
        onClick=\\\"parent.fstatus.location.href='submit?q=%s+%d'\\\"\\>%s\\</td\\>"
              (if not_connected then "c" else "x")
        snum
              (if not_connected then "Conn" else "Disc")
      )
      (
        Printf.sprintf
        "\\<td class=\\\"srb\\\" title=\\\"Remove server\\\"
        onClick=\\\"parent.fstatus.location.href='submit?q=rem+%d'\\\"\\>Rem\\</td\\>"
      snum
      )
      (
        if info.G.server_preferred then begin
        Printf.sprintf
        "\\<td class=\\\"srb\\\" title=\\\"Unset preferred\\\"
        onClick=\\\"parent.fstatus.location.href='submit?q=preferred+false+%s'\\\"\\>P\\</td\\>"
        (Ip.string_of_addr info.G.server_addr)
        end else begin
        Printf.sprintf
        "\\<td class=\\\"srb\\\" title=\\\"Set preferred\\\"
        onClick=\\\"parent.fstatus.location.href='submit?q=preferred+true+%s'\\\"\\>-\\</td\\>"
        (Ip.string_of_addr info.G.server_addr)
        end
      );

      let id_title, id_text = 
        match n.network_name with
          "Donkey" -> begin
           match impl.impl_server_state with
                Connected _ -> begin
                 let cid = (server_cid s) in
                 let (label,shortlabel,our_ip) =
                   if not (server_low_id s) then
                     ("HighID","Hi",
                      (if !!set_client_ip <> cid then
                         Printf.sprintf "(clientIP: %s)"
                           (Ip.to_string !!set_client_ip)
                       else ""))
                    else ("LowID","Lo","")
                 in
                   Printf.sprintf "%s: %s = %s %s"
                      label
                      (Int64.to_string (Ip.to_int64 (Ip.rev cid)))
                      (Ip.to_string cid)
                      our_ip
                 ,shortlabel
               end
               | _ -> "",""
         end
      | _ -> "",""
    in

    let server_state_string = 
      match impl.impl_server_state with
        NotConnected _ when server_blocked s -> "IP blocked"
      | _ -> string_of_connection_state impl.impl_server_state
    in

    html_mods_td buf ([
      ("", "srb", if info.G.server_master then "M" else "-");
      (id_title, "sr", id_text);
      ("", "sr", n.network_name);
      ("", "sr", server_state_string);
      ("", "sr br", ip_port_string);
      ] @ (if Geoip.active () then [(cn, "sr br", CommonPictures.flag_html cc)] else []) @ [
      ("", "sr ar", if info.G.server_nusers = Int64.zero then "" else Printf.sprintf "%Ld" info.G.server_nusers);
      ("", "sr ar br", if info.G.server_max_users = Int64.zero then "" else Printf.sprintf "%Ld" info.G.server_max_users);
      ("", "sr ar br", if info.G.server_lowid_users = Int64.zero then "" else Printf.sprintf "%Ld" info.G.server_lowid_users);
      ("", "sr ar", if info.G.server_nfiles = Int64.zero then "" else Printf.sprintf "%Ld" info.G.server_nfiles)]);

    if info.G.server_published_files = 0 then
      html_mods_td buf ([("", "sr br", "")])
    else
      Printf.bprintf buf
"\\<TD class=\\\"sr br\\\" title=\\\"Show published files\\\"
onClick=\\\"location.href='submit?q=server_shares+%d'\\\"\\>%d\\</TD\\>"
        snum info.G.server_published_files;

    html_mods_td buf ([
      ("", "sr ar", if info.G.server_soft_limit = Int64.zero then "" else Printf.sprintf "%Ld" info.G.server_soft_limit);
      ("", "sr ar br", if info.G.server_hard_limit = Int64.zero then "" else Printf.sprintf "%Ld" info.G.server_hard_limit);
      ("", "sr ar br", if info.G.server_ping = 0 then "" else Printf.sprintf "%d" info.G.server_ping);
      ("", "sr br", info.G.server_version);
      ("", "sr", info.G.server_name);
    ]);

    Printf.bprintf buf "\\<td width=\\\"100%%\\\" class=\\\"sr\\\"\\>%s\\</td\\>\\</tr\\>\n"
      info.G.server_description;
  end
   else
  begin

        Printf.bprintf buf "[%-10s%5d] %15s:%-10s %s\n%45sUsers:%-8Ld Files:%-8Ld State:%s\n"
          (n.network_name)
          (server_num s)
          (Ip.string_of_addr info.G.server_addr)
          (Printf.sprintf "%s%s"
          (string_of_int info.G.server_port)
          (if info.G.server_realport <> 0 
            then "(" ^ (string_of_int info.G.server_realport) ^ ")" 
            else ""))
          (info.G.server_name)
          (if Geoip.active () then Printf.sprintf "%33s/%-2s%9s" cn cc "" else "")
          (info.G.server_nusers)
          (info.G.server_nfiles)
          (if server_blocked s 
            then "IP blocked" 
            else (string_of_connection_state impl.impl_server_state));
      end;

  with e ->
      lprintf_nl "Exception %s in CommonServer.server_print"
        (Printexc2.to_string e)
