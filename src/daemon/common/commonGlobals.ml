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
open Options
open CommonOptions
open BasicSocket
open CommonTypes
open UdpSocket
open TcpBufferedSocket

let zero = Int64.zero
let one = Int64.one
let (++) = Int64.add
let (--) = Int64.sub
let ( ** ) x y = Int64.mul x (Int64.of_int y)
let ( // ) x y = Int64.div x y
  
let networks_string = ref ""
  
let version () = 
  Printf.sprintf "MLNet %s: Multi-Network p2p client (%s)"  
    Autoconf.current_version !networks_string
  
  
(* Should we try to find another port when we cannot bind to the one set
in an option, and then change the option accordingly. ?> *)
let find_other_port = ref false

  
let find_port server_name bind_addr port_option handler =
  if !!port_option <> 0 then
    let rec iter port =
      try
        let sock = TcpServerSocket.create server_name
            (Ip.to_inet_addr bind_addr)
          port handler in
        port_option =:= port;
        Some sock
      with e ->
          if !find_other_port then iter (port+1)
          else begin
              lprintf "Exception %s while starting %s\n" server_name
                (Printexc2.to_string e);
              None
            end
    in
    iter !!port_option
  else None
  
let one_day = 3600. *. 24.
let half_day = one_day /. 2.

let printf_char c =
  if !verbose then 
    (lprint_char c)
    
let printf_string c =
  if !verbose then 
    (lprint_string c)

let minutes25 = 25 * 60
  
let new_connection_control () = {
    control_last_ok = 0;
    control_state = 0;
    control_last_try = 0;
    control_min_reask = !!min_reask_delay;
  }
  
let new_connection_control_recent_ok () = {
    control_last_ok = last_time () - minutes25;
    control_state = 0;
    control_last_try = 0;
    control_min_reask = !!min_reask_delay;
  }
  
let connection_ok cc = 
  cc.control_last_ok <- last_time ();
  cc.control_state <- 0
  
let connection_try cc =
  cc.control_last_try <- last_time ()

let connection_failed cc =
  cc.control_state <- cc.control_state + 1

let connection_next_try cc =
  cc.control_last_try + mini (cc.control_min_reask * cc.control_state)
  cc.control_min_reask

let connection_can_try cc =
  connection_next_try cc < last_time ()

let print_control c =
  lprintf "Connection Control: ok = %d seconds ago, state = %d, last tried = %d seconds ago, delay = %d, next in %d seconds\n"
    (last_time () - c.control_last_ok) c.control_state (last_time () - c.control_last_try) c.control_min_reask (connection_next_try c - last_time ())
  
let connection_must_try cc =
  cc.control_state <- 0

let connection_set_last_conn cc lc =
  cc.control_last_ok <- lc

let connection_last_conn cc =
  cc.control_last_ok

let connection_delay cc = 
  cc.control_last_try <- last_time ();
  cc.control_state <- 0
  
let upload_control = TcpBufferedSocket.create_write_bandwidth_controler 
    (!!max_hard_upload_rate * 1024)
  
let download_control = TcpBufferedSocket.create_read_bandwidth_controler 
    (!!max_hard_download_rate * 1024)

      
let _ =
  option_hook max_hard_upload_rate (fun _ -> 
      TcpBufferedSocket.change_rate upload_control 
        (!!max_hard_upload_rate * 1024));  
  option_hook max_hard_download_rate (fun _ ->
      let rate = !!max_hard_download_rate in
      TcpBufferedSocket.change_rate download_control 
        (rate * 1024))  

let udp_write_controler = UdpSocket.new_bandwidth_controler upload_control

let udp_read_controler = UdpSocket.new_bandwidth_controler download_control
  
let can_open_connection () =
  let ns = nb_sockets () in
  let max = mini !!max_opened_connections MlUnix.max_sockets in
  (*
  if !!debug_net then begin
      lprintf "CAN OPEN (conns: %d < %d && upload U/D: %d %d)\n" ns max 
        (UdpSocket.remaining_bytes udp_write_controler)
        (UdpSocket.remaining_bytes udp_read_controler);
    end; *)
  ns < max
  
(*
  
    
val verify_ip : TcpBufferedSocket.t -> unit

  
val ip_verified : int ref
  
  
val can_open_connection : unit -> bool
      
val new_connection_control : float -> CommonTypes.connection_control
val connection_ok : CommonTypes.connection_control -> unit
val connection_failed : CommonTypes.connection_control -> unit
val connection_can_try : CommonTypes.connection_control -> bool
val connection_must_try : CommonTypes.connection_control -> unit
val connection_set_last_conn : CommonTypes.connection_control -> float -> unit
val connection_last_conn :  CommonTypes.connection_control -> float
val connection_try : CommonTypes.connection_control -> unit
val connection_delay : CommonTypes.connection_control -> unit
  
val printf_char : char -> unit
val printf_string : string -> unit
  
val one_day : float
val half_day : float

val upload_control : TcpBufferedSocket.bandwidth_controler
val download_control : TcpBufferedSocket.bandwidth_controler

*)
  
let gui_server_sock = ref (None : TcpServerSocket.t option)
      
let exit_handlers = ref []
let do_at_exit f =
  exit_handlers := f :: !exit_handlers
      
let exit_properly n =
(*  lprintf "exit_properly handlers\n"; *)
  List.iter (fun f -> try 
(*        lprintf "exit_properly handler ...\n";  *)
        f () ;
(*        lprintf "exit_properly done\n"; *)
      with e -> 
          lprintf "exit_properly (exception %s)"
            (Printexc2.to_string e); lprint_newline ();
  ) !exit_handlers;
(*  lprintf "exit_properly DONE\n"; *)
  Pervasives.exit n

let user_socks = ref ([] : TcpBufferedSocket.t list)
let dialog_history = ref ([] : (int * string * string) list )
  
  
let want_and_not andnot f none value =
(*   lprintf "want_and_not [%s]\n" value; *)
  let ws = String2.split_simplify value ' ' in
  if ws = [] then raise Not_found;
  let wanted = ref "" in
  let not_wanted = ref "" in
  List.iter (fun w ->
      let len = String.length w in
      if len>1 && w.[0] = '-' then
        let w = String.sub w 1 (len-1) in
        if !not_wanted = "" then not_wanted := w
        else not_wanted := !not_wanted ^ " " ^ w
      else
      if !wanted = "" then wanted := w
      else wanted := !wanted ^ " " ^ w
  ) ws;
  let wanted = if !wanted <> "" then f !wanted else none in
  if !not_wanted = "" then wanted else
    andnot wanted  (f !not_wanted)

let want_comb_not andnot comb f none value =
(*  lprintf "want_comb_not [%s]\n" value; *)
  let ws = String2.split_simplify value ' ' in
  let wanted = ref [] in
  let not_wanted = ref [] in
  List.iter (fun w ->
      let len = String.length w in
      if len>1 && w.[0] = '-' then
        let w = String.sub w 1 (len-1) in
        not_wanted := w :: !not_wanted
      else wanted := w :: !wanted
  ) ws;
  let wanted = match !wanted with
      [] -> none
    | w :: tail -> 
        List.fold_left (fun q w ->
            comb q  (f w)
        ) (f w) tail
  in
  match !not_wanted with
    [] -> wanted
  | w :: tail ->
      andnot wanted
        (List.fold_left (fun q w ->
            comb q  (f w)
        ) (f w) tail)

      
let rec rec_simplify_query q =
  match q with
    QAnd (q1, q2) ->
      (
       match (rec_simplify_query q1, rec_simplify_query q2) with
	QNone, QNone -> QNone
       | QNone, q2' -> q2'
       | q1', QNone -> q1'
       | q1', q2' -> QAnd (q1',q2')
      )
  | QOr (q1, q2) ->
      (
       match (rec_simplify_query q1, rec_simplify_query q2) with
	 QNone, QNone -> QNone
       | QNone, q2' -> q2'
       | q1', QNone -> q1'
       | q1', q2' -> QOr (q1',q2')
      )
  | QAndNot (q1, q2) ->
      (
       match (rec_simplify_query q1, rec_simplify_query q2) with
	 QNone, QNone -> QNone
       | QNone, q2' -> QNone
       | q1', QNone -> q1'
       | q1', q2' -> QAndNot (q1',q2')
      )
  | QHasWord _
  | QHasField _
  | QHasMinVal _
  | QHasMaxVal _
  | QNone -> q

let rec canonize_query q =
  let q1 = match q with
    
    | QAnd (q, QNone) | QAnd (QNone, q) -> q
    | QOr (q, QNone) | QOr (QNone,q) -> q
    | QAndNot (q, QNone) -> q
    
    | QAndNot ( QAndNot (q1,q2), q3 ) ->   QAndNot ( q1, QAnd(q2,q3))
    | QAndNot (q1, QAndNot(q2,q3)) ->      QAndNot (QAnd(q1,q2), q3)          
    | QAnd (q1, QAndNot (q2,q3)) ->        QAndNot (QAnd (q1,q2), q3)
    | QAnd (QAndNot (q1, q2), q3) ->       QAndNot (QAnd (q1, q3), q2)
    
    | QAnd (q1, q2) -> QAnd (canonize_query q1, canonize_query q2)
    | QOr (q1, q2) -> QOr (canonize_query q1, canonize_query q2)
    | QAndNot (q1, q2) -> QAndNot (canonize_query q1, canonize_query q2)
    | _ -> q
  in
  if q <> q1 then canonize_query q1 else q
    
let simplify_query q =
  let q = canonize_query q in
  match rec_simplify_query q with
    QNone -> QHasWord " "
  | q' -> q'

            
let string_of_tags tags =
  let buf = Buffer.create 100 in
      List.iter (fun t ->
          Buffer.add_string buf (Printf.sprintf "%-3s "
              (match t.tag_value with
                String s -> s
              | Uint64 i -> Int64.to_string i
              | Fint64 i -> Int64.to_string i
              | _ -> "???"
            ))
  ) tags;
  Buffer.contents buf

      
  
  (* first GUI have gui_num = 2, since newly created objects have _update = 1 *)
let gui_counter = ref 2

  (*
let ip_of_addr addr f = 
  if addr.addr_name <> "" then
    if addr.addr_age + !!ip_cache_timeout < last_time () then begin
        Ip.async_ip  addr.addr_name (fun ip ->
            addr.addr_ip <- ip;
            addr.addr_age <- last_time ();
            f ip)
      end else
      f addr.addr_ip
  else
    f addr.addr_ip
  
let sync_ip_of_addr addr = 
  if addr.addr_name <> "" then
    if addr.addr_age + !!ip_cache_timeout < last_time () then begin
        let ip = Ip.from_name  addr.addr_name in
        addr.addr_ip <- ip;
        addr.addr_age <- last_time ();
        ip
      end else
      addr.addr_ip
  else
    addr.addr_ip
    
let new_addr_ip ip = {
    addr_ip = ip; addr_name = Ip.to_string ip; addr_age = 0;
  }
  
let new_addr_name name = {
    addr_ip = Ip.null; addr_name = name; addr_age = 0
  }
  
let string_of_addr addr =
  if addr.addr_name = "" then Ip.to_string addr.addr_ip else addr.addr_name
    
let addr_of_string s =
  let ip = try Ip.of_string s with _ -> Ip.null in
  if ip <> Ip.null then new_addr_ip ip else new_addr_name s

let addr_is_ip addr = addr.addr_name = ""
    *)

let  upload_counter = ref Int64.zero
let  download_counter = ref Int64.zero
let nshared_files = ref 0
let shared_counter = ref Int64.zero
  
  
let rec print_tags tags =
  match tags with
    [] -> ()
  | tag :: tags ->
      lprintf "  \"%s\" = " (String.escaped tag.tag_name);
      begin
        match tag.tag_value with
        | Uint64 n -> lprintf "%s" (Int64.to_string n)
        | Fint64 n -> lprintf "%s" (Int64.to_string n)
        | Addr ip -> lprintf "%s" (Ip.to_string ip)
        | String s -> lprintf "\"%s\"" 
              (String.escaped s)
      end;
      print_tags tags

let rec fprint_tags oc tags =
  match tags with
    [] -> Printf.fprintf oc "\n";
          ()
  | tag :: tags ->
      Printf.fprintf oc "%s = " (String.escaped tag.tag_name);
      begin
        match tag.tag_value with
        | Uint64 n -> Printf.fprintf oc "%s" (Int64.to_string n)
        | Fint64 n -> Printf.fprintf oc "%s" (Int64.to_string n)
        | Addr ip -> Printf.fprintf oc "%s" (Ip.to_string ip)
        | String s -> Printf.fprintf oc "\"%s\"" 
              (String.escaped s)
      end;
      fprint_tags oc tags

let rec bprint_tags buf tags =
  match tags with
    [] -> Printf.bprintf buf "\n";
          ()
  | tag :: tags ->
      Printf.bprintf buf "%s = " (String.escaped tag.tag_name);
      begin
        match tag.tag_value with
        | Uint64 n -> Printf.bprintf buf "%s" (Int64.to_string n)
        | Fint64 n -> Printf.bprintf buf "%s" (Int64.to_string n)
        | Addr ip -> Printf.bprintf buf "%s" (Ip.to_string ip)
        | String s -> Printf.bprintf buf "\"%s\"" 
              (String.escaped s)
      end;
      bprint_tags buf tags

let aborted_download = ref (None : int option)

(* let searches = ref ([] : search list) *)
  
let core_included = ref false
let gui_included = ref false

let gui_reconnected = ref false
  
let core_gui_fifo = (Fifo.create () : GuiProto.to_gui Fifo.t)
let gui_core_fifo = (Fifo.create () : GuiProto.from_gui Fifo.t)
  
let init_hooks = ref ([] : (unit -> unit) list)
  
let add_init_hook f =
  init_hooks := f :: !init_hooks


let file_kinds = ref []

let add_web_kind kind f =
  file_kinds := (kind,f) :: !file_kinds

let mldonkey_wget url f = 
  let module H = Http_client in
  let r = {
      H.basic_request with
      H.req_url = Url.of_string url;
      H.req_user_agent = 
      Printf.sprintf "MLdonkey %s" Autoconf.current_version;
    } in
  
  H.wget r f  
  
  
let load_url kind url =
  lprintf "QUERY URL %s\n" url; 
  let f = 
    try 
      List.assoc kind !file_kinds 
    with e -> failwith (Printf.sprintf "Unknown kind [%s]" kind)
  in 
  try
    mldonkey_wget url f
  with e -> failwith (Printf.sprintf "Exception %s while loading %s"
          (Printexc2.to_string e) url)
  
let load_file kind file =
  try 
    (List.assoc kind !file_kinds) file
  with e -> 
      lprintf "Exception %s while loading kind %s\n" 
        (Printexc2.to_string e)
      kind

let chat_message_fifo = (Fifo.create () : (int * string * int * string * string) Fifo.t) 

let log_chat_message i num n s = 
  Fifo.put chat_message_fifo (last_time(),i,num,n,s);
  
  while (Fifo.length chat_message_fifo) > !!html_mods_max_messages  do
    let foo = Fifo.take chat_message_fifo in ()
  done
let last_message_log = ref 0
  
let html_mods_table_header buf n c l =
    Printf.bprintf buf "\\<div class=\\\"%s\\\"\\>\\<table id=\\\"%s\\\" name=\\\"%s\\\" class=\\\"%s\\\" cellspacing=0 cellpadding=0\\>\\<tr\\>" 
    c n n c;
    List.iter (fun (w,x,y,z)  -> 
     Printf.bprintf buf "\\<td onClick=\\\"_tabSort(this,%s);\\\" class=\\\"%s\\\" title=\\\"%s\\\"\\>%s\\</td\\>" 
     w x y z;
    ) l;
    Printf.bprintf buf "\\</tr\\>"

let html_mods_td buf l =
    List.iter (fun (t,c,d)  -> 
     Printf.bprintf buf "\\<td class=\\\"%s\\\" %s\\>%s\\</td\\>" 
     c (if t <> "" then "title=\\\"" ^ t ^ "\\\"" else "") d;
    ) l
  
let debug_clients = ref Intset.empty

let default_user = {
    ui_user_name = "admin";
    ui_user_searches = []; 
    ui_last_search = None;
    ui_last_results = [];
    ui_http_conn = None;
  }
  
let ui_users = ref [default_user]
  
      
let find_ui_user user =
  let rec iter list =
    match list with
      [] ->
        let u = {
            ui_user_name = user;
            ui_user_searches = [];
            ui_last_search = None;
            ui_last_results = [];
            ui_http_conn = None;
          } in
        ui_users := u :: !ui_users;
        u
    | u :: tail -> 
        if u.ui_user_name = user then u else iter tail
  in
  iter !ui_users
  
  
let valid_password user pass =  
  let pass = Md4.Md4.string pass in
  try
    let password = List.assoc user !!users in
    password = pass 
  with _ -> false

(* control_: means that it is the limited bandwidth, not the unlimited one
  used by the interfaces. tcp_: the full bandwidth (limited+unlimited) *)
      
let udp_upload_rate = ref 0
(* let tcp_upload_rate = ref 0 *)
let control_upload_rate = ref 0
let udp_download_rate = ref 0
(* let tcp_download_rate = ref 0 *)
let control_download_rate = ref 0

let sd_udp_upload_rate = ref 0
let sd_tcp_upload_rate = ref 0
let sd_control_upload_rate = ref 0

let bandwidth_samples = Fifo2.create ()
let short_delay_bandwidth_samples = Fifo2.create ()

let nmeasures = 6
let dummy_sample = Array.make nmeasures 0.

let trimto n samples =
  let len = ref (Fifo2.length samples) in
  while !len > n do
    ignore (Fifo2.take samples);
    decr len
  done

let last samples =
  try
    Fifo2.read samples
  with _ ->
      (last_time (), dummy_sample) 

let derive (t1, sample1) (t2, sample2) =
  let dt = t2 - t1 in
  if dt <> 0 then
    let fdt = float_of_int dt in
    (dt, Array.init nmeasures 
        (fun i -> int_of_float ((sample2.(i) -. sample1.(i)) /. fdt)))
  else
    (0, Array.make nmeasures 0) 

let update_link_stats () =
  
  let put time sample samples =
    assert (Array.length sample = nmeasures);
    Fifo2.put samples (time, sample) in
  
  let last_count_time, last_sample = 
    last bandwidth_samples in
  let time = last_time () in
  let sample = [|
      Int64.to_float !tcp_uploaded_bytes;
      Int64.to_float !tcp_downloaded_bytes;
      Int64.to_float (moved_bytes upload_control);
      Int64.to_float (moved_bytes download_control);
      Int64.to_float !udp_uploaded_bytes;
      Int64.to_float !udp_downloaded_bytes;|] in
  
  (match derive (last_count_time, last_sample) (time, sample) with
      _, [|tur; tdr; cur; cdr; uur; udr|] ->
        
(*
  tcp_upload_rate := tur;
  tcp_download_rate := tdr;
  *)
        control_upload_rate := cur;
        control_download_rate := cdr;
        udp_upload_rate := uur;
        udp_download_rate := udr
    | _ -> failwith "wrong number of measures");

(*
  lprintf "BANDWIDTH %d/%d %d/%d\n" !control_upload_rate !tcp_upload_rate
    !control_download_rate !tcp_download_rate ;
*) 
  put time sample bandwidth_samples;
  trimto 20 bandwidth_samples;
  
  let sd_last_count_time, sd_last_sample = 
    last short_delay_bandwidth_samples in
  (match derive (sd_last_count_time, sd_last_sample) (time, sample) with
      _, [|tur; _; cur; _; uur; _|] ->
        sd_tcp_upload_rate := tur;
        sd_control_upload_rate := cur;
        sd_udp_upload_rate := uur
    | _ -> failwith "wrong number of measures");
  
  put time sample short_delay_bandwidth_samples;
  trimto 5 short_delay_bandwidth_samples
  
let history_size = 6
let upload_history = Fifo2.create ()
  
let upload_usage () = 
  !udp_upload_rate + !control_upload_rate

let short_delay_upload_usage () = 
  !sd_udp_upload_rate + !sd_control_upload_rate

let update_upload_history () =
  Fifo2.put upload_history (upload_usage ());
  let len = ref (Fifo2.length upload_history) in
  while !len > history_size do
    ignore (Fifo2.take upload_history);
    decr len
  done

let detected_uplink_capacity () =
  List.fold_left maxi 0 (Fifo2.to_list upload_history)


let int_tag s i = 
  { tag_name = s; tag_value = Uint64 (Int64.of_int i) }

let int32_tag s i = 
  { tag_name = s; tag_value = Uint64 i }

let string_tag s i = 
  { tag_name = s; tag_value = String i }

(* Name,FrameHeight *)
let html_mods_styles = ref
  [| ("Green",80) ; ("Small",42); ("L.Blue",80); 
	 ("L.Purple",80); ("Monochrome",80); ("Corona",80) |]

let partial_chunk c =
  match c with
    '0' | '1' -> true
  | _ -> false
    
let waiting_connections = Fifo.create ()
  
let schedule_connections () =
  let max_wanted = mini 
      (!!max_connections_per_second + nb_sockets ()) MlUnix.max_sockets in
  try
    while nb_sockets () < max_wanted do
      let f = Fifo.take waiting_connections in
      try f () with _ -> ()
    done
  with _ -> ()
      
let add_pending_connection f =
  Fifo.put waiting_connections f
      
let parse_magnet url =
  let url = Url.of_string url in
  if url.Url.file = "magnet:" then 
    let uids = ref [] in
    let name = ref "" in
    List.iter (fun (value, arg) ->
        if String2.starts_with value "xt" then
          uids := expand_uids (uid_of_string arg :: !uids)
        else 
        if String2.starts_with value "dn" then
          name := Url.decode arg
        else 
        if arg = "" then
(* This is an error in the magnet, where a & has been kept instead of being
  url-encoded *)
          name := Printf.sprintf "%s&%s" !name value
        else
          lprintf "MAGNET: unused field %s = %s\n"
            value arg
    ) url.Url.args;
    !name, !uids
  else raise Not_found
