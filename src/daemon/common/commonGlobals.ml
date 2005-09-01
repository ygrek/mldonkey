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


(*************************************************************************)
(*                                                                       *)
(*                         short_lazy                                    *)
(*                                                                       *)
(*************************************************************************)

(* Store the result of a computation for a very short time to avoid
recomputing it too often. Each value is associated with a uniq
 '(string, int1, int2)' that should be provided by the caller.
  *)

module ShortLazy : sig
    val compute : string * int * int -> ('a -> 'b) -> 'a -> 'b
  end = struct

    type t

    let short_lazy_values = Hashtbl.create 111
    let compute name f x =
      let f = (Obj.magic f : t -> t) in
      let x = (Obj.magic x : t) in
      try
        let (f',x',v) = Hashtbl.find short_lazy_values name in

        if f' != f || x <> x' then
          (Hashtbl.remove short_lazy_values name; raise Not_found);
        Obj.magic (v : t)
      with _ ->
          let v = f x in
          Hashtbl.add short_lazy_values name (f,x,v);
          Obj.magic (v : t)

    let _ =
      add_infinite_timer 5. (fun _ -> Hashtbl.clear short_lazy_values)
  end

(*************************************************************************)
(*                                                                       *)
(*                         ............                                  *)
(*                                                                       *)
(*************************************************************************)

(* ripped from gui_misc *)

let ko = 1024.0
let mo = ko *. ko
let go = mo *. ko
let tob = go *. ko

let size_of_int64 size =
  if !!html_mods_human_readable then
    let f = Int64.to_float size in
  if f > tob then
      Printf.sprintf "%.2fT" (f /. tob)
  else
     if f > go then
      Printf.sprintf "%.2fG" (f /. go)
     else
      if f > mo then
      Printf.sprintf "%.1fM" (f /. mo)
      else
     if f > ko then
       Printf.sprintf "%.1fk" (f /. ko)
     else
       Int64.to_string size
  else
    Int64.to_string size

let networks_string = ref ""

let patches_string = ref ""

let version () =
  Printf.sprintf "MLNet %s: Multi-Network p2p client (%s)"
    Autoconf.current_version !networks_string

(* Should we try to find another port when we cannot bind to the one set
in an option, and then change the option accordingly. ?> *)
let find_other_port = ref false

let shorten str limit =
  (* TODO: we schould change all strings to utf8 when
     they come into the core instead. *)
  let name = Charset.to_utf8 (* String.escaped *) str in
  let len = String.length name in
  let max_len = maxi limit 10 in
  if len > max_len then
    let prefix = String.sub name 0 (max_len -7) in
    let suffix = String.sub name (len-4) 4 in
    Printf.sprintf "%s...%s" prefix suffix
  else name

let client_short_name c =
  shorten c !!max_client_name_len

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
              lprintf_nl "Exception %s while starting %s" server_name
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

let connection_was_tried cc =
  cc.control_last_try > 0

let print_control c =
  lprintf_nl "Connection Control: ok = %d seconds ago, state = %d, last tried = %d seconds ago, delay = %d, next in %d seconds"
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
    "Upload"
    (!!max_hard_upload_rate * 1024)

let download_control = TcpBufferedSocket.create_read_bandwidth_controler
  "Download"
    (!!max_hard_download_rate * 2048) (* TODO: changed from 1024 to 2048 because of bug *)


let _ =
  option_hook max_hard_upload_rate (fun _ ->
      TcpBufferedSocket.change_rate upload_control
        (!!max_hard_upload_rate * 1024));
  option_hook max_hard_download_rate (fun _ ->
      let rate = !!max_hard_download_rate in
      TcpBufferedSocket.change_rate download_control
        (rate * 2048)) (* TODO: changed from 1024 to 2048 because of bug *)

let udp_write_controler = UdpSocket.new_bandwidth_controler upload_control

let udp_read_controler = UdpSocket.new_bandwidth_controler download_control

let gui_server_sock = ref (None : TcpServerSocket.t option)

let pid = Unix.getpid ()

let do_at_exit f =
  Pervasives.at_exit (fun _ ->
      if Unix.getpid () = pid then
        try f () with e -> ())

let exit_properly n = Pervasives.exit n

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

let rec find_tag name tags =
  match tags with
    [] -> raise Not_found
  | { tag_name = tag_name; tag_value = v } :: _ when tag_name = name -> v
  | _ :: tail -> find_tag name tail




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

let upload_counter = ref Int64.zero
let download_counter = ref Int64.zero
let nshared_files = ref 0
let nshared_bytes = ref Int64.zero
let shared_counter = ref Int64.zero

let string_of_field t =
  match t with
  | Field_Artist -> "artist"
  | Field_Title -> "title"
  | Field_Album -> "album"
  | Field_Format -> "format"
  | Field_Type -> "type"
  | Field_Length -> "length"
  | Field_Availability -> "availability"
  | Field_Completesources -> "completesources"
  | Field_Filename -> "filename"
  | Field_Size -> "size"
  | Field_Uid -> "uid"
  | Field_Bitrate -> "bitrate"
  | Field_Codec -> "codec"
  | Field_Filerating -> "rating"
  | Field_Lastseencomplete -> "lastcompl"
  | Field_Medialength -> "mlen"
  | Field_Mediacodec -> "mediacodec"
  | Field_UNKNOWN s -> s

let field_of_string t =
  match String.lowercase t with
  | "artist" -> Field_Artist
  | "title" -> Field_Title
  | "album" -> Field_Album
  | "format" -> Field_Format
  | "type" -> Field_Type
  | "length" -> Field_Length
  | "availability" -> Field_Availability
  | "completesources" -> Field_Completesources
  | "filename" -> Field_Filename
  | "size" -> Field_Size
  | "uid" -> Field_Uid
  | "bitrate" -> Field_Bitrate
  | "codec" -> Field_Codec
  | "rating" -> Field_Filerating
  | "lastcompl" -> Field_Lastseencomplete 
  | "mlen" -> Field_Medialength
  | "mediacodec" -> Field_Mediacodec
  | _ -> Field_UNKNOWN t

let escaped_string_of_field tag =
  match tag.tag_name with
  | Field_UNKNOWN s -> String.escaped s
  | t -> string_of_field t


let string_of_tag tag =
  Printf.sprintf "  \"%s\" = %s" (escaped_string_of_field tag)
  (string_of_tag_value tag.tag_value)

let rec print_tags tags =
  match tags with
    [] -> ()
  | tag :: tags ->
      lprintf "  \"%s\" = %s" (escaped_string_of_field tag)
      (string_of_tag_value tag.tag_value);
      print_tags tags

let rec fprint_tags oc tags =
  match tags with
    [] -> Printf.fprintf oc "\n"
  | tag :: tags ->
      Printf.fprintf oc "%s = %s" (escaped_string_of_field tag)
      (string_of_tag_value tag.tag_value);
      fprint_tags oc tags

let rec bprint_tags buf tags =
  match tags with
    [] -> Printf.bprintf buf "\n"
  | tag :: tags ->
      Printf.bprintf buf "%s = %s" (escaped_string_of_field tag)
      (string_of_tag_value tag.tag_value);
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

let chat_message_fifo = (Fifo.create () : (int * string * int * string * string) Fifo.t)

let log_chat_message i num n s =
  Fifo.put chat_message_fifo (last_time(),i,num,n,s);
  try
    let oc = open_out_gen [Open_creat; Open_wronly; Open_append] 0o600 !messages_log in
    Printf.fprintf oc "%s: %s (%s): %s\n" (Date.simple (BasicSocket.date_of_int (last_time ()))) n i s;
    close_out oc;
  with e ->
    lprintf_nl "[ERROR] Exception %s while trying to log message to %s"
      (Printexc2.to_string e) !messages_log;

  while (Fifo.length chat_message_fifo) > !!html_mods_max_messages do
    let foo = Fifo.take chat_message_fifo in ()
  done
let last_message_log = ref 0

let html_mods_commands buf n c l =
    (* Name Class List *)
    Printf.bprintf buf "\\<div class=\\\"%s\\\"\\>\\<table id=\\\"%s\\\"
    name=\\\"%s\\\" class=\\\"%s\\\" cellspacing=0 cellpadding=0
    width=\\\"100pc\\\"\\>\\<tbody\\>\\<tr\\>"
    c n n c;
    List.iter (fun (w,x,y,z)  ->
     (* Class Title Onclick Value *)
     Printf.bprintf buf "\\<td class=\\\"%s\\\"
     title=\\\"%s\\\" onMouseOver=\\\"mOvr(this,'mOvr1');\\\" onMouseOut=\\\"mOut(this);\\\"
     onClick=\\\"%s\\\" \\>%s\\</td\\>"
     w x y z;
    ) l;
    Printf.bprintf buf "\\</tr\\>\\</tbody\\>\\</table\\>\\</div\\>"

(*
  html_mods_commands buf "commandsTable" "commands" [
    ( "bu bbig", "Extend search to more servers and view results", "mSub('output','vr');", "Extend search" ) ;
*)


let html_mods_table_header buf n c l =
    (* Name Class List *)
    Printf.bprintf buf "\\<div class=\\\"%s\\\"\\>\\<table id=\\\"%s\\\" name=\\\"%s\\\" class=\\\"%s\\\" cellspacing=0 cellpadding=0\\>"
    c n n c;
    if List.length l > 0 then begin
        Printf.bprintf buf "\\<tr\\>";
        List.iter (fun (w,x,y,z)  ->
         (* Sort Class Title Value *)
         Printf.bprintf buf "\\<td onClick=\\\"_tabSort(this,%s);\\\" class=\\\"%s\\\" title=\\\"%s\\\"\\>%s\\</td\\>"
         w x y z;
        ) l;
        Printf.bprintf buf "\\</tr\\>"
      end
(* Add colspan functionality to html_mods_table_header *)

let html_mods_table_header_colspan buf n c l =
    (* Name Class List *)
    Printf.bprintf buf "\\<div class=\\\"%s\\\"\\>\\<table id=\\\"%s\\\" name=\\\"%s\\\" class=\\\"%s\\\" cellspacing=0 cellpadding=0\\>\\<tr\\>"
    c n n c;
    List.iter (fun (v,w,x,y,z)  ->
     (* Sort Class Title Value *)
     Printf.bprintf buf "\\<td colspan=%s onClick=\\\"_tabSort(this,%s);\\\" class=\\\"%s\\\" title=\\\"%s\\\"\\>%s\\</td\\>"
     v w x y z;
    ) l;
    Printf.bprintf buf "\\</tr\\>"

let html_mods_table_no_header buf n c l =
    (* 1 row * n cols *)
    (* Name Class List *)
    Printf.bprintf buf "\\<div class=\\\"%s\\\"\\>\\<table id=\\\"%s\\\" name=\\\"%s\\\" class=\\\"%s\\\" cellspacing=0 cellpadding=0\\>\\<tr\\>"
    c n n c;
    List.iter (fun (t,c,d)  ->
    (* Title Class Value *)
     Printf.bprintf buf "\\<td class=\\\"%s\\\" %s\\>%s\\</td\\>"
     c (if t <> "" then "title=\\\"" ^ t ^ "\\\"" else "") d;
    ) l;
    Printf.bprintf buf "\\</tr\\>\\</table\\>\\</div\\>"

let html_mods_table_one_row buf n c l =
    (* 1 row * n cols *)
    (* Name Class List *)
    Printf.bprintf buf "\\<div class=\\\"%s\\\"\\>\\<table id=\\\"%s\\\" name=\\\"%s\\\" class=\\\"%s\\\" cellspacing=0 cellpadding=0\\>\\<tr\\>"
    c n n c;
    List.iter (fun (t,c,d)  ->
    (* Title Class Value *)
     Printf.bprintf buf "\\<td class=\\\"%s\\\" %s\\>%s\\</td\\>"
     c (if t <> "" then "title=\\\"" ^ t ^ "\\\"" else "") d;
    ) l;
    Printf.bprintf buf "\\</tr\\>\\</table\\>\\</div\\>"

let html_mods_table_one_col buf n c l =
    (* n rows * 1 col *)
    (* Name Class List *)
    Printf.bprintf buf "\\<div class=\\\"%s\\\"\\>\\<table id=\\\"%s\\\" name=\\\"%s\\\" class=\\\"%s\\\" cellspacing=0 cellpadding=0\\>\\<tr\\>"
    c n n c;
    List.iter (fun (t,c,d)  ->
    (* Title Class Value *)
     Printf.bprintf buf "\\<tr\\>\\<td class=\\\"%s\\\" %s\\>%s\\</td\\>\\</tr\\>"
     c (if t <> "" then "title=\\\"" ^ t ^ "\\\"" else "") d;
    ) l;
    Printf.bprintf buf "\\</tr\\>\\</table\\>\\</div\\>"

let html_mods_td buf l =
    (* List *)
    List.iter (fun (t,c,d)  ->
    (* Title Class Value *)
     Printf.bprintf buf "\\<td class=\\\"%s\\\" %s\\>%s\\</td\\>"
     c (if t <> "" then "title=\\\"" ^ t ^ "\\\"" else "") d;
    ) l

let html_mods_counter = ref true

let html_mods_cntr () =
    html_mods_counter := not !html_mods_counter;
    if !html_mods_counter then 1 else 2

let html_mods_cntr_init () =
  html_mods_counter := true


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

let history_size = 720
let history_h_size = 720

let upload_history = Fifo2.create ()
let download_history = Fifo2.create ()
let upload_h_history = Fifo2.create ()
let download_h_history = Fifo2.create ()

let upload_usage () =
  !udp_upload_rate + !control_upload_rate

let short_delay_upload_usage () =
  !sd_udp_upload_rate + !sd_control_upload_rate

let download_usage () =
  !udp_download_rate + !control_download_rate

let update_download_history () =
  Fifo2.put download_history (download_usage ());
  let len = ref (Fifo2.length download_history) in
  while !len > history_size do
    ignore (Fifo2.take download_history);
    decr len
  done

let update_upload_history () =
  Fifo2.put upload_history (upload_usage ());
  let len = ref (Fifo2.length upload_history) in
  while !len > history_size do
    ignore (Fifo2.take upload_history);
    decr len
  done

let update_h_download_history () =
  Fifo2.put download_h_history ((List.fold_left (+) 0 (Fifo2.to_list download_history)) / ((Fifo2.length download_history)));
  let len = ref (Fifo2.length download_h_history) in
  while !len > history_h_size do
    ignore (Fifo2.take download_h_history);
    decr len
  done

let update_h_upload_history () =
  Fifo2.put upload_h_history ((List.fold_left (+) 0 (Fifo2.to_list upload_history)) / ((Fifo2.length upload_history)));
  let len = ref (Fifo2.length upload_h_history) in
  while !len > history_h_size do
    ignore (Fifo2.take upload_h_history);
    decr len
  done

let detected_link_capacity link =
  List.fold_left maxi 0 (Fifo2.to_list link)

let detected_uplink_capacity () =
  List.fold_left maxi 0 (Fifo2.to_list upload_history)

let detected_downlink_capacity () =
  List.fold_left maxi 0 (Fifo2.to_list download_history)


let new_tag name v =
  { tag_name = name; tag_value = v }

let int_tag s i =
  { tag_name = s; tag_value = Uint64 (Int64.of_int i) }

let int64_tag s i =
  { tag_name = s; tag_value = Uint64 i }

let string_tag s i =
  { tag_name = s; tag_value = String i }

let for_int_tag tag f =
  match tag.tag_value with
    Uint64 i | Fint64 i -> f (Int64.to_int i)
  | String _ -> ()
  | Addr _ -> ()
  | Pair _ -> ()
  | Uint16 n | Uint8 n -> f n

let for_int64_tag tag f =
  match tag.tag_value with
    Uint64 i | Fint64 i -> f i
  | String _ -> ()
  | Addr _ -> ()
  | Pair _ -> ()
  | Uint8 n | Uint16 n -> f (Int64.of_int n)

let for_two_int16_tag tag f =
  match tag.tag_value with
    Uint64 i | Fint64 i ->
      let i1 = Int64.to_int (Int64ops.right64 i 16) in
      let i0 = Int64.to_int i in
      let i0 = i0 land 0xffff in
      let i1 = i1 land 0xffff in
      f i0 i1
  | String _ -> ()
  | Addr _ -> ()
  | Pair _ -> ()
  | Uint8 n | Uint16 n -> f n 0

let for_string_tag tag f =
  match tag.tag_value with
    Uint64 _ | Fint64 _ -> ()
  | String s -> f s
  | Addr _ -> ()
  | Pair _ -> ()
  | Uint16 _ | Uint8 _ -> ()

let partial_chunk c =
  match c with
    '0' | '1' -> true
  | _ -> false

module Connections = struct

  end


let parse_magnet url =
  let url = Url.of_string url in
  if url.Url.short_file = "magnet:" then
    let uids = ref [] in
    let name = ref "" in
    List.iter (fun (value, arg) ->
        if String2.starts_with value "xt" then
          uids := Uid.expand (Uid.of_string arg :: !uids)
        else
        if String2.starts_with value "dn" then
          name := Url.decode arg
        else
        if arg = "" then
(* This is an error in the magnet, where a & has been kept instead of being
  url-encoded *)
          name := Printf.sprintf "%s&%s" !name value
        else
          lprintf_nl "MAGNET: unused field %s = %s"
            value arg
    ) url.Url.args;
    !name, !uids
  else raise Not_found

    (*
module CanBeCompressed = struct

    let to_deflate = ref []
    let to_deflate_len = ref 0

    let compression_buffer_len = 20000
    let compression_buffer = String.create compression_buffer_len

    let deflate_connection sock =
      lprintf "Creating deflate connection\n";
      let comp = Deflate (Zlib.inflate_init true, Zlib.deflate_init 6 true) in
      CompressedConnection (comp,
        buf_create !max_buffer_size, buf_create !max_buffer_size, sock)

    let rec iter_deflate sock zs wbuf =
      if wbuf.len > 0 then begin
          lprintf "iter_deflate\n";
          let (_, used_in, used_out) = Zlib.deflate zs
              wbuf.buf wbuf.pos wbuf.len
              compression_buffer 0 compression_buffer_len
              Zlib.Z_SYNC_FLUSH in
          lprintf "deflated %d/%d -> %d\n" used_in wbuf.len used_out;
          lprintf "[%s]\n" (String.escaped (String.sub compression_buffer 0 used_out));
          write sock compression_buffer 0 used_out;
          buf_used wbuf used_in;
          if used_in > 0 || used_out > 0 then
            iter_deflate sock zs wbuf
        end

    let deflate_timer _ =
      List.iter (fun conn ->
          try
            match conn with
              CompressedConnection (comp, _, wbuf, sock) ->
                if closed sock then raise Exit;
                let Deflate (_, zs) = comp in
                iter_deflate sock zs wbuf
            | _ -> ()
          with e ->
              lprintf "[ERROR] Exception %s in CanBeCompressed.deflate_timer\n"
                (Printexc2.to_string e)
      ) !to_deflate;
      to_deflate := [];
      to_deflate_len := 0

    let to_deflate conn =
      if not (List.memq conn !to_deflate) then
        to_deflate := conn :: !to_deflate;
      if !to_deflate_len > 1000000 then
        deflate_timer ()

    let write_string conn s =
      lprintf "write_string\n";
      let len = String.length s in
      match conn with
        Connection sock -> write_string sock s
      | CompressedConnection (_,_,wbuf,sock) ->
          lprintf "CanBeCompressed.write_string %d\n" len;
          to_deflate_len := !to_deflate_len + len;
          to_deflate conn;
          buf_add sock wbuf s 0 len
      | _ -> assert false

    let rec iter_inflate zs sock b rbuf =
      if b.len > 0 then begin
          lprintf "iter_inflate %d\n" b.len;
          lprintf "[%s]\n" (String.escaped (String.sub b.buf b.pos b.len));
          let (_, used_in, used_out) = Zlib.inflate zs b.buf b.pos b.len
              compression_buffer 0 compression_buffer_len
              Zlib.Z_SYNC_FLUSH in
          lprintf "inflated %d/%d -> %d\n" used_in b.len used_out;
          lprintf "[%s]\n" (String.escaped (String.sub compression_buffer 0 used_out));
          buf_add sock rbuf compression_buffer 0 used_out;
          buf_used b used_in;
          if used_in > 0 || used_out > 0 then
            iter_inflate zs sock b rbuf
        end

    let buf conn =
      lprintf "CanBeCompressed.buf\n";
      try
        match conn with
          Connection sock -> buf sock
        | CompressedConnection (comp,rbuf,_,sock) ->

            let b = buf sock in
            let Deflate (zs, _) = comp in
            if b.len > 0 then iter_inflate zs sock b rbuf;
            rbuf
        | _ -> assert false
      with e ->
          lprintf "[ERROR] Exception %s in CanBeCompressed.buf\n"
            (Printexc2.to_string e);
          raise e
  end
    *)

let ip_reachable ip =
  !!allow_local_network || Ip.reachable ip

let do_if_connected tcp_connection f =
  match tcp_connection with
    Connection sock -> f sock
  | _ -> ()

let new_activity () = {
    activity_begin = BasicSocket.last_time ();
    activity_client_overnet_connections = 0;
    activity_client_overnet_indirect_connections = 0;
    activity_client_overnet_successful_connections = 0;
    activity_client_edonkey_connections = 0;
    activity_client_edonkey_indirect_connections = 0;
    activity_client_edonkey_successful_connections = 0;
    activity_server_edonkey_connections = 0;
    activity_server_edonkey_successful_connections = 0;
    }

let nactivities = ref 0
let activities = Fifo.create ()
let activity = ref (new_activity ())

let _ =
  add_infinite_timer 60. (fun _ ->
      Fifo.put activities !activity;
      incr nactivities;
      if !nactivities > 2000 then begin
          ignore (Fifo.take activities);
          decr nactivities
        end;
      activity := new_activity ()
  )

