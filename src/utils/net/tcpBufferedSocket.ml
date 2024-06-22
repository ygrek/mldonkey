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

open Int64ops
open Printf2
open BasicSocket


let latencies = Hashtbl.create 2131

let max_opened_connections = ref (fun () -> 20)
let max_connections_per_second = ref (fun () -> 50)

let opened_connections = ref 0
let opened_connections_this_second = ref 0

let max_buffer_size = ref 50000

let bind_address = ref Unix.inet_addr_any
let ip_packet_size = ref 40
let mtu_packet_size = ref 1500
let minimal_packet_size = ref 600
let packet_frame_size = 1

let proc_net_fs = ref true

let tcp_uploaded_bytes = ref Int64.zero
let tcp_downloaded_bytes = ref Int64.zero

let exn_exit = Exit

(*************************************************************************)
(*                                                                       *)
(*                         TYPES                                         *)
(*                                                                       *)
(*************************************************************************)

type event =
  WRITE_DONE
| CAN_REFILL
| CONNECTED
| BUFFER_OVERFLOW
| READ_DONE of int
| BASIC_EVENT of BasicSocket.event

let string_of_event = function
| CONNECTED -> "CONNECTED"
| WRITE_DONE -> "WRITE_DONE"
| CAN_REFILL -> "CAN_REFILL"
| BUFFER_OVERFLOW -> "BUFFER_OVERFLOW"
| READ_DONE n -> Printf.sprintf "READ_DONE %d" n
| BASIC_EVENT e -> string_of_basic_event e

type token = {
    mutable token_used : bool;
    connection_manager : connection_manager;
  }

and connection_manager = {
    cm_name : string;
    mutable nestablished_connections : int;
    mutable nwaiting_connections : int;
    mutable nconnections_last_second : int;
    waiting_connections : (token * (token -> unit)) Fifo.t;
  }

type buf = {
    mutable buf : bytes;
    mutable pos : int;
    mutable len : int;
    mutable max_buf_size : int;
    mutable min_buf_size : int
  }

type t = {
    name : string;
    mutable closing : bool;
    mutable sock_in : BasicSocket.t;
    mutable sock_out : BasicSocket.t;
    mutable rbuf : buf;
    mutable wbuf : buf;
    mutable event_handler : handler;
    mutable error : close_reason;
    mutable nread : int;
    mutable ndown_packets : int;
    mutable nwrite : int;
    mutable nup_packets : int;
    mutable monitored : bool;

    mutable read_control : bandwidth_controler option;
    mutable write_control : bandwidth_controler option;
    mutable write_power : int;
    mutable read_power : int;

    mutable peer_addr : (Ip.t * int) option;
    mutable my_ip : Ip.t;

    mutable noproxy : bool;
    mutable connecting : bool;
    mutable host : Ip.t;
    mutable connect_time : float;

    mutable token : token;

    mutable compression : (
      Zlib.stream *
      Zlib.stream *
      buf * (* read buffer after decompression *)
      buf (* write buffer before decompression *)
    ) option;

  }

and handler = t -> event -> unit

and bandwidth_controler = {
    bc_name : string;
    mutable remaining_bytes : int;
    mutable total_bytes : int;
    mutable nconnections : int;
    mutable connections : t list;
    allow_io : bool ref;
    mutable remaining_bytes_user : ((* total *) int -> (* remaining *) int -> unit);
    mutable moved_bytes : int64;
    mutable lost_bytes : int array; (* 3600 samples*)
    mutable forecast_bytes : int;

    mutable ndone_last_second : int;
  }


(*************************************************************************)
(*                                                                       *)
(*                         Connections Managers                          *)
(*                                                                       *)
(*************************************************************************)

let connection_managers = ref []

let create_connection_manager name =
  let manager = {
      cm_name = name;
      nestablished_connections = 0;
      nwaiting_connections = 0;
      waiting_connections = Fifo.create ();
      nconnections_last_second = 0;
  }
  in
  connection_managers := manager :: !connection_managers;
  manager

let create_token manager = {
    token_used = false;
    connection_manager = manager;
  }

let add_pending_connection manager f =
  let token = create_token manager in
  Fifo.put manager.waiting_connections (token, f);
  manager.nwaiting_connections <- manager.nwaiting_connections + 1;
  token

let can_open_connection manager =
  manager.nestablished_connections +
    manager.nwaiting_connections < !max_opened_connections ()


(******
  This scheduler does not use the already established connections.
*****)

let schedule_connections () =
  let max_wanted = !max_opened_connections () in
  let max_connections_per_second = !max_connections_per_second () in

  let rec iter todo_managers done_managers =

(*
    lprintf "todo_managers %d done_managers %d\n"
(List.length todo_managers) (List.length done_managers);
  *)
    match todo_managers with
      [] -> begin
          match done_managers with
            [] -> ()
          | _ -> iter done_managers []
        end
    | manager :: tail ->
(*
        lprintf "!opened_connections_this_second %d < max_connections_per_second %d\n" !opened_connections_this_second max_connections_per_second;
        lprintf "&& !opened_connections %d < max_wanted %d\n"
          !opened_connections max_wanted;
*)
        if !opened_connections_this_second < max_connections_per_second
            && !opened_connections < max_wanted then
          if
            (try
                let (token,f) =
                  Fifo.take manager.waiting_connections in
                manager.nwaiting_connections <-
                  manager.nwaiting_connections - 1;
                if not token.token_used then begin
                    f token;
                  end;
(* prevent in any case the token from being used later *)
                token.token_used <- true;
                true
              with _ -> false)
          then
            iter tail (manager :: done_managers)
          else
            iter tail done_managers
  in
  iter !connection_managers []

let cancel_token token =
  token.token_used <- true

let used_token token = token.token_used

let unlimited_connection_manager = create_connection_manager "Unlimited"

let reset_connection_scheduler _ =
  if !verbose_bandwidth > 0 then
    lprintf_nl "[BW1 %6d] Connections opened this second : %d/%d total %d/%d"
      (last_time ())
      !opened_connections_this_second
      (!max_connections_per_second ())
      !opened_connections
      (!max_opened_connections ());

  List.iter (fun cm ->
      if !verbose_bandwidth > 0 then begin
          if cm.nconnections_last_second > 0 then
            lprintf_nl "[BW1 %6d]    %s opened %d connections last second"
              (last_time ()) cm.cm_name cm.nconnections_last_second;
          if cm.nwaiting_connections > 0 then
            lprintf_nl "[BW1 %6d]    %s still waits for %d connections"
              (last_time ()) cm.cm_name cm.nwaiting_connections;
       end;
      cm.nconnections_last_second <- 0;
  ) !connection_managers;

  opened_connections_this_second := 0

let use_token token fd =
  if token.token_used then begin
      (try Unix.close fd with _ -> ());
      failwith "Token already used";
    end;
  token.token_used <- true;
  token.connection_manager.nestablished_connections <-
    token.connection_manager.nestablished_connections + 1;
  incr opened_connections;
  incr opened_connections_this_second

(*************************************************************************)
(*                                                                       *)
(*                         Bandwidth Consumers                           *)
(*                                                                       *)
(*************************************************************************)

let add_connect_latency ip time =
  if ip <> Ip.null && time > 1. then
    let delay = current_time () -. time in
    let delayf = 1000. *. delay in
    let delay = int_of_float delayf in
    let delay = if delay > 65000 then 65000 else delay in
(*
lprintf "add_connect_latency %s -> %d (%f)\n"
(Ip.to_string ip) delay delayf;
  *)
    try
      let latency, samples = Hashtbl.find latencies ip in
      incr samples;
      if !latency > delay then latency := delay
    with _ ->
        Hashtbl.add latencies ip (ref delay, ref 1)

let forecast_bytes t nbytes =
  match t with
    None -> ()
  | Some bc ->
      let nip_packets = 1 + nbytes / !mtu_packet_size in
      let nbytes = nbytes + nip_packets * !ip_packet_size in
      let nframes = 1 + nbytes / packet_frame_size in
      bc.forecast_bytes <- bc.forecast_bytes +
        (nframes * packet_frame_size)

let register_bytes t nbytes =
  match t with
    None -> ()
  | Some bc ->
      let nip_packets = 1 + nbytes / !mtu_packet_size in
      let nbytes = nbytes + nip_packets * !ip_packet_size in
      let nframes = 1 + nbytes / packet_frame_size in
      bc.remaining_bytes <- bc.remaining_bytes -
      (nframes * packet_frame_size)

let forecast_download t n =
  forecast_bytes t.read_control n

let forecast_upload t n =
  forecast_bytes t.write_control n

let register_download t n =
  register_bytes t.read_control n

let register_upload t n =
  register_bytes t.write_control n

let accept_connection_bandwidth t =
  register_download t 0;
  forecast_upload t 0;
  forecast_download t 0

let best_packet_size nbytes =
  let nbytes = max nbytes !minimal_packet_size in
  let nip_packets = 1 + nbytes / !mtu_packet_size in
  let headers = nip_packets * !ip_packet_size in
  let nframes = 1 + (nbytes + headers) / packet_frame_size in
  nframes * packet_frame_size - headers

(*************************************************************************)
(*                                                                       *)
(*                         Buffers management                            *)
(*                                                                       *)
(*************************************************************************)

let copy_read_buffer = ref true

let big_buffer_len = 65536
let big_buffer = String.create big_buffer_len

let min_buffer_read = 2000
let min_read_size = min_buffer_read - 100

let old_strings_size = 20
let old_strings = Array.make old_strings_size Bytes.empty
let old_strings_len = ref 0

let new_string () =
  if !old_strings_len > 0 then begin
      decr old_strings_len;
      let s = old_strings.(!old_strings_len) in
      old_strings.(!old_strings_len) <- Bytes.empty;
      s
    end else
    Bytes.create min_buffer_read

let delete_string s =
  if !old_strings_len < old_strings_size &&
    Bytes.length s = min_buffer_read then begin
      old_strings.(!old_strings_len) <- s;
      incr old_strings_len;
    end

let buf_create max =
  {
    buf = Bytes.empty;
    pos = 0;
    len = 0;
    max_buf_size = max;
    min_buf_size = min_read_size;
  }


let buf_used b nused =
  if nused = b.len then
    ( b.len <- 0;
      b.pos <- 0;
      delete_string b.buf;
      b.buf <- Bytes.empty;
      )
  else
    (b.len <- b.len - nused; b.pos <- b.pos + nused)

let buf_size  t =
  (Bytes.length t.rbuf.buf),
  (Bytes.length t.wbuf.buf)

(*************************************************************************)
(*                                                                       *)
(*                         buf_add                                       *)
(*                                                                       *)
(*************************************************************************)

let buf_add t b s pos1 len =
  let curpos = b.pos + b.len in
  let max_len =
    if Bytes.length b.buf = 0 then
      begin
        b.buf <- new_string ();
        min_buffer_read
      end else
      Bytes.length b.buf in
  if max_len - curpos < len then (* resize before blit *)
    if b.len + len < max_len then (* just move to 0 *)
      begin
        Bytes.blit b.buf b.pos b.buf 0 b.len;
        Bytes.blit s pos1 b.buf b.len len;
        b.len <- b.len + len;
        b.pos <- 0;
      end
    else
    if b.len + len > b.max_buf_size then begin
        lprintf "[TCP_BS]: BUFFER OVERFLOW %d+%d> %d " b.len len b.max_buf_size ;

        lprintf "MESSAGE: [";
        for i = pos1 to pos1 + (min len 20) - 1 do
          lprintf "(%d)" (int_of_char (Bytes.get s i));
        done;
        if len > 20 then lprintf "...";
        lprintf "]\n";

        t.event_handler t BUFFER_OVERFLOW;
(* TODO: why do we have this ??? in case of BUFFER_OVERFLOW, just close the
  socket !!! *)
      end
    else
    let new_len = min (max (2 * max_len) (b.len + len)) b.max_buf_size  in
(*    if t.monitored then
      (lprintf "Allocate new for %d\n" len; ); *)
    let new_buf = String.create new_len in
    Bytes.blit b.buf b.pos new_buf 0 b.len;
    Bytes.blit s pos1 new_buf b.len len;
    b.len <- b.len + len;
    b.pos <- 0;
    if max_len = min_buffer_read then delete_string b.buf;
(*    if t.monitored then
      (lprintf "new buffer allocated\n"; ); *)
    b.buf <- new_buf
  else begin
      Bytes.blit s pos1 b.buf curpos len;
      b.len <- b.len + len
    end

(*************************************************************************)
(*                                                                       *)
(*                         Sockets management                            *)
(*                                                                       *)
(*************************************************************************)

let buf t = t.rbuf
let setsock_iptos_throughput t =
  ignore (setsock_iptos_throughput (fd (t.sock_in)));
  ignore (setsock_iptos_throughput (fd (t.sock_out)))

let closed t = closed t.sock_out
let error t = t.error
let sock_used t nused = buf_used t.rbuf nused
let remaining_to_write t =  t.wbuf.len
let nread t = t.nread
let nwritten t = t.nwrite
let can_write t =  t.wbuf.len = 0
let can_write_len t len =
  let b = t.wbuf.max_buf_size > t.wbuf.len + len in
(*  if not b then
    lprintf "can_write_len failed: %d < %d + %d\n"
      t.wbuf.max_buf_size t.wbuf.len len; *)
  b
let not_buffer_more t max =  t.wbuf.len < max
let get_rtimeout t = get_rtimeout t.sock_in
let max_refill t = t.wbuf.max_buf_size - t.wbuf.len

let close t s =
(*
  if t.monitored then begin
      lprintf "close with %s %s\n" t.error s;
end;
*)
  if not t.closing then
    begin
      try
        t.token.connection_manager.nestablished_connections <-
          t.token.connection_manager.nestablished_connections - 1;
        decr opened_connections;
        t.closing <- true;
        delete_string t.rbuf.buf;
        delete_string t.wbuf.buf;
        t.rbuf.buf <- Bytes.empty;
        t.wbuf.buf <- Bytes.empty;
        if t.nread > 0 then begin
            register_upload t 0;
            forecast_download t 0;
          end;
        close t.sock_in s;
        if t.sock_in != t.sock_out then
          close t.sock_out s
        (* (Printf.sprintf "%s after %d/%d" s t.nread t.nwrite) *)
      with e ->
          lprintf "Exception %s in TcpBufferedSocket.close\n"
            (Printexc2.to_string e);
          raise e
    end

let shutdown t s =
  (*
  if t.monitored then begin
      lprintf "shutdown\n";
end;
  *)
  (try
      BasicSocket.shutdown t.sock_out s;
      if t.sock_in != t.sock_out then
        BasicSocket.shutdown t.sock_in s;
      with e ->
       lprintf "exception %s in shutdown\n" (Printexc2.to_string e);
        );
  (try close t s with  e ->
        lprintf "exception %s in shutdown\n" (Printexc2.to_string e);
  )

(*************************************************************************)
(*                                                                       *)
(*                         write                                         *)
(*                                                                       *)
(*************************************************************************)

let write t s pos1 len =
(*  lprintf "want_write %d\n" len; *)
  if len > 0 && not (closed t) then
    let pos2 = pos1 + len in
    let b = t.wbuf in
    let pos1 =
      if b.len = 0 && not t.connecting && (match t.write_control with
            None ->
(*              lprintf "NO CONTROL\n"; *)
              true
          | Some bc ->
(*              lprintf "LIMIT %d\n" bc.total_bytes; *)
              bc.total_bytes = 0)
      then
        try
          let fd = fd t.sock_out in
(*       lprintf "WRITE [%s]\n" (String.escaped (String.sub s pos1 len)); *)
          let nw = MlUnix.write fd s pos1 len in
          if !verbose_bandwidth > 1 then
            lprintf_nl "[BW2 %6d] immediate write %d/%d on %s:%d"
              (last_time ()) nw len t.name (sock_num t.sock_out);

          register_upload t len;
          forecast_download t 0;

(*          if t.monitored then begin
              lprintf "write: direct written %d\n" nw;
end; *)
          tcp_uploaded_bytes := !tcp_uploaded_bytes ++ (Int64.of_int nw);
          (match t.write_control with
              None -> ()
            | Some bc ->
                bc.moved_bytes <- bc.moved_bytes ++ (Int64.of_int nw));
          if t.nwrite = 0 then begin
(*              if t.connecting then
                lprintf "WRITE BEFORE CONNECTION.......\n";
              lprintf "add_connect_latency at %f\n" (current_time ()); *)
              add_connect_latency t.host t.connect_time;
            end;

          t.nwrite <- t.nwrite + nw;
          if nw = 0 then (close t Closed_by_peer; pos2) else
            pos1 + nw
        with
          Unix.Unix_error ((Unix.EWOULDBLOCK | Unix.EAGAIN | Unix.ENOTCONN), _, _) -> pos1
        | e ->
            t.error <- Closed_for_error (Printf.sprintf "Write Error: %s" (Printexc2.to_string e));
            close t t.error;

(*      lprintf "exce %s in read\n" (Printexc2.to_string e);  *)
            raise e

      else pos1
    in
    if pos2 > pos1 then
      let sock = t.sock_out in
      must_write sock true;
      buf_add t b s pos1 (pos2 - pos1)

(*************************************************************************)
(*                                                                       *)
(*                         can_read_handler                              *)
(*                                                                       *)
(*************************************************************************)

let dummy_sock = Obj.magic 0


(* max_len is the maximal length we authorized to read, min_read_size
is the minimal size we authorize to read *)

let can_read_handler t sock max_len =
(*  let max_len = 100000 in (* REMOVE THIS: don't care about bw *) *)
  let b = t.rbuf in
  let curpos = b.pos + b.len in
(*  lprintf "curpos %d/%d\n" curpos b.len; *)
  let buffer, buffer_pos, buffer_len =
    if !copy_read_buffer then
      big_buffer, 0, big_buffer_len
    else
    let can_write_in_buffer =
      if Bytes.length b.buf = 0 then
        if b.min_buf_size <= min_buffer_read then begin
            b.buf <- new_string ();
            min_buffer_read
          end else begin
            b.buf <- String.create b.min_buf_size;
            b.min_buf_size
          end
      else
      let buf_len = Bytes.length b.buf in
      if buf_len - curpos < min_read_size then
        if b.len + min_read_size > b.max_buf_size then
          (
            t.event_handler t BUFFER_OVERFLOW;
            lprintf "[OVERFLOW] in %s" (info sock);
            close t Closed_for_overflow;
            raise exn_exit
          )
        else
        if b.len + min_read_size < buf_len then
          (
            Bytes.blit b.buf b.pos b.buf 0 b.len;
            b.pos <- 0;
            buf_len - b.len
          )
        else
        let new_len = min
            (max
              (2 * buf_len) (b.len + min_read_size)) b.max_buf_size
        in
        let new_buf = String.create new_len in
        Bytes.blit b.buf b.pos new_buf 0 b.len;
        b.pos <- 0;
        b.buf <- new_buf;
        new_len - b.len
      else
        buf_len - curpos
    in
    b.buf, b.pos+b.len, can_write_in_buffer
  in
  let can_read = min max_len buffer_len in
  if can_read > 0 then
    let old_len = b.len in
    let nread = try
(*        lprintf "{can read %d} --> " can_read; *)
(*        lprintf "Unix.read %d/%d/%d\n"  (String.length b.buf) (b.pos + b.len) can_read;  *)
        Unix.read (fd sock) buffer buffer_pos can_read;

        
      with
        Unix.Unix_error((Unix.EWOULDBLOCK | Unix.EAGAIN), _,_) as e -> raise e
      | e ->
          t.error <- Closed_for_error (Printf.sprintf "Can Read Error: %s" (Printexc2.to_string e));
          close t t.error;

(*      lprintf "exce %s in read\n" (Printexc2.to_string e); *)
          raise e

    in

    (*
    if nread = can_read then begin
        lprintf "READ LIMITED BY BW CONTROL: %d\n" nread;
      end;
*)

    if !verbose_bandwidth > 1 then
      lprintf_nl "[BW2 %6d] %sread %d/%d/%d on %s:%d" (last_time ())
        (if old_len > 0 then "completing " else "") nread can_read max_len
          t.name (sock_num t.sock_in);


    if !copy_read_buffer then
        buf_add t b big_buffer 0 nread
    else
      b.len <- b.len + nread;
(*    lprintf " %d\n" nread; *)
    b.min_buf_size <- min b.max_buf_size (
      max (nread + nread / 2) min_read_size);

    (*
    if nread = can_read then begin
        lprintf "Unix.read: read limited: %d\n" nread;
        lprintf "    given to handler: %d\n" max_len;
        lprintf "    given by buffer: %d\n" buffer_len;
    end;
*)

    tcp_downloaded_bytes := !tcp_downloaded_bytes ++ (Int64.of_int nread);
    (match t.read_control with
        None -> () | Some bc ->
          bc.moved_bytes <- bc.moved_bytes ++ (Int64.of_int nread));

    t.nread <- t.nread + nread;
    if nread > 0 then begin
        register_download t nread;
        register_upload t 0;
      end;
    if nread = 0 then begin
      close t Closed_by_peer;
    end else begin

      try
(*              if t.monitored then
   (lprintf "event handler READ DONE\n"; ); *)
        t.event_handler t (READ_DONE nread);
      with
      | e ->
(*                if t.monitored then
   (lprintf "Exception in READ DONE\n"; ); *)
          t.error <- Closed_for_error (Printf.sprintf "READ_DONE Error: %s" (Printexc2.to_string e));
          close t t.error;

(*      lprintf "exce %s in read\n" (Printexc2.to_string e);  *)
          raise e
    end

(*************************************************************************)
(*                                                                       *)
(*                         can_write_handler                             *)
(*                                                                       *)
(*************************************************************************)

let can_write_handler t sock max_len =
(*      if t.monitored then (
          lprintf "CAN_WRITE (%d)\n" t.wbuf.len;
        ); *)
  let b = t.wbuf in
  if not t.connecting then (
      if b.len > 0 then
        begin
          try
(*     lprintf "try write %d/%d\n" max_len t.wbuf.len; *)
            let fd = fd sock in
(*            lprintf "WRITE [%s]\n" (String.escaped
              (String.sub b.buf b.pos max_len)); *)
            let nw = MlUnix.write fd b.buf b.pos max_len in
            if !verbose_bandwidth > 1 then
              lprintf_nl "[BW2 %6d] postponed %swrite %d/%d/%d on %s:%d"
                  (last_time ()) (if max_len < b.len then "partial " else "")
                  nw max_len b.len t.name (sock_num t.sock_out);

(*            if t.monitored then
(lprintf "written %d\n" nw; ); *)
        tcp_uploaded_bytes := !tcp_uploaded_bytes ++ (Int64.of_int nw);
        (match t.write_control with
            None -> ()
          | Some bc ->
              bc.moved_bytes <- bc.moved_bytes ++ (Int64.of_int nw));
            if t.nwrite = 0 then begin
(*                lprintf "add_connect_latency at %f\n" (current_time ()); *)
                add_connect_latency t.host t.connect_time;
              end;
        t.nwrite <- t.nwrite + nw;
        b.len <- b.len - nw;
        b.pos <- b.pos + nw;
        if nw = 0 then close t Closed_by_peer else
        if b.len = 0 then begin
            b.pos <- 0;
            delete_string b.buf;
            b.buf <- Bytes.empty;
          end
      with
        Unix.Unix_error((Unix.EWOULDBLOCK | Unix.EAGAIN ), _,_) as e -> raise e
      | e ->
          t.error <- Closed_for_error (Printf.sprintf "Can Write Error: %s" (Printexc2.to_string e));
          close t t.error;

(*      lprintf "exce %s in read\n" (Printexc2.to_string e);  *)
          raise e

    end;
  if not (closed t) then begin
      t.event_handler t CAN_REFILL;
      if b.len = 0 then begin
          delete_string b.buf;
          b.pos <- 0;
          must_write t.sock_out false;
          t.event_handler t WRITE_DONE
        end
    end
  )

(*************************************************************************)
(*                                                                       *)
(*                         tcp_handler                                   *)
(*                                                                       *)
(*************************************************************************)

let tcp_handler_write t sock =
  begin
    match t.write_control with
      None ->
        can_write_handler t sock t.wbuf.len
    | Some bc ->
        if bc.total_bytes = 0 then
          can_write_handler t sock t.wbuf.len
        else begin
            bc.connections <- t :: bc.connections;
            bc.nconnections <- t.write_power + bc.nconnections
          end
  end

let get_latencies verbose =
  let b = Buffer.create 300 in
  let counter = ref 0 in
  Hashtbl.iter (fun ip (latency, samples) ->
      incr counter;
  ) latencies;
  LittleEndian.buf_int b !counter;
  Hashtbl.iter (fun ip (latency, samples) ->
      if !verbose then lprintf "   Latency TCP: %s -> %d (%d samples)\n" (Ip.to_string ip) !latency !samples;
      LittleEndian.buf_ip b ip;
      LittleEndian.buf_int16 b !latency;
      LittleEndian.buf_int16 b !samples;
  ) latencies;
  Hashtbl.clear latencies;
  Buffer.contents b

let tcp_handler t event =
  match event with
  | CAN_READ ->
(*      lprintf "CAN_READ\n"; *)
      begin
        match t.read_control with
          None ->
            can_read_handler t t.sock_in 1000000
        | Some bc ->
            if bc.total_bytes = 0 then
              can_read_handler t t.sock_in 1000000
            else begin
(*                lprintf "DELAYED\n";  *)
                if bc.remaining_bytes > 0 then
                  begin
                    bc.connections <- t :: bc.connections;
                    bc.nconnections <- t.read_power + bc.nconnections
                  end
              end
      end
  | CAN_WRITE ->
(*      lprintf "CAN_WRITE\n";   *)
      let can_write =
        if t.nwrite = 0 && t.noproxy && t.connecting then  begin
            t.connecting <- false;
            t.event_handler t CONNECTED;
            t.nwrite = 0
          end else true
      in
      if can_write then tcp_handler_write t t.sock_out
  | _ -> t.event_handler t (BASIC_EVENT event)


(*************************************************************************)
(*                                                                       *)
(*                         Bandwidth Controlers                          *)
(*                                                                       *)
(*************************************************************************)

let read_bandwidth_controlers = ref []
let write_bandwidth_controlers = ref []

let create_read_bandwidth_controler name rate =
  let bc = {
      bc_name = name;
      remaining_bytes = rate;
      total_bytes = rate;
      nconnections = 0;
      connections = [];
      allow_io = ref true;
      remaining_bytes_user = (fun _ _ -> ());
      moved_bytes = Int64.zero;
      lost_bytes = Array.make 3600 0;
      forecast_bytes = 0;
      ndone_last_second = 0;
  } in
  read_bandwidth_controlers := bc :: !read_bandwidth_controlers;
  bc

let create_write_bandwidth_controler name rate =
  let bc = {
      bc_name = name;
      remaining_bytes = rate;
      total_bytes = rate;
      nconnections = 0;
      connections = [];
      allow_io = ref true;
      remaining_bytes_user = (fun _ _ -> ());
      moved_bytes = Int64.zero;
      lost_bytes = Array.make 3600 0;
      forecast_bytes = 0;
      ndone_last_second = 0;
    } in
  write_bandwidth_controlers := bc :: !write_bandwidth_controlers;
  bc

let change_rate bc rate =
  bc.total_bytes <- rate

let bandwidth_controler t sock =
  (match t.read_control with
      None -> ()
    | Some bc ->
(*         must_read sock (bc.total_bytes = 0 || bc.remaining_bytes > 0)); *)
        (* bandwidth_controler is called before socket is engaged into data transfer.
           This can be either upload or download connection, but when download speed
           is capped and fully saturated the above condition will be false,
           consequently the socket will never get [want_read] property and
           will never get considered by the event loop, until finally being 
           closed on timeout. This bug manifests itself with no _new_ BT upload clients
           or inability to connect to servers (DC) when download is running.
           This is a temporary fix, bandwidth limiting logic needs some global refactoring *)
        must_read sock true);
  (match t.write_control with
      None -> ()
    | Some bc ->
        must_write sock ((bc.total_bytes = 0 || bc.remaining_bytes > 0)
          && t.wbuf.len > 0))

let reset_bandwidth_controlers _ =
  List.iter (fun bc ->
      bc.remaining_bytes_user bc.total_bytes bc.remaining_bytes;
      bc.remaining_bytes <- bc.total_bytes - bc.forecast_bytes;
      if !verbose_bandwidth > 0 && bc.ndone_last_second > 0 then
        lprintf_nl "[BW1 %6d] %s read %d/%d last second" (last_time ())
          bc.bc_name bc.ndone_last_second bc.total_bytes;
      if !verbose_bandwidth > 0 && bc.forecast_bytes > 0 then
        lprintf_nl "[BW1 %6d] %s forecast read %d bytes for next second" (last_time ())
          bc.bc_name bc.forecast_bytes;
      bc.forecast_bytes <- 0;
      bc.ndone_last_second <- 0;
      if bc.remaining_bytes > 0 then bc.allow_io := true
(*            lprintf "READ remaining_bytes: %d" bc.remaining_bytes;  *)
  ) !read_bandwidth_controlers;
  List.iter (fun bc ->
      bc.remaining_bytes_user bc.total_bytes bc.remaining_bytes;
      bc.remaining_bytes <- bc.total_bytes - bc.forecast_bytes;
      if !verbose_bandwidth > 0 && bc.ndone_last_second > 0 then
        lprintf_nl "[BW1 %6d] %s wrote %d/%d last second" (last_time ())
          bc.bc_name bc.ndone_last_second bc.total_bytes;
      if !verbose_bandwidth > 0 && bc.forecast_bytes > 0 then
        lprintf_nl "[BW1 %6d] %s forecast write %d bytes for next second" (last_time ())
          bc.bc_name bc.forecast_bytes;
      bc.forecast_bytes <- 0;
      bc.ndone_last_second <- 0;
      if bc.remaining_bytes > 0 then bc.allow_io := true;
(*
          lprintf "WRITE remaining_bytes: %d\n" bc.remaining_bytes;
          *)
  ) !write_bandwidth_controlers

let compute_lost_byte bc =
  if bc.total_bytes = 0 then -1 else
  let sum = ref Int64.zero in
  for i = 0 to 3600-1 do
    sum := !sum ++ (Int64.of_int bc.lost_bytes.(i));
  done;
  Int64.to_int (!sum // 3600L)

let moved_bytes bc = bc.moved_bytes

let set_remaining_bytes_user bc f =
  bc.remaining_bytes_user <- f

let set_lost_bytes bc lost sec =
  bc.lost_bytes.(sec mod 3600) <- lost

(*************************************************************************)
(*                                                                       *)
(*                         Setting handlers                              *)
(*                                                                       *)
(*************************************************************************)

let set_closer t f =
  let old_handler = t.event_handler in
  let handler t ev =
(*    if t.monitored then (lprintf "set_closer handler\n"); *)
    match ev with
      BASIC_EVENT (CLOSED s) ->
(*        lprintf "READ_DONE %d\n" nread; *)
        f t s
    |_ -> old_handler t ev
  in
  t.event_handler <- handler

let set_rtimer t f =
  let old_handler = t.event_handler in
  let handler t ev =
(*    if t.monitored then (lprintf "set_closer handler\n"); *)
    match ev with
      BASIC_EVENT (RTIMEOUT | LTIMEOUT) ->
        f t
    |_ -> old_handler t ev
  in
  t.event_handler <- handler

let set_handler t event handler =
  let old_handler = t.event_handler in
  let handler t ev =
(*    if t.monitored then (lprintf "set_handler handler\n"; ); *)
    if ev = event then
      handler t
    else
      old_handler t ev
  in
  t.event_handler <- handler

let set_refill t f =
  set_handler t CAN_REFILL f;
  (try f t with _ -> ())

let close_after_write t =
  if t.wbuf.len = 0 then begin
(*    lprintf "close_after_write: CLOSE\n";  - log output removed  *)
      shutdown t Closed_by_user
    end
  else
    set_handler t WRITE_DONE (fun t ->
(*      lprintf "close_after_write: CLOSE\n";  - log output removed  *)
        shutdown t Closed_by_user)

let http_proxy = ref None

let set_reader t f =
(*  lprintf "set_reader for %s\n" t.host; *)
  let old_handler = t.event_handler in
  let ff =
    if t.noproxy then
      f
    else
    match !http_proxy with
    | None -> f
    | Some _ ->
        fun sock nread ->
(* HTTP/1.0 200 OK\n\n *)
          let b = buf sock in
          let rcode, rstr, rstr_end =
            try
              let rcode_pos = 8 (*String.index_from b.buf b.pos ' '*) in
              let rcode = Bytes.sub b.buf (rcode_pos+1) 3 in
              let rstr_pos = 12 (*String.index_from b.buf (rcode_pos+1) ' '*) in
              let rstr_end = Bytes.index_from b.buf (rstr_pos+1) '\n' in
              let rstr = Bytes.sub b.buf (rstr_pos+1) (rstr_end-rstr_pos-1) in
              lprintf "From proxy for %s: %s %s\n"
                (Ip.to_string sock.host) (Bytes.unsafe_to_string rcode) (Bytes.unsafe_to_string rstr);
              rcode, rstr, rstr_end
            with _ ->
                Bytes.empty, Bytes.empty, 0
          in
          (match (Bytes.to_string rcode) with
              "200" -> (*lprintf "Connect to client via proxy ok\n";*)
                let pos = Bytes.index_from b.buf (rstr_end+1) '\n' in
                let used = pos + 1 - b.pos in
                sock_used sock used;
                if nread != used then
                  f sock (nread - used)
            | _ ->
(* fall back to user handler *)
                f sock nread);
          let handler t ev =
            match ev with
              READ_DONE nread -> f t nread
            |_ -> old_handler t ev
          in
          t.event_handler <- handler;
          t.connecting <- false;
          t.event_handler t CONNECTED;
          if t.nwrite = 0 then tcp_handler t CAN_WRITE;
          lprintf "old handler set\n"
  in
  let handler t ev =
    match ev with
      READ_DONE nread -> ff t nread
    | _ -> old_handler t ev
  in
  t.event_handler <- handler

let set_handler t event handler =
  let old_handler = t.event_handler in
  let handler t ev =
(*    if t.monitored then (lprintf "set_handler handler\n"; ); *)
    if ev = event then
      handler t
    else
      old_handler t ev
  in
  t.event_handler <- handler

let set_refill t f =
  set_handler t CAN_REFILL f;
  if t.wbuf.len = 0 then (try f t with _ -> ())

let set_connected t f =
  set_handler t CONNECTED f

(*************************************************************************)
(*                                                                       *)
(*                         Socket Configuration                          *)
(*                                                                       *)
(*************************************************************************)

let set_read_controler t bc =
  t.read_control <- Some bc;
(*  set_before_select t.sock (bandwidth_controler t); *)
  set_allow_read t.sock_in bc.allow_io;
  bandwidth_controler t t.sock_in

let set_write_controler t bc =
  t.write_control <- Some bc;
(*  set_before_select t.sock (bandwidth_controler t); *)
  set_allow_write t.sock_out bc.allow_io;
  bandwidth_controler t t.sock_out

let set_monitored t b = t.monitored <- b
let monitored t = t.monitored

let set_rtimeout s t = set_rtimeout s.sock_in t
let set_wtimeout s t = set_wtimeout s.sock_out t

let set_write_power t p = (* t.write_power <- p *) ()
let set_read_power t p = (* t.read_power <- p *) ()

let set_lifetime s = set_lifetime s.sock_in

(*************************************************************************)
(*                                                                       *)
(*                         Printing Information                          *)
(*                                                                       *)
(*************************************************************************)

let dump_socket t buf =
  print_socket buf t.sock_in;
  print_socket buf t.sock_out;
  Printf.bprintf buf "rbuf: %d/%d wbuf: %d/%d\n" t.rbuf.len
    (Bytes.length t.rbuf.buf) t.wbuf.len (Bytes.length t.wbuf.buf)

let stats buf t =
  BasicSocket.stats buf t.sock_in;
  BasicSocket.stats buf t.sock_out;
  Printf.bprintf buf "  rbuf size: %d/%d\n" (Bytes.length t.rbuf.buf)
  t.rbuf.max_buf_size;
  Printf.bprintf buf "  wbuf size: %d/%d\n" (Bytes.length t.wbuf.buf)
  t.wbuf.max_buf_size

(*************************************************************************)
(*                                                                       *)
(*                         create                                        *)
(*                                                                       *)
(*************************************************************************)

let create token name fd handler =
  use_token token fd;
  MlUnix.set_close_on_exec fd;
  let t = {
      name = name;
      token = token;
      closing = false;
      sock_in = dummy_sock;
      sock_out = dummy_sock;
      rbuf = buf_create !max_buffer_size;
      wbuf = buf_create !max_buffer_size;
      event_handler = handler;
      error = Closed_by_peer;
      nread = 0;
      ndown_packets = 0;
      nwrite = 0;
      nup_packets = 0;
      monitored = false;
      read_control = None;
      write_control = None;
      write_power = 1;
      read_power = 1;
      connect_time = 0.;
      peer_addr = None;
      my_ip = Ip.null;
      noproxy = true;
      connecting = false;
      host = Ip.null;
      compression = None;
    } in
  let sock = BasicSocket.create name fd (fun _ event ->
        tcp_handler t event) in
  set_printer sock (fun sock ->
        Printf.sprintf "%s (nread: %d nwritten: %d) [U %s,D %s]" name t.nread t.nwrite
        (string_of_bool (t.read_control <> None)) (string_of_bool (t.write_control <> None));
        ;
    );
  set_dump_info sock (dump_socket t);
  if !debug then
      lprintf_nl "[tBS] fd %d %s" (sock_num sock) name;
  t.sock_in <- sock;
  t.sock_out <- sock;
  t

(*************************************************************************)
(*                                                                       *)
(*                         create_pipe                                   *)
(*                                                                       *)
(*************************************************************************)

let create_pipe token name fd_in fd_out handler =
  use_token token fd_in;
  MlUnix.set_close_on_exec fd_in;
  MlUnix.set_close_on_exec fd_out;
  let t = {
      name = name;
      token = token;
      closing = false;
      sock_in = dummy_sock;
      sock_out = dummy_sock;
      rbuf = buf_create !max_buffer_size;
      wbuf = buf_create !max_buffer_size;
      event_handler = handler;
      error = Closed_by_peer;
      nread = 0;
      ndown_packets = 0;
      nwrite = 0;
      nup_packets = 0;
      monitored = false;
      read_control = None;
      write_control = None;
      write_power = 1;
      read_power = 1;
      peer_addr = None;
      my_ip = Ip.null;
      noproxy = true;
      connecting = false;
      host = Ip.null;
      connect_time = 0.;
      compression = None;
    } in

  let fname = (fun _ ->
        Printf.sprintf "%s (nread: %d nwritten: %d) [U %s,D %s]" name
          t.nread t.nwrite (string_of_bool (t.read_control <> None))
        (string_of_bool (t.write_control <> None));
        ;
    ) in


  let sock_in = BasicSocket.create name fd_in (fun _ event ->
        tcp_handler t event) in
  if !debug then
      lprintf_nl "[tBS] fd %d %s" (sock_num sock_in) name;
  set_printer sock_in fname;
  set_dump_info sock_in (dump_socket t);

  let sock_out = BasicSocket.create name fd_out (fun _ event ->
        tcp_handler t event) in
  set_printer sock_out fname;
  set_dump_info sock_out (dump_socket t);

  t.sock_in <- sock_in;
  t.sock_out <- sock_out;
  t

(*************************************************************************)
(*                                                                       *)
(*                         create_blocking                               *)
(*                                                                       *)
(*************************************************************************)

let create_blocking token name fd handler =
  use_token token fd;
  MlUnix.set_close_on_exec fd;
  let t = {
      name = name;
      token = token;
      closing = false;
      sock_in = dummy_sock;
      sock_out = dummy_sock;
      rbuf = buf_create !max_buffer_size;
      wbuf = buf_create !max_buffer_size;
      event_handler = handler;
      error = Closed_by_peer;
      nread = 0;
      ndown_packets = 0;
      nwrite = 0;
      nup_packets = 0;
      monitored = false;
      read_control = None;
      write_control = None;
      write_power = 1;
      read_power = 1;
      peer_addr = None;
      my_ip = Ip.null;
      noproxy = true;
      connecting = false;
      host = Ip.null;
      connect_time = 0.;
      compression = None;
    } in
  let sock = create_blocking name fd (fun sock event ->
        tcp_handler t event) in
  t.sock_in <- sock;
  t.sock_out <- sock;
  set_dump_info sock (dump_socket t);
  t

let create_simple token name fd =
  create token name fd (fun _ _ -> ())

(*************************************************************************)
(*                                                                       *)
(*                         connect                                       *)
(*                                                                       *)
(*************************************************************************)

let connect token name host port handler =
  if token.token_used then failwith "Token already used";
  try
(*    lprintf "CONNECT %s:%d\n" (Unix.string_of_inet_addr host) port; *)
    let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    if !bind_address <> Unix.inet_addr_any then
      Unix.bind s (Unix.ADDR_INET (!bind_address, 0));
    let proxy_ip, proxy_port, proxy_auth =
      match !http_proxy with
      | None -> Ip.null, 0, None
      | Some (h, p, auth) -> Ip.from_name h, p, auth
    in
    let ip = Ip.of_inet_addr host in
    let use_proxy = proxy_ip <> Ip.null && proxy_ip <> ip in
    if use_proxy then begin
(* connect to proxy in blocking mode, so we sure, connections established when we send CONNECT *)
        lprintf "via proxy\n";
        Unix.connect s (Unix.ADDR_INET(Ip.to_inet_addr proxy_ip, proxy_port));

        let buf = Buffer.create 200 in
        let dotted_host = Unix.string_of_inet_addr host in
        Printf.bprintf buf "CONNECT %s:%d HTTP/1.1\n" dotted_host port;
        Printf.bprintf buf "Pragma: no-cache\n";
        Printf.bprintf buf "Cache-Control: no-cache\n";
        Printf.bprintf buf "Connection: Keep-Alive\n";
        Printf.bprintf buf "Proxy-Connection: Keep-Alive\n";
        begin match proxy_auth with
        | Some (login,password) ->
            Printf.bprintf buf "Proxy-Authorization: Basic %s\n" (Base64.encode_to_string (login ^ ":" ^ password))
        | None -> () 
        end;
        Printf.bprintf buf "User-Agent: MLdonkey/%s\n" Autoconf.current_version;
        Printf.bprintf buf "\n";
        ignore (MlUnix.write s (Buffer.to_bytes buf) 0 (Buffer.length buf))
      end;
    let t = create token name s handler in

    if !verbose_bandwidth > 1 then
      lprintf_nl "[BW2 %6d] connect on %s:%d" (last_time ()) t.name (sock_num t.sock_out);

    token.connection_manager.nconnections_last_second <-
      token.connection_manager.nconnections_last_second + 1;

    must_write t.sock_out true;
    try
      t.host <- ip;
(*      lprintf "add_connect at %f\n" (current_time ()); *)
      t.connect_time <- current_time ();
      if use_proxy then begin
          t.noproxy <- false;
        end else
        Unix.connect s (Unix.ADDR_INET(host,port));

      t.connecting <- true;
      register_upload t 0;    (* The TCP SYN packet *)
      forecast_download t 0;  (* The TCP ACK packet *)
      forecast_upload t 0;    (* The TCP ACK packet *)
(*      if use_proxy then begin
        end; *)
      t
    with
      Unix.Unix_error((Unix.EINPROGRESS|Unix.EINTR|Unix.EWOULDBLOCK),_,_) ->
        t.connecting <- true;
        register_upload t 0;             (* The TCP SYN packet *)
        forecast_download t 0;  (* The TCP ACK packet *)
        forecast_upload t 0;    (* The TCP ACK packet *)
        t
    | Unix.Unix_error (Unix.ENETUNREACH,_,_) as e ->
        (* log nothing here, but later in donkeyClient.ml *)
        close t Closed_connect_failed;
        raise e
    | e ->
        close t Closed_connect_failed;
        raise e
  with
    Unix.Unix_error (Unix.ENETUNREACH,_,_) as e -> raise e (* avoid logging *)
  | Unix.Unix_error (Unix.ENOBUFS,_,_) as e ->
      if Autoconf.windows then lprintf_nl
        "No more free buffers, read http://support.microsoft.com/kb/q196271/ to fix this problem";
      raise e
  | e ->
      lprintf_nl "Exception (%s) before connect to host %s:%d"
          (Printexc2.to_string e) (Unix.string_of_inet_addr host) port;
      raise e



(*************************************************************************)
(*                                                                       *)
(*                         IP addresses                                  *)
(*                                                                       *)
(*************************************************************************)


let my_ip t =
  if t.my_ip = Ip.null then
    let fd = fd t.sock_in in
    match Unix.getsockname fd with
      Unix.ADDR_INET (ip, port) ->
        let ip = Ip.of_inet_addr ip in
        t.my_ip <- ip;
        ip
    | _ -> raise Not_found
  else t.my_ip

let peer_addr t =
  match t.peer_addr with
    Some (ip, port) -> (ip,port)
  | None ->
      let fd = fd t.sock_out in
      match Unix.getpeername fd with
        Unix.ADDR_INET (ip, port) ->
          let ip = Ip.of_inet_addr ip in
          t.peer_addr <- Some (ip, port);
          ip, port
  | _ -> raise Not_found

let peer_ip t = fst (peer_addr t)
let peer_port t = snd (peer_addr t)

          (*
let host t =
  let fd = fd t.sock_out in
  match Unix.getpeername fd with
    Unix.ADDR_INET (ip, port) -> Ip.of_inet_addr ip, port
  | _ -> raise Not_found
        *)

(*************************************************************************)
(*                                                                       *)
(*                         Sending Marshalled Values                     *)
(*                                                                       *)
(*************************************************************************)

open AnyEndian
open LittleEndian

let internal_buf = Buffer.create 17000

let simple_send_buf buf sock =
  let s = Buffer.to_bytes buf in
  Buffer.reset buf;
  buf_int8 buf 228;
  let len = Bytes.length s in
  buf_int buf len;
  write sock (Buffer.to_bytes buf) 0 5;
  write sock s 0 len

let value_send sock m =
  (* Buffer.reset internal_buf; *)
  Buffer.reset internal_buf;
  Buffer.add_string internal_buf (Marshal.to_string m []);
  simple_send_buf internal_buf sock

let value_handler f sock nread =
  let b = buf sock in
  try
    while b.len >= 5 do
      let msg_len = get_int_bytes b.buf (b.pos+1) in
      if b.len >= 5 + msg_len then
        begin
          let s = Bytes.sub b.buf (b.pos+5) msg_len in
          let t = Marshal.from_bytes  s 0 in
          buf_used b  (msg_len + 5);
          f t sock;
          ()
        end
      else raise Not_found
    done
  with Not_found -> ()

(*************************************************************************)
(*                                                                       *)
(*                         exec_command                                  *)
(*                                                                       *)
(*************************************************************************)

let exec_command token cmd args handler =
  MlUnix.execvp_command cmd args (fun in_read out_write ->
      let t = create_pipe token "pipe" in_read out_write
        handler in
      must_read t.sock_in false;
      t
  )


(*************************************************************************)
(*                                                                       *)
(*                         Setting configuration                         *)
(*                                                                       *)
(*************************************************************************)

let set_max_connections_per_second f =
  max_connections_per_second := f

let set_max_opened_connections f =
  max_opened_connections := f

let set_max_input_buffer  t len =
  t.rbuf.max_buf_size <- len

let set_max_output_buffer  t len =
  t.wbuf.max_buf_size <- len

(*************************************************************************)
(*                                                                       *)
(*                         Compression                                   *)
(*                                                                       *)
(*************************************************************************)

let to_deflate = ref []
let to_deflate_len = ref 0

let compression_buffer_len = !max_buffer_size / 10
let compression_buffer = String.create compression_buffer_len

let deflate_connection sock =
  (* lprintf "Creating deflate connection\n"; *)
  let comp = Some (Zlib.inflate_init true, Zlib.deflate_init 6 true,
      buf_create !max_buffer_size, buf_create !max_buffer_size) in
  sock.compression <- comp

let rec iter_deflate sock zs wbuf =
  if wbuf.len > 0 then begin
(*      lprintf "iter_deflate\n"; *)
      let (_, used_in, used_out) = Zlib.deflate zs
          wbuf.buf wbuf.pos wbuf.len
          compression_buffer 0 compression_buffer_len
          Zlib.Z_SYNC_FLUSH in
(*
     lprintf "deflated %d/%d -> %d\n" used_in wbuf.len used_out;
      lprintf "[%s]\n" (String.escaped (String.sub compression_buffer 0 used_out));
*)
      write sock compression_buffer 0 used_out;
      buf_used wbuf used_in;
      if used_in > 0 || used_out > 0 then
        iter_deflate sock zs wbuf
    end

let deflate_timer _ =
  List.iter (fun sock ->
      try
        match sock.compression with
          Some (_, zs, _, wbuf) ->
            if closed sock then raise Exit;
            iter_deflate sock zs wbuf
        | _ -> ()
      with e ->
          lprintf "[ERROR] Exception %s in CanBeCompressed.deflate_timer\n"
            (Printexc2.to_string e)
  ) !to_deflate;
  to_deflate := [];
  to_deflate_len := 0

let _to_deflate conn =
  if not (List.memq conn !to_deflate) then
    to_deflate := conn :: !to_deflate;
  if !to_deflate_len > 1000000 then
    deflate_timer ()

let rec iter_inflate zs sock b rbuf =
  if b.len > 0 then begin
(*
lprintf "iter_inflate %d\n" b.len;
lprintf "[%s]\n" (String.escaped (String.sub b.buf b.pos b.len));
*)
      let (_, used_in, used_out) = Zlib.inflate zs b.buf b.pos b.len
          compression_buffer 0 compression_buffer_len
          Zlib.Z_SYNC_FLUSH in
(*
      lprintf "inflated %d/%d -> %d\n" used_in b.len used_out;
      lprintf "[%s]\n" (String.escaped (String.sub compression_buffer 0 used_out));
*)
      buf_add sock rbuf compression_buffer 0 used_out;
      buf_used b used_in;
      if used_in > 0 || used_out > 0 then
        iter_inflate zs sock b rbuf
    end

let buf t =
  match t.compression with
    None -> t.rbuf
  | Some (zs, _, rbuf, _) ->
(*      lprintf "CanBeCompressed.buf\n"; *)
      let b = buf t in
      if b.len > 0 then iter_inflate zs t b rbuf;
      rbuf

let write t s pos len =
  match t.compression with
    None -> write t s pos len
  | Some (_,_, _,wbuf) ->

      to_deflate_len := !to_deflate_len + len;
      _to_deflate t;
      buf_add t wbuf s pos len

let write_bytes t s = write t s 0 (Bytes.length s)
let write_string t s = write_bytes t (Bytes.unsafe_of_string s)

(*************************************************************************)
(*                                                                       *)
(*                         MAIN                                          *)
(*                                                                       *)
(*************************************************************************)

let _ =
  add_bandwidth_second_timer (fun _ ->
      reset_bandwidth_controlers ();
      reset_connection_scheduler ();
      deflate_timer ();
  );

  set_before_select_hook (fun _ ->
      schedule_connections ();
      List.iter (fun bc ->
          let old_value = !(bc.allow_io) in
          bc.allow_io := (bc.total_bytes = 0 || bc.remaining_bytes > 0);
          if !verbose_bandwidth > 2 && (old_value <> !(bc.allow_io)) then
            lprintf_nl "[BW3 %6d] %20s: stop reading" (last_time ()) bc.bc_name
      ) !read_bandwidth_controlers;
      List.iter (fun bc ->
          let old_value = !(bc.allow_io) in
          bc.allow_io := (bc.total_bytes = 0 || bc.remaining_bytes > 0);
          if !verbose_bandwidth > 2 && (old_value <> !(bc.allow_io)) then
            lprintf_nl "[BW3 %6d] %20s: stop writing" (last_time ()) bc.bc_name
      ) !write_bandwidth_controlers;
  );

(*
Some ideas:

* sort the connections so that we try to read and write as
  much as possible to/from connections on which we already have read/written
  a lot.
* postpone reads and writes for a few seconds so that we have more to
  read/write.

  dhcppc3:~#  ping computer.domain.org -f -s <packet_size> -c 1000
 where computer.domain.org is a nearby computer on a 100 Mbs link.

On my cable link, making <packet_size> vary from 1 to 2000 shows:
  * packets > 1450 are 100% lost
  * 300 > packets > 1 are 5% lost but exactly the same time (14.5 s)
  * packets > 300 are 30% lost and higher time (17s)

So, we can suppose that sending 250 bytes is exactly the same as sending one
byte, and the contrary, so [TODO]: round up all packets sent and received
to an option 'packet_unit' (default 250). Ask Simon if it makes sense.

1000 packets transmitted, 947 received, 5% packet loss, time 14191ms
, pipe 6, ipg/ewma 14.205/0.000 ms

  *)


  set_after_select_hook (fun _ ->
      List.iter (fun bc ->
          if bc.remaining_bytes > 0 then begin

              bc.connections <- List.sort (fun t1 t2 ->
                  let w1 = t1.nread in
                  let w2 = t2.nread in
                  compare w1 w2
              ) bc.connections;

              if !verbose_bandwidth > 2 && bc.nconnections > 0 then begin
                  lprintf_nl "[BW3 %6d] %d read-waiting connections for %d allowed" (last_time ()) bc.nconnections bc.remaining_bytes;
                  List.iter (fun t ->
                      lprintf_nl "[BW3 %6d]   %s:%d [buffered %d]" (last_time ()) t.name (sock_num t.sock_in) ((buf t).len);
                  ) bc.connections;
                end;


              List.iter (fun t ->
                  if bc.remaining_bytes > 0 then
                    let nconnections = max bc.nconnections 1 in
                    let can_read = max 1 (bc.remaining_bytes / nconnections) in
                    let can_read = max !ip_packet_size (can_read * t.read_power) in
                    (try
(*                    lprintf "allow to read %d\n" can_read; *)
                        can_read_handler t t.sock_in can_read
                      with e ->
(*                          lprintf "Exception %s in can_read_handler %s:%d\n"
                            (Printexc2.to_string e)
                          t.name (sock_num t.sock_in) *) ()
                    );
                    bc.nconnections <- bc.nconnections - t.read_power
                  else
                    if !verbose_bandwidth > 2 then
                      lprintf_nl "[BW3 %6d]    %s:%d could not read" (last_time ())
                        t.name (sock_num t.sock_out)
              ) bc.connections;
(*              if bc.remaining_bytes > 0 then bc.allow_io := false; *)
            end;

          if !verbose_bandwidth > 2 then begin

              if bc.remaining_bytes > 0 then
                lprintf_nl "[BW3 %6d] still %d bytes to read" (last_time ()) bc.remaining_bytes;

              if bc.nconnections > 0 then
                lprintf_nl "[BW3 %6d] still %d read-waiting connections after loop"
                  (last_time ()) bc.nconnections;
            end;


          bc.connections <- [];
          bc.nconnections <- 0;
      ) !read_bandwidth_controlers;
      List.iter (fun bc ->
          if bc.remaining_bytes > 0 then begin

              bc.connections <- List.sort (fun t1 t2 ->
                  let w1 = remaining_to_write t1 in
                  let w2 = remaining_to_write t2 in
                  compare w1 w2
              ) bc.connections;

              if !verbose_bandwidth > 2 && bc.nconnections > 0 then begin
                  lprintf_nl "[BW3 %6d] %d write-waiting connections for %d allowed"
                    (last_time ()) bc.nconnections bc.remaining_bytes;
                  List.iter (fun t ->
                      lprintf_nl "[BW3 %6d]   %s:%d [buffered %d]"
                        (last_time ()) t.name (sock_num t.sock_out) (remaining_to_write t);
                  ) bc.connections;
                end;
              List.iter (fun t ->
                  if bc.remaining_bytes > 0 then
                    let nconnections = max bc.nconnections 1 in
                    let can_write = max 1 (bc.remaining_bytes / nconnections) in
                    let can_write = best_packet_size (can_write * t.write_power)
                    in
                    let old_nwrite = t.nwrite in
                    (try
(*                    lprintf "WRITE\n";  *)
                        can_write_handler t t.sock_out (min can_write t.wbuf.len)
                      with _ -> ());
                    bc.remaining_bytes <- bc.remaining_bytes -
                    t.nwrite + old_nwrite;
                    bc.nconnections <- bc.nconnections - t.write_power;
                  else
                  if !verbose_bandwidth > 2 then
                    lprintf_nl "[BW3 %6d]    %s:%d could not write buffered %d bytes"
                      (last_time ()) t.name (sock_num t.sock_out) (remaining_to_write t)
              ) bc.connections;
(*            if bc.remaining_bytes > 0 then bc.allow_io := false; *)
            end;
          if !verbose_bandwidth > 2 then begin

              if bc.remaining_bytes > 0 then begin
                  lprintf_nl "[BW3 %6d] still %d bytes to write"
                    (last_time ()) bc.remaining_bytes;
                  List.iter (fun t ->
                      let len = remaining_to_write t in
                      if len > 0 then
                        lprintf_nl "[BW3 %6d]    %s:%d could write %d" (last_time ())
                          t.name (sock_num t.sock_out) len
                  ) bc.connections
                end;

              if bc.nconnections > 0 then
                lprintf_nl "[BW3 %6d] still %d write-waiting connections after loop"
                  (last_time ()) bc.nconnections;
            end;
          bc.connections <- [];
          bc.nconnections <- 0;
      ) !write_bandwidth_controlers
  )

let prevent_close t =
  prevent_close t.sock_in;
  prevent_close t.sock_out

let must_write t bool = must_write t.sock_out bool
let output_buffered t = t.wbuf.len

let _ =
  Heap.add_memstat "tcpBufferedSocket" (fun level buf ->
      Printf.bprintf buf "  %d latencies\n" (Hashtbl.length latencies);
      Printf.bprintf buf "  String.length big_buffer: %d\n" (Bytes.length big_buffer);
      Printf.bprintf buf "  connection_managers: %d\n" (List.length !connection_managers);
      Printf.bprintf buf "  read_bandwidth_controlers: %d\n" (List.length !read_bandwidth_controlers);
      Printf.bprintf buf "  write_bandwidth_controlers: %d\n" (List.length !write_bandwidth_controlers);
      Printf.bprintf buf "  to_deflate: %d\n" (List.length !to_deflate);
      Printf.bprintf buf "  max_opened_connections: %d\n" (!max_opened_connections ());
      Printf.bprintf buf "  max_connections_per_second: %d\n" (!max_connections_per_second ());
      Printf.bprintf buf "  max_buffer_size: %d\n" (!max_buffer_size);
  )
