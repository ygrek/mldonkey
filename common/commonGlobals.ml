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
open CommonOptions
open BasicSocket
open CommonTypes

  
let version = "
MLDonkey: Objective-Caml Client/Server for the eDonkey2000 Network
Release: 1.99
"

    
let one_day = 3600. *. 24.
let half_day = one_day /. 2.
  

let printf_char c =
  if !!verbose then 
    (print_char c; Pervasives.flush Pervasives.stdout)
    
let printf_string c =
  if !!verbose then 
    (print_string c; Pervasives.flush Pervasives.stdout)

  
let new_connection_control last_conn = {
    control_next_try = last_time () -. 1.;
    control_last_conn = last_conn;
    control_next_delay = !!min_retry_delay;
  }
  
let connection_ok cc = 
  cc.control_next_delay <- !!min_retry_delay;
  cc.control_last_conn <- last_time ();
  cc.control_next_try <- last_time () +. !!min_retry_delay
  
let connection_try cc =
  cc.control_next_try <- last_time () +. cc.control_next_delay

let connection_failed cc =
  cc.control_next_delay <- minf (cc.control_next_delay *. 2.) half_day

let connection_can_try cc =
  cc.control_next_try < last_time ()
  
let connection_must_try cc =
  cc.control_next_try <- last_time () -. 1.

let connection_set_last_conn cc lc =
  cc.control_last_conn <- lc

let connection_last_conn cc =
  cc.control_last_conn

let connection_delay cc =
  cc.control_next_try <- maxf cc.control_next_try
    (last_time () +. !!min_retry_delay)

    
let can_open_connection () =
  nb_sockets () < !!max_opened_connections 
  
let upload_control = TcpBufferedSocket.create_write_bandwidth_controler 
    (!!max_hard_upload_rate * 1024)
  
let download_control = TcpBufferedSocket.create_read_bandwidth_controler 
    (!!max_hard_download_rate * 1024)

let ip_verified = ref 0
    
let verify_ip sock =
  if !ip_verified < 10 then
(*  Printf.printf "VERIFY IP";  print_newline (); *)
  try
    incr ip_verified;
    let ip = TcpBufferedSocket.my_ip sock in
    if ip <> Ip.localhost  then begin
        Printf.printf "USING %s FOR CLIENT IP" (Ip.to_string ip);
        print_newline ();
        client_ip =:= ip;
        ip_verified := 10;
      end;
  with e -> 
      Printf.printf "Exception %s while verifying IP address"
        (Printexc.to_string e); print_newline ()

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
  
let (guis : CommonTypes.gui_record list ref) = ref []

let gui_server_sock = ref (None : TcpServerSocket.t option)
      
let exit_handlers = ref []
let do_at_exit f =
  exit_handlers := f :: !exit_handlers
      
let exit_properly _ =
(*  Printf.printf "exit_properly handlers"; print_newline (); *)
  List.iter (fun f -> try 
(*        Printf.printf "exit_properly handler ..."; print_newline (); *)
        f () ;
(*        Printf.printf "exit_properly done"; print_newline (); *)
      with e -> 
          Printf.printf "exit_properly (exception %s)"
            (Printexc.to_string e); print_newline ();
  ) !exit_handlers;
(*  Printf.printf "exit_properly DONE"; print_newline (); *)
  Pervasives.exit 0

let user_socks = ref ([] : TcpBufferedSocket.t list)
let dialog_history = ref ([] : (int * string * string) list )

  
  
let want_and_not f value =
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
  if !not_wanted = "" then
    f !wanted
  else
  if !wanted = "" then
    f !wanted
  else
    QAndNot (f !wanted, f !not_wanted)

let want_comb_not comb f value =
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
      [] -> raise Not_found
    | w :: tail -> 
        List.fold_left (fun q w ->
            comb q  (f w)
        ) (f w) tail
  in
  match !not_wanted with
    [] -> wanted
  | w :: tail ->
      QAndNot (wanted, 
        List.fold_left (fun q w ->
            comb q  (f w)
        ) (f w) tail)

let or_comb q1 q2 = QOr(q1,q2)
let and_comb q1 q2 = QAnd(q1,q2)

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

let simplify_query q =
  match rec_simplify_query q with
    QNone -> QHasWord " "
  | q' -> q'

            
let string_of_tags tags =
  let buf = Buffer.create 100 in
      List.iter (fun t ->
          Buffer.add_string buf (Printf.sprintf "%-3s "
              (match t.tag_value with
                String s -> s
              | Uint32 i -> Int32.to_string i
              | Fint32 i -> Int32.to_string i
              | _ -> "???"
            ))
  ) tags;
  Buffer.contents buf

      
  
  (* first GUI have gui_num = 2, since newly created objects have _update = 1 *)
let gui_counter = ref 1
  