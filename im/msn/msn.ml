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

open Md4
open ImTypes
open BasicSocket
open TcpBufferedSocket
open Options
  
open ImProtocol
open ImAccount
open ImTypes
open ImOptions
  
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)

type switchboard_server = {
    mutable ss_sock : TcpBufferedSocket.t option;
    mutable ss_users : string list;
    mutable ss_messages : (string * string) list;
  }

type account = {
    mutable account_sock : TcpBufferedSocket.t option;
    mutable account_password : string;
    mutable account_login : string;
    mutable account_account : account account_impl;
    mutable account_autologin : bool;
    mutable tryId : int;
    mutable msn_sock : TcpBufferedSocket.t option;
    mutable msn_switches :   ((Ip.t * int) * switchboard_server) list;
    mutable msn_xfr_requests : (int * switchboard_server) list;
    mutable ns_sock : TcpBufferedSocket.t option;
    mutable ns_ip : Ip.t;
    mutable ns_port : int;
  }
  
let protocol = ImProtocol.new_protocol "MSN" ()
let (protocol_ops : unit protocol_impl) = as_protocol_impl protocol

let (account_ops : account ImAccount.account_ops) = 
  ImAccount.new_account_ops protocol
  
let (chat_ops : chat ImChat.chat_ops) = 
  ImChat.new_chat_ops protocol
  
let (room_ops : room ImRoom.room_ops) = 
  ImRoom.new_room_ops protocol
  
let (identity_ops : identity ImIdentity.identity_ops) = 
  ImIdentity.new_identity_ops protocol

          
let as_account a = as_account a.account_account    

  

(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)

  
  
let msn_server = "messenger.hotmail.com"
let msn_port = 1863
  
let mime_header = 
  Printf.sprintf "MIME-Version: 1.0\r\nContent-Type: text/plain; charset=UTF-8\r\nUser-Agent: mldonkey/%s\r\nX-MMS-IM-Format: FN=Arial; EF=; CO=0; PF=0\r\n\r\n"
    Autoconf.current_version

let mime_header_len = String.length mime_header
  
let write_string sock s =
  lprintf "SEND [%s]" (String.escaped s); lprint_newline ();
  write_string sock s
  
  
  
let get_ms_sock account =
  match account.msn_sock with
    None -> failwith "NOT CONNECTED"
  | Some sock -> sock

let get_ns_sock account =
  match account.ns_sock with
    None -> failwith "NOT CONNECTED"
  | Some sock -> sock

let msn_ms_handler account s event = 
  match event with
    BASIC_EVENT (CLOSED s) ->
      lprintf "disconnected from Master Server"; lprint_newline ();
      account.msn_sock <- None
  | _ -> ()

let msn_ns_handler account s event = 
  match event with
    BASIC_EVENT (CLOSED s) ->
      lprintf "disconnected from Notification Server"; lprint_newline ();
      account.ns_sock <- None
  | _ -> ()

let msn_ss_handler account ss s event = 
  match event with
    BASIC_EVENT (CLOSED s) ->
      lprintf "disconnected from Switchboard Server"; lprint_newline ();
      ss.ss_sock <- None;
      account.msn_switches <- List.filter (fun (_,s) -> s != ss) 
      account.msn_switches
  | _ -> ()

let msn_error errcode = 
  match errcode with
  
  | 200 -> 
      "Syntax Error (probably a Gaim bug)"
  
  | 201 -> 
      "Invalid Parameter (probably a Gaim bug)"
  
  | 205 -> 
      "Invalid User"
  
  | 206 -> 
      "Fully Qualified Domain Name missing"
  
  | 207 -> 
      "Already Login"
  
  | 208 -> 
      "Invalid Username"
  
  | 209 -> 
      "Invalid Friendly Name"
  
  | 210 -> 
      "List Full"
  
  | 215 -> 
      "Already there"
  
  | 216 -> 
      "Not on list"
  
  | 217 -> 
      "User is offline"
  
  | 218 -> 
      "Already in the mode"
  
  | 219 -> 
      "Already in opposite list"
  
  | 280 -> 
      "Switchboard failed"
  
  | 281 -> 
      "Notify Transfer failed"
  
  
  | 300 -> 
      "Required fields missing"
  
  | 302 -> 
      "Not logged in"
  
  
  | 500 -> 
      "Internal server error"
  
  | 501 -> 
      "Database server error"
  
  | 510 -> 
      "File operation error"
  
  | 520 -> 
      "Memory allocation error"
  
  
  | 600 -> 
      "Server busy"
  
  | 601 -> 
      "Server unavailable"
  
  | 602 -> 
      "Peer Notification server down"
  
  | 603 -> 
      "Database connect error"
  
  | 604 -> 
      "Server is going down (abandon ship)"
  
  
  | 707 -> 
      "Error creating connection"
  
  | 711 -> 
      "Unable to write"
  
  | 712 -> 
      "Session overload"
  
  | 713 -> 
      "User is too active"
  
  | 714 -> 
      "Too many sessions"
  
  | 715 -> 
      "Not expected"
  
  | 717 -> 
      "Bad friend file"
  
  
  | 911 -> 
      "Authentication failed"
  
  | 913 -> 
      "Not allowed when offline"
  
  | 920 -> 
      "Not accepting new users"
  
  | 924 -> 
      "User unverified"
  
  | _ -> 
      "Unknown Error Code"

let incr_tryId account =
  account.tryId <- account.tryId + 1
 
let msn_really_send account sock msg =   
  let msg = msg in (* translate to utf8 *)
  write_string sock
    (Printf.sprintf "MSG %d N %d\r\n%s%s" account.tryId
      (mime_header_len + String.length msg)
    mime_header msg);
  incr_tryId account
              
let rec msn_ss_parser account ss sock tokens msg =
  match tokens with
    []  -> assert false
      
  | "USR" :: _ -> 
      begin
        match ss.ss_users with
          [] -> lprintf "NO USER!!!"; lprint_newline ();
        | who :: _ -> 
            write_string sock (Printf.sprintf "CAL %d %s\r\n" account.tryId who);
            incr_tryId account
      end

  | "CAL" :: _ -> ()
      
  | "JOI" :: _ :: user :: _ ->
      let rec iter list left =
        match list with 
          (who, msg) :: tail ->
            if who = user then (msn_really_send account sock msg; iter tail left)
            else iter tail ((who,msg) :: left)
        | [] -> left
      in
      ss.ss_messages <- List.rev (iter ss.ss_messages [])

  | "MSG" :: _ :: user :: _ -> 
      let len = String.length msg in
      let rec iter pos =
        if pos + 3 >= len then len  else
        if msg.[pos] = '\r' && msg.[pos+1] = '\n' &&
          msg.[pos+2] = '\r' && msg.[pos+3] = '\n' then
          pos+4 else iter (pos+1)
      in
      let pos = iter 0 in
      let msg = String.sub msg pos (len-pos) in
      lprintf "MSG RECEIVED FROM %s : [%s]" user 
        (String.escaped msg); lprint_newline ();

  | "NAK" :: _ ->
      lprintf "The session is probably closed, and the message was 
      not received"; lprint_newline ();
      close sock "closed session"
      
  | "BYE" :: user :: _ ->
      lprintf "User %s is now unavailable" user; lprint_newline ();
      
  | opcode :: _ ->
      lprintf "UNKNOWN OPCODE (SS)                        [%s]" opcode; 
      lprint_newline ();
      match opcode.[0] with
        '0' .. '9' ->
          lprintf "ERROR %s" (msn_error (int_of_string opcode));
          lprint_newline ();
      | _ ->
          lprintf "UNKNOWN MESSAGE"; lprint_newline ();
          ()

      
let rec msn_parser account sock tokens msg =
  match tokens with
    []  -> assert false
  | "VER" :: _ :: "MSNP5" :: _ -> 
      write_string sock (Printf.sprintf "INF %d\r\n" account.tryId);
      incr_tryId account
  
  | "INF" :: _ :: "MD5" :: _ ->
      write_string sock (Printf.sprintf "USR %d MD5 I %s\r\n" account.tryId
          account.account_login
      );
      incr_tryId account

  | "ADD" :: _ (* num *) :: "RL" :: _ (* 2 *) :: user :: friend :: _ -> 
       write_string sock (Printf.sprintf "ADD %d AL %s %s\r\n" account.tryId
          user friend
      );
      incr_tryId account
      
  | "XFR" :: _ :: "NS" :: ns :: _ ->
      let name, port = try
          String2.cut_at ns ':' with _ -> ns, "1863"
      in
      let port = int_of_string port in
      let ip = Ip.from_name name in
      lprintf "XFR connect to [%s] %d" (Ip.to_string ip) port;
      lprint_newline ();
      close sock "ok";
      account.ns_ip <- ip;
      account.ns_port <- port;
      msn_ns_connect account
  
  | "XFR" :: num :: "SB" :: ns :: _ :: auth :: _ ->
      let name, port = try
          String2.cut_at ns ':' with _ -> ns, "1863"
      in
      let num = int_of_string num in
      let ss = List.assoc num account.msn_xfr_requests in
      account.msn_xfr_requests <- List.remove_assoc num account.msn_xfr_requests;
      
      lprintf "SB request found"; lprint_newline ();
      
      let port = int_of_string port in
      let ip = Ip.from_name name in
      lprintf "XFR connect to [%s] %d" (Ip.to_string ip) port;
      lprint_newline ();      
      
      begin
        try
          
          let ss_old = List.assoc (ip, port) account.msn_switches in
          lprintf "ALREADY CONNECTED TO THE SWITCHBOARD";
          raise Not_found (* For now, connect several times *)
        with _ ->
            msn_ss_connect account ss ip port auth
      end  
      
  | "USR" :: _ ::  "MD5" :: _ :: friend :: _ ->
      let s = friend ^ account.account_password in
      lprintf "pass: [%s]" s; lprint_newline ();
      let md5 = String.lowercase (Md5.to_string (Md5.string s)) in
      write_string sock (
        Printf.sprintf "USR %d MD5 S %s\r\n" account.tryId md5);
      incr_tryId account
  
  | "USR" :: _ :: "OK" :: _ ->      
      write_string sock (
        Printf.sprintf "SYN %d 0\r\n" account.tryId);
      incr_tryId account

  | "LST" :: _ :: "RL" :: _ ->
      write_string sock (
        Printf.sprintf "CHG %d NLN\r\n" account.tryId);
      incr_tryId account
      
  | "BLP" :: _ ->
      ()
      
  | ("PRP" | "GTC" | "QNG" | "QRY" | "REM") :: _ -> (* UNUSED MESSAGES *)
      ()
      
  | opcode :: _ ->
      lprintf "UNKNOWN OPCODE                            [%s]" opcode; 
      lprint_newline ();
      match opcode.[0] with
        '0' .. '9' ->
          lprintf "ERROR %s" (msn_error (int_of_string opcode));
          lprint_newline ();
      | _ ->
          lprintf "UNKNOWN MESSAGE"; lprint_newline ();
          ()

and msn_reader msn_parser sock nread = 
  try
    lprintf "server to client %d" nread; 
    lprint_newline ();
    let b = TcpBufferedSocket.buf sock in
    LittleEndian.dump (String.sub b.buf b.pos b.len);
    lprint_newline ();
    
    let rec iter pos max_pos =
      if pos < max_pos - 1 then
        if b.buf.[pos] = '\r' && b.buf.[pos+1] = '\n' then

          let tokens = String2.split_simplify (
              String.sub b.buf b.pos (pos - b.pos)) ' ' in
          
          match tokens with
            [] ->
              TcpBufferedSocket.buf_used sock (pos-b.pos+2);
              (if b.len > 0 then iter b.pos (b.pos + b.len))

          | "MSG" :: _ ->
              begin              
                match List.rev tokens with
                  len :: _ ->
                    let len = int_of_string len in
                    lprintf "MUST WAIT: %d" len; lprint_newline ();
                    if b.len - pos - 2 >= len then begin
                        lprintf "OK !!!!!!"; lprint_newline ();
                        TcpBufferedSocket.buf_used sock (pos-b.pos+2);
                        let msg = String.sub b.buf b.pos len in
                        TcpBufferedSocket.buf_used sock len;
                        lprintf "MSG:"; lprint_newline ();
                        LittleEndian.dump msg;
                        lprint_newline ();
                        msn_parser  sock tokens msg; 
                        
                        (if not (closed sock) &&
                            b.len > 0 then iter b.pos (b.pos + b.len))
                    
                      end
                | _ -> failwith "BAD MSG"
              end
          | opcode :: args ->
              TcpBufferedSocket.buf_used sock (pos-b.pos+2);
          
              msn_parser  sock tokens ""; 
              (if not (closed sock) && b.len > 0 then
                  iter b.pos (b.pos + b.len))
          
        else
          iter (pos+1) max_pos
    in
    iter (maxi (b.pos + b.len - nread - 1) b.pos) (b.pos + b.len)
  with e ->
      close sock "Exception"

and msn_ns_connect account =
  match account.ns_sock with
    Some sock -> () (* already connected *)
  | None ->
      lprintf "connecting to Notification Server"; lprint_newline ();
      let sock = TcpBufferedSocket.connect "im to ns" 
          (Ip.to_inet_addr account.ns_ip) 
        account.ns_port 
          (msn_ns_handler account) in
      account.ns_sock <- Some sock;
      set_reader sock (msn_reader (msn_parser account)); 
      write_string sock (Printf.sprintf "VER %d MSNP5\r\n" account.tryId);
      incr_tryId account

and msn_ss_connect account ss ip port auth =
  lprintf "connecting to SwitchBoard Server"; lprint_newline ();
  let sock = TcpBufferedSocket.connect "im to ns" 
    (Ip.to_inet_addr ip) 
    port 
      (msn_ss_handler account ss) in
  account.msn_switches <- ((ip,port) , ss) :: account.msn_switches;
  set_reader sock (msn_reader (msn_ss_parser account ss)); 
  ss.ss_sock <- Some sock;
  write_string sock (Printf.sprintf "USR %d %s %s\r\n" account.tryId
    account.account_login  auth);
  incr_tryId account

(*
  ascii: [ V E R   0   M S N P 5(13)(10)]
*)
  
let msn_login account =
  match account.msn_sock with
    Some sock -> () (* already connected *)
  | None ->
      lprintf "connecting to msn"; lprint_newline ();
      let ip = Ip.from_name msn_server in
      let sock = TcpBufferedSocket.connect "im to msn" 
          (Ip.to_inet_addr ip) 
        msn_port 
          (msn_ms_handler account) in
      account.msn_sock <- Some sock;
      set_reader sock (msn_reader (msn_parser account)); 
      write_string sock (Printf.sprintf "VER %d MSNP5\r\n" account.tryId);
      incr_tryId account
  
let msn_remove_buddy who group  = ()
let msn_add_buddy who group = ()
  
let msn_keepalive account = 
  write_string (get_ns_sock account) "PNG\r\n"
  
let msn_set_idle idle = ()

let msn_send account who msg = 
(* This is done in two steps *)
  
  try
    List.iter (fun (_, ss) ->
        if List.mem who ss.ss_users then
          match ss.ss_sock with
            Some sock when  ss.ss_messages = [] ->
              msn_really_send account sock msg;
              raise Exit
          | _ ->
              lprintf "ADDING MESSAGE"; lprint_newline ();
              ss.ss_messages <- ss.ss_messages @ [who, msg];
              raise Exit
    ) account.msn_switches;
    let ss = {
        ss_sock = None;
        ss_users = [who];
        ss_messages = [];      
      } in
    account.msn_xfr_requests <- (account.tryId, ss) :: account.msn_xfr_requests;
    ss.ss_messages <- ss.ss_messages @ [who, msg];
    write_string (get_ns_sock account)
    (Printf.sprintf "XFR %d SB\r\n" account.tryId);
    incr_tryId account;
  with Exit -> ()
  
(*
let protocol = {
    im_name = "MSN";
    im_login = msn_login;
    im_remove_buddy = msn_remove_buddy;
    im_add_buddy = msn_add_buddy;
    im_keepalive = msn_keepalive;
    im_set_idle = msn_set_idle;
    im_send = msn_send;
  }
  *)

  

(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)

let new_account_val () =
  let rec impl = {
      impl_account_ops = account_ops;
      impl_account_val = account;
      impl_account_num = 0;
      impl_account_status = Status_offline;
    } and 
    account = {
      account_login = "";
      account_password = "";
      account_sock = None;
      account_account = impl;
      account_autologin = false;
      tryId = 0;
      msn_sock = None;
      msn_switches = [];
      msn_xfr_requests = [];
      ns_sock = None;
      ns_ip = Ip.null;
      ns_port = 1863;      
    } in
  account
  
let register_account account =
  let impl = account.account_account in
  update_account_num impl;
  accounts =:= (ImAccount.as_account impl) :: !!accounts

let account_record a =
  [
    "account_login", "Nick", false,
    FromString (fun s ->  a.account_login <- s),
    ToString (fun _ ->  a.account_login);
   
    "account_password", "Password", false,
    FromString (fun s -> a.account_password <- s),
    ToString (fun _ -> a.account_password);
    
    "auto_login", "Auto Login", true,
    FromBool (fun b -> a.account_autologin <- b),
    ToBool (fun _ -> a.account_autologin);    
  ]
  
let _ =
  protocol_ops.op_protocol_account_from_option <- (fun p assocs -> 
      let account = new_account_val () in
      from_record (account_record account) assocs;
      register_account account;
      if account.account_autologin then (try msn_login account with _ -> ());
      as_account account
  );
  account_ops.op_account_to_option <- (fun account -> 
      to_record (account_record account)
  );
  protocol_ops.op_protocol_new_account <- (fun p ->
      let account = new_account_val () in
      register_account account;
      as_account account);
  account_ops.op_account_keepalive <- msn_keepalive;
  account_ops.op_account_name <- (fun account -> account.account_login);
  account_ops.op_account_config_record <- (fun a -> account_record a)
