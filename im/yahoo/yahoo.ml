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

(* Translated from sources of Gaim *)

open Options
open Md4
open BigEndian
open BasicSocket
open TcpBufferedSocket

open ImChat
open ImTypes
open ImProtocol
open ImAccount
open ImOptions  
open ImEvent
open ImIdentity
  
let yahoo_servers = [ "cs.yahoo.com"; "scs.yahoo.com" ]
let yahoo_server =  "scs.yahoo.com"
(* let yahoo_server =  "127.0.0.1" for testing purposes *)
let yahoo_port = 5050
  
  
  
type yahoo_service =
| YAHOO_SERVICE_LOGON
| YAHOO_SERVICE_LOGOFF
| YAHOO_SERVICE_ISAWAY
| YAHOO_SERVICE_ISBACK
| YAHOO_SERVICE_IDLE
| YAHOO_SERVICE_MESSAGE
| YAHOO_SERVICE_IDACT
| YAHOO_SERVICE_IDDEACT
| YAHOO_SERVICE_MAILSTAT
| YAHOO_SERVICE_USERSTAT
| YAHOO_SERVICE_NEWMAIL
| YAHOO_SERVICE_CHATINVITE
| YAHOO_SERVICE_CALENDAR
| YAHOO_SERVICE_NEWPERSONALMAIL
| YAHOO_SERVICE_NEWCONTACT
| YAHOO_SERVICE_ADDIDENT
| YAHOO_SERVICE_ADDIGNORE
| YAHOO_SERVICE_PING
| YAHOO_SERVICE_GROUPRENAME
| YAHOO_SERVICE_SYSMESSAGE
| YAHOO_SERVICE_PASSTHROUGH2
| YAHOO_SERVICE_CONFINVITE
| YAHOO_SERVICE_CONFLOGON
| YAHOO_SERVICE_CONFDECLINE
| YAHOO_SERVICE_CONFLOGOFF
| YAHOO_SERVICE_CONFADDINVITE
| YAHOO_SERVICE_CONFMSG
| YAHOO_SERVICE_CHATLOGON
| YAHOO_SERVICE_CHATLOGOFF
| YAHOO_SERVICE_CHATMSG
| YAHOO_SERVICE_GAMELOGON
| YAHOO_SERVICE_GAMELOGOFF
| YAHOO_SERVICE_GAMEMSG
| YAHOO_SERVICE_FILETRANSFER
| YAHOO_SERVICE_NOTIFY
| YAHOO_SERVICE_AUTHRESP
| YAHOO_SERVICE_LIST
| YAHOO_SERVICE_AUTH
| YAHOO_SERVICE_ADDBUDDY
| YAHOO_SERVICE_REMBUDDY
| YAHOO_SERVICE_UNKNOWN of int

      
type yahoo_status =
| YAHOO_STATUS_AVAILABLE
| YAHOO_STATUS_BRB
| YAHOO_STATUS_BUSY
| YAHOO_STATUS_NOTATHOME
| YAHOO_STATUS_NOTATDESK
| YAHOO_STATUS_NOTINOFFICE
| YAHOO_STATUS_ONPHONE
| YAHOO_STATUS_ONVACATION
| YAHOO_STATUS_OUTTOLUNCH
| YAHOO_STATUS_STEPPEDOUT
| YAHOO_STATUS_INVISIBLE
| YAHOO_STATUS_CUSTOM
| YAHOO_STATUS_IDLE
| YAHOO_STATUS_OFFLINE
| YAHOO_STATUS_TYPING
| YAHOO_STATUS_UNKNOWN of int


(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)

type account = {
    mutable account_account : account account_impl;

    mutable account_sock : TcpBufferedSocket.t option;
    mutable account_password : string;
    mutable account_login : string;
    mutable account_friends : (string, identity) Hashtbl.t;
    mutable account_autologin : bool;
  }

and identity = {
    mutable identity_identity : identity identity_impl;

    mutable identity_login : string; 
    mutable identity_account : account;
    mutable identity_chat : chat option;
  }

and chat = {
    mutable chat_chat : chat chat_impl;
    
    mutable chat_friends : identity list;
    mutable chat_account : account;
  }
  
let protocol = ImProtocol.new_protocol "Yahoo" ()
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
let as_identity a = as_identity a.identity_identity    
let as_chat a = as_chat a.chat_chat

  

(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)

  
  
  
let string_of_service s =
  match s with
| YAHOO_SERVICE_LOGON -> "YAHOO_SERVICE_LOGON"
| YAHOO_SERVICE_LOGOFF -> "YAHOO_SERVICE_LOGOFF"
| YAHOO_SERVICE_ISAWAY -> "YAHOO_SERVICE_ISAWAY"
| YAHOO_SERVICE_ISBACK -> "YAHOO_SERVICE_ISBACK"
| YAHOO_SERVICE_IDLE -> "YAHOO_SERVICE_IDLE"
| YAHOO_SERVICE_MESSAGE -> "YAHOO_SERVICE_MESSAGE"
| YAHOO_SERVICE_IDACT -> "YAHOO_SERVICE_IDACT"
| YAHOO_SERVICE_IDDEACT -> "YAHOO_SERVICE_IDDEACT"
| YAHOO_SERVICE_MAILSTAT -> "YAHOO_SERVICE_MAILSTAT"
| YAHOO_SERVICE_USERSTAT -> "YAHOO_SERVICE_USERSTAT"
| YAHOO_SERVICE_NEWMAIL -> "YAHOO_SERVICE_NEWMAIL"
| YAHOO_SERVICE_CHATINVITE -> "YAHOO_SERVICE_CHATINVITE"
| YAHOO_SERVICE_CALENDAR -> "YAHOO_SERVICE_CALENDAR"
| YAHOO_SERVICE_NEWPERSONALMAIL -> "YAHOO_SERVICE_NEWPERSONALMAIL"
| YAHOO_SERVICE_NEWCONTACT -> "YAHOO_SERVICE_NEWCONTACT"
| YAHOO_SERVICE_ADDIDENT -> "YAHOO_SERVICE_ADDIDENT"
| YAHOO_SERVICE_ADDIGNORE -> "YAHOO_SERVICE_ADDIGNORE"
| YAHOO_SERVICE_PING -> "YAHOO_SERVICE_PING"
| YAHOO_SERVICE_GROUPRENAME -> "YAHOO_SERVICE_GROUPRENAME"
| YAHOO_SERVICE_SYSMESSAGE -> "YAHOO_SERVICE_SYSMESSAGE"
| YAHOO_SERVICE_PASSTHROUGH2 -> "YAHOO_SERVICE_PASSTHROUGH2"
| YAHOO_SERVICE_CONFINVITE -> "YAHOO_SERVICE_CONFINVITE"
| YAHOO_SERVICE_CONFLOGON -> "YAHOO_SERVICE_CONFLOGON"
| YAHOO_SERVICE_CONFDECLINE -> "YAHOO_SERVICE_CONFDECLINE"
| YAHOO_SERVICE_CONFLOGOFF -> "YAHOO_SERVICE_CONFLOGOFF"
| YAHOO_SERVICE_CONFADDINVITE -> "YAHOO_SERVICE_CONFADDINVITE"
| YAHOO_SERVICE_CONFMSG -> "YAHOO_SERVICE_CONFMSG"
| YAHOO_SERVICE_CHATLOGON -> "YAHOO_SERVICE_CHATLOGON"
| YAHOO_SERVICE_CHATLOGOFF -> "YAHOO_SERVICE_CHATLOGOFF"
| YAHOO_SERVICE_CHATMSG -> "YAHOO_SERVICE_CHATMSG"
| YAHOO_SERVICE_GAMELOGON -> "YAHOO_SERVICE_GAMELOGON"
| YAHOO_SERVICE_GAMELOGOFF -> "YAHOO_SERVICE_GAMELOGOFF"
| YAHOO_SERVICE_GAMEMSG -> "YAHOO_SERVICE_GAMEMSG"
| YAHOO_SERVICE_FILETRANSFER -> "YAHOO_SERVICE_FILETRANSFER"
| YAHOO_SERVICE_NOTIFY -> "YAHOO_SERVICE_NOTIFY"
| YAHOO_SERVICE_AUTHRESP -> "YAHOO_SERVICE_AUTHRESP"
| YAHOO_SERVICE_LIST -> "YAHOO_SERVICE_LIST"
| YAHOO_SERVICE_AUTH -> "YAHOO_SERVICE_AUTH"
| YAHOO_SERVICE_ADDBUDDY -> "YAHOO_SERVICE_ADDBUDDY"
| YAHOO_SERVICE_REMBUDDY -> "YAHOO_SERVICE_REMBUDDY"
| YAHOO_SERVICE_UNKNOWN i -> Printf.sprintf "YAHOO_SERVICE_UNKNOWN %d" i
  
let int_of_service s = 
  match s with
  | YAHOO_SERVICE_LOGON -> 1
  | YAHOO_SERVICE_LOGOFF -> 2
  | YAHOO_SERVICE_ISAWAY -> 3
  | YAHOO_SERVICE_ISBACK -> 4
  | YAHOO_SERVICE_IDLE -> 5 (* place holder *)
  | YAHOO_SERVICE_MESSAGE -> 6
  | YAHOO_SERVICE_IDACT -> 7
  | YAHOO_SERVICE_IDDEACT -> 8
  | YAHOO_SERVICE_MAILSTAT -> 9
  | YAHOO_SERVICE_USERSTAT -> 10
  | YAHOO_SERVICE_NEWMAIL -> 11
  | YAHOO_SERVICE_CHATINVITE -> 12
  | YAHOO_SERVICE_CALENDAR -> 13
  | YAHOO_SERVICE_NEWPERSONALMAIL -> 14
  | YAHOO_SERVICE_NEWCONTACT -> 15
  | YAHOO_SERVICE_ADDIDENT -> 16 
  | YAHOO_SERVICE_ADDIGNORE -> 17
  | YAHOO_SERVICE_PING -> 18
  | YAHOO_SERVICE_GROUPRENAME -> 19
  | YAHOO_SERVICE_SYSMESSAGE -> 0x14
  | YAHOO_SERVICE_PASSTHROUGH2 -> 0x16
  | YAHOO_SERVICE_CONFINVITE -> 0x18
  | YAHOO_SERVICE_CONFLOGON -> 25
  | YAHOO_SERVICE_CONFDECLINE -> 26
  | YAHOO_SERVICE_CONFLOGOFF -> 27
  | YAHOO_SERVICE_CONFADDINVITE -> 28
  | YAHOO_SERVICE_CONFMSG -> 29
  | YAHOO_SERVICE_CHATLOGON -> 30
  | YAHOO_SERVICE_CHATLOGOFF -> 31
  | YAHOO_SERVICE_CHATMSG -> 0x20
  | YAHOO_SERVICE_GAMELOGON -> 0x28
  | YAHOO_SERVICE_GAMELOGOFF -> 0x29
  | YAHOO_SERVICE_GAMEMSG -> 0x2a
  | YAHOO_SERVICE_FILETRANSFER -> 0x46
  | YAHOO_SERVICE_NOTIFY -> 0x4B
  | YAHOO_SERVICE_AUTHRESP -> 0x54
  | YAHOO_SERVICE_LIST -> 0x55
  | YAHOO_SERVICE_AUTH -> 0x57
  | YAHOO_SERVICE_ADDBUDDY -> 0x83
  | YAHOO_SERVICE_REMBUDDY -> 0x84
  | YAHOO_SERVICE_UNKNOWN s -> s
  
let service_of_int s = 
  match s with
  | 1 -> YAHOO_SERVICE_LOGON
  | 2 -> YAHOO_SERVICE_LOGOFF
  | 3 -> YAHOO_SERVICE_ISAWAY
  | 4 -> YAHOO_SERVICE_ISBACK
  | 5 (* place holder *) -> YAHOO_SERVICE_IDLE
  | 6 -> YAHOO_SERVICE_MESSAGE
  | 7 -> YAHOO_SERVICE_IDACT
  | 8 -> YAHOO_SERVICE_IDDEACT
  | 9 -> YAHOO_SERVICE_MAILSTAT
  | 10 -> YAHOO_SERVICE_USERSTAT
  | 11 -> YAHOO_SERVICE_NEWMAIL
  | 12 -> YAHOO_SERVICE_CHATINVITE
  | 13 -> YAHOO_SERVICE_CALENDAR
  | 14 -> YAHOO_SERVICE_NEWPERSONALMAIL
  | 15 -> YAHOO_SERVICE_NEWCONTACT
  | 16  -> YAHOO_SERVICE_ADDIDENT
  | 17 -> YAHOO_SERVICE_ADDIGNORE
  | 18 -> YAHOO_SERVICE_PING
  | 19 -> YAHOO_SERVICE_GROUPRENAME
  | 0x14 -> YAHOO_SERVICE_SYSMESSAGE
  | 0x16 -> YAHOO_SERVICE_PASSTHROUGH2
  | 0x18 -> YAHOO_SERVICE_CONFINVITE
  | 25 -> YAHOO_SERVICE_CONFLOGON
  | 26 -> YAHOO_SERVICE_CONFDECLINE
  | 27 -> YAHOO_SERVICE_CONFLOGOFF
  | 28 -> YAHOO_SERVICE_CONFADDINVITE
  | 29 -> YAHOO_SERVICE_CONFMSG
  | 30 -> YAHOO_SERVICE_CHATLOGON
  | 31 -> YAHOO_SERVICE_CHATLOGOFF
  | 0x20 -> YAHOO_SERVICE_CHATMSG
  | 0x28 -> YAHOO_SERVICE_GAMELOGON
  | 0x29 -> YAHOO_SERVICE_GAMELOGOFF
  | 0x2a -> YAHOO_SERVICE_GAMEMSG
  | 0x46 -> YAHOO_SERVICE_FILETRANSFER
  | 0x4B -> YAHOO_SERVICE_NOTIFY
  | 0x54 -> YAHOO_SERVICE_AUTHRESP
  | 0x55 -> YAHOO_SERVICE_LIST
  | 0x57 -> YAHOO_SERVICE_AUTH
  | 0x83 -> YAHOO_SERVICE_ADDBUDDY
  | 0x84 -> YAHOO_SERVICE_REMBUDDY
  | s -> YAHOO_SERVICE_UNKNOWN s
  
let int_of_status s =
  match s with
  | YAHOO_STATUS_AVAILABLE -> 0
  | YAHOO_STATUS_BRB -> 1
  | YAHOO_STATUS_BUSY -> 2
  | YAHOO_STATUS_NOTATHOME -> 3
  | YAHOO_STATUS_NOTATDESK -> 4
  | YAHOO_STATUS_NOTINOFFICE -> 5
  | YAHOO_STATUS_ONPHONE -> 6
  | YAHOO_STATUS_ONVACATION -> 7
  | YAHOO_STATUS_OUTTOLUNCH -> 8
  | YAHOO_STATUS_STEPPEDOUT -> 9
  | YAHOO_STATUS_INVISIBLE -> 12
  | YAHOO_STATUS_CUSTOM -> 99
  | YAHOO_STATUS_IDLE -> 999
  | YAHOO_STATUS_OFFLINE -> 0x5a55aa56 
  | YAHOO_STATUS_TYPING -> 0x16
  | YAHOO_STATUS_UNKNOWN s -> s  
  
let status_of_int s =
  match s with
  | 0 -> YAHOO_STATUS_AVAILABLE
  | 1 -> YAHOO_STATUS_BRB
  | 2 -> YAHOO_STATUS_BUSY
  | 3 -> YAHOO_STATUS_NOTATHOME
  | 4 -> YAHOO_STATUS_NOTATDESK
  | 5 -> YAHOO_STATUS_NOTINOFFICE
  | 6 -> YAHOO_STATUS_ONPHONE
  | 7 -> YAHOO_STATUS_ONVACATION
  | 8 -> YAHOO_STATUS_OUTTOLUNCH
  | 9 -> YAHOO_STATUS_STEPPEDOUT
  | 12 -> YAHOO_STATUS_INVISIBLE
  | 99 -> YAHOO_STATUS_CUSTOM
  | 999 -> YAHOO_STATUS_IDLE
  | 0x5a55aa56  -> YAHOO_STATUS_OFFLINE
  | 0x16 -> YAHOO_STATUS_TYPING
  | s -> YAHOO_STATUS_UNKNOWN s

let string_of_status s =
  match s with
  | YAHOO_STATUS_BRB ->    "Be Right Back";
  | YAHOO_STATUS_BUSY ->    "Busy";
  | YAHOO_STATUS_NOTATHOME ->    "Not At Home";
  | YAHOO_STATUS_NOTATDESK ->    "Not At Desk";
  | YAHOO_STATUS_NOTINOFFICE ->    "Not In Office";
  | YAHOO_STATUS_ONPHONE ->    "On Phone";
  | YAHOO_STATUS_ONVACATION ->    "On Vacation";
  | YAHOO_STATUS_OUTTOLUNCH ->    "Out To Lunch";
  | YAHOO_STATUS_STEPPEDOUT ->    "Stepped Out";
  | YAHOO_STATUS_INVISIBLE ->    "Invisible";
  | _ ->	"Online"
      
      
      
(********************************************************************

           Authentification stuff

*********************************************************************)
      
      
      
let base64digits = 
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789._"

  
(* This is taken from Sylpheed by Hiroyuki Yamamoto. *)
  
let to_y64 s =
  let buf = Buffer.create 30 in
  let inlen = String.length s in
  let rec iter pos inlen =
    match inlen with
      0 -> Buffer.contents buf
    | 1 ->
        let in0 = get_int8 s pos in
        Buffer.add_char buf base64digits.[in0 lsr 2];
        let fragment = (in0 lsl 4) land 0x30 in
        Buffer.add_char buf base64digits.[fragment];
        Buffer.add_char buf '-';
        Buffer.add_char buf '-';
        Buffer.contents buf        
    | 2 ->
        let in0 = get_int8 s pos in
        let in1 = get_int8 s (pos+1) in

        Buffer.add_char buf base64digits.[in0 lsr 2];
        let fragment = ((in0 lsl 4) land 0x30) lor (in1 lsr 4) in
        Buffer.add_char buf base64digits.[fragment];
        Buffer.add_char buf base64digits.[(in1 lsl 2) land 0x3c];
        Buffer.add_char buf '-';
        Buffer.contents buf
    | _ ->
        let in0 = get_int8 s pos in
        let in1 = get_int8 s (pos+1) in
        let in2 = get_int8 s (pos+2) in
        Buffer.add_char buf (base64digits.[in0 lsr 2]);
        Buffer.add_char buf (base64digits.[
            ((in0 lsl 4) land 0x30) lor (in1 lsr 4)]);
        Buffer.add_char buf  (base64digits.[
            ((in1 lsl 2) land 0x3c) lor (in2 lsr 6)]);
        Buffer.add_char buf base64digits.[in2 land 0x3f];
        iter (pos+3) (inlen-3)
  in
  iter 0 inlen

external yahoo_crypt : string -> string -> string -> unit = "ml_yahoo_crypt"
  
  
let yahoo_process_auth account seed =
  let client_nick = account.account_login in
  
  let sv = get_int8 seed 15 in
  let sv = sv mod 8 in
  
  let password_hash = to_y64 (Md5.direct_to_string 
      (Md5.string account.account_password)) in
  
  let crypt_hash = String.create 100 in
  yahoo_crypt account.account_password "$1$_2S43d5f$" crypt_hash;
  let pos = String.index crypt_hash '\000' in
  let crypt_hash = String.sub crypt_hash 0 pos in
  let crypt_hash = to_y64 (Md5.direct_to_string (Md5.string crypt_hash)) in
  
  let get_seed pos = (get_int8 seed pos) mod 16 in

  let (hash_string_p, hash_string_c) =
    match sv with
    | 1 | 6 ->
        let checksum = seed.[get_seed 9] in
        Printf.sprintf "%c%s%s%s" checksum client_nick seed password_hash,
        Printf.sprintf "%c%s%s%s" checksum client_nick seed crypt_hash
    | 2 | 7 ->
        let checksum = seed.[get_seed 15] in
        Printf.sprintf "%c%s%s%s" checksum seed password_hash client_nick,
        Printf.sprintf "%c%s%s%s" checksum seed crypt_hash client_nick
    | 3 ->
        let checksum = seed.[get_seed 1] in
        Printf.sprintf "%c%s%s%s" checksum client_nick password_hash seed,
        Printf.sprintf "%c%s%s%s" checksum client_nick crypt_hash seed
    | 4 ->
        let checksum = seed.[get_seed 3] in
        Printf.sprintf "%c%s%s%s" checksum password_hash seed client_nick,
        Printf.sprintf "%c%s%s%s" checksum crypt_hash seed client_nick
    | 0 | 5 ->
        let checksum = seed.[get_seed 7] in
        Printf.sprintf "%c%s%s%s" checksum password_hash client_nick seed,
        Printf.sprintf "%c%s%s%s" checksum crypt_hash client_nick seed
    | _ -> assert false
  in
    
  let result6 = to_y64 (Md5.direct_to_string (Md5.string hash_string_p)) in
  let result96 = to_y64 (Md5.direct_to_string (Md5.string hash_string_c)) in
    
  result6, result96
      
      
      
      
      
(* format of a Yahoo message:

int32: "YMSG"
int16: 0x0600
int16: 0x00
int16: pktlen (* payload *)
int16: service
int32: status
int32: id
char[pktlen]: payload

*)

type 'a packet = {
    service : yahoo_service;
    status : yahoo_status;
    id : int32;
    payload : 'a;
  }
      
let yahoo_header_len = 4 + 2 + 2 + 2 + 2 + 4 + 4
      
let cut_messages parse f sock nread =
  Printf.printf "server to client %d" nread; 
  print_newline ();
  let b = TcpBufferedSocket.buf sock in
  try
    while b.len >= yahoo_header_len do
      let msg_len = get_int16 b.buf (b.pos+8) in
      
      if b.len >= yahoo_header_len + msg_len then
        let header = String.sub b.buf b.pos yahoo_header_len in
(*        
        Printf.printf "NEW MESSAGE: header:"; print_newline ();
        LittleEndian.dump header;
        print_newline (); *)
        let service = get_int16 b.buf (b.pos+10) in
        let status = get_int b.buf (b.pos+12) in
        let id = get_int32 b.buf (b.pos+16) in       
        Printf.printf "server_to_client: one message"; 
        print_newline ();
        let s = String.sub b.buf (b.pos + yahoo_header_len) msg_len in
        let service = service_of_int service in
        Printf.printf "NEW MESSAGE: service %s status %x payload:" 
          (string_of_service service) status;
        print_newline ();
        (*
        LittleEndian.dump s;
        print_newline ();
*)
        
        let pkt = {
            service = service;
            status = status_of_int status;
            id = id;
            payload = s;
          } in
        TcpBufferedSocket.buf_used sock  (msg_len + yahoo_header_len);
        let t = parse pkt in
        f t sock
      else raise Not_found
    done
  with Not_found -> ()
  | e ->
      Printf.printf "EXCEPTION: %s" (Printexc2.to_string e); print_newline ();
      raise e
      
let buf = Buffer.create 30000
      
let send_message sock pkt = 
  Buffer.clear buf;
  Buffer.add_char buf 'Y';  
  Buffer.add_char buf 'M';  
  Buffer.add_char buf 'S';  
  Buffer.add_char buf 'G';  
  buf_int16 buf 0x0600;
  buf_int16 buf 0;
  buf_int16 buf (String.length pkt.payload);
  buf_int16 buf (int_of_service pkt.service);
  buf_int buf (int_of_status pkt.status);
  buf_int32 buf pkt.id;
  Buffer.add_string buf pkt.payload;
  let s = Buffer.contents buf in
  Buffer.clear buf;
  Printf.printf "SENDING MESSAGE:"; print_newline ();
  LittleEndian.dump s; print_newline ();
  write_string sock s;
  ()
  
let yahoo_send_message sock pkt = 
  Buffer.clear buf;
  List.iter (fun (key, data) ->
      Printf.bprintf buf "%d" key;
      buf_int8 buf 0xc0;
      buf_int8 buf 0x80;
      Buffer.add_string buf data;
      buf_int8 buf 0xc0;
      buf_int8 buf 0x80;
  ) pkt.payload;
  send_message sock { pkt with payload = Buffer.contents buf }
    

let new_packet0 service status payload =
  {
    service = service;
    status = status;
    id = Int32.zero;
    payload = payload;
  }

let rec cut_in_pairs list =
  match list with
    [] -> []
  | key :: _ :: value :: _ :: tail ->
      Printf.printf "[%s=%s]" key value; print_newline ();
      (int_of_string key, value) :: (cut_in_pairs tail)
  | _ -> []

  
let c128 = char_of_int 128
let c192 = char_of_int 192
  
let cut_pairs s =
  String2.replace_char s c128 c192;
  let list = String2.split s c192 in
  (*
  List.iter (fun s ->
      Printf.printf "item [%s]" s; print_newline ();
) list; 
  *)
  cut_in_pairs list
      
let yahoo_parser pkt = { pkt with
    payload = cut_pairs pkt.payload } 

      
      
      
      

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
      account_login = "NEW LOGIN";
      account_password = "NEW PASSWORD";
      account_sock = None;
      account_account = impl;
      account_autologin = false;
      account_friends = Hashtbl.create 13;
    } in
  account
  
let register_account account =
  let impl = account.account_account in
  update_account_num impl;
  accounts =:= (ImAccount.as_account impl) :: !!accounts

let new_chat_val account =
  let rec impl = {
      impl_chat_ops = chat_ops;
      impl_chat_val = chat;
      impl_chat_num = 0;
      impl_chat_account = as_account account;
    } and 
    chat = {
      chat_account = account;
      chat_friends = [];
      chat_chat = impl;
    } in
  chat
  
let register_chat chat =
  let impl = chat.chat_chat in
  update_chat_num impl

let new_identity_val account =
  let rec impl = {
      impl_identity_ops = identity_ops;
      impl_identity_val = identity;
      impl_identity_num = 0;
(*      impl_identity_status = Status_offline; *)
    } and 
    identity = {
      identity_login = "NEW FRIEND";
      identity_identity = impl;
      identity_account = account;
      identity_chat = None;
    } in
  identity
  
let register_identity identity =
  let impl = identity.identity_identity in
  Hashtbl.add identity.identity_account.account_friends 
    identity.identity_login identity;
  update_identity_num impl

let account_record a =
  [
    "account_login", "Nick", false,
    FromString (fun s ->  a.account_login <- s),
    ToString (fun _ -> a.account_login);
   
    "account_password", "Password", false,
    FromString (fun s -> a.account_password <- s),
    ToString (fun _ -> a.account_password);
    
    "auto_login", "Auto Login", true,
    FromBool (fun b -> a.account_autologin <- b),
    ToBool (fun _ -> a.account_autologin);    
  ]

let identity_record a =
  [
    "identity_login", "Nick", false,
    FromString (fun s ->  a.identity_login <- s),
    ToString (fun _ -> a.identity_login);
  ]
      
(*********************************************************************

                  More interesting functions
  
*********************************************************************)
  
let get_sock account =
  match account.account_sock with
    None -> failwith "NOT CONNECTED"
  | Some sock -> sock

let yahoo_handler account s event = 
  match event with
    BASIC_EVENT (CLOSED s) ->
      Printf.printf "disconnected from yahoo"; print_newline ();
      account.account_sock <- None;
      set_account_status (as_account account) Status_offline;
  | _ -> ()

(* NEW MESSAGE: service 2 status ffffffff payload: *)

let yahoo_process_status account pkt = 
  Printf.printf "NOT IMPLEMENTED: yahoo_process_status"; print_newline ();
  List.iter (fun (key, value) ->
      match key with
      | 1 -> 
          set_account_status (as_account account) 
          (Status_online Online_available);
          add_event (Account_event (as_account account));
      | _ -> ()
  ) pkt.payload
  
let yahoo_process_notify pkt = 
  Printf.printf "NOT IMPLEMENTED: yahoo_process_notify"; print_newline ();
  ()

let id_open_chat id =
  let chat = 
    match id.identity_chat with
      None ->
        let chat = new_chat_val id.identity_account in
        chat.chat_friends <- [id];
        id.identity_chat <- Some chat;
        register_chat chat;
        chat
    | Some chat -> chat
  in
  add_event (Chat_open_event (as_chat chat));
  chat
  
let yahoo_process_message account pkt = 
  let from = List.assoc 4 pkt.payload in
  let msg = List.assoc 14 pkt.payload in
(*  let tm = List.assoc 15 pkt.payload in *)
  
(*
        if (pkt->status <= 1 || pkt->status == 5) {
                char *m;
                int i, j;
                strip_linefeed(msg);
                m = msg;
                for (i = 0, j = 0; m[i]; i++) {
                        if (m[i] == 033) {
                                while (m[i] && (m[i] != 'm'))
                                        i++;
                                if (!m[i])
                                        i--;
                                continue;
                        }
                        msg[j++] = m[i];
                }
                msg[j] = 0;
                serv_got_im(gc, from, msg, 0, tm, -1);
        } else if (pkt->status == 2) {
                do_error_dialog(_("Your message did not get sent."), _("Gaim - Error"));
        }

  *)
  
  Printf.printf "MESSAGE FROM %s: %s" from msg; print_newline ();
  begin
(* Who sent the message *)
    let id = try
        Hashtbl.find account.account_friends from
      with _ ->
          let id = new_identity_val account in
          id.identity_login <- from;
          register_identity id;
          id
    in
(* On which chat ? *)
    let chat = id_open_chat id in
    add_event (Chat_message_event (as_chat chat, as_identity id, msg))
  end
  
let yahoo_process_mail pkt = 
  Printf.printf "NOT IMPLEMENTED: yahoo_process_mail"; print_newline ();
  ()
  
let yahoo_process_contact pkt = 
  Printf.printf "NOT IMPLEMENTED: yahoo_process_contact"; print_newline ();
  ()

let yahoo_add_friend_in_group account group name =
  Printf.printf "group: [%s] [%s]" group name; print_newline ();
  let id =
    try
      Hashtbl.find account.account_friends name
    with _ -> 
        let id = new_identity_val account in
        id.identity_login <- name;
        register_identity id;
        add_event (Account_friend_event (as_identity id));
        id
  in
  ()
  
let yahoo_process_list account  pkt = 
  List.iter (fun (key, value) ->
      if key = 87 then
        List.iter (fun line ->
            let (group, names) = String2.cut_at line ':' in
            let names = String2.split_simplify names ',' in
            List.iter (fun name ->
                yahoo_add_friend_in_group account group name;
            ) names
        ) (String2.split_simplify value '\n')
  ) pkt.payload;
  ()
      
let yahoo_reader account pkt sock = 
  Printf.printf "Message from Yahoo"; print_newline ();
  match pkt.service with
  | YAHOO_SERVICE_AUTH ->  
      let result6, result96 = yahoo_process_auth account
        (List.assoc 94 pkt.payload) in
      yahoo_send_message (get_sock account) (new_packet0 
          YAHOO_SERVICE_AUTHRESP YAHOO_STATUS_AVAILABLE
          [
          0, account.account_login;
          6, result6;
          96, result96;
          1, account.account_login
        ])
  | YAHOO_SERVICE_LOGON
  | YAHOO_SERVICE_LOGOFF
  | YAHOO_SERVICE_ISAWAY
  | YAHOO_SERVICE_ISBACK
  | YAHOO_SERVICE_GAMELOGON
  | YAHOO_SERVICE_GAMELOGOFF ->
      yahoo_process_status account pkt

  | YAHOO_SERVICE_NOTIFY ->
      yahoo_process_notify pkt
      
  | YAHOO_SERVICE_MESSAGE
  | YAHOO_SERVICE_GAMEMSG ->
    yahoo_process_message account pkt

  | YAHOO_SERVICE_NEWMAIL ->
    yahoo_process_mail pkt

  | YAHOO_SERVICE_NEWCONTACT ->
      yahoo_process_contact pkt
    
  | YAHOO_SERVICE_LIST ->
      yahoo_process_list account pkt

  | _ -> 
      Printf.printf "UNUSED MESSAGE"; print_newline ()
      
let yahoo_login account =
  match account.account_sock with
    Some sock -> () (* already connected *)
  | None ->
      Printf.printf "connecting to yahoo %s" account.account_login; print_newline ();
      set_account_status (as_account account) Status_connecting;
      let ip = Ip.from_name yahoo_server in
      
      let sock = TcpBufferedSocket.connect "im to yahoo" 
          (Ip.to_inet_addr ip) 
        yahoo_port 
          (yahoo_handler account) in
      account.account_sock <- Some sock;
      set_account_status (as_account account) Status_connecting;
      set_reader sock (cut_messages yahoo_parser (yahoo_reader account));
      yahoo_send_message sock (
        new_packet0 YAHOO_SERVICE_AUTH YAHOO_STATUS_AVAILABLE
          [
          1, account.account_login
        ]);
      Printf.printf "connecting to yahoo %s" account.account_login; print_newline ()
      
let yahoo_remove_buddy account who group =
  let pkt = {
      service = YAHOO_SERVICE_REMBUDDY;
      status = YAHOO_STATUS_AVAILABLE;
      id = Int32.zero;
      payload = [
        1, account.account_login;
        7, who;
        65, group;
      ];
    } in
  yahoo_send_message (get_sock account) pkt

(* We should try to find the group from "who" (gaim uses "Buddies" 
for default) *)
let yahoo_add_buddy account who group =
  let pkt = {
      service = YAHOO_SERVICE_ADDBUDDY;
      status = YAHOO_STATUS_AVAILABLE;
      id = Int32.zero;
      payload = [
        1, account.account_login;
        7, who;
        65, group;
      ];      
    } in
  yahoo_send_message (get_sock account) pkt
  
let yahoo_keepalive account =
  yahoo_send_message (get_sock account) (
    new_packet0 YAHOO_SERVICE_PING YAHOO_STATUS_AVAILABLE [])

(* We should probably check our current status *)
let yahoo_set_idle account idle = 
  yahoo_send_message (get_sock account) (if idle then
      new_packet0 YAHOO_SERVICE_ISAWAY YAHOO_STATUS_IDLE []
    else
      new_packet0 YAHOO_SERVICE_ISAWAY YAHOO_STATUS_AVAILABLE []
  )

let yahoo_send account who what =
  yahoo_send_message (get_sock account) (
    new_packet0 YAHOO_SERVICE_MESSAGE YAHOO_STATUS_OFFLINE 
      [
      1, account.account_login;
      5, who;
      14, what;
    ])

  
(* Used to activate a given identifier ?? *)
let yahoo_act_id account entry =
  yahoo_send_message (get_sock account) (
    new_packet0 YAHOO_SERVICE_IDACT YAHOO_STATUS_AVAILABLE
      [ 3, entry ])
  
let yahoo_send_typing account who typ = 
  yahoo_send_message (get_sock account) (
    new_packet0 YAHOO_SERVICE_NOTIFY YAHOO_STATUS_TYPING
    [
      49, "TYPING";
      1, account.account_login;
      14, " ";
      13, (if typ then "1" else "0");
      5, who;
      1002, "1"
    ])

(*
let protocol = {
    im_name = "Yahoo";
    im_login = yahoo_login;
    im_remove_buddy = yahoo_remove_buddy;
    im_add_buddy = yahoo_add_buddy;
    im_keepalive = yahoo_keepalive;
    im_set_idle = yahoo_set_idle;
    im_send = yahoo_send;
  }
  
  *)

let _ =
  protocol_ops.op_protocol_account_from_option <- (fun p assocs -> 
      let account = new_account_val () in
      from_record (account_record account) assocs;
      register_account account;
      if account.account_autologin then (try yahoo_login account with _ -> ());
      as_account account
  );
  account_ops.op_account_login <- (fun account ->
      (try yahoo_login account with _ -> ());
  );
  account_ops.op_account_to_option <- (fun account -> 
      to_record (account_record account)
  );
  protocol_ops.op_protocol_new_account <- (fun p ->
      let account = new_account_val () in
      register_account account;
      as_account account);
  account_ops.op_account_keepalive <- yahoo_keepalive;
  account_ops.op_account_name <- (fun account -> account.account_login);
  account_ops.op_account_config_record <- (fun a -> account_record a);
  account_ops.op_account_new_identity <- (fun account ->
      let identity = new_identity_val account in
      register_identity identity;
      as_identity identity);
  identity_ops.op_identity_config_record <- (fun id -> identity_record id);
  account_ops.op_account_contacts <- (fun account ->
      List.map as_identity (Hashtbl2.to_list account.account_friends));
  identity_ops.op_identity_name <- (fun id -> id.identity_login);
  chat_ops.op_chat_send <- (fun chat msg ->
      List.iter (fun id ->
          yahoo_send chat.chat_account id.identity_login msg
      ) chat.chat_friends;
      add_event (Chat_my_message (as_chat chat, msg))
  );
  chat_ops.op_chat_name <- (fun chat ->
      let s = ref "" in
      List.iter (fun f ->
          s := f.identity_login ^ " " ^ !s
      ) chat.chat_friends;
      !s
  );
  identity_ops.op_identity_open_chat <- (fun id ->
      ignore (id_open_chat id));
  chat_ops.op_chat_close <- (fun chat ->
      add_event (Chat_close_event (as_chat chat));
      List.iter (fun id -> 
          id.identity_chat <- None
      ) chat.chat_friends
  )

  
  (*
(************************************************************************)

(*          Fake a server (useful when you have no network ;) )         *)

(************************************************************************)
  
type client = {
    nick : string;
    mutable friends : client list;
    mutable status : status;
    mutable sock : TcpBufferedSocket.t option;
    mutable friend_of : client list;
  }

let clients = Hashtbl.create 13
  
let new_client name =
  try
    Hashtbl.find clients name
  with _ ->
      let client = {
          nick = name;
          friends = [];
          status = Status_offline;
          sock = None;
          friend_of = [];
        } in
      Hashtbl.add clients name client;
      client
  
let yahoo_client_reader c pkt sock = 
  match pkt.service with
  | YAHOO_SERVICE_AUTH ->  
      List.iter (fun (key, value) ->
          match key with
          | 1 -> 
              let client = new_client value in
              client.sock <- Some sock;
              client.status <- Status_online Online_available;
              c := Some client;
              
              yahoo_send_message sock (
                new_packet0 YAHOO_SERVICE_LOGON YAHOO_STATUS_AVAILABLE
                  [
                  1, value;
                ]);
              
              yahoo_send_message sock (
                new_packet0 YAHOO_SERVICE_LIST YAHOO_STATUS_AVAILABLE
                  [
                  1, value;
                  87, (let s = ref "Buddies:" in
                    List.iter (fun f ->
                        s := !s ^ f.nick ^ ","
                    ) client.friends;
                    !s^"\n")
                ]);
              
      | _ -> ()
  ) pkt.payload
      
  | YAHOO_SERVICE_LOGON
  | YAHOO_SERVICE_LOGOFF
  | YAHOO_SERVICE_ISAWAY
  | YAHOO_SERVICE_ISBACK
  | YAHOO_SERVICE_GAMELOGON
  | YAHOO_SERVICE_GAMELOGOFF ->
      ()

  | YAHOO_SERVICE_NOTIFY ->
      yahoo_process_notify pkt
      
  | YAHOO_SERVICE_MESSAGE
  | YAHOO_SERVICE_GAMEMSG ->
      begin
        let sender = ref "" in
        let receiver = ref "" in
        let message = ref "" in
          
        List.iter (fun (key, value) ->
            match key with
            | 1 -> sender := value
            | 5 -> receiver := value
            | 14 -> message := value
            | _ -> ()
        ) pkt.payload;
        
        let client = new_client !receiver in
        match client.sock with
          None ->
(* Should send a warning *) ()
        | Some sock ->

            yahoo_send_message sock (
              new_packet0 YAHOO_SERVICE_MESSAGE YAHOO_STATUS_OFFLINE 
                [
                4, !sender;
                14, !message;
              ])
            
        
      end

  | YAHOO_SERVICE_NEWMAIL ->
    yahoo_process_mail pkt

  | YAHOO_SERVICE_NEWCONTACT ->
      yahoo_process_contact pkt
    
  | YAHOO_SERVICE_LIST ->
      ()

  | _ -> 
      Printf.printf "UNUSED MESSAGE"; print_newline ()


let notify_status client friend =
  ()
      
let client_connection_handler t event =
  match event with
    TcpServerSocket.CONNECTION (s, Unix.ADDR_INET (from_ip, from_port)) ->
      Printf.printf "CONNECTION From Yahoo CLient !!!"; print_newline ();
      let c = ref None in
      let sock = 
        TcpBufferedSocket.create "yahoo client connection" s 
        (fun _ _ -> ()) 
        (*client_msg_to_string*)
      in
      set_reader sock (cut_messages yahoo_parser (yahoo_client_reader c));
      set_closer sock (fun _ _ ->
          match !c with
            None ->
              c := None;
          | Some client ->
              c := None;
              client.status <- Status_offline;
              client.sock <- None;
              List.iter (fun c ->
(* Notify new state *)
                  notify_status c client
              ) client.friend_of
      ) 
  | _ -> ()      

let add_friend c1 c2 =
  c1.friends <- c2 :: c1.friends;
  c2.friend_of <- c1 :: c2.friend_of;
  notify_status c1 c2
      
let _ =
  try
    let _ = Sys.getenv "YAHOO_SERVER" in
    Printf.printf "STARTING YAHOO SERVER"; print_newline ();
    let sock = TcpServerSocket.create 
      "yahoo client server" Unix.inet_addr_any
        5050 client_connection_handler in
    Printf.printf "Server binded on port 5050"; print_newline ();
    ()
  with _ -> ()
      
let _ =
(* Initial state of server *)
  let b8_cro = new_client "b8_cro" in
  let b8_bavard = new_client "b8_bavard" in
  add_friend b8_cro b8_bavard;
  add_friend b8_bavard b8_cro;
  ()
  *)
