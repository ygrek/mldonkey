(* Copyright 2001, 2002 sy23, b8_fee_carabine, INRIA *)
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
open ImRoom

(*
nickname: char[9]

client: nickname, hostname, login, server

channel[200]: start with &#, no space, CTRL-G or ,
{}| = lowercase([]\ )

space separated
[:prefix] command args\r\n

prefix: severname | nick [!user] [@host]
  
args: - no space
      - ':' trailing with spaces


IRC C: JOIN #francais 
IRC S: :sy23!~simon@d177.dhcp212-198-64.noos.fr JOIN :#francais
event_chat_join: sy23 1 #francais
IRC S: :brunner.freenode.net MODE #francais +n
IRC S: :brunner.freenode.net 353 sy23 = #francais :@sy23
event_chat_buddy_join: sy23 1 @sy23
IRC S: :brunner.freenode.net 366 sy23 #francais :End of /NAMES list.
IRC C: JOIN #mldonkey 
IRC S: :sy23!~simon@d177.dhcp212-198-64.noos.fr JOIN :#mldonkey
event_chat_join: sy23 2 #mldonkey
IRC S: :brunner.freenode.net 332 sy23 #mldonkey :..:: Welcome to the best sharing client for linux: MLDONKEY for the edonkey and overnet network -- please visit: 
http://www.mldonkeyworld.com for more info ::..
IRC S: :brunner.freenode.net 333 sy23 #mldonkey lemmy 1039911115
IRC S: :brunner.freenode.net 353 sy23 = #mldonkey :sy23 mobydick_ butze theos nitram_ ehrgeiz rgselk tristate pokey pango_ radenko calc mulligan_ tommie vnc|
zzz Pucmel lemmy Vaste
event_chat_buddy_join: sy23 2 sy23
event_chat_buddy_join: sy23 2 mobydick_
event_chat_buddy_join: sy23 2 butze
event_chat_buddy_join: sy23 2 theos
event_chat_buddy_join: sy23 2 nitram_
event_chat_buddy_join: sy23 2 ehrgeiz
event_chat_buddy_join: sy23 2 rgselk
event_chat_buddy_join: sy23 2 tristate
event_chat_buddy_join: sy23 2 pokey
event_chat_buddy_join: sy23 2 pango_
event_chat_buddy_join: sy23 2 radenko
event_chat_buddy_join: sy23 2 calc
event_chat_buddy_join: sy23 2 mulligan_
event_chat_buddy_join: sy23 2 tommie
event_chat_buddy_join: sy23 2 vnc|zzz
event_chat_buddy_join: sy23 2 Pucmel
event_chat_buddy_join: sy23 2 lemmy
event_chat_buddy_join: sy23 2 Vaste
IRC S: :brunner.freenode.net 366 sy23 #mldonkey :End of /NAMES list.
event_chat_send: sy23 2 just checking.... bye all
IRC C: PRIVMSG #mldonkey :just checking.... bye all
event_chat_recv: sy23 2 sy23
chat clicked close button
conversation close callback
IRC C: PART #mldonkey
IRC S: :sy23!~simon@d177.dhcp212-198-64.noos.fr PART #mldonkey :
event_chat_leave: sy23 2
Leaving room #mldonkey.
chat clicked close button
conversation close callback
IRC C: PART #francais
IRC S: :sy23!~simon@d177.dhcp212-198-64.noos.fr PART #francais :
event_chat_leave: sy23 1
Leaving room #francais.
date: Mon Jan  6 13:56:53 2003
event_signoff: sy23
removing NOP
IRC C: QUIT :Download Gaim [http://gaim.sourceforge.net/]


  *)
  
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
    mutable account_login : string;
    mutable account_server : string;
    mutable account_port : int;
    mutable account_identities : (string, identity) Hashtbl.t;
    mutable account_friends : identity list;
    mutable account_autologin : bool;
    
    mutable account_rooms : (string, room) Hashtbl.t;
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

and room = {
    mutable room_room :  room room_impl;

    mutable room_name : string; 
    mutable room_account : account;
  }
  
let protocol = ImProtocol.new_protocol "IRC" ()
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
      account_sock = None;
      account_account = impl;
      account_server = "irc.openprojects.net";
      account_port = 6667;
      account_autologin = false;
      account_friends = [];
      account_identities = Hashtbl.create 13;
      account_rooms = Hashtbl.create 13;
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

let new_room_val account name =
  let rec impl = {
      impl_room_ops = room_ops;
      impl_room_val = room;
      impl_room_num = 0;
      impl_room_account = as_account account;
    } and 
    room = {
      room_name = name;
      room_account = account;
      room_room = impl;
    } in
  room
  
let register_room room =
  let impl = room.room_room in
  update_room_num impl

let new_identity_val account =
  let rec impl = {
      impl_identity_ops = identity_ops;
      impl_identity_val = identity;
      impl_identity_num = 0;
      impl_identity_account = as_account account;
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
  Hashtbl.add identity.identity_account.account_identities 
    identity.identity_login identity;
  update_identity_num impl

let account_record a =
  [
    "account_login", "Nick", false,
    FromString (fun s ->  a.account_login <- s),
    ToString (fun _ -> a.account_login);
   
    "account_server", "Server", false,
    FromString (fun s -> a.account_server <- s),
    ToString (fun _ -> a.account_server);
   
    "account_port", "Server Port", false,
    FromInt (fun s -> a.account_port <- s),
    ToInt (fun _ -> a.account_port);
    
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
  
(*********************************************************************

                  More interesting functions
  
*********************************************************************)
  
let get_sock account =
  match account.account_sock with
    None -> failwith "NOT CONNECTED"
  | Some sock -> sock

let irc_handler account s event = 
  match event with
    BASIC_EVENT (CLOSED s) ->
      Printf.printf "disconnected from irc"; print_newline ();
      account.account_sock <- None;
      set_account_status (as_account account) Status_offline;
  | _ -> ()

(* NEW MESSAGE: service 2 status ffffffff payload: *)
(*
let irc_process_status account pkt = 
  Printf.printf "NOT IMPLEMENTED: irc_process_status"; print_newline ();
  List.iter (fun (key, value) ->
      match key with
      | 1 -> 
          set_account_status (as_account account) 
          (Status_online Online_available);
          add_event (Account_event (as_account account));
      | _ -> ()
  ) pkt.payload
  
let irc_process_notify pkt = 
  Printf.printf "NOT IMPLEMENTED: irc_process_notify"; print_newline ();
  ()
  
let irc_process_message account pkt = 
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
  
let irc_process_mail pkt = 
  Printf.printf "NOT IMPLEMENTED: irc_process_mail"; print_newline ();
  ()
  
let irc_process_contact pkt = 
  Printf.printf "NOT IMPLEMENTED: irc_process_contact"; print_newline ();
  ()
  *)

let new_identity account name =
  let id =
    try
      Hashtbl.find account.account_identities name
    with _ -> 
        let id = new_identity_val account in
        id.identity_login <- name;
        register_identity id;
        id
  in
  id

(*
let irc_process_list account  pkt = 
  List.iter (fun (key, value) ->
      if key = 87 then
        List.iter (fun line ->
            let (group, names) = String2.cut_at line ':' in
            let names = String2.split_simplify names ',' in
            List.iter (fun name ->
                irc_add_friend_in_group account group name;
            ) names
        ) (String2.split_simplify value '\n')
  ) pkt.payload;
  ()
      
let irc_reader account pkt sock = 
  Printf.printf "Message from Yahoo"; print_newline ();
  match pkt.service with
  | YAHOO_SERVICE_AUTH ->  
      let result6, result96 = irc_process_auth account
        (List.assoc 94 pkt.payload) in
      irc_send_message (get_sock account) (new_packet0 
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
      irc_process_status account pkt

  | YAHOO_SERVICE_NOTIFY ->
      irc_process_notify pkt
      
  | YAHOO_SERVICE_MESSAGE
  | YAHOO_SERVICE_GAMEMSG ->
    irc_process_message account pkt

  | YAHOO_SERVICE_NEWMAIL ->
    irc_process_mail pkt

  | YAHOO_SERVICE_NEWCONTACT ->
      irc_process_contact pkt
    
  | YAHOO_SERVICE_LIST ->
      irc_process_list account pkt

  | _ -> 
      Printf.printf "UNUSED MESSAGE"; print_newline ()
*)

let verbose = ref true

let rec get_arg s pos len =
  if pos >= len then raise Not_found;
  if s.[pos] = ':' then String.sub s (pos+1) (len-pos-1), len else
  if s.[pos] = ' ' then get_arg s (pos+1) len else
  try
    let new_pos = String.index_from s pos ' ' in
    String.sub s pos (new_pos - pos), new_pos
  with _ -> String.sub s (pos+1) (len-pos-1), len
    
    
  
let rec get_args s pos len =
  try
    let (arg, pos) = get_arg s pos len in
    arg :: (get_args s pos len)
  with _ -> []
      
let irc_parser s = 
  let len = String.length s in
  
  let prefix, pos = 
    if len>0 && s.[0] = ':' then
      let pos = String.index_from s 1 ' ' in
      String.sub s 1 (pos-1), pos
    else "", 0
  in
  let cmd, pos = get_arg s pos len in
  let args = get_args s pos len in
  
  prefix, cmd, args

let parse_prefix prefix =
  try
    let nick, _ = String2.cut_at prefix '!' in
    nick
  with _ -> 
      try
        let nick, _ = String2.cut_at prefix '@' in
        nick
      with _ ->   prefix

let find_room account room_name = 
  try
    Printf.printf "FIND ROOM [%s]" room_name ; print_newline ();
    Hashtbl.find account.account_rooms room_name
  with _ ->
      let room = new_room_val account room_name in
      register_room room;
      Hashtbl.add account.account_rooms room_name room;
      add_event (Room_join (as_room room.room_room));
      room
      
          
let irc_reader account (prefix, command, args) sock = 
  begin  
    match account_status (as_account account) with
      Status_offline | Status_connecting ->
        set_account_status (as_account account) 
        (Status_online Online_available);
        add_event (Account_event (as_account account));
    | _ -> ()
  end;
  match command, args with
    "PING", arg :: _ -> write_string sock (Printf.sprintf "PONG %s" arg)
  
  | "JOIN", room_name :: _ ->   
      begin      
        let nick = parse_prefix prefix in
        let room = find_room account room_name in
        (*
        if nick = account.account_login then
add_event (Room_join (as_room room.room_room))
        else *)
        let id = new_identity account nick in
        add_event (Room_user_join (as_room room.room_room, 
            as_identity id))
      end      
  
  | "353", my_nick :: _ :: room_name :: names ->
      let room = find_room account room_name in
      
      List.iter (fun nick ->
          if nick <> account.account_login then
            let id = new_identity account nick in
            add_event (Room_user_join (as_room room.room_room, 
                as_identity id))
      ) names

  | "332", my_nick :: room_name :: motd :: _ ->
      let room = find_room account room_name in
      add_event (Room_public_message (as_room room.room_room, 
                motd))
      
      
      (*
	case 4:
		if (!strncmp(word[5], "u2.10", 5))
			id->six_modes = TRUE;
		else
			id->six_modes = FALSE;
		break;
	case 5:
		handle_005(gc, word, word_eol);
		break;
	case 301:
		if (id->in_whois) {
			id->liststr = g_string_append(id->liststr, "<BR><b>Away: </b>");

			if (word_eol[5][0] == ':')
				id->liststr = g_string_append(id->liststr, word_eol[5] + 1);
			else
				id->liststr = g_string_append(id->liststr, word_eol[5]);
		} else
			irc_got_im(gc, word[4], word_eol[5], IM_FLAG_AWAY, time(NULL));
		break;
	case 303:
		handle_list(gc, &word_eol[4][1]);
		break;
	case 311:
	case 312:
	case 313:
	case 317:
	case 319:
		handle_whois(gc, word, word_eol, n);
		break;
	case 322:
		handle_roomlist(gc, word, word_eol);
		break;
	case 323:
	case 318:
		if ((id->in_whois || id->in_list) && id->liststr) {
			GString *str = decode_html(id->liststr->str);
			g_show_info_text(gc, NULL, 2, str->str, NULL);
			g_string_free(str, TRUE);
			g_string_free(id->liststr, TRUE);
			id->liststr = NULL;
			id->in_whois = FALSE;
			id->in_list = FALSE;
		}
		break;
	case 324:
		handle_mode(gc, word, word_eol, TRUE);
		break;
	case 332:
		handle_topic(gc, text);
		break;
	case 353:
		handle_names(gc, word[5], word_eol[6]);
		break;
	case 376:
		irc_request_buddy_update(gc);
		break;
	case 401:
		do_error_dialog(_("No such nick/channel"), _("IRC Error"));
		break;
	case 402:
		do_error_dialog(_("No such server"), _("IRC Error"));
	case 431:
		do_error_dialog(_("No nickname given"), _("IRC Error"));
		break;
	}
*)


  | _ -> 
      Printf.printf "UNUSED MESSAGE"; print_newline ()
      
let cut_messages parser reader sock nread =
    if !verbose then begin
        Printf.printf "server to client: read %d" nread; 
        print_newline ();
      end;
    
    let b = TcpBufferedSocket.buf sock in
    let offset = if nread < b.len then 1 else 0 in
    let nread = nread + offset in
    let pos = b.pos + b.len - nread in
    let rec iter pos nread =
      if nread > 1 then
        if b.buf.[pos] = '\r' && b.buf.[pos+1] = '\n' then begin
            
            if !verbose then begin
                Printf.printf "server_to_client: complete message"; 
                print_newline ();
              end;

            let s = String.sub b.buf b.pos (pos-b.pos) in
            let used = pos - b.pos + 2 in
            buf_used sock used;
            
            if !verbose then begin
                Printf.printf "Message: %s" s; 
                print_newline ();
              end;
            
            (try reader (parser s) sock with e -> 
                  Printf.printf "Exception %s in Irc.cut_messages"
                    (Printexc2.to_string e); print_newline ();
            );

            if not (closed sock) then
              iter b.pos b.len
            
          end else
          iter (pos+1) (nread-1)
    in
    iter pos nread
            

let irc_login account =
  match account.account_sock with
    Some sock -> () (* already connected *)
  | None ->
      Printf.printf "connecting to irc %s" account.account_login; 
      print_newline ();
      set_account_status (as_account account) Status_connecting;

      try
      let sock = TcpBufferedSocket.connect "im to irc" 
          (Ip.to_inet_addr (Ip.from_name account.account_server)) 
        account.account_port 
          (irc_handler account) in
      account.account_sock <- Some sock;
      set_account_status (as_account account) Status_connecting;
      set_reader sock (cut_messages irc_parser (irc_reader account));
      write_string sock (Printf.sprintf "NICK %s\r\n" 
        account.account_login);
      write_string sock (Printf.sprintf "USER %s %s %s :mlim(%s)\r\n" 
          account.account_login (Unix.gethostname()) 
        account.account_server account.account_login);

      Printf.printf "connecting to irc %s" account.account_login; print_newline ()

      with e ->
          Printf.printf "Exception %s in irc_login" (Printexc2.to_string e);
          print_newline ();
          (match account.account_sock with
              None -> ()
            | Some sock ->
                account.account_sock <- None;
                close sock "exception");
          set_account_status (as_account account) Status_offline          
          
let irc_keepalive account = ()
let irc_send account id msg = ()

let _ =
  protocol_ops.op_protocol_account_from_option <- (fun p assocs -> 
      let account = new_account_val () in
      from_record (account_record account) assocs;
      register_account account;
      if account.account_autologin then (try irc_login account with _ -> ());
      as_account account
  );
  account_ops.op_account_login <- (fun account ->
      (try irc_login account with _ -> ());
  );
  account_ops.op_account_has_rooms <- (fun account -> 
      account.account_sock <> None);
  room_ops.op_room_name <- (fun room -> room.room_name);
  account_ops.op_account_to_option <- (fun account -> 
      to_record (account_record account)
  );
  protocol_ops.op_protocol_new_account <- (fun p ->
      let account = new_account_val () in
      register_account account;
      as_account account);
  account_ops.op_account_keepalive <- irc_keepalive;
  account_ops.op_account_name <- (fun account -> account.account_login);
  account_ops.op_account_config_record <- (fun a -> account_record a);
  account_ops.op_account_new_identity <- (fun account ->
      let identity = new_identity_val account in
      register_identity identity;
      as_identity identity);
  identity_ops.op_identity_config_record <- (fun id -> identity_record id);
  account_ops.op_account_contacts <- (fun account ->
      List.map as_identity account.account_friends);
  identity_ops.op_identity_name <- (fun id -> id.identity_login);
  chat_ops.op_chat_send <- (fun chat msg ->
      List.iter (fun id ->
          irc_send chat.chat_account id.identity_login msg
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
  );
  
  account_ops.op_account_join_room <- (fun account room_name ->
      match account.account_sock with
        None -> ()
      | Some sock ->
          write_string sock (Printf.sprintf "JOIN %s\r\n"  room_name)
  )
    
(*
Message: :sy23!~sy23@pc4-cmbg2-5-cust143.cmbg.cable.ntl.com JOIN :#mldonkey
Discarding event
server to client: read 9
server to client: read 411
server_to_client: complete message
Message: :saberhagen.freenode.net 332 sy23 #mldonkey :..:: Welcome to the best sharing client for linux: MLDONKEY for the edonkey and overnet network -- please visit: http://www.mldonkeyworld.com for more info ::..
UNUSED MESSAGE
server_to_client: complete message
Message: :saberhagen.freenode.net 333 sy23 #mldonkey lemmy 1039911115
UNUSED MESSAGE
server_to_client: complete message
Message: :saberhagen.freenode.net 353 sy23 = #mldonkey :sy23 [FS]Cyberchriss tfe nitram ceu pango radenko_ |Holgi|_ pokey mobydick_ d00f mulligan_ tommie vnc|zzz _spiridon calc Pucmel lemmy Vaste 
UNUSED MESSAGE
server_to_client: complete message
Message: :saberhagen.freenode.net 366 sy23 #mldonkey :End of /NAMES list.
UNUSED MESSAGE
server to client: read 426
server_to_client: complete message
Message: :USERNOTICE!usernotice@usernotice.utility.freenode NOTICE sy23 :You've connected to the Freenode network via IRC.OPENPROJECTS.NET, which is being PHASED OUT.  If you connected directly, please change your client's config to point to IRC.FREENODE.NET and reconnect.  If you connected via another project's irc hostname, irc client default or web site, please let the project owners know to change their pointer!  Thanks.
UNUSED MESSAGE


  *)