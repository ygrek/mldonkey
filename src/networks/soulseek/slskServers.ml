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
open CommonOptions
open SlskProtocol
open CommonResult
open BasicSocket
open CommonGlobals
open CommonTypes
open CommonClient
open CommonComplexOptions
open GuiProto
open Options
open CommonFile
open CommonUser
open CommonRoom
open CommonTypes
open CommonShared
open CommonServer
open SlskTypes
open SlskOptions
open SlskGlobals
open TcpBufferedSocket
open SlskProtocol

let no_retry = ref false
  
let disconnect_server s r =
  match s.server_sock with
    NoConnection -> ()
  | ConnectionWaiting token ->
      cancel_token token;
      s.server_sock <- NoConnection
  | Connection sock ->
      close sock r;
      s.server_sock <- NoConnection;
      set_server_state s (NotConnected (r, -1));
      connected_servers := List2.removeq s !connected_servers

let server_to_client s m sock =
  
  if !verbose_msg_servers then begin
      lprintf "Message from server"; lprint_newline ();
      S2C.print m;
      lprint_newline ();
    end;
  
  match m with
  | S2C.LoginAckReq t ->
      set_rtimeout sock 300.;
      begin
        match t with
          S2C.LoginAck.Success (message, ip) ->
            set_server_state s Connected_initiating;
            lprintf "Message from server: %s\n" message;
        | S2C.LoginAck.Failure message ->
            lprintf "Rejected from server: %s\n" message;
            no_retry :=  (message = "INVALIDPASS");
            disconnect_server s (Closed_for_error message)
        
      end
  | S2C.RoomListReq t ->
      set_server_state s (Connected (-1));
      connected_servers := s :: !connected_servers;
      List.iter (fun (name, nusers) ->
          let room = new_room name in
          room.room_nusers <- nusers) t
  | S2C.ConnectToPeerReq t ->
      S2C.print m;
      let module C = S2C.ConnectToPeer in
      let c = new_client t.C.name in
      c.client_addr <- Some (t.C.ip, t.C.port);
      SlskClients.connect_result c t.C.token
  | S2C.PriviledgedUsersReq _ -> ()
  | S2C.SayChatroomReq (room, user, message) ->
      begin
        let room = Hashtbl.find rooms_by_name room in
        let user = new_user user in
        room.room_messages <- room_new_message 
          (as_room room.room_room) (PublicMessage (user_num user, message))
        :: room.room_messages;
      end
  | S2C.UserJoinedRoomReq (room, user, _) ->
      let room = Hashtbl.find rooms_by_name room in
      let user = new_user user in
      room_add_user (as_room room.room_room) (as_user user.user_user)
  | S2C.UserLeftRoomReq (room, user) ->
      let room = Hashtbl.find rooms_by_name room in
      let user = new_user user in
      room_remove_user (as_room room.room_room) (as_user user.user_user)
      
  | S2C.JoinRoomReplyReq t ->
      let module J = S2C.JoinRoomReply in
      let room = Hashtbl.find rooms_by_name t.J.room in
      List.iter (fun u ->
          let user = new_user u.J.name in
          room_add_user (as_room room.room_room) (as_user user.user_user)
      ) t.J.users
      
  | S2C.GetPeerAddressReplyReq (name, ip, port) ->
      
      begin
        try
          let c = Hashtbl.find clients_by_name name in
          c.client_addr <- Some (ip, port);

          if client_browsed_tag land client_type c <> 0 then
               SlskClients.connect_peer c 300 [C2C.GetSharedFileListReq]
          
        with Not_found ->
            lprintf "Client %s not found\n" name; 
      end
      
  | _ -> 
      lprintf "Unused message from server:\n";
      SlskProtocol.S2C.print m;
      lprint_newline () 
      
let connect_server s = 
  Ip.async_ip_of_addr s.server_addr (fun ip ->      
      match s.server_sock with
      | NoConnection ->
          if can_open_connection connection_manager then
            let ctoken = 
              add_pending_connection connection_manager (fun ctoken ->
                  s.server_sock <- NoConnection;
                  try
                    connection_try s.server_connection_control;
                    let sock = TcpBufferedSocket.connect 
                      ctoken "slsk to server" 
                        (Ip.to_inet_addr ip)
                      s.server_port (fun _ _ -> ())  in
                    
                    set_reader sock (soulseek_handler S2C.parse 
                        (server_to_client s));
                    
                    set_server_state s Connecting;
                    set_read_controler sock download_control;
                    set_write_controler sock upload_control;
                    
                    set_rtimeout sock 20.;
                    set_handler sock (BASIC_EVENT RTIMEOUT) (fun s ->
                        lprintf "Connection timeout"; lprint_newline ();
                        close s Closed_for_timeout
                    );
                    set_closer sock (fun _ r -> disconnect_server s r);
                    s.server_nick <- 0;
                    s.server_sock <- Connection sock;
                    server_send sock (
                      let module L = C2S.Login in
                      C2S.LoginReq {
                        L.login = local_login ();
                        L.password = !!password;
                        L.version = 180; (* This is what pyslsk sends *)
                      });
                    server_send sock (C2S.SetWaitPortReq !!slsk_port)
                  with e -> 
                      lprintf "%s:%d IMMEDIAT DISCONNECT %s"
                        (Ip.string_of_addr s.server_addr) s.server_port
                        (Printexc2.to_string e); lprint_newline ();
                      disconnect_server s (Closed_for_exception e)
              ) in
            s.server_sock <- ConnectionWaiting ctoken;
      | _ -> ()
  )
  
let recover_files () = ()
 
let ask_for_file file =
  List.iter (fun c ->
      try
        incr SlskClients.requests;
        c.client_requests <- (!SlskClients.requests, file) :: c.client_requests;
        SlskClients.connect_peer c 300 [C2C.TransferRequestReq
            (true, (* download *)
            !SlskClients.requests,
            List.assq file c.client_files,
            (file_size file))
        ];
      with e ->
          lprintf "Exception %s in ask_for_file" (Printexc2.to_string e);
          lprint_newline ();
  ) file.file_clients
  
let ask_for_files () =
  Hashtbl.iter (fun _ file ->
      if file_state file = FileDownloading then
        ask_for_file file
  ) files_by_key

let servers_line = "--servers"
let slsk_kind =  "slsk_server_list"

let load_server_list_last = ref 0
let load_server_list filename = 
  load_server_list_last := last_time ();
  let s = File.to_string filename in
  try
    let s = String2.replace s '\r' "\n" in
    let find_server_line = ref false in
    List.iter (fun s ->
        if !find_server_line then
          if s = servers_line then
            find_server_line := false
          else ()
        else
        match String2.split_simplify s ':' with
          [_;_;server_name; server_port] -> 
            let port = int_of_string server_port in
            lprintf "NEW SERVER %s:%d" server_name port; lprint_newline ();
            (*
            main_server_name =:= server_name;
main_server_port =:= port;
  *)
            ignore (new_server (Ip.addr_of_string server_name) port);
            
        | _ -> ()
    ) (String2.split_simplify s '\n')
  with _ ->
      lprintf "Unable to parse soulseek server file %s" filename;
      lprint_newline ()
  
let server_list = ref []

let update_server_list () =
  if !server_list = [] && not !no_retry then
    Hashtbl.iter (fun _ s ->
        server_list := s :: !server_list) servers_by_addr
    
let rec connect_servers () =
  if !connected_servers = [] then
    match !server_list with
      [] ->
        ()
(*        if !load_server_list_last + 600 < last_time () then
load_url slsk_kind "http://www.slsk.org/slskinfo2"; *)
    | s :: tail ->
        server_list := tail;
        connect_server s

       
let can_retry () =
  no_retry := false;
  update_server_list ()

let _ =
  add_web_kind slsk_kind load_server_list;
  option_hook login can_retry;
  option_hook global_login can_retry;
  option_hook password can_retry
