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
  
let disconnect_server s =
  match s.server_sock with
    None -> ()
  | Some sock ->
      close sock "";
      s.server_sock <- None;
      set_server_state s NotConnected;
      connected_servers := List2.removeq s !connected_servers

let server_to_client s m sock =
  match m with
    S2C.LoginAckReq t ->
      begin
        match t with
          S2C.LoginAck.Success (message, ip) ->
            set_server_state s Connected_initiating;
            Printf.printf "Message from server: %s" message;
            print_newline ();
        | S2C.LoginAck.Failure message ->
            Printf.printf "Rejected from server: %s" message;
            print_newline ();
            disconnect_server s
        
      end
  | S2C.RoomListReq t ->
      set_server_state s Connected_idle;
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
      room_new_user (as_room room.room_room) (as_user user.user_user)
  | S2C.UserLeftRoomReq (room, user) ->
      ()
  | _ -> 
      Printf.printf "Unused message from server:"; print_newline ();
      SlskProtocol.S2C.print m;
      print_newline () 
      
let connect_server s = 
  match s.server_sock with
    Some _ -> ()
  | None ->
      if can_open_connection () then
        try
          connection_try s.server_connection_control;
          let ip = ip_of_addr s.server_addr in
          let sock = TcpBufferedSocket.connect "slsk to server" (
              Ip.to_inet_addr ip)
            s.server_port (fun _ _ -> ())  in
          
          set_reader sock (soulseek_handler S2C.parse 
            (server_to_client s));
          
          set_server_state s Connecting;
          set_read_controler sock download_control;
          set_write_controler sock upload_control;
          
          set_rtimeout sock 60.;
          set_handler sock (BASIC_EVENT RTIMEOUT) (fun s ->
              close s "timeout"  
          );
          s.server_nick <- 0;
          s.server_sock <- Some sock;
          server_send sock (
            let module L = C2S.Login in
            C2S.LoginReq {
              L.login = login ();
              L.password = !!password;
              L.version = 200;
            });
          server_send sock (C2S.SetWaitPortReq !!slsk_port)
        with e -> 
            Printf.printf "%s:%d IMMEDIAT DISCONNECT %s"
              (string_of_addr s.server_addr) s.server_port
              (Printexc.to_string e); print_newline ();
(*      Printf.printf "DISCONNECTED IMMEDIATLY"; print_newline (); *)
            s.server_sock <- None;
            set_server_state s NotConnected;
            connection_failed s.server_connection_control
            
  
  
  
let recover_files () = ()
let ask_for_files () = ()
  