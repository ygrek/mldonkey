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

open CommonGlobals
open Options
open TcpBufferedSocket
open AgGlobals	
open AgOptions
open AgTypes

(* need to detect CONNECTIOn in tcpBufferedSocket, and to detect ip from
that point. *)
  
module AP = AgProtocol
module AC = AgClients
module DO = CommonOptions
  
let client_to_server t sock =
  Printf.printf "MESSAGE RECEIVED"; print_newline ();
  match t with
  | AP.SendSharesStandByReq ->  
      server_sock := Some sock;
      AP.server_send sock AP.ReadyToSendSharesReq 

  | AP.SendSharesReq ->  
      AP.server_send sock AP.ReadyForTransferReq
        
  | AP.FileTransferReq t ->
      checked_file_transfer := true;
      AC.init_file_transfer t
  | _ ->
      Printf.printf "UNUSED MESSAGE:" ;print_newline ();
      AP.print t;
      print_newline () 
      
let redirect_to sock ip port =
  Printf.printf "Redirected to %s:%d" (Ip.to_string ip) port;
  print_newline ();
  server_connection_state := Connecting_to_server;
  close sock "Redirecting";
  let sock = connect  "audio_galaxy to server"
      (Ip.to_inet_addr ip) port (fun _ _ -> ())
  in
  verify_ip sock;
(* Now, really connect to the server *)
  message_counter := 0;
  checked_file_transfer := false;
  TcpBufferedSocket.set_reader sock (AgProtocol.audiogal_handler
      client_to_server);
  TcpBufferedSocket.set_closer sock (fun _ s ->
      close sock "closed";
      
      Printf.printf "DISCONNECTED FROM SERVER"; print_newline ();
      server_connection_state := Not_connected;
      match !server_sock with
        None -> ()
      | Some sock -> server_sock := None
  );
  BasicSocket.set_rtimeout (TcpBufferedSocket.sock sock) 30.;

  let ip = try TcpBufferedSocket.my_ip sock with
      e ->
        Printf.printf "Couldn't get my IP %s"
          (Printexc.to_string e); print_newline (); !!DO.client_ip in
  AP.server_send sock (
    let module L = AP.Login in
    AP.LoginReq {
      L.name = !!login;
      L.password = !!password;
      L.version = "0.520L";
      L.ip = ip;
    }
  );
  ()
  
let connect_server () =
  match  !server_connection_state with
    Not_connected ->
      begin
        try
          Printf.printf "TRYING TO CONNECT TO audiogalaxy redirector";
          print_newline ();
          let sock = connect "audiogalaxy to redirectory"
              (Ip.to_inet_addr (if !!gold_account then
                  !!gold_redirection_server_ip
                else !!redirection_server_ip))
            21 (fun _ _ -> ())
          in
          verify_ip sock;
          server_connection_state := Connecting_to_redirector;
          TcpBufferedSocket.set_reader sock (AgProtocol.redirection_handler
              redirect_to);
          
          TcpBufferedSocket.set_closer sock (fun _ s ->
              close sock "closed";
              match !server_connection_state with
                Connecting_to_redirector 
              | Connected_to_redirector ->
                  server_connection_state := Not_connected
              | _ -> ());
          
          BasicSocket.set_rtimeout (TcpBufferedSocket.sock sock) 30.;
        with e ->
            Printf.printf "Exception %s while trying to connect !!"
              (Printexc.to_string e);
            print_newline () 
      end
  | _ -> ()
