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

open BasicSocket
open TcpBufferedSocket
open CommonSearch
open SlskProtocol
open CommonResult
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
open SlskProtocol

let listen () = ()

  
let disconnect_peer c =
  match c.client_peer_sock with
    None -> ()
  | Some sock ->
      Printf.printf "DISCONNECTED FROM PEER"; print_newline ();
      close sock "";
      c.client_peer_sock <- None

let disconnect_download c =
  match c.client_download_sock with
    None -> ()
  | Some sock ->
      Printf.printf "DISCONNECTED FROM SOURCE"; print_newline ();
      close sock "";
      c.client_download_sock <- None;
      set_client_state c NotConnected

let disconnect_result c sock =
  Printf.printf "DISCONNECTED FROM RESULT"; print_newline ();
  close sock "";
  c.client_result_socks <- List2.removeq sock c.client_result_socks

let client_to_client c t sock =
  Printf.printf "MESSAGE FROM PEER"; print_newline ();
  match t with
  | C2C.FileSearchResultReq t ->
      begin
        let module SR = C2C.FileSearchResult in
        let u = new_user t.SR.user in
        try
          let q = List.assoc t.SR.id !SlskGlobals.searches in
          List.iter (fun file ->
              try
                let basename = Filename2.basename file.C2C.file_name in
                let r = new_result basename file.C2C.file_size in
                add_result_source r u file.C2C.file_name;
                search_add_result q r.result_result
              with e ->
                  Printf.printf "Exception %s for file %s" 
                    (Printexc.to_string e) file.C2C.file_name;
                  print_newline ();
            ) t.SR.files;
            ()
      with Not_found ->
          Printf.printf "******* NO SEARCH ASSOCIATED WITH %d ******"
            t.SR.id; print_newline ();
      end
  | _ -> 
      Printf.printf "Unused message from client:"; print_newline ();
      SlskProtocol.C2C.print t;
      print_newline () 

let connect_peer c token msgs =
  match c.client_peer_sock with
    Some sock -> 
      List.iter (fun t -> client_send sock t) msgs
  | None ->
      try
        match c.client_addr with
          None -> ()
        | Some (ip,port) ->
            Printf.printf "CONNECTING"; print_newline ();
            connection_try c.client_connection_control;      
            let sock = connect "peer connect" 
                (Ip.to_inet_addr ip) port
                (fun _ _ -> ())
            in
            set_closer sock (fun _ _ -> disconnect_peer c);
            TcpBufferedSocket.set_read_controler sock download_control;
            TcpBufferedSocket.set_write_controler sock upload_control;
            set_rtimeout sock 30.;
            TcpBufferedSocket.set_reader sock (
              soulseek_handler C2C.parse (client_to_client c));
            c.client_peer_sock <- Some sock;
            init_peer_connection sock (login ()) token;
            List.iter (fun t -> client_send sock t) msgs
      with e ->
          Printf.printf "Exception %s while connecting to client" 
            (Printexc.to_string e);
          print_newline ();
          disconnect_peer c

let connect_result c token =
  try
    match c.client_addr with
      None -> ()
    | Some (ip,port) ->
        Printf.printf "CONNECTING"; print_newline ();
        connection_try c.client_connection_control;      
        let sock = connect "peer connect" 
            (Ip.to_inet_addr ip) port
            (fun _ _ -> ())
        in
        set_closer sock (fun _ _ -> disconnect_result c sock);
        TcpBufferedSocket.set_read_controler sock download_control;
        TcpBufferedSocket.set_write_controler sock upload_control;
        set_rtimeout sock 30.;
        TcpBufferedSocket.set_reader sock (
          soulseek_handler C2C.parse (client_to_client c));
        c.client_result_socks <- sock :: c.client_result_socks;
        init_result_connection sock token
      with e ->
          Printf.printf "Exception %s while connecting to client" 
            (Printexc.to_string e);
          print_newline ()
          
let connect_download c =
  try
    match c.client_addr with
      None -> ()
    | Some (ip,port) ->
        connection_try c.client_connection_control;
        let sock = connect "client download" 
            (Ip.to_inet_addr ip) port
            (fun _ _ -> ())
        in
        set_closer sock (fun _ _ -> disconnect_download c);
        TcpBufferedSocket.set_read_controler sock download_control;
        TcpBufferedSocket.set_write_controler sock upload_control;
        set_rtimeout sock 30.;
        (*
        TcpBufferedSocket.set_reader sock (client_reader c);
        init_download_connection sock
*)          
  with e ->
      Printf.printf "Exception %s while connecting to client" 
        (Printexc.to_string e);
      print_newline ();
      disconnect_download c
