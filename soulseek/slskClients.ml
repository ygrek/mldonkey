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


open CommonInteractive
open SlskComplexOptions
open CommonOptions
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

let requests = ref 0  

  
let on_close c d =
  Printf.printf "DISCONNECTED FROM SOURCE"; print_newline ();
  c.client_downloads <- List2.removeq d c.client_downloads

let on_finished  file d =
  current_files := List2.removeq file !current_files;
  old_files =:= (file_best_name (as_file file.file_file), file_size file) :: !!old_files;
  List.iter (fun c ->
      c.client_files <- List.remove_assoc file c.client_files      
  ) file.file_clients
  
let listen () = ()
      
let disconnect_peer c =
  match c.client_peer_sock with
    None -> ()
  | Some sock ->
      Printf.printf "DISCONNECTED FROM PEER"; print_newline ();
      close sock "";
      c.client_peer_sock <- None;
      c.client_requests <- []

let disconnect_result c sock =
  Printf.printf "DISCONNECTED FROM RESULT"; print_newline ();
  close sock "";
  c.client_result_socks <- List2.removeq sock c.client_result_socks
  
let connect_download c file req =
  try
    match c.client_addr with
      None -> ()
    | Some (ip,port) ->
        connection_try c.client_connection_control;
        let sock = connect "client download" 
            (Ip.to_inet_addr ip) port
            (fun _ _ -> ())
        in
        let  d = CommonDownloads.new_download sock 
            (as_client c.client_client) (as_file file.file_file)
          1 (on_close c) (on_finished file) 
          commit_in_subdir in
        set_reader sock (CommonDownloads.download_reader d);
        init_download_connection sock file (login()) req 
        d.CommonDownloads.download_pos;
  
  with e ->
      Printf.printf "Exception %s while connecting to client" 
        (Printexc2.to_string e);
      print_newline ()

let client_to_client c t sock =
  Printf.printf "MESSAGE FROM PEER"; print_newline ();
  C2C.print t;
  print_newline ();
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
                    (Printexc2.to_string e) file.C2C.file_name;
                  print_newline ();
            ) t.SR.files;
            ()
      with Not_found ->
          Printf.printf "******* NO SEARCH ASSOCIATED WITH %d ******"
            t.SR.id; print_newline ();
      end

  | C2C.TransferRequestReq (false, req_id, file_name, size) ->
(* Someone wants to upload to us !! *)
      begin
        try
          let short_file_name = Filename2.basename file_name in
          let file = Hashtbl.find files_by_key (String.lowercase file_name) in
          
          Printf.printf "File Found"; print_newline ();
          if size <> file_size file then begin
              Printf.printf "Bad file size"; print_newline ();
              raise Exit
            end;
          if file_state file = FileDownloading then begin
              client_send sock (C2C.TransferOKReplyReq (req_id, 
                  file_size file));
              connect_download c file req_id
              
            end else begin
              incr requests;
              client_send sock (C2C.TransferFailedReplyReq (!requests,
                "Not needed anymore"))
            end
          
        with e ->
            Printf.printf "Exception %s for TransferRequestReq Upload %s:%Ld"
              (Printexc2.to_string e) file_name size; print_newline ();
      end
      
  | C2C.SharedFileListReq files ->
      List.iter (fun (dir, files) ->
          List.iter (fun f ->
              let r = new_result f.C2C.file_name f.C2C.file_size in
              add_result_source r c.client_user (Filename.concat dir f.C2C.file_name);
              client_new_file (as_client c.client_client) dir
                (as_result r.result_result)
          ) files
      ) files

  | C2C.TransferOKReplyReq (req, filesize) ->
      begin
        try
          let file = List.assoc req c.client_requests in
          c.client_requests <- List.remove_assoc req c.client_requests;
          connect_download c file req
        with
          Not_found ->
            Printf.printf "req %d not found !" req; print_newline ();
      end      

  | C2C.TransferFailedReplyReq (req, reason) ->
      begin
        try
          let file = List.assoc req c.client_requests in
          c.client_requests <- List.remove_assoc req c.client_requests;
          if String.lowercase reason = "queued" then
            set_client_state c Connected_queued
        with
          Not_found ->
            Printf.printf "req %d not found !" req; print_newline ();
      end      
      
  | _ -> 
      Printf.printf "Unused message from client:"; print_newline ();
      SlskProtocol.C2C.print t;
      print_newline () 

let connect_peer c token msgs =
  Printf.printf "CONNECT PEER"; print_newline ();
  match c.client_peer_sock with
    Some sock -> 
      List.iter (fun t -> client_send sock t) msgs
  | None ->
      try
        match c.client_addr with
          None -> 
            Printf.printf "NO ADDRESS FOR CLIENT"; print_newline ();
            List.iter (fun s ->
                match s.server_sock with
                  None -> ()
                | Some sock ->
                    Printf.printf "ASKING FOR CLIENT IP: %s"  c.client_name;
                    print_newline ();
                    server_send sock (C2S.GetPeerAddressReq c.client_name);
            ) !connected_servers
            
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
            (Printexc2.to_string e);
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
            (Printexc2.to_string e);
          print_newline ()
