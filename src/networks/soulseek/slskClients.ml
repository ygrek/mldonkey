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
open CommonDownloads
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

    
let listen () = ()
      
let disconnect_peer c reason =
  match c.client_peer_sock with
    None -> ()
  | Some sock ->
      lprintf "DISCONNECTED FROM PEER"; lprint_newline ();
      close sock reason;
      c.client_peer_sock <- None;
      c.client_requests <- []

let disconnect_result c sock =
  lprintf "DISCONNECTED FROM RESULT"; lprint_newline ();
  close sock Closed_by_user;
  c.client_result_socks <- List2.removeq sock c.client_result_socks
  
module Download = CommonDownloads.Make(struct 
      
      type c = client
      type f = file
      
      let file file = as_file file.file_file
      let client client = as_client client.client_client        
      let subdir_option = commit_in_subdir
      
      let client_disconnected d =
        lprintf "DISCONNECTED FROM SOURCE"; lprint_newline ();
        let c = d.download_client in
        c.client_downloads <- List2.removeq d c.client_downloads
      
      
      let download_finished d =
        let file = d.download_file in
        if List.memq file !current_files then begin
            current_files := List2.removeq file !current_files;
            old_files =:= (file_best_name (as_file file.file_file), 
              file_size file) :: !!old_files;
            List.iter (fun c ->
                c.client_files <- List.remove_assoc file c.client_files      
            ) file.file_clients
          end
        
    end)
  
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
        let  d = Download.new_download sock c file 1 in
        set_reader sock (Download.download_reader d);
        init_download_connection sock file (local_login()) req 
        d.download_pos;
  
  with e ->
      lprintf "Exception %s while connecting to client" 
        (Printexc2.to_string e);
      lprint_newline ()

let client_to_client c t sock =
  if !verbose_msg_clients then begin
      lprintf "MESSAGE FROM PEER"; lprint_newline ();
      C2C.print t;
      lprint_newline ();
    end;
  
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
                search_add_result true q r.result_result
              with e ->
                  lprintf "Exception %s for file %s" 
                    (Printexc2.to_string e) file.C2C.file_name;
                  lprint_newline ();
            ) t.SR.files;
            ()
      with Not_found ->
          lprintf "******* NO SEARCH ASSOCIATED WITH %d ******"
            t.SR.id; lprint_newline ();
      end

  | C2C.TransferRequestReq (false, req_id, file_name, size) ->
(* Someone wants to upload to us !! *)
      begin
        try
          let short_file_name = Filename2.basename file_name in
          let file = Hashtbl.find files_by_key (String.lowercase file_name) in
          
          lprintf "File Found"; lprint_newline ();
          if size <> file_size file then begin
              lprintf "Bad file size"; lprint_newline ();
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
            lprintf "Exception %s for TransferRequestReq Upload %s:%Ld"
              (Printexc2.to_string e) file_name size; lprint_newline ();
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
            lprintf "req %d not found !" req; lprint_newline ();
      end      

  | C2C.TransferFailedReplyReq (req, reason) ->
      begin
        try
          let file = List.assoc req c.client_requests in
          c.client_requests <- List.remove_assoc req c.client_requests;
          let reason =  String.lowercase reason in
          if reason = "queued" then
            set_client_state c (Connected 0)
          else
            update_file_state (file.file_file) (FileAborted reason)
        with
          Not_found ->
            lprintf "req %d not found !" req; lprint_newline ();
      end      
      
  | _ -> 
      lprintf "Unused message from client:"; lprint_newline ();
      SlskProtocol.C2C.print t;
      lprint_newline () 

let connect_peer c token msgs =
  if !verbose_msg_clients then begin
      lprintf "CONNECT PEER"; lprint_newline ();
    end;
  match c.client_peer_sock with
    Some sock -> 
      List.iter (fun t -> client_send sock t) msgs
  | None ->
      try
        match c.client_addr with
          None -> 
            if !verbose_msg_clients then begin
                lprintf "NO ADDRESS FOR CLIENT"; lprint_newline ();
              end;
            List.iter (fun s ->
                match s.server_sock with
                  None -> ()
                | Some sock ->
                    if !verbose_msg_servers then begin
                        lprintf "ASKING FOR CLIENT IP: %s"  c.client_name;
                        lprint_newline ();
                      end;
                    server_send sock (C2S.GetPeerAddressReq c.client_name);
            ) !connected_servers
            
        | Some (ip,port) ->
            if !verbose_msg_clients then begin
                lprintf "CONNECTING"; lprint_newline ();
              end;
            connection_try c.client_connection_control;      
            let sock = connect "peer connect" 
                (Ip.to_inet_addr ip) port
                (fun _ _ -> ())
            in
            set_closer sock (fun _ r -> disconnect_peer c r);
            TcpBufferedSocket.set_read_controler sock download_control;
            TcpBufferedSocket.set_write_controler sock upload_control;
            set_rtimeout sock 30.;
            TcpBufferedSocket.set_reader sock (
              soulseek_handler C2C.parse (client_to_client c));
            c.client_peer_sock <- Some sock;
            init_peer_connection sock (local_login ()) token;
            List.iter (fun t -> client_send sock t) msgs
      with e ->
          lprintf "Exception %s while connecting to client\n" 
            (Printexc2.to_string e);
          disconnect_peer c (Closed_for_exception e)

let connect_result c token =
  try
    match c.client_addr with
      None -> ()
    | Some (ip,port) ->
        if !verbose_msg_clients then begin
            lprintf "CONNECTING"; lprint_newline ();
          end;
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
          lprintf "Exception %s while connecting to client" 
            (Printexc2.to_string e);
          lprint_newline ()
