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
open CommonGlobals
open CommonClient
open CommonComplexOptions
open CommonTypes
open CommonFile
open Options
open TcpBufferedSocket
open AgGlobals	
open AgOptions
open AgTypes
open AgComplexOptions

(* need to detect CONNECTIOn in tcpBufferedSocket, and to detect ip from
that point. *)
  
module AP = AgProtocol
module DO = CommonOptions
  
let connect_to_client = ()

let fts_send file_id fts = 
  match !server_sock with
    None -> ()
  | Some sock ->
      AP.server_send sock (
        let module F = AP.FileTransferState in
        AP.FileTransferStateReq {
          F.file_id = file_id;  
          F.file_state = fts;
        })

let client_close c sock =
  lprintf "DISCONNECTED FROM CLIENT"; lprint_newline ();
  close sock "closed";
  match c.client_sock with
    Some ss when ss == sock ->
      c.client_sock <- None;
      fts_send c.client_file_id FTS_CONNECTION_CLOSED
  | _ -> ()

    
let file_complete file file_id =
  lprintf "FILE %s DOWNLOADED" file.file_name;
  lprint_newline ();
  fts_send file_id FTS_DOWNLOAD_COMPLETE;
  file_completed (as_file file.file_file);
  current_files := List2.removeq file !current_files;
  old_files =:= (file.file_name, (file_size file)) :: !!old_files;
  
  (*
  let incoming_dir =
    if !!commit_in_subdir <> "" then
      Filename.concat !!DO.incoming_directory !!commit_in_subdir
    else !!DO.incoming_directory
  in
  (try Unix2.safe_mkdir incoming_dir with _ -> ());
  let new_name = rename_to_incoming_dir file.file_temp
      (Filename.concat incoming_dir (canonize_basename file.file_name)) in
  CommonChat.send_warning_for_downloaded_file file.file_name
*)
  ()
    
let file_writter c sock nread =
(*  lprintf "RECEIVED %d bytes" nread; lprint_newline (); *)
  let b = TcpBufferedSocket.buf sock in
  let file = c.client_file in
  begin
    let fd = try
        Unix32.force_fd (file_fd file) 
      with e -> 
          lprintf "In Unix32.force_fd"; lprint_newline ();
          raise e
    in
    let final_pos = Unix32.seek32 (file_fd file) c.client_file_pos Unix.SEEK_SET in
    Unix2.really_write fd b.buf b.pos b.len;
  end;
  c.client_file_pos <- Int32.add c.client_file_pos (Int32.of_int nread);
  TcpBufferedSocket.buf_used sock b.len;
  if c.client_file_pos > (file_downloaded file) then begin
      file.file_file.impl_file_downloaded <- c.client_file_pos;
    end;
  if (file_downloaded file) = (file_size file) then begin
      file_complete file c.client_file_id
    end
  
let peer_sender c sock (t: AP.PeerReq.file_request) =
  ()
      
let init_file_transfer t =
  let module FT = AP.FileTransfer in
  let file = new_file t.FT.file_id t.FT.filename t.FT.size in
  
  if (file_downloaded file) = (file_size file) then begin
      lprintf "FILE DOWNLOADED"; lprint_newline ();
      fts_send t.FT.file_id FTS_DOWNLOAD_COMPLETE
    end else
  let module P = AP.PeerReq in
    
  if (file_downloaded file) < (file_size file) &&
    not (List.memq file !current_files) then begin
      current_files := file :: !current_files;
    end;
  
  begin
    match file.file_client with
      Some c ->
        lprintf "ALREADY A CLIENT FOR THAT FILE"; lprint_newline ();
        begin
          match c.client_sock with
            None -> ()
          | Some sock -> 
              client_close c sock
        end;
        
        begin
          match c.client_server with
            None -> ()
          | Some sock -> TcpServerSocket.close sock "Another client"
        end;
    | _ -> ()
  end;
  
  let rec c = {
      client_client = client_impl;
      client_sock = None;
      client_server = None;
      client_ip = t.FT.ip;
      client_port = t.FT.port;
      client_file = file;
      client_file_pos = (file_downloaded file);
      client_direction = t.FT.direction;
      client_id = t.FT.local_id;
      client_file_id = t.FT.file_id;
    } and
    client_impl = {
      dummy_client_impl with
      impl_client_val = c;
      impl_client_ops = client_ops;
    }
  in
  CommonClient.new_client client_impl;
  
  file.file_client <- Some c;
  match t.FT.connect with
    Connect -> 
      
      let sock = connect  "audiogalaxy to client" 
        (Ip.to_inet_addr c.client_ip) c.client_port
          (fun sock event -> 
            match event with
              TcpBufferedSocket.BASIC_EVENT (RTIMEOUT|LTIMEOUT) -> 
                client_close c sock
            | _ -> ())
      in
      c.client_sock <- Some sock;
(* Now, really connect to the server *)

      let connected = ref false in
      TcpBufferedSocket.set_reader sock (match c.client_direction with
          ReceiveFile -> 
            P.receive_peer_handler (file_writter c)
        | SendFile -> P.send_peer_handler (peer_sender c)
      );
      TcpBufferedSocket.set_closer sock (fun _ s -> client_close c sock);
      BasicSocket.set_rtimeout (TcpBufferedSocket.sock sock) 10.;
      
      P.peer_send sock {
        P.file_id = c.client_file_id;
        P.file_size = (file_size file);
        P.file_pos = c.client_file_pos;
      };
      fts_send c.client_file_id FTS_SETTING_UP; 
  
  | Listen -> 
      
      try
        let sock = TcpServerSocket.create "audiogalaxy client waiting" 
          Unix.inet_addr_any
            c.client_port (fun sock event ->
              match event with
                TcpServerSocket.CONNECTION (s, 
                  Unix.ADDR_INET(from_ip, from_port)) ->
                  TcpServerSocket.close sock "CONNECTION RECEIVED";
                  lprintf "CONNECTION RECEIVED FROM %s"
                    (Ip.to_string (Ip.of_inet_addr from_ip))
                  ; 
                  lprint_newline ();
                  
                  
                  let sock = TcpBufferedSocket.create "audiogalaxy client connection" s (fun _ _ -> ()) in
                  c.client_sock <- Some sock;
                  c.client_server <- None;
                  TcpBufferedSocket.set_closer sock (fun _ s ->
                      client_close c sock
                  );
                  BasicSocket.set_rtimeout (TcpBufferedSocket.sock sock) 30.;
                  let module P = AP.PeerReq in
                  P.peer_send sock {
                    P.file_id = c.client_file_id;
                    P.file_size = (file_size file);
                    P.file_pos = c.client_file_pos;
                  };
                  
                  TcpBufferedSocket.set_reader sock (match c.client_direction with
                      ReceiveFile -> P.receive_peer_handler (file_writter c)
                    | SendFile -> P.send_peer_handler (peer_sender c)
                  );
                  ()
                  
              | TcpServerSocket.BASIC_EVENT BasicSocket.RTIMEOUT -> 
                  lprintf "TIMEOURT"; lprint_newline ();
                  fts_send c.client_file_id FTS_TIMEOUT ;

              | _ -> ()
                  ) in
        c.client_server <- Some sock;

        fts_send c.client_file_id FTS_SETTING_UP

      with e ->
          fts_send c.client_file_id  FTS_PORT_NOT_AVAILABLE
          
  
