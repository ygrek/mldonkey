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
open Printf2
open CommonUploads
open CommonInteractive

open CommonClient
open BasicSocket
open CommonOptions
open TcpBufferedSocket
open CommonGlobals
open CommonFile
open CommonUser
open CommonRoom
open CommonServer
open CommonResult
open CommonTypes
open CommonSearch
open Options
open DcTypes
open DcOptions
open DcGlobals
open DcProtocol

let disconnect_client c reason = 
  match c.client_sock with
  | Connection sock ->
      connection_failed c.client_connection_control;      
      lprintf "CLOSE SOCKET"; lprint_newline ();
      close sock reason;
      set_client_disconnected c reason;
      if c.client_files = [] then
        remove_client c
  | ConnectionWaiting token -> 
      cancel_token token;
      c.client_sock <- NoConnection
  | _ -> ()
      
let char_percent =  int_of_char '%'
let char_z =  int_of_char 'z'
        
let create_key () =
  let len = 50 + Random.int 50 in
  let key = String.create len in
  for i = 0 to len - 1 do
    key.[i] <- char_of_int (char_percent + Random.int (char_z - char_percent))
  done;
  LockReq { 
    Lock.info = !!client_keyinfo;
    Lock.key = key
  }

(*
LockReq { 
  Lock.info = "Pk=DCPLUSPLUS0.177ABCABC"; 
  Lock.key = "EXTENDEDPROTOCOLABCABCABCABCABCABC"
});  
*)

let should_browse c =
  let cc = as_client c.client_client in
  c.client_all_files = None && ((is_friend  cc) || (is_contact  cc)) 
  
let init_connection nick_sent c sock =
  c.client_receiving <- Int64.zero;
  c.client_sock <- Connection sock;
  connection_ok c.client_connection_control;  
  if not nick_sent then begin
      let my_nick = match c.client_user.user_servers with
          [] -> local_login ()
        | s :: _ -> s.server_last_nick 
      in
      server_send !verbose_msg_clients sock (MyNickReq my_nick);
      server_send !verbose_msg_clients sock (create_key ());
    end;
  if should_browse c then
    server_send !verbose_msg_clients sock (DirectionReq { 
        Direction.direction = Download;
        Direction.level = 31666;
      })      
  else  
  if c.client_files = [] then
    server_send !verbose_msg_clients sock (DirectionReq { 
        Direction.direction = Upload;
        Direction.level = 666;
      }) else
    server_send !verbose_msg_clients sock (DirectionReq { 
        Direction.direction = Download;
        Direction.level = 666;
      })

    

      
let read_first_message nick_sent t sock =
  lprintf "FIRST MESSAGE"; lprint_newline ();
  print t;
  lprint_newline ();
  match t with 
    MyNickReq n ->
      begin
        let c = new_client n in
        match c.client_sock with
          Connection _ -> lprintf "Already connected"; lprint_newline ();
            close sock (Closed_for_error "already connected");
            raise Not_found
            
        | _ ->
            init_connection nick_sent c sock;
            Some c
      end
  | _ ->
      lprintf "BAD MESSAGE"; 
      print t; lprint_newline ();
      close sock (Closed_for_error "bad message");
      raise Not_found
  
let client_reader c t sock =
  if !verbose_msg_clients then begin
      lprintf "FROM CLIENT %s" c.client_name; lprint_newline ();
      print t;
      lprint_newline ();
    end;
  
  match t with
    MyNickReq n ->
      connection_ok c.client_connection_control;
      if c.client_name != n then begin
          lprintf "Bad nickname for client %s/%s" n c.client_name; 
          lprint_newline ();
          disconnect_client c (Closed_for_error "Bad Nickname");
          raise Not_found
        end;
  
  | LockReq lock ->
      server_send !verbose_msg_clients sock (
        KeyReq { Key.key = DcKey.gen lock.Lock.key });
      let cc = as_client c.client_client in
      begin
        if should_browse c then begin            
            
            lprintf "TRY TO DOWNLOAD FILE LIST"; lprint_newline ();
            server_send !verbose_msg_clients sock (GetReq {
                Get.name = "MyList.DcLst";
                Get.pos = Int64.one;
              });
            c.client_pos <- Int64.zero;
            c.client_download <- DcDownloadList (Buffer.create 10000)
          
          end else 
        let rec iter_files files =
          match files with
          | [] -> lprintf "NO FILE TO UPLOAD"; lprint_newline ();
          
          | (file, filename) :: tail -> 
              if file_state file = FileDownloading then begin
                  lprintf "GET: file downloaded %Ld"  (file_downloaded file);
                  lprint_newline ();
                  server_send !verbose_msg_clients sock (GetReq {
                      Get.name = filename;
                      Get.pos = Int64.succ (file_downloaded file);
                    });
                  c.client_download <- DcDownload file;
                  c.client_pos <- (file_downloaded file);
                end
              else 
                iter_files tail
        in
        iter_files c.client_files
      end
      
  | KeyReq _ ->
      lprintf "DISCARD KEY ..."; lprint_newline ();
  
  | DirectionReq t ->
(* HERE, we should check for upload slots ...*)
      if t.Direction.direction = Download then begin
(* this client wants something from us ... *)

(* is this OK (we sent two Direction messages !) ? *)
          server_send !verbose_msg_clients sock (DirectionReq { 
              Direction.direction = Upload;
              Direction.level = 666;
            });
        end;
      ()
  
  | FileLengthReq t ->
      begin
        match c.client_download with
          DcDownload file ->
            if t = (file_size file) then begin
                c.client_receiving <- t;
              end else begin
                lprintf "Bad file size: %Ld  <> %Ld"
                  t
                  (file_size file);
                lprint_newline ();
                disconnect_client c (Closed_for_error "Bad file size");
                raise Not_found
              end
        
        | DcDownloadList buf ->
            c.client_receiving <- t;
        
        | _ ->
            lprintf "Not downloading any thing"; lprint_newline ();
            disconnect_client c (Closed_for_error "Nothing to download");
            raise Not_found
      end;
      server_send !verbose_msg_clients sock SendReq
  
  | GetReq t ->
(* this client REALLY wants to download from us !! *)
      lprintf "GET REQ"; lprint_newline ();
      let list = make_shared_list () in
      lprintf "Shared list = [%s]\n" list;
      
      if t.Get.name = "MyList.DcLst" then begin
          c.client_pos <- Int64.zero;
          let list = Che3.compress list in
          c.client_download <- DcUploadList list;
          server_send !verbose_msg_clients sock (FileLengthReq (
              Int64.of_int (String.length list)))
        end else begin 
(* Upload not yet implemented *)
          let filename = String2.replace t.Get.name '\\' "/"  in
          try
            lprintf "Wants to upload [%s]\n" filename;
            let sh = CommonUploads.find_by_name filename in
            
            c.client_pos <- Int64.pred t.Get.pos;
            lprintf "from pos %Ld\n" c.client_pos;
            let info = IndexedSharedFiles.get_result sh.shared_info in  
            let rem = info.shared_size -- c.client_pos in
            lprintf "remaining %Ld\n" rem;
            server_send !verbose_msg_clients sock (FileLengthReq rem);
            c.client_download <- DcUpload sh
          with e ->
              lprintf "Exception %s in GetReq\n" (Printexc2.to_string e);
              
              lprintf "List of shared files:\n";
              Hashtbl.iter (fun name sh ->
                  lprintf "   [%s]\n" name;
                  lprintf "   [%s]\n" filename;
              ) CommonUploads.shared_files;
              lprintf "done\n";
              
              ()
        end
  
  | SendReq ->
      c.client_pos <- Int64.zero;
      let rec refill sock =
        lprintf "FILL SOCKET"; lprint_newline ();
        let len = remaining_to_write sock in
        let can = maxi (8192 - len) 0 in
        if can > 0 then
          match c.client_download with
            DcUploadList list ->
              lprintf "DcUploadList"; lprint_newline ();
              let slen = String.length list in
              let pos = Int64.to_int c.client_pos in
              if pos < slen then begin
                  let send_len = mini (slen - pos) can in
                  lprintf "Sending %d" send_len; lprint_newline ();
                  TcpBufferedSocket.write sock list pos send_len;
                  lprintf "sent"; lprint_newline ();
                  c.client_pos <- c.client_pos ++ (Int64.of_int send_len);
                  
                  if pos + len = slen then begin
(* Normally, the client should close the connection after the download,
but since we don't want a buggy client to keep this connection, just
close it after a long timeout. *)
                      set_lifetime sock 120.
                    end else
                  if remaining_to_write sock = 0 then refill sock
                
                end 
          
          | DcUpload sh -> 
              lprintf "DcUpload"; lprint_newline ();            
              let info = IndexedSharedFiles.get_result sh.shared_info in  
              let slen = info.shared_size in
              let pos = c.client_pos in
              if pos < slen then
                let rlen = 
                  let rem = slen -- pos in
                  if rem > Int64.of_int can then can else Int64.to_int rem
                in
                let upload_buffer = String.create rlen in
                Unix32.read sh.shared_fd pos upload_buffer 0 rlen;
                TcpBufferedSocket.write sock upload_buffer 0 rlen;
                c.client_pos <- c.client_pos ++ (Int64.of_int rlen);
                if c.client_pos = slen then begin
(* Normally, the client should close the connection after the download,
but since we don't want a buggy client to keep this connection, just
close it after a long timeout. *)
                    set_lifetime sock 120. 
                  end else
                if remaining_to_write sock = 0 then refill sock
                  | _ -> assert false
      in
      set_refill sock refill;
      set_handler sock WRITE_DONE (fun sock -> 
          lprintf "CLOSE SOCK AFTER REFILL DONE"; lprint_newline ();
          close sock Closed_by_user)
        
  | _ ->
      lprintf "###UNUSED CLIENT MESSAGE###########"; lprint_newline ();
      DcProtocol.print t

let file_complete file = 
(*
  lprintf "FILE %s DOWNLOADED" f.file_name;
lprint_newline ();
  *)
  file_completed (as_file file.file_file);
  current_files := List2.removeq file !current_files;
  List.iter (fun c ->
      c.client_files <- List.remove_assoc file c.client_files
  ) file.file_clients
  
let client_downloaded c sock nread = 
  lprintf ".";
  if nread > 0 then
    match c.client_download with
    | DcDownload file ->
        let b = TcpBufferedSocket.buf sock in
        set_rtimeout sock half_day;
        Unix32.write (file_fd file) c.client_pos  b.buf b.pos b.len;
        (*
        begin
          let fd = try
              Unix32.force_fd (file_fd file) 
            with e -> 
                lprintf "In Unix32.force_fd"; lprint_newline ();
                raise e
          in
          let final_pos = Unix32.seek64 (file_fd file) c.client_pos Unix.SEEK_SET in
          Unix2.really_write fd b.buf b.pos b.len;
        end; *)
(*      lprintf "DIFF %d/%d" nread b.len; lprint_newline ();*)
        c.client_pos <- c.client_pos ++ (Int64.of_int b.len);
(*
      lprintf "NEW SOURCE POS %s" (Int64.to_string c.source_pos);
lprint_newline ();
  *)
        buf_used b b.len;
        if c.client_pos > (file_downloaded file) then begin
            add_file_downloaded (as_file file.file_file)
            (c.client_pos -- (file_downloaded file));
          end;
        if (file_downloaded file) = (file_size file) then begin
            close sock Closed_by_user;
            file_complete file 
          end
          
    | DcDownloadList buf ->
        lprintf "DcDownloadList"; lprint_newline ();
        let b = TcpBufferedSocket.buf sock in        
        let len = b.len in
        Buffer.add_substring buf b.buf b.pos b.len;
        buf_used b b.len;
        c.client_receiving <- c.client_receiving -- (Int64.of_int len);
        lprintf "Received %d of List\n" len; 
        close sock Closed_by_user; 
        if c.client_receiving = Int64.zero then begin
            lprintf "----------------------------------------\n"; 
            lprintf "RECEIVED COMPLETE FILE LIST \n"; 
            lprintf "----------------------------------------\n"; 
            
            let s = Buffer.contents buf in
            let s = Che3.decompress s in
            try
              lprintf "LIST: [%s]\n" (String.escaped s);
              let files = parse_list c.client_user s in
              lprintf "PARSED\n"; 
              c.client_all_files <- Some files;
              List.iter (fun (dirname,r) ->
                  lprintf "NEW FILE in %s\n" dirname; 
                  client_new_file (as_client c.client_client) dirname
                    r
              ) files;
              ()
            with e ->
                lprintf "Exception %s in parse client files\n"
                  (Printexc2.to_string e);
          end
    | _ -> assert false

let init_anon_client init_sent sock =
  
  TcpBufferedSocket.set_read_controler sock download_control;
  TcpBufferedSocket.set_write_controler sock upload_control;
  TcpBufferedSocket.set_rtimeout sock 30.;
  
  let c = ref None in
  TcpBufferedSocket.set_closer sock (fun _ s ->
      lprintf "DISCONNECTED FROM CLIENT\n"; 
      match !c with
        None -> ()
      | Some c ->  disconnect_client c s
  );
  TcpBufferedSocket.set_reader sock (
    dc_handler3 verbose_msg_clients c (read_first_message init_sent) client_reader
      client_downloaded)  
  
let listen () =
  try
    let sock = TcpServerSocket.create "DC client server" 
        Unix.inet_addr_any
        !!dc_port
        (fun sock event ->
          match event with
            TcpServerSocket.CONNECTION (s, 
              Unix.ADDR_INET(from_ip, from_port)) ->
              lprintf "CONNECTION RECEIVED FROM %s FOR PUSH\n"
              (Ip.to_string (Ip.of_inet_addr from_ip))
              ; 
              
              let token = create_token connection_manager in
              let sock = TcpBufferedSocket.create token
                  "DC client connection" s (fun _ _ -> ()) in
              init_anon_client false sock

          | _ -> ()
      ) in
    listen_sock := Some sock;
    ()
  with e ->
      lprintf "Exception %s while init DC server\n" 
        (Printexc2.to_string e)

let connect_client c =
  let token = 
    add_pending_connection connection_manager
      (fun token ->
        try
          match c.client_addr with
            None -> ()
          | Some (ip,port) ->
              connection_try c.client_connection_control;      
              let sock = connect token "client download" 
                  (Ip.to_inet_addr ip) port
                  (fun sock event ->
                    match event with
                    | BASIC_EVENT (RTIMEOUT | LTIMEOUT) ->
                        disconnect_client c Closed_for_timeout
                    | BASIC_EVENT (CLOSED s) ->
                        disconnect_client c s
                    | _ -> ()
                )
              in
              TcpBufferedSocket.set_read_controler sock download_control;
              TcpBufferedSocket.set_write_controler sock upload_control;
              set_rtimeout sock 30.;
              TcpBufferedSocket.set_reader sock (
                dc_handler3 verbose_msg_clients (ref (Some c)) (read_first_message false) client_reader
                  client_downloaded);
              
              init_connection false c sock;
        
        with e ->
            lprintf "Exception %s while connecting to client\n" 
              (Printexc2.to_string e);
            disconnect_client c Closed_connect_failed
    )
  in
  c.client_sock <- ConnectionWaiting token
          
let connect_anon s ip port =
  let token =
    add_pending_connection connection_manager (fun token ->
        lprintf "CONNECT ANON\n"; 
        try
          let sock = connect token "client download" 
              (Ip.to_inet_addr ip) port
              (fun _ _ -> ())
          in
          init_anon_client true sock;
          server_send !verbose_msg_clients sock (MyNickReq s.server_last_nick);
          server_send !verbose_msg_clients sock (
            create_key ());
        
        with e ->
            lprintf "Exception %s while connecting to  anon client\n" 
              (Printexc2.to_string e)
    )      
  in
  ()