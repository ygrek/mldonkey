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

let disconnect_client c = 
  match c.client_sock with
    None -> ()
  | Some sock ->
      connection_failed c.client_connection_control;      
      lprintf "CLOSE SOCKET"; lprint_newline ();
      close sock "client close";
      set_client_disconnected c;
      if c.client_files = [] then
        remove_client c

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

let init_connection nick_sent c sock =
  c.client_receiving <- Int64.zero;
  c.client_sock <- Some sock;
  connection_ok c.client_connection_control;  
  if not nick_sent then begin
      let my_nick = match c.client_user.user_servers with
          [] -> !!client_name
        | s :: _ -> s.server_last_nick 
      in
      server_send !verbose_msg_clients sock (MyNickReq my_nick);
      server_send !verbose_msg_clients sock (create_key ());
    end;
  match c.client_all_files, client_type c with
  | Some _, _  (* we already have browsed this client *)
  | None, NormalClient ->
      if c.client_files = [] then
        server_send !verbose_msg_clients sock (DirectionReq { 
            Direction.direction = Upload;
            Direction.level = 666;
          }) else
        server_send !verbose_msg_clients sock (DirectionReq { 
            Direction.direction = Download;
            Direction.level = 666;
          })
  | _ ->
      server_send !verbose_msg_clients sock (DirectionReq { 
          Direction.direction = Download;
          Direction.level = 31666;
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
          Some sock -> lprintf "Already connected"; lprint_newline ();
            close sock "already connected";
            raise Not_found
            
        | None ->
            init_connection nick_sent c sock;
            Some c
      end
  | _ ->
      lprintf "BAD MESSAGE"; 
      print t; lprint_newline ();
      close sock "bad message";
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
          disconnect_client c;
          raise Not_found
        end;
  
  | LockReq lock ->
      server_send !verbose_msg_clients sock (
        KeyReq { Key.key = DcKey.gen lock.Lock.key });
      begin
        match client_type c, c.client_all_files with
        | (FriendClient | ContactClient), None ->

(* So, we cannot downlaod anything else from a friend ??? *)
            
            
            lprintf "TRY TO DOWNLOAD FILE LIST"; lprint_newline ();
            server_send !verbose_msg_clients sock (GetReq {
                Get.name = "MyList.DcLst";
                Get.pos = Int64.one;
              });
            c.client_pos <- Int64.zero;
            c.client_download <- DcDownloadList (Buffer.create 10000)
        
        | _ ->
            let rec iter_files files =
              match files with
              | [] -> lprintf "NO FILE TO UPLOAD"; lprint_newline ();
              
              | (file, filename) :: tail -> 
                  if file_state file = FileDownloading then begin
                      lprintf "GET: file downloaded %Ld"  (file_downloaded file);
                      lprint_newline ();
                      server_send !verbose_msg_clients sock (GetReq {
                          Get.name = filename;
                          Get.pos = Int64.add (file_downloaded file) Int64.one;
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
                disconnect_client c;
                raise Not_found
              end
        
        | DcDownloadList buf ->
            c.client_receiving <- t;
        
        | _ ->
            lprintf "Not downloading any thing"; lprint_newline ();
            disconnect_client c;
            raise Not_found
      end;
      server_send !verbose_msg_clients sock SendReq
  
  | GetReq t ->
(* this client REALLY wants to download from us !! *)
      lprintf "GET REQ"; lprint_newline ();
      
      if t.Get.name = "MyList.DcLst" then begin
          c.client_pos <- Int64.zero;
          let list = make_shared_list () in
          let list = Che3.compress list in
          c.client_download <- DcUploadList list;
          server_send !verbose_msg_clients sock (FileLengthReq (
              Int64.of_int (String.length list)))
        end else begin 
(* Upload not yet implemented *)
          try
            let sh = CommonUploads.find t.Get.name in
            c.client_pos <- Int64.sub t.Get.pos Int64.one;
            let rem = Int64.sub sh.shared_size c.client_pos in
            server_send !verbose_msg_clients sock (FileLengthReq rem);
            c.client_download <- DcUpload sh
          with _ ->
              ()
        end
  
  | SendReq ->
      c.client_pos <- Int64.zero;
      let refill sock =
        lprintf "FILL SOCKET"; lprint_newline ();
        let len = remaining_to_write sock in
        match c.client_download with
          DcUploadList list ->
            lprintf "DcUploadList"; lprint_newline ();
            let slen = String.length list in
            let pos = Int64.to_int c.client_pos in
            if pos < slen then begin
                let send_len = mini (slen - pos) (8192 - len) in
                lprintf "Sending %d" send_len; lprint_newline ();
                TcpBufferedSocket.write sock list pos send_len;
                lprintf "sent"; lprint_newline ();
                c.client_pos <- Int64.add c.client_pos (Int64.of_int send_len);
                
                if pos + len = slen then begin
(* Normally, the client should close the connection after the download,
but since we don't want a buggy client to keep this connection, just
close it after a long timeout. *)
                    set_lifetime sock 120.
                  end
              
              end 
        
        | DcUpload sh -> 
            lprintf "DcUpload"; lprint_newline ();            
            let slen = sh.shared_size in
            let pos = c.client_pos in
            if pos < slen then
              let rlen = 
                let rem = Int64.sub slen  pos in
                let can = 8192 - len in
                if rem > Int64.of_int can then can else Int64.to_int rem
              in
              let upload_buffer = String.create rlen in
              Unix32.read sh.shared_fd pos upload_buffer 0 rlen;
              TcpBufferedSocket.write sock upload_buffer 0 rlen;
              c.client_pos <- Int64.add c.client_pos (Int64.of_int rlen);
              if c.client_pos = slen then begin
(* Normally, the client should close the connection after the download,
but since we don't want a buggy client to keep this connection, just
close it after a long timeout. *)
                    set_lifetime sock 120. 
                end
          | _ -> assert false
      in
      set_refill sock refill;
      set_handler sock WRITE_DONE (fun sock -> 
          lprintf "CLOSE SOCK AFTER REFILL DONE"; lprint_newline ();
          close sock "write done")
        
  | _ ->
      lprintf "###UNUSED CLIENT MESSAGE###########"; lprint_newline ();
      DcProtocol.print t

let file_complete file = 
(*
  lprintf "FILE %s DOWNLOADED" f.file_name;
lprint_newline ();
  *)
  CommonComplexOptions.file_completed (as_file file.file_file);
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
        c.client_pos <- Int64.add c.client_pos (Int64.of_int b.len);
(*
      lprintf "NEW SOURCE POS %s" (Int64.to_string c.source_pos);
lprint_newline ();
  *)
        TcpBufferedSocket.buf_used sock b.len;
        if c.client_pos > (file_downloaded file) then begin
            add_file_downloaded file.file_file
            (Int64.sub c.client_pos (file_downloaded file));
          end;
        if (file_downloaded file) = (file_size file) then begin
            close sock "file downloaded";
            file_complete file 
          end
          
    | DcDownloadList buf ->
        lprintf "DcDownloadList"; lprint_newline ();
        let b = TcpBufferedSocket.buf sock in        
        let len = b.len in
        Buffer.add_substring buf b.buf b.pos b.len;
        buf_used sock b.len;
        c.client_receiving <- Int64.sub c.client_receiving (Int64.of_int len);
        lprintf "Received %d of List" len; lprint_newline ();
        close sock "file list received"; lprint_newline ();
        if c.client_receiving = Int64.zero then begin
            lprintf "----------------------------------------"; lprint_newline ();
            lprintf "RECEIVED COMPLETE FILE LIST "; lprint_newline ();
            lprintf "----------------------------------------"; lprint_newline ();
            
            let s = Buffer.contents buf in
            let s = Che3.decompress s in
            try
              lprintf "LIST: [%s]" (String.escaped s);
              lprint_newline (); 
              let files = parse_list c.client_user s in
              lprintf "PARSED"; lprint_newline (); 
              c.client_all_files <- Some files;
              List.iter (fun (dirname,r) ->
                  lprintf "NEW FILE in %s" dirname; lprint_newline (); 
                  client_new_file (as_client c.client_client) dirname
                    (as_result r.result_result)
              ) files;
              ()
            with e ->
                lprintf "Exception %s in parse client files"
                  (Printexc2.to_string e);
                ; lprint_newline ();
          end
    | _ -> assert false

let init_anon_client init_sent sock =
  
  TcpBufferedSocket.set_read_controler sock download_control;
  TcpBufferedSocket.set_write_controler sock upload_control;
  BasicSocket.set_rtimeout (TcpBufferedSocket.sock sock) 30.;
  
  let c = ref None in
  TcpBufferedSocket.set_closer sock (fun _ s ->
      lprintf "DISCONNECTED FROM CLIENT"; lprint_newline ();
      match !c with
        None -> ()
      | Some c ->  disconnect_client c
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
              lprintf "CONNECTION RECEIVED FROM %s FOR PUSH"
              (Ip.to_string (Ip.of_inet_addr from_ip))
              ; 
              lprint_newline ();
              
              
              let sock = TcpBufferedSocket.create
                  "DC client connection" s (fun _ _ -> ()) in
              init_anon_client false sock

          | _ -> ()
      ) in
    listen_sock := Some sock;
    ()
  with e ->
      lprintf "Exception %s while init DC server" 
        (Printexc2.to_string e);
      lprint_newline ()

let connect_client c =
  try
    match c.client_addr with
      None -> ()
    | Some (ip,port) ->
        connection_try c.client_connection_control;      
        let sock = connect "client download" 
            (Ip.to_inet_addr ip) port
            (fun sock event ->
              match event with
              | BASIC_EVENT (RTIMEOUT | LTIMEOUT) ->
                  disconnect_client c
              | BASIC_EVENT (CLOSED _) ->
                  disconnect_client c
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
      lprintf "Exception %s while connecting to client" 
        (Printexc2.to_string e);
      lprint_newline ();
      disconnect_client c

let connect_anon s ip port =
  lprintf "CONNECT ANON"; lprint_newline ();
  try
    let sock = connect "client download" 
        (Ip.to_inet_addr ip) port
        (fun _ _ -> ())
    in
    init_anon_client true sock;
    server_send !verbose_msg_clients sock (MyNickReq s.server_last_nick);
    server_send !verbose_msg_clients sock (
      create_key ());
          
  with e ->
      lprintf "Exception %s while connecting to  anon client" 
        (Printexc2.to_string e);
      lprint_newline ()
      
