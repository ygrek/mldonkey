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
      Printf.printf "CLOSE SOCKET"; print_newline ();
      close sock "client close";
      set_client_state c NotConnected;
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
  c.client_receiving <- Int32.zero;
  c.client_sock <- Some sock;
  connection_ok c.client_connection_control;  
  if not nick_sent then begin
      let my_nick = match c.client_user.user_servers with
          [] -> !!client_name
        | s :: _ -> s.server_last_nick 
      in
      debug_server_send sock (MyNickReq my_nick);
      debug_server_send sock (create_key ());
    end;
  match c.client_all_files, client_type c with
  | Some _, _  (* we already have browsed this client *)
  | None, NormalClient ->
      if c.client_files = [] then
        debug_server_send sock (DirectionReq { 
            Direction.direction = Upload;
            Direction.level = 666;
          }) else
        debug_server_send sock (DirectionReq { 
            Direction.direction = Download;
            Direction.level = 666;
          })
  | _ ->
      debug_server_send sock (DirectionReq { 
          Direction.direction = Download;
          Direction.level = 31666;
        })      


      
let read_first_message nick_sent t sock =
  Printf.printf "FIRST MESSAGE"; print_newline ();
  print t;
  match t with 
    MyNickReq n ->
      begin
        let c = new_client n in
        match c.client_sock with
          Some sock -> Printf.printf "Already connected"; print_newline ();
            close sock "already connected";
            raise Not_found
            
        | None ->
            init_connection nick_sent c sock;
            Some c
      end
  | _ ->
      Printf.printf "BAD MESSAGE"; 
      print t; print_newline ();
      close sock "bad message";
      raise Not_found
  
let client_reader c t sock =
  Printf.printf "FROM CLIENT %s" c.client_name; print_newline ();
  print t;
  match t with
    MyNickReq n ->
      connection_ok c.client_connection_control;
      if c.client_name != n then begin
          Printf.printf "Bad nickname for client %s/%s" n c.client_name; 
          print_newline ();
          disconnect_client c;
          raise Not_found
        end;
  
  | LockReq lock ->
      debug_server_send sock (
        KeyReq { Key.key = DcKey.gen lock.Lock.key });
      begin
        match client_type c, c.client_all_files with
        | (FriendClient | ContactClient), None ->
            Printf.printf "TRY TO DOWNLOAD FILE LIST"; print_newline ();
            debug_server_send sock (GetReq {
                Get.name = "MyList.DcLst";
                Get.pos = Int32.one;
              });
            c.client_pos <- Int32.zero;
            c.client_download <- DcDownloadList (Buffer.create 10000)
        
        | _ ->
            match c.client_files with
              [] -> Printf.printf "NO FILE TO UPLOAD"; print_newline ();
            
            | (file, filename) :: _ -> 
                debug_server_send sock (GetReq {
                    Get.name = filename;
                    Get.pos = Int32.add (file_downloaded file) Int32.one;
                  });
                c.client_download <- DcDownload file;
                c.client_pos <- (file_downloaded file);
      end
  
  | KeyReq _ ->
      Printf.printf "DISCARD KEY ..."; print_newline ();
  
  | DirectionReq t ->
(* HERE, we should check for upload slots ...*)
      if t.Direction.direction = Download then begin
(* this client wants something from us ... *)

(* is this OK (we sent two Direction messages !) ? *)
          debug_server_send sock (DirectionReq { 
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
                Printf.printf "Bad file size: %ld  <> %ld"
                  t
                   (file_size file);
                print_newline ();
                disconnect_client c;
                raise Not_found
              end
        
        | DcDownloadList buf ->
            c.client_receiving <- t;
        | _ ->
            Printf.printf "Not downloading any thing"; print_newline ();
            disconnect_client c;
            raise Not_found
      end;
      debug_server_send sock SendReq
  
  | GetReq t ->
(* this client REALLY wants to download from us !! *)
      if t.Get.name = "MyList.DcLst" then begin
          c.client_pos <- Int32.zero;
          let list = make_shared_list () in
          let list = Che3.compress list in
          c.client_download <- DcUploadList list;
          debug_server_send sock (FileLengthReq (
              Int32.of_int (String.length list)))
        end else begin 
(* Upload not yet implemented *)
          try
            let sh = Hashtbl.find shared_files t.Get.name in
            c.client_pos <- Int32.sub t.Get.pos Int32.one;
            let rem = Int32.sub sh.shared_size c.client_pos in
            debug_server_send sock (FileLengthReq rem);
            c.client_download <- DcUpload sh
          with _ ->
              ()
        end
  
  | SendReq ->
      let refill sock =
        Printf.printf "FILL SOCKET"; print_newline ();
        let len = remaining_to_write sock in
        if len < 8192 then
          match c.client_download with
            DcUploadList list ->
              let slen = String.length list in
              let pos = Int32.to_int c.client_pos in
              if pos < slen then
                TcpBufferedSocket.write sock list pos (mini (slen - pos) (8192 - len))
          | DcUpload sh -> 
              let slen = sh.shared_size in
              let pos = c.client_pos in
              if pos < slen then
                let fd = sh.shared_fd in
                ignore (Unix32.seek32 fd pos Unix.SEEK_SET);
                let rlen = 
                  let rem = Int32.sub slen  pos in
                  let can = 8192 - len in
                  if rem > Int32.of_int can then can else Int32.to_int rem
                in
                let upload_buffer = String.create rlen in
                Unix2.really_read (Unix32.force_fd fd) upload_buffer 0 rlen;
                TcpBufferedSocket.write sock upload_buffer 0 rlen
          | _ -> assert false
      in
      set_refill sock refill;
      set_handler sock WRITE_DONE (fun sock -> close sock "write done")
        
  | _ ->
      Printf.printf "###UNUSED CLIENT MESSAGE###########"; print_newline ();
      DcProtocol.print t

     (*
let save_file_as file filename =

(* finally move file *)
  let incoming_dir =
    if !!commit_in_subdir <> "" then
      Filename.concat !!incoming_directory !!commit_in_subdir
    else !!incoming_directory
  in
  (try Unix2.safe_mkdir incoming_dir with _ -> ());
  let new_name = 
    Filename.concat incoming_dir (canonize_basename file.file_name)
  in
  try
    Printf.printf "*******  RENAME %s to %s *******" 
      (file_disk_name file) new_name; print_newline ();
    let new_name = rename_to_incoming_dir 
      (file_disk_name file)  new_name in
    Printf.printf "*******  RENAME %s to %s DONE *******" 
      (file_disk_name file) new_name; print_newline ();
    set_file_disk_name file new_name
  with e ->
      Printf.printf "Exception %s in rename" (Printexc2.to_string e);
      print_newline () 
      *)

let file_complete file = 
(*
  Printf.printf "FILE %s DOWNLOADED" f.file_name;
print_newline ();
  *)
  CommonComplexOptions.file_completed (as_file file.file_file);
  current_files := List2.removeq file !current_files;
(*  old_files =:= (f.file_name, f.file_size) :: !!old_files; *)
  List.iter (fun c ->
      c.client_files <- List.remove_assoc file c.client_files
  ) file.file_clients
  
let client_downloaded c sock nread = 
  Printf.printf "."; flush stdout; 
  if nread > 0 then
    match c.client_download with
    | DcDownload file ->
        let b = TcpBufferedSocket.buf sock in
        set_rtimeout sock half_day;
        begin
          let fd = try
              Unix32.force_fd (file_fd file) 
            with e -> 
                Printf.printf "In Unix32.force_fd"; print_newline ();
                raise e
          in
          let final_pos = Unix32.seek32 (file_fd file) c.client_pos Unix.SEEK_SET in
          Unix2.really_write fd b.buf b.pos b.len;
        end;
(*      Printf.printf "DIFF %d/%d" nread b.len; print_newline ();*)
        c.client_pos <- Int32.add c.client_pos (Int32.of_int b.len);
(*
      Printf.printf "NEW SOURCE POS %s" (Int32.to_string c.source_pos);
print_newline ();
  *)
        TcpBufferedSocket.buf_used sock b.len;
        if c.client_pos > (file_downloaded file) then begin
            file.file_file.impl_file_downloaded <- c.client_pos;
            file_must_update file;
          end;
        if (file_downloaded file) = (file_size file) then
          file_complete file 
          
    | DcDownloadList buf ->
        let b = TcpBufferedSocket.buf sock in        
        let len = b.len in
        Buffer.add_substring buf b.buf b.pos b.len;
        buf_used sock b.len;
        c.client_receiving <- Int32.sub c.client_receiving (Int32.of_int len);
        if c.client_receiving = Int32.zero then begin
            (*
            Printf.printf "----------------------------------------"; print_newline ();
            Printf.printf "RECEIVED COMPLETE FILE LIST "; print_newline ();
Printf.printf "----------------------------------------"; print_newline ();
  *)
            let s = Buffer.contents buf in
            let s = Che3.decompress s in
            try
(*              Printf.printf "LIST: [%s]" (String.escaped s);
              print_newline (); *)
              let files = parse_list c.client_user s in
(*              Printf.printf "PARSED"; print_newline (); *)
              c.client_all_files <- Some files;
              List.iter (fun (dirname,r) ->
(*                  Printf.printf "NEW FILE"; print_newline (); *)
                  client_new_file (as_client c.client_client) dirname
                    (as_result r.result_result)
              ) files;
              ()
            with e ->
                Printf.printf "Exception %s in parse client files"
                  (Printexc2.to_string e);
                ; print_newline ();
          end
    | _ -> assert false

let init_anon_client init_sent sock =
  
  TcpBufferedSocket.set_read_controler sock download_control;
  TcpBufferedSocket.set_write_controler sock upload_control;
  BasicSocket.set_rtimeout (TcpBufferedSocket.sock sock) 30.;
  
  let c = ref None in
  TcpBufferedSocket.set_closer sock (fun _ s ->
      Printf.printf "DISCONNECTED FROM CLIENT"; print_newline ();
      match !c with
        None -> ()
      | Some c ->  disconnect_client c
  );
  TcpBufferedSocket.set_reader sock (
    dc_handler3 c (read_first_message init_sent) client_reader
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
              Printf.printf "CONNECTION RECEIVED FROM %s FOR PUSH"
              (Ip.to_string (Ip.of_inet_addr from_ip))
              ; 
              print_newline ();
              
              
              let sock = TcpBufferedSocket.create
                  "DC client connection" s (fun _ _ -> ()) in
              init_anon_client false sock

          | _ -> ()
      ) in
    listen_sock := Some sock;
    ()
  with e ->
      Printf.printf "Exception %s while init DC server" 
        (Printexc2.to_string e);
      print_newline ()

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
                BASIC_EVENT RTIMEOUT ->
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
          dc_handler3 (ref (Some c)) (read_first_message false) client_reader
            client_downloaded);
        
        init_connection false c sock;
          
  with e ->
      Printf.printf "Exception %s while connecting to client" 
        (Printexc2.to_string e);
      print_newline ();
      disconnect_client c

let connect_anon s ip port =
  Printf.printf "CONNECT ANON"; print_newline ();
  try
    let sock = connect "client download" 
        (Ip.to_inet_addr ip) port
        (fun _ _ -> ())
    in
    init_anon_client true sock;
    debug_server_send sock (MyNickReq s.server_last_nick);
    debug_server_send sock (
      create_key ());
          
  with e ->
      Printf.printf "Exception %s while connecting to  anon client" 
        (Printexc2.to_string e);
      print_newline ()
      
