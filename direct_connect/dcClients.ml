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
open CommonOptions
open TcpBufferedSocket
open CommonGlobals
open CommonFile
open CommonUser
open CommonChatRoom
open CommonServer
open CommonResult
open CommonTypes
open CommonSearch
open Options
open DcTypes
open DcOptions
open DcGlobals
open DcProtocol

let client_close c = 
  match c.client_sock with
    None -> ()
  | Some sock ->
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
  key
        
let init_connection nick_sent c sock =
  c.client_receiving <- Int32.zero;
  c.client_sock <- Some sock;
  if not nick_sent then begin
      let my_nick = match c.client_server with
          None -> !!client_name
        | Some s -> s.server_last_nick 
      in
      server_send sock (MyNickReq my_nick);
      server_send sock (
        LockReq { Lock.info = ""; Lock.key = create_key ()});
    end;
  if c.client_files = [] then
    server_send sock (DirectionReq { 
        Direction.download = false;
        Direction.level = 666;
      }) else
    server_send sock (DirectionReq { 
        Direction.download = true;
        Direction.level = 666;
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
      if c.client_name != n then begin
          Printf.printf "Bad nickname for client %s/%s" n c.client_name; 
          print_newline ();
          client_close c;
          raise Not_found
        end;
  
  | LockReq lock ->
      server_send sock (
        KeyReq { Key.key = DcKey.gen lock.Lock.key });
      begin
        match client_type c, c.client_all_files with
        | (FriendClient | ContactClient), None ->
            server_send sock (GetReq {
                Get.name = "MyList.DcLst";
                Get.pos = Int32.one;
              });
            c.client_pos <- Int32.zero;
            c.client_download <- DcDownloadList (Buffer.create 10000)
        
        | _ ->
            match c.client_files with
              [] -> Printf.printf "NO FILE TO UPLOAD"; print_newline ();
            
            | (file, filename) :: _ -> 
                server_send sock (GetReq {
                    Get.name = filename;
                    Get.pos = Int32.add file.file_downloaded Int32.one;
                  });
                c.client_download <- DcDownload file;
                c.client_pos <- file.file_downloaded;
      end
  
  | KeyReq _ ->
      Printf.printf "DISCARD KEY ..."; print_newline ();
  
  | DirectionReq t ->
      (* HERE, we should check for upload slots ...
      if t.Direction.download then begin
          Printf.printf "UPLOAD NOT IMPLEMENTED"; print_newline ();
          client_close c;
          raise Not_found
        end;
      Printf.printf "GOOD DIRECTION: %d" t.Direction.level; print_newline ();
*)
      ()
      
  | FileLengthReq t ->
      begin
        match c.client_download with
          DcDownload file ->
            if Int32.add c.client_pos t = file.file_size then begin
                c.client_receiving <- t;
              end else begin
                Printf.printf "Bad file size: %ld + %ld = %ld <> %ld"
                  c.client_pos  t
                (Int32.add c.client_pos t)  file.file_size;
                print_newline ();
                client_close c;
                raise Not_found
              end
        
        | DcDownloadList buf ->
            c.client_receiving <- t;
        | _ ->
            Printf.printf "Not downloading any thing"; print_newline ();
            client_close c;
            raise Not_found
      end
  
  | _ ->
      Printf.printf "###UNUSED CLIENT MESSAGE###########"; print_newline ();
      DcProtocol.print t

      
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
  ) file.file_clients;
  
(* finally move file *)
  let incoming_dir =
    if !!commit_in_subdir <> "" then
      Filename.concat !!DO.incoming_directory !!commit_in_subdir
    else !!DO.incoming_directory
  in
  (try Unix2.safe_mkdir incoming_dir with _ -> ());
  let new_name = 
    Filename.concat incoming_dir file.file_name
  in
(*  Printf.printf "RENAME to %s" new_name; print_newline ();*)
  Unix2.rename file.file_temp  new_name
  
let client_downloaded c sock nread = 
  Printf.printf "----------------------------------------"; print_newline ();
  Printf.printf "CLIENT RECEIVE STREAM !!!!!!!!!!!!!!!!!!"; print_newline ();
  Printf.printf "----------------------------------------"; print_newline ();
  if nread > 0 then
    match c.client_download with
    | DcDownload file ->
        let b = TcpBufferedSocket.buf sock in
        set_rtimeout sock half_day;
        begin
          let fd = try
              Unix32.force_fd file.file_fd 
            with e -> 
                Printf.printf "In Unix32.force_fd"; print_newline ();
                raise e
          in
          let final_pos = Unix32.seek32 file.file_fd c.client_pos Unix.SEEK_SET in
          Unix2.really_write fd b.buf b.pos b.len;
        end;
(*      Printf.printf "DIFF %d/%d" nread b.len; print_newline ();*)
        c.client_pos <- Int32.add c.client_pos (Int32.of_int b.len);
(*
      Printf.printf "NEW SOURCE POS %s" (Int32.to_string c.source_pos);
print_newline ();
  *)
        TcpBufferedSocket.buf_used sock b.len;
        if c.client_pos > file.file_downloaded then begin
            file.file_downloaded <- c.client_pos;
            file_must_update file;
          end;
        if file.file_downloaded = file.file_size then
          file_complete file 
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
      | Some c ->  client_close c
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
    ()
  with e ->
      Printf.printf "Exception %s while init DC server" 
        (Printexc.to_string e);
      print_newline ()

let connect_client c =
  try
    match c.client_addr with
      None -> ()
    | Some (ip,port) ->
        let sock = connect "client download" 
            (Ip.to_inet_addr ip) port
            (fun sock event ->
              match event with
                BASIC_EVENT RTIMEOUT ->
                  client_close c
              | BASIC_EVENT (CLOSED _) ->
                  client_close c
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
        (Printexc.to_string e);
      print_newline ();
      client_close c

let connect_anon s ip port =
  Printf.printf "CONNECT ANON"; print_newline ();
  try
    let sock = connect "client download" 
        (Ip.to_inet_addr ip) port
        (fun _ _ -> ())
    in
    init_anon_client true sock;
    server_send sock (MyNickReq s.server_last_nick);
    server_send sock (
      LockReq { Lock.info = ""; Lock.key = create_key ()});
    
          
  with e ->
      Printf.printf "Exception %s while connecting to  anon client" 
        (Printexc.to_string e);
      print_newline ()
      
