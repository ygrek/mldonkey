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
open CommonFile
open CommonComplexOptions
open CommonTypes
open OpennapComplexOptions
open BasicSocket
open TcpBufferedSocket
open Options
open OpennapTypes
open OpennapOptions
open CommonGlobals
open OpennapGlobals
  
module OP = OpennapProtocol
module OG = OpennapGlobals
module DO = CommonOptions  
module DG = CommonGlobals

let disconnect_client c =
  match c.client_sock with
    None -> ()
  | Some sock -> close sock "client disconnected"
      
let client_handler c sock event = 
  match event with
    BASIC_EVENT (CLOSED s) ->      
(*      Printf.printf "CONNECTION WITH CLIENT LOST (%s)" s; print_newline (); *)
      disconnect_client c
  | BASIC_EVENT (RTIMEOUT|LTIMEOUT) ->
      disconnect_client c
  | _ -> ()


let rec remove_download file list =
  match list with
    [] -> []
  | f :: tail ->
      if f == file then tail else f :: (remove_download file tail)
      
let file_complete file =
(*  Printf.printf "FILE %s DOWNLOADED" f.file_name;
  print_newline (); *)
  (try file_completed (as_file file.file_file)
    with e ->
        Printf.printf "Exception %s in file completed"
        (Printexc2.to_string e)
        ; print_newline ());
  current_files := List2.removeq file !current_files;
  old_files =:= (file.file_name, file_size file) :: !!old_files;
  List.iter (fun c ->
      c.client_files <- List.remove_assoc file c.client_files
  ) file.file_clients;

  (*
(* finally move file *)
  let incoming_dir =
    if !!commit_in_subdir <> "" then
      Filename.concat !!DO.incoming_directory !!commit_in_subdir
    else !!DO.incoming_directory
  in
  (try Unix2.safe_mkdir incoming_dir with _ -> ());
  let new_name =  Filename.concat incoming_dir 
    (canonize_basename file.file_name)
  in
(*  Printf.printf "RENAME to %s" new_name; print_newline (); *)
  let new_name = rename_to_incoming_dir 
    (file_disk_name file)  new_name in
  set_file_disk_name file new_name
*)
  ()
(*

5.2  Firwalled Downloading

As described above, when the file needs to be pushed from a client behind a
firewall, the downloader sends a 500 message to the server.  This causes a
501 message to be sent to the uploader, which is similar to the 204 message
for a normal download.

Once the uploader receives the 501 message from the server, they should make
a TCP connection to the downloader's data port (given in the 501 message).
Upon connection, the downloader's client will sent one byte, the ASCII
character `1'.  The uploader should then send the string "SEND" in a single
packet, and then the information:
	<mynick> "<filename>" <size>
where <mynick> is the uploader's napster user name, <filename> is the file
being sent, and <size> is the size of the file in bytes.

Upon receipt, the downloading client will either send the byte offset at
whcih the transfer should start, or an error message such as
"INVALID REQUEST".  The byte offset should be sent as a single packet
in plain ASCII digits.  Just as with above in section 4.1, a 0 byte offset
indicates the transfer should begin at the start of the file.

Each client should notify the server that they are uploading or downloading
with the 218/219 (downloading) or 220/221 (uploading) command pairs (see
section 4.1 for more detailed information).

*)
  
let index_sub s pos len c =
  let find = String.index_from s pos c in
  if find >= pos + len then raise Not_found;
  find
  
let read_stream c file sock b =
  print_char '.'; flush stdout;
  begin
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
    if c.client_pos > file_downloaded file then begin
        file.file_file.impl_file_downloaded <- c.client_pos;
        file_must_update (as_file file.file_file)
      end;
    if file_downloaded file = file_size file then
      
      file_complete file             
  end
  
let client_reader c =
  let state = ref 0 in
  let rec iter sock nread =
(*    Printf.printf "CLIENT READER %d BYTES" nread; print_newline (); *)
    if nread > 0 then
      let b = buf sock in
      if !state = 0 then (* waiting for 1 *) begin
          if b.buf.[b.pos] = '1' then begin
              
(*              Printf.printf "1 RECEIVED"; print_newline (); *)
              state := 1;
              buf_used sock 1;            
              write_string sock "GET";
              match c.client_files with
                (file, filename) :: _ ->
                  c.client_file <- Some file;
                  connection_ok c.client_connection_control;
                  let s =  (Printf.sprintf "%s \"%s\" %ld"
                        (match c.client_user.user_servers with
                          [] -> !!CommonOptions.client_name
                        | s :: _ -> s.server_last_nick) filename 
                        (file_downloaded file)) in
                  write_string sock s;
                  c.client_pos <- file_downloaded file;
                  iter sock (nread - 1)
              | _ -> 
                  Printf.printf "No file or source"; print_newline ();
                  disconnect_client c
            end
          else begin
              Printf.printf "bad non 1 reply"; print_newline ();
              disconnect_client c
            end
        end
      else
      if !state = 1 then (* waiting for length *) begin
(*          Printf.printf "RECEIVED [%s]" (String.escaped 
              (String.sub b.buf b.pos (min b.len 20)));
          print_newline ();
*)
          
          let pos = b.pos + b.len - nread in
          let rec find_end pos =
            if pos - b.pos = b.len then -1 else
            match b.buf.[pos] with
              '0' .. '9' -> find_end (pos+1)
            | _ -> pos
          in
          let pos_end = find_end pos in
          if pos_end >= 0 then
            let len = pos_end - b.pos in
            let size = String.sub b.buf b.pos len in
            buf_used sock len;            
(*            Printf.printf "SIZE READ : [%s]" size; print_newline ();*)
            let total_size = Int32.of_string size in
            state := 2;
            iter sock (nread - len)
        end else 
      match c.client_file with
        None -> close sock "not downloading"
      | Some file -> read_stream c file sock b
  in
  iter 
  
let client_reader2 c sock nread =
(*    Printf.printf "CLIENT READER %d BYTES" nread; print_newline ();  *)
  if nread = 0 then () else
  let b = buf sock in
  set_rtimeout sock half_day;
  match !c with
    None -> (* waiting for SENDnick "filename" size *) 
      begin
        Printf.printf "RECEIVED [%s]" 
        (String.escaped (String.sub b.buf b.pos (min b.len 300)));
        print_newline ();
        try
          let space = index_sub b.buf b.pos b.len ' ' in
            Printf.printf "SPACE FOUND AT %d" space; print_newline (); 
          let quote = index_sub b.buf space (b.len - space) '"' in
            Printf.printf "QUOTE FOUND AT %d" quote; print_newline (); 
          if space+1 <> quote then failwith "BAD SPACE"; 
          let quote2 = index_sub b.buf (quote+1) (b.len - quote - 1) '"' in
            Printf.printf "QUOTE2 FOUND AT %d" quote2; print_newline (); 
          let nick = String.sub b.buf b.pos (space - b.pos) in
            Printf.printf "nick ok"; print_newline (); 
          let file_name = String.sub b.buf (quote+1) (quote2 - quote - 1) in
            Printf.printf "name ok"; print_newline (); 
          let size = String.sub b.buf (quote2+2) (b.len - quote2 - 2) in
            Printf.printf "FROM [%s] FILE [%s] SIZE [%s]" nick file_name size;
            print_newline (); 
          
          buf_used sock b.len;
          
          let file_name = OpennapGlobals.basename file_name in
          let file = OpennapGlobals.find_file 
              file_name (Int32.of_string size) in
          
          List.iter (fun cc ->
              if cc.client_name = nick then 
                match cc.client_sock with
                  None ->
                    cc.client_file <- Some file;
                    cc.client_pos <- file_downloaded file;
                    write_string sock (Int32.to_string cc.client_pos);
                    cc.client_sock <- Some sock;
                    c := Some cc
                | Some sock ->
                    close sock "already connected";
                    raise Not_found
          ) file.file_clients
        
        with e -> 
            write_string sock "INVALID REQUEST";  
            Printf.printf "EXCEPTION %s in client_reader2" (Printexc2.to_string e); print_newline ();
      
      
      end
  | Some c ->
      match c.client_file with
        None -> close sock "not downloading"
      | Some file ->
          read_stream c file sock b
          
let connect_client c =
  match c.client_addr with
    None -> assert false
  | Some (ip, port) ->
      c.client_file <- None;
(*      Printf.printf "TRYING TO CONNECT CLIENT ON %s:%d" 
(Ip.to_string ip) port; print_newline (); *)
      let sock = TcpBufferedSocket.connect "opennap to client" (
          Ip.to_inet_addr ip) port 
          (client_handler c)  in
      c.client_sock <- Some sock;
      set_read_controler sock DG.download_control;
      set_write_controler sock DG.upload_control;
      
      set_reader sock (client_reader c);
      set_rtimeout sock 30.
      
      
      
let listen () =
  try
    let sock = TcpServerSocket.create "opennap client server" 
        Unix.inet_addr_any
        !!client_port
        (fun sock event ->
          match event with
            TcpServerSocket.CONNECTION (s, 
              Unix.ADDR_INET(from_ip, from_port)) ->
(*
              Printf.printf "CONNECTION RECEIVED FROM %s FOR PUSH"
                (Ip.to_string (Ip.of_inet_addr from_ip))
              ; 
              print_newline ();
              
              Printf.printf "INDIRECT CONNECTION !!!!"; print_newline ();
*)
              let c = ref None in
              let sock = TcpBufferedSocket.create
                  "opennap client connection" s (fun _ _ -> ()) in
              TcpBufferedSocket.set_read_controler sock download_control;
              TcpBufferedSocket.set_write_controler sock upload_control;
              
              TcpBufferedSocket.set_closer sock (fun _ s ->
                  match !c with
                    None -> ()
                  | Some c ->
                      disconnect_client c
              );
              BasicSocket.set_rtimeout (TcpBufferedSocket.sock sock) 30.;
              
              write_string sock "1";
              
              TcpBufferedSocket.set_reader sock (client_reader2 c)
          | _ -> ()
      ) in
    listen_sock := Some sock;
    ()
  with e ->
      Printf.printf "Exception %s while init limewire server" 
        (Printexc2.to_string e);
      print_newline ()
      