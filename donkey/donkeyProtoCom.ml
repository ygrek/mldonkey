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

open CommonFile
open CommonGlobals
open DonkeyOptions
open DonkeyGlobals
open DonkeyTypes
open CommonTypes
open Options
open CommonGlobals
open LittleEndian
open DonkeyMftp
open BasicSocket
open TcpBufferedSocket
open CommonOptions
  
let buf = TcpBufferedSocket.internal_buf

        
let client_msg_to_string magic msg =
  Buffer.clear buf;
  buf_int8 buf magic;
  buf_int buf 0;
  DonkeyProtoClient.write buf msg;

  (*
  if !verbose_msg_clients then begin
      Printf.printf "MESSAGE TO CLIENT:";  print_newline (); 
      DonkeyProtoClient.print msg; 
      print_newline ();
    end;
*)  
  let s = Buffer.contents buf in
  let len = String.length s - 5 in
  str_int s 1 len;
  s
  
let server_msg_to_string msg =
  Buffer.clear buf;
  buf_int8 buf 227;
  buf_int buf 0;
  DonkeyProtoServer.write buf msg;
  
  
  if !verbose_msg_servers then begin
      Printf.printf "MESSAGE TO SERVER:";  print_newline (); 
      DonkeyProtoServer.print msg; 
      print_newline ();
    end;

  let s = Buffer.contents buf in
  let len = String.length s - 5 in
  str_int s 1 len;
  s

let server_send sock m =
(*
  Printf.printf "Message to server"; print_newline ();
  DonkeyProtoServer.print m;
*)
  write_string sock (server_msg_to_string m)

let direct_client_sock_send sock m =
  write_string sock (client_msg_to_string 227 m)
  
let client_send c m =
  if !verbose_msg_clients then begin
      Printf.printf "Received from client %s(%s)"
        c.client_name (brand_to_string c.client_brand);
      (match c.client_kind with
          Indirect_location _ -> ()
        | Known_location (ip,port) ->
            Printf.printf " [%s:%d]" (Ip.to_string ip) port;
      );
      print_newline ();
      DonkeyProtoClient.print m;
      print_newline ();
    end;
  match c.client_sock with
    None -> ()
  | Some sock ->
      direct_client_sock_send sock m

let emule_send sock m =
  let m = client_msg_to_string 0xc5 m in
  (*
  Printf.printf "Message to emule client:"; print_newline ();
  LittleEndian.dump m;
  print_newline ();
  print_newline (); *)
  write_string sock m

let client_msg_to_string m = client_msg_to_string 227 m
  
let servers_send socks m =
  let m = server_msg_to_string m in
  List.iter (fun s -> write_string s m) socks
  
      
let client_handler2 c ff f =
  let msgs = ref 0 in
  fun sock nread ->

    if !verbose then begin
        Printf.printf "between clients %d" nread; 
        print_newline ();
      end;
    let module M= DonkeyProtoClient in
    let b = TcpBufferedSocket.buf sock in
    try
      while b.len >= 5 do
        let opcode = get_int8 b.buf b.pos in
        let msg_len = get_int b.buf (b.pos+1) in
        if b.len >= 5 + msg_len then
          begin
            if !verbose then begin
                Printf.printf "client_to_client"; 
                print_newline ();
              end;
            let s = String.sub b.buf (b.pos+5) msg_len in
            TcpBufferedSocket.buf_used sock  (msg_len + 5);
            let t = M.parse opcode s in
(*          M.print t;   
print_newline (); *)
            incr msgs;
            match !c with
              None -> c := ff t sock
            | Some c -> f c t sock
          end
        else raise Not_found
      done
    with Not_found -> ()
  
let cut_messages parse f sock nread =
  if !verbose then begin
      Printf.printf "server to client %d" nread; 
      print_newline ();
    end;

  let b = TcpBufferedSocket.buf sock in
  try
    while b.len >= 5 do
      let opcode = get_int8 b.buf b.pos in
      let msg_len = get_int b.buf (b.pos+1) in
      if b.len >= 5 + msg_len then
        begin
          if !verbose then begin
              Printf.printf "server_to_client"; 
              print_newline ();
            end;
          let s = String.sub b.buf (b.pos+5) msg_len in
          TcpBufferedSocket.buf_used sock (msg_len + 5);
          let t = parse opcode s in
          f t sock
        end
      else raise Not_found
    done
  with Not_found -> ()

let udp_send t ip port msg =
  try
  Buffer.clear buf;
  buf_int8 buf 227;
  DonkeyProtoServer.udp_write buf msg;
  let s = Buffer.contents buf in
  UdpSocket.write t s ip port
  with e ->
      Printf.printf "Exception %s in udp_send" (Printexc2.to_string e);
      print_newline () 

      (*
let udp_send_if_possible t bc addr msg =
  try
    Buffer.clear buf;
    buf_int8 buf 227;
    DonkeyProtoServer.udp_write buf msg;
    let s = Buffer.contents buf in
    let len = String.length s in
    (*Printf.printf "possible ?"; print_newline ();*)
    if if_possible bc len then
      UdpSocket.write t s 0 (String.length s) addr
  with e ->
      Printf.printf "Exception %s in udp_send" (Printexc2.to_string e);
      print_newline () 
      *)

let udp_handler f sock event =
  let module M = DonkeyProtoServer in
  match event with
    UdpSocket.READ_DONE ->
      UdpSocket.read_packets sock (fun p -> 
          try
            let pbuf = p.UdpSocket.content in
            let len = String.length pbuf in
            if len = 0 || 
              int_of_char pbuf.[0] <> 227 then begin
                if !verbose_unknown_messages then begin
                    Printf.printf "Received unknown UDP packet"; print_newline ();
                    dump pbuf;
                  end
              end else begin
                let t = M.parse 227 (String.sub pbuf 1 (len-1)) in
(*              M.print t; *)
                f t p
              end
          with e ->
              Printf.printf "Error %s in udp_handler"
                (Printexc2.to_string e); print_newline () 
      ) ;
  | _ -> ()

let propagation_socket = UdpSocket.create_sendonly ()

let counter = ref 1
  
(* Learn how many people are using mldonkey at a current time, and which 
servers they are connected to --> build a database of servers

Now, get some more information:
- Which version do they use ?
- How much data is shared ?

Note that the exact content/type/name of the files is not sent, nor
any private information. Just for statistics. Can be disabled in the 
  downloads.ini config file anyway.
*)
  
let propagate_working_servers servers peers =
  if !!DonkeyOptions.propagate_servers then begin
      decr counter;
      if !counter = 0 then begin
          counter := 6;
          try
            Buffer.clear buf;
            buf_int8 buf DonkeyOpenProtocol.udp_magic; (* open protocol *)
            buf_int8 buf 0;    
            let ip = Ip.my () in
            buf_ip buf ip; (* The client IP *)

(* The server IPs *)
            buf_list buf_peer buf servers; (* The servers he is connected to *)

(* Some statistics on the network *)
            buf_string buf Autoconf.current_version;
            buf_int buf (last_time () - start_time); (* uptime in sec *)
            let module S = CommonShared in
            let total_shared = ref Int64.zero in
            let total_uploaded = ref Int64.zero in
            
            S.shared_iter (fun s ->
                let i = S.as_shared_impl s in
                total_uploaded := 
                Int64.add !total_uploaded i.S.impl_shared_uploaded;
                total_shared := 
                Int64.add !total_shared i.S.impl_shared_size
            );

            buf_int64 buf !total_shared;
            buf_int64 buf !total_uploaded;
            
(* Overnet peers *)
	    buf_int buf (List.length peers);
            List.iter (fun (ip,port) -> 
                buf_ip buf ip; buf_int16 buf port) peers;

(* Statistics for Supernode creation *)
	    buf_int16 buf !!max_hard_upload_rate;
	    buf_int16 buf !!max_hard_download_rate;
	    buf_int buf (compute_lost_byte upload_control);
	    buf_int buf (compute_lost_byte download_control);

            let s = Buffer.contents buf in    
            let name, port = !!redirector in
            UdpSocket.write propagation_socket s (Ip.from_name name) port
          with e ->
              Printf.printf "Exception %s in udp_sendonly" (Printexc2.to_string e);
              print_newline () 
        end      
    end
    
let udp_basic_handler f sock event =
  match event with
    UdpSocket.READ_DONE ->
      UdpSocket.read_packets sock (fun p -> 
          try
            let pbuf = p.UdpSocket.content in
            let len = String.length pbuf in
            if len = 0 || 
              int_of_char pbuf.[0] <> DonkeyOpenProtocol.udp_magic then begin
                if !verbose_unknown_messages then begin
                    Printf.printf "Received unknown UDP packet"; print_newline ();
                    dump pbuf;
                  end;
              end else begin
                let t = String.sub pbuf 1 (len-1) in
                f t p
              end
          with e ->
              Printf.printf "Error %s in udp_basic_handler"
                (Printexc2.to_string e); print_newline () 
      ) ;
  | _ -> ()


let new_string msg s =
  let len = String.length s - 5 in
  str_int s 1 len  
  
let empty_string = ""
  
let direct_servers_send s msg =
  servers_send s msg
  
let direct_client_send s msg =
  client_send s msg
  
let direct_server_send s msg =
  server_send s msg

let tag_file file =
  (string_tag "filename"
    (
      let name = file_best_name file in
      let name = if String2.starts_with name "hidden." then
          String.sub name 7 (String.length name - 7)
        else name in
      if !verbose then begin
          Printf.printf "SHARING %s" name; print_newline ();
        end;
      name
    ))::
  (int32_tag "size" file.file_file.impl_file_size) ::
  (
    (match file.file_format with
        Unknown_format ->
          (try
              if !verbose then begin
                  Printf.printf "%s: FIND FORMAT %s"
                    (string_of_date (last_time ()))
                  (file_disk_name file); 
                  print_newline ();
                end;
              file.file_format <- 
                CommonMultimedia.get_info 
                (file_disk_name file)
            with _ -> ())
      | _ -> ()
    );
    
    match file.file_format with
      Unknown_format -> []
    | AVI _ ->
        [
          { tag_name = "type"; tag_value = String "Video" };
          { tag_name = "format"; tag_value = String "avi" };
        ]
    | MP3 _ ->
        [
          { tag_name = "type"; tag_value = String "Audio" };
          { tag_name = "format"; tag_value = String "mp3" };
        ]
    | FormatType (format, kind) ->
        [
          { tag_name = "type"; tag_value = String kind };
          { tag_name = "format"; tag_value = String format };
        ]
  )        
  
(* Computes tags for shared files *)
let make_tagged sock files =
  (List2.tail_map (fun file ->
        { f_md4 = file.file_md4;
          f_ip = client_ip sock;
          f_port = !client_port;
          f_tags = tag_file file;
        }
    ) files)
  
let direct_server_send_share sock msg =
  
(*  Printf.printf "SEND %d FILES TO SHARE" (List.length msg); print_newline ();*)
  
  let max_len = !!client_buffer_size - 100 - 
    TcpBufferedSocket.remaining_to_write sock in
  if !verbose then begin
      Printf.printf "SENDING SHARES"; print_newline ();
    end;

  Buffer.clear buf;
  buf_int8 buf 227;
  buf_int buf 0;
  buf_int8 buf 21; (* ShareReq *)
  buf_int buf 0;
  let nfiles, prev_len = DonkeyProtoServer.Share.write_files_max buf (
      make_tagged (Some sock) msg) 0 max_len in
  let s = Buffer.contents buf in
  let s = String.sub s 0 prev_len in
  let len = String.length s - 5 in
  str_int s 1 len;
  str_int s 6 nfiles;
  write_string sock s
        
let direct_client_send_files sock msg =
  let max_len = !!client_buffer_size - 100 - 
    TcpBufferedSocket.remaining_to_write sock in
  Buffer.clear buf;
  buf_int8 buf 227;
  buf_int buf 0;
  buf_int8 buf 75; (* ViewFilesReply *)
  buf_int buf 0;
  let nfiles, prev_len = DonkeyProtoClient.ViewFilesReply.write_files_max buf (
      make_tagged (Some sock) msg)
    0 max_len in
  let s = Buffer.contents buf in
  let s = String.sub s 0 prev_len in
  let len = String.length s - 5 in
  str_int s 1 len;
  str_int s 6 nfiles;
  write_string sock s

  
let udp_server_send s t =
  udp_send (get_udp_sock ()) s.server_ip (s.server_port+4)  t
