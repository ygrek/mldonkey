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

open Options
open CommonGlobals
open LittleEndian
open DonkeyMftp
open BasicSocket
open TcpBufferedSocket

type server_msg = DonkeyProtoServer.t 
type client_msg =  DonkeyProtoClient.t 

type server_sock = TcpBufferedSocket.t
type client_sock = TcpBufferedSocket.t

  
let verbose = ref false

let buf = TcpBufferedSocket.internal_buf

  
let server_msg_to_string msg =
  Buffer.clear buf;
  buf_int8 buf 227;
  buf_int buf 0;
  DonkeyProtoServer.write buf msg;
  let s = Buffer.contents buf in
  let len = String.length s - 5 in
  str_int s 1 len;
  s
        
let client_msg_to_string msg =
  Buffer.clear buf;
  buf_int8 buf 227;
  buf_int buf 0;
  DonkeyProtoClient.write buf msg;
(*      DonkeyProtoClient.print msg.msg;  *)
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
  
let client_send sock m =
  write_string sock (client_msg_to_string m)

  
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
        let msg_len = get_int b.buf (b.pos+1) in
        if b.len >= 5 + msg_len then
          begin
            if !verbose then begin
                Printf.printf "client_to_client"; 
                print_newline ();
              end;
            let s = String.sub b.buf (b.pos+5) msg_len in
            TcpBufferedSocket.buf_used sock  (msg_len + 5);
            let t = M.parse s in
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
      let msg_len = get_int b.buf (b.pos+1) in
      if b.len >= 5 + msg_len then
        begin
          if !verbose then begin
              Printf.printf "server_to_client"; 
              print_newline ();
            end;
          let s = String.sub b.buf (b.pos+5) msg_len in
          TcpBufferedSocket.buf_used sock  (msg_len + 5);
          let t = parse s in
          f t sock
        end
      else raise Not_found
    done
  with Not_found -> ()

let udp_send t addr msg =
  try
  Buffer.clear buf;
  buf_int8 buf 227;
  DonkeyProtoServer.udp_write buf msg;
  let s = Buffer.contents buf in
  UdpSocket.write t s 0 (String.length s) addr
  with e ->
      Printf.printf "Exception %s in udp_send" (Printexc.to_string e);
      print_newline () 
  
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
      Printf.printf "Exception %s in udp_send" (Printexc.to_string e);
      print_newline () 
        
let udp_handler f sock event =
  let module M = DonkeyProtoServer in
  match event with
    UdpSocket.READ_DONE ->
      List.iter (fun p -> 
          try
            let pbuf = p.UdpSocket.content in
            let len = String.length pbuf in
            if len = 0 || 
              int_of_char pbuf.[0] <> 227 then begin
                Printf.printf "Received unknown UDP packet"; print_newline ();
                dump pbuf;
              end else begin
                let t = M.parse (String.sub pbuf 1 (len-1)) in
(*              M.print t; *)
                f t p
              end
          with e ->
              Printf.printf "Error %s in udp_handler"
                (Printexc.to_string e); print_newline () 
      ) sock.UdpSocket.rlist;
      sock.UdpSocket.rlist <- []
  | _ -> ()

let propagation_socket = UdpSocket.create_sendonly ()

let counter = ref 1
  
(* Learn how many people are using mldonkey at a current time, and which 
  servers they are connected to --> build a database of servers *)
let propagate_working_servers servers =
  if !!DonkeyOptions.propagate_servers then begin
      decr counter;
      if !counter = 0 then begin
          counter := 6;
          try
            Buffer.clear buf;
            buf_int8 buf DonkeyOpenProtocol.udp_magic; (* open protocol *)
            buf_int8 buf 0;    
            let ip = Ip.my () in
            buf_ip buf ip;
            buf_list buf_peer buf servers;
            let s = Buffer.contents buf in    
            UdpSocket.write propagation_socket s 0 (String.length s) 
            (Ip.to_sockaddr (Ip.of_ints (128,93,52,5)) 4665)
          with e ->
              Printf.printf "Exception %s in udp_sendonly" (Printexc.to_string e);
              print_newline () 
        end      
    end
    
let udp_basic_handler f sock event =
  match event with
    UdpSocket.READ_DONE ->
      List.iter (fun p -> 
          try
            let pbuf = p.UdpSocket.content in
            let len = String.length pbuf in
            if len = 0 || 
              int_of_char pbuf.[0] <> DonkeyOpenProtocol.udp_magic then begin
                Printf.printf "Received unknown UDP packet"; print_newline ();
                dump pbuf;
              end else begin
                let t = String.sub pbuf 1 (len-1) in
                f t p
              end
          with e ->
              Printf.printf "Error %s in udp_basic_handler"
                (Printexc.to_string e); print_newline () 
      ) sock.UdpSocket.rlist;
      sock.UdpSocket.rlist <- []
  | _ -> ()


let new_string msg s =
  let len = String.length s - 5 in
  str_int s 1 len  
  
let empty_string = ""
  
let client_msg msg = msg
let server_msg msg = msg
  
let direct_servers_send s msg =
  servers_send s (server_msg msg)
  
let direct_client_send s msg =
  client_send s (client_msg msg)
  
let direct_server_send s msg =
  server_send s (server_msg msg)
  
