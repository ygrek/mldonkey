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
open Unix
open TcpBufferedSocket
open Mftp
open Options
open Mftp_comm
  
(*********  OPTIONS *********)  
  
let directory = ref "."
let must_save_options = ref false
  
let args = [
    "-d", Arg.String ((:=) directory), " <dir>: server directory";
  ]
  
let msg = "Ocaml edonkey server"
let _ = Arg.parse args (fun _ -> Arg.usage args msg) msg

let config_filename = "server.ini"
  
let _ =
  Options.filename := 
  (try Filepath.find_in_path [!directory] config_filename with
      _ -> 
        Printf.printf "No config file found. Generating one."; 
        print_newline ();
        must_save_options := true;
        Filename.concat !directory config_filename);
  (try Options.load () with _ -> ())
  
  
(************* TYPES **********)

let split_words s =
  let len = String.length s in
  let s = String.lowercase s in
  let rec iter_out i =
    if i = len then [] else
    let c = s.[i] in
    match c with
      'a' .. 'z' | '0' .. '9' ->
        iter_in i (i+1)
    | _ -> iter_out (i+1)
        
  and iter_in p0 i =
    if i = len then [String.sub s p0 (i-p0)] else
    let c = s.[i] in
    match c with
      'a' .. 'z' | '0' .. '9' ->
        iter_in p0 (i+1)
    | _ -> (String.sub s p0 (i - p0)) :: iter_out (i+1)
    
  in
  iter_out 0

let send_buf buf sock =
  let s = Buffer.contents buf in
  Buffer.clear buf;
  Mftp.buf_int8 buf 227;
  let len = String.length s in
  Mftp.buf_int32_32 buf (Int32.of_int len);
  TcpBufferedSocket.write sock (Buffer.contents buf) 0 5;
  TcpBufferedSocket.write sock s 0 len
  
let hexa_digit x =
  if x >= 10 then Char.chr (Char.code 'A' + x - 10)
  else Char.chr (Char.code '0' + x)
      
let dummy_handler _ _ = ()
  
let udp_send t addr msg =
  Buffer.clear buf;
  buf_int8 buf 227;
  Mftp_server.write buf msg;
  let s = Buffer.contents buf in
  UdpSocket.write t s 0 (String.length s) addr
  
let rec add_new_servers servers other_servers to_add =
  let module Q = Mftp_server.QueryServersReply in
  match servers with 
    [] -> other_servers @ to_add
  | s :: tail ->
      let s = { 
          server_ip = s.Q.ip;
          server_port = s.Q.port;
        } in
      add_new_servers tail other_servers 
        (if List.mem s other_servers then to_add else (s :: to_add))
        

let rec find_servers servers n left =
  if n = 0 then left else
  match servers with
    [] -> left
  | s :: tail ->
      find_servers tail (n-1) (
        let module Q = Mftp_server.QueryServersReply in
        { Q.ip = s.server_ip; Q.port = s.server_port; } :: left)
          
    
      
let udp_handler sock event =
  let module M = Mftp_server in
  match event with
    UdpSocket.READ_DONE ->
      List.iter (fun p -> 
          let pbuf = p.UdpSocket.content in
          let len = String.length pbuf in
          if len = 0 || 
            int_of_char pbuf.[0] <> 227 then begin
              Printf.printf "Received unknown UDP packet"; print_newline ();
              dump pbuf;
            end else begin
              let t = M.parse (String.sub pbuf 1 (len-1)) in
              M.print t;
              match t with
                M.QueryServersReplyUdpReq t -> 
                  let module Q = M.QueryServersReply in
                  other_servers := add_new_servers t.Q.servers 
                    !other_servers []
              | M.QueryServersUdpReq t -> 
                  let module Q = M.QueryServers in
                  let to_addr = p.UdpSocket.addr in
                  let servers = find_servers !other_servers 200 [] in
                  udp_send sock to_addr (
                    let module M = Mftp_server in
                    M.QueryServersReplyUdpReq (
                      let module Q = M.QueryServersReply in
                      {
                        Q.server_ip = Ip.of_inet_addr my_ip;
                        Q.server_port = !!server_port;
                        Q.servers = servers;
                      }));
                  
              | M.Message150UdpReq (t1, t2,t3) ->            
                  let to_addr = p.UdpSocket.addr in
                  udp_send sock to_addr (M.Message151UdpReq
                    (t1, t2, t3, Int32.of_int 1, Int32.of_int 0, 0))
                    
                  
              | _ -> 
                  Printf.printf "UNKNOWN"; print_newline ();
            end
      ) sock.UdpSocket.rlist;
      sock.UdpSocket.rlist <- []
  | _ -> ()
      
let _ =
  if !must_save_options then Options.save_with_help ();
(*  ignore(TcpServerSocket.create 4663 handler); *)
  ignore(TcpServerSocket.create !!server_port handler);
  let udp_sock = UdpSocket.create (!!server_port + 4) udp_handler in
  if !!seed_ip <> "" then begin
      udp_send udp_sock (Unix.ADDR_INET (inet_addr_of_string !!seed_ip,
          !!seed_port + 4)) (
          let module M = Mftp_server in
          M.QueryServersUdpReq (
            let module Q = M.QueryServers in
            {
              Q.ip = Ip.of_inet_addr my_ip;
              Q.port = !!server_port;
            }));
    end;
  Printf.printf "server started at %d" !!server_port; print_newline ();
  BasicSocket.loop ()
  
