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
open TcpClientSocket
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

let server_port = define_option ["port"] "port to bind on" int_option 4661
let server_name = define_option ["name"] "small name of server" string_option 
    "mldonkey"
let server_desc = define_option ["desc"] "small description of server" string_option "mldonkey general server"
let welcome_messages = define_option ["welcome"] "list of welcome messages"
    (list_option string_option) [ "Welcome to my mldonkey server" ]
let seed_ip = define_option ["seedIP"] "IP of server to register" string_option
    ""
let seed_port = define_option ["seedPort"] "Port of server to register" int_option 4661
let max_clients = define_option ["maxClients"] "Maximal number of clients connected" int_option 200
  
  
(************* TYPES **********)
  
type file_entry = {
    mutable file_names : (string * (file_entry Indexer.entry)) list;
    file_md4 : Md4.t;
    file_size : int32;
    mutable file_avail : int; 
    file_tags : tag  list;
    mutable file_clients : client list;
  }
  
and client = {
    mutable client_id : Ip.t; 
    mutable client_md4 : Md4.t;
    mutable client_sock: TcpClientSocket.t;
    mutable from_ip : Ip.t;
    mutable from_port: int;
    mutable client_files : file_entry list;
    mutable client_tags: tag list;
  }

type server = {
    server_ip : Ip.t;
    server_port : int;
  }

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
  TcpClientSocket.write sock (Buffer.contents buf) 0 5;
  TcpClientSocket.write sock s 0 len
  
let hexa_digit x =
  if x >= 10 then Char.chr (Char.code 'A' + x - 10)
  else Char.chr (Char.code '0' + x)
      
let dummy_handler _ _ = ()

let index = Indexer.create ()

let files_by_md4 = Hashtbl.create 1023
let clients_by_id = Hashtbl.create 101
(* let anon_client_by_id = Numset.create () *)
let client_counter = ref 0  
let get_client id = Hashtbl.find clients_by_id id

let other_servers = ref []

let rec tag_find v tags =
  match tags with
    [] -> raise Not_found
  | { tag_name = tag_name; tag_value = tag_value } :: _ 
    when tag_name = v -> tag_value
  | _ :: tags -> tag_find v tags

let host = Host.from_name (Unix.gethostname ())
let my_ip = try Host.ip host with _ -> 
      Printf.printf "Using loopback IP"; print_newline ();
      Host.local_ip

let rec tags_find_size tags =
  match tags with
    [] -> raise Not_found
  | { tag_name = "length"; tag_value = Uint32 s } :: _ -> s
  | _ :: tags -> tags_find_size tags

      
let client_to_server client sock nread =
  let module Proto= Mftp_server in
  let b = TcpClientSocket.buf sock in
  try
    while b.len >= 5 do
      let msg_len = Mftp.get_int b.buf (b.pos+1) in
      if b.len >= 5 + msg_len then
        begin
          Printf.printf "client_to_server"; print_newline ();
          let s = String.sub b.buf (b.pos+5) msg_len in
          TcpClientSocket.buf_used sock  (msg_len + 5);
          let t = Proto.parse s in
          Proto.print t;
          match t with
            Proto.ConnectReq t ->
              client.client_md4 <- t.Proto.Connect.md4;
              client.client_tags <- t.Proto.Connect.tags;
              
              client.from_port <- t.Proto.Connect.port;
              
(* send ID back to client *)
              server_send sock  (Proto.SetIDReq{ 
                  Mftp_server.SetID.id = client.client_id});

              
(* send some messages *)
              List.iter (fun msg ->
                  server_send sock (Proto.MessageReq msg)) !!welcome_messages
          | Proto.AckIDReq _ -> ()
              
          | Proto.ShareReq t ->
              
              let files = List.map (fun file ->
                    let md4 = file.Proto.Share.md4 in
                    let tags = file.Proto.Share.tags in
                    let file =
                      try
                        Hashtbl.find files_by_md4 md4 
                        with _ ->
                          let file = {
                              file_names = [];
                              file_md4 = md4;  
                              file_tags = tags;
                              file_clients = [];
                              file_avail = 0;
                              file_size = tags_find_size tags;
                            }  in
                          Hashtbl.add files_by_md4 md4 file;
                          file
                    in
                    begin try
                        match tag_find "filename" tags with
                          String filename -> 
                            if not (List.mem_assoc filename file.file_names ) 
                            then 
                              file.file_names <- (filename, 
                                Indexer.add index filename file) :: 
                              file.file_names
                            
                        | _ -> raise Not_found
                      with _ -> ()
                    end;
                    file
                ) t in
              List.iter (fun file -> 
                  file.file_clients <- client :: file.file_clients;
                  file.file_avail <- file.file_avail + 1;
                  ) files;
              client.client_files <- files @ client.client_files;

              server_send sock (Proto.ServerListReq []);
              
              server_send sock (
                let module R = Proto.ServerInfo in
                Proto.ServerInfoReq {
                  R.md4 = Md4.null;
                  R.ip = Ip.of_inet_addr my_ip;
                  R.port = !!server_port;
                  R.tags = [
                    { tag_name = "name"; 
                      tag_value = String !!server_name };
                    { tag_name = "description"; 
                      tag_value = String !!server_desc };
                  ];
                })

              
          | Proto.QueryReq t ->
              let module R = Proto.Query in

              let rec do_query t =
                match t with
                  R.And (q1, q2) ->
                    let l1, f1 = do_query q1 in
                    let l2, f2 = do_query q2 in
                    Indexer.And (l1, l2), (fun e -> f1 e && f2 e)
                | R.Or (q1, q2) ->
                    let l1, f1 = do_query q1 in
                    let l2, f2 = do_query q2 in
                    Indexer.Or (l1, l2), (fun e -> f1 e || f2 e)
                | R.HasWord s ->
                    let words = split_words s in
                    let rec iter list =
                      match list with
                        [] -> Indexer.Neutral
                      | [ s ] -> Indexer.Word s
                      | s :: l ->
                          Indexer.And (Indexer.Word s, iter l)
                    in
                    iter words, (fun _ -> true)
                | R.HasMinVal ("length", value) ->
                    Printf.printf "MINSIZE"; print_newline ();
                    Indexer.Neutral, (fun file ->
                        Printf.printf "checked"; print_newline ();
                        file.file_size >= value)
                | R.HasMaxVal ("length", value) ->
                    Printf.printf "MAXSIZE"; print_newline ();
                    Indexer.Neutral, (fun file ->
                        file.file_size <= value)
                | R.HasField (field, value) ->
                    Indexer.Neutral, (fun file ->
                        try
                          match tag_find field file.file_tags with
                            String s when s = value -> true
                          | _ -> false
                        with _ -> false
                    )
                | _ -> Indexer.Neutral, (fun _ -> true) 
              in
              let index_query, check = do_query t in
              
(* TODO: predicate not implemented *)
              let files = Indexer.find index index_query in
              let _, files = List.fold_left (fun (max,res) file ->
                    let file = Indexer.value file in
                    if max>0 && check file then
                      (max-1, file :: res) 
                    else
                      (max, res)
                      
                    
                )  (200, []) files in
              
(* send reply back *)
              let module R = Proto.QueryReply in
              Buffer.clear buf;
              Proto.write buf (Proto.QueryReplyReq(
(* we should not reply files such that file.clients = 0 *)
(* we should limit returned files to 200 *)
                  List.fold_left (fun list file ->
                      match file.file_clients with
                        [] -> list
                      | client :: _ ->
                          {
                            R.md4 = file.file_md4;
                            R.ip = client.client_id;
                            R.port = client.from_port;
                            R.tags = file.file_tags @ 
                              [{ tag_name = "availability";
                                tag_value = Uint32 (
                                  Int32.of_int file.file_avail);
                              }] ;
                          } :: list
                  ) [] files));
              send_buf buf sock

          | Proto.QueryLocationReq t ->
              let module R = Proto.QueryLocation in
              let md4 = t in
              begin
                try
                  let file = Hashtbl.find files_by_md4 md4 in
(* we should only reply clients that are OK *)

                  Buffer.clear buf;
                  let module R = Proto.QueryLocationReply in
                  Proto.write buf (Proto.QueryLocationReplyReq(
(* we should not reply files such that file.clients = 0 *)
(* we should limit returned files to 200 *)
                      { R.md4 = md4;
                        R.locs = 
                        List.map (fun client ->
                            {
                              R.ip = client.from_ip;
                              R.port = client.from_port;
                            }
                        ) file.file_clients; }
                    ));
                  send_buf buf sock;
                  Printf.printf "REPLY SENT"; print_newline ();
                  
                with e -> TcpClientSocket.close sock 
                    (Printf.sprintf "Error: %s" (Printexc.to_string e))
              end;
              
          | _ -> ()
        end
      else raise Not_found
    done
  with Not_found -> ()

let remove_file file =
  Hashtbl.remove files_by_md4 file.file_md4;
(* remove file from index *)
  List.iter (fun (_,entry) ->
      Indexer.remove entry
  ) file.file_names
      
let remove_client client sock msg =
(* remove the client from all tables *)
  Hashtbl.remove clients_by_id client.client_id;
  
(* remove the client from files *)
  List.iter (fun file -> 
      file.file_clients <- List2.removeq client file.file_clients;
      if file.file_clients = [] then
        remove_file file
  ) client.client_files;
  ()
  
let handler t event =
  Printf.printf "CONNECTION"; print_newline ();
  match event with
    TcpServerSocket.CONNECTION (s, Unix.ADDR_INET (from_ip, from_port)) ->
            
      let sock = TcpClientSocket.create s dummy_handler in
      
      let ip = Ip.of_inet_addr from_ip in
      let id = 
        try
          ignore (Hashtbl.find clients_by_id ip);
            incr client_counter;
            Ip.of_rev_int !client_counter
        with _ -> ip
            in
      let client = {
          client_id = id;
          client_sock = sock;
          from_ip = ip;
          from_port = from_port;
          client_files = [];
          client_tags = [];
          client_md4 = Md4.null;
        } in
      Hashtbl.add clients_by_id id client;
      
      TcpClientSocket.set_reader sock (client_to_server client);
      TcpClientSocket.set_closer sock 
      (remove_client client)
  | _ -> 
      Printf.printf "???"; print_newline ();
      ()      
  
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
      
(* reponse (propagation de la liste des serveurs connus):
227: header
161: opcode
(127)(0)(0)(1)(53)(18): mon adresse
2: nbre de serveurs connus  
servers_connus (IP + port)
  *)
      
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
  
