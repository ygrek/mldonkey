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

open CommonOptions
open CommonUser
open CommonRoom
open CommonServer
open CommonComplexOptions
open CommonSearch
open CommonResult
open CommonTypes
open BasicSocket
open TcpBufferedSocket
open Options
open DcOptions
open DcComplexOptions
open CommonGlobals
open DcTypes
open DcProtocol
open DcGlobals

module CO = CommonOptions


let try_connect_client c =
  if connection_can_try c.client_connection_control then
    match c.client_sock with
    | Some _ -> ()
    | None -> 
        match c.client_addr with
          None ->
            List.iter (fun s ->
                match s.server_sock with
                  None -> ()
                | Some sock ->
                    debug_server_send sock (
                      let module C = ConnectToMe in
                      ConnectToMeReq {
                        C.nick = c.client_name;
                        C.ip = CO.client_ip (Some sock);
                        C.port = !!dc_port;
                      }
                    );
                                        
                    debug_server_send sock (
                      let module C = RevConnectToMe in
                      RevConnectToMeReq {
                        C.orig = s.server_last_nick;
                        C.dest = c.client_name;
                      }
                    );
            )                    
            c.client_user.user_servers 
        | Some (ip, port) ->
            DcClients.connect_client c
  
    
let rec remove_short list list2 =
  match list with
    [] -> List.rev list2
  | s :: list -> 
      if String.length s < 5 then (* keywords should had list be 5 bytes *)
        remove_short list list2
      else
        remove_short list (s :: list2)
          
let stem s =
  let s = String.lowercase (String.copy s) in
  for i = 0 to String.length s - 1 do
    let c = s.[i] in
    match c with
      'a'..'z' | '0' .. '9' -> ()
    | _ -> s.[i] <- ' ';
  done;
  remove_short (String2.split s ' ') []
           
let ask_for_file file =
  List.iter (fun c ->
      try_connect_client c
  ) file.file_clients
  
let ask_for_files () =
  Hashtbl.iter (fun _ file ->
      ask_for_file file
  ) files_by_key
     
let recover_files_from_server s = 
  List.iter (fun file ->
      
      List.iter (fun c ->
          match c.client_sock with
          | Some _ -> ()
          | None -> 
              match c.client_addr with
                Some _ -> ()
              | None ->
                  match s.server_sock with
                    None -> ()
                  | Some sock ->
                      debug_server_send sock (
                        let module C = ConnectToMe in
                        ConnectToMeReq {
                          C.nick = c.client_name;
                          C.ip = CO.client_ip (Some sock);
                          C.port = !!dc_port;
                        }
                      );
                      
                      debug_server_send sock (
                        let module C = RevConnectToMe in
                        RevConnectToMeReq {
                          C.orig = s.server_last_nick;
                          C.dest = c.client_name;
                        }
                      );
      )                    
      file.file_clients;
      
      
(* try to find new sources by queries *)
      let keywords = 
        match stem file.file_name with 
          [] | [_] -> 
(*            Printf.printf "Not enough keywords to recover %s" f.file_name;
            print_newline (); *)
            [file.file_name]
        | l -> l
      in
      let words = String2.unsplit keywords ' ' in
(*      ignore (send_query (Recover_file keywords) keywords) *)
      let module S = Search in
      let msg = SearchReq {
          S.orig = Printf.sprintf "Hub:%s" s.server_last_nick;
          S.sizelimit = NoLimit;
          S.filetype = 0;
          S.words = words;
        } in
      match s.server_sock with
        None -> ()
      | Some sock ->
          debug_server_send sock msg; 
  ) !current_files;
  ()
  
let server_addr s = string_of_addr s.server_addr

  
let disconnect_server s =
  match s.server_sock with
    None -> ()
  | Some sock -> 
      (try close sock "user disconnect" with _ -> ());      
      decr nservers;
      Printf.printf "%s:%d CLOSED received by server"
        (server_addr s) s.server_port; print_newline ();
      connection_failed (s.server_connection_control);
      s.server_sock <- None;
      Printf.printf "******** NOT CONNECTED *****"; print_newline ();
      set_server_state s NotConnected;
      set_room_state s RoomClosed;
      connected_servers := List2.removeq s !connected_servers;
      s.server_messages <- 
        (room_new_message (as_room s.server_room) 
          (ServerMessage "************* CLOSED ***********\n"))
        :: s.server_messages;
      room_must_update (as_room s.server_room)
      
let server_handler s sock event = 
  match event with
    BASIC_EVENT (CLOSED _) -> disconnect_server s      
  | _ -> ()

      
      
let rec client_to_server s m sock = 
(*
  Printf.printf "From %s:%d"
    (server_addr s) s.server_port; print_newline ();
  DcProtocol.print m;
  *)
  match m with
    LockReq lock ->
      server_send sock (
        KeyReq { Key.key = DcKey.gen lock.Lock.key });
      let nick = login s in
      server_send sock (
        ValidateNickReq nick)
  
  | ForceMoveReq t ->
      
      let s = new_server (new_addr_name t) 411 in
      close sock "force move";
      connect_server s
  
  | ConnectToMeReq t ->
(* nick/ip/port *)
      begin
        Printf.printf "From %s:%d"
          (server_addr s) s.server_port; print_newline ();
        DcProtocol.print m;

(*
        let c = new_client t.ConnectToMe.nick in
        c.client_addr <- Some (t.ConnectToMe.ip, t.ConnectToMe.port);
        match c.client_sock with 
          None ->  
            DcClients.connect_client c
        | Some sock ->
            Printf.printf "We are already connected to that client !!";
print_newline () 
*)
        DcClients.connect_anon s t.ConnectToMe.ip t.ConnectToMe.port
      end
  
  | HubNameReq t ->
      s.server_name <- t;
      server_must_update s
  
  | HelloReq t ->
      if t = s.server_last_nick then begin
          set_rtimeout sock half_day;
          List.iter (fun m ->
              server_send sock (UnknownReq m);
          ) !!login_messages;
          Printf.printf "*****  CONNECTED  ******"; print_newline (); 
          set_server_state s Connected_idle;
          set_room_state s RoomOpened;
          connected_servers := s :: !connected_servers;
          let module I = MyINFO in
          server_send sock (MyINFOReq {
              I.dest = "$ALL";
              I.nick = s.server_last_nick;
              I.description = !!client_description;
              I.speed = !!client_speed;
              I.kind = 6;
              I.email = "";
              I.size = Int64.to_float !shared_counter +. !!shared_offset;
            });
          recover_files_from_server s
        end  else
        ignore (user_add s t)
  
  | OpListReq list ->
      List.iter (fun nick ->
          let u = user_add s nick in
          u.user_admin <- true;
          user_must_update (as_user u.user_user)
      ) list
  
  | MyINFOReq t ->
      if t.MyINFO.nick <> s.server_last_nick then begin
          let u = user_add s t.MyINFO.nick in
          u.user_link <- t.MyINFO.speed;
          u.user_data <- t.MyINFO.size;
          user_must_update (as_user u.user_user)
        end
  
  | ToReq t ->
      let orig = user_add s t.To.orig in
      let message = t.To.message in
      
      s.server_messages <- (
        room_new_message (as_room s.server_room)
        (PrivateMessage (orig.user_user.impl_user_num,
            message))) :: s.server_messages;
      room_must_update (as_room s.server_room)
  
  | QuitReq t ->
(*user_remove (user_add s t) *) ()
  
  | NickListReq t ->
      List.iter (fun t  -> ignore (user_add s t)) t
  
  | MessageReq t ->
      s.server_messages <- (room_new_message
          (as_room s.server_room) 
        (ServerMessage t)) :: s.server_messages;
      room_must_update (as_room s.server_room)
  
  | SearchReq t ->
      let orig = t.Search.orig in
      if String.sub orig 0 4 = "Hub:" then
        let nick = String.sub orig 4 (String.length orig - 4) in
        ignore (user_add s nick)
  
  | SRReq t ->
      begin
        if s.server_search_timeout < last_time () then
          s.server_search <- None;
        begin
          try
            let file = find_file (Filename2.basename t.SR.filename) t.SR.filesize in 
            Printf.printf "**** FILE RECOVERED ****"; print_newline ();
            let user = new_user (Some s) t.SR.owner in
            let c = add_file_client file user t.SR.filename in
            try_connect_client c      
          with _ -> ()
        end;
        
        match s.server_search with 
          None -> ()
        | Some q -> 
(*            add_source result t s; *)
            let result = new_result t.SR.filename t.SR.filesize in
            let user = new_user (Some s) t.SR.owner in
            add_result_source result user t.SR.filename;
            search_add_result q result.result_result
      end
  
  | UnknownReq "" -> ()
  
  | _ -> 
      Printf.printf "###UNUSED SERVER MESSAGE###########"; print_newline ();
      DcProtocol.print m



and connect_server s =
  match s.server_sock with
  | Some _ -> ()
  | None ->
      if can_open_connection () then
        try
          connection_try s.server_connection_control;
          incr nservers;
          printf_char 's'; 
          let ip = ip_of_addr s.server_addr
          in
          let sock = TcpBufferedSocket.connect "directconnect to server" (
              Ip.to_inet_addr ip)
            s.server_port (server_handler s)  in
          
          set_server_state s Connecting;
          set_read_controler sock download_control;
          set_write_controler sock upload_control;
          
          set_reader sock (DcProtocol.dc_handler (client_to_server s));
          set_rtimeout sock 60.;
          set_handler sock (BASIC_EVENT RTIMEOUT) (fun s ->
              close s "timeout"  
          );
          s.server_nick <- 0;
          s.server_sock <- Some sock;
        with e -> 
            Printf.printf "%s:%d IMMEDIAT DISCONNECT %s"
              (string_of_addr s.server_addr) s.server_port
              (Printexc.to_string e); print_newline ();
(*      Printf.printf "DISCONNECTED IMMEDIATLY"; print_newline (); *)
            decr nservers;
            s.server_sock <- None;
            set_server_state s NotConnected;
            connection_failed s.server_connection_control
            
let try_connect_server s =
  if connection_can_try s.server_connection_control then
      match s.server_sock with
        Some _ -> ()
      | None -> 
          connect_server s
    
let rec connect_one_server () =
  if can_open_connection () then
    match !servers_list with
      [] ->
        Hashtbl.iter (fun _ h ->
            servers_list := h :: !servers_list) servers_by_addr;
        if !servers_list = [] then begin
            Printf.printf "No DC server to connect to"; print_newline ();
            raise Not_found;
          end;
        connect_one_server ()
    | s :: list ->
        servers_list := list;
        try_connect_server s
      
let connect_servers () = 
  if !nservers < !!max_connected_servers then
    for i = !nservers to !!max_connected_servers do
      connect_one_server ()
    done

let parse_servers_list s =
  let lines = String2.split s '\n' in
  List.iter (fun s ->
      match String2.split s '|' with
        server_name :: server_addr :: server_info :: server_nusers :: _ ->
          let s = new_server (addr_of_string server_addr) 411 in
          s.server_name <- server_name;
          s.server_info <- server_info;
          (try s.server_nusers <- int_of_string server_nusers with _ -> ());
      | _ -> 
          Printf.printf "Bad line [%s]" s; print_newline ();      
  ) lines;
  ()
    
let load_servers_list url =
  let url = if url = "" then !!servers_list_url
    else "" in
  let filename = Filename.temp_file "http_" ".tmp" in
  let file_oc = open_out filename in
  let file_size = ref 0 in
  Http_client.get_page (Url.of_string url) []
  (fun maxlen headers sock nread ->
(*        Printf.printf "..."; print_newline (); *)
        let buf = TcpBufferedSocket.buf sock in
        
        if nread > 0 then begin
            let left = 
              if maxlen >= 0 then
                min (maxlen - !file_size) nread
              else nread
            in
            output file_oc buf.buf buf.pos left;
            buf_used sock left;
            file_size := !file_size + left;
            if nread > left then
              TcpBufferedSocket.close sock "end read"
          end
        else
        if nread = 0 then begin
            close_out file_oc;
            try
              parse_servers_list (File.to_string filename);
              Sys.remove filename
            with e ->
                Printf.printf
                  "Exception %s in loading downloaded file %s"
                  (Printexc.to_string e) filename
          
          end
    )

module P = GuiTypes
  
let _ =
  server_ops.op_server_info <- (fun s ->
      if !!enable_directconnect then
      {
        P.server_num = (server_num s);
        P.server_network = network.network_num;
        P.server_addr = s.server_addr;
        P.server_port = s.server_port;
        P.server_score = 0;
        P.server_tags = [];
        P.server_nusers = s.server_nusers;
        P.server_nfiles = 0;
        P.server_state = server_state s;
        P.server_name = s.server_name;
        P.server_description = s.server_info;
        P.server_users = None;
        }
      else raise Not_found)
     
let recover_files () = 
  List.iter (fun file ->
(* try to connect to known sources *)
      List.iter (fun c ->
          Printf.printf "ATTEMPT TO RECOVER CONNECTION TO %s"
            c.client_user.user_nick; print_newline ();
          try_connect_client c;
          List.iter (fun s ->
              match s.server_sock with
                Some _ -> ()
              | None ->
                  try_connect_server s
          ) c.client_user.user_servers
      ) file.file_clients;


(* try to find new sources by queries *)
      let keywords = 
        match stem file.file_name with 
          [] | [_] -> 
(*            Printf.printf "Not enough keywords to recover %s" f.file_name;
            print_newline (); *)
            [file.file_name]
        | l -> l
      in
      let words = String2.unsplit keywords ' ' in
(*      ignore (send_query (Recover_file keywords) keywords) *)
      List.iter (fun s ->
          if s.server_search_timeout < last_time () then
            let module S = Search in
            let msg = SearchReq {
                S.orig = Printf.sprintf "Hub:%s" s.server_last_nick;
                S.sizelimit = NoLimit;
                S.filetype = 0;
                S.words = words;
              } in
            match s.server_sock with
              None -> ()
            | Some sock ->
                debug_server_send sock msg; 
                Printf.bprintf  buf "Sending search\n") !connected_servers
  ) !current_files;
  ()
  
let _ =
  server_ops.op_server_connect <- connect_server;
  server_ops.op_server_disconnect <- disconnect_server;
  server_ops.op_server_query_users <- (fun s ->
      match s.server_sock with
        None -> ()
      | Some sock ->
          server_send sock (GetNickListReq)
  );
  server_ops.op_server_users <- (fun s ->
      List2.tail_map (fun u -> as_user u.user_user) s.server_users
  );
  server_ops.op_server_remove <- (fun s ->
      disconnect_server s;
      server_remove s
  )
  
  
