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
open CommonChatRoom
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

let server_addr s =
  match s.server_addr with
    AddrIp ip -> Ip.to_string ip
  | AddrName s -> s
  
let exit_exn = Exit
let basename filename =
  let s =
    let len = String.length filename in
    try
      let pos = String.rindex_from filename (len-1) '\\' in
      String.sub filename (pos+1) (len-pos-1)
    with _ ->      
        try
          if len > 2 then
            let c1 = Char.lowercase filename.[0] in
            let c2 = filename.[1] in
            match c1,c2 with
              'a'..'z', ':' ->
                String.sub filename 2 (len -2 )
            | _ -> raise exit_exn
          else raise exit_exn
        with _ -> Filename.basename filename
  in
  String.lowercase s

let results_by_file = Hashtbl.create 111

let sources_by_key = Hashtbl.create 111
  
let new_result r s =
  let basename = basename r.SR.filename in
  let key = (basename, r.SR.filesize) in
  
  let result = try
      Hashtbl.find results_by_file key
    with _ ->
        let rec result = {
            result_result = result_impl;
            result_name = basename;
            result_size = r.SR.filesize;
            result_sources = [];
          } and
          result_impl = {
            impl_result_val = result;
            impl_result_ops = result_ops;
            impl_result_num = 0;
          } in
        CommonResult.new_result result_impl;
        Hashtbl.add results_by_file key result;
        result
  in
  
  let server_num = server_num s in
  let server = match r.SR.server_ip with
      None -> s.server_name | Some h -> h
  in
  
  let key = (r.SR.owner, server_num) in
  
  let src = try
      Hashtbl.find sources_by_key key
    with _ ->
        let src = {
            source_nick = r.SR.owner;
            source_server = s;
          } in
        
        Hashtbl.add sources_by_key key src;
        src
  in
  
  if not (List.mem_assoc src result.result_sources) then
    result.result_sources <- (src, r.SR.filename) :: result.result_sources;
  
  result
      
let server_handler s sock event = 
  match event with
    BASIC_EVENT (CLOSED _) ->
      
      if s.server_sock <> None then decr nservers;
      Printf.printf "%s:%d CLOSED received by server"
      (server_addr s) s.server_port; print_newline ();
      connection_failed (s.server_connection_control);
      s.server_sock <- None;
      set_server_state s NotConnected;
      set_room_state s RoomClosed;
      connected_servers := List2.removeq s !connected_servers;
      s.server_messages <- (ServerMessage "************* CLOSED ***********\n")
      :: s.server_messages;
      room_must_update (as_room s.server_room)
      
  | _ -> ()

let rec client_to_server s t sock = 
(*
  Printf.printf "From %s:%d"
    (server_addr s) s.server_port; print_newline ();
DcProtocol.print t;

  *)
  match t with
    LockReq lock ->
      server_send sock (
        KeyReq { Key.key = DcKey.gen lock.Lock.key });
      let nick = login s in
      server_send sock (
        ValidateNickReq nick)

  | ForceMoveReq t ->
      
      let s = new_server (AddrName t) in
      close sock "force move";
      connect_server s

  | ConnectToMeReq t ->
(* nick/ip/port *)
      begin
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
              I.description = "mldonkey client";
              I.speed = "DSL";
              I.kind = 6;
              I.email = "";
              I.size = !shared_total +. !!shared_offset;
            })
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
      
      s.server_messages <- (PrivateMessage (orig.user_user.impl_user_num,
          message)) :: s.server_messages;
      room_must_update (as_room s.server_room)

  | QuitReq t ->
      user_remove (user_add s t)
        
  | NickListReq t ->
      List.iter (fun t  -> ignore (user_add s t)) t
        
  | MessageReq t ->
      s.server_messages <- (ServerMessage t) :: s.server_messages;
      room_must_update (as_room s.server_room)

  | SearchReq t ->
      let orig = t.Search.orig in
      if String.sub orig 0 4 = "Hub:" then
        let nick = String.sub orig 4 (String.length orig - 4) in
        ignore (user_add s nick)
      
  | SRReq t ->
      begin
        match s.server_searches with 
          [] -> assert false
        | q :: _ -> 
            let result = new_result t s in
            search_add_result q result.result_result
      end
      
  | UnknownReq "" -> ()
      
  | _ -> 
      Printf.printf "###UNUSED SERVER MESSAGE###########"; print_newline ();
      DcProtocol.print t

      
      
and connect_server s =
  if can_open_connection () then
    try
      connection_try s.server_connection_control;
      incr nservers;
      printf_char 's'; 
      let ip =
        match s.server_addr with
          AddrIp ip -> ip
        | AddrName name -> 
            try
              match s.server_ip_cached with
                None -> raise exit_exn
              | Some (ip, date) ->
                  if date +. !!ip_cache_timeout < last_time () then
                    raise Not_found;
                  ip
            with _ ->
                let ip = Ip.from_name name in
                s.server_ip_cached <- Some (ip, last_time ());
                ip
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
          (match s.server_addr with
            AddrIp ip -> Ip.to_string ip
          | AddrName s -> s) s.server_port
          (Printexc.to_string e); print_newline ();
(*      Printf.printf "DISCONNECTED IMMEDIATLY"; print_newline (); *)
        decr nservers;
        s.server_sock <- None;
        set_server_state s NotConnected;
        connection_failed s.server_connection_control
  
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
        if connection_can_try s.server_connection_control then
          begin
            match s.server_sock with
              Some _ -> ()
            | None -> 
                connect_server s
          end
      
let connect_servers () = 
  if !nservers < !!max_connected_servers then
    for i = !nservers to !!max_connected_servers do
      connect_one_server ()
    done

let string_to_addr v =
  let ip = try Ip.of_string v with _ -> Ip.null in
  if ip <> Ip.null then AddrIp ip else AddrName v

let parse_servers_list s =
  let lines = String2.split s '\n' in
  List.iter (fun s ->
      match String2.split s '|' with
        server_name :: server_addr :: server_info :: server_nusers :: _ ->
          let s = new_server (string_to_addr server_addr) in
          s.server_name <- server_name;
          s.server_info <- server_info;
          (try s.server_nusers <- int_of_string server_nusers with _ -> ());
      | _ -> 
          Printf.printf "Bad line [%s]" s; print_newline ();      
  ) lines;
  ()
    
let load_servers_list url =
  let url = if url = "" then "http://www.neo-modus.com/PublicHubList.config"
    else "" in
  let filename = Filename.temp_file "http_" ".tmp" in
  let file_oc = open_out filename in
  let file_size = ref 0 in
  Http_client.get_page (Url.of_string url) []
    (Http_client.default_headers_handler 
      (fun maxlen sock nread ->
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
    ))

module P = Gui_proto
  
let _ =
  server_ops.op_server_info <- (fun s ->
      if !!enable_directconnect then
      {
        P.server_num = (server_num s);
        P.server_network = network.network_num;
        P.server_ip = (match s.server_addr with
            AddrIp ip -> ip
          | AddrName _ -> Ip.null);
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
  
let recover_files () = ()
  
let ask_for_file file =
  List.iter (fun c ->
      match c.client_sock with 
      | Some sock -> ()
      | None -> 
          match c.client_server with
            None -> ()
          | Some s ->
              match s.server_sock with
                None -> ()
              | Some sock ->
                  server_send sock (
                    let module C = ConnectToMe in
                    ConnectToMeReq {
                      C.nick = s.server_last_nick;
                      C.ip = !!CO.client_ip;
                      C.port = !!dc_port;
                    }
                  );
                  server_send sock (
                    let module C = RevConnectToMe in
                    RevConnectToMeReq {
                      C.orig = s.server_last_nick;
                      C.dest = c.client_name;
                    }
                  );
  ) file.file_clients
  
let ask_for_files () =
  Hashtbl.iter (fun _ file ->
      ask_for_file file
  ) files_by_key