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

open CommonSearch
open CommonServer
open CommonTypes
open CommonResult
open CommonComplexOptions
open CommonFile
open CommonGlobals
open Options
open BasicSocket
open TcpBufferedSocket
open OpennapTypes
open OpennapGlobals
open OpennapOptions
open OpennapComplexOptions

module DG = CommonGlobals
module DO = CommonOptions
module OP = OpennapProtocol
module OT = OpennapTypes
  
let send_search s ss msg =
  if not (List.mem_assoc ss s.server_pending_searches) then
    let f ss =
      s.server_searches <- Some ss;
(*      add_timer 5.0 (fun _ -> *)
          match s.server_sock with
            None -> ()
          | Some sock -> 
          OP.debug_server_send sock (OP.SearchReq msg)
(*          ) *)
  in
  match s.server_searches with
      None -> f ss
    | Some _ -> 
        s.server_pending_searches <- s.server_pending_searches @ [ss, f]

let update_searches s =
  match s.server_pending_searches with
    [] -> ()
  | (ss, f) :: tail ->
      s.server_pending_searches <- tail;
      f ss
        
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

let send_query ss words =
  let module S = OP.Search in
  let t = { S.dummy_search with 
      S.artist = Some (String2.unsplit words ' ') } in
  List.iter (fun s ->
      send_search s ss t
  ) !connected_servers
      
let recover_files () =
  List.iter (fun file ->
      let r = file.file_result in
      let f = r.result_file in
      let keywords = 
        match stem f.file_name with 
          [] | [_] -> 
            Printf.printf "Not enough keywords to recover %s" f.file_name;
            print_newline ();
            [f.file_name]
        | l -> l
      in
      ignore (send_query (Recover_file keywords) keywords)
  ) !current_files;
  ()
  
let recover_files_from_server s =
  match s.server_sock with
    None -> ()
  | Some sock -> 
      List.iter (fun file ->
          let r = file.file_result in
          let f = r.result_file in
          let keywords = 
            match stem f.file_name with 
              [] | [_] -> 
                Printf.printf "Not enough keywords to recover %s" f.file_name;
                print_newline ();
                [f.file_name]
            | l -> l
          in
          let module S = OP.Search in
          let t = { S.dummy_search with 
              S.artist = Some (String2.unsplit keywords ' ') } in
          send_search s (Recover_file keywords) t
      ) !current_files;
      ()
      
  
let new_nick s =
  s.server_nick_num <- s.server_nick_num + 1;
  s.server_last_nick <- if s.server_nick_num = 0 then !!DO.client_name else
    Printf.sprintf "%s[%d]" !!DO.client_name s.server_nick_num  
    
let try_nick s sock =
  new_nick s;
  OP.server_send sock (OP.NickCheckReq s.server_last_nick)


let get_file_from_source src file r =
  Printf.printf "GET FILE FROM SOURCE !!!!!!!!!!!!!!!!!!!!"; print_newline ();
  try
    if connection_can_try src.source_connection_control then begin
        connection_try src.source_connection_control;      
        Printf.printf "Opennap.get_file_from_source not implemented"; 
        print_newline ();
        let s = src.source_server in
        match s.server_sock with
          None -> ()
        | Some sock ->
            connection_failed src.source_connection_control;      
            if not (List.mem_assoc src.source_nick s.server_sources) then
                s.server_sources <- (src.source_nick,src) :: s.server_sources;

(* emulate WinMX behavior *)
            OP.debug_server_send sock (OP.PrivateMessageReq (
                let module PM = OP.PrivateMessage in
                {
                  PM.nick = src.source_nick;
                  PM.message = "//WantQueue";
                }));
            
            OP.debug_server_send sock (OP.DownloadRequestReq (
                let module DR = OP.DownloadRequest in
                {
                  DR.nick = src.source_nick;
                  DR.filename = List.assq r src.source_files;
                }
              ));
      end
  with e ->
      Printf.printf "Exception %s in get_file_from_source" 
      (Printexc.to_string e);
      print_newline ()

  
let download_file (r : result) =
  let f = r.result_file in
  let file = new_file (Md4.random ()) f.file_name f.file_size in
  Printf.printf "DOWNLOAD FILE %s" f.file_name; print_newline ();
  if not (List.memq file !current_files) then begin
      current_files := file :: !current_files;
    end;
  List.iter (fun src ->
      add_download file src ""; (* 0 since index is already known. verify ? *)
      get_file_from_source src file r;
  ) r.result_sources;
  ()

let login_on_server s sock =
  new_nick s;
  OP.server_send sock (OP.NewUserLoginReq (
      let module NUL = OP.NewUserLogin in
      {
        NUL.nick = s.server_last_nick;
        NUL.password = !!client_password;
        NUL.port = !!client_port;
        NUL.client_info = !!client_info;
        NUL.link_type = OT.LinkUnknown;
        NUL.email = "nomail";
      }))

let try_login_on_server s sock =
  new_nick s;
  OP.server_send sock (OP.LoginReq (
      let module NUL = OP.Login in
      {
        NUL.nick = s.server_last_nick;
        NUL.password = !!client_password;
        NUL.port = !!client_port;
        NUL.client_info = !!client_info;
        NUL.link_type = OT.LinkCable;
      }))
  
let update_source s t =
  let module Q = OP.SearchReply in
  let src = new_source s t.Q.nick t.Q.ip in
  
  src.source_link <- t.Q.link_type;
  src
  
let server_handler s sock event = 
  match event with
    BASIC_EVENT (CLOSED _) ->
      
      if s.server_sock <> None then decr nservers;
(*      Printf.printf "%s:%d CLOSED received by server"
      (Ip.to_string s.server_ip) s.server_port; print_newline ();
*)
      DG.connection_failed (s.server_connection_control);
      s.server_sock <- None;
      set_server_state s NotConnected;
      connected_servers := List2.removeq s !connected_servers;
  | _ -> ()

let client_to_server s t sock =
  if !DG.ip_verified < 10 then DG.verify_ip sock;
  match t with
  | OP.ErrorReq error ->
      Printf.printf "SERVER %s:%d %s" (Ip.to_string s.server_ip)
      s.server_port s.server_net; print_newline ();
      Printf.printf "ERROR FROM SERVER: %s" error; print_newline () 
  | OP.MessageReq error -> ()
(*      Printf.printf "SERVER %s:%d %s" (Ip.to_string s.server_ip) 
      s.server_port s.server_net; print_newline ();
      Printf.printf "MESSAGE FROM SERVER: %s" error; print_newline ()  *)
  | OP.NickAlreadyUsedReq ->
(*      Printf.printf "NICK NAME ALREADY USED %d" s.server_nick; 
      print_newline (); *)
      try_login_on_server s sock;
(*
      s.server_nick <- s.server_nick + 1;
try_nick s sock;
  *)
  | OP.NickInvalidReq ->
(*      Printf.printf "NICK NAME IS INVALID %s" !!DO.client_name; *)
      print_newline ();
  | OP.NickUnusedReq ->
(*      Printf.printf "NICK NAME ACCEPTED"; print_newline (); *)
      login_on_server s sock
  | OP.LoginAckReq mail ->
      set_rtimeout sock DG.half_day;
      Printf.printf "*****  CONNECTED %s  ******" mail; print_newline (); 
      set_server_state s Connected_idle;
      connected_servers := s :: !connected_servers;
      recover_files_from_server s
  | OP.ServerStatsReq t ->
      DG.connection_ok s.server_connection_control;
      let module SS = OP.ServerStats in
      s.server_nfiles <- t.SS.files;
      s.server_nusers <- t.SS.users;
      s.server_size <- t.SS.size;
  | OP.SearchReplyReq t ->
      let module SR = OP.SearchReply in
      begin
        match s.server_searches with 
          None -> assert false
        | Some (Normal_search q) -> 
            let src = update_source s t in
            let result = new_result (basename t.SR.filename) t.SR.size in
            add_source result src t.SR.filename;
            search_add_result q result.result_result;
        | Some (Recover_file _) -> 
            begin
              try
                let file = find_file (basename t.SR.filename) t.SR.size in 
                Printf.printf "++++++++++ RECOVER %s ++++++++" t.SR.filename;
                print_newline ();
                let r = file.file_result in
                Printf.printf "1"; print_newline ();
                let src = update_source s t in
                add_source r src t.SR.filename;
                Printf.printf "2"; print_newline ();
                Printf.printf "3"; print_newline ();
                add_download file src ""; (* 0 since index is already known. verify ? *)
                Printf.printf "4"; print_newline ();
                get_file_from_source src file r;
                Printf.printf "5"; print_newline ();
                Printf.printf "6"; print_newline ();
              with _ -> ()
            end
      end
  | OP.EndOfSearchReplyReq ->
      begin
        match s.server_searches with 
          None -> assert false
        | Some (Normal_search q) -> 
            s.server_searches <- None;
            update_searches s
        | Some (Recover_file _) -> 
            s.server_searches <- None;
            update_searches s
      end
  
  | OP.DownloadAckReq t ->
      
      begin
        let module DA = OP.DownloadAck in
        Printf.printf "DownloadAckReq %s !!!!!!!!!!!!!!!!!!!!!!!!" t.DA.nick; 
        print_newline (); 
        try
          let src = List.assoc t.DA.nick s.server_sources in
          
          if t.DA.port = 0 then (
              Printf.printf "************** Must download indirectly  *************"; 
              print_newline ();
              OP.debug_server_send sock (OP.AlternateDownloadRequestReq (
                  let module DR = OP.DownloadRequest in
                  {
                    DR.nick = t.DA.nick;
                    DR.filename = t.DA.filename;
                  }
                ));
            ) else (
              Printf.printf "************** Can download directly *************"; 
              print_newline ();
              let ip = t.DA.ip in
              let port = t.DA.port in
              OpennapClients.connect_client src ip port
            );
        with e ->
            Printf.printf "SOURCES: "; print_newline ();
            List.iter (fun (nick, src) ->
                Printf.printf "{ %s }" nick; print_newline ();
            ) s.server_sources;
            Printf.printf "Exception %s in DownloadAckReq" 
              (Printexc.to_string e); print_newline (); 
      end;
      
      
  | OP.DownloadErrorReq t ->
      begin
        ()
      end;
      let module DE = OP.DownloadError in
      Printf.printf "?????????Download Error %s %s ???????????" t.DE.nick t.DE.filename;
      print_newline ();
      
  | _ -> 
      Printf.printf "#################  UNUSED   ###############"; 
      print_newline ();
      OpennapProtocol.print t
      
let connect_server s =
  if DG.can_open_connection () then
    try
(*      Printf.printf "CONNECTING ONE SERVER"; print_newline ();  *)
      DG.connection_try s.server_connection_control;
      incr nservers;
      DG.printf_char 's'; 
      let sock = TcpBufferedSocket.connect "opennap to server" (
          Ip.to_inet_addr s.server_ip) s.server_port 
          (server_handler s) (* Mftp_comm.server_msg_to_string*)  in
      set_server_state s Connecting;
      set_read_controler sock DG.download_control;
      set_write_controler sock DG.upload_control;
      
      set_reader sock (OpennapProtocol.opennap_handler (client_to_server s));
      set_rtimeout sock 5.;
      set_handler sock (BASIC_EVENT RTIMEOUT) (fun s ->
          close s "timeout"  
      );
      s.server_nick_num <- 0;
      s.server_searches <- None;
      s.server_pending_searches <- [];
      try_nick s sock;  
(*      try_login_on_server s sock; *)
      s.server_sock <- Some sock;
    with e -> 
        Printf.printf "%s:%d IMMEDIAT DISCONNECT %s"
          (Ip.to_string s.server_ip) s.server_port
          (Printexc.to_string e); print_newline ();
(*      Printf.printf "DISCONNECTED IMMEDIATLY"; print_newline (); *)
        decr nservers;
        s.server_sock <- None;
        set_server_state s NotConnected;
        DG.connection_failed s.server_connection_control
        
let rec connect_one_server () =
  if DG.can_open_connection () then
    match !servers_list with
      [] ->
        servers_list := !current_servers;
        if !servers_list = [] then raise Not_found;
        connect_one_server ()
    | s :: list ->
        servers_list := list;
        if DG.connection_can_try s.server_connection_control then
          begin
            match s.server_sock with
              Some _ -> ()
            | None -> 
                connect_server s
          end

  
let connect_servers () = Printf.printf "CONNECT SERVERS"; print_newline ();
  if !nservers < !!max_connected_servers then
    for i = !nservers to !!max_connected_servers do
      connect_one_server ()
    done
    
        
let ask_for_files () = 
  List.iter (fun file ->
      let r = file.file_result in
      let f = r.result_file in
      List.iter (fun s ->
          get_file_from_source s file r
      ) r.result_sources
  ) !current_files;
  ()

  