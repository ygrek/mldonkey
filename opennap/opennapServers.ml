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

open Md4
open CommonClient
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

let end_of_search s =
  match s.server_pending_searches with
    [] -> ()
  | _ :: tail ->
      s.server_pending_searches <- tail;
      match tail with
      | (ss, f) :: _ -> f ss
      | _ -> ()
  
let send_search fast s ss msg =
  if not (List.mem_assoc ss s.server_pending_searches) then
    let f ss =
      match s.server_sock with
        None -> ()
      | Some sock -> 
          Printf.printf "SENDING SEARCH TO %s" s.server_desc; print_newline ();
          s.server_searches <- Some ss;
          OP.debug_server_send sock (OP.SearchReq msg)
    in
    match s.server_pending_searches with
      [] ->
        s.server_pending_searches <- [ss, f];
        f ss
    | first :: tail ->
        if fast then
          s.server_pending_searches <- 
          first :: (ss, f) :: s.server_pending_searches
        else
          s.server_pending_searches <- s.server_pending_searches @ [ss, f]

        
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
      send_search false s ss t
  ) !connected_servers
      
let recover_files () =
  List.iter (fun file ->
      let keywords = 
        match stem file.file_name with 
          [] | [_] -> 
(*            Printf.printf "Not enough keywords to recover %s" f.file_name;
            print_newline (); *)
            [file.file_name]
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
          let keywords = 
            match stem file.file_name with 
              [] | [_] -> 
(*                Printf.printf "Not enough keywords to recover %s" f.file_name;
                print_newline (); *)
                [file.file_name]
            | l -> l
          in
          let module S = OP.Search in
          let t = { S.dummy_search with 
              S.artist = Some (String2.unsplit keywords ' ') } in
          send_search false s (Recover_file keywords) t
      ) !current_files;
      ()
      
  
let new_nick s =
  s.server_nick_num <- s.server_nick_num + 1;
  s.server_last_nick <- if s.server_nick_num = 0 then !!DO.client_name else
    Printf.sprintf "%s[%d]" !!DO.client_name s.server_nick_num  
    
let try_nick s sock =
  new_nick s;
  OP.server_send sock (OP.NickCheckReq s.server_last_nick)


let get_file_from_source c file =
(*  Printf.printf "GET FILE FROM SOURCE !!!!!!!!!!!!!!!!!!!!"; print_newline (); *)
  try
    if connection_can_try c.client_connection_control then begin
        connection_try c.client_connection_control;      
(*        Printf.printf "Opennap.get_file_from_source not implemented";  
print_newline (); *)
        List.iter (fun s ->
            match s.server_sock with
              None -> ()
            | Some sock ->
                connection_failed c.client_connection_control;      

(* emulate WinMX behavior *)
                OP.debug_server_send sock (OP.PrivateMessageReq (
                    let module PM = OP.PrivateMessage in
                    {
                      PM.nick = c.client_name;
                      PM.message = "//WantQueue";
                    }));
                
                OP.debug_server_send sock (OP.DownloadRequestReq (
                    let module DR = OP.DownloadRequest in
                    {
                      DR.nick = c.client_name;
                      DR.filename = List.assq file c.client_files;
                    }
                  ));
        ) c.client_user.user_servers;
        
        end
  with e ->
      Printf.printf "Exception %s in get_file_from_source" 
      (Printexc2.to_string e);
      print_newline ()

  
let download_file (r : result) =
  let file = new_file (Md4.random ()) r.result_name r.result_size in
(*  Printf.printf "DOWNLOAD FILE %s" f.file_name; print_newline (); *)
  if not (List.memq file !current_files) then begin
      current_files := file :: !current_files;
    end;
  List.iter (fun (user, filename) ->
      Printf.printf "Adding source %s (%d servers)" user.user_nick
        (List.length user.user_servers); print_newline ();
      let c = add_file_client file user  filename in
      get_file_from_source c file;
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

  (*
let update_source s t =
  let module Q = OP.SearchReply in
  let c = new_source s t.Q.nick t.Q.ip in
  
  c.client_link <- t.Q.link_type;
  c
    *)

let disconnect_server s =
      match s.server_sock with
        None -> ()
  | Some sock -> 
      
      (try close sock "user disconnect" with _ -> ());
      decr nservers;
(*      Printf.printf "%s:%d CLOSED received by server"
      (Ip.to_string s.server_ip) s.server_port; print_newline ();
*)
      DG.connection_failed (s.server_connection_control);
      s.server_sock <- None;
      set_server_state s NotConnected;
      connected_servers := List2.removeq s !connected_servers

let server_handler s sock event = 
  match event with
    BASIC_EVENT (CLOSED _) -> disconnect_server s      
  | _ -> ()

let client_to_server s t sock =
  match t with
    
  | OP.ErrorReq error ->
      Printf.printf "SERVER %s:%d %s" (Ip.to_string s.server_ip) 
      s.server_port s.server_net; print_newline ();
      Printf.printf "ERROR FROM SERVER: %s" error; print_newline () 
(*      Printf.printf "SERVER %s:%d %s" (Ip.to_string s.server_ip) 
      s.server_port s.server_net; print_newline ();
Printf.printf "MESSAGE FROM SERVER: %s" error; print_newline ()  *)

  | OP.MessageReq error -> 
      let msg = Printf.sprintf "From server %s [%s:%d]: %s\n"
          s.server_desc (Ip.to_string s.server_ip) s.server_port error in
      CommonEvent.add_event (Console_message_event msg)

    
  | OP.NickAlreadyUsedReq ->
(*      Printf.printf "NICK NAME ALREADY USED %d" s.server_nick; 
      print_newline (); *)
      try_login_on_server s sock;
(*
      s.server_nick <- s.server_nick + 1;
try_nick s sock;
*)
      
  | OP.NickInvalidReq ->
(*      Printf.printf "NICK NAME IS INVALID %s" !!DO.client_name; 
print_newline (); *)
      ()
      
  | OP.NickUnusedReq ->
(*      Printf.printf "NICK NAME ACCEPTED"; print_newline (); *)
      login_on_server s sock
      
  | OP.LoginAckReq mail ->
      set_rtimeout sock DG.half_day;
(*      Printf.printf "*****  CONNECTED %s  ******" mail; print_newline (); *)
      set_server_state s Connected_idle;
      connected_servers := s :: !connected_servers;
      
      (try
          let nshared_files = ref 0 in
          Hashtbl.iter (fun _ sh ->
              if !nshared_files > !!max_shared_files then raise Exit;

              let (tag,info) = sh.shared_format in
              OP.debug_server_send sock (OP.AddFileReq (
                  let module M = OP.AddFile in
                  {
                    M.filename = sh.shared_codedname;
                    M.md5 = Md5.to_string Md5.null;
                    M.size = Int64.of_int info.Mp3tag.filesize;
                    M.bitrate = info.Mp3tag.bitrate;
                    M.freq = 0;
                    M.length = info.Mp3tag.duration;
                  }
                ))
              
          ) shared_files;
          
        with _ -> ());
      
      recover_files_from_server s
      
  | OP.ServerStatsReq t ->
      DG.connection_ok s.server_connection_control;
      let module SS = OP.ServerStats in
      s.server_nfiles <- t.SS.files;
      s.server_nusers <- t.SS.users;
      s.server_size <- t.SS.size;
      
  | OP.SearchReplyReq t ->
      Printf.printf "***  SearchReplyReq ***"; print_newline ();
      let module SR = OP.SearchReply in
      begin
        match s.server_searches with 
          None -> assert false
        | Some (Normal_search q) -> 
            let user = new_user (Some s) t.SR.nick in
            user.user_link <- t.SR.link_type;
            let result = new_result (basename t.SR.filename) t.SR.size in
            add_source result user t.SR.filename;
            CommonInteractive.search_add_result q result.result_result;
        | Some (Recover_file _) -> 
            begin
              try
                let file = find_file (basename t.SR.filename) t.SR.size in 
                Printf.printf "++++++++++ RECOVER %s ++++++++" t.SR.filename;
                print_newline (); 
                
(*                Printf.printf "1"; print_newline (); *)
                let result = new_result (basename t.SR.filename) t.SR.size in
                let user = new_user (Some s) t.SR.nick in
                let c = add_file_client file user t.SR.filename in
                add_source result user t.SR.filename;
(*                Printf.printf "2"; print_newline (); *)
(*                Printf.printf "3"; print_newline (); *)
(*                Printf.printf "4"; print_newline (); *)
                get_file_from_source c file;
(*                Printf.printf "5"; print_newline (); *)
(*                Printf.printf "6"; print_newline (); *)
              with _ -> ()
            end
      end
      
  | OP.BrowseUserReplyReq t ->
      begin
        match s.server_browse_queue with
          c :: _ ->
            let module BU = OP.BrowseUserReply in
            let r = new_result (basename t.BU.filename) t.BU.size in
            add_source r c.client_user t.BU.filename;
            let rs = match c.client_all_files with
                None -> []
              | Some rs -> rs in
            if not (List.memq r rs) then begin
                c.client_all_files <- Some (r :: rs);
                client_new_file (as_client c.client_client) 
                (Filename.dirname t.BU.filename)
                (as_result r.result_result) 
              end
        | _ -> ()
      end
      
  | OP.EndOfSearchReplyReq ->
      Printf.printf "END OF SEARCH ON %s" s.server_desc; print_newline ();
      begin
        match s.server_searches with 
          None -> assert false
        | Some (Normal_search q) -> 
            s.server_searches <- None;
            end_of_search s
        | Some (Recover_file _) -> 
            s.server_searches <- None;
            end_of_search s
      end
  
  | OP.DownloadAckReq t ->
      
      let module DA = OP.DownloadAck in
      Printf.printf "DownloadAckReq %s !!!!!!!!!!!!!!!!!!!!!!!!" t.DA.nick; 
      print_newline (); 
       
      let c = new_client t.DA.nick in
      
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
          c.client_addr <- Some (ip, port);
          OpennapClients.connect_client c
        );
    
  | OP.BrowseUserReplyEndReq ->
      begin
        match s.server_browse_queue with
          [] -> ()
        | _ :: tail -> s.server_browse_queue <- tail
      end
      
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
      set_rtimeout sock !!server_connection_timeout;
      set_handler sock (BASIC_EVENT RTIMEOUT) (fun s ->
          close s "timeout"  
      );
      s.server_nick_num <- 0;
      s.server_searches <- None;
      s.server_pending_searches <- [];
      s.server_browse_queue <- [];
      try_nick s sock;  
(*      try_login_on_server s sock; *)
      s.server_sock <- Some sock;
    with e -> 
        Printf.printf "%s:%d IMMEDIAT DISCONNECT %s"
          (Ip.to_string s.server_ip) s.server_port
          (Printexc2.to_string e); print_newline ();
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

  
let connect_servers () = 
(*  Printf.printf "CONNECT SERVERS"; print_newline (); *)
  if !nservers < !!max_connected_servers then
    for i = !nservers to !!max_connected_servers do
      connect_one_server ()
    done
    
        
let ask_for_files () = 
  List.iter (fun file ->
      List.iter (fun c ->
          get_file_from_source c file
      ) file.file_clients
  ) !current_files;
  ()

    
let _ =
  server_ops.op_server_connect <- connect_server;
  server_ops.op_server_disconnect <- disconnect_server;
(*
(*  server_ops.op_server_query_users <- (fun s -> *)
      match s.server_sock with
        None -> ()
      | Some sock ->
          server_send sock (GetNickListReq)
  );
(*  server_ops.op_server_users <- (fun s -> *)
      List2.tail_map (fun u -> as_user u.user_user) s.server_users
);
  *)
  server_ops.op_server_remove <- (fun s ->
      disconnect_server s;
      server_remove s
  );
  
  network.op_network_connected <- (fun _ -> !connected_servers != []);
  network.op_network_save_complex_options <- (fun _ -> ())
  
  