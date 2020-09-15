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

open AnyEndian
open Printf2
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
      do_if_connected s.server_sock (fun sock ->
          lprintf "SENDING SEARCH TO %s\n" s.server_desc; 
          s.server_searches <- Some ss;
          OP.debug_server_send sock (OP.SearchReq msg)
      )
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
(*            lprintf "Not enough keywords to recover %s\n" f.file_name; *)
            [file.file_name]
        | l -> l
      in
      ignore (send_query (Recover_file keywords) keywords)
  ) !current_files;
  ()
  
let recover_files_from_server s =
  do_if_connected  s.server_sock (fun sock ->
      List.iter (fun file ->
          let keywords = 
            match stem file.file_name with 
              [] | [_] -> 
(*                lprintf "Not enough keywords to recover %s\n" f.file_name; *)
                [file.file_name]
            | l -> l
          in
          let module S = OP.Search in
          let t = { S.dummy_search with 
              S.artist = Some (String2.unsplit keywords ' ') } in
          send_search false s (Recover_file keywords) t
      ) !current_files;
  )
  
let new_nick s =
  s.server_nick_num <- s.server_nick_num + 1;
  s.server_last_nick <- if s.server_nick_num = 0 then !!DO.global_login else
    Printf.sprintf "%s[%d]" !!DO.global_login s.server_nick_num  
    
let try_nick s sock =
  new_nick s;
  OP.server_send sock (OP.NickCheckReq s.server_last_nick)


let get_file_from_source c file =
(*  lprintf "GET FILE FROM SOURCE !!!!!!!!!!!!!!!!!!!!\n"; *)
  try
    if connection_can_try c.client_connection_control then begin
        connection_try c.client_connection_control;      
(*        lprintf "Opennap.get_file_from_source not implemented\n";  *)
        List.iter (fun s ->
            do_if_connected s.server_sock (fun sock ->
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
            )
        ) c.client_user.user_servers;
        
        end
  with e ->
      lprintf "Exception %s in get_file_from_source\n" 
      (Printexc2.to_string e)

  
let download_file hash (r : CommonTypes.result_info) =
  let file = new_file (Md4.random ())
   (List.hd r.result_names)
   r.result_size in
(*  lprintf "DOWNLOAD FILE %s\n" f.file_name;  *)
  if not (List.memq file !current_files) then begin
      current_files := file :: !current_files;
    end;
  begin
    let sources = Hashtbl.find result_sources r.result_num in
    List.iter (fun (user,filename) ->
      lprintf "Adding source %s (%d servers)\n" user.user_nick
        (List.length user.user_servers); 
      let c = add_file_client file user filename in
      get_file_from_source c file;
  ) !sources;
  end;
  as_file file.file_file

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

let disconnect_server s r =
  
  match s.server_sock with
    NoConnection -> ()
  | ConnectionWaiting token ->
      cancel_token token;
      s.server_sock <- NoConnection
  | Connection sock -> 
      
      (try close sock r with _ -> ());
      decr nservers;
(*      lprintf "%s:%d CLOSED received by server\n"
      (Ip.to_string s.server_ip) s.server_port; 
*)
      DG.connection_failed (s.server_connection_control);
      s.server_sock <- NoConnection;
      set_server_state s (NotConnected (r, -1));
      connected_servers := List2.removeq s !connected_servers

let server_handler s sock event = 
  match event with
    BASIC_EVENT (CLOSED r) -> disconnect_server s r
  | _ -> ()

let client_to_server s t sock =
  match t with
    
  | OP.ErrorReq error ->
      lprintf "SERVER %s:%d %s\n" (Ip.to_string s.server_ip) 
      s.server_port s.server_net; 
      lprintf "ERROR FROM SERVER: %s\n" error; 

  | OP.MessageReq error -> 
      let msg = Printf.sprintf "From server %s [%s:%d]: %s\n"
          s.server_desc (Ip.to_string s.server_ip) s.server_port error in
      CommonEvent.add_event (Console_message_event msg)

    
  | OP.NickAlreadyUsedReq ->
(*      lprintf "NICK NAME ALREADY USED %d\n" s.server_nick; *)
      try_login_on_server s sock;
(*
      s.server_nick <- s.server_nick + 1;
try_nick s sock;
*)
      
  | OP.NickInvalidReq ->
(*      lprintf "NICK NAME IS INVALID %s\n" !!DO.client_name; *)
      ()
      
  | OP.NickUnusedReq ->
      lprintf "NICK NAME ACCEPTED\n";
      login_on_server s sock
      
  | OP.LoginAckReq mail ->
      set_rtimeout sock Date.half_day_in_secs;
      lprintf "*****  CONNECTED %s  ******\n" mail;
      set_server_state s (Connected (-1));
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
      s.server_nfiles <- Int64.of_int t.SS.files;
      s.server_nusers <- Int64.of_int t.SS.users;
      s.server_size <- t.SS.size;
      server_must_update (as_server s.server_server)
      
  | OP.SearchReplyReq t ->
      lprintf "***  SearchReplyReq ***\n"; 
      let module SR = OP.SearchReply in
      begin
        match s.server_searches with 
          None -> assert false
        | Some (Normal_search q) -> 
            let user = new_user (Some s) t.SR.nick in
            user.user_link <- t.SR.link_type;
            let result = new_result (basename t.SR.filename) t.SR.size in
            add_source result user t.SR.filename;
            CommonInteractive.search_add_result true q result;
        | Some (Recover_file _) -> 
            begin
              try
                let file = find_file (basename t.SR.filename) t.SR.size in 
                lprintf "++++++++++ RECOVER %s ++++++++\n" t.SR.filename;
                
                let result = new_result (basename t.SR.filename) t.SR.size in
                let user = new_user (Some s) t.SR.nick in
                let c = add_file_client file user t.SR.filename in
                add_source result user t.SR.filename;
                get_file_from_source c file;
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
                r
              end
        | _ -> ()
      end
      
  | OP.EndOfSearchReplyReq ->
      lprintf "END OF SEARCH ON %s\n" s.server_desc; 
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
      lprintf "DownloadAckReq %s !!!!!!!!!!!!!!!!!!!!!!!!\n" t.DA.nick; 
       
      let c = new_client t.DA.nick in
      
      if t.DA.port = 0 then (
          lprintf "************** Must download indirectly  *************\n"; 
          OP.debug_server_send sock (OP.AlternateDownloadRequestReq (
              let module DR = OP.DownloadRequest in
              {
                DR.nick = t.DA.nick;
                DR.filename = t.DA.filename;
              }
            ));
        ) else (
          lprintf "************** Can download directly *************\n"; 
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
      lprintf "?????????Download Error %s %s ???????????\n" 
        t.DE.nick t.DE.filename; 
      
  | _ -> 
      lprintf "#################  UNUSED   ###############\n"; 
      OpennapProtocol.print t
      
let connect_server s =
  if can_open_connection connection_manager then
    let token =
      add_pending_connection connection_manager (fun token ->
          s.server_sock <- NoConnection;
          try
(*      lprintf "CONNECTING ONE SERVER\n"; *)
            DG.connection_try s.server_connection_control;
            incr nservers;
            let sock = TcpBufferedSocket.connect token "opennap to server" 
                (Ip.to_inet_addr s.server_ip) s.server_port 
                (server_handler s) (* Mftp_comm.server_msg_to_string*)  in
            set_server_state s Connecting;
            set_read_controler sock DG.download_control;
            set_write_controler sock DG.upload_control;
            
            set_reader sock (OpennapProtocol.opennap_handler (client_to_server s));
            set_rtimeout sock !!server_connection_timeout;
            set_handler sock (BASIC_EVENT RTIMEOUT) (fun s ->
                close s Closed_for_timeout
            );
            s.server_nick_num <- 0;
            s.server_searches <- None;
            s.server_pending_searches <- [];
            s.server_browse_queue <- [];
            try_nick s sock;  
(*      try_login_on_server s sock; *)
            s.server_sock <- Connection sock;
          with e -> 
              lprintf "%s:%d IMMEDIAT DISCONNECT %s"
                (Ip.to_string s.server_ip) s.server_port
                (Printexc2.to_string e); 
(*      lprintf "DISCONNECTED IMMEDIATLY\n"; *)
              decr nservers;
              s.server_sock <- NoConnection;
              set_server_state s (NotConnected (Closed_connect_failed, -1));
              DG.connection_failed s.server_connection_control
      )
    in
    s.server_sock <- ConnectionWaiting token
    
let rec connect_one_server () =
  if can_open_connection connection_manager then
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
              NoConnection -> connect_server s
            | _ -> ()
          end

  
let connect_servers () = 
(*  lprintf "CONNECT SERVERS\n"; *)
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
  server_ops.op_server_disconnect <- (fun s ->
                                          disconnect_server s Closed_by_user);
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
      disconnect_server s Closed_by_user;
      server_remove s
  );
  
  network.op_network_connected <- (fun _ -> !connected_servers != []);
  network.op_network_save_complex_options <- (fun _ -> ());
  network.op_network_update_options <- (fun _ -> ());
  network.op_network_save_sources <- (fun _ -> ())

(*
If you run a packet sniffer on WPNP (WinMX Peer Network Protocol) packets,
it will soon become apparent that these packets are not transmitted in cleartext.
For instance, search terms cannot directly be found in the packet stream.

The reason for this is that the packets are encoded using a simple XOR based algorithm.
It cannot really be called encryption, since there is no key except packet length.

The encoding algorithm in question first xors the first byte with the last, then repeatedly
xors a byte with its preceding byte, moving from the next-to-first byte to the last one,
one byte at a time. This is done five times. The procedure varies slightly the first time,
where the first byte is not XORed with the last byte, but rather with the packet[1] length.
*)

external winmx_encode : string -> int -> unit = "winmx_encode_ml"
external winmx_decode : string -> int -> unit = "winmx_decode_ml"
  
module Pandora = struct
    
    type t = UDP | TCP
    
    type cnx = {
        ip1 : string;
        port1 : int;
        ip2 : string;
        port2 : int;
        packets_in : Buffer.t;
        packets_out : Buffer.t;
      }
    let connections = Hashtbl.create 13
    
    let rec iter s pos =
      if s.[pos] = '\n' then
        if s.[pos+1] = '\n' then pos+2
        else
        if s.[pos+1] = '\r' then
          if s.[pos+2] = '\n' then pos+3
          else iter s (pos+1)
        else iter s (pos+1)
      else 
      if s.[pos] = '\r' then
        if s.[pos] = '\n' then
          if s.[pos+1] = '\n' then pos+2
          else
          if s.[pos+1] = '\r' then
            if s.[pos+2] = '\n' then pos+3
            else iter s (pos+1)
          else iter s (pos+1)
        else
          iter s (pos+1)
      else iter s (pos+1)
    
    let hescaped s =
      String2.replace_char s '\r' ' ';s
    
    let parse s_out s_in = 
      lprintf "OUTPUT:\n";

      lprintf "INPUT:\n";
      dump s_in
    
    let commit () =  
      Hashtbl.iter (fun _ cnx ->
          try
            lprintf "CONNECTION %s:%d --> %s:%d\n" 
              cnx.ip1 cnx.port1 cnx.ip2 cnx.port2;
            
            let s = Buffer.contents cnx.packets_out in
            let len = String.length s in
            
            if String2.starts_with s "GET" || 
              String2.starts_with s "POST" then begin
                (*
                lprintf "Http connect to\n";
                let h1 = iter s 0 in
                lprintf "Header 1: \n%s\n" (hescaped (String.sub s 0 h1));
                
                let s = Buffer.contents cnx.packets_in in
                if String2.starts_with s "HTTP" then begin
                    lprintf "Http connected from\n";
                    let h1 = iter s 0 in
                    lprintf "Header 1: \n%s\n" (hescaped (String.sub s 0 h1));
                  end 
                else
lprintf "bad HTTP reply\n"*)
                ()
              end else begin
                parse 
                  (Buffer.contents cnx.packets_out) 
                (Buffer.contents cnx.packets_in);
              end
          with           
          | e ->
              lprintf "Exception %s\n" (Printexc2.to_string e)
      ) connections
    
    let new_packet (kind:t) (number:int) ip1 port1 ip2 port2 data = 
      match kind with
        UDP -> 
          begin
            try
(*
              lprintf "New packet (len=%d):\n%s\n" 
                (String.length data) (String.escaped data); *)
              ()
            with e ->
(*                lprintf "Could not parse UDP packet:\n"; *)
                ()
          end
      | TCP -> 
(*
          lprintf "\nNew packet %s:%d -> %s:%d (len=%d):\n"
            ip1 port1 ip2 port2
            
            (String.length data);
          dump data;
*)          
          (
            let out_packet = (ip1, port1, ip2, port2) in
            let in_packet = (ip2, port2, ip1, port1) in
            
            try
              let cnx =  Hashtbl.find connections out_packet in
              Buffer.add_string cnx.packets_out data; 
              ()
            with _ ->
                try
                  let cnx =  Hashtbl.find connections in_packet in
                  Buffer.add_string cnx.packets_in data 
                with _ ->
                    let cnx = {
                        ip1 = ip1;
                        port1 = port1;
                        ip2 = ip2;
                        port2 = port2;
                        packets_out = Buffer.create 100;
                        packets_in = Buffer.create 100;
                      } in
                    Hashtbl.add connections out_packet cnx;
                    Buffer.add_string cnx.packets_out data);

          (*
          let len = String.length data in
          for i = 0 to len -1 do
            let j = len - i in
            let data = String.sub data i j in
            winmx_decode data (String.length data);
            lprintf "DECODED DATA [%d,%d]:\n" i j;
            dump data;
done; *)
          ()
          
  end
  
  
  