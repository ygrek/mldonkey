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

open CommonSwarming
open Printf2
open Md4
open CommonOptions
open CommonSearch
open CommonServer
open CommonComplexOptions
open CommonFile
open BasicSocket
open TcpBufferedSocket

open CommonTypes
open CommonGlobals
open Options
open GnutellaTypes
open GnutellaGlobals
open GnutellaOptions
open GnutellaProtocol
open GnutellaComplexOptions

  
open Gnutella1

let update_user t =
  let module Q = QueryReply in
  let user = new_user (match t.Q.dont_connect with
        Some true ->  Indirect_location ("", t.Q.guid)
      | _ -> Known_location(t.Q.ip, t.Q.port))
  in
  user.user_speed <- t.Q.speed;
  user

let update_client t =
  let module Q = QueryReply in
  let c = new_client (match t.Q.dont_connect with
        Some true ->  Indirect_location ("", t.Q.guid)
      | _ -> Known_location(t.Q.ip, t.Q.port))
  in
  
  c.client_user.user_speed <- t.Q.speed;
  c

let server_to_client s p sock =
  set_lifetime sock 3600.;
  if !verbose_msg_servers then begin
      lprintf "server_to_client\n";
      print p;
    end;
  match p.pkt_payload with
  | PingReq t ->
      if p.pkt_hops <= 3 then
        server_send s {
          p with 
          pkt_hops = 0;
          pkt_type = PONG;
          pkt_payload = (
            let module P = Pong in
            PongReq {
              P.ip = (DO.client_ip (Some sock));
              P.port = !!client_port;
              P.nfiles = 10;
              P.nkb = 10;
            });
        };
      if s.server_need_qrt then begin
          s.server_need_qrt <- false;
          Gnutella.send_qrt_sequence s
        end
  
  
  | PongReq t ->
      
      let module P = Pong in
(*      lprintf "FROM %s:%d" (Ip.to_string t.P.ip) t.P.port; *)
      if p.pkt_uid = s.server_ping_last then begin
          s.server_nfiles_last <- s.server_nfiles_last + t.P.nfiles;
          s.server_nkb_last <- s.server_nkb_last + t.P.nkb
        end
  
  | QueryReq t ->
(*      lprintf "REPLY TO QUERY NOT IMPLEMENTED YET :(\n";*)
      lprintf "SEARCH RECEIVED\n";
      begin
        try
          let q = 
            let q = 
              match String2.split_simplify t.Query.keywords ' ' with
                [] -> raise Not_found
              | s :: tail ->
                  List.fold_left (fun q s ->
                      QAnd (q, (QHasWord s))
                  ) (QHasWord s) tail
            in
(*
            match t.Search.sizelimit with
            | NoLimit -> q
            | AtMost n -> 
                QAnd (QHasMaxVal (CommonUploads.filesize_field, n),q)
            | AtLeast n -> 
QAnd (QHasMinVal (CommonUploads.filesize_field, n),q)
*) 
            q
          in
          try
            let files = CommonUploads.query q in
            lprintf "%d replies found\n" (Array.length files); 

(* How many matches should we return ? Let's say 10. *)
            if files <> [||] then
              let module M = QueryReply in
              let module C = CommonUploads in
              let replies = ref [] in
              for i = 0 to mini (Array.length files - 1) 9 do
                let sh = files.(i) in
                let infos = ref [] in
                List.iter (fun uid ->
                    match uid with
                      Sha1 (s, _) -> infos := s :: !infos;
                    |  _ -> ()
                ) sh.CommonUploads.shared_uids;
                replies := {
                  M.index = sh.C.shared_id;
                  M.size = sh.C.shared_size;
                  M.name = sh.C.shared_codedname;
                  M.info = !infos;
                } :: !replies
              done;
              let module P = QueryReply in
              let t = QueryReplyReq {
                  P.guid = !!client_uid;
                  P.ip = DO.client_ip None;
                  P.port = !!client_port;
                  P.speed = 1300; 
                  P.files = !replies; 
                  P.vendor = "MLDK"; 
                  P.speed_measured = None; 
                  P.busy = None; 
                  P.stable = None; 
                  P.xml_reply = ""; 
                  P.support_chat = false; 
                  P.dont_connect = None;
                } in
              let pp = { (new_packet t) with
                  pkt_hops = 0;
                  pkt_uid = p.pkt_uid;
                } in
              server_send s pp
          
          with Not_found -> ()
        with Not_found ->
            lprintf "Query browse\n"   
      end

(* GUID + Index of file to be pushed + ip + port *)
  | PushReq t ->
      let uid = t.Push.guid in
      let index = t.Push.index in
      let ip = t.Push.ip in
      let port = t.Push.port in
      GnutellaClients.push_connection uid index ip port
  
  | QueryReplyReq t ->
(*      lprintf "REPLY TO QUERY\n";*)
      let module Q = QueryReply in
      let s = 
        try 
          let s = Hashtbl.find searches_by_uid p.pkt_uid in
          let user = update_user t in
          Some (s, user) with
          _ -> None 
      in
(*          lprintf "ADDING RESULTS\n";*)
      List.iter (fun f ->
(*              lprintf "NEW RESULT %s\n" f.Q.name; *)
          
          let uids = ref [] in
          List.iter (fun s ->
              if s.[0] = '{' || s.[0] = '<' then begin
(* probably XML. print to remember that we should be able to use this
information. *)
                  lprintf "xml of result: %s\n" (String.escaped s);
                end else
                uids := (extract_uids s) @ !uids
          ) f.Q.info;
          
          (try
              let file = Hashtbl.find files_by_key (f.Q.name, f.Q.size) in
              lprintf "++++++++++++ RECOVER FILE BY KEY %s +++++++++++\n" 
                file.file_name; 
              let c = update_client t in
              add_download file c (FileByIndex (f.Q.index, f.Q.name))
            with _ -> ());
          
          List.iter (fun uid ->
              try
                let file = Hashtbl.find files_by_uid uid in
                lprintf "++++++++++++ RECOVER FILE BY UID %s +++++++++++\n" 
                  file.file_name; 
                
                if file_size file = Int64.zero then begin
                    lprintf "Recover correct file size\n";
                    file.file_file.impl_file_size <- f.Q.size;
                    Int64Swarmer.set_size file.file_swarmer f.Q.size;
                    file_must_update file;
                  end;
                
                let c = update_client t in
                add_download file c (FileByIndex (f.Q.index,f.Q.name));
              with _ -> ()
          ) !uids;
          
          match s with
            None -> ()
          | Some (s, user) ->
              let r = new_result f.Q.name f.Q.size !uids in
              
              
              add_source r user (FileByIndex (f.Q.index,f.Q.name));
              
              CommonInteractive.search_add_result s.search_search 
                r.result_result;
      ) t.Q.files;
  | _ -> ()

let init s sock gconn =       
  server_send s 
    { (new_packet (PingReq Ping.SimplePing)) with pkt_ttl = 4; };        
  gconn.gconn_handler <- Reader
    (gnutella_handler parse (server_to_client s))
  
    
