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

open Printf2
open Md4
open Options  
  
open BasicSocket
open TcpBufferedSocket

open CommonUploads
open CommonOptions
open CommonSearch
open CommonServer
open CommonComplexOptions
open CommonFile
open CommonDownloads
open CommonTypes
open CommonGlobals

open GnutellaNetwork
open GnutellaTypes
open GnutellaGlobals
open GnutellaOptions
open GnutellaProtocol
open GnutellaComplexOptions
open GnutellaProto
  
(*************************************************************************)
(*                                                                       *)
(*                         update_user                                   *)
(*                                                                       *)
(*************************************************************************)

let update_user t =
  let module Q = QueryReply in
  let user = new_user (match t.Q.dont_connect with
        Some true ->  Indirect_location ("", t.Q.guid, t.Q.ip, t.Q.port)
      | _ -> Known_location(t.Q.ip, t.Q.port))
  in
  user.user_nick <- (Md4.to_string t.Q.guid);
  user.user_speed <- t.Q.speed;
  user
  
(*************************************************************************)
(*                                                                       *)
(*                         update_client                                 *)
(*                                                                       *)
(*************************************************************************)

let update_client t =
  let module Q = QueryReply in
  let c = new_client (match t.Q.dont_connect with
        Some true ->  Indirect_location ("", t.Q.guid, t.Q.ip, t.Q.port)
      | _ -> Known_location(t.Q.ip, t.Q.port))
  in
  
  c.client_user.user_nick <- (Md4.to_string t.Q.guid);
  c.client_user.user_speed <- t.Q.speed;
  c
  
(*************************************************************************)
(*                                                                       *)
(*                         server_to_client                              *)
(*                                                                       *)
(*************************************************************************)

let server_to_client s p sock =
  set_lifetime sock 3600.;
  if !verbose_msg_servers then begin
      lprintf "RECEIVED server_to_client:\n";
      print p;
    end;

try 
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
              P.ip = (client_ip (Connection sock));
              P.port = !!client_port;
(* TODO: change this *)
              P.nfiles = 10;
              P.nkb = 10;
              P.ggep = [];
(*
              [
                Cobs.GGEP_GUE_guess 1; 
                Cobs.GGEP_VC_vendor ("MLDK", 2,4)]; *)
            });
        };
      if s.server_need_qrt then begin
          s.server_need_qrt <- false;
          GnutellaProto.send_qrt_sequence s false
        end
  
  
  | PongReq t ->
      
      let module P = Pong in
      (*      lprintf "FROM %s:%d" (Ip.to_string t.P.ip) t.P.port; *)
      if p.pkt_uid = s.server_ping_last then begin
          s.server_nfiles_last <- Int64.add s.server_nfiles_last (Int64.of_int t.P.nfiles);
          s.server_nkb_last <- s.server_nkb_last + t.P.nkb;
          s.server_nfiles <- (Int64.of_int t.P.nfiles);
          s.server_nkb <- t.P.nkb;
          server_must_update (as_server s.server_server);
        end
  
  | QueryReq t ->
      if !verbose_msg_servers then
        lprintf "SEARCH RECEIVED\n";
      begin
        try
          let files =
            let find_file q =
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
              let files = CommonUploads.query q in
              files
            in
            let rec iter_exts exts =
              match exts with
                [] -> find_file t
              | ext :: tail ->
                  if String.length ext > 20 && String2.starts_with ext "urn:" then
                    let uid = ext in
                    let sh = find_by_uid (Uid.of_string uid) in
                    [sh, IndexedSharedFiles.get_result sh.shared_info]
                  else
                    iter_exts tail
            in
            iter_exts t.Query.xml_query
          in
          let files = Array.of_list files in
          if !verbose_msg_servers then
            lprintf "%d replies found\n" (Array.length files); 

(* How many matches should we return ? Let's say 10. *)
          if files <> [||] then
            let module M = QueryReply in
            let module C = CommonUploads in
            let replies = ref [] in
            for i = 0 to min (Array.length files - 1) 9 do
              let sh, info = files.(i) in
              let infos = ref [] in
              List.iter (fun uid ->
                  match Uid.to_uid uid with
                    Sha1 _ | Bitprint _ -> 
                      infos := Uid.to_string uid :: !infos;
                  | Ed2k _ when GnutellaNetwork.accept_ed2kuid ->
                      infos := Uid.to_string uid :: !infos;
                  |  _ -> ()
              ) info.CommonUploads.shared_uids;
              if !infos <> [] then
                replies := {
                  M.index = info.C.shared_id;
                  M.size = info.C.shared_size;
                  M.name = Filename.basename sh.C.shared_codedname;
                  M.info = !infos;
                } :: !replies
            done;
            if !replies <> [] then
              let module P = QueryReply in
              let t = QueryReplyReq {
                  P.guid = !!client_uid;
                  P.ip = client_ip NoConnection;
                  P.port = !!client_port;
                  P.speed = 1300; 
                  P.files = !replies; 
                  P.vendor = "MLDK"; 
                  P.speed_measured = None; 
                  P.busy = None; 
                  P.stable = None; 
                  P.xml_reply = ""; 
                  P.support_chat = false; 
                  P.dont_connect = if !!dont_connect then Some true else None;
                } in
              let pp = { (new_packet t) with
                  pkt_hops = 0;
                  pkt_uid = p.pkt_uid;
                } in
              server_send s pp
              
        with Not_found -> ()
(*            lprintf "Query browse\n"    *)
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
              if String.length s > 20 && String2.starts_with s "urn:" then
                uids := (GnutellaGlobals.extract_uids s) @ !uids
(* probably XML. print to remember that we should be able to use this
information. *)
              else
                if !verbose then lprintf "info: %s\n" (String.escaped s);
          ) f.Q.info;
          
          if !verbose then
            begin
              lprintf "Received %d uids\n" (List.length !uids);
              List.iter (fun uid ->
                lprintf "   %s\n" (Uid.to_string uid);
              ) !uids;
            end;
          (*
          (try
              let file = Hashtbl.find files_by_key (f.Q.name, f.Q.size) in
              if !verbose_msg_servers then
                lprintf "++++++++++++ RECOVER FILE BY NAME %s +++++++++++\n" 
                file.file_name; 
              let c = update_client t in
              add_download file c (FileByIndex (f.Q.index, f.Q.name))
            with _ -> ());
*)
          
          List.iter (fun uid ->
              try
                let file = Hashtbl.find files_by_uid uid in
                if !verbose_msg_servers then
                  lprintf "++++++++++++ RECOVER FILE BY UID %s +++++++++++\n" 
                  file.file_name; 
                
                if file_size file = Int64.zero then begin
                    lprintf "Recover correct file size\n";
                    file.file_file.impl_file_size <- f.Q.size;
                    
                    failwith "GnutellaHandler: Recover with old size 0 not implemented"
(*                    
                    CommonSwarming.set_size file.file_swarmer f.Q.size;
                    file_must_update file; *)
                  end;

                if file.file_name = "" then begin
                    lprintf "Recover correct name\n";
                    file.file_name <- f.Q.name;
                    file.file_file.impl_file_best_name <- f.Q.name;
                    file_must_update file;
                  end;
                
                let c = update_client t in
                add_download file c (FileByIndex (f.Q.index,f.Q.name));
                GnutellaClients.connect_client c
              with _ -> 
                  if !verbose_unexpected_messages then
                    lprintf "No file with uid %s\n" (Uid.to_string uid)
          ) !uids;
          
          match s with
            None -> ()
          | Some (s, user) ->
              let r = new_result f.Q.name f.Q.size [] !uids [] in
              
              add_source r user (FileByIndex (f.Q.index,f.Q.name)) ;
              
              match s.search_search with
                UserSearch (s,_, _) ->
                  CommonInteractive.search_add_result true s r
              | _ -> ()
      ) t.Q.files;
  | _ -> ()
  
with e ->
  if !verbose then
  lprintf_nl "server_to_client exception %s" (Printexc2.to_string e)
  
(*************************************************************************)
(*                                                                       *)
(*                         init                                          *)
(*                                                                       *)
(*************************************************************************)

let init s sock gconn =       
  connected_servers := s :: !connected_servers;
  server_send s 
  (new_packet (PingReq Ping.SimplePing));        
  server_send s 
    { (new_packet (VendorReq (Vendor.Supported
            [
            "BEAR", 4,1;
            "BEAR", 7,1;
            "GTKG", 7,1;
            ]
        ))) with pkt_ttl = 1; };
  gconn.gconn_handler <- Reader
    (gnutella_handler parse (server_to_client s));
  List.iter (fun file -> server_recover_file file sock s) !current_files

  
(*************************************************************************)
(*                                                                       *)
(*                         udp_client_handler                            *)
(*                                                                       *)
(*************************************************************************)
  
let udp_client_handler ip port buf =
  if !verbose then
    lprintf "Unexpected UDP packet: \n%s\n" (Bytes.undafe_to_string (Bytes.escaped (Bytes.to_string buf)))
  
  
let update_shared_files () = ()
let declare_word _ = new_shared_words := true
