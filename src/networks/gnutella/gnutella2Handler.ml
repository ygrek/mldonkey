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
open Gnutella2Proto

let update_client u =
  new_client u.user_kind

let gnutella_uid md4 =
  let md4 = Md4.to_string md4 in
  let s0_8 = String.sub md4 0 8 in
  let s8_12 = String.sub md4 8 4 in
  let s12_16 = String.sub md4 12 4 in
  let s16_20 = String.sub md4 16 4 in
  let s20_32 = String.sub md4 20 12 in
  Printf.sprintf "%s-%s-%s-%s-%s" s0_8 s8_12 s12_16 s16_20 s20_32
  
let xml_profile () = 
  Printf.sprintf 
  "<?xml version=\"1.0\"?><gProfile xmlns=\"http://www.shareaza.com/schemas/GProfile.xsd\"><gnutella guid=\"%s\"/><identity><handle primary=\"%s\"/><name last=\"\" first=\"%s\"/></identity><location><political city=\"\"/></location><avatar path=\"\"/></gProfile>"
    (gnutella_uid !!client_uid)
  !!client_name
  !!client_name
  
let g2_packet_handler s sock gconn p = 
  let h = s.server_host in
  if !verbose_msg_servers then begin
      lprintf "Received %s packet from %s:%d: \n%s\n" 
        (match sock with Connection _ -> "TCP" | _ -> "UDP")
      (Ip.to_string h.host_ip) h.host_port
        (Print.print p);
    end;
  match p.g2_payload with 
  | PI -> 
      server_send sock s (packet PO []);
      if s.server_need_qrt && (match s.server_sock with
            Connection _ -> true | _ -> false) then begin
          s.server_need_qrt <- false;
          send_qrt_sequence s false
        end
  
  | PO -> ()
  | UPROC -> 
      server_send sock s 
        (packet UPROD [
          (packet (UPROD_XML (xml_profile ())) [])
        ])
  
  | UPROD -> ()
  
  | LNI ->
      List.iter (fun p ->
          match p.g2_payload with
            LNI_V v -> s.server_vendor <- v
          | _ -> ()
      ) p.g2_children;
      server_send sock s 
        (packet LNI [
          packet (LNI_NA (client_ip sock, !!client_port))  [];
          packet (LNI_GU !!client_uid) [];
          packet (LNI_V "MLDK") [];
          packet (LNI_LS (Int32.zero,Int32.zero)) [];
        ])          
  
  | QKA ->
      List.iter (fun c ->
          match c.g2_payload with
            QKA_QK key -> s.server_query_key <- UdpQueryKey key
          | _ -> ()
      ) p.g2_children;
(* Now, we can extend our current searches on this host *)
      Hashtbl.iter (fun _ ss ->
          if not (Intset.mem h.host_num ss.search_hosts) then 
            match ss.search_search with
            | UserSearch (_,words) ->
                server_send_query ss.search_uid words NoConnection s
            | FileWordSearch (_,words) -> ()
(*                server_send_query ss.search_uid words NoConnection s *)
            | FileUidSearch (file, uid) ->
                server_ask_uid NoConnection s ss.search_uid uid file.file_name
      ) searches_by_uid;

(*      
  | Q2 md4 ->
      if !verbose_msg_servers then
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
            if !verbose_msg_servers then
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
                  M.name = Filename.basename sh.C.shared_codedname;
                  M.info = !infos;
                } :: !replies
              done;
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
                  P.dont_connect = None;
                } in
              let pp = { (new_packet t) with
                  pkt_hops = 0;
                  pkt_uid = p.pkt_uid;
                } in
              server_send s pp
          
          with Not_found -> ()
        with Not_found -> ()
(*            lprintf "Query browse\n"    *)
      end
*)
  
  | KHL ->
      List.iter (fun c ->
          match c.g2_payload with
            KHL_NH (ip,port) 
          | KHL_CH ((ip,port),_) ->
              let h = new_host ip port true 2 in ()
          | _ -> ()
      ) p.g2_children;
      let children = ref [] in
      List.iter (fun s ->
          if s.server_vendor <> "" then
            let h = s.server_host in
            match server_state s with
              Connected _ ->
                let p = packet 
                    (KHL_CH 
                      ((h.host_ip, h.host_port), int32_time ()))
                  [
                    (packet (KHL_CH_V s.server_vendor) [])
                  ] in
                children := p :: !children
            | _ -> ()
      ) !g2_connected_servers;
      server_send sock s (
        packet KHL [
          (packet (KHL_TS (int64_time ())) !children) 
        ]
      )
  
  | QA suid ->
      let ss = try Some (
            let ss = Hashtbl.find searches_by_uid suid in
            ss.search_hosts <- Intset.add h.host_num ss.search_hosts;
            ss
          )
        with _ -> None in
      List.iter (fun c ->
          match c.g2_payload with
            QA_D ((ip,port),_)
          | QA_S ((ip,port),_) -> 
              let h = new_host ip port true 2 in
              h.host_connected <- last_time ();
              begin
                match ss with
                  None -> ()
                | Some ss ->
                    ss.search_hosts <- Intset.add h.host_num ss.search_hosts
              end
          
          | _ -> ()
      ) p.g2_children
  
  | QH2 (_, suid) ->

(*

- : Xml.xml =
XML ("audios",
 [("xsi:nonamespaceschemalocation",
  "http://www.limewire.com/schemas/audio.xsd")],
 [XML ("audio", [], [])])
  
Xml.parse_string
- : Xml.xml =
XML ("audios",
 [("xsi:nonamespaceschemalocation",
  "http://www.limewire.com/schemas/audio.xsd")],
 [XML ("audio",
   [("samplerate", "44100"); ("seconds", "125"); ("index", "0");
    ("bitrate", "128")],
   []);
  XML ("audio",
   [("title", "It&apos;s Not Unusal"); ("samplerate", "44100");
    ("track", "1"); ("seconds", "120"); ("artist", "Tom Jones");
    ("description", "Tom Jones, hehe"); ("album", "Mars Attacks Soundtrack");
    ("index", "1"); ("bitrate", "128"); ("genre", "Retro"); ("year", "1997")],
   [])])

- : Xml.xml =
XML ("audios",
 [("xsi:nonamespaceschemalocation",
  "http://www.limewire.com/schemas/audio.xsd")],
 [XML ("audio",
   [("samplerate", "44100"); ("seconds", "239"); ("index", "0");
    ("bitrate", "128")],
   [])])

*)
      
      let s = 
        try 
          let s = Hashtbl.find searches_by_uid suid in
          Some s with
          _ -> 
            lprintf "***** No Corresponding Search ****\n";
            None 
      in
      let user_nick = ref "" in
      let user_uid = ref Md4.null in
      let user_addr = ref None in
      let user_vendor = ref None in
      let user_files = ref [] in
      let xml_info = ref None in
      List.iter (fun c ->
          match c.g2_payload with
            QH2_GU uid -> user_uid := uid
          | QH2_NA addr -> user_addr := Some addr
          | QH2_V vendor -> user_vendor := Some vendor
          | QH2_UPRO -> 
              List.iter (fun c ->
                  match c.g2_payload with
                    QH2_UPRO_NICK nick -> user_nick := nick
                  | _ -> ()
              ) c.g2_children
          | QH2_H ->
              let res_urn = ref None in
              let res_url = ref "" in
              let res_size = ref None in
              let res_name = ref "" in
              List.iter (fun c ->
                  match c.g2_payload with
                    QH2_H_URN urn -> res_urn := Some urn
                  | QH2_H_URL url -> res_url := url
                  | QH2_H_DN s ->
                      res_name := s
                  | QH2_H_SZDN (sz, s) ->
                      if !res_size = None then 
                        res_size := Some sz;
                      res_name := s
                  | QH2_H_SZ sz -> res_size := Some sz
                  | _ -> ()
              ) c.g2_children;
              user_files := (!res_urn, !res_size, !res_name, !res_url, []) ::
              !user_files
          | QH2_MD xml ->
              begin
                try
                  let xml = Xml.parse_string xml in
                  xml_info := Some xml
                with e ->
                    lprintf "Exception %s while parsing: \n%s\n"
                      (Printexc2.to_string e) xml
                    
              end
          | _ -> ()              
      ) p.g2_children;
      
      let user_files = List.rev !user_files in
      let user_files = match !xml_info with
          None -> user_files
        | Some _ -> user_files in
      
      if !verbose_msg_servers then begin
          lprintf "Results Received: \n";
          List.iter (fun (urn, size, name, url, tags) ->
              lprintf "    %s [size %s] %s -- %s\n"
                name (match size with
                  None -> "??" | Some sz -> Int64.to_string sz) 
              (match urn with
                  None -> "no URN"
                | Some urn -> string_of_uid urn)
              url
          ) user_files;
        end;      
      
      let user = 
        let user = new_user (match !user_addr with
              None -> Indirect_location (!user_nick, !user_uid)
            | Some (ip,port) -> Known_location (ip,port))
        in
        (match !user_vendor with Some v -> user.user_vendor <- v | _ -> ());
        user.user_uid <- !user_uid;
        user.user_nick <- !user_nick; 
        user.user_gnutella2 <- true;
        user
      in
      
(*          lprintf "ADDING RESULTS\n";*)
      List.iter (fun (urn, size, name, url, tags) ->
(*              lprintf "NEW RESULT %s\n" f.Q.name; *)
          
          let url = match urn with
              None -> url
            | Some uid ->
                Printf.sprintf "/uri-res/N2R?%s" (string_of_uid uid)
          in
          
          (match size with
              None -> ()
            | Some size ->
                try
                  let file = Hashtbl.find files_by_key (name, size) in
                  lprintf "++++++++++++ RECOVER FILE BY KEY %s +++++++++++\n" 
                    file.file_name; 
                  let c = update_client user in
                  add_download file c (FileByUrl url)
                with _ -> ());
          
          (match urn with
              None -> ()
            | Some uid ->
                try
                  let file = Hashtbl.find files_by_uid uid in
                  lprintf "++++++++++++ RECOVER FILE BY UID %s +++++++++++\n" 
                    file.file_name; 
                  
                  (match size with
                      None -> ()
                    | Some size ->
                        if file_size file = Int64.zero then begin
                            lprintf "Recover correct file size\n";
                            file.file_file.impl_file_size <- size;
                            Int64Swarmer.set_size file.file_swarmer size;
                            file_must_update file;
                          end);
                  
                  let c = update_client user in
                  add_download file c (FileByUrl url)
                with _ -> ()
          );
          
          match s, size with
          | Some s, Some size ->
              let uids = match urn with 
                  None -> [] | Some uid -> [uid] in
              let r = new_result name size uids in
              
              add_source r user (FileByUrl url);
              
              begin
                match s.search_search with
                  UserSearch (s,_) ->
                    CommonInteractive.search_add_result s r.result_result
                | _ -> ()
              end
          | _ -> ()
      ) user_files;
      
  | _ -> 
      if !verbose_unknown_messages then
        lprintf "g2_packet_handler: unexpected packet %s\n"
        (Print.print p)

          
let udp_packet_handler ip port msg = 
  let h = new_host ip port true 2 in
  host_queue_add g2_active_udp_queue h (last_time ());
  h.host_connected <- last_time ();
(*  if !verbose_udp then
    lprintf "Received UDP packet from %s:%d: \n%s\n" 
      (Ip.to_string ip) port (Print.print msg);*)
  let s = new_server ip port in
  s.server_connected <- int32_time ();
  g2_packet_handler s NoConnection () msg
  (*
  match msg.g2_payload with
  | PI -> 
      udp_send ip port (packet PO [])
  | _ -> 
      if !verbose_unknown_messages then
        lprintf "g2_udp_packet_handler: unexpected packet \n%s\n"
          (Print.print msg)
*)      

let init s sock gconn = 
  g2_connected_servers := s :: !g2_connected_servers;
  gconn.gconn_handler <- 
    Reader (g2_handler (g2_packet_handler s (Connection sock)));
  server_send_ping s.server_sock s;
  server_send_ping NoConnection s;
  server_send NoConnection s (packet PI []);
  (match s.server_query_key with
      UdpQueryKey _  -> ()
    | _ -> host_send_qkr s.server_host);
  server_send (Connection sock) s (packet UPROC [])

  (*
    Gnutella.recover_files_from_server s;    
*)

(* A good session: PI, KHL, LNI *)