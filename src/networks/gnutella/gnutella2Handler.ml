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
open Gnutella2

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
  lprintf "Received %s packet from %s:%d: \n%s\n" 
    (match sock with Connection _ -> "TCP" | _ -> "UDP")
  (Ip.to_string h.host_ip) h.host_port
  (Print.print p);
  match p.g2_payload with 
  | PI -> 
      server_send sock s (packet PO []);
      if s.server_need_qrt && (match s.server_sock with
            Connection _ -> true | _ -> false) then begin
          s.server_need_qrt <- false;
          Gnutella.send_qrt_sequence s
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
      ) !connected_servers;
      server_send sock s (
        packet KHL [
          (packet (KHL_TS (int64_time ())) !children) 
        ]
      )
  
  | QH2 (_, suid) ->
      
      let user_nick = ref "" in
      let user_uid = ref Md4.null in
      let user_addr = ref None in
      let user_vendor = ref None in
      let user_files = ref [] in
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
                  | QH2_H_DN (sz, dn,s) ->
                      let s =
                        if !res_size = None then 
                          (res_size := Some sz; dn)
                        else s
                      in
                      res_name := s
                  | QH2_H_SZ sz -> res_size := Some sz
                  | _ -> ()
              ) c.g2_children;
              user_files := (!res_urn, !res_size, !res_name, !res_url) ::
              !user_files
          | _ -> ()              
      ) p.g2_children;
      
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
      
      let s = 
        try 
          let s = Hashtbl.find searches_by_uid suid in
          Some s with
          _ -> None 
      in
(*          lprintf "ADDING RESULTS\n";*)
      List.iter (fun (urn, size, name, url) ->
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
              
              CommonInteractive.search_add_result s.search_search 
                r.result_result;
          | _ -> ()
      ) !user_files;
      
  | _ -> 
      if !verbose_unknown_messages then
      lprintf "g2_packet_handler: unexpected packet %s\n"
        (Print.print p)

          
let udp_packet_handler ip port msg = 
  let h = new_host ip port true 2 in
  host_queue_add ultrapeers_recent_queue h (last_time ());
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
  gconn.gconn_handler <- 
    Reader (g2_handler (g2_packet_handler s (Connection sock)));
  server_send_ping s.server_sock s;
  server_send_ping NoConnection s;
  server_send NoConnection s (packet PI []);
  server_send (Connection sock) s (packet UPROC [])


(* A good session: PI, KHL, LNI *)