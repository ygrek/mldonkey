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
  
let g2_packet_handler s sock gconn p = 
  lprintf "Received packet: \n%s\n" (Print.print p);
  match p.g2_payload with 
  | PI -> server_send s (packet PO [])
  | UPROC -> server_send s (packet UPROD [])
  | LNI ->
      server_send s 
        (packet LNI [
          packet (LNI_NA (client_ip (Some sock), !!client_port))  [];
          packet (LNI_GU !!client_uid) [];
          packet (LNI_V "MLDK") [];
          packet (LNI_LS (Int32.zero,Int32.zero)) [];
        ])          
  
  | KHL ->
      List.iter (fun c ->
          match c.g2_payload with
            KHL_NH addr 
          | KHL_CH (addr,_) ->
              Fifo.put ultrapeers2_queue addr
          | _ -> ()
      ) p.g2_children
  
  | QH2 (_, suid) ->
      
      let user_nick = ref "" in
      let user_uid = ref Md4.null in
      let user_addr = ref None in
      let user_vendor = ref None in
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
              let res_url = ref None in
              let res_size = ref None in
              let res_name = ref None in
              List.iter (fun c ->
                  match c.g2_payload with
                    QH2_H_URN urn -> res_urn := Some urn
                  | QH2_H_URL url -> res_url := Some url
                  | QH2_H_DN (sz, dn,s) ->
                      let s =
                        if !res_size = None then 
                          (res_size := Some sz; dn)
                        else s
                      in
                      res_name := Some s
                  | QH2_H_SZ sz -> res_size := Some sz
                  | _ -> ()
              ) c.g2_children
          | _ -> ()              
      ) p.g2_children;

      (*
      let s = 
        try 
          let s = Hashtbl.find searches_by_uid suid in
          let user = new_user (match !user_addr with
                None -> Indirect_location (!user_nick, !user_uid)
              | Some (ip,port) -> Known_location (ip,port))
          in
          (match !user_vendor with Some v -> user.user_vendor <- v | _ -> ());
          user.user_uid <- !user_uid;
          user.user_nick <- !user_nick; | _ -> ());
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
*)      
(*

QH2 0  5EB574CD5F1CF57FFFC65EF897A93E00
  GU ACE4D70BD61D1B226D60D9FB555B097A
  NA 212.198.64.83:18130 
  V 'RAZA'
  NH
  NH
  NH
  HG 1 
    SS 0  2  0 
  HG 2 
    SS 0  2  0 
  UPRO
    NICK 'fab'
  BUP
  H
    URN 'urn:bitprint:WIXYJFVJMIWNMUWPRPBGUTODIV52RMJA.CN25MLNU3XNN7IHKZMNOA63XG6SKDJ2W7Z3HONA'
    G 1 
    URL ''


*)
      
      
  | _ -> 
      lprintf "g2_packet_handler: unexpected packet %s\n"
        (Print.print p)


let init s sock gconn = 
  gconn.gconn_handler <- Reader (g2_handler (g2_packet_handler s sock))

(* A good session: PI, KHL, LNI *)