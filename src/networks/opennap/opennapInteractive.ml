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
open CommonSearch
open CommonGlobals
open CommonUser
open CommonClient
open CommonResult
open CommonServer
open CommonTypes
open CommonComplexOptions
open CommonFile
open OpennapGlobals
open Options
open OpennapComplexOptions
open OpennapTypes
module OG = OpennapGlobals

module OS = OpennapServers
module OP = OpennapProtocol
module OC = OpennapClients

let  _ =
  network.op_network_search <- (fun q  buf ->
      let query = q.search_query in
      let module S = OP.Search in
      let t = S.dummy_search in
      let rec iter t q =
        match q with
        | QOr (q1,q2) 
        | QAnd (q1, q2) ->
            iter (iter t q1) q2
        | QAndNot (q1,q2) -> iter t q1 
        | QHasWord w ->
            begin
              match t.S.artist with
                None -> { t with S.artist = Some w }
              | Some s -> { t with S.artist = Some (Printf.sprintf "%s %s" s w)}

(*
          match t.S.title, t.S.artist with
            None, _ -> { t with S.title = Some w }
          | Some s, None -> { t with S.artist = Some w }
          | Some s, _ ->
{t with S.title = Some (Printf.sprintf "%s %s" s w)}
  *)
            end
        | QHasField(field, value) ->
            lprintf "******  HAS FIELD  %s %s ********\n" 
            (string_of_field field) value; 
            begin
              match field with
                Field_Album | Field_Title -> t
              | Field_Artist | _ -> t
            end
        | QHasMinVal (field, value) ->
            begin
              match field with
                Field_unknown "bitrate" ->  
                  { t with S.bitrate = Some (Int64.to_int value, OP.AtLeast) };
              | Field_Size -> t
              | _ -> t
            end
        | QHasMaxVal (field, value) ->
            begin
              match field with
                Field_unknown "bitrate" -> 
                  { t with S.bitrate = Some (Int64.to_int value, OP.AtBest) };
              | Field_Size -> t
              | _ -> t
            end
        | QNone ->
            lprintf "OpennapInteractive.start_search: QNone in query\n";
            t
      in
      let msg = iter t query in
      List.iter (fun s ->
          OpennapServers.send_search true s  (Normal_search q)  msg;
          Printf.bprintf  buf "Sending search\n")
      !OG.connected_servers)
  
let try_send buf num tail =
  let num = int_of_string num in
  let msg = OP.UnknownReq (num, tail) in
  List.iter (fun s ->
      match s.server_sock with
        None -> ()
      | Some sock ->
          OP.server_send sock msg; 
          Printf.bprintf  buf "Sending message\n") !OG.connected_servers

let string_of_length d =
  Printf.sprintf "%02d:%02d" (d / 60) (d mod 60)



let _ = 
  result_ops.CommonResult.op_result_download <- (fun r _ force ->
      OpennapServers.download_file r)

let file_num file =
  file.file_file.impl_file_num

module P = GuiTypes
  
let _ =
  file_ops.op_file_cancel <- (fun file ->
      Hashtbl.remove OpennapGlobals.files_by_key (
        file.file_name, file_size file);
      current_files := List2.removeq file !current_files;
      file_cancel (as_file file.file_file)
  );
  file_ops.op_file_info <- (fun file ->
      {
        P.file_name = file.file_name;
        P.file_num = (file_num file);
        P.file_network = network.network_num;
        P.file_names = [file.file_name];
        P.file_md4 = file.file_id;
        P.file_size = file_size file;
        P.file_downloaded = file_downloaded file;
        P.file_nlocations = 0;
        P.file_nclients = 0;
        P.file_state = file_state file;
        P.file_sources = None;
        P.file_download_rate = 0.0;
        P.file_chunks = "0";
        P.file_availability = [network.network_num, "0"];
        P.file_format = FormatUnknown;
        P.file_chunks_age = [|0 |];
        P.file_age = 0;
        P.file_last_seen = BasicSocket.last_time ();
        P.file_priority = file_priority (as_file file.file_file);
        P.file_uids = [];
        }    
  )
  
let _ =
  server_ops.op_server_info <- (fun s ->
      {
        P.server_num = (server_num s);
        P.server_network = network.network_num;
        P.server_addr = Ip.addr_of_ip s.server_ip;
        P.server_port = s.server_port;
        P.server_score = 0;
        P.server_tags = [];
        P.server_nusers = s.server_nusers;
        P.server_nfiles = s.server_nfiles;
        P.server_state = server_state s;
        P.server_name = s.server_desc;
        P.server_description = s.server_net;
        P.server_users = None;
        P.server_banner = "";
        }
  )

module C = CommonTypes
  
let _ =
  result_ops.op_result_info <- (fun r ->
       {
        C.result_num = r.result_result.impl_result_num;    
        C.result_network = network.network_num;
        
        C.result_names = [r.result_name];
        C.result_md4 = Md4.null;
        C.result_size = r.result_size;
        C.result_format = result_format_of_name r.result_name;
        C.result_type = result_media_of_name r.result_name;
        C.result_tags = [];
        C.result_comment = "";
        C.result_done = false;
      }   
  )

let _ =  
  user_ops.op_user_info <- (fun user ->
      {
        P.user_num = user.user_user.impl_user_num;
        P.user_md4 = Md4.null;
        P.user_name = user.user_nick;
        P.user_ip = Ip.null;
        P.user_port = 0;
        P.user_tags = [];
        
        P.user_server = (match user.user_servers with
            [] -> 
              lprintf "%s(%d) is not on any server\n" 
              user.user_nick user.user_user.impl_user_num;
              0
          | s :: _ -> s.server_server.impl_server_num);
      })
  
let _ =
  network.op_network_connected_servers <- (fun _ ->
      List2.tail_map (fun s -> as_server s.server_server) !connected_servers
  )
  
let browse_client c = 
  let user = c.client_user in
  List.iter (fun s ->
      match s.server_sock with
        None -> ()
      | Some sock ->
          s.server_browse_queue <- s.server_browse_queue @ [c];
          OP.server_send sock (OP.BrowseUserReq user.user_nick);
  ) user.user_servers
  
let _ =
  client_ops.op_client_info <- (fun c ->
      {
        P.client_network = network.network_num;
        P.client_kind = Indirect_location (c.client_name, Md4.null);
        P.client_state = client_state (as_client c.client_client);
        P.client_type = client_type c;
        P.client_tags = [];
        P.client_name = c.client_name;
        P.client_files = None;
        P.client_num = (client_num (as_client c.client_client));
        P.client_rating = 0;
        P.client_chat_port = 0 ;
        P.client_connect_time = BasicSocket.last_time ();
        P.client_software = "";
        P.client_downloaded = zero;
        P.client_uploaded = zero;
        P.client_upload = None;
(*        P.client_sock_addr = ""; *)
      }
  );
  client_ops.op_client_browse <- (fun c immediate ->
      browse_client c  );
  
  
  
  network.op_network_add_server <- (fun ip port ->
      as_server (new_server (Ip.ip_of_addr ip) port).server_server
  )