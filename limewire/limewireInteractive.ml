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
open CommonOptions
open CommonServer
open CommonResult
open CommonTypes
open CommonComplexOptions
open CommonFile
open Options
open LimewireTypes
open LimewireOptions
open LimewireGlobals
open LimewireComplexOptions

open LimewireProtocol
  
let _ =
  network.op_network_search <- (fun search buf ->
      let query = search.search_query in
      let keywords = CommonInteractive.keywords_of_query query in
      let words = String2.unsplit keywords ' ' in
      let p = LimewireServers.send_query !connected_servers words "urn:" in
      
      let s = {
          search_search = search;
          search_uid = p.pkt_uid;
        } in
      Hashtbl.add searches_by_uid p.pkt_uid s;
      ());
  network.op_network_connected <- (fun _ ->
      !connected_servers <> []   
  );
  network.op_network_share <- (fun fullname codedname size ->
      LimewireServers.new_shared_words := true;
      CommonUploads.add_shared fullname codedname size
  )
  
let _ =
  result_ops.op_result_download <- (fun result _ force ->
      LimewireServers.download_file result)

let file_num file =
  file.file_file.impl_file_num

let _ =
  file_ops.op_file_sources <- (fun file ->
      lprintf "file_sources\n"; 
      List2.tail_map (fun c ->
          as_client c.client_client
      ) file.file_clients
  );
  file_ops.op_file_recover <- (fun file ->
      LimewireServers.gen_query file !connected_servers;          
      List.iter (fun c ->
          LimewireServers.get_file_from_source c file
      ) file.file_clients
  )

  
module P = GuiTypes
  
let _ =
  file_ops.op_file_cancel <- (fun file ->
      remove_file file;
      file_cancel (as_file file.file_file);
  );
  file_ops.op_file_info <- (fun file ->
      {
        P.file_name = file.file_name;
        P.file_num = (file_num file);
        P.file_network = network.network_num;
        P.file_names = [file.file_name];
        P.file_md4 = Md4.null;
        P.file_size = file_size file;
        P.file_downloaded = file_downloaded file;
        P.file_nlocations = 0;
        P.file_nclients = 0;
        P.file_state = file_state file;
        P.file_sources = None;
        P.file_download_rate = file_download_rate file.file_file;
        P.file_chunks = "0";
        P.file_availability = "0";
        P.file_format = Unknown_format;
        P.file_chunks_age = [|0|];
        P.file_age = file_age file;
        P.file_last_seen = BasicSocket.last_time ();
        P.file_priority = file_priority (as_file file.file_file);
      }    
  )
  
let _ =
  server_ops.op_server_info <- (fun s ->
      if !!enable_limewire then
        {
          P.server_num = (server_num s);
          P.server_network = network.network_num;
          P.server_addr = new_addr_ip s.server_ip;
          P.server_port = s.server_port;
          P.server_score = 0;
          P.server_tags = [];
          P.server_nusers = 0;
          P.server_nfiles = s.server_nfiles;
          P.server_state = server_state s;
          P.server_name = s.server_agent;
          P.server_description = "";
          P.server_users = None;
          P.server_banner = "";
          } else
        raise Not_found
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
  network.op_network_connected_servers <- (fun _ ->
      List2.tail_map (fun s -> as_server s.server_server) !connected_servers
  );
  network.op_network_parse_url <- (fun url ->
      match String2.split (String.escaped url) '|' with
      | "lw://" :: "server" :: ip :: port :: _ ->  
          let ip = Ip.of_string ip in
          let port = int_of_string port in
          let s = new_server ip port in
          true
      | "lw://" :: "friend" :: uid :: ip :: port :: _ ->  
          let ip = Ip.of_string ip in
          let port = int_of_string port in
          let md4 = Md4.of_string uid in
          let c = new_client md4 (Known_location (ip, port)) in
          friend_add (as_client c.client_client);
          true
      | "lw://" :: "friend" :: uid :: _ ->
          let md4 = Md4.of_string uid in
          let c = new_client md4 (Indirect_location ("", md4)) in
          friend_add (as_client c.client_client);
          true
          
      | _ -> 
          let url = Url.of_string url in
          if url.Url.file = "magnet:" then begin
              let uids = ref [] in
              let name = ref "" in
              List.iter (fun (value, arg) ->
                  if String2.starts_with value "xt" then
                    uids := (extract_uids arg) @ !uids
                  else 
                  if String2.starts_with value "dn" then
                    name := Url.decode value
              ) url.Url.args;
              if !uids <> [] then begin
(* Start a download for this file *)
                  let r = new_result !name Int64.zero !uids in
                  LimewireServers.download_file r;
                  true
                end
              else false
            end else
            false
  )
    
let browse_client c = 
  lprintf "Limewire: browse client not implemented\n";
  ()
  
let _ =
  client_ops.op_client_info <- (fun c ->
      {
        P.client_network = network.network_num;
        P.client_kind = c.client_user.user_kind;
        P.client_state = client_state (as_client c.client_client);
        P.client_type = client_type c;
        P.client_tags = [];
        P.client_name = (match c.client_user.user_kind with
          | Known_location (ip, port) ->
              Printf.sprintf "%s:%d" (Ip.to_string ip) port
          | Indirect_location (_, id) -> 
              Printf.sprintf "UID[%s...]" (String.sub (Md4.to_string id) 0 12)
        );
        P.client_files = None;
        P.client_num = (client_num (as_client c.client_client));
        P.client_rating = 0;
        P.client_chat_port = 0 ;
      }
  );
  client_ops.op_client_browse <- (fun c immediate ->
      browse_client c
  )
  
  
let _ =  
  user_ops.op_user_info <- (fun user ->
      {
        P.user_num = user.user_user.impl_user_num;
        P.user_md4 = user.user_uid;
        P.user_name = "";
        P.user_ip = Ip.null;
        P.user_port = 0;
        P.user_tags = [];
        
        P.user_server = 0;
      })
