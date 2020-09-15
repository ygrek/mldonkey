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
open OpenFTTypes
open OpenFTOptions
open OpenFTGlobals
open OpenFTComplexOptions

open OpenFTProtocol

let _ =
  network.op_network_search <- (fun search buf ->
      let query = search.search_query in
      let keywords = ref [] in
      
      let rec iter q = 
        match q with
        | QOr (q1,q2) 
        | QAnd (q1, q2) -> iter q1; iter q2
        | QAndNot (q1,q2) -> iter q1 
        | QHasWord w -> keywords := String2.split_simplify w ' '
        | QHasField(field, w) ->
            begin
              match field with
                "Album"
              | "Title"
              | "Artist"
              | _ -> keywords := String2.split_simplify w ' '
            end
        | QHasMinVal (field, value) ->
            begin
              match field with
                "bitrate"
              | "size"
              | _ -> ()
            end
        | QHasMaxVal (field, value) ->
            begin
              match field with
                "bitrate"
              | "size"
              | _ -> ()
            end
        | QNone ->
            prerr_endline "OpenFTInteractive.start_search: QNone in query";
            ()
      in
      iter query;
      
      let id = OpenFTServers.send_query !keywords in
      
      let s = {
          search_search = search;
          search_id = id;
        } in
      Hashtbl.add searches_by_uid id s;
      ())

let _ =
  result_ops.op_result_download <- (fun result _ force ->
      OpenFTServers.download_file result)

let file_num file =
  file.file_file.impl_file_num

let _ =
  file_ops.op_file_cancel <- (fun file ->
      current_files := List2.remove file !current_files;
      file_cancel (as_file file.file_file);
  );
  file_ops.op_file_sources <- (fun file ->
      List2.tail_map (fun c ->
          as_client c.client_client
      ) file.file_clients
  )
  
module P = GuiTypes
  
let _ =
  file_ops.op_file_cancel <- (fun file ->
      Hashtbl.remove OpenFTGlobals.files_by_md5 file.file_md5;
      current_files := List2.removeq file !current_files      
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
        P.file_sub_files = [];
      }    
  )
  
let _ =
  server_ops.op_server_info <- (fun s ->
      if !!enable_openft then
        {
          P.server_num = (server_num s);
          P.server_network = network.network_num;
          P.server_addr = new_addr_ip s.server_ip;
          P.server_port = s.server_port;
          P.server_realport = 0;
          P.server_score = 0;
          P.server_tags = [];
          P.server_nusers = 0;
          P.server_nfiles = s.server_nfiles;
          P.server_state = server_state s;
          P.server_name = s.server_agent;
          P.server_description = "";
          P.server_users = None;
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
      | "ft://" :: "server" :: ip :: port :: _ ->  
          let ip = Ip.of_string ip in
          let port = int_of_string port in
          let s = new_server ip port in
          "", true
      | "ft://" :: "friend" :: ip :: port :: http_port :: _ ->  
          let ip = Ip.of_string ip in
          let port = int_of_string port in
          let http_port = int_of_string http_port in
          let c = new_client ip port http_port in
          friend_add (as_client c.client_client);
          "", true
      | _ -> false
  )

let browse_client c = 
  lprintf "OpenFT: browse client not implemented"; lprint_newline ();
  ()
  
let _ =
  client_ops.op_client_info <- (fun c ->
      let s = c.client_user.user_server in
      {
        P.client_network = network.network_num;
        P.client_kind = Known_location (s.server_ip, s.server_port);
        P.client_state = client_state (as_client c.client_client);
        P.client_type = client_type c;
        P.client_tags = [];
        P.client_name = "";
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
        P.user_md4 = Md4.null;
        P.user_name = "";
        P.user_ip = Ip.null;
        P.user_port = 0;
        P.user_tags = [];
        
        P.user_server = 0;
      })

