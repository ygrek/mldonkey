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

open CommonServer
open CommonTypes
open CommonFile
open Options
open OpennapGlobals
open OpennapOptions
open OpennapTypes
open CommonGlobals
  
    
let value_to_addr v =
  match v with
    List [v1;v2] | SmallList [v1;v2] ->
      (Ip.of_string (value_to_string v1), value_to_int v2)
  | _ -> failwith "Options: Not an int32 pair"

let addr_to_value ip port =
  SmallList [string_to_value (Ip.to_string ip); int_to_value port]

let value_to_server assocs =
  let get_value name conv = conv (List.assoc name assocs) in
  let get_value_nil name conv = 
    try conv (List.assoc name assocs) with _ -> []
  in
  let ip, port = get_value "server_addr" (fun v ->
        match v with
          List [ip;port] | SmallList [ip;port] ->
            let ip = Ip.of_string (value_to_string ip) in
            let port = value_to_int port in
            ip, port
        | _ -> failwith  "Options: Not an server option"
    ) in
  let l = OpennapGlobals.new_server ip port in
  
  (try
      l.server_desc <- get_value "server_desc" value_to_string 
    with _ -> ());
  (try
      l.server_net <- get_value "server_net" value_to_string
    with _ -> ());
  (try
      connection_set_last_conn l.server_connection_control
        (min (get_value "server_age" value_to_float) 
        (BasicSocket.last_time ()));
    with _ -> ());
  as_server l.server_server


let server_to_value c = [
    "server_addr", addr_to_value c.server_ip  c.server_port;
    "server_desc", string_to_value c.server_desc;
    "server_net", string_to_value c.server_net;
    "server_age", float_to_value (
      connection_last_conn c.server_connection_control);
  ]

(*  
let known_servers = define_option opennap_ini
    ["known_servers"] "List of known servers"
    (list_option ServerOption.t) []

*)


let value_to_file is_done assocs =
  let get_value name conv = conv (List.assoc name assocs) in
  let get_value_nil name conv = 
    try conv (List.assoc name assocs) with _ -> []
  in
  
  let file_name = get_value "file_name" value_to_string in
  let file_id = 
    try
      Md4.of_string (get_value "file_id" value_to_string)
    with _ -> failwith "Bad file_id"
  in
  let file_size = try
      value_to_int32 (List.assoc "file_size" assocs) 
    with _ -> failwith "Bad file size"
  in
  
  let file = new_file file_id file_name file_size in

  if file_state file <> FileDownloaded then
    current_files := file :: !current_files;
    
  (try
      ignore (get_value "file_sources" (value_to_list (fun v ->
              match v with
                SmallList [nick; filename; (SmallList servers | List servers) ] 
              | List [nick; filename; (SmallList servers | List servers)] ->
                  let nick = value_to_string nick in
                  let filename = value_to_string filename in
                  let user = new_user None nick in
                  let c = add_file_client file user filename in
                  List.iter (fun v ->
                      match v with
                        SmallList [ip; port]
                      | List [ip; port] ->
                          let addr = from_value Ip.option ip in
                          let port = value_to_int port in
                          let s = new_server addr port in
                          if not (List.memq s user.user_servers) then
                            user.user_servers <- s :: user.user_servers
                      | _ -> ()
                  ) servers
              | _ -> failwith "Bad source"
          )))
    with e -> 
        Printf.printf "Exception %s while loading source"
          (Printexc2.to_string e); 
        print_newline ();
  );
  
  as_file file.file_file

let file_to_value file =
  [
    "file_size", int32_to_value (file_size file);
    "file_name", string_to_value file.file_name;
    "file_downloaded", int32_to_value (file_downloaded file);
    "file_id", string_to_value (Md4.to_string file.file_id);
    "file_sources", 
    list_to_value "Opennap Sources" (fun c ->
        let filename = List.assoc file c.client_files  in
        SmallList [string_to_value c.client_user.user_nick;
          string_to_value filename;
          (list_to_value "Opennap Sources" (fun s ->
                SmallList [to_value Ip.option s.server_ip;
                  int_to_value s.server_port]
            ) c.client_user.user_servers)]
    ) file.file_clients;

  ]
  
  (*
let files = 
  define_option opennap_ini ["files"] 
  "The files currently being downloaded" (list_option FileOption.t) []

let done_files = 
  define_option opennap_ini ["done_files"] 
  "The files completely downloaded" (list_option FileOption.t) []

    *)

let old_files = 
  define_option opennap_ini ["old_files"]
    "" (list_option (tuple2_option (string_option, int32_option))) []  
      
let _ =
  network.op_network_add_file <- value_to_file;
  file_ops.op_file_to_option <- file_to_value;
  server_ops.op_server_sort <- (fun s ->
      connection_last_conn s.server_connection_control);
  network.op_network_add_server <- value_to_server;
  server_ops.op_server_to_option <- server_to_value;  
  
 