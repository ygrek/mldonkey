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

open CommonFile
open CommonChatRoom
open CommonComplexOptions
open CommonServer
open CommonResult
open CommonTypes
open BasicSocket
open Options
open DcTypes
open DcOptions
open CommonGlobals
open DcGlobals

(*      
let addr_to_value addr =
  match addr with
    AddrIp ip -> to_value Ip.option ip
  | AddrName s -> string_to_value s
      
let value_to_addr v =
  let ip = from_value Ip.option v in
  if ip <> Ip.null then AddrIp ip else AddrName (value_to_string v)
*)

let value_to_server  assocs =
  let get_value name conv = conv (List.assoc name assocs) in
  let get_value_nil name conv = 
    try conv (List.assoc name assocs) with _ -> []
  in
  let server_addr = get_value "server_addr" value_to_addr in
  let server_port = get_value "server_port" value_to_int in
  let h = new_server server_addr server_port in
  h.server_name <- get_value "server_name" value_to_string;
  h.server_info <- get_value "server_info" value_to_string;
  h.server_nusers <- get_value "server_nusers" value_to_int;
  as_server h.server_server


let server_to_value h =
  let list = [
      "server_name", string_to_value h.server_name;
      "server_addr", addr_to_value h.server_addr;
      "server_info", string_to_value h.server_info;
      "server_nusers", int_to_value h.server_nusers;
      "server_port", int_to_value h.server_port;
    ] in
  list
  


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
                        SmallList [addr; port]
                      | List [addr; port] ->
                          let addr = value_to_addr addr in
                          let port = value_to_int port in
                          let s = new_server addr port in
                          user.user_servers <- s :: user.user_servers
                      | _ -> ()
                  ) servers
              | _ -> failwith "Bad source"
          )))
    with e -> 
        Printf.printf "Exception %s while loading source"
          (Printexc.to_string e); 
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
    list_to_value (fun c ->
        let filename = List.assoc file c.client_files  in
        SmallList [string_to_value c.client_user.user_nick;
          string_to_value filename;
          (list_to_value (fun s ->
                SmallList [addr_to_value s.server_addr;
                  int_to_value s.server_port]
            ) c.client_user.user_servers)]
    ) file.file_clients;
  ]
        
let _ =
  server_ops.op_server_sort <- (fun s ->
      connection_last_conn s.server_connection_control);
  network.op_network_add_server <- value_to_server;
  server_ops.op_server_to_option <- server_to_value;  
  network.op_network_add_file <- value_to_file;
  file_ops.op_file_to_option <- file_to_value
  