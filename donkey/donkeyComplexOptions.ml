(* Copyright 2002 b8_bavard, b8_fee_carabine, INRIA *)
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

open CommonClient
open CommonServer
open CommonComplexOptions
open CommonFile
open Options
open CommonTypes
open DonkeyTypes
open DonkeyOptions
open CommonOptions
open CommonGlobals
open DonkeyGlobals

let set_features () =
  ignore (String2.tokens !!features)

  
let _ =
  option_hook features set_features

    
(************ COMPLEX OPTIONS *****************)
  
let value_to_addr v =
  match v with
    List [v1;v2] | SmallList [v1;v2] ->
      (Ip.of_string (value_to_string v1), value_to_int v2)
  | _ -> failwith "Options: Not an int32 pair"

let addr_to_value ip port =
  SmallList [string_to_value (Ip.to_string ip); int_to_value port]
  
let value_to_md4 v =
  Md4.of_string (value_to_string v)

let value_to_client is_friend assocs = 
  let get_value name conv = conv (List.assoc name assocs) in
  let get_value_nil name conv = 
    try conv (List.assoc name assocs) with _ -> []
  in
  let kind = 
    try
      get_value "client_addr" (fun v ->
          match v with
            List [ip;port] | SmallList [ip;port] ->
              let ip = Ip.of_string (value_to_string ip) in
              let port = value_to_int port in
              Known_location (ip, port)
          | _ -> failwith  "Options: Not an client option"
      ) 
    with _ ->        
        let md4 = try
            get_value "client_md4" value_to_md4 with _ -> Md4.null
        in
        let name = try
            get_value "client_name" value_to_string with _ -> "" in
        Indirect_location(name, md4)
  in
  let l = DonkeyGlobals.new_client kind in
  
  let md4 = try
      get_value "client_md4" value_to_md4 
    with _ -> Md4.null
  in
  let name = try
      get_value "client_name" value_to_string with _ -> "" in
  set_client_name l name md4;
  
  (try
      l.client_checked <- get_value "client_checked" value_to_bool
    with _ -> ());
  (try
      CommonGlobals.connection_set_last_conn l.client_connection_control
        (min (get_value "client_age" value_to_float) 
        (BasicSocket.last_time ()));
    with _ -> ());
  if is_friend then friend_add l;
  l
  
let value_to_donkey_client v = 
  match v with
    List [ip;port] | SmallList [ip;port] ->
      let ip = Ip.of_string (value_to_string ip) in
      let port = value_to_int port in
      DonkeyGlobals.new_client (Known_location (ip, port))
  | Module assocs ->
      value_to_client false assocs 
  | _ -> assert false
      
let client_to_value c =
  let list = [
      "client_md4", string_to_value (Md4.to_string c.client_md4);
      "client_name", string_to_value c.client_name;
      "client_age", float_to_value (
        CommonGlobals.connection_last_conn 
        c.client_connection_control);
      "client_checked", bool_to_value c.client_checked;
    ]
  in
  
  match c.client_kind with
    Known_location (ip, port) ->        
      ("client_addr", addr_to_value ip  port) :: list
  | _ -> list
      
let donkey_client_to_value c =
  Options.Module (client_to_value c)  

let client_option =
  define_option_class "Client" value_to_donkey_client
  donkey_client_to_value 

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
      let l = DonkeyGlobals.new_server ip port !!initial_score in
      
      (try
          l.server_description <- get_value "server_desc" value_to_string 
        with _ -> ());
      (try
          l.server_name <- get_value "server_name" value_to_string
        with _ -> ());
      (try
          connection_set_last_conn l.server_connection_control
            (min (get_value "server_age" value_to_float) 
            (BasicSocket.last_time ()));
        with _ -> ());
      as_server l.server_server

let server_to_value c =
 [
    "server_addr", addr_to_value c.server_ip  c.server_port;
    "server_desc", string_to_value c.server_description;
    "server_name", string_to_value c.server_name;
    "server_age", float_to_value (
      connection_last_conn c.server_connection_control);
  ]

let value_to_int32pair v =
  match v with
    List [v1;v2] | SmallList [v1;v2] ->
      (value_to_int32 v1, value_to_int32 v2)
  | _ -> 
      failwith "Options: Not an int32 pair"

let value_to_state v =
  match v with
    StringValue "Paused" -> FilePaused
  | StringValue "Downloading" -> FileDownloading
  | StringValue "Downloaded" -> FileDownloaded
  | _ -> raise Not_found

let state_to_value s = 
  match s with
  | FilePaused -> StringValue "Paused"
  | FileDownloaded -> StringValue "Downloaded"
  | _ -> StringValue "Downloading"

let value_to_file is_done assocs =
  let get_value name conv = conv (List.assoc name assocs) in
  let get_value_nil name conv = 
    try conv (List.assoc name assocs) with _ -> []
  in
  
  let file_md4_name = 
    try
      get_value "file_md4" value_to_string
    with _ -> failwith "Bad file_md4"
  in
  let file_size = try
      value_to_int32 (List.assoc "file_size" assocs) 
    with _ -> Int32.zero
  in

  let file_state = get_value "file_state" value_to_state in
  
  let file = DonkeyGlobals.new_file file_state (
      Filename.concat !!temp_directory file_md4_name)
    (Md4.of_string file_md4_name) file_size true in
  
  (try
      file.file_file.impl_file_age <- get_value "file_age" value_to_float
    with _ -> ());

  (try 
      if file.file_exists then begin
(* only load absent chunks if file previously existed. *)
          file.file_absent_chunks <- 
            get_value "file_absent_chunks" 
            (value_to_list value_to_int32pair);
        end
    with _ -> ()                );
  
  file.file_filenames <-
    get_value_nil "file_filenames" (value_to_list value_to_string);
  
  (try
      file.file_all_chunks <- get_value "file_all_chunks"
        value_to_string
    with _ -> ());
  
  List.iter (fun c ->
      DonkeyGlobals.new_source file  c;
  )
  (get_value_nil "file_locations" (value_to_list value_to_donkey_client));
  
  (try
      file.file_chunks_age <-
        get_value "file_chunks_age" 
        (fun v -> 
          let list = value_to_list value_to_float v in
          Array.of_list list)
    with _ -> ());
  
  let md4s = get_value_nil "file_md4s" (value_to_list value_to_md4) in
  file.file_md4s <- (if md4s = [] then file.file_md4s else md4s);
  as_file file.file_file
  
  
let string_of_chunks file =
  let s = String.create file.file_nchunks in
  for i = 0 to file.file_nchunks - 1 do
    s.[i] <- (
      match file.file_chunks.(i) with
        PresentVerified | PresentTemp -> '1'
      | _ -> '0'
    )
  done;
  s

let file_to_value file =
  let locs = ref [] in
  Intmap.iter (fun _ c ->
      match c.client_kind with
        Indirect_location _ -> 
          if c.client_md4 <> Md4.null then 
            locs := c :: !locs
      | _ ->             
          locs := c :: !locs
  ) file.file_sources;
  
  [
    "file_md4", string_to_value (Md4.to_string file.file_md4);
    "file_size", int32_to_value file.file_file.impl_file_size;
    "file_all_chunks", string_to_value file.file_all_chunks;
    "file_state", state_to_value (file_state file);
    "file_absent_chunks", List
      (List.map (fun (i1,i2) -> 
          SmallList [int32_to_value i1; int32_to_value i2])
      file.file_absent_chunks);
    "file_filenames", List
      (List.map (fun s -> string_to_value s) file.file_filenames);
    "file_age", FloatValue file.file_file.impl_file_age;
    "file_md4s", List
      (List.map (fun s -> string_to_value (Md4.to_string s)) 
      file.file_md4s);
    "file_downloaded", int32_to_value file.file_file.impl_file_downloaded;
    "file_chunks_age", List (Array.to_list 
        (Array.map float_to_value file.file_chunks_age));
    "file_locations", list_to_value "Donkey Locations" donkey_client_to_value
      !locs;        
  ]
  
module SharedFileOption = struct
    
    let value_to_shinfo v =
      match v with
        Options.Module assocs ->
          let get_value name conv = conv (List.assoc name assocs) in
          let get_value_nil name conv = 
            try conv (List.assoc name assocs) with _ -> []
          in
          
          let sh_md4s = try
              value_to_list (fun v ->
                  Md4.of_string (value_to_string v)) (List.assoc "md4s" assocs)
            with _ -> failwith "Bad shared file md4"
          in
          let sh_size = try
              value_to_int32 (List.assoc "size" assocs) 
            with _ -> failwith "Bad shared file size"
          in
          let sh_name = try
              value_to_filename (List.assoc "name" assocs)
            with _ -> failwith "Bad shared file name"
          in
          let sh_mtime = try
              value_to_float (List.assoc "mtime" assocs)
            with _ -> failwith "Bad shared file mtime"
          in
          { sh_name = sh_name; sh_mtime = sh_mtime;
            sh_size = sh_size; sh_md4s = sh_md4s;
          }
          
      | _ -> failwith "Options: not a shared file info option"
          
    let shinfo_to_value sh =
      Options.Module [
        "name", filename_to_value sh.sh_name;
        "md4s", list_to_value "Shared Md4" (fun md4 ->
            string_to_value (Md4.to_string md4)) sh.sh_md4s;
        "mtime", float_to_value sh.sh_mtime;
        "size", int32_to_value sh.sh_size;
      ]
    
    
    let t = define_option_class "SharedFile" value_to_shinfo shinfo_to_value
  end
    
      
(*  
let done_files = 
  define_option files_ini ["done_files"] 
  "The files whose download is finished" (list_option FileOption.t) []
    *)

let old_files = 
  define_option downloads_ini ["old_files"] 
  "The files that were downloaded" (list_option Md4.option) []

  (*
let files = 
  define_option files_ini ["files"] 
  "The files currently being downloaded" (list_option FileOption.t) []
*)

(*
let known_servers = define_option servers_ini["known_servers"] "List of known servers"
    (list_option ServerOption.t) []
    *)

let known_shared_files = define_option shared_files_ini 
    ["shared_files"] "" 
    (list_option SharedFileOption.t) []
  
(************  UPDATE OPTIONS *************)  
  
let add_server ip port =
  try
    DonkeyGlobals.find_server ip port 
  with _ ->
      let s = DonkeyGlobals.new_server ip port !!initial_score in
      DonkeyGlobals.servers_ini_changed := true;
      s
  
let remove_server ip port =
  try
    let s = DonkeyGlobals.find_server ip port in
    DonkeyGlobals.servers_ini_changed := true;
    DonkeyGlobals.remove_server ip port
  with _ -> ()

    
let load _ =
  try
    Options.load shared_files_ini
  with Sys_error _ ->
      Options.save_with_help shared_files_ini

let save _ =
  Options.save_with_help shared_files_ini
  
let _ =
  network.op_network_add_file <- value_to_file;
(*  file_ops.op_file_to_option <- file_to_value; *)
  
  network.op_network_add_server <- value_to_server;
  server_ops.op_server_to_option <- server_to_value;
  
  network.op_network_add_client <- (fun is_friend l ->
      as_client (value_to_client is_friend l).client_client);
  client_ops.op_client_to_option <- client_to_value;
  
  network.op_network_load_complex_options <- load;
  network.op_network_save_complex_options <- save
  
