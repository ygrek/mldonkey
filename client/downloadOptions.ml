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
open Mftp
open Options
open DownloadTypes
open DownloadGlobals
open Unix
open Gui_types
  
let downloads_ini = create_options_file "./downloads.ini"
let shared_files_ini = create_options_file "./shared_files.ini"
let servers_ini = create_options_file "./servers.ini"
let files_ini = create_options_file "./files.ini"
let friends_ini = create_options_file "./friends.ini"
  
let initial_score = define_option downloads_ini ["initial_score"] "" int_option 5

let random_letter () =
  char_of_int (97 + Random.int 26)
  
  
let client_name = define_option downloads_ini ["client_name"] "small name of client" string_option 
    (Printf.sprintf "mldonkey_%c%c%c%c%c" (random_letter ()) (random_letter ()) 
    (random_letter ()) (random_letter ()) (random_letter ()))

let small_retry_delay = define_option downloads_ini ["small_retry_delay"] 
  "" float_option 30.
let medium_retry_delay = define_option downloads_ini ["medium_retry_delay"] "Minimal delay between two connection attempts to the same host" float_option 600.
let long_retry_delay = define_option downloads_ini ["long_retry_delay"] 
    "Minimal delay between two connection attempts to the same host, 
  when the first one failed" float_option 3600.
let client_timeout = define_option downloads_ini ["client_timeout"] 
  "Timeout on client connections when not queued" float_option 120.

let interface_buffer = define_option downloads_ini ["interface_buffer"] 
  "The size of the buffer between the client and its GUI. Can be useful
to increase when the connection between them has a small bandwith" int_option
  1000000

let previewer = define_option downloads_ini ["previewer"]
  "Name of program used for preview (first arg is local filename, second arg
    is name of file as searched on eDonkey" string_option
  "mldonkey_previewer"
  
let gui_port = 
  define_option downloads_ini ["gui_port"] "port for user interaction" int_option 4001

let update_gui_delay = define_option downloads_ini ["update_gui_delay"] 
  "Delay between updates to the GUI" float_option 10.
 
let temp_directory = define_option downloads_ini ["temp_directory" ] 
    "The directory where temporary files should be put" 
  string_option "temp"
  
let incoming_directory = 
  define_option downloads_ini ["incoming_directory" ] 
    "The directory where downloaded files should be moved after commit" 
  string_option "incoming"

let http_port = 
  define_option downloads_ini ["http_port"] "The port used to connect to your client with a WEB browser" int_option 4080
  
let http_login = 
  define_option downloads_ini ["http_login"] "Your login when using a WEB browser" string_option ""
  
let http_password = 
  define_option downloads_ini ["http_password"] "Your password when using a WEB browser" string_option ""

let initialized = define_option downloads_ini ["initialized"] 
  "(not used)"
    bool_option false
    
let max_upload_rate = define_option downloads_ini ["max_upload_rate"] 
  "The maximal upload rate you can tolerate" int_option 3000
  
let max_download_rate = define_option downloads_ini ["max_download_rate"] 
    "The maximal download rate you can tolerate (0 = no limit)" int_option 0

let max_xs_packets = define_option downloads_ini ["max_xs_packets"] 
  "Max number of UDP packets per round for eXtended Search" int_option 30

let max_dialog_history = define_option downloads_ini ["max_dialog_history"]
    "Max number of messages of Chat remembered" int_option 30
  
let _ =
  option_hook max_upload_rate (fun _ ->
      if !!max_upload_rate < 1  then
        max_upload_rate =:= 1)
  
  
let string_list_option = define_option_class "String"
    (fun v ->
      match v with
        List _ | SmallList _ -> ""
      | _ -> value_to_string v
  )
  string_to_value

  
let features = define_option downloads_ini ["features"] "" string_list_option ""
let set_features () =
  List.iter (Mftp_client.set_features has_upload) (String2.tokens !!features)

  
let _ =
  option_hook features set_features
  
let password = define_option downloads_ini ["password"] 
  "The password to access your client from the GUI (setting it disables
  the command-line client)" string_option ""

let port = define_option downloads_ini ["port"] "The port used for connection by other donkey clients." int_option 4662

let save_options_delay = 
  define_option downloads_ini ["save_options_delay"] 
  "The delay between two saves of the 'downloads.ini' file" float_option 60.0

let check_client_connections_delay = 
  define_option downloads_ini ["check_client_connections_delay"] 
  "Delay used to request file sources" float_option 180.0
    
let check_connections_delay = 
  define_option downloads_ini ["check_connections_delay"] 
  "The delay between server connection rounds" float_option 5.0
  
let max_connected_servers = define_option downloads_ini ["max_connected_servers"] 
    "The number of servers you want to stay connected to" int_option 10

let max_udp_sends = define_option downloads_ini ["max_udp_sends"] 
    "The number of UDP packets you send every check_client_connections_delay" 
  int_option 10

  (*
let _ =
  option_hook max_connected_servers (fun _ ->
      if !!max_connected_servers > 10 && !has_upload then
        max_connected_servers =:= 10)
  *)

let retry_delay = define_option downloads_ini ["retry_delay"] "" float_option 3600.
let server_connection_timeout = define_option downloads_ini ["server_connection_timeout"] 
  "timeout when connecting to a server" float_option 5.

let telnet_port = define_option downloads_ini ["telnet_port"] "port for user interaction" int_option 4000

let max_server_age = define_option downloads_ini ["max_server_age"] "max number of days after which an unconnected server is removed" int_option 7

let use_file_history = define_option downloads_ini ["use_file_history"] "keep seen files in history to allow local search (can be expensive in memory)" bool_option true
  
let save_file_history = define_option downloads_ini ["save_file_history"] "save the file history in a file and load it at startup" bool_option true

  
let filters = define_option downloads_ini ["filters"] 
    "filters on replies (replies will be kept)."
    string_list_option ""

let smtp_server = define_option downloads_ini ["smtp_server"] 
  "The mail server you want to use (must be SMTP). Use hostname or IP address"
    string_option "127.0.0.1"

let smtp_port = define_option downloads_ini ["smtp_port"] 
  "The port to use on the mail server (default 25)"
  int_option 25

let mail = define_option downloads_ini ["mail"]
  "Your e-mail if you want to receive mails when downloads are completed"
    string_option ""

  
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
  
module ClientOption = struct
    let value_to_client v = 
      match v with
        List [ip;port] | SmallList [ip;port] ->
          let ip = Ip.of_string (value_to_string ip) in
          let port = value_to_int port in
          new_client (Known_location (ip, port))
      | Module assocs ->
          let get_value name conv = conv (List.assoc name assocs) in
          let get_value_nil name conv = 
            try conv (List.assoc name assocs) with _ -> []
          in
          let kind = get_value "client_addr" (fun v ->
                match v with
                  List [ip;port] | SmallList [ip;port] ->
                    let ip = Ip.of_string (value_to_string ip) in
                    let port = value_to_int port in
                    Known_location (ip, port)
                | _ -> failwith  "Options: Not an client option"
            ) in
          let l = new_client kind in
          
          (try
              l.client_md4 <- get_value "client_md4" value_to_md4 
            with _ -> ());
          (try
              l.client_name <- get_value "client_name" value_to_string
            with _ -> ());
          l
      | _ -> failwith "Options: Not a client"

                  
    let client_to_value c =
      match c.client_kind with
        Known_location (ip, port) ->
          Options.Module [
            "client_addr", addr_to_value ip  port;
            "client_md4", string_to_value (Md4.to_string c.client_md4);
            "client_name", string_to_value c.client_name;
          ]
      | _ -> failwith "client_to_value: bad client"

    
    let t =
      define_option_class "Client" value_to_client client_to_value

  end
  


module IpOption = struct
    
    let value_to_ip v = 
      Ip.of_string (value_to_string v)
      
    let ip_to_value ip =
      string_to_value (Ip.to_string ip)
      
    let t = define_option_class "Ip" value_to_ip ip_to_value      
      
  end
    
module ServerOption = struct
    let value_to_server v = 
      match v with
        List [ip;port] | SmallList [ip;port] ->
          let ip = Ip.of_string (value_to_string ip) in
          let port = value_to_int port in
          new_server ip port !!initial_score
      | Module assocs ->
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
          let l = new_server ip port !!initial_score in
          
          (try
              l.server_description <- get_value "server_desc" value_to_string 
            with _ -> ());
          (try
              l.server_name <- get_value "server_name" value_to_string
            with _ -> ());
          (try
              connection_set_last_conn l.server_connection_control
                (get_value "server_age" value_to_float);
            with _ -> ());
          l
      | _ -> failwith "Options: Not a server"

                  
    let server_to_value c =
      Options.Module [
        "server_addr", addr_to_value c.server_ip  c.server_port;
        "server_desc", string_to_value c.server_description;
        "server_name", string_to_value c.server_name;
        "server_age", float_to_value (
          connection_last_conn c.server_connection_control);
      ]

    
    let t =
      define_option_class "Server" value_to_server server_to_value

  end
  

  
module FileOption = struct
    
    let value_to_int32pair v =
      match v with
        List [v1;v2] | SmallList [v1;v2] ->
          (value_to_int32 v1, value_to_int32 v2)
      | _ -> 
          failwith "Options: Not an int32 pair"
    
    let value_to_file v =
      match v with
        Options.Module assocs ->
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
          
          let file = new_file (
              Filename.concat !!temp_directory file_md4_name)
            (Md4.of_string file_md4_name) file_size in
          
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
          
          file.file_known_locations <- 
            get_value_nil "file_locations" (value_to_list 
              ClientOption.value_to_client);
          let md4s = get_value_nil "file_md4s" (value_to_list value_to_md4) in
          file.file_md4s <- (if md4s = [] then file.file_md4s else md4s);
          file
      
      | _ -> failwith "Options: not a file option"
    
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
      Options.Module [
        "file_md4", string_to_value (Md4.to_string file.file_md4);
        "file_size", int32_to_value file.file_size;
        "file_all_chunks", string_to_value file.file_all_chunks;
        "file_absent_chunks", List
          (List.map (fun (i1,i2) -> 
              SmallList [int32_to_value i1; int32_to_value i2])
          file.file_absent_chunks);
        "file_filenames", List
          (List.map (fun s -> string_to_value s) file.file_filenames);
        "file_locations", list_to_value ClientOption.client_to_value
          file.file_known_locations;
        "file_md4s", List
          (List.map (fun s -> string_to_value (Md4.to_string s)) 
          file.file_md4s);
        "file_downloaded", int32_to_value file.file_downloaded;      
      ]
    
    let t =
      define_option_class "File" value_to_file file_to_value
    ;;
  end

  
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
        "md4s", list_to_value (fun md4 ->
            string_to_value (Md4.to_string md4)) sh.sh_md4s;
        "mtime", float_to_value sh.sh_mtime;
        "size", int32_to_value sh.sh_size;
      ]
    
    
    let t = define_option_class "SharedFile" value_to_shinfo shinfo_to_value
  end
    

module Md4Option = struct
    
    let value_to_md4 v = 
      match v with
        Options.Module assocs ->
          let get_value name conv = conv (List.assoc name assocs) in
          let get_value_nil name conv = 
            try conv (List.assoc name assocs) with _ -> []
          in
          
          
          let file_md4_name = 
            try
              get_value "file_md4" value_to_string
            with _ -> failwith "Bad file_md4"
          in
          Md4.of_string file_md4_name
          
      | _ -> Md4.of_string (value_to_string v)
    let md4_to_value v = string_to_value (Md4.to_string v)
    
    
    let t =
      define_option_class "Md4" value_to_md4 md4_to_value
    ;;
  end

  
let allowed_ips = define_option downloads_ini ["allowed_ips"]
    "list of IP address allowed to control the client via telnet/GUI/WEB"
    (list_option IpOption.t) [Ip.of_string "127.0.0.1"]

let done_files = 
  define_option files_ini ["done_files"] 
  "The files whose download is finished" (list_option FileOption.t) []

let old_files = 
  define_option downloads_ini ["old_files"] 
  "The files that were downloaded" (list_option Md4Option.t) []

let known_friends = 
  define_option friends_ini ["friends"] 
  "The list of known friends" (list_option ClientOption.t) []

let client_md4 = define_option downloads_ini ["client_md4"]
    "The MD4 of this client" Md4Option.t (Md4.random ())
  
let files = 
  define_option files_ini ["files"] 
  "The files currently being downloaded" (list_option FileOption.t) []

let known_servers = define_option servers_ini["known_servers"] "List of known servers"
    (list_option ServerOption.t) []

let known_shared_files = define_option shared_files_ini 
    ["shared_files"] "" 
    (list_option SharedFileOption.t) []
  
(************  UPDATE OPTIONS *************)  
  
let add_server ip port =
  try
    find_server ip port 
  with _ ->
      let s = new_server ip port !!initial_score in
      servers_ini_changed := true;
      known_servers =:= s :: !!known_servers;
      s
  
let remove_server ip port =
  try
    let s = find_server ip port in
    servers_ini_changed := true;
    known_servers  =:= List2.removeq s  !!known_servers ;
    DownloadGlobals.remove_server ip port
  with _ -> ()

      

      
  (*
module FriendOption = struct
    open Options
      
    let value_to_friend v =
      match v with
        List [v1;v2] | SmallList [v1;v2] ->
          let ip = Ip.of_string (value_to_string v1) in
          let port = value_to_int v2 in
          new_friend ip port
      | _ -> failwith "Options: Not an int32 pair"

    let friend_to_value c =
      SmallList [
        string_to_value (Ip.to_string c.friend_ip);
        int_to_value c.friend_port
      ]
      
    let t =
      define_option_class "Friend" value_to_friend friend_to_value
    ;;
  end
*)  



let max_allowed_connected_servers () =
  min 5 !!max_connected_servers
