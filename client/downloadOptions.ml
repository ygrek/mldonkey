open Mftp
open Options
open DownloadTypes
open DownloadGlobals
open Unix
open Gui_types
  
let initial_score = define_option ["initial_score"] "" int_option 5
      
let client_name = define_option ["client_name"] "small name of client" string_option 
    "mldonkey"

let small_retry_delay = define_option ["small_retry_delay"] 
  "" float_option 30.
let medium_retry_delay = define_option ["medium_retry_delay"] "Minimal delay between two connection attempts to the same host" float_option 600.
let long_retry_delay = define_option ["long_retry_delay"] 
    "Minimal delay between two connection attempts to the same host, 
  when the first one failed" float_option 3600.
let client_timeout = define_option ["client_timeout"] 
  "Timeout on client connections when not queued" float_option 120.

let interface_buffer = define_option ["interface_buffer"] 
  "The size of the buffer between the client and its GUI. Can be useful
to increase when the connection between them has a small bandwith" int_option
  1000000

let previewer = define_option ["previewer"]
  "Name of program used for preview (first arg is local filename, second arg
    is name of file as searched on eDonkey" string_option
  "mldonkey_previewer"
  
let gui_port = 
  define_option ["gui_port"] "port for user interaction" int_option 4001

let update_gui_delay = define_option ["update_gui_delay"] 
  "Delay between updates to the GUI" float_option 10.
 
let temp_directory = define_option ["temp_directory" ] 
    "The directory where temporary files should be put" 
  string_option "temp"
  
let incoming_directory = 
  define_option ["incoming_directory" ] 
    "The directory where downloaded files should be moved after commit" 
  string_option "incoming"

let http_port = 
  define_option ["http_port"] "The port used to connect to your client with a WEB browser" int_option 4080
  
let http_login = 
  define_option ["http_login"] "Your login when using a WEB browser" string_option ""
  
let http_password = 
  define_option ["http_password"] "Your password when using a WEB browser" string_option ""
  
let max_upload_rate = define_option ["max_upload_rate"] 
  "The maximal upload rate you can tolerate" int_option 30

let max_xs_packets = define_option ["max_xs_packets"] 
  "Max number of UDP packets per round for eXtended Search" int_option 30

let max_dialog_history = define_option ["max_dialog_history"]
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

  
let features = define_option ["features"] "" string_list_option ""
(* put "no-up" in features to remove upload *)
let set_features () =
  List.iter (Mftp_client.set_features has_upload) (String2.tokens !!features)

  
let _ =
  option_hook features set_features
  
let password = define_option ["password"] 
  "The password to access your client from the GUI (setting it disables
  the command-line client)" string_option ""

let port = define_option ["port"] "The port used for connection by other donkey clients." int_option 4662

let save_options_delay = 
  define_option ["save_options_delay"] 
  "The delay between two saves of the 'downloads.ini' file" float_option 60.0

let check_client_connections_delay = 
  define_option ["check_client_connections_delay"] 
  "Delay used to request file sources" float_option 180.0
    
let check_connections_delay = 
  define_option ["check_connections_delay"] 
  "The delay between server connection rounds" float_option 5.0
  
let max_connected_servers = define_option ["max_connected_servers"] 
    "The number of servers you want to stay connected to" int_option 10

let max_udp_sends = define_option ["max_udp_sends"] 
    "The number of UDP packets you send every check_client_connections_delay" 
  int_option 10

  (*
let _ =
  option_hook max_connected_servers (fun _ ->
      if !!max_connected_servers > 10 && !has_upload then
        max_connected_servers =:= 10)
  *)

let retry_delay = define_option ["retry_delay"] "" float_option 3600.
let server_connection_timeout = define_option ["server_connection_timeout"] 
  "timeout when connecting to a server" float_option 30.

let telnet_port = define_option ["telnet_port"] "port for user interaction" int_option 4000

let max_server_age = define_option ["max_server_age"] "max number of days after which an unconnected server is removed" int_option 7

let use_file_history = define_option ["use_file_history"] "keep seen files in history to allow local search (can be expensive in memory)" bool_option true
  
let save_file_history = define_option ["save_file_history"] "save the file history in a file and load it at startup" bool_option true
  
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

let client_md4 = define_option ["client_md4"]
    "The MD4 of this client" Md4Option.t (Md4.random ())
  
let files = 
  define_option ["files"] 
  "The files currently being downloaded" (list_option FileOption.t) []

let done_files = 
  define_option ["done_files"] 
  "The files whose download is finished" (list_option FileOption.t) []

let old_files = 
  define_option ["old_files"] 
  "The files that were downloaded" (list_option Md4Option.t) []

let known_friends = 
  define_option ["friends"] 
  "The list of known friends" (list_option ClientOption.t) []

let known_servers = define_option ["known_servers"] "List of known servers"
    (list_option ServerOption.t) []

  
  
  
  
(************  UPDATE OPTIONS *************)  
  
let add_server ip port =
  try
    find_server ip port 
  with _ ->
      let s = new_server ip port !!initial_score in
      known_servers =:= s :: !!known_servers;
      s
  
let remove_server ip port =
  try
    let s = find_server ip port in
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

