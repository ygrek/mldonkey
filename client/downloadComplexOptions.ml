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

open Options
open DownloadTypes
open Gui_types
open DownloadOptions

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
  
module ClientOption = struct
    let value_to_client v = 
      match v with
        List [ip;port] | SmallList [ip;port] ->
          let ip = Ip.of_string (value_to_string ip) in
          let port = value_to_int port in
          DownloadGlobals.new_client (Known_location (ip, port))
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
          let l = DownloadGlobals.new_client kind in
          
          (try
              l.client_md4 <- get_value "client_md4" value_to_md4 
            with _ -> ());
          (try
              l.client_name <- get_value "client_name" value_to_string
            with _ -> ());
          (try
              l.client_checked <- get_value "client_checked" value_to_bool
            with _ -> ());
          (try
              DownloadGlobals.connection_set_last_conn l.client_connection_control
                (min (get_value "client_age" value_to_float) 
                (BasicSocket.last_time ()));
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
            "client_age", float_to_value (
              DownloadGlobals.connection_last_conn 
                c.client_connection_control);
            "client_checked", bool_to_value c.client_checked;
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
          DownloadGlobals.new_server ip port !!initial_score
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
          let l = DownloadGlobals.new_server ip port !!initial_score in
          
          (try
              l.server_description <- get_value "server_desc" value_to_string 
            with _ -> ());
          (try
              l.server_name <- get_value "server_name" value_to_string
            with _ -> ());
          (try
              DownloadGlobals.connection_set_last_conn l.server_connection_control
                (min (get_value "server_age" value_to_float) 
                (BasicSocket.last_time ()));
            with _ -> ());
          l
      | _ -> failwith "Options: Not a server"

                  
    let server_to_value c =
      Options.Module [
        "server_addr", addr_to_value c.server_ip  c.server_port;
        "server_desc", string_to_value c.server_description;
        "server_name", string_to_value c.server_name;
        "server_age", float_to_value (
          DownloadGlobals.connection_last_conn c.server_connection_control);
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
    
    let value_to_state v =
      match v with
        StringValue "Paused" -> FilePaused
      | StringValue "Downloading" -> FileDownloading
      | _ -> raise Not_found
    
    let state_to_value s = 
      match s with
        FilePaused -> StringValue "Paused"
      | _ -> StringValue "Downloading"
    
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
          
          let file = DownloadGlobals.new_file (
              Filename.concat !!temp_directory file_md4_name)
            (Md4.of_string file_md4_name) file_size true in
          
          (try 
              if file.file_exists then begin
(* only load absent chunks if file previously existed. *)
                  file.file_absent_chunks <- 
                    get_value "file_absent_chunks" 
                    (value_to_list value_to_int32pair);
                end
            with _ -> ()                );
          
          (try
              let file_state = get_value "file_state" value_to_state in
              file.file_state <- file_state;
            with _ -> ());
          
          file.file_filenames <-
            get_value_nil "file_filenames" (value_to_list value_to_string);
          
          (try
              file.file_all_chunks <- get_value "file_all_chunks"
                value_to_string
            with _ -> ());
          
          List.iter (fun c ->
              file.file_known_locations <- Intmap.add c.client_num  c
                file.file_known_locations;
              if not (List.memq file c.client_files) then
                c.client_files <- file :: c.client_files;                  
          )
          (get_value_nil "file_locations" (value_to_list 
                ClientOption.value_to_client));
          
          (try
              file.file_chunks_age <-
              get_value "file_chunks_age" 
                (fun v -> 
                  let list = value_to_list value_to_float v in
                  Array.of_list list)
            with _ -> ());
          
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
      let locs = ref [] in
      Intmap.iter (fun _ c ->
          if c.client_md4 <> Md4.null then 
            locs := c :: !locs
      ) file.file_known_locations;
         
      Options.Module [
        "file_md4", string_to_value (Md4.to_string file.file_md4);
        "file_size", int32_to_value file.file_size;
        "file_all_chunks", string_to_value file.file_all_chunks;
        "file_state", state_to_value file.file_state;
        "file_absent_chunks", List
          (List.map (fun (i1,i2) -> 
              SmallList [int32_to_value i1; int32_to_value i2])
          file.file_absent_chunks);
        "file_filenames", List
          (List.map (fun s -> string_to_value s) file.file_filenames);
        "file_locations", list_to_value ClientOption.client_to_value
          !locs;
        "file_md4s", List
          (List.map (fun s -> string_to_value (Md4.to_string s)) 
          file.file_md4s);
        "file_downloaded", int32_to_value file.file_downloaded;
        "file_chunks_age", List (Array.to_list 
            (Array.map float_to_value file.file_chunks_age));
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
    
  
let allowed_ips = define_option downloads_ini ["allowed_ips"]
    "list of IP address allowed to control the client via telnet/GUI/WEB"
    (list_option Ip.option) [Ip.of_string "127.0.0.1"]

let _ = 
  Options.set_string_wrappers allowed_ips 
    (fun list ->
      List.fold_left (fun s ip ->
          Printf.sprintf "%s %s" (Ip.to_string ip) s
      ) "" list
  )
  (fun s ->
      let list = String2.tokens s in
      List.map (fun ip -> Ip.of_string ip) list
  )
    
  
let done_files = 
  define_option files_ini ["done_files"] 
  "The files whose download is finished" (list_option FileOption.t) []

let old_files = 
  define_option downloads_ini ["old_files"] 
  "The files that were downloaded" (list_option Md4.option) []

let known_friends = 
  define_option friends_ini ["friends"] 
  "The list of known friends" (list_option ClientOption.t) []

let client_md4 = define_option downloads_ini ["client_md4"]
    "The MD4 of this client" Md4.option (Md4.random ())
  
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
    DownloadGlobals.find_server ip port 
  with _ ->
      let s = DownloadGlobals.new_server ip port !!initial_score in
      DownloadGlobals.servers_ini_changed := true;
      known_servers =:= s :: !!known_servers;
      s
  
let remove_server ip port =
  try
    let s = DownloadGlobals.find_server ip port in
    DownloadGlobals.servers_ini_changed := true;
    known_servers  =:= List2.removeq s  !!known_servers ;
    DownloadGlobals.remove_server ip port
  with _ -> ()

      (*
type query_entry = 
  Q_AND of query_entry list
| Q_OR of query_entry list
| Q_NOT of query_entry * query_entry
  
| Q_KEYWORDS of string * string
| Q_MINSIZE of string * string
| Q_MAXSIZE of string * string
| Q_FORMAT of string * string
| Q_KIND of string * string
  
| Q_MP3_AUTHORS of string * string
| Q_MP3_TITLE of string * string
| Q_MP3_ALBUM of string * string
| Q_MP3_BITRATE of string * string
    *)

let rec string_of_option v =
  match v with
    Module m -> "{ MODULE }"
  | StringValue s -> Printf.sprintf "STRING [%s]" s
  | IntValue i -> Printf.sprintf "INT [%s]" (Int32.to_string i)
  | FloatValue f -> Printf.sprintf "FLOAT [%f]" f
  | List l | SmallList l ->
      (List.fold_left (fun s v ->
            s ^ (string_of_option v) ^ ";" 
        ) "LIST [" l) ^ "]"

module QueryOption = struct
    let rec query_to_value q =
      match q with
      | Q_AND list ->
          List ((StringValue "AND") :: (List.map query_to_value list))
      | Q_OR list ->
          List ((StringValue "OR"):: (List.map query_to_value list))
      | Q_HIDDEN list ->
          List ((StringValue "HIDDEN"):: (List.map query_to_value list))
      | Q_ANDNOT (q1, q2) ->
          SmallList [StringValue "ANDNOT"; query_to_value q1; query_to_value q2 ]
      | Q_MODULE (s, q) ->
          SmallList [StringValue "MODULE"; StringValue s;  query_to_value q]
          
        
        | Q_KEYWORDS (label, s) ->
            SmallList [StringValue "KEYWORDS"; StringValue label; StringValue s]
        | Q_MINSIZE (label, s) ->
            SmallList [StringValue "MINSIZE"; StringValue label; StringValue s]
        | Q_MAXSIZE (label, s) ->
            SmallList [StringValue "MAXSIZE"; StringValue label; StringValue s]
        | Q_FORMAT (label, s) ->
            SmallList [StringValue "FORMAT"; StringValue label; StringValue s]
        | Q_MEDIA (label, s) ->
            SmallList [StringValue "MEDIA"; StringValue label; StringValue s]
        
        | Q_MP3_ARTIST (label, s) ->
            SmallList [StringValue "MP3_ARTIST"; StringValue label; StringValue s]
        | Q_MP3_TITLE (label, s) ->
            SmallList [StringValue "MP3_TITLE"; StringValue label; StringValue s]
        | Q_MP3_ALBUM (label, s) ->
            SmallList [StringValue "MP3_ALBUM"; StringValue label; StringValue s]
        | Q_MP3_BITRATE (label, s) ->
            SmallList [StringValue "MP3_BITRATE"; StringValue label; StringValue s]
          
    let rec value_to_query v =
      match v with
      | SmallList ((StringValue "AND") :: list)
      | List ((StringValue "AND") :: list) -> 
          Q_AND (List.map value_to_query list)

      | SmallList ((StringValue "OR") :: list)
      | List ((StringValue "OR") :: list) -> 
          Q_OR (List.map value_to_query list)

      | SmallList ((StringValue "HIDDEN") :: list)
      | List ((StringValue "HIDDEN") :: list) -> 
          Q_HIDDEN (List.map value_to_query list)

      | SmallList [StringValue "ANDNOT"; v1; v2 ]
      | List [StringValue "ANDNOT"; v1; v2 ] -> 
          Q_ANDNOT (value_to_query v1, value_to_query v2)

      | SmallList [StringValue "MODULE"; StringValue label; v2 ]
      | List [StringValue "MODULE"; StringValue label; v2 ] -> 
          Q_MODULE (label, value_to_query v2)


          
      | SmallList [StringValue "KEYWORDS"; StringValue label; StringValue s]
      | List [StringValue "KEYWORDS"; StringValue label; StringValue s] ->
          Q_KEYWORDS (label, s)

      | SmallList [StringValue "MINSIZE"; StringValue label; StringValue s]
      | List [StringValue "MINSIZE"; StringValue label; StringValue s] ->
          Q_MINSIZE (label, s)

      | SmallList [StringValue "MAXSIZE"; StringValue label; StringValue s]
      | List [StringValue "MAXSIZE"; StringValue label; StringValue s] ->
          Q_MAXSIZE (label, s)

      | SmallList [StringValue "MINSIZE"; StringValue label; IntValue s]
      | List [StringValue "MINSIZE"; StringValue label; IntValue s] ->
          Q_MINSIZE (label, Int32.to_string s)

      | SmallList [StringValue "MAXSIZE"; StringValue label; IntValue s]
      | List [StringValue "MAXSIZE"; StringValue label; IntValue s] ->
          Q_MAXSIZE (label, Int32.to_string s)

      | SmallList [StringValue "FORMAT"; StringValue label; StringValue s]
      | List [StringValue "FORMAT"; StringValue label; StringValue s] ->
          Q_FORMAT (label, s)

      | SmallList [StringValue "MEDIA"; StringValue label; StringValue s]
      | List [StringValue "MEDIA"; StringValue label; StringValue s] ->
          Q_MEDIA (label, s)
          
      | SmallList [StringValue "MP3_ARTIST"; StringValue label; StringValue s]
      | List [StringValue "MP3_ARTIST"; StringValue label; StringValue s] ->
          Q_MP3_ARTIST (label, s)

      | SmallList [StringValue "MP3_TITLE"; StringValue label; StringValue s]
      | List [StringValue "MP3_TITLE"; StringValue label; StringValue s] ->
          Q_MP3_TITLE (label, s)

      | SmallList [StringValue "MP3_ALBUM"; StringValue label; StringValue s]
      | List [StringValue "MP3_ALBUM"; StringValue label; StringValue s] ->
          Q_MP3_ALBUM (label, s)

      | SmallList [StringValue "MP3_BITRATE"; StringValue label; StringValue s]
      | List [StringValue "MP3_BITRATE"; StringValue label; StringValue s] ->
          Q_MP3_BITRATE (label, s)

      | SmallList [StringValue "MP3_BITRATE"; StringValue label; IntValue s]
      | List [StringValue "MP3_BITRATE"; StringValue label; IntValue s] ->
          Q_MP3_BITRATE (label, Int32.to_string s)
          
      | _ -> failwith (Printf.sprintf "Query option: error while parsing %s"
              (string_of_option  v)
          )
      
    let t = define_option_class "Query" value_to_query query_to_value    
  end
      
let customized_queries = define_option searches_ini ["customized_queries"] ""
    (list_option (tuple2_option (string_option, QueryOption.t)))
  [ 
    "Complex Search", 
    Q_AND [
      Q_KEYWORDS ("keywords", "");
      Q_MODULE ("Simple Options",
        Q_AND [
          Q_MINSIZE ("Min Size", "");
          Q_MAXSIZE ("Max Size", "");
          Q_MEDIA ("Media", "");
          Q_FORMAT ("Format", "");
        ];
      );
      Q_MODULE ("Mp3 Options",
        Q_AND [
          Q_MP3_ARTIST ("Artist", ""); 
          Q_MP3_ALBUM ("Album", ""); 
          Q_MP3_TITLE ("Title", ""); 
          Q_MP3_BITRATE ("Min Bitrate", ""); 
        ]
      );
    ];
    "Search for mp3s", 
    Q_AND [
      Q_KEYWORDS ("keywords", "");
      Q_MP3_ARTIST ("Artist", ""); 
      Q_MP3_ALBUM ("Album", ""); 
      Q_MP3_TITLE ("Title", ""); 
      Q_MP3_BITRATE ("Min Bitrate", ""); 
      Q_HIDDEN [
        Q_MEDIA ("Media", "Audio");
        Q_FORMAT ("Format", "mp3");
      ]
    ];
    "Search for movies", 
    Q_AND [
      Q_KEYWORDS ("keywords", "");
      Q_HIDDEN [
        Q_MINSIZE ("Min Size", "500000000");
        Q_MEDIA ("Media", "Video");
        Q_FORMAT ("Format", "avi");
      ]
    ];
    "Search for albums",
    Q_AND [
      Q_KEYWORDS ("Keywords", "album");
      Q_HIDDEN [
        Q_ANDNOT (
          Q_MINSIZE ("Min Size", "30000000"),
          Q_FORMAT ("Format", "mp3")
        );
      ]
    ];
  ]
