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

open Int64ops
open Options  
open Printf2
open BasicSocket
  
open CommonGlobals
open CommonClient
open CommonServer
open CommonNetwork
open CommonOptions
open CommonTypes
open CommonFile


(*************************************************************************)
(*                                                                       *)
(*                         FILES                                         *)
(*                                                                       *)
(*************************************************************************)

module FileOption = struct
    
    let value_to_state v =
      match v with
      | StringValue "Paused" -> FilePaused
      | StringValue "Downloading" -> FileDownloading
      | StringValue "Downloaded" -> FileDownloaded
      | _ -> raise Not_found
    
    let state_to_value s = 
      match s with
      | FilePaused | FileAborted _ -> StringValue "Paused"
      | FileDownloaded -> StringValue "Downloaded"
      | _ -> StringValue "Downloading"
    
    
    let value_to_file is_done v =
      match v with
        Options.Module assocs ->
          let get_value name conv = conv (List.assoc name assocs) in
          let network = try get_value "file_network" value_to_string
            with _ -> "Donkey" in
          let network = network_find_by_name network in
          let file_state = 
            try
              get_value "file_state" value_to_state 
            with _ -> FileDownloading
          in          
          let file_size = try
              value_to_int64 (List.assoc "file_size" assocs) 
            with _ -> Int64.zero
          in
          
          let file = network_file_of_option network file_size 
              file_state assocs in
          let priority = try get_value "file_priority" value_to_int 
            with _ -> 0 in
          
          let impl = as_file_impl file in
          (try
              impl.impl_file_age <- 
                normalize_time (get_value "file_age" value_to_int)
            with _ -> ());
          set_file_state file file_state;       
          (match file_state with
              FileDownloading -> lprintf "New downloading file\n";
            | FileDownloaded -> lprintf "New downloaded file\n";
            | _ -> lprintf "..........\n"
          );
          
          (try
              set_file_best_name file
              (get_value "file_filename" value_to_string)
            with _ -> ());
          set_file_priority file priority;
          file
      | _ -> assert false
          
    let file_to_value file =
        let netname = string_to_value (file_network file).network_name in
      let impl = as_file_impl file in
      Options.Module (
        ("file_network", netname) ::
        ("file_size", int64_to_value (file_size file)) ::
        ("file_priority", int_to_value (file_priority file)) ::
        ("file_state", state_to_value (file_state file)) ::
        ("file_filename", string_to_value (file_best_name file)) ::
        ("file_age", IntValue (Int64.of_int impl.impl_file_age)) ::
          (file_to_option file)
        )
          
    let t is_done =
      define_option_class "File" (value_to_file is_done) file_to_value
    ;;
  end


let swarmers_section = file_section files_ini [] ""
let files_section = file_section files_ini [] "" 
  
let done_files = 
  define_option files_section ["done_files"] 
    "The files whose download is finished" (
    listiter_option (FileOption.t true)) []
  
let files = 
  define_option files_section ["files"] 
    "The files currently being downloaded" (
    listiter_option (FileOption.t false)) []

(*************************************************************************)
(*                                                                       *)
(*                         RESULTS                                       *)
(*                                                                       *)
(*************************************************************************)

let value_to_tag =
  value_to_tuple2 (fun (v1,v2) ->
      let name = value_to_string v1 in
      let value = match v2 with
          IntValue i -> Uint64 i
        | _ -> String (value_to_string v2)
      in
      { tag_name = name; tag_value = value })
  
let tag_to_value = 
  tuple2_to_value (fun tag ->
      string_to_value tag.tag_name, 
      match tag.tag_value with
        Uint64 i -> int64_to_value i
      | String s -> string_to_value s
      | Addr _ -> assert false
      | Fint64 i -> int64_to_value i
      | Uint16 n | Uint8 n -> int_to_value n
  )
  
module ResultOption = struct 
    
    let value_to_result v =
      match v with
        Options.Module assocs ->
          let get_value name conv = conv (List.assoc name assocs) in
          
          let uids = get_value "uids" (fun uids ->
                let uids = value_to_list value_to_string uids in
                List.map Uid.of_string uids) in
          let names = get_value "names" (value_to_list value_to_string) in
          let size = get_value "size" value_to_int64 in
          let time = get_value "time" value_to_int in
          let format = try get_value "format" value_to_string with _ -> "" in
          let file_type = try get_value "type" value_to_string with _ -> "" in
          let tags = try get_value "tags" (value_to_list value_to_tag) 
              with _ -> [] in
          let r = { 
              CommonResult.dummy_result with
              result_uids = uids;
              result_names = names;
              result_size = size;
              result_time = time;
              result_format = format;
              result_type = file_type;
              result_tags = tags;
            }            
          in
          let rs = CommonResult.update_result_num r in
          rs
      | _ -> assert false
    
    let result_to_value rs =
      let r = CommonResult.get_result rs in
      
      let tags = ref [] in
      List.iter (fun tag ->
          match tag.tag_name with
            "availability" | "completesources" -> ()
          | _ -> tags := tag :: !tags;
      ) r.result_tags;
      
      let list = [] in
      let list =  if !tags = [] then list else
          ("tags", list_to_value tag_to_value !tags) :: list
      in
      let list = if r.result_format = "" then list else
          ("format", string_to_value r.result_format) :: list
      in
      let list = if r.result_type = "" then list else
          ("type", string_to_value r.result_type) :: list
      in          
      let list = 
        ("uids", SmallList 
            (List.map (fun uid ->
                string_to_value (Uid.to_string uid)) r.result_uids)) ::
        ("names", SmallList
            (List.map string_to_value r.result_names)) ::
        ("size", int64_to_value r.result_size) ::
        ("time", int_to_value r.result_time) ::
        list
      in
      Options.Module list
      
    let t =
      define_option_class "Result" value_to_result result_to_value   
    end

let results_section = file_section results_ini [] ""
let results = define_option results_section ["results"] ""
    (list_option ResultOption.t) []
let known_uids = define_option results_section ["known_uids"] ""
    (list_option (tuple2_option (Uid.option, int_option))) []
  
(*************************************************************************)
(*                                                                       *)
(*                         SERVERS                                       *)
(*                                                                       *)
(*************************************************************************)

  
module ServerOption = struct
    
    let value_to_server v =
      match v with
        Options.Module assocs ->
          let get_value name conv = conv (List.assoc name assocs) in
          let network = try
              get_value "server_network" value_to_string
            with _ -> "Donkey"
          in
          let network = 
            try network_find_by_name network with e ->
                lprintf "Network %s not supported\n" network;
                raise e
              in
          let server = network_server_of_option network assocs in
          server
      | _ -> assert false
    
    let server_to_value server =
      Options.Module (
        ("server_network", 
          string_to_value (server_network server).network_name)
        ::
        (server_to_option server)
      )
      
    let t =
      define_option_class "Server" value_to_server server_to_value
  end


let servers = define_option servers_section
    ["known_servers"] "List of known servers"
    (intmap_option (fun s -> server_num s) ServerOption.t) Intmap.empty


(*************************************************************************)
(*                                                                       *)
(*                         QUERIES                                       *)
(*                                                                       *)
(*************************************************************************)

let rec string_of_option v =
  match v with
    Module m -> "{ MODULE }"
  | StringValue s -> Printf.sprintf "STRING [%s]" s
  | IntValue i -> Printf.sprintf "INT [%Ld]" i
  | FloatValue f -> Printf.sprintf "FLOAT [%f]" f
  | OnceValue v -> string_of_option v
  | List l | SmallList l ->
      (List.fold_left (fun s v ->
            s ^ (string_of_option v) ^ ";" 
        ) "LIST [" l) ^ "]"
  | DelayedValue _ -> assert false
      
      
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

      | Q_COMBO _ -> assert false
          
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
          Q_MINSIZE (label, Int64.to_string s)

      | SmallList [StringValue "MAXSIZE"; StringValue label; IntValue s]
      | List [StringValue "MAXSIZE"; StringValue label; IntValue s] ->
          Q_MAXSIZE (label, Int64.to_string s)

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
          Q_MP3_BITRATE (label, Int64.to_string s)
          
      | _ -> failwith (Printf.sprintf "Query option: error while parsing %s"
              (string_of_option  v)
          )
      
    let t = define_option_class "Query" value_to_query query_to_value    
  end

let searches_section = file_section searches_ini [] ""
  
let max_saved_searches = define_option searches_section
    ["max_saved_searches"] "Maximal number of saved searches"
    int_option 10
  
let customized_queries = define_option searches_section
  ["customized_queries"] ""
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
    "MP3 Search", 
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
    "Movie Search", 
    Q_AND [
      Q_KEYWORDS ("keywords", "");
      Q_HIDDEN [
        Q_MINSIZE ("Min Size", "500000000");
        Q_MEDIA ("Media", "Video");
        Q_FORMAT ("Format", "avi");
      ]
    ];
    "Album Search",
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

let special_queries = define_option searches_section
    ["special_queries"] "Shortcuts for special specialized searches"
    (list_option (tuple2_option (string_option, string_option)))
  [
    "-1cd", "-maxsize 735000000";
    "-movies", "avi -minsize 650000000 -1cd";
    "-mp3s", "mp3 -minsize 3000000 -maxsize 10000000";
    "-albums", "album -minsize 30000000 -maxsize 150000000";
    "-nosex", "-without xxx";
  ]
  
let customized_queries =
  let custom = ref None in
  fun () ->
    match !custom with
      Some cq -> cq
    | None ->
        
        let rec intern q =
          match q with
          | Q_AND list -> Q_AND (List.map intern list)
          | Q_OR list -> Q_OR (List.map intern list)
          | Q_HIDDEN list -> Q_HIDDEN (List.map intern list)
          | Q_ANDNOT (q1,q2) -> Q_ANDNOT (intern q1, intern q2)
          
          | Q_MP3_BITRATE (s, v) -> Q_MP3_BITRATE (_s s, v)
          | Q_MP3_ALBUM (s, v) -> Q_MP3_ALBUM (_s s, v)
          | Q_MP3_TITLE (s, v) -> Q_MP3_TITLE (_s s, v)
          | Q_MP3_ARTIST (s, v) -> Q_MP3_ARTIST (_s s, v)
          
          | Q_COMBO (s, v, l) -> Q_COMBO (_s s, v, l)
          
          | Q_MEDIA (s, v) -> Q_MEDIA (_s s, v)
          | Q_KEYWORDS (s, v) -> Q_KEYWORDS (_s s, v)
          | Q_MINSIZE (s, v) -> Q_MINSIZE (_s s, v)
          | Q_MAXSIZE (s, v) -> Q_MAXSIZE (_s s, v)
          | Q_FORMAT (s, v) -> Q_FORMAT (_s s, v)
          
          | Q_MODULE (s,q) -> Q_MODULE (_s s, intern q)
        in
        let qs = 
          (List.map (fun (s,v) ->
                _s s, intern v) !!customized_queries)
        in
        custom := Some qs;
        qs

        
(*************************************************************************)
(*                                                                       *)
(*                         CLIENTS                                       *)
(*                                                                       *)
(*************************************************************************)

        
module ClientOption = struct
    
    let value_to_client is_friend v =
      match v with
        Options.Module assocs ->
          let get_value name conv = conv (List.assoc name assocs) in
          let network = try
              get_value "client_network" value_to_string
            with _ -> "Donkey"
          in
          let network = network_find_by_name network in
          let c = network_client_of_option network is_friend assocs in
          c
      | _ -> assert false
          
    let client_to_value client =
      Options.Module (
        ("client_network", string_to_value (client_network client).network_name)
        ::
        (client_to_option client)
      )
      
    let t is_friend =
      define_option_class "Client" (value_to_client is_friend) 
      client_to_value
    ;;
  end

let friends_section = file_section friends_ini [] ""
let friends = 
  define_option friends_section ["friends"] 
    "The list of known friends" (listiter_option (ClientOption.t true)) []
  
      
let contacts = ref []

  
(*************************************************************************)
(*                                                                       *)
(*                         SHARING                                       *)
(*                                                                       *)
(*************************************************************************)
  
module SharingOption = struct
    
    let value_to_sharing v = 
      match v with
      | Module assocs -> begin
            let get_value name conv default =
              try conv (List.assoc name assocs) 
              with _ -> default 
            in
            let sharing_recursive = get_value "recursive"
                value_to_bool false 
            in
            let sharing_minsize = get_value "minsize"
                value_to_int64 zero 
            in
            let sharing_maxsize = get_value "maxsize"
                value_to_int64 Int64.max_int 
            in
            let sharing_extensions = get_value "extensions"
                (value_to_list value_to_string) [] 
            in
            {
             sharing_recursive = sharing_recursive; 
             sharing_minsize = sharing_minsize; 
             sharing_maxsize = sharing_maxsize; 
             sharing_extensions = sharing_extensions; 
            }
          end
      | _ -> assert false
    
    let sharing_to_value s =
      let list = [
          "recursive", bool_to_value s.sharing_recursive;
          "extensions", 
          list_to_value string_to_value s.sharing_extensions;
          "minsize", int64_to_value s.sharing_minsize;
          "maxsize", int64_to_value s.sharing_maxsize;
        ]
      in
      Options.Module list
      
    let t =
      define_option_class "Sharing" value_to_sharing
        sharing_to_value 

  end

let sharing_only_directory = {
      sharing_extensions = [];
      sharing_recursive = false;
      sharing_minsize = Int64.of_int 1;
      sharing_maxsize = Int64.max_int;
    }  
  
let sharing_strategies = define_option searches_section
    ["customized_sharing"] ""
    (list_option (tuple2_option (string_option, SharingOption.t)))
  [ 

(* For mp3 sharers: recursively share all .mp3 files < 10 MB *)
    "mp3s", {
      sharing_extensions = [".mp3"];
      sharing_recursive = true;
      sharing_minsize = zero;
      sharing_maxsize = megabytes 10;
    };

(* For video sharers: recursively share all .avi files > 500 MB *)
    "avis", {
      sharing_extensions = [".avi"];
      sharing_recursive = true;
      sharing_minsize = megabytes 500;
      sharing_maxsize = Int64.max_int;
    };

    "all_files", {
      sharing_extensions = [];
      sharing_recursive = true;
      sharing_minsize = Int64.of_int 1;
      sharing_maxsize = Int64.max_int;
    };
    
(* For incoming directory, share all files in the directory (not recursive) *)
    "only_directory", sharing_only_directory;
  ]

(*************************************************************************)
(*                                                                       *)
(*                         SHARED                                        *)
(*                                                                       *)
(*************************************************************************)
  
module SharedDirectoryOption = struct
    
    let value_to_shared_directory v = 
      match v with
      | Module assocs -> begin
            let get_value name conv =  conv (List.assoc name assocs) 
            in
            let get_value_safe name conv default =
              try conv (List.assoc name assocs) 
              with _ -> default 
            in
            let shdir_dirname = get_value "dirname" value_to_filename
            in
            let shdir_priority = get_value_safe "priority" value_to_int 0
            in
            let shdir_networks = get_value_safe "networks"
                (value_to_list value_to_string) [] 
            in
            let shdir_strategy = get_value_safe "strategy"
                value_to_string "only_directory"
            in
            {
              shdir_dirname = shdir_dirname; 
              shdir_strategy = shdir_strategy; 
              shdir_networks = shdir_networks; 
              shdir_priority = shdir_priority; 
            }
          end
      
      | SmallList [dir; prio] 
      | List [dir; prio] ->
          {
            shdir_dirname = value_to_filename dir; 
            shdir_strategy = "only_directory"; 
            shdir_networks = []; 
            shdir_priority = value_to_int prio; 
          }          
      | v -> 
          let dir = value_to_string v in
          let prio = 0 in
          {
            shdir_dirname = dir; 
            shdir_strategy = "only_directory"; 
            shdir_networks = []; 
            shdir_priority = prio; 
          }
          
    let shared_directory_to_value s =
      let list = [
          "dirname", filename_to_value s.shdir_dirname;
          "networks", 
          list_to_value string_to_value s.shdir_networks;
          "strategy", 
          string_to_value  s.shdir_strategy;
          "priority", int_to_value s.shdir_priority;
        ]
      in
      Options.Module list
      
    let t =
      define_option_class "Shared Directory" value_to_shared_directory
        shared_directory_to_value

  end
    
  
let shared_directories = 
  define_option CommonOptions.path_section ["shared_directories" ] 
  "Directories where files will be shared"
    (list_option SharedDirectoryOption.t) 
  [
    {
      shdir_dirname = "shared";
      shdir_priority = 0;
      shdir_networks = [];
      shdir_strategy = "all_files";
    }
  ]


    
(*************************************************************************)
(*                                                                       *)
(*                         Functions                                     *)
(*                                                                       *)
(*************************************************************************)

let load () = 
  Options.load files_ini;
  Options.load servers_ini;
  Options.load searches_ini;
  Options.load results_ini;
  results =:= [];
  List.iter (fun (uid, time) ->
      Hashtbl.add CommonResult.known_uids (Uid.to_uid uid) time;
  ) !!known_uids;
  known_uids =:= [];
  Options.load friends_ini
  
let save () = 
  networks_iter (fun n -> network_save_complex_options n);
  
  Options.save_with_help files_ini;
  Options.save_with_help searches_ini;
  Options.save_with_help friends_ini;
  Options.save_with_help servers_ini;
  begin
    match !!save_results with
    | 0 -> ()
    | 1 ->
        Hashtbl.iter (fun uid time ->
            let uid = Uid.create uid in
            known_uids =:= (uid, time) :: !!known_uids;
        ) CommonResult.known_uids;
        Options.save_with_help results_ini;
        known_uids =:= [];
    | _ ->
        CommonResult.results_iter (fun _ rs -> results =:= rs :: !!results);
        Options.save_with_help results_ini;
        results =:= [];
    end;
  lprintf "Options correctly saved";
  CommonGlobals.print_localtime ()
