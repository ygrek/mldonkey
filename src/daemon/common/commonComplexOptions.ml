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

let log_prefix = "[cCO]"

let lprintf_nl fmt =
  lprintf_nl2 log_prefix fmt

let lprintf_n fmt =
  lprintf2 log_prefix fmt

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
          let network = 
            try network_find_by_name network with e ->
                lprintf_nl
		  "Error %s for network %s while parsing file %s"
		    (Printexc2.to_string e)
		    network
		    (get_value "file_filename" value_to_string);
    lprintf_nl "This core is lacking support for network %s, exiting" network;
                exit_properly 70
          in
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

          (try
              set_file_best_name file
              (get_value "file_filename" value_to_string)
            with _ -> ());
          set_file_priority file priority;

          if !verbose then lprintf_nl "New %s file %s"
	      (match file_state with
                 FileDownloading -> "downloading"
               | FileDownloaded -> "downloaded"
               | _ -> "other")
	     (file_best_name file);

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
      { tag_name = field_of_string name; tag_value = value })
  
let tag_to_value = 
  tuple2_to_value (fun tag ->
      string_to_value (string_of_field tag.tag_name), 
      match tag.tag_value with
        Uint64 i -> int64_to_value i
      | String s -> string_to_value s
      | Addr _ -> assert false
      | Fint64 i -> int64_to_value i
      | Uint16 n | Uint8 n -> int_to_value n
      | Pair (n1,n2) -> assert false
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
      let r = CommonResult.IndexedResults.get_result rs in
      
      let tags = ref [] in
      List.iter (fun tag ->
          match tag.tag_name with
            Field_Availability | Field_Completesources -> ()
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
                lprintf_nl "Network %s not supported" network;
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
            let get_bool_value name =  get_value name value_to_bool false in
            let sharing_recursive = get_bool_value "recursive" in
            let sharing_minsize = get_value "minsize" value_to_int64 zero 
            in
            let sharing_maxsize = get_value "maxsize" value_to_int64
              Int64.max_int 
            in
            let sharing_extensions = get_value "extensions"
                (value_to_list value_to_string) [] 
            in
            let sharing_directories = get_bool_value "directories" in
            let sharing_incoming = get_bool_value "incoming" in
            {
              sharing_directories = sharing_directories;
              sharing_incoming = sharing_incoming;
              sharing_recursive = sharing_recursive; 
              sharing_minsize = sharing_minsize; 
              sharing_maxsize = sharing_maxsize; 
              sharing_extensions = sharing_extensions; 
            }
          end
      | _ -> assert false
    
    let sharing_to_value s =
      let assocs = [
          "extensions", 
          list_to_value string_to_value s.sharing_extensions;
          "minsize", int64_to_value s.sharing_minsize;
          "maxsize", int64_to_value s.sharing_maxsize;
        ]
      in
      let add_bool_value name v assocs =
        if v then
          (name , bool_to_value v) :: assocs
        else assocs
      in
      let assocs = add_bool_value "directories" s.sharing_directories assocs in
      let assocs = add_bool_value "incoming" s.sharing_incoming assocs in
      let assocs = add_bool_value "recursive" s.sharing_recursive assocs in

      Options.Module assocs
      
    let t =
      define_option_class "Sharing" value_to_sharing
        sharing_to_value 

  end

let sharing_only_directory = {
    sharing_incoming = false;
    sharing_directories = false;
    sharing_extensions = [];
    sharing_recursive = false;
    sharing_minsize = Int64.one;
    sharing_maxsize = Int64.max_int;
  }  
  
let sharing_incoming_directories = {
    sharing_incoming = true;
    sharing_directories = true;
    sharing_extensions = [];
    sharing_recursive = false;
    sharing_minsize = Int64.one;
    sharing_maxsize = Int64.max_int;
  }  
  
let sharing_incoming_files = {
    sharing_incoming = true;
    sharing_directories = false;
    sharing_extensions = [];
    sharing_recursive = false;
    sharing_minsize = Int64.one;
    sharing_maxsize = Int64.max_int;
  }

let sharing_directories = {
      sharing_incoming = false;
      sharing_directories = true;
      sharing_extensions = [];
      sharing_recursive = false;
      sharing_minsize = Int64.one;
      sharing_maxsize = Int64.max_int;
    }
  
let sharing_strategies = define_option searches_section
    ["customized_sharing"] ""
    (list_option (tuple2_option (string_option, SharingOption.t)))
  [ 

(* For mp3 sharers: recursively share all .mp3 files < 10 MB *)
    "mp3s", {
      sharing_incoming = false;
      sharing_directories = false;
      
      sharing_extensions = [".mp3"];
      sharing_recursive = true;
      sharing_minsize = zero;
      sharing_maxsize = megabytes 10;
    };

(* For video sharers: recursively share all .avi files > 500 MB *)
    "avis", {
      sharing_incoming = false;
      sharing_directories = false;
      sharing_extensions = [".avi"];
      sharing_recursive = true;
      sharing_minsize = megabytes 500;
      sharing_maxsize = Int64.max_int;
    };

    "all_files", {
      sharing_incoming = false;
      sharing_directories = false;
      sharing_extensions = [];
      sharing_recursive = true;
      sharing_minsize = Int64.one;
      sharing_maxsize = Int64.max_int;
    };
    
    "incoming_files",  sharing_incoming_files;
    
    "incoming_directories", sharing_incoming_directories;
    
(* For incoming directory, share all files in the directory (not recursive) *)
    "only_directory", sharing_only_directory;
    
    "directories", sharing_directories;
  ]

let _ =
  option_hook sharing_strategies (fun _ ->
      
      if not (List.exists 
            (fun (_,s) -> s.sharing_incoming && s.sharing_directories) 
          !!sharing_strategies) then
        sharing_strategies =:= 
          ("incoming_directories", sharing_incoming_directories) :: 
        !!sharing_strategies;
      
      if not (List.exists 
            (fun (_,s) -> s.sharing_incoming && not s.sharing_directories) 
          !!sharing_strategies) then
        sharing_strategies =:= 
          ("incoming_files", sharing_incoming_files) :: !!sharing_strategies;
      
  )

let sharing_strategies name =
  match name with
  | "incoming_files" -> sharing_incoming_files
  | "incoming_directories" -> sharing_incoming_directories
  | "only_directory" -> sharing_only_directory
  | "directories" -> sharing_directories
  | _ ->
      try
        List.assoc name !!sharing_strategies
      with _ -> sharing_only_directory
  
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
  "                Incoming and shared directories.
   At least two entries have to be present here, one with strategy
   incoming_files and one with strategy incoming_directories.
   Both entries can point to the same directory.
   If one of the two strategies is missing, MLDonkey will create a default
   directory with its entry here.
   Finished BT multifile downloads are committed to the first directory
   with strategy incoming_directories. Other downloads are committed
   to the first directory with the strategy incoming_files.
   If more than one directory has one of the incoming_* strategies
   it will be ignored on commit, but they are shared nonetheless.
   Other strategies can be found in searches.ini, section customized_sharing."
    (list_option SharedDirectoryOption.t) 
  [
    {
      shdir_dirname = "shared";
      shdir_priority = 0;
      shdir_networks = [];
      shdir_strategy = "all_files";
    };
    {
      shdir_dirname = "incoming/files";
      shdir_priority = 0;
      shdir_networks = [];
      shdir_strategy = "incoming_files";
    };
    {
      shdir_dirname = "incoming/directories";
      shdir_priority = 0;
      shdir_networks = [];
      shdir_strategy = "incoming_directories";
    }
  ]


let search_incoming_files () =
  try
    List.find (fun s -> s.shdir_strategy = "incoming_files") 
    !!shared_directories
  with Not_found ->
        let dirname = Filename.concat "incoming" "files" in
        let s = {
        shdir_dirname = dirname;
        shdir_priority = 0;
        shdir_networks = [];
        shdir_strategy = "incoming_files";
          }
        in
        shared_directories =:= s :: !!shared_directories;
        s

let incoming_files () =
  let dir = search_incoming_files () in
    Unix2.safe_mkdir dir.shdir_dirname;
    Unix2.can_write_to_directory dir.shdir_dirname;
    dir

let search_incoming_directories () =
  try
    List.find (fun s -> s.shdir_strategy = "incoming_directories") 
    !!shared_directories
  with Not_found ->
      let dirname = Filename.concat "incoming" "directories" in
      let s = {
          shdir_dirname = dirname;
          shdir_priority = 0;
          shdir_networks = [];
          shdir_strategy = "incoming_directories";
        }
      in
      shared_directories =:= s :: !!shared_directories;
      s

let incoming_directories () =
  let dir = search_incoming_directories () in
    Unix2.safe_mkdir dir.shdir_dirname;
    Unix2.can_write_to_directory dir.shdir_dirname;
    dir

let _ =
(* Check the definition of the incoming_files and incoming_directories in
shared_directories *)
  let verification = ref false in
  option_hook shared_directories (fun _ ->
      if not !verification then begin
          verification := true;
          ignore (incoming_files ());
          ignore (incoming_directories ());
          verification := false
        end
  )
      
    
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

let allow_saving_ini_files = ref true

let save () =
  if !allow_saving_ini_files then begin
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
  lprintf_nl "Options correctly saved"
  end

let save_sources () =
  if !allow_saving_ini_files then begin
  networks_iter (fun n -> network_save_sources n);
  lprintf_nl "Sources correctly saved"
  end

open Zip

let backup_zip archive files =
  try 
    Unix2.tryopen_write_zip archive (fun oc ->
      List.iter (fun file ->
	try
          let s = Unix.stat file in
	  Zip.copy_file_to_entry file oc ~level:9 ~mtime:s.Unix.st_mtime file
	with e ->
	  failwith (Printf.sprintf "Zip: error %s in %s" (Printexc2.to_string e) file)
      ) files)
  with e ->
    failwith (Printf.sprintf "Zip: error %s in %s" (Printexc2.to_string e) archive)

open Tar

let backup_tar archive files =
  let failed_files = ref [] in
  Unix2.tryopen_write_tar ~compress:`Gzip archive (fun otar ->
    List.iter (fun arg ->
      try
	let header, s =
	  Unix2.tryopen_read_bin arg (fun ic ->
	    let stat = Unix.stat arg in
	    let size = stat.Unix.st_size in
	    if size > Sys.max_string_length then
	      failwith (Printf.sprintf "Tar: file %s too big, limit %d byte" arg Sys.max_string_length);
	    let header = 
	      { Tar.t_name = arg;
	      t_mode = 0o644;
	      t_uid = stat.Unix.st_uid;
	      t_gid = stat.Unix.st_gid;
	      t_size = 0;
	      t_mtime = Int32.of_float stat.Unix.st_mtime;
	      t_chksum = 0;
	      t_typeflag = REGULAR;
	      t_linkname = "";
	      t_format = POSIX_FORMAT;
	      t_uname = "";
	      t_gname = "";
	      t_devmajor = 0;
	      t_devminor = 0;
	      t_prefix = "";
	      t_gnu = None;} in
	    let s = String.create size in
	    Pervasives.really_input ic s 0 size;
	    header, s) in
	Tar.output otar header s
      with e ->
	let error = Printexc2.to_string e in
	if error = "Gzip.Error(\"error during compression\")"
	  && Autoconf.windows && arg = "fasttrack.ini" then begin
	    (* for whatever reason this error is raised on Windows,
               but fasttrack.ini is stored correctly *)
	    if !verbose then
        lprintf_nl "Tar: Windows specific pseudo error %s in %s" error arg
	  end
	else begin
	  failed_files := arg :: !failed_files;
    lprintf_nl "Tar: error %s in %s" error arg
        end
    ) files);
    if !failed_files <> [] then
      failwith (Printf.sprintf "Tar: error backing up %s"
	(String.concat " " (List.rev !failed_files)))

let backup_options () =
  let counter = ref 1 in
  let backup_prefix = "backup-" in
  let old_backups = List.rev (List.sort (fun o -> compare o)
    (List.filter (fun o -> (
	String.lowercase (Filename2.extension o) = ".tar.gz"
	|| String.lowercase (Filename2.extension o) = ".zip")
	  && String.sub o 0 (String.length backup_prefix) = backup_prefix)
    (Unix2.list_directory "old_config")))
  in
  List.iter (fun s ->
    incr counter;
    if !counter > !!backup_options_generations then
        Sys.remove (Filename.concat "old_config" s)
    ) old_backups;
  let format =
    begin
      match !!backup_options_format with
        "zip" -> ".zip"
      | _ -> ".tar.gz"
    end
  in
  begin
    try
    let archive = Filename.concat "old_config" (backup_prefix ^ Date.reverse (Unix.time ()) ^ format) in
      let files = List.sort (fun o -> compare o) (List.filter (fun o ->
	  String.lowercase (Filename2.last_extension o) = ".ini"
	  && o <> "file_sources.ini") 
	    (Unix2.list_directory file_basedir)) in
      begin
        match (Filename2.last_extension archive) with
          ".zip" -> backup_zip archive files
        | _ -> backup_tar archive files
      end
    with e -> lprintf_nl "Exception %s while options backup" (Printexc2.to_string e); raise e
  end;
  lprintf_nl "Options backup as %s correctly saved" format
             
let _ =
  CommonBlocking.add_update_hook CommonServer.check_blocked_servers;
  CommonBlocking.add_update_hook CommonServer.server_must_update_all;
  option_hook ip_blocking (fun _ ->
    try
      CommonBlocking.set_ip_blocking_list !!ip_blocking
    with _ -> ()
  );
  option_hook ip_blocking_countries (fun _ ->
    CommonBlocking.set_ip_blocking_countries !!ip_blocking_countries
  );
  option_hook ip_blocking_countries_block (fun _ ->
    CommonBlocking.set_ip_blocking_countries_block !!ip_blocking_countries_block;
    CommonBlocking.set_ip_blocking_countries !!ip_blocking_countries
  );
  option_hook geoip_dat (fun _ ->
    try
      CommonBlocking.set_geoip_dat !!geoip_dat
    with _ -> ()
  );
  option_hook max_opened_connections (fun _ ->
  if !verbose then lprintf_nl
    "checking max_opened_connections = %d for validity" !!max_opened_connections;
(* original code from ./src/config/unix/MlUnix.ml
let max_all_sockets = getdtablesize ()
let max_sockets = max (max_all_sockets - 100) (max_all_sockets / 2)
let max_filedescs = (max_all_sockets - max_sockets) / 2 *)

  (* ulimit open files. minimum 150, most systems have 1024 *)
  let max_all_sockets = Unix2.c_getdtablesize () in

  (* old max_sockets code: max (150 - 100) (150 / 2),
     minimum number of max_opened_connections *)
  let min_conns = 75 in

  if min_conns > !!max_opened_connections then begin
    lprintf_nl "max_opened_connections is set too low (%d), raising to %d"
      !!max_opened_connections min_conns;
    max_opened_connections =:= min_conns
  end;

  let total_files = (* maximum number of files in use at the same time *)
    (maxi (List.length !!files) !!max_concurrent_downloads) + !!max_upload_slots + 20 (* ini files etc. *)
  in

  let wanted_socks = !!max_opened_connections + total_files in

  if max_all_sockets < wanted_socks then
    if max_all_sockets < total_files + min_conns then (* check if ulimit is enough to allow total_files + min_conns *)
      begin
        lprintf_nl "only %d file descriptors available, raise ulimit open files to at least %d"
          max_all_sockets wanted_socks;
        lprintf_nl "FD info: max_opened_connections %d, number of (possible) concurrent downloads %d, = %d fd needed"
          !!max_opened_connections total_files wanted_socks;
        CommonGlobals.exit_properly 71
      end
  else
    begin
      let new_max_opened_connections =
        maxi (max_all_sockets - total_files) (max_all_sockets / 2)
      in
      lprintf_nl "max_opened_connections is set too high (%d), reducing to %d"
        !!max_opened_connections new_max_opened_connections;
      max_opened_connections =:= new_max_opened_connections;
    end;

  if !verbose then lprintf_nl
    "max_opened_connections %d, total_files %d, max_concurrent_downloads %d, !!files %d"
      !!max_opened_connections total_files !!max_concurrent_downloads (List.length !!files);

  TcpBufferedSocket.set_max_opened_connections
    (fun _ -> !!max_opened_connections);

  Unix32.max_cache_size := total_files - 20;
  calc_real_max_indirect_connections ()
)

let _ =
  Heap.add_memstat "CommonComplexOptions" (fun level buf ->
      Printf.bprintf buf "  friends: %d\n" (List.length !!friends);
      Printf.bprintf buf "  contacts: %d\n" (List.length !contacts);
  )
