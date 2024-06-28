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
open CommonUserDb
open CommonTypes
open CommonFile
open Gettext

let _s x = _s "CommonComplexOptions" x
let _b x = _b "CommonComplexOptions" x

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
          let get_value_nil name conv =
            try conv (List.assoc name assocs) with Not_found -> [] in
          let filename = get_value "file_filename" value_to_string in
          let network = try get_value "file_network" value_to_string
            with _ -> "Donkey" in
          let network = 
            try network_find_by_name network with e ->
                lprintf_nl
                  "Error %s for network %s while parsing file %s"
                    (Printexc2.to_string e) network filename;
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
          
          let file_user =
            try
              let u = get_value "file_owner" value_to_string in
                begin
                  try
                    user2_user_find u
                  with Not_found ->
                    lprintf_nl "file_owner %s of %s does not exist, changing to %s"
                      u filename (admin_user ()).user_name;
                    admin_user ()
                end
            with Not_found ->
                lprintf_nl "file_owner of %s is empty, changing to %s"
                  filename (admin_user ()).user_name;
                admin_user ()
          in

          let file_group =
            let dgroup = user2_print_user_default_group file_user in
            try
              match (get_value "file_group" value_to_stringoption) with
                None -> None
              | Some g ->
                  begin
                    try
                      let g = user2_group_find g in
                      if List.mem g file_user.user_groups then
                        Some g
                      else
                        begin
                          lprintf_nl "file_owner %s is not member of file_group %s, changing file_group of %s to user_default_group %s"
                            file_user.user_name g.group_name filename dgroup;
                          file_user.user_default_group
                        end
                    with Not_found ->
                      lprintf_nl "file_group %s of %s not found, changing to user_default_group %s of user %s"
                        g filename dgroup file_user.user_name;
                      file_user.user_default_group
                  end
            with Not_found ->
                lprintf_nl "file_group of %s is empty, changing to user_default_group %s of user %s"
                  filename dgroup file_user.user_name;
                file_user.user_default_group
          in

          let file = network_file_of_option network file_size 
              file_state file_user file_group assocs in
          
          let impl = as_file_impl file in
          (try
              impl.impl_file_age <- 
                normalize_time (get_value "file_age" value_to_int)
            with _ -> ());

          (try
              impl.impl_file_release <- 
                get_value "file_release" value_to_bool
            with _ -> ());

          set_file_state file file_state;       

          (try
              set_file_best_name file filename 0
            with _ -> ());

          (try
            List.iter (fun s -> add_file_filenames file s)
              (get_value_nil "file_filenames" (value_to_list value_to_string))
            with _ -> ());

          let priority = try get_value "file_priority" value_to_int 
            with _ -> 0 in
          set_file_priority file priority;

          if !verbose && !CommonGlobals.is_startup_phase then
            lprintf_nl "Started download of %s, user:group %s:%s"
              (file_best_name file)
              file_user.user_name
              (user2_print_group file_group);

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
        ("file_filenames", List
        (List.map string_to_value impl.impl_file_filenames)) ::
        ("file_age", IntValue (Int64.of_int impl.impl_file_age)) ::
        ("file_release", bool_to_value impl.impl_file_release) ::
        ("file_owner", string_to_value (file_owner file).user_name) ::
        ("file_group", stringoption_to_value (match file_group file with Some g -> Some g.group_name | None -> None)) ::
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
    "The files currently being downloaded, primary downloads must come first" (
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
                lprintf_nl "Loading servers, network %s not supported, deleting server" network;
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
    "-nosex", "-not xxx";
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
    sharing_minsize = zero;
    sharing_maxsize = Int64.max_int;
  }  
  
let sharing_incoming_directories = {
    sharing_incoming = true;
    sharing_directories = true;
    sharing_extensions = [];
    sharing_recursive = false;
    sharing_minsize = zero;
    sharing_maxsize = Int64.max_int;
  }  
  
let sharing_incoming_files = {
    sharing_incoming = true;
    sharing_directories = false;
    sharing_extensions = [];
    sharing_recursive = true;
    sharing_minsize = zero;
    sharing_maxsize = Int64.max_int;
  }

let sharing_directories = {
      sharing_incoming = false;
      sharing_directories = true;
      sharing_extensions = [];
      sharing_recursive = false;
      sharing_minsize = zero;
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
      sharing_minsize = zero;
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

let sharing_strategy name =
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
(*
            let shdir_networks = get_value_safe "networks"
                (value_to_list value_to_string) [] 
            in
*)
            let shdir_strategy = get_value_safe "strategy"
                value_to_string "only_directory"
            in
            {
              shdir_dirname = shdir_dirname; 
              shdir_strategy = shdir_strategy; 
              shdir_networks = []; (* shdir_networks; *)
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
(*
          "networks", 
          list_to_value string_to_value s.shdir_networks;
*)
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

let default_incoming_files = {
  shdir_dirname = Filename.concat "incoming" "files";
  shdir_priority = 0;
  shdir_networks = [];
  shdir_strategy = "incoming_files";
  }

let default_incoming_directories = {
  shdir_dirname = Filename.concat "incoming" "directories";
  shdir_priority = 0;
  shdir_networks = [];
  shdir_strategy = "incoming_directories";
  }

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
   MLdonkey searches all shared_directories with incoming_* strategies
   on commit and uses the first one with enough free diskspace.
   Other strategies can be found in searches.ini, section customized_sharing."
    (list_option SharedDirectoryOption.t) 
  [
    {
      shdir_dirname = "shared";
      shdir_priority = 0;
      shdir_networks = [];
      shdir_strategy = "all_files";
    };
    default_incoming_files;
    default_incoming_directories;
  ]


let search_incoming_files () =
  let list =
    List.filter (fun s -> s.shdir_strategy = "incoming_files") !!shared_directories
  in
  match list with
  | [] -> shared_directories =:= default_incoming_files :: !!shared_directories;
        [default_incoming_files]
  | l -> l

let search_incoming_directories () =
  let list =
    List.filter (fun s -> s.shdir_strategy = "incoming_directories") !!shared_directories
  in
  match list with
  | [] -> shared_directories =:= default_incoming_directories :: !!shared_directories;
        [default_incoming_directories]
  | l -> l

let shared_directories_including_user_commit () =
(* This function is to be used in bTInteractive.try_share_file which is not recursive.
   Its a replacement for !!shared_directories and provides the same list, but with
   sub-directories added based on user_commit_dir.
   This function works without disc access to avoid overhead. *)
  let list = ref [] in
  List.iter (fun s ->
    let user_commit_dir_list = ref [] in
    if (sharing_strategy s.shdir_strategy).sharing_incoming then
      begin
        user2_users_iter (fun u ->
          if u.user_commit_dir <> "" then
            user_commit_dir_list := !user_commit_dir_list @
              [{
                shdir_dirname = Filename.concat s.shdir_dirname u.user_commit_dir;
                shdir_priority = s.shdir_priority;
                shdir_networks = s.shdir_networks;
                shdir_strategy = s.shdir_strategy;
              }]
        )
      end;
    list := !list @ [s] @ !user_commit_dir_list
  ) !!shared_directories;
  !list

let incoming_dir usedir ?user ?needed_space ?network () =

  let directories =
    if usedir then
      search_incoming_directories ()
    else
      search_incoming_files ()
  in

  let dirname_user =
    match user with
    | None -> ""
    | Some user -> user.user_commit_dir
  in

(*
  let dirname_network =
    match network with
    | None -> ""
    | Some network -> network
  in
*)
(* todo: make the dir naming order user configurable *)
  let compute_dir_name dir =
    let dirname = Filename2.normalize (Filename.concat dir dirname_user) in
(*    let dirname = Filename.concat dirname dirname_network in *)
    dirname
  in

  let checkdir =
    let module U = Unix.LargeFile in
    try
      List.find (fun d ->
        let dirname = compute_dir_name d.shdir_dirname in
(* check if temp_directory and incoming are on different partitions *)
        try
          if (U.stat dirname).U.st_dev <> (U.stat !!temp_directory).U.st_dev then
            begin
              match needed_space with
              | None -> true
              | Some needed_space ->
                  match Unix32.diskfree dirname with
                    Some v -> v >= needed_space
                  | _ -> true
            end
          else true
        with _ -> true
        ) directories
    with Not_found -> raise Incoming_full;
  in

  let newdir = {
      shdir_dirname = (compute_dir_name checkdir.shdir_dirname);
      shdir_priority = checkdir.shdir_priority;
      shdir_networks = checkdir.shdir_networks;
      shdir_strategy = checkdir.shdir_strategy;
    }
  in
  Unix2.safe_mkdir newdir.shdir_dirname;
  Unix2.can_write_to_directory newdir.shdir_dirname;
  newdir


let _ =
(* Check the definition of the incoming_files and incoming_directories in
shared_directories *)
  let verification = ref false in
  option_hook shared_directories (fun _ ->
      if not !verification then begin
          verification := true;
          ignore (incoming_dir false ());
          ignore (incoming_dir true ());
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
  shorten_all_file_filenames !!max_filenames;
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
  lprintf_nl (_b "Options correctly saved")
  end

let save_sources () =
  if !allow_saving_ini_files then begin
  networks_iter (fun n -> network_save_sources n);
  lprintf_nl (_b "Sources correctly saved")
  end

let backup_zip archive files =
  try 
    Unix2.tryopen_umask 0o066 (fun _old_umask ->
      Unix2.tryopen_write_zip archive (fun oc ->
        List.iter (fun file -> try
          let module U = Unix.LargeFile in
          let s = U.stat file in
          Zip.copy_file_to_entry file oc ~level:9 ~mtime:s.U.st_mtime file
        with e ->
          failwith (Printf.sprintf "Zip: error %s in %s" (Printexc2.to_string e) file)
      ) files))
  with e ->
    failwith (Printf.sprintf "Zip: error %s in %s" (Printexc2.to_string e) archive)

open Tar

let backup_tar archive files =
  let failed_files = ref [] in
  Unix2.tryopen_umask 0o066 (fun _old_umask ->
    Unix2.tryopen_write_tar ~compress:`Gzip archive (fun otar ->
      List.iter (fun arg -> try
        let header, s =
          Unix2.tryopen_read_bin arg (fun ic ->
            let stat = Unix.stat arg in
            let size = stat.Unix.st_size in
            if size > Sys.max_string_length then
              failwith (Printf.sprintf
                  "Tar: file %s too big, system limit %d byte, use .zip to avoid this limit"
                    arg Sys.max_string_length);
            let header = 
              { Tar.t_name = arg;
              t_mode = stat.Unix.st_perm;
              t_uid = stat.Unix.st_uid;
              t_gid = stat.Unix.st_gid;
              t_size = stat.Unix.st_size;
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
            let s = Bytes.create size in
            Pervasives.really_input ic s 0 size;
            header, Bytes.unsafe_to_string s) in
        Tar.output otar header s
      with
      | e ->
          failed_files := arg :: !failed_files;
          lprintf_nl "Tar: skipping %s, error %s" arg (Printexc2.to_string e)
      ) files
    )
  );
  if !failed_files <> [] then
    failwith (Printf.sprintf "Tar: skipped %s due to backup errors"
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
      let archive =
        Filename.concat "old_config" (backup_prefix ^ Date.reverse (Unix.time ()) ^ format)
      in
      let files =
        List.sort (fun o -> compare o) (List.filter (fun o ->
          String.lowercase (Filename2.last_extension o) = ".ini"
          && o <> "file_sources.ini")
            (Unix2.list_directory file_basedir))
      in
      begin
        match (Filename2.last_extension archive) with
          ".zip" -> backup_zip archive files
        | _ -> backup_tar archive files
      end
    with e -> lprintf_nl "Exception %s while options backup" (Printexc2.to_string e)
  end;
  lprintf_nl (_b "Options backup as %s correctly saved") format
             
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
  option_hook max_filenames (fun _ ->
    shorten_all_file_filenames !!max_filenames
  );

  option_hook max_upload_slots (fun _ ->
      if !!max_upload_slots < 3 then
        max_upload_slots =:= 3;
      networks_iter (fun n -> network_check_upload_slots n)
    );

  let max_opened_connections_pass = ref 0 in
  option_hook max_opened_connections (fun _ ->

  incr max_opened_connections_pass;

(* let users see if the option is called again *)
  let lprintf_nl s = lprintf_nl2
      (Printf.sprintf "%s pass %d:" log_prefix !max_opened_connections_pass) s in

  if !verbose then lprintf_nl
    "checking max_opened_connections = %d for validity" !!max_opened_connections;

(* maximum value of open sockets/files allowed *)
  let max_all_fds = Unix2.c_getdtablesize () in

(* ini files, dynamic libs, etc. *)
  let reserved_fds = max CommonOptions.min_reserved_fds (max_all_fds / 50) in

(* minimum number of max_opened_connections, p2p needs some sockets *)
  let min_conns = CommonOptions.min_connections in
(* max_conns *should* be greater than min_conns at that point, because of
   the sanity check at start time in CommonOptions;
   taking the max is just a safety belt from a paranoid :) *)
  let max_conns = max min_conns
    (max_all_fds - reserved_fds - Unix32.max_cache_size_default) in

  let print_stats verbose =
    if verbose then begin
      lprintf_nl "file descriptors status: total allowed (ulimit -n) %d" max_all_fds;
      lprintf_nl "- max_opened_connections %d (%d%% indirect)"
        !!max_opened_connections !!max_indirect_connections;
      lprintf_nl "- file cache size %d" (Unix32.get_max_cache_size ());
      lprintf_nl "- reserved %d" reserved_fds;
      let s,v =
        let v1 =
          max_all_fds - !!max_opened_connections - (Unix32.get_max_cache_size ()) - reserved_fds
        in
        if v1 >= 0 then "left", v1 else "missing", (abs v1)
      in
      lprintf_nl "= %d descriptors %s" v s
    end
  in

  if !!max_opened_connections < min_conns then begin
    lprintf_nl "max_opened_connections is set too low (%d), raising to %d"
      !!max_opened_connections min_conns;
    print_stats true;
    max_opened_connections =:= min_conns
  end
  else if !!max_opened_connections > max_conns then begin
    lprintf_nl "max_opened_connections is set too high (%d), lowering to %d"
      !!max_opened_connections max_conns;
    print_stats true;
    max_opened_connections =:= max_conns
  end
  else begin
    TcpBufferedSocket.set_max_opened_connections (fun _ -> !!max_opened_connections);

    let unused_fds = max_conns - !!max_opened_connections in

    Unix32.set_max_cache_size
      (Unix32.max_cache_size_default + unused_fds * 75 / 100);

    calc_real_max_indirect_connections ();

    print_stats !verbose
  end;

  if !verbose then lprintf_nl "checking max_opened_connections finished";
  decr max_opened_connections_pass
)

let _ =
  Heap.add_memstat "CommonComplexOptions" (fun level buf ->
      Printf.bprintf buf "  friends: %d\n" (List.length !!friends);
      Printf.bprintf buf "  contacts: %d\n" (List.length !contacts);
  )
