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

open Printf2
open BasicSocket
open Md4
open Options
  
open CommonClient
open CommonServer
open CommonComplexOptions
open CommonFile
open CommonTypes
open CommonOptions
open CommonGlobals
open CommonSwarming
    
open MultinetTypes
open MultinetFunctions
open MultinetComplexOptions

open DonkeyGlobals
open DonkeyTypes
open DonkeyOptions
  
let file_sources_ini = create_options_file (
    Filename.concat file_basedir "file_sources.ini")
  
let stats_ini = create_options_file (
    Filename.concat file_basedir "stats.ini")  

(* emulate emule onlinesig.dat 
 
 <connected (0|1)> | <server name> | <ip> | <port#>
 <downloadrate %.1f> | <uploadrate %.1f>| <queuesize int>
 
 I know this is stupid, but "give the people what they want"..
 
 *)

let create_online_sig () =
  
  let most_users = ref 0 in
  let server_name= ref "" in
  let server_ip = ref "" in
  let server_port = ref 0 in
  List.iter (fun s -> 
      if s.server_nusers > !most_users then begin 
          server_name := s.server_name;
          server_ip := (Ip.to_string s.server_ip);
          server_port := s.server_port;
          most_users := s.server_nusers;
        end
  ) (connected_servers());
  
  let oc = open_out (Filename.concat file_basedir "onlinesig.dat") in
  
  if !most_users = 0 then
    output_string oc ("0\n")
  else 
    output_string oc (Printf.sprintf "1|%s|%s|%d\n" !server_name !server_ip !server_port);
  let dlkbs = (( (float_of_int !udp_download_rate) +. (float_of_int !control_download_rate)) /. 1024.0) in
  let ulkbs = (( (float_of_int !udp_upload_rate) +. (float_of_int !control_upload_rate)) /. 1024.0) in
  
  output_string oc (Printf.sprintf "%.1f|%.1f|%d\n" dlkbs ulkbs 
      (Intmap.length !CommonUploads.pending_slots_map));
  close_out oc
  
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
      l.client_overnet <- get_value "client_overnet" value_to_bool
    with _ -> ());
  
  (try
      let last_conn =
        (min (get_value "client_age" value_to_int) 
          (BasicSocket.last_time ()))
      in
      let last_conn = normalize_time last_conn in
      CommonGlobals.connection_set_last_conn l.client_connection_control
        last_conn;
    with _ -> ());
  
(* Is it really useful ? I don't think so. It is only used when restarting
the client several times, which is not that current... 
  
  (try
     let last_filereqs =
       (min (get_value "client_last_filereqs" value_to_float)
	  (BasicSocket.last_time ())) in
       l.client_last_filereqs <- last_filereqs
   with _ -> ()); *)
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
      "client_age", int_to_value (
        CommonGlobals.connection_last_conn 
        c.client_connection_control);
      "client_last_filereqs", int_to_value c.client_last_filereqs;
      "client_overnet", bool_to_value c.client_overnet;
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
            (normalize_time (mini (get_value "server_age" value_to_int) 
            (BasicSocket.last_time ())));
        with _ -> ());
      as_server l.server_server

let server_to_value c =
  let fields = 
    [
    "server_addr", addr_to_value c.server_ip  c.server_port;
      "server_age", int_to_value (
      connection_last_conn c.server_connection_control);
  ]
  in
  
  let fields = 
    if c.server_description <> "" then 
      ("server_desc", string_to_value c.server_description) :: fields
    else fields in
  
  let fields = 
    if c.server_name <> "" then 
      ("server_name", string_to_value c.server_name) :: fields
    else fields in
  
  List.rev fields

let value_to_int32pair v =
  match v with
    List [v1;v2] | SmallList [v1;v2] ->
      (value_to_int64 v1, value_to_int64 v2)
  | _ -> 
      failwith "Options: Not an int32 pair"

let value_to_file file_shared assocs =
  let get_value name conv = conv (List.assoc name assocs) in
  let get_value_nil name conv = 
    try conv (List.assoc name assocs) with _ -> []
  in
  
  let file_md4_name = 
    try
      get_value "file_md4" value_to_string
    with _ -> failwith "Bad file_md4"
  in
  
  let file = Hashtbl.find files_by_md4  (Md4.of_string file_md4_name) 
  in
  
  (try
      Int64Swarmer.set_verified_bitmap file.file_partition
        (get_value  "file_all_chunks" value_to_string)
    with e -> 
        lprintf "Exception %s while loading bitmap\n"
          (Printexc2.to_string e); 
  );

  (*
  (try
      let mtime = Unix32.mtime64 (file_disk_name file)  in
      let old_mtime = value_to_float (List.assoc "file_mtime" assocs) in
      file.file_mtime <- old_mtime;
      let file_chunks = get_value "file_all_chunks" value_to_string in
      Int64Swarmer.set_verified_bitmap file.file_partition file_chunks
    with _ -> 
        lprintf "Could not load chunks states"; lprint_newline (););
  *)
  let md4s = get_value_nil "file_md4s" (value_to_list value_to_md4) in
  file.file_md4s <- (if md4s = [] then file.file_md4s else Array.of_list md4s);
  file_md4s_to_register := file :: !file_md4s_to_register
  

let file_to_value file =
  lprintf "file_to_value\n";
  [
    "file_md4", string_to_value (Md4.to_string file.file_md4);
    "file_all_chunks", string_to_value 
      (Int64Swarmer.verified_bitmap file.file_partition);  
    "file_md4s", List
      (List.map (fun s -> string_to_value (Md4.to_string s)) 
      (Array.to_list file.file_md4s));
  ]
  
  
module StatsOption = struct
    
    let value_to_stat v =
      match v with
        Options.Module assocs ->
          let get_value name conv = conv (List.assoc name assocs) in
          let get_value_nil name conv = 
            try conv (List.assoc name assocs) with _ -> []
          in
          { 
            brand_seen = value_to_int (List.assoc "seen" assocs);
            brand_banned = value_to_int (List.assoc "banned" assocs);
            brand_filerequest = value_to_int (List.assoc "filereqs" assocs);
            brand_download = value_to_int64 (List.assoc "download" assocs);
            brand_upload = value_to_int64 (List.assoc "upload" assocs);
          }
          
      | _ -> failwith "Options: not a stat option"
          
    let stat_to_value b =
      Options.Module [
        "seen", int_to_value b.brand_seen;
        "banned", int_to_value b.brand_banned;
        "filereqs", int_to_value b.brand_filerequest;
        "download", int64_to_value b.brand_download;
        "upload", int64_to_value b.brand_upload;
      ]
    
    
    let t = define_option_class "Stat" value_to_stat stat_to_value
  end

let value_to_module f v =
  match v with
    Module list -> f list
  | _ -> failwith "Option should be a module"
      
let save_time = define_header_option file_sources_ini 
    ["save_time"] "" int_option (last_time ())
      
module ClientOption = struct
    
    let value_to_source assocs = 
      let get_value names conv = conv (
          let rec iter names =
            match names with [] -> raise Not_found
            | name :: tail ->
                try
                  List.assoc name assocs
                with Not_found -> iter tail
          in
          iter names
        ) in
      let get_value_nil name conv = 
        try conv (List.assoc name assocs) with _ -> []
      in
      
      let addr = get_value ["addr"; "source_addr"] (fun v ->
            match v with
              List [ip;port] | SmallList [ip;port] ->
                let ip = Ip.of_string (value_to_string ip) in
                let port = value_to_int port in
                (ip, port)
            | _ -> failwith  "Options: Not an source option")
      in
      let files = get_value ["files"; "source_files"] (value_to_list 
            Md4.value_to_hash) in
      
      let overnet_source = try
          get_value ["overnet"; "source_overnet"] value_to_bool
        with _ -> false in
      
      let current_time = last_time () in
      
      let last_conn = 
        try
          min (get_value ["age"; "source_age"] value_to_int) current_time
        with _ -> current_time
      in
      
      let last_conn = normalize_time last_conn in

      let basic_score = try get_value ["score"] value_to_int with _ -> -10 in
      
      let rec iter files =
        match files with
          [] -> raise SideEffectOption
        | md4 :: tail ->
            try
              let file = find_file md4 in
              let s = DonkeySources.old_source basic_score last_conn addr file in
              s.source_overnet <- overnet_source;
              List.iter (fun md4 ->
                  try
                    let file = find_file md4 in
                    DonkeySources.add_source_request s file 0 File_expected;
                  with _ -> ()
              ) files;
              s
            with _ -> iter tail
      in 
      iter files
    
    let source_to_value s =
      let last_conn, basic_score = match s.source_client with
        | SourceLastConnection (basic_score, time, _) -> time, basic_score
        | SourceClient c -> last_time (), c.client_score
      in
      let (ip, port) = s.source_addr in
      
      let files = ref [] in
      let score = ref basic_score in
      List.iter (fun r ->
          if r.request_result > File_not_found then begin
              files := once_value (Md4.hash_to_value r.request_file.file_md4)
              :: !files;
              (*
              score := !score + (match r.request_result with
                | File_possible
                | File_not_found -> 0
                | File_expected -> 1
                | File_new_source -> 1
                | File_found -> 20
                | File_chunk -> 40
| File_upload -> 60)
  *)
            end
      ) s.source_files;
      
      let list = [
          "score", int_to_value !score;
          "addr", addr_to_value ip port;
          "files", smalllist_to_value "file list" 
            (fun s -> s)
          !files;
          
        ] in
      let list = 
        if last_conn < 1 then list else
          ("age", int_to_value s.source_age) :: list
      in
      let list = if s.source_overnet then
          ("overnet", bool_to_value true) :: list else list
      in
      Module list
      
    let t = define_option_class "Source" (value_to_module value_to_source)
      source_to_value
  end
    
      
(*  
let done_files = 
  define_option files_ini ["done_files"] 
  "The files whose download is finished" (list_option FileOption.t) []
    *)

let old_files = 
  define_expert_option donkey_section ["old_files"] 
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
  
(************  UPDATE OPTIONS *************)  
  
let add_server ip port =
  try
    DonkeyGlobals.find_server ip port
  with _ ->
      let s = DonkeyGlobals.new_server ip port !!initial_score in
      DonkeyGlobals.servers_ini_changed := true;
      s
  
let check_add_server ip port =
  if Ip.valid ip && not (is_black_address ip port) && port <> 4662 then
    add_server ip port
  else raise Not_found

let safe_add_server ip port =
  if Ip.valid ip && not (is_black_address ip port) && port <> 4662 then
    try
      ignore (DonkeyGlobals.find_server ip port)
    with _ ->
        let s = DonkeyGlobals.new_server ip port !!initial_score in
        DonkeyGlobals.servers_ini_changed := true
        
let remove_server ip port =
  try
    let s = DonkeyGlobals.find_server ip port in
    DonkeyGlobals.servers_ini_changed := true;
    DonkeyGlobals.remove_server ip port
  with _ -> ()

      
let file_sources_section = file_section file_sources_ini [] ""
let sources = define_option file_sources_section
    ["sources"] "" 
    (listiter_option ClientOption.t) []
  
      
let load _ =
  lprintf "LOADING SHARED FILES AND SOURCES"; lprint_newline ();
  (try
      Options.load stats_ini;
    with Sys_error _ -> ())
  
let stats_section = file_section stats_ini [] ""
  
let guptime = define_option stats_section ["guptime"] "" int_option 0
  
let new_stats_array () = 
  Array.init brand_count (fun _ ->
      { dummy_stats with brand_seen = 0 }
  )
  
let gstats_by_brand = define_option stats_section ["stats"] "" 
    (array_option StatsOption.t) (new_stats_array ())

let _ =
  option_hook gstats_by_brand (fun _ ->
      let old_stats = !!gstats_by_brand in
      let old_len = Array.length old_stats in
      if old_len <> brand_count then
        let t = new_stats_array () in
        for i = 0 to old_len - 1 do
          t.(i) <- old_stats.(i)
        done;
        gstats_by_brand =:= t
  )
  
let diff_time = ref 0

let save _ =
  lprintf "SAVING SHARED FILES AND SOURCES"; lprint_newline ();
  guptime =:= !!guptime + (last_time () - start_time) - !diff_time;
  diff_time := (last_time () - start_time);
  Options.save_with_help stats_ini;
  sources =:= [];
  DonkeySources.iter (fun s -> 
      (match s.source_client with
        SourceClient c -> s.source_files <- c.client_files;
      | _ -> ());      
      sources =:= s :: !!sources);
  
  save_time =:= last_time ();
  
  Options.save_with_help file_sources_ini;
  lprintf "SAVED"; lprint_newline ();
  sources =:= [];
  create_online_sig ()
    
let guptime () = !!guptime - !diff_time

  
let load_sources () = 
  (try 
      Options.load file_sources_ini;
    with _ -> ())

    

let old_value_to_file is_done assocs =
  let get_value name conv = conv (List.assoc name assocs) in
  let get_value_nil name conv = 
    try conv (List.assoc name assocs) with _ -> []
  in
  
  let file_md4_name = 
    try
      get_value "file_md4" value_to_string
    with _ -> failwith "Bad file_md4"
  in
  let filenames = get_value "file_filenames" (value_to_list value_to_string) in
  let ed2k = Md4.of_string file_md4_name in
  
  let file_size = try
      value_to_int64 (List.assoc "file_size" assocs) 
    with _ -> Int64.zero
  in
  
  let old_file = Filename.concat !!temp_directory file_md4_name in
  
  let file_shared = new_download None filenames
      file_size (Some old_file)
    [uid_of_uid (Ed2k ("", ed2k))] in
  
  let file_state = get_value "file_state" value_to_state in
  set_file_state file_shared file_state;
    
  
  (try 
      Int64Swarmer.set_absent file_shared.file_swarmer 
        (get_value "file_absent_chunks" 
          (value_to_list value_to_int32pair));
      add_file_downloaded file_shared.file_file
        (Int64Swarmer.downloaded file_shared.file_swarmer)      
    with _ -> ()                
  );

  let file = 
    let file = ref None in
    Hashtbl.iter (fun _ f ->
        if f.file_multinet == file_shared then 
          file := Some f
    ) files_by_md4;
    match !file with
      None -> raise Not_found
    | Some file -> file
  in
      
  let md4s = get_value_nil "file_md4s" (value_to_list value_to_md4) in
  file.file_md4s <- Array.of_list md4s;
  MultinetTypes.as_file file_shared
  
let _ =
  network_file_ops.op_download_of_value <- value_to_file; 
  file_ops.op_download_to_value <- file_to_value;
  network.op_network_file_of_option <- old_value_to_file;
  
  network.op_network_server_of_option <- value_to_server;
  server_ops.op_server_to_option <- server_to_value;
  
  network.op_network_client_of_option <- (fun is_friend l ->
      as_client (value_to_client is_friend l).client_client);
  client_ops.op_client_to_option <- client_to_value;
  
  network.op_network_load_complex_options <- load;
  network.op_network_save_complex_options <- save
  
