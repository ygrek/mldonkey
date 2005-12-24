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
  
open CommonDownloads
open CommonSwarming  
open CommonClient
open CommonServer
open CommonComplexOptions
open CommonFile
open CommonTypes
open CommonOptions
open CommonGlobals
open CommonResult
  
open DonkeyTypes
open DonkeyOptions
open DonkeyGlobals

let shared_files_ini = create_options_file "shared_files_new.ini"

let file_sources_ini = create_options_file "file_sources.ini"
  
let stats_ini = create_options_file "stats.ini"

let mod_stats_ini = create_options_file "stats_mod.ini"

let shared_section = file_section shared_files_ini [] ""
let stats_section = file_section stats_ini [] ""
let mod_stats_section = file_section mod_stats_ini [] ""
let file_sources_section = file_section file_sources_ini [] ""
  


(* emulate emule onlinesig.dat 

<connected (0|1)> | <server name> | <ip> | <port#>
<downloadrate %.1f> | <uploadrate %.1f>| <queuesize int>

I know this is stupid, but "give the people what they want"..

*)

let lprintf_nl () =
  lprintf "%s[EDK] "
  (log_time ()); lprintf_nl2

let create_online_sig () =
  
  let most_users = ref Int64.zero in
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
  
  let oc = open_out "onlinesig.dat" in
  
  if !most_users = Int64.zero then
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
  let (ip,port) = 
    try
      get_value "client_addr" (fun v ->
          match v with
            List [ip;port] | SmallList [ip;port] ->
              let ip = Ip.of_string (value_to_string ip) in
              let port = value_to_int port in
              (ip, port)
          | _ -> failwith  "Options: Not an client option"
      ) 
    with _ -> 
        failwith "Source without address: removed"
(*
        let md4 = try
            get_value "client_md4" value_to_md4 with _ -> Md4.null
        in
        let name = try
            get_value "client_name" value_to_string with _ -> "" in
        Indirect_location(name, md4) *)
  in
  
  let _ = 
    try
      let last_conn =
        (min (get_value "client_age" value_to_int) 
          (BasicSocket.last_time ()))
      in
      let last_conn = normalize_time last_conn in
      last_conn
    with _ -> 0 in
  let l = DonkeyGlobals.new_client (Direct_address (ip,port)) in
  
  let md4 = try
      get_value "client_md4" value_to_md4 
    with _ -> Md4.null
  in
  let name = try
      get_value "client_name" value_to_string with _ -> "" in
  set_client_name l name md4;
  
  (*
  CommonGlobals.connection_set_last_conn l.client_connection_control
    last_conn;
*)  
    
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
      DonkeyGlobals.new_client (Direct_address (ip, port))
  | Module assocs ->
      value_to_client false assocs 
  | _ -> assert false
      
let client_to_value c =
  let list = [
      "client_md4", string_to_value (Md4.to_string c.client_md4);
      "client_name", string_to_value c.client_name;
(*      "client_age", int_to_value (
        CommonGlobals.connection_last_conn 
        c.client_connection_control); *)
(*      "client_last_filereqs", int_to_value c.client_last_filereqs; *)
    ]
  in
  
  match c.client_kind with
    Direct_address (ip, port) ->   
      ("client_addr", addr_to_value ip  port) :: list
  | _ -> raise Exit (* Don't save these options *)
      
let donkey_client_to_value c =
  Options.Module (client_to_value c)  

let client_option =
  define_option_class "Client" value_to_donkey_client
  donkey_client_to_value 

let value_to_server assocs = 
  let get_value name conv = conv (List.assoc name assocs) in
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
      l.server_preferred <- get_value "server_preferred" value_to_bool
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
  
  let fields = 
    if c.server_preferred then
      ("server_preferred", bool_to_value true) :: fields else
      fields in
  List.rev fields

let value_to_int32pair v =
  match v with
    List [v1;v2] | SmallList [v1;v2] ->
      (value_to_int64 v1, value_to_int64 v2)
  | _ -> 
      failwith "Options: Not an int32 pair"

let value_to_file file_size file_state assocs =
  let get_value name conv = conv (List.assoc name assocs) in
  let get_value_nil name conv = 
    try conv (List.assoc name assocs) with _ -> []
  in
  
  let file_md4 = 
    try
      get_value "file_md4" value_to_string
    with _ -> failwith "Bad file_md4"
  in
  let file_diskname =
    let filename =
      try
        get_value "file_diskname" value_to_string
      with _ ->
        let filename = Filename.concat !!temp_directory file_md4 in
          lprintf_nl () "getting file_diskname from ini failed, testing for ed2k-temp-file %s" filename;
        if Sys.file_exists filename then
          filename
        else
          begin
            let filename =
              Filename.concat
                !!temp_directory
                ( string_of_uid ( Ed2k (Md4.of_string file_md4) ) )
            in
            lprintf_nl () "geting file_diskname from ini failed, testing for ed2k-temp-file %s"
              filename;
            if Sys.file_exists filename then
              filename
            else
              Filename.concat
                !!temp_directory
                ( file_string_of_uid ( Ed2k (Md4.of_string file_md4) ) )
          end
    in
    if not (Sys.file_exists filename) then
      (* I think we should die here, to prevent any corruption. *)
      lprintf_nl () "ERROR ED2K-TEMP-FILE %s DOES NOT EXIST, THIS WILL PERHAPS LEAD TO CORRUPTION IN THAT DOWNLOAD!"
        filename;
    if !verbose then lprintf_nl () "ed2k-temp-file %s used." filename;
    filename
  in

  let filenames = List.map (fun name -> name, GuiTypes.noips()) 
    (get_value_nil "file_filenames" (value_to_list value_to_string)) in
  
  let file = DonkeyGlobals.new_file file_diskname file_state
    (Md4.of_string file_md4) file_size filenames true in

  (*
  (try 
      if file.file_exists then begin
(* only load absent chunks if file previously existed. *)
          file.file_absent_chunks <- 
            get_value "file_absent_chunks" 
            (value_to_list value_to_int32pair);
        end
    with _ -> ()                );
*)
  
  
  
  (try
      set_file_best_name (as_file file)
      (get_value "file_filename" value_to_string)
    with _ -> update_best_name file);
  
(*
  (try
      let mtime = Unix32.mtime (file_disk_name file)  in
      let old_mtime = value_to_float (List.assoc "file_mtime" assocs) in
      file.file_mtime <- old_mtime;
      let file_chunks = get_value "file_all_chunks" value_to_string in
      file.file_chunks <- Array.init file.file_nchunks 
        (fun i ->
          let c = file_chunks.[i] in
          if c = '0' then AbsentVerified else
          if c = '2' then PresentVerified
          else PresentTemp
      )
    with _ -> 
        lprintf "Could not load chunks states\n"; );
*)
  let md4s = try get_value "file_md4s" (value_to_array value_to_md4) 
    with _ -> [||] in
  
  if md4s <> [||] then file.file_computed_md4s <- md4s;
  
  (match file.file_swarmer with
      None -> ()
    | Some swarmer ->
        Int64Swarmer.value_to_swarmer swarmer assocs;
        Int64Swarmer.set_verifier swarmer (if md4s = [||] then
            VerificationNotAvailable
          else
            Verification
            (Array.map (fun md4 -> Ed2k md4) md4s))
  );
  
(*
  List.iter (fun c ->
      DonkeyGlobals.new_source file  c;
  )
   (get_value_nil "file_locations" (value_to_list value_to_donkey_client)); *)
  
(*
  (try
      file.file_chunks_age <-
        get_value "file_chunks_age" 
        (fun v -> 
          let list = value_to_list (fun v -> normalize_time (value_to_int v)) v in
          Array.of_list list)
    with _ -> ());
  
  file.file_file.impl_file_last_seen <- (
    if file.file_chunks_age = [||]
    then 0
    else Array2.min file.file_chunks_age);
*)
  
  as_file file
  
(*  
let string_of_chunks file =
  let nchunks = file.file_nchunks in
  let s = String.make nchunks '0' in
  for i = 0 to nchunks - 1 do
    s.[i] <- (match file.file_chunks.(i) with
      | PresentVerified -> '2'
      | AbsentVerified 
      | PartialVerified _ -> '0'
      | _ -> '1' (* don't know ? *)
    )
  done;
  s
    *)

let file_to_value file =
  let fields =
    [
      "file_md4", string_to_value (Md4.to_string file.file_md4);
      "file_diskname", string_to_value file.file_diskname;
(*      "file_all_chunks", string_to_value (string_of_chunks file);   *)
(*      "file_absent_chunks", List
        (List.map (fun (i1,i2) -> 
            SmallList [int64_to_value i1; int64_to_value i2])
        file.file_absent_chunks); *)
      ("file_md4s", 
        array_to_value Md4.hash_to_value file.file_computed_md4s);
      "file_filenames", List
        (List.map (fun (s,_) -> string_to_value s) file.file_filenames);
(*      "file_mtime", float_to_value (
        try Unix32.mtime (file_disk_name file) with _ -> 0.0) *)
    ]
  in
  let fields = 
  match file.file_swarmer with
    None -> fields
    | Some swarmer ->
        Int64Swarmer.swarmer_to_value swarmer fields
  in
  fields
    
module SharedFileOption = struct
    
    let value_to_shinfo v =
      match v with
        Options.Module assocs ->
          let sh_md4s = try
              value_to_array (fun v ->
                  Md4.of_string (value_to_string v)) (List.assoc "md4s" assocs)
            with _ -> failwith "Bad shared file md4"
          in
          let sh_size = try
              value_to_int64 (List.assoc "size" assocs) 
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
        "md4s", array_to_value Md4.hash_to_value sh.sh_md4s;
        "mtime", float_to_value sh.sh_mtime;
        "size", int64_to_value sh.sh_size;
      ]
    
    
    let t = define_option_class "SharedFile" value_to_shinfo shinfo_to_value
  end
  
module StatsOption = struct
    
    let value_to_stat v =
      match v with
        Options.Module assocs ->
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

module ModStatsOption = struct
    
    let value_to_mod_stat v =
      match v with
        Options.Module assocs ->
          { 
            brand_seen = value_to_int (List.assoc "mseen" assocs);
            brand_banned = value_to_int (List.assoc "mbanned" assocs);
            brand_filerequest = value_to_int (List.assoc "mfilereqs" assocs);
            brand_download = value_to_int64 (List.assoc "mdownload" assocs);
            brand_upload = value_to_int64 (List.assoc "mupload" assocs);
          }
          
      | _ -> failwith "Options: not a mod_stat option"
          
    let stat_to_mod_value b =
      Options.Module [
        "mseen", int_to_value b.brand_seen;
        "mbanned", int_to_value b.brand_banned;
        "mfilereqs", int_to_value b.brand_filerequest;
        "mdownload", int64_to_value b.brand_download;
        "mupload", int64_to_value b.brand_upload;
      ]
    
    
    let t = define_option_class "ModStat" value_to_mod_stat stat_to_mod_value
  end

let value_to_module f v =
  match v with
    Module list -> f list
  | _ -> failwith "Option should be a module"
      
let save_time = define_header_option file_sources_ini 
    ["save_time"] "" int_option (last_time ())
    
      
(*  
let done_files = 
  define_option files_ini ["done_files"] 
  "The files whose download is finished" (list_option FileOption.t) []
    *)

module UidOption = struct
    
    let value_to_hash v = 
      try
        let uid = Uid.of_string (value_to_string v) in
        ignore (Uid.to_uid uid);
        uid
      with _ -> Uid.create (Ed2k (Md4.value_to_hash v))
            
    let hash_to_value v = string_to_value (Uid.to_string v)
      
    let t =
      define_option_class "Uid" value_to_hash hash_to_value
    
  end

let brotherhood = 
  define_option donkey_section ["brotherhood"] 
    "The links between files being downloaded" (list_option 
      (list_option Md4.option)) []

  (*
let files = 
  define_option files_ini ["files"] 
  "The files currently being downloaded" (list_option FileOption.t) []
*)

(*
let known_servers = define_option servers_ini["known_servers"] "List of known servers"
    (list_option ServerOption.t) []
    *)

let known_shared_files = define_option shared_section 
    ["shared_files"] "" 
    (list_option SharedFileOption.t) []
  
(************  UPDATE OPTIONS *************)  

(* This function is used only with the "n" command and the 
op_network_add_server method, which are the only ways for the user
to enter servers, bypassing the update_server_list variable. *)
  
let force_add_server ip port =
  try
    DonkeyGlobals.find_server ip port
  with _ ->
      let s = DonkeyGlobals.new_server ip port !!initial_score in
      s        
      
let check_add_server ip port =
  if Ip.usable ip &&
    not (is_black_address ip port) && port <> 4662 then
    force_add_server ip port
  else raise Not_found

let safe_add_server ip port =
  if Ip.usable ip &&
    not (is_black_address ip port) && port <> 4662 then
    try
      ignore (DonkeyGlobals.find_server ip port)
    with _ ->
        ignore (DonkeyGlobals.new_server ip port !!initial_score)
        
let remove_server ip port =
  try
    let _ = DonkeyGlobals.find_server ip port in
    DonkeyGlobals.remove_server ip port
  with _ -> ()

let config_files_loaded = ref false  
      
let load _ =
  if !verbose then lprintf_nl () "Loading shared files";
  (try
      Options.load shared_files_ini;
      Options.load stats_ini;
      Options.load mod_stats_ini;
    with Sys_error _ ->
        Options.save_with_help shared_files_ini);
  config_files_loaded := true
  
let guptime = define_option stats_section ["guptime"] "" int_option 0
  
let new_stats_array () = 
  Array.init brand_count (fun _ ->
      { dummy_stats with brand_seen = 0 }
  )
  
let new_mod_stats_array () = 
  Array.init brand_mod_count (fun _ ->
      { dummy_stats with brand_seen = 0 }
  )

let gstats_array = define_option stats_section ["stats"] "" 
    (array_option StatsOption.t) (new_stats_array ())

let gstats_mod_array = define_option mod_stats_section ["stats"] "" 
    (array_option ModStatsOption.t) (new_mod_stats_array ())

let _ =
  option_hook gstats_array (fun _ ->
      let old_stats = !!gstats_array in
      let old_len = Array.length old_stats in
      if old_len <> brand_count then
        let t = new_stats_array () in
        for i = 0 to old_len - 1 do
          t.(i) <- old_stats.(i)
        done;
        gstats_array =:= t
  )
  
let _ =
  option_hook gstats_mod_array (fun _ ->
      let old_mod_stats = !!gstats_mod_array in
      let old_mod_len = Array.length old_mod_stats in
      if old_mod_len <> brand_mod_count then
        let t = new_mod_stats_array () in
        for i = 0 to old_mod_len - 1 do
          t.(i) <- old_mod_stats.(i)
        done;
        gstats_mod_array =:= t
  )

let diff_time = ref 0

let sources_loaded = ref false  (* added 2.5.24 *)

let save _ =
  if !config_files_loaded then begin
      Options.save_with_help shared_files_ini;
      guptime =:= !!guptime + (last_time () - start_time) - !diff_time;
      diff_time := (last_time () - start_time);
      Options.save_with_help stats_ini;
      Options.save_with_help mod_stats_ini;
      create_online_sig ()
    end

let save_sources _ =
  if !sources_loaded then begin
      save_time =:= last_time ();
      let cleaner = DonkeySources.attach_sources_to_file file_sources_section in
      Options.save_with_help file_sources_ini;
      cleaner ()
    end

let guptime () = !!guptime - !diff_time
  
let load_sources () =
  (lprintf "(loading sources ";
   try 
      let cleaner = DonkeySources.attach_sources_to_file file_sources_section in
      cleaner ();
      lprintf ".";
      Options.load file_sources_ini;
      lprintf ".";
      cleaner ();
      lprintf ".";
      sources_loaded := true;
      lprintf " completed) ";
      List.iter (fun list ->
          let files = ref [] in
          List.iter (fun m ->
              try
                let file = find_file m in
                files := file.file_sources :: !files
              with _ -> ()
          ) list;
          match !files with
            [] | [_] -> ()
          | files ->
              DonkeySources.set_brothers files
      ) !!brotherhood
    with _ -> ())

let check_result r tags =
  if r.result_names = [] || r.result_size = Int64.zero then begin
      if !verbose then begin
          lprintf_n () "BAD RESULT:";
          List.iter (fun tag ->
              lprintf "[%s] = [%s]" (string_of_field tag.tag_name)
                (string_of_tag_value tag.tag_value);
              lprint_newline ();
          ) tags;
        end;
      false
    end
  else true

(* Inserted in complexOptions to be able to access old_files (ugly) *)
let result_of_file md4 tags =
  let rec r = {  dummy_result with
      result_uids = [Uid.create (Ed2k md4)];
      result_done = (List.mem md4 !!old_files) || (Hashtbl.mem files_by_md4 md4); 
    } in
  List.iter (fun tag ->
      match tag with
        { tag_name = Field_Filename; tag_value = String s } ->
          r.result_names <- s :: r.result_names
      | { tag_name = Field_Size; tag_value = Uint64 v } ->
          r.result_size <- v;
      | { tag_name = Field_Format; tag_value = String s } ->
          r.result_tags <- tag :: r.result_tags;
          r.result_format <- s
      | { tag_name = Field_Type; tag_value = String s } ->
          r.result_tags <- tag :: r.result_tags;
          r.result_type <- s
      | _ ->
          r.result_tags <- tag :: r.result_tags
  ) tags;
  if check_result r tags then
    let rs = update_result_num r in
    Some rs
  else None
    
  
let _ =
  network.op_network_file_of_option <- value_to_file;
  file_ops.op_file_to_option <- file_to_value;
  
  network.op_network_server_of_option <- value_to_server;
  server_ops.op_server_to_option <- server_to_value;
  
  network.op_network_client_of_option <- (fun is_friend l ->
      as_client (value_to_client is_friend l));
  client_ops.op_client_to_option <- client_to_value;
  
  network.op_network_load_complex_options <- load;
  network.op_network_save_complex_options <- save;
  network.op_network_save_sources <- save_sources;
  
  let cleaner = DonkeySources.attach_sources_to_file file_sources_section in
  cleaner ()
