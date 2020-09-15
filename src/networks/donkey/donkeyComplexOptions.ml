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
  
open CommonSwarming  
open CommonClient
open CommonServer
open CommonFile
open CommonTypes
open CommonOptions
open CommonGlobals
open CommonResult
  
open DonkeyTypes
open DonkeyOptions
open DonkeyGlobals

module VB = VerificationBitmap

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

let create_online_sig () =
  
  let most_users = ref Int64.zero in
  let server_name= ref "" in
  let server_ip = ref "" in
  let server_port = ref 0 in
  List.iter (fun s -> 
      match s.server_nusers with
      | Some v when v > !most_users ->
          server_name := s.server_name;
          server_ip := (Ip.to_string s.server_ip);
          server_port := s.server_port;
          most_users := (match s.server_nusers with None -> 0L | Some v -> v)
      | _ -> ()
  ) (connected_servers());
  
  Unix2.tryopen_write "onlinesig.dat" (fun oc ->
  
  if !most_users = Int64.zero then
    output_string oc ("0\n")
  else 
    output_string oc (Printf.sprintf "1|%s|%s|%d\n" !server_name !server_ip !server_port);
  let dlkbs = (( (float_of_int !udp_download_rate) +. (float_of_int !control_download_rate)) /. 1024.0) in
  let ulkbs = (( (float_of_int !udp_upload_rate) +. (float_of_int !control_upload_rate)) /. 1024.0) in
  
  output_string oc (Printf.sprintf "%.1f|%.1f|%d\n" dlkbs ulkbs 
      (Intmap.length !CommonUploads.pending_slots_map)))
  
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
  let cc = Geoip.get_country_code_option ip in
  let l = DonkeyGlobals.new_client (Direct_address (ip,port)) cc in
  
  let md4 = try
      get_value "client_md4" value_to_md4 
    with _ -> Md4.null
  in
  let name = try
      get_value "client_name" value_to_string with _ -> "" in
  set_client_name l name md4;
  
  if is_friend then friend_add l;
  l
  
let value_to_donkey_client v = 
  match v with
    List [ip;port] | SmallList [ip;port] ->
      let ip = Ip.of_string (value_to_string ip) in
      let port = value_to_int port in
      let cc = Geoip.get_country_code_option ip in
      DonkeyGlobals.new_client (Direct_address (ip, port)) cc
  | Module assocs ->
      value_to_client false assocs 
  | _ -> assert false
      
let client_to_value c =
  let list = [
      "client_md4", string_to_value (Md4.to_string c.client_md4);
      "client_name", string_to_value c.client_name;
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
  let l = DonkeyGlobals.new_server ip port in
  
  (try
      l.server_name <- get_value "server_name" value_to_string
    with _ -> ());
  (try
      l.server_preferred <- get_value "server_preferred" value_to_bool
    with _ -> ());
  (try
      connection_set_last_conn l.server_connection_control
        (normalize_time (min (get_value "server_age" value_to_int) 
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

let value_to_file file_size file_state user group assocs =
  let get_value name conv = conv (List.assoc name assocs) in

  let file_md4 = 
    try
      get_value "file_md4" value_to_string
    with _ -> failwith "Bad file_md4"
  in
  let file_diskname, empty =
    let filename =
      try
        get_value "file_diskname" value_to_string
      with _ ->
        let filename = Filename.concat !!temp_directory file_md4 in
          lprintf_nl "getting file_diskname from ini failed, testing for ed2k-temp-file %s" filename;
        if Sys.file_exists filename then
          filename
        else
          begin
            let filename =
              Filename.concat
                !!temp_directory
                ( string_of_uid ( Ed2k (Md4.of_string file_md4) ) )
            in
            lprintf_nl "getting file_diskname from ini failed, testing for ed2k-temp-file %s"
              filename;
            if Sys.file_exists filename then
              filename
            else
              Filename.concat
                !!temp_directory
                ( file_string_of_uid ( Ed2k (Md4.of_string file_md4) ) )
          end
    in
    let file_exists filename = Sys.file_exists filename in
    if not (file_exists filename) then
      (* I think we should die here, to prevent any corruption. *)
      lprintf_nl "Error: temp file %s not found, re-creating empty one" filename;

    if !verbose && (not !CommonGlobals.is_startup_phase) then
      lprintf_nl "ed2k-temp-file %s used." filename;

    filename, not (file_exists filename)
  in

  let file = DonkeyGlobals.new_file file_diskname file_state
    (Md4.of_string file_md4) file_size "" true user group in

  (try
      set_file_best_name (as_file file)
      (get_value "file_filename" value_to_string) 0
    with _ -> update_best_name file);
  
  let md4s = try get_value "file_md4s" (value_to_array value_to_md4) 
    with _ -> [||] in
  
  if md4s <> [||] then
    begin
      if md4_of_array md4s <> (Md4.of_string file_md4) ||
        Array.length md4s <> file.file_nchunk_hashes then
        lprintf_nl "discarding partial chunks hashes, computed hash is wrong for %s"
          (file_best_name file)
      else
        file.file_computed_md4s <- md4s
    end;

  (match file.file_swarmer with
      None -> ()
    | Some swarmer ->
        CommonSwarming.value_to_frontend swarmer assocs;
        CommonSwarming.set_verifier swarmer (if md4s = [||] then
            VerificationNotAvailable
          else
            Verification
            (Array.map (fun md4 -> Ed2k md4) md4s));
        if empty then
          begin
            lprintf_nl "re-created missing temp file of %s , resetting chunk status to missing"
              (file_best_name file);
            let ver_str = String.make (Array.length md4s) (VB.state_to_char VB.State_missing) in
            CommonSwarming.set_chunks_verified_bitmap swarmer (VB.of_string ver_str);
          end
  );
  as_file file
  
let file_to_value file =
  let fields =
    [
      "file_md4", string_to_value (Md4.to_string file.file_md4);
      "file_diskname", string_to_value file.file_diskname;
      ("file_md4s", 
        array_to_value Md4.hash_to_value file.file_computed_md4s);
    ]
  in
  let fields = 
  match file.file_swarmer with
    None -> fields
    | Some swarmer ->
        CommonSwarming.frontend_to_value swarmer fields
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
      let s = DonkeyGlobals.new_server ip port in
      s        
      
let check_add_server ip port =
  if Ip.usable ip &&
    not (is_black_address ip port None) && port <> 4662 then
    force_add_server ip port
  else raise Not_found

let safe_add_server ip port =
  if Ip.usable ip &&
    not (is_black_address ip port None) && port <> 4662 then
    try
      ignore (DonkeyGlobals.find_server ip port)
    with _ ->
        ignore (DonkeyGlobals.new_server ip port)
        
let config_files_loaded = ref false  
      
let load _ =
  if !verbose then lprintf_nl "Loading shared files";
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
  if !sources_loaded && !!keep_sources then begin
      save_time =:= last_time ();
      let cleaner = DonkeySources.attach_sources_to_file file_sources_section in
      Options.save_with_help file_sources_ini;
      cleaner ()
    end

let guptime () = !!guptime - !diff_time
  
let load_sources () =
  if not !!keep_sources then () else
  (try 
      let cleaner = DonkeySources.attach_sources_to_file file_sources_section in
      cleaner ();
      Options.load file_sources_ini;
      cleaner ();
      sources_loaded := true;
      lprintf_nl "loading sources completed"
    with _ -> ())

let check_result r tags =
  if r.result_names = [] || r.result_size = Int64.zero then begin
      if !verbose then begin
          lprintf_n "Bad search result: ";
          List.iter (fun tag ->
              lprintf "[%s] = [%s] " (string_of_field tag.tag_name)
                (string_of_tag_value tag.tag_value)
          ) tags;
          lprint_newline ();
        end;
      false
    end
  else true

(* Inserted in complexOptions to be able to access old_files (ugly) *)
let result_of_file md4 tags =
  let r = {  dummy_result with
      result_uids = [Uid.create (Ed2k md4)];
      result_done = (List.mem md4 !!old_files) || (Hashtbl.mem files_by_md4 md4); 
      result_source_network = network.network_num;
    } in
  List.iter (fun tag ->
      match tag with
        { tag_name = Field_Filename; tag_value = String s } ->
          r.result_names <- s :: r.result_names
      | { tag_name = Field_Size; tag_value = (Uint64 v| Fint64 v) } ->
          r.result_size <- v;
      | { tag_name = Field_Size; tag_value = (Uint16 v| Uint8 v) } ->
          r.result_size <- Int64.of_int v;
      | { tag_name = Field_Size_Hi; tag_value = Uint8 v } ->
          r.result_size <- Int64.logor r.result_size (Int64.shift_left (Int64.of_int v) 32);
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
