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
open Printf2
open Md4
open Options

open BasicSocket
open TcpBufferedSocket

open GuiTypes

open CommonShared
open CommonServer
open CommonResult
open CommonClient
open CommonUser
open CommonNetwork
open CommonTypes
open CommonFile

open DonkeyProtoCom
open DonkeyServers
open DonkeyOneFile
open DonkeyComplexOptions
open DonkeyTypes
open DonkeyOptions
open DonkeyGlobals
open DonkeyClient
open CommonGlobals
open CommonOptions
open DonkeyUdp

open Gettext

let _s x = _s "DonkeyInteractive" x
let _b x = _b "DonkeyInteractive" x

let porttest_result = ref PorttestNotStarted

module VB = VerificationBitmap

let log_prefix = "[EDK]"

let lprintf_nl fmt =
  lprintf_nl2 log_prefix fmt

let result_name r =
  match r.result_names with
    [] -> None
  | name :: _ -> Some name


let op_file_proposed_filenames file =
  file.file_file.impl_file_filenames

let reconnect_all file =
  DonkeyProtoOvernet.Overnet.recover_file file;
  DonkeyProtoKademlia.Kademlia.recover_file file;

(* This is expensive, no ? *)
(*  DonkeySources.reschedule_sources file; *)
  List.iter (fun s ->
      match s.server_sock, server_state s with
      | Connection sock, (Connected _ | Connected_downloading _) ->
          s.server_waiting_queries <- file :: s.server_waiting_queries
      | _ -> ()
  ) (connected_servers())

let forget_search s =
  if !xs_last_search = s.search_num then begin
      xs_last_search := (-1);
      xs_servers_list := [];
    end;
  DonkeyProtoKademlia.Kademlia.forget_search s;
  DonkeyProtoOvernet.Overnet.forget_search s


let load_server_met filename =
  if !!update_server_list_server_met then
  try
    let module S = DonkeyImport.Server in
    let s = File.to_string filename in
    let ss = S.read s in
    List.iter (fun r ->
        try
          let server = check_add_server r.S.ip r.S.port in
          List.iter (fun tag ->
              match tag with
              |  { tag_name = Field_KNOWN "name"; tag_value = String s } ->
                  server.server_name <- s;
              |  { tag_name = Field_KNOWN "description" ; tag_value = String s } ->
                  server.server_description <- s
              |  { tag_name = Field_KNOWN "version" ; tag_value = Uint64 s } ->
                  server.server_version <- Printf.sprintf "%d.%d"
                                            ((Int64.to_int s) lsr 16) ((Int64.to_int s) land 0xFFFF)
              |  { tag_name = Field_KNOWN "ping" ; tag_value = Uint64 s } ->
                  server.server_ping <- (Int64.to_int s)
              |  { tag_name = Field_KNOWN "dynip" ; tag_value = String s } ->
                  server.server_dynip <- s
              |  { tag_name = Field_KNOWN "users" ; tag_value = Uint64 s } ->
                  (match server.server_nusers with
                  | None -> server.server_nusers <- Some s | _ -> ())
              |  { tag_name = Field_KNOWN "files" ; tag_value = Uint64 s } ->
                  (match server.server_nfiles with
                  | None -> server.server_nfiles <- Some s | _ -> ())
              |  { tag_name = Field_KNOWN "maxusers" ; tag_value = Uint64 s } ->
                  (match server.server_max_users with
                  | None -> server.server_max_users <- Some s | _ -> ())
              |  { tag_name = Field_KNOWN "softfiles" ; tag_value = Uint64 s } ->
                  (match server.server_soft_limit with
                  | None -> server.server_soft_limit <- Some s | _ -> ())
              |  { tag_name = Field_KNOWN "hardfiles" ; tag_value = Uint64 s } ->
                  (match server.server_hard_limit with
                  | None -> server.server_hard_limit <- Some s | _ -> ())
              |  { tag_name = Field_KNOWN "auxportslist" ; tag_value = String s } ->
                  server.server_auxportslist <- s
              |  { tag_name = Field_KNOWN "lowusers" ; tag_value = Uint64 s } ->
                  (match server.server_lowid_users with
                  | None -> server.server_lowid_users <- Some s | _ -> ())
              |  { tag_name = Field_KNOWN "tcpportobfuscation" ; tag_value = Uint64 s } ->
                  server.server_obfuscation_tcp <- Some (Int64.to_int s)
              |  { tag_name = Field_KNOWN "udpportobfuscation" ; tag_value = Uint64 s } ->
                  server.server_obfuscation_udp <- Some (Int64.to_int s)
              |  { tag_name = Field_KNOWN "country" ; tag_value = String s } -> ()
              |  { tag_name = Field_KNOWN "udpflags" ; tag_value = Uint64  s } -> ()
              |  { tag_name = Field_KNOWN "refs" ; tag_value = Uint64  s } -> ()
              | _ -> lprintf_nl "parsing server.met, unknown field %s" (string_of_tag tag)
          ) r.S.tags;
          server_must_update server
        with _ -> ()
    ) ss;
    List.length ss
  with e ->
      lprintf_nl "Exception %s while loading %s" (Printexc2.to_string e)
      filename;
      0
  else 0

let unpack_server_met filename url =
  let ext = String.lowercase (Filename2.extension filename) in
    let last_ext = String.lowercase (Filename2.last_extension filename) in
    let real_ext = if last_ext = ".zip" then last_ext else ext in
    match real_ext with
      | ".zip" ->
          (try
            let result =
              Unix2.tryopen_read_zip filename (fun ic ->
                try
                  let file = Zip.find_entry ic "server.met" in
                    lprintf_nl (_b "server.met found in %s") url;
                  file.Zip.filename
                with e ->
                    lprintf_nl (_b "Exception %s while extracting server.met from %s")
                    (Printexc2.to_string e) url;
                  raise e) in
            (try
              ignore(Misc.archive_extract filename "zip")
            with e ->
                lprintf_nl (_b "Exception %s while extracting server.met from %s")
                (Printexc2.to_string e) url;
              raise e);
            result
          with
          | Zip.Error _ -> filename
          | e ->
            lprintf_nl "Exception %s while opening %s"
              (Printexc2.to_string e) url;
            raise Not_found)
    | ".met.gz" | ".met.bz2" | ".gz" | ".bz2" ->
        (let filetype =
          if ext = ".bz2" || ext = ".met.bz2" then "bz2" else "gz" in 
        try
          Misc.archive_extract filename filetype
        with
        | Gzip.Error _ -> filename
        | e ->
          lprintf_nl "Exception %s while extracting from %s"
            (Printexc2.to_string e) url;
          raise Not_found)
(* if file is not a supported archive type try loading servers from that file anyway *)
    | _ -> filename

let download_server_met url =
  let module H = Http_client in
    let r = {
      H.basic_request with
      H.req_url = Url.of_string url;
      H.req_proxy = !CommonOptions.http_proxy;
      H.req_user_agent = get_user_agent ();
      H.req_max_retry = 10;
      H.req_save = true;
    } in
    H.wget r (fun filename ->
      try
        let nservers = List.length (Hashtbl2.to_list servers_by_key) in
        let s = unpack_server_met filename url in
    let n = load_server_met s in
      if s <> filename then Sys.remove s;
            lprintf_nl (_b "server.met loaded from %s, %d servers found, %d new ones inserted")
        url n ((List.length (Hashtbl2.to_list servers_by_key)) - nservers)
      with e -> ()
    )

let already_done = Failure (Printf.sprintf (_b "File already downloaded (use 'force_download' if necessary)"))
let no_download_to_force = Failure (Printf.sprintf (_b "No forceable download found"))
exception Already_downloading of string
exception Already_shared of string

let really_query_download filename size md4 location old_file absents user group =

  begin
    try
      let file = Hashtbl.find files_by_md4 md4 in
      if file_state file = FileDownloaded then
        raise already_done;
    with Not_found -> ()
  end;

  List.iter (fun file ->
      if file.file_md4 = md4 then raise already_done)
  !current_files;

  let file_diskname = Filename.concat !!temp_directory
    (file_string_of_uid (Ed2k md4)) in
  begin
    match old_file with
    | Some filename when file_diskname <> filename ->
        if Sys.file_exists filename
           && not (Sys.file_exists file_diskname)
          then
            (try
              lprintf_nl "Renaming edonkey temp-file from %s to %s"
                  filename file_diskname;
              Unix2.rename filename file_diskname;
              Unix2.chmod file_diskname 0o644;
              with e ->
                lprintf_nl "Could not rename %s to %s: exception %s"
                  filename file_diskname (Printexc2.to_string e);
            )
        else lprintf_nl "THERE IS SOME PROBLEM WITH RECOVERING TEMP-FILES, THAT COULD CAUSE FILE-CORRUPTION!!!!!!!!!!! filename: %s  exists:%b file_diskname: %s  exists:%b"
               filename (Sys.file_exists filename)
               file_diskname (Sys.file_exists file_diskname);
    | _ -> ()
  end;

(* TODO RESULT  let other_names = DonkeyIndexer.find_names md4 in *)
  let file = new_file file_diskname FileDownloading md4 size filename true user group in
  begin
    match absents with
      None -> ()
    | Some absents ->
        match file.file_swarmer with
          None -> assert false
        | Some swarmer ->
            let absents = 
              List.sort (fun (p1, _) (p2, _) -> compare p1 p2) absents in
            CommonSwarming.set_absent swarmer absents
  end;

  if !verbose then lprintf_nl (_b "Started new download, file %s, size %Ld, md4 %s, user:group %s:%s")
    (file_best_name file) size (Md4.to_string md4) user.CommonTypes.user_name (CommonUserDb.user2_print_group group);

  DonkeyProtoOvernet.Overnet.recover_file file;
  DonkeyProtoKademlia.Kademlia.recover_file file;

  current_files := file :: !current_files;
(*  !file_change_hook file; *)
(*  set_file_size file (file_size file); *)

  List.iter (fun s ->
      add_query_location file s
  ) (connected_servers());

  (try
      let servers = Hashtbl.find_all udp_servers_replies file.file_md4 in
      List.iter (fun s ->
          udp_server_send_query_location s [(file.file_md4,(file_size file))]
      ) servers
    with _ -> ());

  (match location with
      None -> ()
    | Some num ->
        let c = client_find num in
        client_connect c
        (*
        try
          let c = find_client num in
          (match c.client_kind with
              Indirect_location ->
                if not (Intmap.mem c.client_num file.file_indirect_locations) then
                  file.file_indirect_locations <- Intmap.add c.client_num c
                    file.file_indirect_locations

            | _ ->
                if not (Intmap.mem c.client_num file.file_known_locations) then
                  new_known_location file c
          );
          if not (List.memq file c.client_files) then
            c.client_files <- file :: c.client_files;
          match client_state c with
            NotConnected ->
              connect_client !!client_ip [file] c
          | Connected_busy | Connected_idle | Connected_queued ->
              begin
                match c.client_sock with
                  None -> ()
                | Some sock ->
                    DonkeyClient.query_files c sock [file]
              end
          | _ -> ()
with _ -> ()
*)
  );
  as_file file

let query_download filename size md4 location old_file absents force user group =
  if force then
    if !forceable_download = [] then
      raise no_download_to_force
    else
      begin
        let f = List.hd !forceable_download in
          forceable_download := [];
          really_query_download (List.hd f.result_names) f.result_size md4 None None None user group
      end
  else
    begin
      try
        let file = find_file md4 in
          if (file_state file) = FileShared then
            raise (Already_shared (Printf.sprintf (_b "File is already shared%s")
              (match file.file_shared with
                 None -> ""
               | Some sh -> (" in " ^ (Filename2.dirname sh.impl_shared_fullname)))))
          else
            begin
(* jave TODO: if a user currently not downloading this file is requesting the download add this user
   to the list of users currently downloading this file *)
              forceable_download := [];
              raise (Already_downloading (Printf.sprintf (_b "File is already in download queue of %s") (file_owner (as_file file)).CommonTypes.user_name))
            end
      with Not_found ->
        begin
        if List.mem md4 !!old_files then begin
          (* copy file info into result for later usage in force_download *)
          let r = { dummy_result with
              result_uids = [Uid.create (Ed2k md4)];
              result_names = [filename];
              result_size = size;
              result_force = true; (* marker for force_download *)
              result_modified = false;
                result_source_network = network.network_num;
            } in
            forceable_download := [r];
            raise already_done
          end
        else
          begin
            forceable_download := [];
            really_query_download filename size md4 location old_file absents user group
          end
        end
    end

let result_download r filenames force user group =
  let rec iter uids =
    match uids with
      [] -> raise IgnoreNetwork
    | uid :: tail ->
        match Uid.to_uid uid with
          Ed2k md4 ->
            query_download (List.hd filenames) r.result_size md4 None None None force user group
        | _  -> iter tail
  in
  iter r.result_uids

let load_prefs filename =
  try
    let module P = DonkeyImport.Pref in
    let s = File.to_string filename in
    let t = P.read s in
    t.P.client_tags, t.P.option_tags
  with e ->
      lprintf_nl "Exception %s while loading %s" (Printexc2.to_string e)
      filename;
      [], []

let import_temp temp_dir =
  let list = Unix2.list_directory temp_dir in
  let module P = DonkeyImport.Part in
  let user = CommonUserDb.admin_user () in
  List.iter (fun filename ->
      try
        if Filename2.last_extension filename = ".part" then
          let filename = Filename.concat temp_dir filename in
          let met = filename ^ ".met" in
          if Sys.file_exists met then
            let s = File.to_string met in
            let f = P.read s in
            let filename_met = ref None in
            let size = ref Int64.zero in
            List.iter (fun tag ->
                match tag with
                  { tag_name = Field_Filename; tag_value = String s } ->
                    lprintf_nl "Import Donkey %s" s;

                    filename_met := Some s;
                | { tag_name = Field_Size; tag_value = Uint64 v } ->
                    size := v
                | { tag_name = Field_Size_Hi; tag_value = Uint64 v } ->
                    size := Int64.logor !size (Int64.shift_left v 32)
                | _ -> ()
            ) f.P.tags;
            ignore (really_query_download
                (match !filename_met with
                   None -> filename
                | Some s -> s) !size f.P.md4 None
              (Some filename) (Some (List.rev f.P.absents)) user user.user_default_group);
      with _ -> ()
  ) list


let import_config dirname =
  ignore (load_server_met (Filename.concat dirname "server.met"));
  let ct, ot = load_prefs (Filename.concat dirname "pref.met") in
  let temp_dir = ref (Filename.concat dirname "temp") in

  List.iter (fun tag ->
      match tag with
      | { tag_name = Field_KNOWN "name"; tag_value = String s } ->
          login =:=  s
      | { tag_name = Field_KNOWN "port"; tag_value = Uint64 v } ->
          donkey_port =:=  Int64.to_int v
      | _ -> ()
  ) ct;

  List.iter (fun tag ->
      match tag with
      | { tag_name = Field_KNOWN "temp"; tag_value = String s } ->
          if Sys.file_exists s then (* be careful on that *)
            temp_dir := s
          else (lprintf_nl "Bad temp directory, using default";
              )
      | _ -> ()
  ) ot;

  import_temp !temp_dir

let newline = Bytes.of_string "\n"
let broadcast msg =
  let s = Bytes.cat msg newline in
  let len = Bytes.length s in
  List.iter (fun sock ->
      TcpBufferedSocket.write sock s 0 len
  ) !user_socks

  (*
let saved_name file =
  let name = longest_name file in
(*  if !!use_mp3_tags then
    match file.file_format with
      Mp3 tags ->
        let module T = Mp3tag in
        let name = match name.[0] with
            '0' .. '9' -> name
          | _ -> Printf.sprintf "%02d-%s" tags.T.tracknum name
        in
        let name = if tags.T.album <> "" then
            Printf.sprintf "%s/%s" tags.T.album name
          else name in
        let name = if tags.T.artist <> "" then
            Printf.sprintf "%s/%s" tags.T.artist name
          else name in
        name
    | _ -> name
else *)
  name
    *)

(* 
let print_file buf file =
  Printf.bprintf buf "[%-5d] %s %10Ld %32s %s"
    (file_num file) (file_best_name file) (file_size file) (Md4.to_string file.file_md4)
    (if file_state file = FileDownloaded then "Done" 
      else Int64.to_string (file_downloaded file));

  Buffer.add_char buf '\n';

  Printf.bprintf buf "Connected clients:\n";

  let sock_to_string c =
    match c.client_source.DonkeySources.source_sock with
       NoConnection -> string_of_date (c.client_source.DonkeySources.source_age)
     | ConnectionWaiting _ -> "Connecting"
     | Connection _ -> "Connected"
  in

  let f _ c =
    match c.client_kind with
      Direct_address (ip, port) ->
        Printf.bprintf  buf "[%-5d] %12s %-5d    %s\n"
          (client_num c) (Ip.to_string ip) port (sock_to_string c)
    | _ ->
        Printf.bprintf  buf "[%-5d] %12s            %s\n"
          (client_num c) "Indirect" (sock_to_string c)
  in

(* Intmap.iter f file.file_sources; *)
  match file.file_swarmer with
    None -> ()
  | Some swarmer ->
      let bitmap = CommonSwarming.verified_bitmap swarmer in
      Printf.bprintf buf "\nChunks: %s\n" bitmap
*)

let recover_md4s md4 =
  let file = find_file md4 in
  match file.file_swarmer with
    None -> ()
  | Some swarmer ->
      CommonSwarming.verify_all_chunks swarmer

  (*
  if file.file_chunks <> [||] then
    for i = 0 to file.file_nchunks - 1 do
      file.file_chunks.(i) <- (match file.file_chunks.(i) with
          PresentVerified -> PresentTemp
        | AbsentVerified -> AbsentTemp
        | PartialVerified x -> PartialTemp x
        | x -> x)
    done
*)



let parse_donkey_url url user group =
  let url = Str.global_replace (Str.regexp "|sources,") "|sources|" url in
  match String2.split url '|' with
(* TODO RESULT *)
  | "ed2k://" :: "file" :: name :: size :: md4 :: "/" :: "sources" :: sources :: _
  | "file" :: name :: size :: md4 :: "/" :: "sources" :: sources :: _ ->
(*  ed2k://|file|Wikipedia_3.3_noimages.iso|2666311680|747735CD46B61DA92973E9A8840A9C99|/|sources,62.143.4.124:4662|/  *)
      if Int64.of_string size >= max_emule_file_size then
        (Printf.sprintf (_b "Files > %s are not allowed")
          (Int64ops.int64_to_human_readable max_emule_file_size)), false
      else
        begin
          let md4 = if String.length md4 > 32 then
            String.sub md4 0 32 else md4 in
          let new_sources = ref [] in
          let s = String2.split sources ',' in
            List.iter (fun s ->
              begin try
                match String2.split s ':' with
                 [ip;port] ->
                    let source_ip = Ip.of_string ip in
                    let source_port = int_of_string port in
                    new_sources := (source_ip, source_port) :: !new_sources
                | _ -> ()
              with _ -> ()
          end) s;
          begin
            try
              let file = query_download name (Int64.of_string size)
                (Md4.of_string md4) None None None false user group in
              let new_file = find_file (Md4.of_string md4) in
              CommonInteractive.start_download file;
              if !new_sources <> [] then
                begin
                  List.iter (fun (source_ip, source_port) ->
                    add_source new_file source_ip source_port Ip.null 0
                  ) !new_sources;
                  (Printf.sprintf (_b "added %d sources to new download") (List.length !new_sources)), true
                end
              else "", true
            with
              Already_downloading (s)
            | Already_shared (s) -> s, false
            | e -> (Printexc2.to_string e), false
          end
        end
  | "ed2k://" :: "file" :: name :: size :: md4 :: _
  | "file" :: name :: size :: md4 :: _ ->
      if Int64.of_string size >= max_emule_file_size then
        (Printf.sprintf (_b "Files > %s are not allowed")
          (Int64ops.int64_to_human_readable max_emule_file_size)), false
      else
        let md4 = if String.length md4 > 32 then
          String.sub md4 0 32 else md4 in
        let name =
          let name2 = Filename2.filesystem_compliant name `Unknown 0 in
            if name2 = "" then
              Printf.sprintf "urn_ed2k_%s" md4
            else
              name2
        in
          begin try
            let file = query_download name (Int64.of_string size)
              (Md4.of_string md4) None None None false user group;
            in
            CommonInteractive.start_download file;
            "", true
          with
            Already_downloading (s)
          | Already_shared (s) -> s, false
          | e -> (Printexc2.to_string e), false
          end
  | "ed2k://" :: "server" :: ip :: port :: _
  | "server" :: ip :: port :: _ ->
      let ip = Ip.of_string ip in
      let s = force_add_server ip (int_of_string port) in
      server_connect (as_server s.server_server);
      "", true
  | "ed2k://" :: "serverlist" :: url :: _
  | "serverlist" :: url :: _ ->
      if !!update_server_list_server_met then
        ignore (download_server_met url);
      "", true
  | "ed2k://" :: "friend" :: ip :: port :: _
  | "friend" :: ip :: port :: _ ->
      let ip = Ip.of_string ip in
      let port = int_of_string port in
      let c = new_client (Direct_address (ip,port)) None in
      friend_add c;
      "", true

  | _ -> "", false

let ip_of_server_cid s =
  match s.server_cid with
    None -> Ip.null
  | Some ip -> ip

let op_file_check file =
  match file.file_swarmer with
    None -> ()
  | Some swarmer ->
      CommonSwarming.verify_all_chunks_immediately swarmer

let register_commands list =
  register_commands
    (List2.tail_map (fun (n,f,h) -> (n, "Network/Edonkey", f,h)) list)

let commands = [
    "n", Arg_multiple (fun args o ->
        let buf = o.conn_buf in
        let ip, port =
          match args with
            [ip ; port] -> ip, port
          | [ip] -> ip, "4661"
          | _ -> failwith "n <ip> [<port>]: bad argument number"
        in
        let ip = Ip.from_name ip in
        let port = int_of_string port in

        let _ = force_add_server ip port in
        Printf.bprintf buf "New server %s:%d\n"
          (Ip.to_string ip) port;
        ""
    ), "<ip> [<port>] :\t\t\tadd a server";

    "afr", Arg_multiple (fun args o ->
        let ip, port =
          match args with
            [ip ; port] -> ip, port
          | [ip] -> ip, "4662"
          | _ -> failwith "afr <ip> [<port>]: bad argument number"
        in
        let ip = Ip.from_name ip in
        let port = int_of_string port in
        let c = new_client (Direct_address (ip,port)) None in
        friend_add c;
        "friend added";
    ),  "<ip> [<port>] :\t\t\tadd a friend";

(*
    "comments", Arg_one (fun filename o ->
(* TODO        DonkeyIndexer.load_comments filename;
        DonkeyIndexer.save_comments (); *)
        "comments loaded and saved"
    ), "<filename> :\t\t\tload comments from file";

    "comment", Arg_two (fun md4 comment o ->
        let buf = o.conn_buf in
        let md4 = Md4.of_string md4 in
(* TODO        DonkeyIndexer.add_comment md4 comment; *)
        "Comment added"
    ), "<md4> \"<comment>\" :\t\tadd comment on a md4";
*)

    "import", Arg_one (fun dirname o ->
        try
          import_config dirname;
          "config loaded"
        with e ->
            Printf.sprintf "error %s while loading config" (
              Printexc2.to_string e)
    ), "<dirname> :\t\t\timport the config from dirname";

    "import_temp", Arg_one (fun dirname o ->
        try
          import_temp dirname;
          "temp files loaded"
        with e ->
            Printf.sprintf "error %s while loading temp files" (
              Printexc2.to_string e)
    ), "<temp_dir> :\t\timport the old edonkey temp directory";

(*
    "load_old_history", Arg_none (fun o ->
(* TODO        DonkeyIndexer.load_old_history (); *)
        "Old history loaded"
    ), ":\t\t\tload history.dat file";
*)

    "servers", Arg_one (fun filename o ->
  if !!update_server_list_server_met then
    begin
      let nservers = List.length (Hashtbl2.to_list servers_by_key) in
      if (String2.starts_with filename "http") then
        begin
          ignore (download_server_met filename);
          Printf.sprintf "download of %s started, check log for results" filename
        end
      else
        if Sys.file_exists filename then begin
                let n = load_server_met (unpack_server_met filename "") in
                  Printf.sprintf "%d servers found, %d new ones inserted"
        n ((List.length (Hashtbl2.to_list servers_by_key)) - nservers)
          end
        else
          Printf.sprintf "%s does not exist, ignoring..." filename
    end
  else
    Printf.sprintf "ED2K-update_server_list_server_met is disabled, ignoring..."
    ), "<filename|URL> :\t\tadd the servers from a server.met file or URL";

    "id", Arg_none (fun o ->
        let buf = o.conn_buf in
        List.iter (fun s ->
            Printf.bprintf buf "For %s (%s) --->   %s\n"
              (string_of_server s) s.server_name
              (match s.server_cid with
                None -> "waiting"
              | Some ip ->
                  if low_id ip then
                    Printf.sprintf "%s (LowID)" (Int64.to_string (Ip.to_int64 (Ip.rev ip)))
                  else
                    Printf.sprintf "%s (HighID)" (Ip.to_string (ip_of_server_cid s)))
        ) (connected_servers());
        ""
    ), ":\t\t\t\t\tprint ID on connected servers";

    "recover_bytes", Arg_multiple (fun args o ->
        let buf = o.conn_buf in

        List.iter (fun arg ->
            let num = int_of_string arg in
            List.iter (fun file ->
                if file_num file = num then begin
                    match file.file_swarmer with
                      None -> ()
                    | Some swarmer ->
                        let segments = CommonFile.recover_bytes (as_file file) in
                        let old_downloaded = CommonSwarming.downloaded swarmer in
                        CommonSwarming.set_present swarmer segments;
                        let new_downloaded = CommonSwarming.downloaded swarmer in
                        Printf.bprintf buf "Recovered %Ld bytes for %s\n"
                          (new_downloaded -- old_downloaded)
                        (file_best_name file)

                  end
            ) !current_files
        ) args;
        ""
    ) , "<f1> <f2> ... :\t\ttry to recover these files at byte level";

    "preferred", Arg_two (fun arg1 arg2 o ->
        if CommonUserDb.user2_is_admin o.conn_user.ui_user then
        let preferred = bool_of_string arg1 in
        let ip = Ip.of_string arg2 in
        Hashtbl.iter (fun ip_s s ->
            if ip_s = ip then begin
              s.server_preferred <- preferred;
              server_must_update s;
            end
        ) servers_by_key;
        "ok"
        else
          _s "You are not allowed to change preferred status"
    ), "<true|false> <ip> :\t\tset the server with this IP as preferred";

    "bs", Arg_multiple (fun args o ->
        if CommonUserDb.user2_is_admin o.conn_user.ui_user then begin
        List.iter (fun arg ->
            let range = Ip.range_of_string arg in
            server_black_list =:=  range :: !!server_black_list;
        ) args;
        "done"
        end else
          _s "You are not allowed to blacklist servers"
    ), "<range1> <range2> ... :\t\tadd these IPs to the servers black list (can be single IPs, CIDR ranges or begin-end ranges)";

    "port", Arg_one (fun arg o ->
        if CommonUserDb.user2_is_admin o.conn_user.ui_user then begin
        donkey_port =:= int_of_string arg;
        "new port will change at next restart"
        end else
          _s "You are not allowed to change connection port"
    ),
    "<port> :\t\t\t\tchange connection port";

    "scan_temp", Arg_none (fun o ->
        if CommonUserDb.user2_is_admin o.conn_user.ui_user then begin
        let buf = o.conn_buf in
        let list = Unix2.list_directory !!temp_directory in

        let counter = ref 0 in
        let tr = ref "dl-1" in

        if use_html_mods o then begin

Printf.bprintf buf
"\\<script language=javascript\\>
\\<!--
function submitRenameForm(i) {
var formID = document.getElementById(\\\"renameForm\\\" + i)
parent.fstatus.location.href='submit?q=rename+'+i+'+\\\"'+encodeURIComponent(formID.newName.value)+'\\\"';
}
//--\\>
\\</script\\>";

          html_mods_table_header buf "scan_tempTable" "scan_temp" [
            ( Str, "srh", "Filename", "Filename (press ENTER to rename)" ) ;
            ( Str, "srh", "Status", "Status" ) ;
            ( Str, "srh", "MD4 (link=ed2k)", "MD4 (link=ed2k)" ); ];


    end;

        List.iter (fun filename ->
            incr counter;
            if (!counter mod 2 == 0) then tr := "dl-1"
            else tr := "dl-2";
            let uid =
              try Uid.of_string filename
              with _ -> Uid.no
            in
            let (other,md4) =
              match Uid.to_uid uid with
              | Ed2k md4 -> (false,md4)
              | NoUid ->
                  (try
                     if String.length filename = 32 then
                       (false,(Md4.of_string filename))
                     else (true,Md4.null)
                   with _ ->
                     (true,Md4.null)
                  )
              | _ -> (true,Md4.null)
            in
            try
              if other then raise Not_found;
              try
                let file = find_file md4 in
                if use_html_mods o then
          let fnum = (file_num file) in
                  Printf.bprintf buf "
        \\<tr class=\\\"%s\\\"\\>\\<td class=\\\"sr\\\"\\>
        \\<form name=\\\"renameForm%d\\\" id=\\\"renameForm%d\\\" action=\\\"javascript:submitRenameForm(%d);\\\"\\>
        \\<input style=\\\"font: 8pt sans-serif\\\" name=\\\"newName\\\" type=text size=50 value=\\\"%s\\\"\\>\\</input\\>\\</td\\>\\</form\\>
                \\<td class=\\\"sr \\\"\\>%s\\</td\\>
                \\<td class=\\\"sr \\\"\\>\\<A HREF=\\\"%s\\\"\\>%s\\</A\\>\\</td\\>\\</tr\\>"
                    !tr fnum fnum fnum (file_best_name file) "Downloading" (file_print_ed2k_link (file_best_name file) (file_size file) file.file_md4) filename
                else
                  Printf.bprintf buf "%s is %s %s\n" filename
                    (file_best_name file)
                  "(downloading)"
              with _ ->
                  if use_html_mods o then
                    Printf.bprintf buf "\\<tr class=\\\"%s\\\"\\>\\<td class=\\\"sr\\\"\\>%s\\</td\\>
                \\<td class=\\\"sr \\\"\\>%s\\</td\\>
                \\<td class=\\\"sr \\\"\\>%s\\</td\\>\\</tr\\>" !tr
(* TODO RESULT                      (try
                        let names = DonkeyIndexer.find_names md4 in
                        List.hd names
                      with _ -> "Never Seen") *) "?"
                    (if List.mem md4 !!old_files then
                        "Old file" else "Unknown")
                    filename
                  else
                    Printf.bprintf buf "%s %s %s\n"
                      filename
                      (if List.mem md4 !!old_files then
                        "is an old file" else "is unknown")
(* TODO RESULT                    (try
                        let names = DonkeyIndexer.find_names md4 in
                        List.hd names
                      with _ -> "and never seen") *) "?"

            with _ ->
                if use_html_mods o then
                  Printf.bprintf buf "\\<tr class=\\\"%s\\\"\\>\\<td class=\\\"sr\\\"\\>Unknown\\</td\\>
                \\<td class=\\\"sr \\\"\\>\\</td\\>
                \\<td class=\\\"sr \\\"\\>%s\\</td\\>\\</tr\\>" !tr filename
                else
                  Printf.bprintf buf "%s unknown\n" filename

        ) list;

        if use_html_mods o then Printf.bprintf buf "\\</table\\>\\</div\\>";
        "" end
        else begin
          print_command_result o "You are not allowed to use scan_temp";
        "" end

    ), ":\t\t\t\tprint temp directory content";

    "sources", Arg_none (fun o ->
        if CommonUserDb.user2_is_admin o.conn_user.ui_user then begin
          DonkeySources.print o.conn_buf o.conn_output;
          "" end
        else begin
          print_command_result o "You are not allowed to list sources";
          "" end
    ), ":\t\t\t\tshow sources currently known";

    "xs", Arg_none (fun o ->
        let buf = o.conn_buf in
        if !xs_last_search >= 0 then begin
            try
              DonkeyUdp.make_xs (CommonSearch.search_find !xs_last_search);
              if o.conn_output = HTML then
                html_mods_table_one_row buf "searchTable" "search" [
                  ("", "srh", "Extended search started"); ]
              else
                Printf.bprintf buf "extended search started";
            with e -> Printf.bprintf buf "Error %s" (Printexc2.to_string e)
          end else begin
            if o.conn_output = HTML then
              html_mods_table_one_row buf "searchTable" "search" [
                ("", "srh", "No previous search to extend"); ]
            else
              Printf.bprintf buf "No previous search to extend";
          end;
        ""
    ), ":\t\t\t\t\textend the last search";

    "clh", Arg_none (fun o ->
(* TODO RESULT        DonkeyIndexer.clear (); *)
        "local history cleared"
    ), ":\t\t\t\t\tclear local history";

(* TODO RESULT *)
    "dd", Arg_two(fun size md4 o ->
        let file = query_download md4 (Int64.of_string size)
          (Md4.of_string md4) None None None false o.conn_user.ui_user o.conn_user.ui_user.user_default_group in
        CommonInteractive.start_download file;
        "download started"
    ), "<size> <md4> :\t\t\tdownload from size and md4";

    "remove_old_servers", Arg_none (fun o ->
        let buf = o.conn_buf in
        DonkeyServers.remove_old_servers ();
        if o.conn_output = HTML then
          html_mods_table_one_row buf "serversTable" "servers" [
            ("", "srh", "Clean done"); ]
        else
          Printf.bprintf buf "clean done";
        ""
    ), ":\t\t\tremove servers that have not been connected for several days";

    "reset_md4", Arg_none (fun _ ->
        set_simple_option donkey_ini "client_md4" (Md4.to_string (mldonkey_md4 (Md4.random ())));
        if Autoconf.donkey_sui_works () then set_simple_option donkey_ini "client_private_key" (DonkeySui.SUI.create_key ());
        "reset client_md4/client_private_key"
    ), ":\t\t\t\treset client_md4/client_private_key to random values";

    "bp", Arg_multiple (fun args o ->
        if CommonUserDb.user2_is_admin o.conn_user.ui_user then begin
        List.iter (fun arg ->
            let port = int_of_string arg in
            port_black_list =:=  port :: !!port_black_list;
        ) args;
        "done"
        end else
          _s "You are not allowed to blacklist ports"
    ), "<port1> <port2> ... :\t\tadd these ports to the port black list";
  ]

let _ =
  register_commands commands;
  file_ops.op_file_download_order <- (fun file strategy ->
      match file.file_swarmer with
      | None -> None
      | Some s ->
          (match strategy with
          (* return current strategy *)
          | None -> Some (CommonSwarming.get_strategy s)
          | Some strategy -> CommonSwarming.set_strategy s strategy;
                      Some (CommonSwarming.get_strategy s))
  );
  file_ops.op_file_resume <- (fun file ->
      reconnect_all file;
  );
  file_ops.op_file_pause <- (fun file ->
    DonkeySources.iter_active_sources (fun s ->
      let s_uid = s.DonkeySources.source_uid in
      let c = new_client s_uid s.DonkeySources.source_country_code in
      match client_state c with
      | Connected_downloading f when f = file_num file -> disconnect_client c Closed_by_peer
      | _ -> ()
    ) file.file_sources
  );
  file_ops.op_file_queue <- file_ops.op_file_pause;

  file_ops.op_file_commit <- (fun file new_name ->

      CommonSwarming.remove_swarmer file.file_swarmer;
      file.file_swarmer <- None;
(*      DonkeyStats.save_download_history file; *)

      if !!keep_downloaded_in_old_files
          && not (List.mem file.file_md4 !!old_files) then
        old_files =:= file.file_md4 :: !!old_files;
      DonkeyShare.remember_shared_info file new_name;

(**************************************************************

Since the new version of Unix32.rename does not update the
file name, we need to create a new 'file' and stop using the old
one. For that, we need first to remove definitively the old one
from the system, and thus to disconnect all uploaders for this
file.---------> to be done urgently

***************************************************************)

      unshare_file file;
  );
  network.op_network_connected <- (fun _ ->
    !nservers > 0
  );
  network.op_network_private_message <- (fun iddest s ->
      try
        let c = DonkeyGlobals.find_client_by_name iddest in
        match c.client_source.DonkeySources.source_sock with
          NoConnection ->
            DonkeyClient.reconnect_client c;
            c.client_pending_messages <- c.client_pending_messages @ [s];
        | ConnectionWaiting _ ->
            c.client_pending_messages <- c.client_pending_messages @ [s];
        | Connection sock ->
            client_send c (DonkeyProtoClient.SayReq s)
      with
        Not_found -> ()
  );
  network.op_network_download <- (fun r user group ->
      result_download r r.result_names r.result_force user group
  )

module P = GuiTypes

  
(* How often is this function called when the interface is running ?
is it called when no interface is connected ? it should be as fast
as possible. *)

let _ =
  file_ops.op_file_info <- (fun file ->
      try
        let last_seen = match file.file_swarmer with
            None -> [| last_time () |]
          | Some swarmer -> CommonSwarming.compute_last_seen swarmer
        in
        let v =
            { (impl_file_info file.file_file) with

            P.file_network = network.network_num;
            P.file_md4 = file.file_md4;
            P.file_all_sources = file.file_sources.DonkeySources.manager_all_sources;
            P.file_active_sources = file.file_sources.DonkeySources.manager_active_sources;
            P.file_chunks = 
                (match file.file_swarmer with
                | None -> None
                | Some swarmer -> Some (CommonSwarming.chunks_verified_bitmap swarmer));
            P.file_chunk_size = 
                (match file.file_swarmer with
                | None -> None
                | Some t -> Some (List.map (fun t -> t.CommonSwarming.t_chunk_size) t.CommonSwarming.t_s.CommonSwarming.s_networks));
            P.file_availability =
              [
                network.network_num,
                (match file.file_swarmer with
                  | None -> "" 
                  | Some swarmer -> CommonSwarming.chunks_availability swarmer)
              ];
            P.file_format = file.file_format;
            P.file_chunks_age = last_seen;
            P.file_uids = [Uid.create (Ed2k file.file_md4)];
            P.file_comments = file.file_comments
          } in
        v
      with e ->
          lprintf_nl "Exception %s in op_file_info" (Printexc2.to_string e);
          raise e

  )

let _ =
  server_ops.op_server_info <- (fun s ->
      check_server_country_code s;
      if !!enable_donkey then
        { (impl_server_info s.server_server) with

          P.server_network = network.network_num;
          P.server_addr = Ip.addr_of_ip s.server_ip;
          P.server_port = s.server_port;
          P.server_realport = (match s.server_realport with Some x -> x | None -> 0);
          P.server_country_code = s.server_country_code;
          P.server_score = s.server_score;
          P.server_nusers = (match s.server_nusers with None -> 0L | Some v -> v);
          P.server_nfiles = (match s.server_nfiles with None -> 0L | Some v -> v);
          P.server_name = s.server_name;
          P.server_description = s.server_description;
          P.server_banner = s.server_banner;
          P.server_preferred = s.server_preferred;
          P.server_master = s.server_master;
          P.server_published_files = (List.length s.server_sent_shared);
          P.server_version = s.server_version;
          P.server_max_users = (match s.server_max_users with None -> 0L | Some v -> v);
          P.server_soft_limit = (match s.server_soft_limit with None -> 0L | Some v -> v);
          P.server_hard_limit = (match s.server_hard_limit with None -> 0L | Some v -> v);
          P.server_lowid_users = (match s.server_lowid_users with None -> 0L | Some v -> v);
          P.server_ping = s.server_ping;
          P.server_features = let temp_buf = Buffer.create 100 in
            if s.server_has_zlib then Printf.bprintf temp_buf "zlib ";
            if s.server_has_newtags then Printf.bprintf temp_buf "newtags ";
            if s.server_has_unicode then Printf.bprintf temp_buf "unicode ";
            if s.server_has_related_search then Printf.bprintf temp_buf "related_search ";
            if s.server_has_tag_integer then Printf.bprintf temp_buf "tag_integer ";
            if s.server_has_largefiles then Printf.bprintf temp_buf "largefiles ";
            if s.server_has_get_sources then Printf.bprintf temp_buf "getsources ";
            if s.server_has_get_sources2 then Printf.bprintf temp_buf "getsources2 ";
            if s.server_has_get_files then Printf.bprintf temp_buf "getfiles ";
            (match s.server_obfuscation_tcp with
             | Some p when p <> 0 -> Printf.bprintf temp_buf "tcp_obfuscation(%d) " p | _ -> ());
            (match s.server_obfuscation_udp with
             | Some p when p <> 0 -> Printf.bprintf temp_buf "udp_obfuscation(%d) " p | _ -> ());
            if s.server_auxportslist <> "" then Printf.bprintf temp_buf "auxportslist %s " s.server_auxportslist;
            if s.server_dynip <> "" then Printf.bprintf temp_buf "dynip %s " s.server_dynip;
            if Buffer.contents temp_buf <> "" then Some (Buffer.contents temp_buf) else None;
        }
      else raise Not_found
  )


let _ =
  user_ops.op_user_info <- (fun u ->
      {
        P.user_num = u.user_user.impl_user_num;
        P.user_md4 = u.user_md4;
        P.user_ip = u.user_ip;
        P.user_port = u.user_port;
        P.user_tags = []; (* u.user_tags; *)
        P.user_name = u.user_name;
        P.user_server = u.user_server.server_server.impl_server_num;
      }
  )

let string_of_client_ip c =
  try match c.client_source.DonkeySources.source_sock with
      Connection sock -> (Ip.to_string (peer_ip sock))
    | _ -> ""
  with _ -> ""

let get_ips_cc_cn c =
  check_client_country_code c;
  try
    match c.client_kind with
    | Direct_address (ip,port) ->
        let cc,cn = Geoip.get_country_code_name c.client_country_code in
        (Ip.to_string ip),cc,cn
    | Indirect_address (_,_,_,_,real_ip) ->
        let cc,cn = Geoip.get_country_code_name c.client_country_code in
        (Ip.to_string real_ip),cc,cn
    | _ ->  
        let cc,cn = Geoip.unknown_country in
        (string_of_client_ip c),cc,cn
  with _ -> ("X","??","Country Error")


let _ =
  client_ops.op_client_info <- (fun c ->
      check_client_country_code c;
      { (impl_client_info c.client_client) with

        P.client_network = network.network_num;
        P.client_kind = (match c.client_kind with
            Direct_address (ip, port) -> Known_location (ip,port)
          | Indirect_address (server_ip, server_port, ip, port, real_ip) -> 
              Indirect_location (c.client_name,c.client_md4, real_ip, port)
          | _ -> Indirect_location (c.client_name,c.client_md4, c.client_ip, 0));
        P.client_country_code = c.client_country_code;
        P.client_state = client_state c;
        P.client_type = client_type c;
        P.client_name = c.client_name;
        P.client_rating = c.client_rating;
        P.client_connect_time = c.client_connect_time;
        P.client_software = brand_to_string_short c.client_brand;
        P.client_os = c.client_osinfo;
        P.client_release = c.client_emule_proto.emule_release;
        P.client_emulemod = brand_mod_to_string_short c.client_brand_mod;
        P.client_total_downloaded = c.client_total_downloaded;
        P.client_total_uploaded = c.client_total_uploaded;
        P.client_session_downloaded = c.client_session_downloaded;
        P.client_session_uploaded = c.client_session_uploaded;
        P.client_upload =
        (match client_upload (as_client c) with
            Some f -> Some (CommonFile.file_best_name f)
          | None -> None);
        P.client_sui_verified = c.client_sui_verified;
        P.client_file_queue = List.map (fun (file,_,_) -> as_file file) c.client_file_queue
      }
  );
  client_ops.op_client_debug <- (fun c debug ->
      c.client_debug <- debug)

let _ =
  server_ops.op_server_remove <- (fun s ->
      DonkeyGlobals.remove_server s.server_ip s.server_port
  );
  server_ops.op_server_connect <- connect_server;
  server_ops.op_server_disconnect <- (fun s ->
      disconnect_server s Closed_by_user);

  server_ops.op_server_query_users <- (fun s ->
      match s.server_sock, server_state s with
        Connection sock, (Connected _ | Connected_downloading _) ->
          server_send sock (DonkeyProtoServer.QueryUsersReq "");
          Fifo.put s.server_users_queries false
      | _ -> ()
  );
  server_ops.op_server_find_user <- (fun s user ->
      match s.server_sock, server_state s with
        Connection sock, (Connected _ | Connected_downloading _) ->
          server_send sock (DonkeyProtoServer.QueryUsersReq user);
          Fifo.put s.server_users_queries true
      | _ -> ()
  );
  server_ops.op_server_users <- (fun s ->
      List2.tail_map (fun u -> as_user u.user_user) s.server_users)    ;

  server_ops.op_server_published <- (fun s ->
      List.map (fun f -> as_file f) s.server_sent_shared);

  server_ops.op_server_cid <- (fun s -> ip_of_server_cid s);

  server_ops.op_server_low_id <- (fun s -> low_id (ip_of_server_cid s));

  server_ops.op_server_set_preferred <- (fun s b ->
    s.server_preferred <- b;
    server_must_update s);

  server_ops.op_server_rename <- (fun s name ->
    s.server_name <- name;
    server_must_update s);

  ()

let _ =
  file_ops.op_file_save_as <- (fun file name ->
      add_file_filenames (as_file file) name;
      set_file_best_name (as_file file) name 0
  );
  file_ops.op_file_shared <- (fun file ->
      match file.file_shared with
        None -> None
      | Some sh -> Some (as_shared sh)
  );
  file_ops.op_file_set_format <- (fun file format ->
      file.file_format <- format);
  file_ops.op_file_check <- op_file_check;
  file_ops.op_file_recover <- (fun file ->
      if file_state file = FileDownloading then
        reconnect_all file);
  file_ops.op_file_all_sources <- (fun file ->
      let list = ref [] in
      DonkeySources.iter_all_sources (fun s ->
          let s_uid = s.DonkeySources.source_uid in
          let c = new_client s_uid s.DonkeySources.source_country_code in
          list := (as_client c) :: !list
      ) file.file_sources;
      !list
  );
  file_ops.op_file_active_sources <- (fun file ->
      let list = ref [] in
      DonkeySources.iter_active_sources (fun s ->
          let s_uid = s.DonkeySources.source_uid in
          let c = new_client s_uid s.DonkeySources.source_country_code in
          list := (as_client c) :: !list
      ) file.file_sources;
      !list
  );
  file_ops.op_file_print <- (fun file o ->

     let buf = o.conn_buf in
     if not (use_html_mods o) then begin
        let cntr = ref 0 in
        List.iter (fun (ip, n, r, c) ->
          incr cntr;
          Printf.bprintf buf
            "Comment %d: Rating(%d): %s (%s/%s)\n" !cntr r (Charset.Locale.to_utf8 c) n (Ip.to_string ip)) file.file_comments
     end else begin
     let tr () =
      Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr ())
     in

      tr ();
      html_mods_td buf [
        ("Fake check links", "sr br", "Fakecheck");
        ("", "sr", Printf.sprintf "\\<a target=\\\"_blank\\\" href=\\\"http://bitzi.com/lookup/urn:ed2k:%s\\\"\\>[Bitzi-Bitpedia]\\</a\\> \\<a target=\\\"_blank\\\" href=\\\"http://www.filedonkey.com/url/%s\\\"\\>[FileDonkey]\\</a\\>"
            (Md4.to_string file.file_md4) (Md4.to_string file.file_md4)
        ) ];

      let cntr = ref 0 in
      List.iter (fun (ip, n, r, c) -> 
        incr cntr;
        tr ();
        html_mods_td buf [
          ("Comment", "sr br", Printf.sprintf "Comment %d" !cntr);
          ("User rating and comment", "sr", Printf.sprintf "Rating(%d): %s (%s/%s)" r (Charset.Locale.to_utf8 c) n (Ip.to_string ip));
        ];
      ) file.file_comments;

      tr ();
      html_mods_td buf [
        ("File History Links", "sr br", "File History");
        ("","sr", Printf.sprintf
"\\<a target=\\\"_blank\\\" href=\\\"http://tothbenedek.hu/ed2kstats/ed2k?hash=%s\\\"\\>Toth File History\\</a\\>
\\<a target=\\\"_blank\\\" href=\\\"http://ed2k.titanesel.ws/ed2k.php?hash=%s\\\"\\>Titanesel File History\\</a\\>"
            (Md4.to_string file.file_md4) (Md4.to_string file.file_md4)
        )
       ];
      tr ();
      let ed2k = file_print_ed2k_link (file_best_name file) (file_size file) file.file_md4 in
      html_mods_td buf [
        ("ed2k link", "sr br", "ed2k link");
        ("", "sr", Printf.sprintf "\\<a href=\\\"%s\\\"\\>%s\\</A\\>" ed2k ed2k) ];
      tr ();
      let optionlist = ref "" in
      List.iter (fun name ->
          optionlist := !optionlist ^ Printf.sprintf "\\<option value=\\\"%s\\\"\\>%s\n" name name;
      ) file.file_file.impl_file_filenames;


      let input_size = (Printf.sprintf "%d" ((String.length (file_best_name file))+3)) in
      html_mods_td buf [
        ("Rename file by selecting an alternate name", "sr br", "Filename");
        ("Rename file", "sr",
          Printf.sprintf "\\<script language=javascript\\>
\\<!--
function submitRenameForm(i) {
var formID = document.getElementById(\\\"renameForm\\\" + i)
parent.fstatus.location.href='submit?q=rename+%d+\\\"'+encodeURIComponent(formID.newName.value)+'\\\"';
}
//--\\>
\\</script\\>" (file_num file)

   ^ "\\<table border=0 cellspacing=0 cellpadding=0\\>\\<tr\\>"
   ^ "\\<form name=\\\"renameForm1\\\" id=\\\"renameForm1\\\" action=\\\"javascript:submitRenameForm(1);\\\"\\>"
   ^ "\\<td\\>"
   ^ "\\<select name=\\\"newName\\\" id=\\\"newName\\\" onchange=\\\"javascript:renameForm2.newName.value=renameForm1.newName.options[renameForm1.newName.selectedIndex].value;this.form.submit();\\\"\\>"
   ^ Printf.sprintf "\\<option value=\\\"%s\\\" selected\\>%s\n" (file_best_name file) (file_best_name file)
   ^ !optionlist
   ^ "\\</select\\>\\</td\\>\\</form\\>\\</tr\\>\\<tr\\>\\<form name=\\\"renameForm2\\\" id=\\\"renameForm2\\\" action=\\\"javascript:submitRenameForm(2);\\\"\\>"
   ^ "\\<td\\>"
   ^ "\\<input name=\\\"newName\\\" type=text size=" ^ input_size ^ " value=\\\"" ^ (file_best_name file) ^ "\\\"\\>\\</input\\>\\</td\\>\\</form\\>\\</tr\\>\\</table\\>"
    ) ];
  end
  );
  file_ops.op_file_print_sources <- (fun file o ->

    if not (use_html_mods o) then raise Not_found;
    let buf = o.conn_buf in
    let sources_list = ref [] in
    DonkeySources.iter_relevant_sources (fun s ->
      let s_uid = s.DonkeySources.source_uid in
      let c = new_client s_uid s.DonkeySources.source_country_code in
      sources_list := (s,c) :: !sources_list
    ) file.file_sources;

    if List.length !sources_list > 0 then 
    begin

      let chunks =
      (match file.file_swarmer with
      | None -> None 
      | Some swarmer -> Some (CommonSwarming.chunks_verified_bitmap swarmer))
      in

      html_mods_table_header buf "sourcesTable" "sources al" ([
          ( Num, "srh ac", "Client number (click to add as friend)", "Num" ) ;
          ( Str, "srh", "[A] = Active download from client", "A" ) ;
          ( Str, "srh", "Client state", "CS" ) ;
          ( Str, "srh", "Client name", "Name" ) ;
          ( Str, "srh", "Client brand", "CB" ) ;
          ( Str, "srh", "Client release", "CR" ) ;
        ] @
          (if !!emule_mods_count then [( Str, "srh", "eMule MOD", "EM" )] else [])
        @ [
          ( Str, "srh", "Overnet [T]rue, [F]alse", "O" ) ;
          ( Str, "srh", "Connection [I]ndirect, [D]irect", "C" ) ;
          ( Str, "srh", "Secure User Identification [N]one, [P]assed, [F]ailed", "S" ) ;
          ( Str, "srh br", "IP address", "IP address" ) ;
          ] @ (if Geoip.active () then [( Str, "srh br", "Country Code/Name", "CC" )] else []) @ [
          ( Num, "srh ar", "Total UL bytes to this client for all files", "tUL" ) ;
          ( Num, "srh ar br", "Total DL bytes from this client for all files", "tDL" ) ;
          ( Num, "srh ar", "Session UL bytes to this client for all files", "sUL" ) ;
          ( Num, "srh ar br", "Session DL bytes from this client for all files", "sDL" ) ;
          ( Num, "srh ar", "Your queue rank on this client", "Rnk" ) ;
          ( Num, "srh ar br", "Source score", "Scr" ) ;
          ( Num, "srh ar br", "Last ok", "LO" ) ;
          ( Num, "srh ar", "Request score", "RS" ) ;
          ( Num, "srh ar", "Request queue (see sources command)", "RQ" ) ;
          ( Num, "srh ar br", "Request time (last connect) (# minutes ago)", "RT" ) ;
          ( Str, "srh", "Has a slot [T]rue, [F]alse", "H" ) ;
          ( Str, "srh br", "Banned [T]rue, [F]alse", "B" ) ;
          ( Num, "srh ar", "Requests sent", "RS" ) ;
          ( Num, "srh ar", "Requests received", "RR" ) ;
          ( Num, "srh ar br", "Connected time (minutes)", "CT" ) ;
          ( Str, "srh br", "Client MD4", "MD4" ) ;
          ( Str, "srh", "Chunks (absent|partial|present|verified)",
          match chunks with
          | None -> ""
          | Some chunks -> colored_chunks chunks) ;
          ( Num, "srh ar", "Number of full chunks", (Printf.sprintf "%d"
            (match chunks with
            | None -> 0
            | Some chunks ->
                VerificationBitmap.fold_lefti (fun acc _ s ->
                  if s = VerificationBitmap.State_verified then acc + 1
                  else acc) 0 chunks))) ]);


      html_mods_cntr_init();
      List.iter (fun (s,c) ->
        let ac = as_client c in
        try
          Printf.bprintf buf "\\<tr onMouseOver=\\\"mOvr(this);\\\" onMouseOut=\\\"mOut(this);\\\" class=\\\"dl-%d\\\"\\>"
            (html_mods_cntr());

          Printf.bprintf buf "\\<td title=\\\"Add as Friend\\\" class=\\\"srb ar\\\" onClick=\\\"parent.fstatus.location.href='submit?q=friend_add+%d'\\\"\\>%d\\</TD\\>"
            (client_num c) (client_num c);

          let req_queue, req_score, req_min = 
              try 
                let r = DonkeySources.find_request s file.file_sources in
                let q = r.DonkeySources.request_queue in
                let s = r.DonkeySources.request_score in
                let t = r.DonkeySources.request_time in
                (Printf.sprintf "%d" q,
                 Printf.sprintf "%d" s,
                 if t = 0 then "N" else Printf.sprintf "%d" ((last_time() - t) / 60))
              with _ -> ("?","?","?") in

          let ip_string,cc,cn = get_ips_cc_cn c in

          html_mods_td buf ([
            ("", "sr", (match c.client_download with
                  None -> ""
                | Some _ -> (
                      let qfiles = c.client_file_queue in
                      let (qfile, qchunks,_) =  List.hd qfiles in
                      if (qfile == file) then
                        "A" else "";)) );
            ((string_of_connection_state (client_state c)), "sr",
              (short_string_of_connection_state (client_state c)) );
            (String.escaped c.client_name, "sr", client_short_name c.client_name);
            (client_software (brand_to_string c.client_brand) c.client_osinfo, "sr",
             client_software_short (brand_to_string_short c.client_brand) c.client_osinfo);
            ("", "sr", c.client_emule_proto.emule_release);

            ] @
            (if !!emule_mods_count then [(brand_mod_to_string c.client_brand_mod, "sr", brand_mod_to_string_short c.client_brand_mod)] else [])
            @ [

            ("", "sr", (if DonkeySources.source_brand c.client_source then "T" else "F"));
            ("", "sr", (match c.client_kind with
                  | Direct_address (ip,port) -> "D"
                  | _ -> "I"
                ));
            ("", "sr", (match c.client_sui_verified with
                  | None -> "N"
                  | Some b -> if b then "P" else "F"
                ));
            ("", "sr br", ip_string);
            ] @ (if Geoip.active () then [(cn, "sr br", CommonPictures.flag_html cc)] else []) @ [
            ("", "sr ar", (size_of_int64 c.client_total_uploaded));
            ("", "sr ar br", (size_of_int64 c.client_total_downloaded));
            ("", "sr ar", (size_of_int64 c.client_session_uploaded));
            ("", "sr ar br", (size_of_int64 c.client_session_downloaded));
            ("", "sr ar", Printf.sprintf "%d" c.client_rank);
            ("", "sr ar br", Printf.sprintf "%d" c.client_source.DonkeySources.source_score);
            ("", "sr ar br", (string_of_date (c.client_source.DonkeySources.source_age)));
            ("", "sr ar", req_score);
            ("", "sr ar", req_queue);
            ("", "sr ar br", req_min);
            ("", "sr ar", (if client_has_a_slot ac then "T" else "F"));
            ("", "sr ar br", (if c.client_banned then "T" else "F"));
            ("", "sr ar", Printf.sprintf "%d" c.client_requests_sent);
            ("", "sr ar", Printf.sprintf "%d" c.client_requests_received);
            ("", "sr ar br", Printf.sprintf "%d" (((last_time ()) - c.client_connect_time) / 60));
            ("", "sr br", (Md4.to_string c.client_md4)); ]);

          Printf.bprintf buf "\\<td class=\\\"sr \\\"\\>";

          ( let qfiles = c.client_file_queue in
            if qfiles <> [] then begin
                try
                  let _, qchunks,_ = List.find (fun (qfile, _,_) ->
                        qfile == file) qfiles in
                  let tc = ref 0 in
                  let arr = 
                    VerificationBitmap.init (Bitv.length qchunks) (fun i -> 
                    if Bitv.get qchunks i then begin
                      incr tc;
                      VerificationBitmap.State_complete
                    end else VerificationBitmap.State_missing) in
                  Printf.bprintf buf "%s\\</td\\>\\<td class=\\\"sr ar\\\"\\>%d\\</td\\>"
                    (CommonFile.colored_chunks arr) !tc;
                with Not_found -> (
                      Printf.bprintf buf "\\</td\\>\\<td class=\\\"sr ar\\\"\\>\\</td\\>"
                    );
              end
            else
              Printf.bprintf buf "\\</td\\>\\<td class=\\\"sr ar\\\"\\>\\</td\\>"
          );

           Printf.bprintf buf "\\</tr\\>";
        with _ -> ()

      ) (List.sort (fun (s1,c1) (s2,c2) -> compare (client_num c1) (client_num c2)) !sources_list);


    Printf.bprintf buf "\\</table\\>\\</div\\>\\<br\\>";

   end;

  );
  file_ops.op_file_cancel <- (fun file ->
      CommonSwarming.remove_swarmer file.file_swarmer;
      file.file_swarmer <- None;
      Hashtbl.remove files_by_md4 file.file_md4;
      current_files := List2.removeq file !current_files;
      DonkeySources.remove_file_sources_manager file.file_sources;
      if !!keep_cancelled_in_old_files &&
        not (List.mem file.file_md4 !!old_files) then
        old_files =:= file.file_md4 :: !!old_files;
      DonkeyProtoOvernet.Overnet.cancel_recover_file file;
      DonkeyProtoKademlia.Kademlia.cancel_recover_file file;
  );
  file_ops.op_file_files <- (fun file impl ->
      match file.file_swarmer with
        None -> [CommonFile.as_file impl]
      | Some swarmer ->
          CommonSwarming.subfiles swarmer)

let try_recover_temp_file filename md4 =
  try
    ignore (Hashtbl.find files_by_md4 md4)
  with Not_found ->
      let user = CommonUserDb.admin_user () in
      let file_diskname = Filename.concat !!temp_directory filename in
      let size = Unix32.getsize file_diskname in
      if size <> zero then
        begin
          ignore (really_query_download (Md4.to_string md4) size md4 None (Some file_diskname) None user user.user_default_group);
          recover_md4s md4
        end

let _ =
  network.op_network_extend_search <- (fun s e ->
      match e with
      | ExtendSearchLocally ->
(* TODO RESULT          DonkeyIndexer.find s      *) ()
      | ExtendSearchRemotely ->
          DonkeyUdp.make_xs s
  );

  network.op_network_clean_servers <- (fun _ ->
      DonkeyServers.remove_old_servers ());

  network.op_network_connect_servers <- (fun _ ->
      force_check_server_connections true);

(* TODO RESULT *)
  network.op_network_recover_temp <- (fun _ ->
      let files = Unix2.list_directory !!temp_directory in
      List.iter (fun filename ->
          let uid =
            try Uid.of_string filename
            with _ -> Uid.no
          in
          match Uid.to_uid uid with
          | Ed2k md4 ->
              (try
                 try_recover_temp_file filename md4
               with e ->
                 lprintf_nl "exception %s in recover_temp"
                 (Printexc2.to_string e);
              )
          | NoUid ->
              (if String.length filename = 32 then
                 try
                   let md4 = Md4.of_string filename in
                   try_recover_temp_file filename md4
                 with e ->
                   lprintf_nl "exception %s in recover_temp"
                     (Printexc2.to_string e);
              )
          | _ -> ()
      ) files
  );

  network.op_network_parse_url <- parse_donkey_url;
  network.op_network_reset <- (fun _ -> ());

  network.op_network_close_search <- (fun s -> ());
  network.op_network_check_upload_slots <- (fun _ -> ());
  network.op_network_porttest_start <- (fun _ ->
    CommonInteractive.run_porttest porttest_result ~tcp:(!!donkey_port) ~udp:(!!donkey_port+4)
  );
  network.op_network_forget_search <- forget_search

(* emule<->mldonkey disconnects during chat, and this doesn't seem to auto reconnect
when sending a message? emule or ml problem? *)
let _ =
  client_ops.op_client_say <- (fun c s ->
      match c.client_source.DonkeySources.source_sock with
      | NoConnection ->
          DonkeyClient.reconnect_client c;
          c.client_pending_messages <- c.client_pending_messages @ [s];
      | ConnectionWaiting _ ->
          c.client_pending_messages <- c.client_pending_messages @ [s];
      | Connection sock ->
          client_send c (DonkeyProtoClient.SayReq s)
  );
  client_ops.op_client_files <- (fun c ->
      match c.client_all_files with
        None ->  []
      | Some files ->
          List2.tail_map (fun r -> "", r) files);
  client_ops.op_client_browse <- (fun c immediate ->
      if !verbose then lprintf_nl "connecting friend %s" (full_client_identifier c);
      match c.client_source.DonkeySources.source_sock with
      | Connection sock when c.client_emule_proto.emule_noviewshared <> 1 ->
          if !verbose then lprintf_nl "retrieving filelist from friend %s" (full_client_identifier c);
          client_send c (
            let module M = DonkeyProtoClient in
            let module C = M.ViewFiles in
            M.ViewFilesReq C.t);
      | NoConnection when c.client_emule_proto.emule_noviewshared <> 1 ->
          if !verbose then lprintf_nl "re-connecting friend %s"
            (full_client_identifier c);
          set_must_browse (as_client c);
          reconnect_client c
      | _ -> ()
  );
  client_ops.op_client_connect <- (fun c ->
      match c.client_source.DonkeySources.source_sock with
        NoConnection ->  reconnect_client c
      | _ -> ()
  );
  client_ops.op_client_disconnect <- (fun c ->
      DonkeyClient.disconnect_client c Closed_by_user
  );
  client_ops.op_client_clear_files <- (fun c ->
      c.client_all_files <- None;
  );

  client_ops.op_client_bprint <- (fun c buf ->
      Printf.bprintf buf "\t\t%s (last_ok <%s>)\n"
        c.client_name
        (string_of_date (c.client_source.DonkeySources.source_age))
  );
  client_ops.op_client_print_info <- (fun c o ->
      let buf = o.conn_buf in
      let ip_string,cc,cn = get_ips_cc_cn c in

      Printf.bprintf buf "Client %d: %s\n"
        (client_num c)
        (full_client_identifier c);
      (
      match c.client_osinfo with
      | Some i -> Printf.bprintf buf "  osinfo: %s\n" i
      | None -> ()
      );
      Printf.bprintf buf "  state: %s, rank: %d\n"
        (string_of_connection_state (client_state c)) c.client_rank;
      if Geoip.active () then Printf.bprintf buf "  country: %s: %s\n" cc cn;
      Printf.bprintf buf "  MD4: %s\n" (Md4.to_string c.client_md4);
      Printf.bprintf buf "  downloaded\n";
      Printf.bprintf buf "   - session %s\n" (size_of_int64 c.client_session_downloaded);
      Printf.bprintf buf "   - total   %s\n" (size_of_int64 c.client_total_downloaded);
      (
      match c.client_download with
      | Some (f,_) -> Printf.bprintf buf "  downloading file %s\n" (file_best_name f)
      | None -> Printf.bprintf buf "  not downloading\n"
      );
      Printf.bprintf buf "  uploaded\n";
      Printf.bprintf buf "   - session %s\n" (size_of_int64 c.client_session_uploaded);
      Printf.bprintf buf "   - total   %s\n" (size_of_int64 c.client_total_uploaded);
      (
      match c.client_upload with
      | Some u -> Printf.bprintf buf "  uploading file %s\n" (file_best_name u.up_file)
      | _ -> Printf.bprintf buf "  not uploading\n"
      );
      Printf.bprintf buf "  SUI %s\n" (
        match c.client_sui_verified with
        | None -> "not supported"
        | Some b -> if b then "passed" else "failed"
        );
      Printf.bprintf buf "  kind: %s\n" (
        match c.client_kind with
        | Direct_address (ip,port) ->
                    Printf.sprintf "highID %s:%d" (Ip.to_string ip) port
        | Indirect_address (server_ip, server_port, id, port, real_ip) ->
                    Printf.sprintf "lowID %s:%d, server %s:%d"
                      (Ip.to_string real_ip) port (Ip.to_string server_ip) server_port
        | Invalid_address (name,md4) -> Printf.sprintf "invalid"
        );
      if c.client_emule_proto.received_miscoptions1 then
        Printf.bprintf buf "\nmiscoptions1:\n%s" (DonkeyProtoClient.print_emule_proto_miscoptions1 c.client_emule_proto)
      else
        Printf.bprintf buf "no miscoptions1 received\n";
      if c.client_emule_proto.received_miscoptions2 then
        Printf.bprintf buf "miscoptions2:\n%s" (DonkeyProtoClient.print_emule_proto_miscoptions2 c.client_emule_proto)
      else
        Printf.bprintf buf "no miscoptions2 received\n";
      List.iter (fun (file,_,_) -> Printf.bprintf buf "\nQueue: %s" (file_best_name file)) c.client_file_queue;
      List.iter (fun r ->
        Printf.bprintf buf "\nSource file: %s, score %d, request time %d"
          (CommonFile.file_best_name (r.DonkeySources.request_file.DonkeySources.manager_file ()))
          r.DonkeySources.request_score
          r.DonkeySources.request_time;
       ) c.client_source.DonkeySources.source_files;
  );
  client_ops.op_client_dprint <- (fun c o file ->
      let info = file_info file in
      let buf = o.conn_buf in

      try

        (match c.client_download with
            None -> ()
          | Some _ ->  (
                let qfiles = c.client_file_queue in
                let (qfile, qchunks,_) =  List.hd qfiles in
                if (qfile == (as_file_impl file).impl_file_val) then begin
            Printf.bprintf buf "[Donkey%6d] Name  : %-27s IP   : %-20s"
              (client_num c)
              (shorten c.client_name 20)
              (match c.client_kind with
                  Direct_address (ip,port) -> (Ip.to_string ip)
                  |  _ -> (string_of_client_ip c));
                    Printf.bprintf buf "\n%14sDown  : %-10s                  Uploaded: %-10s  Ratio: %s%1.1f (%s)\n" ""
                    (Int64.to_string c.client_total_downloaded)
                    (Int64.to_string c.client_total_uploaded)
                    (if c.client_total_downloaded > c.client_total_uploaded then "-" else "+")
                    (if c.client_total_uploaded > Int64.zero then (Int64.to_float (Int64.div c.client_total_downloaded c.client_total_uploaded)) else (1.))
                    (brand_to_string c.client_brand);
                    (Printf.bprintf buf "%14sFile  : %s\n" "" info.GuiTypes.file_name);
                  end;

              )

        )

      with _ -> ()

  );

  client_ops.op_client_dprint_html <- (fun c o file str ->
      let info = file_info file in
      let buf = o.conn_buf in
      try
        (match c.client_download with
            None -> false
          | Some _ ->  (
                let qfiles = c.client_file_queue in
                let (qfile, qchunks,_) =  List.hd qfiles in
                if (qfile == (as_file_impl file).impl_file_val) then begin

                    Printf.bprintf buf " \\<tr onMouseOver=\\\"mOvr(this);\\\" onMouseOut=\\\"mOut(this);\\\"
      class=\\\"%s\\\"\\> \\<td title=\\\"Add as friend\\\" class=\\\"srb ar\\\"
      onClick=\\\"parent.fstatus.location.href='submit?q=friend_add+%d'\\\"\\>%d\\</TD\\>"
                      str (client_num c) (client_num c);

                    let ip_string,cc,cn = get_ips_cc_cn c in

                    html_mods_td buf ([
                      (string_of_connection_state (client_state c), "sr",
                        short_string_of_connection_state (client_state c));
                      (Md4.to_string c.client_md4, "sr", client_short_name c.client_name);
                      ("", "sr", brand_to_string_short c.client_brand);
                      ("", "sr", c.client_emule_proto.emule_release);
                      ] @
                      (if !!emule_mods_count then [("", "sr", brand_mod_to_string_short c.client_brand_mod)] else [])
                      @ [
                        ("", "sr", (if DonkeySources.source_brand c.client_source
                               then "T" else "F"));
                      ("", "sr ar", Printf.sprintf "%d" (((last_time ()) - c.client_connect_time) / 60));
                      ("", "sr", (match c.client_kind with
                            | Direct_address (ip,port) -> Printf.sprintf "D"
                            | _ -> Printf.sprintf "I"
                          ));
                      ("", "sr", (match c.client_sui_verified with
                        | None -> "N"
                        | Some b -> if b then "P" else "F"
                      )); 
                      ("", "sr", ip_string);
                      ] @ (if Geoip.active () then [(cn, "sr", CommonPictures.flag_html cc)] else []) @ [
                      ("", "sr ar", (size_of_int64 c.client_total_uploaded));
                      ("", "sr ar", (size_of_int64 c.client_total_downloaded));
                      ("", "sr ar", (size_of_int64 c.client_session_uploaded));
                      ("", "sr ar", (size_of_int64 c.client_session_downloaded));
                      ("", "sr", info.GuiTypes.file_name) ]);

                    Printf.bprintf buf "\\</tr\\>";
                    true
                  end
                else false;
              )
        )
      with _ -> false;
  )

let _ =
  user_ops.op_user_set_friend <- (fun u ->
      let s = u.user_server in
      DonkeyUdp.add_user_friend s u
  )

let _ =
  shared_ops.op_shared_state <- (fun f o ->
    match file_state f with
    | FileShared ->
      (match file_shared (as_file f) with
      | None -> "no file_shared info"
      | Some f ->
            let pre_share1_dir =
              String2.replace (Filename2.dirname (as_shared_impl f).impl_shared_fullname) '\\' "/" in
            let pre_share2_dir =
              try       
                String2.after pre_share1_dir
                (String2.search_from
                (Filename2.dirname (as_shared_impl f).impl_shared_fullname) 0 (Sys.getcwd ()) +
                String.length (Sys.getcwd ()))
              with Not_found -> pre_share1_dir
            in
            let dir =
              if String2.check_prefix pre_share2_dir "/" then String2.after pre_share2_dir 1 else pre_share2_dir in
            if o.conn_output = HTML then
              Printf.sprintf "\\<a href=\\\"submit?q=debug_dir+%s\\\"\\>%s\\</a\\>" (Http_server.html_real_escaped dir) (Http_server.html_real_escaped dir)
            else Printf.sprintf "Shared in %s" dir)
    | state -> string_of_state state
  );
  shared_ops.op_shared_unshare <- (fun file ->
      unshare_file file;
(* Should we or not ??? *)
      if file_state file = FileShared then
        try Hashtbl.remove files_by_md4 file.file_md4 with _ -> ();
  );
  shared_ops.op_shared_info <- (fun file ->
   let module T = GuiTypes in
     match file.file_shared with
        None -> assert false
      | Some impl ->
          { (impl_shared_info impl) with
            T.shared_network = network.network_num;
            T.shared_filename = file_best_name file;
            T.shared_uids = [Uid.create (Ed2k file.file_md4)];
            }
  );
  pre_shared_ops.op_shared_info <- (fun s ->
      let module T = GuiTypes in
      let impl = s.shared_shared in
      { (impl_shared_info impl) with
        T.shared_network = network.network_num }
  )

let _ =
  CommonWeb.add_web_kind "server.met" "List of donkey servers" 
    (fun url filename ->
    if !!enable_donkey && !!update_server_list_server_met then
      begin
        lprintf_nl "server.met loaded from %s" url;
        let s = unpack_server_met filename url in
        let nservers = List.length (Hashtbl2.to_list servers_by_key) in
        let n = load_server_met s in
        if s <> filename then Sys.remove s;
        lprintf_nl "%d servers found, %d new ones inserted"
          n ((List.length (Hashtbl2.to_list servers_by_key)) - nservers)
      end
    else
      if not !!enable_donkey then
        lprintf_nl "eDonkey module is disabled, ignoring..."
      else
        lprintf_nl "ED2K-update_server_list_server_met is disabled, ignoring..."
  );

  file_ops.op_file_proposed_filenames <- op_file_proposed_filenames;
  network.op_network_add_server <- (fun ip port ->
      let s = DonkeyComplexOptions.force_add_server (Ip.ip_of_addr ip) port in
      as_server s.server_server
  )
