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
open CommonGlobals
open CommonClient
open CommonOptions
open CommonTypes
open CommonFile
open Options
open FileTPTypes
open FileTPOptions
open FileTPGlobals
open FileTPComplexOptions
open BasicSocket

open Gettext
let _s x = _s "FileTPInteractive" x
let _b x = _b "FileTPInteractive" x

module VB = VerificationBitmap

let _ =
  network.op_network_connected <- (fun _ -> true);
  network.op_network_ports <- (fun _ -> []);
  network.op_network_connected_servers <- (fun _ -> [])


let file_num file =
  file.file_file.impl_file_num

let _ =
  file_ops.op_file_all_sources <- (fun file ->
(*      lprintf "file_sources\n";  *)
      List2.tail_map (fun c ->
          as_client c
      ) file.file_clients
  );
  file_ops.op_file_active_sources <- file_ops.op_file_all_sources

module P = GuiTypes


let clean_stop file =
      CommonSwarming.remove_swarmer file.file_swarmer;
      file.file_swarmer <- None;
  List.iter (fun c ->
    c.client_downloads <- List.filter 
      (fun cd -> cd.download_file.file_id <> file.file_id) c.client_downloads;
    if (List.length c.client_downloads) == 0 then begin
      FileTPClients.disconnect_client c Closed_by_user;
      client_remove c;
    end;
  ) file.file_clients

let _ =
  file_ops.op_file_cancel <- (fun file ->
      clean_stop file;
      remove_file file;
  );
  file_ops.op_file_info <- (fun file ->
      { (impl_file_info file.file_file) with
        P.file_fields = P.Fields_file_info.all;
        P.file_network = network.network_num;
        P.file_all_sources = (List.length file.file_clients);
        P.file_active_sources = (List.length file.file_clients);
        P.file_chunks = (match file.file_swarmer with
        | None -> None 
        | Some swarmer ->
            Some (CommonSwarming.chunks_verified_bitmap swarmer));
        P.file_chunk_size = (match file.file_swarmer with
        | None -> None 
        | Some t ->
            Some (List.map (fun t -> t.CommonSwarming.t_chunk_size) t.CommonSwarming.t_s.CommonSwarming.s_networks));
        P.file_availability =
        [network.network_num,(match file.file_swarmer with
          None -> "" | Some swarmer ->
            CommonSwarming.chunks_availability swarmer)];
        P.file_format = FormatNotComputed 0;
        P.file_uids = [Uid.create (FileTP file.file_id)];
      }
  )

module C = CommonTypes

let string_of_client_addr c = c.client_hostname

let _ =
  client_ops.op_client_info <- (fun c ->
      check_client_country_code c;
      { (impl_client_info c.client_client) with
        P.client_network = network.network_num;
        P.client_kind = Known_location (Ip.from_name c.client_hostname,
          c.client_port);
        P.client_country_code = c.client_country_code;
        P.client_state = client_state (as_client c);
        P.client_type = client_type c;
        P.client_name = (Printf.sprintf "%s:%d"
          c.client_hostname c.client_port);
        P.client_num = (client_num (as_client c));
        P.client_connect_time = BasicSocket.last_time ();
        P.client_software = c.client_software;
        P.client_total_downloaded = c.client_total_downloaded;
        P.client_session_downloaded = c.client_session_downloaded;
      }
  );
    client_ops.op_client_bprint <- (fun c buf ->
        let cc = as_client c in
        let cinfo = client_info cc in
        Printf.bprintf buf "%s (%s)\n"
          cinfo.GuiTypes.client_name
          (string_of_client_addr c)
    );
   client_ops.op_client_dprint <- (fun c o file ->
        let info = file_info file in
        let buf = o.conn_buf in
        let cc = as_client c in
        let cinfo = client_info cc in
        client_print cc o;
        Printf.bprintf buf "client: %s downloaded: %s uploaded: %s"
          cinfo.GuiTypes.client_software
          (Int64.to_string cinfo.GuiTypes.client_total_downloaded)
        (Int64.to_string cinfo.GuiTypes.client_total_uploaded);
        Printf.bprintf buf "\nfilename: %s\n\n" info.GuiTypes.file_name;
    );
    client_ops.op_client_dprint_html <- (fun c o file str ->
        let info = file_info file in
        let buf = o.conn_buf in
        let cc = as_client c in
        let cinfo = client_info cc in
        let client_ip, client_port =
          match cinfo.GuiTypes.client_kind with
            Indirect_location (_, _, ip, port)
          | Known_location (ip, port) -> ip, port
        in
        let ccode,cname =
          Geoip.get_country_code_name cinfo.GuiTypes.client_country_code
        in

        Printf.bprintf buf " \\<tr onMouseOver=\\\"mOvr(this);\\\"
    onMouseOut=\\\"mOut(this);\\\" class=\\\"%s\\\"\\>" str;

        let show_emulemods_column = ref false in
           if Autoconf.donkey = "yes" then begin
               if !!emule_mods_count then
                   show_emulemods_column := true
        end;

        html_mods_td buf ([
          ("", "srb ar", Printf.sprintf "%d" (client_num cc));
          ((string_of_connection_state (client_state cc)), "sr",
            (short_string_of_connection_state (client_state cc)));
          ("", "sr", cinfo.GuiTypes.client_name);
          ("", "sr", cinfo.GuiTypes.client_software);
          ("", "sr", cinfo.GuiTypes.client_release);
          ] @
          (if !show_emulemods_column then [("", "sr", "")] else [])
          @ [
          ("", "sr", "F");
          ("", "sr ar", Printf.sprintf "%d"
              (((last_time ()) - cinfo.GuiTypes.client_connect_time) / 60));
          ("", "sr", "D");
          ("", "sr", "N");
          ("", "sr", Printf.sprintf "%s:%d" (Ip.to_string client_ip) client_port);
          ] @ (if Geoip.active () then [(cname, "sr", CommonPictures.flag_html ccode)] else []) @ [
          ("", "sr ar", (size_of_int64 cinfo.GuiTypes.client_total_uploaded));
          ("", "sr ar", (size_of_int64 cinfo.GuiTypes.client_total_downloaded));
          ("", "sr ar", (size_of_int64 cinfo.GuiTypes.client_session_uploaded));
          ("", "sr ar", (size_of_int64 cinfo.GuiTypes.client_session_downloaded));
          ("", "sr", info.GuiTypes.file_name); ]);
        true
    )

let rec download_file_from_mirror file u referer =

  let proto = match u.Url.proto with
    | "http" -> FileTPHTTP.proto
    | "ftp" -> FileTPFTP.proto
    | "ssh" -> FileTPSSH.proto
    | s -> failwith
        (Printf.sprintf "Unknown URL protocol [%s]" s)
  in

  let client_hostname = u.Url.server in
  let client_port = u.Url.port in
  let c = new_client proto client_hostname client_port referer in
  add_download file c u;
  FileTPClients.get_file_from_source c file; 
  ()

and find_mirrors file u =
  let url = Url.to_string u in
  let urllen = String.length url in
  let rec iter1 list =
    match list with
      [] -> ()
    | list :: tail ->
        iter2 list list;
        iter1 tail

  and iter2 mirrors list =
    match list with
      [] -> ()
    | name :: tail ->
        let namelen = String.length name in
        if urllen > namelen &&
          String.sub url 0 namelen = name then
          let suffix = String.sub url namelen (urllen - namelen) in
          List.iter (fun name ->
              download_file_from_mirror file (Url.of_string
                (name ^ suffix)) name) mirrors
        else
          iter2 mirrors tail
  in
  iter1 !!mirrors

let previous_url = ref ""  
  
let download_file url referer user group =
  let u = Url.of_string url in

  if List.mem u !!old_files && !previous_url <> url then begin
      previous_url := url;
      failwith "URL already downloaded: repeat command again to force";
    end;

  let file = new_file (Md4.random ()) u.Url.full_file zero user group in
  
  if !verbose then
    lprintf_nl "Started new download: %s from %s" (file_best_name file) url; 
  if not (List.memq file !current_files) then begin
      current_files := file :: !current_files;
    end;
  
  CommonInteractive.start_download (as_file file);
  download_file_from_mirror file u referer;
  find_mirrors file u

(* I think this is a real bad idea, we should check this by ensuring that the
   bt-url-handler is called first. *)
let is_http_torrent headers url =
  let ext = String.lowercase (Filename2.last_extension url) in
  ext = ".torrent" || ext = ".tor"
     || (String2.contains headers "content-type application/x-bittorrent")

let get_regexp_int text r =
  ignore (Str.search_forward r text 0);
  let a = Str.group_beginning 1 in
  let b = Str.group_end 1 in
  Int64.of_string (String.sub text a (b - a))

let get_regexp_string text r =
  ignore (Str.search_forward r text 0);
  let a = Str.group_beginning 1 in
  let b = Str.group_end 1 in
    String.sub text a (b - a)

(* This is called when a dllink is entered into the console.
   It returns true if this file can be handled by fileTP,
   and false otherwise.
 *)
let op_network_parse_url url user group =
  let location_regexp = "Location: \\(.*\\)" in
  let real_url = get_regexp_string url (Str.regexp location_regexp) in
  if (is_http_torrent url real_url) && !!enable_bittorrent then
    "this seems to be a torrent, ignoring...", false
  else
    if (String2.check_prefix real_url "http://") then (
      let length_regexp = "Content-Length: \\(.*\\)" in
       try let length = get_regexp_int url (Str.regexp length_regexp) in
         if (length > 0L) then begin
           download_file real_url "" user group; "started FileTP download", true
         end
         else "can not parse Content-Length", false
       with Not_found -> 
           "Unknown file length. Use a web browser", false
    )
    else 
      if (String2.check_prefix real_url "ftp://") ||
         (String2.check_prefix real_url "ssh://") then (
      download_file real_url "" user group;
      "started FileTP download", true)
    else
      "invalid URL", false

let _ =
  network.op_network_parse_url <- op_network_parse_url

let commands = [
    "http", "Network/FileTP", Arg_multiple (fun args o ->
        try
        (match args with
          url :: [referer] -> download_file url referer o.conn_user.ui_user o.conn_user.ui_user.user_default_group
        | [url] -> download_file url "" o.conn_user.ui_user o.conn_user.ui_user.user_default_group
        | _ -> raise Not_found);
        let buf = o.conn_buf in
        if o.conn_output = HTML then
          html_mods_table_one_row buf "serversTable" "servers" [
            ("", "srh", "download started"); ]
        else
          Printf.bprintf buf "download started";
        _s ""
        with Not_found ->
            let buf = o.conn_buf in
            if o.conn_output = HTML then
              html_mods_table_one_row buf "serversTable" "servers" [
                ("", "srh", "Not enough parameters"); ]
            else
              Printf.bprintf buf "Not enough parameters";
            _s ""
        | e ->
            let error = Printexc2.to_string e in
            let buf = o.conn_buf in
            if o.conn_output = HTML then
              html_mods_table_one_row buf "serversTable" "servers" [
                ("", "srh", error); ]
            else
              Printf.bprintf buf "%s" error;
            _s ""
    ), "<url> <referer> :\t\t\tstart downloading this URL";

    "mirror", "Network/FileTP", Arg_multiple (fun args o ->
        try
          let num = ref "" in
          let url = ref "" in
          let referer = ref "" in
          (match args with
            nums :: urls :: [referers] -> num := nums; url := urls; referer := referers
          | nums :: [urls] -> num := nums; url := urls; referer := ""
          | _ -> raise Not_found);

          if !verbose then
            lprintf_nl "MIRROR [%s] [%s]" !num !url;
          let u = Url.of_string !url in

          if List.mem u !!old_files && !previous_url <> !url then begin
              previous_url := !url;
              failwith "URL already downloaded: repeat command again to force";
            end;
          let num = int_of_string !num in
          Hashtbl.iter (fun _ file ->
              if !verbose then
                lprintf_nl "COMPARE %d/%d" (file_num file) num;
              if file_num file = num then begin
                  if !verbose then
                    lprintf_nl "Try HEAD from mirror";


                  download_file_from_mirror file u !referer;
                  find_mirrors file u;

                  raise Exit
                end
          ) files_by_uid;
         let buf = o.conn_buf in
          if o.conn_output = HTML then
            html_mods_table_one_row buf "serversTable" "servers" [
              ("", "srh", "file not found"); ]
          else
            Printf.bprintf buf "file not found";
          _s ""
        with
        | Exit -> 
            let buf = o.conn_buf in
            if o.conn_output = HTML then
              html_mods_table_one_row buf "serversTable" "servers" [
                ("", "srh", "mirror added"); ]
            else
              Printf.bprintf buf "mirror added";
            _s ""
        | Not_found ->
            if !verbose then
              lprintf_nl "Not enough parameters";
            let buf = o.conn_buf in
            if o.conn_output = HTML then
              html_mods_table_one_row buf "serversTable" "servers" [
                ("", "srh", "Not enough parameters"); ]
            else
              Printf.bprintf buf "Not enough parameters";
            _s ""        
    ), "<num> <url> <referer> :\t\tadd URL as mirror for num";
    ]

let _ =
  CommonNetwork.register_commands commands;
  network.op_network_share <- (fun fullname codedname size -> ());
  network.op_network_search <- (fun ss buf -> ());
  network.op_network_download <- (fun r user group -> dummy_file);
  file_ops.op_file_commit <- (fun file new_name -> clean_stop file);
  file_ops.op_file_pause <- (fun file -> 
    List.iter (fun c ->
      match c.client_connected_for with
      | Some s when s.file_id = file.file_id -> 
          FileTPClients.disconnect_client c Closed_by_user;
      | _ -> ()
    ) file.file_clients
  );
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
  file_ops.op_file_queue <- file_ops.op_file_pause;
  file_ops.op_file_resume <- (fun file -> ());
  file_ops.op_file_print <- (fun file buf -> ());
  network.op_network_close_search <- (fun s -> ());
  network.op_network_forget_search <- (fun s -> ());
  network.op_network_connect_servers <- (fun s -> ());
  network.op_network_reset <- (fun _ -> ());
  network.op_network_porttest_result <- (fun _ -> PorttestNotAvailable);
  network.op_network_check_upload_slots <- (fun _ -> ());
  network.op_network_recover_temp <- (fun s -> ())
