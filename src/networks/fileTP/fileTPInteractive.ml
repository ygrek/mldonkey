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

open CommonDownloads
open Int64ops
open Xml
open Printf2
open Md4
open CommonSearch
open CommonGlobals
open CommonUser
open CommonClient
open CommonOptions
open CommonServer
open CommonResult
open CommonTypes
open CommonComplexOptions
open CommonFile
open CommonInteractive
open Options
open FileTPTypes
open FileTPOptions
open FileTPGlobals
open FileTPComplexOptions
open BasicSocket
open Autoconf

open FileTPProtocol

open Gettext
let _s x = _s "FileTPInteractive" x
let _b x = _b "FileTPInteractive" x

let _ =
  network.op_network_connected <- (fun _ -> true);
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

let _ =
  file_ops.op_file_cancel <- (fun file ->
      remove_file file;
  );
  file_ops.op_file_info <- (fun file ->
      {
        P.file_fields = P.Fields_file_info.all;
        P.file_comment = file_comment (as_file file);
        P.file_name = file_best_name file;
        P.file_num = (file_num file);
        P.file_network = network.network_num;
        P.file_names = file.file_filenames;
        P.file_md4 = Md4.null;
        P.file_size = file_size file;
        P.file_downloaded = file_downloaded file;
        P.file_all_sources = 0;
        P.file_active_sources = 0;
        P.file_state = file_state file;
        P.file_sources = None;
        P.file_download_rate = file_download_rate file.file_file;
        P.file_chunks = (match file.file_swarmer with
          None -> "" | Some swarmer ->
            Int64Swarmer.verified_bitmap swarmer);
        P.file_availability =
        [network.network_num,(match file.file_swarmer with
          None -> "" | Some swarmer ->
            Int64Swarmer.availability swarmer)];
        P.file_format = FormatNotComputed 0;
        P.file_chunks_age = [|0|];
        P.file_age = file_age file;
        P.file_last_seen = BasicSocket.last_time ();
        P.file_priority = file_priority (as_file file);
        P.file_uids = [Uid.create (FileTP file.file_id)];
      }
  )

module C = CommonTypes

let string_of_client_addr c = c.client_hostname

let _ =
  client_ops.op_client_info <- (fun c ->
      {
        P.client_network = network.network_num;
        P.client_kind = Known_location (Ip.from_name c.client_hostname,
          c.client_port);
        P.client_state = client_state (as_client c);
        P.client_type = client_type c;
        P.client_tags = [];
        P.client_name = (Printf.sprintf "%s:%d"
          c.client_hostname c.client_port);
        P.client_files = None;
        P.client_num = (client_num (as_client c));
        P.client_rating = 0;
        P.client_chat_port = 0 ;
        P.client_connect_time = BasicSocket.last_time ();
        P.client_software = "";
        P.client_release = "";
        P.client_emulemod = "";
        P.client_downloaded = zero;
        P.client_uploaded = zero;
        P.client_upload = None;
(*		P.client_sock_addr = (; *)
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
          "fT" (* cinfo.GuiTypes.client_software *)
          (Int64.to_string cinfo.GuiTypes.client_downloaded)
        (Int64.to_string cinfo.GuiTypes.client_uploaded);
        Printf.bprintf buf "\nfilename: %s\n\n" info.GuiTypes.file_name;
    );
    client_ops.op_client_dprint_html <- (fun c o file str ->
        let info = file_info file in
        let buf = o.conn_buf in
        let cc = as_client c in
        let cinfo = client_info cc in
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
          ("", "sr", "TP"); (* cinfo.GuiTypes.client_software *)
          ("", "sr", ""); (* cinfo.GuiTypes.client_release *)
          ] @
          (if !show_emulemods_column then [("", "sr", "")] else [])
          @ [
          ("", "sr", "F");
          ("", "sr ar", Printf.sprintf "%d"
              (((last_time ()) - cinfo.GuiTypes.client_connect_time) / 60));
          ("", "sr", "D");
          ("", "sr", (string_of_client_addr c));
          ("", "sr ar", (size_of_int64 cinfo.GuiTypes.client_uploaded));
          ("", "sr ar", (size_of_int64 cinfo.GuiTypes.client_downloaded));
          ("", "sr", info.GuiTypes.file_name); ]);
        true
    )

(* As in bittorrent: make an initial connection just to know the complete
  size of the file and disconnect immediatly after. *)

  (*
let rec start_download_file_from_mirror proto file url u result_size =
  (*
  lprintf "RECEIVED HEADERS\n";
  let content_length = ref None in
  List.iter (fun (name, content) ->
      if String.lowercase name = "content-length" then
        try
          content_length := Some (Int64.of_string content)
        with _ ->
            lprintf "bad content length [%s]\n" content;
  ) headers;
  match !content_length with
    None -> failwith "Unable to mirror download (HEAD failed)"
  | Some result_size -> *)
      lprintf "STARTING DOWNLOAD WITH SIZE %Ld\n" result_size;
      if file_size file <> result_size then
        lprintf "Unable to mirror download (files have different sizes)"
      else
      let client_hostname = url.Url.server in
      let client_port = url.Url.port in
      let c = new_client proto client_hostname client_port in
      add_download file c url.Url.full_file;
      FileTPClients.get_file_from_source c file;
      ()

let test_mirrors file urls =
  List.iter (fun url ->
      try
        let u = Url.of_string url in
        let proto = match u.Url.proto with
          | "http" -> FileTPHTTP.proto
          | "ftp" -> FileTPFTP.proto
          | "ssh" -> FileTPSSH.proto
          | s -> failwith
              (Printf.sprintf "Unknown URL protocol [%s]" s)
        in
        proto.proto_check_size u url
          (start_download_file_from_mirror proto file)
        (*
        let module H = Http_client in
        let r = {
            H.basic_request with
            H.req_url = u;
            H.req_proxy = !CommonOptions.http_proxy;
            H.req_request = H.HEAD;
            H.req_user_agent = user_agent;
          } in

H.whead r (start_download_file_from_mirror file u)
  *)
      with _ -> ()) urls

let
let download_file_from_mirror file url =
  test_mirrors file [url];
  find_mirrors file url
    *)



let rec download_file_from_mirror file u =

  let proto = match u.Url.proto with
    | "http" -> FileTPHTTP.proto
    | "ftp" -> FileTPFTP.proto
    | "ssh" -> FileTPSSH.proto
    | s -> failwith
        (Printf.sprintf "Unknown URL protocol [%s]" s)
  in

  let client_hostname = u.Url.server in
  let client_port = u.Url.port in
  let c = new_client proto client_hostname client_port in
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
                (name ^ suffix))) mirrors
        else
          iter2 mirrors tail
  in
  iter1 !!mirrors

let previous_url = ref ""

let download_file url =
  let u = Url.of_string url in

  if List.mem u !!old_files && !previous_url <> url then begin
      previous_url := url;
      failwith "URL already downloaded: repeat command again to force";
    end;

  let file = new_file (Md4.random ()) u.Url.full_file zero in

  lprintf "DOWNLOAD FILE %s\n" (file_best_name  file);
  if not (List.memq file !current_files) then begin
      current_files := file :: !current_files;
    end;

  download_file_from_mirror file u;
  find_mirrors file u

(* I think this is a real bad idea, we should check this by ensuring that the
   bt-url-handler is called first. *)
let is_http_torrent headers url =
  let ext = String.lowercase (Filename2.last_extension url) in
  if ext = ".torrent" || ext = ".tor"
     || (String2.contains headers "content-type application/x-bittorrent")
    then true
  else false

let get_regexp_int text r =
  ignore (Str.search_forward r text 0);
  let a = Str.group_beginning 1 in
  let b = Str.group_end 1 in
  int_of_string (String.sub text a (b - a))

let get_regexp_string text r =
  ignore (Str.search_forward r text 0);
  let a = Str.group_beginning 1 in
  let b = Str.group_end 1 in
    String.sub text a (b - a)

(* This is called when a dllink is entered into the console.
   It returns true if this file can be handled by fileTP,
   and false otherwise.
 *)
let rec op_network_parse_url url =
  if !verbose then lprintf "filetp.op_network_parse_url\n";
  let location_regexp = "Location: \\(.*\\)" in
  let real_url = get_regexp_string url (Str.regexp location_regexp) in
  if !verbose then lprintf "real url: %s\n" real_url;
  if (is_http_torrent url real_url) then false
  else
    if (String2.check_prefix real_url "http://") then (
      lprintf "http download\n";
      let length_regexp = "Content-Length: \\(.*\\)" in
       try let length = get_regexp_int url (Str.regexp length_regexp) in
         if (length > 0) then begin
           download_file real_url; true
         end
         else raise Not_found
       with Not_found -> lprintf "Unknown file length. Use a web browser\n"; false
    )
    else if (String2.check_prefix url "ftp://") || (String2.check_prefix url "ssh://") then (
      download_file url;
      true
    )
    else
      false

let _ =
  network.op_network_parse_url <- op_network_parse_url

open Queues
open GuiTypes

let commands = [
    "http", "Network/FileTP", Arg_one (fun arg o ->
        download_file arg;
        _s "download started"
    ), " <url> :\t\t\t\tstart downloading this URL";

    "mirror", "Network/FileTP", Arg_two (fun num url o ->
        try
          lprintf "MIRROR [%s] [%s]\n" num url;
          let u = Url.of_string url in

          if List.mem u !!old_files && !previous_url <> url then begin
              previous_url := url;
              failwith "URL already downloaded: repeat command again to force";
            end;
          let num = int_of_string num in
          Hashtbl.iter (fun _ file ->
              lprintf "COMPARE %d/%d\n" (file_num file) num;
              if file_num file = num then begin
                  lprintf "Try HEAD from mirror\n";

                  download_file_from_mirror file u;
                  find_mirrors file u;

                  raise Exit
                end
          ) files_by_uid;
          _s "file not found"
        with Exit -> _s "mirror added"
    ), " <num> <url> :\t\t\tadd url as mirror for num";
    ]

let _ =
  CommonNetwork.register_commands commands;
  (* Shut up "Network.share not implemented by FileTP" *)
  network.op_network_share <- (fun fullname codedname size -> ());
  (* Same with Network.search and Network.forget_search... *)
  network.op_network_search <- (fun ss buf -> ());
  network.op_network_forget_search <- (fun s -> ());
  (* and Network.recover_temp *)
  network.op_network_recover_temp <-
    (fun _ ->
     if !verbose_hidden_errors then
       lprintf "recover_temp is not implemented for FileTP.\n";
    );

