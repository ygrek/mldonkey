(* Copyright 2001, 2002 b52_simon :), b8_bavard, b8_fee_carabine, INRIA *)
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
open Printf2
open Md4
open BasicSocket
  
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
open CommonSwarming
open CommonInteractive
  
open BTTypes
open BTOptions
open BTGlobals
open BTComplexOptions
open BTProtocol
  
open Gettext  
let _s x = _s "BTInteractive" x
let _b x = _b "BTInteractive" x  

let _ =
  network.op_network_connected <- (fun _ -> true)

let file_num file =
  file.file_file.impl_file_num

let _ =
  file_ops.op_file_sources <- (fun file ->
(*      lprintf "file_sources\n"; *)
      let list = ref [] in
      Hashtbl.iter (fun _ c ->
          list := (as_client c) :: !list
      ) file.file_clients;
      !list
  );
  file_ops.op_file_debug <- (fun file ->
      let buf = Buffer.create 100 in
(*      Int64Swarmer.debug_print buf file.file_swarmer; *)
      Hashtbl.iter (fun _ c ->
          Printf.bprintf buf "Client %d: %s\n" (client_num c)
          (match c.client_sock with
              NoConnection -> "No Connection"
            | Connection _  -> "Connected"
            | ConnectionWaiting _ -> "Waiting for Connection"
          )
      ) file.file_clients;
      Buffer.contents buf
  );
  file_ops.op_file_commit <- (fun file new_name ->
      if not (List.mem (file.file_name, file_size file) !!old_files) then
        begin
          old_files =:= (file.file_name, file_size file) :: !!old_files;
          set_file_state file FileShared;
(*          try Unix32.rename (file_fd file) (new_name) with _ -> () *)
        end	
  )
  
  
module P = GuiTypes
  
let _ =
  file_ops.op_file_cancel <- (fun file ->
      remove_file file;
      BTClients.disconnect_clients file;
(*
      (try  Unix32.remove (file_fd file)  with e -> ());
      file_cancel (as_file file.file_file);
*)
  );
  file_ops.op_file_info <- (fun file ->
      {
        P.file_comment = file_comment (as_file file.file_file);
        P.file_name = file.file_name;
        P.file_num = (file_num file);
        P.file_network = network.network_num;
        P.file_names = [file.file_name, P.noips()];
        P.file_md4 = Md4.null;
        P.file_size = file_size file;
        P.file_downloaded = file_downloaded file;
        P.file_nlocations = 0;
        P.file_nclients = 0;
        P.file_state = file_state file;
        P.file_sources = None;
        P.file_download_rate = file_download_rate file.file_file;
        P.file_chunks = Int64Swarmer.verified_bitmap file.file_swarmer;
        P.file_availability = 
        [network.network_num,Int64Swarmer.availability file.file_swarmer];
        P.file_format = FormatNotComputed 0;
        P.file_chunks_age = [|0|];
        P.file_age = file_age file;
        P.file_last_seen = BasicSocket.last_time ();
        P.file_priority = file_priority (as_file file.file_file);
        P.file_uids = [];
      }    
  )

module C = CommonTypes
            
open Bencode
  
let load_torrent_file filename =
  let s = File.to_string filename in  
  
  let download_filename = Filename.concat downloads_directory
      (Filename.basename filename) in
  File.from_string download_filename s;
  
  let file_id, torrent = BTTracker.decode_torrent s in
  let file = new_download file_id torrent.torrent_name 
      torrent.torrent_length 
      torrent.torrent_announce torrent.torrent_piece_size 
      torrent.torrent_files FileDownloading
  in
  file.file_files <- torrent.torrent_files;
  file.file_chunks <- torrent.torrent_pieces;
  BTClients.get_sources_from_tracker file torrent.torrent_announce;
  ()


let try_share_file filename =
  lprintf "BTInteractive.try_share_file: %s\n" filename;
  let s = File.to_string filename in  
  let file_id, torrent = BTTracker.decode_torrent s in
  
  let filename = Filename.concat !!incoming_directory torrent.torrent_name in
  if Sys.file_exists filename then 
    let file_u = 
      if torrent.torrent_files <> [] then
        Unix32.create_multifile filename
          [Unix.O_RDWR; Unix.O_CREAT] 0o666 torrent.torrent_files
      else
        Unix32.create_rw filename
    in
    lprintf "Sharing file %s\n" filename;
    
    
    let file = new_file file_id torrent.torrent_name 
        torrent.torrent_length 
        torrent.torrent_announce torrent.torrent_piece_size 
        torrent.torrent_files filename FileShared
    in

    let verified = Int64Swarmer.verified_bitmap file.file_swarmer in
    let verified = String.make (String.length verified) '3' in
    Int64Swarmer.set_verified_bitmap file.file_swarmer verified;

    lprintf "......\n";
    file.file_files <- torrent.torrent_files;
    file.file_chunks <- torrent.torrent_pieces;
    BTClients.connect_tracker file torrent.torrent_announce "completed" 
      (fun _ -> ())
    
(* Call one minute after start, and then every 20 minutes. Should 
  automatically contact the tracker. *)    
let share_files _ =
  lprintf "BTInteractive.share_files\n";
  List.iter (fun dir ->
      let filenames = Unix2.list_directory dir in
      List.iter (fun file ->
          let filename = Filename.concat dir file in
          try_share_file filename
      ) filenames
  ) [seeded_directory; tracked_directory]
    
  
let _ =
  network.op_network_parse_url <- (fun url ->
      let ext = String.lowercase (Filename2.last_extension url) in
      lprintf "Last extension: %s\n" ext;
      if ext = ".torrent" || ext = ".tor" then
        try
          lprintf "Trying to load %s\n" url;
          load_torrent_file url;
          true
        with e ->
            lprintf "Exception %s while loading\n" (Printexc2.to_string e);
            let module H = Http_client in
            let r = {
                H.basic_request with
                H.req_url = Url.of_string url;
                H.req_proxy = !CommonOptions.http_proxy;
                H.req_user_agent = 
                Printf.sprintf "MLdonkey %s" Autoconf.current_version;
              } in
            
            H.wget r load_torrent_file;
            lprintf "wget started\n";
            
            true
      else
        false
  )
  
let _ = (
    client_ops.op_client_info <- (fun c ->
        let (ip,port) = c.client_host in
        let id = c.client_uid in
        {
          P.client_network = network.network_num;
          P.client_kind = Known_location (ip,port);
          P.client_state = client_state (as_client c);
          P.client_type = client_type c;
          P.client_tags = [];
          P.client_name = 
          (Printf.sprintf "%s:%d" (Ip.to_string ip) port);
          P.client_files = None;
          P.client_num = (client_num c);
          P.client_rating = 0;
          P.client_chat_port = 0 ;
          P.client_connect_time = BasicSocket.last_time ();
          P.client_software = "";
          P.client_emulemod = "";
          P.client_downloaded = c.client_downloaded;
          P.client_uploaded = c.client_uploaded;
          P.client_upload = None;
(*          P.client_sock_addr = (Ip.to_string ip); *)
        }
    );
    client_ops.op_client_connect <- (fun c ->
        BTClients.connect_client c   
    );
    client_ops.op_client_disconnect <- (fun c ->
        BTClients.disconnect_client c Closed_by_user
    );
    client_ops.op_client_bprint <- (fun c buf ->
        let cc = as_client c in
        let cinfo = client_info cc in
        Printf.bprintf buf "%s (%s)\n" 
          cinfo.GuiTypes.client_name
          (Sha1.to_string c.client_uid)
    );
    client_ops.op_client_bprint_html <- (fun c buf file ->
        
        html_mods_td buf [
          ("", "sr br ar", Printf.sprintf "%d" (client_num c));
          ("", "sr br", (Sha1.to_string c.client_uid));
          ("", "sr", (Ip.to_string (fst c.client_host)));
          ("", "sr br ar", Printf.sprintf "%d" (snd c.client_host));
          ("", "sr ar", (size_of_int64 c.client_uploaded));
          ("", "sr ar br", (size_of_int64 c.client_downloaded));
          ("", "sr", (if c.client_interested then "T" else "F"));
          ("", "sr", (if c.client_choked then "T" else "F"));
          ("", "sr br ar", (Int64.to_string c.client_allowed_to_write));
(* This is way too slow for 1000's of chunks on a page with 100's of sources 
		("", "sr", (CommonFile.colored_chunks (Array.init (String.length c.client_bitmap)
       (fun i -> (if c.client_bitmap.[i] = '1' then 2 else 0)) )) );
*)
          ("", "sr ar", (let fc = ref 0 in 
              (String.iter (fun s -> if s = '1' then incr fc) c.client_bitmap );
              (Printf.sprintf "%d" !fc) ) ) ];
    );
    client_ops.op_client_dprint <- (fun c o file ->
        let info = file_info file in
        let buf = o.conn_buf in
        let cc = as_client c in
        let cinfo = client_info cc in
        client_print cc o;
        Printf.bprintf buf (_b "\n%18sDown  : %-10s                  Uploaded: %-10s  Ratio: %s%1.1f (%s)\n") ""
        (Int64.to_string c.client_downloaded)
        (Int64.to_string c.client_uploaded)
        (if c.client_downloaded > c.client_uploaded then "-" else "+")
        (if c.client_uploaded > Int64.zero then (Int64.to_float (Int64.div c.client_downloaded c.client_uploaded)) else (1.))
        ("BT");
        (Printf.bprintf buf (_b "%18sFile  : %s\n") "" info.GuiTypes.file_name);
    );
    client_ops.op_client_dprint_html <- (fun c o file str ->
        let info = file_info file in
        let buf = o.conn_buf in
        let cc = as_client c in
        let cinfo = client_info cc in
        Printf.bprintf buf " \\<tr onMouseOver=\\\"mOvr(this);\\\"
	onMouseOut=\\\"mOut(this);\\\" class=\\\"%s\\\"\\>" str;
        
        html_mods_td buf [
          ("", "srb ar", Printf.sprintf "%d" (client_num c));
          ((string_of_connection_state (client_state cc)), "sr", 
            (short_string_of_connection_state (client_state cc)));
          ((Sha1.to_string c.client_uid), "sr", cinfo.GuiTypes.client_name);
          ("", "sr", "bT"); (* cinfo.GuiTypes.client_software *)
          ("", "sr", "F");
          ("", "sr ar", Printf.sprintf "%d" 
              (((last_time ()) - cinfo.GuiTypes.client_connect_time) / 60));
          ("", "sr", "D");
          ("", "sr", (Ip.to_string (fst c.client_host)));
          ("", "sr ar", (size_of_int64 c.client_uploaded));
          ("", "sr ar", (size_of_int64 c.client_downloaded));
          ("", "sr", info.GuiTypes.file_name); ];
        true
    )
)

  
let _ =
  CommonNetwork.register_commands 
    [
    "compute_torrent", "Network/Bittorrent", Arg_one (fun filename o ->
        let announce = Printf.sprintf "http://%s:%d/tracker"
            (Ip.to_string (CommonOptions.client_ip None)) !!tracker_port in
        
        let basename = Filename.basename filename in
        let torrent = Filename.concat tracked_directory 
            (Printf.sprintf "%s.torrent" basename)
        in
        lprintf "1\n";
        BTTracker.generate_torrent announce torrent filename;
        lprintf "2\n";
        BTTracker.scan_tracked_directory ();
        lprintf "3\n";
        try_share_file torrent;
        lprintf "4\n";
        ".torrent file generated"
    ), _s " <filename> : generate the corresponding <filename>.torrent file
in torrents/tracked/. The file is automatically tracked, and seeded if
    in incoming/";
    
    "torrents", "Network/Bittorrent", Arg_none (fun o ->
        
        if !!tracker_port <> 0 then
          begin          
            Printf.bprintf o.conn_buf (_b ".torrent files available:\n");
            let files = Unix2.list_directory tracked_directory in
            List.iter (fun file ->
                Printf.bprintf o.conn_buf "http://%s:%d/%s\n"
                  (Ip.to_string (CommonOptions.client_ip None)) !!tracker_port
                  file
            ) files;
            _s "done"
          end
        else
          _s "Tracker not activated (tracker_port = 0)"
    ), _s " : print all .torrent files on this server";
    
    (*
    "print_torrent", Arg_one (fun filename o ->

        ".torrent file printed"
    ), " <filename.torrent> : print the content of filename"
*)    
        
  ]
  