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
open CommonSwarming
open CommonInteractive
open Options
open BTTypes
open BTOptions
open BTGlobals
open BTComplexOptions
open BasicSocket

open BTProtocol

let _ =
  network.op_network_connected <- (fun _ -> true)

let file_num file =
  file.file_file.impl_file_num

let _ =
  file_ops.op_file_sources <- (fun file ->
      lprintf "file_sources\n"; 
      let list = ref [] in
      Hashtbl.iter (fun _ c ->
          list := (as_client c) :: !list
      ) file.file_clients;
      !list
  );
  file_ops.op_file_debug <- (fun file ->
      let buf = Buffer.create 100 in
      Int64Swarmer.debug_print buf file.file_swarmer;
      Hashtbl.iter (fun _ c ->
          Printf.bprintf buf "Client %d: %s\n" (client_num c)
          (match c.client_sock with
              NoConnection -> "No Connection"
            | Connection _ | CompressedConnection _ -> "Connected"
            | ConnectionWaiting -> "Waiting for Connection"
            | ConnectionAborted -> "Connection Aborted"
          )
      ) file.file_clients;
      Buffer.contents buf
  );
  file_ops.op_file_commit <- (fun file new_name ->
      try
        if file.file_files <> [] then 
	  let base_dir_name = if String2.check_suffix new_name ".torrent" then
            String.sub new_name 0 ((String.length new_name) - 8)
	  else 
            new_name ^ ".d" in
          let bt_fd = Unix32.create_ro new_name in
          List.iter (fun (filename, begin_pos, end_pos) ->
              let filename = Filename.concat base_dir_name filename in
              lprintf "Would save file as %s\n" filename;
              let dirname = Filename.dirname filename in
              Unix2.safe_mkdir dirname;
              lprintf "Copying %Ld %Ld to 0\n"
                begin_pos (end_pos -- begin_pos);
              let fd = Unix32.create
                  filename [Unix.O_RDWR; Unix.O_CREAT] 0o666 in
              Unix32.copy_chunk bt_fd fd begin_pos zero 
                (Int64.to_int (end_pos -- begin_pos));
              Unix32.close fd
          ) file.file_files;
          Unix32.close bt_fd;
          if !!delete_original then Sys.remove new_name
      with e ->
          lprintf "Exception %s while commiting BitTorrent file"
            (Printexc.to_string e)
  ) 
  
  
module P = GuiTypes
  
let _ =
  file_ops.op_file_cancel <- (fun file ->
      remove_file file;
      BTClients.disconnect_clients file;
      (try  Unix32.remove (file_fd file)  with e -> ());
      file_cancel (as_file file.file_file);
  );
  file_ops.op_file_info <- (fun file ->
      {
        P.file_name = file.file_name;
        P.file_num = (file_num file);
        P.file_network = network.network_num;
        P.file_names = [file.file_name];
        P.file_md4 = Md4.null;
        P.file_size = file_size file;
        P.file_downloaded = file_downloaded file;
        P.file_nlocations = 0;
        P.file_nclients = 0;
        P.file_state = file_state file;
        P.file_sources = None;
        P.file_download_rate = file_download_rate file.file_file;
        P.file_chunks = Int64Swarmer.verified_bitmap file.file_partition;
        P.file_availability = 
        [network.network_num,Int64Swarmer.verified_bitmap file.file_partition];
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
  lprintf ".torrent file loaded\n";
  let s = File.to_string filename in
(*            lprintf "Loaded: %s\n" (String.escaped s); *)
  let v = Bencode.decode s in
(*            lprintf "Decoded file: %s\n" (Bencode.print v);  *)
  
  
  let announce = ref "" in
  let file_info = ref (List []) in
  let file_name = ref "" in
  let file_piece_size = ref zero in
  let file_pieces = ref "" in
  let length = ref zero in
  let file_files = ref [] in
  
  begin
    match v with
      Dictionary list ->
        List.iter (fun (key, value) ->
            match key, value with 
              String "announce", String tracker_url ->
                announce := tracker_url
            | String "info", ((Dictionary list) as info) ->
                
                file_info := info;
                List.iter (fun (key, value) ->
                    match key, value with
                    | String "files", List files ->
                        
                        let current_pos = ref zero in
                        List.iter (fun v ->
                            match v with
                              Dictionary list ->
                                let current_file = ref "" in
                                let current_length = ref zero in
                                
                                List.iter (fun (key, value) ->
                                    match key, value with
                                      String "path", List path ->
                                        current_file := 
                                        Filepath.path_to_string '/'
                                          (List.map (fun v ->
                                              match v with
                                                String s -> s
                                              | _ -> assert false
                                          ) path)
                                    
                                    | String "length", Int n ->
                                        length := !length ++ n;
                                        current_length := n;
                                    
                                    | String key, _ -> 
                                        lprintf "other field [%s] in files\n" key
                                    | _ -> 
                                        lprintf "other field in files\n"
                                ) list;
                                
                                assert (!current_length <> zero);
                                assert (!current_file <> "");
                                file_files := (!current_file, !current_pos, !current_pos ++ !current_length) :: !file_files;
                                current_pos := !current_pos ++ !current_length
                            
                            | _ -> assert false
                        ) files;
                    
                    | String "length", Int n -> 
                        length := n
                    | String "name", String name ->
                        file_name := name
                    | String "piece length", Int n ->
                        file_piece_size := n
                    | String "pieces", String pieces ->
(*                                  lprintf "Provided pieces: %d\n" 
                                    ((String.length pieces) / 20); *)
                        file_pieces := pieces
                    | String key, _ -> 
                        lprintf "other field [%s] in info\n" key
                    | _ -> 
                        lprintf "other field in info\n"
                ) list
            
            | String key, _ -> 
                lprintf "other field [%s] after info\n" key
            | _ -> 
                lprintf "other field after info\n"
        ) list
    | _ -> assert false
  end;
  
  assert (!announce <> "");
  assert (!file_name <> "");
  assert (!file_piece_size <> zero);
  assert (!file_pieces <> "");
  
  assert (!file_info = Bencode.decode (Bencode.encode !file_info));
  
  let file_id = Sha1.string (Bencode.encode !file_info) in
  let npieces = 
    1+ Int64.to_int ((!length -- one) // !file_piece_size)
  in
(*            lprintf "npieces %d length %Ld piece %Ld %d\n"
              npieces !length !file_piece_size (String.length !file_pieces); *)
  let pieces = Array.init npieces (fun i ->
        let s = String.sub !file_pieces (i*20) 20 in
        Sha1.direct_of_string s
    ) in
  
  if !file_files <> [] && not (String2.check_suffix !file_name ".torrent") then
    file_name := !file_name ^ ".torrent";

  let file = new_file file_id !file_name !length 
      !announce !file_piece_size
  in
  file.file_files <- !file_files;
  file.file_chunks <- pieces;
  BTClients.connect_tracker file !announce;
  ()
  
  
  
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
          P.client_downloaded = c.client_downloaded;
          P.client_uploaded = c.client_uploaded;
          P.client_upload = None;
          P.client_sock_addr = (Ip.to_string ip);
        }
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
        Printf.bprintf buf "client: %s downloaded: %s uploaded: %s"
          "bT" (* cinfo.GuiTypes.client_software *)
          (Int64.to_string c.client_downloaded)
        (Int64.to_string c.client_uploaded);
        Printf.bprintf buf "\nfilename: %s\n\n" info.GuiTypes.file_name;
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

  
