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
open Md4
open Options
open CommonOptions
open Printf2

open BasicSocket
  
open CommonGlobals
  
open BTOptions
open BTTypes
open BTGlobals
open Bencode

  
open Gettext  
let _s x = _s "BTTracker" x
let _b x = _b "BTTracker" x  

(* 

We could have a a-la-edonkey tracker: it would connect back to incoming
  client, and check whether they are accessible from the outside world,
and also check which chunks they have before sending them sources, so
that we can filter out immediatly sources that are not interesting for
them.
*)
  
(*

torrents/: for BitTorrent
  downloads/: .torrent files of current downloads
  tracked/: .torrent files of tracked downloads
    * If the file appears in incoming/, it is automatically seeded.
  seeded/:
     * If the file appears in incoming/, it is automatically seeded.

  *)
  
let decode_torrent s =
  lprintf ".torrent file loaded\n";
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
  
  let parse_files files =
    let current_pos = ref zero in
    List.iter (fun v ->
        match v with
          Dictionary list ->
            let current_file = ref "" in
            let current_length = ref zero in
            let length_set = ref false in
            
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
                    length_set := true
                
                | String key, _ -> 
                    lprintf "other field [%s] in files\n" key
                | _ -> 
                    lprintf "other field in files\n"
            ) list;
            
            assert (!length_set);
            assert (!current_file <> "");
            file_files := (!current_file, !current_length) :: !file_files;
            current_pos := !current_pos ++ !current_length
        
        | _ -> assert false
    ) files;
  in
  
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
                        parse_files files                    
                    | String "length", Int n -> 
                        length := n
                    | String "name", String name ->
                        file_name := name
                    | String "piece length", Int n ->
                        file_piece_size := n
                    | String "pieces", String pieces ->
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

(*  if !file_files <> [] && not (String2.check_suffix !file_name ".torrent") then
    file_name := !file_name ^ ".torrent";*)
  file_files := List.rev !file_files;
  
  file_id, {
    torrent_name = !file_name;
    torrent_length = !length;
    torrent_announce = !announce;
    torrent_piece_size = !file_piece_size;
    torrent_files = !file_files;
    torrent_pieces = pieces;
  }
  
let encode_torrent torrent =
  
  let npieces = Array.length torrent.torrent_pieces in
  let pieces = String.create (20 * npieces) in
  for i = 0 to npieces - 1 do
    String.blit (Sha1.direct_to_string torrent.torrent_pieces.(i)) 0
      pieces (i*20) 20
  done;

  let encode_file (filename, size) =
    Dictionary [
      String "path", List (List.map 
          (fun s -> String s)(Filepath.string_to_path '/' filename));
      String "length", Int size;
    ]
  in
  
  let files = 
    match torrent.torrent_files with 
      [] ->       
        String "length", Int torrent.torrent_length
    | _ ->
        String "files", 
        List (List.map encode_file torrent.torrent_files)
  in
  
  let info =
    Dictionary [
      files;
      String "name", String torrent.torrent_name;
      String "piece length", Int torrent.torrent_piece_size;
      String "pieces", String pieces;
    ]
  in
  
  let info_encoded = Bencode.encode info in
  let file_id = Sha1.string info_encoded in
  file_id, 
  Dictionary [
    String "announce", String torrent.torrent_announce;
    String "info", info;
  ]
  
let chunk_size = Int64.of_int (256 * 1024)  
    
let make_torrent announce filename = 
  let basename = Filename.basename filename in
  let files, t =
    if Unix2.is_directory filename then
      let rec iter_directory list dirname =
        let files = Unix2.list_directory (Filename.concat filename dirname) in
        iter_files list dirname files
      
      and iter_files list dirname files =
        match files with
          [] -> list
        | file :: tail ->
            let basename = Filename.concat dirname file in
            let fullname = Filename.concat filename basename in
            let left =
              if Unix2.is_directory fullname then
                iter_directory list basename
              else
                (basename, Unix32.getsize fullname) :: list
            in
            iter_files left dirname tail
      in
      let files = iter_directory [] "" in
      let t = Unix32.create_multifile filename Unix32.ro_flag 0o666 files in
      files, t
    else
      [], Unix32.create_ro filename
  in
  
  Unix32.flush_fd t;
  let length = Unix32.getsize64 t in
  let npieces = 1+ Int64.to_int ((length -- one) // chunk_size) in
  let pieces = Array.create npieces Sha1.null in
  for i = 0 to npieces - 1 do
    let begin_pos = chunk_size *.. i in
    
    let end_pos = begin_pos ++ chunk_size in
    let end_pos = 
      if end_pos > length then length else end_pos in
    
    let sha1 = Sha1.digest_subfile t
        begin_pos (end_pos -- begin_pos) in
    pieces.(i) <- sha1
  done;
  
  {
    torrent_name = basename;
    torrent_length = length;
    torrent_announce = announce;
    torrent_piece_size = chunk_size;
    torrent_files = files;
    torrent_pieces = pieces;
  }

let generate_torrent announce torrent_filename filename =
  let torrent = make_torrent announce filename in
  let file_id, encoded = encode_torrent torrent in
  let encoded = Bencode.encode encoded in
  File.from_string torrent_filename encoded 


open Http_server

type tracker_peer = {
    peer_id : Sha1.t;
    mutable peer_ip : Ip.t;
    mutable peer_port : int;
    mutable peer_active : int;
  }
  
type tracker = {
    mutable tracker_table : (Sha1.t, tracker_peer) Hashtbl.t;
    mutable tracker_peers : tracker_peer Fifo.t;
  }
  
let current_tracked_files = ref (Hashtbl.create 13)

  
  
let int64_of_string v =
  try
    Int64.of_string v
  with e ->
      lprintf "Exception %s in int64_of_string [%s]\n" 
        (Printexc2.to_string e) v;
      raise e
  
let int_of_string v =
  try
    int_of_string v
  with e ->
      lprintf "Exception %s in int_of_string [%s]\n" 
        (Printexc2.to_string e) v;
      raise e
  
let http_handler t r =
  begin
    match r.get_url.Url.short_file with
      "/tracker" ->
        begin
          try
            
            let args = r.get_url.Url.args in
            let info_hash = ref Sha1.null in
            let peer_id = ref Sha1.null in
            let port = ref 0 in
            let uploaded = ref zero in
            let downloaded = ref zero in
            let left = ref zero in
            let event = ref "" in
            List.iter (fun (name, arg) ->
                match name with
                | "info_hash" -> info_hash := Sha1.direct_of_string arg
                | "peer_id" -> peer_id := Sha1.direct_of_string arg
                | "port" -> port := int_of_string arg
                | "uploaded" -> uploaded := int64_of_string arg
                | "downloaded" -> downloaded := int64_of_string arg
                | "left" -> left  := int64_of_string arg
                | "event" -> event := arg
                | _ -> lprintf "BTTracker: Unexpected [%s=%s]\n" name arg
            ) args;
            
            let tracker = 
              Hashtbl.find !current_tracked_files !info_hash 
            in
            
            let peer = 
              try 
                let peer = 
                  Hashtbl.find tracker.tracker_table !peer_id
                in
                peer.peer_ip <- TcpBufferedSocket.peer_ip r.sock;
                peer.peer_port <- !port;
                peer.peer_active <- last_time ();
                peer
              with _ -> 
                  let peer = 
                    { 
                      peer_id = !peer_id;
                      peer_ip = TcpBufferedSocket.peer_ip r.sock;
                      peer_port = !port;
                      peer_active = last_time ();
                    } in
                  Hashtbl.add tracker.tracker_table !peer_id peer;
                  Fifo.put tracker.tracker_peers peer;
                  peer
            in
            let head =
              match !event with
                "completed" ->
(* Reply with clients that could not connect to this tracker otherwise *)
                  []
              
              | "stopped" -> 
(* Don't return anything *)
                  []
              
              | _ ->
(* Return the 20 best peers *)
                  
                  let list = ref [] in
                  
                  (try
                      for i = 0 to 19 do
                        let peer = Fifo.take tracker.tracker_peers in
                        list := peer :: !list
                      done
                    with _ -> ());
                  
                  List.iter (fun p ->
                      lprintf "Tracker send: %s:%d\n" 
                        (Ip.to_string p.peer_ip) p.peer_port;
                      Fifo.put tracker.tracker_peers p
                  ) !list;
                  
                  !list
            in
(* reply by sending [head] *)
            
            let message = 
              Dictionary [
                String "interval", Int (Int64.of_int 600);
                String "peers", List 
                  (List.map (fun p ->
                      Dictionary [
                        String "peer id", String 
                          (Sha1.direct_to_string p.peer_id);
                        String "ip", String (Ip.to_string p.peer_ip);
                        String "port", 
                        Int (Int64.of_int p.peer_port);
                      ]
                  ) head)
              ]
            in
            
            r.reply_content <-  Bencode.encode message
          
          with e ->
              lprintf "BTTracker: Exception %s\n" (Printexc2.to_string e);
              raise e
        end
    
    | filename ->
        try
          
          if (Filename2.last_extension filename <> ".torrent") then
            failwith "Incorrect filename 1";
          for i = 1 to String.length filename - 1 do
            let c = filename.[i] in
            if c = '/' || c = '\\' then failwith "Incorrect filename 2"
          done;
          if filename.[1] = ':' then failwith "Incorrect filename 3";
          
          let filename = Filename.concat tracked_directory filename in
          r.reply_content <- File.to_string filename
        with e ->
            lprintf "BTTracker: for request [%s] exception %s\n" 
              (Url.to_string r.get_url) (Printexc2.to_string e);
            
            r.reply_head <- "404 Not Found"
  end;

  add_reply_header r "Server" "MLdonkey";
  add_reply_header r "Connection" "close";
  add_reply_header r "Content-Type" "application/x-bittorrent"
  
let tracker_sock = ref None

let scan_tracked_directory _ = 
  let filenames = Unix2.list_directory tracked_directory in
  
  let old_tracked_files = !current_tracked_files in
  current_tracked_files := Hashtbl.create 13;
  List.iter (fun filename ->
      let filename = Filename.concat tracked_directory filename in
      try
        let s = File.to_string filename in
        let (info_hash : Sha1.t), torrent = decode_torrent s in
        let tracker = 
          try
            Hashtbl.find old_tracked_files info_hash
          with Not_found ->
              {
                tracker_table = Hashtbl.create 13;
                tracker_peers = Fifo.create ();
              }
        in
        Hashtbl.add !current_tracked_files info_hash tracker
      with e ->
          lprintf "Cannot track file %s\n" filename
  ) filenames
      
let start_tracker () = 
  let config = {
      bind_addr = Unix.inet_addr_any ;
      port = !!tracker_port;
      requests = [];
      addrs = [ Ip.of_string "255.255.255.255" ];
      base_ref = "";
      default = http_handler;      
    } in
  let sock = TcpServerSocket.create "BT tracker"
      (Ip.to_inet_addr !!client_bind_addr) !!tracker_port (Http_server.handler config) in
  tracker_sock := Some sock;
  scan_tracked_directory ();
  ()

let stop_tracker () =
  match !tracker_sock with
    None -> ()
  | Some sock ->
(* We should also close all the sockets opened for HTTP connections, no ? *)
      TcpServerSocket.close sock Closed_by_user;
      tracker_sock := None
  
let clean_tracker_timer () =
  let time_threshold = last_time () - 3600 in
  Hashtbl.iter (fun _ tracker ->
      let list = ref [] in
      let old_peers = ref [] in
      Hashtbl.iter (fun _ peer -> 
          if peer.peer_active < time_threshold then
            old_peers := peer :: !old_peers
          else
          if Ip.valid peer.peer_ip && ip_reachable peer.peer_ip then
            list := peer :: !list) 
      tracker.tracker_table;
      List.iter (fun p ->
          Hashtbl.remove tracker.tracker_table p.peer_id
      ) !old_peers;
      Fifo.clear tracker.tracker_peers;
      List.iter (fun p ->
          Fifo.put tracker.tracker_peers p) !list
  ) !current_tracked_files
  
let _ =
  add_infinite_timer 120. scan_tracked_directory
  
