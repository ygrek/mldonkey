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
open Bencode

  
open Gettext  
let _s x = _s "BTTorrent" x
let _b x = _b "BTTorrent" x  

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

    
let chunk_size = Int64.of_int (256 * 1024)    
  
let decode_torrent s =
  if !verbose_msg_servers then lprintf ".torrent file loaded\n";
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
                    if !verbose_msg_servers then lprintf "other field [%s] in files\n" key
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
                        if !verbose_msg_servers then lprintf "other field [%s] in info\n" key
                    | _ -> 
                        lprintf "other field in info\n"
                ) list            
            | String key, _ -> 
                if !verbose_msg_servers then lprintf "other field [%s] after info\n" key
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
                (basename, Unix32.getsize fullname false) :: list
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
  let length = Unix32.getsize64 t false in
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
