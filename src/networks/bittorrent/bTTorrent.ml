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
  if !verbose_msg_servers then lprintf_nl "[BT] .torrent file loaded";
(*            lprintf "Loaded: %s\n" (String.escaped s); *)
  let v = Bencode.decode s in
(*            lprintf "Decoded file: %s\n" (Bencode.print v);  *)


  let announce = ref "" in
  let announce_list = ref [] in
  let file_info = ref (List []) in
  let file_name = ref "" in
  let file_torrent_filename = ref "" in
  let file_name_utf8 = ref None in
  let file_piece_size = ref zero in
  let file_pieces = ref "" in
  let file_comment = ref "" in
  let file_created_by = ref "" in
  let file_creation_date = ref zero in
  let file_modified_by = ref "" in
  let file_encoding = ref "" in
  let file_codepage = ref zero in
  let file_ed2k_hash = ref "" in
  let file_is_private = ref false in
  let file_aps = ref (List []) in
  let file_dht_backup_enable = ref zero in
  let length = ref zero in
  let file_files = ref [] in
  let file_files_utf8 = ref [] in

  let parse_files files =
    let current_pos = ref zero in
    List.iter (fun v ->
        match v with
          Dictionary list ->
            let current_file = ref "" in
            let current_file_utf8 = ref "" in
            let current_length = ref zero in
            let length_set = ref false in

            let path_list_to_string l =
                    Filepath.path_to_string '/'
                (
                  List.map (fun v ->
                          match v with
                            String s -> s
                          | _ -> assert false
                  ) l
                )
            in

            List.iter (fun (key, value) ->
                match key, value with
                | "path", List path ->
                    current_file := path_list_to_string path;
                    if !verbose_msg_servers then
                      lprintf_nl "[BT] Parsed a new path: [%s]" !current_file
                | "path.utf-8", List path_utf8 -> 
                    current_file_utf8 := path_list_to_string path_utf8;
                    if !verbose_msg_servers then
                      lprintf_nl "[BT] Parsed path.utf-8: [%s]" !current_file
                | "length", Int n ->
                    length := !length ++ n;
                    current_length := n;
                    length_set := true

                | key, _ ->
                    if !verbose_msg_servers then lprintf_nl "[BT] other field [%s] with value [%s] in files" key (Bencode.print value)
            ) list;

            assert (!length_set);
            assert (!current_file <> "" || !current_file_utf8 <> "");
            file_files := (!current_file, !current_length) :: !file_files;
            if !current_file_utf8 <> "" then
              file_files_utf8 := (!current_file_utf8, !current_length) :: !file_files_utf8;
            current_pos := !current_pos ++ !current_length

        | _ -> assert false
    ) files;
  in

  begin
    match v with
      Dictionary list ->
        List.iter (fun (key, value) ->
            match key, value with
            | "announce", String tracker_url ->
               if !verbose_msg_servers then
                 lprintf_nl "[BT] New tracker added: %s" tracker_url;
                announce := tracker_url
            | "announce-list", List list ->
                List.iter (fun url_list ->
                    let next_urls = ref [] in
                    match url_list with
                    | List next_url_list ->
                        List.iter (fun myvalue ->
                            match myvalue with
                            | String next_url ->
                                next_urls := next_url :: !next_urls;
                                if !verbose_msg_servers then
                                  lprintf_nl "[BT] New tracker received :%s" next_url
                            | _ ->
                                if !verbose_msg_servers then
                                  lprintf_nl "[BT] error while decoding announce list"
                        ) next_url_list;
                        if List.length !next_urls > 1 then begin
                            next_urls := List2.shuffle !next_urls;
                            announce_list := !next_urls @ !announce_list
                          end
                        else
                          announce_list := (try List.hd !next_urls with _ -> "") :: !announce_list
                    | _ ->
                        lprintf_nl "[BT] unknown field in announce list"
                    ) list;
                    announce_list := List.rev !announce_list;
                    if !verbose_msg_servers then
                      List.iter (fun url ->
                        lprintf_nl "[BT] New tracker added :%s" url
                        ) !announce_list
            | "info", ((Dictionary list) as info) ->

                file_info := info;
                List.iter (fun (key, value) ->
                    match key, value with
                    | "files", List files ->
                        parse_files files
                    | "length", Int n ->
                        length := n
                    | "name", String name ->
                        file_name := name
                    | "piece length", Int n ->
                        file_piece_size := n
                    | "pieces", String pieces ->
                        file_pieces := pieces
                    | "ed2k", String string_ed2k ->
                        if !!enable_donkey then
                          file_ed2k_hash := string_ed2k;
                      (* TODO: Add new ed2k download if ed2k hash is available,
                               then merge it with current download *)
                    | "sha1", String string_sha1 -> ()
                      (* TODO: Parse sha1 hash *)

                    | "publisher", String created_by ->
                      file_created_by := created_by
                    | "publisher-url", String publisher_url ->
                      file_created_by := !file_created_by ^ " @ " ^ publisher_url

                    | "name.utf-8", String name_utf8 ->
                        file_name_utf8 := Some name_utf8

                    | "publisher.utf-8", String publisher_utf8 -> ()
                    | "publisher-url.utf-8", String publisher_url_utf8 -> ()

                    | "private", Int n ->
                        file_is_private := n <> 0L;
                        if !verbose_msg_servers && !file_is_private then
                            lprintf_nl "[BT] torrent is private"
                    | key, _ ->
                        if !verbose_msg_servers then
                          lprintf_nl "[BT] found other field [%s] with value [%s] in info" key (Bencode.print value)
                ) list

            | "comment", String comment
            | "comment.utf-8", String comment ->
              file_comment := comment
            (* Next 2 strings are after info sometimes  *)
            | "publisher", String created_by ->
              file_created_by := created_by
            | "publisher-url", String publisher_url ->
              file_created_by := !file_created_by ^ " @ " ^ publisher_url

            | "created by", String created_by ->
              file_created_by := created_by
            | "creation date", Int creation_date ->
              file_creation_date := creation_date
            | "modified-by", String modified_by ->
              file_modified_by := modified_by
            | "encoding", String encoding ->
              file_encoding := encoding
            | "codepage", Int codepage ->
              file_codepage := codepage
            | "torrent filename", String torrent_filename ->
              file_torrent_filename := torrent_filename
            | "nodes", nodes -> ()
              (* TODO : nodes is a list of DHT Network nodes ,parse and use them *)

(*
              file_nodes := nodes
*)

            | "azureus_properties", ((Dictionary list) as azureus_properties) ->
                file_aps := azureus_properties;
                List.iter (fun (key, value) ->
                    match key, value with
                    | "dht_backup_enable", Int n ->
                        file_dht_backup_enable := n;
                        if !verbose_msg_servers &&
                          Int64.to_int !file_dht_backup_enable = 1 then
                            lprintf_nl "[BT] azureus properties : Torrent has dht backup"
                    | key, _ ->
                        if !verbose_msg_servers then
                          lprintf_nl "[BT] found other field [%s] with value [%s] in azureus properties" key (Bencode.print value)
                ) list
            | key, _ ->
                if !verbose_msg_servers then lprintf_nl "[BT] found other field [%s] with value [%s] after info" key (Bencode.print value)
        ) list
    | _ -> assert false
  end;

(* Convert codepage number to Charset name, for example: 936 -> CP936 *)
  if !file_codepage <> 0L && !file_encoding = "" then
    file_encoding := "CP" ^ (Int64.to_string !file_codepage);

  let real_file_name =
    match !file_name_utf8 with
    | None -> Charset.safe_convert !file_encoding !file_name
    | Some name -> name
  in
  assert (real_file_name <> "");
  assert (!file_piece_size <> zero);
  assert (!file_pieces <> "");
  assert (!file_info = Bencode.decode (Bencode.encode ~strict:false !file_info));

  let file_id = Sha1.string (Bencode.encode ~strict:false !file_info) in
  let npieces = 1 + Int64.to_int ((!length -- one) // !file_piece_size) in
  let pieces = Array.init npieces (fun i ->
        let s = String.sub !file_pieces (i*20) 20 in
        Sha1.direct_of_string s
    ) in

(* Only at this point we know if the torrent contains an "encoding" field
   If UTF8 filenames were found, use them. If not and we have a charset
   value used for encoding, convert non-UTF8 filenames to UTF8 ones. *)
  if !file_files_utf8 <> [] then
    file_files := !file_files_utf8
  else
    if !file_encoding <> "" then
      begin
        let file_files_encoded = ref [] in
        List.iter (fun (name, length) ->
          file_files_encoded := !file_files_encoded @ [(Charset.safe_convert !file_encoding name), length]
        ) !file_files;
        file_files := !file_files_encoded
      end;

  (match List.length !file_files with
  | 0 -> ()
  | 1 -> file_name := (fst (List.hd !file_files));
         file_files := []
  | _ -> file_files := List.rev !file_files);

  file_id, {
    torrent_name = real_file_name;
    torrent_filename = !file_torrent_filename;
    torrent_name_utf8 = real_file_name;
    torrent_length = !length;
    torrent_announce = !announce;
    torrent_announce_list = !announce_list;
    torrent_piece_size = !file_piece_size;
    torrent_files = !file_files;
    torrent_pieces = pieces;
    torrent_comment = Charset.safe_convert !file_encoding !file_comment;
    torrent_created_by = Charset.safe_convert !file_encoding !file_created_by;
    torrent_creation_date = !file_creation_date;
    torrent_modified_by = Charset.safe_convert !file_encoding !file_modified_by;
    torrent_encoding = !file_encoding;
    torrent_private = !file_is_private;

(*
    torrent_nodes = !file_nodes;
*)
  }

let encode_torrent torrent =

  let npieces = Array.length torrent.torrent_pieces in
  let pieces = Bytes.create (20 * npieces) in
  for i = 0 to npieces - 1 do
    String.blit (Sha1.direct_to_string torrent.torrent_pieces.(i)) 0
      pieces (i*20) 20
  done;
  let pieces = Bytes.unsafe_to_string pieces in

  let encode_file (filename, size) =
    Dictionary [
      "length", Int size;
      "path", List (List.map
          (fun s -> String s)(Filepath.string_to_path '/' filename));
    ]
  in

  let files =
    match torrent.torrent_files with
      [] ->
        "length", Int torrent.torrent_length
    | _ ->
        "files",
        List (List.map encode_file torrent.torrent_files)
  in

  let info =
    Dictionary [
      files;
      "name", String torrent.torrent_name;
      "name.utf-8", String torrent.torrent_name_utf8;
      "piece length", Int torrent.torrent_piece_size;
      "pieces", String pieces;
      "private", Int (if torrent.torrent_private then 1L else 0L);
    ]
  in

  let info_encoded = Bencode.encode info in
  let file_id = Sha1.string info_encoded in
  file_id,
  Dictionary [
    "announce", String torrent.torrent_announce;
    "comment", String torrent.torrent_comment;
    "created by", String torrent.torrent_created_by;
    "creation date", Int torrent.torrent_creation_date;
    "encoding", String torrent.torrent_encoding;
    "info", info;
    "modified-by", String torrent.torrent_modified_by;
(*
    String "nodes", String torrent.torrent_nodes;
*)
  ]

let make_torrent announce filename comment is_private =
  let announce_list = [ announce ] in
  let basename = Filename2.basename filename in
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
      let t = Unix32.create_multifile filename false files in
      files, t
    else
      [], Unix32.create_ro filename
  in

  Unix32.flush_fd t;
  let length = Unix32.getsize64 t in
  let npieces = 1+ Int64.to_int ((length -- one) // chunk_size) in
  let pieces = Array.make npieces Sha1.null in
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
    torrent_filename = "";
    torrent_name_utf8 = Charset.Locale.to_utf8 basename;
    torrent_length = length;
    torrent_announce = announce;
    torrent_announce_list = announce_list;
    torrent_piece_size = chunk_size;
    torrent_files = files;
    torrent_pieces = pieces;
    torrent_comment =
      if String.length comment > 1 then
        comment
      else
        Printf.sprintf "Created by MLdonkey/%s" Autoconf.current_version;

    torrent_created_by = Printf.sprintf "MLdonkey/%s" Autoconf.current_version;
    torrent_creation_date = Int64.of_float (Unix.gettimeofday ());
    torrent_modified_by = "";
    torrent_encoding = "";
    torrent_private = is_private;
(*
    torrent_nodes = "";
*)
  }

let generate_torrent announce torrent_filename torrent_comment torrent_private filename =
  let torrent = make_torrent announce filename torrent_comment torrent_private in
  let file_id, encoded = encode_torrent torrent in
  let encoded = Bencode.encode encoded in
  File.from_string torrent_filename encoded;
  file_id

