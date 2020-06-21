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

open Md4
open LittleEndian
open Unix
open Printf2

open BTTypes

let announce = ref ""
let torrent_filename = ref ""
let torrent_comment = ref ""
let torrent_private = ref 0
let zero = Int64.zero
let one = Int64.one
let (++) = Int64.add
let (--) = Int64.sub
let ( ** ) x y = Int64.mul x y
let ( // ) x y = Int64.div x y

let check_tracker () =
  if !announce = "" then begin
      Printf.printf "You must specify the tracker url with -tracker <url>\n";
      exit 2;
    end

let check_torrent () =
  if !torrent_filename = "" then begin
      Printf.printf "You must specify the .torrent filename with -torrent <filename>\n";
      exit 2;
    end

let _ =
  let args = [
    "-tracker", Arg.Set_string announce,
    "<url> set the tracker to put in the torrent file";
    "-torrent", Arg.Set_string torrent_filename,
    "<filename.torrent> the .torrent file to use";
    "-comment", Arg.Set_string torrent_comment,
    "\"<string>\" some comments on the torrent";
    "-private", Arg.Set_int torrent_private,
    "<0|1> set the private flag";

    "-change", Arg.Unit (fun _ ->
        check_tracker ();
        check_torrent ();
        let s = File.to_string !torrent_filename in
        let torrent_id, torrent = BTTorrent.decode_torrent s in
        let torrent = { torrent with 
          BTTypes.torrent_announce = !announce;
          BTTypes.torrent_modified_by = Printf.sprintf "MLdonkey/%s" Autoconf.current_version } in
        let torrent_id, encoded =  BTTorrent.encode_torrent torrent in
        let s = Bencode.encode encoded in
        File.from_string !torrent_filename s;
        Printf.printf "Torrent file of %s modified\n" (Sha1.to_hexa torrent_id);
    ), " change the tracker inside a .torrent file";

    "-print", Arg.Unit (fun filename ->
        check_torrent ();
        let s = File.to_string !torrent_filename in
        let torrent_id, torrent = BTTorrent.decode_torrent s in
        Printf.printf "Torrent name: %s\n" torrent.torrent_name;
        Printf.printf "        comment: %s\n" torrent.torrent_comment;
        Printf.printf "        created by: %s\n" torrent.torrent_created_by;
        Printf.printf "        creation date: %s\n" (Date.to_string (Int64.to_float
          torrent.torrent_creation_date));
        Printf.printf "        modified by: %s\n" torrent.torrent_modified_by;
        Printf.printf "        length: %Ld\n" torrent.torrent_length;
        Printf.printf "        encoding: %s\n" torrent.torrent_encoding;
        Printf.printf "        tracker: %s\n" torrent.torrent_announce;
        Printf.printf "        private: %s\n" (if torrent.torrent_private then "yes" else "no");
        Printf.printf "        piece size: %Ld\n" torrent.torrent_piece_size;
        Printf.printf "  Pieces: %d\n" (Array.length torrent.torrent_pieces);
        Array.iteri (fun i s ->
            Printf.printf "    %3d: %s\n" i (Sha1.to_hexa s)
        ) torrent.torrent_pieces;
        if torrent.torrent_files <> [] then begin
            Printf.printf "  Files: %d\n" (List.length torrent.torrent_files);
            List.iter (fun (s, len) ->
                Printf.printf "    %10Ld : %s\n" len s
            ) torrent.torrent_files;
          end;
    ), "<filename.torrent> print the contents of a .torrent file";

    "-create", Arg.String (fun filename ->
        check_tracker ();
        check_torrent ();
        try
          let hash = BTTorrent.generate_torrent !announce !torrent_filename !torrent_comment 
            (!torrent_private<>0) filename
          in
          Printf.printf "Torrent file generated : %s\n" (Sha1.to_hexa hash);
        with
          exn -> Printf.printf "Cannot create torrent : %s\n" (Printexc2.to_string exn); exit 2
    ),"<filename> compute hashes of filename(s) (can be a directory) and generate a .torrent file";

    "-split", Arg.String (fun filename ->
        check_torrent ();

        let s = File.to_string !torrent_filename in
        let torrent_id, torrent = BTTorrent.decode_torrent s in

        let base_dir_name =
          String.sub !torrent_filename 0 ((String.length !torrent_filename) - 8)
        in

        let bt_fd = Unix32.create_ro filename in
        let rec iter begin_pos list =
          match list with
            [] -> ()
          | (filename, size) :: tail ->
              let end_pos = begin_pos ++ size in
              let filename = Filename.concat base_dir_name filename in
              let dirname = Filename.dirname filename in
              Unix2.safe_mkdir dirname;
              lprintf "Copying %Ld %Ld to 0\n"
                begin_pos (end_pos -- begin_pos);
              let fd = Unix32.create_rw filename in
              Unix32.copy_chunk bt_fd fd begin_pos zero
                (Int64.to_int (end_pos -- begin_pos));
              Unix32.close fd;

              iter end_pos tail
        in
        iter zero torrent.torrent_files;
        Unix32.close bt_fd;

    ), "<filename> split a file corresponding to a .torrent file";

    "-check", Arg.String (fun filename ->
        check_torrent ();
        let s = File.to_string !torrent_filename in
        let torrent_id, torrent = BTTorrent.decode_torrent s in

        if torrent.torrent_name <> Filename.basename filename then begin
            Printf.printf "WARNING: %s <> %s\n"
              torrent.torrent_name (Filename.basename filename);
          end;
        let t = if torrent.torrent_files <> [] then
            Unix32.create_multifile filename false
              torrent.torrent_files
          else  Unix32.create_ro filename
        in

        let length = Unix32.getsize64 t in

        if torrent.torrent_length <> length then begin
            Printf.printf "ERROR: computed size %Ld <> torrent size %Ld\n"
              length torrent.torrent_length;
            exit 2;
          end;

        let chunk_size = torrent.torrent_piece_size in
        let npieces = 1 + Int64.to_int ((length -- one) // chunk_size) in

        if Array.length torrent.torrent_pieces <> npieces then begin
            Printf.printf "ERROR: computed npieces %d <> torrent npieces %d\n"
              npieces (Array.length torrent.torrent_pieces);
            exit 2;

          end;

        for i = 0 to npieces - 1 do
          let begin_pos = chunk_size ** (Int64.of_int i) in

          let end_pos = begin_pos ++ chunk_size in
          let end_pos = 
            if end_pos > length then length else end_pos in

          let sha1 = Sha1.digest_subfile t
              begin_pos (end_pos -- begin_pos) in
          if torrent.torrent_pieces.(i) <> sha1 then begin
              Printf.printf "WARNING: piece %d (%Ld-%Ld) has SHA1 %s instead of %s\n"
                i begin_pos end_pos
                (Sha1.to_hexa sha1)
              (Sha1.to_hexa torrent.torrent_pieces.(i));
            end
        done;

        Printf.printf "Torrent file verified !!!\n";

    ), "<filename> check that <filename> is well encoded by a .torrent";
  ]
  in
  Arg.parse (Arg.align args)
    (fun s ->
      Printf.printf "Don't know what to do with %s\n" s;
      Printf.printf "Use --help to get some help\n";
      exit 2;
      )
      ("make_torrent : manipulate .torrent files\n\n" ^
        "Quick Howto:\n" ^
        "- create a new torrent:\n" ^
        "make_torrent -tracker http://ip:port/announce -torrent file.torrent " ^
        "-comment \"mldonkey.sf.net\" -create file\n\n" ^
        "- change the tracker of a torrent file:\n" ^
        "make_torrent -tracker http://ip:port/tracker -torrent myfile.torrent -change\n\n" ^
        "- print the infos of a torrent file:\n" ^
        "make_torrent -torrent myfile.torrent -print\n\n\n" ^
        "All Options:");
