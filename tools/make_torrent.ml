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

  
let zero = Int64.zero
let one = Int64.one
let (++) = Int64.add
let (--) = Int64.sub
let ( ** ) x y = Int64.mul x (Int64.of_int y)
let ( // ) x y = Int64.div x y


open BTTypes

let announce = ref ""
let torrent_filename = ref ""
  
let check_tracker () =
  if !announce = "" then begin
      Printf.printf "You must specify the tracker url with -tracker <url>";
      print_newline (); 
      exit 2;
    end
  
let check_torrent () =
  if !torrent_filename = "" then begin
      Printf.printf "You must specify the .torrent filename with -torrent <filename>";
      print_newline (); 
      exit 2;
    end
    
let _ =
  Arg.parse [
    "-tracker", Arg.String ((:=) announce),
    "<url> : set the tracker to put in the torrent file";
    "-torrent", Arg.String ((:=) torrent_filename),
    "<filename.torrent> : the .torrent file to use";
    
    "-change", Arg.Unit (fun _ ->
        check_tracker ();
        check_torrent ();
        let s = File.to_string !torrent_filename in
        let torrent_id, torrent = BTTorrent.decode_torrent s in
        let torrent = { torrent with BTTypes.torrent_announce = !announce } in
        let torrent_id, encoded =  BTTorrent.encode_torrent torrent in
        let s = Bencode.encode encoded in
        File.from_string !torrent_filename s;
        Printf.printf "Torrent file of %s modified" (Sha1.to_string torrent_id);
        print_newline ();
    ), ": change the tracker inside a .torrent file";
    
    "-print", Arg.Unit (fun filename ->
        check_torrent ();
        let s = File.to_string !torrent_filename in
        let torrent_id, torrent = BTTorrent.decode_torrent s in
        Printf.printf "Torrent name: %s\n" torrent.torrent_name;
        Printf.printf "        length: %Ld\n" torrent.torrent_length;
        Printf.printf "        tracker: %s\n" torrent.torrent_announce;
        Printf.printf "        piece size: %Ld\n" torrent.torrent_piece_size;
        Printf.printf "  Pieces: %d\n" (Array.length torrent.torrent_pieces);
        Array.iteri (fun i s ->
            Printf.printf "    %3d: %s\n" i (Sha1.to_string s)
        ) torrent.torrent_pieces;
        if torrent.torrent_files <> [] then begin
            Printf.printf "  Files: %d\n" (List.length torrent.torrent_files);
            List.iter (fun (s, len) ->
                Printf.printf "    %10Ld : %s\n" len s
            ) torrent.torrent_files;
          end;
        print_newline ();
    ), "<filename.torrent>: change the tracker inside a .torrent file";
    
    "-create", Arg.String (fun filename ->
        check_tracker ();
        check_torrent ();
        BTTorrent.generate_torrent !announce !torrent_filename filename;
        Printf.printf "Torrent file generated";
        print_newline ();
    )," <filename> : compute hashes of filenames and generate a .torrent file";
    
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
    
    ), "<filename> : split a file corresponding to a .torrent file";
    
    "-check", Arg.String (fun filename ->
        check_torrent ();
        let s = File.to_string !torrent_filename in
        let torrent_id, torrent = BTTorrent.decode_torrent s in
        
        if torrent.torrent_name <> Filename.basename filename then begin
            Printf.printf "WARNING: %s <> %s" 
              torrent.torrent_name (Filename.basename filename);
            print_newline ();
          end;
        let t = if torrent.torrent_files <> [] then
            Unix32.create_multifile filename Unix32.ro_flag 0o666 
              torrent.torrent_files
          else  Unix32.create_ro filename
        in
        
        let length = Unix32.getsize64 t in
        
        if torrent.torrent_length <> length then begin
            Printf.printf "ERROR: computed size %Ld <> torrent size %Ld"
              length torrent.torrent_length;
            print_newline ();
            exit 2;
          end;
        
        let chunk_size = torrent.torrent_piece_size in
        let npieces = 1+ Int64.to_int ((length -- one) // chunk_size) in
        
        if Array.length torrent.torrent_pieces <> npieces then begin
            Printf.printf "ERROR: computed npieces %d <> torrent npieces %d"
              npieces (Array.length torrent.torrent_pieces);
            print_newline ();
            exit 2;
          
          end;
        
        for i = 0 to npieces - 1 do
          let begin_pos = chunk_size ** i in
          
          let end_pos = begin_pos ++ chunk_size in
          let end_pos = 
            if end_pos > length then length else end_pos in
          
          let sha1 = Sha1.digest_subfile t
              begin_pos (end_pos -- begin_pos) in
          if torrent.torrent_pieces.(i) <> sha1 then begin
              Printf.printf "WARNING: piece %d (%Ld-%Ld) has SHA1 %s instead of %s"
                i begin_pos end_pos 
                (Sha1.to_string sha1)
              (Sha1.to_string torrent.torrent_pieces.(i));
              print_newline ();
            end
        done;

        Printf.printf "Torrent file verified !!!";
        print_newline ();

    ), " <filename> : check that <filename> is well encoded by a .torrent";
  ]
    (fun s ->
      Printf.printf "Don't know what to do with %s\n" s;
      Printf.printf "Use --help to get some help";
      print_newline (); 
      exit 2;
      )
      ": manipulate .torrent files";
    
  