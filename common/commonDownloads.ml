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


open CommonInteractive
open CommonClient
open CommonComplexOptions
open CommonTypes
open CommonFile
open Options
open BasicSocket
open TcpBufferedSocket

open CommonGlobals
open CommonOptions
  
(*
A common function for all networks were the file is got in one single piece,
  and the connection is closed at the end.
*)

type download = {
    download_file : file; (* the file being downloaded *)
    download_client : client;
    mutable download_pos : int32; (* the position in the file *)
    mutable download_sock : TcpBufferedSocket.t option;
(* the socket for download (often shared in client structure) *)
    mutable download_on_close : (download -> unit);
(* function called when
    the socket is closed (Unix.close is already done) *)
    download_subdir : string Options.option_record;
(* The subdir option for the commit *)
    download_on_finish : (download -> unit);
(* A function to call when download is finished ! *)
    
(* When the data is long enough to be accepted *)
    mutable download_min_read : int;
  }

let disconnect_download d =
  match d.download_sock with
    None -> ()
  | Some sock ->
      close sock "";
      (try d.download_on_close d with _ -> ());
      Printf.printf "DISCONNECTED FROM SOURCE"; print_newline ();
      d.download_sock <- None
      
let file_complete d file =
(*
  Printf.printf "FILE %s DOWNLOADED" f.file_name;
print_newline ();
  *)
  file_completed file;
  (try d.download_on_finish d with _ -> ())
         
let download_reader d sock nread = 
  if d.download_sock = None then  raise Exit;
  let file = d.download_file in
  if nread >= d.download_min_read then
    let b = TcpBufferedSocket.buf sock in
    d.download_min_read <- 1;
    set_rtimeout sock 120.;
    set_client_state d.download_client Connected_busy;
    begin
      let fd = try
          Unix32.force_fd (file_fd file) 
        with e -> 
            Printf.printf "In Unix32.force_fd"; print_newline ();
            raise e
      in
      let final_pos = Unix32.seek32 (file_fd file) d.download_pos
        Unix.SEEK_SET in
      Unix2.really_write fd b.buf b.pos b.len;
    end;
(*      Printf.printf "DIFF %d/%d" nread b.len; print_newline ();*)
    d.download_pos <- Int32.add d.download_pos (Int32.of_int b.len);
(*
      Printf.printf "NEW SOURCE POS %s" (Int32.to_string c.client_pos);
print_newline ();
  *)
    TcpBufferedSocket.buf_used sock b.len;
    if d.download_pos > file_downloaded file then begin
        (as_file_impl file).impl_file_downloaded <- d.download_pos;
        file_must_update file;
      end;
    if file_downloaded file = file_size file then
      file_complete d file 

      
let new_download sock c file min_read on_close on_finish subdir_option  =
  let d = {
      download_client = c;
      download_file = file;
      download_sock = Some sock;
      download_pos = file_downloaded file;
      download_on_finish = on_finish;
      download_on_close = on_close;
      download_subdir = subdir_option;
      download_min_read = min_read;
    } in
  set_closer sock (fun _ _ -> disconnect_download d);
  TcpBufferedSocket.set_read_controler sock download_control;
  TcpBufferedSocket.set_write_controler sock upload_control;
  set_rtimeout sock 30.;
  d  

(* We should implement a common uploader too for all networks where
upload is done linearly. *)
  
  
type upload = {
    upload_file : file; (* the file being uploaded *)
    upload_client : client;
    mutable upload_pos : int32; (* the position in the file *)
    mutable upload_end : int32; (* the last position in the file to upload *)
    mutable upload_sock : TcpBufferedSocket.t option; (* the socket for upload (often shared in client structure) *)
    mutable upload_on_close : (upload -> unit); (* function called when
    the socket is closed (Unix.close is already done) *)
    upload_subdir : string Options.option_record;
(* The subdir option for the commit *)
    upload_on_finish : (upload -> unit);
(* A function to call when upload is finished ! *)    
  }
