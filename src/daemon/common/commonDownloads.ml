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

open Printf2
open Int64ops
open CommonInteractive
open CommonClient
open CommonTypes
open CommonFile
open TcpBufferedSocket

open CommonGlobals

module Basic = struct
(*
A common function for all networks were the file is got in one single piece,
  and the connection is closed at the end.
*)
  
type ('file,'client) download = {
    download_file : 'file; (* the file being downloaded *)
    download_client : 'client;
    mutable download_min_read : int;
    mutable download_pos : int64; (* the position in the file *)
    mutable download_sock : TcpBufferedSocket.t option;
  }

module Make(M: sig
      type f
      type c
      val file : f -> file
      val client :   c -> client
      val client_disconnected :  (f, c) download -> unit
      val download_finished :  (f, c) download -> unit
    end) = 
  struct
    
    let disconnect_download (d : (M.f, M.c) download) reason =
      match d.download_sock with
        None -> ()
      | Some sock ->
          close sock reason;
          (try M.client_disconnected d with _ -> ());
          lprintf "DISCONNECTED FROM SOURCE\n"; 
          d.download_sock <- None
    
    let file_complete d =
(*
  lprintf "FILE %s DOWNLOADED\n" f.file_name;
  *)
      file_completed (M.file d.download_file);
      (try M.download_finished d with _ -> ())
    
    let download_reader d sock nread = 
      lprint_string ".";
      if d.download_sock = None then  raise Exit;
      let file = M.file d.download_file in
      if nread >= d.download_min_read then
        let b = TcpBufferedSocket.buf sock in
        d.download_min_read <- 1;
        set_rtimeout sock 120.;
        set_client_state (M.client d.download_client) 
        (Connected_downloading (file_num file));
        (*
        begin
          let fd = try
              Unix32.force_fd (file_fd file) 
            with e -> 
                lprintf "In Unix32.force_fd\n"; 
                raise e
          in
          let final_pos = Unix32.seek64 (file_fd file) d.download_pos
              Unix.SEEK_SET in *)
        Unix32.write_bytes (file_fd file) d.download_pos b.buf b.pos b.len;
(*        end; *)
(*      lprintf "DIFF %d/%d\n" nread b.len; *)
        d.download_pos <- d.download_pos ++ (Int64.of_int b.len);
(*
      lprintf "NEW SOURCE POS %s\n" (Int64.to_string c.client_pos);
  *)
        buf_used b b.len;
        if d.download_pos > file_downloaded file then 
          add_file_downloaded file
          (d.download_pos -- (file_downloaded file));
        if file_downloaded file = file_size file then
          file_complete d
    
    
    let new_download sock (c :M.c) (file : M.f) min_read =
      let d = {
          download_client = c;
          download_file = file;
          download_sock = Some sock;
          download_pos = file_downloaded (M.file file);
          download_min_read = min_read;
        } in
      set_closer sock (fun _ reason -> disconnect_download d reason);
      TcpBufferedSocket.set_read_controler sock download_control;
      TcpBufferedSocket.set_write_controler sock upload_control;
      set_rtimeout sock 30.;
      d  
      
end
end
