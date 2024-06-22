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

open CommonShared
open CommonClient
open Options
open TcpBufferedSocket
open DonkeyProtoCom
open DonkeyTypes
open DonkeyGlobals
open DonkeyOptions
open CommonOptions
open CommonGlobals
open DonkeyStats

let msg_block_size_int = 10240
let msg_block_size = Int64.of_int msg_block_size_int
let upload_buffer = String.create msg_block_size_int
let max_msg_size = 15000

(* For upload, it is clearly useless to completely fill a
socket. Since we will try to upload to this client again when the
Fifo queue has been scanned, we can wait for it to download what we
have already given. 

max_hard_upload_rate * 1024 * nseconds

where nseconds = Fifo.length upload_clients
  
  *)
  
module NewUpload = struct
    
    
    let check_end_upload c sock = ()
(*
      if c.client_bucket = 0 then
        direct_client_send sock (
          let module M = DonkeyProtoClient in
          let module Q = M.CloseSlot in
            M.CloseSlotReq Q.t)
*)
    
    let send_small_block c sock file begin_pos len_int = 
(*      lprintf "send_small_block %d\n" len_int; *)
(*      let len_int = Int32.to_int len in *)
      try
        if !verbose_upload then
          lprintf_nl "Sending %s to %s, begin %Ld len %d"
            (file_best_name file) (full_client_identifier c)
            (begin_pos) (len_int);
        
        if file_is_largefile file && c.client_emule_proto.emule_largefiles <> 1 then raise Donkey_large_file;
        let msg =  
          (
            let module M = DonkeyProtoClient in
            let module B = M.Bloc in
            M.BlocReq {  
              B.md4 = file.file_md4;
              B.usesixtyfour = (begin_pos ++ (Int64.of_int len_int)) > old_max_emule_file_size;
              B.start_pos = begin_pos;
              B.end_pos = begin_pos ++ (Int64.of_int len_int);
              B.bloc_str = "";
              B.bloc_begin = 0;
              B.bloc_len = 0; 
            }
          ) in
        let s = client_msg_to_string c.client_emule_proto msg in
        let slen = Bytes.length s in
        let upload_buffer = String.create (slen + len_int) in
        Bytes.blit s 0 upload_buffer 0 slen;
        DonkeyProtoCom.new_string msg upload_buffer;
        Unix32.read (file_fd file) begin_pos upload_buffer slen len_int;
        let uploaded = Int64.of_int len_int in
        count_upload c uploaded;
        CommonUploads.consume_bandwidth len_int;
        (match file.file_shared with None -> ()
          | Some impl ->
              shared_must_update_downloaded (as_shared impl);
              impl.impl_shared_uploaded <- 
                impl.impl_shared_uploaded ++ uploaded);
        
        write sock upload_buffer 0 (Bytes.length upload_buffer);
        check_end_upload c sock
      with
      | End_of_file -> lprintf_nl "Can not send file %s to %s, file removed?"
                         (file_best_name file) (full_client_identifier c)
      | Donkey_large_file -> lprintf_nl "File %s is too large for %s."
                         (file_best_name file) (full_client_identifier c)
      | e -> if !verbose then lprintf_nl
               "Exception %s in send_small_block" (Printexc2.to_string e)
    
    let rec send_client_block c sock per_client =
(*      lprintf "send_client_block\n"; *)
      if per_client > 0 && can_write_len sock max_msg_size then
        match c.client_upload with
        | Some ({ up_chunks = current_chunk :: chunks; _ } as up)  ->
            if up.up_file.file_shared = None then begin
(* Is there a message to warn that a file is not shared anymore ? *)
                c.client_upload <- None;
              end else
            let max_len = up.up_end_chunk -- up.up_pos in
            let max_len = Int64.to_int max_len in
            let msg_block_size_int = min msg_block_size_int per_client in
            if max_len <= msg_block_size_int then
(* last block from chunk *)
              begin
                send_small_block c sock up.up_file up.up_pos max_len;
                if !verbose_upload then
                    lprintf_nl "End of chunk (%d) %Ld %s" max_len up.up_end_chunk (file_best_name up.up_file);
                up.up_flying_chunks <- up.up_flying_chunks @ [current_chunk];
                up.up_chunks <- chunks;
                let per_client = per_client - max_len in
                match chunks with
                | [] -> 
                    if !verbose_upload then
                        lprintf_nl "NO MORE CHUNKS";
                    up.up_waiting <- false;
                    if up.up_finish && !!upload_complete_chunks then
                      DonkeyOneFile.remove_client_slot c;
                | (begin_pos, end_pos) :: _ ->
                    up.up_pos <- begin_pos;
                    up.up_end_chunk <- end_pos;
                    send_client_block c sock per_client
              end
            else
(* small block from chunk *)
              begin
                send_small_block c sock up.up_file up.up_pos 
                  msg_block_size_int;
                up.up_pos <- up.up_pos ++ (Int64.of_int msg_block_size_int);
                let per_client = per_client-msg_block_size_int in
                send_client_block c sock per_client
              end
        | _ -> ()
    
    let upload_to_client c size = 
(*      lprintf "upload_to_client %d\n" size; *)
      do_if_connected  c.client_source.DonkeySources.source_sock (fun sock ->
(*    lprintf "upload_to_client %d connected\n"  (maxi max_msg_size size); *)
          
          let size = min max_msg_size size in
          send_client_block c sock size;
           (match c.client_upload with
            | Some ({ up_chunks = _ :: _; _ }) ->
                if !CommonGlobals.has_upload = 0 then
                  CommonUploads.ready_for_upload (as_client c)
            | _ -> ()
          )
      )
    let _ =
      client_ops.op_client_can_upload <- upload_to_client
    
  end
