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

open CommonShared
open CommonServer
open CommonComplexOptions
open GuiProto
open CommonClient
open CommonFile
open CommonUser
open CommonSearch
open CommonTypes
open Options
open BasicSocket
open TcpBufferedSocket
open DonkeyMftp
open DonkeyOneFile
open DonkeyProtoCom
open DonkeyTypes
open DonkeyGlobals
open DonkeyComplexOptions
open DonkeyOptions
open CommonOptions
open DonkeyClient  
open CommonGlobals
open DonkeyStats

let verbose_upload = false
      
let msg_block_size_int = 10000
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
    
    let rec send_small_block c sock file begin_pos len_int = 
(*      lprintf "send_small_block %d\n" len_int; *)
(*      let len_int = Int32.to_int len in *)
      CommonUploads.consume_bandwidth len_int;
      try
        if !verbose then begin
            lprintf "send_small_block(%s-%s) %Ld %d\n"
              c.client_name (brand_to_string c.client_brand)
            (begin_pos) (len_int);
          end;
        
        let msg =  
          (
            let module M = DonkeyProtoClient in
            let module B = M.Bloc in
            M.BlocReq {  
              B.md4 = file.file_md4;
              B.start_pos = begin_pos;
              B.end_pos = Int64.add begin_pos (Int64.of_int len_int);
              B.bloc_str = "";
              B.bloc_begin = 0;
              B.bloc_len = 0; 
            }
          ) in
        let s = client_msg_to_string c.client_emule_proto msg in
        let slen = String.length s in
        let upload_buffer = String.create (slen + len_int) in
        String.blit s 0 upload_buffer 0 slen;
        DonkeyProtoCom.new_string msg upload_buffer;
        Unix32.read (file_fd file) begin_pos upload_buffer slen len_int;
        let uploaded = Int64.of_int len_int in
        count_upload c file uploaded;
        (match file.file_shared with None -> ()
          | Some impl ->
              shared_must_update_downloaded (as_shared impl);
              impl.impl_shared_uploaded <- 
                Int64.add impl.impl_shared_uploaded uploaded);
        if c.client_connected then
          printf_string "U[OUT]"
        else
          printf_string "U[IN]";
        
        write_string sock upload_buffer;
        check_end_upload c sock
      with e -> 
          if !verbose_hidden_errors then lprintf "Exception %s in send_small_block\n" (Printexc2.to_string e) else ()
    
    let rec send_client_block c sock per_client =
(*      lprintf "send_client_block\n"; *)
      if per_client > 0 && CommonUploads.remaining_bandwidth () > 0 then
        match c.client_upload with
        | Some ({ up_chunks = _ :: chunks } as up)  ->
            if up.up_file.file_shared = None then begin
(* Is there a message to warn that a file is not shared anymore ? *)
                c.client_upload <- None;
              end else
            let max_len = Int64.sub up.up_end_chunk up.up_pos in
            let max_len = Int64.to_int max_len in
            let msg_block_size_int = mini msg_block_size_int per_client in
            if max_len <= msg_block_size_int then
(* last block from chunk *)
              begin
                if verbose_upload then begin
                    lprintf "END OF CHUNK (%d) %Ld\n" max_len up.up_end_chunk; 
                  end;
                send_small_block c sock up.up_file up.up_pos max_len;
                up.up_chunks <- chunks;
                let per_client = per_client - max_len in
                match chunks with
                  [] -> 
                    if verbose_upload then begin
                        lprintf "NO CHUNKS\n"; 
                      end;
                    c.client_upload <- None;
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
                up.up_pos <- Int64.add up.up_pos 
                  (Int64.of_int msg_block_size_int);
                let per_client = per_client-msg_block_size_int in
                if can_write_len sock max_msg_size then
                  send_client_block c sock per_client
              end
        | _ -> ()
    
    let upload_to_client c size = 
(*      lprintf "upload_to_client %d\n" size; *)
      do_if_connected  c.client_source.DonkeySources.source_sock (fun sock ->
(*    lprintf "upload_to_client %d connected\n"  (maxi max_msg_size size); *)
          
(* changed in 2.5.27: mini instead of maxi *)
          let size = mini max_msg_size size in
          if CommonUploads.can_write_len sock size then 
            begin
(*              lprintf "can_write_len %d\n"  (maxi max_msg_size size); *)
              send_client_block c sock size;
            end;
          (match c.client_upload with
              None -> ()
            | Some up ->
                if !CommonUploads.has_upload = 0 then
                  CommonUploads.ready_for_upload (as_client c)
          )
      )
    let _ =
      client_ops.op_client_can_upload <- upload_to_client
    
  end
