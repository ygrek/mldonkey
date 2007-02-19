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
open CommonInteractive
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
exception Cache_table_hit of string * string
type cache_entry = {
  md4 : Md4.t;
  begin_offset : int64;
  end_offset : int64;
  cached_part : string;
  comp_part : string
  }
let cache_table_index = ref 0
let cache_table_size = !!upload_compression_table_size
let ( cache_table : cache_entry Weak.t ) = Weak.create cache_table_size
let cached_load file begin_offset end_offset compress=
  try
    for i = 0 to cache_table_size-1 do
      match Weak.get cache_table i with
        Some ({md4=md4;begin_offset=bo;end_offset=eo;cached_part=cached_file;comp_part=cached_comp}) when (md4 = file.file_md4) && (bo=begin_offset) && (eo=end_offset) ->
          if !verbose_upload then
            lprintf_nl "Cache Hit for %s (%Ld,%Ld)" (file_best_name file) begin_offset end_offset;
          if (compress && (String.length cached_comp > 0)) || not compress then raise (Cache_table_hit(cached_file,cached_comp))
        | _ -> ()
    done;
    let entry_length = Int64.to_int(end_offset -- begin_offset) in
    let cached_file = String.create entry_length in
    Unix32.read (file_fd file) begin_offset cached_file 0 entry_length;
    let cached_comp = if compress then
      Zlib.compress_string ~level:!!upload_compression_level cached_file
    else
      ""
    in
    cache_table_index := (!cache_table_index + 1) mod cache_table_size;
    let (entry : cache_entry)={
      md4=file.file_md4;
      begin_offset=begin_offset;
      end_offset=end_offset;
      cached_part=cached_file;
      comp_part=cached_comp} in
      Weak.set cache_table !cache_table_index (Some entry);
    if !verbose_upload then
      lprintf_nl "Cache Miss for %s (%Ld,%Ld) orig.len %d comp.len %d" (file_best_name file) begin_offset end_offset (String.length cached_file) (String.length cached_comp);
    Some (cached_file,cached_comp)
  with
    | Cache_table_hit (cached_file,cached_comp) -> Some (cached_file,cached_comp)
    | End_of_file -> if !verbose then lprintf_nl
	       "End_of_file in cached_load file %s size %Ld begin %Ld end %Ld" (file_best_name file) (file_size file) begin_offset end_offset; None

  
module NewUpload = struct
    
    
    let check_end_upload c sock = ()
(*
      if c.client_bucket = 0 then
	direct_client_send sock (
	  let module M = DonkeyProtoClient in
	  let module Q = M.CloseSlot in
	    M.CloseSlotReq Q.t)
*)
    
    let rec send_small_block_plain c sock file begin_offset cfile pos len_int sixtyfour = 
      try
	if !verbose_upload then
          lprintf_nl "Sending plain %s to %s, begin_offset %Ld pos %d len %d"
	    (file_best_name file) (full_client_identifier c)
          (begin_offset) (pos) (len_int);
        
        let msg =  
          (
            let module M = DonkeyProtoClient in
            let module B = M.Bloc in
            M.BlocReq {  
              B.md4 = file.file_md4;
              B.usesixtyfour = sixtyfour;
              B.start_pos = begin_offset ++ (Int64.of_int pos);
              B.end_pos = begin_offset ++ (Int64.of_int(pos + len_int));
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
        String.blit cfile pos upload_buffer slen len_int;
        let uploaded = Int64.of_int len_int in
        count_upload c uploaded;
	CommonUploads.consume_bandwidth len_int;
        (match file.file_shared with None -> ()
          | Some impl ->
              shared_must_update_downloaded (as_shared impl);
              impl.impl_shared_uploaded <- 
                impl.impl_shared_uploaded ++ uploaded);
        
        write_string sock upload_buffer;
        check_end_upload c sock
      with
      | e -> if !verbose then lprintf_nl
	       "Exception %s in send_small_block_plain" (Printexc2.to_string e)
    
    let rec send_small_block_compressed c sock file begin_offset ccomp pos len_int pay_len sixtyfour = 
      try
        if !verbose_upload then
          lprintf_nl "Sending compressed %s to %s, begin_offset %Ld pos %d len %d"
			 (file_best_name file) (full_client_identifier c)
          (begin_offset) (pos) (len_int);
        
        let msg =  
          (
            let module M = DonkeyProtoClient in
            let module B = M.EmuleCompressedPart in
            M.EmuleCompressedPart {  
              B.md4 = file.file_md4;
              B.usesixtyfour = sixtyfour;
              B.statpos = begin_offset;
              B.newsize = Int64.of_int pay_len;
              B.bloc = "";
            }
          ) in
        let s = client_msg_to_string c.client_emule_proto msg in
        let slen = String.length s in
        let upload_buffer = String.create (slen + len_int) in
        String.blit s 0 upload_buffer 0 slen;
        DonkeyProtoCom.new_string msg upload_buffer;
        String.blit ccomp pos upload_buffer slen len_int;
        CommonUploads.consume_bandwidth len_int;
        
        write_string sock upload_buffer;
        check_end_upload c sock
      with
      | e -> if !verbose then lprintf_nl
	       "Exception %s in send_small_block_compressed" (Printexc2.to_string e)
    
    let rec send_client_block c sock per_client =
      try
      if per_client > 0 && CommonUploads.can_write_len sock max_msg_size then
        match c.client_upload with
        | Some ({ up_chunks = (begin_offset,end_offset) :: chunks } as up)  ->
            if file_is_largefile up.up_file && c.client_emule_proto.emule_largefiles <> 1 then begin
              DonkeyOneFile.remove_client_slot c;
              lprintf_nl "File %s is too large for %s." (file_best_name up.up_file) (full_client_identifier c);
            end else
            if up.up_file.file_shared = None then
(* Is there a message to warn that a file is not shared anymore ? *)
              DonkeyOneFile.remove_client_slot c
              else
            let compress =
              !!upload_compression &&
              (c.client_emule_proto.emule_compression <> 0) &&
              not (List.mem (String.lowercase (Filename2.last_extension2 (file_best_name up.up_file)))
                  !!upload_compression_ext_exclude)
            in
            let cfile,ccomp = match cached_load up.up_file begin_offset end_offset compress with
              Some (cached_file,cached_comp) -> cached_file,cached_comp
              | _ -> "",""
            in
            let compressed = compress && ((String.length ccomp) + !!upload_compression_threshold < (String.length cfile)) in
            let pay_len = if compressed then (String.length ccomp) else (String.length cfile) in
            let pos = Int64.to_int (up.up_pos -- begin_offset) in
            let max_len = pay_len - pos in
            let allowed_msg_block_size_int = min msg_block_size_int per_client in
            let sixtyfour = end_offset >= old_max_emule_file_size in
            if max_len <= allowed_msg_block_size_int then
(* last block from chunk *)
              begin
                if compressed then
                  begin
                    send_small_block_compressed c sock up.up_file begin_offset ccomp pos max_len pay_len sixtyfour;
                    let uploaded = end_offset -- begin_offset in
                    count_upload c uploaded;
                    (match up.up_file.file_shared with None -> ()
                      | Some impl ->
                        shared_must_update_downloaded (as_shared impl);
                        impl.impl_shared_uploaded <- 
                        impl.impl_shared_uploaded ++ uploaded)
                  end
                else 
                  send_small_block_plain c sock up.up_file begin_offset cfile pos max_len sixtyfour
                ;
                if !verbose_upload then
                    lprintf_nl "End of chunk %Ld %Ld %s" begin_offset end_offset (file_best_name up.up_file);
		up.up_flying_chunks <- up.up_flying_chunks @ [(begin_offset,end_offset)];
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
              if allowed_msg_block_size_int >= msg_block_size_int then
                begin
                  if compressed then
              begin
                      send_small_block_compressed c sock up.up_file begin_offset ccomp pos msg_block_size_int pay_len sixtyfour;
                    end
                  else
                    begin
                      send_small_block_plain c sock up.up_file begin_offset cfile pos msg_block_size_int sixtyfour;
                    end
                  ;
                up.up_pos <- up.up_pos ++ (Int64.of_int msg_block_size_int);
                let per_client = per_client-msg_block_size_int in
                send_client_block c sock per_client
              end
        | _ -> ()
      with
      | e -> if !verbose then lprintf_nl
	       "Exception %s in send_client_block" (Printexc2.to_string e)
    
    let upload_to_client c size = 
(*      lprintf "upload_to_client %d\n" size; *)
      do_if_connected  c.client_source.DonkeySources.source_sock (fun sock ->
(*    lprintf "upload_to_client %d connected\n"  (maxi max_msg_size size); *)
          
          let size = min max_msg_size size in
          send_client_block c sock size;
           (match c.client_upload with
            | Some ({ up_chunks = _ :: _ }) ->
                if !CommonUploads.has_upload = 0 then
                  CommonUploads.ready_for_upload (as_client c)
	    | _ -> ()
          )
      )
    let _ =
      client_ops.op_client_can_upload <- upload_to_client
    
  end
