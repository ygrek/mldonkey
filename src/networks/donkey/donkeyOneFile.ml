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
  
open BasicSocket
open TcpBufferedSocket

open CommonSwarming
open CommonSources
open CommonInteractive
open CommonGlobals
open CommonFile
open CommonClient
open CommonTypes
open CommonOptions

open DonkeyProtoCom
open DonkeyTypes
open DonkeyOptions
open DonkeyComplexOptions
open DonkeyGlobals
open DonkeyStats
    
(* Sort files in clients file queue in order of priority and percentage downloaded
   This way higher priority files will be asked/downloaded first if the client does have more
   than one file to offer.
   Only sort if client_block is not set.
   Once the block has been finished allow changing order.
*)

let sort_file_queue c =
  match c.client_download with
    Some _ -> ()
  | None ->
      (match c.client_file_queue with
        [] -> ()
      | [ (file, chunks, up) ] ->
          if !verbose_download || c.client_debug then
              lprintf_nl "sort_file_queue: single file. client(%d): %s, file(%d): %s" (client_num c) c.client_name (file_num file) (file_best_name file);
      | (file, chunks, up) :: _ ->
          let fn = file_num file in
          if !verbose_download || c.client_debug then
              lprintf_nl "sort_file_queue: multiple files. client(%d): %s, file(%d): %s" (client_num c) c.client_name (file_num file) (file_best_name file);
          c.client_file_queue <- List.stable_sort (fun (f1, _, _) (f2, _, _) ->
              let v = file_priority f2 - file_priority f1 in
              if v <> 0 then v else
              let s1 = if (file_size f1) > Int64.zero then
                  Int64.to_int ((file_downloaded f1) ** 100L // (file_size f1))
                else 0 in
              let s2 = if (file_size f2) > Int64.zero then
                  Int64.to_int ((file_downloaded f2) ** 100L // (file_size f2))
                else 0 in
              s2 - s1
          ) c.client_file_queue;
          (match c.client_file_queue with
            [] -> ()
          | (file, chunks, _) :: _ ->
              if (file_num file) <> fn && (!verbose_download || c.client_debug) then
                lprintf_nl "sort_file_queue: queue change. client(%d): %s, file(%d): %s" (client_num c) c.client_name (file_num file) (file_best_name file)))

let remove_client_slot c =
  if c.client_debug || (
     !verbose &&
     (c.client_session_uploaded > 0L || c.client_session_downloaded > 0L)) then
    lprintf_nl "Client[%d] %s disconnected, connected %s%s%s"
      (client_num c) (full_client_identifier c)
      (Date.time_to_string (last_time () - c.client_connect_time) "verbose")
      (if c.client_total_uploaded > 0L then
        Printf.sprintf ", send %s (%s)%s"
          (size_of_int64 c.client_session_uploaded)
          (size_of_int64 c.client_total_uploaded)
          (match client_upload (as_client c) with | None -> ""
           | Some f -> " of " ^ (CommonFile.file_best_name f)) else "")
      (if c.client_total_downloaded > 0L then
         Printf.sprintf ", rec %s (%s)%s"
          (size_of_int64 c.client_session_downloaded)
          (size_of_int64 c.client_total_downloaded)
          (match c.client_download with | None -> ""
           | Some (f,_) -> " of " ^ (file_best_name f)) else "");
  set_client_has_a_slot (as_client c) NoSlot;
  client_send c (
    let module M = DonkeyProtoClient in
    let module Q = M.OutOfParts in
    M.OutOfPartsReq Q.t);
  c.client_session_uploaded <- 0L;
  c.client_upload <- None

let unshare_file file =
  match file.file_shared with
    None -> ()
  | Some s -> 
      if !verbose_share || !verbose then
        lprintf_nl "Unsharing file %s" (file_best_name file);
      List.iter (fun s ->
        s.server_sent_shared <- List2.removeq file s.server_sent_shared;
      ) (connected_servers ());
      file.file_shared <- None;
      decr nshared_files;
      CommonShared.shared_calculate_total_bytes ();
      (try Unix32.close  (file_fd file) with _ -> ());
      
      begin
        H.iter (fun c ->
            match c.client_upload with
              Some { up_file = f; _ } when f == file ->
                remove_client_slot c
            | _ -> ()
        ) clients_by_kind
      end

let declare_completed_file file = 
  DonkeyShare.remember_shared_info file (file_disk_name file);
  file_completed (as_file file);
  CommonSwarming.remove_swarmer file.file_swarmer;
  file.file_swarmer <- None;
  unshare_file file;
  (try
      let format = CommonMultimedia.get_info
          (file_disk_name file) in
      file.file_format <- format
    with _ -> ())
  
(** What to do when a file is finished
  @param file the finished file
*)         
let download_finished file = 
  if List.memq file !current_files then begin      
      current_files := List2.removeq file !current_files;
      declare_completed_file file
    end

(** Check if a file is finished or not.
  A file is finished if all blocks are verified.
  @param file The file to check status
*)
let check_file_downloaded file = 
  match file_state file with
    FileCancelled | FileShared | FileDownloaded -> ()
  | _ ->
      match file.file_swarmer with
        None -> ()
      | Some swarmer ->
          let bitmap = CommonSwarming.chunks_verified_bitmap swarmer in
(*          lprintf "Verified bitmap: [%s]\n" bitmap; *)
          let verified = VB.for_all (( = ) VB.State_verified) bitmap in
          
          let downloaded = CommonSwarming.downloaded swarmer in
          if file_downloaded file <> downloaded then begin
              lprintf_nl "ERROR: file_downloaded file (%Ld) <> downloaded swarmer (%Ld)"
                (file_downloaded file) downloaded
            end;
          
          if verified then begin
              if (file_size file <> downloaded)
              then
                lprintf_nl "Downloaded size differs after complete verification";
              download_finished file
            end
            
let check_files_downloaded () =
  List.iter check_file_downloaded !current_files;
  Hashtbl.iter (fun file_md4 file ->
      match file.file_shared with 
        Some _ -> ()
      | None ->
          match file.file_swarmer with
            None -> ()
          | Some swarmer ->
              let bitmap = CommonSwarming.chunks_verified_bitmap swarmer in
              if VB.existsi (fun _ s -> s = VB.State_verified) bitmap then
                DonkeyShare.must_share_file file
  ) files_by_md4

let add_client_chunks c file client_chunks =
  match file.file_swarmer with
    None -> failwith "add_client_chunks: no swarmer"
  | Some swarmer ->
      let rec iter list =
        match list with
          (f, chunks, up) :: tail ->
            if f != file then iter tail
            else begin
                CommonSwarming.update_uploader_intervals up
                (AvailableBitv client_chunks);
                Bitv.blit client_chunks 0 chunks 0 (Bitv.length chunks)
              end
            
        | [] ->
            let up = CommonSwarming.register_uploader swarmer (as_client c) 
              (AvailableBitv client_chunks) in
            c.client_file_queue <-  c.client_file_queue @
              [file, client_chunks, up]
      in
      iter c.client_file_queue
      

(* let next_file _ = failwith "next_file not implemented" *)
      
let send_get_range_request c file ranges = 
  let rec check_large (rangelist : (int64 * int64 * range) list) =
    match rangelist with
    | [] -> false
    | (x,y,_ : (int64 * int64 * range))::tail_range ->
            (x > old_max_emule_file_size) || (y > old_max_emule_file_size) || (check_large tail_range)
  in
  let is_large_request = check_large ranges in
  if file_is_largefile file && c.client_emule_proto.emule_largefiles <> 1 then
    lprintf_nl "File %s is too large for %s." (file_best_name file) (full_client_identifier c)
  else
  match c.client_source.DonkeySources.source_sock with
  | Connection sock ->
      
      set_rtimeout sock !queue_timeout;
      let module M = DonkeyProtoClient in
      let module Q = M.QueryBloc in
      let msg, len =
        match ranges with
          [x1,y1,_] ->
            {
              Q.md4 = file.file_md4;
              Q.usesixtyfour = is_large_request;
              Q.start_pos1 = x1;
              Q.end_pos1 = y1;
              Q.start_pos2 = zero;
              Q.end_pos2 = zero;
              Q.start_pos3 = zero;
              Q.end_pos3 = zero;
            }, y1 -- x1
        
        | [x1,y1,_; x2,y2,_] ->
            {
              Q.md4 = file.file_md4;
              Q.usesixtyfour = is_large_request;
              Q.start_pos1 = x1;
              Q.end_pos1 = y1;
              Q.start_pos2 = x2;
              Q.end_pos2 = y2;
              Q.start_pos3 = zero;
              Q.end_pos3 = zero;
            }, y1 -- x1
        
        | [x1,y1,_; x2,y2,_; x3,y3,_ ] ->
            {
              Q.md4 = file.file_md4;
              Q.usesixtyfour = is_large_request;
              Q.start_pos1 = x1;
              Q.end_pos1 = y1;
              Q.start_pos2 = x2;
              Q.end_pos2 = y2;
              Q.start_pos3 = x3;
              Q.end_pos3 = y3;
            }, y1 -- x1
        
        | _ -> assert false
      in
      let msg = M.QueryBlocReq msg in
      set_read_power sock (!!upload_power + max 0 (file_priority file));
(*      lprintf "QUEUE DOWNLOAD REQUEST\n"; *)
(*      CommonUploads.queue_download_request (fun _ ->  *)
          client_send c msg 
(*          ) (Int64.to_int len) *)
  | _ -> assert false
      
let rec get_from_client c =
  match c.client_download with
    None ->
(*      lprintf "get_from_client: no download\n"; *)
      begin
        match c.client_file_queue with
          [] -> 
            
(*            lprintf "get_from_client: no more file\n"; *)
            if not (client_has_a_slot (as_client c)) then begin
(*                connection_delay c.client_connection_control; *)
                match c.client_source.DonkeySources.source_sock with
                  Connection sock ->
                    TcpBufferedSocket.close sock
                      (Closed_for_error "No file to download");
                | _ -> ()
              end
        
        | (file, chunks, up) :: tail ->

(* Should we start a download without asking for a slot first ?? *)
(*            lprintf "get_from_client: next file\n"; *)
            c.client_download <- Some (file,up);
            get_from_client c
      end
  
  | Some (file,up) ->
      
      try
        let _ = CommonSwarming.current_blocks up in
        let ranges = CommonSwarming.current_ranges up in
        let before_find_range = List.length ranges in

(*        lprintf "WAITING FOR %d BLOCS\n" before_find_range; *)
        if before_find_range < 3 then
          let rec iter n =
            if n < 3 then
              try
                ignore (CommonSwarming.find_range up zone_size);
                iter (n+1)
              with 
                Not_found -> n
            else n
          in
          let after_find_range = iter before_find_range in
          if after_find_range > before_find_range then 
            
            let ranges = CommonSwarming.current_ranges up in
            send_get_range_request c file ranges;
            
          else
(* No new range to download in this block *)
              match ranges with
                [] ->
                  raise Not_found (* will query the next block *)
                  
              | _ -> 
(* Wait for the already requested ranges before requesting the next block *)
                  ()                  
              
        else
(* We already have 3 ranges in the current block *)
          ()
      with Not_found ->
(*          lprintf "get_from_client: no range\n"; *)
          try
            let swarmer = CommonSwarming.uploader_swarmer up in       
            (try CommonSwarming.verify_one_chunk swarmer with _ -> ());
            let _ = CommonSwarming.find_blocks up in
            get_from_client c
            
          with Not_found ->
(*              lprintf "get_from_client: no block\n"; *)
              match CommonSwarming.current_ranges up with
                [] ->
(* We have nothing to wait for in the current file *)
                  begin
                    
(*                    lprintf "get_from_client: no expected ranges\n"; *)
                    
                    c.client_download <- None;
                    CommonSwarming.unregister_uploader up;
                    match c.client_file_queue with
                      [] -> assert false
                    | _ :: tail -> 
(* We can go to next file now *)
                        c.client_file_queue <- tail;
                        get_from_client c
                  end
              | _ -> 
(* We are still waiting for the previous requested ranges from this file *)
                  ()
                  
      
(* start_download: ask for a slot in the queue of the remote client,
  or start querying blocks if already in the queue *)
let request_slot c = 
    if c.client_slot = SlotNotAsked then begin
      if !verbose_download then lprintf_nl "start_download";
      do_if_connected c.client_source.DonkeySources.source_sock (fun sock ->
          sort_file_queue c;
          match c.client_file_queue with
            [] -> ()
          | (file, _,_ ) :: _ ->
              client_send c (
                let module M = DonkeyProtoClient in
                let module Q = M.JoinQueue in
                M.JoinQueueReq (Some file.file_md4));
              c.client_slot <- SlotAsked;
      )
    end
  
let block_received c md4 begin_pos bloc bloc_pos bloc_len = 
    
  match c.client_download with
    None -> 
      if !verbose then
        lprintf_nl "block_received for unknown file (md4 %s) for data from %s"
          (Md4.to_string md4) (full_client_identifier c)
  | Some (file, up) ->
      
      if file.file_md4 <> md4 then begin
          if !verbose then
            lprintf_nl "block_received for wrong file, received: %s, expected: %s, from %s"
              (Md4.to_string md4)
              (Md4.to_string file.file_md4)
              (full_client_identifier c)
        end else begin
          DonkeySources.set_request_result c.client_source file.file_sources File_upload;
          
          c.client_rating <- c.client_rating + 10;
          
          set_client_state c (Connected_downloading (file_num file));
          let len64 = Int64.of_int bloc_len in

(* TODO: verify the received data has been requested *)
          
          let swarmer = CommonSwarming.uploader_swarmer up in
          let old_downloaded = CommonSwarming.downloaded swarmer in
          
          begin
            try
              CommonSwarming.received up begin_pos bloc bloc_pos bloc_len
            with
            | e ->
                let m =
(*          Printf.sprintf "File %s begin_pos=%s bloc_begin=%d bloc_len=%d:\nError %s while writing block%s\n" (file_best_name file) (Int64.to_string begin_pos) t.Q.bloc_begin t.Q.bloc_len (Printexc2.to_string e)  *)
                  (match e with 
                      Unix.Unix_error (Unix.ENOSPC, _, _) -> " (Disk full?)"
                    | _ -> "") in
(*                    Printf2.lprint_string m; *)
                CommonEvent.add_event (Console_message_event m);
                if e <> End_of_file then begin
                    let m = Printf.sprintf "File %s paused, exception %s.\n"
                              (file_best_name file) (Printexc2.to_string e) in
                    Printf2.lprint_string m;
                    CommonEvent.add_event (Console_message_event m);
                    file_pause (as_file file) (CommonUserDb.admin_user ());
                    raise e
                  end
          
          end;

(*            List.iter CommonSwarming.alloc_range c.client_ranges; *)
          let new_downloaded = 
            CommonSwarming.downloaded swarmer in
          count_download c (new_downloaded -- old_downloaded);
(*
          if not (List.mem c.client_ip bb.block_contributors) then
            bb.block_contributors <- c.client_ip :: 
            bb.block_contributors;
*)          
          
          
          if new_downloaded -- old_downloaded < len64 then
              if !verbose_download then begin
                lprintf_nl "ALREADY RECEIVED: %Ld < %Ld"
              (new_downloaded -- old_downloaded) len64;          
              end;
          get_from_client c
        end    
        
let search_found filter search md4 tags =
  let file_name = ref "" in
  let file_size = ref Int64.zero in
  let availability = ref 0 in
  let new_tags = ref [] in
  List.iter (fun tag ->
      match tag with
        { tag_name = Field_Filename; tag_value = String s } -> file_name := s
      | { tag_name = Field_Size; tag_value = Uint64 v } -> file_size := v
      | { tag_name = Field_Size_Hi; tag_value = Uint8 v } ->
          file_size := Int64.logor !file_size (Int64.shift_left (Int64.of_int v) 32)
      | { tag_name = Field_Availability; tag_value = (Uint64 v| Fint64 v) } ->
          availability := Int64.to_int v;  new_tags := tag :: !new_tags
      | _ -> new_tags := tag :: !new_tags
  ) tags;
(*  TODO INDEX try
    let rs = DonkeyIndexer.find_result md4 in
    CommonInteractive.search_add_result filter search rs.result_result; 
(* TODO ADD AVAILABILITY *)
    let doc = rs.result_index in
    let result = Store.get store doc in
    if not (List.mem !file_name result.result_names) then begin
        DonkeyIndexer.add_name result !file_name;
        result.result_names <- !file_name :: result.result_names
      end
  with _ -> *)
      match result_of_file md4 tags with
        None -> ()
      | Some r ->
      try
(* TODO INDEX       let rs = DonkeyIndexer.index_result new_result in 
        let doc = rs.result_index in
        CommonInteractive.search_add_result filter search rs.result_result;
       let result = Store.get store doc in
*)
        CommonInteractive.search_add_result filter search r;
        ()
      with _ ->  (* the file was probably filtered *)
          ()
