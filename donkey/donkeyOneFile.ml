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
open CommonSearch
open CommonGlobals
open CommonComplexOptions
open CommonFile  
open CommonClient
open CommonComplexOptions
open CommonTypes
open Options
open BasicSocket
open TcpBufferedSocket
open DonkeyMftp
open DonkeyImport
open DonkeyProtoCom
open DonkeyTypes
open DonkeyOptions
open CommonOptions
open DonkeyComplexOptions
open DonkeyGlobals          
  
let chunk_pos i =
  Int64.mul (Int64.of_int i)  block_size

let chunk_end file i =
  let pos = Int64.mul (Int64.of_int (i+1))  block_size in
  if pos > file_size file then 
    file_size file else pos
    
let new_block file i =
  
  let begin_pos = chunk_pos i in
  let end_pos = chunk_end file i in
  {
    block_present = false;
    block_begin = begin_pos;
    block_end = end_pos;
    block_nclients = 0;
    block_zones = [];
    block_pos = i;
    block_file = file;
  } 

let zone_present z =
  z.zone_begin >= z.zone_end

let sort_zones b =
  let zones = List.fold_left (fun zones z ->
        if zone_present z then zones else z :: zones
    ) [] b.block_zones 
  in
  b.block_zones <- Sort.list (fun z1 z2 ->
      z1.zone_nclients < z2.zone_nclients ||
      (z1.zone_nclients == z2.zone_nclients &&
        z1.zone_begin < z2.zone_begin)
  ) zones
      
      
let rec create_zones file begin_pos end_pos list =
(*  Printf.printf "create_zones for %ld-%ld"
    begin_pos end_pos;
  print_newline (); *)
  if begin_pos = end_pos then list
  else
  let zone_end = Int64.add begin_pos zone_size in
(*  Printf.printf "ZONE END %ld" zone_end; print_newline ();*)
  let zone_end2 = if zone_end > end_pos then begin
(*        Printf.printf "%ld > %ld" zone_end
         end_pos
        
        ; print_newline (); *)
        end_pos
        
      end else zone_end in
(*  Printf.printf "CORRECTED ZONE END %ld" zone_end; print_newline (); *)
  create_zones file zone_end2 end_pos ({
      zone_begin = begin_pos;
      zone_end = zone_end2;
      zone_nclients = 0;
    } :: list ) 
  
let client_file c =
  match c.client_file_queue with
    [] -> failwith "No file for this client"
  | (file, _) :: _ -> file

let download_fifo = Fifo.create ()

  
let clean_client_zones c =
  match c.client_block with None -> ()
  | Some b ->
      c.client_block <- None;
      b.block_nclients <- b.block_nclients - 1;
      List.iter (fun z ->
          z.zone_nclients <- z.zone_nclients - 1) c.client_zones;
      sort_zones b;
      c.client_zones <- []
  
      
let query_zones c b = 
  let file = client_file c in
  sort_zones b;
  match c.client_sock with
    None -> assert false
  | Some sock ->
      
      set_rtimeout sock !queue_timeout;
        let module M = DonkeyProtoClient in
        let module Q = M.QueryBloc in
      let msg, len =           
        match c.client_zones with
            [z] ->
              {
                Q.md4 = file.file_md4;
                Q.start_pos1 = z.zone_begin;
                Q.end_pos1 = z.zone_end;
                Q.start_pos2 = Int64.zero;
                Q.end_pos2 = Int64.zero;
                Q.start_pos3 = Int64.zero;
                Q.end_pos3 = Int64.zero;
              }, Int64.to_int (Int64.sub z.zone_end z.zone_begin)
          
          | [z1;z2] ->
              {
                Q.md4 = file.file_md4;
                Q.start_pos1 = z1.zone_begin;
                Q.end_pos1 = z1.zone_end;
                Q.start_pos2 = z2.zone_begin;
                Q.end_pos2 = z2.zone_end;
                Q.start_pos3 = Int64.zero;
                Q.end_pos3 = Int64.zero;
              }, Int64.to_int (Int64.sub z1.zone_end z1.zone_begin)
          
          | [z1;z2;z3] ->
              {
                Q.md4 = file.file_md4;
                Q.start_pos1 = z1.zone_begin;
                Q.end_pos1 = z1.zone_end;
                Q.start_pos2 = z2.zone_begin;
                Q.end_pos2 = z2.zone_end;
                Q.start_pos3 = z3.zone_begin;
                Q.end_pos3 = z3.zone_end;
              }, Int64.to_int (Int64.sub z1.zone_end z1.zone_begin)
          
          | _ -> assert false
      in
      let msg = M.QueryBlocReq msg in
      set_read_power sock (c.client_power + maxi 0 (file_priority file));
      if !!max_hard_download_rate <> 0 then begin
(*          Printf.printf "CLIENT: put in download fifo"; print_newline (); *)
        Fifo.put download_fifo (sock, msg, len)
        end else
        direct_client_send c msg
        

        
(* create a list with all absent intervals *)

let put_absents file =

  let temp_chunk chunk =
    match chunk with
      PresentTemp | AbsentTemp | PartialTemp _ -> true
    | _ -> false
  in
  
  let rec iter_chunks_in i zs =
(*    Printf.printf "iter_chunks_in %d" i; print_newline (); *)
    if i < file.file_nchunks then 
    match zs with
      [] -> ()
      | (begin_pos, end_pos) :: tail ->
          (*
          Printf.printf "begin_pos %Ld (chnk end %Ld)" begin_pos
            (chunk_end file i); print_newline (); *)
          if begin_pos >= chunk_end file i then
            iter_chunks_in (i+1) zs
          else
          if end_pos <= chunk_pos i then
            iter_chunks_in i tail
          else
          if begin_pos <= chunk_pos i && end_pos >= chunk_end file i then begin
(*              Printf.printf "full absent chunk %d" i; print_newline (); *)
              file.file_chunks.(i) <- (
                if temp_chunk file.file_chunks.(i) then AbsentTemp else 
                  AbsentVerified);
              iter_chunks_in (i+1) ((chunk_end file i, end_pos) :: tail)
            end else
            
          let b = new_block file i in
(*          Printf.printf "new_block %d" i; print_newline (); *)
          file.file_chunks.(i) <- (if temp_chunk file.file_chunks.(i) then 
              PartialTemp b else PartialVerified b);
          iter_blocks_in i b zs
        
  and iter_blocks_in i b zs =
(*    Printf.printf "iter_blocks_in %d" i; print_newline (); *)
    match zs with
      [] -> 
        sort_zones b
    | (begin_pos, end_pos) :: tail ->
        if begin_pos >= b.block_end then begin
(*            Printf.printf "need sort_zones..."; print_newline (); *)
            sort_zones b;
            iter_chunks_in (i+1) zs 
          end
        else
        if end_pos >= b.block_end then begin
(*            Printf.printf "need create_zones and sort_zones..."; print_newline (); *)
            b.block_zones <- create_zones file begin_pos b.block_end
              b.block_zones;
            sort_zones b;
            iter_chunks_in (i+1) ((b.block_end, end_pos) :: tail)
          end else begin
(*            Printf.printf "need create_zones ..."; print_newline (); *)
            b.block_zones <- create_zones file begin_pos end_pos b.block_zones;
            iter_blocks_in i b tail
          end
    
    
  in

  (*
  Printf.printf "nchunks %d" file.file_nchunks; print_newline ();
  
  List.iter (fun (i1,i2) ->
      Printf.printf "ABSENT: %Ld-%Ld" i1 i2; print_newline ();
  ) file.file_absent_chunks;
*)  
  
  iter_chunks_in 0 file.file_absent_chunks
  
      
let find_absents file =
  let rec iter_chunks_out i prev =
    if i = file.file_nchunks then prev else
    match file.file_chunks.(i) with
      AbsentTemp | AbsentVerified ->
        iter_chunks_in (i+1) (chunk_pos i) prev
    | PresentTemp | PresentVerified ->
        iter_chunks_out (i+1) prev
    | PartialTemp b | PartialVerified b ->
        let zs = Sort.list (fun z1 z2 ->
              z1.zone_begin <= z2.zone_begin
          ) b.block_zones in
        iter_blocks_out i zs prev
        
  and iter_chunks_in i begin_pos prev =
    if i = file.file_nchunks then (begin_pos, file_size file) :: prev else
    match file.file_chunks.(i) with
      AbsentTemp | AbsentVerified ->
        iter_chunks_in (i+1) begin_pos prev
    | PresentTemp |  PresentVerified ->
        iter_chunks_out (i+1) ((begin_pos, chunk_pos i) :: prev)
    | PartialTemp b | PartialVerified b ->
        let zs = Sort.list (fun z1 z2 ->
              z1.zone_begin <= z2.zone_begin
          ) b.block_zones in
        iter_blocks_in i zs begin_pos (chunk_pos i) prev
    
  and iter_blocks_in i zs begin_pos end_pos prev =
    match zs with
      [] -> 
        if end_pos = chunk_pos (i+1) then
          iter_chunks_in (i+1) begin_pos prev
        else
          iter_chunks_out (i+1) ((begin_pos, end_pos) :: prev)
    | z :: zs ->
        if end_pos = z.zone_begin then
          iter_blocks_in i zs begin_pos z.zone_end prev
        else
          iter_blocks_in i zs z.zone_begin z.zone_end
            ((begin_pos, end_pos) :: prev)
    
  and iter_blocks_out i zs prev =
    match zs with
      [] -> 
        iter_chunks_out (i+1) prev
    | z :: zs ->
        iter_blocks_in i zs z.zone_begin z.zone_end prev
        
  in
  iter_chunks_out 0 []
    
let compute_size file =
  if file_size file <> Int64.zero then
    
    let absents = ref Int64.zero in
    for i = 0 to file.file_nchunks - 1 do
      match file.file_chunks.(i) with
        PresentTemp | PresentVerified -> ()
      | AbsentTemp | AbsentVerified -> absents := Int64.add !absents (
            Int64.sub (chunk_end file i) (chunk_pos i))
      | PartialTemp b | PartialVerified b ->
          List.iter (fun z ->
              absents := Int64.add !absents (
                Int64.sub z.zone_end z.zone_begin)
          ) b.block_zones
    done;
    let current = Int64.sub (file_size file) !absents in
    file.file_file.impl_file_downloaded <- current;
    if file_downloaded file > file_size file then begin
        Printf.printf "******* downloaded %Ld > %Ld size after compute_size ***** for %s"
          (file_downloaded file)
        (file_size file)
        (file_best_name file);
        print_newline () 
      end;
    file_must_update file
    
let print_time tm =
  let module U = Unix in
  Printf.printf "TIME %d/%d/%d %2d:%02d:%02d" 
    tm.U.tm_mday tm.U.tm_mon tm.U.tm_year
    tm.U.tm_hour tm.U.tm_min tm.U.tm_sec;
  print_newline ()
  
    
let verify_chunk file i =
  if file.file_md4s = [] then file.file_chunks.(i) else
  let file_md4s = Array.of_list file.file_md4s in
  let begin_pos = chunk_pos i in
  let end_pos = chunk_end file i in
  let len = Int64.sub end_pos begin_pos in
  let md4 = file_md4s.(i) in
  if !verbose then begin
      Printf.printf "verify_chunk %s[%d] %Ld[%Ld]" 
        (file_disk_name file) i
      begin_pos len; print_newline ();
    end;
  let t1 = Unix.gettimeofday () in
  let new_md4 = Md4.digest_subfile (file_fd file) begin_pos len in
  let t2 = Unix.gettimeofday () in
  if !verbose then begin
      Printf.printf "Delay for MD4: %2.2f" (t2 -. t1); print_newline ();
    end;
  (*
  let mmap = Mmap.mmap file.file_name 
    (file_fd file) begin_pos len in
  let mmap_md4 = Mmap.md4_sub mmap Int64.zero len in
  Mmap.munmap mmap;
  if new_md4 <> mmap_md4 then begin
      Printf.printf "BAD md4 computation"; print_newline ();
      exit 1;
    end else begin
      Printf.printf "GOOD md4 computation"; print_newline ();
end;
  *)
  if new_md4 = md4 then begin
      DonkeyShare.must_share_file file;
      PresentVerified
    end else begin
(*
print_newline ();
Printf.printf "VERIFICATION FAILED";print_newline ();
Printf.printf "%s\n%s" (Md4.to_string md4) (Md4.to_string new_md4);
print_newline ();
*)
      AbsentVerified
    end      
    
let verify_file_md4 file i b =
  let state = verify_chunk file i in
  file.file_chunks.(i) <- (
    if state = PresentVerified then begin
        PresentVerified
      end
    else
    match b with
      PartialTemp bloc -> PartialVerified bloc
    | PresentTemp -> AbsentVerified
    | _ -> AbsentVerified);
  (*
  file.file_all_chunks.[i] <- (if state = PresentVerified then '1' else '0');
  *)
  () 
  
let rec find_client_zone c = 
  match c.client_block with
    None -> find_client_block c
  | Some b ->
(* client_zones : les zones en cours de telechargement *)
(* block_zones : les zones disponibles pour telechargement *)
(*
      Printf.printf "Current zones for client:";
      List.iter (fun z -> 
          Printf.printf "zone: %ld-%ld"
            (z.zone_begin) (z.zone_end)
      ) c.client_zones;
print_newline ();
  *)
      let z = match c.client_zones with
        | [z1] -> if zone_present z1 then [] else [z1]
        | [z1;z2] ->
            let z = if zone_present z2 then [] else [z2] in
            if zone_present z1 then z else z1 :: z 
        | [z1;z2;z3] ->
            let z = if zone_present z3 then [] else [z3] in
            let z = if zone_present z2 then z else z2 :: z in
            if zone_present z1 then z else z1 :: z 
        | _ -> []
      
      in
      let rem_zones = List.length b.block_zones in
(*      Printf.printf "Remaining %d zones" rem_zones; print_newline (); *)
      match z with
        [z1;z2;z3] -> ()
      | [z1;z2] when rem_zones <= 2 -> ()
      | [z1] when rem_zones <= 1 -> ()
      | [z1;z2] -> find_zone3 c b z1 z2 b.block_zones
      | [z1]  -> find_zone2 c b z1 b.block_zones
      | _ -> find_zone1 c b b.block_zones

and print_client_zones n b c =
  (match c.client_block with
      None -> Printf.printf "\n%d: CLIENT ZONES WITH NO BLOCK %d" 
          (client_num c) n;
        print_newline ();
    | Some bb ->
        if b != bb then begin
            Printf.printf "\n%d: CLIENT ZONES WITH BAD BLOCK %d" 
              (client_num c) n;
            print_newline ();
          end);          
  if !verbose then begin
      Printf.printf "\n%d: ZONES IN %d" (client_num c) n; 
      List.iter (fun z ->
          Printf.printf " [%Ld - %Ld] " (z.zone_begin)(z.zone_end);
          print_newline ();
      ) c.client_zones;
      print_newline ();
    end;

and find_zone3 c b z1 z2 zones =
  match zones with
    [] -> 
      c.client_zones <- [z1;z2]; 
      print_client_zones 1 b c;
      query_zones c b
  | z :: zones ->
      if (not (zone_present z)) && z != z1 && z != z2 then begin
          c.client_zones <- [z1;z2;z];
          print_client_zones 2 b c;
          z.zone_nclients <- z.zone_nclients + 1;
          query_zones c b
        end
      else find_zone3 c b z1 z2 zones

and find_zone2 c b z1 zones =
  match zones with
    [] -> 
      c.client_zones <- [z1]; 
      print_client_zones 3 b c;
      query_zones c b
  | z :: zones ->
      if (not (zone_present z)) && z != z1 then begin
          z.zone_nclients <- z.zone_nclients + 1;
          find_zone3 c b z1 z zones
        end
      else find_zone2 c b z1 zones

and find_zone1 c b zones =
  let file = client_file c in
  match zones with
    [] -> 
(* no block to download !! *)
      c.client_zones <- []; 
      printf_string "[BLOCK]";
      b.block_present <- true;
      b.block_nclients <- b.block_nclients - 1;      
      file.file_chunks.(b.block_pos) <- PresentTemp;
      let state = verify_chunk file b.block_pos in
      file.file_chunks.(b.block_pos) <- state;
(*      (file.file_all_chunks).[b.block_pos] <- (if state = PresentVerified 
then '1' else '0'); *)
      
      file.file_absent_chunks <- List.rev (find_absents file);
      c.client_block <- None;
      find_client_block c
  
  | z :: zones ->
      if (not (zone_present z)) then begin
          z.zone_nclients <- z.zone_nclients + 1;
          find_zone2 c b z zones
        end else
        find_zone1 c b zones

and zero_block file i =
(* disk fragmentation prevention:
   This should help the bad filesystems, and the others too ;)
   When a chunk is about to be used for the first time, zero it, 
   allocating all the disk space at once *)
  try
    (match file.file_chunks.(i) with
        AbsentTemp | AbsentVerified ->
(*	   Printf.printf "Allocating disk space";
	   print_newline (); *)
          let chunk_begin = chunk_pos i in
          let final_pos = Unix32.seek64 (file_fd file) 
            chunk_begin Unix.SEEK_SET in
          if final_pos <> chunk_begin then begin
              Printf.printf "BAD LSEEK %Ld/%Ld"
                (final_pos)
              (chunk_begin); print_newline ();
              raise Not_found
            end;
          let fd = try
              Unix32.force_fd (file_fd file) 
            with e -> 
                Printf.printf "In Unix32.force_fd"; print_newline ();
                raise e
          in
          let buffer_size = 128 * 1024 in
          let buffer = String.make buffer_size '\000' in
          let remaining = ref (Int64.to_int (Int64.sub (chunk_end file i) chunk_begin)) in
          while !remaining >= buffer_size do
            Unix2.really_write fd buffer 0 buffer_size;
            remaining := !remaining - buffer_size;
          done;
          if !remaining > 0 then
            Unix2.really_write fd buffer 0 !remaining;
      
      | _ -> Printf.printf "Trying to zero some existing chunk!!";
          print_newline ())
  with _ -> ()

and check_file_block c file i max_clients =
  if c.client_chunks.(i) then begin
      begin
        match file.file_chunks.(i) with
          AbsentTemp | PartialTemp _ ->
            Printf.printf "check_file_block: verify md4"; print_newline ();
            verify_file_md4 file i file.file_chunks.(i)
        | _ -> ()
      end;
      
      match file.file_chunks.(i) with
      
      | AbsentVerified ->
          let b = new_block file i in
          b.block_zones <- create_zones file b.block_begin b.block_end [];
          
          b.block_nclients <- 1;            
          if !verbose then begin
              Printf.printf "\n%d: NEW BLOCK [%Ld - %Ld]" (client_num c)
              (b.block_begin) (b.block_end);
              print_newline ();
            end;
          zero_block file i;
          c.client_block <- Some b;
          file.file_chunks.(i) <- PartialVerified b;
          find_client_zone c;
          raise Not_found
      
      | PartialVerified b ->
          if b.block_nclients < max_clients then begin
              b.block_nclients <- b.block_nclients + 1;            
              c.client_block <- Some b;
              if !verbose then begin
                  Printf.printf "\n%d: NEW CLIENT FOR BLOCK [%Ld - %Ld]"
                    (client_num c)
                  (b.block_begin) (b.block_end);
                  print_newline ();
                end;
              
              file.file_chunks.(i) <- PartialVerified b;
              find_client_zone c;
              raise Not_found
            end      
      | _ -> ()
    end

and start_download c =
  if not c.client_asked_for_slot then begin
      if !verbose_download then begin
          Printf.printf "start_download..."; print_newline ();
        end;
      match c.client_sock with
        None -> ()
      | Some sock ->
          
          match c.client_file_queue with
            [] -> ()
          | (file, (_, chunks)) :: _ ->
              
              direct_client_send c (
                let module M = DonkeyProtoClient in
                let module Q = M.JoinQueue in
                M.JoinQueueReq Q.t);                        
              c.client_asked_for_slot <- true;
              
              restart_download c
    end

and restart_download c = 
  if !verbose_download then begin
      Printf.printf "restart_download..."; print_newline ();
    end;
  match c.client_sock with
    None -> ()
  | Some sock ->
      
      match c.client_file_queue with
        [] -> ()
      | (file, (_, chunks)) :: _ ->
          
          c.client_block <- None;
          c.client_chunks <- chunks;
          c.client_all_chunks <- String.make file.file_nchunks '0';
          c.client_zones <- [];
          
          for i = 0 to file.file_nchunks - 1 do
            if c.client_chunks.(i)  then 
              c.client_all_chunks.[i] <- '1';
          done;          
          if file.file_md4s = [] && file_size file > block_size then begin
              direct_client_send c (
                let module M = DonkeyProtoClient in
                let module C = M.QueryChunkMd4 in
                M.QueryChunkMd4Req file.file_md4);
            end;                   
          set_rtimeout sock !!queued_timeout;
          set_client_state c (Connected 0)

and find_client_block c =
(* find an available block *)
  
  if !verbose_download then begin
      Printf.printf "find_client_block: started"; print_newline ();
    end;
  match c.client_file_queue with
    [] ->
    (* Emule may reconnect and give the slot without us asking for it.
    We have to fix this behavior in the future. *)
      if !verbose_download then begin    
          Printf.printf "Client %d: NO FILE IN QUEUE" (client_num c); 
          print_newline ();
        end
        
  | (file, (_, chunks)) :: files -> 
      
      if !verbose_download then begin
          Printf.printf "File %s state %s"
            (file_best_name file)
          (string_of_file_state 
              (file_state file)); print_newline ();
        end;
      if file_state file <> FileDownloading then next_file c else 
      
      if !verbose_download then begin
          Printf.printf "find_client_block: continuing"; print_newline ();
        end;
      
      if !verbose then begin
          for i = 0 to file.file_nchunks - 1 do
            print_char (match file.file_chunks.(i) with
              | PartialVerified _ -> 'P'
              | PartialTemp _ -> 'p'
              | AbsentVerified -> 'A'
              | AbsentTemp -> 'a'
              | PresentVerified -> 'D'
              | PresentTemp -> 'd')
          done;
          print_newline ();
        end;
      
      
      begin
        match c.client_block with 
          None ->
            printf_string "[FREE]";
        | Some _ ->
            printf_string "[USED]";
      end;
      try  
        let last = file.file_nchunks - 1 in
        
        if !!random_order_download then begin
            
            if !verbose_download then begin
                Printf.printf "find_client_block: random_order_download"; print_newline ();
              end;

(* chunks with MD4 already computed *)
            for i = 0 to last do
              let j = file.file_chunks_order.(i) in
              if c.client_chunks.(j) && 
                (match file.file_chunks.(j) with
                    AbsentVerified -> true
                  | PartialVerified b when b.block_nclients = 0 -> true
                  | _ -> false
                ) then
                check_file_block c file j !!sources_per_chunk
            done;        

(* chunks whose computation will probably lead to only one MD4 *)
            for i = 0 to last do
              let j = file.file_chunks_order.(i) in
              if c.client_chunks.(j) && 
                (match file.file_chunks.(j) with
                    AbsentTemp -> true
                  | PartialTemp b when b.block_nclients = 0 -> true
                  | _ -> false
                ) then
                check_file_block c file j  !!sources_per_chunk
            done;        

(* rare chunks *)
(* while different clients should try to get different chunks, each client
   should try to complete the chunks it started: if the rare sources
   disappear, all partial chunks will become useless *)
            for i = 0 to last do
              let j = file.file_chunks_order.(i) in
              if c.client_chunks.(j) && file.file_available_chunks.(j) = 1 then
                check_file_block c file j max_int;
            done;

(* chunks with no client *)
            for i = 0 to last do
              let j = file.file_chunks_order.(i) in
              check_file_block c file j  !!sources_per_chunk
            done;

(* chunks with several clients *)
            for i = 0 to last do
              let j = file.file_chunks_order.(i) in
              check_file_block c file j max_int
            done;
          
          end else begin
            
            if !verbose_download then begin
                Printf.printf "find_client_block: NOT RANDOM ORDER (last = %d)" last;
                print_newline ();
              end;
            
            if c.client_chunks.(last) then
              check_file_block c file last max_int;
            if last > 0 && c.client_chunks.(last-1) then
              check_file_block c file (last-1) max_int;

(* chunks with MD4 already computed *)
            for i = 0 to file.file_nchunks - 1 do
              if c.client_chunks.(i) && (match file.file_chunks.(i) with
                    AbsentVerified -> true
                  | PartialVerified b when b.block_nclients = 0 -> true
                  | _ -> false
                ) then
                check_file_block c file i  !!sources_per_chunk
            done;        

(* chunks whose computation will probably lead to only one MD4 *)
            for i = 0 to file.file_nchunks - 1 do
              if c.client_chunks.(i) && (match file.file_chunks.(i) with
                    AbsentTemp -> true
                  | PartialTemp b when b.block_nclients = 0 -> true
                  | _ -> false
                ) then
                check_file_block c file i  !!sources_per_chunk
            done;        

(* rare chunks *)
            let rare_blocks = ref [] in
            for i = 0 to file.file_nchunks - 1 do
              if c.client_chunks.(i) && file.file_available_chunks.(i) = 1 then
                rare_blocks := (Random.int 1000, i) :: !rare_blocks
            done;        
            
            let rare_blocks = Sort.list (fun (c1,_) (c2,_) -> c1 <= c2)
              !rare_blocks in
            
            List.iter (fun (_,i) ->
                check_file_block c file i max_int) rare_blocks;

(* chunks with no client *)
            check_file_block c file last max_int;
            if last > 0 then  check_file_block c file (last-1) max_int;
            for i = 0 to file.file_nchunks - 1 do
              check_file_block c file i  !!sources_per_chunk
            done;

(* chunks with several clients *)
            for i = 0 to file.file_nchunks - 1 do
              check_file_block c file i max_int
            done;
            
            if !verbose_download then begin
                Printf.printf "No block found ???"; print_newline ();
                for i = 0 to file.file_nchunks - 1 do
                  
                  Printf.printf "%d: client %c source %s"
                    i
                    (if chunks.(i) then '1' else '0')
                  (match file.file_chunks.(i) with
                      PresentTemp -> "p"
                    | PresentVerified -> "P"
                    | AbsentTemp -> "a"
                    | AbsentVerified -> "A"
                    | PartialTemp _ -> "d"
                    | PartialVerified _ -> "D");
                  print_newline ();
                done;
              end
              
            
          end;
(* THIS CLIENT CANNOT HELP ANYMORE: USELESS FOR THIS FILE *)
        printf_string "[NEXT]";
        next_file c
with e -> 
    if !verbose_download then begin
        Printf.printf "find_client_block: exception %s"
          (Printexc2.to_string e); print_newline ();
        ()
      end
          
and next_file c =
  
  Printf.printf "next_file..."; print_newline ();
  match c.client_file_queue with
    [] -> assert false
  | (file, (chunks, _) ) :: files -> 
      DonkeyGlobals.remove_client_chunks file chunks;
      match c.client_sock with
        None -> 
          Printf.printf "next_file: client not connected"; print_newline ();
          ()
      | Some sock ->
          match files with
            [] ->
              Printf.printf "next_file: no more file"; print_newline ();
              if not c.client_has_a_slot then begin
                  connection_delay c.client_connection_control;
(* This guy could still want to upload from us !!! *)
                  TcpBufferedSocket.close sock "useless client";            
                  raise Not_found
                end
          | _ ->
              c.client_file_queue <- files;
              restart_download c
              
      
let disconnect_chunk ch =
  match ch with
  | PartialTemp b | PartialVerified b ->
      let file = b.block_file in
      b.block_present <- true;
      List.iter (fun z ->
          z.zone_begin <- file_size file;
      ) b.block_zones;
      b.block_zones <- []            
  | AbsentTemp | AbsentVerified | PresentTemp  | PresentVerified -> ()

let verify_chunks file =
  Printf.printf "Start verifying\n";
  if file.file_md4s <> [] then
    for i = 0 to file.file_nchunks - 1 do
      let b = file.file_chunks.(i)  in
      match b with
        PresentVerified | AbsentVerified | PartialVerified _ ->
          ()
      | _ ->
          let state = verify_chunk file i in
          file.file_chunks.(i) <- (
            if state = PresentVerified then begin
                Printf.printf "(PRESENT VERIFIED)";
                print_newline ();
                PresentVerified
              end
            else
            match b with
              PartialTemp bloc -> PartialVerified bloc
            | PresentTemp ->
                file.file_file.impl_file_downloaded <- Int64.sub (file_downloaded file) block_size;
                
                if file_downloaded file > file_size file then begin
                    Printf.printf "******* downloaded %Ld > %Ld size after verify_chunks ***** for %s"
                      (file_downloaded file)
                    (file_size file)
                    (file_best_name file);
                    print_newline () 
                  end;
                
                Printf.printf "(CORRUPTION FOUND)"; print_newline ();
                file_must_update file;
                AbsentVerified
              | _ -> AbsentVerified);
(*            file.file_all_chunks.[i] <- 
              (if state = PresentVerified then '1' else '0'); *)
    
    done;
  Printf.printf "Done verifying\n";
  file.file_absent_chunks <- List.rev (find_absents file);
  compute_size file
 
let random_chunks_order nchunks =

  let n = ref 0 in
  let nbegin = ref 0 in
  let nend = ref (nchunks-1) in
  let order = Array.create nchunks 0 in

  let from_the_beginning =
    if !n < nchunks then begin
      order.(!n) <- !nbegin;
      incr n;
      incr nbegin 
    end in

  let from_the_end =
    if !n < nchunks then begin
      order.(!n) <- !nend;
      incr n;
      decr nend
    end in

(* Fisher-Yates shuffle *)
  let shuffle a start len =
    for i = len-1 downto 1 do
      let j = Random.int (i+1) in
      if i <> j then
        let temp = a.(i+start) in
        a.(i+start) <- a.(j+start);
        a.(j+start) <- temp
    done in
(* download the last chunk *)
    from_the_end;
(* download the first chunk *)
    from_the_beginning;
(* download the second last chunk too *)
    from_the_end;
(* download the remaining chunks at random *)
    if !n < nchunks then begin
      for i = !n to nchunks-1 do
	order.(i) <- !nbegin;
	incr nbegin
      done;
      shuffle order !n (nchunks - !n)
    end;
    order
  
let set_file_size file sz =
  
  if sz <> Int64.zero then begin
      
      if file_size file = Int64.zero then 
        file.file_absent_chunks <- [Int64.zero, sz];
      file.file_file.impl_file_size <- sz;
      file.file_nchunks <- Int64.to_int (Int64.div  
          (Int64.sub sz Int64.one) block_size)+1;
      
      if file.file_chunks = [||] then 
        file.file_chunks <- Array.create file.file_nchunks (
          if not (Sys.file_exists (file_disk_name file)) then begin
(*            Printf.printf "Setting Absent Verified chunks"; print_newline ();*)
              
              AbsentVerified
            end else begin
(*            Printf.printf "Setting Absent Verified chunks"; print_newline (); *)
              AbsentTemp
            
            end);
      
      file.file_chunks_order <- random_chunks_order file.file_nchunks;
      
      Unix32.ftruncate64 (file_fd file) sz; (* at this point, file exists *)
      
      put_absents file;

      file.file_initialized <- true;
      
      (*
      Printf.printf "AFTER put_absents:"; print_newline ();
      for i = 0 to file.file_nchunks - 1 do
        Printf.printf "  chunk[%d]: %s" i 
          (match file.file_chunks.(i) with
            PresentVerified ->               
              "shared"
          | AbsentVerified -> "absent"
          | PartialVerified _ -> "partial"
          | PartialTemp _ -> "partial compute"
          | AbsentTemp -> "absent compute"
          | PresentTemp -> "present compute");
        print_newline ();
      done;
*)

      for i = 0 to file.file_nchunks - 1 do
        match file.file_chunks.(i) with
          PresentVerified ->               
            DonkeyShare.must_share_file file;
        | _ -> ()
      done;

      
      compute_size file;
      (* verify_chunks file;  *)
      
(*
      List.iter (fun (p0,p1) ->
Printf.printf "%ld <---> %ld" (p0) (p1);
  print_newline ();
) file.file_absent_chunks;
  *)
    end

    
(*              
  
   B--------------------------------------------------E       OK
   B--------------------E                                     OK
                      B-------------------------------E       OK
   B-----E                                                    OUT
                      B-----------E                           OUT
                                                 B----E       OUT
----------------|----------------------------|---------------------
         z.zone_begin                   z.zone_end


*)
    
    
let update_zone file begin_pos end_pos z =
  if z.zone_begin >= begin_pos && z.zone_end <= end_pos then begin
(* the zone has completely been downloaded *)
      
      add_file_downloaded file.file_file (Int64.sub z.zone_end z.zone_begin);
      
      if file_downloaded file > file_size file then begin
          Printf.printf "******* downloaded %Ld > %Ld size after update_zone ***** for %s"
            (file_downloaded file)
          (file_size file)
          (file_best_name file);
          print_newline () 
        end;
      
      file_must_update file;
      z.zone_begin <- z.zone_end;
      if !verbose && end_pos > z.zone_end then begin
          Printf.printf "EXCEEDING: %Ld>%Ld" (end_pos)
          (z.zone_end);
          print_newline ();
        end
    
    end else
  if z.zone_begin >= begin_pos && z.zone_begin < end_pos then begin
(* the block is at the beginning of the zone *)
      
      add_file_downloaded file.file_file (Int64.sub end_pos z.zone_begin);
      if file_downloaded file > file_size file then begin
          Printf.printf "******* downloaded %Ld > %Ld size after update_zone (2) ***** for %s"
            (file_downloaded file)
          (file_size file)
          (file_best_name file);
          print_newline () 
        end;
      
      z.zone_begin <- end_pos;
      file_must_update file;
      
      
    end else
  if z.zone_end > begin_pos && z.zone_end <= end_pos then begin
(* the block is at the end of the zone *)
      
      add_file_downloaded file.file_file (Int64.sub z.zone_end begin_pos);
      if file_downloaded file > file_size file then begin
          Printf.printf "******* downloaded %Ld > %Ld size after update_zone (3) ***** for %s"
            (file_downloaded file)
          (file_size file)
          (file_best_name file);
          print_newline () 
        end;
      
      z.zone_end <- begin_pos;
      file_must_update file;
      
      
      
    end

(*  else begin 
      if !verbose then begin
          Printf.printf "CAN'T UPDATE ZONE %ld-%ld WITH %ld-%ld"
            (z.zone_begin)
          (z.zone_end)
          (begin_pos)
          (end_pos)
          ; print_newline ();
        end
    end        
*)
      
let download_engine () =
  if not (Fifo.empty download_fifo) then begin
      let credit = !!max_hard_download_rate in
      let credit = if credit = 0 then 10000 else credit in
      download_credit := !download_credit + credit;
      let rec iter () =
        if !download_credit > 0 && not (Fifo.empty download_fifo) then  
          begin
            (try
                let (sock, msg, len) = Fifo.take download_fifo in
                download_credit := !download_credit - (len / 1000 + 1);
                direct_client_sock_send sock msg
              with _ -> ());
            iter ()
          end
      in
      iter ()
    end

(*  
let best_name file =
  match file.file_filenames with
    [] -> Md4.to_string file.file_md4
  | name :: _ -> name
        *)

  (*
let move_file_to_done_files md4 =
  try
    let file = Hashtbl.find files_by_md4 md4 in
    file_completed (as_file file.file_file);
    
  with e -> 
      Printf.printf "move_file_to_done_files NOT FOUND";
      print_newline ();
      raise e 
        *)

let remove_file md4 =
  try
    let file = Hashtbl.find files_by_md4 md4 in
    file_cancel (as_file file.file_file);
    Unix32.close (file_fd file);
    (try Sys.remove (file_disk_name file) with e -> 
          Printf.printf "Exception %s in remove %s"
            (Printexc2.to_string e) (file_disk_name file);
          print_newline ());
    (try Hashtbl.remove files_by_md4 file.file_md4 with _ -> ());
    (match file.file_shared with
        None -> ()
      | Some s -> CommonShared.shared_unshare (CommonShared.as_shared s));
    file.file_shared <- None;
(*    !file_change_hook file; *)
    current_files := List2.removeq file !current_files;
  with e -> 
      Printf.printf "remove_file NOT FOUND";
      print_newline ();
      raise e 
    
let check_file_downloaded file =
  if file.file_absent_chunks = [] then
    try
      Array.iteri (fun i b ->
          match b with
            PresentVerified -> ()
          | PresentTemp -> 
              let b = verify_chunk file i in
              file.file_chunks.(i) <- b;
(*
              file.file_all_chunks.[i] <- (
                match b with
                  PresentVerified -> '1'
                | PresentTemp ->  '0'
                | AbsentVerified ->
                    Printf.printf "(CORRUPTION FOUND)";
                    print_newline ();
                    '0'
                | _ ->
                    Printf.printf "OTHER"; print_newline ();
                    '0'
);
  *)
              raise Not_found
          | _ -> raise Not_found
      ) file.file_chunks;        
      current_files := List2.removeq file !current_files;
      DonkeyShare.remember_shared_info file (file_disk_name file);
      file_completed (as_file file.file_file);
      (try
          let format = CommonMultimedia.get_info 
              (file_disk_name file) in
          file.file_format <- format
        with _ -> ());

    with _ -> ()

(* 
This function is called periodically, to compute md4s of files being 
downloaded. If a file is completely present, it is only added to the
downloaded list when all chunks have been verified.
*)
        
let check_downloaded_files () =
  List.iter check_file_downloaded !current_files;  
  (try
      List.iter (fun file ->
          if file.file_md4s <> [] then 
(* First check only md4s of potentially present chunks. This
will allow to fasten the sharing of these chunks. *)
            Array.iteri (fun i b ->
                match b with
                | PresentTemp ->
(*                      Printf.printf "verify file md4 %d %d"
                        file.file_num i; print_newline (); *)
                    verify_file_md4 file i b;
                    compute_size file;
                    raise Not_found
                | _ -> ()
            ) file.file_chunks) !current_files;
      List.iter (fun file ->
          if file.file_md4s <> [] then 
(* First check only md4s of potentially present chunks *)
            Array.iteri (fun i b ->
                match b with
                  PartialVerified _ | AbsentVerified
                | PresentVerified -> ()
                | _ ->
(*                      Printf.printf "verify file md4 %d %d"
                        file.file_num i; print_newline (); *)
                    verify_file_md4 file i b;
                    compute_size file;
                    raise Not_found
            ) file.file_chunks
            ) !current_files;
    with _ -> ())

let _ = 
  file_ops.op_file_to_option <- (fun file ->
      if file.file_chunks <> [||] && file.file_initialized then begin
          file.file_absent_chunks <- List.rev (find_absents file);
          check_file_downloaded file;
        end;
      file_to_value file)
                    
let check_files_md4s () =
  try
    check_downloaded_files ();
    DonkeyShare.check_shared_files ();
    
  with _ -> ()

      
let search_found search md4 tags = 
  let file_name = ref "" in
  let file_size = ref Int64.zero in
  let availability = ref 0 in
  let new_tags = ref [] in
  List.iter (fun tag ->
      match tag with
        { tag_name = "filename"; tag_value = String s } -> file_name := s
      | { tag_name = "size"; tag_value = Uint64 v } -> file_size := v
      | { tag_name = "availability"; tag_value = (Uint64 v| Fint64 v) } ->
          availability := Int64.to_int v;  new_tags := tag :: !new_tags
      | _ -> new_tags := tag :: !new_tags
  ) tags;
  try
    let rs = DonkeyIndexer.find_result md4 in
(*    Printf.printf "search_add_result"; print_newline (); *)
    CommonInteractive.search_add_result search rs.result_result; (* ADD AVAILABILITY *)
(*    Printf.printf "search_add_result DONE"; print_newline (); *)
    let doc = rs.result_index in
    let result = Store.get store doc in
(*    old_avail := !old_avail + !availability; *)
    if not (List.mem !file_name result.result_names) then begin
        DonkeyIndexer.add_name result !file_name;
        result.result_names <- !file_name :: result.result_names
      end
  with _ ->
      match result_of_file md4 tags with
        None -> ()
      | Some new_result ->
      try
        let rs = DonkeyIndexer.index_result new_result in      
        let doc = rs.result_index in
(*        Printf.printf "search_add_result"; print_newline (); *)
        CommonInteractive.search_add_result search rs.result_result;
(*        Printf.printf "search_add_result DONE"; print_newline (); *)
        let result = Store.get store doc in
        ()
      with _ ->  (* the file was probably filtered *)
          ()
