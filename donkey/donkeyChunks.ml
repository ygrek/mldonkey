(* Copyright 2001, 2002 b8_bavard, b8_fee_carabine, b52_simon INRIA *)
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
open DonkeyGlobals          
  
let chunk_pos i =
  Int64.mul (Int64.of_int i)  block_size

let chunk_end file i =
  let pos = Int64.mul (Int64.of_int (i+1))  block_size in
  if pos > file_size file then 
    file_size file else pos

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



let chunk_present file i =
  
  (match file.file_chunks.(i) with
    PartialTemp b | PartialVerified b ->
      b.block_present <- true;
      List.iter (fun z ->
            z.zone_begin <- z.zone_end
      ) b.block_zones;
      b.block_zones <- []
  | _ -> ()
  );
  file.file_chunks.(i) <- PresentVerified
  
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
                file.file_file.impl_file_downloaded <- 
                  Int64.sub (file_downloaded file) block_size;
                
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

(*************************************************************

   Save the files containing chunks identified by
  (md4, chunk_pos, chunk_size) so that, when one of these 
  chunks is received, it can be duplicated in all files.
duplicate_chunks is called with the 'dup' command, and should
   probably be called every 5 minutes
  
**************************************************************)  
  
let md4_table = Hashtbl.create 112
  
let register_md4 i md4 (begin_pos : int64) (len : int64) file = 
  try
    let files = Hashtbl.find md4_table (md4, i, begin_pos, len) in
    if not (List.memq file !files) then begin
        files := file :: !files;
        Printf.printf "Files";
        List.iter (fun file -> Printf.printf " %d" (file_num file)) !files;
        Printf.printf "share block %s" (Md4.to_string md4);
        print_newline ();
      end
  with _ ->
      Hashtbl.add md4_table (md4, i, begin_pos, len) (ref [file])
      
let register_md4s md4s file_num file_size = 
  
  let len = List.length md4s in
  let rec iter md4s i chunk_pos =
    match md4s with
      [] -> ()
    | md4 :: tail ->
        let chunk_end = Int64.add chunk_pos block_size in
        let chunk_size = if chunk_end > file_size then
            Int64.sub file_size chunk_pos else block_size in
        register_md4 i md4 chunk_pos chunk_size file_num;
        iter tail (i+1) chunk_end
  in
  iter md4s 0 Int64.zero

let copy_chunk other_file file chunk_pos chunk_size =
  Printf.printf "Copying chunk"; print_newline ();
  let file_in = 
    Unix.openfile (file_disk_name other_file)
    [Unix.O_RDONLY] 0o666 in
  try
    let file_out = 
      Unix.openfile (file_disk_name file)
      [Unix.O_RDWR; Unix.O_CREAT] 0x666
    in
    try
      ignore (Unix2.c_seek64 file_in chunk_pos Unix.SEEK_SET);
      ignore (Unix2.c_seek64 file_out chunk_pos Unix.SEEK_SET);
      let buffer_len = 32768 in
      let len = Int64.to_int chunk_size in
      let buffer = String.create buffer_len in
      let rec iter file_in file_out len =
        if len > 0 then
          let can_read = mini buffer_len len in
          let nread = Unix.read file_in buffer 0 buffer_len in
          Unix2.really_write file_out buffer 0 nread;
          iter file_in file_out (len-nread)
      in
      iter file_in file_out len;
      Printf.printf "Chunk copied"; print_newline ();
      
    with _ -> Unix.close file_out; raise Exit
  
  with _ -> Unix.close file_in; raise Exit
      
let duplicate_chunks () =
    
  List.iter (fun file ->
      register_md4s file.file_md4s file (file_size file))
  !file_md4s_to_register;
  file_md4s_to_register := [];

  let modified_files = ref [] in
  
  List.iter (fun file ->
      
      let md4s = file.file_md4s in
      let file_size = file_size file in
      let file_num = file_num file in
      let len = List.length md4s in
      let rec iter md4s i chunk_pos =
        match md4s with
          [] -> ()
        | md4 :: tail ->
            let chunk_end = Int64.add chunk_pos block_size in
            let chunk_size = if chunk_end > file_size then
                Int64.sub file_size chunk_pos else block_size in
            
            (try
                match file.file_chunks.(i) with
                  PresentVerified | PresentTemp -> ()
                | _ ->
                    
(* Try to find a file where this chunk is already present *)
                    let files = Hashtbl.find md4_table 
                        (md4, i, chunk_pos, chunk_size)
                    in
                    List.iter (fun other_file ->
                        match other_file.file_chunks.(i) with
                          PresentVerified ->
                            Printf.printf "Should copy chunk %d [%Ld:%Ld] from %s to %s" i chunk_pos chunk_size 
                              (file_best_name other_file) (file_best_name file);
                            print_newline ();

                            if not (List.memq file !modified_files) then
                              modified_files := file :: !modified_files;
                            
                            copy_chunk other_file file chunk_pos chunk_size;

                            chunk_present file i;
                            file_must_update file;
                            raise Exit
                        | _ -> ()
                    ) !files
              with _ -> ()
            );
            
            iter tail (i+1) chunk_end
      in
      iter md4s 0 Int64.zero;
  ) !current_files;

(* Recompute the size of the files where we have duplicated chunks *)
  
  List.iter (fun file ->
      file.file_absent_chunks <- List.rev (find_absents file);
      compute_size file      
  ) !modified_files
  
