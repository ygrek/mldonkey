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
open Options
open Unix
open BasicSocket
open TcpClientSocket
open Mftp
open Files
open Mftp_comm
open DownloadTypes
open DownloadOptions
open DownloadComplexOptions
open DownloadGlobals
open Gui_types
  
let client_state t =
  match t with
    Connected_queued -> "Queued"
  | NotConnected -> "Disconnected"
  | Connected_idle -> "Connected"
  | Connected_busy -> "Downloading"
  | Connecting -> "Connecting"
  | Connected_initiating -> "Initiating"
  | Removed -> "Removed"
      
let set_client_state c s =
  if c.client_state = Connected_busy then decr nclients;
  if s = Connected_busy then incr nclients;  
  c.client_state <- s


let must_share_file file =
  if not file.file_shared then begin
      file.file_shared <- true;
      incr nshared_files;
      new_shared := file :: !new_shared
    end

          
  
let chunk_pos i =
  Int32.mul (Int32.of_int i)  block_size

let chunk_end file i =
  let pos = Int32.mul (Int32.of_int (i+1))  block_size in
  if pos > file.file_size then file.file_size else pos
    
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

let sort_zones b =
  let zones = List.fold_left (fun zones z ->
        if z.zone_begin = z.zone_end then z.zone_present <- true;
        if z.zone_present  then zones else z :: zones
    ) [] b.block_zones 
  in
  b.block_zones <- Sort.list (fun z1 z2 ->
      z1.zone_nclients < z2.zone_nclients ||
      (z1.zone_nclients == z2.zone_nclients &&
        z1.zone_begin < z2.zone_begin)
  ) zones

let disconnected_from_client c msg =
  printf_string "-c"; 
  c.client_chunks <- [||];
  c.client_sock <- None;
  let files = c.client_files in
  List.iter (fun (file, chunks) -> remove_client_chunks file chunks)  files;    
  c.client_files <- [];
  begin
    match c.client_kind with
      Known_location _ -> ()
    | Indirect_location ->
        Hashtbl.remove indirect_clients_by_md4 c.client_md4;
        List.iter (fun (file,_) ->
            file.file_indirect_locations <- 
              List2.removeq c file.file_indirect_locations
        ) files;
        remove_client c
  end;
  set_client_state c NotConnected;
  begin
    match c.client_block with None -> ()
    | Some b ->
        c.client_block <- None;
        b.block_nclients <- b.block_nclients - 1;
        List.iter (fun z ->
            z.zone_nclients <- z.zone_nclients - 1) c.client_zones;
        sort_zones b
  end
  

let rec create_zones file begin_pos end_pos list =
  if begin_pos = end_pos then list
  else
  let zone_end = Int32.add begin_pos zone_size in
  let zone_end = if zone_end > end_pos then end_pos else zone_end in
  create_zones file zone_end end_pos ({
      zone_begin = begin_pos;
      zone_end = zone_end;
      zone_nclients = 0;
      zone_present = false;
      zone_file = file;
    } :: list ) 

let client_file c =
  match c.client_files with
    [] -> failwith "No file for this client"
  | (file, _) :: _ -> file

let download_fifo = Fifo.create ()
  
  
      
let query_zones c b = 
  let file = client_file c in
  sort_zones b;
  match c.client_sock with
    None -> assert false
  | Some sock ->
      
      set_rtimeout (TcpClientSocket.sock sock) !queue_timeout;
        let module M = Mftp_client in
        let module Q = M.QueryBloc in
      let msg, len =           
        match c.client_zones with
            [z] ->
              {
                Q.md4 = file.file_md4;
                Q.start_pos1 = z.zone_begin;
                Q.end_pos1 = z.zone_end;
                Q.start_pos2 = Int32.zero;
                Q.end_pos2 = Int32.zero;
                Q.start_pos3 = Int32.zero;
                Q.end_pos3 = Int32.zero;
              }, Int32.to_int (Int32.sub z.zone_end z.zone_begin)
          
          | [z1;z2] ->
              {
                Q.md4 = file.file_md4;
                Q.start_pos1 = z1.zone_begin;
                Q.end_pos1 = z1.zone_end;
                Q.start_pos2 = z2.zone_begin;
                Q.end_pos2 = z2.zone_end;
                Q.start_pos3 = Int32.zero;
                Q.end_pos3 = Int32.zero;
              }, Int32.to_int (Int32.sub z1.zone_end z1.zone_begin)
          
          | [z1;z2;z3] ->
              {
                Q.md4 = file.file_md4;
                Q.start_pos1 = z1.zone_begin;
                Q.end_pos1 = z1.zone_end;
                Q.start_pos2 = z2.zone_begin;
                Q.end_pos2 = z2.zone_end;
                Q.start_pos3 = z3.zone_begin;
                Q.end_pos3 = z3.zone_end;
              }, Int32.to_int (Int32.sub z1.zone_end z1.zone_begin)
          
          | _ -> assert false
      in
      let msg = M.QueryBlocReq msg in
      if !!max_download_rate <> 0 then
        Fifo.put download_fifo (sock, msg, len)
      else
        client_send sock msg
        

        
(* create a list with all absent intervals *)

let put_absents file =

  for i = 0 to file.file_nchunks - 1 do
    file.file_chunks.(i) <- PresentTemp;
  done;
  
  let rec iter_chunks_in i zs =
    if i < file.file_nchunks then 
    match zs with
      [] -> ()
    | (begin_pos, end_pos) :: tail ->
        if begin_pos >= chunk_end file i then
          iter_chunks_in (i+1) zs
        else
        if end_pos <= chunk_pos i then
          iter_chunks_in i tail
          else
          if begin_pos <= chunk_pos i && end_pos >= chunk_end file i then begin
              file.file_chunks.(i) <- AbsentTemp;
              iter_chunks_in (i+1) ((chunk_end file i, end_pos) :: tail)
            end else
          let b = new_block file i in
          file.file_chunks.(i) <- PartialTemp b;
          iter_blocks_in i b zs
        
  and iter_blocks_in i b zs =
    match zs with
      [] -> 
        sort_zones b
    | (begin_pos, end_pos) :: tail ->
        if begin_pos >= b.block_end then begin
            sort_zones b;
            iter_chunks_in (i+1) zs 
          end
        else
        if end_pos >= b.block_end then begin
            b.block_zones <- create_zones file begin_pos b.block_end
              b.block_zones;
            sort_zones b;
            iter_chunks_in (i+1) ((b.block_end, end_pos) :: tail)
          end else begin
            b.block_zones <- create_zones file begin_pos end_pos b.block_zones;
            iter_blocks_in i b tail
          end
    
    
  in
  
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
    if i = file.file_nchunks then (begin_pos, file.file_size) :: prev else
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
  if file.file_size <> Int32.zero then
    
    let absents = ref Int32.zero in
    for i = 0 to file.file_nchunks - 1 do
      match file.file_chunks.(i) with
        PresentTemp | PresentVerified -> ()
      | AbsentTemp | AbsentVerified -> absents := Int32.add !absents (
            Int32.sub (chunk_end file i) (chunk_pos i))
      | PartialTemp b | PartialVerified b ->
          List.iter (fun z ->
              absents := Int32.add !absents (
                Int32.sub z.zone_end z.zone_begin)
          ) b.block_zones
    done;
    let current = Int32.sub file.file_size !absents in
    if file.file_downloaded <> current then 
      file.file_changed <- SmallChange;
    file.file_downloaded <- current
    
let print_time tm =
  let module U = Unix in
  Printf.printf "TIME %d/%d/%d %2d:%02d:%02d" 
    tm.U.tm_mday tm.U.tm_mon tm.U.tm_year
    tm.U.tm_hour tm.U.tm_min tm.U.tm_sec;
  print_newline ()
  
  
let print_stats file = ()
  (*
  print_newline ();
  print_time (Unix.localtime (last_time ()));
  compute_size file;
  let nc = ref 0 in
  let ncc = ref 0 in
  List.iter (fun  c ->
      incr nc;
      match c.client_sock with
        None -> ()
      | _ -> incr ncc;
  ) file.file_known_locations;
  List.iter (fun c ->
      incr nc;
      match c.client_sock with
        None -> ()
      | _ -> incr ncc;
  ) file.file_indirect_locations;
  Printf.printf "CLIENTS CONNECTED: %d/%d :" !ncc !nc;
  List.iter (fun c ->
      print_char (
        match c.client_state with
        | Connected_initiating
        | Connected_idle -> 'C'
        | Connecting -> 'c'
        | NotConnected -> ' '
        | Connected_queued -> 'Q'
        | Connected_busy -> 'D'
        | Removed -> 'R'
            )
  ) file.file_known_locations;
  List.iter (fun c ->
      print_char (
        match c.client_state with
        | Connected_initiating
        | Connected_idle -> 'C'
        | Connecting -> 'c'
        | NotConnected -> ' '
        | Connected_queued -> 'Q'
        | Connected_busy -> 'D'
        | Removed -> 'R'
            )
  ) file.file_indirect_locations;
  print_newline ()
*)
  
let verify_chunk file i =
  if file.file_md4s = [] then file.file_chunks.(i) else
  let file_md4s = Array.of_list file.file_md4s in
  let begin_pos = chunk_pos i in
  let end_pos = chunk_end file i in
  let len = Int32.sub end_pos begin_pos in
  let md4 = file_md4s.(i) in
  let new_md4 = Md4.digest_subfile file.file_fd begin_pos len in
  (*
  let mmap = Mmap.mmap file.file_name 
    (file_fd file) begin_pos len in
  let mmap_md4 = Mmap.md4_sub mmap Int32.zero len in
  Mmap.munmap mmap;
  if new_md4 <> mmap_md4 then begin
      Printf.printf "BAD md4 computation"; print_newline ();
      exit 1;
    end else begin
      Printf.printf "GOOD md4 computation"; print_newline ();
end;
  *)
  if new_md4 = md4 then begin
      must_share_file file;
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
  file.file_all_chunks.[i] <- 
    (if state = PresentVerified then '1' else '0')
  
let rec find_client_zone c = 
  match c.client_block with
    None -> find_client_block c
  | Some b ->
(* client_zones : les zones en cours de telechargement *)
(* block_zones : les zones disponibles pour telechargement *)
      let z = match c.client_zones with
        | [z1] -> if z1.zone_present then [] else [z1]
        | [z1;z2] ->
            let z = if z2.zone_present then [] else [z2] in
            if z1.zone_present then z else z1 :: z 
        | [z1;z2;z3] ->
            let z = if z3.zone_present then [] else [z3] in
            let z = if z2.zone_present then z else z2 :: z in
            if z1.zone_present then z else z1 :: z 
        | _ -> []
      
      in
      let rem_zones = List.length b.block_zones in
      match z with
        [z1;z2;z3] -> ()
      | [z1;z2] when rem_zones <= 2 -> ()
      | [z1] when rem_zones <= 1 -> ()
      | [z1;z2] -> find_zone3 c b z1 z2 b.block_zones
      | [z1]  -> find_zone2 c b z1 b.block_zones
      | _ -> find_zone1 c b b.block_zones

and print_client_zones n b c =
  (match c.client_block with
      None -> Printf.printf "\n%d: CLIENT ZONES WITH NO BLOCK %d" c.client_num n;
        print_newline ();
    | Some bb ->
        if b != bb then begin
            Printf.printf "\n%d: CLIENT ZONES WITH BAD BLOCK %d" c.client_num n;
            print_newline ();
          end);          
  if !!verbose then begin
      Printf.printf "\n%d: ZONES IN %d" c.client_num n; 
      List.iter (fun z ->
          Printf.printf " [%s - %s] " (Int32.to_string z.zone_begin)
          (Int32.to_string z.zone_end);
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
      if (not z.zone_present) && z != z1 && z != z2 then begin
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
      if (not z.zone_present) && z != z1 then begin
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
      (file.file_all_chunks).[b.block_pos] <- (if state = PresentVerified 
        then '1' else '0');
      file.file_absent_chunks <- List.rev (find_absents file);
      c.client_block <- None;
      find_client_block c
  
  | z :: zones ->
      if (not z.zone_present) then begin
          z.zone_nclients <- z.zone_nclients + 1;
          find_zone2 c b z zones
        end else
        find_zone1 c b zones

and check_file_block c file i max_clients =
  if c.client_chunks.(i) then begin
      begin
        match file.file_chunks.(i) with
          AbsentTemp | PartialTemp _ ->
            verify_file_md4 file i file.file_chunks.(i)
        | _ -> ()
      end;
      
      match file.file_chunks.(i) with
      
      | AbsentVerified ->
          let b = new_block file i in
          b.block_zones <- create_zones file b.block_begin b.block_end [];
          
          b.block_nclients <- 1;            
          if !!verbose then begin
              Printf.printf "\n%d: NEW BLOCK [%s - %s]" c.client_num
                (Int32.to_string b.block_begin) (Int32.to_string b.block_end);
              print_newline ();
            end;
          c.client_block <- Some b;
          file.file_chunks.(i) <- PartialVerified b;
          find_client_zone c;
          raise Not_found
      
      | PartialVerified b when b.block_nclients < max_clients ->
          b.block_nclients <- 1;            
          c.client_block <- Some b;
          if !!verbose then begin
              Printf.printf "\n%d: NEW BLOCK [%s - %s]" c.client_num
                (Int32.to_string b.block_begin) (Int32.to_string b.block_end);
              print_newline ();
            end;
          
          file.file_chunks.(i) <- PartialVerified b;
          find_client_zone c;
          raise Not_found
      
      | _ -> ()
    end

and start_download c =
  match c.client_sock with
    None -> ()
  | Some sock ->
      match c.client_files with
        [] -> ()
      | (file, chunks) :: _ ->
          c.client_block <- None;
          c.client_chunks <- chunks;
          c.client_all_chunks <- String.make file.file_nchunks '0';
          c.client_zones <- [];
          
          for i = 0 to file.file_nchunks - 1 do
            if c.client_chunks.(i)  then 
              c.client_all_chunks.[i] <- '1';
          done;          
          if file.file_md4s = [] && file.file_size > block_size then begin
              client_send sock (
                let module M = Mftp_client in
                let module C = M.QueryChunkMd4 in
                M.QueryChunkMd4Req file.file_md4);
            
            end;
          
          
          client_send sock (
            let module M = Mftp_client in
            let module Q = M.JoinQueue in
            M.JoinQueueReq Q.t);              
          
          set_rtimeout (TcpClientSocket.sock sock) infinite_timeout;
          set_client_state c Connected_queued

and find_client_block c =
(* find an available block *)
  
  match c.client_files with
    [] -> assert false
  | (file, chunks) :: files -> 
      
      begin
        match c.client_block with 
          None ->
            printf_string "[FREE]";
        | Some _ ->
            printf_string "[USED]";
      end;
      try  
        let last = file.file_nchunks - 1 in
        if c.client_chunks.(last) && file.file_available_chunks.(last) = 1 then
          check_file_block c file last max_int;

        let rare_blocks = ref [] in
        for i = 0 to file.file_nchunks - 1 do
          if c.client_chunks.(i) && file.file_available_chunks.(i) = 1 then
            rare_blocks := (Random.int 1000, i) :: !rare_blocks
        done;        
        
        let rare_blocks = Sort.list (fun (c1,_) (c2,_) -> c1 <= c2)
          !rare_blocks in
        
        List.iter (fun (_,i) ->
            check_file_block c file i max_int) rare_blocks;

        check_file_block c file last max_int;
        for i = 0 to file.file_nchunks - 1 do
          check_file_block c file i 1
        done;
        for i = 0 to file.file_nchunks - 1 do
          check_file_block c file i max_int
        done;
(* THIS CLIENT CANNOT HELP ANYMORE: USELESS FOR THIS FILE *)
        printf_string "[NEXT]";
        next_file c
      
      with _ -> ()

and next_file c =
  
  match c.client_files with
    [] -> assert false
  | (file, chunks) :: files -> 
      remove_client_chunks file chunks;
      match c.client_sock with
        None -> ()
      | Some sock ->
          match files with
            [] ->
              connection_delay c.client_connection_control;
              TcpClientSocket.close sock "useless client";            
              raise Not_found
          | _ ->
              c.client_files <- files;
              start_download c
              
      
let disconnect_chunk ch =
  match ch with
  | PartialTemp b | PartialVerified b ->
      let file = b.block_file in
      b.block_present <- true;
      List.iter (fun z ->
          z.zone_begin <- file.file_size;
      ) b.block_zones;
      b.block_zones <- []            
  | AbsentTemp | AbsentVerified | PresentTemp  | PresentVerified -> ()

let verify_chunks file =
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
                  Printf.printf "(CORRUPTION FOUND)"; print_newline ();
                  AbsentVerified
              | _ -> AbsentVerified);
            file.file_all_chunks.[i] <- 
              (if state = PresentVerified then '1' else '0');
    
    done;
  file.file_absent_chunks <- List.rev (find_absents file);
  compute_size file

let set_file_size file sz =
  
  if sz <> Int32.zero then begin
      if file.file_size = Int32.zero then 
          file.file_absent_chunks <- [Int32.zero, sz];
      file.file_size <- sz;
      file.file_nchunks <- Int32.to_int (Int32.div  
          (Int32.sub sz Int32.one) block_size)+1;
      file.file_chunks <- Array.create file.file_nchunks AbsentTemp;
      Unix32.ftruncate32 file.file_fd sz;
      
      file.file_all_chunks <- String.make file.file_nchunks '0';
      
      for i = 0 to file.file_nchunks - 1 do
        if (file.file_all_chunks).[i] = '1' then 
          file.file_chunks.(i) <- PresentTemp;
      done;

      put_absents file;
      
      for i = 0 to file.file_nchunks - 1 do
        if file.file_chunks.(i) = PresentTemp then
          file.file_all_chunks.[i] <- '1'
      done;
      compute_size file;
      (* verify_chunks file;  *)
      
(*
      List.iter (fun (p0,p1) ->
Printf.printf "%s <---> %s" (Int32.to_string p0) (Int32.to_string p1);
  print_newline ();
) file.file_absent_chunks;
  *)
    end
  


      
let update_zone file begin_pos end_pos z =
  if z.zone_begin <= begin_pos &&
    z.zone_end > begin_pos then 
    if z.zone_end <= end_pos then begin
        file.file_downloaded <- 
          Int32.add file.file_downloaded 
          (Int32.sub z.zone_end z.zone_begin);
        file.file_changed <- SmallChange;
        z.zone_present <- true;
        z.zone_begin <- z.zone_end;
      end 
    else begin
        file.file_downloaded <- 
          Int32.add file.file_downloaded 
          (Int32.sub end_pos z.zone_begin);
        file.file_changed <- SmallChange;
        z.zone_begin <- end_pos;
      end


let move_file src dst md4 =
  src =:= List.rev (List.fold_left (fun files file ->
        if file.file_md4 = md4 then begin
            dst =:= file :: !!dst;
            files 
          end
        else file :: files
    ) [] !!src)
  
let remove_file md4 =
  files =:= List.rev (List.fold_left (fun files file ->
        if file.file_md4 = md4 then begin
            file.file_state <- FileCancelled;
            Unix32.close file.file_fd;
            file.file_shared <- false;
            decr nshared_files;
            (try Sys.remove file.file_hardname with _ -> ());
            (try Hashtbl.remove files_by_md4 file.file_md4 with _ -> ());
            file.file_hardname <- "";
            !file_change_hook file;
            files end
        else file :: files
    ) [] !!files);
  ()
  
open Mailer
  
let best_name file =
  match file.file_filenames with
    [] -> Md4.to_string file.file_md4
  | name :: _ -> name
  
let mail_for_completed_file file =
  if !!mail <> "" then
    let line1 = "\r\n mldonkey has completed the download of:\r\n\r\n" in
    let line2 = Printf.sprintf "\r\ned2k://|file|%s|%s|%s|\r\n" 
        (best_name file)
      (Int32.to_string file.file_size)
      (Md4.to_string file.file_md4)
    in
    
    let mail = {
        mail_to = !!mail;
        mail_from = Printf.sprintf "mldonkey <%s>" !!mail;
        mail_subject = Printf.sprintf "mldonkey completed download";
        mail_body = line1 ^ line2;
      } in
    sendmail !!smtp_server !!smtp_port mail
    
let check_file_downloaded file =
  if file.file_absent_chunks = [] then
    try
      Array.iteri (fun i b ->
          match b with
            PresentVerified -> ()
          | PresentTemp -> 
              let b = verify_chunk file i in
              file.file_chunks.(i) <- b;
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
              raise Not_found
          | _ -> raise Not_found
      ) file.file_chunks;        
      file.file_state <- FileDownloaded;
      (try mail_for_completed_file file with e ->
            Printf.printf "Exception %s in sendmail" (Printexc.to_string e);
            print_newline ());
      (try
          let format = DownloadMultimedia.get_info file.file_hardname in
          file.file_format <- format
        with _ -> ());
      small_change_file file;
      !file_change_hook file;
      move_file files done_files file.file_md4
    with _ -> ()
        
let update_options file =    
  file.file_absent_chunks <- List.rev (find_absents file);
  check_file_downloaded file;
  print_stats file;
  file.file_changed <- SmallChange

let new_file_to_share sh =
  try
(* How do we compute the total MD4 of the file ? *)
  
  let md4s = List.rev sh.sh_md4s in
  let md4 = match md4s with
      [md4] -> md4
    | [] -> Printf.printf "No md4 for %s" sh.sh_name;
        print_newline ();
        raise Not_found
    | _ -> 
        let len = List.length md4s in
        let s = String.create (len * 16) in
        let rec iter list i =
          match list with
            [] -> ()
          | md4 :: tail ->
              let md4 = Md4.direct_to_string md4 in
              String.blit md4 0 s i 16;
              iter tail (i+16)
        in
        iter md4s 0;
        Md4.string s
  in
  let file = new_file sh.sh_name md4 sh.sh_size false in
  must_share_file file;
  file.file_md4s <- md4s;
  file.file_filenames <- [Filename.basename sh.sh_name]; 
  file.file_chunks <- Array.make file.file_nchunks PresentVerified;
  file.file_absent_chunks <- [];
  file.file_all_chunks <- String.make file.file_nchunks '1';
  file.file_state <- FileRemoved;
  (try 
      file.file_format <- DownloadMultimedia.get_info file.file_hardname
    with _ -> ());
    Printf.printf "Sharing %s" sh.sh_name;
    print_newline ();
  with e ->
      Printf.printf "Exception %s while sharing %s" (Printexc.to_string e)
      sh.sh_name; print_newline () 
      
let check_files_md4s timer =
  reactivate_timer timer;
  if !new_shared != [] then
    begin
      let msg = Mftp_server.ShareReq (DownloadServers.make_tagged !new_shared) 
      in
      new_shared := [];
      let socks = ref [] in
      List.iter (fun s ->
          match s.server_sock with
            None -> ()
          | Some sock ->
              socks := sock :: !socks) !connected_server_list;
      servers_send !socks msg;
    end;
  
  try
    List.iter  check_file_downloaded !!files;
    
    (try
        List.iter (fun file ->
            if file.file_md4s <> [] then
              Array.iteri (fun i b ->
                  match b with
                    PartialVerified _ | AbsentVerified -> ()
                  | PresentVerified
                  | _ ->
                      verify_file_md4 file i b;
                      compute_size file;
                      raise Not_found
              ) file.file_chunks
        ) !!files;
      with _ -> ());
    
    match !shared_files with
      [] -> ()
    
    | sh :: files ->
        try
          if not (Sys.file_exists sh.shared_name) then begin
              Printf.printf "Shared file doesn't exist"; print_newline ();
              raise Not_found;
            end;
          if Unix32.getsize32 sh.shared_name <> sh.shared_size then begin
              Printf.printf "Bad shared file size" ; print_newline ();
              raise Not_found;
            end;
          let end_pos = Int32.add sh.shared_pos block_size in
          let end_pos = if end_pos > sh.shared_size then sh.shared_size
            else end_pos in
          let len = Int32.sub end_pos sh.shared_pos in
          
          let new_md4 = Md4.digest_subfile (sh.shared_fd) sh.shared_pos len in
          
          sh.shared_list <- new_md4 :: sh.shared_list;
          sh.shared_pos <- end_pos;
          if end_pos = sh.shared_size then begin
              shared_files := files;
              let s = {
                  sh_name = sh.shared_name;
                  sh_size = sh.shared_size;
                  sh_md4s = sh.shared_list;
                  sh_mtime = (let st = Unix.stat sh.shared_name in
                    st.Unix.st_mtime);
                } in
              Printf.printf "NEW SHARED FILE %s" sh.shared_name; 
              print_newline ();
              Hashtbl.add shared_files_info sh.shared_name s;
              known_shared_files =:= s :: !!known_shared_files;
              new_file_to_share s;
              if !shared_files = [] then begin
(*                  Printf.printf "Saving shared files"; print_newline (); *)
                  save shared_files_ini
                end                
            end
        with e ->
            Printf.printf "Exception %s prevents sharing"
              (Printexc.to_string e);
            print_newline ();
            shared_files := files;
            if !shared_files = [] then begin
(*                Printf.printf "Saving shared files"; print_newline (); *)
                save shared_files_ini
              end                
              
    
    
  with _ -> ()

      open Unix
      

let file_size filename = Unix32.getsize32 filename
let local_dirname = Sys.getcwd ()
  
let rec add_shared_files dirname =
  let files = Unix2.list_directory dirname in
  List.iter (fun file ->
      let name =  Filename.concat dirname file in
      try
        if Unix2.is_directory name then
          add_shared_files name
        else
        let size = file_size name in
        if size > Int32.zero then
          let real_name =  
            Filename2.normalize (
              if Filename.is_relative name then
                Filename.concat local_dirname name
              else name) in
          try
            let s = Hashtbl.find shared_files_info real_name in
            let mtime = (Unix.stat real_name).Unix.st_mtime in
            if s.sh_mtime = mtime && s.sh_size = size then begin
(*                Printf.printf "USING OLD MD4s for %s" real_name;
                print_newline (); *)
                new_file_to_share s
              end else begin
                Printf.printf "Shared file %s has been modified" real_name;
                print_newline ();
                Hashtbl.remove shared_files_info real_name;
                known_shared_files =:= List2.removeq s !!known_shared_files
              end
          with Not_found ->
              Printf.printf "No info on %s" real_name; print_newline (); 
              shared_files := {
                shared_name = real_name;              
                shared_size = size;
                shared_list = [];
                shared_pos = Int32.zero;
                shared_fd = Unix32.create real_name [O_RDONLY] 0o444;
              } :: !shared_files
      with _ -> ()
  ) files
  
let download_engine () =
  if not (Fifo.empty download_fifo) then begin
      download_credit := !download_credit + !!max_download_rate;
      let rec iter () =
        if !download_credit > 0 && not (Fifo.empty download_fifo) then  
          begin
            (try
                let (sock, msg, len) = Fifo.take download_fifo in
                download_credit := !download_credit - (len / 1000 + 1);
                client_send sock msg
              with _ -> ());
            iter ()
          end
      in
      iter ()
    end
    
  
        
  