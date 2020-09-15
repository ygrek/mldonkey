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
open Queues
open Printf2
open Md4
open Options

open BasicSocket
open TcpBufferedSocket

open CommonSwarming
open CommonOptions
open CommonInteractive
open CommonTypes
open CommonFile
open CommonGlobals

open FileTPTypes
open FileTPOptions
open FileTPGlobals
open FileTPComplexOptions

      (*
let max_range_size = Int64.of_int (256 * 1024)
  *)
(*
let range_size file =  min_range_size
*)
  (*
  let range =  file_size file // 10L in
  max (min range max_range_size) min_range_size
*)

let max_queued_ranges = 1

let nranges file =
  let filesize = file_size file in
  if (filesize = 0L || !!chunk_size = 0) then 1
  else Int64.to_int ((filesize) // (min_range_size file)) + 5

let pause_for_cause f r user =
  lprintf_nl "Pausing file %s (%s)" (file_best_name f) r;
  file_pause (as_file f) user

let disconnect_client c r =
  match c.client_sock with
  | Connection sock ->
      (try
          if !verbose_msg_clients then
              lprintf_nl "Disconnected from source";
          c.client_requests <- [];
          c.client_session_downloaded <- 0L;
          connection_failed c.client_connection_control;
          set_client_disconnected c r;
          close sock r;
          c.client_sock <- NoConnection;
          List.iter (fun d ->
              match d.download_uploader with
                None -> ()
              | Some up ->
                  CommonSwarming.unregister_uploader up;
                  d.download_ranges <- [];
                  d.download_uploader <- None;
            ) c.client_downloads;
          if c.client_reconnect then
            List.iter (fun d ->
                let file = d.download_file in
                if not (List.memq file c.client_in_queues) then begin
                    Queue.put file.file_clients_queue (0,c);
                    c.client_in_queues <- file :: c.client_in_queues
                  end;
            ) c.client_downloads;
          match c.client_connected_for with
            None -> ()
          | Some file ->
              file.file_nconnected_clients <- file.file_nconnected_clients - 1;
(*
              lprintf "For file %s, %d/%d clients connected (disconnected from %d)\n"
                (file.file_name) file.file_nconnected_clients (nranges file)
(client_num (as_client c.client_client));
  *)
              c.client_connected_for <- None
      with e ->
          lprintf "Exception %s in disconnect_client\n"
            (Printexc2.to_string e))
  | ConnectionWaiting token  ->
      cancel_token token;
      c.client_sock <- NoConnection;
      (match c.client_connected_for with
            None -> ()
          | Some file ->
            file.file_nconnected_clients <- file.file_nconnected_clients - 1
      );
      c.client_connected_for <- None;
  | _ -> ()

let download_finished file =
  if List.memq file !current_files then begin
      file_completed (as_file file);
      FileTPGlobals.remove_file file;
      List.iter (fun c ->
          List.iter (fun d ->
              if d.download_file == file then
                old_files =:= d.download_url :: !!old_files
          ) c.client_downloads;
          c.client_downloads <- remove_download file c.client_downloads;
      ) file.file_clients
    end

let check_finished swarmer file =
  if file_state file <> FileDownloaded &&
    (file_size file = CommonSwarming.downloaded swarmer) then begin
      download_finished file
    end

let get_from_client sock (c: client) =

  let rec iter downloads =
    match downloads with
      [] ->
        if !verbose_msg_clients then
          lprintf "No other download to start\n";
        raise Not_found
    | d :: tail ->
        let file = d.download_file in
        if file_size file = 0L || file_state file  <> FileDownloading then
          iter tail
        else begin
            if !verbose_msg_clients then begin
              lprintf "Finding on client %s %s\n" 
               (Md4.to_string file.file_id) (file_best_name file);
              end;

            if !verbose_swarming then begin
                lprintf "Current download:\n  Current chunks: ";
                List.iter (fun (x,y) -> lprintf "%Ld-%Ld " x y) d.download_chunks;
                lprintf "\n  Current ranges: ";
                List.iter (fun (x,y,r) ->
(*              let (x,y) = CommonSwarming.range_range r in *)
                    lprintf "%Ld-%Ld " x y) d.download_ranges;
                lprintf "\n  Current blocks: ";
(*                List.iter (fun b -> CommonSwarming.print_block b) d.download_blocks; *)
                lprintf "\n\nFinding Range: \n";
              end;
            let range =
              match d.download_uploader with
                None -> assert false
              | Some up ->
                  let swarmer = match file.file_swarmer with
                      None -> assert false | Some sw -> sw
                  in
                  try
                    let rec iter () =
                      match d.download_blocks with
                      | [] ->
                          if !verbose_swarming then lprintf "No block\n";
                          let _chunk, blocks = CommonSwarming.find_blocks up in

(*                          lprintf "GOT BLOCKS:\n"; *)
                          if !verbose_swarming then CommonSwarming.print_uploaders swarmer;

                          if !verbose_swarming then begin
                              lprintf "Blocks Found: "; 
                            List.iter (fun b ->
                              CommonSwarming.print_block b.up_block) blocks;
                            end;
                          d.download_blocks <- blocks;
                          iter ()
                      | blocks ->

                          if !verbose_swarming then  begin
                            lprintf "Current Blocks: "; List.iter (fun b -> 
                              CommonSwarming.print_block b.up_block) blocks;
                            end;
                          try
                            let range_size = (min_range_size file) in
                            let (x,y,r) = CommonSwarming.find_range up range_size in
                            if !verbose_swarming then begin
                              lprintf_nl "find_range: %Ld =  x: %Ld y: %Ld" range_size x y;
                              CommonSwarming.print_uploaders swarmer;
                            end;

                            d.download_ranges <- d.download_ranges @ [x,y,r];
(*                        CommonSwarming.alloc_range r; *)
                            (x,y)
                          with Not_found ->
                              if !verbose_swarming then
                                lprintf "Could not find range in current blocks\n";
(*                          d.download_blocks <- List2.removeq b d.download_blocks; *)
                              d.download_blocks <- [];
                              iter ()
                    in
                    iter ()
                  with Not_found ->
                      check_finished swarmer file;
                      raise Not_found
            in
            c.client_proto.proto_send_range_request c range sock d;
          end
  in
  iter c.client_downloads

let init_client c sock =
  TcpBufferedSocket.set_read_controler sock download_control;
  TcpBufferedSocket.set_write_controler sock upload_control

let connect_client c =
(*  lprintf "connect_client...\n"; *)
  match c.client_sock with
    NoConnection ->
(*      lprintf "NoConnection\n"; *)
(* Count this connection in the first file counter. Here, we assume
that the connection will not be aborted (otherwise, disconnect_client
  should clearly be called). *)
      (try List.iter (fun d ->
              let file = d.download_file in
              if file_state file = FileDownloading then
                begin
                  c.client_connected_for <- Some file;
                  file.file_nconnected_clients <-
                    file.file_nconnected_clients + 1;
(*
                  lprintf "For file %s, %d/%d clients connected (connecting %d)\n"
                    (file.file_name)
                  file.file_nconnected_clients (nranges file)
                    (client_num (as_client c.client_client)); *)
                  raise Exit;
                end
          ) c.client_downloads with _ -> ());
(*      lprintf "...\n"; *)
(*      lprintf "connect_client... pending\n"; *)
      let token =
        add_pending_connection connection_manager (fun token ->
            let exists = List.exists (fun d ->
                  let file = d.download_file in
                  file_state file = FileDownloading
              ) c.client_downloads
            in
            if exists then
              try
                if !verbose_msg_clients then 
                  lprintf_nl "connecting %s:%d" c.client_hostname c.client_port;
                c.client_reconnect <- false;
                let sock = c.client_proto.proto_connect token c (fun sock ->
                    try 
                      List.iter (fun d ->
                          let file = d.download_file in
                          if file_size file <> 0L then begin
                            let swarmer = match file.file_swarmer with
                                None -> assert false | Some sw -> sw
                            in
                            let chunks = [ 0L, file_size file ] in
                            let up = CommonSwarming.register_uploader swarmer
                              (as_client c) (AvailableIntervals chunks) 
                            in
                            d.download_uploader <- Some up
                         end
                      ) c.client_downloads;

                      init_client c sock;
                      get_from_client sock c;

                    with e ->
                        lprintf_nl "Exception %s" (Printexc2.to_string e);
                  )
                in
                set_client_state c Connecting;
                c.client_sock <- Connection sock;
                TcpBufferedSocket.set_closer sock (fun _ s ->
                    disconnect_client c s
                );
                set_rtimeout sock 30.;
                if !verbose_msg_clients then begin
                    lprintf_nl "SET_SOCK_HANDLER" ;
                  end;

                c.client_proto.proto_set_sock_handler c sock

              with e ->
                  lprintf "Exception %s while connecting to client\n"
                    (Printexc2.to_string e);
                  disconnect_client c (Closed_for_exception e)
            else
              disconnect_client c Closed_by_user
        ) in
      c.client_sock <- ConnectionWaiting token
  | ConnectionWaiting _ ->
(*      lprintf "ConnectionWaiting...\n" *) ()
  | Connection _ ->
(*      lprintf "Already connected\n" *) ()

let get_file_from_source c file =
(*  lprintf "      get_file_from_source\n"; *)
  if connection_can_try c.client_connection_control then begin
(*      lprintf "       Queuing connection...\n"; *)
      connection_try c.client_connection_control;
      if not (List.memq file c.client_in_queues) then begin
          Queue.put file.file_clients_queue (1,c);
          c.client_in_queues <- file :: c.client_in_queues
        end
    end

let ask_for_files () = (* called every minute *)
(*  lprintf "ask_for_files\n"; *)
  List.iter (fun file ->
(*      lprintf "  for file\n"; *)
      List.iter (fun c ->
(*          lprintf "   for client\n"; *)
          get_file_from_source c file
      ) file.file_clients
  ) !current_files;
(*  lprintf "done\n"; *)
  ()

let manage_hosts () =
  List.iter (fun file ->
      if file_state file = FileDownloading then
        try
(* For each file, we allow only (nranges+5) simultaneous communications,
  to prevent too many clients from saturing the line for only one file. *)
          let max_nconnected_clients = nranges file in
          (* lprintf_nl "%s %d | %d < %d" (Md4.to_string file.file_id) (Queue.length file.file_clients_queue) file.file_nconnected_clients max_nconnected_clients; *)
          while file.file_nconnected_clients < max_nconnected_clients do
            let (_,c) = Queue.take file.file_clients_queue in
            c.client_in_queues <- List2.removeq file c.client_in_queues;
            if file_size file = 0L then
              let proto = c.client_proto in
              List.iter (fun d ->
                  if d.download_file == file then
                    let url = d.download_url in
                    proto.proto_check_size file url (fun size ->
                        set_file_size file size;
                      connect_client c;
                    );
              ) c.client_downloads
            else begin
              connect_client c
            end
          done
        with _ -> ()
  ) !current_files
