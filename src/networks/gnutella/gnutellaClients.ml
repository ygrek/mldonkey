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
open BasicSocket
open TcpBufferedSocket
open Int64ops
open Printf2
open Md4
open Queues
  
open CommonSwarming
open CommonUploads
open CommonOptions
open CommonInteractive
open CommonTypes
open CommonGlobals

open GnutellaTypes
open GnutellaOptions
open GnutellaGlobals
open GnutellaProtocol
  
(*************************************************************************)
(*                                                                       *)
(*                         Global Values                                 *)
(*                                                                       *)
(*************************************************************************)

let max_upload_buffer_len = 102400
let upload_buffer = String.create max_upload_buffer_len
let current_downloads = ref ([] : TcpBufferedSocket.t list)
  
(*************************************************************************)
(*                                                                       *)
(*                         clean_sources                                 *)
(*                                                                       *)
(*************************************************************************)

let clean_sources () =
  let list = ref [] in
  let obsolete_time = last_time () - 3600 in
  Hashtbl.iter (fun key listref ->
      let newlist = ref [] in
      List.iter (fun ((_, time) as u) ->
          if time > obsolete_time then newlist := u :: !newlist
      ) !listref;
      if !newlist <> [] then
        list := (key, newlist) :: !list
  ) result_sources;
  Hashtbl.clear result_sources;
  List.iter (fun (key, listref) ->
      Hashtbl.add result_sources key listref
  ) !list
  
  
(*************************************************************************)
(*                                                                       *)
(*                         download_finished                             *)
(*                                                                       *)
(*************************************************************************)
      
let download_finished file = 
  if List.memq file !current_files then begin
      file_completed (as_file file);
      CommonSwarming.remove_swarmer file.file_swarmer;
      file.file_swarmer <- None;
(* TODO: maybe this remove file may have a bad effect on sharing *)
      GnutellaGlobals.remove_file file;
      old_files =:= (file.file_name, file_size file) :: !!old_files;
      List.iter (fun c ->
          c.client_downloads <- remove_download file c.client_downloads
      ) file.file_clients
    end
  
(*************************************************************************)
(*                                                                       *)
(*                         check_finished                                *)
(*                                                                       *)
(*************************************************************************)
    
let check_finished swarmer file =
  if CommonSwarming.check_finished swarmer then
    download_finished file
  
(*************************************************************************)
(*                                                                       *)
(*                         init_client                                   *)
(*                                                                       *)
(*************************************************************************)
  
let init_client sock =
  TcpBufferedSocket.set_read_controler sock download_control;
  TcpBufferedSocket.set_write_controler sock upload_control;
  ()
  
(*************************************************************************)
(*                                                                       *)
(*                         find_request                                  *)
(*                                                                       *)
(*************************************************************************)

let rec find_request c sock headers =
(*
    if !verbose_msg_clients then begin
        lprintf "HEADER FROM CLIENT:\n";
        AnyEndian.dump_ascii header; 
      end; *)
  
  let d = 
    match c.client_requests with 
      [] -> failwith "[GDO] No download request !!!"
    | d :: tail ->
        c.client_requests <- tail;
        d
  in
  let file = d.download_file in
  
  (try
      match file.file_ttr with
        Some _ -> ()
      | None ->
          if not d.download_ttr_requested then
            let (url, _) = 
              List.assoc "x-thex-uri" headers 
(* TODO: this header is used by Gnutella2 when you want to specify to which
  depth you want the tigertree:
                  List.assoc "x-tigertree-path" headers *)
            in
            if String.length url > 0 && url.[0] = '/' then begin
                
                let s = Printf.sprintf "GET %s HTTP/1.1" url in
                
                let nblocks =
                  let size = file_size file in
                  let chunk_size = CommonUploads.tiger_block_size in
                  Int64.to_int (size // chunk_size) + 1 in
                let tiger_pos = CommonUploads.tiger_pos nblocks in
                let end_pos = tiger_pos + nblocks in
(* 24 is the size of SHA1 digests *)
                let end_pos = end_pos * 24 in
                let range = Printf.sprintf "0-%d" (end_pos-1) in
                
                if !verbose_msg_clients then
                  lprintf "[TTR] Requesting ttr with range %s\n" range;
                let headers =
                  ["Range", Printf.sprintf "bytes=%s" range]
                in
                let s = make_download_request c s headers in
                if !verbose_msg_clients then
                  lprintf "[TTR] SENDING TTR REQUEST: %s\n" (String.escaped s);
                write_string sock s;
                d.download_ttr_requested <- true;
                d.download_ranges <- d.download_ranges @ [TTRReq nblocks];
                c.client_requests <- c.client_requests @ [d];
              end else
              lprintf "[TTR] Url is not relative: %s\n" url;
    with _ -> ());
  
  
  match d.download_ranges with
    [] -> assert false
  | req :: tail ->
      d.download_ranges <- tail;
      match req with
      | HEADReq ->
          
          if !!gnutella_experimental && 
            not c.client_support_head_request then
            c.client_support_head_request <- true;
(* TODO: why do we use the HEAD request ?? For the headers ? *)
(*        lprintf "Received Reply to HEAD request: %s\n"
          (String.escaped header); *)
(*        d.download_head <- Head  (first_line, headers); *)
          (fun _ _ _ -> ()), read_more d c sock
      
      | RANGEReq (_,_,r) ->
(*    let size = file_size file in*)
          
          set_client_state c (Connected_downloading (file_num file));

(* Send the next request !!! *)
          for i = 1 to GnutellaNetwork.max_queued_ranges do
            if List.length d.download_ranges <= 
                GnutellaNetwork.max_queued_ranges then
              (try get_from_client sock c with _ -> ());
          done;
          
          read_some d c, read_more d c sock
      
      | TTRReq nblocks ->
(* TODO loading the ttr in main memory is probably a bad idea, we should
save it on disk in the next version. *)
          if !verbose_msg_clients then begin
              lprintf "[TTR] header received: \n";
              print_head "" headers;
            end;
          let buf = Buffer.create 100 in
          let read_ttr counter_pos b to_read_int =
            Buffer.add_subbytes buf b.buf b.pos to_read_int
          in
          let read_more () =
            if !verbose_msg_clients then
              lprintf "[TTR] ttr loaded\n";
            let ttr = Buffer.contents buf in
            let tiger_pos = CommonUploads.tiger_pos nblocks in
            let array = Array.make nblocks TigerTree.null in
            for i = 0 to nblocks - 1 do
              array.(i) <- TigerTree.direct_of_string 
                (String.sub ttr ((tiger_pos + i) * 24) 24)
            done;
            file.file_ttr <- Some array;
            
            (match file.file_swarmer with
                None -> ()
              | Some swarmer ->
                  if !verbose_msg_clients then
                    lprintf "[TTR] set_verifier\n";
                  CommonSwarming.set_verifier swarmer 
                    (Verification (Array.map (fun ttr -> TigerTree ttr) array))
            );
            
            
            
            read_more d c sock ()
          in
          read_ttr, read_more

(*************************************************************************)
(*                                                                       *)
(*                         read_some                                     *)
(*                                                                       *)
(*************************************************************************)

and read_some d c counter_pos b to_read_int = 
  
  let up = match d.download_uploader with
      None -> assert false
    | Some up -> up in
  
  let file = d.download_file in
  
  if file_state file <> FileDownloading then begin
      disconnect_client c Closed_by_user;
      raise Exit;
    end;

  begin
    try
      CommonSwarming.received up
        counter_pos b.buf b.pos to_read_int;
    with e -> 
        lprintf "FT: Exception %s in CommonSwarming.received\n"
          (Printexc2.to_string e);
(* TODO: we should pause the download !!! *)
  end;
  c.client_reconnect <- true

(*************************************************************************)
(*                                                                       *)
(*                         read_more                                     *)
(*                                                                       *)
(*************************************************************************)

and read_more d c sock () = 
(* If we have no more range to receive, disconnect *)
  if d.download_ranges = [] then get_from_client sock c

(*************************************************************************)
(*                                                                       *)
(*                         client_parse_header                           *)
(*                                                                       *)
(*************************************************************************)

and client_parse_header c gconn sock (first_line, headers) = 
  if !verbose_msg_clients then begin
      lprintf "[GDO] CLIENT PARSE HEADER\n"; 
    end;
  try
    set_lifetime sock 3600.;


(* The reply should be  "HTTP/1.1 200 OK" *)
    let space_pos = String.index first_line ' ' in
    let code = String.sub first_line (space_pos+1) 3 in
    let code = int_of_string code in
    
    GnutellaProtocol.parse_headers c first_line headers;

(* TODO in case of bad code, just jump to the next reply header... *)
    if  code < 200 || code > 299 then
      failwith "Bad HTTP code";    
    
    connection_ok c.client_connection_control;
    set_client_state c Connected_initiating;    
    set_rtimeout sock 120.;
    
    let start_pos, end_pos = 
      try
        let (range,_) = List.assoc "content-range" headers in
        try
          let npos = (String.index range 'b')+6 in
          let dash_pos = try String.index range '-' with _ -> -10 in
          let slash_pos = try String.index range '/' with _ -> -20 in
          let star_pos = try String.index range '*' with _ -> -30 in
          if star_pos = slash_pos-1 then
            failwith "Cannot parse range"
(*            Int64.zero, size (* "bytes */X" *) *)
          else 
          let x = Int64.of_string (
              String.sub range npos (dash_pos - npos) )
          in
          let len = String.length range in
          let y = Int64.of_string (
              String.sub range (dash_pos+1) (slash_pos - dash_pos - 1))
          in
          if slash_pos = star_pos - 1 then 
            x, Int64.succ y (* "bytes x-y/*" *)
          else
          let z = Int64.of_string (
              String.sub range (slash_pos+1) (len - slash_pos -1) )
          in
          if y = z then 
            failwith "Cannot parse range"
(* Int64.pred x, size *)
          else x, Int64.succ y
        with 
        | e ->
            lprintf "[GDO] Exception %s for range [%s]\n" 
              (Printexc2.to_string e) range;
            raise e
      with e -> 
(* TODO: we should be able to reply to a request containing no Content-Range
iff Content-Length = Requested Length and 
  Requested Range = 0-Content-Length-1 ??? *)

(*
          try
            if code <> 206 && code <> 200 then raise Not_found;
            let (len,_) = List.assoc "content-length" headers in
            let len = Int64.of_string len in
            lprintf "Specified length: %Ld\n" len;
            match d.download_ranges with
              [] -> raise Not_found
            | (start_pos,end_pos,r) :: _ -> 
                lprintf "WARNING: Assuming client is replying to range\n";
                if len <> end_pos -- start_pos then
                  begin
                    lprintf "\n\nERROR: bad computed range: %Ld-%Ld/%Ld \n"
                      start_pos end_pos len;
                    print_head first_line headers;
                    raise Not_found
                  end;
                (start_pos, end_pos)
          with _ ->  *)
(* A bit dangerous, no ??? *)
          if !verbose_unknown_messages then
            begin
              lprintf "[GDO] ERROR: Could not find/parse range header (exception %s), disconnect\n" 
                (Printexc2.to_string e);
              print_head first_line headers
            end;
          disconnect_client c (Closed_for_error "Bad HTTP Range");
          raise Exit
    in 
    (try
        let (len,_) = List.assoc "content-length" headers in
        let len = Int64.of_string len in
(*        lprintf "[GDO] Specified length: %Ld\n" len; *)
        if len <> end_pos -- start_pos then
          begin
            lprintf "[GDO] ERROR: bad computed range: %Ld-%Ld/%Ld \n"
              start_pos end_pos len;
            print_head first_line headers;
            failwith "Bad Computed Range"
          
          end
      with _ -> 
          lprintf "[WARNING]: no Content-Length field\n";
          print_head first_line headers;
    );

(*
    lprintf "[GDO] Receiving range: %Ld-%Ld (len = %Ld)\n"    
      start_pos end_pos (end_pos -- start_pos); 
    print_head first_line headers;
*)
    
    let read_some, read_more = find_request c sock headers in
    
    let counter_pos = ref start_pos in
    gconn.gconn_handler <- Reader (fun gconn sock ->
        
        let b = TcpBufferedSocket.buf sock in
        let to_read = min (end_pos -- !counter_pos) 
          (Int64.of_int b.len) in

(*        lprintf "[GDO] Reading: end_pos %Ld counter_pos %Ld len %d = to_read %Ld\n"
          end_pos !counter_pos b.len to_read;
  *)      
        let to_read_int = Int64.to_int to_read in
        
        read_some !counter_pos b to_read_int;
        buf_used b to_read_int;
        counter_pos := !counter_pos ++ to_read;
        if !counter_pos = end_pos then begin
            read_more ();
            gconn.gconn_handler <- HttpReader (4,
              ["HTTP",client_parse_header c], 
              GnutellaFunctions.default_handler)
          end)
  
  with e ->
      if !verbose_unknown_messages then
        begin
          lprintf "[GDO] Exception %s in client_parse_header\n" (Printexc2.to_string e);
          print_head first_line headers;
        end;
      disconnect_client c (Closed_for_exception e);
      raise e

(*************************************************************************)
(*                                                                       *)
(*                         get_from_client                               *)
(*                                                                       *)
(*************************************************************************)

(* TODO: implement TigerTree download *)
(* TODO: use Gnutella2 alternative uids *)

and get_from_client sock (c: client) =
  
  let rec iter downloads =
    match downloads with
      [] -> 
        if !verbose_msg_clients then
          lprintf "No other download to start\n";
        raise Not_found
    | d :: tail ->
        
        if file_state d.download_file <> FileDownloading then iter tail
        else begin
            
            let file = d.download_file in
            
            let swarmer = match file.file_swarmer with
                None -> assert false | Some sw -> sw
            in
            
            let up = match d.download_uploader with
                None -> 
                  let chunks = [ Int64.zero, file_size file ] in
                  let up = CommonSwarming.register_uploader swarmer 
                      (as_client c)
                    (AvailableIntervals chunks) in
                  d.download_uploader <- Some up;
                  up                
              
              | Some up -> up in 
            
            if !verbose_msg_clients then begin
                lprintf "FINDING ON CLIENT\n";
              end;
            let file = d.download_file in
            if !verbose_msg_clients then begin
                lprintf "FILE FOUND, ASKING\n";
              end;
            
            if !verbose_swarming then begin
                lprintf "Current download:\n  Current chunks: "; 
                List.iter (fun (x,y) -> lprintf "%Ld-%Ld " x y) d.download_chunks;
                lprintf "\n  Current ranges: ";
                List.iter (fun req ->
                    match req with
                      RANGEReq (x,y,r) ->
(*              let (x,y) = CommonSwarming.range_range r               in *)
                        lprintf "%Ld-%Ld " x y
                    | HEADReq -> 
                        lprintf "HEAD"
                    | TTRReq _ ->
                        lprintf "TTR"
                ) d.download_ranges;
                
                lprintf "\n  Current blocks: ";
(*          List.iter (fun b -> CommonSwarming.print_block b.up_block) d.download_blocks; *)
                lprintf "\n\nFinding Range: \n";
              end;
            let range = 
              try
                let rec iter () =
                  match d.download_blocks with
                  | [] -> 
                      if !verbose_swarming then lprintf "No block\n";
                      (try CommonSwarming.verify_one_chunk swarmer with _ -> ());
                      let _chunk, blocks = CommonSwarming.find_blocks up in
                      
                      if !verbose_swarming then begin
                          lprintf "GOT BLOCKS:\n";
                          CommonSwarming.print_uploaders swarmer;
                          lprintf "Blocks Found: "; 
                          List.iter (fun b ->
                            CommonSwarming.print_block b.up_block) blocks;
                        end;
                      
                      d.download_blocks <- blocks;
                      iter ()
                  | blocks ->
(*
                if !verbose_swarming then begin
                    lprintf "Current Block: "; CommonSwarming.print_block b;
end;
  *)
                      try
                        let (x,y,r) = 
                          CommonSwarming.find_range up (Int64.of_int (256 * 1024)) in 
                        
                        if !verbose_swarming then begin
                            lprintf "GOT RANGE:\n";
                            CommonSwarming.print_uploaders swarmer;
                          end;
                        d.download_ranges <- d.download_ranges @ 
                          [RANGEReq (x,y,r)];
                        Printf.sprintf "%Ld-%Ld" x (Int64.pred y)
                      with Not_found ->
                          if !verbose_swarming then 
                            lprintf "Could not find range in current block\n";
                          d.download_blocks <- [];
                          iter ()
                in
                iter ()
              with Not_found -> 
    if !verbose_unknown_messages then
                  lprintf_nl "Unable to get a block !!";
                  check_finished swarmer file;
                  raise Not_found
            in
            let s = request_of_download "GET" d in
            let headers =
              ("Range", Printf.sprintf "bytes=%s" range) :: []
            in
            let s = make_download_request c s headers in
            if !verbose_msg_clients then
              lprintf "SENDING REQUEST: %s\n" (String.escaped s);
            write_string sock s;
            c.client_requests <- c.client_requests @ [d];
            if !verbose_msg_clients then 
              lprintf "Asking %s For Range %s\n" (Md4.to_string c.client_user.user_uid) 
              range
          end
  in 
  iter c.client_downloads

(*************************************************************************)
(*                                                                       *)
(*                         disconnect_client                             *)
(*                                                                       *)
(*************************************************************************)

and disconnect_client c r =
  if !verbose then
    lprintf_nl "DISCONNECT CLIENT";
  match c.client_sock with
  | Connection sock -> 
      (try
          if !verbose_msg_clients then
              lprintf_nl "Disconnected from source for %s" (string_of_reason r); 
          c.client_requests <- [];
          connection_failed c.client_connection_control;
          set_client_disconnected c r;
          close sock r;
          c.client_sock <- NoConnection;
          List.iter (fun d ->
              let file = d.download_file in
              if not (List.memq file c.client_in_queues) then begin
                  Queue.put file.file_clients_queue (0,c);
                  c.client_in_queues <- file :: c.client_in_queues
                end;
              match d.download_uploader with
                None -> ()
              | Some up ->
                  d.download_uploader <- None;
                  CommonSwarming.unregister_uploader up;
                  d.download_blocks <- [];
                  d.download_ranges <- [];
          ) c.client_downloads;
          begin
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
          end;
          
          if c.client_reconnect then begin
              c.client_reconnect <- false;
              connect_client c
            end
        with e -> 
            lprintf "Exception %s in disconnect_client\n"
              (Printexc2.to_string e))
  | _ -> ()

(*************************************************************************)
(*                                                                       *)
(*                         connect_client                                *)
(*                                                                       *)
(*************************************************************************)

and connect_client c =
  match c.client_sock with
  | Connection _ | ConnectionWaiting _ -> ()
  | NoConnection ->

(* Count this connection in the first file counter. Here, we assume
that the connection will not be aborted (otherwise, disconnect_client
should clearly be called). *)
      let download = ref None in
      (try List.iter (fun d ->
              let file = d.download_file in
              if file_state file = FileDownloading then
                begin
                  download := Some d;
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
      begin
        
        match c.client_user.user_kind with
          Indirect_location (_, uid, _, _) -> 
            GnutellaProto.ask_for_push uid 
        | Known_location (ip, port) ->
            let token =
              add_pending_connection connection_manager (fun token ->
                  try
                    if !verbose_msg_clients then begin
                        lprintf "connect_client\n";
                      end;
                    if !verbose_msg_clients then begin
                        lprintf "connecting %s:%d\n" (Ip.to_string ip) port; 
                      end;
                    c.client_reconnect <- false;
                    let sock = connect token "gnutella download" 
                        (Ip.to_inet_addr ip) port
                        (fun sock event ->
                          match event with
                          | BASIC_EVENT RTIMEOUT ->
                              disconnect_client c Closed_for_timeout
                          | BASIC_EVENT LTIMEOUT ->
                              disconnect_client c Closed_for_lifetime
                          | BASIC_EVENT (CLOSED s) ->
                              disconnect_client c s

(* You can only use the CONNECTED signal if the socket is not yet controlled
by the bandwidth manager... 2004/02/03: Normally, not true anymore, it should
  now work even in this case... *)
                          
                          | CONNECTED ->
                              init_client sock;                
                              get_from_client sock c
                          | _ -> ()
                      )
                    in
                    
                    c.client_host <- Some (ip, port);
                    c.client_country_code <- None;
                    check_client_country_code c;
                    set_client_state c Connecting;
                    c.client_sock <- Connection sock;
                    TcpBufferedSocket.set_closer sock (fun _ s ->
                        disconnect_client c s
                    );
                    set_rtimeout sock 30.;
                    set_gnutella_sock sock !verbose_msg_clients
                      (HttpReader (4, ["HTTP", client_parse_header c], 
                        GnutellaFunctions.default_handler))
                  
                  with e ->
                      lprintf "Exception %s while connecting to client\n" 
                        (Printexc2.to_string e);
                      disconnect_client c (Closed_for_exception e)
              );
            in
            c.client_sock <- ConnectionWaiting token
      end
      
(*
  
1022569854.519 24.102.10.39:3600 -> 212.198.235.45:51736 of len 82
ascii [ 
G I V   8 1 : 9 7 4 3 2 1 3 F B 4 8 6 2 3 D 0 F F D F A B B 3 8 0 E C 6 C 0 0 / P o l i c e   V i d e o   -   E v e r y   B r e a t h   Y o u   T a k e . m p g(10)(10)]

"GIV %d:%s/%s\n\n" file.file_number client.client_md4 file.file_name

*)


(*************************************************************************)
(*                                                                       *)
(*                         push_handler                                  *)
(*                                                                       *)
(*************************************************************************)

let push_handler cc gconn sock (first_line, headers) = 
  if !verbose_msg_clients then begin
      lprintf "PUSH";
      print_head first_line headers;
    end;
  try
    let (ip, port) = TcpBufferedSocket.peer_addr sock in
    
    if !verbose_msg_clients then begin    
        lprintf "PARSING GIV HEADER\n"; 
      end;
    let colon_pos = String.index first_line ':' in
    let uid = Md4.of_string (String.sub first_line (colon_pos+1) 32) in
    let index = int_of_string (String.sub first_line 4 (colon_pos-4)) in
    if !verbose_msg_clients then begin
        lprintf "PARSED\n";
      end;
    let c = try
        Hashtbl.find clients_by_uid (Indirect_location ("", uid, ip, port)) 
      with _ ->
          try
            Hashtbl.find clients_by_uid (Known_location (ip,port))
          with _ ->
              let c = new_client  (Indirect_location ("", uid, ip, port)) in
              if String.length c.client_user.user_nick == 0 then
                c.client_user.user_nick <- (Md4.to_string uid);
              c
            
    in
    c.client_host <- Some (ip, port);
    check_client_country_code c;
    match c.client_sock with
    | Connection _ -> 
        if !verbose_msg_clients then begin
            lprintf "ALREADY CONNECTED\n"; 
          end;
        close sock (Closed_for_error "already connected");
        raise End_of_file
    | _ ->
        if !verbose_msg_clients then begin
            lprintf "NEW CONNECTION\n";
          end;
        cc := Some c;
        c.client_sock <- Connection sock;
        connection_ok c.client_connection_control;
        try
          if !verbose_msg_clients then begin
              lprintf "FINDING FILE %d\n" index; 
            end;
          let d = find_download_by_index index c.client_downloads in
          if !verbose_msg_clients then begin
              lprintf "FILE FOUND\n";
            end;
          
          c.client_downloads <- d :: (List2.removeq d c.client_downloads);
          get_from_client sock c;
          gconn.gconn_handler <- HttpReader (4, 
            ["HTTP", client_parse_header c], 
            GnutellaFunctions.default_handler)
        with e ->
            lprintf "Exception %s during client connection\n"
              (Printexc2.to_string e);
            disconnect_client c (Closed_for_exception e);
            raise End_of_file
  with e ->
      lprintf "Exception %s in push_handler\n" (Printexc2.to_string e);
      print_head first_line headers;
      (match !cc with Some c -> disconnect_client c (Closed_for_exception e)
        | _ -> ());
      raise e

(*************************************************************************)
(*                                                                       *)
(*                         read_request                                  *)
(*                                                                       *)
(*************************************************************************)

(* TODO: add Gnutella2 alternative UIDs *)
(* TODO: implement TigerTree upload *)
      
let read_request url headers gconn sock =
  let url = Url.of_string url in
  
  let reader, size, add_headers = find_file_to_upload gconn url in
  let partial, (chunk_pos, chunk_end) = 
    try
      true, 
      let (range,_) = List.assoc "range" headers in
      match Http_server.parse_range range with
        x, None, _ -> x, size
      | x, Some y, Some z ->
          if y = z then (* some vendor bug *)
            Int64.pred x, y
          else
            x, Int64.succ y
      | x, Some y, None ->
          x, Int64.succ y
    with _ -> false,  (Int64.zero, size)
  
  in
  let chunk_len = chunk_end -- chunk_pos in
(* TODO: assert that the the range is inside the file *)
  
  
  let header =           
    let s = 
      Printf.sprintf "HTTP/1.1 %s"
        (if partial then "206 Partial Reply" else "200 OK") in
    let headers =
      ("Connection", "Keep-Alive") ::
      ("Content-type", "application/binary") ::
      ("Content-length", Printf.sprintf "%Ld" chunk_len) ::
      add_headers
    in
    let headers =
      if partial then begin
          ("Accept-Ranges", "bytes") ::
          ("Content-range", 
            Printf.sprintf "bytes %Ld-%Ld/%Ld"
              chunk_pos (Int64.pred chunk_end) size) ::
          headers
        end else headers in
    let headers =
      if gconn.gconn_client_info_sent then headers else
        begin
          gconn.gconn_client_info_sent <- true;
          ("Remote-IP", Ip.to_string (fst (peer_addr sock)) ) ::
          ("Server", get_user_agent ()) :: headers
        end
    in
    make_http_header s headers
  in
  
  let uc = {
      uc_sock = sock;
      uc_partial = true;
      uc_reader = reader;
      uc_chunk_pos = chunk_pos;
      uc_chunk_len = chunk_end -- chunk_pos;
      uc_chunk_end = chunk_end;
      uc_size = size;
      uc_header = header;
    } in
  uc

(*************************************************************************)
(*                                                                       *)
(*                         find_slot                                     *)
(*                                                                       *)
(*************************************************************************)

let find_slot sock =
  let rec iter list rem =
    match list with 
      [] ->
        if List.length rem >= !!max_available_slots then begin
            
            if !verbose_msg_clients then begin
                lprintf "[GUP] All slots used:\n";
                List.iter (fun s ->
                    let (ip, port) = peer_addr s in
                    lprintf "    by %s:%d %b\n" (Ip.to_string ip) port
                      (closed s);
                ) rem;
              end;
            failwith "All Slots Used";
          end;
        sock :: rem
    | s :: tail ->
        if s == sock then list @ rem
        else
        if closed s then
          iter tail rem
        else
          iter tail (s :: rem)
  in
  current_downloads := iter !current_downloads []
  

(*************************************************************************)
(*                                                                       *)
(*                         get_handler                                   *)
(*                                                                       *)
(*************************************************************************)

let get_handler get_request cc gconn sock (first_line, headers) = 
  if !verbose_msg_clients then begin
      lprintf "[GUP] GET";
      print_head first_line headers;
    end;
  try
(* We don't want to buffer more than 100 kB per upload connection *)
    set_max_output_buffer sock max_upload_buffer_len;
    if !verbose_msg_clients then
      lprintf "[GUP] After set_max_output_buffer: max_refill %d\n" 
        (max_refill sock);
(*        lprintf "parse_head\n";    *)
    
    let request_end = String.index first_line ' ' in
    let request = String.sub first_line 0 request_end in
    
    let url_end = String.index_from first_line (request_end+1) ' ' in
    let url = String.sub first_line (request_end+1)
      (url_end - request_end-1) in
    
    let proto_len = String.length first_line in
    let proto = String.sub first_line (url_end+1) (proto_len-url_end-1) in
    if !verbose_msg_clients then
      lprintf "[GUP] Header parsed: [%s] [%s] [%s]\n" request url proto;
(* "/get/num/filename" *)

(* First of all, can we accept this request ???? *)
    find_slot sock;    
    let uc = read_request url headers gconn sock in
    let header_sent = ref false in

(* For HEAD request, do as if we had already sent the data *)
    if not get_request then  uc.uc_chunk_pos <- uc.uc_chunk_end;
    
    let write_done = ref false in
    let rec refill sock =
      if !verbose_msg_clients then
        lprintf "[GUP] refill called\n";
      if !write_done && not !!keep_alive && remaining_to_write sock = 0 then
        begin
          if !verbose_msg_clients then
            lprintf "CLOSING AFTER WRITE\n";
          close sock (Closed_by_user)
        
      end else
      let can_write = max_refill sock in
      if not !header_sent then
        if can_write > String.length uc.uc_header then begin
(* BUG: send the header *)
            TcpBufferedSocket.write_string sock uc.uc_header;
            if !verbose_msg_clients then begin
                lprintf "[GUP] Sending Header:\n";
                AnyEndian.dump uc.uc_header;
                lprintf "\n";
              end;
            header_sent:=true;
          end;
        if !header_sent then begin
            if uc.uc_chunk_pos = uc.uc_chunk_end then begin
                if !verbose_msg_clients then
                  lprintf "[GUP] Finished replying to header\n";
                write_done := true;
                if !!keep_alive then
                  match gconn.gconn_refill with
                    []  -> ()
                  | [_] -> 
                      gconn.gconn_refill <- []
                  | _ :: ((refill :: _ ) as tail) -> 
                      gconn.gconn_refill <- tail;
                      refill sock
              end else
              
            if get_request then
              let pos = uc.uc_chunk_pos in
              let to_write = uc.uc_chunk_end -- pos in
              let rlen = min (max_refill sock) (Int64.to_int to_write) in
              if !verbose_msg_clients then
                lprintf "[GUP] to_write: %d/%Ld/%d\n" rlen to_write
                  (remaining_to_write sock);
              
              if rlen > 0 then begin
                  uc.uc_reader pos upload_buffer 0 rlen;
                  if !verbose_msg_clients then
                    lprintf "[GUP] Writting %d\n" rlen;
                  TcpBufferedSocket.write sock upload_buffer 0 rlen;
                  
                  uc.uc_chunk_pos <- uc.uc_chunk_pos ++ (Int64.of_int rlen);
                  if remaining_to_write sock = 0 then refill sock
                end
          end;
    in
    gconn.gconn_refill <- gconn.gconn_refill @ [refill];
    if !verbose_msg_clients then
      lprintf "[GUP] refill: %d refillers\n" (List.length gconn.gconn_refill);
    match gconn.gconn_refill with
      refill :: tail ->
(* First refill handler, must be called immediatly *)
        refill sock
    | _ -> (* Already a refill handler, wait for it to finish its job *)
        ()
  
  with e ->
(* TODO: send back a 404 NOT FOUND error *)
      if !verbose_msg_clients then begin
          lprintf "[GUP] Exception %s in get_handler\n" (Printexc2.to_string e);
          print_head first_line headers;
        end;
      close sock (Closed_for_exception e);
      raise e
      
(*************************************************************************)
(*                                                                       *)
(*                         listen                                        *)
(*                                                                       *)
(*************************************************************************)

let listen () =
  try
    let sock = TcpServerSocket.create "gnutella client server" 
        (Ip.to_inet_addr !!client_bind_addr)
        !!client_port
        (fun sock event ->
          match event with
            TcpServerSocket.CONNECTION (s, 
              Unix.ADDR_INET(from_ip, from_port)) ->
              if !verbose then
                lprintf "CONNECTION RECEIVED FROM %s FOR PUSH\n%s"
                  (Ip.to_string (Ip.of_inet_addr from_ip))
                    "*********** CONNECTION ***********\n";
              
              let token = create_token connection_manager in
              let sock = TcpBufferedSocket.create token
                  "gnutella client connection" s 
                  (fun sock event -> 
                    match event with
                      BASIC_EVENT RTIMEOUT -> close sock Closed_for_timeout
                    | BASIC_EVENT LTIMEOUT ->  close sock Closed_for_lifetime
                    | _ -> ()
                )
              in
              TcpBufferedSocket.set_read_controler sock download_control;
              TcpBufferedSocket.set_write_controler sock upload_control;

              let c = ref None in
              TcpBufferedSocket.set_closer sock (fun _ s ->
                  match !c with
                    Some c ->  disconnect_client c s
                  | None -> 
                      if !verbose then
                        lprintf "DISCONNECTION BEFORE CLIENT %s:%d IS KNOWN\n"
                          (Ip.to_string (peer_ip sock)) (peer_port sock)
              );
              TcpBufferedSocket.set_rtimeout sock 30.;
              set_gnutella_sock sock !verbose_msg_clients
                (HttpReader (4,
                  [
                    "GIV", push_handler c;
                    "GET", get_handler true c;
                    "HEAD", get_handler false c;
                  ], GnutellaFunctions.default_handler)
              );
          | _ -> ()
      ) in
    listen_sock := Some sock;
    ()
  with e ->
      lprintf "Exception %s while init gnutella server\n" 
        (Printexc2.to_string e)

(*************************************************************************)
(*                                                                       *)
(*                         push_connection                               *)
(*                                                                       *)
(*************************************************************************)
      
let push_connection guid index ip port =
  let _ =
    add_pending_connection connection_manager (fun token ->
        let sh =Hashtbl.find shareds_by_id index in
        let sock = connect token "gnutella download" 
            (Ip.to_inet_addr ip) port
            (fun sock event -> 
              match event with
                BASIC_EVENT RTIMEOUT -> close sock Closed_for_timeout
              | BASIC_EVENT LTIMEOUT -> close sock Closed_for_lifetime
              | _ -> ()
          )
        in
        lprintf "CONNECTION PUSHED TO %s\n" (Ip.to_string ip); 
        
        TcpBufferedSocket.set_read_controler sock download_control;
        TcpBufferedSocket.set_write_controler sock upload_control;
        
        let c = ref None in
        TcpBufferedSocket.set_closer sock (fun _ s ->
            match !c with
              Some c ->  disconnect_client c s
            | None -> ()
        );
        TcpBufferedSocket.set_rtimeout sock 30.;
(* TODO test this, looks strange... *)
        set_gnutella_sock sock !verbose_msg_clients
          (HttpReader (4, [
            "GET", get_handler true c;
            "HEAD", get_handler false c;
            ], GnutellaFunctions.default_handler));
        write_string sock 
          (Printf.sprintf "GIV %d:%s/%s\n\n" 
            index (Md4.to_string guid) sh.shared_codedname)
    )

  in
  ()
  
