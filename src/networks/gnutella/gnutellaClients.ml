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



(*

How shareaza knows the correspondances between URNs:
  
ascii: [ H T T P / 1 . 1   5 0 3   B u s y(10)(10) S e r v e r :   S h a r e a z a   1 . 8 . 8 . 0(10)(10) R e m o t e - I P :   8 1 . 1 0 0 . 8 6 . 1 4 3(10)(10) C o n n e c t i o n :   K e e p - A l i v e(10)(10) A c c e p t - R a n g e s :   b y t e s(10)(10) X - P e r H o s t :   2(10)(10) X - N i c k :   B i l l o u t 6 6 6(10)(10) X - C o n t e n t - U R N :   u r n : b i t p r i n t : U K 4 B I K S O X 5 D 3 N C W 3 5 G Y A L U H A T U 5 4 C D I V . E T U 4 3 V C R 3 T N K V Z R M 3 U N S K 5 G T B 5 H X A V M F U W W C Z Y Y(10)(10) X - C o n t e n t - U R N :   e d 2 k : 9 a 9 4 c 7 8 9 5 8 9 7 f f a 5 d 7 c f e 5 8 0 b e a 9 7 2 8 6(10)(10) X - T i g e r T r e e - P a t h :   / g n u t e l l a / t i g e r t r e e / v 3 ? u r n : t r e e : t i g e r / : E T U 4 3 V C R 3 T N K V Z R M 3 U N S K 5 G T B 5 H X A V M F U W W C Z Y Y(10)(10) X - T h e x - U R I :   / g n u t e l l a / t h e x / v 1 ? u r n : t r e e : t i g e r / : E T U 4 3 V C R 3 T N K V Z R M 3 U N S K 5 G T B 5 H X A V M F U W W C Z Y Y & d e p t h = 9 & e d 2 k = 0(10)(10) C o n t e n t - T y p e :   t e x t / h t m l(10)(10) C o n t e n t - L e n g t h :   2 1 2 5(10)(10)(10)]

*)
  
open CommonShared
open CommonUploads
open Printf2
open CommonOptions
open CommonDownloads
open Md4
open CommonInteractive
open CommonClient
open CommonComplexOptions
open CommonTypes
open CommonFile
open Options
open BasicSocket
open TcpBufferedSocket

open CommonGlobals
open CommonSwarming  
open GnutellaTypes
open GnutellaOptions
open GnutellaGlobals
open GnutellaComplexOptions

open GnutellaProtocol

let http_ok = "HTTP 200 OK"
let http11_ok = "HTTP/1.1 200 OK"
  
let disconnect_client c r =
  match c.client_sock with
  | Connection sock -> 
      (try
        if !verbose_msg_clients then begin
            lprintf "Disconnected from source\n"; 
          end;
        c.client_requests <- [];
        connection_failed c.client_connection_control;
        set_client_disconnected c r;
        close sock r;
        c.client_sock <- NoConnection
      with e -> 
          lprintf "Exception %s in disconnect_client\n"
            (Printexc2.to_string e))
  | _ -> ()

let download_finished file = 
  if List.memq file !current_files then begin
      file_completed (as_file file.file_file);
      GnutellaGlobals.remove_file file;
      old_files =:= (file.file_name, file_size file) :: !!old_files;
      List.iter (fun c ->
          c.client_downloads <- remove_download file c.client_downloads
      ) file.file_clients
    end
    
let check_finished file =
  if file_state file <> FileDownloaded &&
    (file_size file = Int64Swarmer.downloaded file.file_swarmer) then
    download_finished file
   
let (++) = Int64.add
let (--) = Int64.sub
  
(*
let client_to_client s p sock =
  match p.pkt_payload with
  | _ -> ()
*)      

                
let rec client_parse_header c gconn sock header = 
  if !verbose_msg_clients then begin
      lprintf "CLIENT PARSE HEADER\n"; 
    end;
  try
    set_lifetime sock 3600.;
    let d = 
      match c.client_requests with 
        [] -> failwith "No download request !!!"
      | d :: tail ->
          c.client_requests <- tail;
          d
    in
    connection_ok c.client_connection_control;
    set_client_state c Connected_initiating;    
    if !verbose_msg_clients then begin
        lprintf "HEADER FROM CLIENT:\n";
        AnyEndian.dump_ascii header; 
      end;
    let file = d.download_file in
    let size = file_size file in
    
    let endline_pos = String.index header '\n' in
    let http, code = 
      match String2.split (String.sub header 0 endline_pos
        ) ' ' with
      | http :: code :: ok :: _ -> 
          let code = int_of_string code in
          if not (String2.starts_with (String.lowercase http) "http") then
            failwith "Not in http protocol";
          http, code
      | _ -> failwith "Not a HTTP header line"
    in
    if !verbose_msg_clients then begin
        lprintf "GOOD HEADER FROM CONNECTED CLIENT\n";
      end;
    
    set_rtimeout sock 120.;
(*              lprintf "SPLIT HEADER...\n"; *)
    let lines = Http_client.split_header header in
(*              lprintf "REMOVE HEADLINE...\n"; *)
    let headers = match lines with
        [] -> raise Not_found        
      | _ :: headers -> headers
    in
(*                  lprintf "CUT HEADERS...\n"; *)
    let headers = Http_client.cut_headers headers in
(*                  lprintf "START POS...\n"; *)
    
    begin
      try
        
        let (locations,_) = 
          List.assoc "x-gnutella-alternate-location" headers in
        let locations = String2.split locations ',' in
        
        lprintf "Alternate locations\n";
        let urls = List.map (fun s ->
              match String2.split_simplify s ' ' with
                [] -> lprintf "Cannot parse : %s\n" s; ""
              | url :: _ ->
                  lprintf "  Location: %s\n" url; url
          ) locations in
        lprintf "\n";
        
        let files = ref [] in
        (try
            let (urn,_) = List.assoc "x-gnutella-content-urn" headers in

(* Contribute: maybe we can find the bitprint associated with a SHA1 here,
  and use it for corruption detection... *)
            
            
            
            let uids = expand_uids [uid_of_string urn] in
            List.iter (fun uid ->
                try
                  files := (Hashtbl.find files_by_uid uid) :: !files
                
                with _ -> ()
            ) uids
          with Not_found ->
              match c.client_requests with 
                d :: _ -> files := d.download_file :: !files
              | _ -> ()
        );
        
        List.iter (fun file ->
            List.iter (fun url ->
                try
                  let url = Url.of_string url in
                  let ip = Ip.of_string url.Url.server in
                  let port = url.Url.port in
                  let uri = url.Url.full_file in
                  
                  let c = new_client (Known_location (ip,port)) in
                  add_download file c (FileByUrl uri)
                
                with _ -> ()
            ) urls
        ) !files
      
      with _ -> ()
    
    end;

(*                
Shareaza adds:
X-TigerTree-Path: /gnutella/tigertree/v2?urn:tree:tiger/:YRASTJJK6JPRHREV3JGIFLSHSQAYDODVTSJ4A3I
X-Metadata-Path: /gnutella/metadata/v1?urn:tree:tiger/:7EOOAH7YUP7USYTMOFVIWWPKXJ6VD3ZE633C7AA
*)
    
    if  code < 200 || code > 299 then
      failwith "Bad HTTP code";
    
    
    let start_pos, end_pos = 
      try
        let (range,_) = List.assoc "content-range" headers in
        try
          let npos = (String.index range 'b')+6 in
          let dash_pos = try String.index range '-' with _ -> -10 in
          let slash_pos = try String.index range '/' with _ -> -20 in
          let star_pos = try String.index range '*' with _ -> -30 in
          if star_pos = slash_pos-1 then
            Int64.zero, size (* "bytes */X" *)
          else
          let x = Int64.of_string (
              String.sub range npos (dash_pos - npos) )
          in
          let len = String.length range in
          let y = Int64.of_string (
              String.sub range (dash_pos+1) (slash_pos - dash_pos - 1))
          in
          if slash_pos = star_pos - 1 then 
            x,y ++ Int64.one (* "bytes x-y/*" *)
          else
          let z = Int64.of_string (
              String.sub range (slash_pos+1) (len - slash_pos -1) )
          in
          if y = z then x -- Int64.one, size else 
            x,y ++ Int64.one
        with 
        | e ->
            lprintf "Exception %s for range [%s]\n" 
              (Printexc2.to_string e) range;
            raise e
      with e -> 
          try
            if code <> 206 then raise Not_found;
            let (len,_) = List.assoc "content-length" headers in
            let len = Int64.of_string len in
            lprintf "Specified length: %Ld\n" len;
            match d.download_ranges with
              [] -> raise Not_found
            | (start_pos,end_pos,r) :: _ -> 
                lprintf "WARNING: Assuming client is replying to range\n";
                if len <> end_pos -- start_pos then
                  begin
                    lprintf "\n\nERROR: bad computed range: %Ld-%Ld/%Ld \n%s\n"
                      start_pos end_pos len
                      (String.escaped header);
                    raise Not_found
                  end;
                (start_pos, end_pos)
          with _ -> 
(* A bit dangerous, no ??? *)
              lprintf "ERROR: Could not find/parse range header (exception %s), disconnect\nHEADER: %s\n" 
                (Printexc2.to_string e)
              (String.escaped header);
              disconnect_client c (Closed_for_error "Bad range");
              raise Exit
    in 
    (try
        let (len,_) = List.assoc "content-length" headers in
        let len = Int64.of_string len in
        lprintf "Specified length: %Ld\n" len;
        if len <> end_pos -- start_pos then
          begin
            failwith "\n\nERROR: bad computed range: %Ld-%Ld/%Ld \n%s\n"
              start_pos end_pos len
              (String.escaped header);
          end
      with _ -> 
          lprintf "[WARNING]: no Content-Length field\n%s\n"
            (String.escaped header)
    );
    
    lprintf "Receiving range: %Ld-%Ld (len = %Ld)\n%s\n"    
      start_pos end_pos (end_pos -- start_pos)
    (String.escaped header)
    ;
    set_client_state c (Connected_downloading);
    let counter_pos = ref start_pos in
(* Send the next request !!! *)
    for i = 0 to 6 do
      if List.length d.download_ranges < 6 then
        (try get_from_client sock c with _ -> ());
    done;
    gconn.gconn_handler <- Reader (fun gconn sock ->
        if file_state file <> FileDownloading then begin
            disconnect_client c Closed_by_user;
            raise Exit;
          end;

        let b = TcpBufferedSocket.buf sock in
        let to_read = min (end_pos -- !counter_pos) 
          (Int64.of_int b.len) in
        (*
        lprintf "Reading: end_pos %Ld counter_pos %Ld len %d = to_read %Ld\n"
end_pos !counter_pos b.len to_read;
   *)
        let to_read_int = Int64.to_int to_read in
(*
  lprintf "CHUNK: %s\n" 
          (String.escaped (String.sub b.buf b.pos to_read_int)); *)
        let old_downloaded = 
          Int64Swarmer.downloaded file.file_swarmer in
        List.iter (fun (_,_,r) -> Int64Swarmer.free_range r) 
        d.download_ranges;
        
        Int64Swarmer.received file.file_swarmer
          !counter_pos b.buf b.pos to_read_int;
        List.iter (fun (_,_,r) ->
            Int64Swarmer.alloc_range r) d.download_ranges;
        let new_downloaded = 
          Int64Swarmer.downloaded file.file_swarmer in
        
        (match d.download_ranges with
            [] -> lprintf "EMPTY Ranges !!!\n"
          | r :: _ -> 
(*
              let (x,y) = Int64Swarmer.range_range r in
              lprintf "Received %Ld [%Ld] (%Ld-%Ld) -> %Ld\n"
                !counter_pos to_read
                x y 
                (new_downloaded -- old_downloaded)
*)        
              ()
        );
        
        if new_downloaded = file_size file then
          download_finished file;
        if new_downloaded <> old_downloaded then
          add_file_downloaded file.file_file
            (new_downloaded -- old_downloaded);
        (*
lprintf "READ %Ld\n" (new_downloaded -- old_downloaded);
lprintf "READ: buf_used %d\n" to_read_int;
  *)
        buf_used b to_read_int;
        counter_pos := !counter_pos ++ to_read;
        if !counter_pos = end_pos then begin
            match d.download_ranges with
              [] -> assert false
            | (_,_,r) :: tail ->
                (*
                lprintf "Ready for next chunk (version %s)\nHEADER:%s\n" http
                  (String.escaped header);
                *)
                Int64Swarmer.free_range r;
                d.download_ranges <- tail;
                gconn.gconn_handler <- HttpHeader
                  (client_parse_header c);
          end)
    
  with e ->
      lprintf "Exception %s in client_parse_header\n" (Printexc2.to_string e);
      AnyEndian.dump header;      
      disconnect_client c (Closed_for_exception e);
      raise e
      
and friend_parse_header c gconn sock header =
  try
    if String2.starts_with header gnutella_200_ok then begin
        set_rtimeout sock half_day;
        let lines = Http_client.split_header header in
        match lines with
          [] -> raise Not_found        
        | _ :: headers ->
            let headers = Http_client.cut_headers headers in
            let (agent,_) =  List.assoc "user-agent" headers in
            if String2.starts_with agent "LimeWire" ||
              String2.starts_with agent "Gnucleus" ||
              String2.starts_with agent "BearShare"              
            then
              begin
(* add_peers headers; *)
                write_string sock "GNUTELLA/0.6 200 OK\r\n\r\n";
                if !verbose_msg_clients then begin
                    lprintf "********* READY TO BROWSE FILES *********\n";
                  end;
(*
                gconn.gconn_handler <- Reader
(gnutella_handler parse (client_to_client c))
  *)
              end
            else raise Not_found
      end 
    else raise Not_found
  with e -> 
      lprintf "Exception %s in friend_parse_header\n" 
        (Printexc2.to_string e); 
      disconnect_client c (Closed_for_exception e)

and get_from_client sock (c: client) =
  match c.client_downloads with
    [] -> 
      if !verbose_msg_clients then
        lprintf "No other download to start\n";
      raise Not_found
  | d :: tail ->
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
          List.iter (fun (x,y,r) ->
(*              let (x,y) = Int64Swarmer.range_range r               in *)
              lprintf "%Ld-%Ld " x y) d.download_ranges;
          lprintf "\n  Current blocks: ";
          List.iter (fun b -> Int64Swarmer.print_block b) d.download_blocks;
          lprintf "\n\nFinding Range: \n";
        end;
      let range = 
        try
          let rec iter () =
            match d.download_block with
              None -> 
                if !verbose_swarming then
                  lprintf "No block\n";
                let b = Int64Swarmer.get_block d.download_blocks in
                if !verbose_swarming then begin
                    lprintf "Block Found: "; Int64Swarmer.print_block b;
                  end;
                d.download_block <- Some b;
                iter ()
            | Some b ->
                if !verbose_swarming then begin
                    lprintf "Current Block: "; Int64Swarmer.print_block b;
                  end;
                try
                  let r = Int64Swarmer.find_range b 
                      d.download_chunks (List.map 
                        (fun (_,_,r) -> r)d.download_ranges)
                    (Int64.of_int (256 * 1024)) in
                  let (x,y) = Int64Swarmer.range_range r in 
                  d.download_ranges <- d.download_ranges @ [x,y,r];
                  Int64Swarmer.alloc_range r;
                  Printf.sprintf "%Ld-%Ld" x (y -- Int64.one)
                with Not_found ->
                    if !verbose_swarming then 
                      lprintf "Could not find range in current block\n";
                    d.download_blocks <- List2.removeq b d.download_blocks;
                    d.download_block <- None;
                    iter ()
          in
          iter ()
        with Not_found -> 
            lprintf "Unable to get a block !!";
            check_finished file;
            raise Not_found
      in
      let buf = Buffer.create 100 in
      (match d.download_uri with
          FileByUrl url -> Printf.bprintf buf "GET %s HTTP/1.1\r\n" url
        | FileByIndex (index, name) -> 
            Printf.bprintf buf "GET /get/%d/%s HTTP/1.1\r\n" index
              name);
      (match c.client_host with
          None -> ()
        | Some (ip, port) ->
            Printf.bprintf buf "Host: %s:%d\r\n" 
              (Ip.to_string ip) port);
      Printf.bprintf buf "User-Agent: %s\r\n" user_agent;
      Printf.bprintf buf "Range: bytes=%s\r\n" range;
      Printf.bprintf buf "\r\n";
      let s = Buffer.contents buf in
      if !verbose_msg_clients then
        lprintf "SENDING REQUEST: %s\n" (String.escaped s);
      write_string sock s;
      c.client_requests <- c.client_requests @ [d];
      if !verbose_msg_clients then 
        lprintf "Asking %s For Range %s\n" (Md4.to_string c.client_user.user_uid) 
        range
      
let connect_client c =
  match c.client_sock with
  | Connection _ | CompressedConnection _ -> ()
  | ConnectionWaiting -> ()
  | ConnectionAborted -> c.client_sock <- ConnectionWaiting;
  | NoConnection ->
      add_pending_connection (fun _ ->
          match c.client_sock with
            ConnectionAborted -> c.client_sock <- NoConnection
          | Connection _ | NoConnection -> ()
          | _ ->
          try
          if !verbose_msg_clients then begin
              lprintf "connect_client\n";
            end;
            match c.client_user.user_kind with
              Indirect_location _ -> ()
            | Known_location (ip, port) ->
                if !verbose_msg_clients then begin
                    lprintf "connecting %s:%d\n" (Ip.to_string ip) port; 
                  end;
                let sock = connect "gnutella download" 
                    (Ip.to_inet_addr ip) port
                    (fun sock event ->
                      match event with
                        BASIC_EVENT (RTIMEOUT|LTIMEOUT) ->
                          disconnect_client c Closed_for_timeout
                      | BASIC_EVENT (CLOSED s) ->
                          disconnect_client c s
                      | _ -> ()
                  )
                in
                TcpBufferedSocket.set_read_controler sock download_control;
                TcpBufferedSocket.set_write_controler sock upload_control;
                
                c.client_host <- Some (ip, port);
                set_client_state c Connecting;
                c.client_sock <- Connection sock;
                TcpBufferedSocket.set_closer sock (fun _ s ->
                    disconnect_client c s
                );
                set_rtimeout sock 30.;
                match c.client_downloads with
                  [] -> 
(* Here, we should probably browse the client or reply to
an upload request *)
                    
                    if !verbose_msg_clients then begin
                        lprintf "NOTHING TO DOWNLOAD FROM CLIENT\n";
                      end;
                    if client_type c = NormalClient then                
                      disconnect_client c (Closed_for_error "Nothing to download");
                    set_gnutella_sock sock !verbose_msg_clients
                      (HttpHeader (friend_parse_header c));
                    let s = add_header_fields 
                        "GNUTELLA CONNECT/0.6\r\n" sock 
                        (Printf.sprintf "Remote-IP: %s\r\n\r\n" (Ip.to_string ip))
                    in
(*
        lprintf "SENDING\n";
        AP.dump s;
  *)
                    write_string sock s;
                
                
                | d :: _ ->
                    if !verbose_msg_clients then begin
                        lprintf "READY TO DOWNLOAD FILE\n";
                      end;
                    
                    get_from_client sock c;
                    set_gnutella_sock sock !verbose_msg_clients
                      (HttpHeader (client_parse_header c))
          
          with e ->
              lprintf "Exception %s while connecting to client\n" 
                (Printexc2.to_string e);
              disconnect_client c (Closed_for_exception e)
      );
      c.client_sock <- ConnectionWaiting

(*
  
1022569854.519 24.102.10.39:3600 -> 212.198.235.45:51736 of len 82
ascii [ 
G I V   8 1 : 9 7 4 3 2 1 3 F B 4 8 6 2 3 D 0 F F D F A B B 3 8 0 E C 6 C 0 0 / P o l i c e   V i d e o   -   E v e r y   B r e a t h   Y o u   T a k e . m p g(10)(10)]

"GIV %d:%s/%s\n\n" file.file_number client.client_md4 file.file_name

*)
(*
let find_file file_name file_size = 
  let key = (file_name, file_size) in
  try
    Hashtbl.find files_by_key key  
  with e ->
      lprintf "NO SUCH DOWNLOAD\n";
      raise e
        *)


let current_downloads = ref []

let push_handler cc gconn sock header = 
  if !verbose_msg_clients then begin
      lprintf "PUSH HEADER: [%s]\n" (String.escaped header);
    end;
  try
    let (ip, port) = TcpBufferedSocket.host sock in
    
    if String2.starts_with header "GIV" then begin
        if !verbose_msg_clients then begin    
            lprintf "PARSING GIV HEADER\n"; 
          end;
        let colon_pos = String.index header ':' in
        let slash_pos = String.index header '/' in
        let uid = Md4.of_string (String.sub header (colon_pos+1) 32) in
        let index = int_of_string (String.sub header 4 (colon_pos-4)) in
        if !verbose_msg_clients then begin
            lprintf "PARSED\n";
          end;
        let c = try
            Hashtbl.find clients_by_uid (Indirect_location ("", uid)) 
          with _ ->
              try
                Hashtbl.find clients_by_uid (Known_location (ip,port))
              with _ ->
                  new_client  (Indirect_location ("", uid)) 
        in
        c.client_host <- Some (ip, port);
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
              gconn.gconn_handler <- HttpHeader (client_parse_header c)
            with e ->
                lprintf "Exception %s during client connection\n"
                  (Printexc2.to_string e);
                disconnect_client c (Closed_for_exception e);
                raise End_of_file
      end
    else begin
(*        lprintf "parse_head\n";    *)
        let r = Http_server.parse_head (header ^ "\n") in
        let url = r.Http_server.get_url in
        lprintf "Header parsed: %s ... %s\n"
          (r.Http_server.request) (url.Url.file);
(* "/get/num/filename" *)
        assert (r.Http_server.request = "GET");
        
(* First of all, can we accept this request ???? *)
        
        let rec iter list rem =
          match list with 
            [] ->
              if List.length rem >= !!max_available_slots then
                failwith "All Slots Used";
              sock :: rem
          | s :: tail ->
              if s == sock then list @ rem
              else
              if closed sock then
                iter tail rem
              else
                iter tail (s :: rem)
        in
        current_downloads := iter !current_downloads [];
        let file = url.Url.file in                    
        let sh = 
          if file = "/uri-res/N2R" then
            match url.Url.args with
              [(urn,_)] ->
                lprintf "Found /uri-res/N2R request\n";
                Hashtbl.find shareds_by_uid urn
                
            | _ -> failwith "Cannot parse /uri-res/N2R request"
          else
          let get = String.lowercase (String.sub file 0 5) in
          assert (get = "/get/");
          let pos = String.index_from file 5 '/' in
          let num = String.sub file 5 (pos - 5) in
          let filename = String.sub file (pos+1) (String.length file - pos - 1) in
          lprintf "Download of file %s, filename = %s\n" num filename;
          let num = int_of_string num in
          lprintf "Download of file %d, filename = %s\n" num filename;
          CommonUploads.find_by_num num
        in

(*
BUG:
  * check whether a range is requested, and we need to
  * to send a correct reply
*)
        let no_range = Int64.zero, sh.shared_size in
        let chunk_pos, chunk_end = 
          try
            let range, _, _ = List.assoc "Range" r.Http_server.headers in
            match parse_range range with
              x, None, _ -> x, sh.shared_size
            | x, Some y, Some z ->
                if y = z then (* some vendor bug *)
                  x -- Int64.one, y
                else
                  x, y ++ Int64.one
            | x, Some y, None ->
                x, y ++ Int64.one
          with _ -> no_range
        in
        let uc = {
            uc_sock = sock;
            uc_file = sh;
(* BUG: parse the range header *)
            uc_chunk_pos = chunk_pos;
            uc_chunk_len = chunk_end -- chunk_pos;
            uc_chunk_end = chunk_end;
          } in
        let header_sent = ref false in
        
        let impl = sh.shared_impl in
        impl.impl_shared_requests <- impl.impl_shared_requests + 1;
        shared_must_update_downloaded (as_shared impl);
        
        let rec refill sock =
          lprintf "refill called\n";
          if uc.uc_chunk_pos = uc.uc_chunk_end then begin
              match gconn.gconn_refill with
                []  -> ()
              | [_] -> gconn.gconn_refill <- []
              | _ :: ((refill :: _ ) as tail) -> 
                  gconn.gconn_refill <- tail;
                  refill sock
            end else
          if not !header_sent then begin
(* BUG: send the header *)
              let buf = Buffer.create 100 in
              Printf.bprintf buf "HTTP/1.1 200 OK\r\n";
              Printf.bprintf buf "Server: %s\r\n" user_agent;
              Printf.bprintf buf "Content-type:application/binary\r\n";
              Printf.bprintf buf "Content-length: %Ld\r\n" uc.uc_chunk_len;
              if (chunk_pos, chunk_end) <> no_range then begin
                  Printf.bprintf buf "Accept-Ranges: bytes\r\n";
                  Printf.bprintf buf "Content-range: bytes=%Ld-%Ld/%Ld\r\n"
                    chunk_pos (uc.uc_chunk_end -- Int64.one)
                  sh.shared_size;
                end;
              Buffer.add_string buf "\r\n";
              let s = Buffer.contents buf in
              TcpBufferedSocket.write_string sock s;
              lprintf "Sending Header:\n";
              AnyEndian.dump s;
              lprint_newline ();
              header_sent:=true;
            end;
          let len = remaining_to_write sock in
          let can = maxi (8192 - len) 0 in
          let slen = sh.shared_size in
          let pos = uc.uc_chunk_pos in
          if pos < uc.uc_chunk_end && can > 0 then
            let rlen = 
              let rem = slen --  pos in
              if rem > Int64.of_int can then can else Int64.to_int rem
            in
            let upload_buffer = String.create rlen in
            Unix32.read sh.shared_fd pos upload_buffer 0 rlen;
            TcpBufferedSocket.write sock upload_buffer 0 rlen;
            
            let impl = sh.shared_impl in
            impl.impl_shared_uploaded <- 
              impl.impl_shared_uploaded ++ (Int64.of_int rlen);
            shared_must_update_downloaded (as_shared impl);

            uc.uc_chunk_pos <- uc.uc_chunk_pos ++ (Int64.of_int rlen);
            if remaining_to_write sock = 0 then refill sock
        in
        gconn.gconn_refill <- refill :: gconn.gconn_refill;
        match gconn.gconn_refill with
          refill :: tail ->
(* First refill handler, must be called immediatly *)
            refill sock
        | _ -> (* Already a refill handler, wait for it to finish its job *)
            ()
      end
  with e ->
      lprintf "Exception %s in push_handler: %s\n" (Printexc2.to_string e)
      (String.escaped header);
      (match !cc with Some c -> disconnect_client c (Closed_for_exception e)
        | _ -> ());
      raise e

      (*
let client_parse_header2 c sock header = 
    match !c with
    Some c ->
      client_parse_header c sock header
  | _ -> assert false
      

let client_reader2 c sock nread = 
  match !c with
    None -> assert false
  | Some c ->
      match c.client_file with
        None -> assert false
      | Some d ->
          Download.download_reader d sock nread
            *)

let listen () =
  try
    let sock = TcpServerSocket.create "gnutella client server" 
        Unix.inet_addr_any
        !!client_port
        (fun sock event ->
          match event with
            TcpServerSocket.CONNECTION (s, 
              Unix.ADDR_INET(from_ip, from_port)) ->
              lprintf "CONNECTION RECEIVED FROM %s FOR PUSH\n"
                (Ip.to_string (Ip.of_inet_addr from_ip))
              ; 
              
              lprintf "*********** CONNECTION ***********\n";
              let sock = TcpBufferedSocket.create
                  "gnutella client connection" s 
                  (fun sock event -> 
                    match event with
                      BASIC_EVENT (RTIMEOUT|LTIMEOUT) -> close sock Closed_for_timeout
                    | _ -> ()
                )
              in
              TcpBufferedSocket.set_read_controler sock download_control;
              TcpBufferedSocket.set_write_controler sock upload_control;

              let c = ref None in
              TcpBufferedSocket.set_closer sock (fun _ s ->
                  match !c with
                    Some c ->  disconnect_client c s
                  | None -> ()
              );
              BasicSocket.set_rtimeout (TcpBufferedSocket.sock sock) 30.;
              set_gnutella_sock sock !verbose_msg_clients
                (HttpHeader (push_handler c));
          | _ -> ()
      ) in
    listen_sock := Some sock;
    ()
  with e ->
      lprintf "Exception %s while init gnutella server\n" 
        (Printexc2.to_string e)
      
let push_connection guid index ip port =
  let sh = CommonUploads.find_by_num index in
  let sock = connect "gnutella download" 
      (Ip.to_inet_addr ip) port
      (fun sock event -> 
        match event with
          BASIC_EVENT (RTIMEOUT|LTIMEOUT) -> close sock Closed_for_timeout
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
  BasicSocket.set_rtimeout (TcpBufferedSocket.sock sock) 30.;
  set_gnutella_sock sock !verbose_msg_clients
    (HttpHeader (push_handler c));
  write_string sock 
    (Printf.sprintf "GIV %d:%s/%s\n\n" 
      index (Md4.to_string guid) sh.shared_codedname)
    
