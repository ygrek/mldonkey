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
  
open LimewireTypes
open LimewireOptions
open LimewireGlobals
open LimewireComplexOptions

open LimewireProtocol

let http_ok = "HTTP 200 OK"
let http11_ok = "HTTP/1.1 200 OK"

  
module Download = CommonDownloads.Make(struct 
      
      type c = client
      type f = file
      
      let file file = as_file file.file_file
      let client client = as_client client.client_client
      let subdir_option = commit_in_subdir
            
      let client_disconnected d =
        let c = d.download_client in
        if !verbose_msg_clients then begin
            lprintf "Disconnected from source"; lprint_newline ();    
          end;
        connection_failed c.client_connection_control;
        set_client_disconnected c;
        c.client_sock <- None;
        c.client_file <- None
      
      let download_finished d = 
        let file = d.download_file in
        current_files := List2.removeq file !current_files;
        old_files =:= (file.file_name, file_size file) :: !!old_files;
        List.iter (fun c ->
            c.client_downloads <- List.remove_assoc file c.client_downloads
        ) file.file_clients
        
    end)

  
let disconnect_client c =
  match c.client_sock with
    None -> ()
  | Some sock -> 
      try
        if !verbose_msg_clients then begin
            lprintf "Disconnected from source"; lprint_newline ();    
          end;
        connection_failed c.client_connection_control;
        set_client_disconnected c;
        close sock "closed";
        c.client_sock <- None
      with _ -> ()
          
let is_http_ok header = 
  let pos = String.index header '\n' in
  match String2.split (String.sub header 0 pos) ' ' with
    http :: code :: ok :: _ -> 
      let code = int_of_string code in
      code >= 200 && code < 299 && 
      String2.starts_with (String.lowercase http) "http"
  | _ -> false
  
let client_parse_header c handler sock header = 
  if !verbose_msg_clients then begin
      lprintf "CLIENT PARSE HEADER"; lprint_newline ();
    end;
  try
    match c.client_file with 
      None -> close sock "no download !"; raise Exit
    | Some d ->
        connection_ok c.client_connection_control;
        set_client_state c Connected_initiating;    
        if !verbose_msg_clients then begin
            lprintf "HEADER FROM CLIENT:"; lprint_newline ();
            LittleEndian.dump_ascii header; 
          end;
        if is_http_ok header then
          begin
            
            if !verbose_msg_clients then begin
                lprintf "GOOD HEADER FROM CONNECTED CLIENT"; lprint_newline ();
              end;
            
            set_rtimeout sock 120.;
(*              lprintf "SPLIT HEADER..."; lprint_newline (); *)
            let lines = Http_client.split_header header in
(*              lprintf "REMOVE HEADLINE..."; lprint_newline (); *)
            match lines with
              [] -> raise Not_found        
            | _ :: headers ->
(*                  lprintf "CUT HEADERS..."; lprint_newline (); *)
                let headers = Http_client.cut_headers headers in
(*                  lprintf "START POS..."; lprint_newline (); *)
                let start_pos = 
                  try
                    let range = List.assoc "content-range" headers in
                    try
                      let npos = (String.index range 'b')+6 in
                      let dash_pos = try String.index range '-' with _ -> -10 in
                      let slash_pos = try String.index range '/' with _ -> -20 in
                      let star_pos = try String.index range '*' with _ -> -30 in
                      if star_pos = slash_pos-1 then
                        Int64.zero (* "bytes */X" *)
                      else
                      let x = Int64.of_string (
                          String.sub range npos (dash_pos - npos) )
                      in
                      if slash_pos = star_pos - 1 then 
                        x (* "bytes x-y/*" *)
                      else
                      let len = String.length range in
                      let y = Int64.of_string (
                          String.sub range (dash_pos+1) (slash_pos - dash_pos - 1))
                      in
                      let z = Int64.of_string (
                          String.sub range (slash_pos+1) (len - slash_pos -1) )
                      in
                      if y = z then Int64.sub x Int64.one else x
                    with 
                    | e ->
                        lprintf "Exception %s for range [%s]" 
                          (Printexc2.to_string e) range;
                        lprint_newline ();
                        raise e
                  with Not_found -> Int64.zero
                in                  
                if d.CommonDownloads.download_pos <> start_pos then  begin
                    lprintf "Asked %s Bad range %s for %s"
                      (Md4.to_string c.client_user.user_uid)
                    (Int64.to_string start_pos)
                    (Int64.to_string d.CommonDownloads.download_pos);
                    lprint_newline ();
                    raise Exit
                  end;
                set_client_state c (Connected 0);
                handler := Reader (fun h sock nread ->
                    Download.download_reader d sock nread)
          end else begin
            if !verbose_msg_clients then begin
                lprintf "BAD HEADER FROM CONNECTED CLIENT:"; lprint_newline ();
                LittleEndian.dump header;
              end;        
            disconnect_client c
          end
  with e ->
      lprintf "Exception %s in client_parse_header" (Printexc2.to_string e);
      lprint_newline ();
      LittleEndian.dump header;      
      disconnect_client c;
      raise e

let client_to_client s p sock =
  match p.pkt_payload with
  | _ -> ()

let friend_parse_header c handler sock header =
  try
    if String2.starts_with header gnutella_200_ok then begin
        set_rtimeout sock half_day;
        let lines = Http_client.split_header header in
        match lines with
          [] -> raise Not_found        
        | _ :: headers ->
            let headers = Http_client.cut_headers headers in
            let agent =  List.assoc "user-agent" headers in
            if String2.starts_with agent "LimeWire" ||
              String2.starts_with agent "Gnucleus" ||
              String2.starts_with agent "BearShare"              
            then
              begin
                (* add_peers headers; *)
                write_string sock "GNUTELLA/0.6 200 OK\r\n\r\n";
                if !verbose_msg_clients then begin
                    lprintf "********* READY TO BROWSE FILES *********";
                    lprint_newline ();
                  end;
                handler := Reader (gnutella_handler parse (client_to_client c))
              end
            else raise Not_found
      end 
    else raise Not_found
  with e -> 
      lprintf "Exception %s in friend_parse_header" 
        (Printexc2.to_string e); lprint_newline ();
      disconnect_client c
      
let get_from_client sock (c: client) (file : file) =
  if !verbose_msg_clients then begin
      lprintf "FINDING ON CLIENT"; lprint_newline ();
    end;
  let index = List.assoc file c.client_downloads in
  if !verbose_msg_clients then begin
      lprintf "FILE FOUND, ASKING"; lprint_newline ();
      end;

  let new_pos = file_downloaded file in
  write_string sock (add_simplified_header_fields
      (Printf.sprintf "GET /get/%d/%s HTTP/1.0\r\n"
        index file.file_name) 
    (Printf.sprintf "Range: bytes=%Ld-\r\n\r\n" new_pos));
    let d = Download.new_download sock 
    c
    file
    (mini
        (Int64.to_int (Int64.sub (file_size file) (file_downloaded file)))
      1000)
  in
  c.client_file <- Some d;
  lprintf "Asking %s For Range %Ld" (Md4.to_string c.client_user.user_uid) new_pos; lprint_newline ();
  d
  
let connect_client c =
  match c.client_sock with
  | Some sock -> ()
  |    None ->
      if !verbose_msg_clients then begin
          lprintf "connect_client"; lprint_newline ();
        end;
      try
        match c.client_user.user_kind with
          Indirect_location _ -> ()
        | Known_location (ip, port) ->
            if !verbose_msg_clients then begin
                lprintf "connecting %s:%d" (Ip.to_string ip) port; 
                lprint_newline ();
              end;
            let sock = connect "limewire download" 
                (Ip.to_inet_addr ip) port
                (fun sock event ->
                  match event with
                    BASIC_EVENT (RTIMEOUT|LTIMEOUT) ->
                      disconnect_client c
                  | BASIC_EVENT (CLOSED _) ->
                      disconnect_client c
                  | _ -> ()
              )
            in
            match c.client_downloads with
              [] -> 
(* Here, we should probably browse the client or reply to
an upload request *)
                TcpBufferedSocket.set_read_controler sock download_control;
                TcpBufferedSocket.set_write_controler sock upload_control;
                
                set_client_state c Connecting;
                c.client_sock <- Some sock;
                TcpBufferedSocket.set_closer sock (fun _ _ ->
                    disconnect_client c
                );
                set_rtimeout sock 30.;
                if !verbose_msg_clients then begin
                    lprintf "NOTHING TO DOWNLOAD FROM CLIENT"; lprint_newline ();
                  end;
                if client_type c = NormalClient then                
                  disconnect_client c;
                set_reader sock (handlers !verbose_msg_clients
                    (HttpHeader (friend_parse_header c)));
                let s = add_header_fields 
                    "GNUTELLA CONNECT/0.6\r\n" sock 
                    (Printf.sprintf "Remote-IP: %s\r\n\r\n" (Ip.to_string ip))
                in
(*
        lprintf "SENDING"; lprint_newline ();
        AP.dump s;
  *)
                write_string sock s;
            
            
            | (file, _) :: _ ->
                if !verbose_msg_clients then begin
                    lprintf "READY TO DOWNLOAD FILE"; lprint_newline ();
                  end;
                let d = get_from_client sock c file in
                set_reader sock (handlers !verbose_msg_clients
                    (HttpHeader (client_parse_header c)))                
          
  with e ->
      lprintf "Exception %s while connecting to client" 
        (Printexc2.to_string e);
      lprint_newline ();
      disconnect_client c


(*
  
1022569854.519 24.102.10.39:3600 -> 212.198.235.45:51736 of len 82
ascii [ 
G I V   8 1 : 9 7 4 3 2 1 3 F B 4 8 6 2 3 D 0 F F D F A B B 3 8 0 E C 6 C 0 0 / P o l i c e   V i d e o   -   E v e r y   B r e a t h   Y o u   T a k e . m p g(10)(10)]

"GIV %d:%s/%s\n\n" file.file_number client.client_md4 file.file_name

*)

let find_file file_name file_size = 
  let key = (file_name, file_size) in
  try
    Hashtbl.find files_by_key key  
  with e ->
      lprintf "NO SUCH DOWNLOAD"; lprint_newline ();
      raise e
      
let push_handler cc handler sock header = 
  if !verbose_msg_clients then begin
      lprintf "PUSH HEADER: [%s]" (String.escaped header);
      lprint_newline (); 
    end;
  try
    if String2.starts_with header "GIV" then begin
        if !verbose_msg_clients then begin    
            lprintf "PARSING GIV HEADER"; lprint_newline (); 
          end;
        let colon_pos = String.index header ':' in
        let slash_pos = String.index header '/' in
        let uid = Md4.of_string (String.sub header (colon_pos+1) 32) in
        let index = int_of_string (String.sub header 4 (colon_pos-4)) in
        if !verbose_msg_clients then begin
            lprintf "PARSED"; lprint_newline ();
          end;
        let c = new_client uid (Indirect_location ("", uid)) in
        match c.client_sock with
          Some _ -> 
            if !verbose_msg_clients then begin
                lprintf "ALREADY CONNECTED"; lprint_newline (); 
              end;
            close sock "already connected";
            raise End_of_file
        | None ->
            if !verbose_msg_clients then begin
                lprintf "NEW CONNECTION"; lprint_newline ();
              end;
            cc := Some c;
            c.client_sock <- Some sock;
            connection_ok c.client_connection_control;
            try
              if !verbose_msg_clients then begin
                  lprintf "FINDING FILE %d" index; lprint_newline ();
                end;
              let file = List2.assoc_inv index c.client_downloads in
              if !verbose_msg_clients then begin
                  lprintf "FILE FOUND"; lprint_newline ();
                end;
              let d = get_from_client sock c file in
              handler := HttpHeader (client_parse_header c)
            with e ->
                lprintf "Exception %s during client connection"
                  (Printexc2.to_string e);
                lprint_newline ();
                disconnect_client c;
                raise End_of_file
      end
    else begin
        lprintf "parse_head\n";
        let r = Http_server.parse_head (header ^ "\n") in
        lprintf "Header parsed: %s ... %s\n"
          (r.Http_server.request) (r.Http_server.get_url.Url.file);
        
(* OK, we have the GET request from a client, we need:
   * to parse the url and find the file
   * to check whether a range is requested
   * to send the file     
*)
        raise Not_found
      end
  with e ->
      lprintf "Exception %s in push_handler\n" (Printexc2.to_string e);
      (match !cc with Some c -> disconnect_client c | _ -> ());
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
    let sock = TcpServerSocket.create "limewire client server" 
        Unix.inet_addr_any
        !!client_port
        (fun sock event ->
          match event with
            TcpServerSocket.CONNECTION (s, 
              Unix.ADDR_INET(from_ip, from_port)) ->
              lprintf "CONNECTION RECEIVED FROM %s FOR PUSH"
                (Ip.to_string (Ip.of_inet_addr from_ip))
              ; 
              lprint_newline (); 
              
              
              let sock = TcpBufferedSocket.create
                  "limewire client connection" s (fun _ _ -> ()) in
              TcpBufferedSocket.set_read_controler sock download_control;
              TcpBufferedSocket.set_write_controler sock upload_control;

              let c = ref None in
              TcpBufferedSocket.set_closer sock (fun _ s ->
                  match !c with
                    Some c ->  disconnect_client c
                  | None -> ()
              );
              BasicSocket.set_rtimeout (TcpBufferedSocket.sock sock) 30.;
              TcpBufferedSocket.set_reader sock (
                handlers !verbose_msg_clients (HttpHeader (push_handler c)));
          | _ -> ()
      ) in
    listen_sock := Some sock;
    ()
  with e ->
      lprintf "Exception %s while init limewire server" 
        (Printexc2.to_string e);
      lprint_newline ();
      
      