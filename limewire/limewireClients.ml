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
      
let disconnect_from_client c =
  try
    Printf.printf "Disconnected from source"; print_newline ();    
    connection_failed c.client_connection_control;
    match c.client_sock with
      None -> ()
    | Some sock -> close sock "closed";
  with _ -> ()

let is_http_ok header = 
  let pos = String.index header '\n' in
  match String2.split (String.sub header 0 pos) ' ' with
    http :: code :: ok :: _ -> 
      let code = int_of_string code in
      code >= 200 && code < 299 && 
      String2.starts_with (String.lowercase http) "http"
  | _ -> false
  
let client_parse_header c sock header = 
  try
    connection_ok c.client_connection_control;
(*
  *)
    if is_http_ok header then
      begin
        (*
Printf.printf "GOOD HEADER FROM CONNECTED CLIENT"; print_newline ();
  *)
              set_rtimeout sock 120.;
(*              Printf.printf "SPLIT HEADER..."; print_newline (); *)
              let lines = Http_client.split_header header in
(*              Printf.printf "REMOVE HEADLINE..."; print_newline (); *)
              match lines with
                [] -> raise Not_found        
              | _ :: headers ->
(*                  Printf.printf "CUT HEADERS..."; print_newline (); *)
                  let headers = Http_client.cut_headers headers in
(*                  Printf.printf "START POS..."; print_newline (); *)
                  let start_pos = 
                    try
                      let range = List.assoc "content-range" headers in
                      try
                        let npos = (String.index range 'b')+6 in
                        let dash_pos = try String.index range '-' with _ -> -10 in
                        let slash_pos = try String.index range '/' with _ -> -20 in
                        let star_pos = try String.index range '*' with _ -> -30 in
                        if star_pos = slash_pos-1 then
                          Int32.zero (* "bytes */X" *)
                        else
                        let x = Int32.of_string (
                            String.sub range npos (dash_pos - npos) )
                        in
                        if slash_pos = star_pos - 1 then 
                          x (* "bytes x-y/*" *)
                        else
                        let len = String.length range in
                        let y = Int32.of_string (
                            String.sub range (dash_pos+1) (slash_pos - dash_pos - 1))
                        in
                        let z = Int32.of_string (
                            String.sub range (slash_pos+1) (len - slash_pos -1) )
                        in
                        if y = z then Int32.sub x Int32.one else x
                      with 
                      | e ->
                          Printf.printf "Exception %s for range [%s]" 
                            (Printexc.to_string e) range;
                          print_newline ();
                          raise e
                    with Not_found -> Int32.zero
                  in                  
                  if c.client_pos <> start_pos then 
                    failwith (Printf.sprintf "Bad range %s for %s"
                        (Int32.to_string start_pos)
                      (Int32.to_string c.client_pos));
                  ()
      end else begin
        (*
        Printf.printf "BAD HEADER FROM CONNECTED CLIENT:"; print_newline ();
BigEndian.dump header;
        *) 
        disconnect_from_client c
      end
  with e ->
      Printf.printf "Exception %s in client_parse_header" (Printexc.to_string e);
      print_newline ();
      BigEndian.dump header;      
      disconnect_from_client c
    
let file_complete file =
(*
  Printf.printf "FILE %s DOWNLOADED" f.file_name;
print_newline ();
  *)
  file_completed (as_file file.file_file);
  current_files := List2.removeq file !current_files;
  old_files =:= (file.file_name, file.file_size) :: !!old_files;
  List.iter (fun c ->
      c.client_downloads <- List.remove_assoc file c.client_downloads      
  ) file.file_clients;
  
(* finally move file *)
  let incoming_dir =
    if !!commit_in_subdir <> "" then
      Filename.concat !!DO.incoming_directory !!commit_in_subdir
    else !!DO.incoming_directory
  in
  (try Unix2.safe_mkdir incoming_dir with _ -> ());
  let new_name = 
    Filename.concat incoming_dir file.file_name
  in
(*  Printf.printf "RENAME to %s" new_name; print_newline ();*)
  Unix2.rename file.file_temp  new_name;
  file.file_temp <- new_name

let client_to_client s p sock =
  match p.pkt_payload with
  | _ -> ()
     
let client_reader c sock nread = 
  if nread > 0 then
    let b = TcpBufferedSocket.buf sock in
    if not c.client_error then begin
        match c.client_file with
          None -> disconnect_from_client c
        | Some file ->
            set_rtimeout sock half_day;
        begin
              let fd = try
                  Unix32.force_fd file.file_fd 
                with e -> 
                    Printf.printf "In Unix32.force_fd"; print_newline ();
                    raise e
              in
              let final_pos = Unix32.seek32 file.file_fd c.client_pos Unix.SEEK_SET in
              Unix2.really_write fd b.buf b.pos b.len;
        end;
(*      Printf.printf "DIFF %d/%d" nread b.len; print_newline ();*)
        c.client_pos <- Int32.add c.client_pos (Int32.of_int b.len);
(*
      Printf.printf "NEW SOURCE POS %s" (Int32.to_string c.client_pos);
print_newline ();
  *)
        TcpBufferedSocket.buf_used sock b.len;
        if c.client_pos > file.file_downloaded then begin
            file.file_downloaded <- c.client_pos;
            file_must_update file;
          end;
        if file.file_downloaded = file.file_size then
          file_complete file 
      end else begin
(*      Printf.printf "ERROR REPORTED: [%s]" (String.sub b.buf b.pos b.len);
      print_newline (); *)
        disconnect_from_client c
      end
        
let client_parse_header c sock header =
  try
    if String2.starts_with header gnutella_200_ok ||
      String2.starts_with header gnutella_503_shielded then begin
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
                Printf.printf "********* READY TO BROWSE FILES *********";
                print_newline ();
              end
            else raise Not_found
      end 
    else raise Not_found
  with _ -> disconnect_from_client c
      
let get_from_client sock (c: client) (file : file) =
  let index = List.assoc file c.client_downloads in
  write_string sock (Printf.sprintf 
      "GET /get/%d/%s HTTP/1.0\r\nUser-Agent: LimeWire 2.4\r\nRange: bytes=%s-\r\n\r\n" index file.file_name (Int32.to_string file.file_downloaded));
  c.client_pos <- file.file_downloaded;
  c.client_file <- Some file;
  set_rtimeout sock 30.;
  c.client_error <- false  
    
let connect_client c =
  try
    match c.client_user.user_kind with
      Indirect_location _ -> ()
    | Known_location (ip, port) ->
        let sock = connect "limewire download" 
            (Ip.to_inet_addr ip) port
            (fun sock event ->
              match event with
                BASIC_EVENT RTIMEOUT ->
                  disconnect_from_client c
              | BASIC_EVENT (CLOSED _) ->
                  disconnect_from_client c
              | _ -> ()
          )
        in
        verify_ip sock;
        TcpBufferedSocket.set_read_controler sock download_control;
        TcpBufferedSocket.set_write_controler sock upload_control;
        
        c.client_sock <- Some sock;
        TcpBufferedSocket.set_closer sock (fun _ _ ->
            disconnect_from_client c
        );
        set_rtimeout sock 30.;
        match c.client_downloads with
          [] -> 
(* Here, we should probably browse the client or reply to
an upload request *)
            if client_type c = NormalClient then                
              disconnect_from_client c;
            set_reader sock (handler (client_parse_header c)
              (gnutella_handler parse (client_to_client c))
            );
            let s = Printf.sprintf 
                "GNUTELLA CONNECT/0.6\r\nUser-Agent: LimeWire 2.4.4\r\nX-My-Address: %s:%d\r\nX-Ultrapeer: False\r\nX-Query-Routing: 0.1\r\nRemote-IP: %s\r\n\r\n"
                (Ip.to_string !!DO.client_ip) !!client_port
                (Ip.to_string ip)
            in
(*
        Printf.printf "SENDING"; print_newline ();
        AP.dump s;
  *)
            write_string sock s;
            
            
        | (file, _) :: _ ->
            get_from_client sock c file;
            set_reader sock (handler (client_parse_header c) (client_reader c));

        
          
  with e ->
      Printf.printf "Exception %s while connecting to client" 
        (Printexc.to_string e);
      print_newline ();
      disconnect_from_client c


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
      Printf.printf "NO SUCH DOWNLOAD"; print_newline ();
      raise e
      
let push_handler cc sock header = 
(*  Printf.printf "PUSH HEADER: [%s]" (String.escaped header);
  print_newline (); *)
  try
    if String2.starts_with header "GIV" then begin
(*        Printf.printf "PARSING GIV HEADER"; print_newline (); *)
        let colon_pos = String.index header ':' in
        let slash_pos = String.index header '/' in
        let uid = Md4.of_string (String.sub header (colon_pos+1) 32) in
        let index = int_of_string (String.sub header 4 (colon_pos-4)) in
        let c = new_client uid (Indirect_location ("", uid)) in
        match c.client_sock with
          Some _ -> 
(*            Printf.printf "ALREADY CONNECTED"; print_newline (); *)
            close sock "already connected"
        | None ->
            cc := Some c;
            c.client_sock <- Some sock;
            connection_ok c.client_connection_control;
            try
              let file = List2.assoc_inv index c.client_downloads in
              get_from_client sock c file
            with e ->
                Printf.printf "Exception %s during client connection"
                  (Printexc.to_string e);
                print_newline ();
                disconnect_from_client c
      end
    else raise Not_found
  with _ ->
      match !cc with 
        None -> raise Not_found
      | Some c ->
          disconnect_from_client c;
          raise Not_found
  
let client_parse_header2 c sock header = 
    match !c with
    Some c ->
      client_parse_header c sock header
  | _ -> assert false
      

let client_reader2 c sock nread = 
  match !c with
    Some c ->
      client_reader c sock nread
  | _ -> assert false
      
let listen () =
  try
    let sock = TcpServerSocket.create "limewire client server" 
        Unix.inet_addr_any
        !!client_port
        (fun sock event ->
          match event with
            TcpServerSocket.CONNECTION (s, 
              Unix.ADDR_INET(from_ip, from_port)) ->
(*              Printf.printf "CONNECTION RECEIVED FROM %s FOR PUSH"
                (Ip.to_string (Ip.of_inet_addr from_ip))
              ; 
              print_newline (); *)
              
              
              let sock = TcpBufferedSocket.create
                  "limewire client connection" s (fun _ _ -> ()) in
              TcpBufferedSocket.set_read_controler sock download_control;
              TcpBufferedSocket.set_write_controler sock upload_control;

              let c = ref None in
              TcpBufferedSocket.set_closer sock (fun _ s ->
                  match !c with
                    Some c ->  disconnect_from_client c
                  | None -> ()
              );
              BasicSocket.set_rtimeout (TcpBufferedSocket.sock sock) 30.;
              TcpBufferedSocket.set_reader sock (
                handlers [push_handler c; 
                  client_parse_header2 c]
                  (client_reader2 c));
          | _ -> ()
      ) in
    ()
  with e ->
      Printf.printf "Exception %s while init limewire server" 
        (Printexc.to_string e);
      print_newline ();
      
      