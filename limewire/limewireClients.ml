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
    
let client s =
  match s.source_client with
    None -> 
      Printf.printf "NO SOURCE CLIENT"; print_newline ();
      raise Not_found
  | Some c -> c
      
let disconnect_from_source s =
  try
    let c = client s in
    connection_failed s.source_connection_control;
    let sock = c.source_sock in
    close sock "closed";
    Printf.printf "Disconnected from source"; print_newline ();
    s.source_client <- None
  with _ -> ()

let new_client s sock =
  disconnect_from_source s;
  let c = {
    source_sock = sock;
    source_pos = Int32.zero;
    source_error = false; 
    source_file = None;
    source = Some s;
    } in
  s.source_client <- Some c;
  c

let is_http_ok header = 
  let pos = String.index header '\n' in
  match String2.split (String.sub header 0 pos) ' ' with
    http :: code :: ok :: _ -> 
      let code = int_of_string code in
      code >= 200 && code < 299 && 
      String2.starts_with (String.lowercase http) "http"
  | _ -> false
  
let client_parse_header s file sock header = 
  try
    connection_ok s.source_connection_control;
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
                  let c = client s in
                  if c.source_pos <> start_pos then 
                    failwith (Printf.sprintf "Bad range %s for %s"
                        (Int32.to_string start_pos)
                      (Int32.to_string c.source_pos));
                  ()
      end else begin
        (*
        Printf.printf "BAD HEADER FROM CONNECTED CLIENT:"; print_newline ();
BigEndian.dump header;
        *) 
        disconnect_from_source s
      end
  with e ->
      Printf.printf "Exception %s in client_parse_header" (Printexc.to_string e);
      print_newline ();
      BigEndian.dump header;      
      disconnect_from_source s

let rec remove_download file list =
  match list with
    [] -> []
  | f :: tail ->
      if f == file then tail else f :: (remove_download file tail)
    
let file_complete file =
  let r = file.file_result in
  let f = r.result_file in
(*
  Printf.printf "FILE %s DOWNLOADED" f.file_name;
print_newline ();
  *)
  file_completed (as_file file.file_file);
  current_files := List2.removeq file !current_files;
  old_files =:= (f.file_name, f.file_size) :: !!old_files;
  List.iter (fun s ->
      s.source_downloads <- remove_download file s.source_downloads      
  ) r.result_sources;
  
(* finally move file *)
  let incoming_dir =
    if !!commit_in_subdir <> "" then
      Filename.concat !!DO.incoming_directory !!commit_in_subdir
    else !!DO.incoming_directory
  in
  (try Unix2.safe_mkdir incoming_dir with _ -> ());
  let new_name = 
    Filename.concat incoming_dir f.file_name
  in
(*  Printf.printf "RENAME to %s" new_name; print_newline ();*)
  Unix2.rename file.file_temp  new_name
     
let client_reader s file sock nread = 
  if nread > 0 then
  let c = client s in
  let b = TcpBufferedSocket.buf sock in
  if not c.source_error then begin
        let f = file.file_result.result_file in
        set_rtimeout sock half_day;
      begin
        let fd = try
            Unix32.force_fd file.file_fd 
          with e -> 
              Printf.printf "In Unix32.force_fd"; print_newline ();
              raise e
        in
        let final_pos = Unix32.seek32 file.file_fd c.source_pos Unix.SEEK_SET in
        Unix2.really_write fd b.buf b.pos b.len;
      end;
(*      Printf.printf "DIFF %d/%d" nread b.len; print_newline ();*)
      c.source_pos <- Int32.add c.source_pos (Int32.of_int b.len);
(*
      Printf.printf "NEW SOURCE POS %s" (Int32.to_string c.source_pos);
print_newline ();
  *)
      TcpBufferedSocket.buf_used sock b.len;
      if c.source_pos > file.file_downloaded then begin
            file.file_downloaded <- c.source_pos;
            file_must_update file;
        end;
      if file.file_downloaded = f.file_size then
        file_complete file 
    end else begin
(*      Printf.printf "ERROR REPORTED: [%s]" (String.sub b.buf b.pos b.len);
      print_newline (); *)
      disconnect_from_source s
    end

let get_from_client sock s c file =
  
  let r = file.file_result in
  let f = r.result_file in
  let index = List.assq r s.source_files in
  write_string sock (Printf.sprintf 
      "GET /get/%d/%s HTTP/1.0\r\nUser-Agent: LimeWire 2.4\r\nRange: bytes=%s-\r\n\r\n" index f.file_name (Int32.to_string file.file_downloaded));
  c.source_pos <- file.file_downloaded;
  c.source_file <- Some file;
  set_rtimeout sock 30.;
  c.source_error <- false  

let client_close c =
  match c.source with
    None -> close c.source_sock ""
  | Some s -> disconnect_from_source s
    
let connect_source s =
  try
    let sock = connect "limewire download" 
        (Ip.to_inet_addr s.source_ip) s.source_port 
        (fun sock event ->
          match event with
            BASIC_EVENT RTIMEOUT ->
              disconnect_from_source s
          | BASIC_EVENT (CLOSED _) ->
              disconnect_from_source s
          | _ -> ()
      )
    in
    TcpBufferedSocket.set_read_controler sock download_control;
    TcpBufferedSocket.set_write_controler sock upload_control;
    
    let c = new_client s sock in
    match s.source_downloads with
      [] -> disconnect_from_source s
    | file :: _ ->
        get_from_client sock s c file;
(*        Printf.printf "++++++++ CONNECTING TO CLIENT +++++++"; print_newline (); *)
        
        TcpBufferedSocket.set_closer sock (fun _ s ->
            client_close c
        );
        set_rtimeout sock 30.;
         set_reader sock (handler (client_parse_header s file)
          (client_reader s file));

        
          
  with e ->
      Printf.printf "Exception %s while connecting to client" 
        (Printexc.to_string e);
      print_newline ();
      disconnect_from_source s


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
      
let push_handler c sock header = 
(*  Printf.printf "PUSH HEADER: [%s]" (String.escaped header);
  print_newline (); *)
  try
    if String2.starts_with header "GIV" then begin
(*        Printf.printf "PARSING GIV HEADER"; print_newline (); *)
        let colon_pos = String.index header ':' in
        let slash_pos = String.index header '/' in
        let uid = Md4.of_string (String.sub header (colon_pos+1) 32) in
        let index = int_of_string (String.sub header 4 (colon_pos-4)) in
        let s = Hashtbl.find sources_by_uid uid in
(*        Printf.printf "SOURCE FOUND"; print_newline (); *)
        match s.source_client with
          Some cc -> 
(*            Printf.printf "ALREADY CONNECTED"; print_newline (); *)
            client_close c
        | None ->
            connection_ok s.source_connection_control;
            c.source <- Some s;
            s.source_client <- Some c;
            let r = List2.assoc_inv index s.source_files in
            let f = r.result_file in
            let file = find_file f.file_name f.file_size in
            c.source_file <- Some file;
            get_from_client sock s c file
      end
    else raise Not_found
  with _ ->
      client_close c;
      raise Not_found
  
let client_parse_header2 c sock header = 
    match c.source, c.source_file with
    Some s, Some file ->
      client_parse_header s file sock header
  | _ -> assert false

let client_reader2 c sock nread = 
  match c.source, c.source_file with
    Some s, Some file ->
      client_reader s file sock nread
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

              let c = {
                  source_sock = sock;
                  source_pos = Int32.zero;
                  source_error = false;
                  source = None;
                  source_file = None;
                } in
              TcpBufferedSocket.set_closer sock (fun _ s ->
                  client_close c
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
      
      