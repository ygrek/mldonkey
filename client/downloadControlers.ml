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

open DownloadSearch
open Options
open Mftp
open Mftp_comm
open DownloadServers
open BasicSocket
open TcpBufferedSocket
open DownloadOneFile
open DownloadFiles
open DownloadComplexOptions
open DownloadTypes
open DownloadOptions
open DownloadGlobals
open DownloadClient
open Gui_types
open DownloadInteractive

  
let dollar_escape with_frames s =
  String2.convert false (fun b escaped c ->
      if escaped then
        match c with
        | 'O' -> if with_frames then
              Buffer.add_string b " target=output"; false
        | 'S' -> if with_frames then
              Buffer.add_string b " target=status"; false
        | 'P' -> if with_frames then
              Buffer.add_string b " target=\"_parent\""; false
        | _ -> Buffer.add_char b '$'; Buffer.add_char b c; false
      else
      if c = '$' then true else
        (Buffer.add_char b c; false)) s

let eval auth buf cmd options =
  let l = String2.tokens cmd in
  match l with
    [] -> ()
  | cmd :: args ->
      if cmd = "help" || cmd = "?" then begin
          Printf.bprintf  buf "Available commands are:\n";
          List.iter (fun (cmd, _, help) ->
              Printf.bprintf  buf "%s %s\n" cmd help) 
          DownloadCommands.commands
        end else
      if cmd = "q" then
        raise CommandCloseSocket
      else
      if cmd = "auth" then
        let arg_password =
          match args with
            [] -> ""
          | s1 :: _ -> s1
        in
        if !!password = arg_password then begin
            auth := true;
            Printf.bprintf buf "Full access enabled"
          end else
          Printf.bprintf buf "Bad login/password"
      else
      if !auth then
        DownloadCommands.execute_command 
          DownloadCommands.commands buf options cmd args      
      else
          Printf.bprintf buf "Command not authorized\n Use 'auth <password>' before."

(* The telnet client *)
        
let buf = Buffer.create 1000

let user_reader options auth sock nread  = 
  let b = TcpBufferedSocket.buf sock in
  let end_pos = b.pos + b.len in
  let new_pos = end_pos - nread in
  for i = new_pos to end_pos - 1 do
    let c = b.buf.[i] in
    if c = '\n' || c = '\r' || c = '\000' then 
      let len = i - b.pos in
      let cmd = String.sub b.buf b.pos len in
      buf_used sock (len+1);
      try
        Buffer.clear buf;
        eval auth buf cmd options;
        Buffer.add_char buf '\n';
        TcpBufferedSocket.write_string sock (Buffer.contents buf)
      with
        CommandCloseSocket ->
          (try
              shutdown sock "user quit";
          with _ -> ());
      | e ->
          TcpBufferedSocket.write_string sock
            (Printf.sprintf "exception [%s]\n" (Printexc.to_string e));
          
  done
  
let user_closed sock  msg =
  user_socks := List2.removeq sock !user_socks;
  ()
  
let telnet_handler t event = 
  match event with
    TcpServerSocket.CONNECTION (s, Unix.ADDR_INET (from_ip, from_port)) ->
      let from_ip = Ip.of_inet_addr from_ip in
      if Ip.matches from_ip !!allowed_ips then 
        let sock = TcpBufferedSocket.create_simple s in
        let auth = ref (!!password = "") in
        let options = {
            conn_output = TEXT;
            conn_sortvd = NotSorted;
            conn_filter = (fun _ -> ());
          } in
        TcpBufferedSocket.set_reader sock (user_reader options auth);
        TcpBufferedSocket.set_closer sock user_closed;
        user_socks := sock :: !user_socks;
        TcpBufferedSocket.write_string sock "\nWelcome on mldonkey command-line\n";
        TcpBufferedSocket.write_string sock "\nUse ? for help\n\n";
      else 
        Unix.close s

  | _ -> ()

(* The HTTP client *)

let buf = Buffer.create 1000
      
open Http_server

let add_simple_commands buf =
  Buffer.add_string buf !!web_common_header  

let http_add_header buf = 
  Buffer.add_string  buf "HTTP/1.0 200 OK\r\n";
  Buffer.add_string  buf "Pragma: no-cache\r\n";
  Buffer.add_string  buf "Server: MLdonkey\r\n";
  Buffer.add_string  buf "Connection: close\r\n";
  Buffer.add_string  buf "Content-Type: text/html; charset=iso-8859-1\r\n";
  Buffer.add_string  buf "\r\n"

let any_ip = Ip.of_inet_addr Unix.inet_addr_any
  
let html_open_page buf r open_body =
  Buffer.clear buf;
  
  http_add_header buf;
  
  Buffer.add_string buf
  "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\" 
  \"http://www.w3.org/TR/html4/frameset.dtd\"><HTML>\n<HEAD>\n";
  Buffer.add_string buf !!html_header;
  if !display_vd then begin
      let url = { r.get_url with
          Url.server = Ip.to_string (
            if !!http_bind_addr = any_ip then
              !!client_ip
            else
              !!http_bind_addr);
          Url.port = !!http_port;
          Url.proto = "http";
          } in
      Printf.bprintf buf "<meta http-equiv=Refresh
      content=\"%d; URL=%s\">" !!vd_reload_delay
      (Url.to_string true url);
    end;
  Buffer.add_string buf "</HEAD>\n";
  if open_body then Buffer.add_string buf "<BODY>\n";    
  if not !!use_html_frames then add_simple_commands buf;
  ()
  
let html_close_page buf =
  Buffer.add_string buf "</BODY>\n";  
  Buffer.add_string buf "</HTML>\n";
  ()

let http_handler options t r =
  display_vd := false;
  if (!!http_password <> "" || !!http_login <> "") &&
    (r.options.passwd <> !!http_password || r.options.login <> !!http_login)
  then begin
      Buffer.clear buf;  
      need_auth buf "MLdonkey"
      end
  else
    begin
      try
        match r.get_url.Url.file with
        | "/commands.html" ->
            html_open_page buf r true;
            Buffer.add_string buf !!web_common_header
        | "/" | "/index.html" -> 
            if !!use_html_frames then begin
                html_open_page buf r false;
                Printf.bprintf buf "
            <frameset src=\"index\" rows=\"%d,2*\">
               <frameset src=\"index\" cols=\"5*,1*\">
                  <frame name=\"commands\" src=\"/commands.html\">
                  <frame name=\"status\" src=\"/noframe.html\">
               </frameset>
               <frame name=\"output\" src=\"/oneframe.html\">
            </frameset>" !!commands_frame_height             
              end else
              html_open_page buf r true
        | "/complex_search.html" ->
            html_open_page buf r true;
            complex_search buf
        | "/noframe.html"
        | "/oneframe.html" ->
            html_open_page buf r true
        
        | "/filter" ->
            html_open_page buf r true;
            let b = Buffer.create 10000 in 
            let filter = ref (fun _ -> ()) in
            begin              
              match r.get_url.Url.args with
                ("num", num) :: args ->
                  List.iter (fun (arg, value) ->
                      match arg with
                      | "media" -> 
                          let old_filter = !filter in
                          filter := (fun r ->
                              if r.result_type = value then raise Not_found;
                              old_filter r
                          )
                      | "format" -> 
                          let old_filter = !filter in
                          filter := (fun r ->
                              if r.result_format = value then raise Not_found;
                              old_filter r
                          )
                      | "size" -> 
                          let old_filter = !filter in
                          let mega5 = Int32.of_int (5 * 1024 * 1024) in
                          let mega20 = Int32.of_int (20 * 1024 * 1024) in
                          let mega400 = Int32.of_int (400 * 1024 * 1024) in
                          let min, max = match value with
                              "0to5" -> Int32.zero, mega5
                            | "5to20" -> mega5, mega20
                            | "20to400" -> mega20, mega400
                            | "400+" -> mega400, Int32.max_int
                            | _ -> Int32.zero, Int32.max_int
                          in
                          filter := (fun r ->
                              if r.result_size >= min && 
                                r.result_size <= max then
                                raise Not_found;
                              old_filter r
                          )
                      | _ -> ()
                  )  args;
                  
                  let num = int_of_string num in
                  List.iter (fun s ->
                      if s.search_num = num then
                        print_search b s { options with conn_filter = !filter }
                  ) !searches;
                  
                  
                  Buffer.add_string buf (html_escaped (Buffer.contents b))
              | _ -> 
                  Buffer.add_string buf "Bad filter"
            end
            
        | "/results" ->
            html_open_page buf r true;
            let b = Buffer.create 10000 in
            List.iter (fun (arg, value) ->
                match arg with
                  "d" -> begin
                    try
                        let num = int_of_string value in 
                        let doc = Intmap.find num !last_search in
                        let r = Store.get DownloadIndexer.store doc in

                        let (size, md4, names) = 
                          r.result_size, r.result_md4, r.result_names
                        in
                        query_download names size md4 None None None;
                        Printf.bprintf buf "Download of file %s started\<br\>"
                          (Md4.to_string md4)
                        with  e -> 
                        Printf.bprintf buf "Error %s with %s\<br\>" 
                          (Printexc.to_string e) value
                    end
                | _ -> ()
            ) r.get_url.Url.args;
            Buffer.add_string buf (html_escaped (Buffer.contents b))
            
            
        | "/files" ->
            
            List.iter (fun (arg, value) ->
                match arg with
                  "cancel" -> 
                    let num = int_of_string value in
                    List.iter (fun file ->
                        if file.file_num = num then remove_file file.file_md4
                    ) !!files
                | "pause" -> 
                    let num = int_of_string value in
                    List.iter (fun file ->
                        if file.file_num = num then begin
                            file.file_state <- FilePaused;
                            file.file_changed <- FileInfoChange;
                            !file_change_hook file
                          end
                    ) !!files
                | "resume" -> 
                    let num = int_of_string value in
                    List.iter (fun file ->
                        if file.file_num = num then begin
                            file.file_state <- FileDownloading;
                            reconnect_all file;
                            file.file_changed <- FileInfoChange;
                            !file_change_hook file
                          end
                    ) !!files
                | "sortby" -> 
                    begin
                      match value with
                      | "Percent" -> options.conn_sortvd <- ByPercent
                      | "File" -> options.conn_sortvd <- ByName
                      | "Downloaded" -> options.conn_sortvd <- ByDone
                      | "Size" -> options.conn_sortvd <- BySize
                      | "Rate" -> options.conn_sortvd <- ByRate
                      | _ -> ()
                    end
                | _ -> 
                    Printf.printf "FILE: Unbound argument %s/%s" arg value;
                    print_newline ();
            ) r.get_url.Url.args;
            let b = Buffer.create 10000 in
            Buffer.add_string b (display_file_list b options);
            
            html_open_page buf r true;
            Buffer.add_string buf (html_escaped (Buffer.contents b))
            
        | "/submit" ->
            begin
              match r.get_url.Url.args with
              | [ "q", "download"; "md4", md4_string; "size", size_string ] ->
                  html_open_page buf r true;
(*                  if !!use_html_frames then html_open_page buf r; *)
                  query_download [] (Int32.of_string size_string)
                  (Md4.of_string md4_string) None None None;
                  Printf.bprintf buf  "\n<pre>\nDownload started\n</pre>\n";
                  
              | ("q", cmd) :: other_args ->
                  List.iter (fun arg ->
                      match arg with
                      | "sortby", "size" -> options.conn_sortvd <- BySize
                      | "sortby", "name" -> options.conn_sortvd <- ByName
                      | "sortby", "rate" -> options.conn_sortvd <- ByRate
                      | "sortby", "done" -> options.conn_sortvd <- ByDone
                      | "sortby", "percent" -> options.conn_sortvd <- ByPercent
                      | _ -> ()
                  ) other_args;
                  let s = 
                    let b = Buffer.create 10000 in
                    eval (ref true) b cmd options;
                    html_escaped (Buffer.contents b)
                  in
                  html_open_page buf r true;
                  Printf.bprintf buf  "\n<pre>\n%s\n</pre>\n" s;

              | [ ("custom", query) ] ->
                  html_open_page buf r true;
                  custom_query buf query
                  
              | ("custom", query) :: args ->
                  html_open_page buf r true;
                  send_custom_query buf query args
  
              | [ "setoption", _ ; "option", name; "value", value ] ->
                  html_open_page buf r true;
                  Options.set_simple_option downloads_ini name value;
                  Buffer.add_string buf "Option value changed"
                  
              | args -> 
                  List.iter (fun (s,v) ->
                      Printf.printf "[%s]=[%s]" (String.escaped s) (String.escaped v);
                      print_newline ()) args;
                  
                  raise Not_found
            end
        | cmd ->
            html_open_page buf r true;
            Printf.bprintf buf "No page named %s" cmd
      with e ->
          Printf.bprintf buf "\nException %s\n" (Printexc.to_string e);
    end;
  
  html_close_page buf;
  let s = Buffer.contents buf in
  let s = dollar_escape !!use_html_frames s in
  let len = String.length s in
(*  TcpBufferedSocket.set_monitored t; *)
  TcpBufferedSocket.set_max_write_buffer t (len + 100);
  
  TcpBufferedSocket.write t s 0 len;
  TcpBufferedSocket.close_after_write t
        
let http_options = { 
    conn_output = HTML;
    conn_sortvd = NotSorted;
    conn_filter = (fun _ -> ());
  }      
  
let create_http_handler () = 
  create {
    bind_addr = Ip.to_inet_addr !!http_bind_addr;
    port = !!http_port;
    requests = [];
    addrs = !!allowed_ips;
    base_ref = "";
    default = http_handler http_options;
  }
