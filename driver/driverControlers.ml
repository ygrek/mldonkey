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

open CommonResult
open CommonInteractive
open CommonNetwork
open CommonSearch
open CommonTypes
open CommonGlobals
open GuiTypes
open CommonComplexOptions
open CommonFile
open Options
open BasicSocket
open TcpBufferedSocket
open DriverInteractive
open CommonOptions
  
let rec dollar_escape with_frames s =
  String2.convert false (fun b escaped c ->
      if escaped then
        match c with
        | 'O' -> if with_frames then
              Buffer.add_string b " target=output"; false
        | 'S' -> if with_frames then
              Buffer.add_string b " target=status"; false
        | 'P' -> if with_frames then
              Buffer.add_string b " target=\"_parent\""; false
        | 'G' -> false
        | _ -> 
            try
              Buffer.add_string b (dollar_escape with_frames
                  (CommonNetwork.escape_char c));
              false
              
            with _ ->
                Buffer.add_char b '$'; Buffer.add_char b c; false
      else
      if c = '$' then true else
        (Buffer.add_char b c; false)) s

let eval auth cmd options =
  let buf = options.conn_buf in
  let l = String2.tokens cmd in
  match l with
    [] -> ()
  | cmd :: args ->
      if cmd = "help" || cmd = "?" then begin
          Printf.bprintf  buf "Available commands are:\n";
          List.iter (fun (cmd, _, help) ->
              Printf.bprintf  buf "%s %s\n" cmd help) 
          !CommonNetwork.network_commands
        end else
      if cmd = "q" then
        raise CommonTypes.CommandCloseSocket
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
        DriverCommands.execute_command 
          !CommonNetwork.network_commands options cmd args      
      else
          Printf.bprintf buf "Command not authorized\n Use 'auth <password>' before."

              
(* This function is called every hour to check if we have something to do 
just now *)
        
let calendar_options = {
    conn_buf = Buffer.create 1000;
    conn_output = TEXT;
    conn_sortvd = NotSorted;
    conn_filter = (fun _ -> ());
  }
      
let check_calendar () =
  let time = last_time () in
  let tm = Unix.localtime time in
  List.iter (fun (days, hours, command) ->
      if List.mem tm.Unix.tm_wday days &&
        List.mem tm.Unix.tm_hour hours then begin
          eval (ref true) command calendar_options;
          Printf.printf "Calendar execute: %s\n%s" command
            (Buffer.contents calendar_options.conn_buf);
          print_newline ();
          Buffer.clear calendar_options.conn_buf;          
        end
  ) !!calendar
  
        
(* The telnet client *)
        
let user_reader options auth sock nread  = 
  let b = TcpBufferedSocket.buf sock in
  let end_pos = b.pos + b.len in
  let new_pos = end_pos - nread in
  let rec iter i =
    let end_pos = b.pos + b.len in
    if i < end_pos then
      let c = b.buf.[i] in
      if c = '\n' || c = '\r' || c = '\000' then 
        let len = i - b.pos in
        let cmd = String.sub b.buf b.pos len in
        buf_used sock (len+1);
        let buf = options.conn_buf in
        Buffer.clear buf;
        eval auth cmd options;
        Buffer.add_char buf '\n';
        TcpBufferedSocket.write_string sock (Buffer.contents buf);
        iter b.pos
       else
         iter (i+1)
  in
  try
    iter new_pos
  with
  | CommonTypes.CommandCloseSocket ->
    (try
       shutdown sock "user quit";
     with _ -> ());
  | e ->
    TcpBufferedSocket.write_string sock
       (Printf.sprintf "exception [%s]\n" (Printexc.to_string e))

  
let user_closed sock  msg =
  user_socks := List2.removeq sock !user_socks;
  ()
  
let telnet_handler t event = 
  match event with
    TcpServerSocket.CONNECTION (s, Unix.ADDR_INET (from_ip, from_port)) ->
      let from_ip = Ip.of_inet_addr from_ip in
      if Ip.matches from_ip !!allowed_ips then 
        let sock = TcpBufferedSocket.create_simple 
          "telnet connection"
          s in
        let auth = ref (!!password = "") in
        let options = {
            conn_buf = Buffer.create 1000;
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

(* The Chat client *)

let chat_handler t event = 
  match event with
    TcpServerSocket.CONNECTION (s, Unix.ADDR_INET (from_ip, from_port)) ->
      (
       try
	 prerr_endline "incoming chat connection";
	 let from_ip = Ip.of_inet_addr from_ip in
	 if Ip.matches from_ip !!allowed_ips then 
	   (
	    let chanin = Unix.in_channel_of_descr s in
	    let chanout = Unix.out_channel_of_descr s in
	    let paq = Chat_proto.read_packet_channel chanin in
	    prerr_endline "received a message on chat port";
	    let ret = 
	      match paq with
		((v,id,(host,port)),iddest,pro) ->
		  if v <> CommonChat.version then 
		    None
		  else
		    Some paq
	    in
	    close_out chanout;
	    (match ret with
	      None -> ()
	    | Some ((v,id,(host,port)),iddest,pro) ->
		prerr_endline "received a good message on chat port";
		match pro with
		  Chat_proto.Hello ->
		    CommonChat.send_hello_ok ()
		| Chat_proto.HelloOk -> ()
		| Chat_proto.AddOpen _ -> ()
		| Chat_proto.Byebye -> ()
		| Chat_proto.RoomMessage _ ->
		    (* A VOIR *)
		    ()
		| Chat_proto.Message s ->
		    if iddest = !!CommonOptions.chat_console_id then
		   (* we must eval the string as a command *)
		      (
                       let options = {
                         conn_buf = Buffer.create 1000;
                         conn_output = TEXT;
                         conn_sortvd = NotSorted;
                         conn_filter = (fun _ -> ());
                       } in
                       let buf = options.conn_buf in
		       let auth = ref true in
		       eval auth s options;
		       CommonChat.send_text !!CommonOptions.chat_console_id None 
			 (Buffer.contents buf);
		       Buffer.reset buf
		      )
		    else
		   (* we must forward the message *)
                      (networks_iter (fun r ->
			network_private_message r iddest s)
                      )
	    )
	   )
	 else 
           Unix.close s
     with
       Failure mess ->
	 prerr_endline mess;
	 Unix.close s
    )
  | _ ->
      ()

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
  
let html_open_page buf t r open_body =
  Buffer.clear buf;
  
  http_add_header buf;
  
  Buffer.add_string buf
  "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\" 
  \"http://www.w3.org/TR/html4/frameset.dtd\"><HTML>\n<HEAD>\n";
  Buffer.add_string buf !!html_header;
  if !CommonInteractive.display_vd then begin
      let url = { r.get_url with
          Url.server = Ip.to_string (my_ip t);
(*
          if !!http_bind_addr = any_ip then
              client_ip None
            else
!!http_bind_addr);
  *)
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
  CommonInteractive.display_vd := false;
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
            html_open_page buf t r true;
            Buffer.add_string buf !!web_common_header
        | "/" | "/index.html" -> 
            if !!use_html_frames then begin
                html_open_page buf t r false;
                Printf.bprintf buf "
            <frameset src=\"index\" rows=\"%d,2*\">
               <frameset src=\"index\" cols=\"5*,1*\">
                  <frame name=\"commands\" src=\"/commands.html\">
                  <frame name=\"status\" src=\"/noframe.html\">
               </frameset>
               <frame name=\"output\" src=\"/oneframe.html\">
            </frameset>" !!commands_frame_height             
              end else
              html_open_page buf t r true
        | "/complex_search.html" ->
            html_open_page buf t r true;
            CommonSearch.complex_search buf
        | "/noframe.html"
        | "/oneframe.html" ->
            html_open_page buf t r true
        
        | "/filter" ->
            html_open_page buf t r true;
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
                  let s = search_find num in
                  
                  DriverInteractive.print_search b s
                    { options with conn_filter = !filter };
                  
                  Buffer.add_string buf (html_escaped (Buffer.contents b))
              
              | _ -> 
                  Buffer.add_string buf "Bad filter"
            end
        
        
        | "/results" ->
            html_open_page buf t r true;
            let b = Buffer.create 10000 in
            List.iter (fun (arg, value) ->
                match arg with
                  "d" -> begin
                      try
                        let num = int_of_string value in 
                        let r = result_find num in
                        result_download r [];

                        Printf.bprintf buf "Download of file %d started<br>"
                          num
                      with  e -> 
                          Printf.bprintf buf "Error %s with %s<br>" 
                            (Printexc.to_string e) value;
                          results_iter (fun n  r ->
                              Printf.bprintf buf "IN TABLE: %d   <br>\n" n)
                    end
                | _ -> ()
            ) r.get_url.Url.args;
            Buffer.add_string buf (html_escaped (Buffer.contents b))
            
            
        | "/files" ->
            
            List.iter (fun (arg, value) ->
                match arg with
                  "cancel" -> 
                    let num = int_of_string value in
                    let file = file_find num in
                    file_cancel file
                | "pause" -> 
                    let num = int_of_string value in
                    let file = file_find num in
                    file_pause file
                | "resume" -> 
                    let num = int_of_string value in
                    let file = file_find num in
                    file_resume file
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
            DriverInteractive.display_file_list b options;
            
            html_open_page buf t r true;
            Buffer.add_string buf (html_escaped (Buffer.contents b))

        | "/submit" ->
            begin
              match r.get_url.Url.args with
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
                    let b = options.conn_buf in
                    Buffer.clear b;
                    eval (ref true) cmd options;
                    html_escaped (Buffer.contents b)
                  in
                  html_open_page buf t r true;
                  Printf.bprintf buf  "\n<pre>\n%s\n</pre>\n" s;

              | [ ("custom", query) ] ->
                  html_open_page buf t r true;
                  CommonSearch.custom_query buf query
                  
              | ("custom", query) :: args ->
                  html_open_page buf t r true;
                  send_custom_query buf 
                    (let module G = GuiTypes in
                    { G.search_num = 0;
                      G.search_query = query;
                      G.search_type = RemoteSearch;
                      G.search_max_hits = 10000;
                    })
                     args
  
              | [ "setoption", _ ; "option", name; "value", value ] ->
                  html_open_page buf t r true;
                  Options.set_simple_option downloads_ini name value;
                  Buffer.add_string buf "Option value changed"
                  
              | args -> 
                  List.iter (fun (s,v) ->
                      Printf.printf "[%s]=[%s]" (String.escaped s) (String.escaped v);
                      print_newline ()) args;
                  
                  raise Not_found
end

        | cmd ->
            html_open_page buf t r true;
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
    conn_buf = Buffer.create 10000;
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
