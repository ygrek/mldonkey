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

  
let rec dollar_escape o with_frames s =
  String2.convert false (fun b escaped c ->
      if escaped then
        match c with
        | 'O' -> if with_frames then
              if !!html_mods then Buffer.add_string b "output"
              else Buffer.add_string b " target=\"output\""; 
              false
        | 'S' -> if with_frames then
              if !!html_mods then Buffer.add_string b "fstatus"
              else Buffer.add_string b " target=\"fstatus\"";
              false
        | 'P' -> if with_frames then
              if !!html_mods then Buffer.add_string b "_parent"
              else Buffer.add_string b " target=\"_parent\""; 
              false
        | 'G' -> false
            
        | 'r' ->
            if o.conn_output = ANSI then 
              Buffer.add_string b Terminal.ANSI.ansi_RED;
            false
            
        | 'b' ->
            if o.conn_output = ANSI then 
              Buffer.add_string b Terminal.ANSI.ansi_BLUE;
            false
            
        | 'n' ->
            if o.conn_output = ANSI then 
              Buffer.add_string b Terminal.ANSI.ansi_NORMAL;
            false
            
        | _ -> 
(*
            try
              Buffer.add_string b (dollar_escape with_frames
                  (CommonNetwork.escape_char c));
              false
            
            with _ -> *)
                Buffer.add_char b '$'; Buffer.add_char b c; false
      else
      if c = '$' then true else
        (Buffer.add_char b c; false)) s

let eval auth cmd o =
  let buf = o.conn_buf in
  let cmd =
    if String2.check_prefix cmd "ed2k://" then "dllink " ^ cmd
    else if String2.check_prefix cmd "fha://" then "ovlink " ^ cmd
    else cmd in
  let l = String2.tokens cmd in
  match l with
    [] -> ()
  | cmd :: args ->
      if cmd = "longhelp" || cmd = "??" then begin
          let module M = CommonMessages in
          Buffer.add_string  buf !!M.available_commands_are;
          List.iter (fun (cmd, _, help) ->
              Printf.bprintf  buf "$r%s$n %s\n" cmd help) 
          !CommonNetwork.network_commands
        end else 
      if cmd = "help" || cmd = "?" then begin
          
          let module M = CommonMessages in
          Buffer.add_string  buf
            "Main commands are:

$bServers:$n
          $rvm$n : list connected servers
          $rvma$n : list all servers
          $rc/x <num>$n : connect/disconnect from a server

$bDownloads:$n
          $rvd$n : view current downloads
          $rcancel/pause/resume <num>$n : cancel/pause/resume download <num>

$bSearches:$n
          $rs  <keywords>$n : start a search for keywords <keywords> on the network
          $rvr$n : view results of the last search
          $rd <num>$n : download result number <num>
          $rvs$n : view previous searches
          $rvr <num>$n : view results of search <num>

$bGeneral:$n
          $rsave$n : save configuration files
          $rkill$n : kill mldonkey properly
          $rq$n : quit this interface

Use '$r";
           if o.conn_output = HTML then
             Buffer.add_string buf "\\<a href=\\\"/submit?q=longhelp\\\"\\>";
           Buffer.add_string buf "longhelp";
           if o.conn_output = HTML then
             Buffer.add_string buf "\\</a\\>";
           Buffer.add_string buf "$n' or '$r";
           if o.conn_output = HTML then
             Buffer.add_string buf "\\<a href=\\\"/submit?q=longhelp\\\"\\>";
           Buffer.add_string buf "??";
           if o.conn_output = HTML then
             Buffer.add_string buf "\\</a\\>";
           Buffer.add_string buf "$n' for all commands.
Use '$rhelp command$n' or '$r? command$n' for help on a command.
            ";
          List.iter (fun arg ->
              List.iter (fun (cmd, _, help) ->
                  if cmd = arg then    
                    Printf.bprintf  buf "%s %s\n" cmd help) 
              !CommonNetwork.network_commands)
          args

        end else
      if cmd = "q" then
        raise CommonTypes.CommandCloseSocket
      else
      if cmd = "auth" then
        let user, pass =
          match args with
            [] -> failwith "Usage: auth <user> <password>"
          | [s1] -> "admin", s1
          | user :: pass :: _ -> user, pass
        in
        if valid_password user pass then begin
            auth := true;
            o.conn_user <- find_ui_user user;
            let module M = CommonMessages in
            Buffer.add_string buf !!M.full_access
          end else 
        let module M = CommonMessages in
        Buffer.add_string buf !!M.bad_login
      else
      if !auth then
        DriverCommands.execute_command 
          !CommonNetwork.network_commands o cmd args      
      else
      let module M = CommonMessages in
      Buffer.add_string buf !!M.command_not_authorized

              
(* This function is called every hour to check if we have something to do 
just now *)
        
let calendar_options = {
    conn_buf = Buffer.create 1000;
    conn_output = TEXT;
    conn_sortvd = NotSorted;
    conn_filter = (fun _ -> ());
    conn_user = default_user;
    conn_width = 80; conn_height = 0;
  }
      
let check_calendar () =
  let time = last_time () in
  let tm = Unix.localtime (date_of_int time) in
  List.iter (fun (days, hours, command) ->
      if (List.mem tm.Unix.tm_wday days || days = [])  &&
        (List.mem tm.Unix.tm_hour hours || hours = []) then begin
          eval (ref true) command calendar_options;
          lprintf "Calendar execute: %s\n%s" command
            (Buffer.contents calendar_options.conn_buf);
          lprint_newline ();
          Buffer.clear calendar_options.conn_buf;          
        end
  ) !!calendar
  

(*************************************************************

                  The Telnet Server
  
**************************************************************)  

let before_telnet_output o sock = 
  if o.conn_output = ANSI && o.conn_height <> 0 then
    write_string sock (Printf.sprintf 
        "%s%s\n%s%s" 
        (Terminal.gotoxy 0 (o.conn_height-3))
      Terminal.ANSI.ansi_CLREOL
      Terminal.ANSI.ansi_CLREOL
      (Terminal.gotoxy 0 (o.conn_height-3)))
  
let after_telnet_output o sock = 
  if o.conn_output = ANSI && o.conn_height <> 0 then
    write_string sock (Printf.sprintf "\n\n%s"
        (Terminal.gotoxy 0 (o.conn_height - 2)));
  if o.conn_output = ANSI then
    write_string sock (Printf.sprintf "%sMLdonkey command-line:%s\n> "
      Terminal.ANSI.ansi_REVERSE
      Terminal.ANSI.ansi_NORMAL)
  
(*  
let user_reader o telnet sock nread  = 
  let b = TcpBufferedSocket.buf sock in
  let end_pos = b.pos + b.len in
  let new_pos = end_pos - nread in
  let rec iter i =
    let end_pos = b.pos + b.len in
    for i = b.pos to b.pos + b.len - 1 do
      let c = int_of_char b.buf.[i] in
      if c <> 13 && c <> 10 && (c < 32 || c > 127) then
        lprintf "term[%d] = %d\n" i c;
    done;
    
    if i < end_pos then
      let c = b.buf.[i] in
      let c = int_of_char c in
      if c = 13 || c = 10 || c = 0 then
        let len = i - b.pos  in
        let cmd = String.sub b.buf b.pos len in
        buf_used sock (len+1);
        if cmd <> "" then begin
            before_telnet_output o sock;
            let buf = o.conn_buf in
            Buffer.clear buf;
            if o.conn_output = ANSI then Printf.bprintf buf "> $b%s$n\n" cmd;
            eval telnet.telnet_auth cmd o;
            Buffer.add_char buf '\n';
            if o.conn_output = ANSI then Buffer.add_string buf "$n";
            TcpBufferedSocket.write_string sock 
              (dollar_escape o false (Buffer.contents buf));
            after_telnet_output o sock;
          end;
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
      before_telnet_output o sock;
      TcpBufferedSocket.write_string sock
        (Printf.sprintf "exception [%s]\n" (Printexc2.to_string e));
      after_telnet_output o sock
        *)

type telnet_state =
  EMPTY
| STRING
| IAC
| WILL
| WONT
| DO
| DONT
| NAWS
| SB

type telnet_conn = {
    telnet_buffer : Buffer.t;
    mutable telnet_iac : bool;
    mutable telnet_wait : int;
    telnet_auth : bool ref;
  }


let iac_will_naws = "\255\253\031"  
  
let user_reader o telnet sock nread  = 
  let b = TcpBufferedSocket.buf sock in
  let end_pos = b.pos + b.len in
  let new_pos = end_pos - nread in
  let rec iter () =
    if b.len > 0 then
      let c = b.buf.[b.pos] in
      buf_used sock 1;
(*      lprintf "char %d\n" (int_of_char c); *)
      if c = '\255' && not telnet.telnet_iac then begin
          telnet.telnet_iac <- true;
          iter ()
        end else
      if c <> '\255' && telnet.telnet_iac then begin
          telnet.telnet_iac <- false;
          (match c with
              '\250' | '\251' -> 
                Buffer.add_char telnet.telnet_buffer c;                
                telnet.telnet_wait <- 1
            | _ -> 
                Buffer.clear telnet.telnet_buffer
          );
          iter ()
        end else
      
      let i = int_of_char c in
      telnet.telnet_iac <- false;
      let is_normal_char = i > 31 && i < 127 in
      
      if telnet.telnet_wait = 1 then begin
          Buffer.add_char telnet.telnet_buffer c;
          let cmd = Buffer.contents telnet.telnet_buffer in
          telnet.telnet_wait <- 0;
          let len = String.length cmd in
          if len = 2 then
            match cmd with
              "\251\031" -> 
                Buffer.clear telnet.telnet_buffer
            | "\250\031" -> 
                telnet.telnet_wait <- 4
            | _ -> 
                (*
                lprintf "telnet server: Unknown control sequence %s\n"
                  (String.escaped cmd);                *)
                Buffer.clear telnet.telnet_buffer
          else
          let s = String.sub cmd 0 2 in
          Buffer.clear telnet.telnet_buffer;
          match s with
          | "\250\031" -> 
              let dx = BigEndian.get_int16 cmd 2 in
              let dy = BigEndian.get_int16 cmd 4 in
              o.conn_width <- dx;
              o.conn_height <- dy;
(*              lprintf "SIZE RECEIVED %d x %d\n" dx dy; *)
          | _ -> 
              (*
              lprintf "telnet server: Unknown control sequence %s\n"
              (String.escaped cmd); *)
              ()
        end else 
      if telnet.telnet_wait > 1 then begin
          Buffer.add_char telnet.telnet_buffer c;
          telnet.telnet_wait <- telnet.telnet_wait - 1;
        end else
      if is_normal_char then 
        Buffer.add_char telnet.telnet_buffer c
      else begin
(* evaluate the command *)
          let cmd = Buffer.contents telnet.telnet_buffer in
          Buffer.clear telnet.telnet_buffer;
          if cmd <> "" then begin
              before_telnet_output o sock;
              let buf = o.conn_buf in
              Buffer.clear buf;
              if o.conn_output = ANSI then Printf.bprintf buf "> $b%s$n\n" cmd;
              eval telnet.telnet_auth cmd o;
              Buffer.add_char buf '\n';
              if o.conn_output = ANSI then Buffer.add_string buf "$n";
              TcpBufferedSocket.write_string sock 
                (dollar_escape o false (Buffer.contents buf));
              after_telnet_output o sock;
            end;
          if i = 255 then telnet.telnet_wait <- 2;
        end;
      iter ()
  in
  try
    iter ()
  with
  | CommonTypes.CommandCloseSocket ->
    (try
       shutdown sock "user quit";
     with _ -> ());
  | e -> 
      before_telnet_output o sock;
      TcpBufferedSocket.write_string sock
        (Printf.sprintf "exception [%s]\n" (Printexc2.to_string e));
      after_telnet_output o sock

  
let user_closed sock  msg =
  user_socks := List2.removeq sock !user_socks;
  ()

(* Here, we clearly need a good html parser to remove tags, and to translate
special characters. Avoid them in the meantime. *)
let text_of_html html = 
  String2.convert false (fun buf state c -> 
    if state then
      c <> '>' 
    else 
      if c = '<' then true else begin
	Buffer.add_char buf c;
	false
      end
) html
  
let telnet_handler t event = 
  match event with
    TcpServerSocket.CONNECTION (s, Unix.ADDR_INET (from_ip, from_port)) ->
      let from_ip = Ip.of_inet_addr from_ip in
      if Ip.matches from_ip !!allowed_ips then 
        let sock = TcpBufferedSocket.create_simple 
          "telnet connection"
          s in
        let telnet = {
            telnet_auth = ref (empty_password "admin");
            telnet_iac = false;
            telnet_wait = 0;
            telnet_buffer = Buffer.create 100;
          } in
        let o = {
            conn_buf = Buffer.create 1000;
            conn_output = (if !!term_ansi then ANSI else TEXT);
            conn_sortvd = NotSorted;
            conn_filter = (fun _ -> ());
            conn_user = default_user;
            conn_width = 80; 
            conn_height = 0;
          } in
        TcpBufferedSocket.set_max_write_buffer sock !!interface_buffer;
        TcpBufferedSocket.set_reader sock (user_reader o telnet);
        TcpBufferedSocket.set_closer sock user_closed;
        user_socks := sock :: !user_socks;

        TcpBufferedSocket.write_string sock iac_will_naws;

        before_telnet_output o sock;
        TcpBufferedSocket.write_string sock (text_of_html !!motd_html);

        TcpBufferedSocket.write_string sock (dollar_escape o false
            "\n$bWelcome on mldonkey command-line$n\n\nUse $r?$n for help\n\n");
        
        after_telnet_output o sock
      else 
        Unix.close s

  | _ -> ()

(*************************************************************

                  The Chat Server
  
**************************************************************)  

let chat_handler t event = 
  match event with
    TcpServerSocket.CONNECTION (s, Unix.ADDR_INET (from_ip, from_port)) ->
      (
        try
          let o = {
              conn_buf = Buffer.create 1000;
              conn_output = TEXT;
              conn_sortvd = NotSorted;
              conn_filter = (fun _ -> ());
              conn_user = default_user;
              conn_width = 80; conn_height = 0;
            } in
          
	 let from_ip = Ip.of_inet_addr from_ip in
	 if Ip.matches from_ip !!allowed_ips then 
	   (
	    let chanin = Unix.in_channel_of_descr s in
	    let chanout = Unix.out_channel_of_descr s in
	    let paq = Chat_proto.read_packet_channel chanin in
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
                            let buf = o.conn_buf in
                            Buffer.clear buf;
		       let auth = ref true in
		       eval auth s o;
		       CommonChat.send_text !!CommonOptions.chat_console_id None 
			 (dollar_escape o false (Buffer.contents buf));
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
	 lprintf "%s\n" mess;
	 Unix.close s
    )
  | _ ->
      ()

(*************************************************************

                  The HTTP Server
  
**************************************************************)  

let buf = Buffer.create 1000
      
let html_page = ref true  

open Http_server

let add_simple_commands buf =
  Buffer.add_string buf (if !!html_mods then 
       (match !!html_mods_style with 
	   1 -> !!CommonMessages.web_common_header_mods1
	 | _ -> !!CommonMessages.web_common_header_mods0)
	else
      !!CommonMessages.web_common_header_old)

let http_add_gen_header buf =
  Buffer.add_string  buf "HTTP/1.0 200 OK\r\n";
  Buffer.add_string  buf "Server: MLdonkey\r\n";
  Buffer.add_string  buf "Connection: close\r\n"

let http_add_html_header buf = 
  http_add_gen_header buf;
  Buffer.add_string  buf "Pragma: no-cache\r\n";
  Buffer.add_string  buf "Content-Type: text/html; charset=iso-8859-1\r\n";
  Buffer.add_string  buf "\r\n"

let http_add_css_header buf = 
  http_add_gen_header buf;
  Buffer.add_string  buf "Content-Type: text/css; charset=iso-8859-1\r\n";
  Buffer.add_string  buf "\r\n"

let http_add_js_header buf =
  http_add_gen_header buf;
  Buffer.add_string  buf "Content-Type: text/javascript; charset=iso-8859-1\r\n";
  Buffer.add_string  buf "\r\n"

let any_ip = Ip.of_inet_addr Unix.inet_addr_any
  
let html_open_page buf t r open_body =
  Buffer.clear buf;
  html_page := true;
  http_add_html_header buf;
  
  if not !!html_mods then 
    (Buffer.add_string buf
      "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\" 
        \"http://www.w3.org/TR/html4/frameset.dtd\">\n<HTML>\n<HEAD>\n";)
    else Buffer.add_string buf "<html>\n<head>\n";
  
  if !CommonInteractive.display_vd then begin
      Buffer.add_string buf 
        (if !!html_mods then 
        (match !!html_mods_style with 
		  1 -> !!CommonMessages.download_html_header_mods1
		| _ -> !!CommonMessages.download_html_header_mods0)
		else !!CommonMessages.download_html_header_old);
      Printf.bprintf buf "<meta http-equiv=Refresh
          content=\"%d\">" !!vd_reload_delay;
    end else 
    Buffer.add_string buf !!(if !!html_mods then 
   		(match !!html_mods_style with 
		1 -> CommonMessages.html_header_mods1 
	  |	_ -> CommonMessages.html_header_mods0)
	else CommonMessages.html_header_old);
  
  Buffer.add_string buf "</head>\n";
  if open_body then Buffer.add_string buf "<body>\n";    
  if not !!use_html_frames then add_simple_commands buf;
  ()
  
let html_close_page buf =
  Buffer.add_string buf "</body>\n";  
  Buffer.add_string buf "</html>\n";
  ()
  
  
let http_handler o t r =
  CommonInteractive.display_vd := false;
  
  
  let user = if r.options.login = "" then "admin" else r.options.login in
  if not (valid_password user r.options.passwd) then begin
      Buffer.clear buf;  
      need_auth buf !!http_realm
    end
  else
    begin
      let user = find_ui_user user  in
      let o = match user.ui_http_conn with
        Some oo -> oo.conn_buf <- o.conn_buf; oo
      | None -> let oo = { o with conn_user = user } in
           user.ui_http_conn <- Some oo; oo
      in
      try
        match r.get_url.Url.file with
        | "/commands.html" ->
            html_open_page buf t r true;
            Buffer.add_string buf !!(if !!html_mods then
				(match !!html_mods_style with
	            1 -> CommonMessages.web_common_header_mods1
              | _ -> CommonMessages.web_common_header_mods0)
              else
                CommonMessages.web_common_header_old)
        | "/" | "/index.html" -> 
            if !!use_html_frames then begin
                html_open_page buf t r false;
                if !!html_mods then
                  Printf.bprintf buf "
			 <frameset src=\"index\" rows=\"%d,25,*\">
                  <frame name=\"commands\" NORESIZE SCROLLING=\"NO\" NOSHADE marginwidth=0 marginheight=0 BORDER=0 FRAMESPACING=0 FRAMEBORDER=0 src=\"/commands.html\">
                  <frame name=\"fstatus\" NORESIZE SCROLLING=\"NO\" NOSHADE marginwidth=0 marginheight=0 BORDER=0 FRAMESPACING=0 FRAMEBORDER=0 src=\"/noframe.html\">
               <frame name=\"output\" NORESIZE NOSHADE marginwidth=0 marginheight=0 BORDER=0 FRAMESPACING=0 FRAMEBORDER=0 src=\"/oneframe.html\">
            </frameset>" !!commands_frame_height
                else
                  Printf.bprintf buf "
            <frameset src=\"index\" rows=\"%d,2*\">
               <frameset src=\"index\" cols=\"5*,1*\">
                  <frame name=\"commands\" src=\"/commands.html\">
                  <frame name=\"fstatus\" src=\"/noframe.html\">
               </frameset>
               <frame name=\"output\" src=\"/oneframe.html\">
            </frameset>" !!commands_frame_height; 
              end else
              html_open_page buf t r true
        | "/complex_search.html" ->
            html_open_page buf t r true;
            CommonSearch.complex_search buf
        | "/noframe.html" -> 
            html_open_page buf t r true
        
        | "/oneframe.html" ->
            html_open_page buf t r true;
            Buffer.add_string buf !!motd_html
        
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
                          let mega5 = Int64.of_int (5 * 1024 * 1024) in
                          let mega20 = Int64.of_int (20 * 1024 * 1024) in
                          let mega400 = Int64.of_int (400 * 1024 * 1024) in
                          let min, max = match value with
                              "0to5" -> Int64.zero, mega5
                            | "5to20" -> mega5, mega20
                            | "20to400" -> mega20, mega400
                            | "400+" -> mega400, Int64.max_int
                            | _ -> Int64.zero, Int64.max_int
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
                    { o with conn_filter = !filter };
                  
                  Buffer.add_string buf (html_escaped 
                    (Buffer.contents b))
              
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
                        result_download r [] false;
                        
                        let module M = CommonMessages in
                        Gettext.buftext buf M.download_started num
                      with  e -> 
                          Printf.bprintf buf "Error %s with %s<br>" 
                            (Printexc2.to_string e) value;
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
                      | "Percent" -> o.conn_sortvd <- ByPercent
                      | "%" -> o.conn_sortvd <- ByPercent
                      | "File" -> o.conn_sortvd <- ByName
                      | "Downloaded" -> o.conn_sortvd <- ByDone
                      | "DLed" -> o.conn_sortvd <- ByDone
                      | "Size" -> o.conn_sortvd <- BySize
                      | "Rate" -> o.conn_sortvd <- ByRate
                      | "ETA" -> o.conn_sortvd <- ByETA
                      | "Priority" -> o.conn_sortvd <- ByPriority
                      | "Age" -> o.conn_sortvd <- ByAge
                      | "Last" -> o.conn_sortvd <- ByLast
                      | "Srcs" -> o.conn_sortvd <- BySources
                      | "A" -> o.conn_sortvd <- ByASources
                      | "N" -> o.conn_sortvd <- ByNet
                      | _ -> ()
                    end
                | _ -> 
                    lprintf "/files: Unbound argument \"%s=%s\"" arg value;
                    lprint_newline ();
            ) r.get_url.Url.args;
            let b = Buffer.create 10000 in
            
            DriverInteractive.display_file_list b o;
            html_open_page buf t r true;
            Buffer.add_string buf (html_escaped (Buffer.contents b))

        | "/submit" ->
            begin
              match r.get_url.Url.args with
              | ("q", cmd) :: other_args ->
                  List.iter (fun arg ->
                      match arg with
                      | "sortby", "size" -> o.conn_sortvd <- BySize
                      | "sortby", "name" -> o.conn_sortvd <- ByName
                      | "sortby", "rate" -> o.conn_sortvd <- ByRate
                      | "sortby", "done" -> o.conn_sortvd <- ByDone
                      | "sortby", "percent" -> o.conn_sortvd <- ByPercent
                      | "sortby", "priority" -> o.conn_sortvd <- ByPriority
                      | _ -> ()
                  ) other_args;
                  let s = 
                    let b = o.conn_buf in
                    Buffer.clear b;
                    eval (ref true) cmd o;
                    html_escaped (Buffer.contents b)
                  in
                  html_open_page buf t r true;
                  Printf.bprintf buf  "\n<pre>\n%s\n</pre>\n" s;

              | [ ("custom", query) ] ->
                  html_open_page buf t r true;
                  CommonSearch.custom_query buf query
                  
              | ("custom", query) :: args ->
                  html_open_page buf t r true;
                  send_custom_query o.conn_user buf 
                    (let module G = GuiTypes in
                    { G.search_num = 0;
                      G.search_query = query;
                      G.search_type = RemoteSearch;
                      G.search_max_hits = 10000;
                    })
                     args
  
              | [ "setoption", _ ; "option", name; "value", value ] ->
                  html_open_page buf t r true;
                  CommonInteractive.set_fully_qualified_options name value;
                  Buffer.add_string buf "Option value changed"

              | args -> 
                  List.iter (fun (s,v) ->
                      lprintf "[%s]=[%s]" (String.escaped s) (String.escaped v);
                      lprint_newline ()) args;
                  
                  raise Not_found
end

        | "/h.css" ->
            Buffer.clear buf;
            html_page := false; 
            http_add_css_header buf;
            Buffer.add_string buf (if !!html_mods then
			   !CommonMessages.html_css_mods
               else !!CommonMessages.html_css_old)

        | "/dh.css" ->          
            Buffer.clear buf;
            html_page := false; 
            http_add_css_header buf;
            Buffer.add_string buf (if !!html_mods then 
			   !CommonMessages.download_html_css_mods
               else !!CommonMessages.download_html_css_old)

        | "/i.js" ->
            Buffer.clear buf;
            html_page := false; 
            http_add_js_header buf;
            Buffer.add_string buf !!(if !!html_mods then
				(match !!html_mods_style with 
				 1 -> CommonMessages.html_js_mods1
			   | _ -> CommonMessages.html_js_mods0)
               else
                CommonMessages.html_js_old)

        | "/di.js" ->          
            Buffer.clear buf;
            html_page := false; 
            http_add_js_header buf;
            Buffer.add_string buf !!(if !!html_mods then 
                (match !!html_mods_style with 
				1 -> CommonMessages.download_html_js_mods1
			  | _ -> CommonMessages.download_html_js_mods0)
               else
                CommonMessages.download_html_js_old)

        | cmd ->
            html_open_page buf t r true;
            Printf.bprintf buf "No page named %s" cmd
      with e ->
          Printf.bprintf buf "\nException %s\n" (Printexc2.to_string e);
    end;
  
  if !html_page then
  html_close_page buf;
  let s = Buffer.contents buf in
  let s = dollar_escape o !!use_html_frames s in
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
    conn_user = default_user;
    conn_width = 80; conn_height = 0;
  }      
  
let create_http_handler () = 
  let config = {
      bind_addr = Ip.to_inet_addr !!http_bind_addr ;
      port = !!http_port;
      requests = [];
      addrs = !!allowed_ips;
      base_ref = "";
      default = http_handler http_options;      
    } in
  option_hook allowed_ips (fun _ -> config.addrs <- !!allowed_ips);
  let sock = find_port "http server" !!http_bind_addr http_port
      (Http_server.handler config) in
  config.port <- !!http_port
  
