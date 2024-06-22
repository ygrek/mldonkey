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
open Printf2
open CommonResult
open CommonInteractive
open CommonNetwork
open CommonSearch
open CommonTypes
open CommonGlobals
open CommonShared
open GuiTypes
open CommonComplexOptions
open CommonFile
open Options
open BasicSocket
open TcpBufferedSocket
open DriverGraphics
open DriverInteractive
open CommonOptions
open CommonUserDb

let log_prefix = "[dCon]"

let lprintf_nl fmt =
  lprintf_nl2 log_prefix fmt

let lprintf_n fmt =
  lprintf2 log_prefix fmt

let dollar_escape o with_frames s =
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

        | 'g' ->
            if o.conn_output = ANSI then
              Buffer.add_string b Terminal.ANSI.ansi_GREEN;
            false

        | 'c' ->
            if o.conn_output = ANSI then
              Buffer.add_string b Terminal.ANSI.ansi_CYAN;
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
    if String2.check_prefix cmd "ed2k://" ||
       String2.check_prefix cmd "magnet:" ||
       String2.check_prefix cmd "ftp://" ||
       String2.check_prefix cmd "http://" then "dllink " ^ cmd
    else if String2.check_prefix cmd "fha://" then "ovlink " ^ cmd
    else cmd in
  let l = String2.tokens cmd in
  match l with
  | [] -> ()
  | "longhelp"::subs | "??"::subs ->
      let filter cmd = List.for_all (String2.contains cmd) subs in
      let module M = CommonMessages in
      if o.conn_output = HTML then begin
          Buffer.add_string buf "\\<div class=\\\"cs\\\"\\>";
          html_mods_table_header buf "helpTable" "results" [];
          Buffer.add_string buf "\\<tr\\>";
          html_mods_td buf [
            ("", "srh", M.available_commands_are);
            ("", "srh", "");
            ("", "srh", ""); ];
          Buffer.add_string buf "\\</tr\\>";
          html_mods_cntr_init ();
          let show (cmd, _, _, help) =
              let ncmd = ref cmd in (* why? *)
              let nhelp = ref help in
              Printf.bprintf buf "\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr ());
              html_mods_td buf [
                ("", "sr", "\\<a href=\\\"submit?q=" ^ !ncmd ^
                  "\\\"\\>" ^ !ncmd ^ "\\</a\\>");
                ("", "srw", Str.global_replace (Str.regexp "\n") "\\<br\\>" !nhelp);
                ("", "sr", "\\<a href=\\\"http://mldonkey.sourceforge.net/" ^ (String2.upp_initial !ncmd) ^
                  "\\\"\\>wiki\\</a\\>"); ];
              Printf.bprintf buf "\\</tr\\>\n";
          in
          List.iter show 
            (List.sort (fun (c1,_, _,_) (c2,_, _,_) -> compare c1 c2)
              (List.filter (fun (c,_,_,_) -> filter c) !CommonNetwork.network_commands));
          Printf.bprintf buf "\\</table\\>\\</div\\>";
          html_mods_table_header buf "helpTable" "results" [];
          Printf.bprintf buf "\\<tr class=\\\"dl-1\\\"\\>";
          html_mods_td buf [
            ("", "sr", "< > : required parameter");
            ("", "sr", "[< >] : optional parameter");
            ("", "sr", "< 1 | 2 > : alternative parameter"); ];
          Printf.bprintf buf "\\</table\\>\\</div\\>\\</div\\>"
        end else
        begin
          Buffer.add_string  buf M.available_commands_are;
          let list = Hashtbl2.to_list2 commands_by_kind in
          let list = List.sort (fun (s1,_) (s2,_) -> compare s1 s2) list in
          List.iter (fun (s,list) ->
              match List.sort (fun (s1,_) (s2,_) -> compare s1 s2) (List.filter (fun (s,_) -> filter s) !list) with
              | [] -> ()
              | list ->
                Printf.bprintf buf "\n   $b%s$n:\n" s;
                List.iter (fun (cmd, help) -> Printf.bprintf buf "$r%s$n %s\n" cmd help) list
          ) list;
        end

    | ["help"] | ["?"] | ["man"] ->
          let module M = CommonMessages in
           if o.conn_output = HTML then
             begin
               Buffer.add_string buf "\\<div class=\\\"cs\\\"\\>";
               html_mods_table_header buf "helpTable" "results" [];
               Buffer.add_string buf "\\<tr\\>";
               html_mods_td buf [
                 ("", "srh", M.main_commands_are);
                 ("", "srh", ""); ];
               Buffer.add_string buf "\\</tr\\>\\<tr class=\\\"dl-1\\\"\\>";
               html_mods_td buf [
                 ("", "sr", "$bServers:$n");
                 ("", "sr", ""); ];
               Buffer.add_string buf "\\</tr\\>\\<tr class=\\\"dl-2\\\"\\>";
               html_mods_td buf [
                 ("", "sr", "$r\\<a href=\\\"submit?q=vm\\\"\\>" ^
                   "vm\\</a\\>$n");
                 ("", "sr", "list connected servers"); ];
               Buffer.add_string buf "\\</tr\\>\\<tr class=\\\"dl-2\\\"\\>";
               html_mods_td buf [
                 ("", "sr", "$r\\<a href=\\\"submit?q=vma\\\"\\>" ^
                   "vma\\</a\\>$n");
                 ("", "sr", "list all servers"); ];
               Buffer.add_string buf "\\</tr\\>\\<tr class=\\\"dl-2\\\"\\>";
               html_mods_td buf [
                 ("", "sr", "$rc/x <num>$n");
                 ("", "sr", "connect/disconnect from a server"); ];
               Buffer.add_string buf "\\</tr\\>\\<tr class=\\\"dl-1\\\"\\>";
               html_mods_td buf [
                 ("", "sr", "$bDownloads:$n");
                 ("", "sr", ""); ];
               Buffer.add_string buf "\\</tr\\>\\<tr class=\\\"dl-2\\\"\\>";
               html_mods_td buf [
                 ("", "sr", "$r\\<a href=\\\"submit?q=vd\\\"\\>" ^
                   "vd\\</a\\>$n");
                 ("", "sr", "view current downloads"); ];
               Buffer.add_string buf "\\</tr\\>\\<tr class=\\\"dl-2\\\"\\>";
               html_mods_td buf [
                 ("", "sr", "$rcancel/pause/resume <num>$n");
                 ("", "sr", "cancel/pause/resume download <num>"); ];
               Buffer.add_string buf "\\</tr\\>\\<tr class=\\\"dl-1\\\"\\>";
               html_mods_td buf [
                 ("", "sr", "$bSearches:$n");
                 ("", "sr", ""); ];
               Buffer.add_string buf "\\</tr\\>\\<tr class=\\\"dl-2\\\"\\>";
               html_mods_td buf [
                 ("", "sr", "$rs  <keywords>$n");
                 ("", "sr", "start a search for keywords <keywords> on the network"); ];
               Buffer.add_string buf "\\</tr\\>\\<tr class=\\\"dl-2\\\"\\>";
               html_mods_td buf [
                 ("", "sr", "$r\\<a href=\\\"submit?q=vr\\\"\\>" ^
                   "vr\\</a\\>$n");
                 ("", "sr", "view results of the last search"); ];
               Buffer.add_string buf "\\</tr\\>\\<tr class=\\\"dl-2\\\"\\>";
               html_mods_td buf [
                 ("", "sr", "$rd <num>$n");
                 ("", "sr", "download result number <num>"); ];
               Buffer.add_string buf "\\</tr\\>\\<tr class=\\\"dl-2\\\"\\>";
               html_mods_td buf [
                 ("", "sr", "$r\\<a href=\\\"submit?q=vs\\\"\\>" ^
                   "vs\\</a\\>$n");
                 ("", "sr", "view previous searches"); ];
               Buffer.add_string buf "\\</tr\\>\\<tr class=\\\"dl-2\\\"\\>";
               html_mods_td buf [
                 ("", "sr", "$rvr <num>$n");
                 ("", "sr", "view results of search <num>"); ];
               Buffer.add_string buf "\\</tr\\>\\<tr class=\\\"dl-1\\\"\\>";
               html_mods_td buf [
                 ("", "sr", "$bGeneral:$n");
                 ("", "sr", ""); ];
               Buffer.add_string buf "\\</tr\\>\\<tr class=\\\"dl-2\\\"\\>";
               html_mods_td buf [
                 ("", "sr", "$r\\<a href=\\\"submit?q=save\\\"\\>" ^
                   "save\\</a\\>$n");
                 ("", "sr", "save configuration files"); ];
               Buffer.add_string buf "\\</tr\\>\\<tr class=\\\"dl-2\\\"\\>";
               html_mods_td buf [
                 ("", "sr", "$rkill$n");
                 ("", "sr", "kill mldonkey properly"); ];
               Buffer.add_string buf "\\</tr\\>\\<tr class=\\\"dl-2\\\"\\>";
               html_mods_td buf [
                 ("", "sr", "$rq$n");
                 ("", "sr", "quit this interface"); ];
               Buffer.add_string buf "\\</tr\\>\\</table\\>\\</div\\>\n";
               html_mods_table_header buf "helpTable" "results" [];
               Buffer.add_string buf "\\<tr class=\\\"dl-1\\\"\\>";
               html_mods_td buf [
                 ("", "sr", "Use '$r\\<a href=\\\"submit?q=longhelp\\\"\\>" ^
                   "longhelp\\</a\\>$n' or '$r\\<a href=\\\"submit?q=longhelp\\\"\\>" ^
                   "??\\</a\\>$n' for all commands. Specify substring to filter."); ];
               Buffer.add_string buf "\\</tr\\>\\<tr class=\\\"dl-1\\\"\\>";
               html_mods_td buf [
                 ("", "sr", "Use '$rhelp command$n' or '$r? command$n' for help on a command."); ];
               Buffer.add_string buf "\\</tr\\>\\</table\\>\\</div\\>\\</div\\>\n"
            end
          else
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
          $rs <keywords>$n : start a search for keywords <keywords> on the network
          $rvr$n : view results of the last search
          $rd <num>$n : download result number <num>
          $rvs$n : view previous searches
          $rvr <num>$n : view results of search <num>

$bGeneral:$n
          $rsave$n : save configuration files
          $rkill$n : kill mldonkey properly
          $rq$n : quit this interface

Use '$rlonghelp$n' or '$r??$n' for all commands.
Use '$rlonghelp str$n' or '$r?? str$n' for all commands that contain specified substring.
Use '$rhelp command$n' or '$r? command$n' for help on a command.
            ";
    | "?" :: args | "help" :: args | "man" :: args ->
          List.iter (fun arg ->
              match List.filter (fun (cmd, _, _, _) -> cmd = arg) !CommonNetwork.network_commands with
              | [] -> Printf.bprintf buf "Unknown command : %s\n" arg
              | l -> List.iter (fun (_,_,_,help) -> Printf.bprintf  buf "%s %s\n" arg help) l)
          args
    | one :: two ->
        let cmd, args =
          (try
             let command = List.assoc one !!alias_commands in
             match String2.split command ' ' with
                 []   -> raise Not_found (* can't happen *)
               | [a]  -> a, two
               | a::b -> a, (b @ two)
           with
               Not_found -> one, two)
        in
      if cmd = "q" then
        raise CommonTypes.CommandCloseSocket
      else
      if cmd = "auth" then
        let user, pass =
          match args with
            [] -> failwith "Usage: auth <user> <password>"
          | [s1] -> (admin_user ()).CommonTypes.user_name, s1
          | user :: pass :: _ -> user, pass
        in
        if valid_password user pass then begin
            auth := true;
            o.conn_user <- find_ui_user user;
            if not !verbose_no_login then lprintf_nl "Authenticated user: %s" user;
            let module M = CommonMessages in
            Buffer.add_string buf M.full_access;
            (match DriverInteractive.real_startup_message () with
               Some s -> Buffer.add_string buf ("\n" ^ s);
             | None -> ());
          end else
        let module M = CommonMessages in
        Buffer.add_string buf M.bad_login
      else
      if !auth then
        DriverCommands.execute_command
          !CommonNetwork.network_commands o cmd args
      else
      let module M = CommonMessages in
      Buffer.add_string buf M.command_not_authorized


(* This function is called every hour to check if we have something to do
just now *)

let calendar_options = {
    conn_buf = Buffer.create 1000;
    conn_output = TEXT;
    conn_sortvd = NotSorted;
    conn_filter = (fun _ -> ());
    conn_user = find_ui_user CommonUserDb.admin_user_name;
    conn_width = 80; conn_height = 0;
    conn_info = Some (CALENDAR, (Ip.null, 0));
  }

let check_calendar () =
  let time = last_time () in
  let tm = Unix.localtime (date_of_int time) in
  List.iter (fun (days, hours, command) ->
      if (List.mem tm.Unix.tm_wday days || days = []) &&
        (List.mem tm.Unix.tm_hour hours || hours = []) then begin
          lprintf_nl "Calendar execute: %s" command;
          eval (ref true) command calendar_options;
          lprintf_nl "Calendar result: %s" (Buffer.contents calendar_options.conn_buf);
          Buffer.reset calendar_options.conn_buf;
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
            Buffer.reset buf;
            if o.conn_output = ANSI then Printf.bprintf buf "> $c%s$n\n" cmd;
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

(*
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
*)

type telnet_conn = {
    telnet_buffer : Buffer.t;
    mutable telnet_iac : bool;
    mutable telnet_wait : int;
    telnet_auth : bool ref;
  }

let iac_will_8bit = "\255\253\000"
let iac_will_naws = "\255\253\031"

let user_reader o telnet sock nread =
  let b = TcpBufferedSocket.buf sock in
  let rec iter () =
    if b.len > 0 then
      let c = Bytes.get b.buf b.pos in
      buf_used b 1;
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
                Buffer.reset telnet.telnet_buffer
          );
          iter ()
        end else

      let i = int_of_char c in
      telnet.telnet_iac <- false;
      let is_normal_char = i > 31 in

      if telnet.telnet_wait = 1 then begin
          Buffer.add_char telnet.telnet_buffer c;
          let cmd = Buffer.contents telnet.telnet_buffer in
          telnet.telnet_wait <- 0;
          let len = String.length cmd in
          if len = 2 then
            match cmd with
              "\251\031" ->
                Buffer.reset telnet.telnet_buffer
            | "\250\031" ->
                telnet.telnet_wait <- 4
            | _ ->
                (*
                lprintf "telnet server: Unknown control sequence %s\n"
                  (String.escaped cmd);                *)
                Buffer.reset telnet.telnet_buffer
          else
          let s = String.sub cmd 0 2 in
          Buffer.reset telnet.telnet_buffer;
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
          Buffer.reset telnet.telnet_buffer;
          if cmd <> "" then begin
              before_telnet_output o sock;
              let buf = o.conn_buf in
              Buffer.reset buf; 
              if o.conn_output = ANSI then Printf.bprintf buf "> $c%s$n\n" cmd;
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
       shutdown sock Closed_by_user;
     with _ -> ());
  | e ->
      before_telnet_output o sock;
      TcpBufferedSocket.write_string sock
        (Printf.sprintf "exception [%s]\n" (Printexc2.to_string e));
      after_telnet_output o sock


let user_closed sock  msg =
  user_socks := List2.removeq sock !user_socks;
  ()

let telnet_handler t event =
  match event with
    TcpServerSocket.CONNECTION (s, Unix.ADDR_INET (from_ip, from_port)) ->
      let from_ip = Ip.of_inet_addr from_ip in
        if not !verbose_no_login then lprintf_nl "Telnet connection from %s" (Ip.to_string from_ip);
        let token = create_token unlimited_connection_manager in
        let sock = TcpBufferedSocket.create_simple token
          "telnet connection"
          s in
        let telnet = {
            telnet_auth = ref (has_empty_password (admin_user ()));
            telnet_iac = false;
            telnet_wait = 0;
            telnet_buffer = Buffer.create 100;
          } in
        let o = {
            conn_buf = Buffer.create 1000;
            conn_output = (if !!term_ansi then ANSI else TEXT);
            conn_sortvd = NotSorted;
            conn_filter = (fun _ -> ());
            conn_user = find_ui_user CommonUserDb.admin_user_name;
            conn_width = 80;
            conn_height = 0;
            conn_info = Some (TELNET, (from_ip, from_port));
          } in
        (match Ip_set.match_ip !allowed_ips_set from_ip with
        | true -> 
        TcpBufferedSocket.prevent_close sock;
        TcpBufferedSocket.set_max_output_buffer sock !!interface_buffer;
        TcpBufferedSocket.set_reader sock (user_reader o telnet);
        TcpBufferedSocket.set_closer sock user_closed;
        user_socks := sock :: !user_socks;

        TcpBufferedSocket.write_string sock iac_will_8bit;
        TcpBufferedSocket.write_string sock iac_will_naws;

        before_telnet_output o sock;
        TcpBufferedSocket.write_string sock
           (Printf.sprintf "Welcome to MLDonkey %s\n" Autoconf.current_version);

        TcpBufferedSocket.write_string sock (dollar_escape o false
            "$cWelcome on mldonkey command-line$n\n\nUse $r?$n for help\n\n");

        after_telnet_output o sock

        | false ->
        before_telnet_output o sock;
        let reject_message =
          Printf.sprintf "Telnet connection from %s rejected (see allowed_ips setting)\n"
            (Ip.to_string from_ip)
        in
        TcpBufferedSocket.write_string sock (dollar_escape o false reject_message);
        shutdown sock Closed_connect_failed;
        if not !verbose_no_login then lprintf_n "%s" reject_message;
        Unix.close s)

  | _ -> ()

(*************************************************************

                  The HTTP Server

**************************************************************)

open Http_server

let buf = Buffer.create 1000

type http_file =
  BIN
| HTM
| MLHTM
| TXT
| UNK

type file_ext =
  BINARY
| CSS
| HTMLS
| ICON
| JPEG
| JAVASCRIPT
| MPEG
| AVI
| WMV
| ASF
| MOV
| OGM
| RM
| MKV
| PNG
| GIF
| MP3
| WMA
| OGG
| TEXTS
| UNKN
| WML

let http_file_type = ref UNK

let extension_to_file_ext extension =
  match extension with
  | "bin" -> BINARY
  | "css" -> CSS
  | "htm"
  | "html" -> HTMLS
  | "ico" -> ICON
  | "jpe"
  | "jpeg"
  | "jpg" -> JPEG
  | "js" -> JAVASCRIPT
  | "vob"
  | "mpe"
  | "mpeg"
  | "mpg" -> MPEG
  | "avi" -> AVI
  | "wmv" -> WMV
  | "asf" -> ASF
  | "mov"
  | "movie"
  | "qt" -> MOV
  | "ogm" -> OGM
  | "ra"
  | "ram"
  | "rm"
  | "rmvb"
  | "rv9"
  | "rt" -> RM
  | "mkv" -> MKV
  | "png" -> PNG
  | "gif" -> GIF
  | "mp3" -> MP3
  | "wma" -> WMA
  | "ogg" -> OGG
  | "txt" -> TEXTS
  | "wml" -> WML
  | _ -> UNKN

let ext_to_file_type ext =
  match ext with
    UNKN -> UNK
  | BINARY -> BIN
  | CSS -> TXT
  | HTMLS -> HTM
  | ICON -> BIN
  | JAVASCRIPT -> TXT
  | JPEG -> BIN
  | MPEG -> BIN
  | AVI -> BIN
  | WMV -> BIN
  | ASF -> BIN
  | MOV -> BIN
  | OGM -> BIN
  | RM -> BIN
  | MKV -> BIN
  | PNG -> BIN
  | GIF -> BIN
  | MP3 -> BIN
  | WMA -> BIN
  | OGG -> BIN
  | TEXTS -> TXT
  | WML -> TXT

let ext_to_mime_type ext =
  match ext with
    UNKN -> ""
  | BINARY -> "application/octet-stream"
  | CSS -> "text/css"
  | HTMLS -> "text/html"
  | ICON -> "image/x-icon"
  | JAVASCRIPT -> "text/javascript"
  | JPEG -> "image/jpg"
  | MPEG -> "video/mpeg"
  | AVI -> "video/x-msvideo"
  | WMV -> "video/x-ms-wmv"
  | ASF -> "video/x-ms-asf"
  | MOV -> "video/quicktime"
  | OGM -> "application/ogg" (* is that correct ? *)
  | RM -> "audio/x-pn-realaudio"
  | MKV -> "video/x-matroska" (* is that correct ? *)
  | PNG -> "image/png"
  | GIF -> "image/gif"
  | MP3 -> "audio/mpeg"
  | WMA -> "audio/x-ms-wma"
  | OGG -> "application/ogg" (* is that correct ? *)
  | TEXTS -> "text/plain"
  | WML -> "text/vnd.wap.wml"

let default_charset = "charset=UTF-8"

let get_theme_page page =
        let theme = Filename.concat html_themes_dir !!html_mods_theme in
        let fname = Filename.concat theme page in fname

let theme_page_exists page =
    Sys.file_exists (get_theme_page page)

(* if files are small really_input should be okay *)
let read_theme_page page =
  let theme_page = get_theme_page page in
  Unix2.tryopen_read theme_page (fun file ->
    let size = (Unix.stat theme_page).Unix.st_size in
    really_input_string file size)

let http_add_gen_header r =
  add_reply_header r "Server" ("MLdonkey/"^Autoconf.current_version);
  add_reply_header r "Connection" "close"

let add_gzip_headers r =
  if !!html_use_gzip then begin
    add_reply_header r "Content-Encoding" "gzip";
    add_reply_header r "Vary" "Accept-Encoding";
  end

let http_add_html_header r =
  let ext = extension_to_file_ext "html" in
  http_file_type := ext_to_file_type ext;
  http_add_gen_header r;
  add_reply_header r "Pragma" "no-cache";
  add_reply_header r "Content-Type" ((ext_to_mime_type ext) ^ ";" ^ default_charset);
  add_gzip_headers r

let http_add_text_header r ext =
  http_file_type := ext_to_file_type ext;
  http_add_gen_header r;
  add_reply_header  r "Content-Type" ((ext_to_mime_type ext) ^ ";" ^ default_charset);
  add_gzip_headers r

let http_add_bin_info_header r clen =
  add_reply_header r "Accept-Ranges" "bytes"

let http_add_bin_header r ext clen =
  http_file_type := ext_to_file_type ext;
  http_add_gen_header r;
  add_reply_header r "Content-Type" (ext_to_mime_type ext);
  http_add_bin_info_header r clen

let http_add_bin_stream_header r ext =
  http_file_type := BIN;
  http_add_gen_header r;
  let mime_type = ext_to_mime_type ext in
  let mime_type = if mime_type <> "" then mime_type
  else "application/binary" in
  add_reply_header r "Content-Type" mime_type;
  add_reply_header r "Accept-Ranges" "bytes"

let http_send_bin r buf filename =
  let file_to_send =
    if theme_page_exists filename then
      File.to_string (get_theme_page filename)
    else
      try
        File.to_string filename
      with _ -> raise Not_found
  in
  let ext = extension_to_file_ext (Filename2.last_extension2 filename) in
  http_add_bin_header r ext (String.length file_to_send);
  add_reply_header r "Cache-Control" "no-cache";
  add_reply_header r "Pragma" "no-cache";
  Buffer.add_string buf file_to_send

let http_send_bin_pictures r buf filename =
  let file_to_send =
    try
      Hashtbl.find CommonPictures.files filename
    with Not_found ->
      try
        if String.sub filename 0 4 = "flag" then
          Hashtbl.find CommonPictures.files "flag_--.png"
        else
          raise Not_found
    with _ -> raise Not_found
  in
  let ext = extension_to_file_ext (Filename2.last_extension2 filename) in
  http_add_bin_header r ext (String.length file_to_send);
  Buffer.add_string buf file_to_send

let http_error_no_gd img_type =
  match img_type with
    "jpg" ->
      (match Autoconf.has_gd_jpg with
        true -> false
      | false -> lprintf_nl "Warning: GD jpg support disabled"; true)
  | "png" ->
      (match Autoconf.has_gd_png with
        true -> false
      | false -> lprintf_nl "Warning: GD png support disabled"; true)
  | _ ->
      (match Autoconf.has_gd with
        true -> false
      | false -> lprintf_nl "Warning: GD support disabled"; true)
let any_ip = Ip.of_inet_addr Unix.inet_addr_any

let html_open_page buf t r open_body =
  Buffer.reset buf;
  http_add_html_header r;

  if not !!html_mods then
    (Buffer.add_string buf
      "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\"
        \"http://www.w3.org/TR/html4/frameset.dtd\">\n<HTML>\n<HEAD>\n";)
    else Buffer.add_string buf "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">\n<html>\n<head>\n";
  if !CommonInteractive.display_vd then begin
      let this_page = "dheader.html" in
      Buffer.add_string buf
        (
                if !!html_mods_theme <> "" && theme_page_exists this_page then
                        read_theme_page this_page else
                        if !!html_mods then !!CommonMessages.download_html_header_mods0
                        else !!CommonMessages.download_html_header_old);
      Printf.bprintf buf "<meta http-equiv=\"refresh\" content=\"%d\">" !!vd_reload_delay;
    end else
    if !CommonInteractive.display_bw_stats then
      Printf.bprintf buf "<meta http-equiv=\"refresh\" content=\"%d\">" !!html_mods_bw_refresh_delay;
   
        let this_page = "header.html" in
    Buffer.add_string buf (
                if !!html_mods_theme <> "" && theme_page_exists this_page then
                        read_theme_page this_page else
                        if !!html_mods then !!CommonMessages.html_header_mods0
                        else !!CommonMessages.html_header_old);

  Buffer.add_string buf "</head>\n";
  if open_body then Buffer.add_string buf "<body>\n"

let html_close_page buf close_body =
  if close_body then Buffer.add_string buf "</body>\n";
  Buffer.add_string buf "</html>\n"

let clear_page buf =
  Buffer.reset buf;
  http_file_type := UNK

let send_preview r file fd size filename exten =
  let (begin_pos, end_pos) =
    try
      let (begin_pos, end_pos) = request_range r in
      let end_pos = match end_pos with
          None -> size
        | Some end_pos -> end_pos in
      let range_size = end_pos -- begin_pos in
      add_reply_header r "Content-Length"
        (Int64.to_string range_size);
      add_reply_header r "Content-Range"
        (Printf.sprintf "bytes %Ld-%Ld/%Ld"
          begin_pos (end_pos -- one)
        size);
      r.reply_head <- "206 Partial Content";
      begin_pos, end_pos
    with _ ->
        add_reply_header r "Content-Length"
          (Int64.to_string size);
        zero, size
  in
  let len = String.length exten in
  let exten = if len = 0 then exten
      else String.lowercase (String.sub exten 1 (len - 1)) in
  http_add_bin_stream_header r (extension_to_file_ext exten);

  add_reply_header r "Content-Disposition"
  (Printf.sprintf "inline;filename=\"%s\"" (Filename.basename filename)); 
  let s = Bytes.create 200000 in
  set_max_output_buffer r.sock (Bytes.length s);
  set_rtimeout r.sock 10000.;
  let rec stream_file file pos sock =
    let max = (max_refill sock) - 1 in
    if max > 0 && !pos < end_pos then
      let max64 = min (end_pos -- !pos) (Int64.of_int max) in
      let max = Int64.to_int max64 in
      Unix32.read fd !pos s 0 max;
      pos := !pos ++ max64;
      set_lifetime sock 60.;
(*                      lprintf "HTTPSEND: refill %d %Ld\n" max !pos;*)
(*                    lprintf "HTTPSEND: [%s]\n" (String.escaped
                        (String.sub s 0 max)); *)
      write sock s 0 max;
      if output_buffered sock = 0 then begin
(*                          lprintf "Recursing STREAM\n"; *)
          stream_file file pos sock
        end
  in
  r.reply_stream <- Some (stream_file file (ref begin_pos))


let http_handler o t r =
  CommonInteractive.display_vd := false;
  CommonInteractive.display_bw_stats := false;
  clear_page buf;
  let short_file =
    let file = r.get_url.Url.short_file in
    match !!http_root_url with
    | "" | "/" -> `File file
    | root ->
      let root = if not (String2.starts_with root "/") then "/" ^ root else root in
      (* we want to treat "/root" requests as invalid and redirect them to "/root/" *)
      let root_dir = if root <> "" && root.[String.length root - 1] = '/' then root else root ^ "/" in
      if String2.starts_with ("/"^file) root_dir then
        `File (String2.after file (String.length root_dir - 1))
      else
        `Redirect root_dir
  in
  if !Http_server.verbose && short_file <> `File "" then
    lprintf_nl "received URL %s %s"
      r.get_url.Url.short_file
      (let b = Buffer.create 100 in
         List.iter (fun (arg, value) -> Printf.bprintf b " %s %s" arg value) r.get_url.Url.args;
         if Buffer.contents b <> "" then Printf.sprintf "(%s)" (Buffer.contents b) else "");

  let user = if r.options.login = "" then (admin_user ()).CommonTypes.user_name else r.options.login in
  if not (valid_password user r.options.passwd) || (short_file = `File "logout") then begin
      clear_page buf;
      http_file_type := HTM;
      let _, error_text_long, head = Http_server.error_page Unauthorized (TcpBufferedSocket.my_ip r.sock) !!http_port in
      Buffer.add_string buf error_text_long;
      r.reply_head <- head;
      http_add_html_header r;
      add_reply_header r "WWW-Authenticate" (Printf.sprintf "Basic realm=\"%s\"" !!http_realm);
    end
  else
    begin
      let user = find_ui_user user  in
      let o = match user.ui_http_conn with
          Some oo -> oo.conn_buf <- o.conn_buf;
            oo.conn_info <- Some (WEB, peer_addr t); oo
        | None -> let oo = { o with conn_user = user;
                                    conn_info = Some (WEB, peer_addr t)} in
            user.ui_http_conn <- Some oo; oo
      in
      try
        match short_file with
        | `Redirect url ->
          let _, error_text_long, head = Http_server.error_page (Moved url)
            (TcpBufferedSocket.my_ip r.sock)
            !!http_port
          in
          r.reply_head <- head;
          add_reply_header r "Location" url;
          http_add_html_header r;
          Buffer.add_string buf error_text_long
        | `File short_file ->
        match short_file with
        | "wap.wml" ->
            begin
              clear_page buf;
              http_add_text_header r WML;
              let dlkbs =
                (( (float_of_int !udp_download_rate) +. (float_of_int !control_download_rate)) /. 1024.0) in
              let ulkbs =
                (( (float_of_int !udp_upload_rate) +. (float_of_int !control_upload_rate)) /. 1024.0) in
              Printf.bprintf buf "
<?xml version=\"1.0\"?>
<!DOCTYPE wml PUBLIC \"-//WAPFORUM//DTD WML 1.1//EN\" \"http://www.wapforum.org/DTD/wml_1.1.xml\">

<wml>
<card id=\"main\" title=\"MLDonkey Index Page\">  ";
(* speed *)
              Printf.bprintf buf "<p align=\"left\">
   <small>
    DL %.1f KB/s (%d|%d) UL: %.1f KB/s (%d|%d)
   </small>
</p>" dlkbs !udp_download_rate !control_download_rate ulkbs !udp_upload_rate !control_upload_rate;


(* functions *)
              List.iter (fun (arg, value) ->
                  match arg with
                    "VDC" ->
                      let num = int_of_string value in
                      let file = file_find num in
                      file_cancel file o.conn_user.ui_user
                  | "VDP" ->
                      let num = int_of_string value in
                      let file = file_find num in
                      file_pause file o.conn_user.ui_user
                  | "VDR" ->
                      let num = int_of_string value in
                      let file = file_find num in
                      file_resume file o.conn_user.ui_user
                  | _ -> ()
              ) r.get_url.Url.args;

(* downloads *)
              Printf.bprintf buf "<p align=\"left\"><small>";
              let mfiles = List2.tail_map file_info !!files in
              List.iter (fun file ->
                  Printf.bprintf buf  "<a href=\"wap.wml?%s=%d\">%s</a> <a href=\"wap.wml?VDC=%d\">C</a> [%-5d] %5.1f %s %s/%s <br />" 
                    (if downloading file then "VDP" else "VDR" ) 
                    (file.file_num) 
                    (if downloading file then "P" else "R" ) 
                    (file.file_num) 
                    (file.file_num) 
                    (file.file_download_rate /. 1024.)
                    (short_name file) 
                    (print_human_readable file (file.file_size -- file.file_downloaded)) 
                    (print_human_readable file file.file_size);
              ) mfiles;
              Printf.bprintf buf "<br />Downloaded %d/%d files " (List.length !!done_files) (List.length !!files);
              Printf.bprintf buf "</small></p>";
              Printf.bprintf buf "</card></wml>";
            end
        | "commands.html" ->
            html_open_page buf t r true;
            let this_page = "commands.html" in
            Buffer.add_string buf (
              if !!html_mods_theme <> "" && theme_page_exists this_page then
                read_theme_page this_page else
              if !!html_mods then !!CommonMessages.web_common_header_mods0
              else !!CommonMessages.web_common_header_old)
        | "multidllink.html" ->
            html_open_page buf t r true;
            let this_page = "multidllink.html" in
            Buffer.add_string buf (
              if !!html_mods_theme <> "" && theme_page_exists this_page then
                read_theme_page this_page else
              if !!html_mods then !!CommonMessages.multidllink_mods0
              else !!CommonMessages.multidllink_old)
        | "" | "index.html" ->
                html_open_page buf t r false;
                let this_page = "frames.html" in
                if !!html_mods_theme <> "" && theme_page_exists this_page then
                  Buffer.add_string buf (read_theme_page this_page) else
                if !!html_mods then
                  (if !!html_frame_border then
                    Printf.bprintf buf
"<frameset src=\"index\" rows=\"%d,25,*\">
<frame name=\"commands\" noresize scrolling=\"no\" noshade marginwidth=0 marginheight=0 border=0 framespacing=0 src=\"commands.html\">
<frame name=\"fstatus\" noresize scrolling=\"no\" noshade marginwidth=0 marginheight=0 border=0 framespacing=0 src=\"noframe.html\">
<frame name=\"output\" noresize noshade marginwidth=0 marginheight=0 border=0 framespacing=0 src=\"oneframe.html\">
</frameset>
" !!commands_frame_height
                  else
                    Printf.bprintf buf
"<frameset src=\"index\" rows=\"%d,25,*\" frameborder=\"no\">
<frame name=\"commands\" noresize scrolling=\"no\" noshade marginwidth=0 marginheight=0 border=0 framespacing=0 src=\"commands.html\">
<frame name=\"fstatus\" noresize scrolling=\"no\" noshade marginwidth=0 marginheight=0 border=0 framespacing=0 src=\"noframe.html\">
<frame name=\"output\" noresize noshade marginwidth=0 marginheight=0 border=0 framespacing=0 src=\"oneframe.html\">
</frameset>
" !!commands_frame_height)
                else
                  Printf.bprintf buf
"<frameset src=\"index\" rows=\"%d,2*\">
<frameset src=\"index\" cols=\"5*,1*\">
<frame name=\"commands\" src=\"commands.html\">
<frame name=\"fstatus\" src=\"noframe.html\">
</frameset>
<frame name=\"output\" src=\"oneframe.html\">
</frameset>
" !!commands_frame_height
        | "complex_search.html" ->
            html_open_page buf t r true;
            CommonSearch.complex_search buf
        | "noframe.html" ->
            html_open_page buf t r true

        | "oneframe.html" ->
            html_open_page buf t r true;
            Buffer.add_string buf (Printf.sprintf "<br><div align=\"center\"><h3 class=\"header3\">%s %s</h3></div>"
              (Printf.sprintf (_b "Welcome to MLDonkey")) Autoconf.current_version);
            if !!motd_html <> "" then Buffer.add_string buf !!motd_html;
            if user2_is_admin o.conn_user.ui_user then
            (match DriverInteractive.real_startup_message () with
               Some s -> Buffer.add_string buf (Printf.sprintf "<p><pre><b><h3>%s</b></h3></pre>" s);
             | None -> ())

        | "bw_updown.png" ->
            (match http_error_no_gd "png" with
              false ->
                G.do_draw_pic "Traffic" "s(kb)" "t(h:m:s)" download_history upload_history;
                http_send_bin r buf "bw_updown.png"
            | true -> raise Not_found)

        | "bw_updown.jpg" ->
            (match http_error_no_gd "jpg" with
              false ->
                G.do_draw_pic "Traffic" "s(kb)" "t(h:m:s)" download_history upload_history;
                http_send_bin r buf "bw_updown.jpg"
            | true -> raise Not_found)

        | "bw_download.png" ->
            (match http_error_no_gd "png" with
              false ->
                G.do_draw_down_pic "Traffic" "download" "s(kb)" "t(h:m:s)" download_history;
                http_send_bin r buf "bw_download.png"
            | true -> raise Not_found)

        | "bw_download.jpg" ->
            (match http_error_no_gd "jpg" with
              false ->
                G.do_draw_down_pic "Traffic" "download" "s(kb)" "t(h:m:s)" download_history;
                http_send_bin r buf "bw_download.jpg"
            | true -> raise Not_found)

        | "bw_upload.png" ->
            (match http_error_no_gd "png" with
              false ->
                G.do_draw_up_pic "Traffic" "upload" "s(kb)" "t(h:m:s)" upload_history;
                http_send_bin r buf "bw_upload.png"
            | true -> raise Not_found)

        | "bw_upload.jpg" ->
            (match http_error_no_gd "jpg" with
            | false ->
                G.do_draw_up_pic "Traffic" "upload" "s(kb)" "t(h:m:s)" upload_history;
                http_send_bin r buf "bw_upload.jpg"
            | true -> raise Not_found)

        | "bw_h_updown.png" ->
            (match http_error_no_gd "png" with
            | false ->
                G.do_draw_h_pic "Traffic" "s(kb)" "t(h:m:s)" download_h_history upload_h_history;
                http_send_bin r buf "bw_h_updown.png"
            | true -> raise Not_found)

        | "bw_h_updown.jpg" ->
            (match http_error_no_gd "jpg" with
            | false ->
                G.do_draw_h_pic "Traffic" "s(kb)" "t(h:m:s)" download_h_history upload_h_history;
                http_send_bin r buf "bw_h_updown.jpg"
            | true -> raise Not_found)

        | "bw_h_download.png" ->
            (match http_error_no_gd "png" with
            | false ->
                G.do_draw_down_h_pic "Traffic" "download" "s(kb)" "t(h:m:s)" download_h_history;
                http_send_bin r buf "bw_h_download.png"
            | true -> raise Not_found)

        | "bw_h_download.jpg" ->
            (match http_error_no_gd "jpg" with
            | false ->
                G.do_draw_down_h_pic "Traffic" "download" "s(kb)" "t(h:m:s)" download_h_history;
                http_send_bin r buf "bw_h_download.jpg"
            | true -> raise Not_found)

        | "bw_h_upload.png" ->
            (match http_error_no_gd "png" with
            | false ->
                G.do_draw_up_h_pic "Traffic" "upload" "s(kb)" "t(h:m:s)" upload_h_history;
                http_send_bin r buf "bw_h_upload.png"
            | true -> raise Not_found)

        | "bw_h_upload.jpg" ->
            (match http_error_no_gd "jpg" with
            | false ->
                G.do_draw_up_h_pic "Traffic" "upload" "s(kb)" "t(h:m:s)" upload_h_history;
                http_send_bin r buf "bw_h_upload.jpg"
            | true -> raise Not_found)

        | "tag.png" ->
            (match http_error_no_gd "png" with
            | false ->
                G.do_draw_tag !!html_mods_vd_gfx_tag_title download_history upload_history;
                http_send_bin r buf "tag.png"
            | true -> raise Not_found)

        | "tag.jpg" ->
            (match http_error_no_gd "jpg" with
            | false ->
                G.do_draw_tag !!html_mods_vd_gfx_tag_title download_history upload_history;
                http_send_bin r buf "tag.jpg"
            | true -> raise Not_found)

        | "filter" ->
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

        | "results" ->
            html_open_page buf t r true;
            let b = Buffer.create 10000 in
            List.iter (fun (arg, value) ->
                match arg with
                  "d" -> begin
                      try
                        let num = int_of_string value in
                        let r = find_result num in
                        let files = result_download r [] false o.conn_user.ui_user in
                        List.iter CommonInteractive.start_download files;

                        let module M = CommonMessages in
                        Gettext.buftext buf M.download_started num
                      with  e ->
                          Printf.bprintf buf "Error %s with %s<br>"
                            (Printexc2.to_string e) value;
                    end
                | _ -> ()
            ) r.get_url.Url.args;
            Buffer.add_string buf (html_escaped (Buffer.contents b))

        | "files" ->

            List.iter (fun (arg, value) ->
                match arg with
                  "cancel" ->
                    let num = int_of_string value in
                    let file = file_find num in
                    file_cancel file o.conn_user.ui_user
                | "pause" ->
                    let num = int_of_string value in
                    let file = file_find num in
                    file_pause file o.conn_user.ui_user
                | "resume" ->
                    let num = int_of_string value in
                    let file = file_find num in
                    file_resume file o.conn_user.ui_user
                | "release" ->
                    let num = int_of_string value in
                    let file = file_find num in
                    set_file_release file true o.conn_user.ui_user
                | "norelease" ->
                    let num = int_of_string value in
                    let file = file_find num in
                    set_file_release file false o.conn_user.ui_user
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
                      | "Avail" -> o.conn_sortvd <- ByAvail
                      | "Cm" -> o.conn_sortvd <- ByComments
                      | "User" -> o.conn_sortvd <- ByUser
                      | "Group" -> o.conn_sortvd <- ByGroup
                      | _ -> ()
                    end
                | _ -> ()
            ) r.get_url.Url.args;
            let b = Buffer.create 10000 in

            let list = List2.tail_map file_info (user2_filter_files !!files o.conn_user.ui_user) in
            DriverInteractive.display_file_list b o list;
            html_open_page buf t r true;
            Buffer.add_string buf (html_escaped (Buffer.contents b))

        | "submit" ->
            begin

              match r.get_url.Url.args with
                | [ "jvcmd", "multidllink" ; "links", links] ->
                    html_open_page buf t r true;
                    List.iter (fun url ->
                      let url = fst (String2.cut_at url '\013') in
                      if url <> "" then
                        begin
                          Buffer.add_string buf (html_escaped (dllink_parse (o.conn_output = HTML) url o.conn_user.ui_user));
                          Buffer.add_string buf (html_escaped "\\<P\\>")
                        end
                    ) (String2.split links '\n')

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
                    clear_page b;
                    eval (ref true) cmd o;
                    html_escaped (Buffer.contents b)
                  in
                  html_open_page buf t r true;

(* Konqueror doesn't like html within <pre> *)
                  let drop_pre = ref false in
                  let rawcmd = ref cmd in

                  if String.contains cmd ' ' then
                    rawcmd := String.sub cmd 0 (String.index cmd ' ');

                  (match !rawcmd with
                    | "vm" | "vma" | "view_custom_queries"  | "xs" | "vr"
                    | "afr" | "friend_remove" | "reshare" | "recover_temp"
                    | "c" | "commit" | "bw_stats" | "ovweb" | "friends"
                    | "message_log" | "friend_add" | "remove_old_servers"
                    | "downloaders" | "uploaders" | "scan_temp" | "cs"
                    | "version" | "rename" | "force_download" | "close_fds"
                    | "vd" | "vo" | "voo" | "upstats" | "shares" | "share"
                    | "unshare" | "stats" | "users" | "block_list" -> 
                        drop_pre := true;
                    | _ -> ());
                  Printf.bprintf buf "%s\n"
                    (if use_html_mods o && !drop_pre then s else "\n<pre>\n" ^ s ^ "</pre>");

              | [ ("custom", query) ] ->
                  html_open_page buf t r true;
                  CommonSearch.custom_query buf query

              | ("custom", query) :: args ->
                  html_open_page buf t r true;
                  send_custom_query o.conn_user buf query args

              | [ ("api", cmd) ] ->
                  clear_page o.conn_buf;
                  eval (ref true) cmd o;
                  Buffer.add_string buf (Buffer.contents o.conn_buf)

              | [ "setoption", _ ; "option", name; "value", value ] ->
                  html_open_page buf t r true;
                  let gui_type, ip, port =
                    match o.conn_info with
                    | None -> None, None, None
                    | Some (gui_type, (ip, port)) -> Some gui_type, Some ip, Some port
                  in
                  if user2_is_admin o.conn_user.ui_user then
                    begin
                      CommonInteractive.set_fully_qualified_options name value
                        ~user:(Some o.conn_user.ui_user.CommonTypes.user_name)
                        ~ip:ip ~port:port ~gui_type:gui_type ();
                      Buffer.add_string buf "Option value changed"
                    end
                  else
                    Buffer.add_string buf "You are not allowed to change options"

              | args ->
                  List.iter (fun (s,v) ->
                      lprintf_nl "[%s]=[%s]" (String.escaped s) (String.escaped v))
                  args;
                  raise Not_found
            end

        | "preview_download" ->
            begin
              clear_page buf;
              match r.get_url.Url.args with
                ["q", file_num] ->
                  let file_num = int_of_string file_num in
                  let file = file_find file_num in
                  let fd = file_fd file in
                  let size = file_size file in
                  let filename = file_best_name file in
                  let exten = Filename2.last_extension filename in
                    send_preview r file fd size filename exten

              | args ->
                  List.iter (fun (s,v) ->
                      lprintf_nl "[%s]=[%s]" (String.escaped s) (String.escaped v))
                  args;
                  raise Not_found
            end

        | "preview_upload" ->
            begin
              clear_page buf;
              match r.get_url.Url.args with
                ["q", file_num] ->
                  let file_num = int_of_string file_num in
                  let file = shared_find file_num in
                  let impl = as_shared_impl file in
                  let info = shared_info file in
                  let filename = impl.impl_shared_fullname in
                  let exten = Filename2.last_extension impl.impl_shared_codedname in
                  if not (Sys.file_exists filename) then
                    begin
          lprintf_nl "file %s not found" filename;
                      raise Not_found
                    end;
                  let fd = Unix32.create_ro filename in
                  let size = info.shared_size in
                    send_preview r file fd size filename exten

              | args ->
                  List.iter (fun (s,v) ->
                      lprintf_nl "[%s]=[%s]" (String.escaped s) (String.escaped v))
                  args;
                  raise Not_found
            end

        | "h.css" ->
            clear_page buf;
            http_add_text_header r CSS;
            let this_page = "h.css" in
            Buffer.add_string buf (
              if !!html_mods_theme <> "" && theme_page_exists this_page then
                read_theme_page this_page else
              if !!html_mods then !CommonMessages.html_css_mods
              else !!CommonMessages.html_css_old)

        | "dh.css" ->
            clear_page buf;
            http_add_text_header r CSS;
            let this_page = "dh.css" in
            Buffer.add_string buf (
              if !!html_mods_theme <> "" && theme_page_exists this_page then
                read_theme_page this_page else
              if !!html_mods then !CommonMessages.download_html_css_mods
              else !!CommonMessages.download_html_css_old)

        | "i.js" ->
            clear_page buf;
            http_add_text_header r JAVASCRIPT;
            let this_page = "i.js" in
            Buffer.add_string buf (
              if !!html_mods_theme <> "" && theme_page_exists this_page then
                read_theme_page this_page else
              if !!html_mods then !!CommonMessages.html_js_mods0
              else !!CommonMessages.html_js_old)

        | "di.js" ->
            clear_page buf;
            http_add_text_header r JAVASCRIPT;
            let this_page = "di.js" in
            Buffer.add_string buf (
              if !!html_mods_theme <> "" && theme_page_exists this_page then
                read_theme_page this_page else
              if !!html_mods then !!CommonMessages.download_html_js_mods0
              else !!CommonMessages.download_html_js_old)
        | s ->  http_send_bin_pictures r buf (String.lowercase s)
      with
      | Not_found ->
          let _, error_text_long, head = Http_server.error_page (Not_Found r.get_url.Url.full_file)
                        (TcpBufferedSocket.my_ip r.sock) !!http_port
    in
          r.reply_head <- head;
          http_add_html_header r;
          Buffer.add_string buf error_text_long
      | e ->
          http_add_text_header r TEXTS;
          Printf.bprintf buf "%sException %s\n"
            (if Buffer.length buf = 0 then "" else "\n") (Printexc2.to_string e);
          r.reply_stream <- None
    end;

  let s =
    match !http_file_type with
      HTM -> html_close_page buf false; dollar_escape o true (Buffer.contents buf)
    | MLHTM -> html_close_page buf true; dollar_escape o true (Buffer.contents buf)
    | TXT
    | UNK
    | BIN -> Buffer.contents buf
  in
  r.reply_content <- 
    if !http_file_type <> BIN && !!html_use_gzip then 
      Bytes.unsafe_to_string (Zlib2.gzip_string s)
    else s

let http_options = {
    conn_buf = Buffer.create 10000;
    conn_output = HTML;
    conn_sortvd = NotSorted;
    conn_filter = (fun _ -> ());
    conn_user = find_ui_user CommonUserDb.admin_user_name;
    conn_width = 80; conn_height = 0;
    conn_info = Some (WEB, (Ip.null, 0));
  }

let create_http_handler () =
  let config = {
      bind_addr = Ip.to_inet_addr !!http_bind_addr ;
      port = !!http_port;
      requests = [];
      addrs = Ip_set.of_list !!allowed_ips;
(* do not limit access to MLDonkey web interface by IP blocklist *)
      use_ip_block_list = false;
      base_ref = "";
      default = http_handler http_options;
    } in
  option_hook allowed_ips (fun _ ->
    config.addrs <- Ip_set.of_list !!allowed_ips);
  ignore(find_port "http server" !!http_bind_addr http_port
      (Http_server.handler config));
  config.port <- !!http_port
