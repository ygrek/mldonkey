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
open Md4
open Options
open BasicSocket

open GuiTypes

open CommonResult
open CommonMessages
open CommonGlobals
open CommonShared
open CommonSearch
open CommonClient
open CommonServer
open CommonNetwork
open CommonTypes
open CommonFile
open CommonComplexOptions
open CommonOptions
open CommonUserDb
open CommonInteractive

open DriverInteractive

open Gettext

module VB = VerificationBitmap

let log_prefix = "[dCmd]"

let lprintf_nl fmt =
  lprintf_nl2 log_prefix fmt

let lprintf_n fmt =
  lprintf2 log_prefix fmt

let _s x = _s "DriverCommands" x
let _b x = _b "DriverCommands" x

type command_links_data = {
  filename : string;
  filesize : int64;
  fileid : Md4.t;
  }

let to_cancel = ref []

let files_to_cancel o =
  let buf = o.conn_buf in
  Printf.bprintf buf (_b "Files to be cancelled:\n");
  List.iter (fun file ->
      file_print file o
  ) !to_cancel;
  "Type 'confirm yes/no' to cancel them"

let execute_command arg_list output cmd args =
  if !verbose then
    lprintf_nl "execute command %S %s" cmd (String.concat " " (List.map (Printf.sprintf "%S") args));
  let buf = output.conn_buf in
  try
    let rec iter list =
      match list with
        [] ->
          Gettext.buftext buf no_such_command cmd
      | (command, _, arg_kind, help) :: tail ->
          if command = cmd then begin
            if !verbose_user_commands && not (user2_is_admin output.conn_user.ui_user) then
              lprintf_nl "user %s issued command %s%s"
                output.conn_user.ui_user.user_name
                cmd
                (if args = [] then "" else ", args " ^ String.concat " " args);
            Buffer.add_string buf (
              match arg_kind, args with
                Arg_none f, [] -> f output
              | Arg_multiple f, _ -> f args output
              | Arg_one f, [arg] -> f arg  output
              | Arg_two f, [a1;a2] -> f a1 a2 output
              | Arg_three f, [a1;a2;a3] -> f a1 a2 a3 output
              | _ -> bad_number_of_args command help
            )
            end
          else
            iter tail
    in
    iter arg_list
  with Not_found -> ()

let list_options_html o list =
  let buf = o.conn_buf in
  if !!html_mods_use_js_helptext then
    html_mods_table_header buf "upstatsTable" "upstats" [
      ( Str, "srh", "Option name", "Name (Help=mouseOver)" ) ;
      ( Str, "srh", "Option value", "Value (press ENTER to save)" ) ;
      ( Str, "srh", "Option default", "Default" );
      ( Str, "srh", "Option type", "Type" );
    ]
  else
    html_mods_table_header buf "voTable" "vo" [
      ( Str, "srh", "Option name", "Name" ) ;
      ( Str, "srh", "Option value", "Value (press ENTER to save)" ) ;
      ( Str, "srh", "Option default", "Default" ) ;
      ( Str, "srh", "Option type", "Type" );
      ( Str, "srh", "Option description", "Help" ) ];

  html_mods_cntr_init ();

  List.iter (fun o ->
      Printf.bprintf buf "\\<tr class=\\\"dl-%d\\\"" (html_mods_cntr ());
    
        if !!html_mods_use_js_helptext then
          Printf.bprintf buf " onMouseOver=\\\"mOvr(this);setTimeout('popLayer(\\\\\'%s\\\\\')',%d);setTimeout('hideLayer()',%d);return true;\\\" onMouseOut=\\\"mOut(this);hideLayer();setTimeout('hideLayer()',%d)\\\"\\>"
          (Str.global_replace (Str.regexp "\n") "\\<br\\>" (Http_server.html_real_escaped o.option_help)) !!html_mods_js_tooltips_wait !!html_mods_js_tooltips_timeout !!html_mods_js_tooltips_wait
        else
          Printf.bprintf buf "\\>";

      if String.contains o.option_value '\n' then
        Printf.bprintf buf "\\<td class=\\\"sr\\\"\\>
\\<a href=\\\"http://mldonkey.sourceforge.net/%s\\\"\\>%s\\</a\\>
\\<form action=\\\"submit\\\" target=\\\"$S\\\" onsubmit=\\\"javascript: {setTimeout('window.location.replace(window.location.href)',500);}\\\"\\>
\\<input type=hidden name=setoption value=q\\>\\<input type=hidden name=option value=%s\\>\\</td\\>
\\<td\\>\\<textarea name=value rows=5 cols=20 wrap=virtual\\>%s\\</textarea\\>
\\<input type=submit value=Modify\\>"
                  (String2.upp_initial o.option_name) o.option_name o.option_name o.option_value
      else
        begin
        Printf.bprintf buf "\\<td class=\\\"sr\\\"\\>
\\<a href=\\\"http://mldonkey.sourceforge.net/%s\\\"\\>%s\\</a\\>\\</td\\>
\\<td class=\\\"sr\\\"\\>\\<form action=\\\"submit\\\" target=\\\"$S\\\" onsubmit=\\\"javascript: {setTimeout('window.location.replace(window.location.href)',500);}\\\"\\>
\\<input type=hidden name=setoption value=q\\>\\<input type=hidden name=option value=%s\\>"
                  (String2.upp_initial o.option_name) o.option_name o.option_name;

          if o.option_value = "true" || o.option_value = "false" then
            Printf.bprintf buf "\\<select style=\\\"font-family: verdana; font-size: 10px;\\\"
                                name=\\\"value\\\" onchange=\\\"this.form.submit()\\\"\\>
                                \\<option selected\\>%s\\<option\\>%s\\</select\\>"
                                o.option_value (if o.option_value="true" then "false" else "true")
          else
            Printf.bprintf buf "\\<input style=\\\"font-family: verdana; font-size: 10px;\\\"
                                type=text name=value onchange=\\\"track_changed(this)\\\" size=20 value=\\\"%s\\\"\\>"
                                o.option_value;
        end;
        Printf.bprintf buf "\\</td\\>\\</form\\>\\<td class=\\\"sr\\\"\\>%s\\</td\\>" (shorten o.option_default 40);
        Printf.bprintf buf "\\<td class=\\\"sr\\\"\\>%s\\</td\\>"
          ((if o.option_restart then Printf.sprintf "restart " else "") ^
           (if o.option_internal then Printf.sprintf "internal " else "") ^
           (if o.option_public then Printf.sprintf "public " else ""));
        if not !!html_mods_use_js_helptext then
          Printf.bprintf buf "\\<td class=\\\"sr\\\"\\>%s\\</td\\>" (Str.global_replace (Str.regexp "\n") "\\<br\\>" o.option_help);
        Printf.bprintf buf "\\</tr\\>"

  )list;
  Printf.bprintf  buf "\\</table\\>\\</div\\>"


let list_options oo list =
  let buf = oo.conn_buf in
  if oo.conn_output = HTML then
    Printf.bprintf  buf "\\<table border=0\\>";
  List.iter (fun o ->
      if String.contains o.option_value '\n' then begin
          if oo.conn_output = HTML then
            Printf.bprintf buf "
                  \\<tr\\>\\<td\\>\\<form action=\\\"submit\\\" $S\\>
                  \\<input type=hidden name=setoption value=q\\>
                  \\<input type=hidden name=option value=%s\\> %s \\</td\\>\\<td\\>
                  \\<textarea name=value rows=10 cols=70 wrap=virtual\\>
                  %s
                  \\</textarea\\>
                  \\<input type=submit value=Modify\\>
                  \\</td\\>\\</tr\\>
                  \\</form\\>
                  " o.option_name o.option_name o.option_value
        end
      else
      if oo.conn_output = HTML then
        Printf.bprintf buf "
              \\<tr\\>\\<td\\>\\<form action=\\\"submit\\\" $S\\>
\\<input type=hidden name=setoption value=q\\>
\\<input type=hidden name=option value=%s\\> %s \\</td\\>\\<td\\>
              \\<input type=text name=value onchange=\\\"track_changed(this)\\\" size=40 value=\\\"%s\\\"\\>
\\</td\\>\\</tr\\>
\\</form\\>
              " o.option_name o.option_name o.option_value
      else
        Printf.bprintf buf "$b%s$n = $r%s$n\n" o.option_name o.option_value)
  list;
  if oo.conn_output = HTML then
    Printf.bprintf  buf "\\</table\\>"

let list_calendar o list =
  let buf = o.conn_buf in
  if o.conn_output = HTML then begin
      html_mods_table_header buf "web_infoTable" "vo" [
        ( Str, "srh", "Weekdays", "Weekdays" ) ;
        ( Str, "srh", "Hours", "Hours" ) ;
        ( Str, "srh", "Command", "Command" ) ] ;
      html_mods_cntr_init ();
      List.iter (fun (wdays, hours, command) ->
          Printf.bprintf buf "\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr ());
          let wdays_string = ref "" in
          let hours_string = ref "" in
          List.iter (fun day ->
              if !wdays_string = "" then
                wdays_string := string_of_int day
              else
                wdays_string := Printf.sprintf "%s %s" !wdays_string (string_of_int day)) wdays;
          List.iter (fun hour ->
              if !hours_string = "" then
                hours_string := string_of_int hour
              else
                hours_string := Printf.sprintf "%s %s" !hours_string (string_of_int hour)) hours;
          Printf.bprintf buf "
              \\<td title=\\\"%s\\\" class=\\\"sr\\\"\\>%s\\</td\\>
              \\<td class=\\\"sr\\\"\\>%s\\</td\\>" command !wdays_string !hours_string;
          Printf.bprintf buf "
              \\<td class=\\\"sr\\\"\\>%s\\</td\\>
              \\</tr\\>" command
      ) list;
      Printf.bprintf buf "\\</table\\>\\</div\\>"
    end
  else begin
      Printf.bprintf buf "weekdays / hours / command :\n";
      List.iter (fun (wdays, hours, command) ->
          let wdays_string = ref "" in
          let hours_string = ref "" in
          List.iter (fun day ->
              if !wdays_string = "" then
                wdays_string := string_of_int day
              else
                wdays_string := Printf.sprintf "%s %s" !wdays_string (string_of_int day)) wdays;
          List.iter (fun hour ->
              if !hours_string = "" then
                hours_string := string_of_int hour
              else
                hours_string := Printf.sprintf "%s %s" !hours_string (string_of_int hour)) hours;
          Printf.bprintf buf "%s\n%s\n%s\n" !wdays_string !hours_string command
      )list
    end

(*** Note: don't add _s to all command description as it is already done here *)

let register_commands section list =
  register_commands
    (List2.tail_map
      (fun (cmd, action, desc) -> (cmd, section, action, _s desc)) list)


(*************************************************************************)
(*                                                                       *)
(*                         Driver/General                                *)
(*                                                                       *)
(*************************************************************************)


let _ =
  register_commands "Driver/General"
    [

    "dump_heap", Arg_none (fun o ->
(*        Gc.dump_heap (); *)
        "heap dumped"
    ), ":\t\t\t\tdump heap for debug";

    "alias", Arg_multiple ( fun args o ->
        let out = ref "" in
        if List.length args = 0 then begin
          out := "List of aliases\n\n";
          List.iter (
            fun (a,b) ->
              out := !out ^ a ^ " -> " ^ b ^ "\n"
          ) !!alias_commands;
        end
        else begin
          match args with
              [] | [_] -> out := "Too few arguments"
            | al::def ->
                (try
                   let old_def = List.assoc al !!alias_commands in
                   out := "removing " ^ al ^ " -> " ^ old_def ^ "\n";
                   alias_commands =:= List.remove_assoc al !!alias_commands;
                 with _ -> ());

                let definition = String.concat " " def in
                alias_commands =:=  (al,definition) :: !!alias_commands;
                out := !out ^ "Alias added";
        end;

        !out
    ), ":\t\t\t\t\t$badd a command alias\n"
       ^"\t\t\t\t\tfor example: \"alias ca cancel all\" makes an alias\n"
       ^"\t\t\t\t\t\"ca\" performing \"cancel all\"\n"
       ^"\t\t\t\t\tto substitute an alias just make a new one\n"
       ^"\t\t\t\t\tfor example: \"alias ca vd\"$n";


    "unalias", Arg_one (
      fun arg o ->
        (try
           let old_def = List.assoc arg !!alias_commands in
           alias_commands =:= List.remove_assoc arg !!alias_commands;
           "removing " ^ arg ^ " -> " ^ old_def
         with _ -> "Alias not found");

    ), ":\t\t\t\t$bdelete a command alias\n"
       ^"\t\t\t\t\texample: \"unalias ca\"$n";

    "q", Arg_none (fun o ->
        raise CommonTypes.CommandCloseSocket
    ), ":\t\t\t\t\t$bclose telnet$n";
    
    "logout", Arg_none (fun o ->
        let buf = o.conn_buf in
        if o.conn_output = HTML then begin
          if has_empty_password o.conn_user.ui_user then
            print_command_result o "logout not required, your password is empty!"
          else begin
            if use_html_mods o then begin
              html_mods_table_header buf "helpTable" "results" [];
              Buffer.add_string buf "\\<tr class=\\\"dl-1\\\"\\>";
              html_mods_td buf [("", "sr", "Are you sure?"); ];
              Buffer.add_string buf "\\</tr\\>\\<tr class=\\\"dl-1\\\"\\>";
              html_mods_td buf [("", "sr", "\\<div align=\\\"center\\\"\\>\\<a href=\\\"logout\\\" target=\\\"_parent\\\"\\>yes\\</a\\>\\</div\\>"); ];
              Buffer.add_string buf "\\</tr\\>\\</table\\>\\</div\\>";
            end else
              Printf.bprintf buf "Are you sure? \\<a href=\\\"logout\\\" target=\\\"_parent\\\"\\>yes\\</a\\>"
          end
        end else
          raise CommonTypes.CommandCloseSocket;
      ""
    ), ":\t\t\tlogout interface";      

    "kill", Arg_none (fun o ->
        if user2_is_admin o.conn_user.ui_user then
          begin
            CommonInteractive.clean_exit 0;
            _s "exit"
          end
        else
          _s "You are not allowed to kill MLDonkey"
        ), ":\t\t\t\t\t$bsave and kill the server$n";

    "urladd", Arg_multiple (fun args o ->
       let (kind, url, period) = match args with
          | [kind; url; period] -> kind, url, int_of_string period
          | [kind; url] -> kind, url, 0
          | _  -> failwith "Bad number of arguments"
        in
        web_infos_add kind period url;
        (match web_infos_find url with
        | None -> ()
        | Some w -> CommonWeb.load_url true w);
        "url added to web_infos. downloading now"
    ), "<kind> <url> [<period>] :\tload this file from the web\n"
       ^"\t\t\t\t\tkind is either server.met (if the downloaded file is a server.met)\n"
       ^"\t\t\t\t\tperiod is the period between updates (in hours, default 0 = only loaded at startup)";

    "urlremove", Arg_one (fun url o ->
        match web_infos_find url with
        | None -> "URL does not exists in web_infos"
        | Some w -> web_infos_remove w.url;
                      "removed URL from web_infos"
    ), "<url> :\t\t\tremove URL from web_infos";

    "force_web_infos", Arg_multiple (fun args o ->
        (match args with
        | [] -> CommonWeb.load_web_infos false true;
                "requesting all web_infos files"
        | args -> let list = ref [] in
                  List.iter (fun arg ->
                    Hashtbl.iter (fun key w ->
                      if w.kind = arg || w.url = arg then begin
                        CommonWeb.load_url false w;
                        list := arg :: !list
                      end
                  ) web_infos_table) args;
                  if !list = [] then
                    Printf.sprintf "found no web_infos entries for %s" (String.concat " " args)
                  else
                    Printf.sprintf "requesting web_infos %s" (String.concat " " !list))
    ), "[<list of kind|URL>] :\tre-download web_infos, leave empty to re-download all";

    "recover_temp", Arg_none (fun o ->
        networks_iter (fun r ->
            try
              CommonNetwork.network_recover_temp r
            with _ -> ()
        );
        let buf = o.conn_buf in
        if o.conn_output = HTML then
          html_mods_table_one_row buf "serversTable" "servers" [
            ("", "srh", "Recover temp finished"); ]
        else
          Printf.bprintf buf "Recover temp finished";
        _s ""
    ), ":\t\t\t\trecover lost files from temp directory";

    "vc", Arg_multiple (fun args o ->
        let buf = o.conn_buf in
        if args = ["all"] then begin
            if use_html_mods o then html_mods_table_header buf "vcTable" "vc" ([
                ( Num, "srh ac", "Client number", "Num" ) ;
                ( Str, "srh", "Network", "Network" ) ;
                ( Str, "srh", "IP address", "IP address" ) ;
                ] @ (if Geoip.active () then [( Str, "srh", "Country Code/Name", "CC" )] else []) @ [
                ( Str, "srh", "Client name", "Client name" ) ;
                ( Str, "srh", "Client brand", "CB" ) ;
                ( Str, "srh", "Client release", "CR" ) ;
                ] @
                (if !!emule_mods_count then [( Str, "srh", "eMule MOD", "EM" )] else [])
                @ [
                ( Str, "srh", "Client file queue", "Q" ) ;
                ( Num, "srh ar", "Total UL bytes to this client for all files", "tUL" ) ;
                ( Num, "srh ar br", "Total DL bytes from this client for all files", "tDL" ) ;
                ( Num, "srh ar", "Session UL bytes to this client for all files", "sUL" ) ;
                ( Num, "srh ar", "Session DL bytes from this client for all files", "sDL" )]);

            html_mods_cntr_init ();
            let all_clients_list = clients_get_all () in
            List.iter (fun num ->
                let c = client_find num in
                let i = client_info c in
                if use_html_mods o then Printf.bprintf buf "\\<tr class=\\\"dl-%d\\\"
                  title=\\\"Add as friend\\\"
                  onClick=\\\"parent.fstatus.location.href='submit?q=friend_add+%d'\\\"
                  onMouseOver=\\\"mOvr(this);\\\"
                  onMouseOut=\\\"mOut(this);\\\"\\>"
                    (html_mods_cntr ()) num;
                    client_print c o;
                    if use_html_mods o then
                    html_mods_td buf ([
                     (client_software i.client_software i.client_os, "sr", client_software_short i.client_software i.client_os);
                     ("", "sr", i.client_release);
                     ] @
                     (if !!emule_mods_count then [("", "sr", i.client_emulemod)] else [])
                     @ [
                     ("", "sr", Printf.sprintf "%d" (List.length i.client_file_queue));
                     ("", "sr ar", (size_of_int64 i.client_total_uploaded));
                     ("", "sr ar br", (size_of_int64 i.client_total_downloaded));
                     ("", "sr ar", (size_of_int64 i.client_session_uploaded));
                     ("", "sr ar", (size_of_int64 i.client_session_downloaded))]);
                if use_html_mods o then Printf.bprintf buf "\\</tr\\>"
                else Printf.bprintf buf "\n";
            ) all_clients_list;
            if use_html_mods o then Printf.bprintf buf "\\</table\\>\\</div\\>";
          end
        else
          List.iter (fun num ->
              let num = int_of_string num in
              let c = client_find num in
              try client_print_info c o with e -> print_command_result o (Printexc2.to_string e);
          ) args;
        ""
    ), "<num|all> :\t\t\t\tview client (use arg 'all' for all clients)";

    "version", Arg_none (fun o ->
        print_command_result o (CommonGlobals.version ());
        ""
    ), ":\t\t\t\tprint mldonkey version";

    "uptime", Arg_none (fun o ->
        print_command_result o (log_time () ^ "- up " ^
          Date.time_to_string (last_time () - start_time) "verbose");
        ""
    ), ":\t\t\t\tcore uptime";

    "sysinfo", Arg_none (fun o ->
        let buf = o.conn_buf in
        ignore(buildinfo (o.conn_output = HTML) buf);
        if o.conn_output = HTML then Printf.bprintf buf "\\<P\\>";
        ignore(runinfo (o.conn_output = HTML) buf o);
        if o.conn_output = HTML then Printf.bprintf buf "\\<P\\>";
        ignore(portinfo (o.conn_output = HTML) buf);
        if o.conn_output = HTML then Printf.bprintf buf "\\<P\\>";
        ignore(diskinfo (o.conn_output = HTML) buf);
        ""
    ), ":\t\t\t\tprint mldonkey core build, runtime and disk information";

    "buildinfo", Arg_none (fun o ->
        let buf = o.conn_buf in
        ignore(buildinfo (o.conn_output = HTML) buf);
        ""
    ), ":\t\t\t\tprint mldonkey core build information";

    "runinfo", Arg_none (fun o ->
        let buf = o.conn_buf in
        ignore(runinfo (o.conn_output = HTML) buf o);
        ""
    ), ":\t\t\t\tprint mldonkey runtime information";

    "portinfo", Arg_none (fun o ->
        let buf = o.conn_buf in
        ignore(portinfo (o.conn_output = HTML) buf);
        ""
    ), ":\t\t\t\tprint mldonkey port usage information";

    "diskinfo", Arg_none (fun o ->
        let buf = o.conn_buf in
        ignore(diskinfo (o.conn_output = HTML) buf);
        ""
    ), ":\t\t\t\tprint mldonkey disk information";

    "activity", Arg_one (fun arg o ->
        let arg = int_of_string arg in
        let buf = o.conn_buf in
        let activity_begin = last_time () - arg * 60 in
        Fifo.iter (fun a ->
            if a.activity_begin > activity_begin then begin
                Printf.bprintf buf "%s: activity =\n" (BasicSocket.string_of_date a.activity_begin);
                Printf.bprintf buf "   servers: edonkey %03d/%03d\n"
                  a.activity_server_edonkey_successful_connections
                  a.activity_server_edonkey_connections;
                Printf.bprintf buf "   clients: overnet %03d/%03d edonkey %03d/%03d\n"
                  a.activity_client_overnet_successful_connections
                  a.activity_client_overnet_connections
                  a.activity_client_edonkey_successful_connections
                  a.activity_client_edonkey_connections;
                Printf.bprintf buf "   indirect: overnet %03d edonkey %03d\n"
                  a.activity_client_overnet_indirect_connections
                  a.activity_client_edonkey_indirect_connections;
              end
        ) activities;
        ""
    ), "<minutes> :\t\t\tprint activity in the last <minutes> minutes";

    "clear_message_log", Arg_none (fun o ->
        Fifo.clear chat_message_fifo;
        Printf.sprintf "Chat messages cleared"
     ), ":\t\t\t\tclear chat message buffer";

    "message_log", Arg_multiple (fun args o ->
        let buf = o.conn_buf in
        html_mods_cntr_init ();

        (match args with
            [arg] ->
              let refresh_delay = int_of_string arg in
              if use_html_mods o && refresh_delay > 1 then
                Printf.bprintf buf "\\<meta http-equiv=\\\"refresh\\\" content=\\\"%d\\\"\\>"
                  refresh_delay;
          | _ -> ());

(* rely on GC? *)

        while (Fifo.length chat_message_fifo) > !!html_mods_max_messages  do
          ignore (Fifo.take chat_message_fifo)
        done;

        if use_html_mods o then Printf.bprintf buf "\\<div class=\\\"messages\\\"\\>";

        last_message_log := last_time();
        Printf.bprintf buf "%d logged messages\n" (Fifo.length chat_message_fifo);

        if Fifo.length chat_message_fifo > 0 then
          begin

            if use_html_mods o then
              html_mods_table_header buf "serversTable" "servers" [
                ( Str, "srh", "Timestamp", "Time" ) ;
                ( Str, "srh", "IP address", "IP address" ) ;
                ( Num, "srh", "Client number", "Num" ) ;
                ( Str, "srh", "Client name", "Client name" ) ;
                ( Str, "srh", "Message text", "Message" ) ] ;

            List.iter (fun (t,i,num,n,s) ->
                if use_html_mods o then begin
                    Printf.bprintf buf "\\<tr class=\\\"dl-%d\\\"\\>"
                      (html_mods_cntr ());
                    html_mods_td buf [
                      ("", "sr", Date.simple (BasicSocket.date_of_int t));
                      ("", "sr",  i);
                      ("", "sr", Printf.sprintf "%d" num);
                      ("", "sr", n);
                      ("", "srw", (if String.length s > 11 && String.sub s 0 11 = "data:image/" then
                          "\\<img src=\\\"" ^ String.escaped s ^ "\\\">"
                        else String.escaped s)) ];
                    Printf.bprintf buf "\\</tr\\>"
                  end
                else
                  Printf.bprintf buf "\n%s [client #%d] %s(%s): %s\n"
                    (Date.simple (BasicSocket.date_of_int t)) num n i s;
            ) (List.rev (Fifo.to_list chat_message_fifo));
            if use_html_mods o then Printf.bprintf buf
                "\\</table\\>\\</div\\>\\</div\\>";

          end;

        ""
    ), ":\t\t\t\tmessage_log [refresh delay in seconds]";

    "message", Arg_multiple (fun args o ->
        let buf = o.conn_buf in
        match args with
          n :: msglist ->
            let msg = List.fold_left (fun a1 a2 ->
                  a1 ^ a2 ^ " "
              ) "" msglist in
            let cnum = int_of_string n in
            let c = client_find cnum in
            let g = client_info c in
            client_say c msg;
            log_chat_message "FROM ME" cnum ("TO: " ^ g.client_name) msg;
            Printf.sprintf "Sending msg to client #%d: %s" cnum msg;
        | _ ->
            if use_html_mods o then begin

                Printf.bprintf buf "\\<script type=\\\"text/javascript\\\"\\>
\\<!--
function submitMessageForm() {
var formID = document.getElementById(\\\"msgForm\\\")
var regExp = new RegExp (' ', 'gi') ;
var msgTextOut = formID.msgText.value.replace(regExp, '+');
parent.fstatus.location.href='submit?q=message+'+formID.clientNum.value+\\\"+\\\"+msgTextOut;
formID.msgText.value=\\\"\\\";
}
//--\\>
\\</script\\>";

                Printf.bprintf buf "\\<iframe id=\\\"msgWindow\\\" name=\\\"msgWindow\\\" height=\\\"80%%\\\"
            width=\\\"100%%\\\" scrolling=yes src=\\\"submit?q=message_log+20\\\"\\>\\</iframe\\>";

                Printf.bprintf buf "\\<form style=\\\"margin: 0px;\\\" name=\\\"msgForm\\\" id=\\\"msgForm\\\" action=\\\"javascript:submitMessageForm();\\\"\\>";
                Printf.bprintf buf "\\<table width=100%% cellspacing=0 cellpadding=0 border=0\\>\\<tr\\>\\<td\\>";
                Printf.bprintf buf "\\<select style=\\\"font-family: verdana;
            font-size: 12px; width: 150px;\\\" id=\\\"clientNum\\\" name=\\\"clientNum\\\" \\>";

                Printf.bprintf buf "\\<option value=\\\"1\\\"\\>Client/Friend list\n";

                let found_nums = ref [] in
                let fifo_list = Fifo.to_list chat_message_fifo in
                let fifo_list = List.rev fifo_list in
                let found_select = ref 0 in
                List.iter (fun (t,i,num,n,s) ->
                    if not (List.mem num !found_nums) then begin

                        found_nums := num :: !found_nums;
                        Printf.bprintf buf "\\<option value=\\\"%d\\\" %s\\>%d:%s\n"
                          num
                          (if !found_select=0 then "selected" else "";)
                        num (try
                            let c = client_find num in
                            let g = client_info c in
                            g.client_name
                          with _ -> "unknown/expired");
                        found_select := 1;
                      end
                ) fifo_list;
                List.iter (fun c ->
                    let g = client_info c in
                    if not (List.mem g.client_num !found_nums) then begin
                        found_nums := g.client_num :: !found_nums;
                        Printf.bprintf buf "\\<option value=\\\"%d\\\"\\>%d:%s\n"
                          g.client_num g.client_num g.client_name;
                      end
                ) !!friends;

                Printf.bprintf buf "\\</select\\>\\</td\\>";
                Printf.bprintf buf "\\<td width=100%%\\>\\<input style=\\\"width: 99%%; font-family: verdana; font-size: 12px;\\\"
                type=text id=\\\"msgText\\\" name=\\\"msgText\\\" size=50 \\>\\</td\\>";
                Printf.bprintf buf "\\<td\\>\\<input style=\\\"font-family: verdana;
            font-size: 12px;\\\" type=submit value=\\\"Send\\\"\\>\\</td\\>\\</form\\>";
                Printf.bprintf buf "\\<form style=\\\"margin: 0px;\\\" id=\\\"refresh\\\" name=\\\"refresh\\\"
            action=\\\"javascript:msgWindow.location.reload();\\\"\\>
            \\<td\\>\\<input style=\\\"font-family: verdana; font-size: 12px;\\\" type=submit
            Value=\\\"Refresh\\\"\\>\\</td\\>\\</form\\>";
                Printf.bprintf buf "\\<form style=\\\"margin: 0px;\\\" id=\\\"clear\\\" name=\\\"clear\\\"
            action=\\\"javascript:window.location.href='submit?q=clear_message_log'\\\"\\>
            \\<td\\>\\<input style=\\\"font-family: verdana; font-size: 12px;\\\" type=submit
            Value=\\\"Clear\\\"\\>\\</td\\>\\</form\\>\\</tr\\>\\</table\\>";
                ""
              end
            else
              _s "Usage: message <client num> <msg>\n";

    ), "<client num> <msg> :\t\tsend a message to a client";

    "calendar_add", Arg_two (fun hour action o ->
        let buf = o.conn_buf in
        calendar =:= ([0;1;2;3;4;5;6], [int_of_string hour], action)
        :: !!calendar;
        if use_html_mods o then
          html_mods_table_one_row buf "serversTable" "servers" [
            ("", "srh", "action added"); ]
        else
          Printf.bprintf buf "action added";
        _s ""
    ), "<hour> \"<command>\" :\tadd a command to be executed every day";

    "vcal", Arg_none (fun o ->
        let buf = o.conn_buf in
        if use_html_mods o then begin
            Printf.bprintf buf "\\<div class=\\\"vo\\\"\\>
                \\<table class=main cellspacing=0 cellpadding=0\\>\\<tr\\>\\<td\\>";
            if !!calendar = [] then
              html_mods_table_one_row buf "serversTable" "servers" [
                ("", "srh", "no jobs defined"); ]
            else
              list_calendar o !!calendar;
            Printf.bprintf buf "\\</td\\>\\</tr\\>\\</table\\>\\</div\\>\\<P\\>";
            print_option_help o calendar
          end
        else
          if List.length !!calendar = 0 then
            Printf.bprintf buf "no jobs defined"
          else
            list_calendar o !!calendar;
        ""
    ), ":\t\t\t\t\tprint calendar";

    ]

(*************************************************************************)
(*                                                                       *)
(*                         Driver/Servers                                *)
(*                                                                       *)
(*************************************************************************)

let _ =
  register_commands "Driver/Servers"
    [

    "vm", Arg_none (fun o ->
        let buf = o.conn_buf in
        if use_html_mods o then Printf.bprintf buf
            "\\<div class=\\\"servers\\\"\\>\\<table align=center border=0 cellspacing=0 cellpadding=0\\>\\<tr\\>\\<td\\>";
        CommonInteractive.print_connected_servers o;
        if use_html_mods o then Printf.bprintf buf "\\</td\\>\\</tr\\>\\</table\\>\\</div\\>";
        ""), ":\t\t\t\t\t$blist connected servers$n";

    "vma", Arg_none (fun o ->
        let buf = o.conn_buf in
        html_mods_cntr_init ();
        let nb_servers = ref 0 in
        if use_html_mods o then server_print_html_header buf "";
        Intmap.iter (fun _ s ->
            try
              server_print s o;
              incr nb_servers;
            with e ->
                lprintf "Exception %s in server_print\n"
                  (Printexc2.to_string e);
        ) !!servers;
        if use_html_mods o then begin
            Printf.bprintf buf "\\</table\\>\\</div\\>";
            html_mods_table_one_row buf "serversTable" "servers" [
              ("", "srh", Printf.sprintf "Servers: %d known" !nb_servers); ]
          end
        else
          Printf.bprintf buf "Servers: %d known\n" !nb_servers;
        if Autoconf.donkey = "yes" && not !!enable_servers then
          begin
            if use_html_mods o then begin
                Printf.bprintf buf "\\<div class=servers\\>";
                html_mods_table_one_row buf "upstatsTable" "upstats" [
                  ("", "srh", ("You disabled server usage, therefore you are not" ^
                    " able to connect ED2K servers. " ^
                    "To use servers again 'set enable_servers true'")); ]
              end
            else
              Buffer.add_string buf ("You disabled server usage, therefore you are not" ^
                " able to connect ED2K servers.\n" ^
                "To use servers again 'set enable_servers true'\n");
            if use_html_mods o then Printf.bprintf buf "\\</div\\>"
          end;
    ""), ":\t\t\t\t\tlist all known servers";

    "rem", Arg_multiple (fun args o ->
        let counter = ref 0 in
        match args with
          ["all"] ->
            Intmap.iter ( fun _ s ->
              server_remove s;
              incr counter
            ) !!servers;
            Printf.sprintf (_b "Removed all %d servers") !counter
        | ["blocked"] ->
            Intmap.iter ( fun _ s ->
              if server_blocked s then
                begin
                  server_remove s;
                  incr counter
                end
            ) !!servers;
            Printf.sprintf (_b "Removed %d blocked servers") !counter
        | ["disc"] ->
            Intmap.iter (fun _ s ->
              match server_state s with
                NotConnected _ ->
                  begin
                    server_remove s;
                    incr counter
                  end
              | _ -> ()) !!servers;
            Printf.sprintf (_b "Removed %d disconnected servers") !counter
        | _ ->
            List.iter (fun num ->
                let num = int_of_string num in
                let s = server_find num in
                server_remove s
            ) args;
            Printf.sprintf (_b"%d servers removed") (List.length args)
    ), "<server numbers|all|blocked|disc> :\tremove server(s) ('all'/'blocked'/'disc' = all/IP blocked/disconnected servers)";

    "server_banner", Arg_one (fun num o ->
        let num = int_of_string num in
        let s = server_find num in
        (match server_state s with
            NotConnected _ -> ()
          | _ ->   server_banner s o);
        ""
    ), "<num> :\t\t\tprint banner of connected server <num>";

    "server_shares", Arg_one (fun num o ->
        if user2_is_admin o.conn_user.ui_user then
        let s = server_find (int_of_string num) in
        (match server_state s with
           Connected _ -> let list = ref [] in
            List.iter (fun f -> 
              match file_shared f with
                None -> ()
              | Some sh -> list := (as_shared_impl sh) :: !list)
              (server_published s);
            print_upstats o !list (Some s)
          | _ -> ()
        )
        else print_command_result o "You are not allowed to use this command";
        _s ""
    ), "<num> :\t\t\tshow list of files published on server <num>";

    "c", Arg_multiple (fun args o ->
        let buf = o.conn_buf in
        match args with
          [] ->
            networks_iter network_connect_servers;
            if o.conn_output = HTML then
              html_mods_table_one_row buf "serversTable" "servers" [
                ("", "srh", "Connecting more servers"); ]
            else
              Printf.bprintf buf "connecting more servers";
            _s
        ""
        | _ ->
            List.iter (fun num ->
                let num = int_of_string num in
                let s = server_find num in
                server_connect s
            ) args;
            if o.conn_output = HTML then
              html_mods_table_one_row buf "serversTable" "servers" [
                ("", "srh", "Connecting more servers"); ]
            else
              Printf.bprintf buf "connecting server";
            _s
        ""
    ), "[<num>] :\t\t\t\tconnect to more servers (or to server <num>)";

    "x", Arg_multiple (fun args o ->
        let counter = ref 0 in
        let is_connected state =
          match state with
          | Connecting
          | Connected _
          | Connected_initiating -> true
          | _ -> false
        in
        let print_result v =
            print_command_result o
             (Printf.sprintf (_b "Disconnected %d server%s") !counter (Printf2.print_plural_s !counter))
        in
        match args with
        | ["all"] ->
            Intmap.iter ( fun _ s ->
              if is_connected (server_state s) then begin
                server_disconnect s;
                incr counter
              end
            ) !!servers;
            print_result !counter;
        ""
        | _ ->
            List.iter (fun num ->
                let num = int_of_string num in
                let s = server_find num in
                if is_connected (server_state s) then begin
                  server_disconnect s;
                  incr counter
                end
            ) args;
            print_result !counter;
        ""
    ), "<server numbers|all> :\t\tdisconnect from server(s)";

  ]

(*************************************************************************)
(*                                                                       *)
(*                         Driver/Friends                                *)
(*                                                                       *)
(*************************************************************************)

let _ =
  register_commands "Driver/Friends"
    [

    "vfr", Arg_none (fun o ->
        List.iter (fun c ->
            client_print c o) !!friends;
        ""
    ), ":\t\t\t\t\tview friends";

    "gfr", Arg_one (fun num o ->
        let num = int_of_string num in
        let c = client_find num in
        client_browse c true;
        _s "client browse"
    ), "<client num> :\t\t\task friend files";

    "friend_add", Arg_one (fun num o ->
        let num = int_of_string num in
        let c = client_find num in
        friend_add c;
        _s "Added friend"
    ), "<client num> :\t\tadd client <client num> to friends";

    "friend_remove", Arg_multiple (fun args o ->
        if args = ["all"] then begin
            List.iter (fun c ->
                friend_remove c
            ) !!friends;
            _s "Removed all friends"
          end else begin
            List.iter (fun num ->
                let num = int_of_string num in
                let c = client_find num in
                friend_remove c;
            ) args;
            Printf.sprintf (_b "%d friends removed") (List.length args)
          end
    ), "<client numbers|all> :\tremove friend (use arg 'all' for all friends)";

    "friends", Arg_none (fun o ->
        let buf = o.conn_buf in

        if use_html_mods o then begin
            Printf.bprintf buf "\\<div class=\\\"friends\\\"\\>\\<table class=main cellspacing=0 cellpadding=0\\>
\\<tr\\>\\<td\\>
\\<table cellspacing=0 cellpadding=0  width=100%%\\>\\<tr\\>
\\<td class=downloaded width=100%%\\>\\</td\\>
\\<td nowrap class=fbig\\>\\<a onclick=\\\"javascript:window.location.reload()\\\"\\>Refresh\\</a\\> \\</td\\>
\\<td nowrap class=fbig\\>\\<a onclick=\\\"javascript:
                  { parent.fstatus.location.href='submit?q=friend_remove+all';
                    setTimeout('window.location.reload()',1000);
                    }\\\"\\>Remove All\\</a\\>
\\</td\\>
\\<td nowrap class=\\\"fbig pr\\\"\\>\\<a onclick=\\\"javascript: {
                   var getip = prompt('Friend IP [port] ie: 192.168.0.1 4662','192.168.0.1 4662')
                   var reg = new RegExp (' ', 'gi') ;
                   var outstr = getip.replace(reg, '+');
                   parent.fstatus.location.href='submit?q=afr+' + outstr;
                    setTimeout('window.location.reload()',1000);
                    }\\\"\\>Add by IP\\</a\\>
\\</td\\>
\\</tr\\>\\</table\\>
\\</td\\>\\</tr\\>
\\<tr\\>\\<td\\>";
            html_mods_table_header buf "friendsTable" "friends" [
              ( Num, "srh", "Client number", "Num" ) ;
              ( Str, "srh", "Remove", "Remove" ) ;
              ( Str, "srh", "Network", "Network" ) ;
              ( Str, "srh", "Name", "Name" ) ;
              ( Str, "srh", "State", "State" ) ] ;
          end;
        html_mods_cntr_init ();
        List.iter (fun c ->
            let i = client_info c in
            let n = network_find_by_num i.client_network in
            if use_html_mods o then
              begin

                Printf.bprintf buf "\\<tr class=\\\"dl-%d\\\"
                onMouseOver=\\\"mOvr(this);\\\"
                onMouseOut=\\\"mOut(this);\\\"\\>"
                  (html_mods_cntr ());

                Printf.bprintf buf "
                        \\<td title=\\\"Client number\\\"
                        onClick=\\\"location.href='submit?q=files+%d'\\\"
                        class=\\\"srb\\\"\\>%d\\</td\\>
                        \\<td title=\\\"Remove friend\\\"
                        onClick=\\\"parent.fstatus.location.href='submit?q=friend_remove+%d'\\\"
                        class=\\\"srb\\\"\\>Remove\\</td\\>
                        \\<td title=\\\"Network\\\" class=\\\"sr\\\"\\>%s\\</td\\>
                        \\<td title=\\\"Name (click to view files)\\\"
                        onClick=\\\"location.href='submit?q=files+%d'\\\"
                        class=\\\"sr\\\"\\>%s\\</td\\>
                        \\<td title=\\\"Click to view files\\\"
            onClick=\\\"location.href='submit?q=files+%d'\\\"
            class=\\\"sr\\\"\\>%s\\</td\\>
                        \\</tr\\>"
                  i.client_num
                  i.client_num
                  i.client_num
                  n.network_name
                  i.client_num
                  i.client_name
                  i.client_num

                  (let rs = try client_files c with _ -> [] in
                  if (List.length rs) > 0 then Printf.sprintf "%d Files Listed" (List.length rs)
                  else string_of_connection_state (client_state c) )

              end

            else
              Printf.bprintf buf "[%s %d] %s" n.network_name
                i.client_num i.client_name
        ) !!friends;

        if use_html_mods o then
          Printf.bprintf buf " \\</table\\>\\</td\\>\\<tr\\>\\</table\\>\\</div\\>";

        ""
    ), ":\t\t\t\tdisplay all friends";

    "files", Arg_one (fun arg o ->
        let buf = o.conn_buf in
        let n = int_of_string arg in
        List.iter (fun c ->
            if client_num c = n then begin
                let rs = client_files c in

                let rs = List2.tail_map (fun (s, rs) ->
                      let r = IndexedResults.get_result rs in
                      rs, r, 1
                  ) rs in
                o.conn_user.ui_last_results <- [];
                DriverInteractive.print_results 0 buf o rs;

                ()
              end
        ) !!friends;
        ""), "<client num> :\t\t\tprint files from friend <client num>";


  ]

(*************************************************************************)
(*                                                                       *)
(*                         Driver/Network                                *)
(*                                                                       *)
(*************************************************************************)

let _ =
  register_commands "Driver/Network"
    [

    "nu", Arg_one (fun num o ->
        if user2_is_admin o.conn_user.ui_user then begin
        let num = int_of_string num in

        if num > 0 then (* we want to disable upload for a short time *)
          let num = min !CommonGlobals.upload_credit num in
          CommonGlobals.has_upload := !CommonGlobals.has_upload + num;
          CommonGlobals.upload_credit := !CommonGlobals.upload_credit - num;
        else

        if num < 0 && !CommonGlobals.has_upload > 0 then begin
(* we want to restart upload probably *)
          let num = - num in
          let num = min num !CommonGlobals.has_upload in
          CommonGlobals.has_upload := !CommonGlobals.has_upload - num;
          CommonGlobals.upload_credit := !CommonGlobals.upload_credit + num;
        end;
        if !CommonGlobals.has_upload > 0 then clear_upload_slots ();
        print_command_result o
        (Printf.sprintf "upload disabled for %d minutes (remaining credits %d)"
          !CommonGlobals.has_upload !CommonGlobals.upload_credit)
        end else
          print_command_result o "You are not allowed to disable upload"; ""
    ), "<m> :\t\t\t\tdisable upload during <m> minutes, queue all files";

    "vu", Arg_none (fun o ->
        Printf.sprintf
          "Upload credits : %d minutes\nUpload disabled for %d minutes"
          !CommonGlobals.upload_credit !CommonGlobals.has_upload;

    ), ":\t\t\t\t\tview upload credits";

    "bw_stats", Arg_multiple (fun args o ->
        let buf = o.conn_buf in
        if use_html_mods o then
          begin
            display_bw_stats := true;
            let refresh_delay = ref !!html_mods_bw_refresh_delay in
            if args <> [] then begin
                let newrd = int_of_string (List.hd args) in
                if newrd > 1 then refresh_delay := newrd;
              end;

            let dlkbs =
              (( (float_of_int !udp_download_rate) +. (float_of_int !control_download_rate)) /. 1024.0) in
            let ulkbs =
              (( (float_of_int !udp_upload_rate) +. (float_of_int !control_upload_rate)) /. 1024.0) in

            Printf.bprintf buf "\\</head\\>\\<body\\>\\<div class=\\\"bw_stats\\\"\\>";
            Printf.bprintf buf "\\<table class=\\\"bw_stats\\\" cellspacing=0 cellpadding=0\\>\\<tr\\>";
            Printf.bprintf buf "\\<td\\>\\<table border=0 cellspacing=0 cellpadding=0\\>\\<tr\\>";

            html_mods_td buf [
              ("Download KB/s (UDP|TCP), total", "bu bbig bbig1 bb4", Printf.sprintf "Down: %.1f KB/s (%d|%d), %s"
                  dlkbs !udp_download_rate !control_download_rate (size_of_int64 !download_counter));
              ("Upload KB/s (UDP|TCP), total", "bu bbig bbig1 bb4", Printf.sprintf "Up: %.1f KB/s (%d|%d), %s"
                  ulkbs !udp_upload_rate !control_upload_rate (size_of_int64 !upload_counter));
              ("Total shared files/bytes", "bu bbig bbig1 bb4", Printf.sprintf "Shared(%d): %s"
                !nshared_files (size_of_int64 !nshared_bytes));
              ("Bandwidth (Up|Down), open connections (max_opened_connections)", "bu bbig bbig1 bb3",
                Printf.sprintf "DL %d KB/s, UL %d KB/s, Conn: %d (%d)"
                !!max_hard_download_rate !!max_hard_upload_rate (nb_sockets ()) !!max_opened_connections) ]; 

            Printf.bprintf buf "\\</tr\\>\\</table\\>\\</td\\>\\</tr\\>\\</table\\>\\</div\\>";

            Printf.bprintf buf "\\<script type=\\\"text/javascript\\\"\\>window.parent.document.title='(D:%.1f) (U:%.1f) | %s | %s'\\</script\\>"
              dlkbs ulkbs o.conn_user.ui_user.user_name (CommonGlobals.version ())
          end
        else
          DriverInteractive.print_bw_stats buf;
        ""
    ), ":\t\t\t\tprint current bandwidth stats";

    "bw_toggle", Arg_multiple (fun args o ->
        let change_bw () =
                let ul_bkp = !!max_hard_upload_rate_2 in
                let dl_bkp = !!max_hard_download_rate_2 in
                let max_conn = !!max_opened_connections_2 in
                max_hard_upload_rate_2 =:= !!max_hard_upload_rate;
                max_hard_download_rate_2 =:= !!max_hard_download_rate;
                max_opened_connections_2 =:= !!max_opened_connections;
                max_hard_upload_rate =:= ul_bkp;
                max_hard_download_rate =:=  dl_bkp;
                max_opened_connections =:= max_conn;
        in
        if user2_is_admin o.conn_user.ui_user then begin
        (
                match (List.map String.lowercase args) with
                | ["high"] ->
                        if !!max_opened_connections < !!max_opened_connections_2 then
                                change_bw ()
                | ["low"] ->
                        if !!max_opened_connections > !!max_opened_connections_2 then
                                change_bw ()
                | _ -> change_bw ()
                );
                print_command_result o (Printf.sprintf
                  "new upload rate: %d | new download rate: %d | new max opened connections: %d"
                    !!max_hard_upload_rate !!max_hard_download_rate !!max_opened_connections)
                end
        else
          print_command_result o "You are not allowed to toggle bandwidth";
        ""
    ), "[<high|low>]:\t\t\ttoggle between the two rate and opened connection sets, high/low depend on option max_open_connections*";

    "costats", Arg_multiple (fun args o ->
        let filter cs =
          match (List.map String.lowercase args) with
          | [] -> cs.country_total_upload <> 0L || cs.country_total_download <> 0L
          | ["all"] -> true
          | args ->
              let match_star = Str.regexp "\\*" in
              let regexp = Str.regexp ("^\\("
                ^ (List.fold_left (fun acc a -> acc
                ^ (if acc <> "" then "\\|" else "")
                ^ (Str.global_replace match_star ".*" a)) "" args)
                ^ "\\)$") in
              let check_string s =
                Str.string_match regexp (String.lowercase s) 0 in
              check_string cs.country_code ||
              check_string cs.country_name ||
              check_string cs.country_continent
        in
        let buf = o.conn_buf in
        if use_html_mods o then
          begin
            let u1 = BasicSocket.last_time () - !CommonStats.start_time in
            let u2 = (CommonStats.guptime () + u1) in
            let t1 = Printf.sprintf "Session uptime: %s" (Date.time_to_string u1 "verbose") in
            let t2 = Printf.sprintf "Total uptime: %s" (Date.time_to_string u2 "verbose") in
            html_mods_big_header_start buf "shares" [t1;t2];

            html_mods_table_header buf ~total:"1" "sharesTable" "shares" [
               ( Str, "srh", "Country name", "Country" ) ;
               ( Str, "srh", "Country code", "Code" ) ;
               ( Str, "srh", "Continent", "Con" ) ;
               ( Num, "srh ar", "Session uploaded", "sUl" ) ;
               ( Num, "srh ar", "Session downloaded", "sDl" ) ;
               ( Num, "srh ar", "Session seen", "sSe" ) ;
               ( Num, "srh ar", "Total uploaded", "tUl" ) ;
               ( Num, "srh ar", "Total downloaded", "tDl" ) ;
               ( Num, "srh ar", "Total seen", "tSe" ) ;
            ];
            html_mods_cntr_init ();
            let csu = ref 0L in
            let csd = ref 0L in
            let css = ref 0L in
            let ctu = ref 0L in
            let ctd = ref 0L in
            let cts = ref 0L in
            List.iter (fun cs ->
              if filter cs then begin
                Printf.bprintf buf "\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr ());
                html_mods_td buf [
                    ("", "sr", cs.country_name);
                    (cs.country_code, "sr", CommonPictures.flag_html cs.country_code);
                    ("", "sr", cs.country_continent);
                    ("", "sr ar", size_of_int64 cs.country_session_upload);
                    ("", "sr ar", size_of_int64 cs.country_session_download);
                    ("", "sr ar", Printf.sprintf "%Ld" cs.country_session_seen);
                    ("", "sr ar", size_of_int64 cs.country_total_upload);
                    ("", "sr ar", size_of_int64 cs.country_total_download);
                    ("", "sr ar", Printf.sprintf "%Ld" cs.country_total_seen);
                    ];
                Printf.bprintf buf "\\</tr\\>\n";
                csu := !csu ++ cs.country_session_upload;
                csd := !csd ++ cs.country_session_download;
                css := !css ++ cs.country_session_seen;
                ctu := !ctu ++ cs.country_total_upload;
                ctd := !ctd ++ cs.country_total_download;
                cts := !cts ++ cs.country_total_seen;
              end
              ) (List.sort (fun c1 c2 -> compare c1.country_code c2.country_code) !!CommonStats.country_stats);
              Printf.bprintf buf "\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr ());

              html_mods_td buf [ (* Display totals *)
                ("", "sr total", "Total");
                ("", "sr total", "");
                ("", "sr total", "");
                ("", "sr ar total", size_of_int64 !csu);
                ("", "sr ar total", size_of_int64 !csd);
                ("", "sr ar total", Printf.sprintf "%Ld" !css);
                ("", "sr ar total", size_of_int64 !ctu);
                ("", "sr ar total", size_of_int64 !ctd);
                ("", "sr ar total", Printf.sprintf "%Ld" !cts);
                ];
              Printf.bprintf buf "\\</tr\\>\\</table\\>\\</td\\>\\</tr\\>\\</table\\>\n"
          end
        else
          begin
            let list = ref [] in
            List.iter (fun cs ->
              if filter cs then list := [|
                cs.country_name;
                cs.country_code;
                cs.country_continent;
                size_of_int64 cs.country_session_upload;
                size_of_int64 cs.country_session_download;
                Printf.sprintf "%Ld" cs.country_session_seen;
                size_of_int64 cs.country_total_upload;
                size_of_int64 cs.country_total_download;
                Printf.sprintf "%Ld" cs.country_total_seen;
              |] :: !list
            ) (List.sort (fun c1 c2 -> compare c1.country_code c2.country_code) !!CommonStats.country_stats);
            print_table_text buf
            [|
              Align_Left; Align_Left; Align_Left; Align_Right; Align_Right; Align_Right; Align_Right; Align_Right; Align_Right |]
            [|
              "Country";
              "Code";
              "Con";
              "sUL";
              "sDL";
              "sSeen";
              "tUL";
              "tDL";
              "tSeen";
            |] (List.rev !list)
          end;
      _s ""), "[<all|regex>]:\t\t\tdisplay country based transfer statistics for countries with data transfered,\n\t\t\t\t\tuse arg 'all' for all countries seen\n\t\t\t\t\tor * as wildcard for country name, code and continent";

    "countries", Arg_none (fun o ->
        let buf = o.conn_buf in
        if use_html_mods o then
          begin
            html_mods_table_header buf "sharesTable" "shares" [
               ( Num, "srh ar", "Number", "Num" ) ;
               ( Str, "srh", "Country code", "Code" ) ;
               ( Str, "srh", "Country name", "Country" ) ;
               ( Str, "srh", "Continent code", "Con" ) ;
               ( Str, "srh", "Continent name", "Continent" ) ;
            ];
            html_mods_cntr_init ();
            Array.iteri (fun i _ ->
              Printf.bprintf buf "\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr ());
              html_mods_td buf [
                ("", "sr ar", Printf.sprintf "%d" i);
                (Geoip.country_code_array.(i), "sr", CommonPictures.flag_html (String.lowercase Geoip.country_code_array.(i)));
                ("", "sr", Geoip.country_name_array.(i));
                ("", "sr", Geoip.country_continent_code_array.(i));
                ("", "sr", Geoip.country_continent_name_array.(i));
              ];
              Printf.bprintf buf "\\</tr\\>\n"
            ) Geoip.country_code_array;
            Printf.bprintf buf "\\</table\\>\n";
          end
        else
          begin
            let list = ref [] in
            Array.iteri (fun i _ ->
              list := [|
                Printf.sprintf "%d" i;
                Geoip.country_code_array.(i);
                Geoip.country_name_array.(i);
                Geoip.country_continent_code_array.(i);
                Geoip.country_continent_name_array.(i);
              |] :: !list
            ) Geoip.country_code_array;
            print_table_text buf
            [|
              Align_Right; Align_Left; Align_Left; Align_Left; Align_Left |]
            [|
              "Num";
              "Country";
              "Code";
              "Con";
              "Continent";
            |] (List.rev !list)
          end;
      _s ""), ":\t\t\t\tdisplay country database";

    "reset_costats", Arg_none (fun o ->
        CommonStats.country_reset ();
        print_command_result o (_s "country statistics resetted");
      _s ""), ":\t\t\t\treset country based transfer statistics and save statistics.ini";

    "stats", Arg_none (fun o ->
        CommonInteractive.network_display_stats o;
        if use_html_mods o then
          print_gdstats o;
      _s ""), ":\t\t\t\t\tdisplay transfer statistics";

    "gdstats", Arg_none (fun o ->
        if Autoconf.has_gd then
          if use_html_mods o then
            print_gdstats o
          else
            print_command_result o (_s "Only available on HTML interface")
        else
          print_command_result o (_s "Gd support was not compiled");
      _s ""), ":\t\t\t\tdisplay graphical transfer statistics";

    "gdremove", Arg_none (fun o ->
        if Autoconf.has_gd then
          begin
            DriverGraphics.G.really_remove_files ();
            print_command_result o (_s "Gd files were removed")
          end
        else
          print_command_result o (_s "Gd support was not compiled");
      _s ""), ":\t\t\t\tremove graphical transfer statistics files";

    "!", Arg_multiple (fun arg o ->
        if !!allow_any_command then
          match arg with
            c :: args ->
              let cmd = try List.assoc c !!allowed_commands with Not_found -> c in
              (try
                 let pipe_out, pipe_in = Unix.pipe () in
                 let pid = Unix.create_process cmd 
                   (Array.of_list (Filename2.basename c :: args))
                   Unix.stdin pipe_in pipe_in in
                 Unix.close pipe_in;
                 (* can't close pipe_out in the already forked+executed process... *)
                 let output = Buffer.create 1024 in
                 let buffersize = 1024 in
                 let buffer = String.create buffersize in
                 (try
                    while true do
                      let nread = Unix.read pipe_out buffer 0 buffersize in
                      if nread = 0 then raise End_of_file;
                      Buffer.add_subbytes output buffer 0 nread
                    done
                  with 
                  | End_of_file -> ()
                  | Unix.Unix_error (code, f, arg) ->
                      lprintf_nl "%s failed%s: %s" f (if arg = "" then "" else " on " ^ arg) (Unix.error_message code));
                 (try Unix.close pipe_out with _ -> ());
                 let _pid, status = Unix.waitpid [] pid in
                 Printf.sprintf (_b "%s\n---------------- %s") 
                   (Buffer.contents output) 
                   (match status with
                    | Unix.WEXITED exitcode ->
                        Printf.sprintf "Exited with code %d" exitcode
                    | Unix.WSIGNALED signal ->
                        Printf.sprintf "Was killed by signal %d" signal
                    | Unix.WSTOPPED signal -> (* does it matter for us ? *)
                        Printf.sprintf "Was stopped by signal %d" signal)
                   
               with Unix.Unix_error (code, f, arg) ->
                 Printf.sprintf "%s failed%s: %s" f (if arg = "" then "" else " on " ^ arg) (Unix.error_message code))
          | [] -> _s "no command given"
        else
        match arg with
          [arg] ->
            (try
            let cmd = List.assoc arg !!allowed_commands in
            let tmp = Filename2.temp_file "com" ".out" in
            let ret = Sys.command (Printf.sprintf "%s > %s"
                  cmd tmp) in
            let output = File.to_string tmp in
            Sys.remove tmp;
            Printf.sprintf (_b "%s\n---------------- Exited with code %d") output ret
            with e -> "For arbitrary commands, you must set 'allowed_any_command'")
        | [] ->
            _s "no command given"
        | _ -> "For arbitrary commands, you must set 'allowed_any_command'"
    ), "<cmd> :\t\t\t\tstart command <cmd>\n\t\t\t\t\tmust be allowed in 'allowed_commands' option or by 'allow_any_command' if arguments";


  ]

(*************************************************************************)
(*                                                                       *)
(*                         Driver/Networks                               *)
(*                                                                       *)
(*************************************************************************)

let _ =
  register_commands "Driver/Networks"
    [

    "networks", Arg_none (fun o ->
        let buf = o.conn_buf in
        print_network_modules buf o;
        ""
    ) , ":\t\t\t\tprint all networks";

    "enable", Arg_one (fun num o ->
        if user2_is_admin o.conn_user.ui_user then
          begin
            let n = network_find_by_num (int_of_string num) in
            network_enable n;
            print_command_result o "network enabled"
          end
        else
          print_command_result o "You are not allowed to enable networks";
        _s ""
    ) , "<num> :\t\t\t\tenable a particular network";

    "disable", Arg_one (fun num o ->
        if user2_is_admin o.conn_user.ui_user then
          begin
            let n = network_find_by_num (int_of_string num) in
            network_disable n;
            print_command_result o "network disabled"
          end
        else
          print_command_result o "You are not allowed to disable networks";
        _s ""
    ) , "<num> :\t\t\t\tdisable a particular network";
    
    "discover_ip", Arg_none (fun o ->
      CommonGlobals.discover_ip true;
      print_command_result o "discover ip started";
           ""
    ) , ":\t\t\t\tstart IP discovery";

    "force_porttest", Arg_none (fun o ->
    networks_iter (fun n ->
      match network_porttest_result n with
        | PorttestNotAvailable -> ()
        | _ -> network_porttest_start n;
    );
    if use_html_mods o then
      print_command_result o "porttest started, use command
        \\<u\\>\\<a onclick=\\\"javascript:window.location.href='submit?q=porttest'\\\"\\>porttest\\</a\\>\\</u\\> to see results"
    else
      print_command_result o "porttest started, use command 'porttest' to see results";
           ""
    ) , ":\t\t\tforce start network porttest";        

    "porttest", Arg_none (fun o ->
        let buf = o.conn_buf in
        let age time = Date.time_to_string (BasicSocket.last_time () - time) "verbose" in
        let list = ref [] in
        let put_list e = list := e :: !list in
        networks_iter (fun n -> 
        match network_porttest_result n with
         | PorttestNotAvailable ->
             put_list (n.network_name , "Porttest not available")
         | PorttestNotStarted ->
             put_list (n.network_name , "Porttest started");
             network_porttest_start n
         | PorttestInProgress time ->
             put_list (n.network_name , Printf.sprintf "Porttest in progress, started %s ago" (age time))
         | PorttestResult (time, s) ->
             put_list (n.network_name , Printf.sprintf "Porttest finished %s ago \n%s" (age time) s)
    );
    if use_html_mods o then begin
      Printf.bprintf buf "\\<div class=\\\"shares\\\"\\>\\<table class=main cellspacing=0 cellpadding=0\\>
                          \\<tr\\>\\<td\\>\\<table cellspacing=0 cellpadding=0  width=100%%\\>
                          \\<tr\\>\\<td class=downloaded width=100%%\\>\\</td\\>
                            \\<td nowrap class=\\\"fbig\\\"\\>
                            \\<a onclick=\\\"javascript:window.location.href='submit?q=force_porttest'\\\"\\>Restart porttest\\</a\\>\\</td\\>
                            \\<td nowrap class=\\\"fbig pr\\\"\\>
                            \\<a onclick=\\\"javascript:window.location.reload()\\\"\\>Refresh results\\</a\\>\\</td\\>
                          \\</tr\\>\\</table\\>\\</td\\>\\</tr\\>\\<tr\\>\\<td\\>";
      html_mods_table_header buf "sharesTable" "shares" [
        ( Str, "srh", "Network", "Network" ) ;
        ( Str, "srh", "Result", "Result" ) ]
    end;
    html_mods_cntr_init ();
    List.iter (fun (net, result) -> 
      if use_html_mods o then
        Printf.bprintf buf "\\<tr class=\\\"dl-%d\\\"\\>\\<td class=\\\"sr\\\"\\>%s\\</td\\>\\<td class=\\\"sr\\\"\\>%s\\</td\\>\\</tr\\>"
          (html_mods_cntr ()) net (Str.global_replace (Str.regexp "\n") "\\<br\\>" result)
      else
        Printf.bprintf buf "----- %s: -----\n%s\n\n" net result;
    ) !list;
    if use_html_mods o then
      Printf.bprintf buf "\\</table\\>\\</div\\>\\</td\\>\\</tr\\>\\</table\\>"
    else
      Printf.bprintf buf "\n\nuse command 'porttest' again to refresh the results \nuse command 'force_porttest' to force a new porttest";
        ""
    ) , ":\t\t\t\tprint network porttest results";

    ]

(*************************************************************************)
(*                                                                       *)
(*                         Driver/Searches                               *)
(*                                                                       *)
(*************************************************************************)

let _ =
  register_commands "Driver/Searches"
    [

    "forget", Arg_multiple (fun args o ->
        let user = o.conn_user in
        begin
          match args with
            ["all"] ->
              List.iter (fun s ->
                  CommonSearch.search_forget user (CommonSearch.search_find s.search_num);
              ) user.ui_user_searches
          | [] ->
              begin
                match user.ui_user_searches with
                  [] -> ()
                | s :: _ ->
                    CommonSearch.search_forget user
                      (CommonSearch.search_find s.search_num);
              end

          | _ ->
              List.iter (fun arg ->
                  let num = int_of_string arg in
                  CommonSearch.search_forget user (CommonSearch.search_find num)
              ) args;
        end;
        ""
    ), "<num1> <num2> ... :\t\tforget searches <num1> <num2> ...";

    "vr", Arg_multiple (fun args o ->
        let buf = o.conn_buf in
        let user = o.conn_user in
        match args with
          num :: _ ->
            List.iter (fun num ->
                let num = int_of_string num in
                let s = search_find num in
                DriverInteractive.print_search buf s o) args;
            ""
        | [] ->
            begin
              match user.ui_user_searches with
                [] ->
                  if o.conn_output = HTML then
                    html_mods_table_one_row buf "searchTable" "search" [
                      ("", "srh", "No search to print"); ]
                  else
                    Printf.bprintf buf "No search to print";
            ""
              | s :: _ ->
                  DriverInteractive.print_search buf s o;
                  ""
            end;
    ), "[<num>] :\t\t\t\t$bview results of a search$n";

    "s", Arg_multiple (fun args o ->
        let buf = o.conn_buf in
        let user = o.conn_user in
        let query, net = CommonSearch.search_of_args args in
        ignore (CommonInteractive.start_search user
            (let module G = GuiTypes in
            { G.search_num = 0;
              G.search_query = query;
              G.search_max_hits = 10000;
              G.search_type = RemoteSearch;
              G.search_network = net;
            }) buf);
        ""
    ), "<query> :\t\t\t\t$bsearch for files on all networks$n\n\n\tWith special args:\n\t-network <netname>\n\t-minsize <size>\n\t-maxsize <size>\n\t-media <Video|Audio|...>\n\t-Video\n\t-Audio\n\t-format <format>\n\t-title <word in title>\n\t-album <word in album>\n\t-artist <word in artist>\n\t-field <field> <fieldvalue>\n\t-not <word>\n\t-and <word>\n\t-or <word>\n";

    "ls", Arg_multiple (fun args o ->
        let buf = o.conn_buf in
        let user = o.conn_user in
        let query, net = CommonSearch.search_of_args args in
        ignore (CommonInteractive.start_search user
            (let module G = GuiTypes in
            { G.search_num = 0;
              G.search_query = query;
              G.search_max_hits = 10000;
              G.search_type = LocalSearch;
              G.search_network = net;
            }) buf);
        ""
    ), "<query> :\t\t\t\tsearch for files locally\n\n\tWith special args:\n\t-network <netname>\n\t-minsize <size>\n\t-maxsize <size>\n\t-media <Video|Audio|...>\n\t-Video\n\t-Audio\n\t-format <format>\n\t-title <word in title>\n\t-album <word in album>\n\t-artist <word in artist>\n\t-field <field> <fieldvalue>\n\t-not <word>\n\t-and <word>\n\t-or <word>\n";

    "vs", Arg_none (fun o ->
        let buf = o.conn_buf in
        let user = o.conn_user in
        let num_searches = List.length user.ui_user_searches in
        if num_searches < 1 then
          if o.conn_output = HTML then
            html_mods_table_one_row buf "searchTable" "search" [
              ("", "srh", "No search yet"); ]
          else
            Printf.bprintf buf "No search yet"
        else begin
            if o.conn_output = HTML then
              Printf.bprintf  buf "Searching %d queries\n" (
                List.length user.ui_user_searches);
            List.iter (fun s ->
                Printf.bprintf buf "%s[%-5d]%s %s %s (found %d)\n"
                  (if o.conn_output = HTML then
                    Printf.sprintf "\\<a href=\\\"submit\\?q=forget\\+%d\\\" target=fstatus\\>[Forget]\\</a\\> \\<a href=\\\"submit\\?q=vr\\+%d\\\"\\>" s.search_num s.search_num
                  else "")
                s.search_num
                  s.search_string
                  (if o.conn_output = HTML then "\\</a\\>" else "")
                (if s.search_waiting = 0 then _s "done" else
                    string_of_int s.search_waiting)
                s.search_nresults
            ) (List.sort (fun f1 f2 -> compare f1.search_num f2.search_num)
            user.ui_user_searches)
          end;
        ""
    ), ":\t\t\t\t\tview all queries";

    "view_custom_queries", Arg_none (fun o ->
        let buf = o.conn_buf in
        if o.conn_output <> HTML then
          Printf.bprintf buf "%d custom queries defined\n"
            (List.length (customized_queries ()));
        let custom_commands = ref [] in
        List.iter (fun (name, q) ->
            if o.conn_output = HTML then
              begin
                if use_html_mods o then
                  custom_commands := !custom_commands @ [ ( "bu bbig",
                  name,
                  Printf.sprintf "mSub('output','custom=%s')" (Url.encode_to_string name),
                  name ) ; ]
                else
                  Printf.bprintf buf
                    "\\<a href=\\\"submit\\?custom=%s\\\" $O\\> %s \\</a\\>\n"
                    (Url.encode_to_string name) name;
              end
            else

              Printf.bprintf buf "[%s]\n" name
        ) (customized_queries ());

        if use_html_mods o then
          html_mods_commands buf "commandsTable" "commands" (!custom_commands @ [
            ("bu bbig", "Visit FileHeaven",
             "parent.frames[_getFrameByName('output')].location.href='http://www.fileheaven.org/'", "FileHeaven");
            ("bu bbig", "Visit FileDonkey",
             "parent.frames[_getFrameByName('output')].location.href='http://www.filedonkey.com/'", "FileDonkey");
            ("bu bbig", "Visit Bitzi",
             "parent.frames[_getFrameByName('output')].location.href='http://www.fileheaven.org/'", "Bitzi");
            ("bu bbig", "Visit eMugle",
             "parent.frames[_getFrameByName('output')].location.href='http://www.emugle.com/'", "eMugle");
          ]);
        ""
    ), ":\t\t\tview custom queries";

    "d", Arg_multiple (fun args o ->
        List.iter (fun arg ->
            CommonInteractive.download_file o arg) args;
        ""
    ), "<num> :\t\t\t\t$bfile to download$n";

    "force_download", Arg_none (fun o ->
        if !forceable_download = [] then
          begin
            let output = (if o.conn_output = HTML then begin
                let buf = Buffer.create 100 in
                Printf.bprintf buf "\\<div class=\\\"cs\\\"\\>";
                html_mods_table_header buf "dllinkTable" "results" [];
                Printf.bprintf buf "\\<tr\\>";
                html_mods_td buf [ ("", "srh", "No download to force"); ];
                Printf.bprintf buf "\\</tr\\>\\</table\\>\\</div\\>\\</div\\>";
                Buffer.contents buf
              end
            else begin
                Printf.sprintf "No download to force"
            end) in
            _s output
          end
        else
          begin
            let r = List.hd !forceable_download in
              CommonNetwork.networks_iter (fun n ->
                ignore (network_download n r o.conn_user.ui_user o.conn_user.ui_user.user_default_group));

            let output = (if o.conn_output = HTML then begin
                let buf = Buffer.create 100 in
                Printf.bprintf buf "\\<div class=\\\"cs\\\"\\>";
                html_mods_table_header buf "dllinkTable" "results" [];
                Printf.bprintf buf "\\<tr\\>";
                html_mods_td buf [ ("", "srh", "Forced start of "); ];
                Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-1\\\"\\>";
                html_mods_td buf [ ("", "sr", (List.hd r.result_names)); ];
                Printf.bprintf buf "\\</tr\\>\\</table\\>\\</div\\>\\</div\\>";
                Buffer.contents buf
              end
            else begin
                Printf.sprintf "Forced start of : %s" (List.hd r.result_names)
            end) in
            _s output
          end;
    ), ":\t\t\tforce download of an already downloaded file";

    ]

(*************************************************************************)
(*                                                                       *)
(*                         Driver/Options                                *)
(*                                                                       *)
(*************************************************************************)

let _ =
  register_commands "Driver/Options"
    [

    "set", Arg_two (fun name value o ->
        if user2_is_admin o.conn_user.ui_user then begin
        try
          try
            let gui_type, ip, port =
              match o.conn_info with
              | None -> None, None, None
              | Some (gui_type, (ip, port)) -> Some gui_type, Some ip, Some port
            in
            CommonInteractive.set_fully_qualified_options name value
              ~user:(Some o.conn_user.ui_user.user_name)
              ~ip:ip ~port:port ~gui_type:gui_type ();
            Printf.sprintf "option %s value changed" name
          with _ ->
              Options.set_simple_option downloads_ini name value;
              Printf.sprintf "option %s value changed" name
        with
        | Not_found -> Printf.sprintf "Option %s does not exist" name
        | e -> Printf.sprintf "Error %s" (Printexc2.to_string e)
          end
        else
          _s "You are not allowed to change options"
    ), "<option_name> <option_value> :\t$bchange option value$n";

    "save", Arg_multiple (fun args o ->
        if !allow_saving_ini_files then begin
        match args with
          ["options"] -> DriverInteractive.save_config (); _s "options saved"
        | ["sources"] -> CommonComplexOptions.save_sources (); _s "sources saved"
        | ["backup"] -> CommonComplexOptions.backup_options (); _s "backup saved"
        | ["all"] ->
               DriverInteractive.save_config ();
               CommonComplexOptions.save_sources ();
               CommonComplexOptions.backup_options ();
                _s "options, sources and backup saved"
        | _ -> DriverInteractive.save_config ();
               CommonComplexOptions.save_sources (); _s "options and sources saved"
        end else _s "base directory full, ini file saving disabled until core shutdown"
        ), "[<options|sources|backup>] :\tsave options and/or sources or backup (empty for options and sources)";

    "vo", Arg_none (fun o ->
        let buf = o.conn_buf in
        if use_html_mods o then begin

          if !!html_mods_use_js_helptext then
            Printf.bprintf buf "\\<div id=\\\"object1\\\" style=\\\"position:absolute; background-color:#FFFFDD;color:black;border-color:black;border-width:20px;font-size:8pt; visibility:visible; left:25px; top:-100px; z-index:+1\\\" onmouseover=\\\"overdiv=1;\\\"  onmouseout=\\\"overdiv=0; setTimeout(\\\'hideLayer()\\\',1000)\\\"\\>\\&nbsp;\\</div\\>";
            
          Printf.bprintf buf "\\<div class=\\\"friends\\\"\\>\\<table class=main cellspacing=0 cellpadding=0\\>
\\<tr\\>\\<td\\>
\\<table cellspacing=0 cellpadding=0  width=100%%\\>\\<tr\\>
\\<td class=downloaded width=100%%\\>\\</td\\>
\\<td nowrap title=\\\"Show shares Tab (also related for incoming directory)\\\" class=\\\"fbig fbigpad\\\"\\>\\<a onclick=\\\"javascript:window.location.href='submit?q=shares'\\\"\\>Shares\\</a\\>\\</td\\>
%s
\\<td nowrap title=\\\"Show Web_infos Tab where you can add/remove automatic downloads like serverlists\\\" class=\\\"fbig fbigpad\\\"\\>\\<a onclick=\\\"javascript:window.location.href='submit?q=vwi'\\\"\\>Web infos\\</a\\>\\</td\\>
\\<td nowrap title=\\\"Show Calendar Tab, there are information about automatically jobs\\\" class=\\\"fbig fbigpad\\\"\\>\\<a onclick=\\\"javascript:window.location.href='submit?q=vcal'\\\"\\>Calendar\\</a\\>\\</td\\>
\\<td nowrap title=\\\"Change to simple Webinterface without html_mods\\\" class=\\\"fbig fbigpad\\\"\\>\\<a onclick=\\\"javascript:window.location.href='submit?q=html_mods'\\\"\\>Toggle html_mods\\</a\\>\\</td\\>
\\<td nowrap title=\\\"voo\\\" class=\\\"fbig pr fbigpad\\\"\\>\\<a onclick=\\\"javascript:window.location.href='submit?q=voo+1'\\\"\\>Full Options\\</a\\>\\</td\\>
\\</tr\\>\\</table\\>
\\</td\\>\\</tr\\>
\\<tr\\>\\<td\\>"
(if (user2_is_admin o.conn_user.ui_user) then
  "\\<td nowrap title=\\\"Show users Tab where you can add/remove Users\\\" class=\\\"fbig fbigpad\\\"\\>\\<a onclick=\\\"javascript:window.location.href='submit?q=users'\\\"\\>Users\\</a\\>\\</td\\>"
 else "");

            list_options_html o  (
              [
(* replaced strings_of_option_html by strings_of_option *)
                strings_of_option max_hard_upload_rate;
                strings_of_option max_hard_download_rate;
                strings_of_option telnet_port;
                strings_of_option gui_port;
                strings_of_option http_port;
                strings_of_option global_login;
                strings_of_option allowed_ips;
                strings_of_option set_client_ip;
                strings_of_option force_client_ip;
                strings_of_option discover_ip;
              ] );

            Printf.bprintf buf "\\</td\\>\\</tr\\>\\<tr\\>\\<td\\>\\<table cellspacing=0 cellpadding=0  width=100%%\\>\\<tr\\>\\<td class=downloaded width=100%%\\>\\</td\\>
\\<td nowrap title=\\\"Toggle option helptext from javascript popup to html table\\\" class=\\\"fbig fbigb pr fbigpad\\\"\\>
\\<a onclick=\\\"javascript: {parent.fstatus.location.href='submit?q=set+html_mods_use_js_helptext+%s'; setTimeout('window.location.replace(window.location.href)',1000);return true;}\\\"\\>toggle js_helptext\\</a\\>
\\</td\\>\\</tr\\>\\</table\\>\\</td\\>\\</tr\\>\\</table\\>\\</div\\>\\</br\\>" (if !!html_mods_use_js_helptext then "false" else "true");
            
            html_mods_table_one_row buf "downloaderTable" "downloaders" [
            ("", "srh", "!! press ENTER to send changes to core !!"); ]

          end

        else
          list_options o  (
            [
              strings_of_option max_hard_upload_rate;
              strings_of_option max_hard_download_rate;
              strings_of_option telnet_port;
              strings_of_option gui_port;
              strings_of_option http_port;
              strings_of_option global_login;
              strings_of_option allowed_ips;
              strings_of_option set_client_ip;
              strings_of_option force_client_ip;
              strings_of_option discover_ip;
            ]
          );

        "\nUse '$rvoo$n' for all options"
    ), ":\t\t\t\t\t$bdisplay options$n";




    "voo", Arg_multiple (fun args o ->
        let buf = o.conn_buf in
        let put fmt = Printf.bprintf buf fmt in
        let changed_list = List.sort (fun d1 d2 -> compare d1 d2) (List.filter (fun o -> 
            o.option_value <> o.option_default && not (String2.starts_with o.option_name "enable_")
            ) (CommonInteractive.all_simple_options ())) in
        if use_html_mods o then begin

            put "\\<script type=\\\"text/javascript\\\"\\>
\\<!--
function pluginSubmit() {
var formID = document.getElementById(\\\"pluginForm\\\");
var v = formID.plugin.value;
location.href='submit?q=voo+'+v;
}
function submitHtmlModsStyle() {
var formID = document.getElementById(\\\"htmlModsStyleForm\\\");
var v = formID.modsStyle.value;
if (\\\"0123456789.\\\".indexOf(v) == -1)
{ parent.fstatus.location.href='submit?q=html_theme+\\\"'+v+'\\\"';} else
{ parent.fstatus.location.href='submit?q=html_mods_style+'+v;}
}
//--\\>
\\</script\\>";

            let button ~title ~cls ~cmd content = put "\\<td nowrap title=\\\"%s\\\" class=\\\"%s\\\"\\>\\<a onclick=\\\"javascript:window.location.href='submit?q=%s';setTimeout('window.location.replace(window.location.href)',500)\\\"\\>%s\\</a\\>\\</td\\>" title cls cmd content
            in

            let select name options =
              put "\\<select id=\\\"%s\\\" name=\\\"%s\\\"
style=\\\"padding: 0px; font-size: 10px; font-family: verdana\\\" onchange=\\\"this.form.submit()\\\"\\>" name name;
              List.iter (fun (n,v) ->
                put "\\<option value=\\\"%s\\\"\\>%s\\</option\\>\n" n v;
              ) options; 
              put "\\</select\\>"
            in

            let tabnumber = ref 0 in
            let mtabs = ref 1 in

            if !!html_mods_use_js_helptext then
             put "\\<div id=\\\"object1\\\" style=\\\"position:absolute; background-color:#FFFFDD;color:black;border-color:black;border-width:20px;font-size:8pt; visibility:visible; left:25px; top:-100px; z-index:+1\\\" onmouseover=\\\"overdiv=1;\\\"  onmouseout=\\\"overdiv=0; setTimeout(\\\'hideLayer()\\\',1000)\\\"\\>\\&nbsp;\\</div\\>";

            put "\\<div class=\\\"vo\\\"\\>";
            put "\\<table class=main cellspacing=0 cellpadding=0\\>";
            put "\\<tr\\>\\<td\\>";
            put "\\<table cellspacing=0 cellpadding=0 class='hcenter'\\>\\<tr\\>";

            List.iter (fun (s,title) ->
                incr tabnumber; incr mtabs;
                button ~title ~cls:"fbig" ~cmd:(Printf.sprintf "voo+%d" !tabnumber) s
            ) [ ("Client", "Client related options & Up/Download limitations ") ; 
                ("Ports", "Interface ports, each Network port is stored in Network plugin options") ; 
                ("html", "Show Webinterface related options") ; 
                ("Delays", "Delays & timeouts") ; 
                ("Files", "File related options") ; 
                ("Mail", "eMail information options") ; 
                ("Net", "activate/deaktivate Networks, some TCP/IP & IP blocking options") ; 
                ("Misc", "miscellaneous") ;
                ("changed", "Show changed options") ];

            button ~title:"Show all options" ~cls:"fbig" ~cmd:"voo" "All";
            put "\\<td nowrap class=\\\"fbig pr\\\"\\>
\\<form style=\\\"margin: 0px;\\\" name=\\\"pluginForm\\\" id=\\\"pluginForm\\\"
action=\\\"javascript:pluginSubmit();\\\"\\>";

            let options = 
              let netlist = List.map
                (fun s -> incr tabnumber; s,!tabnumber) 
                (CommonInteractive.all_active_network_opfile_network_names ())
              in
              let duplist = ref [] in
              List.map (fun (s,t) ->
                let name = if List.memq s !duplist then s^"+" else s in
                duplist := name :: !duplist;
                string_of_int t, name
              ) (List.sort (fun (s1,_) (s2,_) -> compare s1 s2) netlist);
            in

            select "plugin" (("0","Plugins") :: options);

            put "\\</form\\>\\</td\\>\\</tr\\>\\</table\\>";
            put "\\</td\\>\\</tr\\>";
            put "\\<tr\\>\\<td\\>";

            list_options_html o (
              match args with
              |  [] | _ :: _ :: _ ->
                  CommonInteractive.all_simple_options ()

              | ["changed"] ->
                  changed_list

              | [arg] ->
                  try
                  let tab = int_of_string arg in
                  match tab with
                    1 ->
                      [
                        strings_of_option global_login;
                        strings_of_option set_client_ip;
                        strings_of_option force_client_ip;
                        strings_of_option discover_ip;
                        strings_of_option max_upload_slots;
                        strings_of_option max_release_slots;
                        strings_of_option dynamic_slots;
                        strings_of_option max_hard_upload_rate;
                        strings_of_option max_hard_download_rate;
                        strings_of_option max_opened_connections;
                        strings_of_option max_hard_upload_rate_2;
                        strings_of_option max_hard_download_rate_2;
                        strings_of_option max_opened_connections_2;
                        strings_of_option max_indirect_connections;
                        strings_of_option max_connections_per_second;
                        strings_of_option max_concurrent_downloads;
                      ]

                  | 2 ->
                      [
                        strings_of_option gui_bind_addr;
                        strings_of_option telnet_bind_addr;
                        strings_of_option http_bind_addr;
                        strings_of_option client_bind_addr;
                        strings_of_option gui_port;
                        strings_of_option telnet_port;
                        strings_of_option http_port;
                        strings_of_option http_root_url;
                        strings_of_option http_realm;
                        strings_of_option allowed_ips;
                      ]
                  | 3 ->
                      [
                        strings_of_option html_mods_use_relative_availability;
                        strings_of_option html_mods_human_readable;
                        strings_of_option html_mods_vd_network;
                        strings_of_option html_mods_vd_active_sources;
                        strings_of_option html_mods_vd_age;
                        strings_of_option html_mods_vd_user;
                        strings_of_option html_mods_vd_group;
                        strings_of_option html_mods_vd_last;
                        strings_of_option html_mods_vd_prio;
                        strings_of_option html_mods_show_pending;
                        strings_of_option html_mods_load_message_file;
                        strings_of_option html_mods_max_messages;
                        strings_of_option html_mods_bw_refresh_delay;
                        strings_of_option html_frame_border;
                        strings_of_option html_checkbox_vd_file_list;
                        strings_of_option html_checkbox_search_file_list;
                        strings_of_option commands_frame_height;
                        strings_of_option html_vd_barheight;
                        strings_of_option html_vd_chunk_graph;
                        strings_of_option html_vd_chunk_graph_style;
                        strings_of_option html_vd_chunk_graph_max_width;
                        strings_of_option display_downloaded_results;
                        strings_of_option vd_reload_delay;
                        strings_of_option html_use_gzip;
                        strings_of_option html_flags;
                        strings_of_option html_mods_use_js_tooltips;
                        strings_of_option html_mods_js_tooltips_wait;
                        strings_of_option html_mods_js_tooltips_timeout;
                        strings_of_option html_mods_use_js_helptext;
                        ] @ (if Autoconf.has_gd then
                        [strings_of_option html_mods_vd_gfx;] else []) @
                        (if Autoconf.has_gd_jpg && Autoconf.has_gd_png
                         then [strings_of_option html_mods_vd_gfx_png;] else []) @
                        (if Autoconf.has_gd then [
                        strings_of_option html_mods_vd_gfx_remove;
                        strings_of_option html_mods_vd_gfx_split;
                        strings_of_option html_mods_vd_gfx_stack;
                        strings_of_option html_mods_vd_gfx_fill;
                        strings_of_option html_mods_vd_gfx_flip;
                        strings_of_option html_mods_vd_gfx_mean;
                        strings_of_option html_mods_vd_gfx_transparent;
                        strings_of_option html_mods_vd_gfx_h;
                        strings_of_option html_mods_vd_gfx_h_intervall;
                        strings_of_option html_mods_vd_gfx_h_dynamic;
                        strings_of_option html_mods_vd_gfx_h_grid_time;
                        strings_of_option html_mods_vd_gfx_subgrid;
                        strings_of_option html_mods_vd_gfx_x_size;
                        strings_of_option html_mods_vd_gfx_y_size;
                        strings_of_option html_mods_vd_gfx_tag;
                        strings_of_option html_mods_vd_gfx_tag_use_source;
                        strings_of_option html_mods_vd_gfx_tag_source;
                        strings_of_option html_mods_vd_gfx_tag_png;
                        strings_of_option html_mods_vd_gfx_tag_enable_title;
                        strings_of_option html_mods_vd_gfx_tag_title;
                        strings_of_option html_mods_vd_gfx_tag_title_x_pos;
                        strings_of_option html_mods_vd_gfx_tag_title_y_pos;
                        strings_of_option html_mods_vd_gfx_tag_dl_x_pos;
                        strings_of_option html_mods_vd_gfx_tag_dl_y_pos;
                        strings_of_option html_mods_vd_gfx_tag_ul_x_pos;
                        strings_of_option html_mods_vd_gfx_tag_ul_y_pos;
                        strings_of_option html_mods_vd_gfx_tag_x_size;
                        strings_of_option html_mods_vd_gfx_tag_y_size;
                        ] else [])
                  | 4 ->
                      [
                        strings_of_option save_options_delay;
                        strings_of_option update_gui_delay;
                        strings_of_option server_connection_timeout;
                        strings_of_option compaction_delay;
                        strings_of_option min_reask_delay;
                        strings_of_option buffer_writes;
                        strings_of_option buffer_writes_delay;
                        strings_of_option buffer_writes_threshold;
                      ]
                  | 5 ->
                      [
                        strings_of_option previewer;
                        strings_of_option temp_directory;
                        strings_of_option filenames_utf8;
                        strings_of_option share_scan_interval;
                        strings_of_option hdd_temp_minfree;
                        strings_of_option hdd_temp_stop_core;
                        strings_of_option hdd_coredir_minfree;
                        strings_of_option hdd_coredir_stop_core;
                        strings_of_option hdd_send_warning_interval;
                        strings_of_option file_started_cmd;
                        strings_of_option file_completed_cmd;
                        strings_of_option allow_browse_share;
                        strings_of_option auto_commit;
                        strings_of_option pause_new_downloads;
                        strings_of_option release_new_downloads;
                        strings_of_option create_file_mode;
                        strings_of_option create_dir_mode;
                        strings_of_option create_file_sparse;
                        strings_of_option log_file;
                        strings_of_option log_file_size;
                        strings_of_option log_size;
                      ]
                  | 6 ->
                      [
                        strings_of_option mail;
                        strings_of_option smtp_port;
                        strings_of_option smtp_server;
                        strings_of_option smtp_login;
                        strings_of_option smtp_password;
                        strings_of_option add_mail_brackets;
                        strings_of_option filename_in_subject;
                        strings_of_option url_in_mail;
                      ]
                  | 7 ->
                      ( (if Autoconf.donkey = "yes" then [(strings_of_option enable_overnet)] else [])
                        @ [
                        ] @
                        (if Autoconf.donkey = "yes" then [(strings_of_option enable_kademlia)] else [])
                        @ [
                        ] @
                        (if Autoconf.donkey = "yes" then [(strings_of_option enable_donkey)] else [])
                        @ [
                        ] @
                        (if Autoconf.bittorrent = "yes" then [(strings_of_option enable_bittorrent)] else [])
                        @ [
                        ] @
                        (if Autoconf.fasttrack = "yes" then [(strings_of_option enable_fasttrack)] else [])
                        @ [
                        ] @
                        (if Autoconf.opennapster = "yes" then [(strings_of_option enable_opennap)] else [])
                        @ [
                        ] @
                        (if Autoconf.soulseek = "yes" then [(strings_of_option enable_soulseek)] else [])
                        @ [
                        ] @
                        (if Autoconf.gnutella = "yes" then [(strings_of_option enable_gnutella)] else [])
                        @ [
                        ] @
                        (if Autoconf.gnutella2 = "yes" then [(strings_of_option enable_gnutella2)] else [])
                        @ [
                        ] @
                        (if Autoconf.direct_connect = "yes" then [(strings_of_option enable_directconnect)] else [])
                        @ [
                        ] @
                        (if Autoconf.openft = "yes" then [(strings_of_option enable_openft)] else [])
                        @ [
                        ] @
                        (if Autoconf.filetp = "yes" then [(strings_of_option enable_fileTP)] else [])
                        @ [
                        ] @
                        (if Autoconf.upnp_natpmp then [(strings_of_option upnp_port_forwarding)] else [])
                        @ [
                        ] @
                        (if Autoconf.upnp_natpmp then [(strings_of_option clear_upnp_port_at_exit)] else [])
                        @ [
                        strings_of_option tcpip_packet_size;
                        strings_of_option mtu_packet_size;
                        strings_of_option minimal_packet_size;
                        strings_of_option ip_blocking;
                        strings_of_option ip_blocking_descriptions;
                        strings_of_option ip_blocking_countries;
                        strings_of_option ip_blocking_countries_block;
                      ])
                  | 8 ->
                      [
                        strings_of_option term_ansi;
                        strings_of_option run_as_user;
                        strings_of_option run_as_useruid;
                        strings_of_option messages_filter;
                        strings_of_option comments_filter;
                        strings_of_option max_displayed_results;
                        strings_of_option max_name_len;
                        strings_of_option max_result_name_len;
                        strings_of_option max_filenames;
                        strings_of_option max_client_name_len;
                        strings_of_option emule_mods_count;
                        strings_of_option emule_mods_showall;
                        strings_of_option backup_options_format;
                        strings_of_option backup_options_delay;
                        strings_of_option backup_options_generations;
                        strings_of_option small_files_slot_limit;
                      ]
                  | 9 ->
                      changed_list

                  | _ ->
                      let v = CommonInteractive.some_simple_options (tab - !mtabs) in
                      List.sort (fun d1 d2 -> compare d1 d2) v;
              with _ ->
                    let v = CommonInteractive.parse_simple_options args in
                    List.sort (fun d1 d2 -> compare d1 d2) v;


            );
            put "\\</td\\>\\</tr\\>";
            put "\\<tr\\>\\<td\\>";

            put "\\<table cellspacing=0 cellpadding=0 class='hcenter'\\>\\<tr\\>";

            button ~title:"Show shares Tab (also related for incoming directory)" ~cls:"fbig fbigb" ~cmd:"shares" "Shares";
            if (user2_is_admin o.conn_user.ui_user) then
              button ~title:"Show users Tab where you can add/remove Users" ~cls:"fbig fbigb" ~cmd:"users" "Users";

            button ~title:"Show Web_infos Tab where you can add/remove automatic downloads like serverlists" ~cls:"fbig fbigb" ~cmd:"vwi" "Web infos";
            button ~title:"Show Calendar Tab, there are information about automatically jobs" ~cls:"fbig fbigb" ~cmd:"vcal" "Calendar";
            put "\\<td nowrap class=\\\"fbig fbigb pr\\\"\\>
\\<form style=\\\"margin: 0px;\\\" name=\\\"htmlModsStyleForm\\\" id=\\\"htmlModsStyleForm\\\"
action=\\\"javascript:submitHtmlModsStyle();\\\"\\>";

            let options =
              ("0", "style/theme")
              ::
              Array.to_list (Array.mapi (fun i style -> string_of_int i, style.style_name) CommonMessages.styles)
              @
              if Sys.file_exists html_themes_dir then begin
                let list = Unix2.list_directory html_themes_dir in
                List.fold_left (fun acc d ->
                  if Unix2.is_directory (Filename.concat html_themes_dir d) then
                    let sd = (if String.length d > 11 then String.sub d 0 11 else d) in
                    (d,sd) :: acc
                  else
                    acc
                ) [] (List.sort (fun d1 d2 -> compare d1 d2) list);
              end
              else []
            in

            select "modsStyle" options;

            put "\\</form\\>\\</td\\>\\</tr\\>\\</table\\>";
            put "\\</td\\>\\</tr\\>";
            put "\\<tr\\>\\<td\\>";
            put "\\<table cellspacing=0 cellpadding=0 class='hcenter'\\>\\<tr\\>";
            button ~title:"Change to simple Webinterface without html_mods" ~cls:"fbig fbigb fbigpad" ~cmd:"html_mods" "toggle html_mods";
            put "\\<td nowrap title=\\\"Toggle option helptext from javascript popup to html table\\\" class=\\\"fbig fbigb pr fbigpad\\\"\\>
\\<a onclick=\\\"javascript: {parent.fstatus.location.href='submit?q=set+html_mods_use_js_helptext+%s'; setTimeout('window.location.replace(window.location.href)',1000);return true;}\\\"\\>toggle js_helptext\\</a\\>" (if !!html_mods_use_js_helptext then "false" else "true");
            put "\\</tr\\>\\</table\\>\\</td\\>\\</tr\\>\\</table\\>\\</div\\>\\</br\\>";

          html_mods_table_one_row buf "downloaderTable" "downloaders" [
          ("", "srh", "!! press ENTER to send changes to core !!"); ];
          end

        else begin
          match args with
            | [] | _ :: _ :: _ -> list_options o (CommonInteractive.all_simple_options ())
            | ["9"] | ["changed"] -> list_options o changed_list
            | [_] -> list_options o (CommonInteractive.parse_simple_options args);
          end;
        ""
    ), "[<option>|changed]:\t\t\tprint options (use * as wildcard), 'changed' prints all changed options, leave empty to print all options";

    "vwi", Arg_none (fun o ->
        let buf = o.conn_buf in
        if use_html_mods o then begin
            Printf.bprintf buf "\\<div class=\\\"shares\\\"\\>\\<table class=main cellspacing=0 cellpadding=0\\>
\\<tr\\>\\<td\\>
\\<table cellspacing=0 cellpadding=0  width='100%%'\\>\\<tr\\>
\\<td class=downloaded width='100%%'\\>\\</td\\>
\\<td nowrap title=\\\"force downloading all web_infos files\\\" class=\\\"fbig\\\"\\>
\\<a onclick=\\\"javascript: {parent.fstatus.location.href='submit?q=force_web_infos';}\\\"\\>Re-download all\\</a\\>
\\<td nowrap class=\\\"fbig pr\\\"\\>\\<a onclick=\\\"javascript: {
                   var getdir = prompt('Input: <kind> <URL> [<period>]','server.met URL')
                   parent.fstatus.location.href='submit?q=urladd+' + encodeURIComponent(getdir);
                   setTimeout('window.location.reload()',1000);
                    }\\\"\\>Add URL\\</a\\>
\\</td\\>
\\</tr\\>\\</table\\>
\\</td\\>\\</tr\\>
\\<tr\\>\\<td\\>";

            if Hashtbl.length web_infos_table = 0 then
              html_mods_table_one_row buf "serversTable" "servers" [
                ("", "srh", "no jobs defined"); ]
            else begin

              html_mods_table_header buf "web_infoTable" "vo" [
                ( Str, "srh ac", "Click to remove URL", "Remove" ) ;
                ( Str, "srh", "Download now", "DL" ) ;
                ( Str, "srh", "Filetype", "Type" ) ;
                ( Num, "srh", "Interval in hours", "Interval" ) ;
                ( Str, "srh", "URL", "URL" ) ;
                ( Str, "srh", "URL state", "State" ) ;
              ] ;

              html_mods_cntr_init ();
              Hashtbl.iter (fun key w ->
                Printf.bprintf buf "\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr ());
                Printf.bprintf buf "
        \\<td title=\\\"Click to remove URL\\\"
        onMouseOver=\\\"mOvr(this);\\\"
        onMouseOut=\\\"mOut(this);\\\"
        onClick=\\\'javascript:{
        parent.fstatus.location.href=\\\"submit?q=urlremove+\\\\\\\"%s\\\\\\\"\\\"
        setTimeout(\\\"window.location.reload()\\\",1000);}'
        class=\\\"srb\\\"\\>Remove\\</td\\>" (Url.encode_to_string w.url);
                Printf.bprintf buf "
        \\<td title=\\\"Download now\\\"
        onMouseOver=\\\"mOvr(this);\\\"
        onMouseOut=\\\"mOut(this);\\\"
        onClick=\\\'javascript:{
        parent.fstatus.location.href=\\\"submit?q=force_web_infos+\\\\\\\"%s\\\\\\\"\\\";}'
        class=\\\"srb\\\"\\>DL\\</td\\>" (Url.encode_to_string w.url);
          Printf.bprintf buf "
              \\<td title=\\\"%s\\\" class=\\\"sr\\\"\\>%s\\</td\\>
              \\<td class=\\\"sr\\\"\\>%d\\</td\\>"  w.url w.kind w.period;
          Printf.bprintf buf "
              \\<td class=\\\"sr\\\"\\>%s\\</td\\>" w.url;
          Printf.bprintf buf "
              \\<td class=\\\"sr\\\"\\>%s\\</td\\>
              \\</tr\\>" (string_of_web_infos_state w.state);
              ) web_infos_table;
            end;
            Printf.bprintf buf "\\</table\\>\\</td\\>\\<tr\\>\\</table\\>\\</div\\>\\<P\\>";

            html_mods_table_header buf "web_infoTable" "vo" [
              ( Str, "srh", "Web kind", "Kind" );
              ( Str, "srh", "Description", "Type" ) ];

            html_mods_cntr_init ();
            List.iter (fun (kind, data) ->
                Printf.bprintf buf "\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr ());
                Printf.bprintf buf "
              \\<td class=\\\"sr\\\"\\>%s\\</td\\>
              \\<td class=\\\"sr\\\"\\>%s\\</td\\>" kind data.description
            ) !CommonWeb.file_kinds;

            Printf.bprintf buf "\\</table\\>\\</td\\>\\<tr\\>\\</table\\>\\</div\\>\\<P\\>";
            print_option_help o web_infos

          end
        else
            begin
              Printf.bprintf buf "kind / period / url / state :\n";
              Hashtbl.iter (fun key w ->
                  Printf.bprintf buf "%s ; %d ; %s; %s\n"
                    w.kind w.period w.url (string_of_web_infos_state w.state)
              ) web_infos_table;
              Printf.bprintf buf "\nAllowed values for kind:\n";
              List.iter (fun (kind, data) ->
                  Printf.bprintf buf "%s - %s\n" kind data.description
              ) !CommonWeb.file_kinds
            end;
        ""
    ), ":\t\t\t\t\tprint web_infos options";

    "options", Arg_multiple (fun args o ->
        let buf = o.conn_buf in
        match args with
          [] ->
            Printf.bprintf buf "Available sections for options: \n";

            List.iter (fun s ->
                Printf.bprintf buf "  $b%s$n\n" (section_name s);
            ) (sections downloads_ini);

            networks_iter (fun r ->
                List.iter (fun file ->
                    List.iter (fun s ->
                        Printf.bprintf buf "  $b%s::%s$n\n"
                          r.network_name
                          (section_name s);
                    ) (sections file)
                ) r.network_config_file
            );
            "\n\nUse 'options section' to see options in this section"

        | ss ->

            let print_section name prefix (s: options_section) =
              if List.mem name ss then
                Printf.bprintf buf "Options in section $b%s$n:\n" name;
              List.iter (fun o ->
                  Printf.bprintf buf "  %s [$r%s%s$n]= $b%s$n\n"
                    (if o.option_desc = "" then
                      o.option_name else o.option_desc)
                  prefix o.option_name o.option_value
              ) (strings_of_section_options "" s)
            in
            List.iter (fun s ->
                print_section (section_name s) "" s
            ) (sections downloads_ini);

            networks_iter (fun r ->
                List.iter (fun file ->
                    List.iter (fun s ->
                        print_section
                          (Printf.sprintf "%s::%s" r.network_name
                            (section_name s)) (r.network_shortname ^ "-") s
                    ) (sections file)
                ) r.network_config_file
            );

            "\nUse '$rset option \"value\"$n' to change a value where options is
the name between []"
    ), ":\t\t\t\t$bprint options values by section$n";

  ]

(*************************************************************************)
(*                                                                       *)
(*                         Driver/Sharing                                *)
(*                                                                       *)
(*************************************************************************)

let _ =
  register_commands "Driver/Sharing"
    [

    "reshare", Arg_none (fun o ->
        let buf = o.conn_buf in
        shared_check_files ();
        if o.conn_output = HTML then
          html_mods_table_one_row buf "serversTable" "servers" [
            ("", "srh", "Reshare check done"); ]
        else
          Printf.bprintf buf "Reshare check done";
        _s ""
    ), ":\t\t\t\tcheck shared files for removal";

     "debug_disk", Arg_one (fun arg o ->
         let buf = o.conn_buf in
         let print_i64o = function
           | None -> "Unknown"
           | Some v -> Printf.sprintf "%Ld" v in
         let print_io = function
           | None -> "Unknown"
           | Some v -> Printf.sprintf "%d" v in
         Printf.bprintf buf "working on dir %s\n" arg;
         Printf.bprintf buf "bsize %s\n" (print_i64o (Unix32.bsize arg));
         Printf.bprintf buf "blocks %s\n" (print_i64o (Unix32.blocks arg));
         Printf.bprintf buf "bfree %s\n" (print_i64o (Unix32.bfree arg));
         Printf.bprintf buf "bavail %s\n" (print_i64o (Unix32.bavail arg));
         Printf.bprintf buf "fnamelen %s\n" (print_io (Unix32.fnamelen arg));
         Printf.bprintf buf "filesystem %s\n" (Unix32.filesystem arg);
         let print_i64o_amount = function
           | None -> "Unknown"
           | Some v -> Printf.sprintf "%Ld - %s" v (size_of_int64 v) in
         Printf.bprintf buf "disktotal %s\n" (print_i64o_amount (Unix32.disktotal arg));
         Printf.bprintf buf "diskfree %s\n" (print_i64o_amount (Unix32.diskfree arg));
         Printf.bprintf buf "diskused %s\n" (print_i64o_amount (Unix32.diskused arg));
         let print_percento = function
           | None -> "Unknown"
           | Some p -> Printf.sprintf "%d%%" p in
         Printf.bprintf buf "percentused %s\n" (print_percento (Unix32.percentused arg));
         Printf.bprintf buf "percentfree %s\n" (print_percento (Unix32.percentfree arg));
         let stat = Unix.LargeFile.stat arg in
         Printf.bprintf buf "\nstat_device %d\n" stat.Unix.LargeFile.st_dev;
         Printf.bprintf buf "stat_inode %d\n" stat.Unix.LargeFile.st_ino;

         _s ""
     ), "debug command (example: disk .)";

     "debug_dir", Arg_one (fun arg o ->
         let buf = o.conn_buf in
         let filelist = Unix2.list_directory arg in
         Printf.bprintf buf "%d entries in dir %s\n" (List.length filelist) arg;
         List.iter (fun file ->
           Printf.bprintf buf "%s\n     %s\nMime %s\n\n"
             file
             (match Magic.M.magic_fileinfo (Filename.concat arg file) false with
                None -> "unknown"
              | Some fileinfo -> fileinfo)
             (match Magic.M.magic_fileinfo (Filename.concat arg file) true with
                None -> "unknown"
              | Some fileinfo -> fileinfo)
         ) filelist;
         _s ""
     ), "debug command (example: disk .)";

     "debug_fileinfo", Arg_one (fun arg o ->
         let buf = o.conn_buf in
         (try
            let module U = Unix.LargeFile in
            let s = U.stat arg in
            Printf.bprintf buf "st_dev %d\n" s.U.st_dev;
            Printf.bprintf buf "st_ino %d\n" s.U.st_ino;
            Printf.bprintf buf "st_uid %d\n" s.U.st_uid;
            Printf.bprintf buf "st_gid %d\n" s.U.st_gid;
            Printf.bprintf buf "st_size %Ld\n" s.U.st_size;
            Printf.bprintf buf "st_atime %s\n" (Date.to_full_string s.U.st_atime);
            Printf.bprintf buf "st_mtime %s\n" (Date.to_full_string s.U.st_mtime);
            Printf.bprintf buf "st_ctime %s\n" (Date.to_full_string s.U.st_ctime);
            let user,group = Unix32.owner arg in
            Printf.bprintf buf "username %s\n" user;
            Printf.bprintf buf "groupname %s\n" group;
          with e -> Printf.bprintf buf "Error %s when opening %s\n" (Printexc2.to_string e) arg);
         _s ""
     ), "debug command (example: file .)";

     "debug_rlimit", Arg_none (fun o ->
         let buf = o.conn_buf in
         let cpu = Unix2.ml_getrlimit Unix2.RLIMIT_CPU in
         let fsize = Unix2.ml_getrlimit Unix2.RLIMIT_FSIZE in
         let data = Unix2.ml_getrlimit Unix2.RLIMIT_DATA in
         let stack = Unix2.ml_getrlimit Unix2.RLIMIT_STACK in
         let core = Unix2.ml_getrlimit Unix2.RLIMIT_CORE in
         let rss = Unix2.ml_getrlimit Unix2.RLIMIT_RSS in
         let nprof = Unix2.ml_getrlimit Unix2.RLIMIT_NPROF in
         let nofile = Unix2.ml_getrlimit Unix2.RLIMIT_NOFILE in
         let memlock = Unix2.ml_getrlimit Unix2.RLIMIT_MEMLOCK in
         let rlimit_as = Unix2.ml_getrlimit Unix2.RLIMIT_AS in
         Printf.bprintf buf "cpu %d %d\n" cpu.Unix2.rlim_cur cpu.Unix2.rlim_max;
         Printf.bprintf buf "fsize %d %d\n" fsize.Unix2.rlim_cur fsize.Unix2.rlim_max;
         Printf.bprintf buf "data %d %d\n" data.Unix2.rlim_cur data.Unix2.rlim_max;
         Printf.bprintf buf "stack %d %d\n" stack.Unix2.rlim_cur stack.Unix2.rlim_max;
         Printf.bprintf buf "core %d %d\n" core.Unix2.rlim_cur core.Unix2.rlim_max;
         Printf.bprintf buf "rss %d %d\n" rss.Unix2.rlim_cur rss.Unix2.rlim_max;
         Printf.bprintf buf "nprof %d %d\n" nprof.Unix2.rlim_cur nprof.Unix2.rlim_max;
         Printf.bprintf buf "nofile %d %d\n" nofile.Unix2.rlim_cur nofile.Unix2.rlim_max;
         Printf.bprintf buf "memlock %d %d\n" memlock.Unix2.rlim_cur memlock.Unix2.rlim_max;
         Printf.bprintf buf "as %d %d\n" rlimit_as.Unix2.rlim_cur rlimit_as.Unix2.rlim_max;
         _s ""
     ), "debug command";

    "shares", Arg_none (fun o ->
        if user2_is_admin o.conn_user.ui_user then begin
        let buf = o.conn_buf in

        if use_html_mods o then begin
            Printf.bprintf buf "\\<div class=\\\"shares\\\"\\>\\<table class=main cellspacing=0 cellpadding=0\\>
\\<tr\\>\\<td\\>
\\<table cellspacing=0 cellpadding=0  width='100%%'\\>\\<tr\\>
\\<td class=downloaded width=100%%\\>\\</td\\>
\\<td nowrap class=\\\"fbig pr\\\"\\>\\<a onclick=\\\"javascript: {
                   var getdir = prompt('Input: <priority#> <directory> [<strategy>] (surround dir with quotes if necessary)','0 /home/mldonkey/share')
                   parent.fstatus.location.href='submit?q=share+' + encodeURIComponent(getdir);
                   setTimeout('window.location.reload()',1000);
                    }\\\"\\>Add Share\\</a\\>
\\</td\\>
\\</tr\\>\\</table\\>
\\</td\\>\\</tr\\>
\\<tr\\>\\<td\\>";

            html_mods_table_header buf "sharesTable" "shares" [
               ( Str, "srh ac", "Click to unshare directory", "Unshare" ) ;
               ( Num, "srh ar", "Priority", "P" ) ;
               ( Str, "srh", "Directory", "Directory" ) ;
               ( Str, "srh", "Strategy", "Strategy" ) ;
               ( Num, "srh ar", "HDD used", "used" ) ;
               ( Num, "srh ar", "HDD free", "free" ) ;
               ( Num, "srh ar", "% free", "% free" ) ;
               ( Str, "srh", "Filesystem", "FS" ) ];

            html_mods_cntr_init ();
            List.iter (fun shared_dir ->
                let dir = shared_dir.shdir_dirname in
                Printf.bprintf buf "\\<tr class=\\\"dl-%d\\\"\\>
        \\<td title=\\\"Click to unshare this directory\\\"
        onMouseOver=\\\"mOvr(this);\\\"
        onMouseOut=\\\"mOut(this);\\\"
        onClick=\\\'javascript:{
        parent.fstatus.location.href=\\\"submit?q=unshare+\\\\\\\"%s\\\\\\\"\\\"
        setTimeout(\\\"window.location.reload()\\\",1000);}'
        class=\\\"srb\\\"\\>Unshare\\</td\\>
        \\<td class=\\\"sr ar\\\"\\>%d\\</td\\>
        \\<td class=\\\"sr\\\"\\>%s\\</td\\>
        \\<td class=\\\"sr\\\"\\>%s\\</td\\>
        \\<td class=\\\"sr ar\\\"\\>%s\\</td\\>
        \\<td class=\\\"sr ar\\\"\\>%s\\</td\\>
        \\<td class=\\\"sr ar\\\"\\>%s\\</td\\>
        \\<td class=\\\"sr\\\"\\>%s\\</td\\>\\</tr\\>"
                (html_mods_cntr ())
                (Url.encode_to_string dir)
                shared_dir.shdir_priority
                dir
                shared_dir.shdir_strategy
                (match Unix32.diskused dir with
                | None -> "---"
                | Some du -> size_of_int64 du)
                (match Unix32.diskfree dir with
                | None -> "---"
                | Some df -> size_of_int64 df)
                (match Unix32.percentfree dir with
                | None -> "---"
                | Some p -> Printf.sprintf "%d%%" p)
                (Unix32.filesystem dir);
            )
            !!shared_directories;
  
            Printf.bprintf buf "\\</table\\>\\</td\\>\\<tr\\>\\</table\\>\\</div\\>\\<P\\>";
            print_option_help o shared_directories;
            Printf.bprintf buf "\\<P\\>";

            html_mods_big_header_start buf "sharesTable" ["Share strategies"];
            html_mods_table_header buf "sharesTable" "shares" [
               ( Str, "srh", "Name", "Name" ) ;
               ( Str, "srh", "Incoming", "Incoming" ) ;
               ( Str, "srh", "Directories", "Directories" ) ;
               ( Str, "srh", "Recursive", "Recursive" ) ;
               ( Num, "srh", "Minsize", "Minsize" ) ;
               ( Num, "srh", "Maxsize", "Maxsize" ) ;
               ( Str, "srh", "Extensions", "Extensions" ) ];

            html_mods_cntr_init ();

            let int64_print v =
              if v = Int64.max_int then "unlimited" else Int64ops.int64_to_human_readable v in

            List.iter (fun (s,t) ->
                Printf.bprintf buf "\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr ());
                html_mods_td buf [
                    ("", "sr", s);
                    ("", "sr", string_of_bool t.sharing_incoming);
                    ("", "sr", string_of_bool t.sharing_directories);
                    ("", "sr", string_of_bool t.sharing_recursive);
                    ("", "sr", (int64_print t.sharing_minsize));
                    ("", "sr", (int64_print t.sharing_maxsize));
                    ("", "sr", (String.concat " " t.sharing_extensions));
                    ];
                Printf.bprintf buf "\\</tr\\>\n"
              ) !!sharing_strategies;

          end
        else
          begin

            Printf.bprintf buf "Shared directories:\n";
            List.iter (fun sd ->
                Printf.bprintf buf "  %d %s %s\n"
                sd.shdir_priority sd.shdir_dirname sd.shdir_strategy)
            !!shared_directories;

          end;
        ""
          end
        else
          _s "You are not allowed to list shared directories"
    ), ":\t\t\t\tprint shared directories";

    "share", Arg_multiple (fun args o ->
        if user2_is_admin o.conn_user.ui_user then begin
        let (prio, arg, strategy) = match args with
          | [prio; arg; strategy] -> int_of_string prio, arg, strategy
          | [prio; arg] -> int_of_string prio, arg, "only_directory"
          | [arg] -> 0, arg, "only_directory"
          | _  -> failwith "Bad number of arguments"
        in

        let shdir = {
            shdir_dirname = arg;
            shdir_priority = prio;
            shdir_networks = [];
            shdir_strategy = strategy;
          } in

        if Unix2.is_directory arg then
          begin
            try
              let d = List.find (fun d -> d.shdir_dirname = arg) !!shared_directories in
              let old_prio = d.shdir_priority in
              d.shdir_priority <- prio;
              Printf.sprintf "prio of %s changed from %d to %d"
                d.shdir_dirname old_prio d.shdir_priority
            with Not_found ->
              shared_directories =:= shdir :: !!shared_directories;
              shared_add_directory shdir;
              Printf.sprintf "directory %s added%s"
                shdir.shdir_dirname
                (if shdir.shdir_priority <> 0 then
                    Printf.sprintf " with prio %d" shdir.shdir_priority
                 else "")
          end
        else
          "no such directory"
          end
        else
          _s "You are not allowed to share directories"
    ), "<priority> <dir> [<strategy>] :\tshare directory <dir> with <priority> [and sharing strategy <strategy>]";

    "unshare", Arg_one (fun arg o ->

        if user2_is_admin o.conn_user.ui_user then begin
        let found = ref false in
        shared_directories =:= List.filter (fun sd ->
            let diff = sd.shdir_dirname <> arg in
            if not diff then begin
              found := true;
              shared_iter (fun s ->
                let impl = as_shared_impl s in
                  if (Filename.dirname impl.impl_shared_fullname) = arg
                    then shared_unshare s
              )
            end;
            diff
        ) !!shared_directories;
        if !found then begin
            CommonShared.shared_check_files ();
            _s "directory removed"
          end else
          _s "directory already unshared"
          end
        else
          _s "You are not allowed to unshare directories"
    ), "<dir> :\t\t\t\tunshare directory <dir>";

    "upstats", Arg_none (fun o ->
        if not (user2_can_view_uploads o.conn_user.ui_user) then
          print_command_result o "You are not allowed to see upload statistics"
        else
          begin
            let list = ref [] in
            shared_iter (fun s ->
              let impl = as_shared_impl s in
              list := impl :: !list
            );
            print_upstats o !list None;
          end;
        _s ""
    ), ":\t\t\t\tstatistics on upload";

    "links", Arg_multiple (fun args o ->
        let buf = o.conn_buf in
        if not (user2_can_view_uploads o.conn_user.ui_user) then
          print_command_result o "You are not allowed to see shared files list"
        else begin

        let list = Hashtbl.create !shared_counter in

        let compute_shares () =
          shared_iter (fun s ->
            let impl = as_shared_impl s in
            try
              ignore (Hashtbl.find list impl.impl_shared_id)
            with Not_found ->
                Hashtbl.add list impl.impl_shared_id {
                    filename = impl.impl_shared_codedname;
                    filesize = impl.impl_shared_size;
                    fileid = impl.impl_shared_id;
                })
        in

        let compute_downloads () =
          List.iter (fun f ->
            try
              ignore (Hashtbl.find list f.file_md4)
            with Not_found ->
                Hashtbl.add list f.file_md4 {
                    filename = f.file_name;
                    filesize = f.file_size;
                    fileid = f.file_md4;
                }) (List2.tail_map file_info 
                      (user2_filter_files !!files o.conn_user.ui_user))
        in

        let list =
          List.sort ( fun f1 f2 ->
            String.compare
              (Filename.basename f1.filename)
              (Filename.basename f2.filename)
        )
        (match args with
         | ["downloading"] -> compute_downloads (); Hashtbl2.to_list list
         | ["shared"] -> compute_shares (); Hashtbl2.to_list list
         | _ -> compute_shares (); compute_downloads (); Hashtbl2.to_list list)
        in

        List.iter (fun f ->
          if (f.fileid <> Md4.null) then
            Printf.bprintf buf "%s\n" (file_print_ed2k_link
              (Filename.basename f.filename) f.filesize f.fileid);
        ) list;
        end;
        "Done"
    ), "[downloading|shared|empty for all]: list links of shared files";

    "uploaders", Arg_none (fun o ->
        let buf = o.conn_buf in

        if not (user2_can_view_uploads o.conn_user.ui_user) then
          print_command_result o "You are not allowed to see uploaders list"
        else begin

        let nuploaders = Intmap.length !uploaders in
        if use_html_mods o then
          begin
            html_mods_cntr_init ();
            Printf.bprintf buf "\\<div class=\\\"uploaders\\\"\\>";
            html_mods_table_one_row buf "uploadersTable" "uploaders" [
              ("", "srh", Printf.sprintf "Total upload slots: %d (%d) | Pending slots: %d\n" nuploaders
                (Fifo.length CommonUploads.upload_clients)
                (Intmap.length !CommonUploads.pending_slots_map)); ];
            if nuploaders > 0 then

              begin

                html_mods_table_header buf "uploadersTable" "uploaders" ([
                  ( Num, "srh ac", "Client number", "Num" ) ;
                  ( Str, "srh", "Network", "Network" ) ;
                  ( Str, "srh", "Connection type [I]ndirect [D]irect", "C" ) ;
                  ( Str, "srh", "Client name", "Client name" ) ;
                  ( Str, "srh", "Secure User Identification [N]one, [P]assed, [F]ailed", "S" ) ;
                  ( Str, "srh", "IP address", "IP address" ) ;
                  ] @ (if Geoip.active () then [( Str, "srh", "Country Code/Name", "CC" )] else []) @ [
                  ( Str, "srh", "Connected time (minutes)", "CT" ) ;
                  ( Str, "srh", "Client brand", "CB" ) ;
                  ( Str, "srh", "Client release", "CR" ) ;
                  ] @
                  (if !!emule_mods_count then [( Str, "srh", "eMule MOD", "EM" )] else [])
                  @ [
                  ( Num, "srh ar", "Total DL bytes from this client for all files", "tDL" ) ;
                  ( Num, "srh ar", "Total UL bytes to this client for all files", "tUL" ) ;
                  ( Num, "srh ar", "Session DL bytes from this client for all files", "sDL" ) ;
                  ( Num, "srh ar", "Session UL bytes to this client for all files", "sUL" ) ;
                  ( Str, "srh ar", "Slot kind", "Slot" ) ;
                  ( Str, "srh", "Filename", "Filename" ) ]);

                List.iter (fun c ->
                    try
                      let i = client_info c in
                      if is_connected i.client_state then begin

                          Printf.bprintf buf "\\<tr class=\\\"dl-%d\\\"
                        title=\\\"[%d] Add as friend (avg: %.1f KB/s)\\\"
                        onMouseOver=\\\"mOvr(this);\\\"
                        onMouseOut=\\\"mOut(this);\\\"
                        onClick=\\\"parent.fstatus.location.href='submit?q=friend_add+%d'\\\"\\>"
                            (html_mods_cntr ()) (client_num c)
                          (Int64.to_float i.client_session_uploaded /. 1024. /.
                              float_of_int (max 1 ((last_time ()) - i.client_connect_time)))
                          (client_num c);

                          html_mods_td buf [
                            ("", "sr", Printf.sprintf "%d" (client_num c)); ];

                          let ips,cc,cn = string_of_kind_geo i.client_kind i.client_country_code in

                          client_print_html c o;
                          html_mods_td buf ([
                            ("", "sr", (match i.client_sui_verified with
                              | None -> "N"
                               | Some b -> if b then "P" else "F"
                            )); 
                            ("", "sr", ips);
                            ] @ (if Geoip.active () then [(cn, "sr", CommonPictures.flag_html cc)] else []) @ [
                            ("", "sr", Printf.sprintf "%d" (((last_time ()) - i.client_connect_time) / 60));
                            (client_software i.client_software i.client_os, "sr", client_software_short i.client_software i.client_os);
                            ("", "sr", i.client_release);
                            ] @
                            (if !!emule_mods_count then [("", "sr", i.client_emulemod)] else [])
                            @ [
                            ("", "sr ar", size_of_int64 i.client_total_downloaded);
                            ("", "sr ar", size_of_int64 i.client_total_uploaded);
                            ("", "sr ar", size_of_int64 i.client_session_downloaded);
                            ("", "sr ar", size_of_int64 i.client_session_uploaded);
                            (let text1, text2 =
                                match client_slot c with
                                | FriendSlot -> "Friend", "F"
                                | ReleaseSlot -> "Release", "R"
                                | SmallFileSlot -> "Small file", "S"
                                | PrioSlot dir -> "Prio dir: " ^ dir, "P"
                                | _ -> "", "" in text1, "sr ar", text2);
                            ("", "sr", (match i.client_upload with
                                  Some f -> shorten f !!max_name_len
                                | None -> "") ) ]);

                          Printf.bprintf buf "\\</tr\\>"
                        end
                    with _ -> ()
                ) (List.sort
                    (fun c1 c2 -> compare (client_num c1) (client_num c2))
                  (Intmap.to_list !uploaders));
                Printf.bprintf buf "\\</table\\>\\</div\\>";
              end;

            if !!html_mods_show_pending && Intmap.length !CommonUploads.pending_slots_map > 0 then

              begin
                Printf.bprintf buf "\\<br\\>\\<br\\>";
                html_mods_table_header buf "uploadersTable" "uploaders" ([
                  ( Num, "srh ac", "Client number", "Num" ) ;
                  ( Str, "srh", "Network", "Network" ) ;
                  ( Str, "srh", "Connection type [I]ndirect [D]irect", "C" ) ;
                  ( Str, "srh", "Client name", "Client name" ) ;
                  ( Str, "srh", "Secure User Identification [N]one, [P]assed, [F]ailed", "S" ) ;
                  ( Str, "srh", "IP address", "IP address" ) ;
                  ] @ (if Geoip.active () then [( Str, "srh", "Country Code/Name", "CC" )] else []) @ [
                  ( Str, "srh", "Client brand", "CB" ) ;
                  ( Str, "srh", "Client release", "CR" ) ;
                  ] @
                  (if !!emule_mods_count then [( Str, "srh", "eMule MOD", "EM" )] else [])
                  @ [
                  ( Num, "srh ar", "Total DL bytes from this client for all files", "tDL" ) ;
                  ( Num, "srh ar", "Total UL bytes to this client for all files", "tUL" ) ;
                  ( Num, "srh ar", "Session DL bytes from this client for all files", "sDL" ) ;
                  ( Num, "srh ar", "Session UL bytes to this client for all files", "sUL" ) ;
                  ( Str, "srh", "Filename", "Filename" ) ]);

                Intmap.iter (fun cnum c ->

                    try
                      let i = client_info c in
                      let ips,cc,cn = string_of_kind_geo i.client_kind i.client_country_code in

                      Printf.bprintf buf "\\<tr class=\\\"dl-%d\\\"
                                        title=\\\"Add as Friend\\\" onMouseOver=\\\"mOvr(this);\\\" onMouseOut=\\\"mOut(this);\\\"
                                        onClick=\\\"parent.fstatus.location.href='submit?q=friend_add+%d'\\\"\\>"
                        (html_mods_cntr ()) cnum;

                      html_mods_td buf [
                        ("", "sr", Printf.sprintf "%d" (client_num c)); ];

                      client_print_html c o;

                      html_mods_td buf ([
                        ("", "sr", (match i.client_sui_verified with
                          | None -> "N"
                          | Some b -> if b then "P" else "F"
                        )); 
                        ("", "sr", ips);
                        ] @ (if Geoip.active () then [(cn, "sr", CommonPictures.flag_html cc)] else []) @ [
                        (client_software i.client_software i.client_os, "sr", client_software_short i.client_software i.client_os);
                        ("", "sr", i.client_release);
                        ] @
                        (if !!emule_mods_count then [("", "sr", i.client_emulemod )] else [])
                        @ [
                        ("", "sr ar", size_of_int64 i.client_total_downloaded);
                        ("", "sr ar", size_of_int64 i.client_total_uploaded);
                        ("", "sr ar", size_of_int64 i.client_session_downloaded);
                        ("", "sr ar", size_of_int64 i.client_session_uploaded);
                        ("", "sr", (match i.client_upload with
                              Some f -> shorten f !!max_name_len
                            | None -> "") ) ]);

                      Printf.bprintf buf "\\</tr\\>";
                    with _ -> ();

                ) !CommonUploads.pending_slots_map;
                Printf.bprintf buf "\\</table\\>\\</div\\>";
              end;
            Printf.bprintf buf "\\</div\\>"
          end
        else
          begin
            Intmap.iter (fun _ c ->
                try
                  let i = client_info c in
                  client_print c o;
                  Printf.bprintf buf "client: %s downloaded: %s uploaded: %s\n" i.client_software (Int64.to_string i.client_total_downloaded) (Int64.to_string i.client_total_uploaded);
                  match i.client_upload with
                    Some cu ->
                      Printf.bprintf buf "      filename: %s\n" cu
                  | None -> ()
                with _ ->
                    Printf.bprintf buf "no info on client %d\n" (client_num c )
            ) !uploaders;

            Printf.bprintf buf "Total upload slots: %d (%d) | Pending slots: %d\n" nuploaders
              (Fifo.length CommonUploads.upload_clients)
            (Intmap.length !CommonUploads.pending_slots_map);
          end
      end;
      ""
    ), ":\t\t\t\tshow users currently uploading";


  ]

(*************************************************************************)
(*                                                                       *)
(*                         Driver/Downloads                              *)
(*                                                                       *)
(*************************************************************************)

(* TODO richer condition language *)
let parse_filter args =
  match args with
  | ["where";"priority";(">"|"<"|">="|"<="|"="|"<>" as op);n] ->
    let n = int_of_string n in
    let op = match op with
    | ">" -> (>)
    | "<" -> (<)
    | ">=" -> (>=)
    | "<=" -> (<=)
    | "=" -> (=)
    | "<>" -> (<>)
    | _ -> assert false
    in
    `Filter (fun file -> op (file_priority file) n)
  | ["all"] -> `All
  | l -> `Files (List.map int_of_string l)

let filter_files args k =
  match parse_filter args with
  | `All -> List.iter k !!files
  | `Filter filter -> List.iter (fun file -> if filter file then k file) !!files
  | `Files l -> List.iter begin fun num ->
      match try Some (file_find num) with _ -> None with
      | None -> ()
      | Some file -> k file
    end l

let () =
  let resume_alias s = s, Arg_multiple (fun args o ->
    filter_files args (fun file -> file_resume file o.conn_user.ui_user);
    ""
    ), "<nums|all|where filter> :\t\t\tresume a paused download (use arg 'all' for all files)"
  in
  register_commands "Driver/Downloads"
    [

    "priority", Arg_multiple (fun args o ->
        let buf = o.conn_buf in
        match args with
          p :: files ->
            let absolute, p = if String2.check_prefix p "=" then
                true, int_of_string (String2.after p 1)
              else false, int_of_string p
            in
            filter_files files begin fun file ->
              let priority = if absolute then p else file_priority file + p in
              let priority = max (-100) (min 100 priority) in
              set_file_priority file priority;
              Printf.bprintf buf "Setting priority of %s to %d\n"
                (file_best_name file) (file_priority file)
            end;
            force_download_quotas ();
            _s "done"
        | [] -> "Bad number of args"

    ), "<priority> <nums|all|where filter> :\tchange file priorities";

    "download_order", Arg_two (fun num v o ->
        try
          let file = file_find (int_of_string num) in
          (match v with
          | "linear" -> ignore (CommonFile.file_download_order file (Some CommonTypes.LinearStrategy))
          | _ -> ignore (CommonFile.file_download_order file (Some CommonTypes.AdvancedStrategy)));
          _s (Printf.sprintf "Changed download order of %s to %s"
                (file_best_name file) (file_print_download_order file))
        with e -> Printf.sprintf "Exception %s" (Printexc2.to_string e)
    ), "<file number> <random|linear> :\tchange download order of file blocks (default random, with first and last block first)";

    "confirm", Arg_one (fun arg o ->
        match String.lowercase arg with
          "yes" | "y" | "true" ->
            List.iter (fun file ->
                try
                  file_cancel file o.conn_user.ui_user
                with e ->
                    lprintf "Exception %s in cancel file %d\n"
                      (Printexc2.to_string e) (file_num file)
            ) !to_cancel;
            to_cancel := [];
            _s "Files cancelled"
        | "no" | "n" | "false" ->
            to_cancel := [];
            _s "cancel aborted"
        | "what" | "w" ->
            files_to_cancel o
        | _ -> failwith "Invalid argument"
    ), "<yes|no|what> :\t\t\tconfirm cancellation";

    "test_recover", Arg_one (fun num o ->

        let num = int_of_string num in
        let file = file_find num in
        let segments = CommonFile.recover_bytes file in
        let buf = o.conn_buf in
        Printf.bprintf buf "Segments:\n";
        let downloaded = ref zero in
        List.iter (fun (begin_pos, end_pos) ->
            Printf.bprintf buf "   %Ld - %Ld\n" begin_pos end_pos;
            downloaded := !downloaded ++ (end_pos -- begin_pos);
        ) segments;
        Printf.sprintf "Downloaded: %Ld\n" !downloaded
    ), "<num> :\t\t\tprint the segments downloaded in file";


    "cancel", Arg_multiple (fun args o ->

        let file_cancel num =
          if not (List.memq num !to_cancel) then
            to_cancel := num :: !to_cancel
        in
        if args = ["all"] && user2_is_admin o.conn_user.ui_user then
          List.iter (fun file ->
              file_cancel file
          ) !!files
        else
          List.iter (fun num ->
              let num = int_of_string num in
              List.iter (fun file ->
                  if (as_file_impl file).impl_file_num = num then begin
                      lprintf "TRY TO CANCEL FILE\n";
                      file_cancel file
                    end
              ) !!files) args;
        files_to_cancel o
    ), "<num|all> :\t\t\tcancel download (use arg 'all' for all files)";

    "downloaders", Arg_none (fun o ->
        let buf = o.conn_buf in

        if use_html_mods o then
          html_mods_table_header buf "downloadersTable" "downloaders" ([
              ( Num, "srh ac", "Client number (click to add as friend)", "Num" ) ;
              ( Str, "srh", "Client state", "CS" ) ;
              ( Str, "srh", "Client name", "Name" ) ;
              ( Str, "srh", "Client brand", "CB" ) ;
              ( Str, "srh", "Client release", "CR" ) ;
            ] @
              (if !!emule_mods_count then [( Str, "srh", "eMule MOD", "EM" )] else [])
            @ [
              ( Str, "srh", "Overnet [T]rue, [F]alse", "O" ) ;
              ( Num, "srh ar", "Connected time (minutes)", "CT" ) ;
              ( Str, "srh", "Connection [I]ndirect, [D]irect", "C" ) ;
              ( Str, "srh", "Secure User Identification [N]one, [P]assed, [F]ailed", "S" ) ;
              ( Str, "srh", "IP address", "IP address" ) ;
              ] @ (if Geoip.active () then [( Str, "srh", "Country Code/Name", "CC" )] else []) @ [ 
              ( Num, "srh ar", "Total UL bytes to this client for all files", "tUL");
              ( Num, "srh ar", "Total DL bytes from this client for all files", "tDL");
              ( Num, "srh ar", "Session UL bytes to this client for all files", "sUL");
              ( Num, "srh ar", "Session DL bytes from this client for all files", "sDL");
              ( Str, "srh", "Filename", "Filename" ) ]);

        let counter = ref 0 in

        List.iter
          (fun file ->
            if (CommonFile.file_downloaders file o !counter) then counter := 0 else counter := 1;
        ) (user2_filter_files !!files o.conn_user.ui_user);

        if use_html_mods o then Printf.bprintf buf "\\</table\\>\\</div\\>";

        ""
    ) , ":\t\t\t\tdisplay downloaders list";

    "verify_chunks", Arg_multiple (fun args o ->
        let buf = o.conn_buf in
        match args with
        | [] -> ""
        | "all"::[] ->
            Printf.bprintf buf "Verifying chunks of all files";
            List.iter file_check !!files;
            _s "done"
        | l ->
            let l = List.map int_of_string l in
            List.iter
              (fun file ->
                if List.mem (file_num file) l then
                begin
                  Printf.bprintf  buf "Verifying chunks of file %d : %s" (file_num file) (file_best_name file);
                  file_check file;
                end)
              !!files;
            ""
    ), "<num|all> :\t\t\tverify chunks of file <num> (use 'all' for all files)";

    "pause", Arg_multiple (fun args o ->
      filter_files args (fun file -> file_pause file o.conn_user.ui_user);
      ""
    ), "<nums|all|where priority < prio> :\t\t\tpause a download (use arg 'all' for all files)";

    resume_alias "resume";
    resume_alias "unpause";
    resume_alias "continue";

    "release", Arg_one (fun arg o ->
        let num = int_of_string arg in
        let file = file_find num in
        let old_state = file_release file in
        set_file_release file (not (file_release file)) o.conn_user.ui_user;
        Printf.sprintf "%s, file: %s"
          (match old_state, file_release file with
              true, false -> "deactivated release state"
            | false, true -> "activated release state"
            | _ -> "unchanged status, enough rights?")
          (shorten (file_best_name file) !!max_name_len)
    ), "<num> :\t\t\t\tchange release state of a download";

    "commit", Arg_none (fun o ->
        List.iter (fun file ->
            file_commit file
        ) !!done_files;
        let buf = o.conn_buf in
        if o.conn_output = HTML then
          html_mods_table_one_row buf "serversTable" "servers" [
            ("", "srh", "Committed"); ]
        else
          Printf.bprintf buf "Committed";
        ""
    ) , ":\t\t\t\t$bmove downloaded files to incoming directory$n";

    "vd", Arg_multiple (fun args o ->
        let buf = o.conn_buf in
        let list = user2_filter_files !!files o.conn_user.ui_user in
        let filelist = List2.tail_map file_info list in
        match args with
          | ["queued"] ->
              let list = List.filter ( fun f -> f.file_state = FileQueued ) filelist in
              DriverInteractive.display_active_file_list buf o list;
              ""
          | ["paused"] ->
              let list = List.filter ( fun f -> f.file_state = FilePaused ) filelist in
              DriverInteractive.display_active_file_list buf o list;
              ""
          | ["downloading"] ->
              let list = List.filter ( fun f -> f.file_state = FileDownloading ) filelist in
              DriverInteractive.display_file_list buf o list;
              ""
          | [arg] ->
            let num = int_of_string arg in
            if o.conn_output = HTML then
              begin
                if use_html_mods o then
                  Printf.bprintf buf "\\<div class=\\\"sourcesTable al\\\"\\>\\<table cellspacing=0 cellpadding=0\\>
                                \\<tr\\>\\<td\\>
                                \\<table cellspacing=0 cellpadding=0 width=100%%\\>\\<tr\\>
                                \\<td nowrap class=\\\"fbig\\\"\\>\\<a onclick=\\\"javascript:window.location.href='files'\\\"\\>Display all files\\</a\\>\\</td\\>
                                \\<td nowrap class=\\\"fbig\\\"\\>\\<a onClick=\\\"javascript:parent.fstatus.location.href='submit?q=verify_chunks+%d'\\\"\\>Verify chunks\\</a\\>\\</td\\>
                                \\<td nowrap class=\\\"fbig\\\"\\>\\<a onClick=\\\"javascript:window.location.href='preview_download?q=%d'\\\"\\>Preview\\</a\\>\\</td\\>
                                \\<td nowrap class=\\\"fbig\\\"\\>\\<a onClick=\\\"javascript:window.location.href='submit?q=debug_get_download_prio+%d'\\\"\\>Debug\\</a\\>\\</td\\>
                                \\<td nowrap class=\\\"fbig pr\\\"\\>\\<a onclick=\\\"javascript:window.location.reload()\\\"\\>Reload\\</a\\>\\</td\\>
                                \\<td class=downloaded width=100%%\\>\\</td\\>
                                \\</tr\\>\\</table\\>
                                \\</td\\>\\</tr\\>
                                \\<tr\\>\\<td\\>" num num num
                else begin
                    Printf.bprintf  buf "\\<a href=\\\"files\\\"\\>Display all files\\</a\\>  ";
                    Printf.bprintf  buf "\\<a href=\\\"submit?q=verify_chunks+%d\\\"\\>Verify chunks\\</a\\>  " num;
                    Printf.bprintf  buf "\\<a href=\\\"submit?q=preview+%d\\\"\\>Preview\\</a\\> \n " num;
                  end
              end;
            List.iter
              (fun file -> if (as_file_impl file).impl_file_num = num then
                  CommonFile.file_print file o)
            list;
            List.iter
              (fun file -> if (as_file_impl file).impl_file_num = num then
                  CommonFile.file_print file o)
            !!done_files;
            ""
        | _ ->
            DriverInteractive.display_file_list buf o filelist;
            ""
    ), "[<num>|queued|paused|downloading] :\t$bview file info for download <num>, or lists of queued, paused or downloading files, or all downloads if no argument given$n";

    "preview", Arg_one (fun arg o ->

        let num = int_of_string arg in
        let file = file_find num in
        file_preview file;
        _s "done"
    ), "<file number> :\t\t\tstart previewer for file <file number>";

    "rename", Arg_two (fun arg new_name o ->
        let num = int_of_string arg in
        try
          let file = file_find num in
          set_file_best_name file new_name 0;
          Printf.sprintf (_b "Download %d renamed to %s") num (file_best_name file)
        with e -> Printf.sprintf (_b "No file number %d, error %s") num (Printexc2.to_string e)
    ), "<num> \"<new name>\" :\t\tchange name of download <num> to <new name>";

    "filenames_variability", Arg_none (fun o ->
      let list = List2.tail_map file_info
        (user2_filter_files !!files o.conn_user.ui_user) in
      DriverInteractive.filenames_variability o list;
      _s "done"
    ), ":\t\t\ttell which files have several very different names";

    "dllink", Arg_multiple (fun args o ->
        let url = String2.unsplit args ' ' in
        dllink_parse (o.conn_output = HTML) url o.conn_user.ui_user
        ), "<link> :\t\t\t\tdownload ed2k, sig2dat, torrent or other link";

    "dllinks", Arg_one (fun arg o ->
        let result = Buffer.create 100 in
        let file = File.to_string arg in
        let lines = String2.split_simplify file '\n' in
        List.iter (fun line ->
          Buffer.add_string result (dllink_parse (o.conn_output = HTML) line o.conn_user.ui_user);
          Buffer.add_string result (if o.conn_output = HTML then "\\<P\\>" else "\n")
        ) lines;
        (Buffer.contents result)
    ), "<file> :\t\t\tdownload all the links contained in the file";

  ]

(*************************************************************************)
(*                                                                       *)
(*                         Driver/Users                                  *)
(*                                                                       *)
(*************************************************************************)

let _ =
  register_commands "Driver/Users" [

    "useradd", Arg_multiple (fun args o ->
        let group_convert g =
          try
            if String.lowercase g = "none" || g = "" then None
            else Some (user2_group_find g).group_name
          with Not_found -> None
        in
        let (user, pass, group, cdir, mail, mdl) =
          match args with
          | [user; pass; group; cdir; mail; mdl] ->
              user, pass, (group_convert group), cdir, mail, (try int_of_string mdl with _ -> 0)
          | [user; pass; group; cdir; mail] -> user, pass, (group_convert group), cdir, mail, 0
          | [user; pass; group; cdir] -> user, pass, (group_convert group), cdir, "", 0
          | [user; pass; group] -> user, pass, (group_convert group), "", "", 0
          | [user; pass] -> user, pass, Some admin_group_name, "", "", 0
          | _ -> failwith "wrong parameters"
        in
        if user2_is_admin o.conn_user.ui_user
          || o.conn_user.ui_user.user_name = user then
          if user2_user_exists user then
            begin
              user2_user_set_password (user2_user_find user) pass;
              print_command_result o (Printf.sprintf "Password of user %s changed" user)
            end
          else
            begin
              match group with
              | None -> user2_user_add user (Md4.string pass)
                          ~groups:[] ~default_group:None ~commit_dir:cdir ~mail:mail ~max_dl:mdl ();
                        print_command_result o (Printf.sprintf "User %s added" user)
              | Some g -> user2_user_add user (Md4.string pass)
                            ~groups:[g] ~default_group:group ~commit_dir:cdir ~mail:mail ~max_dl:mdl ();
                          print_command_result o (Printf.sprintf "User %s added, group %s" user g)
            end
        else
          print_command_result o "You are not allowed to add users";
        _s ""
    ), "<user> <passwd> [<group>] [<commit_dir>] [<mail>] [<max_downloads>]: add new mldonkey user/change user password";

    "userdel", Arg_one (fun user o ->
        if user <> o.conn_user.ui_user.user_name then
          if user2_is_admin o.conn_user.ui_user then
            if user = (admin_user ()).user_name then
              print_command_result o "User 'admin' can not be removed"
            else
              try
                let u = user2_user_find user in
                let n = user2_num_user_dls u in
                if n <> 0 then print_command_result o
                  (Printf.sprintf "User %s has %d downloads, can not delete" user n)
                else
                  user2_user_remove user;
                  print_command_result o (Printf.sprintf "User %s removed" user)
              with
                Not_found -> print_command_result o (Printf.sprintf "User %s does not exist" user)
          else
            print_command_result o "You are not allowed to remove users"
        else
          print_command_result o "You can not remove yourself";
        _s ""
    ), "<user> :\t\t\tremove a mldonkey user";

    "usergroupadd", Arg_two (fun user group o ->
        if user2_is_admin o.conn_user.ui_user then
          begin
            try
              let u = user2_user_find user in
                begin
                  try
                    let g = user2_group_find group in
                    if List.mem g u.user_groups then
                      print_command_result o
                        (Printf.sprintf "User %s already member of group %s" u.user_name g.group_name)
                    else
                      begin
                        user2_user_add_group u g;
                        print_command_result o
                          (Printf.sprintf "Added group %s to user %s" g.group_name u.user_name)
                      end
                  with Not_found -> print_command_result o (Printf.sprintf "Group %s does not exist" group)
                end
            with Not_found -> print_command_result o (Printf.sprintf "User %s does not exist" user)
          end
        else
          print_command_result o "You are not allowed to add groups to a user";
        _s ""
    ), "<user> <group> :\t\tadd a group to a mldonkey user";

    "usergroupdel", Arg_two (fun user group o ->
        if user2_is_admin o.conn_user.ui_user
          || o.conn_user.ui_user.user_name = user then
          begin
            try
              let u = user2_user_find user in
                begin
                  try
                    let g = user2_group_find group in
                    if not (List.mem g u.user_groups) then
                      print_command_result o (Printf.sprintf "User %s is not member of group %s" user group)
                    else
                      if Some g = u.user_default_group then
                        print_command_result o (Printf.sprintf "Group %s is default group of user %s, can not remove. Use command userdgroup to change default_group." group user)
                      else
                        begin
                          let counter = ref 0 in
                          List.iter (fun f -> 
                            if file_owner f = u && file_group f = Some g then
                              begin
                                incr counter;
                                set_file_group f u.user_default_group
                              end
                          ) !!files;
                          user2_user_remove_group (user2_user_find user) (user2_group_find group);
                          print_command_result o (Printf.sprintf "Removed group %s from user %s%s"
                            group user
                            (if !counter = 0 then "" else Printf.sprintf ", changed file_group of %d file%s to default_group %s"
                               !counter (Printf2.print_plural_s !counter) (user2_print_group u.user_default_group)))
                        end
                  with Not_found -> print_command_result o (Printf.sprintf "Group %s does not exist" group)
                end
            with Not_found -> print_command_result o (Printf.sprintf "User %s does not exist" user)
          end

        else
          print_command_result o "You are not allowed to remove groups from a user";
        _s ""
    ), "<user> <group> :\t\tremove a group from a mldonkey user";

    "userdgroup", Arg_two (fun user group o ->
        if user2_is_admin o.conn_user.ui_user
          || o.conn_user.ui_user.user_name = user then
          begin
            try
              let u = user2_user_find user in
                begin
                  try
                    let g = if String.lowercase group = "none" then None else Some (user2_group_find group) in
                    let update_dgroup () =
                      match g with
                        None -> true
                      | Some g1 when List.mem g1 u.user_groups -> true
                      | _ -> false
                    in
                    if update_dgroup () then
                      begin
                        user2_user_set_default_group u g;
                        print_command_result o (Printf.sprintf "Changed default group of user %s to group %s" u.user_name (user2_print_user_default_group u))
                      end
                    else print_command_result o (Printf.sprintf "User %s is not member of group %s" u.user_name group)
                  with Not_found -> print_command_result o (Printf.sprintf "Group %s does not exist" group)
                end
            with Not_found -> print_command_result o (Printf.sprintf "User %s does not exist" user)
          end
        else
          print_command_result o "You are not allowed to change default group";
        _s ""
    ), "<user> <group|None> :\tchange user default group";

    "passwd", Arg_one (fun passwd o ->
        begin
          try
            let u = user2_user_find o.conn_user.ui_user.user_name in
            user2_user_set_password u passwd;
            print_command_result o (Printf.sprintf "Password of user %s changed" u.user_name)
          with Not_found -> print_command_result o (Printf.sprintf "User %s does not exist" o.conn_user.ui_user.user_name)
        end;
        _s ""
    ), "<passwd> :\t\t\tchange own password";

    "usermail", Arg_two (fun user mail o ->
        if user2_is_admin o.conn_user.ui_user
          || o.conn_user.ui_user.user_name = user then
          begin
            try
              let u = user2_user_find user in
              user2_user_set_mail u mail;
              print_command_result o (Printf.sprintf "User %s has new mail %s" user mail)
            with Not_found -> print_command_result o (Printf.sprintf "User %s does not exist" user)
          end
        else print_command_result o "You are not allowed to change mail addresses";
        _s ""
    ), "<user> <mail> :\t\tchange user mail address";

    "userdls", Arg_two (fun user dls o ->
        if user2_is_admin o.conn_user.ui_user then
          begin
            try
              let u = user2_user_find user in
              user2_user_set_dls u (int_of_string dls);
              print_command_result o (Printf.sprintf "User %s has now %s downloads allowed" user (user2_print_user_dls (user2_user_find user)))
            with Not_found -> print_command_result o (Printf.sprintf "User %s does not exist" user)
          end
        else print_command_result o "You are not allowed to change this value";
        _s ""
    ), "<user> <num> :\t\t\tchange number of allowed concurrent downloads";

    "usercommit", Arg_two (fun user dir o ->
        if user2_is_admin o.conn_user.ui_user
          || o.conn_user.ui_user.user_name = user then
          begin
            try
              let u = user2_user_find user in
              user2_user_set_commit_dir u dir;
              print_command_result o (Printf.sprintf "User %s has new commit dir %s" u.user_name u.user_commit_dir)
            with Not_found -> print_command_result o (Printf.sprintf "User %s does not exist" user)
          end
        else print_command_result o "You are not allowed to change this value";
        _s ""
    ), "<user> <dir> :\t\tchange user specific commit directory";

    "groupadd", Arg_two (fun group admin o ->
        let g_admin =
          try
            bool_of_string admin
          with _ -> false
        in
        if user2_is_admin o.conn_user.ui_user then
          if user2_group_exists group then
            print_command_result o (Printf.sprintf "Group %s already exists" group)
          else
            begin
              user2_group_add group g_admin;
              print_command_result o (Printf.sprintf "Group %s added" group)
            end
        else
          print_command_result o "You are not allowed to add a group";
        _s ""
    ), "<group> <admin: true|false> :\tadd new mldonkey group";

    "groupdel", Arg_one (fun group o ->
        if user2_is_admin o.conn_user.ui_user then
          begin
            try
              let g = user2_group_find group in
              let g_dls = user2_num_group_dls g in
              let g_mem = user2_num_group_members g in
              if g_dls <> 0 then
                print_command_result o
                  (Printf.sprintf "Can not remove group %s, it has %d download%s"
                    group g_dls (Printf2.print_plural_s g_dls))
              else
                if g_mem <> 0 then
                  print_command_result o
                    (Printf.sprintf "Can not remove group %s, it has %d member%s"
                      group g_mem (Printf2.print_plural_s g_mem))
                else
                  if g = admin_group () then
                    print_command_result o (Printf.sprintf "Can not remove system group %s" group)
                  else
                    begin
                      user2_group_remove g;
                      print_command_result o (Printf.sprintf "Removed group %s" group)
                    end
            with Not_found -> print_command_result o (Printf.sprintf "Group %s does not exist" group)
          end
        else
          print_command_result o "You are not allowed to remove users";
          _s ""
    ), "<group> :\t\t\tremove an unused mldonkey group";

    "groupadmin", Arg_two (fun group admin o ->
        if user2_is_admin o.conn_user.ui_user then
          begin
            try
              let g = user2_group_find group in
              if g = admin_group () then
                print_command_result o (Printf.sprintf "Can not change state of system group %s" group)
              else
                begin
                  user2_group_admin g (bool_of_string admin);
                  print_command_result o (Printf.sprintf "Changed admin status of group %s to %b" g.group_name g.group_admin)
                end
            with Not_found -> print_command_result o (Printf.sprintf "Group %s does not exist" group)
          end
        else
          print_command_result o "You are not allowed to change group admin status";
        _s ""
    ), "<group> <true|false> :\tchange group admin status";

    "users", Arg_none (fun o ->
        let buf = o.conn_buf in
        if user2_is_admin o.conn_user.ui_user then begin

        if use_html_mods o then begin
            Printf.bprintf buf "\\<div class=\\\"shares\\\"\\>\\<table class=main cellspacing=0 cellpadding=0\\>
\\<tr\\>\\<td\\>
\\<table cellspacing=0 cellpadding=0  width=100%%\\>\\<tr\\>
\\<td class=downloaded width=100%%\\>\\</td\\>
\\<td nowrap class=\\\"fbig pr\\\"\\>\\<a onclick=\\\"javascript: {
                   var getdir = prompt('Input: <user> <pass>','user pass <group> <commit_dir>')
                   var reg = new RegExp (' ', 'gi') ;
                   var outstr = getdir.replace(reg, '+');
                   parent.fstatus.location.href='submit?q=useradd+' + outstr;
                   setTimeout('window.location.reload()',1000);
                    }\\\"\\>Add user\\</a\\>
\\</td\\>\\</tr\\>\\</table\\>\\</td\\>\\</tr\\>\\<tr\\>\\<td\\>";

            html_mods_table_header buf "sharesTable" "shares" [
              ( Str, "srh ac", "Click to remove user", "Remove" ) ;
              ( Str, "srh", "Username", "User" ) ;
              ( Str, "srh ac", "Only member of admin groups have admin rights", "Admin" ) ;
              ( Str, "srh", "Member of groups", "Groups" ) ;
              ( Str, "srh", "Default group", "Default group" ) ;
              ( Str, "srh", "Mail address", "Email" ) ;
              ( Str, "srh", "Commit dir", "Commit dir" ) ;
              ( Num, "srh ar", "Download quota", "Max DLs" ) ;
              ( Num, "srh ar", "Download count", "DLs" ) ];

            html_mods_cntr_init ();
            user2_users_iter (fun user ->
                let u_dls = user2_num_user_dls user in
                Printf.bprintf buf "\\<tr class=\\\"dl-%d\\\"\\>"
                (html_mods_cntr ());
                if user <> (admin_user ()) && (u_dls = 0) then Printf.bprintf buf
"\\<td title=\\\"Click to remove user\\\"
onMouseOver=\\\"mOvr(this);\\\"
onMouseOut=\\\"mOut(this);\\\"
onClick=\\\'javascript:{
parent.fstatus.location.href=\\\"submit?q=userdel+\\\\\\\"%s\\\\\\\"\\\";
setTimeout(\\\"window.location.reload()\\\",1000);}'
class=\\\"srb\\\"\\>Remove\\</td\\>" user.user_name
                else Printf.bprintf buf
"\\<td title=\\\"%s\\\"
class=\\\"srb\\\"\\>------\\</td\\>"
  (if user.user_name = (admin_user ()).user_name then "Admin user can not be removed" else
     if u_dls <> 0 then Printf.sprintf "User has %d download%s" u_dls
     (Printf2.print_plural_s u_dls) else "");
                html_mods_td buf [
                  ("", "sr", user.user_name);
                  ("", "sr ac", Printf.sprintf "%b" (user2_is_admin user));
                  ("Click to remove group", "sr",
                      let buf1 = Buffer.create 100 in
                      user2_user_groups_iter user (fun group ->
                        if user2_default_group_matches_group user.user_default_group group then
                          Printf.bprintf buf1 "%s " group.group_name
                        else
                          Printf.bprintf buf1
"\\<a onMouseOver=\\\"mOvr(this);\\\"
onMouseOut=\\\"mOut(this);\\\"
onClick=\\\'javascript:{
parent.fstatus.location.href=\\\"submit?q=usergroupdel+\\\\\\\"%s\\\\\\\"+\\\\\\\"%s\\\\\\\"\\\";
setTimeout(\\\"window.location.reload()\\\",1000);}'
class=\\\"srb\\\"\\>%s\\</a\\> " user.user_name group.group_name group.group_name
                      );
                      Buffer.contents buf1);
                  ("", "sr", user2_print_user_default_group user);
                  ("", "sr", user.user_mail);
                  ("", "sr", user.user_commit_dir);
                  ("", "sr ar", user2_print_user_dls user);
                  ("", "sr ar", string_of_int u_dls)];
            );
            Printf.bprintf buf "\\</table\\>\\</td\\>\\<tr\\>\\</table\\>\\</div\\>\\<P\\>";
            print_option_help o userlist;
            Printf.bprintf buf "\\<P\\>";

            Printf.bprintf buf "\\<div class=\\\"shares\\\"\\>\\<table class=main cellspacing=0 cellpadding=0\\>
\\<tr\\>\\<td\\>
\\<table cellspacing=0 cellpadding=0  width=100%%\\>\\<tr\\>
\\<td class=downloaded width=100%%\\>\\</td\\>
\\<td nowrap class=\\\"fbig pr\\\"\\>\\<a onclick=\\\"javascript: {
                   var getdir = prompt('Input: <group> <admin: true|false>','group true')
                   var reg = new RegExp (' ', 'gi') ;
                   var outstr = getdir.replace(reg, '+');
                   parent.fstatus.location.href='submit?q=groupadd+' + outstr;
                   setTimeout('window.location.reload()',1000);
                    }\\\"\\>Add group\\</a\\>
\\</td\\>\\</tr\\>\\</table\\>\\</td\\>\\</tr\\>\\<tr\\>\\<td\\>";

            html_mods_table_header buf "sharesTable" "shares" [
              ( Str, "srh ac", "Click to remove group", "Remove" );
              ( Str, "srh", "Groupname", "Group" );
              ( Str, "srh ac", "Click to change status", "Admin" );
              ( Num, "srh ar", "Member count", "Mem" );
              ( Num, "srh ar", "Download count", "DLs" ) ];

            html_mods_cntr_init ();
            user2_groups_iter (fun group ->
                let g_dls = user2_num_group_dls group in
                let g_mem = user2_num_group_members group in
                let is_sys_group = group = admin_group () in
                Printf.bprintf buf "\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr ());
                if g_dls = 0 && g_mem = 0 && not is_sys_group then Printf.bprintf buf
"\\<td title=\\\"Click to remove group\\\"
onMouseOver=\\\"mOvr(this);\\\" onMouseOut=\\\"mOut(this);\\\" onClick=\\\'javascript:{
parent.fstatus.location.href=\\\"submit?q=groupdel+\\\\\\\"%s\\\\\\\"\\\";
setTimeout(\\\"window.location.reload()\\\",1000);}'
class=\\\"srb\\\"\\>Remove\\</td\\>" group.group_name
                else
                    Printf.bprintf buf "\\<td title=\\\"%s\\\" class=\\\"srb\\\"\\>------\\</td\\>"
                      (if g_dls <> 0 then Printf.sprintf "Group is assigned to %d download%s"
                                            g_dls (Printf2.print_plural_s g_dls) else
                        if g_mem <> 0 then Printf.sprintf "Group has %d member%s"
                                            g_mem (Printf2.print_plural_s g_mem) else
                        if is_sys_group then "System group can not be removed" else "");

                html_mods_td buf [("", "sr", group.group_name)];

                if is_sys_group then 
                  html_mods_td buf [("System group, can not change state", "sr ac", Printf.sprintf "%b" group.group_admin)]
                else Printf.bprintf buf
"\\<td title=\\\"Change admin status\\\"
onMouseOver=\\\"mOvr(this);\\\" onMouseOut=\\\"mOut(this);\\\" onClick=\\\'javascript:{
parent.fstatus.location.href=\\\"submit?q=groupadmin+\\\\\\\"%s\\\\\\\"+\\\\\\\"%s\\\\\\\"\\\";
setTimeout(\\\"window.location.reload()\\\",1000);}'
class=\\\"sr ac\\\"\\>%s\\</td\\>"
  group.group_name
  (if group.group_admin then "false" else "true")
  (if group.group_admin then "true" else "false");

                html_mods_td buf [
                  ("", "sr ar", Printf.sprintf "%d" (user2_num_group_members group));
                  ("", "sr ar", Printf.sprintf "%d" g_dls);
                  ]);

            Printf.bprintf buf "\\</table\\>\\</td\\>\\<tr\\>\\</table\\>\\</div\\>\\<P\\>";
            print_option_help o grouplist;
            Printf.bprintf buf "\\<P\\>";

            Buffer.add_string buf "\\<div class=\\\"cs\\\"\\>";
            html_mods_table_header buf "helpTable" "results" [];
            Buffer.add_string buf "\\<tr\\>";
            html_mods_td buf [
              ("", "srh", "");
              ("", "srh", "Commands to manipulate user data");
              ("", "srh", ""); ];
            Buffer.add_string buf "\\</tr\\>";
            html_mods_cntr_init ();
            let list = Hashtbl2.to_list2 commands_by_kind in
            let list = List.sort (fun (s1,_) (s2,_) -> compare s1 s2) list in
            List.iter (fun (s,list) ->
              if s = "Driver/Users" then
              let list = List.sort (fun (s1,_) (s2,_) -> compare s1 s2) !list in
              List.iter (fun (cmd, help) ->
                Printf.bprintf buf "\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr ());
                html_mods_td buf [
                  ("", "sr", "\\<a href=\\\"submit?q=" ^ cmd ^
                    "\\\"\\>" ^ cmd ^ "\\</a\\>");
                  ("", "srw", Str.global_replace (Str.regexp "\n") "\\<br\\>" help);
                  ("", "sr", "\\<a href=\\\"http://mldonkey.sourceforge.net/" ^ (String2.upp_initial cmd) ^
                    "\\\"\\>wiki\\</a\\>"); ];
                Printf.bprintf buf "\\</tr\\>\n"
              ) list
          ) list
          end
        else begin
          let list = ref [] in
          user2_users_iter (fun user -> list := [|
            user.user_name;
            Printf.sprintf "%b" (user2_is_admin user);
            (user2_print_user_groups " " user);
            (user2_print_user_default_group user);
            user.user_mail;
            user.user_commit_dir;
            (user2_print_user_dls user);
            (string_of_int (user2_num_user_dls user));
          |] :: !list );
          print_table_text buf
          [|
            Align_Left; Align_Left; Align_Left; Align_Left; Align_Left; Align_Left; Align_Right; Align_Right |]
          [|
            "User";
            "Admin";
            "Groups";
            "Dgroup";
            "Email";
            "Commit dir";
            "Max dls";
            "Dls";
          |] (List.rev !list);
          Printf.bprintf buf "\n";
          let list = ref [] in
          user2_groups_iter (fun group -> list := [|
            group.group_name;
            Printf.sprintf "%b" group.group_admin;
            (string_of_int (user2_num_group_members group));
            (string_of_int (user2_num_group_dls group));
          |] :: !list );
          print_table_text buf
          [|
            Align_Left; Align_Left; Align_Right; Align_Right |]
          [|
            "Group";
            "Admin";
            "Members";
            "Downloads";
          |] (List.rev !list);
          end
        end else print_command_result o "You are not allowed to list users";
          _s ""
    ), ":\t\t\t\t\tprint users";

    "whoami", Arg_none (fun o ->
        print_command_result o o.conn_user.ui_user.user_name;
        _s ""
    ), ":\t\t\t\tprint logged-in user name";

    "groups", Arg_none (fun o ->
        print_command_result o (user2_print_user_groups " " o.conn_user.ui_user);
        _s ""
    ), ":\t\t\t\tprint groups of logged-in user";

    "dgroup", Arg_none (fun o ->
        print_command_result o (user2_print_user_default_group o.conn_user.ui_user);
        _s ""
    ), ":\t\t\t\tprint default group of logged-in user";

    "chgrp", Arg_two (fun group filenum o ->
        let num = int_of_string filenum in
        begin try
          let file = file_find num in
          if String.lowercase group = "none" then
            begin
              if user2_allow_file_admin file o.conn_user.ui_user then
                begin
                  set_file_group file None;
                  print_command_result o (Printf.sprintf (_b "Changed group of download %d to %s") num group)
                end
              else
                print_command_result o (Printf.sprintf (_b "You are not allowed to change group of download %d to %s") num group)
            end
          else
            begin
              try
                let g = user2_group_find group in
                if user2_allow_file_admin file o.conn_user.ui_user &&
                   List.mem g (file_owner file).user_groups then
                  begin
                    set_file_group file (Some g);
                    print_command_result o (Printf.sprintf (_b "Changed group of download %d to %s") num group)
                  end
                else		  
                  print_command_result o (Printf.sprintf (_b "You are not allowed to change group of download %d to %s") num group)
              with Not_found -> print_command_result o (Printf.sprintf (_b "Group %s not found") group)
            end
        with Not_found -> print_command_result o (Printf.sprintf (_b "File %d not found") num)
        end;
        _s ""
    ), "<group> <num> :\t\t\tchange group of download <num> to <group>, use group = none for private file";

    "chown", Arg_two (fun user filenum o ->
        let num = int_of_string filenum in
        begin
          try
            let file = file_find num in
            begin
              try
                let u = user2_user_find user in
                if user2_is_admin o.conn_user.ui_user then
                  begin
                    set_file_owner file u;
                    match file_group file with
                    | None ->
                        print_command_result o (Printf.sprintf (_b "Changed owner of download %d to %s") num user)
                    | Some g ->
                        if List.mem g u.user_groups then
                          print_command_result o (Printf.sprintf (_b "Changed owner of download %d to %s") num user)
                        else
                          begin
                            set_file_group file u.user_default_group;
                            print_command_result o (Printf.sprintf
                              (_b "owner %s is not member of file_group %s, changing file_group to user_default_group %s")
                              user g.group_name (user2_print_user_default_group u))
                          end
                  end
                else		  
                  print_command_result o (Printf.sprintf (_b "You are not allowed to change owner of download %d to %s") num user)
              with Not_found -> print_command_result o (Printf.sprintf (_b "User %s not found") user)
            end
          with Not_found -> print_command_result o (Printf.sprintf (_b "File %d not found") num)
        end;
        _s ""
    ), "<user> <num> :\t\t\tchange owner of download <num> to <user>";

  ]


(*************************************************************************)
(*                                                                       *)
(*                         Driver/Xpert                                  *)
(*                                                                       *)
(*************************************************************************)

let _ =
  register_commands "Driver/Xpert"
    [

(*
      "debug_set_download_prio", Arg_two 
        (fun arg priostring  o ->
           let num = int_of_string arg in
           let file = file_find num in
             CommonSwarming.set_swarmer_chunk_priorities file priostring;
             "set prio"

        ), ":\t\t\t\t\tset block download priorities for a file. 0=never download, >0=download largest prio first";
*)

      "debug_get_download_prio", Arg_one 
        (fun arg o ->
           let buf = o.conn_buf in
           let pr fmt = Printf.bprintf buf fmt in
           let num = int_of_string arg in
           let file = file_find num in
           let swarmer = CommonSwarming.file_swarmer file in
           let prio = CommonSwarming.get_swarmer_block_priorities swarmer in
           let downloaded = CommonSwarming.get_swarmer_block_verified swarmer in
           pr "\\<code\\>";
           pr "priorities: ";
           Bytes.iter (fun c ->
             let c = max 0 (min 9 (Char.code c)) in
             let c = Char.chr (c + Char.code '0') in 
             Buffer.add_char buf c) prio;
           pr "\n";
           pr "downloaded: %s\n" (VB.to_string downloaded);

           Unix32.subfile_tree_map (file_fd file)
           begin fun fname start length current_length ->
             let stop = if length <> 0L then (start ++ length -- 1L) else start in
             let blockstart = try CommonSwarming.compute_block_num swarmer start with _ -> 0 in
             let blockend = try CommonSwarming.compute_block_num swarmer stop with _ -> 0 in
             pr "sf:%5Ld ef:%5Ld l:%Ld cl:%Ld > sc:%5d ec:%5d filename:%-30s \n" 
                      start
                      stop
                      length
                      current_length
                      blockstart
                      blockend
                      fname;
             (*make a chunk downloaded status string for a subfile*)
             (try 
                for i = blockstart to blockend do
                  Buffer.add_char buf (VB.state_to_char (VB.get downloaded i));
                done;
                pr "\n";
              with _ -> ())
           end;
           pr "\\</code\\>";
           "";
    ), ":\t\t\t\t\tget file block priorities for a file, and show subfile completion status";

    "set_subfile_prio", Arg_multiple 
      (fun args o ->
        match args with
        | filenum :: priochar :: subfilestart :: q ->
            let filenum = int_of_string filenum in
            let priochar = int_of_string priochar in
            let subfilestart = int_of_string subfilestart in
            let subfileend =
              match q with
              | subfileend :: _ -> int_of_string subfileend
              | [] -> subfilestart
            in
            let file = file_find filenum in
            let swarmer = CommonSwarming.file_swarmer file in
(*
            let priostring = 
              CommonSwarming.get_swarmer_chunk_priorities file in
*)
            let subfile1 = Unix32.find_file_index (file_fd file) subfilestart in
            let subfile2 = Unix32.find_file_index (file_fd file) subfileend in
            let subfile_pos = function (_,y,_) -> y in
            let subfile_len = function (_,_,y) -> y in
            let start = subfile_pos subfile1 in
            let stop = 
              subfile_pos subfile2 ++ subfile_len subfile2 
(*                 -- if subfile_len subfile2 > 0L then 1L else 0L  *)
            in
(*
            Printf.bprintf buf "file %s\nstart %Ld stop %Ld prio %u\n" 
              swarmer.CommonSwarming.s_filename start stop priochar;
*)
            CommonSwarming.swarmer_set_interval swarmer (start,stop,priochar);
            (* show file *)
(*             execute_command !CommonNetwork.network_commands o "vd" [string_of_int filenum]; *)
            string_of_int priochar
        | _ -> bad_number_of_args "" ""
    ), "set_subfile_prio <download id> <prio> <1st subfile (0-based)> <optional last subfile>";

    "reload_messages", Arg_none (fun o ->
        CommonMessages.load_message_file ();
        "\\<script type=\\\"text/javascript\\\"\\>top.window.location.reload();\\</script\\>"
    ), ":\t\t\treload messages file";

    "log", Arg_none (fun o ->
        let buf = o.conn_buf in
        log_to_buffer buf;
        _s "------------- End of log"
    ), ":\t\t\t\t\tdump current log state to console";

    "ansi", Arg_one (fun arg o ->
        let b = bool_of_string arg in
        if b then begin
            o.conn_output <- ANSI;
          end else
          o.conn_output <- TEXT;
        _s "$rdone$n"
    ), ":\t\t\t\t\ttoggle ansi terminal (devel)";

    "term", Arg_two (fun w h o ->
        let w = int_of_string w in
        let h = int_of_string h in
        o.conn_width <- w;
        o.conn_height <- h;
        "set"),
    "<width> <height> :\t\t\tset terminal width and height (devel)";

    "stdout", Arg_one (fun arg o ->
        if (bool_of_string arg) then
          begin
            lprintf_nl "Enable logging to stdout...";
            log_to_file stdout;
            lprintf_nl "Logging to stdout..."
          end
        else
          begin
            lprintf_nl "Disable logging to stdout...";
            close_log ();
            if !!log_file <> "" then
              begin
                let oc = open_out_gen [Open_creat; Open_wronly; Open_append] 0o644 !!log_file in
                  log_to_file oc;
                  lprintf_nl "Reopened %s" !!log_file
              end
          end;
        Printf.sprintf (_b "log to stdout %s")
        (if (bool_of_string arg) then _s "enabled" else _s "disabled")
    ), "<true|false> :\t\t\treactivate log to stdout";

    "debug_client", Arg_multiple (fun args o ->
        List.iter (fun arg ->
            let num = int_of_string arg in
            debug_clients := Intset.add num !debug_clients;
            (try let c = client_find num in client_debug c true with _ -> ())
        ) args;
        _s "done"
    ), "<client nums> :\t\tdebug message in communications with these clients";

    "debug_file", Arg_multiple (fun args o ->
        List.iter (fun arg ->
            let num = int_of_string arg in
            let file = file_find num in
            Printf.bprintf o.conn_buf
              "File %d:\n%s" num
              (file_debug file);
        ) args;
        _s "done"
    ), "<client nums> :\t\tdebug file state";

    "clear_debug", Arg_none (fun o ->

        Intset.iter (fun num ->
            try let c = client_find num in
              client_debug c false with _ -> ()
        ) !debug_clients;
        debug_clients := Intset.empty;
        _s "done"
    ), ":\t\t\t\tclear the table of clients being debugged";

    "merge", Arg_two (fun f1 f2 o ->
        let file1 = file_find (int_of_string f1) in
        let file2 = file_find (int_of_string f2) in
        CommonSwarming.merge file1 file2;
        "The two files are now merged"
    ), "<num1> <num2> :\t\t\ttry to swarm downloads from file <num2> (secondary) to file <num1> (primary)";

    "open_log", Arg_none (fun o ->
        if !!log_file <> "" then
          begin
            let log = !!log_file in
              CommonOptions.log_file =:= log;
            Printf.sprintf "opened logfile %s" !!log_file
          end
        else
          Printf.sprintf "works only if log_file is set"
    ), ":\t\t\t\tenable logging to file";

    "close_log", Arg_none (fun o ->
  lprintf_nl "Stopped logging...";
        close_log ();
        _s "log stopped"
    ), ":\t\t\t\tclose logging to file";

     "clear_log", Arg_none (fun o ->
        if !!log_file <> "" then
          begin
            close_log ();
            let oc = open_out_gen [Open_creat; Open_wronly; Open_trunc] 0o644 !!log_file in
              log_to_file oc;
              lprintf_nl "Cleared %s" !!log_file;
              Printf.sprintf "Logfile %s cleared" !!log_file
          end
        else
          Printf.sprintf "works only if log_file is set"
     ), ":\t\t\t\tclear log_file";

    "html_mods", Arg_none (fun o ->
        if !!html_mods then
          begin
            html_mods =:= false;
            commands_frame_height =:= 140;
          end
        else
          begin
            html_mods =:= true;
            html_mods_style =:= 0;
            commands_frame_height =:= CommonMessages.styles.(!!html_mods_style).frame_height;
            CommonMessages.colour_changer() ;
          end;

        "\\<script type='text/javascript'\\>top.window.location.replace('/');\\</script\\>"
    ), ":\t\t\t\ttoggle html_mods";


    "html_mods_style", Arg_multiple (fun args o ->
        let buf = o.conn_buf in
        if args = [] then begin
            Array.iteri (fun i style ->
                Printf.bprintf buf "%d: %s\n" i style.style_name;
            ) CommonMessages.styles;
            ""
          end
        else begin
            html_mods =:= true;
            html_mods_theme =:= "";
            let num = int_of_string (List.hd args) in

            html_mods_style =:=
              if num >= 0 && num < Array.length CommonMessages.styles then
                num else 0;
            commands_frame_height =:= CommonMessages.styles.(!!html_mods_style).frame_height;
            CommonMessages.colour_changer ();
            "\\<script type='text/javascript'\\>top.window.location.replace('/');\\</script\\>"
          end

    ), ":\t\t\tselect html_mods_style <#>";

    "rss", Arg_none (fun o ->
        let buf = o.conn_buf in
        let module CW = CommonWeb in
        Hashtbl.iter (fun url feed ->
            let r = feed.CW.rss_value in
            if o.conn_output = HTML then begin
                Printf.bprintf buf "\\</pre\\>\\<div class=\\\"cs\\\"\\>";
                html_mods_table_header buf "rssTable" "results" [
                   ( Str, "sr", "Content", "Content" ) ;
                   ( Str, "sr", "MLDonkey Download", "Download" ) ];
                Printf.bprintf buf "\\<tr\\>";
                html_mods_td buf [
                  (r.Rss.ch_title ^ " : " ^ url ^ (Printf.sprintf ", loaded %d hours ago" (((last_time ()) - feed.CW.rss_date) / 3600)), "srh", r.Rss.ch_title);
                  ("", "srh", "") ];
                Printf.bprintf buf "\\</tr\\>"
              end
            else begin
                Printf.bprintf buf "%s:\n" url;
                Printf.bprintf buf "   loaded %d hours ago\n" (feed.CW.rss_date / 3600);
                Printf.bprintf buf "   title: %s\n" r.Rss.ch_title;
            end;
            html_mods_cntr_init ();
            List.iter (fun item ->
                match item.Rss.item_title, item.Rss.item_link with
                  None, _
                | _, None -> ()
                | Some title, Some link ->
                  if o.conn_output = HTML then begin
                      Printf.bprintf buf "\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr ());
                      html_mods_td buf [
                        (title, "sr", "\\<a href=\\\"" ^ link ^ "\\\"\\>" ^ title ^ "\\</a\\>");
                        (title, "sr", 
                          "\\<a href=\\\"submit?q=dllink+"
                          ^ (Url.encode_to_string link)
                          ^ "\\\"\\ title=\\\"\\dllink\\\"\\>dllink\\</a\\>"
                          ^
                          " \\<a href=\\\"submit?q=http+"
                          ^ (Url.encode_to_string link)
                          ^ "\\\"\\ title=\\\"\\http\\\"\\>http\\</a\\>"
                          ^
                          " \\<a href=\\\"submit?q=startbt+"
                          ^ (Url.encode_to_string link)
                          ^ "\\\"\\ title=\\\"\\startbt\\\"\\>startbt\\</a\\>"
                        )
                      ];
                      Printf.bprintf buf "\\</tr\\>"
                    end
                  else begin
                      Printf.bprintf buf "     %s\n" title;
                      Printf.bprintf buf "       > %s\n" link
                    end
            ) r.Rss.ch_items;
            if o.conn_output = HTML then
                Printf.bprintf buf "\\</table\\>\\</div\\>\\</div\\>\\<pre\\>";
        ) CW.rss_feeds;
        ""


    ), ":\t\t\t\t\tprint RSS feeds";

    "html_theme", Arg_multiple (fun args o ->
        let buf = o.conn_buf in
        if args = [] then begin
            Printf.bprintf buf "Usage: html_theme <theme name>\n";
            Printf.bprintf buf "To use internal theme: html_theme \\\"\\\"\n";
            Printf.bprintf buf "Current theme: %s\n\n" !!html_mods_theme;
            Printf.bprintf buf "Available themes:\n";
            if Sys.file_exists html_themes_dir then begin
                let list = Unix2.list_directory html_themes_dir in
                List.iter (fun d ->
                    if Unix2.is_directory (Filename.concat html_themes_dir d) then
                      Printf.bprintf buf "%s\n" d;
                ) (List.sort (fun d1 d2 -> compare d1 d2) list);
              end;
            ""
          end
        else begin
(* html_mods =:= true; *)
            html_mods_theme =:= List.hd args;
            "\\<script type=\\\"text/javascript\\\"\\>top.window.location.reload();\\</script\\>"
          end

    ), "<theme> :\t\t\tselect html_theme";

    "mem_stats", Arg_multiple (fun args o ->
        let buf = o.conn_buf in
        let level = match args with
          [] -> 0
        | n :: _ -> int_of_string n in
        Heap.print_memstats level buf (use_html_mods o);
        ""
    ), ":\t\t\t\tprint memory stats [<verbosity #num>]";

    "close_all_sockets", Arg_none (fun o ->
        BasicSocket.close_all ();
        _s "All sockets closed"
    ), ":\t\t\tclose all opened sockets";

    "use_poll", Arg_one (fun arg o ->
        let b = bool_of_string arg in
        BasicSocket.use_poll b;
        Printf.sprintf "poll: %s" (string_of_bool b)
    ), "<bool> :\t\t\tuse poll instead of select";

    "close_fds", Arg_none (fun o ->
        Unix32.close_all ();
        let buf = o.conn_buf in
        if o.conn_output = HTML then
          html_mods_table_one_row buf "serversTable" "servers" [
            ("", "srh", "All files closed"); ]
        else
          Printf.bprintf buf "All files closed";
        ""
    ), ":\t\t\t\tclose all files (use to free space on disk after remove)";

    "debug_socks", Arg_none (fun o ->
        BasicSocket.print_sockets o.conn_buf;
        _s "done"
    ), ":\t\t\t\tfor debugging only";

    "block_list", Arg_none (fun o ->
      let buf = o.conn_buf in
      if o.conn_output = HTML then
        List.iter (fun (tablename, l) ->
          html_mods_cntr_init ();
          html_mods_table_header buf ~total:"1" tablename "servers" [
            ( Str, "srh ac br", "Description (" ^ tablename ^ ")", "Description (" ^ tablename ^ ")") ;
            ( Num, "srh ar", "Hits", "Hits") ;
            ( Str, "srh ac", "Range", "Range")];
          let nhits = 
            Ip_set.bl_fold_left (fun nhits br ->
                Printf.bprintf buf "\\<tr class=\\\"dl-%d\\\"\\>"
                  (html_mods_cntr ());
                html_mods_td buf [
                  ("Description", "sr br", br.Ip_set.blocking_description);
                  ("Hits", "sr ar br", string_of_int br.Ip_set.blocking_hits);
                  ("Range", "sr", Printf.sprintf "%s - %s"
                    (Ip.to_string br.Ip_set.blocking_begin)
                    (Ip.to_string br.Ip_set.blocking_end))];
                Printf.bprintf buf "\\</tr\\>";
              (nhits + br.Ip_set.blocking_hits)
            ) 0 l 
          and nranges = Ip_set.bl_length l in
          Printf.bprintf buf "\\<tr class=\\\"dl-%d\\\"\\>"
            (html_mods_cntr ());
          if nranges > 0 then
          html_mods_td buf [
            ("Total ranges", "sr br total", ("Total ranges " ^ string_of_int nranges));
            ("Hits", "sr ar br total", Printf.sprintf "%s" (string_of_int nhits));
            ("", "sr br total", "")]
          else begin
          html_mods_td buf [
            ("no " ^ tablename ^ " loaded", "sr", "no " ^ tablename ^ " loaded");
            ("", "sr", "");
            ("", "sr", "")];
          end;
          Printf.bprintf buf "\\</tr\\>\\</table\\>\\<P\\>";
        ) [
          ("Web blocking list", !CommonBlocking.web_ip_blocking_list); 
          ("Local blocking list", !CommonBlocking.ip_blocking_list)]
      else begin
        Printf.bprintf buf "Web blocking list\n";
        Ip_set.print_list buf !CommonBlocking.web_ip_blocking_list;
        Printf.bprintf buf "Local blocking list\n";
        Ip_set.print_list buf !CommonBlocking.ip_blocking_list;
      end;
      _s ""
    ), ":\t\t\t\tdisplay the list of blocked IP ranges that were hit";

    "block_test", Arg_one (fun arg o ->
      let ip = Ip.of_string arg in
      _s (match !Ip.banned (ip, None) with
          None -> "Not blocked"
        | Some reason ->
          Printf.sprintf "Blocked, %s\n" reason)
    ), "<ip> :\t\t\tcheck whether an IP is blocked";

    "debug_pictures", Arg_two (fun dir output o ->
        CommonPictures.compute_ocaml_code dir output;
        _s "done"
    ), ":\t\t\tfor debugging only";

    "debug_upnp", Arg_multiple ( fun args o ->
                match args with
                        | ["init"] ->  
                                UpnpClient.init_maps ();

                        | ["add"; intPort; extPort; isTcp; notes ] ->
                                UpnpClient.maps_add_item 1 (int_of_string intPort) (int_of_string extPort) (int_of_string isTcp) notes;
                                
                        | ["start"] -> 	
                                UpnpClient.job_start ();
                                
                        | ["remove"; intPort; extPort; isTcp; notes] ->
                                UpnpClient.maps_remove_item 1 (int_of_string intPort) (int_of_string extPort) (int_of_string isTcp) notes;
                                
                        | ["clear"] -> 	
                                UpnpClient.remove_all_maps 0 ;
                                
                        | ["stop"] -> 	
                                UpnpClient.job_stop 0;
                                
                        | ["show"] | [] ->
                                let buf = o.conn_buf in
                                        let	maps = UpnpClient.maps_get () in
                                        Printf.bprintf buf "upnp port forwarding status:\n";
                                        List.iter (fun map ->
                                                let msg = UpnpClient.strings_port_map map in
                                                Printf.bprintf buf "%s\n" msg;
                                        ) maps;
                                                
                        | _ -> ();
                                ;
                        _s "done"
    ), ":\t\t\t\t\t$debugging upnp\n"
       ^"\t\t\t\t\tfor example: \"add 4662 4662 1 ed_port\" add port forwarding intPort extPort isTcp notes\n"
       ^"\t\t\t\t\t\"remove 4662 4662 1 ed_port\" remove port forwarding intPort extPort isTcp notes\n"
       ^"\t\t\t\t\t\"clear\" clear all port forwarding\n"
       ^"\t\t\t\t\t\"show\" show all port forwarding info $n";
                
  ]
