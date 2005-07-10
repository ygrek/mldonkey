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
open Options
open BasicSocket
open TcpBufferedSocket
open Ip_set

open GuiTypes

open CommonDownloads
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
open CommonInteractive
open CommonEvent

open DriverInteractive

open Gettext
open Autoconf

let _s x = _s "DriverCommands" x
let _b x = _b "DriverCommands" x

let to_cancel = ref []

let files_to_cancel o =
  let buf = o.conn_buf in
  Printf.bprintf buf (_b "Files to be cancelled:\n");
  List.iter (fun file ->
      file_print file o
  ) !to_cancel;
  "Type 'confirm yes/no' to cancel them"

let execute_command arg_list output cmd args =
  let buf = output.conn_buf in
  try
    let rec iter list =
      match list with
        [] ->
          Gettext.buftext buf no_such_command cmd
      | (command, _, arg_kind, help) :: tail ->
          if command = cmd then
            Buffer.add_string buf (
              match arg_kind, args with
                Arg_none f, [] -> f output
              | Arg_multiple f, _ -> f args output
              | Arg_one f, [arg] -> f arg  output
              | Arg_two f, [a1;a2] -> f a1 a2 output
              | Arg_three f, [a1;a2;a3] -> f a1 a2 a3 output
              | _ -> bad_number_of_args
            )
          else
            iter tail
    in
    iter arg_list
  with Not_found -> ()


let list_options_html o list =
  let buf = o.conn_buf in
  html_mods_table_header buf "voTable" "vo" [
    ( "0", "srh", "Option name", "Name (Help=mouseOver)" ) ;
    ( "0", "srh", "Option value", "Value (press ENTER to save)" ) ;
    ( "0", "srh", "Option default", "Default" ) ] ;

  let counter = ref 0 in

  List.iter (fun o ->
      incr counter;
      if (!counter mod 2 == 0) then Printf.bprintf buf "\\<tr class=\\\"dl-1\\\"\\>"
      else Printf.bprintf buf "\\<tr class=\\\"dl-2\\\"\\>";

      if String.contains o.option_value '\n' then
        Printf.bprintf buf "
                  \\<td title=\\\"%s\\\" class=\\\"sr\\\"\\>%s\\<form action=\\\"submit\\\" target=\\\"$S\\\"\\>
                  \\<input type=hidden name=setoption value=q\\>
                  \\<input type=hidden name=option value=%s\\>\\</td\\>\\<td\\>\\<textarea
					name=value rows=5 cols=20 wrap=virtual\\>
                  %s
                  \\</textarea\\>\\<input type=submit value=Modify\\>
                  \\</td\\>\\<td class=\\\"sr\\\"\\>%s\\</td\\>\\</tr\\>
                  \\</form\\>
                  " o.option_help o.option_name o.option_name o.option_value o.option_default

      else

        begin

          Printf.bprintf buf "
              \\<td title=\\\"%s\\\" class=\\\"sr\\\"\\>%s\\</td\\>
		      \\<td class=\\\"sr\\\"\\>\\<form action=\\\"submit\\\" target=\\\"$S\\\"\\>\\<input type=hidden
				name=setoption value=q\\>\\<input type=hidden name=option value=%s\\>"  o.option_help o.option_name o.option_name;

          if o.option_value = "true" || o.option_value = "false" then

            Printf.bprintf buf "\\<select style=\\\"font-family: verdana; font-size: 10px;\\\"
									name=\\\"value\\\" onchange=\\\"this.form.submit()\\\"\\>
									\\<option selected\\>%s\\<option\\>%s\\</select\\>"
              o.option_value
              (if o.option_value="true" then "false" else "true")
          else

            Printf.bprintf buf "\\<input style=\\\"font-family: verdana; font-size: 10px;\\\"
				type=text name=value size=20 value=\\\"%s\\\"\\>"
              o.option_value;

          Printf.bprintf buf "
              \\</td\\>
              \\<td class=\\\"sr\\\"\\>%s\\</td\\>
			  \\</tr\\>\\</form\\>
              " o.option_default
        end;


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
              \\<input type=text name=value size=40 value=\\\"%s\\\"\\>
\\</td\\>\\</tr\\>
\\</form\\>
              " o.option_name o.option_name o.option_value
      else
        Printf.bprintf buf "$b%s$n = $r%s$n\n" o.option_name o.option_value)
  list;
  if oo.conn_output = HTML then
    Printf.bprintf  buf "\\</table\\>"

let list_web_infos o list =
  let buf = o.conn_buf in
  if o.conn_output = HTML then begin
      html_mods_table_header buf "web_infoTable" "vo" [
        ( "0", "srh", "Option type", "Type" ) ;
        ( "0", "srh", "Option delay", "Delay" ) ;
        ( "0", "srh", "Option value", "Value" ) ] ;
      let counter = ref 0 in
      List.iter (fun (kind, period, url) ->
          incr counter;
          if (!counter mod 2 == 0) then Printf.bprintf buf "\\<tr class=\\\"dl-1\\\"\\>"
          else Printf.bprintf buf "\\<tr class=\\\"dl-2\\\"\\>";
          Printf.bprintf buf "
              \\<td title=\\\"%s\\\" class=\\\"sr\\\"\\>%s\\</td\\>
	      \\<td class=\\\"sr\\\"\\>%d\\</td\\>"  url kind period;
          Printf.bprintf buf "
              \\<td class=\\\"sr\\\"\\>%s\\</td\\>
              \\</tr\\>" url
      )list;
      Printf.bprintf  buf "\\</table\\>\\</div\\>"
    end
  else begin
      Printf.bprintf buf "kind / period / url :\n";
      List.iter (fun (kind, period, url) ->
          Printf.bprintf buf "%s ; %d ; %s\n"  kind period url
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
    ), ":\t\t\t\t\t$badd an command alias\n"
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

    ), ":\t\t\t\t\t$bdelete an command alias  example: \"unalias ca\"$n";

    "q", Arg_none (fun o ->
        raise CommonTypes.CommandCloseSocket
    ), ":\t\t\t\t\t$bclose telnet$n";

    "kill", Arg_none (fun o ->
        if o.conn_user == default_user then
	  begin
            CommonGlobals.exit_properly 0;
	    _s "exit"
	  end
        else
          _s "Only 'admin' is allowed to do that"
        ), ":\t\t\t\t\t$bsave and kill the server$n";

    "add_url", Arg_two (fun kind url o ->
        let buf = o.conn_buf in
        let v = (kind, 1, url) in
        if not (List.mem v !!web_infos) then
          web_infos =:=  v :: !!web_infos;
        CommonWeb.load_url kind url;
        "url added to web_infos. downloading now"
    ), "<kind> <url> :\t\t\tload this file from the web.
\t\t\t\t\tkind is either server.met (if the downloaded file is a server.met)";

    "recover_temp", Arg_none (fun o ->
        networks_iter (fun r ->
            try
              CommonNetwork.network_recover_temp r
            with _ -> ()
        );
        _s "done"
    ), ":\t\t\t\trecover lost files from temp directory";

    "vc", Arg_multiple (fun args o ->
        if args = ["all"] then begin 
            let buf = o.conn_buf in

            if use_html_mods o then html_mods_table_header buf "vcTable" "vc" [
                ( "1", "srh ac", "Client number", "Num" ) ;
                ( "0", "srh", "Network", "Network" ) ;
                ( "0", "srh", "IP address", "IP address" ) ;
                ( "0", "srh", "Client name", "Client name" ) ];

            let counter = ref 0 in
            let all_clients_list = clients_get_all () in
            List.iter (fun num ->
                let c = client_find num in
                if use_html_mods o then Printf.bprintf buf "\\<tr class=\\\"%s\\\"
			 title=\\\"Add as friend\\\" 
			 onClick=\\\"parent.fstatus.location.href='submit?q=friend_add+%d'\\\"
            onMouseOver=\\\"mOvr(this);\\\"
            onMouseOut=\\\"mOut(this);\\\"\\>"
                    (if (!counter mod 2 == 0) then "dl-1" else "dl-2") num;
                client_print c o;
                if use_html_mods o then Printf.bprintf buf "\\</tr\\>"
                else Printf.bprintf buf "\n";
                incr counter;
            ) all_clients_list;
            if use_html_mods o then Printf.bprintf buf "\\</table\\>\\</div\\>";
          end
        else
          List.iter (fun num ->
              let num = int_of_string num in
              let c = client_find num in
              client_print c o;
          ) args;
        ""
    ), "<num> :\t\t\t\tview client (use arg 'all' for all clients)";

    "version", Arg_none (fun o ->
        let buf = o.conn_buf in
        let version = CommonGlobals.version () in
        if o.conn_output = HTML then
          begin
            Printf.bprintf buf "\\<div class=\\\"cs\\\"\\>";
            html_mods_table_header buf "versionTable" "results" [];
            Printf.bprintf buf "\\<tr\\>";
            html_mods_td buf [ ("", "srh", version); ];
            Printf.bprintf buf "\\</tr\\>\\</table\\>\\</div\\>\\</div\\>";
          end
        else
            Printf.bprintf buf "%s" version;
        ""
    ), ":\t\t\t\tprint mldonkey version";

    "buildinfo", Arg_none (fun o ->
        let buf = o.conn_buf in
        let buildinfo = CommonGlobals.buildinfo () in
        if o.conn_output = HTML then
          begin
            Printf.bprintf buf "\\<div class=\\\"cs\\\"\\>";
            html_mods_table_header buf "versionTable" "results" [];
            Printf.bprintf buf "\\<tr class=\\\"dl-1\\\"\\>";
            html_mods_td buf [ ("", "sr", Str.global_replace (Str.regexp "\n") "\\<br\\>" buildinfo); ];
            Printf.bprintf buf "\\</tr\\>\\</table\\>\\</div\\>\\</div\\>";
          end
        else
            Printf.bprintf buf "%s" buildinfo;
        ""
    ), ":\t\t\t\tprint mldonkey core build information";

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
    ), " <minutes> :\t\t\tprint activity in the last <minutes> minutes";

    "message_log", Arg_multiple (fun args o ->
        let buf = o.conn_buf in
        let counter = ref 0 in

        (match args with
            [arg] ->
              let refresh_delay = int_of_string arg in
              if use_html_mods o && refresh_delay > 1 then
                Printf.bprintf buf "\\<meta http-equiv=\\\"refresh\\\" content=\\\"%d\\\"\\>"
                  refresh_delay;
          | _ -> ());

(* rely on GC? *)

        while (Fifo.length chat_message_fifo) > !!html_mods_max_messages  do
          let foo = Fifo.take chat_message_fifo in ()
        done;

        if use_html_mods o then Printf.bprintf buf "\\<div class=\\\"messages\\\"\\>";

        last_message_log := last_time();
        Printf.bprintf buf "%d logged messages\n" (Fifo.length chat_message_fifo);

        if Fifo.length chat_message_fifo > 0 then
          begin

            if use_html_mods o then
              html_mods_table_header buf "serversTable" "servers" [
                ( "0", "srh", "Timestamp", "Time" ) ;
                ( "0", "srh", "IP address", "IP address" ) ;
                ( "1", "srh", "Client number", "Num" ) ;
                ( "0", "srh", "Client name", "Client name" ) ;
                ( "0", "srh", "Message text", "Message" ) ] ;

            Fifo.iter (fun (t,i,num,n,s) ->
                if use_html_mods o then begin
                    Printf.bprintf buf "\\<tr class=\\\"%s\\\"\\>"
                      (if (!counter mod 2 == 0) then "dl-1" else "dl-2");
                    html_mods_td buf [
                      ("", "sr", Date.simple (BasicSocket.date_of_int t));
                      ("", "sr",  i);
                      ("", "sr", Printf.sprintf "%d" num);
                      ("", "sr", n);
                      ("", "srw", (String.escaped s)) ];
                    Printf.bprintf buf "\\</tr\\>"
                  end
                else
                  Printf.bprintf buf "\n%s [client #%d] %s(%s): %s\n"
                    (Date.simple (BasicSocket.date_of_int t)) num n i s;
                incr counter;
            ) chat_message_fifo;
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
            client_say (client_find cnum) msg;
	    log_chat_message "localhost" 0 !!global_login msg;
            Printf.sprintf "Sending msg to client #%d: %s" cnum msg;
        | _ ->
            if use_html_mods o then begin

                Printf.bprintf buf "\\<script language=javascript\\>
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
            Value=\\\"Refresh\\\"\\>\\</td\\>\\</form\\>\\</tr\\>\\</table\\>";
                ""
              end
            else
              _s "Usage: message <client num> <msg>\n";

    ), ":\t\t\t\tmessage [<client num> <msg>]";


    "add_user", Arg_two (fun user pass o ->
        if o.conn_user == default_user
	   || o.conn_user == (find_ui_user user) then
          try
            let p = List.assoc user !!users in
            let pass = Md4.string pass in
(* In place replacement....heurk *)
            String.blit (Md4.direct_to_string pass) 0
              (Md4.direct_to_string p) 0 16;
            _s "Password changed"
          with _ ->
              users =:= (user, Md4.string pass) :: !!users;
              _s "User added"
        else
          _s "Only 'admin' is allowed to do that"
    ), "<user> <passwd> :\t\tadd a new mldonkey user";

    "remove_user", Arg_one (fun user o ->
        if o.conn_user == default_user then
	  if user = "admin" then
	    _s "User 'admin' can not be removed"
	  else
	    begin
	      let found = ref false in
		users =:= List.filter (fun (s,_) ->
		let diff = s <> user in
		  if not diff then found := true;
		  diff
		) !!users;
	        if !found then
		  _s (Printf.sprintf "user %s removed" user)
	        else
	          _s (Printf.sprintf "user %s not found" user)
	    end
        else
          _s "Only 'admin' is allowed to do that"
    ), "<user> :\t\tremove a mldonkey user";


    "users", Arg_none (fun o ->
        if o.conn_user == default_user then

        let buf = o.conn_buf in

        if use_html_mods o then begin
            Printf.bprintf buf "\\<div class=\\\"shares\\\"\\>\\<table class=main cellspacing=0 cellpadding=0\\>
\\<tr\\>\\<td\\>
\\<table cellspacing=0 cellpadding=0  width=100%%\\>\\<tr\\>
\\<td class=downloaded width=100%%\\>\\</td\\>
\\<td nowrap class=\\\"fbig pr\\\"\\>\\<a onclick=\\\"javascript: {
                   var getdir = prompt('Input: <user> <pass>','user pass')
                   var reg = new RegExp (' ', 'gi') ;
                   var outstr = getdir.replace(reg, '+');
                   parent.fstatus.location.href='submit?q=add_user+' + outstr;
                   setTimeout('window.location.reload()',1000);
                    }\\\"\\>Add User\\</a\\>
\\</td\\>
\\</tr\\>\\</table\\>
\\</td\\>\\</tr\\>
\\<tr\\>\\<td\\>";

            html_mods_table_header buf "sharesTable" "shares" [
              ( "0", "srh ac", "Click to remove user", "Remove" ) ;
              ( "0", "srh", "User", "Username" ) ];

            let counter = ref 0 in

            List.iter (fun (user,_) ->
                incr counter;
                Printf.bprintf buf "\\<tr class=\\\"%s\\\"\\>"
                (if !counter mod 2 == 0 then "dl-1" else "dl-2");
		if user <> "admin" then Printf.bprintf buf "
        \\<td title=\\\"Click to remove user\\\"
        onMouseOver=\\\"mOvr(this);\\\"
        onMouseOut=\\\"mOut(this);\\\"
        onClick=\\\'javascript:{
        parent.fstatus.location.href=\\\"submit?q=remove_user+\\\\\\\"%s\\\\\\\"\\\";
        setTimeout(\\\"window.location.reload()\\\",1000);}'
        class=\\\"srb\\\"\\>Remove\\</td\\>" user
		else Printf.bprintf buf "
        \\<td title=\\\"\\\"
        class=\\\"srb\\\"\\>------\\</td\\>";
		Printf.bprintf buf
		  "\\<td class=\\\"sr\\\"\\>%s\\</td\\>\\</tr\\>" user
            )
            !!users;

            Printf.bprintf buf "\\</table\\>\\</td\\>\\<tr\\>\\</table\\>\\</div\\>";
          end
        else
          begin
            Printf.bprintf buf "Users:\n";
            List.iter (fun (user,_) ->
                Printf.bprintf buf "  %s\n"
                user)
            !!users;
          end;
        ""
        else
          _s "Only 'admin' is allowed to do that"
    ), ":\t\t\t\t\tprint users";

    "calendar_add", Arg_two (fun hour action o ->
        calendar =:= ([0;1;2;3;4;5;6;7], [int_of_string hour], action)
        :: !!calendar;
        _s "action added"
    ), "<hour> \"<command>\" :\tadd a command to be executed every day";


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
        if use_html_mods o then Printf.bprintf buf "\\</table\\>\\</div\\>";
        Printf.bprintf buf "Servers: %d known\n" !nb_servers;
        if Autoconf.donkey = "yes" && not !!enable_servers then
          begin
            if use_html_mods o then Printf.bprintf buf "\\<div class=servers\\>";
            Printf.bprintf buf "You disabled server usage, therefore you are not able to connect ED2K servers.\n";
            Printf.bprintf buf "To use servers again 'set enable_servers true'\n";
            if use_html_mods o then Printf.bprintf buf "\\</div\\>"
          end;
    ""), ":\t\t\t\t\tlist all known servers";

    "rem", Arg_multiple (fun args o ->
        if args = ["all"] then
	  begin
            Intmap.iter ( fun _ s ->
              server_remove s
            ) !!servers;
            "Removed all servers"
          end else begin
            List.iter (fun num ->
                let num = int_of_string num in
                let s = server_find num in
                server_remove s
            ) args;
            Printf.sprintf (_b"%d servers removed") (List.length args)
          end
    ), "<server numbers> :\t\t\tremove server (use arg 'all' for all servers)";

    "server_banner", Arg_one (fun num o ->
        let buf = o.conn_buf in
        let num = int_of_string num in
        let s = server_find num in
        (match server_state s with
            NotConnected _ -> ()
          | _ ->   server_banner s o);
        ""
    ), "<num> :\t\t\tprint banner of connected server <num>";

    "c", Arg_multiple (fun args o ->
        match args with
          [] ->
            networks_iter network_connect_servers;
            _s "connecting more servers"
        | _ ->
            List.iter (fun num ->
                let num = int_of_string num in
                let s = server_find num in
                server_connect s
            ) args;
            _s "connecting server"
    ),
    "[<num>] :\t\t\t\tconnect to more servers (or to server <num>)";

    "x", Arg_one (fun num o ->
        let num = int_of_string num in
        let s = server_find num in
        (match server_state s with
            NotConnected _ -> ()
          | _ ->   server_disconnect s);
        ""
    ), "<num> :\t\t\t\tdisconnect from server";

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
    ), "<client numbers> :\t\tremove friend (use arg 'all' for all friends)";

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
              ( "1", "srh", "Client number", "Num" ) ;
              ( "0", "srh", "Remove", "Remove" ) ;
              ( "0", "srh", "Network", "Network" ) ;
              ( "0", "srh", "Name", "Name" ) ;
              ( "0", "srh", "State", "State" ) ] ;
          end;
        let counter = ref 0 in
        List.iter (fun c ->
            let i = client_info c in
            let n = network_find_by_num i.client_network in
            if use_html_mods o then
              begin

                Printf.bprintf buf "\\<tr class=\\\"%s\\\"
                onMouseOver=\\\"mOvr(this);\\\"
                onMouseOut=\\\"mOut(this);\\\"\\>"
                  (if (!counter mod 2 == 0) then "dl-1" else "dl-2");

                incr counter;
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

                  (let rs = client_files c in
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
                Printf.bprintf buf "Reinitialising download selectors\n";
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
        let buf = o.conn_buf in
        let num = int_of_string num in

        if num > 0 then (* we want to disable upload for a short time *)
          let num = mini !CommonUploads.upload_credit num in
          CommonUploads.has_upload := !CommonUploads.has_upload + num;
          CommonUploads.upload_credit := !CommonUploads.upload_credit - num;
          Printf.sprintf
            "upload disabled for %d minutes (remaining credits %d)"
            !CommonUploads.has_upload !CommonUploads.upload_credit
        else

        if num < 0 && !CommonUploads.has_upload > 0 then
(* we want to restart upload probably *)
          let num = - num in
          let num = mini num !CommonUploads.has_upload in
          CommonUploads.has_upload := !CommonUploads.has_upload - num;
          CommonUploads.upload_credit := !CommonUploads.upload_credit + num;
          Printf.sprintf
            "upload disabled for %d minutes (remaining credits %d)"
            !CommonUploads.has_upload !CommonUploads.upload_credit

        else ""
    ), "<m> :\t\t\t\t\tdisable upload during <m> minutes (multiple of 5)";

    "bw_stats", Arg_multiple (fun args o ->
        let buf = o.conn_buf in
        if use_html_mods o then
          begin

            let refresh_delay = ref !!html_mods_bw_refresh_delay in
            if args <> [] then begin
                let newrd = int_of_string (List.hd args) in
                if newrd > 1 then refresh_delay := newrd;
              end;
            Printf.bprintf buf "\\<meta http-equiv=\\\"refresh\\\" content=\\\"%d\\\"\\>" !refresh_delay;

            let dlkbs =
              (( (float_of_int !udp_download_rate) +. (float_of_int !control_download_rate)) /. 1024.0) in
            let ulkbs =
              (( (float_of_int !udp_upload_rate) +. (float_of_int !control_upload_rate)) /. 1024.0) in

            Printf.bprintf buf "\\<div class=\\\"bw_stats\\\"\\>";
            Printf.bprintf buf "\\<table class=\\\"bw_stats\\\" cellspacing=0 cellpadding=0\\>\\<tr\\>";
            Printf.bprintf buf "\\<td\\>\\<table border=0 cellspacing=0 cellpadding=0\\>\\<tr\\>";

            html_mods_td buf [
              ("Download KB/s (UDP|TCP)", "bu bbig bbig1 bb4", Printf.sprintf "Down: %.1f KB/s (%d|%d)"
                  dlkbs !udp_download_rate !control_download_rate);
              ("Upload KB/s (UDP|TCP)", "bu bbig bbig1 bb4", Printf.sprintf "Up: %.1f KB/s (%d|%d)"
                  ulkbs !udp_upload_rate !control_upload_rate);
              ("Total shared files/bytes", "bu bbig bbig1 bb4", Printf.sprintf "Shared(%d): %s"
                !nshared_files (size_of_int64 !nshared_bytes));
              ("Total uploaded bytes", "bu bbig bbig1 bb3", Printf.sprintf "Uploaded: %s"
                (size_of_int64 !upload_counter) ) ];


            Printf.bprintf buf "\\</tr\\>\\</table\\>\\</td\\>\\</tr\\>\\</table\\>\\</div\\>";

            Printf.bprintf buf "\\<script language=\\\"JavaScript\\\"\\>window.parent.document.title='(D:%.1f) (U:%.1f) | %s'\\</script\\>"
              dlkbs ulkbs (CommonGlobals.version ())
          end
        else
          Printf.bprintf buf "Down: %.1f KB/s ( %d + %d ) | Up: %.1f KB/s ( %d + %d ) | Shared: %d/%s | Uploaded: %s"
            (( (float_of_int !udp_download_rate) +. (float_of_int !control_download_rate)) /. 1024.0)
          !udp_download_rate
            !control_download_rate
            (( (float_of_int !udp_upload_rate) +. (float_of_int !control_upload_rate)) /. 1024.0)
          !udp_upload_rate
            !control_upload_rate
            !nshared_files
            (size_of_int64 !nshared_bytes)
            (size_of_int64 !upload_counter);
        ""
    ), ":\t\t\t\tprint current bandwidth stats";

    "stats", Arg_none (fun o ->
        let buf = o.conn_buf in
        CommonInteractive.network_display_stats buf o;
        if use_html_mods o then
          print_gdstats buf o;
      _s ""), ":\t\t\t\t\tdisplay transfer statistics";

    "gdstats", Arg_none (fun o ->
        let buf = o.conn_buf in
	if Autoconf.has_gd then
          if use_html_mods o then
            print_gdstats buf o
          else
            Printf.bprintf buf "Only available on HTML interface"
	else
	  Printf.bprintf buf "Gd support was not compiled";
      _s ""), ":\t\t\t\t\tdisplay graphical transfer statistics";

    "gdremove", Arg_none (fun o ->
        let buf = o.conn_buf in
	if Autoconf.has_gd then
	  begin
	    CommonGraphics.really_remove_files ();
	    Printf.bprintf buf "Gd files were removed"
	  end
	else
	  Printf.bprintf buf "Gd support was not compiled";
      _s ""), ":\t\t\t\tremove graphical transfer statistics files";

    "!", Arg_multiple (fun arg o ->
        if !!allow_any_command then
          match arg with
            c :: tail ->
              let args = String2.unsplit tail ' ' in
              let cmd = List.assoc c !!allowed_commands in
              let tmp = Filename.temp_file "com" ".out" in
              let ret = Sys.command (Printf.sprintf "%s %s > %s"
                    cmd args tmp) in
              let output = File.to_string tmp in
              Sys.remove tmp;
              Printf.sprintf (_b "%s\n---------------- Exited with code %d") output ret
          | _ -> _s "no command given"
        else
        match arg with
          [arg] ->
            let cmd = List.assoc arg !!allowed_commands in
            let tmp = Filename.temp_file "com" ".out" in
            let ret = Sys.command (Printf.sprintf "%s > %s"
                  cmd tmp) in
            let output = File.to_string tmp in
            Sys.remove tmp;
            Printf.sprintf (_b "%s\n---------------- Exited with code %d") output ret
        | [] ->
            _s "no command given"
        | _ -> "For arbitrary commands, you must set 'allowed_any_command'"
    ), "<cmd> :\t\t\t\tstart command <cmd> (must be allowed in 'allowed_commands' option or by 'allow_any_command' if arguments)";


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
    ) , " :\t\t\t\tprint all networks";

    "enable", Arg_one (fun num o ->
        let buf = o.conn_buf in
        let n = network_find_by_num (int_of_string num) in
        network_enable n;
        _s "network enabled"
    ) , " <num> :\t\t\t\tenable a particular network";

    "disable", Arg_one (fun num o ->
        let buf = o.conn_buf in
        let n = network_find_by_num (int_of_string num) in
        network_disable n;
        _s "network disabled"
    ) , " <num> :\t\t\t\tdisable a particular network";

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
        let buf = o.conn_buf in
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
    ), "<num1> <num2> ...:\t\tforget searches <num1> <num2> ...";

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
                [] -> "No search to print"
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
        ) (Sort.list (fun f1 f2 -> f1.search_num < f2.search_num)
          user.ui_user_searches); ""), ":\t\t\t\t\tview all queries";

    "view_custom_queries", Arg_none (fun o ->
        let buf = o.conn_buf in
        if o.conn_output <> HTML then
          Printf.bprintf buf "%d custom queries defined\n"
            (List.length (customized_queries ()));
        List.iter (fun (name, q) ->
            if o.conn_output = HTML then
              begin

                if use_html_mods o then
                  Printf.bprintf buf
                    "\\<a href=\\\"submit\\?custom=%s\\\" target=\\\"$O\\\"\\>%s\\</a\\> "
                    (Url.encode name) name

                else
                  Printf.bprintf buf
                    "\\<a href=\\\"submit\\?custom=%s\\\" $O\\> %s \\</a\\>\n"
                    (Url.encode name) name;
              end
            else

              Printf.bprintf buf "[%s]\n" name
        ) (customized_queries ());

        if use_html_mods o then
          Printf.bprintf buf "\\<a
            href=\\\"http://www.fileheaven.org/\\\" name=\\\"FileHeaven\\\" target=\\\"$O\\\"\\>FileHeaven\\</a\\> \\<a
            href=\\\"http://www.filedonkey.com\\\" name=\\\"FileDonkey\\\" target=\\\"$O\\\"\\>FileDonkey\\</a\\> \\<a
            href=\\\"http://bitzi.com/search/\\\" name=\\\"Bitzi\\\" target=\\\"$O\\\"\\>Bitzi\\</a\\> ";

        ""
    ), ":\t\t\tview custom queries";

    "d", Arg_multiple (fun args o ->
        List.iter (fun arg ->
            CommonInteractive.download_file o arg) args;
        ""),
    "<num> :\t\t\t\t$bfile to download$n";

    "force_download", Arg_none (fun o ->
        let buf = o.conn_buf in
        match !CommonGlobals.aborted_download with
          None -> "No download to force"
        | Some r ->
            let r = CommonResult.find_result r in
            let files = CommonResult.result_download
                r [] true
            in
            List.iter CommonInteractive.start_download files;
            "download forced"
    ), ":\t\t\t\tforce download of an already downloaded file";

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
        try
          try
            let buf = o.conn_buf in
            CommonInteractive.set_fully_qualified_options name value;
            Printf.sprintf "option %s value changed" name

(*
            let pos = String.index name '-' in
            let prefix = String.sub name 0 pos in
            let name = String.sub name (pos+1) (String.length name - pos-1) in
            networks_iter (fun n ->
                match n.network_config_file with
                  None -> ()
                | Some opfile ->
                    List.iter (fun p ->
                        if p = prefix then begin
                            set_simple_option opfile name value;
                            Printf.bprintf buf "option %s :: %s value changed"
                            n.network_name name

                          end)
                    n.network_prefixes
);
  *)
          with _ ->
              Options.set_simple_option downloads_ini name value;
              Printf.sprintf "option %s value changed" name
        with e ->
            Printf.sprintf "Error %s" (Printexc2.to_string e)
    ), "<option_name> <option_value> :\t$bchange option value$n";

    "save", Arg_none (fun o ->
        DriverInteractive.save_config ();
        _s "saved"), ":\t\t\t\t\tsave";

    "vo", Arg_none (fun o ->
        let buf = o.conn_buf in
        if use_html_mods o then begin

            Printf.bprintf buf "\\<div class=\\\"friends\\\"\\>\\<table class=main cellspacing=0 cellpadding=0\\>
\\<tr\\>\\<td\\>
\\<table cellspacing=0 cellpadding=0  width=100%%\\>\\<tr\\>
\\<td class=downloaded width=100%%\\>\\</td\\>
\\<td nowrap class=fbig\\>\\<a onclick=\\\"javascript:window.location.href='submit?q=shares'\\\"\\>Shares\\</a\\>\\</td\\>
\\<td nowrap class=fbig\\>\\<a onclick=\\\"javascript:window.location.href='submit?q=users'\\\"\\>Users\\</a\\>\\</td\\>
\\<td nowrap class=fbig\\>\\<a onclick=\\\"javascript:window.location.href='submit?q=html_mods'\\\"\\>Toggle html_mods\\</a\\>\\</td\\>
\\<td nowrap class=fbig\\>\\<a onclick=\\\"javascript:window.location.href='submit?q=voo+1'\\\"\\>Full Options\\</a\\>\\</td\\>
\\<td nowrap class=\\\"fbig pr\\\"\\>\\<a onclick=\\\"javascript:parent.fstatus.location.href='submit?q=save'\\\"\\>Save\\</a\\>\\</td\\>
\\</tr\\>\\</table\\>
\\</td\\>\\</tr\\>
\\<tr\\>\\<td\\>";

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
              ] );

            Printf.bprintf buf "\\</td\\>\\<tr\\>\\</table\\>\\</div\\>"
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
            ]
          );

        "\nUse '$rvoo$n' for all options"
    ), ":\t\t\t\t\t$bdisplay options$n";




    "voo", Arg_multiple (fun args o ->
        let buf = o.conn_buf in
        if use_html_mods o then begin

            Printf.bprintf buf "\\<script language=javascript\\>
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


            let tabnumber = ref 0 in
            let mtabs = ref 1 in

            Printf.bprintf buf "\\<div class=\\\"vo\\\"\\>\\<table class=main cellspacing=0 cellpadding=0\\>
\\<tr\\>\\<td\\>
\\<table cellspacing=0 cellpadding=0  width=100%%\\>\\<tr\\>
\\<td class=downloaded width=100%%\\>\\</td\\>";


            List.iter (fun s ->
                incr tabnumber; incr mtabs;
                Printf.bprintf buf "\\<td nowrap class=fbig\\>\\<a onclick=\\\"javascript:window.location.href='submit?q=voo+%d'\\\"\\>%s\\</a\\>\\</td\\>"
                  !tabnumber s
            ) [ "Client" ; "Ports" ; "html" ; "Delays" ; "Files" ; "Mail" ; "Net" ; "Misc" ];

            Printf.bprintf buf "
\\<td nowrap class=\\\"fbig\\\"\\>\\<a onclick=\\\"javascript:window.location.href='submit?q=voo'\\\"\\>All\\</a\\>\\</td\\>
\\<td nowrap class=\\\"fbig fbig pr\\\"\\>
\\<form style=\\\"margin: 0px;\\\" name=\\\"pluginForm\\\" id=\\\"pluginForm\\\"
action=\\\"javascript:pluginSubmit();\\\"\\>
\\<select id=\\\"plugin\\\" name=\\\"plugin\\\"
style=\\\"padding: 0px; font-size: 10px; font-family: verdana\\\" onchange=\\\"this.form.submit()\\\"\\>
\\<option value=\\\"0\\\"\\>Plugins\n";

            let netlist = ref [] in
            List.iter (fun s ->
                incr tabnumber;
                netlist := !netlist @ [(s,!tabnumber)]

            ) (CommonInteractive.all_active_network_opfile_network_names ());

            let duplist = ref [] in
            let netname = ref "" in
            List.iter (fun tup ->
                let s = (fst tup) in
                let t = (snd tup) in
                if List.memq s !duplist then
                  netname := Printf.sprintf "%s+" s
                else netname := s;
                duplist := !duplist @ [!netname];
                Printf.bprintf buf "\\<option value=\\\"%d\\\"\\>%s\\</option\\>\n"
                  t !netname
            ) (List.sort (fun d1 d2 -> compare (fst d1) (fst d2)) !netlist);

            Printf.bprintf buf "\\</select\\>\\</td\\>\\</form\\>
\\</tr\\>\\</table\\>
\\</td\\>\\</tr\\>
\\<tr\\>\\<td\\>";

            list_options_html o (
              match args with
                [] | _ :: _ :: _ ->
                  let v=   CommonInteractive.all_simple_options () in
                  v

              | [arg] ->
                  try
                  let tab = int_of_string arg in
                  match tab with
                    1 ->
                      [
			strings_of_option global_login;
			strings_of_option set_client_ip;
			strings_of_option force_client_ip;
			strings_of_option run_as_user;
			strings_of_option run_as_useruid;
			strings_of_option max_upload_slots;
			strings_of_option dynamic_slots;
			strings_of_option max_hard_upload_rate;
			strings_of_option max_hard_download_rate;
			strings_of_option max_opened_connections;
			strings_of_option max_connections_per_second;
			strings_of_option max_concurrent_downloads;
                      ]

                  | 2 ->
                      [
			strings_of_option gui_bind_addr;
			strings_of_option telnet_bind_addr;
			strings_of_option http_bind_addr;
			strings_of_option chat_bind_addr;
			strings_of_option gui_port;
			strings_of_option telnet_port;
			strings_of_option http_port;
			strings_of_option chat_port;
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
			strings_of_option html_mods_vd_last;
			strings_of_option html_mods_vd_prio;
			strings_of_option html_mods_vd_queues;
			strings_of_option html_mods_show_pending;
			strings_of_option html_mods_load_message_file;
			strings_of_option html_mods_max_messages;
			strings_of_option html_mods_bw_refresh_delay;
			strings_of_option use_html_frames;
			strings_of_option html_checkbox_vd_file_list;
			strings_of_option html_checkbox_search_file_list;
			strings_of_option commands_frame_height;
			strings_of_option html_vd_barheight;
			strings_of_option display_downloaded_results;
			strings_of_option vd_reload_delay;
			strings_of_option html_use_gzip;
			] @ (if Autoconf.has_gd then
			[strings_of_option html_mods_vd_gfx;] else []) @
			(if Autoconf.has_gd_jpg && Autoconf.has_gd_png
			 then [strings_of_option html_mods_vd_gfx_png;] else []) @
			(if Autoconf.has_gd then [
			strings_of_option html_mods_vd_gfx_remove;
			strings_of_option html_mods_vd_gfx_split;
			strings_of_option html_mods_vd_gfx_fill;
			strings_of_option html_mods_vd_gfx_flip;
			strings_of_option html_mods_vd_gfx_mean;
			strings_of_option html_mods_vd_gfx_transparent;
			strings_of_option html_mods_vd_gfx_h;
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
			strings_of_option client_timeout;
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
			strings_of_option file_started_cmd;
			strings_of_option file_completed_cmd;
			strings_of_option allow_browse_share;
			strings_of_option auto_commit;
			strings_of_option create_dir_mask;
			strings_of_option log_file;
			strings_of_option log_size;
                      ]
                  | 6 ->
                      [
			strings_of_option mail;
			strings_of_option smtp_port;
			strings_of_option smtp_server;
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
			strings_of_option tcpip_packet_size;
			strings_of_option mtu_packet_size;
			strings_of_option minimal_packet_size;
			strings_of_option network_update_url;
			strings_of_option mlnet_redirector;
			strings_of_option http_proxy_tcp;
			strings_of_option http_proxy_server;
			strings_of_option http_proxy_port;
                      ])
                  | 8 ->
                      [
			strings_of_option term_ansi;
			strings_of_option messages_filter;
			strings_of_option max_displayed_results;
			strings_of_option max_name_len;
			strings_of_option max_client_name_len;
			strings_of_option emule_mods_count;
			strings_of_option emule_mods_showall;
			strings_of_option chat_app_port;
			strings_of_option chat_app_host;
			strings_of_option chat_console_id;
			strings_of_option chat_warning_for_downloaded;
                      ]

                  | _ ->
                      let v = CommonInteractive.some_simple_options (tab - !mtabs) in
                      List.sort (fun d1 d2 -> compare d1 d2) v;
              with _ ->
                    let v = CommonInteractive.parse_simple_options args in
                    List.sort (fun d1 d2 -> compare d1 d2) v;


            );
            Printf.bprintf buf "
\\</td\\>\\</tr\\>
\\<tr\\>\\<td\\>
\\<table cellspacing=0 cellpadding=0  width=100%%\\>\\<tr\\>
\\<td class=downloaded width=100%%\\>\\</td\\>
\\<td nowrap class=\\\"fbig fbigb\\\"\\>\\<a onclick=\\\"javascript:window.location.href='submit?q=shares'\\\"\\>Shares\\</a\\>\\</td\\>
\\<td nowrap class=\\\"fbig fbigb\\\"\\>\\<a onclick=\\\"javascript:window.location.href='submit?q=users'\\\"\\>Users\\</a\\>\\</td\\>
\\<td nowrap class=\\\"fbig fbigb\\\"\\>\\<a onclick=\\\"javascript:parent.fstatus.location.href='submit?q=save'\\\"\\>Save\\</a\\>\\</td\\>
\\<td nowrap class=\\\"fbig fbigb\\\"\\>\\<a onclick=\\\"javascript:window.location.href='submit?q=html_mods'\\\"\\>toggle html_mods\\</a\\>\\</td\\>
\\<td nowrap class=\\\"fbig fbigb pr\\\"\\>
\\<form style=\\\"margin: 0px;\\\" name=\\\"htmlModsStyleForm\\\" id=\\\"htmlModsStyleForm\\\"
action=\\\"javascript:submitHtmlModsStyle();\\\"\\>
\\<select id=\\\"modsStyle\\\" name=\\\"modsStyle\\\"
style=\\\"padding: 0px; font-size: 10px; font-family: verdana\\\" onchange=\\\"this.form.submit()\\\"\\>
\\<option value=\\\"0\\\"\\>style/theme\n";

            Array.iteri (fun i h ->
                Printf.bprintf buf "\\<option value=\\\"%d\\\"\\>%s\\</option\\>\n" i (fst h);
            ) !html_mods_styles;

            if Sys.file_exists html_themes_dir then
              let list = Unix2.list_directory html_themes_dir in
              List.iter (fun d ->
                  if Unix2.is_directory (Filename.concat html_themes_dir d) then
                    let sd = (if String.length d > 11 then String.sub d 0 11 else d) in
                    Printf.bprintf buf "\\<option value=\\\"%s\\\"\\>%s\\</option\\>\n" d sd;
              ) (List.sort (fun d1 d2 -> compare d1 d2) list);


              Printf.bprintf buf "
\\</select\\>\\</td\\>
\\</tr\\>\\</form\\>\\</table\\>";
              Printf.bprintf buf "\\</td\\>\\</tr\\>\\</table\\>\\</div\\>";
          end
        else begin
            list_options o (CommonInteractive.parse_simple_options args)
          end;
        ""
    ), ":\t\t\t\t\tprint all options";

    "vwi", Arg_none (fun o ->
        let buf = o.conn_buf in
        if use_html_mods o then begin
            Printf.bprintf buf "\\<div class=\\\"vo\\\"\\>
                \\<table class=main cellspacing=0 cellpadding=0\\>\\<tr\\>\\<td\\>";
            list_web_infos o !!web_infos;
            Printf.bprintf buf "\\</td\\>\\</tr\\>\\</table\\>\\</div\\>";
          end
        else begin
            list_web_infos o !!web_infos
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

(*
    "options", Arg_multiple (fun args o ->
        let buf = o.conn_buf in
        match args with
          [] ->
            let sections = ref [] in
            Printf.bprintf buf "Available sections for options: \n";
            List.iter (fun  (section, message, option, optype) ->
                if not (List.mem section !sections) then begin
                    Printf.bprintf buf "  $b%s$n\n" section;
                    sections := section :: !sections
                  end
            ) gui_options_panel;

            List.iter (fun (section, list) ->
                if not (List.mem section !sections) then begin
                    Printf.bprintf buf "  $b%s$n\n" section;
                    sections := section :: !sections
                  end)
            ! CommonInteractive.gui_options_panels;
            "\n\nUse 'options section' to see options in this section"

        | sections ->
            List.iter (fun s ->
                Printf.bprintf buf "Options in section $b%s$n:\n" s;
                List.iter (fun (section, message, option, optype) ->
                    if s = section then
                      Printf.bprintf buf "  %s [$r%s$n]= $b%s$n\n"
                        message option
                        (get_fully_qualified_options option)
                ) gui_options_panel;

                List.iter (fun (section, list) ->
                    if s = section then
                      List.iter (fun (message, option, optype) ->
                          Printf.bprintf buf "  %s [$b%s$n]= $b%s$n\n"
                            message option
                            (get_fully_qualified_options option)
                      ) list)
                ! CommonInteractive.gui_options_panels;
            ) sections;
            "\nUse '$rset option \"value\"$n' to change a value where options is
the name between []"
    ), ":\t\t\t\t$bprint options values by section$n";
*)

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
        _s "check done"
    ), ":\t\t\t\tcheck shared files for removal";
    
    "shares", Arg_none (fun o ->

        let buf = o.conn_buf in
        
        if use_html_mods o then begin
            Printf.bprintf buf "\\<div class=\\\"shares\\\"\\>\\<table class=main cellspacing=0 cellpadding=0\\> 
\\<tr\\>\\<td\\>
\\<table cellspacing=0 cellpadding=0  width=100%%\\>\\<tr\\>
\\<td class=downloaded width=100%%\\>\\</td\\>
\\<td nowrap class=\\\"fbig pr\\\"\\>\\<a onclick=\\\"javascript: { 
                   var getdir = prompt('Input: <priority#> <directory> (surround dir with quotes if necessary)','0 /home/mldonkey/share')
                   var reg = new RegExp (' ', 'gi') ;
                   var outstr = getdir.replace(reg, '+');
                   parent.fstatus.location.href='submit?q=share+' + outstr;
                   setTimeout('window.location.reload()',1000);
                    }\\\"\\>Add Share\\</a\\>
\\</td\\>
\\</tr\\>\\</table\\>
\\</td\\>\\</tr\\> 
\\<tr\\>\\<td\\>";

            html_mods_table_header buf "sharesTable" "shares" [ 
              ( "0", "srh ac", "Click to unshare directory", "Unshare" ) ; 
              ( "1", "srh ar", "Priority", "P" ) ; 
              ( "0", "srh", "Directory", "Directory" ) ;
              ( "0", "srh", "Strategy", "Strategy" ) ]; 
            
            let counter = ref 0 in

(* TODO update HTML for incoming directories now in shared_directories
            Printf.bprintf buf "\\<tr class=\\\"dl-1\\\"\\>\\<td title=\\\"Incoming directory is always shared\\\" class=\\\"srb\\\"\\>Incoming\\</td\\>
\\<td class=\\\"sr ar\\\"\\>0\\</td\\>\\<td title=\\\"Incoming\\\" class=\\\"sr\\\"\\>%s\\</td\\>\\</tr\\>" !!incoming_directory;
*)

            List.iter (fun shared_dir -> 
                incr counter;
                Printf.bprintf buf "\\<tr class=\\\"%s\\\"\\>
        \\<td title=\\\"Click to unshare this directory\\\" 
        onMouseOver=\\\"mOvr(this);\\\" 
        onMouseOut=\\\"mOut(this);\\\"
        onClick=\\\'javascript:{ 
        parent.fstatus.location.href=\\\"submit?q=unshare+\\\\\\\"%s\\\\\\\"\\\"; 
        setTimeout(\\\"window.location.reload()\\\",1000);}'
        class=\\\"srb\\\"\\>Unshare\\</td\\>
        \\<td class=\\\"sr ar\\\"\\>%d\\</td\\>
        \\<td class=\\\"sr\\\"\\>%s\\</td\\>
        \\<td class=\\\"sr\\\"\\>%s\\</td\\>\\</tr\\>"
                (if !counter mod 2 == 0 then "dl-1" else "dl-2")
                shared_dir.shdir_dirname
                shared_dir.shdir_priority
                shared_dir.shdir_dirname
                shared_dir.shdir_strategy;
            )
            !!shared_directories;

            Printf.bprintf buf "\\</table\\>\\</td\\>\\<tr\\>\\</table\\>\\</div\\>";
          end
        else
          begin

            Printf.bprintf buf "Shared directories:\n";
(*            Printf.bprintf buf "  %d %s\n" !!incoming_directory_prio !!incoming_directory; *)
            List.iter (fun sd ->
                Printf.bprintf buf "  %d %s %s\n"
                sd.shdir_priority sd.shdir_dirname sd.shdir_strategy)
            !!shared_directories;

          end;
        ""
    ), ":\t\t\t\t\tprint shared directories";

    "share", Arg_multiple (fun args o ->
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
          if not (List.mem shdir !!shared_directories) then begin
              shared_directories =:= shdir :: !!shared_directories;
              shared_add_directory shdir;
              "directory added"
            end (* else
            if not (List.mem (arg, prio) !!shared_directories) then begin
              shared_directories =:= (arg, prio) :: List.remove_assoc arg !!shared_directories;
              shared_add_directory (arg, prio);
              "prio changed"
            end *) else
            "directory already shared"
        else
          "no such directory"
    ), "<priority> <dir> [<strategy>] :\tshare directory <dir> with <priority> [and sharing strategy <strategy>]";

    "unshare", Arg_one (fun arg o ->

        let found = ref false in
        shared_directories =:= List.filter (fun sd ->
            let diff = sd.shdir_dirname <> arg in
            if not diff then found := true;
            diff
        ) !!shared_directories;
        if !found then begin
            CommonShared.shared_check_files ();
            _s "directory removed"
          end else
          _s "directory already unshared"
    
    ), "<dir> :\t\t\t\tshare directory <dir>";

    "upstats", Arg_none (fun o ->
        let buf = o.conn_buf in
        
        if use_html_mods o then Printf.bprintf buf "\\<div class=\\\"upstats\\\"\\>"
        else Printf.bprintf buf "Upload statistics:\n";
        Printf.bprintf buf "Session: %s uploaded | Shared(%d): %s\n"
          (size_of_int64 !upload_counter) !nshared_files (size_of_int64 !nshared_bytes);

        let list = ref [] in
        shared_iter (fun s ->
            let impl = as_shared_impl s in
            list := impl :: !list
        );

        if use_html_mods o then 
          html_mods_table_header buf "upstatsTable" "upstats" [
            ( "1", "srh", "Total file requests", "Reqs" ) ;
            ( "1", "srh", "Total bytes sent", "Total" ) ;
            ( "1", "srh", "Upload Ratio", "UPRatio" ) ;
            ( "0", "srh", "Filename", "Filename" );
            ( "0", "srh", "Statistic links", "Stats" ) ]
        else
          begin
            Printf.bprintf buf " Requests |  Bytes   | Uploaded | File\n";
            Printf.bprintf buf "----------+----------+----------+----------------------------------------------------\n";
          end;

        let counter = ref 0 in

        let list = Sort.list (fun f1 f2 ->
              (f1.impl_shared_requests = f2.impl_shared_requests &&
                f1.impl_shared_uploaded > f2.impl_shared_uploaded) ||
              (f1.impl_shared_requests > f2.impl_shared_requests )
          ) !list in

        List.iter (fun impl ->
            if use_html_mods o then
              begin
                incr counter;

                let ed2k = Printf.sprintf "ed2k://|file|%s|%s|%s|/"
                    (Filename.basename impl.impl_shared_codedname)
                  (Int64.to_string impl.impl_shared_size)
                  (Md4.to_string impl.impl_shared_id) in

		Printf.bprintf buf "\\<tr class=\\\"%s\\\"\\>"
		  (if (!counter mod 2 == 0) then "dl-1" else "dl-2";);
		
		let uploaded = Int64.to_float impl.impl_shared_uploaded in
		let size = Int64.to_float impl.impl_shared_size in
		
		html_mods_td buf [
		  ("", "sr ar", Printf.sprintf "%d" impl.impl_shared_requests);
		  ("", "sr ar", size_of_int64 impl.impl_shared_uploaded);
		  ("", "sr ar", Printf.sprintf "%5.1f" ( if size < 1.0 then 0.0 else (uploaded *. 100.) /. size));
		  ("", "sr", Printf.sprintf "\\<a href=\\\"%s\\\"\\>%s\\</a\\>"
		      ed2k (Filename.basename impl.impl_shared_codedname));
		  ("", "sr", (if impl.impl_shared_id = Md4.null then "" else
		    Printf.sprintf "\\<a href=\\\"http://stats.razorback2.com/ed2khistory?ed2k=%s\\\"\\>%s\\</a\\>
				    \\<a href=\\\"http://bitzi.com/lookup/urn:ed2k:%s\\\"\\>%s\\</a\\>"
		      (Md4.to_string impl.impl_shared_id) "R" (Md4.to_string impl.impl_shared_id) "B")) ];
		Printf.bprintf buf "\\</tr\\>\n";
              end
            else
              Printf.bprintf buf "%9d | %8s | %7s%% | %-50s\n"
		(impl.impl_shared_requests)
              (size_of_int64 impl.impl_shared_uploaded)
              (Printf.sprintf "%3.1f" ((Int64.to_float impl.impl_shared_uploaded *. 100.) /. Int64.to_float impl.impl_shared_size))
              (shorten (Filename.basename impl.impl_shared_codedname) !!max_name_len);
        ) list;

        if use_html_mods o then Printf.bprintf buf "\\</table\\>\\</div\\>\\</div\\>";


        _s ""
    ), ":\t\t\t\tstatistics on upload";

    "links", Arg_none (fun o ->
        let buf = o.conn_buf in
        let list = ref [] in
        shared_iter (fun s ->
          let impl = as_shared_impl s in
          list := impl :: !list
        );

        let list =
          List.sort ( fun f1 f2 ->
            String.compare
              (Filename.basename f1.impl_shared_codedname)
              (Filename.basename f2.impl_shared_codedname)
        ) !list in

        List.iter (fun impl ->
          if (impl.impl_shared_id <> Md4.null) then
           Printf.bprintf buf "ed2k://|file|%s|%s|%s|/\n"
              (Filename.basename impl.impl_shared_codedname)
              (Int64.to_string impl.impl_shared_size)
              (Md4.to_string impl.impl_shared_id);
        ) list;
        "Done"
    ), ":\t\t\t\t\tlist links of shared files";
    
    "uploaders", Arg_none (fun o ->
        let buf = o.conn_buf in

        let nuploaders = Intmap.length !uploaders in 

        if use_html_mods o then

          begin

            let counter = ref 0 in

            Printf.bprintf buf "\\<div class=\\\"uploaders\\\"\\>Total upload slots: %d (%d) | Pending slots: %d\n" nuploaders
              (Fifo.length CommonUploads.upload_clients)
            (Intmap.length !CommonUploads.pending_slots_map);

            if nuploaders > 0 then

              begin

                html_mods_table_header buf "uploadersTable" "uploaders" ([ 
                  ( "1", "srh ac", "Client number", "Num" ) ; 
                  ( "0", "srh", "Network", "Network" ) ; 
                  ( "0", "srh", "Connection type [I]ndirect [D]irect", "C" ) ;
                  ( "0", "srh", "Client name", "Client name" ) ;
                  ( "0", "srh", "IP address", "IP address" ) ;
                  ( "0", "srh", "Connected time (minutes)", "CT" ) ;
                  ( "0", "srh", "Client brand", "CB" ) ;
                  ] @
                  (if !!emule_mods_count then [( "0", "srh", "eMule MOD", "EM" )] else [])
                  @ [
                  ( "0", "srh ar", "Total DL bytes from this client for all files", "DL" ) ;
                  ( "0", "srh ar", "Total UL bytes to this client for all files", "UL" ) ;
                  ( "0", "srh", "Filename", "Filename" ) ]);

                List.iter (fun c ->
                    try
                      let i = client_info c in
                      if is_connected i.client_state then begin
                          incr counter;

                          Printf.bprintf buf "\\<tr class=\\\"%s\\\"
                        title=\\\"[%d] Add as friend (avg: %.1f KB/s)\\\"
                        onMouseOver=\\\"mOvr(this);\\\"
                        onMouseOut=\\\"mOut(this);\\\"
                        onClick=\\\"parent.fstatus.location.href='submit?q=friend_add+%d'\\\"\\>"
                            ( if (!counter mod 2 == 0) then "dl-1" else "dl-2";) (client_num c)
                          ( float_of_int (Int64.to_int i.client_uploaded / 1024) /.
                              float_of_int (max 1 ((last_time ()) - i.client_connect_time)) )
                          (client_num c);

                          html_mods_td buf [
                            ("", "sr", Printf.sprintf "%d" (client_num c)); ];

                          client_print_html c o;
                          html_mods_td buf ([
                            ("", "sr", (string_of_kind  i.client_kind));
                            ("", "sr", Printf.sprintf "%d" (((last_time ()) - i.client_connect_time) / 60));
                            ("", "sr", i.client_software);
                            ] @
                            (if !!emule_mods_count then [("", "sr", i.client_emulemod)] else [])
                            @ [
                            ("", "sr ar", size_of_int64 i.client_downloaded);
                            ("", "sr ar", size_of_int64 i.client_uploaded);
                            ("", "sr", (match i.client_upload with
                                  Some cu -> cu
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
                  ( "1", "srh ac", "Client number", "Num" ) ;
                  ( "0", "srh", "Network", "Network" ) ;
                  ( "0", "srh", "Connection type [I]ndirect [D]irect", "C" ) ;
                  ( "0", "srh", "Client name", "Client name" ) ;
                  ( "0", "srh", "Client brand", "CB" ) ;
                  ] @
                  (if !!emule_mods_count then [( "0", "srh", "eMule MOD", "EM" )] else [])
                  @ [
                  ( "0", "srh ar", "Total DL bytes from this client for all files", "DL" ) ;
                  ( "0", "srh ar", "Total UL bytes to this client for all files", "UL" ) ;
                  ( "0", "srh", "IP address", "IP address" ) ]);

                Intmap.iter (fun cnum c ->

                    try
                      let i = client_info c in
                      incr counter;

                      Printf.bprintf buf "\\<tr class=\\\"%s\\\"
					title=\\\"Add as Friend\\\" onMouseOver=\\\"mOvr(this);\\\" onMouseOut=\\\"mOut(this);\\\"
					onClick=\\\"parent.fstatus.location.href='submit?q=friend_add+%d'\\\"\\>"
                        ( if (!counter mod 2 == 0) then "dl-1" else "dl-2";) cnum;

                      html_mods_td buf [
                        ("", "sr", Printf.sprintf "%d" (client_num c)); ];

                      client_print_html c o;

                      html_mods_td buf ([
                        ("", "sr", i.client_software);
                        ] @
                        (if !!emule_mods_count then [("", "sr", i.client_emulemod )] else [])
                        @ [
                        ("", "sr ar", size_of_int64 i.client_downloaded);
                        ("", "sr ar", size_of_int64 i.client_uploaded);
                        ("", "sr", string_of_kind i.client_kind); ]);

                      Printf.bprintf buf "\\</tr\\>";
                    with _ -> ();

                ) !CommonUploads.pending_slots_map;
                Printf.bprintf buf "\\</table\\>\\</div\\>";

              end;

            Printf.bprintf buf "\\</div\\>";
            ""
          end
        else
          begin

            Intmap.iter (fun _ c ->
                try
                  let i = client_info c in

                  client_print c o;
                  Printf.bprintf buf "client: %s downloaded: %s uploaded: %s\n" i.client_software (Int64.to_string i.client_downloaded) (Int64.to_string i.client_uploaded);
                  match i.client_upload with
                    Some cu ->
                      Printf.bprintf buf "      filename: %s\n" cu
                  | None -> ()
                with _ ->
                    Printf.bprintf buf "no info on client %d\n" (client_num c )
            ) !uploaders;

            Printf.sprintf "Total upload slots: %d (%d) | Pending slots: %d\n" nuploaders
              (Fifo.length CommonUploads.upload_clients)
            (Intmap.length !CommonUploads.pending_slots_map);


          end


    ), ":\t\t\t\tshow users currently uploading";


  ]

(*************************************************************************)
(*                                                                       *)
(*                         Driver/Downloads                              *)
(*                                                                       *)
(*************************************************************************)

let _ =
  register_commands "Driver/Downloads"
    [

    "priority", Arg_multiple (fun args o ->
        let buf = o.conn_buf in
        match args with
          p :: files ->
            let absolute, p = if String2.check_prefix p "=" then
                true, int_of_string (String2.after p 1)
              else false, int_of_string p in
            List.iter (fun arg ->
                try
                  let file = file_find (int_of_string arg) in
                  let priority = if absolute then p
                    else (file_priority file) + p in
                  let priority = if priority < -100 then -100 else
                    if priority > 100 then 100 else priority in
                  set_file_priority file priority;
                  Printf.bprintf buf "Setting priority of %s to %d\n"
                    (file_best_name file) (file_priority file);
                with _ -> failwith (Printf.sprintf "No file number %s" arg)
            ) files;
            force_download_quotas ();
            _s "done"
        | [] -> "Bad number of args"

    ), "<priority> <files numbers> :\tchange file priorities";

    "confirm", Arg_one (fun arg o ->
        match String.lowercase arg with
          "yes" | "y" | "true" ->
            List.iter (fun file ->
                try
                  file_cancel file
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
    ), " <yes|no|what>:\t\t\tconfirm cancellation";

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
    ), " <num> :\t\t\tprint the segments downloaded in file";


    "cancel", Arg_multiple (fun args o ->

        let file_cancel num =
          if not (List.memq num !to_cancel) then
            to_cancel := num :: !to_cancel
        in
        if args = ["all"] then
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
    ), "<num> :\t\t\t\tcancel download (use arg 'all' for all files)";

    "downloaders", Arg_none (fun o ->
        let buf = o.conn_buf in

        if use_html_mods o then
          html_mods_table_header buf "downloadersTable" "downloaders" ([
              ( "1", "srh ac", "Client number (click to add as friend)", "Num" ) ;
              ( "0", "srh", "Client state", "CS" ) ;
              ( "0", "srh", "Client name", "Name" ) ;
              ( "0", "srh", "Client brand", "CB" ) ;
            ] @
              (if !!emule_mods_count then [( "0", "srh", "eMule MOD", "EM" )] else [])
            @ [
              ( "0", "srh", "Overnet [T]rue, [F]alse", "O" ) ;
              ( "1", "srh ar", "Connected time (minutes)", "CT" ) ;
              ( "0", "srh", "Connection [I]ndirect, [D]irect", "C" ) ;
              ( "0", "srh", "IP address", "IP address" ) ;
              ( "1", "srh ar", "Total UL bytes to this client for all files", "UL" ) ;
              ( "1", "srh ar", "Total DL bytes from this client for all files", "DL" ) ;
              ( "0", "srh", "Filename", "Filename" ) ]);

        let counter = ref 0 in

        List.iter
          (fun file ->
            if (CommonFile.file_downloaders file o !counter) then counter := 0 else counter := 1;
        ) !!files;

        if use_html_mods o then Printf.bprintf buf "\\</table\\>\\</div\\>";

        ""
    ) , ":\t\t\t\tdisplay downloaders list";

    "verify_chunks", Arg_multiple (fun args o ->
        let buf = o.conn_buf in
        match args with
          [arg] ->
            let num = int_of_string arg in
(*            if o.conn_output = HTML then *)
              List.iter
                (fun file -> if (as_file_impl file).impl_file_num = num then
                    begin
                      Printf.bprintf  buf "Verifying Chunks of file %d" num;
                      file_check file;
                    end
              )
              !!files;
            ""
        | _ -> ();
            _s "done"
    ), "<num> :\t\t\tverify chunks of file <num>";

    "pause", Arg_multiple (fun args o ->
        if args = ["all"] then
          List.iter (fun file ->
              file_pause file;
          ) !!files
        else
          List.iter (fun num ->
              let num = int_of_string num in
              List.iter (fun file ->
                  if (as_file_impl file).impl_file_num = num then begin
                      file_pause file
                    end
              ) !!files) args; ""
    ), "<num> :\t\t\t\tpause a download (use arg 'all' for all files)";

    "resume", Arg_multiple (fun args o ->
        if args = ["all"] then
          List.iter (fun file ->
              file_resume file
          ) !!files
        else
          List.iter (fun num ->
              let num = int_of_string num in
              List.iter (fun file ->
                  if (as_file_impl file).impl_file_num = num then begin
                      file_resume file
                    end
              ) !!files) args; ""
    ), "<num> :\t\t\t\tresume a paused download (use arg 'all' for all files)";

    "commit", Arg_none (fun o ->
        List.iter (fun file ->
            file_commit file
        ) !!done_files;
        "Commited"
    ) , ":\t\t\t\t\t$bmove downloaded files to incoming directory$n";

    "vd", Arg_multiple (fun args o ->
        let buf = o.conn_buf in
        match args with
          | ["queued"] ->
              let list = List2.tail_map file_info !!files in
              let list = List.filter ( fun f -> f.file_state = FileQueued ) list in
              let list = Sort.list (fun f1 f2 -> f1.file_name >= f2.file_name) list in
              simple_print_file_list false buf list o;
              ""
          | ["paused"] ->
              let list = List2.tail_map file_info !!files in
              let list = List.filter ( fun f -> f.file_state = FilePaused ) list in
              let list = Sort.list (fun f1 f2 -> f1.file_name >= f2.file_name) list in
              simple_print_file_list false buf list o;
              ""
          | ["downloading"] ->
              let list = List2.tail_map file_info !!files in
              let list = List.filter ( fun f -> f.file_state = FileDownloading ) list in
              let list = Sort.list
                ( fun f1 f2 ->
                  Int64.sub f2.file_size f2.file_downloaded <=
                  Int64.sub f1.file_size f1.file_downloaded
                ) list in
              simple_print_file_list false buf list o;
              if !!done_files <> [] then
                begin
                  simple_print_file_list true buf
                    (List2.tail_map file_info !!done_files) o;
                  Printf.bprintf buf "Use 'commit' to move downloaded files to the incoming directory"
                end;
              ""
          | [arg] ->
            let num = int_of_string arg in
            if o.conn_output = HTML then
              begin
                if use_html_mods o then
                  Printf.bprintf buf "\\<div class=\\\"sourcesTable al\\\"\\>\\<table cellspacing=0 cellpadding=0\\>
				\\<tr\\>\\<td\\>
				\\<table cellspacing=0 cellpadding=0 width=100%%\\>\\<tr\\>
				\\<td nowrap class=\\\"fbig\\\"\\>\\<a href=\\\"files\\\"\\>Display all files\\</a\\>\\</td\\>
				\\<td nowrap class=\\\"fbig\\\"\\>\\<a href=\\\"submit?q=verify_chunks+%d\\\"\\>Verify chunks\\</a\\>\\</td\\>
				\\<td nowrap class=\\\"fbig\\\"\\>\\<a href=\\\"submit?q=preview+%d\\\"\\>Preview\\</a\\>\\</td\\>
				\\<td nowrap class=\\\"fbig pr\\\"\\>\\<a href=\\\"javascript:window.location.reload()\\\"\\>Reload\\</a\\>\\</td\\>
				\\<td class=downloaded width=100%%\\>\\</td\\>
				\\</tr\\>\\</table\\>
				\\</td\\>\\</tr\\>
				\\<tr\\>\\<td\\>" num num
                else begin	
                    Printf.bprintf  buf "\\<a href=\\\"files\\\"\\>Display all files\\</a\\>  ";
                    Printf.bprintf  buf "\\<a href=\\\"submit?q=verify_chunks+%d\\\"\\>Verify chunks\\</a\\>  " num;
                    Printf.bprintf  buf "\\<a href=\\\"submit?q=preview+%d\\\"\\>Preview\\</a\\> \n " num;
                  end
              end;
            List.iter
              (fun file -> if (as_file_impl file).impl_file_num = num then
                  CommonFile.file_print file o)
            !!files;
            List.iter
              (fun file -> if (as_file_impl file).impl_file_num = num then
                  CommonFile.file_print file o)
            !!done_files;
            ""
        | _ ->
            DriverInteractive.display_file_list buf o;
            ""
    ), "<num> :\t\t\t\t$bview file info$n";

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
          set_file_best_name file new_name;
          Printf.sprintf (_b "Download %d renamed to %s") num new_name
        with _ -> Printf.sprintf (_b "No file number %d") num
    ), "<num> \"<new name>\" :\t\tchange name of download <num> to <new name>";

    "dllink", Arg_multiple (fun args o ->
        if !verbose then lprintf "dllink\n";
        let buf = o.conn_buf in
        let query_networks url =
          if not (networks_iter_until_true
                    (fun n ->
                       try
                         network_parse_url n url
                       with e ->
                         Printf.bprintf buf "Exception %s for network %s\n"
                           (Printexc2.to_string e) (n.network_name);
                         false
                    )) then
            let output = (if o.conn_output = HTML then begin
                let buf = Buffer.create 100 in
                Printf.bprintf buf "\\<div class=\\\"cs\\\"\\>";
                html_mods_table_header buf "dllinkTable" "results" [];
                Printf.bprintf buf "\\<tr\\>";
                html_mods_td buf [ ("", "srh", "Unable to match URL"); ];
                Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-1\\\"\\>";
                html_mods_td buf [ ("", "sr", url); ];
                Printf.bprintf buf "\\</tr\\>\\</table\\>\\</div\\>\\</div\\>";
                Buffer.contents buf
              end
            else begin
                Printf.sprintf "Unable to match URL : %s" url
            end) in
            _s output
          else
            let output = (if o.conn_output = HTML then begin
                let buf = Buffer.create 100 in
                Printf.bprintf buf "\\<div class=\\\"cs\\\"\\>";
                html_mods_table_header buf "dllinkTable" "results" [];
                Printf.bprintf buf "\\<tr\\>";
                html_mods_td buf [ ("", "srh", "Added link"); ];
                Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-1\\\"\\>";
                html_mods_td buf [ ("", "sr", url); ];
                Printf.bprintf buf "\\</tr\\>\\</table\\>\\</div\\>\\</div\\>";
                Buffer.contents buf
              end
            else begin
                Printf.sprintf "Added link : %s" url
            end) in
            _s output
        in

        let url = String2.unsplit args ' ' in
        if (String2.starts_with url "http") then (
            let u = Url.of_string url in
            let module H = Http_client in
            let r = {
              H.basic_request with
                H.req_url =  u;
                H.req_proxy = !CommonOptions.http_proxy;
                H.req_request = H.HEAD;
                H.req_user_agent =
                       Printf.sprintf "MLdonkey/%s" Autoconf.current_version;
            } in
            H.whead r
                (fun headers ->
                   (* Combine the list of header fields into one string *)
                   let concat_headers =
                     (List.fold_right (fun (n, c) t -> n ^ ": " ^ c ^ "\n" ^ t) headers "")
                   in
                   ignore (query_networks concat_headers)
                );
            let output = (if o.conn_output = HTML then begin
                let buf = Buffer.create 100 in
                Printf.bprintf buf "\\<div class=\\\"cs\\\"\\>";
                html_mods_table_header buf "dllinkTable" "results" [];
                Printf.bprintf buf "\\<tr\\>";
                html_mods_td buf [ ("", "srh", "Parsing HTTP url"); ];
                Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-1\\\"\\>";
                html_mods_td buf [ ("", "sr", url); ];
                Printf.bprintf buf "\\</tr\\>\\</table\\>\\</div\\>\\</div\\>";
                Buffer.contents buf
              end
            else begin
                Printf.sprintf "Parsing HTTP url : %s" url
            end) in
            _s output
            )
        else
            query_networks url
        ), "<link> :\t\t\t\tdownload ed2k, sig2dat, torrent or other link";

    "dllinks", Arg_one (fun arg o ->
        let buf = o.conn_buf in

        let file = File.to_string arg in
        let lines = String2.split_simplify file '\n' in
        List.iter (fun line ->
            ignore (networks_iter_until_true (fun n ->
                  network_parse_url n line))
        ) lines;
        let output = (if o.conn_output = HTML then begin
            let buf = Buffer.create 100 in
            Printf.bprintf buf "\\<div class=\\\"cs\\\"\\>";
            html_mods_table_header buf "dllinksTable" "results" [];
            Printf.bprintf buf "\\<tr\\>";
            html_mods_td buf [ ("", "srh", "Added links"); ];
            Printf.bprintf buf "\\</tr\\>";
            List.iter (fun line ->
                Printf.bprintf buf "\\<tr class=\\\"dl-1\\\"\\>";
                html_mods_td buf [ ("", "sr", line); ];
                Printf.bprintf buf "\\</tr\\>\\";
            ) lines;
            Printf.bprintf buf "\\</tr\\>\\</table\\>\\</div\\>\\</div\\>";
            Buffer.contents buf
          end
        else begin
            Printf.sprintf "done"
        end) in
        _s output
    ), "<file> :\t\t\t\tdownload all the links contained in the file";

  ]



(*************************************************************************)
(*                                                                       *)
(*                         Driver/Xpert                                  *)
(*                                                                       *)
(*************************************************************************)

let _ =
  register_commands "Driver/Xpert"
    [

    "reload_messages", Arg_none (fun o ->
        CommonMessages.load_message_file ();
        "\\<script language=Javascript\\>top.window.location.reload();\\</script\\>"
    ), ":\t\t\treload messages file";

    "log", Arg_none (fun o ->
        let buf = o.conn_buf in
        log_to_buffer buf;
        _s "------------- End of log"
    ), ":\t\t\t\t\tdump current log state to console";

    "ansi", Arg_one (fun arg o ->
        let buf = o.conn_buf in
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
        let buf = o.conn_buf in
        let b = bool_of_string arg in
        set_logging b;
        Printf.sprintf (_b "log to stdout %s")
        (if b then _s "enabled" else _s "disabled")
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
        Int64Swarmer.merge file1 file2;
        "The two files are now merged"
    ), " <num1> <num2> :\t\t\ttry to swarm downloads from file <num2> (secondary) to file <num1> (primary)";

    "log_file", Arg_one (fun arg o ->
        let oc = open_out arg in
        log_to_file oc;
        _s "log started"
    ), "<file> :\t\t\tstart logging in file <file>";

    "close_log", Arg_none (fun o ->
        close_log ();
        _s "log stopped"
    ), ":\t\t\t\tclose logging to file";


    "html_mods", Arg_none (fun o ->
        let buf = o.conn_buf in

        if !!html_mods then
          begin
            html_mods =:= false;
            commands_frame_height =:= 140;
          end
        else
          begin
            html_mods =:= true;
            html_mods_style =:= 0;
            commands_frame_height =:= (snd !html_mods_styles.(!!html_mods_style));
            use_html_frames =:= true;
            CommonMessages.colour_changer() ;
          end;

        "\\<script language=Javascript\\>top.window.location.reload();\\</script\\>"
    ), ":\t\t\t\ttoggle html_mods";


    "html_mods_style", Arg_multiple (fun args o ->
        let buf = o.conn_buf in
        if args = [] then begin
            Array.iteri (fun i h ->
                Printf.bprintf buf "%d: %s\n" i (fst h);
            ) !html_mods_styles;
            ""
          end
        else begin
            html_mods =:= true;
            use_html_frames =:= true;
            html_mods_theme =:= "";
            let num = int_of_string (List.hd args) in

            if num >= 0 && num < (Array.length !html_mods_styles) then begin
                html_mods_style =:= num;
                commands_frame_height =:= (snd !html_mods_styles.(num));
                CommonMessages.colour_changer ();
              end
            else begin
                html_mods_style =:= 0;
                commands_frame_height =:= (snd !html_mods_styles.(!!html_mods_style));
                CommonMessages.colour_changer ();
              end;
            "\\<script language=Javascript\\>top.window.location.reload();\\</script\\>"
          end

    ), ":\t\t\tselect html_mods_style <#>";

    "rss", Arg_none (fun o ->
        let buf = o.conn_buf in
        let module CW = CommonWeb in
        Hashtbl.iter (fun url feed ->
            let r = feed.CW.rss_value in
            if o.conn_output = HTML then begin
                Printf.bprintf buf "\\</pre\\>\\<div class=\\\"cs\\\"\\>";
                html_mods_table_header buf "rssTable" "results" [];
                Printf.bprintf buf "\\<tr\\>";
                html_mods_td buf [
                  (r.Rss.ch_title ^ " : " ^ url ^ (Printf.sprintf ", loaded %d hours ago" (feed.CW.rss_date / 3600)), "srh", r.Rss.ch_title); ];
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
                        (title, "sr", "\\<a href=\\\"submit?q=dllink+" ^ (Url.encode link) ^ "\\\"\\ title=\\\"" ^ link ^ "\\\"\\>" ^ title ^ "\\</a\\>"); ];
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


    ), " :\t\t\t\t\tprint RSS feeds";

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
(* html_mods =:= true;
            use_html_frames =:= true; *)
            html_mods_theme =:= List.hd args;
            "\\<script language=Javascript\\>top.window.location.reload();\\</script\\>"
          end

    ), "<theme>:\t\t\tselect html_theme";

    "mem_stats", Arg_one (fun level o ->
        let buf = o.conn_buf in
        Heap.print_memstats (int_of_string level) buf
          (if o.conn_output = HTML then true else false);
        ""
    ), ":\t\t\t\tprint memory stats";

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
        "All files closed"
    ), ":\t\t\t\tclose all files (use to free space on disk after remove)";

    "debug_socks", Arg_none (fun o ->
        BasicSocket.print_sockets o.conn_buf;
        _s "done"
    ), ":\t\t\t\tfor debugging only";

    "block_list", Arg_none (fun o ->
      Ip_set.print_list o.conn_buf !Ip_set.bl;
      _s "done"
    ), ":\t\t\t\tdisplay the list of blocked IP ranges that were hit";

    "block_test", Arg_one (fun arg o ->
      let ip = Ip.of_string arg in
      _s (match match_ip_aux !Ip_set.bl ip with
	  None -> "Not blocked"
	| Some br ->
	    Printf.sprintf "Blocked, %s\n" br.blocking_description)),
      "<ip> :\t\t\tcheck whether an IP is blocked";
  ]
