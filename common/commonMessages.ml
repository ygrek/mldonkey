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

open Gettext
open Options
  
let message_file_name = try
    Sys.getenv "MLDONKEY_MESSAGES"
  with _ -> 
      Filename.concat (try Sys.getenv "HOME" with _ -> ".") 
      ".mldonkey_messages.ini"
  
let _ =
  Printf.printf "Using Message File %s" message_file_name; print_newline ()
      
let message_file = Options.create_options_file message_file_name
let message name t x = define_option message_file [name] "" t x
let string name x = define_option message_file [name] "" string_option x

  
let html_header = define_option message_file ["html_header"]
  "The header used in the WEB interface (modify to add your CSS)"
    string_option  
  "<title>MLdonkey: Web Interface</title>
<style type=\"text/css\">
<!--
body,th,td {background-color:#EAE8CF;color: #3F702E; font-family: Verdana, Arial, Helvetica, sans-serif;font-size: 12px;}
a{ text-decoration: none;}
a:hover { text-decoration: underline; color : #660000; }
a:link,a:active,a:visited { color : #660000; }
-->
</style>
<script>
<!--
function ed2k(){
var cmdValue = \"dllink \" + document.cmdFormular.q.value;
document.cmdFormular.q.value = cmdValue;
document.cmdFormular.submit();
}

function ovlink(){
var cmdValue = \"ovlink \" + document.cmdFormular.q.value;
document.cmdFormular.q.value = cmdValue;
document.cmdFormular.submit();
}
-->
  </script>


    "
  
let download_html_header = define_option message_file ["download_html_header"]
  "The header used in the WEB interface for downloads (modify to add your CSS)"
    string_option  
  "
<title>MLdonkey: Web Interface</title>
<style type=\"text/css\">
<!--
body{background-color:#E5E5E5;color: #3F702E; font-family: Verdana, Arial, Helvetica, sans-serif;font-size: 13px; margin: 2;}
td,pre {color: #3F702E; font-family: Verdana, Arial, Helvetica, sans-serif;font-size: 13px;}

td.loaded{background-color:#000077; font-size:3px;}
td.remain{background-color:#770000; font-size:3px;}

a{ text-decoration: none; font-weight: bold;}
a:link,a:active,a:visited { color: #882222; }
a:hover { color: #000000; text-decoration: underline;}

a.extern:visited,a.extern:hover,a.extern:active { color: #000099; }
a.extern:hover { color: #000000; }
-->
</style>
<script>                  
<!--
function ovlink(){
var cmdValue = \"ovlink \" + document.cmdFormular.q.value;
document.cmdFormular.q.value = cmdValue;
document.cmdFormular.submit();
}

function ed2k(){
var cmdValue = \"dllink \" + document.cmdFormular.q.value;
document.cmdFormular.q.value = cmdValue;
document.cmdFormular.submit();
}
-->
</script>
    "

    
let web_common_header = define_option message_file
    ["web_common_header"] "The header displayed in the WEB interface"
    string_option
  "
<table width=\"100%\" border=\"0\">
<tr>
<td align=\"left\" valign=\"middle\" width=\"*\"><a href=\"http://www.freesoftware.fsf.org/mldonkey/\" $O><b>MLdonkey Home</b></a></td>
<form action=\"submit\" $O name=\"cmdFormular\">
<td><input type=\"text\" name=\"q\" size=60 value=\"\"></td>
<td><input type=\"submit\" value=\"Execute\"></td>
<td><input type=\"button\" value=\"ed2k://\" onClick=\"ed2k()\"></td>
<td><input type=\"button\" value=\"fha://\" onClick=\"ovlink()\"></td>
</form>
</tr>
</table>
<table width=\"100%\" border=\"0\">
<tr>
<td><a href=\"/submit?q=vd\" $O>Downloads</a></td>
<td><a href=\"/submit?q=view_custom_queries\" $S>Custom Search</a></td>
<td><a href=\"/submit?q=vm\" $O>Connected Servers</a></td>
<td><a href=\"/submit?q=vo\" $O>Preferences</a></td>
<td align=\"right\"><a href=\"/submit?q=help\" $O>Help</a></td>
</tr>
<tr>
<td><a href=\"/submit?q=upstats\" $O>Uploads</a></td>
<td><a href=\"/submit?q=xs\" $S>Extend Search</a></td>
<td><a href=\"/submit?q=c\" $S>Connect more Servers</a></td>
<td><a href=\"/submit?q=ovweb\" $S>Load Overnet peers</a></td>
<td align=\"right\"><a class=\"extern\" href=\"http://forums.edonkey2000.com/phpBB/ \" $O>donkey Forum</a></td>
</tr>
<tr>
<td><a href=\"/submit?q=commit\" $S>Commit</a></td>
<td><a href=\"/submit?q=vs\" $O>View Searches</a></td>
<td><a href=\"/submit?q=vma\" $O>View all Servers</a></td>
<td><a href=\"/submit?q=ovstats\" $O>Overnet Stats</a></td>
<td align=\"right\"><a class=\"extern\" href=\"http://savannah.nongnu.org/projects/mldonkey/\" $O>Project</a></td>
</tr>
<tr>
<td><a href=\"/submit?q=reshare\" $S>Reshare Files</a></td>
<td><a href=\"/submit?q=vr\" $O>Search Results</a></td>
<td><a href=\"/submit?q=remove_old_servers\" $S>Remove old Server</a></td>
<td><a href=\"/submit?q=version\" $S>Version</a></td>
<td align=\"right\"><a href=\"/submit?q=kill\" $O>Kill MLdonkey</a></td>
</tr>
</table> 
"
  
  (*
  "
<table width=\"100%\" border=\"0\">
<tr>
<td align=\"left\" valign=\"middle\" width=\"*\"><a href=\"http://www.freesoftware.fsf.org/mldonkey/\" $O><b>MLdonkey Home</b></ a></td>
<form action=\"submit\" $O name=\"cmdFormular\">
<td align=\"right\" valign=\"middle\"><input type=\"text\" name=\"q\" size=60 value=\"\"></td>
<td align=\"right\" valign=\"middle\"><input type=\"submit\" value=\"Execute\"></td>
<td align=\"right\" valign=\"middle\"><input type=\"button\" value=\"Link\" onClick=\"ed2k()\"></td>
</form>
</tr>
</table>
<br>
<table width=\"100%\" border=\"0\">
<tr>
<td width=\"20%\" align=\"left\"><a href=\"/submit?q=vd\" $O>Downloads</a></td>
<td width=\"20%\" align=\"left\"><a href=\"/submit?q=view_custom_queries\" $S>Custom Search</a></td>
<td width=\"20%\" align=\"left\"><a href=\"/submit?q=vm\" $O>Connected Servers</a></td>
<td width=\"20%\" align=\"right\"><a href=\"/submit?q=help\" $O>Help</a></td>
</tr>
<tr>
<td width=\"20%\" align=\"left\"><a href=\"/submit?q=upstats\" $O>Uploads</a></td>
<td width=\"20%\" align=\"left\"><a href=\"/submit?q=xs\" $O>Extend Search</a></td>
<td width=\"20%\" align=\"left\"><a href=\"/submit?q=c\" $S>Connect more Servers</a></td>
<td width=\"20%\" align=\"right\"><a href=\"/submit?q=vo\" $O>Preferences</a></td>
</tr>
<tr>
<td width=\"20%\" align=\"left\"><a href=\"/submit?q=ovstats\" $O>Overnet Stats</a></td>
<td width=\"20%\" align=\"left\"><a href=\"/submit?q=vs\" $O>View Searches</a></td>
<td width=\"20%\" align=\"left\"><a href=\"/submit?q=vma\" $O>View all Servers</a></td>
<td width=\"20%\" align=\"right\"><a href=\"/submit?q=reshare\" $S>Reshare Files</a></td>
</tr>
<tr>
<td width=\"20%\" align=\"left\"><a href=\"/submit?q=commit\" $S>Commit</a></td>
<td width=\"20%\" align=\"left\"><a href=\"/submit?q=vr\" $O>Search Results</a></td>
<td width=\"20%\" align=\"left\"><a href=\"/submit?q=ovweb\" $O>Load Overnet peers</a></td>
<td width=\"20%\" align=\"right\"><a href=\"/submit?q=kill\" $O>Kill MLdonkey</a></td>
</tr>
</table> 
"
*)

let available_commands_are = string "available_commands_are" 
  "Available commands are:\n"
  
let command_not_authorized = string "command_not_authorized"
  "Command not authorized\n Use 'auth <password>' before."

let bad_login = string "bad_login"
  "Bad login/password"
  
let full_access = string "full_access"
  "Full access enabled"

let download_started = message "download_started"
    (T.boption (T.int T.bformat))  "Download of file %d started<br>"  

let no_such_command = message "no_such_command"
    (T.boption (T.string T.bformat))   "No such command %s\n"
  
let bad_number_of_args = string "bad_number_of_args" 
    "Bad number of arguments"
  
let _ =
  (
    try
      Options.load message_file
    with
      Sys_error _ ->
        (try Options.save message_file with _ -> ())
    | e ->
        Printf.printf "Error %s loading message file %s"
          (Printexc.to_string e) 
        (Options.options_file_name message_file);
        print_newline ();
        Printf.printf "Using default messages."; print_newline ();
  )
