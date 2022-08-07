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

open Options
open Printf2
open Md4
open BasicSocket

open GuiTypes
  
open CommonInteractive
open CommonOptions
open CommonGlobals
open CommonClient
open CommonFile
open CommonUser
open CommonNetwork
open CommonServer
open CommonTypes
  
open DcTypes
open DcOptions
open DcGlobals
open DcShared
open DcProtocol
open DcServers
open Xml


let log_prefix = "[dcInt]"

let lprintf_nl fmt =
  lprintf_nl2 log_prefix fmt

(* Start new dowload from result *)
let start_new_download u tth fdir fname fsize user group =
    try
      ignore (Hashtbl.find dc_shared_files_by_hash tth);
      if !verbose_download then lprintf_nl "Shared file with same hash exists (%s) (%s)" fname tth;
      None 
    with _ ->
        let f = new_file tth fdir fname fsize user group in   (* ...create new file *)
        match (file_state f) with
        | FileDownloaded | FileShared -> if !verbose_download then lprintf_nl "File already downloaded"; None
        | FileDownloading -> if !verbose_download then lprintf_nl "File being downloaded"; None
        | FilePaused -> if !verbose_download then lprintf_nl "File paused"; None
        | FileAborted _ | FileCancelled | FileQueued -> 
            if !verbose_download then lprintf_nl "File state invalid"; None
        | FileNew -> 
            file_add f.file_file FileDownloading;
            match u with
            | None -> Some f
            | Some user ->
              let c = new_client_to_user_with_file user f in
              c.client_state <- DcDownloadWaiting f;
              if (can_user_start_downloading user) then begin
                user.user_state <- TryingToSendFirstContact;
                c.client_state <- DcDownloadConnecting (f,current_time ());
                ignore (DcClients.try_connect_client c)
              end;
              Some f

(* Start downloading of a file by user selection from resultlist *) 
let start_result_download r user group =
  let filename = List.hd r.result_names in
  let rinfo = Hashtbl.find dc_result_info r.result_num in
  let newfile = start_new_download (Some rinfo.user) rinfo.tth rinfo.directory filename r.result_size user group in
  (match newfile with 
  | Some f -> as_file f.file_file (* return CommonFile.file *) 
  | _ -> raise Not_found )

let exn_catch f x = try `Ok (f x) with exn -> `Exn exn
let opt_default default = function None -> default | Some v -> v

let parse_url url user group =
  match exn_catch parse_magnet_url url with
  | `Exn _ -> "Not a magnet url", false
  | `Ok magnet ->
    if !verbose then
      lprintf_nl "Got magnet url %S" url;
    (* TODO multiple TTHs, multiple xt, automatic merge of downloads from different networks (?!) *) 
    match List2.filter_map (function TigerTree tth -> Some tth | _ -> None) magnet#uids with
    | [] -> "No TTH found in magnet url", false
    | tth::_ ->
      let (_ : _ option) = start_new_download None (TigerTree.to_string tth) "" magnet#name (opt_default 0L magnet#size) user group in
      magnet#name, true

(* register DC commands *)
let register_commands list =
  register_commands (List2.tail_map (fun (n,f,h) -> (n, "Direct Connect", f,h)) list)

let command l = String.concat "+" (List.map Url.encode l)

let td_command text title ?(blink=false) ?(target=`Output) cmd =
  Printf.sprintf
     "\\<td class=\\\"srb\\\" %sonMouseOver=\\\"mOvr(this);\\\"
     onMouseOut=\\\"mOut(this);\\\" title=\\\"%s\\\"
     onClick=\\\"parent.%s.location.href='submit?q=%s'\\\"\\>%s\\</td\\>"
     (if blink then "style=\\\"text-decoration:blink\\\" " else "")
     title (match target with `Output -> "output" | `Status -> "fstatus")
     (command cmd)
     text

(* Print DC hubs header *)
let dc_hublist_print_html_header buf ext =
    html_mods_table_header buf "serversTable" (Printf.sprintf "servers%s" ext) [
                ( Num, "srh", "Hub number", "#" ) ;
                ( Str, "srh", "Add hub to servers", "Add" ) ;
                ( Str, "srh", "Hub name", "Hub name" ) ; 
                ( Str, "srh", "IP address", "IP address" ) ;
                ( Num, "srh", "Users in hub", "Users" ) ;
                ( Str, "srh", "Hub info", "Info" ) ]

(* print in html or txt list of hubs *)
let hublist_print h hnum o =
  let buf = o.conn_buf in
  let hname = shorten_string h.dc_name 50 in
  let hinfo = shorten_string h.dc_info 50 in
  if use_html_mods o then begin
    Printf.bprintf buf "
    \\<tr class=\\\"dl-%d\\\"\\>
    \\<td class=\\\"srb\\\" \\>%d\\</td\\>
    %s
    \\<td class=\\\"sr\\\"\\>%s\\</td\\>
    \\<td class=\\\"sr\\\"\\>%s:%d\\</td\\>
    \\<td class=\\\"sr\\\"\\>%d\\</td\\>
    \\<td width=\\\"100%%\\\" class=\\\"sr\\\"\\>%s\\</td\\>\\</tr\\>\n"
    (html_mods_cntr ())
    hnum
    (td_command "Add" "Add" ~target:`Status
      ["dcn"; Ip.string_of_addr h.dc_ip; string_of_int h.dc_port])
    hname
    (Ip.string_of_addr h.dc_ip) h.dc_port
    h.dc_nusers hinfo
  end else begin
    Printf.bprintf buf "[%5d] %20s %25s:%-10d Users:%-8d %20s\n"
      hnum
      hname
      (Ip.string_of_addr h.dc_ip) h.dc_port
      h.dc_nusers
      hinfo
                  end

(* Print DC users header *)
let dc_user_print_html_header buf =
    html_mods_table_header buf "serversTable" "servers" [
                ( Num, "srh", "User number", "#" );
                ( Str, "srh", "User name", "Name" );
                ( Str, "srh", "User type", "Type" );
                ( Num, "srh", "Users slots (all/free)", "Slots" );
                ( Num, "srh", "Users connected hubs (Normal/Vipped/Opped)", "Hubs" );
                ( Str, "srh", "Users mode", "Mode" );
                ( Num, "srh", "Users shared size", "Shared" );
                ( Str, "srh", "User state", "State" );
                ( Str, "srh", "User description field", "Description" );
                ( Num, "srh", "User clients number", "Clients" );
                ( Num, "srh", "Users servers number", "Servers" );
                ( Str, "srh", "Download this clients filelist", "Filelist" );
                ( Str, "srh", "Open chat window with this user. Blinking tells there are new unread messages", "Chat");
                ( Num, "srh", "User total uploaded bytes", "Up" );
                ( Num, "srh", "User total downloaded bytes", "Down" );
                ( Str, "srh", "User client supports", "Supports" ); ];
    ()

(* print in html or txt list of users *)
let user_print user num o =
  let buf = o.conn_buf in
  let utype =
    (match user.user_type with
    | Normal -> "Normal"
    | Vip -> "Vip"
    | Op -> "Op"
    (*| Bot -> "Bot"*) )
  in
  let state =
    (match user.user_state with
    | UserIdle -> "NotDefined"
    | TryingToSendFirstContact -> "TryingToSendFirstContact"
    | UserActiveMeInitiating -> "UserActiveMeInitiating"
    | UserActiveUserInitiating -> "UserActiveUserInitiating"
    | UserPassiveUserInitiating _ -> "UserPassiveUserInitiating" )
  in
  let clients = List.length user.user_clients in
  let servers = List.length user.user_servers in
  let messages = user_has_new_messages user in 
  let hasmynick = has_my_nick user in
  let hubs =
    let a,b,c = user.user_myinfo.hubs in
    Printf.sprintf "(%d/%d/%d)" a b c
  in 
  let supports = 
    if (List.length user.user_clients > 0) then begin
      let c = List.hd user.user_clients in
        (match c.client_supports with
        | Some supports ->
            DcProtocol.Supports.create_supports_string (ClientSupports supports)
        | _ -> empty_string )
    end else empty_string
  in  
  if use_html_mods o then begin
    Printf.bprintf buf "
    \\<tr class=\\\"dl-%d\\\"\\>
    \\<td class=\\\"srb\\\" \\>%d\\</td\\>
    \\<td class=\\\"sr\\\"\\>%s\\</td\\>
    \\<td class=\\\"sr\\\"\\>%s\\</td\\>
    \\<td class=\\\"sr\\\"\\>%d\\</td\\>
    \\<td class=\\\"sr\\\"\\>%s\\</td\\>
    \\<td class=\\\"sr\\\"\\>%c\\</td\\>
    \\<td class=\\\"sr\\\"\\>%s\\</td\\>
    \\<td class=\\\"sr\\\"\\>%s\\</td\\>
    \\<td class=\\\"sr\\\"\\>%s\\</td\\>
    \\<td class=\\\"sr\\\"\\>%d\\</td\\>
    \\<td class=\\\"sr\\\"\\>%d\\</td\\>
    %s%s
    \\<td class=\\\"sr\\\"\\>%s\\</td\\>
    \\<td class=\\\"sr\\\"\\>%s\\</td\\>
    \\<td class=\\\"sr\\\"\\>%s\\</td\\>\\</tr\\>\n"
    (html_mods_cntr ()) num user.user_nick utype user.user_myinfo.slots hubs user.user_myinfo.mode
    (size_of_int64 user.user_myinfo.sharesize) state user.user_myinfo.description clients servers
    (if not hasmynick && (servers > 0) then  (* is connected to any servers with us *)
       td_command "Get List" "Download user filelist" ~target:`Status ["dcloadfilelist"; user.user_nick]
     else begin
       let txt =
         if hasmynick then "Me"
         else "No connection"
       in 
       Printf.sprintf "\\<td class=\\\"sr\\\"\\>%s\\</td\\>" txt
     end ) 
    (if not hasmynick then (* not me  *)
       td_command "Open chat" "Open message window to this user" ~blink:messages ["dcmessages"; user.user_nick]
     else 
       "\\<td class=\\\"sr\\\"\\>\\</td\\>" )
     (size_of_int64 user.user_uploaded) (size_of_int64 user.user_downloaded) supports
  end else 
    Printf.bprintf buf "[%5d] %-20s %8s %20s\n" num user.user_nick utype state

(* Print DC hubs header *)
let dc_hub_print_html_header buf =
    html_mods_table_header buf "serversTable" "servers" [
                ( Num, "srh", "Hub number", "#" ) ;
                ( Str, "srh", "Set/UnSet server autoconnection state", "Auto" ) ;
                ( Str, "srh", "Hub name", "Hub name" ) ;
                ( Str, "srh", "IP address", "IP address" ) ;
                ( Str, "srh", "My state in this hub", "State" ) ;
                ( Num, "srh", "Users in hub", "Users" ) ;
                ( Str, "srh", "Hub info", "Info" );
                ( Str, "srh", "Open chat window with this hub. Blinking tells there are new unread message", "Chat" ) ]

(* Print list of connected hubs *)
let hub_print s num o =
  let buf = o.conn_buf in
  let sinfo = shorten_string s.server_info 50 in
  let sname = shorten_string s.server_name 50 in
  let sip = Ip.to_string s.server_ip in
  let sport = s.server_port in
  let susers = List.length s.server_users in
  let smessages = ((List.length s.server_messages) > s.server_read_messages) in
  let sstate = dc_hubstate_to_text s in
  if use_html_mods o then begin
    Printf.bprintf buf "
    \\<tr class=\\\"dl-%d\\\"\\>
    \\<td class=\\\"srb\\\" \\>%d\\</td\\>
    %s
    \\<td class=\\\"sr\\\"\\>%s\\</td\\>
    \\<td class=\\\"sr\\\"\\>%s:%d\\</td\\>
    \\<td class=\\\"sr\\\"\\>%s\\</td\\>
    %s
    \\<td class=\\\"sr\\\"\\>%s\\</td\\>
    %s\\</tr\\>\n"
    (html_mods_cntr ())
    num
    (td_command
      (if s.server_autoconnect then "UnSet" else "Set") 
      "Set this hub autoconnection state"
      ["dcautoconnect"; (if s.server_autoconnect then "false" else "true"); sip] )
    sname sip sport sstate 
    (td_command (string_of_int susers) "Show users for this hub only" ["dcusers";sip] )
    sinfo
    (td_command "Open chat" "Open this hubs chat windows" ~blink:smessages ["dcmessages";sip;string_of_int sport])
  end else begin
    Printf.bprintf buf "[%5d] %20s %25s:%-10d Users:%-8d %20s\n"
      num
      sname
      sip sport
      susers
      sinfo
            end

(* Print DC clients header *)
let dc_client_print_html_header buf =
    html_mods_table_header buf "serversTable" "servers" [
                ( Num, "srh", "Client number", "#" );
                ( Str, "srh", "Remove Client", "Rem" );
                ( Str, "srh", "Client name", "Name" );
                ( Str, "srh", "Client ip/port", "Ip:Port" );
                ( Str, "srh", "Client state", "State" );
                ( Str, "srh", "Client connection", "Conn" );
                ( Str, "srh", "Client last error/count", "Error" );
                ( Str, "srh", "Client file", "File" ); ];
            ()

(* print in html or txt list of clients *)
let client_print name client num o =
  let buf = o.conn_buf in
  let ip,port =
  (match client.client_addr with
   | Some (ip,port) -> Ip.to_string ip,port
   | None -> "None",0 )
      in
  let conn = 
    (match client.client_sock with
    | Connection _ -> "Connected"
    | ConnectionWaiting _ -> "Connecting..."
    | NoConnection -> "NoConnection" )
  in 
  let state = client_state_to_string client in
  let error = 
    (match client.client_error with
    | NoError -> empty_string
    | NoFreeSlots -> Printf.sprintf "NoFreeSlots %d" client.client_error_count
    | FileNotAvailable -> Printf.sprintf "FileNotAvailable %d" client.client_error_count 
    | UserNotReachable -> Printf.sprintf "UserNotReachable %d" client.client_error_count
    | ClosedOnInit -> Printf.sprintf "ClosedOnInit %d" client.client_error_count
    | ConnectionResetByPeer -> Printf.sprintf "ConnectionResetByPeer %d" client.client_error_count
    | UploadError -> Printf.sprintf "UploadError %d" client.client_error_count
    | UserDontReplyOnTime -> Printf.sprintf "UserDontReplyOnTime %d" client.client_error_count )
  in
  let fil = 
    (match client.client_state with 
    | DcDownloadWaiting file
    | DcDownloadConnecting (file,_)
    | DcDownload file -> file.file_name
    | DcUpload (_,fd_file,_,_) 
    | DcUploadList fd_file
    | DcDownloadList fd_file -> Unix32.filename fd_file
    | DcUploadListStarting filename -> filename
    | DcUploadStarting (dcsh,_,_) -> dcsh.dc_shared_codedname
    | _ -> "None" )
  in
  if use_html_mods o then begin
    Printf.bprintf buf "
    \\<tr class=\\\"dl-%d\\\"\\>
    \\<td class=\\\"sr\\\" \\>%d\\</td\\>
    %s
    \\<td class=\\\"sr\\\"\\>%s\\</td\\>
    \\<td class=\\\"sr\\\"\\>%s:%d\\</td\\>
    \\<td class=\\\"sr\\\" \\>%s\\</td\\>
    \\<td class=\\\"sr\\\" \\>%s\\</td\\>
    \\<td class=\\\"sr\\\" \\>%s\\</td\\>
    \\<td class=\\\"sr\\\" \\>%s\\</td\\>\\</tr\\>\n"
    (html_mods_cntr ()) num
    (td_command "Rem" "Remove client" ~target:`Status 
      ["dcremclient"; string_of_int (client_num (as_client client.client_client))] )
    name ip port state conn error fil
  end else 
    Printf.bprintf buf "[%5d] %25s %25s:%-10d S:%15s C:%15s F:%15s\n" 
      num name ip port state conn fil

(* Print DC files header *)
let dc_file_print_html_header buf =
    html_mods_table_header buf "serversTable" "servers" [
                ( Num, "srh", "File number", "#" );
                ( Str, "srh", "File name/path", "File" );
                ( Num, "srh", "File size", "Size" );
                ( Str, "srh", "Tiger Tree Hash and magnet url", "TTH and magnet" ); 
                ( Num, "srh", "Files clients number (sources)", "Clients" );
                ( Num, "srh", "Autosearches done", "Searches" );
                ( Str, "srh", "Find new source by tth", "Find TTH" );
                ( Str, "srh", "Find new source by similar name context", "Find similar" );  ];
    ()  

let html_show_tth file size tth =
  begin match exn_catch TigerTree.of_string tth with
  | `Exn _ -> ""
  | `Ok hash ->
    let magnet = object 
      method name = Filename.basename file
      method size = match size with 0L -> None | _ -> Some size (* do not report size if not available *)
      method uids = [TigerTree hash]
    end in
    Printf.sprintf "\\<a href=\\\"%s\\\"\\>%s\\</a\\>" (show_magnet_url magnet) tth
  end

let html_show_shared dcsh =
  html_show_tth dcsh.dc_shared_fullname dcsh.dc_shared_size dcsh.dc_shared_tiger_root

let html_show_file file =
  html_show_tth file.file_name file.file_file.impl_file_size file.file_unchecked_tiger_root

(* TODO better *)
let translate' s =
  let s = String2.replace_char s char32 char42 in  (*   to * *)
  let s = String2.replace_char s char39 char58 in  (* ' to : *)
  let s = String2.replace_char s char38 char60 in  (* & to < *)
  s

let translate s =
  String2.replace_char (translate' s) char43 char62  (* + to > *)

let untranslate' s =
  let s = String2.replace_char s char42 char32 in  (* * to   *)
  let s = String2.replace_char s char58 char39 in  (* : to ' *)
  let s = String2.replace_char s char60 char38 in  (* < to & *)
  s

let untranslate s =
  String2.replace_char (untranslate' s) char62 char43 (* > to + *)

(* print in html or txt list of files *)
let file_print file num o =
  let buf = o.conn_buf in
  if use_html_mods o then begin
    Printf.bprintf buf "
    \\<tr class=\\\"dl-%d\\\"\\>
    \\<td class=\\\"srb\\\" \\>%d\\</td\\>
    \\<td class=\\\"sr\\\"\\>%s\\</td\\>
    \\<td class=\\\"srb\\\" \\>%Ld\\</td\\>
    \\<td class=\\\"srb\\\" \\>%s\\</td\\>
    \\<td class=\\\"srb\\\" \\>%d\\</td\\>
    \\<td class=\\\"srb\\\" \\>%d\\</td\\>
    %s
    %s\\</tr\\>\n"
    (html_mods_cntr ()) num file.file_name file.file_file.impl_file_size 
    (html_show_file file) (List.length file.file_clients) file.file_autosearch_count
    (td_command "Find TTH" "Find new client for this file by TTH" ["dcfindsource"; file.file_unchecked_tiger_root])
    (td_command "Find similar" "Find new client for this file by similar name" ["dcfindsource"; translate' file.file_name])
  end else
    Printf.bprintf buf "[%5d] %40s %-15Ld %5d\n"
      num file.file_name file.file_file.impl_file_size (List.length file.file_clients)

(* Print DC shared files header *)
let dc_shared_print_html_header buf =
    html_mods_table_header buf "serversTable" "servers" [
                ( Num, "srh", "File number", "#" );
                ( Str, "srh", "Shared file name", "Name" );
                ( Num, "srh", "Shared file size", "Size" );
                ( Str, "srh", "Tiger Tree Hash and magnet url", "TTH and magnet" );
                (*( Num, "srh", "Shared files Tiger tree array length", "TTree #" );*) ];
    ()


(* print in html or txt list of shared  files *)
let shared_print dcsh num o =
  let buf = o.conn_buf in
  if use_html_mods o then begin
    Printf.bprintf buf "
    \\<tr class=\\\"dl-%d\\\"\\>
    \\<td class=\\\"srb\\\" \\>%d\\</td\\>
    \\<td class=\\\"sr\\\"\\>%s\\</td\\>
    \\<td class=\\\"srb\\\" \\>%Ld\\</td\\>
    \\<td class=\\\"srb\\\" \\>%s\\</td\\>\\</tr\\>\n"
    (html_mods_cntr ()) num dcsh.dc_shared_codedname dcsh.dc_shared_size
    (html_show_shared dcsh)
  end else
    Printf.bprintf buf "[%5d] %40s %-15Ld %24s\n"
      num dcsh.dc_shared_codedname dcsh.dc_shared_size dcsh.dc_shared_tiger_root 

(* Print DC filelist header *)
let dc_filelist_print_html_header buf =
    html_mods_table_header buf "serversTable" "servers" [
                ( Num, "srh", "Number", "#" ) ;
                ( Str, "srh", "Filelist name", "Filelist" ) ]

(* Print one line from filelist *)
let filelist_print fname line o =
  let buf = o.conn_buf in
  if use_html_mods o then begin
    Printf.bprintf buf "
    \\<tr class=\\\"dl-%d\\\"\\>
    \\<td class=\\\"srb\\\" \\>%d\\</td\\>
    %s
    \\</tr\\>\n"
    (html_mods_cntr ())
    line
    (td_command fname "Open filelist" ["dcshowfilelist"; fname])
  end else begin
    Printf.bprintf buf "[%5d] %s\n" line fname
  end

type dc_int_groups = G_users|G_hubs|G_clients|G_files|G_shared|G_filelists

(* register users,clients,files *)
let dc_list o group_type group_name =
    let buf = o.conn_buf in
    let num = ref 1 in
    html_mods_cntr_init ();
    let html f = if use_html_mods o then f buf else () in
    begin try
         begin match group_type with
         | G_users -> 
             let new_messages_list = ref [] in       (* lets order users with unread messages to the top *)
             let others_list = ref [] in
             Hashtbl.iter (fun _ user ->
               if user_has_new_messages user then new_messages_list := user :: !new_messages_list
               else others_list := user :: !others_list
             ) users_by_name;
             html dc_user_print_html_header;
             List.iter (fun user -> user_print user !num o; incr num) !new_messages_list;
             List.iter (fun user -> user_print user !num o; incr num) !others_list;
         | G_hubs ->
             html dc_hub_print_html_header;
             Hashtbl.iter (fun _ s -> hub_print s !num o; incr num) servers_by_ip
             (*List.iter (fun s -> hub_print s !num o; incr num) !connected_servers*)
         | G_clients ->
             html dc_client_print_html_header;
             List.iter (fun c ->
               (match c.client_name with
                 | Some n -> client_print n c !num o; incr num
                 | None -> () )
             ) !clients_list
         | G_files ->
             html dc_file_print_html_header;
             List.iter (fun file -> file_print file !num o; incr num) !current_files;
         | G_shared ->
             html dc_shared_print_html_header;
             Hashtbl.iter (fun _ dcsh -> shared_print dcsh !num o; incr num) dc_shared_files_by_codedname
         | G_filelists ->
             html dc_filelist_print_html_header;
             let filelist = Unix2.list_directory filelist_directory in
             List.iter (fun fname -> filelist_print fname !num o; incr num) filelist;
         end;
         if use_html_mods o then 
           Printf.bprintf buf "\\</table\\>\\</div\\>";
      with e -> 
        lprintf_nl "Exception %s in printing %s" (Printexc2.to_string e) group_name
    end;
    empty_string

(* Print DC filelist files header *)
let dc_filelist_files_print_html_header buf =
    html_mods_table_header buf "serversTable" (Printf.sprintf "servers") [
                ( Num, "srh", "Number", "#" );
                ( Str, "srh", "File/Directory name", "File/Directory name" );
                ( Num, "srh", "File Size", "Size" );
                ( Str, "srh", "Files TTH", "TTH" ) ]

(* Print one line from filelist file *)
let filelist_file_print is_file spaces username dir fname fsize ftth line o =
  (* is_file  = if true, make the whole filename a link with submit command to load a file 
     spaces
     username = username to submit in command
     dir      = current directory path to submit in command
     fname    = filename from mylist
     fsize    = filesize from mylist
     ftth     = tth from mylist *)
  let buf = o.conn_buf in
  if use_html_mods o then begin
    Printf.bprintf buf "
    \\<tr class=\\\"dl-%d\\\"\\>
    \\<td class=\\\"srb\\\" \\>%d\\</td\\>
    %s
    \\<td class=\\\"srb\\\" \\>%s\\</td\\>
    \\<td class=\\\"srb\\\" \\>%s\\</td\\>\\</tr\\>\n"
    (html_mods_cntr ())
    line
    (if is_file then
       td_command (spaces^fname) "Start downloading" ~target:`Status
         ["dcloadfile"; username; ftth; translate dir; translate fname; fsize]
     else
       Printf.sprintf "\\<td class=\\\"srb\\\" \\>\\<b\\>%s%s\\</b\\>\\</td\\>" spaces fname
    )
    fsize
    ftth
  end else begin
    Printf.bprintf buf "%30s %10s %30s\n" fname fsize ftth
  end

(* Print DC info header *)
let dc_info_html_header buf =
  html_mods_table_header buf "sharesTable" "shares" [
       ( Str, "srh", "Direct Connect information", "DC Info" ) ;
       ( Str, "srh", empty_string, empty_string ) ]

(* Print DC info *)
let dc_info_print info data line o =
  let buf = o.conn_buf in
  if use_html_mods o then begin
    Printf.bprintf buf " 
      \\<tr class=\\\"dl-%d\\\"\\>
      \\<td class=\\\"sr\\\"\\>%s\\</td\\>
      \\<td class=\\\"sr\\\"\\>%s\\</td\\>\\</tr\\>"
      (html_mods_cntr ()) info data
  end else begin
    Printf.bprintf buf "%s:  %s\n" info data
  end

let show_dc_buttons o =
  let buf = o.conn_buf in
  let button id ?(cmd="dc"^id) ?(txt=String.capitalize id) () =
    Printf.bprintf buf "\\<form style=\\\"margin: 0px;\\\" id=\\\"%s\\\" name=\\\"%s\\\"
      action=\\\"javascript:parent.output.location.href='submit?q=%s'\\\"\\>
      \\<td\\>\\<input style=\\\"font-family: verdana; font-size: 12px;\\\" type=submit
      Value=\\\"%s\\\"\\>\\</td\\>\\</form\\>" id id cmd txt
  in
  if use_html_mods o then 
  begin
    Printf.bprintf buf "\\<table\\>\\<tr\\>";
    button "users" ~cmd:"dcusers+all" ();
    button "clients" ~cmd:"dcclients" ();
    button "hubs" ();
    button "shared" ();
    button "files" ();
    button "info" ~txt:"DC Info" ();
    button "hublistshow" ~cmd:"dchublist" ~txt:"Show hublist" ();
    button "filelists" ();
    Printf.bprintf buf "\\</tr\\>\\</table\\>";
  end

(* List of commands to register *)
let commands = [

  "dc", Arg_none (fun o ->
    if use_html_mods o then
      show_dc_buttons o
    else
      Printf.bprintf buf "Try `?? dc` for more commands\n";
    dc_list o G_hubs "hubs"
  ), ": Show Direct Connect buttons";

  (* 'dcn address [port]'  Add a new DC server with optional port (default 411) *)
  "dcn", Arg_multiple (fun args o ->
    let ip, port =
      (match args with
      | [ip ; port] -> ip, port
      | [ip] -> ip, "411"
      | _ -> failwith "dcn <ip> [<port>]: bad argument number" )
    in
    let ip_addr = Ip.addr_of_string ip in
    Ip.async_ip_of_addr ip_addr (fun t -> (* do DNS check here *)
      let port = int_of_string port in
      if !verbose_msg_servers then lprintf_nl "New server being added: (%s) (%s) (%d)" ip (Ip.to_string t) port;
      if (Ip.valid t) && (port>0) && (port<65536) then 
        ignore (new_server ip_addr t port) ) (fun _ -> ());
    empty_string
  ), "<ip> [<port>] : Add a server. Default port number is 411";

  (* List connected hubs for chatting *)
  "dchubs", Arg_none (fun o -> show_dc_buttons o; dc_list o G_hubs "hubs" 
  ), ": Show connected DC hubs";

  (* List all DC users *)
  "dcusers", Arg_one (fun args o ->
    show_dc_buttons o;
    let buf = o.conn_buf in
    (match args with
    | "all" -> dc_list o G_users "users"
    | ip -> 
        (try
          let s = Hashtbl.find servers_by_ip ip in
          let num = ref 1 in
          let new_messages_list = ref [] in       (* lets order users with unread messages to the top *)
          let others_list = ref [] in
          List.iter (fun user ->
            if user_has_new_messages user then new_messages_list := user :: !new_messages_list
            else others_list := user :: !others_list
          ) s.server_users;
          dc_user_print_html_header buf;
          List.iter (fun user -> user_print user !num o; incr num) !new_messages_list;
          List.iter (fun user -> user_print user !num o; incr num) !others_list;
          empty_string 
        with _ -> "dcusers <ip> : ip not valid" ) );
  ), "<all>|<ip> :Show DC users"; 

  (* List all DC clients *)
  "dcclients", Arg_none (fun o -> show_dc_buttons o; dc_list o G_clients "clients"
  ), ": Show all DC clients"; 

  (* List all DC files *)
  "dcfiles", Arg_none (fun o -> show_dc_buttons o; dc_list o G_files "files"
  ), ": Show all DC files";

  (* List all DC shared files *)
  "dcshared", Arg_none (fun o -> show_dc_buttons o; dc_list o G_shared "shared"
  ), ": Show all DC shared files. All/Hashed ";

  (* 'dchublist [args]' - Show dchub list with optional filters args (max 5) *)
  "dchublist", Arg_multiple (fun args o ->
    show_dc_buttons o;
    let buf = o.conn_buf in
    let filter = ref [] in
    let print_hublist () =
      if use_html_mods o then
        begin
          html_mods_table_one_row buf "serversTable" "servers" [
            (empty_string, "srh", Printf.sprintf "Showing hublist"); ];
          Printf.bprintf buf "\\</table\\>\\</div\\>"
        end
      else
        Printf.bprintf buf "Showing hublist";
      html_mods_cntr_init ();
      let nb_hubs = ref 0 in
      if use_html_mods o then dc_hublist_print_html_header buf empty_string;
      let show_all = if (!filter = []) then true else false in
      List.iter (fun h ->
        let hub_has_string searched =
          if String2.contains (Ip.string_of_addr h.dc_ip) searched ||
            String2.contains (string_of_int h.dc_port) searched ||
            String2.contains h.dc_info searched ||
            String2.contains h.dc_name searched then true
          else false in
        let print_hub () =
          (try
            hublist_print h !nb_hubs o;
            incr nb_hubs;
           with e ->
             if !verbose_msg_servers then
               lprintf_nl "Exception %s in hub_print\n" (Printexc2.to_string e))
        in
        if show_all then
          print_hub ()
        else
          begin
            let print = ref false in
            let finished = ref false in
            let counter = ref 0 in
            let filters_length = List.length !filter in
            while (!print = false) && (!finished = false) do
              if (!counter = filters_length) || (!counter > 5) then
                finished := true
              else
                if (hub_has_string (List.nth !filter !counter)) then print := true;
              incr counter
            done;
            if (!print = true) then print_hub ()
          end
      ) !dc_hublist;
      let txt = if show_all then "(showing all hubs from hublist)" else "(filtered)" in
      if use_html_mods o then
        begin
          Printf.bprintf buf "\\</table\\>\\</div\\>";
          html_mods_table_one_row buf "serversTable" "servers" [
            (empty_string, "srh", Printf.sprintf "Hubs: %d known %s" !nb_hubs txt); ]
        end
      else
        Printf.bprintf buf "Hubs: %d known %s" !nb_hubs txt
    in
    (match args with
    | [] -> ()
    | rest_args -> filter := rest_args
    );
    print_hublist ();
    empty_string
  ), "[filtertext]: dchublist fin - filters hubs with text fin";

  (* 'dcuserip name' query user-ip from hub  *)
  "dcuserip", Arg_multiple (fun args o ->
    let buf = o.conn_buf in
    (match args with
    | [hub ; port ; name] -> 
        (try
          let s = Hashtbl.find servers_by_ip hub in
          (match s.server_sock with
          | Connection sock -> dc_send_msg sock ( UserIPReq ( [name] ))
          | _ -> () ) 
        with _ -> 
          if !verbose_unexpected_messages then
            lprintf_nl "dcuserip: No server found by ip (%s) (%s) (%s)" hub port name )
    | _ -> if !verbose_unexpected_messages then lprintf_nl "dcuserip: Invalid args count (%d)" (List.length args) );
    Printf.bprintf buf "User query sent to hubs\n";
    empty_string
  ), "<user> : Query users ip from hub";

  "dcmsglog", Arg_multiple (fun args o ->
    let buf = o.conn_buf in
    let counter = ref 0 in
    let messages,name,topic =
      (match args with
      | delay :: ip :: port :: _ ->
          (try
            let s = Hashtbl.find servers_by_ip ip in
            let topic = 
              (match s.server_sock with
              | Connection _ -> s.server_topic
              | _ -> "NOT CONNECTED TO SERVER" )
            in
            s.server_read_messages <- List.length s.server_messages;      (* messages are set as read before   *) 
            s.server_messages,                                            (* they are actually printed to user *)
            (shorten_string s.server_name 50),
            topic
          with _ ->
              if !verbose_unexpected_messages then lprintf_nl "dcmsglog: No server with address found";
              raise Not_found )
      | delay :: n :: _ ->
          (try
            let u = search_user_by_name n in
            let connected = ((List.length u.user_servers) > 0) in
            u.user_read_messages <- List.length u.user_messages;           (* messages are set as read before   *)
            u.user_messages, u.user_nick,                                  (* they are actually printed to user *)
            (if connected then empty_string
             else "User not connected to any servers at the moment...")
          with _ ->
              if !verbose_unexpected_messages then lprintf_nl "dcmsglog: No user found";
              raise Not_found )
      | _ ->
          if !verbose_unexpected_messages then lprintf_nl "dcmsglog: Invalid args count (%d)" (List.length args);
          raise Not_found )
    in
    if use_html_mods o then begin
       Printf.bprintf buf "\\<div class=\\\"messages\\\"\\>";
       Printf.bprintf buf "\\<div\\>Chatting with \\<b\\>%s\\</b\\> - %d logged messages\\</div\\>"
         name (List.length messages);
       Printf.bprintf buf "\\<div\\>\\<i\\>%s\\</i\\>\\</div\\>" topic 
    end else     
      Printf.bprintf buf "%d logged messages\n" (List.length messages);
    if use_html_mods o then html_mods_table_header buf "serversTable" "servers" [
                ( Str, "srh", "Timestamp", "Time" );
                ( Str, "srh", "Who message is from", "From" );
                ( Str, "srh", "Message text", "Message" ) ];
    List.iter (fun (t,f,m) ->
      let msg =
        (match m with
        | PrivateMessage (_, msg) -> msg
        | PublicMessage (_, msg) -> msg
        | ServerMessage msg -> msg )
      in
      if use_html_mods o then begin  
        Printf.bprintf buf "\\<tr class=\\\"%s\\\"\\>"
          (if (!counter mod 2 == 0) then "dl-1" else "dl-2");
          html_mods_td buf [
            (empty_string, "sr", Date.simple (BasicSocket.date_of_int t));
            (empty_string, "sr", f);
            (empty_string, "srw", String2.replace msg '\r' "\\<br/\\>") ];
          Printf.bprintf buf "\\</tr\\>"
      end else begin
        Printf.bprintf buf "\n%s [%s] : %s\n" (Date.simple (BasicSocket.date_of_int t)) f msg
      end;
      incr counter
    ) messages;
    if use_html_mods o then Printf.bprintf buf "\\</table\\>\\</div\\>\\</div\\>";
    empty_string
  ), "<refresh> <user> | <refresh> <serverip> <serverport>";

  "dcmessages", Arg_multiple (fun args o ->
    show_dc_buttons o;
    let buf = o.conn_buf in
    let s,u =
      (match args with
      | ip :: port :: _ ->
          (try
            let s = Hashtbl.find servers_by_ip ip in
            Some s, None
          with _ ->
              if !verbose_unexpected_messages then lprintf_nl "dcmessages: No server with address found";
              raise Not_found )
      | n :: _ ->
          (try
            let u = search_user_by_name n in       
            None, Some u  
          with _ ->
              if !verbose_unexpected_messages then lprintf_nl "dcmessages: No user found";
              raise Not_found )
      | _ ->
          if !verbose_unexpected_messages then lprintf_nl "dcmessages: No user or server";
          raise Not_found )
    in
    if use_html_mods o then begin
      Printf.bprintf buf "\\<script type=\\\"text/javascript\\\"\\>
\\<!--
function submitCmd() {
var formID = document.getElementById(\\\"msgForm\\\")
parent.output.location.href='submit?q=dcmessages+'+encodeURIComponent(formID.sendCmd.value)
}
function submitMessageForm() {
var formID = document.getElementById(\\\"msgForm\\\")
var msgTextOut = encodeURIComponent(formID.msgText.value)
var msgUserOut = encodeURIComponent(formID.sendCmd.value)
parent.fstatus.location.href='submit?q=dcsendmsg+'+msgUserOut+\\\"+\\\"+msgTextOut
formID.msgText.value=\\\"\\\"
msgWindow.location.reload();
}
//--\\>
\\</script\\>";
      let sendmsg,namelist,first =
        (match s with 
        | Some s ->
            let ip,port = (Ip.to_string s.server_ip),(string_of_int s.server_port) in
            command ["dcmsglog";"20";ip;port], s.server_users, Printf.sprintf "%s %s" ip port 
        | None ->
            (match u with 
            | Some u ->
                command ["dcmsglog";"20";u.user_nick], [], u.user_nick
            | None -> 
                if !verbose_unexpected_messages then lprintf_nl "dcmessages: No user or server";
                raise Not_found )
  )
      in
      Printf.bprintf buf "\\<iframe id=\\\"msgWindow\\\" name=\\\"msgWindow\\\" height=\\\"80%%\\\"
        width=\\\"100%%\\\" scrolling=yes src=\\\"submit?q=%s\\\"\\>\\</iframe\\>" sendmsg;
      Printf.bprintf buf "\\<form style=\\\"margin: 0px;\\\" name=\\\"msgForm\\\" id=\\\"msgForm\\\" action=\\\"javascript:submitMessageForm()\\\"\\>";
      Printf.bprintf buf "\\<table width=100%% cellspacing=0 cellpadding=0 border=0\\>\\<tr\\>\\<td\\>";
      Printf.bprintf buf "\\<select style=\\\"font-family: verdana; font-size: 12px; width: 150px;\\\" id=\\\"sendCmd\\\" name=\\\"sendCmd\\\" \\>";
      Printf.bprintf buf "\\<option value=\\\"%s\\\"\\>%s" first first;
      List.iter (fun u ->
        if not (has_my_nick u) then
          Printf.bprintf buf "\\<option value=\\\"%s\\\"\\>%s" u.user_nick u.user_nick
      ) namelist;
      Printf.bprintf buf "\\</select\\>\\</td\\>";
      Printf.bprintf buf "\\<td width=100%%\\>\\<input style=\\\"width: 99%%; font-family: verdana; font-size: 12px;\\\"
        type=text id=\\\"msgText\\\" name=\\\"msgText\\\" size=50 \\>\\</td\\>";
      Printf.bprintf buf "\\<td\\>\\<input style=\\\"font-family: verdana;
        font-size: 12px;\\\" type=submit value=\\\"Send\\\"\\>\\</td\\>\\</form\\>";
      Printf.bprintf buf "\\<form style=\\\"margin: 0px;\\\" id=\\\"refresh\\\" name=\\\"refresh\\\"
        action=\\\"javascript:msgWindow.location.reload();\\\"\\>
        \\<td\\>\\<input style=\\\"font-family: verdana; font-size: 12px;\\\" type=submit
        Value=\\\"Refresh\\\"\\>\\</td\\>\\</form\\>\\</tr\\>\\</table\\>";
      Printf.bprintf buf "\\<table\\>\\<tr\\>\\<form style=\\\"margin: 0px;\\\" id=\\\"users\\\" name=\\\"users\\\"
        action=\\\"javascript:parent.output.location.href='submit?q=dcusers'\\\"\\>
        \\<td\\>\\<input style=\\\"font-family: verdana; font-size: 12px;\\\" type=submit
        Value=\\\"Users\\\"\\>\\</td\\>\\</form\\>";
      Printf.bprintf buf "\\<form style=\\\"margin: 0px;\\\" id=\\\"hubs\\\" name=\\\"hubs\\\"
        action=\\\"javascript:parent.output.location.href='submit?q=dchubs'\\\"\\>
        \\<td\\>\\<input style=\\\"font-family: verdana; font-size: 12px;\\\" type=submit
        Value=\\\"Hubs\\\"\\>\\</td\\>\\</form\\>"; 
      Printf.bprintf buf "\\<form style=\\\"margin: 0px;\\\" id=\\\"hubs\\\" name=\\\"hubs\\\"
        action=\\\"javascript:submitCmd()\\\"\\>
        \\<td\\>\\<input style=\\\"font-family: verdana; font-size: 12px;\\\" type=submit
        Value=\\\"Open chat\\\"\\>\\</td\\>\\</form\\>";
      Printf.bprintf buf "\\</tr\\>\\</table\\>";
      empty_string
    end else
      _s "Usage: dcmessages <username> | <serverip> <serverport>\n" 
  ), "<username> | <serverip> <serverport> : Show user or server messages ";


  (* message type      = (int * room_message) list
     room_message type =  | ServerMessage of string
                          | PublicMessage of int * string
                          | PrivateMessage of int * string *)
  (* 'dcsendmsg hub port user message' - send message to specific user  *)
  "dcsendmsg", Arg_multiple (fun args o ->
    let buf = o.conn_buf in
    (*let failtxt = "dcsendmsg <user> <message> | <serverip> <serverport> <message> : bad arguments" in*)
    let u = (* check if first argument is valid user *)
      (try
        let u = search_user_by_name (List.hd args) in
        Some u
      with _ -> None )
    in 
    let s =
      (try
         let s = Hashtbl.find servers_by_ip (List.hd args) in
         Some s
      with _ -> None )
    in
    (match u with 
    | Some u -> (* message is private usermessage *)
        (match args with
        | _ :: messages ->
            if not (has_my_nick u) && ((List.length u.user_servers) > 0) then begin
              let sent = ref false in
              List.iter (fun s -> (* find a server we are connected to with this user *)
                (match s.server_sock with
                | Connection sock -> 
                    if not !sent then begin
                      let msg = String2.unsplit messages ' 'in
                      dc_send_msg sock (
                        ToReq { To.dest = u.user_nick;
                                To.from = s.server_last_nick;
                                To.message = msg } );
                        sent := true;
                      u.user_messages <- u.user_messages @ [
                        (int_of_float (current_time ()), s.server_last_nick, PrivateMessage (0, msg))];
                    end
                | _ -> () )
              ) u.user_servers
            end
        | _ -> () )
    | None ->
        (match s with (* message is probably hub chatmessage but check ip *)
        | None -> if !verbose_unexpected_messages then lprintf_nl "dcsendmsg: No User or Server found"
        | Some s ->
            (match args with
            | _ :: _ :: messages ->
                (match s.server_sock with
                | Connection sock ->
                    let msg = String2.unsplit messages ' 'in
                    dc_send_msg sock ( MessageReq { Message.from = s.server_last_nick;  Message.message = msg } );
                    (* don't save this message, it is echoed from hub back to us and saved then *)
                    (* message window is refreshed too quickly to show this *)  
                | _ -> () )
            | _ -> () )
  )
    );
    Printf.bprintf buf "User query sent to hubs\n";
    empty_string
  ), "<user> <message> : Send message to user";

  (* Try to load file from filelist *)
  "dcloadfile", Arg_multiple (fun args o ->
    let buf = o.conn_buf in
    (match args with
    | [uname ; tth ; dir ; fname ; fsize] -> (* convert filenames back to normal *)
        if !verbose_download then lprintf_nl "dcloadfile: (%s) (%s) (%s)" dir fname tth; 
        Printf.bprintf buf "Trying to download file: %s from user: %s\n" fname uname;
        (try 
          let u = search_user_by_name uname in
          let user = o.conn_user.ui_user in
          let (_ : _ option) = start_new_download (Some u) tth (untranslate dir) (untranslate fname) (Int64.of_string fsize) user user.user_default_group in
          ()
        with _ -> if !verbose_download then lprintf_nl "dcloadfile: No user found" )
    | _ ->
        if !verbose_unexpected_messages then
          lprintf_nl "dcloadfile: bad arguments count (%d)" (List.length args) ); 
    empty_string
  ), "<username> <tth> <directory> <filename> : Load a file";

  (* load filelist from user *)
  "dcloadfilelist", Arg_one (fun args o ->
    let buf = o.conn_buf in
    (match args with
    | name ->
        (try
          let u = search_user_by_name name in
          if ((List.length u.user_servers) > 0) then begin
            if not (filelist_already_downloading u) then begin    (* and is connected to server with us *) 
              if !verbose_msg_clients || !verbose_download then
                lprintf_nl "Loading filelist from user %s" name;  (* not already loading filelist       *)
              let c = new_client () in
              c.client_name <- Some u.user_nick;
              add_client_to_user c u;
              c.client_state <- DcDownloadListWaiting;
              if (can_user_start_downloading u) then begin
                u.user_state <- TryingToSendFirstContact;
                c.client_state <- DcDownloadListConnecting (0,!!firewalled,current_time ()); (* level is set later *)
                ignore (DcClients.try_connect_client c);
              end
            end
          end 
        with _ ->
          if !verbose_unexpected_messages then lprintf_nl "dcloadfilelist: No user (%s) found" name) );
    Printf.bprintf buf "Trying to download filelist\n";
    empty_string
  ), "<name> : Download filelist from user";

  "dcfilelists", Arg_none (fun o -> show_dc_buttons o; dc_list o G_filelists "filelists"
  ), ": List all filelists on disk";

  "dcremclient", Arg_one (fun args o ->
    let buf = o.conn_buf in
    (match args with
    | num ->
        (try
          let cc = CommonClient.client_find (int_of_string num) in
          let impl = as_client_impl cc in
          let c = impl.impl_client_val in
          (match c.client_state with
          | DcDownloadWaiting _ | DcDownloadListWaiting ->
              Printf.bprintf buf "Removing one client by name %s" (clients_username c);
              remove_client c
          | _ -> () )
        with _ ->
            if !verbose_msg_clients then lprintf_nl "dcremclient: bad arguments (internal command)" ) ); 
    empty_string
  ), "<num> : Remove client by num";

  "dcfindsource", Arg_one (fun args o ->
    (match args with
    | tth_or_filename -> 
        (*lprintf_nl "Got dcfindsource command: (%s)" tth_or_filename;*)
        let tth_or_filename = untranslate' tth_or_filename in
        if (is_valid_tiger_hash tth_or_filename) then begin
          let query = QAnd (QHasField (Field_Type , "TTH") , (QHasWord tth_or_filename)) in
          let search = CommonSearch.new_search o.conn_user 
            (let module G = GuiTypes in
              { G.search_num = 0;
                G.search_query = query;
                G.search_max_hits = 10000;
                G.search_type = RemoteSearch;
                G.search_network = network.network_num;
              } ) 
          in 
          dc_with_connected_servers (fun s -> DcClients.server_send_search s search 9 tth_or_filename);  
          dc_last_manual_search := current_time ();
        end else begin
          let fname = Filename.basename tth_or_filename in
          let words = clean_string fname in
          let words_list = String2.split_simplify words ' ' in
          let rec add_query list =
            (match list with 
            | hd :: [] -> QHasWord hd
            | hd :: tail -> QAnd ((QHasWord hd) , (add_query tail)) 
            | [] -> failwith "No words to search")
          in
          let query = add_query words_list in
          let search = CommonSearch.new_search o.conn_user
            (let module G = GuiTypes in
              { G.search_num = 0;
                G.search_query = query;
                G.search_max_hits = 10000;
                G.search_type = RemoteSearch;
                G.search_network = network.network_num;
              } )
          in
          dc_with_connected_servers (fun s -> DcClients.server_send_search s search 1 tth_or_filename);  
          dc_last_manual_search := current_time ();
        end ); 
    empty_string
  ), ": Find new source for a file";

  "dcinfo", Arg_none (fun o ->
    show_dc_buttons o;
    let buf = o.conn_buf in
    let server_list =
      let lst = ref [] in
        List.iter (fun s ->
          let data = 
            shorten_string s.server_name 20 ^ "  (nick = " ^ s.server_last_nick ^ ")  (uptime = " ^
              (Date.time_to_string (int_of_float (current_time ()) -
                int_of_float (s.server_connection_time)) "verbose") ^
                (string_of_int (List.length s.server_users)) ^ ")"
          in
          lst := !lst @ [ (empty_string, data) ]
        ) !connected_servers;
        !lst
    in
    let norm_hubs,reg_hubs,opped_hubs = get_myhubs_info () in 
    html_mods_cntr_init ();
    if use_html_mods o then dc_info_html_header buf;
    let nservers = List.length !connected_servers in
    let hashed =
      [ Printf.sprintf "%d files" (Hashtbl.length dc_shared_files_by_hash) ] @
      match !dc_files_to_hash with [] -> [] | l -> [ Printf.sprintf "%d queued" (List.length l) ] @
      match !dc_tiger_computing with
      | Some dcsh when dcsh.dc_shared_size > 0L ->
        let progress = Int64.to_float dcsh.dc_shared_pos /. Int64.to_float dcsh.dc_shared_size *. 100. in
        [ Printf.sprintf "hashing: %s (%3.0f%%)" (Filename.basename dcsh.dc_shared_fullname) progress ]
      | _ -> []
    in
    let list = [
      ("Hub supports", (DcProtocol.Supports.create_supports_string (HubSupports mldonkey_dc_hub_supports)) );
      ("Client supports", (DcProtocol.Supports.create_supports_string (ClientSupports mldonkey_dc_client_supports)) );
      ("All/Open slots", Printf.sprintf "%d / %d" (open_slots ()) (current_slots ()) );
      ("Mode", (if !!firewalled then "Passive" else "Active") );
      ("Connected servers", (if nservers > 0 then string_of_int nservers else ""));
      ("  Server list:", empty_string ); ]      
      @ server_list @ [
      ("Hubs", (Printf.sprintf "Normal:%d  Vipped:%d  Opped:%d" norm_hubs reg_hubs opped_hubs) );
      ("Hashed", String.concat ", " hashed);
    ] in
    let counter = ref 0 in
    List.iter (fun (info,data) -> 
      dc_info_print info data line o;
      incr counter
    ) list;
    if use_html_mods o then Printf.bprintf buf "\\</table\\>\\</div\\>";
    empty_string
  ), ": Direct Connect info";

  (* load filelist from user *)
  "dcshowfilelist", Arg_one (fun args o ->
    show_dc_buttons o;
    let buf = o.conn_buf in
    (match args with
    | filename -> 
        let username, extension =  
          (try
            if (Filename.check_suffix filename mylist_ext) then         (* if extension is DcLst ...          *)
              Filename.chop_suffix filename mylist_ext,mylist_ext       (* return filename without it and ext *) 
            else if (Filename.check_suffix filename bz2_ext) then begin (* else if extension is bz2 ...       *)
              let filename = Filename.chop_suffix filename bz2_ext in   (* chop it off *)
              if (Filename.check_suffix filename xml_ext) then          (* check if there is extension xml ...*)
                Filename.chop_suffix filename xml_ext,mylistxmlbz2_ext  (* return filename without it and ext *)
              else raise Not_found                                      
            end else raise Not_found 
          with _ -> filename, empty_string) 
        in
        let spaces num =   (* add as many "...":s to string that counter num *)
          let s = ref "" in
          let rec iter num =
            if (num <> 0) then begin
              s := !s ^ "...";
              iter (num-1)
            end
          in iter num;
          !s
        in
        if (extension = mylist_ext) then begin               (* parse MyList.DcLst *)
          (try 
            let s = file_to_che3_to_string (Filename.concat filelist_directory filename) in
            if not (Charset.is_utf8 s) then lprintf_nl "not utf8 : %S" s;
            let s = Charset.Locale.to_utf8 s in (* really needed? *)
            let s = String2.replace_char s char13 '\n' in
            (try
              let lines = String2.split_simplify s '\n' in
              let mlist = ref ([] : dc_mylistnode list) in   (* root node of the MyList *)
              let tablist = ref [(-1, mlist)] in             (* list of previous open directory node for every tab *)
                                                             (* [(0 , list ref); (1 , list ref) ... *)
              let rec count_tabs line pos =                  (* count current lines tabs *)
                if line.[pos] <> '\t' then pos else
                count_tabs line (pos+1)
              in
              let add_dir name tabs list =
                let newlist = ref ([] : dc_mylistnode list) in
                list := !list @ [MylistDirectory (name, newlist)]; (* add this dir node to current list *)
                if (List.mem_assq tabs !tablist) then begin  (* check if a directory exists already for this tab *)
                  tablist := List.remove_assq tabs !tablist; (* remove existing previous tab *)
                end;
                tablist := !tablist @ [(tabs, newlist)];     (* add current list to this tab *)
              in
              let add_file name size list =
                list := !list @ [MylistFile (name, size)];   (*add this file to current node *)
              in
              let find_tab_dir tabs =                        (* find the node of last directory for this tab *)
                let nlist = List.assq tabs !tablist in
                nlist                                        (* return list ref *)
              in                   
              let rec parse lines ctabs clist =
                (match lines with 
                | first :: tail ->
                    let ltabs = count_tabs first 0 in        (* count lines tabs *)
                    (match (String2.split first '|') with
                    | dir :: [] ->                           (* if line is directory *)
                        let dir = String2.after dir ltabs in
                        if ltabs = ctabs then begin 
                          let nlist = find_tab_dir (ltabs-1) in
                          add_dir dir ltabs nlist;
                          if (tail <> []) then begin
                            let nlist = find_tab_dir ltabs in
                            parse tail ltabs nlist
                          end
                        end else if ltabs > ctabs then begin
                          add_dir dir ltabs clist;
                          let nlist = find_tab_dir ltabs in
                          if (tail <> []) then parse tail ltabs nlist
                        end else begin 
                          let nlist = find_tab_dir ltabs in
                          parse lines ltabs nlist
                        end
                    | name :: size :: [] ->                  (* if line is file *)
                        let name = String2.after name ltabs in
                        if ltabs > ctabs then begin
                          add_file name size clist;
                          if (tail <> []) then parse tail ctabs clist
                        end else if ltabs <= ctabs then begin
                          let nlist = find_tab_dir (ltabs-1) in
                          add_file name size nlist;
                          parse tail (ltabs-1) nlist
                        end
                    | _ -> failwith (Printf.sprintf "Unknown line (%s)" first ) )
                | [] -> () )
              in
              parse lines (pred 0) mlist; 
              
              html_mods_cntr_init ();
              let line = ref 0 in
              if use_html_mods o then dc_filelist_files_print_html_header buf;
               let rec print node dir tabs =
                incr line;
                (match node with 
                | MylistFile (name, size) ->
                    filelist_file_print true (spaces tabs) username dir name size empty_string !line o;
                    (*lprintf_nl "(%s) (%s)" (!spaces ^ name) (Int64.to_string size);*)
                | MylistDirectory (name, nlist) -> 
                    let dir =
                      if dir = "" then name 
                      else dir ^ "/" ^ name
                    in
                    filelist_file_print false (spaces tabs) username dir name empty_string empty_string !line o; 
                    (*lprintf_nl "(%s) list_count=(%d)" (!spaces ^ dir) (List.length !nlist);*)
                
                    List.iter (fun node -> print node dir (tabs+1)) !nlist )
              in
              List.iter (fun node -> print node empty_string 0) !mlist;
              if use_html_mods o then Printf.bprintf buf "\\</table\\>\\</div\\>";   
            with e ->
                if !verbose_unexpected_messages then
                  lprintf_nl "dcshowfilelist: (%s) in .DcLst parsing" (Printexc2.to_string e) )
          with _ -> if !verbose_unexpected_messages then lprintf_nl "Error in che3 decompressing" )

        end else if (extension = mylistxmlbz2_ext) then begin (* parse .xml.bz2 *)
          (try                                                (* try to unzip *)
            let s = Buffer.contents (file_to_bz2_to_buffer (Filename.concat filelist_directory filename)) in
             (try                                             (* try to parse xml and make a www page *)
              let xml = Xml.parse_string s in
              let parse_xml_chars s =                         (* in xml there are html-escapes mixed in *)  
                let s = dc_replace_str_to_str s "&amp;" "&" in
                s
              in
              if ((Xml.tag xml) <> "FileListing") then failwith "Xml-file don't start with FileListing";
              html_mods_cntr_init (); 
              let line = ref 1 in
              if use_html_mods o then dc_filelist_files_print_html_header buf;
              let rec parse x dir ndirs =   (* iterate this with all xml elements (one node with tag and attributes *) 
                let tag = Xml.tag x in
                let dir = 
                  if tag = "File" then dir  (* if tag of element is file, return existing dir name *)
                  else begin                (* else this is "probably" a directory *)
                    let newdir = Xml.attrib x "Name" in (* get dir name *)
                    if dir = "" then newdir (* if this is first dir to add, don't add the "/" *)
                    else dir ^ "/" ^ newdir (* else add this new dir path name to existing dir *)
                  end
                in 
                (match tag with
                | "File" ->                 (* is this xml element is a file element, add its line to http-page *)
                    let fname = Xml.attrib x "Name" in
                    let fname = parse_xml_chars fname in 
                    let fsize = Xml.attrib x "Size" in
                    let ftth = Xml.attrib x "TTH" in
                    let dir = dir in
                    filelist_file_print true (spaces ndirs) username dir fname fsize ftth !line o; 
                | "Directory" ->            (* or if it is a directory element, add its line to http-page *)
                    let fname = Xml.attrib x "Name" in
                    let fname = parse_xml_chars fname in 
                    let fsize = empty_string in
                    let ftth = empty_string in
                    let dir = dir in
                    filelist_file_print false (spaces ndirs) username dir fname fsize ftth !line o;
                | _ -> failwith "Tag not File or Directory" );
                incr line;
                Xml.iter (fun x -> parse x dir (ndirs+1)) x
              in
              Xml.iter (fun x -> parse x empty_string 0) xml;
              if use_html_mods o then Printf.bprintf buf "\\</table\\>\\</div\\>";
            with 
            | Error e -> if !verbose_unexpected_messages then lprintf_nl "%s" (Xml.error e)
            | e -> if !verbose_unexpected_messages then
                lprintf_nl "dcshowfilelist: (%s) in xml parsing" (Printexc2.to_string e) )
          with _ -> if !verbose_unexpected_messages then lprintf_nl "Error in bz2 unzipping" )
        end else if !verbose_unexpected_messages then lprintf_nl "dcshowfilelist: Filelist extension not valid" );
    empty_string
  ), "<name> : Show filelist for user";

  "dcautoconnect", Arg_two (fun arg1 arg2 o ->
    show_dc_buttons o;
    try
      let s = Hashtbl.find servers_by_ip arg2 in
      let auto = bool_of_string arg1 in
      s.server_autoconnect <- auto;
      server_must_update s;
      "ok"
    with exn -> Printf.sprintf "Failed : %s" (Printexc2.to_string exn)
  ), "<true/false> <ip> : Set/unset the server autoconnection state";

  ] (* end of   let commands = *)

module P = GuiTypes

(* register user operations *)
let _ = 
  register_commands commands;

  user_ops.op_user_info <- (fun user -> (* CHECK *)
      {
        P.user_num = user.user_user.impl_user_num;
        P.user_md4 = Md4.null;
        P.user_name = user.user_nick;
        P.user_ip = Ip.null;
        P.user_port = 0;
      P.user_tags = 
        (let list =
          if user.user_data > 1. then 
            [ { tag_name =  Field_UNKNOWN "link"; tag_value = String user.user_link };
              { tag_name =  Field_UNKNOWN "shared"; tag_value = String (
                  Printf.sprintf "%12.0f" user.user_data) } ]
          else []
        in
        (match user.user_type with
        | Normal | Vip -> list
        | Op -> { tag_name =  Field_UNKNOWN "admin"; tag_value = String "admin" } :: list ) );
      P.user_server = 
        (match user.user_servers with
        | [] -> 0
        | s :: _ -> s.server_server.impl_server_num );
    }
        );
  user_ops.op_user_remove <- (fun user -> () )
(*
  user_ops.op_user_browse_files <- (fun user ->
      let c = client_of_user user in
      contact_add (as_client c.client_client)
  );
  user_ops.op_user_set_friend <- (fun user ->
      let c = client_of_user user in
      friend_add (as_client c.client_client)
  )
            
    mutable op_user_network : network;
    mutable op_user_set_friend : ('a -> unit);
    mutable op_user_browse_files : ('a -> unit);
        

*)

(*module C = CommonTypes
let _ =
  result_ops.op_result_info <- (fun r ->
      {
        C.result_num = r.result_result.impl_result_num;    
        C.result_network = network.network_num;
        
        C.result_names = [r.result_name];
        C.result_md4 = Md4.null;
        C.result_size = r.result_size;
        C.result_format = result_format_of_name r.result_name;
        C.result_type = result_media_of_name r.result_name;
        C.result_tags = [];
        C.result_comment = "";
        C.result_done = false;
      }
  )
  *)


(* register file operations *)
let _ =
  file_ops.op_file_info <- (fun file ->
      {
        P.file_fields = Fields_file_info.all;
        P.file_comment = empty_string;
        P.file_name = file.file_name;
        P.file_num = (file_num file);
        P.file_network = network.network_num;
        P.file_names = [file.file_name];
        P.file_md4 = Md4.null;
        P.file_size = file_size file;
        P.file_downloaded = file_downloaded file;
        P.file_all_sources = 0;
        P.file_active_sources = 0;
        P.file_state = file_state file;
        P.file_sources = None;
        P.file_download_rate = file_download_rate file.file_file;
        P.file_chunks = None;
        P.file_chunk_size = None;
        P.file_availability = [network.network_num, "0"];
        P.file_format = FormatUnknown;
        P.file_chunks_age = [|0|];
        P.file_age = file_age file;
        P.file_last_seen = BasicSocket.last_time ();
        P.file_priority = file_priority (as_file file.file_file);
        P.file_uids = [];
        P.file_sub_files = [];
        P.file_comments = [];
        P.file_magic = None;
        P.file_user = empty_string;
        P.file_group = empty_string;
        P.file_release = false;
      }    
  );
  file_ops.op_file_all_sources <- (fun file ->
      List2.tail_map (fun c -> as_client c.client_client)
      file.file_clients
  );
  file_ops.op_file_active_sources <- file_ops.op_file_all_sources;
  file_ops.op_file_cancel <- (fun file -> 
    let remove_files_clients_not_downloading () =
      List.iter (fun c ->                       
        (match c.client_state with              
        | DcDownload f -> ()                            (* only one client should be in this state *)
        | _ ->
            remove_client c )
      ) file.file_clients;
    in
    (try
      List.iter (fun c ->                               (* find one files client that is currently downloading *)
        (match c.client_state with
        | DcDownload _ -> raise (Found_client c)
        | _ -> () )
      ) file.file_clients;
      raise Not_found
    with
    | Found_client c ->                                 (* found a download slot tried to continue *)
        (match (DcClients.find_next_client c) with      (* try to continue slot *)
        | Some cl ->
            (match c.client_sock with
            | Connection sock ->
                remove_files_clients_not_downloading ();
                remove_file_not_clients file;
                c.client_receiving <- Int64.zero; 
                c.client_pos <- Int64.zero;
                TcpBufferedSocket.set_rtimeout sock 30.;
                DcClients.next_download false c sock cl (* try to change downloading *)
            | _ -> () )
        | None ->
            remove_files_clients_not_downloading ();
            remove_file_not_clients file;
            c.client_state <- DcIdle;
            (match c.client_sock with
            | Connection sock -> TcpBufferedSocket.close sock (Closed_for_error "File cancelled")
            | _ -> () ) 
        )
    | Not_found ->
        remove_file_with_clients file );
    if !verbose_download then lprintf_nl "File %s cancelled" file.file_name;
  );
  file_ops.op_file_commit <- (fun file name -> 
    remove_file_with_clients file;
  );
  file_ops.op_file_pause <- (fun file -> 
    (try
      List.iter (fun c ->
        (match c.client_state with
        | DcDownload f -> if file == f then raise (Found_client c)
        | _ -> () )
      ) file.file_clients;
      raise Not_found
    with
    | Found_client c ->
        (match (DcClients.find_next_client c) with     (* try to continue slot *)
        | Some cl ->
            (match c.client_sock with
            | Connection sock -> 
                DcClients.next_download true c sock cl (* try to change downloading *)
            | _ -> () )
        | None -> c.client_state <- DcIdle (* DcPaused ? *))
    | Not_found -> () ) (* Should not happen *)
  );

  (*
  file_ops.op_file_files <- (fun file impl -> 
      match file.file_swarmer with
        None -> [CommonFile.as_file impl]
      | Some swarmer ->
          CommonSwarming.subfiles swarmer)
; *)
  (*
  file_ops.op_file_save_as <- (fun file new_name  ->
      match file_state file with
        FileDownloaded | FileShared ->
          DcClients.save_file_as file new_name
      | _ -> ()
);
  *)

(*let client_of_user user =
  let c = new_client user.user_nick in
  c
  
*)
  
(*
    mutable op_file_network : network; *)
    file_ops.op_file_save_as <- (fun _ _ -> ());
    file_ops.op_file_resume <- (fun _ -> ());
    file_ops.op_file_set_format <- (fun _ _ -> ());
    file_ops.op_file_check <- (fun _ -> ());
    file_ops.op_file_recover <- (fun _ -> ());
    file_ops.op_file_print <- (fun file o ->
      let buf = o.conn_buf in
      if use_html_mods o then 
      begin
        let td l =
          Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr ());
          html_mods_td buf l
        in
        td [
          ("Directory", "sr br", "Directory");
          ("", "sr", file.file_directory) ];
        td [
          ("Filename", "sr br", "Filename");
          ("", "sr", file.file_name) ];
        td [
          ("Tiger tree hash and magnet url", "sr", "TTH and magnet");
          ("", "sr", html_show_file file) ];
        td [
          ("Automatic TTH searches performed", "sr", "Autosearches");
          ("", "sr", string_of_int file.file_autosearch_count) ];
      end
      else
        ())
    (*file_ops.op_file_print_html <- (fun _ _ -> lprintf_nl "Received (op_file_print_html)"; ());*)
    (*file_ops.op_file_print_sources_html <- (fun _ _ -> lprintf_nl "Received (op_file_print_sources_html)"; ())*)
(*    mutable op_file_files : ('a -> 'a file_impl -> file list);
    mutable op_file_debug : ('a -> string);
    mutable op_file_proposed_filenames : ('a -> string list);
*)

let _ =
  CommonWeb.add_web_kind "hublist" "DirectConnect hublist"
    (fun url filename ->
      if !!enable_directconnect then
        begin
          try
            dc_hublist := (
              match List.rev (String2.split filename '.') with
              | "bz2"::"xml"::_ ->
                Unix2.with_remove (Misc2.bz2_extract filename) (fun filename ->
                  DcServers.make_hublist_from_xml (Xml.parse_file filename))
              | "xml"::_ -> DcServers.make_hublist_from_xml (Xml.parse_file filename)
              | "bz2"::_ -> Unix2.with_remove (Misc2.bz2_extract filename) DcServers.make_hublist_from_file
              | _ -> DcServers.make_hublist_from_file filename);
            lprintf_nl "Loaded DC hublist, %d entries" (List.length !dc_hublist)
          with e -> 
            lprintf_nl "Exception while parsing hublist from %S : %s" url (Printexc2.to_string e);
            raise Not_found
        end
      else
        begin
          lprintf_nl "DirectConnect module is disabled, ignoring...";
          raise Not_found
        end
    )
