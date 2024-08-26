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

open Md4

open Printf2
open CommonClient
open CommonShared
open CommonServer
open CommonResult
open CommonFile
open CommonComplexOptions
open CommonInteractive
open GuiTypes
open CommonNetwork
open Options
open BasicSocket
open CommonTypes
open CommonGlobals
open CommonOptions
open CommonUserDb
open Int64ops

module VB = VerificationBitmap

let log_prefix = "[dIve]"

let lprintf_nl fmt =
  lprintf_nl2 log_prefix fmt

let lprintf_n fmt =
  lprintf2 log_prefix fmt

let verify_user_admin () =
  let warning =
    "SECURITY WARNING: user admin has an empty password, use command: useradd admin password\n"
  in
  if has_empty_password (admin_user ()) && !!allowed_ips <>
    [(Ip.range_of_string (strings_of_option allowed_ips).option_default)] then
    begin
      lprintf_n "%s" warning;
      warning
    end
  else ""
  
let check_supported_os () =
  let uname = Unix32.uname () in
  let message = Printf.sprintf "MLDonkey is not able to run faultless under %s" uname; in
  if uname <> "" && not (Unix32.os_supported ()) then begin
    lprintf_nl "%s" message;
    message;
  end
  else ""

let dns_works = ref false
  
let real_startup_message () =
  let s =
  !startup_message ^ (verify_user_admin ()) ^ (check_supported_os ()) 
  ^ (if not !dns_works then "DNS resolution does not work\n" else "")
  ^ (if not !Charset.Locale.conversion_enabled then "Charset conversion does not work, disabled\n" else "")
  ^ (match !created_new_base_directory with
       None -> ""
     | Some dir -> (Printf.sprintf "MLDonkey created a new home directory in %s\n" dir))
  ^ (if not !allow_saving_ini_files then "Base directory is full, ini file saving disabled\n" else "")
  ^ (if !all_temp_queued then "Temp directory is full, all downloads are queued\n" else "")
  ^ (if !hdd_full_log_closed then "Logfile directory is full, logging redirected to RAM\n" else "")
  ^ (if Autoconf.donkey = "yes" && not !!enable_servers && !!enable_donkey then
     "You disabled option enable_servers, you will not be able to connect to ED2K servers\n"
     else "")
  in
  if s = "" then None else Some s

let hdd_check () =
  let dir_full dir mb =
    match Unix32.diskfree dir with
    | None -> false
    | Some v when ((Unix32.filesystem dir = "NFS_SUPER_MAGIC") && v = zero) -> false
    | Some v -> v < megabytes mb
  in

  if dir_full !!temp_directory !!hdd_temp_minfree then
    if !!hdd_temp_stop_core then begin
      send_dirfull_warning !!temp_directory true "MLDonkey shuts down";
      CommonInteractive.clean_exit 0
      end
    else begin
      send_dirfull_warning !!temp_directory true "MLDonkey queues all downloads";
      all_temp_queued := true
    end
  else
    begin
      all_temp_queued := false;
      try
        ignore (Hashtbl.find last_sent_dir_warning !!temp_directory);
        (try Hashtbl.remove last_sent_dir_warning !!temp_directory with Not_found -> ());
        send_dirfull_warning !!temp_directory false "MLDonkey unqueues all downloads"
      with Not_found -> ()
    end;

  let core_dir = Sys.getcwd () in
  if dir_full core_dir !!hdd_coredir_minfree then
    if !!hdd_coredir_stop_core then begin
      send_dirfull_warning core_dir true "MLDonkey shuts down";
      CommonInteractive.clean_exit 0
      end
    else
      begin
        allow_saving_ini_files := false;
        send_dirfull_warning core_dir true "MLDonkey base directory partition full, stop saving ini files"
      end
  else
    begin
      allow_saving_ini_files := true;
      try
        ignore (Hashtbl.find last_sent_dir_warning core_dir);
        (try Hashtbl.remove last_sent_dir_warning core_dir with Not_found -> ());
        send_dirfull_warning core_dir false "MLDonkey base directory partition has enough free space again, saving ini files again"
      with Not_found -> ()
    end;

  if !!log_file <> "" && (not (keep_console_output ())) then begin
  let log_dir = Filename.dirname !!log_file in
  if dir_full log_dir !!hdd_coredir_minfree then
    begin
      hdd_full_log_closed := true;
      send_dirfull_warning log_dir true "MLDonkey logdirectory partition full, redirect log to RAM";
      close_log ()
    end
  else
    begin
      if !hdd_full_log_closed then log_file =:= !!log_file;
      hdd_full_log_closed := false;
      try
        ignore (Hashtbl.find last_sent_dir_warning log_dir);
        (try Hashtbl.remove last_sent_dir_warning log_dir with Not_found -> ());
        send_dirfull_warning log_dir false "MLDonkey logdirectory partition has enough free space again, re-enabling logging"
      with Not_found -> ()
    end
  end

(* ripped from gui_downloads *)

let calc_file_eta f =
  let size = Int64.to_float f.file_size in
  let downloaded = Int64.to_float f.file_downloaded in
  let missing = size -. downloaded in
  let rate = f.file_download_rate in
  let hundays = 1000.0 *. 60.0 *. 60.0 *. 24.0 in
  match f.file_state with
    FilePaused | FileQueued -> int_of_float (hundays +. 2.)
  | _ -> (
        if rate < 12. then int_of_float (hundays +. 1.) else
  begin
  let rate =
    if rate < 0.0001
    then
      let time = BasicSocket.last_time () in
      let age = time - f.file_age in
        if age > 0
        then downloaded /. (float_of_int age)
        else 0.
    else rate
  in
  let eta =
    if rate < 11.
    then hundays
    else missing /. rate
  in
  int_of_float eta
  end
  )


let file_availability f =
  match f.file_availability with
    (_,avail) :: _ ->
      (match f.file_chunks with
      | None -> 0.
      | Some chunks ->
          let rec loop i p n =
            if i < 0
            then
              if n < 0.0001
              then 0.0
              else (p /. n *. 100.0)
            else
              if partial_chunk (VB.get chunks i)
              then
                if avail.[i] <> (char_of_int 0)
                then loop (i - 1) (p +. 1.0) (n +. 1.0)
                else loop (i - 1) p (n +. 1.0)
              else loop (i - 1) p n
          in
          loop ((String.length avail) - 1) 0.0 0.0)
  | _ -> 0.0

let string_availability s =
  match s with
    (_,s) :: _ ->

      let len = String.length s in
      let p = ref 0 in
      for i = 0 to len - 1 do
        if int_of_char s.[i] <> 0 then begin
            incr p
          end
      done;
      if len = 0 then 0.0 else
        (float_of_int !p /. float_of_int len *. 100.)
  | _ -> 0.0

let get_file_availability f =
if !!html_mods_use_relative_availability
        then file_availability f
        else string_availability f.file_availability

let number_of_comments f =
  List.length f.file_comments

(* WARNING: these computations are much more expensive as they seem.
We use the ShortLazy to avoid recomputing the result too many times,
in particular when sorting the files depending on their number of sources...

2004/06/18: file.file_all_sources is used when not zero, and in this case,
  also file.file_active_sources.
  *)

let number_of_sources gf =
  List.length (file_all_sources (file_find gf.file_num))

let number_of_sources gf =
  if gf.file_all_sources > 0 then
    gf.file_all_sources
  else
  ShortLazy.compute ("number_of_sources", gf.file_num, 0)
  number_of_sources gf

let number_of_active_sources gf =
  let nasrcs = ref 0 in
  List.iter (fun fsrc ->
    match (client_state fsrc) with
        Connected_downloading _ -> incr nasrcs
      | _ -> ()
  ) (file_active_sources (file_find gf.file_num));
  !nasrcs

let number_of_active_sources gf =
  if gf.file_all_sources > 0 then
    gf.file_active_sources
  else begin
      ShortLazy.compute ("number_of_active_sources", gf.file_num, 0)
      number_of_active_sources gf
    end

let net_name gf =
        let n = network_find_by_num gf.file_network in
        n.network_name

let short_net_name gf =
    let nn = net_name gf in
        match nn with
   | "OpenNapster" -> "N"
   | "Direct Connect" -> "C"
   | "FileTP" -> "T"
   | _ -> String.sub nn 0 1


module Html = struct
    let begin_td buf = Printf.bprintf buf "\\<td\\>"
    let begin_td_option buf option= Printf.bprintf buf "\\<td %s\\>" option
    let end_td buf = Printf.bprintf buf "\\</td\\>"
    let begin_table buf = Printf.bprintf buf "\\<table\\>"
    let begin_table_option buf option = Printf.bprintf buf "\\<table %s\\>" option
    let end_table buf = Printf.bprintf buf "\\</table\\>"
    let begin_tr buf =  Printf.bprintf buf "\\<tr\\>"
    let end_tr buf =  Printf.bprintf buf "\\</tr\\>"

    let button buf value onclick =
      Printf.bprintf buf "
      \\<input type=\\\"button\\\" value=\\\"%s\\\" onclick=\\\"%s\\\"\\>"
      value onclick
  end

let initialization_completed = ref false

let save_config () =
  (try Unix32.flush () with e ->
        Printf2.lprintf "Exception %s while flushing\n" (Printexc2.to_string e)
  );
  if !initialization_completed then (
    if !allow_saving_ini_files then begin
      Options.save_with_help downloads_ini;
      Options.save_with_help_private users_ini;
      CommonComplexOptions.save ();
      CommonUploads.save ();
      CommonStats.save ();
      networks_iter_all (fun r ->
          List.iter (fun opfile ->
              Options.save_with_help opfile
          ) r.network_config_file);
    end
    ) else (
      Printf2.lprintf "Initialization not completed, bypassing state saving\n"
    );
  ()

let age_to_day date =
  (last_time () - date) / Date.day_in_secs


let percent file =
  let downloaded = Int64.to_float file.file_downloaded in
  let size = Int64.to_float file.file_size in
  if size < 1.0
  then 0.0
  else (downloaded *. 100.) /. size

let short_name file =
  shorten file.file_name !!max_name_len

type table_align =
  Align_Left
| Align_Right
| Align_Center

let col_sep = " "
let add buf s align max_len =
  let slen = try Charset.utf8_length s with e -> String.length s in
  let diff = max_len - slen in
  match align with
    Align_Center ->
      let left = diff / 2 in
      let right = diff - left in
      Printf.bprintf buf "%s%s%s"
        (String.make left ' ') s (String.make right ' ')
  | Align_Right ->
      Printf.bprintf buf "%s%s" (String.make diff ' ') s
  | Align_Left ->
      Printf.bprintf buf "%s%s" s (String.make diff ' ')

let print_table_text buf alignments titles lines =
  let max_cols = ref (max (Array.length titles) (Array.length alignments)) in
  List.iter (fun line ->
      let len = Array.length line in
      if len > !max_cols then max_cols := len
  ) lines;
  let ncols = !max_cols in
  let cols = Array.make ncols 0 in
  List.iter (fun line ->
      let len = Array.length line in
      for i = 0 to len-1 do
        let slen = try Charset.utf8_length line.(i) with e -> String.length line.(i) in
        if cols.(i) <  slen then cols.(i) <- slen
      done;
  ) (titles :: lines);
  Array.iteri (fun i s ->
      add buf s Align_Center cols.(i);
      Buffer.add_string buf col_sep;
  ) titles;
  Buffer.add_char buf '\n';
  let aligns = Array.make ncols Align_Center in
  Array.iteri (fun i al -> aligns.(i) <- al) alignments;
  List.iter (fun line ->
      let len = Array.length line in
      Array.iteri (fun i s ->
          add buf s aligns.(i) cols.(i);
          if i+1 < len then  Buffer.add_string buf col_sep;
      ) line;
      Buffer.add_char buf '\n';
  ) lines


let print_table_html_mods buf lines =

  html_mods_cntr_init ();

  List.iter (fun line ->
      Printf.bprintf buf "\\<tr class=\\\"dl-%d\\\"" (html_mods_cntr ());
      Array.iter (fun data ->
          Printf.bprintf buf "%s" data;
      ) line;
      Html.end_tr buf;
  ) lines;
  Html.end_table buf;
  Html.end_td buf;
  Html.end_tr buf;
  Html.end_table buf;
  Printf.bprintf buf "\\</div\\>"



let print_table_html spacing buf aligns titles lines =
  Html.begin_table buf;

  Html.begin_tr buf;
  Array.iter (fun title ->
      Printf.bprintf buf "\\<td align=center\\>%s\\</td\\>" title;
      Printf.bprintf buf "\\<td width=%d\\> \\</td\\>" spacing;
  ) titles;
  let naligns = Array.length aligns in
  Html.end_tr buf;

  List.iter (fun line ->
      Html.begin_tr buf;
      Array.iteri (fun i title ->
          Printf.bprintf buf "\\<td%s nowrap\\>%s\\</td\\>"
            (if i >= naligns then "" else
            match aligns.(i) with
              Align_Center -> " align=center"
            | Align_Left -> " align=left"
            | Align_Right -> " align=right")
          title;
          Printf.bprintf buf "\\<td width=%d\\> \\</td\\>" spacing;
      ) line;
      Html.end_tr buf;
  ) lines;
  Html.end_table buf


let downloading file =
  match file.file_state with
  | FileDownloading | FileQueued -> true
  | _ -> false

let stalled file =
  match file.file_state with
  | FilePaused | FileQueued -> true
  | _ -> false


let print_file_html_form buf files =


  Printf.bprintf buf "
\\<script language=JavaScript\\>\\<!--
function pauseAll(x){for(i=0;i\\<document.selectForm.elements.length;i++){var j=document.selectForm.elements[i];if (j.name==\\\"pause\\\") {j.checked=x;}}}
function resumeAll(x){for(i=0;i\\<document.selectForm.elements.length;i++){var j=document.selectForm.elements[i];if (j.name==\\\"resume\\\") {j.checked=x;}}}
function cancelAll(x){for(i=0;i\\<document.selectForm.elements.length;i++){var j=document.selectForm.elements[i];if (j.name==\\\"cancel\\\") {j.checked=x;}}}
  function clearAll(x){for(i=0;i\\<document.selectForm.elements.length;i++){var j=document.selectForm.elements[i];if (j.type==\\\"checkbox\\\") {j.checked=x;}}}//--\\>\\</script\\>
  ";

  Printf.bprintf buf "\\<form name=selectForm action=\\\"files\\\"\\>";


  Html.begin_table_option  buf "width=100%";

  Html.begin_td_option buf "width=50%";
  Printf.bprintf buf "\\<input type=submit value='Submit changes'\\>";
  Html.end_td buf;

  Html.begin_td_option buf "width=50%";
  Html.end_td buf;

  Html.begin_td buf;
  Html.button buf "Pause all" "pauseAll(true);";
  Html.end_td buf;

  Html.begin_td buf;
  Html.button buf "Resume all" "resumeAll(true);";
  Html.end_td buf;

  Html.begin_td buf;
  Html.button buf "Cancel all" "cancelAll(true);";
  Html.end_td buf;

  Html.begin_td buf;
  Html.button buf "Clear all" "clearAll(false);";
  Html.end_td buf;

  Html.end_table buf;

  print_table_html 10 buf
    [| Align_Left; Align_Left; Align_Left; Align_Right; Align_Right; Align_Right; Align_Right; Align_Right|]
    [|
    "[ Num ]";
    "P/R/C";
    "\\<input type=radio value=File name=sortby\\> File";
    "\\<input type=radio value=Percent name=sortby\\> Percent";
    "\\<input type=radio value=Downloaded name=sortby\\> Downloaded";
    "\\<input type=radio value=Size name=sortby\\> Size";
    "Old";
    "\\<input type=radio value=Rate name=sortby\\> Rate";
    "\\<input type=radio value=Priority name=sortby\\> Priority";
  |]
    (List.map (fun file ->
        [|
          (Printf.sprintf "[\\<a href=\\\"submit\\?q\\=vd+%d\\\"\\>%-5d\\</a\\> %s]"
              file.file_num
              file.file_num
            (net_name file)
          );

          (if downloading file then
              Printf.sprintf
                "\\<input name=pause type=checkbox value=%d\\> R
                \\<input name=cancel type=checkbox value=%d\\>"
                file.file_num
                file.file_num
            else
              Printf.sprintf
                "P
              \\<input name=resume type=checkbox value=%d\\>
                \\<input name=cancel type=checkbox value=%d\\>"
                file.file_num
                file.file_num);

          ( let size = Int64.to_float file.file_size in
            let downloaded = Int64.to_float file.file_downloaded in
            let size = if size < 1. then 1. else size in
            Printf.sprintf "%s \\<br\\>
\\<table cellpadding=0 cellspacing=0 width=100%%\\>\\<tr\\>
\\<td class=\\\"loaded\\\" style=\\\"height:%dpx\\\" width=\\\"%d%%\\\"\\> \\</td\\>
\\<td class=\\\"remain\\\" style=\\\"height:%dpx\\\" width=\\\"%d%%\\\"\\> \\</td\\>
\\</tr\\>\\</table\\>"
            (short_name file)
            (!!html_vd_barheight)
            (truncate (downloaded /. size *. 100.))
            (!!html_vd_barheight)
            (truncate ( (1. -. downloaded /. size) *. 100.))
          );              	

          (Printf.sprintf "%5.1f" (percent file));
          (Int64.to_string file.file_downloaded);
          (Int64.to_string file.file_size);
          (Printf.sprintf "%d:%s"
              (age_to_day file.file_age)
            (
              let len = Array.length file.file_chunks_age in
              if len = 0 then "-" else
              let min = ref (last_time ()) in
              for i = 0 to len - 1 do
                if file.file_chunks_age.(i) < !min then
                  min := file.file_chunks_age.(i)
              done;
              if !min = 0 then "-" else
                string_of_int (age_to_day !min)));

          (match file.file_state with
            | FileQueued ->  "Queued"
            | FilePaused ->  "Paused"
            | FileAborted s -> Printf.sprintf "Aborted %s" s
            | _ ->
            if file.file_download_rate < 10.24 then
              "-"
            else
              Printf.sprintf "%5.1f" (file.file_download_rate /. 1024.));
          (Printf.sprintf "%3d" file.file_priority);
        |]
    ) files);
  Printf.bprintf buf "\\</form\\>"


let print_file_html_mods buf guifiles =

    if (List.length guifiles) > 0 then begin
        let tsize = ref Int64.zero in
        let tdl = ref Int64.zero in
        let trate = ref 0.0 in
        let qsize = ref Int64.zero in
        let qdl = ref Int64.zero in
        let qnum = ref 0 in

        List.iter (fun file ->
         tsize := !tsize ++ file.file_size;
     tdl := !tdl ++ file.file_downloaded;
         trate := !trate +. file.file_download_rate;

         if file.file_state = FileQueued then begin
                qsize := !qsize ++ file.file_size;
                qdl := !qdl ++ file.file_downloaded;
                incr qnum;
         end;

        ) guifiles;

  Printf.bprintf buf "\\</pre\\>
\\<script language=JavaScript\\>\\<!--
function pauseAll(x){for(i=0;i\\<document.selectForm.elements.length;i++){var j=document.selectForm.elements[i];if (j.name==\\\"pause\\\") {j.checked=x;}}}
function resumeAll(x){for(i=0;i\\<document.selectForm.elements.length;i++){var j=document.selectForm.elements[i];if (j.name==\\\"resume\\\") {j.checked=x;}}}
function cancelAll(x){for(i=0;i\\<document.selectForm.elements.length;i++){var j=document.selectForm.elements[i];if (j.name==\\\"cancel\\\") {j.checked=x;}}}
function clearAll(x){for(i=0;i\\<document.selectForm.elements.length;i++){var j=document.selectForm.elements[i];if (j.type==\\\"checkbox\\\") {j.checked=x;}}}
function submitPriority(num,cp,sel) {
        // 2 line workaround for mozilla mouseout bug:
        var row = sel.parentNode.parentNode.parentNode;
        row.className=mOvrClass;
        var divID = document.getElementById(\\\"divSelectPriority\\\" + num);
        var selectID = document.getElementById(\\\"selectPriority\\\" + num);
        var params='';
        if (selectID.value.length \\> 0) {params = '+'+selectID.value+'+'+num;}
        var np = selectID.value;
        if (np.charAt(0) == \\\"=\\\") {var p = parseInt(np.substring(1,99));}
        else {var p = parseInt(cp) + parseInt(selectID.value);}
        var str='\\<select id=\\\"selectPriority' + num + '\\\" style=\\\"font-size: 8px; font-family: verdana\\\" onchange=\\\"javascript:submitPriority(' + num + ',' + p + ',this)\\\"\\>';
        if (p != 20 \\&\\& p != 10 \\&\\& p != 0 \\&\\& p != -10 \\&\\& p != -20) { str += '\\<OPTION value=\\\"=' + p + '\\\" SELECTED\\>' + p; }
        str += '\\<option value=\\\"=20\\\"'; if (p==20) {str += \\\" SELECTED\\\"}; str += '\\>Very High';
        str += '\\<option value=\\\"=10\\\"'; if (p==10) {str += \\\" SELECTED\\\"}; str += '\\>High';
        str += '\\<option value=\\\"=0\\\"'; if (p==0) {str += \\\" SELECTED\\\"}; str += '\\>Normal';
        str += '\\<option value=\\\"=-10\\\"'; if (p==-10) {str += \\\" SELECTED\\\"}; str += '\\>Low';
        str += '\\<option value=\\\"=-20\\\"'; if (p==-20) {str += \\\" SELECTED\\\"}; str += '\\>Very Low';
        str += '\\<option value=\\\"10\\\"\\>+10';
        str += '\\<option value=\\\"5\\\"\\>+5';
        str += '\\<option value=\\\"1\\\"\\>+1';
        str += '\\<option value=\\\"-1\\\"\\>-1';
        str += '\\<option value=\\\"-10\\\"\\>-10';
        str += \\\"\\</select\\>\\\";
        divID.innerHTML = str;
        parent.fstatus.location.href='submit?q=priority' + params;
}
//--\\>\\</script\\>

\\<div class=main\\>";

if !!html_mods_use_js_tooltips then Printf.bprintf buf 
"\\<div id=\\\"object1\\\" style=\\\"position:absolute; background-color:#FFFFDD;color:black;border-color:black;border-width:20px;font-size:8pt; visibility:visible; left:25px; top:
-100px; z-index:+1\\\" onmouseover=\\\"overdiv=1;\\\"  onmouseout=\\\"overdiv=0; setTimeout(\\\'hideLayer()\\\',1000)\\\"\\>\\&nbsp;\\</div\\>";

        Printf.bprintf buf "\\<form id=\\\"selectForm\\\" name=\\\"selectForm\\\" action=\\\"files\\\"\\>
\\<table class=main cellspacing=0 cellpadding=0\\>

\\<tr\\>\\<td\\>

\\<table cellspacing=0  cellpadding=0  width=100%%\\>\\<tr\\>
\\<td %s class=downloaded width=100%%\\>Total(%d): %s/%s @ %.1f KB/s\\</td\\>%s

\\<td class=big\\>\\<input class=bigbutton type=\\\"button\\\" value=\\\"Pause all\\\" onclick=\\\"pauseAll(true);\\\"\\>\\</td\\>
\\<td class=big\\>\\<input class=bigbutton type=\\\"button\\\" value=\\\"Resume all\\\" onclick=\\\"resumeAll(true);\\\"\\>\\</td\\>
\\<td class=big\\>\\<input class=bigbutton type=\\\"button\\\" value=\\\"Clear all\\\" onclick=\\\"clearAll(false);\\\"\\>\\</td\\>
\\<td class=\\\"big pr\\\"\\>\\<input class=bigbutton type=submit value='Submit changes'\\>\\</td\\>
\\</tr\\>\\</table\\>

\\</td\\>\\</tr\\>
\\<tr\\>\\<td\\>

\\<table width=\\\"100%%\\\" class=\\\"downloaders\\\" cellspacing=0 cellpadding=0\\>\\<tr\\>

\\<td title=\\\"Pause\\\"  class=\\\"dlheader np\\\"\\>P\\</td\\>
\\<td title=\\\"Resume\\\" class=\\\"dlheader np\\\"\\>R\\</td\\>
\\<td title=\\\"Cancel\\\" class=\\\"dlheader brs\\\"\\>C\\</td\\>
\\<td title=\\\"Click to switch release status\\\" class=\\\"dlheader brs\\\"\\>R\\</td\\>"
(if !qnum > 0 then begin
        Printf.sprintf "title=\\\"Active(%d): %s/%s | Queued(%d): %s/%s\\\""
        (List.length guifiles - !qnum) (size_of_int64 (!tdl -- !qdl)) (size_of_int64 (!tsize -- !qsize))
        !qnum (size_of_int64 !qdl) (size_of_int64 !qsize);
end
else "")
(List.length guifiles) (size_of_int64 !tdl) (size_of_int64 !tsize) (!trate /. 1024.)
(let unread = ref 0 in
Fifo.iter (fun (t,i,num,n,s) -> if t > !last_message_log then incr unread) chat_message_fifo;
if !unread > 0 then Printf.sprintf "\\<td onMouseOver=\\\"mOvr(this);\\\" onMouseOut=\\\"mOut(this);\\\" class=downloaded title=\\\"%d unread messages\\\"\\>\\<a onClick=\\\"mSub('fstatus','version');mSub('output','message')\\\"\\>(+%d)\\</a\\>\\&nbsp;\\</td\\>" !unread !unread else "");

if !!html_mods_vd_network then Printf.bprintf buf
"\\<td title=\\\"Sort by network\\\" class=dlheader\\>\\<input style=\\\"padding-left: 0px; padding-right: 0px;\\\" class=headbutton type=submit value=N name=sortby\\>\\</td\\>";

Printf.bprintf buf
"\\<td title=\\\"Sort by filename\\\" class=dlheader\\>\\<input class=headbutton type=submit value=File name=sortby\\>\\</td\\>";

if !!html_mods_vd_user then Printf.bprintf buf
"\\<td title=\\\"Sort by user\\\" class=dlheader\\>\\<input class=headbutton type=submit value=User name=sortby\\>\\</td\\>";

if !!html_mods_vd_group then Printf.bprintf buf
"\\<td title=\\\"Sort by group\\\" class=dlheader\\>\\<input class=headbutton type=submit value=Group name=sortby\\>\\</td\\>";

Printf.bprintf buf
"\\<td title=\\\"Sort by size\\\" class=dlheader\\>\\<input class=headbutton type=submit value=Size name=sortby\\>\\</td\\>
\\<td title=\\\"Sort by size downloaded\\\" class=dlheader\\>\\<input class=\\\"headbutton ar\\\" type=submit value=DLed name=sortby\\>\\</td\\>
\\<td title=\\\"Sort by percent\\\" class=dlheader\\>\\<input class=headbutton type=submit value=%% name=sortby\\>\\</td\\>";
if !!html_mods_vd_comments then Printf.bprintf buf
"\\<td title=\\\"Sort by comments\\\" class=dlheader\\>\\<input style=\\\"padding-left: 0px; padding-right: 0px;\\\" class=headbutton type=submit value=Cm name=sortby\\>\\</td\\>";

Printf.bprintf buf
"\\<td title=\\\"Sort by number of sources\\\" class=dlheader\\>\\<input style=\\\"padding-left: 0px; padding-right: 0px;\\\" class=headbutton type=submit value=Srcs name=sortby\\>\\</td\\>";

if !!html_mods_vd_active_sources then Printf.bprintf buf
"\\<td title=\\\"Sort by number of active sources\\\" class=dlheader\\>\\<input style=\\\"padding-left: 0px; padding-right: 0px;\\\" class=headbutton type=submit value=A name=sortby\\>\\</td\\>";

Printf.bprintf buf
"\\<td title=\\\"Sort by file availability percentage (using %s availability)\\\" class=dlheader\\>\\<input style=\\\"padding-left: 0px; padding-right: 0px;\\\" class=headbutton type=submit value=Avail name=sortby\\>\\</td\\>"
(if !!html_mods_use_relative_availability then "Relative" else "Total");

if !!html_mods_vd_age then Printf.bprintf buf
"\\<td title=\\\"Sort by age of download\\\" class=dlheader\\>\\<input style=\\\"padding-left: 0px; padding-right: 0px;\\\" class=headbutton type=submit value=Age name=sortby\\>\\</td\\>";

if !!html_mods_vd_last then Printf.bprintf buf
"\\<td title=\\\"Sort by last seen complete\\\" class=dlheader\\>\\<input style=\\\"padding-left: 0px; padding-right: 0px;\\\" class=headbutton type=submit value=Last name=sortby\\>\\</td\\>";

Printf.bprintf buf
"\\<td title=\\\"Sort by rate\\\" class=dlheader\\>\\<input style=\\\"padding-left: 0px; padding-right: 0px;\\\" class=headbutton type=submit value=Rate name=sortby\\>\\</td\\>
\\<td title=\\\"Sort by estimated time of arrival\\\" class=dlheader\\>\\<input style=\\\"padding-left: 0px; padding-right: 0px;\\\" class=headbutton type=submit value=ETA name=sortby\\>\\</td\\>";

if !!html_mods_vd_prio then Printf.bprintf buf "\\<td title=\\\"Sort by priority\\\" class=dlheader\\>\\<input style=\\\"padding-left: 0px; padding-right: 0px;\\\" class=headbutton type=submit value=Priority name=sortby\\>\\</td\\>";

Printf.bprintf buf "\\</tr\\>";

let ctd fn td = Printf.sprintf "\\<td onClick=\\\"location.href='submit?q=vd+%d';return true;\\\" class=\\\"dl ar\\\"\\>%s\\</td\\>" fn td in

  print_table_html_mods buf
    (List.map (fun file ->
        [|
          (if !!html_mods_use_js_tooltips then
                        Printf.sprintf "
                                onMouseOver=\\\"mOvr(this);setTimeout('popLayer(\\\\\'%s<br>%sFile#: %d<br>Network: %s<br>User%s %s%s%s\\\\\')',%d);setTimeout('hideLayer()',%d);return true;\\\" onMouseOut=\\\"mOut(this);hideLayer();setTimeout('hideLayer()',%d)\\\"\\>"
                        (Http_server.html_real_escaped (Charset.Locale.to_utf8 file.file_name))
                        (match file_magic (file_find file.file_num) with
                           None -> ""
                         | Some magic -> "File type: " ^ (Http_server.html_real_escaped magic) ^ "<br>")
                        file.file_num
                        (net_name file)
                        (if file.file_group = "none" then "" else ":Group")
                        file.file_user
                        (if file.file_group = "none" then "" else Printf.sprintf ":%s" file.file_group)

      (if file.file_comments = [] then "" else
                            begin
           let num_comments = number_of_comments file in
           let buf1 = Buffer.create (!!max_comment_length * num_comments) in
           Printf.bprintf buf1 "<br><br>Comments(%d):<br>" (num_comments);
           let comments =
             if List.length file.file_comments > 5 then
               fst (List2.cut 5 file.file_comments) @ [Ip.null, "", 0, (_s "MLDonkey note: click file for more comments")]
             else
               file.file_comments
           in
           List.iter (fun (_,_,_,s) -> Printf.bprintf buf1 "%s<br>" (Http_server.html_real_escaped (Charset.Locale.to_utf8 s))) comments;
                              Buffer.contents buf1
                            end)

                        !!html_mods_js_tooltips_wait
                        !!html_mods_js_tooltips_timeout
                        !!html_mods_js_tooltips_wait
       else Printf.sprintf " onMouseOver=\\\"mOvr(this);return true;\\\" onMouseOut=\\\"mOut(this);\\\"\\>");

        (if downloading file then
              Printf.sprintf "\\<td class=\\\"dl al np\\\"\\>\\<input class=checkbox name=pause type=checkbox value=%d\\>\\</td\\>
                \\<td class=\\\"dl al np\\\"\\>R\\</td\\>
                \\<td class=\\\"dl al brs\\\"\\>\\<input class=checkbox name=cancel type=checkbox value=%d\\>\\</td\\>"
                file.file_num
                file.file_num		
            else
              Printf.sprintf "\\<td class=\\\"dl al np\\\"\\>P\\</td\\>
                \\<td class=\\\"dl al np\\\"\\>\\<input class=checkbox name=resume type=checkbox value=%d\\>\\</td\\>
                \\<td class=\\\"dl al brs\\\"\\>\\<input class=checkbox name=cancel type=checkbox value=%d\\>\\</td\\>"
                file.file_num
                file.file_num);

          Printf.sprintf "\\<td onClick=\\\"location.href='files?%s=%d';return true;\\\" class=\\\"dl al brs\\\"\\>\\%s\\</td\\>"
            (if file.file_release then "norelease" else "release")
            file.file_num (if file.file_release then "R" else "-");

          (if !!html_mods_vd_network then
                        Printf.sprintf "\\<td onClick=\\\"location.href='submit?q=vd+%d';return true;\\\"
                        title=\\\"%s\\\" class=\\\"dl al\\\"\\>%s\\</td\\>"
                        file.file_num (net_name file) (short_net_name file) else "");

          ( let size = Int64.to_float file.file_size in
            let downloaded = Int64.to_float file.file_downloaded in
            let size = if size < 1. then 1. else size in
            (if !!html_mods_use_js_tooltips then
            Printf.sprintf "\\<TD onClick=\\\"location.href='submit?q=vd+%d';return true;\\\"
                        class=\\\"dl al\\\"\\>%s\\<br\\>
                        \\<table cellpadding=0 cellspacing=0 width=100%%\\>\\<tr\\>
                        \\<td class=\\\"loaded\\\" style=\\\"height:%dpx\\\" width=\\\"%d%%\\\"\\> \\</td\\>
                        \\<td class=\\\"remain\\\" style=\\\"height:%dpx\\\" width=\\\"%d%%\\\"\\> \\</td\\>
                        \\</tr\\>\\</table\\>\\</td\\>"
            file.file_num
            (short_name file)
            (!!html_vd_barheight)
            (truncate (downloaded /. size *. 100.))
            (!!html_vd_barheight)
            (truncate ( (1. -. downloaded /. size) *. 100.))
         else
             Printf.sprintf "\\<TD onClick=\\\"location.href='submit?q=vd+%d';return true;\\\"
                        title=\\\"[File#: %d] [Net: %s] [Comments: %d]%s\\\" class=\\\"dl al\\\"\\>%s\\<br\\>
                        \\<table cellpadding=0 cellspacing=0 width=100%%\\>\\<tr\\>
                        \\<td class=\\\"loaded\\\" style=\\\"height:%dpx\\\" width=\\\"%d%%\\\"\\> \\</td\\>
                        \\<td class=\\\"remain\\\" style=\\\"height:%dpx\\\" width=\\\"%d%%\\\"\\> \\</td\\>
                        \\</tr\\>\\</table\\>\\</td\\>"
            file.file_num
            file.file_num
                        (net_name file) (number_of_comments file)
                        (if !!max_name_len < String.length file.file_name then " " ^ file.file_name else "")
            (short_name file)
            (!!html_vd_barheight)
            (truncate (downloaded /. size *. 100.))
            (!!html_vd_barheight)
            (truncate ( (1. -. downloaded /. size) *. 100.)));
          );

          (if !!html_mods_vd_user then ctd file.file_num file.file_user else "");
          (if !!html_mods_vd_group then ctd file.file_num file.file_group else "");

          (ctd file.file_num (size_of_int64 file.file_size));
          (ctd file.file_num (size_of_int64 file.file_downloaded));
          (ctd file.file_num (Printf.sprintf "%.1f" (percent file)));

          (if !!html_mods_vd_comments then
            ctd file.file_num (Printf.sprintf "%d" (number_of_comments file))
            else "");

          (ctd file.file_num (Printf.sprintf "%d" (number_of_sources file)));

          (if !!html_mods_vd_active_sources then
            ctd file.file_num (Printf.sprintf "%d" (number_of_active_sources file))
                  else "");

          (ctd file.file_num (Printf.sprintf "%.0f" (get_file_availability file)));


          (if !!html_mods_vd_age then
                        ctd file.file_num (let age = (BasicSocket.last_time ()) - file.file_age in Date.time_to_string age "long")
                   else "");

          (if !!html_mods_vd_last then
                        ctd file.file_num (if file.file_last_seen > 0
             then let last = (BasicSocket.last_time ()) - file.file_last_seen in
                Date.time_to_string last "long"
             else "-"
                )
                        else ""
          );

          (ctd file.file_num
                (match file.file_state with
                                   FilePaused -> "Paused"
                 | FileQueued -> "Queued"
                 | _ -> if file.file_download_rate < 10.24 then "-"
                       else Printf.sprintf "%5.1f" (file.file_download_rate /. 1024.)
                )
          );

          (ctd file.file_num (if (file.file_download_rate < 10.24 || stalled file) then "-"
                                else Date.time_to_string (calc_file_eta file) "long"));

          (if !!html_mods_vd_prio then
              (Printf.sprintf "\\<td class=\\\"dl ar\\\"\\>\\<div id=\\\"divSelectPriority%d\\\"\\>\\<select id=\\\"selectPriority%d\\\"
                        style=\\\"font-size: 8px; font-family: verdana\\\" onchange=\\\"javascript:submitPriority(%d,%d,this)\\\"\\>\n"
                        file.file_num file.file_num file.file_num file.file_priority)
                        ^ (match file.file_priority with 0 | -10 | 10 | -20 | 20 -> "" | _ ->
                          Printf.sprintf "\\<option value=\\\"=%d\\\" SELECTED\\>%d\n" file.file_priority file.file_priority)
                        ^ "\\<option value=\\\"=20\\\""  ^ (if file.file_priority = 20 then " SELECTED" else "") ^ "\\>Very high\n"
                        ^ "\\<option value=\\\"=10\\\""  ^ (if file.file_priority = 10 then " SELECTED" else "") ^ "\\>High\n"
                        ^ "\\<option value=\\\"=0\\\""  ^ (if file.file_priority = 0 then " SELECTED" else "") ^ "\\>Normal\n"
                        ^ "\\<option value=\\\"=-10\\\""  ^ (if file.file_priority = -10 then " SELECTED" else "") ^ "\\>Low\n"
                        ^ "\\<option value=\\\"=-20\\\""  ^ (if file.file_priority = -20 then " SELECTED" else "") ^ "\\>Very Low\n"
                        ^ "\\<option value=\\\"10\\\"\\>+10\n"
                        ^ "\\<option value=\\\"5\\\"\\>+5\n"
                        ^ "\\<option value=\\\"1\\\"\\>+1\n"
                        ^ "\\<option value=\\\"-1\\\"\\>-1\n"
                        ^ "\\<option value=\\\"-5\\\"\\>-5\n"
                        ^ "\\<option value=\\\"-10\\\"\\>-10\n"
                        ^ "\\</select\\>\\</div\\>"
               else "");

        |]
    ) guifiles);

  Printf.bprintf buf "\\</form\\>"

end
else
  html_mods_table_one_row buf "downloaderTable" "downloaders" [
    ("", "srh", (Printf.sprintf (_b "!! No files, please use search or the dllink <url> command to add a new download !!"))); ]

let html_mods_done_files buf files =

  Printf.bprintf buf "\\</pre\\>
\\<div class=\\\"main\\\"\\>
\\<table class=main cellspacing=0 cellpadding=0\\>

\\<tr\\>\\<td\\>

\\<form name=selectForm2 action=\\\"files\\\"\\>
\\<table cellspacing=0  cellpadding=0  width=100%%\\>\\<tr\\>
\\<td class=downloaded width=100%%\\>Total: %d - Use '\\<a target=\\\"fstatus\\\" href=\\\"submit?q=commit\\\"\\>commit\\</a\\>' to move these completed files to the incoming directory\\</td\\>
\\</tr\\>\\</table\\>

\\</td\\>\\</tr\\>
\\<tr\\>\\<td\\>

\\<table class=downloaders cellspacing=0 cellpadding=0\\>\\<tr\\>

\\<td title=\\\"Number\\\" class=dlheader\\>Num\\</td\\>
\\<td title=\\\"Network\\\" class=dlheader\\>Network\\</td\\>
\\<td title=\\\"Sort by filename\\\" class=dlheader\\>\\<input class=headbutton type=submit value=File name=sortby\\>\\</td\\>
\\<td title=\\\"Sort by size\\\" class=dlheader\\>\\<input class=headbutton type=submit value=Size name=sortby\\>\\</td\\>
\\<td title=\\\"Hash\\\" class=dlheader\\>Hash\\</td\\>

\\</tr\\>
" (List.length files);


     print_table_html_mods buf

    (List.map (fun file ->
        [|
            (Printf.sprintf "\\>\\<td class=dl\\>%d\\</td\\>\\<td class=dl\\>%s\\</td\\>"
             file.file_num (net_name file));
            (Printf.sprintf "\\<td class=dl\\>%s\\</td\\>"
            (short_name file));
            (Printf.sprintf "\\<td class=dl\\>%s\\</td\\>"
            (Int64.to_string file.file_size));
            (Printf.sprintf "\\<td class=dl\\>%s\\</td\\>"
            (string_of_uids file.file_uids))
        |]
    ) files);
  Printf.bprintf buf "\\</form\\>"

let print_human_readable file size =
 (if Int64.to_float size >= 1024. && Int64.to_float size < 1048576. then
    (Printf.sprintf "%5.1f%s" (Int64.to_float size /. 1024.) ("kb") )
  else if size >= Int64.of_float 1048576. && Int64.to_float size < 1073741824. then
    (Printf.sprintf "%5.1f%s" (Int64.to_float size /. 1048576.) ("mb") )
  else if size >= Int64.of_float 1073741824. then
    (Printf.sprintf "%5.1f%s" (Int64.to_float size /. 1073741824.) ("gb") )
  else if size < Int64.zero then
    (Printf.sprintf "%d chunks"
      (match file.file_chunks with
      | None -> 0
      | Some chunks -> 
          VB.fold_lefti (fun acc _ s -> match s with
            | VB.State_missing | VB.State_partial -> acc
            | VB.State_complete | VB.State_verified -> acc + 1
          ) 0 chunks))
  else (Printf.sprintf "%8s%s" (Int64.to_string size) ("b") ) )

let simple_print_file_list finished buf files format =
  let print_table = if format.conn_output = HTML then print_table_html 2
    else print_table_text in
  if not finished then
    if format.conn_output = HTML && !!html_checkbox_vd_file_list then
      begin
        if !!html_mods then print_file_html_mods buf files
        else
          print_file_html_form buf files
      end
    else
      print_table buf
        [|
        Align_Left; Align_Left; Align_Right; Align_Left; Align_Left; Align_Left;
        Align_Right; Align_Right; Align_Right |]
        (if format.conn_output = HTML then
          [|
            "[ Num ]";
            "\\<a href=\\\"submit\\?q\\=vd\\&sortby\\=name\\\"\\> File \\</a\\>";
            "\\<a href=\\\"submit\\?q\\=vd\\&sortby\\=percent\\\"\\> Percent \\</a\\>";
            "\\<a href=\\\"submit\\?q\\=vd\\&sortby\\=done\\\"\\> Downloaded \\</a\\>";
            "\\<a href=\\\"submit\\?q\\=vd\\&sortby\\=size\\\"\\> Size \\</a\\>";
            "Old";
            "\\<a href=\\\"submit\\?q\\=vd\\&sortby\\=rate\\\"\\> Rate \\</a\\>";
            "\\<a href=\\\"submit\\?q\\=vd\\&sortby\\=priority\\\"\\> Priority \\</a\\>";
          |] else
          [|
            "$nNum";
            "Rele";
            "Comm";
            "User";
            "Group";
            "File";
            "    %";
            "    Done";
            "    Size";
            "lSeen";
            "Old";
            " Active";
            "Rate";
            "Prio";
          |]
      )
      (List.map (fun file ->
            let rate, color =
              match file.file_state with
              | FilePaused -> "Paused", "$r"
              | FileQueued -> "Queued", "$g"
              | FileAborted s -> Printf.sprintf "Aborted %s" s, "$r"
              | _ ->
                  if file.file_download_rate < 10.24 then
                    "-", "$n"
                  else
                    Printf.sprintf "%4.1f" (
                      file.file_download_rate /. 1024.), "$c"
            in
            [|
              (Printf.sprintf "%0s[%0s]%0s"
                  (if !!term_ansi then (color)
                  else "")
                  (if format.conn_output = HTML then
                   (Printf.sprintf "\\<a href=\\\"submit\\?q\\=vd\\+%d\\\" $S\\>%0s%4d\\</a\\>"
                    file.file_num
                    (short_net_name file)
                    file.file_num)
                   else
                    (Printf.sprintf "%0s%4d"
                (short_net_name file)
                file.file_num))
                  (if format.conn_output = HTML then
                    Printf.sprintf "[\\<a href=\\\"submit\\?q\\=cancel\\+%d\\\" $S\\>CANCEL\\</a\\>][\\<a href=\\\"submit\\?q\\=%s\\+%d\\\" $S\\>%s\\</a\\>] "
                      file.file_num
                      (if downloading file then "pause" else "resume" )
                    file.file_num
                      (if downloading file then "PAUSE" else "RESUME")
                  else ""));
              (Printf.sprintf "%s" (if file.file_release then "R" else "-"));
              (Printf.sprintf "%4d" (number_of_comments file));
              file.file_user;
              file.file_group;
              (short_name file);
              (Printf.sprintf "%3.1f" (percent file));
              (if !!improved_telnet then (print_human_readable file file.file_downloaded)
                else (Int64.to_string file.file_downloaded) );
              (if !!improved_telnet then (print_human_readable file file.file_size)
                else (Int64.to_string file.file_size) );
              (Printf.sprintf "%s"
                (if file.file_last_seen > 0 then
                   let last = (BasicSocket.last_time ()) - file.file_last_seen in
                     Date.time_to_string last "long"
                 else "-"
                ));
              (Printf.sprintf "%d:%s" (age_to_day file.file_age)
                (
                  let len = Array.length file.file_chunks_age in
                  if len = 0 then "-" else
                  let min = ref (last_time ()) in
                  for i = 0 to len - 1 do
                    if file.file_chunks_age.(i) < !min then
                      min := file.file_chunks_age.(i)
                  done;
                  if !min = 0 then "-" else
                    string_of_int (age_to_day !min)));
              (Printf.sprintf "%2d/%-4d" (number_of_active_sources file) (number_of_sources file));
              rate;
              (Printf.sprintf "%4d" (file.file_priority);)
            |]
        ) files)
      else
  if use_html_mods format then
    html_mods_done_files buf files
  else

    print_table buf
      [||]
      (if format.conn_output = HTML then
        [|
          "[ Num ]";
          "\\<a href=\\\"submit\\?q\\=vd\\&sortby\\=name\\\"\\> File \\</a\\>";
          "\\<a href=\\\"submit\\?q\\=vd\\&sortby\\=size\\\"\\> Size \\</a\\>";
          "MD4";
        |]
      else
        [|
          "[ Num ]";
          "File";
          "Size";
          "MD4";
        |]
    )
    (List.map (fun file ->
          [|
            (Printf.sprintf "[%s %-5d]"
                (net_name file)
              file.file_num);
            (short_name file);
            (Int64.to_string file.file_size);
            (Md4.to_string file.file_md4)
          |]
      ) files)

let print_bw_stats buf =
  Printf.bprintf buf "Down: %.1f KB/s ( %d + %d ) | Up: %.1f KB/s ( %d + %d ) | Shared: %d/%s | Downloaded: %s | Uploaded: %s"
       (( (float_of_int !udp_download_rate) +. (float_of_int !control_download_rate)) /. 1024.0)
         !udp_download_rate
         !control_download_rate
       (( (float_of_int !udp_upload_rate) +. (float_of_int !control_upload_rate)) /. 1024.0)
         !udp_upload_rate
         !control_upload_rate
         !nshared_files
  (size_of_int64 !nshared_bytes)
  (size_of_int64 !download_counter)
  (size_of_int64 !upload_counter)

let console_topic () =
  Printf.sprintf "(DL: %.1f | UL: %.1f) MLNet %s"
    (( (float_of_int !udp_download_rate) +. (float_of_int !control_download_rate)) /. 1024.0)
    (( (float_of_int !udp_upload_rate) +. (float_of_int !control_upload_rate)) /. 1024.0)
    Autoconf.current_version

let display_active_file_list buf o list =
  display_vd := true;

  if not (use_html_mods o) then begin
(*    Printf.bprintf buf "Downloaded %d/%d files\n" (List.length !!done_files)
    (List.length !!files); *)
    print_bw_stats buf;
    Printf.bprintf buf "\n";
  end;

  if o.conn_output <> HTML && !!improved_telnet then
  begin
    let list = 
      List.sort (fun f1 f2 -> compare (percent f2) (percent f1)) list in
    simple_print_file_list false buf list o
  end
  else
  let list =
    try
      let sorter =
        match o.conn_sortvd with

        | BySize -> (fun f1 f2 -> compare f2.file_size f1.file_size)
        | ByRate -> (fun f1 f2 ->
                if stalled f1 then 1 else
                if stalled f2 then -1 else
                  compare f2.file_download_rate f1.file_download_rate)
        | ByName -> (fun f1 f2 -> String.compare 
                       (String.lowercase f1.file_name) 
                       (String.lowercase f2.file_name))
        | ByDone -> (fun f1 f2 -> 
                       compare f2.file_downloaded f1.file_downloaded)
        | ByPriority -> (fun f1 f2 -> 
                           compare f2.file_priority f1.file_priority)
        | BySources -> (fun f1 f2 -> compare 
                          (number_of_sources f2) (number_of_sources f1))
        | ByASources -> (fun f1 f2 -> 
                           compare (number_of_active_sources f2) 
                                   (number_of_active_sources f1))
        | ByPercent -> (fun f1 f2 -> compare (percent f2) (percent f1))
        | ByETA -> (fun f1 f2 -> compare (calc_file_eta f1) (calc_file_eta f2))
        | ByAge -> (fun f1 f2 -> compare f2.file_age f1.file_age)
        | ByLast -> (fun f1 f2 -> compare f2.file_last_seen f1.file_last_seen)
        | ByNet -> (fun f1 f2 -> compare (net_name f1) (net_name f2))
        | ByAvail -> (fun f1 f2 -> compare 
                        (get_file_availability f2) (get_file_availability f1))
        | ByComments -> (fun f1 f2 -> compare 
                           (number_of_comments f2) (number_of_comments f1))
        | ByUser -> (fun f1 f2 -> compare f1.file_user f2.file_user)
        | ByGroup -> (fun f1 f2 -> compare f1.file_group f2.file_group)
        | NotSorted -> raise Not_found
      in
      List.sort sorter list
    with _ -> list
  in
  simple_print_file_list false buf list o

let display_file_list buf o l =
  display_active_file_list buf o l;
  if not (use_html_mods o) then
    Printf.bprintf buf "%0sDownloaded %d files\n" (if !!term_ansi then "$n" else "") (List.length !!done_files);
  if !!done_files <> [] then begin
(*      List.iter (fun file -> CommonFile.file_print file o)   !!done_files; *)
      simple_print_file_list true buf
        (List2.tail_map file_info !!done_files) o;
      if not (use_html_mods o) then
        if !!auto_commit then
          if o.conn_output = HTML then
            html_mods_table_one_row buf "searchTable" "search" [
              ("", "srh", "Files will be automatically committed in the incoming directory"); ]
          else
            Printf.bprintf buf
              "Files will be automatically committed in the incoming directory"
        else
          if o.conn_output = HTML then
            html_mods_table_one_row buf "searchTable" "search" [
              ("", "srh", "Use 'commit' to move downloaded files to the incoming directory"); ]
          else
            Printf.bprintf buf
              "Use 'commit' to move downloaded files to the incoming directory"
    end


let get_tag_value tag =
  match tag.tag_value with
 | Uint64 i -> String.escaped (Int64.to_string i)
 | Fint64 i -> String.escaped (Int64.to_string i)
 | Uint16 i | Uint8 i -> String.escaped (string_of_int i)
 | String s -> String.escaped s
 | Addr i -> Ip.to_string i
 | Pair (x,y) -> Printf.sprintf "%Ld, %Ld" x y

let old_print_search buf o results =
  let user = o.conn_user in
  let counter = ref 0 in
  if use_html_mods o then
  begin
    html_mods_cntr_init ();
    if !!html_mods_use_js_tooltips then Printf.bprintf buf "\\<div id=\\\"object1\\\" style=\\\"position:absolute; background-color:#FFFFDD;color:black;border-color:black;border-width:20px;font-size:8pt; visibility:visible; left:25px; top:-100px; z-index:+1\\\" onmouseover=\\\"overdiv=1;\\\"  onmouseout=\\\"overdiv=0; setTimeout(\\\'hideLayer()\\\',1000)\\\"\\>\\&nbsp;\\</div\\>\n";
    html_mods_table_header_colspan buf "resultsTable" "results" [
      ( "1", "0", "srh", "Network", "Network" ) ;
      ( "1", "0", "srh", "File", "File (mouseover)" ) ;
      ( "1", "1", "srh ar", "Size", "Size" ) ;
      ( "1", "1", "srh ar", "Availability", "A" ) ;
      ( "1", "1", "srh ar", "Complete Sources", "C" ) ;
      ( "2", "0", "srh", "Hash (click for lookup)", "Hash check" ) ;
      ( "1", "1", "srh ar", "Length", "Len" ) ;
      ( "1", "1", "srh ar", "Codec", "Code" ) ;
      ( "1", "1", "srh ar", "Bitrate", "Rate" ) ;
      ( "1", "0", "srh", "Tags (mouseover)", "Tags" ) ] ;
  end;
  (try
      List.iter (fun (rs,r,avail) ->
          if !!display_downloaded_results || not r.result_done  then begin
              incr counter;
              if !counter >= !!max_displayed_results then raise Exit;

              if use_html_mods o then
                Printf.bprintf buf "\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr ());

              user.ui_last_results <- (!counter, rs) :: user.ui_last_results;
              let network_name = 
                try 
                 let n = network_find_by_num r.result_source_network in
                 n.network_name 
                with _ -> "Unknown"
              in
              
                if use_html_mods o 
                  then Printf.bprintf buf "\\<td class=\\\"sr\\\"\\>%s\\</td\\>"
                   (let rec iter uids =
                      match uids with
                        [] -> network_name
                      | uid :: tail ->
                          match Uid.to_uid uid with
                            Ed2k md4 ->
                              Printf.sprintf "\\<a href=\\\"%s\\\"\\>%s\\</a\\>"
                                (file_print_ed2k_link (List.hd r.result_names) r.result_size md4)
                                network_name
                            | _  -> iter tail
                    in
                    iter r.result_uids)
                  else Printf.bprintf  buf "[%5d] %s " !counter network_name;

              if o.conn_output = HTML then begin
                  if !!html_mods then
                    if !!html_mods_use_js_tooltips then
                    begin
                      Printf.bprintf buf "\\<td onMouseOver=\\\"setTimeout('popLayer(\\\\\'";
                      begin
                        match r.result_names with
                          [] -> ()
                        | name :: names ->
                           Printf.bprintf buf "%s" (Http_server.html_real_escaped name);
                            List.iter (fun s ->
                               if use_html_mods o then Printf.bprintf buf "\\<BR\\>";
                                Printf.bprintf buf "       %s" (Http_server.html_real_escaped s)
                            ) names;
                        if use_html_mods o then Printf.bprintf buf "\\<BR\\>";
                      end;
                      let nl = ref false in
                      List.iter (fun t ->
                          match t.tag_name with
                          | Field_KNOWN "FTH" | Field_KNOWN "urn" -> ()
                          | _ ->
                              Buffer.add_string buf ((if !nl then "<br>" else begin nl := true;"" end) ^
                                escaped_string_of_field t ^ ": " ^ get_tag_value t);
                      ) r.result_tags;
                    Printf.bprintf buf "\\\\\')',%d);setTimeout('hideLayer()',%d);return true;\\\"  onMouseOut=\\\"hideLayer();setTimeout('hideLayer()',%d);return true;\\\" class=\\\"sr\\\"\\>\\<a href=results\\?d=%d target=\\\"$S\\\"\\>"
                        !!html_mods_js_tooltips_wait
                        !!html_mods_js_tooltips_timeout
                        !!html_mods_js_tooltips_wait
                         r.result_num
                    end
                    else begin
                      Printf.bprintf buf "\\<td title=\\\"";
                      let nl = ref false in
                      List.iter (fun t ->
                          match t.tag_name with
                          | Field_KNOWN "FTH" | Field_KNOWN "urn" -> ()
                          | _ ->
                              Buffer.add_string buf ((if !nl then "\n" else begin nl := true;"" end) ^
                                  "|| (" ^
                                escaped_string_of_field t ^ "): " ^ get_tag_value t);
                      ) r.result_tags;

                      Printf.bprintf buf "\\\" class=\\\"sr\\\"\\>\\<a href=results\\?d=%d target=\\\"$S\\\"\\>" r.result_num
                    end
                  else Printf.bprintf buf "\\<a href=results\\?d=%d $S\\>" r.result_num;
                end;
              begin
                match r.result_names with
                  [] -> ()
                | name :: names ->
                    Printf.bprintf buf "%s\n" (shorten name !!max_result_name_len);
                    List.iter (fun s ->
                        if use_html_mods o then Printf.bprintf buf "\\<BR\\>";
                        Printf.bprintf buf "       %s\n" (shorten s !!max_result_name_len)
                    ) names;
              end;
              if r.result_done then
                begin
                  if use_html_mods o then Printf.bprintf buf "\\<BR\\>";
                  Printf.bprintf buf " ALREADY DOWNLOADED\n "
                end;
              begin
                match r.result_comment with
                  "" -> ()
                | comment -> begin
                      if use_html_mods o then Printf.bprintf buf "\\<BR\\>";
                      Printf.bprintf buf "COMMENT: %s\n" comment
                    end;
              end;
              if o.conn_output = HTML then
                begin
                  if !!html_mods then Printf.bprintf buf "\\</a\\>\\</td\\>"
                  else Printf.bprintf buf "\\</a href\\>";
                end;
              let hash = ref (string_of_uids r.result_uids) in
              let real_hash =
                if String.contains !hash ':' then
                  String.sub !hash ((String.rindex !hash ':')+1)
                  ((String.length !hash) - (String.rindex !hash ':') - 1)
                else
                  !hash
              in
              let clength = ref "" in
              let ccodec = ref "" in
              let cmediacodec = ref "" in
              let cbitrate = ref "" in
              let cavail = ref (string_of_int avail) in
              let csource = ref "" in
              let cformat = ref "" in
              List.iter (fun t ->
                  (match t.tag_name with
                    | Field_KNOWN "urn"
                    | Field_KNOWN "FTH"  -> hash := get_tag_value t
                    | Field_Availability -> cavail := get_tag_value t
                    | Field_Completesources -> csource := get_tag_value t
                    | Field_Length -> clength := get_tag_value t
                    | Field_Codec -> ccodec := get_tag_value t
                    | Field_Mediacodec -> cmediacodec := get_tag_value t
                    | Field_Bitrate -> cbitrate := get_tag_value t
                    | Field_Format -> cformat := get_tag_value t
                    | _ -> ())) r.result_tags;

              if use_html_mods o then
                Printf.bprintf buf "\\<td class=\\\"sr ar\\\"\\>%s\\</td\\>
                        \\<td class=\\\"sr ar\\\"\\>%s\\</td\\>
                        \\<td class=\\\"sr ar\\\"\\>%s\\</td\\>
                        \\<td class=\\\"sr\\\"\\>\\<a href=\\\"http://bitzi.com/lookup/ed2k:%s\\\"\\>BI\\</a\\>\\</td\\>
                        \\<td class=\\\"sr\\\"\\>\\<a href=\\\"http://www.filedonkey.com/url/%s\\\"\\>FD\\</a\\>\\</td\\>
                        \\<td class=\\\"sr ar\\\"\\>%s\\</td\\>
                        \\<td class=\\\"sr ar\\\"\\>%s\\</td\\>
                        \\<td class=\\\"sr ar\\\"\\>%s\\</td\\>"
                  (size_of_int64 r.result_size)
                  !cavail
                  !csource
                  real_hash
                  real_hash
                  !clength
                  (if !ccodec = "" then
                     begin
                       if !cmediacodec = "" then
                         !cformat 
                       else
                         !cmediacodec
                     end
                   else
                     !ccodec)
                  !cbitrate
              else	Printf.bprintf  buf "          %10s %10s "
                  (Int64.to_string r.result_size)
                (string_of_uids r.result_uids);

              if use_html_mods o then begin
                  Printf.bprintf buf "\\<td class=\\\"sr\\\"\\>";
                  List.iter (fun t ->
                      (match t.tag_name with
                        | Field_Completesources
                        | Field_Availability
                        | Field_Length
                        | Field_Codec
                        | Field_Mediacodec
                        | Field_Format
                        | Field_Bitrate
(* TODO : "urn" shouldn't be some kind of Field_Uid of Gnutella ? *)
                        | Field_KNOWN "urn"
(* TODO : "FTH" shouldn't be some kind of Field_Uid of Fasttrack ? *)
                        | Field_KNOWN "FTH"  -> ()
                        | _ ->
                            Buffer.add_string buf ("\\<span title=\\\"" ^
                                get_tag_value t ^ "\\\"\\>(" ^
                              escaped_string_of_field t ^ ") \\</span\\>");
                      )
                  ) r.result_tags;
                  Printf.bprintf buf "\\</td\\>\\</tr\\>";
                end
              else
                List.iter (fun t ->
                    Buffer.add_string buf (Printf.sprintf "%-3s "
                        (if t.tag_name = Field_Availability then !cavail else
                          get_tag_value t))
                ) r.result_tags;
              Buffer.add_char buf '\n';
            end
      ) results;
      if use_html_mods o then Printf.bprintf buf "\\</table\\>"
    with _ -> ())


let add_filter_table buf search_num =

  Printf.bprintf buf "\\<form action=\\\"filter\\\"\\>";
  Printf.bprintf buf "\\<input type=hidden name=num value=%d\\>" search_num;

  Printf.bprintf buf "\\<table\\>";
  Printf.bprintf buf "\\<tr\\>";

  Printf.bprintf buf "\\<td\\>";
  Printf.bprintf buf "\\<input type=submit value='Filter Out'\\>";
  Printf.bprintf buf "\\</td\\>";

  Printf.bprintf buf "\\</tr\\>\\<tr\\>";

  Printf.bprintf buf "\\<td\\>\\<table\\>\\<tr\\>";

  Printf.bprintf buf "\\<table\\>";
  Printf.bprintf buf "\\<td\\> Media: \\</td\\>";
  Printf.bprintf buf "\\<td\\>\\<input name=media type=checkbox value=Audio\\> Audio \\</td\\>";
  Printf.bprintf buf "\\<td\\>\\<input name=media type=checkbox value=Video\\> Video \\</td\\>";
  Printf.bprintf buf "\\<td\\>\\<input name=media type=checkbox value=Pro\\> Pro \\</td\\>";
  Printf.bprintf buf "\\<td\\>\\<input name=media type=checkbox value=Doc\\> Doc \\</td\\>";
  Printf.bprintf buf "\\</table\\>";

  Printf.bprintf buf "\\</tr\\>\\<tr\\>";

  Printf.bprintf buf "\\<table\\>";
  Printf.bprintf buf "\\<td\\> Formats: \\</td\\>";
  Printf.bprintf buf "\\<td\\>\\<input name=format type=checkbox value=mp3\\> Mp3 \\</td\\>";
  Printf.bprintf buf "\\<td\\>\\<input name=format type=checkbox value=avi\\> Avi \\</td\\>";
  Printf.bprintf buf "\\<td\\>\\<input name=format type=checkbox value=zip\\> Zip \\</td\\>";
  Printf.bprintf buf "\\<td\\>\\<input name=format type=checkbox value=mpg\\> Mpg \\</td\\>";
  Printf.bprintf buf "\\</table\\>";

  Printf.bprintf buf "\\</tr\\>\\<tr\\>";

  Printf.bprintf buf "\\<table\\>";
  Printf.bprintf buf "\\<td\\> Sizes: \\</td\\>";
  Printf.bprintf buf "\\<td\\>\\<input name=size type=checkbox value=0to5\\> 0/5 MB \\</td\\>";
  Printf.bprintf buf "\\<td\\>\\<input name=size type=checkbox value=5to20\\> 5/20 MB \\</td\\>";
  Printf.bprintf buf "\\<td\\>\\<input name=size type=checkbox value=20to400\\> 20/400 MB \\</td\\>";
  Printf.bprintf buf "\\<td\\>\\<input name=size type=checkbox value=400\\> 400+ MB \\</td\\>";
  Printf.bprintf buf "\\</table\\>";

  Printf.bprintf buf "\\</tr\\>\\</table\\>\\</td\\>";
  Printf.bprintf buf "\\</tr\\>";

  Printf.bprintf buf "\\</table\\>";

  Printf.bprintf buf "\\</form\\>"

(* with checkboxes *)
let print_search_html buf results o search_num =
  let user = o.conn_user in
  let counter = ref 0 in

  let files = ref [] in

  (try
      List.iter  (fun (rs, r, avail) ->

          try
            o.conn_filter r;
            if !!display_downloaded_results || not r.result_done  then
              let tags_string =
                let buf = Buffer.create 100 in
                List.iter (fun t ->
                    Buffer.add_string buf (Printf.sprintf "%-3s "
                        (if t.tag_name = Field_Availability then "" else
                        match t.tag_value with
                          String s -> s
                        | Uint64 i -> Int64.to_string i
                        | Fint64 i -> Int64.to_string i
                        | _ -> "???"
                      ))
                ) r.result_tags;
                Buffer.contents buf
              in
              incr counter;
              if !counter >= !!max_displayed_results then raise Exit;
              user.ui_last_results <- (!counter, rs) :: user.ui_last_results;
              files := [|

                (Printf.sprintf "[%5d]\\<input name=d type=checkbox value=%d\\>" !counter r.result_num);
                (Int64.to_string r.result_size);
                (string_of_int avail);

                (
                  let names = r.result_names in
                  let names = if r.result_done then
                      names @ ["ALREADY DOWNLOADED"] else names in
                  let names = match  r.result_comment with
                    | "" -> names
                    | comment ->
                        names @ ["COMMENT: " ^ comment]
                  in
                  match names with
                    [name] -> name
                  | _ ->
                      let buf = Buffer.create 100 in
                      Buffer.add_string buf "\\<table\\>\n";
                      List.iter (fun s ->
                          Buffer.add_string buf "\\<tr\\>\\<td\\>";
                          Buffer.add_string buf s;
                          Buffer.add_string buf "\\</td\\>\\</tr\\>";
                      ) names;
                      Buffer.add_string buf "\\</table\\>\n";

                      Buffer.contents buf
                );

                tags_string;


                (string_of_uids r.result_uids);
              |] :: !files
          with _ -> ()
      ) results;
    with _ -> ());

  if !counter > !!filter_table_threshold then
    add_filter_table buf search_num;

  Printf.bprintf buf "\\<form action=results\\>";
  Printf.bprintf buf "\\<input type=submit value='Submit Changes'\\>";
  print_table_html 10 buf [||]
    [|
    "[ Num ]";
    "Size";
    "Avail";
    "Names";
    "Tags";
    "MD4";
  |]
    (List.rev !files);
  Printf.bprintf buf "\\</form\\>"



let print_results stime buf o results =

  let user = o.conn_user in
  let print_table = if o.conn_output = HTML then print_table_html 2
    else print_table_text in

  let counter = ref 0 in
  let nsources = ref 0 in
  let totalsize = ref 0L in
  let files = ref [] in
  (try
      List.iter (fun (rs, r,avail) ->
          if !!display_downloaded_results || not r.result_done then begin
              incr counter;
              nsources := !nsources + avail;
              totalsize := !totalsize ++ r.result_size ** (Int64.of_int avail);
              if !counter >= !!max_displayed_results then raise Exit;
              user.ui_last_results <- (!counter, rs) :: user.ui_last_results;
              let new_result = !!save_results > 0 && r.result_time >= stime in
              files := [|

                (if use_html_mods o then
                    Printf.sprintf "\\>\\<td class=\\\"sr\\\"\\>%d\\</td\\>" !counter
                  else Printf.sprintf "%s[%s%5d]"
                      (if new_result && !!term_ansi then "$b" else "$n")
                      (if new_result then "N" else " ")
                    !counter);

                (if use_html_mods o then
                    "\\<td class=\\\"sr ar\\\"\\>" ^  size_of_int64 r.result_size ^ "\\</td\\>"
                  else Int64.to_string r.result_size
                );

                (if use_html_mods o then
                    "\\<td class=\\\"sr ar\\\"\\>" ^  (string_of_int avail) ^ "\\</td\\>"
                  else (string_of_int avail)
                );

                (if r.result_done then
                   begin
                        if use_html_mods o then
                            "\\<td class=\\\"sr ar\\\"\\>D\\</td\\>"
                          else "dled"
                   end
                 else
                   begin
                        if use_html_mods o then
                            "\\<td class=\\\"sr ar\\\"\\> \\</td\\>"
                          else " "
                   end
                );

                (Printf.sprintf "%s%s%s"
                    (if o.conn_output = HTML then begin
                        if !!html_mods then Printf.sprintf "\\<td class=\\\"sr\\\"\\>\\<a href=results\\?d=%d target=\\\"$S\\\"\\>" r.result_num
                        else Printf.sprintf "\\<a href=results\\?d=%d $S\\>" r.result_num;
                      end
                    else "")

                  ( shorten (
                      let names = r.result_names in
                      let names = match  r.result_comment with
                          "" -> names
                        |  comment ->
                            names @ ["COMMENT: " ^ comment]
                      in
                      match names with
                        [name] -> name
                      | _ ->
                          let buf = Buffer.create 100 in
                          if o.conn_output = HTML then Buffer.add_string buf "\\<table\\>\n";
                          List.iter (fun s ->
                              if o.conn_output = HTML then Buffer.add_string buf "\\<tr\\>";
                              Buffer.add_string buf s;
                              if o.conn_output = HTML then Buffer.add_string buf "\\</tr\\>";
                          ) names;
                          if o.conn_output = HTML then Buffer.add_string buf "\\</table\\>\n";

                          Buffer.contents buf
                    ) !!max_result_name_len)
                  (if o.conn_output = HTML then
                      begin
                        if !!html_mods then "\\</a\\>\\</td\\>"
                        else "\\</a href\\>"
                      end
                    else ""
                  )
                );

                (let buf = Buffer.create 100 in

                  if use_html_mods o then Buffer.add_string buf "\\<td class=\\\"sr\\\"\\>";

                  List.iter (fun t ->
                      Buffer.add_string buf (Printf.sprintf "%-3s "
                          (if t.tag_name = Field_Availability then "" else
                          match t.tag_value with
                            String s -> s
                          | Uint64 i -> Int64.to_string i
                          | Fint64 i -> Int64.to_string i
                          | _ -> ""
                        ))
                  ) r.result_tags;
                  Buffer.contents buf);

                (
                  let uid = string_of_uids r.result_uids in
                  if use_html_mods o then
                    Printf.sprintf "\\<td class=\\\"sr\\\"\\>%s\\</td\\>" uid
                  else uid
                );

              |] :: !files;
            end
      ) results;
    with _ -> ());
  if use_html_mods o then
    begin

      html_mods_table_header buf "resultsTable" "results" [
                ( Num, "srh", "Number", "#" ) ;
                ( Num, "srh ar", "Size", "Size" ) ;
                ( Str, "srh ar", "Availability", "A" )  ;
                ( Str, "srh ar", "Status, D = already downloaded", "S" )  ;
                ( Str, "srh", "Filename", "Name" ) ;
                ( Str, "srh", "Tag", "Tag" ) ;
                ( Str, "srh", "MD4", "MD4" ) ];

      print_table_html_mods buf
       (List.rev !files)

    end

  else

    print_table buf [| Align_Left; Align_Right; Align_Right; Align_Right; Align_Left; Align_Left; Align_Left|]
      [|
      "[ Num ]";
      "Size";
      "Avail";
      "Status";
      "Names";
      "Tags";
      "MD4";
    |]

    (List.rev !files);
  Printf.bprintf buf "%d sources, total available %s\n" !nsources (size_of_int64 !totalsize)


let print_search buf s o =
  let user = o.conn_user in
  user.ui_last_search <- Some s;
  user.ui_last_results <- [];
  let results = ref [] in
  Intmap.iter (fun r_num (avail,rs) ->
      let r = IndexedResults.get_result rs in
      results := (rs, r, !avail) :: !results) s.search_results;
  let results = List.sort (fun (_, r1,_) (_, r2,_) ->
        compare r2.result_size r1.result_size
    ) !results in

  Printf.bprintf buf "Result of search %d\n" s.search_num;
  Printf.bprintf buf "%d results (%s)\n" s.search_nresults
    (if s.search_waiting = 0 then "done" else
      (string_of_int s.search_waiting) ^ " waiting");

  if o.conn_output != HTML then print_results s.search_time buf o results else
    begin
      if !!html_checkbox_search_file_list then
        print_search_html buf results o s.search_num
      else
        old_print_search buf o results
    end

let browse_friends () =
  List.iter (fun c -> client_browse c false) !!friends;
  List.iter (fun c -> client_browse c false) !contacts


let networks_header buf =
    html_mods_table_header buf "networkTable" "networkInfo" [
      ( Str, "srh br", "Network name", "Network" ) ;
      ( Str, "srh br", "Status", "Status" ) ;
      ( Str, "srh br", "Has upload", "Upload" ) ;
      ( Str, "srh br", "Has servers", "Servers" ) ;
      ( Str, "srh br", "Has supernodes", "Supernodes" ) ;
      ( Str, "srh br", "Has search", "Search" ) ;
      ( Str, "srh br", "Has chat", "Chat" ) ;
      ( Str, "srh br", "Has rooms", "Rooms" ) ;
      ( Str, "srh", "Has multinet", "Multinet" ) ;
      ( Str, "srh", "Has porttest", "Porttest" ) ]

let print_network_modules buf o =
  let buf = o.conn_buf in
  if use_html_mods o then
    begin
      Printf.bprintf buf "\\<div class=\\\"cs\\\"\\>";
      networks_header buf;
      html_mods_cntr_init ();

      networks_iter_all
        (fun n ->
          if not (List.mem VirtualNetwork n.network_flags) then
            try
              let net_has e = if List.mem e n.network_flags then "yes" else "" in
              let net_has_porttest () =
                match network_porttest_result n with
                  PorttestNotAvailable -> ""
                | _ -> "yes" in
              Printf.bprintf buf "\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr ());
              html_mods_td buf [
                ("", "sr br", n.network_name);
                ("", "sr br", if network_is_enabled n then "Enabled" else "Disabled");
                ("", "sr br", net_has NetworkHasUpload);
                ("", "sr br", net_has NetworkHasServers);
                ("", "sr br", net_has NetworkHasSupernodes);
                ("", "sr br", net_has NetworkHasSearch);
                ("", "sr br", net_has NetworkHasChat);
                ("", "sr br", net_has NetworkHasRooms);
                ("", "sr br", net_has NetworkHasMultinet);
                ("", "sr"   , (net_has_porttest ())); ];
              Printf.bprintf buf "\\</tr\\>";
            with _ -> ()
        );
      Printf.bprintf buf "\\</table\\>\\</div\\>\n";
      html_mods_table_header buf "networkTable" "networkInfo" [];
      Printf.bprintf buf "\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr ());
      html_mods_td buf [
        ("", "sr br",
          "This table prints information about the capabilities of\nMLDonkey network modules, not the networks themselves"); ];
      Printf.bprintf buf "\\</table\\>\\</div\\>\\</div\\>\n"
    end
  else
    begin
      Printf.bprintf buf "Networks:";
      networks_iter_all (fun n ->
          try
            Printf.bprintf buf "\n   %2d %-30s %s" n.network_num n.network_name
                (if network_is_enabled n then "Enabled" else "Disabled")
          with _ -> ())
    end

let print_gdstats o =
  let buf = o.conn_buf in
  let picture_suffix () =
    if !!html_mods_vd_gfx_png then
      Printf.bprintf buf "png\\\"\\>"
    else
      Printf.bprintf buf "jpg\\\"\\>";
  in
  if Autoconf.has_gd then
    (
      if !!html_mods_vd_gfx then
        (
          Printf.bprintf buf "\\<br\\>\\<table class=bw_stats cellpadding=0 cellspacing=0 align=center\\>\\<tr\\>\\<td\\>";
          if !!html_mods_vd_gfx_split then
            begin
              Printf.bprintf buf "\\<img src=\\\"bw_download.";
              picture_suffix ();
              if !!html_mods_vd_gfx_flip then
                Printf.bprintf buf "\\<br\\>";
              Printf.bprintf buf "\\<img src=\\\"bw_upload.";
              picture_suffix ();
            end
          else
            begin
              Printf.bprintf buf "\\<img src=\\\"bw_updown.";
              picture_suffix ();
            end;
          Printf.bprintf buf "\\</td\\>\\</tr\\>\\</table\\>";
          if !!html_mods_vd_gfx_h then
            (
              Printf.bprintf buf "\\<br\\>\\<table class=bw_stats cellpadding=0 cellspacing=0 align=center\\>\\<tr\\>\\<td\\>";
              if !!html_mods_vd_gfx_split then
                begin
                  Printf.bprintf buf "\\<img src=\\\"bw_h_download.";
                  picture_suffix ();
                  if !!html_mods_vd_gfx_flip then
                    Printf.bprintf buf "\\<br\\>";
                  Printf.bprintf buf "\\<img src=\\\"bw_h_upload.";
                  picture_suffix ();
                end
              else
                begin
                  Printf.bprintf buf "\\<img src=\\\"bw_h_updown.";
                  picture_suffix ();
                end;
              Printf.bprintf buf "\\</td\\>\\</tr\\>\\</table\\>";
            );
          if !!html_mods_vd_gfx_tag then
            begin
              Printf.bprintf buf "\\<br\\>\\<br\\>\\<table class=bw_stats cellpadding=0 cellspacing=0 align=center\\>\\<tr\\>\\<td\\>\\<img src=\\\"tag.";
              picture_suffix ();
              Printf.bprintf buf "\\</td\\>\\</tr\\>\\</table\\>";
            end;
        );
    )
  else
    (* fake call if no gd *)
    DriverGraphics.G.do_draw_pic "" "" "" download_history download_history

let buildinfo html buf =
  let tack listref e =
    listref := e :: !listref in
  let list = ref [] in
  tack list
    (
      "Version:\t", 
      "MLNet Multi-Network p2p client version " ^ Autoconf.current_version 
      ^ (match Filename.basename (Sys.executable_name) with
         | "mlnet" | "mlnet.static" -> ""
         | bin -> Printf.sprintf " (%s)" bin)
    );
  if Autoconf.scm_version <> "" then 
    tack list 
      (
        "SCM version:\t", 
        Autoconf.scm_version
      );
  tack list
    (
      "Networks:\t", 
      !networks_string
    );
  tack list 
    (
      "OCaml version:\t", 
      Sys.ocaml_version ^ 
      " - C compiler version: " ^ Autoconf.cc_version ^
      (if Autoconf.cxx_version <> "" then 
        " - C++ compiler version: " ^ Autoconf.cxx_version else "")
    );
  tack list 
    (
      "Built on:\t", 
      Autoconf.build_system ^ " (" ^ Unix2.endianness () ^ ")" ^
      (if Autoconf.glibc_version = "" then "" 
      else
        let real_glibc_version = MlUnix.glibc_version_num () in
        if real_glibc_version = "" || 
          real_glibc_version = Autoconf.glibc_version 
        then " with glibc " ^ Autoconf.glibc_version
        else 
          Printf.sprintf " (Warning: glibc version mismatch, %s present on your system, MlDonkey was compiled with %s)"
            real_glibc_version Autoconf.glibc_version)
    );
  if Autoconf.configure_arguments <> "" then 
    tack list
      (
        "Configure args:\t", 
        Autoconf.configure_arguments 
      );
  if !patches_string <> "" then 
    tack list
      (
        "Patches:\t", 
        !patches_string 
      );
  tack list 
    (
      "Features:\t",
      " threads" ^
      (let s = Zlib2.zlib_version_num () in 
         Printf.sprintf " zlib%s" (if s <> "" then "-" ^ s else "")) ^
      (if Autoconf.bzip2 then
        let s, _ = String2.cut_at (Misc2.bzlib_version_num ()) ',' in
        Printf.sprintf " bzip2%s" (if s <> "" then "-" ^ s else "")
      else " no-bzip2") ^
        (match Autoconf.has_gd, Autoconf.has_gd_png, Autoconf.has_gd_jpg with
        | false, _, _ -> " no-gd"
        | _, true, true -> 
            let s = DriverGraphics.G.png_version_num () in
            Printf.sprintf " gd(jpg/png%s)" (if s <> "" then "-" ^ s else "")
        | _, true, false ->
            let s = DriverGraphics.G.png_version_num () in
            Printf.sprintf " gd(png%s)" (if s <> "" then "-" ^ s else "")
        | _, false, true ->
            " gd(jpg)"
        | _, false, false ->
            " gd(neither jpg nor png ?)") ^
      (match Autoconf.has_iconv, !Charset.Locale.conversion_enabled with
       | true, true -> " iconv(active)"
       | true, false -> " iconv(inactive)"
       | false, _ -> " no-iconv") ^
      (match Autoconf.magic, !Autoconf.magic_works with
       | true, true -> " magic(active)"
       | true, false -> " magic(inactive)"
       | false, _ -> " no-magic") ^
      (if Autoconf.upnp_natpmp then " upnp natpmp" else " no-upnp no-natpmp") ^
      (if Autoconf.check_bounds then " check-bounds" else " no-check-bounds")
    );
  let list = List.rev !list in
     
    if html then
      html_mods_table_header buf "sharesTable" "shares" [
       ( Str, "srh", "core Build information", "Buildinfo" ) ;
       ( Str, "srh", "", "" ) ]
    else
      Printf.bprintf buf "\n\t--Buildinfo--\n";
    html_mods_cntr_init ();
    List.iter (fun (desc, text) ->
      if html then 
        Printf.bprintf buf "\\<tr class=\\\"dl-%d\\\"\\>\\<td class=\\\"sr\\\"\\>%s\\</td\\>\\<td class=\\\"sr\\\"\\>%s\\</td\\>\\</tr\\>"
          (html_mods_cntr ()) desc text
      else
        Printf.bprintf buf "%s %s\n" desc text;
    ) list;
    if html then 
      Printf.bprintf buf "\\</table\\>\\</div\\>"

let runinfo html buf o =
  let bl_loc = Ip_set.bl_length !CommonBlocking.ip_blocking_list in
  let bl_web = Ip_set.bl_length !CommonBlocking.web_ip_blocking_list in

  let tack listref e = 
    listref := e :: !listref in
  let list = ref [] in
  tack list
    (
      "MLDonkey user:\t\t", 
      Printf.sprintf "%s (%s) - uptime: %s%s"
        o.conn_user.ui_user.user_name
        (if has_empty_password o.conn_user.ui_user then "Warning: empty Password"
        else "PW Protected")
        (Date.time_to_string (last_time () - start_time) "verbose")
        (
        let user_group =
          (try Some (Unix.getpwuid (Unix.getuid())).Unix.pw_name with _ -> None),
          (try Some (Unix.getgrgid (Unix.getgid())).Unix.gr_name with _ -> None)
        in
        match user_group with
        | (Some g, Some u) -> Printf.sprintf " - running as %s:%s" u g
        | _ -> ""
        )
    );
  tack list
    (
      "Enabled nets:\t", 
      List.fold_left (fun acc (c, s) -> 
        if c then Printf.sprintf "%s %s" acc s else acc) ""
        [(Autoconf.donkey = "yes" && !!enable_donkey, "Donkey");
         (Autoconf.donkey = "yes" && !!enable_overnet, "Overnet");
         (Autoconf.donkey = "yes" && !!enable_kademlia, "Kademlia");
         (Autoconf.bittorrent = "yes" && !!enable_bittorrent, "BitTorrent");
         (Autoconf.direct_connect = "yes" && !!enable_directconnect, "DirectConnect");
         (Autoconf.fasttrack = "yes" && !!enable_fasttrack, "Fasttrack");
         (Autoconf.gnutella = "yes" && !!enable_gnutella, "Gnutella");
         (Autoconf.gnutella2 = "yes" && !!enable_gnutella2, "G2");
         (Autoconf.filetp = "yes" && !!enable_fileTP, "FileTP")]
    );
  tack list
    (
      "Server usage:\t", 
      if !!enable_servers then "enabled" 
      else "disabled (you are not able to connect to ED2K Servers)"
    );
  tack list
    (
      "Geoip:\t\t", 
      if Geoip.active () then "enabled, GeoLite data created by MaxMind, available from http://maxmind.com/"
      else "disabled, to enable adjust web_infos in downloads.ini for automatic download"
    );
  tack list
    ( 
      "IP blocking:\t",
      if bl_loc = 0 && bl_web = 0 then "no blocking list loaded"
      else Printf.sprintf "local: %d ranges - web: %d ranges" bl_loc bl_web
    );
  if not !dns_works then 
    tack list
      (
        "DNS:\t\t",
        Printf.sprintf "DNS resolution not available, web_infos %s not work"
          (if Autoconf.bittorrent = "yes" then "and BT does" else "do")
      );
  if Autoconf.magic then
    tack list
      (
        "Libmagic:\t",
        Printf.sprintf "file-type recognition database%s present"
          (if !Autoconf.magic_works then "" else " not")
      );
  tack list
    (
      "System info:\t",
      let uname = Unix32.uname () in
      if uname <> "" then 
        uname ^
        (if not (Unix32.os_supported ()) then 
          " - \nWARNING:\t not supported operating system" else "")
      else "unknown"
    );
  tack list
    ( 
      "",
      Printf.sprintf "\t\t language: %s - locale: %s - UTC offset: %s"
        Charset.Locale.default_language
        Charset.Locale.locale_string
        (Rss_date.mk_timezone (Unix.time ()))
    );
  tack list
    (
      "", 
      Printf.sprintf "\t\t max_string_length: %d - word_size: %d - max_array_length: %d - max_int: %d"
        Sys.max_string_length
        Sys.word_size
        Sys.max_array_length
        Pervasives.max_int
    );
  tack list
    (
      "", 
      Printf.sprintf "\t\t max file descriptors: %d - max useable file size: %s" 
        (Unix2.c_getdtablesize ())
        (match Unix2.c_sizeofoff_t () with
         | 4 -> "2GB"
         |  _ -> 
              Printf.sprintf "2^%d-1 bits (do the maths ;-p)"
                ((Unix2.c_sizeofoff_t () * 8)-1))
    );
  let list = List.rev !list in

    if html then
      html_mods_table_header buf "sharesTable" "shares" [
       ( Str, "srh", "core runtime information", "Runinfo" ) ;
       ( Str, "srh", "", "" ) ]
    else
      Printf.bprintf buf "\n\t--Runinfo--\n";
    html_mods_cntr_init ();
    List.iter (fun (desc, text) ->
      if html then 
        Printf.bprintf buf "\\<tr class=\\\"dl-%d\\\"\\>\\<td class=\\\"sr\\\"\\>%s\\</td\\>\\<td class=\\\"sr\\\"\\>%s\\</td\\>\\</tr\\>"
          (html_mods_cntr ()) desc text
      else
        Printf.bprintf buf "%s %s\n" desc text;
    ) list;
    if html then 
      Printf.bprintf buf "\\</table\\>\\</div\\>"

type port_info = {
    netname : string;
    port : int;
    portname : string
  }

let portinfo html buf =
  let network_name_list_width = ref 7 in (* "Network" *)
  let list = ref [] in
  networks_iter (fun r ->
    if String.length r.network_name > !network_name_list_width then
      network_name_list_width := String.length r.network_name;
    List.iter (fun (p,s) -> if p <> 0 then list := !list @
      [{netname = r.network_name; port = p; portname = s}]) (network_ports r)
  );
  List.iter (fun (p,s) -> if p <> 0 then list := !list @
      [{netname = "Core"; port = p; portname = s}])
    (network_ports (network_find_by_name "Global Shares"));

  let fill_network s = String.make (max 0 (!network_name_list_width - 7)) s in
  if html then
      html_mods_table_header buf "sharesTable" "shares" [
       ( Str, "srh", "Network", "Network" ) ;
       ( Num, "srh ar", "Port", "Port" ) ;
       ( Str, "srh", "Type", "Type" ) ]
  else
    begin
      Printf.bprintf buf "\n\t--Portinfo--\n";
      Printf.bprintf buf "Network%s|  Port|Type\n" (fill_network ' ');
      Printf.bprintf buf "-------%s+------+-------------------\n" (fill_network '-')
    end;

  html_mods_cntr_init ();
  List.iter (fun p ->
    if html then
      Printf.bprintf buf "\\<tr class=\\\"dl-%d\\\"\\>\\<td class=\\\"sr\\\"\\>%s\\</td\\>\\<td class=\\\"sr ar\\\"\\>%d\\</td\\>\\<td class=\\\"sr\\\"\\>%s\\</td\\>"
        (html_mods_cntr ()) p.netname p.port p.portname
    else
      Printf.bprintf buf "%-*s|%6d|%s\n"
        (max !network_name_list_width (!network_name_list_width - String.length p.netname)) p.netname p.port p.portname
    ) (List.sort (fun p1 p2 -> String.compare p1.netname p2.netname) !list);
  if html then
    Printf.bprintf buf "\\</table\\>\\</td\\>\\<tr\\>\\</table\\>\\</div\\>"

let diskinfo html buf =
  let list = ref [] in
  ignore (search_incoming_files ());
  ignore (search_incoming_directories ());
  list := (Filename2.temp_dir_name (), "$MLDONKEY_TEMP") :: !list;
  List.iter (fun dir ->
     list := (dir.shdir_dirname, (Printf.sprintf "shared (%s)" dir.shdir_strategy))
     :: !list) !!shared_directories;
  list := (!!temp_directory, "temp/downloading") :: !list;
  list := (Sys.getcwd (), "core/ini files") :: !list;

  let len_dir = ref 9 in
  let len_strategy = ref 29 in (* "shared (incoming_directories)" *)
  List.iter ( fun (dir, strategy) ->
    len_dir := max !len_dir (String.length dir);
    len_strategy := max !len_strategy (String.length strategy)
  ) !list;
  let fill_dir = String.make (!len_dir - 9) ' ' in
  let fill_dir_line = String.make (!len_dir - 9) '-' in
  let fill_strategy = String.make (!len_strategy - 4) ' ' in
  let fill_strategy_line = String.make (!len_strategy - 4) '-' in
  html_mods_cntr_init ();
  if html then
      html_mods_table_header buf "sharesTable" "shares" [
       ( Str, "srh", "Directory", "Directory" ) ;
       ( Str, "srh", "Directory type", "Type" ) ;
       ( Num, "srh ar", "HDD used", "used" ) ;
       ( Num, "srh ar", "HDD free", "free" ) ;
       ( Num, "srh ar", "% free", "% free" ) ;
       ( Str, "srh", "Filesystem", "FS" ) ]
  else
    begin
      Printf.bprintf buf "\n\t--Diskinfo--\n";
      Printf.bprintf buf "Directory%s|Type%s|    used|    free|%%free|Filesystem\n"
        fill_dir fill_strategy;
      Printf.bprintf buf "---------%s+----%s+--------+--------+-----+----------\n"
        fill_dir_line fill_strategy_line;
    end;
  List.iter (fun (dir, strategy) ->
        let diskused =
          match Unix32.diskused dir with
          | None -> Printf.sprintf "---"
          | Some du -> size_of_int64 du
        in
        let diskfree =
          match Unix32.diskfree dir with
          | None -> Printf.sprintf "---"
          | Some df -> size_of_int64 df
        in
        let percentfree =
          match Unix32.percentfree dir with
          | None -> Printf.sprintf "---"
          | Some p -> Printf.sprintf "%d%%" p
        in
        let filesystem = Unix32.filesystem dir in
        if html then
            Printf.bprintf buf "\\<tr class=\\\"dl-%d\\\"\\>\\<td class=\\\"sr\\\"\\>%s\\</td\\>\\<td class=\\\"sr\\\"\\>%s\\</td\\>
              \\<td class=\\\"sr ar\\\"\\>%s\\</td\\>\\<td class=\\\"sr ar\\\"\\>%s\\</td\\>
              \\<td class=\\\"sr ar\\\"\\>%s\\</td\\>\\<td class=\\\"sr\\\"\\>%s\\</td\\>\\</tr\\>"
            (html_mods_cntr ()) dir strategy diskused diskfree percentfree filesystem
        else
          Printf.bprintf buf "%-*s|%-*s|%8s|%8s|%5s|%-s\n"
            (max !len_dir (!len_dir - String.length dir)) dir
            (max !len_strategy (!len_strategy - String.length strategy)) strategy
            diskused diskfree percentfree filesystem
        ) !list;
  if html then
    Printf.bprintf buf "\\</table\\>\\</td\\>\\<tr\\>\\</table\\>\\</div\\>"

let print_option_help o option =
  let buf = o.conn_buf in
  let help_text = get_help option in
  if use_html_mods o then
    begin
      Printf.bprintf buf "\\<div class=\\\"cs\\\"\\>";
      html_mods_table_header buf "versionTable" "results" [];
      Printf.bprintf buf "\\<tr\\>";
      html_mods_td buf [ ("", "srh", "Helptext"); ];
      Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-1\\\"\\>";
      html_mods_td buf [ ("", "sr", Str.global_replace (Str.regexp "\n") "\\<br\\>" help_text); ];
      Printf.bprintf buf "\\</tr\\>\\</table\\>\\</div\\>\\</div\\>";
    end
  else
    Printf.bprintf buf "\n\t--Helptext--\n%s\n" help_text

let dllink_print_result html url header results =
  let buf = Buffer.create 100 in
  if html then
    begin
      Printf.bprintf buf "\\<div class=\\\"cs\\\"\\>";
      html_mods_table_header buf "dllinkTable" "results" [];
      Printf.bprintf buf "\\<tr\\>";
      html_mods_td buf [ ("", "srh", header); ];
      Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-1\\\"\\>";
      html_mods_td buf [ ("", "sr", url); ]
    end
  else
    Printf.bprintf buf "%s : %s\n" header url;
  List.iter (fun s ->
    if html then
      begin
        Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-1\\\"\\>";
        html_mods_td buf [ ("", "sr", s); ]
      end
    else
      Printf.bprintf buf "%s\n" s) (List.rev results);
  if html then Printf.bprintf buf "\\</tr\\>\\</table\\>\\</div\\>\\</div\\>";
  Buffer.contents buf

let dllink_query_networks html url user group =
  let result = ref [] in
  if not (networks_iter_until_true (fun n ->
    try
      let s,r = network_parse_url n url user group in
        if s = "" then
          r
        else
          let s1 = Printf.sprintf "%s: %s" n.network_name s in
            result := s1 :: !result;
            r
    with e ->
      let s1 = Printf.sprintf "%s: Exception %s"
        (n.network_name) (Printexc2.to_string e)
      in
        result := s1 :: !result;
        false
  )) then
    dllink_print_result html url "Unable to match URL" !result
  else
    dllink_print_result html url "Added link" !result

let dllink_parse html url user =
  if (String2.starts_with url "http") then (
    let u = Url.of_string url in
    let module H = Http_client in
    let r = {
      H.basic_request with
      H.req_url =  u;
      H.req_proxy = !CommonOptions.http_proxy;
      H.req_request = H.HEAD;
      H.req_max_retry = 10;
      H.req_referer = (
        let (rule_search,rule_value) =
        try (List.find(fun (rule_search,rule_value) ->
          Str.string_match (Str.regexp rule_search) u.Url.server 0
        ) !!referers )
        with Not_found -> ("",Url.to_string u) in
        Some (Url.of_string rule_value) );
      H.req_headers = (try
        let cookies = List.assoc u.Url.server !!cookies in
          [ ( "Cookie", List.fold_left (fun res (key, value) ->
              if res = "" then
                key ^ "=" ^ value
              else
                res ^ "; " ^ key ^ "=" ^ value
            ) "" cookies
            ) ]
         with Not_found -> []);
      H.req_user_agent = get_user_agent ();
    } in
    H.whead r (fun headers ->
      (* Combine the list of header fields into one string *)
      let concat_headers =
        (List.fold_right (fun (n, c) t -> n ^ ": " ^ c ^ "\n" ^ t) headers "")
      in
      ignore (dllink_query_networks html concat_headers user user.user_default_group)
    );
    dllink_print_result html url "Parsing HTTP url" [])
  else
    if (String2.starts_with url "ftp") then
      dllink_query_networks html (Printf.sprintf "Location: %s" url) user user.user_default_group
    else
      dllink_query_networks html url user user.user_default_group

module UnionFind = struct
  type t = int array
  let create_sets n =
    Array.init n (fun i -> i) (* each element is its own leader *)
  let find_leader t i =
    let rec fix_point i =
      let parent = t.(i) in
      if parent <> i then fix_point parent 
      else i in
    let leader = fix_point i in
    t.(i) <- leader;
    leader
  let merge_sets t i j =
    let leaderi = find_leader t i in
    let leaderj = find_leader t j in
    t.(leaderi) <- leaderj
  let number_of_sets t =
    let nsets = ref 0 in
    Array.iteri (fun i ti ->
      if i = ti then incr nsets) t;
    !nsets
end

let filenames_variability o list =
  (* over this number of filenames, exact variability is not computed
     (too expensive) *)
  let bypass_threshold = 100 in
  (* minimum distance that must exist between two groups of filenames
     so they're considered separate *)
  let gap_threshold = 4 in

  let buf = o.conn_buf in

  let is_alphanum = function
    | 'A' .. 'Z'
    | 'a' .. 'z'
    | '0' .. '9' -> true
    | _ -> false in

  let canonized_words s =
    let len = String.length s in
    let current_word = Buffer.create len in
    let rec outside_word i wl =
      if i < len then
        if not (is_alphanum s.[i]) then outside_word (i + 1) wl
        else begin (* start of a new word *)
          Buffer.add_char current_word (Char.lowercase s.[i]);
          inside_word (i + 1) wl
        end
      else wl
    and inside_word i wl =
      if i < len then 
        if not (is_alphanum s.[i]) then begin (* end of the word *)
          let wl = Buffer.contents current_word :: wl in
          Buffer.reset current_word;
          outside_word i wl
        end else begin
          Buffer.add_char current_word (Char.lowercase s.[i]);
          inside_word (i + 1) wl
        end
      else Buffer.contents current_word :: wl
    in
    outside_word 0 [] in

  let costs = { 
    Levenshtein.insert_cost = 1; 
    Levenshtein.delete_cost = 1; 
    Levenshtein.replace_cost = 2 } in
  (* we can only assume the distance is symetric if insert and
     delete costs are the same *)
  assert (costs.Levenshtein.insert_cost = costs.Levenshtein.delete_cost);
  let dist = Levenshtein.ForWords.distance costs in

  let score_list =
    List.map (fun fileinfo ->
      (* canonize filenames by keeping only lowercase words, and
         sorting them so that initial order doesn't matter;
         Remove duplicate canonized filenames *)
      let fns = Array.of_list (List.fold_left (fun acc fn ->
        let new_fn =
          Array.of_list (List.sort String.compare (canonized_words fn)) in
        if List.mem new_fn acc then acc else new_fn :: acc
      ) [] fileinfo.file_names) in

      let nfilenames = Array.length fns in
      if nfilenames > bypass_threshold then
        fileinfo, bypass_threshold
      else
        let unionfind_sets = UnionFind.create_sets nfilenames in
        for i = 0 to nfilenames - 2 do
          let d1 = dist fns.(i) in
          for j = i + 1 to nfilenames - 1 do
            if d1 fns.(j) < gap_threshold then
              UnionFind.merge_sets unionfind_sets i j
          done
        done;
        fileinfo, UnionFind.number_of_sets unionfind_sets
    ) list in

  (* files with most clusters at the end of results table *)
  let sorted_score_list =
    List.sort (fun (_, nc1) (_, nc2) -> compare nc1 nc2)
      score_list in

  let print_table = if o.conn_output = HTML then print_table_html 2
    else print_table_text in
  print_table buf
    [|
      Align_Left; Align_Left; Align_Right |]
    [| 
      "Num";
      "File";
      "Clusters" |]
    (List.map (fun (fileinfo, nc) ->
      let n = network_find_by_num fileinfo.file_network in
      [| 
        Printf.sprintf "[%-s %5d]" n.network_name (fileinfo.file_num);
        shorten fileinfo.file_name 80;
        string_of_int nc |]
    ) sorted_score_list)

let print_upstats o list server =
  let buf = o.conn_buf in
  if use_html_mods o then
    begin
      if !!html_mods_use_js_tooltips then Printf.bprintf buf 
"\\<div id=\\\"object1\\\" style=\\\"position:absolute; background-color:#FFFFDD;color:black;border-color:black;border-width:20px;font-size:8pt; visibility:visible; left:25px; top:
-100px; z-index:+1\\\" onmouseover=\\\"overdiv=1;\\\"  onmouseout=\\\"overdiv=0; setTimeout(\\\'hideLayer()\\\',1000)\\\"\\>\\&nbsp;\\</div\\>";

      Printf.bprintf buf "\\<div class=\\\"upstats\\\"\\>";
      match server with
        None ->
          html_mods_table_one_row buf "upstatsTable" "upstats" [
            ("", "srh", Printf.sprintf "Session: %s uploaded | Shared(%d): %s\n"
            (size_of_int64 !upload_counter) !nshared_files (size_of_int64 !nshared_bytes)); ]
      | Some s -> let info = server_info s in
          html_mods_table_one_row buf "upstatsTable" "upstats" [
            ("", "srh", Printf.sprintf "%d files shared on %s (%s:%s)"
              info.G.server_published_files info.G.server_name
              (Ip.string_of_addr info.G.server_addr)
              (string_of_int info.G.server_port)); ]
    end
  else
    begin
      Printf.bprintf buf "Upload statistics:\n";
      Printf.bprintf buf "Session: %s uploaded | Shared(%d): %s\n"
        (size_of_int64 !upload_counter) !nshared_files (size_of_int64 !nshared_bytes)
    end;

  if use_html_mods o then
    html_mods_table_header buf "upstatsTable" "upstats" [
      ( Num, "srh", "Total file requests", "Reqs" ) ;
      ( Num, "srh", "Total bytes sent", "Total" ) ;
      ( Num, "srh", "Upload Ratio", "UPRatio" ) ;
      ( Str, "srh", "Preview", "P" ) ;
      ( Str, "srh", "Filename", "Filename" );
      ( Str, "srh", "Statistic links", "Stats" );
      ( Str, "srh", "Published on servers", "Publ" );
      ( Str, "srh", "Share status", "Status" )
    ]
  else
    begin
      Printf.bprintf buf " Requests |  Bytes   | Uploaded | File\n";
      Printf.bprintf buf "----------+----------+----------+----------------------------------------------------\n";
    end;

  html_mods_cntr_init ();
  let list = List.sort (fun f1 f2 ->
    let c = compare f2.impl_shared_requests f1.impl_shared_requests in
    if c <> 0 then c else
    compare f2.impl_shared_uploaded f1.impl_shared_uploaded
    ) list in

  List.iter (fun impl ->
    if use_html_mods o then
      begin
        let published = List.length impl.impl_shared_servers in
        let ed2k = file_print_ed2k_link
          (Filename.basename impl.impl_shared_codedname)
          impl.impl_shared_size impl.impl_shared_id in

        Printf.bprintf buf "\\<tr class=\\\"dl-%d\\\"" (html_mods_cntr ());
        (if !!html_mods_use_js_tooltips then
           Printf.bprintf buf " onMouseOver=\\\"mOvr(this);setTimeout('popLayer(\\\\\'%s<br>%s%s\\\\\')',%d);setTimeout('hideLayer()',%d);return true;\\\" onMouseOut=\\\"mOut(this);hideLayer();setTimeout('hideLayer()',%d)\\\"\\>"
            (Http_server.html_real_escaped (Filename.basename (Charset.Locale.to_utf8 impl.impl_shared_codedname)))
            (match impl.impl_shared_file with
               None -> "no file info"
             | Some file -> match file_magic file with | None -> "no magic"
             | Some magic -> "File type: " ^ (Http_server.html_real_escaped magic) ^ "<br>")
            (if impl.impl_shared_servers = [] then "" else
               Printf.sprintf "<br>Published on %d %s<br>%s"
                published (if published = 1 then "server" else "servers")
                (let listbuf = Buffer.create 100 in
                   List.iter (fun s -> let info = server_info s in
                     Printf.bprintf listbuf "%s (%s:%s%s)<br>"
                        info.server_name
                        (Ip.string_of_addr info.server_addr)
                        (string_of_int info.server_port)
                        (if info.server_realport <> 0
                           then "(" ^ (string_of_int info.server_realport) ^ ")" else "")
                        ) impl.impl_shared_servers;
                 Buffer.contents listbuf))
                !!html_mods_js_tooltips_wait
                !!html_mods_js_tooltips_timeout
                !!html_mods_js_tooltips_wait
         else Printf.bprintf buf " onMouseOver=\\\"mOvr(this);return true;\\\" onMouseOut=\\\"mOut(this);\\\"\\>");

        let uploaded = Int64.to_float impl.impl_shared_uploaded in
        let size = Int64.to_float impl.impl_shared_size in
        html_mods_td buf [
          ("", "sr ar", Printf.sprintf "%d" impl.impl_shared_requests);
          ("", "sr ar", size_of_int64 impl.impl_shared_uploaded);
          ("", "sr ar", Printf.sprintf "%5.1f" ( if size < 1.0 then 0.0 else (uploaded *. 100.) /. size));
          ("", "sr", Printf.sprintf "\\<a href=\\\"preview_upload?q=%d\\\"\\>P\\</a\\>" impl.impl_shared_num);
          ("", "sr", (if impl.impl_shared_id = Md4.null then
                        (shorten (Filename.basename impl.impl_shared_codedname) !!max_name_len)
                      else
                        Printf.sprintf "\\<a href=\\\"%s\\\"\\>%s\\</a\\>"
                          ed2k (shorten (Filename.basename impl.impl_shared_codedname) !!max_name_len)));
          ("", "sr", (if impl.impl_shared_id = Md4.null then "" else
                        Printf.sprintf "\\<a href=\\\"http://tothbenedek.hu/ed2kstats/ed2k?hash=%s\\\"\\>%s\\</a\\>
\\<a href=\\\"http://ed2k.titanesel.ws/ed2k.php?hash=%s\\\"\\>%s\\</a\\>
\\<a href=\\\"http://bitzi.com/lookup/ed2k:%s\\\"\\>%s\\</a\\>"
                      (Md4.to_string impl.impl_shared_id) "T1"
                      (Md4.to_string impl.impl_shared_id) "T2"
                      (Md4.to_string impl.impl_shared_id) "B"));
          ("", "sr ar", Printf.sprintf "%d" published);
          ("", "sr", shared_state (as_shared impl) o);
        ];
        Printf.bprintf buf "\\</tr\\>\n";
      end
    else
      Printf.bprintf buf "%9d | %8s | %7s%% | %-50s\n"
        (impl.impl_shared_requests)
        (size_of_int64 impl.impl_shared_uploaded)
        (Printf.sprintf "%3.1f" ((Int64.to_float impl.impl_shared_uploaded *. 100.) /. Int64.to_float impl.impl_shared_size))
        (shorten (Filename.basename impl.impl_shared_codedname) !!max_name_len)
  ) list;

  if use_html_mods o then Printf.bprintf buf "\\</table\\>\\</div\\>\\</div\\>"
