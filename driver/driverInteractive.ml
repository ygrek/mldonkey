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

open CommonClient
open CommonNetwork
open CommonResult
open CommonFile
open CommonComplexOptions
open CommonInteractive
open GuiTypes
open CommonNetwork
open Options
open BasicSocket
open TcpBufferedSocket
open CommonTypes
open CommonGlobals
open CommonOptions
open CommonTypes

   
(* ripped from gui_downloads *)

let time_to_string time =
  let days = time / 60 / 60 / 24 in
  let rest = time - days * 60 * 60 * 24 in
  let hours = rest / 60 / 60 in
  let rest = rest - hours * 60 * 60 in
  let minutes = rest / 60 in
  let seconds = rest - minutes * 60 in
    if days > 0
    then Printf.sprintf " %dd " days
    else if hours > 0
    then Printf.sprintf " %dh %dm " hours minutes 
    else Printf.sprintf " %dm " minutes 

let calc_file_eta f =
  let size = Int64.to_float f.file_size in
  let downloaded = Int64.to_float f.file_downloaded in
  let missing = size -. downloaded in
  let rate = f.file_download_rate in
  let hundays = 1000.0 *. 60.0 *. 60.0 *. 24.0 in
  match f.file_state with
        FilePaused -> int_of_float (hundays +. 2.)
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
  let rec loop i p n =
    if i < 0
    then 
      if n < 0.0001
      then "-"
      else Printf.sprintf "%5.1f" (p /. n *. 100.0)
    else
      if f.file_chunks.[i] = '0'
      then
    if f.file_availability.[i] <> (char_of_int 0)
    then loop (i - 1) (p +. 1.0) (n +. 1.0)
    else loop (i - 1) p (n +. 1.0)
      else loop (i - 1) p n
  in  
    loop ((String.length f.file_availability) - 1) 0.0 0.0

let string_availability s =
  let len = String.length s in
  let p = ref 0 in
  for i = 0 to len - 1 do
    if int_of_char s.[i] <> 0 then begin
        incr p
      end
  done;
  if len = 0 then "" else 
    Printf.sprintf "%5.1f" (float_of_int !p /. float_of_int len *. 100.)
    
  

  
  
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
  
let save_config () =
  Options.save_with_help downloads_ini;
  networks_iter (fun r -> 
      match r.network_config_file with
        None -> ()
      | Some opfile -> Options.save_with_help opfile);
  CommonComplexOptions.save ();

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
  let name = file.file_name in
  let len = String.length name in
  let max_name_len = maxi !!max_name_len 10 in
  if len > max_name_len then
    let prefix = String.sub name 0 (max_name_len -7) in
    let suffix = String.sub name (len-4) 4 in
    Printf.sprintf "%s...%s" prefix suffix
  else name

type table_align = 
  Align_Left
| Align_Right
| Align_Center

let col_sep = "  "
let add buf s align max_len =
  let slen = String.length s in
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
  let cols = Array.create ncols 0 in
  List.iter (fun line ->
      let len = Array.length line in
      for i = 0 to len-1 do 
        let slen = String.length line.(i) in
        if cols.(i) <  slen then cols.(i) <- slen
      done;
  ) (titles :: lines);
  Array.iteri (fun i s -> 
      add buf s Align_Center cols.(i);
      Buffer.add_string buf col_sep;
  ) titles;
  Buffer.add_char buf '\n';
  let aligns = Array.create ncols Align_Center in
  Array.iteri (fun i al -> aligns.(i) <- al) alignments; 
  List.iter (fun line ->
      Array.iteri (fun i s ->
          add buf s aligns.(i) cols.(i);
      Buffer.add_string buf col_sep;
      ) line;
      Buffer.add_char buf '\n';      
  ) lines

   
let print_table_html_mods buf lines =
  
  let counter = ref 0 in
  
  List.iter (fun line ->
      if (!counter mod 2 == 0) then Printf.bprintf buf "\\<TR class=dl-1"
      else Printf.bprintf buf "\\<TR class=dl-2";
      incr counter;
      
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
      Printf.bprintf buf "\\<TD ALIGN=CENTER\\>%s\\</TD\\>" title;
      Printf.bprintf buf "\\<TD WIDTH=%d\\> \\</TD\\>" spacing;
  ) titles;
  let naligns = Array.length aligns in
  Html.end_tr buf;

  List.iter (fun line ->
      Html.begin_tr buf;
      Array.iteri (fun i title ->
          Printf.bprintf buf "\\<TD%s nowrap\\>%s\\</TD\\>" 
            (if i >= naligns then "" else
            match aligns.(i) with
              Align_Center -> " ALIGN=CENTER"
            | Align_Left -> " ALIGN=LEFT"
            | Align_Right -> " ALIGN=RIGHT")
          title;
          Printf.bprintf buf "\\<TD WIDTH=%d\\> \\</TD\\>" spacing;
      ) line;
      Html.end_tr buf;
  ) lines;
  Html.end_table buf
  
let print_file_html_form buf files =
  
  
  Printf.bprintf buf "
\\<script language=JavaScript\\>\\<!--
function pauseAll(x){for(i=0;i\\<document.selectForm.elements.length;i++){var j=document.selectForm.elements[i];if (j.name==\\\"pause\\\") {j.checked=x;}}}
function resumeAll(x){for(i=0;i\\<document.selectForm.elements.length;i++){var j=document.selectForm.elements[i];if (j.name==\\\"resume\\\") {j.checked=x;}}}
function cancelAll(x){for(i=0;i\\<document.selectForm.elements.length;i++){var j=document.selectForm.elements[i];if (j.name==\\\"cancel\\\") {j.checked=x;}}}
  function clearAll(x){for(i=0;i\\<document.selectForm.elements.length;i++){var j=document.selectForm.elements[i];if (j.type==\\\"checkbox\\\") {j.checked=x;}}}//--\\>\\</script\\>
  ";
  
  Printf.bprintf buf "\\<form name=selectForm action=/files\\>";
  
  
  Html.begin_table_option  buf "width=100%";
  
  Html.begin_td_option buf "width=50%";
  Printf.bprintf buf "\\<input type=submit value='Submit Changes'\\>";
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
    [| Align_Left; Align_Left; Align_Left; Align_Right; Align_Right; Align_Right; Align_Right|] 
    [|
    "[ Num ]"; 
    "P/R/C";
    "\\<input type=radio value=File name=sortby\\> File"; 
    "\\<input type=radio value=Percent name=sortby\\> Percent"; 
    "\\<input type=radio value=Downloaded name=sortby\\> Downloaded"; 
    "\\<input type=radio value=Size name=sortby\\> Size"; 
    "Old"; 
    "\\<input type=radio value=Rate name=sortby\\> Rate"; 
  |] 
    (List.map (fun file ->
        [|
          (Printf.sprintf "[\\<a href=/submit\\?q\\=vd+%d\\>%-5d\\</a\\> \\<a href=http://edonkeyfakes.ath.cx/fakecheck/update/fakecheck.php\\?size\\=%s\\&md4=%s\\>%s\\</a\\>]" 
              file.file_num
              file.file_num
              (Int64.to_string file.file_size)
            (Md4.to_string file.file_md4)
            (let n = network_find_by_num file.file_network in
              n.network_name)            
          );
          

          (if file.file_state = FileDownloading then
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
\\<td class=loaded width=%d%%\\>\\&nbsp;\\</td\\>
\\<td class=remain width=%d%%\\>\\&nbsp;\\</td\\>
\\</tr\\>\\</table\\>"
            (short_name file)
            (truncate (downloaded /. size *. 100.))
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
              FilePaused ->  "Paused"
            | FileAborted s -> Printf.sprintf "Aborted %s" s
            | _ -> 
            if file.file_download_rate < 10.24 then
              "-"
            else
              Printf.sprintf "%5.1f" (file.file_download_rate /. 1024.));
        |]
    ) files);
  Printf.bprintf buf "\\</form\\>"

  
  let print_file_html_mods buf guifiles =
  
    if (List.length guifiles) > 0 then begin
        
  Printf.bprintf buf "\\</pre\\>
\\<script language=JavaScript\\>\\<!--
function pauseAll(x){for(i=0;i\\<document.selectForm.elements.length;i++){var j=document.selectForm.elements[i];if (j.name==\\\"pause\\\") {j.checked=x;}}}
function resumeAll(x){for(i=0;i\\<document.selectForm.elements.length;i++){var j=document.selectForm.elements[i];if (j.name==\\\"resume\\\") {j.checked=x;}}}
function cancelAll(x){for(i=0;i\\<document.selectForm.elements.length;i++){var j=document.selectForm.elements[i];if (j.name==\\\"cancel\\\") {j.checked=x;}}}
function clearAll(x){for(i=0;i\\<document.selectForm.elements.length;i++){var j=document.selectForm.elements[i];if (j.type==\\\"checkbox\\\") {j.checked=x;}}}
//--\\>\\</script\\>

\\<div class=main\\>
\\<form name=selectForm action=/files\\>
\\<table class=main cellspacing=0 cellpadding=0\\> 

\\<tr\\>\\<td\\>

\\<table cellspacing=0  cellpadding=0  width=100%%\\>\\<tr\\>
\\<td class=downloaded width=100%%\\>Downloading %d files\\</td\\>

\\<td class=big\\>\\<input class=bigbutton type=\\\"button\\\" value=\\\"Pause all\\\" onclick=\\\"pauseAll(true);\\\"\\>\\</td\\>
\\<td class=big\\>\\<input class=bigbutton type=\\\"button\\\" value=\\\"Resume all\\\" onclick=\\\"resumeAll(true);\\\"\\>\\</td\\>
\\<td class=big\\>\\<input class=bigbutton type=\\\"button\\\" value=\\\"Clear all\\\" onclick=\\\"clearAll(false);\\\"\\>\\</td\\>
\\<td class=\\\"big pr\\\"\\>\\<input class=bigbutton type=submit value='Submit Changes'\\>\\</td\\>
\\</tr\\>\\</table\\>

\\</td\\>\\</tr\\>
\\<tr\\>\\<td\\>

\\<table class=downloaders cellspacing=0 cellpadding=0\\>\\<tr\\>

\\<TD title=\\\"Pause/Resume/Cancel\\\" class=\\\"dlheader\\\"\\>P/R/C\\</TD\\>
\\<TD title=\\\"Sort by Filename\\\" class=dlheader\\>\\<input class=headbutton type=submit value=File name=sortby\\>\\</TD\\>
\\<TD title=\\\"Sort by Size\\\" class=dlheader\\>\\<input class=headbutton type=submit value=Size name=sortby\\>\\</TD\\>
\\<TD title=\\\"Sort by Bytes Downloaded\\\" class=dlheader\\>\\<input class=\\\"headbutton ar\\\" type=submit value=DLed name=sortby\\>\\</TD\\>
\\<TD title=\\\"Sort by Percent\\\" class=dlheader\\>\\<input class=headbutton type=submit value=%% name=sortby\\>\\</TD\\>
\\<TD title=\\\"Number of Sources\\\" class=\\\"dlheader ar\\\"\\>Srcs\\</TD\\>
\\<TD title=\\\"File Availability Percentage (Using %s Availability)\\\" class=\\\"dlheader ac\\\"\\>Avail\\</TD\\>
" (List.length guifiles)
  (if !!html_mods_use_relative_availability then "Relative"
                                            else "Total")
  ;

if !!html_mods_vd_age then Printf.bprintf buf "
\\<TD title=\\\"Sort by Age of Download\\\" class=dlheader\\>\\<input style=\\\"padding-left: 0px; padding-right: 0px;\\\" class=headbutton type=submit value=Age name=sortby\\>\\</TD\\>
";

if !!html_mods_vd_last then Printf.bprintf buf "
\\<TD title=\\\"Sort by Last Seen Complete\\\" class=dlheader\\>\\<input style=\\\"padding-left: 0px; padding-right: 0px;\\\" class=headbutton type=submit value=Last name=sortby\\>\\</TD\\>
";

Printf.bprintf buf "
\\<TD title=\\\"Sort by Rate\\\" class=dlheader\\>\\<input style=\\\"padding-left: 0px; padding-right: 0px;\\\" class=headbutton type=submit value=Rate name=sortby\\>\\</TD\\>
\\<TD title=\\\"Sort by ETA\\\" class=dlheader\\>\\<input style=\\\"padding-left: 0px; padding-right: 0px;\\\" class=headbutton type=submit value=ETA name=sortby\\>\\</TD\\>
\\</tr\\>";

  print_table_html_mods buf 
    (List.map (fun file ->
        [|


          (if file.file_state = FileDownloading then
              Printf.sprintf "
				onMouseOver=\\\"mOvr(this);return true;\\\" onMouseOut=\\\"mOut(this,this.bgColor);\\\"\\>
                \\<TD class=\\\"dl al\\\"\\>\\<input class=checkbox name=pause type=checkbox value=%d\\> R
                \\<input class=checkbox name=cancel type=checkbox value=%d\\>\\</td\\>"
                file.file_num
                file.file_num
            else 
              Printf.sprintf "
				onMouseOver=\\\"mOvr(this);return true;\\\" onMouseOut=\\\"mOut(this,this.bgColor);\\\"\\>
                \\<TD class=\\\"dl al\\\"\\>P
                \\<input class=checkbox name=resume type=checkbox value=%d\\>
                \\<input class=checkbox name=cancel type=checkbox value=%d\\>\\</td\\>"
                file.file_num
                file.file_num);
          
          ( let size = Int64.to_float file.file_size in
            let downloaded = Int64.to_float file.file_downloaded in
            let size = if size < 1. then 1. else size in
            Printf.sprintf "\\<TD onClick=\\\"location.href='/submit?q=vd+%d';return true;\\\" 
title=\\\"Network: %s\\\" class=\\\"dl al\\\"\\>%s\\<br\\>
\\<table cellpadding=0 cellspacing=0 width=100%%\\>\\<tr\\>
\\<td class=loaded width=%d%%\\>\\&nbsp;\\</td\\>
\\<td class=remain width=%d%%\\>\\&nbsp;\\</td\\>
\\</tr\\>\\</table\\>\\</td\\>"
            file.file_num
  			(let n = network_find_by_num file.file_network in
              n.network_name)      
            (short_name file)
            (truncate (downloaded /. size *. 100.))
            (truncate ( (1. -. downloaded /. size) *. 100.))
          );           

          (Printf.sprintf "\\<TD onClick=\\\"location.href='/submit?q=vd+%d';return true;\\\" class=\\\"dl ar\\\"\\>%s\\</td\\>" file.file_num (size_of_int64 file.file_size));
          (Printf.sprintf "\\<TD onClick=\\\"location.href='/submit?q=vd+%d';return true;\\\" class=\\\"dl ar\\\"\\>%s\\</td\\>" file.file_num (size_of_int64 file.file_downloaded));
          (Printf.sprintf "\\<TD onClick=\\\"location.href='/submit?q=vd+%d';return true;\\\" class=\\\"dl ar\\\"\\>%5.1f\\</td\\>" file.file_num (percent file));

(* craaaazy  
need CommonTypes.file  file.file_sources is ?  i dunno *)

          ( 

            let nsrcs = ref 0 in      
            let foo = 
                List.iter 
              (fun cfile -> if (as_file_impl cfile).impl_file_num =
                      file.file_num then 
                   let srcs = file_sources cfile in
                        nsrcs := List.length srcs
                )
            !!files in
            Printf.sprintf "\\<TD onClick=\\\"location.href='/submit?q=vd+%d';return true;\\\" class=\\\"dl ar\\\"\\>%d\\</td\\>" file.file_num !nsrcs

          );

          (Printf.sprintf "\\<TD onClick=\\\"location.href='/submit?q=vd+%d';return true;\\\" class=\\\"dl ar\\\"\\> %s\\</td\\>"
            file.file_num 
            (if !!html_mods_use_relative_availability
            then file_availability file
            else string_availability file.file_availability)
          );


          (if !!html_mods_vd_age then 
			Printf.sprintf "\\<TD onClick=\\\"location.href='/submit?q=vd+%d';return true;\\\" class=\\\"dl ar\\\"\\>%s\\</td\\>"
            file.file_num
            (let age = (BasicSocket.last_time ()) - file.file_age in
            time_to_string age)
		   else Printf.sprintf ""
          );


          (if !!html_mods_vd_last then 
			Printf.sprintf "\\<TD onClick=\\\"location.href='/submit?q=vd+%d';return true;\\\" class=\\\"dl ar\\\"\\>%s\\</td\\>"
            file.file_num
            (if file.file_last_seen > 0
             then let last = (BasicSocket.last_time ()) - file.file_last_seen in
                time_to_string last
             else Printf.sprintf "-"
          	)
			else Printf.sprintf ""

		
          );
          
          (Printf.sprintf "\\<TD onClick=\\\"location.href='/submit?q=vd+%d';return true;\\\" class=\\\"dl ar\\\"\\>%s\\</td\\>"
                file.file_num
                (if file.file_state = FilePaused then
                     "Paused"
                    else
                        if file.file_download_rate < 10.24 then "-"
                        else Printf.sprintf "%5.1f" (file.file_download_rate /. 1024.)
                
                )
          );

          (Printf.sprintf "\\<TD onClick=\\\"location.href='/submit?q=vd+%d';return true;\\\" class=\\\"dl ar\\\"\\>"
            file.file_num ^
			(if (file.file_download_rate < 10.24 || file.file_state = FilePaused) then "-" 
				else time_to_string (calc_file_eta file)) 
			^ "\\</td\\>"
		  );


        |]
    ) guifiles);
  Printf.bprintf buf "\\</form\\>"

end
else Printf.bprintf buf "No files"

  
let html_mods_done_files buf files = 

  Printf.bprintf buf "\\</pre\\>
\\<div class=\\\"main\\\"\\>
\\<table class=main cellspacing=0 cellpadding=0\\> 

\\<tr\\>\\<td\\>

\\<form name=selectForm2 action=/files\\>
\\<table cellspacing=0  cellpadding=0  width=100%%\\>\\<tr\\>
\\<td class=downloaded width=100%%\\>Total: %d - Use 'commit' to move these completed files to the incoming directory\\</td\\>
\\</tr\\>\\</table\\>

\\</td\\>\\</tr\\>
\\<tr\\>\\<td\\>

\\<table class=downloaders cellspacing=0 cellpadding=0\\>\\<tr\\>

\\<TD title=\\\"Number\\\" class=dlheader\\>Num\\</TD\\>
\\<TD title=\\\"Network\\\" class=dlheader\\>Network\\</TD\\>
\\<TD title=\\\"Sort by Filename\\\" class=dlheader\\>\\<input class=headbutton type=submit value=File name=sortby\\>\\</TD\\>
\\<TD title=\\\"Sort by Size\\\" class=dlheader\\>\\<input class=headbutton type=submit value=Size name=sortby\\>\\</TD\\>
\\<TD title=\\\"MD4\\\" class=dlheader\\>MD4\\</TD\\>

\\</tr\\>
" (List.length files);
    
        
     print_table_html_mods buf 

    (List.map (fun file ->
        [|
            (Printf.sprintf "\\>\\<TD class=dl\\>%d\\</TD\\>\\<TD class=dl\\>%s\\</TD\\>"
               file.file_num
              (let n = network_find_by_num file.file_network in
                n.network_name));

            (Printf.sprintf "\\<TD class=dl\\>%s\\</TD\\>"
            (short_name file));
            (Printf.sprintf "\\<TD class=dl\\>%s\\</TD\\>"
            (Int64.to_string file.file_size));
            (Printf.sprintf "\\<TD class=dl\\>%s\\</TD\\>"
            (Md4.to_string file.file_md4))
        |]
    ) files);
  Printf.bprintf buf "\\</form\\>"
  
let simple_print_file_list finished buf files format =
  let print_table = if format.conn_output = HTML then print_table_html 2
    else print_table_text in
  if not finished then
    if format.conn_output = HTML && ( !!html_checkbox_file_list  ) then
      begin
        if !!html_mods then print_file_html_mods buf files
        else
          print_file_html_form buf files  
      end
    else
      print_table buf 
        [| Align_Left; Align_Left; Align_Right; Align_Right; Align_Right; Align_Right |] 
        (if format.conn_output = HTML then
          [|
            "[ Num ]"; 
            "\\<a href=/submit\\?q\\=vd\\&sortby\\=name\\> File \\</a\\>"; 
            "\\<a href=/submit\\?q\\=vd\\&sortby\\=percent\\> Percent \\</a\\>"; 
            "\\<a href=/submit\\?q\\=vd\\&sortby\\=done\\> Downloaded \\</a\\>";
            "\\<a href=/submit\\?q\\=vd\\&sortby\\=size\\> Size \\</a\\>"; 
            "Old";
            "\\<a href=/submit\\?q\\=vd\\&sortby\\=rate\\> Rate \\</a\\>"; 
          |] else
          [|
            "[ Num ]"; 
            "File";
            "Percent"; 
            "Downloaded";
            "Size";
            "Old";
            "Rate";
          |]     
      )
      (List.map (fun file ->
            [|
              (Printf.sprintf "[%s %-5d]%s"
                  (let n = network_find_by_num file.file_network in
                  n.network_name)
                file.file_num
                  (if format.conn_output = HTML then  
                    Printf.sprintf "[\\<a href=/submit\\?q\\=cancel\\+%d $S\\>CANCEL\\</a\\>][\\<a href=/submit\\?q\\=%s\\+%d $S\\>%s\\</a\\>] " 
                      file.file_num
                      (match file.file_state with
                        FileDownloading -> "pause"
                      | _ -> "resume"
                    ) 
                    file.file_num
                      (match file.file_state with
                        FileDownloading -> "PAUSE"
                      | _ -> "RESUME"
                    ) 
                  else ""));
              (short_name file);
              (Printf.sprintf "%5.1f" (percent file));
              (Int64.to_string file.file_downloaded);
              (Int64.to_string file.file_size);
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
              
              (match file.file_state with
                | FilePaused -> "Paused"
                | FileAborted s -> Printf.sprintf "Aborted %s" s
                | _ ->
                    if file.file_download_rate < 10.24 then
                      "-"
                    else
                      Printf.sprintf "%5.1f" (
                        file.file_download_rate /. 1024.));
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
          "\\<a href=/submit\\?q\\=vd\\&sortby\\=name\\> File \\</a\\>"; 
          "\\<a href=/submit\\?q\\=vd\\&sortby\\=size\\> Size \\</a\\>"; 
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
                (let n = network_find_by_num file.file_network in
                n.network_name)
              file.file_num);
            (short_name file);
            (Int64.to_string file.file_size);
            (Md4.to_string file.file_md4)
          |]
      ) files)
  
let display_file_list buf o =
  display_vd := true;
  if not (use_html_mods o) then
    Printf.bprintf buf "Downloaded %d/%d files\n" (List.length !!done_files) 
    (List.length !!files);

   
  let list = List2.tail_map file_info !!files in
  let list = 
    try
      let sorter =
        match o.conn_sortvd with
        
        | BySize -> (fun f1 f2 -> f1.file_size >= f2.file_size)
        | ByRate -> (fun f1 f2 -> 
                        (match f1.file_state with
                        FilePaused -> false
                      | _ -> match f2.file_state with
                              FilePaused -> true
                            | _ -> f1.file_download_rate >= f2.file_download_rate
                        )
                    )
        | ByName -> (fun f1 f2 -> f1.file_name <= f2.file_name)
        | ByDone -> (fun f1 f2 -> f1.file_downloaded >= f2.file_downloaded)
        | ByPercent -> (fun f1 f2 -> percent f1 >= percent f2)
        | ByETA -> (fun f1 f2 -> calc_file_eta f1 <= calc_file_eta f2)
        | ByAge -> (fun f1 f2 -> f1.file_age >= f2.file_age)
        | ByLast -> (fun f1 f2 -> f1.file_last_seen >= f2.file_last_seen)
        | NotSorted -> raise Not_found
      in
      Sort.list sorter list
    with _ -> list
  in
  simple_print_file_list false buf list o

let display_file_list buf o =
  display_file_list buf o;
  if not (use_html_mods o) then
    Printf.bprintf buf "\nDownloaded %d files\n" (List.length !!done_files);
  if !!done_files <> [] then begin
(*      List.iter (fun file -> CommonFile.file_print file o)   !!done_files; *)
      simple_print_file_list true buf 
        (List2.tail_map file_info !!done_files) o; 
      if not (use_html_mods o) then Printf.bprintf buf
          "Use 'commit' to move downloaded files to the incoming directory"
    end




let old_print_search buf output results = 
  let counter = ref 0 in
  if output.conn_output = HTML && !!html_mods then Printf.bprintf buf "\\<table
  id=\\\"resultsTable\\\" name=\\\"resultsTable\\\" class=\\\"sources\\\"
  cellspacing=0 cellpadding=0\\>\\<tr\\>
\\<td title=\\\"Network\\\" onClick=\\\"_tabSort(this,0);\\\" class=\\\"srh\\\"\\>Network\\</td\\>
\\<td title=\\\"Filename\\\" onClick=\\\"_tabSort(this,0);\\\" class=\\\"srh\\\"\\>Filename\\</td\\>
\\<td title=\\\"Size\\\" onClick=\\\"_tabSort(this,1);\\\" class=\\\"srh ar\\\"\\>Size\\</td\\>
\\<td title=\\\"MD4\\\" onClick=\\\"_tabSort(this,0);\\\" class=\\\"srh\\\"\\>MD4\\</td\\>
\\<td title=\\\"Tag\\\" onClick=\\\"_tabSort(this,0);\\\" class=\\\"srh\\\"\\>Tag\\</td\\>
\\</tr\\>";
  
  (try
      List.iter (fun (rs,r,avail) ->
          incr counter;
          if !counter >= !!max_displayed_results then raise Exit;          
          
          if output.conn_output = HTML && !!html_mods then
            begin
              if (!counter mod 2 == 0) then Printf.bprintf buf "\\<tr class=\\\"dl-1\\\"\\>"
              else Printf.bprintf buf "\\<tr class=\\\"dl-2\\\"\\>";
            end;
          
          last_results := (!counter, rs) :: !last_results;
          if !!html_mods && output.conn_output = HTML then Printf.bprintf buf "\\<td class=\\\"sr\\\"\\>%s\\</td\\>"
              (let n = network_find_by_num r.result_network in
              n.network_name)
          else Printf.bprintf  buf "[%5d] %s " 
              !counter
              (let n = network_find_by_num r.result_network in
              n.network_name);
          if output.conn_output = HTML then 
            if !!html_mods then Printf.bprintf buf "\\<td class=\\\"sr\\\"\\>\\<A HREF=/results\\?d=%d target=\\\"$S\\\"\\>" r.result_num
            else Printf.bprintf buf "\\<A HREF=/results\\?d=%d $S\\>" r.result_num;
            begin
              match r.result_names with
                [] -> ()
              | name :: names ->
                  Printf.bprintf buf "%s\n" name;
                  List.iter (fun s -> 
                      if !!html_mods && output.conn_output = HTML then Printf.bprintf buf "\\<BR\\>";
                      Printf.bprintf buf "       %s\n" s
                  ) names;
            end;
          if r.result_done then 
            begin
              if !!html_mods && output.conn_output = HTML then Printf.bprintf buf "\\<BR\\>";
              Printf.bprintf buf " ALREADY DOWNLOADED\n "
            end;
          begin
            match r.result_comment with
              "" -> ()
            | comment -> begin
                  if !!html_mods && output.conn_output = HTML then Printf.bprintf buf "\\<BR\\>";
                  Printf.bprintf buf "COMMENT: %s\n" comment
                end;
          end;
          if output.conn_output = HTML then 
            begin
              if !!html_mods then Printf.bprintf buf "\\</A\\>\\</td\\>"
              else Printf.bprintf buf "\\</A HREF\\>";
            end;
          if !!html_mods && output.conn_output = HTML then 
            Printf.bprintf buf "\\<td class=\\\"sr ar\\\"\\>%s\\</td\\>\\<td class=\\\"sr\\\"\\>%s\\</td\\>"
              (size_of_int64 r.result_size)
            (Md4.to_string r.result_md4)
          else	Printf.bprintf  buf "          %10s %10s " 
              (Int64.to_string r.result_size)
            (Md4.to_string r.result_md4);
          
          if !!html_mods && output.conn_output = HTML then Printf.bprintf buf "\\<td class=\\\"sr\\\"\\>";
          List.iter (fun t ->
              Buffer.add_string buf (Printf.sprintf "%-3s "
                  (if t.tag_name = "availability" then string_of_int avail else
                  
                  match t.tag_value with
                    String s -> s
                  | Uint64 i -> Int64.to_string i
                  | Fint64 i -> Int64.to_string i
                  | _ -> "???"
                ))
          ) r.result_tags;
          if !!html_mods && output.conn_output = HTML then Printf.bprintf buf "\\</td\\>\\</tr\\>";
          Buffer.add_char buf '\n';
      ) results;
      if !!html_mods && output.conn_output = HTML then Printf.bprintf buf "\\</table\\>"
    with _ -> ())
  
  
let add_filter_table buf search_num = 

  Printf.bprintf buf "\\<form action=/filter\\>";
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
let print_search_html buf results format search_num = 
  let counter = ref 0 in
  
  let files = ref [] in
  
  (try
      List.iter  (fun (rs, r, avail) ->

          try
            format.conn_filter r;
            if !!display_downloaded_results || not r.result_done  then 
              let tags_string = 
                let buf = Buffer.create 100 in
                List.iter (fun t ->
                    Buffer.add_string buf (Printf.sprintf "%-3s "
                        (if t.tag_name = "availability" then "" else
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
              last_results := (!counter, rs) :: !last_results;
              files := [|
                (Printf.sprintf "[%5d]\\<input name=d type=checkbox value=%d\\>" !counter r.result_num);
                
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
                      Buffer.add_string buf "\\<TABLE\\>\n";
                      List.iter (fun s -> 
                          Buffer.add_string buf "\\<TR\\>\\<TD\\>";
                          Buffer.add_string buf s;
                          Buffer.add_string buf "\\</TD\\>\\</TR\\>";
                      ) names;
                      Buffer.add_string buf "\\</TABLE\\>\n";
                      
                      Buffer.contents buf
                );
                
                (Int64.to_string r.result_size);
                
                tags_string;
                
                (string_of_int avail);
                
                (Md4.to_string r.result_md4);
              |] :: !files
          with _ -> ()
      ) results;
    with _ -> ());
  
  if !counter > !!filter_table_threshold then
    add_filter_table buf search_num;
  
  Printf.bprintf buf "\\<form action=/results\\>";
  Printf.bprintf buf "\\<input type=submit value='Submit Changes'\\>";
  print_table_html 10 buf [||] 
    [|
    "[ Num ]";
    "Names";
    "Size";
    "Tags";
    "Avail";
    "MD4";
  |] 
    (List.rev !files);
  Printf.bprintf buf "\\</form\\>"      

  
  
let print_results buf format results =
  
  let print_table = if format.conn_output = HTML then print_table_html 2
    else print_table_text in
  
  let counter = ref 0 in
  let files = ref [] in
  (try
      List.iter (fun (rs, r,avail) ->
          if !!display_downloaded_results || not r.result_done  then begin
              incr counter;
              if !counter >= !!max_displayed_results then raise Exit;
              last_results := (!counter, rs) :: !last_results;
              files := [|
                
                (if format.conn_output = HTML && !!html_mods then
                    Printf.sprintf "\\>\\<td class=\\\"sr\\\"\\>%d\\</td\\>\\<td class=\\\"sr\\\"\\>" !counter
                  else Printf.sprintf "[%5d]" !counter);
                
                (Printf.sprintf "%s%s%s"
                    (if format.conn_output = HTML then begin
                        if !!html_mods then Printf.sprintf "\\<A HREF=/results\\?d=%d target=\\\"$S\\\"\\>" r.result_num
                        else Printf.sprintf "\\<A HREF=/results\\?d=%d $S\\>" r.result_num;
                      end
                    else "")
                  
                  (
                    let names = r.result_names in
                    let names = if r.result_done then
                        names @ ["ALREADY DOWNLOADED"] else names in
                    let names = match  r.result_comment with
                        "" -> names
                      |  comment ->
                          names @ ["COMMENT: " ^ comment] 
                    in
                    match names with
                      [name] -> name
                    | _ ->
                        let buf = Buffer.create 100 in
                        Buffer.add_string buf "\\<TABLE\\>\n";
                        List.iter (fun s -> 
                            Buffer.add_string buf "\\<TR\\>";
                            Buffer.add_string buf s;
                            Buffer.add_string buf "\\</TR\\>";
                        ) names;
                        Buffer.add_string buf "\\</TABLE\\>\n";
                        
                        Buffer.contents buf
                  )
                  (if format.conn_output = HTML then 
                      begin
                        if !!html_mods then "\\</A\\>\\</td\\>"
                        else "\\</A HREF\\>" 
                      end
                    else ""
                  )
                );
                
                
                (if format.conn_output = HTML && !!html_mods then 
                    "\\<td class=\\\"sr\\\"\\>" ^  size_of_int64 r.result_size ^ "\\</td\\>"
                  else Int64.to_string r.result_size
                );
                
                
                (let buf = Buffer.create 100 in
                  
                  if format.conn_output = HTML && !!html_mods then Buffer.add_string buf "\\<td class=\\\"sr\\\"\\>";
                  
                  List.iter (fun t ->
                      Buffer.add_string buf (Printf.sprintf "%-3s "
                          (if t.tag_name = "availability" then "" else
                          match t.tag_value with
                            String s -> s
                          | Uint64 i -> Int64.to_string i
                          | Fint64 i -> Int64.to_string i
                          | _ -> "???"
                        ))
                  ) r.result_tags;
                  Buffer.contents buf);
                
                (if format.conn_output = HTML && !!html_mods then 
                    "\\<td class=\\\"sr\\\"\\>" ^  (string_of_int avail) ^ "\\</td\\>"
                  else (string_of_int avail)
                );
                
                (if format.conn_output = HTML && !!html_mods then 
                    "\\<td class=\\\"sr\\\"\\>" ^  (Md4.to_string r.result_md4) ^ "\\</td\\>"
                  else (Md4.to_string r.result_md4)
                );
              
              |] :: !files;
            end
      ) results;
    with _ -> ());
  if !!html_mods && format.conn_output = HTML then
    begin
      
      Printf.bprintf buf "
\\<table class=sources cellspacing=0 cellpadding=0\\>\\<tr\\>
\\<td title=\\\"Number\\\" onClick=\\\"_tabSort(this,0);\\\" class=\\\"srh\\\"\\>#\\</td\\>
\\<td title=\\\"Filename\\\" onClick=\\\"_tabSort(this,0);\\\" class=\\\"srh\\\"\\>Name\\</td\\>
\\<td title=\\\"Size\\\" onClick=\\\"_tabSort(this,1);\\\" class=\\\"srh\\\"\\>Size\\</td\\>
\\<td title=\\\"Tag\\\" onClick=\\\"_tabSort(this,0);\\\" class=\\\"srh\\\"\\>Tag\\</td\\>
\\<td title=\\\"Availability\\\" onClick=\\\"_tabSort(this,1);\\\" class=\\\"srh\\\"\\>A\\</td\\>
\\<td title=\\\"MD4\\\" onClick=\\\"_tabSort(this,0);\\\" class=\\\"srh\\\"\\>MD4\\</td\\>
\\</tr\\>
";
      
      print_table_html_mods buf
        
        (List.rev !files)
    
    end
  
  else
    
    
    print_table buf [||] 
      [|
      "[ Num ]";
      "Names";
      "Size";
      "Tags";
      "Avail";
      "MD4";
    |] 
    
    (List.rev !files)
  
  
let print_search buf s format = 
  
  last_search := Some s;
  last_results := [];
  let results = ref [] in
  Intmap.iter (fun r_num (avail,r) ->
      results := (r, result_info r, !avail) :: !results) s.search_results;
  let results = Sort.list (fun (_, r1,_) (_, r2,_) ->
        r1.result_size > r2.result_size
    ) !results in
  
  Printf.bprintf buf "Result of search %d\n" s.search_num;
  Printf.bprintf buf "Reinitialising download selectors\n";
  Printf.bprintf buf "%d results (%s)\n" s.search_nresults 
    (if s.search_waiting = 0 then "done" else
      (string_of_int s.search_waiting) ^ " waiting");
  
  if not !!new_print_search then old_print_search buf format results else
    begin
      if format.conn_output = HTML && !!html_checkbox_file_list then
        print_search_html buf results format s.search_num
      else
        print_results buf format results 
    end  

let browse_friends () =
  List.iter (fun c -> client_browse c false) !!friends;
  List.iter (fun c -> client_browse c false) !contacts
  

  
