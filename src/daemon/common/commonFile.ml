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
open Md4
open CommonClient
open Options
open CommonTypes
open CommonOptions
open CommonGlobals
  
type 'a file_impl = {
    mutable impl_file_update : int;
    mutable impl_file_state : file_state;

    mutable impl_file_num : int;
    mutable impl_file_val : 'a;
    mutable impl_file_ops : 'a file_ops;
    mutable impl_file_size : int64;
    mutable impl_file_age : int;
    mutable impl_file_fd : Unix32.t;
    mutable impl_file_downloaded : int64;
    mutable impl_file_received : int64;
    mutable impl_file_last_received : (int64 * int) list;
    mutable impl_file_last_rate : float;
    mutable impl_file_best_name : string;
    mutable impl_file_priority: int; (* normal = 0, low < 0, high > 0 *)
    mutable impl_file_last_seen : int;
  }


and 'a file_ops = {
    mutable op_file_network : network;
    
(* This method is called just before the file is moved to the incomming
section, and thus, before it is shared. *)
    mutable op_file_commit : ('a -> string -> unit);
    
(* This method is called when the name under which the file should be saved
has been changed. The method should not perform the move, just know that
it will happen soon. *)
    mutable op_file_save_as : ('a -> string -> unit);
    mutable op_file_to_option : ('a -> (string * option_value) list);
    mutable op_file_cancel : ('a -> unit);
    mutable op_file_pause : ('a -> unit);
    mutable op_file_resume : ('a -> unit);
    mutable op_file_info : ('a -> GuiTypes.file_info);
    mutable op_file_set_format : ('a -> CommonTypes.format -> unit);
    mutable op_file_check : ('a -> unit);
    mutable op_file_recover : ('a -> unit);
    mutable op_file_sources : ('a -> client list);
    mutable op_file_comment : ('a -> string);
    mutable op_file_set_priority : ('a -> int -> unit);
    mutable op_file_print_sources_html : ('a -> Buffer.t -> unit);
    mutable op_file_debug : ('a -> string);
  }
  
let as_file  (file : 'a file_impl) =
  let (file : file) = Obj.magic file in
  file
  
let as_file_impl  (file : file) =
  let (file : 'a file_impl) = Obj.magic file in
  file
  
let file_num file =
  let impl = as_file_impl  file in
  impl.impl_file_num

let dummy_file_impl = {
    impl_file_update = 1;
    impl_file_state = FileNew;
    impl_file_num = 0;
    impl_file_val = 0;
    impl_file_ops = Obj.magic 0;
    impl_file_size = Int64.zero;
    impl_file_age = 0;
    impl_file_fd = Unix32.create_diskfile "" [Unix.O_RDONLY] 0o666;
    impl_file_downloaded = Int64.zero;
    impl_file_received = Int64.zero;
    impl_file_last_received = [];
    impl_file_last_rate = 0.0;
    impl_file_best_name = "<UNKNOWN>";
    impl_file_priority = 0;
    impl_file_last_seen = 0;
  }
  
let dummy_file = as_file dummy_file_impl  

module H = Weak2.Make(struct
      type t = file
      let hash file = Hashtbl.hash (file_num file)
      
      let equal x y  = (file_num x) = (file_num y)
    end)

let file_counter = ref 0
let files_by_num = H.create 1027
  
  
let _ = 
  Heap.add_memstat "CommonFile" (fun buf ->
      let counter = ref 0 in
      H.iter (fun _ -> incr counter) files_by_num;
      Printf.bprintf buf "  files: %d\n" !counter;
  )

let ni n m = 
  let s = Printf.sprintf "File.%s not implemented by %s" 
      m n.network_name in
  lprintf "%s\n" s; 
  s
  
let fni n m = failwith (ni n m)
let ni_ok n m = ignore (ni n m)

let file_must_update file =
  let impl = as_file_impl file in
  if impl.impl_file_update <> 0 then
    CommonEvent.add_event (File_info_event file);
  impl.impl_file_update <- 0

let file_must_update_downloaded file =
  let impl = as_file_impl file in
  if impl.impl_file_update > 0 then
    begin
      impl.impl_file_update <- - impl.impl_file_update;
      CommonEvent.add_event (File_info_event file);
    end

let update_file_num impl =
  if impl.impl_file_num = 0 then begin
      incr file_counter;
      impl.impl_file_num <- !file_counter;
      H.add files_by_num (as_file impl);
      file_must_update (as_file impl);
    end
    
    
let update_file_state impl state =
  impl.impl_file_state <- state;
  file_must_update (as_file impl)
  
let file_to_option (file : file) =
  let file = as_file_impl file in
  file.impl_file_ops.op_file_to_option file.impl_file_val

  (*
let file_print (file : file) buf =
  let file = as_file_impl file in
  file.impl_file_ops.op_file_print file.impl_file_val buf
    *)

let file_save_as (file : file) name =
  let file = as_file_impl file in
  file.impl_file_ops.op_file_save_as file.impl_file_val name

let file_comment (file : file) =
  let file = as_file_impl file in
  file.impl_file_ops.op_file_comment file.impl_file_val

let file_network (file : file) =
  let file = as_file_impl file in
  file.impl_file_ops.op_file_network

let file_info (file : file) =
  let file = as_file_impl file in
  file.impl_file_ops.op_file_info file.impl_file_val

let file_pause (file : file) =
  let file = as_file_impl file in
  match file.impl_file_state with
  | FileDownloading | FileQueued ->
      update_file_state file FilePaused;
      file.impl_file_ops.op_file_pause file.impl_file_val
  | _ -> ()

let file_resume (file : file) =
  let file = as_file_impl file in
  match file.impl_file_state with
  | FilePaused | FileAborted _ ->
      update_file_state file FileDownloading;
      file.impl_file_ops.op_file_resume file.impl_file_val
  | _ -> ()

let set_file_state file state = 
  let impl = as_file_impl file in
  update_file_state impl state
      
let file_best_name (file : file) =
  let file = as_file_impl file in
  file.impl_file_best_name
  
let set_file_best_name file name = 
  let file = as_file_impl file in
  let name = String2.replace name '/' "::" in
  file.impl_file_best_name <- name

let file_set_format (file : file) format =
  let file = as_file_impl file in
  file.impl_file_ops.op_file_set_format file.impl_file_val format


let file_check (file : file) =
  let file = as_file_impl file in
  file.impl_file_ops.op_file_check file.impl_file_val


let file_recover (file : file) =
  let file = as_file_impl file in
  file.impl_file_ops.op_file_recover file.impl_file_val

let file_debug (file : file) =
  let file = as_file_impl file in
  file.impl_file_ops.op_file_debug file.impl_file_val

let file_sources file =
  let impl = as_file_impl file in
  try impl.impl_file_ops.op_file_sources impl.impl_file_val with _ -> []

let file_print_sources_html (file : file) buf =
  let file = as_file_impl file in
  file.impl_file_ops.op_file_print_sources_html file.impl_file_val buf
  
let files_ops = ref []
  
let new_file_ops network = 
  let f = 
    {
      op_file_network =  network;
      op_file_commit = (fun _ _ -> ni_ok network "file_commit");
      op_file_save_as = (fun _ _ -> ni_ok network "file_save_as");
(*    op_file_print = (fun _ _ -> ni_ok network "file_print"); *)
      op_file_to_option = (fun _ -> fni network "file_to_option");
      op_file_cancel = (fun _ -> ni_ok network "file_cancel");
      op_file_info = (fun _ -> fni network "file_info");
      op_file_pause = (fun _ -> ni_ok network "file_pause");
      op_file_resume = (fun _ -> ni_ok network "file_resume");
(*      op_file_disk_name = (fun _ -> fni network "file_disk_name"); *)
      op_file_check = (fun _ -> ni_ok network "file_check");
      op_file_recover = (fun _ -> ni_ok network "file_recover");
      op_file_set_format = (fun _ -> fni network "file_set_format");
      op_file_sources = (fun _ -> fni network "file_sources");
      op_file_comment = (fun _ -> ni_ok network "file_comment"; "");
      op_file_set_priority = (fun _ _ -> ni_ok network "file_set_priority");
      op_file_print_sources_html = (fun _ _ -> ni_ok network "file_print_sources_html");
      op_file_debug = (fun _ -> "");
    }
  in
  let ff = (Obj.magic f : int file_ops) in
  files_ops := (ff, { ff with op_file_network = ff.op_file_network })
  :: ! files_ops;
  f

let check_file_implementations () =
  lprintf "\n---- Methods not implemented for CommonFile ----\n";
  List.iter (fun (c, cc) ->
      let n = c.op_file_network.network_name in
      lprintf "\n  Network %s\n" n; 
      if c.op_file_to_option == cc.op_file_to_option then 
        lprintf "op_file_to_option\n";
      if c.op_file_info == cc.op_file_info then
        lprintf "op_file_info\n";
      if c.op_file_commit == cc.op_file_commit then
        lprintf "op_file_commit\n";
      if c.op_file_save_as == cc.op_file_save_as then
        lprintf "op_file_save_as\n";
      if c.op_file_cancel == cc.op_file_cancel then
        lprintf "op_file_cancel\n";
      if c.op_file_pause == cc.op_file_pause then
        lprintf "op_file_pause\n";
      if c.op_file_resume == cc.op_file_resume then
        lprintf "op_file_resume\n";
(*      if c.op_file_disk_name == cc.op_file_disk_name then
        lprintf "op_file_disk_name\n"; *)
      if c.op_file_check == cc.op_file_check then
        lprintf "op_file_check\n";
      if c.op_file_recover == cc.op_file_recover then
        lprintf "op_file_recover\n";
      if c.op_file_set_format == cc.op_file_set_format then
        lprintf "op_file_set_format\n";
      if c.op_file_sources == cc.op_file_sources then
        lprintf "op_file_sources\n";
      if c.op_file_print_sources_html == cc.op_file_print_sources_html then
        lprintf "op_file_print_sources_html\n";
  ) !files_ops;
  lprint_newline () 

  
let file_find num = 
  H.find files_by_num (as_file {
    dummy_file_impl   with impl_file_num = num
  })

let file_state file =
  let impl = as_file_impl file in
  impl.impl_file_state  
  
let file_add_source (file : file) c =
  client_must_update c;
  CommonEvent.add_event (File_add_source_event (file,c))
  
let file_remove_source (file : file) c =
  CommonEvent.add_event (File_remove_source_event (file,c))

let rec last = function
    [x] -> x
  | _ :: l -> last l
  | _ -> (Int64.zero, 0)
    
let sample_timer () =
  let trimto list length =
    let (list, _) = List2.cut length list in
    list 
  in
  let time = BasicSocket.last_time () in
  H.iter (fun file ->
      let impl = as_file_impl file in
      impl.impl_file_last_received <-
        trimto ((impl.impl_file_received, time) :: 
        impl.impl_file_last_received) 
      !!CommonOptions.download_sample_size;
      match impl.impl_file_last_received with
        _ :: (last_received, _) :: _ ->
          if last_received = impl.impl_file_received &&
            impl.impl_file_last_rate > 0. then
            file_must_update_downloaded file
      | _ -> ()
  ) files_by_num

let file_download_rate impl =
  let time = BasicSocket.last_time () in
  let (last_received, file_last_time) = last impl.impl_file_last_received in
  let time = time - file_last_time in
  let diff = Int64.sub impl.impl_file_received last_received in
  let rate = if time > 0 && diff > Int64.zero then begin
        (Int64.to_float diff) /. (float_of_int time);
      end else 0.0
  in
  impl.impl_file_last_rate <- rate;
  rate
  
let add_file_downloaded impl n =
  impl.impl_file_downloaded <- Int64.add impl.impl_file_downloaded n;
(* you cannot remove received bytes *)
  if Int64.compare n Int64.zero > 0 then
    impl.impl_file_received <- Int64.add impl.impl_file_received n;
  file_must_update_downloaded (as_file impl)
    
let file_size file = 
  (as_file_impl file).impl_file_size
  
let file_disk_name file =
  Unix32.filename (as_file_impl file).impl_file_fd
      
let file_fd file =
  (as_file_impl file).impl_file_fd

let set_file_disk_name file filename=
  Unix32.rename (file_fd file) filename
  
let file_downloaded file = (as_file_impl file).impl_file_downloaded

let file_network file =
  (as_file_impl file).impl_file_ops.op_file_network

let file_priority file = 
  (as_file_impl file).impl_file_priority
  
let file_set_priority file p = 
  let impl = as_file_impl file in
  if impl.impl_file_priority <> p then begin
      impl.impl_file_priority <- p;      
      impl.impl_file_ops.op_file_set_priority  impl.impl_file_val p;      
      file_must_update file
    end

let file_preview (file : file) =
  let cmd = Printf.sprintf "%s \"%s\" \"%s\"" !!previewer
	      (file_disk_name file) (file_best_name file) in
  ignore (Sys.command cmd)

let com_files_by_num = files_by_num
let files_by_num = ()

module G = GuiTypes

let file_downloaders file o cnt = 
  let buf = o.conn_buf in

      let srcs = file_sources file in
		
      let counter = ref cnt in
      List.iter (fun c ->
		if client_state c = Connected_downloading then begin
          if use_html_mods o then begin 
              if (client_dprint_html c o file (if !counter mod 2 == 0 then "dl-1" else "dl-2";))
					then incr counter;
		  end
		  else begin
			client_dprint c o file;
		  end;
		end;
      ) srcs;

	if !counter mod 2 = 0 then true else false
  
(* Use span for Opera DOM compatibility *)
let colored_chunks chunks =
  let ostr = ref "" in
  let previous = ref 0 in
  let runlength = ref 0 in
  let nextbit b =
    if b = !previous then
      incr runlength
    else begin
      if !runlength > 0 then begin
	ostr := !ostr ^ Printf.sprintf 
	"\\<span class=\\\"chunk%d\\\"\\>" !previous;
	while !runlength > 0 do
          ostr := !ostr ^ "\\&nbsp;";
          decr runlength
	done;
    ostr := !ostr ^ "\\</span\\>"
      end;
      previous := b;
      runlength := 1
    end in
  Array.iter (fun b -> nextbit b) chunks;
  nextbit 99;
  !ostr

let file_print file o = 
  let impl = as_file_impl file in
  let info = file_info file in
  let n = impl.impl_file_ops.op_file_network in
  let buf = o.conn_buf in
  let srcs = file_sources file in


(* TODO: make multinet aware *)
  
  if use_html_mods o then begin
      
      html_mods_table_header buf "sourcesInfo" "sourcesInfo" [ 
        ( "0", "srh br", "File Info", "Info" ) ; 
        ( "0", "srh", "Value", "Value" ) ]; 
      
      Printf.bprintf buf "\\<tr class=\\\"dl-1\\\"\\>";
      html_mods_td buf [
        ("File number/Network", "sr br", "[#] Network");
        ("", "sr", Printf.sprintf "[%d] %s" (file_num file) n.network_name) ];
      
      Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-2\\\"\\>";
      html_mods_td buf [
        ("Downloaded/Total size", "sr br", "DLed/Size");
        ("", "sr", Printf.sprintf "%s bytes of %s bytes" 
            (Int64.to_string info.G.file_downloaded) (Int64.to_string info.G.file_size) ) ];
      
      Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-1\\\"\\>";
      html_mods_td buf [
        ("File priority", "sr br", "Priority");
        ("", "sr", Printf.sprintf "%d" (file_priority file)) ];
      
      Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-2\\\"\\>";
      html_mods_td buf [
        ("Number of file sources", "sr br", "Sources");
        ("", "sr", Printf.sprintf "%d" (List.length srcs)) ];
      
      Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-1\\\"\\>";
      html_mods_td buf [
        ("0=Missing, 1=Partial, 2=Complete, 3=Verified", "sr br", "Chunks");
        ("0=Missing, 1=Partial, 2=Complete, 3=Verified", "sr", info.G.file_chunks) ];
      
      (match n.network_name with 
          "BitTorrent" -> (
              Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-2\\\"\\>";
              html_mods_td buf [
                ("Filename", "sr br", "Filename");
                ("", "sr", (file_best_name file)) ]; )
        
        | _ -> (
              
              Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-2\\\"\\>";
              html_mods_td buf [
                ("Fake check links", "sr br", "Fakecheck");
                ("", "sr", Printf.sprintf "\\<a href=\\\"http://donkeyfakes.gambri.net/fakecheck/update/fakecheck.php\\?md4=%s\\\"\\>[Donkey-Fakes]\\</a\\>
    				\\<a href=\\\"http://www.sharereactor.com/fakesearch.php\\?search=ed2k://|file|%s|%s|%s|/\\\"\\>[ShareReactor Fakecheck]\\</a\\>" 
                    (Md4.to_string info.G.file_md4) 
                  (info.G.file_name) (Int64.to_string info.G.file_size) (Md4.to_string info.G.file_md4)) ];
              
              
              Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-1\\\"\\>";
              
              html_mods_td buf [
                ("ed2k link", "sr br", "ed2k link");
                ("", "sr", Printf.sprintf "\\<a href=\\\"ed2k://|file|%s|%s|%s|/\\\"\\>ed2k://|file|%s|%s|%s|/\\</A\\>" 
                    (info.G.file_name) (Int64.to_string info.G.file_size) (Md4.to_string info.G.file_md4)
                  (info.G.file_name) (Int64.to_string info.G.file_size) (Md4.to_string info.G.file_md4)) ];
              
              Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-2\\\"\\>";
              let optionlist = ref "" in
              List.iter (fun (name,_) -> 
                  optionlist := !optionlist ^ Printf.sprintf "\\<option value=\\\"%s\\\"\\>%s\n" name name;
              ) info.G.file_names;
              
              
              let input_size = (Printf.sprintf "%d" ((String.length (file_best_name file))+3)) in
              html_mods_td buf [
                ("Rename file by selecting an alternate name", "sr br", "Filename");
                ("Rename file", "sr", Printf.sprintf "\\<script language=javascript\\>
\\<!-- 
function submitRenameForm(i) {
var formID = document.getElementById(\\\"renameForm\\\" + i)
var regExp = new RegExp (' ', 'gi') ;
var renameTextOut = formID.newName.value.replace(regExp, '+'); 
parent.fstatus.location.href='submit?q=rename+%d+\\\"'+renameTextOut+'\\\"';
}
//--\\>
\\</script\\>" (file_num file) 
                  
                  ^ "\\<table border=0 cellspacing=0 cellpadding=0\\>\\<tr\\>\\<td\\>"
                    ^ "\\<form name=\\\"renameForm1\\\" id=\\\"renameForm1\\\" action=\\\"javascript:submitRenameForm(1);\\\"\\>"
                    ^ "\\<select name=\\\"newName\\\" id=\\\"newName\\\" onchange=\\\"this.form.submit()\\\"\\>"
                    ^ Printf.sprintf "\\<option value=\\\"%s\\\" selected\\>%s\n" (file_best_name file) (file_best_name file)
                  ^ !optionlist 
                    ^ "\\</select\\>\\</td\\>\\</tr\\>\\</form\\>\\<tr\\>\\<td\\>\\<form name=\\\"renameForm2\\\" id=\\\"renameForm2\\\" action=\\\"javascript:submitRenameForm(2);\\\"\\>"
                    ^ "\\<input name=\\\"newName\\\" type=text size=" ^ input_size ^ " value=\\\"" ^ (file_best_name file) ^ "\\\"\\>\\</input\\>\\</td\\>\\</tr\\>\\</form\\>\\</table\\>") ];
            
            )
      );
      
      Printf.bprintf buf "\\</tr\\>\\</table\\>\\</div\\>"; 
      Printf.bprintf buf "\\</td\\>\\</tr\\>\\</table\\>\\</div\\>\\<br\\>";
    
    end else
    begin
      Printf.bprintf buf "[%-s %5d] %-50s %10s %10s\npriority %d\n" 
        n.network_name (file_num file) (match info.G.file_names with
          [] -> Md4.to_string info.G.file_md4
        | (name,_) :: _ -> name)
      (Int64.to_string info.G.file_size)
      (Int64.to_string info.G.file_downloaded)
      (file_priority file);
      Printf.bprintf buf "Chunks: [%-s]\n" info.G.file_chunks;
      List.iter (fun (name,_) -> Printf.bprintf buf "    (%s)\n" name) info.G.file_names
	end;
  
  (try
      
      let bitTorrentHeader () = 
        
        html_mods_table_header buf "sourcesTable" "sources al" [ 
          ( "1", "srh br ac", "Client number", "Num" ) ; 
          ( "0", "srh br", "Client UID", "UID" ) ; 
          ( "0", "srh", "IP address", "IP address" ) ; 
          ( "0", "srh br ar", "Port", "Port" ) ; 
          ( "1", "srh ar", "Total UL bytes to this client for all files", "UL" ) ; 
          ( "1", "srh ar br", "Total DL bytes from this client for all files", "DL" ) ; 
          ( "1", "srh ar", "Interested [T]rue, [F]alse", "I" ) ; 
          ( "1", "srh ar", "Choked [T]rue, [F]alse", "C" ) ; 
          ( "1", "srh br ar", "Allowed to write", "A" ) ; 
(* 
		( "0", "srh", "Bitmap (absent|partial|present|verified)", (colored_chunks 
        (Array.init (String.length info.G.file_chunks)
        (fun i -> ((int_of_char info.G.file_chunks.[i])-48)))) ) ; 
*)
          ( "1", "srh ar", "Number of full chunks", (Printf.sprintf "%d"
		(String.length (String2.replace (String2.replace info.G.file_chunks '0' "") '1' "")) )) ]
      
      in
      
      let fastTrackHeader () = 
        
        html_mods_table_header buf "sourcesTable" "sources al" [ 
          ( "1", "srh br ac", "Client number", "Num" ) ; 
          ( "0", "srh br", "Client Name", "Name" ) ; 
          ( "0", "srh", "IP address", "IP address" ) ; 
          ( "1", "srh ar", "Total UL bytes to this client for all files", "UL" ) ; 
          ( "1", "srh ar br", "Total DL bytes from this client for all files", "DL" ) ; ]
      
      in
      
	let gnutellaHeader () = 

		html_mods_table_header buf "sourcesTable" "sources al" [ 
		( "1", "srh br ac", "Client number", "Num" ) ; 
		( "0", "srh br", "Client Name", "Name" ) ; 
		( "0", "srh", "IP address", "IP address" ) ; 
		( "1", "srh ar", "Total UL bytes to this client for all files", "UL" ) ; 
		( "1", "srh ar br", "Total DL bytes from this client for all files", "DL" ) ; ]

	in

      let defaultHeader () = 
        
        html_mods_table_header buf "sourcesTable" "sources al" [ 
          ( "1", "srh ac", "Client number (click to add as friend)", "Num" ) ; 
          ( "0", "srh", "[A] = Active download from client", "A" ) ; 
          ( "0", "srh", "Client state", "CS" ) ; 
          ( "0", "srh", "Client name", "Name" ) ; 
          ( "0", "srh", "Client brand", "CB" ) ; 
          ( "0", "srh", "Overnet [T]rue, [F]alse", "O" ) ; 
          ( "0", "srh", "Connection [I]ndirect, [D]irect", "C" ) ; 
          ( "0", "srh br", "IP address", "IP address" ) ; 
          ( "1", "srh ar", "Total UL bytes to this client for all files", "UL" ) ; 
          ( "1", "srh ar br", "Total DL bytes from this client for all files", "DL" ) ; 
          ( "1", "srh ar", "Your queue rank on this client", "Rnk" ) ; 
          ( "1", "srh ar br", "Source score", "Scr" ) ; 
          ( "1", "srh ar", "Last ok (minutes)", "LO" ) ; 
          ( "1", "srh ar", "Last try (minutes)", "LT" ) ; 
          ( "1", "srh ar br", "Next try (minutes)", "NT" ) ; 
          ( "0", "srh", "Has a slot [T]rue, [F]alse", "H" ) ; 
          ( "0", "srh br", "Banned [T]rue, [F]alse", "B" ) ; 
          ( "1", "srh ar", "Requests sent", "RS" ) ; 
          ( "1", "srh ar", "Requests received", "RR" ) ; 
          ( "1", "srh ar br", "Connected time (minutes)", "CT" ) ; 
          ( "0", "srh br", "Client MD4", "MD4" ) ; 
          ( "0", "srh", "Chunks (absent|partial|present|verified)", (colored_chunks 
                (Array.init (String.length info.G.file_chunks)
                (fun i -> ((int_of_char info.G.file_chunks.[i])-48)))) ) ; 
          ( "1", "srh ar", "Number of full chunks", (Printf.sprintf "%d"
                (let fc = ref 0 in (String.iter (fun s -> if s = '2' then incr fc) info.G.file_chunks );!fc ))) ];
      
      in
      
      if use_html_mods o = false then Printf.bprintf buf "%d sources:\n" (List.length srcs);
      
      if use_html_mods o && srcs <> [] then begin
          
          (match n.network_name with 
              "BitTorrent" ->  bitTorrentHeader ();
            | "Fasttrack" ->  fastTrackHeader ();
	   | "Gnutella" ->  gnutellaHeader ();
            | _ -> defaultHeader ();)
        
        end;
      
      let counter = ref 0 in
      List.iter (fun c ->
          incr counter;
          
          if use_html_mods o then begin
              Printf.bprintf buf " \\<tr onMouseOver=\\\"mOvr(this);\\\" 
				onMouseOut=\\\"mOut(this);\\\" class=\\\"%s\\\"\\>"
              (if (!counter mod 2 == 0) then "dl-1" else "dl-2");
              client_bprint_html c buf file;
              Printf.bprintf buf "\\</tr\\>";
            end
          else
            if !!all_sources_on_telnet then client_bprint c buf;
      ) srcs;

      if use_html_mods o && srcs <> [] then begin
        Printf.bprintf buf "\\</table\\>\\</div\\>\\<br\\>";
        if !!html_mods_vd_queues then file_print_sources_html file buf;
      end

  with _ -> ())
(*
let file_size file = 
  (as_file_impl file).impl_file_size
  
let file_disk_name file =
  Unix32.filename (as_file_impl file).impl_file_fd
      
let file_fd file =
  (as_file_impl file).impl_file_fd

let set_file_disk_name file filename =
  Unix32.rename (file_fd file) filename
  
let file_downloaded file = (as_file_impl file).impl_file_downloaded

let file_network file =
  (as_file_impl file).impl_file_ops.op_file_network

let file_priority file = 
  (as_file_impl file).impl_file_priority
  
let file_set_priority file p = 
  let impl = as_file_impl file in
  if impl.impl_file_priority <> p then begin
      impl.impl_file_priority <- p;      
      impl.impl_file_ops.op_file_set_priority  impl.impl_file_val p;      
      file_must_update file
    end
*)
