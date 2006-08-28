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
open Int64ops
open CommonClient
open Options
open CommonTypes
open CommonOptions
open CommonGlobals

let log_prefix = "[cF]"

let lprintf_nl fmt =
  lprintf_nl2 log_prefix fmt

let lprintf_n fmt =
  lprintf2 log_prefix fmt
	    
(*************************************************************************)
(*                                                                       *)
(*                         TYPES                                         *)
(*                                                                       *)
(*************************************************************************)

type 'a file_impl = {
    mutable impl_file_update : int;
    mutable impl_file_state : file_state;

    mutable impl_file_comment : string;
    mutable impl_file_num : int;
    mutable impl_file_val : 'a;
    mutable impl_file_ops : 'a file_ops;
    mutable impl_file_size : int64;
    mutable impl_file_age : int;
    mutable impl_file_fd : Unix32.t option;
    mutable impl_file_downloaded : int64;
    mutable impl_file_received : int64;
    mutable impl_file_last_received : (int64 * int) list;
    mutable impl_file_last_rate : float;
    mutable impl_file_best_name : string;
    mutable impl_file_magic : string option;
    mutable impl_file_priority: int; (* normal = 0, low < 0, high > 0 *)
    mutable impl_file_last_seen : int;
    mutable impl_file_probable_name : string option;
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
    mutable op_file_all_sources : ('a -> client list);
    mutable op_file_active_sources : ('a -> client list);
    mutable op_file_comment : ('a -> string);
    mutable op_file_set_priority : ('a -> int -> unit);
    mutable op_file_print_html : ('a -> Buffer.t -> unit);
    mutable op_file_print_sources_html : ('a -> Buffer.t -> unit);
    mutable op_file_files : ('a -> 'a file_impl -> file list);    
(* added in 2.5.27 to remove use of network names in global modules *)
    mutable op_file_debug : ('a -> string);
    mutable op_file_proposed_filenames : ('a -> string list);
  }


(*************************************************************************)
(*                                                                       *)
(*                         as_file...                                    *)
(*                                                                       *)
(*************************************************************************)

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
    impl_file_fd = None;
    impl_file_downloaded = Int64.zero;
    impl_file_received = Int64.zero;
    impl_file_last_received = [];
    impl_file_last_rate = 0.0;
    impl_file_best_name = "<UNKNOWN>";
    impl_file_magic = None;
    impl_file_priority = 0;
    impl_file_last_seen = 0;
    impl_file_comment = "";
    impl_file_probable_name = None;
  }

let dummy_file = as_file dummy_file_impl

(*************************************************************************)
(*                                                                       *)
(*                         Global values                                 *)
(*                                                                       *)
(*************************************************************************)

module H = Weak.Make(struct
      type t = file
      let hash file = Hashtbl.hash (file_num file)
      
      let equal x y  = (file_num x) = (file_num y)
    end)

let file_counter = ref 0
let files_by_num = H.create 1027

(*************************************************************************)
(*                                                                       *)
(*                         Stub functions                                *)
(*                                                                       *)
(*************************************************************************)

let ni n m =
  let s = Printf.sprintf "File.%s not implemented by %s"
      m n.network_name in
  lprintf_nl "%s" s;
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

let set_file_comment file comment =
  let impl = as_file_impl file in
  impl.impl_file_comment <- comment

let file_comment file = 
  let impl = as_file_impl file in
  impl.impl_file_comment
      
let file_best_name (file : file) =
  let file = as_file_impl file in
  file.impl_file_best_name
  
let set_file_best_name file name fs namemax =
  let file = as_file_impl file in
  let old_name = file.impl_file_best_name in
  let real_name = Filename2.filesystem_compliant name fs namemax in
  if real_name = "" then
    lprintf_nl "can not rename file \"%s\" to \"%s\""
      (String.escaped file.impl_file_best_name) (String.escaped real_name)
  else begin
    file.impl_file_best_name <- real_name;
    if name <> real_name then
      lprintf_nl "wanted new name \"%s\" changed to \"%s\" due to system limitations"
        (String.escaped name) (String.escaped file.impl_file_best_name);
    if !verbose && old_name <> file.impl_file_best_name then
      lprintf_nl "best_name of \"%s\" changed to \"%s\""
        (String.escaped old_name) (String.escaped file.impl_file_best_name)
  end

let set_file_format (file : file) format =
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

let file_files (file : file) =
  let impl = as_file_impl file in
  impl.impl_file_ops.op_file_files impl.impl_file_val impl

let file_all_sources file =
  let impl = as_file_impl file in
  try impl.impl_file_ops.op_file_all_sources impl.impl_file_val with _ -> []

let file_active_sources file =
  let impl = as_file_impl file in
  try impl.impl_file_ops.op_file_active_sources impl.impl_file_val with _ -> []

(* Default for networks that don't implement it *)
let default_file_print_sources_html file buf =
  let cfile = as_file file in
  let allsources = ref (file_all_sources cfile) in
  if List.length !allsources > 0 then begin

    html_mods_table_header buf "sourcesTable" "sources al" [
        ( "1", "srh br ac", "Client number", "Num" ) ;
        ( "0", "srh br", "Client Name", "Name" ) ;
        ( "0", "srh br", "IP address", "IP address" ) ;
        ( "0", "srh br", "Client software", "CS" ) ;
        ( "1", "srh ar", "Total UL bytes to this client for all files", "UL" ) ;
        ( "1", "srh ar br", "Total DL bytes from this client for all files", "DL" ) ; ];

    html_mods_cntr_init ();

    List.iter (fun c ->
        let cinfo = client_info c in
        let addr = 
        (match cinfo.GuiTypes.client_kind with
          Indirect_location (_, _, ip, port)
        | Known_location (ip, port) -> Printf.sprintf "%s:%d" (Ip.to_string ip) port
        )
        in

        Printf.bprintf buf "\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr());

        html_mods_td buf [
          ("", "sr br ar", Printf.sprintf "%d" (client_num c));
          ("", "sr br", cinfo.GuiTypes.client_name);
          ("", "sr br", addr);
          ("", "sr br", cinfo.GuiTypes.client_software);
          ("", "sr ar", (size_of_int64 cinfo.GuiTypes.client_uploaded));
          ("", "sr ar br", (size_of_int64 cinfo.GuiTypes.client_downloaded)); ];

        Printf.bprintf buf "\\</tr\\>";

    ) !allsources;

    Printf.bprintf buf "\\</table\\>\\</div\\>\\<br\\>";

  end


let file_print_sources_html (file : file) buf =
  let file = as_file_impl file in
  try file.impl_file_ops.op_file_print_sources_html file.impl_file_val buf with _ ->
    default_file_print_sources_html file buf

let file_print_html file buf =
  let impl = as_file_impl file in
  impl.impl_file_ops.op_file_print_html impl.impl_file_val buf

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

let add_file_downloaded file n =
  let impl = as_file_impl file in
  impl.impl_file_downloaded <- Int64.add impl.impl_file_downloaded n;
(* you cannot remove received bytes *)
  if Int64.compare n Int64.zero > 0 then
    impl.impl_file_received <- Int64.add impl.impl_file_received n;
  file_must_update_downloaded (as_file impl)

let file_size file = 
  (as_file_impl file).impl_file_size

let file_fd file =
  match (as_file_impl file).impl_file_fd with
    | Some fd -> fd
    | None -> raise Not_found

let file_disk_name file =
  Unix32.filename (file_fd file)

let set_file_fd file fd =
  (as_file_impl file).impl_file_fd <- Some fd

let set_file_disk_name file filename =
  let orig_fd = file_fd file in
  Unix32.rename orig_fd filename

let file_downloaded file = (as_file_impl file).impl_file_downloaded

let file_network file =
  (as_file_impl file).impl_file_ops.op_file_network

let file_priority file =
  (as_file_impl file).impl_file_priority

let set_file_priority file p =
  let impl = as_file_impl file in
  if impl.impl_file_priority <> p then begin
      impl.impl_file_priority <- p;
      impl.impl_file_ops.op_file_set_priority  impl.impl_file_val p;
      file_must_update file
    end

let file_magic file =
  (as_file_impl file).impl_file_magic
  
let set_file_magic file magic =
  match magic with
    None -> ()
  | Some magic -> (as_file_impl file).impl_file_magic <- Some (HashMagic.merge files_magic magic)

let check_magic file =
  let check file =
    match Magic.M.magic_fileinfo (file_disk_name file) false with
      None -> ()
    | Some magic -> set_file_magic file (Some magic)
  in
  let magic = file_magic file in
    match magic with
      None -> check file
    | Some magic when magic = "data" || magic = "empty" -> check file
    | _ -> ()

let set_file_last_seen file age =
  let impl = as_file_impl file in
  impl.impl_file_last_seen <- age
    
let file_preview (file : file) =
  let cmd = Printf.sprintf "%s \"%s\" \"%s\"" !!previewer
	      (file_disk_name file) (file_best_name file) in
  ignore (Sys.command cmd)

(*************************************************************************)
(*                                                                       *)
(*                         file_downloaders                              *)
(*                                                                       *)
(*************************************************************************)

module G = GuiTypes

let file_downloaders file o cnt =

      let srcs = file_active_sources file in
      let counter = ref cnt in
      List.iter (fun c ->
	match (client_state c) with
            Connected_downloading _ ->
              (begin
                if use_html_mods o then begin
                  if (client_dprint_html c o file (if !counter mod 2 == 0 then "dl-1" else "dl-2";))
		    then incr counter;
		  end
		  else begin
			client_dprint c o file;
		  end;
	       end)
          | _ -> ()
      ) srcs;

	!counter mod 2 = 0

(*************************************************************************)
(*                                                                       *)
(*                         file_print                                    *)
(*                                                                       *)
(*************************************************************************)

(* Use span for Opera DOM compatibility *)
let colored_chunks chunks =
  let chunks_length = VerificationBitmap.length chunks in
  let graph_width = min !!html_vd_chunk_graph_max_width (8 * chunks_length) in
  let ostr = Buffer.create 100 in
  Printf.bprintf ostr "\\<table cellspacing=0 cellpadding=0 width=\\\"%dpx\\\"\\>\\<tr\\>" graph_width;
  let current_output_bit = ref 0 in
  let display_bar color length =
    let resize n = (n * graph_width) / chunks_length in
    if length > 0 then begin
      let new_output_bit = !current_output_bit + length in
      let rl_resized = (resize new_output_bit) - (resize !current_output_bit) in
      (* Show only "visible" chunks *)
      if rl_resized > 0 then
	Printf.bprintf ostr
	  "\\<td class=\\\"chunk%d\\\" style=\\\"width:%dpx\\\"\\>\\</td\\>" color rl_resized;
      current_output_bit := new_output_bit
    end in

  (let color_of_state = function
    | VerificationBitmap.State_missing -> 0
    | VerificationBitmap.State_partial -> 1
    | VerificationBitmap.State_complete -> 2
    | VerificationBitmap.State_verified -> 3 in
    match !!html_vd_chunk_graph_style with
     | 0 ->
	 let previous = ref VerificationBitmap.State_missing in
	 let runlength = ref 0 in

	 VerificationBitmap.iteri (fun _ b ->
	   if b = !previous then
	     incr runlength
	   else begin
	     if !runlength > 0 then
	       display_bar (color_of_state !previous) !runlength;
	     previous := b;
	     runlength := 1
	   end
	 ) chunks;
	 display_bar (color_of_state !previous) !runlength
     | _ ->
	 let missing = ref 0 in
	 let partial = ref 0 in
	 let complete = ref 0 in
	 let verified = ref 0 in

	 VerificationBitmap.iteri (fun _ b -> 
	   match b with
             | VerificationBitmap.State_missing -> incr missing
             | VerificationBitmap.State_partial -> incr partial
             | VerificationBitmap.State_complete -> incr complete
             | VerificationBitmap.State_verified -> incr verified
	 ) chunks;
	 match !!html_vd_chunk_graph_style with
	   | 1 ->
	       display_bar 0 !missing;
	       display_bar 1 !partial;
	       display_bar 2 !complete;
	       display_bar 3 !verified
	   | 2 ->
	       display_bar 3 !verified;
	       display_bar 2 !complete;
	       display_bar 1 !partial;
	       display_bar 0 !missing
	   | _ -> ()
  );
  Buffer.add_string ostr "\\</tr\\>\\</table\\>";
  Buffer.contents ostr

let file_print file o =
  let impl = as_file_impl file in
  let info = file_info file in
  let n = impl.impl_file_ops.op_file_network in
  let buf = o.conn_buf in
  let srcs = file_all_sources file in

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

      (match info.G.file_chunks with 
      | None -> ()
      | Some chunks ->
	  let tt = "0=Missing, 1=Partial, 2=Complete, 3=Verified" in
	  let tc = VerificationBitmap.length chunks in
	  let c0 = ref 0 in
	  let c1 = ref 0 in
	  let c2 = ref 0 in
	  let c3 = ref 0 in   

	  VerificationBitmap.iteri (fun _ c ->
            match c with
            | VerificationBitmap.State_missing -> incr c0
            | VerificationBitmap.State_partial -> incr c1
            | VerificationBitmap.State_complete -> incr c2
            | VerificationBitmap.State_verified -> incr c3
	  ) chunks;
      
	  let header = Printf.sprintf "%d (%d+%d+%d+%d): " tc !c0 !c1 !c2 !c3 in

	  html_mods_td buf [
            (tt, "sr br", "Chunks");
            (tt, "sr", 
            header ^ if !!html_vd_chunk_graph then
              colored_chunks chunks
            else
              VerificationBitmap.to_string chunks
            ) ]
      );

      (match file_magic file with
        Some magic ->
	    Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-2\\\"\\>";
	    html_mods_td buf [
            ("File type computed by libmagic", "sr br", "File magic");
            ("", "sr", magic) ]
       | _ -> ());

      file_print_html file buf;
      
      Printf.bprintf buf "\\</tr\\>\\</table\\>\\</div\\>"; 
      Printf.bprintf buf "\\</td\\>\\</tr\\>\\</table\\>\\</div\\>\\<br\\>";

    end else
    begin
      Printf.bprintf buf "[%-s %5d]\n%s\n%s%s\nTotal   %10s\nPartial %10s\npriority %d\n"
        n.network_name
        (file_num file)
        (shorten (file_best_name file) 80)
	(match file_magic file with
	   Some magic -> Printf.sprintf "%s\n" magic
	 | None -> "")
        (string_of_uids info.G.file_uids)
        (Int64.to_string info.G.file_size)
        (Int64.to_string info.G.file_downloaded)
        (file_priority file);
      Printf.bprintf buf "Chunks: [%-s]\n"
	(match info.G.file_chunks with
	| None -> ""
	| Some chunks -> VerificationBitmap.to_string chunks);
      (match impl.impl_file_probable_name with
          None -> ()
        | Some filename ->
            Printf.bprintf buf "Probable name: %s\n" filename);
      List.iter (fun (name,_) -> Printf.bprintf buf "    (%s)\n" name) info.G.file_names
    end;

  (try
            
      if !!print_all_sources then begin
          if use_html_mods o then
            file_print_sources_html file buf
          else begin
            Printf.bprintf buf "%d sources:\n" (List.length srcs);
          let print_source c =
                  Printf.bprintf buf "  [%4d] " (client_num c);
                  client_bprint c buf;
          in
          List.iter print_source srcs;
        end;
        end
    
    with _ -> ())

let file_print_ed2k_link filename filesize md4hash =
  if md4hash = Md4.null then "" else
  Printf.sprintf "ed2k://|file|%s|%s|%s|/"
    (Url.encode filename) (Int64.to_string filesize) (Md4.to_string md4hash)

(*************************************************************************)
(*                                                                       *)
(*                         add_segment (internal)                        *)
(*                                                                       *)
(*************************************************************************)

let add_segment begin_pos end_pos segments =
(*  lprintf "Adding segment %Ld - %Ld\n" begin_pos end_pos;  *)
  match segments with
    [] ->
      (begin_pos, end_pos) :: segments
  | (begin_pos2, end_pos2) :: tail ->
      if begin_pos -- end_pos2 <= !!max_recover_gap then
        (begin_pos2, end_pos) :: tail
      else
        (begin_pos, end_pos) :: segments

(*************************************************************************)
(*                                                                       *)
(*                         recover_bytes                                 *)
(*                                                                       *)
(*************************************************************************)

let recover_bytes file =
  let size = file_size file in
  let fd = file_fd file in
  let len = 32768 in
  let len64 = Int64.of_int len in
  let s = String.create len in
  
  let rec iter_file_out file_pos segments =

(*    lprintf "iter_file_out pos = %Ld\n" file_pos; *)
    if file_pos = size then segments else
    
    let max64 = min len64 (size -- file_pos) in
    let max = Int64.to_int max64 in
    if try
        Unix32.read fd file_pos s 0 max;
        true
      with _ -> false
    then 
      iter_string_out (file_pos++max64) 0 max segments
    else segments

  and iter_file_in file_pos begin_pos segments =
(*    lprintf "iter_file_in file_pos = %Ld begin_pos = %Ld\n" file_pos begin_pos; *)

    if file_pos = size then
      if begin_pos <> size then
        add_segment begin_pos size segments 
      else segments
    else

    let max64 = min len64 (size -- file_pos) in
    let max = Int64.to_int max64 in
    if try
        Unix32.read fd file_pos s 0 max;
        true
      with _ -> false
    then 
      iter_string_in (file_pos++max64) 0 max begin_pos segments
    else
    if begin_pos < file_pos then 
      add_segment begin_pos file_pos segments
    else segments

  and iter_string_out file_pos pos max segments =

(*    lprintf "iter_string_out file_pos = %Ld pos = %d len = %d\n"
      file_pos pos max;

    iter_string_out_ file_pos pos max segments

  and  iter_string_out_ file_pos pos max segments = *)
    if pos = max then
      iter_file_out file_pos segments
    else
    if s.[pos] = '\000' then
      iter_string_out file_pos (pos+1) max segments
    else
    let begin_pos = file_pos -- (Int64.of_int (max - pos)) in
(*    lprintf "  Downloaded byte at %Ld\n" begin_pos; *)
    iter_string_in file_pos (pos+1) max begin_pos  segments

  and iter_string_in file_pos pos max begin_pos segments =

    (*
    lprintf "iter_string_in file_pos = %Ld pos = %d len = %d begin_pos = %Ld\n"
    file_pos pos max begin_pos;

    iter_string_in_ file_pos pos max begin_pos segments

  and iter_string_in_ file_pos pos max begin_pos segments = *)

    if pos = max then
      iter_file_in file_pos begin_pos segments
    else
    if s.[pos] = '\000' then
      let end_pos = file_pos -- (Int64.of_int (max - pos)) in
(*      lprintf "  0 byte at %Ld\n" end_pos; *)
      iter_string_out file_pos (pos+1) max 
      (add_segment begin_pos end_pos segments)
    else
      iter_string_in file_pos (pos+1) max begin_pos segments

  in
  let segments = List.rev (iter_file_out zero []) in
  (*
  lprintf "CommonFile.recover_bytes: recovered segments\n";
  List.iter (fun (begin_pos, end_pos) ->
      lprintf "   %Ld - %Ld\n" begin_pos end_pos
  ) segments;
*)
  segments

(*************************************************************************)
(*                                                                       *)
(*                         Define file_ops                               *)
(*                                                                       *)
(*************************************************************************)

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
      op_file_files = (fun _ impl -> [as_file impl]);
      op_file_pause = (fun _ -> ni_ok network "file_pause");
      op_file_resume = (fun _ -> ni_ok network "file_resume");
(*      op_file_disk_name = (fun _ -> fni network "file_disk_name"); *)
      op_file_check = (fun _ -> ni_ok network "file_check");
      op_file_recover = (fun _ -> ni_ok network "file_recover");
      op_file_set_format = (fun _ -> fni network "file_set_format");
      op_file_all_sources = (fun _ -> fni network "file_all_sources");
      op_file_active_sources = (fun _ -> fni network "file_active_sources");
      op_file_comment = (fun _ -> ni_ok network "file_comment"; "");
      op_file_set_priority = (fun _ _ -> ni_ok network "file_set_priority");
      op_file_print_html = (fun _ _ -> ni_ok network "file_print_html");
      op_file_print_sources_html = (fun _ _ -> fni network "file_print_sources_html");
      op_file_debug = (fun _ -> "");
      op_file_proposed_filenames = (fun impl -> []);
    }
  in
  let ff = (Obj.magic f : int file_ops) in
  files_ops := (ff, { ff with op_file_network = ff.op_file_network })
  :: ! files_ops;
  f

let check_file_implementations () =
  lprintf_nl "\n---- Methods not implemented for CommonFile ----";
  List.iter (fun (c, cc) ->
      let n = c.op_file_network.network_name in
      lprintf_nl "\n  Network %s" n; 
      if c.op_file_to_option == cc.op_file_to_option then 
        lprintf_nl "op_file_to_option";
      if c.op_file_info == cc.op_file_info then
        lprintf_nl "op_file_info";
      if c.op_file_commit == cc.op_file_commit then
        lprintf_nl "op_file_commit";
      if c.op_file_save_as == cc.op_file_save_as then
        lprintf_nl "op_file_save_as";
      if c.op_file_cancel == cc.op_file_cancel then
        lprintf_nl "op_file_cancel";
      if c.op_file_pause == cc.op_file_pause then
        lprintf_nl "op_file_pause";
      if c.op_file_resume == cc.op_file_resume then
        lprintf_nl "op_file_resume";
(*      if c.op_file_disk_name == cc.op_file_disk_name then
        lprintf_nl "op_file_disk_name"; *)
      if c.op_file_check == cc.op_file_check then
        lprintf_nl "op_file_check";
      if c.op_file_recover == cc.op_file_recover then
        lprintf_nl "op_file_recover";
      if c.op_file_set_format == cc.op_file_set_format then
        lprintf_nl "op_file_set_format";
      if c.op_file_all_sources == cc.op_file_all_sources then
        lprintf_nl "op_file_all_sources";
      if c.op_file_active_sources == cc.op_file_active_sources then
        lprintf_nl "op_file_active_sources";
      if c.op_file_print_html == cc.op_file_print_html then
        lprintf_nl "op_file_print_html";
      if c.op_file_print_sources_html == cc.op_file_print_sources_html then
        lprintf_nl "op_file_print_sources_html";
  ) !files_ops;
  lprint_newline ()

(*************************************************************************)
(*                                                                       *)
(*                         MAIN                                          *)
(*                                                                       *)
(*************************************************************************)

let _ =
  Heap.add_memstat "CommonFile" (fun level buf ->
      let counter = ref 0 in
      H.iter (fun f -> incr counter;
       if level > 0 then
         Printf.bprintf buf "%d sources: %s: %s \n" 
          (List.length (file_all_sources f)) 
          (file_network f).network_name
          (file_best_name f);
      ) files_by_num;
      Printf.bprintf buf "  files: %d\n" !counter;
      Printf.bprintf buf "  files_ops: %d\n" (List.length !files_ops);
      let counter = ref 0 in
      HashMagic.iter (fun _ -> incr counter) files_magic;
      Printf.bprintf buf "  files_magic: %d\n" !counter;
  )


(*************************************************************************)
(*                                                                       *)
(*                         write_block                                   *)
(*                                                                       *)
(*************************************************************************) 

let file_write file offset s pos len =
(*
      lprintf "DOWNLOADED: %d/%d/%d\n" pos len (String.length s);
      AnyEndian.dump_sub s pos len;
*)

  if !!CommonOptions.buffer_writes then 
    Unix32.buffered_write_copy (file_fd file) offset s pos len
  else
    Unix32.write  (file_fd file) offset s pos len

let file_verify file key begin_pos end_pos =
  Unix32.flush_fd (file_fd file);
  if !verbose_md4 then begin
      lprintf_nl "Checksum to compute: %Ld-%Ld of %s" begin_pos end_pos
        (file_disk_name file);
    end;
  try
    let computed = match key with
      | Ed2k md4 ->
          let result = Md4.digest_subfile (file_fd file)
            begin_pos (end_pos -- begin_pos) in
          Ed2k result
      | Sha1 sha1 ->
          let result = Sha1.digest_subfile (file_fd file)
            begin_pos (end_pos -- begin_pos) in
          Sha1 result
      | TigerTree ttr ->
          let result = TigerTree.digest_subfile (file_fd file)
            begin_pos (end_pos -- begin_pos) in
          TigerTree result
      | _ -> 
          raise Not_found
    in
    let result = computed = key in
    if !verbose_md4 then begin
        lprintf_nl "Checksum computed: %s against %s = %s"
          (string_of_uid key) 
        (string_of_uid computed)
        (if result then "VERIFIED" else "CORRUPTED");
      end;
    result
  with
    | Not_found -> raise Not_found
    | e -> lprintf_nl "Checksum computation failed: Exception: %s" (Printexc2.to_string e);
    false

let file_mtime file = Unix32.mtime64 (file_fd file)
  
let file_copy file1 file2 pos1 pos2 size =
  Unix32.copy_chunk (file_fd file1)  (file_fd file2)
  pos1 pos2 (Int64.to_int size)

let propose_filename file =
(*  lprintf "propose_filename...\n"; *)
  let impl = as_file_impl file in
  let filenames = impl.impl_file_ops.op_file_proposed_filenames 
      impl.impl_file_val in
(*  lprintf "  %d names proposed\n" (List.length filenames); *)
  let h = Hashtbl.create 113 in
  let filenames = List2.tail_map (fun filename ->
        filename, String2.stem filename) filenames in
  List.iter (fun (_, words) ->
      List.iter (fun w ->
          try
            let r = Hashtbl.find h w in
            incr r
          with _ ->
              Hashtbl.add h w (ref 0)
      ) words
  ) filenames;
  let filenames = List2.tail_map (fun (filename, words) ->
        let initial_value = 
          if String2.starts_with filename "urn:" then 0 else 1
        in
        let v = ref initial_value in
        List.iter (fun w ->
            v := !v + ! (Hashtbl.find h w)
        ) words;
(*        lprintf "    %d FOR %s\n" !v filename; *)

        filename, !v
    ) filenames in
  let filenames = List.sort (fun (_,s1) (_,s2) ->
        compare s2 s1
    ) filenames in
  match filenames with
    [] -> ()
  | (filename,_) :: _ -> impl.impl_file_probable_name <- Some filename

let propose_filenames () =
  H.iter (fun file -> propose_filename file)  files_by_num

let forceable_download = ref []

(*************************************************************************)
(*                                                                       *)
(*                         Protect globals                               *)
(*                                                                       *)
(*************************************************************************)

let com_files_by_num = files_by_num
let files_by_num = ()

let impl_file_info impl =
  let module T = GuiTypes in
  {
    T.file_fields = T.Fields_file_info.all;
    T.file_comment = impl.impl_file_comment;
    T.file_name = impl.impl_file_best_name;
    T.file_num = impl.impl_file_num;
    T.file_size = impl.impl_file_size;
    T.file_downloaded = impl.impl_file_downloaded;
    T.file_state = impl.impl_file_state;
    T.file_download_rate = file_download_rate impl;
    T.file_priority = impl.impl_file_priority;
    T.file_age = impl.impl_file_age;
    T.file_last_seen = impl.impl_file_last_seen;

    T.file_network = 0;
    T.file_names = [];
    T.file_md4 = Md4.null;
    T.file_all_sources = 0;
    T.file_active_sources = 0;
    T.file_sources = None;
    T.file_chunks = None;
    T.file_availability = [];
    T.file_format = FormatNotComputed 0;
    T.file_chunks_age = [||];
    T.file_uids = [];
    T.file_sub_files = [];
  }
