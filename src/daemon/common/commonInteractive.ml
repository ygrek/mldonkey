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
open CommonOptions
open BasicSocket  
open TcpBufferedSocket
open CommonGlobals
open CommonSearch
open Options
open CommonNetwork
open CommonResult
open CommonServer
open CommonTypes

  
(* ripped from gui_misc *)

let ko = 1024.0 
let mo = ko *. ko 
let go = mo *. ko 
let tob = go *. ko 

let size_of_int64 size =
  if !!html_mods_human_readable then
    let f = Int64.to_float size in
	if f > tob then
      Printf.sprintf "%.2fT" (f /. tob)
	else
     if f > go then
      Printf.sprintf "%.2fG" (f /. go)
     else
      if f > mo then
      Printf.sprintf "%.1fM" (f /. mo)
      else
     if f > ko then
       Printf.sprintf "%.1fk" (f /. ko)
     else
       Int64.to_string size
  else
    Int64.to_string size

  
let days = ref 0      
let hours = ref 0    
  
let cut_messages f sock nread =
  let b = buf sock in
  try
    while b.len >= 4 do
      let msg_len = LittleEndian.get_int b.buf b.pos in
      if b.len >= 4 + msg_len then
        begin
          let s = String.sub b.buf (b.pos+4) msg_len in
          buf_used sock (msg_len + 4);
          let opcode = LittleEndian.get_int16 s 0 in
          (f opcode s : unit)
        end
      else raise Not_found
    done
  with Not_found -> ()
  
let load_web_infos () =
(* Try to connect the redirector to get interesting information, since we
are not allowed to use savannah anymore. The redirector should be able to
support the charge, at least, currently. *)
  let (name, port) = !!mlnet_redirector in
  Ip.async_ip name (fun ip ->
      try
        lprintf "connecting to redirector\n";
        let sock = TcpBufferedSocket.connect "connect redirector"
            (Ip.to_inet_addr ip) port            
            (fun sock event ->
              match event with
              | BASIC_EVENT (LTIMEOUT | RTIMEOUT) -> 
                  TcpBufferedSocket.close sock "timeout redirector"
              | _ -> ())
        in
        TcpBufferedSocket.set_rtimeout sock 30.;
        let to_read = ref [] in
        set_reader sock (cut_messages (fun opcode s ->
              lprintf "redirector info received\n";
              let module L = LittleEndian in
              
              let motd_html_s, pos = L.get_string16 s 2 in
              let servers_met_s, pos = L.get_string16 s pos in
              let peers_ocl_s, pos = L.get_string16 s pos in
              let motd_conf_s, pos = L.get_string16 s pos in
              
              motd_html =:= motd_html_s;
              
              let servers_met_file = Filename.temp_file "servers" ".met" in
              File.from_string servers_met_file servers_met_s;
              load_file "servers.met" servers_met_file;

              let peers_ocl_file = Filename.temp_file "peers" ".ocl" in
              File.from_string peers_ocl_file peers_ocl_s;
              load_file "ocl" peers_ocl_file;

              let motd_conf_file = Filename.temp_file "motd" ".conf" in
              File.from_string motd_conf_file motd_conf_s;
              load_file "motd.conf" motd_conf_file;              
              
              lprintf "Redirector info loaded\n";
              TcpBufferedSocket.close sock "info received"
          ))
      with e -> 
          lprintf "Exception %s while connecting redirector\n"
            (Printexc2.to_string e)
  );
  
  if !!network_update_url <> "" then begin
    load_url "motd.html" (Filename.concat !!network_update_url "motd.html");
    load_url "motd.conf" (Filename.concat !!network_update_url "motd.conf");
  end;
  List.iter (fun (kind, period, url) ->
      if !days mod period = 0 then load_url kind url
  ) !!CommonOptions.web_infos

let display_vd = ref false
  
let print_results o =
  let buf = o.conn_buf in
  let user = o.conn_user in
  match user.ui_user_searches with
    [] -> Printf.bprintf buf "No search to print\n"
  | q :: _ ->
      user.ui_last_search <- Some q;
      user.ui_last_results <- [];
      let counter = ref 1 in
      Intmap.iter (fun r_num (count,r) ->
          CommonResult.result_print r !counter o;
          user.ui_last_results <- (!count, r) :: user.ui_last_results;
          incr counter;
      ) q.search_results

      
let download_file o arg =
  let user = o.conn_user in
  let buf = o.conn_buf in
  Printf.bprintf buf "%s\n" (
    try
      match user.ui_last_search with
        None -> "no last search"
      | Some s ->
          let result = List.assoc (int_of_string arg) user.ui_last_results  in
          CommonResult.result_download result [] false; 
          "download started"
    with
    | Failure s -> s
    | _ -> "could not start download"
  )
  
let start_search user query buf =
  let s = CommonSearch.new_search user query in
  networks_iter (fun r -> r.op_network_search s buf);
  s
  
let print_connected_servers o =
  let buf = o.conn_buf in
  networks_iter (fun r ->
      try
       let list = network_connected_servers r in
		if r.network_name <> "BitTorrent" then begin	
       if use_html_mods o then Printf.bprintf buf "\\<div class=servers\\>";
       Printf.bprintf buf "--- Connected to %d servers on the %s network ---\n"
         (List.length list) r.network_name;
       if use_html_mods o then Printf.bprintf buf "\\</div\\>";
		end;
       if use_html_mods o && List.length list > 0 then server_print_html_header buf;

      let counter = ref 0 in  
       List.iter (fun s ->
        incr counter;
		if use_html_mods o then 
         Printf.bprintf buf "\\<tr class=\\\"%s\\\"\\>"     
          (if (!counter mod 2 == 0) then "dl-1" else "dl-2");
        server_print s o;
       ) (List.sort (fun s1 s2 -> compare (server_num s1) (server_num s2)) list);

        if use_html_mods o && List.length list > 0 then 
           Printf.bprintf buf "\\</table\\>\\</div\\>";
       with e ->
           Printf.bprintf  buf "Exception %s in print_connected_servers"
             (Printexc2.to_string e);

  )
  
let send_custom_query user buf s args = 
  let query = s.GuiTypes.search_query in
  try
    let q = List.assoc query !!CommonComplexOptions.customized_queries in
    let args = ref args in
    let get_arg arg_name = 
(*      lprintf "Getting %s" arg_name; lprint_newline (); *)
      match !args with
        (label, value) :: tail ->
          args := tail;
          if label = arg_name then value else begin
              Printf.bprintf buf "Error expecting argument %s instead of %s" arg_name label;  
              raise Exit
            end
      | _ ->
          Printf.bprintf buf "Error while expecting argument %s" arg_name;
          raise Exit
    in
    let rec iter q =
      match q with
      | Q_COMBO _ -> assert false
      | Q_KEYWORDS _ -> 
          let value = get_arg "keywords" in
          want_and_not andnot (fun w -> QHasWord w) QNone value
          
      | Q_AND list ->
          begin
            let ands = ref [] in
            List.iter (fun q ->
                try ands := (iter q) :: !ands with _ -> ()) list;
            match !ands with
              [] -> raise Not_found
            | [q] -> q
            | q1 :: tail ->
                List.fold_left (fun q1 q2 -> QAnd (q1,q2)) q1 tail
          end

      | Q_HIDDEN list ->
          begin
            let ands = ref [] in
            List.iter (fun q ->
                try ands := (iter q) :: !ands with _ -> ()) list;
            match !ands with
              [] -> raise Not_found
            | [q] -> q
            | q1 :: tail ->
                List.fold_left (fun q1 q2 -> QAnd (q1,q2)) q1 tail
          end
          
      | Q_OR list ->
          begin
            let ands = ref [] in
            List.iter (fun q ->
                try ands := (iter q) :: !ands with _ -> ()) list;
            match !ands with
              [] -> raise Not_found
            | [q] -> q
            | q1 :: tail ->
                List.fold_left (fun q1 q2 -> QOr (q1,q2)) q1 tail
          end
          
      | Q_ANDNOT (q1, q2) ->
          begin
            let r1 = iter q1 in
            try
              QAndNot(r1, iter q2)
            with Not_found -> r1
          end
          
      | Q_MODULE (s, q) -> iter q

      | Q_MINSIZE _ ->
          let minsize = get_arg "minsize" in
          let unit = get_arg "minsize_unit" in
          if minsize = "" then raise Not_found;
          let minsize = Int64.of_string minsize in
          let unit = Int64.of_string unit in
          QHasMinVal ("size", Int64.mul minsize unit)

      | Q_MAXSIZE _ ->
          let maxsize = get_arg "maxsize" in
          let unit = get_arg "maxsize_unit" in
          if maxsize = "" then raise Not_found;
          let maxsize = Int64.of_string maxsize in
          let unit = Int64.of_string unit in
          QHasMaxVal ("size", Int64.mul maxsize unit)

      | Q_FORMAT _ ->
          let format = get_arg "format" in
          let format_propose = get_arg "format_propose" in
          let format = if format = "" then 
              if format_propose = "" then raise Not_found
              else format_propose
            else format in
          want_comb_not andnot
            or_comb
            (fun w -> QHasField("format", w)) QNone format
          
      | Q_MEDIA _ ->
          let media = get_arg "media" in
          let media_propose = get_arg "media_propose" in
          let media = if media = "" then 
              if media_propose = "" then raise Not_found
              else media_propose
            else media in
          QHasField("type", media)

      | Q_MP3_ARTIST _ ->
          let artist = get_arg "artist" in
          if artist = "" then raise Not_found;
          want_comb_not andnot and_comb 
            (fun w -> QHasField("Artist", w)) QNone artist
          
      | Q_MP3_TITLE _ ->
          let title = get_arg "title" in
          if title = "" then raise Not_found;
          want_comb_not andnot and_comb 
            (fun w -> QHasField("Title", w)) QNone title
          
      | Q_MP3_ALBUM _ ->
          let album = get_arg "album" in
          if album = "" then raise Not_found;
          want_comb_not andnot and_comb 
            (fun w -> QHasField("Album", w)) QNone album
          
      | Q_MP3_BITRATE _ ->
          let bitrate = get_arg "bitrate" in
          if bitrate = "" then raise Not_found;
          QHasMinVal("bitrate", Int64.of_string bitrate)

    in
    try
      let request = CommonGlobals.simplify_query (iter q) in
      Printf.bprintf buf "Sending query !!!";
      ignore (start_search user {s with GuiTypes.search_query = request } buf)
    with
      Not_found ->
        Printf.bprintf buf "Void query %s" query        
  with 
    Not_found ->
      Printf.bprintf buf "No such custom search %s" query
  | Exit -> ()
  | e -> 
      Printf.bprintf buf "Error %s while parsing request"
        (Printexc2.to_string e)

let all_simple_options () =
  let options = ref (simple_options downloads_ini) in
  networks_iter_all (fun r ->
      match r.network_config_file with
        None -> ()
      | Some opfile ->
          let args = simple_options opfile in
          let prefix = r.network_prefix () in
          let args = 
            if prefix = "" then args else 
              List2.tail_map (fun (arg, value) ->
                (Printf.sprintf "%s%s" prefix arg, value)) 
            args
          in
          options := !options @ args
  );
  !options

let all_simple_options_html () =
  let options = ref (simple_options_html downloads_ini) in
  networks_iter_all (fun r ->
      match r.network_config_file with
        None -> ()
      | Some opfile ->
          let args = simple_options_html opfile in
          let prefix = r.network_prefix () in
          let args = 
            if prefix = "" then args else 
              List2.tail_map (fun (arg, value, def, help) ->
                (Printf.sprintf "%s%s" prefix arg, value, def, help)) 
            args
          in
          options := !options @ args
  );
  !options

let apply_on_fully_qualified_options name f =
  if !verbose then begin
      lprintf "For option %s" name; lprint_newline ();
    end;
  let rec iter prefix opfile =
    let args = simple_options opfile in
    List.iter (fun (old_name, old_value) ->
        let new_name = Printf.sprintf "%s%s" prefix old_name in
        if new_name = name then
          (f opfile old_name old_value; raise Exit))
    args
  in
  try
    iter "" downloads_ini;
    if not (networks_iter_all_until_true (fun r ->
            try
              match r.network_config_file with
                None -> false
              | Some opfile ->
                  let prefix = r.network_prefix () in
                  iter prefix opfile;
                  false
            with Exit -> true
        )) then begin
        lprintf "Could not set option %s" name; lprint_newline ();
        raise Not_found
      end
  with Exit -> ()


let set_fully_qualified_options name value =
  apply_on_fully_qualified_options name (fun opfile old_name old_value ->
    set_simple_option opfile old_name value)


let get_fully_qualified_options name =
  let value = ref None in
  (try
      apply_on_fully_qualified_options name (fun opfile old_name old_value ->
      value := Some (get_simple_option opfile old_name)
      );
    with _ -> ());
  match !value with
    None -> "????"
  | Some s -> s

let add_item_to_fully_qualified_options name value =
  ()

let del_item_from_fully_qualified_options name value = 
  ()

let keywords_of_query query =
  let keywords = ref [] in
  
  let rec iter q = 
    match q with
    | QOr (q1,q2) 
    | QAnd (q1, q2) -> iter q1; iter q2
    | QAndNot (q1,q2) -> iter q1 
    | QHasWord w -> keywords := String2.split_simplify w ' '
    | QHasField(field, w) ->
        begin
          match field with
            "Album"
          | "Title"
          | "Artist"
          | _ -> keywords := String2.split_simplify w ' '
        end
    | QHasMinVal (field, value) ->
        begin
          match field with
            "bitrate"
          | "size"
          | _ -> ()
        end
    | QHasMaxVal (field, value) ->
        begin
          match field with
            "bitrate"
          | "size"
          | _ -> ()
        end
    | QNone ->
        lprintf "LimewireInteractive.start_search: QNone in query\n";
        ()
  in
  iter query;
  !keywords

let gui_options_panels = ref ([] : (string * (string * string * string) list) list)
  
let register_gui_options_panel name panel =
  if not (List.mem_assoc name !gui_options_panels) then
    gui_options_panels := (name, panel) :: !gui_options_panels
    
    
let _ =
  add_infinite_option_timer filter_search_delay (fun _ ->
      if !!filter_search then begin
(*          lprintf "Filter search results"; lprint_newline (); *)
          List.iter (fun user ->
              List.iter  (fun s -> CommonSearch.Filter.find s) 
              user.ui_user_searches;
          ) !ui_users
        end;
      CommonSearch.Filter.clear ()
  )
  
let search_add_result s r =
  if not !!filter_search then begin
(*      lprintf "Adding result to filter"; lprint_newline (); *)
      CommonSearch.search_add_result_in s r
    end
  else
    CommonSearch.Filter.add (as_result r)

let main_options = ref ([] : (string * Arg.spec * string) list)
  
let add_main_options list =
  main_options := !main_options @ list

  
(*************************************************************

Every minute, sort the files by priority, and test if the
files with the highest priority are in FileDownloading state,
and the ones with lowest priority in FileQueued state, if there
is a max_concurrent_downloads constraint.

In the future, we could try to mix this with the multi-users
system to give some fairness between downloads of different 
users.
  
**************************************************************)  

open CommonFile
  
let force_download_quotas () = 
  let files = List.sort (fun f1 f2 -> 
        let v = file_priority f2 - file_priority f1 in
        if v <> 0 then v else
        let s1 = file_downloaded f1 in
        let s2 = file_downloaded f2 in
        if s1 = s2 then 0 else
        if s2 > s1 then 1 else -1
        )
    !!CommonComplexOptions.files in
  
  let rec iter list priority files ndownloads nqueued =
    match list, files with
      [], [] -> ()
    | [], _ -> 
        iter_line list priority files ndownloads nqueued
    | f :: tail , _ :: _ when file_priority f < priority ->
        iter_line list priority files ndownloads nqueued
    | f :: tail, files ->
        match file_state f with
          FileDownloading ->
            iter tail (file_priority f) (f :: files) (ndownloads+1) nqueued
        | FileQueued ->
            iter tail (file_priority f) (f :: files) ndownloads (nqueued+1)
        | _ ->
            iter tail (file_priority f) files ndownloads nqueued
        
  and iter_line list priority files ndownloads nqueued = 
    if ndownloads > !!max_concurrent_downloads then
      match files with
        [] -> assert false
      | f :: tail ->
          match file_state f with
            FileDownloading ->
              set_file_state f FileQueued;
              iter_line list priority tail (ndownloads-1) nqueued
          | _ -> iter_line list priority tail ndownloads (nqueued-1)
    else
    if ndownloads < !!max_concurrent_downloads && nqueued > 0 then
      match files with
        [] -> assert false
      | f :: tail ->
          match file_state f with
            FileQueued ->
              set_file_state f FileDownloading;
              iter_line list priority tail (ndownloads+1) (nqueued-1)
          | _ -> iter_line list priority tail ndownloads nqueued      
    else
      iter list priority [] ndownloads nqueued
    
  in
  iter files max_int [] 0 0
  
let _ =
  option_hook max_concurrent_downloads (fun _ ->
      force_download_quotas ()   
  )
