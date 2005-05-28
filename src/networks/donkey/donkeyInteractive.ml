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
open TcpBufferedSocket

open GuiTypes

open CommonShared
open CommonServer
open CommonResult
open CommonClient
open CommonUser
open CommonInteractive
open CommonNetwork
open CommonDownloads  
open CommonTypes
open CommonComplexOptions
open CommonFile
  
open DonkeySearch
open DonkeyMftp
open DonkeyProtoCom
open DonkeyServers
open DonkeyOneFile
open DonkeyFiles
open DonkeyComplexOptions
open DonkeyTypes
open DonkeyOptions
open DonkeyGlobals
open DonkeyClient
open CommonGlobals
open CommonOptions
open DonkeyStats
  
let result_name r =
  match r.result_names with
    [] -> None
  | name :: _ -> Some name


let op_file_proposed_filenames file = 
  if !verbose then lprintf "op_file_proposed_filenames\n";
  List2.tail_map fst file.file_filenames
      
let reconnect_all file =
  DonkeyProtoOvernet.Overnet.recover_file file;
  DonkeyProtoKademlia.Kademlia.recover_file file;
  
(* This is expensive, no ? *)
(*  DonkeySources.reschedule_sources file; *)
  List.iter (fun s ->
      match s.server_sock, server_state s with
      | Connection sock, (Connected _ | Connected_downloading _) ->
          s.server_waiting_queries <- file :: s.server_waiting_queries
      | _ -> ()
  ) (connected_servers())
    
let forget_search s =  
  if !xs_last_search = s.search_num then begin
      xs_last_search := (-1);
      xs_servers_list := [];
    end;
  DonkeyProtoKademlia.Kademlia.forget_search s;
  DonkeyProtoOvernet.Overnet.forget_search s
  
  
let load_server_met filename =
  try
    let module S = DonkeyImport.Server in
    let s = File.to_string filename in
    let ss = S.read s in
    List.iter (fun r ->
        try
          let server = check_add_server r.S.ip r.S.port in
          List.iter (fun tag ->
              match tag with
                { tag_name = Field_UNKNOWN "name"; tag_value = String s } -> 
                  server.server_name <- s;
              |  { tag_name = Field_UNKNOWN "description" ; tag_value = String s } ->
                  server.server_description <- s
              | _ -> ()
          ) r.S.tags
        with _ -> ()
    ) ss;
    List.length ss
  with e ->
      lprintf "Exception %s while loading %s\n" (Printexc2.to_string e)
      filename;
      0

let already_done = Failure "File already downloaded (use 'force_download' if necessary)"
      
let really_query_download filenames size md4 location old_file absents =
  
  if !verbose then lprintf "really_query_download..................... \n";
  begin
    try
      let file = Hashtbl.find files_by_md4 md4 in
      if file_state file = FileDownloaded then 
        raise already_done;
    with Not_found -> ()
  end;
  
  List.iter (fun file -> 
      if file.file_md4 = md4 then raise already_done) 
  !current_files;

  let file_diskname = Filename.concat !!temp_directory 
    (file_string_of_uid (Ed2k md4)) in
  begin
    match old_file with
    | Some filename when file_diskname <> filename ->
        if Sys.file_exists filename
           && not (Sys.file_exists file_diskname)
          then
            (try
              lprintf "Renaming edonkey temp-file from %s to %s\n"
                  filename file_diskname;
              Unix2.rename filename file_diskname;
              Unix.chmod file_diskname 0o644;
              with e ->
                lprintf "Could not rename %s to %s: exception %s\n"
                  filename file_diskname (Printexc2.to_string e);
            )
        else lprintf "THERE IS SOME PROBLEM WITH RECOVERING TEMP-FILES, THAT COULD CAUSE FILE-CORRUPTION!!!!!!!!!!! filename: %s  exists:%b file_diskname: %s  exists:%b\n"
               filename (Sys.file_exists filename)
               file_diskname (Sys.file_exists file_diskname);
    | _ -> ()
  end;

(* TODO RESULT  let other_names = DonkeyIndexer.find_names md4 in *)
  let filenames =
    let other_names = [] in
    let filenames = List.fold_left (fun names name ->
          if List.mem name names then names else names @ [name]
      ) filenames other_names in 
    (List.map (fun name -> name , { ips=[]; nips=0}) filenames)
  in

  let file = new_file file_diskname FileDownloading md4 size filenames true in
  begin
    match absents with
      None -> ()
    | Some absents -> 
        match file.file_swarmer with
          None -> assert false
        | Some swarmer ->
            let absents = Sort.list (fun (p1,_) (p2,_) -> p1 <= p2) absents in
            Int64Swarmer.set_absent swarmer absents
  end;

  DonkeyProtoOvernet.Overnet.recover_file file;
  DonkeyProtoKademlia.Kademlia.recover_file file;
  
  current_files := file :: !current_files;
(*  !file_change_hook file; *)
(*  set_file_size file (file_size file); *)
  
  DonkeyNeighbours.recover_downloads [file];
  
  List.iter (fun s ->
      add_query_location file s
  ) (connected_servers());

  (try
      let servers = Hashtbl.find_all udp_servers_replies file.file_md4 in
      List.iter (fun s ->
          udp_server_send s (DonkeyProtoUdp.QueryLocationUdpReq [file.file_md4])
      ) servers
    with _ -> ());
  
  (match location with
      None -> ()
    | Some num ->
        let c = client_find num in
        client_connect c
        (*
        try 
          let c = find_client num in
          (match c.client_kind with
              Indirect_location -> 
                if not (Intmap.mem c.client_num file.file_indirect_locations) then
                  file.file_indirect_locations <- Intmap.add c.client_num c
                    file.file_indirect_locations
            
            | _ -> 
                if not (Intmap.mem c.client_num file.file_known_locations) then
                  new_known_location file c
          );
          if not (List.memq file c.client_files) then
            c.client_files <- file :: c.client_files;
          match client_state c with
            NotConnected -> 
              connect_client !!client_ip [file] c
          | Connected_busy | Connected_idle | Connected_queued ->
              begin
                match c.client_sock with
                  None -> ()
                | Some sock -> 
                    DonkeyClient.query_files c sock [file]
              end
          | _ -> ()
with _ -> ()
*)
  );
  as_file file

let query_download filenames size md4 location old_file absents force =
  
(* TODO RESULT
  if not force then
    List.iter (fun m -> 
        if m = md4 then begin
            let r = try
                DonkeyIndexer.find_result md4
              with _ -> 
(* OK, we temporary create a result corresponding to the
file that should have been download, but has already been downloaded *)
                  
                  
                  let r = { 
                      result_num = 0;
                      result_network = network.network_num;
                      result_md4 = md4;
                      result_names = filenames;
                      result_size = size;
                      result_tags = [];
                      result_type = "";
                      result_format = "";
                      result_comment = "";
                      result_done = false;
                    } in
                  DonkeyIndexer.index_result_no_filter r 
            in
            aborted_download := Some (result_num (as_result r.result_result));
            raise already_done
          end) 
    !!old_files; *)
  really_query_download filenames size md4 location old_file absents

let result_download r filenames force =  
  let rec iter uids =
    match uids with
      [] -> raise IgnoreNetwork
    | uid :: tail ->
        match Uid.to_uid uid with
          Ed2k md4 ->
            query_download filenames r.result_size md4 None None None force
        | _  -> iter tail
  in
  iter r.result_uids
  
let load_prefs filename = 
  try
    let module P = DonkeyImport.Pref in
    let s = File.to_string filename in
    let t = P.read s in
    t.P.client_tags, t.P.option_tags
  with e ->
      lprintf "Exception %s while loading %s\n" (Printexc2.to_string e)
      filename;
      [], []
      
let import_temp temp_dir =  
  let list = Unix2.list_directory temp_dir in
  let module P = DonkeyImport.Part in
  List.iter (fun filename ->
      try
        if Filename2.last_extension filename = ".part" then
          let filename = Filename.concat temp_dir filename in
          let met = filename ^ ".met" in
          if Sys.file_exists met then
            let s = File.to_string met in
            let f = P.read s in
            let filenames = ref [] in
            let size = ref Int64.zero in
            List.iter (fun tag ->
                match tag with
                  { tag_name = Field_Filename; tag_value = String s } ->
                    lprintf "Import Donkey %s\n" s; 
                    
                    filenames := s :: !filenames;
                | { tag_name = Field_Size; tag_value = Uint64 v } ->
                    size := v
                | _ -> ()
            ) f.P.tags;
            let file = query_download !filenames !size f.P.md4 None 
              (Some filename) (Some (List.rev f.P.absents)) true;
            in 
            ()
      with _ -> ()
  ) list
  
  
let import_config dirname =
  ignore (load_server_met (Filename.concat dirname "server.met"));
  let ct, ot = load_prefs (Filename.concat dirname "pref.met") in
  let temp_dir = ref (Filename.concat dirname "temp") in

  List.iter (fun tag ->
      match tag with
      | { tag_name = Field_UNKNOWN "name"; tag_value = String s } ->
          login =:=  s
      | { tag_name = Field_UNKNOWN "port"; tag_value = Uint64 v } ->
          donkey_port =:=  Int64.to_int v
      | _ -> ()
  ) ct;

  List.iter (fun tag ->
      match tag with
      | { tag_name = Field_UNKNOWN "temp"; tag_value = String s } ->
          if Sys.file_exists s then (* be careful on that *)
            temp_dir := s
          else (lprintf "Bad temp directory, using default\n";
              )
      | _ -> ()
  ) ot;

  import_temp !temp_dir
  
let broadcast msg =
  let s = msg ^ "\n" in
  let len = String.length s in
  List.iter (fun sock ->
      TcpBufferedSocket.write sock s 0 len
  ) !user_socks

  (*
let saved_name file =
  let name = longest_name file in
(*  if !!use_mp3_tags then
    match file.file_format with
      Mp3 tags ->
        let module T = Mp3tag in
        let name = match name.[0] with
            '0' .. '9' -> name
          | _ -> Printf.sprintf "%02d-%s" tags.T.tracknum name
        in
        let name = if tags.T.album <> "" then
            Printf.sprintf "%s/%s" tags.T.album name
          else name in
        let name = if tags.T.artist <> "" then
            Printf.sprintf "%s/%s" tags.T.artist name
          else name in
        name          
    | _ -> name
else *)
  name
    *)

let print_file buf file =
  Printf.bprintf buf "[%-5d] %s %10Ld %32s %s" 
    (file_num file)
  (file_best_name file)
  (file_size file)
  (Md4.to_string file.file_md4)
  (if file_state file = FileDownloaded then
      "Done" else
      Int64.to_string (file_downloaded file));
  Buffer.add_char buf '\n';
  Printf.bprintf buf "Connected clients:\n";
  let f _ c =
    match c.client_kind with
      Direct_address (ip, port) ->
        Printf.bprintf  buf "[%-5d] %12s %-5d    %s\n"
          (client_num c)
        (Ip.to_string ip)
        port
          (match c.client_source.DonkeySources.source_sock with
            NoConnection  -> 
              string_of_date (c.client_source.DonkeySources.source_age)
          | ConnectionWaiting _ -> "Connecting"
          | Connection _ -> "Connected")
    | _ ->
        Printf.bprintf  buf "[%-5d] %12s            %s\n"
          (client_num c)
        "Indirect"
          (match c.client_source.DonkeySources.source_sock with
            NoConnection -> 
              string_of_date (c.client_source.DonkeySources.source_age)
          | ConnectionWaiting _ -> "Connecting"
          | Connection _ -> "Connected")
  in

(* Intmap.iter f file.file_sources; *)
  match file.file_swarmer with
    None -> ()
  | Some swarmer ->
      let bitmap = Int64Swarmer.verified_bitmap swarmer in
      Printf.bprintf buf "\nChunks: %s\n" bitmap      
  
let recover_md4s md4 =
  let file = find_file md4 in  
  match file.file_swarmer with
    None -> ()
  | Some swarmer ->
      Int64Swarmer.verify_all_chunks swarmer false
  
  (*
  if file.file_chunks <> [||] then
    for i = 0 to file.file_nchunks - 1 do
      file.file_chunks.(i) <- (match file.file_chunks.(i) with
          PresentVerified -> PresentTemp
        | AbsentVerified -> AbsentTemp
        | PartialVerified x -> PartialTemp x
        | x -> x)
    done
*)    

    
let parse_donkey_url url =
  match String2.split (String.escaped url) '|' with
(* TODO RESULT *)
  | "ed2k://" :: "file" :: name :: size :: md4 :: _
  | "file" :: name :: size :: md4 :: _ ->
      let md4 = if String.length md4 > 32 then
          String.sub md4 0 32 else md4 in
      let file = query_download [name] (Int64.of_string size)
        (Md4.of_string md4) None None None false;
      in
      CommonInteractive.start_download file;
      true
  | "ed2k://" :: "server" :: ip :: port :: _
  | "server" :: ip :: port :: _ ->
      let ip = Ip.of_string ip in
      let s = force_add_server ip (int_of_string port) in
      server_connect (as_server s.server_server);
      true 
  | "ed2k://" :: "friend" :: ip :: port :: _
  | "friend" :: ip :: port :: _ ->
      let ip = Ip.of_string ip in
      let port = int_of_string port in
      let c = new_client (Direct_address (ip,port)) in
      friend_add c;
      true
  
  | _ ->  false

      
let register_commands list =
  register_commands
    (List2.tail_map (fun (n,f,h) -> (n, "Network/Edonkey", f,h)) list)

let commands = [
    "n", Arg_multiple (fun args o ->
        let buf = o.conn_buf in
        let ip, port =
          match args with
            [ip ; port] -> ip, port
          | [ip] -> ip, "4661"
          | _ -> failwith "n <ip> [<port>]: bad argument number"
        in
        let ip = Ip.from_name ip in
        let port = int_of_string port in
        
        let s = force_add_server ip port in
        Printf.bprintf buf "New server %s:%d\n" 
          (Ip.to_string ip) port;
        ""
    ), "<ip> [<port>] :\t\t\tadd a server";
    
    "afr", Arg_multiple (fun args o ->
        let buf = o.conn_buf in
        let ip, port =
          match args with
            [ip ; port] -> ip, port
          | [ip] -> ip, "4662"
          | _ -> failwith "afr <ip> [<port>]: bad argument number"
        in
        let ip = Ip.from_name ip in
        let port = int_of_string port in
        let c = new_client (Direct_address (ip,port)) in
        friend_add c;
        "friend added";
    ),  "<ip> [<port>] :\t\t\tadd a friend";
    
    "vu", Arg_none (fun o ->
        let buf = o.conn_buf in
        Printf.sprintf 
          "Upload credits : %d minutes\nUpload disabled for %d minutes" 
          !CommonUploads.upload_credit !CommonUploads.has_upload;
    
    ), ":\t\t\t\t\tview upload credits";
    
    
    "comments", Arg_one (fun filename o ->
        let buf = o.conn_buf in
(* TODO        DonkeyIndexer.load_comments filename; 
        DonkeyIndexer.save_comments (); *)
        "comments loaded and saved"
    ), "<filename> :\t\t\tload comments from file";
    
    "comment", Arg_two (fun md4 comment o ->
        let buf = o.conn_buf in
        let md4 = Md4.of_string md4 in
(* TODO        DonkeyIndexer.add_comment md4 comment; *)
        "Comment added"
    ), "<md4> \"<comment>\" :\t\tadd comment on a md4";
    
    "import", Arg_one (fun dirname o ->
        let buf = o.conn_buf in
        try
          import_config dirname;
          "config loaded"
        with e ->
            Printf.sprintf "error %s while loading config" (
              Printexc2.to_string e)
    ), "<dirname> :\t\t\timport the config from dirname";
    
    "import_temp", Arg_one (fun dirname o ->
        let buf = o.conn_buf in
        try
          import_temp dirname;
          "temp files loaded"
        with e ->
            Printf.sprintf "error %s while loading temp files" (
              Printexc2.to_string e)
    ), "<temp_dir> :\t\t\timport the old edonkey temp directory";
    
    "load_old_history", Arg_none (fun o ->
        let buf = o.conn_buf in
(* TODO        DonkeyIndexer.load_old_history (); *)
        "Old history loaded"
    ), ":\t\t\tload history.dat file";
    
    "servers", Arg_one (fun filename o ->
        let buf = o.conn_buf in
        try
          let n = load_server_met filename in
          Printf.sprintf "%d servers loaded" n
        with e -> 
            Printf.sprintf "error %s while loading file" (Printexc2.to_string e)
    ), "<filename> :\t\t\tadd the servers from a server.met file";
    
    
    "id", Arg_none (fun o ->
        let buf = o.conn_buf in
        List.iter (fun s ->
            Printf.bprintf buf "For %s:%d  --->   %s\n"
              (Ip.to_string s.server_ip) s.server_port
              (match s.server_cid with
                None -> "waiting"
              | Some ip ->
                  if Ip.valid ip then
                    Ip.to_string ip
                  else
                    Int64.to_string (Ip.to_int64 (Ip.rev ip)))
        ) (connected_servers());
        ""
    ), ":\t\t\t\t\tprint ID on connected servers";
    
    "set_brothers", Arg_multiple (fun args o ->
        let buf = o.conn_buf in
        
        let files = ref [] in
        List.iter (fun arg ->
            let num = int_of_string arg in
            List.iter (fun file ->
                if file_num file = num then begin
                    Printf.bprintf buf "%s\n" (file_best_name file);
                    files := file :: !files
                  end
            ) !current_files
        ) args;
        
        DonkeySources.set_brothers 
          (List.map (fun file -> file.file_sources) !files);
        brotherhood =:= 
          (List.map (fun file -> file.file_md4) !files) :: !!brotherhood;
        "    are now defined as colocated"
    ) , "<f1> < f2> ... :\t\tdefine these files as probably colocated";
    
    "recover_bytes", Arg_multiple (fun args o ->
        let buf = o.conn_buf in
        
        List.iter (fun arg ->
            let num = int_of_string arg in
            List.iter (fun file ->
                if file_num file = num then begin
                    match file.file_swarmer with
                      None -> ()
                    | Some swarmer ->
                        let segments = CommonFile.recover_bytes (as_file file) in
                        let old_downloaded = Int64Swarmer.downloaded swarmer in
                        Int64Swarmer.set_present swarmer segments;
                        let new_downloaded = Int64Swarmer.downloaded swarmer in
                        Printf.bprintf buf "Recovered %Ld bytes for %s\n"
                          (new_downloaded -- old_downloaded) 
                        (file_best_name file)
                        
                  end
            ) !current_files
        ) args;
        ""
    ) , "<f1> < f2> ... :\t\ttry to recover these files at byte level";

    "preferred", Arg_two (fun arg1 arg2 o ->
        let preferred = bool_of_string arg1 in
        let ip = Ip.of_string arg2 in
        Hashtbl.iter (fun ip_s s ->
            if ip_s = ip then begin
              s.server_preferred <- preferred;
              server_must_update s;
            end
        ) servers_by_key;
        "ok"
    ), "<true/false> <ip> :\t\tset the server with this IP as preferred";
    
    "bs", Arg_multiple (fun args o ->
        List.iter (fun arg ->
            let ip = Ip.of_string arg in
            server_black_list =:=  ip :: !!server_black_list;
        ) args;
        "done"
    ), "<ip1> <ip2> ... :\t\t\tadd these IPs to the servers black list";
    
    "port", Arg_one (fun arg o ->
        donkey_port =:= int_of_string arg;
        "new port will change at next restart"),
    "<port> :\t\t\t\tchange connection port";
    
    "add_url", Arg_two (fun kind url o ->
        let buf = o.conn_buf in
        let v = (kind, 1, url) in
        if not (List.mem v !!web_infos) then
          web_infos =:=  v :: !!web_infos;
        CommonWeb.load_url kind url;
        "url added to web_infos. downloading now"
    ), "<kind> <url> :\t\t\tload this file from the web. Kind is either server.met (if the downloaded file is a server.met)";
    
    "scan_temp", Arg_none (fun o ->
        let buf = o.conn_buf in
        let list = Unix2.list_directory !!temp_directory in
        
        let counter = ref 0 in
        let tr = ref "dl-1" in
        
        if use_html_mods o then begin

Printf.bprintf buf 
"\\<script language=javascript\\>
\\<!-- 
function submitRenameForm(i) {
var formID = document.getElementById(\\\"renameForm\\\" + i)
var regExp = new RegExp (' ', 'gi') ;
var renameTextOut = formID.newName.value.replace(regExp, '+'); 
parent.fstatus.location.href='submit?q=rename+'+i+'+\\\"'+renameTextOut+'\\\"';
}
//--\\>
\\</script\\>";
          
          html_mods_table_header buf "scan_tempTable" "scan_temp" [ 
            ( "0", "srh", "Filename", "Filename (press ENTER to rename)" ) ; 
            ( "0", "srh", "Status", "Status" ) ;
            ( "0", "srh", "MD4 (link=ed2k)", "MD4 (link=ed2k)" ); ];
        

		end;
        
        List.iter (fun filename ->
            incr counter;
            if (!counter mod 2 == 0) then tr := "dl-1"
            else tr := "dl-2";
            let uid =
              try Uid.of_string filename
              with _ -> Uid.no
            in
            let (other,md4) =
              match Uid.to_uid uid with
              | Ed2k md4 -> (false,md4)
              | NoUid ->
                  (try
                     if String.length filename = 32 then
                       (false,(Md4.of_string filename))
                     else (true,Md4.null)
                   with _ ->
                     (true,Md4.null)
                  )
              | _ -> (true,Md4.null)
            in
            try
              if other then raise Not_found;
              try
                let file = find_file md4 in
                if use_html_mods o then
				  let fnum = (file_num file) in 
                  Printf.bprintf buf "
				\\<tr class=\\\"%s\\\"\\>\\<td class=\\\"sr\\\"\\>
				\\<form name=\\\"renameForm%d\\\" id=\\\"renameForm%d\\\" action=\\\"javascript:submitRenameForm(%d);\\\"\\>
				\\<input style=\\\"font: 8pt sans-serif\\\" name=\\\"newName\\\" type=text size=50 value=\\\"%s\\\"\\>\\</input\\>\\</td\\>\\</form\\>
                \\<td class=\\\"sr \\\"\\>%s\\</td\\> 
                \\<td class=\\\"sr \\\"\\>\\<A HREF=\\\"%s\\\"\\>%s\\</A\\>\\</td\\>\\</tr\\>"
                    !tr fnum fnum fnum (file_best_name file) "Downloading" (file_comment (as_file file)) filename  
                else
                  Printf.bprintf buf "%s is %s %s\n" filename
                    (file_best_name file)
                  "(downloading)" 
              with _ ->
                  if use_html_mods o then
                    Printf.bprintf buf "\\<tr class=\\\"%s\\\"\\>\\<td class=\\\"sr\\\"\\>%s\\</td\\>
                \\<td class=\\\"sr \\\"\\>%s\\</td\\> 
                \\<td class=\\\"sr \\\"\\>%s\\</td\\>\\</tr\\>" !tr 
(* TODO RESULT                      (try
                        let names = DonkeyIndexer.find_names md4 in
                        List.hd names
                      with _ -> "Never Seen") *) "?"
                    (if List.mem md4 !!old_files then
                        "Old file" else "Unknown")
                    filename
                  else
                    Printf.bprintf buf "%s %s %s\n"
                      filename
                      (if List.mem md4 !!old_files then
                        "is an old file" else "is unknown")
(* TODO RESULT                    (try
                        let names = DonkeyIndexer.find_names md4 in
                        List.hd names
                      with _ -> "and never seen") *) "?"
            
            with _ -> 
                if use_html_mods o then
                  Printf.bprintf buf "\\<tr class=\\\"%s\\\"\\>\\<td class=\\\"sr\\\"\\>Unknown\\</td\\>
                \\<td class=\\\"sr \\\"\\>\\</td\\> 
                \\<td class=\\\"sr \\\"\\>%s\\</td\\>\\</tr\\>" !tr filename 
                else
                  Printf.bprintf buf "%s unknown\n" filename
        
        ) list;
        
        if use_html_mods o then Printf.bprintf buf "\\</table\\>\\</div\\>";
        
        "done";
    ), ":\t\t\t\tprint temp directory content";
    
    "sources", Arg_none (fun o ->
        let buf = o.conn_buf in
        DonkeySources.print buf o.conn_output;
        ""
    ), ":\t\t\t\tshow sources currently known";

    (*
    "update_sources", Arg_none (fun o ->
        let buf = o.conn_buf in
        DonkeySources.recompute_ready_sources ();
        "done"
    ), ":\t\t\trecompute order of connections to sources (experimental)";
*)
    
    "xs", Arg_none (fun o ->
        let buf = o.conn_buf in
        if !xs_last_search >= 0 then begin
            try
              DonkeyUdp.make_xs (CommonSearch.search_find !xs_last_search);
              "extended search started"
            with e -> Printf.sprintf "Error %s" (Printexc2.to_string e)
          end else "No previous search to extend"),
    ":\t\t\t\t\textend the last search";
    
    "clh", Arg_none (fun o ->
        let buf = o.conn_buf in
(* TODO RESULT        DonkeyIndexer.clear (); *)
        "local history cleared"
    ), ":\t\t\t\t\tclear local history";

(* TODO RESULT *)
    "dd", Arg_two(fun size md4 o ->
        let buf = o.conn_buf in
        let file = query_download [] (Int64.of_string size)
          (Md4.of_string md4) None None None false in
        CommonInteractive.start_download file;
        "download started"    
    ), "<size> <md4> :\t\t\tdownload from size and md4";
    
    "dup", Arg_none (fun _ ->
        DonkeyChunks.duplicate_chunks (); "done"),
    ":\t\t\t\t\tfind duplicate chunks (experimental)";
    
    "remove_old_servers", Arg_none (fun o ->
        let buf = o.conn_buf in
        DonkeyServers.remove_old_servers ();
        "clean done"
    ), ":\t\t\tremove servers that have not been connected for several days";
    
    
    
    "bp", Arg_multiple (fun args o ->
        List.iter (fun arg ->
            let port = int_of_string arg in
            port_black_list =:=  port :: !!port_black_list;
        ) args;
        "done"
    ), "<port1> <port2> ... :\t\t\tadd these Ports to the port black list";
    
    "send_servers", Arg_none (fun o ->
        CommonWeb.connect_redirector ();
        "done"
    ), ":\t\t\t\tsend the list of connected servers to the redirector";
  
  ]
          
let _ =
  register_commands commands;
  file_ops.op_file_resume <- (fun file ->
      reconnect_all file;
  );
  file_ops.op_file_set_priority <- (fun file _ ->
(*  TODO PRIORITY: take care priorities in CommonSources
DonkeySources.recompute_ready_sources ()        *)
      ()
  );
  file_ops.op_file_pause <- (fun file -> ()  );
  
  file_ops.op_file_commit <- (fun file new_name ->
      
(*      DonkeyStats.save_download_history file; *)
      
      if !!keep_cancelled_in_old_files
          && not (List.mem file.file_md4 !!old_files) then
        old_files =:= file.file_md4 :: !!old_files;
      DonkeyShare.remember_shared_info file new_name;
      
(**************************************************************

Since the new version of Unix32.rename does not update the
file name, we need to create a new 'file' and stop using the old
one. For that, we need first to remove definitively the old one
from the system, and thus to disconnect all uploaders for this
file.---------> to be done urgently

***************************************************************)
      
      unshare_file file;
  );
  network.op_network_connected <- (fun _ ->
    !nservers > 0  
  );
  network.op_network_private_message <- (fun iddest s ->      
      try
        let c = DonkeyGlobals.find_client_by_name iddest in
        match c.client_source.DonkeySources.source_sock with
          NoConnection -> 
            DonkeyClient.reconnect_client c;
            c.client_pending_messages <- c.client_pending_messages @ [s];
        | ConnectionWaiting _ ->
            c.client_pending_messages <- c.client_pending_messages @ [s];
        | Connection sock ->
            client_send c (DonkeyProtoClient.SayReq s)
      with
        Not_found ->
          CommonChat.send_text !!CommonOptions.chat_console_id None 
            (Printf.sprintf "client %s unknown" iddest)
  );
  network.op_network_download <- (fun r ->
      result_download r r.result_names false
  )

module P = GuiTypes

(* How often is this function called when the interface is running ?
is it called when no interface is connected ? it should be as fast
as possible. *)
  
let _ =
  file_ops.op_file_info <- (fun file ->
      try
        let last_seen = match file.file_swarmer with
            None -> [| last_time () |]
          | Some swarmer -> Int64Swarmer.compute_last_seen swarmer
        in
        let v = 
          {
            P.file_fields = Fields_file_info.all;
            
            P.file_comment = file_comment (as_file file);
            P.file_name = file_best_name file;
            P.file_num = (file_num file);
            P.file_network = network.network_num;
            P.file_names = file.file_filenames;
            P.file_md4 = file.file_md4;
            P.file_size = file_size file;
            P.file_downloaded = file_downloaded file;
            P.file_all_sources = file.file_sources.DonkeySources.manager_all_sources;
            P.file_active_sources = file.file_sources.DonkeySources.manager_active_sources;
            P.file_state = file_state file;
            P.file_sources = None;
            P.file_download_rate = file_download_rate file.file_file;
            P.file_chunks = (match file.file_swarmer with
                None -> "" | Some swarmer ->
                  Int64Swarmer.verified_bitmap swarmer);
            P.file_priority = file_priority  file;
            P.file_availability =  
            [network.network_num,(match file.file_swarmer with
                  None -> "" | Some swarmer ->
                    Int64Swarmer.availability swarmer)];
            P.file_format = file.file_format;
            P.file_chunks_age = last_seen;
            P.file_age = file_age file;
            P.file_last_seen = file.file_file.impl_file_last_seen;
            P.file_uids = [Uid.create (Ed2k file.file_md4)];
          } in
        v
      with e ->
          lprintf "Exception %s in op_file_info\n" (Printexc2.to_string e);
          raise e
          
  )
  
let _ =
  server_ops.op_server_info <- (fun s ->
      if !!enable_donkey then 
        {
          P.server_num = (server_num s);
          P.server_network = network.network_num;
          P.server_addr = Ip.addr_of_ip s.server_ip;
          P.server_port = s.server_port;
          P.server_realport = (match s.server_realport with Some x -> x | None -> 0);
          P.server_score = s.server_score;
          P.server_tags = []; (* s.server_tags; *)
          P.server_nusers = s.server_nusers;
          P.server_nfiles = s.server_nfiles;
          P.server_state = server_state s;
          P.server_name = s.server_name;
          P.server_description = s.server_description;
          P.server_banner = s.server_banner;
          P.server_users = None;
          P.server_preferred = s.server_preferred;
        }
      else raise Not_found
  )


let _ =
  user_ops.op_user_info <- (fun u ->
      {
        P.user_num = u.user_user.impl_user_num;
        P.user_md4 = u.user_md4;
        P.user_ip = u.user_ip;
        P.user_port = u.user_port;
        P.user_tags = []; (* u.user_tags; *)
        P.user_name = u.user_name;
        P.user_server = u.user_server.server_server.impl_server_num;
      }
  )

let string_of_client_addr c =
  try match c.client_source.DonkeySources.source_sock with
      Connection sock -> (Ip.to_string (peer_ip sock)) 
    | _ -> ""
  with _ -> ""
      
let _ =
  client_ops.op_client_info <- (fun c ->
      {
        P.client_network = network.network_num;
        P.client_kind = (match c.client_kind with
            Direct_address (ip, port) -> Known_location (ip,port)
          | _ -> Indirect_location (c.client_name,c.client_md4));
        P.client_state = client_state c;
        P.client_type = client_type c;
        P.client_tags = []; (* c.client_tags; *)
        P.client_name = c.client_name;
        P.client_files = None;
        P.client_num = (client_num c);
        P.client_rating = c.client_rating;
        P.client_chat_port = 0 ;
        P.client_connect_time = c.client_connect_time;
        P.client_software = gbrand_to_string c.client_brand;
        P.client_emulemod = gbrand_mod_to_string c.client_mod_brand;
        P.client_downloaded = c.client_downloaded;
        P.client_uploaded = c.client_uploaded;
(*        P.client_source.source_sock_addr =    (); *)
        P.client_upload = 
        (match c.client_upload with
            Some cu -> Some (file_best_name cu.up_file)
          | None -> None);        
      }
  );
  client_ops.op_client_debug <- (fun c debug ->
      c.client_debug <- debug)

let _ =
  network.op_network_connect_servers <- (fun _ ->
      force_check_server_connections true  )

let disconnect_server s r =
  match s.server_sock with
    NoConnection -> ()
  | ConnectionWaiting token ->
      cancel_token token;
      s.server_sock <- NoConnection
  | Connection sock ->
      TcpBufferedSocket.shutdown sock r

let _ =
  server_ops.op_server_remove <- (fun s ->
      DonkeyComplexOptions.remove_server s.server_ip s.server_port
  );
  server_ops.op_server_connect <- connect_server;
  server_ops.op_server_disconnect <- (fun s ->
      disconnect_server s Closed_by_user);

  server_ops.op_server_query_users <- (fun s ->
      match s.server_sock, server_state s with
        Connection sock, (Connected _ | Connected_downloading _) ->
          server_send sock (DonkeyProtoServer.QueryUsersReq "");
          Fifo.put s.server_users_queries false
      | _ -> ()
  );
  server_ops.op_server_find_user <- (fun s user ->
      match s.server_sock, server_state s with
        Connection sock, (Connected _ | Connected_downloading _) ->
          server_send sock (DonkeyProtoServer.QueryUsersReq user);
          Fifo.put s.server_users_queries true
      | _ -> ()      
  );
  server_ops.op_server_users <- (fun s ->
      List2.tail_map (fun u -> as_user u.user_user) s.server_users)    ;
  server_ops.op_server_cid <- (fun s -> 
      match s.server_cid with
        None -> Ip.null
      | Some ip -> ip);
  server_ops.op_server_set_preferred <- (fun s b ->
    s.server_preferred <- b;
    server_must_update s);
  server_ops.op_server_rename <- (fun s name ->
    s.server_name <- name;
    server_must_update s);

  ()

let _ =
  file_ops.op_file_save_as <- (fun file name ->
      file.file_filenames <- [name, noips()];
      set_file_best_name (as_file file) name
  );
  file_ops.op_file_set_format <- (fun file format ->
      file.file_format <- format);
  file_ops.op_file_check <- (fun file ->
      DonkeyChunks.verify_chunks file);  
  file_ops.op_file_recover <- (fun file ->
      if file_state file = FileDownloading then 
        reconnect_all file);  
  file_ops.op_file_all_sources <- (fun file ->
      let list = ref [] in
      DonkeySources.iter_all_sources (fun s -> 
          let s_uid = s.DonkeySources.source_uid in
          let c = new_client s_uid in
          list := (as_client c) :: !list
      ) file.file_sources;
      !list
  );
  file_ops.op_file_active_sources <- (fun file ->
      let list = ref [] in
      DonkeySources.iter_active_sources (fun s -> 
          let s_uid = s.DonkeySources.source_uid in
          let c = new_client s_uid in
          list := (as_client c) :: !list
      ) file.file_sources;
      !list
  );
  file_ops.op_file_print_html <- (fun file buf ->

     html_mods_cntr_init ();

     let tr () = 
      Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-%d\\\"\\>" (html_mods_cntr ())
     in 

      tr ();
      html_mods_td buf [
        ("Fake check links", "sr br", "Fakecheck");
        ("", "sr", Printf.sprintf "\\<a target=\\\"_blank\\\" href=\\\"http://donkeyfakes.gambri.net/fakecheck.php\\?hash=%s\\\"\\>[Donkey-Fakes]\\</a\\> \\<a target=\\\"_blank\\\" href=\\\"http://bitzi.com/lookup/urn:ed2k:%s\\\"\\>[Bitzi-Bitpedia]\\</a\\> \\<a target=\\\"_blank\\\" href=\\\"http://www.filedonkey.com/url/%s\\\"\\>[FileDonkey]\\</a\\>"
            (Md4.to_string file.file_md4) (Md4.to_string file.file_md4) (Md4.to_string file.file_md4)
        ) ];

      tr ();
      html_mods_td buf [
        ("File History Links", "sr br", "File History");
        ("","sr", Printf.sprintf "\\<a target=\\\"_blank\\\" href=\\\"http://stats.razorback2.com/ed2khistory\\?ed2k=%s\\\"\\>RazorBack File History\\</a\\>"
            (Md4.to_string file.file_md4)
          )
       ];
      tr ();
      html_mods_td buf [
        ("ed2k link", "sr br", "ed2k link");
        ("", "sr", Printf.sprintf "\\<a href=\\\"ed2k://|file|%s|%s|%s|/\\\"\\>ed2k://|file|%s|%s|%s|/\\</A\\>"
            (file_best_name file)
          (Int64.to_string (file_size file))
          (Md4.to_string file.file_md4)
          (file_best_name file)
          (Int64.to_string (file_size file))
          (Md4.to_string file.file_md4)) ];

      tr ();
      let optionlist = ref "" in
      List.iter (fun (name,_) ->
          optionlist := !optionlist ^ Printf.sprintf "\\<option value=\\\"%s\\\"\\>%s\n" name name;
      ) file.file_filenames;


      let input_size = (Printf.sprintf "%d" ((String.length (file_best_name file))+3)) in
      html_mods_td buf [
        ("Rename file by selecting an alternate name", "sr br", "Filename");
        ("Rename file", "sr",
          Printf.sprintf "\\<script language=javascript\\>
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
   ^ "\\<select name=\\\"newName\\\" id=\\\"newName\\\" onchange=\\\"javascript:renameForm2.newName.value=renameForm1.newName.options[renameForm1.newName.selectedIndex].value;this.form.submit();\\\"\\>"
   ^ Printf.sprintf "\\<option value=\\\"%s\\\" selected\\>%s\n" (file_best_name file) (file_best_name file)
   ^ !optionlist
   ^ "\\</select\\>\\</td\\>\\</tr\\>\\</form\\>\\<tr\\>\\<td\\>\\<form name=\\\"renameForm2\\\" id=\\\"renameForm2\\\" action=\\\"javascript:submitRenameForm(2);\\\"\\>"
   ^ "\\<input name=\\\"newName\\\" type=text size=" ^ input_size ^ " value=\\\"" ^ (file_best_name file) ^ "\\\"\\>\\</input\\>\\</td\\>\\</tr\\>\\</form\\>\\</table\\>"
    ) ];


  );
  file_ops.op_file_print_sources_html <- (fun file buf ->

   if List.length (file_ops.op_file_all_sources file) > 0 then begin

      let chunks = 
      (match file.file_swarmer with
       None -> "" | Some swarmer ->
       Int64Swarmer.verified_bitmap swarmer)
      in


      html_mods_table_header buf "sourcesTable" "sources al" ([ 
          ( "1", "srh ac", "Client number (click to add as friend)", "Num" ) ; 
          ( "0", "srh", "[A] = Active download from client", "A" ) ; 
          ( "0", "srh", "Client state", "CS" ) ; 
          ( "0", "srh", "Client name", "Name" ) ; 
          ( "0", "srh", "Client brand", "CB" ) ; 
        ] @
          (if !!emule_mods_count then [( "0", "srh", "eMule MOD", "EM" )] else [])
        @ [
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
          ( "0", "srh", "Chunks (absent|partial|present|verified)", 
            (colored_chunks 
                (Array.init (String.length chunks)
                (fun i -> ((int_of_char chunks.[i])-48)))) ) ; 
          ( "1", "srh ar", "Number of full chunks", (Printf.sprintf "%d"
                (let fc = ref 0 in (String.iter (fun s -> if s = '2' then incr fc) 
                  chunks );!fc ))) ]);


      let counter = ref 0 in
      DonkeySources.iter_all_sources (fun s -> 
          let s_uid = s.DonkeySources.source_uid in
          let c = new_client s_uid in
          let c1 = (as_client c) in
          incr counter;

        try 
          let i = (client_info (as_client c)) in


          Printf.bprintf buf "\\<tr onMouseOver=\\\"mOvr(this);\\\" 
        onMouseOut=\\\"mOut(this);\\\" class=\\\"%s\\\"\\>"
           (if (!counter mod 2 == 0) then "dl-1" else "dl-2");

          
          Printf.bprintf buf "\\<td title=\\\"Add as Friend\\\" class=\\\"srb ar\\\"
    onClick=\\\"parent.fstatus.location.href='submit?q=friend_add+%d'\\\"\\>%d\\</TD\\>"
            (client_num c) (client_num c);
          
          html_mods_td buf ([
            ("", "sr", (match c.client_download with 
                  None -> Printf.sprintf "" 
                | Some _ -> Printf.sprintf "%s" ( 
                      let qfiles = c.client_file_queue in
                      let (qfile, qchunks,_) =  List.hd qfiles in
                      if (qfile == file) then
                        "A" else "";)) );
            ((string_of_connection_state (client_state c)), "sr", 
              (short_string_of_connection_state (client_state c)) );
            (String.escaped c.client_name, "sr", client_short_name c.client_name);
            (brand_to_string c.client_brand, "sr", gbrand_to_string c.client_brand);
            ] @
            (if !!emule_mods_count then [(brand_mod_to_string c.client_mod_brand, "sr", gbrand_mod_to_string c.client_mod_brand)] else [])
            @ [
              ("", "sr", (if DonkeySources.source_brand c.client_source
                  then "T" else "F"));
            ("", "sr", (match c.client_kind with 
                  | Direct_address (ip,port) -> Printf.sprintf "D"
                  | _ -> Printf.sprintf "I"
                ));
            ("", "sr br", match c.client_kind with
                Direct_address (ip,port) -> Printf.sprintf "%s" (Ip.to_string ip)
              | _ -> (string_of_client_addr c));
            ("", "sr ar", (size_of_int64 c.client_uploaded));
            ("", "sr ar br", (size_of_int64 c.client_downloaded));
            ("", "sr ar", Printf.sprintf "%d" c.client_rank);
            ("", "sr ar br", Printf.sprintf "%d" c.client_source.DonkeySources.source_score);
            ("", "sr ar", (string_of_date (c.client_source.DonkeySources.source_age)));
            ("", "sr ar", ("-"));
            ("", "sr ar br", "-");
            ("", "sr ar", (if client_has_a_slot (as_client c) then "T" else "F"));
            ("", "sr ar br", (if c.client_banned then "T" else "F"));
            ("", "sr ar", Printf.sprintf "%d" c.client_requests_sent);
            ("", "sr ar", Printf.sprintf "%d" c.client_requests_received);
            ("", "sr ar br", Printf.sprintf "%d" (((last_time ()) - c.client_connect_time) / 60));
            ("", "sr br", (Md4.to_string c.client_md4)); ]);
          
          Printf.bprintf buf "\\<td class=\\\"sr \\\"\\>";
          
          ( let qfiles = c.client_file_queue in
            if qfiles <> [] then begin
                try
                  let _, qchunks,_ = List.find (fun (qfile, _,_) ->
                        qfile == file) qfiles in
                  let tc = ref 0 in
                  Printf.bprintf buf "%s\\</td\\>\\<td class=\\\"sr ar\\\"\\>%d\\</td\\>" 
                    (CommonFile.colored_chunks (Array.init (Array.length qchunks) 
                      (fun i -> (if qchunks.(i) then 2 else 0)) )) 
                  (Array.iter (fun b -> if b then incr tc) qchunks;!tc);
                with Not_found -> (
                      Printf.bprintf buf "\\</td\\>\\<td class=\\\"sr ar\\\"\\>\\</td\\>" 
                    );
              end
            else
              Printf.bprintf buf "\\</td\\>\\<td class=\\\"sr ar\\\"\\>\\</td\\>" 
          );

           Printf.bprintf buf "\\</tr\\>";
        with _ -> ()

      ) file.file_sources;

    Printf.bprintf buf "\\</table\\>\\</div\\>\\<br\\>";

   end;

  );
  file_ops.op_file_cancel <- (fun file ->
      Hashtbl.remove files_by_md4 file.file_md4;
      current_files := List2.removeq file !current_files;
      DonkeySources.remove_file_sources_manager file.file_sources;
      if !!keep_cancelled_in_old_files &&
        not (List.mem file.file_md4 !!old_files) then
        old_files =:= file.file_md4 :: !!old_files;
      DonkeyProtoOvernet.Overnet.cancel_recover_file file;
      DonkeyProtoKademlia.Kademlia.cancel_recover_file file;
  );
  file_ops.op_file_comment <- (fun file ->
      Printf.sprintf "ed2k://|file|%s|%Ld|%s|" 
        (file_best_name file)
      (file_size file)
      (Md4.to_string file.file_md4)
  );
  file_ops.op_file_files <- (fun file impl -> 
      match file.file_swarmer with
        None -> [CommonFile.as_file impl]
      | Some swarmer ->
          Int64Swarmer.subfiles swarmer)

let try_recover_temp_file filename md4 =
  try
    ignore (Hashtbl.find files_by_md4 md4)
  with Not_found ->
      let file_diskname = Filename.concat !!temp_directory filename in
      let size = Unix32.getsize file_diskname true in
      if size <> zero then
        let names =
            (* TODO RESULT
            try DonkeyIndexer.find_names md4
            with _ -> *)
            []
        in
        let file =
          query_download names size md4 None (Some file_diskname) None true
        in
        recover_md4s md4
        
let _ =
  network.op_network_extend_search <- (fun s e ->
      match e with
      | ExtendSearchLocally ->
(* TODO RESULT          DonkeyIndexer.find s      *) ()
      | ExtendSearchRemotely ->
          DonkeyUdp.make_xs s
  );
  
  network.op_network_clean_servers <- (fun _ ->
      DonkeyServers.remove_old_servers ());
  
  network.op_network_connect_servers <- (fun _ ->
      force_check_server_connections true);

(* TODO RESULT *)
  network.op_network_recover_temp <- (fun _ ->
      if !verbose then lprintf "op_network_recover_temp ++++++++++++++++++++++++++\n";
      let files = Unix2.list_directory !!temp_directory in
      List.iter (fun filename ->
          let uid =
            try Uid.of_string filename
            with _ -> Uid.no
          in
          match Uid.to_uid uid with
          | Ed2k md4 ->
              (try
                 try_recover_temp_file filename md4
               with e ->
                 lprintf "exception %s in recover_temp\n"
                 (Printexc2.to_string e);
              )
          | NoUid ->
              (if String.length filename = 32 then
                 try
                   let md4 = Md4.of_string filename in
                   try_recover_temp_file filename md4
                 with e ->
                   lprintf "exception %s in recover_temp\n"
                     (Printexc2.to_string e);
              )
          | _ -> ()
      ) files
  );
  
  network.op_network_parse_url <- parse_donkey_url;
  
  network.op_network_forget_search <- forget_search

(* emule<->mldonkey disconnects during chat, and this doesn't seem to auto reconnect
when sending a message? emule or ml problem? *)
let _ =
  client_ops.op_client_say <- (fun c s ->
      match c.client_source.DonkeySources.source_sock with
      | NoConnection -> 
          DonkeyClient.reconnect_client c;
          c.client_pending_messages <- c.client_pending_messages @ [s];
      | ConnectionWaiting _ -> 
          c.client_pending_messages <- c.client_pending_messages @ [s];
      | Connection sock ->
          client_send c (DonkeyProtoClient.SayReq s)
  );  
  client_ops.op_client_files <- (fun c ->
      match c.client_all_files with
        None ->  []
      | Some files -> 
          List2.tail_map (fun r -> "", r) files);
  client_ops.op_client_browse <- (fun c immediate ->
      lprintf "*************** should browse  ***********\n"; 
      match c.client_source.DonkeySources.source_sock with
      | Connection sock    ->
(*
      lprintf "****************************************";
      lprint_newline ();
      lprintf "       ASK VIEW FILES         ";
lprint_newline ();
  *)
          client_send c (
            let module M = DonkeyProtoClient in
            let module C = M.ViewFiles in
            M.ViewFilesReq C.t);          
      | NoConnection -> 
          lprintf "****************************************\n";
          lprintf "       TRYING TO CONTACT FRIEND         \n";
          
          reconnect_client c
      | _ -> ()
  );
  client_ops.op_client_connect <- (fun c ->
      match c.client_source.DonkeySources.source_sock with
        NoConnection ->  reconnect_client c
      | _ -> ()
  );
  client_ops.op_client_disconnect <- (fun c ->
      DonkeyClient.disconnect_client c Closed_by_user
  );
  client_ops.op_client_clear_files <- (fun c ->
      c.client_all_files <- None;
  );
  
  client_ops.op_client_bprint <- (fun c buf ->
      Printf.bprintf buf "\t\t%s (last_ok <%s>)\n"
        c.client_name
        (string_of_date (c.client_source.DonkeySources.source_age))
  );
  client_ops.op_client_dprint <- (fun c o file ->
      let info = file_info file in
      let buf = o.conn_buf in
      
      try
        
        (match c.client_download with
            None -> ()
          | Some _ ->  ( 
                let qfiles = c.client_file_queue in
                let (qfile, qchunks,_) =  List.hd qfiles in
                if (qfile == (as_file_impl file).impl_file_val) then begin
	          Printf.bprintf buf "[Donkey%6d] Name  : %-27s IP   : %-20s"
	            (client_num c)
	            (shorten c.client_name 20)
	            (match c.client_kind with
	                Direct_address (ip,port) -> (Ip.to_string ip)
	                |  _ -> (string_of_client_addr c));
                    Printf.bprintf buf "\n%14sDown  : %-10s                  Uploaded: %-10s  Ratio: %s%1.1f (%s)\n" ""
                    (Int64.to_string c.client_downloaded) 
                    (Int64.to_string c.client_uploaded)
                    (if c.client_downloaded > c.client_uploaded then "-" else "+")
                    (if c.client_uploaded > Int64.zero then (Int64.to_float (Int64.div c.client_downloaded c.client_uploaded)) else (1.))
                    (brand_to_string c.client_brand);
                    (Printf.bprintf buf "%14sFile  : %s\n" "" info.GuiTypes.file_name);
                  end;
              
              )
        
        )
      
      with _ -> ()
  
  );
  
  client_ops.op_client_dprint_html <- (fun c o file str ->
      let info = file_info file in
      let buf = o.conn_buf in
      try
        (match c.client_download with
            None -> false 
          | Some _ ->  ( 
                let qfiles = c.client_file_queue in
                let (qfile, qchunks,_) =  List.hd qfiles in
                if (qfile == (as_file_impl file).impl_file_val) then begin
                    let i = (client_info (as_client c)) in
                    
                    Printf.bprintf buf " \\<tr onMouseOver=\\\"mOvr(this);\\\" onMouseOut=\\\"mOut(this);\\\" 
			class=\\\"%s\\\"\\> \\<td title=\\\"Add as friend\\\" class=\\\"srb ar\\\"
			onClick=\\\"parent.fstatus.location.href='submit?q=friend_add+%d'\\\"\\>%d\\</TD\\>"
                      str (client_num c) (client_num c);
                    
                    html_mods_td buf ([
                      (string_of_connection_state (client_state c), "sr", 
                        short_string_of_connection_state (client_state c));
                      (Md4.to_string c.client_md4, "sr", client_short_name c.client_name);
                      ("", "sr", gbrand_to_string c.client_brand);
                      ] @
                      (if !!emule_mods_count then [("", "sr", gbrand_mod_to_string c.client_mod_brand)] else [])
                      @ [
                        ("", "sr", (if DonkeySources.source_brand c.client_source
                               then "T" else "F"));
                      ("", "sr ar", Printf.sprintf "%d" (((last_time ()) - c.client_connect_time) / 60));
                      ("", "sr", (match c.client_kind with  
                            | Direct_address (ip,port) -> Printf.sprintf "D"
                            | _ -> Printf.sprintf "I"
                          ));
                      ("", "sr", match c.client_kind with
                          Direct_address (ip,port) -> Printf.sprintf "%s" (Ip.to_string ip)
                        |  _ -> (string_of_client_addr c));
                      ("", "sr ar", (size_of_int64 c.client_uploaded));
                      ("", "sr ar", (size_of_int64 c.client_downloaded));
                      ("", "sr", info.GuiTypes.file_name) ]);
                    
                    Printf.bprintf buf "\\</tr\\>";
                    true
                  end
                else false;
              )
        )
      with _ -> false;
  )
  
let _ =
  user_ops.op_user_set_friend <- (fun u ->
      let s = u.user_server in
      DonkeyUdp.add_user_friend s u
  )
  
let _ =
  shared_ops.op_shared_unshare <- (fun file ->
      if !verbose_share then lprintf "************ UNSHARE FILE ************\n";
      unshare_file file;
      
(* Should we or not ??? *)
      if file_state file = FileShared then
        try Hashtbl.remove files_by_md4 file.file_md4 with _ -> ();
  );
  shared_ops.op_shared_info <- (fun file ->
   let module T = GuiTypes in
     match file.file_shared with
        None -> assert false
      | Some impl ->
          { (impl_shared_info impl) with 
            T.shared_network = network.network_num;
            T.shared_filename = file_best_name file;
            T.shared_uids = [Uid.create (Ed2k file.file_md4)];
            }
  );
  pre_shared_ops.op_shared_info <- (fun s ->
      let module T = GuiTypes in
      let impl = s.shared_shared in
      { (impl_shared_info impl) with 
        T.shared_network = network.network_num }
  )

let _ =
  CommonWeb.add_web_kind "server.met" (fun _ filename ->
      lprintf "FILE LOADED\n"; 
      let n = load_server_met filename in
      lprintf "%d SERVERS ADDED" n; lprint_newline ();    
  );
  CommonWeb.add_web_kind "servers.met" (fun _ filename ->
      lprintf "FILE LOADED\n"; 
      let n = load_server_met filename in
      lprintf "%d SERVERS ADDED\n" n; 
  );
  CommonWeb.add_web_kind "comments.met" (fun _ filename ->
(* TODO      DonkeyIndexer.load_comments filename; *)
      lprintf "COMMENTS ADDED\n"; 
  );
  
  file_ops.op_file_proposed_filenames <- op_file_proposed_filenames;  
  network.op_network_add_server <- (fun ip port ->
      let s = DonkeyComplexOptions.force_add_server (Ip.ip_of_addr ip) port in
      as_server s.server_server
  )
