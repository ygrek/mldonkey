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

open CommonUploads
open CommonGlobals
open CommonUser
open CommonClient
open CommonOptions
open CommonServer
open CommonResult
open CommonTypes
open CommonFile
open CommonInteractive
open CommonHosts

open GnutellaHandler
open GnutellaTypes
open GnutellaOptions
open GnutellaGlobals

module VB = VerificationBitmap

(* Don't share files greater than 10 MB on Gnutella and limit to 200 files. 
 Why ? Because we don't store URNs currently, and we don't want mldonkey
 to compute hashes for hours at startup *)
let max_shared_file_size = 1000000000L
let max_shared_files = 200
let shared_files_counter = ref 0

(*
type query =
  QAnd of query * query
| QOr of query * query
| QAndNot of query * query
| QHasWord of string
| QHasField of string * string
| QHasMinVal of string * int64
| QHasMaxVal of string * int64
| QNone (** temporary, used when no value is available ;
     must be simplified before transforming into strings *)
*)

let recover_file file =
  match file.file_swarmer with
    None -> ()
  | Some swarmer ->
      GnutellaClients.check_finished swarmer file;
      if file_state file = FileDownloading then
        GnutellaServers.really_recover_file file        

let download_file r user group =
  let file = GnutellaServers.really_download_file r user group in
  recover_file file;
  as_file file


let find_search s =
  let module M = struct exception Found of local_search end in
  try
    Hashtbl.iter (fun _ ss ->
        match ss.search_search with
          UserSearch (sss, _,_) when s == sss -> raise (M.Found ss)
        | _ -> ()
    ) searches_by_uid;
    raise Not_found
  with M.Found s -> s

let result_download r =
  if !verbose then
    lprintf_nl "result_download";
  let rec iter uids =
    match uids with
      [] -> raise IgnoreNetwork
    | uid :: tail ->
        if !verbose then lprintf_nl "UID: %s Accept(BP: %B ED2K: %B MD5: %B)" 
            (Uid.to_string uid) 
            GnutellaNetwork.accept_bitprint
            GnutellaNetwork.accept_ed2kuid
            GnutellaNetwork.accept_md5ext;

        match Uid.to_uid uid with
          Sha1 _ | Bitprint _ when GnutellaNetwork.accept_bitprint ->
            download_file r
        | Ed2k _ when GnutellaNetwork.accept_ed2kuid  ->
            download_file r
        | Md5Ext _ when GnutellaNetwork.accept_md5ext  ->
            download_file r
        | _  -> iter tail
  in
  if !verbose then
    lprintf_nl "%d uids" (List.length r.result_uids);
  iter r.result_uids

let declare_file _ = should_update_shared_files := true
  
let ask_for_uids sh =
  if !verbose then lprintf_nl "ask_for_uids Accept(BP: %B ED2K: %B MD5: %B)" 
     GnutellaNetwork.accept_bitprint
     GnutellaNetwork.accept_ed2kuid
     GnutellaNetwork.accept_md5ext;
  let info = IndexedSharedFiles.get_result sh.shared_info in
  if GnutellaNetwork.accept_ed2kuid then begin
      if !verbose then
        lprintf_nl "Ask for ED2K uid";
      CommonUploads.ask_for_uid sh ED2K (fun sh uid ->
          let uid = Uid.to_string uid in
          declare_word uid;
          declare_file ();
          if !verbose then
            lprintf_nl "Ed2k uid available (size %d)" 
            (Array.length info.CommonUploads.shared_md4s);
      );
    end;
  if GnutellaNetwork.accept_md5ext then begin
      if !verbose then
        lprintf_nl "Ask for MD5EXT uid";
      CommonUploads.ask_for_uid sh MD5EXT (fun sh uid ->
          let uid = Uid.to_string uid in
          declare_word uid;
          declare_file ();          
          if !verbose then
            lprintf_nl "Md5ext uid available: %s"  uid
      );
    end;

  if GnutellaNetwork.accept_bitprint then begin
      if !verbose then
        lprintf_nl "Ask for BITPRINT uid";
      CommonUploads.ask_for_uid sh BITPRINT (fun sh uid ->
          declare_word uid;
          declare_file ();
          if !verbose then
            lprintf_nl "Bitprint tree available (size %d)" 
              (Array.length info.CommonUploads.shared_tiger);
          ()
      );
      CommonUploads.ask_for_uid sh TIGER (fun sh uid ->
          let uid = Uid.to_string uid in
          declare_word uid;
          declare_file ();
          if !verbose then
            lprintf_nl "Tiger tree available (size %d)" 
              (Array.length info.CommonUploads.shared_tiger);
      );
      CommonUploads.ask_for_uid sh SHA1 (fun sh uid -> 
          let uid = Uid.to_string uid in
          if !verbose_share then
            lprintf_nl "Could share urn: %s" uid;
(* TODO : enter this shared file in the QRT *)
          declare_word uid;
          declare_file ();          
      );
    end
    
let _ =
  network.op_network_search <- (fun search buf ->
      match search.search_type with
        LocalSearch -> ()
      | _ ->
          
          let query = search.search_query in
          let words, xml_query = GnutellaProto.translate_query query in
          let words = String2.unsplit words ' ' in

(* Maybe we could generate the id for the query from the query itself, so
that we can reuse queries *)
          
          let uid = GnutellaProto.new_search_uid () in 
          let s = {
              search_search = UserSearch (search, words, xml_query);
              search_uid = uid;
              search_hosts = Intset.empty;
            } in
          
          GnutellaServers.send_query s;
          
          Hashtbl.add searches_by_uid uid s;
          ());
  network.op_network_close_search <- (fun s ->
      Hashtbl.remove searches_by_uid (find_search s).search_uid  
  );
  network.op_network_forget_search <- (fun s ->
      Hashtbl.remove searches_by_uid (find_search s).search_uid  
  );
  network.op_network_reset <- (fun _ -> ());
  network.op_network_connected <- (fun _ ->
      !connected_servers <> []
  );
  network.op_network_ports <- (fun _ ->
    [
    !!client_port, "client_port TCP+UDP";
    ]);
  network.op_network_porttest_result <- (fun _ -> PorttestNotAvailable);
  network.op_network_check_upload_slots <- (fun _ -> ());
  network.op_network_share <- (fun fullname codedname size ->
      (*
      lprintf "*************** op_network_share %s\n
      %d %d %Ld %Ld" fullname
        !shared_files_counter max_shared_files 
        size max_shared_file_size      
      ; *)
      if !shared_files_counter < max_shared_files &&
        size < max_shared_file_size then begin
(*          lprintf "*************** op_network_share %s\n" fullname; *)

          incr shared_files_counter;
          declare_word codedname;
          let sh = CommonUploads.add_shared fullname codedname size in
(*          lprintf "*************** op_network_share %s\n" fullname; *)
          
          List.iter declare_word (String2.stem codedname);
          
          ask_for_uids sh
        end
  );
(* TODO RESULT *)
  network.op_network_download <- (fun r user ->
      result_download r user
  )
  
let file_num file =
  file.file_file.impl_file_num

let _ =
  file_ops.op_file_all_sources <- (fun file ->
(*      lprintf "file_sources\n";  *)
      List2.tail_map (fun c ->
          as_client c
      ) file.file_clients
  );
  file_ops.op_file_files <- (fun file impl -> 
      match file.file_swarmer with
        None -> [CommonFile.as_file impl]
      | Some swarmer ->
          CommonSwarming.subfiles swarmer)
  ;
  file_ops.op_file_active_sources <- file_ops.op_file_all_sources;
  file_ops.op_file_check <- (fun file ->
      match file.file_swarmer with
        None -> failwith "verify_chunks: no swarmer to verify chunks"
      | Some swarmer ->
          match file.file_ttr with
            None -> failwith "No TTR for verification"
          | Some ttt ->
              CommonSwarming.verify_all_chunks_immediately swarmer
  );  
  
  file_ops.op_file_recover <- (fun file ->
      recover_file file;
      List.iter (fun c ->
          GnutellaServers.get_file_from_source c file
      ) file.file_clients
  );
  file_ops.op_file_resume <- (fun file ->
      recover_file file;
      List.iter (fun c ->
          GnutellaServers.get_file_from_source c file
      ) file.file_clients
  );
  file_ops.op_file_download_order <- (fun file strategy ->
      match file.file_swarmer with
      | None -> None
      | Some s ->
          (match strategy with
          (* return current strategy *)
          | None -> Some (CommonSwarming.get_strategy s)
          | Some strategy -> CommonSwarming.set_strategy s strategy;
                      Some (CommonSwarming.get_strategy s))
  )

module P = GuiTypes
  
let _ =
  file_ops.op_file_print <- (fun file buf -> ());
  file_ops.op_file_cancel <- (fun file ->
      CommonSwarming.remove_swarmer file.file_swarmer;
      file.file_swarmer <- None;
      remove_file file;
      GnutellaProto.cancel_recover_files file
  );
  file_ops.op_file_commit <- (fun file new_name ->
      CommonSwarming.remove_swarmer file.file_swarmer;
      file.file_swarmer <- None;
  );
  file_ops.op_file_info <- (fun file ->
      { (impl_file_info file.file_file) with
        P.file_network = network.network_num;
        
         P.file_chunks = (match file.file_swarmer with
         | None -> None 
         | Some swarmer -> 
             Some (CommonSwarming.chunks_verified_bitmap swarmer));
        P.file_chunk_size = (match file.file_swarmer with
         | None -> None
         | Some t ->
             Some (List.map (fun t -> t.CommonSwarming.t_chunk_size) t.CommonSwarming.t_s.CommonSwarming.s_networks));
        P.file_availability =   [network.network_num,
           (match file.file_swarmer with
           None -> "" | Some swarmer ->
                 CommonSwarming.chunks_availability swarmer)];
        
        P.file_chunks_age = [|0|];
        P.file_last_seen = BasicSocket.last_time ();
        P.file_uids = file.file_uids;
      }    
  )
  
let _ =
  server_ops.op_server_info <- (fun s ->
      check_server_country_code s;
      if !!enable_gnutella then
        { (impl_server_info s.server_server) with

          P.server_network = network.network_num;
          P.server_addr = s.server_host.host_addr;
          P.server_port = s.server_host.host_port;
          P.server_country_code = s.server_country_code;
          P.server_nusers = s.server_nusers;
          P.server_max_users = s.server_maxnusers;
          P.server_nfiles = s.server_nfiles;
          P.server_name = s.server_agent;
          P.server_version = s.server_vendor;
          P.server_description = s.server_description;

        } 
        else raise Not_found
  );
  server_ops.op_server_connect <- (fun s ->
      GnutellaServers.connect_server s.server_host);
  server_ops.op_server_disconnect <-
(fun s -> GnutellaServers.disconnect_server s BasicSocket.Closed_by_user);
  server_ops.op_server_to_option <- (fun _ -> raise Not_found)

module C = CommonTypes

(* TODO 
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
        C.result_tags = r.result_tags @ (List.map (fun uid ->
            string_tag ("urn") (Uid.to_string uid)
        ) r.result_uids);
        C.result_comment = "";
        C.result_done = false;
      }   
  )
*)
  
let _ =
  network.op_network_connected_servers <- (fun _ ->
      List2.tail_map (fun s -> as_server s.server_server)
      !connected_servers
  );
  network.op_network_parse_url <- (fun url user group ->
      match String2.split (String.escaped url) '|' with
      | "gnut://" :: "server" :: ip :: port :: _ ->  
          let ip = Ip.addr_of_string ip in
          let port = int_of_string port in
          let _ = new_server ip port in
          "added Gnutella server", true
      | "gnut://" :: "friend" :: uid :: ip :: port :: _ ->  
          let ip = Ip.of_string ip in
          let port = int_of_string port in
          let md4 = Md4.of_string uid in
          let c = new_client (Known_location (ip, port)) in
          c.client_user.user_uid <- md4;
          friend_add (as_client c);
          "added Gnutella friend", true
      | "gnut://" :: "friend" :: uid :: _ ->
          let md4 = Md4.of_string uid in
          let c = new_client (Indirect_location ("", md4, Ip.null, 0)) in
          friend_add (as_client c);
          "added Gnutella friend", true
      
      | _ -> 
          let (name, size, uids) =
            try
              GnutellaProto.parse_url url
            with
              Not_found -> "",zero,[]
          in
          if uids <> [] then begin
              (* Start a download for this file *)
              let rs = new_result name size [] uids [] in
              let r = IndexedResults.get_result rs in
              let file = download_file r user group in
              CommonInteractive.start_download file;
              "started Gnutella download", true
            end
          else "", false
  )
  
let _ =
  client_ops.op_client_info <- (fun c ->
      check_client_country_code c;
      { (impl_client_info c.client_client) with

        P.client_network = network.network_num;
        P.client_kind = c.client_user.user_kind;
        P.client_type = client_type c;
        P.client_name = if c.client_user.user_speed > 0 
                          then Printf.sprintf "%s (%d)" c.client_user.user_nick c.client_user.user_speed
                          else c.client_user.user_nick;
        P.client_country_code = c.client_country_code;
        P.client_software = c.client_user.user_software;

      }
  );
  client_ops.op_client_browse <- (fun _ _ -> lprintf_nl "Gnutella: browse client not implemented";
  );

    client_ops.op_client_bprint <- (fun c buf ->
        let cc = as_client c in
        let cinfo = client_info cc in
        Printf.bprintf buf "%s (%s)\n"
          cinfo.GuiTypes.client_name
          (string_of_kind cinfo.GuiTypes.client_kind)
    );    
   client_ops.op_client_dprint <- (fun c o file ->
        let info = file_info file in
        let buf = o.conn_buf in
        let cc = as_client c in
        let cinfo = client_info cc in
        client_print cc o; 
        Printf.bprintf buf "client: %s downloaded: %s uploaded: %s"
          "gN" (* cinfo.GuiTypes.client_software *)
          (Int64.to_string cinfo.GuiTypes.client_total_downloaded)
        (Int64.to_string cinfo.GuiTypes.client_total_uploaded);
        Printf.bprintf buf "\nfilename: %s\n\n" info.GuiTypes.file_name;
    );    
    client_ops.op_client_dprint_html <- (fun c o file str ->
        let info = file_info file in
        let buf = o.conn_buf in
        let cc = as_client c in
        let cinfo = client_info cc in
        let ccode,cname =
          match c.client_host with
          | None -> Geoip.unknown_country
          | Some _ -> Geoip.get_country_code_name cinfo.GuiTypes.client_country_code
        in
        Printf.bprintf buf " \\<tr onMouseOver=\\\"mOvr(this);\\\"
    onMouseOut=\\\"mOut(this);\\\" class=\\\"%s\\\"\\>" str;
        
        let show_emulemods_column = ref false in
           if Autoconf.donkey = "yes" then begin
               if !!emule_mods_count then
                   show_emulemods_column := true
        end;

        html_mods_td buf ([
          ("", "srb ar", Printf.sprintf "%d" (client_num cc));
          ((string_of_connection_state (client_state cc)), "sr",
            (short_string_of_connection_state (client_state cc)));
          ("", "sr", cinfo.GuiTypes.client_name);
          ("", "sr", "gN"); (* cinfo.GuiTypes.client_software *)
          ("", "sr", ""); (* cinfo.GuiTypes.client_release *)
          ] @
          (if !show_emulemods_column then [("", "sr", "")] else [])
          @ [
          ("", "sr", "F");
          ("", "sr ar", Printf.sprintf "%d" 
              (((last_time ()) - cinfo.GuiTypes.client_connect_time) / 60));
          ("", "sr", "D");
          ("", "sr", "N");
          ("", "sr", (string_of_kind cinfo.GuiTypes.client_kind));
          ] @ (if Geoip.active () then [( cname, "sr br", CommonPictures.flag_html ccode)] else []) @ [
          ("", "sr ar", (size_of_int64 cinfo.GuiTypes.client_total_uploaded));
          ("", "sr ar", (size_of_int64 cinfo.GuiTypes.client_total_downloaded));
          ("", "sr ar", (size_of_int64 cinfo.GuiTypes.client_session_uploaded));
          ("", "sr ar", (size_of_int64 cinfo.GuiTypes.client_session_downloaded));
          ("", "sr", info.GuiTypes.file_name); ]);
        true
    )     
  
  
let _ =  
  
  user_ops.op_user_info <- (fun user ->
      {
        P.user_num = user.user_user.impl_user_num;
        P.user_md4 = user.user_uid;
        P.user_name = "";
        P.user_ip = Ip.null;
        P.user_port = 0;
        P.user_tags = [];
        
        P.user_server = 0;
      });
  
  network.op_network_add_server <- (fun ip port ->
      as_server (new_server ip port).server_server
  )

open Queues
  
let commands = [
    "gstats", "Network/Gnutella", Arg_none (fun o ->
        let buf = o.conn_buf in
        if o.conn_output = HTML then
          begin
            Printf.bprintf buf "\\<div class=results\\>";
            html_mods_table_header buf "gstatsTable" "sources" [];
            Printf.bprintf buf "\\<tr\\>";
            html_mods_td buf [
              ("", "srh", "gstats statistics");
              ("", "srh", ""); ];
            Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-1\\\"\\>";
            html_mods_td buf [
              ("", "sr", Printf.sprintf "ultrapeers_waiting_queue: %d\n"
                (Queue.length ultrapeers_waiting_queue));
              ("", "sr", Printf.sprintf "peers_waiting_queue: %d\n"
                (Queue.length peers_waiting_queue)); ];
            Printf.bprintf buf "\\</tr\\>\\<tr class=\\\"dl-1\\\"\\>";
            html_mods_td buf [
              ("", "sr", Printf.sprintf "active_udp_queue: %d\n"
                (Queue.length active_udp_queue));
              ("", "sr", Printf.sprintf "waiting_udp_queue: %d\n"
                (Queue.length waiting_udp_queue)); ];
            Printf.bprintf buf "\\</tr\\>\\</table\\>\\</div\\>\\</div\\>\n"
          end
        else
          begin
            Printf.bprintf buf "ultrapeers_waiting_queue: %d\n"
              (Queue.length ultrapeers_waiting_queue);
            Printf.bprintf buf "peers_waiting_queue: %d\n"
              (Queue.length peers_waiting_queue);
            Printf.bprintf buf "active_udp_queue: %d\n"
              (Queue.length active_udp_queue);
            Printf.bprintf buf "waiting_udp_queue: %d\n"
              (Queue.length waiting_udp_queue);
          end;
        ""
    ), ":\t\t\t\tprint stats on Gnutella network";

    ]
  
let _ = 
  CommonNetwork.register_commands commands
  

let recover_files () = (* called every 10 minutes *)
  List.iter (fun file ->
      try recover_file file  with _ -> ()
  ) !current_files;
