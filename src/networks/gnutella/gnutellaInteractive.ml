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

open Xml
open Printf2
open Md4
open CommonSearch
open CommonGlobals
open CommonUser
open CommonClient
open CommonOptions
open CommonServer
open CommonResult
open CommonTypes
open CommonComplexOptions
open CommonFile
open CommonInteractive
open Options
open GnutellaTypes
open GnutellaOptions
open GnutellaGlobals
open GnutellaComplexOptions
open BasicSocket

open GnutellaProtocol

(* Don't share files greater than 10 MB on Gnutella and limit to 200 files. 
 Why ? Because we don't store URNs currently, and we don't want mldonkey
 to compute hashes for hours at startup *)
let max_shared_file_size = Int64.of_int 10000000
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

let xml_to_string xml = 
  "<?xml version=\"1.0\"?>" ^ (  Xml.to_string xml)
      
let audio_schema tags = 
  XML ("audios",
    [("xsi:nonamespaceschemalocation",
        "http://www.limewire.com/schemas/audio.xsd")],
    [XML ("audio", tags, [])])

(*
[
("artist", "Tom Jones");
("album", "Mars Attacks Soundtrack");
("title", "It&apos;s Not Unusal");

("sampleRate", "44100"); 
("seconds", "239"); 
("index", "0");
("bitrate", "128")
("track", "1"); 
("description", "Tom Jones, hehe"); 
("genre", "Retro");
("year", "1997")
] 

*)
  
let xml_query_of_query q =

  let keywords = ref [] in
  let add_words w =
    keywords := (String2.split_simplify w ' ') @ !keywords
  in
  let audio = ref false in
  let tags = ref [] in  
  let rec iter q = 
    match q with
    | QOr (q1,q2) 
    | QAnd (q1, q2) -> iter q1; iter q2
    | QAndNot (q1,q2) -> iter q1 
    | QHasWord w ->  add_words w
    | QHasField(field, w) ->
        begin
          match field with
            Field_Type -> 
              begin
                match String.lowercase w with
                  "audio" -> audio := true
                | _ -> add_words w
              end
          | Field_Format ->
              begin
                match String.lowercase w with
                | "mp3" | "wav" -> 
                    add_words w;
                    audio := true
                | _ -> add_words w
              end
          | Field_Album -> tags := ("album", w) :: !tags; add_words w
          | Field_Artist -> tags := ("artist", w) :: !tags; add_words w
          | Field_Title -> tags := ("title", w) :: !tags; add_words w
          | _ -> add_words w
        end
    | QHasMinVal (field, value) -> ()
    | QHasMaxVal (field, value) -> ()
    | QNone ->  ()
  in
  iter q;
  !keywords, if !audio then xml_to_string (audio_schema !tags) else ""
      
let _ =
  network.op_network_search <- (fun search buf ->
      let query = search.search_query in
      let words, xml_query = xml_query_of_query query in
      let words = String2.unsplit words ' ' in

(* Maybe we could generate the id for the query from the query itself, so
that we can reuse queries *)
      let uid = Md4.random () in
      
      let s = {
          search_search = UserSearch (search, words, xml_query);
          search_uid = uid;
          search_hosts = Intset.empty;
        } in
      
      Gnutella.send_query uid words;
      
      Hashtbl.add searches_by_uid uid s;
      ());
  network.op_network_close_search <- (fun s ->
      Hashtbl.remove searches_by_uid (find_search s).search_uid  
  );
  network.op_network_forget_search <- (fun s ->
      Hashtbl.remove searches_by_uid (find_search s).search_uid  
  );
  network.op_network_connected <- (fun _ ->
      !connected_servers <> []
  );
  network.op_network_share <- (fun fullname codedname size ->
      if !shared_files_counter < max_shared_files &&
        size < max_shared_file_size then begin
        incr shared_files_counter;
      GnutellaProtocol.new_shared_words := true;
      let sh = CommonUploads.add_shared fullname codedname size in
      CommonUploads.ask_for_uid sh SHA1 (fun sh uid -> 
            lprintf "Could share urn\n";
            ())
      end
  )
  
let _ =
  result_ops.op_result_download <- (fun result _ force ->
      GnutellaServers.download_file result)

let file_num file =
  file.file_file.impl_file_num

let _ =
  file_ops.op_file_sources <- (fun file ->
      lprintf "file_sources\n"; 
      List2.tail_map (fun c ->
          as_client c.client_client
      ) file.file_clients
  );
  file_ops.op_file_recover <- (fun file ->
      GnutellaServers.recover_file file;
      List.iter (fun c ->
          GnutellaServers.get_file_from_source c file
      ) file.file_clients
  )

  
module P = GuiTypes
  
let _ =
  file_ops.op_file_cancel <- (fun file ->
      remove_file file;
      file_cancel (as_file file.file_file);
      List.iter (fun s ->
          Hashtbl.remove searches_by_uid s.search_uid
      ) file.file_searches
  );
  file_ops.op_file_info <- (fun file ->
      {
        P.file_name = file.file_name;
        P.file_num = (file_num file);
        P.file_network = network.network_num;
        P.file_names = [file.file_name, P.noips()];
        P.file_md4 = Md4.null;
        P.file_size = file_size file;
        P.file_downloaded = file_downloaded file;
        P.file_nlocations = 0;
        P.file_nclients = 0;
        P.file_state = file_state file;
        P.file_sources = None;
        P.file_download_rate = file_download_rate file.file_file;
        P.file_chunks = "0";
        P.file_availability = [network.network_num, "0"];
        P.file_format = FormatNotComputed 0;
        P.file_chunks_age = [|0|];
        P.file_age = file_age file;
        P.file_last_seen = BasicSocket.last_time ();
        P.file_priority = file_priority (as_file file.file_file);
        P.file_uids = [];
      }    
  )
  
let _ =
  server_ops.op_server_info <- (fun s ->
      if !!enable_gnutella then
        {
          P.server_num = (server_num s);
          P.server_network = network.network_num;
          P.server_addr = Ip.addr_of_ip s.server_host.host_ip;
          P.server_port = s.server_host.host_port;
          P.server_score = 0;
          P.server_tags = [];
          P.server_nusers = s.server_nusers;
          P.server_nfiles = s.server_nfiles;
          P.server_state = server_state s;
          P.server_name = s.server_agent;
          P.server_description = "";
          P.server_users = None;
          P.server_banner = "";
          } else
        raise Not_found
  );
  server_ops.op_server_connect <- (fun s ->
      GnutellaServers.connect_server 
        (nservers)
      s.server_gnutella2 
      GnutellaServers.retry_and_fake s.server_host []);
  server_ops.op_server_disconnect <-
(fun s -> GnutellaServers.disconnect_server s BasicSocket.Closed_by_user);
  server_ops.op_server_to_option <- (fun _ -> raise Not_found)

module C = CommonTypes
  
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

  
let _ =
  network.op_network_connected_servers <- (fun _ ->
      List2.tail_map (fun s -> as_server s.server_server)
      !connected_servers
  );
  network.op_network_parse_url <- (fun url ->
      match String2.split (String.escaped url) '|' with
      | "gnut://" :: "server" :: ip :: port :: _ ->  
          let ip = Ip.of_string ip in
          let port = int_of_string port in
          let s = new_server ip port in
          true
      | "gnut://" :: "friend" :: uid :: ip :: port :: _ ->  
          let ip = Ip.of_string ip in
          let port = int_of_string port in
          let md4 = Md4.of_string uid in
          let c = new_client (Known_location (ip, port)) in
          c.client_user.user_uid <- md4;
          friend_add (as_client c.client_client);
          true
      | "gnut://" :: "friend" :: uid :: _ ->
          let md4 = Md4.of_string uid in
          let c = new_client (Indirect_location ("", md4)) in
          friend_add (as_client c.client_client);
          true
      
      | _ -> 
          let (name, uids) = parse_magnet url in
          if uids <> [] then begin
(* Start a download for this file *)
              let r = new_result name Int64.zero [] uids in
              let file = GnutellaServers.download_file r in
              CommonInteractive.start_download file;
              true
            end
          else false
  )
  
let browse_client c = 
  lprintf "Gnutella: browse client not implemented\n";
  ()
  
let _ =
  client_ops.op_client_info <- (fun c ->
      {
        P.client_network = network.network_num;
        P.client_kind = c.client_user.user_kind;
        P.client_state = client_state (as_client c.client_client);
        P.client_type = client_type c;
        P.client_tags = [];
        P.client_name = (match c.client_user.user_kind with
          | Known_location (ip, port) ->
              Printf.sprintf "%s:%d" (Ip.to_string ip) port
          | Indirect_location (_, id) -> 
              Printf.sprintf "UID[%s...]" (String.sub (Md4.to_string id) 0 12)
        );
        P.client_files = None;
        P.client_num = (client_num (as_client c.client_client));
        P.client_rating = 0;
        P.client_chat_port = 0 ;
        P.client_connect_time = BasicSocket.last_time ();
        P.client_software = "";
        P.client_downloaded = zero;
        P.client_uploaded = zero;
        P.client_upload = None;
(*        P.client_sock_addr = (match c.client_user.user_kind with
                        | Known_location (ip,port) -> Ip.to_string ip
                        | _ -> ""); *)
      }
  );
  client_ops.op_client_browse <- (fun c immediate ->
      browse_client c
  );

    client_ops.op_client_bprint <- (fun c buf ->
        let cc = as_client c.client_client in
        let cinfo = client_info cc in
        Printf.bprintf buf "%s (%s)\n"
          cinfo.GuiTypes.client_name
          (string_of_kind cinfo.GuiTypes.client_kind)
    );    
    client_ops.op_client_bprint_html <- (fun c buf file ->
        let cc = as_client c.client_client in
        let cinfo = client_info cc in
        
        html_mods_td buf [
          ("", "sr br ar", Printf.sprintf "%d" (client_num cc));
          ("", "sr br", cinfo.GuiTypes.client_name);
          ("", "sr", (string_of_kind cinfo.GuiTypes.client_kind));
          ("", "sr ar", (size_of_int64 cinfo.GuiTypes.client_uploaded));
          ("", "sr ar br", (size_of_int64 cinfo.GuiTypes.client_downloaded)); ];
    );    
   client_ops.op_client_dprint <- (fun c o file ->
        let info = file_info file in
        let buf = o.conn_buf in
        let cc = as_client c.client_client in
        let cinfo = client_info cc in
        client_print cc o; 
        Printf.bprintf buf "client: %s downloaded: %s uploaded: %s"
          "gN" (* cinfo.GuiTypes.client_software *)
          (Int64.to_string cinfo.GuiTypes.client_downloaded)
        (Int64.to_string cinfo.GuiTypes.client_uploaded);
        Printf.bprintf buf "\nfilename: %s\n\n" info.GuiTypes.file_name;
    );    
    client_ops.op_client_dprint_html <- (fun c o file str ->
        let info = file_info file in
        let buf = o.conn_buf in
        let cc = as_client c.client_client in
        let cinfo = client_info cc in
        Printf.bprintf buf " \\<tr onMouseOver=\\\"mOvr(this);\\\"
    onMouseOut=\\\"mOut(this);\\\" class=\\\"%s\\\"\\>" str;
        
        html_mods_td buf [ 
          ("", "srb ar", Printf.sprintf "%d" (client_num cc));
          ((string_of_connection_state (client_state cc)), "sr",
            (short_string_of_connection_state (client_state cc)));
          ("", "sr", cinfo.GuiTypes.client_name);
          ("", "sr", "gN"); (* cinfo.GuiTypes.client_software *)
          ("", "sr", "F");
          ("", "sr ar", Printf.sprintf "%d" 
              (((last_time ()) - cinfo.GuiTypes.client_connect_time) / 60));
          ("", "sr", "D");
          ("", "sr", (string_of_kind cinfo.GuiTypes.client_kind));
          ("", "sr ar", (size_of_int64 cinfo.GuiTypes.client_uploaded));
          ("", "sr ar", (size_of_int64 cinfo.GuiTypes.client_downloaded));
          ("", "sr", info.GuiTypes.file_name); ];
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
      as_server (new_server (Ip.ip_of_addr ip) port).server_server
  )

open Queues
open GuiTypes
  
let commands = [
    "gstats", Arg_none (fun o ->
        let buf = o.conn_buf in
        Printf.bprintf buf "ultrapeers_waiting_queue: %d\n" 
          (Queue.length ultrapeers_waiting_queue);
        Printf.bprintf buf "peers_waiting_queue: %d\n" 
          (Queue.length peers_waiting_queue);
        Printf.bprintf buf "active_udp_queue: %d\n" 
          (Queue.length active_udp_queue);
        Printf.bprintf buf "waiting_udp_queue: %d\n" 
          (Queue.length waiting_udp_queue);
        ""
    ), " :\t\t\t\tprint stats on Gnutella network";
    
    ]
  
let _ = 
  CommonNetwork.register_commands commands
  
