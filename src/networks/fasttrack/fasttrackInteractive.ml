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

open CommonHosts
open CommonDownloads
open Int64ops
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
open FasttrackTypes
open FasttrackOptions
open FasttrackGlobals
open FasttrackComplexOptions
open BasicSocket
open Autoconf

open FasttrackProtocol

(* Don't share files greater than 10 MB on Fasttrack and limit to 200 files. 
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
          UserSearch (sss, _,_,_) when s == sss -> raise (M.Found ss)
        | _ -> ()
    ) searches_by_uid;
    raise Not_found
  with M.Found s -> s
  
let parse_query q =

  let realm = ref "" in
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
          | Field_Type -> realm := String.lowercase w 
          | Field_Format ->
              begin
                match String.lowercase w with
                | "mp3" | "wav" -> 
                    add_words w;
                    realm := "audio"
                | _ -> add_words w
              end
          | Field_Album -> tags := (Substring ("album", w)) :: !tags
          | Field_Artist -> tags := (Substring ("artist", w)) :: !tags
          | Field_Title -> tags := (Substring ("title", w)) :: !tags
          | Field_unknown tag ->
              if List.mem_assoc tag name_of_tag then
                tags := (Substring (tag, w)) :: !tags
          | Field_Filename ->
              tags := (Substring ("filename", w)) :: !tags
          | Field_Uid -> ()
          | Field_Size -> ()
        end
    | QHasMinVal (field, value) -> 
        begin
          match field with
          | Field_Size -> tags := (AtLeast ("size", value)) :: !tags
          | Field_unknown tag -> 
              if List.mem_assoc tag name_of_tag then
                tags := (AtLeast (tag, value)) :: !tags
          | _ -> ()
        end
    | QHasMaxVal (field, value) -> 
        begin
          match field with
            Field_Size -> tags := (AtMost ("size", value)) :: !tags
          | Field_unknown tag -> 
              if List.mem_assoc tag name_of_tag then
                tags := (AtMost (tag, value)) :: !tags
          | _ -> ()
        end
    | QNone ->  ()
  in
  iter q;
  !keywords, !realm, !tags
      
let _ =
  network.op_network_search <- (fun search buf ->
      let query = search.search_query in
      let words, realm, tags = parse_query query in
      let words = String2.unsplit words ' ' in

(* Maybe we could generate the id for the query from the query itself, so
that we can reuse queries *)
      let uid = Md4.random () in
      
      let ss = {
          (* no exclude, no realm *)
          search_search = UserSearch (search, words, realm, tags);
          search_id = !search_num;
        } in
      incr search_num;
      List.iter (fun s -> FasttrackProto.server_send_query s ss) !connected_servers;
      
      Hashtbl.add searches_by_uid ss.search_id ss;
      ());
  network.op_network_close_search <- (fun s ->
      Hashtbl.remove searches_by_uid (find_search s).search_id  
  );
  network.op_network_forget_search <- (fun s ->
      Hashtbl.remove searches_by_uid (find_search s).search_id  
  );
  network.op_network_connected <- (fun _ ->
      !connected_servers <> []
  );
  network.op_network_share <- (fun fullname codedname size ->
      (*
      if !shared_files_counter < max_shared_files &&
        size < max_shared_file_size then begin
        incr shared_files_counter;
      FasttrackProtocol.new_shared_words := true;
      let sh = CommonUploads.add_shared fullname codedname size in
      CommonUploads.ask_for_uid sh SHA1 (fun sh uid -> 
            lprintf "Could share urn\n";
            ())
end
*)
      ()
  );
  network.op_network_download <- (fun r ->
      let rec iter uids =
        match uids with
          [] -> raise IgnoreNetwork
        | uid :: tail ->
            match Uid.to_uid uid with
              Md5Ext hash ->
                FasttrackServers.download_file hash r
            | _  -> iter tail
      in
      iter r.result_uids
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
          Int64Swarmer.subfiles swarmer)
  ;
  file_ops.op_file_active_sources <- file_ops.op_file_all_sources;
  file_ops.op_file_recover <- (fun file ->
      FasttrackServers.recover_file file;
      List.iter (fun c ->
          FasttrackServers.get_file_from_source c file
      ) file.file_clients
  );
  file_ops.op_file_print_sources_html_header <- (fun file buf info ->
      
      html_mods_table_header buf "sourcesTable" "sources al" [ 
        ( "1", "srh br ac", "Client number", "Num" ) ; 
        ( "0", "srh br", "Client Name", "Name" ) ; 
        ( "0", "srh", "IP address", "IP address" ) ; 
        ( "1", "srh ar", "Total UL bytes to this client for all files", "UL" ) ; 
        ( "1", "srh ar br", "Total DL bytes from this client for all files", "DL" ) ; ]      
  )

  
module P = GuiTypes
  
let _ =
  file_ops.op_file_cancel <- (fun file ->
      remove_file file;
      file_cancel (as_file file);
      Hashtbl.remove searches_by_uid  file.file_search.search_id
  );
  file_ops.op_file_info <- (fun file ->
      {
        P.file_fields = P.Fields_file_info.all;
        
        P.file_comment = file_comment (as_file file);
        P.file_name = file.file_name;
        P.file_num = (file_num file);
        P.file_network = network.network_num;
        P.file_names = file.file_filenames;
        P.file_md4 = Md4.null;
        P.file_size = file_size file;
        P.file_downloaded = file_downloaded file;
        P.file_all_sources = 0;
        P.file_active_sources = 0;
        P.file_state = file_state file;
        P.file_sources = None;
        P.file_download_rate = file_download_rate file.file_file;
        P.file_chunks = (match file.file_swarmer with
            None -> "" | Some swarmer ->
              Int64Swarmer.verified_bitmap swarmer);
        P.file_availability =   [network.network_num,
          (match file.file_swarmer with
          None -> "" | Some swarmer ->
                Int64Swarmer.availability swarmer)];

        P.file_format = FormatNotComputed 0;
        P.file_chunks_age = [|0|];
        P.file_age = file_age file;
        P.file_last_seen = BasicSocket.last_time ();
        P.file_priority = file_priority (as_file file);
        P.file_uids = [];
      }    
  )
  
let _ =
  server_ops.op_server_info <- (fun s ->
      if !!enable_fasttrack then
        {
          P.server_num = (server_num s);
          P.server_network = network.network_num;
          P.server_addr = s.server_host.host_addr;
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
          P.server_preferred = false;
        } 
        else raise Not_found
  );
  server_ops.op_server_connect <- (fun s ->
      FasttrackServers.connect_server s.server_host);
  server_ops.op_server_disconnect <- (fun s ->
      FasttrackServers.disconnect_server s BasicSocket.Closed_by_user);
  server_ops.op_server_to_option <- (fun _ -> raise Not_found)

module C = CommonTypes  
  
let _ =
  network.op_network_connected_servers <- (fun _ ->
      List2.tail_map (fun s -> as_server s.server_server)
      !connected_servers
  );

  network.op_network_parse_url <- (fun url ->
      match String2.split (String.escaped url) '|' with
      | "sig2dat://" :: file :: length :: uuhash :: _ 
      | "sig2dat:///" :: file :: length :: uuhash :: _ ->
      
          let filename =
	    let len = String.length file in
	    let rec iter1 pos =
	      if pos = len then raise Exit;
	      if file.[pos] = ':' then iter2 (pos+1)
	      else iter1 (pos+1)
            and  iter2 pos =
	      if pos = len then raise Exit;
	      if file.[pos] = ' ' then iter2 (pos+1)
	      else String.sub file pos (len - pos)
	    in
	    iter1 0
	  in
      
          let size =

            let len = String.length length in
            let rec iter1 pos =
              if pos = len then raise Exit;
              if length.[pos] = ':' then iter2 (pos+1)
              else iter1 (pos+1)
            and  iter2 pos =
              if pos = len then raise Exit;
              if length.[pos] = ' ' then iter2 (pos+1)
              else iter3 pos (pos+1)
            and iter3 begin_pos pos =
              if pos = len then raise Exit;
              if length.[pos] = 'B' || length.[pos] = ' ' then
                String.sub length begin_pos (pos - begin_pos)
              else iter3 begin_pos (pos+1)
	    in
	    iter1 0
            
          in
          
          let hash = 

            let len = String.length uuhash in
            let rec iter1 pos =
              if pos = len then raise Exit;
              if uuhash.[pos] = '=' then iter2 pos (pos+1)
              else iter1 (pos+1)
            and iter2 begin_pos pos =
              if pos = len then raise Exit;
              if uuhash.[pos] = '=' then
                String.sub uuhash begin_pos (pos+1 - begin_pos)
              else iter2 begin_pos (pos+1)
	    in
	    iter1 0
            
          in
          
          lprintf "sig2dat: [%s] [%s] [%s]\n" filename size hash;
          let size = Int64.of_string size in
          let hash = Md5Ext.of_string hash in
          
          let rs = new_result filename size [] hash in
          let r = get_result rs in
          let file = FasttrackServers.download_file hash r in
          CommonInteractive.start_download file;
          true
      | "ft://" :: "server"  :: ip :: port :: _ ->  
          let ip = Ip.addr_of_string ip in
          let port = int_of_string port in
          let s = new_server ip port in
          true
      | "ft://" :: "friend" :: uid :: ip :: port :: _ ->  
          let ip = Ip.of_string ip in
          let port = int_of_string port in
          let md4 = Md4.of_string uid in
          let c = new_client (Known_location (ip, port)) in
          c.client_user.user_uid <- md4;
          friend_add (as_client c);
          true
      | "ft://" :: "friend" :: uid :: _ ->
          let md4 = Md4.of_string uid in
          let c = new_client (Indirect_location ("", md4)) in
          friend_add (as_client c);
          true
      | _ -> false);
  ()
  
let browse_client c = 
  lprintf "Fasttrack: browse client not implemented\n";
  ()

let string_of_client_addr c =
  match c.client_user.user_kind with
  | Known_location (ip,port) -> Ip.to_string ip
  | _ -> ""
      
let _ =
  client_ops.op_client_info <- (fun c ->
      {
        P.client_network = network.network_num;
        P.client_kind = c.client_user.user_kind;
        P.client_state = client_state (as_client c);
        P.client_type = client_type c;
        P.client_tags = [];
        P.client_name = (match c.client_user.user_kind with
          | Known_location (ip, port) ->
              Printf.sprintf "%s:%d" (Ip.to_string ip) port
          | Indirect_location (_, id) -> 
              Printf.sprintf "UID[%s...]" (String.sub (Md4.to_string id) 0 12)
        );
        P.client_files = None;
        P.client_num = (client_num (as_client c));
        P.client_rating = 0;
        P.client_chat_port = 0 ;
        P.client_connect_time = BasicSocket.last_time ();
        P.client_software = "";
        P.client_emulemod = "";
        P.client_downloaded = zero;
        P.client_uploaded = zero;
        P.client_upload = None;
(*		P.client_sock_addr = (; *)
      }
  );
  client_ops.op_client_browse <- (fun c immediate ->
      browse_client c
  );
    client_ops.op_client_bprint <- (fun c buf ->
        let cc = as_client c in
        let cinfo = client_info cc in
        Printf.bprintf buf "%s (%s)\n"
          cinfo.GuiTypes.client_name
          (string_of_client_addr c)
    );
    client_ops.op_client_bprint_html <- (fun c buf file ->
        let cc = as_client c in
        let cinfo = client_info cc in

        html_mods_td buf [
          ("", "sr br ar", Printf.sprintf "%d" (client_num cc));
          ("", "sr br", cinfo.GuiTypes.client_name);
          ("", "sr", (string_of_client_addr c));
          ("", "sr ar", (size_of_int64 cinfo.GuiTypes.client_uploaded));
          ("", "sr ar br", (size_of_int64 cinfo.GuiTypes.client_downloaded)); ];
    );
   client_ops.op_client_dprint <- (fun c o file ->
        let info = file_info file in
        let buf = o.conn_buf in
        let cc = as_client c in 
        let cinfo = client_info cc in
        client_print cc o;
        Printf.bprintf buf "client: %s downloaded: %s uploaded: %s"
          "fT" (* cinfo.GuiTypes.client_software *)
          (Int64.to_string cinfo.GuiTypes.client_downloaded)
        (Int64.to_string cinfo.GuiTypes.client_uploaded);
        Printf.bprintf buf "\nfilename: %s\n\n" info.GuiTypes.file_name;
    );    
    client_ops.op_client_dprint_html <- (fun c o file str ->
        let info = file_info file in
        let buf = o.conn_buf in
        let cc = as_client c in
        let cinfo = client_info cc in
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
          ("", "sr", "fT"); (* cinfo.GuiTypes.client_software *)
          ] @
          (if !show_emulemods_column then [("", "sr", "")] else [])
          @ [
          ("", "sr", "F");
          ("", "sr ar", Printf.sprintf "%d" 
              (((last_time ()) - cinfo.GuiTypes.client_connect_time) / 60));
          ("", "sr", "D");
          ("", "sr", (string_of_client_addr c));
          ("", "sr ar", (size_of_int64 cinfo.GuiTypes.client_uploaded));
          ("", "sr ar", (size_of_int64 cinfo.GuiTypes.client_downloaded));
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
open GuiTypes
  
let commands = []
  
let _ = 
  CommonNetwork.register_commands commands
  
