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
  
open CommonOptions
open CommonGlobals
open CommonComplexOptions
open CommonClient
open CommonFile
open CommonUser
open CommonRoom
open CommonServer
open CommonResult
open CommonTypes
open CommonSearch
  
open DcTypes
open DcOptions
open DcGlobals
open DcProtocol

module CO = CommonOptions

let download r filenames =
  let key = (r.result_name, r.result_size) in
  if not (Hashtbl.mem files_by_key key) then begin
      let file = new_file (Md4.random()) r.result_name r.result_size in
      List.iter (fun (user, filename) ->
          ignore (add_file_client file user filename)
      ) r.result_sources;
      DcServers.ask_for_file file
    end

let browse_client c =
  DcServers.try_connect_client c          
    
let _ =
  network.op_network_search <- (fun q buf ->
      let query = q.search_query in
      let module S = Search in
      let words = ref [] in
      let filetype = ref 1 in
      let sizelimit = ref NoLimit in
      let rec iter q =
        match q with
        | QOr (q1,q2) 
        | QAnd (q1, q2) -> iter q1; iter q2
        | QAndNot (q1,q2) -> iter q1 
        | QHasWord w -> words := w :: !words
        | QHasField(field, w) ->
            begin
              match field with
              | Field_Type -> begin
(*
1 for any file type
2 for audio files ("mp3", "mp2", "wav", "au", "rm", "mid", "sm")
3 for compressed files ("zip", "arj", "rar", "lzh", "gz", "z", "arc", "pak")
4 for documents ("doc", "txt", "wri", "pdf", "ps", "tex")
5 for executables ("pm", "exe", "bat", "com")
6 for pictures ("gif", "jpg", "jpeg", "bmp", "pcx", "png", "wmf", "psd")
7 for video ("mpg", "mpeg", "avi", "asf", "mov")
8 for folders
*)
                    
                    match w with
                    | "Audio" -> filetype := 2
                    | "Video" -> filetype := 7
                    | "Doc" -> filetype := 4
                    | _ -> lprintf "Unknown document type [%s]" w;
                        lprint_newline ();
                  end
                  
              | _ -> words := w :: !words
            end
        | QHasMinVal (field, value) ->
            begin
              match field with
              | Field_Size -> sizelimit := AtLeast value
              | _ -> ()
            end
        | QHasMaxVal (field, value) ->
            begin
              match field with
              | Field_Size -> sizelimit := AtMost value
              | _ -> ()
            end
        | QNone ->
            lprintf "DcInteractive.start_search: QNone in query\n";
            ()
      
      in
      iter query;
      let words = String2.unsplit !words ' ' in
      List.iter (fun s ->
          match s.server_sock with
            None -> ()
              | Some sock ->
              let msg = SearchReq {
              S.orig = 
              (if (not DcServers.active_search_supported) || !!firewalled then
                  Printf.sprintf "Hub:%s" s.server_last_nick
                else
                  Printf.sprintf "%s:%d" (
                    Ip.to_string (CO.client_ip (Some sock)))
                  !!dc_port
              );
              S.sizelimit = !sizelimit;
              S.filetype = !filetype;
              S.words = words;
            } in
              server_send !CommonOptions.verbose_msg_servers sock msg; 
              s.server_search <- Some q; 
              s.server_search_timeout <- last_time () + !!search_timeout;
              Printf.bprintf  buf "Sending search\n") !connected_servers
  );
  network.op_network_connected <- (fun _ ->
      !connected_servers <> []
  );
  network.op_network_parse_url <- (fun url ->
      match String2.split (String.escaped url) '|' with
      | "dc://" :: "server" :: addr :: _ ->  
          let addr, port = 
            let ip, port = match String2.split addr ':' with
                [ addr; port ] -> addr, int_of_string port
              | _ -> addr, 411
            in            
            let addr = Ip.addr_of_string addr in
            addr, port
          in
          let s = new_server addr port in
          true
      | "dc://" :: "friend" :: nick :: [] ->  
          let c = new_client nick in
          friend_add (as_client c.client_client);
          true
      | "dc://" :: "friend" :: nick :: addr :: _ ->  
          let addr, port = 
            let ip, port = match String2.split addr ':' with
                [ addr; port ] -> addr, int_of_string port
              | _ -> addr, 411
            in            
            let ip = Ip.of_string addr in
            ip, port
          in
          let c = new_client nick in
          c.client_addr <- Some (addr, port);
          friend_add (as_client c.client_client);
          true
      | _ -> false
  )

let _ =
  result_ops.op_result_download <- (fun r filenames force ->
      download r filenames   
  )

module P = GuiTypes
  
    
let _ =
  server_ops.op_server_connect <- DcServers.connect_server;
  server_ops.op_server_disconnect <- (fun s -> 
					  DcServers.disconnect_server s Closed_by_user);
  server_ops.op_server_query_users <- (fun s ->
      match s.server_sock with
        None -> ()
      | Some sock ->
          server_send !verbose_msg_servers sock (GetNickListReq)
  );
  server_ops.op_server_users <- (fun s ->
      let list = ref [] in
      StringMap.iter (fun _ u -> list := as_user u.user_user :: !list) 
      s.server_users;
      !list
  );
  server_ops.op_server_remove <- (fun s ->
      DcServers.disconnect_server s Closed_by_user;
      server_remove s
  )

let _ =
  room_ops.op_room_info <- (fun s ->
      {
        P.room_num = s.server_room.impl_room_num;
        P.room_network = network.network_num;
        P.room_name = s.server_name;
        P.room_state = s.server_room.impl_room_state;
        P.room_users = (* List2.tail_map (fun u -> 
            u.user_user.impl_user_num
        ) s.server_users*) [];
        P.room_messages = [];
        P.room_nusers = StringMap.length s.server_users;
      }
  );
  room_ops.op_room_users <- server_ops.op_server_users;
  room_ops.op_room_messages <- (fun s age ->
      extract_messages s.server_messages age);
  room_ops.op_room_send_message <- (fun s m ->
      match s.server_sock with
        None -> ()
      | Some sock ->
          match m with
            PublicMessage (0,m) ->
              let m = Printf.sprintf "<%s> %s" s.server_last_nick m in
              server_send !verbose_msg_servers sock (MessageReq m)
          | _ -> assert false
  );
  room_ops.op_room_name <- (fun s ->
      s.server_name
  )
  
let _ = 
  user_ops.op_user_info <- (fun user ->
      {
        P.user_num = user.user_user.impl_user_num;
        P.user_md4 = Md4.null;
        P.user_name = user.user_nick;
        P.user_ip = Ip.null;
        P.user_port = 0;
        P.user_tags = (
          let list = if user.user_data > 1. then 
              [ 
                { tag_name =  "link"; tag_value = String user.user_link };
                { tag_name =  "shared"; tag_value = String (
                    Printf.sprintf "%12.0f" user.user_data) }
              ]          else []
          in
          if user.user_admin then
            { tag_name =  "admin"; tag_value = String "admin" } :: list
          else list
        );
            
        
        P.user_server = (match user.user_servers with
            [] -> 
              lprintf "%s(%d) is not on any server" user.user_nick user.user_user.impl_user_num;
              lprint_newline ();
              0
          | s :: _ -> s.server_server.impl_server_num);
      });
  user_ops.op_user_remove <- (fun user -> ())

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
        C.result_tags = [];
        C.result_comment = "";
        C.result_done = false;
      }
  )
  
let _ =
  file_ops.op_file_info <- (fun file ->
      {
        P.file_name = file.file_name;
        P.file_num = (file_num file);
        P.file_network = network.network_num;
        P.file_names = [file.file_name];
        P.file_md4 = file.file_id;
        P.file_size = file_size file;
        P.file_downloaded = file_downloaded file;
        P.file_nlocations = 0;
        P.file_nclients = 0;
        P.file_state = file_state file;
        P.file_sources = None;
        P.file_download_rate = file_download_rate file.file_file;
        P.file_chunks = "0";
        P.file_availability = [network.network_num, "0"];
        P.file_format = FormatUnknown;
        P.file_chunks_age = [|0|];
        P.file_age = file_age file;
        P.file_last_seen = BasicSocket.last_time ();
        P.file_priority = file_priority (as_file file.file_file);
        P.file_uids = [];
      }    
  );
  (*
  file_ops.op_file_save_as <- (fun file new_name  ->
      match file_state file with
        FileDownloaded | FileShared ->
          DcClients.save_file_as file new_name
      | _ -> ()
);
  *)
  file_ops.op_file_sources <- (fun file ->
      List2.tail_map (fun c -> as_client c.client_client)
      file.file_clients
  )

let client_of_user user =
  let c = new_client user.user_nick in
  c
  
let _ =
  user_ops.op_user_browse_files <- (fun user ->
      let c = client_of_user user in
      contact_add (as_client c.client_client)
  );
  user_ops.op_user_set_friend <- (fun user ->
      let c = client_of_user user in
      friend_add (as_client c.client_client)
  )
  
  
let _ =
  client_ops.op_client_info <- (fun c ->
      {
        P.client_network = network.network_num;
        P.client_kind = Indirect_location (c.client_name, Md4.null);
        P.client_state = client_state (as_client c.client_client);
        P.client_type = client_type c;
        P.client_tags = [];
        P.client_name = c.client_name;
        P.client_files = None;
        P.client_num = (client_num (as_client c.client_client));
        P.client_rating = 0;
        P.client_chat_port = 0 ;
        P.client_connect_time = last_time ();
        P.client_software = "";
        P.client_downloaded = zero;
        P.client_uploaded = zero;
        P.client_upload = None;
		P.client_sock_addr = "";
      }
  );
  client_ops.op_client_browse <- (fun c immediate ->
      browse_client c
  );
  client_ops.op_client_files <- (fun c -> 
      match c.client_all_files with None -> [] | Some list -> 
          List2.tail_map (fun (s, r) ->
            s, as_result r.result_result  
          ) list)

let _ =
  network.op_network_save_complex_options <- (fun _ -> ());
  network.op_network_add_server <- (fun ip port ->
      as_server (new_server ip port).server_server
  )
  
