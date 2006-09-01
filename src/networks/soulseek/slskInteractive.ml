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

open CommonInteractive
open Int64ops
open Printf2
open Md4
open CommonGlobals
open BasicSocket
open CommonComplexOptions
open CommonClient
open CommonFile
open CommonUser
open CommonRoom
open CommonServer
open CommonResult
open CommonTypes
open CommonSearch
open Options
open SlskTypes
open SlskOptions
open SlskGlobals
open SlskProtocol


let download r filenames =
  let result_name = List.hd r.result_names in
  let key = (result_name, r.result_size) in
  if not (Hashtbl.mem files_by_key 
        (String.lowercase result_name)) then begin
      let file = new_file (Md4.random()) result_name r.result_size in
      begin
        try
          List.iter (fun (user, filename) ->
              ignore (add_file_client file user filename)
          ) !(Hashtbl.find result_sources r.result_num);
        with _ -> ()
      end;
      SlskServers.ask_for_file file;
      as_file file.file_file
    end else
    failwith "Download already started"

let _ =
  network.op_network_search <- (fun q buf ->
      
(* Shouldn't we filter searches for other things than mp3s ? *)
(* Shouldn't we also prevent searches if we don't have a correct IP
  address, since the replies won't probably get in ? *)
      
      next_token =:= !!next_token + 1;
      let query = q.search_query in
      let words = ref [] in
      let rec iter q =
        match q with
        | QOr (q1,q2) 
        | QAnd (q1, q2) -> iter q1; iter q2
        | QAndNot (q1,q2) -> iter q1 
        | QHasWord w -> words := w :: !words
        | QHasField(field, w) ->
            begin
              match field with
(*            "type" -> () *)
              | _ -> words := w :: !words
            end
        | _ ->
            ()
      
      in
      iter query;
      lprintf "SEARCH ADDED FOR %d" !!next_token; lprint_newline ();
      SlskGlobals.searches := (!!next_token, q) :: !SlskGlobals.searches;
      let words = String2.unsplit !words ' ' in
      lprintf "SEARCH COMPUTED"; lprint_newline ();
      let msg = C2S.FileSearchReq {
          C2S.FileSearch.words = words;
          C2S.FileSearch.id = !!next_token;
        } in
      List.iter (fun s ->
          do_if_connected s.server_sock (fun sock ->
              lprintf "SENDING SEARCH"; lprint_newline ();
              server_send sock msg; 
              s.server_search <- Some q; 
              Printf.bprintf  buf "Sending search\n") 
      )
      !connected_servers
  )
  
    
let _ =
  let module P = GuiTypes in
  server_ops.op_server_connect <- SlskServers.connect_server;
  server_ops.op_server_disconnect <- (fun s ->
      SlskServers.disconnect_server s Closed_by_user);
(*
  server_ops.op_server_query_users <- (fun s ->
      match s.server_sock with
        None -> ()
      | Some sock ->
          server_send sock (GetNickListReq)
);
  server_ops.op_server_users <- (fun s ->
      List2.tail_map (fun u -> as_user u.user_user) s.server_users
  );
  server_ops.op_server_remove <- (fun s ->
      disconnect_server s;
      server_remove s
);
    *)
  server_ops.op_server_info <- (fun s ->
      { (impl_server_info s.server_server) with
        P.server_num = (server_num s);
        P.server_network = network.network_num;
        P.server_addr = s.server_addr;
        P.server_port = s.server_port;
        P.server_nusers = s.server_nusers;
        P.server_state = server_state s;
        P.server_name = s.server_name;
        P.server_description = s.server_info;
        P.server_preferred = false;
        }
  )

    
let _ = 
  let module P = GuiTypes in
  user_ops.op_user_info <- (fun user ->
      {
        P.user_num = user.user_user.impl_user_num;
        P.user_md4 = Md4.null;
        P.user_name = user.user_nick;
        P.user_ip = Ip.null;
        P.user_port = 0;
        P.user_tags = [];
        P.user_server = 0;
      });
  user_ops.op_user_remove <- (fun user -> ())

module C = CommonTypes
  
  (*
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
        C.result_tags = 
        List.map (fun (user, filename) ->          
            { tag_name = "SOURCE"; tag_value = String (
                Printf.sprintf "[%s]:%s" user.user_nick filename) }
        ) r.result_sources;
        C.result_comment = "";
        C.result_done = false;
      }
  );
  result_ops.op_result_download <- (fun r filenames force ->
      download r filenames   
  )
  *)

let _ =
  network.op_network_download <- (fun r ->
      download r []   
  )

let _ =
  let module P = GuiTypes in
  file_ops.op_file_info <- (fun file ->
      { (impl_file_info file.file_file) with
        P.file_fields = P.Fields_file_info.all;

        P.file_comment = file_comment (as_file file.file_file);
        P.file_name = file_best_name (as_file file.file_file);
        P.file_num = (file_num file);
        P.file_network = network.network_num;
        P.file_md4 = file.file_id;
        P.file_size = file_size file;
        P.file_downloaded = file_downloaded file;
        P.file_state = file_state file;
        P.file_download_rate = file_download_rate file.file_file;
        P.file_chunks = "0";
        P.file_availability = [network.network_num,"0"];
        P.file_format = FormatNotComputed 0;
        P.file_chunks_age = [|0|];
        P.file_age = file_age file;
        P.file_last_seen = BasicSocket.last_time ();
        P.file_priority = file_priority (as_file file.file_file);
      }    
  );
  (*
  file_ops.op_file_save_as <- (fun file new_name  ->
      match file_state file with
        FileDownloaded | FileShared ->
          SlskClients.save_file_as file new_name
      | _ -> ()
);
  *)
  file_ops.op_file_all_sources <- (fun file ->
      List2.tail_map (fun c -> as_client c.client_client)
      file.file_clients
  );
  file_ops.op_file_active_sources <- file_ops.op_file_all_sources

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
  let module P = GuiTypes in
  client_ops.op_client_info <- (fun c ->
      { (impl_client_info c.client_client) with
        P.client_network = network.network_num;
        P.client_kind = Indirect_location (c.client_name, Md4.null, Ip.null, 0);
        P.client_state = client_state (as_client c.client_client);
        P.client_type = client_type c;
        P.client_name = c.client_name;
        P.client_num = (client_num (as_client c.client_client));
        P.client_connect_time = last_time ();
      }
  );
  client_ops.op_client_browse <- (fun c immediate ->
      lprintf "CLIENT BROWSE !!!!"; lprint_newline ();
      SlskClients.connect_peer c 300 [C2C.GetSharedFileListReq]
  )



  
let _ =
  let module P = GuiTypes in
  room_ops.op_room_info <- (fun s ->
      {
        P.room_num = s.room_room.impl_room_num;
        P.room_network = network.network_num;
        P.room_name = s.room_name;
        P.room_state = s.room_room.impl_room_state;
        P.room_users = List2.tail_map (fun u -> 
            u.user_user.impl_user_num
        ) s.room_users;
        P.room_messages = [];
        P.room_nusers = s.room_nusers;
      }
  );
  room_ops.op_room_messages <- (fun s age ->
      extract_messages s.room_messages age
  );
  room_ops.op_room_send_message <- (fun room m ->
      let msg =
        match m with
          PublicMessage (0,m) ->
            C2S.SayChatroomReq (room.room_name, m)
        | _ -> assert false
      in
      List.iter (fun s ->
          do_if_connected s.server_sock (fun sock ->
              server_send sock msg)) !connected_servers  
  );
  room_ops.op_room_name <- (fun s -> s.room_name);
  room_ops.op_room_resume <- (fun r ->
      set_room_state r RoomOpened;
      List.iter (fun s ->
          do_if_connected s.server_sock (fun sock ->
              server_send sock (C2S.JoinRoomReq r.room_name))
      ) !connected_servers
  );
  room_ops.op_room_close <- (fun r ->
      set_room_state r RoomPaused;
      List.iter (fun s ->
          do_if_connected s.server_sock (fun sock ->
              server_send sock (C2S.LeaveRoomReq r.room_name))
      ) !connected_servers
      
  );
    network.op_network_connected <- (fun _ ->
      !connected_servers <> []
  );
  room_ops.op_room_users <- (fun room ->
      let list = ref [] in
      List.iter (fun u ->
          list := (as_user u.user_user) :: !list
      ) room.room_users;
      !list
  )
  
