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

open CommonFile
open CommonUser
open CommonChatRoom
open CommonServer
open CommonResult
open CommonTypes
open CommonSearch
open Options
open DcTypes
open DcOptions
open DcGlobals
open DcProtocol

module CO = CommonOptions

let download r filenames =
  let key = (r.result_name, r.result_size) in
  if not (Hashtbl.mem files_by_key key) then begin
      let file = new_file (Md4.random()) r.result_name r.result_size in
      List.iter (fun (s, filename) ->
          match s.source_server.server_sock with
            None -> ()
          | Some sock ->
              let c = add_source file s filename in
              server_send sock (
                let module C = ConnectToMe in
                ConnectToMeReq {
                  C.nick = s.source_server.server_last_nick;
                  C.ip = !!CO.client_ip;
                  C.port = !!dc_port;
                }
              )
      ) r.result_sources;
      ()
    end
    
let _ =
  server_ops.op_server_print <- (fun s o ->
      let buf = o.conn_buf in
      Printf.bprintf buf
        "Connected to %s:%d (%s) nusers %d\n"
        (DcServers.server_addr s) s.server_port
        s.server_name s.server_nusers      
  )

let _ =
  network.op_network_search <- (fun q buf ->
      Printf.printf "+++++++++++++  SEARCH ON DC +++++++++++++"; print_newline ();
      Printf.printf "+++++++++++++  SEARCH ON DC +++++++++++++"; print_newline ();
      Printf.printf "+++++++++++++  SEARCH ON DC +++++++++++++"; print_newline ();
      Printf.printf "+++++++++++++  SEARCH ON DC +++++++++++++"; print_newline ();
      let query = q.search_query in
      let module S = Search in
      let words = ref [] in
      let filetype = ref 0 in
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
(*            "type" -> () *)
              | _ -> words := w :: !words
            end
        | QHasMinVal (field, value) ->
            begin
              match field with
              | "size" -> sizelimit := AtLeast value
              | _ -> ()
            end
        | QHasMaxVal (field, value) ->
            begin
              match field with
              | "size" -> sizelimit := AtMost value
              | _ -> ()
            end
        | QNone ->
            prerr_endline "DcInteractive.start_search: QNone in query";
            ()
      
      in
      iter query;
      searches := q :: !searches;
      let words = String2.unsplit !words ' ' in
      Printf.printf "+++++++++++++  SENDING SEARCH ON DC +++++++++++++"; print_newline ();
      Printf.printf "+++++++++++++  SENDING SEARCH ON DC +++++++++++++"; print_newline ();
      Printf.printf "+++++++++++++  SENDING SEARCH ON DC +++++++++++++"; print_newline ();
      List.iter (fun s ->
          Printf.printf "FOR CONNECTED SERVER"; print_newline ();
          let msg = SearchReq {
              S.orig = Printf.sprintf "Hub:%s" s.server_last_nick;
              S.sizelimit = !sizelimit;
              S.filetype = !filetype;
              S.words = words;
            } in
          Printf.printf "??????"; print_newline ();
          match s.server_sock with
            None -> Printf.printf "NOT CONNECTED !!!!!!!!!"; print_newline ();
          | Some sock ->
              server_send sock msg; 
              s.server_searches <- q :: s.server_searches; 
              Printf.bprintf  buf "Sending search\n") !connected_servers
  )  

let _ =
  result_ops.op_result_print <- (fun r count o ->
      let buf = o.conn_buf in
      Printf.bprintf buf "[DC %5d] %-70s %10ld   "
        count
        r.result_name r.result_size;
      List.iter (fun (s,_) ->
          Printf.bprintf buf "(%s on %s)" s.source_nick 
          s.source_server.server_name)
      r.result_sources;
      Printf.bprintf buf "\n";
  );
  result_ops.op_result_download <- (fun r filenames ->
      download r filenames   
  )
  
let _ =
  room_ops.op_room_info <- (fun s ->
      {
        P.room_num = s.server_room.impl_room_num;
        P.room_network = network.network_num;
        P.room_name = s.server_name;
        P.room_state = s.server_room.impl_room_state;
        P.room_users = List2.tail_map (fun u -> 
            u.user_user.impl_user_num
        ) s.server_users;
      }
  );
  room_ops.op_room_messages <- (fun s ->
      let list = List.rev s.server_messages in
      s.server_messages <- [];
      list);
  room_ops.op_room_send_message <- (fun s m ->
      match s.server_sock with
        None -> ()
      | Some sock ->
          match m with
            PublicMessage (0,m) ->
              let m = Printf.sprintf "<%s> %s" s.server_last_nick m in
              server_send sock (MessageReq m)
          | _ -> assert false
  )
  
let _ = 
  user_ops.op_user_info <- (fun user ->
      {
        P.user_num = user.user_user.impl_user_num;
        P.user_md4 = Md4.null;
        P.user_name = user.user_nick;
        P.user_ip = Ip.null;
        P.user_port = 0;
        P.user_tags = [];
        P.user_server = user.user_server.server_server.impl_server_num;
        P.user_state = user.user_user.impl_user_state;
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
        C.result_format = "";
        C.result_type = "";
        C.result_tags = [];
        C.result_comment = None;
        C.result_done = false;
      }
  )
  
let _ =
  file_ops.op_file_info <- (fun file ->
       {
        P.file_num = (file_num file);
        P.file_network = network.network_num;
        P.file_names = [file.file_name];
        P.file_md4 = Md4.null;
        P.file_size = file.file_size;
        P.file_downloaded = file.file_downloaded;
        P.file_nlocations = 0;
        P.file_nclients = 0;
        P.file_state = file_state file;
        P.file_sources = None;
        P.file_download_rate = 0.0;
        P.file_chunks = "0";
        P.file_availability = "0";
        P.file_format = Unknown_format;
        P.file_chunks_age = [|0.0|];
        P.file_age = 0.0;
      }    
  )