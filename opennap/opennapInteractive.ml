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

open CommonResult
open CommonServer
open CommonTypes
open CommonComplexOptions
open CommonFile
open OpennapGlobals
open Options
open OpennapComplexOptions
open OpennapTypes
module OG = OpennapGlobals

module OS = OpennapServers
module OP = OpennapProtocol
module OC = OpennapClients


let _ =
  server_ops.op_server_print <- (fun s o ->
      let buf = o.conn_buf in
      Printf.bprintf buf
      "Connected to %s:%d (%s) with %d users %d files %d GB\n"
        (Ip.to_string s.server_ip) s.server_port s.server_net s.server_nusers
        s.server_nfiles s.server_size
  )
  
let  _ =
  network.op_network_search <- (fun q  buf ->
      let query = q.search_query in
  let module S = OP.Search in
  let t = S.dummy_search in
  let rec iter t q =
    match q with
    | QOr (q1,q2) 
    | QAnd (q1, q2) ->
        iter (iter t q1) q2
    | QAndNot (q1,q2) -> iter t q1 
    | QHasWord w ->
        begin
          match t.S.artist with
            None -> { t with S.artist = Some w }
          | Some s -> { t with S.artist = Some (Printf.sprintf "%s %s" s w)}
          
          (*
          match t.S.title, t.S.artist with
            None, _ -> { t with S.title = Some w }
          | Some s, None -> { t with S.artist = Some w }
          | Some s, _ ->
{t with S.title = Some (Printf.sprintf "%s %s" s w)}
  *)
        end
    | QHasField(field, value) ->
        Printf.printf "******  HAS FIELD  %s %s ********" field value; 
        print_newline ();
         begin
          match field with
            "Album" | "Title" -> t
          | "Artist" | _ -> t
        end
    | QHasMinVal (field, value) ->
        begin
          match field with
            "bitrate" ->  
              { t with S.bitrate = Some (Int32.to_int value, OP.AtLeast) };
          | "size" -> t
          | _ -> t
        end
    | QHasMaxVal (field, value) ->
        begin
          match field with
            "bitrate" -> 
              { t with S.bitrate = Some (Int32.to_int value, OP.AtBest) };
          | "size" -> t
          | _ -> t
        end
    | QNone ->
	prerr_endline "OpennapInteractive.start_search: QNone in query";
	t
  in
  let msg = iter t query in
  CommonSearch.searches := q :: !CommonSearch.searches;
  List.iter (fun s ->
      OpennapServers.send_search s  (Normal_search q)  msg;
      Printf.bprintf  buf "Sending search\n")
  !OG.connected_servers)
  
let try_send buf num tail =
  let num = int_of_string num in
  let msg = OP.UnknownReq (num, tail) in
  List.iter (fun s ->
      match s.server_sock with
        None -> ()
      | Some sock ->
          OP.server_send sock msg; 
          Printf.bprintf  buf "Sending message\n") !OG.connected_servers

let string_of_length d =
  Printf.sprintf "%02d:%02d" (d / 60) (d mod 60)



let _ = 
  result_ops.CommonResult.op_result_print <- (fun r num o ->
      let buf = o.conn_buf in
      let file = r.result_file in
      Printf.bprintf buf "[Opennap %5d] %-70s %10s   "
      num
      file.file_name (Int32.to_string file.file_size);
(*
file.file_bitrate file.file_freq
(string_of_length file.file_length); 
*)
      List.iter (fun s ->
          Printf.bprintf buf "(%s on %s)"
          s.source_nick (OP.string_of_link s.source_link))
      r.result_sources;
      Printf.bprintf buf "\n";
  );
  result_ops.CommonResult.op_result_download <- (fun r _ ->
      OpennapServers.download_file r)

let file_num file =
  file.file_file.impl_file_num

module P = Gui_proto
  
let _ =
  file_ops.op_file_cancel <- (fun file ->
      let r = file.file_result in
      let f = r.result_file in
      Hashtbl.remove OpennapGlobals.files_by_key (f.file_name, f.file_size);
      current_files := List2.removeq file !current_files      
  );
  file_ops.op_file_print <- (fun file o ->
      let buf = o.conn_buf in
      let f = file.file_result.result_file in
      Printf.bprintf buf "[Opennap %5d] %-50s %10s %10s\n" 
      (file_num file) f.file_name 
      (Int32.to_string f.file_size)
      (Int32.to_string file.file_downloaded)      
  );
  file_ops.op_file_info <- (fun file ->
      let r = file.file_result in
      let f = r.result_file in
       {
        P.file_num = (file_num file);
        P.file_network = network.network_num;
        P.file_names = [f.file_name];
        P.file_md4 = file.file_id;
        P.file_size = f.file_size;
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
  
let _ =
  server_ops.op_server_info <- (fun s ->
      {
        P.server_num = (server_num s);
        P.server_network = network.network_num;
        P.server_ip = s.server_ip;
        P.server_port = s.server_port;
        P.server_score = 0;
        P.server_tags = [];
        P.server_nusers = s.server_nusers;
        P.server_nfiles = s.server_nfiles;
        P.server_state = server_state s;
        P.server_name = s.server_desc;
        P.server_description = s.server_net;
        P.server_users = None;
      }
  )

module C = CommonTypes
  
let _ =
  result_ops.op_result_info <- (fun r ->
       {
        C.result_num = r.result_result.impl_result_num;    
        C.result_network = network.network_num;
        
        C.result_names = [r.result_file.file_name];
        C.result_md4 = Md4.null;
        C.result_size = r.result_file.file_size;
        C.result_format = "";
        C.result_type = "";
        C.result_tags = [];
        C.result_comment = None;
        C.result_done = false;
      }   
  )
  