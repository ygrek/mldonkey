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
open BasicSocket
open TcpBufferedSocket
open Options
open AnyEndian
  
open CommonSwarming  
open CommonUploads
open CommonOptions
open CommonSearch
open CommonServer
open CommonComplexOptions
open CommonFile
open CommonTypes
open CommonGlobals

  
open FasttrackTypes
open FasttrackGlobals
open FasttrackOptions
open FasttrackProtocol
open FasttrackComplexOptions
open FasttrackProto
          
let udp_packet_handler ip port msg = 
  let h = new_host ip port Ultrapeer in
  host_queue_add active_udp_queue h (last_time ());
  h.host_connected <- last_time ();
(*  if !verbose_udp then
    lprintf "Received UDP packet from %s:%d: \n%s\n" 
      (Ip.to_string ip) port (Print.print msg);*)
  let s = new_server ip port in
  s.server_connected <- int32_time ()


let server_msg_handler sock s msg_type m =
(*  lprintf "Message received: %d len %d\n" msg_type (String.length m); *)
  match msg_type with
    0x00 -> (* SessMsgNodeList *)
      lprintf "SessMsgNodeList\n";
      set_rtimeout sock half_day;
      set_server_state s (Connected (-1));
      s.server_connected <- int32_time ();    
      if not (List.memq s !connected_servers) then
        connected_servers := s :: !connected_servers;
      
      let n = String.length m / 8 in
      for i = 0 to n - 1 do
        let l_ip = LittleEndian.get_ip m (i*8) in
        let l_port = BigEndian.get_int16 m (i*8+4) in
        let unknown = BigEndian.get_int16 m (i*8+6) in
        
        lprintf "    LittleEndian Node %s:%d   %d\n" (Ip.to_string l_ip) l_port unknown;
        let (h : host) = new_host (Ip.addr_of_ip l_ip) l_port Ultrapeer in
        ();
      done;
      if s.server_host.host_kind = IndexServer then
        close sock Closed_by_user
      else begin
          List.iter (fun file ->
              Fifo.put s.server_searches file.file_search
          ) !current_files;
        end
  
  | 0x06 -> (* SessMsgQuery *)
      lprintf "SessMsgQuery\n";

(*


*)
  
  | 0x07 -> (* SessMsgQueryReply *)
      lprintf "SessMsgQueryReply\n";

(* probably supernode address *)
      let s_ip = LittleEndian.get_ip m 0 in
      let s_port = BigEndian.get_int16 m 4 in
      
      let id = BigEndian.get_int16 m 6 in
      
      let s = Hashtbl.find searches_by_uid id in
      
      let nresults = BigEndian.get_int16 m 8 in
      lprintf "Results: %d\n" nresults;
      
      let len = String.length m in
      let rec iter pos n = 
        if n > 0 && pos + 32 < len then
          let user_ip = LittleEndian.get_ip m pos in
          let user_port = BigEndian.get_int16 m (pos+4) in
          let user_bandwidth = get_int8 m (pos+6) in
          let pos = pos + 7 in
          let user_name, user_netname, pos =
            if get_int8 m pos = 2 then
              "unknown", "unknown", pos+1
            else
            let end_name = String.index_from m pos '\001' in
            let end_netname = String.index_from m end_name '\000' in
            String.sub m pos (end_name - pos),
            String.sub m (end_name+1) (end_netname - end_name -1),
            end_netname + 1
          in
          
          lprintf "   User %s@%s %s:%d\n" user_name user_netname
            (Ip.to_string user_ip) user_port;
          
          let result_hash = Md5Ext.direct_of_string (String.sub m pos 20) in
          let checksum, pos = get_dynint m (pos+20) in
          let result_size, pos = get_dynint m pos in
          let ntags, pos = get_dynint m pos in
          let ntags = Int64.to_int ntags in
          
          lprintf "   Result %s size: %Ld tags: %d\n" 
            (Md5Ext.to_string_case false result_hash) result_size ntags;
          
          
          let rec iter_tags name pos n tags =
            if n > 0 && pos < len-2 then
              let tag, pos = get_dynint m pos in
              let tag = Int64.to_int tag in
              let tag_len, pos = get_dynint m pos in
              let tag_len = Int64.to_int tag_len in
              let tagdata = String.sub m pos tag_len in
              let name = if tag = 2 then tagdata else name in
              let tag = try
                  List2.assoc_inv tag name_of_tag
                with _ -> 
                    string_of_int tag
              in
              iter_tags name (pos + tag_len) (n-1) 
              ((string_tag tag tagdata) :: tags)
            else
              name, tags, pos
          in
          let result_name, tags, pos = iter_tags "Unknown" pos ntags [] in
          List.iter (fun tag ->
              lprintf "      Tag: %s --> %s\n" tag.tag_name (string_of_tag
                  tag.tag_value);
          ) tags;
          let user = new_user (Known_location (user_ip, user_port)) in
(*
          let url = Printf.sprintf 
            "FastTrack://%s:%d/.hash=%s" (Ip.to_string user_ip)
            user_port (Md5Ext.to_string_case false result_hash) in *)
          let url = Printf.sprintf 
              "/.hash=%s" (Md5Ext.to_string_case false result_hash) in 
          begin
            match s.search_search with
              UserSearch (sss, _,_,_) ->
                
                let r = new_result result_name result_size tags result_hash in
                add_source r user (FileByUrl url);
                CommonInteractive.search_add_result false sss r.result_result
            
            | FileSearch file ->
                let c = new_client user.user_kind in
                add_download file c (FileByUrl url);
                
                if not (List.mem result_name file.file_filenames) then 
                  file.file_filenames <- file.file_filenames @ [result_name] ;
                
            | _ -> ()
          end;
          iter pos (n-1)
      in
      iter 10 nresults

  | 0x08 -> (* SessMsgQueryEnd *)
      lprintf "SessMsgQueryEnd\n";
      
(*
fst_searchlist_process_reply (FST_PLUGIN->searches, msg_type, msg_data);


*)
      
  | 0x09 -> (* SessMsgNetworkStats *)
      lprintf "SessMsgNetworkStats\n";
      
(*
unsigned int mantissa, exponent;

if(fst_packet_remaining(msg_data) < 12) // 97 bytes total now? was 60?
break;

FST_PLUGIN->stats->users = ntohl(fst_packet_get_uint32 (msg_data));	// number of users	
FST_PLUGIN->stats->files = ntohl(fst_packet_get_uint32 (msg_data));	// number of files

mantissa = ntohs(fst_packet_get_uint16 (msg_data));	// mantissa of size
exponent = ntohs(fst_packet_get_uint16 (msg_data));	// exponent of size

if (exponent >= 30)
FST_PLUGIN->stats->size = mantissa << (exponent-30);
else
FST_PLUGIN->stats->size = mantissa >> (30-exponent);	 

// what follows in the packet is the number of files and their size per media type (6 times)
// we do not currently care for those

// something else with a size of 37 byte follows, dunno what it is

FST_DBG_3 ("received network stats: %d users, %d files, %d GB", FST_PLUGIN->stats->users, FST_PLUGIN->stats->files, FST_PLUGIN->stats->size);
break;


  *)
      
  | 0x1d -> (* SessMsgNetworkName *)
      lprintf "SessMsgNetworkName\n";

      let netname = m in
      
      lprintf "   ***** Sending Network Name\n";
      server_send s 0x1d network_name

  | _ ->
      lprintf "   ******* Unknown message %d\n" msg_type;
      AnyEndian.dump m
