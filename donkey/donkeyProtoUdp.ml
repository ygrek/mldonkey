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
open Autoconf
open LittleEndian
open CommonTypes
open CommonGlobals
open DonkeyMftp

module QueryUdpReply  = struct 
    
    type t = tagged_file
    
    let names_of_tag = [
        1, "filename";
        2, "size";
        3, "type";
        4, "format";
        21, "availability";
      ]        
    
    
    let get_file  s pos =
      let md4 = get_md4 s pos in
      let ip = get_ip s (pos + 16) in
      let port = get_port s (pos + 20) in
      let tags, pos = get_tags s (pos+22) names_of_tag in
      let file = {
          f_md4 = md4;
          f_ip = ip;
          f_port = port;
          f_tags = tags;
        } in
      file
    
    
    let parse len s =
      get_file s 1
    
    let print t = 
      lprintf "FOUND:\n";
      lprintf "  MD4: %s\n" (Md4.to_string t.f_md4);
      lprintf "  ip: %s\n" (Ip.to_string t.f_ip);
      lprintf "  port: %d\n" t.f_port;
      lprintf "  tags: ";
      print_tags t.f_tags;
      lprint_newline ()

    let fprint oc t = 
      Printf.fprintf oc "FOUND:\n";
      Printf.fprintf oc "%s\n" (Md4.to_string t.f_md4);
      Printf.fprintf oc "%s\n" (Ip.to_string t.f_ip);
      Printf.fprintf oc "%d\n" t.f_port;
      Printf.fprintf oc "TAGS:\n";
      fprint_tags oc t.f_tags;
         Printf.fprintf oc "\n"
    
    let write buf file =
      buf_md4 buf file.f_md4;
      buf_ip buf file.f_ip;
      buf_port buf file.f_port;
      buf_tags buf file.f_tags names_of_tag

  end
  
module QueryCallUdp  = struct 
    type t = {
        ip : Ip.t;
        port : int;
        id : Ip.t;
      }
          
    let parse len s = 
      let ip = get_ip s 1 in
      let port = get_port s 5 in
      let id = get_ip s 7 in
      { ip = ip; port = port; id = id; }
      
    let print t = 
      lprintf "QueryCall %s : %d --> %s" (Ip.to_string t.ip) t.port
        (Ip.to_string t.id);
      lprint_newline ()

    let fprint oc t = 
      Printf.fprintf oc "QueryCall %s : %d --> %s\n" (Ip.to_string t.ip) t.port
        (Ip.to_string t.id)
      
    let write buf t = 
      buf_ip buf t.ip;
      buf_port buf t.port;
      buf_ip buf t.id
      
  end

  
module PingServerUdp = struct (* client -> serveur pour identification ? *)
    type t = int64
      
      
    let parse len s =
      try
	get_int64_32 s 1(*, get_int8 s 2, get_int8 s 3*)
      with _ ->
	Int64.zero
      
    (*let print (t1,t2,t3) = 
      lprintf "MESSAGE 150 UDP %d %d %d" t1 t2 t3;
      lprint_newline ()*)

    let print t =
      lprintf "PING %s\n " (Int64.to_string t)
            
    let fprint oc t =
      Printf.fprintf oc "PING %s\n" (Int64.to_string t)
      
    let write buf t =
      buf_int64_32 buf t
                 
                   
    (* let fprint oc (t1,t2,t3) = 
      Printf.fprintf oc "MESSAGE 150 UDP %d %d %d\n" t1 t2 t3*)
      
    (*let write buf (t1,t2,t3) = 
      buf_int8 buf t1;
      buf_int8 buf t2;
      buf_int8 buf t3;*)
      
  end

module PingServerReplyUdp = struct (* reponse du serveur a 150 *)
    type t = int64 *  int64 * int64
      
    (*let parse len s =
      get_int8 s 1, get_int8 s 2, get_int8 s 3, get_int8 s 4,
      get_int64_32 s 5, get_int64_32 s 9
      
    let print (t1,t2,t3,t4,t5,t6) = 
      lprintf "MESSAGE 150 UDP %d %d %d %d %s %s" t1 t2 t3 t4
        (Int64.to_string t5) (Int64.to_string t6);
      lprint_newline ()

    let fprint oc (t1,t2,t3,t4,t5,t6) = 
      Printf.fprintf oc "MESSAGE 150 UDP %d %d %d %d %s %s" t1 t2 t3 t4
        (Int64.to_string t5) (Int64.to_string t6)
      
    let write buf (t1,t2,t3,t4,t5,t6) = 
      buf_int8 buf t1;
      buf_int8 buf t2;
      buf_int8 buf t3;
      buf_int8 buf t4;
      buf_int64_32 buf t5;
      buf_int64_32 buf t6;*)
      
    let parse len s =
        get_int64_32 s 1, get_int64_32 s 5, get_int64_32 s 9

     let print (t1,t2,t3) =
         lprintf "PING REPLY no:%s clients:%s files:%s\n" (Int64.to_string t1)
                           (Int64.to_string t2) (Int64.to_string t3)
      
     let fprint oc (t1,t2,t3) =
         Printf.fprintf oc "PING REPLY no:%s clients:%s files:%s\n" (Int64.to_string t1)
                           (Int64.to_string t2) (Int64.to_string t3)

      let write buf (t1,t2,t3) =
          buf_int64_32 buf t1;
          buf_int64_32 buf t2;
          buf_int64_32 buf t3;
                           
  end
  
module ServerDescUdp = struct
  type t = {
    ip : Ip.t;
  }

  let parse len s =
    try
      let ip = get_ip s 1 in
	{
	  ip = ip
	}
    with _ ->
      {
	  ip = Ip.null
      }
      
  let print t =
    lprintf "ServerDescUdpReq %s\n" (Ip.to_string t.ip)

  let write buf t =
    buf_ip buf t.ip

end

module ServerDescReplyUdp = struct
  type t = {
    name : string;
    desc : string;
  }

  let parse len s =
    let name, pos = get_string s 1 in
    let desc, pos = get_string s pos in
     {
       name = name;
       desc = desc;
     }
      
  let print t =
    lprintf "ServerDescReplyUdpReq\n";
    lprintf "name : %s\n" t.name;
    lprintf "desc : %s\n" t.desc

  let write buf t =
    buf_string buf t.name;
    buf_string buf t.desc

end


    
module ServerListUdp = struct
  type t = {
    ip : Ip.t;
  }

  let parse len s =
    try
      let ip = get_ip s 1 in
	{
	  ip = ip;
	}
    with _ ->
      {
	ip = Ip.null
      }
	
  let print t =
    lprintf "ServerListUdp %s\n" (Ip.to_string t.ip)

  let write buf t =
    buf_ip buf t.ip

end

module QueryServers = DonkeyProtoServer.QueryServers
module QueryServersReply = DonkeyProtoServer.QueryServersReply
module QueryLocation = DonkeyProtoServer.QueryLocation  
module QueryLocationReply = DonkeyProtoServer.QueryLocationReply  
module Query = DonkeyProtoServer.Query
  
type t =
| QueryServersUdpReq of QueryServers.t  
| QueryServersReplyUdpReq of QueryServersReply.t  

| PingServerUdpReq of PingServerUdp.t
| PingServerReplyUdpReq of PingServerReplyUdp.t

| QueryLocationUdpReq of QueryLocation.t  
| QueryLocationReplyUdpReq of QueryLocationReply.t  

| QueryReplyUdpReq of QueryUdpReply.t
| QueryUdpReq of CommonTypes.query
| QueryCallUdpReq of QueryCallUdp.t
| FileGroupInfoUdpReq of QueryLocationReply.t    
| ServerDescUdpReq of ServerDescUdp.t
| ServerDescReplyUdpReq of ServerDescReplyUdp.t   
| ServerListUdpReq of ServerListUdp.t    

| EmuleReaskFilePingUdpReq of Md4.t  
| EmuleReaskAckUdpReq of Md4.t
| EmuleFileNotFoundUdpReq
| EmuleQueueFullUdpReq

  
| UnknownUdpReq of int * string
    
let parse magic s =
  try 
    let len = String.length s in
    if len = 0 then raise Not_found;
    let opcode = int_of_char (s.[0]) in
(*    lprintf "opcode: %d" opcode; lprint_newline (); *)
    match opcode with 
    | 150 -> PingServerUdpReq (PingServerUdp.parse len s)
    | 151 -> PingServerReplyUdpReq (PingServerReplyUdp.parse len s)
    
    | 152 -> QueryUdpReq (Query.parse len s)
    | 153 -> QueryReplyUdpReq (QueryUdpReply.parse len s)
    | 154 -> QueryLocationUdpReq (QueryLocation.parse len s)
    | 155 -> QueryLocationReplyUdpReq (QueryLocationReply.parse len s)
    | 156 -> QueryCallUdpReq (QueryCallUdp.parse len s)
    | 160 -> QueryServersUdpReq (QueryServers.parse len s)
    | 161 -> QueryServersReplyUdpReq (QueryServersReply.parse len s)
    | 162 -> ServerDescUdpReq (ServerDescUdp.parse len s)
    | 163 -> ServerDescReplyUdpReq (ServerDescReplyUdp.parse len s)
    | 164 -> ServerListUdpReq (ServerListUdp.parse len s)

    | 144 -> EmuleReaskFilePingUdpReq (get_md4 s 1)
    | 145 -> EmuleReaskAckUdpReq (get_md4 s 1)
    | 146 -> EmuleFileNotFoundUdpReq
    | 147 -> EmuleQueueFullUdpReq
        
    | _ -> raise Exit  
  with
    e -> 
      lprintf "From UDP:"; lprint_newline ();
      dump s;
      UnknownUdpReq (magic, s)
      
            
let print t =
  begin
    match t with
    
    | QueryUdpReq t -> Query.print t
    | QueryReplyUdpReq t -> QueryUdpReply.print t
    | QueryLocationUdpReq t -> QueryLocation.print t
    | QueryLocationReplyUdpReq t
    | FileGroupInfoUdpReq t      ->    QueryLocationReply.print t
    | QueryCallUdpReq t -> QueryCallUdp.print t

        | QueryServersUdpReq t -> QueryServers.print t
    | QueryServersReplyUdpReq t -> QueryServersReply.print t
    
    | PingServerUdpReq t -> PingServerUdp.print t
    | PingServerReplyUdpReq t -> PingServerReplyUdp.print t
    | ServerDescUdpReq t -> ServerDescUdp.print t
    | ServerDescReplyUdpReq t -> ServerDescReplyUdp.print t
    | ServerListUdpReq t -> ServerListUdp.print t
    
    | EmuleReaskFilePingUdpReq md4 ->
        lprintf "EmuleReaskFilePingUdpReq %s" (Md4.to_string md4)
    | EmuleReaskAckUdpReq md4 ->
        lprintf "EmuleReaskAckUdpReq %s" (Md4.to_string md4)
    | EmuleFileNotFoundUdpReq ->
        lprintf "EmuleFileNotFoundUdpReq"
    | EmuleQueueFullUdpReq ->
        lprintf "EmuleQueueFullUdpReq"
        
    | UnknownUdpReq (magic, s) -> 
        lprintf "UnknownReq magic %d" magic; lprint_newline ();
        dump s; lprint_newline ();
  end;
  lprint_newline ()
  
let write buf t =
  match t with
  
  | UnknownUdpReq (magic, s) ->
      buf_int8 buf magic;
      Buffer.add_string buf s
  
  | EmuleReaskFilePingUdpReq md4 ->
      buf_int8 buf 197;
      buf_int8 buf 145;
      buf_md4 buf md4
  
  | EmuleReaskAckUdpReq md4 ->
      buf_int8 buf 197;
      buf_int8 buf 145;
      buf_md4 buf md4
  
  | EmuleFileNotFoundUdpReq ->
      buf_int8 buf 197;
      buf_int8 buf 146
  
  | EmuleQueueFullUdpReq ->
      buf_int8 buf 197;
      buf_int8 buf 147        
  
  | _ ->
      buf_int8 buf 227;
      match t with
      | QueryServersUdpReq t -> 
          buf_int8 buf 160;
          QueryServers.write buf t
      | QueryServersReplyUdpReq t -> 
          buf_int8 buf 161;
          QueryServersReply.write buf t
      
      | ServerDescUdpReq t ->
          buf_int8 buf 162;
          ServerDescUdp.write buf t
      | ServerDescReplyUdpReq t ->
          buf_int8 buf 163;
          ServerDescReplyUdp.write buf t
      | ServerListUdpReq t ->
          buf_int8 buf 164;
          ServerListUdp.write buf t
      
      | PingServerUdpReq t -> 
          buf_int8 buf 150;
          PingServerUdp.write buf t
      | PingServerReplyUdpReq t -> 
          buf_int8 buf 151;
          PingServerReplyUdp.write buf t
      
      | QueryLocationUdpReq t ->
          buf_int8 buf 154;
          QueryLocation.write buf t
      | QueryLocationReplyUdpReq t ->
          buf_int8 buf 155;
          QueryLocationReply.write buf t
      | QueryUdpReq t -> 
          buf_int8 buf 152;
          Query.write buf t
      | QueryReplyUdpReq t ->
          buf_int8 buf 153;
          QueryUdpReply.write buf t
      | QueryCallUdpReq t -> 
          buf_int8 buf 156;
          QueryCallUdp.write buf t
      | FileGroupInfoUdpReq t ->
          buf_int8 buf 251;
          QueryLocationReply.write buf t

          
      | EmuleQueueFullUdpReq
      | EmuleFileNotFoundUdpReq
      | EmuleReaskAckUdpReq _
      | EmuleReaskFilePingUdpReq _
      | UnknownUdpReq _ -> assert false
