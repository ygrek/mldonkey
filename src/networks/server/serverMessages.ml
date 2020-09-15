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

(* declaration des differents types de messages entre les servers*)

open Md4

open BasicSocket
open TcpBufferedSocket


open GuiProto
open LittleEndian
open CommonTypes
open CommonGlobals
open DonkeyMftp


module M = DonkeyProtoServer

let buf_bool buf bl =
  let str = string_of_bool bl in
    buf_string buf str

let get_bool s pos =
  let str,size = get_string s pos in
    bool_of_string str,size 
    

(*new server connect to the groupe*)
module ServerConnect = struct
    type t = {
        md4 : Md4.t;
        ip: Ip.t;
        port: int;
        max_clients : int;
        max_files : int;
        tags : tag list;
        
 }

  let names_of_tag =
      [
        1, "name";
        11, "description";
      ]

    let parse len s =
      let md4 = get_md4 s 1 in
      let ip = get_ip s 17 in
      let port = get_port s 21 in
(*      lprintf "port: %d" port; lprint_newline (); *)
      let max_clients = get_int s 23 in
      let max_files = get_int s 27 in
      let ntags = get_int s 31 in
      let tags, pos = get_tags s 35 ntags names_of_tag in
      {
        md4 = md4;
        ip = ip;
        port = port;
        max_clients = max_clients;
        max_files = max_files;
        tags = tags;
      }

    let print t =
      lprintf "SERVER CONNECT:\n";
      lprintf "MD4: %s\n" (Md4.to_string t.md4);
      lprintf "ip: %s\n" (Ip.to_string t.ip);
      lprintf "port: %d\n" t.port;
      lprintf "max_clients: %d\n" t.max_clients;
      lprintf "max_files: %d\n" t.max_files;
      lprintf "tags: ";
      print_tags t.tags

    let write buf t =
      buf_md4 buf t.md4;
      buf_ip buf t.ip;
      buf_port buf t.port;
      buf_int buf t.max_clients;
      buf_int buf t.max_files;
      buf_tags buf t.tags names_of_tag

end

module ACKConnect = struct 
  type t = {
    group_id : Md4.t;
    server_master_id : int;
    server_id : int;
    tags : tag list;
  }

  let names_of_tag =
    [
      1, "name";
      11, "description";
    ]

  let parse len s =
    let group_id = get_md4 s 1 in
    let server_master_id = get_int s 17 in
    let server_id = get_int s 21 in
    let ntags = get_int s 25 in 
    let tags, pos = get_tags s 29 ntags names_of_tag in
      {
        group_id = group_id;
        server_master_id = server_master_id;
        server_id = server_id;
        tags = tags;
      }
     
  let print t =
    lprintf "SERVER ACCEPT IN GROUPE:\n";
    lprintf "Group_id: %s\n" (Md4.to_string t.group_id);
    lprintf "master_id: %d \n" t.server_master_id;
    lprintf "ip: %d\n" t.server_id;
    lprintf "tags: ";
    print_tags t.tags
      
   
  let write buf t =
      buf_md4 buf t.group_id;
      buf_int buf t.server_master_id;
      buf_int buf t.server_id;
      buf_tags buf t.tags names_of_tag
   
end

module ConnectRocky = struct
  type t = {   
    group_id : Md4.t;
    server_id : int;
    server_ip : Ip.t;
    server_port : int;
    server_tags : tag list;
  }


  let names_of_tag =
    [
      1, "name";
      11, "description";
    ]

  let parse len s =
    let group_id = get_md4 s 1 in
    let server_id = get_int s 17 in
    let server_ip = get_ip s 21 in
    let server_port = get_port s 25 in
    let ntags = get_int s 29 in 
    let tags, pos = get_tags s 33 ntags names_of_tag in
      {
        group_id = group_id;
        server_id = server_id;
        server_ip = server_ip; 
        server_port = server_port;
        server_tags = tags;
      }
     
  let print t =
    lprintf "SERVER ACCEPT IN GROUPE:\n";
    lprintf "Group_id: %s\n" (Md4.to_string t.group_id);
    lprintf "id: %d\n" t.server_id;
    lprintf "ip: %s\n" (Ip.to_string t.server_ip);
    lprintf "port: %d\n" t.server_port;
    lprintf "tags: ";
    print_tags t.server_tags
      
   
  let write buf t =
      buf_md4 buf t.group_id;
      buf_int buf t.server_id;
      buf_ip buf t.server_ip;
      buf_port buf t.server_port;
      buf_tags buf t.server_tags names_of_tag
   
end


module ConnectByGroup = struct
  type t = {   
    group_id : Md4.t;
    server_id : int;
    server_ip : Ip.t;
    server_port : int;
  }


 
  let parse len s =
    let group_id = get_md4 s 1 in
    let server_id = get_int s 17 in
    let server_ip = get_ip s 21 in
    let server_port = get_port s 25 in 
      {
        group_id = group_id;
        server_id = server_id;
        server_ip = server_ip; 
        server_port = server_port;
      }
     
  let print t =
    lprintf "ConnectByGroup:\n";
    lprintf "Group_id: %s\n" (Md4.to_string t.group_id);
    lprintf "id: %d\n" t.server_id;
    lprintf "ip: %s\n" (Ip.to_string t.server_ip);
    lprintf "port: %d\n" t.server_port
   
      
   
  let write buf t =
      buf_md4 buf t.group_id;
      buf_int buf t.server_id;
      buf_ip buf t.server_ip;
      buf_port buf t.server_port
   
end



module Recovery = struct
  type t = {   
    group_id : Md4.t;
    server_id : int;
    server_ip : Ip.t;
    server_port : int;
  }


 
  let parse len s =
    let group_id = get_md4 s 1 in
    let server_id = get_int s 17 in
    let server_ip = get_ip s 21 in
    let server_port = get_port s 25 in 
      {
        group_id = group_id;
        server_id = server_id;
        server_ip = server_ip; 
        server_port = server_port;
      }
     
  let print t =
    lprintf "RECOVERY PROTOCOL NEED:\n";
    lprintf "Group_id: %s\n" (Md4.to_string t.group_id);
    lprintf "id: %d\n" t.server_id;
    lprintf "ip: %s\n" (Ip.to_string t.server_ip);
    lprintf "port: %d\n" t.server_port
   
      
   
  let write buf t =
      buf_md4 buf t.group_id;
      buf_int buf t.server_id;
      buf_ip buf t.server_ip;
      buf_port buf t.server_port
   
end



module ServerNotification = struct
  type t = {
    group_id : Md4.t;
    server_id : int; 
    server_ip : Ip.t;
    server_port : int;
  }
      

  let parse len s =
    let group_id = get_md4 s 1 in
    let id = get_int s 17 in 
    let ip = get_ip s 21 in
    let port = get_port s 25 in 
      {
        group_id = group_id;
        server_id = id;
        server_ip = ip;
        server_port = port;
      }


  let print t =  
    lprintf "SERVER NOTIFICATION:\n";
    lprintf "From groupe: %s\n" (Md4.to_string t.group_id);
    lprintf "Server id: %d\n" t.server_id;
    lprintf "Server ip: %s\n" (Ip.to_string t.server_ip);
    lprintf "Server port: %d\n" t.server_port


   let write buf t = 
     buf_md4 buf t.group_id;
     buf_int buf t.server_id;
     buf_ip buf t.server_ip;
     buf_port buf t.server_port

end

module ServerSupp = struct 
  type t =  {
    group_id : Md4.t;
    server_id : int;
  }
      
  let parse len s =
    let group_id = get_md4 s 1 in
    let id = get_int s 17 in
      {
        group_id = group_id;
        server_id = id;
      }
      
  let print t =  
    lprintf "SERVER LEAVE THE GROUPE:\n";
    lprintf "From groupe: %s\n" (Md4.to_string t.group_id);
    lprintf "Server id: %d\n" t.server_id

  let write buf t = 
    buf_md4 buf t.group_id;
    buf_int buf t.server_id 

end

module LocalisationInit = struct
  type localisation = {
    source_ip : Ip.t;
    source_port : int; 
    }  

  type md4_sources = {
    md4 : Md4.t;
    sources : localisation list;
  }

  type t = md4_sources list
      

  let rec list_sources s pos nb_sources =
    if nb_sources = 0 then [] else
      let ip = get_ip s pos in
      let port = get_port s (pos+4) in
      let source = {
        source_ip = ip;
        source_port = port;
      } in
      let sources = list_sources s (pos+8) (nb_sources-1) in
        source :: sources



  let rec list_md4 s pos nb_md4 =
    if nb_md4 = 0 then [] else
      let md4 = get_md4 s pos in
      let nb_sources = get_int s (pos+16) in
      let sources = list_sources s (pos+20) nb_sources in
      let md4_loc = {
        md4 = md4;
        sources = sources;
      } in
      let md4_locs = list_md4 s (pos+20+nb_sources*8) (nb_md4-1) in
        md4_loc :: md4_locs

      

  let parse len s =
    let nb_md4 = get_int s 1 in
    let locs = list_md4 s 5 nb_md4 in
      locs
        
        
  
  let print t =  
    lprintf "Server Shared:\n";
    List.iter ( fun y ->
                  lprintf "File MD4: %s\n" (Md4.to_string y.md4);
                  List.iter ( fun x ->
                                lprintf "Source ip: %s\n" (Ip.to_string x.source_ip);
                                lprintf "Source port: %d\n" x.source_port
                            ) y.sources
              ) t


    let write buf t =
      buf_int buf (List.length t);
      List.iter ( fun y ->
                    buf_md4 buf y.md4;
                    buf_int buf (List.length y.sources);
                    List.iter ( fun x ->
                                  buf_ip buf x.source_ip;
                                  buf_port buf x.source_port
                              ) y.sources
                ) t

    end

module LocalisationNotif= struct
  type localisation = {
    add : bool;
    source_ip : Ip.t;
    source_port : int; 
    }  

  type md4_sources = {
    md4 : Md4.t;
    sources : localisation list;
  }

  type t = md4_sources list
      

  let rec list_sources s pos nb_sources =
    if nb_sources = 0 then [] else
      let add = GuiDecoding.get_bool s pos in
      let ip = get_ip s (pos+1) in
      let port = get_port s (pos+5) in
      let source = {
        add = add;
        source_ip = ip;
        source_port = port;
      } in
      let sources = list_sources s (pos+8) (nb_sources-1) in
        source :: sources



  let rec list_md4 s pos nb_md4 =
    if nb_md4 = 0 then [] else
      let md4 = get_md4 s pos in
      let nb_sources = get_int s (pos+16) in
      let sources = list_sources s (pos+20) nb_sources in
      let md4_loc = {
        md4 = md4;
        sources = sources;
      } in
      let md4_locs = list_md4 s (pos+20+nb_sources*9) (nb_md4-1) in
        md4_loc :: md4_locs

      

  let parse len s =
    let nb_md4 = get_int s 1 in
    let locs = list_md4 s 5 nb_md4 in
      locs
        
        
  
  let print t =  
    lprintf "Server Shared:\n";
    List.iter ( fun y ->
                  lprintf "File MD4: %s\n" (Md4.to_string y.md4);
                  List.iter ( fun x ->
                                lprintf "Source ip: %s\n" (Ip.to_string x.source_ip);
                                lprintf "Source port: %d\n" x.source_port
                            ) y.sources
              ) t


    let write buf t =
      buf_int buf (List.length t);
      List.iter ( fun y ->
                    buf_md4 buf y.md4;
                    buf_int buf (List.length y.sources);
                    List.iter ( fun x ->
                                  buf_ip buf x.source_ip;
                                  buf_port buf x.source_port
                              ) y.sources
                ) t

    end

module LocateNotif= struct
  type localisation = {
    add : bool;
    source_id : Ip.t;
    mutable source_ip : Ip.t;
    source_port : int; 
    
}  

  type t =  {
    message_id : int;
    nb_notifs : int;
    ack : int;
    notifs : (Md4.t, localisation list) Hashtbl.t;
  } 
      

  let rec list_sources s pos nb_sources =
    if nb_sources = 0 then [],pos else
      let add,pos = get_bool s pos in
        (*lprintf "add %s at pos %d \n" (string_of_bool add) pos;*)
      let id = get_ip s pos in
      let ip = get_ip s (pos+4) in
        (*lprintf "at  %s" (Ip.to_string ip);*)
      let port = get_port s (pos+8) in
        (*lprintf "port  %d\n" port;*)
      let source = {
        add = add;
        source_id = id;
        source_ip = ip;
        source_port = port;
      } in
      let sources,pos = list_sources s (pos+10) (nb_sources-1) in
        (*lprintf "at pos %d \n" pos;*)
        (source :: sources),pos




  let rec list_md4 htbl s pos nb_md4 =
      if nb_md4 = 0 then htbl else
        let md4 = get_md4 s pos in
          (*lprintf "get %s\n" (Md4.to_string md4);*)
        let nb_sources = get_int s (pos+16) in
          (*lprintf "nbsources %d\n"  nb_sources;*)
        let sources,pos = list_sources s (pos+20) nb_sources in
          Hashtbl.add htbl md4 sources;
          (list_md4 htbl s pos (nb_md4-1))

      

  let parse len s =
    try
      let message_id = get_int s 1 in
      let nb_md4 = get_int s 5 in
      let ack = get_int s 9 in
      let notifs = Hashtbl.create nb_md4 in 
      let notifs = list_md4 notifs s 13 nb_md4 in
      (*let count = ref 0 in
        Hashtbl.iter (fun md4 sources ->
                        count := !count + (List.length sources)
                     ) notifs;
        lprintf "During Parsing %d notifs\n" !count;*)
        {
          message_id = message_id;
          nb_notifs = nb_md4;
          ack = ack;
          notifs = notifs;
        }
    with _ ->
      lprintf "Parsing PB\n";
      raise Not_found

  
  let print t =  
    lprintf "SERVER SHARED NOTIFICATION %d : %d NOTIFS\n" t.message_id t.nb_notifs;
     lprintf "Ack for : %d\n" t.ack;
    Hashtbl.iter ( fun id notifs ->
                     lprintf "File MD4: %s\n" (Md4.to_string id);
                     List.iter ( fun x ->
                                   if x.add then
                                     lprintf "ADD\n"
                                   else
                                     lprintf "SUPP\n";
                                   lprintf "Source ip: %s\n" (Ip.to_string x.source_ip);
                                   lprintf "Source port: %d\n" x.source_port
                               ) notifs
                 ) t.notifs
      
      
  let write buf t =
    buf_int buf t.message_id;
    buf_int buf t.nb_notifs;
    buf_int buf t.ack;
    Hashtbl.iter ( fun id notif ->
                    buf_md4 buf id;
                    buf_int buf (List.length notif);
                    List.iter ( fun x ->
                                  buf_bool buf x.add;
                                  buf_ip buf x.source_id;
                                  buf_ip buf x.source_ip;
                                  buf_port buf x.source_port
                              ) notif
                ) t.notifs

end

module AckNotif = struct
  type t = int

  let parse len s =
    (get_int s 1) 

  let print t =
    lprintf "ACK %d NOTIF PACKET\n" t
  
  let write buf t =
    buf_int buf t

end




module AddSource = struct
    type t = {
      md4 : Md4.t;
      source_ip : Ip.t;
      source_port : int; 
    }  

    let parse len s = 
      let md4 = get_md4 s 1 in
      let ip = get_ip s 17 in
      let port = get_port s 21 in
      {
        md4 = md4;
        source_ip = ip;
        source_port = port;
      }
 
    let print t =  
      lprintf "ADD A SOURCE:\n";
      lprintf "File MD4: %s\n" (Md4.to_string t.md4);
      lprintf "Source ip: %s\n" (Ip.to_string t.source_ip);
      lprintf "Source port: %d\n" t.source_port


    let write buf t =
      buf_md4 buf t.md4; 
      buf_ip buf t.source_ip;
      buf_port buf t.source_port

    end

module SuppSource = struct
    type t = {
        md4 : Md4.t;
        source_ip : Ip.t;
        source_port : int;
    }

    let parse len s =  
      let md4 = get_md4 s 1 in
      let ip = get_ip s 17 in
      let port = get_port s 21 in
      {
        md4 = md4;
        source_ip = ip;
        source_port = port;
      }
      
    let print t =  
      lprintf "SUPP A SOURCE:\n";
      lprintf "File MD4: %s\n" (Md4.to_string t.md4);
      lprintf "Source ip: %s\n" (Ip.to_string t.source_ip);
      lprintf "Source port: %d\n" t.source_port

    let write buf t = 
      buf_md4 buf t.md4; 
      buf_ip buf t.source_ip;
      buf_port buf t.source_port

    end

module QueryFileInfo = struct
   type t = Md4.t list

   let rec get_list_md4 size s len =
      if size = 0 then 
        []
      else 
        (get_md4 s len) :: get_list_md4 (size-1) s (len+16) 
      

   let parse len s =
      let nb_md4 = get_int s 1 in
      get_list_md4 nb_md4 s 5 

    let print t =
    lprintf "List of Md4:";
    List.iter (fun md4 ->
        lprintf "%s\n" (Md4.to_string md4)
              ) t

     let write buf t =
        buf_int buf (List.length t);
        List.iter (fun md4 -> 
                   buf_md4 buf md4
                  ) t

end


module QueryFileInfoReply = struct

   type info = {
       md4 : Md4.t;
       tags : tag list;
        }

   type t = info list

   let names_of_tag =
   [
        1, "filename";
        2, "size";
        3, "type";
        4, "format";
      ]

   let rec get_list_info size s pos =
      if size = 0 then 
        []
      else 
        begin
           let md4 = get_md4 s pos in
           let ntags = get_int s (pos+16) in
           let tags, pos = get_tags s (pos+20) ntags names_of_tag in
        { 
          md4 = md4;
          tags = tags;
        } :: (get_list_info (size-1) s pos)
        end 
      

   let parse len s =
      let nb_md4 = get_int s 1 in
      get_list_info nb_md4 s 5 

    let print t =
    lprintf "List of Md4:";
    List.iter (fun i ->
        lprintf "%s\n" (Md4.to_string i.md4);
        print_tags i.tags 
              ) t

     let write buf t =
        buf_int buf (List.length t);
        List.iter (fun i -> 
                   buf_md4 buf i.md4;
                   buf_tags buf i.tags names_of_tag;
                  ) t

end


(*equals to QueryCallUDP*)
module QueryUserConnect = struct 
    type t = {
       local_client_id : Ip.t;
       client_ip : Ip.t;
       client_port : int;
    }


 let parse len s =  
      let id = get_ip s 1 in
      let ip = get_ip s 5 in
      let port = get_port s 9 in
      {
        local_client_id = id;
        client_ip = ip;
        client_port = port;
      }
      
    let print t =  
      lprintf "CONNECT A REMOTE FIREWALLED CLIENT:\n";
      lprintf "Local id: %s\n" (Ip.to_string t.local_client_id);
      lprintf "Source ip: %s\n" (Ip.to_string t.client_ip);
      lprintf "Source port: %d\n" t.client_port

    let write buf t = 
      buf_ip buf t.local_client_id; 
      buf_ip buf t.client_ip;
      buf_port buf t.client_port


end 

module Message = struct 
    type t = string
    
    let parse len s = 
      let v, pos = get_string s 1 in
      v
    
    let print t = 
      lprintf "MESSAGE:\n";
      lprintf "message = \"%s\"" (String.escaped t)
    
    let fprint oc t = 
      Printf.fprintf oc "MESSAGE:\n";
      Printf.fprintf oc "%s\n" (String.escaped t)

    let write buf t =
      buf_string buf t
  end


module Req  = struct
    type t

    let parse len s = raise Not_found
    let print t = raise Not_found
    let write buf s = raise Not_found
  end


type t = 
| ServerConnectReq of ServerConnect.t
| ACKConnectReq of ACKConnect.t
(*| ConnectRockyReq of ConnectRocky.t*) 
| ConnectByGroupReq of ConnectByGroup.t
| RecoveryReq of Recovery.t
| ServerNotificationReq of ServerNotification.t
| ServerSuppReq of ServerSupp.t
(*| AddSourceReq of AddSource.t
| SuppSourceReq of SuppSource.t
| LocalisationInitReq of LocalisationInit.t
| LocalisationNotifReq of LocalisationNotif.t*)
| LocateNotifReq of LocateNotif.t
| AckNotifReq of AckNotif.t
| QueryUserConnectReq of QueryUserConnect.t
| QueryFileInfoReq of QueryFileInfo.t
| QueryFileInfoReplyReq of QueryFileInfoReply.t 
| UnKnownReq of string
| MessageReq of Message.t
| QuitReq

let parse magic s =
  try
    let len = String.length s in
    if len = 0 then raise Not_found;
    let opcode = int_of_char (s.[0]) in
(*    lprintf "opcode: %d" opcode; lprint_newline (); *)
    match opcode with
    | 1 -> ServerConnectReq (ServerConnect.parse len s)
    | 2 -> QuitReq
    | 3 -> ACKConnectReq (ACKConnect.parse len s)
    | 4 -> ConnectByGroupReq (ConnectByGroup.parse len s)
    | 5 -> MessageReq (Message.parse len s)
    | 6 -> ServerSuppReq (ServerSupp.parse len s)
    | 10 -> ServerNotificationReq (ServerNotification.parse len s)
    (*| 12 -> ConnectRockyReq (ConnectRocky.parse len s)*)
    | 13 -> RecoveryReq (Recovery.parse len s)
    (*| 30 -> AddSourceReq (AddSource.parse len s)
    | 31 -> SuppSourceReq (SuppSource.parse len s)*)
    (*| 32 -> LocalisationInitReq (LocalisationInit.parse len s)
    | 33 -> LocalisationNotifReq (LocalisationNotif.parse len s)*)
    | 33 -> AckNotifReq (AckNotif.parse len s)
    | 34 -> LocateNotifReq (LocateNotif.parse len s)
    | 35 -> QueryFileInfoReq (QueryFileInfo.parse len s)
    | 36 -> QueryFileInfoReplyReq (QueryFileInfoReply.parse len s)
    | 50 -> QueryUserConnectReq (QueryUserConnect.parse len s)
    | _ -> raise Not_found
  with e ->
      lprintf "From server to server :"; lprint_newline ();
      dump s;
      UnKnownReq s


let print t =
  begin
    match t with
    | ServerConnectReq t -> ServerConnect.print t
    | QuitReq -> lprintf("Server Quit Relais Group\n");
    | ACKConnectReq t -> ACKConnect.print t
    | ConnectByGroupReq t -> ConnectByGroup.print t
    | ServerSuppReq t -> ServerSupp.print t
    | ServerNotificationReq t -> ServerNotification.print t
    (*| ConnectRockyReq t -> ConnectRocky.print t*)
    | RecoveryReq t -> Recovery.print t
    (*| AddSourceReq t -> AddSource.print t
    | SuppSourceReq t -> SuppSource.print t
    | LocalisationInitReq t -> LocalisationInit.print t
    | LocalisationNotifReq t -> LocalisationNotif.print t*)
    | LocateNotifReq t -> LocateNotif.print t
    | AckNotifReq t -> AckNotif.print t
    | QueryFileInfoReq t -> QueryFileInfo.print t
    | QueryFileInfoReplyReq t -> QueryFileInfoReply.print t 
    | QueryUserConnectReq t -> QueryUserConnect.print t
    | UnKnownReq t-> DonkeyProtoServer.print (DonkeyProtoServer.UnknownReq t)
    | MessageReq t -> Message.print t
  end;
  lprint_newline()




let udp_write buf t =
  match t with
  | ServerConnectReq t ->
      buf_int8 buf 1;
      ServerConnect.write buf t
  | QuitReq ->
      buf_int8 buf 2
  | ACKConnectReq t -> 
      buf_int8 buf 3;
      ACKConnect.write buf t
  | ConnectByGroupReq t -> 
      buf_int8 buf 4;
      ConnectByGroup.write buf t
  | MessageReq t -> 
      buf_int8 buf 5;
      Message.write buf t
  | ServerSuppReq t -> 
      buf_int8 buf 6;
      ServerSupp.write buf t
  | ServerNotificationReq t ->
      buf_int8 buf 10;
      ServerNotification.write buf t 
  (*| ConnectRockyReq t -> 
      buf_int8 buf 12;
      ConnectRocky.write buf t*)
  | RecoveryReq t ->
      buf_int8 buf 13;
      Recovery.write buf t
  (*| LocalisationInitReq t ->
      buf_int8 buf 32;
      LocalisationInit.write buf t 
  | LocalisationNotifReq t ->
      buf_int8 buf 33;
      LocalisationNotif.write buf t*) 
  | AckNotifReq t ->
      buf_int8 buf 33;
      AckNotif.write buf t
  | LocateNotifReq t ->
      buf_int8 buf 34;
      LocateNotif.write buf t 
  (*| AddSourceReq t ->
      buf_int8 buf 30;
      AddSource.write buf t
  | SuppSourceReq t ->
      buf_int8 buf 31;
      SuppSource.write buf t*)
  | QueryFileInfoReq t ->
      buf_int8 buf 35;
      QueryFileInfo.write buf t
  | QueryFileInfoReplyReq t ->
      buf_int8 buf 36;
      QueryFileInfoReply.write buf t
  | QueryUserConnectReq t ->
      buf_int8 buf 50;
      QueryUserConnect.write buf t
  | _ -> ()
      
let write buf t = 
  udp_write buf t


type group_sock = TcpBufferedSocket.t

let buf = TcpBufferedSocket.internal_buf

let group_msg_to_string msg =
  Buffer.reset buf;
  buf_int8 buf 227;
  buf_int buf 0;
  write buf msg;
  let s = Buffer.contents buf in
  let len = String.length s - 5 in
  str_int s 1 len;
  s

let group_send sock m =
  write_string sock (group_msg_to_string m)

let group_msg msg = msg

let direct_group_send s msg =
  group_send s (msg)


