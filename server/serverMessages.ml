(* declaration des differents types de messages entre les servers*)

open BasicSocket
open TcpBufferedSocket

open Gui_proto
open BigEndian
open CommonTypes
open CommonGlobals
open DonkeyMftp


module M = Mftp_server

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
(*      Printf.printf "port: %d" port; print_newline (); *)
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
      Printf.printf "SERVER CONNECT:\n";
      Printf.printf "MD4: %s\n" (Md4.to_string t.md4);
      Printf.printf "ip: %s\n" (Ip.to_string t.ip);
      Printf.printf "port: %d\n" t.port;
      Printf.printf "max_clients: %d\n" t.max_clients;
      Printf.printf "max_files: %d\n" t.max_files;
      Printf.printf "tags: ";
      print_tags t.tags

    let write buf t =
      buf_md4 buf t.md4;
      buf_ip buf t.ip;
      buf_port buf t.port;
      buf_int buf t.max_clients;
      buf_int buf t.max_files;
      buf_int buf (List.length t.tags);
      buf_tags buf t.tags names_of_tag

end

module ACKConnect = struct 
  type t = {
    group_id : Md4.t;
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
    let server_id = get_int s 17 in
    let ntags = get_int s 21 in 
    let tags, pos = get_tags s 25 ntags names_of_tag in
      {
	group_id = group_id;
	server_id = server_id;
	tags = tags;
      }
     
  let print t =
    Printf.printf "SERVER ACCEPT IN GROUPE:\n";
    Printf.printf "Group_id: %s\n" (Md4.to_string t.group_id);
    Printf.printf "ip: %d\n" t.server_id;
    Printf.printf "tags: ";
    print_tags t.tags
      
   
  let write buf t =
      buf_md4 buf t.group_id;
      buf_int buf t.server_id;
      buf_int buf (List.length t.tags);
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
    Printf.printf "SERVER ACCEPT IN GROUPE:\n";
    Printf.printf "Group_id: %s\n" (Md4.to_string t.group_id);
    Printf.printf "id: %d\n" t.server_id;
    Printf.printf "ip: %s\n" (Ip.to_string t.server_ip);
    Printf.printf "port: %d\n" t.server_port;
    Printf.printf "tags: ";
    print_tags t.server_tags
      
   
  let write buf t =
      buf_md4 buf t.group_id;
      buf_int buf t.server_id;
      buf_ip buf t.server_ip;
      buf_port buf t.server_port;
      buf_int buf (List.length t.server_tags);
      buf_tags buf t.server_tags names_of_tag
   
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
    Printf.printf "SERVER ACCEPT IN GROUPE:\n";
    Printf.printf "Group_id: %s\n" (Md4.to_string t.group_id);
    Printf.printf "id: %d\n" t.server_id;
    Printf.printf "ip: %s\n" (Ip.to_string t.server_ip);
    Printf.printf "port: %d\n" t.server_port
   
      
   
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
    Printf.printf "SERVER NOTIFICATION:\n";
    Printf.printf "From groupe: %s\n" (Md4.to_string t.group_id);
    Printf.printf "Server id: %d\n" t.server_id;
    Printf.printf "Server ip: %s\n" (Ip.to_string t.server_ip);
    Printf.printf "Server port: %d\n" t.server_port


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
    Printf.printf "SERVER LEAVE THE GROUPE:\n";
    Printf.printf "From groupe: %s\n" (Md4.to_string t.group_id);
    Printf.printf "Server id: %d\n" t.server_id

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
    Printf.printf "Server Shared:\n";
    List.iter ( fun y ->
		  Printf.printf "File MD4: %s\n" (Md4.to_string y.md4);
		  List.iter ( fun x ->
				Printf.printf "Source ip: %s\n" (Ip.to_string x.source_ip);
				Printf.printf "Source port: %d\n" x.source_port
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
      let add = Gui_proto.Decoding.get_bool s pos in
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
    Printf.printf "Server Shared:\n";
    List.iter ( fun y ->
		  Printf.printf "File MD4: %s\n" (Md4.to_string y.md4);
		  List.iter ( fun x ->
				Printf.printf "Source ip: %s\n" (Ip.to_string x.source_ip);
				Printf.printf "Source port: %d\n" x.source_port
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
    source_ip : Ip.t;
    source_port : int; 
    }  

  type t =  {
    nb_notifs : int;
    notifs : (Md4.t, localisation list) Hashtbl.t;
  } 
      

  let rec list_sources s pos nb_sources =
    if nb_sources = 0 then [],pos else
      let add,pos = get_bool s pos in
      let ip = get_ip s pos in
      let port = get_port s (pos+4) in
      let source = {
	add = add;
	source_ip = ip;
	source_port = port;
      } in
      let sources,pos = list_sources s (pos+8) (nb_sources-1) in
	(source :: sources),pos



  let rec list_md4 htbl s pos nb_md4 =
    if nb_md4 = 0 then htbl else
      let md4 = get_md4 s pos in
      let nb_sources = get_int s (pos+16) in
      let sources,pos = list_sources s (pos+20) nb_sources in
	Hashtbl.add htbl md4 sources;
	(list_md4 htbl s pos (nb_md4-1))

      

  let parse len s =
    let nb_md4 = get_int s 1 in
    let notifs = Hashtbl.create nb_md4 in 
      {
	nb_notifs = nb_md4;
	notifs = (list_md4 notifs s 5 nb_md4)
      }
     
	
	
  
  let print t =  
    Printf.printf "Server Shared:%d\n" t.nb_notifs;
    Hashtbl.iter ( fun id notifs ->
		     Printf.printf "File MD4: %s\n" (Md4.to_string id);
		     List.iter ( fun x ->
				   if x.add then
				     Printf.printf "ADD\n"
				   else
				     Printf.printf "SUPP\n";
				   Printf.printf "Source ip: %s\n" (Ip.to_string x.source_ip);
				   Printf.printf "Source port: %d\n" x.source_port
			       ) notifs
		 ) t.notifs
      
      
  let write buf t =
    buf_int buf t.nb_notifs;
    Hashtbl.iter ( fun id notif ->
		    buf_md4 buf id;
		    buf_int buf (List.length notif);
		    List.iter ( fun x ->
				  buf_bool buf x.add;
				  buf_ip buf x.source_ip;
				  buf_port buf x.source_port
			      ) notif
		) t.notifs

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
      Printf.printf "ADD A SOURCE:\n";
      Printf.printf "File MD4: %s\n" (Md4.to_string t.md4);
      Printf.printf "Source ip: %s\n" (Ip.to_string t.source_ip);
      Printf.printf "Source port: %d\n" t.source_port


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
      Printf.printf "SUPP A SOURCE:\n";
      Printf.printf "File MD4: %s\n" (Md4.to_string t.md4);
      Printf.printf "Source ip: %s\n" (Ip.to_string t.source_ip);
      Printf.printf "Source port: %d\n" t.source_port

    let write buf t = 
      buf_md4 buf t.md4; 
      buf_ip buf t.source_ip;
      buf_port buf t.source_port

    end

(*equals to QueryCallUDP*)
module QueryUserConnect = struct 
    type t = {
       md4 : Md4.t;
       local_client_id : Ip.t;
       client_ip : Ip.t;
       client_port : int;
    }


 let parse len s =  
      let md4 = get_md4 s 1 in
      let id = get_ip s 17 in
      let ip = get_ip s 21 in
      let port = get_port s 25 in
      {
        md4 = md4;
        local_client_id = id;
        client_ip = ip;
        client_port = port;
      }
      
    let print t =  
      Printf.printf "CONNECT A REMOTE FIREWALLED CLIENT:\n";
      Printf.printf "File MD4: %s\n" (Md4.to_string t.md4);
      Printf.printf "Local id: %s\n" (Ip.to_string t.local_client_id);
      Printf.printf "Source ip: %s\n" (Ip.to_string t.client_ip);
      Printf.printf "Source port: %d\n" t.client_port

    let write buf t = 
      buf_md4 buf t.md4;
      buf_ip buf t.local_client_id; 
      buf_ip buf t.client_ip;
      buf_port buf t.client_port


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
| ConnectRockyReq of ConnectRocky.t 
| RecoveryReq of Recovery.t
| ServerNotificationReq of ServerNotification.t
| ServerSuppReq of ServerSupp.t
| AddSourceReq of AddSource.t
| SuppSourceReq of SuppSource.t
| LocalisationInitReq of LocalisationInit.t
| LocalisationNotifReq of LocalisationNotif.t
| LocateNotifReq of LocateNotif.t
| QueryUserConnectReq of QueryUserConnect.t
| UnKnownReq of string
| QuitReq

let parse s =
  try
    let len = String.length s in
    if len = 0 then raise Not_found;
    let opcode = int_of_char (s.[0]) in
(*    Printf.printf "opcode: %d" opcode; print_newline (); *)
    match opcode with
    | 1 -> ServerConnectReq (ServerConnect.parse len s)
    | 2 -> QuitReq
    | 3 -> ACKConnectReq (ACKConnect.parse len s)
    | 10 -> ServerNotificationReq (ServerNotification.parse len s)
    | 11 -> ServerSuppReq (ServerSupp.parse len s)
    | 12 -> ConnectRockyReq (ConnectRocky.parse len s)
    | 13 -> RecoveryReq (Recovery.parse len s)
    | 30 -> AddSourceReq (AddSource.parse len s)
    | 31 -> SuppSourceReq (SuppSource.parse len s)
    | 32 -> LocalisationInitReq (LocalisationInit.parse len s)
    | 33 -> LocalisationNotifReq (LocalisationNotif.parse len s)
    | 34 -> LocateNotifReq (LocateNotif.parse len s)
    | 50 -> QueryUserConnectReq (QueryUserConnect.parse len s)
    | _ -> raise Not_found
  with e ->
      Printf.printf "From server to server :"; print_newline ();
      dump s;
      UnKnownReq s


let print t =
  begin
    match t with
    | ServerConnectReq t -> ServerConnect.print t
    | QuitReq -> Printf.printf("Server Quit Relais Group\n");
    | ACKConnectReq t -> ACKConnect.print t
    | ServerNotificationReq t -> ServerNotification.print t
    | ServerSuppReq t -> ServerSupp.print t
    | ConnectRockyReq t -> ConnectRocky.print t
    | RecoveryReq t -> Recovery.print t
    | AddSourceReq t -> AddSource.print t
    | SuppSourceReq t -> SuppSource.print t
    | LocalisationInitReq t -> LocalisationInit.print t
    | LocalisationNotifReq t -> LocalisationNotif.print t
    | LocateNotifReq t -> LocateNotif.print t
    | QueryUserConnectReq t -> QueryUserConnect.print t
    | UnKnownReq t-> Mftp_server.print (Mftp_server.UnknownReq t)
  end;
  print_newline()




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
  | ServerNotificationReq t ->
      buf_int8 buf 10;
      ServerNotification.write buf t 
  | ServerSuppReq t ->
      buf_int8 buf 11;
      ServerSupp.write buf t
  | ConnectRockyReq t -> 
      buf_int8 buf 12;
      ConnectRocky.write buf t
  | RecoveryReq t ->
      buf_int8 buf 13;
      Recovery.write buf t
  | LocalisationInitReq t ->
      buf_int8 buf 32;
      LocalisationInit.write buf t 
  | LocalisationNotifReq t ->
      buf_int8 buf 33;
      LocalisationNotif.write buf t 
  | LocateNotifReq t ->
      buf_int8 buf 34;
      LocateNotif.write buf t 
  | AddSourceReq t ->
      buf_int8 buf 30;
      AddSource.write buf t
  | SuppSourceReq t ->
      buf_int8 buf 31;
      SuppSource.write buf t
  | QueryUserConnectReq t ->
      buf_int8 buf 50;
      QueryUserConnect.write buf t
  | _ -> ()
      
let write buf t = 
  udp_write buf t


type group_sock = TcpBufferedSocket.t

let buf = TcpBufferedSocket.internal_buf

let group_msg_to_string msg =
  Buffer.clear buf;
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


