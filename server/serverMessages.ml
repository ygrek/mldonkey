(* declaration des differents types de messages entre les servers*)


open BigEndian
open CommonTypes
open CommonGlobals
open DonkeyMftp


module M = Mftp_server

module ServerConnect = struct
    type t = {
        md4 : Md4.t;
        ip: Ip.t;
        port: int;
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
      let ntags = get_int s 23 in
      let tags, pos = get_tags s 27 ntags names_of_tag in
      {
        md4 = md4;
        ip = ip;
        port = port;
        tags = tags;
      }

    let print t =
      Printf.printf "SERVER CONNECT:\n";
      Printf.printf "MD4: %s\n" (Md4.to_string t.md4);
      Printf.printf "ip: %s\n" (Ip.to_string t.ip);
      Printf.printf "port: %d\n" t.port;
      Printf.printf "tags: ";
      print_tags t.tags

    let write buf t =
      buf_md4 buf t.md4;
      buf_ip buf t.ip;
      buf_port buf t.port;
      buf_int buf (List.length t.tags);
      buf_tags buf t.tags names_of_tag

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


    let write buf s =
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

    let write buf s = 
      buf_md4 buf t.md4; 
      buf_ip buf t.source_ip;
      buf_port buf t.source_port

    end

module QueryUserConnect = struct 
    type t = {
       md4 : Md4.t;
       local_id : Ip.t;
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
        local_id = id;
        client_ip = ip;
        client_port = port;
      }
      
    let print t =  
      Printf.printf "CONNECT A REMOTE FIREWALLED CLIENT:\n";
      Printf.printf "File MD4: %s\n" (Md4.to_string t.md4);
      Printf.printf "Local id: %s\n" (Ip.to_string t.local_id)
      Printf.printf "Source ip: %s\n" (Ip.to_string t.client_ip);
      Printf.printf "Source port: %d\n" t.client_port

    let write buf s = 
      buf_md4 buf t.md4;
      buf_ip buf t.local_id 
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
| AddSourceReq of AddSource.t
| SuppSourceReq of SuppSource.t
| QueryUserConnectReq of QueryUserConnect.t
| QuitReq


let parse s =
  try
    let len = String.length s in
    if len = 0 then raise Not_found;
    let opcode = int_of_char (s.[0]) in
(*    Printf.printf "opcode: %d" opcode; print_newline (); *)
    match opcode with
    | 1 -> ServerConnectReq (ServerConnect.parse len s)
    | 20 -> QuitReq
    | 30 -> AddSourceReq (AddSource.parse len s)
    | 40 -> SuppSourceReq (SuppSource.parse len s)
    | 50 -> QueryUserConnectReq (QueryUserConnect.parse len s)
    | _ -> raise Not_found
  with e ->
      Printf.printf "From server to server :"; print_newline ();
      dump s;
      UnknownReq s


let print t =
  begin
    match t with
    | ServerConnectReq -> ServerConnect.print t
    | QuitReq -> Printf.printf("Server Quit Relais Group");print_newline () 
    | AddSourceReq -> AddSource.print t
    | SuppSourceReq -> SuppSource.print t
    | QueryUserConnectReq -> QueryUserConnect.print t
  end 
  print_newline()




let udp_write buf t =
  match t with
  | ServerConnectReq t ->
      buf_int8 buf 1;
      Connect.write buf t
  | QuitReq ->
      buf_int8 buf 20
  | AddSourceReq ->
      buf_int8 buf 30;
      Connect.write buf t
  | SuppSourceReq ->
      buf_int8 buf 40;
      Connect.write buf t
  | QueryUserConnectReq ->
      buf_int8 buf 50;
      Connect.write buf t

let write buf t = 
  udp_write buf t
