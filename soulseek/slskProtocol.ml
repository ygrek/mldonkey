(*
Download the server list from:
  
http://www.soulseek.org/slskinfo

135
www.soulseek.org/slsk135.exe
135: THIS UPDATE IS NECESSARY TO BE ABLE TO LOG ON! Fixed faulty firewall detection that could cause undue browsing and file transfer difficulties. Changed protocol to conserve bandwidth. Fixed transfer looping bug.
--servers
Soulseek Classic - Electronic music:for the sharing of electronic music:sk.nikita.cx:2240
Test Server:Offline 99% of the time:sk.nikita.cx:2242


  
 for listenport in range(2234,2240):

""" Server and peers send each other small binary messages, that start
    with length and message code followed by the actual messsage data. 
    These are the codes."""
    servercodes = {Login:1,SetWaitPort:2,
                   GetPeerAddress:3,AddUser:5,GetUserStatus:7,SayChatroom:13,
                   JoinRoom:14,LeaveRoom:15,UserJoinedRoom:16,UserLeftRoom:17,
                   ConnectToPeer:18,MessageUser:22,MessageAcked:23,
                   FileSearch:26,GetUserStats:36,QueuedDownloads:40,
                   PlaceInLineResponse:60,RoomAdded:62,RoomRemoved:63,
                   RoomList:64,ExactFileSearch:65,AdminMessage:66, 
                   GlobalUserList:67,TunneledMessage:68,PrivilegedUsers:69,
                   CantConnectToPeer:1001}
    peercodes = {GetSharedFileList:4, SharedFileList:5, FileSearchResult:9,
                UserInfoRequest:15,UserInfoReply:16, FolderContentsRequest:36,
                FolderContentsResponse:37, TransferRequest:40,
                TransferResponse:41,TransferSomething:42,PlaceInLine:44,
                TransferSomething2:46}


  Connection a un serveur:
slskmessages.Login(self.frame.config.sections["server"]["login"],self.frame.config.sections["server"]["passw"],200)

slskmessages.SetWaitPort(self.frame.waitport)

Apres reception d'un message prive:
slskmessages.MessageAcked(msg.msgid)


GET http://www.soulseek.org/slskinfo2

REPLY:
www.soulseek.org/slsk135.exe(13)(10)
135: Fixed faulty firewall detection that could cause undue browsing and file transferdifficulties. Changed protocol to conserve bandwidth. Fixed transfer looping bug.(13)(10)
--servers(13)(10)
Soulseek Classic - Electronic music:for the sharing of electronic music:www.soulseek.org:2242

GET http://www.soulseek.org//login.html

REPLY:
127(13)(10)
  
=== GETUSERSTATS === To server
[36]{
string : NICK
}
  *)

open TcpBufferedSocket
open LittleEndian
open SlskTypes
open CommonTypes
open CommonGlobals

let get_ip s pos = Ip.rev (get_ip s pos)
let buf_ip buf ip = buf_ip buf (Ip.rev ip)

  
let get_string s pos =
  let len = get_int s pos in
  String.sub s (pos+4) len, pos+4+len
  
let buf_string buf s =
  buf_int buf (String.length s);
  Buffer.add_string buf s

module C2S = struct
    
    module Login = struct
        type t = {
            login : string;
            password: string;
            version: int;
          }
          
        let parse s = 
          let login, pos = get_string s 0 in
          let password, pos = get_string s pos in
          let version = get_int s pos in
          { login = login; password = password; version = version; }
        
        let print t =
          Printf.printf "LOGIN login:%s password:%s version:%d" 
            t.login t.password t.version;
          print_newline () 
          
        let write buf t =
          buf_string buf t.login;
          buf_string buf t.password;
          buf_int buf t.version
          
      end
    
    type t = 
    | LoginReq of Login.t
    | GetUserStatsReq of string
    | UnknownReq of string
    
    let parse opcode s =
      try
        match opcode with
          1 -> LoginReq (Login.parse s)
        | 36 -> 
            let user, _ = get_string s 0 in
            GetUserStatsReq user
        | _ -> raise Not_found
      with
        e -> 
          Printf.printf "From client:"; print_newline ();
          dump s;
          UnknownReq s
    
    let print t =
      match t with
        LoginReq t -> Login.print t  
      | GetUserStatsReq t -> 
          Printf.printf "GetUserStats %s" t; print_newline () 
      | UnknownReq s -> Printf.printf "%s" s
          
    let write buf t =
      match t with
        LoginReq t -> buf_int buf 1; Login.write buf t  
      | GetUserStatsReq t -> buf_int buf 36; buf_string buf t
      | UnknownReq s -> Buffer.add_string buf s
      
  end
  
module S2C = struct
    
    module LoginAck = struct
        type t = 
          Success of string * Ip.t
        | Failure of string

        let parse s = 
          match int_of_char s.[0] with
            1 -> 
              let message,pos = get_string s 1 in
              let ip = get_ip s pos in
              Success (message, ip)
          | _ -> 
              let reason,pos = get_string s 1 in
              Failure reason
        
        let print t =
          match t with
            Success (message, ip) ->
              Printf.printf "LOGIN ACK: %s" message;
              print_newline ();
              Printf.printf "   IP: %s" (Ip.to_string ip);
              print_newline ()
          | Failure reason ->
              Printf.printf "LOGIN FAILURE %s" reason;
              print_newline () 
              
        let write buf t =
          match t with
            Success (message, ip) ->
              buf_int8 buf 1;
              buf_string buf message;
              buf_ip buf ip
          | Failure reason -> 
              assert false; (* NOT SURE OF THIS PACKET *)
              buf_string buf reason
          
      end

    module RoomList = struct
        type t = {
            nrooms : int;
            room_names : string array;
            room_nusers : int array;
          }

        let parse s =
          let nrooms = get_int s 0 in
          let room_names = Array.create nrooms "" in
          let room_nusers = Array.create nrooms 0 in
          let rec iter_names nleft pos =
            if nleft = 0 then pos else
            let name, pos = get_string s pos in
            room_names.(nrooms - nleft) <- name;
            iter_names (nleft - 1) pos
          in
          let pos = iter_names nrooms 4 in
          let rec iter_nusers nleft pos =
            if nleft = 0 then pos else
            let nusers = get_int s pos in
            room_nusers.(nrooms - nleft) <- nusers;
            iter_nusers (nleft - 1) (pos+4)
          in
          let pos = iter_nusers nrooms 4 in
          { nrooms = nrooms; room_names = room_names; 
            room_nusers = room_nusers; }
          
        let print t =
          Printf.printf "Room list: %d rooms" t.nrooms;
          print_newline ();
          for i = 0 to t.nrooms - 1 do
            Printf.printf "    %50s  %-10d" t.room_names.(i) t.room_nusers.(i);
            print_newline () 
          done
          
        let write buf t =
          buf_int buf t.nrooms;
          Array.iter (buf_string buf) t.room_names;
          Array.iter (buf_int buf) t.room_nusers
      end
    
    type t = 
    | LoginAckReq of LoginAck.t
    | RoomListReq of RoomList.t
    | UnknownReq of string
    
    let parse opcode s =
      try
        match opcode with
          1 -> LoginAckReq (LoginAck.parse s)
        | 64 -> RoomListReq (RoomList.parse s)
        | _ -> raise Not_found
      with
        e -> 
          Printf.printf "From client:"; print_newline ();
          dump s;
          UnknownReq s
    
    let print t =
      match t with
        LoginAckReq t -> LoginAck.print t  
      | RoomListReq t -> RoomList.print t
      | UnknownReq s -> Printf.printf "%s" s
          
    let write buf t =
      match t with
        LoginAckReq t -> buf_int buf 1; LoginAck.write buf t  
      | RoomListReq t -> buf_int buf 64; RoomList.write buf t
      | UnknownReq s -> Buffer.add_string buf s
      
  end
  
module C2C = struct
    
    module Login = struct
        type t = {
            login : string;
            password: string;
            version: int;
          }
        
        let parse s = 
          failwith "Login.parse not implemented"
        
        let print t =
          Printf.printf "LOGIN login:%s password:%s version:%d" 
            t.login t.password t.version;
          print_newline () 
          
        let write buf t =
          failwith "Login.write not implemented"          
          
      end
    
    type t = 
    | LoginReq of Login.t
    | UnknownReq of string
    
    let parse opcode s =
      try
        match opcode with
          1 -> LoginReq (Login.parse s)
        | _ -> raise Not_found
      with
        e -> 
          Printf.printf "From client:"; print_newline ();
          dump s;
          UnknownReq s
    
    let print t =
      match t with
        LoginReq t -> Login.print t  
      | UnknownReq s -> Printf.printf "%s" s
          
    let write buf t =
      match t with
        LoginReq t -> buf_int buf 1; Login.write buf t  
      | UnknownReq s -> Buffer.add_string buf s
      
  end
  
let soulseek_handler parse f sock nread =
  let b = TcpBufferedSocket.buf sock in
  try
    while b.len >= 4 do
      let msg_len = get_int b.buf b.pos in
      if b.len >= 4 + msg_len then
        begin
          let opcode = get_int b.buf (b.pos+4) in
          let data = String.sub b.buf (b.pos+8) (msg_len-4) in
          TcpBufferedSocket.buf_used sock (msg_len + 4);
          let t = parse opcode data in
(*          print t; *)
          f t sock
        end
      else raise Not_found
    done
  with Not_found -> ()
