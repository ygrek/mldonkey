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

open AgTypes

open TcpBufferedSocket

module AG = AgGlobals
  
let str_int16 s pos i =
  s.[pos+1] <- char_of_int (i land 255);
  s.[pos] <- char_of_int ((i lsr 8) land 255)

let str_int32 s pos i =
  str_int16 s pos ((i lsr 16) land 65535);
  str_int16 s (pos+2) (i land 65535)

(* These are BIG endian representation. Different from Mftp. Maybe put them
in another module *)

let get_int8 s pos = 
  int_of_char s.[pos]

let get_int16 s pos =
  let c1 = int_of_char s.[pos+1] in
  let c2 = int_of_char s.[pos] in
  c1 + c2 * 256

let get_int32 s pos =
  ((get_int16 s pos) lsl 16) lor (get_int16 s (pos+2))

  
let get_string s pos =
  let len = get_int16 s pos in
  try
    String.sub s (pos+2) len, pos+2+len
  with e ->
      Printf.printf "exception in get_string %d(%d)" pos len; print_newline ();
      raise e
  
let buf_int8 buf i =
  Buffer.add_char buf (char_of_int (i land 255))
  
let buf_int16 buf i =
  let i = i land 65535 in
  Buffer.add_char buf (char_of_int (i lsr 8));
  Buffer.add_char buf (char_of_int (i land 255))
  
let buf_int32 buf i =
  buf_int16 buf ((i lsr 16) land 65535);
  buf_int16 buf (i land 65535)

  
let buf_string buf s =
  buf_int16 buf (String.length s);
  Buffer.add_string buf s

let buf_md4 buf m = buf_string buf (Md4.direct_to_string m)
  
open Int32ops
  
  let get_int32_8 s pos =
  Int32.of_int (int_of_char s.[pos])

let get_int32_32 s pos = 
  let c4 = get_int32_8 s pos in
  let c3 = get_int32_8 s (pos+1) in
  let c2 = get_int32_8 s (pos+2) in
  let c1 = get_int32_8 s (pos+3) in
  c1 +. (left32 c2 8) +. (left32 c3 16) +. (left32 c4 24)           

let const_int32_255 = Int32.of_int 255  
let buf_int32_8 buf i =
  Buffer.add_char buf (char_of_int (Int32.to_int (
        Int32.logand i const_int32_255)))

let buf_int32_32 oc i =
  buf_int32_8 oc (right32 i  24);
  buf_int32_8 oc (right32 i  16);
  buf_int32_8 oc (right32 i  8);
  buf_int32_8 oc i

let buf_ip buf ip =
  buf_string buf (Ip.to_string ip)
  
let get_ip s pos =
  let ip_s, pos = get_string s pos in
  Ip.of_string ip_s, pos
  
let dump s =
  let len = String.length s in
  Printf.printf "ascii: [";
  for i = 0 to len - 1 do
    let c = s.[i] in
    let n = int_of_char c in
    if n > 31 && n < 127 then
      Printf.printf " %c" c
    else
      Printf.printf "(%d)" n
  done;
  Printf.printf "]\n";
  Printf.printf "dec: [";
  for i = 0 to len - 1 do
    let c = s.[i] in
    let n = int_of_char c in
    Printf.printf "(%d)" n            
  done;
  Printf.printf "]\n"

(************************************************************************)
  
let exit_exn = Exit
  
let string_of_fts fts =  
  match fts with
    FTS_DOWNLOAD_COMPLETE -> "FTS_DOWNLOAD_COMPLETE"
  | FTS_INTERNAL_ERROR -> "FTS_INTERNAL_ERROR"
  | FTS_TIMEOUT -> "FTS_TIMEOUT"
  | FTS_SETTING_UP -> "FTS_SETTING_UP"
  | FTS_CONNECTION_ERROR -> "FTS_CONNECTION_ERROR"
  | FTS_CONNECTION_CLOSED -> "FTS_CONNECTION_CLOSED"
  | FTS_PORT_NOT_AVAILABLE -> "FTS_PORT_NOT_AVAILABLE"
  | FTS_STOPPED -> "FTS_STOPPED"
  | FTS_RESET_CONNECTION -> "FTS_RESET_CONNECTION"
  
let fts_of_int i =
  match i with
    0 -> FTS_DOWNLOAD_COMPLETE
  | 1 -> FTS_INTERNAL_ERROR
  | 2 -> FTS_TIMEOUT
  | 3 -> FTS_SETTING_UP
  | 4 -> FTS_CONNECTION_ERROR
  | 5 -> FTS_CONNECTION_CLOSED
  | 7 -> FTS_PORT_NOT_AVAILABLE
  | 8 -> FTS_STOPPED
  | 9 -> FTS_RESET_CONNECTION
  | _ -> assert false
  
let fts_of_int i =
  match i with
    0 -> FTS_DOWNLOAD_COMPLETE
  | 1 -> FTS_INTERNAL_ERROR
  | 2 -> FTS_TIMEOUT
  | 3 -> FTS_SETTING_UP
  | 4 -> FTS_CONNECTION_ERROR
  | 5 -> FTS_CONNECTION_CLOSED
  | 7 -> FTS_PORT_NOT_AVAILABLE
  | 8 -> FTS_STOPPED
  | 9 -> FTS_RESET_CONNECTION
  | _ -> assert false
  
let int_of_fts fts =
  match fts with
    FTS_DOWNLOAD_COMPLETE -> 0
  | FTS_INTERNAL_ERROR -> 1
  | FTS_TIMEOUT -> 2
  | FTS_SETTING_UP -> 3
  | FTS_CONNECTION_ERROR -> 4
  | FTS_CONNECTION_CLOSED -> 5
  | FTS_PORT_NOT_AVAILABLE -> 7
  | FTS_STOPPED -> 8
  | FTS_RESET_CONNECTION -> 9

  
module Empty = functor(M: sig val msg : string end) -> 
    struct
    
      let parse s = ()
      
      let print t =
        Printf.printf "message %s" M.msg
        
      let write buf t = ()
    end
  
module SendSharesStandBy = Empty (struct let msg = "SendSharesStandBy" end)  
module SendShares = Empty (struct let msg = "SendShares" end)  
module ReadyToSendShares = Empty (struct let msg = "ReadyToSendShares" end)  
module ReadyForTransfer = Empty (struct let msg = "ReadyForTransfer" end)  
  
let get_md4 s pos =
  let s, pos = get_string s pos in
  Md4.direct_of_string s, pos
  
module FileTransfer = struct
    type t = {
        file_id : Md4.t;
        connect: connect_flag;
        direction : direction_flag;
        ip : Ip.t;
        port : int;
        local_id : int32;
        filename : string;
        size : int32;
      }
      
    let parse s = 
      let file_id, pos = get_md4 s 0 in
      let connect = match get_int8 s pos with
          0 -> Listen | 1 -> Connect | _ -> raise Not_found
      in
      let direction = match get_int8 s (pos+1) with
          0 -> SendFile | 1 -> ReceiveFile | _ -> raise Not_found
      in
      let ip, pos = get_ip s (pos+2) in
      let port = get_int32 s pos in
      let local_id = get_int32_32 s (pos+4) in
      let filename, pos = get_string s (pos+8) in
      let size = get_int32_32 s pos in
      {
        file_id = file_id;
        connect = connect;
        direction = direction;
        ip = ip;
        port = port;
        local_id = local_id;
        filename = filename;
        size = size;
      }
      
    let print t = 
      Printf.printf "Transfer file: %s %s %s:%d %s %s of size %ld id %ld"
        (Md4.to_string t.file_id)
      (match t.connect with
          Connect -> "ConnectTo"
        | Listen -> "Listen for")
      (Ip.to_string t.ip)
      t.port
        (match t.direction with
          SendFile -> "To Send"
        | ReceiveFile -> "To receive")
      t.filename
        t.size
        t.local_id;
      print_newline () 
            
    let write buf t = 
      buf_md4 buf t.file_id;
      buf_int8 buf (match t.connect with Listen -> 0 | Connect -> 1);
      buf_int8 buf (match t.direction with SendFile -> 0 | ReceiveFile -> 1);
      buf_ip buf t.ip;
      buf_int32 buf t.port;
      buf_int32_32 buf t.local_id;
      buf_string buf t.filename;
      buf_int32_32 buf t.size
    
  end

module Login = struct
    type t = {
        name : string;
        password : string;
        version : string; (* 0.520L *)
        ip: Ip.t;
      }
      
    let parse s = 
      let name, pos = get_string s 4 in
      let password, pos = get_string s pos in
      let version, pos = get_string s pos in
      let ip, pos = get_ip s pos in
      {
        name = name;
        password = password;
        version = version;
        ip = ip;
      }
      
    let print t = 
      Printf.printf "LOGIN FROM %s [%s] v %s from %s"
        t.name t.password t.version (Ip.to_string t.ip);
      print_newline ()
      
    let write buf t = 
      buf_int32 buf 0;
      buf_string buf t.name;
      buf_string buf t.password;
      buf_string buf t.version;
      buf_ip buf t.ip;
      buf_int32 buf 0;
      buf_int16 buf 0
    
  end

module FileTransferState = struct
    type t = {
        file_id : Md4.t;
        file_state : file_transfer_state;
      }
      
    let parse s = 
      let file_id,pos = get_md4 s 0 in
      let file_state = get_int32 s pos in
      {
        file_id = file_id;
        file_state = fts_of_int file_state;
      }
      
    let print t = 
      Printf.printf "FILE STATE : %s %s" (Md4.to_string t.file_id)
        (string_of_fts t.file_state);
      print_newline () 
      
    let write buf t = 
      buf_md4 buf t.file_id;
      buf_int32 buf (int_of_fts t.file_state)
    
  end

module StopFileTransfer = struct
    type t = Md4.t
      
    let parse s = 
      let file_id,pos = get_md4 s 0 in
      file_id
      
    let print t = 
      Printf.printf "STOP FILE TRANSFER : %s" (Md4.to_string t);
      print_newline () 
      
    let write buf t = 
      buf_md4 buf t
    
  end

(*

NEW SHARE:
(69)(95)(208)(213)
(0)(0)(0)(125)
(0)(0)(0)(12)
  
(0)(2)
(0)(0)(0)(0) 				LOCAL ID OF FILE
(0)(43) J e a n   F E R R A T   -   Q u e   l a   m o n t a g n e   e s t   b e l l e . m p 3				LOCAL FILE NAME
(0)(6) s h a r e s			"shares"
(0)(44)(219)(24)				SIZE
(0)(128)					BITRATE
(0)(0)(0)(183)				LENGTH
(0)					NO ID3v1
						TITLE
						ARTIST
  						ALBUM
(1)					YES ID3v2
(0)(25) Q u e   l a   M o n t a g n e   e s t   B e l l e	TITLE
(0)(11) J e a n   F e r r a t				ARTIST
(0)(0)							ALBUM
(0)(0)							TRACKNUM
(0)(0)							TAG


FROM SERVER: NEW SHARE RESPONSE
(69)(95)(208)(213)
(0)(0)(0)(43)
(0)(0)(0)(9)
(0)(13)
(0)(0)(0)(0)				LOCAL ID
(0)(15) 8 0 2 c d b 1 8 0 a 2 9 2 2 f	FILE SET
(0)(0)(0)(0)				SERVER ID
(0)(162)(146)(47)				SONG ID
(0)(44)(219)(24)				FILE SIZE

DELETE REQUEST: {FILE HASH, FILE NAME}
  
*)
  
module WrongLogin = struct
    type t = 
      IncorrectPassword
    | UnknownUsername
    | BadClientVersion
    | AlreadyConnected
    | GoldAccoundExpired
    | NotGoldAccount
    | OtherError of int
    
    let parse s = 
      match get_int32 s 0 with
        0 -> IncorrectPassword
      | 1 -> UnknownUsername
      | 2 -> BadClientVersion
      | 3 -> AlreadyConnected
      | 4 -> GoldAccoundExpired
      | 5 -> NotGoldAccount
      | v -> OtherError v
    
    let print t = 
      Printf.printf "Login Error %s"
        (match t with
          IncorrectPassword -> "IncorrectPassword"
        | UnknownUsername -> "UnknownUsername"
        | BadClientVersion -> "BadClientVersion"
        | AlreadyConnected -> "AlreadyConnected"
        | GoldAccoundExpired -> "GoldAccoundExpired"
        | NotGoldAccount -> "NotGoldAccount"
        | OtherError v -> Printf.sprintf "OtherError %d" v);
      print_newline () 
      
    let write buf t = 
      buf_int32 buf (
        match t with
          IncorrectPassword -> 0
        | UnknownUsername -> 1
        | BadClientVersion -> 2
        | AlreadyConnected -> 3
        | GoldAccoundExpired -> 4
        | NotGoldAccount -> 5
        | OtherError v -> v
      )
  end

module Msg = struct
    type t = ()
      
    let parse s = ()
      
    let print t = ()
      
    let write buf t = ()
    
  end
  
type t =
| LoginReq of Login.t
| SendSharesReq
| SendSharesStandByReq
| ReadyToSendSharesReq
| FileTransferReq of FileTransfer.t
| FileTransferStateReq of FileTransferState.t
| WrongLoginReq of WrongLogin.t
| ReadyForTransferReq
| StopFileTransferReq of StopFileTransfer.t
| UnknownReq of int * string

(*

#define LOGIN                                   0
#define LOGOFF                                  1

#define NEW_SHARE                               2
#define OLD_SHARE                               3

#define READY_FOR_TRANSFER                      5

#define FILE_TRANSFER_STATE                     6

#define READY_TO_SEND_SHARES                    7
#define SEND_SHARES_STAND_BY                    8
#define SEND_SHARES                             9
#define FILE_TRANSFER                           10
#define STOP_FILE_TRANSFER                      11

#define NEW_SHARE_RESPONSE                      13

#define WRONG_LOGIN_MESSAGE                     14

#define NEW_USER                                23

#define EX_ACCOUNT_INFORMATION                  31

#define DATA_TO_UPDATE_SHARES                   33

#define DELETE_RESPONSE                         34

#define NEW_ACCOUNT_INFORMATION                 35
  
#define NEW_ACCOUNT_INFORMATION_RESPONSE        50
#define EX_ACCOUNT_INFORMATION_RESPONSE         51
#define DELETE_REQUEST                          52
#define PLAY_A_FILE_IN_LOCAL_DB                 53
#define LOGIN_HASH_PREFIX                       55

*)
  
let parse msg_type data msg_len = 
  try
    match msg_type with
    | 0 -> LoginReq (Login.parse data)
    | 8  -> SendSharesStandByReq
    | 9  -> SendSharesReq
    | 7 -> ReadyToSendSharesReq
    | 10 -> FileTransferReq (FileTransfer.parse data)
    | 11 -> StopFileTransferReq (StopFileTransfer.parse data)
    | 6 -> FileTransferStateReq (FileTransferState.parse data)
    | 14 -> WrongLoginReq (WrongLogin.parse data)
    | 5 -> ReadyForTransferReq
    | _ -> raise Not_found
  with e -> 
      Printf.printf "EXception %s in parse" (Printexc2.to_string e); print_newline ();
      UnknownReq (msg_type,data)
      
let print t =
  begin
    match t with
    | LoginReq t -> Login.print t
    | SendSharesReq -> SendShares.print t
    | SendSharesStandByReq -> SendSharesStandBy.print t
    | ReadyToSendSharesReq -> ReadyToSendShares.print t
    | FileTransferReq t -> FileTransfer.print t
    | FileTransferStateReq t -> FileTransferState.print t
    | WrongLoginReq t -> WrongLogin.print t
    | ReadyForTransferReq -> ReadyForTransfer.print t
    | StopFileTransferReq t -> StopFileTransfer.print t
    | UnknownReq (msg_type, s) -> 
        Printf.printf "Unknown msg type: %d" msg_type; print_newline ();
        dump s
  end;
  print_newline () 
  
let buf = Buffer.create 10000

let write buf t =
  begin
    match t with
    | LoginReq t ->
        buf_int16 buf 0;
        Login.write buf t
    | ReadyForTransferReq -> 
        buf_int16 buf 5;
    | SendSharesStandByReq -> 
        buf_int16 buf 8;
    | SendSharesReq -> 
        buf_int16 buf 9;
    | ReadyToSendSharesReq ->
        buf_int16 buf 7;
    | FileTransferReq t -> 
        buf_int16 buf 10;
        FileTransfer.write buf t     
    | FileTransferStateReq t -> 
        buf_int16 buf 6;
        FileTransferState.write buf t     
    | WrongLoginReq t -> 
        buf_int16 buf 14;
        WrongLogin.write buf t     
    | StopFileTransferReq t -> 
        buf_int16 buf 11;
        StopFileTransfer.write buf t     
    | UnknownReq (msg_type, data) -> 
        buf_int16 buf msg_type;
        Buffer.add_string buf data;
        
  end
  
(*****************************************************************)
      
  
exception BadConnection  

let check_header sock s pos = 
  if not (
      get_int8 s pos = 69
        && get_int8 s (pos+1) = 95
        && get_int8 s (pos+2) = 208
        && get_int8 s (pos+3) = 213
    ) then raise BadConnection
  
let audiogal_handler f sock nread =
  let b = TcpBufferedSocket.buf sock in
  try
    while b.len >= 8 do
      check_header sock b.buf b.pos;      
      let msg_len = get_int32 b.buf (b.pos+4) in
      if b.len >= 4 + msg_len then
        begin
          let msg_type = get_int16 b.buf (b.pos+12) in
          let data = String.sub b.buf (b.pos+14) (msg_len-10) in
          TcpBufferedSocket.buf_used sock (msg_len + 4);
          let t = parse msg_type data msg_len in
          print t;
          f t sock
        end
      else raise Not_found
    done
  with 
  | Not_found -> ()
  | BadConnection ->
      Printf.printf "BAD REPLY"; print_newline ();
      dump (String.sub b.buf b.pos b.len);
      close sock "BadConnection"

(*
RECEIVE FROM REDIRECT SERVER:
(69)(95)(208)(213)	 	HEADER
(0)(0)(0)(26) 			MESSAGE SIZE (total - 4)
(0)(0)(0)(1)			MESSAGE ID
(0)(0)(0)(21) 			INFO SERVER PORT
(0)(12)  			IP LENGTH
6 4 . 2 4 5 . 5 9 . 8 2 		IP
*)

(* In fact, it is a list of servers which is returned ???*)
      
let redirection_handler f sock nread =
  let b = TcpBufferedSocket.buf sock in
  try
    while b.len >= 8 do
      check_header sock b.buf b.pos;      
      let msg_len = get_int32 b.buf (b.pos+4) in
      if b.len >= 4 + msg_len then
        let ip, port =
          try
            let data = String.sub b.buf (b.pos+12) (msg_len-8) in
            TcpBufferedSocket.buf_used sock (msg_len + 4);
            let port = get_int32 data 0 in
            let ip_s, _ = get_string data 4 in
            Ip.of_string ip_s, port
          with _ -> raise BadConnection
        in
        f sock ip port
      else raise Not_found
    done
  with
    Not_found -> ()
  | BadConnection ->
      Printf.printf "BAD REPLY"; print_newline ();
      dump (String.sub b.buf b.pos b.len);
      close sock "BadConnection"

module PeerReq = struct

(*    
SEND: to file transfer
(69)(95)(208)(213)
(0)(0)(0)(34)
(0)(0)(0)(7)
(0)(16)(0)(0)(0)(1)(0)(0)(0)(1)(0)(0)(0)(0)(0)(0)(0)(0)	FILE ID
(0)(0)(0)(1)						FILE SIZE
(0)(0)(0)(0)						FILE OFFSET
*)
    
    type file_request = {
        file_id : Md4.t;
        file_size : int32;
        file_pos : int32;
      }
    
    type file_response = {
        f1 : int32;
        f2 : int32;
      }
    
    let send_peer_handler f sock nread =
      let b = TcpBufferedSocket.buf sock in
      try
        while b.len >= 8 do
          check_header sock b.buf b.pos;      
          let msg_len = get_int32 b.buf (b.pos+4) in
          if b.len >= 4 + msg_len then
            let t =
              try
                let data = String.sub b.buf (b.pos+12) (msg_len-8) in
                TcpBufferedSocket.buf_used sock (msg_len + 4);
                let file_id,pos = get_md4 data 0 in
                let file_size = get_int32_32 data pos in
                let file_pos = get_int32_32 data (pos+4) in
                { file_id = file_id; 
                  file_pos = file_pos;  
                  file_size = file_size;
                }
              with _ -> raise BadConnection
            in
            f sock t
          else raise Not_found
        done
      with
        Not_found -> ()
      | BadConnection ->
          Printf.printf "BAD REPLY"; print_newline ();
          dump (String.sub b.buf b.pos b.len);
          close sock "BadConnection"
    
    let receive_peer_handler f =      
      let ok = ref false in
      fun sock nread ->
        let b = TcpBufferedSocket.buf sock in
        try
          while b.len > 0 do
            if not !ok then begin
                check_header sock b.buf b.pos;      
                let msg_len = get_int32 b.buf (b.pos+4) in
                if b.len >= 4 + msg_len then
                    try
                      let data = String.sub b.buf (b.pos+12) (msg_len-8) in
                      TcpBufferedSocket.buf_used sock (msg_len + 4);
                      let s, pos = get_string data 0 in
                      if s <> "OK" then raise BadConnection;
                      ok := true
                    with _ -> raise BadConnection
                else
                  raise Not_found
              end
            else
              f sock nread
          done
        with
          Not_found -> ()
        | BadConnection ->
            Printf.printf "BAD REPLY"; print_newline ();
            dump (String.sub b.buf b.pos b.len);
            close sock "BadConnection"
            
    let peer_send sock fr =
      Buffer.clear buf;
      Buffer.add_char buf (char_of_int 69); 
      Buffer.add_char buf (char_of_int 95); 
      Buffer.add_char buf (char_of_int 208); 
      Buffer.add_char buf (char_of_int 213); 
      buf_int32 buf 0;
      buf_int32 buf 0;
      
      buf_md4 buf fr.file_id;
      buf_int32_32 buf fr.file_size;
      buf_int32_32 buf fr.file_pos;
      
      let s = Buffer.contents buf in
      let len = String.length s - 4 in
      str_int32 s 4 len;
      Printf.printf "WRITE STRING OF %d" len; print_newline ();
      write_string sock s 
      
  end

let server_msg_to_string msg n =
  Buffer.clear buf;
  Buffer.add_char buf (char_of_int 69); 
  Buffer.add_char buf (char_of_int 95); 
  Buffer.add_char buf (char_of_int 208); 
  Buffer.add_char buf (char_of_int 213); 
  buf_int32 buf 0;
  buf_int32 buf n;
  write buf msg;
  let s = Buffer.contents buf in
  let len = String.length s - 4 in
  str_int32 s 4 len;
  s 
    
let server_send sock m =
  Printf.printf "SENDING TO SERVER:"; print_newline ();
  print m;
  write_string sock (server_msg_to_string m !AG.message_counter);
  incr AG.message_counter
    
let debug_server_send sock m =
  write_string sock (server_msg_to_string m !AG.message_counter);
  incr AG.message_counter
  
  (*
1) resolve the IP of a redirection server.
2) connect to it on port 21

RECEIVE:
(69)(95)(208)(213)	 	START SIGNAL
(0)(0)(0)(26) 			MESSAGE SIZE (total - 4)
(0)(0)(0)(1)			MESSAGE ID
(0)(0)(0)(21) 			INFO SERVER PORT
(0)(12)  			IP LENGTH
6 4 . 2 4 5 . 5 9 . 8 2 		IP

3) Connect to the server above

  SEND:	"LOGIN"
(69)(95)(208)(213) 		START SIGNAL
(0)(0)(0)(67)      		MESSAGE SIZE
(0)(0)(0)(0) 			MESSAGE NUMBER
(0)(0)				LOGIN MESSAGE
(0)(0)(0)(0)
(0)(11) a u d i o t e s t 1 6	LOGIN NAME
(0)(9) a u d i o 2 0 0 2		PASSWORD
(0)(6) 0 . 5 2 0 L		VERSION
(0)(13) 2 1 2 . 1 9 8 . 6 3 . 1 1	IP
(0)(0)
(0)(0)
(0)(0)


RECEIVE: "SEND_SHARES_STAND_BY"
(69)(95)(208)(213)                  START SIGNAl
(0)(0)(0)(10)                       MESSAGE SIZE
(0)(0)(0)(0)                        MESSAGE NUMBER
(0)(8)
  
SEND: "READY_TO_SEND_SHARES"
(69)(95)(208)(213)
(0)(0)(0)(10)
(0)(0)(0)(1)
(0)(7)

plusieurs fois ...
  
RECEIVE: "FILE TRANSFER"
(69)(95)(208)(213)
(0)(0)(0)(72)
(0)(0)(0)(5)
(0)(10)
  
(0)(16)(0)(0)(0)(1)(0)(0)(0)(1)(0)(0)(0)(0)(0)(0)(0)(0)	FILEID
(1) 				CONNECT
(1) 				DIRECTION
(0)(12) 6 4 . 2 4 5 . 5 8 . 8 1	IP
(0)(0)(160)(218)			PORT
(0)(0)(0)(0)			LOCAL ID
(0)(14) n e w   p r o x y   t e s t	FILENAME
(0)(0)(0)(1)			FILESIZE

      /*
    Type        Name    Contents
    Block       FILE_ID         Identifier for file transfer
    Byte        CONNECT         Connection type : 
        Value   Meaning
        0       Listen for connection
        1       Connect to remote machine
    Byte        DIRECTION       Connection direction : 
        Value   Meaning
        0       Send file to remote host
        1       Receive file from remote host
    String      IP              IP to connect to/mask to accept from
    Integer     PORT            Port to connect on/listen on
    Integer     LOCAL_ID        Client identifier number
    String      FILENAME        Suggested filename to transfer as
    Integer     FILESIZE
    */


SEND TO SERVER: "FILE TRANSFER STATE"
(69)(95)(208)(213)
(0)(0)(0)(32)
(0)(0)(0)(6)
(0)(6)
(0)(16)(0)(0)(0)(1)(0)(0)(0)(1)(0)(0)(0)(0)(0)(0)(0)(0)	FILE ID
(0)(0)(0)(3)						FTS SETTING UP

SEND: to file transfer
(69)(95)(208)(213)
(0)(0)(0)(34)
(0)(0)(0)(7)
(0)(16)(0)(0)(0)(1)(0)(0)(0)(1)(0)(0)(0)(0)(0)(0)(0)(0)	FILE ID
(0)(0)(0)(1)						FILE SIZE
(0)(0)(0)(0)						FILE OFFSET

RECEIVE FROM SERVER: "FILE TRANSFER"
(69)(95)(208)(213)
(0)(0)(0)(72)
(0)(0)(0)(6)

(0)(10)
(0)(16)(0)(0)(0)(2)(0)(0)(0)(2)(0)(0)(0)(0)(0)(0)(0)(0)
(0)
(1)
(0)(12) 6 4 . 2 4 5 . 5 9 . 8 2
(0)(0)(0)(0)
(0)(14) n e w   p r o x y   t e s t
(0)(0)(0)(1)

SEND TO SERVER: "FILE TRANSFER STATE"
(69)(95)(208)(213)
(0)(0)(0)(32)
(0)(0)(0)(8)

(0)(6)
(0)(16)(0)(0)(0)(1)(0)(0)(0)(1)(0)(0)(0)(0)(0)(0)(0)(0)	FILE ID
(0)(0)(0)(5)						FTS CONNECTION CLOSED


SECONDE CONNECTION:

FROM REDIRECTION SERVER: "REDIRECTION"

(69)(95)(208)(213)
(0)(0)(0)(27)
(0)(0)(0)(1)
(0)(0)(0)(21)
(0)(13) 6 4 . 2 4 5 . 5 9 . 1 1 8

FROM SERVER: "WRONG_LOGIN_MESSAGE"


          
SEND SHARES !!!

1022232875.085 212.198.63.11:6529 -> 64.245.58.87:5376 of len 129
ascii [ E _(208)(213)(0)(0)(0) }(0)(0)(0)(12)(0)(2)(0)(0)(0)(0)(0) + J e a n   F E R R A T   -   Q u e   l a   m o n t a g n e   e s t   b e l l e . m p 3(0)(6) s h a r e s(0) ,(219)(24)(0)(128)(0)(0)(0)(183)(0)(1)(0)(25) 0)(11) J e a n   F e r r a t(0)(0)(0)(0)(0)(0)]
dec [

TO SERVER: READY FOR TRANSFER
(69)(95)(208)(213)
(0)(0)(0)(10)
(0)(0)(0)(13)
(0)(5)

FROM SERVER: FILE TRANSFER
ascii [ E _(208)(213)(0)(0)(0) U(0)(0)(0)(10)(0)(10)(0)(16)(1)(214)(169) t(26) \ n(151) <(238)(8)(202) <(238)(8)(203)(1)(1)(0)(15) 2 1 2 . 1 9 8 . 1 8 9 . 1 8 9(0)(0)(160) 1(0)(0)(0) O(0)(24) d a l i d a   -   j ' a t t e n d r a i . m p 3(0) =(183)(28)]
dec [
(69)(95)(208)(213)
(0)(0)(0)(85)
(0)(0)(0)(10)
(0)(10)
(0)(16)(1)(214)(169)(116)(26)(92)(110)(151)(60)(238)(8)(202)(60)(238)(8)(203)(1)(1)(0)(15)(50)(49)(50)(46)(49)(57)(56)(46)(49)(56)(57)(46)(49)(56)(57)(0)(0)(160)(49)(0)(0)(0)(79)(0)(24)(100)(97)(108)(105)(100)(97)(32)(45)(32)(106)(39)(97)(116)(116)(101)(110)(100)(114)(97)(105)(46)(109)(112)(51)(0)(61)(183)(28)]

TO SERVER: SETTING UP
dec [(69)(95)(208)(213)(0)(0)(0)(32)(0)(0)(0)(14)(0)(6)(0)(16)(1)(214)(169)(116)(26)(92)(110)(151)(60)(238)(8)(202)(60)(238)(8)(203)(0)(0)(0)(3)]


TO SOMEONE ELSE:
(69)(95)(208)(213)(0)(0)(0)(34)(0)(0)(0)(15)
(0)(16)(1)(214)(169)(116)(26)(92)(110)(151)(60)(238)(8)(202)(60)(238)(8)(203)
(0)(61)(183)(28)
(0)(0)(0)(0)


FROM SOMEONE ELSE:
(69)(95)(208)(213)
(0)(0)(0)(12)
(0)(0)(94)(112)
(0)(2)(79)(75)
  
*)
