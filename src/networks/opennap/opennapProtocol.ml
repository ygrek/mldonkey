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
  
(*

  Add redirection:
Unknown msg type: 821
ascii: [ m u l t i s c a n . m y i p . o r g   6 6 6 6]

  
de
ERVER 217.228.79.25:8888 kaenguru
ERROR FROM SERVER: This server is full (900 connections)
EXception Not_found in parse
#################  UNUSED   ###############
Unknown msg type: 821
ascii: [ a 3 s e n . s y t e s . n e t   7 7 7 7]
dec: [(97)(51)(115)(101)(110)(46)(115)(121)(116)(101)(115)(46)(110)(101)(116)(32)(55)(55)(55)(55)]

SERVER 80.139.227.43:6680 oldnap
ERRO


  RVER 62.163.26.78:7777 nederlandnap
ERROR FROM SERVER: This server is full (400 connections)
SERVER 62.254.134.69:4444 n/a
ERROR FROM SERVER: This server is full (1200 users)
SERVER 213.106.2.65:6644 powernap
ERROR FROM SERVER: This server is full (501 connections)
SERVER 213.215.174.37:8888 djnap
ERROR FROM SERVER: This server is full (1100 connections)
SERVER 12.217.88.57:8888 gomig
ERROR FROM SERVER: Only registered users are allowed to login
DownloadAckReq TheBorg1976 !!!!!!!!!!!!!!!!!!!!!!!!
************** Can download directly *************
Exception Not_found in DownloadAckReq
EXception Not_found in parse
#################  UNUSED   ###############
Unknown msg type: 748
ascii: []
dec: []

Opennap.get_file_from_source not implemented
SENDING 205 [lefrog //WantQueue]
SENDING 203 [lefrog "D:\Dion, Celine\Ave Maria.mp3"]

  
EXception Not_found in parse
##############
Unknown msg type: 620
ascii: [ v e r t i c a l _ l i m i t   " E : \ w i n m x _ t r a d e \ M y l e n e   F a r m e r \ M y l e n e   F a r m e r   -   X X L . m p g "   3 3 9 5 8 2 8 8   1 0 0 4 5]
dec: [(118)(101)(114)(116)(105)(99)(97)(108)(95)(108)(105)(109)(105)(116)(32)(34)(69)(58)(92)(119)(105)(110)(109)(120)(95)(116)(114)(97)(100)(101)(92)(77)(121)(108)(101)(110)(101)(32)(70)(97)(114)(109)(101)(114)(92)(77)(121)(108)(101)(110)(101)(32)(70)(97)(114)(109)(101)(114)(32)(45)(32)(88)(88)(76)(46)(109)(112)(103)(34)(32)(51)(51)(57)(53)(56)(50)(56)(56)(32)(49)(48)(48)(52)(53)]

Must download indirectly
EXception Not_found in parse
##############
Unknown msg type: 620
ascii: [ v e r t i c a l _ l i m i t   " E : \ w i n m x _ t r a d e \ M y l e n e   F a r m e r \ D i s k   2 \ M y l e n e   F a r m e r   -   X X L . m p g "   3 3 9 5 8 2 8 8   1 0 0 4 5]

EXception Not_found in parse
##############
Unknown msg type: 752
ascii: [ g u i t o u 4 1]
dec: [(103)(117)(105)(116)(111)(117)(52)(49)]

EXception Not_found in parse
##############
Unknown msg type: 620
ascii: [ g u i t o u 4 1   " G : \ M P 3   d e   A  (224)   M \ M y l(232) n e   F a r m e r \ 1 0 - M Y L E N E   F A R M E R   -   L a   V e u v e   N o i r e . m p 3 "   0   0]

*)
  
open TcpBufferedSocket
  
open OpennapTypes



(* These are little endian representation. Similar to Mftp. Maybe put them
in another module *)

let get_int8 s pos = 
  int_of_char s.[pos]

let get_int16 s pos =
  let c1 = int_of_char s.[pos] in
  let c2 = int_of_char s.[pos+1] in
  c1 lor (c2 lsl 8)

let buf_int8 buf i =
  Buffer.add_char buf (char_of_int (i land 0xff))
  
let buf_int16 buf i =
  let i = i land 0xffff in
  Buffer.add_char buf (char_of_int (i land 0xff));
  Buffer.add_char buf (char_of_int (i lsr 8))
  
let dump s =
  let len = String.length s in
  lprintf "ascii: [";
  for i = 0 to len - 1 do
    let c = s.[i] in
    let n = int_of_char c in
    if n > 31 && n < 127 then
      lprintf " %c" c
    else
      lprintf "(%d)" n
  done;
  lprintf "]\n";
  lprintf "dec: [";
  for i = 0 to len - 1 do
    let c = s.[i] in
    let n = int_of_char c in
    lprintf "(%d)" n            
  done;
  lprintf "]\n"

(************************************************************************)
  
let exit_exn = Exit
  
let safe_string s =
  if s = "" || String.contains s ' ' then 
    Printf.sprintf "\"%s\"" ( (*String.escaped*) s) 
  else s
    
  (*
  if s = "" then "\"\""
  else
  try
    match s.[0] with
      'a'..'z' | 'A'..'Z' ->
        for i = 1 to String.length s - 1 do
          match s.[i] with
            'a'..'z' | 'A'..'Z' | '_' | '0'..'9' -> ()
          | _ -> raise exit_exn
        done;
        s
    | _ -> raise exit_exn
  with
    _ -> Printf.sprintf "\"%s\"" ( (*String.escaped*) s)
;;
*)
  
let get_string s pos =
  let len = String.length s in
  let rec iter pos =
    if pos = len then "", len else
    let c = s.[pos] in
    if c = ' ' then
      iter (pos+1)
    else
    if c = '"' then
      let pos2 = String.index_from s (pos+1) '"' in
      let str = String.sub s (pos+1) (pos2 - pos - 1) in
      str, pos2+1
    else
    try
      let pos2 = String.index_from s pos ' ' in
      let str = String.sub s pos (pos2 - pos) in
      str, pos2
    with _ ->
        String.sub s pos (len-pos), len
  in 
  iter pos
  
let get_strings s pos =
  let len = String.length s in
  let rec iter pos list =
    if pos = len then List.rev list else
    let c = s.[pos] in
    if c = ' ' then
      iter (pos+1) list
    else
    if c = '"' then
      let pos2 = String.index_from s (pos+1) '"' in
      let str = String.sub s (pos+1) (pos2 - pos - 1) in
      iter (pos2+1) (str :: list)
    else
    let s, pos =
      try
        let pos2 = String.index_from s pos ' ' in
        let str = String.sub s pos (pos2 - pos) in
        str, pos2
      with _ ->
          String.sub s pos (len-pos), len
    in
    iter pos (s :: list)
  in 
  iter pos []

(*****************************************************************)
    
    let int_of_link t =
      match t with
      | LinkUnknown -> 0
      | Link14_4 -> 1
      | Link28_8 -> 2
      | Link33_6 -> 3
      | Link56_7 -> 4
      | Link64K_ISDN -> 5
      | Link128K_ISDN -> 6
      | LinkCable -> 7
      | LinkDSL -> 8
      | LinkT1 -> 9
      | LinkT3 -> 10
    
    let string_of_link t =
      match t with
      | LinkUnknown -> "Unknown Link"
      | Link14_4 -> "14.4 kbs"
      | Link28_8 ->  "28.8 kbs"
      | Link33_6 ->  "33.6 kbs"
      | Link56_7 ->  "56.7 kbs"
      | Link64K_ISDN -> "64K ISDN"
      | Link128K_ISDN -> "128K ISDN"
      | LinkCable -> "Cable"
      | LinkDSL -> "DSL"
      | LinkT1 -> "T1"
      | LinkT3 -> "T3 or Greater"
    
    let link_of_int t =
      match t with
      | 0 -> LinkUnknown
      | 1 -> Link14_4
      | 2 -> Link28_8
      | 3 -> Link33_6
      | 4 -> Link56_7
      | 5 -> Link64K_ISDN
      | 6 -> Link128K_ISDN
      | 7 -> LinkCable
      | 8 -> LinkDSL
      | 9 -> LinkT1
      | _ -> LinkT3

  
module SimpleString = functor(M: sig val msg : string end) -> 
  struct
    
    type t = string
      
      let parse s = s
      
      let print t =
      lprintf "message %s: %s" M.msg t
      
    let write buf t = Buffer.add_string buf t
  end
  
module Empty = functor(M: sig val msg : string end) -> 
    struct
    
      let parse s = ()
      
      let print t =
        lprintf "message %s" M.msg
        
      let write buf t = ()
    end

module Error = SimpleString(struct let msg = "Error" end)
  (* Error: server --> client *)
module Message = SimpleString(struct let msg = "Message" end)
(* Message: server --> client *)

module Login = struct (* Login: client --> server *)
      
    type t = {
        nick : string;
        password : string;
        port : int;
        client_info : string;
        link_type: link_type;
      }
      
    let parse s = 
      let nick, pos = get_string s 0 in
      let password, pos = get_string s pos in
      let port_s, pos = get_string s pos in
      let client_info, pos = get_string s pos in
      let link_type_s, pos = get_string s pos in
      let port = int_of_string port_s in
      let link_type = link_of_int (int_of_string link_type_s) in
      { 
        nick = nick;
        password = password;
        port = port;
        client_info = client_info;
        link_type = link_type;
      }
      
    let print t =
      lprintf "LOGIN %s %s %d \"%s\" %s"
        t.nick t.password t.port t.client_info (string_of_link t.link_type)
      
    let write buf t =
      Printf.bprintf buf "%s %s %d \"%s\" %d" 
        t.nick t.password t.port t.client_info (int_of_link t.link_type)
  end
  
module NewUserLogin = struct (* Login: client --> server *)
      
    type t = {
        nick : string;
        password : string;
        port : int;
        client_info : string;
        link_type: link_type;
        email : string;
      }
      
    let parse s = 
      let nick, pos = get_string s 0 in
      let password, pos = get_string s pos in
      let port_s, pos = get_string s pos in
      let client_info, pos = get_string s pos in
      let link_type_s, pos = get_string s pos in
      let email, pos = get_string s pos in
      let port = int_of_string port_s in
      let link_type = link_of_int (int_of_string link_type_s) in
      { 
        nick = nick;
        password = password;
        port = port;
        client_info = client_info;
        link_type = link_type;
        email = email;
      }
      
    let print t =
      lprintf "NEW USER LOGIN %s %s %d \"%s\" %s %s"
        t.nick t.password t.port t.client_info (string_of_link t.link_type)
      t.email
      
    let write buf t =
      Printf.bprintf buf "%s %s %d \"%s\" %d %s" 
        t.nick t.password t.port t.client_info (int_of_link t.link_type)
      t.email
  end

module SearchReply = struct
    type t = {
    filename : string;
    md5 : string;
    size : int64;
    bitrate : int;
    freq : int;
    length : int;
    nick : string;
    ip : Ip.t;
    link_type : link_type;
    weight : int;
  }
      
    let parse s =
      match get_strings s 0 with
        filename ::
        md5 ::
        size_s ::
        bitrate_s ::
        freq_s ::
        length_s ::
        nick ::
        ip_s ::
        link_type_s ::
        tail -> begin
            let weight = match tail with
                [] -> 0
              | weight :: _ -> int_of_string weight
            in
            
            let size = Int64.of_string size_s in
            let bitrate = int_of_string bitrate_s in
            let freq = int_of_string freq_s in
            let length = int_of_string length_s in
            let ip = Ip.of_int64 (Int64.of_string ip_s) in
            let link_type = link_of_int (int_of_string link_type_s) in
            
            { 
              filename = filename;
              md5 = md5;
              size = size;
              bitrate = bitrate;
              freq = freq;
              length = length;
              nick = nick;
              ip = ip;
              link_type = link_type;
              weight = weight;
            }
          end
      | _ -> failwith "Bad number of args in search reply"            

      
    let print t = 
      lprintf "SEARCH REPLY \"%s\" %s %s %d %d %d %s %s %s %d"
        t.filename t.md5 (Int64.to_string t.size) t.bitrate t.freq t.length
        t.nick (Ip.to_string t.ip) 
      (string_of_link t.link_type) t.weight
      
    let write buf t =
      Printf.bprintf buf "\"%s\" %s %s %d %d %d %s %s %d %d"
        t.filename t.md5 (Int64.to_string t.size) t.bitrate t.freq t.length
        t.nick (Int64.to_string (Ip.to_int64 t.ip))
      (int_of_link t.link_type) t.weight      
  end
  
module LoginAck = SimpleString(struct let msg = "LoginAck" end)
(* server --> client (email address of registration) *)
  
module VersionCheck = SimpleString(struct let msg = "VersionCheck" end)
(* client --> server "2.0" sound good *)
  
module AutoUpgrade = SimpleString(struct let msg = "AutoUpgrade" end)
(* server --> client big security hole *)

module NickCheck = SimpleString(struct let msg = "NickCheck" end)
(* client --> server *)
  
module NickUnused = Empty(struct let msg = "NickUnused" end)
module NickAlreadyUsed = Empty(struct let msg = "NickAlreadyUsed" end)
module NickInvalid = Empty(struct let msg = "NickInvalid" end)
  
type  compare = AtLeast | AtBest | EqualTo

let string_of_compare comp =
  match comp with
    AtLeast -> "AT LEAST"
  | AtBest -> "AT BEST"
  | EqualTo -> "EQUAL TO"

let compare_of_strings comp =
  match comp with
    "AT LEAST" -> AtLeast
  | "AT BEST" -> AtBest
  | "EQUAL TO" -> EqualTo
  | _ -> failwith "compare_of_strings failed"
      
module Search = struct
    type t = {
        artist : string option;
        max_results : int; (* max = 100 *)
        title : string option;
        linespeed : (link_type * compare) option;
        bitrate : (int * compare) option; (* kbps *)
        freq : (int * compare) option; (* Hz *)
        wma_file : bool;
        local_only : bool; (* not on linked servers *)
      }

    let dummy_search = {
        artist = None;
        max_results = 100; (* max = 100 *)
        title = None;
        linespeed = None;
        bitrate = None; (* kbps *)
        freq = None; (* Hz *)
        wma_file = false;
        local_only = false; (* not on linked servers *)        
      }
      
    let parse s = 
      let list = get_strings s 0 in
      let rec iter artist list t =
        match list with
          "FILENAME" :: "CONTAINS" :: name :: tail ->
            iter false tail (
              if artist then { t with artist = Some name }
              else { t with title = Some name })
        | "MAX_RESULTS" :: max :: tail ->
            iter false tail { t with max_results = min 100 (int_of_string max) }
        | "LINESPEED" :: comp  :: value :: tail ->
            iter false tail { t with linespeed = Some (
                link_of_int (int_of_string value), 
                compare_of_strings comp) }
        | "BITRATE" :: comp :: value :: tail ->
            iter false tail { t with bitrate = Some (
                (int_of_string value), 
                compare_of_strings comp) }
        | "FREQ" :: comp :: value :: tail ->
            iter false tail { t with freq = Some (
                (int_of_string value), 
                compare_of_strings comp) }
        | "WMA-FILE" :: tail ->
            iter false tail { t with wma_file = true }            
        | "LOCAL_ONLY" :: tail ->
            iter false tail { t with local_only = true }            
        | _ -> raise Not_found
      in
      iter true list dummy_search
      
    let print t = ()
      
    let write buf t = 
      (match t.artist with None -> () | Some s -> 
            Printf.bprintf buf "FILENAME CONTAINS \"%s\" " s);
      Printf.bprintf buf "MAX_RESULTS %d" t.max_results;
      (match t.title with None -> () | Some s -> 
            Printf.bprintf buf " FILENAME CONTAINS \"%s\"" s);
      (match t.linespeed with None -> () | Some (lt, comp) -> 
            Printf.bprintf buf " LINESPEED \"%s\" %d"
              (string_of_compare comp) (int_of_link lt));
      (match t.bitrate with None -> () | Some (br, comp) -> 
            Printf.bprintf buf " BITRATE \"%s\" %d"
              (string_of_compare comp) br);
      (match t.freq with None -> () | Some (fr, comp) -> 
            Printf.bprintf buf " FREQ \"%s\" %d"
              (string_of_compare comp) fr);
      if t.wma_file then Printf.bprintf buf " WMA-FILE";
      if t.local_only then Printf.bprintf buf " LOCAL_ONLY";
      ()
      
  end

module EndOfSearchReply = Empty(struct let msg = "EndOfSearchReply" end)
module GetServerStats = Empty(struct let msg = "GetServerStats" end)
  
module DownloadRequest = struct
    type t = {
        nick  : string;
        filename : string;
      }
      
    let parse s = 
      match get_strings s 0 with
        [ nick ; filename ] -> { nick = nick; filename = filename }
      | _ -> raise Not_found
      
    let print t = 
      lprintf "DownloadRequest from %s of %s" t.nick t.filename
      
    let write buf t = 
      Printf.bprintf buf "%s \"%s\"" t.nick t.filename
  end
  
module DownloadAck = struct
    type t = {
        nick  : string;
        ip : Ip.t;
        port : int;
        filename : string;
        md5: string;
        linespeed : link_type;
      }
      
    let parse s = 
      lprintf "DOWNLOAD ACK [%s]" s; lprint_newline ();
      match get_strings s 0 with
        [ nick ; ip_s; port_s; filename; md5; linespeed_s ] -> 
          let ip = Ip.rev (Ip.of_int64 (Int64.of_string ip_s)) in
          lprintf "IP %s = %s" ip_s (Ip.to_string ip);
          lprint_newline ();
          { nick = nick; 
            ip = ip;
            port = int_of_string port_s;
            filename = filename;
            md5 = md5;
            linespeed = link_of_int (int_of_string linespeed_s);
          }
      | _ -> raise Not_found
      
    let print t = 
      lprintf "DownloadAck from %s at %s:%d (%s) of %s %s" 
        t.nick (Ip.to_string t.ip) t.port (string_of_link t.linespeed)
      t.filename t.md5
      
    let write buf t = 
      Printf.bprintf buf "%s %s %d \"%s\" %s %d" 
        t.nick (Int64.to_string (Ip.to_int64 (Ip.rev t.ip))) t.port
        t.filename t.md5 (int_of_link t.linespeed)
  end
  
module DownloadError = struct
    type t = {
        nick  : string;
        filename : string;
      }
      
    let parse s = 
      match get_strings s 0 with
        [ nick ; filename ] -> { nick = nick; filename = filename }
      | _ -> raise Not_found
      
    let print t = 
      lprintf "DownloadError from %s of %s" t.nick t.filename
      
    let write buf t = 
      Printf.bprintf buf "%s \"%s\"" t.nick t.filename
  end
  
module PrivateMessage = struct
    type t = {
        nick  : string;
        message : string;
      }
      
    let parse s = 
      match String2.splitn s ' ' 1 with
        [ nick ; message ] -> { nick = nick; message = message }
      | _ -> raise Not_found
      
    let print t = 
      lprintf "PrivateMessage from %s : %s" t.nick t.message
      
    let write buf t = 
      Printf.bprintf buf "%s %s" t.nick t.message
  end
  
module ServerStats = struct
    type t = {
        users  : int;
        files : int;
        size : int;
      }
      
    let parse s = 
      match get_strings s 0 with
        users_s :: files_s :: size_s :: _ -> 
          { users = int_of_string users_s;
            files = int_of_string files_s;
            size = int_of_string size_s;
          }
      | _ -> raise Not_found
      
    let print t = 
      lprintf "ServerStats  %d users %d files %d GB" 
      t.users t.files t.size
      
    let write buf t = 
      Printf.bprintf buf "%d %d %d" t.users t.files t.size
  end
  
module Resume = struct
    type t = {
        md5  : string;
        filesize : int64;
      }
      
    let parse s = 
      match get_strings s 0 with
        [ md5 ; filesize_s ] -> 
          { md5 = md5;
            filesize = Int64.of_string filesize_s;
          }
      | _ -> raise Not_found
      
    let print t = 
      lprintf "RequestResume  %s %s" 
        t.md5 (Int64.to_string t.filesize)
      
    let write buf t = 
      Printf.bprintf buf "%s %s" t.md5 (Int64.to_string t.filesize)
  end
  
module ResumeReply = DownloadAck
module EndOfResumeReply = Empty (struct let msg = "EndOfResumeReply" end)  
  
module BrowseUser = SimpleString (struct let msg = "BrowseUser" end)
module BrowseUserReplyEnd = Empty (struct let msg = "BrowseUser" end)
  
module BrowseUserReply = struct
    type t = {
        nick : string;
        filename : string;
        md5 : string;
        size : int64;
        bitrate : int;
        freq : int;
        length : int;
      }
      
    let parse s =
      match get_strings s 0 with
        nick ::
        filename ::
        md5 ::
        size_s ::
        bitrate_s ::
        freq_s ::
        length_s ::
        tail -> begin
            let size = Int64.of_string size_s in
            let bitrate = int_of_string bitrate_s in
            let freq = int_of_string freq_s in
            let length = int_of_string length_s in
            { 
              filename = filename;
              md5 = md5;
              size = size;
              bitrate = bitrate;
              freq = freq;
              length = length;
              nick = nick;
            }
          end
      | _ -> failwith "Bad number of args in browse reply"            

      
    let print t = 
      lprintf "BROWSE REPLY %s \"%s\" %s %s %d %d %d"
        t.nick t.filename t.md5 (Int64.to_string t.size) t.bitrate t.freq 
      t.length
        
    let write buf t =
      Printf.bprintf buf "%s \"%s\" %s %s %d %d %d"
        t.nick t.filename t.md5
      (Int64.to_string t.size) t.bitrate t.freq t.length
  end
  
module AddFile = struct
    type t = {
        filename : string;
        md5 : string;
        size : int64;
        bitrate : int;
        freq : int;
        length : int;
      }
      
    let parse s =
      match get_strings s 0 with
        filename ::
        md5 ::
        size_s ::
        bitrate_s ::
        freq_s ::
        length_s ::
        tail -> begin
            let size = Int64.of_string size_s in
            let bitrate = int_of_string bitrate_s in
            let freq = int_of_string freq_s in
            let length = int_of_string length_s in
            { 
              filename = filename;
              md5 = md5;
              size = size;
              bitrate = bitrate;
              freq = freq;
              length = length;
            }
          end
      | _ -> failwith "Bad number of args in browse reply"            

      
    let print t = 
      lprintf "ADD FILE \"%s\" %s %s %d %d %d"
      t.filename t.md5 (Int64.to_string t.size) t.bitrate t.freq t.length
        
    let write buf t =
      Printf.bprintf buf "\"%s\" %s %s %d %d %d"
        t.filename t.md5
      (Int64.to_string t.size) t.bitrate t.freq t.length
  end

module Msg = struct
    type t = unit
      
    let parse s = ()
      
    let print t = ()
      
    let write buf t = ()
    
  end
  
type t =
| ErrorReq of Error.t
| LoginReq of Login.t
| LoginAckReq of LoginAck.t 
| VersionCheckReq of VersionCheck.t
| AutoUpgradeReq of AutoUpgrade.t
(* to login on a server *)
| NickCheckReq of NickCheck.t
| NickUnusedReq
| NickAlreadyUsedReq
| NickInvalidReq
| PrivateMessageReq of PrivateMessage.t
| NewUserLoginReq of NewUserLogin.t
| ServerStatsReq of ServerStats.t
| GetServerStatsReq
(* for searches on the server *)
| SearchReq of Search.t
| SearchReplyReq of SearchReply.t
| EndOfSearchReplyReq
(* to download a file *)
| DownloadRequestReq of DownloadRequest.t
| DownloadAckReq of DownloadAck.t
| DownloadErrorReq of DownloadError.t
(* to resume a download *)
| ResumeReq of Resume.t
| ResumeReplyReq of ResumeReply.t
| EndOfResumeReplyReq
| MessageReq of Message.t

| AlternateDownloadRequestReq of DownloadRequest.t
    
| BrowseUserReq of BrowseUser.t
| BrowseUserReplyReq of BrowseUserReply.t
| BrowseUserReplyEndReq

| AddFileReq of AddFile.t
  
| UnknownReq of int * string
  
let parse msg_type data msg_len = 
  try
    match msg_type with
    | 0 | 404 -> ErrorReq (Error.parse data)
    | 2 -> LoginReq (Login.parse data)
    | 3 -> LoginAckReq (LoginAck.parse data)
    | 4 -> VersionCheckReq (VersionCheck.parse data)
    | 5 -> AutoUpgradeReq (AutoUpgrade.parse data)
    | 6 -> NewUserLoginReq (NewUserLogin.parse data)
    | 7 -> NickCheckReq (NickCheck.parse data)
    | 8 -> NickUnusedReq
    | 9 -> NickAlreadyUsedReq
    | 10 -> NickInvalidReq
        
    | 100 -> AddFileReq (AddFile.parse data)
        
    | 200 -> SearchReq (Search.parse data)
    | 201 -> SearchReplyReq (SearchReply.parse data)
    | 202 -> EndOfSearchReplyReq
    | 203 -> DownloadRequestReq (DownloadRequest.parse data)
    | 204 -> DownloadAckReq (DownloadAck.parse data)
    | 205 -> PrivateMessageReq (PrivateMessage.parse data)
    | 206 -> DownloadErrorReq (DownloadError.parse data)

    | 211 -> BrowseUserReq (BrowseUser.parse data)
    | 212 -> BrowseUserReplyReq (BrowseUserReply.parse data)
    | 213 -> BrowseUserReplyEndReq
        
    | 214 -> 
        if msg_len = 0 then GetServerStatsReq else
        ServerStatsReq (ServerStats.parse data)
    | 215 -> ResumeReq (Resume.parse data)
    | 216 -> ResumeReplyReq (DownloadAck.parse data)
    | 217 -> EndOfResumeReplyReq
        | 621 -> MessageReq (Message.parse data)
    
    | 500 -> AlternateDownloadRequestReq (DownloadRequest.parse data)
    | _ -> raise Not_found
  with e -> 
      lprintf "EXception %s in parse" (Printexc2.to_string e); lprint_newline ();
      UnknownReq (msg_type,data)
      
let print t =
  begin
    match t with
    | ErrorReq t -> Error.print t
    | LoginReq t -> Login.print t
    | LoginAckReq t -> LoginAck.print t
    | VersionCheckReq t -> VersionCheck.print t    
    | AutoUpgradeReq t -> AutoUpgrade.print t
    | NewUserLoginReq t -> NewUserLogin.print t
    | NickCheckReq t -> NickCheck.print t
    | NickUnusedReq -> NickUnused.print ()
    | NickAlreadyUsedReq -> NickAlreadyUsed.print ()
    | NickInvalidReq  -> NickInvalid.print ()
    | SearchReq t -> Search.print t
    | SearchReplyReq t -> SearchReply.print t
    | EndOfSearchReplyReq -> EndOfSearchReply.print ()

    | AddFileReq t -> AddFile.print t
        
    | DownloadRequestReq t -> DownloadRequest.print t
    | DownloadAckReq t -> DownloadAck.print t
    | PrivateMessageReq t -> PrivateMessage.print t
    | DownloadErrorReq t -> DownloadError.print t
    
    | BrowseUserReplyEndReq -> BrowseUserReplyEnd.print ()
    | BrowseUserReplyReq t -> BrowseUserReply.print t
    | BrowseUserReq t -> BrowseUser.print t
        
    | ServerStatsReq t -> ServerStats.print t
    | ResumeReq t -> Resume.print t
    | ResumeReplyReq t -> 
        lprintf "RESUME "; DownloadAck.print t
    | EndOfResumeReplyReq -> EndOfResumeReply.print t
    | MessageReq t -> Message.print t
    | GetServerStatsReq -> GetServerStats.print ()
    | AlternateDownloadRequestReq t -> 
        lprintf "ALTERNATE "; DownloadRequest.print t
    
    | UnknownReq (msg_type, s) -> 
        lprintf "Unknown msg type: %d" msg_type; lprint_newline ();
        dump s
  end;
  lprint_newline () 
  
let buf = Buffer.create 10000

let write buf t =
  begin
    match t with
    | ErrorReq t -> 
        buf_int16 buf 0;
        Error.write buf t
    | LoginReq t -> 
        buf_int16 buf 2;
        Login.write buf t
    | LoginAckReq t -> 
        buf_int16 buf 3;
        LoginAck.write buf t
    | VersionCheckReq t -> 
        buf_int16 buf 4;
        VersionCheck.write buf t
    | AutoUpgradeReq t -> 
        buf_int16 buf 5;
        AutoUpgrade.write buf t
    | NewUserLoginReq t -> 
        buf_int16 buf 6;
        NewUserLogin.write buf t
    | NickCheckReq t -> 
        buf_int16 buf 7;
        NickCheck.write buf t
    | NickUnusedReq -> 
        buf_int16 buf 8
    | NickAlreadyUsedReq -> 
        buf_int16 buf 9
    | NickInvalidReq  -> 
        buf_int16 buf 10        
    | AddFileReq t ->
        buf_int16 buf 100;
        AddFile.write buf t
    | SearchReq t -> 
        buf_int16 buf 200;
        Search.write buf t
    | SearchReplyReq t -> 
        buf_int16 buf 201;
        SearchReply.write buf t
    | EndOfSearchReplyReq -> 
        buf_int16 buf 202
    | DownloadRequestReq t ->
        buf_int16 buf 203;
        DownloadRequest.write buf t
    | DownloadAckReq t ->
        buf_int16 buf 204;
        DownloadAck.write buf t
    | PrivateMessageReq t ->
        buf_int16 buf 205;
        PrivateMessage.write buf t
    | DownloadErrorReq t ->
        buf_int16 buf 206;
        DownloadError.write buf t

    | BrowseUserReq t ->
        buf_int16 buf 211;
        BrowseUser.write buf t

    | BrowseUserReplyReq t ->
        buf_int16 buf 212;
        BrowseUserReply.write buf t

    | BrowseUserReplyEndReq ->
        buf_int16 buf 213
        
    | ServerStatsReq t ->
        buf_int16 buf 214;
        ServerStats.write buf t
    | GetServerStatsReq ->
        buf_int16 buf 214;
    | ResumeReq t ->
        buf_int16 buf 215;
        Resume.write buf t
    | ResumeReplyReq t ->
        buf_int16 buf 216;
        ResumeReply.write buf t
    | EndOfResumeReplyReq ->
        buf_int16 buf 217
    | AlternateDownloadRequestReq t ->
        buf_int16 buf 500;
        DownloadRequest.write buf t
       | MessageReq t ->
        buf_int16 buf 621;
        Message.write buf t
     
    | UnknownReq (msg_type, data) -> 
        buf_int16 buf msg_type;
        Buffer.add_string buf data;
        
        
  end
  
(*****************************************************************)
  
let opennap_handler f sock nread =
  let b = TcpBufferedSocket.buf sock in
  try
    while b.len >= 4 do
      let msg_len = get_int16 b.buf b.pos in
      if b.len >= 4 + msg_len then
        begin
          let msg_type = get_int16 b.buf (b.pos+2) in
          let data = String.sub b.buf (b.pos+4) msg_len in
          TcpBufferedSocket.buf_used b (msg_len + 4);
          let t = parse msg_type data msg_len in
(*          print t; *)
          f t sock
        end
      else raise Not_found
    done
  with Not_found -> ()

(*

5.  Client-Client Protocol

File transfer occur directly between clients without passing through the
server.  There are four transfer modes, upload, download, firewalled upload,
firewalled download.  The normal method of transfer is that the client
wishing to download a file makes a TCP connection to the client holding the
file on their data port.  However, in the case where the client sharing the
file is behind a firewall, it is necessary for them to "push" the data by
making a TCP connection to the downloader's data port.

5.1  Normal Downloading

Regardless of which mode, the downloading client will first issue either a 
search(200) or browse(211) command to the server.  This returns a list of
files and information on the client sharin the file.  To request a download,
a get(203) request is sent to the server.  The server will respond with
a get ack(204) containing more detailed information.

This is the point at which the different methods diverge.  If the 204 get
ack says that the remote clients data port is 0, you must send a 500 request
to the server requesting that the remote client send the data to you.  In
this case you wait for the remote client to connect to your own data port.

In the case where the sharing client is not firewalled, you make a TCP
connection to the data port specified in the 204 message from the server.
The remote client should accept the connection and immediately send one
ASCII char, `1' (ASCII 49).  Once you read this char, you send a request
for the file you wish to download.  First send the string "GET" in a single
packet, then send
        <mynick> "<filename>" <offset>
where <mynick> is your napster user name, <filename> is the file you wish to
download, and <offset> if the byte offst in the file to begin the transfer
at (if you are downloading for the first time, and not resuming a prior
transfer, you should uses 0 to start at the beginning of the file).

The remote client will then return the file size, or an error message such
as "INVALID REQUEST" or "FILE NOT SHARED".  Note that the file size is not
terminated in any special way, and the best way to figure out the size is to
keep reading until you hit a character that is not a digit (it will usually
be 0xff which is the start of the MP3 frame sync header, but if a ID3v2
tag is present it might look different).  Immediately following the file
size is where the data stream for the file begins.

Once the data transfer is initiated, the downloader should notify the server
that they are downloading a file by sending the 218 message.  Once the
transfer is complete, you send a 219 message to indicate you have finished
the download.  Note that this is cummalitive, so that if you are downloading
multiple files, you send one 218/219 pair for EACH concurrent download--this
is how the server knows how many transfers you have going on.  Likewise,
the uploader should send one 220 messge for each upload, and one 221 when
each upload has finished.

5.2  Firwalled Downloading

As described above, when the file needs to be pushed from a client behind a
firewall, the downloader sends a 500 message to the server.  This causes a
501 message to be sent to the uploader, which is similar to the 204 message
for a normal download.

Once the uploader receives the 501 message from the server, they should make
a TCP connection to the downloader's data port (given in the 501 message).
Upon connection, the downloader's client will sent one byte, the ASCII
character `1'.  The uploader should then send the string "SEND" in a single
packet, and then the information:
        <mynick> "<filename>" <size>
where <mynick> is the uploader's napster user name, <filename> is the file
being sent, and <size> is the size of the file in bytes.

Upon receipt, the downloading client will either send the byte offset at
whcih the transfer should start, or an error message such as
"INVALID REQUEST".  The byte offset should be sent as a single packet
in plain ASCII digits.  Just as with above in section 4.1, a 0 byte offset
indicates the transfer should begin at the start of the file.

Each client should notify the server that they are uploading or downloading
with the 218/219 (downloading) or 220/221 (uploading) command pairs (see
section 4.1 for more detailed information).


*)

open LittleEndian

let server_msg_to_string msg =
  Buffer.reset buf;
  buf_int16 buf 0;
  write buf msg;
  let s = Buffer.contents buf in
  let len = String.length s - 4 in
  str_int16 s 0 len;
  s
 
    
let server_send sock m =
  write_string sock (server_msg_to_string m)

let server_msg_to_string msg =
  Buffer.reset buf;
  buf_int16 buf 0;
  write buf msg;
  let s = Buffer.contents buf in
  let len = String.length s - 4 in
  let n = get_int16 s 2 in
  lprintf "SENDING %d [%s]" n (String.sub s 4 len) ; lprint_newline ();
  str_int16 s 0 len;
  s
 
    
let debug_server_send sock m =
  write_string sock (server_msg_to_string m)

(*
napster messages
================

by drscholl@users.sourceforge.net
April 7, 2000

0.  Foreward

This is meant to be an open specification.  If you find errors or know of
additional functionality not described hereafter, please send me email.  It
benefits the entire community to have a complete and accurate protocol
specification.  Not only does it allow for clients to be developed for any
platform, but also decreases the strain on the server having to parse out
bad client messages.

Disclaimer: The following information was gathered by analyzing the protocol
between the linux nap client and may not resemble the official Windows client
protocol.

1.  Recent Changes

* Section 3: 430-432 channel invite messages (2001-04-07)
* Section 3: WMA-FILE used as checksum for .wma files (2001-03-12)
* numeric 607 adds download client's speed (2000-12-28)
* added new numerics 640-642 for direct client to client browsing support
  (2000-12-23)

2.  Client-Server protocol

Napster uses TCP for client to server communication.  Typically the servers
run on ports 8888 and 7777.  Note that this is different from the `metaserver'
(or redirector) which runs on port 8875.

each message to/from the server is in the form of
        <length><type><data>
where <length> and <type> are 2 bytes each.  <length> specifies the length in
bytes of the <data> portion of the message.  Be aware that <length> and <type>
appear to be in little-endian format (least significant byte goes first).  For
example, in the C language you would encode the number 1 as
        const unsigned char num[2] = { 0x01, 0x00 };
and 256 would be encoded as
        const unsigned char num[2] = { 0x00, 0x01 };
[The above is for illustrative purposes only, there are much quicker ways to
actually encode a number. -ed]
The <data> portion of the message is a plain ASCII string.

Note that in many cases, strings are passed as double-quoted entries.  For
example, filenames and client id strings are always sent as
        "random band - generic cowboy song.mp3"
or
        "nap v0.8"
Where required, double quotes are used in the description of the messages
below.

Some additional information about use of quotes inside of quotes:
> The answer is, no, it doesn't do escaping of quotes.  If you try searching
> for the phrase 'a "quoted" string' on the windows client, you get no songs
> found, and "invalid search request" printed in yellow in your console
> window.  (don't know what code that is, sorry.)
>
> and no wonder-- a little birdie told me that the client sends this:
>
> FILENAME CONTAINS "a "quoted" string" MAX_RESULTS 100

[contributed by Ben Byer <bbyer@rice.edu>.  -ed]

Note that unlike the IRC protocol, each line does NOT end in \r\n.  The
<length> field specifies exactly how much data you should read.

3.  Message Types

The following section describes the format of the <data> section for each
specific message type.  Each field is denoted with <>.  The fields in a
message are separated by a single space character (ASCII 32).  Where
appropriate, examples of the <data> section for each message are given.

<type> can be one of the following (converted to big-endian):

0	error message [SERVER]

        Format: <message>

2	login [CLIENT]

        Format: <nick> <password> <port> "<client-info>" <link-type> [ <num> ]

        <port> is the port the client is listening on for data transfer.  if
                this value is 0, it means that the client is behind a firewall
                and can only push files outward.  it is expected that requests
                for downloads be made using the 500 message (see below)
        <client-info> is a string containing the client version info
        <link-type> is an integer indicating the client's bandwidth
                0  unknown
                1  14.4 kbps
                2  28.8 kpbs
                3  33.6 kbps
                4  56.7 kbps
                5  64K ISDN
                6  128K ISDN
                7  Cable
                8  DSL
                9  T1
                10 T3 or greater
        <num> build number for the windows client [optional]
                
        Example:

        foo badpass 6699 "nap v0.8" 3

3	login ack [SERVER]

        Format: <email>

        the server sends this message to the client after a succesful
        login (2).  If the nick is registered, the <email> address given at
        registration time is returned.  If the nick is not registered, a
        dummy value is returned.

4       version check [CLIENT]

        Format: <version>

        Server sends [5] if an update is needed. All other responses are
        ignored.

        <version> = string, version 2.0b5a sends "2.0"

5       "auto-upgrade" [SERVER]

        Format: <version> <hostname:filename>

        Napster is out of date, get a new version.
        Or also known as gaping security hole.

        <version>  = string, the new version number.
        <hostname> = string, hostname of upgrade (http) server
        <filename> = string, filename

        Connections are always made to port 80.

        The HTTP Request:
                GET <filename> HTTP/1.0
                Connection: Keep-Alive
                Host: <hostname>

        Expected HTTP Response.
                Content-Length: <size>
                Content-Type: <doesn't matter>
                <data>

        Upgrade file is save as "upgrade.exe".
        And executed as: upgrade.exe UPGRADE "<current.exe>"

        As far as I can tell no confirmation is requested by Napster when it
        receives this message.  And immediately start to "auto-upgrade". To keep
        users informed that Napster is doing something potentially very harmful
        to their computer it displays a message saying it's "auto-upgrading".

        I think this pretty bad, since all someone has to do is hack a napster
        server et voila a zillion clients at your disposal.

        As far as I known this only affects the Win32 2.0b5a Napster client.
        Other clients/versions I don't know.

        [This section was contributed by Thomas van der Heijden <thom@bart.nl>
        -ed]

6	new user login [CLIENT]

        Format: <nick> <pass> <port> "<client-info>" <speed> <email-address>

        This message is used when logging in for the first time to register
        a new nickname.  The client normally checks for an unused nick using
        the "check nickname" (7) message and upon receipt of a "nickname not
        registered" (8) from the server will proceceed to log in with this
        command.

        note: this message is similar to the 0x02 message, with the addition	
        of <email-address> on the end

        Example:

        foo foo 6699 "nap v0.8" 3 email@here.com

7	nick check [CLIENT]

        Format: <nick>

        Queries the server to see if <nick> is already registered.  This
        message is typically used prior to logging in for the first time to
        check for a valid nickname.

        Response to this message is one of 8, 9 or 10

8	nickname not registered [SERVER]

        Format: (empty)

        The server sends this message in response to the "nick check" (7)
        message to indicate that the specified nickname is not already
        registered and is ok to use.

9	nickname already registered [SERVER]

        Format: (empty)

        The server sends this message when the nickname the client has
        requested has already been registered by another user

10 (0x0a)	invalid nickname [SERVER]

        Format: (empty)

        This server sends this message in response to the "check nickname"
        (7) messages when the specified nickname is invalid, usually due to
        bad characters such as spaces or other non-printable characters.

        The Napster, Inc. servers only accept (upper- and lower-case) nicks
        comprised of the following:
               abcdefghijklmnopqrstuvwxyz1234567890_[]{}-@^!$

11	??? [CLIENT]

        Format: <nick> <pass>

        [returns "parameters are unparsable" -ed]

12	??? [SERVER]

        Format: (empty)

        this message is returned in response to message 11 from the client

13	echo??? [SERVER]

        Format: <numeric>: [args]

        Prior to issuing a login (2) command, the server will echo back the
        received numeric and any arguments of any commands it receives.

14	login options [CLIENT]

        NAME:%s ADDRESS:%s CITY:%s STATE:%s PHONE:%s AGE:%s INCOME:%s EDUCATION:%s

        Example:

        NAME: kev  ADDRESS:  CITY: ephrata  STATE: pa  PHONE:  AGE: 60 or older INCOME: $100,000 or more        EDUCATION: Graduate Degree

100 (0x64)	client notification of shared file [CLIENT]

        Format: "<filename>" <md5> <size> <bitrate> <frequency> <time>

        <md5> see section "MD5"
        <size> is bytes
        <bitrate> is kbps
        <frequency> is hz
        <time> is seconds
        
        Example:

        "generic band - generic song.mp3" b92870e0d41bc8e698cf2f0a1ddfeac7 443332 128 44100 60

102 (0x66)	remove file [CLIENT]

        Format: <filename>

        client requests to remove file from shared library

110		unshare all files [CLIENT]

        Format: (empty)

        Notifies the server that the client is no longer sharing any of the
        files previously shared.

200 (0xc8)	client search request [CLIENT]

    [FILENAME CONTAINS "artist name"] MAX_RESULTS <max> [FILENAME CONTAINS
"song"] [LINESPEED <compare> <link-type>] [BITRATE <compare> "<br>"] [FREQ
<compare> "<freq>"] [WMA-FILE] [LOCAL_ONLY]

        The artist name and the song name are, obviously, treated
                the same by the server; confirm this for yourself
                on the windows client.

        max is a number; if it is greater than 100, the server will
                only return 100 results.

        <compare> is one of the following:
                "AT LEAST" "AT BEST" "EQUAL TO"

        <link-type> see 0x02 (client login) for a description

        <br> is a number, in kbps

        <freq> is a sample frequency, in Hz

        LOCAL_ONLY causes the server to only search for files from users on
                the same server rather than all linked servers.

        The windows client filters by ping time inside the client.
                It pretty much has to, and it's easy to see the
                result by setting ping time to at best 100 ms or
                so, and max search terms to 50.  You'll get back
                like 3 results, but the client will still tell you
                that it found "50 results".

        Examples:
                FILENAME CONTAINS "Sneaker Pimps" MAX_RESULTS 75 FILENAME
                         CONTAINS "tesko suicide" BITRATE "AT LEAST" "128"

                MAX_RESULTS 100 FILENAME CONTAINS "Ventolin" LINESPEED
                        "EQUAL TO" 10

        [Thanks to Ben Byer <bbyer@rice.edu> for this contribution.  -ed]

201 (0xc9)	search response [SERVER]

        "<filename>" <md5> <size> <bitrate> <frequency> <length> <nick> <ip>
        <link-type> [weight]

        <md5> see secton "MD5"
        <size> is file size in bytes
        <bitrate> is mp3 bit rate in kbps
        <frequency> is sample rate in hz
        <length> is the play length of the mp3 in seconds
        <nick> the person sharing the file
        <ip> is an unsigned long integer representing the ip address of the
                user with this file
        <link-type> see message client login (2) message for a description
        [weight]	a weighting factor to allow the client to sort the
                        list of results.  positive values indicate a "better"
                        match, negative values indicate a "worse" match.
                        If not present, the weight should be considered to be
                        0.

        Example:

        "random band - random song.mp3" 7d733c1e7419674744768db71bff8bcd 2558199 128 44100 159 lefty 3437166285 4

202 (0xca)	end of search response from server [SERVER]

        Format: (empty)

203 (0xcb)	download request [CLIENT]

        Format: <nick> "<filename>"

        client requests to download <filename> from <nick>.  client expects
        to make an outgoing connection to <nick> on their specified data
        port.

        Example:

        mred "C:\Program Files\Napster\generic cowboy song.mp3"

        SEE ALSO: 500 alternate download request

204 (0xcc)	download ack [SERVER]

        <nick> <ip> <port> "<filename>" <md5> <linespeed>

        server sends this message in response to a 203 request.

        <nick> is the user who has the file
        <ip> is an unsigned long integer representing the ip address
        <port> is the port <nick> is listening on
        <filename> is the file to retrieve
        <md5> is the md5 sum
        <linespeed> is the user's connection speed (see login(2))

        Example:

        lefty 4877911892 6699 "generic band - generic song.mp3" 10fe9e623b1962da85eea61df7ac1f69 3

205 (0xcd)	private message to/from another user [CLIENT, SERVER]

        <nick> <message>

        note the same type is used for a client sending a msg or recieving one

        [Commentary: this message causes problems if you consider linking
        servers together.  With the current one server situation, the server
        just rewrites the message it receives with the name of the client that
        sent it and passes it to the recipient client.  However, in the case
        where the recipient and sender are not on the same server, there is
        loss of information without encapsulating it.  It would have been
        better to put both the sender and recipient because if the servers
        are ever linked they will have to make a new message type for this
        situation.  -ed]

206 (0xce)	get error [SERVER]

        <nick> "<filename>"

        the server sends this message when the file that the user has
        requested to download is unavailable (such as the user is not logged
        in).

207 (0xcf)	add hotlist entry [CLIENT]

        <user>

        This message is used to add additional entries to the client's
        hotlist.  The server will send 209 and 210 messages when a user
        on the hotlist has logged in or out, respectively.

208 (0xd0)	hotlist [CLIENT]

        <user>

        This message is used to send the initial list of hotlist entries
        during the initial login process.  It is normally send prior to
        to the add file (100) commands.  To add more entries to the hotlist
        after the initial list is sent, clients should use the 207 message
        instead.

209 (0xd1)	user signon [SERVER]

        <user> <speed>

        server is notifying client that a user in their hotlist, <user>,
        has signed on the server with link <speed>

210 (0xd2)	user signoff [SERVER]

        <user>

        server is notifying client that a user on their hotlist, <user>, has
        signed off the server.

        this message is also sent by the server when the client attempts to
        browse a nonexistent client.  [why don't they just use 404 for
        this? -ed]

211 (0xd3)	browse a user's files [CLIENT]

        <nick>

        the client sends this message when it wants to get a list of the files
        shared by a specific client

212 (0xd4)	browse response [SERVER]

        <nick> "<filename>" <md5> <size> <bitrate> <frequency> <time>

        <nick> is the user contributing the file
        <filename> is the mp3 file contributed
        <md5> is the has of the mp3 file
        <size> is the file size in bytes
        <bitrate> is the mp3 bitrate in kbps
        <frequence> is the sampling frequency in Hz
        <time> is the play time in seconds

        Example:

        foouser "generic band - generic song.mp3" b92870e0d41bc8e698cf2f0a1ddfeac7 443332 128 44100 60

213 (0xd5)	end of browse list [SERVER]

        <nick> [ip]

        indicates no more entries in the browse list for <user>.  <ip> gives
        the client's IP address.

214 (0xd6)	server stats [CLIENT, SERVER]

        client: no data
        server: <users> <# files> <size>

        <size> is approximate total library size in gigabytes
        this message is sent by the server occasionaly without request

        Example:

        553 64692 254

215 (0xd7)	request resume [CLIENT]

        <checksum> <filesize>

        client is requesting a list of all users which have the file with
        the characteristics.  the server responds with a list of 216 messages
        for each match, followed by a 217 message to terminate the list

216 (0xd8)	resume search response [SERVER]

        <user> <ip> <port> <filename> <checksum> <size> <speed>

        this message contains the matches for the resume request (215).  the
        list is terminated by a 217 message.

217 (0xd9)	end of resume search list [SERVER]

        no data.

        this messag terminates a list of 216 messages initiated by a 215
        client request

218 (0xda)	downloading file [CLIENT]

        no body.

        client sends this message to the server to indicate they are in the
        process of downloading a file.  this adds 1 to the download count
        which the server maintains.

219 (0xdb)	download complete [CLIENT]

        no body.

        client sends this message to the server to indicate they have
        completed the file for which a prior 218 message was sent.  this
        subtracts one from the download count the server maintains

220 (0xdc)	uploading file [CLIENT]

        no body.

        client sends this message to indicate they are uploading a file.
        this adds one to the upload count maintained by the server.

221 (0xdd)	upload complete [CLIENT]

        no body.

        client sends this message when they are finished uploading a file.
        this subtracts one from the upload count maintained by the server.

300 (0x12c)	optional ports [CLIENT]

        <port>

        This command allows the client to specify optional ports to try for
        data connections if the one currently in use is unreachable by other
        parties.

301 (0x12d)	hotlist ack [SERVER]

        <user>

        server is notifying client that <user> has successfully be added to
        their hotlist

302 (0x12e)	hotlist error [SERVER]

        <user>

        server is notifying client that it was unable to add <user> to their
        hotlist.  [can you only add registered nicks to your hotlist? -ed]

303 (0x12f)	remove user from hotlist [CLIENT]

        <user>

        client is notifying the server that it no longer wishes to request
        notifications about <user> when they sign on or off the server.  no
        response is sent in return.

310	??? [CLIENT, SERVER]

        client: no data
        server: 0

        unknown command.  server returns 0 regardless of any parameters

316	client disconnect??? [CLIENT, SERVER]

        client: no data
        server: 0

        The server sends this message with a value of `0' when the client is
        about to be disconnected.

        It is unkonwn what this command does when issued by the client.  The
        server appears to send the 316 message without disconnected the client
        in this case.

        [the server seems to send this message if you send it a numeric
        greater than 1000.  the server will also disconnect you after
        sending this message.  -ed]

320 (0x140)	user ignore list [CLIENT, SERVER]

        client: no data
        server: <count>

        client request to display the list of ignored users.
        server returns the number of users being ignored

321 (0x141)	user ignore list entry [SERVER]

        <user>

        server sends each ignored nick in response to a 320 request.  the
        list is terminated by a 320 message with the number of ignored users.

322 (0x142)	add user to ignore list [CLIENT, SERVER]

        <user>

        server acks the request by returning the nick

323 (0x143)	remove user from ignore list [CLIENT]

        <user>

        server acks the request by returning the nick to be removed from
        the ignore list.

324 (0x144)	user is not ignored [SERVER]

        <user>

        server indicates that <user> is not currently ignored in response to
        a 323 request.

325 (0x145)	user is already ignored [SERVER]

        <user>

        server indicates the specified user is already on the user's ignore
        list

326 (0x146)	clear ignore list [CLIENT, SERVER]

        client: no data.
        server: <count>

        client requests the server clear its ignore list.  server returns the
        number of entries removed from the ignore list.

330 (0x14a)	blocked ip list [CLIENT]

332 (0x14c)	block ip [CLIENT]

333 (0x14d)	unblock ip [CLIENT]

400 (0x190)	join channel [CLIENT]

        <channel-name>

        the client sends this command to join a channel

401 (0x191)	part channel [CLIENT, SERVER]

        <channel-name>

        The client sends this command to part a channel.  Server sends this
        message to indicate that the client has parted the channel.  Note
        that this is sometimes sent even when the client has not requested,
        so a client _must_ be prepared to accept it at any time.

402 (0x192)	send public message [CLIENT]

        <channel> <message>

403 (0x193)	public message [SERVER]

        <channel> <nick> <text>

        this message is sent by the server when a client sends a public message
        to a channel.

        Example:

        80's espinozaf hello...hola

404 (0x194)	error message [SERVER]

        <text>

        This message is used to send general error messages to a client that
        is logged in.  Note: Message 0 is only sent during the login process.

        Examples:

        User nosuchuser is not currently online.
        Channel #nosuchchannel does not exist!
        permission denied
        ping failed, blah is not online

405 (0x195)	join acknowledge [SERVER]

        <channel>

        the server sends this message to the client to acknowlege that it
        has joined the requested channel (400)
        
406 (0x196)	join message [SERVER]

        <channel> <user> <sharing> <link-type>

        <user> has joined <channel>

        Example:

        80's WilmaFlinstone 12 2

407 (0x197)	user parted channel [SERVER]

        <channel> <nick> <sharing> <linespeed>

        Example:

        80's DLongley 23 7

408 (0x198)	channel user list entry [SERVER]

        this message is identical to the join (406) message.  the server will
        send the list of users in the channel prior to the client join command
        in this message.  joins that occur after the client has joined will
        be noted by a 406 message.

409 (0x199)	end of channel user list [SERVER]

        <channel>

        this message is sent by the server to indicate it has sent all informati	about the users in a channel

410 (0x19a)	channel topic [CLIENT, SERVER]

        <channel> <topic>

        sent when joining a channel or a new topic is set.  a client requesting
        topic change also uses this message.

        [why didn't they put a field to indicate WHO changed the topic?  as
        it is now you can only tell that it was changed.  -ed]

420 (0x1a4)	channel ban list [CLIENT, SERVER]

        Format: <channel>

        This command is used by clients to request the list of channel bans,
        and by the server to indicate the end of the channel ban list for
        a channel (also see 421).

421 (0x1a5)	channel ban list entry [SERVER]

422 (0x1a6)	channel ban [CLIENT]

        <channel> <user|ip> [ "<reason>" ]

423 (0x1a7)	channel unban [CLIENT]

        <channel> <user|ip> [ "<reason>" ]

424 (0x1a8)	channel ban clear [CLIENT]

        Format: <channel>

425	channel motd (alternate topic (?)) [SERVER]

        Format: <message>

        [The server sends this command when a user joins one of the
        predefined channels.  It seems to send a default message if the
        topic for the channel is the default, otherwise it sends the current
        channel topic.  -ed]


430	invite a user [CLIENT, SERVER]                                                 
        client: <nick> <channel>                                               
        server: <nick> <channel> "<topic>" <unknown_digit> <unknown_text>       
                                                                                
        This command is used by Napster 2.0 BETA 9.6 to invite a user to        
        a channel.                                                              
        client:                                                                 
                <nick> - nick of invited user                                   
                <channel> - channel user <nick> was invited to                  
        server:                                                                 
                <nick> - nick of user who was inviting                          
                <channel> - channel                                             
                <topic> - channel's topic                                       
                <unknown_digit> - ??? ("0" works fine)                          
                <unknown_text> - ??? ("Hello!" works fine)                      
                                                                                
                Last two arguments i cannot check because i temporary           
                don't have internet access (e-mail only). If someone            
                will test this message on napster.com servers please            
                send me results.                                                
                                                                                
        When user receives 430 message user should reply with 431 or 432.       
                                                                                
        Client example:                                                         
                                                                                
        CyberAlien2 #test_channel                                               
                                                                                
        Server  example:                                                        
                                                                                
        CyberAlien3 #test_channel "Welcome to the #test_channel." 0 Hello!      
                                                                                
        !!! If anyone known what should server reply instead of "0 Hello!"      
        please tell me.                                                         
                                                                                
                                                                                
431	invitation accepted [CLIENT]                                                    
        <user> <channel>                                                        
                                                                                
        <user> - user who was inviting                                          
                                                                                
432	invitation declined [CLIENT]

        format: copy of command received in 430 message                         

500 (0x1f4)	alternate download request [CLIENT]

        <nick> "<filename>"

        requests that <nick> make an outgoing connection to the requesters
        client and send <filename>.  this message is for use when the
        person sharing the file can only make an outgoing tcp connection
        because of firewalls blocking incoming messages.  this message should
        be used to request files from users who have specified their data
        port as 0 in their login message

501 (0x1f5)	alternate download ack [SERVER]

        <nick> <ip> <port> "<filename>" <md5> <speed>

        this message is sent to the uploader when their data port is set to
        0 to indicate they are behind a firewall and need to push all data
        outware.  the uploader is responsible for connecting to the
        downloader to transfer the file.

600 (0x258)	request user's link speed [CLIENT]

        <nick>

601 (0x259)	link speed response [SERVER]

        <nick> <linespeed>

602 (0x25a)	??? [CLIENT]

        <?>

        server returns a 404 with "*gulp* Drink milk, it does a body good!"

603 (0x25b)	whois request [CLIENT]

        <nick>

604 (0x25c)	whois response [SERVER]

        <nick> "<user-level>" <time> "<channels>" "<status>" <shared>
        <downloads> <uploads> <link-type> "<client-info>" [ <total downloads>
        <total_uploads> <ip> <connecting port> <data port> <email> ]

        <user-level> is one of "User", "Moderator", "Admin" or "Elite"
        <time> is seconds this user has been connected
        <channels> is the list of channels the client is a member of, each
                separated by a space (ASCII 32)
        <status> is one of "Active", "Inactive" (offline) or "Remote" (on a
                different server)
        <shared> is number of files user has available for download
        <downloads> is the current number of downloads in progress
        <uploads> is the current number of uploads in progress
        <link-type> see 0x02 (client login) above
        <client-info> see 0x02 (client login) above

        The following fields are displayed for user level moderator and
        above:

        <total uploads>
        <total downloads>
        <ip>			note: can be "unavailable"
        <connecting port>
        <data port>
        <email>			note: can be unavailable

        Example:

        lefty "User" 1203 "80's " "Active" 0 0 0 3 "nap v0.8"

605 (0x25d)	whowas response [SERVER]

        <user> <level> <last-seen>

        if the user listed in a 603 request is not currently online, the
        server sends this message.

        <user> is the user for which information was requested
        <level> is the user's last known userlevel (user/mod/admin)
        <last-seen> is the last time at which this user was seen, measured
                as seconds since 12:00am on January 1, 1970 (UNIX time_t).

606 (0x25e)	change user level [CLIENT]

        <nick> <level>

        changes the privileges for <nick> to <level>.  client must be admin
        level to execute this request

        [I have not verified this message since I don't have admin status
        on any of the servers.  -ed]

607 (0x25f)	upload request [SERVER]

        <nick> "<filename>" <speed>

        this message is used to notify the client that user <nick> has
        requested upload of <filename>

        Example:

        lefty "generic band - generic song.mp3" 7

608 (0x260)	accept upload request [CLIENT]

        <nick> "<filename>"

        client is notifying server that upload of <filename> to <nick> is
        accepted, and that the requesting client may begin download

        Example:

        lefty "generic band - generic song.mp3"

609 (0x261)     accept failed [SERVER]

        <nick> "<filename>"

        this message is sent by the server when the client that the file was
        requested from does not accept the upload request.

610 (0x262)	kill (disconnect) a user [CLIENT]

        <nick> [ "<reason>" ]

        client request to disconnect a user.

611 (0x263)	nuke a user [CLIENT]

        <nick>

        client request to delete account for <nick>

612 (0x264)	ban user [CLIENT]

        <nick | ip> [ "<reason>" ]

        client request to place a ban on either a specific nickname or
        an ip address

613 (0x265)	set data port for user [CLIENT, SERVER]

        client: <user> <port>
        server: <port>

        This command is used by administrators to request that another
        client set the port used for data transfers to <port>.  The server
        sends a message with the requested port number to the target
        client.  NOTE: the target client can change its port number to
        whatever it wishes using the 703 command.

614 (0x266)	unban user [CLIENT]

        Format: <nick | ip> [ "<reason>" ]

615 (0x267)	show bans for server [CLIENT]

        Format: (empty)

        client requests the list of banned ips for the current server

616 (0x268)	(ip?) ban list entry [SERVER]

        Format: <ip> <nick> "<reason>" <time> <n>

        <ip> is the string version of the ip banned
        <nick> is the user setting the ban
        <reason> is the reason given
        <time> is the time_t when the ban was set
        <n> is ???.  value is either 0 or 1

        This message is sent in response to the 615 client request, one
        for each ban.  The list is terminated with a 0 length 615 message
        from the server.

        Example:
        
        207.172.245. valkyrie "DoS exploit" 947304224 0

617 (0x269)	list channels [CLIENT, SERVER]

        Format: (empty)

        client requests a list of channels on the server.  server responds
        with 618/617

        server indicates end of channel list using this message.

618 (0x26a)	channel list entry [SERVER]

        Format: <channel-name> <number-of-users> <topic>

        this is the server response to a 617 client request, one for each
        channel.

        Example:

        Help 50 OpenNap help channel

619 (0x26b)	queue limit [CLIENT]

        Format: <nick> "<filename>" <n>

        a client may limit the number of downloads from a particular client.
        once the limit for a particular user has been reached, the uploading
        client can send this message to notify the downloader that they
        have hit the limit and can't have any more simultaneous downloads.
        <nick> is the user who hit the limit, <filename> is the file they
        were trying to download when they hit the limit, and <n> is the
        number of simultaneous downloads allowed.

        Example:

        joebob "C:\MP3\Generic Band - Generic Song.mp3" 3

620 (0x26c)	queue limit [SERVER]

        <nick> "<filename>" <filesize> <digit>

        This message is sent by the server in response to the 619 client
        request, when one user needs to notify another that they have
        reached the maximum allowed simultaneous downloads.  When the server
        recieves the 619 request from the uploading client, it sends the 620
        message to the downloading client.  The only difference in format is
        the addition of the <nick> field in the 620 message which specifies
        the username of the uploading agent which is notifying the
        downloader that the queue is full.

        Example:

        joebob "C:\MP3\Generic Band - Generic Song.mp3" 1234567 3

621 (0x26d)	message of the day [CLIENT, SERVER]

        <text>

        Server: each 621 message contains a single line of text

        Client: client sends a 621 command with no data to request the
        motd be sent.  The server will usually send this automatically after
        a user logs in, so this command allows a user to reread it upon
        request.

622 (0x26e)	muzzle a user [CLIENT]

        <nick> [ "<reason>" ]

        client requests that <nick> not be allowed to send public messages

623 (0x26f)	unmuzzle a user [CLIENT]

        <nick> [ "<reason>" ]

        client requests that the enforced silence on <nick> be lifted

624 (0x270)	un-nuke a user [CLIENT]

        <user>

625 (0x271)	change a user's linespeed [CLIENT]

        <user> <speed>

626 (0x272)	data port error [CLIENT, SERVER]

        <user>

        When a downloading client detects that the uploader's data port
        is unreachable, it should send a 626 message to the server with
        the nick of the user for which the connection failed.  The server
        then relays the message to the uploader, substituing the
        downloader's nickname in the message.

627 (0x273)	operator message [CLIENT, SERVER]

        client: <text>
        server: <nick> <text>

        client request to send a message to all admins/moderators

628 (0x274)	global message [CLIENT, SERVER]

        client: <text>
        server: <nick> <text>

        client request send a message to all users

629 (0x275)	banned users [SERVER]

        Format: <nick>

        when displaying the ban list for the server, this message is used
        to indicate banned nicknames.

630-639 missing

640	direct browse request [CLIENT, SERVER]

        Client: <nick>
        Server: <nick> [ip port]

        Client: request files for <nick>
        Server: <nick> is requesting your files.  optionally, <ip> and
                <port> are given if the client getting browsed is firewalled

        This message is sent to initiate a direct client to client browsing of
        shared files.

        See section 5.3.

641	direct browse accept [CLIENT, SERVER]

        Client: <nick>
        Server: <nick> <ip> <port>

        The client to be browsed sends this message back to the server to
        indicate it is willing to accept a direct browse from <nick>.

        The server sends this message to the requestor of the browse to
        indicate where it should connect in order to do a direct browse from
        <nick>.

        See section 5.3

642	direct browse error [SERVER]

        <nick> "message"

        Server sends this messags in response to a 640 request if there is an
        error (eg. both clients are firewalled, or the browsee is not sharing
        any files).

        See section 5.3

643-649	missing

650-651	??? [CLIENT]

        permission denied.

652 (0x28c)	cloak user [CLIENT]

        sets the current user to "invisible"

653-699	missing.

700	change link speed [CLIENT]

        <speed>

        client is notifying server that its correct link speed is <speed>,
        in the range 0-10 (see the login message for details).

701	change user password [CLIENT]

        <password>

        client wishes to change their password

702	change email address [CLIENT]

        <email address>

        client wishes to change their email address

703	change data port [CLIENT]

        <port>

        client is changing the data port being listened on for file
        transfers

704-747	missing.

748	login attempt [SERVER]

        the server sends this message to a logged in client when another
        client attempts to log in with the same nickname.

749	missing.

750 (0x2ee)	server ping [CLIENT, SERVER]

        server: none
        client: <user>

        Napster 2.0b5a sends the username in a response to a 750 from the
        server.

        [server returns an empty 750 command in response. server ping? -ed]

751 (0x2ef)	ping user [CLIENT, SERVER]

        <user>

        client is attempting to determine if <user>'s connection is alive

752 (0x2f0)	pong response [CLIENT, SERVER]

        <user>

        this message is sent in response to the the 751 (PING) requeset

753 (0x2f1)	change password for another user [CLIENT]

        <user> <password> "<reason>"

        allows an administrator to change the password for another user

754-769	missing.

770	??? [CLIENT]

        permission denied.

771	??? [CLIENT]

        permission denied.

772-799	missing.

800 (0x320)	reload config [CLIENT]

        <config variable>

        resets configuration parameter to its default value

801 (0x321)	server version [CLIENT]

        no data.

        client request's a server's version

802-804	missing.

805	???

        [returns permission denied. -ed]

810 (0x32a)	set config [CLIENT]

        <config string>

        request a change in server configuration variables

811 (0x32b)	???	[CLIENT]

        [returns permission denied. -ed]

820 (0x334)	clear channel [CLIENT]

        <channel>

        remove all users from the specified channel

821 (0x335)	redirect client to another server [CLIENT, SERVER]

        client: <user> <server> <port>
        server: <server> <port>
        
        This command allows an administrator to redirect clients to another
        server.

822 (0x336)	cycle client [CLIENT, SERVER]

        client: <user> <metaserver>
        server: <metaserver>

        This commands allows an administrator to make a client restart the
        login process by first connecting to the metaserver (redirector) at
        <host>.

823 (0x337)	set channel level [CLIENT]

        <channel> <level>

        Sets <channel> such that users must be at least <level> in order to
        enter the channel

824 (0x338)	emote [CLIENT, SERVER]

        client: <channel> "<text>"
        server: <channel> <user> "<text>"

        A variation of the public message command to indicate an action by
        the user.  Often implemented as the "/me" command in IRC clients.

825 (0x339)	user list entry [SERVER]

        <channel> <user> <files shared> <speed>

        an 825 message is sent for each user in the channel specified by the
        830 message

        Example:

        Help testor3 0 3

        [This appears to be exactly the same format as the 408 message.  -ed]

826 (0x33a)	channel limit [CLIENT]

        <channel> <limit>

        sets the maximum number of users that may enter a channel.  <limit>
        is an integer value between 0 and 999999999.  setting it higher that
        this results in the limit being set to -1.  by default, created
        channels have a limit of 200.  there appears to be no restriction on
        any channel member changing the limit, and no notification is given

827 (0x33b)	show all channels [CLIENT, SERVER]

        no data.

        client request to show all channels, including "hidden" channels.
        the server also sends this message to terminate

828 (0x33c)	channel list [SERVER]

        <channel> <users> <n1> <level> <limit> "<topic>"

        the server sends a list of 828 commands, one for each channel,
        including "hidden" channels that don't show up in the normal channel
        list.

        <level> is the mimimum user level required for entry into a channel
        <limit> is the max number of users allowed in a channel

        <n1> is either 0 or 1.  seems to be 1 if the channel was user
        created, or 0 if a predefined channel???

829 (0x33d)	kick user from channel [CLIENT]

        <channel> <user> [ "<reason>" ]

830 (0x33e)	list users in channel [CLIENT, SERVER]

        <channel>

        client requests a list of all users in <channel>.  server responds
        with a 825 response for each user, followed by an 830 response with
        no data [why didn't they just use the 409 message? -ed]

831 (0x33f)	global user list [CLIENT]

        [returns permission denied.  -ed]

832-869	missing.

870	add files by directory [CLIENT]

        Format: "<directory>" "<file>" <md5> <size> <bitrate> <freq> <duration>
        [ ... ]

        This command allows a client to share multiple files in the same
        directory as a shortcut to multiple 100 (share file) commands.
        <directory> is the path which should be prepended to all the given
        filenames in the rest of the command.  <file> is the name of the file
        to share *without* its directory component.  When other clients do a
        browse/share, the real path will be reported as <directory>/<file>.

        The portion of this command after the <directory> may be repeated as
        many times as necessary for files in the same directory. NOTE: most
        servers will not accept commands of length longer than 2048 bytes
        so you still may need to break up the files into multiple commands
        if there are many files in a single directory.

871-899	missing.

900     connection test [SERVER]

        <ip> <port> <data>

        <ip>   - string,  ip address to connect to.
        <port> - integer, port to connect to
        <data> - string,  data to send to server.

        Try to connect to <ip> on <port> for atmost 1000 seconds.  If the
        connection succeeds send the <data> to target.

        [reported by Thomas van der Heijden <thom@bart.nl>]

901     listen test [SERVER]

        <port> <timeout> <data>

        <port>    - integer, port to listen on
        <timeout> - integer, time to wait for connection in seconds
        <data>    - string,  data to send after connection has been made.

        Listen on <port> for <timeout> seconds. If a connection arrives,
        return <data> to sender.

        [reported by Thomas van der Heijden <thom@bart.nl>]

920	??? [CLIENT]

        Format: 1

        This is sent by the BETA8 client prior to login.  Purpose unknown.

3.  MD5

It looks like the vast majority of the files are hashed using the first
299,008 bytes of the file.  There have been some cases where the hash
matches at 300,032 bytes, but no correlation has been drawn as to when that
happens.  The speculation at this point is that it might have to do with
the existence of a ID3v2 tag, or perhaps the file was sampled at 48kHz...?

The current method seems to be: skip id3v2, seek to frame sync and hash.

Note: the linux nap client (versions 0.7 - 0.9) seem to hash exactly 300,000
bytes, which is NOT what the official windows client does.

More recent Napster clients seem to send the string WMA-FILE as the checksum
instead of the actual hash value for .wma files.

5.  Client-Client Protocol

File transfer occur directly between clients without passing through the
server.  There are four transfer modes, upload, download, firewalled upload,
firewalled download.  The normal method of transfer is that the client
wishing to download a file makes a TCP connection to the client holding the
file on their data port.  However, in the case where the client sharing the
file is behind a firewall, it is necessary for them to "push" the data by
making a TCP connection to the downloader's data port.

5.1  Normal Downloading

Regardless of which mode, the downloading client will first issue either a 
search(200) or browse(211) command to the server.  This returns a list of
files and information on the client sharin the file.  To request a download,
a get(203) request is sent to the server.  The server will respond with
a get ack(204) containing more detailed information.

This is the point at which the different methods diverge.  If the 204 get
ack says that the remote clients data port is 0, you must send a 500 request
to the server requesting that the remote client send the data to you.  In
this case you wait for the remote client to connect to your own data port.

In the case where the sharing client is not firewalled, you make a TCP
connection to the data port specified in the 204 message from the server.
The remote client should accept the connection and immediately send one
ASCII char, `1' (ASCII 49).  Once you read this char, you send a request
for the file you wish to download.  First send the string "GET" in a single
packet, then send
        <mynick> "<filename>" <offset>
where <mynick> is your napster user name, <filename> is the file you wish to
download, and <offset> if the byte offst in the file to begin the transfer
at (if you are downloading for the first time, and not resuming a prior
transfer, you should uses 0 to start at the beginning of the file).

The remote client will then return the file size, or an error message such
as "INVALID REQUEST" or "FILE NOT SHARED".  Note that the file size is not
terminated in any special way, and the best way to figure out the size is to
keep reading until you hit a character that is not a digit (it will usually
be 0xff which is the start of the MP3 frame sync header, but if a ID3v2
tag is present it might look different).  Immediately following the file
size is where the data stream for the file begins.

Once the data transfer is initiated, the downloader should notify the server
that they are downloading a file by sending the 218 message.  Once the
transfer is complete, you send a 219 message to indicate you have finished
the download.  Note that this is cummalitive, so that if you are downloading
multiple files, you send one 218/219 pair for EACH concurrent download--this
is how the server knows how many transfers you have going on.  Likewise,
the uploader should send one 220 messge for each upload, and one 221 when
each upload has finished.

5.2  Firwalled Downloading

As described above, when the file needs to be pushed from a client behind a
firewall, the downloader sends a 500 message to the server.  This causes a
501 message to be sent to the uploader, which is similar to the 204 message
for a normal download.

Once the uploader receives the 501 message from the server, they should make
a TCP connection to the downloader's data port (given in the 501 message).
Upon connection, the downloader's client will sent one byte, the ASCII
character `1'.  The uploader should then send the string "SEND" in a single
packet, and then the information:
        <mynick> "<filename>" <size>
where <mynick> is the uploader's napster user name, <filename> is the file
being sent, and <size> is the size of the file in bytes.

Upon receipt, the downloading client will either send the byte offset at
whcih the transfer should start, or an error message such as
"INVALID REQUEST".  The byte offset should be sent as a single packet
in plain ASCII digits.  Just as with above in section 4.1, a 0 byte offset
indicates the transfer should begin at the start of the file.

Each client should notify the server that they are uploading or downloading
with the 218/219 (downloading) or 220/221 (uploading) command pairs (see
section 4.1 for more detailed information).

5.3 Client to Client Browsing

Napster 2.0 BETA 8 adds a feature which allows direct client-to-client
browsing of file lists.  To request a browse, a client uses the
        640 <nick>
command.  The server then sends a
        640 <requester>
to the client which is getting browsed with the nick of the client that is
requesting the browse.  If the client accepts the browse request, it sends
back a
        641 <requestor>
to the server with the nick of the client requesting the browse.  The
server then sends a
        641 <nick> <ip> <port>
to the requesting client.  In the case of an error, the server will send a 642
command in response to the 640 command.

The browsing client then makes a TCP conection to the remote client's data
port.  After getting the "1" character, the browsing client sends a
        GETLIST
At which point the remote client sends its nick followed by a linefeed (\n)
by itself in a single packet (ie, one write() call)
        <nick> LF
followed by the list of files being shared (the format being the same as
the data of the "share" command).  Each line is terminated by a linefeed char.
At the end of the list, an additional linefeed char is sent and then the
client closes the connection.

In the case that the remote client is firewalled, the browse list will have to
be pushed to the requesting client.  The remote client makes a TCP connection
to the requesting client, then sends
        SENDLIST <nick>\n
followed by the list of files, as with the "GETLIST" command response.

6.  Where to get more help?

Join the napdev mailing list by sending email to napdev-subscribe@onelist.com
or by visiting the community page http://www.onelist.com/community/napdev/.
This list is designed for open source napster developers to share information
about the specification or applications.

7.  Acknowledgements

A big THANKS goes to the following people who contributed valuable information
to this specification:

Ben Byer <bbyer@rice.edu>
JT <jtraub@dragoncat.net>
Evan Martin <eeyem@u.washington.edu>
Colten Edwards (aka panasync@efnet) <edwards@bitchx.dimension6.com>
Thomas van der Heijden <thom@bart.nl>


*)

