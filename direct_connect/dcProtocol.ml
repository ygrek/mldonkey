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

open CommonGlobals
open DcGlobals
open DcTypes
open TcpBufferedSocket


module Empty = functor(M: sig val msg : string end) -> 
    struct
    
      let parse s = ()
      
      let print t =
        Printf.printf "message %s" M.msg
        
      let write buf t = ()
    end

module Empty2 = functor(M: sig val msg : string end) -> 
    struct
    
      let parse s = ()
      
      let print t =
        Printf.printf "message %s" M.msg
        
      let write buf t = Printf.bprintf buf "$%s" M.msg
    end

module SimpleNick =  functor(M: sig val msg : string end) -> 
    struct
    type t = string
      
      let parse nick = nick 
      
      let print t = 
        Printf.printf "%s [%s]" M.msg (String.escaped t) ;
        print_newline () 
      
      let write buf t = 
        Printf.bprintf buf " %s" t
    
  end

module SimpleNick2 =  functor(M: sig val msg : string end) -> 
    struct
    type t = string
      
      let parse nick = nick 
      
      let print t = 
        Printf.printf "%s [%s]" M.msg (String.escaped t) ;
        print_newline () 
      
      let write buf t = 
        Printf.bprintf buf "$%s %s" M.msg t
    
  end

module Lock = struct
    type t = {
        key : string;
        info : string;
      }
      
    let parse s = 
      match String2.splitn s ' ' 1 with
      | [key; info] -> { key = key; info = info }
      | _ -> assert false
      
    let print t = 
      Printf.printf "LOCK [%s] %s" (String.escaped t.key) 
      (String.escaped t.info);
      print_newline () 
      
    let write buf t = 
      Printf.bprintf buf " %s %s" t.key t.info
  end
  
type direction = Upload | Download
  
module Direction = struct
    type t = {
        direction : direction;
        level : int;
      }
      
    let parse s = 
      match String2.split s ' ' with
      | ["Download"; level] -> { 
            direction = Download; 
            level = int_of_string level }
      | ["Upload"; level] -> { 
            direction = Upload; 
            level = int_of_string level }
      | _ -> assert false
      
    let print t = 
      Printf.printf "Direction %s %d" (
        match t.direction with
          Download -> "Download" 
        | Upload -> "Upload") t.level;
      print_newline () 
      
    let write buf t = 
      Printf.bprintf buf "$Direction %s %d" (match t.direction with
          Download -> "Download" 
        | Upload -> "Upload") t.level
    
  end
  
module Get = struct
    type t = {
        name : string;
        pos : int64;
      }
      
    let parse s = 
      let len = String.length s in
      let pos = String.rindex s '$' in
      {
        name = String.sub s 0 pos; 
        pos = Int64.of_string (String.sub s (pos+1) (len-pos-1));
      }
      
    let print t = 
      Printf.printf "Get [%s] %Ld" (String.escaped t.name) 
      t.pos;
      print_newline () 
      
    let write buf t = 
      Printf.bprintf buf "$Get %s$%Ld" t.name t.pos
    
  end
  
module FileLength = struct
    type t = int64
      
    let parse s = Int64.of_string s
      
    let print t = 
      Printf.printf "FileLength %Ld" t;
      print_newline () 
      
    let write buf t = 
      Printf.bprintf buf "$FileLength %Ld" t
    
  end


module Key = struct
    type t = {
        key : string;
      }
      
    let parse key = { key = key }
      
    let print t = 
      Printf.printf "KEY [%s]" (String.escaped t.key) ;
      print_newline () 
      
    let write buf t = 
      Printf.bprintf buf " %s" t.key
    
  end


module To = struct
    type t = {
        dest : string;
        orig : string;
        message : string;
      }
      
    let parse s = 
      match String2.splitn s ' ' 3 with
      | [dest; "From:"; orig; m] -> 
          let m = String.sub m 1 (String.length m - 1) in
          { dest = dest; orig = orig; message = m }
      | _ -> assert false
      
    let print t = 
      Printf.printf "To %s FROM %s: %s" t.dest t.orig t.message;
      print_newline () 
      
    let write buf t = 
      Printf.bprintf buf " %s From: %s $%s" t.dest t.orig t.message
    
  end

module Search = struct
    type t = {
        orig : string;
        sizelimit : sizelimit;
        filetype : int;
        words : string;
      }
    
    let parse s = 
(*      Printf.printf "SEARCH: [%s]" (String.escaped s); print_newline (); *)
      let list = String2.split_simplify s ' ' in
      let rec iter before after =
        match after with
          [] -> assert false
        | s :: tail ->
            if String.length s >= 8 && s.[1] = '?' && s.[3] = '?' then
              before, String2.unsplit after '$'
            else
              iter (Printf.sprintf "%s %s" before s) tail
      in
      let (orig, search) = iter "" list in
      begin
        match String2.splitn search '?' 4 with
          [has_size; size_kind; size; filetype; words] ->
            String2.replace_char words '$' ' ';
            let size = 
              match has_size, size_kind with
                "T", "T" -> AtMost (Int64.of_float (float_of_string size))
              |  "T", "F" -> AtLeast (Int64.of_float (float_of_string size))
              | _ -> NoLimit
            in
            {
              orig = orig;
              sizelimit = size;
              filetype = int_of_string filetype;
              words = words;
            } 
        | _ -> assert false
      end
      
    let print t = begin
      match t.sizelimit with
      | AtLeast n ->
          Printf.printf "Search %s TYPE %d FOR %s of at least %Ld" 
            t.orig t.filetype t.words n
      | AtMost n ->
          Printf.printf "Search %s TYPE %d FOR %s of at most %Ld" 
            t.orig t.filetype t.words n
      | NoLimit ->
          Printf.printf "Search %s TYPE %d FOR %s" 
            t.orig t.filetype t.words
      end;          
      print_newline () 
      
    let write buf t = 
      Printf.bprintf buf " %s %c?%c?%s?%d?%s"
        t.orig 
        (if t.sizelimit = NoLimit then 'F' else 'T')
      (match t.sizelimit with
          AtMost _ -> 'T'        | _ -> 'F')
      (match t.sizelimit with
          AtMost n -> Int64.to_string n
        | AtLeast n -> Int64.to_string n
        | _  -> "0")
      t.filetype
        (let s = String.copy t.words in 
        String2.replace_char s ' ' '$'; s)
    
  end
    
module HubName = struct
    type t =  string
      
    let parse name = name 
      
    let print t = 
      Printf.printf "HUB NAME [%s]" (String.escaped t) ;
      print_newline () 
      
    let write buf t = 
      Printf.bprintf buf " %s" t
    
  end
    
module NickList = struct
    type t = string list
      
      let parse users = String2.split_simplify users '$' 
          
    let print t = 
      Printf.printf "NICK LIST "; 
      List.iter (fun s -> Printf.printf "%s " s) t;
      print_newline () 
      
    let write buf t = 
      Buffer.add_char buf ' ';
      List.iter (fun s -> Printf.bprintf  buf "%s$$" s) t
    
  end
    
module OpList = struct
    type t =  string list

      
      let parse users = String2.split_simplify users '$' 
          
    let print t = 
      Printf.printf "OP LIST "; 
      List.iter (fun s -> Printf.printf "%s " s) t;
      print_newline () 
      
    let write buf t = 
      Buffer.add_char buf ' ';
      List.iter (fun s -> Printf.bprintf  buf "%s$$" s) t
    
  end

let char5 = char_of_int 5
  
module SR = struct
    type t = {
        owner : string;
        filename : string;
        filesize : int64;
        open_slots : int;
        all_slots : int;
        server_name : string;
        server_ip : string option;
        to_nick : string option;
      }
    
    let parse s = 
      let owner_and_filename , size_and_slots, server_info =
        match String2.split s char5 with
          [owner_and_filename; size_and_slots; server_info] -> 
            owner_and_filename , size_and_slots, server_info
        | [ owner_and_filename; server_info] ->
            
            let pos = String.rindex owner_and_filename ' ' in
            let len = String.length owner_and_filename in
            let size_and_slots = Printf.sprintf "0 %s" 
                (String.sub owner_and_filename (pos+1) (len - pos - 1))
            in
            let owner_and_filename = String.sub owner_and_filename 0 pos in
            owner_and_filename, size_and_slots, server_info
        | _ -> assert false
      in
      begin
        match String2.splitn owner_and_filename ' ' 1 with
          [owner; filename] -> begin
              match String2.splitn size_and_slots ' ' 1 with
                [size; slots] -> begin
                    match String2.splitn slots '/' 1 with
                      [open_slots; all_slots] -> begin
                          match String2.split server_info ' ' with
                            server_name :: server_tail ->
                              {
                                owner = owner;
                                filename = filename;
                                filesize = Int64.of_string size;
                                open_slots = int_of_string open_slots;
                                all_slots = int_of_string all_slots;
                                server_name = server_name;
                                to_nick = Some "not_implemented";
                                server_ip = match server_tail with
                                  [] -> None
                                | [server_ip] ->
                                    let len = String.length server_ip in
                                    if len > 2 then
                                      Some (String.sub server_ip 1 (len-2))
                                    else None
                                | _ -> None
                              }
                          
                          | _ -> assert false
                        end
                    | _ -> assert false
                  end
              | _ -> assert false
            end
        | _ -> assert false
      end
      
    let print t = 
      Printf.printf "SEARCH REPLY On %s (%d/%d): %s %Ld" 
        t.owner t.open_slots t.all_slots t.filename 
        t.filesize;
      print_newline () 

      (*
      opendchub-0.6.7/src/commands.c: * $SR fromnick filename\5filesize openslots/totalslots\5hubname (hubip:hubport)\5tonick| */
*)
      
    let write buf t = 
      Printf.bprintf buf " %s %s%c%s %d/%d%c%s%s" 
        t.owner t.filename char5 (Int64.to_string t.filesize)
      t.open_slots t.all_slots char5 t.server_name
        (match t.server_ip with
          None -> ""
        | Some server_ip -> Printf.sprintf " (%s)" server_ip);
      match t.to_nick with
        None -> ()
      | Some nick ->
          Printf.bprintf buf "%c%s" char5 nick
    
  end
    
module Version = struct
    type t = {
        version : string;
      }
      
    let parse version = { version = version }
      
    let print t = 
      Printf.printf "VERSION [%s]" (String.escaped t.version) ;
      print_newline () 
      
    let write buf t = 
      Printf.bprintf buf " %s" t.version
    
  end
  
module MyINFO = struct
    type t = {
        dest : string; (* $ALL *)
        nick : string;
        description : string;
        speed : string;
        kind : int;
        email : string;
        size : float;
      }

      (*
    let rec get_ending_dollar list t =
      match list with
        [] -> assert false
      | s :: tail ->
          let len = String.length s in
          assert (len > 0);
          if t.description = "" then            
            if s.[len-1] = '$' then
              {t with 
                description = String.sub s 0 (len-1) }, tail
            else
              get_ending_dollar tail { t with description = s }
          
          else
          if s.[len-1] = '$' then
            {t with 
              description = Printf.sprintf "%s %s" t.description 
                (String.sub s 0 (len-1))
            
            }, tail
          else
            get_ending_dollar tail { t with 
              description = Printf.sprintf "%s %s" t.description s }
            
    let dummy = {
        dest = "$ALL";
        nick = "";
        description = "";
        speed = "";
        kind = Normal;
        email = "";
        size = Int64.zero;
      }
*)
      
    let parse s = 
      match String2.split s '$' with
      | _ :: part1 :: _ :: part2 :: email :: part3 :: _ ->
          begin
            match String2.splitn part1 ' ' 2 with
              dest :: nick :: desc :: [] ->
                let len2 = String.length part2 in
                let speed = String.sub part2 0 (len2-1) in
                let kind = int_of_char part2.[len2-1] in
                
                let size = try
                    let pos = String.index part3 ',' in
                    part3.[pos] <- '.';
                    (float_of_string part3) *. 1024. *. 1024. *. 1024.
                    with _ -> float_of_string part3 in
                { 
                  dest = dest;
                  nick = nick;
                  description = desc;
                  speed = speed;
                  kind = kind;
                  size = size;
                  email = email;
                }
            | _ -> assert false
          end
          
      | list -> List.iter (fun s -> Printf.printf "{%s}" 
              (String.escaped s)) list;
          print_newline ();
          raise Not_found

          
        (*

        {}{ALL mldonkey_jetti_0 mldonkey client}{ }{DSL\006}{}{20000000000}{}
        [] | [_] -> assert false
      | dest :: nick :: tail -> 
          let t, tail = get_ending_dollar tail { dummy with
              dest = dest; nick = nick } in
          match tail with
            [tail] ->
              begin
                match String2.split tail '$' with
                  [""; speed; email; size; ""] ->
                    let len = String.length speed in
                    {
                      t with
                      speed = String.sub speed 0 (len-1);
                      kind = kind_of_char speed.[len-1];
                      size = Int64.of_string size;
                    }
                | _ -> assert false
              end
          | _ -> assert false
*)
        
    let print t = 
      Printf.printf "MyINFO %s %s %s %s %f" 
        t.dest t.nick t.description t.speed t.size;
      print_newline () 
    
    let write buf t = 
      Printf.bprintf buf " %s %s %s$ $%s%c$%s$%10.0f$" 
        t.dest t.nick t.description t.speed
        (char_of_int t.kind) t.email t.size
      
  end

    
module NickAndAddr(M: sig val msg : string end) = struct
      
    type t = {
        nick : string;
        ip : Ip.t;
        port : int;
      }
      
    let parse s = 
      let (nick, rem) = String2.cut_at s ' ' in
      let (ip, port) = String2.cut_at rem ':' in
      {
        nick = nick;
        ip = Ip.of_string ip;
        port = int_of_string port;
      }
      
    let print t = 
      Printf.printf "%s %s %s:%d" M.msg t.nick (Ip.to_string t.ip) t.port;
      print_newline () 
      
    let write buf t = 
      Printf.bprintf  buf "$%s %s %s:%d" M.msg t.nick (Ip.to_string t.ip) t.port;
    
  end
    
module RevConnectToMe = struct
      
    type t = {
        dest : string;
        orig : string;
      }
      
    let parse s = 
      let (dest, orig) = String2.cut_at s ' ' in
      {
        dest = dest;
        orig = orig;
      }
      
    let print t = 
      Printf.printf "RevConnectToMe %s %s" t.dest t.orig;
      print_newline () 
      
    let write buf t = 
      Printf.bprintf  buf "$RevConnectToMe %s %s"  t.dest t.orig;
    
  end
    
module ConnectToMe = NickAndAddr(struct let msg = "ConnectToMe" end)
module MultiConnectToMe = NickAndAddr(struct let msg = "MultiConnectToMe" end)
  
module Msg = struct
    type t = ()
      
    let parse s = ()
      
    let print t = ()
      
    let write buf t = ()
    
  end

module MyNick = SimpleNick2(struct let msg = "MyNick" end)
module ValidateNick = SimpleNick(struct let msg = "ValidateNick" end)
module ForceMove = SimpleNick(struct let msg = "ForceMove" end)
module Hello = SimpleNick(struct let msg = "Hello" end)
module Quit = SimpleNick(struct let msg = "Quit" end)
module GetNickList = Empty(struct let msg = "GetNickList" end)
  
module GetListLen = Empty2(struct let msg = "GetListLen" end)
module MaxedOut = Empty2(struct let msg = "MaxedOut" end)
module Send = Empty2(struct let msg = "Send" end)
module Canceled = Empty2(struct let msg = "Canceled" end)
  
type t =
| LockReq of Lock.t
| KeyReq of Key.t
| MyNickReq of MyNick.t
| ValidateNickReq of ValidateNick.t
| ForceMoveReq of ForceMove.t
| HelloReq of Hello.t
| QuitReq of Quit.t
| ToReq of To.t

| GetNickListReq
| NickListReq of NickList.t
| OpListReq of OpList.t
| VersionReq of Version.t
| SRReq of SR.t

| SearchReq of Search.t
| MultiSearchReq of Search.t
| HubNameReq of HubName.t
| MessageReq of string
| MyINFOReq of MyINFO.t
| UnknownReq of string

| RevConnectToMeReq of RevConnectToMe.t
| ConnectToMeReq of ConnectToMe.t
| MultiConnectToMeReq of MultiConnectToMe.t

| GetReq of Get.t
| FileLengthReq of FileLength.t
| DirectionReq of Direction.t
| GetListLenReq
| MaxedOutReq
| SendReq
| CanceledReq
  
let parse debug s = 
  try
    
    if debug then begin
      Printf.printf "PARSE: {%s}" (String.escaped s); print_newline ();
      end;
    
    let ws = String2.splitn s ' ' 1 in
    match ws with
    | [] -> UnknownReq s
    | [cmd ; args] ->
        if String.length cmd > 0 then
          if cmd.[0] = '$' then
            match cmd with
            | "$Lock" -> LockReq (Lock.parse args)
            | "$Key" -> KeyReq (Key.parse args)
            | "$MyNick" -> MyNickReq (MyNick.parse args)
            
            | "$GetNickList" -> GetNickListReq
            | "$NickList" -> NickListReq (NickList.parse args)
            | "$OpList" -> OpListReq (OpList.parse args)
            | "$Version" -> VersionReq (Version.parse args)
            | "$SR" -> SRReq (SR.parse args)
            
            | "$ValidateNick" -> ValidateNickReq (ValidateNick.parse args)
            | "$ForceMove" -> ForceMoveReq (ForceMove.parse args)
            | "$Hello" -> HelloReq (Hello.parse args)
            | "$Quit" -> QuitReq (Quit.parse args)
            | "$HubName" -> HubNameReq (HubName.parse args)
            | "$MyINFO" -> MyINFOReq (MyINFO.parse args)
            | "$To:" -> ToReq (To.parse args)
            | "$Search" -> SearchReq (Search.parse args)
            | "$MultiSearch" -> MultiSearchReq (Search.parse args)
            
            
            | "$Get" -> GetReq (Get.parse args)
            | "$FileLength" -> FileLengthReq (FileLength.parse args)
            | "$Direction" -> DirectionReq (Direction.parse args)
            
            | "$RevConnectToMe" -> RevConnectToMeReq (RevConnectToMe.parse args)
            | "$ConnectToMe" -> 
                Printf.printf "Message [%s]" (String.escaped s);
                print_newline ();
                ConnectToMeReq (ConnectToMe.parse args)
            | "$MultiConnectToMe" -> MultiConnectToMeReq 
                  (MultiConnectToMe.parse args)
            | _ -> UnknownReq s
          else
            MessageReq s
        else UnknownReq s
    | [ cmd ] ->
        begin
          match cmd with
          | "$Send" -> SendReq
          | "$GetListLen" -> GetListLenReq
          | "$MaxedOut" -> MaxedOutReq
          | "$Canceled" -> CanceledReq
              
          | _ -> UnknownReq s
        end
    | _ -> UnknownReq s
  with e ->
      Printf.printf "Exception %s in parse" (Printexc2.to_string e);
      print_newline ();
      UnknownReq s
      
let write buf m =
  match m with
    LockReq t -> Buffer.add_string buf "$Lock"; Lock.write buf t
  | KeyReq t -> Buffer.add_string buf "$Key"; Key.write buf t
  | MyNickReq t -> MyNick.write buf t
  | HelloReq t -> Buffer.add_string buf "$Hello"; Hello.write buf t
  | QuitReq t -> Buffer.add_string buf "$Quit"; Quit.write buf t
  | SearchReq t -> Buffer.add_string buf "$Search"; Search.write buf t
  | MultiSearchReq t -> Buffer.add_string buf "$MultiSearch"; Search.write buf t
  | MyINFOReq t -> Buffer.add_string buf "$MyINFO"; MyINFO.write buf t
  | ForceMoveReq t -> Buffer.add_string buf "$ForceMove"; ForceMove.write buf t
  
  | RevConnectToMeReq t -> RevConnectToMe.write buf t
  | ConnectToMeReq t -> ConnectToMe.write buf t
  | MultiConnectToMeReq t -> MultiConnectToMe.write buf t
  
  
  | GetReq t -> Get.write buf t
  | FileLengthReq t -> FileLength.write buf t
  | DirectionReq t -> Direction.write buf t
  | GetListLenReq -> GetListLen.write buf ()
  | MaxedOutReq -> MaxedOut.write buf ()
  | SendReq -> Send.write buf ()
  | CanceledReq -> Canceled.write buf ()
  
  
  | GetNickListReq -> Buffer.add_string buf "$GetNickList"
  | NickListReq t -> 
      Buffer.add_string buf "$NickList"; 
      NickList.write buf t
  | OpListReq t -> Buffer.add_string buf "$OpList"; OpList.write buf t
  | VersionReq t -> Buffer.add_string buf "$Version"; Version.write buf t
  | SRReq t -> Buffer.add_string buf "$SR"; SR.write buf t
  
  
  | ToReq t -> Buffer.add_string buf "$To:"; To.write buf t
  | ValidateNickReq t -> 
      Buffer.add_string buf "$ValidateNick"; ValidateNick.write buf t
  | HubNameReq t -> 
      Buffer.add_string buf "$HubName"; HubName.write buf t
  | MessageReq t -> Buffer.add_string buf t
  | UnknownReq t -> Buffer.add_string buf t
      
let print m =
  begin
    match m with
      LockReq t -> Lock.print t
    | KeyReq t -> Key.print t
    | HelloReq t -> Hello.print t
    | QuitReq t -> Quit.print t
    | MyNickReq t -> MyNick.print t
    
    | GetNickListReq -> GetNickList.print ()
    | NickListReq t -> NickList.print t
    | OpListReq t -> OpList.print t
    | VersionReq t -> Version.print t
    | SRReq t -> SR.print t
    | ForceMoveReq t -> ForceMove.print t
    
    
    | GetReq t -> Get.print t
    | FileLengthReq t -> FileLength.print t
    | DirectionReq t -> Direction.print t
    | GetListLenReq -> GetListLen.print ()
    | MaxedOutReq -> MaxedOut.print ()
    | SendReq -> Send.print ()
    | CanceledReq -> Canceled.print ()
    
    
    | RevConnectToMeReq t -> RevConnectToMe.print t
    | ConnectToMeReq t -> ConnectToMe.print t
    | MultiConnectToMeReq t -> MultiConnectToMe.print t
    
    
    | SearchReq t -> Search.print t
    | MultiSearchReq t -> Printf.printf "MULTI "; Search.print t
    | MyINFOReq t -> MyINFO.print t
    | ValidateNickReq t -> ValidateNick.print t
    | HubNameReq t -> HubName.print t
    | ToReq t -> To.print t
    | MessageReq t -> Printf.printf "MESSAGE: %s" t
    | UnknownReq t -> Printf.printf "UNKNOWN:"; 
        LittleEndian.dump t
  end;
  print_newline () 
  
let dc_handler debug f sock nread =
  let b = TcpBufferedSocket.buf sock in
  try
    let rec iter nread =
      if nread > 0 then begin
          let pos = String.index_from b.buf (b.pos + b.len - nread) '|' in
          if pos < b.pos + b.len then
            let s = String.sub b.buf b.pos (pos - b.pos) in
            buf_used sock (pos - b.pos + 1);
            f (parse !debug s) sock;
            iter b.len
        end
    in
    iter nread
  with Not_found -> ()

let dc_handler3 debug c ff f r sock nread =
  let b = TcpBufferedSocket.buf sock in
  try
    let rec iter nread =
      if nread > 0 then 
        match !c with 
          Some c when c.client_receiving <> Int64.zero ->
            r c sock nread
        | _ ->
            let pos = String.index_from b.buf (b.pos + b.len - nread) '|' in
            if pos < b.pos + b.len then
              let s = String.sub b.buf b.pos (pos - b.pos) in
              buf_used sock (pos - b.pos + 1);
              let m = parse !debug s in
              match !c with
                None -> 
                  c := ff m sock;
                  iter b.len
              | Some c ->
                  f c m sock;
                  iter b.len
    
    in
    iter nread
  with Not_found -> ()
      
let buf = Buffer.create 100
      
let server_send debug sock m =
(*  Printf.printf "SENDING"; print_newline ();
  print m; *)
  Buffer.clear buf;
  write buf m;
  Buffer.add_char buf '|';
  let s = Buffer.contents buf in
  if debug then begin
      Printf.printf "BUFFER SENT[%s]" (String.escaped s); print_newline ();
    end;
  write_string sock s
      
let debug_server_send sock m =
  Printf.printf "SENDING"; print_newline ();
  print m; 
  Buffer.clear buf;
  write buf m;
  Buffer.add_char buf '|';
  let s = Buffer.contents buf in
  Printf.printf "BUFFER SENT[%s]" (String.escaped s); print_newline ();  
  write_string sock s

let rec count_tabs line pos =
  if line.[pos] <> '\t' then pos else
    count_tabs line (pos+1)

let char_13 = char_of_int 13
    
let parse_list user s =
  String2.replace_char s char_13 '\n';
(*  Printf.printf "step 1"; print_newline (); *)
  let lines = String2.split_simplify s '\n' in
(*  Printf.printf "step 2"; print_newline (); *)
  let rec iter ntabs  dirname lines list =
    match lines with 
      [] -> [], list
    | line :: tail ->
        let all_tabs = count_tabs line 0 in
        if ntabs = all_tabs then
          let len = String.length line in
          try
            let pos = String.index line '|' in
            let name = String.sub line all_tabs (pos-all_tabs) in
            let size = String.sub line (pos+1) (len - pos - 1) in
(*            Printf.printf "%s : %s" name size; print_newline (); *)
            let r = new_result name (Int64.of_string size) in
            let filename = Filename.concat dirname name in
            add_result_source r user filename;
            iter ntabs dirname tail ((dirname, r) :: list)
          with _ ->
(* a directory *)
              let name = String.sub line all_tabs (len-all_tabs) in
              let lines, list = iter (ntabs+1) (Filename.concat dirname name)
                tail list in
              iter ntabs dirname lines list
        else begin
(*            Printf.printf "all_tabs: %d; ntabs: %d" all_tabs ntabs; 
            print_newline (); *)
            assert (all_tabs < ntabs);
            lines, list
          end
  in
  let (_, list) = iter 0 "" lines [] in
  list
  
let rec buf_tabs buf n =
  if n > 0 then begin
      Buffer.add_char buf '\t';
      buf_tabs buf (n-1)
    end

open CommonUploads
    
let make_shared_list () =
  let buf = Buffer.create 1000 in
  let rec iter ntabs node =
    let dirname =  node.shared_dirname in
    let ntabs =
      if dirname = "" then ntabs else begin
          buf_tabs buf ntabs;
          Printf.bprintf buf "%s\r\n" dirname;
          ntabs+1
        end
    in
    List.iter (fun sh ->
        buf_tabs buf ntabs;
        Printf.bprintf buf "%s|%Ld\r\n" (
          Filename.basename sh.shared_fullname) sh.shared_size
    ) node.shared_files;
    List.iter (fun (_, node) ->
        iter ntabs node
    ) node.shared_dirs
  in
  iter 0 shared_tree;
  Buffer.contents buf 
 