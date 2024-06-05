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
open CommonOptions
open DcTypes
open DcGlobals
open TcpBufferedSocket
open Options
(*open AnyEndian*)

let log_prefix = "[dcPro]"

let lprintf_nl fmt =
  lprintf_nl2 log_prefix fmt

(* Replace one string to another string from string *)
(*let dc_replace_str_to_str s find_str to_str =
  if find_str = to_str then failwith "dc_replace_str_to_str find_str = to_str";
  let flen = String.length find_str in
  let str = ref "" in
  let rest = ref "" in
  let index = ref 0 in
  let rec replace s =
    let ok =
      (try
        index := String2.search_from s 0 find_str
      with
      | Not_found -> index := -1 );
      if (!index = -1) then begin
        str := !str ^ s; true
      end else begin
        str := !str ^ String2.before s !index ^ to_str;
        rest := String2.after s (!index+flen);
        false
      end
    in
    if not ok then replace !rest 
    else !str
  in
  replace s *)

(* Decode chat messages *)
(* You can now use $ and | in the chat. *)
(* DC++ uses the HTML standard &#36; and &#124; to replace them *)
let dc_decode_chat s =  (* convert html characters &#36; to '|' and &#124; and &amp; from text *)
  let s = dc_replace_str_to_str s "&#124;" "|" in
  let s = dc_replace_str_to_str s "&#36;" "$" in
  let s = dc_replace_str_to_str s "&amp;" "&" in
  s

(* Encode chat messages *)
let dc_encode_chat s = (* convert '|'and '$' to html characters &#36; and &#124; *)
  let s = String2.replace s '$' "&#36;" in
  let s = String2.replace s '|' "&#124;" in
  let s = String2.replace s '&' "&amp;" in
  s

(* Reuseable modules for simple commands *)
module Empty = functor(M: sig val msg : string end) ->  struct
  let parse s = ()
  let print t = lprintf_nl "%s" M.msg
      let write buf t = ()
    end

module Empty2 = functor(M: sig val msg : string end) ->  struct
      let parse s = ()
  let print t = lprintf_nl "%s" M.msg
      let write buf t = Printf.bprintf buf "$%s" M.msg
    end

(* DC uses 1-byte encodings *)
(* Probably better convert to/from utf at transport layer!? *)

let utf_to_dc s =
  (* FIXME need hub-specific encodings *)
(*   Charset.convert Charset.UTF_8 Charset.CP1252 s *)
  try
    Charset.convert Charset.UTF_8 (Charset.charset_from_string !!DcOptions.default_encoding) s
  with
    _ -> Charset.Locale.to_locale s

let dc_to_utf s =
  try
    Charset.convert (Charset.charset_from_string !!DcOptions.default_encoding) Charset.UTF_8 s
  with
    _ -> Charset.Locale.to_utf8 s

let make_name s =
  match String2.split s '/' with
  | ["TTH";tth] -> 
      if is_valid_tiger_hash tth then NameTTH tth else failwith "Invalid TTH"
(*
  | ""::path -> 
      if List.exists (function "." | ".." -> true | _ -> false) path then failwith "Invalid path" else NameShared path
*)
  | [file] -> NameSpecial file
  | _ -> failwith ("Invalid name : " ^ s)

let show_name = function
(*   | NameShared l -> "/" ^ String.concat "/" l *)
  | NameSpecial s -> s
  | NameTTH tth -> "TTH/" ^ tth

module SimpleCmd(M: sig val msg : string end) = struct
  type t = string
  let parse nick = dc_to_utf nick
  let print t = lprintf_nl "%s (%s)" M.msg t
  let write buf t = Printf.bprintf buf "$%s %s" M.msg (utf_to_dc t)
end

(*module NickAndAddr (M: sig val msg : string end) = struct
  type t = {
    nick : string;
    ip : Ip.t;
    port : int;
  }
  let parse s = 
    let (nick, rem) = String2.cut_at s ' ' in
    let (ip, port) = String2.cut_at rem ':' in {
      nick = nick;
      ip = Ip.of_string ip;
      port = int_of_string port;
    }
  let print t = lprintf_nl "%s %s %s:%d" M.msg t.nick (Ip.to_string t.ip) t.port
  let write buf t = Printf.bprintf  buf "$%s %s %s:%d" M.msg t.nick (Ip.to_string t.ip) t.port;
end *)

module SimpleNickList = functor (M: sig val cmd : string end) -> struct
  type t = string list
  let parse t =
    let list = String2.split_simplify t '$' in
    let list = List.rev_map (fun nick -> dc_to_utf nick) list in
    list
  let print t = 
    lprintf "%s list ( " M.cmd;
    List.iter (fun s -> lprintf "%s " s) t;
    lprintf_nl " )"
  let write buf t = 
    Buffer.add_char buf ' ';
    List.iter (fun nick -> Printf.bprintf buf "%s %s$$" M.cmd (utf_to_dc nick)) t
  end

(* Command modules *)

module Adc = functor (A: sig val command : string end) -> struct
(* ADCSEARCH ?? -- DC++ ShareManager.cpp -> ShareManager::AdcSearch::AdcSearch *)
(* ADC Protocol Draft 0.12
GET type identifier start_pos bytes

Requests that a certain file or binary data be transmitted. <start_pos> counts 0 as the first byte. <bytes> may be
set to -1 to indicate that the sending client should fill it in with the number of bytes needed to complete the
file from <start_pos>. <type> is a [a-zA-Z0-9]+ string that specifies the namespace for identifier and BASE requires
that clients recognize the types 'file', 'tthl' and 'list'. Extensions may add to the identifier names as well as
 add new types.
'file' transfers transfer the file data in binary, starting at <start_pos> and sending <bytes> bytes.
Identifier must be a TTH root value from the 'TTH/' root.
'tthl' transfers send the largest set of leaves available) as a binary stream of leaf data, right-to-left, with no
spacing in between them. <start_pos> must be set to 0 and <bytes> to -1 when requesting the data. <bytes> must
contain the total binary size of the leaf stream in SND, and by dividing this length by the individual hash length,
the number of leaves, and thus the leaf level can be deducted. The received leaves can then be used to reconstruct
the entire tree, and the resulting root must match the root of the file (this verifies the integrity of the tree
itself). Identifier must be a TTH root value from the 'TTH/' root.
'list' transfers are used for partial file lists and have a directory as identifier. <start_pos> is always 0 and
<bytes> contains the uncompressed length of the generated XML text in the corresponding SND. An optional flag 'RE1'
means that the client is requesting a recursive list and that the sending client should send the directory itself
and all subdirectories as well. If this is too much, the sending client may choose to send only parts. The flag
should be taken as a hint that the requesting client will be getting the subdirectories as well, so they might as
well be sent in one go. Identifier must be a directory in the unnamed root, ending (and beginning) with ‘/’. *)

  type t = {
    adctype : adc_type;
    start_pos : int64;
    bytes : int64;
    zl : bool;
  }

  let parse s =
    try
      match String2.split s ' ' with
      | adc_type :: ident :: start_pos :: bytes :: flags ->
        {
          adctype = begin match adc_type with
                    | "file" -> AdcFile (make_name ident)
(*                     | "tthl" -> AdcTthl (match name with NameTTH tth -> tth | _ -> failwith "tthl") *)
                    | "list" -> AdcList (ident, List.mem "RE1" flags)
(*                     ((match name with NameShared dir -> dir | _ -> failwith "list"),  *)
                    | _ -> failwith "Unknown ADC GET type" 
          end;
          start_pos = Int64.of_string start_pos;
          bytes = Int64.of_string bytes;
          zl = List.mem "ZL1" flags;
        }
      | _ -> failwith "Invalid ADC GET format"
    with exn ->
      if !verbose_msg_clients || !verbose_upload then 
        lprintf_nl "Error in AdcGet parsing : %s" (Printexc2.to_string exn);
      raise Not_found

  let to_string t = 
    let adc_type,ident,flags =
      match t.adctype with
(*       | AdcTthl tth -> "tthl", show_name (NameTTH tth), [] *)
      | AdcFile name -> "file", show_name name, ""
      | AdcList (path,re) -> "list", path, " RE1"
    in
    let flags = if t.zl then flags ^ " ZL1" else flags in
    Printf.sprintf "$%s %s %s %Ld %Ld%s" A.command
      adc_type ident t.start_pos t.bytes flags

  let print t = lprintf_nl "%s" (to_string t)
  let write buf t = Buffer.add_string buf (to_string t)

  end

module AdcGet = Adc (struct let command = "ADCGET" end)

module AdcSnd = Adc (struct let command = "ADCSND" end)

module Canceled = Empty2(struct let msg = "Canceled" end)

module ConnectToMe = struct 
    type t = {
    nick : string;
    ip : Ip.t;
    port : int;
      }
      
    let parse s = 
    let snick, rnick, senderip =
      match String2.split s ' ' with
      | [ snick ; rnick ; senderip ] -> snick, rnick, senderip (* NMDC compatible clients: *)
      | [ rnick ; senderip ] -> create_temp_nick (), rnick, senderip   (* DC++, NMDC v2.205 and DC:PRO v0.2.3.97A: *)
      | _ -> raise Not_found  
    in 
    let (ip,port) = String2.cut_at senderip ':' in {
      nick = dc_to_utf snick;
      ip = Ip.of_string ip;
      port = int_of_string port;
    }
  let print t = lprintf_nl "$ConnectToMe %s %s:%d" t.nick (Ip.to_string t.ip) t.port
    let write buf t = 
     Printf.bprintf buf " %s %s:%d" (utf_to_dc t.nick) (Ip.to_string t.ip) t.port;
     if !verbose_msg_clients then lprintf_nl "Sending: (%s)" (Buffer.contents buf);
  end
  
  
module Direction = struct
    type t = {
    direction : dc_direction;
        level : int;
      }
  let txt_upload = "Upload"
  let txt_download = "Download" 
      
    let parse s = 
      match String2.split s ' ' with
      | ["Download"; level] -> { 
      direction = Download (int_of_string level); 
            level = int_of_string level }
      | ["Upload"; level] -> { 
      direction = Upload (int_of_string level); 
            level = int_of_string level }
    | _ -> raise Not_found
      
  let print t = lprintf_nl "Direction %s %d" (
        match t.direction with
      | Download _ -> txt_download 
      | Upload _ -> txt_upload) t.level      
      
    let write buf t = 
      Printf.bprintf buf "$Direction %s %d" 
        (match t.direction with
         | Download _ -> txt_download 
         | Upload _ -> txt_upload)
        t.level
end

module FileLength = struct
  type t = int64
  let parse s = Int64.of_string s
  let print t = lprintf_nl "FileLength %Ld" t
  let write buf t = Printf.bprintf buf "$FileLength %Ld" t
  end

module ForceMove = SimpleCmd(struct let msg = "ForceMove" end)

module Get = struct
  type t = {
    filename : string;
    pos : int64;
    }
  let parse s =
    let len = String.length s in
    let pos = String.rindex s '$' in {
      filename = dc_to_utf (String.sub s 0 pos);
      pos = Int64.of_string (String.sub s (pos+1) (len-pos-1));
    }
  let print t = lprintf_nl "Get [%s] %Ld" t.filename t.pos
  let write buf t =
    Printf.bprintf buf "$Get %s$%Ld" (utf_to_dc t.filename) t.pos;
    if !verbose_msg_clients then lprintf_nl "Sending: (%s)" (Buffer.contents buf)
  end

module GetListLen = Empty2(struct let msg = "GetListLen" end)

module Hello = SimpleCmd(struct let msg = "Hello" end)
module HubName = SimpleCmd(struct let msg = "HubName" end)

module Key = struct
    type t = {
        key : string;
      }
    let parse key = { key = key }
  let print t = lprintf_nl "$Key (%s)" t.key
  let write buf t = Printf.bprintf buf " %s" t.key
end
      
module Lock = struct
  type t = {
    key : string;
    info : string;
    extended_protocol: bool;
  }
  let ext_txt = "EXTENDEDPROTOCOL"
  let parse s = 
    match String2.splitn s ' ' 1 with
    | [key; info] -> {
        extended_protocol = (String2.string_ncmp key ext_txt (String.length ext_txt)); (* if s has ext_txt at start, return true *)
        key = key;
        info = info }
    | _ -> raise Not_found
  let print t = lprintf_nl "$Lock %s%s Pk=%s" ext_txt t.key Autoconf.current_version
    let write buf t = 
    Printf.bprintf buf " %s%s Pk=%s" ext_txt t.key Autoconf.current_version
  end

module Message = struct
    type t = {
    from : string;
        message : string;
      }
  let parse m =
    let l = String.length m in
    if l > 75000 then begin
      lprintf_nl "Overlength <Message>: (%s...%d chars)" (shorten_string m 100) l;
      raise Not_found
    end else begin
      if (m.[0] = '<') then begin
        (match String2.splitn m ' ' 1 with
        | [from; m] ->
            let from = String2.replace from char60 empty_string in
            let from = String2.replace from char62 empty_string in
            let m = dc_decode_chat m in
            { from = dc_to_utf from; message = dc_to_utf m }
        | _ -> raise Not_found )
      end else begin
        let m = dc_decode_chat m in
        { from = "-"; message = dc_to_utf m } 
      end
    end
  let print t = lprintf_nl "<Message> (%s) (%s)" t.from t.message
  let write buf t =
    let m = utf_to_dc t.message in
    let m = dc_encode_chat m in
    Printf.bprintf buf "<%s> %s" (utf_to_dc t.from) m
end

module MyINFO = struct
  let return_no_tags dest nick tag email share = 
    { (* basic info record to return as result... *)
      (* Some hubs (Y-hub) send MyInfo without any info, so eg. we don't know users states *)
      dest = dest;
      nick = nick;
      description = tag;
      client_brand = "";
      version = "";
      mode = 'P'; 
      hubs = (0 , 0 , 0);
      slots = 0;
      conn_speed = "";
      open_upload_slot = 0;
      flag = 1;
      sharesize = share;
      email = email;
      bwlimit = 0;
    }
  (* ALL OpChat Operator chat - only for OPs$ $$$0$ *)
  (* ALL nick <description>$ $<connection><flag>$<e-mail>$<sharesize>$ *)
  (* ALL nick <McDC 0.38><++ V:0.691,M:P,H:0/0/10,S:1> *)
  (* ALL nick $ $  $$245524999567$|  Hub can send this also *)
  (* ALL nick <StrgDC++ V:1.00 RC9,M:P,H:9/0/0,S:3>$ $DSL^A$$957396830$ 
  *)
    let parse s = 
    (try
      (match String2.split s '$' with               (* divide string to list by delimiter '$' *)
      | [] -> raise Not_found                       (* MyInfo basic structure was wrong *)
      | _ :: nickdesc :: _ :: connf :: email :: share :: _ ->  
          let l = String.length connf in
          let speed = if (l > 1) then String.sub connf 0 (l-1) else "" in    (* if no conn. type, set to "" *)
          let flag = if (l > 0) then int_of_char connf.[l-1] else 1 in       (* if no flag, set to 1 (normal) *)
          let size = (try Int64.of_string share with _ -> Int64.of_int 0) in (* if no share, set to 0 *)

          let dest_nick,tagline =
            (match String2.split nickdesc '<' with  (* continue dividing nick and description field with '<' ... *)
            | [ dest_nick ; tagline ] -> dest_nick , tagline
            | [ dest_nick ; _ ; tagline] -> dest_nick , tagline
            | [ dest_nick ] -> dest_nick , ""
            | _ -> if !verbose_msg_clients then lprintf_nl "No. of '<':s is wrong in nickdesk"; raise Not_found )
          in
          let dest,nick = 
            (match String2.splitn dest_nick ' ' 2 with
            | [ dest ; nick ; _ ] -> dest, dc_to_utf nick 
            | _ -> if !verbose_msg_clients then lprintf_nl "No. of ' ':s is wrong in dest_nick"; raise Not_found ) 
          in
          if tagline = "" then return_no_tags dest nick tagline email size 
          else begin
            let tagline = 
              (match String2.split tagline '>' with      (* split desc with '>' *)
              | [tagline ; _ ] -> tagline
              | _ -> if !verbose_msg_clients then lprintf_nl "No. of '>':s is wrong in nickdesk"; raise Not_found )
            in
            (match String2.splitn tagline ' ' 1 with        (* split desc with one ' ' *)
            | [client ; tags] ->
                let version = ref "" in
                let mode = ref 'A' in
                let hubs = ref (0 , 0 , 0) in
                let slots = ref 0 in
                let upload = ref 0 in 
                let bwlimit = ref 0 in
                List.iter (fun str ->                 (* split tags with ',' for this iteration *) 
                  let l = String.length str in
                  if (l > 2) then
                    (match str.[0] with
                    | 'v' (* GreylinkDC++ *)
                    | 'V' -> (try version := String2.after str 2 with _ -> () ) 
                    | 'M' -> if (str.[2] = 'P') then mode := 'P'
                    | 'H' ->
                        (match String2.split str '/' with
                        | a :: b :: c :: _ -> hubs :=
                            ( (try int_of_string (String2.after a 2) with _ -> 0 ), 
                            (try int_of_string b with _ -> 0 ),
                            (try int_of_string c with _ -> 0 ) )
                        | _ -> () )
                    | 'S' -> (try slots := int_of_string (String2.after str 2) with _ -> () )
                    | 'O' -> (try upload := int_of_string (String2.after str 2) with _ ->  () )
                    | 'L' | 'B' -> (try bwlimit := int_of_string (String2.after str 2) with _ ->  () )
                    | _ -> 
                        if !verbose_unknown_messages then 
                          lprintf_nl "MyINFO: Unknown tag (%c) in (%s) (%s). Implement or fake line ?" str.[0] tagline nick )
                ) (String2.split tags ','); 
                {                                   (* pass this info record as result.. *) 
                  dest = dest;
                  nick = nick;
                  description = tagline;
                  client_brand = client;
                  version = !version;
                  mode = !mode;
                  hubs = !hubs;
                  slots = !slots;
                  conn_speed = speed;
                  open_upload_slot = !upload;
                  flag = flag;
                  sharesize = size;
                  email = email;
                  bwlimit = !bwlimit;
                }
            | _ ->       (* description has no ' ' separator for client and tags *)
                if !verbose_msg_clients || !verbose_unexpected_messages then begin 
                  lprintf_nl "MyINFO: No correct ' ' separator in tagline (%s)" tagline;
                  lprintf_nl "MyINFO: Whole line is: (%s)" s
                end;
                return_no_tags dest nick tagline email size)
        end
      | _ -> raise Not_found ) (* MyInfo basic structure was wrong *) 
    with _ -> 
      lprintf_nl "Error in MyInfo parsing";
      raise Not_found )

  let print t = lprintf_nl "$MyINFO %s %s %s %s %Ld" t.dest t.nick t.description t.conn_speed t.sharesize
    let write buf t = 
    Printf.bprintf buf " %s %s %s$ $%s%c$%s$%Ld$" 
      t.dest (utf_to_dc t.nick) t.description t.conn_speed
      (char_of_int t.flag) t.email t.sharesize
end

module MyNick = SimpleCmd(struct let msg = "MyNick" end)
module Quit = SimpleCmd(struct let msg = "Quit" end)
module NickList = SimpleNickList(struct let cmd = "NickList" end)
module OpList = SimpleNickList(struct let cmd = "OpList" end)

module RevConnectToMe = struct
  type t = {
    dest : string;
    orig : string;
  }
  let parse s = 
    let (orig , dest) = String2.cut_at s ' ' in {
      dest = dc_to_utf dest;
      orig = dc_to_utf orig;
    }
  let print t = lprintf_nl "$RevConnectToMe %s %s" t.orig t.dest
  let write buf t = 
    Printf.bprintf buf "$RevConnectToMe %s %s" (utf_to_dc t.orig) (utf_to_dc t.dest);
    if !verbose_msg_clients then lprintf_nl "Sending: (%s)" (Buffer.contents buf)
  end

module Search = struct
    type t = {
    passive : bool;
    ip : string;
    port : string;
    nick : string;
        sizelimit : sizelimit;
        filetype : int;
    words_or_tth : string;
      }
    
  (*  Active user:       $Search <ip>:<port> <searchstring>
      Passive user:      $Search Hub:<requestornick> <searchstring>

      <ip>             = client IP address
      <port>           = UDP port on which the client is listening for responses. 
      <requestornick>  = Nick of the Passive User doing the Search. 

      <searchstring>   = <sizerestricted>?<isminimumsize>?<size>?<datatype>?<searchpattern>
      <sizerestricted> = 'T' if the search should be restricted to files of a minimum or maximum size, otherwise 'F'.
      <isminimumsize>  = 'F' if <sizerestricted> is 'F' or if the size restriction places an upper limit
                         on file size, otherwise 'T'.
      <size>           = minimum or maximum size of the file to report (according to <isminimumsize>)
                         if <sizerestricted> is 'T', otherwise 0.
      <datatype>       = restricts the search to files of a particular type. It is an integer selected from:
                         * 1 for any file type
                         * 2 for audio files ("mp3", "mp2", "wav", "au", "rm", "mid", "sm")
                         * 3 for compressed files ("zip", "arj", "rar", "lzh", "gz", "z", "arc", "pak")
                         * 4 for documents ("doc", "txt", "wri", "pdf", "ps", "tex")
                         * 5 for executables ("pm", "exe", "bat", "com")
                         * 6 for pictures ("gif", "jpg", "jpeg", "bmp", "pcx", "png", "wmf", "psd")
                         * 7 for video ("mpg", "mpeg", "avi", "asf", "mov")
                         * 8 for folders
                         * 9 for TTH 
      <searchpattern>  = used by other users to determine if any files match. Non-alphanumeric characters
                         (including spaces and periods) are replaced by '$'.
      Examples:   
        64.78.55.32:412 T?T?500000?1?Gentoo$2005
        Hub:SomeNick T?T?500000?1?Gentoo$2005
  *)
  let s_tth = "TTH:"

    let parse s = 
    (try
      let orig , search = 
        (match String2.split_simplify s ' ' with
        | [orig ; search ] -> orig , search
        | _ -> raise Not_found )
      in
      let passive , nick , ip , port =
        (match String2.splitn orig ':' 1 with 
        | ["Hub" ; nick] ->
            true, dc_to_utf nick, empty_string, empty_string
        | [ip ; port] -> 
            false, empty_string, ip, port
        | _ -> raise Not_found )
      in
      (match String2.splitn search '?' 4 with
      | [has_size; size_kind; size; filetype; words] ->
          let filetype = int_of_string filetype in
          let words =                                        (* strip TTH: from TTH-search or return search words *)
            if filetype = 9 then                             (* TTH *)
              dc_replace_str_to_str words s_tth empty_string (* Strip TTH: *)             
            else begin                                       (* normal search words *)
              let s = ref (String.copy words) in
              String2.replace_char !s '$' ' ';
              String.lowercase !s 
            end
          in 
          let words = dc_to_utf words in 
          let size =
            (match has_size, size_kind with
            | "T", "T" -> AtMost (Int64.of_float (float_of_string size))
              |  "T", "F" -> AtLeast (Int64.of_float (float_of_string size))
            | _ -> NoLimit )
          in {
            passive = passive;
            nick = nick;
            ip = ip;
            port = port;
            sizelimit = size;
            filetype = filetype;
            words_or_tth = words;
            } 
      | _ -> raise Not_found )
    with _ ->
      if !verbose_msg_clients then lprintf_nl "Search parsing error: (%s)" s;
      raise Not_found )

  let print t = lprintf_nl "$Search %s %s %d %s" t.nick t.ip t.filetype t.words_or_tth 
  let write buf t =
      Printf.bprintf buf " %s %c?%c?%s?%d?%s"
      (if t.passive then "Hub:" ^ (utf_to_dc t.nick) else t.ip ^ ":" ^ t.port )
      (if t.sizelimit = NoLimit then 'F' else 'T')
      (match t.sizelimit with
       | AtMost _ -> 'T'
       | _ -> 'F' )
      (match t.sizelimit with
       | AtMost n -> Int64.to_string n
        | AtLeast n -> Int64.to_string n
        | _  -> "0")
      t.filetype
      (let words =
         if t.filetype = 9 then s_tth ^ t.words_or_tth    (* if TTH search is wanted, send root hash *)
         else begin
           let s = ref (String.copy t.words_or_tth) in    (* otherwise send search words *) 
           String2.replace_char !s char32 '$';
           !s
         end
       in
       utf_to_dc words);
    (*if !verbose_msg_clients then lprintf_nl "Sending: (%s)" (Buffer.contents buf)*)
  end

module Send = Empty2 (struct let msg = "Send" end)

module SR = struct
    type t = {
        owner : string;
    directory : string;
        filename : string;
        filesize : int64;
        open_slots : int;
        all_slots : int;
        server_name : string;
    tth : string;
    server_ip : string;
    server_port : string;
        to_nick : string option;
      }
    let parse s = 
    (try 
      let is_dir, owner_and_filename , size_and_slots, server_info =
        (match String2.split s char5 with
        | [owner_and_filename; size_and_slots; server_info] -> (* result is a file *) 
            false, owner_and_filename, size_and_slots, server_info
        | [owner_and_filename; server_info] ->                 (* result is a directory, currently not supported *)
            let pos = String.rindex owner_and_filename ' ' in
            let len = String.length owner_and_filename in
            let size_and_slots = Printf.sprintf "0 %s" 
              (String.sub owner_and_filename (pos+1) (len - pos - 1)) in
            let owner_and_filename = String.sub owner_and_filename 0 pos in
            true , owner_and_filename, size_and_slots, server_info
        | _ -> raise Not_found )
      in
      (match String2.splitn owner_and_filename ' ' 1 with
      | [owner; filename_with_dir] ->            (* $SR User1 mypathmotd.txt<0x05>437 3/4<0x05>Testhub (10.10.10... *)
          let directory , filename = 
            if is_dir then filename_with_dir, "" (* if it was directory, null the filename *) 
            else begin   
              (try
                let pos = String.rindex filename_with_dir char92 in
                dc_to_utf (String.sub filename_with_dir 0 pos) ,
                dc_to_utf (String2.after filename_with_dir (pos+1))
               with _ -> "" , filename_with_dir )
            end
          in
          (match String2.splitn size_and_slots ' ' 1 with   (*...<0x05>437 3/4<0x05>...  *)
          | [size; slots] -> 
              (match String2.splitn slots '/' 1 with
              | [open_slots; all_slots] -> 
                  let get_server_and_tth str =           (* function for separation of TTH and servername     *)
                    (match String2.splitn str ':' 1 with (* the <server_name> is replaced with TTH:<tth_hash> *)
                    | ["TTH" ; tiger_root] -> tiger_root, "" 
                    | [server_name] -> "", (dc_to_utf server_name)
                    | _ -> "","" )
                  in
                  let server_name, tth, ip, port =
                    (try
                      let pos = String.rindex server_info '(' in
                      let server_or_tth = String.sub server_info 0 (pos-1) in
                      let server_addr = String.sub server_info (pos+1) (String.length server_info - (pos+2)) in
                      let ip,port =
                        (match String2.split server_addr ':' with
                        | [ip ; port] -> ip, port
                        | [ip] -> ip,""
                        | _ -> "","" )
                      in
                      let tth,server_name = get_server_and_tth server_or_tth in
                      server_name,tth,ip,port
                    with _ ->                      (* if error above, try to find only TTH hash from start *)
                      let pos = String.index server_info '(' in
                      let server_or_tth = String.sub server_info 0 (pos-1) in
                      let tth, server_name = get_server_and_tth server_or_tth in
                      server_name, tth, "", "" )
      in
                              {
                    owner = dc_to_utf owner;
                    directory = directory;
                                filename = filename;
                    filesize = ( try Int64.of_string size with _ -> Int64.of_int 0 );
                    open_slots = ( try int_of_string open_slots with _ -> 0 );
                    all_slots = ( try int_of_string all_slots with _ -> 0 );
                    to_nick = None; (* hubs should not send this at all *)
                                server_name = server_name;
                    tth = tth;
                    server_ip = ip;
                    server_port = port; 
                              }
              | _ -> raise Not_found )
          | _ -> raise Not_found )
      | _ -> raise Not_found )
    with e ->
      if !verbose_msg_clients then lprintf_nl "Error in SR parsing (%s)" s;
      raise Not_found )
                          
  let print t = lprintf_nl "$SR %s (%d/%d): %s %s %Ld (%s)" 
      t.owner t.open_slots t.all_slots t.directory t.filename t.filesize t.tth
      (* opendchub-0.6.7/src/commands.c: * $SR fromnick filename\5filesize openslots/totalslots\5hubname (hubip:hubport)\5tonick| */ *)
    let write buf t = 
    Printf.bprintf buf " %s %s\\%s%c%s %d/%d%cTTH:%s (%s:%s)" 
      (utf_to_dc t.owner)
      (utf_to_dc t.directory)
      (utf_to_dc t.filename) char5 (Int64.to_string t.filesize)
      t.open_slots t.all_slots char5 t.tth t.server_ip t.server_port;
    (match t.to_nick with
    | None -> ()
    | Some nick -> Printf.bprintf buf "%c%s" char5 (utf_to_dc nick) );
    (*if !verbose_msg_clients then lprintf_nl "Sending: (%s)" (Buffer.contents buf)*)
end

module Supports = struct (* Extended DC++ *)

  let x_nogetinfo = "NoGetINFO"
  let x_nohello = "NoHello"
  let x_userip2 = "UserIP2"
  let x_usercommand = "UserCommand"
  let x_tthsearch = "TTHSearch"
  let x_opplus = "OpPlus"
  let x_feed = "Feed"
  let x_mcto = "MCTo"
  let x_hubtopic = "HubTopic"

  let x_bzlist = "BZList"
  let x_minislots = "MiniSlots"
  let x_getzblock = "GetZBlock"
  let x_xmlbzlist = "XmlBZList"
  let x_adcget = "ADCGet"
  let x_tthl = "TTHL"
  let x_tthf = "TTHF"
  let x_zlig = "ZLIG"
  let x_clientid = "ClientID"
  let x_chunk = "CHUNK"
  let x_gettestzblock = "GetTestZBlock"
  let x_getcid = "GetCID"

  let create_supports_string t =
    let c = ref " " in
    let s = ref "" in 
    (match t with
    | HubSupports t ->  
        if t.nogetinfo = true then s := !s ^ !c ^ x_nogetinfo;
        if t.nohello = true then s := !s ^ !c ^ x_nohello;
        if t.userip2 = true then s := !s ^ !c ^ x_userip2;
        if t.usercommand = true then s := !s ^ !c ^ x_usercommand;
        if t.tthsearch = true then s := !s ^ !c ^ x_tthsearch;
        if t.opplus = true then s := !s ^ !c ^ x_opplus;
        if t.feed = true then s := !s ^ !c ^ x_feed;
        if t.mcto = true then s := !s ^ !c ^ x_mcto;
        if t.hubtopic = true then s := !s ^ !c ^ x_hubtopic;
    | ClientSupports t ->
        if t.bzlist = true then s := !s ^ !c ^ x_bzlist;
        if t.minislots = true then s := !s ^ !c ^ x_minislots;
        if t.getzblock = true then s := !s ^ !c ^ x_getzblock;
        if t.xmlbzlist = true then s := !s ^ !c ^ x_xmlbzlist;
        if t.adcget = true then s := !s ^ !c ^ x_adcget;
        if t.tthl = true then s := !s ^ !c ^ x_tthl;
        if t.tthf = true then s := !s ^ !c ^ x_tthf;
        if t.zlig = true then s := !s ^ !c ^ x_zlig;
        if t.clientid = true then s := !s ^ !c ^ x_clientid;
        if t.chunk = true then s := !s ^ !c ^ x_chunk;
        if t.gettestzblock = true then s := !s ^ !c ^ x_gettestzblock;
        if t.getcid = true then s := !s ^ !c ^ x_getcid );
    !s
  
  let support_exists lst x =
    let result = ref false in
    let rec loop i =
      if !result = false then
        match i with 
        | [] -> ()
        | hd :: tl -> ( 
            if hd = x then result := true
            else loop tl
          )
    in
    loop lst;
    !result
    
  let parse source m =
    let l = String2.split m ' ' in
    (match source with
    | true -> HubSupports {  (* message is from server *) 
        nogetinfo = (support_exists l x_nogetinfo);
        nohello = (support_exists l x_nohello);
        userip2 = (support_exists l x_userip2);
        usercommand = (support_exists l x_usercommand);
        tthsearch = (support_exists l x_tthsearch);
        opplus = (support_exists l x_opplus);
        feed = (support_exists l x_feed);
        mcto = (support_exists l x_mcto);
        hubtopic = (support_exists l x_hubtopic);
      }
    | false -> ClientSupports { (* message is from client *) 
        bzlist = (support_exists l x_bzlist);
        minislots = (support_exists l x_minislots);
        getzblock = (support_exists l x_getzblock);
        xmlbzlist = (support_exists l x_xmlbzlist);
        adcget = (support_exists l x_adcget);
        tthl = (support_exists l x_tthl);
        tthf = (support_exists l x_tthf);
        zlig = (support_exists l x_zlig);
        clientid = (support_exists l x_clientid);
        chunk = (support_exists l x_chunk);
        gettestzblock = (support_exists l x_gettestzblock);
        getcid = (support_exists l x_getcid);
      } )
      
  let print t = lprintf_nl "$Supports%s" (create_supports_string t) 
  let write buf t = Printf.bprintf buf "$Supports%s" (create_supports_string t) 
  end
  
module To = struct
    type t = {
    dest : string;
    from : string;
    message : string;
      }
    let parse s = 
    if ((String.length s) > 75000) then begin
      lprintf_nl "Overlength $To: (%s)" (shorten_string s 50);
          raise Not_found
    end else begin
      (match String2.splitn s ' ' 4 with
      | [dest ; "From:" ; from ; _ ; message] -> 
          let m =
            let m = dc_decode_chat message in
            m
          in {
            dest = dc_to_utf dest;
            from = dc_to_utf from;
            message = dc_to_utf m;
                    }
      | _ -> raise Not_found )
              end
  let print t = lprintf_nl "$To (%s) (%s) (%s)" t.dest t.from t.message
    let write buf t = 
    let m = dc_encode_chat t.message in
    let from = utf_to_dc t.from in
    Printf.bprintf buf " %s From: %s $<%s> %s" 
      (utf_to_dc t.dest) from from 
      (utf_to_dc m)
  end

module UGetBlock = struct
    type t = {
    ufilename : string;
    ubytes : int64;
    upos : int64;
      }
    let parse s = 
    (match String2.splitn s ' ' 2 with
    | [pos; bytes; filename ] ->
        let filename = dc_to_utf filename in
      {
          ufilename = filename;
          ubytes = Int64.of_string bytes;
          upos = Int64.of_string pos;
      }
    | _ -> raise Not_found )
  let print t = lprintf_nl "Get %Ld %Ld %s" t.upos t.ubytes t.ufilename
    let write buf t = 
    Printf.bprintf buf "$Get %Ld$ %Ld %s" t.upos t.ubytes t.ufilename; (*UTF8*)
    if !verbose_msg_clients then lprintf_nl "Sending: (%s)" (Buffer.contents buf)
  end

module UserIP = struct
  type t = string list

  let parse s = String2.split_simplify s '$'

  let parse_nameip =
    List2.filter_map (fun s ->
      match String2.split s ' ' with
      | [name;addr] -> Some (dc_to_utf name, Ip.addr_of_string addr)
      | _ -> None)

  let print st = 
    lprintf "UserIP list (";
    List.iter (fun s -> lprintf "%s " (dc_to_utf s)) st;
    lprintf_nl ")"

  let write buf st =
    Printf.bprintf buf "$UserIP %s" (String.concat "$$" (List.map utf_to_dc st))
  end

(* Message type definitions and basic parsing *)
type t =
  | AdcGetReq of AdcGet.t
  | AdcSndReq of AdcSnd.t
  | BadPassReq 
  | CanceledReq
  | ConnectToMeReq of ConnectToMe.t
  | DirectionReq of Direction.t
  | ErrorReq of string
  | FailedReq of string
  | FileLengthReq of FileLength.t
  | ForceMoveReq of ForceMove.t
  | GetListLenReq
  | GetNickListReq
  | GetPassReq
  | GetReq of Get.t
  | HelloReq of Hello.t
  | HubIsFullReq
  | HubNameReq of HubName.t
  | HubTopicReq of string
  | KeyReq of Key.t
  | LockReq of Lock.t
  | LogedInReq of string
  | MaxedOutReq
  | MessageReq of Message.t
  (*| MultiConnectToMeReq of MultiConnectToMe.t*)
  (*| MultiSearchReq of Search.t*)
  | MyInfoReq of DcTypes.dc_myinfo
  | MyNickReq of MyNick.t
  | MyPassReq of string
  | NickListReq of NickList.t
  | OpListReq of OpList.t
  | QuitReq of Quit.t
  | RevConnectToMeReq of RevConnectToMe.t
  | SearchReq of Search.t
  | SendReq
  | SRReq of SR.t
  | SupportsReq of DcTypes.dc_supports
  | ToReq of To.t
  | UGetBlockReq of UGetBlock.t
  | UnknownReq of string
  | UserCommandReq
  | UserIPReq of UserIP.t
  | ValidateDenideReq of string
  | ValidateNickReq of string
  | VersionReq of string 

(* Parse messages from a string s that "dc_handler" has sent and already separated
   from sock.buffer with '|'. From now on, parse the actual commands that start
   with '$' or '<'
   source: true = server, false = client *)                                                               
let dc_parse source s = 
  (try
    let ws = String2.splitn s ' ' 1 in
    (match ws with
    | [] -> UnknownReq ""       (* ignore empty messages *)
    | [cmd ; args] ->           (* two part message - cmd and args *)
        if (cmd.[0] = '$') then (* Commands with '$' ...*)
          (match cmd with
          | "$ADCGET" -> AdcGetReq (AdcGet.parse args)
          | "$ADCSND" -> AdcSndReq (AdcSnd.parse args) 
          | "$ConnectToMe" -> ConnectToMeReq (ConnectToMe.parse args)
          | "$Direction" -> DirectionReq (Direction.parse args)
          | "$Error" -> ErrorReq s
          | "$Failed" -> FailedReq s
          | "$FileLength" -> FileLengthReq (FileLength.parse args)
          | "$ForceMove" -> ForceMoveReq (ForceMove.parse args)
          | "$Get" -> GetReq (Get.parse args)
          | "$GetNickList" -> GetNickListReq
          | "$Hello" -> HelloReq (Hello.parse args)
          | "$HubName" -> HubNameReq (HubName.parse args)
          | "$HubTopic" -> HubTopicReq (dc_to_utf args)
          | "$Key" -> KeyReq (Key.parse args)
          | "$Lock" -> LockReq (Lock.parse args)
          | "$LogedIn" -> LogedInReq args
          (*| "$MultiConnectToMe" -> MultiConnectToMeReq (MultiConnectToMe.parse args)*)
          (*| "$MultiSearch" -> MultiSearchReq (Search.parse args)*)
          | "$MyINFO" -> MyInfoReq (MyINFO.parse args)
          | "$MyNick" -> MyNickReq (MyNick.parse args)
          | "$NickList" -> NickListReq (NickList.parse args)
          | "$OpList" -> OpListReq (OpList.parse args)
          | "$Quit" -> QuitReq (Quit.parse args)
          | "$RevConnectToMe" -> RevConnectToMeReq (RevConnectToMe.parse args)
          | "$Search" -> SearchReq (Search.parse args)
          | "$SR" -> SRReq (SR.parse args)
          | "$Supports" -> SupportsReq (Supports.parse source args) (* here we need the info about source type*)
          | "$To:" -> ToReq (To.parse args)
          | "$UGetBlock" -> UGetBlockReq (UGetBlock.parse args)
          | "$UserCommand" -> UserCommandReq
          | "$UserIP" -> UserIPReq (UserIP.parse args)
          | "$ValidateDenide" -> ValidateDenideReq args 
          | "$Version" -> VersionReq args (* VersionReq (Version.parse args) *)
          | _ -> UnknownReq s )
        else              (* all other two part messages, that don't start with '$' get type MessageReq *)
          MessageReq (Message.parse s)
    | [ cmd ] ->            (* Messages with only one part *)
        if (cmd.[0] = '$') then 
          (match cmd with
          | "$BadPass" -> BadPassReq
          | "$Canceled" -> CanceledReq
          | "$GetListLen" -> GetListLenReq
          | "$GetPass" -> GetPassReq 
          | "$HubIsFull" -> HubIsFullReq
          | "$MaxedOut" -> MaxedOutReq
          | "$Send" -> SendReq
          | _ -> UnknownReq s )
        else
          MessageReq (Message.parse s)
    | _ -> UnknownReq s )
  with _ ->
    UnknownReq s )
              
let dc_write buf m =
  (match m with
  | AdcGetReq t -> AdcGet.write buf t
  | AdcSndReq t -> AdcSnd.write buf t
  | BadPassReq -> ()
  | CanceledReq -> Canceled.write buf ()
  | ConnectToMeReq t -> Buffer.add_string buf "$ConnectToMe"; ConnectToMe.write buf t
  | DirectionReq t -> Direction.write buf t
  | ErrorReq s -> Printf.bprintf buf "$Error <%s>" (utf_to_dc s) 
  | FailedReq s -> Printf.bprintf buf "$Failed <%s>" s (*UTF8*)
  | FileLengthReq t -> FileLength.write buf t
  | ForceMoveReq t -> ForceMove.write buf t
  | GetListLenReq -> GetListLen.write buf ()
  | GetNickListReq -> Buffer.add_string buf "$GetNickList"
  | GetPassReq -> ()
  | GetReq t -> Get.write buf t
  | HelloReq t -> Hello.write buf t
  | HubIsFullReq -> ()
  | HubNameReq t -> HubName.write buf t
  | HubTopicReq s -> ()
  | LockReq t -> Buffer.add_string buf "$Lock"; Lock.write buf t
  | LogedInReq s -> ()
  | KeyReq t -> Buffer.add_string buf "$Key"; Key.write buf t
  | MaxedOutReq -> Buffer.add_string buf "$MaxedOut"
  | MessageReq t -> Message.write buf t
  (*| MultiConnectToMeReq t -> MultiConnectToMe.write buf t*)
  (*| MultiSearchReq t -> Buffer.add_string buf "$MultiSearch"; Search.write buf t*)
  | MyInfoReq t -> Buffer.add_string buf "$MyINFO"; MyINFO.write buf t
  | MyNickReq t -> MyNick.write buf t
  | MyPassReq s -> Printf.bprintf buf "$MyPass %s" s
  | NickListReq t -> NickList.write buf t
  | OpListReq t -> OpList.write buf t
  | QuitReq t -> Quit.write buf t
  | RevConnectToMeReq t -> RevConnectToMe.write buf t
  | SearchReq t -> Buffer.add_string buf "$Search"; Search.write buf t
  | SendReq -> Send.write buf ()
  | SRReq t -> Buffer.add_string buf "$SR"; SR.write buf t
  | SupportsReq t -> Supports.write buf t
  | ToReq t -> Buffer.add_string buf "$To:"; To.write buf t
  | UnknownReq t -> Buffer.add_string buf t
  | UGetBlockReq t -> UGetBlock.write buf t
  | UserCommandReq -> ()
  | UserIPReq t -> UserIP.write buf t
  | ValidateNickReq s -> Printf.bprintf buf "$ValidateNick %s" s
  | ValidateDenideReq s -> Buffer.add_string buf s
  | VersionReq s -> Printf.bprintf buf "$Version %s" s )

let dc_print m =
  (match m with
  | AdcGetReq t -> AdcGet.print t
  | AdcSndReq t -> AdcSnd.print t
  | BadPassReq -> lprintf_nl "$BadPass"
  | CanceledReq -> Canceled.print ()
  | ConnectToMeReq t -> ConnectToMe.print t
  | DirectionReq t -> Direction.print t
  | ErrorReq t -> lprintf_nl "$Error"
  | FailedReq t -> lprintf_nl "$Failed"
  | FileLengthReq t -> FileLength.print t
  | ForceMoveReq t -> ForceMove.print t
  | GetListLenReq -> GetListLen.print ()
  | GetNickListReq -> lprintf_nl "$GetNickList"
  | GetPassReq -> lprintf_nl "$GetPass"
  | GetReq t -> Get.print t
  | HelloReq t -> Hello.print t
  | HubIsFullReq -> lprintf_nl "$HubIsFull"
  | HubNameReq t -> HubName.print t
  | HubTopicReq _ -> lprintf_nl "$HubTopic"
  | LockReq t -> Lock.print t
  | LogedInReq _ -> lprintf_nl "$LogedIn"
  | KeyReq t -> Key.print t
  | MaxedOutReq -> lprintf_nl "$MaxedOut"
  | MessageReq t -> Message.print t
  (*| MultiConnectToMeReq t -> MultiConnectToMe.print t*)
  (*| MultiSearchReq t -> lprintf "MULTI "; Search.print t*)
  | MyPassReq s -> lprintf_nl "$MyPass %s" s
  | MyInfoReq t -> MyINFO.print t
  | MyNickReq t -> MyNick.print t
  | NickListReq t -> NickList.print t
  | OpListReq t -> OpList.print t
  | QuitReq t -> Quit.print t
  | RevConnectToMeReq t -> RevConnectToMe.print t
  | SearchReq t -> Search.print t
  | SendReq -> Send.print ()
  | SRReq t -> SR.print t
  | SupportsReq t -> Supports.print t
  | ToReq t -> To.print t
  | UGetBlockReq t -> UGetBlock.print t
  | UnknownReq s -> lprintf_nl "UnknownReq: (%s)..." s
  | UserCommandReq -> lprintf_nl "UserCommand"
  | UserIPReq st -> UserIP.print st
  | ValidateNickReq s -> lprintf_nl "$ValidateNick %s" s
  | ValidateDenideReq s -> lprintf_nl "$ValidateDenide %s" s
  | VersionReq s -> lprintf_nl "$Version %s" s )

(* server incoming messages handler *) (* |7467673|738838383| *)
let dc_handler_server f sock nread =
  let b = TcpBufferedSocket.buf sock in
  (try
    let rec iter nread =
      if nread > 0 then begin
        let pos = Bytes.index_from b.buf b.pos '|' in
        if pos < (b.pos + b.len) then begin
          let len = pos - b.pos in
          let s = Bytes.sub b.buf b.pos len in
          buf_used b (len+1);
          begin 
            let ss = Bytes.to_string s in
            try f (dc_parse true ss) sock
            with exn -> lprintf_nl "server handler %S : %s" ss (Printexc2.to_string exn)
          end;
          iter b.len
        end
      end
    in
    iter nread
  with Not_found -> () )

(* client incoming messages handler *)
let dc_handler_client c fm nm dm sock nread = (* fm = (read_first_message false) t sock *)
                                              (* nm = DcClients.client_reader c t sock *)
                                              (* dm = DcClients.client_downloaded c sock nread  *)
  let b = TcpBufferedSocket.buf sock in
  (try
    let rec iter nread =
      if nread > 0 then begin 
        (match !c with 
        | Some c when c.client_receiving <> Int64.zero -> (* if we are downloading from client ...*)
            dm c sock nread 
        | _ ->                                            (* or message is a new connection ... *)
            let pos = Bytes.index_from b.buf b.pos '|' in
            if pos < (b.pos + b.len) then begin
              let len = pos - b.pos in
              let s = Bytes.sub b.buf b.pos len in
              let ss = Bytes.to_string s in
              let msg = dc_parse false ss in
              buf_used b (len+1);
              begin try
                (match !c with
                | None -> c := fm msg sock (* do this only once per new non-existing client eg. we are in ACTIVE mode *)
                | Some c -> nm c msg sock); (* after initial connection is established *)
              with exn -> lprintf_nl "client handler %S : %s" ss (Printexc2.to_string exn)
              end;
              iter b.len
            end )
      end
    in
    iter nread
  with Not_found ->
    (*lprintf_nl "Message from client cut: (%s)" (String.sub b.buf b.pos b.len);*)
    () )

let buf = Buffer.create 100

(* To servers and to clients outgoing messages *)
let dc_send_msg sock m =
  Buffer.reset buf;
  dc_write buf m;
  Buffer.add_char buf '|';
  let s = Buffer.contents buf in
  write_string sock s

