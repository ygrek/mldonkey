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

open CommonTypes
  (*

1. Download the .torrent file
*****************************
 {
 "announce" = "http://sucs.org:6969/announce";
 "info" =  {
    "files" =  [
    {
    "length" =  682164224;
    "path" =  [ "Mandrake91-cd1-inst.i586.iso";  ]
    }
    ;
     {
     "length" =  681279488;
     "path" =  [
     "Mandrake91-cd2-ext.i586.iso";
      ]
      ;
       }
       ;
        {
        "length" =  681574400;
        "path" =  [
        "Mandrake91-cd3-i18n.i586.iso";
         ]
         ;
          }
          ;
           ]
           ;
           "name" = "mandrake9.1";
           "piece length" =  262144;
           "pieces" =  "[EAd\155ã´gÛ ÓþËf\134Ê«\025\016ôÍµ,1U\150À
\132\147îª\n%ù\\é,\012ÿC\008GÈÓd!æ¾öuL!\134Ô\016\152&\017¾\008³¢d\029Ë3\031Ï\134
#»×\025\137¡=¢.®\019§´\138î.ñ\151O\137Ùÿ,£ç&\019ÀÛ¢Ã§\156.ù\150<Eªª\153\018\145\
149d\147[+J=º\155l\139Î\028¡dVÉ\000-\017°Å¤\013\154¼>A¹Ã5ïIt\007\020©ãÚÀÈÈ\014O®
ô1\152UÄ\026K\021^ãúì5Í¿ü \026\149\131q\024\015¸]Òþ£\027&\148\\ã-©\028WMÂ5...";
 }
 ;
  }

2. Extract BitTorrent information needed:
*****************************************
  
Metainfo files are bencoded dictionaries with the following keys -

'announce'
    The url of the tracker.

'info'
    This maps to a dictionary, with keys described below.

    The 'name' key maps to a string which is the suggested name to save
    the file (or directory) as. It is purely advisory.

    'piece length' maps to the number of bytes in each piece the file is
    split into. For the purposes of transfer, files are split into
    fixed-size pieces which are all the same length except for possibly
    the last one which may be truncated. Piece length is almost always a
    power of two, most commonly 2^20 .

    'pieces' maps to a string whose length is a multiple of 20. It is to
    be subdivided into strings of length 20, each of which is the sha1
    hash of the piece at the corresponding index.

    There is also a key 'length' or a key 'files', but not both or
    neither. If 'length' is present then the download represents a
    single file, otherwise it represents a set of files which go in a
    directory structure.

    In the single file case, 'length' maps to the length of the file in
    bytes.

    For the purposes of the other keys, the multi-file case is treated
    as only having a single file by concatenating the files in the order
    they appear in the files list. The files list is the value 'files'
    maps to, and is a list of dictionaries containing the following keys -

'length'
The length of the file, in bytes. 'path'
    A list of strings corresponding to subdirectory names, the last of
which is the actual file name (a zero length list is an error case).

In the single file case, the 'name' key is the name of a file, in the
muliple file case, it's the name of a directory.


3. Contact the tracker regularly to update file information
***********************************************************
  
Tracker GET requests have the following keys by HTTP:

'info_hash'
    The 20 byte sha1 hash of the bencoded form of the 'info' value from
    the metainfo file. Note that this is a substring of the metainfo
    file. This value will almost certainly have to be escaped.

'peer_id'
    A string of length 20 which this downloader uses as its id. Each
    downloader generates its own id at random at the start of a new
    download. This value will also almost certainly have to be escaped.

'ip'
    An optional parameter giving the ip (or dns name) which this peer is
    at. Generally used for the origin if it's on the same machine as the
    tracker.

'port'
    The port number this peer is listening on. Common behavior is for a
    downloader to try to listen on port 6881 and if that port is taken
    try 6882, then 6883, etc. and give up after 6889.

'uploaded'
    The total amount uploaded so far, encoded in base ten ascii.

'downloaded'
    The total amount downloaded so far, encoded in base ten ascii.

'left'
    The number of bytes this peer still has to download, encoded in base
    ten ascii. Note that this can't be computed from downloaded and the
    file length since it might be a resume, and there's a chance that
    some of the downloaded data failed an integrity check and had to be
    re-downloaded.

'event'
    This is an optional key which maps to 'started', 'completed', or
    'stopped' (or '', which is the same as not being present). 
  
---> bencoded replu:
  { 'failure reason' = ... }
or
{
 'interval' = ....; (* before next request to tracker *)
 'peers' =  [ 
   {
    'peer id' = ....;
    'ip' - ....;
    'port' = ....;
   };
   ....
  ] 
}

4. Contact every peer regularly
*******************************

Handshake:

type int = BigEndian.int32
  
--->
string8 (prefixed by length): "BitTorrent protocol" 
int8[8]: reserved(zeros)
int8[20 bytes]: Sha1.string (Bencode.encode file.file_info)
int8[20 bytes]: peer id
  
<---
string8 (prefixed by length): "BitTorrent protocol" 
int8[8]: reserved(zeros)
int8[20 bytes]: Sha1.string (Bencode.encode file.file_info)
int8[20 bytes]: peer id

----> disconnect if sha1 don't match, or if peer id is unexpected

msg: 
        int: len of message (byte+payload) 0 -> keepalive sent every 2 minutes
        byte8: opcode of message
        int8[..]: payload
        
opcodes:        
Connections start out choked and not interested.

No payload:
    * 0 - choke: you have been blocked
    * 1 - unchoke: you have been unblocked
    * 2 - interested: I'm interested in downloading this file now
    * 3 - not interested: I'm not interested in downloading this file now
With bencoded payload:
    * 4 - have
          int : index of new completed chunk          
    * 5 - bitfield: 
          string: a bitfield of bit 1 for downloaded chunks
          byte: bits are inverted 0....7 ---> 7 .... 0      
    * 6 - request
          int: index
          int: begin
          int: length (power of 2, 2 ^ 15)
    * 7 - piece
          int: index
          int: begin
          string: piece
    * 8 - cancel: cancel a requesu
          int: index
          int: begin
          int: length (power of 2, 2 ^ 15)

Chock/unchock every 10 seconds        
*)



open Printf2
open CommonOptions
open BTOptions
open Options
open Md4
open CommonGlobals
open BigEndian
open TcpBufferedSocket
open AnyEndian
open BTTypes
open BTGlobals
  
type ghandler =
  BTHeader of (gconn -> TcpBufferedSocket.t -> 
  (string * Sha1.t * Sha1.t) -> unit)
| Reader of (gconn -> TcpBufferedSocket.t -> unit)

and gconn = {
    mutable gconn_handler : ghandler;
    mutable gconn_refill : (TcpBufferedSocket.t -> unit) list;
    mutable gconn_close_on_write : bool;
  }

type msg = 
| Choke
| Unchoke
| Interested
| NotInterested
| Have of int64
| BitField of string
| Request of int * int64 * int64
| Piece of int * int64 * string * int * int
| Cancel of int64 * int64 * int64
| Ping  

  
let bt_print msg =
  match msg with
  | Choke -> lprintf "Choke\n"
  | Unchoke -> lprintf "Unchoke\n"
  | Interested -> lprintf "Interested\n"
  | NotInterested -> lprintf "NotInterested\n"
  | Have n -> lprintf "Have %Ld\n" n
  | BitField s -> lprintf "BitField %s\n" (String.escaped s)
  | Request (index, offset, len) -> 
      lprintf "Request %d %Ld[%Ld]\n" index offset len
  | Piece (index, offset, s, pos, len) -> 
      lprintf "Piece %d %Ld[%d]\n" index offset len
  | Cancel _ -> lprintf "Cancel\n"
  | Ping   -> lprintf "Ping\n"
    
let bt_parser opcode m = 
  match opcode with
    0 -> Choke
  | 1 -> Unchoke
  | 2 -> Interested
  | 3 -> NotInterested
  | 4 -> Have (get_int64_32 m 0)
  | 5 -> BitField m
  | 6 -> Request (get_int m 0, get_int64_32 m 4, get_int64_32 m 8)
  | 7 -> Piece (get_int m 0, get_int64_32 m 4, m, 8, String.length m - 8)
  | _ -> raise Not_found
  
let bt_handler parse_fun handler sock =
    try
      let b = TcpBufferedSocket.buf sock in
      (*
    lprintf "BT HANDLER\n";
dump (String.sub b.buf b.pos b.len);
  *)
    try
      while b.len >= 4 do
        let msg_len = get_int b.buf b.pos in
        if b.len >= 4 + msg_len then
          begin
              TcpBufferedSocket.buf_used sock 4;
(*              lprintf "Message complete\n"; *)
            if msg_len > 0 then 
              let opcode = get_int8 b.buf b.pos in
              let payload = String.sub b.buf (b.pos+1) (msg_len-1) in
              TcpBufferedSocket.buf_used sock msg_len;
(*              lprintf "Opcode %d\n" opcode; *)
                try
                  let p = parse_fun opcode payload in
(*              lprintf "Parsed, calling handler\n"; *)
                  handler sock p
                with Not_found -> ()
          end
        else raise Not_found
      done
    with 
    | Not_found -> ()
    with e ->
        lprintf "Exception %s in bt_handler\n"
          (Printexc2.to_string e)
        
let handlers info gconn =
  let rec iter_read sock nread =
(*    lprintf "iter_read %d\n" nread; *)
    let b = TcpBufferedSocket.buf sock in
    if b.len > 0 then
      match gconn.gconn_handler with
      | BTHeader h ->
          
(*          dump (String.sub b.buf b.pos (min b.len 100)); *)
          let slen = get_int8 b.buf b.pos in
          if slen + 49 <= b.len then
            let proto = String.sub b.buf (b.pos+1) slen in
            let file_id = Sha1.direct_of_string 
                (String.sub  b.buf (b.pos+9+slen) 20) in
            let peer_id = Sha1.direct_of_string 
                (String.sub  b.buf (b.pos+29+slen) 20) in
            let proto,pos = get_string8 b.buf b.pos in
            TcpBufferedSocket.buf_used sock (slen+49);
            h gconn sock (proto, file_id, peer_id);
            if not (TcpBufferedSocket.closed sock) then 
              iter_read sock 0

      | Reader h -> 
          let len = b.len in
          h gconn sock;
          if b.len < len then iter_read sock 0
  in
  iter_read
  
let set_bt_sock sock info ghandler = 
  let gconn = {
      gconn_handler = ghandler;
      gconn_refill = [];
      gconn_close_on_write = false;
    } in
  TcpBufferedSocket.set_reader sock (handlers info gconn);
  TcpBufferedSocket.set_refill sock (fun sock ->
      match gconn.gconn_refill with
        [] -> ()
      | refill :: _ -> refill sock
  );
  TcpBufferedSocket.set_handler sock TcpBufferedSocket.WRITE_DONE (
    fun sock ->
      match gconn.gconn_refill with
        [] -> ()
      | _ :: tail -> 
          gconn.gconn_refill <- tail;
          match tail with
            [] -> 
              if gconn.gconn_close_on_write then 
                set_lifetime sock 30.
(*                TcpBufferedSocket.close sock "write done" *)
          | refill :: _ -> refill sock)
              
      
  
(*  
No payload:
    * 0 - choke: you have been blocked
    * 1 - unchoke: you have been unblocked
    * 2 - interested: I'm interested in downloading this file now
    * 3 - not interested: I'm not interested in downloading this file now
With bencoded payload:
    * 4 - have
          int : index of new completed chunk          
    * 5 - bitfield: 
          string: a bitfield of bit 1 for downloaded chunks
          byte: bits are inverted 0....7 ---> 7 .... 0      
    * 6 - request
          int: index
          int: begin
          int: length (power of 2, 2 ^ 15)
    * 7 - piece
          int: index
          int: begin
          string: piece
    * 8 - cancel: cancel a requesu
          int: index
          int: begin
          int: length (power of 2, 2 ^ 15)
*)

let buf = Buffer.create 100
let send_client c msg =
  try
    match c.client_sock with 
      NoConnection | ConnectionWaiting | ConnectionAborted -> 
        failwith "Client is not connected\n"
(*      lprintf "send_client: not connected\n";        *) 
    | Connection sock ->
        Buffer.clear buf;
(*        lprintf "send_client\n";         *)
        begin
          buf_int buf 0;
          match msg with
          | Choke -> buf_int8 buf 0
          | Unchoke -> buf_int8 buf 1
          | Interested -> buf_int8 buf 2
          | NotInterested -> buf_int8 buf 3
          | Have i -> buf_int8 buf 4; buf_int64_32 buf i
          | BitField string -> buf_int8 buf 5; Buffer.add_string buf string
          | Request (index, pos, len) ->
              buf_int8 buf 6; 
              buf_int buf index; buf_int64_32 buf pos; buf_int64_32 buf len
          | Piece (num, index, s, pos, len) ->
              buf_int8 buf 7; 
              buf_int buf num;
              buf_int64_32 buf index; 
              Buffer.add_substring buf s pos len
          
          | Cancel _ -> ()
          | Ping -> ()          
        end;
        let s = Buffer.contents buf in
        str_int s 0 (String.length s - 4);
        if !verbose_msg_clients then begin
            lprintf "CLIENT %d: Sending "
              (client_num c);
            bt_print msg;
          end;
(*        dump s; *)
        write_string sock s
  with e ->
      lprintf "CLIENT %d: Error %s in send_client\n" (client_num c)
        (Printexc2.to_string e)
      
let zero8 = String.make 8 '\000'
  
let send_init file c sock = 
  let buf = Buffer.create 100 in
  buf_string8 buf  "BitTorrent protocol";
  Buffer.add_string buf zero8;
  Buffer.add_string buf (Sha1.direct_to_string file.file_id);
  Buffer.add_string buf (Sha1.direct_to_string c.client_uid);
  let s = Buffer.contents buf in
  write_string sock s
  
