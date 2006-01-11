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

open Int64ops
open Options
open Md4
open Printf2

open BasicSocket
open LittleEndian
open AnyEndian
open TcpBufferedSocket

open GnutellaNetwork
open GnutellaTypes
open GnutellaGlobals
open GnutellaOptions

open CommonTypes
open CommonGlobals
open CommonOptions
open CommonShared
open CommonUploads
  
(* replace [header_] by [hsrep_] *)
type handshake_reply = {
    mutable hsrpl_ultrapeer : bool;    
    mutable hsrpl_agent : string;
    mutable hsrpl_query_key : query_key;
    mutable hsrpl_content_type : string;
    mutable hsrpl_gnutella2 : bool;
    mutable hsrpl_accept_deflate : bool;
    mutable hsrpl_content_deflate : bool;
    mutable hsrpl_ultrapeer_needed : bool;
  }

type handshake_request = {
    mutable hsreq_local_address : string;
    mutable hsreq_remote_address : string;
    mutable hsreq_ultrapeer : bool;
    mutable hsreq_ultrapeer_needed : bool;
    mutable hsreq_accept_deflate : bool;
    mutable hsreq_content_deflate : bool;
  }


module QrtReset = struct
(*
    struct gnutella_qrp_reset {
        guchar table_length[4]; /* little endian */
        guchar infinity;
} __attribute__((__packed__));
*)
    
    type t = {
        table_length : int;
        infinity : int;
      }
    
    let parse s = 
      { table_length = get_int s 1;
        infinity = get_uint8 s 5;
      }
    
    let print t = 
      lprintf "QRT Reset %d %d" t.table_length t.infinity
    
    let write buf t = 
      buf_int buf t.table_length;
      buf_int8 buf t.infinity
  
  end

module QrtPatch = struct
(*
struct gnutella_qrp_patch {
        guchar seq_no;
        guchar seq_size;
        guchar compressor;
        guchar entry_bits;
} __attribute__((__packed__));
*)
    
    type t = {
        seq_no : int; (* char *)
        seq_size : int; (* char *)
        compressor : int; (* char *)
        entry_bits : int; (* char *)
        table : string;
      }
    
    let parse s = 
(*      lprintf "RECEIVING QRT TABLE SIZE %d\n" (String.length s - 4); *)
      {
        seq_no = get_uint8 s 1;
        seq_size = get_uint8 s 2;
        compressor = get_uint8 s 3;
        entry_bits = get_uint8 s 4;
        table = String.sub s 5 (String.length s - 5);
      }
    
    let patches = ref ""
    
    let print buf t = 
      Printf.bprintf buf "QRT PATCH:\n";
      Printf.bprintf buf "  seq_no: %d/%d\n" t.seq_no t.seq_size;
      Printf.bprintf buf "  compressor: %d\n" t.compressor;
      Printf.bprintf buf "  entry bits: %d\n" t.entry_bits;
      Printf.bprintf buf "  table: ";
      if t.seq_no = 1 then patches := t.table
      else patches := !patches ^ t.table;
      if t.seq_no = t.seq_size then begin
          try
            if t.compressor < 2 then
              let table = 
                if t.compressor = 1 then
                  Autoconf.zlib__uncompress_string2 !patches
                else
                  !patches
              in
              let nbits = ref 0 in
              for i = 0 to String.length table - 1 do
                let c = int_of_char table.[i] in
                for j = 0 to 7 do
                  if (1 lsl j) land c <> 0 then begin
                      incr nbits;
                      Printf.bprintf buf "(%d)" (i*8+j);
                    end
                done
              done;
              Printf.bprintf buf "  = %d bits\n" !nbits               
            else
              Printf.bprintf buf " (compressed) \n"
          with e ->
              Printf.bprintf buf " (exception %s)\n"
                (Printexc2.to_string e)
        end else
        Printf.bprintf buf " (partial) \n"            
    
    let write buf t = 
      buf_int8 buf t.seq_no;
      buf_int8 buf t.seq_size;
      buf_int8 buf t.compressor;
      buf_int8 buf t.entry_bits;
(*      lprintf "SENDING QRT TABLE SIZE %d\n" (String.length t.table); *)
      Buffer.add_string buf t.table
  
  end

let gnutella_ok = "GNUTELLA OK"     
let gnutella_200_ok = "GNUTELLA/0.6 200 OK"
let gnutella_503_shielded = "GNUTELLA/0.6 503 I am a shielded leaf node"

let add_header_fields header sock trailer =
  let buf = Buffer.create 100 in
  Printf.bprintf buf "%s" header;
  Printf.bprintf buf "User-Agent: %s\r\n" user_agent;
  Printf.bprintf buf "X-My-Address: %s:%d\r\n"
    (Ip.to_string (client_ip (Some sock))) !!client_port;
  Printf.bprintf buf "X-Ultrapeer: False\r\n";
  Printf.bprintf buf "X-Query-Routing: 0.1\r\n";
  Printf.bprintf buf "GGEP: 0.5\r\n";
  Printf.bprintf buf "%s" trailer;
  Buffer.contents buf

let set_gnutella_sock sock info ghandler = 
  let gconn = {
      gconn_file_info_sent = [];
      gconn_client_info_sent = false;
      gconn_handler = ghandler;
      gconn_refill = [];
      gconn_close_on_write = false;
      gconn_verbose = ref false;
    } in
  TcpBufferedSocket.set_reader sock (GnutellaFunctions.handlers info gconn);
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

    
let bloom_hash_magic = Int32.of_string  "0x4F1BBCDC"
let bloom_hash_magic_int64 =  int64_of_uint32 bloom_hash_magic

let bloom_hash_fast x bits =
  let xx = int64_of_uint32 x in
  let prod = xx ** bloom_hash_magic_int64 in
  let ret = Int64.shift_left prod  32 in     (* prod << 32 *)
  Int64.shift_right_logical ret (32 + (32 - bits))   (* ret >>> (32 + (32 - bits))  *)

let bloom_hash_full s pos len bits =
  let xor = ref Int32.zero in
  let j = ref 0 in
  for i = pos to len - 1 do
    let b = Int32.of_int (int_of_char (Char.lowercase s.[i])) in
    let b = Int32.shift_left b (!j * 8) in
    xor := Int32.logxor !xor b;
    j := (!j+1) mod 4;
  done;
  bloom_hash_fast !xor bits
  
let bloom_hash s bits = bloom_hash_full s 0 (String.length s) bits

  
module WordSet = Set.Make(struct
      type t = string
      let compare = compare
    end)
  
let new_shared_words = ref false
let all_shared_words = ref []
let cached_qrt_table = ref ""

let update_shared_words () = 
  if !verbose_share then
    lprintf_nl () "update_shared_words";
  all_shared_words := [];
  cached_qrt_table := "";
  let module M = CommonUploads in
  let words = ref WordSet.empty in
  let register_words s = 
    let ws = String2.stem s in
    List.iter (fun w ->
        words := WordSet.add w !words
    ) ws
  in
  let rec iter node =
    List.iter (fun sh ->
        let info = IndexedSharedFiles.get_result sh.shared_info in
        if !verbose_share then
          lprintf_nl () "CODED name: %s" sh.M.shared_codedname;
        register_words sh.M.shared_codedname;
        List.iter (fun uid ->
            words := WordSet.add (Uid.to_string uid) !words
        ) info.M.shared_uids;
    ) node.M.shared_files;
    List.iter (fun (_,node) ->
        register_words node.M.shared_dirname;
        iter node
    ) node.M.shared_dirs;
  in
  iter M.shared_tree;
  WordSet.iter (fun s ->
      all_shared_words := s :: !all_shared_words
  ) !words;
  if !verbose_share then
    begin
      lprintf "SHARED WORDS: ";
      List.iter (fun s ->
        lprintf "%s " s
      ) !all_shared_words;
      lprint_newline ()
    end

        
(*

How shareaza knows the correspondances between URNs:
  
ascii:[
HTTP/1.1 503 Busy
Server: Shareaza 1.8.8.0
Remote-IP: 81.100.86.143
Connection: Keep-Alive
Accept-Ranges: bytes
X-PerHost: 2
X-Nick: Billout666
X-Content-URN: 
  urn:bitprint:UK4BIKSOX5D3NCW35GYALUHATU54CDIV.ETU43VCR3TNKVZRM3UNSK5GTB5HXAVMFUWWCZYY
X-Content-URN: 
  ed2k:9a94c7895897ffa5d7cfe580bea97286
X-TigerTree-Path: 
  /gnutella/tigertree/v3?urn:tree:tiger/:ETU43VCR3TNKVZRM3UNSK5GTB5HXAVMFUWWCZYY
X-Thex-URI: 
  /gnutella/thex/v1?urn:tree:tiger/:ETU43VCR3TNKVZRM3UNSK5GTB5HXAVMFUWWCZYY&depth=9&ed2k=0
Content-Type: text/html
Content-Length: 2125]

*)
 
let parse_headers c first_line headers =

      try
        
        let (locations,_) = 
          List.assoc "x-gnutella-alternate-location" headers in
        let locations = String2.split locations ',' in
        
        lprintf_nl () "Alternate locations";
        let urls = List.map (fun s ->
              match String2.split_simplify s ' ' with
                [] -> lprintf_nl () "Cannot parse : %s" s; ""
              | url :: _ ->
                  lprintf_nl () "  Location: %s" url; url
          ) locations in
        lprint_newline ();
        
        let files = ref [] in
        (try
            let (urn,_) = List.assoc "x-gnutella-content-urn" headers in

(* Contribute: maybe we can find the bitprint associated with a SHA1 here,
  and use it for corruption detection... *)
            
            
            
            let uids = Uid.expand [Uid.of_string urn] in
            List.iter (fun uid ->
                try
                  files := (Hashtbl.find files_by_uid uid) :: !files
                
                with _ -> ()
            ) uids
          with Not_found ->
              match c.client_requests with 
                d :: _ -> files := d.download_file :: !files
              | _ -> ()
        );
        
        List.iter (fun file ->
            List.iter (fun url ->
                try
                  let url = Url.of_string url in
                  let ip = Ip.of_string url.Url.server in
                  let port = url.Url.port in
                  let uri = url.Url.full_file in
                  
                  let c = new_client (Known_location (ip,port)) in
                  add_download file c (FileByUrl uri)
                
                with _ -> ()
            ) urls
        ) !files
      
      with _ -> ()
    
(*                
Shareaza adds:
X-TigerTree-Path: /gnutella/tigertree/v2?urn:tree:tiger/:YRASTJJK6JPRHREV3JGIFLSHSQAYDODVTSJ4A3I
X-Metadata-Path: /gnutella/metadata/v1?urn:tree:tiger/:7EOOAH7YUP7USYTMOFVIWWPKXJ6VD3ZE633C7AA
*)

open CommonUploads
      
let headers_of_shared_file gconn sh = 
  let headers = ref [] in
  if not (List.mem sh.shared_id gconn.gconn_file_info_sent) then 
    begin
      gconn.gconn_file_info_sent <- 
        sh.shared_id :: gconn.gconn_file_info_sent;
      headers := 
        List.map (fun uid ->
            ("X-Content-URN", Uid.to_string uid)
        ) sh.shared_uids;
      
      match sh.shared_bitprint with
        None -> ()
      | Some bp ->
          let s = Uid.to_string bp in
          headers := 
          ("X-Thex-Uri", Printf.sprintf "/uri-res/N2T?%s" s) :: !headers
    end;
  !headers
    
      
let request_of_download request d =
  let s =   
    (match d.download_uri with
        FileByUrl url -> Printf.sprintf "%s %s HTTP/1.1" request url
      | FileByIndex (index, name) -> 
          Printf.sprintf "%s /get/%d/%s HTTP/1.1" request index name)
  in
  s
  
let make_download_request c s headers =
  let headers =
    ("User-Agent", user_agent) ::
    ("Connection", "Keep-Alive") ::
    (match c.client_host with
        None -> headers
      | Some (ip, port) ->
          ("Host", Printf.sprintf "Host: %s:%d" (Ip.to_string ip) port) :: headers
    )
  in
  make_http_header s headers
  
(*
  
                  match c.client_downloads with
                    [] -> 
(* Here, we should probably browse the client or reply to
an upload request *)
                      
                      if !verbose_msg_clients then begin
                          lprintf "NOTHING TO DOWNLOAD FROM CLIENT\n";
                        end;
                      
                      if client_browsed_tag land client_type c = 0 then
                        disconnect_client c (Closed_for_error "Nothing to download");
                      set_gnutella_sock sock !verbose_msg_clients
                        (HttpHeader (friend_parse_header c));
                      let s = GnutellaProtocol.add_header_fields 
                          "GNUTELLA CONNECT/0.6\r\n" sock 
                          (Printf.sprintf "Remote-IP: %s\r\n\r\n" (Ip.to_string ip))
                      in
(*
        lprintf "SENDING\n";
        AP.dump s;
  *)
                      write_string sock s;
                  
                  
                  | d :: _ ->
                      if !verbose_msg_clients then begin
                          lprintf "READY TO DOWNLOAD FILE\n";
                        end;
                      
                      
                      List.iter (fun d ->
                          let file = d.download_file in
                          if file_size file <> zero then
                            let swarmer = match file.file_swarmer with
                                None -> assert false | Some sw -> sw
                            in
                            let chunks = [ Int64.zero, file_size file ] in
                            let bs = Int64Swarmer.register_uploader swarmer 
                                (as_client c)
                                (AvailableRanges chunks) in
                            d.download_uploader <- Some bs
                      ) c.client_downloads;
                      
                      
                      get_from_client sock c;
*)
  
(*  
and friend_parse_header c gconn sock header =
  try
    if String2.starts_with header gnutella_200_ok then begin
        set_rtimeout sock half_day;
        let lines = Http_client.split_header header in
        match lines with
          [] -> raise Not_found        
        | _ :: headers ->
            let headers = Http_client.cut_headers headers in
            let (agent,_) =  List.assoc "user-agent" headers in
            if String2.starts_with agent "LimeWire" ||
              String2.starts_with agent "Gnucleus" ||
              String2.starts_with agent "BearShare"              
            then
              begin
(* add_peers headers; *)
                write_string sock "GNUTELLA/0.6 200 OK\r\n\r\n";
                if !verbose_msg_clients then begin
                    lprintf "********* READY TO BROWSE FILES *********\n";
                  end;
(*
                gconn.gconn_handler <- Reader
(gnutella_handler parse (client_to_client c))
  *)
              end
            else raise Not_found
      end 
    else raise Not_found
  with e -> 
      lprintf "Exception %s in friend_parse_header\n" 
        (Printexc2.to_string e); 
      disconnect_client c (Closed_for_exception e)
*)
  
let find_file_to_upload gconn url =
  let file = url.Url.short_file in
  
  if file = "/uri-res/N2T" then
    match url.Url.args with
      [(urn,_)] ->
        let uid = Uid.of_string urn in
        let urn = Uid.to_file_string uid in
        let filename = Filename.concat "ttr" urn in
        if Sys.file_exists filename then
          let fd = Unix32.create_ro filename in
          
          (fun pos upload_buffer spos rlen ->
              Unix32.read fd pos upload_buffer spos rlen),
          Unix32.getsize64 fd, []
        else
          failwith (Printf.sprintf "Cannot find TigerTree [%s]" urn)
    | _ -> failwith "Cannot parse /uri-res/N2T request"
  else
  
  
  let sh =
    if file = "/uri-res/N2R" then
      match url.Url.args with
        [(urn,_)] ->
          if !verbose_msg_clients then
            lprintf_nl () "Found /uri-res/N2R request";
          find_by_uid (Uid.of_string urn)
      
      | _ -> failwith "Cannot parse /uri-res/N2R request"
    else
    let get = String.lowercase (String.sub file 0 5) in
    assert (get = "/get/");
    let pos = String.index_from file 5 '/' in
    let num = String.sub file 5 (pos - 5) in
    let filename = String.sub file (pos+1) (String.length file - pos - 1) in
    if !verbose_msg_clients then
      lprintf_nl () "Download of file %s, filename = %s" num filename;
    let num = int_of_string num in
    Hashtbl.find shareds_by_id  num
  in
  
  let info = IndexedSharedFiles.get_result sh.shared_info in  
  let impl = sh.shared_impl in
  impl.impl_shared_requests <- impl.impl_shared_requests + 1;
  shared_must_update_downloaded (as_shared impl);
  
  (fun pos upload_buffer spos rlen ->
      
      let impl = sh.shared_impl in
      impl.impl_shared_uploaded <- 
        impl.impl_shared_uploaded ++ (Int64.of_int rlen);
      shared_must_update_downloaded (as_shared impl);
      
      Unix32.read sh.shared_fd pos upload_buffer spos rlen),
  info.shared_size,
  
  headers_of_shared_file gconn info
