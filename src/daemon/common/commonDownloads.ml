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
open CommonClient
open CommonComplexOptions
open CommonTypes
open CommonFile
open Options
open BasicSocket
open TcpBufferedSocket

open CommonGlobals
open CommonOptions
  
module SimpleDownload  = struct
  
(*
A common function for all networks were the file is got in one single piece,
  from only one client  and the connection is closed at the end.
*)
    
    type ('file,'client) download = {
        download_file : 'file; (* the file being downloaded *)
        download_client : 'client;
        mutable download_min_read : int;
        mutable download_pos : int64; (* the position in the file *)
        mutable download_sock : TcpBufferedSocket.t option;
      }
    
    module Make(M: sig
          type f
          type c
          val file : f -> file
          val client :   c -> client
          val client_disconnected :  (f, c) download -> unit
          val subdir_option : string Options.option_record
          val download_finished :  (f, c) download -> unit
        end) = 
      struct
        
        let disconnect_download (d : (M.f, M.c) download) reason =
          match d.download_sock with
            None -> ()
          | Some sock ->
              close sock reason;
              (try M.client_disconnected d with _ -> ());
              lprintf "DISCONNECTED FROM SOURCE\n"; 
              d.download_sock <- None
        
        let file_complete d =
(*
  lprintf "FILE %s DOWNLOADED\n" f.file_name;
  *)
          file_completed (M.file d.download_file);
          (try M.download_finished d with _ -> ())
        
        let download_reader d sock nread = 
          lprint_string ".";
          if d.download_sock = None then  raise Exit;
          let file = M.file d.download_file in
          if nread >= d.download_min_read then
            let b = TcpBufferedSocket.buf sock in
            d.download_min_read <- 1;
            set_rtimeout sock 120.;
            set_client_state (M.client d.download_client) 
            (Connected_downloading);
(*
        begin
          let fd = try
              Unix32.force_fd (file_fd file) 
            with e -> 
                lprintf "In Unix32.force_fd\n"; 
                raise e
          in
          let final_pos = Unix32.seek64 (file_fd file) d.download_pos
              Unix.SEEK_SET in *)
            Unix32.write (file_fd file) d.download_pos b.buf b.pos b.len;
(*        end; *)
(*      lprintf "DIFF %d/%d\n" nread b.len; *)
            d.download_pos <- Int64.add d.download_pos (Int64.of_int b.len);
(*
      lprintf "NEW SOURCE POS %s\n" (Int64.to_string c.client_pos);
  *)
            TcpBufferedSocket.buf_used b b.len;
            if d.download_pos > file_downloaded file then 
              add_file_downloaded (as_file_impl file)
              (Int64.sub d.download_pos (file_downloaded file));
            if file_downloaded file = file_size file then
              file_complete d
        
        
        let new_download sock (c :M.c) (file : M.f) min_read =
          let d = {
              download_client = c;
              download_file = file;
              download_sock = Some sock;
              download_pos = file_downloaded (M.file file);
              download_min_read = min_read;
            } in
          set_closer sock (fun _ reason -> disconnect_download d reason);
          TcpBufferedSocket.set_read_controler sock download_control;
          TcpBufferedSocket.set_write_controler sock upload_control;
          set_rtimeout sock 30.;
          d  
      
      end
  
  end


open Md4
open CommonSwarming
  
module SharedDownload = struct

(* This module defines a file that can be downloaded from several network
simultaneously. Thus, it defines a superset of the operations that are
possible on files in all networks.

Currently, it is used by the Fasttrack, Gnutella and BitTorrent plugins.

Each plugin must provide the two sets of operation 'network_file' and
'file_network', and register itself using the 'register_network' function.
When a new download is created, the 'op_download_start' function of
each registered network is called, and this function must try to
identify one the 'file_uids' so that it can locate the file on its
network.
  *)
    
    type bitzi_ticket =
    | Bitzi_next_retry of int (* retry after this date *)
    | Bitzi_not_found     (* don't retry *)
    | Bitzi_ticket of string
    
    
    type impl
    
    type file = {        
        file_file : file CommonFile.file_impl;
        file_id : string;
        mutable file_name : string;
        file_swarmer : Int64Swarmer.t;
        
        mutable file_uids : file_uid list;
        mutable file_filenames : string list;
        
        mutable file_impls : (impl network_file * impl) list;
        mutable file_files : (string * int64 * int64) list;
        mutable file_verified_partition : Int64Swarmer.partition;  
        mutable file_bitzi_ticket : bitzi_ticket;
      }
    
    and 'a network_file = {
        mutable op_download_network : file_network;
        mutable op_download_finish : ('a -> unit); 
        mutable op_download_recover : ('a -> unit);
        mutable op_download_pause : ('a -> unit);
        mutable op_download_resume : ('a -> unit);
        mutable op_download_sources : ('a -> client list);
        mutable op_download_debug : ('a -> Buffer.t -> unit);
        mutable op_download_to_value : ('a -> (string * option_value) list);
      }
    
    and file_network = {
        mutable op_download_netname : string;
        mutable op_download_start : (file -> unit);
        mutable op_download_of_value : (file ->
        (string * option_value) list -> unit);
      }
    
    let file_num file = file.file_file.impl_file_num
    let file_size file = file.file_file.impl_file_size
    let file_downloaded file = file_downloaded (as_file file.file_file)
    let file_age file = file.file_file.impl_file_age
    let file_fd file = file.file_file.impl_file_fd
    let file_disk_name file = file_disk_name (as_file file.file_file)
    let set_file_disk_name file = set_file_disk_name (as_file file.file_file)
    let file_state file =  file_state (as_file file.file_file)      
    let set_file_state file =  set_file_state (as_file file.file_file)      
    let file_must_update file =
      file_must_update (as_file file.file_file)
    let file_priority file = file.file_file.impl_file_priority
    let file_best_name file = file_best_name (as_file file.file_file)
    let set_file_best_name file = set_file_best_name (as_file file.file_file)
    let file_add_source file = file_add_source (as_file file.file_file)      
    let file_last_seen file = file.file_file.impl_file_last_seen
    let set_file_last_seen file v = file.file_file.impl_file_last_seen <- v
    let file_must_update_downloaded file = 
      file_must_update_downloaded (as_file  file.file_file)
    let as_file file = as_file file.file_file
    
    let network = CommonUploads.network
    
    let (file_ops : file CommonFile.file_ops) = 
      CommonFile.new_file_ops network    
    
    let networks = ref []
    let current_files = ref []
    let files_by_num = Hashtbl.create 13
    
    let register_network f =
      networks := f :: !networks;
      ()
    
    let new_network_file n = {
        op_download_finish = (fun _ -> ());
        op_download_sources = (fun _ -> []);
        op_download_recover = (fun _ -> ());
        op_download_pause = (fun _ -> ());
        op_download_resume = (fun _ -> ());
        op_download_to_value = (fun _ -> []);
        op_download_debug = (fun _ _ -> ());
        op_download_network = n;
      }
    
    let new_file_network name = {
        op_download_netname = name;
        op_download_start = (fun _ -> ());
        op_download_of_value = (fun  _ _ -> ());
      }
    
    let add_file_impl file (f : 'a network_file) (v : 'a) =
      file.file_impls <- (Obj.magic (f,v)) :: file.file_impls
    
    
    
    
    
    let add_uids file uids =
      List.iter (fun uid ->
          let uid = uid_of_uid uid in
          if not (List.mem uid file.file_uids) then
            file.file_uids <- uid :: file.file_uids
      ) uids
    
    module Bitzi = struct
(* 
  
To find equivalent UIDs on different networks, we can use www.bitzi.com:

http://ticket.bitzi.com/rdf/BCLD3DINKJJRGYGHIYAX7HG5HXSM3XNH.E4IHTEMZIJE4NBCWSBZ6TIWQTDGYYXVPGIRJ5KQ
http://ticket.bitzi.com/rdf/urn:sha1:BCLD3DINKJJRGYGHIYAX7HG5HXSM3XNH 
  
  # let t = Xml.parse_file "xml1";;
val t : Xml.xml =
  Xml.XML ("rdf:RDF",
   [("xmlns:rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#");
    ("xmlns:rdfs", "http://www.w3.org/2000/01/rdf-schema#");
    ("xmlns:dc", "http://purl.org/dc/elements/1.1/");
    ("xmlns:bz", "http://bitzi.com/xmlns/2002/01/bz-core#");
    ("xmlns:mm", "http://musicbrainz.org/mm/mm-2.0#")],
   [Xml.XML ("bz:Authentication", [], []);
    Xml.XML ("rdf:Description",
     [("rdf:about", "urn:sha1:BCLD3DINKJJRGYGHIYAX7HG5HXSM3XNH");
      ("bz:ticketMinted", "2003-06-08 22:15:33 GMT");
      ("bz:ticketExpires", "2003-06-09 22:15:33 GMT");
      ("bz:ticketFirstCreated", "2001-07-15 23:48:07 GMT");
      ("bz:ticketFilename",
      "BCLD3DINKJJRGYGHIYAX7HG5HXSM3XNH-200306092215.bztk");
      ("bz:ticketRequestsDay", "14"); ("bz:ticketOutboundClicksDay", "0");
      ("bz:fileGoodness", "2.2"); ("bz:fileJudgement", "Best Version");
      ("dc:format", "audio/mp3"); ("bz:fileLength", "4577608");
("bz:fileFirst20Bytes", "4944330200000000006E54543200000F00546865");

  
("bz:fileMD5", "NTUHNJDIJH6IDHQ5GPOSUFVSCM");
("bz:fileUUHash", "mcR96OHbEJLjxV4eudW9YDMW8q4");
("bz:fileED2kHash", "83d6c3bef2732d310bd9105e850c7864");

  
      ("mm:duration", "286743"); ("bz:audioBitrate", "128");
      ("bz:audioSamplerate", "44100"); ("bz:audioChannels", "1");
      ("bz:audioSha1", "LQAIDHEMU43QFN5JNYFAVFEYZI3TOZ6L")],
     [Xml.XML ("dc:description",
       [("dc:title", "The Bjork Song");
        ("dc:creator", "The Brunching Shuttlecocks");
        ("bz:albumName", "www.brunching.com"); ("mm:trackNum", "1");
        ("dc:date", "2000")],
       []);
      Xml.XML ("bz:fileName",
       [("dc:title", "Brunching Shuttlecocks - Bjorksong.mp3")], []);
      Xml.XML ("bz:fileName", [("dc:title", "bjorksong.mp3")], []);
      Xml.XML ("bz:sponsor",
       [("rdf:resource",
        "http://bitzi.com/r/s?u=http%3A%2F%2Fwww.cafeshops.com%2Fcp%2Fstore.aspx%3Fs%3Dbitzi&#38;v=0&#38;x=NDZ7243XSSFBHLWUVDCCRGJ3UA7CWDREHE2DINZVGUQDCMRYEAYCAMBAGA");
        ("dc:title", "New: Bitzi Logo Stuff");
        ("bz:sponsorText1", "T-shirts, hats, bags, and more ");
        ("bz:sponsorText2", "Support Bitzi -- show us your bits!");
        ("bz:sponsorSite", "www.cafeshops.com"); ("bz:sponsorRank", "1");
        ("bz:sponsorClickCost", "affiliate")],
       [])])])

  *)
        
        let make_request url = 
          
          let module H = Http_client in
          let r = {
              H.basic_request with
              
              H.req_url = Url.of_string url;
              H.req_user_agent = "Mozilla/5.0 (Linux 2.4.19-16mdk i686; U) Opera 6.11  [en]";
              H.req_referer = None;
              H.req_request = H.GET;
            } in
          
          r
        
        let progress _ _ = ()
        
        let parse_bitzi_ticket file s =
          lprintf "Bitzi ticket downloaded:%s\n" (String.escaped s);
          let xml = Xml.parse_string s in
          file.file_bitzi_ticket <- Bitzi_ticket s;
          let Xml.XML (_,_, list) = xml in
          List.iter (fun (Xml.XML (header, args, _)) ->
              match header with
                "rdf:Description" -> 
(*                                 
("bz:fileMD5", "NTUHNJDIJH6IDHQ5GPOSUFVSCM");
("bz:fileUUHash", "mcR96OHbEJLjxV4eudW9YDMW8q4");
("bz:fileED2kHash", "83d6c3bef2732d310bd9105e850c7864");
*)
                  List.iter (fun (field, value) ->
                      match field with
                      | "bz:fileMD5" -> ()
                      | "bz:fileUUHash" ->
                          let md5ext = Md5Ext.of_string ("=" ^ value ^ "=") in
                          add_uids file [Md5Ext ("", md5ext)]
                      | "bz:fileED2kHash" ->
                          let md4 = Md4.of_string value in
                          add_uids file [Ed2k ("", md4)]
                      
                      | _ -> ()
                  ) args;
                  List.iter (fun f -> 
                      try f.op_download_start file 
                      with _ -> ()) !networks;                            
              | _ -> ()
          ) list

(*
>  http://bitzi.com/lookup/OILFTPP7BIHSFNQHEAJ3GZOTSBOCN57P
>  http://bitzi.com/lookup/ed2k:0dfdf429ecec79510d9baf9f517d5ece
>  http://bitzi.com/lookup/sig2dat:egOkYJlkawkKLl9vtqPGB44HL5M
*)
        
        let parse_possible_bitzi_ticket sha1 file s =
          lprintf "****** parse_possible_bitzi_ticket *******\n";
          lprintf "Bitzi ticket downloaded:%s\n" (String.escaped s);
          let xml = Xml.parse_string s in
          file.file_bitzi_ticket <- Bitzi_ticket s;
          let Xml.XML (_,_, list) = xml in
          let found_uids = ref [uid_of_uid (Sha1 ("", sha1))] in
          let found_file_size = ref zero in
          List.iter (fun (Xml.XML (header, args, _)) ->
              match header with
                "rdf:Description" -> 
(*                                 
("bz:fileMD5", "NTUHNJDIJH6IDHQ5GPOSUFVSCM");
("bz:fileUUHash", "mcR96OHbEJLjxV4eudW9YDMW8q4");
("bz:fileED2kHash", "83d6c3bef2732d310bd9105e850c7864");
*)
                  List.iter (fun (field, value) ->
                      match field with
                      | "bz:fileMD5" -> ()
                      | "bz:fileUUHash" ->
                          let md5ext = Md5Ext.of_string ("=" ^ value ^ "=") in
                          found_uids := (uid_of_uid (Md5Ext ("", md5ext))) :: !found_uids
                      | "bz:fileED2kHash" ->
                          let md4 = Md4.of_string value in
                          found_uids := (uid_of_uid (Ed2k ("", md4))) :: !found_uids
                      | "bz:fileLength" ->
                          found_file_size := Int64.of_string value
                      | _ -> ()
                  ) args;
              | _ -> ()
          ) list;
          if !found_file_size = file_size file then
            List.iter (fun uid ->
                if List.mem uid file.file_uids then begin
(* One of the uids found match the file, it's our file ! *)
                    lprintf "********** possible bitzi ticket match file\n";
                    add_uids file !found_uids;
                    List.iter (fun f -> 
                        try f.op_download_start file 
                        with _ -> ()) !networks;                            
                  
                  end
            ) !found_uids
        
        let request_possible_bitzi_ticket file sha1 =
          lprintf "***** request_possible_bitzi_ticket *****\n";
          Http_client.wget_string 
            (make_request
              (Printf.sprintf "http://ticket.bitzi.com/rdf/urn:sha1:%s"
                (Sha1.to_string sha1))) 
          (parse_possible_bitzi_ticket sha1 file) progress
        
        let parse_bitzi_lookup file s = 
          lprintf "******* parse_bitzi_lookup *******\n%s\n"
            (String.escaped s);
          let rec iter pos =
            let pos = (String2.search_from s pos "urn:sha1:")+9 in
            let sha1 = String.sub s pos 32 in
            lprintf "******** urn:sha1:%s found *********\n" sha1;
            let sha1 = Sha1.of_string sha1 in
            request_possible_bitzi_ticket file sha1;
            iter pos
          in
          try
            iter 0
          with _ -> ()
        
        let query_bitzi_ticket file =
          match file.file_bitzi_ticket with
            Bitzi_ticket xml -> ()
          | Bitzi_not_found -> ()
          | Bitzi_next_retry time ->
              if last_time () > time then begin
                  file.file_bitzi_ticket <- Bitzi_next_retry (last_time () + 600);
                  List.iter (fun uid ->
                      match uid with
                        Sha1 (_, sha1) ->
                          
                          lprintf "Retrieve bitzi ticket\n";
                          file.file_bitzi_ticket <- Bitzi_not_found;
                          Http_client.wget_string 
                            (make_request
                              (Printf.sprintf "http://ticket.bitzi.com/rdf/urn:sha1:%s"
                                (Sha1.to_string sha1))) 
                          (parse_bitzi_ticket file) progress
                      | _ -> ()
                  ) file.file_uids;
                  match file.file_bitzi_ticket with
                    Bitzi_next_retry _ ->
(* No Sha1 uid: try the ed2k or sig2dat uids  *)
                      List.iter (fun uid ->
                          match uid with
                            Ed2k (_, ed2k) ->
                              
                              lprintf "********** Retrieve bitzi lookup by ed2k\n";
                              Http_client.wget_string 
                                (make_request
                                  (Printf.sprintf "http://bitzi.com/lookup/ed2k:%s" (Md4.to_string ed2k)))
                              (parse_bitzi_lookup file) progress
                          | Md5Ext (_, md5ext) ->
                              
                              lprintf "********** Retrieve bitzi lookup by sig2dat\n";
                              let md5ext = Md5Ext.to_string md5ext in
                              let len = String.length md5ext in
                              Http_client.wget_string 
                                (make_request
                                  (Printf.sprintf "http://bitzi.com/lookup/sig2dat:%s" (String.sub md5ext 1 (len -2))))
                              (parse_bitzi_lookup file) progress
                          | _ -> ()
                      ) file.file_uids;
                  
                  
                  | _ -> ()
                end
        
        let minute_timer _ =
          List.iter (fun file ->
              query_bitzi_ticket file) !current_files
      
      end

(* We should have a Hashtbl keeping the files by their UID *)
    
    let min_range = Int64.of_int (256 * 1024)
    let int64_100 = Int64.of_int 100
    
    let new_download file_id file_name file_size old_file file_uids = 
      let file_id = match file_id with
          Some file_id -> file_id
        | None ->
            let file_id = ref None in
            List.iter (fun uid ->
                match uid with
                  Ed2k _ | Sha1 _ | Md5Ext _ ->
                    file_id := Some (string_of_uid uid)
                | _ -> ()
            ) file_uids;
            match !file_id with
              None -> Md4.to_string (Md4.random ())
            | Some file_id -> file_id
      in
      let file_temp = Filename.concat !!temp_directory 
          (Printf.sprintf "COMMON-%s" file_id) in
      
      (match old_file with
          None -> () 
        | Some old_file ->
            try
              Unix2.rename old_file file_temp;
              Unix.chmod file_temp 0o644;
            with e -> 
                lprintf "[ERROR] Exception %s while copying old file [%s]"
                  (Printexc2.to_string e) old_file
      );
      let t = Unix32.create file_temp [Unix.O_RDWR; Unix.O_CREAT] 0o666 in
      let swarmer = Int64Swarmer.create () in
      let chunk_size = max min_range (file_size // int64_100) in
      let partition = fixed_partition swarmer "display" chunk_size in
      let rec file = {
          file_file = file_impl;
          file_id = file_id;
          file_name = file_name;
          file_swarmer = swarmer;
          file_uids = expand_uids file_uids;
          file_filenames = [file_name];
          file_impls = [];
          file_files = [];
          file_verified_partition = partition;
          file_bitzi_ticket = Bitzi_next_retry 0;
        } and file_impl =  {
          dummy_file_impl with
          impl_file_fd = t;
          impl_file_size = file_size;
          impl_file_downloaded = Int64.zero;
          impl_file_val = file;
          impl_file_ops = file_ops;
          impl_file_age = last_time ();          
          impl_file_best_name = file_name;
        } 
      in
      Int64Swarmer.set_size swarmer file_size;  
      Int64Swarmer.set_writer swarmer (fun offset s pos len ->      
          
          if !!CommonOptions.buffer_writes then 
            Unix32.buffered_write_copy t offset s pos len
          else
            Unix32.write  t offset s pos len
      );
      current_files := file :: !current_files;
      file_add file_impl FileDownloading;
      Hashtbl.add files_by_num (file_num file) file;
(*      lprintf "ADD FILE TO DOWNLOAD LIST\n"; *)

(* We should probably recall this part every few minutes... *)
      
      List.iter (fun f -> try f.op_download_start file with _ -> ()) !networks;
      (try Bitzi.query_bitzi_ticket file with _ -> ());
      file      
    
    let new_filename file name = 
      if not (List.mem name file.file_filenames) then begin
          file.file_filenames <- file.file_filenames @ [name] ;
(*          update_best_name file*)
        end
    
    let remove_file file =
      Hashtbl.remove files_by_num (file_num file);
      Unix32.close (file_fd file);
      current_files := List2.removeq file !current_files;
      List.iter (fun (n,v) -> n.op_download_finish v) file.file_impls
    
    let _ =
      let module P = GuiTypes in
      file_ops.op_file_cancel <- (fun file ->
          remove_file file;
          (try  Unix32.remove (file_fd file)  with e -> ());
          file_cancel (as_file file);
      );
      file_ops.op_file_info <- (fun file ->
          let avail = 
            Int64Swarmer.verified_bitmap file.file_verified_partition
          in
          {
            P.file_name = file.file_name;
            P.file_num = (file_num file);
            P.file_network = network.network_num;
            P.file_names = file.file_filenames;
            P.file_md4 = Md4.null;
            P.file_size = file_size file;
            P.file_downloaded = file_downloaded file;
            P.file_nlocations = 0;
            P.file_nclients = 0;
            P.file_state = file_state file;
            P.file_sources = None;
            P.file_download_rate = file_download_rate file.file_file;
            P.file_chunks = avail;
            P.file_availability = avail;
            P.file_format = FormatNotComputed 0;
            P.file_chunks_age = [|0|];
            P.file_age = file_age file;
            P.file_last_seen = BasicSocket.last_time ();
            P.file_priority = file_priority  file;
            P.file_uids = file.file_uids;
          }    
      );
      file_ops.op_file_sources <- (fun file ->
          let list = ref [] in
          List.iter (fun (n,v) ->
              list := (n.op_download_sources v) @ !list) file.file_impls;
          !list
      );
      file_ops.op_file_save_as <- (fun file name ->
          file.file_filenames <- [name];
          set_file_best_name  file name
      );      
      file_ops.op_file_recover <- (fun file ->
          if file_state file = FileDownloading then
            List.iter (fun (n,v) ->
                n.op_download_recover v) file.file_impls;
      );
      file_ops.op_file_pause <- (fun file ->
          List.iter (fun (n,v) -> n.op_download_pause v) file.file_impls
      );
      file_ops.op_file_resume <- (fun file ->
          List.iter (fun (n,v) -> n.op_download_resume v) file.file_impls
      );
      file_ops.op_file_debug <- (fun file ->
          let buf = Buffer.create 100 in
          Int64Swarmer.debug_print buf file.file_swarmer;
          List.iter (fun (n,v) ->
              try n.op_download_debug v buf with _ -> ()
          ) file.file_impls;
          Buffer.contents buf
      );
      file_ops.op_file_commit <- (fun file new_name ->
          try
            if file.file_files <> [] then 
              let old_file = new_name ^ ".torrent" in
              Sys.rename new_name old_file;
              let old_fd = Unix32.create_ro old_file in
              List.iter (fun (filename, begin_pos, end_pos) ->
                  let filename = Filename.concat new_name filename in
                  lprintf "Would save file as %s\n" filename;
                  let dirname = Filename.dirname filename in
                  Unix2.safe_mkdir dirname;
                  lprintf "Copying %Ld %Ld to 0\n"
                    begin_pos (end_pos -- begin_pos);
                  let fd = Unix32.create
                      filename [Unix.O_RDWR; Unix.O_CREAT] 0o666 in
                  Unix32.copy_chunk old_fd fd begin_pos zero (end_pos -- begin_pos);
                  Unix32.close fd
              ) file.file_files;
              Unix32.close old_fd;
              if !!delete_original then Sys.remove old_file
          with e ->
              lprintf "Exception %s while commiting BitTorrent file"
                (Printexc.to_string e)
      ) 
    
    let value_to_int32pair v =
      match v with
        List [v1;v2] | SmallList [v1;v2] ->
          (value_to_int64 v1, value_to_int64 v2)
      | _ -> 
          failwith "Options: Not an int32 pair"
    
    let value_to_state v =
      match v with
      | StringValue "Paused" -> FilePaused
      | StringValue "Downloading" -> FileDownloading
      | StringValue "Downloaded" -> FileDownloaded
      | _ -> raise Not_found
    
    let state_to_value s = 
      match s with
      | FilePaused | FileAborted _ -> StringValue "Paused"
      | FileDownloaded -> StringValue "Downloaded"
      | _ -> StringValue "Downloading"
    
    let value_to_file is_done assocs =
      let get_value name conv = conv (List.assoc name assocs) in
      let get_value_nil name conv = 
        try conv (List.assoc name assocs) with _ -> []
      in
      
      let file_name = get_value "file_name" value_to_string in
      let file_id = 
        try
          get_value "file_id" value_to_string
        with _ -> failwith "Bad file_id"
      in
      let file_size = try
          value_to_int64 (List.assoc "file_size" assocs) 
        with _ -> failwith "Bad file size"
      in
      let file_uids = try
          value_to_list (fun v ->
              uid_of_string (value_to_string v)  
          ) (List.assoc "file_hashes" assocs)
        with _ -> 
            lprintf "[ERROR]: Could not load hash for %s\n"
              file_name;
            []
      in
      
      let file = new_download (Some file_id) file_name file_size None file_uids in
      
      (try
          let file_state = get_value "file_state" value_to_state in
          set_file_state file file_state;  
        with _ -> ());
      
      (try
          file.file_file.impl_file_age <- 
            normalize_time (get_value "file_age" value_to_int)
        with _ -> ());
      
      let _ = 
        try
          List.iter (fun v ->
              file.file_files <- v :: file.file_files
          )
          (get_value "file_files" 
              (value_to_list (fun v ->
                  match v with
                    SmallList [name; p1;p2]
                  | List [name; p1;p2] ->
                      value_to_string name, value_to_int64 p1, value_to_int64 p2
                  | _ -> assert false
              )))
        with _ -> ()
      in
      (try
          file.file_filenames <-
            get_value_nil "file_filenames" (value_to_list value_to_string);
        with _ -> ());
      (try
          let s = get_value "file_bitzi_ticket"  value_to_string in
          if s <> "" then
            file.file_bitzi_ticket <- Bitzi_ticket s
        with _ -> ());
      
      (try 
          Int64Swarmer.set_present file.file_swarmer 
            (get_value "file_present_chunks" 
              (value_to_list value_to_int32pair));
          add_file_downloaded file.file_file
            (Int64Swarmer.downloaded file.file_swarmer)      
        with _ -> ()                
      );
      (try
          match (List.assoc "file_on_networks" assocs) with
            List l | SmallList l ->
              List.iter (fun v ->
                  match v with
                    Module assocs ->
                      let netname = value_to_string (List.assoc "network" assocs) in
                      List.iter (fun n ->
                          if n.op_download_netname = netname then
                            n.op_download_of_value file assocs
                      ) !networks
                  | _ -> ()
              ) l
          | _ -> ()
        with _ -> ());
      
      as_file file
    
    let file_to_value file =
      [
        "file_size", int64_to_value (file_size file);
        "file_name", string_to_value file.file_name;
        "file_downloaded", int64_to_value (file_downloaded file);
        "file_id", string_to_value file.file_id;
        "file_state", state_to_value (file_state file);
        "file_hashes", list_to_value "Hashes" (fun uid ->
            string_to_value (string_of_uid uid)
        ) file.file_uids;
        "file_filenames", List
          (List.map (fun s -> string_to_value s) file.file_filenames);        
        "file_present_chunks", List
          (List.map (fun (i1,i2) -> 
              SmallList [int64_to_value i1; int64_to_value i2])
          (Int64Swarmer.present_chunks file.file_swarmer));
        "file_files", list_to_value ""
          (fun (name, p1,p2) ->
            SmallList [string_to_value name; int64_to_value p1; int64_to_value p2])
        file.file_files;
        
        "file_on_networks", 
        List (List.map (fun (n,v) ->
              Module (
                ("network", string_to_value n.op_download_network.op_download_netname) ::
                (n.op_download_to_value v)
              )) file.file_impls);
        "file_mtime", float_to_value (
          try Unix32.mtime64 (file_disk_name file) with _ -> 0.0);
        "file_age", IntValue (Int64.of_int (file_age file));
        "file_bitzi_ticket", string_to_value (match file.file_bitzi_ticket with
            Bitzi_ticket s -> s | _ -> "");
      ]
    
    let _ =
      network.op_network_file_of_option <- value_to_file;
      file_ops.op_file_to_option <- file_to_value
    
    let download_is_finished file = 
      if List.memq file !current_files then begin
          lprintf "[DEBUG] file is finished...2\n";
          file_completed (as_file file);
          remove_file file;
          List.iter (fun (n,v) -> n.op_download_finish v) file.file_impls
        end    
    
    let check_finished file = 
      try
        if Int64Swarmer.dirty file.file_swarmer then
          Int64Swarmer.verify_file file.file_swarmer;
        if file_state file <> FileDownloaded &&
          file_size file = Int64Swarmer.downloaded file.file_swarmer then begin
            lprintf "[DEBUG] check file is finished...2\n";
            (let partition = file.file_verified_partition in
              lprintf "[DEBUG] check file is finished...3\n";
              let bitmap = Int64Swarmer.verified_bitmap partition in
              for i = 0 to String.length bitmap - 1 do
                if bitmap.[i] <> '3' then raise Not_found;
              done;  
              lprintf "[DEBUG] check file is finished...4\n";
            );
            lprintf "[DEBUG] check file is finished...5\n";
            download_is_finished file
          end
      with _ -> 
          lprintf "[DEBUG] check file is finished...1\n";
          ()
    
    let check_finished_timer _ =
      List.iter check_finished !current_files
    
    let downloaded file = Int64Swarmer.downloaded file.file_swarmer

(* This should be a per-network command... For example, the BitTorrent
URN should contain both the tracker and the info, so that the
tracker can be different for two URNs, but the info still the same. *)
    
    open GuiTypes  
    let _ =
      let commands = [
          
          "debug_download", Arg_none (fun o ->
              let buf = o.conn_buf in
              List.iter (fun file ->
                  Int64Swarmer.debug_print buf file.file_swarmer  
              ) !current_files;
              lprintf "DEBUG: \n%s\n" (Buffer.contents buf);
              ""
          ), " : print info on downloads";
          
          "add_uid", Arg_multiple (fun args o ->
              let buf = o.conn_buf in
              Printf.bprintf buf "nargs\n";
              match args with
                file_num :: ( (_ :: _) as tail) ->
                  Printf.bprintf buf "file_num: %s\n" file_num;
                  let file_num = int_of_string file_num in
                  Printf.bprintf buf "file_num: %d\n" file_num;
                  let file = Hashtbl.find files_by_num file_num in
                  Printf.bprintf buf "file found\n";
                  List.iter (fun urn ->
                      Printf.bprintf buf "urn: %s\n" urn;
                      let uids = 
                        let (header, _) = String2.cut_at urn ':' in
                        match String.lowercase header with
                          "urn" -> [uid_of_string urn]
                        | "ed2k" -> []
                        | "magnet" ->
                            let (_,uids) = parse_magnet urn in
                            uids
                        | "sig2dat" -> []
                        | _ -> failwith "Unknown URN kind"
                      in
                      if uids = [] then
                        Printf.bprintf buf "Not yet implemented\n";
                      add_uids file uids
                  ) tail;
                  Printf.bprintf buf "done\n";
                  List.iter (fun f -> 
                      try f.op_download_start file with _ -> ()) !networks;
                  "URNs added"
              | _ -> "Not enough arguments"
          ), " <file_num> <urns>: add URNs to a given downloads:\n
  Correct URNs are of the form:\n
urn:ed2k:ed2k_in_hexa\n
urn:bitprint:sha1_in_hexa.tiger_in_base32\n
urn:sha1:sha1_in_hexa\n
urn:sig2dat:sig2dat_in_base32\n
or URLS (ed2k://, magnet?, sig2dat://).
          ";
          
          
          "recover_temp", Arg_none (fun o ->
              let buf = o.conn_buf in
              let files = Unix2.list_directory !!temp_directory in
              List.iter (fun filename ->
                  try
                    let file_size = Unix32.getsize64 (Filename.concat 
                          !!temp_directory filename) in
                    let len = String.length filename in
                    let uid =
                      if String2.starts_with filename "COMMON-" then
                        let uid = String.sub filename 7 (len-7) in
                        uid_of_string uid
                      else if String.length filename = 32 then
                        uid_of_uid (Ed2k ("",Md4.of_string filename))
                      else raise Not_found
                    in
                    let file_shared = 
                      new_download None (string_of_uid uid) file_size 
                        (Some filename) []
                    in
                    lprintf "File %s recovered to %s" filename 
                    (file_disk_name file_shared)
                  with e ->
                      lprintf "exception %s in recover_temp\n"
                        (Printexc2.to_string e);
              ) files;
              "done"
          ), ":\t\t\t\trecover lost files from temp directory";
          
          
        ] in
      
      CommonNetwork.register_commands commands;
      ()

  end
