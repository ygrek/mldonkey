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
open CommonResult
open CommonInteractive
open CommonNetwork
open CommonSearch
(* open CommonTypes *)
open CommonGlobals
open GuiTypes
(* open CommonComplexOptions *)
(* open CommonFile *)
open Options
open BasicSocket
open TcpBufferedSocket
(* open DriverInteractive *)

module DP500(M: sig

      module CommonTypes : sig
          type file
        end

      open CommonTypes
      module CommonFile : sig

          val file_best_name : file -> string
          val file_disk_name : file -> string
          val file_size : file -> int64
          val file_downloaded : file -> int64
        end

      module CommonOptions : sig 
          
          val incoming_directory : string Options.option_record
          val temp_directory : string Options.option_record
          val dp500_directory : string Options.option_record
          val allowed_ips : Ip.t list Options.option_record
          val dp500_buffer : int Options.option_record
          val dp500_pclink : bool Options.option_record
          val telnet_bind_addr : Ip.t Options.option_record
          val dp500_port : int Options.option_record
        end
        
      val files : unit -> file list
        
    end) =
  struct
    
    open M
    open CommonFile
      open CommonOptions 
  
(* TODO: add all the functionnalities that the dp500 lacks:
  - shuffled directory (reply with random mp3 from directory)
  - shuffled tree (reply with random mp3 from tree)
  - search by subword (give a tree structure with the 26 letters per directory)
*)
  
(*************************************************************************)
(*                                                                       *)
(*                         Global values                                 *)
(*                                                                       *)
(*************************************************************************)

let verbose_dp500 = ref true
let pclink_buf = ref ""
let pclink_buf_len = ref 0

let pclink_socks = ref []    

(* An horrible hack... The DP500 requires to have an extension of the
file before playing it, so we add it after a @, but we then need
to remove it when the file is received. *)

let nregistered_files = ref 0
let registered_files_of_diskname = Hashtbl.create 13
let registered_files_of_num = Hashtbl.create 13

let max_best_name_len = 100
let max_disk_name_len = 80

(*************************************************************************)
(*                                                                       *)
(*                         pclink_reader                                 *)
(*                                                                       *)
(*************************************************************************)
  
let pclink_reader telnet sock nread = 
  let b = buf sock in
  if nread > 0 then
    let s = 
      match !telnet with 
        None ->
          if !verbose_dp500 then lprintf "dp500 forwarding...\n";
          let handler s nread =
            if nread > 0 then
              let b = buf s in
              if !verbose_dp500 then lprintf "Writing %d from pclink\n" b.len;
              AnyEndian.dump (String.sub b.buf b.pos b.len);
              write sock b.buf b.pos b.len;
              buf_used b b.len
          in
          let token = create_token unlimited_connection_manager in
          let s = connect token "pclink relay" 
              (Ip.to_inet_addr Ip.localhost) 7000 (fun _ _ -> ())
          in
          set_reader s handler;
          set_closer s (fun _ r ->
              close sock r);
          telnet := Some s;
          s
          
      | Some s -> s
    in  
    if !verbose_dp500 then lprintf "writing %d to pclink\n" b.len;
    AnyEndian.dump (String.sub b.buf b.pos b.len);
    write s b.buf b.pos b.len;
    buf_used b b.len

(*************************************************************************)
(*                                                                       *)
(*                         good_file                                     *)
(*                                                                       *)
(*************************************************************************)

let good_file kind is_dir file =
  String.length file > 1 && file.[0] <> '.' && 
  (is_dir ||
    let ext = String.lowercase (Filename2.last_extension file) in
    if !verbose_dp500 then lprintf "Extension [%s]\n" ext;
    List.mem ext
    (match String.lowercase kind with
        "video" -> [ ".avi"; ".mpg"; ".mpeg" ]
      | "audio" -> [ ".mp3"; ".ogg"; ".wma" ]
      | "picture" -> [ ".jpg"; ".jpeg"; ".jpe"; ".gif"; ".tiff" ]
      | kind -> 
          if !verbose_dp500 then lprintf "Unknow kind [%s]\n" kind;
          []))

(*************************************************************************)
(*                                                                       *)
(*                         need_correction                               *)
(*                                                                       *)
(*************************************************************************)
  
let need_correction best_name disk_name =
  
  (String.length best_name > max_best_name_len) ||
  (String.length disk_name > max_disk_name_len) ||
  ( let best_ext = Filename2.last_extension best_name in
    let disk_ext = Filename2.last_extension disk_name in  
    String.lowercase best_ext <> String.lowercase disk_ext)

(*************************************************************************)
(*                                                                       *)
(*                         correct_length                                *)
(*                                                                       *)
(*************************************************************************)

let correct_length best_name =
  let len = String.length best_name in
  if len > max_best_name_len then
    (String.sub best_name 0 (max_best_name_len - 8)) ^ "..." ^ 
    (String.sub best_name (len-5) 5)
  else
    best_name
    

(*************************************************************************)
(*                                                                       *)
(*                         register_file                                 *)
(*                                                                       *)
(*************************************************************************)
  
let register_file best_name disk_name =
  
  let disk_name = 
    let len = String.length disk_name in
    if len > 2 && disk_name.[0] = '.' &&
      disk_name.[1] = '/' then
      String.sub disk_name 2 (len-2)
    else
      disk_name
  in

  if need_correction best_name disk_name then
    (best_name, disk_name)
  else
  
  let ext = Filename2.last_extension best_name in
  
  let n =
    try
      Hashtbl.find registered_files_of_diskname disk_name
    with _ -> 
        let n = !nregistered_files in
        incr nregistered_files;
        Hashtbl.add registered_files_of_diskname disk_name n;
        Hashtbl.add registered_files_of_num n (best_name, disk_name);
        n
  in
  correct_length best_name, Printf.sprintf "%d@%s" n ext

(*************************************************************************)
(*                                                                       *)
(*                         correct_file                                  *)
(*                                                                       *)
(*************************************************************************)
  
let correct_file file =
  let file = 
    try
      let pos = String.index file '@' in
      let num = int_of_string (String.sub file 0 pos) in
      let ext = String.sub file (pos+1) (String.length file - pos - 1) in
      
      let (best_name, disk_name) = Hashtbl.find registered_files_of_num num in
      if Filename2.last_extension best_name = ext then
        disk_name else disk_name ^ ext
        
      (*
      let file = CommonFile.file_find num in
      if Filename2.last_extension (file_best_name file) = ext then
        file_disk_name file
      else
        file_disk_name file ^ ext
*)
      
    with _ -> file in
  file

(*************************************************************************)
(*                                                                       *)
(*                         exec_command                                  *)
(*                                                                       *)
(*************************************************************************)
  
let exec_command telnet line sock =
  try
    let (command, tail) = String2.cut_at line ' ' in
    if command = "LIST" then
      let (kind, tail) = String2.cut_at tail ' ' in
      match String2.split tail '|' with
        _ :: argument :: _ ->
          
          let list_directory base_directory argument =
            
            let files = 
              try
                let directory = Filename.concat base_directory argument in
                Unix2.list_directory directory 
              with e -> 
                  lprintf "Exception in LIST %s\n" (Printexc2.to_string e);
                  []
            in
            base_directory, argument, 
            List2.tail_map (fun file -> 
                let abs_file = Filename.concat argument file in
                let full_file = Filename.concat base_directory abs_file in
                (file,file, full_file)) files
          
          in
          
          let mlnet_incoming_string = "MLNET INCOMING" in
          let mlnet_incoming_string_len = String.length mlnet_incoming_string in
          
          let mlnet_temp_string = "MLNET TEMP" in
          let mlnet_temp_string_len = String.length mlnet_temp_string in
          
          
          let base_directory, argument, files = match kind with
            | "VIDEO" -> 

                if argument = "" then begin
                    if !!incoming_directory <> "" then
                      write_string sock (Printf.sprintf "%s|%s|1|\r\n"
                          mlnet_incoming_string mlnet_incoming_string
                      );
                    
                    if !!temp_directory <> "" then
                      write_string sock (Printf.sprintf "%s|%s|1|\r\n"
                          mlnet_temp_string mlnet_temp_string 
                      );
                    end;
                if !!incoming_directory <> "" &&
                  String2.starts_with argument mlnet_incoming_string then
                    list_directory !!incoming_directory
                      (String.sub argument mlnet_incoming_string_len
                        (String.length argument - mlnet_incoming_string_len))
                  
                else
                if !!temp_directory <> "" &&
                  String2.starts_with argument mlnet_temp_string then
                    !!temp_directory,
                    (String.sub argument mlnet_temp_string_len
                        (String.length argument - mlnet_temp_string_len)),
                    let list = ref [] in
                    List.iter (fun file ->
                        let best_name = file_best_name file in
                        let disk_name = file_disk_name file in
                        let ext = Filename2.last_extension best_name in
                        lprintf "Adding %s\n" best_name;
                        let file_size = Int64.to_float (file_size file) in
                        let file_downloaded = 
                          Int64.to_float (file_downloaded file) in
                        
                        list := (
                          Printf.sprintf "%2.1f%%/%4.1fM %s"
                            (file_downloaded /. file_size *. 100. )
                          (file_size /. 1048576.)
                          best_name, Filename.basename disk_name, disk_name
(* Printf.sprintf "%d@%s" (file_num file) ext *)) :: 
                        !list
                    ) (files ());
                    !list
                else 
                  list_directory
                    (Filename.concat !!dp500_directory "video") argument
            
            | "AUDIO" -> 
                list_directory
                  (Filename.concat !!dp500_directory "audio") argument
            
            | "PICTURE" -> 
                list_directory (Filename.concat !!dp500_directory "photos")
                argument
            
            | _ -> failwith "No such kind"          
          in
          
          let files = List.sort (fun (s1,_,_) (s2,_,_) ->
                compare (String.lowercase s1) (String.lowercase s2)) files in
          
          
          List.iter (fun (best_name,file, full_file) ->
              lprintf "Check %s/%s\n" best_name full_file;
              let is_dir = Unix2.is_directory full_file in
              if good_file kind is_dir best_name then
                let line = 
                  if  is_dir then
                    Printf.sprintf "%s|%s|1|\r\n"
                      best_name file
                  else
                  let (best_name, disk_name) = register_file best_name full_file
                  in
                  Printf.sprintf "%s|%s|0|\r\n" best_name disk_name
                in
                if !verbose_dp500 then lprintf "Write [%s]\n" (String.escaped line);
                write_string sock line;
          ) files;
          close_after_write sock
      | _ -> failwith "Unable to parse LIST command"
    else
    if command = "SIZE" then
      let file, tail = String2.cut_at tail '|' in
      let file = correct_file file in
      if !verbose_dp500 then lprintf "Size of [%s]\n" file;
      let fd = Unix32.create_ro file in
      let size = Unix32.getsize64 fd in
      write_string sock (Printf.sprintf "%015Ld" size)
    else
    if command = "GET" then
      let file, tail = String2.cut_at tail '|' in
      match String2.split tail ' ' with
        _ :: begin_read :: len_read :: _ ->
          if !verbose_dp500 then lprintf "Get of [%s] %s [%s]\n" file begin_read len_read;
          let file = correct_file file in
          
          if !verbose_dp500 then lprintf "Get of [%s] %s [%s]\n" file begin_read len_read;          
          
          let fd = Unix32.create_ro file in
          let len_read = int_of_string len_read in
          if len_read = 1 then begin
              incr telnet;
              if !telnet > 10 then close sock (Closed_for_error "");
              raise Not_found
            end;
          let pclink_buf, len_read = 
            if len_read = 0 then
              let size = Unix32.getsize64 fd in
              let size = Int64.to_int size in
              String.create size, size
            else
            if len_read > !pclink_buf_len then begin
                pclink_buf := String.create len_read;
                pclink_buf_len := len_read;
                !pclink_buf, len_read
              end else
              !pclink_buf, len_read
          in
          Unix32.read fd (Int64.of_string begin_read) pclink_buf 0 len_read;
          write sock pclink_buf 0 len_read;
          if !verbose_dp500 then lprintf "Written %d bytes\n" len_read;
      | _ -> failwith "Cannot parse pos + len"
    else
      raise Not_found
      
  with e ->
      close sock (Closed_for_error "");
      if !verbose_dp500 then lprintf "Exception %s for [%s]\n" (Printexc2.to_string e) line
      

(*************************************************************************)
(*                                                                       *)
(*                         pclink_reader                                 *)
(*                                                                       *)
(*************************************************************************)
      
let pclink_reader telnet sock nread =
  if !telnet < 10 then
    let b = buf sock in
    if !verbose_dp500 then lprintf "BUFFER: [%s]\n" (String.escaped (String.sub b.buf b.pos b.len));
    let rec iter () =
      if b.len > 0 then
        if String2.starts_with b.buf "ACTION 2" then
          let rec iter_pipe i =
            if i < b.len then
              if b.buf.[b.pos + i] = '|' then
                let file = String.sub b.buf b.pos i in
                let file = correct_file file in
                buf_used b (i+1);
                if !verbose_dp500 then lprintf "Existing file [%s]\n" (String.escaped file);
                
                write_string sock (
                  if Sys.file_exists file then
                    "200" 
                  else 
                    "404"
                );
                iter ()
              else
                iter_pipe (i+1)
          in
          buf_used b 9;
          iter_pipe 0
        else
        if String2.starts_with b.buf "ACTION 1" then
          let rec iter_pipe args i =
            if i < b.len then
              if b.buf.[b.pos + i] = '|' then
                let file = String.sub b.buf b.pos i in
                if !verbose_dp500 then lprintf "Argument [%s]\n" (String.escaped file);
                buf_used b (i+1);
                let args = file :: args in
                if List.length args = 3 then begin
                    
                    let file = String.sub file 1 (String.length file - 1) in
                    let file = correct_file file in
                    if !verbose_dp500 then lprintf "Existing file [%s]\n" (String.escaped file);
                    
                    write_string sock (
                      if Sys.file_exists file then
                        "200" 
                      else 
                        "404"
                    );
                    
                    
                    iter ()
                  end else
                  iter_pipe args 0
              else
                iter_pipe args (i+1)
          in
          buf_used b 9;
          iter_pipe [] 0
        
        else
        let rec iter_nl i =
          if i + 3 < b.len then
            if 
              b.buf.[b.pos + i] = '\r' &&
              b.buf.[b.pos + i + 1] = '\n' &&
              b.buf.[b.pos + i + 2] = '\r' &&
              b.buf.[b.pos + i + 3] = '\n' then
              let line = String.sub b.buf b.pos i in
              buf_used b (i+4);
              
              if !verbose_dp500 then lprintf "Exec command [%s]\n" (String.escaped line);
              exec_command telnet line sock;
              iter ()
            else
              iter_nl (i+1)
        in
        iter_nl 0
    in
    iter ()
    
    
(*************************************************************************)
(*                                                                       *)
(*                         pclink_closed                                 *)
(*                                                                       *)
(*************************************************************************)
  
let pclink_closed sock reason = ()
  
(*************************************************************************)
(*                                                                       *)
(*                         pclink_handler                                *)
(*                                                                       *)
(*************************************************************************)

  
let pclink_handler t event = 
  match event with
    TcpServerSocket.CONNECTION (s, Unix.ADDR_INET (from_ip, from_port)) ->
      if !verbose_dp500 then lprintf "dp500 CONNECTION !!\n";
      let from_ip = Ip.of_inet_addr from_ip in
      if Ip.matches from_ip !!allowed_ips then 
        let token = create_token unlimited_connection_manager in
        if !verbose_dp500 then lprintf "dp500 connection allowed\n";
        let sock = TcpBufferedSocket.create_simple token
            "pclink connection"
            s in
        let telnet = ref 0 in
        
        TcpBufferedSocket.prevent_close sock;
        TcpBufferedSocket.set_max_input_buffer sock 10000;
        TcpBufferedSocket.set_max_output_buffer sock !!dp500_buffer;
        TcpBufferedSocket.set_reader sock (pclink_reader telnet);
        TcpBufferedSocket.set_closer sock pclink_closed;
        pclink_socks := sock :: !pclink_socks;
      
      else 
        Unix.close s
  
  | _ -> ()
      
let start () =
  if !!dp500_pclink then 
    ignore (find_port  "dp500 server" !!telnet_bind_addr
        dp500_port pclink_handler);  
  
end
