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
open CommonGlobals
open BasicSocket
open CommonClient
open CommonServer
open CommonNetwork
open Options
open CommonOptions
open CommonTypes
open CommonFile

let addr_to_value addr =
  if addr.addr_name = "" then
    to_value Ip.option addr.addr_ip
  else
  if addr.addr_ip = Ip.null then
    string_to_value addr.addr_name
  else
    SmallList [
      string_to_value addr.addr_name;
      to_value Ip.option addr.addr_ip;
      int_to_value addr.addr_age
    ]
    
let value_to_addr v =
  match v with
    StringValue s ->
      let ip = from_value Ip.option v in
      if ip = Ip.null then
        new_addr_name s
      else
        new_addr_ip ip
  | SmallList [StringValue name; ip ; age]
  | List [StringValue name; ip ; age] ->
      let addr = new_addr_name name in
      let age = value_to_int age in
      if age + !!ip_cache_timeout > last_time () then begin
          addr.addr_age <- age;
          addr.addr_ip <- from_value Ip.option ip
        end;
      addr
  | _ -> assert false
      
module FileOption = struct
    
    let value_to_file is_done v =
      match v with
        Options.Module assocs ->
          let get_value name conv = conv (List.assoc name assocs) in
          let network = try get_value "file_network" value_to_string
            with _ -> "Donkey" in
          let network = network_find_by_name network in
          let file = network_add_file network is_done assocs in
          let priority = try get_value "file_priority" value_to_int 
            with _ -> 0 in
          file_set_priority file priority;
          file
      | _ -> assert false
    
    let file_to_value file =
        let netname = string_to_value (file_network file).network_name in
        Options.Module (
        ("file_network", netname) ::
        ("file_priority", int_to_value (file_priority file)) ::
          (file_to_option file)
        )
          
    let t is_done =
      define_option_class "File" (value_to_file is_done) file_to_value
    ;;
  end

    
let done_files = 
  define_option files_ini ["done_files"] 
    "The files whose download is finished" (
    listiter_option (FileOption.t true)) []
  
let files = 
  define_option files_ini ["files"] 
    "The files currently being downloaded" (
    listiter_option (FileOption.t false)) []
    
module ServerOption = struct
    
    let value_to_server v =
      match v with
        Options.Module assocs ->
          let get_value name conv = conv (List.assoc name assocs) in
          let network = try
              get_value "server_network" value_to_string
            with _ -> "Donkey"
          in
          let network = 
            try network_find_by_name network with e ->
                lprintf "Network %s not supported\n" network;
                raise e
              in
          let server = network_add_server network assocs in
          server
      | _ -> assert false
    
    let server_to_value server =
      Options.Module (
        ("server_network", 
          string_to_value (server_network server).network_name)
        ::
        (server_to_option server)
      )
      
    let t =
      define_option_class "Server" value_to_server server_to_value
  end


let servers = define_option servers_ini
    ["known_servers"] "List of known servers"
    (intmap_option (fun s -> server_num s) ServerOption.t) Intmap.empty


let rec string_of_option v =
  match v with
    Module m -> "{ MODULE }"
  | StringValue s -> Printf.sprintf "STRING [%s]" s
  | IntValue i -> Printf.sprintf "INT [%Ld]" i
  | FloatValue f -> Printf.sprintf "FLOAT [%f]" f
  | OnceValue v -> string_of_option v
  | List l | SmallList l ->
      (List.fold_left (fun s v ->
            s ^ (string_of_option v) ^ ";" 
        ) "LIST [" l) ^ "]"
  | DelayedValue _ -> assert false
      
      
module QueryOption = struct
    let rec query_to_value q =
      match q with
      | Q_AND list ->
          List ((StringValue "AND") :: (List.map query_to_value list))
      | Q_OR list ->
          List ((StringValue "OR"):: (List.map query_to_value list))
      | Q_HIDDEN list ->
          List ((StringValue "HIDDEN"):: (List.map query_to_value list))
      | Q_ANDNOT (q1, q2) ->
          SmallList [StringValue "ANDNOT"; query_to_value q1; query_to_value q2 ]
      | Q_MODULE (s, q) ->
          SmallList [StringValue "MODULE"; StringValue s;  query_to_value q]
          
        
        | Q_KEYWORDS (label, s) ->
            SmallList [StringValue "KEYWORDS"; StringValue label; StringValue s]
        | Q_MINSIZE (label, s) ->
            SmallList [StringValue "MINSIZE"; StringValue label; StringValue s]
        | Q_MAXSIZE (label, s) ->
            SmallList [StringValue "MAXSIZE"; StringValue label; StringValue s]
        | Q_FORMAT (label, s) ->
            SmallList [StringValue "FORMAT"; StringValue label; StringValue s]
        | Q_MEDIA (label, s) ->
            SmallList [StringValue "MEDIA"; StringValue label; StringValue s]
        
        | Q_MP3_ARTIST (label, s) ->
            SmallList [StringValue "MP3_ARTIST"; StringValue label; StringValue s]
        | Q_MP3_TITLE (label, s) ->
            SmallList [StringValue "MP3_TITLE"; StringValue label; StringValue s]
        | Q_MP3_ALBUM (label, s) ->
            SmallList [StringValue "MP3_ALBUM"; StringValue label; StringValue s]
        | Q_MP3_BITRATE (label, s) ->
            SmallList [StringValue "MP3_BITRATE"; StringValue label; StringValue s]

      | Q_COMBO _ -> assert false
          
    let rec value_to_query v =
      match v with
      | SmallList ((StringValue "AND") :: list)
      | List ((StringValue "AND") :: list) -> 
          Q_AND (List.map value_to_query list)

      | SmallList ((StringValue "OR") :: list)
      | List ((StringValue "OR") :: list) -> 
          Q_OR (List.map value_to_query list)

      | SmallList ((StringValue "HIDDEN") :: list)
      | List ((StringValue "HIDDEN") :: list) -> 
          Q_HIDDEN (List.map value_to_query list)

      | SmallList [StringValue "ANDNOT"; v1; v2 ]
      | List [StringValue "ANDNOT"; v1; v2 ] -> 
          Q_ANDNOT (value_to_query v1, value_to_query v2)

      | SmallList [StringValue "MODULE"; StringValue label; v2 ]
      | List [StringValue "MODULE"; StringValue label; v2 ] -> 
          Q_MODULE (label, value_to_query v2)


          
      | SmallList [StringValue "KEYWORDS"; StringValue label; StringValue s]
      | List [StringValue "KEYWORDS"; StringValue label; StringValue s] ->
          Q_KEYWORDS (label, s)

      | SmallList [StringValue "MINSIZE"; StringValue label; StringValue s]
      | List [StringValue "MINSIZE"; StringValue label; StringValue s] ->
          Q_MINSIZE (label, s)

      | SmallList [StringValue "MAXSIZE"; StringValue label; StringValue s]
      | List [StringValue "MAXSIZE"; StringValue label; StringValue s] ->
          Q_MAXSIZE (label, s)

      | SmallList [StringValue "MINSIZE"; StringValue label; IntValue s]
      | List [StringValue "MINSIZE"; StringValue label; IntValue s] ->
          Q_MINSIZE (label, Int64.to_string s)

      | SmallList [StringValue "MAXSIZE"; StringValue label; IntValue s]
      | List [StringValue "MAXSIZE"; StringValue label; IntValue s] ->
          Q_MAXSIZE (label, Int64.to_string s)

      | SmallList [StringValue "FORMAT"; StringValue label; StringValue s]
      | List [StringValue "FORMAT"; StringValue label; StringValue s] ->
          Q_FORMAT (label, s)

      | SmallList [StringValue "MEDIA"; StringValue label; StringValue s]
      | List [StringValue "MEDIA"; StringValue label; StringValue s] ->
          Q_MEDIA (label, s)
          
      | SmallList [StringValue "MP3_ARTIST"; StringValue label; StringValue s]
      | List [StringValue "MP3_ARTIST"; StringValue label; StringValue s] ->
          Q_MP3_ARTIST (label, s)

      | SmallList [StringValue "MP3_TITLE"; StringValue label; StringValue s]
      | List [StringValue "MP3_TITLE"; StringValue label; StringValue s] ->
          Q_MP3_TITLE (label, s)

      | SmallList [StringValue "MP3_ALBUM"; StringValue label; StringValue s]
      | List [StringValue "MP3_ALBUM"; StringValue label; StringValue s] ->
          Q_MP3_ALBUM (label, s)

      | SmallList [StringValue "MP3_BITRATE"; StringValue label; StringValue s]
      | List [StringValue "MP3_BITRATE"; StringValue label; StringValue s] ->
          Q_MP3_BITRATE (label, s)

      | SmallList [StringValue "MP3_BITRATE"; StringValue label; IntValue s]
      | List [StringValue "MP3_BITRATE"; StringValue label; IntValue s] ->
          Q_MP3_BITRATE (label, Int64.to_string s)
          
      | _ -> failwith (Printf.sprintf "Query option: error while parsing %s"
              (string_of_option  v)
          )
      
    let t = define_option_class "Query" value_to_query query_to_value    
  end
      
let customized_queries = define_option searches_ini ["customized_queries"] ""
    (list_option (tuple2_option (string_option, QueryOption.t)))
  [ 
    "Complex Search", 
    Q_AND [
      Q_KEYWORDS ("keywords", "");
      Q_MODULE ("Simple Options",
        Q_AND [
          Q_MINSIZE ("Min Size", "");
          Q_MAXSIZE ("Max Size", "");
          Q_MEDIA ("Media", "");
          Q_FORMAT ("Format", "");
        ];
      );
      Q_MODULE ("Mp3 Options",
        Q_AND [
          Q_MP3_ARTIST ("Artist", ""); 
          Q_MP3_ALBUM ("Album", ""); 
          Q_MP3_TITLE ("Title", ""); 
          Q_MP3_BITRATE ("Min Bitrate", ""); 
        ]
      );
    ];
    "MP3 Search", 
    Q_AND [
      Q_KEYWORDS ("keywords", "");
      Q_MP3_ARTIST ("Artist", ""); 
      Q_MP3_ALBUM ("Album", ""); 
      Q_MP3_TITLE ("Title", ""); 
      Q_MP3_BITRATE ("Min Bitrate", ""); 
      Q_HIDDEN [
        Q_MEDIA ("Media", "Audio");
        Q_FORMAT ("Format", "mp3");
      ]
    ];
    "Movie Search", 
    Q_AND [
      Q_KEYWORDS ("keywords", "");
      Q_HIDDEN [
        Q_MINSIZE ("Min Size", "500000000");
        Q_MEDIA ("Media", "Video");
        Q_FORMAT ("Format", "avi");
      ]
    ];
    "Album Search",
    Q_AND [
      Q_KEYWORDS ("Keywords", "album");
      Q_HIDDEN [
        Q_ANDNOT (
          Q_MINSIZE ("Min Size", "30000000"),
          Q_FORMAT ("Format", "mp3")
        );
      ]
    ];
  ]
  
module ClientOption = struct
    
    let value_to_client is_friend v =
      match v with
        Options.Module assocs ->
          let get_value name conv = conv (List.assoc name assocs) in
          let network = try
              get_value "client_network" value_to_string
            with _ -> "Donkey"
          in
          let network = network_find_by_name network in
          let c = network_add_client network is_friend assocs in
          c
      | _ -> assert false
          
    let client_to_value client =
      Options.Module (
        ("client_network", string_to_value (client_network client).network_name)
        ::
        (client_to_option client)
      )
      
    let t is_friend =
      define_option_class "Client" (value_to_client is_friend) 
      client_to_value
    ;;
  end
  
let friends = 
  define_option friends_ini ["friends"] 
    "The list of known friends" (listiter_option (ClientOption.t true)) []
  
let load () = 
  Options.load files_ini;
  Options.load servers_ini;
  Options.load searches_ini;
  Options.load friends_ini
  
let save () = 
  networks_iter (fun n -> network_save_complex_options n);
  
(*  servers =:= server_sort (); *)
  
  Options.save_with_help files_ini;
  Options.save_with_help searches_ini;
  Options.save_with_help friends_ini;
  Options.save_with_help servers_ini;
  lprintf "Options correctly saved\n"

(*************  ADD/REMOVE FUNCTIONS ************)

let canonize_basename name =
  let name = String.copy name in
  for i = 0 to String.length name - 1 do
    match name.[i] with
    | '/' | '\\' -> name.[i] <- '_'
    | _ -> ()
  done;
  name
  
let file_commited_name file =   
  let network = file_network file in
  let best_name = file_best_name file in
  let file_name = file_disk_name file in
  let incoming_dir =
    if (!!((network.network_incoming_subdir))) <> "" then
      Filename.concat !!incoming_directory
        !!(network.network_incoming_subdir)
    else !!incoming_directory
  in
  (try Unix2.safe_mkdir incoming_dir with _ -> ());
  let new_name = 
    Filename.concat incoming_dir (canonize_basename 
        (file_best_name file))
  in
  let new_name = 
    if Sys.file_exists new_name then
      let rec iter num =
        let new_name = Printf.sprintf "%s.%d" new_name num in
        if Sys.file_exists new_name then
          iter (num+1)
        else new_name
      in
      iter 1
    else new_name in
  new_name  
  
(* This function is called on each downloaded file when the "commit" command
  is received. *)
  
let file_commit file =
  let impl = as_file_impl file in
  if impl.impl_file_state = FileDownloaded then
    let new_name = file_commited_name file in
    try
      lprintf "*******  RENAME %s to %s *******\n"
        (file_disk_name file) new_name; 
      Unix2.rename (file_disk_name file) new_name;
        
      lprintf "*******  RENAME %s to %s DONE *******\n" 
        (file_disk_name file) new_name; 
      set_file_disk_name file new_name;
      let best_name = file_best_name file in  
      Unix32.close (file_fd file);
      (* Commit the file first, and share it after... *)
      impl.impl_file_ops.op_file_commit impl.impl_file_val new_name;
      ignore (CommonShared.new_shared "completed" best_name new_name);
      done_files =:= List2.removeq file !!done_files;
      update_file_state impl FileShared;
    with e ->
      lprintf "Exception in file_commit: %s\n" (Printexc2.to_string e)
      
let file_cancel file =
  try
  let impl = as_file_impl file in

  if impl.impl_file_state <> FileCancelled then begin
        update_file_state impl FileCancelled;
        impl.impl_file_ops.op_file_cancel impl.impl_file_val;
        Unix32.close (file_fd file);
        files =:= List2.removeq file !!files;
    end
  with e ->
      lprintf "Exception in file_cancel: %s" (Printexc2.to_string e);
      lprint_newline ()

        
let mail_for_completed_file file =
  if !!mail <> "" then
    let module M = Mailer in
    let line1 = "mldonkey has completed the download of:\r\n\r\n" in

    let line2 = Printf.sprintf "\r\nFile: %s\r\nSize: %Ld bytes\r\nComment: %s\r\n" 
      (file_best_name file)
      (file_size file)
      (file_comment file)
    in
    
    let subject = if !!filename_in_subject then
        Printf.sprintf "[mldonkey] file received - %s"
        (file_best_name file)
      else
        Printf.sprintf "mldonkey, file received";        
    in
    
    let mail = {
        M.mail_to = !!mail;
        M.mail_from = Printf.sprintf "mldonkey <%s>" !!mail;
        M.mail_subject = subject;
        M.mail_body = line1 ^ line2;
      } in
    M.sendmail !!smtp_server !!smtp_port !!add_mail_brackets mail

let chat_for_completed_file file =
  CommonChat.send_warning_for_downloaded_file (file_best_name file)

      
let file_completed (file : file) =
  try
    let impl = as_file_impl file in
    if impl.impl_file_state = FileDownloading then begin
        files =:= List2.removeq file !!files;
        done_files =:= file :: !!done_files;
        update_file_state impl FileDownloaded;  
        let file_name = file_disk_name file in
        let file_id = Filename.basename file_name in
        ignore (CommonShared.new_shared "completed" (
            file_best_name file )
          file_name);
	if !!auto_commit then
	  file_commit file;
        (try mail_for_completed_file file with e ->
              lprintf "Exception %s in sendmail" (Printexc2.to_string e);
              lprint_newline ());
        if !!CommonOptions.chat_warning_for_downloaded then
          chat_for_completed_file file;
        
        if !!file_completed_cmd <> "" then begin
MlUnix.fork_and_exec  !!file_completed_cmd 
                              [|
                              file_name;
                              file_id;
                              Int64.to_string (file_size file);
                              file_best_name file
                            |]

          end
          

      end
  with e ->
      lprintf "Exception in file_completed: %s" (Printexc2.to_string e);
      lprint_newline ()
      
let file_add impl state = 
  try
    let file = as_file impl in
    if impl.impl_file_state = FileNew then begin
        update_file_num impl;
        (match state with
            FileDownloaded -> 
              done_files =:= file :: !!done_files;
          | FileShared
          | FileNew
          | FileCancelled -> ()
              
          | FileAborted _
          | FileDownloading
          | FileQueued
          | FilePaused -> 
              files =:= file :: !!files);
        update_file_state impl state
      end
  with e ->
      lprintf "Exception in file_add: %s" (Printexc2.to_string e);
      lprint_newline ()
      
let server_remove server =
  try
    let impl = as_server_impl server in
    if impl.impl_server_state <> RemovedHost then begin
        set_server_state server RemovedHost;
        (try impl.impl_server_ops.op_server_remove impl.impl_server_val
          with _ -> ());
        servers =:= Intmap.remove (server_num server) !!servers;
      end
  with e ->
      lprintf "Exception in server_remove: %s" (Printexc2.to_string e);
      lprint_newline ()
  
let server_add impl =
  let server = as_server impl in
  if impl.impl_server_state = NewHost then begin
      server_update_num impl;
      servers =:= Intmap.add (server_num server) server !!servers;
      impl.impl_server_state <- NotConnected (-1)
    end

let contacts = ref []

let friend_add c =
  let impl = as_client_impl c in
  match impl.impl_client_type with
    FriendClient -> ()
  | old_state ->
      impl.impl_client_type <- FriendClient;
      client_must_update c;
      friends =:= c :: !!friends;
      contacts := List2.removeq c !contacts;
      if old_state <> ContactClient then
        impl.impl_client_ops.op_client_browse impl.impl_client_val true

(* Maybe we should not add the client to the contact list and completely remove
it ? *)
let friend_remove c =
  try
    let impl = as_client_impl c in
    match  impl.impl_client_type with 
      FriendClient ->
        impl.impl_client_type <- NormalClient;
        client_must_update c;
        friends =:= List2.removeq c !!friends;
        impl.impl_client_ops.op_client_clear_files impl.impl_client_val
    | ContactClient ->
        impl.impl_client_type <- NormalClient;
        client_must_update c;
        contacts := List2.removeq c !contacts;
        impl.impl_client_ops.op_client_clear_files impl.impl_client_val
        
    | _ -> ()
  with e ->
      lprintf "Exception in friend_remove: %s" (Printexc2.to_string e);
      lprint_newline ()
  
let contact_add c =
  let impl = as_client_impl c in
  match impl.impl_client_type with
    FriendClient | ContactClient -> ()
  | _ ->
      impl.impl_client_type <- ContactClient;
      client_must_update c;
      contacts := c :: !contacts;
      impl.impl_client_ops.op_client_browse impl.impl_client_val true

let contact_remove c =
  try
    let impl = as_client_impl c in
    match  impl.impl_client_type with 
      ContactClient ->
        impl.impl_client_type <- NormalClient;
        client_must_update c;
        contacts := List2.removeq c !contacts;
        impl.impl_client_ops.op_client_clear_files impl.impl_client_val
    | _ -> ()
  with e ->
      lprintf "Exception in contact_remove: %s" (Printexc2.to_string e);
      lprint_newline ()

      
