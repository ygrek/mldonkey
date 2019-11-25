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

open CommonInteractive
open Printf2
open CommonOptions
open CommonClient
open CommonFile
open CommonServer
open CommonTypes
open CommonGlobals
open Options

open DcTypes
open DcGlobals

let log_prefix = "[dcCO]"

let lprintf_nl fmt =
  lprintf_nl2 log_prefix fmt

let dc_shared_files_ini = create_options_file "shared_files_dc.ini"

let dc_shared_section = file_section dc_shared_files_ini [] ""

(*      
let addr_to_value addr =
  match addr with
    AddrIp ip -> to_value Ip.option ip
  | AddrName s -> string_to_value s
      
let value_to_addr v =
  let ip = from_value Ip.option v in
  if ip <> Ip.null then AddrIp ip else AddrName (value_to_string v)
*)

module SharedDcFileOption = struct

  let value_to_shinfo v =
    (match v with
    | Options.Module assocs ->
        let sh_fname =
          (try
             value_to_filename (List.assoc "fname" assocs)
          with _ -> failwith "Bad DC shared file fullname" )
        in
        let sh_cname =
          (try
             value_to_string (List.assoc "cname" assocs)
          with _ -> failwith "Bad DC shared file codedname" )
        in
        let sh_sname =
          (try
            String.lowercase (List.nth (String2.splitn sh_cname '/' 1) 1) (* strip the "shared##" *)
          with _ -> failwith "Bad DC shared file codedname" )
        in 
        let sh_root =
          (try
             value_to_string (List.assoc "root" assocs)
          with _ -> failwith "Bad DC shared file tiger root hash" )
        in
        (*let sh_tths =
          (try
            value_to_array (fun v ->
              TigerTree.of_string (value_to_string v)) (List.assoc "tths" assocs)
          with _ -> failwith "Bad DC shared file tiger root hashes" )
        in*)
        let sh_size =
          (try
             value_to_int64 (List.assoc "size" assocs)
          with _ -> failwith "Bad DC shared file size" )
        in
        {
          dc_shared_fullname = sh_fname;
          dc_shared_codedname = sh_cname;
          dc_shared_searchname = sh_sname;
          dc_shared_size = sh_size;
          dc_shared_tiger_list = [];
          dc_shared_tiger_root = sh_root;
          (*dc_shared_tiger_array = sh_tths;*)
          dc_shared_pos = Int64.zero;
          dc_shared_chunks = 0;
        }
    | _ -> failwith "Options: not a shared file info option" )

  let shinfo_to_value dcsh =
    Options.Module [
      "fname", filename_to_value dcsh.dc_shared_fullname;
      "cname", string_to_value dcsh.dc_shared_codedname;
      "root", string_to_value dcsh.dc_shared_tiger_root;
      (*"tths", array_to_value TigerTree.hash_to_value dcsh.dc_shared_tiger_array;*)
      "size", int64_to_value dcsh.dc_shared_size;
    ]     

  let t = define_option_class "SharedDcFile" value_to_shinfo shinfo_to_value
end

let dc_saved_shared_files = define_option dc_shared_section
    ["shared_files"] ""
    (list_option SharedDcFileOption.t) [] 

(* End of shared file definition *)


let value_to_server  assocs =
  let get_value name conv = conv (List.assoc name assocs) in
(*  let get_value_nil name conv = 
    try conv (List.assoc name assocs) with _ -> []
  in *)
  let server_addr = get_value "server_addr" Ip.value_to_addr in
  let server_port = get_value "server_port" value_to_int in
  let t = Ip.ip_of_addr server_addr in (* DNS *)
  if (Ip.valid t) && (server_port>0) && (server_port<65536) then begin
    let h = new_server server_addr t server_port in
  h.server_name <- get_value "server_name" value_to_string;
  h.server_info <- get_value "server_info" value_to_string;
    h.server_autoconnect <- get_value "server_autoconnect" value_to_bool;
  as_server h.server_server
  end else failwith "Bad Server DNS" 

let server_to_value h =
  let list = [
      "server_name", string_to_value h.server_name;
      "server_addr", Ip.addr_to_value h.server_addr;
      "server_info", string_to_value h.server_info;
      (*"server_nusers", int_to_value (Int64.to_int h.server_nusers);*)
      "server_port", int_to_value h.server_port;
      "server_autoconnect", bool_to_value h.server_autoconnect;
    ] in
  list
  

(* parse options for files *)
let value_to_file file_size file_state user group assocs =
  let get_value name conv = conv (List.assoc name assocs) in
(*  let get_value_nil name conv = 
    try conv (List.assoc name assocs) with _ -> []
  in*) 
  let f_unchecked_tiger_root = get_value "file_root" value_to_string in
  let f_directory = get_value "file_dir" value_to_string in  
  let f_name = get_value "file_filename" value_to_string in
  let f_size = get_value "file_size" value_to_int64 in
  let f_downloaded = get_value "file_downloaded" value_to_int64 in
  let f = DcGlobals.new_file f_unchecked_tiger_root f_directory f_name f_size user group in
  if (file_downloaded f) <> f_downloaded then
    failwith "Disk file size don't match downloaded info";  
  if f_downloaded <> f_size then begin            (* check if file is downloaded already *)
    file_add f.file_file FileDownloading;
    (try
      ignore ( 
        get_value "file_sources" (value_to_list (fun v -> 
          let name = value_to_string v in
          if name <> empty_string then begin 
            let u = DcGlobals.new_user None name in
            let c = new_client_to_user_with_file u f in
            c.client_state <- DcDownloadWaiting f;
            c.client_pos <- f_downloaded;
          end 
        ))
      ) 
    with e -> 
      lprintf_nl "Exception (%s) while loading source" (Printexc2.to_string e) )
  end else begin
    file_add f.file_file FileDownloaded;   (* file is downloaded to temp - committing is needed *)
    remove_file_not_clients f              (* remove it immediately from dc *)
  end;
  as_file f.file_file
  
let file_to_value file =
  [
    "file_root", string_to_value file.file_unchecked_tiger_root;
    "file_dir",  string_to_value file.file_directory;
    (*"file_name", string_to_value file.file_name;*)
    (*"file_size", int64_to_value (file_size file);*)
    "file_downloaded", int64_to_value (file_downloaded file);
    "file_sources", 
    list_to_value (fun c ->
(*      let ip, port = 
        (match c.client_addr with
        | None -> "","" 
        | Some (cip , cport) -> 
            (Ip.to_string cip), cport )
      in
      SmallList [*)
      (match c.client_name with
      | Some name -> string_to_value name
      | None -> string_to_value "" )
(*        string_to_value ip;
        int_to_value port ]*)
    ) file.file_clients;
  ]
  
let client_to_value c =
  let name =
    (match c.client_name with
    | Some name -> string_to_value name
    | None -> raise Not_found )
  in
  (match c.client_addr with 
  | None -> raise Not_found
  | Some (ip, port) ->
      let list = [
          "client_name", name;
          "client_ip", Ip.ip_to_value ip;
          "client_port", int_to_value port;
        ]
      in
      list )
      
let value_to_client is_friend assocs = (* CHECK *)
  let get_value name conv = conv (List.assoc name assocs) in
  let ip = get_value "client_ip" Ip.value_to_ip in
  let port = get_value "client_port" value_to_int in
  let name = get_value "client_name" value_to_string in
  let c = DcGlobals.new_client () in
  c.client_addr <- Some (ip, port);
  c.client_name <- Some name; 
  (*  if is_friend then friend_add (as_client c.client_client);*)
  c
  
let load () =
  (try
    Options.load dc_shared_files_ini;
  with Sys_error _ ->
    Options.save_with_help dc_shared_files_ini );
  dc_config_files_loaded := true;
  lprintf_nl "config files loaded"

let save () =
  if !dc_config_files_loaded then begin
    Options.save_with_help dc_shared_files_ini;
  end

let _ =
  set_after_load_hook dc_shared_files_ini (fun _ ->
    let to_be_removed = ref [] in
    List.iter (fun n_dcsh ->
      (try                                               (* lets try to find existing dcsh info *)
        ignore (Hashtbl.find dc_shared_files_by_fullname n_dcsh.dc_shared_fullname);
        (*if (dcsh.dc_shared_size = n_dcsh.dc_shared_size) then begin
          if (dcsh.dc_shared_codedname = n_dcsh.dc_shared_codedname) then () 
          else  *)
        to_be_removed := n_dcsh :: !to_be_removed; 
      with _ ->
        if (Sys.file_exists n_dcsh.dc_shared_fullname) &&                                  (* if file exists ... *)
           (Unix32.getsize n_dcsh.dc_shared_fullname = n_dcsh.dc_shared_size) then begin   (* and size matches   *)
          (*lprintf_nl "New shared file from option-file (%s)" n_dcsh.dc_shared_codedname;*)
          Hashtbl.add dc_shared_files_by_fullname n_dcsh.dc_shared_fullname n_dcsh;
          Hashtbl.add dc_shared_files_by_codedname n_dcsh.dc_shared_codedname n_dcsh;
          if (n_dcsh.dc_shared_tiger_root = empty_string) (*|| (n_dcsh.dc_shared_tiger_array = [||])*) then begin 
            dc_files_to_hash := n_dcsh :: !dc_files_to_hash;
          end else begin
            Hashtbl.add dc_shared_files_by_hash n_dcsh.dc_shared_tiger_root n_dcsh;  
          end;
          dc_add_shared_file dc_shared_tree n_dcsh (String2.split n_dcsh.dc_shared_codedname '/') 
        end else to_be_removed := n_dcsh :: !to_be_removed )
    ) !!dc_saved_shared_files;

    List.iter (fun dcsh ->
      (*lprintf_nl "Removing shared file from option-file (%s)" dcsh.dc_shared_codedname;*)
      dc_saved_shared_files =:= List2.removeq dcsh !!dc_saved_shared_files
    ) !to_be_removed;
  ); 

  set_after_load_hook files_ini (fun _ ->
    lprintf_nl "LETS reverse clients list NOW";
  );

  network.op_network_load_complex_options <- (fun _ -> load () );
  network.op_network_save_complex_options <- (fun _ -> save () );
  server_ops.op_server_sort <- (fun s ->
      connection_last_conn s.server_connection_control);
  network.op_network_server_of_option <- value_to_server;
  server_ops.op_server_to_option <- server_to_value;  
  network.op_network_file_of_option <- value_to_file;
  file_ops.op_file_to_option <- file_to_value;
  client_ops.op_client_to_option <- client_to_value;
  network.op_network_client_of_option <- (fun is_friend c ->
      as_client (value_to_client is_friend c).client_client)

