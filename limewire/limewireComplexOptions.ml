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
open CommonFile
open Options
open LimewireTypes
open LimewireOptions
open LimewireGlobals

let ultrapeers = define_option limewire_ini ["ultrapeers"]
    "Known ultrapeers" (list_option (tuple2_option (Ip.option, int_option)))
  []

module ClientOption = struct
    
    let value_to_client v = 
      match v with
      | Module assocs ->
          
          let get_value name conv = conv (List.assoc name assocs) in
          let get_value_nil name conv = 
            try conv (List.assoc name assocs) with _ -> []
          in
          let client_ip = get_value "client_ip" (from_value Ip.option)
          in
          let client_port = get_value "client_port" value_to_int in
          let client_uid = get_value "client_uid" (from_value Md4.option) in
          let s = new_source client_uid client_ip client_port in
          
          (try
              s.source_speed <- get_value "client_speed" value_to_int 
            with _ -> ());
          (try
              s.source_push <- get_value "client_push" value_to_bool
            with _ -> ());
          s
      | _ -> failwith "Options: Not a client"

                  
    let client_to_value s =
      Options.Module [
        "client_uid", to_value Md4.option s.source_uid;
        "client_ip", to_value Ip.option s.source_ip;
        "client_port", int_to_value s.source_port;
        "client_speed", int_to_value s.source_speed;
        "client_push", bool_to_value s.source_push;
      ]

    let t =
      define_option_class "Client" value_to_client client_to_value

  end
  
  let value_to_file is_done assocs =
          let get_value name conv = conv (List.assoc name assocs) in
          let get_value_nil name conv = 
            try conv (List.assoc name assocs) with _ -> []
          in
          
          let file_name = get_value "file_name" value_to_string in
          let file_id = 
            try
              Md4.of_string (get_value "file_id" value_to_string)
            with _ -> failwith "Bad file_id"
          in
          let file_size = try
              value_to_int32 (List.assoc "file_size" assocs) 
            with _ -> failwith "Bad file size"
          in
          
          let file = new_file file_id file_name file_size in
          
          (try
              ignore (get_value "file_sources" (value_to_list (fun v ->
                    match v with
                      SmallList [c; index] | List [c;index] ->
                        let s = ClientOption.value_to_client c in
                        add_download file s (value_to_int index)
                    | _ -> failwith "Bad source"
                )))
            with e -> 
                Printf.printf "Exception %s while loading source"
                  (Printexc.to_string e); 
                print_newline ();
                );
          as_file file.file_file
          
    let file_to_value file =
      let r = file.file_result in
       [
        "file_size", int32_to_value file.file_result.result_file.file_size;
        "file_name", string_to_value file.file_result.result_file.file_name;
        "file_downloaded", int32_to_value file.file_downloaded;
        "file_id", string_to_value (Md4.to_string file.file_id);
        "file_sources", 
        list_to_value (fun s ->
            SmallList [ClientOption.client_to_value s;
              int_to_value (List.assq r s.source_files)]
            ) r.result_sources
        ;
      ]

let old_files = 
  define_option limewire_ini ["old_files"]
    "" (list_option (tuple2_option (string_option, int32_option))) []
    
    
let save_config () =
  let list = Fifo.to_list ultrapeers_queue in
  ultrapeers =:= (List.map (fun s ->
        (s.server_ip, s.server_port)) !connected_servers) @ list;
  Printf.printf "SAVE OPTIONS"; print_newline ();
  Options.save_with_help limewire_ini

  
let _ =
  network.op_network_add_file <- value_to_file;
  file_ops.op_file_to_option <- file_to_value
