open Options
open Unix
open BasicSocket
open TcpClientSocket
open Mftp
open Files
open Mftp_comm
open DownloadTypes
open DownloadOptions
open DownloadGlobals
open Gui_types
  
let verbose = ref false
  
let first_name file =
  match file.file_filenames with
    [] -> Filename.basename file.file_name
  | name :: _ -> name

let make_tagged files =
    (List.map (fun file ->
          { f_md4 = file.file_md4;
            f_ip = !client_ip;
            f_port = !client_port;
            f_tags = 
            { tag_name = "filename";
              tag_value = String (first_name file); } ::
            { tag_name = "size";
              tag_value = Uint32 file.file_size; } ::
            (
              (match file.file_format with
                  Unknown_format ->
                    (try
                        file.file_format <- 
                          DownloadMultimedia.get_info file.file_name
                      with _ -> ())
                | _ -> ()
              );
              
              match file.file_format with
                Unknown_format -> []
              | AVI _ ->
                  [
                    { tag_name = "type"; tag_value = String "Video" };
                    { tag_name = "format"; tag_value = String "avi" };
                  ]
              | Mp3 _ ->
                  [
                    { tag_name = "type"; tag_value = String "Audio" };
                    { tag_name = "format"; tag_value = String "mp3" };
                  ]
              | FormatType (format, kind) ->
                  [
                    { tag_name = "type"; tag_value = String kind };
                    { tag_name = "format"; tag_value = String format };
                  ]
            )
          }
      ) files)
    
let last_connected_server () =
  match !servers_list with 
  | s :: _ -> s
  | [] -> 
      servers_list := 
      Hashtbl2.fold (fun key s l ->
          s :: l
      ) servers_by_key [];
      match !servers_list with
        [] -> assert false
      | s :: _ -> s

let all_servers () =
  Hashtbl2.fold (fun key s l ->
      s :: l
  ) servers_by_key []


(****************************************************)
        
let update_options () =  
  known_servers =:=  Sort.list (fun s1 s2 -> 
      s1.server_score < s2.server_score ||
      (s1.server_score = s2.server_score &&
        (connection_last_conn 
            s1.server_connection_control) 
        < (connection_last_conn s2.server_connection_control))
  ) !!known_servers

let all_shared () =  
  let shared_files = ref [] in
  Hashtbl.iter (fun md4 file ->
      if file.file_shared then shared_files := file :: !shared_files
  ) files_by_md4;
  !shared_files
  
let server_handler s sock event = 
  match event with
    BASIC_EVENT (CLOSED _) ->
      if s.server_sock <> None then decr nservers;

      (*
            Printf.printf "%s:%d CLOSED received by server"
(Ip.to_string s.server_ip) s.server_port; print_newline ();
  *)
      connection_failed (s.server_connection_control);
      s.server_sock <- None;
      s.server_state <- NotConnected;
      s.server_score <- s.server_score - 1;
      s.server_users <- [];
      s.server_changed <- BigChange;
      (try !server_change_hook s with e -> 
            Printf.printf "Exception %s in server_change_hook"
              (Printexc.to_string e); 
            print_newline () );
      !server_is_disconnected_hook s
  | _ -> ()
  
let client_to_server s t sock =
  let module M = Mftp_server in
  if !verbose then begin
      Printf.printf "FROM SERVER:"; print_newline ();
      M.print t;  
      print_newline ();
    end;
  match t with
    M.SetIDReq t ->
      s.server_cid <- t;
      set_rtimeout (TcpClientSocket.sock sock) infinite_timeout;
      s.server_state <- Connected_initiating;
      s.server_score <- s.server_score + 5;
      connection_ok (s.server_connection_control);
      !server_change_hook s;

      server_send sock (
        let module A = M.AckID in
        M.AckIDReq A.t
      );
      
      server_send sock (M.ShareReq (make_tagged (
            let shared_files = all_shared () in
            if !nservers <= max_allowed_connected_servers then begin
                s.server_master <- true;
                shared_files
              end else
            match shared_files with
              f :: _ -> [f]
            | _ -> []
          )));
  
  | M.ServerListReq l ->
      let module Q = M.ServerList in
      List.iter (fun s ->
          ignore (add_server s.Q.ip s.Q.port);
      ) l
  
  | M.ServerInfoReq t ->
      
      let module Q = M.ServerInfo in
(* query file locations *)
      s.server_score <- s.server_score + 1;
      s.server_tags <- t.Q.tags;
      List.iter (fun tag ->
          match tag with
            { tag_name = "name"; tag_value = String name } -> 
              s.server_name <- name
          | { tag_name = "description"; tag_value = String desc } ->
              s.server_description <- desc
          | _ -> ()
      ) s.server_tags;
      s.server_state <- Connected_idle;
      !server_change_hook s;

      !server_is_connected_hook s sock
  
  | M.InfoReq (users, files) ->
      s.server_nusers <- users;
      s.server_nfiles <- files;
      s.server_changed <- SmallChange;
      !server_change_hook s
  | _ -> 
      !received_from_server_hook s sock t
      
let connect_server s =
  try
(*                Printf.printf "CONNECTING ONE SERVER"; print_newline (); *)
    connection_try s.server_connection_control;
    s.server_cid <- !client_ip;
    incr nservers;
    let sock = TcpClientSocket.connect (
        Ip.to_inet_addr s.server_ip) s.server_port 
        (server_handler s) in
    s.server_state <- Connecting;
    !server_change_hook s;
    
    set_reader sock (Mftp_comm.server_handler
        (client_to_server s));
    set_rtimeout (TcpClientSocket.sock sock) !!server_connection_timeout;
    set_handler sock (BASIC_EVENT RTIMEOUT) (fun s ->
        close s "timeout"  
    );
    
    s.server_sock <- Some sock;
    s.server_state <- Connecting;
    server_send sock (
      let module M = Mftp_server in
      let module C = M.Connect in
      M.ConnectReq {
        C.md4 = !!client_md4;
        C.ip = !client_ip;
        C.port = !client_port;
        C.tags = !client_tags;
      }
    );
  with _ -> 
      (*
      Printf.printf "%s:%d IMMEDIAT DISCONNECT "
      (Ip.to_string s.server_ip) s.server_port; print_newline ();
*)
(*      Printf.printf "DISCONNECTED IMMEDIATLY"; print_newline (); *)
      decr nservers;
      s.server_sock <- None;
      s.server_state <- NotConnected;
      !server_change_hook s
      
let rec connect_one_server () =
  match !servers_list with
    [] ->
      
      servers_list := !!known_servers;
      (*
      Hashtbl2.fold (fun key s l ->
          s :: l
) servers_by_key [];
  *)
      if !servers_list = [] then raise Not_found;
      connect_one_server ()
  | s :: list ->
      servers_list := list;
      if connection_can_try s.server_connection_control then
        begin
(* connect to server *)
          match s.server_sock with
            Some _ -> ()
          | None -> 
              if s.server_score < 0 then begin
(*                  Printf.printf "TOO BAD SCORE"; print_newline ();*)
                  connect_one_server ()
                end
              else
                connect_server s

        end
      

let force_check_server_connections user =
  if user || !nservers < max_allowed_connected_servers then begin
      if !nservers < !!max_connected_servers then
        begin
          for i = !nservers to !!max_connected_servers-1 do
            connect_one_server ();
          done;
        end
    end
    
let rec check_server_connections timer =
(*  Printf.printf "Check connections"; print_newline (); *)
  reactivate_timer timer;
  force_check_server_connections false

let remove_old_servers () =
  let list = ref [] in
  let day = 3600. *. 24. in
  let min_last_conn =  last_time () -. 
    float_of_int !!max_server_age *. day in
  List.iter (fun s ->
      if connection_last_conn s.server_connection_control < min_last_conn then
        begin
(*                  Printf.printf "*******  SERVER REMOVED ******"; 
                  print_newline (); *)
          s.server_state <- Removed;
          !server_change_hook s
        end else
        list := s :: !list
  ) !!known_servers;
  if List.length !list > 200 then
    known_servers =:= List.rev !list
  
  
let remove_old_servers_timer () = 
  if List.length !!known_servers > 1000 then
    remove_old_servers ()

(* Don't let more than max_allowed_connected_servers running for
more than 5 minutes *)
    
let update_master_servers timer =
(*  Printf.printf "UPDATE MASTER SERVERS"; print_newline ();*)
  reactivate_timer timer;
(*  Printf.printf "update_master_servers"; print_newline (); *)
  let nmasters = ref 0 in
  List.iter (fun s ->
      if s.server_master then
        match s.server_sock with
          None -> 
            (*
Printf.printf "SERVER IS DISCONNECTED !!!"; print_newline ();
*)
            ()
        | Some _ -> 
            (*
Printf.printf "Master Server is connected"; print_newline ();
  *)
            incr nmasters;
  ) !connected_server_list;
  let nconnected_servers = ref 0 in
  List.iter (fun s ->
      incr nconnected_servers;
      if not s.server_master then
        if !nmasters < max_allowed_connected_servers then begin
            match s.server_sock with
              None -> 
              (*  Printf.printf "MASTER NOT CONNECTED"; print_newline ();  *)
                ()
            | Some sock ->                
(*                Printf.printf "NEW MASTER SERVER"; print_newline (); *)
                s.server_master <- true;
                incr nmasters;
                server_send sock (Mftp_server.ShareReq
                  (make_tagged (all_shared ())));
          end else
        if !nconnected_servers > max_allowed_connected_servers then begin
(* remove one third of the servers every 5 minutes *)
            nconnected_servers := !nconnected_servers - 3;
(*            Printf.printf "DISCONNECT FROM EXTRA SERVER %s:%d "
(Ip.to_string s.server_ip) s.server_port; print_newline ();
  *)
            (match s.server_sock with
                None ->                   
                  (*
                  Printf.printf "Not connected !"; 
print_newline ();
*)
                  ()

              | Some sock ->
  (*                Printf.printf "shutdown"; print_newline (); *)
                  (shutdown sock "max allowed"));
          end
  ) 
  (* reverse the list, so that first servers to connect are kept ... *)
  (List.rev !connected_server_list)
    
  