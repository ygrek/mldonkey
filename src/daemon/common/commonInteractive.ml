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
open Printf2
open CommonOptions
open BasicSocket
open Options

open CommonClient
open CommonNetwork
open CommonUserDb
open CommonFile
open CommonGlobals
open CommonSearch
open CommonServer
open CommonTypes
open CommonComplexOptions

let log_prefix = "[cInt]"

let lprintf_nl fmt =
  lprintf_nl2 log_prefix fmt

let lprintf_n fmt =
  lprintf2 log_prefix fmt

(*************  ADD/REMOVE FUNCTIONS ************)
let check_forbidden_chars (uc : Charset.uchar) =
  match uc with
  | 47  (* '/'  *)
  | 92  (* '\\' *) -> 95 (* '_' *)
  (* Windows can't do these *)
  | 58  (* ':'  *)
  | 42  (* '*'  *)
  | 63  (* '?'  *)
  | 34  (* '"'  *)
  | 60  (* '<'  *)
  | 62  (* '>'  *)
  | 124 (* '|'  *)
  | 37  (* '%'  *) when Autoconf.windows -> 95 (* '_' *)
  | _ -> uc

let canonize_basename name =
  let buf = Buffer.create 100 in
  let uname = Charset.Locale.to_utf8 name in
  for i = 0 to Charset.utf8_length uname - 1 do
    (* replace chars on users request *)
    let uc = Charset.utf8_get uname i in
    try
      let us = List.assoc uc !!utf8_filename_conversions in
      for j = 0 to Charset.utf8_length us - 1 do
        let uc' = Charset.utf8_get us j in
        let uc'' = check_forbidden_chars uc' in
        Charset.add_uchar buf uc''
      done
    with _ ->
      begin
        let uc' = check_forbidden_chars uc in
        Charset.add_uchar buf uc'
      end
  done;
  if not Autoconf.windows && not !!filenames_utf8 then
    Charset.Locale.to_locale (Buffer.contents buf)
  else
    Buffer.contents buf (* Windows uses patched OCaml which always uses Unicode chars *)

let last_sent_dir_warning = Hashtbl.create 10

let hdd_full_log_closed = ref false
let all_temp_queued = ref false

let send_dirfull_warning dir full line1 =
  if !!smtp_server <> "" && !!smtp_port <> 0 then begin
  let status = if full then "is full" else "has enough space again" in
  lprintf_nl "WARNING: Directory %s %s, %s" dir status line1;
  if (not (keep_console_output ())) then
    Printf.eprintf "\n%s WARNING: Directory %s %s, %s\n%!" (log_time ()) dir status line1;
  if !!hdd_send_warning_interval <> 0 then
    let current_time = last_time () in
    let time_threshold =
      current_time - !!hdd_send_warning_interval * Date.hour_in_secs in
    let send_mail_again =
      try
        let last = Hashtbl.find last_sent_dir_warning dir in
        last < time_threshold
      with Not_found -> true
    in
    if send_mail_again then begin
      if full then Hashtbl.replace last_sent_dir_warning dir current_time;
      CommonEvent.add_event (Console_message_event
        (Printf.sprintf "\nWARNING: %s %s, %s\n" dir status line1));
      match String2.tokens !!mail with
      | [] -> ()
      | mails ->
        let module M = Mailer in
        let subject = Printf.sprintf "[mldonkey@%s] AUTOMATED WARNING: %s %s" (Unix.gethostname ()) dir status in
        let mail = {
          M.mail_to = mails;
          M.mail_from = List.hd mails;
          M.mail_subject = subject;
          M.mail_body = line1;
          M.smtp_login = !!smtp_login;
          M.smtp_password = !!smtp_password;
        } in
        try
          M.sendmail !!smtp_server !!smtp_port !!add_mail_brackets mail
        with _ -> ()
    end
  end

let file_committed_name incoming_dir file =
  (try Unix2.safe_mkdir incoming_dir with _ -> ());
  let fs = Unix32.filesystem_type incoming_dir in
  let namemax =
    match Unix32.fnamelen incoming_dir with
    | None -> 0
    | Some v -> v
  in
  let new_name =
    Filename2.filesystem_compliant 
      (canonize_basename (file_best_name file)) fs namemax in

  let new_name =
    if Sys.file_exists (Filename.concat incoming_dir new_name) then
      let rec iter num =
        let new_name =
          Filename2.filesystem_compliant
            (Printf.sprintf "%s_%d" new_name num) fs namemax in
          if Sys.file_exists (Filename.concat incoming_dir new_name) then
            iter (num+1)
          else new_name
      in
      iter 1
    else new_name in
  set_file_best_name file (file_best_name file) ~fs namemax;
  Filename.concat incoming_dir new_name

let script_for_file file incoming new_name =
  let info = file_info file in
  let temp_name = file_disk_name file in
  let file_id = Filename.basename temp_name in
  let size = Int64.to_string (file_size file) in
  let duration =
    string_of_int ((BasicSocket.last_time ()) - info.G.file_age)
  in
  let network = network_find_by_num info.G.file_network in
  let filename = Filename.basename new_name in
  let file_group_info =
    match file_group file with
    | None -> 0, []
    | Some _ ->
      let users = ref [] in
      let counter = ref 0 in
      user2_users_iter (fun u ->
        if file_owner file <> u &&
          user2_can_view_file u (file_owner file) (file_group file) then
            begin
              incr counter;
              users :=  (Printf.sprintf "FILE_GROUP_USER_%d" !counter, u.user_name) ::
                        (Printf.sprintf "FILE_GROUP_DIR_%d" !counter, u.user_commit_dir) :: !users
            end);
      !counter, !users
  in
  begin try
  MlUnix.fork_and_exec !!file_completed_cmd
      [|  (* keep those for compatibility *)
      "";
      file_id; (* $1 *)
      size; (* $2 *)
      filename (* $3 *)
    |]
     ~vars:([("TEMPNAME",  temp_name);
            ("FILEID",    file_id);
            ("FILESIZE",  size);
            ("FILENAME",  filename);
            ("FILEHASH",  string_of_uids info.G.file_uids);
            ("DURATION",  duration);
            ("DLFILES",   string_of_int (List.length !!files)); 
            ("INCOMING",  incoming);
            ("NETWORK",   network.network_name);
            ("ED2K_HASH", (file_print_ed2k_link filename (file_size file) info.G.file_md4));
            ("FILE_OWNER",(file_owner file).user_name);
            ("FILE_GROUP",user2_print_group (file_group file));
            ("USER_MAIL", ( if (file_owner file).user_mail <> "" then
                              (file_owner file).user_mail
                            else
                              match String2.tokens !!mail with [] -> "" | x::_ -> x));
            ("FILE_GROUP_CNT", string_of_int (fst (file_group_info)));
            ]
            @ snd (file_group_info))

  with e -> 
      lprintf_nl "Exception %s while executing %s"
        (Printexc2.to_string e) !!file_completed_cmd
  end

(********
These two functions 'file_commit' and 'file_cancel' should be the two only
functions in mldonkey able to destroy a file, the first one by moving it,
the second one by deleting it.

Note that when the network specific file_commit function is called, the
file has already been moved to the incoming/ directory under its new
name.
*)

let file_commit file =
  let impl = as_file_impl file in
  if impl.impl_file_state = FileDownloaded then
    let subfiles = file_files file in
    match subfiles with
      primary :: secondary_files ->
        if primary == file then
       (try
          let file_name = file_disk_name file in
          let incoming =
            incoming_dir
            (Unix2.is_directory file_name)
            ~needed_space:(file_size file)
            ~user:(file_owner file)
            ()
          in

          let new_name = file_committed_name incoming.shdir_dirname file in
            if Unix2.is_directory file_name then begin
              Unix2.safe_mkdir new_name;
              Unix2.chmod new_name !Unix32.create_dir_mode;
            end;

(*          the next line really moves the file *)
            set_file_disk_name file new_name;

            if !!file_completed_cmd <> "" then
              script_for_file file incoming.shdir_dirname new_name;

            let best_name = file_best_name file in
            Unix32.destroy (file_fd file);

            if Unix2.is_directory file_name then Unix2.remove_all_directory file_name;

            let impl = as_file_impl file in
(* When the commit action is called, the file is supposed not to exist anymore. *)
            impl.impl_file_ops.op_file_commit impl.impl_file_val new_name;

            begin
              try
                if not (Unix2.is_directory new_name) then
                  ignore (CommonShared.new_shared
                      incoming.shdir_dirname incoming.shdir_priority
                      best_name new_name);
              with e ->
                  lprintf_nl "Exception %s while trying to share committed file"
                    (Printexc2.to_string e);
            end;

            update_file_state impl FileShared;
            done_files =:= List2.removeq file !!done_files;
            files =:= List2.removeq file !!files;

            List.iter (fun file ->
(* Commit the file first, and share it after... *)
                try
                  let impl = as_file_impl file in
                  update_file_state impl FileCancelled;
                  impl.impl_file_ops.op_file_cancel impl.impl_file_val;
                  done_files =:= List2.removeq file !!done_files;
                  files =:= List2.removeq file !!files;

                with e ->
                    lprintf_nl "Exception %s in file_commit secondaries" (Printexc2.to_string e);
            ) secondary_files
        with
          Incoming_full ->
            send_dirfull_warning "" true
              (Printf.sprintf "all incoming dirs are full, can not commit %s" (file_best_name file))
        | e -> lprintf_nl "Exception in file_commit: %s" (Printexc2.to_string e))
    | _ -> assert false

let file_cancel file user =
  if user2_allow_file_admin file user then
  try
    let impl = as_file_impl file in
    if impl.impl_file_state <> FileCancelled then
      let subfiles = file_files file in
      if file != List.hd subfiles then
        failwith "Cannot cancel non primary file";
      List.iter (fun file ->
          try
            update_file_state impl FileCancelled;
            impl.impl_file_ops.op_file_cancel impl.impl_file_val;
            files =:= List2.removeq file !!files;
          with e ->
              lprintf_nl "Exception %s in file_cancel" (Printexc2.to_string e);
      ) subfiles;
      try
        let fd = file_fd file in
        (try
           Unix32.remove fd
         with e ->
           lprintf_nl "Sys.remove %s exception %s"
             (file_disk_name file)
             (Printexc2.to_string e));
        Unix32.destroy fd
      with Not_found -> ()
  with e ->
      lprintf_nl "Exception in file_cancel: %s" (Printexc2.to_string e)

let mail_for_completed_file file =
  let usermail = (file_owner file).user_mail in
  let mail = String2.tokens !!mail in
  if (mail <> [] || usermail <> "") && !!smtp_server <> "" && !!smtp_port <> 0 then begin
    let module M = Mailer in
    let info = file_info file in
    let line1 = "mldonkey has completed the download of:\r\n\r\n" in

    let line2 = Printf.sprintf "\r\nFile: %s\r\nSize: %Ld bytes\r\nHash: %s\r\nFile was downloaded in %s\r\n"
      (file_best_name file)
      (file_size file)
      (string_of_uids info.G.file_uids)
      (let age = (BasicSocket.last_time ()) - info.G.file_age in Date.time_to_string age "verbose")
    in

    let line3 = if (file_comment file) = "" then "" else
        Printf.sprintf "\r\nComment: %s\r\n" (file_comment file)
    in

    let subject = if !!filename_in_subject then
        Printf.sprintf "mldonkey - %s complete" (file_best_name file)
      else
        Printf.sprintf "mldonkey - download complete"
    in

(* TODO: This information can be wrong *)
    let incoming = incoming_dir (Unix2.is_directory (file_disk_name file)) () in

    let line4 = if !!url_in_mail = "" then "" else
      Printf.sprintf "\r\n<%s/%s%s/%s>\r\n"
        !!url_in_mail
        incoming.shdir_dirname
        (if (file_owner file).user_commit_dir = "" then ""
         else Printf.sprintf "/%s" (file_owner file).user_commit_dir)
        (Url.encode (file_best_name file))
    in

    let line5 = if !!auto_commit then "" else
        Printf.sprintf "\r\nauto_commit is disabled, file is not committed to incoming"
    in

    let line6 =
      Printf.sprintf "\r\nUser/Group: %s:%s" (file_owner file).user_name (user2_print_group (file_group file))
    in

    let line7 =
      Printf.sprintf "\r\nHost: %s\r\n" (Unix.gethostname ())
    in

    let send_mail address admin =
      let mail = {
        M.mail_to = address;
        M.mail_from = List.hd address;
        M.mail_subject = subject;
        M.mail_body = line1 ^ line2 ^ line3 ^ line4 ^ line5 ^ (if admin then line6 else "") ^ line7;
        M.smtp_login = !!smtp_login;
        M.smtp_password = !!smtp_password;
      } in
        M.sendmail !!smtp_server !!smtp_port !!add_mail_brackets mail
    in
    if mail <> [] then send_mail mail true; (* Multiuser ToDo: this mail is for the admin user, optional? *)
    if usermail <> "" && [usermail] <> mail then (try send_mail [usermail] false with Not_found -> ())
  end

let file_completed (file : file) =
  try
    let impl = as_file_impl file in
    if impl.impl_file_state = FileDownloading then begin
        CommonSwarming.duplicate_chunks ();
        set_file_release file false (admin_user ());
        files =:= List2.removeq file !!files;
        done_files =:= file :: !!done_files;
        update_file_state impl FileDownloaded;
        (try mail_for_completed_file file with e ->
              lprintf_nl "Exception %s in sendmail" (Printexc2.to_string e);
              );
      end
  with e ->
      lprintf_nl "Exception in file_completed: %s" (Printexc2.to_string e)

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
              files =:= !!files @ [file]);
        update_file_state impl state
      end
  with e ->
      lprintf_nl "[cInt] Exception in file_add: %s" (Printexc2.to_string e)

let server_remove server =
  begin
    match server_state server with
      NotConnected _ -> ()
    | _ -> server_disconnect server
  end;
  try
    let impl = as_server_impl server in
    if impl.impl_server_state <> RemovedHost then begin
        set_server_state server RemovedHost;
        (try impl.impl_server_ops.op_server_remove impl.impl_server_val
          with _ -> ());
        servers =:= Intmap.remove (server_num server) !!servers
      end
  with e ->
      lprintf_nl "[cInt] Exception in server_remove: %s" (Printexc2.to_string e)

let server_add impl =
  let server = as_server impl in
  if impl.impl_server_state = NewHost then begin
      server_update_num impl;
      servers =:= Intmap.add (server_num server) server !!servers;
      impl.impl_server_state <- NotConnected (BasicSocket.Closed_by_user, -1);
    end

let friend_add c =
  let impl = as_client_impl c in
  if not (is_friend c) then begin
      set_friend c;
      client_must_update c;
      friends =:= c :: !!friends;
      contacts := List2.removeq c !contacts;
      if network_is_enabled ((as_client_impl c).impl_client_ops.op_client_network) then
        impl.impl_client_ops.op_client_browse impl.impl_client_val true
    end

(* Maybe we should not add the client to the contact list and completely remove
it ? *)
let friend_remove c =
  try
    let impl = as_client_impl c in
    if is_friend c then begin
        set_not_friend c;
        client_must_update c;
        friends =:= List2.removeq c !!friends;
        impl.impl_client_ops.op_client_clear_files impl.impl_client_val
      end else
    if is_contact c then begin
        set_not_contact c;
        client_must_update c;
        contacts := List2.removeq c !contacts;
        impl.impl_client_ops.op_client_clear_files impl.impl_client_val
      end

  with e ->
      lprintf_nl "Exception in friend_remove: %s" (Printexc2.to_string e)

let contact_add c =
  let impl = as_client_impl c in
  if not (is_friend c || is_contact c) then begin
      set_contact c;
      client_must_update c;
      contacts := c :: !contacts;
      if network_is_enabled ((as_client_impl c).impl_client_ops.op_client_network) then
        impl.impl_client_ops.op_client_browse impl.impl_client_val true
    end

let contact_remove c =
  try
    let impl = as_client_impl c in
    if is_contact c then begin
        set_not_contact c;
        client_must_update c;
        contacts := List2.removeq c !contacts;
        impl.impl_client_ops.op_client_clear_files impl.impl_client_val
      end
  with e ->
      lprintf_nl "Exception in contact_remove: %s" (Printexc2.to_string e)

let clean_exit n =
  begin
  let can_exit = networks_for_all network_clean_exit in
  if can_exit then exit_properly n
  else 
    let rec retry_later retry_counter =
      add_timer 1. (fun _ ->
        let can_exit = networks_for_all network_clean_exit in
        if can_exit || retry_counter > !!shutdown_timeout then
          exit_properly n
        else retry_later (retry_counter + 1)) in
    retry_later 0;

  if (upnp_port_forwarding ()) then
    begin
    if !!clear_upnp_port_at_exit then
      UpnpClient.remove_all_maps 0 ;
    UpnpClient.job_stop 3;
    end;
  end

let time_of_sec sec =
  let hours = sec / 60 / 60 in
  let rest = sec - hours * 60 * 60 in
  let minutes = rest / 60 in
  let seconds = rest - minutes * 60 in
  if hours > 0 then Printf.sprintf "%d:%02d:%02d" hours minutes seconds
  else if minutes > 0 then Printf.sprintf "%d:%02d" minutes seconds
  else Printf.sprintf "00:%02d" seconds


let display_vd = ref false
let display_bw_stats = ref false

let start_download file =
  if !!pause_new_downloads then file_pause file (admin_user ());
  if !!release_new_downloads then set_file_release file true (admin_user ());
  if !!file_started_cmd <> "" then
    begin
      let info = file_info file in
      let temp_name = file_disk_name file in
      let file_id = Filename.basename temp_name in
      let size = Int64.to_string (file_size file) in
      let network = network_find_by_num info.G.file_network in
      let filename = file_best_name file in
      let file_group_info =
        match file_group file with
        | None -> 0, []
        | Some _ ->
          let users = ref [] in
          let counter = ref 0 in
          user2_users_iter (fun u ->
            if file_owner file <> u &&
              user2_can_view_file u (file_owner file) (file_group file) then
                begin
                  incr counter;
                  users :=  (Printf.sprintf "FILE_GROUP_USER_%d" !counter, u.user_name) ::
                            (Printf.sprintf "FILE_GROUP_DIR_%d" !counter, u.user_commit_dir) :: !users
                end);
          !counter, !users
      in
      MlUnix.fork_and_exec  !!file_started_cmd
      [|
      !!file_started_cmd;
        "-file";
        string_of_int (CommonFile.file_num file);
      |]
     ~vars:([("TEMPNAME",  temp_name);
            ("FILEID",    file_id);
            ("FILESIZE",  size);
            ("FILENAME",  filename);
            ("FILEHASH",  string_of_uids info.G.file_uids);
            ("DLFILES",   string_of_int (List.length !!files)); 
            ("NETWORK",   network.network_name);
            ("ED2K_HASH", (file_print_ed2k_link filename (file_size file) info.G.file_md4));
            ("FILE_OWNER",(file_owner file).user_name);
            ("FILE_GROUP",user2_print_group (file_group file));
            ("USER_MAIL", ( if (file_owner file).user_mail <> "" then
                              (file_owner file).user_mail
                            else
                              match String2.tokens !!mail with [] -> "" | x::_ -> x));
            ("FILE_GROUP_CNT", string_of_int (fst (file_group_info)));
            ]
            @ snd (file_group_info))
    end

let download_file o arg =
  let user = o.conn_user in
  let buf = o.conn_buf in
  Printf.bprintf buf "%s\n" (
    try
      match user.ui_last_search with
        None -> "no last search"
      | Some s ->
          let result = List.assoc (int_of_string arg) user.ui_last_results in
          let files = CommonResult.result_download
            result [] false user.ui_user in
          List.iter start_download files;
          "download started"
    with
    | Failure s -> s
    | _ -> "could not start download"
  )

let start_search user query buf =
  let s = CommonSearch.new_search user query in
  begin
    match s.search_type with
    LocalSearch ->
        CommonSearch.local_search s
    | _ ->
        networks_iter (fun r ->
            if query.GuiTypes.search_network = 0 ||
              r.network_num = query.GuiTypes.search_network
            then network_search r s buf);
  end;
  s


let network_must_update n =
   CommonEvent.add_event (Network_info_event n)

let network_display_stats o =
  networks_iter_all (fun r ->
    try
      if List.mem NetworkHasStats r.network_flags then
        network_display_stats r o
    with _ -> ())

let print_connected_servers o =
  let buf = o.conn_buf in
  networks_iter (fun r ->
      try
        let list = network_connected_servers r in
        if List.mem NetworkHasServers r.network_flags ||
          List.mem NetworkHasSupernodes r.network_flags
        then begin
           if use_html_mods o then begin
               html_mods_table_one_row buf "serversTable" "servers" [
                 ("", "srh", Printf.sprintf (_b "--- Connected to %d servers on the %s network ---\n")
                   (List.length list) r.network_name); ]
             end
           else
             Printf.bprintf buf (_b "--- Connected to %d servers on the %s network ---\n")
               (List.length list) r.network_name;
          end;
         if use_html_mods o && List.length list > 0 then server_print_html_header buf "C";

      html_mods_cntr_init ();
       List.iter (fun s ->
        server_print s o;
       ) (List.sort (fun s1 s2 -> compare (server_num s1) (server_num s2)) list);
        if use_html_mods o && List.length list > 0 then
           Printf.bprintf buf "\\</table\\>\\</div\\>";
        if Autoconf.donkey = "yes" && r.network_name = "Donkey" && not !!enable_servers then
          begin
            if use_html_mods o then Printf.bprintf buf "\\<div class=servers\\>";
            Printf.bprintf buf (_b "You disabled server usage, therefore you are not able to connect ED2K servers.\n");
            Printf.bprintf buf (_b "To use servers again 'set enable_servers true'\n");
            if use_html_mods o then Printf.bprintf buf "\\</div\\>"
          end;
       with e ->
           Printf.bprintf  buf "Exception %s in print_connected_servers"
             (Printexc2.to_string e);
  )

let send_custom_query user buf query args =
  try
    let q = List.assoc query (CommonComplexOptions.customized_queries()) in
    let args = ref args in
    let get_arg arg_name =
(*      lprintf "Getting %s\n" arg_name; *)
      match !args with
        (label, value) :: tail ->
          args := tail;
          if label = arg_name then value else begin
              Printf.bprintf buf "Error expecting argument %s instead of %s" arg_name label;
              raise Exit
            end
      | _ ->
          Printf.bprintf buf "Error while expecting argument %s" arg_name;
          raise Exit
    in
    let rec iter q =
      match q with
      | Q_COMBO _ -> assert false
      | Q_KEYWORDS _ ->
          let value = get_arg "keywords" in
          want_and_not andnot (fun w -> QHasWord w) QNone value

      | Q_AND list ->
          begin
            let ands = ref [] in
            List.iter (fun q ->
                try ands := (iter q) :: !ands with _ -> ()) list;
            match !ands with
              [] -> raise Not_found
            | [q] -> q
            | q1 :: tail ->
                List.fold_left (fun q1 q2 -> QAnd (q1,q2)) q1 tail
          end

      | Q_HIDDEN list ->
          begin
            let ands = ref [] in
            List.iter (fun q ->
                try ands := (iter q) :: !ands with _ -> ()) list;
            match !ands with
              [] -> raise Not_found
            | [q] -> q
            | q1 :: tail ->
                List.fold_left (fun q1 q2 -> QAnd (q1,q2)) q1 tail
          end

      | Q_OR list ->
          begin
            let ands = ref [] in
            List.iter (fun q ->
                try ands := (iter q) :: !ands with _ -> ()) list;
            match !ands with
              [] -> raise Not_found
            | [q] -> q
            | q1 :: tail ->
                List.fold_left (fun q1 q2 -> QOr (q1,q2)) q1 tail
          end

      | Q_ANDNOT (q1, q2) ->
          begin
            let r1 = iter q1 in
            try
              QAndNot(r1, iter q2)
            with Not_found -> r1
          end

      | Q_MODULE (s, q) -> iter q

      | Q_MINSIZE _ ->
          let minsize = get_arg "minsize" in
          let unit = get_arg "minsize_unit" in
          if minsize = "" then raise Not_found;
          let minsize = Int64.of_string minsize in
          let unit = Int64.of_string unit in
          QHasMinVal (Field_Size, Int64.mul minsize unit)

      | Q_MAXSIZE _ ->
          let maxsize = get_arg "maxsize" in
          let unit = get_arg "maxsize_unit" in
          if maxsize = "" then raise Not_found;
          let maxsize = Int64.of_string maxsize in
          let unit = Int64.of_string unit in
          QHasMaxVal (Field_Size, maxsize ** unit)

      | Q_FORMAT _ ->
          let format = get_arg "format" in
          let format_propose = get_arg "format_propose" in
          let format = if format = "" then
              if format_propose = "" then raise Not_found
              else format_propose
            else format in
          want_comb_not andnot
            or_comb
            (fun w -> QHasField(Field_Format, w)) QNone format

      | Q_MEDIA _ ->
          let media = get_arg "media" in
          let media_propose = get_arg "media_propose" in
          let media = if media = "" then
              if media_propose = "" then raise Not_found
              else media_propose
            else media in
          QHasField(Field_Type, media)

      | Q_MP3_ARTIST _ ->
          let artist = get_arg "artist" in
          if artist = "" then raise Not_found;
          want_comb_not andnot and_comb
            (fun w -> QHasField(Field_Artist, w)) QNone artist

      | Q_MP3_TITLE _ ->
          let title = get_arg "title" in
          if title = "" then raise Not_found;
          want_comb_not andnot and_comb
            (fun w -> QHasField(Field_Title, w)) QNone title

      | Q_MP3_ALBUM _ ->
          let album = get_arg "album" in
          if album = "" then raise Not_found;
          want_comb_not andnot and_comb
            (fun w -> QHasField(Field_Album, w)) QNone album

      | Q_MP3_BITRATE _ ->
          let bitrate = get_arg "bitrate" in
          if bitrate = "" then raise Not_found;
          QHasMinVal(Field_KNOWN "bitrate", Int64.of_string bitrate)

    in
    try
      let request = CommonIndexing.simplify_query (iter q) in
      Printf.bprintf buf "Sending query !!!";

      let s =
        let module G = GuiTypes in
        { G.search_num = 0;
          G.search_query = request;
          G.search_type = RemoteSearch;
          G.search_max_hits = 10000;
          G.search_network = (
            try
              let net = get_arg "network" in
              (network_find_by_name net).network_num
            with _ -> 0);
        }
      in
      ignore (start_search user s buf)
    with
      Not_found ->
        Printf.bprintf buf "Void query %s" query
  with
    Not_found ->
      Printf.bprintf buf "No such custom search %s" query
  | Exit -> ()
  | e ->
      Printf.bprintf buf "Error %s while parsing request"
        (Printexc2.to_string e)

let sort_options l =
  List.sort (fun o1 o2 ->
      String.compare o1.option_name o2.option_name) l

let opfile_args r opfile =
  let prefix = r.network_shortname ^ "-" in
  simple_options prefix opfile true

let all_simple_options () =
  let options = ref (sort_options
      (simple_options "" downloads_ini true)
    )
  in
  networks_iter_all (fun r ->
      List.iter (fun opfile ->
          options := !options @ (opfile_args r opfile)
      )
      r.network_config_file
  );
  !options

let parse_simple_options args =
  let v = all_simple_options () in
  match args with
    [] -> v
    | args ->
      let match_star = Str.regexp "\\*" in
      let options_filter = Str.regexp ("^\\("
        ^ (List.fold_left (fun acc a -> acc
        ^ (if acc <> "" then "\\|" else "")
        ^ (Str.global_replace match_star ".*" a)) "" args)
        ^ "\\)$") in
      List.filter (fun o -> Str.string_match options_filter o.option_name 0) v

let some_simple_options num =
               let cnt = ref 0 in
               let options = ref [] in
               networks_iter_all (fun r ->
                       List.iter (fun opfile ->
                               if !cnt = num then begin
          options := !options @ (opfile_args r opfile)

                       end;
                       incr cnt
                       ) r.network_config_file
               );
               !options

let all_active_network_opfile_network_names () =
       let names = ref [] in
       networks_iter_all (fun r ->
               List.iter (fun opfile ->
                       names := !names @ [r.network_name]
               ) r.network_config_file
       );
       !names

let apply_on_fully_qualified_options name f =
  let iter prefix opfile =
    let args = simple_options prefix opfile true in
    List.iter (fun o ->
        if o.option_name = name then
          (f opfile o.option_shortname o.option_value; raise Exit))
    args
  in
  try
    iter "" downloads_ini;
    iter "" users_ini;
    if not (networks_iter_all_until_true (fun r ->
            try
              List.iter (fun opfile ->
                  let prefix = r.network_shortname ^ "-" in
                  iter prefix opfile;
              )
              r.network_config_file ;
              false
            with Exit -> true
        )) then begin
        lprintf_nl "Could not set option %s" name;
        raise Not_found
      end
  with Exit -> ()

let get_fully_qualified_options name =
  let value = ref None in
  (try
      apply_on_fully_qualified_options name (fun opfile old_name old_value ->
      value := Some (get_simple_option opfile old_name)
      );
    with _ -> ());
  match !value with
    None -> "unknown"
  | Some s -> s

let set_fully_qualified_options name value ?(user = None) ?(ip = None) ?(port = None) ?(gui_type = None) () =
  let old_value = get_fully_qualified_options name in
  apply_on_fully_qualified_options name
    (fun opfile old_name old_value -> set_simple_option opfile old_name value);
  if !verbose && old_value <> get_fully_qualified_options name then
    begin
      let ip_port_text =
        match ip with
        | None -> "IP unknown"
        | Some ip ->
            Printf.sprintf "from host %s%s" (Ip.to_string ip)
              (match port with | None -> "" | Some port -> Printf.sprintf ":%d" port)
      in
      lprintf_nl "User %s changed option %s %s %s, old: %s, new %s"
        (match user with | None -> "unknown" | Some user -> user)
        name ip_port_text
        (match gui_type with
        | None -> "GUI type unknown"
        | Some gt -> Printf.sprintf "using %s interface" (connection_type_to_text gt))
        old_value (get_fully_qualified_options name)
    end

let keywords_of_query query =
  let keywords = ref [] in

  let rec iter q =
    match q with
    | QOr (q1,q2)
    | QAnd (q1, q2) -> iter q1; iter q2
    | QAndNot (q1,q2) -> iter q1
    | QHasWord w -> keywords := (String2.split_simplify w ' ') @ !keywords
    | QHasField(field, w) ->
        begin
          match field with
            Field_Album
          | Field_Title
          | Field_Artist
          | _ -> keywords := (String2.split_simplify w ' ') @ !keywords
        end
    | QHasMinVal (field, value) ->
        begin
          match field with
            Field_KNOWN "bitrate"
          | Field_Size
          | _ -> ()
        end
    | QHasMaxVal (field, value) ->
        begin
          match field with
            Field_KNOWN "bitrate"
          | Field_Size
          | _ -> ()
        end
    | QNone ->
        lprintf_nl "start_search: QNone in query";
        ()
  in
  iter query;
  !keywords

let gui_options_panels = ref ([] : (string * (string * string * string) list) list)

let register_gui_options_panel name panel =
  if not (List.mem_assoc name !gui_options_panels) then
    gui_options_panels := (name, panel) :: !gui_options_panels

let _ =
  add_infinite_timer filter_search_delay (fun _ ->
(*      if !!filter_search then *) begin
(*          lprintf "Filter search results\n"; *)
        List.iter (fun user ->
            List.iter  (fun s -> CommonSearch.Filter.find s)
            user.ui_user_searches;
        ) !ui_users
      end;
      CommonSearch.Filter.clear ();
  )

let search_add_result filter s r =
  if !CommonSearch.clean_local_search <> 0 then
    CommonSearch.Local.add r;
  if not filter (*!!filter_search*) then begin
(*      lprintf "Adding result to filter\n"; *)
      CommonSearch.search_add_result_in s r
    end
  else
    CommonSearch.Filter.add r

let main_options = ref ([] : (string * Arg.spec * string) list)

let add_main_options list =
  main_options := !main_options @ list


(*************************************************************

Every minute, sort the files by priority, and test if the
files with the highest priority are in FileDownloading state,
and the ones with lowest priority in FileQueued state, if there
is a max_concurrent_downloads constraint.

**************************************************************)

open CommonFile

type user_file_list = {
  file_list : file list;
  downloads_allowed : int option;
}

let force_download_quotas () =

  let queue_files files =
    List.iter (fun file ->
      if file_state file = FileDownloading then
        file_queue file
    ) files in

  let queue_user_file_list (_user, user_file_list) =
    queue_files user_file_list.file_list in

  if !all_temp_queued then
    queue_files !!CommonComplexOptions.files
  else

  (* create the assoc list of downloads of each user *)
    let files_by_user = List.fold_left (fun acc f ->
      let owner = CommonFile.file_owner f in
      try
        let owner_file_list = List.assoc owner acc in
        (owner, { owner_file_list with 
          file_list = f :: owner_file_list.file_list }) :: 
          List.remove_assoc owner acc
      with Not_found ->
        (owner, { 
          downloads_allowed = 
            (match owner.user_max_concurrent_downloads with
            | 0 -> None
            | i -> Some i);
          file_list = [f] }) :: acc
    ) [] !!CommonComplexOptions.files in

    (* sort each user's list separately *)
    let files_by_user = List.map (fun (owner, owner_file_list) ->
      owner, { owner_file_list with
        file_list = List.sort (fun f1 f2 ->
          let v = compare (file_priority f2) (file_priority f1) in
          if v <> 0 then v else
            (* [egs] do not start downloading a small file 
               against an already active download *)
            let d1 = file_downloaded f1 in
            let d2 = file_downloaded f2 in
            let active1 = d1 > 0L in
            let active2 = d2 > 0L in
            if not active1 && active2 then 1
            else if active1 && not active2 then -1
            else 
              (* Try to download in priority files with fewer bytes missing
                 Rationale: once completed, it may allow to recover some disk space *)
              let remaining1 = file_size f1 -- d1 in
              let remaining2 = file_size f2 -- d2 in
              compare remaining1 remaining2
        ) owner_file_list.file_list }
    ) files_by_user in

    (* sort the assoc list itself with user with highest quota first *)
    let files_by_user = 
      List.sort (fun (_owner1, { downloads_allowed = allowed1; file_list = _ }) 
                     (_owner2, { downloads_allowed = allowed2; file_list = _ }) ->
        match allowed1, allowed2 with
        | None, None -> 0
        | None, _ -> -1
        | _, None -> 1
        | Some allowed1, Some allowed2 -> compare allowed2 allowed1
      ) files_by_user in

    (* serve users round-robin, starting with the one with highest quota *)
    let rec iter downloads_left to_serve served =
      if downloads_left = 0 then begin
        List.iter queue_user_file_list to_serve;
        List.iter queue_user_file_list served
      end else
        match to_serve with
        | [] ->
            if served = [] then () (* nothing left to rotate *)
            else (* new round *)
              iter downloads_left served []
        | (_owner, { file_list = []; downloads_allowed = _ }) :: others ->
            (* user satisfied, remove from lists *)
            iter downloads_left others served
        | ((_owner, { downloads_allowed = Some 0; file_list = _ }) as first) :: others ->
            (* reached quota, remove from future rounds *)
            queue_user_file_list first;
            iter downloads_left others served
        | (owner, { file_list = first_file :: other_files; 
                    downloads_allowed = allowed }) :: others ->
            let is_downloading =
              match file_state first_file with
              | FileDownloading -> true
              | FileQueued ->
                  file_resume first_file (admin_user ());
                  true
              | _ -> false in
            if is_downloading then
              iter (downloads_left - 1) others 
                ((owner, { 
                  file_list = other_files;
                  downloads_allowed = match allowed with
                    | None -> None
                    | Some i -> Some (i - 1)
                }) :: served)
            else 
              iter downloads_left others
                ((owner, {
                  file_list = other_files;
                  downloads_allowed = allowed
                }) :: served) in
    iter !!max_concurrent_downloads files_by_user []

let _ =
  option_hook max_concurrent_downloads (fun _ ->
      ignore (force_download_quotas ())
  )

let run_porttest ?udp ~tcp result =
  result := PorttestInProgress (last_time ());
  let module H = Http_client in
  let url = Printf.sprintf "http://porttest.emule-project.net:81/ct_noframe.php?lang=&tcpport=%d" tcp in
  let url = match udp with
  | None -> url
  | Some udp -> url ^ Printf.sprintf "&udpport=%d" udp
  in
  let r = { H.basic_request with
    H.req_url = Url.of_string url;
    (* no sense to test ports via proxy! *)
(*       H.req_proxy = !CommonOptions.http_proxy; *)
    H.req_max_retry = 3;
    H.req_user_agent = get_user_agent () }
  in
  H.wget r begin fun file ->
    Unix2.tryopen_read file begin fun cin ->
      try
        while true do
          let line = input_line cin in
          try
            if Str.string_match (Str.regexp "^<P>Testing IP") line 0 then
              result := PorttestResult (last_time (), line)
          with _ -> ()
        done
      with End_of_file -> ()
    end
  end
