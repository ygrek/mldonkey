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
open Options

open BasicSocket
open TcpBufferedSocket

open CommonClient
open CommonTypes

open FileTPTypes
open FileTPOptions
open FileTPGlobals

open FileTPClients

(*************************************************************************)
(*                                                                       *)
(*                         MAIN                                          *)
(*                                                                       *)
(*************************************************************************)

let shell_command hostname =
  try
    match List.assoc hostname !!remote_shells with
      (cmd :: tail) as args ->
        cmd, Array.of_list args
    | _ -> raise Not_found
  with
  | _ ->
      "ssh",  [|  "ssh"; hostname |]

(*************************************************************************)
(*                                                                       *)
(*                         MAIN                                          *)
(*                                                                       *)
(*************************************************************************)

let segment_received c num s pos =
  if Bytes.length s > 0 then
    let d =
      match c.client_downloads with
        [] -> disconnect_client c Closed_by_user; raise Exit
      | d :: _ ->
          let file = d.download_file in
          if file_num file <> num || file_state file <> FileDownloading then begin
              disconnect_client c Closed_by_user;
              raise Exit;
            end;
          d
    in
    let file = d.download_file in
(*
  lprintf "CHUNK: %s\n"
          (String.escaped (String.sub b.buf b.pos to_read_int)); *)

    begin
      try
        match d.download_uploader with
          None -> assert false
        | Some up ->
                          
            let swarmer = CommonSwarming.uploader_swarmer up in

            let old_downloaded =
              CommonSwarming.downloaded swarmer in

            CommonSwarming.received up
              pos s 0 (Bytes.length s);

            let new_downloaded =
              CommonSwarming.downloaded swarmer in

            c.client_session_downloaded <- c.client_session_downloaded ++ (new_downloaded -- old_downloaded);
            c.client_total_downloaded <- c.client_total_downloaded ++ (new_downloaded -- old_downloaded);
            client_must_update (as_client c);

            if new_downloaded = file_size file then
              download_finished file;

      with e ->
          lprintf "FT: Exception %s in CommonSwarming.received\n"
            (Printexc2.to_string e)
    end;
    c.client_reconnect <- true;
(*          List.iter (fun (_,_,r) ->
              CommonSwarming.alloc_range r) d.download_ranges; *)
(*
lprintf "READ %Ld\n" (new_downloaded -- old_downloaded);
lprintf "READ: buf_used %d\n" to_read_int;
  *)

    (match d.download_ranges with
      | (xx,yy,r) :: tail when  pos >= xx && pos <= yy ->
          let (x,y) = CommonSwarming.range_range r in
          lprintf "Remaining: %Ld-%Ld/%Ld-%Ld\n"
            x y xx yy;
          if x = y then begin
              d.download_ranges <- tail;
              (match c.client_sock with
                  Connection sock ->
                    for i = 1 to max_queued_ranges do
                      if List.length d.download_ranges <= max_queued_ranges then
                        (try get_from_client sock c with _ -> ());
                    done;
                | _ -> ());
(* If we have no more range to receive, disconnect *)
              lprintf "\n ********** RANGE DOWNLOADED  ********** \n";
            end
      | (xx,yy,r) :: tail ->
          let (x,y) = CommonSwarming.range_range r in
          lprintf "Bad range (%Ld): %Ld-%Ld/%Ld-%Ld\n"  pos
            x y xx yy;
      | [] ->
          lprintf "***** outside of ranges !!!\n"
    )

(*************************************************************************)
(*                                                                       *)
(*                         MAIN                                          *)
(*                                                                       *)
(*************************************************************************)

let ssh_send_range_request c (x,y) sock d =
  let file = d.download_url.Url.full_file in
  TcpBufferedSocket.write_string sock
    (Printf.sprintf "%s %s %Ld %Ld %d '.%s'\n"  !!get_range !!range_arg x y
    (file_num d.download_file) file);
  ()

(*************************************************************************)
(*                                                                       *)
(*                         MAIN                                          *)
(*                                                                       *)
(*************************************************************************)

let ssh_set_sock_handler c sock =
  ()

(*************************************************************************)
(*                                                                       *)
(*                         MAIN                                          *)
(*                                                                       *)
(*************************************************************************)

let ssh_check_size file url start_download_file =
  let token = create_token unlimited_connection_manager in
  let shell, args = shell_command url.Url.server in
  lprintf "SHELL: ";

  Array.iter (fun s -> lprintf " %s" s) args;
  lprintf "\n";
  let sock, pid = exec_command token  shell args (fun _ _ -> ());
  in

  set_rtimer sock (fun _ ->
      lprintf "SSH TIMER\n";
      (try Unix.kill Sys.sigkill pid with _ -> ());
      TcpBufferedSocket.close sock Closed_for_timeout);
  set_closer sock (fun _ s ->
      lprintf "SSH closed\n";
      (try Unix.kill Sys.sigkill pid with _ -> ());
      close sock s);
  TcpBufferedSocket.set_reader sock (fun sock nread ->
      let b = TcpBufferedSocket.buf sock in
      lprintf "SSH reader %d [%s]\n" nread (Bytes.unsafe_to_string (Bytes.escaped (Bytes.sub b.buf b.pos b.len)));
      let rec iter i =
        if i < b.len then
          if (Bytes.get b.buf (b.pos + i)) = '\n' then begin
              let slen = if i > 0 && (Bytes.get b.buf (b.pos + i - 1)) = '\r' then
                  i - 1
                else i in
              let line = String.sub (Bytes.to_string b.buf) b.pos slen in
              lprintf "SSH LINE [%s]\n" line;
              buf_used b (i+1);
              if String2.starts_with line "[SIZE " then begin
                  let pos = String.index line ']' in
                  let size = String.sub line 6 (pos-6) in
                  lprintf "Size [%s]\n" size;
                  let result_size = Int64.of_string size in
                  start_download_file result_size;
                  (try Unix.kill Sys.sigkill pid with _ ->());
                end else
                iter 0
            end else
            iter (i+1)
      in
      iter 0
  );
  let command =    (Printf.sprintf "%s size '.%s' ; exit\n" !!get_range
      url.Url.full_file) in
  lprintf "SSH send [%s]\n" command;
  TcpBufferedSocket.write_string sock command;
  set_rtimeout sock 15.

(*************************************************************************)
(*                                                                       *)
(*                         MAIN                                          *)
(*                                                                       *)
(*************************************************************************)

type segment =
  Nothing
| SegmentPos of int * int64 * int * int
| SegmentXPos of int * int64 * int * int
| Segment of int * int64 * int * int * string
| SegmentX of int * int64 * int * int * string

let ssh_connect token c f =
  let shell, args = shell_command c.client_hostname in
  let sock, pid = exec_command token  shell args (fun _ _ -> ());
  in
  set_rtimer sock (fun _ ->
      (try Unix.kill Sys.sigkill pid with _ -> ());
      disconnect_client c Closed_for_timeout);
  set_closer sock (fun _ s ->
      (try Unix.kill Sys.sigkill pid with _ -> ());
      disconnect_client c s);
  let segment = ref Nothing in
  TcpBufferedSocket.set_reader sock (fun sock nread ->
      let b = TcpBufferedSocket.buf sock in
      let rec iter () =

        match !segment with
          SegmentXPos (file_num, pos, len, elen) ->

            if b.len >= elen then begin
                segment := SegmentX (file_num, pos, len, elen,
                  String.sub (Bytes.to_string b.buf) b.pos elen);
                buf_used b elen;
                iter0 0
              end

        | _ ->
            iter0 0

      and iter0 i =
        if i < b.len then
          if (Bytes.get b.buf (b.pos + i)) = '\n' then begin
              let slen = if i > 0 && (Bytes.get b.buf (b.pos + i - 1)) = '\r' then
                  i - 1
                else i in
              let line = String.sub (Bytes.to_string b.buf) b.pos slen in
(*              lprintf "SSH LINE [%s]\n" line; *)
              buf_used b (i+1);

              if String2.starts_with line "[SEGMENT " && !segment = Nothing then
                let line = String.sub line 9 (String.length line - 10) in
                lprintf "segmented [%s]\n" line;
                match String2.split_simplify line ' '  with
                | [file_num; pos; len; elen] ->
                    let file_num = int_of_string file_num in
                    let pos = Int64.of_string pos in
                    let len = int_of_string len in
                    let elen = int_of_string elen in
                    segment := SegmentPos (file_num, pos,len,elen);
                    lprintf "Waiting for segment...\n";
                    iter0 0

                | ["8bits"; file_num; pos; len; elen] ->
                    let file_num = int_of_string file_num in
                    let pos = Int64.of_string pos in
                    let len = int_of_string len in
                    let elen = int_of_string elen in
                    segment := SegmentXPos (file_num, pos,len,elen);
                    lprintf "Waiting for segment X...\n";
                    iter ()

                | _ ->
                    lprintf "SSH: unexpected segment\n";
                    disconnect_client c (Closed_for_error "unexpected reply")
              else
              if line  = "[/SEGMENT]" then
                match !segment with
                | Segment (file_num, pos, len, elen, s) ->
                    lprintf "******* SEGMENT RECEIVED *******\n";
(*
                    lprintf "Received/expected: %d/%d\n" (String.length s) elen;
*)
                    let ss = Base64.decode s in
(*
                    lprintf "Decoded/expected: %d/%d\n" (String.length ss) len;
*)
                    segment_received c file_num ss pos;
                    segment := Nothing;
                    iter0 0

                | SegmentX (file_num, pos, len, elen, ss) ->
                    lprintf "******* SEGMENT RECEIVED *******\n";
                    segment_received c file_num (Bytes.of_string ss) pos;
                    segment := Nothing;
                    iter0 0
                | _ ->
                    lprintf "SSH: unexpected end of segment\n";
                    disconnect_client c (Closed_for_error "unexpected reply")

              else
              match !segment with
                Nothing -> iter0 0
              | SegmentPos (file_num, pos, len, elen) ->
                  segment := Segment (file_num, pos, len, elen, line);
                  lprintf "++++++++++++ SEGEMNT RECEIVED ++++++++++\n";
                  iter0 0
              | _ -> begin
                    lprintf "SSH: unexpected line\n";
                    disconnect_client c (Closed_for_error "unexpected reply")
                  end
            end else
            iter0 (i+1)

      in
      iter ()
  );
  f sock;
  sock

let proto =
  {
    proto_send_range_request = ssh_send_range_request;
    proto_set_sock_handler = ssh_set_sock_handler;
    proto_check_size = ssh_check_size;
    proto_string = "ssh";
    proto_connect = ssh_connect;
  }
