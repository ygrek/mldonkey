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
open CommonTypes
open GuiTypes
open GuiProto
open TcpBufferedSocket

let buf = Buffer.create 1000

let escape buf s =
  for i = 0 to String.length s - 1 do
    (match s.[i] with
      '(' | ')' | '{' | '}' | ';' | '\\' ->
        Buffer.add_char buf '\\'
      | _ -> ());
    Buffer.add_char buf s.[i]
  done
  
let rec encode buf (GiftCommand(command, opt_arg, args)) =
  Buffer.add_string buf command;
  begin
    match opt_arg with
      None -> ()
    | Some arg ->
        Buffer.add_char buf '(';
        escape buf arg;
        Buffer.add_char buf ')'
  end;
  if args <> [] then Buffer.add_char buf '\n';
  List.iter (fun arg ->
      encode_key buf arg
  ) args;
  Buffer.add_string buf ";\n"

and encode_key buf (GiftCommand(command, opt_arg, args)) =
  Buffer.add_string buf command;
  begin
    match opt_arg with
      None -> ()
    | Some arg ->
        Buffer.add_char buf '(';
        escape buf arg;
        Buffer.add_char buf ')'
  end;
  if args <> [] then begin
      Buffer.add_string buf "{\n";
      List.iter (fun arg ->
          encode_key buf arg
      ) args;
      Buffer.add_string buf "}"
    end;
  Buffer.add_string buf "\n"
  

let file_id gui file_num =
  try
    string_of_int (Hashtbl.find gui.gui_identifiers (0, file_num)),
    "CHGDOWNLOAD"
  with _ ->
      let id = gui.gui_id_counter + 1 in
      gui.gui_id_counter <- id;
      Hashtbl.add gui.gui_identifiers (0, file_num) id;
      Hashtbl.add gui.gui_identifiers_rev id (0, file_num);
      string_of_int id, "ADDDOWNLOAD"
  
let translate gui t =
  match t with
    GiftServerAttach (server, version) ->
      GiftCommand("ATTACH", Some "", 
        [
          GiftCommand("server", Some server, []);
          GiftCommand("version", Some version, []);
        ])
  | GiftServerStats list ->
      GiftCommand("STATS", None, 
        List.map (fun (protocol, users, files, size) ->
            GiftCommand(protocol, None, [
                GiftCommand("files", Some files, []);
                GiftCommand("size", Some size, []);
                GiftCommand("users", Some users, []);
              ]
            )) list)
      
  | Search_result (search_num, id, Some r) ->
      let num = r.result_num in
      GiftCommand("ITEM", Some (string_of_int search_num), [
          GiftCommand("user", Some "no user", []);
          GiftCommand("node", Some "no server", []);
          GiftCommand("availability", Some "1", []);
          GiftCommand("size", Some (Int64.to_string r.result_size), []);
          GiftCommand("url", Some (string_of_int num), []);
          GiftCommand("file", Some (match r.result_names with
                [] -> "no name"
              | name :: _ -> name), []);
          GiftCommand("mime", Some "application/octect-stream", []);
          GiftCommand("hash", Some (string_of_int num), []);
        ])

  | Search_waiting (search_num, 0) ->
      GiftCommand("ITEM", Some (string_of_int search_num), [])

  | File_info file ->
      let file_id, command = file_id gui file.file_num in
      GiftCommand(command, Some file_id, [
          GiftCommand("hash", Some file_id, []);
          GiftCommand("state", Some (match file.file_state with
                FilePaused | FileQueued -> "Paused"
              | FileDownloaded -> "Completed"
              | _ -> "Active"
            ), []);
          GiftCommand("transmit", Some (Int64.to_string file.file_downloaded), []);
          GiftCommand("size", Some (Int64.to_string file.file_size), []);
          GiftCommand("file", Some (match file.file_names with
                [] -> "no name"
              | name :: _ -> name), []);
          GiftCommand("shared", Some "1", []);
          
          GiftCommand("throughput", 
            Some (string_of_int (int_of_float (file.file_download_rate *. 1000.))) , []);
          GiftCommand("elapsed", Some "10000", []);
          
        ])
        (*
     SOURCE* { 
        user (username)
        url (url)
        statusgrl (status
        status (protocol status)
        start (chunk start) 
        transmit (chunk transmit)
        total (chunk total)
     }
  ;
*)          
      
  | _ -> raise UnsupportedGuiMessage
      
let gui_send gui sock t = 
  try
    Buffer.reset buf;
    encode buf (translate gui t);
    let s = Buffer.contents buf in
    lprintf "Sending to GIFT GUI: \n%s\n" s;
    write_string sock s;
  with UnsupportedGuiMessage -> ()
