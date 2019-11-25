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

open CommonGlobals
open CommonTypes

open DonkeyProtoCom
open DonkeyTypes
open DonkeyGlobals

module P = GuiProto

let local_search search =
  ()
  (*
  if !!local_index_find_cmd <> "" then
    try
      let (t_in, t_out) = exec_command !!local_index_find_cmd [||]
          (fun sock ev -> ()) in
      let lines = ref [] in
      set_reader t_in (fun t_in nread ->
          let buf = TcpBufferedSocket.buf t_in in
          let s = buf.buf in
          let rec iter () =
            let pos = buf.pos in
            let len = buf.len in
            try
              let pos2 = String.index_from s pos '\n' in
              let line = String.sub s pos (pos2 - pos) in
              buf_used t_in (pos2 - pos + 1);
              if line = "end result" then
                let l = List.rev !lines in
                lines := [];

                try
                  let r = {
                      result_names = [];
                      result_md4 = Md4.null;
                      result_size = Int32.zero;
                      result_format = "";
                      result_type = "";
                      result_tags = [];
                      result_comment = None;
                      result_done = false;
                    } in
                  List.iter (fun (name, value) ->
                      match name with
                        "name" -> r.result_names <- value :: r.result_names
                      | "md4" -> r.result_md4 <- Md4.of_string value
                      | "size" -> r.result_size <- Int32.of_string value
                      | "format" -> r.result_format <- value
                      | "type" -> r.result_type <- value
                      | "string_tag" ->
                          let name, v = String2.cut_at value ':' in
                          r.result_tags <- {
                            tag_name = name;
                            tag_value = String v;
                          } :: r.result_tags
                      | "int_tag" ->
                          let name, v = String2.cut_at value ':' in
                          r.result_tags <- {
                            tag_name = name;
                            tag_value = Uint32 (Int32.of_string v);
                          } :: r.result_tags
                      | _ ->
                          lprintf "discarding result line %s:%s" name value;
                          lprint_newline ();
                  ) l;
                  if r.result_md4 = Md4.null || r.result_size = Int32.zero then
                    failwith "Not enough information in result";
                  let doc = DonkeyIndexer.index_result r in
                  add_to_search search r doc

                with e ->
                    lprintf "result discarded for exn %s"
                      (Printexc2.to_string e); lprint_newline ()
              else begin
                  try
                    let pos = String.index line ':' in
                    let name = String.sub line 0 pos in
                    let value = String.sub line (pos+1)
                      (String.length line - pos - 1)
                    in
                    lines := (name, value) :: !lines
                  with e ->
                      lprintf "Discarding line %s" line; lprint_newline ();
                end;
              iter ()
            with _ -> ()
          in
          iter ()
      );
      failwith "Translate query to OUT not implemented"
(*
    let buf = Buffer.create 100 in
    let q = search.search_query in
    if q.search_words <> [] then
      Printf.bprintf buf "words:%s\n" (String2.unsplit q.search_words ' ');
    (match q.search_minsize with None -> () | Some size ->
          Printf.bprintf buf "minsize:%s\n" (Int32.to_string size));
    (match q.search_maxsize with None -> () | Some size ->
          Printf.bprintf buf "maxsize:%s\n" (Int32.to_string size));
    (match q.search_min_bitrate with None -> () | Some size ->
          Printf.bprintf buf "minrate:%s\n" (Int32.to_string size));
    (match q.search_media with None -> () | Some s ->
          Printf.bprintf buf "media:%s\n" s);
    (match q.search_format with None -> () | Some s ->
          Printf.bprintf buf "format:%s\n" s);
    (match q.search_title with None -> () | Some s ->
          Printf.bprintf buf "title:%s\n" s);
    (match q.search_album with None -> () | Some s ->
          Printf.bprintf buf "album:%s\n" s);
    (match q.search_artist with None -> () | Some s ->
          Printf.bprintf buf "artist:%s\n" s);
    Buffer.add_string buf "end query\n";
TcpBufferedSocket.write_string t_out (Buffer.contents buf)
  *)
    with e ->
        lprintf "Exception %s while starting local_index_find\n"
          (Printexc2.to_string e)

*)

let send_search search query =
  List.iter (fun s ->
      do_if_connected s.server_sock (fun sock ->
          let module M = DonkeyProtoServer in
          let module Q = M.Query in
          server_send sock (M.QueryReq query);
          Fifo.put s.server_search_queries search
      )
  ) (connected_servers());
  DonkeyUdp.make_xs search;
  local_search search

let send_subscribe search query =
  xs_last_search := search.search_num;
  let module M = DonkeyProtoServer in
  let module Q = M.Query in
  List.iter (fun s ->
      do_if_connected  s.server_sock (fun sock ->
(*          if s.server_mldonkey then
            server_send sock (
              M.Mldonkey_SubscribeReq (search.search_num, 3600, query))
          else *) begin
              server_send sock (M.QueryReq query);
              Fifo.put s.server_search_queries search
            end
      )
  ) (connected_servers());
  DonkeyUdp.make_xs search;
  local_search search

let new_search search =
  search.search_waiting <- search.search_waiting +
  List.length (connected_servers());
  search

let _ =
  network.op_network_search <- (fun ss buf ->
      let search = new_search ss in
      let query = search.search_query in
      match ss.search_type with
        RemoteSearch ->
          send_search search query;
          Printf.bprintf buf "Query %d sent to %d server(s)\n"
            ss.search_num (List.length (connected_servers()))
      | LocalSearch -> ()
      | SubscribeSearch ->
          send_subscribe search query;
          Printf.bprintf buf "Query %d sent to %d server(s)\n"
            search.search_num (List.length (connected_servers()))
  )
