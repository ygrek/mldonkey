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

open CommonOptions
open TcpBufferedSocket
open BasicSocket
open CommonGlobals
open CommonSearch
open Options
open CommonNetwork
open CommonResult
open CommonServer
open CommonTypes

let file_kinds = ref []

let add_web_kind kind f =
  file_kinds := (kind,f) :: !file_kinds
  
let load_url kind url =
  Printf.printf "QUERY URL %s" url; print_newline ();
  let filename = Filename.temp_file "http_" ".tmp" in
  let file_oc = open_out filename in
  let file_size = ref 0 in

  Http_client.get_page (Url.of_string url) []
    (fun maxlen headers sock nread ->
        let buf = TcpBufferedSocket.buf sock in
        
        if nread > 0 then begin
            let left = 
              if maxlen >= 0 then
                mini (maxlen - !file_size) nread
              else nread
            in
            output file_oc buf.buf buf.pos left;
            buf_used sock left;
            file_size := !file_size + left;
            if nread > left then
              TcpBufferedSocket.close sock "end read"
          end
        else
        if nread = 0 then begin
            close_out file_oc;
            try
              let f = 
                try
                  List.assoc kind !file_kinds 
                with
                  _ -> failwith (Printf.sprintf "Unknown kind [%s]" kind)
              in
              (f filename : unit);
(*              Sys.remove filename *)
            with e ->
                Printf.printf
                  "Exception %s in loading downloaded file %s"
                  (Printexc.to_string e) filename
                
          end
    )

let days = ref 0      
let hours = ref 0    

let load_web_infos () =
  List.iter (fun (kind, period, url) ->
      if !days mod period = 0 then load_url kind url
  ) !!CommonOptions.web_infos

let display_vd = ref false
  
let last_search = ref None
let last_results = ref []
  
let print_results o =
  let buf = o.conn_buf in
  match !CommonSearch.searches with
    [] -> Printf.bprintf buf "No search to print\n"
  | q :: _ ->
      last_search := Some q;
      last_results := [];
      let counter = ref 1 in
      Intmap.iter (fun r_num (count,r) ->
          CommonResult.result_print r !counter o;
          last_results := (!count, r) :: !last_results;
          incr counter;
      ) q.search_results

      
let download_file buf arg =
  Printf.bprintf buf "%s\n" (
    try
      match !last_search with
        None -> "no last search"
      | Some s ->
          let result = List.assoc (int_of_string arg) !last_results  in
          CommonResult.result_download result [] false; 
          "download started"
    with
    | Failure s -> s
    | _ -> "could not start download"
  )
  
let start_search query buf =
  let s = CommonSearch.new_search query in
  networks_iter (fun r -> r.op_network_search s buf);
  s
  
let print_connected_servers o =
  let buf = o.conn_buf in
  networks_iter (fun r ->
      try
      let list = network_connected_servers r in
      Printf.bprintf buf "--- Connected to %d servers on the %s network ---\n"
        (List.length list) r.network_name;
      List.iter (fun s ->
          server_print s o;
      ) list;
      with e ->
          Printf.bprintf  buf "Exception %s in print_connected_servers"
            (Printexc.to_string e);
          print_newline ();
  )
  
let send_custom_query buf s args = 
  let query = s.GuiTypes.search_query in
  try
    let q = List.assoc query !!CommonComplexOptions.customized_queries in
    let args = ref args in
    let get_arg arg_name = 
(*      Printf.printf "Getting %s" arg_name; print_newline (); *)
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
      | Q_KEYWORDS _ -> 
          let value = get_arg "keywords" in
          want_and_not andnot (fun w -> QHasWord w) value
          
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
          let minsize = Int32.of_string minsize in
          let unit = Int32.of_string unit in
          QHasMinVal ("size", Int32.mul minsize unit)

      | Q_MAXSIZE _ ->
          let maxsize = get_arg "maxsize" in
          let unit = get_arg "maxsize_unit" in
          if maxsize = "" then raise Not_found;
          let maxsize = Int32.of_string maxsize in
          let unit = Int32.of_string unit in
          QHasMaxVal ("size", Int32.mul maxsize unit)

      | Q_FORMAT _ ->
          let format = get_arg "format" in
          let format_propose = get_arg "format_propose" in
          let format = if format = "" then 
              if format_propose = "" then raise Not_found
              else format_propose
            else format in
          want_comb_not andnot
            or_comb
            (fun w -> QHasField("format", w)) format
          
      | Q_MEDIA _ ->
          let media = get_arg "media" in
          let media_propose = get_arg "media_propose" in
          let media = if media = "" then 
              if media_propose = "" then raise Not_found
              else media_propose
            else media in
          QHasField("type", media)

      | Q_MP3_ARTIST _ ->
          let artist = get_arg "artist" in
          if artist = "" then raise Not_found;
          want_comb_not andnot and_comb 
            (fun w -> QHasField("Artist", w)) artist
          
      | Q_MP3_TITLE _ ->
          let title = get_arg "title" in
          if title = "" then raise Not_found;
          want_comb_not andnot and_comb 
            (fun w -> QHasField("Title", w)) title
          
      | Q_MP3_ALBUM _ ->
          let album = get_arg "album" in
          if album = "" then raise Not_found;
          want_comb_not andnot and_comb 
            (fun w -> QHasField("Album", w)) album
          
      | Q_MP3_BITRATE _ ->
          let bitrate = get_arg "bitrate" in
          if bitrate = "" then raise Not_found;
          QHasMinVal("bitrate", Int32.of_string bitrate)

    in
    try
      let request = iter q in
      Printf.bprintf buf "Sending query !!!";
      ignore (start_search {s with GuiTypes.search_query = request } buf)
    with
      Not_found ->
        Printf.bprintf buf "Void query %s" query        
  with 
    Not_found ->
      Printf.bprintf buf "No such custom search %s" query
  | Exit -> ()
  | e -> 
      Printf.bprintf buf "Error %s while parsing request"
        (Printexc.to_string e)

let all_simple_options () =
  let options = ref (simple_options downloads_ini) in
  networks_iter_all (fun r ->
      match r.network_config_file with
        None -> ()
      | Some opfile ->
          let args = simple_options opfile in
          let prefix = !!(r.network_prefix) in
          let args = 
            if prefix = "" then args else 
              List2.tail_map (fun (arg, value) ->
                (Printf.sprintf "%s%s" prefix arg, value)) 
            args
          in
          options := !options @ args
  );
  !options

let set_fully_qualified_options name value =
  Printf.printf "For option %s" name; print_newline ();
  let rec iter prefix opfile =
    let args = simple_options opfile in
    List.iter (fun (old_name, old_value) ->
        let new_name = Printf.sprintf "%s%s" prefix old_name in
        if new_name = name then
          (set_simple_option opfile old_name value; raise Exit))
    args
  in
  try
    iter "" downloads_ini;
    if not (networks_iter_all_until_true (fun r ->
            try
              match r.network_config_file with
                None -> false
              | Some opfile ->
                  let prefix = r.network_prefix in
                  iter (if !!prefix = "" then "" else !!prefix^"-") opfile;
                  false
            with Exit -> true
        )) then begin
        Printf.printf "Could not set option %s" name; print_newline ();
      end
  with Exit -> ()
      
let keywords_of_query query =
  let keywords = ref [] in
  
  let rec iter q = 
    match q with
    | QOr (q1,q2) 
    | QAnd (q1, q2) -> iter q1; iter q2
    | QAndNot (q1,q2) -> iter q1 
    | QHasWord w -> keywords := String2.split_simplify w ' '
    | QHasField(field, w) ->
        begin
          match field with
            "Album"
          | "Title"
          | "Artist"
          | _ -> keywords := String2.split_simplify w ' '
        end
    | QHasMinVal (field, value) ->
        begin
          match field with
            "bitrate"
          | "size"
          | _ -> ()
        end
    | QHasMaxVal (field, value) ->
        begin
          match field with
            "bitrate"
          | "size"
          | _ -> ()
        end
    | QNone ->
        prerr_endline "LimewireInteractive.start_search: QNone in query";
        ()
  in
  iter query;
  !keywords

let gui_options_panels = ref ([] : (string * (string * string * string) list) list)
  
let register_gui_options_panel name panel =
  if not (List.mem_assoc name !gui_options_panels) then
    gui_options_panels := (name, panel) :: !gui_options_panels
    
    
    