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


open Options
open Mftp
open Mftp_comm
open DownloadServers
open BasicSocket
open TcpClientSocket
open DownloadOneFile
open DownloadFiles
open DownloadComplexOptions
open DownloadTypes
open DownloadOptions
open DownloadGlobals
open DownloadClient
open Gui_types
 
let search_of_args args =

  let rec iter args q =
    match args with
      [] -> q
    | "-minsize" :: minsize :: args ->
        let minsize = Int32.of_string minsize in
        iter args ((QHasMinVal("size", minsize)) :: q)
    | "-maxsize"  :: maxsize :: args ->
        let maxsize = Int32.of_string maxsize in
        iter args  ((QHasMaxVal("size", maxsize)) :: q)
    | "-avail"  :: avail :: args ->
        let avail = Int32.of_string avail in
        iter args ((QHasMinVal("avail", avail)) :: q)
    | "-media"  :: filetype :: args ->
        iter args ((QHasField("type", filetype)) :: q)
    | "-Video"  :: args ->
        iter args ((QHasField("type", "Video")) :: q)
    | "-Audio"  :: filetype :: args ->
        iter args ((QHasField("type", "Audio")) :: q)
    | "-format"  :: format :: args ->
        iter args ((QHasField("format", format)) :: q)
    | "-artist"  :: format :: args ->
        iter args ((QHasField("Artist", format)) :: q)
    | "-title"  :: format :: args ->
        iter args ((QHasField("Title", format)) :: q)
    | "-album"  :: format :: args ->
        iter args ((QHasField("Album", format)) :: q)
    | "-field"  :: field :: format :: args ->
        iter args ((QHasField(field, format)) :: q)
    | s :: args ->
        iter args ((QHasWord(s)) :: q)
  in
  let q = iter args [] in
  match q with 
    [] -> failwith "Void query"
  | q1 :: tail ->
      List.fold_left (fun q1 q2 ->
            QAnd (q1,q2)
        ) q1 tail
   

    
let search_string q =
  let rec iter q =
    match q with
      QAnd (q1, q2) -> Printf.sprintf "(%s) AND (%s)" (iter q1) (iter q2)
    | QOr (q1, q2) ->  Printf.sprintf "(%s) OR (%s)" (iter q1) (iter q2)
    | QAndNot (q1, q2) ->  Printf.sprintf "(%s) AND NOT (%s)" (iter q1) (iter q2)
    | QHasWord s -> Printf.sprintf "CONTAINS[%s]" s
    | QHasField (f,s) -> Printf.sprintf "[%s]CONTAINS[%s]" f s
    | QHasMinVal (f,v) -> Printf.sprintf "[%s]>%s" f (Int32.to_string v)
    | QHasMaxVal (f,v) -> Printf.sprintf "[%s]<%s" f (Int32.to_string v)
  
  in
  iter q

let local_search search =
  ()
  (*
  if !!local_index_find_cmd <> "" then
    try
      let (t_in, t_out) = exec_command !!local_index_find_cmd [||] 
          (fun sock ev -> ()) in
      let lines = ref [] in
      set_reader t_in (fun t_in nread ->
          let buf = TcpClientSocket.buf t_in in
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
                          Printf.printf "discarding result line %s:%s" name value;
                          print_newline ();
                  ) l;
                  if r.result_md4 = Md4.null || r.result_size = Int32.zero then
                    failwith "Not enough information in result";
                  let doc = DownloadIndexer.index_result r in
                  add_to_search search r doc
                
                with e -> 
                    Printf.printf "result discarded for exn %s" 
                      (Printexc.to_string e); print_newline ()
              else begin
                  try
                    let pos = String.index line ':' in
                    let name = String.sub line 0 pos in
                    let value = String.sub line (pos+1) 
                      (String.length line - pos - 1)
                    in
                    lines := (name, value) :: !lines
                  with e ->
                      Printf.printf "Discarding line %s" line; print_newline ();
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
TcpClientSocket.write_string t_out (Buffer.contents buf)
  *)
    with e ->
        Printf.printf "Exception %s while starting local_index_find"
          (Printexc.to_string e); print_newline ()

*)
  
let send_search search query =
  last_xs := search.search_num;
  List.iter (fun s ->
      match s.server_sock with
        None -> ()
      | Some sock ->
          let module M = Mftp_server in
          let module Q = M.Query in
          server_send sock (M.QueryReq query);
          let nhits = ref 0 in
          let rec handler s _ t =
            let nres = List.length t in
            nhits := !nhits + nres;
            if !last_xs = search.search_num && nres = 201 &&
              !nhits < search.search_max_hits then
              begin
                match s.server_sock with
                  None -> ()
                | Some sock ->
                    server_send sock M.QueryMoreResultsReq;
                    Fifo.put s.server_search_queries handler      
              end;
            search_handler search t
          in
          Fifo.put s.server_search_queries handler
  ) !connected_server_list;
  make_xs search;
  local_search search        

  
let new_search query =
  incr search_counter;
  {
    search_max_hits = 200;
    search_query = query;
    search_files = Hashtbl.create 127;
    search_num = !search_counter;
    search_nresults = 0;
    search_waiting = List.length !connected_server_list;
    search_string = search_string query;
    search_handler = (fun _ -> ());
    search_xs_servers = !!known_servers;
  }

let start_search query buf =

  let search = new_search query in
  let query = search.search_query in
  searches := search :: !searches;  
  send_search search query;
  Printf.bprintf buf "Query %d Sent to %d\n"
    search.search_num (List.length !connected_server_list)  

  
type englob_op = IN_NOOP | IN_AND | IN_OR
  
let custom_query buf query =
  try
    let q = List.assoc query !!customized_queries in
    Printf.bprintf buf "
    <center>
    <h2> Custom Search %s </h2>
    </center>

<form action=/submit>
<input type=hidden name=custom value=\"%s\">
<input type=submit value=Search>\n" query query;
    
    let rec iter q in_op =
      match q with
      
      | Q_AND list ->
          if in_op <> IN_AND then begin
              Buffer.add_string buf "<table border=1>";
              Buffer.add_string buf "<tr>";
              Buffer.add_string buf "<td>";
              Buffer.add_string buf "AND";
              Buffer.add_string buf "</td>";
              Buffer.add_string buf "<td>";
              Buffer.add_string buf "<table border=1>";
              List.iter (fun q ->
                  Buffer.add_string buf "<tr><td>";
                  iter q IN_AND;
                  Buffer.add_string buf "</td></tr>";
              ) list;
              Buffer.add_string buf "</table>";
              Buffer.add_string buf "</td>";
              Buffer.add_string buf "</tr>";
              Buffer.add_string buf "</table>";
            end else begin
              Buffer.add_string buf "<table border=0>";
              List.iter (fun q ->
                  Buffer.add_string buf "<tr><td>";
                  iter q IN_AND;
                  Buffer.add_string buf "</td></tr>";
              ) list;
              Buffer.add_string buf "</table>";              
            end
      
      | Q_OR list ->
          if in_op <> IN_OR then begin
              Buffer.add_string buf "<table border=1>";
              Buffer.add_string buf "<tr>";
              Buffer.add_string buf "<td>";
              Buffer.add_string buf "OR";
              Buffer.add_string buf "</td>";
              Buffer.add_string buf "<td>";
              Buffer.add_string buf "<table border=1>";
              List.iter (fun q ->
                  Buffer.add_string buf "<tr><td>";
                  iter q IN_OR;
                  Buffer.add_string buf "</td></tr>";
              ) list;
              Buffer.add_string buf "</table>";
              Buffer.add_string buf "</td>";
              Buffer.add_string buf "</tr>";
              Buffer.add_string buf "</table>";
            end else begin
              Buffer.add_string buf "<table border=0>";
              List.iter (fun q ->
                  Buffer.add_string buf "<tr><td>";
                  iter q IN_OR;
                  Buffer.add_string buf "</td></tr>";
              ) list;
              Buffer.add_string buf "</table>";              
            end
            
      | Q_ANDNOT (q1,q2) ->
          Buffer.add_string buf "<table border=1>";
          Buffer.add_string buf "<tr>";
          Buffer.add_string buf "<td>";
          iter q1 IN_AND;
          Buffer.add_string buf "</td>";
          Buffer.add_string buf "</tr>";

          Buffer.add_string buf "<tr>";
          Buffer.add_string buf "<table border=1>";
          Buffer.add_string buf "<tr>";
          Buffer.add_string buf "<td>";
          Buffer.add_string buf "AND NOT";
          Buffer.add_string buf "</td>";
          Buffer.add_string buf "<td>";
          iter q2 IN_AND;
          Buffer.add_string buf "</td>";
          Buffer.add_string buf "</tr>";
          Buffer.add_string buf "</table>";
          Buffer.add_string buf "</tr>";

      | Q_KEYWORDS (label, default) ->
          Printf.bprintf buf "
<table border=0>
          <td>%s </td> <td width=\"1%%\"><input type=text name=keywords size=40 value=\"%s\"></td>
</table>" label default
          
      | Q_MINSIZE (label, default) ->
          Printf.bprintf  buf "
<table border=0>
<td> %s </td> 

<td> 
<input type=text name=minsize size=40 value=\"%s\">
</td>

<td> 
<select name=minsize_unit>
<option value=1048576> MBytes </option>
<option value=1024> kBytes </option>
<option value=1> Bytes </option>
</select>
</td>
</table>
          " label (if default = "" then "" else
            try
              let size = Int32.of_string default in
              let size = Int32.div size (Int32.of_int 1048576) in
              Int32.to_string size
            with _ -> "")

      | Q_MAXSIZE (label, default) ->
          Printf.bprintf  buf "
<table border=0>
<td> %s </td> 

<td> 
<input type=text name=maxsize size=40 value=\"%s\">
</td>

<td> 
<select name=maxsize_unit>
<option value=1048576> MBytes </option>
<option value=1024> kBytes </option>
<option value=1> Bytes </option>
</select>
</td>
</table>
          " label (if default = "" then "" else
            try
              let size = Int32.of_string default in
              let size = Int32.div size (Int32.of_int 1048576) in
              Int32.to_string size
            with _ -> "")
          
      | Q_MP3_BITRATE (label, default) -> 
          Printf.bprintf buf "
<table border=0>

<tr>
<td>
%s
</td>


<td> 
<select name=bitrate>
<option value=\"%s\"> --- </option>
<option value=64> 64 </option>
<option value=96> 96 </option>
<option value=128> 128 </option>
<option value=160> 160 </option>
<option value=192> 192 </option>
</select>
</td>

</tr>

</table>
          " label default
      | Q_MP3_ALBUM (label, default) -> 
          Printf.bprintf buf "

<table border=0>

<tr>
<td> %s </td> 
<td> 
<input type=text name=album size=40 value=\"%s\">
</td>
</tr>
</table>

          " label default
      | Q_MP3_TITLE (label, default) -> 
          Printf.bprintf buf "
 
<table border=0>

<tr>
<td> %s </td> 
<td> 
<input type=text name=title size=40 value=\"%s\">
</td>
</tr>
</table>

         " label default
      | Q_MP3_ARTIST (label, default) -> 
          Printf.bprintf buf "

<table border=0>

<tr>
<td> %s </td> 
<td> 
<input type=text name=artist size=40 value=\"%s\">
</td>
</tr>
</table>

          " label default
          
      | Q_MEDIA (label, default) -> 

          Printf.bprintf buf "
<table border=0>
          
<tr>
<td> %s </td> 

<td> 
<input type=text name=media size=40 value=\"%s\">
</td>

<td> 
<select name=media_propose>
<option value=\"\"> --- </option>
<option value=Audio> Audio </option>
<option value=Video> Video </option>
<option value=Pro> Program </option>
<option value=Doc> Document </option>
<option value=Image> Image </option>
<option value=Col> Collection </option>
</select>
</td>

</tr>
</table>

          " label default

          
      | Q_FORMAT (label, default) -> 
          Printf.bprintf  buf "
<table border=0>
<tr>
<td> %s </td> 

<td> 
<input type=text name=format size=40 value=\"%s\">
</td>

<td> 
<select name=format_propose>
<option value=\"\"> --- </option>
<option value=avi> avi </option>
<option value=mp3> mp3 </option>
<option value=zip> zip </option>
<option value=gif> gif </option>
</select>
</td>

</tr>
</table>

          " label default
          
      | Q_MODULE (label, q) -> 
          Printf.bprintf buf "<table border=0> <tr><td> <h3> %s </h3> </td></tr>" label;
          Printf.bprintf buf "<tr><td>";
          iter q in_op;
          Printf.bprintf buf "</td></tr>";
          Printf.bprintf buf "</table>";
          
      | Q_HIDDEN list ->
          List.iter iter_hidden list
          
    and iter_hidden q =
      match q with
      
      | Q_AND list | Q_OR list | Q_HIDDEN list ->
          List.iter iter_hidden list
          
      | Q_ANDNOT (q1,q2) ->
          iter_hidden q1;
          iter_hidden q2;
          
      | Q_KEYWORDS (label, default) ->
          Printf.bprintf buf 
          "<input type=hidden name=keywords value=\"%s\">"
          default
          
      | Q_MINSIZE (label, default) ->
          Printf.bprintf  buf 
          "<input type=hidden name=minsize value=\"%s\">
           <input type=hidden name=minsize_unit value=\"1\">" default

      | Q_MAXSIZE (label, default) ->
          Printf.bprintf  buf 
          "<input type=hidden name=maxsize value=\"%s\">
           <input type=hidden name=maxsize_unit value=\"1\">" default
          
      | Q_MP3_BITRATE (label, default) -> 
          Printf.bprintf buf 
          "<input type=hidden name=bitrate value=\"%s\">"
          default

      | Q_MP3_ALBUM (label, default) -> 
          Printf.bprintf buf 
          "<input type=album name=bitrate value=\"%s\">"
          default

      | Q_MP3_TITLE (label, default) -> 
          Printf.bprintf  buf 
          "<input type=hidden name=title value=\"%s\">"
          default

      | Q_MP3_ARTIST (label, default) -> 
          Printf.bprintf  buf 
          "<input type=hidden name=artist value=\"%s\">"
          default
          
      | Q_MEDIA (label, default) -> 
          Printf.bprintf  buf 
          "<input type=hidden name=media value=\"%s\">
          <input type=hidden name=media_propose value=\"\">"
          default

          
      | Q_FORMAT (label, default) -> 
          Printf.bprintf  buf 
          "<input type=hidden name=format value=\"%s\">
          <input type=hidden name=format_propose value=\"\">"
          default
          
      | Q_MODULE (label, q) -> 
          iter_hidden q

    in
    iter q IN_AND;
    Buffer.add_string buf "</form>"
    
  with Not_found ->
      Printf.bprintf buf "No custom search %s" query

let complex_search buf =
  Buffer.add_string  buf
  "
<center>
<h2> Complex Search </h2>
</center>

<form action=/submit>
<table border=0>
<tr>
<td width=\"1%\"><input type=text name=query size=40 value=\"\"></td>
<td align=left><input type=submit value=Search></td>
</tr>
</table>

<h3> Simple Options </h3>

<table border=0>
<tr>
<td> Min size </td> 

<td> 
<input type=text name=minsize size=40 value=\"\">
</td>

<td> 
<select name=minsize_unit>
<option value=1048576> MBytes </option>
<option value=1024> kBytes </option>
<option value=1> Bytes </option>
</select>
</td>

</tr>

<tr>
<td> Max size </td> 

<td> 
<input type=text name=maxsize size=40 value=\"\">
</td>

<td> 
<select name=maxsize_unit>
<option value=1048576> Mbytes </option>
<option value=1024> kBytes </option>
<option value=1> Bytes </option>
</select>
</td>

</tr>

<tr>
<td> Media </td> 

<td> 
<input type=text name=media size=40 value=\"\">
</td>

<td> 
<select name=media_propose>
<option value=\"\"> --- </option>
<option value=Audio> Audio </option>
<option value=Video> Video </option>
<option value=Pro> Program </option>
<option value=Doc> Document </option>
<option value=Image> Image </option>
<option value=Col> Collection </option>
</select>
</td>

</tr>

<tr>
<td> Format </td> 

<td> 
<input type=text name=format size=40 value=\"\">
</td>

<td> 
<select name=format_propose>
<option value=\"\"> --- </option>
<option value=avi> avi </option>
<option value=mp3> mp3 </option>
<option value=zip> zip </option>
</select>
</td>

</tr>

</table>

<h3> Mp3 options </h3>

<table border=0>

<tr>
<td> Album </td> 
<td> 
<input type=text name=album size=40 value=\"\">
</td>
</tr>

<tr>
<td> Artist </td> 
<td> 
<input type=text name=artist size=40 value=\"\">
</td>
</tr>

<tr>
<td> Title </td> 
<td> 
<input type=text name=title size=40 value=\"\">
</td>
</tr>

<tr>
<td>
Min bitrate
</td>


<td> 
<select name=bitrate>
<option value=\"\"> --- </option>
<option value=64> 64 </option>
<option value=96> 96 </option>
<option value=128> 128 </option>
<option value=160> 160 </option>
<option value=192> 192 </option>
</select>
</td>

</tr>

</table>

<h3> Boolean options </h3>

<table border=0>

<tr>
<td> And </td> 
<td> 
<input type=text name=and size=40 value=\"\">
</td>
</tr>

<tr>
<td> Or </td> 
<td> 
<input type=text name=or size=40 value=\"\">
</td>
</tr>

<tr>
<td> Not </td> 
<td> 
<input type=text name=not size=40 value=\"\">
</td>
</tr>

</table>
</form>
"
      
let send_custom_query buf query args = 
  try
    let q = List.assoc query !!customized_queries in
    let args = ref args in
    let get_arg arg_name = 
      Printf.printf "Getting %s" arg_name; print_newline ();
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
          want_and_not (fun w -> QHasWord w) value
          
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
          want_comb_not 
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
          want_comb_not and_comb 
            (fun w -> QHasField("Artist", w)) artist
          
      | Q_MP3_TITLE _ ->
          let title = get_arg "title" in
          if title = "" then raise Not_found;
          want_comb_not and_comb 
            (fun w -> QHasField("Title", w)) title
          
      | Q_MP3_ALBUM _ ->
          let album = get_arg "album" in
          if album = "" then raise Not_found;
          want_comb_not and_comb 
            (fun w -> QHasField("Album", w)) album
          
      | Q_MP3_BITRATE _ ->
          let bitrate = get_arg "bitrate" in
          if bitrate = "" then raise Not_found;
          QHasMinVal("bitrate", Int32.of_string bitrate)

    in
    try
      let request = iter q in
      Printf.bprintf buf "Sending query !!!";
      start_search request buf
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