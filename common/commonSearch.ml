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
open CommonNetwork
open CommonResult
open Options
open CommonTypes

    
let search_string q =
  let rec iter q =
    match q with
      QAnd (q1, q2) -> Printf.sprintf "(%s) AND (%s)" (iter q1) (iter q2)
    | QOr (q1, q2) ->  Printf.sprintf "(%s) OR (%s)" (iter q1) (iter q2)
    | QAndNot (q1, q2) ->  Printf.sprintf "(%s) AND NOT (%s)" (iter q1) (iter q2)
    | QHasWord s -> Printf.sprintf "CONTAINS[%s]" s
    | QHasField (f,s) -> Printf.sprintf "[%s]CONTAINS[%s]" f s
    | QHasMinVal (f,v) -> Printf.sprintf "[%s]>%ld" f v
    | QHasMaxVal (f,v) -> Printf.sprintf "[%s]<%ld" f v
    | QNone ->
	prerr_endline "QNone in query";
	""
  in
  iter q
  
let search_num = ref 0
let searches_by_num = Hashtbl.create 1027
  
let searches = ref ([] : search list)
  
let new_search query =
  incr search_num;
  let s = {
      search_num = !search_num;      
      search_max_hits = 10000;
      search_query = query;
      search_nresults = 0;
      search_results = Intmap.empty;
      search_waiting = 0;
      search_string = search_string query;
      search_closed = false;
      op_search_new_result_handlers = [];
      op_search_end_reply_handlers = [];
    } in
  Hashtbl.add searches_by_num !search_num s;
  searches := s :: !searches;
  s

let search_find num = Hashtbl.find searches_by_num num

let search_add_result s r =
  try
    let (c,_) = Intmap.find r.impl_result_num s.search_results in
    incr c
  with _ ->
      s.search_results <- Intmap.add r.impl_result_num (ref 1, as_result r)
      s.search_results;
      let r = as_result r in 
      List.iter (fun f -> f r) s.op_search_new_result_handlers

let search_end_reply s = 
  s.search_waiting <- s.search_waiting - 1;
  List.iter (fun f -> f ()) s.op_search_end_reply_handlers  
      
(* should call a handler to send the result to the GUI ... *)
      
        
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
   

  
type englob_op = IN_NOOP | IN_AND | IN_OR
  
let custom_query buf query =
  try
    let q = List.assoc query !!CommonComplexOptions.customized_queries in
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

let search_forget s =
  networks_iter (fun n -> network_forget_search n s);
  Hashtbl.remove searches_by_num s.search_num;
  searches := List2.removeq s !searches
  
  
let search_media_list = 
  [ "Program", "Pro";
    "Documentation", "Doc";
    "Collection", "Col";
  ]

module P = Gui_proto

(** Transforme une query_entry en provenance de la gui
   en query. Les conversions pour Media sont faites
   au vol, pour donkey.*)

let rec mftp_query_of_query_entry qe =
  match qe with
    P.Q_AND ([]|[_]) -> QNone
  | P.Q_AND (h :: q) ->
      List.fold_left
	(fun acc -> fun q -> QAnd (acc, mftp_query_of_query_entry q))
	(mftp_query_of_query_entry h)
	q

  | P.Q_OR ([]|[_]) -> QNone
  | P.Q_OR (h :: q) ->
      List.fold_left
	(fun acc -> fun q -> QOr (acc, mftp_query_of_query_entry q))
	(mftp_query_of_query_entry h)
	q
  | P.Q_ANDNOT (q1,q2) ->
      QAndNot 
	(mftp_query_of_query_entry q1,
	 mftp_query_of_query_entry q2)
  | P.Q_MODULE (_, q) ->
      mftp_query_of_query_entry q
	
  | P.Q_KEYWORDS (_,s) ->
      (
       try want_and_not (fun w -> QHasWord w) s
       with Not_found -> QNone
      )

  | P.Q_MINSIZE (_,s) ->
      (
       try QHasMinVal ("size", Int32.of_string s)
       with _ -> QNone
      )
  | P.Q_MAXSIZE (_,s) ->
      (
       try QHasMaxVal ("size", Int32.of_string s)
       with _ -> QNone
      )
  | P.Q_FORMAT (_,s) ->
      (
       try
	 want_comb_not or_comb
           (fun w -> QHasField("format", w)) s
       with Not_found ->
	 QNone
      )
  | P.Q_MEDIA (_,s) ->
      (
       try QHasField("type", List.assoc s search_media_list)
       with Not_found -> 
	 match String2.split_simplify s ' ' with
	   [] -> QNone
	 | _ -> QHasField ("type", s)
      )
	
  | P.Q_MP3_ARTIST (_,s) ->
      (
       try 
	 want_comb_not and_comb 
           (fun w -> QHasField("Artist", w)) s
       with Not_found ->
	 QNone
      )

  | P.Q_MP3_TITLE (_,s) ->
      (
       try 
	 want_comb_not and_comb 
           (fun w -> QHasField("Title", w)) s
       with Not_found ->
	 QNone
      ) 
  | P.Q_MP3_ALBUM (_,s) ->
      (
       try 
	 want_comb_not and_comb 
           (fun w -> QHasField("Album", w)) s
       with Not_found ->
	 QNone
      )

  | P.Q_MP3_BITRATE (_,s) ->
      let bitrate = 
	try Int32.of_string s
	with _ -> Int32.of_int 1
      in
      QHasMinVal("bitrate", bitrate)

  | P.Q_HIDDEN [q] ->
      mftp_query_of_query_entry q
  | P.Q_HIDDEN l ->
      mftp_query_of_query_entry (P.Q_AND l)


