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
open Misc
open Cddb_lexer
open Mp3tag

(* 
What do we want ? Clearly, we simply want something like a
'list of words'  --->  'disc_title disc_year disc_id/ track_num track_title'
correspondance. Thus, we can remove a lot of information from 
the files: we can simply keep:
  
disc_id
disc_title
disc_year
track_title

All other information is useless for our purpose. Simply like that, we can
expect to decrease by four the size of the database (well, it is still
very big !). 

Then, for each word, we keep a disc_id/track_num where it appears (note that, if
the word appears in the disc_title, all the track_nums have to be added). This
can be done by associating a simple integer to each disc_id/track_num.

Try this storage:

words.index: all words are stored one after the other, with a 0 separator. The word get its position in the file as index. Suffixes are immediatly generated from a word and share its location in the file.

albums.index: the albums. each album takes its position in the file as an index.
it is stored as a tracks start position, a discid, the number of words in its title (one char), and the words (one int each)

tracks.index: the tracks. each track takes its position in the file as an index.
it is stored as: an album index (one int), a track number (one char),
the number of words (one char) and the words (one int each). Special cases:
  / and - are word delimiters, and special words.

Note that it can be generated linearly...

FIRST POSSIBILITY:
  
The indexer:
A tree node:
int: 0 (no corresponding entries) or the position of a track array.
char: the type of node 0: range 1: assoc  
  range:
    char: first char
    char: number of chars
    int array: positions of next nodes (0 if none)

  assoc:
    char: tested char (in increasing order) or 0 if last one
    int: position of corresponding node

track array:
  int: number of tracks
  int array: track positions in tracks.index

SECOND POSSIBILITY: index only albums...
  
  *)

type node = {
    mutable word : int;
    mutable tracks : int array;
    mutable next_nodes : (char * node) list;
  }
  
type t = {
    disc_id : string;
    disc_title : string;
    disc_year : int;
    disc_length : int;
    disc_info : string;
    disc_genre : string;
    
    track_title : string array;
    track_info : string array;
    track_pos : int array;
  }
  
module Tags = Id3v1
open Tags
  
let set_tags = ref false
let pattern = ref None
let save = ref false
  
let default_title = ref ""
let default_artist = ref ""
let default_album = ref ""
let default_year = ref ""
let default_tracknum = ref 0
let pauthor = ref None
let ptitle = ref None
let ptrack = ref None
let palbum = ref None
  
let set_filename_tags filename =
  let name = Filename.basename filename in
  let tags = 
    try
      let tags = Tags.read filename in
      lprintf "Filename %s is already tagged" filename; 
      lprint_newline ();  
      tags
    with _ -> 
        { 
          title = !default_title;
          artist = !default_artist; 
          album = !default_album;
          year = !default_year; 
          comment = ""; 
          tracknum = !default_tracknum; 
          genre = 0; 
        }
  in
  begin
    match !pattern with
      None -> ()
    | Some regexp ->
        if Str.string_match regexp name 0 then
          begin
            (match !pauthor with
                None -> ()
              | Some i ->
                  tags.artist <- Str.matched_group i name;
                  lprintf "Artist set to %s" tags.artist;
                  lprint_newline ();
            );
            (match !ptitle with
                None -> ()
              | Some i ->
                  tags.title <- Str.matched_group i name;
                  lprintf "Title set to %s" tags.title;
                  lprint_newline ();
            );
            (match !palbum with
                None -> ()
              | Some i ->
                  tags.album <- Str.matched_group i name;
                  lprintf "Album set to %s" tags.album;
                  lprint_newline ();
            );
            (match !ptrack with
                None -> ()
              | Some i ->
                  tags.tracknum <- int_of_string (Str.matched_group i name);
                  lprintf "Tracknum set to %d" tags.tracknum;
                  lprint_newline ();
            );
          end
        else
          (lprintf "%s doesn't match regexp" filename;
            lprint_newline () )
  end;
  lprintf "TAGS:"; lprint_newline ();
  lprintf "Title: %s" tags.title; lprint_newline ();
  lprintf "Artist: %s" tags.artist; lprint_newline ();
  lprintf "Album: %s" tags.album; lprint_newline ();
  lprintf "Year: %s" tags.year; lprint_newline ();
  lprintf "Track: %d" tags.tracknum; lprint_newline ();

  if !set_tags then
    Tags.write tags filename;
  
  let dirname = Filename.dirname filename in
  let new_name = dirname in
  let new_name = if tags.artist <> "" then
      Filename.concat new_name tags.artist else new_name in
  let new_name = if tags.album <> "" then
      Filename.concat new_name tags.album else new_name in
  let name = if tags.title <> "" then tags.title else name in
  let new_name = Filename.concat new_name
      (Printf.sprintf "%s%s.mp3" 
      (if tags.tracknum <> 0 && match name.[0] with
            '0' .. '9' -> false | _ -> true then
          Printf.sprintf "%02d-" tags.tracknum else "")
      name )
  in
  
  
  
  lprintf "Proposed name: %s" new_name; lprint_newline ();
  if !save then begin
      Unix2.safe_mkdir (Filename.dirname new_name);
      Unix2.rename filename new_name;
    end;
  ()

(*let files = Hashtbl.create 111 *)
let converted_file = ref None

let convert_nl s =
  for i = 1 to String.length s - 1 do
    if s.[i-1] = '\\' && s.[i] = 'n' then begin
        s.[i-1] <- ' ';
        s.[i] <- '\n';
      end
  done

(*  
let nwords = ref 0
let words = Hashtbl.create 111 

    *)

let get_int s pos =
  let c1 = int_of_char s.[pos+3] in
  let c2 = int_of_char s.[pos+2] in
  let c3 = int_of_char s.[pos+1] in
  let c4 = int_of_char s.[pos] in
  c1 + ((c2 + ((c3 + (c4 lsl 8)) lsl 8)) lsl 8)

let str_int s pos i =
  s.[pos+3] <- char_of_int (i land 255);
  s.[pos+2] <- char_of_int ((i lsr 8) land 255);
  s.[pos+1] <- char_of_int ((i lsr 16) land 255);
  s.[pos] <- char_of_int ((i lsr 24) land 255)

  (*
let buf_int8 buf i =
  Buffer.add_char buf (char_of_int (i land 255))
  
let buf_int buf (i : int) = 
  buf_int8 buf i;
  buf_int8 buf (i lsr 8);
  buf_int8 buf (i lsr 16);
  buf_int8 buf (i lsr 24)

let buf = Buffer.create 1000
let oxff = char_of_int 255
  
let index_string s =
  let list = String2.split_simplify s ' ' in
  Buffer.clear buf;
  List.iter (fun s ->
      match String.length s with
      | 1 -> Buffer.add_string buf s; Buffer.add_char buf oxff;
          Buffer.add_char buf oxff; Buffer.add_char buf oxff
      | 2 -> Buffer.add_string buf s;
          Buffer.add_char buf oxff; Buffer.add_char buf oxff
      | 3 -> Buffer.add_string buf s; Buffer.add_char buf oxff
      | _ -> 
          try
            let (i: int) = Hashtbl.find words s in
            buf_int buf i
          with _ ->
              let i = !nwords in
              incr nwords;
              Hashtbl.add words s i;
              buf_int buf i
  ) list;
  Buffer.contents buf

let index_file file =
  {
    disc_id = file.disc_id;
    disc_title = index_string file.disc_title;
    disc_year = file.disc_year;
    disc_length = file.disc_length;
    disc_info = index_string file.disc_info;
    disc_genre = file.disc_genre;
    
    track_title = Array.map index_string file.track_title;
    track_info = Array.map index_string file.track_info;
    track_pos = file.track_pos;
  }
    *)

let index file =
  let disc_id = ref None in
  let disc_length = ref None in
  let disc_year = ref None in
  let disc_title = ref None in
  let disc_info = ref "" in
  let disc_genre = ref None in
  
  let track_pos = ref [] in
  let track_title = ref [] in
  let track_info = ref [] in
  
  let ntracks = ref 0 in
  
  List.iter (fun info ->
      match info with
        TRACKPOS s -> track_pos := (int_of_string s) :: !track_pos
      | DISCLENGTH s -> disc_length := Some (int_of_string s)
      | DISCID s -> disc_id := Some s
      | DTITLE s -> disc_title := Some s
      | DYEAR "" -> ()
      | DYEAR s -> disc_year := Some (int_of_string s)
      | DGENRE "" -> ()
      | DGENRE s -> disc_genre := Some s
      | EXTD "" -> ()
      | EXTD s -> disc_info := Printf.sprintf "%s%s" !disc_info s
      | PLAYORDER _ -> ()
      | TTITLE (num, s) -> 
          ntracks := max !ntracks (num+1);
          track_title := (num,s) :: !track_title
      | EXTT (_, "") -> ()
      | EXTT (num, s) -> track_info := (num,s) :: !track_info
      | EOF -> ()
  ) file;
  let disc_id = match !disc_id with None ->
        failwith "No DISC ID" | Some s -> s in
  if List.length !track_pos <> !ntracks then begin
      lprintf "%s: track_pos %d <> ntracks %d" disc_id (List.length !track_pos) !ntracks; 
      lprint_newline ();
    end;
  let titles = Array.create !ntracks "" in
  let info = Array.create !ntracks "" in
  let pos = Array.of_list (List.rev !track_pos) in
  List.iter (fun (i,t) -> 
      titles.(i) <- (if titles.(i) = "" then t else
          Printf.sprintf "%s%s" titles.(i) t)) !track_title;
  List.iter (fun (i,s) -> 
      info.(i) <- ( if info.(i) = "" then s else
        Printf.sprintf "%s%s" info.(i) s) )
  !track_info;

  convert_nl !disc_info;
(*  lprintf "Disc info [%s] : %s" disc_id !disc_info; lprint_newline (); *)
  let file = 
    {
    disc_id = disc_id;
    disc_title = (match !disc_title with None ->
          failwith "No DISC TITLE"| Some s -> s);
    disc_year = (match !disc_year with None -> 0 | Some i -> i);
    disc_length = (match !disc_length with None -> 0 | Some i -> i);
    disc_info = !disc_info;
    disc_genre = (match !disc_genre with None -> "" | Some s -> "");
    
    track_title = titles;
    track_info = info;
    track_pos = pos;
    } in
(*  index_file *) file

let print_file oc file =
  (*
  lprintf "Disc %s: %s" file.disc_id file.disc_title; lprint_newline ();
  lprintf "Year %d Length %d" file.disc_year file.disc_length;
  lprint_newline ();
  lprintf "Info: %s" file.disc_info; lprint_newline ();
  for i = 0 to Array.length file.track_title - 1 do
    lprintf "Track %2d Pos:%9d Name: %s [%s]" 
      (i+1) file.track_pos.(i) file.track_title.(i) file.track_info.(i);
    lprint_newline ();
  done;
  lprintf "End Disc"; lprint_newline () 
*)
  Printf.fprintf oc "DISCID=%s\n" file.disc_id; 
  Printf.fprintf oc "DTITLE=%s\n" (String.lowercase file.disc_title); 
  for i = 0 to Array.length file.track_title - 1 do
    Printf.fprintf oc "TTITLE%d=%s\n" i (String.lowercase file.track_title.(i)); 
  done;
  Printf.fprintf oc "\n"

(* some hexa stuff... one module would be shared with Md4 *)
let i_a = int_of_char 'a'  
let i_A = int_of_char 'A'  
let i_f = int_of_char 'f'  
let i_F = int_of_char 'F'  
let i_0 = int_of_char '0'
let i_9 = int_of_char '9'

let digit_hexa c =
  let i = int_of_char c in
  if i >= i_a && i <= i_f then i - i_a + 10 else
  if i >= i_A && i <= i_F then i - i_A + 10 else
  if i >= i_0 && i <= i_9 then i - i_0 else
    failwith "Bad hexa char"

let hexa_to_string s =
  let len = String.length s in
  let p = String.create (2 * len) in
  for i = 0 to len-1 do
    let c = s.[i] in
    let n = int_of_char c in
    let i0 = (n/16) land 15 in
    let i1 = n land 15 in
    p.[2 * i] <- hexa_digit i0;
    p.[2 * i+1] <- hexa_digit i1;
  done;
  p
      
let hexa_of_string s len =
  let p = String.create (len/2) in
  for i = 0 to len/2 - 1 do
    let c0 = s.[2*i] in
    let c1 = s.[2*i+1] in
    p.[i] <- char_of_int ((16 * digit_hexa c0) + digit_hexa c1);
  done;
  p

let delimiters = [ 
    "." ; "," ; ";" ; "\"" ; "/" ; "-" ; "(" ; ")" ; ":" ; "\""; "["; "]"; "?" ; "!"; "{"; "}" ; "<" ; ">" ; "+"; "#"
    ]
  
let rec index_delim_from s pos len =
  if pos = len then raise Not_found;
  match s.[pos] with
    '.' | ',' | ';' | '\'' | '/' | '-' | '(' | ')' | '[' | ']' | ':' | '"' | '?' | '!' | '{' | '}' | '<' | '>' | '+' | '#' -> pos
  | _ -> index_delim_from s (pos+1) len
  
let rec index_spaces_from s pos len =
  if pos = len then raise Not_found;
  match s.[pos] with
    ' ' | '_' | '~' | '\\' -> pos
  | _ -> index_spaces_from s (pos+1) len
  
let split_simplify s =
  let len = String.length s in
  let rec iter pos =
    if pos = len then [] else
    let pos2 = try index_delim_from s pos len with _ -> len  in
    
    let pos3 = try index_spaces_from s pos len with _ -> len in
    if pos3 < pos2 then
      if pos3 = pos then iter (pos3+1) else
        (String.sub s pos (pos3-pos)) :: (iter (pos3+1))
    else
    if pos2 = pos then
      (String.make 1 s.[pos2]) :: (iter (pos2+1))        
    else
    if pos2 < len then
      (String.sub s pos (pos2-pos)) :: (String.make 1 s.[pos2]) ::
      (iter (pos2+1))
    else
      [String.sub s pos (len-pos)]
  in
  iter 0
;;

let s1 = String.create 1
let s2 = String.create 2
let s3 = String.create 3
let s4 = String.create 4
let s5 = String.create 5


let put_string oc pos s =
  output_string oc s;
  pos := !pos + String.length s

let put_int oc pos i =
  let s = String.create 4 in
  str_int s 0 i;
  put_string oc pos s

let put_int8 oc pos i =
  let s = String.make 1 (char_of_int i) in
  put_string oc pos s

let save_tree tree =
  let tree_oc = open_out "tree.index" in
  let tree_pos = ref 0 in (* we use a one offset *)
  put_int tree_oc tree_pos 0;
  
  let rec save_tree node =
    let first_char = ref 255 in
    let last_char = ref 0 in
    let nodes = List.map 
        (fun (char, node) -> 
          let i = int_of_char char in
          if i < !first_char then first_char := i;
          if i > !last_char then last_char := i;
          (char, save_tree node)) 
      node.next_nodes in
    let array = save_array node.tracks in
    let node_num = !tree_pos in
    put_int tree_oc tree_pos node.word;
    put_int tree_oc tree_pos array;
    
    if 3 * List.length nodes > !last_char - !first_char then
(* assoc *)
      save_assoc nodes
    else
      save_range nodes !first_char !last_char;
    node_num
    
  and save_array track_list =
    let len = Array.length track_list in
    if len = 0 then 0 else
    let array_pos = !tree_pos in
    put_int tree_oc tree_pos len;
    for i = 0 to len - 1 do
      if track_list.(i) <> -1 then
        put_int tree_oc tree_pos track_list.(i);
    done;
    array_pos
    
  and save_assoc nodes =
    put_int8 tree_oc tree_pos 0;
    List.iter (fun (char, num) ->
        put_int8 tree_oc tree_pos (int_of_char char);
        put_int tree_oc tree_pos num) nodes;
    put_int8 tree_oc tree_pos 0
    
  and save_range nodes first last = 
(*    lprintf "range %d %d-%d not implemented" (List.length nodes) 
    first last; lprint_newline ();*)
    save_assoc nodes (* use assocs ... *)
    
  in
  let first_node = save_tree tree in
  seek_out tree_oc 0;
  put_int tree_oc tree_pos first_node;
  close_out tree_oc

  
let index_file filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  
  let words_oc = open_out "words.index" in
  let words_pos = ref 0 in
  
  let albums_oc = open_out "albums.index" in
  let albums_pos = ref 0 in
  
  let tracks_oc = open_out "tracks.index" in
  let tracks_pos = ref 0 in
  
  let tree = { 
      word = -1;
      tracks = [||]; 
      next_nodes = [] } in
  
  let rec set_word_num node w pos len num =
    if len = pos then
      if node.word = -1 then
        (node.word <- num; num) else
        node.word
    else
    let node = try
        List.assoc w.[pos] node.next_nodes
      with _ ->
          let new_node = { word = -1; tracks = [||]; next_nodes = [] } in
          node.next_nodes <- (w.[pos], new_node) :: node.next_nodes;
          new_node
    in
    set_word_num node w (pos+1) len num
  in
  
  let rec add_suffixes w pos len num =
    if pos+2 < len then
      let suffix = String.sub w pos (len - pos) in
      if set_word_num tree suffix 0 (len-pos) num = num then
        add_suffixes w (pos+1) len (num+1)
  in
  
  let rec remove_unreadable words =
    match words with
      [] -> []
    | w :: ws ->
        let ws = remove_unreadable ws in
        let len = String.length w in
        try
          for i = 0 to len - 1 do
            let c = int_of_char w.[i] in
            let c = if c >= 192 && c <= 222 then c+32 else c  in
            match c with
            | 247
            | 254 -> raise Exit
                | _ -> 
                if c >= 127 && c <= 191 then raise Exit
          done;
          w :: ws
        with _ ->
            lprintf "- [%s]" w; lprint_newline (); ws 
  in
  
  let convert_chars w =
    let len = String.length w in  
    let buf = Buffer.create 30 in
    for i = 0 to len - 1 do
      let c = int_of_char w.[i] in
      let c = if c >= 192 && c <= 222 then c+32 else c  in
      Buffer.add_string buf (match c with
        | 124 -> "u"
        | 160 -> " "
        | 180 -> "'"
        | 223 -> "ss"
        | 224 -> "a"
        | 225 -> "a"
        | 226 -> "a"
        | 227 -> "a"
        | 228 -> "a"
        | 229 -> "a"
        | 230 -> "e"
        | 231 -> "c"
        | 232 -> "e"
        | 233 -> "e"
        | 234 -> "e"
        | 235 -> "e"
        | 236 -> "i"
        | 237 -> "i"
        | 238 -> "i"
        | 239 -> "i"
        | 240 -> "o"
        | 241 -> "n"
        | 242 -> "o"
        | 243 -> "o"
        | 244 -> "o"
        | 245 -> "o"
        | 246 -> "o"

        | 248 -> "o"
        | 249 -> "u"
        | 250 -> "u"
        | 251 -> "u"
        | 252 -> "u"
        | 253 -> "y"
        | 254 -> "b"
        | 255 -> "y"
        | _ -> 
             String.make 1 (char_of_int c))
    done;
    Buffer.contents buf

  in
  
  let index_string oc pos s =
    let s = convert_chars s in
    let words = split_simplify s in
    let words = remove_unreadable words in
    let nwords = List.length words in
    put_int8 oc pos nwords;
    List.iter (fun w ->
        let len = String.length w in
        let w = String.lowercase w in
        match len with
          1 -> 
            let s = s2 in
            s.[0] <- char_of_int 127;
            s.[1] <- w.[0];
            put_string oc pos s;
            let w_num = (127 lsl 24) + (int_of_char w.[0] lsl 16) in
            ignore (set_word_num tree w 0 1 w_num)
        | 2 ->
            let s = s3 in
            s.[0] <- char_of_int 126;
            s.[1] <- w.[0];
            s.[2] <- w.[1];
            put_string oc pos s;
            let w_num = (126 lsl 24) + (int_of_char w.[0] lsl 16) +
                (int_of_char w.[1] lsl 8) in
            ignore (set_word_num tree w 0 2 w_num)
        
        | 3 ->
            let s = s4 in
            s.[0] <- char_of_int 125;
            s.[1] <- w.[0];
            s.[2] <- w.[1];
            s.[3] <- w.[2];
            put_string oc pos s;
            let w_num = (125 lsl 24) + (int_of_char w.[0] lsl 16)
              + (int_of_char w.[1] lsl 8) + int_of_char w.[2] in
            ignore (set_word_num tree w 0 3 w_num)
        
        | len ->
            let w_num = 
              let w_num = set_word_num tree w 0 len !words_pos in
              if w_num = !words_pos then begin
                  put_string words_oc words_pos w;
                  put_int8 words_oc words_pos 0;
                  add_suffixes w 0 len w_num;
                  w_num
                end else w_num
            in
            put_int oc pos w_num
    ) words;
    words
  in
  
  let rec add_tree_word num w len node pos =
    if pos = len then
      let alen = Array.length node.tracks in
      try
        for i = 0 to alen - 1 do
          if node.tracks.(i) = -1 then
            (node.tracks.(i) <- num; raise Exit)
          else if node.tracks.(i) = num then raise Exit
        done;
(* not found *)
        let new_len = alen + 1 + alen / 5 in
        let new_array = Array.create new_len (-1) in
        Array.blit node.tracks 0 new_array 0 alen;
        node.tracks <- new_array;        
        node.tracks.(alen) <- num;
      with _ -> ()
    else
    let node =
      try
        List.assoc w.[pos] node.next_nodes 
      with _ -> assert false;
          let new_node = { 
              word = -1;
              tracks = [||]; 
              next_nodes = []; 
            } in
          node.next_nodes <- (w.[pos], new_node) :: node.next_nodes;
          new_node
    in
    add_tree_word num w len node (pos+1)
  
  in
  let add_tree_words num ws =
    List.iter (fun w ->
        let len = String.length w in
        if not (List.mem w delimiters) then
          add_tree_word num w len tree 0) ws
  in

(*
  let index_item album_num list =
    let list = List.rev list in
    let disc_words = ref []  in
    List.iter (fun v ->
        match v with
          DISCID s -> 
            put_int albums_oc albums_pos !tracks_pos;
            let s = of_string s 8 in
            put_string albums_oc albums_pos s
        | DTITLE s ->
            disc_words := index_string albums_oc albums_pos s
        | TTITLE (i, title) ->
            let track_num = !tracks_pos in
            put_int tracks_oc tracks_pos album_num;
            put_int8 tracks_oc tracks_pos i;
            let track_words = index_string tracks_oc tracks_pos title in
            add_tree_words track_num !disc_words;
            add_tree_words track_num track_words;
        | _ -> assert false
    ) list
  in
*)
  
  let index_item album_num list =
    let list = List.rev list in
    List.iter (fun v ->
        match v with
          DISCID s -> 
            put_int albums_oc albums_pos !tracks_pos;
            let s = hexa_of_string s 8 in
            put_string albums_oc albums_pos s
        | DTITLE s ->
            let disc_words = index_string albums_oc albums_pos s in
            add_tree_words album_num disc_words
        
        | TTITLE (i, title) ->
            let track_num = !tracks_pos in
            let track_words = index_string tracks_oc tracks_pos title in
            if track_words = [] then
              (lprintf "track removed [%s]" title; lprint_newline ())
            else begin
                put_int tracks_oc tracks_pos album_num;
                put_int8 tracks_oc tracks_pos i;
                add_tree_words album_num track_words;
              end
        | _ -> assert false
    ) list;
    flush albums_oc;
    flush words_oc;
    flush tracks_oc;
  in
  
  let rec iter list =
    let value = Cddb_lexer.line lexbuf in
    match value with
      DISCID v -> 
        index_item !albums_pos list; iter [value]
    | EOF -> index_item !albums_pos list
    | _ -> iter (value :: list)
  in
  iter [];
  close_in ic;
  close_out words_oc;
  close_out albums_oc;
  put_int tracks_oc tracks_pos (-1);
  close_out tracks_oc;
  
  save_tree tree
    
let convert_file oc filename =
  try
    let file = Cddb_file.read filename in
    let file = index file in
    print_file oc file; 
(*    Hashtbl.add files (Filename.basename filename) file *)
  with e ->
      lprintf "%s : Exception %s" filename
        (Printexc2.to_string e);
      lprint_newline ();
      exit 1

let input_int ic pos =
  try
    seek_in ic pos;
    really_input ic s4 0 4;
    get_int s4 0
  with e -> lprintf "at pos %d" pos; lprint_newline (); raise e

let input_int8 ic pos =
  try
    seek_in ic pos;
    really_input ic s1 0 1;
    int_of_char s1.[0]
  with e -> lprintf "at pos %d" pos; lprint_newline (); raise e
      
let input_string ic pos len =
  try
    seek_in ic pos;
    let s = String.create len in
    really_input ic s 0 len;
    s
  with e -> lprintf "at pos %d" pos; lprint_newline (); raise e
      
let s255 = String.create 255
let input_word ic pos =
  try
    seek_in ic pos;
    let rec iter pos =
      let c = input_char ic in
      s255.[pos] <- c;
      if int_of_char c = 0 then String.sub s255 0 pos else
        iter (pos+1)
    in
    iter 0
  with e -> lprintf "at pos %d" pos; lprint_newline (); raise e
      
let rec merge_lists l1 l2 tail =
  match l1, l2 with
    h1 :: t1, h2 :: t2 -> 
      if h1 < h2 then
        merge_lists t1 l2 (h1 :: tail)
      else
      if h1 = h2 then
        merge_lists t1 t2 (h1 :: tail)
      else
        merge_lists l1 t2 (h2 :: tail)
  | h1 :: t1, [] ->
      (List.rev tail) @ l1
  | [], _ :: _ -> 
      (List.rev tail) @ l2
  | [], [] -> List.rev tail

  
let find words =
  let words_array = Array.of_list words in
  let nwords = Array.length words_array in
  let album_scores = Array.create (nwords * nwords+1) [] in
  let track_scores = Array.create (nwords * nwords + nwords +1) [] in
  let tree_ic = open_in "tree.index" in
  let albums_ic = open_in "albums.index" in
  let words_ic = open_in "words.index" in
  let tracks_ic = open_in "tracks.index" in
  let initial_node_pos = input_int tree_ic 0 in
  
  
  let decompress_title ic pos =
    let nwords = input_int8 ic pos in
    let rec iter_words nwords pos ws =
      if nwords = 0 then List.rev ws else
      let c = input_int8 ic pos in
      match c with
        127 -> 
          let w = input_string ic (pos+1) 1 in
          iter_words (nwords-1) (pos+2) (w :: ws)
      | 126 ->
          let w = input_string ic (pos+1) 2 in
          iter_words (nwords-1) (pos+3) (w :: ws)
      | 125 ->
          let w = input_string ic (pos+1) 3 in
          iter_words (nwords-1) (pos+4) (w :: ws)
      | _ -> 
          let word_pos = input_int ic pos in
          let w = input_word words_ic word_pos in
          iter_words (nwords-1) (pos+4) (w :: ws)
    in
    let ws = iter_words nwords (pos+1) [] in
    String2.unsplit ws ' '
  in
  
  let iter_title f ic pos =
(*
  let f w1 w2 =
      lprintf "<"; lprint_newline ();
      f w1 w2;
      lprintf ">"; lprint_newline ();      
in
  *)
    let nwords = input_int8 ic pos in
    let rec iter_words nwords pos =
      if nwords > 0 then
        let c = input_int8 ic pos in
(*        lprintf "pos %d" pos; lprint_newline (); *)
        let w_num = input_int ic pos in
        match c with
          127 -> 
            let w = input_string ic (pos+1) 1 in
            let w_num = w_num land (0xffff lsl 16) in
            f w_num w;
            iter_words (nwords-1) (pos+2)
        | 126 ->
            let w = input_string ic (pos+1) 2 in
            let w_num = w_num land (0xffffff lsl 8) in
            f w_num w;
            iter_words (nwords-1) (pos+3)
        | 125 ->
            let w = input_string ic (pos+1) 3 in
            f w_num w;            
            iter_words (nwords-1) (pos+4)
        | _ -> 
            let word_pos = input_int ic pos in
            let w = input_word words_ic word_pos in
            f w_num w;
            iter_words (nwords-1) (pos+4)
      else
        pos
    in
    let pos = iter_words nwords (pos+1) in
(*    lprintf "done iter_title %d" pos; lprint_newline (); *)
    pos
  in
  
  let rec find_word w len pos node_pos =
(*    lprintf "find_word pos %d for %s" pos w; lprint_newline (); *)
    if len = pos then
      let w_num = input_int tree_ic node_pos in
(*      lprintf "w_num: %d" w_num; lprint_newline (); *)
      let w_array = if w_num = -1 then [||] else
        let array_pos = input_int tree_ic (node_pos+4) in
        let array_len = input_int tree_ic array_pos in
        Array.init array_len (fun i ->
            input_int tree_ic (array_pos + 4 + 4 * i)
        )
      in
      (w_num, w_array)
    else
    let c = int_of_char w.[pos] in
(*    lprintf "find %d[%c]" c (char_of_int c); lprint_newline (); *)
    let rec iter_assoc char_pos =
      let cc = input_int8 tree_ic char_pos in
(*  lprintf "try with %d[%c]" cc (char_of_int cc); lprint_newline (); *)
      if cc = 0 then (-1, [||]) else
      if cc = c then
        let node_pos = input_int tree_ic (char_pos+1) in
        find_word w len (pos+1) node_pos
      else
        iter_assoc (char_pos+5)
    in
    
    iter_assoc (node_pos+9)
  in
  
  let hits = ref [] in
  
  let word_nums = List.map (fun w ->
        let (w_num, w_array) = find_word w (String.length w) 0 initial_node_pos
        in
        if w_num = -1 then begin
            lprintf "No hit for %s" w; lprint_newline ();
          end;
        hits := merge_lists !hits (Array.to_list w_array) [];
(*        lprintf "word %s ---> %d" w w_num; lprint_newline (); *)
        w_num
    ) words in
  
  let print_album cd_pos =
    let tracks_pos = input_int albums_ic cd_pos in
    let disc_id = input_string albums_ic (cd_pos+4) 4 in
    lprintf "disc_id %s" (String.lowercase (
        hexa_to_string disc_id)); lprint_newline (); 
    
    let album = decompress_title albums_ic (cd_pos+8) in
    lprintf "Album: %s" album;
    lprint_newline ();
  in    
  
  let positions = Array.create nwords 0 in
(*  lprintf "%d albums" (List.length !hits); lprint_newline (); *)
  List.iter (fun cd_pos  ->
(*      lprintf "album_pos %d" cd_pos; lprint_newline ();
      print_album cd_pos; *)
      
      let album_score = ref 0 in
(* compute the score of each album, and of each track of each album *) 
      let tracks_pos = input_int albums_ic cd_pos in
      let disc_id = input_string albums_ic (cd_pos+4) 4 in
(*      lprintf "disc_id %s" (String.lowercase ( 
          hexa_to_string disc_id)); lprint_newline (); *)
      let disc_counted = ref [] in
      ignore (iter_title (fun w_num w ->
(*            lprintf "word %s --> %d" w w_num; lprint_newline (); *)
            if not (List.memq w_num !disc_counted) &&
              List.memq w_num word_nums then begin
                disc_counted := w_num :: !disc_counted;
                album_score := !album_score + nwords;
              end;
        ) albums_ic  (cd_pos+8));
(*      lprintf "score : %d" !album_score; lprint_newline (); *)
      album_scores.(!album_score) <- cd_pos :: album_scores.(!album_score);
      let album_score = !album_score in

(* for each track, computes the track score *)
      let rec iter_track track_pos =
(*        lprintf "track_pos %d" track_pos; lprint_newline (); *)
        let disc_pos = input_int tracks_ic track_pos in
        if disc_pos = cd_pos then begin
(*            lprintf "in track"; lprint_newline (); *)
            let track_score = ref album_score in
            let track_num  = input_int8 tracks_ic (track_pos+4) in
            let w_counted = ref [] in
            let next_track = iter_title (fun w_num w ->
(*       lprintf "word %s --> %d" w w_num; lprint_newline (); *)
                  if not (List.memq w_num !w_counted) &&
                    List.memq w_num word_nums then begin
                      w_counted := w_num :: !w_counted;
                      if List.memq w_num !disc_counted then
                        incr track_score
                      else 
                        track_score := !track_score + nwords
                    end;
              
              ) tracks_ic  (track_pos+5) in
(*            lprintf "1"; lprint_newline (); *)
            if !track_score > album_score then begin
(*                lprintf "3 %d" !track_score; lprint_newline (); *)
                track_scores.(!track_score) <- 
                  track_pos :: track_scores.(!track_score);
(*                lprintf "Track %d : %s" track_num 
                  (decompress_title tracks_ic (track_pos+5));
                lprint_newline (); 
                lprintf "Track %d score: %d" track_num !track_score;
                lprint_newline (); *)
              end;
(*            lprintf "2"; lprint_newline ();
            lprintf "next_track"; lprint_newline (); *)
            iter_track next_track
          end
      in
      iter_track tracks_pos;
      ()    
  ) !hits;
  
  let rec print score =
    if score > 0 then begin
        List.iter (fun track_pos ->        
            let disc_pos = input_int tracks_ic track_pos in
            let track_num  = input_int8 tracks_ic (track_pos+4) in            
            let track_title = decompress_title tracks_ic (track_pos+5) in
            let disc_title = decompress_title albums_ic (disc_pos + 8) in
            lprintf "SCORE: %d keywords" score; lprint_newline ();
            lprintf "TRACK %3d OF %s" track_num disc_title;
            lprint_newline ();
            lprintf "   %s" track_title; lprint_newline ();
        ) track_scores.(score);
        
        if score <= nwords * nwords then
          List.iter (fun disc_pos ->        
              let disc_title = decompress_title albums_ic (disc_pos + 8) in
              lprintf "SCORE: %d keywords" score; lprint_newline ();
              lprintf "ALBUM:   %s" disc_title; lprint_newline ();
          ) album_scores.(score);
        
        print (score-1);
      end
  in
  print (nwords * (nwords + 1))

  
let convert_dir oc dirname =
  Unix2.iter_directory (convert_file oc) dirname
      
let _ = 
(*  Gc.set { (Gc.get ()) with Gc.max_overhead = 30 }; *)
  Arg.parse [
    "-s", Arg.Set set_tags, ": allow tag modification";
    "-save", Arg.Set save, " : move file depending on tags";
    "-p", Arg.String (fun s -> pattern := Some (Str.regexp s)), 
    " <regexp> : regexp to recognise fields";
    "-pauthor", Arg.Int (fun i -> pauthor := Some i), 
    " <i> : position of author in regexp";
    "-ptitle", Arg.Int (fun i -> ptitle := Some i), 
    " <i> : position of title in regexp";
    "-ptrack", Arg.Int (fun i -> ptrack := Some i), 
    " <i> : position of track in regexp";
    "-palbum", Arg.Int (fun i -> palbum := Some i), 
    " <i> : position of album in regexp";
    "-cddb", Arg.String (fun s ->
        ignore (Cddb_file.read s)), "";
    "-index", Arg.String index_file, "";
    "-convert", Arg.String (fun s -> 
        let oc = open_out s in
        converted_file := Some oc), 
    " <filename> : convert a list of CDDB files into this binary file";
    "-find", Arg.String (fun s ->
        let ws = String2.split_simplify s ' ' in
        find ws
    ), " : find albums with keywords";
    "-send_udp", Arg.Unit (fun _ ->
        let sock = UdpSocket.create_sendonly () in
        for i = 0 to 100000 do
          let s = Printf.sprintf "Packet %d" i in
          UdpSocket.write sock s (Ip.of_string "128.93.52.5") 1999
        done;
        BasicSocket.loop ()
    ), " : test UDP send";
  ] 
    (fun filename ->
      match !converted_file with
        None ->  set_filename_tags filename
      | Some oc -> convert_dir oc filename
  ) "usage";
  match !converted_file with
    None -> ()
  | Some oc ->
      close_out oc

      
  
