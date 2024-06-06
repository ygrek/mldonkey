(**************************************************************************)
(*  Copyright 2003, 2002 b8_bavard, b8_zoggy, , b52_simon INRIA            *)
(*                                                                        *)
(*    This file is part of mldonkey.                                      *)
(*                                                                        *)
(*    mldonkey is free software; you can redistribute it and/or modify    *)
(*    it under the terms of the GNU General Public License as published   *)
(*    by the Free Software Foundation; either version 2 of the License,   *)
(*    or (at your option) any later version.                              *)
(*                                                                        *)
(*    mldonkey is distributed in the hope that it will be useful,         *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of      *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       *)
(*    GNU General Public License for more details.                        *)
(*                                                                        *)
(*    You should have received a copy of the GNU General Public License   *)
(*    along with mldonkey; if not, write to the Free Software             *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston,               *)
(*    MA  02111-1307  USA                                                 *)
(*                                                                        *)
(**************************************************************************)

(** Reading and writing tags in MP3 files. *)


(** Reading and writing id3 v1.1 tags. *)
module Id3v1 = 
  struct

    type tag = { 
        mutable title: string; 
        mutable artist: string; 
        mutable album: string;
        mutable year:string; 
        mutable comment: string; 
        mutable tracknum: int; 
        mutable genre: int 
      }

    (** Check if the given file has a id3 v1.1 tag.
       @raise Sys_error if an error occurs while opening the file. 
    *)
    let has_tag filename =
      let ic = open_in_bin filename in
      let len = in_channel_length ic in
      let res =
        if len < 128 then false else begin
          seek_in ic (len - 128);
          let buffer = String.create 3 in
          really_input ic buffer 0 3;
          buffer = (Bytes.of_string "TAG")
        end in
      close_in ic;
      res

let read_channel ic =
    let len = in_channel_length ic in
    if len < 128 then raise Not_found;
    seek_in ic (len - 128);
    let readstring len =
      let buf = String.create len in
      really_input ic buf 0 len;
      Mp3_misc.chop_whitespace (Bytes.to_string buf) 0 in
    if readstring 3 <> "TAG" then raise Not_found;
    let title = readstring 30 in
    let artist = readstring 30 in
    let album = readstring 30 in
    let year = readstring 4 in
    let comment = readstring 29 in
    let tracknum = input_byte ic in
    let genre = input_byte ic in
    { title = title; artist = artist; album = album; year = year;
      comment = comment; tracknum = tracknum; genre = genre }

    (** Read the tags in a mp3 file.
       @raise Not_found if the file doesn't contain tags.
       @raise Sys_error if an error occurs while opening the file. 
    *)
    let read_tag filename =
      let ic = open_in_bin filename in
      try
        let res = read_channel ic in
        close_in ic; res
      with x ->
        close_in ic; raise x
          
    (** Write the given tag info into the given file. 
       @raise Sys_error if an error occurs with the file.
    *)
    let write ?(title = "") ?(artist = "") ?(album = "") 
        ?(year = "") ?(comment = "") ?(tracknum = 1) ?(genre = 0)
        filename =
      let oc =
        open_out_gen [Open_wronly; Open_append; Open_binary] 0o666 filename in
      let put s len =
        let l = String.length s in
        for i = 0 to min l len - 1 do output_char oc s.[i] done;
        for i = l to len - 1 do output_byte oc 0 done in
      output_string oc "TAG";
      put title 30;
      put artist 30;
      put album 30;
      put year 4;
      put comment 29;
      output_byte oc tracknum;
      output_byte oc genre;
      close_out oc

    (** Write the given tag structure into the given file. 
       @raise Sys_error if an error occurs with the file.
    *)
    let write_tag filename tag =
      write ~title: tag.title ~artist: tag.artist ~album: tag.album
        ~year: tag.year ~comment: tag.comment ~tracknum: tag.tracknum
        ~genre: tag.genre filename

    let merge t1 t2 =
      { title = if t2.title <> "" then t2.title else t1.title;
        artist = if t2.artist <> "" then t2.artist else t1.artist;
        album = if t2.album <> "" then t2.album else t1.album;
        year = if t2.year <> "" && t2.year <> "0" then t2.year else t1.year;
        comment = if t2.comment <> "" then t2.comment else t1.comment;
        tracknum = if t2.tracknum <> 0 then t2.tracknum else t1.tracknum;
        genre = if t2.genre <> 0xFF then t2.genre else t1.genre }
        
    let no_tag = {title = ""; artist = ""; album = ""; year = "";
                   comment = ""; tracknum = 0; genre = 0xFF }
  end



module Id3v2 = struct

  type tag = (string * string) list

  let unsynchronization = ref false
  let last_byte_read = ref 0

  let input_byte ic =
    let b = Pervasives.input_byte ic in
    let b =
      if b = 0 && !unsynchronization && !last_byte_read = 0xFF
      then Pervasives.input_byte ic
      else b in
    last_byte_read := b;
    b

  let input_buffer ic len =
    let buff = String.create len in
    for i = 0 to len - 1 do
      buff.[i] <- Char.chr (input_byte ic)
    done;
    buff

  let input_int4 ic =
    let b4 = input_byte ic in let b3 = input_byte ic in
    let b2 = input_byte ic in let b1 = input_byte ic in
    (b4 lsl 24) lor (b3 lsl 16) lor (b2 lsl 8) lor b1

  let skip_bytes ic n =
    for i = 1 to n do ignore(input_byte ic) done

  let valid_header header =
       Bytes.sub header 0 3 = Bytes.of_string "ID3"
    && (Char.code (Bytes.get header 3) = 3 || Char.code (Bytes.get header 3) = 4)
    && Char.code (Bytes.get header 5) land 0b00111111 = 0
    && Char.code (Bytes.get header 6) land 0b10000000 = 0
    && Char.code (Bytes.get header 7) land 0b10000000 = 0
    && Char.code (Bytes.get header 8) land 0b10000000 = 0
    && Char.code (Bytes.get header 9) land 0b10000000 = 0

  let length_header header =
    ((Char.code (Bytes.get header 6) lsl 21) lor
     (Char.code (Bytes.get header 7) lsl 14) lor
     (Char.code (Bytes.get header 8) lsl 7) lor
     (Char.code (Bytes.get header 9)))

  let decode_framedata id data =
    if id = "TXXX" then begin
      if data.[0] <> '\000' then raise Not_found;
      let datapos = 1 + String.index_from data 1 '\000' in
      Mp3_misc.chop_whitespace data datapos
    end else if id.[0] = 'T' then begin
      if data.[0] <> '\000' then raise Not_found;
      Mp3_misc.chop_whitespace data 1
    end else
      data

  let read_channel ic =
    try
      let header = Bytes.create 10 in
      really_input ic header 0 10;
      if not (valid_header header) then raise Not_found;
      let len = length_header header in
      let startpos = pos_in ic in
      (* Record use of unsynchronization *)
      unsynchronization := ((Char.code (Bytes.get header 5) land 0b10000000) <> 0);
      last_byte_read := 0;
      (* Skip extended header if present *)
      if Char.code (Bytes.get header 5) land 0b01000000 <> 0 then
        skip_bytes ic (input_int4 ic);
      (* Collect frames *)
      let tags = ref [] in
      while pos_in ic < startpos + len do
        let frameid = (Bytes.to_string (input_buffer ic 4)) in
        let framelen = input_int4 ic in
        let flags1 = input_byte ic in
        let flags2 = input_byte ic in
        let framedata = (Bytes.to_string (input_buffer ic framelen)) in
        if flags1 land 0b00011111 = 0 && flags2 = 0 then begin
          try
            tags := (frameid, decode_framedata frameid framedata) :: !tags
          with Not_found ->
            ()
        end
      done;
      List.rev !tags
    with x ->
      raise x

  let read_tag filename =
    let ic = open_in_bin filename in
    try
      let res = read_channel ic in
      close_in ic; res
    with x ->
      close_in ic; raise x

  let last_byte_written = ref 0

  let output_byte oc b =
    if !last_byte_written = 0xFF then begin
      if b = 0 || b land 0b11100000 = 0b11100000 then
        Pervasives.output_byte oc 0
    end;
    Pervasives.output_byte oc b;
    last_byte_written := b

  let output_int4 oc n =
    output_byte oc (n lsr 24);
    output_byte oc (n lsr 16);
    output_byte oc (n lsr 8);
    output_byte oc n

  let output_encoded_int4 oc n =
    Pervasives.output_byte oc (((n lsr 21) land 0x7F) lsl 24);
    Pervasives.output_byte oc (((n lsr 14) land 0x7F) lsl 16);
    Pervasives.output_byte oc (((n lsr 7) land 0x7F) lsl 8);
    Pervasives.output_byte oc (n land 0x7F)

  let output_string oc s =
    for i = 0 to String.length s - 1 do output_byte oc (Char.code s.[i]) done

  let output_frame oc (name, data) =
    assert (String.length name = 4);
    if name = "TXXX" then begin
      output_string oc name;                    (* tag *)
      output_int4 oc (String.length data + 10); (* length *)
      output_byte oc 0; output_byte oc 0;       (* null flags *)
      output_byte oc 0;                         (* this is ISO Latin1 *)
      output_string oc "Comment";               (* dummy name *)
      output_byte oc 0;                         (* end of name *)
      output_string oc data;                    (* data *)
      output_byte oc 0                          (* termination *)
    end else if name.[0] = 'T' then begin
      output_string oc name;                    (* tag *)
      output_int4 oc (String.length data + 2);  (* length *)
      output_byte oc 0; output_byte oc 0;       (* null flags *)
      output_byte oc 0;                         (* this is ISO Latin1 *)
      output_string oc data;                    (* data *)
      output_byte oc 0                          (* termination *)
    end else begin
      output_string oc name;                    (* tag *)
      output_int4 oc (String.length data);      (* length *)
      output_byte oc 0; output_byte oc 0;       (* null flags *)
      output_string oc data                     (* raw data *)
    end

  let append_data oc filename =
    let ic = open_in_bin filename in
    try
      begin try
        let header = Bytes.create 10 in
        really_input ic header 0 10;
        if not (valid_header header) then raise Not_found;
        seek_in ic (pos_in ic + length_header header)
      with Not_found | End_of_file ->
        seek_in ic 0
      end;
      let buffer = String.create 4096 in
      let rec copy_file () =
        let n = input ic buffer 0 (Bytes.length buffer) in
        if n = 0 then () else begin output oc buffer 0 n; copy_file () end in
      copy_file ();
      close_in ic
    with x ->
      close_in ic; raise x

  let write_tag ?src:srcname filename data =
    let origname =
      match srcname with
        None ->
          Unix2.rename filename (filename ^ ".bak"); filename ^ ".bak"
      | Some s -> s in
    try
      let oc = open_out_bin filename in
      begin try
        (* Output header *)
        output_string oc "ID3\003\000\128";
        let totalsize_pos = pos_out oc in
        output_encoded_int4 oc 0;
        last_byte_written := 0;
        (* Output frames *)
        List.iter (output_frame oc) data;
        (* Patch total size *)
        let end_pos = pos_out oc in
        seek_out oc totalsize_pos;
        output_encoded_int4 oc (end_pos - totalsize_pos - 4);
        seek_out oc end_pos;
        (* Append old file *)
        append_data oc origname
      with x ->
        close_out oc; raise x
      end;
      close_out oc;
      begin match srcname with
        None -> Sys.remove origname
      | Some s -> ()
      end
    with x ->
      begin match srcname with
        None -> Unix2.rename origname filename
      | Some s -> ()
      end;
      raise x

  let merge t1 t2 =
    t1 @ List.filter (fun (tag, data) -> not (List.mem_assoc tag t1)) t2
  
  let no_tag = []

end

let v2_to_v1 tags =
  { Id3v1.title =
      (try List.assoc "TIT2" tags with Not_found -> "");
    Id3v1.artist =
      (try List.assoc "TPE1" tags with Not_found ->
       try List.assoc "TPE2" tags with Not_found -> "");
    Id3v1.album =
      (try List.assoc "TALB" tags with Not_found -> "");
    Id3v1.year =
      (try List.assoc "TYEA" tags with Not_found -> "");
    Id3v1.comment =
      (try List.assoc "TXXX" tags with Not_found -> "");
    Id3v1.tracknum =
      (try int_of_string (List.assoc "TRCK" tags)
       with Not_found | Failure _ -> 0);
    Id3v1.genre =
      (try let g = List.assoc "TCON" tags in
           int_of_string (String.sub g 1 (String.length g - 2))
       with Not_found | Failure _ | Invalid_argument _ -> 0xFF)
  }

let v1_to_v2 t =
  let tags = ref [] in
  if t.Id3v1.genre <> 0xFF then
    tags := ("TCON", "(" ^ string_of_int t.Id3v1.genre ^ ")") :: !tags;
  if t.Id3v1.tracknum <> 0 then
    tags := ("TRCK", string_of_int t.Id3v1.tracknum) :: !tags;
  if t.Id3v1.comment <> "" then
    tags := ("TXXX", t.Id3v1.comment) :: !tags;
  if t.Id3v1.year <> "" && t.Id3v1.year <> "0" then
    tags := ("TYEA", t.Id3v1.year) :: !tags;
  if t.Id3v1.album <> "" then
    tags := ("TALB", t.Id3v1.album) :: !tags;
  if t.Id3v1.artist <> "" then
    tags := ("TPE1", t.Id3v1.artist) :: !tags;
  if t.Id3v1.title <> "" then
    tags := ("TIT2", t.Id3v1.title) :: !tags;
  !tags

let read_channel_both_v1 ic =
  let t2 =
    try v2_to_v1(Id3v2.read_channel ic) with Not_found -> Id3v1.no_tag in
  let t1 =
    try Id3v1.read_channel ic with Not_found -> Id3v1.no_tag in
  Id3v1.merge t2 t1

let read_channel_both_v2 ic =
  let t2 =
    try Id3v2.read_channel ic with Not_found -> Id3v2.no_tag in
  let t1 =
    try v1_to_v2(Id3v1.read_channel ic) with Not_found -> Id3v2.no_tag in
  Id3v2.merge t2 t1

let read_file_both_v1 filename =
  let ic = open_in_bin filename in
  try
    let res = read_channel_both_v1 ic in
    close_in ic; res
  with x ->
    close_in ic; raise x

let read_file_both_v2 filename =
  let ic = open_in_bin filename in
  try
    let res = read_channel_both_v2 ic in
    close_in ic; res
  with x ->
    close_in ic; raise x

let write_file_both_v1 ?src:srcname filename tag =
  Id3v2.write_tag ?src:srcname filename (v1_to_v2 tag);
  Id3v1.write_tag filename tag

let write_file_both_v2 ?src:srcname filename tag =
  Id3v2.write_tag ?src:srcname filename tag;
  Id3v1.write_tag filename (v2_to_v1 tag)

