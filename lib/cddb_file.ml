open Cddb_lexer

let rec strncmp_aux s1 s2 pos len =
  pos >= len || ( 
    s1.[pos] = s2.[pos] && strncmp_aux s1 s2 (pos+1) len)

let strncmp s1 s2 len =
  String.length s1 >= len && String.length s2 >= len &&
  strncmp_aux s1 s2 0 len

let read_lines filename =
  let s = File.to_string filename in
  let lexbuf = Lexing.from_string s in
  let rec iter list = 
    try
      let value = Cddb_lexer.line lexbuf in
      if value = EOF then list else
        iter (value :: list)
    with
      e ->
        Printf.printf "Exception %s at pos %d, file %s"
          (Printexc2.to_string e) (Lexing.lexeme_start lexbuf)
        filename; print_newline (); exit 0
  in
  let list = iter [] in
  let list = List.rev list in
  (*
  List.iter (fun (s,v) ->
      Printf.printf "%s = %s" s v; print_newline ();
) list;
  *)
  list

  (*
type t = {
    id : string;
    author : string;
    album : string;
    year : string option;
    genre : string option;
    titles : (string * int) list;
    info : string list;
  }

let dummy = {
    id = "";
    author = "";
    album = "";
    year = None;
    genre = None;
    titles = [];
    info = [];
  }
  
let read filename =
  let lines = read_lines filename in
  let rec iter_before lines frames =
    match lines with
      [] -> failwith "Not enough track info"
    | (name, value) :: lines ->
        if name = "DISCLENGTH" then
          let end_frame = (int_of_string value) * 75 in
          let ntracks = List.length frames in
          let rec iter_frames list end_frame lengths =
            match list with
              [] -> lengths
            | frame :: tail ->
                iter_frames tail frame 
                  (( (end_frame - frame) / 75) :: lengths)
          in
          let lengths = iter_frames frames end_frame [] in
          let array = Array.of_list lengths in
          let array = Array.map (fun l -> "", l) array in
          iter_after lines { dummy with titles = array }
        else
        if name = "TRACKPOS" then
          iter_before lines ((int_of_string value) :: frames)
        else
          failwith (Printf.sprintf "Unexpected key %s" name)
          
  and iter_after lines t =
    match lines with
      [] -> t
    | (name, value) :: tail ->
        iter_after tail (
          match name with
            "DISCID" -> { t with id = value }
          | "DTITLE" ->
              let pos = try
                  String.index value '/' with
                  _ -> failwith "Can't find author" in
              let author = String.sub value 0 pos in
              let album = String.sub value (pos+1)
                (String.length value - (pos + 1)) in
              { t with album = album; author = author }
          | "DYEAR" ->
              if value = "" then t else { t with year = Some value }
          | "DGENRE" ->
              if value = "" then t else { t with genre = Some value }
          | _ ->  
              let s = Marshal.to_string t [] in
              Printf.printf "len %d" (String.length s); print_newline ();
              failwith (Printf.sprintf "Unknown key %s" name)
        )
  in
  let _ = iter_before lines [] in
  ()
  
  *)

let read filename =
  read_lines filename
  