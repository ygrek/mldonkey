open Mp3tag
  
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
      Printf.printf "Filename %s is already tagged" filename; 
      print_newline ();  
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
                  Printf.printf "Artist set to %s" tags.artist;
                  print_newline ();
            );
            (match !ptitle with
                None -> ()
              | Some i ->
                  tags.title <- Str.matched_group i name;
                  Printf.printf "Title set to %s" tags.title;
                  print_newline ();
            );
            (match !palbum with
                None -> ()
              | Some i ->
                  tags.album <- Str.matched_group i name;
                  Printf.printf "Album set to %s" tags.album;
                  print_newline ();
            );
            (match !ptrack with
                None -> ()
              | Some i ->
                  tags.tracknum <- int_of_string (Str.matched_group i name);
                  Printf.printf "Tracknum set to %d" tags.tracknum;
                  print_newline ();
            );
          end
        else
          (Printf.printf "%s doesn't match regexp" filename;
            print_newline () )
  end;
  Printf.printf "TAGS:"; print_newline ();
  Printf.printf "Title: %s" tags.title; print_newline ();
  Printf.printf "Artist: %s" tags.artist; print_newline ();
  Printf.printf "Album: %s" tags.album; print_newline ();
  Printf.printf "Year: %s" tags.year; print_newline ();
  Printf.printf "Track: %d" tags.tracknum; print_newline ();
  
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
  
  
  
  Printf.printf "Proposed name: %s" new_name; print_newline ();
  if !save then begin
      Unix2.safe_mkdir (Filename.dirname new_name);
      Sys.rename filename new_name;
    end;
  ()
  
let _ = 
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
    "-cddb", Arg.String Cddb_file.read, "";
  ] 
    set_filename_tags "usage"
  