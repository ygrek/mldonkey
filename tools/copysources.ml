open Printf2
open Sys;;
open Unix;;

let rec iter srcdir destdir =
  lprintf "iter: \n%s\n%s\n" srcdir destdir;
  let s = 
    try
      File.to_string (Filename.concat srcdir "CVS/Entries") 
    with _ -> 
        File.to_string (Filename.concat srcdir "cvs/Entries")
  in
  List.iter (fun line ->
      match String2.split line '/' with
        "D" :: directory :: _ ->
          lprintf "Directory [%s]\n" directory;
          let new_srcdir = Filename.concat srcdir directory in
          if file_exists new_srcdir then
            let new_destdir = Filename.concat destdir directory in
            mkdir new_destdir 0o755;
            iter new_srcdir new_destdir
      | "" :: file :: _ -> 
          lprintf "File [%s]\n" file;
          let full_srcfile = Filename.concat srcdir file in
          let full_destfile = Filename.concat destdir file in
          (*
Unix2.copy full_srcfile full_destfile;
*)
          ignore (command (Printf.sprintf "cp '%s' '%s'" 
              full_srcfile   full_destfile))
      | _ -> ()
  ) (String2.split_simplify s '\n')

let _ =
  ignore (command "rm -rf /tmp/mldonkey");
  mkdir "/tmp/mldonkey" 0o755;
  iter "." "/tmp/mldonkey"
