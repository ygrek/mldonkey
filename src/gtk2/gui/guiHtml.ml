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
open Options
open GuiNetHtml
open CommonTypes
open GuiTypes2

open Md4

module O = GuiOptions
module M = GuiMessages
module U = GuiUtf8
module Mi = GuiMisc

let cache_dir = Filename.concat GuiMessages.gui_config_dir "cache"

let clean_cache_dir () =
  let files = Sys.readdir cache_dir in
  Array.iter (fun basename ->
    let filename = Filename.concat cache_dir basename in
    try Sys.remove filename with _ -> ()
  ) files

let _ =
  Unix2.safe_mkdir cache_dir;
  Unix2.can_write_to_directory cache_dir;
  clean_cache_dir ()

let user_agent =
(* "Mozilla/5.0 (Linux 2.4.19-16mdk i686; U) Opera 6.11  [en]" *)
  Printf.sprintf "Mlgui/%s (%s) [en]" Autoconf.current_version Autoconf.build_system

let make_request url =
  let module H = Http_client in
  let auth = match !!O.gtk_connection_http_proxy_login with
  | "" -> None
  | _ -> Some (!!O.gtk_connection_http_proxy_login, !!O.gtk_connection_http_proxy_password)
  in
  let proxy =
    if !!O.gtk_connection_http_use_proxy
      then Some (!!O.gtk_connection_http_proxy_server, !!O.gtk_connection_http_proxy_port, auth)
      else None
  in
  let r = {
      H.basic_request with
      H.req_url        = Url.of_string url;
      H.req_proxy      = proxy;
      H.req_user_agent = user_agent;
      H.req_referer    = None;
      H.req_request    = H.GET;
      H.req_max_retry  = 1;                (* max 1 redirection for filedonkey.com *)
    } in
  r

let file_from_url url =
  try
    if not (Sys.file_exists cache_dir) then Unix.mkdir cache_dir 0o755;
    let file = Bytes.of_string url in
    for i = 0 to Bytes.length file - 1 do
      match Bytes.get file i with
        '/'
      | '?' | '*' | '&' | ':' -> Bytes.set file i '_'
      | _ -> ()
    done;
    Filename.concat cache_dir (Bytes.unsafe_to_string file)
  with _ -> ""

(*************************************************************************)
(*                                                                       *)
(*                         uids_to_md4                                   *)
(*                                                                       *)
(*************************************************************************)

let uids_to_md4 uids =
  let s = ref "" in
  begin
    try
      List.iter (fun uid ->
        match (Uid.to_uid uid) with
          Ed2k md4 -> (s := Md4.to_string md4; raise Exit)
        | _ -> ()
      ) uids
    with _ -> ()
  end;
  if !s <> ""
    then !s
    else raise Not_found

(*************************************************************************)
(*                                                                       *)
(*                         create_stat                                   *)
(*                                                                       *)
(*************************************************************************)

let create_stat file stat =
  let rec iter l =
    match l with
      [] ->
        begin
          let s = {
            stats_file_type         = stat;
            stats_file_history      = "";
            stats_file_rating       = "";
            stats_file_availability = 0;
            stats_file_completed    = 0;
          } in
          file.g_file_stats <- s :: file.g_file_stats;
          s
        end
    | s :: tail ->
        if s.stats_file_type = stat then s else iter tail
  in
  iter file.g_file_stats

(*************************************************************************)
(*                                                                       *)
(*                         get_stat                                      *)
(*                                                                       *)
(*************************************************************************)

let get_stat file stat =
  let rec iter l =
    match l with
      [] -> raise Not_found
    | s :: tail ->
        if s.stats_file_type = stat then s else iter tail
  in
  iter file.g_file_stats

(*************************************************************************)
(*                                                                       *)
(*                         remove_stat                                   *)
(*                                                                       *)
(*************************************************************************)

let remove_stat file stat =
  let l = List.filter (fun s -> s.stats_file_type <> stat) file.g_file_stats in
  file.g_file_stats <- l

(*************************************************************************)
(*                                                                       *)
(*                         get_html                                      *)
(*                                                                       *)
(*************************************************************************)

let get_html url_name f progress =
  let r = make_request url_name in
  let module H = Http_client in
  let filename = file_from_url (Url.to_string r.H.req_url) in
  H.wget_string r (fun _s ->
    begin
      try
        (try Sys.remove filename with _ -> ());
        File.from_string filename _s;
      with _ -> (lprintf_nl "failed in H.wget_string")
    end;
    Unix2.tryopen_read_bin filename (fun in_chan ->
      let lexb = Lexing.from_channel in_chan in
      let dl = GuiNetHtml.parse_document
        ~return_declarations:true ~return_pis:true
        ~return_comments:true lexb
      in
      f dl)) (fun n m -> progress n m)


(*************************************************************************)
(*                                                                       *)
(*                         get_xml                                       *)
(*                                                                       *)
(*************************************************************************)

let get_xml url_name f progress =
  let r = make_request url_name in
  let module H = Http_client in
  H.wget_string r (fun s ->
    let xml = Xml.parse_string s in
    f xml) (fun n m -> progress n m)

(*************************************************************************)
(*                                                                       *)
(*                         print_xml                                     *)
(*                                                                       *)
(*************************************************************************)

let print_xml xml =
  let buf = Buffer.create 1000 in
  let rec iter _xml indent =
    let s_indent = String.make indent ' ' in
    match _xml with
        Xml_types.Element (s, sl,xml_list) ->
          begin
            Buffer.add_string buf (Printf.sprintf "%sElement: %s\n" s_indent s);
            List.iter (fun (_s, __s) ->
              Buffer.add_string buf (Printf.sprintf "%s  %s=%s\n" s_indent _s __s);
            ) sl;
            List.iter (fun __xml -> iter __xml (indent + 4)) xml_list
          end
      | Xml_types.PCData s ->
          Buffer.add_string buf (Printf.sprintf "%sData: %s\n" s_indent s)
  in
  iter xml 0;
  Printf2.lprintf "%s\n" (Buffer.contents buf);
  flush stdout;
  Buffer.clear buf;
  Buffer.reset buf

(*************************************************************************)
(*                                                                       *)
(*                         parse_razorback_stats                         *)
(*                                                                       *)
(*************************************************************************)

let parse_razorback_stats stats md4 referer on_png_completed on_not_found progress dl =
  let not_found = Printf.sprintf "Sorry, the file ed2k::%s was not found on our database" md4 in
  let progress_desc = Printf.sprintf "http://stats.razorback2.com :: %s" !M.dT_lb_stats_get_image in
  let rec iter _dl =
    List.iter (fun d ->
      match d with
          Element (s, sl, __dl) when s = "img" ->
            begin
              List.iter (fun (_s, __s) ->
                if _s = "src" & String2.contains __s md4
                  then begin
                    let url_name = Filename.concat referer __s in
                    let basename = Filename2.basename __s in
                    let filename = Filename.concat cache_dir basename in
                    let r = make_request url_name in
                    let module H = Http_client in
                    H.wget_string r (fun ___s ->
                      try
                        (try Sys.remove filename with _ -> ());
                        File.from_string filename ___s;
                        stats.stats_file_history <- filename;
                        on_png_completed ();
                      with _ -> (lprintf_nl "failed in H.wget_string")) (fun n m -> progress progress_desc n m)
                  end
              ) sl;
              iter __dl
            end

        | Element (s, sl, __dl) when s = "tfoot" ->
            begin
              let n = ref 0 in
              let rec _iter ___dl pos =
                List.iter (fun _d ->
                  match _d with
                      Element (_s, _sl, ____dl) when _s = "img" ->
                        begin
                          List.iter (fun (__s, ___s) ->
                            if __s = "title"
                              then begin
                                stats.stats_file_rating <- (GuiUtf8.utf8_of ___s)
                              end
                          ) _sl;
                          _iter ____dl pos
                        end

                    | Element (_s, _sl, ____dl) when _s = "td" ->
                        begin
                          incr pos;
                          _iter ____dl pos
                        end

                    | Element (_s, _sl, ____dl) ->
                          _iter ____dl pos

                    | Data _s ->
                        if !pos = 5
                          then begin
                            try
                              stats.stats_file_completed <- int_of_string _s
                            with _ -> ()
                          end else if !pos = 6
                          then begin
                            try
                              stats.stats_file_availability <- int_of_string _s
                            with _ -> ()
                          end

                ) ___dl
              in
              _iter __dl n;
              iter __dl
            end

        | Element (s, sl, __dl) ->
            iter __dl

        | Data s ->
            begin
              if String2.contains s not_found
                then on_not_found md4
            end

    ) _dl
  in
  iter dl

(*************************************************************************)
(*                                                                       *)
(*                         get_razorback2_stats                          *)
(*                                                                       *)
(*************************************************************************)

(* **********************************

    Element: body
        Data: 
Sorry, the file ed2k:: was not found on our database

        Element: br

  ***********************************
Element: !
  contents DOCTYPE html public "-//W3C//DTD HTML 4.01 Transitional//EN"
Element: html
    Data: 

    Element: head
        Data: 

        Element: meta
          http-equiv content-type
          content text/html;charset=iso-8859-1
        Data: 

        Element: meta
          name keywords
          content edonkey emule ed2khistory razorback database fakes files uncomplete
        Data: 

        Element: meta
          name description
          content razorback history database of ed2k/emule files
        Data: 

        Element: title
            Data: ed2kHistory of file ed2k::8B16AC1A76E4D1D750547931E8ADD3F7
        Data:

..........

        Element: center
            Element: img
              src /images/8B/8B16AC1A76E4D1D750547931E8ADD3F7.png
              width 800
              height 288
              alt history graph for file ed2k::8B16AC1A76E4D1D750547931E8ADD3F7

..........

            Element: tfoot
              align right
                Element: tr
                  bgcolor #B0B0B0
                    Element: td
                        Element: font
                          size +2
                            Data: Total 39/
                            Element: a
                              href http://www.gruk.org/list.php
                                Data: 59
                    Element: td
                    Element: td
                    Element: td
                        Element: img
                          src /gifs/FileRating5.gif
                          alt Excellent
                          title Excellent (global rating 5, given by 1 clients (0%))
                    Element: td
                        Element: font
                          size +2
                            Data: 9822
                    Element: td
                        Element: font
                          size +2
                            Data: 9864
                        Data: 

*)

let get_razorback2_stats file on_png_completed on_not_found progress_html progress_png =
  let md4 = uids_to_md4 file.g_file_uids in
  let stat = create_stat file RazorBack in
  let referer = "http://stats.razorback2.com/" in
  let url = Printf.sprintf "%sed2khistory?ed2k=%s" referer md4 in
  let progress_desc = Printf.sprintf "http://stats.razorback2.com :: %s" !M.dT_lb_stats_get_main_page in
  get_html url (parse_razorback_stats stat md4 referer on_png_completed on_not_found (progress_html md4)) (progress_png md4 progress_desc)

(*************************************************************************)
(*                                                                       *)
(*                         parse_filedonkey_stats                        *)
(*                                                                       *)
(*************************************************************************)

let parse_filedonkey_stats stat md4 on_completed on_not_found dl =
  let pos = ref 0 in
  let search_succeeded = ref true in
  let rec iter _dl =
    List.iter (fun d ->
      match d with
        Element (s, sl, __dl) ->
          begin
            iter __dl
          end
      | Data s ->
          if String2.contains s "your search has failed"
          then (search_succeeded := false; on_not_found md4)
          else if String2.contains s "Availability"
            then pos := 1
            else if String2.contains s "Complete Sources"
              then pos := 2
              else if !pos = 1
                then begin
                  try
                    stat.stats_file_availability <- int_of_string s;
                    pos := 0
                  with _ -> pos := 0
                end else if !pos = 2
                  then begin
                    try
                      stat.stats_file_completed <- int_of_string s;
                      pos := 0
                    with _ -> pos := 0
                  end
    ) _dl
  in
  iter dl;
  if !search_succeeded then on_completed ()

(*************************************************************************)
(*                                                                       *)
(*                         get_filedonkey_stats                          *)
(*                                                                       *)
(*************************************************************************)

(*
http://www.filehash.com/url/F3BC3066A1C6993C32714E0AB4152CDB
Element: html
  xmlns http://www.w3.org/1999/xhtml
    Data:   
    Element: head
        Element: title
            Data: ed2k URL for file "mldonkey-scripts.tgz"

.........

                            Element: p
                                Data: ed2k: 
                                Element: a
                                  href ed2k://|file|mldonkey-scripts [found via www.FileDonkey.com].tgz|1500|91DF9F63F18218CDE012714D59BC8A50|/
                                    Data:  mldonkey-scripts.tgz
                            Element: p
                            Element: p
                                Data: Availability: 
                                Element: b
                                    Data: 06
                            Element: p
                                Data: Size: 
                                Element: b
                                    Data: 1500
                            Element: p
                                Data: Complete Sources: 
                                Element: b
                                    Data: 2
                            Element: p
                                Data:  
                            Element: p
                            Element: p
                            Element: p
                                Data: Also Known As:
                            Element: p
                                Data:  
                                Element: b
                                    Data: mldonkey-scripts.tar.gz
                            Element: p
                                Data:  
                            Element: p
                            Element: p
                                Data: 
*)

let get_filedonkey_stats file on_completed on_not_found progress_html =
  let md4 = uids_to_md4 file.g_file_uids in
  let stat = create_stat file FileDonkey in
  let referer = "http://www.filedonkey.com/" in
  let url = Printf.sprintf "%surl/%s" referer md4 in
  let progress_desc = Printf.sprintf "http://www.filedonkey.com/ :: %s" !M.dT_lb_stats_get_main_page in
  get_html url (parse_filedonkey_stats stat md4 on_completed on_not_found) (progress_html md4 progress_desc)


(*************************************************************************)
(*                                                                       *)
(*                         parse_isohunt_stats                           *)
(*                                                                       *)
(*************************************************************************)

let parse_hisohunt_desc stat s file_size found_title found_desc =
  let file_size = Mi.size_of_int64 file_size in
  let len = String.length s in
  if len > 0
    then begin
      let pos = String2.search_from s 0 "Size: " in
      let pos = pos + 6 in
      let s = String.sub s pos (len - pos) in
      let pos = String2.search_from s 0 "<br>" in
      let size = Bytes.unsafe_of_string @@ String.sub s 0 pos in
      Bytes.set size (pos - 1) 'o';
      let size = Bytes.unsafe_to_string size in
      let len = String.length s in
      let pos = String2.search_from s 0 "Seeds: " in
      let pos = pos + 7 in
      let s = String.sub s pos (len - pos) in
      let pos = String2.search_from s 0 "<br>" in
      let completed = String.sub s 0 pos in
      let len = String.length s in
      let pos = String2.search_from s 0 "Leechers: " in
      let pos = pos + 10 in
      let s = String.sub s pos (len - pos) in
      let pos = String2.search_from s 0 "<br>" in
      let availability = String.sub s 0 pos in
      let len = String.length s in
      let pos = String2.search_from s 0 "Downloads:" in
      let pos = pos + 11 in
      let s = String.sub s pos (len - pos) in
      let pos = String2.search_from s 0 "<p>" in
      let downloads = String.sub s 0 pos in
      let size = Mi.int64_of_size size in
      let size = Mi.size_of_int64 size in
      if size = file_size
        then begin
          stat.stats_file_completed <- int_of_string completed;
          stat.stats_file_availability <- int_of_string availability;
          stat.stats_file_rating <- (Printf.sprintf "[Downloads: %s]" downloads);
          raise Exit
        end else begin
          found_title := false;
          found_desc := false
        end
   end else begin
     found_title := false;
     found_desc := false
   end

let parse_isohunt_stats stat file_name file_size on_completed on_not_found xml =
  let found_title = ref false in
  let found_desc = ref false in
  let rec iter _xml =
    match _xml with
        Xml_types.Element (s, sl,xml_list) when s = "description" ->
            (if !found_title then found_desc := true);
            List.iter (fun __xml -> iter __xml) xml_list

      | Xml_types.Element (s, sl,xml_list) ->
            List.iter (fun __xml -> iter __xml) xml_list

      | Xml_types.PCData s ->
          if s = file_name
           then found_title := true
           else if !found_desc then parse_hisohunt_desc stat s file_size found_title found_desc
  in
  try
    iter xml;
    if !found_title && !found_desc
      then on_completed ()
      else on_not_found file_name
  with
    Exit ->
      begin
        if !found_title && !found_desc
        then on_completed ()
        else on_not_found file_name
      end
  | _ -> on_not_found file_name

(*************************************************************************)
(*                                                                       *)
(*                         get_isohunt_stats                             *)
(*                                                                       *)
(*************************************************************************)

let isohunt_normalize_string str =
  let s = String.copy str in
  String2.replace s ' ' "+"

let get_isohunt_stats file on_completed on_not_found progress_xml =
  let file_name = file.g_file_name in
  let req = isohunt_normalize_string file_name in
  let stat = create_stat file IsoHunt in
  let referer = "http://isohunt.com/" in
  let url = Printf.sprintf "%sjs/rss.php?ihq=%s&op=and&iht=/" referer req in
  let progress_desc = Printf.sprintf "http://isohunt.com/ :: %s" !M.dT_lb_stats_get_main_page in
  get_xml url (parse_isohunt_stats stat file_name file.g_file_size on_completed on_not_found) (progress_xml file_name progress_desc)
