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
  clean_cache_dir ();
  ignore (GMain.Timeout.add ~ms:3600000 ~callback:
    (fun _ ->
      clean_cache_dir ();
      true
  ))

let make_request url =
  let module H = Http_client in
  let proxy =
    if !!O.gtk_connection_http_use_proxy
      then Some (!!O.gtk_connection_http_proxy_server, !!O.gtk_connection_http_proxy_port)
      else None
  in
  let r = {
      H.basic_request with
      H.req_url = Url.of_string url;
      H.req_proxy = proxy;
      H.req_user_agent = "Mozilla/5.0 (Linux 2.4.19-16mdk i686; U) Opera 6.11  [en]";
      H.req_referer = None;
      H.req_request = H.GET;
    } in
  r

let file_from_url url =
  try
    if not (Sys.file_exists cache_dir) then Unix.mkdir cache_dir 0o755;
    let file = String.copy url in
    for i = 0 to String.length file - 1 do
      match file.[i] with
        '/'
      | '?' | '*' | '&' | ':' -> file.[i] <- '_'
      | _ -> ()
    done;
    Filename.concat cache_dir file
  with _ -> ""

let get url_name f =
  let r = make_request url_name in
  let module H = Http_client in
  let filename = file_from_url (Url.to_string r.H.req_url) in
  H.wget_string r (fun _s ->
    begin
      try
        (try Sys.remove filename with _ -> lprintf_nl2 "cannot remove file %s" filename);
        File.from_string filename _s;
      with _ -> (lprintf_nl2 "failed in H.wget_string")
    end;
    let in_chan = open_in_bin filename in
    let lexb = Lexing.from_channel in_chan in
    let dl = GuiNetHtml.parse_document
               ~return_declarations:true ~return_pis:true
               ~return_comments:true lexb
    in
    f dl;
    close_in in_chan) (fun n m -> lprintf_nl2 "progress: %d / %d" n m)


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

let parse_razorback_stats stats md4 referer dl =
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
                        (try Sys.remove filename with _ -> lprintf_nl2 "cannot remove file %s" filename);
                        File.from_string filename ___s;
                        stats.razorback_file_history <- filename
                      with _ -> (lprintf_nl2 "failed in H.wget_string")) (fun n m -> lprintf_nl2 "progress: %d / %d" n m)
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
                                stats.razorback_file_rating <- (GuiUtf8.utf8_of ___s)
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
                              stats.razorback_file_completed <- int_of_string _s
                            with _ -> ()
                          end else if !pos = 6
                          then begin
                            try
                              stats.razorback_file_avalaibility <- int_of_string _s
                            with _ -> ()
                          end

                ) ___dl
              in
              _iter __dl n;
              iter __dl
            end

        | Element (s, sl, __dl) ->
            iter __dl

        | _ -> ()

    ) _dl
  in
  iter dl

let get_razorback2_stats file =
  try
    let md4 = uids_to_md4 file.g_file_uids in
    let stats = {
      razorback_file_history      = "";
      razorback_file_rating       = "";
      razorback_file_avalaibility = 0;
      razorback_file_completed    = 0;
    } in
    file.g_file_razorback_stats <- Some stats;
    let referer = "http://stats.razorback2.com/" in
    let url = Printf.sprintf "%sed2khistory?ed2k=%s" referer md4 in
    get url (parse_razorback_stats stats md4 referer)
  with _ -> ()

(*
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

        Element: script
          type text/javascript
          src /sorttable3.js
            Data: 
        Data: 

    Data: 

    Element: body
        Data: 

        Element: script
          type text/javascript
            Data: <!-- 
google_ad_client = "pub-4061973295205334"; 
google_ad_width = 800; 
google_ad_height = 90; 
google_ad_format = "728x90_as"; 
google_ad_channel ="5618134257"; 
google_color_border = "A8DDA0"; 
google_color_bg = "EBFFED"; 
google_color_link = "0000CC"; 
google_color_url = "008000"; 
google_color_text = "6F6F6F"; 
//-->
        Data:  

        Element: center
            Element: script
              type text/javascript
              src http://pagead2.googlesyndication.com/pagead/show_ads.js
                Data:  

        Data: 

        Element: br
        Data: 

        Element: center
            Element: img
              src /images/8B/8B16AC1A76E4D1D750547931E8ADD3F7.png
              width 800
              height 288
              alt history graph for file ed2k::8B16AC1A76E4D1D750547931E8ADD3F7
        Data: 

        Element: br
        Element: form
          action /ed2khistory
          method get
            Data: 
File Hash : 
            Element: input
              name ed2k
              type text
              maxlength 200
              size 43
            Data: &nbsp;
            Element: input
              type submit
              value Submit
        Data: 
16063744 files in ed2khistory database
        Element: br
        Data: 

        Element: br
        Element: table
          border 1
          width 100%
          bgcolor #D8D8D8
          cellspacing 0
          id t1
          class sortable
            Data: 

            Element: colgroup
              align right
                Data: 

            Element: colgroup
              align right
                Data: 

            Element: colgroup
              align right
                Data: 

            Element: colgroup
              align center
                Data: 

            Element: colgroup
              align right
                Data: 

            Element: colgroup
              align right
                Data: 

            Element: thead
                Element: tr
                  bgcolor #B0B0B0
                    Data:  
                    Element: th
                        Data: Server
                    Element: th
                        Data: File Name
                    Element: th
                        Data: Size
                    Element: th
                        Data: Rating
                    Element: th
                        Data: Complete
                    Element: th
                        Data: Avail

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

            Element: tbody
              align right
                Data: 

                Element: tr
                    Element: td
                      title &lt;&lt;&lt; Saugcenter &gt;&gt;&gt;
                        Data: 80.190.233.144:6565
                    Element: td
                        Data: eMule0.46c-Installer.exe
                    Element: td
                        Data: 4577316
                    Element: td
                        Data: &nbsp;
                    Element: td
                        Data: 653
                    Element: td
                        Data: 655

                Element: tr
                    Element: td
                      title Breizh Punisher's
                        Data: 213.251.133.129:6567
                    Element: td
                        Data: eMule0.46c-Installer.exe
                    Element: td
                        Data: 4577316
                    Element: td
                        Data: &nbsp;
                    Element: td
                        Data: 184
                    Element: td
                        Data: 186

                Element: tr
                    Element: td
                      title !-= www.FreeSexBay.com =-!
                        Data: 83.149.102.1:4242
                    Element: td
                        Data: eMule0.46c-Installer.exe
                    Element: td
                        Data: 4577316
                    Element: td
                        Data: &nbsp;
                    Element: td
                        Data: 119
                    Element: td
                        Data: 120

                Element: tr
                    Element: td
                      title eD2k Infinity
                        Data: 213.251.161.152:4661
                    Element: td
                        Data: emule0.46c-installer.exe
                    Element: td
                        Data: 4577316
                    Element: td
                        Data: &nbsp;
                    Element: td
                        Data: 37
                    Element: td
                        Data: 37

                Element: tr
                    Element: td
                      title Breizh Digitalus
                        Data: 213.186.47.84:4661
                    Element: td
                        Data: eMule0.46c-Installer.exe
                    Element: td
                        Data: 4577316
                    Element: td
                      align center
                        Element: img
                          src /gifs/FileRating5.gif
                          alt Excellent
                          title Excellent for 0% of clients
                    Element: td
                        Data: 500
                    Element: td
                        Data: 504

                Element: tr
                    Element: td
                      title Em Server No.1
                        Data: 193.138.230.251:4242
                    Element: td
                        Data: eMule0.46c-Installer.exe
                    Element: td
                        Data: 4577316
                    Element: td
                        Data: &nbsp;
                    Element: td
                        Data: 144
                    Element: td
                        Data: 146

                Element: tr
                    Element: td
                      title BiG BanG 9
                        Data: 80.239.200.108:3000
                    Element: td
                        Data: eMule0.46c_Installer.exe
                    Element: td
                        Data: 4577316
                    Element: td
                        Data: &nbsp;
                    Element: td
                        Data: 400
                    Element: td
                        Data: 400

                Element: tr
                    Element: td
                      title !-= www.FreeOsex.com =-!
                        Data: 83.149.123.189:4321
                    Element: td
                        Data: eMule0.46c-Installer.exe
                    Element: td
                        Data: 4577316
                    Element: td
                        Data: &nbsp;
                    Element: td
                        Data: 100
                    Element: td
                        Data: 100

                Element: tr
                    Element: td
                      title &lt;&lt;&lt; Trafficservice24.de  &gt;&gt;&gt;
                        Data: 81.169.143.167:7777
                    Element: td
                        Data: eMule0.46c-Installer.exe
                    Element: td
                        Data: 4577316
                    Element: td
                        Data: &nbsp;
                    Element: td
                        Data: 93
                    Element: td
                        Data: 94

                Element: tr
                    Element: td
                      title !!!****ifreeex.net &gt;&gt;Hardcore XXX Movies !!!***
                        Data: 194.30.160.81:4661
                    Element: td
                        Data: eMule0.46c-Installer.exe
                    Element: td
                        Data: 4577316
                    Element: td
                        Data: &nbsp;
                    Element: td
                        Data: 96
                    Element: td
                        Data: 96

                Element: tr
                    Element: td
                      title Rackbox 2
                        Data: 213.251.161.69:4661
                    Element: td
                        Data: eMule0.46c-Installer.exe
                    Element: td
                        Data: 4577316
                    Element: td
                        Data: &nbsp;
                    Element: td
                        Data: 73
                    Element: td
                        Data: 73

                Element: tr
                    Element: td
                      title &#65403;FPRO&#65387;
                        Data: 213.251.161.103:4661
                    Element: td
                        Data: eMule0.46c-Installer.exe
                    Element: td
                        Data: 4577316
                    Element: td
                        Data: &nbsp;
                    Element: td
                        Data: 153
                    Element: td
                        Data: 154

                Element: tr
                    Element: td
                      title Asyrix 2.1
                        Data: 80.190.251.50:4321
                    Element: td
                        Data: eMule0.46c-Installer.[content.emule-project.net].exe
                    Element: td
                        Data: 4577316
                    Element: td
                        Data: &nbsp;
                    Element: td
                        Data: 536
                    Element: td
                        Data: 536

                Element: tr
                  bgcolor #D0B0B0
                    Element: td
                      title Razorback 2.1
                        Data: 195.245.244.244:3000
                    Element: td
                        Data: eMule0.46c-Installer.exe
                    Element: td
                        Data: 4577316
                    Element: td
                        Data: &nbsp;
                    Element: td
                        Data: 645
                    Element: td
                        Data: 646

                Element: tr
                    Element: td
                      title ### DOINGDO ###
                        Data: 212.112.238.82:4661
                    Element: td
                        Data: eMule0.46c-Installer.exe
                    Element: td
                        Data: 4577316
                    Element: td
                        Data: &nbsp;
                    Element: td
                        Data: 155
                    Element: td
                        Data: 156

                Element: tr
                    Element: td
                      title BiG BanG 8
                        Data: 80.239.200.107:3000
                    Element: td
                        Data: eMule0.46c-Installer.exe
                    Element: td
                        Data: 4577316
                    Element: td
                        Data: &nbsp;
                    Element: td
                        Data: 159
                    Element: td
                        Data: 160

                Element: tr
                    Element: td
                      title BiG BanG 10
                        Data: 80.239.200.109:3000
                    Element: td
                        Data: eMule0.46c-Installer.exe
                    Element: td
                        Data: 4577316
                    Element: td
                        Data: &nbsp;
                    Element: td
                        Data: 123
                    Element: td
                        Data: 123

                Element: tr
                    Element: td
                      title !-= www.FreeOsex.com =-!
                        Data: 83.149.123.188:4321
                    Element: td
                        Data: eMule0.46c-Installer.exe
                    Element: td
                        Data: 4577316
                    Element: td
                        Data: &nbsp;
                    Element: td
                        Data: 85
                    Element: td
                        Data: 85

                Element: tr
                    Element: td
                      title BiG BanG 6
                        Data: 80.239.200.105:3000
                    Element: td
                        Data: eMule0.46c-Installer.exe
                    Element: td
                        Data: 4577316
                    Element: td
                        Data: &nbsp;
                    Element: td
                        Data: 30
                    Element: td
                        Data: 31

                Element: tr
                    Element: td
                      title ChezToff 2 (Serveur Fr)
                        Data: 213.251.136.83:4661
                    Element: td
                        Data: eMule0.46c-Installer.exe
                    Element: td
                        Data: 4577316
                    Element: td
                        Data: &nbsp;
                    Element: td
                        Data: 92
                    Element: td
                        Data: 93

                Element: tr
                    Element: td
                      title BiG BanG 11
                        Data: 80.239.200.110:3000
                    Element: td
                        Data: eMule0.46c-Installer.exe
                    Element: td
                        Data: 4577316
                    Element: td
                        Data: &nbsp;
                    Element: td
                        Data: 197
                    Element: td
                        Data: 197

                Element: tr
                    Element: td
                      title Asyrix 2.2
                        Data: 217.20.127.165:4321
                    Element: td
                        Data: eMule0.46c-Installer.exe
                    Element: td
                        Data: 4577316
                    Element: td
                        Data: &nbsp;
                    Element: td
                        Data: 15
                    Element: td
                        Data: 16

                Element: tr
                    Element: td
                      title Byte Devils
                        Data: 194.213.0.20:3306
                    Element: td
                        Data: eMule0.46c [par Ratiatum.com].exe
                    Element: td
                        Data: 4577316
                    Element: td
                        Data: &nbsp;
                    Element: td
                        Data: 63
                    Element: td
                        Data: 64

                Element: tr
                    Element: td
                      title www.UseNeXT.to
                        Data: 212.112.243.146:4661
                    Element: td
                        Data: eMule0.46c-Installer.exe
                    Element: td
                        Data: 4577316
                    Element: td
                        Data: &nbsp;
                    Element: td
                        Data: 53
                    Element: td
                        Data: 53

                Element: tr
                    Element: td
                      title ChezToff (Serveur Fr)
                        Data: 213.186.60.106:4661
                    Element: td
                        Data: eMule0.46c-Installer.exe
                    Element: td
                        Data: 4577316
                    Element: td
                        Data: &nbsp;
                    Element: td
                        Data: 1275
                    Element: td
                        Data: 1278

                Element: tr
                    Element: td
                      title !!!****ifreeex.net &gt;&gt;Hardcore XXX Movies !!!***
                        Data: 194.30.160.41:4661
                    Element: td
                        Data: eMule0.46c-Installer.exe
                    Element: td
                        Data: 4577316
                    Element: td
                        Data: &nbsp;
                    Element: td
                        Data: 225
                    Element: td
                        Data: 228

                Element: tr
                    Element: td
                      title BiG BanG 4
                        Data: 80.239.200.103:3000
                    Element: td
                        Data: eMule0.46c-Installer.exe
                    Element: td
                        Data: 4577316
                    Element: td
                        Data: &nbsp;
                    Element: td
                        Data: 143
                    Element: td
                        Data: 146

                Element: tr
                    Element: td
                      title Jibsworld (www.jd2k.com)
                        Data: 213.186.45.91:4661
                    Element: td
                        Data: eMule0.46c-Installer.exe
                    Element: td
                        Data: 4577316
                    Element: td
                        Data: &nbsp;
                    Element: td
                        Data: 51
                    Element: td
                        Data: 51

                Element: tr
                    Element: td
                      title Jibsworld2 (www.jd2k.com)
                        Data: 213.251.162.35:4661
                    Element: td
                        Data: eMule0.46c-Installer.exe
                    Element: td
                        Data: 4577316
                    Element: td
                        Data: &nbsp;
                    Element: td
                        Data: 86
                    Element: td
                        Data: 86

                Element: tr
                    Element: td
                      title ChezToff 3 (Serveur Fr)
                        Data: 213.251.134.191:4661
                    Element: td
                        Data: Emule0.46C-Installer.exe
                    Element: td
                        Data: 4577316
                    Element: td
                        Data: &nbsp;
                    Element: td
                        Data: 217
                    Element: td
                        Data: 217

                Element: tr
                    Element: td
                      title Byte Devils
                        Data: 194.213.0.10:3306
                    Element: td
                        Data: eMule0.46c-Installer.exe
                    Element: td
                        Data: 4577316
                    Element: td
                        Data: &nbsp;
                    Element: td
                        Data: 10
                    Element: td
                        Data: 10

                Element: tr
                    Element: td
                      title DonkeyServer No1
                        Data: 62.241.53.2:4242
                    Element: td
                        Data: eMule0.46c-Installer.exe
                    Element: td
                        Data: 4577316
                    Element: td
                        Data: &nbsp;
                    Element: td
                        Data: 2585
                    Element: td
                        Data: 2597

                Element: tr
                    Element: td
                      title &lt;&lt;&lt; Saugcenter &gt;&gt;&gt;
                        Data: 212.112.241.158:6565
                    Element: td
                        Data: eMule0.46c-Installer.exe
                    Element: td
                        Data: 4577316
                    Element: td
                        Data: &nbsp;
                    Element: td
                        Data: 101
                    Element: td
                        Data: 102

                Element: tr
                    Element: td
                      title BiG BanG 2
                        Data: 80.239.200.101:3000
                    Element: td
                        Data: eMule0.46c-Installer.exe
                    Element: td
                        Data: 4577316
                    Element: td
                        Data: &nbsp;
                    Element: td
                        Data: 132
                    Element: td
                        Data: 132

                Element: tr
                    Element: td
                      title Byte Devils
                        Data: 194.213.0.30:3306
                    Element: td
                        Data: eMule0.46c-Installer.exe
                    Element: td
                        Data: 4577316
                    Element: td
                        Data: &nbsp;
                    Element: td
                        Data: 66
                    Element: td
                        Data: 66

                Element: tr
                    Element: td
                      title BiG BanG 3
                        Data: 80.239.200.102:3000
                    Element: td
                        Data: eMule0.46c-Installer.exe
                    Element: td
                        Data: 4577316
                    Element: td
                        Data: &nbsp;
                    Element: td
                        Data: 106
                    Element: td
                        Data: 106

                Element: tr
                    Element: td
                      title BiG BanG 5
                        Data: 80.239.200.104:3000
                    Element: td
                        Data: eMule0.46c-Installer.exe
                    Element: td
                        Data: 4577316
                    Element: td
                        Data: &nbsp;
                    Element: td
                        Data: 114
                    Element: td
                        Data: 114

                Element: tr
                    Element: td
                      title maxx1462
                        Data: 84.150.43.187:9955
                    Element: td
                        Data: eMule0.46c-Installer.exe
                    Element: td
                        Data: 4577316
                    Element: td
                        Data: &nbsp;
                    Element: td
                        Data: 1
                    Element: td
                        Data: 1

                Element: tr
                    Element: td
                      title ADSL Group
                        Data: 217.91.58.88:7000
                    Element: td
                        Data: eMule0.46c-Installer.exe
                    Element: td
                        Data: 4577316
                    Element: td
                        Data: &nbsp;
                    Element: td
                        Data: 5
                    Element: td
                        Data: 5

        Data: 

        Element: p
            Data: Legend :
        Element: ul
        Element: p
Element: li
    Data: Avail : The number of clients connected to the server sharing this file
    Element: p
Element: li
    Data: Complete : The number of clients connected to the server sharing this file and that have the complete file
    Element: p
Element: li
    Data: asked/h : The number of requests per hour received by razorback asking for sources. That gives an indication on the number of clients that are downloading this file
    Element: p
Element: li
    Data: File rating is a score given by a client to a file, between 1 (Very bad) to 5 (Excellent). Servers then give you the average of those scores.
    Element: br
    Element: ul
        Element: li
            Element: img
              src /gifs/FileRating1.gif
              alt Very bad
              title Invalid / Corrupt / Fake
            Data:  : Means majority of clients said the file was a fake, or corrupt, or invalid
            Element: br
        Element: li
            Element: img
              src /gifs/FileRating2.gif
              alt Poor
              title Poor
            Data:  : Means majority of clients said the file was of poor quality
            Element: br
        Element: li
            Element: img
              src /gifs/FileRating3.gif
              alt Fair
              title Fair
            Data:  : Fair quality
            Element: br
        Element: li
            Element: img
              src /gifs/FileRating4.gif
              alt Good
              title Good
            Data:  : Good
            Element: br
        Element: li
            Element: img
              src /gifs/FileRating5.gif
              alt Excellent
              title Excellent
            Data:  : Excellent
            Element: br
    Data: 

    Data: 

*)
