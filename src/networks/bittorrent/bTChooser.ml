(* Copyright 2003, Denis Fortin

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

open Int64ops
open Printf2
open BasicSocket
open List2
open Options

open CommonOptions
open CommonTypes

open BTRate
open BTTypes
open BTOptions
open BTGlobals

(*given some files choose the next uploaders based on their behavior
  Will choose max_uploaders_per_torrent uploaders for each file in list
  fun_comp is the function use to classify clients
*)
let choose_next_uploaders files fun_comp =
  let full_list = ref ([] : BTTypes.client list)
  and keepn orl l i =
    (*constructs a list of i items max with all
      orl items +  some l items *)
    let orig_num = List.length orl in
    if orig_num < i && i > 0 then
      let keep,rest = cut (i - orig_num) l in
      orl@keep,rest
    else
      orl,l
  in
  List.iter (fun f ->
      let max_list = ref ([] : BTTypes.client list) in
      (*all clients*)
      let possible_uploaders = ref ([] :  BTTypes.client list) in
      Hashtbl.iter (fun _ c ->
          begin
            possible_uploaders := (c::!possible_uploaders);
          end )  f.file_clients;
      if !verbose_upload then
          lprintf_nl "clients num %d as possible uploaders for file %s" (List.length !possible_uploaders) f.file_name;
      (*Interested clients with a connection*)
      let filtl = List.filter (fun c -> c.client_interested == true
            && (c.client_sock != NoConnection)
        ) !possible_uploaders in
      (*sort by biggest contributor*)
      let sortl = List.sort fun_comp filtl in
      (*add max_uploaders_per_torrent-1 from the best*)
      let to_add,next = keepn !max_list sortl (!!max_uploaders_per_torrent - 1) in
      max_list:= to_add;
      (* clients in optim are current optimistic uploaders (30 seconds) *)
      let optim,notoptim = List.partition ( fun a ->
            (Rate.ratesince a.client_upload_rate) > 0
              && a.client_last_optimist + 30 > last_time()
        ) next in
      let notoptim = List.sort (fun a b -> compare a.client_last_optimist b.client_last_optimist) notoptim in
      (*add at least one optimistic uploader*)
      let to_add,next =  keepn !max_list (optim) (!!max_uploaders_per_torrent) in
      max_list := to_add;
      (*fill up with not optimistic uploaders*)
      let to_add,_ = keepn !max_list (notoptim) (!!max_uploaders_per_torrent) in
      full_list := !full_list @ to_add;
      if !verbose_upload then
        begin
          lprintf_n "potential uploaders count: %d list: [" (List.length to_add);
          List.iter (fun cr ->
              let (ip,port) = cr.client_host in
                  lprintf " %s:%d" (Ip.to_string ip) port;
             ) to_add;
          lprintf " ]\n";
        end;

  ) files;
  !full_list


let choose_best_downloaders files =
  (*sort: left to download, then priority*)
  let files = List.stable_sort
    (fun a b -> compare
          ((file_size a) -- (match a.file_swarmer with 
                             | None -> Int64.zero 
                             | Some swarmer -> 
                                 CommonSwarming.downloaded swarmer))
          ((file_size b) -- (match b.file_swarmer with 
                             | None -> Int64.zero 
                             | Some swarmer -> 
                                 CommonSwarming.downloaded swarmer))
        ) files in
  let files = List.stable_sort
       (fun a b -> compare
                    (CommonFile.file_priority (CommonFile.as_file b.file_file))
                    (CommonFile.file_priority (CommonFile.as_file a.file_file))
                    ) files in
  (*use sort function that puts the client we download best from on top*)
  choose_next_uploaders files (fun a b -> Rate.compare b.client_downloaded_rate
        a.client_downloaded_rate)


let choose_best_uploaders files =
  (*use sort function that puts the client we upload best to on top*)
  choose_next_uploaders files (fun a b -> Rate.compare b.client_upload_rate
        a.client_upload_rate)


let choose_uploaders files =
  (*list of new uploaders from the files we download and the files we seed*)
  let next_uploaders =
       ( (choose_best_downloaders (List.filter
              (fun f ->  file_state f = FileDownloading )
              !current_files ))
       @ (choose_best_uploaders (List.filter
              (fun f ->  file_state f = FileShared )
              !current_files ))
       ) in
  (*do some debug output and choose the first max_bt_uploaders from the list*)
  begin
    if !verbose_upload then
        begin
          lprintf_n "next_uploaders: %d list: [" (List.length next_uploaders);
          List.iter (fun cr ->
              let (ip,port) = cr.client_host in
                  lprintf " %s:%d" (Ip.to_string ip) port;
             ) next_uploaders;
          lprintf " ]\n";
        end;
    if (List.length next_uploaders) > !!max_bt_uploaders then
        let keep,rest = List2.cut !!max_bt_uploaders next_uploaders in
        begin
          if !verbose_upload then
             begin
               lprintf_n "cut next_uploaders: %d list: [" (List.length keep);
               List.iter (fun cr ->
                    let (ip,port) = cr.client_host in
                        lprintf " %s:%d" (Ip.to_string ip) port;
                   ) keep;
               lprintf " ]\n";
             end;
          keep
        end
    else next_uploaders
  end
