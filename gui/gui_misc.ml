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

open CommonTypes
(** Misc. functions. *)
module O = Gui_options
module G = Gui_global


open GuiProto

let ko = Int32.of_int 1024
  
let unit_of_string s =
  match String.lowercase s with
    "mo" -> Int32.mul ko ko
  | "ko" -> ko
  | _ -> Int32.one

let ko = 1024.0
let mo = ko *. ko
let go = mo *. ko


(*
let option_of_string s =
  if s = "" then None else Some s
  
let submit_search (gui: gui) local ()=
  let module P = GuiProto in
  incr search_counter;
  let search_num = !search_counter in
  current_search := !search_counter;
  nresults := 0;
  update_searches_label ();
  let s = gui#tab_searches in
  let q = match s#entry_search_words#text with
      "" -> []
    | s -> [want_and_not (fun s -> QHasWord s) s] in
  let q = match s#entry_search_minsize#text with
      "" -> q
    | v -> 
        (QHasMinVal ("size", 
            Int32.mul (Int32.of_string v)
            (unit_of_string s#combo_search_minsize_unit#entry#text)
          )) :: q
  in
  let q = match s#entry_search_maxsize#text with
      "" -> q
    | v -> 
        (QHasMaxVal ("size", 
            Int32.mul (Int32.of_string v)
            (unit_of_string s#combo_search_maxsize_unit#entry#text)
          )) :: q
  in
  let q = match s#combo_format#entry#text with
      "" -> q
    | v ->
        let v = try List.assoc v  search_format_list with _ -> v in
        (want_comb_not or_comb
            (fun w -> QHasField("format", w)) v) :: q

  in
  let q = match s#combo_search_media#entry#text with
      "" -> q
    | v ->
        (QHasField("type", 
            try List.assoc v  search_media_list with _ -> v)) :: q

  in
  let q = match s#entry_title#text with
      "" -> q
    | v -> (want_comb_not and_comb (fun v -> QHasField("Title", v)) v):: q
  in
  let q = match s#entry_artist#text with
      "" -> q
    | v -> (want_comb_not and_comb (fun v -> QHasField("Artist", v)) v):: q
  in
  let q = match s#entry_album#text with
      "" -> q
    | v -> (want_comb_not and_comb (fun v -> QHasField("Album", v)) v) :: q
  in
  let q = match s#combo_min_bitrate#entry#text with
      "" -> q
    | v -> (QHasMaxVal("bitrate", Int32.of_string v)) :: q
  in
  match q with 
    [] -> ()
  | q1 :: tail ->
      let q = List.fold_left (fun q1 q2 ->
            QAnd (q1,q2)
        ) q1 tail in
  gui_send (P.Search_query (local, 
      { 
        P.search_max_hits = int_of_string s#combo_max_hits#entry#text;
        P.search_query = q;
        P.search_num = !search_counter;
      }));
  let new_tab = new box_search () in

  let clist_search = MyCList.create gui new_tab#clist_search_results       
      [
(* SIZE *)
      (fun r -> (Printf.sprintf "%10s" (Int32.to_string r.result_size)));
(* NAME *)
      (fun r -> (*short_name*) (first_name r)) ;
(* FORMAT *)      
      (fun r -> r.result_format);
(* TAGS *)
      (fun r -> string_of_tags r.result_tags);
(* MD4 *)
      (fun r -> Md4.to_string r.result_md4);
    ] 
    
  in
  MyCList.set_can_select_all clist_search;
  MyCList.set_size_callback clist_search (fun n ->
      if search_num = !current_search then begin
          nresults := n; update_searches_label ()
        end);
  MyCList.set_selected_callback clist_search (fun _ r ->
      tab_searches#label_file_comment#set_text (
        match r.result_comment with
          None -> ""
        | Some comment ->
            Printf.sprintf "%s COMMENT: %s" (first_name r) comment
      ));
  MyCList.set_context_menu clist_search search_make_menu;
  ignore (new_tab#button_search_download#connect#clicked 
      (search_download clist_search gui));
  ignore (new_tab#button_stop#connect#clicked 
      (search_stop clist_search gui !search_counter));
  ignore (new_tab#button_close#connect#clicked 
      (search_close clist_search gui !search_counter));
  let label_query = new_tab#label_query in
  Hashtbl.add searches !search_counter (clist_search, label_query);
  let n = add_search_page clist_search in
  tab_searches#notebook_results#append_page 
    ~tab_label:(GMisc.label ~text:(
      Printf.sprintf "Search %d" !search_counter) ())#coerce
    new_tab#coerce;
  tab_searches#notebook_results#goto_page n

let clean_gui _ =
  gui#label_connect_status#set_text "Not connected";
  MyCList.clear clist_servers;
  MyCList.clear clist_downloads;
  MyCList.clear clist_downloaded;
  MyCList.clear clist_friends;
  MyCList.clear clist_server_users;
  MyCList.clear clist_friend_files;
  MyCList.clear clist_file_locations;
  Hashtbl.clear locations;
  Hashtbl.clear searches;
  (let text = gui#tab_console#text in
    text#delete_text 0 (text#length));
  (let text = gui#tab_friends#text_dialog in
    text#delete_text 0 (text#length));
  nconnected_servers := 0;
  ndownloaded := 0;
  ndownloads := 0;
  current_file := None;
  current_friend := -1;
  update_server_label ();
  update_download_label ();
  ignore (update_current_file ())
  
let disconnect gui = 
  match !connection_sock with
    None -> ()
  | Some sock ->
      clean_gui ();
      TcpBufferedSocket.close sock "user close";
      connection_sock := None

let reconnect gui =
  (try disconnect gui with _ -> ());
  clean_gui ();
  let sock = TcpBufferedSocket.connect "gui to client"
      (try
        let h = Unix.gethostbyname 
            (if !!hostname = "" then Unix.gethostname () else !!hostname) in
        h.Unix.h_addr_list.(0)
      with 
        e -> 
          Printf.printf "Exception %s in gethostbyname" (Printexc2.to_string e);
          print_newline ();
          try 
            inet_addr_of_string !!hostname
          with e ->
              Printf.printf "Exception %s in inet_addr_of_string" 
                (Printexc2.to_string e);
              print_newline ();
              raise Not_found
    )
    !!port (fun _ _ -> 
        ()) in
  try
    connection_sock := Some sock;
    TcpBufferedSocket.set_closer sock (fun _ _ -> 
        match !connection_sock with
          None -> ()
        | Some s -> 
            if s == sock then begin
                connection_sock := None;
                clean_gui ();      
              end
    );
    TcpBufferedSocket.set_max_write_buffer sock !!interface_buffer;
    TcpBufferedSocket.set_reader sock (value_handler (value_reader gui));
    gui#label_connect_status#set_text "Connecting"
  with e ->
      Printf.printf "Exception %s in connecting" (Printexc2.to_string e);
      print_newline ();
      TcpBufferedSocket.close sock "error";
      connection_sock := None
*)

let (!!) = Options.(!!)
let (=:=) = Options.(=:=)

let short_name n =
  let len = String.length n in
  if len > 35 then
    Printf.sprintf "%s...%s" (String.sub n 0 27) (String.sub n (len-5) 5)
  else n

let is_connected state =
  match state with
  | Connected_initiating
  | Connected_busy
  | Connected_idle
  | Connected_queued -> true
  | NotConnected
  | Connecting
  | RemovedHost
  | BlackListedHost
  | NewHost -> false

let save_gui_options gui =
  (* Compute layout *)
  (
   match gui#window#children with
     [] -> ()
   | w :: _ ->
       let (w,h) = Gdk.Window.get_size w#misc#window in
       O.gui_width =:= w;
       O.gui_height =:= h
  );
  Options.save_with_help Gui_options.mldonkey_gui_ini

let set_hpaned (hpaned : GPack.paned) prop =
  let (w1,_) = Gdk.Window.get_size hpaned#misc#window in
  let ndx1 = (w1 * !!prop) / 100 in
  hpaned#child1#misc#set_geometry ~width: ndx1 ();
  hpaned#child2#misc#set_geometry ~width: (w1 - ndx1 - hpaned#handle_size) ()

let set_vpaned (hpaned : GPack.paned) prop =
  let (_,h1) = Gdk.Window.get_size hpaned#misc#window in
  let ndy1 = (h1 * !!prop) / 100 in
  hpaned#child1#misc#set_geometry ~height: ndy1 ();
  hpaned#child2#misc#set_geometry ~height: (h1 - ndy1 - hpaned#handle_size) ()
  

let get_hpaned gui (hpaned: GPack.paned) prop =
  
  ignore (hpaned#child1#coerce#misc#connect#size_allocate
      ~callback: (fun r ->
        let (w1,_) = Gdk.Window.get_size hpaned#misc#window in
        prop =:= r.Gtk.width * 100 / (max 1 (w1 - hpaned#handle_size));
        save_gui_options gui
    ))

let get_vpaned gui (hpaned: GPack.paned) prop =
  
  ignore (hpaned#child1#coerce#misc#connect#size_allocate
      ~callback: (fun r ->
        let (_,h1) = Gdk.Window.get_size hpaned#misc#window in
        prop =:= r.Gtk.height * 100 / (max 1 (h1 - hpaned#handle_size));
        save_gui_options gui
    ))

let create_search query_entry max_hits search_type =
  let s = {
    GuiTypes.search_num = !Gui_global.search_counter ;
    GuiTypes.search_query = query_entry ;
      GuiTypes.search_max_hits = max_hits ;
      GuiTypes.search_type = search_type;
  } 
  in
  incr Gui_global.search_counter;
  s

let rec rec_description_of_query q = 
  match q with
  | Q_HIDDEN l
  | Q_AND l
  | Q_OR l -> List.flatten (List.map rec_description_of_query l)

  | Q_ANDNOT (q1, q2) -> rec_description_of_query q1
  | Q_MODULE (_,q) -> rec_description_of_query q
	
  | Q_KEYWORDS (_,s) -> [s]
  | Q_MINSIZE _ 
  | Q_MAXSIZE _ -> []
  | Q_FORMAT (_,s)
  | Q_MEDIA  (_,s)
  | Q_MP3_ARTIST (_,s)
  | Q_MP3_TITLE (_,s)
  | Q_MP3_ALBUM (_,s) -> [s]

  | Q_MP3_BITRATE _ -> []



(** Retourne quelques mots pour résumer une requete *)
let description_of_query q =
  match rec_description_of_query q with 
    [] -> "stupid query"
  | [s] -> s
  | [s1 ; s2] -> s1^" "^s2
  | s1 :: s2 :: s3 :: _ -> s1^" "^s2^" "^s3
 

(** To pretty-print a file size (int32) *)
let size_of_int32 size =
  if !!Gui_options.use_size_suffixes then
    let f = Int32.to_float size in
    if f > go then
      Printf.sprintf "%.2fG" (f /. go)
    else
      if f > mo then
      Printf.sprintf "%.1fM" (f /. mo)
      else
	if f > ko then
	  Printf.sprintf "%.1fk" (f /. ko)
	else
	  Int32.to_string size
  else
    Int32.to_string size

(** To pretty-print a file size (int64) *)
let size_of_int64 size =
  if !!Gui_options.use_size_suffixes then
    let f = Int64.to_float size in
    if f > go then
      Printf.sprintf "%.2fG" (f /. go)
    else
      if f > mo then
      Printf.sprintf "%.1fM" (f /. mo)
      else
	if f > ko then
	  Printf.sprintf "%.1fk" (f /. ko)
	else
	  Int64.to_string size
  else
    Int64.to_string size

(** Return a color for a given name. *)
let color_of_name name =
  let accs = [| ref 0 ; ref 0 ; ref 0 |] in
  for i = 0 to (String.length name) - 1 do
    let m = i mod 3 in
    accs.(m) := !(accs.(m)) + Char.code name.[i]
  done;
  let r = !(accs.(0)) mod 210 in
  let g = !(accs.(1)) mod 210 in
  let b = !(accs.(2)) mod 210 in
  let s = Printf.sprintf "#%02X%02X%02X" r g b in
  `NAME s
