# 2 "gui/gui_trailer.ml"
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

open Mftp
open BasicSocket
open TcpClientSocket
open Unix
open Gui_proto
open Gui_types
  
let short_name n =
  let len = String.length n in
  if len > 35 then
    Printf.sprintf "%s...%s" (String.sub n 0 27) (String.sub n (len-5) 5)
  else n

let first_name r = match r.result_names with
    [] -> assert false
  | name :: _ -> name
  
let gui =
  ignore (GMain.Main.init ()) ;
  new gui () 
      
let connection_sock = ref None
let search_counter = ref 0
  
exception NotConnectedToClient    
let gui_send t = 
  match !connection_sock with
    None -> raise NotConnectedToClient
  | Some sock ->
      value_send sock (t : Gui_proto.from_gui)
  
let window = GWindow.window 
    ~title: "MLdonkey"
    ~width: 800 ~height: 500
    () 

let tab_searches = gui#tab_searches
let tab_servers = gui#tab_servers
let tab_downloads = gui#tab_downloads
let tab_options = gui#tab_options
let tab_friends = gui#tab_friends
let tab_help = gui#tab_help
  
let is_connected s =
  match s.server_state with
  | Connected_initiating
  | Connected_busy
  | Connected_idle
  | Connected_queued -> true
  | NotConnected
  | Connecting
  | Removed -> false


let default_make_menu _ =
  (List.map 
      (fun (s,f) -> `I (s, f))
    []
  )

let clists_need_resize = ref []
  
let need_resize clist = 
  if not (List.memq clist !clists_need_resize) then 
    clists_need_resize := clist :: !clists_need_resize

module MyCList: sig 
    type ('a, 'b) t
    type ('a, 'b) node
    
    val create : gui -> int GList.clist -> ('b -> string) list -> ('c, 'b) t
    
    val set_context_menu : 
      ('c, 'b)t -> (('c, 'b) t ->  GToolbox.menu_entry list) -> unit
    val set_selected_callback :
      ('c, 'b)t -> (('c, 'b) t -> 'b -> unit) -> unit
    val set_replace_value : 
      ('c, 'b)t -> ('b -> 'b -> 'b) -> unit
    val set_size_callback :
      ('c, 'b)t -> (int -> unit) -> unit
    
    val clear : ('a, 'b) t -> unit
    val add : ('a, 'b) t -> 'a -> 'b -> unit
    val update : ('a, 'b) t -> 'a -> 'b -> unit
    val selection : ('a, 'b) t -> 'b list
    val set_value : ('a, 'b) t -> 'a -> 'b -> unit
    val remove : ('a, 'b) t -> 'a -> unit
    val unselect_all : ('a,'b) t -> unit
    val find : ('a,'b) t -> 'a -> 'b
    val size : ('a,'b) t -> int
    val set_can_select_all: ('a,'b) t -> unit

    val set_multiple_select :  ('a,'b) t -> bool -> unit
    val get_multiple_select :  ('a,'b) t -> bool
    val set_auto_resize :  ('a,'b) t -> bool -> unit
    val get_auto_resize :  ('a,'b) t -> bool
      
    val iter :  ('a,'b) t -> ('a -> 'b -> unit) -> unit
  end = struct 
    
    type ('a,'b) t = {
        clist: int GList.clist;
        ncols : int;
        cols : ('b -> string) list;
        cols_array : ('b -> string) array;
        gui : gui;
        num_table : (int, ('a,'b) node) Hashtbl.t;
        table : ('a, ('a,'b) node) Hashtbl.t;
        mutable num_counter : int;        
        mutable selection : int list;
        mutable make_menu : (('a, 'b) t ->  GToolbox.menu_entry list);
        mutable selected_callback : (('a, 'b) t -> 'b -> unit);
        mutable replace_value : ('b -> 'b -> 'b);
        mutable size_callback : (int -> unit);
        mutable can_select_all : bool;
        mutable multiple_select : bool;
        mutable auto_resize : bool;
      }
    
    and ('a,'b) node = {
        num : int;
        key : 'a;
        mutable value : 'b;
        mutable desc : string list;
      }

    let set_can_select_all t =
      t.can_select_all <- true
      
    let iter t f =
      let list = ref [] in
      Hashtbl.iter (fun _ node -> list := node :: !list) t.table;
      List.iter (fun node -> f node.key node.value) !list
    
    let row t node =
      let rec find_row clist row num =
        if clist#get_row_data row = num then row 
        else find_row clist (row+1) num
      in
      find_row t.clist 0 node.num
    
    let set_selected_callback t f =
      t.selected_callback <- f
          
    let unselect_all t =
      t.clist#unselect_all ();
      t.selection <- []

    let select_all t = 
      t.clist#select_all ()

    let set_multiple_select t b =
      t.multiple_select <- b;
      if b then begin
          t.clist#set_selection_mode `EXTENDED;
        end else begin
          t.clist#set_selection_mode `MULTIPLE;
        end
        
    let set_auto_resize t b =
      t.auto_resize <- b;
      if b then need_resize t.clist
    let get_auto_resize t = 
      t.auto_resize
    let get_multiple_select t = t.multiple_select
      
    let set_context_menu t f =
      t.make_menu <- (fun t ->
          let items = f t in
          let items = 
            match t.selection with
              _ :: _ :: _ ->
                (`I ("Unselect all", (fun _ -> unselect_all t)))  :: items
            | _ -> items
          in
          let items = 
            if t.can_select_all then
              (`I ("Select all", (fun _ -> select_all t)))  :: items
            else items 
          in
          let items = 
            items @
              [
              (if get_multiple_select t then
                  (`I ("Multiple selection mode", (fun _ -> 
                          set_multiple_select t false)))
                  else
                  (`I ("Extended selection mode", (fun _ -> 
                          set_multiple_select t true))));
              (if get_auto_resize t then  
                  (`I ("No auto resize", (fun _ -> 
                          set_auto_resize t false)))
                else
                  (`I ("Auto resize", (fun _ -> 
                          set_auto_resize t true)))
              )
              ] 
          in
          items)
    
    let set_replace_value t f = 
      t.replace_value <- f
      
    let find t a = (Hashtbl.find t.table a).value
    let size t = t.clist#rows
    let select_row t ~row ~column ~event = 
      let num = t.clist#get_row_data row in
      let node = Hashtbl.find t.num_table num in
      t.selected_callback t node.value;
      t.selection <- num :: t.selection
    
    let unselect_row t ~row ~column ~event = 
      let num  = t.clist#get_row_data row in
      t.selection <- List2.remove num t.selection
    
    let get_row t key =
      Hashtbl.find t.table key
    
    let get_value t row = row.value
    let set_value t key value =
      if t.auto_resize then need_resize t.clist;
      let node = Hashtbl.find t.table key in
      node.value <- t.replace_value node.value value;
      begin
        match t.selection with
          num :: _ when node.num = num ->
            t.selected_callback t node.value
        | _ -> ()
      end;
      let rec iter i cols descs =
        match cols, descs with
          f :: cols, old_desc :: descs ->
            let desc = try f value with
                e -> 
                  Printf.printf "Exception %s in desc" (Printexc.to_string e);
                  print_newline ();
                  old_desc in
            if desc <> old_desc then
              t.clist#set_cell ~text: desc (row t node) i;
            desc :: (iter (i+1) cols descs)
        | [], [] -> 
            []
        | [], _ ->
            []
        | _ -> 
            []
      in
      node.desc <- iter 0  t.cols node.desc
      
    let selection t = List.map (fun num ->
          (Hashtbl.find t.num_table num).value
      ) t.selection
    
    let contextual_menu t ev =
      GdkEvent.Button.button ev = 3 &&
      GdkEvent.get_type ev = `BUTTON_PRESS &&
      (
        GToolbox.popup_menu 
          ~x: (int_of_float (GdkEvent.Button.x ev))
        ~y: (int_of_float (GdkEvent.Button.y ev))
        ~entries: (t.make_menu t)
        ;
        true
      )


    let set_size_callback t f = t.size_callback <- f
      
    let create gui clist cols  =
      let t = { 
          clist = clist; 
          ncols = List.length cols;
          cols = cols;
          cols_array = Array.of_list cols;
          gui  = gui;
          num_table = Hashtbl.create 127;
          table = Hashtbl.create 127;
          num_counter = 1;
          selection = [];
          make_menu = default_make_menu;
          selected_callback = (fun _ _ -> ());
          replace_value = (fun _ x -> x);
          size_callback = (fun _ -> ());
          can_select_all = false;
          multiple_select = true;
          auto_resize = true;
      }
      in
      set_context_menu t default_make_menu;
      ignore (t.clist#connect#select_row (select_row t));
      ignore (t.clist#connect#unselect_row (unselect_row t));
      ignore (t.clist#event#connect#button_press ~callback:
        (contextual_menu t));
      for i = 0 to t.ncols do
        t.clist#set_column (* ~auto_resize: true *) ~resizeable: true i;
      done;
      ignore (t.clist#connect#click_column ~callback:(fun i ->
          t.clist#set_sort ~column: i ~auto:false ~dir: `DESCENDING ();
          t.clist#sort ();
        )) ;
      clist#set_selection_mode `EXTENDED;
      t

  
    let clear t =
      t.clist#clear ();
      Hashtbl.clear t.num_table;
      Hashtbl.clear t.table;
      t.selection <- [];
      if t.auto_resize then need_resize t.clist

    let add t key value =
      if t.auto_resize then need_resize t.clist;
      let desc = List.map (fun f -> f value) t.cols in
      let row = t.clist#append desc  in
      t.num_counter <- t.num_counter + 1;
      let num = t.num_counter in
      let node = {
          num = num;
          key = key;
          value = value;
          desc = desc;
        } in
      t.clist#set_row_data row num;
(*      GToolbox.autosize_clist t.clist;*)
      Hashtbl.add t.num_table num node;
      Hashtbl.add t.table key node;
      t.size_callback (t.clist#rows)

    let update t key value =
      try
        set_value t key value        
      with Not_found ->
          add t key value
      
    let node t row =
      let num = t.clist#get_row_data row in
      Hashtbl.find t.num_table num

    let remove t key =
      if t.auto_resize then need_resize t.clist;
      let node = Hashtbl.find t.table key in
      Hashtbl.remove t.table key;
      Hashtbl.remove t.num_table node.num;
      t.clist#remove (row t node);
      t.size_callback (t.clist#rows)
      
  end


let update_sizes timer = 
  reactivate_timer timer;
  List.iter (fun clist -> 
      try clist#columns_autosize () with _ -> ()) !clists_need_resize;
  clists_need_resize := []
  
  
let for_selection list f () =
  List.iter (fun file ->
      try f file with _ -> ()) (MyCList.selection list);
  MyCList.unselect_all list

let for_selection_keep_selected list f () =
  List.iter (fun file ->
      try f file with _ -> ()) (MyCList.selection list)

  
let string_of_tags tags =
  let buf = Buffer.create 100 in
      List.iter (fun t ->
          Buffer.add_string buf (Printf.sprintf "%-3s "
              (match t.tag_value with
                String s -> s
              | Uint32 i -> Int32.to_string i
              | Fint32 i -> Int32.to_string i
              | _ -> "???"
            ))
  ) tags;
  Buffer.contents buf

let string_of_state state =
  match state with
    NotConnected -> ""
  | Connecting -> "Connecting"
  | Connected_initiating -> "Initiating"
  | Connected_busy
  | Connected_idle -> "Connected"
  | Connected_queued -> "Queued"
  | Removed -> "Removed"

let nconnected_servers = ref 0
      
let (clist_servers : 
    ((Ip.t * int), Gui_proto.server_info) MyCList.t) = 
  MyCList.create gui gui#tab_servers#clist_servers 
    [
(* IP & PORT *)      
    (fun s -> Printf.sprintf "%16s : %-5d" (
          Ip.to_fixed_string s.server_ip) s.server_port);
(* STATUS *)  
    (fun s -> string_of_state s.server_state);
(* NUSERS *)  
    (fun s -> 
        if s.server_nusers = 0 then "" else
        Printf.sprintf "%+5s" (string_of_int s.server_nusers));
(* NFILES *)  
    (fun s -> 
        if s.server_nfiles = 0 then "" else
          Printf.sprintf "%+7s"  (string_of_int s.server_nfiles));
(* NAME & DESC *)    
    (fun s -> 
        if s.server_name = "" then "" else
        Printf.sprintf "%s [%s]" 
        s.server_name s.server_description);
  ] 

let clist_server_users =
  MyCList.create gui gui#tab_servers#clist_users
  [
(* KIND *) (fun u -> if Ip.valid u.user_ip then "Direct" else "");
(* NAME *) (fun u -> u.user_name)
  ]

let clist_server_users_current = ref (-1)

let server_make_menu t =
  (List.map 
      (fun (s,f) -> `I (s, f))
    ([("Resolve IP", for_selection clist_servers
            (fun s -> 
              let name = Ip.resolve_one s.server_ip in
              MyCList.set_value clist_servers 
                (s.server_ip, s.server_port) s
          )
        );

      ])
  )

let nservers = ref 0
let nfriends = ref 0
let nresults = ref 0
let current_search = ref (-1)
  
let update_searches_label () =
  gui#label_searches_status#set_text (Printf.sprintf 
      "%d Friends %d Search Results" !nfriends !nresults)
  
let update_server_label () =
  gui#label_servers_status#set_text
    (Gui_messages.connected_to_servers !nconnected_servers !nservers)
  
let _ =
  MyCList.set_can_select_all clist_servers;
  MyCList.set_can_select_all clist_server_users;
  MyCList.set_replace_value clist_servers (fun old_s new_s ->
      begin
        match old_s.server_more_info, new_s.server_more_info with
          Some _, None -> new_s.server_more_info <- old_s.server_more_info
        | _ -> ()
      end;
      new_s);
  MyCList.set_selected_callback clist_servers (fun _ s ->
      match s.server_more_info with
        None -> ()
      | Some si ->
          if !clist_server_users_current <> s.server_num then begin
              clist_server_users_current := s.server_num;
              MyCList.clear clist_server_users;
            end;
          List.iter (fun u -> 
              MyCList.update clist_server_users (u.user_name, u.user_md4) u) 
          si.server_users;
  );
  MyCList.set_context_menu clist_servers server_make_menu;
  MyCList.set_size_callback clist_servers (fun n ->
      nservers := n;
      update_server_label ()
  )
  
let add_user_to_friends =
  for_selection  clist_server_users (fun u ->
      gui_send (AddUserFriend u))
  

  
let string_of_file_state state =
  match state with
    FileDownloading -> "Downloading"
  | FileCancelled -> "Cancelled"
  | FilePaused -> "Paused"
  | FileDownloaded -> "Done"
  | FileRemoved -> "Removed"
      
let (clist_downloads : 
    (Md4.t, Gui_proto.file_info) MyCList.t) = 
  MyCList.create gui gui#tab_downloads#clist_downloads
    [
(* FILENAME *)
    (fun f -> match f.file_name with
          [] -> "<unknown>"
        | name :: _ -> short_name name);
(* SIZE *)    
    (fun f -> Printf.sprintf "%+10s" (Int32.to_string f.file_size));
(* DOWNLOADED *)
    (fun f -> Printf.sprintf "%+10s" (Int32.to_string f.file_downloaded));
(* PERCENT *)
    (fun f -> 
        Printf.sprintf "%5.1f" (
          Int32.to_float f.file_downloaded /. Int32.to_float f.file_size
          *. 100.)
        );
(* RATE *)
    (fun f -> 
        if f.file_download_rate > 0. then
          Printf.sprintf "%5.1f" (
            (f.file_download_rate /. 1024.) )
        else "");
(* STATE *)
    (fun f -> string_of_file_state f.file_state);    
(* AVAIL *)
    (fun f -> 
        let len = String.length f.file_availability in
        let p = ref 0 in
        for i = 0 to len - 1 do
          if f.file_availability.[i] <> '0' then begin
              incr p
            end
        done;
        if len = 0 then "" else 
          Printf.sprintf "%5.1f" (float_of_int !p /. float_of_int len *. 100.)
    );
(* MD4 *)
    (fun f -> Md4.to_string f.file_md4);    
  ]

let (clist_file_locations :
    (int, client_info) MyCList.t) =
  MyCList.create gui tab_downloads#clist_locations
    [
    (fun c -> match c.client_kind with
          Known_location _ -> "Direct"
        | _ -> "");
    (fun c -> c.client_name);
    (fun c -> string_of_state c.client_state);
  ]

let add_friend_location =
  for_selection clist_file_locations (fun c ->
      gui_send (AddFriend c.client_num)
  )  

  
  
let menu_save_file t =
  match MyCList.selection t  with
  | [file] ->
      
      let save_as file () =
        let file_opt = GToolbox.input_string ~title: Gui_messages.mes_save 
            Gui_messages.mes_save in
        match file_opt with
          None -> ()
        | Some name -> 
            prerr_endline ("save as "^name); 
            gui_send (Gui_proto.SaveFile (file.file_md4, name))
      in
      List.map 
        (fun (s,f) -> `I (s, f))
      (      
        let items =
          ("Preview file", (fun _ ->
              gui_send (Preview file.file_md4))) ::
          (Gui_messages.mes_save, save_as file) ::
          
          (List.map (fun name ->
                name, (fun _ -> 
                    gui_send (Gui_proto.SaveFile (file.file_md4, name))
                )) file.file_name
          )
        in
        match file.file_format with
          Mp3 tag ->
            let edit_mp3tag file () = 
(*              Printf.printf "do it"; print_newline (); *)
              (
                Mp3_ui.edit_tag_v1 Gui_messages.mes_edit_mp3 tag ;
                gui_send (Gui_proto.ModifyMp3Tags (file.file_md4, tag))
              )
            in
            (Gui_messages.mes_edit_mp3, edit_mp3tag file) :: items
        
        | _ ->
            items)
  
  | _ -> []
      
let menu_downloads_file t =
  List.map 
    (fun (s,f) -> `I (s, f))
  [ 
    "Pause/Resume file(s)", for_selection clist_downloads (fun file -> 
        gui_send (SwitchDownload file.file_md4));
    "Cancel file(s)", 
    for_selection clist_downloads (fun file -> 
        gui_send (RemoveDownload_query file.file_md4));
    "Verify all chunks file(s)",
    for_selection clist_downloads
      (fun file -> 
        gui_send (VerifyAllChunks file.file_md4));
    "Preview file",
    for_selection clist_downloads
      (fun file ->
        gui_send (Preview file.file_md4));
    "Get Format Info",
    for_selection clist_downloads (fun file ->
        gui_send (QueryFormat file.file_md4));
  ]
      
let (clist_downloaded : 
    (Md4.t, Gui_proto.file_info) MyCList.t) = 
  MyCList.create gui gui#tab_downloads#clist_downloaded
    [
    (fun f -> match f.file_name with
          [] -> "<unknown>"
        | name :: _ -> short_name name);
    (fun f -> Printf.sprintf "%+10s" (Int32.to_string f.file_size));
    (fun f -> Printf.sprintf "%+10s" (Int32.to_string f.file_downloaded));
    (fun f -> string_of_file_state f.file_state);
    (fun f -> Md4.to_string f.file_md4);    
  ]

  
  
let clist_file_locations_current = ref (-1)
  
let ndownloads = ref 0
let ndownloaded = ref 0
  
let set_clist_file_locations_file file =
  match file.file_more_info with
    None -> ()
  | Some mi ->
      if !clist_file_locations_current <> file.file_num then begin
          clist_file_locations_current := file.file_num;
          MyCList.clear clist_file_locations;
        end;
      List.iter (fun c -> 
          MyCList.update clist_file_locations c.client_num c) 
      mi.file_known_locations;
      List.iter (fun c -> 
          MyCList.update clist_file_locations c.client_num c) 
      mi.file_indirect_locations      

let update_download_label () =
  gui#label_download_status#set_text (Mes.downloaded_files !ndownloaded !ndownloads);
  tab_downloads#label_downloading#set_text (Mes.downloading_files !ndownloads);
  tab_downloads#label_downloaded#set_text (Mes.files_downloaded !ndownloaded)

let save_all_files _ =
  MyCList.iter clist_downloaded (fun _ file ->
      let name = 
        match file.file_name with
          [] -> Md4.to_string file.file_md4
        | name :: _ ->  name
      in
      gui_send (Gui_proto.SaveFile (file.file_md4, name)))
  
let current_file = ref None

let colorGreen = `NAME "green"
let colorRed   = `NAME "red"
let colorBlue  = `NAME "blue"
let colorWhite =`WHITE
let colorBlack = `BLACK

let drawing = ref (None :   [ `window] GDraw.drawable option)
  
let redraw_current _ =
  let drawing = match !drawing with
      None -> 
        
        let w = tab_downloads#draw_availability#misc#window in
        let d = new GDraw.drawable w in
        tab_downloads#draw_availability#misc#show ();
        drawing := Some d;
        d        
    | Some d -> d
  in

  let wx, wy = drawing#size in
  drawing#set_foreground colorWhite;
  drawing#rectangle ~filled: true ~x:0 ~y:0 ~width:wx ~height:wy ();
  match !current_file with
    None -> true
  | Some file ->
      let nchunks = String.length file.file_chunks in
      let dx = min 3 (wx / nchunks) in
      let offset = (wx - nchunks * dx) / 2 in
      let offset = if offset < 0 then 0 else offset in
      for i = 0 to nchunks - 1 do
        drawing#set_foreground (
          if file.file_chunks.[i] = '1' then
            colorGreen
          else
          match  file.file_availability.[i] with
            '0' -> colorRed
          | '1' -> colorBlue
          | _ -> colorBlack);
        drawing#rectangle ~filled: true
        ~x:(offset + i*dx) ~y: 0 
          ~width: dx ~height:wy ()
      done;
      false

  
let update_current_file () =
  tab_downloads#label_file_info#set_text (
    match !current_file with
      None -> ""
    | Some file ->
        Printf.sprintf "NAME: %s SIZE: %s FORMAT: %s" (match file.file_name with
            [] -> Md4.to_string file.file_md4
          | name :: _ -> name)
        (Int32.to_string file.file_size) 
        (match file.file_format with
          AVI f ->
            Printf.sprintf "AVI: %s %dx%d %d fps %d bpf"
              f.avi_codec f.avi_width f.avi_height 
              f.avi_fps f.avi_rate
	| Mp3 tag ->
	    Printf.sprintf "MP3: %s - %s (%d): %s"
	      tag.Mp3tag.artist tag.Mp3tag.album 
	      tag.Mp3tag.tracknum tag.Mp3tag.title
        | _ -> "Unknown")
        ;
  );
  redraw_current ()
      
let _ =
  MyCList.set_can_select_all clist_downloads;  
  MyCList.set_can_select_all clist_downloaded;  
  MyCList.set_can_select_all clist_file_locations;  
  MyCList.set_size_callback clist_downloads (fun n ->
      ndownloads := n;
      update_download_label ();
  );
  MyCList.set_replace_value clist_downloads (fun old_file new_file ->
      begin
        match old_file.file_more_info, new_file.file_more_info with
          Some _, None -> new_file.file_more_info <- old_file.file_more_info
        | _ -> ()
      end;
      new_file);
  MyCList.set_selected_callback clist_downloads (fun clist_downloads file ->
      MyCList.unselect_all clist_downloaded;
      set_clist_file_locations_file file;
      current_file := Some file;
      ignore (update_current_file ())
  );
  MyCList.set_size_callback clist_downloaded (fun n ->
      ndownloaded := n;
      update_download_label ()
  );

  MyCList.set_context_menu clist_downloads  menu_downloads_file;
  MyCList.set_context_menu clist_downloaded menu_save_file;
  MyCList.set_selected_callback clist_downloaded (fun _ file ->
      MyCList.unselect_all clist_downloads;
      set_clist_file_locations_file file)

let clist_friends = MyCList.create gui tab_friends#clist_friends
    [
    (fun c -> 
        match c.client_more_info with
          None -> string_of_state c.client_state
        | Some _ -> "Files listed");    
    (fun c -> let name = c.client_name in
        if name = "" then "<unknown>" else name);
    (fun c -> match c.client_kind with
          Known_location _ -> "Direct"
        | _ -> "");
  ]
  
let clist_friend_files = MyCList.create gui tab_friends#clist_friends_files
    [
(* FILENAME *) 
    (fun r -> (*short_name*) (first_name r));
(* SIZE *)
    (fun r -> Printf.sprintf "%10s" (Int32.to_string r.result_size));
(* PROPERTIES *)
    (fun r -> string_of_tags r.result_tags);
(* MD4 *)
    (fun r -> Md4.to_string r.result_md4);
    ]

let _ =
  MyCList.set_can_select_all clist_friends;
  MyCList.set_can_select_all clist_friend_files

    
let friends_make_menu t =
  (List.map 
      (fun (s,f) -> `I (s, f))
    ([("Connect Friend", for_selection clist_friends
            (fun c -> 
              gui_send (ConnectFriend c.client_num)
              )
        );
        
      ])
  )

  
let _ =
  MyCList.set_size_callback clist_friends (fun n ->
      nfriends := n;
      update_searches_label ());
  MyCList.set_context_menu clist_friends friends_make_menu
  
let remove_friend =
  for_selection clist_friends (fun c ->
      gui_send (RemoveFriend c.client_num))
  
let find_friend _ =
  let friend_name = tab_friends#entry_find_friend#text in
  if friend_name <> "" then begin
      gui_send (FindFriend friend_name)
    end

let server_key s =
  {
    key_ip = s.server_ip;
    key_port = s.server_port;
  }
  
let view_users =
  for_selection clist_servers (fun s ->
      gui_send (ViewUsers (server_key s)))

let connect_server =
  for_selection clist_servers (fun s ->
      gui_send (ConnectServer (server_key s)))

let disconnect_server =
  for_selection clist_servers (fun s ->
      gui_send (DisconnectServer (server_key s)))
  
let remove_old_servers _ =
  gui_send CleanOldServers
  
let connect_all =
  for_selection  clist_downloads
    (fun file -> gui_send (ConnectAll file.file_md4))
  
let current_friend =  ref (-1) 
let _ =
  MyCList.set_replace_value clist_friends (fun old_c new_c ->
      begin
        match old_c.client_more_info, new_c.client_more_info with
          Some _, None -> 
            new_c.client_more_info <- old_c.client_more_info
        | _ -> ()
      end;
      new_c);
  MyCList.set_selected_callback clist_friends (fun _ c ->
      if !current_friend != c.client_num then begin
          match c.client_more_info with
            None -> ()
          | Some ci ->
              MyCList.clear clist_friend_files;
              begin
                match ci.client_all_files with
                  None -> ()
                | Some list ->
                    List.iter (fun r -> 
                        MyCList.add clist_friend_files r.result_md4 r) 
                    list;
                    current_friend := c.client_num
              end
        end
  )

  
let searches = Hashtbl.create 127

let search_npages = ref 0
let search_pages = Array.create 50 None
  
let add_search_page v =
  let n = !search_npages in
  search_pages.(n) <- Some v;
  incr search_npages;
  n
  
let get_search_page v =
  let rec iter n =
    match search_pages.(n) with
    | Some vv when v == vv -> n
    | _ -> iter (n+1)
  in
  iter 0
  
let remove_search_page v =
  let n = get_search_page v in
  decr search_npages;
  Array.blit search_pages (n+1) search_pages n (!search_npages - n);
  n
      
      (*
let clist_searches = MyCList.create gui gui#tab_searches#clist_results
    [
    first_name ;
    (fun r -> Int32.to_string r.result_size);
    (fun r -> string_of_tags r.result_tags);
    (fun r -> Md4.to_string r.result_md4);
    ]
    *)

let addr, port = 
  match Sys.argv with 
    [| _; host; port |] -> 
      let h = Unix.gethostbyname host in
      h.h_addr_list.(0), int_of_string port
  | [| _; host |] -> 
      let h = Unix.gethostbyname host in
      h.h_addr_list.(0), 4001
  | [| _ |] ->
      inet_addr_of_string "127.0.0.1", 4001
  | _ -> 
      Printf.printf "Too many arguments: [<host> [<port>]]";
      print_newline ();
      exit 0

let addr = ref addr
let port = ref port
      
let download_cancel (gui:gui) () =
  let module P = Gui_proto in
  for_selection clist_downloads (fun file ->
      gui_send (RemoveDownload_query file.file_md4)
  ) ()
  
let search_stop clist_search gui num () =
  Hashtbl.remove searches num
  
let search_close clist_search gui num () =
  (try Hashtbl.remove searches num with _ -> ());
  gui_send (ForgetSearch num);
  let n = remove_search_page clist_search in
  tab_searches#notebook_results#remove_page n
      
      
let search_download clist_search gui () =
  let module P = Gui_proto in
  for_selection  clist_search (fun r ->
      gui_send (Download_query 
          (r.result_names, r.result_size, r.result_md4, None))
  ) ()

let download_ed2k_url _ =
  let url = tab_downloads#entry_ed2k_url#text in
  match String2.split (String.escaped url) '|' with
    ["ed2k://"; "file"; name; size; md4; ""] 
  | ["file"; name; size; md4; ""] ->
      gui_send (Download_query 
          ([name], Int32.of_string size, Md4.of_string md4, None));
      tab_downloads#entry_ed2k_url#set_text ""
  | _ -> ()
  
let download_md4 _ =
  let md4 = tab_downloads#entry_md4#text in
  gui_send (Download_query 
      ([], Int32.zero, Md4.of_string md4, None));
  tab_downloads#entry_md4#set_text ""
  
let download_friend_files =
  let module P = Gui_proto in
  for_selection clist_friend_files (fun r ->
      gui_send (Download_query 
          (r.result_names, r.result_size, r.result_md4, Some (!current_friend)))
  )   

  
let (options_assocs : 
    (string * GEdit.entry) list) = 
  let options = gui#tab_options in
  [
    "port",        options#entry_options_conn_port;
    "telnet_port", options#entry_options_rmt_port;
    "gui_port",    options#entry_options_gui_port;
    "save_options_delay", options#entry_options_save_delay;
    "check_client_connections_delay", options#entry_options_check_clients_delay;
    "check_connections_delay", options#entry_options_check_servers_delay;
    "small_retry_delay", options#entry_options_small_retry_delay;
    "medium_retry_delay", options#entry_options_medium_retry_delay;
    "long_retry_delay", options#entry_options_long_retry_delay;
    "client_name",   options#entry_options_name;
    "password",  options#entry_options_password;
    "max_connected_servers", options#entry_options_maxconn_servers;
    "max_upload_rate", options#entry_options_upload_limit;
    "server_connection_timeout", options#entry_options_server_timeout;
    "client_timeout", options#entry_options_client_timeout;
    "max_server_age", options#entry_max_server_age;    
    "update_gui_delay", options#entry_refresh_delay;
  ]
  

let options_assocs_rev = List.map (fun (name,widget) ->
      widget, name) options_assocs
  
  
let password = ref ""
  
let value_reader (gui: gui) t sock =
  try
    let module P = Gui_proto in
    match t with
      
    | Dialog (name , s) ->
        let len = String.length name + 1 in
        let fg = `NAME "red" in
        ignore (gui#tab_friends#text_dialog#insert
            ~foreground:fg (name ^ ":"));
        ignore (gui#tab_friends#text_dialog#insert 
            (Printf.sprintf " <%s>\n" s));
        
    | Console text ->
        
        ignore (gui#tab_console#text#insert_text text 0)
        
    | LocalInfo l ->
        
        gui#label_upload_status#set_text (
          Printf.sprintf "Upload %d/%d" l.upload_counter l.shared_files)
        
    | Connected v -> 
        if v <> Gui_types.version then begin
            Printf.printf "Bad GUI version"; print_newline ();
            TcpClientSocket.close sock "bad version";
          end
        else begin
(*        Printf.printf "Connected"; print_newline (); *)
            
            gui#label_connect_status#set_text "Connected";
            gui_send (Password (Gui_types.version,!password))
          end
    
    | P.Search_result r -> 
(*        Printf.printf "Search_result"; print_newline (); *)
        begin
          try
            let clist_search, label_query = 
              Hashtbl.find searches r.P.result_num in
            let r = r.P.result_res in
            MyCList.add clist_search r.result_md4 r
          with _ -> 
              ()
        end
    
    | P.Search_waiting (num,waiting) -> 
(*        Printf.printf "Search_waiting"; print_newline (); *)
        begin
          try
            let (_, label_waiting) = Hashtbl.find searches num in
            label_waiting#set_text (
              Printf.sprintf "Waiting for %d replies" waiting);
          with _ ->               ()
        end
        
    | P.Download_file f ->
(*        Printf.printf "Download_file"; print_newline (); *)
        let clist = gui#tab_downloads#clist_downloads in
        begin
          match f.file_state with
            FileCancelled ->
              MyCList.remove clist_downloads f.file_md4 
          | FileDownloaded ->
              begin
                (try MyCList.remove clist_downloads f.file_md4  with _ -> ());
                (try
                    MyCList.set_value clist_downloaded f.file_md4 f        
                  with _ ->
                      MyCList.add clist_downloaded f.file_md4 f
                )
              end
          | FileRemoved ->
              MyCList.remove clist_downloaded f.file_md4             
          | _ ->
              (try
                  MyCList.set_value clist_downloads f.file_md4 f        
                with _ ->
                    MyCList.add clist_downloads f.file_md4 f
              )
        end
    
    | P.Server_info s ->
(*        Printf.printf "Server_info"; print_newline ();  *)
        let key = s.P.server_ip, s.P.server_port in
        
        begin
          try
            let os = MyCList.find clist_servers key in
            begin
              match is_connected s, is_connected os with
                true, false -> incr nconnected_servers
              | false, true -> decr nconnected_servers
              | _ -> ()
            end;
            if s.server_state = Removed then
              (try MyCList.remove clist_servers key with _ -> ())
            else
              MyCList.set_value clist_servers key s
          with _ ->
              if s.server_state <> Removed then
                begin
                  if is_connected s then incr nconnected_servers;
                  MyCList.add clist_servers key s;
                end
        end;
        update_server_label ()
    
    | P.GuiConnected ->
        let md4_list = ref [] in
        let client_list = ref [] in
        MyCList.iter clist_downloads (fun md4 file -> 
            md4_list := md4 :: !md4_list
            ) ;
        MyCList.iter clist_downloaded (fun md4 file -> 
            md4_list := md4 :: !md4_list
        ) ;
        MyCList.iter clist_friends (fun num _ ->
            client_list := num :: !client_list 
        ) ;
        gui_send (SendMoreInfo (!md4_list, !client_list))
        
        
    | P.Options_info list ->
(*        Printf.printf "Options_info"; print_newline ();  *)
        let options = gui#tab_options in
        
        let rec iter list =
          match list with
            [] -> ()
          | (name, value) :: tail ->
              (
                try
                  let widget = List.assoc name options_assocs in
                  widget#set_text value
                with _ -> 
(* 
  Printf.printf "No widget for %s" name; 
print_newline () 
*)
                    ()
  );
              iter tail
        in
        iter list
        
    | P.Client_info c -> 
(*        Printf.printf "Client_info"; print_newline (); *)
        begin
          match c.client_is_friend with
            Friend ->
              begin
                try
                  ignore (MyCList.find clist_friends c.client_num);
                  MyCList.set_value clist_friends c.client_num c
                with _ ->
                    MyCList.add clist_friends c.client_num c
              end
          | FriendRemoved ->
              begin
                (try MyCList.remove clist_friends c.client_num with _ -> ())
              end
          | NotAFriend ->
              ()
        end;              
        ()
  with e ->
      Printf.printf "EXception %s in reader" (Printexc.to_string e);
      print_newline ()
      
let ko = Int32.of_int 1024
  
let unit_of_string s =
  match String.lowercase s with
    "mo" -> Int32.mul ko ko
  | "ko" -> ko
  | _ -> Int32.one

let search_media_list = 
  [ "Program", "Pro";
    "Documentation", "Doc";
    "Collection", "Col";
  ]
  
let search_format_list = []

let option_of_string s =
  if s = "" then None else Some s
  
let submit_search (gui: gui) local ()=
  let module P = Gui_proto in
  incr search_counter;
  let search_num = !search_counter in
  current_search := !search_counter;
  nresults := 0;
  update_searches_label ();
  let s = gui#tab_searches in
  gui_send (P.Search_query (local, {
      P.search_words = [s#entry_search_words#text];
      P.search_minsize = (let minsize = s#entry_search_minsize#text in
        if minsize = "" then None else Some (
            Int32.mul (Int32.of_string minsize)
            (unit_of_string s#combo_search_minsize_unit#entry#text)
          ));
      P.search_maxsize = (let maxsize = s#entry_search_maxsize#text in
        if maxsize = "" then None else Some (
            Int32.mul (Int32.of_string maxsize)
            (unit_of_string s#combo_search_minsize_unit#entry#text)
          ));
      P.search_avail = None;
      P.search_media = (
        let media = s#combo_search_media#entry#text in
        try
          Some (List.assoc media  search_media_list)
        with _ -> 
            if media = "" then None else Some media);
      P.search_format = (
        let format = s#combo_format#entry#text in
        try
          Some (List.assoc format search_format_list)
        with _ -> 
            if format = "" then None else Some format);
      P.search_min_bitrate = ( 
        let bitrate = s#combo_min_bitrate#entry#text in
        if bitrate = "" then None else
        try
          Some (Int32.of_string bitrate)
        with _ -> None);    
      P.search_title = option_of_string s#entry_title#text;
      P.search_artist = option_of_string s#entry_artist#text;
      P.search_album = option_of_string s#entry_album#text;
      P.search_fields = [];
      P.search_num = !search_counter;
    }));
  let new_tab = new box_search () in

  let clist_search = MyCList.create gui new_tab#clist_search_results       
      [
(* SIZE *)
      (fun r -> (Printf.sprintf "%10s" (Int32.to_string r.result_size)));
(* NAME *)
      (fun r -> (*short_name*) (first_name r)) ;
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
      TcpClientSocket.close sock "user close";
      connection_sock := None

let reconnect gui =
  (try disconnect gui with _ -> ());
  let sock = TcpClientSocket.connect !addr !port (fun _ _ -> 
        ()) in
  connection_sock := Some sock;
  TcpClientSocket.set_closer sock (fun _ _ -> 
      clean_gui ();      
      connection_sock := None;
      );
  TcpClientSocket.set_reader sock (value_handler (value_reader gui));
  gui#label_connect_status#set_text "Connecting"


let servers_connect_more (gui : gui) () =
  gui_send (Gui_proto.ConnectMore_query)
  
let servers_addserver (gui : gui) () = 
  let module P = Gui_proto in
  gui_send (P.AddServer_query {
      P.key_ip = Ip.of_string gui#tab_servers#entry_servers_new_ip#text;
      P.key_port = int_of_string gui#tab_servers#entry_servers_new_port#text;
    });
  tab_servers#entry_servers_new_ip#set_text "";
  tab_servers#entry_servers_new_port#set_text ""

  
let friends_addfriend (gui : gui) () = 
  gui_send (AddNewFriend (Ip.of_string
        gui#tab_friends#entry_friends_new_ip#text,
      int_of_string gui#tab_friends#entry_friends_new_port#text));
  tab_friends#entry_friends_new_ip#set_text "";
  tab_friends#entry_friends_new_port#set_text ""

let save_options (gui: gui) () =
  try
  let module P = Gui_proto in
  let options = tab_options in

    
    (*
  let o = {
      connection_port = int_of_string options#entry_options_conn_port#text;
      control_port = int_of_string options#entry_options_rmt_port#text;
      gui_port = gui_port;
        
      save_options_delay = float_of_string options#entry_options_save_delay#text ;
      check_client_connections_delay = float_of_string
        options#entry_options_check_clients_delay#text;
      check_server_connections_delay = float_of_string
        options#entry_options_check_servers_delay#text;
      small_retry_delay = float_of_string
        options#entry_options_small_retry_delay#text;
      medium_retry_delay = float_of_string 
        options#entry_options_medium_retry_delay#text;
      long_retry_delay = float_of_string
        options#entry_options_long_retry_delay#text;
        
        name = options#entry_options_name#text;
        password = 
      max_connected_servers = int_of_string 
        options#entry_options_maxconn_servers#text;
      features =  options#entry_options_features#text;
(*      download_limit = int_of_string
        options#entry_options_download_limit#text; *)
      upload_limit = int_of_string
        options#entry_options_upload_limit#text;

      server_timeout = float_of_string 
        options#entry_options_server_timeout#text;
      client_timeout = float_of_string 
        options#entry_options_client_timeout#text ;
      max_server_age = int_of_string
        options#entry_max_server_age#text;
} in
  *)
    port := int_of_string options#entry_options_gui_port#text;
    password := options#entry_options_password#text;
    gui_send (P.SaveOptions_query (List.map (fun 
         (name, widget) -> name, widget#text   
        ) options_assocs ))
  with _ ->
      Printf.printf "ERROR SAVING OPTIONS"; print_newline ()
      
let servers_remove (gui : gui) () = 
  let module P = Gui_proto in
  for_selection clist_servers (fun s ->
      gui_send (P.RemoveServer_query (server_key s));
  ) ()

(*

*)
  
let _ =
  
  ignore (window#add gui#box#coerce) ;
  ignore (gui#box#connect#destroy window#destroy) ;
  ignore (window#connect#destroy GMain.Main.quit) ;

(* Connect buttons to actions *)
  ignore (gui#itemQuit#connect#activate (fun _ -> exit 0));
  ignore (gui#itemKill#connect#activate (fun _ -> 
        gui_send KillServer));
  ignore (gui#itemReconnect#connect#activate (fun _ -> reconnect gui));
  ignore (gui#itemDisconnect#connect#activate (fun _ -> disconnect gui));
  ignore (gui#itemServers#connect#activate (fun _ -> gui#notebook#goto_page 0));
  ignore (gui#itemDownloads#connect#activate (fun _ -> gui#notebook#goto_page 1));
  ignore (gui#itemFriends#connect#activate (fun _ -> gui#notebook#goto_page 2));
  ignore (gui#itemSearches#connect#activate (fun _ -> gui#notebook#goto_page 3));
  ignore (gui#itemOptions#connect#activate (fun _ -> gui#notebook#goto_page 4));
  ignore (gui#itemHelp#connect#activate (fun _ -> gui#notebook#goto_page 5));
  ignore (tab_searches#button_search_submit#connect#clicked (submit_search gui false));
  ignore (tab_searches#button_local_search#connect#clicked (submit_search gui true));  
  
  ignore (tab_searches#button_extended_search#connect#clicked 
    (fun _ -> gui_send ExtendedSearch));

  ignore (tab_searches#entry_search_words#connect#activate (submit_search gui false));
(*
  ignore (tab_searches#clist_search_results#connect#select_row (search_set_selection gui));
ignore (tab_searches#clist_search_results#connect#unselect_row (search_unset_selection gui));
  *)
(*
  ignore (gui#clist_download#connect#select_row (download_set_selection gui));
ignore (gui#clist_download#connect#unselect_row (download_unset_selection gui));
  *)
  ignore (tab_downloads#button_download_cancel#connect#clicked
      (download_cancel gui));
  ignore (tab_servers#button_servers_add#connect#clicked (servers_addserver gui));
  ignore (tab_friends#button_friends_add#connect#clicked 
      (friends_addfriend gui));
  ignore (tab_servers#button_servers_connect_more#connect#clicked (
      servers_connect_more gui));
  ignore (tab_servers#button_servers_remove#connect#clicked (
      servers_remove gui));
  ignore (tab_options#button_options_save#connect#clicked (save_options gui));
  ignore (tab_options#entry_options_password#connect#activate (fun _ ->
        password := tab_options#entry_options_password#text;
    ));  
  ignore (gui#tab_console#entry_command#connect#activate (fun _ ->
        gui_send (Command gui#tab_console#entry_command#text);
        gui#tab_console#entry_command#set_text ""
    ));
  
  ignore (tab_options#entry_options_gui_port#connect#activate (fun _ ->
        port := int_of_string (tab_options#entry_options_gui_port#text);
    ));
  ignore (tab_options#entry_server_hostname#connect#activate (fun _ ->
        let h = Unix.gethostbyname (tab_options#entry_server_hostname#text) in
        addr := h.h_addr_list.(0)
    ));
  ignore (tab_friends#entry_dialog#connect#activate (fun _ ->
        let s = tab_friends#entry_dialog#text in
        gui_send (SayFriends (s, List.map (fun c -> c.client_num)
            (MyCList.selection clist_friends)
          ));
        tab_friends#entry_dialog#set_text "";
    ));
  
  
  ignore (tab_downloads#button_downloaded_save#connect#clicked 
    save_all_files);
  
  ignore (tab_downloads#button_download_add_friend#connect#clicked
      add_friend_location);
  
  ignore (tab_friends#button_friends_download#connect#clicked
      download_friend_files);
  ignore (tab_friends#button_friends_remove#connect#clicked
      remove_friend);
  ignore (tab_friends#entry_find_friend#connect#activate find_friend);
  
  ignore (tab_servers#button_remove_old_servers#connect#clicked
      remove_old_servers);
  ignore (tab_servers#button_servers_view_users#connect#clicked
      view_users);
  ignore (tab_downloads#button_download_retry_connect#connect#clicked
      connect_all);
  ignore (tab_servers#button_servers_connect#connect#clicked
      connect_server);
  ignore (tab_servers#button_servers_disconnect#connect#clicked
      disconnect_server);
  ignore (tab_servers#button_add_to_friends#connect#clicked 
      add_user_to_friends);
  
  ignore (gui#tab_console#button_clear_console#connect#clicked
      (fun _ ->
        let text = gui#tab_console#text in
        text#delete_text 0 (text#length)));
  
  ignore (tab_help#text#insert_text Gui_messages.help_text 0);
  
  ignore (tab_downloads#entry_ed2k_url#connect#activate
      download_ed2k_url);
  ignore (tab_downloads#entry_md4#connect#activate
      download_md4);
  
  ignore (tab_downloads#draw_availability#event#connect#expose
      ~callback:redraw_current);

  ignore (window#add_accel_group gui#accel_menubar);
  ignore (window#show ()) ;
  
  reconnect gui; 
  
  let gtk_handler timer =
    reactivate_timer timer;
    while Glib.Main.pending () do
      ignore (Glib.Main.iteration false)
    done;
  in
    
  add_timer 0.1 gtk_handler;
  add_timer 2.0 update_sizes;
  loop ()
