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

(** GUI for the lists of files. *)

open Printf2
open Options
open Gettext
open CommonTypes
open GuiTypes
open Gui_types
open Gui_columns
open GMain

module M = Gui_messages
module P = Gpattern
module O = Gui_options
module Mi = Gui_misc
module G = Gui_global

let (!!) = Options.(!!)


let the_col_width = ref 70

let string_color_of_state state =
  match state with
  | Connected_downloading -> gettext M.downloading, Some !!O.color_downloading 
  | Connected (-1) -> gettext M.connected, Some !!O.color_connected 
  | Connecting  -> gettext M.connecting, Some !!O.color_connecting
  | NewHost -> "NEW HOST", None
  | Connected_initiating -> gettext M.initiating, Some !!O.color_not_connected
  | Connected 0 -> gettext M.queued, Some !!O.color_connected
  | Connected n -> Printf.sprintf "Ranked %d" n, Some !!O.color_connected
  | NotConnected (_,n) -> 
      if n = -1 then
        "", None
      else
      if n = 0 then
        "Queued out",  Some !!O.color_not_connected 
      else
      if n > 0 then
        Printf.sprintf "Ranked %d Out" n,  Some !!O.color_not_connected
      else
        Printf.sprintf "Failed %d" (- n - 1), Some !!O.color_not_connected
        
  | RemovedHost -> gettext M.removed, Some !!O.color_not_connected
  | BlackListedHost -> gettext M.black_listed, Some !!O.color_not_connected
      
let string_color_of_client friend_tab c =
  match c.gclient_files with
    Some _ when friend_tab -> 
      gettext M.o_col_files_listed, Some !!O.color_files_listed
  | _ -> string_color_of_state c.gclient_state

let shorten maxlen s =
  let len = String.length s in
  if len > maxlen then
    (String.sub s 0 (maxlen-3)) ^ "..."
  else if s = "" then
      "http://go.to/mldonkey"
  else s

let state_pix state =
    match state with
        Connected_downloading -> O.gdk_pix M.o_xpm_downloading
      | Connected (-1) -> O.gdk_pix M.o_xpm_connect_y
      | Connecting  -> O.gdk_pix M.o_xpm_connect_m
      | NewHost -> O.gdk_pix M.o_xpm_connect_n
      | Connected_initiating -> O.gdk_pix M.o_xpm_connect_m
      | Connected 0 -> O.gdk_pix M.o_xpm_connect_y
      | Connected n -> O.gdk_pix M.o_xpm_connect_y
      | NotConnected (_,n) -> O.gdk_pix M.o_xpm_connect_n
      | RemovedHost -> O.gdk_pix M.o_xpm_removedhost
      | BlackListedHost -> O.gdk_pix M.o_xpm_blacklistedhost

let client_pix c =
  match c.client_files with
    Some l -> O.gdk_pix M.o_xpm_files_listed
  | _ -> state_pix c.client_state


let type_pix t =
  if t land client_friend_tag <> 0 then O.gdk_pix M.o_xpm_friend_user else
  if t land client_contact_tag <> 0 then  O.gdk_pix M.o_xpm_contact_user else
    O.gdk_pix M.o_xpm_normal_user


let get_friend_pix c =
  let pix1 = type_pix c.client_type in
  let pix2 = client_pix c in
  let pixmap = GDraw.pixmap ~width:32 ~height:16 ~mask:true
                 ~colormap:(Gdk.Color.get_system_colormap ()) ()
  in
  let mask = match pixmap#mask with Some m -> m | None -> assert false in
  let wmask = new GDraw.drawable mask in
  let _ = match  pix1#mask with
              Some m ->
                  let image = Gdk.Image.get m ~x:0 ~y:0 ~width:16 ~height:16 in
                  let pixel = Gdk.Color.pixel (GDraw.color `BLACK) in
                  for i = 0 to 15 do
                    for j = 0 to 15 do
                      let col =
                        if Gdk.Image.get_pixel image ~x:i ~y:j  = pixel then
                          `BLACK
                          else `WHITE
                      in
                      wmask#set_foreground col;
                      wmask#point ~x:i ~y:j
                    done
                  done
            | None -> ()
  in
  let _ = match  pix2#mask with
              Some m ->
                  let image = Gdk.Image.get m ~x:0 ~y:0 ~width:16 ~height:16 in
                  let pixel = Gdk.Color.pixel (GDraw.color `BLACK) in
                  for i = 0 to 15 do
                    for j = 0 to 15 do
                      let col =
                        if Gdk.Image.get_pixel image ~x:i ~y:j  = pixel then
                          `BLACK
                          else `WHITE
                      in
                      wmask#set_foreground col;
                      wmask#point ~x:(i + 16) ~y:j
                    done
                  done
            | None -> ()
  in
  pixmap#put_pixmap ~x:0 ~y:0 ~xsrc:0 ~ysrc:0 ~width:16 ~height:16 pix1#pixmap;
  pixmap#put_pixmap ~x:16 ~y:0 ~xsrc:0 ~ysrc:0 ~width:16 ~height:16 pix2#pixmap;
  pixmap



class dialog friend =
  object (self)
    inherit Gui_friends_base.dialog ()
    
    val mutable name = friend.gclient_name
    method name =  name
    method friend = friend
    method num = friend.gclient_num
    
    method send s =
      Gui_com.send (GuiProto.MessageToClient (friend.gclient_num, s))
    
    method handle_message mes =
      wt_dialog#insert ~foreground: (Gui_misc.color_of_name name) name;
      wt_dialog#insert (" : "^mes^"\n");
      wt_dialog#set_position (wt_dialog#length - 1)
    
    initializer
    let return () = 
      let s = wt_input#get_chars 0 wt_input#length in
      let len = String.length s in
      let s2 = 
        if len <= 0 then s
        else
        match s.[0] with
          '\n' -> String.sub s 1 (len - 1)
        | _ -> s
      in
      self#send s2;
      wt_dialog#insert 
        ~foreground: (Gui_misc.color_of_name !Gui_options.client_name)
      !Gui_options.client_name;
      wt_dialog#insert (" : "^s2^"\n") ;
      wt_input#delete_text ~start: 0 ~stop: wt_input#length
    
    in
    Okey.add wt_input ~mods: [] GdkKeysyms._Return return;
    Okey.add_list wt_input ~mods: [`CONTROL]
      [GdkKeysyms._c; GdkKeysyms._C]
      box#destroy;
    Okey.add_list wt_dialog ~mods: [`CONTROL] 
      [GdkKeysyms._c; GdkKeysyms._C]
      box#destroy;

end



class box columns friend_tab =
  let titles = List.map Gui_columns.Client.string_of_column !!columns in
  object (self)
    inherit [gui_client_info] Gpattern.filtered_plist `EXTENDED titles true (fun c -> c.gclient_num) as pl
      inherit Gui_friends_base.box () as box
    
    val mutable columns = columns

    method filter = (fun _ -> false)

    method set_list_bg bg font =
      let wlist = self#wlist in
      let style = wlist#misc#style#copy in
      style#set_base [ (`NORMAL, bg)];
      style#set_font font;
      wlist#misc#set_style style;
      wlist#set_row_height 18;
      wlist#columns_autosize ()

    method set_columns l =
      columns <- l;
      self#set_titles (List.map Gui_columns.Client.string_of_column !!columns);
      self#update;
      self#set_list_bg (`NAME !!O.color_list_bg)
        (Gdk.Font.load_fontset !!O.font_list)
      
    method column_menu  i = 
      [
        `I (gettext M.mAutosize, fun _ -> GToolbox.autosize_clist self#wlist);
        `I (gettext M.mSort, self#resort_column i);
        `I (gettext M.mRemove_column,
          (fun _ -> 
              match !!columns with
                _ :: _ :: _ ->
                                                      (let l = !!columns in
                    match List2.cut i l with
                      l1, _ :: l2 ->
                        columns =:= l1 @ l2;
                        self#set_columns columns
                    | _ -> ())

                  
              | _ -> ()
          )
        );
        `M (gettext M.mAdd_column_after, (
            List.map (fun (c,s) ->
                (`I (s, (fun _ -> 
                        let c1, c2 = List2.cut (i+1) !!columns in
                        columns =:= c1 @ [c] @ c2;
                        self#set_columns columns
                    )))
            ) Gui_columns.Client.column_strings));
        `M (gettext M.mAdd_column_before, (
            List.map (fun (c,s) ->
                (`I (s, (fun _ -> 
                        let c1, c2 = List2.cut i !!columns in
                        columns =:= c1 @ [c] @ c2;
                        self#set_columns columns
                    )))
            ) Gui_columns.Client.column_strings));
      ]

    method coerce = box#vbox#coerce
    
    method compare_by_col col f1 f2 =
      match col with
        Col_client_name -> compare f1.gclient_name f2.gclient_name
      |	Col_client_state -> compare f1.gclient_state f2.gclient_state
      |	Col_client_kind -> compare f1.gclient_kind f2.gclient_kind
      | Col_client_network -> compare f1.gclient_network f2.gclient_network
      | Col_client_type -> compare f1.gclient_type f2.gclient_type
      | Col_client_rating -> compare f1.gclient_rating f2.gclient_rating
      | Col_client_connect_time -> compare f1.gclient_connect_time f2.gclient_connect_time
      | Col_client_software -> compare f1.gclient_software f2.gclient_software
      | Col_client_downloaded -> compare f1.gclient_downloaded f2.gclient_downloaded
      | Col_client_uploaded -> compare f1.gclient_uploaded f2.gclient_uploaded
      | Col_client_upload -> compare f1.gclient_upload f2.gclient_upload
      | Col_client_sock_addr -> compare f1.gclient_sock_addr f2.gclient_sock_addr
    
    method compare f1 f2 =
      let abs = if current_sort >= 0 then current_sort else - current_sort in
      let col = 
        try List.nth !!columns (abs - 1) 
        with _ -> Col_client_name
      in
      let res = self#compare_by_col col f1 f2 in
      res * current_sort
    
    method content_by_col f col =
      match col with
        Col_client_name -> shorten !!O.max_client_name_len f.gclient_name
      | Col_client_state -> fst (string_color_of_client friend_tab f)
      | Col_client_type -> (let t =  f.gclient_type in
            if t land client_friend_tag <> 0 then gettext M.friend else
            if t land client_contact_tag <> 0 then gettext M.contact else
              "Y")
      | Col_client_network -> Gui_global.network_name f.gclient_network
      | Col_client_kind -> (
          match f.gclient_kind with
            Known_location _ -> gettext M.direct
          | _ -> "")
      | Col_client_rating -> string_of_int f.gclient_rating
      | Col_client_connect_time -> Gui_graph.time_to_string (BasicSocket.last_time () - f.gclient_connect_time)
      | Col_client_software -> f.gclient_software
      | Col_client_downloaded -> Gui_misc.size_of_int64 f.gclient_downloaded
      | Col_client_uploaded -> Gui_misc.size_of_int64 f.gclient_uploaded
      | Col_client_upload -> (match f.gclient_upload with
                                 Some s -> s
                               | _ -> "")
      | Col_client_sock_addr -> f.gclient_sock_addr

              
    method content f =
      let strings = List.map 
          (fun col -> match col with
               Col_client_name ->
                 (match f.gclient_pixmap with
                     Some pixmap -> P.Pixtext (self#content_by_col f col, pixmap)
                   | _ -> P.String (self#content_by_col f col))
             | Col_client_network ->
                 (match f.gclient_net_pixmap with
                     Some pixmap -> P.Pixmap (pixmap)
                   | _ -> P.String (self#content_by_col f col))
             | _ -> P.String (self#content_by_col f col))
	  !!columns 
      in
      let col_opt = 
	match snd (string_color_of_client friend_tab f) with
	  None -> Some `BLACK
	| Some c -> Some (`NAME c)
      in
      (strings, col_opt)

    method find_client num = self#find num

    method set_tb_style tb = 
        if Options.(!!) Gui_options.mini_toolbars then
          (wtool1#misc#hide (); wtool2#misc#show ()) else
          (wtool2#misc#hide (); wtool1#misc#show ());
      wtool1#set_style tb;
      wtool2#set_style tb

    initializer
      box#vbox#pack ~expand: true pl#box
  end


let is_filtered c =
  List.memq c.gclient_network !Gui_global.networks_filtered

class box_friends box_files friend_tab =
  object (self)
    inherit box O.friends_columns friend_tab
    
    val mutable box_friends_is_visible = (false : bool)
    val mutable icons_are_used = (!!O.use_icons : bool)
    
    
    method filter = is_filtered    
    
    method filter_networks = self#refresh_filter
    
    method remove () = 
      List.iter
        (fun c -> Gui_com.send (GuiProto.RemoveFriend c.gclient_num))
      self#selection
    
    method remove_all_friends () = 
      self#clear;
      box_files#clear;
      Gui_com.send GuiProto.RemoveAllFriends
    
    method find_friend () =
      match GToolbox.input_string (gettext M.find_friend) (gettext M.name) with
        None -> ()
      |	Some s ->
          Gui_com.send (GuiProto.FindFriend s)
    
    method on_select c =
(* in case we select several rows it is not necessary
    to display the files of each client selected (10 clients selected = 10 updates of files list).
    It is a non sens because the user do not select several rows
    for that purpose, but instead would prefer to suppress them ...
    We will display only the first one instead of the last one
    (like this there is only display the first one and not display the
    first one and then the last one).
    Furthermore this loads the CPU with the pixmaps and can
    block everything => see with Soulseek *)
      if c = List.hd (List.rev self#selection) then
        match c.gclient_files with
          None -> 
(*          lprintf "No file for friend %d" c.client_num; lprint_newline (); *)
            Gui_com.send (GuiProto.GetClient_files c.gclient_num)
        
        |	Some tree -> 
(*          lprintf "%d files for friend %d" (List.length l) c.client_num; 
          lprint_newline (); *)
            begin
              let w = self#wlist#misc#window in
              box_files#update_gdk_window w;
(* Printf.printf "Gui_friends on_select %d\n" c.gclient_num;
              flush stdout;*)
              let (row, fi) = self#find_client c.gclient_num in
              let f = self#to_core_client fi in
              fi.gclient_pixmap <-
              if icons_are_used then
                Some (get_friend_pix f)
              else None;
              self#update_row fi row;
              box_files#update_tree (Some tree)
            end
    
    method on_deselect f =
(* Printf.printf "Gui_friends on_deselect %d\n" f.gclient_num;
      flush stdout;*)
      box_files#update_tree None
    
    val mutable on_double_click = (fun _ -> ())
    
    method set_on_double_click f = on_double_click <- f
    
    method on_double_click f = on_double_click f
    
    method menu =
      match self#selection with
        [] -> [ `I (gettext M.find_friend, self#find_friend) ;
            `I (gettext M.remove_all_friends_text, self#remove_all_friends)]
      |	_ -> [ `I (gettext M.find_friend, self#find_friend) ;
            `I (gettext M.remove, self#remove) ;
            `I (gettext M.remove_all_friends_text, self#remove_all_friends)]
    
    method to_core_client c =
      {
        client_num = c.gclient_num;
        client_network = c.gclient_network;
        client_kind = c.gclient_kind;
        client_state = c.gclient_state;
        client_type = c.gclient_type;
        client_tags = c.gclient_tags;
        client_name = c.gclient_name;
        client_files = c.gclient_files;
        client_rating = c.gclient_rating;
        client_chat_port = 0;
        client_connect_time = c.gclient_connect_time;
        client_software = c.gclient_software;
        client_downloaded = c.gclient_downloaded;
        client_uploaded = c.gclient_uploaded;
        client_upload = c.gclient_upload;
(*        client_sock_addr = c.gclient_sock_addr;*)
      }
    
    method to_gui_client c =
      {
        gclient_num = c.client_num;
        gclient_network = c.client_network;
        gclient_kind = c.client_kind;
        gclient_state = c.client_state;
        gclient_type = c.client_type;
        gclient_tags = c.client_tags;
        gclient_name = c.client_name;
        gclient_files = c.client_files;
        gclient_rating = c.client_rating;
        gclient_connect_time = c.client_connect_time;
        gclient_software = c.client_software;
        gclient_downloaded = c.client_downloaded;
        gclient_uploaded = c.client_uploaded;
        gclient_upload = c.client_upload;
        gclient_sock_addr = string_of_kind c.client_kind;
        gclient_net_pixmap =
        if icons_are_used then
          Some (Gui_options.network_pix
              (Gui_global.network_name c.client_network))
        else None;
        gclient_pixmap =
        if icons_are_used then
          Some (get_friend_pix c)
        else None;
      }
    
    method update_friend f_new =
      if client_browsed_tag land f_new.client_type = 0 then
        self#h_remove_friend f_new.client_num
      else
      try
        begin
          let (row, f) = self#find_client f_new.client_num in
          f.gclient_state <- f_new.client_state;
          f.gclient_type <- f_new.client_type;
          f.gclient_pixmap <-
          if icons_are_used then
            Some (get_friend_pix f_new)
          else None;
          f.gclient_name <- f_new.client_name;
          f.gclient_kind <- f_new.client_kind;
(* added *)
          f.gclient_tags <- f_new.client_tags;
          f.gclient_rating <- f_new.client_rating;
          f.gclient_connect_time <-  f_new.client_connect_time;
          f.gclient_software <- f_new.client_software;
          f.gclient_downloaded <- f_new.client_downloaded;
          f.gclient_uploaded <- f_new.client_uploaded;
          f.gclient_upload <- f_new.client_upload;
          f.gclient_sock_addr <- string_of_kind f_new.client_kind;
          if box_friends_is_visible then self#update_row f row
        end
      with
        Not_found ->
          let fi = self#to_gui_client f_new in
          self#add_item fi
    
    method is_visible b =
      box_friends_is_visible <- b
    
    method h_remove_friend num =
      try
        let (row, i) = self#find_client num in
        self#remove_item row i;
        selection <- List.filter (fun fi -> fi.gclient_num <> num) selection
      with
        Not_found -> ()
    
    method update_friend_state (num, state) =
      try
        let (row, fi) = self#find_client num in
        fi.gclient_state <- state;
        fi.gclient_pixmap <-
        if icons_are_used then
          Some (get_friend_pix (self#to_core_client fi))
        else None;
        if box_friends_is_visible then self#update_row fi row
      with
        Not_found -> ()
    
    method update_friend_type (num, friend_kind) =
      try
        let (row, fi) = self#find_client num in
        if client_browsed_tag land friend_kind = 0 then
          self#h_remove_friend num
        else begin
            fi.gclient_type <- friend_kind;
            fi.gclient_pixmap <-
            if icons_are_used then
              Some (get_friend_pix (self#to_core_client fi))
            else None;
            if box_friends_is_visible then self#update_row fi row
          end
      with
        Not_found -> ()

    method add_friend_files (num , dirname, file_num) =
      try
        let file = Hashtbl.find G.results file_num in
          try
            let (_, c) = self#find_client num in
              try
                let tree = match c.gclient_files with
                    None -> { file_tree_list = []; file_tree_name = "" }
                  | Some tree -> { tree with file_tree_list = tree.file_tree_list }
                in
                add_file tree dirname file;
                c.gclient_files <- Some tree
              with _ ->
(*                  lprintf "File already there"; lprint_newline (); *)
                  ()
            with _ ->
(*                lprintf "Unknown client %d" num; lprint_newline (); *)
                (* Gui_com.send (GuiProto.GetClient_info num); *)
                ()
          with _ ->
(*              lprintf "Unknown file %d" file_num;
              lprint_newline (); *)
              ()

    method update_icons b =
      icons_are_used <- b;
      let (f, label, step) =
        if b then
          ((fun c ->
            c.gclient_net_pixmap <-
              Some (Gui_options.network_pix
                     (Gui_global.network_name c.gclient_network));
            c.gclient_pixmap <-
              Some (get_friend_pix (self#to_core_client c));
          ), gettext M.friends_add_icons, 1)
          else
            ((fun c ->
              c.gclient_net_pixmap <- None;
              c.gclient_pixmap <- None;
            ), gettext M.friends_remove_icons, 10)
      in
      Gui_options.generate_with_progress label self#get_all_items f step

end

let is_filtered2 c =
  ((match c.gclient_upload with
      None -> true
    | _ -> false) ||
  (List.memq c.gclient_network !G.networks_filtered))



class box_list friend_tab =
  let vbox_list = GPack.vbox () in
  
  object (self)
    inherit box O.file_locations_columns friend_tab as prebox
    
    val mutable c_to_update = ([] : int list)
    val mutable icons_are_used = (!!O.use_icons : bool)

    method filter = is_filtered2

    method retrieve_clients (c_num_list : int * (int list)) =
      (* Printf.printf "Gui_friends Received Clients : %d\n" (List.length (snd c_num_list));
      flush stdout; *)
      let l = ref [] in
      let file_num = fst c_num_list in
      let num_list = snd c_num_list in
      List.iter ( fun num ->
        try
          let (_, c) = self#find num in
          l := c::!l
        with _ -> ()
      ) num_list;
      (* Printf.printf "Gui_friends Sent Clients : %d\n" (List.length !l);
      flush stdout; *)
      (file_num, !l)

    method coerce = vbox_list#coerce
    
    method add_to_friends () = 
      List.iter
        (fun c -> 
          if c.gclient_name <> "" then
            Gui_com.send (GuiProto.AddClientFriend c.gclient_num))
      self#selection
    
    method menu =
      match self#selection with
        [] -> []
      |	_ -> [ `I (gettext M.add_to_friends, self#add_to_friends) ]
    
    method to_core_client c =
      {
        client_num = c.gclient_num;
        client_network = c.gclient_network;
        client_kind = c.gclient_kind;
        client_state = c.gclient_state;
        client_type = c.gclient_type;
        client_tags = c.gclient_tags;
        client_name = c.gclient_name;
        client_files = c.gclient_files;
        client_rating = c.gclient_rating;
        client_chat_port = 0;
        client_connect_time = c.gclient_connect_time;
        client_software = c.gclient_software;
        client_downloaded = c.gclient_downloaded;
        client_uploaded = c.gclient_uploaded;
        client_upload = c.gclient_upload;
(*        client_sock_addr = string_of_kind c.gclient_kind; *)
      }

    method to_gui_client c =
      {
        gclient_num = c.client_num;
        gclient_network = c.client_network;
        gclient_kind = c.client_kind;
        gclient_state = c.client_state;
        gclient_type = c.client_type;
        gclient_tags = c.client_tags;
        gclient_name = c.client_name;
        gclient_files = c.client_files;
        gclient_rating = c.client_rating;
        gclient_connect_time =  c.client_connect_time;
        gclient_software = c.client_software;
        gclient_downloaded = c.client_downloaded;
        gclient_uploaded = c.client_uploaded;
        gclient_upload = c.client_upload;
        gclient_sock_addr = string_of_kind c.client_kind;
        gclient_net_pixmap = None;
        gclient_pixmap = None;
      }

    (* the core does not treat client_downloaded & client_uploaded changes as event
    As a consequence to display correctly these values we need to ask the core to
    send them again. We will fill a list of clients to be updated. This list will be
    sent every 6 seconds *)
    method fill_c_to_update c =
      if not (List.mem c.gclient_num c_to_update) then
        match c.gclient_state, c.gclient_upload with
            Connected_downloading, _ -> c_to_update <- c.gclient_num::c_to_update
          | _, Some s ->  c_to_update <- c.gclient_num::c_to_update
          | _ -> ()
                          
    method send_and_flush =
      List.iter (fun num ->
                                  Gui_com.send (GuiProto.GetClient_info num)
      ) c_to_update;
      c_to_update <- []
                
    method update_client c_new =
      try
        let (row, c) = self#find_client c_new.client_num in
        if c_new.client_state = RemovedHost then
          begin
(*            lprintf "Removing client from locations panel"; 
            lprint_newline (); *)
            self#remove_item row c
          end
          else begin
            c.gclient_state <- c_new.client_state;
            c.gclient_rating <- c_new.client_rating;
            c.gclient_connect_time <- c_new.client_connect_time;
            c.gclient_name <- c_new.client_name;
            c.gclient_kind <- c_new.client_kind;
            c.gclient_tags <- c_new.client_tags;
            c.gclient_software <- c_new.client_software;
            c.gclient_downloaded <- c_new.client_downloaded;
            c.gclient_uploaded <- c_new.client_uploaded;
            (* here it seems that client_upload is not set by the core
            to None sometimes when the client leaves the uploads. Or maybe
            the core just sends update_client_state. So we need to reset it here *)
            (match c.gclient_state with
                Connected _
              | Connected_downloading -> c.gclient_upload <- c_new.client_upload
              | _ -> c.gclient_upload <- None );
            c.gclient_sock_addr <- string_of_kind c_new.client_kind;
            if icons_are_used && (not (self#filter c)) && (c.gclient_net_pixmap = None) then
              c.gclient_net_pixmap <-
                Some (Gui_options.network_pix
                        (Gui_global.network_name c_new.client_network));
            if icons_are_used && (not (self#filter c)) && (c.gclient_pixmap = None) then
              begin
                c.gclient_type <- c_new.client_type;
                c.gclient_pixmap <-
                  Some (type_pix c_new.client_type)
              end;
            if c.gclient_type <> c_new.client_type then
              begin
                c.gclient_type <- c_new.client_type;
                c.gclient_pixmap <-
                  if icons_are_used && (not (self#filter c)) then
                    Some (type_pix c_new.client_type)
                    else None
              end;
            self#fill_c_to_update c;
            self#refresh_item row c
          end
      with
        Not_found ->
          let ci = self#to_gui_client c_new in
          self#add_item ci;
          if icons_are_used && (not (self#filter ci)) then
            Gui_com.send (GuiProto.GetClient_info ci.gclient_num)


    method update_client_state (num, state) =
      try
        let (row, c) = self#find_client num in
        (* here it seems that client_upload is not set by the core
        to None sometimes when the client leaves the uploads. Or maybe
        the core just sends update_client_state. So we need to reset it here *)
        (match c.gclient_state with
                Connected _
              | Connected_downloading -> ()
              | _ -> c.gclient_upload <- None );
        c.gclient_state <- state;
        self#fill_c_to_update c;
        self#refresh_item row c
      with
        Not_found ->
          Gui_com.send (GuiProto.GetClient_info num)

    method update_client_type (num, friend_kind) =
      try
        let (row, c) = self#find_client num in
        c.gclient_type <- friend_kind;
        c.gclient_pixmap <-
          if icons_are_used && (not (self#filter c)) then
            Some (type_pix c.gclient_type)
            else None;
        self#refresh_item row c
      with
        Not_found ->
          Gui_com.send (GuiProto.GetClient_info num)
    
    method clean_table clients =
      (* Printf.printf "Gui_friends Clean Table\n";
      flush stdout; *)
      let data = ref [] in
      List.iter (fun (c_num :int) ->
          try
            let row, c = self#find_client c_num in
            data := c :: !data
          with _ -> ()
      ) clients;
      self#reset_data !data
    

    method update_icons b =
      icons_are_used <- b;
      let (f, label, step) =
        if b then
          ((fun c ->
            if (not (self#filter c)) then
              begin
                c.gclient_net_pixmap <-
                  Some (Gui_options.network_pix
                          (Gui_global.network_name c.gclient_network));
                c.gclient_pixmap <-
                  Some (type_pix c.gclient_type)
              end
          ), gettext M.uploads_add_icons, 20)
          else
            ((fun c ->
              c.gclient_net_pixmap <- None;
              c.gclient_pixmap <- None;
            ), gettext M.uploads_remove_icons, 200)
      in
      Gui_options.generate_with_progress label self#get_all_items f step
    
    initializer
      vbox_list#pack ~expand: true prebox#coerce;
      
      ignore(Timeout.add ~ms:6000 ~callback:
        (fun _ -> self#send_and_flush;
                  true))
(*
      Gui_misc.insert_buttons wtool1 wtool2
        ~text: (gettext M.add_to_friends)
      ~tooltip: (gettext M.add_to_friends)
      ~icon: (M.o_xpm_add_to_friends)
      ~callback: self#add_to_friends
        ()
  *)            

  end


class pane_friends () =
  let files = new Gui_results.box_dir_files () in
  let friends = new box_friends files true in
  let wnote_chat =
    GPack.notebook ~homogeneous_tabs:false ~show_border:true
      ~scrollable:true ~popup:false ()
  in
  object (self)
    (** The list of open chat dialogs *)
    val mutable dialogs = ([] : dialog list)

    (** Remove the dialog with the given client num from the list of dialogs. *)
    method remove_dialog c_num =
      (
       try
	 let d = List.find (fun d -> d#num = c_num) dialogs in
	 dialogs <- List.filter (fun d -> not (d#num = c_num)) dialogs;
	 let n = wnote_chat#page_num d#coerce in
	 wnote_chat#remove_page n
       with
	 Not_found ->
	   ()
      );

    (** Find the window and dialog with the given client. If
       it was not found, create it and add it to the list of dialogs.*)
    method get_dialog client =
      try
	let d = List.find 
	    (fun d -> d#num = client.gclient_num)
	    dialogs 
	in
	d#wt_input#misc#grab_focus ();
	d
      with
	Not_found ->
	  let dialog = new dialog client in
	  let hbox = GPack.hbox ~homogeneous:false ~spacing:5 () in
	  let wl =
	    GMisc.label ~text: client.gclient_name
	      ~packing:(hbox#pack ~expand:true ~fill:true) ()
	  in
	  let hbox1 =
	    GPack.hbox ~homogeneous:false
	      ~packing:(hbox#pack ~expand:false ~fill:false) () in
	  let button = GButton.button ~packing:(hbox1#pack ~expand:false ~fill:false) () in
	  let close_pix =
	    GMisc.pixmap (O.gdk_pix M.o_xpm_mini_close_search)
		~packing:(button#add) ()
	  in
	  ignore (button#connect#clicked ~callback:(fun _ -> dialog#box#destroy ()));
	  wnote_chat#append_page ~tab_label: hbox#coerce dialog#coerce;
	  ignore (dialog#box#connect#destroy
		    (fun () -> dialogs <- List.filter (fun d -> not (d#num = client.gclient_num)) dialogs));
	  dialogs <- dialog :: dialogs;
	  dialog#wt_input#misc#grab_focus ();
	  dialog      

    inherit Gui_friends_base.paned ()

    method box_friends = friends
    method box_files = files
    method hpaned = wpane
    method vpaned = wpane2

    method is_visible b = friends#is_visible b

    method h_add_friend_files (num , dirname, file_num) =
      friends#add_friend_files (num , dirname, file_num)

    method h_update_friend_state (num, state) =
      friends#update_friend_state (num, state)

    method h_update_friend_type (num, friend_kind) =
      friends#update_friend_type (num, friend_kind)

    method h_update_friend c =
      friends#update_friend c

    method c_update_icons b =
      friends#update_icons b ;
      files#c_update_icons b

    method clear =
      files#clear ;
      friends#clear

    method set_tb_style tb =
      files#set_tb_style tb ;
      friends#set_tb_style tb

    method set_list_bg bg font =
      files#set_list_bg bg font;
      friends#set_list_bg bg font

    initializer

      friends#set_on_double_click (fun f -> ignore (self#get_dialog f));

      wpane#add1 friends#coerce;
      vbox2#pack wnote_chat#coerce ~expand:true ~fill:true;
      wpane2#add1 files#coerce;

      let style = evbox1#misc#style#copy in
      style#set_bg [ (`NORMAL, (`NAME "#494949"))];
      evbox1#misc#set_style style;
      let style = label#misc#style#copy in
      style#set_fg [ (`NORMAL, `WHITE)];
      label#misc#set_style style

  end
