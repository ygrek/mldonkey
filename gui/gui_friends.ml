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

open CommonTypes
open GuiTypes
open Gui_columns

module M = Gui_messages
module P = Gpattern
module O = Gui_options
module Mi = Gui_misc
module G = Gui_global

let (!!) = Options.(!!)

let string_color_of_state state =
  match state with
  | Connected_busy -> M.downloading, Some !!O.color_downloading 
  | Connected_idle -> M.connected, Some !!O.color_connected 
  | Connecting  -> M.connecting, Some !!O.color_connecting
  | NotConnected
  | NewHost -> "", None
  | Connected_initiating -> M.initiating, Some !!O.color_not_connected
  | Connected_queued -> M.queued, Some !!O.color_not_connected
  | RemovedHost -> M.removed, Some !!O.color_not_connected
  
let string_color_of_client c =
  match c.client_files with
    Some _ -> M.o_col_files_listed, Some !!O.color_downloading 
  | _ -> string_color_of_state c.client_state



class dialog friend =
  object (self)
    inherit Gui_friends_base.dialog ()

    val mutable name = friend.client_name
    method name =  name
    method friend = friend
    method num = friend.client_num

    method send s =
      Printf.printf "MessageToClient(%d,%s)" friend.client_num s;
      print_newline ();
      Gui_com.send (GuiProto.MessageToClient (friend.client_num, s))

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

class box columns () =
  let titles = List.map Gui_columns.Client.string_of_column columns in
  object (self)
    inherit [client_info] Gpattern.plist `EXTENDED titles true as pl
    inherit Gui_friends_base.box () as box

    val mutable columns = columns
    method set_columns l =
      columns <- l;
      self#set_titles (List.map Gui_columns.Client.string_of_column columns);
      self#update

    method coerce = box#vbox#coerce

    method compare_by_col col f1 f2 =
      match col with
	Col_client_name -> compare f1.client_name f2.client_name
      |	Col_client_state -> compare f1.client_state f2.client_state
      |	Col_client_kind -> compare f1.client_kind f2.client_kind
      | Col_client_network -> compare f1.client_network f2.client_network
      | Col_client_type -> compare f1.client_type f2.client_type
          
          
    method compare f1 f2 =
      let abs = if current_sort >= 0 then current_sort else - current_sort in
      let col = 
	try List.nth columns (abs - 1) 
	with _ -> Col_client_name
      in
      let res = self#compare_by_col col f1 f2 in
      res * current_sort

    method content_by_col f col =
      match col with
	Col_client_name -> f.client_name
      |	Col_client_state -> fst (string_color_of_client f)
      |  Col_client_type -> (match f.client_type with
              FriendClient -> M.friend
            | ContactClient -> M.contact
            | NormalClient -> "")
      |  Col_client_network -> Gui_global.network_name f.client_network
      |	Col_client_kind -> 
	  match f.client_kind with
            Known_location _ -> M.direct
	  | _ -> ""

    method content f =
      let strings = List.map 
	  (fun col -> P.String (self#content_by_col f col))
	  columns 
      in
      let col_opt = 
	match snd (string_color_of_client f) with
	  None -> Some `BLACK
	| Some c -> Some (`NAME c)
      in
      (strings, col_opt)

    method find_client num =
      let rec iter n l =
        match l with
          [] -> raise Not_found
        | c :: q ->
            if c.client_num = num then
              (n, c)
            else
              iter (n+1) q
      in
      iter 0 data

    method set_tb_style = wtool#set_style

    method clear = self#update_data []

    initializer
      box#vbox#pack ~expand: true pl#box
  end


let is_filtered c =
  List.memq c.client_network !Gui_global.networks_filtered


class box_friends box_results () =
  object (self)
    inherit box !!O.friends_columns ()

    val mutable filtered_data = []
      
    method filter_networks = 
      let data = data @ filtered_data in
      let rec iter filtered not_filtered data =
        match data with
          [] -> List.rev filtered, List.rev not_filtered
        | s :: tail ->
            if is_filtered s then
              iter (s :: filtered) not_filtered tail
            else
              iter filtered (s :: not_filtered) tail
      in
      let (filtered, not_filtered) = iter [] [] data in
      filtered_data <- filtered;
      self#update_data not_filtered

    method remove () = 
      List.iter
	(fun c -> Gui_com.send (GuiProto.RemoveFriend c.client_num))
	self#selection

    method remove_all_friends () = 
      self#update_data [];
      Gui_com.send GuiProto.RemoveAllFriends

    method find_friend () =
      match GToolbox.input_string M.find_friend M.name with
	None -> ()
      |	Some s ->
	  Gui_com.send (GuiProto.FindFriend s)

    method on_select c =
      match c.client_files with
        None -> 
(*          Printf.printf "No file for friend %d" c.client_num; print_newline (); *)
          Gui_com.send (GuiProto.GetClient_files c.client_num)
      |	Some tree -> 
(*          Printf.printf "%d files for friend %d" (List.length l) c.client_num; 
          print_newline (); *)
          box_results#update_data (list_files tree)

    method on_deselect f =
      box_results#update_data []

    val mutable on_double_click = (fun _ -> ())
    method set_on_double_click f = on_double_click <- f
    method on_double_click f = on_double_click f

    method menu =
      match self#selection with
	[] -> [ `I (M.find_friend, self#find_friend) ;
		`I (M.remove_all_friends, self#remove_all_friends)]
      |	_ -> [ `I (M.find_friend, self#find_friend) ;
	       `I (M.remove, self#remove) ;
	       `I (M.remove_all_friends, self#remove_all_friends)]


    method h_update_friend f_new =
      try
        let (row, f) = self#find_client f_new.client_num in
        if f_new.client_files <> None then 
          f.client_files <- f_new.client_files;
        f.client_state <- f_new.client_state;
        f.client_type <- f_new.client_type;
        f.client_rating <- f_new.client_rating;
        f.client_name <- f_new.client_name;
        
        f.client_kind <- f_new.client_kind;
        f.client_tags <- f_new.client_tags;
        self#update_row f row
      with
	Not_found ->
	  data <- data @ [f_new] ;
	  self#insert ~row: self#wlist#rows f_new

    method h_remove_friend num =
      try
	let (row, _) = self#find_client num in
	self#wlist#remove row;
	data <- List.filter (fun fi -> fi.client_num <> num) data ;
	selection <- List.filter (fun fi -> fi.client_num <> num) selection
      with
	Not_found -> ()

    initializer
      ignore
	(wtool#insert_button 
	   ~text: M.find_friend
	   ~tooltip: M.find_friend
	   ~icon: (Gui_icons.pixmap M.o_xpm_find_friend)#coerce
	   ~callback: self#find_friend
	   ()
	);
      ignore
	(wtool#insert_button 
	   ~text: M.remove
	   ~tooltip: M.remove
	   ~icon: (Gui_icons.pixmap M.o_xpm_remove)#coerce
	   ~callback: self#remove
	   ()
	);
      ignore
	(wtool#insert_button 
	   ~text: M.remove_all_friends
	   ~tooltip: M.remove_all_friends
	   ~icon: (Gui_icons.pixmap M.o_xpm_remove_all_friends)#coerce
	   ~callback: self#remove_all_friends
	   ()
	);
  end

class box_list () =
  let vbox_list = GPack.vbox () in
  let label_locs = GMisc.label () in

  object (self)
    inherit box !!O.file_locations_columns () as prebox

    method coerce = vbox_list#coerce

    method add_to_friends () = 
      List.iter
        (fun c -> 
          if c.client_name <> "" then
            Gui_com.send (GuiProto.AddClientFriend c.client_num))
	self#selection

    method menu =
      match self#selection with
	[] -> []
      |	_ -> [ `I (M.add_to_friends, self#add_to_friends) ]

    method update_data_by_file file_opt =
      G.nclocations := 0;
      G.nlocations := 0;
      let l = ref [] in
      (
       match file_opt with
       | None -> ()
       | Some file ->
            match file.file_sources with
	     None -> Gui_com.send (GuiProto.GetFile_locations file.file_num)
	   | Some list ->
                List.iter 
		 (fun num ->
		  try
		    let c = Hashtbl.find G.locations num in
		    if Mi.is_connected c.client_state then incr G.nclocations;
		    l := c :: !l
		  with _ -> 
		    Gui_com.send (GuiProto.GetClient_info num)
		 )  list

      );
      G.nlocations := List.length !l;
      self#update_data !l;
      self#update_locations_label
		 
    method h_update_location c_new =
      try
	let (row, c) = self#find_client c_new.client_num in
	(
	 match Mi.is_connected c_new.client_state, Mi.is_connected c.client_state with
           false , false
	 | true, true -> ()
	 | false , _ -> 
             decr G.nclocations ;
             self#update_locations_label
	 | _, false -> 
             incr G.nclocations ;
             self#update_locations_label
	);
        if c_new.client_files <> None then
          c.client_files <- c_new.client_files;
        c.client_state <- c_new.client_state;
        c.client_rating <- c_new.client_rating;
        c.client_name <- c_new.client_name;
        
        c.client_kind <- c_new.client_kind;
        c.client_tags <- c_new.client_tags;
        self#update_row c row
      with
	Not_found ->
	  ()

    method update_locations_label =
      label_locs#set_text (Gui_messages.connected_to_locations !G.nclocations !G.nlocations)

    initializer
      vbox_list#pack ~expand: true prebox#coerce;
      vbox_list#pack ~expand: false label_locs#coerce;

      ignore
	(wtool#insert_button 
	   ~text: M.add_to_friends
	   ~tooltip: M.add_to_friends
	   ~icon: (Gui_icons.pixmap M.o_xpm_add_to_friends)#coerce
	   ~callback: self#add_to_friends
	   ()
	);

  end


class pane_friends () =
  let results = new Gui_results.box false !!O.results_columns () in
  let friends = new box_friends results () in
  let wnote_chat = GPack.notebook () in
  let wpane2 = GPack.paned `VERTICAL () in
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
	    (fun d -> d#num = client.client_num)
	    dialogs 
	in
	d#wt_input#misc#grab_focus ();
	d
      with
	Not_found ->
	  let dialog = new dialog client in
	  let wl = GMisc.label ~text: client.client_name () in
	  wnote_chat#append_page ~tab_label: wl#coerce dialog#coerce;
	  ignore (dialog#box#connect#destroy
		    (fun () -> dialogs <- List.filter (fun d -> not (d#num = client.client_num)) dialogs));
	  dialogs <- dialog :: dialogs;
	  dialog#wt_input#misc#grab_focus ();
	  dialog      

    inherit Gui_friends_base.paned ()

    method box_friends = friends
    method box_results = results
    method hpaned = wpane
    method vpaned = wpane2

    method set_tb_style st =
      results#set_tb_style st ;
      friends#set_tb_style st 

    method clear =
      results#clear ;
      friends#clear

    initializer
      friends#set_on_double_click (fun f -> ignore (self#get_dialog f));

      wpane#add1 friends#coerce;
      wpane#add2 wpane2#coerce ;

      wpane2#add2 wnote_chat#coerce;
      wpane2#add1 results#coerce;
  end
