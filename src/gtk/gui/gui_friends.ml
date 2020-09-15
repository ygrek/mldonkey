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

open Options
open Gettext
open CommonTypes
open GuiTypes
open Gui_columns

module M = Gui_messages
module P = Gpattern
module O = Gui_options
module Mi = Gui_misc
module G = Gui_global
module VB = VerificationBitmap

let (!!) = Options.(!!)

let string_color_of_state state =
  match state with
  | Connected_downloading _ -> gettext M.downloading, Some !!O.color_downloading 
  | Connected (-1)
  | Connected (-2) -> gettext M.connected, Some !!O.color_connected 
  | Connecting  -> gettext M.connecting, Some !!O.color_connecting
  | NewHost -> "NEW HOST", None
  | ServerFull -> "server full", None
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
  string_color_of_state c.client_state

let shorten maxlen s =
  let len = String.length s in
  if len > maxlen then
    (String.sub s 0 (maxlen-3)) ^ "..."
  else s

class dialog friend =
  object (self)
    inherit Gui_friends_base.dialog ()
    
    val mutable name = friend.client_name
    method name =  name
    method friend = friend
    method num = friend.client_num
    
    method send s =
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

class box columns friend_tab =
  let titles = List.map Gui_columns.Client.string_of_column !!columns in
  object (self)
    inherit [client_info] Gpattern.plist `EXTENDED titles true (fun c -> c.client_num) as pl
      inherit Gui_friends_base.box () as box
    
    val mutable columns = columns
    method set_columns l =
      columns <- l;
      self#set_titles (List.map Gui_columns.Client.string_of_column !!columns);
      self#update
      
    method column_menu  i = 
      [
        `I ("Autosize", fun _ -> self#wlist#columns_autosize ());
        `I ("Sort", self#resort_column i);
        `I ("Remove Column",
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
        `M ("Add Column After", (
            List.map (fun (c,s) ->
                (`I (s, (fun _ -> 
                        let c1, c2 = List2.cut (i+1) !!columns in
                        columns =:= c1 @ [c] @ c2;
                        self#set_columns columns
                    )))
            ) Gui_columns.Client.column_strings));
        `M ("Add Column Before", (
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
        Col_client_name -> compare f1.client_name f2.client_name
      |	Col_client_state -> compare f1.client_state f2.client_state
      |	Col_client_kind -> compare f1.client_kind f2.client_kind
      | Col_client_network -> compare f1.client_network f2.client_network
      | Col_client_type -> compare f1.client_type f2.client_type
    
    
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
        Col_client_name -> shorten !!O.max_client_name_len f.client_name
      | Col_client_state -> fst (string_color_of_client friend_tab f)
      | Col_client_type -> (let t = f.client_type in
            if t land client_friend_tag <> 0 then gettext M.friend else
            if t land client_contact_tag <> 0 then gettext M.contact else
              "Y")
      | Col_client_network -> Gui_global.network_name f.client_network
      | Col_client_kind -> 
          match f.client_kind with
            Known_location _ -> gettext M.direct
          | _ -> ""
              
    method content f =
      let strings = List.map 
          (fun col -> P.String (self#content_by_col f col))
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
  List.memq c.client_network !Gui_global.networks_filtered


class box_friends box_files friend_tab =
  object (self)
    inherit box O.friends_columns friend_tab
    
    method filter = is_filtered    
    
    method filter_networks = self#refresh_filter
    
    method remove () = 
      List.iter
        (fun c -> Gui_com.send (GuiProto.RemoveFriend c.client_num))
      self#selection
    
    method remove_all_friends () = 
      self#clear;
      Gui_com.send GuiProto.RemoveAllFriends
    
    method find_friend () =
      match GToolbox.input_string (gettext M.find_friend) (gettext M.name) with
        None -> ()
      |	Some s ->
          Gui_com.send (GuiProto.FindFriend s)
    
    method on_select c =
          Gui_com.send (GuiProto.GetClient_files c.client_num)
    
    method on_deselect f =
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
    
    
    method h_update_friend f_new =
      try
        let (row, f) = self#find_client f_new.client_num in
        f.client_state <- f_new.client_state;
        f.client_type <- f_new.client_type;
        f.client_rating <- f_new.client_rating;
        f.client_name <- f_new.client_name;
        
        f.client_kind <- f_new.client_kind;
        f.client_tags <- f_new.client_tags;
        self#update_row f row
      with
        Not_found ->
          self#add_item f_new
    
    method h_remove_friend num =
      try
        let (row, i) = self#find_client num in
        self#remove_item row i;
        selection <- List.filter (fun fi -> fi.client_num <> num) selection
      with
        Not_found -> ()
    
    initializer
      Gui_misc.insert_buttons wtool1 wtool2
          ~text: (gettext M.find_friend)
        ~tooltip: (gettext M.find_friend)
        ~icon: (M.o_xpm_find_friend)
          ~callback: self#find_friend
        ();
      Gui_misc.insert_buttons wtool1 wtool2
        ~text: (gettext M.remove)
      ~tooltip: (gettext M.remove)
      ~icon: (M.o_xpm_remove)
      ~callback: self#remove
        ();
      Gui_misc.insert_buttons wtool1 wtool2
        ~text: (gettext M.remove_all_friends_text)
      ~tooltip: (gettext M.remove_all_friends_tips)
      ~icon: (M.o_xpm_remove_all_friends)
      ~callback: self#remove_all_friends
      ();
end


let colorGreen = `NAME "green"
let colorDGreen= `NAME "darkgreen"
let colorRed   = `NAME "red"
let colorDRed  = `NAME "darkred"
let colorBlue  = `NAME "blue"
let colorGray  = `NAME "gray"
let colorWhite =`WHITE
let colorBlack = `BLACK

class box_list (client_info_box : GPack.box) friend_tab =
  let vbox_list = GPack.vbox () in
  let label_locs = GMisc.label () in
  
  object (self)
    inherit box O.file_locations_columns friend_tab as prebox
    
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
      |	_ -> [ `I (gettext M.add_to_friends, self#add_to_friends) ]
    
    val mutable delayed_sources_update = None
    
    method update_data_by_file file_opt =
      if delayed_sources_update = None then 
        BasicSocket.add_timer 0.1 (fun _ ->
            match delayed_sources_update with
              Some file_opt ->
                delayed_sources_update <- None;
                G.nclocations := 0;
                G.nlocations := 0;
                let l = ref [] in
                (
                  match file_opt with
                  | None -> ()
                  | Some file ->
                      match file.file_sources with
                        None -> 
                          
                          Gui_com.send (GuiProto.GetFile_locations file.file_num)
                      | Some list ->
                          List.iter 
                            (fun num ->
                              try
                                let c = Hashtbl.find G.locations num in
                                if Mi.is_connected c.client_state then incr G.nclocations;
                                (c.client_state <- 
                                   match c.client_state with
                                       Connected_downloading n ->
                                         if n = file.file_num 
                                           then Connected_downloading n
                                           else Connected (-1)
                                     | _ -> c.client_state);    
                                l := c :: !l
                              with _ -> 
                                  Gui_com.send (GuiProto.GetClient_info num)
                          )  list
                
                );
                G.nlocations := List.length !l;
                self#reset_data !l;
                self#update_locations_label;
            | _ -> ());
      delayed_sources_update <- Some file_opt
    
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
        
        if c_new.client_state = RemovedHost then
          begin
(*            lprintf "Removing client from locations panel"; 
            lprint_newline (); *)
            decr G.nlocations;
            self#update_locations_label;
            self#remove_item row c
          end else
        let _ = () in          
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
      label_locs#set_text 
        (Printf.sprintf !!Gui_messages.connected_to_locations !G.nclocations !G.nlocations)
    
    initializer
      vbox_list#pack ~expand: true prebox#coerce;
      vbox_list#pack ~expand: false label_locs#coerce;
      
      Gui_misc.insert_buttons wtool1 wtool2
        ~text: (gettext M.add_to_friends)
      ~tooltip: (gettext M.add_to_friends)
      ~icon: (M.o_xpm_add_to_friends)
      ~callback: self#add_to_friends
        ()
      
    val mutable selected = None
    method on_select c = 
      match selected with
      | Some (cc,_) when c == cc -> ()
      | _ ->
          begin
            match selected with
              None -> ()
            | Some (_, w) -> w#coerce#destroy ()
          end;
          let files = 
            try
              let files = Intmap.find c.client_num !Gui_global.availabilities 
              in
              let list = ref [] in
              Intmap.iter (fun file_num (avail, file) ->
                  list := (avail, file) :: !list
              ) !files;
              Array.of_list !list
            with _ -> [||]
          in
          
          let table = GPack.table ~rows: (2+ 2* Array.length files)
            ~columns:1 ~packing:(
              client_info_box#pack ~expand:false ~fill:true)   () in
          selected <- Some (c, table);
          
          let rects = Array.mapi (fun i (avail, file) ->
                let file_name_label = 
                  GMisc.label ~text:(Printf.sprintf "%s" 
                    (Gui_misc.short_name file.file_name))
                  ~justify:`LEFT ~line_wrap:true ~xalign:(-1.0) ~yalign:(-1.0) ()
                in
                table#attach ~left:1 ~top:(3+2 *i) file_name_label#coerce; 
                let avail_label =  GMisc.drawing_area ~height:20 () in
                table#attach ~left:1 ~top:(4+2 *i) ~expand:`X avail_label#coerce; 
                
                ignore (avail_label#event#connect#expose ~callback:
                  (fun _ -> 
                      let w = avail_label#misc#window in
                      let d = new GDraw.drawable w in
                      avail_label#misc#show ();
                      
                      let wx, wy = d#size in
                      (match file.file_chunks with
                      | None -> ()
                      | Some chunks ->
                          let nchunks = VB.length chunks in
                          let dx = if wx < nchunks then 1 else min !!O.chunk_width (wx / nchunks) in
                          let dx2 = if dx <= 2 then dx else dx - 1 in
                          for j = 0 to nchunks - 1 do
                            d#set_foreground
                              (match avail.[j] >= '1', VB.get chunks j with
                              | true, (VB.State_complete | VB.State_verified) ->
                                  colorDGreen
                              | true, (VB.State_missing | VB.State_partial) ->
                                  colorGreen
                              | false, (VB.State_complete | VB.State_verified) ->
                                  colorDRed
                              | false, (VB.State_missing | VB.State_partial) ->
                                  colorRed);
                            d#rectangle ~filled: true ~x:(j*dx) ~y: 0 ~width: dx2 ~height:wy ();
                          done);
                      
                      false));
                
                avail_label, avail
            ) files in
          
          (*
          for i = 0 to Array.length files - 1 do
            let (avail, file) = files.(i) in
            let file_name_label = 
              GMisc.label ~text:(Printf.sprintf "%s" file.file_name)
              ~justify:`LEFT ~line_wrap:true ~xalign:(-1.0) ~yalign:(-1.0) ()
            in
            table#attach ~left:1 ~top:(2+2 *i) file_name_label#coerce; 
            let avail_label =  GMisc.drawing_area ~width: 200 ~height:20 () in
            table#attach ~left:1 ~top:(3+2 *i) avail_label#coerce; 
            
           
done;
  *)            

          let client_name_label = 
            GMisc.label ~text:(Printf.sprintf "Client name: %s" c.client_name)
            ~justify:`LEFT ~line_wrap:true ~xalign:(-1.0) ~yalign:(-1.0) ()
          in
          table#attach ~left:1 ~top:1 client_name_label#coerce; 

          let client_info_label = 
            GMisc.label ~text:(Printf.sprintf "     num: %d" c.client_num)
            ~justify:`LEFT ~line_wrap:true ~xalign:(-1.0) ~yalign:(-1.0) ()
          in
          table#attach ~left:1 ~top:2 client_info_label#coerce; 

  end


class pane_friends () =
  let files = new Gui_results.box_dir_files () in
  let friends = new box_friends files true in
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
    method box_files = files
    method hpaned = wpane
    method vpaned = wpane2

    method set_tb_style st =
      files#set_tb_style st ;
      friends#set_tb_style st 

    method clear =
      files#clear ;
      friends#clear

    initializer
      friends#set_on_double_click (fun f -> ignore (self#get_dialog f));

      wpane#add1 friends#coerce;
      wpane#add2 wpane2#coerce ;

      wpane2#add2 wnote_chat#coerce;
      wpane2#add1 files#coerce;

  end
