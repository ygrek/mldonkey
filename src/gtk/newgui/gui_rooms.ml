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

module M = Gui_messages
module P = Gpattern
module O = Gui_options
module Mi = Gui_misc
module G = Gui_global

let (!!) = Options.(!!)


class box tb_style () =
  object(self)
    inherit Gui_rooms_base.box tb_style () 
    
    method set_tb_style (tb_style : Gtk.Tags.toolbar_style) = ()

    initializer

      let style = evbox#misc#style#copy in
      style#set_bg [ (`NORMAL, (`NAME "#494949"))];
      evbox#misc#set_style style;
      let style = label_rooms_messages#misc#style#copy in
      style#set_fg [ (`NORMAL, `WHITE)];
      label_rooms_messages#misc#set_style style

end

class rooms_box columns () =
  
  let titles = List.map Gui_columns.Room.string_of_column !!columns in 
  object (self)
    inherit [Gui_types.gui_room_info] Gpattern.filtered_plist `SINGLE titles true
    (fun r -> r.groom_num) as pl
      inherit Gui_users_base.box () as box
    
    val mutable columns = columns
    val mutable icons_are_used = (!!O.use_icons : bool)

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
      self#set_titles (List.map Gui_columns.Room.string_of_column !!columns);
      self#update;
      self#set_list_bg (`NAME !!O.color_list_bg)
        (Gdk.Font.load_fontset !!O.font_list)
      
    method column_menu  i = 
      [
        `I (M.mAutosize, fun _ -> self#wlist#columns_autosize ());
        `I ( M.mSort, self#resort_column i);
        `I ( M.mRemove_column,
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
        `M (M.mAdd_column_after, (
            List.map (fun (c,s,_) ->
                (`I (s, (fun _ -> 
                        let c1, c2 = List2.cut (i+1) !!columns in
                        columns =:= c1 @ [c] @ c2;
                        self#set_columns columns
                    )))
            ) Gui_columns.Room.column_strings));
        `M (M.mAdd_column_before, (
            List.map (fun (c,s,_) ->
                (`I (s, (fun _ -> 
                        let c1, c2 = List2.cut i !!columns in
                        columns =:= c1 @ [c] @ c2;
                        self#set_columns columns
                    )))
            ) Gui_columns.Room.column_strings));
      ]

    method filter r =
      match r.groom_state with
      | RoomClosed -> true
      | _ -> false
      
    method compare_by_col col f1 f2 =
      match col with
        Col_room_name -> compare f1.groom_name f2.groom_name
      | Col_room_network -> compare f1.groom_network f2.groom_network
      | Col_room_nusers -> compare f1.groom_nusers f2.groom_nusers
    
    
    method compare f1 f2 =
      let abs = if current_sort >= 0 then current_sort else - current_sort in
      let col = 
        try List.nth !!columns (abs - 1) 
        with _ -> Col_room_name
      in
      let res = self#compare_by_col col f1 f2 in
      res * current_sort
    
    method content_by_col f col =
      match col with
        Col_room_name -> f.groom_name
      | Col_room_network -> G.network_name f.groom_network
      | Col_room_nusers -> string_of_int f.groom_nusers
          
    method content f =
      let strings = List.map 
          (fun col -> match col with
               Col_room_network ->
                 (match f.groom_net_pixmap with
                     Some pixmap -> P.Pixmap (pixmap)
                   | _ -> P.String (self#content_by_col f col ))
             | _ -> P.String (self#content_by_col f col ))
        !!columns 
      in
      let col_opt = Some `BLACK      in
      (strings, col_opt)
    
    method find_room num = self#find num

    method add_room room = self#add_item room
    
    method remove_room row room = self#remove_item row room
    
    method core_to_gui_room room =
      {
       groom_num = room.room_num;
       groom_network = room.room_network;
       groom_name = room.room_name;
       groom_state = room.room_state;
       groom_users = room.room_users;
       groom_messages = room.room_messages;
       groom_nusers = room.room_nusers;
       groom_net_pixmap =
         if icons_are_used then
           Some (Gui_options.network_pix
                   (Gui_global.network_name room.room_network))
           else None;
      }

    method update_icons b =
      icons_are_used <- b;
      let (f, label, step) =
        if b then
          ((fun r ->
          r.groom_net_pixmap <-
            Some (Gui_options.network_pix
                    (Gui_global.network_name r.groom_network))
          ), M.pW_lb_rooms_add_icons, 1)
          else
            ((fun r ->
            r.groom_net_pixmap <- None
            ), M.pW_lb_rooms_remove_icons, 10)
      in
      Gui_options.generate_with_progress label self#get_all_items f step;
      self#update

    method set_tb_style tb = 
        if Options.(!!) Gui_options.mini_toolbars then
          (wtool1#misc#hide (); wtool2#misc#show ()) else
          (wtool2#misc#hide (); wtool1#misc#show ());
      wtool1#set_style tb;
      wtool2#set_style tb
    
    initializer

      box#vbox#pack ~expand: true pl#box ;

end

class box_users room =
  
  object (self)
    
    inherit Gui_users.box_users ()

    initializer

      label_users#set_text (M.rT_lb_users)

end

class opened_rooms_box on_select =
  object (self)
    
    inherit rooms_box O.rooms_columns  () as box_rooms
    
    method add_room room =
      box_rooms#add_room room
      
    initializer 
      BasicSocket.add_infinite_timer 1. (fun _ ->
          if self#selection = [] then begin
            let w = box_rooms#wlist in
            let rows = w#rows in
            if rows > 0 then
                w#select (rows - 1) 0 end
      )

(*     method rooms = data *)
    method on_select room = on_select (Some room)
    method on_deselect room = on_select None

    initializer

      label_users#set_text (Gui_messages.rT_lb_opened_rooms);
      let style = evbox1#misc#style#copy in
      style#set_bg [ (`NORMAL, (`NAME "#494949"))];
      evbox1#misc#set_style style;
      let style = label_users#misc#style#copy in
      style#set_fg [ (`NORMAL, `WHITE)];
      label_users#misc#set_style style

end

class paused_rooms_box () =
  object (self)

    inherit rooms_box O.rooms_columns  () as box_rooms
    
    method on_double_click room = 
      Gui_com.send (GuiProto.SetRoomState (room.groom_num, RoomOpened))

    initializer

      label_users#set_text (Gui_messages.rT_lb_available_rooms);
      let style = evbox1#misc#style#copy in
      style#set_bg [ (`NORMAL, (`NAME "#494949"))];
      evbox1#misc#set_style style;
      let style = label_users#misc#style#copy in
      style#set_fg [ (`NORMAL, `WHITE)];
      label_users#misc#set_style style

end

let add_room_user room user_num =
  if not (List.memq user_num room.groom_users) then begin
      room.groom_users <- user_num :: room.groom_users;
    end
    
let insert_text (messages: box) ?user ?(priv=false) s =
  let user_info = 
    match user with
      None -> None
    | Some u ->
        let col = Gui_misc.color_of_name u in
        Some (col,
          Printf.sprintf "%s%s :" u
            (if priv then " (private)" else ""))
  in
  (
    match user_info with
      None -> ()
    | Some (c,s) ->
        messages#wt_10#insert ~foreground: c s
  );
  messages#wt_10#insert s

let update_users r (users : box_users) =
  let list = ref [] in
  List.iter (fun u ->
      try
        let user_info = Hashtbl.find G.users u in
        list := user_info :: !list
      with _ ->
          Gui_com.send (GuiProto.GetUser_info u);
  ) r.groom_users;
  users#reset_data !list

let append_message r (messages:box) msg =
  match msg with
  | ServerMessage s -> 
(* try to get the user name to put some color: ????
Username of what ? ServerMessage is a message sent from the server, not from
  a user !!! *)
      let len = String.length s in
      if len > 0 then
        (
          let (user, mes) = 
            match s.[0] with
              '<' ->
                (
                  try 
                    let pos = String.index s '>' in
                    let u = String.sub s 1 (pos - 1) in
                    let mes = String.sub s (pos + 1) (len - pos - 1) in
                    (Some u, mes)
                  with
                    _ ->
                      (None, s)
                )
            | _ ->
                (None, s)
          in
          insert_text messages ?user (mes^"\n")
        )
      else
        ()      
  | PublicMessage (user_num, s) ->
      add_room_user r user_num;
      let user = Hashtbl.find G.users user_num in
      insert_text messages ~user: user.user_name (s^"\n")
  | PrivateMessage (user_num, s) ->
      add_room_user r user_num;
      let user = Hashtbl.find G.users user_num in
      insert_text messages ~user: user.user_name ~priv: true (s^"\n")

class pane_rooms () =
  
  let select = ref (fun room -> ()) in
  let (widgets: (int, box_users * box) Hashtbl.t) = Hashtbl.create 13 in
  let opened_rooms = new opened_rooms_box (fun room -> !select room) in
  let paused_rooms = new paused_rooms_box () in
  
  let users_box = GPack.vbox ~homogeneous:false () in
  let messages_box = GPack.vbox ~homogeneous:false () in
  
  let current_widgets = ref (-1) in
  
  object(self)
    inherit Gui_rooms_base.box2 ()
    
    method clear_widgets room = 
      try
        let (users, messages) = Hashtbl.find widgets room.groom_num in
        if !current_widgets = room.groom_num then
          current_widgets := -1;
        users#coerce#destroy ();
        messages#coerce#destroy ();
        Hashtbl.remove widgets room.groom_num
      with e -> 
          lprintf "Clear widget: %s" (Printexc2.to_string e);
          lprint_newline ()
    
    method room_info room =
      begin
        try
          let (num, old_room) = try
              opened_rooms#find_room room.room_num 
            with _ -> paused_rooms#find_room room.room_num in
          
          if old_room.groom_state <> room.room_state then begin
              
              match old_room.groom_state, room.room_state with
              | (RoomPaused | RoomClosed), RoomOpened ->
                  paused_rooms#remove_room num old_room;
                  let roomi = opened_rooms#core_to_gui_room room in
                  opened_rooms#add_room roomi
              | RoomOpened, RoomPaused ->
                  opened_rooms#remove_room num old_room;
                  let roomi = paused_rooms#core_to_gui_room room in
                  self#clear_widgets roomi;
                  paused_rooms#add_room roomi
              | _ -> 
                  old_room.groom_state <- room.room_state
            end
        with Not_found ->
            match room.room_state with
            | RoomPaused
            | RoomClosed ->
                let roomi = paused_rooms#core_to_gui_room room in
                paused_rooms#add_room roomi
            | RoomOpened ->
                let roomi = opened_rooms#core_to_gui_room room in
                opened_rooms#add_room roomi
      end
(* Maybe automatic selection is not that good ?:
      ;
      match opened_rooms#rooms with
        [room] -> opened_rooms#on_select room
      | _ -> () *)
      
    
    method add_room_message room_num msg =
      try
        let (num, room) = try
            opened_rooms#find_room room_num 
          with _ -> paused_rooms#find_room room_num in
        try
          let (users, messages) = Hashtbl.find widgets room_num in
          append_message room messages msg
        with _ -> 
            room.groom_messages <- msg :: room.groom_messages;
      with e -> 
          lprintf "ROOM %d Exception %s" room_num (Printexc2.to_string e);
          lprint_newline ();
          ()
    
    method set_tb_style st =
      opened_rooms#set_tb_style st;
      paused_rooms#set_tb_style st;
      opened_rooms#iter
        (fun room ->
          try
            let (users, _) = Hashtbl.find widgets room.groom_num in
            users#set_tb_style st;
          with _ -> ()
        )
    
    method set_list_bg bg font =
      opened_rooms#set_list_bg bg font;
      paused_rooms#set_list_bg bg font;
      opened_rooms#iter
        (fun room ->
          try
            let (users, messages) = Hashtbl.find widgets room.groom_num in
            users#set_list_bg bg font;
            let w = messages#wt_10 in
            let style = w#misc#style#copy in
            style#set_base [ (`NORMAL, bg)];
            style#set_font font;
            w#misc#set_style style
          with _ -> ()
        )
    
    method hpaned = hpaned
    
    method clear = 
      opened_rooms#clear;
      paused_rooms#clear;
      opened_rooms#iter self#clear_widgets
    
    method  remove_room_user room_num user_num =
      try
        let (num, room) = try
            opened_rooms#find_room room_num 
          with _ -> paused_rooms#find_room room_num in
        if List.memq user_num room.groom_users then begin
            room.groom_users <- List2.removeq user_num room.groom_users;
            try
              let (users, messages) = Hashtbl.find widgets room_num in
              let row, user = users#find user_num in
              users#remove_item row user
            with _ -> ()
          end                
      with _ -> ()
    
    
    method  add_room_user room_num user_num =
      try
        let (num, room) = try
            opened_rooms#find_room room_num 
          with _ -> paused_rooms#find_room room_num in
        if not (List.memq user_num room.groom_users) then begin
            room.groom_users <- user_num :: room.groom_users;
            try
              let (users, messages) = Hashtbl.find widgets room_num in
              users#add_item (Hashtbl.find G.users user_num)
            with _ -> ()
          end
      with _ -> ()
    
    method c_update_icons b =
      opened_rooms#update_icons b;
      paused_rooms#update_icons b

    initializer
      
      hpaned#add1 paused_rooms#coerce;
      room_pane#add1 opened_rooms#coerce;
      room_pane#add2 users_box#coerce;
      rooms_pane#add2 messages_box#coerce;
      
      let clean_widgets () = 
        
        (try
            let (users, messages) = Hashtbl.find widgets !current_widgets
            in
            users#coerce#misc#hide ();
            messages#coerce#misc#hide ();
            current_widgets := (-1)
          with e -> 
              lprintf "Normal exception %s in unmap old users/messages?"
                (Printexc2.to_string e));
      in
      
      select := (fun room ->      
          match room with
            None -> clean_widgets ()
          
          | Some room ->
              
              let (users, messages) = try Hashtbl.find widgets room.groom_num
                with _ ->
                    let users = new box_users room in
                    
                    let messages = new box () () in
                    let  on_entry_return () =
                      match messages#we_11#text with
                        "" -> ()
                      |	s ->
                          
                          Gui_com.send
                            (GuiProto.SendMessage (room.groom_num,
                              PublicMessage (0,s)));
                          messages#we_11#set_text "";
                          insert_text messages (Printf.sprintf "%s\n" s) 
                            ~user:!Gui_options.client_name
                    in          
                    Okey.add messages#we_11 ~mods: [] GdkKeysyms._Return
                      on_entry_return;
                    Hashtbl.add widgets room.groom_num (users, messages);
                    
                    
                    Gui_misc.insert_buttons users#wtool1 users#wtool2
                      ~text: (M.rT_lb_close_room)
                      ~tooltip: (M.rT_ti_close_room)
                      ~icon: (M.o_xpm_close_room)
                      ~callback: (fun _ ->  
                          match room.groom_state with
                            RoomOpened ->
                              Gui_com.send (GuiProto.SetRoomState (
                                  room.groom_num, RoomClosed))
                          | _ -> 
                              try
                                let (num, old_room) = 
                                  opened_rooms#find_room room.groom_num
                                in
                                opened_rooms#remove_room num old_room;
                                self#clear_widgets room;
                                paused_rooms#add_room room
                              with e -> 
                                  lprintf "Exception %s in close room"
                                    (Printexc2.to_string e); lprint_newline ();
                      )
                    ();

                    List.iter (fun msg ->
                        append_message room messages msg
                    ) (List.rev room.groom_messages);
                    room.groom_messages <- [];
                    users#set_list_bg (`NAME !!O.color_list_bg)
                      (Gdk.Font.load_fontset !!O.font_list);
                    users#set_tb_style !!O.toolbars_style;
                    let w = messages#wt_10 in
                    let style = w#misc#style#copy in
                    style#set_base [ (`NORMAL, (`NAME !!O.color_list_bg))];
                    style#set_font (Gdk.Font.load_fontset !!O.font_list);
                    w#misc#set_style style;
                    users_box#add users#coerce;
                    messages_box#add messages#coerce;
                    
                    users, messages
              in
              clean_widgets ();
              
              users#coerce#misc#show ();
              messages#coerce#misc#show ();
              current_widgets := room.groom_num;
              
              update_users room users
      )      
end

(* for now, no way to update users ? *)
let user_info user = ()

