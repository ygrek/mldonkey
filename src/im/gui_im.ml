(* Copyright 2002 b8_fange *)
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
open ImOptions
open ImAccount
open ImProtocol
open ImEvent
open ImTypes
open ImIdentity
open ImChat
open ImRoom  
open Gpattern

  
  
  
  
  
  
  
  
  
  
  
  
let quit_on_close = ref false
  
class dialog_box () =
  let box = GPack.vbox ~homogeneous:false () in
  let wscroll =
    GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
      ~placement:`TOP_LEFT ~packing:(box#pack ~expand:true ~fill:true) ()
  in
  let wt_dialog =
    GEdit.text ~editable:false ~word_wrap:true ~line_wrap:true
      ~packing:(wscroll#add) ()
  in
  let wtool =
    GButton.toolbar ~orientation:`HORIZONTAL ~style:`ICONS ~space_style:`EMPTY
      ~tooltips:true ~button_relief:`NORMAL
      ~packing:(box#pack ~expand:false ~fill:true) ()
  in
  let wt_input =
    GEdit.text ~height:50 ~editable:true ~word_wrap:true ~line_wrap:true
      ~packing:(box#pack ~expand:false ~fill:true) ()
  in
  let wb_show_hide =
    GButton.button ~packing:(box#pack ~expand:false ~fill:true) ()
  in
  let _65 =
    GMisc.label ~text:(Chat_messages.show_hide_people) ~justify:`LEFT
      ~line_wrap:true ~packing:(wb_show_hide#add) ()
  in
  let wscroll_people =
    GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
      ~placement:`TOP_LEFT ~packing:(box#pack ~expand:true ~fill:true) ()
  in
  let wlist_people =
    GList.clist
      ~titles:([Chat_messages.id; Chat_messages.host; Chat_messages.port])
      ~shadow_type:`NONE ~selection_mode:`SINGLE ~titles_show:true
      ~packing:(wscroll_people#add) ()
  in
  object
    val box = box
    val wscroll = wscroll
    val wt_dialog = wt_dialog
    val wtool = wtool
    val wt_input = wt_input
    val wb_show_hide = wb_show_hide
    val wscroll_people = wscroll_people
    val wlist_people = wlist_people
    method box = box
    method wscroll = wscroll
    method wt_dialog = wt_dialog
    method wtool = wtool
    method wt_input = wt_input
    method wb_show_hide = wb_show_hide
    method wscroll_people = wscroll_people
    method wlist_people = wlist_people
    method coerce = box#coerce
  end

  
class dialog (chat : chat) = 
  object (self)
    inherit dialog_box ()
        
    val mutable name = chat_name chat

    method name =  name

    method send s =
      lprintf "SEND MESSAGE %s" s; lprint_newline ();
      chat_send chat s

    method handle_message source_id mes =
      wt_dialog#insert ~foreground: (`NAME "red") source_id;
      wt_dialog#insert (" : "^mes^"\n");
      wt_dialog#set_position (wt_dialog#length - 1)

    method handle_my_message mes =
      wt_dialog#insert ~foreground: (`NAME "green") 
      (account_name (chat_account chat));
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
        wt_input#delete_text ~start: 0 ~stop: wt_input#length
          
      in
      Okey.add wt_input ~mods: [] GdkKeysyms._Return return;
      Okey.add_list wt_input ~mods: [`CONTROL]
        [GdkKeysyms._c; GdkKeysyms._C]
        box#destroy;
      Okey.add_list wt_dialog ~mods: [`CONTROL] 
        [GdkKeysyms._c; GdkKeysyms._C]
        box#destroy;
      Okey.add_list wt_input ~mods: [`CONTROL] 
        [GdkKeysyms._l; GdkKeysyms._L]
        wb_show_hide#clicked;
      Okey.add_list wt_dialog ~mods: [`CONTROL] 
        [GdkKeysyms._l; GdkKeysyms._L]
        wb_show_hide#clicked;

          wscroll_people#misc#hide ();
          let show = ref false in
          ignore (wb_show_hide#connect#clicked
                    (fun () -> 
                      show := not !show;
                      if !show then
                        wscroll_people#misc#show ()
                      else
            wscroll_people#misc#hide ()));
    (*
          List.iter
            (fun (i,h,p) ->
              ignore (wlist_people#append
                        [i ; h ; string_of_int p]))
            people; *)
          GToolbox.autosize_clist wlist_people
  end


let input_record record =
  let module C = Configwin in
  let rec iter list params refs =
    match list with 
      (_, name, _, from_record, to_record) :: tail ->
        let param, f = 
          match from_record, to_record with
            FromString fs, ToString ts ->
              let x = ref (ts ()) in
              C.string ~f: (fun s -> x := s) name !x,
              (fun _ -> fs !x)
          | FromBool fb, ToBool tb ->
              let x = ref (tb ()) in
              C.bool ~f: (fun s -> x := s) name !x,
              (fun _ -> fb !x)
          | FromInt fi, ToInt ti ->
              let x = ref (ti ()) in
              C.string ~f: (fun s -> x := int_of_string s) name 
                (string_of_int !x),
              (fun _ -> fi !x)
          | _ -> assert false
        in
        iter tail (param :: params) (f :: refs)
    | [] ->
        match C.simple_get "" (List.rev params) with
          C.Return_cancel -> ()
        | C.Return_apply | C.Return_ok -> 
            List.iter (fun f -> f ()) refs
  in 
  iter record [] []

  
let ask_for_room account =
  let module C = Configwin in
  let room_name = ref "" in
  let params = [
      C.string ~f: (fun s -> room_name := s) "Room Name:" !room_name;
    ] in
  match C.simple_get "" (List.rev params) with
    C.Return_cancel -> ()
  | C.Return_apply | C.Return_ok -> 
      account_join_room account !room_name
  
let input_record record =
  input_record record;
  Options.save_with_help accounts_ini

let input_account account =
  try
    input_record (account_config_record account);
    add_event (Account_event account);
    Options.save_with_help accounts_ini
  with e ->
      lprintf "Execption %s in input_account"
        (Printexc2.to_string e); lprint_newline ()
      
let string_of_status status =
  match status with
  | Status_online Online_available -> "Online"
  | Status_online Online_away -> "Away"
  | Status_connecting -> "Connecting"
  | Status_offline -> "Offline"

let new_dialog chat =
  let window = GWindow.window ~kind: `DIALOG ~width: 300 ~height: 200 
      ~title: "" () in
  ignore (window#connect#destroy (fun () -> 
        ()
    ));
  let dialog = new dialog chat in
  window#set_title dialog#name;
  ignore (dialog#box#connect#destroy 
      (fun _ -> chat_close chat));
  window#add dialog#box#coerce;
  window#show ();
  dialog#wt_input#misc#grab_focus ();
  window, dialog

class simple_box () =
  let vbox = GPack.vbox ~homogeneous:false () in
  let wtool =
    GButton.toolbar ~orientation:`HORIZONTAL ~style:`BOTH ~space_size:2
      ~space_style:`LINE ~tooltips:true ~button_relief:`NONE
    ~width: 200
      ~packing:(vbox#pack ~expand:false ~fill:true ~padding:2) ()
  in
  object
    val vbox = vbox
    val wtool = wtool
    method vbox = vbox
    method wtool = wtool
    method coerce = vbox#coerce
end

class contacts_window_list () =
  object(self)
    inherit [identity] Gpattern.plist `EXTENDED ["Name"; "Status"; "Temporary"]
      true (fun f -> identity_num f) as pl
    
    method compare id1 id2 = identity_num id1 - identity_num id2
    method content id =
      lprintf "content"; lprint_newline ();
      ([ String (identity_name id); String "offline"; String "yes"] 
          , None)
      
            
    method update_contact id =
      try
        let (row, _ ) = self#find (identity_num id) in
        self#update_row id row
      with _ -> 
          lprintf "add_item"; lprint_newline ();
          self#add_item id;
          lprintf "add_item done"; lprint_newline ();

    method on_double_click id =
      identity_open_chat id
      (*
      let dialog = new_dialog id in
()
  *)
          
end


class identity_list () =
  object(self)
    inherit [identity] Gpattern.plist `EXTENDED ["Name"]
      true (fun f -> identity_num f) as pl
    inherit simple_box () as box
    
    
    method compare id1 id2 = identity_num id1 - identity_num id2
    method content id =
      lprintf "content"; lprint_newline ();
      ([ String (identity_name id) ], None)
      
    method update_contact id =
      try
        let (row, _ ) = self#find (identity_num id) in
        self#update_row id row
      with _ -> 
          self#add_item id;

    method on_double_click id =
      identity_open_chat id
      (*
      let dialog = new_dialog id in
()
  *)

    method remove_identity id = 
      try
        let (row, _ ) = self#find (identity_num id) in
        self#remove_item row id
      with _ -> ()
      
          
    initializer
      box#vbox#pack ~expand: true pl#box;

          
end


let chat_windows = Hashtbl.create 13

let find_chat_window chat =
  Hashtbl.find chat_windows (chat_num chat)
  
let chat_window chat =
  try
    find_chat_window chat
  with _ ->
      let w = new_dialog chat in
      Hashtbl.add chat_windows (chat_num chat) w;
      w

class contacts_window account =
  let contacts = new contacts_window_list () in
  object (self)
    val mutable hidden = true
    
    inherit Gui_im_base.window () as super
    
    method coerce = window#coerce

    method update_contact = contacts#update_contact 
      
    method show = 
      hidden <- false;
      super#window#show

    method hide =
      hidden <- true;
      super#window#coerce#misc#hide
      
    method hidden = hidden
      
    initializer
      friends#add contacts#box
end

let account_windows = Hashtbl.create 13

let find_account_window account = 
  Hashtbl.find account_windows (account_num account) 
  
let account_window account = 
  let window = 
    try find_account_window account
    with _ ->
        let window = new contacts_window account in
        window#window#set_title (Printf.sprintf "%s: account %s"
            (protocol_name (account_protocol account)) (account_name account));
        ignore (window#window#connect#destroy (fun _ ->
              window#hide ()
              ));
        ignore (window#itemQuit#connect#activate 
            (fun _ -> 
              window#hide ();));
        ignore (window#itemAddFriend#connect#activate 
            (fun _ -> 
              let id = account_new_identity account in
              input_record (identity_config_record id)));
        ignore (window#itemOptions#connect#activate 
            (fun _ -> 
              input_account account));
        
        ignore (window#itemSetStatusOffline#connect#activate (fun _ ->
              account_set_status account Status_offline));
        List.iter (fun online ->
            let item =
              GMenu.menu_item
              ~label:(string_of_status (Status_online online))
              ~packing:(window#menuSetStatus#add) ()
            in
            ignore (item#connect#activate (fun _ ->
                  account_set_status account (Status_online online)));

   
        ) (protocol_available_status (account_protocol account));
        
        
        Hashtbl.add account_windows (account_num account) window;
        window
  in
  window


class accounts_window () =
  object(self)
    inherit [account] Gpattern.plist `EXTENDED ["Name"; "Status"; "Protocol"]
      true (fun f -> account_num f) as pl
    
    method compare id1 id2 = account_num id1 - account_num id2
    method content ac =
      ([ String (account_name ac); 
          String (string_of_status (account_status ac));
          String (protocol_name (account_protocol ac))] 
          , None)
      
    method update_account account =
      try
        let (row, _ ) = self#find (account_num account) in
        self#update_row account row
      with _ -> 
          self#add_item account

              
    method menu =
      (match self#selection with
          [] -> []
        | account :: tail ->
            let basic_menu = 
              [
                `I ("Connect/Disconnect", self#connect) ;
                `I ("Settings", self#settings) ;
                `I ("Remove", self#remove) ;
              ] in
            if tail = [] && account_has_rooms account then
              (`I ("Join Room", (fun _ -> ask_for_room account)))::
              (let prefered_rooms = account_prefered_rooms account in
                if prefered_rooms = [] then [] else
                  [ `M ("Prefered Rooms",
                      List.map (fun name ->
                          `I (name, (fun _ -> 
                                account_join_room account name))  
                      ) prefered_rooms)]
              
              ) @basic_menu
            else basic_menu
      )

    method settings () =
      List.iter (fun account ->
          input_account account
      ) self#selection    
      
    method connect () = 
      List.iter (fun account ->
          match account_status account with
            Status_offline -> account_login account
          | _ -> account_logout account
      ) self#selection
      
    method remove () = ()
      
          
    method on_double_click account = 
      (account_window account)#show ()
          
end

class im_window account =
  let accounts = new accounts_window () in
  object (self)
    inherit Gui_im_base.accounts ()
    
    method coerce = window#coerce

    method update_account account = accounts#update_account account

    initializer
      friends#add accounts#box
end

(*
let accounts_window = 
  
  let window = new im_window () in
  window#window#set_title "Accounts Window";
  ignore (window#window#connect#destroy (fun _ ->
        window#coerce#misc#hide ()
    ));
  ignore (window#itemQuit#connect#activate 
      (fun _ -> window#coerce#misc#hide ()));

(*
              ignore (window#itemAddFriend#connect#activate 
                  (fun _ -> 
                    let id = protocol_new_account  in
                    input_record (identity_config_record id)));
              ignore (window#itemOptions#connect#activate 
(fun _ -> 
input_record (account_config_record account)));

*)
  
  
  let _new_accounts = 
    GMenu.menu_item ~label:"New accounts"  ~packing:(window#menubar#add) ()
  in
  let new_accounts = 
    GMenu.menu ~packing:(_new_accounts#set_submenu) () in
  
  
  ImProtocol.iter (fun p ->
      let menu_item =
        GMenu.menu_item ~label: 
        (Printf.sprintf "New %s account" (protocol_name p))
(*              ~active:n.network_enabled *)
        ~packing:new_accounts#add ()
      in
      ignore (menu_item#connect#activate ~callback:(fun _ ->
              let account = protocol_new_account p in
              input_account account;
              ImEvent.add_event (Account_event account);
              lprintf "NEW ACCOUNT"; lprint_newline ();
        ))
  
  );
  window  
*)

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

class room_window (room: room) =
  let room_users = new identity_list () in
  object (self)
    
    inherit Gui_im_base.room_tab ()

    
    
    method update_room = ()    

      (*
    method init_window =
      let (w,_) = Gdk.Window.get_size wpane#misc#window in
      wpane#set_position (w - 200)
*)
      
    method room_event (label: GMisc.label) event =
      lprintf "room event"; lprint_newline ();
      match event with
      | Room_message (_, id, msg) ->
          let nick = identity_name id in
          let c = color_of_name nick in
          text#insert ~foreground: c (Printf.sprintf "%s: " nick);
          text#insert (Printf.sprintf "%s\n" msg)
      
      | Room_user_join (_, identity) ->
          room_users#add_item identity
      
      | Room_user_leave (_, identity) ->
          room_users#remove_identity identity
      
      | _ -> 
          lprintf "unused room event"; lprint_newline ()
    
    method quit_this_room () = 
      room_quit room
      
    initializer
      ignore
        (room_users#wtool#insert_button 
          ~text: "Leave"
          ~tooltip: "Leave this room"
          ~icon: (Gui_options.pixmap Gui_messages.o_xpm_close_room)#coerce
          ~callback: self#quit_this_room
          ()
      );
      let account = room_account room in
      nick_label#set_text (Printf.sprintf "%s:" (account_name account));
      wpane#add2 room_users#coerce;
      room_users#coerce#misc#show ();
      let  on_entry_return () =
        match entry#text with
          "" -> ()
        | s ->
            room_send room s;
            entry#set_text "";
(*self#insert_text (Printf.sprintf "> %s\n" s) *)
      in          
      Okey.add entry ~mods: [] GdkKeysyms._Return
        on_entry_return;
      
      
end

let room_tabs = Hashtbl.create 13
  
let find_room_tab room =
  Hashtbl.find room_tabs (room_num room)

class main_window account =
  let accounts = new accounts_window () in
  let contacts = new contacts_window_list () in
  object (self)
    inherit Gui_im_base.window2 ()
    
    method coerce = window#coerce
    
    method update_contact = contacts#update_contact 
    method update_account account = accounts#update_account account
    
    method find_room_or_create room = 
      try
        Hashtbl.find room_tabs (room_num room)
      with _ -> 
          lprintf "New room %d" (room_num room); lprint_newline ();
          let room_window = new room_window room in
          let label_text = Printf.sprintf "%s: Room %s"
              (protocol_name (room_protocol room))
            (room_name room) in
          let label = GMisc.label ~text: label_text () in
          main_notebook#append_page ~tab_label:label#coerce
            room_window#coerce;
          Hashtbl.add room_tabs (room_num room) (room_window, label);
          room_window, label
          
    method update_room room =
      let room_window, label = self#find_room_or_create room in
      room_window#update_room
          
    method remove_room room =
      let room_window, label = self#find_room_or_create room in
      let page_num = main_notebook#page_num room_window#coerce in
      main_notebook#goto_page 1;
      main_notebook#remove_page page_num;
      Hashtbl.remove room_tabs (room_num room)
      
    method room_event room event =
      let (room_window, label) = self#find_room_or_create room in
      room_window#room_event label event
            
    initializer
      contacts_hbox#add contacts#box;
      accounts_hbox#add accounts#box;
end

let main_window = 
  let window = new main_window () in
  window#window#set_title "IM Window";
  (* Here there is an error if you close directly the window. You cannot call it back !
     #connect#destroy is too late : the window is already destroyed.
     Call #event#connect#delete instead to make operations before destroying the window *)
  (* ignore (window#window#connect#destroy (fun _ ->
        if !quit_on_close then CommonGlobals.exit_properly 0 else
          window#coerce#misc#hide ()
    )); *)
  ignore (window#window#event#connect#delete (fun _ ->
        if !quit_on_close then CommonGlobals.exit_properly 0 else
          window#coerce#misc#hide ();
          true
    ));
  ignore (window#itemQuit#connect#activate 
      (fun _ -> 
        if !quit_on_close then CommonGlobals.exit_properly 0 else
          window#coerce#misc#hide ()));
  let _new_accounts = 
    GMenu.menu_item ~label:"New accounts"  ~packing:(window#menubar#add) ()
  in
  let new_accounts = 
    GMenu.menu ~packing:(_new_accounts#set_submenu) () in
  
  
  ImProtocol.iter (fun p ->
      let menu_item =
        GMenu.menu_item ~label: 
        (Printf.sprintf "New %s account" (protocol_name p))
(*              ~active:n.network_enabled *)
        ~packing:new_accounts#add ()
      in
      ignore (menu_item#connect#activate ~callback:(fun _ ->
              let account = protocol_new_account p in
              input_account account;
              ImEvent.add_event (Account_event account);
              lprintf "NEW ACCOUNT"; lprint_newline ();
        ))
  
  );  
  window
  
  

  
let _ =
  ImEvent.set_event_handler (fun event ->
      match event with
      | Account_event account ->
          lprintf "Account event"; lprint_newline ();
          main_window#update_account account;
          
          (*
          let w = account_window account in          
          if w#hidden && account_status account <> Status_offline then
            w#show ();
          w#label_connect_status#set_text (match account_status account with
              Status_offline -> "Offline"
            | Status_connecting -> "Connecting ... "
            | _ -> "Connected"); *)
      | Account_friend_event id ->
          lprintf "Account_friend_event"; lprint_newline ();
          let account = identity_account id in
          begin  try
              let w = find_account_window account in
              lprintf "Window available"; lprint_newline ();
              w#update_contact id
            with _ -> ()  end
      | Chat_open_event chat ->
          begin
            let w,_ = chat_window chat in
            w#show ()
          end
      | Chat_close_event chat ->
          begin
            try
              let w,_ = find_chat_window chat in
              Hashtbl.remove chat_windows (chat_num chat);
              w#destroy ();
            with _ -> ()
          end
      | Chat_my_message (chat, msg) ->
          begin
            let _,d = chat_window chat in
            d#handle_my_message msg
          end

      | Chat_message_event (chat, id, msg) ->
          begin
            let _,d = chat_window chat in
            d#handle_message (identity_name id) msg
          end
          
      | Room_join room ->
          main_window#update_room room

      | Room_leave room ->
          main_window#remove_room room
          
      | Room_public_message (room, _)
      | Room_message (room, _, _)
      | Room_user_join (room, _) 
      | Room_user_leave (room, _) ->
          main_window#room_event room event
          
(*      | _ -> 
          lprintf "Discarding event"; lprint_newline (); *)
  );
  Gui_global.top_menus := ("IM", (fun menu ->
        
        let menu_item =
          GMenu.menu_item ~label: "IM Window"
            ~packing:menu#add ()
        in
        ignore (menu_item#connect#activate ~callback:(fun _ ->              
              main_window#window#show ()));
        (*
        
  *)
    )) :: !Gui_global.top_menus

  (*
  
let _ =
  accounts_window#window#show ();
  BasicSocket.loop ()
  *)
