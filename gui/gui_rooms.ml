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
open Gui_proto
open Gui_columns

module M = Gui_messages
module P = Gpattern
module O = Gui_options
module Mi = Gui_misc
module G = Gui_global

let (!!) = Options.(!!)

class paned () =
  let vbox = GPack.vbox ~homogeneous:false () in
  let wpane =
    GPack.paned `HORIZONTAL ~packing:(vbox#pack ~expand:true ~fill:true) ()
  in
  object
    val vbox = vbox
    val wpane = wpane
    method vbox = vbox
    method wpane = wpane
    method coerce = vbox#coerce
end;;

class box tb_style () =
  object(self)
    inherit Gui_rooms_base.box tb_style () 
    
    method set_tb_style (tb_style : Gtk.Tags.toolbar_style) = ()
end


class box_users remove =
  
  object (self)
    
    inherit Gui_users.box_users ()
    
    initializer
      
      ignore
        (wtool#insert_button 
          ~text: M.remove
          ~tooltip: M.remove
          ~icon: (Gui_icons.pixmap M.o_xpm_remove)#coerce
          ~callback: (fun _ -> !remove ())
        ()
      );

end
let rooms_by_num = Hashtbl.create 113

class pane_room (room : room_info) =
  let remove = ref (fun _ -> ()) in
  let users = new box_users remove in
  let messages = new box () () in
  object (self)
    inherit paned  ()
    
    method box_messages = messages
    method box_users = users
    method hpaned = wpane
    
    val mutable position = 0
    
    method insert_text s =
      let len = String.length s in
      position <- messages#wt_10#insert_text s ~pos: position;
      ()
    
    
    method on_entry_return () =
      match messages#we_11#text with
        "" -> ()
      |	s ->
          Gui_com.send (Gui_proto.SendMessage (room.room_num, 
              PublicMessage (0,s)));
          messages#we_11#set_text "";
          (*self#insert_text (Printf.sprintf "> %s\n" s) *)
          
    initializer
      Okey.add messages#we_11
        ~mods: []
        GdkKeysyms._Return
        self#on_entry_return      
    
    
    method set_tb_style st = 
      users#set_tb_style st ;
      messages#set_tb_style st

(** {2 Handling core messages} *)

(*
    method h_user_info = servers#h_server_info
    method h_server_state = servers#h_server_state
    method h_server_busy = servers#h_server_busy
    method h_server_user = servers#h_server_user
*)
    
    method add_user num =
      if not (List.memq num room.room_users) then begin
          room.room_users <- num :: room.room_users;
          self#update_users 
        end
    
    method update_users =
      let list = ref [] in
      List.iter (fun u ->
          try
            let user_info = Hashtbl.find G.users u in
            list := user_info :: !list
          with _ ->
              Gui_com.send (GetUser_info u);
      ) room.room_users;
      users#update_data !list

    method remove_user num =
      if List.memq num room.room_users then begin
          room.room_users <- List2.removeq num room.room_users;
          self#update_users              
        end
    
    method coerce = self#vbox#coerce
    
    initializer
      wpane#add1 users#coerce;
      wpane#add2 messages#coerce;
      remove := (fun _ -> 
          let vbox = Hashtbl.find rooms_by_num room.room_num in
          vbox#coerce#destroy ()
      )
end
      

let room_info (wnote: GPack.notebook) room =
  try
    let vbox = Hashtbl.find rooms_by_num room.room_num in
    match room.room_state with
      RoomClosed -> (* remove the room *) 
        Printf.printf "room closed"; print_newline ();
    | RoomOpened -> (* room resumed *) ()
    | RoomPaused -> (* room paused *) 
        Printf.printf "room paused"; print_newline ();
  with
    _ ->
      if room.room_state = RoomOpened then begin
          let wl = GMisc.label ~text: room.room_name () in
          let vbox = new pane_room room in
          let page = wnote#current_page in
          wnote#append_page ~tab_label: wl#coerce vbox#coerce ;
          Hashtbl.add rooms_by_num room.room_num vbox;
          vbox#update_users
        end else
        (Printf.printf "Can add room %d" room.room_num;
          print_newline ())
  
  
let add_room_user num user_num =
  try
    let vbox = Hashtbl.find rooms_by_num num in
    try
      vbox#add_user user_num
    with _ -> 
        Printf.printf "No such user %d" user_num;
        print_newline ();
  with _ -> 
      Printf.printf "No such room %d" num;
      print_newline ()
      
let user_info user = 
  ()  
  (*
  match user.user_state with
    RemovedHost -> 
      Hashtbl.iter (fun num vbox ->
          vbox#remove_user user.user_num
          
      ) rooms_by_num
  | _ -> ()
*)      
  
let add_room_message num msg =
  let vbox = Hashtbl.find rooms_by_num num in
  let append s =
    vbox#insert_text s
  in
  match msg with
  | ServerMessage s -> 
      append (s       ^ "\n");
  | PublicMessage (user_num, s) ->
      add_room_user num user_num;
      let user = Hashtbl.find G.users user_num in
      append (Printf.sprintf "%s: %s\n" user.user_name s)
  | PrivateMessage (user_num, s) ->
      add_room_user num user_num;
      let user = Hashtbl.find G.users user_num in
      append (Printf.sprintf "%s (Private): %s\n" user.user_name s)
  