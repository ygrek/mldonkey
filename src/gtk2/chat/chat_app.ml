(**************************************************************************)
(*  Copyright 2003, 2002 b8_bavard, b8_zoggy, , b52_simon INRIA           *)
(*                                                                        *)
(*    This file is part of mldonkey.                                      *)
(*                                                                        *)
(*    mldonkey is free software; you can redistribute it and/or modify    *)
(*    it under the terms of the GNU General Public License as published   *)
(*    by the Free Software Foundation; either version 2 of the License,   *)
(*    or (at your option) any later version.                              *)
(*                                                                        *)
(*    mldonkey is distributed in the hope that it will be useful,         *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of      *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       *)
(*    GNU General Public License for more details.                        *)
(*                                                                        *)
(*    You should have received a copy of the GNU General Public License   *)
(*    along with mldonkey; if not, write to the Free Software             *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston,               *)
(*    MA  02111-1307  USA                                                 *)
(*                                                                        *)
(**************************************************************************)

(** The application class. *)

open Chat_types
open Chat_proto
open Chat_configwin

module M = Chat_messages

class app 
    ?(no_quit=false)
    pred conf com =
  let data = new Chat_data.data pred conf com in

  object (self)
    val mutable working = false
    val mutable closed = false

    inherit Chat_gui.gui no_quit data as gui

    method handle_paquet p =
      let ((version,id,(host,port)), iddest, proto) = p in
      let reply = 
	if version <> Chat_proto.version then
	  None
	else
	  (
	   match proto with
	   | HelloOk ->
	       data#set_connected id host port;
	       gui#update;
	       None
		 
	   | Hello ->
	       data#set_connected id host port;
	       gui#update;
	       Some HelloOk
		   
	   | Byebye ->
	       data#set_not_connected id host port;
	       gui#update;
	       None
		 
	   | Message mes ->
	       let l = data#people in
	       data#set_connected id host port;
	       if l <> data#people then gui#update;
	       let show = 
		 conf#popup_all or 
		 (List.exists (pred (id,host,port)) conf#people)
	       in
	       let dial = Chat_gui.get_dialog ~show data (Chat_gui.Single (id, host, port)) in
	       dial#handle_message id mes;
	       None
		 
	   | AddOpen (i, (h, p)) ->
	       Chat_messages.verbose (Printf.sprintf "received AddOpen i=%s h=%s p=%d" i h p);
	       self#handle_paquet ((version,i,(h,p)), iddest, Message "");
	       None
		 
	   | RoomMessage (name, people, mes) ->
	       let dial = Chat_gui.get_dialog ~show: true data (Chat_gui.Room (name, people)) in
	       List.iter 
		 (fun (i,h,p) ->
		   if List.exists (data#pred (i,h,p)) conf#people then
		     data#set_connected i h p)
		 people;
	       gui#update;
	       dial#handle_message id mes;
	       None
	  )
      in
      match reply with
	None -> ()
      | Some r ->
	  try data#com#send id (host,port) r
	  with Failure s -> Chat_messages.verbose s

    method accept =
      try
	match data#com#receive with
	  None -> ()
	| Some p -> self#handle_paquet p
      with
	Failure s ->
	  Chat_messages.verbose s;
	  ()


    method work () =
      if working then not closed
      else
	(
	 self#accept;
	 if not closed then
	   ignore (GMain.Timeout.add ~ms: conf#timeout ~callback: self#work);
	 working <- false;
	 false
	)

    method say_hello (id, host, port, state, temp) =
      if temp then ()
      else
	try data#com#send id (host, port) Hello 
	with Failure s ->
	  data#set_not_connected id host port;
	  Chat_messages.verbose s

    method say_byebye (id, host, port, state, temp) =
	try data#com#send id (host, port) Byebye
	with _ -> ()

    method edit_conf =
      let p_id = preference ~help: M.h_id M.id conf#id BString () in
      let p_port = preference ~help:M.h_port M.port (string_of_int conf#port) BInt () in
      let p_timeout = preference ~help:M.h_timeout M.h_timeout (string_of_int conf#timeout) BInt () in
      let p_popup = preference ~help:M.h_popup_all M.popup_all (string_of_bool conf#popup_all) BBool () in
      let p_col_connected = preference ~help:M.h_color_connected
          M.h_color_connected conf#color_connected BColor () in
      let p_col_connected_temp = preference ~help:M.h_color_connected_temp
          M.h_color_connected_temp conf#color_connected_temp BColor () in
      let p_col_not_connected = preference ~help:M.h_color_not_connected
          M.h_color_not_connected conf#color_not_connected BColor () in
      let p_col_myself = preference ~help:M.h_color_myself
          M.h_color_myself conf#color_myself BColor () in
      let on_ok () =
        let id = p_id.pref_new_value in
        let port = int_of_float (safe_int p_port.pref_new_value) in
        let timeout = int_of_float (safe_int p_timeout.pref_new_value) in
        let popup = safe_bool p_popup.pref_new_value in
        let col_connected = p_col_connected.pref_new_value in
        let col_connected_temp = p_col_connected_temp.pref_new_value in
        let col_not_connected = p_col_not_connected.pref_new_value in
        let col_myself = p_col_myself.pref_new_value in
        conf#set_id id;
        conf#set_port port;
        conf#set_timeout timeout;
        conf#set_popup_all popup;
        conf#set_color_connected col_connected;
        conf#set_color_connected_temp col_connected_temp;
        conf#set_color_not_connected col_not_connected;
        conf#set_color_myself col_myself;
        conf#save;
        data#update_people ;
        List.iter self#say_hello data#people;
        gui#update
      in
      let structure =
        [M.connection,
           [p_id;
            p_port;
            p_timeout;
            p_popup;
           ];
         M.colors,
           [p_col_connected;
            p_col_connected_temp;
            p_col_not_connected;
            p_col_myself;
           ]
        ]
      in
      panel ~structure ~on_ok ()

    method toggle_temp_selected =
      List.iter
	(fun (i,h,p,_,t) ->
	  if t then data#add_people i h p
	  else data#remove_people i h p)
	selected_people;
      gui#update

    method kill_people_selected =
       List.iter
	(fun (i,h,p,_,t) ->
	  data#remove_people ~kill: true i h p)
	selected_people;
      gui#update

    method add_people =
      let p_id = preference ~help:M.h_id M.id "" BString () in
      let p_host = preference M.host "" BString () in
      let p_port = preference ~help:M.h_port M.port "5036" BInt () in
      let on_ok () =
        let id = p_id.pref_new_value in
        let host = p_host.pref_new_value in
        let port = int_of_float (safe_int p_port.pref_new_value) in
        match (id, host, port) with
            ("", _, p)
          | (_, "", p)
          | (_, _, p) when p < 1024 -> ()
          | _ -> 
              begin
                data#add_people id host port;
                self#say_hello (data#get_complete_people id host port);
                gui#update
              end
      in
      simple_panel ~prefs:[p_id; p_host; p_port] ~on_ok ()

    initializer
      ignore (itemOptions#connect#activate (fun () -> self#edit_conf));
      ignore (itemToggleTemp#connect#activate
		(fun () -> self#toggle_temp_selected));
      ignore (itemAddPeople#connect#activate (fun () -> self#add_people));
      ignore (itemRemovePeople#connect#activate (fun () -> self#kill_people_selected));

      ignore (GMain.Timeout.add ~ms: conf#timeout ~callback: self#work) ;
      List.iter self#say_hello data#people;
      gui#update;

      ignore (self#box#connect#destroy 
		(fun () -> 
		  List.iter self#say_byebye data#people ;
		  closed <- true;
		  com#close
		)
	     )

  end
