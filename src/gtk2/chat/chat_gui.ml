(**************************************************************************)
(*  Copyright 2003, 2002 b8_bavard, b8_zoggy, , b52_simon INRIA            *)
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

(** Graphical interface. *)

open Chat_types
open Chat_proto

type dialog_type = 
    Single of id * host * port
  | Room of id * (id * host * port) list

let remove_key ~target ~sign =
    ignore (target#misc#disconnect sign)

let add_key ~key ~target ~f ?(mods = [`MOD1]) ?source () =
  let sign =
    target#event#connect#key_press
      ~callback:(fun ev ->
        if GdkEvent.Key.keyval ev = key && GdkEvent.Key.state ev = mods
          then begin
            f (); 
            true 
          end else false
    )
  in
  Gaux.may ~f:(fun w -> 
    ignore (w#misc#connect#destroy 
      ~callback:(fun _ -> remove_key ~target ~sign)
  )) source


class dialog (data : Chat_data.data) typ_dial =
  let id, host, port = 
    match typ_dial with
      Single (i,h,p) -> (i,h,p)
    | Room (n,_) -> n,"",0
  in
  object (self)
    inherit Chat_gui_base.dialog ()
	
    val mutable name =
      match typ_dial with
	Single (id, h, p) -> Printf.sprintf "%s @ %s:%d" id h p
      |	Room (name, _) -> name

    method name =  name
    method id = id
    method host = host
    method port = port

    method send s =
      match typ_dial with
	Single (i,h,p) ->
	  (
	   try data#com#send i (h, p) (Message s) 
	   with Failure s -> Chat_messages.verbose s
	  )
      |	Room (name, people) ->
	  List.iter
	    (fun (i,h,p) ->
	      try data#com#send i (h, p) (RoomMessage (self#name, people, s))
	      with Failure s -> Chat_messages.verbose s
	    )
	    people

    method handle_message source_id mes =
      let col = data#conf#color_connected in
      ignore (wt_dialog#buffer#create_tag ~name:"foreground" [`FOREGROUND col]);
      wt_dialog#buffer#insert ~iter:(wt_dialog#buffer#get_iter `END) ~tag_names:["foreground"] source_id;
      wt_dialog#buffer#insert ~iter:(wt_dialog#buffer#get_iter `END) (" : "^mes^"\n")

    initializer
      let return () = 
        let start = wt_input#buffer#get_iter `START in
        let stop = wt_input#buffer#get_iter `END in
        let s = wt_input#buffer#get_text ~start ~stop () in
	let len = String.length s in
	let s2 = 
	  if len <= 0 then s
	  else
	    match s.[0] with
	      '\n' -> String.sub s 1 (len - 1)
	    | _ -> s
	in
	self#send s2;
        let col = data#conf#color_myself in
        ignore (wt_dialog#buffer#create_tag ~name:"myself_foreground" [`FOREGROUND col]);
        wt_dialog#buffer#insert ~iter:(wt_dialog#buffer#get_iter `END) ~tag_names:["myself_foreground"] data#conf#id;
        wt_dialog#buffer#insert ~iter:(wt_dialog#buffer#get_iter `END) (" : "^s2^"\n");
        wt_input#buffer#delete ~start ~stop
  
      in
      add_key ~key:GdkKeysyms._Return ~target:wt_input ~f:return ~mods:[] ();
      add_key ~key:GdkKeysyms._w ~target:wt_input ~f:box#destroy ~mods:[`CONTROL] ();
      add_key ~key:GdkKeysyms._w ~target:wt_dialog ~f:box#destroy ~mods:[`CONTROL] ();
      add_key ~key:GdkKeysyms._l ~target:wt_input ~f:wb_show_hide#clicked ~mods:[`CONTROL] ();
      add_key ~key:GdkKeysyms._l ~target:wt_dialog ~f:wb_show_hide#clicked ~mods:[`CONTROL] ();

      match typ_dial with
	Single _ -> 
	  wb_show_hide#misc#hide ();
	  wscroll_people#misc#hide ()
      |	Room (name, people) ->
	  wscroll_people#misc#hide ();
	  let show = ref false in
	  ignore (wb_show_hide#connect#clicked
		    (fun () -> 
		      show := not !show;
		      if !show then
			wscroll_people#misc#show ()
		      else
			wscroll_people#misc#hide ()));
	  List.iter
	    (fun (i,h,p) ->
	      ignore (wlist_people#append
			[i ; h ; string_of_int p]))
	    people;
	  GToolbox.autosize_clist wlist_people
  end

(** Liste des dialogs ouverts *)
let dialogs = ref ([] : (GWindow.window * dialog) list)

(** Liste des rooms ouvertes *)
let room_dialogs = ref ([] : (GWindow.window * dialog) list)

(** Remove the dialog with the given id from the list of dialogs. *)
let remove_dialog data typ_dial =
  match typ_dial with
    Single (id,host,port) ->
      dialogs := List.filter
	  (fun (_,d) -> not (data#pred (id,host,port) (d#id, d#host, d#port)))
	  !dialogs
  | Room (name, people) ->
      room_dialogs := List.filter
	  (fun (_,d) -> d#name <> name)
	  !room_dialogs


(** Find the window and dialog with the given id. It
   it was not found, create it and add it to the list of dialogs.*)
let get_dialog ?(show=true) data typ_dial =
  try
    match typ_dial with
      Single (id,host,port) ->
	let (w,d) = List.find 
	    (fun (w,d) -> data#pred (id,host,port) (d#id, d#host, d#port))
	    !dialogs 
	in
	d#wt_input#misc#grab_focus ();
	if show then w#show () ;
	d
    | Room (name, people) ->
	let (w,d) = List.find (fun (_,d) -> d#name = name) !room_dialogs in
	d#wt_input#misc#grab_focus ();
	if show then w#show () ;
	d
  with
    Not_found ->
      let window = GWindow.window ~kind:`TOPLEVEL ~width: 300 ~height: 200 ~title: "" () in
      ignore (window#connect#destroy (fun () -> remove_dialog data typ_dial));
      let dialog = new dialog data typ_dial in
      window#set_title dialog#name;
      ignore (dialog#box#connect#destroy window#destroy);
      window#add dialog#box#coerce;
      if show then window#show ();
      (
       match typ_dial with
	 Single _ -> dialogs := (window, dialog) :: ! dialogs
       | Room _ -> room_dialogs := (window, dialog) :: ! room_dialogs
      );
      dialog#wt_input#misc#grab_focus ();
      dialog

class gui no_quit (data : Chat_data.data) = 
  object (self)
    inherit Chat_gui_base.gui ()

    val mutable people = data#people

    val mutable selected_people = []

    method update =
      wlist#clear ();
      people <- data#people ;
      List.iter
	(fun (id,host,port,state,temp) ->
	  ignore (wlist#append ["" ; id ; host ; (string_of_int port) ; 
				 Chat_messages.yes_or_no temp]
		 );
	  let color,pix = 
	    match state, temp with
	      Connected, true -> 
		(data#conf#color_connected_temp,
		 Chat_icons.create_gdk_pixmap Chat_icons.connected)
	    | Connected, false -> 
		(data#conf#color_connected,
		 Chat_icons.create_gdk_pixmap Chat_icons.connected)
	    | Not_connected, _ -> 
		(data#conf#color_not_connected,
		 Chat_icons.create_gdk_pixmap Chat_icons.not_connected)
	  in
	  wlist#set_cell ~pixmap: pix (wlist#rows - 1) 0 ;
	  wlist#set_row ~foreground: (`NAME color) (wlist#rows - 1)
	)
	data#people;
      GToolbox.autosize_clist wlist;
      selected_people <- []

    method open_dialog (id, host, port, _, _) =
      ignore (get_dialog data (Single (id, host, port)))

    method open_room room_name people =
      let l = List.map (fun (i,h,p,_,_) -> (i,h,p)) people in
      ignore (get_dialog data (Room (room_name, l)))

    method open_dialog_for_selected_people =
      match selected_people with
	[] -> ()
      |	[p] -> self#open_dialog p
      |	l -> 
	  match GToolbox.input_string 
	      ~title: Chat_messages.m_open_dialog_for_selected_people
	      (Chat_messages.room_name^": ") 
	  with
	    None -> ()
	  | Some name ->
	      let c = data#conf in
	      self#open_room name ((c#id, c#hostname, c#port, Connected, false) :: l)

    initializer
      if no_quit then
	itemQuit#misc#hide ()
      else
	ignore (itemQuit#connect#activate box#destroy);
      ignore (itemOpenDialog#connect#activate 
		(fun () -> self#open_dialog_for_selected_people));
      ignore (itemAbout#connect#activate
		(fun () -> 
		  GToolbox.message_box 
		    Chat_messages.m_about
		    Chat_messages.software_about)
	     );


      let maybe_double_click (ev : GdkEvent.Button.t) = 
	let t = GdkEvent.get_type ev in
	match t with
	  `TWO_BUTTON_PRESS -> itemOpenDialog#activate ()
	| _ -> ()
      in

      let f_select ~row ~column ~event =
        try 
	  let (id,host,port,_,_) as p = List.nth people row in
	  if List.exists 
	      (fun (i,h,p,_,_) -> data#pred (id,host,port) (i,h,p)) 
	      selected_people 
	  then
	    ()
	  else
	    selected_people <- p :: selected_people ;
	  match event with
	    None -> ()
	  | Some ev -> maybe_double_click ev
        with _ -> ()
      in
      let f_unselect ~row ~column ~event =
        try 
	  let (id, host, port, _, _) = List.nth people row  in
	  selected_people <- List.filter
	      (fun (i,h,p,_,_) -> not (data#pred (id,host,port) (i,h,p)))
	      selected_people;
	  match event with
	    None -> ()
	  | Some ev -> maybe_double_click ev
        with _ -> ()
      in
      (* connect the select and deselect events *) 
      ignore (wlist#connect#select_row f_select) ;
      ignore (wlist#connect#unselect_row f_unselect) ;


  end
