(***********************************************************************)
(*                               MLChat                                *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** Graphical interface. *)

open Chat_types
open Chat_proto

class dialog (data : Chat_data.data) id host port =
  object (self)
    inherit Chat_gui_base.dialog ()
	
    method id = id (** id of the person this dialog is used to talk to *)
    method host = host
    method port = port

    method send s =
      try data#com#send id (host, port) (Message s) 
      with Failure s ->prerr_endline s

    method handle_message mes =
      wt_dialog#insert ~foreground: (`NAME data#conf#color_connected) id;
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
	  ~foreground: (`NAME data#conf#color_myself) 
	  data#conf#id;
	wt_dialog#insert (" : "^s2^"\n") ;
	wt_input#delete_text ~start: 0 ~stop: wt_input#length
	  
      in
      Okey.add wt_input ~mods: [] GdkKeysyms._Return return;
      Okey.add_list wt_input ~mods: [`CONTROL]
	[GdkKeysyms._c; GdkKeysyms._C]
	box#destroy;
      Okey.add_list wt_dialog ~mods: [`CONTROL] 
	[GdkKeysyms._c; GdkKeysyms._C]
	box#destroy
  end

(** Liste des dialogs ouverts *)
let dialogs = ref ([] : (GWindow.window * dialog) list)


(** Remove the dialog with the given id from the list of dialogs. *)
let remove_dialog id =
  dialogs := List.filter
      (fun (_,d) -> d#id <> id)
      !dialogs

(** Find the window and dialog with the given id. It
   it was not found, create it and add it to the list of dialogs.*)
let get_dialog ?(show=true) data id host port =
  try
    let (w,d) = List.find 
	(fun (w,d) -> data#pred (id,host,port) (d#id, d#host, d#port))
	!dialogs 
    in
    d#wt_input#misc#grab_focus ();
    if show then w#show () ;
    d
  with
    Not_found ->
      let window = GWindow.window ~kind: `DIALOG ~width: 300 ~height: 200 ~title: id () in
      ignore (window#connect#destroy (fun () -> remove_dialog id));
      let dialog = new dialog data id host port in
      ignore (dialog#box#connect#destroy window#destroy);
      window#add dialog#box#coerce;
      if show then window#show ();
      dialogs := (window, dialog) :: ! dialogs;
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
      ignore (get_dialog data id host port)

    method open_dialog_for_selected_people =
      match selected_people with
	[] -> ()
      |	l -> 
	  (* pour l'instant, pas de conférence, un
	     dialog pour chaque personne sélectionnée *)
	  List.iter self#open_dialog l

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
	  selected_people <- (List.nth people row) :: selected_people ;
	  match event with
	    None -> ()
	  | Some ev -> maybe_double_click ev
        with _ -> ()
      in
      let f_unselect ~row ~column ~event =
        try 
	  let (id, host, port, _, _) = List.nth people row  in
	  selected_people <- List.filter
	      (fun (i,h,p,_,_) -> h <> host or p <> port)
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
