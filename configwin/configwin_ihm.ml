(***********************************************************************)
(*                          Configwin                                  *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** This module contains the gui functions of Confgiwin.*)

open Configwin_types

(** This variable contains the last directory where the user selected a file.*)
let last_dir = ref "";;

(** This function allows the user to select a file and returns the
   selected file name. An optional function allows to change the 
   behaviour of the ok button.
   A VOIR : mutli-selection ? *)
let select_files ?dir
    ?(fok : (string -> unit) option)
    the_title =
  let files = ref ([] : string list) in 
  let fs = GWindow.file_selection ~modal:true
      ~title: the_title () in
  (* we set the previous directory, if no directory is given *)
  (
   match dir with
     None ->
       if !last_dir <> "" then
         let _ = fs#set_filename !last_dir in
         ()
       else
         ()
  | Some dir ->
      let _ = fs#set_filename !last_dir in
      ()
  );
 
  let _ = fs # connect#destroy ~callback: GMain.Main.quit in
  let _ = fs # ok_button # connect#clicked ~callback:
      (match fok with
        None ->
          (fun () -> files := [fs#get_filename] ; fs#destroy ())
      | Some f ->
          (fun () -> f fs#get_filename)
      )
  in
  let _ = fs # cancel_button # connect#clicked ~callback:fs#destroy in
  fs # show ();
  GMain.Main.main ();
  match !files with
  | [] ->
      []
  | [""] ->
      []
  | l ->
      (* we keep the directory in last_dir *)
      last_dir := Filename.dirname (List.hd l);
      l
;;

(** Make the user select a date. *)
let select_date title (day,mon,year) =
  let v_opt = ref None in
  let window = GWindow.dialog ~modal:true ~title () in
  let hbox = GPack.hbox ~border_width:10 ~packing:window#vbox#add () in
  let cal = GMisc.calendar ~packing: (hbox#pack ~expand: true) () in
  cal#select_month ~month: mon ~year: year ;
  cal#select_day day;
  let bbox = window#action_area in

  let bok = GButton.button ~label: Configwin_messages.mOk
      ~packing:(bbox#pack ~expand:true ~padding:4) ()
  in
  let bcancel = GButton.button ~label: Configwin_messages.mCancel
      ~packing:(bbox#pack ~expand:true ~padding:4) ()
  in
  ignore (bok#connect#clicked ~callback:
	    (fun () -> v_opt := Some (cal#date); window#destroy ()));
  ignore(bcancel#connect#clicked ~callback: window#destroy);

  bok#grab_default ();
  ignore(window#connect#destroy ~callback: GMain.Main.quit);
  window#set_position `CENTER;
  window#show ();
  GMain.Main.main ();
  !v_opt


(** This class builds a frame with a clist and two buttons :
   one to add items and one to remove the selected items.
   The class takes in parameter a function used to add items and
   a string list ref which is used to store the content of the clist.
   At last, a title for the frame  is also in parameter, so that     
   each instance of the class creates a frame. *)
class list_selection_box (listref : string list ref) add_function title editable =
  let wf = GBin.frame ~label: title () in
  let hbox = GPack.hbox ~packing: wf#add () in
  (* the scroll window and the clist *)
  let wscroll = GBin.scrolled_window ~packing: (hbox#pack ~expand: true) () in
  let wlist = GList.clist ~selection_mode: `EXTENDED ~packing: wscroll#add () in
  (* the vbox for the buttons *)
  let vbox_buttons = GPack.vbox () in
  let _ = 
    if editable then
      let _ = hbox#pack ~expand: false vbox_buttons#coerce in
      ()
    else
      () 
  in
  (* the two buttons add and remove *)
  let wb_add = GButton.button
      ~label: Configwin_messages.mAdd
      ~packing: (vbox_buttons#pack ~expand:false)
      ()
  in
  let wb_remove = GButton.button
      ~label: Configwin_messages.mRemove
      ~packing: (vbox_buttons#pack ~expand:false)
      ()
  in
  object
    (** This method returns the frame created. *)
    method frame = wf

    initializer
      (** the list of selected items *)
      let list_select = ref [] in
      (** create the functions called when the buttons are clicked *)
      let f_add () = 
        (* get the files to add with the function provided *)
        let l = add_function () in
        (* remove from the list the ones which are already in 
           the listref *)
        let l2 = List.fold_left
            (fun acc -> fun ele ->
              if List.mem ele acc then
                acc
              else
                acc @ [ele])
            !listref
            l
        in    
        (* set the new list in the provided listref *)
        listref := l2;
        (* insert the elements in the clist *)
        wlist#freeze ();
        wlist#clear ();
        List.iter (fun ele -> let _ = wlist#append [ ele ] in ()) !listref;
        wlist#thaw ();
        (* the list of selectd elements is now empty *)
        list_select := []
      in
      let f_remove () =
        (* remove the selected items from the listref and the clist *)
        let new_list = List.filter (fun ele -> not (List.mem ele !list_select)) !listref in
        listref := new_list;
        (* update the clist *)
        wlist#freeze ();
        wlist#clear ();
        List.iter (fun ele -> let _ = wlist#append [ ele ] in ()) !listref;
        wlist#thaw ();
        (* the list of selected elements is now empty *)
        list_select := []
      in
      (* connect the functions to the buttons *)
      let _ = wb_add#connect#clicked f_add in
      let _ = wb_remove#connect#clicked f_remove in
      (* connect the selection and deselection of items in the clist *)
      let f_select ~row ~column ~event =
        try
          list_select := (List.nth !listref row) :: !list_select
        with
          Failure _ ->
            ()
      in
      let f_unselect ~row ~column ~event =
        try
          let ele_selected = List.nth !listref row in
          let new_list_select = List.filter (fun ele -> ele <> ele_selected) !list_select in
          list_select := new_list_select
        with
          Failure _ ->
            ()
      in
      (* connect the select and deselect events *)
      let _ = wlist#connect#select_row f_select in
      let _ = wlist#connect#unselect_row f_unselect in

      (* initialize the clist with the listref *)
      wlist#freeze ();
      List.iter (fun ele -> let _ = wlist#append [ ele ] in ()) !listref;
      wlist#thaw ()      
  end;;


(** This class is used to build a box for a string parameter.*)
class string_param_box param =
  let hbox = GPack.hbox () in
  let wl = GMisc.label ~text: param.string_label ~packing: (hbox#pack ~expand: false ~padding: 2) () in
  let we = GEdit.entry
      ~editable: param.string_editable
      ~packing: (hbox#pack ~expand: true ~padding: 2)
      ()
  in
  let _ = we#set_text param.string_value in

  object (self)
    (** This method returns the main box ready to be packed. *)
    method box = hbox#coerce
    (** This method applies the new value of the parameter. *)
    method apply =
      let new_value = we#text in
      if new_value <> param.string_value then
	let _ = param.string_f_apply new_value in
	param.string_value <- new_value
      else
	()
  end ;;

(** This class is used to build a box for a combo parameter.*)
class combo_param_box param =
  let hbox = GPack.hbox () in
  let wl = GMisc.label ~text: param.combo_label ~packing: (hbox#pack ~expand: false ~padding: 2) () in
  let wc = GEdit.combo
      ~popdown_strings: param.combo_choices
      ~value_in_list: (not param.combo_new_allowed)
      ~ok_if_empty: param.combo_blank_allowed
      ~packing: (hbox#pack ~expand: true ~padding: 2)
      ()
  in
  let _ = wc#entry#set_editable param.combo_editable in
  let _ = wc#entry#set_text param.combo_value in

  object (self)
    (** This method returns the main box ready to be packed. *)
    method box = hbox#coerce
    (** This method applies the new value of the parameter. *)
    method apply =
      let new_value = wc#entry#text in
      if new_value <> param.combo_value then
	let _ = param.combo_f_apply new_value in
	param.combo_value <- new_value
      else
	()
  end ;;

(** Class used to pack a custom box. *)
class custom_param_box param =
  object (self)
    method box = param.custom_box#coerce
    method apply = param.custom_f_apply ()
  end

(** This class is used to build a box for a color parameter.*)
class color_param_box param =
  let hbox = GPack.hbox () in
  let wb = GButton.button ~label: param.color_label 
      ~packing: (hbox#pack ~expand: false ~padding: 2) () 
  in
  let we = GEdit.entry
      ~editable: param.color_editable
      ~packing: (hbox#pack ~expand: true ~padding: 2)
      ()
  in
  let set_wb_color s =
    let style = wb#misc#style#copy in
    (
     try style#set_bg [ (`NORMAL, `NAME s) ; ]
     with _ -> ()
    );
    wb#misc#set_style style
  in
  let _ = set_wb_color param.color_value in
  let _ = we#set_text param.color_value in
  let f_sel () =
    let dialog = GWindow.color_selection_dialog
	~title: param.color_label
	~modal: true
	~show: true
	()
    in
    let wb_ok = dialog#ok_button in
    let wb_cancel = dialog#cancel_button in
    let _ = dialog#connect#destroy GMain.Main.quit in
    let _ = wb_ok#connect#clicked
	(fun () -> 
	  let color = dialog#colorsel#get_color in
	  let r = int_of_float (ceil (color.Gtk.red *. 255.)) in
	  let g = int_of_float (ceil (color.Gtk.green *. 255.)) in
	  let b = int_of_float (ceil (color.Gtk.blue *. 255.)) in
	  let s = Printf.sprintf "#%2X%2X%2X" r g b in
	  let _ = 
	    for i = 1 to (String.length s) - 1 do
	      if s.[i] = ' ' then s.[i] <- '0'
	    done
	  in
	  we#set_text s ;
	  set_wb_color s;
	  dialog#destroy ()
	)
    in
    let _ = wb_cancel#connect#clicked dialog#destroy in
    GMain.Main.main ()
  in
  let _ = wb#connect#clicked f_sel in

  object (self)
    (** This method returns the main box ready to be packed. *)
    method box = hbox#coerce
    (** This method applies the new value of the parameter. *)
    method apply =
      let new_value = we#text in
      if new_value <> param.color_value then
	let _ = param.color_f_apply new_value in
	param.color_value <- new_value
      else
	()
  end ;;


(** This class is used to build a box for a text parameter.*)
class text_param_box param =
  let hbox = GPack.hbox () in
  let wl = GMisc.label ~text: param.string_label ~packing: (hbox#pack ~expand: false ~padding: 2) () in
  let wt = GEdit.text
      ~editable: param.string_editable
      ~packing: (hbox#pack ~expand: true ~padding: 2)
      ()
  in
  let _ = wt#insert param.string_value in

  object (self)
    (** This method returns the main box ready to be packed. *)
    method box = hbox#coerce
    (** This method applies the new value of the parameter. *)
    method apply =
      let new_value = wt#get_chars 0 wt#length in
      if new_value <> param.string_value then
	let _ = param.string_f_apply new_value in
	param.string_value <- new_value
      else
	()
  end ;;

(** This class is used to build a box for a boolean parameter.*)
class bool_param_box param =
  let wchk = GButton.check_button
      ~label: param.bool_label
      ()
  in
  let _ = wchk#set_active param.bool_value in
  let _ = wchk#misc#set_sensitive param.bool_editable in

  object (self)
    (** This method returns the check button ready to be packed. *)
    method box = wchk#coerce
    (** This method applies the new value of the parameter. *)
    method apply =
      let new_value = wchk#active in
      if new_value <> param.bool_value then
	let _ = param.bool_f_apply new_value in
	param.bool_value <- new_value
      else
	()
  end ;;

(** This class is used to build a box for a file name parameter.*)
class filename_param_box param =
  let hbox = GPack.hbox () in
  let wb = GButton.button ~label: param.string_label 
      ~packing: (hbox#pack ~expand: false ~padding: 2) () 
  in
  let we = GEdit.entry
      ~editable: param.string_editable
      ~packing: (hbox#pack ~expand: true ~padding: 2)
      ()
  in
  let _ = we#set_text param.string_value in

  let f_click () =
    match select_files param.string_label with
      [] ->
        ()
    | f :: _ ->
        we#set_text f
  in
  let _ = 
    if param.string_editable then
      let _ = wb#connect#clicked f_click in
      ()
    else
      ()
  in

  object (self)
    (** This method returns the main box ready to be packed. *)
    method box = hbox#coerce
    (** This method applies the new value of the parameter. *)
    method apply =
      let new_value = we#text in
      if new_value <> param.string_value then
	let _ = param.string_f_apply new_value in
	param.string_value <- new_value
      else
	()
  end ;;

(** This class is used to build a box for a date parameter.*)
class date_param_box param =
  let v = ref param.date_value in
  let hbox = GPack.hbox () in
  let wb = GButton.button ~label: param.date_label 
      ~packing: (hbox#pack ~expand: false ~padding: 2) () 
  in
  let we = GEdit.entry
      ~editable: false
      ~packing: (hbox#pack ~expand: true ~padding: 2)
      ()
  in
  let _ = we#set_text (param.date_f_string param.date_value) in

  let f_click () =
    match select_date param.date_label param.date_value with
      None -> ()
    | Some (y,m,d) -> 
	v := (d,m,y) ;
	we#set_text (param.date_f_string (d,m,y))
  in
  let _ = 
    if param.date_editable then
      let _ = wb#connect#clicked f_click in
      ()
    else
      ()
  in

  object (self)
    (** This method returns the main box ready to be packed. *)
    method box = hbox#coerce
    (** This method applies the new value of the parameter. *)
    method apply =
      if !v <> param.date_value then
	let _ = param.date_f_apply !v in
	param.date_value <- !v
      else
	()
  end ;;

(** This class is used to build a box for a parameter whose values are lists of strings.*)
class strings_param_box param =
  let listref = ref param.strings_value in
  let frame_selection = new list_selection_box
      listref param.strings_f_add param.strings_label param.strings_editable
  in
      
  object (self)
    (** This method returns the main box ready to be packed. *)
    method box = frame_selection#frame#coerce
    (** This method applies the new value of the parameter. *)
    method apply =
      if !listref <> param.strings_value then
	let _ = param.strings_f_apply !listref in
	param.strings_value <- !listref
      else
	()
  end ;;

(** This class is used to build a box for a parameter whose values are lists of filenames.*)
class filenames_param_box param =
  let listref = ref param.files_value in
  let f_add () = select_files param.files_label in
  let frame_selection = new list_selection_box
      listref f_add  param.files_label param.files_editable
  in
      
  object (self)
    (** This method returns the main box ready to be packed. *)
    method box = frame_selection#frame#coerce
    (** This method applies the new value of the parameter. *)
    method apply =
      if !listref <> param.files_value then
	let _ = param.files_f_apply !listref in
	param.files_value <- !listref
      else
	()
  end ;;


(** This class is used to build a box from a configuration structure 
   and adds the page to the given notebook. *)
class configuration_box conf_struct (notebook : GPack.notebook) =
  (* we build different widgets, according to the conf_struct parameter *)
  let main_box = GPack.vbox () in
  let (label, child_boxes) = 
    match conf_struct with
      Section (label, param_list) ->
	let f parameter =
	      match parameter with
		String_param p ->
		  let box = new string_param_box p in
		  let _ = main_box#pack ~expand: false ~padding: 2 box#box in
		  box
	      |	Combo_param p ->
		  let box = new combo_param_box p in
		  let _ = main_box#pack ~expand: false ~padding: 2 box#box in
		  box
	      |	Text_param p ->
		  let box = new text_param_box p in
		  let _ = main_box#pack ~expand: true ~padding: 2 box#box in
		  box
	      | Bool_param p ->
		  let box = new bool_param_box p in
		  let _ = main_box#pack ~expand: false ~padding: 2 box#box in
		  box
	      |	Filename_param p ->
		  let box = new filename_param_box p in
		  let _ = main_box#pack ~expand: false ~padding: 2 box#box in
		  box
	      |	Strings_param p ->
		  let box = new strings_param_box p in
		  let _ = main_box#pack ~expand: true ~padding: 2 box#box in
		  box
	      |	Custom_param p ->
		  let box = new custom_param_box p in
		  let _ = main_box#pack ~expand: p.custom_expand ~padding: 2 box#box in
		  box
	      |	Color_param p ->
		  let box = new color_param_box p in
		  let _ = main_box#pack ~expand: false ~padding: 2 box#box in
		  box
	      |	Filenames_param p ->
		  let box = new filenames_param_box p in
		  let _ = main_box#pack ~expand: true ~padding: 2 box#box in
		  box
	      |	Date_param p ->
		  let box = new date_param_box p in
		  let _ = main_box#pack ~expand: false ~padding: 2 box#box in
		  box
	in
	let list_children_boxes = List.map f param_list in
	
	(label, list_children_boxes)

    | Section_list (label, struct_list) ->
	let wnote = GPack.notebook 
            (*homogeneous_tabs: true*)
	    ~scrollable: true
	    ~show_tabs: true
	    ~tab_border: 3
	    ~packing: (main_box#pack ~expand: true)
	    ()
	in
	(* we create all the children boxes *)
	let f structure =
	  let new_box = new configuration_box structure wnote in
	  new_box
	in
	let list_child_boxes = List.map f struct_list in
	(label, list_child_boxes)
	
  in
  let page_label = GMisc.label ~text: label () in
  let _ = notebook#append_page 
      ~tab_label: page_label#coerce
      main_box#coerce
  in

  object (self)
    (** This method returns the main box ready to be packed. *)
    method box = main_box#coerce
    (** This method make the new values of the paramters applied, recursively in
       all boxes.*)
    method apply =
      List.iter (fun box -> box#apply) child_boxes
  end
;;


(** This function takes a configuration structure list and creates a window
   to configure the various parameters. *)
let edit ?(with_apply=true)  title ?(width=400) ?(height=400) conf_struct_list =
  let return = ref Return_cancel in
  let window = GWindow.window
      ~modal: true ~title: title
      ~width: width ~height: height ()
  in
  let _ = window#connect#destroy ~callback: GMain.Main.quit in
  let vbox = GPack.vbox ~packing: window#add () in
  let wnote = GPack.notebook 
      (*homogeneous_tabs: true*)
      ~scrollable: true
      ~show_tabs: true
      ~tab_border: 3
      ~packing: (vbox#pack ~expand: true)
      ()
  in
  let list_param_box = 
    List.map (fun conf_struct -> new configuration_box conf_struct wnote)
      conf_struct_list
  in

  let hbox_buttons = GPack.hbox ~packing: (vbox#pack ~expand: false ~padding: 4) () in
  let bApply = GButton.button
      ~label: Configwin_messages.mApply
      ()
  in
  if with_apply then hbox_buttons#pack ~expand: true ~padding: 3 bApply#coerce;
  let bOk = GButton.button
      ~label: Configwin_messages.mOk
      ~packing: (hbox_buttons#pack ~expand: true ~padding: 3)
      ()
  in
  let bCancel = GButton.button
      ~label: Configwin_messages.mCancel
      ~packing: (hbox_buttons#pack ~expand: true ~padding: 3)
      ()
  in
  (* we connect the click on the apply button *)
  let f_apply () = 
    List.iter (fun param_box -> param_box#apply) list_param_box  ;
    return := Return_apply
  in
  let _ = bApply#connect#clicked f_apply in
  (* we connect the click on the ok button : the same than apply but we then close the window *)
  let f_ok () = 
    f_apply () ; 
    return := Return_ok ;
    window#destroy () 
  in
  let _ = bOk#connect#clicked f_ok in
  (* we connect the click on the cancel button : close the window *)
  let f_cancel () = window#destroy () in
  let _ = bCancel#connect#clicked f_cancel in

  let _ = window#show () in
  GMain.Main.main () ;
  !return


(** This function takes a list of parameter specifications and 
   creates a window to configure the various parameters.*)
let simple_edit ?(with_apply=true)  
    title ?width ?height 
    param_list =
  let return = ref Return_cancel in
  let window = GWindow.window ~modal: true ~title: title () in
  let _ = match width, height with
    None, None -> ()
  | Some w, None -> window#misc#set_geometry ~width: w ()
  | None, Some h -> window#misc#set_geometry ~height: h ()
  | Some w, Some h -> window#misc#set_geometry ~width: w ~height: h ()
  in
  let _ = window#connect#destroy ~callback: GMain.Main.quit in
  let main_box = GPack.vbox ~packing: window#add () in
  let f parameter =
    match parameter with
      String_param p ->
	let box = new string_param_box p in
	let _ = main_box#pack ~expand: false ~padding: 2 box#box in
	box
    | Combo_param p ->
	let box = new combo_param_box p in
	let _ = main_box#pack ~expand: false ~padding: 2 box#box in
	box
    | Text_param p ->
	let box = new text_param_box p in
	let _ = main_box#pack ~expand: true ~padding: 2 box#box in
	box
    | Bool_param p ->
	let box = new bool_param_box p in
	let _ = main_box#pack ~expand: false ~padding: 2 box#box in
	box
    | Filename_param p ->
	let box = new filename_param_box p in
	let _ = main_box#pack ~expand: false ~padding: 2 box#box in
	box
    | Strings_param p ->
	let box = new strings_param_box p in
	let _ = main_box#pack ~expand: true ~padding: 2 box#box in
	box
    | Custom_param p ->
	let box = new custom_param_box p in
	let _ = main_box#pack ~expand: p.custom_expand ~padding: 2 box#box in
	box
    | Color_param p ->
	let box = new color_param_box p in
	let _ = main_box#pack ~expand: false ~padding: 2 box#box in
	box
    | Filenames_param p ->
	let box = new filenames_param_box p in
	let _ = main_box#pack ~expand: true ~padding: 2 box#box in
	box
    | Date_param p ->
	let box = new date_param_box p in
	let _ = main_box#pack ~expand: false ~padding: 2 box#box in
	box
  in
  let list_param_box = List.map f param_list in

  let hbox_buttons = GPack.hbox ~packing: (main_box#pack ~expand: false ~padding: 4) () in
  let bApply = GButton.button
      ~label: Configwin_messages.mApply
      ()
  in
  if with_apply then hbox_buttons#pack ~expand: true ~padding: 3 bApply#coerce;
  let bOk = GButton.button
      ~label: Configwin_messages.mOk
      ~packing: (hbox_buttons#pack ~expand: true ~padding: 3)
      ()
  in
  let bCancel = GButton.button
      ~label: Configwin_messages.mCancel
      ~packing: (hbox_buttons#pack ~expand: true ~padding: 3)
      ()
  in
  (* we connect the click on the apply button *)
  let f_apply () = 
    List.iter (fun param_box -> param_box#apply) list_param_box  ;
    return := Return_apply
  in
  let _ = bApply#connect#clicked f_apply in
  (* we connect the click on the ok button : the same than apply but we then close the window *)
  let f_ok () = 
    f_apply () ; 
    return := Return_ok ;
    window#destroy () 
  in
  let _ = bOk#connect#clicked f_ok in
  (* we connect the click on the cancel button : close the window *)
  let f_cancel () = window#destroy () in
  let _ = bCancel#connect#clicked f_cancel in

  let _ = window#show () in
  GMain.Main.main () ;
  !return

  
(** Create a string param. *)
let string ?(edit=true) ?(f=(fun _ -> ())) label v =
  String_param
    {
      string_label = label ;
      string_value = v ;
      string_editable = edit ;
      string_f_apply = f ;
    } 

(** Create a bool param. *)
let bool ?(edit=true) ?(f=(fun _ -> ())) label v =
  Bool_param
    {
      bool_label = label ;
      bool_value = v ;
      bool_editable = edit ;
      bool_f_apply = f ;
    }

(** Create a strings param. *)
let strings ?(edit=true) ?(f=(fun _ -> ())) ?(add=(fun () -> [])) label v =
  Strings_param
    {
      strings_label = label ;
      strings_value = v ;
      strings_editable = edit ;
      strings_f_add = add ;
      strings_f_apply = f ;
    } 
  
(** Create a color param. *)
let color ?(edit=true) ?(f=(fun _ -> ())) label v =
  Color_param
    {
      color_label = label ;
      color_value = v ;
      color_editable = edit ;
      color_f_apply = f ;
    }

(** Create a combo param. *)
let combo ?(edit=true) ?(f=(fun _ -> ())) 
    ?(new_allowed=false) 
    ?(blank_allowed=false) label choices v =
  Combo_param
    {
      combo_label = label ;
      combo_value = v ;
      combo_editable = edit ;
      combo_choices = choices ;
      combo_new_allowed = new_allowed ;
      combo_blank_allowed = blank_allowed ;
      combo_f_apply = f ;
    }

(** Create a text param. *)
let text ?(edit=true) ?(f=(fun _ -> ())) label v = 
  Text_param
    {
      string_label = label ;
      string_value = v ;
      string_editable = edit ;
      string_f_apply = f ;
    } 

(** Create a filename param. *)
let filename ?(edit=true) ?(f=(fun _ -> ())) label v = 
  Filename_param
    {
      string_label = label ;
      string_value = v ;
      string_editable = edit ;
      string_f_apply = f ;
    } 

(** Create a filenames param.*)
let filenames ?(edit=true) ?(f=(fun _ -> ())) label v =
  Filenames_param
    {
      files_label = label ;
      files_value = v ;
      files_editable = edit ;
      files_f_apply = f ;
    } 

(** Create a date param. *)
let date  ?(edit=true) ?(f=(fun _ -> ())) 
    ?(f_string=(fun(d,m,y)->Printf.sprintf "%d/%d/%d" y (m+1) d))
    label v =
  Date_param
    {
      date_label = label ;
      date_value = v ;
      date_editable = edit ;
      date_f_string = f_string ;
      date_f_apply = f ;
    } 

(** Create a custom param.*)
let custom box f expand =
  Custom_param
    {
      custom_box = box ;
      custom_f_apply = f ;
      custom_expand = expand ;
    } 
