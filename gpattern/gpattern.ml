(***********************************************************************)
(*                                GPattern                             *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

type content =
  | String of string 
  | Pixmap of GDraw.pixmap

class virtual ['a] plist
    sel_mode titles titles_show =
  let wscroll = GBin.scrolled_window
      ~hpolicy: `AUTOMATIC
      ~vpolicy: `AUTOMATIC
      () 
  in
  let (wlist : 'a GList.clist) = GList.clist_poly 
      ~titles_show: titles_show
      ~titles: titles
      ~selection_mode: sel_mode
      ~packing: wscroll#add 
      () 
  in
  object (self)
    val mutable data = ([] : 'a list)
    val mutable current_sort = 0
    val mutable selection = ([] : 'a list)
    val mutable wlist = wlist

    method box = wscroll#coerce
    method wlist = wlist

    method update_data l =
      data <- l;
      self#update

    method virtual content : 'a -> content list * GDraw.optcolor option
    method compare = Pervasives.compare
    method selection = selection
    method on_select (d:'a) = ()
    method on_deselect (d:'a) = ()
    method on_double_click (d:'a) = ()

    method private sort l =
      List.sort self#compare l

    method set_titles l =
      wscroll#remove wlist#coerce;
      let (w : 'a GList.clist) = GList.clist_poly 
	  ~titles_show: titles_show
	  ~titles: l
	  ~selection_mode: sel_mode
	  ~packing: wscroll#add 
	  () 
      in
      wlist <- w;
      self#connect_events

    method update_row d row =
      let (l, col_opt) = self#content d in
      let rec iter n l = 
	match l with
	  [] -> ()
	| (String s) :: q -> 
	    wlist#set_cell ~text: s row n;
	    iter (n+1) q
	| (Pixmap p) :: q -> 
	    wlist#set_cell ~pixmap: p row n;
	    iter (n+1) q
      in
      iter 0 l;
      match col_opt with
	None -> ()
      |	Some c -> wlist#set_row ~foreground: c row
      

    method insert ?row d =
      let f_string content =
	match content with
	  String s -> s
	| Pixmap _ -> ""
      in
      let r = 
	match row with
	  None -> ignore (wlist#append []) ; wlist#rows - 1
	| Some p -> ignore (wlist#insert ~row: p []) ; p
      in
      self#update_row d r
      
    method update =
      wlist#freeze ();
      wlist#clear ();
      data <- self#sort data;
      selection <- [];
      List.iter self#insert data;
      GToolbox.autosize_clist wlist;
      wlist#thaw ()

    method menu = ([] : GToolbox.menu_entry list)

    method private connect_events =
      let check_double_click event d =
        (
         match event with 
           None -> ()
         | Some ev -> 
             let t = GdkEvent.get_type ev in
             match t with
               `TWO_BUTTON_PRESS -> self#on_double_click d
             | _ -> ()
        )
      in
      let f_select_table ~row ~column ~event =
        try 
	  let d = List.nth data row in
	  selection <- d ::
            (List.filter (fun d2 -> d <> d2) selection);
	  self#on_select d;
          check_double_click event d
        with Failure _ -> ()
      in
      let f_unselect_table ~row ~column ~event = 
	  let d = List.nth data row in
	  selection <- 
            (List.filter (fun d2 -> d <> d2) selection);
	  self#on_deselect d;
          check_double_click event d
      in
      ignore (wlist#connect#select_row f_select_table);
      ignore (wlist#connect#unselect_row f_unselect_table);
      ignore (wlist#connect#click_column
		(fun c -> 
		  let n = c + 1 in
		  if current_sort = n or (- current_sort) = n then
		    current_sort <- (- current_sort)
		  else
		    current_sort <- n;
		  self#update
		)
	     );
      (* connect the press on button 3 for contextual menu *)
      ignore (wlist#event#connect#button_press ~callback:
                (
                 fun ev ->
                   GdkEvent.Button.button ev = 3 &&
                   GdkEvent.get_type ev = `BUTTON_PRESS &&
                   (
                    GToolbox.popup_menu 
                      ~x: (int_of_float (GdkEvent.Button.x ev))
                      ~y: (int_of_float (GdkEvent.Button.y ev))
                      ~entries: self#menu;
                    true
                   )
                )
             )

    initializer
      self#connect_events;
      GToolbox.autosize_clist self#wlist


  end
