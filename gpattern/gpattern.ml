(* Copyright 2001, 2002, 2003 Maxence Guesdon, b52_simon INRIA *)
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

open GtkBase

type content =
| String of string 
| Pixmap of GDraw.pixmap

type line = content list * GDraw.optcolor option
  
let dummy_line = [], None

class virtual ['a] filtered_plist
    sel_mode titles titles_show get_key =
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
    val mutable items = ([||] : 'a array)
    val mutable contents = ([||] : line array)
    val mutable nitems = 0
    
    val mutable current_sort = 0
    val mutable selection = ([] : 'a list)
    val mutable wlist = wlist
    
    val mutable filtered = Intmap.empty
    val mutable nfiltered = 0
    
    method box = wscroll#coerce
    method wlist = wlist
    
    method virtual content : 'a -> line 
    method compare = Pervasives.compare
    method selection = selection
    method on_select (d:'a) = ()
    method on_deselect (d:'a) = ()
    method on_double_click (d:'a) = ()
    
    method private sort = 
      Array.fill contents 0 nitems dummy_line;
      let array = Array.init nitems (fun i ->
            (items.(i),i)) in
      Array.sort (fun (v1,n1) (v2,n2) ->
          let cmp = self#compare v1 v2 in
          if cmp = 0 then compare n1 n2 else cmp
      ) array;
      for i = 0 to nitems - 1 do
        let item, _ = array.(i) in
        items.(i) <- item
      done
    
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
      if row >= 0 then begin
          try
            let line = self#content d in
            if line <> contents.(row) then begin
                contents.(row) <- line;
                let (l, col_opt) = line in
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
                | Some c -> 
                    wlist#set_row ~foreground: c row
              end
          with e ->
              Printf.printf "Exception %s in update_row" (Printexc2.to_string e);
              print_newline ()
        end else begin
          Printf.printf "update_row < 0"; print_newline ();
        end
    
    method insert ?row d =
      let f_string content =
        match content with
          String s -> s
        | Pixmap _ -> ""
      in
      let r = 
        match row with
          None -> 
            ignore (wlist#append []) ; wlist#rows - 1
        | Some p -> 
            ignore (wlist#insert ~row: p []) ; p
      in
      self#update_row d r
    
    method update =
      wlist#freeze ();
      wlist#clear ();
      self#sort;
      selection <- [];
      for i = 0 to nitems - 1 do
        self#insert items.(i);
      done;
      GToolbox.autosize_clist wlist;
      wlist#thaw ()
    
    method clear =
      wlist#clear ();
      nitems <- 0;
      items <- [||];
      contents <- [||]
    
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
          let d = items.(row) in
          selection <- d ::
          (List.filter (fun d2 -> d <> d2) selection);
          self#on_select d;
          check_double_click event d
        with Failure _ -> ()
      in
      let f_unselect_table ~row ~column ~event = 
        try
          let d = items.(row) in
          selection <- 
            (List.filter (fun d2 -> d <> d2) selection);
          self#on_deselect d;
          check_double_click event d
        with Failure _ -> ()
      in
      ignore (wlist#connect#select_row f_select_table);
      ignore (wlist#connect#unselect_row f_unselect_table);
      ignore (wlist#connect#click_column
          (fun c -> 
            GToolbox.autosize_clist self#wlist;
            self#resort_column c ()
        )
      );

(* connect the press on button 3 for contextual menu *)
      ignore (wlist#event#connect#button_press ~callback:
        (
          fun ev ->
            GdkEvent.Button.button ev = 3 &&
            GdkEvent.get_type ev = `BUTTON_PRESS &&
            (
              GAutoconf.popup_menu 
                ~button: 3
                ~time: 0
              ~entries: self#menu;
              true
            )
        )
      );
      
      
(* connect the press on button 3 for title contextual menu *)
      for col = 0 to wlist#columns - 1 do
        let w = wlist#column_widget col in
        let b = Widget.get_ancestor w#as_widget (Type.from_name "GtkButton") in
        let button = new GButton.button (Object.unsafe_cast b) in
        ignore (button#event#connect#button_press ~callback:
          (
            fun ev ->
              GdkEvent.Button.button ev = 3 &&
              GdkEvent.get_type ev = `BUTTON_PRESS &&
              (
                GAutoconf.popup_menu
                  ~button: 3
                  ~time: 0
                  ~entries: (self#column_menu col);
                true
              )
          )
        )
      done
(*

(* Connection bouton droit sur un titre, pour chaque colonne *)
      Printf.printf "ncols: %d" wlist#columns; print_newline ();

      let col = ref 0 in
      List.iter (fun button ->
          let event = new GObj.event_ops button#as_widget in
          let i = !col in
          incr col;
        ignore 
          (event#connect#button_press ~callback:
          (
            fun ev ->
              
              if GdkEvent.get_type ev = `BUTTON_PRESS then
                
                if GdkEvent.Button.button ev = 1 then
                    let n = i + 1 in
                  if current_sort = n || (- current_sort) = n then
                    current_sort <- (- current_sort)
                  else
                    current_sort <- n;
                  self#update
                else
                if GdkEvent.Button.button ev = 3 then                  
                  GAutoconf.popup_menu
                    ~button: 3
                    ~time: 0
                    ~entries: (self#column_menu i); 
                true
            ))
      ) wlist#children;

          (*
      for i = 0 to wlist#columns - 1 do
        Printf.printf " for %d" i; print_newline ();
        let widget = wlist#column_widget i in

(*
        let tooltips = GData.tooltips ~delay:  1 () in
        tooltips#set_tip ~text:"Essay" ~privat: "test" widget;
*)
        
        let button = GButton.button () in
        wlist#set_column_widget i button#coerce;
        let _ =
          GMisc.label ~width:50 ~text: "toto"
            ~justify:`RIGHT ~line_wrap:true ~xalign:(-1.0) ~yalign:(-1.0)
          ~packing:button#add ()
        in
        Printf.printf "done for %d" i; print_newline ();
        
        
(*        let event = new GObj.event_ops widget#as_widget in*)
        
        ignore 
          (button#event#connect#button_press ~callback:
          (
            fun ev ->
              
              if GdkEvent.get_type ev = `BUTTON_PRESS then
                
                if GdkEvent.Button.button ev = 1 then
                  let n = i + 1 in
                  if current_sort = n || (- current_sort) = n then
                    current_sort <- (- current_sort)
                  else
                    current_sort <- n;
                  self#update
                else
                true
          )
        )
      done; *)
      Printf.printf "ok"; print_newline ();
      self#update

*)
    
    initializer
      self#connect_events;
      GToolbox.autosize_clist self#wlist
    
    
    method resort_column i () =
      let n = i + 1 in
      if current_sort = n or (- current_sort) = n then
        current_sort <- (- current_sort)
      else
        current_sort <- n;
      self#update
      
    method column_menu i = 
      [
        `I ("sort", self#resort_column i);
        (*
        `I ("you clicked on the 3rd button", 
          fun () -> print_string (wlist#column_title i); print_newline ();
            Printf.printf "You asked to remove column %d" i;
            print_newline ();
        ) *)
      ]
      
    method find key =
      let rec iter i len =
        if i = len then (-1, Intmap.find key filtered) else
        if get_key items.(i) = key then (i, items.(i)) else
          iter (i+1) len
      in
      iter 0 nitems
    
    method add_item i =
      if self#filter i then
        self#add_filtered_item i
      else  begin
         let pos = nitems in
          self#add_hidden_item i;
          self#insert ~row: pos i;
        end 
    
    method size = nitems + nfiltered
    
    method private add_hidden_item i =
      if nitems = Array.length items then begin
          let nis = nitems * 2 + 8 in
          let is = Array.create nis i in
          Array.blit items 0 is 0 nitems;
          contents <- Array.create nis dummy_line;
          items <- is
        end else begin
          items.(nitems) <- i;
          contents.(nitems) <- dummy_line;
        end;
      nitems <- nitems+1  
    
    method private add_filtered_item i =
      let key = get_key i in
      filtered <- Intmap.add key i filtered;
      nfiltered <- nfiltered + 1
    
    method refresh_filter =
      let filter = self#filter in
      let add_filtered_item = self#add_filtered_item in
      let add_hidden_item = self#add_hidden_item in
      let old_items = Array.sub items 0 nitems in
      let old_map = filtered in
      wlist#freeze ();
      wlist#clear ();
      nitems <- 0;
      filtered <- Intmap.empty;
      nfiltered <- 0;
      items <- [||];
      contents <- [||];
      Array.iter (fun i -> 
          if filter i then
            add_filtered_item i
          else
            add_hidden_item i
      )  old_items;
      
      Intmap.iter (fun _ i -> 
          if filter i then
            add_filtered_item i
          else
            add_hidden_item i
      )  old_map;
      self#sort;
      selection <- [];
      for i = 0 to nitems - 1 do
        self#insert items.(i);
      done;
      GToolbox.autosize_clist wlist;
      wlist#thaw ()
    
    method reset_data data =
      let filter = self#filter in
      let add_filtered_item = self#add_filtered_item in
      let add_hidden_item = self#add_hidden_item in
      wlist#freeze ();
      wlist#clear ();
      nitems <- 0;
      filtered <- Intmap.empty;
      nfiltered <- 0;
      items <- [||];
      contents <- [||];
      List.iter (fun i -> 
          if filter i then
            add_filtered_item i
          else
            add_hidden_item i
      )  data;
      self#sort;
      selection <- [];
      for i = 0 to nitems - 1 do
        self#insert items.(i);
      done;
      GToolbox.autosize_clist wlist;
      wlist#thaw ()
    
    method virtual filter : 'a -> bool
    
    method refresh_item old_pos i = 
      let old_filtered = old_pos < 0 in
      let new_filtered = self#filter i in
      match old_filtered, new_filtered with
        true, false ->
          filtered <- Intmap.remove (get_key i) filtered;
          nfiltered <- nfiltered - 1;
          let pos = nitems in
          self#add_hidden_item i;
          self#insert ~row: pos i;
      | false, true ->          
          Array.blit items (old_pos+1) items old_pos (nitems - old_pos - 1);
          Array.blit contents (old_pos+1) contents old_pos (nitems - old_pos - 1);
          nitems <- nitems - 1;
          self#wlist#remove old_pos;
          self#add_filtered_item i          
      | false, _  -> 
          self#update_row i old_pos
      | _ -> ()
          
    method remove_item pos i = 
      if pos < 0 then begin
          filtered <- Intmap.remove (get_key i) filtered;
          nfiltered <- nfiltered - 1
      end else begin
          Array.blit items (pos+1) items pos (nitems - pos - 1);
          self#wlist#remove pos;
          nitems <- nitems - 1
        end
    
    method iter (f : 'a -> unit) =
      for i = 0 to nitems - 1 do
        f items.(i)
      done

end

class virtual ['a] plist
    sel_mode titles titles_show get_key =
  object
    inherit ['a] filtered_plist sel_mode titles titles_show get_key
    method filter i = false
end
