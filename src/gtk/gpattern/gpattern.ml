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

open Printf2
open GtkBase
open Gtk

type content =
| String of string 
| Pixmap of GDraw.pixmap
| Pixtext of string * GDraw.pixmap

type line = content list * GDraw.optcolor option
  
type 'a ptree =
  {
   data : 'a;
   mutable children : 'a ptree list
  }

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
  let tooltips = GData.tooltips () in
  object (self)
    val mutable items = ([||] : 'a array)
    val mutable contents = ([||] : line array)
    val mutable nitems = 0
    
    val mutable current_sort = 0
    val mutable selection = ([] : 'a list)
    val mutable wlist = wlist
    
    val mutable filtered = Intmap.empty
    val mutable nfiltered = 0
    
    val mutable columns_width = ([] : (int * int) list)

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
                  | (Pixtext (s, p)) :: q ->
                      wlist#set_cell ~text:s ~pixmap:p ~spacing:5 row n;
                      iter (n+1) q
                in
                iter 0 l;
(*                tooltips#set_tip ((wlist:int)#get_row row)#coerce ~text: "Tooltip";*)
                match col_opt with
                  None -> ()
                | Some c -> 
                    wlist#set_row ~foreground: c row;
              end
          with e ->
              lprintf "Exception %s in update_row\n" (Printexc2.to_string e);
        end else begin
          lprintf "update_row < 0\n"
        end
    
    method insert ?row d =
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
      (* self#wlist#columns_autosize (); *)
      wlist#thaw ()
    
    method clear =
      wlist#clear ();
      nitems <- 0;
      items <- [||];
      contents <- [||];
      filtered <- Intmap.empty
    
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
            (* GToolbox.autosize_clist self#wlist; *)
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
                ~button: 1
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
                  ~button: 1
                  ~time: 0
                  ~entries: (self#column_menu col);
                true
              )
          )
        );

(* connect the columns resizing to draw what could need a column width *)
      ignore (w#misc#connect#size_allocate ~callback:
        (fun rect ->
           let width = rect.width in
           if col < (wlist#columns - 1)
             then columns_width <- List.append columns_width [(col, width)]
             else begin
               columns_width <- List.append columns_width [(col, width)];
               self#has_changed_width columns_width;
               columns_width <- []
               end
           ));
      done;



    
    initializer
      self#connect_events;
      self#wlist#columns_autosize ()
    
    
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
      ]
      
    method has_changed_width l = ()

    method find key =
      let rec iter i len =
        if i = len then (-1, Intmap.find key filtered) else
        if get_key items.(i) = key then (i, items.(i)) else
          iter (i+1) len
      in
      iter 0 nitems
    
    method get_data n =
      if n > nitems then
        raise Not_found
        else items.(n)

    method get_all_items =
      let l = ref [] in
      l := Intmap.to_list filtered;
      for n = 0 to (nitems - 1) do
        l := items.(n)::!l
      done;
      !l

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
      (* freeze & clear reverted to show the user that
      something appened if it takes too long time *)
      wlist#clear ();
      wlist#freeze ();
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
      self#wlist#columns_autosize ();
      wlist#thaw ()
    
    method reset_data data =
      let filter = self#filter in
      let add_filtered_item = self#add_filtered_item in
      let add_hidden_item = self#add_hidden_item in
      (* freeze & clear reverted to show the user that
      something appened if it takes too long time *)
      wlist#clear ();
      wlist#freeze ();
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
      self#wlist#columns_autosize ();
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



class virtual ['a] filtered_ptree
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
  let tooltips = GData.tooltips () in
  object (self)
    constraint 'a = 'b ptree
    val mutable items = ([||] : 'a array)
    val mutable contents = ([||] : line array)
    val mutable nitems = 0

    val mutable current_sort = 0
    val mutable selection = ([] : 'a list)
    val mutable wlist = wlist

    val mutable filtered = Intmap.empty
    val mutable nfiltered = 0

    val mutable columns_width = ([] : (int * int) list)
    val mutable is_expanded = ([] : (int list * int ) list)

    method is_expanded = is_expanded

    method box = wscroll#coerce
    method wlist = wlist

    method virtual content : 'a -> line
    method compare = Pervasives.compare
    method selection = selection
    method on_select (d : 'a ) = ()
    method on_deselect (d : 'a ) = ()
    method on_double_click (d : 'a ) = ()

    method private sort =
      Array.fill contents 0 nitems dummy_line;
      let depth =
        let d = ref 0 in
        for i = 0 to nitems - 1 do
          let l = List.length (get_key (items.(i))) - 1 in
          if l > !d then
            d := l
        done;
        !d
      in
      let array = Array.init nitems (fun i ->
            (items.(i),i)) in
      (* we sort only items belonging to the max depth *)
      Array.sort (fun (v1,n1) (v2,n2) ->
        if (List.length (get_key (v1)) - 1) = depth &&
           (List.length (get_key (v2)) - 1) = depth 
          then
            if depth >= 1 then
              if List.nth (get_key (v1)) (depth - 1) = List.nth (get_key (v2)) (depth - 1) then
                begin
                  let cmp = self#compare v1 v2 in
                  if cmp = 0 then compare n1 n2 else cmp
                end
                else compare n1 n2
              else
                begin
                  let cmp = self#compare v1 v2 in
                  if cmp = 0 then compare n1 n2 else cmp
                end
          else compare n1 n2
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
                      wlist#set_cell ~text:s row n;
                      iter (n+1) q
                   | (Pixmap p) :: q ->
                      wlist#set_cell ~pixmap:p row n;
                      iter (n+1) q
                  | (Pixtext (s, p)) :: q ->
                      wlist#set_cell ~text:s ~pixmap:p ~spacing:5 row n;
                      iter (n+1) q
                in
                iter 0 l;
(*                tooltips#set_tip ((wlist:int)#get_row row)#coerce ~text: "Tooltip";*)
                match col_opt with
                  None -> ()
                | Some c ->
                    wlist#set_row ~foreground: c row;
            end
          with e ->
              lprintf "Exception %s in update_row\n" (Printexc2.to_string e);
        end else begin
          lprintf "update_row < 0\n"
        end

    method insert ?row d =
      let r =
        match row with
          None ->
            ignore (wlist#append []) ; wlist#rows - 1
        | Some p ->
            ignore (wlist#insert ~row: p []) ; p
      in
      self#update_row d r

   method expand i =
     if not (List.mem_assoc (get_key i) is_expanded) then
       begin
         let (row, _) = self#find (get_key i) in
         match i.children with
             [] -> true
           | l ->
               begin
                 wlist#freeze ();
                 let array_of_children = ref [||] in
                 List.iter (fun c ->
                 (* we will just not display children that
                 match the filter rules *)
                   if not (self#filter c) then
                     array_of_children := Array.append !array_of_children [|c|]
                 ) l;
                 let length = Array.length !array_of_children in
                 let new_nitems = nitems + length in
                 let new_items = Array.create new_nitems {data = i.data; children = []} in
                 Array.blit items 0 new_items 0 (row + 1);
                 Array.blit !array_of_children 0 new_items (row + 1) length;
                 Array.blit items (row + 1) new_items (row + length + 1) (nitems - row - 1);
                 let new_contents = Array.create new_nitems dummy_line in
                 Array.blit contents 0 new_contents 0 (row + 1);
                 Array.blit contents row new_contents (row + length + 1) (nitems - row - 1);
                 nitems <- new_nitems;
                 items <- new_items;
                 contents <- new_contents;
                 for r = 0 to length - 1 do
                   self#insert ~row:(row + r + 1) items.(row + r + 1)
                 done;
                 is_expanded <- ((get_key i), length)::is_expanded;
                 wlist#thaw ();
                 true
               end
       end
       else true

    method collapse i =
     if (List.mem_assoc (get_key i) is_expanded) then
       begin
         wlist#freeze ();
         let (row, _) = self#find (get_key i) in
         let length = List.assoc (get_key i) is_expanded in
         let new_nitems = nitems - length in
         let new_items = Array.create new_nitems {data = i.data; children = []} in
         Array.blit items 0 new_items 0 (row + 1);
         Array.blit items (row + length + 1) new_items (row + 1) (nitems - row - length - 1);
         let new_contents = Array.create new_nitems dummy_line in
         Array.blit contents 0 new_contents 0 (row + 1);
         Array.blit contents (row + length + 1) new_contents (row + 1) (nitems - row - length - 1);
         nitems <- new_nitems;
         items <- new_items;
         contents <- new_contents;
         for r = 0 to (length - 1) do
           self#wlist#remove (row + 1)
         done;
         is_expanded <- List.remove_assoc (get_key i) is_expanded;
         wlist#thaw ();
         true
       end
       else true

    method update =
      wlist#freeze ();
      wlist#clear ();
      self#sort;
      selection <- [];
      for i = 0 to nitems - 1 do
        self#insert items.(i);
      done;
      (* the autosize function increases tremendeously
        the time to display the items with pixmaps depending
        on the column size. So I suppressed it *)
      (* self#wlist#columns_autosize ();*)
      wlist#thaw ()

    method clear =
      wlist#clear ();
      nitems <- 0;
      items <- [||];
      contents <- [||];
      filtered <- Intmap.empty

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
            (* GToolbox.autosize_clist self#wlist; *)
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
                ~button: 1
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
                  ~button: 1
                  ~time: 0
                  ~entries: (self#column_menu col);
                true
              )
          )
        );

(* connect the columns resizing to draw what could need a column width *)
      ignore (w#misc#connect#size_allocate ~callback:
        (fun rect ->
           let width = rect.width in
           if col < (wlist#columns - 1)
             then columns_width <- List.append columns_width [(col, width)]
             else begin
               columns_width <- List.append columns_width [(col, width)];
               self#has_changed_width columns_width;
               columns_width <- []
               end
           ));
      done;

    initializer
      self#connect_events;
      self#wlist#columns_autosize ()

    
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
      ]

    method has_changed_width l = ()

    method find key =
      let rec iter i len =
        if i = len then
          if (List.length key) = 1 then
            (-1, Intmap.find (List.hd key) filtered)
            else raise Not_found
          else
            if get_key items.(i) = key then (i, items.(i)) else
            iter (i+1) len
      in
      iter 0 nitems

    method get_data n =
      if n > nitems then
        raise Not_found
        else items.(n)

    method get_all_items =
      let l = ref [] in
      l := Intmap.to_list filtered;
      for n = 0 to (nitems - 1) do
        l := items.(n)::!l
      done;
      !l

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
      if (List.length (get_key i)) = 1 then
        begin
          let key = List.hd (get_key i) in
          filtered <- Intmap.add key i filtered;
          nfiltered <- nfiltered + 1
        end
    
    method refresh_filter =
      let filter = self#filter in
      let add_filtered_item = self#add_filtered_item in
      let add_hidden_item = self#add_hidden_item in
      let old_items = Array.sub items 0 nitems in
      let old_map = filtered in
      (* freeze & clear reverted to show the user that
      something appened if it takes too long time *)
      wlist#clear ();
      wlist#freeze ();
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
        self#insert items.(i)
      done;
      self#wlist#columns_autosize ();
      wlist#thaw ()
    
    method reset_data data =
      let filter = self#filter in
      let add_filtered_item = self#add_filtered_item in
      let add_hidden_item = self#add_hidden_item in
      (* freeze & clear reverted to show the user that
      something appened if it takes too long time *)
      wlist#clear ();
      wlist#freeze ();
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
        self#insert items.(i)
      done;
      self#wlist#columns_autosize ();
      wlist#thaw ()

    method virtual filter : 'a -> bool

    method refresh_item old_pos i =
      let old_filtered = old_pos < 0 in
      let new_filtered = self#filter i in
      match old_filtered, new_filtered with
        true, false ->
          if (List.length (get_key i)) = 1 then
            begin
              let key = List.hd (get_key i) in
              filtered <- Intmap.remove key filtered;
              nfiltered <- nfiltered - 1;
              let pos = nitems in
              self#add_hidden_item i;
              self#insert ~row:pos i;
            end
      | false, true ->          
          Array.blit items (old_pos+1) items old_pos (nitems - old_pos - 1);
          Array.blit contents (old_pos+1) contents old_pos (nitems - old_pos - 1);
          nitems <- nitems - 1;
          self#wlist#remove old_pos;
          if (List.length (get_key i)) = 1 then
            self#add_filtered_item i
      | false, _  ->
          self#update_row i old_pos
      | _ -> ()

    method remove_item pos i =
      if pos < 0 then
        begin
          if (List.length (get_key i)) = 1 then
            begin
              let key = List.hd (get_key i) in
              filtered <- Intmap.remove key filtered;
              nfiltered <- nfiltered - 1
            end
        end else
        begin
          let b = if (List.mem_assoc (get_key i) is_expanded) then
                    self#collapse i
                    else false
          in
          Array.blit items (pos+1) items pos (nitems - pos - 1);
          Array.blit contents (pos+1) contents pos (nitems - pos - 1);
          self#wlist#remove pos;
          nitems <- nitems - 1
        end

    method iter (f : 'a -> unit) =
      for i = 0 to nitems - 1 do
        f items.(i)
      done

end
