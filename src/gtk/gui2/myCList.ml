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

open Printf2
open Options
open BasicSocket
  
open Gui2_options
open GuiProto
open Gui2


let default_make_menu _ =
  (List.map 
      (fun (s,f) -> `I (s, f))
    []
  )
  
let clists_need_resize = ref []
  
let need_resize clist = 
  if not (List.memq clist !clists_need_resize) then 
    clists_need_resize := clist :: !clists_need_resize

let update_sizes timer = 
  reactivate_timer timer;
  if !!auto_resize then
    List.iter (fun clist -> 
        try clist#columns_autosize () with _ -> ()) !clists_need_resize;
  clists_need_resize := []

  
    type ('a,'b) t = {
        clist: int GList.clist;
        ncols : int;
        cols : ('b -> string) list;
	color : 'b -> string option;
        cols_array : ('b -> string) array;
        gui : gui;
        num_table : (int, ('a,'b) node) Hashtbl.t;
        table : ('a, ('a,'b) node) Hashtbl.t;
        mutable num_counter : int;        
        mutable selection : int list;
        mutable make_menu : (('a, 'b) t ->  GToolbox.menu_entry list);
        mutable selected_callback : (('a, 'b) t -> 'b -> unit);
        mutable replace_value : ('b -> 'b -> 'b);
        mutable size_callback : (int -> unit);
        mutable can_select_all : bool;
        mutable multiple_select : bool;
        mutable auto_resize : bool;
      }
    
    and ('a,'b) node = {
        num : int;
        key : 'a;
        mutable value : 'b;
        mutable desc : string list;
      }

    let set_can_select_all t =
      t.can_select_all <- true
      
    let iter t f =
      let list = ref [] in
      Hashtbl.iter (fun _ node -> list := node :: !list) t.table;
      List.iter (fun node -> f node.key node.value) !list
    
    let row t node =
      let rec find_row clist row num =
        if clist#get_row_data row = num then row 
        else find_row clist (row+1) num
      in
      find_row t.clist 0 node.num
    
    let set_selected_callback t f =
      t.selected_callback <- f
          
    let unselect_all t =
      t.clist#unselect_all ();
      t.selection <- []

    let select_all t = 
      t.clist#select_all ()

    let set_multiple_select t b =
      t.multiple_select <- b;
      if b then begin
          t.clist#set_selection_mode `EXTENDED;
        end else begin
          t.clist#set_selection_mode `MULTIPLE;
        end
        
    let set_auto_resize t b =
      t.auto_resize <- b;
      if b then need_resize t.clist
    let get_auto_resize t = 
      t.auto_resize
    let get_multiple_select t = t.multiple_select
      
    let set_context_menu t f =
      t.make_menu <- (fun t ->
          let items = f t in
          let items = 
            match t.selection with
              _ :: _ :: _ ->
                (`I ("Unselect all", (fun _ -> unselect_all t)))  :: items
            | _ -> items
          in
          let items = 
            if t.can_select_all then
              (`I ("Select all", (fun _ -> select_all t)))  :: items
            else items 
          in
          let items = 
            items @
              [
              (if get_multiple_select t then
                  (`I ("Multiple selection mode", (fun _ -> 
                          set_multiple_select t false)))
                  else
                  (`I ("Extended selection mode", (fun _ -> 
                          set_multiple_select t true))));
              (if get_auto_resize t then  
                  (`I ("No auto resize", (fun _ -> 
                          set_auto_resize t false)))
                else
                  (`I ("Auto resize", (fun _ -> 
                          set_auto_resize t true)))
              )
              ] 
          in
          items)
    
    let set_replace_value t f = 
      t.replace_value <- f
      
    let find t a = (Hashtbl.find t.table a).value
    let size t = t.clist#rows
    let select_row t ~row ~column ~event = 
      let num = t.clist#get_row_data row in
      let node = Hashtbl.find t.num_table num in
      t.selected_callback t node.value;
      t.selection <- num :: t.selection
    
    let unselect_row t ~row ~column ~event = 
      let num  = t.clist#get_row_data row in
      t.selection <- List2.remove num t.selection
    
    let get_row t key =
      Hashtbl.find t.table key
    
    let get_value t row = row.value
    let set_value t key value =
      if t.auto_resize then need_resize t.clist;
      let node = Hashtbl.find t.table key in
      node.value <- t.replace_value node.value value;
      begin
        match t.selection with
          num :: _ when node.num = num ->
            t.selected_callback t node.value
        | _ -> ()
      end;
      let rec iter i cols descs =
        match cols, descs with
          f :: cols, old_desc :: descs ->
            let desc = try f value with
                e -> 
                  lprintf "Exception %s in desc" (Printexc2.to_string e);
                  lprint_newline ();
                  old_desc in
            if desc <> old_desc then
	      (
	       let r = row t node in
               t.clist#set_cell ~text: desc r i;
	       match t.color value with
		 None -> ()
	       | Some c -> 
		   ignore (t.clist#set_row ~foreground: (`NAME c) r)
	      );
            desc :: (iter (i+1) cols descs)
        | [], [] -> 
            []
        | [], _ ->
            []
        | _ -> 
            []
      in
      node.desc <- iter 0  t.cols node.desc
      
    let selection t = List2.tail_map (fun num ->
          (Hashtbl.find t.num_table num).value
      ) t.selection
    
    let contextual_menu t ev =
      GdkEvent.Button.button ev = 3 &&
      GdkEvent.get_type ev = `BUTTON_PRESS &&
      (
        GAutoconf.popup_menu 
          ~button: 3
         ~time: 0
         ~entries: (t.make_menu t)
        ;
        true
      )


    let set_size_callback t f = t.size_callback <- f
      
    let create gui clist ?(color=(fun _ -> None)) cols  =
      let t = { 
        clist = clist; 
        ncols = List.length cols;
        cols = cols;
	color = color;
        cols_array = Array.of_list cols;
        gui  = gui;
        num_table = Hashtbl.create 127;
        table = Hashtbl.create 127;
        num_counter = 1;
        selection = [];
        make_menu = default_make_menu;
        selected_callback = (fun _ _ -> ());
        replace_value = (fun _ x -> x);
        size_callback = (fun _ -> ());
        can_select_all = false;
        multiple_select = true;
        auto_resize = true;
      }
      in
      set_context_menu t default_make_menu;
      ignore (t.clist#connect#select_row (select_row t));
      ignore (t.clist#connect#unselect_row (unselect_row t));
      ignore (t.clist#event#connect#button_press ~callback:
        (contextual_menu t));
      for i = 0 to t.ncols do
        t.clist#set_column (* ~auto_resize: true *) ~resizeable: true i;
      done;
      ignore (t.clist#connect#click_column ~callback:(fun i ->
          t.clist#set_sort ~column: i ~auto:false ~dir: `DESCENDING ();
          t.clist#sort ();
        )) ;
      clist#set_selection_mode `EXTENDED;
      t

  
    let clear t =
      t.clist#clear ();
      Hashtbl.clear t.num_table;
      Hashtbl.clear t.table;
      t.selection <- [];
      if t.auto_resize then need_resize t.clist

    let add t key value =
      if t.auto_resize then need_resize t.clist;
      let desc = List.map (fun f -> f value) t.cols in
      let row = t.clist#append desc  in
      (
       match t.color value with
	 None -> ()
       | Some c -> 
	   ignore (t.clist#set_row ~foreground: (`NAME c) (t.clist#rows -1))
      );
      t.num_counter <- t.num_counter + 1;
      let num = t.num_counter in
      let node = {
          num = num;
          key = key;
          value = value;
          desc = desc;
        } in
      t.clist#set_row_data row num;
(*      GToolbox.autosize_clist t.clist;*)
      Hashtbl.add t.num_table num node;
      Hashtbl.add t.table key node;
      t.size_callback (t.clist#rows)

    let update t key value =
      try
        set_value t key value        
      with Not_found ->
          add t key value
      
    let node t row =
      let num = t.clist#get_row_data row in
      Hashtbl.find t.num_table num

    let remove t key =
      if t.auto_resize then need_resize t.clist;
      let node = Hashtbl.find t.table key in
      Hashtbl.remove t.table key;
      Hashtbl.remove t.num_table node.num;
      t.clist#remove (row t node);
      t.size_callback (t.clist#rows)
      
