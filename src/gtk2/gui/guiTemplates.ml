(* Copyright 2004 b8_bavard, INRIA *)
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


(* **************************************************************** *)
(* TODO :                                                           *)
(*    Tooltips                                                      *)
(* **************************************************************** *)

module M = GuiMessages
module O = GuiOptions

let (!!) = Options.(!!)
let (=:=) = Options.(=:=)

let verbose_view = O.gtk_verbose_gview

let verbose_chat = O.gtk_verbose_chat

(* a module that will manage alone a lot of things. Instead of 
 * gpattern for gtk1, this interface is strongly linked to "foreign" data
 * as GuiOptions, GuiMessages, GuiColumns and Options. The aim is to
 * provide a homogeneous interface.
 * The architecture is sumarized as follows :

   module Gview

     + Functors (Parameters to provide)
     |
     |-- + module Column       : see GuiColumns.
     |
     |-- + type item           : the type of the items from which GTree.view will display data.
     |
     |-- + type key            : the type to which an item will be binded.
     |
     |-- + columns             : defines the layout of the GTree.view. See GuiOptions.
     |                           Being an Options.option_record, the use of this value is
     |                           completely "interactive".
     |
     |-- + get_key             : a function to store and retrieve items.


     + Signatures (What you can get to use from the outside)
     |
     |--+ class type g_model   : a common interface for the class treeview.
     |                           inherit from the class GTree.model. That is logic since
     |                           a GTree.view requires a GTree.model to query certain
     |                           characteristics of a data store.
     |
     |--+ class type g_view    : a common interface for the classes g_list and g_tree.
     |                           Why ? Certain operations (sorting, removal, etc...)
     |                           on a "store" may take tremendeous time when a GTree.view
     |                           is connected to it. When we add, remove, store, ... the 
     |                           GTree.model interface of the "store" communicate those
     |                           information to the GTree.view to display/render the changes.
     |                           To improve the speed, we disconnect the g_list / g_tree
     |                           from the #g_view (a treeview for instance). Once these
     |                           operations are over, the g_list / g_tree is reconnected to the GTree.view.
     |
     |--+ class virtual g_list : implements a GTree.list_store and a g_model interface to
     |                           easily link it to a #g_view (a treeview for instance).
     |                           It has been optimized for random access and heavy operations.
     |                           a g_list can have several #g_view connected to it.
     |                           inherit from the class GTree.model_filter
     |
     |--+ class virtual g_tree : implements a GTree.tree_store and a g_model interface to
     |                           easily link it to a #g_view (a treeview for instance).
     |                           It has been optimized for random access and heavy operations.
     |                           a g_tree can have several #g_view connected to it.
     |                           inherit from the class GTree.model_filter
     |
     |--+ class treeview       : implements a GTree.view and a g_view interface to
     |                           easily link it to a #g_model (a g_list or a g_tree for instance).
     |                           a treeview can have only one #g_model connected to it.
     |                           inherit from the class GPack.box
     |
     |
     |--+ treeview             : a convenient fonction to create an instance of the class
     |                           treeview.


 * to build an object from the virtual classes g_list or g_tree,
 * 1 GTree.column_list and 4 methods shall be provided :

     + GTree.column_list : How the data are organized in the g_list/g_tree.
                           Consider a "column" as a simple record field.
     + from_item         : map an item to data as defined by the GTree.column_list.
     + from_new_item     : convenient method to update the data of a row. to avoid
                           to update all the row, can reduce CPU usage. Can be equal to
                           from_item (in this case, all the row is updated!).
     + content           : How the data of the g_list/g_tree will be rendered in a treview.
     + sort_items        : How the data will be sorted as requested by the treeview.

 * Note that to speed up random access to the data stored in a g_list/g_tree,
 * one uses a Hashtbl of Gtk.tree_iter. Since the g_list and g_tree implement a 
 * a GTree.list_store (respectively a GTree.tree_store) there is no particular issue (both
 * have persistent Gtk.tree_iters). That would not have been the case if we were working with a
 * GTree.model_sort or GTree.model_filter instead.
 *)


type key = string

module Gview(V:

(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(*                                                                       *)
(*                         FUNCTOR Argument                              *)
(*                                                                       *)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)

    sig

      module Column : sig

          type column

          val column_strings   : (column * string ref * int) list
          val string_of_column : column -> string
          val int_of_column    : column -> int

        end

      open Column

      type item
      (* type key *)

      val columns              : (column * float) list Options.option_record
      val get_key              : item -> key
      val module_name          : string

    end) = 
  (struct

  open V
  open Column


let lprintf_g_list fmt =
  Printf2.lprintf ("%s g_list: " ^^ fmt) module_name 

let lprintf_g_tree fmt =
  Printf2.lprintf ("%s g_tree: " ^^ fmt) module_name

let lprintf_g_view fmt =
  Printf2.lprintf ("%s g_view: " ^^ fmt) module_name

class type g_model =
  object
    inherit GTree.model
    method content : GTree.view_column -> column -> unit
    method expanded_paths : int array list
    method find_model_key : Gtk.tree_iter -> key
    method set_view : g_view -> unit
    method sort : column -> Gtk.Tags.sort_type option -> unit
    method unset_view : g_view -> unit
  end

and g_view =
object
    method expanded_paths : int array list
    method id : int
    method set_model : g_model -> unit
    method unset_model : unit -> unit
  end

type path_tree =
  {
   path_tree_parent  : int array;      (* for compare_array and partition in g_tree *)
   path_tree_level   : int;            (* should speed up sort / avoid rebuilding an int array *)
   path_tree_iter    : Gtk.tree_iter;  (* a pointer to a row *)
  }

type item_tree =
  {
    mutable item_tree_path    : path_tree option;
    mutable item_tree_removed : bool;  (* to manage easily the stamp *)
  }

type delayed_item =
  {
   delayed_item_item     : item;
   delayed_item_computed : (unit -> unit) option;
  }

let parent_from_indices indices = Array.sub indices 0 (Array.length indices - 1)
let level_from_indices indices = Array.sub indices (Array.length indices - 1) 1

let path_from_indices indices =
  let p = ref "" in
  Array.iter (fun i -> p := !p ^ (Printf.sprintf "%d:" i)) indices;
  p := String.sub !p 0 (String.length !p - 1);
  GTree.Path.from_string !p

let rec partition list res =
  match list with
      [] -> res
    | _ ->
        begin
          let it_p = snd (List.hd list) in
          let (l', l'') =
            List.partition (fun (k, it) ->
              match it.item_tree_path, it_p.item_tree_path with
                None, None -> true
              | Some p, Some _p -> p.path_tree_parent = _p.path_tree_parent
              | _ -> false
            ) list
          in
          partition l'' (l' :: res)
        end

let compare_array a1 a2 =
  let len1 = Array.length a1 in
  let len2 = Array.length a2 in
  let len = min len1 len2 in
  let rec iter n =
    if n = len
      (* a1 a2 have a common node in the tree!
       * put the one with a lower height in the tree first.
       *)
      then compare len1 len2
      else if a1.(n) > a2.(n)
        then 1
        else if a1.(n) < a2.(n)
          then (-1)
          else iter (n + 1)
  in
  iter 0

class virtual g_list (cols : GTree.column_list) =
(* one uses a Hashtbl because, the GTree.model is very bad for random access. *)
  let (table : (key, item_tree) Hashtbl.t) = Hashtbl.create 217 in
  let key_col = cols#add Gobject.Data.string in
(*
 * A bool GTree.column. If the value is true the row will be displayed.
 * As g_list inherit from GTree.model_filter, it is initialized once the object has
 * been built (refer to [self#set_visible_column filter_col]).
 *)
  let filter_col = cols#add Gobject.Data.boolean in
(*
 * It is logic to introduce a GTree.list_store as it is used for simple lists
 * of data where they have no parent-child relationships. Note that no
 * GTree.model_sort is implemented, despite the fact heavy use of sorting functions
 * is made.  The interface class GTree.tree_sortable is used directly 
 * inherited from Gtree.list_store.
 *)
  let store = GTree.list_store cols in
(*
 * Don't touch here because of the cooking done by lablgtk. It is important to
 * refer to the Hashtbl GTree.model_ids.
 *)
  let child_model = store#as_model in
  let child_oid = Gobject.get_oid child_model in
  let obj = GtkTree.TreeModelFilter.create ~child_model [] in
  let _ =
    try 
      let child_id = Hashtbl.find GTree.model_ids child_oid in
      Hashtbl.add GTree.model_ids (Gobject.get_oid obj) child_id
    with Not_found -> ()
  in
  object (self)
(*
 * g_list inherits from the class GTree.model_filter.
 * As a first consequence, inherits from the class GTree.model.
 *)
  inherit GTree.model_filter obj

(* for public use *)
    val store = store
(* allows to change, whenever necessary, the filter function of our g_list *)
    val mutable filter_func = (fun (key : key)  -> true)
(* the list of #g_view that currently use the g_list *)
    val mutable gviews = ([] : g_view list)

    val queue_add = Queue.create ()
    val queue_remove = Queue.create ()

(* the number of rows. Includes filtered rows. *)
    val mutable nitems = 0

(* map an item to the data stored in our g_list as defined by cols *)
    method virtual from_item : Gtk.tree_iter -> item -> unit
(*
 * to update a row with a more fine-grained control.
 * It gives maximum control over what exactly is going to be updated and thus control the CPU usage.
 *)
    method virtual from_new_item : Gtk.tree_iter -> item -> item -> unit
(*
 * defines how the data stored in the g_list will be displayed/rendered in a #g_view
 * (a treeview for instance).
 *)
    method virtual content : GTree.view_column -> column -> unit
(* defines how the data will be sorted on the #g_view request. *)
    method virtual sort_items : column -> key -> key -> int


    method private add_item_while_idle () =
      ignore (Glib.Idle.add (fun _ ->
        try
          let qi = Queue.take queue_add in
          let i = qi.delayed_item_item in
          let key = get_key i in
          let it = Hashtbl.find table key in
          if not it.item_tree_removed
            then begin
              let row = store#append () in
              store#set ~row ~column:filter_col (filter_func key);
              store#set ~row ~column:key_col key;
              let indices = GTree.Path.get_indices (store#get_path row) in
              let level = level_from_indices indices in
              let path = {
                path_tree_parent = [||];
                path_tree_level = level.(0);
                path_tree_iter = row;
              } in
              it.item_tree_path <- Some path;
              nitems <- nitems + 1;
              self#from_item row i;
              Gaux.may ~f:(fun x -> x ()) qi.delayed_item_computed
            end;
          true
        with Queue.Empty -> false | _ -> true))

(* the public method to add an item *)
    method add_item i ?f () =
      let is_empty = Queue.is_empty queue_add in
      let key = get_key i in
      Hashtbl.add table key {item_tree_path = None; item_tree_removed = false};
      Queue.add {delayed_item_item = i; delayed_item_computed  = f} queue_add;
      if is_empty then self#add_item_while_idle ()

(* the public method to retrieve all items stored in the g_list *)
    method all_items () =
      let l = ref [] in
      Hashtbl.iter (fun key it ->
        if not it.item_tree_removed
          then l := key :: !l
      ) table;
      !l

(* the public method to clear the g_list *)
    method clear () =
      Hashtbl.clear table;
      store#clear ();
      nitems <- 0

    method expanded_paths = []

    method private filter_removed key =
      try
        let it = Hashtbl.find table key in
        it.item_tree_removed
      with _ -> false

    method find_key row =
      try
        let key = store#get ~row ~column:key_col in
        let it = Hashtbl.find table key in
        if not it.item_tree_removed
          then key else raise Exit
      with _ -> raise Not_found

    method find_model_key row =
      try
        let store_row = self#convert_iter_to_child_iter row in
        self#find_key store_row
      with _ -> raise Not_found

(* the public method to retrieve an item by its [key] *)
    method find_row key =
      try
        let it = Hashtbl.find table key in
        if not it.item_tree_removed
          then Gaux.may_map ~f:(fun p -> p.path_tree_iter) it.item_tree_path
          else raise Exit
      with _ -> raise Not_found

(* the g_model interface to connect to a #g_view *)
    method gmodel = (self :> g_model)

(* the public number of rows in the g_list *)
    method nitems = nitems

(* the public method to refresh the items filtered *)
    method refresh_filter () =
      let gl = gviews in
      (* disconnect from all the #g_view *)
      List.iter (fun v -> v#unset_model ()) gl;
      (* proceed ... *)
      Hashtbl.iter (fun key it ->
         Gaux.may ~f:(fun p ->
           store#set ~row:p.path_tree_iter ~column:filter_col (filter_func key)) it.item_tree_path
      ) table;
      (* reconnect to all the #g_view *)
      List.iter (fun v -> v#set_model self#gmodel) gl

    method private remove_item_while_idle () =
      ignore (Glib.Idle.add (fun _ ->
        try
          let (key, it) = Queue.take queue_remove in
          Gaux.may ~f:(fun p ->
            ignore (store#remove p.path_tree_iter);
            Hashtbl.remove table key;
            ) it.item_tree_path;
          true
        with Queue.Empty -> false))

(* the public method to remove an item from the g_list *)
    method remove_item key =
      try
        let it = Hashtbl.find table key in
        if not it.item_tree_removed
          then begin
            let is_empty = Queue.is_empty queue_remove in
            it.item_tree_removed <- true;
            Gaux.may ~f:(fun p ->
              store#set ~row:p.path_tree_iter ~column:filter_col (filter_func key);
              nitems <- nitems - 1;
              ) it.item_tree_path;
            Queue.add (key, it) queue_remove;
            if is_empty then self#remove_item_while_idle ()
          end
      with _ -> ()

(* the public method to set the filter function *)
    method set_filter f =
      filter_func <- (fun key -> not (self#filter_removed key) && f key);
      self#refresh_filter ()

(* the public method to connect a #g_view. *)
    method set_view view =
      if not (List.memq view gviews)
        then begin
          gviews <- view :: gviews
        end

(* Try to do this with a GTree.model_sort ! it is so
 * fast that even the filtered items are sorted :-)
 *)
    method private sort' c n =
      let l = ref [] in
      Hashtbl.iter (fun key it ->
        if not (it.item_tree_removed || it.item_tree_path = None)
          then l := (key, it) :: !l;
      ) table;
      let l' =
        List.sort (fun (key1, it1) (key2, it2) ->
          let comp' =  self#sort_items c key1 key2 in
          if comp' = 0
            then begin
              match it1.item_tree_path, it2.item_tree_path with
                Some p1, Some p2 -> compare p1.path_tree_level p2.path_tree_level
              | _ -> comp'
            end else comp'
        ) !l
      in
      let len = max 0 (List.length !l - 1) in
      List.iter (fun (_, it) ->
        let pos = store#get_iter (path_from_indices [|len|]) in
        Gaux.may ~f:(fun p -> ignore (store#move_after ~iter:p.path_tree_iter ~pos)) it.item_tree_path
      ) (if n < 0 then l' else List.rev l')

(* the public method to sort the g_list. *)
    method sort c order_opt =
      let gl = gviews in
      (* disconnect all the #g_view *)
      List.iter (fun v -> v#unset_model ()) gl;
      (* proceed ... *)
      let _ =
        match order_opt with
          None ->
            begin
              let l = ref [] in
              Hashtbl.iter (fun _ it ->
                if not (it.item_tree_removed || it.item_tree_path = None)
                  then l := it :: !l;
              ) table;
              let l' =
                 List.sort (fun it1 it2 ->
                   match it1.item_tree_path, it2.item_tree_path with
                     Some p1, Some p2 -> (-1) * (compare p1.path_tree_level p2.path_tree_level)
                   | _ -> 0
                 ) !l
              in
              let len = max 0 (List.length !l - 1) in
              List.iter (fun it ->
                let pos = store#get_iter (path_from_indices [|len|]) in
                Gaux.may ~f:(fun p -> ignore (store#move_after ~iter:p.path_tree_iter ~pos)) it.item_tree_path
              ) l'
            end
        | Some `ASCENDING -> self#sort' c (-1)
        | Some `DESCENDING -> self#sort' c 1

      in
      (* reconnect all the #g_view *)
      List.iter (fun v -> v#set_model self#gmodel) gl

(* the public method to update an item in the g_list *)
    method update_item row i i_new =
      try
        store#set ~row ~column:filter_col (filter_func (get_key i_new));
        self#from_new_item row i i_new
      with _ -> ()


(* the public method to disconnect a #g_view. *)
    method unset_view view =
      gviews <- List.filter (fun v -> v#id <> view#id) gviews

    initializer

      self#set_visible_column filter_col;

  end


class virtual g_tree (cols : GTree.column_list) =
(* one uses a Hashtbl because, the GTree.model is very bad for random access. *)
  let (table : (key, item_tree) Hashtbl.t) = Hashtbl.create 217 in
  let key_col = cols#add Gobject.Data.string in
(*
 * A bool GTree.column. If the value is true the row will be displayed.
 * As g_tree inherits from GTree.model_filter, it is initialized once the object has
 * been built (refer to [self#set_visible_column filter_col]).
 *)
  let filter_col = cols#add Gobject.Data.boolean in
(*
 * It is logic to introduce a GTree.tree_store as it is used for data
 * where they have parent-child relationships. Note that no
 * GTree.model_sort is implemented, despite heavy use of sorting functions
 * is made. The interface class GTree.tree_sortable is directly used
 * inherited from Gtree.tree_store.
 *)
  let store = GTree.tree_store cols in
(*
 * Don't touch here because of the cooking done by lablgtk. It is important to
 * refer to the Hashtbl GTree.model_ids.
 *)
  let child_model = store#as_model in
  let child_oid = Gobject.get_oid child_model in
  let obj = GtkTree.TreeModelFilter.create ~child_model [] in
  let _ =
    try 
      let child_id = Hashtbl.find GTree.model_ids child_oid in
      Hashtbl.add GTree.model_ids (Gobject.get_oid obj) child_id
    with Not_found -> ()
  in
  object (self)
(*
 * g_tree inherits from the class GTree.model_filter.
 * As a first consequence, inherits from the class GTree.model.
 *)
  inherit GTree.model_filter obj

(* for public use *)
    val store = store

(* allows to change, whenever necessary, the filter function of the g_tree *)
    val mutable filter_func = (fun (key : key) -> true)

(* the list of #g_view that currently use the g_tree *)
    val mutable gviews = ([] : g_view list)

(*
 * Despite it was well explained, the implementation of the connection/disconnection
 * of a g_tree from a #g_view is not that easy - to be frank I didn't find ;-).
 * firstly it is only done if the operation processing is heavy. One of the criteria
 * is the quantity of data to treat. For this "a stamp" is used. If the length
 * of the stamp exceeds a certain limit within a certain time [timerID], the
 * the connection/disconnection scheme proceeds.
 *)
    val queue_remove = Queue.create ()

(* the number of rows. Includes filtered rows. *)
    val mutable nitems = 0
    val mutable expanded_rows = ([] : Gtk.tree_iter list)

(* map an item to the data stored in the g_tree as defined by [cols] *)
    method virtual from_item : Gtk.tree_iter -> item -> unit

(*
 * to update a row with a more fine-grained control.
 * It gives maximum control over what exactly is going to be updated and thus control the CPU usage.
 *)
    method virtual from_new_item : Gtk.tree_iter -> item -> item -> unit
(*
 * defines how the data stored in the g_list will be displayed/rendered in a #g_view
 * (a treeview for instance).
 *)
    method virtual content : GTree.view_column -> column -> unit
(* defines how the data will be sorted on the #g_view request. *)
    method virtual sort_items : column -> key -> key -> int

(* the public method to add an item *)
    method add_item i ?(parent : Gtk.tree_iter option) () =
      let key = get_key i in
      let row = store#append ?parent () in
      store#set ~row ~column:filter_col (filter_func key);
      store#set ~row ~column:key_col key;
      let indices = GTree.Path.get_indices (store#get_path row) in
      let level = level_from_indices indices in
      let parent = parent_from_indices indices in
      let path = {
        path_tree_parent = parent;
        path_tree_level = level.(0);
        path_tree_iter = row;
      } in
      Hashtbl.add table key {item_tree_path = Some path; item_tree_removed = false};
      nitems <- nitems + 1;
      self#from_item row i;

(* the public method to retrieve all items stored in the g_tree *)
    method all_items () =
      let l = ref [] in
      Hashtbl.iter (fun key it ->
        if not it.item_tree_removed
          then l := key :: !l;
      )table;
      !l

(* the public method to clear the g_tree *)
    method clear () =
      Hashtbl.clear table;
      store#clear ();
      nitems <- 0

    method expanded_paths =
      let l = ref [] in
      let l' = ref [] in
      List.iter (fun iter ->
        if store#iter_is_valid iter
          then begin
            l' := iter :: !l';
            (* convert the iter as seen by the store in a GTree.view iter *)
            let path = store#get_path iter in
            let self_path = self#convert_child_path_to_path path in
            l := (GTree.Path.get_indices self_path) :: !l
          end
      ) expanded_rows;
      expanded_rows <- !l';
      !l

    method private filter_removed key =
      try
        let it = Hashtbl.find table key in
        it.item_tree_removed
      with _ -> false

    method find_key row =
      try
        let key = store#get ~row ~column:key_col in
        let it = Hashtbl.find table key in
        if not it.item_tree_removed
          then key else raise Exit
      with _ -> raise Not_found

    method find_model_key row =
      try
        let store_row = self#convert_iter_to_child_iter row in
        self#find_key store_row
      with _ -> raise Not_found

(* the public method to retrieve an item by its [key] *)
    method find_row key =
      try
        let it = Hashtbl.find table key in
        if not it.item_tree_removed
          then begin
            match it.item_tree_path with
                None -> raise Not_found
              | Some p -> p.path_tree_iter
          end else raise Not_found
      with _ -> raise Not_found


(* the g_model interface to connect to a #g_view *)
    method gmodel = (self :> g_model)

(* the public number of rows in the g_tree *)
    method nitems = nitems

(* the public method to refresh the items filtered *)
    method refresh_filter () =
      let gl = gviews in
      (* disconnect from all the #g_view *)
      List.iter (fun v -> v#unset_model ()) gl;
      (* proceed ... *)
      Hashtbl.iter (fun key it ->
         Gaux.may ~f:(fun p ->
           store#set ~row:p.path_tree_iter ~column:filter_col (filter_func key)) it.item_tree_path
      ) table;
      (* reconnect to all the #g_view *)
      List.iter (fun v -> v#set_model self#gmodel) gl

    method private remove_item_while_idle () =
      ignore (Glib.Idle.add (fun _ ->
        try
          let it = Queue.take queue_remove in
          Gaux.may ~f:(fun p ->
            ignore (store#remove p.path_tree_iter)) it.item_tree_path;
          true
        with Queue.Empty -> false))

    method private _remove_item p key =
      let path_str = GTree.Path.to_string (store#get_path p.path_tree_iter) in
      if store#iter_has_child p.path_tree_iter
        then begin
          let len = store#iter_n_children (Some p.path_tree_iter) in
          for i = 0 to (len - 1) do
            try
              let child_path = GTree.Path.from_string (Printf.sprintf "%s:%d" path_str i) in
              let child_row = store#get_iter child_path in
              let k = store#get ~row:child_row ~column:key_col in
              self#remove_item k;
            with _ -> (if !!verbose_view then lprintf_g_tree "failed to find child\n");
          done
        end;
      store#set ~row:p.path_tree_iter ~column:filter_col (filter_func key);
      nitems <- nitems - 1

(* the public method to remove an item from the g_tree.
 * removes the children when a parent is removed.
 *)
    method remove_item key =
      try
        let it = Hashtbl.find table key in
        if not it.item_tree_removed
          then begin
            let is_empty = Queue.is_empty queue_remove in
            it.item_tree_removed <- true;
            Hashtbl.remove table key;
            Gaux.may ~f:(fun p -> self#_remove_item p key) it.item_tree_path;
            Queue.add it queue_remove;
            if is_empty then self#remove_item_while_idle ()
          end
      with _ -> (if !!verbose_view then lprintf_g_tree "in remove_item failed to find item\n")

(* the public method to set the filter function *)
    method set_filter f =
      filter_func <- (fun key -> not (self#filter_removed key) && f key);
      self#refresh_filter ()

(* the public method to connect a #g_view. *)
    method set_view view =
      if not (List.memq view gviews)
        then begin
          gviews <- view :: gviews
        end

    method private sort' c n (l : (key * item_tree) list)=
      let pl = partition l [] in
      List.iter (fun l ->
        let l' =
          List.sort (fun (key1, it1) (key2, it2) ->
            let comp' =  self#sort_items c key1 key2 in
            if comp' = 0
              then begin
                match it1.item_tree_path, it2.item_tree_path with
                  Some p1, Some p2 -> compare_array p1.path_tree_parent p2.path_tree_parent
                | _ -> comp'
              end else comp'
          ) l
        in
        List.iter (fun (_, it) ->
          Gaux.may ~f:self#_swap_row it.item_tree_path
        ) (if n > 0 then l' else List.rev l')
      ) pl

(* the public method to sort the g_tree. *)
    method sort c order_opt =
      let gl = gviews in
      (* disconnect all the #g_view *)
      List.iter (fun v -> v#unset_model ()) gl;
      let store_paths =
        List.map (fun iter ->
          let path = store#get_path iter in
          GTree.Path.get_indices path
        ) expanded_rows
      in
      let store_paths = [||] :: store_paths in
      let l = ref [] in
      Hashtbl.iter (fun key it ->
        match it.item_tree_removed, it.item_tree_path with
            false, Some p ->
              begin
                let current_path = store#get_path p.path_tree_iter in
                let current_indices = GTree.Path.get_indices current_path in
                let current_parent = parent_from_indices current_indices in
                if List.mem current_parent store_paths
                  then l := (key, it) :: !l
              end
          | _ -> ()
      ) table;
      (* proceed ... *)
      let _ =
        match order_opt with
          None ->
            begin
              (* make sure !items are sorted parent(1)->children(1)->...->parent(n)->children(n) !!! *)
              let l' =
                List.sort (fun (_, it1) (_, it2) ->
                   match it1.item_tree_path, it2.item_tree_path with
                     Some p1, Some p2 ->
                       begin
                         let a1 = Array.append p1.path_tree_parent [|p1.path_tree_level|] in
                         let a2 = Array.append p2.path_tree_parent [|p2.path_tree_level|] in
                         (-1) * (compare_array a1 a2)
                       end

                   | _ -> 0
                ) !l
              in
              List.iter (fun (_, it) ->
                Gaux.may ~f:self#_swap_row it.item_tree_path
              ) l'
            end
        | Some `ASCENDING -> self#sort' c (-1) !l
        | Some `DESCENDING -> self#sort' c 1 !l

      in
      (* reconnect all the #g_view *)
      List.iter (fun v -> v#set_model self#gmodel) gl

    method private _swap_row p =
      let current_path = store#get_path p.path_tree_iter in
      let new_indices = GTree.Path.get_indices current_path in
      let index = Array.length new_indices - 1 in
      new_indices.(index) <- 0;
      let pos = store#get_iter (path_from_indices new_indices) in
      ignore (store#move_before ~iter:p.path_tree_iter ~pos)

(* the public method to disconnect a #g_view. *)
    method unset_view view =
      gviews <- List.filter (fun v -> v#id <> view#id) gviews;
      let paths_without_root = List.filter (fun ind -> ind <> [||]) view#expanded_paths in
      (* iters from store are persistant ! *)
      expanded_rows <-
        List.map (fun ind ->
          let self_iter = self#get_iter (path_from_indices ind) in
          (* convert the iter as seen by the GTree.view in a store iter *)
          self#convert_iter_to_child_iter self_iter
        ) paths_without_root

(* the public method to update an item in the g_tree *)
    method update_item row i i_new =
      try
        store#set ~row ~column:filter_col (filter_func (get_key i_new));
        self#from_new_item row i i_new
      with _ -> ()

    initializer

      self#set_visible_column filter_col

  end




(* the graphical interface of the g_list / g_tree "objects". *)
class treeview (obj : [> Gtk.box] Gtk.obj) =
(*
 * just to create a dummy GTree.model that is
 * used for convenience.It should remain empty.
 *)
  let cols = new GTree.column_list in
  let _col = cols#add Gobject.Data.boolean in
  let store = GTree.list_store cols in
  let view = GTree.view ~model:store () in
  object (self)
  inherit GPack.box obj

    val dummy_model = view#model
    val mutable previous_col_clicked = ([] : (column * Gtk.Tags.sort_type) list)
    val mutable expanded_paths = ([] : int array list)
    val mutable menu = (fun _ -> [])
    val mutable on_select = (fun _ -> ())
    val mutable on_double_click = (fun _ -> ())
    val mutable on_collapse = (fun _ _ -> false)
    val mutable on_expand = (fun _ _ -> false)
    val mutable on_collapsed = (fun _ _ -> ())
    val mutable on_expanded = (fun _ _ -> ())
    (* the #g_model to which the treeview is connected *)
    val mutable gmodel = (None : g_model option)

    method private add_column (c : column) (prop : float) ?(pos : int option) () =
      let width = int_of_float (prop *. float_of_int (Gdk.Screen.width ())) in
      let col = GTree.view_column () in
      (* put a label in the GTree.view_column to retrieve the GButton.button later on *)
      let label =
        GMisc.label ~markup:(string_of_column c)
          ~use_underline:true ()
      in
      col#set_widget (Some label#coerce);
      col#set_resizable true;
      col#set_clickable true;
      col#set_sizing `FIXED;
      col#set_fixed_width width;
      ignore (col#connect#clicked ~callback:
        (fun _ ->
           let p = ref 0 in
           (* remove all the indicators from all the GTree.view_column *)
           List.iter (fun _ ->
             (try
                let vc = view#get_column !p in
                vc#set_sort_indicator false
              with _ -> (if !!verbose_view then lprintf_g_view "in add_column failed to append column %d\n" !p));
             incr p
           ) !!columns;
           (* draw the indicator on the GTree.view_column we have clicked on *)
           col#set_sort_indicator true;
           (*
            * is there a g_model connected to the treeview ?
            *   if not do nothing
            *   if yes do something
            *)
           match gmodel with
               None -> ()
             | Some m ->
                   match previous_col_clicked with
                       [c', `DESCENDING] when c' = c ->
                          begin
                            col#set_sort_indicator false;
                            m#sort c None;
                            previous_col_clicked <- []
                          end
                     | [c', `ASCENDING] when c' = c ->
                         begin
                            col#set_sort_order `ASCENDING;
                            m#sort c (Some `DESCENDING);
                            previous_col_clicked <- [c, `DESCENDING]
                          end
                     | _ ->
                         begin
                            col#set_sort_order `DESCENDING;
                            m#sort c (Some `ASCENDING);
                            previous_col_clicked <- [c, `ASCENDING]
                         end
      ));
      (match gmodel with
           None -> (if !!verbose_view then lprintf_g_view "in add_column no model set\n")
         | Some m -> m#content col c);
      (match pos with
          None -> ignore (view#append_column col)
        | Some p -> ignore (view#insert_column col p));
      (*
       * shall be called after the GTree.view_column has been added to the GTree.view .
       * Otherwise you will not get any parent of the GMisc.label .
       *)
      self#connect_header col

    method private connect_header (col : GTree.view_column) =
      let button =
        let rec get_button w =
          match w with
              None -> None
            | Some p -> 
                begin
                  let name = Gobject.Type.name (Gobject.get_type p#as_widget) in
                  if name = "GtkButton"
                    (* try to cast our GObj.widget to a GButton.button *)
                    then
                      try
                        Some (new GButton.button (Gobject.try_cast p#as_widget name))
                      with _ -> get_button p#misc#parent
                    (* check the parent of our GObj.widget *)
                    else get_button p#misc#parent
               end
        in
        get_button col#widget
      in
      match button with
          None -> (if !!verbose_view then lprintf_g_view "in connect_header no button found\n")
        | Some b ->
            (* connect the right click *)
            ignore (b#event#connect#button_press ~callback:
              (fun ev ->
                GdkEvent.Button.button ev = 3 &&
                GdkEvent.get_type ev = `BUTTON_PRESS &&
               (
                GToolbox.popup_menu
                  ~button:1
                  ~time:(GdkEvent.Key.time ev)
                  ~entries:(self#common_menu col);
                 true
               )
            ));
            (* connect CTRL+F10 to popup the menu *)
            ignore (b#event#connect#key_press ~callback:
              (fun ev ->
                 GdkEvent.Key.keyval ev = GdkKeysyms._F10 &&
                 GdkEvent.Key.state ev = [`CONTROL] &&
                (
                 GToolbox.popup_menu
                   ~button:1
                   ~time:(GdkEvent.Key.time ev)
                   ~entries:(self#common_menu col);
                 true
                )
            ))

    method private connect_events () =
      ignore (new GObj.event_ops view#as_widget);
      (* connect CTRL+F10 to popup the menu *)
      ignore (view#event#connect#key_press ~callback:
        (fun ev ->
            GdkEvent.Key.keyval ev = GdkKeysyms._F10 &&
            GdkEvent.Key.state ev = [`CONTROL] &&
            (
             let (_, vc_opt) = view#get_cursor () in
             let com_menu = 
               match vc_opt with
                   Some vc ->
                     begin
                       self#common_menu vc
                     end
               | None -> []
             in
             let cont_menu = self#contextual_menu () in
             let menu =
               match (cont_menu, com_menu) with
                   ([], []) -> []
                 | (m, [])
                 | ([], m) -> m
                 | _ -> cont_menu @ [`S] @ com_menu
             in
             let _ =
               match menu with
                   [] -> ()
                 | _ ->
                   GToolbox.popup_menu
                     ~button:1
                     ~time:(GdkEvent.Key.time ev)
                     ~entries:menu
             in
             GtkSignal.stop_emit ();
             true
            )
      ));
      (* connect right click of the mouse *)
      ignore (view#event#connect#button_press ~callback:
        (fun ev ->
            GdkEvent.Button.button ev = 3 &&
            GdkEvent.get_type ev = `BUTTON_PRESS &&
            (
             let x = int_of_float (GdkEvent.Button.x ev) in
             let y = int_of_float (GdkEvent.Button.y ev) in
             let p_opt = view#get_path_at_pos x y in
             let com_menu =
               match p_opt with
                   None -> []
                 | Some (_, vc, _, _) ->
                     begin
                       self#common_menu vc
                     end
             in
             let cont_menu = self#contextual_menu () in
             let menu =
               match (cont_menu, com_menu) with
                   ([], []) -> []
                 | (m, [])
                 | ([], m) -> m
                 | _ -> cont_menu @ [`S] @ com_menu
             in
             let _ =
               match menu with
                   [] -> ()
                 | _ ->
                   GToolbox.popup_menu
                     ~button:1
                     ~time:(GdkEvent.Key.time ev)
                     ~entries:menu
             in
             GtkSignal.stop_emit ();
             true
            )
        )
      );
      (* connect the row selected *)
      ignore (view#selection#connect#changed ~callback:
        (fun _ ->
           match gmodel with
               None -> ()
             | Some m ->
                 begin
                   (* get the rows selected *)
                   let path_selected = view#selection#get_selected_rows in
                   let list = ref [] in
                   List.iter (fun path ->
                     try
                       (* convert the Gtk.tree_path to a GTk.tree_iter *)
                       let row = view#model#get_iter path in
                       (*
                        * retrieve the item for the given row from the view#model
                        * that shall be the GTree.model interface of the current
                        * #g_model connected to us.
                        *)
                       let key = m#find_model_key row in
                       list := key :: !list
                     with _ -> (if !!verbose_view then lprintf_g_view "in connect_events selection_changed no item found\n")
                   ) path_selected;
                   on_select !list
                 end
      ));
      (* connect the double_clicked signal *)
      ignore (view#connect#row_activated ~callback:
        (fun path vc ->
           match gmodel with
               None -> ()
             | Some m ->
                 try
                   let row = view#model#get_iter path in
                   let key = m#find_model_key row in
                   on_double_click key
                 with _ -> (if !!verbose_view then lprintf_g_view "in connect_events row_activated no item found\n")
      ));
      (* connect the test_expand_row signal *)
      ignore (view#connect#test_expand_row ~callback:
        (fun row path ->
           match gmodel with
               None -> true
             | Some m ->
                 try
                   let key = m#find_model_key row in
                   on_expand path key
                 with _ ->
                   (if !!verbose_view then lprintf_g_view "in connect_events test_expand_row no item found\n");
                   true
      ));
      (* connect the test_collapse_row signal *)
      ignore (view#connect#test_collapse_row ~callback:
        (fun row path ->
           match gmodel with
               None -> true
             | Some m ->
                 try
                   let key = m#find_model_key row in
                   on_collapse path key
                 with _ ->
                   (if !!verbose_view then lprintf_g_view "in connect_events test_collapse_row no item found\n");
                   true
      ));
      (* connect the row_expanded signal *)
      ignore (view#connect#row_expanded ~callback:
        (fun row path ->
           match gmodel with
               None -> ()
             | Some m ->
                 try
                   let key = m#find_model_key row in
                   let indices = GTree.Path.get_indices path in
                   (if not (List.mem indices expanded_paths)
                      then expanded_paths <- indices :: expanded_paths);
                   on_expanded path key
                 with _ -> (if !!verbose_view then lprintf_g_view "in connect_events row_expanded no item found\n")
      ));
      (* connect the row_collapsed signal *)
      ignore (view#connect#row_collapsed ~callback:
        (fun row path ->
           match gmodel with
               None -> ()
             | Some m ->
                 try
                   let key = m#find_model_key row in
                   let indices = GTree.Path.get_indices path in
                   (if (List.mem indices expanded_paths)
                      then expanded_paths <- List.filter (fun a -> indices <> a) expanded_paths);
                   on_collapsed path key
                 with _ -> (if !!verbose_view then lprintf_g_view "in connect_events row_collapsed no item found\n")
      ));
      (* save automatically the column widths on a size change *)
      ignore (view#misc#connect#size_allocate ~callback:
        (fun _ ->
           let column_properties = ref [] in
           let p = ref 0 in
           List.iter (fun (c, _ ) ->
             try
               let col = view#get_column !p in
               let width = col#width in
               let prop = float_of_int width /. float_of_int (Gdk.Screen.width ()) in
               column_properties := (c, prop) :: !column_properties;
               incr p
             with _ -> (if !!verbose_view then lprintf_g_view "in connect_events size_allocate failed to find column %d\n" !p)
           ) !!columns;
           columns =:= List.rev !column_properties
      ));
      (*
       * if the Gtree.view is destroyed, notify to anybody that
       * is connected to us to be disconnected.
       *)
      ignore (view#connect#destroy ~callback:
        (fun _ ->
           (if !!verbose_view then lprintf_g_view "Destroy GuiTemplates treeview id %d\n" self#id);
           (* resort the g_model in its original state can be placed here.
            * for the momentwe keep the last sort order.
            *)
           self#unset_model ()
      ))

    method private common_menu (vc : GTree.view_column) =
      (* default width of a column = 10% of the screen width *)
      let prop = 0.1 in
      (* the position of the currently active column *)
      let pos = self#find_column_pos vc in
      (* Don't add a column that is already existing ! *)
      let possible_cols = List.filter
        (fun (c,_,_) ->
           not (List.mem_assoc c !!columns)
        ) column_strings
      in
      let rec iter p n =
        if p = n
          then true
          else begin
            let col = view#get_column p in
            let autosized = match col#sizing with `AUTOSIZE -> true | _ -> false in
            if autosized then iter (p + 1) n else false
          end
      in
      let all_autosized = iter 0 (List.length !!columns) in
      [
        `C (!M.mAutosize, (match vc#sizing with `AUTOSIZE -> true | _ -> false),
          (fun autosize ->
             try
               let (c, prop) = List.nth !!columns pos in
               let width = int_of_float (prop *. float_of_int (Gdk.Screen.width ())) in
               vc#set_resizable (not autosize);
               vc#set_fixed_width width;
               let model = match gmodel with None -> raise Exit | Some m -> m in
               view#set_model None;
               vc#clear ();
               if autosize
                 then begin
                   vc#set_sizing `AUTOSIZE (* `AUTOSIZE | `FIXED | `GROW_ONLY *)
                 end else begin
                   vc#set_sizing `FIXED
                 end;
               view#set_model (Some (model :> GTree.model));
               model#content vc c
             with _ -> ()
          )
        );
        `C (!M.mAutosize_all, all_autosized,
          (fun autosize ->
             try
               let pos = ref 0 in
               List.iter (fun (c, prop) -> (
                 try
                   let col = view#get_column !pos in
                   let width = int_of_float (prop *. float_of_int (Gdk.Screen.width ())) in
                   col#set_resizable (not autosize);
                   col#set_fixed_width width;
                 with _ -> ());
                 incr pos
               ) !!columns;
               let model = match gmodel with None -> raise Exit | Some m -> m in
               view#set_model None;
               pos := 0;
               List.iter (fun _ -> (
                 try
                   let col = view#get_column !pos in
                   col#clear ();
                   if autosize
                     then begin
                       col#set_sizing `AUTOSIZE (* `AUTOSIZE | `FIXED | `GROW_ONLY *)
                     end else begin
                       col#set_sizing `FIXED
                     end;
                 with _ -> ());
                 incr pos
               ) !!columns;
               view#set_model (Some (model :> GTree.model));
               pos := 0;
               List.iter (fun (c, _) -> (
                 try
                   let col = view#get_column !pos in
                   model#content col c
                 with _ -> ());
                 incr pos
               ) !!columns;
             with _ -> ()
          )
        );
        `I (!M.mRemove_column,
          (fun _ ->
              match !!columns with
                  _ :: _ when (List.length !!columns > 1) ->
                    begin
                      let l = !!columns in
                      match List2.cut pos l with
                          l1, _ :: l2 ->
                            begin
                              (* save the new layout *)
                              columns =:= l1 @ l2;
                              (* remove the column *)
                              ignore (view#remove_column vc)
                            end
                        | _ -> ()
                    end
                | _ -> ()
          )
        );
      ] @ (
      match possible_cols with
          [] -> []
        | _ ->
      [
        `M (!M.mAdd_column_after, (
            List.map (fun (c,s,_) ->
                (`I (!s, (fun _ ->
                          let c1, c2 = List2.cut (pos+1) !!columns in
                          (* save the new layout *)
                          columns =:= c1 @ [c, prop] @ c2;
                          (* add the column after the currently active column *)
                          ignore (self#add_column c prop ~pos:(pos+1) ())
                    )))
            ) possible_cols));
        `M (!M.mAdd_column_before, (
            List.map (fun (c,s,_) ->
                (`I (!s, (fun _ -> 
                        let c1, c2 = List2.cut pos !!columns in
                        (* save the new layout *)
                        columns =:= c1 @ [c, prop] @ c2;
                        (* add the column before the currently active column *)
                        ignore (self#add_column c prop ~pos ())
                    )))
            ) possible_cols));
      ]
      )

    method private contextual_menu () =
      match gmodel with
          None -> []
        | Some m ->
            begin
              (* get the rows selected *)
              let path_selected = view#selection#get_selected_rows in
              let list = ref [] in
              List.iter (fun path ->
                try
                  (* convert the path in Gtk.tree_iter *)
                  let row = view#model#get_iter path in
                  (*
                   * retrieve the item for the given row from the view#model
                   * that shall be the GTree.model interface of the current
                   * #g_model connected to us.
                   *)
                  let key = m#find_model_key row in
                  list := key :: !list
                with _ -> (if !!verbose_view then lprintf_g_view "in contextual_menu no item found\n")
              ) path_selected;
              menu !list
            end

    method expanded_paths =
      expanded_paths <-
       (* make sure the expanded rows are correct! *)
        List.filter (fun ind ->
          let path = path_from_indices ind in
          view#row_expanded path
        ) expanded_paths;
        expanded_paths

    method private find_column_pos (col : GTree.view_column) =
      let ncolumns = List.length !!columns in
      let rec iter pos =
        if pos = ncolumns
          then pos
          else if (view#get_column pos)#get_oid = col#get_oid
            then pos
            else iter (pos+1)
      in
      iter 0

(* public method to expose our g_view interface. Used by a #g_model *)
    method gview = (self :> g_view)

(*
 * unique for the current execution of the program to identify the object.
 *)
    method id = Oo.id self

    method private refresh_content () =
      let pos = ref 0 in
      (* for all the GTree.view_column currently packed in the GTree.view *)
      List.iter (fun (c, _) ->
        try
          let col = view#get_column !pos in
          (*
           * remove all the previous #GTree.cell_renderers packed in
           * the GTree.cell_layout of the GTree.view_column .
           *)
          col#clear ();
          let _ =
            match gmodel with
                None -> ()
              (*
               * refill the GTree.cell_layout of the GTree.view_column
               * with the #GTree.cell_renderers defined by the g_model connected to us.
               *)
              | Some m -> m#content col c;
          in
          incr pos
        with _ -> (if !!verbose_view then lprintf_g_view "in refresh_content failed to find column %d\n" !pos)
      ) !!columns

(*
 * public method to set a particular menu.
 * Can be changed whenever we want.
 *)
    method set_menu f = menu <- f

(* public method to set a #g_model *)
    method set_model model =
      (* show everybody we have a #g_model connected to us *)
      gmodel <- Some model;
      (* say to the #g_model we are connected to it *)
      model#set_view self#gview;
      (* connect the GTree.model interface of the #g_model to our GTree.view *)
      view#set_model (Some (model :> GTree.model));
      (* redefined how the data of the #g_model currently connected shall be displayed/rendered *)
      self#refresh_content ();
      view#selection#unselect_all ();
      List.iter (fun ind ->
        try
          let path = path_from_indices ind in
          view#selection#select_path path;
          List.iter (fun p -> view#expand_row p) (view#selection#get_selected_rows);
          view#selection#unselect_path path;
        with _ -> (if !!verbose_view then lprintf_g_view "failed in set_model expand row\n")
      ) model#expanded_paths

(*
 * public method to set a particular action on a row collapsing.
 * Can be changed whenever necessary.
 *)
    method set_on_collapse f = on_collapse <- f

(*
 * public method to set a particular action on a row collapsed.
 * Can be changed whenever necessary.
 *)
    method set_on_collapsed f = on_collapsed <- f

(* 
 * public method to set a particular action on a double-clicked row.
 * Can be changed whenever necessary.
 *)
    method set_on_double_click f = on_double_click <- f

(*
 * public method to set a particular action on a row expanding.
 * Can be changed whenever necessary.
 *)
    method set_on_expand f = on_expand <- f

(*
 * public method to set a particular action on a row expanded.
 * Can be changed whenever necessary.
 *)
    method set_on_expanded f = on_expanded <- f

(*
 * public method to set a particular action on a row selected.
 * Can be changed whenever necessary.
 *)
    method set_on_select f = on_select <- f

(* 
 * public method to set the selection mode of the GTree.view implemented
 * by the class treeview.
 *)
    method set_selection_mode mode = view#selection#set_mode mode

(* 
 * public method to unset the #g_model currently connected to the treeview.
 *)
    method unset_model () =
      (* show everybody we have no #g_model connected to us *)
      gmodel <- (match gmodel with None -> None | Some m -> m#unset_view self#gview; None);
      expanded_paths <- [];
      (* fall back to our dummy GTree.model *)
      view#set_model None;
      (* blank everything *)
      self#refresh_content ()

    method view = view

    initializer

      let scrolled_box =
        GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
          ~placement:`TOP_LEFT
          ~packing:(self#pack ~expand:true ~fill:true) ()
      in
      (* we add the columns to our GTree.view according to the layout defined by V.columns *)
      List.iter (fun (c, prop) ->
        self#add_column c prop ()
      ) !!columns;
      self#connect_events ();
      (* font and colors are passed once the object has been built. *)
      let font = Pango.Font.from_string !!O.gtk_font_list in
      let col = `NAME !!O.gtk_color_default in
      view#misc#modify_font font;
      view#misc#modify_text [`NORMAL, col];
      view#set_fixed_height_mode true;
      scrolled_box#add view#coerce

end


let treeview ?mode =
  GtkPack.Box.make_params [] ~cont:(
  GContainer.pack_container ~create:(fun p ->
    let b = new treeview (GtkPack.Box.create `VERTICAL p) in
    match mode with None -> b | Some m -> b#set_selection_mode m; b
  ))

end)








open GuiUtf8
module U = GuiUtf8
module A = GuiArt


(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(*                                                                       *)
(*                         CHAT                                          *)
(*                                                                       *)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)

let lprintf_chat_buf fmt =
  Printf2.lprintf ("chat_buffer: " ^^ fmt)

let lprintf_chat_view fmt =
  Printf2.lprintf ("chat_view: " ^^ fmt)

let smileys_1 =
  [
    "(ap)"    , M.icon_emoticon_airplane ;
    "(A)"     , M.icon_emoticon_angel ;
    "(a)"     , M.icon_emoticon_angel ;
    ":>"      , M.icon_emoticon_arrogant ;
    ":->"     , M.icon_emoticon_arrogant ;
    "(?)"     , M.icon_emoticon_asl ;
    ":@"      , M.icon_emoticon_bad ;
    ":-@"     , M.icon_emoticon_bad ;
    "8o|"     , M.icon_emoticon_baringteeth ;
    ":["      , M.icon_emoticon_bat ;
    ":-["     , M.icon_emoticon_bat ;
    "(B)"     , M.icon_emoticon_beer ;
    "(b)"     , M.icon_emoticon_beer ;
    "(||)"    , M.icon_emoticon_bowl ;
    "(Z)"     , M.icon_emoticon_boy ;
    "(z)"     , M.icon_emoticon_boy ;
    "(^)"     , M.icon_emoticon_cake ;
    "(@)"     , M.icon_emoticon_cat ;
    "(ci)"    , M.icon_emoticon_cigaret ;
    "(O)"     , M.icon_emoticon_clock ;
    "(o)"     , M.icon_emoticon_clock ;
    ":S"      , M.icon_emoticon_confused ;
    ":s"      , M.icon_emoticon_confused ;
    ":-S"     , M.icon_emoticon_confused ;
    ":-s"     , M.icon_emoticon_confused ;
    ":'("     , M.icon_emoticon_cry ;
    "(C)"     , M.icon_emoticon_cup ;
    "(c)"     , M.icon_emoticon_cup ;
    "(6)"     , M.icon_emoticon_devil ;
    "(&)"     , M.icon_emoticon_dog ;
    "(&amp;)" , M.icon_emoticon_dog ;
    "({)"     , M.icon_emoticon_dude_hug ;
    ":^)"     , M.icon_emoticon_dunno ;
    ":$"      , M.icon_emoticon_embarrassed ;
    ":-$"     , M.icon_emoticon_embarrassed ;
    "(E)"     , M.icon_emoticon_envelope ;
    "8-)"     , M.icon_emoticon_eyeroll ;
    "(~)"     , M.icon_emoticon_film ;
    "(X)"     , M.icon_emoticon_girl ;
    "(x)"     , M.icon_emoticon_girl ;
    "(})"     , M.icon_emoticon_girl_hug ;
    "(ip)"    , M.icon_emoticon_ip ;
    "(K)"     , M.icon_emoticon_kiss ;
    "(k)"     , M.icon_emoticon_kiss ;
    "(li)"    , M.icon_emoticon_lightning ;
    "(L)"     , M.icon_emoticon_love ;
    "(l)"     , M.icon_emoticon_love ;
    ":)"      , M.icon_emoticon_megasmile ;
    ":-)"     , M.icon_emoticon_megasmile ;
    "(S)"     , M.icon_emoticon_moon ;
    "8-|"     , M.icon_emoticon_nerd ;
    ":O"      , M.icon_emoticon_omg ;
    ":o"      , M.icon_emoticon_omg ;
    ":-O"     , M.icon_emoticon_omg ;
    ":-o"     , M.icon_emoticon_omg ;
    "<:o)"    , M.icon_emoticon_party ;
    "&lt;:o)" , M.icon_emoticon_party ;
    "(pi)"    , M.icon_emoticon_pizza ;
    "(pl)"    , M.icon_emoticon_plate ;
    "(G)"     , M.icon_emoticon_present ;
    "(g)"     , M.icon_emoticon_present ;
    "(R)"     , M.icon_emoticon_rainbow ;
    "(r)"     , M.icon_emoticon_rainbow ;
  ]

(* Take me 1 hour to find the "out of bond" !!!!
   Why the regexp cannot deal with the complete list ?
*)

let smileys_2 =
  [
    ":("      , M.icon_emoticon_sad ;
    ":-("     , M.icon_emoticon_sad ;
    ":<"      , M.icon_emoticon_sad ;
    ":-<"     , M.icon_emoticon_sad ;
    ":&lt;"   , M.icon_emoticon_sad ;
    ":-&lt;"  , M.icon_emoticon_sad ;
    "^o)"     , M.icon_emoticon_sarcastic ;
    ":-*"     , M.icon_emoticon_secret ;
    "(H)"     , M.icon_emoticon_shade ;
    "(h)"     , M.icon_emoticon_shade ;
    "+o("     , M.icon_emoticon_sick ;
    "|-)"     , M.icon_emoticon_sleepy ;
    ":-#"     , M.icon_emoticon_sshh ;
    "(st)"    , M.icon_emoticon_storm ;
    "(#)"     , M.icon_emoticon_sun ;
    ":D"      , M.icon_emoticon_teeth ;
    ":d"      , M.icon_emoticon_teeth ;
    ":-D"     , M.icon_emoticon_teeth ;
    ":-d"     , M.icon_emoticon_teeth ;
    ":&gt;"   , M.icon_emoticon_teeth ;
    ":-&gt;"  , M.icon_emoticon_teeth ;
    "(N)"     , M.icon_emoticon_thumbs_down ;
    "(n)"     , M.icon_emoticon_thumbs_down ;
    "(Y)"     , M.icon_emoticon_thumbs_up ;
    "(y)"     , M.icon_emoticon_thumbs_up ;
    ":P"      , M.icon_emoticon_tongue ;
    ":p"      , M.icon_emoticon_tongue ;
    ":-P"     , M.icon_emoticon_tongue ;
    ":-p"     , M.icon_emoticon_tongue ;
    ":|"      , M.icon_emoticon_ugly ;
    ":-|"     , M.icon_emoticon_ugly ;
    "(U)"     , M.icon_emoticon_ulove ;
    "(u)"     , M.icon_emoticon_ulove ;
    ";)"      , M.icon_emoticon_wink ;
    ";-)"     , M.icon_emoticon_wink ;
  ]


let global_reg = Str.regexp "\\]\\|\\$\\|\\^\\|\\.\\|\\*\\|\\+\\|\\?\\|\\["

let reg_of_list list =
  let sr = ref "" in
  List.iter (fun (s, _) ->
    let l = Str.full_split global_reg s in
    let s =
      let s' = ref "" in
      List.iter (fun t ->
        match t with
            Str.Delim x -> s' := Printf.sprintf "%s\\%s" !s' x
          | Str.Text  x -> s' := Printf.sprintf "%s%s" !s' x
      ) l;
      !s'
    in
    if !sr = ""
      then sr := Printf.sprintf "%s" s
      else sr := Printf.sprintf "%s\\|%s" !sr s
  ) list;
  Str.regexp !sr

let str_reg_1 =
  reg_of_list smileys_1

let str_reg_2 =
  reg_of_list smileys_2

class chat_buffer_priv (obj : Gtk.text_buffer) =
  object (self)
    inherit GText.buffer obj

    val mutable nicknames = ([] : string list)
    val mutable enc_list = ([] : codeset list)
    val mutable maxlines = 500
    val mutable on_entry_return = (fun (s : string) -> ())
    val mutable use_smileys = false

    method enc_list = enc_list
    method maxlines = maxlines
    method set_maxlines n = maxlines <- n
    method set_enc_list (list : codeset list) =
      enc_list <- list
    method on_entry_return = on_entry_return
    method set_on_entry_return f =
      on_entry_return <- f
    method smileys = use_smileys
    method set_smileys b =
      use_smileys <- b

    method private convert_string s =
      if enc_list = []
        then U.utf8_of s
        else U.private_utf8_of s enc_list

    method private get_tag_name s =
      let name = "foreground" ^ s in
      if List.mem s nicknames
        then name
        else begin
          let col = GuiTools.color_of_name (self#convert_string s) in
          ignore (self#create_tag ~name [`FOREGROUND col]);
          nicknames <- s :: nicknames;
          name
        end

    method insert_text text name ?(priv=false) () =
      let iter = self#get_iter `END in
      let from_name = self#convert_string (name ^ (if priv then " (private): " else ": ")) in
      let foreground = self#get_tag_name name in
      self#insert ~iter ~tag_names:[foreground] from_name;
      if use_smileys
        then begin
          let sr_list = Str.full_split str_reg_1 text in
          let sr_list = List.flatten (
            List.map (fun sr ->
              match sr with
                  Str.Text s -> Str.full_split str_reg_2 s
                | _ -> [sr]
            ) sr_list)
          in
          List.iter (fun sr ->
            match sr with
                Str.Delim s ->
                  begin
                    let icon =
                      try
                        List.assoc s smileys_1
                      with _ ->
                        try
                          List.assoc s smileys_2
                        with _ -> M.icon_mime_unknown
                    in
                    let pixbuf = A.get_icon ~icon ~size:A.SMALL () in
                    self#insert_pixbuf ~iter ~pixbuf
                  end
              | Str.Text s ->
                  begin
                    let s' = self#convert_string s in
                    self#insert ~iter s'
                  end
          ) sr_list;
          self#insert ~iter "\n"
        end else begin
          let s = (self#convert_string text) ^ "\n" in
          self#insert ~iter s
        end

    method clear () =
      self#delete
        ~start:self#start_iter
        ~stop:self#end_iter;
      let tags_table = new GText.tag_table self#tag_table in
      List.iter (fun s ->
        let text_tag_opt = tags_table#lookup ("foreground" ^ s) in
        match text_tag_opt with
            None ->  (if !!verbose_chat then lprintf_chat_buf "in clear failed to find text_tag %s\n" ("foreground" ^ s))
          | Some text_tag ->
              begin
                tags_table#remove text_tag;
                (if !!verbose_chat then
                   lprintf_chat_buf "text_tag %s removed\n" ("foreground" ^ s))
              end
      ) nicknames;
      nicknames <- [];
      enc_list <- []

    initializer

      ignore (self#connect#changed ~callback:
        (fun _ ->
           let nlines = self#line_count in
           (if !!verbose_chat then lprintf_chat_buf "in buffer changed %d lines\n" nlines);
           if nlines > maxlines
             then begin
               let stop = self#get_iter `END in
               let offset = maxlines / 10 in
               let lines = (nlines - maxlines) / offset in
               (if !!verbose_chat then lprintf_chat_buf "lines to remove %d\n" (lines * offset));
               let stop = stop#backward_lines (nlines - (lines * offset)) in
               self#delete
                 ~start:self#start_iter
                 ~stop
             end
      ))

  end

class type chat_buffer =
  object
    inherit GText.buffer
    method clear : unit -> unit
    method enc_list : codeset list
    method insert_text : string -> string -> ?priv : bool -> unit -> unit
    method maxlines : int
    method on_entry_return : (string -> unit)
    method set_enc_list : codeset list -> unit
    method set_maxlines : int -> unit
    method set_on_entry_return : (string -> unit) -> unit
    method set_smileys : bool -> unit
    method smileys : bool
  end

class chat_buffer' = (chat_buffer_priv : Gtk.text_buffer -> chat_buffer)

let chat_buffer ?on_entry ?smileys ?tag_table ?text () =
  let tag_table =
    match tag_table with None -> None | Some x -> Some x#as_tag_table in
  let b = new chat_buffer' (GtkText.Buffer.create ?tag_table []) in
  (match text with None -> () | Some t -> b#set_text t);
  (match on_entry with None -> () | Some f -> b#set_on_entry_return f);
  (match smileys with None -> () | Some sm -> b#set_smileys sm);
  b

class chat_view_priv obj =
  let hbox = GPack.hbox ~homogeneous:false ~spacing:18 () in
  let hbox_menu = GPack.hbox ~homogeneous:false ~spacing:6 () in
  let text = GText.view ~editable:false ~wrap_mode:`WORD () in
  let menuitem = GMenu.menu_item ~label:!M.iM_me_character_coding () in
  let label_name = GMisc.label () in
  let entry = GEdit.entry ~editable:true ~has_frame:true () in
  let cols = new GTree.column_list in
  let str_col = cols#add Gobject.Data.string in
  let pixb_col = cols#add Gobject.Data.gobject in
  let store = GTree.list_store cols in
  let smileys_combo = GEdit.combo_box ~model:store ~wrap_width:10 () in
  object (self)
    inherit GPack.box obj

    val mutable buffer = (None : chat_buffer option)
    val mutable extended = false
    val mutable my_name = "mldonkey user"

    val dummy_buffer = text#buffer

    method extended = extended
    method buffer = buffer

    method set_my_name s =
      my_name <- s;
      label_name#set_text (U.utf8_of (Printf.sprintf "%s:" my_name))

    method set_extended b =
      (if !!verbose_chat then lprintf_chat_view "in set_extened run in extended mode %b\n" b);
      if b && not extended
        then begin
          let menubar = GMenu.menu_bar ~packing:(hbox_menu#pack ~expand:false ~fill:true) () in
          menubar#append menuitem;
          menuitem#set_submenu (self#create_menu ())
        end else begin
          List.iter (fun child -> child#misc#hide ()) hbox_menu#children
        end;
      extended <- b

    method private region_to_label region =
      match region with
        Default -> !M.iM_me_default
      | Arabic -> !M.iM_me_arabic
      | Armenian -> !M.iM_me_armenian
      | Baltic -> !M.iM_me_baltic
      | Celtic -> !M.iM_me_celtic
      | CentralEuropean -> !M.iM_me_centraleuropean
      | ChineseSimplified -> !M.iM_me_chinesesimplified
      | ChineseTraditional -> !M.iM_me_chinesetraditional
      | Cyrillic -> !M.iM_me_cyrillic
      | Georgian -> !M.iM_me_georgian
      | Greek -> !M.iM_me_greek
      | Hebrew -> !M.iM_me_hebrew
      | Japanese -> !M.iM_me_japanese
      | Korean -> !M.iM_me_korean
      | Nordic -> !M.iM_me_nordic
      | Romanian -> !M.iM_me_romanian
      | SouthEuropean -> !M.iM_me_southeuropean
      | Tajik -> !M.iM_me_tajik
      | Thai -> !M.iM_me_thai
      | Turkish -> !M.iM_me_turkish
      | Unicode -> !M.iM_me_unicode
      | Vietnamese -> !M.iM_me_vietnamese
      | WesternEuropean -> !M.iM_me_westerneuropean

    method private codeset_to_label codeset =
      U.codeset_to_string codeset

    method private create_menu () =
      let menu = GMenu.menu () in
      let group = ref None in
      let region_list = U.all_regions in
      List.iter (fun region ->
        let label = self#region_to_label region in
        let menuitem = GMenu.menu_item
          ~label ~packing:menu#append ()
        in
        let codesets = U.codeset_list_from_region region in
        let m = GMenu.menu () in
        let m_item = GMenu.radio_menu_item ?group:!group
            ~label:!M.iM_me_auto_detect ~packing:m#append ()
        in
        group := Some (m_item#group);
        ignore (m_item#connect#toggled ~callback:
          (fun _ ->
             match buffer with
                 None -> ()
               | Some buf ->
                   begin
                     buf#set_enc_list codesets;
                     if !!verbose_chat then
                       List.iter (fun enc ->
                         lprintf_chat_view "Item Activated for %s\n" (self#codeset_to_label enc)
                       ) buf#enc_list
                   end
        ));
        let _ =
          match buffer with
              None -> ()
            | Some buf ->
                if codesets = buf#enc_list
                  then begin
                    (if !!verbose_chat then lprintf_chat_view "Setting codeset %s\n" label);
                    m_item#toggled ();
                    m_item#set_active true;
                  end else begin
                    (if !!verbose_chat then lprintf_chat_view "Codeset %s is not set\n" label);
                  end;
        in
        ignore (GMenu.separator_item ~packing:m#append ());
        List.iter (fun codeset ->
          let label = self#codeset_to_label codeset in
          let menu_item = GMenu.radio_menu_item ?group:!group
            ~label ~packing:m#append ()
          in
          group := Some (menu_item#group);
          ignore (menu_item#connect#toggled ~callback:
            (fun _ ->
               match buffer with
                   None -> ()
                 | Some buf ->
                     begin
                       buf#set_enc_list [codeset];
                       if !!verbose_chat then
                         List.iter (fun enc ->
                           lprintf_chat_view "Item Activated for %s\n" (self#codeset_to_label enc)
                         ) buf#enc_list
                     end
          ));
          match buffer with
              None -> ()
            | Some buf ->
                if [codeset] = buf#enc_list
                  then begin
                    (if !!verbose_chat then lprintf_chat_view "Setting codeset %s\n" label);
                    menu_item#toggled ();
                    menu_item#set_active true;
                  end else begin
                    (if !!verbose_chat then lprintf_chat_view "Codeset %s is not set\n" label);
                  end
        ) codesets;
        menuitem#set_submenu m;
      ) region_list;
      menu

    method private return (entry : GEdit.entry) =
      let _ =
        match (entry#text, buffer) with
            ("", _) 
          | (_, None) -> ()
          | (s, Some buf) ->
             begin
               buf#on_entry_return s;
               buf#insert_text s my_name ()
             end
      in
      entry#set_text ""

    method set_buffer (chat_buf : chat_buffer) =
      buffer <- Some chat_buf;
      text#set_buffer (chat_buf :> GText.buffer);
      menuitem#remove_submenu ();
      let _ =
        if chat_buf#smileys
          then smileys_combo#misc#show ()
          else smileys_combo#misc#hide ()
      in
      if extended
        then menuitem#set_submenu (self#create_menu ())

    method clear () =
      match buffer with
          None -> ()
        | Some buf ->
            begin
              text#set_buffer dummy_buffer;
              buffer <- None;
              menuitem#remove_submenu ();
              if extended
                then menuitem#set_submenu (self#create_menu ())
            end

    initializer

      self#set_spacing 6;
      hbox#pack ~expand:false ~fill:true hbox_menu#coerce;
      hbox#pack ~expand:false ~fill:true smileys_combo#coerce;
      smileys_combo#misc#hide ();
      let list =
        let l = ref [] in
        List.iter (fun (t, icon) ->
          if not (List.mem_assoc icon !l) then l := (icon, t) :: !l
        ) (smileys_1 @ smileys_2);
        List.rev !l
      in
      List.iter (fun (icon, t) ->
        let row = store#append () in
        store#set ~row ~column:str_col t;
        store#set ~row ~column:pixb_col (A.get_icon ~icon ~size:A.SMALL ());
      ) list;
      let renderer = GTree.cell_renderer_pixbuf [] in
      smileys_combo#pack renderer;
      smileys_combo#add_attribute renderer "pixbuf" pixb_col ;
      smileys_combo#set_active 1;
      ignore (smileys_combo#connect#changed ~callback:
        (fun _ ->
           match smileys_combo#active_iter with
               Some row -> 
                 begin
                   let s = smileys_combo#model#get ~row ~column:str_col in
                   let t = entry#text in
                   entry#set_text (Printf.sprintf "%s %s " t s);
                   entry#set_position (String.length entry#text)
                 end
             | _ -> ()
      ));
      self#pack ~expand:false ~fill:true hbox#coerce;
      let scrolled_box =
        GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
          ~placement:`TOP_LEFT
          ~packing:(self#pack ~expand:true ~fill:true) ()
      in
      scrolled_box#add text#coerce;
      let hbox_entry =
        GPack.hbox ~homogeneous:false ~spacing:3
          ~packing:(self#pack ~expand:false ~fill:true) ()
      in
      hbox_entry#pack ~expand:false ~fill:true label_name#coerce;
      hbox_entry#pack ~expand:true ~fill:true entry#coerce;
      let font = Pango.Font.from_string !!O.gtk_font_list in
      let col = `NAME !!O.gtk_color_default in
      text#misc#modify_font font;
      text#misc#modify_text [`NORMAL, col];
      text#set_left_margin 6;
      text#set_right_margin 6;
      ignore (text#misc#connect#size_allocate ~callback:
        (fun _ ->
           let iter = text#buffer#get_iter `END in
           ignore(text#scroll_to_iter
             ~use_align:true
             ~xalign:1.
             ~yalign:1.
             iter
           )
      ));
      ignore (entry#event#connect#key_press ~callback:
        (fun ev ->
           GdkEvent.Key.keyval ev = GdkKeysyms._Return &&
           (self#return entry;
            true
           )
      ))

  end

class type chat_view =
  object
    inherit GPack.box
    method buffer : chat_buffer option
    method clear : unit -> unit
    method extended : bool
    method set_buffer : chat_buffer -> unit
    method set_extended : bool -> unit
    method set_my_name : string -> unit
  end

class chat_view' = (chat_view_priv : ([> Gtk.box] as 'a) Gtk.obj -> chat_view)

let chat_view ?extended ?buffer ?my_name =
  GtkPack.Box.make_params [] ~cont:(
  GContainer.pack_container ~create:(fun p ->
    let b = new chat_view' (GtkPack.Box.create `VERTICAL p) in
    (match extended with None -> () | Some ext -> b#set_extended ext);
    (match buffer with None -> () | Some buf -> b#set_buffer buf);
    (match my_name with None -> () | Some s -> b#set_my_name s);
    b
  ))
