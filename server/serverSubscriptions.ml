(* Copyright 2002 b8_bavard, b8_fee_carabine, INRIA *)
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

open CommonTypes
open Md4
(*
  type query =
  QAnd of query * query
  | QOr of query * query
  | QAndNot of query * query
  | QHasWord of string
  | QHasField of string * string
  | QHasMinVal of string * int32
  | QHasMaxVal of string * int32
  | QNone (** temporary, used when no value is available ;
  must be simplified before transforming into strings *)
*)

module Subs = struct
  
  (***************** GLOBALS, TYPES and CONSTRUCTORS *****************)
  type 'a sub_tree = 
      Empty
    | Node of 'a node

  and 'a node = {
    mutable s_id : (Md4.t * int);      (* client md4 + subs id *)
    mutable validate_p : ('a -> bool);
    mutable validate_args : 'a;
    mutable action : ('a sub_tree -> unit);
    mutable level : int;
    mutable parent : 'a sub_tree;
  }

  type 'a pred = {
    mutable p_id : (Md4.t * int);      (* client md4 + subs id *)
    mutable f : (int32 -> 'a sub_tree);
  }

  type 'a query =
      And of 'a query * 'a query
    | Or of 'a query * 'a query
    | AndNot of 'a query * 'a query
    | HasWord of string
    | HasField of int * string
    | HasMinVal of int * int32
    | HasMaxVal of int * int32
        
  (* Hashtable where keys are (field, value) and values are subscription node *)
  let fields = ref (Hashtbl.create 1024)
                 
  (* Hashtable to store predicates. Keys are field and values are 
     (int -> int sub_tree) list *)
  let predicates = ref (Hashtbl.create 10)

  (* Global list of active subscriptions with lifetime associated. Its type is
  ((Md4.t * int) * float) list ref *)
  let active_subs_list = ref []

  (* Pred constructor *)
  let new_pred id f = 
    {p_id = id;
     f = f;}
    
  (* Sub_Tree constructor *)
  let new_sub_tree id validate_p validate_args action level parent = 
    Node {s_id = id;
          validate_p = validate_p;
          validate_args = validate_args;
          action = action;
          level = level;
          parent = parent}
  (***************** END GLOBALS, TYPES and CONSTRUCTORS *****************)

  (****************************** PREDICATES ******************************)
  (* Two default predicate *)
  let predicate_true x = true
  let predicate_false x = false
                            
  (* Predicate for an Or query. At least one of left sub_tree or right sub_tree
  validate *)
  let predicate_or x = (x >= 1)
                         
  (* Predicate for an And query. Both left and right sub_tree validate *)
  let predicate_and x = (x >= 2)

  (* Predicate for an AndNot query.
     Meaning :
     0 => left sub_tree validates and right sub_tree invalidates
     -1 => left sub_tree doesn't validate and right sub_tree invalidates
     1 => left sub_tree validates and right sub_tree doesn't invalidate, so it's ok
  *)
  let predicate_andnot x = (x >= 1)

  (****************************** END PREDICATES ******************************)

  (****************************** ACTIONS ******************************)
  (* A really cool action *)
  let void_action x = ()

  (* Increment parent.validate_args action *)
  let inc_action parent =
    match parent with
        Node n -> n.validate_args <- n.validate_args + 1
      | Empty -> ()
          
  (* Decrement parent.validate_args action *)
  let dec_action parent =
    match parent with
        Node n -> n.validate_args <- n.validate_args - 1
      | Empty -> ()
  (****************************** END ACTIONS ******************************)

  (****************************** UTILS ******************************)
  (* Manage unique number to identify subscription *)
  let numbers_taken = ref []
                        
  let add_subs_number n =
    if not (List.mem n !numbers_taken) then
      (numbers_taken := n :: !numbers_taken;
       true)
    else
      false
        
  let get_unique_subs_number () =
    let rec unique n = 
      if add_subs_number n then
        n
      else
        unique (n + 1)
    in
      unique 0
        
  let return_subs_number n = 
    numbers_taken := List.filter (fun x -> n != x) !numbers_taken

  (* Uniqueness of insertion in a list. May already exists somewhere *)
  let rec insert_unique e l =
    if (List.memq e l) then
      l
    else (e :: l)

  (* Stem and split a string *)
  let stem_and_split s =
    let s = String.lowercase (String.copy s) in
      for i = 0 to String.length s - 1 do
        let c = s.[i] in
          match c with
              'a'..'z' | '0' .. '9' -> ()
            | _ -> s.[i] <- ' ';
      done;
      String2.split_simplify s ' '

  (* WARNING : DUPLICATE CODE FROM serverIndexer.ml. MAYBE THIS SHOULD BE PLACE
  IN A COMMON FILE *)
  (* String field to bit *)
  let name_bit = 1
  let artist_bit = 2 (* tag "Artiste" *)
  let title_bit = 4  (* tag "Title" *)
  let album_bit = 8 (* tag "Album" *)
  let media_bit = 16 (* "type" *)
  let format_bit = 32 (* "format" *)
  let word_bit = 0xffffffff (* anything else *)

  let bit_of_field field =
    match field with
      | "filename" -> name_bit
      | "Artist" -> artist_bit
      | "Title" -> title_bit
      | "Album" -> album_bit
      | "format" -> format_bit 
      | "type" -> media_bit
      | _ -> word_bit

  (* Translate a tag list in an expanded form *)
  let rec cook_field_list l = 
    match l with
        [] -> []
      | hd :: tl ->
          (match hd.tag_value with
               String v ->
                 if ((bit_of_field hd.tag_name) = name_bit) then
                   let word_list = stem_and_split v in
                     List.append ((name_bit, v) :: (cook_field_list tl))
                       (List.map (fun w -> (word_bit, w)) word_list)
                 else
                   ((bit_of_field hd.tag_name), v) :: (cook_field_list tl)
             | _ -> cook_field_list tl)
          
  let rec cook_pred_list l =
    match l with
        [] -> []
      | hd :: tl ->
          (match hd.tag_value with
               Uint32 i
             | Fint32 i -> 
                 ((bit_of_field hd.tag_name), i) :: (cook_pred_list tl)
             | _ -> cook_pred_list tl)

  (****************************** END UTILS ******************************)

  (* Translate a query in yet another format that match my needs *)
  let rec translate_query q =
    match q with
        QAnd(l, r) -> And(translate_query l, translate_query r)
      | QOr(l, r) -> Or(translate_query l, translate_query r)
      | QAndNot(l, r) -> AndNot(translate_query l, translate_query r)
      | QHasWord s -> HasWord s
      | QHasField(field, s) -> HasField(bit_of_field field, s)
      | QHasMinVal (field, minval) -> HasMinVal(bit_of_field field, minval)
      | QHasMaxVal (field, maxval) -> HasMaxVal(bit_of_field field, maxval)
      | _ -> failwith "Subscription not implemented by server"

  (* Parse a query to a subscription sub_tree *)
  let rec query_to_subs id q parent action =
    match q with
        And(l, r) -> 
          let sub_tree = new_sub_tree id predicate_and 0 action 0 parent in
          let 
            left = query_to_subs id l sub_tree inc_action and
            right = query_to_subs id r sub_tree inc_action in
            begin
              (match (sub_tree, left, right) with
                   (Node n, Node l, Node r) ->
                     n.level <- (max l.level r.level) + 1
                 | (Node n, Empty, Node r) ->
                     n.level <- r.level + 1
                 | (Node n, Node l, Empty) ->
                     n.level <- l.level + 1
                 | (Node n, Empty, Empty) ->
                     n.level <- 1
                 | _ -> ());
              sub_tree
            end
      | Or(l, r) ->
          let sub_tree = new_sub_tree id predicate_or 0 action 0 parent in
          let 
            left = query_to_subs id l sub_tree inc_action and
            right = query_to_subs id r sub_tree inc_action in
            begin
              (match (sub_tree, left, right) with
                   (Node n, Node l, Node r) ->
                     n.level <- (max l.level r.level) + 1
                 |  (Node n, Empty, Node r) ->
                      n.level <- r.level + 1
                 | (Node n, Node l, Empty) ->
                     n.level <- l.level + 1
                 | (Node n, Empty, Empty) ->
                     n.level <- 1
                 | _ -> ());
              sub_tree
            end
      | AndNot(l, r) ->
          let sub_tree = new_sub_tree id predicate_andnot 0 action 0 parent in
          let 
            left = query_to_subs id l sub_tree inc_action and
            right = query_to_subs id r sub_tree dec_action in
            begin
              (match (sub_tree, left, right) with
                   (Node n, Node l, Node r) ->
                     n.level <- (max l.level r.level) + 1
                 | (Node n, Empty, Node r) ->
                     n.level <- r.level + 1
                 | (Node n, Node l, Empty) ->
                     n.level <- l.level + 1
                 | (Node n, Empty, Empty) ->
                     n.level <- 1
                 | _ -> ());
              sub_tree
            end
      | HasField(field, s) ->
          let sub_tree = new_sub_tree id predicate_true 0 action 0 parent in
            (try
               let entry = Hashtbl.find !fields (field, s) in
                 Hashtbl.add !fields (field, s) (sub_tree :: entry);
                 sub_tree
             with Not_found ->
               Hashtbl.add !fields (field, s) [sub_tree];
               sub_tree)
      | HasWord s -> 
          let sub_tree = new_sub_tree id predicate_true 0 action 0 parent in
            (try
               let entry = Hashtbl.find !fields (word_bit, s) in
                 Hashtbl.add !fields (word_bit, s) (sub_tree :: entry);
                 sub_tree
             with Not_found ->
               Hashtbl.add !fields (word_bit, s) [sub_tree];
               sub_tree)
      | HasMaxVal(field, value) ->
          let f = (fun v -> 
                     if (v <= value) then
                       new_sub_tree id predicate_true 0 action 0 parent
                     else
                       Empty) in
          let pred = new_pred id f in
            (try
               let entry = Hashtbl.find !predicates field in
                 Hashtbl.add !predicates field (pred :: entry);
                 Empty
             with Not_found ->
               Hashtbl.add !predicates field [pred];
               Empty)
      | HasMinVal(field, value) ->
          let f = (fun v -> 
                     if (v > value) then
                       new_sub_tree id predicate_true 0 action 0 parent
                     else
                       Empty) in
          let pred = new_pred id f in
            (try
               let entry = Hashtbl.find !predicates field in
                 Hashtbl.add !predicates field (pred :: entry);
                 Empty
             with Not_found ->
               Hashtbl.add !predicates field [pred];
               Empty)

  (* Add a new subscription *)
  let add_subscription id lifetime query action =
    (* First put it in the global list with its lifetime *)
    active_subs_list := (id, (Unix.time ()) +. lifetime) :: !active_subs_list;
    
    (* Then put the effective query in the tree structure *)
    ignore(query_to_subs id query Empty action)

  (* Remove a subscription with a complete id given. NOT REMOVED FROM THE GLOBAL
     LIST: done by remove_timeout_subs *)
  let remove_subs_id id =
    let ht = Hashtbl.create 1024 
    and ht2 = Hashtbl.create 10 in
      
      Printf.printf "Removing subs (%s, %d)"
        (Md4.to_string (fst id)) 
        (snd id);
      print_newline ();

      Hashtbl.iter (fun k field_list ->
                      Hashtbl.add ht k
                      (List.filter (fun field -> 
                                      (match field with
                                           Node n -> not (n.s_id = id)
                                         | Empty -> false)) field_list))
        !fields;
      fields := ht;
      
      Hashtbl.iter (fun k pred_list ->
                      Hashtbl.add ht2 k
                      (List.filter (fun pred -> not (pred.p_id = id))
                         pred_list))
        !predicates;
      predicates := ht2
        
  (* Remove a subscription with client md4 given. NOT REMOVED FROM THE GLOBAL
     LIST: done by remove_timeout_subs *)
  let remove_subs_md4 md4 =
    let ht = Hashtbl.create 1024 
    and ht2 = Hashtbl.create 10 in
      
      Hashtbl.iter (fun k field_list ->
                      Hashtbl.add ht k 
                      (List.filter (fun field -> 
                                      (match field with
                                           Node n -> not ((fst n.s_id) = md4)
                                         | Empty -> false)) field_list))
        !fields;
      fields := ht;
      
      Hashtbl.iter (fun k pred_list ->
                      Hashtbl.add ht2 k 
                      (List.filter (fun pred -> not ((fst pred.p_id) = md4))
                         pred_list))
        !predicates;
      predicates := ht2
        
  (* Remove timeout subscriptions *)
  let remove_timeout_subs () =
    let current_time = Unix.time () in
      active_subs_list :=
      List.filter (fun elt -> (match elt with
                                   (id, lifetime) -> 
                                     if (lifetime > current_time) then
                                       true
                                     else
                                       begin
                                         remove_subs_id id;
                                         false
                                       end))
        !active_subs_list
                                     
  (* Find matching subscription and notify to client *)
  let notify field_list pred_list =
    (* Make a flat list of leaf subscription that "talk" about fields present in
       field_list *)
    let rec make_field_subs_list fl =
      match fl with
          [] -> []
        | hd :: tl -> 
            try
              let subs = Hashtbl.find !fields hd in
                List.append (make_field_subs_list tl) subs
            with Not_found ->
              make_field_subs_list tl in

    (* Same as make_field_subs_list but for predicates *)
    let rec make_predicate_subs_list fl =
      match fl with
          [] -> []
        | hd :: tl ->
            let field = (fst hd) and value = (snd hd) in
              try
                let preds = Hashtbl.find !predicates field in
                  List.append (make_predicate_subs_list tl) 
                    (List.map (fun p -> (p.f) value) preds)
              with Not_found ->
                make_predicate_subs_list tl in
    let subs_list = List.append 
                      (make_field_subs_list field_list)
                      (make_predicate_subs_list pred_list) in
    let rec do_notify sl level =
      match sl with
          [] -> ()
        | l ->
            let parents = ref [] in
              List.iter
                (fun sub ->
                   match sub with
                       Node n -> 
                         (* If this is a current level subs *)
                         if (n.level = level) then
                           begin
                             (* If this subs validate: execute the associated
                                action *)
                             if (n.validate_p n.validate_args) then
                               n.action n.parent;
                             (* Insert my parent even if I don't validate *)
                             parents := insert_unique n.parent !parents;
                             (* Reset to zero *)
                             n.validate_args <- 0
                           end
                         else
                           (* Re-insert me for the next round *)
                           parents := insert_unique sub !parents
                     | Empty -> ()(*parents := List.filter (fun x -> x = sub) !parents*)) l;
              (* Recursive call on next level *)
              do_notify !parents (level + 1)
    in
      do_notify subs_list 0
        
end

(****************** TO TEST

  let query1 = (And (HasWord "coucou", And(HasField(1, "meuh meuh"),
  HasMaxVal(2, 100))))

  let notif1 = (fun sub_tree -> Printf.printf "notif1\n")

  Subs.query_to_subs (0, 0) query1 Subs.Empty notif1

  Subs.notify [(1, "mylene")] []

  Subs.notify [(0xffffffff, "coucou"); (1, "meuh meuh"); (5, "haha")] [(2, 50)]

  Subs.notify [(0xffffffff, "coucou"); (1, "meuh meuh"); (5, "haha")] [(2, 150)]

  Subs.notify [(2, "coucou"); (1, "meuh meuh"); (5, "haha")] [(2, 50)]

  let query2 = (Or (HasWord "boudin", AndNot(HasField(2, "coucou"), HasField(1,
  "meuh meuh"))))

  let notif2 = (fun sub_tree -> Printf.printf "notif2\n")

  Subs.query_to_subs (0, 1) query2 Subs.Empty notif2

  Subs.notify [(0xffffffff, "boudin")] []

  Subs.notify [(2, "coucou"); (1, "meuh meuh")] []

  Subs.notify [(2, "coucou"); (2, "meuh meuh")] []

  Subs.notify [(0xffffffff, "coucou"); (1, "meuh meuh"); (2, "coucou")] [(2, 50)]

******************)
