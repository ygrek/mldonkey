type 'a tree_rec = {
    mutable tree_expr : 'a tree_expr;
  }

and 'a tree_expr =
  List of 'a suffix * (char * 'a tree_rec) list
| String of string * 'a suffix
| Suffix of 'a suffix
| Filtered of 'a tree_rec
  
and 'a index = {
    mutable next_doc : int;
    mutable next_word : int;  
    index_tree : 'a tree_rec;
  }

and 'a suffix = {
    suffix_num : int;
    suffix_value : string;
    mutable suffix_word : 'a word option;
    mutable suffix_words : 'a word list;
  }

and 'a word ={
    word_string : string;
    mutable word_filtered : bool;
    mutable word_docs : ('a doc * int) list; 
  }

and 'a doc = {
    doc_value : 'a; 
    mutable doc_filtered : bool;
    doc_num : int;
  }
  
(* pour chaque mot, on obtient une liste de documents.
Pour chaque document, un entier indique les champs dans lesquels
le mot apparait.
  *)
  
let nsuffixes = ref 0
let new_word s =
  incr nsuffixes;  
  { 
    suffix_num = !nsuffixes;
    suffix_value = s;
    suffix_word = None;
    suffix_words = [] ;
  }

let clear index = 
  index.next_doc <- 0;
  index.next_word <- 0;
  index.index_tree.tree_expr <- String ("", new_word  "")

let add_suffix idx s =
  let len = String.length s in
  let rec iter pos tree filtered =
    if pos = len then
      match tree.tree_expr with
      | Suffix _ -> assert false
      | List (num, _) -> num, filtered
      | String (str,num) ->
          if str = "" then num, filtered
          else
          let new_num = new_word (String.sub s 0 pos) in
          tree.tree_expr <- List (new_num, 
            [str.[0], 
              { tree_expr = String (
                  String.sub str 1 (String.length str - 1), num); }]);
          new_num, filtered
      | Filtered tree ->
          iter pos tree true
    else
    match tree.tree_expr with
    | Suffix _ -> assert false
    | String (str, num) ->
        let suffix = String.sub s pos (len - pos) in
        if str = suffix then num, filtered else
        if str = "" then
          let new_num = new_word (String.sub s 0 pos) in
          tree.tree_expr <- List (num, 
            [s.[pos], 
              { tree_expr = String (
                  String.sub s (pos+1) (String.length s -1-pos), new_num); 
              }]);
          new_num, filtered
        else 
          begin
            tree.tree_expr <- List (new_word (String.sub s 0 pos), 
              [str.[0], 
                { tree_expr = String (
                    String.sub str 1 (String.length str - 1), num); }]);
            iter pos tree filtered; 
          end
    | Filtered tree ->
        iter pos tree true
    | List (num, list) -> 
        let c = s.[pos] in
        try 
          iter (pos+1) (List.assoc c list) filtered
        with Not_found ->
            let new_num = new_word s in
            tree.tree_expr <- List (num, (c, { tree_expr = String (
                    String.sub s (pos+1) (String.length s -pos-1), new_num); 
                }) :: list);
            new_num, filtered
  in
  iter 0 idx.index_tree false
  
let add_doc w doc field filtered =
  if filtered || w.word_filtered then begin
(*      Printf.printf "FILTER ON DOC"; print_newline (); *)
      doc.doc_filtered <- true;
    end;
  w.word_docs <- (match w.word_docs with
      (d,f) :: tail when d = doc -> 
        (doc, f lor field) :: w.word_docs
    | _ -> (doc, field) :: w.word_docs)
      
let add idx s doc field =
  let next_suffix = !nsuffixes in
  let suffix, filtered = add_suffix idx s in
(*  if filtered then begin
      Printf.printf "SUFFIX %s IS FILTERED..." s; 
      print_newline (); 
    end; *)
  match suffix.suffix_word with
  | Some w -> (* the word is already inside *)
(*        Printf.printf "Word already exists"; print_newline (); *)
      add_doc w doc field filtered
  | None ->
(*      Printf.printf "New word added"; print_newline (); *)
      let w = { word_docs = [doc, field]; 
          word_string = s; 
          word_filtered = filtered;
        } in
      if filtered then begin
(*          Printf.printf "FILTER ON DOC"; print_newline (); *)
          doc.doc_filtered <- true;
        end;
      suffix.suffix_word <- Some w;
      let len = String.length s in
      let rec iter pos =
(*        Printf.printf "iter %d/%d" pos len; print_newline (); *)
        if pos < len then
          let suff = String.sub s pos (len - pos) in
          let suffix, filtered = add_suffix idx suff in
(*          Printf.printf "added suffix [%s]" suffix.suffix_value; 
          print_newline (); *)
(*          if filtered then begin
              Printf.printf "NEXT SUFFIX %s IS FILTERED..." suff; 
              print_newline ();
            end; *)
          match suffix.suffix_word with
          | Some w -> add_doc w doc field filtered
          | None -> 
              if filtered then begin
                  w.word_filtered <- true;
                  doc.doc_filtered <- true;
                end;
              (match suffix.suffix_words with
                  ww :: _ when w == ww -> ()
                | _ -> 
                    suffix.suffix_words <- w :: suffix.suffix_words
              );
              iter (pos+1)
      in
      iter 1
  
let find idx s =
  let len = String.length s in
  let rec iter pos tree =
    if pos = len then tree.tree_expr else
    match tree.tree_expr with
    | Suffix _ -> assert false
    | String (str, num) ->
        let max_len = len - pos in
        let len1 = String.length str in
        if len1 < max_len then raise Not_found;
        let str2 = String.sub s pos max_len in
        let str1 = String.sub str 0 max_len in
(*        Printf.printf "Check [%s] with [%s]" str1 str2; print_newline (); *)
        if str1 = str2 then begin
            Suffix num 
          end else
          raise Not_found
    | Filtered _ -> raise Not_found
    | List (num, list) ->
        if pos = len then Suffix num else
        let c = s.[pos] in
(*        Printf.printf "Multiplex on [%c]" c; print_newline (); *)
        iter (pos+1) (List.assoc c list)
  in
  iter 0 idx.index_tree

let rec spaces n=
  if n > 0 then begin
      print_char ' ';
      spaces (n-1)
    end
  
let print idx =
  Printf.printf "INDEX"; print_newline ();  
  let rec iter pos tree =
    match tree.tree_expr with
    | Suffix s ->
        spaces pos;
        Printf.printf "SUFFIX %d" s.suffix_num; print_newline (); 
    | String (s, num) ->
        spaces pos;
        Printf.printf "[%s]  ---> [%d]" s num.suffix_num;  
        spaces pos;
        Printf.printf "  ";
        List.iter (fun w ->
            Printf.printf "%s " w.word_string) num.suffix_words;
        print_newline ();
    | List(num, list) ->
        spaces pos;
        Printf.printf "List [%d]" num.suffix_num; print_newline ();
        List.iter (fun (c, tree) ->
            spaces (pos+2);
            Printf.printf "Char [%c]" c; print_newline ();
            iter (pos+4) tree
        ) list;
    | Filtered tree ->
        Printf.printf "FILTERED"; print_newline ();
        iter (pos+2) tree
  in
  iter 0 idx.index_tree
  
let create () =
  {
    next_doc = 0;
    index_tree = { tree_expr = String ("", new_word  "") };
    next_word = 0;
  }

let print_word_docs w =
  Printf.printf "Word [%s]" w.word_string; print_newline ();
  List.iter (fun (doc, _) ->
      Printf.printf "    %d" doc.doc_num; print_newline ()) w.word_docs;
  print_newline ()

let rec print_suffix_docs suffix = 
  List.iter (fun w ->
      print_word_docs w
  ) (match suffix.suffix_word with
      None ->  suffix.suffix_words
    | Some w -> w :: suffix.suffix_words)
  
let rec print_all_docs s =
  match s with
    Suffix suffix ->  print_suffix_docs suffix
  | String (_,s) -> print_suffix_docs s
  | List(s, list) ->
      print_suffix_docs s;
      List.iter (fun (c,t) -> print_all_docs t.tree_expr) list
  | Filtered t ->
      Printf.printf "filtered"; print_newline ();
      print_all_docs t.tree_expr
      
let find_docs idx s =
  try
(*    Printf.printf "********* FIND [%s]  *********" s; print_newline (); *)
    let s = find idx s in
    print_all_docs s
  with Not_found ->
(*      Printf.printf "%s absent" s; 
print_newline (); *)
      ()

let make_doc idx v = 
  idx.next_doc <- idx.next_doc + 1;
  { doc_value = v; doc_num = idx.next_doc; doc_filtered = false; }

(* AND entre plusieurs mots, avec un champ specifie par mot *)
  

  
type request = (string * int) list

module IntMap = Map.Make (struct 
      type t = int
      let compare d1 d2 = d1 - d2 end)

let get_fields tree fields f =
(*  Printf.printf "get_fields"; print_newline (); *)
  let map = ref IntMap.empty in 
  let rec iter tree = 
(* retourne une liste de listes triees de documents *)
    match tree with
      List (s, tree_list) ->
        iter_suffix s;
        List.iter (fun (_, tree) ->
            iter tree.tree_expr
        ) tree_list
    | String (_, s) 
    | Suffix s ->
        iter_suffix s
    | Filtered _ -> ()
  
  and iter_suffix s =
(*    Printf.printf "iter suffix"; print_newline (); *)
    List.iter (fun w ->
(*        Printf.printf "occurences of [%s]" w.word_string; print_newline ();  *)
        if not w.word_filtered then
          List.iter (fun (doc, where) ->
(*            Printf.printf "in doc %d field %d/%d" doc.doc_num where fields; 
            print_newline (); *)
              if where land fields <> 0 && 
                not doc.doc_filtered &&
                f doc && not (
                  IntMap.mem doc.doc_num !map) then begin
(*                Printf.printf "add doc to map"; print_newline (); *)
                  map := IntMap.add doc.doc_num doc !map
                end
          ) w.word_docs) (match s.suffix_word with
        None -> s.suffix_words | Some w -> w :: s.suffix_words);    
  in
  iter tree;
  !map

let and_get_fields and_map tree fields f = 
  let map = ref IntMap.empty in 
  let rec iter tree = 
(* retourne une liste de listes triees de documents *)
    match tree with
      List (s, tree_list) ->
        iter_suffix s;
        List.iter (fun (_, tree) ->
            iter tree.tree_expr
        ) tree_list
    | String (_, s) 
    | Suffix s ->
        iter_suffix s
    | Filtered _ -> ()
        
  and iter_suffix s =
(*    Printf.printf "ITER AND SUFFIX [%d]" s.suffix_num; print_newline (); *)
    List.iter (fun w ->
        if not w.word_filtered then
(*        Printf.printf "ON WORD [%s]" w.word_string; print_newline (); *)
        List.iter (fun (doc, where) ->
(*            Printf.printf "CHECK %d" doc.doc_num; print_newline (); *)
            if where land fields <> 0 &&
              f doc &&
              not doc.doc_filtered &&
              IntMap.mem doc.doc_num and_map &&
              not (
                IntMap.mem doc.doc_num !map) then begin
(*                Printf.printf "add doc to AND map"; print_newline (); *)
                map := IntMap.add doc.doc_num doc !map
              end
            else begin
                
(*                Printf.printf "AND failed"; print_newline (); *)
                ()
              end
        ) w.word_docs)  (match s.suffix_word with
        None -> s.suffix_words | Some w -> w :: s.suffix_words);
  in
  iter tree;
  !map
  

let complex_request idx req f = 
  try
    let  trees = List.map (fun (s,fields) ->
(*          Printf.printf "FIND [%s]" s; print_newline (); *)
          s, find idx s, fields
      ) req in
    match trees with
      [] ->  []
    | (s, tree, fields) :: tail -> 
        let hits = get_fields tree fields f in
        let hits = List.fold_left (fun hits (s, tree, fields) ->
(*              Printf.printf "AND GET FIELD on [%s]" s; print_newline (); *)
              and_get_fields hits tree fields f) hits tail in
        let list = ref [] in
        IntMap.iter (fun _ doc -> 
            if not doc.doc_filtered then
              list := doc :: !list) hits;
(*        Printf.printf "FOUND %d documents" (List.length !list); 
        print_newline (); *)
        !list
  with Not_found -> 
(*      Printf.printf "Not found"; print_newline (); *)
      []
      
let value doc = doc.doc_value

let rec filter_tree tree = 
  match tree.tree_expr with
    List (s, list) ->
      List.iter (fun (c, tree) -> filter_tree tree) list;
      filter_suffix s
  | String (_,s) 
  | Suffix s -> filter_suffix s
  | Filtered t ->
      filter_tree t
      
and filter_suffix s =
  List.iter (fun w ->
      if not w.word_filtered then begin
(*          Printf.printf "FILTER ON WORD [%s]" w.word_string; 
  print_newline (); *)
          w.word_filtered <- true;
          List.iter (fun (doc, where) ->
              if not doc.doc_filtered then
                doc.doc_filtered <- true) w.word_docs
        end)
  (match s.suffix_word with
      None -> s.suffix_words | Some w -> w :: s.suffix_words)
    
let add_filter idx s =
(*  Printf.printf "ADD FILETRR ON %s" s; print_newline (); *)
  let len = String.length s in
  let rec iter pos tree =
    if pos = len then begin
(*        Printf.printf "FILTER AT FINAL POS"; print_newline (); *)
        tree.tree_expr <- Filtered { tree_expr = tree.tree_expr };
        filter_tree tree
      end
    else
    match tree.tree_expr with
    | Suffix _ -> 
(*        Printf.printf "FILTER ON SUFFIX ???"; print_newline (); *)
        assert false
    | String (str, num) ->
(*        Printf.printf "FILTER ON STRING "; print_newline (); *)
        if str = "" then
          let new_num = new_word (String.sub s 0 pos) in
          tree.tree_expr <- List (num, 
            [s.[pos], 
              { tree_expr = String (
                  String.sub s (pos+1) (String.length s -1-pos), new_num); 
              }]);
        else 
          tree.tree_expr <- List (new_word (String.sub s 0 pos), 
            [str.[0], 
              { tree_expr = String (
                  String.sub str 1 (String.length str - 1), num); }]);
        iter pos tree; 
    | Filtered tree ->
(*        Printf.printf "FILTER ON FILTER"; print_newline (); *)
        iter pos tree
    | List (num, list) -> 
(*        Printf.printf "FILTER IN LIST"; print_newline (); *)
        let c = s.[pos] in
        try 
          iter (pos+1) (List.assoc c list)
        with Not_found ->
            let new_num = new_word s in
            let new_tree = { tree_expr = String (
                    String.sub s (pos+1) (String.length s -pos-1), new_num); 
              } in
            tree.tree_expr <- List (num, (c, new_tree) :: list);
            iter (pos+1) new_tree
  in
  iter 0 idx.index_tree

let filter_words index list = 
  List.iter (fun s -> ignore (add_filter index s)) list
  
let clear_filter index = 
  let rec iter tree =
    match tree.tree_expr with
      List (s, list) ->
        List.iter (fun (c, tree) -> iter tree) list;
        iter_suffix s
    | String (_,s) 
    | Suffix s -> iter_suffix s
    | Filtered t ->
(*        Printf.printf "REMOVE FILTER"; print_newline (); *)
        tree.tree_expr <- t.tree_expr;
        iter tree
  
  and iter_suffix s =
    List.iter (fun w ->
        if w.word_filtered then begin
(*            Printf.printf "REMOVE FILTERED WORD %s" w.word_string;
            print_newline (); *)
            w.word_filtered <- false;
            List.iter (fun (doc, where) ->
                if doc.doc_filtered then begin
                    (*
                    Printf.printf "RMOVE FILTER ON DOC"; 
print_newline ();
  *)
                    doc.doc_filtered <- false
                  end) w.word_docs
          end)
    (match s.suffix_word with
    None -> s.suffix_words | Some w -> w :: s.suffix_words)

  in
  iter index.index_tree
  
let filtered doc = doc.doc_filtered