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
  
type 'a query =
  And of 'a query * 'a query
| Or of 'a query * 'a query
| AndNot of 'a query * 'a query
| HasWord of string
| HasField of int * string
| Predicate of ('a -> bool)
  
module Make(Doc : sig
      type t
      
      val num : t -> int
      val filtered : t -> bool
      val filter : t -> bool -> unit
    end) = struct
    
    
    type doc = Doc.t
    
    type filtered_node =
      None
    | Some of node
    | Filtered of node
    
    and node = {
        mutable next_doc : int;
        mutable docs : Doc.t array;
        mutable fields : int array;
        
        mutable nodes : filtered_node array;
      }
    
    type  index = {
        mutable node : node; 
      }
    
    let new_node () = {
        next_doc = 0;
        docs  = [||];
        fields = [||];
        nodes = [||];
      }
    
    let empty = new_node ()
    
    let create () = { node = new_node () }
    
    let convert_char c =
      match c with
        'a' .. 'z' -> int_of_char c - 97
      | 'A' .. 'Z' -> int_of_char c - 65
      | '0' .. '9' -> 26 + (int_of_char c - int_of_char '0')
      | _ -> assert false
    
    let add_doc node doc fields = 
(*      Printf.printf "add_doc"; print_newline (); *)
      let len = Array.length node.docs in
      let pos = node.next_doc  in
      if pos = len then begin
          let new_docs = Array.create (len + len/2 + 2) doc in
          let new_fields = Array.create (len + len/2 + 2) 0 in
          Array.blit node.docs 0 new_docs 0 len;
          Array.blit node.fields 0 new_fields 0 len;
          node.docs <- new_docs;
          node.fields <- new_fields
        end;
      node.docs.(pos) <- doc;
      node.fields.(pos) <- fields;
      node.next_doc <- pos +1
(* ; Printf.printf "done"; print_newline () *)
    
    let add_char node c = 
(*      Printf.printf "add_char"; print_newline (); *)
      let n = new_node () in
      let len = Array.length node.nodes in
      if len <= c then begin
          let new_nodes = Array.create (c+1) None in
          Array.blit node.nodes 0 new_nodes 0 len;
          node.nodes <- new_nodes;
        end;
(*      Printf.printf "set %d %d %d" c len (Array.length node.nodes); print_newline (); *)
      node.nodes.(c) <- Some n;
(*      Printf.printf "done"; print_newline (); *)
      n
    
    let add index string doc fields = 
(*      Printf.printf "add (%s)" string; print_newline (); *)
      try
(*      Printf.printf "add"; print_newline (); *)
        let len = String.length string in
        let rec iter pos node = 
          if pos = len then
            add_doc node doc fields
          else
          let c = string.[pos] in
          let c = convert_char c in
          let node = 
            if Array.length node.nodes > c then
              match node.nodes.(c) with
                None -> add_char node c
              | Some node -> node
              | Filtered _ -> 
                  Doc.filter doc true;
(*                  Printf.printf "doc filtered"; print_newline (); *)
                  raise Not_found
            else
              add_char node c
          in
          iter (pos+1) node
        in
        iter 0 index.node
      with _ -> ()
(* ; Printf.printf "done"; print_newline ()  *)
    
    let clear index = index.node <- new_node () 

    let rec filter_node node bool =
(*      Printf.printf "filter node"; print_newline (); *)
      for i = 0 to node.next_doc - 1 do
(*        Printf.printf "filter doc %s" (string_of_bool bool); print_newline (); *)
        Doc.filter node.docs.(i) bool;
(*        if Doc.filtered node.docs.(i) then
          (Printf.printf "doc is filtered"; print_newline ()); *)
      done

    let rec filter_nodes node bool =
(*      Printf.printf "filter_nodes"; print_newline (); *)
      filter_node node bool;
      let len = Array.length node.nodes in
      for i = 0 to len - 1 do
        match node.nodes.(i) with
          None -> ()
        | Some n -> filter_nodes n bool
        | Filtered n -> filter_nodes n bool
      done
      
    let add_filter index s =
      try
        let len = String.length s in
        let rec iter pos node = 
          let c = s.[pos] in
          let c = convert_char c in
          let n = 
            if Array.length node.nodes > c then
              match node.nodes.(c) with
                None -> add_char node c
              | Some node -> node
              | Filtered _ -> raise Not_found
            else
              add_char node c
          in
          if pos+1 = len then begin
              filter_nodes n true;
              node.nodes.(c) <- Filtered n
            end else
            iter (pos+1) n
        in
        iter 0 index.node
      with _ -> ()
          
      
    let filter_words index list = 
(*      Printf.printf "FILTER ALL"; print_newline (); *)
      List.iter (fun s -> 
(*          Printf.printf "filter (%s)" s; print_newline (); *)
          add_filter index s) list

    let clear_filter index = 
(*      Printf.printf "CLEAR FILTER"; print_newline (); *)
      let rec iter node =
        let len = Array.length node.nodes in
        for i = 0 to len - 1 do
          match node.nodes.(i) with
            Filtered n ->
              node.nodes.(i) <- Some n;
              filter_node n false;
              iter_in n
          | Some n -> iter n
          | _ -> ()
        done
        
      and  iter_in node =
        let len = Array.length node.nodes in
        for i = 0 to len - 1 do
          match node.nodes.(i) with
            Filtered n ->
              node.nodes.(i) <- Some n;
              filter_node n false;
              iter_in n
          | Some n -> 
              filter_node n false;
              iter_in n
          | _ -> ()
        done
      in
      iter index.node
      
    let filtered doc = Doc.filtered doc

      
      
    open Indexer
      
      
      
    let find node s = 
      let len = String.length s in
      let rec iter node pos =
        if pos = len then node else
        let c = s.[pos] in
        let c = convert_char c in
        if Array.length node.nodes > c then
          match node.nodes.(c) with
            None -> raise Not_found
          | Some node -> iter node (pos+1)
          | Filtered _ -> raise Not_found
        else raise Not_found
      in
      try
        iter node.node 0
      with _ -> empty

    let or_get_fields map node fields =
      let rec iter node =
        for i = 0 to node.next_doc - 1 do
          if node.fields.(i) land fields <> 0 then
            let doc = node.docs.(i) in
            if not (Doc.filtered doc) && 
              not (Intmap.mem (Doc.num doc) !map) then
              map := Intmap.add (Doc.num doc) doc !map
        done;
        for i = 0 to Array.length node.nodes - 1  do
          match node.nodes.(i) with
            None -> ()
          | Some node -> iter node
          | Filtered _ -> ()
        done;
      in iter node;
      !map
      
    let and_get_fields node fields and_map = 
      let map = ref Intmap.empty in
      let rec iter node =
        for i = 0 to node.next_doc - 1 do
          if node.fields.(i) land fields <> 0 then
            let doc = node.docs.(i) in
            if (Intmap.mem (Doc.num doc) and_map) &&
              not (Intmap.mem (Doc.num doc) !map) then
              map := Intmap.add (Doc.num doc) doc !map
        done;
        for i = 0 to Array.length node.nodes - 1  do
          match node.nodes.(i) with
            None -> ()
          | Some node -> iter node
          | Filtered _ -> ()
        done;
      in iter node;
      !map
  end
  
  
module FullMake (Doc : Indexer.Doc) = Indexer.FullMake (Doc ) (Make)