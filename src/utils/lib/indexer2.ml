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
        mutable ndocs : int;
        
        mutable nodes : filtered_node array;
      }
    
    type  index = {
        mutable node : node; 
      }
    
    
    let stats index =
      let mem = ref 2 in
      let rec iter node =
        mem := !mem + 9 + 
          Array.length node.docs + Array.length node.fields + 
          Array.length node.nodes;
        Array.iter (fun n ->
            match n with None -> ()
            | Some node -> 
                mem := !mem + 2;
                iter node
            | Filtered node ->
                mem := !mem + 2;
                iter node
        ) node.nodes                
      in
      iter index.node;
      !mem
      
    let new_node () = {
        next_doc = 0;
        docs  = [||];
        fields = [||];
        nodes = [||];
        ndocs = 0;
      }
    
    let empty = new_node ()
    
    let create () = { node = new_node () }
    
    let convert_char c =
      match c with
        'a' .. 'z' -> int_of_char c - 97
      | 'A' .. 'Z' -> int_of_char c - 65
      | '0' .. '9' -> 26 + (int_of_char c - int_of_char '0')
      | _ -> assert false
    
    let exit_exn = Exit
      
    let add_doc node doc fields = 
(*      lprintf "add_doc"; lprint_newline ();  *)
      let len = Array.length node.docs in
      let pos = node.next_doc in
      try
        for i = 0 to node.next_doc - 1 do
          if node.docs.(i) == doc then begin
              node.fields.(i) <- node.fields.(i) lor fields;
              raise exit_exn;
            end;
        done;
        if pos = len then begin
            let new_docs = Array.make (len + len/2 + 2) doc in
            let new_fields = Array.make (len + len/2 + 2) 0 in
            Array.blit node.docs 0 new_docs 0 len;
            Array.blit node.fields 0 new_fields 0 len;
            node.docs <- new_docs;
            node.fields <- new_fields
          end;
        node.docs.(pos) <- doc;
(*        lprintf "Adding doc with field %d" fields; lprint_newline (); *)
        node.fields.(pos) <- fields;
        node.next_doc <- pos +1;
        node.ndocs <- node.ndocs + 1;
        true
      with e ->
(*          lprintf "exn %s" (Printexc2.to_string e); lprint_newline (); *)
          false
(* ; lprintf "done"; lprint_newline () *)
    
    let add_char node c = 
(*      lprintf "add_char"; lprint_newline (); *)
      let n = new_node () in
      let len = Array.length node.nodes in
      if len <= c then begin
          let new_nodes = Array.make (c+1) None in
          Array.blit node.nodes 0 new_nodes 0 len;
          node.nodes <- new_nodes;
        end;
(*      lprintf "set %d %d %d" c len (Array.length node.nodes); lprint_newline (); *)
      node.nodes.(c) <- Some n;
(*      lprintf "done"; lprint_newline (); *)
      n
    
    let add index string doc fields = 
(*      lprintf "add (%s)" string; lprint_newline ();  *)
      try
(*      lprintf "add"; lprint_newline (); *)
        let len = String.length string in
        let rec iter pos node = 
(*          lprintf "pos %d" pos; lprint_newline (); *)
          if pos = len then
            if add_doc node doc fields then begin
                node.ndocs <- node.ndocs + 1;
                true
              end else false
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
                  lprintf_nl "doc filtered";
                  raise Not_found
            else
              add_char node c
          in
          iter (pos+1) node
        in
        ignore (iter 0 index.node)
      with e -> 
(*          lprintf "Exc %s" (Printexc2.to_string e);lprint_newline (); *)
          ()
(* ; lprintf "done"; lprint_newline ()  *)
    
    let clear index = index.node <- new_node () 

    let filter_node node bool =
(*      lprintf "filter node"; lprint_newline (); *)
      for i = 0 to node.next_doc - 1 do
(*        lprintf "filter doc %s\n" (string_of_bool bool); *)
        Doc.filter node.docs.(i) bool;
(*        if Doc.filtered node.docs.(i) then
          (lprintf "doc is filtered\n");  *)
      done

    let rec filter_nodes node bool =
(*      lprintf "filter_nodes"; lprint_newline (); *)
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
(*      lprintf "FILTER ALL"; lprint_newline (); *)
      List.iter (fun s -> 
(*          lprintf "filter (%s)" s; lprint_newline (); *)
          add_filter index s) list

    let clear_filter index = 
(*      lprintf "CLEAR FILTER"; lprint_newline (); *)
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
      
    let size node = node.ndocs
  end
  
  
module FullMake (Doc : Indexer.Doc) = Indexer.FullMake (Doc ) (Make)
  
