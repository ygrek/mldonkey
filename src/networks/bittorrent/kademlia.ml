(** Kademlia 

  Petar Maymounkov and David Mazières
  "Kademlia: A Peer-to-Peer Information System Based on the XOR Metric"
  http://infinite-source.de/az/whitepapers/kademlia_optimized.pdf
*)

let bucket_nodes = 8

(* do not use CommonOptions directly so that tools/bt_dht_node can be compiled separately *)
let verb = ref false
let debug = ref false

module H = Md4.Sha1

let log_prefix = "[btkad]"
let lprintf_nl fmt = Printf2.lprintf_nl2 log_prefix fmt

(** node ID type *)
type id = H.t
let show_id h = let s = H.to_hexa h in (String.sub s 0 7 ^ ".." ^ String.sub s 17 3)
type addr = Ip.t * int

type time = int
let minutes n = 60 * n
let node_period = minutes 15
type status = | Good | Bad | Unknown | Pinged
type node = { id : id; addr : addr; mutable last : time; mutable status : status; }
type bucket = { lo : id; hi : id; mutable last_change : time; mutable nodes : node array; }
(* FIXME better *)
type tree = L of bucket | N of tree * id * tree
type table = { mutable root : tree; self : id; }

let now = BasicSocket.last_time
let diff t = Printf.sprintf "%d sec ago" (now () - t)

let show_addr (ip,port) = Printf.sprintf "%s:%u" (Ip.to_string ip) port

let show_status = function
  | Good -> "good"
  | Bad -> "bad"
  | Unknown -> "unknown"
  | Pinged -> "pinged"

let show_node n =
  Printf.sprintf "%s at %s was %s %s"
    (show_id n.id) (show_addr n.addr) (show_status n.status) (diff n.last)

let show_bucket b =
  lprintf_nl "count : %d lo : %s hi : %s changed : %s" (Array.length b.nodes) (H.to_hexa b.lo) (H.to_hexa b.hi) (diff b.last_change);
  Array.iter (fun n -> lprintf_nl "  %s" (show_node n)) b.nodes

let rec show_tree = function
  | N (l,_,r) -> show_tree l; show_tree r
  | L b -> show_bucket b

let h2s h =
  let s = H.direct_to_string h in
  assert (String.length s = H.length);
  s

type cmp = LT | EQ | GT

let cmp id1 id2 = 
  match String.compare (h2s id1) (h2s id2) with
  | -1 -> LT
  | 0 -> EQ
  | 1 -> GT
  | _ -> assert false

(* boundaries inclusive *)
let inside node hash = not (cmp hash node.lo = LT || cmp hash node.hi = GT)

let middle =
  let s = String.make 20 (Char.chr 0xFF) in
  s.[0] <- Char.chr 0x7F;
  H.direct_of_string s

let middle' =
  let s = String.make 20 (Char.chr 0x00) in
  s.[0] <- Char.chr 0x80;
  H.direct_of_string s

let last =
  H.direct_of_string (String.make 20 (Char.chr 0xFF))

open Big_int

let big_int_of_hash h =
  let s = h2s h in
  let n = ref zero_big_int in
  for i = 0 to String.length s - 1 do
    n := add_int_big_int (Char.code s.[i]) (mult_int_big_int 256 !n)
  done;
  !n

let hash_of_big_int n =
  let s = String.create H.length in
  let n = ref n in
  let div = big_int_of_int 256 in
  for i = String.length s - 1 downto 0 do
    let (d,m) = quomod_big_int !n div in
    s.[i] <- Char.chr (int_of_big_int m);
    n := d
  done;
  assert (eq_big_int zero_big_int !n);
  H.direct_of_string s

let big_int_2 = big_int_of_int 2
(* hash <-> number *)
let h2n = big_int_of_hash
let n2h = hash_of_big_int

let choose_random lo hi =
  assert (cmp lo hi = LT);
  let rec loop a b =
    if cmp a b = EQ then a else
    let mid = n2h (div_big_int (add_big_int (h2n a) (h2n b)) big_int_2) in
    if Random.bool () then loop a mid else loop mid b
  in
  loop lo hi

let split lo hi =
  assert (cmp lo hi = LT);
  let mid = div_big_int (add_big_int (h2n lo) (h2n hi)) big_int_2 in
  n2h mid 

let succ h =
  assert (cmp h last <> EQ);
  n2h (succ_big_int (h2n h))

let distance h1 h2 =
  let s1 = h2s h1 and s2 = h2s h2 in
  let d = ref zero_big_int in
  for i = 0 to H.length - 1 do
    let x = Char.code s1.[i] lxor Char.code s2.[i] in
    d := add_int_big_int x (mult_int_big_int 256 !d)
  done;
  !d

let () =
  assert (LT = cmp H.null middle);
  assert (LT = cmp H.null middle');
  assert (LT = cmp H.null last);
  assert (GT = cmp middle' middle);
  assert (GT = cmp last middle');
  assert (GT = cmp last middle);
  assert (EQ = cmp H.null H.null);
  assert (EQ = cmp middle middle);
  assert (EQ = cmp last last);
  assert (n2h (h2n middle) = middle);
  assert (n2h (h2n middle') = middle');
  assert (n2h (h2n last) = last);
  assert (n2h (h2n H.null) = H.null);
  assert (compare_big_int (h2n H.null) zero_big_int = 0);
  assert (cmp (split H.null last) middle = EQ);
  assert (eq_big_int (distance H.null last) (pred_big_int (power_int_positive_int 2 160)));
  assert (eq_big_int (distance middle' middle) (pred_big_int (power_int_positive_int 2 160)));
  ()

(*
module type Network = sig
  type t
  val ping : t -> addr -> (id -> bool -> unit) -> unit
end
*)

(* module Make(T : Network) = struct *)

exception Nothing 

let make_node id addr st = { id = id; addr = addr; last = now (); status = st; }
let mark n st =
  if !debug then
    lprintf_nl "mark [%s] as %s" (show_node n) (show_status st);
  n.last <- now ();
  n.status <- st
let touch b = b.last_change <- now ()

(*
let rec delete table id =
  let rec loop = function
  | N (l,mid,r) -> (match cmp id mid with LT | EQ -> N (loop l, mid, r) | GT -> N (l, mid, loop r))
  | L b ->
    Array.iter (fun n ->
      if cmp n.id id = EQ then
*)

let rec update ping table st id data =
(*  if !debug then lprintf_nl "insert %s" (show_id node.id); *)
  let rec loop = function
  | N (l,mid,r) -> (match cmp id mid with LT | EQ -> N (loop l, mid, r) | GT -> N (l, mid, loop r))
  | L b ->
    Array.iteri begin fun i n ->
      match cmp n.id id = EQ, n.addr = data with
      | true, true -> mark n st; touch b; raise Nothing
      | true, false | false, true -> 
          if !verb then
            lprintf_nl "conflict [%s] with %s %s, replacing" (show_node n) (show_id id) (show_addr data);
          b.nodes.(i) <- make_node id data st; (* replace *)
          touch b;
          raise Nothing
      | _ -> ()
    end b.nodes;
    if Array.length b.nodes <> bucket_nodes then
    begin
      if !debug then
        lprintf_nl "insert %s %s" (show_id id) (show_addr data);
      b.nodes <- Array.of_list (make_node id data st :: Array.to_list b.nodes);
      touch b;
      raise Nothing
    end;
    Array.iteri (fun i n ->
      if n.status = Good && now () - n.last > node_period then mark n Unknown;
      if n.status = Bad || (n.status = Pinged && now () - n.last > node_period) then
      begin
        if !debug then
          lprintf_nl "replace [%s] with %s" (show_node b.nodes.(i)) (show_id id);
        b.nodes.(i) <- make_node id data st; (* replace *)
        touch b;
        raise Nothing
      end) b.nodes;
    match Array.fold_left (fun acc n -> if n.status = Unknown then n::acc else acc) [] b.nodes with
    | [] ->
    if inside b table.self && gt_big_int (distance b.lo b.hi) (big_int_of_int 256) then
    begin
      if !debug then
        lprintf_nl "split %s %s" (H.to_hexa b.lo) (H.to_hexa b.hi);
      let mid = split b.lo b.hi in
      let (nodes1,nodes2) = List.partition (fun n -> cmp n.id mid = LT) (Array.to_list b.nodes) in
      let new_node = N (
          L { lo = b.lo; hi = mid; last_change = b.last_change; nodes = Array.of_list nodes1; }, 
          mid,
          L { lo = succ mid; hi = b.hi; last_change = b.last_change; nodes = Array.of_list nodes2; } )
      in
      new_node
    end
    else
    begin
      if !debug then
        lprintf_nl "bucket full (%s)" (show_id id);
      raise Nothing
    end
    | unk ->
      let count = ref (List.length unk) in
      if !debug then
        lprintf_nl "ping %d unknown nodes" !count;
      let cb n = fun res ->
        decr count; mark n (match res with Some _ -> Good | None -> Bad); 
        if !count = 0 then (* retry *)
        begin 
          if !debug then
            lprintf_nl "all %d pinged, retry %s" (List.length unk) (show_id id); 
          touch b; 
          update ping table st id data 
        end
      in
      List.iter (fun n -> mark n Pinged; ping n.addr (cb n)) unk;
      raise Nothing
  in
  if id <> table.self then
    try while true do table.root <- loop table.root done with Nothing -> () (* loop until no new splits *)

let insert_node table node =
(*  if !debug then lprintf_nl "insert %s" (show_id node.id); *)
  let rec loop = function
  | N (l,mid,r) -> (match cmp node.id mid with LT | EQ -> N (loop l, mid, r) | GT -> N (l, mid, loop r))
  | L b ->
    Array.iter begin fun n ->
      match cmp n.id node.id = EQ, n.addr = node.addr with
      | true, true ->
        if !verb then lprintf_nl "insert_node: duplicate entry %s" (show_node n);
        raise Nothing
      | true, false | false, true ->
        if !verb then lprintf_nl "insert_node: conflict [%s] with [%s]" (show_node n) (show_node node);
        raise Nothing
      | _ -> ()
    end b.nodes;
    if Array.length b.nodes <> bucket_nodes then
    begin
      b.nodes <- Array.of_list (node :: Array.to_list b.nodes);
      raise Nothing
    end;
    if inside b table.self && gt_big_int (distance b.lo b.hi) (big_int_of_int 256) then
    begin
      let mid = split b.lo b.hi in
      let (nodes1,nodes2) = List.partition (fun n -> cmp n.id mid = LT) (Array.to_list b.nodes) in
      let last_change = List.fold_left (fun acc n -> max acc n.last) 0 in
      let new_node = N (
          L { lo = b.lo; hi = mid; last_change = last_change nodes1; nodes = Array.of_list nodes1; }, 
          mid,
          L { lo = succ mid; hi = b.hi; last_change = last_change nodes2; nodes = Array.of_list nodes2; } )
      in
      new_node
    end
    else
    begin
      if !verb then lprintf_nl "insert_node: bucket full [%s]" (show_node node);
      raise Nothing
    end
  in
  try while true do table.root <- loop table.root done with Nothing -> ()

let all_nodes t =
  let rec loop acc = function
  | N (l,_,r) -> let acc = loop acc l in loop acc r
  | L b -> Array.to_list b.nodes @ acc
  in
  loop [] t.root

(* end *)

let refresh table =
  let expire = now () - node_period in
  let rec loop acc = function
  | N (l,_,r) -> let acc = loop acc l in loop acc r
  | L b when b.last_change < expire ->
    if Array2.exists (fun n -> n.status <> Bad) b.nodes then
      let nodes = Array.map (fun n -> n.id, n.addr) b.nodes in
      (choose_random b.lo b.hi, Array.to_list nodes) :: acc
    else
      acc (* do not refresh buckets with all bad nodes *)
  | L _ -> acc
  in
  loop [] table.root

let find_node t h =
  let rec loop alt = function
  | N (l,mid,r) -> (match cmp h mid with LT | EQ -> loop (r::alt) l | GT -> loop (l::alt) r)
  | L b ->
    let found = Array.to_list b.nodes in
    if Array.length b.nodes = bucket_nodes then found
    else found
(* FIXME 
      List.iter (fun node -> fold (fun acc b -> 
        let acc = Array.to_list b.nodes @ acc in
        if List.length acc >= bucket_nodes then raise Nothing 
*)
  in
  loop [] t.root

let create () = { root = L { lo = H.null; hi = last; last_change = now (); nodes = [||]; };
                  self = H.random (); 
                }

let show_table t =
  lprintf_nl "self : %s now : %d" (show_id t.self) (now ());
  show_tree t.root

let rec fold f acc = function
  | N (l,_,r) -> fold f (fold f acc l) r
  | L b -> f acc b

let size t = fold (fun acc b -> acc + Array.length b.nodes) 0 t.root
let buckets t = fold (fun acc b -> acc + 1) 0 t.root

(*
module NoNetwork : Network = struct 
  let ping addr k = k H.null (Random.bool ())
end
module K = Make(NoNetwork)
*)

let tt () =
  let table = create () in
  show_table table; 
  let addr = Ip.of_string "127.0.0.1", 9000 in
  let ping addr k = k (if Random.bool () then Some (H.null,addr) else None) in
  for i = 1 to 1_000_000 do
    update ping table Good (H.random ()) addr
  done;
  show_table table

module RoutingTableOption = struct

open Options

let value_to_status = function
  | StringValue "good" -> Good
  | StringValue "bad" -> Bad
  | StringValue "pinged" -> Pinged
  | StringValue "unknown" -> Unknown
  | _ -> failwith "RoutingTableOption.value_to_status"

let status_to_value = function
  | Good -> string_to_value "good"
  | Bad -> string_to_value "bad"
  | Pinged -> string_to_value "pinged"
  | Unknown -> string_to_value "unknown"

let value_to_node = function
  | Module props ->
    let get cls s = from_value cls (List.assoc s props) in
    {
      id = H.of_hexa (get string_option "id");
      addr = (get Ip.option "ip", get port_option "port");
      last = get int_option "last";
      status = value_to_status (List.assoc "status" props);
    }
 | _ -> failwith "RoutingTableOption.value_to_node"

let node_to_value n =
  Module [
    "id", string_to_value (H.to_hexa n.id);
    "ip", to_value Ip.option (fst n.addr);
    "port", to_value port_option (snd n.addr);
    "last", int_to_value n.last;
    "status", status_to_value n.status;
  ]

let value_to_table v =
  match v with
  | Module props ->
    let nodes = value_to_list value_to_node (List.assoc "nodes" props) in
    let self = H.of_hexa (value_to_string (List.assoc "self" props)) in
    let t = { root = L { lo = H.null; hi = last; last_change = 0; nodes = [||]; };
              self = self; }
    in
    List.iter (insert_node t) nodes;
    if !debug then show_table t;
    t
  | _ -> failwith "RoutingTableOption.value_to_table"

let table_to_value t =
  if !debug then show_table t;
  Module [
    "self", string_to_value (H.to_hexa t.self);
    "nodes", list_to_value node_to_value (all_nodes t)
  ]

let t = define_option_class "RoutingTable" value_to_table table_to_value

end

