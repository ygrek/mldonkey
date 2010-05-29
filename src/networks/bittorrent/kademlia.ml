(** Generic implementation of Kademlia *)

let bucket_nodes = 8

module H = Md4.Sha1

let pr fmt = Printf.ksprintf print_endline fmt

let () =
  let hash = H.random () in
  pr "len: %d up: %x %x %x" H.length (H.up hash) (H.up2 hash) (H.up3 hash);
  pr "string: %s" (H.to_string hash);
  pr "direct: %S" (H.direct_to_string hash);
  pr "hexa: %s" (H.to_hexa hash);
  pr "bits: %s" (H.to_bits hash);
  pr "base32: %s" (H.to_base32 hash);

(** node ID type *)
type id = H.t
let show_id = H.to_hexa
type addr = Ip.t * int

type time = float
type status = | Good | Bad | Unknown | Pinged
type node = { id : id; addr : addr; mutable last : time; mutable status : status; }
type bucket = { lo : id; hi : id; mutable last_change : time; nodes : node array; }
(* FIXME better *)
type tree = L of bucket | N of tree * tree
type table = { mutable root : tree; self : id; }

let show_addr (ip,port) = Printf.sprintf "%s:%u" (Ip.to_string ip) port

let show_status = function
  | Good -> "good"
  | Bad -> "bad"
  | Unknown -> "unk"
  | Pinged -> "ping"

let show_node n =
  pr " id : %s inet %s last : %f status : %s" 
    (H.to_hexa n.id) (show_addr n.addr) n.last (show_status n.status)

let show_bucket b = 
  pr "lo : %s hi : %s changed : %f" (H.to_hexa b.lo) (H.to_hexa b.hi) b.last_change;
  Array.iter show_node b.nodes

let rec show_table = function 
  | N (l,r) -> show_table l; show_table r
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
let inside x node = not (cmp x node.lo = LT || cmp x node.hi = GT)

let bracket res destroy k =
  let x = try k res with exn -> destroy res; raise exn in
  destroy res;
  x

let with_open_in_bin file = bracket (open_in_bin file) close_in_noerr
let with_open_out_bin file = bracket (open_out_bin file) close_out_noerr

let load file : table = with_open_in_bin file Marshal.from_channel
let store file (t:table) = with_open_out_bin file (fun ch -> Marshal.to_channel ch t [])

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

(* hash <-> number *)
let h2n = big_int_of_hash
let n2h = hash_of_big_int

let split lo hi =
  assert (cmp lo hi = LT);
  let mid = div_big_int (add_big_int (h2n lo) (h2n hi)) (big_int_of_int 2) in
  n2h mid 

let distance h1 h2 =
  let s1 = h2s h1 and s2 = h2s h2 in
  let d = ref zero_big_int in
  for i = 0 to H.length - 1 do
    let x = Char.code s1.[i] lxor Char.code s2.[i] in
    d := add_int_big_int x (mult_int_big_int 256 !d)
  done;
  !d

let () =
  print_endline (show_id H.null);
  print_endline (show_id middle);
  print_endline (show_id middle');
  print_endline (show_id last);
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

exception Nothing

let insert table node =
  let rec loop = function
  | N (l,r) -> if cmp node.id r.lo = LT then N (loop l, r) else N (l, loop r)
  | L b ->
    if Array.find (fun n -> n.id = node.id) 
  Array.length b.nodes <> bucket_nodes -> b.nodes <- Array.of_list (node::Array.to_list b.nodes); raise Nothing
  | L b when inside table.self b -> 
  | L _ -> raise Nothing (* throw away *)
  in
  try table.root <- loop table.root
  with Nothing -> ()

let now = Unix.gettimeofday

let empty () = L { lo = H.null; hi = last; last_change = now (); nodes = [||]; }

let table = { root = empty (); self = H.random (); }

let () =
  show_table !table

