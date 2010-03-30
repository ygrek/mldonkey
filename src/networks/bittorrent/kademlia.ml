(** Generic implementation of Kademlia *)

let k = 8

module H = Md4.Sha1

let pr fmt = Printf.ksprintf print_endline fmt

let () =
  let hash = H.random () in
  pr "%d" H.length;
  pr "%x %x %x" (H.up hash) (H.up2 hash) (H.up3 hash);
  print_endline (H.to_string hash);
  print_endline (H.direct_to_string hash);
  print_endline (H.to_hexa hash);
  print_endline (H.to_bits hash);
  print_endline (H.to_base32 hash);

type id = H.t
let show_id = H.to_hexa

type time = float
type status = | Good | Bad | Unknown | Pinged
type node = { id : id; ip : Ip.t; port : int; mutable last : time; mutable status : status; }
type bucket = { lo : id; hi : id; mutable last_change : time; nodes : node array; }
type table = bucket array

let show_status = function
  | Good -> "good"
  | Bad -> "bad"
  | Unknown -> "unk"
  | Pinged -> "ping"

let show_node n =
  pr " id : %s inet %s:%d last : %f status : %s" 
    (H.to_hexa n.id) (Ip.to_string n.ip) n.port n.last (show_status n.status)

let show_bucket b = 
  pr "lo : %s hi : %s changed : %f" (H.to_hexa b.lo) (H.to_hexa b.hi) b.last_change;
  Array.iter show_node b.nodes

let show_table = Array.iter show_bucket 

type cmp = LT | EQ | GT

let cmp id1 id2 = 
  match String.compare (H.direct_to_string id1) (H.direct_to_string id2) with
  | -1 -> LT
  | 0 -> EQ
  | 1 -> GT
  | _ -> assert false

(* boundaries inclusive *)
let between x lo hi = not (cmp x lo = LT || cmp x hi = GT)

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
  let s = String.make 21 (Char.chr 0x00) in
  s.[0] <- Char.chr 0x80;
  H.direct_of_string s

let last =
  H.direct_of_string (String.make 20 (Char.chr 0xFF))

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
  assert (EQ = cmp last last)

let now = Unix.gettimeofday ()

let create_table () =
  Array.init 2 (function 
    | 0 -> { lo = H.null; hi = middle; last_change = now; nodes = [||]; }
    | _ -> { lo = middle'; hi = last; last_change = now; nodes = [||]; })

let () =
  show_table (create_table ())

