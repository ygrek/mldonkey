type t = bytes
type part_state =
    State_missing | State_partial | State_complete | State_verified

let state_to_char = function
  | State_missing -> '0'
  | State_partial -> '1'
  | State_complete -> '2'
  | State_verified -> '3'

let char_to_state = function
  | '0' -> State_missing
  | '1' -> State_partial
  | '2' -> State_complete
  | '3' -> State_verified
  | _ -> assert false

let create n c = Bytes.make n (state_to_char c)
let get x i = char_to_state (Bytes.get x i)
let set x i c = Bytes.set x i (state_to_char c)
let length = Bytes.length
let init n f =
  let s = Bytes.create n in
  for i = 0 to n - 1 do
    set s i (f i)
  done;
  s
let to_string x = Bytes.to_string x
let of_string x = Bytes.of_string x

let iteri f x =
  let l = Bytes.length x in
  let rec aux i =
    if i < l then begin
      f i (get x i);
      aux (i+1)
    end in
  aux 0

let mapi f x =
  Array.init (length x) (fun i -> f i (get x i))

let fold_lefti f acc x =
  let l = length x in
  let rec aux acc i =
    if i = l then acc
    else aux (f acc i (get x i)) (i + 1) in
  aux acc 0

let existsi p x =
  let l = length x in
  let rec aux i =
    i < l && (p i (get x i) || aux (i+1)) in
  aux 0

let for_all p s =
  let l = length s in
  let rec aux i =
    i >= l || p (get s i) && aux (i+1) in
  aux 0
