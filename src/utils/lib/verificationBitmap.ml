type t = string
type part_state = 
    State_missing | State_partial | State_complete | State_verified
        
let state_to_char = function
  | State_missing -> '0'
  | State_partial -> '1'
  | State_complete -> '2'
  | State_verified -> '3'
;;
let char_to_state = function
  | '0' -> State_missing
  | '1' -> State_partial
  | '2' -> State_complete
  | '3' -> State_verified 
  | _ -> assert false

let create n c = String.make n (state_to_char c)
let get x i = (char_to_state x.[i])
let set x i c = x.[i] <- state_to_char c
let length = String.length 
let init n f =
  let s = String.create n in
  for i = 0 to n - 1 do
    set s i (f i)
  done;
  s
let to_string x = x
let of_string x = x
  
let iteri f x = 
  let l = String.length x in
  let rec aux i =
    if i < l then begin
      f i (char_to_state x.[i]);
      aux (i+1)
    end in
  aux 0
    
let mapi f x =
  Array.init (length x) (fun i -> f i (get x i))
    
let fold_lefti f acc x =
  let l = String.length x in
  let rec aux acc i =
    if i = l then acc
    else aux (f acc i (get x i)) (i + 1) in
  aux acc 0
    
let existsi p x = 
  let l = String.length x in
  let rec aux i =
    i < l && (p i (char_to_state x.[i]) || aux (i+1)) in
  aux 0
    
let for_all p s =
  let l = String.length s in
  let rec aux i =
    i >= l || p (char_to_state s.[i]) && aux (i+1) in
  aux 0
