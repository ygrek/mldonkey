type 'a t = 'a hole_array array
and 'a hole_array =
  Hole 
| Array of 'a option array
  
let bucket_size = 16

let create n = Array.create (n / bucket_size) Hole

let not_found = Not_found
  
let set t n v =
  let array =
    match t.(n / bucket_size) with
      Hole -> 
        let array = Array.create bucket_size None in
        t.(n / bucket_size) <- Array array;
        array
    | Array array -> array
  in
  array.(n mod bucket_size) <- Some v
      
let get t n =
  match t.(n / bucket_size) with
    Hole -> raise not_found
  | Array array -> 
      match array.(n mod bucket_size) with
        None -> raise not_found
      | Some v -> v
  
  