
let rec removeq_rec ele list tail =
  match list with
    [] -> List.rev tail
  | e :: list ->
      if e == ele then removeq_rec ele list tail
      else
        removeq_rec ele list (e :: tail)

let rec removeq ele list =
  removeq_rec ele list []

let rec remove_rec ele list tail =
  match list with
    [] -> List.rev tail
  | e :: list ->
      if e = ele then remove_rec ele list tail
      else
        remove_rec ele list (e :: tail)
        
let remove ele list =
  remove_rec ele list []
  
let rec removeq_first ele list =
  match list with
    e :: tail when e == ele -> tail
  | e :: tail -> e :: (removeq_first ele tail)
  | _ -> []

let rec remove_first ele list =
  match list with
    e :: tail when e = ele -> remove_first ele tail
  | e :: tail -> e :: (remove_first ele tail)
  | _ -> []

let rec cut_rec n list r =
  match n, list with
    (0,_) | (_, []) -> List.rev r, list
  | _, x :: tail ->
      cut_rec (n-1) tail (x :: r)
      
let cut n list =
  if n < 0 then failwith "List2.sub: invalid parameter";
  cut_rec n list []
  