type 'a t = 
  { 
    mutable nfree : int;
    mutable array : 'a option array;
    mutable free : int array;
  }
  
let create () = {
    nfree = 0;
    array = [||];
    free = [||];
  }
  
let add t v =
  let len = Array.length t.array in
  if t.nfree = 0 then begin
      let new_len = 2 * len + 10 in
      let new_array = Array.create new_len None in
      Array.blit t.array 0 new_array 0 len;
      let new_free = Array.create new_len 0 in
      for i = len to new_len - 1 do
        new_free.(i - len) <- i;
      done;
      t.array <- new_array;
      t.free <- new_free;
      t.nfree <- new_len - len;
    end;
  let pos = t.nfree - 1 in
  let free_pos = t.free.(pos) in
  t.array.(pos) <- Some v;
  t.nfree <- pos;
  pos
  
let get t i =  
  match t.array.(i) with 
    None -> raise Not_found
  | Some v -> v
      
let free t i =
  let pos = t.nfree in
  t.nfree <- pos + 1;
  t.free.(pos) <- i
  
  