(* The implementation *)

type chunk = {
  mutable pos : int64;
  mutable len : int64;
  mutable filename : string option;
  mutable next : chunk option;
}

type t = {
  filename : string;
  size : int64;
  mutable chunks : chunk;
}

let create filename size = 
  let dirname = Printf.sprintf "%s.chunks" filename in
  Unix.mkdir dirname;
  {
    filename = filename;
    size = size;
    chunks = {
        pos = Int64.zero; 
        len = size; 
        created = None;
        next = None;
       }; 
  }

let (+) = Int64.add
let (-) = Int64.sub

let chunk_min_size = ref 65000

let split_chunk c pos =
  let cc = {
    created = None;
    pos = pos;
    len = c.len - pos;
   } in
  c.next <- Some cc;
  c.len <- (pos - c.pos)

let rec extend_chunk c size = 
  match c.next with
    None -> 
     lprintf "Cannot extend last chunk\n";
     assert false
  | Some cc ->
     if cc.created <> None then
       begin
         append_chunk c cc;
         remove_chunk cc;
         c.next <- cc.next;
         c.len <- (cc.pos + cc.len) - c.pos;
       end
     else
     if cc.len >= size + !chunk_min_size then 
       begin
         split_chunk cc (cc.pos + size);
         extend_chunk c size;
       end 
     else
       begin
         c.next <- cc.next;
         c.len <- (cc.pos + cc.len) - c.pos;
         ftruncate_chunk c (c.len + cc.len);
       end

let open_chunk c = ()
let create_chunk c = () 

let get_chunk t pos len = 
  let c = t.chunks in
  let rec iter c =

    if c.created <> None && c.pos <= pos && c.pos + c.len >= pos + len then
       (open_chunk c, pos - c.pos)
    else
    if c.created <> None && pos <= c.pos + c.len + !chunk_min_size then
      begin
        extend_chunk c (maxi !chunk_min_size (pos+len - c.pos -c.len + !chunk_min_size));
        iter c
      end
    else
    if c.pos + c.len <= pos then
      match c.next with
      | None -> 
         lprintf "Invalid access in file pos %Ld is after last chunk\n" pos;
         assert false
      | Some c -> iter c
    else
    if c.pos + !chunk_min_size < pos then 
      begin
        split_chunk c pos;
        iter c     
      end
    else
    if c.pos + c.len > pos + (maxi len !chunk_min_size) + !chunk_min_size then
      begin
        split_chunk c (pos+(maxi len !chunk_min_size)+!chunk_min_size);
        iter c
      end
    else 
      begin
        create_chunk c;
        iter c
      end
  in
  iter c

let build t =
  if t.chunks.created = None then create_chunk c;
  while t.chunks.next <> None do
    extend_chunk c t.size
  done;
  


