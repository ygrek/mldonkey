
let find_in_path path name =
  if not (Filename.is_implicit name) then
    if Sys.file_exists name then name else raise Not_found
  else begin
    let rec try_dir = function
        [] -> raise Not_found
      | dir::rem ->
          let fullname = Filename.concat dir name in
          if Sys.file_exists fullname then fullname else try_dir rem
    in try_dir path
  end
      
let string_to_path str =
  let len = String.length str in
  let rec iter start pos =
    if pos >= len then
      [String.sub str start (len - start)]
    else
      if str.[pos] = ' ' || str.[pos] = ':' then
        (String.sub str start (pos - start)) :: (iter (pos+1) (pos+1))
      else
        iter start (pos+1)
  in
  iter 0 0

let path_to_string path =
  let s =   List.fold_left (fun str dir -> str ^ ":" ^ dir) "" path in
  if String.length s > 0 then 
    let len = String.length s in
    String.sub s 1 (len-1)
  else ""
  