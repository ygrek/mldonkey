open Unix
  
let list_directory filename =
  let dir = opendir filename in
  let list = ref [] in
  try
    while true do
      let file = readdir dir in 
      if file <> "." && file <> ".." then begin
          list := file :: !list 
        end;
    done;
    assert false
  with _ -> 
      closedir dir;
      !list

let is_directory filename =
  try let s = Unix.stat filename in s.st_kind = S_DIR with _ -> false

let is_link filename =
  try let s = Unix.lstat filename in s.st_kind = S_LNK with _ -> false
