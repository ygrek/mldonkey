
(* read a whole file *)
let to_string name =
  let chan = open_in name in
  let buf_size = 1024 in
  let buf = String.create buf_size in
  let cont = ref true in
  let rec iter buf nb_read =
    let buf_size = String.length buf in
    let tmp = input chan buf nb_read (buf_size - nb_read) in
    if tmp = 0 then 
      String.sub buf 0 nb_read
    else 
    let nb_read = nb_read + tmp in
    let buf =
      if nb_read = buf_size then 
        String2.resize buf (2 * buf_size)
      else buf
    in
    iter buf nb_read
  in
  let buf = iter buf 0 in
  close_in chan;
  buf
  
let from_string name s =
  let oc = open_out name in
  output_string oc s;
  close_out oc
  
let iter f name =
  let ic = open_in name in
  try
    while true do
      let line = input_line ic in
      f line
    done
  with 
    End_of_file -> close_in ic
  | e -> close_in ic; raise e
  
    
let from_value name s =
  let oc = open_out name in
  output_value oc s;
  close_out oc
  
let to_value name =
  let ic = open_in name in
  try
    let v = input_value ic in
    close_in ic;
    v
  with 
  | e -> close_in ic; raise e
      