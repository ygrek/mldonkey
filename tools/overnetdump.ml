let i_a = int_of_char 'a'  
let i_A = int_of_char 'A'  
let i_f = int_of_char 'f'  
let i_F = int_of_char 'F'  
let i_0 = int_of_char '0'
let i_9 = int_of_char '9'

let digit_hexa c =
  let i = int_of_char c in
  if i >= i_a && i <= i_f then i - i_a + 10 else
  if i >= i_A && i <= i_F then i - i_A + 10 else
  if i >= i_0 && i <= i_9 then i - i_0 else
    failwith "Bad hexa char"
    
let dump header s =
  let len = String.length s in
  if len > 0 then begin
      Printf.printf "%s" header; print_newline ();
      Printf.printf "MESSAGE SIZE: %d" len; print_newline ();
      Printf.printf "ascii: [";
      for i = 0 to len - 1 do
        let c = s.[i] in
        let n = int_of_char c in
        if n > 31 && n < 127 then
          Printf.printf " %c" c
        else
          Printf.printf "(%d)" n
      done;
      Printf.printf "]\n";
      Printf.printf "dec: [";
      for i = 0 to len - 1 do
        let c = s.[i] in
        let n = int_of_char c in
        Printf.printf "(%d)" n            
      done;
      Printf.printf "]\n\n"
    end

let dump header s =
  let len = String.length s in
  if len >= 2 && int_of_char s.[0] = 227 then
    let opcode = int_of_char s.[1] in
    begin
      match opcode with
      | 10 -> Printf.printf "OK: CONNECT MESSAGE"; print_newline ();
      | 11 -> Printf.printf "OK: CONNECT REPLY"; print_newline ();
      | 14 -> Printf.printf "OK: SEARCH MESSAGE"; print_newline ();
      | 15 -> Printf.printf "OK: SEARCH REPLY"; print_newline ();
      | 16 -> Printf.printf "OK: SEARCH GET REPLIES"; print_newline ();
      | 17 -> Printf.printf "OK: ONE REPLY"; print_newline ();
      | _ ->
          Printf.printf "UNKNOWN: opcode %d" opcode; print_newline ();
    end;
    dump header s

let _ =
  let header = ref "" in
  let buf = Buffer.create 1000 in
  try
    let left = ref 0 in
    let rec iter pos line len =
      if pos+1 < len then
        try
          let v = digit_hexa line.[pos] in
          let vv = digit_hexa line.[pos+1] in
          let v = v * 16 + vv in
          if !left = 0 then
            Buffer.add_char buf (char_of_int v)
          else
            decr left;
          iter (pos+2) line len
        with _ -> iter (pos+1) line len
    in
    while true do
      let line = input_line stdin in
      let len = String.length line in
      if len > 2 && line.[0] = '0' && line.[1] = 'x' then
        let pos = String.index line '\t' in
        let end_pos = String.rindex line '\t' in
        let len = end_pos - pos - 2 in
        (*
        if !left > 100000 then begin
            Printf.printf "DISCARD [%s]" line;
            print_newline ();
          end; *)
        let line = String.sub line (pos+2) len in
        iter 0 line len;
      else begin
          dump !header (Buffer.contents buf);
          left := 28;
          Buffer.clear buf;        
          header := line
        end
    done
  with e -> 
      Printf.printf "Exception %s" (Printexc2.to_string e);print_newline ();
      dump !header (Buffer.contents buf)
      
