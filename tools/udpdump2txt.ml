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
      lprintf "%s" header; lprint_newline ();
      lprintf "MESSAGE SIZE: %d" len; lprint_newline ();
      lprintf "ascii: [";
      for i = 0 to len - 1 do
        let c = s.[i] in
        let n = int_of_char c in
        if n > 31 && n < 127 then
          lprintf " %c" c
        else
          lprintf "(%d)" n
      done;
      lprintf "]\n";
      lprintf "dec: [";
      for i = 0 to len - 1 do
        let c = s.[i] in
        let n = int_of_char c in
        lprintf "(%d)" n            
      done;
      lprintf "]\n\n"
    end

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
            lprintf "DISCARD [%s]" line;
            lprint_newline ();
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
      lprintf "Exception %s" (Printexc2.to_string e);lprint_newline ();
      dump !header (Buffer.contents buf)
      
      