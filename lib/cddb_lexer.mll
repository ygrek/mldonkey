{
  
} 

rule line = parse
    '#' { comment lexbuf }
  | eof { "","" }
  | [ 'a'-'z' 'A'-'Z' '0'-'9'] + '=' { 
      let name = Lexing.lexeme lexbuf in
      let name = String.sub name 0 (String.length name - 1) in
      let value = end_of_line lexbuf in
      name, value
    }
    
and comment = parse
    [' '  '\t'] + { comment lexbuf }
  | '\n' { line lexbuf }
  | ['0'-'9'] + { 
      let v = Lexing.lexeme lexbuf in
      let _ = end_of_line lexbuf in
      "TRACKPOS", v
    }
  | "Disc length:" { 
      Printf.printf "dlen"; print_newline ();get_length lexbuf }
  | _ { let s = end_of_line lexbuf in
      Printf.printf "DISCARD [%s]" s; print_newline ();
      line lexbuf }
    
and get_length = parse
  | ['0'-'9'] + { 
      let v = Lexing.lexeme lexbuf in
      let _ = end_of_line lexbuf in
      "DISCLENGTH", v
    }
  |  [' '  '\t'] + { get_length lexbuf }
    
and end_of_line = parse
    [ ^ '\n' ]* { let v = Lexing.lexeme lexbuf in
    let _ = get_end_of_line lexbuf in
    v }

  
and get_end_of_line = parse 
    '\n' { () }
  | eof { () }
    
