{
  
}


rule get_args = parse 
    ';'   {get_args lexbuf} 
  | [' ' '\t'] +   {get_args lexbuf} 
  | eof   { [] }
  | [ ^ ';' ' ' '\t'] [^ '='] * '=' { 
      let lexeme = Lexing.lexeme lexbuf in
      let name = String.sub lexeme 0 (String.length lexeme - 1) in
      let value, others = get_value lexbuf in
      (name, value) :: others
    } 

and get_value = parse
    ' ' + { get_value lexbuf }
  | '"' { let value = get_string lexbuf in
      value, get_args lexbuf }
  | [^ ' ' '"'] [ ^ ';' ] * {
      let value = Lexing.lexeme lexbuf in
      value, get_args lexbuf }
  | eof { "", [] }
    
and cut_line = parse 
    [ ^ ':' ]+ ':' {
    let lexeme = Lexing.lexeme lexbuf in
    let name = String.sub lexeme 0 (String.length lexeme - 1) in
    let value, args =  get_value lexbuf  in
    name, value, args
  }
  
and get_string = parse
    [ ^ '"' ] * '"' {
    let lexeme = Lexing.lexeme lexbuf in
    let value = String.sub lexeme 0 (String.length lexeme - 1) in
    value }
  
