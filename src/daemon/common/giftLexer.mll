

{
  open GiftParser
    
  let buf = Buffer.create 100
    
  let init_string () =
    Buffer.reset buf
  let add_char c = 
    Buffer.add_char buf c
    
}

let firstchar = [ 'a'-'z' 'A'-'Z' ]
let secondchar = [ 'a'-'z' 'A'-'Z' '0'-'9' '_' ]
let space =  [' ' '\010' '\013' '\009' '\012']
  
rule lexer = parse
  space + { lexer lexbuf }
| "{"  { LBRACE }
| "}"  { RBRACE }
| "("  { 
    init_string ();
    string lexbuf 
  }  
| ";" { SEMI }
| firstchar secondchar* 
  { IDENT (Lexing.lexeme lexbuf) }
| eof { EOF }
| _ { failwith "Error during parsing" }
  
and string = parse
  "\\{" { add_char '{'; string lexbuf }
| "\\}" { add_char '}'; string lexbuf }
| "\\(" { add_char '('; string lexbuf }
| "\\)" { add_char ')'; string lexbuf }
| "\\;" { add_char ';'; string lexbuf }
| "\\\\" { add_char '\\'; string lexbuf }
| ")"   { STRING (Buffer.contents buf) }
| eof { failwith "Unexpected end of message in string" }
| [^ ')'] { Buffer.add_string buf (Lexing.lexeme lexbuf); string lexbuf }
    
  
