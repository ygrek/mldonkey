(* xml light - (c)2002 Nicolas Cannasse		*)
(* http://warplayer.free.fr					*)
(* read LICENCE for additional informations *)
{

open Xmlyacc

let curline = ref 1
let sline = ref 0
let pmin = ref 0
let pmax = ref 0
let curstr = ref ""

let reset () = curstr := ""
let set s = curstr := s

let newline lexbuf =
	incr curline;
	sline := Lexing.lexeme_start lexbuf

let get = Lexing.lexeme
let update s =
	pmin := (Lexing.lexeme_start s) - !sline + 1;
	pmax := (Lexing.lexeme_end s) - !sline + 1

let error buf =
	update buf;
	raise Parsing.Parse_error

}

let space = [' ' '\t' '\r']
let ident = ['a'-'z' 'A'-'Z' '_' ':']['a'-'z' 'A'-'Z' '0'-'9' '.' '-' '_' ':']*

rule xml = parse
	| eof	{ EOF }
	| '\n'	{ newline lexbuf; update lexbuf; xml lexbuf }
	| space+{ update lexbuf; xml lexbuf }
	| '<'	{ OPEN }
	| '>'	{ CLOSE }
	| "</"	{ END }
	| "/>"  { UNARYCLOSE }
	| '='	{ EQ }
	| '"'	{ reset(); STRING(dq_string lexbuf) }
	| '\''	{ reset(); STRING(q_string lexbuf) }
	| "<--" { com_string lexbuf; }
	| "<?"   { quest_string lexbuf }
	| "<%"	{ code_string lexbuf; }
	| "<!"	{ doc_string lexbuf; }
	| ident	{ update lexbuf; IDENT(get lexbuf) }
	| _		{ error lexbuf; }
and
	q_string = parse
	| '\''	{ !curstr; }
	| eof	{ error lexbuf; }
	| '\n'	{ error lexbuf; }
	| [^'\'' '\n']+ { set (get lexbuf); q_string lexbuf; }
	| _		{ error lexbuf; }
and
	dq_string = parse
	| '"'	{ !curstr; }
	| eof	{ error lexbuf; }
	| '\n'	{ error lexbuf; }
	| [^'"' '\n']+ { set (get lexbuf); dq_string lexbuf; }
	| _		{ error lexbuf; }
and
	com_string = parse
	| "-->"		{ xml lexbuf; }
  | "?>"		{       xml lexbuf; }
	| [^'-']+	{ com_string lexbuf; }
	| _			{ error lexbuf; }
and
	quest_string = parse
     | "?>"		{   xml lexbuf; }
	| [^'?']+	{ quest_string lexbuf; }
	| _			{ error lexbuf; }
and
	code_string = parse
	| "%>"		{ xml lexbuf; }
	| [^'%']+	{ code_string lexbuf; }
	| _			{ error lexbuf; }
and
	doc_string = parse
	| ">"		{ xml lexbuf; }
	| [^'>']+	{ doc_string lexbuf; }
	| _			{ error lexbuf; }
