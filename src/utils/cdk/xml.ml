(* xml light - (c)2002 Nicolas Cannasse		*)
(* http://warplayer.free.fr					*)
(* read LICENCE for additional informations *)

type xml = Xmltypes.xml = XML of string * (string * string) list * xml list

exception Xml_error of string

let parse lexbuf =
	try
		(Xmlyacc.main Xmllex.xml lexbuf)
	with
		Parsing.Parse_error -> raise (Xml_error (Printf.sprintf "Parse error line %d characters %d-%d" !Xmllex.curline !Xmllex.pmin !Xmllex.pmax))

let parse_string s =
	parse (Lexing.from_string s)

let parse_file f =
	let chan = (open_in_bin f) in
	try
		let x = parse (Lexing.from_channel chan) in
		close_in chan;
		x
	with e -> close_in chan; raise e
		

let get_name x =
	let XML (name,_,_) = x in
	name

let get_attrib name x =
	let XML(_,attribs,_) = x in
	(List.assoc name attribs)

let get_childs x = 
	let XML (_,_,childs) = x in
	childs

let rec to_string x =
	let XML (name,attribs,childs) = x in
	let head = (match attribs with
		[] -> name
		| _ -> name^" "^(String.concat " " (List.map (fun (a,b) ->
                   Printf.sprintf "%s=\"%s\"" a b) attribs))) in
	(match childs with
		[] -> Printf.sprintf "<%s/>" head
		| _ -> Printf.sprintf "<%s> %s </%s>" head (String.concat "" (List.map
                    to_string childs)) name)
				
