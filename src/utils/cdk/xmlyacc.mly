/* xml light - (c)2002 Nicolas Cannasse		*/
/* http://warplayer.free.fr					*/
/* read LICENCE for additional informations */

%{
open Xmltypes
%}
%token EOF
%token <string> STRING
%token <string> IDENT
%token OPEN
%token CLOSE
%token UNARYCLOSE
%token END
%token EQ
%start main
%type <Xmltypes.xml> main
%%
main:
	| xml EOF	{ $1 }
;
xml: 
	| OPEN id attribs CLOSE childs {
		let result,context = $5 in
		if $2 <> context then raise Parsing.Parse_error;
		XML ($2,$3,result)
		}
	| OPEN id attribs UNARYCLOSE {
		XML ($2,$3,[])
		}	
;
childs:
	| xml childs { let result,context = $2 in (($1)::result,context) }
	| END id CLOSE { ([],$2) }
;
attribs:
	| { [] }
	| attribs attrib	{ $1@[$2] }
;
attrib:
	| IDENT EQ STRING { ($1,$3) }
;
id:
	| IDENT { $1 }
;
