(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

{
  type token =
      Lcomment  (* <!-- *)
    | Rcomment  (* --> *)
    | Mcomment  (* within comment *)
    | Ldoctype  (* <! *)
    | Rdoctype  (* > *)
    | Mdoctype  (* within declaration *)
    | Lpi       (* <? *)
    | Rpi       (* ?> or > *)
    | Mpi       (* within processing instruction *)
    | Lelement of string
    | Lelementend of string
    | Relement
    | Cdata of string 
    | Space of int
    | Name of string
    | Is
    | Literal of string
    | Other
    | Eof
}

(* Simplified rules: Only ASCII is recognized as character set *)

let letter = ['A'-'Z' 'a'-'z' ]
let digit = ['0'-'9']
let hexdigit = ['0'-'9' 'A'-'F' 'a'-'f']
let namechar = letter | digit | '.' | ':' | '-' | '_'
let name = ( letter | '_' | ':' ) namechar*
let nmtoken = namechar+
let ws = [ ' ' '\t' '\r' '\n' ]
let string_literal1 = '"' [^ '"' ]* '"'
let string_literal2 = "'" [^ '\'' ]* "'"
let string_literal3 = [^ '"' '\'' '>' '=' ' ' '\t' '\n' '\r' ]+

(* This following rules reflect HTML as it is used, not the SGML
 * rules.
 *)

rule scan_document = parse
  | "<!--"
      { Lcomment }
  | "<!"
      { Ldoctype }
  | "<?"
      { Lpi }
  | "<" name
      { let s = Lexing.lexeme lexbuf in
        Lelement (String.sub s 1 (String.length s - 1))
      }
  | "</" name
      { let s = Lexing.lexeme lexbuf in
        Lelementend (String.sub s 2 (String.length s - 2))
      }
  | "<"                (* misplaced "<" *)
      { Cdata "<" }
  | eof
      { Eof }
  | [^ '<' ]+
      { Cdata (Lexing.lexeme lexbuf)}

and scan_special = parse
  | "</" name 
      { let s = Lexing.lexeme lexbuf in
        Lelementend (String.sub s 2 (String.length s - 2))
      }
  | "<"
      { Cdata "<" }
  | eof
      { Eof }
  | [^ '<' ]+
      { Cdata (Lexing.lexeme lexbuf)}


and scan_comment = parse
  | "-->"
      { Rcomment }  (* FIXME: There may be any number of ws between -- and > *)
  | "-"
      { Mcomment }
  | eof
      { Eof }
  | [^ '-']+
      { Mcomment }

and scan_doctype = parse
  | ">"                   (* Occurence in strings, and [ ] brackets ignored *)
      { Rdoctype }
  | eof
      { Eof }
  | [^ '>' ] +
      { Mdoctype }

and scan_pi = parse
  | "?>"
      { Rpi }
  | ">"
      { Rpi }
  | eof
      { Eof }
  | '?' 
      { Mpi }
  | [^ '>' '?' ] +
      { Mpi }

and scan_element = parse
  | ">"
      { Relement }
  | ws+
      { Space (String.length (Lexing.lexeme lexbuf)) }
  | name
      { Name (Lexing.lexeme lexbuf) }
  | "="
      { Is }
  | string_literal1
      { let s = Lexing.lexeme lexbuf in
        Literal (String.sub s 1 (String.length s - 2)) 
      }
  | string_literal2
      { let s = Lexing.lexeme lexbuf in
        Literal (String.sub s 1 (String.length s - 2)) 
      }
  | string_literal3
      { Literal (Lexing.lexeme lexbuf) }
  | eof
      { Eof }
  | _
      { Other }

(* ======================================================================
 * History:
 * 
 * $Log$
 * Revision 1.1  2005/10/31 18:34:02  spiralvoice
 * patch #4583
 *
 * Revision 2.2  2002/07/06 16:24:44  stolpmann
 * 	Change: Only ASCII letters are accepted in names.
 *
 * Revision 2.1  2001/09/14 14:22:34  stolpmann
 * 	Initial revision (sourceforge)
 *
 *
 * ======================================================================
 * Revision 1.4  2001/07/15 14:18:34  gerd
 * 	Attribute parsing is now less strict.
 *
 * Revision 1.3  2001/06/08 16:25:27  gerd
 * 	Bugfix: </SCRIPT> is now recognized (thanks to David Fox)
 * 	The parser may now return comments, declarations, and processing
 * instructions if requested to do so
 * 	The parser accepts xhtml to some extent
 * 	Now exported: parse_document.
 *
 * Revision 1.2  2001/04/24 19:27:00  gerd
 * 	Added a FIXME comment
 *
 * Revision 1.1  2000/03/03 01:07:25  gerd
 * 	Initial revision.
 *
 * 
 *)
