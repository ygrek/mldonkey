type xml = 
	| Element of (string * (string * string) list * xml list)
	| PCData of string
  
type error_pos = {
	eline : int;
	eline_start : int;
	emin : int;
	emax : int;
}

type error_msg =
	| UnterminatedComment
	| UnterminatedString
	| UnterminatedEntity
	| IdentExpected
	| CloseExpected
	| NodeExpected
	| AttributeNameExpected
	| AttributeValueExpected
	| EndOfTagExpected of string
	| EOFExpected

  
  
  
  
(********** DTD **********)

type dtd_child =
	| DTDTag of string
	| DTDPCData
	| DTDOptional of dtd_child
	| DTDZeroOrMore of dtd_child
	| DTDOneOrMore of dtd_child
	| DTDChoice of dtd_child list
	| DTDChildren of dtd_child list

type dtd_element_type =
	| DTDEmpty
	| DTDAny
	| DTDChild of dtd_child

type dtd_attr_default =
	| DTDDefault of string
	| DTDRequired
	| DTDImplied
	| DTDFixed of string

type dtd_attr_type =
	| DTDCData
	| DTDNMToken
	| DTDEnum of string list

type dtd_item =
	| DTDAttribute of string * string * dtd_attr_type * dtd_attr_default
	| DTDElement of string * dtd_element_type

type dtd = dtd_item list

type checked = {
	c_elements : (string,dtd_element_type) Hashtbl.t;
	c_attribs : (string,(string,(dtd_attr_type * dtd_attr_default)) Hashtbl.t) Hashtbl.t;
}

(********** LEXER **********)
  
  
type error = error_msg * error_pos

type lexer_error =
	| EUnterminatedComment
	| EUnterminatedString
	| EIdentExpected
	| ECloseExpected
	| ENodeExpected
	| EAttributeNameExpected
	| EAttributeValueExpected
	| EUnterminatedEntity

type dtd_error =
	| EInvalidDTDDecl
	| EInvalidDTDTag
	| EDTDItemExpected
	| EInvalidDTDElement
	| EInvalidDTDAttribute

exception Error of lexer_error
exception DTDError of dtd_error

type dtd_decl =
	| DTDFile of string
	| DTDData of dtd

type pos = int * int * int * int

(********  WHY ???? it's a shame to use global vars for that ! *)
let last_pos = ref 0
and current_line = ref 0
and current_line_start = ref 0

let pos lexbuf =
  !current_line ,	!current_line_start ,
  !last_pos ,
  Lexing.lexeme_start lexbuf
  
 