(* moved from xml.ml *)

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

(* moved from dtd.ml, renamed to xml_dtd.ml *)

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
        | DTDID
        | DTDIDRef

type dtd_item =
        | DTDAttribute of string * string * dtd_attr_type * dtd_attr_default
        | DTDElement of string * dtd_element_type

type dtd = dtd_item list

type ('a,'b) hash = ('a,'b) Hashtbl.t

type checked = {
        c_elements : (string,dtd_element_type) hash;
        c_attribs : (string,(string,(dtd_attr_type * dtd_attr_default)) hash) hash;
}

(* moved from xml_lexer.mll *)
  
  
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
