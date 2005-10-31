(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

(** Parsing of HTML *)


(** The type [document] represents parsed HTML documents:
 *
 * {ul
 * {- [Element (name, args, subnodes)] is an element node for an element of
 *   type [name] (i.e. written [<name ...>...</name>]) with arguments [args]
 *   and subnodes [subnodes] (the material within the element). The arguments
 *   are simply name/value pairs. Entity references (something like [&xy;])
 *   occuring in the values are {b not} resolved.
 *
 *   Arguments without values (e.g. [<select name="x" multiple>]: here,
 *   [multiple] is such an argument) are represented as [(name,name)], i.e. the
 *   name is also returned as value.
 *
 *   As argument names are case-insensitive, the names are all lowercase.}
 * {- [Data s] is a character data node. Again, entity references are contained
 *   as such and not as what they mean.}
 * }
 *
 * Character encodings: The parser is restricted to ASCII-compatible
 * encodings (see the function {!Netconversion.is_ascii_compatible} for
 * a definition). In order to read other encodings, the text must be
 * first recoded to an ASCII-compatible encoding (example below).
 * Names of elements and attributes must additionally be ASCII-only.
 *)
type document =
    Element of (string  *  (string*string) list  *  document list)
  | Data of string
;;


(** We also need a type that declares how to handle the various tags.
 * This is called a "simplified DTD", as it is derived from SGML DTDs,
 * but simplified to the extent used in the HTML definition.
 *)

(* Now follows the type definition of simplified DTDs. *)

type element_class =         (* What is the class of an element? *)
  [ `Inline
  | `Block
  | `Essential_block
  | `None
  | `Everywhere
  ]
;;
(** Element classes are a property used in the HTML DTD. For our purposes,
 * we define element classes simply as an enumeration:
 * - [`Inline] is the class of inline HTML elements
 * - [`Block] is the class of block HTML elements
 * - [`Essential_block] is a sub-class of [`Block] with the additional
 *   property that every start tag must be explicitly ended
 * - [`None] means that the members of the class are neither block nor
 *   inline elements, but have to be handled specially
 * - [`Everywhere] means that the members of the class can occur everywhere, 
 *   regardless of whether a constraint allows it or not.
 *)


type model_constraint =      (* The constraint the subelements must fulfill *)
  [ `Inline
  | `Block
  | `Flow                                            (* = `Inline or `Block *)
  | `Empty
  | `Any
  | `Special
  | `Elements of string list             (* Enumeration of allowed elements *)
  | `Or of (model_constraint * model_constraint)
  | `Except of (model_constraint * model_constraint)
  | `Sub_exclusions of (string list * model_constraint)
  ]
;;
(** Model constraints define the possible sub elements of an element:
 * - [`Inline]: The sub elements must belong to the class [`Inline]
 * - [`Block]: The sub elements must be members of the classes [`Block] or 
 *   [`Essential_block]
 * - [`Flow]: The sub elements must belong to the classes [`Inline], [`Block],
 *   or [`Essential_block]
 * - [`Empty]: There are no sub elements
 * - [`Any]: Any sub element is allowed
 * - [`Special]: The element has special content (e.g. [<script>]).
 *   Functionally equivalent to [`Empty]
 * - [`Elements l]: Only these enumerated elements may occur as sub elements
 * - [`Or(m1,m2)]: One of the constraints [m1] or [m2] must hold
 * - [`Except(m1,m2)]: The constraint [m1] must hold, and [m2] must not hold
 * - [`Sub_exclusions(l,m)]: The constraint [m] must hold; furthermore, 
 *   the elements enumerated in list [l] are not allowed as direct or
 *   indirect subelements, even if [m] or the model of a subelement would
 *   allow them. The difference to [`Except(m, `Elements l)] is that the
 *   exclusion is inherited to the subelements. The [`Sub_exclusions]
 *   expression must be toplevel, i.e. it must not occur within an [`Or], 
 *   [`Except], or another ['Sub_exclusions] expression.
 *
 * Note that the members of the class [`Everywhere] are allowed everywhere,
 * regardless of whether the model constraint allows them or not.
 *
 * Note that certain aspects are not modeled:
 * - [#PCDATA]: We do not specify where PCDATA is allowed and where not.
 * - Order, Number: We do neither specify in which order the sub elements must
 *   occur nor how often they can occur
 * - Inclusions: DTDs may describe that an element extraordinarily
 *   allows a list of elements in all sub elements. 
 * - Optional tags: Whether start or end tags can be omitted (to some extent,
 *   this can be expressed with [`Essential_block], however)
 *)

type simplified_dtd =
    (string * (element_class * model_constraint)) list;;
(** A [simplified_dtd] is an associative list of tuples
 *  [(element_name, (element_class, constraint))]: For every [element_name]
 *  it is declared that it is a member of [element_class], and that
 *  the sub elements must satisfy [constraint].
 *
 *  It is not allowed to have several entries for the same element.
 *)

val html40_dtd : simplified_dtd
  (** The (transitional) HTML 4.0 DTD, expressed as [simplified_dtd] *)

val relaxed_html40_dtd : simplified_dtd
  (** A relaxed version of the HTML 4.0 DTD that matches better common
   * practice. In particular, this DTD additionally allows that inline
   * elements may span blocks. For example, 
   * {[ <B>text1 <P>text2 ]}
   * is parsed as
   * {[ <B>text1 <P>text2</P></B> ]}
   * and not as
   * {[ <B>text1 </B><P>text2</P> ]}
   * \- the latter is more correct (and parsed by [html40_dtd]), but is not what
   * users expect.
   *
   * Note that this is still not what many browsers implement. For example,
   * Netscape treats most inline tags specially: [<B>] switches bold on,
   * [</B>] switches bold off. For example,
   * {[ <A href='a'>text1<B>text2<A href='b'>text3 ]}
   * is parsed as
   * {[ <A href='a'>text1<B>text2</B></A><B><A href='b'>text3</A></B> ]}
   * \- there is an extra [B] element around the second anchor! (You can
   * see what Netscape parses by loading a page into the "Composer".)
   * IMHO it is questionable to consider inline tags as switches because
   * this is totally outside of the HTML specification, and browsers may
   * differ in that point.
   *
   * Furthermore, several elements are turned into essential blocks:
   * [TABLE], [UL], [OL], and [DL]. David Fox reported a problem with structures
   * like:
   * {[ <TABLE><TR><TD><TABLE><TR><TD>x</TD></TD></TR></TABLE>y</TD></TR></TABLE> ]}
   * i.e. the [TD] of the inner table has two end tags. Without additional
   * help, the second [</TD>] would close the outer table cell. Because of
   * this problem, tables are now essential meaning that it is not allowed
   * to implicitly add a missing [</TABLE>]; every table element has to
   * be explicitly ended. This rule seems to be what many browsers implement.
   *)

val parse_document : ?dtd:simplified_dtd ->            (* default: html40_dtd *)
                     ?return_declarations:bool ->      (* default: false *)
                     ?return_pis:bool ->               (* default: false *)
                     ?return_comments:bool ->          (* default: false *)
                     Lexing.lexbuf ->
                       document list
  (** Parses the HTML document from a [lexbuf] and returns it. 
   * 
   * @param dtd specifies the DTD to use. By default, [html40_dtd] is used which
   *   bases on the transitional HTML 4.0 DTD
   * @param return_declarations if set, the parser returns [<!...>] declarations
   *   as [Element("!",["contents",c],[])] nodes, where [c] is the string inside
   *   [<!] and [>]. - By default, declarations are skipped.
   * @param return_pis if set, the parser returns [<?...>] (or [<?...?>]) processing
   *   instructions as [Element("?",["contents",c],[])] nodes, where [c] is the
   *   string inside [<?] and [>] (or [?>]). - By default, processing instructions
   *   are skipped.
   * @param return_comments if set, the parser returns [<!--] .... [-->] comments
   *   as [Element("--",["contents",c],[])] nodes, where [c] is the string inside
   *   [<!--] and [-->]. - By default, comments are skipped.
   *)

(** {b Note on XHTML}
 *
 * The parser can read XHTML, as long as the following XML features are not
 * used:
 * - Internal DTD subset, i.e. [<!DOCTYPE html ... [ ... ]>]
 * - External entities
 * - [<!\[CDATA\[]
 * - [<!\[INCLUDE\[]
 * - [<!\[IGNORE\[]
 *
 * The following XML features are ok:
 * - Processing instructions
 * - Empty elements (e.g. [<br/>]) as long as the element is declared as 
 *   [`Empty].
 *)

(** {b Note on Character Encodings}
 *
 * The parser can only read character streams that are encoded in an ASCII-
 * compatible way. For example, it is possible to read a UTF-8-encoded
 * stream, but not a UTF-16-encoded stream. All bytes between 1 and 127
 * are taken as ASCII, and other bytes are ignored (copied from input
 * to output).
 *
 * Non-ASCII-compatible streams must be recoded first. For example, to
 * read a UTF-16-encoded netchannel [ch], use:
 *
 * {[
 * let p = 
 *   new Netconversion.recoding_pipe ~in_enc:`Enc_utf16 ~out_enc:`Enc_utf8 () in
 * let ch' =
 *   new Netchannels.input_filter ch p in
 * let doc =
 *   Nethtml.parse ch' in
 * ch' # close_in();
 * ch # close_in();
 * ]}
 *)

val map_list : (string -> string) -> document list -> document list
  (** [map_list f doclst]:
   * Applies [f] to all attribute values and data strings (except
   * the attributes of "?", "!", or "--" nodes). 
   *
   * This can be used to change the text encoding of a parsed document:
   * {[
   * let doc' = map_list String.lowercase doc
   * ]}
   * converts all text data to lowercase characters. 
   *)

val print_document : document list -> string
