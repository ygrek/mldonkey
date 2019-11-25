(** Bitstring persistent patterns. *)
(* Copyright (C) 2008 Red Hat Inc., Richard W.M. Jones
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version,
 * with the OCaml linking exception described in COPYING.LIB.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 *
 * $Id$
 *)

(**
   {b Warning:} This documentation is for ADVANCED USERS ONLY.
   If you are not an advanced user, you are probably looking
   for {{:Bitstring.html}the Bitstring documentation}.

   {{:#reference}Jump straight to the reference section for
   documentation on types and functions}.

   {2 Introduction}

   Bitstring allows you to name sets of fields and reuse them
   elsewhere.  For example if you frequently need to parse
   Pascal-style strings in the form length byte + string, then you
   could name the [{ strlen : 8 : int; str : strlen*8 : string }]
   pattern and reuse it everywhere by name.

   These are called {b persistent patterns}.

   The basic usage is:

{v
(* Create a persistent pattern called 'pascal_string' which
 * matches Pascal-style strings (length byte + string).
 *)
let bitmatch pascal_string =
  \{ strlen : 8 : int;
    str : strlen*8 : string }

let is_pascal_string bits =
  bitmatch bits with
  | \{ :pascal_string } ->
    printf "matches a Pascal string %s, len %d bytes\n"
      str strlen
v}

   or:

{v
(* Load a persistent pattern from a file. *)
open bitmatch "pascal.bmpp"

let is_pascal_string bits =
  bitmatch bits with
  | \{ :pascal_string } ->
    printf "matches a Pascal string %s, len %d bytes\n"
      str strlen
v}

   {3 Important notes}

   There are some important things you should know about
   persistent patterns before you decide to use them:

   'Persistent' refers to the fact that they can be saved into binary
   files.  However these binary files use OCaml [Marshal] module and
   depend (sometimes) on the version of OCaml used to generate them
   and (sometimes) the version of bitstring used.  So your build system
   should rebuild these files from source when your code is rebuilt.

   Persistent patterns are syntactic.  They work in the same way
   as cutting and pasting (or [#include]-ing) code.  For example
   if a persistent pattern binds a field named [len], then any
   uses of [len] following in the surrounding pattern could
   be affected.

   Programs which generate and manipulate persistent patterns have to
   link to camlp4.  Since camlp4 in OCaml >= 3.10 is rather large, we
   have placed this code into this separate submodule, so that
   programs which just use bitstring don't need to pull in the whole of
   camlp4.  This restriction does not apply to code which only uses
   persistent patterns but does not generate them.  If the distinction
   isn't clear, use [ocamlobjinfo] to look at the dependencies of your
   [*.cmo] files.

   Persistent patterns can be generated in several ways, but they
   can only be {i used} by the [pa_bitstring] syntax extension.
   This means they are purely compile-time constructs.  You
   cannot use them to make arbitrary patterns and run those
   patterns (not unless your program runs [ocamlc] to make a [*.cmo]
   file then dynamically links to the [*.cmo] file).

   {2 Named patterns}

   A named pattern is a way to name a pattern and use it later
   in the same source file.  To name a pattern, use:

   [let bitmatch name = { fields ... } ;;]

   and you can then use the name later on inside another pattern,
   by prefixing the name with a colon.
   For example:

   [bitmatch bits with { :name } -> ...]

   You can nest named patterns within named patterns to any depth.

   Currently the use of named patterns is somewhat limited.
   The restrictions are:

   Named patterns can only be used within the same source file, and
   the names occupy a completely separate namespace from anything
   else in the source file.

   The [let bitmatch] syntax only works at the top level.  We may
   add a [let bitmatch ... in] for inner levels later.

   Because you cannot rename the bound identifiers in named
   patterns, you can effectively only use them once in a
   pattern.  For example, [{ :name; :name }] is legal, but
   any bindings in the first name would be overridden by
   the second name.

   There are no "named constructors" yet, but the machinery
   is in place to do this, and we may add them later.

   {2 Persistent patterns in files}

   More useful than just naming patterns, you can load
   persistent patterns from external files.  The patterns
   in these external files can come from a variety of sources:
   for example, in the [cil-tools] subdirectory are some
   {{:http://cil.sf.net/}Cil-based} tools for importing C
   structures from header files.  You can also generate
   your own files or write your own tools, as described below.

   To use the persistent pattern(s) from a file do:

   [open bitmatch "filename.bmpp" ;;]

   A list of zero or more {!named} patterns are read from the file
   and each is bound to a name (as contained in the file),
   and then the patterns can be used with the usual [:name]
   syntax described above.

   {3 Extension}

   The standard extension is [.bmpp].  This is just a convention
   and you can use any extension you want.

   {3 Directory search order}

   If the filename is an absolute or explicit path, then we try to
   load it from that path and stop if it fails.  See the [Filename]
   module in the standard OCaml library for the definitions of
   "absolute path" and "explicit path".  Otherwise we use the
   following directory search order:

   - Relative to the current directory
   - Relative to the OCaml library directory

   {3 bitstring-objinfo}

   The [bitstring-objinfo] command can be run on a file in order
   to print out the patterns in the file.

   {3 Constructors}

   We haven't implemented persistent constructors yet, although
   the machinery is in place to make this happen.  Any constructors
   found in the file are ignored.

   {2 Creating your own persistent patterns}

   If you want to write a tool to import bitstrings from an
   exotic location or markup language, you will need
   to use the functions found in the {{:#reference}reference section}.

   I will describe using an example here of how you would
   programmatically create a persistent pattern which
   matches Pascal-style "length byte + data" strings.
   Firstly note that there are two fields, so our pattern
   will be a list of length 2 and type {!pattern}.

   You will need to create a camlp4 location object ([Loc.t])
   describing the source file.  This source file is used
   to generate useful error messages for the user, so
   you may want to set it to be the name and location in
   the file that your tool reads for input.  By convention,
   locations are bound to name [_loc]:

   {v
   let _loc = Loc.move_line 42 (Loc.mk "input.xml")
   v}

   Create a pattern field representing a length field which is 8 bits wide,
   bound to the identifier [len]:

   {v
   let len_field = create_pattern_field _loc
   let len_field = set_length_int len_field 8
   let len_field = set_lident_patt len_field "len"
   v}

   Create a pattern field representing a string of [len*8] bits.
   Note that the use of [<:expr< >>] quotation requires
   you to preprocess your source with [camlp4of]
   (see {{:http://brion.inria.fr/gallium/index.php/Reflective_OCaml}this
   page on Reflective OCaml}).

   {v
   let str_field = create_pattern_field _loc
   let str_field = set_length str_field <:expr< len*8 >>
   let str_field = set_lident_patt str_field "str"
   let str_field = set_type_string str_field
   v}

   Join the two fields together and name it:

   {v
   let pattern = [len_field; str_field]
   let named_pattern = "pascal_string", Pattern pattern
   v}

   Save it to a file:

   {v
   let chan = open_out "output.bmpp" in
   named_to_channel chan named_pattern;
   close_out chan
   v}

   You can now use this pattern in another program like this:

   {v
   open bitmatch "output.bmpp" ;;
   let parse_pascal_string bits =
   bitmatch bits with
   | \{ :pascal_string } -> str, len
   | \{ _ } -> invalid_arg "not a Pascal string"
   v}

   You can write more than one named pattern to the output file, and
   they will all be loaded at the same time by [open bitmatch ".."]
   (obviously you should give each pattern a different name).  To do
   this, just call {!named_to_channel} as many times as needed.

   {2:reference Reference}

   {3 Types}
*)

type patt = Camlp4.PreCast.Syntax.Ast.patt
type expr = Camlp4.PreCast.Syntax.Ast.expr
type loc_t = Camlp4.PreCast.Syntax.Ast.Loc.t
(** Just short names for the camlp4 types. *)

type 'a field
(** A field in a persistent pattern or persistent constructor. *)

type pattern = patt field list
(** A persistent pattern (used in [bitmatch] operator), is just a
    list of pattern fields. *)

type constructor = expr field list
(** A persistent constructor (used in [BITSTRING] operator), is just a
    list of constructor fields. *)

type named = string * alt
and alt =
  | Pattern of pattern			(** Pattern *)
  | Constructor of constructor		(** Constructor *)
(** A named pattern or constructor.

    The name is used when binding a pattern from a file, but
    is otherwise ignored. *)

(** {3 Printers} *)

val string_of_pattern : pattern -> string
val string_of_constructor : constructor -> string
val string_of_pattern_field : patt field -> string
val string_of_constructor_field : expr field -> string
(** Convert patterns, constructors or individual fields
    into printable strings for debugging purposes.

    The strings look similar to the syntax used by bitmatch, but
    some things cannot be printed fully, eg. length expressions. *)

(** {3 Persistence} *)

val named_to_channel : out_channel -> named -> unit
(** Save a pattern/constructor to an output channel. *)

val named_to_string : named -> string
(** Serialize a pattern/constructor to a string. *)

val named_to_buffer : string -> int -> int -> named -> int
(** Serialize a pattern/constructor to part of a string, return the length. *)

val named_from_channel : in_channel -> named
(** Load a pattern/constructor from an output channel.

    Note: This is not type safe.  The pattern/constructor must
    have been written out under the same version of OCaml and
    the same version of bitstring. *)

val named_from_string : string -> int -> named
(** Load a pattern/constructor from a string at offset within the string.

    Note: This is not type safe.  The pattern/constructor must
    have been written out under the same version of OCaml and
    the same version of bitstring. *)

(** {3 Create pattern fields}

    These fields are used in pattern matches ([bitmatch]). *)

val create_pattern_field : loc_t -> patt field
(** Create a pattern field.

    The pattern is unbound, the type is set to [int], bit length to [32],
    endianness to [BigEndian], signedness to unsigned ([false]),
    source code location to the [_loc] parameter, and no offset expression.

    To create a complete field you need to call the [set_*]
    functions.  For example, to create [{ len : 8 : int }]
    you would do:

{v
    let field = create_pattern_field _loc in
    let field = set_lident_patt field "len" in
    let field = set_length_int field 8 in
v}
*)

val set_lident_patt : patt field -> string -> patt field
(** Sets the pattern to the pattern binding an identifier
    given in the string.

    The effect is that the field [{ len : 8 : int }] could
    be created by calling [set_lident_patt field "len"]. *)

val set_int_patt : patt field -> int -> patt field
(** Sets the pattern field to the pattern which matches an integer.

    The effect is that the field [{ 2 : 8 : int }] could
    be created by calling [set_int_patt field 2]. *)

val set_string_patt : patt field -> string -> patt field
(** Sets the pattern field to the pattern which matches a string.

    The effect is that the field [{ "MAGIC" : 8*5 : string }] could
    be created by calling [set_int_patt field "MAGIC"]. *)

val set_unbound_patt : patt field -> patt field
(** Sets the pattern field to the unbound pattern (usually written [_]).

    The effect is that the field [{ _ : 8 : int }] could
    be created by calling [set_unbound_patt field]. *)

val set_patt : patt field -> patt -> patt field
(** Sets the pattern field to an arbitrary OCaml pattern match. *)

val set_length_int : 'a field -> int -> 'a field
(** Sets the length in bits of a field to a constant integer.

    The effect is that the field [{ len : 8 : string }] could
    be created by calling [set_length field 8]. *)

val set_length : 'a field -> expr -> 'a field
(** Sets the length in bits of a field to an OCaml expression.

    The effect is that the field [{ len : 2*i : string }] could
    be created by calling [set_length field <:expr< 2*i >>]. *)

val set_endian : 'a field -> Bitstring.endian -> 'a field
(** Sets the endianness of a field to the constant endianness.

    The effect is that the field [{ _ : 16 : bigendian }] could
    be created by calling [set_endian field Bitstring.BigEndian]. *)

val set_endian_expr : 'a field -> expr -> 'a field
(** Sets the endianness of a field to an endianness expression.

    The effect is that the field [{ _ : 16 : endian(e) }] could
    be created by calling [set_endian_expr field e]. *)

val set_signed : 'a field -> bool -> 'a field
(** Sets the signedness of a field to a constant signedness.

    The effect is that the field [{ _ : 16 : signed }] could
    be created by calling [set_signed field true]. *)

val set_type_int : 'a field -> 'a field
(** Sets the type of a field to [int].

    The effect is that the field [{ _ : 16 : int }] could
    be created by calling [set_type_int field]. *)

val set_type_string : 'a field -> 'a field
(** Sets the type of a field to [string].

    The effect is that the field [{ str : 16 : string }] could
    be created by calling [set_type_string field]. *)

val set_type_bitstring : 'a field -> 'a field
(** Sets the type of a field to [bitstring].

    The effect is that the field [{ _ : 768 : bitstring }] could
    be created by calling [set_type_bitstring field]. *)

val set_location : 'a field -> loc_t -> 'a field
(** Sets the source code location of a field.  This is used when
    pa_bitstring displays error messages. *)

val set_offset_int : 'a field -> int -> 'a field
(** Set the offset expression for a field to the given number.

    The effect is that the field [{ _ : 8 : offset(160) }] could
    be created by calling [set_offset_int field 160]. *)

val set_offset : 'a field -> expr -> 'a field
(** Set the offset expression for a field to the given expression.

    The effect is that the field [{ _ : 8 : offset(160) }] could
    be created by calling [set_offset_int field <:expr< 160 >>]. *)

val set_no_offset : 'a field -> 'a field
(** Remove the offset expression from a field.  The field will
    follow the previous field, or if it is the first field will
    be at offset zero. *)

val set_check : 'a field -> expr -> 'a field
(** Set the check expression for a field to the given expression. *)

val set_no_check : 'a field -> 'a field
(** Remove the check expression from a field. *)

val set_bind : 'a field -> expr -> 'a field
(** Set the bind-expression for a field to the given expression. *)

val set_no_bind : 'a field -> 'a field
(** Remove the bind-expression from a field. *)

val set_save_offset_to : 'a field -> patt -> 'a field
(** Set the save_offset_to pattern for a field to the given pattern. *)

val set_save_offset_to_lident : 'a field -> string -> 'a field
(** Set the save_offset_to pattern for a field to identifier. *)

val set_no_save_offset_to : 'a field -> 'a field
(** Remove the save_offset_to from a field. *)

(** {3 Create constructor fields}

    These fields are used in constructors ([BITSTRING]). *)

val create_constructor_field : loc_t -> expr field
(** Create a constructor field.

    The defaults are the same as for {!create_pattern_field}
    except that the expression is initialized to [0].
*)

val set_lident_expr : expr field -> string -> expr field
(** Sets the expression in a constructor field to an expression
    which uses the identifier.

    The effect is that the field [{ len : 8 : int }] could
    be created by calling [set_lident_expr field "len"]. *)

val set_int_expr : expr field -> int -> expr field
(** Sets the expression to the value of the integer.

    The effect is that the field [{ 2 : 8 : int }] could
    be created by calling [set_int_expr field 2]. *)

val set_string_expr : expr field -> string -> expr field
(** Sets the expression to the value of the string.

    The effect is that the field [{ "MAGIC" : 8*5 : string }] could
    be created by calling [set_int_expr field "MAGIC"]. *)

val set_expr : expr field -> expr -> expr field
(** Sets the expression field to an arbitrary OCaml expression. *)

(** {3 Accessors} *)

val get_patt : patt field -> patt
(** Get the pattern from a pattern field. *)

val get_expr : expr field -> expr
(** Get the expression from an expression field. *)

val get_length : 'a field -> expr
(** Get the length in bits from a field.  Note that what is returned
    is an OCaml expression, since lengths can be non-constant. *)

type endian_expr =
  | ConstantEndian of Bitstring.endian
  | EndianExpr of expr

val get_endian : 'a field -> endian_expr
(** Get the endianness of a field.  This is an {!endian_expr} which
    could be a constant or an OCaml expression. *)

val get_signed : 'a field -> bool
(** Get the signedness of a field. *)

type field_type = Int | String | Bitstring

val get_type : 'a field -> field_type
(** Get the type of a field, [Int], [String] or [Bitstring]. *)

val get_location : 'a field -> loc_t
(** Get the source code location of a field. *)

val get_offset : 'a field -> expr option
(** Get the offset expression of a field, or [None] if there is none. *)

val get_check : 'a field -> expr option
(** Get the check expression of a field, or [None] if there is none. *)

val get_bind : 'a field -> expr option
(** Get the bind expression of a field, or [None] if there is none. *)

val get_save_offset_to : 'a field -> patt option
(** Get the save_offset_to pattern of a field, or [None] if there is none. *)
