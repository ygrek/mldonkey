(* Copyright 2001, 2002 b8_bavard, b8_fee_carabine, INRIA *)
(*
    This file is part of mldonkey.

    mldonkey is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    mldonkey is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with mldonkey; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

(** A generic lexical analyzer.


   This module implements a simple ``standard'' lexical analyzer, presented
   as a function from character streams to token streams. It implements
   roughly the lexical conventions of Caml, but is parameterized by the
   set of keywords of your language. 


   Example: a lexer suitable for a desk calculator is obtained by
   {[     let lexer = make_lexer ["+";"-";"*";"/";"let";"="; "("; ")"]  ]}

   The associated parser would be a function from [token stream]
   to, for instance, [int], and would have rules such as:

   {[
           let parse_expr = parser
                  [< 'Int n >] -> n
                | [< 'Kwd "("; n = parse_expr; 'Kwd ")" >] -> n
                | [< n1 = parse_expr; n2 = parse_remainder n1 >] -> n2
           and parse_remainder n1 = parser
                  [< 'Kwd "+"; n2 = parse_expr >] -> n1+n2
                | ...
   ]}
*)


(** The type of tokens. The lexical classes are: [Int] and [Float]
   for integer and floating-point numbers; [String] for
   string literals, enclosed in double quotes; [Char] for
   character literals, enclosed in single quotes; [Ident] for
   identifiers (either sequences of letters, digits, underscores
   and quotes, or sequences of ``operator characters'' such as
   [+], [*], etc); and [Kwd] for keywords (either identifiers or
   single ``special characters'' such as [(], [}], etc). *)
type token =
    Kwd of string
  | Ident of string
  | Int of int32
  | Float of float
  | String of string
  | Char of char
           
(** Construct the lexer function. The first argument is the list of
   keywords. An identifier [s] is returned as [Kwd s] if [s]
   belongs to this list, and as [Ident s] otherwise.
   A special character [s] is returned as [Kwd s] if [s]
   belongs to this list, and cause a lexical error (exception
   [Parse_error]) otherwise. Blanks and newlines are skipped.
   Comments delimited by [(*] and [*)] are skipped as well,
   and can be nested. *)
val make_lexer: string list -> (char Stream.t -> token Stream.t)

        
