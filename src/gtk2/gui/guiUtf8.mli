(* Copyright 2004 b8_bavard, INRIA *)
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

(* Internationalization functions *)

type lang =
  AR
| HY
| LT
| LV
| MI
| CY
| BS
| CS
| HR
| HU
| PL
| SK
| SL
| SH
| SR
| ZH
| BE
| BG
| MK
| RU
| UK
| KA
| EL
| HE
| IW
| JA
| KO
| RO
| MT
| TG
| TH
| TR
| VI
| AF
| AN
| BR
| CA
| DA
| DE
| EN
| ES
| ET
| EU
| FI
| FO
| FR
| GA
| GL
| GV
| ID
| IS
| IT
| KL
| KW
| MS
| NL
| NN
| NO
| OC
| PT
| SQ
| SV
| TL
| UZ
| WA
| YI
| UnknownLang

type codeset =
  WINDOWS_1256
| ISO_8859_6
| IBM864
| ARMSCII_8
| WINDOWS_1257
| ISO_8859_13
| ISO_8859_4
| ISO_8859_14
| WINDOWS_1250
| ISO_8859_2
| IBM852
| GB18030
| GBK
| GB2312
| BIG5_HKSCS
| BIG5
| EUC_TW
| WINDOWS_1251
| CP1251
| CP866
| KOI8_R
| ISO_8859_5
| IBM855
| ISO_IR_111
| KOI8R
| KOI8U
| GEORGIAN_ACADEMYE
| WINDOWS_1253
| ISO_8859_7
| WINDOWS_1255
| ISO_8859_8
| IBM862
| EUC_JP
| ISO_2022_JP
| SHIFT_JIS
| UHC
| JOHAB
| EUC_KR
| ISO_2022_KR
| ISO_8859_10
| ISO_8859_16
| ISO_8859_3
| KOI8_T
| TIS_620
| WINDOWS_1254
| ISO_8859_9
| IBM857
| UCS_2
| UTF_8
| UTF_7
| UTF_16
| UTF_32
| UCS_4
| WINDOWS_1258
| VISCII
| TCVN
| WINDOWS_1252
| ISO_8859_15
| IBM850
| ISO_8859_1

type region =
  Default
| Arabic
| Armenian
| Baltic
| Celtic
| CentralEuropean
| ChineseSimplified
| ChineseTraditional
| Cyrillic
| Georgian
| Greek
| Hebrew
| Japanese
| Korean
| Nordic
| Romanian
| SouthEuropean
| Tajik
| Thai
| Turkish
| Vietnamese
| WesternEuropean
| Unicode

(**
   [lang_to_string lang] returns a string, as a 2-letter ISO 639 language code,
   corresponding to [lang].
   If [lang] is not known, returns "EN".
*)

val lang_to_string : lang -> string

(**
   [string_to_lang s] returns a lang corresponding to the string [s].
   If [s] is not a 2-letter ISO 639 language code known, returns UnknownLang.
*)

val string_to_lang : string -> lang

(**
   [codeset_to_string codeset] returns the string, as defined by glib,
   corresponding to [codeset].
*)

val codeset_to_string : codeset -> string

(**
   [string_to_codeset s] returns the codeset corresponding to the string [s].
   If [s] is unknown, returns ISO_8859_1.
*)

val string_to_codeset : string -> codeset

(**
   [codeset_list_from_region region] returns a predefined codeset list.
   It is provided as a convenience.
*)

val codeset_list_from_region : region -> codeset list

(**
   [codeset_list_from_lang lang] returns a predefined codeset list.
   It is provided as a convenience.
*)

val codeset_list_from_lang : lang -> codeset list

(**
   [set_default_codeset_list lang] sets the current codeset used by the
   functions utf8_of and simple_utf8_of.
*)

val set_default_codeset_list : lang -> unit

(**
   [simple_utf8_of s] converts the string [s] into a valid utf8 string.
   If it fails to convert [s] using the current codeset list, returns
   a string of the same length filled with [Glib.Utf8.from_unichar 2].
*)

val simple_utf8_of : string -> string

(**
   [utf8_of s] converts the string [s] into a valid utf8 string.
   The process may be longer than with [simple_utf8_of], as it
   tries to convert the string character by character.
*)

val utf8_of : string -> string

(**
   [private_utf8_of s enc_list] converts the string [s] into a valid utf8 string.
   It is similar to [utf8_of], except that it uses a private codeset list to parse
   the string [s], instead of using the default codeset list.
*)

val private_utf8_of : string ->  codeset list -> string

(**
   [all_regions] returns the list of regions, as predefined codesets, that
   can be used according to the language or country returns by Glib.Convert.get_charset ().
   It is provided as a convenience.
*)

val all_regions : region list


(** Options *)

val languages : string list

val language_to_string : lang -> string

val language_option : lang Options.option_class
