(* Copyright 2005 b8_bavard, INRIA *)
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

(*
 * This part has been inspired by the debian document "Introduction to i18n":
 * http://www.debian.org/doc/manuals/intro-i18n/
 * 
 *)

(**********************************************************************************)
(*                                                                                *)
(*                          types                                                 *)
(*                                                                                *)
(**********************************************************************************)

type uchar = int

type charset =
| ANSI_X3_4_1968 | ANSI_X3_4_1986 | ASCII | CP367 | IBM367 | ISO_IR_6 | ISO646_US | ISO_646_IRV_1991 | US | US_ASCII | CSASCII
| UTF_8
| ISO_10646_UCS_2 | UCS_2 | CSUNICODE
| UCS_2BE | UNICODE_1_1 | UNICODEBIG | CSUNICODE11
| UCS_2LE | UNICODELITTLE
| ISO_10646_UCS_4 | UCS_4 | CSUCS4
| UCS_4BE
| UCS_4LE
| UTF_16
| UTF_16BE
| UTF_16LE
| UTF_32
| UTF_32BE
| UTF_32LE
| UNICODE_1_1_UTF_7 | UTF_7 | CSUNICODE11UTF7
| UCS_2_INTERNAL
| UCS_2_SWAPPED
| UCS_4_INTERNAL
| UCS_4_SWAPPED
| C99
| JAVA
| CP819 | IBM819 | ISO_8859_1 | ISO_IR_100 | ISO8859_1 | ISO_8859_1_1987 | L1 | LATIN1 | CSISOLATIN1
| ISO_8859_2 | ISO_IR_101 | ISO8859_2 | ISO_8859_2_1987 | L2 | LATIN2 | CSISOLATIN2
| ISO_8859_3 | ISO_IR_109 | ISO8859_3 | ISO_8859_3_1988 | L3 | LATIN3 | CSISOLATIN3
| ISO_8859_4 | ISO_IR_110 | ISO8859_4 | ISO_8859_4_1988 | L4 | LATIN4 | CSISOLATIN4
| CYRILLIC | ISO_8859_5 | ISO_IR_144  | ISO8859_5 | ISO_8859_5_1988 | CSISOLATINCYRILLIC
| ARABIC | ASMO_708 | ECMA_114 | ISO_8859_6 | ISO_IR_127 | ISO8859_6 | ISO_8859_6_1987 | CSISOLATINARABIC
| ECMA_118 | ELOT_928 | GREEK | GREEK8 | ISO_8859_7 | ISO_IR_126 | ISO8859_7 | ISO_8859_7_1987 | CSISOLATINGREEK
| HEBREW | ISO_8859_8 | ISO_IR_138 | ISO8859_8 | ISO_8859_8_1988 | CSISOLATINHEBREW
| ISO_8859_9 | ISO_IR_148 | ISO8859_9 | ISO_8859_9_1989 | L5 | LATIN5 | CSISOLATIN5
| ISO_8859_10 | ISO_IR_157 | ISO8859_10 | ISO_8859_10_1992 | L6 | LATIN6 | CSISOLATIN6
| ISO_8859_13 | ISO_IR_179 | ISO8859_13 | L7 | LATIN7
| ISO_8859_14 | ISO_CELTIC | ISO8859_14 | ISO_IR_199 | ISO_8859_14_1998 | L8 | LATIN8
| ISO_8859_15 | ISO_IR_203 | ISO8859_15 | ISO_8859_15_1998
| ISO_8859_16 | ISO_IR_226 | ISO8859_16 | ISO_8859_16_2000
| KOI8_R | CSKOI8R
| KOI8_U
| KOI8_RU
| CP1250 | MS_EE | WINDOWS_1250
| CP1251 | MS_CYRL | WINDOWS_1251
| CP1252 | MS_ANSI | WINDOWS_1252
| CP1253 | MS_GREEK | WINDOWS_1253
| CP1254 | MS_TURK | WINDOWS_1254
| CP1255 | MS_HEBR | WINDOWS_1255
| CP1256 | MS_ARAB | WINDOWS_1256
| CP1257 | WINBALTRIM | WINDOWS_1257
| CP1258 | WINDOWS_1258
| I_850 | CP850 | IBM850 | CSPC850MULTILINGUAL
| I_862 | CP862 | IBM862 | CSPC862LATINHEBREW
| I_866 | CP866 | IBM866 | CSIBM866
| MAC | MACINTOSH | MACROMAN | CSMACINTOSH
| MACCENTRALEUROPE
| MACICELAND
| MACCROATIAN
| MACROMANIA
| MACCYRILLIC
| MACUKRAINE
| MACGREEK
| MACTURKISH
| MACHEBREW
| MACARABIC
| MACTHAI
| HP_ROMAN8 | R8 | ROMAN8 | CSHPROMAN8
| NEXTSTEP
| ARMSCII_8
| GEORGIAN_ACADEMY
| GEORGIAN_PS
| KOI8_T
| MULELAO_1
| CP1133 | IBM_CP1133
| ISO_IR_166 | TIS_620 | TIS620 | TIS620_0 | TIS620_2529_1 | TIS620_2533_0 | TIS620_2533_1
| CP874 | WINDOWS_874
| VISCII | VISCII1_1_1 | CSVISCII
| TCVN | TCVN_5712 | TCVN5712_1 | TCVN5712_1_1993
| ISO_IR_14 | ISO646_JP | JIS_C6220_1969_RO | JP | CSISO14JISC6220RO
| JISX0201_1976 | JIS_X0201 | X0201 | CSHALFWIDTHKATAKANA
| ISO_IR_87 | JIS0208 | JIS_C6226_1983 | JIS_X0208 | JIS_X0208_1983 | JIS_X0208_1990 | X0208 | CSISO87JISX0208
| ISO_IR_159 | JIS_X0212 | JIS_X0212_1990 | JIS_X0212_1990_0 | X0212 | CSISO159JISX02121990
| CN | GB_1988_80 | ISO_IR_57 | ISO646_CN | CSISO57GB1988
| CHINESE | GB_2312_80 | ISO_IR_58 | CSISO58GB231280
| CN_GB_ISOIR165 | ISO_IR_165
| ISO_IR_149 | KOREAN | KSC_5601 | KS_C_5601_1987 | KS_C_5601_1989 | CSKSC56011987
| EUC_JP | EUCJP | EXTENDED_UNIX_CODE_PACKED_FORMAT_FOR_JAPANESE | CSEUCPKDFMTJAPANESE
| MS_KANJI | SHIFT_JIS | SJIS | CSSHIFTJIS
| CP932
| ISO_2022_JP | CSISO2022JP
| ISO_2022_JP_1
| ISO_2022_JP_2 | CSISO2022JP2
| CN_GB | EUC_CN | EUCCN | GB2312 | CSGB2312
| CP936 | GBK
| GB18030
| ISO_2022_CN | CSISO2022CN
| ISO_2022_CN_EXT
| HZ | HZ_GB_2312
| EUC_TW | EUCTW | CSEUCTW
| BIG_5 | BIG_FIVE | BIG5 | BIGFIVE | CN_BIG5 | CSBIG5
| CP950
| BIG5_HKSCS | BIG5HKSCS
| EUC_KR | EUCKR | CSEUCKR
| CP949 | UHC
| CP1361 | JOHAB
| ISO_2022_KR | CSISO2022KR
| I_437 | CP437 | IBM437 | CSPC8CODEPAGE437
| CP737
| CP775 | IBM775 | CSPC775BALTIC
| I_852 | CP852 | IBM852 | CSPCP852
| CP853
| I_855 | CP855 | IBM855 | CSIBM855
| I_857 | CP857 | IBM857 | CSIBM857
| CP858
| I_860 | CP860 | IBM860 | CSIBM860
| I_861 | CP_IS | CP861 | IBM861 | CSIBM861
| I_863 | CP863 | IBM863 | CSIBM863
| CP864 | IBM864 | CSIBM864
| I_865 | CP865 | IBM865 | CSIBM865
| I_869 | CP_GR | CP869 | IBM869 | CSIBM869
| CP1125

(**********************************************************************************)
(*                                                                                *)
(*                          charsetstubs                                          *)
(*                                                                                *)
(**********************************************************************************)

exception CharsetError

let () = Callback.register_exception "charset_error" CharsetError

external get_charset : unit -> string = "ml_locale_charset"
external get_default_language : unit -> string = "ml_get_default_language"
external convert_string : string -> string -> string -> string = "ml_convert_string"
external is_utf8 : string -> bool = "ml_utf8_validate"

(**********************************************************************************)
(*                                                                                *)
(*                          utf8_get                                              *)
(*                                                                                *)
(**********************************************************************************)

(* taken from camomile *)
(* $Id$ *)
(* Copyright 2002, 2003 Yamagata Yoriyuki. distributed with LGPL *)

let utf8_look s i =
  let n' =
    let n = Char.code s.[i] in
    if n < 0x80 then n else
    if n <= 0xdf then
      (n - 0xc0) lsl 6 lor (0x7f land (Char.code s.[i + 1]))
    else if n <= 0xef then
      let n' = n - 0xe0 in
      let m0 = Char.code s.[i + 2] in
      let m = Char.code (String.unsafe_get s (i + 1)) in
      let n' = n' lsl 6 lor (0x7f land m) in
      n' lsl 6 lor (0x7f land m0)
    else if n <= 0xf7 then
      let n' = n - 0xf0 in
      let m0 = Char.code s.[i + 3] in
      let m = Char.code (String.unsafe_get s (i + 1)) in
      let n' = n' lsl 6 lor (0x7f land m) in
      let m = Char.code (String.unsafe_get s (i + 2)) in
      let n' = n' lsl 6 lor (0x7f land m) in
      n' lsl 6 lor (0x7f land m0)     
    else if n <= 0xfb then
      let n' = n - 0xf8 in
      let m0 = Char.code s.[i + 4] in
      let m = Char.code (String.unsafe_get s (i + 1)) in
      let n' = n' lsl 6 lor (0x7f land m) in
      let m = Char.code (String.unsafe_get s (i + 2)) in
      let n' = n' lsl 6 lor (0x7f land m) in
      let m = Char.code (String.unsafe_get s (i + 3)) in
      let n' = n' lsl 6 lor (0x7f land m) in
      n' lsl 6 lor (0x7f land m0)     
    else if n <= 0xfd then
      let n' = n - 0xfc in
      let m0 = Char.code s.[i + 5] in
      let m = Char.code (String.unsafe_get s (i + 1)) in
      let n' = n' lsl 6 lor (0x7f land m) in
      let m = Char.code (String.unsafe_get s (i + 2)) in
      let n' = n' lsl 6 lor (0x7f land m) in
      let m = Char.code (String.unsafe_get s (i + 3)) in
      let n' = n' lsl 6 lor (0x7f land m) in
      let m = Char.code (String.unsafe_get s (i + 4)) in
      let n' = n' lsl 6 lor (0x7f land m) in
      n' lsl 6 lor (0x7f land m0)
    else invalid_arg "utf8_look"
  in
  if n' lsr 31 = 0 then n' else
  invalid_arg "utf8_look char_of_uint"

let rec search_head s i =
  if i >= String.length s then i else
  let n = Char.code (String.unsafe_get s i) in
  if n < 0x80 || n >= 0xc2 then i else
  search_head s (i + 1)

let utf8_next s i = 
  let n = Char.code s.[i] in
  if n < 0x80 then i + 1 else
  if n < 0xc0 then search_head s (i + 1) else
  if n <= 0xdf then i + 2
  else if n <= 0xef then i + 3
  else if n <= 0xf7 then i + 4
  else if n <= 0xfb then i + 5
  else if n <= 0xfd then i + 6
  else invalid_arg "utf8_next"

let rec nth_aux s i n =
  if n = 0 then i else
  nth_aux s (utf8_next s i) (n - 1)

let utf8_nth s n = nth_aux s 0 n

let utf8_get s n = utf8_look s (utf8_nth s n)

(**********************************************************************************)
(*                                                                                *)
(*                          utf8_length                                           *)
(*                                                                                *)
(**********************************************************************************)

(* taken from camomile *)
(* $Id$ *)
(* Copyright 2002, 2003 Yamagata Yoriyuki. distributed with LGPL *)

let rec length_aux s c i =
  if i >= String.length s then c else
  let n = Char.code (String.unsafe_get s i) in
  let k =
    if n < 0x80 then 1 else
    if n < 0xc0 then invalid_arg "UTF8.length" else
    if n < 0xe0 then 2 else
    if n < 0xf0 then 3 else
    if n < 0xf8 then 4 else
    if n < 0xfc then 5 else
    if n < 0xfe then 6 else
    invalid_arg "UTF8.length" in
  length_aux s (c + 1) (i + k)

let utf8_length s = length_aux s 0 0

(**********************************************************************************)
(*                                                                                *)
(*                          add_uchar (internal)                                  *)
(*                                                                                *)
(**********************************************************************************)


(* taken from camomile *)
(* $Id$ *)
(* Copyright 2002, 2003 Yamagata Yoriyuki. distributed with LGPL *)

external uint_code : uchar -> int = "%identity"

let add_uchar buf u =
  let masq = 0b111111 in
  let k = uint_code u in
  if k < 0 || k >= 0x4000000 then begin
    Buffer.add_char buf (Char.chr (0xfc + (k lsr 30)));
    Buffer.add_char buf (Char.unsafe_chr (0x80 lor ((k lsr 24) land masq))); 
    Buffer.add_char buf (Char.unsafe_chr (0x80 lor ((k lsr 18) land masq)));
    Buffer.add_char buf (Char.unsafe_chr (0x80 lor ((k lsr 12) land masq)));
    Buffer.add_char buf (Char.unsafe_chr (0x80 lor ((k lsr 6) land masq)));
    Buffer.add_char buf (Char.unsafe_chr (0x80 lor (k land masq)));
  end else if k <= 0x7f then
    Buffer.add_char buf (Char.unsafe_chr k)
  else if k <= 0x7ff then begin
    Buffer.add_char buf (Char.unsafe_chr (0xc0 lor (k lsr 6)));
    Buffer.add_char buf (Char.unsafe_chr (0x80 lor (k land masq)))
  end else if k <= 0xffff then begin
    Buffer.add_char buf (Char.unsafe_chr (0xe0 lor (k lsr 12)));
    Buffer.add_char buf (Char.unsafe_chr (0x80 lor ((k lsr 6) land masq)));
    Buffer.add_char buf (Char.unsafe_chr (0x80 lor (k land masq)));
  end else if k <= 0x1fffff then begin
    Buffer.add_char buf (Char.unsafe_chr (0xf0 + (k lsr 18)));
    Buffer.add_char buf (Char.unsafe_chr (0x80 lor ((k lsr 12) land masq)));
    Buffer.add_char buf (Char.unsafe_chr (0x80 lor ((k lsr 6) land masq)));
    Buffer.add_char buf (Char.unsafe_chr (0x80 lor (k land masq)));
  end else begin
    Buffer.add_char buf (Char.unsafe_chr (0xf8 + (k lsr 24)));
    Buffer.add_char buf (Char.unsafe_chr (0x80 lor ((k lsr 18) land masq)));
    Buffer.add_char buf (Char.unsafe_chr (0x80 lor ((k lsr 12) land masq)));
    Buffer.add_char buf (Char.unsafe_chr (0x80 lor ((k lsr 6) land masq)));
    Buffer.add_char buf (Char.unsafe_chr (0x80 lor (k land masq)));
  end

(**********************************************************************************)
(*                                                                                *)
(*                          charset_to_string                                     *)
(*                                                                                *)
(**********************************************************************************)

let charset_to_string charset =
  match charset with
    | ANSI_X3_4_1968 -> "ANSI_X3.4-1968"
    | ANSI_X3_4_1986 -> "ANSI_X3.4-1986"
    | ASCII -> "ASCII"
    | CP367 -> "CP367"
    | IBM367 -> "IBM367"
    | ISO_IR_6 -> "ISO-IR-6"
    | ISO646_US -> "ISO646-US"
    | ISO_646_IRV_1991 -> "ISO_646.IRV:1991"
    | US -> "US"
    | US_ASCII -> "US-ASCII"
    | CSASCII -> "CSASCII"

    | UTF_8 -> "UTF-8"

    | ISO_10646_UCS_2 -> "ISO-10646-UCS-2"
    | UCS_2 -> "UCS-2"
    | CSUNICODE -> "CSUNICODE"

    | UCS_2BE -> "UCS-2BE"
    | UNICODE_1_1 -> "UNICODE-1-1"
    | UNICODEBIG -> "UNICODEBIG"
    | CSUNICODE11 -> "CSUNICODE11"

    | UCS_2LE -> "UCS-2LE"
    | UNICODELITTLE -> "UNICODELITTLE"

    | ISO_10646_UCS_4 -> "ISO-10646-UCS-4"
    | UCS_4 -> "UCS-4"
    | CSUCS4 -> "CSUCS4"

    | UCS_4BE -> "UCS-4BE"

    | UCS_4LE -> "UCS-4LE"

    | UTF_16 -> "UTF-16"

    | UTF_16BE -> "UTF-16BE"

    | UTF_16LE -> "UTF-16LE"

    | UTF_32 -> "UTF-32"

    | UTF_32BE -> "UTF-32BE"

    | UTF_32LE -> "UTF-32LE"

    | UNICODE_1_1_UTF_7 -> "UNICODE-1-1-UTF-7"
    | UTF_7 -> "UTF-7"
    | CSUNICODE11UTF7 -> "CSUNICODE11UTF7"

    | UCS_2_INTERNAL -> "UCS-2-INTERNAL"

    | UCS_2_SWAPPED -> "UCS-2-SWAPPED"

    | UCS_4_INTERNAL -> "UCS-4-INTERNAL"

    | UCS_4_SWAPPED -> "UCS-4-SWAPPED"

    | C99 -> "C99"

    | JAVA -> "JAVA"

    | CP819 -> "CP819"
    | IBM819 -> "IBM819"
    | ISO_8859_1 -> "ISO-8859-1"
    | ISO_IR_100 -> "ISO-IR-100"
    | ISO8859_1 -> "ISO8859-1"
    | ISO_8859_1_1987 -> "ISO_8859-1:1987"
    | L1 -> "L1"
    | LATIN1 -> "LATIN1"
    | CSISOLATIN1 -> "CSISOLATIN1"

    | ISO_8859_2 -> "ISO-8859-2"
    | ISO_IR_101 -> "ISO-IR-101"
    | ISO8859_2 -> "ISO8859-2"
    | ISO_8859_2_1987 -> "ISO_8859-2:1987"
    | L2 -> "L2"
    | LATIN2 -> "LATIN2"
    | CSISOLATIN2 -> "CSISOLATIN2"

    | ISO_8859_3 -> "ISO-8859-3"
    | ISO_IR_109 -> "ISO-IR-109"
    | ISO8859_3 -> "ISO8859-3"
    | ISO_8859_3_1988 -> "ISO_8859-3:1988"
    | L3 -> "L3"
    | LATIN3 -> "LATIN3"
    | CSISOLATIN3 -> "CSISOLATIN3"

    | ISO_8859_4 -> "ISO-8859-4"
    | ISO_IR_110 -> "ISO-IR-110"
    | ISO8859_4 -> "ISO8859-4"
    | ISO_8859_4_1988 -> "ISO_8859-4:1988"
    | L4 -> "L4"
    | LATIN4 -> "LATIN4"
    | CSISOLATIN4 -> "CSISOLATIN4"

    | CYRILLIC -> "CYRILLIC"
    | ISO_8859_5 -> "ISO-8859-5"
    | ISO_IR_144 -> "ISO-IR-144"
    | ISO8859_5 -> "ISO8859-5"
    | ISO_8859_5_1988 -> "ISO_8859-5:1988"
    | CSISOLATINCYRILLIC -> "CSISOLATINCYRILLIC"

    | ARABIC -> "ARABIC"
    | ASMO_708 -> "ASMO-708"
    | ECMA_114 -> "ECMA-114"
    | ISO_8859_6 -> "ISO-8859-6"
    | ISO_IR_127 -> "ISO-IR-127"
    | ISO8859_6 -> "ISO8859-6"
    | ISO_8859_6_1987 -> "ISO_8859-6:1987"
    | CSISOLATINARABIC -> "CSISOLATINARABIC"

    | ECMA_118 -> "ECMA-118"
    | ELOT_928 -> "ELOT_928"
    | GREEK -> "GREEK"
    | GREEK8 -> "GREEK8"
    | ISO_8859_7 -> "ISO-8859-7"
    | ISO_IR_126 -> "ISO-IR-126"
    | ISO8859_7 -> "ISO8859-7"
    | ISO_8859_7_1987 -> "ISO_8859-7:1987"
    | CSISOLATINGREEK -> "CSISOLATINGREEK"

    | HEBREW -> "HEBREW"
    | ISO_8859_8 -> "ISO-8859-8"
    | ISO_IR_138 -> "ISO-IR-138"
    | ISO8859_8 -> "ISO8859-8"
    | ISO_8859_8_1988 -> "ISO_8859-8:1988"
    | CSISOLATINHEBREW -> "CSISOLATINHEBREW"

    | ISO_8859_9 -> "ISO-8859-9"
    | ISO_IR_148 -> "ISO-IR-148"
    | ISO8859_9 -> "ISO8859-9"
    | ISO_8859_9_1989 -> "ISO_8859-9:1989"
    | L5 -> "L5"
    | LATIN5 -> "LATIN5"
    | CSISOLATIN5 -> "CSISOLATIN5"

    | ISO_8859_10 -> "ISO-8859-10"
    | ISO_IR_157 -> "ISO-IR-157"
    | ISO8859_10 -> "ISO8859-10"
    | ISO_8859_10_1992 -> "ISO_8859-10:1992"
    | L6 -> "L6"
    | LATIN6 -> "LATIN6"
    | CSISOLATIN6 -> "CSISOLATIN6"

    | ISO_8859_13 -> "ISO-8859-13"
    | ISO_IR_179 -> "ISO-IR-179"
    | ISO8859_13 -> "ISO8859-13"
    | L7 -> "L7"
    | LATIN7 -> "LATIN7"

    | ISO_8859_14 -> "ISO-8859-14"
    | ISO_CELTIC -> "ISO-CELTIC"
    | ISO_IR_199 -> "ISO-IR-199"
    | ISO8859_14 -> "ISO8859-14"
    | ISO_8859_14_1998 -> "ISO_8859-14:1998"
    | L8 -> "L8"
    | LATIN8 -> "LATIN8"

    | ISO_8859_15 -> "ISO-8859-15"
    | ISO_IR_203 -> "ISO-IR-203"
    | ISO8859_15 -> "ISO8859-15"
    | ISO_8859_15_1998 -> "ISO_8859-15:1998"

    | ISO_8859_16 -> "ISO-8859-16"
    | ISO_IR_226 -> "ISO-IR-226"
    | ISO8859_16 -> "ISO8859-16"
    | ISO_8859_16_2000 -> "ISO_8859-16:2000"

    | KOI8_R -> "KOI8-R"
    | CSKOI8R -> "CSKOI8R"

    | KOI8_U -> "KOI8-U"

    | KOI8_RU -> "KOI8-RU"

    | CP1250 -> "CP1250"
    | MS_EE -> "MS-EE"
    | WINDOWS_1250 -> "WINDOWS-1250"

    | CP1251 -> "CP1251"
    | MS_CYRL -> "MS-CYRL"
    | WINDOWS_1251 -> "WINDOWS-1251"

    | CP1252 -> "CP1252"
    | MS_ANSI -> "MS-ANSI"
    | WINDOWS_1252 -> "WINDOWS-1252"

    | CP1253 -> "CP1253"
    | MS_GREEK -> "MS-GREEK"
    | WINDOWS_1253 -> "WINDOWS-1253"

    | CP1254 -> "CP1254"
    | MS_TURK -> "MS-TURK"
    | WINDOWS_1254 -> "WINDOWS-1254"

    | CP1255 -> "CP1255"
    | MS_HEBR -> "MS-HEBR"
    | WINDOWS_1255 -> "WINDOWS-1255"

    | CP1256 -> "CP1256"
    | MS_ARAB -> "MS-ARAB"
    | WINDOWS_1256 -> "WINDOWS-1256"

    | CP1257 -> "CP1257"
    | WINBALTRIM -> "WINBALTRIM"
    | WINDOWS_1257 -> "WINDOWS-1257"

    | CP1258 -> "CP1258"
    | WINDOWS_1258 -> "WINDOWS-1258"

    | I_850 -> "850"
    | CP850 -> "CP850"
    | IBM850 -> "IBM850"
    | CSPC850MULTILINGUAL -> "CSPC850MULTILINGUAL"

    | I_862 -> "862"
    | CP862 -> "CP862"
    | IBM862 -> "IBM862"
    | CSPC862LATINHEBREW -> "CSPC862LATINHEBREW"

    | I_866 -> "866"
    | CP866 -> "CP866"
    | IBM866 -> "IBM866"
    | CSIBM866 -> "CSIBM866"

    | MAC -> "MAC"
    | MACINTOSH -> "MACINTOSH"
    | MACROMAN -> "MACROMAN"
    | CSMACINTOSH -> "CSMACINTOSH"

    | MACCENTRALEUROPE -> "MACCENTRALEUROPE"

    | MACICELAND -> "MACICELAND"

    | MACCROATIAN -> "MACCROATIAN"

    | MACROMANIA -> "MACROMANIA"

    | MACCYRILLIC -> "MACCYRILLIC"

    | MACUKRAINE -> "MACUKRAINE"

    | MACGREEK -> "MACGREEK"

    | MACTURKISH -> "MACTURKISH"

    | MACHEBREW -> "MACHEBREW"

    | MACARABIC -> "MACARABIC"

    | MACTHAI -> "MACTHAI"

    | HP_ROMAN8 -> "HP-ROMAN8"
    | R8 -> "R8"
    | ROMAN8 -> "ROMAN8"
    | CSHPROMAN8 -> "CSHPROMAN8"

    | NEXTSTEP -> "NEXTSTEP"

    | ARMSCII_8 -> "ARMSCII-8"

    | GEORGIAN_ACADEMY -> "GEORGIAN-ACADEMY"

    | GEORGIAN_PS -> "GEORGIAN-PS"

    | KOI8_T -> "KOI8-T"

    | MULELAO_1 -> "MULELAO-1"

    | CP1133 -> "CP1133"
    | IBM_CP1133 -> "IBM-CP1133"

    | ISO_IR_166 -> "ISO-IR-166"
    | TIS_620 -> "TIS-620"
    | TIS620 -> "TIS620"
    | TIS620_0 -> "TIS620-0"
    | TIS620_2529_1 -> "TIS620.2529-1"
    | TIS620_2533_0 -> "TIS620.2533-0"
    | TIS620_2533_1 -> "TIS620.2533-1"

    | CP874 -> "CP874"
    | WINDOWS_874 -> "WINDOWS-874"

    | VISCII -> "VISCII"
    | VISCII1_1_1 -> "VISCII1.1-1"
    | CSVISCII -> "CSVISCII"

    | TCVN -> "TCVN"
    | TCVN_5712 -> "TCVN-5712"
    | TCVN5712_1 -> "TCVN5712-1"
    | TCVN5712_1_1993 -> "TCVN5712-1:1993"

    | ISO_IR_14 -> "ISO-IR-14"
    | ISO646_JP -> "ISO646-JP"
    | JIS_C6220_1969_RO -> "JIS_C6220-1969-RO"
    | JP -> "JP"
    | CSISO14JISC6220RO -> "CSISO14JISC6220RO"

    | JISX0201_1976 -> "JISX0201-1976"
    | JIS_X0201 -> "JIS_X0201"
    | X0201 -> "X0201"
    | CSHALFWIDTHKATAKANA -> "CSHALFWIDTHKATAKANA"

    | ISO_IR_87 -> "ISO-IR-87"
    | JIS0208 -> "JIS0208"
    | JIS_C6226_1983 -> "JIS_C6226-1983"
    | JIS_X0208 -> "JIS_X0208"
    | JIS_X0208_1983 -> "JIS_X0208-1983"
    | JIS_X0208_1990 -> "JIS_X0208-1990"
    | X0208 -> "X0208"
    | CSISO87JISX0208 -> "CSISO87JISX0208"

    | ISO_IR_159 -> "ISO-IR-159"
    | JIS_X0212 -> "JIS_X0212"
    | JIS_X0212_1990 -> "JIS_X0212-1990"
    | JIS_X0212_1990_0 -> "JIS_X0212.1990-0"
    | X0212 -> "X0212"
    | CSISO159JISX02121990 -> "CSISO159JISX02121990"

    | CN -> "CN"
    | GB_1988_80 -> "GB_1988-80"
    | ISO_IR_57 -> "ISO-IR-57"
    | ISO646_CN -> "ISO646-CN"
    | CSISO57GB1988 -> "CSISO57GB1988"

    | CHINESE -> "CHINESE"
    | GB_2312_80 -> "GB_2312-80"
    | ISO_IR_58 -> "ISO-IR-58"
    | CSISO58GB231280 -> "CSISO58GB231280"

    | CN_GB_ISOIR165 -> "CN-GB-ISOIR165"
    | ISO_IR_165 -> "ISO-IR-165"

    | ISO_IR_149 -> "ISO-IR-149"
    | KOREAN -> "KOREAN"
    | KSC_5601 -> "KSC_5601"
    | KS_C_5601_1987 -> "KS_C_5601-1987"
    | KS_C_5601_1989 -> "KS_C_5601-1989"
    | CSKSC56011987 -> "CSKSC56011987"

    | EUC_JP -> "EUC-JP"
    | EUCJP -> "EUCJP"
    | EXTENDED_UNIX_CODE_PACKED_FORMAT_FOR_JAPANESE -> "EXTENDED_UNIX_CODE_PACKED_FORMAT_FOR_JAPANESE"
    | CSEUCPKDFMTJAPANESE -> "CSEUCPKDFMTJAPANESE"

    | MS_KANJI -> "MS_KANJI"
    | SHIFT_JIS -> "SHIFT-JIS"
    | SJIS -> "SJIS"
    | CSSHIFTJIS -> "CSSHIFTJIS"

    | CP932 -> "CP932"

    | ISO_2022_JP -> "ISO-2022-JP"
    | CSISO2022JP -> "CSISO2022JP"

    | ISO_2022_JP_1 -> "ISO-2022-JP-1"

    | ISO_2022_JP_2 -> "ISO-2022-JP-2"
    | CSISO2022JP2 -> "CSISO2022JP2"

    | CN_GB -> "CN-GB"
    | EUC_CN -> "EUC-CN"
    | EUCCN -> "EUCCN"
    | GB2312 -> "GB2312"
    | CSGB2312 -> "CSGB2312"

    | CP936 -> "CP936"
    | GBK -> "GBK"

    | GB18030 -> "GB18030"

    | ISO_2022_CN -> "ISO-2022-CN"
    | CSISO2022CN -> "CSISO2022CN"

    | ISO_2022_CN_EXT -> "ISO-2022-CN-EXT"

    | HZ -> "HZ"
    | HZ_GB_2312 -> "HZ-GB-2312"

    | EUC_TW -> "EUC-TW"
    | EUCTW -> "EUCTW"
    | CSEUCTW -> "CSEUCTW"

    | BIG_5 -> "BIG-5"
    | BIG_FIVE -> "BIG-FIVE"
    | BIG5 -> "BIG5"
    | BIGFIVE -> "BIGFIVE"
    | CN_BIG5 -> "CN-BIG5"
    | CSBIG5 -> "CSBIG5"

    | CP950 -> "CP950"

    | BIG5_HKSCS -> "BIG5-HKSCS"
    | BIG5HKSCS -> "BIG5HKSCS"

    | EUC_KR -> "EUC-KR"
    | EUCKR -> "EUCKR"
    | CSEUCKR -> "CSEUCKR"

    | CP949 -> "CP949"
    | UHC -> "UHC"

    | CP1361 -> "CP1361"
    | JOHAB -> "JOHAB"

    | ISO_2022_KR -> "ISO-2022-KR"
    | CSISO2022KR -> "CSISO2022KR"

    | I_437 -> "437"
    | CP437 -> "CP437"
    | IBM437 -> "IBM437"
    | CSPC8CODEPAGE437 -> "CSPC8CODEPAGE437"

    | CP737 -> "CP737"

    | CP775 -> "CP775"
    | IBM775 -> "IBM775"
    | CSPC775BALTIC -> "CSPC775BALTIC"

    | I_852 -> "852"
    | CP852 -> "CP852"
    | IBM852 -> "IBM852"
    | CSPCP852 -> "CSPCP852"

    | CP853 -> "CP853"

    | I_855 -> "855"
    | CP855 -> "CP855"
    | IBM855 -> "IBM855"
    | CSIBM855 -> "CSIBM855"

    | I_857 -> "857"
    | CP857 -> "CP857"
    | IBM857 -> "IBM857"
    | CSIBM857 -> "CSIBM857"

    | CP858 -> "CP858"

    | I_860 -> "860"
    | CP860 -> "CP860"
    | IBM860 -> "IBM860"
    | CSIBM860 -> "CSIBM860"

    | I_861 -> "861"
    | CP_IS -> "CP-IS"
    | CP861 -> "CP861"
    | IBM861 -> "IBM861"
    | CSIBM861 -> "CSIBM861"

    | I_863 -> "863"
    | CP863 -> "CP863"
    | IBM863 -> "IBM863"
    | CSIBM863 -> "CSIBM863"

    | CP864 -> "CP864"
    | IBM864 -> "IBM864"
    | CSIBM864 -> "CSIBM864"

    | I_865 -> "865"
    | CP865 -> "CP865"
    | IBM865 -> "IBM865"
    | CSIBM865 -> "CSIBM865"

    | I_869 -> "869"
    | CP_GR -> "CP-GR"
    | CP869 -> "CP869"
    | IBM869 -> "IBM869"
    | CSIBM869 -> "CSIBM869"

    | CP1125 -> "CP1125"

(**********************************************************************************)
(*                                                                                *)
(*                          charset_from_string                                   *)
(*                                                                                *)
(**********************************************************************************)

let charset_from_string s =
  match s with
    | "ANSI_X3.4-1968" -> ANSI_X3_4_1968
    | "ANSI_X3.4-1986" -> ANSI_X3_4_1986
    | "ASCII" -> ASCII
    | "CP367" -> CP367
    | "IBM367" -> IBM367
    | "ISO-IR-6" -> ISO_IR_6
    | "ISO646-US" -> ISO646_US
    | "ISO_646.IRV:1991" -> ISO_646_IRV_1991
    | "US" -> US
    | "US-ASCII" -> US_ASCII
    | "CSASCII" -> CSASCII

    | "UTF-8" -> UTF_8

    | "ISO-10646-UCS-2" -> ISO_10646_UCS_2
    | "UCS-2" -> UCS_2
    | "CSUNICODE" -> CSUNICODE

    | "UCS-2BE" -> UCS_2BE
    | "UNICODE-1-1" -> UNICODE_1_1
    | "UNICODEBIG" -> UNICODEBIG
    | "CSUNICODE11" -> CSUNICODE11

    | "UCS-2LE" -> UCS_2LE
    | "UNICODELITTLE" -> UNICODELITTLE

    | "ISO-10646-UCS-4" -> ISO_10646_UCS_4
    | "UCS-4" -> UCS_4
    | "CSUCS4" -> CSUCS4

    | "UCS-4BE" -> UCS_4BE

    | "UCS-4LE" -> UCS_4LE

    | "UTF-16" -> UTF_16

    | "UTF-16BE" -> UTF_16BE

    | "UTF-16LE" -> UTF_16LE

    | "UTF-32" -> UTF_32

    | "UTF-32BE" -> UTF_32BE

    | "UTF-32LE" -> UTF_32LE

    | "UNICODE-1-1-UTF-7" -> UNICODE_1_1_UTF_7
    | "UTF-7" -> UTF_7
    | "CSUNICODE11UTF7" -> CSUNICODE11UTF7

    | "UCS-2-INTERNAL" -> UCS_2_INTERNAL

    | "UCS-2-SWAPPED" -> UCS_2_SWAPPED

    | "UCS-4-INTERNAL" -> UCS_4_INTERNAL

    | "UCS-4-SWAPPED" -> UCS_4_SWAPPED

    | "C99" -> C99

    | "JAVA" -> JAVA

    | "CP819" -> CP819
    | "IBM819" -> IBM819
    | "ISO-8859-1" -> ISO_8859_1
    | "ISO-IR-100" -> ISO_IR_100
    | "ISO8859-1" -> ISO8859_1
    | "ISO_8859-1" -> ISO_8859_1
    | "ISO_8859-1:1987" -> ISO_8859_1_1987
    | "L1" -> L1
    | "LATIN1" -> LATIN1
    | "CSISOLATIN1" -> CSISOLATIN1

    | "ISO-8859-2" -> ISO_8859_2
    | "ISO-IR-101" -> ISO_IR_101
    | "ISO8859-2" -> ISO8859_2
    | "ISO_8859-2" -> ISO_8859_2
    | "ISO_8859-2:1987" -> ISO_8859_2_1987
    | "L2" -> L2
    | "LATIN2" -> LATIN2
    | "CSISOLATIN2" -> CSISOLATIN2

    | "ISO-8859-3" -> ISO_8859_3
    | "ISO-IR-109" -> ISO_IR_109
    | "ISO8859-3" -> ISO8859_3
    | "ISO_8859-3" -> ISO_8859_3
    | "ISO_8859-3:1988" -> ISO_8859_3_1988
    | "L3" -> L3
    | "LATIN3" -> LATIN3
    | "CSISOLATIN3" -> CSISOLATIN3

    | "ISO-8859-4" -> ISO_8859_4
    | "ISO-IR-110" -> ISO_IR_110
    | "ISO8859-4" -> ISO8859_4
    | "ISO_8859-4" -> ISO_8859_4
    | "ISO_8859-4:1988" -> ISO_8859_4_1988
    | "L4" -> L4
    | "LATIN4" -> LATIN4
    | "CSISOLATIN4" -> CSISOLATIN4

    | "CYRILLIC" -> CYRILLIC
    | "ISO-8859-5" -> ISO_8859_5
    | "ISO-IR-144" -> ISO_IR_144
    | "ISO8859-5" -> ISO8859_5
    | "ISO_8859-5" -> ISO_8859_5
    | "ISO_8859-5:1988" -> ISO_8859_5_1988
    | "CSISOLATINCYRILLIC" -> CSISOLATINCYRILLIC

    | "ARABIC" -> ARABIC
    | "ASMO-708" -> ASMO_708
    | "ECMA-114" -> ECMA_114
    | "ISO-8859-6" -> ISO_8859_6
    | "ISO-IR-127" -> ISO_IR_127
    | "ISO8859-6" -> ISO8859_6
    | "ISO_8859-6" -> ISO_8859_6
    | "ISO_8859-6:1987" -> ISO_8859_6_1987
    | "CSISOLATINARABIC" -> CSISOLATINARABIC

    | "ECMA-118" -> ECMA_118
    | "ELOT_928" -> ELOT_928
    | "GREEK" -> GREEK
    | "GREEK8" -> GREEK8
    | "ISO-8859-7" -> ISO_8859_7
    | "ISO-IR-126" -> ISO_IR_126
    | "ISO8859-7" -> ISO8859_7
    | "ISO_8859-7" -> ISO_8859_7
    | "ISO_8859-7:1987" -> ISO_8859_7_1987
    | "CSISOLATINGREEK" -> CSISOLATINGREEK

    | "HEBREW" -> HEBREW
    | "ISO-8859-8" -> ISO_8859_8
    | "ISO-IR-138" -> ISO_IR_138
    | "ISO8859-8" -> ISO8859_8
    | "ISO_8859-8" -> ISO_8859_8
    | "ISO_8859-8:1988" -> ISO_8859_8_1988
    | "CSISOLATINHEBREW" -> CSISOLATINHEBREW

    | "ISO-8859-9" -> ISO_8859_9
    | "ISO-IR-148" -> ISO_IR_148
    | "ISO8859-9" -> ISO8859_9
    | "ISO_8859-9" -> ISO_8859_9
    | "ISO_8859-9:1989" -> ISO_8859_9_1989
    | "L5" -> L5
    | "LATIN5" -> LATIN5
    | "CSISOLATIN5" -> CSISOLATIN5

    | "ISO-8859-10" -> ISO_8859_10
    | "ISO-IR-157" -> ISO_IR_157
    | "ISO8859-10" -> ISO8859_10
    | "ISO_8859-10" -> ISO_8859_10
    | "ISO_8859-10:1992" -> ISO_8859_10_1992
    | "L6" -> L6
    | "LATIN6" -> LATIN6
    | "CSISOLATIN6" -> CSISOLATIN6

    | "ISO-8859-13" -> ISO_8859_13
    | "ISO-IR-179" -> ISO_IR_179
    | "ISO8859-13" -> ISO8859_13
    | "ISO_8859-13" -> ISO_8859_13
    | "L7" -> L7
    | "LATIN7" -> LATIN7

    | "ISO-8859-14" -> ISO_8859_14
    | "ISO-CELTIC" -> ISO_CELTIC
    | "ISO-IR-199" -> ISO_IR_199
    | "ISO8859-14" -> ISO8859_14
    | "ISO_8859-14" -> ISO_8859_14
    | "ISO_8859-14:1998" -> ISO_8859_14_1998
    | "L8" -> L8
    | "LATIN8" -> LATIN8

    | "ISO-8859-15" -> ISO_8859_15
    | "ISO-IR-203" -> ISO_IR_203
    | "ISO8859-15" -> ISO8859_15
    | "ISO_8859-15" -> ISO_8859_15
    | "ISO_8859-15:1998" -> ISO_8859_15_1998

    | "ISO-8859-16" -> ISO_8859_16
    | "ISO-IR-226" -> ISO_IR_226
    | "ISO8859-16" -> ISO8859_16
    | "ISO_8859-16" -> ISO_8859_16
    | "ISO_8859-16:2000" -> ISO_8859_16_2000

    | "KOI8-R" -> KOI8_R
    | "CSKOI8R" -> CSKOI8R

    | "KOI8-U" -> KOI8_U

    | "KOI8-RU" -> KOI8_RU

    | "CP1250" -> CP1250
    | "MS-EE" -> MS_EE
    | "WINDOWS-1250" -> WINDOWS_1250

    | "CP1251" -> CP1251
    | "MS-CYRL" -> MS_CYRL
    | "WINDOWS-1251" -> WINDOWS_1251

    | "CP1252" -> CP1252
    | "MS-ANSI" -> MS_ANSI
    | "WINDOWS-1252" -> WINDOWS_1252

    | "CP1253" -> CP1253
    | "MS-GREEK" -> MS_GREEK
    | "WINDOWS-1253" -> WINDOWS_1253

    | "CP1254" -> CP1254
    | "MS-TURK" -> MS_TURK
    | "WINDOWS-1254" -> WINDOWS_1254

    | "CP1255" -> CP1255
    | "MS-HEBR" -> MS_HEBR
    | "WINDOWS-1255" -> WINDOWS_1255

    | "CP1256" -> CP1256
    | "MS-ARAB" -> MS_ARAB
    | "WINDOWS-1256" -> WINDOWS_1256

    | "CP1257" -> CP1257
    | "WINBALTRIM" -> WINBALTRIM
    | "WINDOWS-1257" -> WINDOWS_1257

    | "CP1258" -> CP1258
    | "WINDOWS-1258" -> WINDOWS_1258

    | "850" -> I_850
    | "CP850" -> CP850
    | "IBM850" -> IBM850
    | "CSPC850MULTILINGUAL" -> CSPC850MULTILINGUAL

    | "862" -> I_862
    | "CP862" -> CP862
    | "IBM862" -> IBM862
    | "CSPC862LATINHEBREW" -> CSPC862LATINHEBREW

    | "866" -> I_866
    | "CP866" -> CP866
    | "IBM866" -> IBM866
    | "CSIBM866" -> CSIBM866

    | "MAC" -> MAC
    | "MACINTOSH" -> MACINTOSH
    | "MACROMAN" -> MACROMAN
    | "CSMACINTOSH" -> CSMACINTOSH

    | "MACCENTRALEUROPE" -> MACCENTRALEUROPE

    | "MACICELAND" -> MACICELAND

    | "MACCROATIAN" -> MACCROATIAN

    | "MACROMANIA" -> MACROMANIA

    | "MACCYRILLIC" -> MACCYRILLIC

    | "MACUKRAINE" -> MACUKRAINE

    | "MACGREEK" -> MACGREEK

    | "MACTURKISH" -> MACTURKISH

    | "MACHEBREW" -> MACHEBREW

    | "MACARABIC" -> MACARABIC

    | "MACTHAI" -> MACTHAI

    | "HP-ROMAN8" -> HP_ROMAN8
    | "R8" -> R8
    | "ROMAN8" -> ROMAN8
    | "CSHPROMAN8" -> CSHPROMAN8

    | "NEXTSTEP" -> NEXTSTEP

    | "ARMSCII-8" -> ARMSCII_8

    | "GEORGIAN-ACADEMY" -> GEORGIAN_ACADEMY

    | "GEORGIAN-PS" -> GEORGIAN_PS

    | "KOI8-T" -> KOI8_T

    | "MULELAO-1" -> MULELAO_1

    | "CP1133" -> CP1133
    | "IBM-CP1133" -> IBM_CP1133

    | "ISO-IR-166" -> ISO_IR_166
    | "TIS-620" -> TIS_620
    | "TIS620" -> TIS620
    | "TIS620-0" -> TIS620_0
    | "TIS620.2529-1" -> TIS620_2529_1
    | "TIS620.2533-0" -> TIS620_2533_0
    | "TIS620.2533-1" -> TIS620_2533_1

    | "CP874" -> CP874
    | "WINDOWS-874" -> WINDOWS_874

    | "VISCII" -> VISCII
    | "VISCII1.1-1" -> VISCII1_1_1
    | "CSVISCII" -> CSVISCII

    | "TCVN" -> TCVN
    | "TCVN-5712" -> TCVN_5712
    | "TCVN5712-1" -> TCVN5712_1
    | "TCVN5712-1:1993" -> TCVN5712_1_1993

    | "ISO-IR-14" -> ISO_IR_14
    | "ISO646-JP" -> ISO646_JP
    | "JIS_C6220-1969-RO" -> JIS_C6220_1969_RO
    | "JP" -> JP
    | "CSISO14JISC6220RO" -> CSISO14JISC6220RO

    | "JISX0201-1976" -> JISX0201_1976
    | "JIS_X0201" -> JIS_X0201
    | "X0201" -> X0201
    | "CSHALFWIDTHKATAKANA" -> CSHALFWIDTHKATAKANA

    | "ISO-IR-87" -> ISO_IR_87
    | "JIS0208" -> JIS0208
    | "JIS_C6226-1983" -> JIS_C6226_1983
    | "JIS_X0208" -> JIS_X0208
    | "JIS_X0208-1983" -> JIS_X0208_1983
    | "JIS_X0208-1990" -> JIS_X0208_1990
    | "X0208" -> X0208
    | "CSISO87JISX0208" -> CSISO87JISX0208

    | "ISO-IR-159" -> ISO_IR_159
    | "JIS_X0212" -> JIS_X0212
    | "JIS_X0212-1990" -> JIS_X0212_1990
    | "JIS_X0212.1990-0" -> JIS_X0212_1990_0
    | "X0212" -> X0212
    | "CSISO159JISX02121990" -> CSISO159JISX02121990

    | "CN" -> CN
    | "GB_1988-80" -> GB_1988_80
    | "ISO-IR-57" -> ISO_IR_57
    | "ISO646-CN" -> ISO646_CN
    | "CSISO57GB1988" -> CSISO57GB1988

    | "CHINESE" -> CHINESE
    | "GB_2312-80" -> GB_2312_80
    | "ISO-IR-58" -> ISO_IR_58
    | "CSISO58GB231280" -> CSISO58GB231280

    | "CN-GB-ISOIR165" -> CN_GB_ISOIR165
    | "ISO-IR-165" -> ISO_IR_165

    | "ISO-IR-149" -> ISO_IR_149
    | "KOREAN" -> KOREAN
    | "KSC_5601" -> KSC_5601
    | "KS_C_5601-1987" -> KS_C_5601_1987
    | "KS_C_5601-1989" -> KS_C_5601_1989
    | "CSKSC56011987" -> CSKSC56011987

    | "EUC-JP" -> EUC_JP
    | "EUCJP" -> EUCJP
    | "EXTENDED_UNIX_CODE_PACKED_FORMAT_FOR_JAPANESE" -> EXTENDED_UNIX_CODE_PACKED_FORMAT_FOR_JAPANESE
    | "CSEUCPKDFMTJAPANESE" -> CSEUCPKDFMTJAPANESE

    | "MS_KANJI" -> MS_KANJI
    | "SHIFT-JIS" -> SHIFT_JIS
    | "SHIFT_JIS" -> SHIFT_JIS
    | "SJIS" -> SJIS
    | "CSSHIFTJIS" -> CSSHIFTJIS

    | "CP932" -> CP932

    | "ISO-2022-JP" -> ISO_2022_JP
    | "CSISO2022JP" -> CSISO2022JP

    | "ISO-2022-JP-1" -> ISO_2022_JP_1

    | "ISO-2022-JP-2" -> ISO_2022_JP_2
    | "CSISO2022JP2" -> CSISO2022JP2

    | "CN-GB" -> CN_GB
    | "EUC-CN" -> EUC_CN
    | "EUCCN" -> EUCCN
    | "GB2312" -> GB2312
    | "CSGB2312" -> CSGB2312

    | "CP936" -> CP936
    | "GBK" -> GBK

    | "GB18030" -> GB18030

    | "ISO-2022-CN" -> ISO_2022_CN
    | "CSISO2022CN" -> CSISO2022CN

    | "ISO-2022-CN-EXT" -> ISO_2022_CN_EXT

    | "HZ" -> HZ
    | "HZ-GB-2312" -> HZ_GB_2312

    | "EUC-TW" -> EUC_TW
    | "EUCTW" -> EUCTW
    | "CSEUCTW" -> CSEUCTW

    | "BIG-5" -> BIG_5
    | "BIG-FIVE" -> BIG_FIVE
    | "BIG5" -> BIG5
    | "BIGFIVE" -> BIGFIVE
    | "CN-BIG5" -> CN_BIG5
    | "CSBIG5" -> CSBIG5

    | "CP950" -> CP950

    | "BIG5-HKSCS" -> BIG5_HKSCS
    | "BIG5HKSCS" -> BIG5HKSCS

    | "EUC-KR" -> EUC_KR
    | "EUCKR" -> EUCKR
    | "CSEUCKR" -> CSEUCKR

    | "CP949" -> CP949
    | "UHC" -> UHC

    | "CP1361" -> CP1361
    | "JOHAB" -> JOHAB

    | "ISO-2022-KR" -> ISO_2022_KR
    | "CSISO2022KR" -> CSISO2022KR

    | "437" -> I_437
    | "CP437" -> CP437
    | "IBM437" -> IBM437
    | "CSPC8CODEPAGE437" -> CSPC8CODEPAGE437

    | "CP737" -> CP737

    | "CP775" -> CP775
    | "IBM775" -> IBM775
    | "CSPC775BALTIC" -> CSPC775BALTIC

    | "852" -> I_852
    | "CP852" -> CP852
    | "IBM852" -> IBM852
    | "CSPCP852" -> CSPCP852

    | "CP853" -> CP853

    | "855" -> I_855
    | "CP855" -> CP855
    | "IBM855" -> IBM855
    | "CSIBM855" -> CSIBM855

    | "857" -> I_857
    | "CP857" -> CP857
    | "IBM857" -> IBM857
    | "CSIBM857" -> CSIBM857

    | "CP858" -> CP858

    | "860" -> I_860
    | "CP860" -> CP860
    | "IBM860" -> IBM860
    | "CSIBM860" -> CSIBM860

    | "861" -> I_861
    | "CP-IS" -> CP_IS
    | "CP861" -> CP861
    | "IBM861" -> IBM861
    | "CSIBM861" -> CSIBM861

    | "863" -> I_863
    | "CP863" -> CP863
    | "IBM863" -> IBM863
    | "CSIBM863" -> CSIBM863

    | "CP864" -> CP864
    | "IBM864" -> IBM864
    | "CSIBM864" -> CSIBM864

    | "865" -> I_865
    | "CP865" -> CP865
    | "IBM865" -> IBM865
    | "CSIBM865" -> CSIBM865

    | "869" -> I_869
    | "CP-GR" -> CP_GR
    | "CP869" -> CP869
    | "IBM869" -> IBM869
    | "CSIBM869" -> CSIBM869

    | "CP1125" -> CP1125

    | _ -> ASCII


(**********************************************************************************)
(*                                                                                *)
(*                          normalize_language                                    *)
(*                                                                                *)
(**********************************************************************************)

let normalize_language s =
  let s = String.uppercase s in
  if String.length s > 1
  then begin
(* We have to distinguish between ZH_tw and ZH_cn here
   ZH_tw = BIG5/Chinese traditional -- ZH_cn = GBK/Chinese simplified *)
    if String.sub s 0 2 = "ZH" && String.length s > 4 then
      String.sub s 0 5
    else
      String.sub s 0 2
  end else "EN"

(**********************************************************************************)
(*                                                                                *)
(*                          variables                                             *)
(*                                                                                *)
(**********************************************************************************)

(*
let charset_aliases =
[
 [ANSI_X3_4_1968; ANSI_X3_4_1986; ASCII; CP367; IBM367; ISO_IR_6; ISO646_US; ISO_646_IRV_1991; US; US_ASCII; CSASCII];
 [UTF_8];
 [ISO_10646_UCS_2; UCS_2; CSUNICODE];
 [UCS_2BE; UNICODE_1_1; UNICODEBIG; CSUNICODE11];
 [UCS_2LE; UNICODELITTLE];
 [ISO_10646_UCS_4; UCS_4; CSUCS4];
 [UCS_4BE];
 [UCS_4LE];
 [UTF_16];
 [UTF_16BE];
 [UTF_16LE];
 [UTF_32];
 [UTF_32BE];
 [UTF_32LE];
 [UNICODE_1_1_UTF_7; UTF_7; CSUNICODE11UTF7];
 [UCS_2_INTERNAL];
 [UCS_2_SWAPPED];
 [UCS_4_INTERNAL];
 [UCS_4_SWAPPED];
 [C99];
 [JAVA];
 [CP819; IBM819; ISO_8859_1; ISO_IR_100; ISO8859_1; ISO_8859_1_1987; L1; LATIN1; CSISOLATIN1];
 [ISO_8859_2; ISO_IR_101; ISO8859_2; ISO_8859_2_1987; L2; LATIN2; CSISOLATIN2];
 [ISO_8859_3; ISO_IR_109; ISO8859_3; ISO_8859_3_1988; L3; LATIN3; CSISOLATIN3];
 [ISO_8859_4; ISO_IR_110; ISO8859_4; ISO_8859_4_1988; L4; LATIN4; CSISOLATIN4];
 [CYRILLIC; ISO_8859_5; ISO_IR_144; ISO8859_5; ISO_8859_5_1988; CSISOLATINCYRILLIC];
 [ARABIC; ASMO_708; ECMA_114; ISO_8859_6; ISO_IR_127; ISO8859_6; ISO_8859_6_1987; CSISOLATINARABIC];
 [ECMA_118; ELOT_928; GREEK; GREEK8; ISO_8859_7; ISO_IR_126; ISO8859_7; ISO_8859_7_1987; CSISOLATINGREEK];
 [HEBREW; ISO_8859_8; ISO_IR_138; ISO8859_8; ISO_8859_8_1988; CSISOLATINHEBREW];
 [ISO_8859_9; ISO_IR_148; ISO8859_9; ISO_8859_9_1989; L5; LATIN5; CSISOLATIN5];
 [ISO_8859_10; ISO_IR_157; ISO8859_10; ISO_8859_10_1992; L6; LATIN6; CSISOLATIN6];
 [ISO_8859_13; ISO_IR_179; ISO8859_13; L7; LATIN7];
 [ISO_8859_14; ISO_CELTIC; ISO_IR_199; ISO_8859_14; ISO_8859_14_1998; L8; LATIN8];
 [ISO_8859_15; ISO_IR_203; ISO8859_15; ISO_8859_15_1998];
 [ISO_8859_16; ISO_IR_226; ISO8859_16; ISO_8859_16_2000];
 [KOI8_R; CSKOI8R];
 [KOI8_U];
 [KOI8_RU];
 [CP1250; MS_EE; WINDOWS_1250];
 [CP1251; MS_CYRL; WINDOWS_1251];
 [CP1252; MS_ANSI; WINDOWS_1252];
 [CP1253; MS_GREEK; WINDOWS_1253];
 [CP1254; MS_TURK; WINDOWS_1254];
 [CP1255; MS_HEBR; WINDOWS_1255];
 [CP1256; MS_ARAB; WINDOWS_1256];
 [CP1257; WINBALTRIM; WINDOWS_1257];
 [CP1258; WINDOWS_1258];
 [I_850; CP850; IBM850; CSPC850MULTILINGUAL];
 [I_862; CP862; IBM862; CSPC862LATINHEBREW];
 [I_866; CP866; IBM866; CSIBM866];
 [MAC; MACINTOSH; MACROMAN; CSMACINTOSH];
 [MACCENTRALEUROPE];
 [MACICELAND];
 [MACCROATIAN];
 [MACROMANIA];
 [MACCYRILLIC];
 [MACUKRAINE];
 [MACGREEK];
 [MACTURKISH];
 [MACHEBREW];
 [MACARABIC];
 [MACTHAI];
 [HP_ROMAN8; R8; ROMAN8; CSHPROMAN8];   (* no region *)
 [NEXTSTEP];                            (* no region *)
 [ARMSCII_8];
 [GEORGIAN_ACADEMY];
 [GEORGIAN_PS];
 [KOI8_T];
 [MULELAO_1];
 [CP1133; IBM_CP1133];
 [ISO_IR_166; TIS_620; TIS620; TIS620_0; TIS620_2529_1; TIS620_2533_0; TIS620_2533_1];
 [CP874; WINDOWS_874];
 [VISCII; VISCII1_1_1; CSVISCII];
 [TCVN; TCVN_5712; TCVN5712_1; TCVN5712_1_1993];
 [ISO_IR_14; ISO646_JP; JIS_C6220_1969_RO; JP; CSISO14JISC6220RO];
 [JISX0201_1976; JIS_X0201; X0201; CSHALFWIDTHKATAKANA];
 [ISO_IR_87; JIS0208; JIS_C6226_1983; JIS_X0208; JIS_X0208_1983; JIS_X0208_1990; X0208; CSISO87JISX0208];
 [ISO_IR_159; JIS_X0212; JIS_X0212_1990; JIS_X0212_1990_0; X0212; CSISO159JISX02121990];
 [CN; GB_1988_80; ISO_IR_57; ISO646_CN; CSISO57GB1988];
 [CHINESE; GB_2312_80; ISO_IR_58; CSISO58GB231280];
 [CN_GB_ISOIR165; ISO_IR_165];
 [ISO_IR_149; KOREAN; KSC_5601; KS_C_5601_1987; KS_C_5601_1989; CSKSC56011987];
 [EUC_JP; EUCJP; EXTENDED_UNIX_CODE_PACKED_FORMAT_FOR_JAPANESE; CSEUCPKDFMTJAPANESE];
 [MS_KANJI; SHIFT_JIS; SJIS; CSSHIFTJIS];
 [CP932];
 [ISO_2022_JP; CSISO2022JP];
 [ISO_2022_JP_1];
 [ISO_2022_JP_2; CSISO2022JP2];
 [CN_GB; EUC_CN; EUCCN; GB2312; CSGB2312];
 [CP936; GBK];
 [GB18030];
 [ISO_2022_CN; CSISO2022CN];
 [ISO_2022_CN_EXT];
 [HZ; HZ_GB_2312];
 [EUC_TW; EUCTW; CSEUCTW];
 [BIG_5; BIG_FIVE; BIG5; BIGFIVE; CN_BIG5; CSBIG5];
 [CP950];
 [BIG5_HKSCS; BIG5HKSCS];
 [EUC_KR; EUCKR; CSEUCKR];
 [CP949; UHC];
 [CP1361; JOHAB];
 [ISO_2022_KR; CSISO2022KR];
 [I_437; CP437; IBM437; CSPC8CODEPAGE437];
 [CP737];
 [CP775; IBM775; CSPC775BALTIC];
 [I_852; CP852; IBM852; CSPCP852];
 [CP853];                            (* no region *)
 [I_855; CP855; IBM855; CSIBM855];
 [I_857; CP857; IBM857; CSIBM857];
 [CP858];                            (* no region *)
 [I_860; CP860; IBM860; CSIBM860];
 [I_861; CP_IS; CP861; IBM861; CSIBM861];
 [I_863; CP863; IBM863; CSIBM863];
 [CP864; IBM864; CSIBM864];
 [I_865; CP865; IBM865; CSIBM865];
 [I_869; CP_GR; CP869; IBM869; CSIBM869];
 [CP1125];
]
*)

let ascii =
  [
   [ANSI_X3_4_1968; ANSI_X3_4_1986; ASCII; CP367; IBM367; ISO_IR_6; ISO646_US; ISO_646_IRV_1991; US; US_ASCII; CSASCII];
   [C99];
  ]

let arabic =
  [
   [CP1256; MS_ARAB; WINDOWS_1256];
   [ARABIC; ASMO_708; ECMA_114; ISO_8859_6; ISO_IR_127; ISO8859_6; ISO_8859_6_1987; CSISOLATINARABIC];
   [CP864; IBM864; CSIBM864]; [CP864; IBM864; CSIBM864];
   [MACARABIC];
  ]

let armenian =
  [
   [ARMSCII_8];
  ]

let baltic =
  [
   [CP1257; WINBALTRIM; WINDOWS_1257];
   [ISO_8859_13; ISO_IR_179; ISO8859_13; L7; LATIN7];
   [ISO_8859_4; ISO_IR_110; ISO8859_4; ISO_8859_4_1988; L4; LATIN4; CSISOLATIN4];
   [CP775; IBM775; CSPC775BALTIC];
   [MACUKRAINE];
   [MACCROATIAN];
  ]

let celtic =
  [
   [ISO_8859_14; ISO_CELTIC; ISO_IR_199; ISO8859_14; ISO_8859_14_1998; L8; LATIN8];
  ]

let central_european =
  [
   [CP1250; MS_EE; WINDOWS_1250];
   [ISO_8859_2; ISO_IR_101; ISO8859_2; ISO_8859_2_1987; L2; LATIN2; CSISOLATIN2];
   [I_852; CP852; IBM852; CSPCP852];
   [MACCENTRALEUROPE];
  ]

let chinese_simplified =
  [
   [GB18030];
   [CP936; GBK];
   [CN_GB; EUC_CN; EUCCN; GB2312; CSGB2312];
   [CHINESE; GB_2312_80; ISO_IR_58; CSISO58GB231280];
   [CN; GB_1988_80; ISO_IR_57; ISO646_CN; CSISO57GB1988];
   [CN_GB_ISOIR165; ISO_IR_165];
   [ISO_2022_CN; CSISO2022CN];
   [ISO_2022_CN_EXT];
   [HZ; HZ_GB_2312];
  ]

let chinese_traditional =
  [
   [BIG_5; BIG_FIVE; BIG5; BIGFIVE; CN_BIG5; CSBIG5];
   [BIG5_HKSCS; BIG5HKSCS];
   [EUC_TW; EUCTW; CSEUCTW];
   [CP950];
  ]

let cyrillic =
  [
   [CP1251; MS_CYRL; WINDOWS_1251];
   [I_866; CP866; IBM866; CSIBM866];
   [KOI8_R; CSKOI8R];
   [CYRILLIC; ISO_8859_5; ISO_IR_144; ISO8859_5; ISO_8859_5_1988; CSISOLATINCYRILLIC];
   [I_855; CP855; IBM855; CSIBM855];
   [KOI8_U];
   [KOI8_RU];
   [MACCYRILLIC];
   [CP1125];
  ]

let georgian =
  [
   [GEORGIAN_ACADEMY];
   [GEORGIAN_PS];
  ]

let greek =
  [
   [CP1253; MS_GREEK; WINDOWS_1253];
   [ECMA_118; ELOT_928; GREEK; GREEK8; ISO_8859_7; ISO_IR_126; ISO8859_7; ISO_8859_7_1987; CSISOLATINGREEK];
   [MACGREEK];
   [CP737];
   [I_869; CP_GR; CP869; IBM869; CSIBM869];
  ]

let hebrew =
  [
   [CP1255; MS_HEBR; WINDOWS_1255];
   [HEBREW; ISO_8859_8; ISO_IR_138; ISO8859_8; ISO_8859_8_1988; CSISOLATINHEBREW];
   [I_862; CP862; IBM862; CSPC862LATINHEBREW];
   [MACHEBREW];
  ]

let japanese =
  [
   [EUC_JP; EUCJP; EXTENDED_UNIX_CODE_PACKED_FORMAT_FOR_JAPANESE; CSEUCPKDFMTJAPANESE];
   [ISO_2022_JP; CSISO2022JP];
   [ISO_2022_JP_1];
   [ISO_2022_JP_2; CSISO2022JP2];
   [MS_KANJI; SHIFT_JIS; SJIS; CSSHIFTJIS];
   [ISO_IR_14; ISO646_JP; JIS_C6220_1969_RO; JP; CSISO14JISC6220RO];
   [JISX0201_1976; JIS_X0201; X0201; CSHALFWIDTHKATAKANA];
   [ISO_IR_87; JIS0208; JIS_C6226_1983; JIS_X0208; JIS_X0208_1983; JIS_X0208_1990; X0208; CSISO87JISX0208];
   [ISO_IR_159; JIS_X0212; JIS_X0212_1990; JIS_X0212_1990_0; X0212; CSISO159JISX02121990];
   [CP932];
  ]

let korean =
  [
   [CP949; UHC];
   [CP1361; JOHAB];
   [EUC_KR; EUCKR; CSEUCKR];
   [ISO_2022_KR; CSISO2022KR];
   [ISO_IR_149; KOREAN; KSC_5601; KS_C_5601_1987; KS_C_5601_1989; CSKSC56011987];
  ]

let nordic =
  [
   [ISO_8859_10; ISO_IR_157; ISO8859_10; ISO_8859_10_1992; L6; LATIN6; CSISOLATIN6];
   [I_861; CP_IS; CP861; IBM861; CSIBM861];
   [MACICELAND];
   [I_865; CP865; IBM865; CSIBM865];
  ]

let romanian =
  [
   [ISO_8859_16; ISO_IR_226; ISO8859_16; ISO_8859_16_2000];
   [MACROMANIA];
  ]

let south_european =
  [
   [ISO_8859_3; ISO_IR_109; ISO8859_3; ISO_8859_3_1988; L3; LATIN3; CSISOLATIN3];
  ]

let tajik =
  [
   [KOI8_T];
  ]

let thai =
  [
   [CP874; WINDOWS_874];
   [ISO_IR_166; TIS_620; TIS620; TIS620_0; TIS620_2529_1; TIS620_2533_0; TIS620_2533_1];
   [MULELAO_1];
   [CP1133; IBM_CP1133];
   [MACTHAI];
  ]

let turkish =
  [
   [CP1254; MS_TURK; WINDOWS_1254];
   [ISO_8859_9; ISO_IR_148; ISO8859_9; ISO_8859_9_1989; L5; LATIN5; CSISOLATIN5];
   [I_857; CP857; IBM857; CSIBM857];
   [MACTURKISH];
  ]

let unicode =
  [
   [ISO_10646_UCS_2; UCS_2; CSUNICODE];
   [UCS_2BE; UNICODE_1_1; UNICODEBIG; CSUNICODE11];
   [UCS_2LE; UNICODELITTLE];
   [ISO_10646_UCS_4; UCS_4; CSUCS4];
   [UCS_4BE];
   [UCS_4LE];
   [UTF_16];
   [UTF_16BE];
   [UTF_16LE];
   [UTF_32];
   [UTF_32BE];
   [UTF_32LE];
   [UNICODE_1_1_UTF_7; UTF_7; CSUNICODE11UTF7];
   [UCS_2_INTERNAL];
   [UCS_2_SWAPPED];
   [UCS_4_INTERNAL];
   [UCS_4_SWAPPED];
  ]

let vietnamese =
  [
   [CP1258; WINDOWS_1258];
   [VISCII; VISCII1_1_1; CSVISCII];
   [TCVN; TCVN_5712; TCVN5712_1; TCVN5712_1_1993];
  ]

let western_european =
  [
   [CP1252; MS_ANSI; WINDOWS_1252];
   [ISO_8859_15; ISO_IR_203; ISO8859_15; ISO_8859_15_1998];
   [I_850; CP850; IBM850; CSPC850MULTILINGUAL];
   [CP819; IBM819; ISO_8859_1; ISO_IR_100; ISO8859_1; ISO_8859_1; ISO_8859_1_1987; L1; LATIN1; CSISOLATIN1];
   [MAC; MACINTOSH; MACROMAN; CSMACINTOSH];
   [I_437; CP437; IBM437; CSPC8CODEPAGE437];
   [I_860; CP860; IBM860; CSIBM860];
   [I_863; CP863; IBM863; CSIBM863];
  ]

let convert ~from_charset ~to_charset s =
  if s <> "" then begin
    let t = charset_to_string to_charset in
    let f = charset_to_string from_charset in
    convert_string s t f
  end else s

let safe_convert enc s =
  match enc with
  | "" -> s
  | enc ->
    try
      convert
        ~from_charset: (charset_from_string enc)
        ~to_charset:UTF_8
        s
    with _ -> s

(* Locale specific conversions *)
module Locale = struct

(* FIXME move away! *)
let () =
  (* block signals until core started correctly *)
  (MlUnix.set_signal  Sys.sigint
    (Sys.Signal_handle (fun _ -> ())));
  (MlUnix.set_signal  Sys.sigterm
    (Sys.Signal_handle (fun _ -> ())))

let locale =
  try
    let cs = get_charset () in
    charset_from_string cs
  with _ -> ASCII

let locale_string = charset_to_string locale

let (enc_list : string list ref) = ref []
let nenc = ref 0
let char_const = "_"

let default_language =
  let s = get_default_language () in
  let s = normalize_language s in
  s

(*
let all_regions =
  [
   ascii;
   arabic;
   armenian;
   baltic;
   celtic;
   central_european;
   chinese_simplified;
   chinese_traditional;
   cyrillic;
   georgian;
   greek;
   hebrew;
   japanese;
   korean;
   nordic;
   romanian;
   south_european;
   tajik;
   thai;
   turkish;
   vietnamese;
   western_european;
  ]
*)

(* See http://www.gnu.org/software/gettext/manual/html_chapter/gettext_15.html#SEC221
 * The strategy is not perfect. Any comment to improve it, is highly appreciated.
 * The charset list shall be improved according to the language detected on the
 * target machine.
 *)

let charset_list_from_language lang =
  let li = ref [] in
  li := ascii :: unicode :: !li;
  let _ =
    match lang with
        "AR" -> li := arabic :: !li
      | "HY" -> li := armenian :: !li
      | "LT"
      | "LV"
      | "MI" -> li := baltic :: !li
      | "CY" -> li := celtic :: western_european :: !li
      | "BS"
      | "CS"
      | "HR"
      | "HU"
      | "PL"
      | "SK"
      | "SL" -> li := central_european :: !li
      | "SH"
      | "SR" -> li := central_european :: cyrillic ::!li
      | "ZH_CN" -> li := chinese_simplified :: !li
      | "ZH_TW" -> li := chinese_traditional :: !li
      | "ZH" -> li := chinese_traditional :: chinese_simplified :: !li
      | "BE"
      | "BG"
      | "MK"
      | "RU"
      | "UK" -> li := cyrillic :: !li
      | "KA" -> li := georgian :: !li
      | "EL" -> li := greek :: !li
      | "YI"
      | "HE"
      | "IW" -> li := hebrew :: !li
      | "JA" -> li := japanese :: !li
      | "KO" -> li := korean :: !li
      | "RO" -> li := romanian :: central_european :: !li
      | "MT" -> li := south_european :: !li
      | "TG" -> li := tajik :: !li
      | "TH" -> li := thai :: !li
      | "TR" -> li := turkish :: !li
      | "VI" -> li := vietnamese :: !li
      | "AF"
      | "AN"
      | "BR"
      | "CA"
      | "DA"
      | "DE"
      | "EN"
      | "ES"
      | "ET"
      | "EU"
      | "FI"
      | "FO"
      | "FR"
      | "GA"
      | "GL"
      | "GV"
      | "ID"
      | "IS"
      | "IT"
      | "KL"
      | "KW"
      | "MS"
      | "NL"
      | "NN"
      | "NO"
      | "OC"
      | "PT"
      | "SQ"
      | "SV"
      | "TL"
      | "UZ"
      | "WA" -> li := western_european :: !li
      | _ -> ()
  in
  List.flatten !li

let set_default_charset_list (lang : string) =
  (* Let's get rid of charset aliases *)
  let l = List.map (fun li -> List.hd li) (charset_list_from_language lang) in
  enc_list := List.map (fun c -> charset_to_string c ) l;
(* Printf2.lprintf "List of charmap used to convert the strings:\n";
  List.iter (fun enc ->
    Printf2.lprintf "  Use encoding %s\n" enc; 
  ) !enc_list; *)
  nenc := List.length !enc_list

let conversion_enabled = ref true

let slow_encode_from_utf8 s to_codeset =
  let us = ref "" in
  let slen = utf8_length s in
  let buf = Buffer.create 10 in
  for i = 0 to (slen - 1) do
    try
      let uchar = utf8_get s i in
      add_uchar buf uchar;
      let s' = Buffer.contents buf in
      Buffer.reset buf;
      let s' = convert_string s' to_codeset "UTF-8" in
      us := !us ^ s'
    with _ ->
      us := !us ^ char_const
  done;
  !us

let slow_encode s to_codeset =
  if is_utf8 s
  then slow_encode_from_utf8 s to_codeset
  else begin
    let us = ref "" in
    let slen = String.length s in
    for i = 0 to (slen - 1) do
      try
        us := !us ^ (convert_string (String.sub s i 1) to_codeset locale_string)
      with _ ->
        us := !us ^ char_const
    done;
    !us
  end

let fast_encode s to_codeset =
  let rec iter i n =
     if i = n 
       then slow_encode s to_codeset
       else
         try
           let from_codeset = List.nth !enc_list i in
           convert_string s to_codeset from_codeset
         with _ -> iter (i + 1) !nenc
  in iter 0 !nenc

let to_utf8 s =
  if s = ""
  then s
  else if is_utf8 s
  then s
  else fast_encode s "UTF-8"

let to_locale s =
  if s = "" || not !conversion_enabled
  then s
  else begin
    let s = to_utf8 s in
    match locale with
        UTF_8 -> s
      | _ ->
          begin
            try
              convert_string s locale_string "UTF-8"
            with _ ->
              slow_encode_from_utf8 s locale_string
          end
  end

let () =
  set_default_charset_list default_language

end (* Locale *)

