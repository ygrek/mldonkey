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

(** [convert ~from_charset ~to_charset s]
    raise CharsetError if the string s is not entirely convertible. *)
val convert : from_charset : charset -> to_charset : charset -> string -> string
val safe_convert: string -> string -> string

(** [is_utf8 s]
    returns TRUE if s is a valid UTF-8, otherwise returns FALSE.
    Other functions assume strings are valid UTF-8, so it is prudent
    to test their validity for strings from untrusted origins. *)
val is_utf8 : string -> bool

(** [to_utf8 s]
    Converts the input string to UTF-8. *)
val to_utf8 : string -> string

(** [to_locale s]
    Converts the input string to the encoding of the current locale. *)
val to_locale : string -> string

(** [utf8_get s n]
    returns [n]-th Unicode character of [s].
    The call requires O(n)-time. *)
val utf8_get : string -> int -> uchar

(** [utf8_length s]
    returns the number of Unicode characters contained in s *)
val utf8_length : string -> int

(** [add_uchar buf u]
    add one Unicode character to the buffer. *)
val add_uchar : Buffer.t -> uchar -> unit

val default_language : string
val locstr : string
val conversion_enabled : bool ref
