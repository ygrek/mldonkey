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

open Printf2

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

let (_, charset) = Glib.Convert.get_charset ()

let enc_list = ref []
let nenc = ref 0

let lang_to_string lang =
  match lang with
      AR -> "AR"
    | HY -> "HY"
    | LT -> "LT"
    | LV -> "LV"
    | MI -> "MI"
    | CY -> "CY"
    | BS -> "BS"
    | CS -> "CS"
    | HR -> "HR"
    | HU -> "HU"
    | PL -> "PL"
    | SK -> "SK"
    | SL -> "SL"
    | SH -> "SH"
    | SR -> "SR"
    | ZH -> "ZH"
    | BE -> "BE"
    | BG -> "BG"
    | MK -> "MK"
    | RU -> "RU"
    | UK -> "UK"
    | KA -> "KA"
    | EL -> "EL"
    | HE -> "HE"
    | IW -> "IW"
    | JA -> "JA"
    | KO -> "KO"
    | RO -> "RO"
    | MT -> "MT"
    | TG -> "TG"
    | TH -> "TH"
    | TR -> "TR"
    | VI -> "VI"
    | AF -> "AF"
    | AN -> "AN"
    | BR -> "BR"
    | CA -> "CA"
    | DA -> "DA"
    | DE -> "DE"
    | EN -> "EN"
    | ES -> "ES"
    | ET -> "ET"
    | EU -> "EU"
    | FI -> "FI"
    | FO -> "FO"
    | FR -> "FR"
    | GA -> "GA"
    | GL -> "GL"
    | GV -> "GV"
    | ID -> "ID"
    | IS -> "IS"
    | IT -> "IT"
    | KL -> "KL"
    | KW -> "KW"
    | MS -> "MS"
    | NL -> "NL"
    | NN -> "NN"
    | NO -> "NO"
    | OC -> "OC"
    | PT -> "PT"
    | SQ -> "SQ"
    | SV -> "SV"
    | TL -> "TL"
    | UZ -> "UZ"
    | WA -> "WA"
    | YI -> "YI"
    | UnknownLang -> "EN"

let string_to_lang s =
  match s with
    "AR" -> AR
  | "HY" -> HY
  | "LT" -> LT
  | "LV" -> LV
  | "MI" -> MI
  | "CY" -> CY
  | "BS" -> BS
  | "CS" -> CS
  | "HR" -> HR
  | "HU" -> HU
  | "PL" -> PL
  | "SK" -> SK
  | "SL" -> SL
  | "SH" -> SH
  | "SR" -> SR
  | "ZH" -> ZH
  | "BE" -> BE
  | "BG" -> BG
  | "MK" -> MK
  | "RU" -> RU
  | "UK" -> UK
  | "KA" -> KA
  | "EL" -> EL
  | "HE" -> HE
  | "IW" -> IW
  | "JA" -> JA
  | "KO" -> KO
  | "RO" -> RO
  | "MT" -> MT
  | "TG" -> TG
  | "TH" -> TH
  | "TR" -> TR
  | "VI" -> VI
  | "AF" -> AF
  | "AN" -> AN
  | "BR" -> BR
  | "CA" -> CA
  | "DA" -> DA
  | "DE" -> DE
  | "EN" -> EN
  | "ES" -> ES
  | "ET" -> ET
  | "EU" -> EU
  | "FI" -> FI
  | "FO" -> FO
  | "FR" -> FR
  | "GA" -> GA
  | "GL" -> GL
  | "GV" -> GV
  | "ID" -> ID
  | "IS" -> IS
  | "IT" -> IT
  | "KL" -> KL
  | "KW" -> KW
  | "MS" -> MS
  | "NL" -> NL
  | "NN" -> NN
  | "NO" -> NO
  | "OC" -> OC
  | "PT" -> PT
  | "SQ" -> SQ
  | "SV" -> SV
  | "TL" -> TL
  | "UZ" -> UZ
  | "WA" -> WA
  | "YI" -> YI
  |  _ -> UnknownLang

let codeset_to_string codeset =
  match codeset with
     WINDOWS_1256 -> "WINDOWS-1256"
  |  ISO_8859_6 -> "ISO-8859-6"
  |  IBM864 -> "IBM864"
  |  ARMSCII_8 -> "ARMSCII-8"
  |  WINDOWS_1257 -> "WINDOWS-1257"
  |  ISO_8859_13 -> "ISO-8859-13"
  |  ISO_8859_4 -> "ISO-8859-4"
  |  ISO_8859_14 -> "ISO-8859-14"
  |  WINDOWS_1250 -> "WINDOWS-1250"
  |  ISO_8859_2 -> "ISO-8859-2"
  |  IBM852 -> "IBM852"
  |  GB18030 -> "GB18030"
  |  GBK -> "GBK"
  |  GB2312 -> "GB2312"
  |  BIG5_HKSCS -> "BIG5-HKSCS"
  |  BIG5 -> "BIG5"
  |  EUC_TW -> "EUC-TW"
  |  WINDOWS_1251 -> "WINDOWS-1251"
  |  CP1251 -> "CP1251"
  |  CP866 -> "CP866"
  |  KOI8_R -> "KOI8-R"
  |  ISO_8859_5 -> "ISO-8859-5"
  |  IBM855 -> "IBM855"
  |  ISO_IR_111 -> "ISO-IR-111"
  |  KOI8R -> "KOI8R"
  |  KOI8U -> "KOI8U"
  |  GEORGIAN_ACADEMYE -> "GEORGIAN-ACADEMYE"
  |  WINDOWS_1253 -> "WINDOWS-1253"
  |  ISO_8859_7 -> "ISO-8859-7"
  |  WINDOWS_1255 -> "WINDOWS-1255"
  |  ISO_8859_8 -> "ISO-8859-8"
  |  IBM862 -> "IBM862"
  |  EUC_JP -> "EUC-JP"
  |  ISO_2022_JP -> "ISO-2022-JP"
  |  SHIFT_JIS -> "SHIFT-JIS"
  |  UHC -> "UHC"
  |  JOHAB -> "JOHAB"
  |  EUC_KR -> "EUC-KR"
  |  ISO_2022_KR -> "ISO-2022-KR"
  |  ISO_8859_10 -> "ISO-8859-10"
  |  ISO_8859_16 -> "ISO-8859-16"
  |  ISO_8859_3 -> "ISO-8859-3"
  |  KOI8_T -> "KOI8-T"
  |  TIS_620 -> "TIS-620"
  |  WINDOWS_1254 -> "WINDOWS-1254"
  |  ISO_8859_9 -> "ISO-8859-9"
  |  IBM857 -> "IBM857"
  |  UCS_2 -> "UCS-2"
  |  UTF_8 -> "UTF-8"
  |  UTF_7 -> "UTF-7"
  |  UTF_16 -> "UTF-16"
  |  UTF_32 -> "UTF-32"
  |  UCS_4 -> "UCS-4"
  |  WINDOWS_1258 -> "WINDOWS-1258"
  |  VISCII -> "VISCII"
  |  TCVN -> "TCVN"
  |  WINDOWS_1252 -> "WINDOWS-1252"
  |  ISO_8859_15 -> "ISO-8859-15"
  |  IBM850 -> "IBM850"
  |  ISO_8859_1 -> "ISO-8859-1"

let string_to_codeset s =
  match s with
     "WINDOWS-1256" -> WINDOWS_1256
  |  "ISO-8859-6" -> ISO_8859_6
  |  "IBM864" -> IBM864
  |  "ARMSCII-8" -> ARMSCII_8
  |  "WINDOWS-1257" -> WINDOWS_1257
  |  "ISO-8859-13" -> ISO_8859_13
  |  "ISO-8859-4" -> ISO_8859_4
  |  "ISO-8859-14" -> ISO_8859_14
  |  "WINDOWS-1250" -> WINDOWS_1250
  |  "ISO-8859-2" -> ISO_8859_2
  |  "IBM852" -> IBM852
  |  "GB18030" -> GB18030
  |  "GBK" -> GBK
  |  "GB2312" -> GB2312
  |  "BIG5-HKSCS" -> BIG5_HKSCS
  |  "BIG5" -> BIG5
  |  "EUC-TW" -> EUC_TW
  |  "WINDOWS-1251" -> WINDOWS_1251
  |  "CP1251" -> CP1251
  |  "CP866" -> CP866
  |  "KOI8-R" -> KOI8_R
  |  "ISO-8859-5" -> ISO_8859_5
  |  "IBM855" -> IBM855
  |  "ISO-IR-111" -> ISO_IR_111
  |  "KOI8R" -> KOI8R
  |  "KOI8U" -> KOI8U
  |  "GEORGIAN-ACADEMYE" -> GEORGIAN_ACADEMYE
  |  "WINDOWS-1253" -> WINDOWS_1253
  |  "ISO-8859-7" -> ISO_8859_7
  |  "WINDOWS-1255" -> WINDOWS_1255
  |  "ISO-8859-8" -> ISO_8859_8
  |  "IBM862" -> IBM862
  |  "EUC-JP" -> EUC_JP
  |  "ISO-2022-JP" -> ISO_2022_JP
  |  "SHIFT-JIS" -> SHIFT_JIS
  |  "UHC" -> UHC
  |  "JOHAB" -> JOHAB
  |  "EUC-KR" -> EUC_KR
  |  "ISO-2022-KR" -> ISO_2022_KR
  |  "ISO-8859-10" -> ISO_8859_10
  |  "ISO-8859-16" -> ISO_8859_16
  |  "ISO-8859-3" -> ISO_8859_3
  |  "KOI8-T" -> KOI8_T
  |  "TIS-620" -> TIS_620
  |  "WINDOWS-1254" -> WINDOWS_1254
  |  "ISO-8859-9" -> ISO_8859_9
  |  "IBM857" -> IBM857
  |  "UCS-2" -> UCS_2
  |  "UTF-8" -> UTF_8
  |  "UTF-7" -> UTF_7
  |  "UTF-16" -> UTF_16
  |  "UTF-32" -> UTF_32
  |  "UCS-4" -> UCS_4
  |  "WINDOWS-1258" -> WINDOWS_1258
  |  "VISCII" -> VISCII
  |  "TCVN" -> TCVN
  |  "WINDOWS-1252" -> WINDOWS_1252
  |  "ISO-8859-15" -> ISO_8859_15
  |  "IBM850" -> IBM850
  |  _ -> ISO_8859_1

let arabic =
  [
   WINDOWS_1256;
   ISO_8859_6;
   IBM864;
  ]

let armenian =
  [
   ARMSCII_8;
  ]

let baltic =
  [
   WINDOWS_1257;
   ISO_8859_13;
   ISO_8859_4;
  ]

let celtic =
  [
   ISO_8859_14;
  ]

let central_european =
  [
   WINDOWS_1250;
   ISO_8859_2;
   IBM852;
  ]

let chinese_simplified =
  [
   GB18030;
   GBK;
   GB2312;
  ]

let chinese_traditional =
  [
   BIG5_HKSCS;
   BIG5;
   EUC_TW;
  ]

let cyrillic =
  [
   WINDOWS_1251;
   CP1251;
   CP866;
   KOI8_R;
   ISO_8859_5;
   IBM855;
   ISO_IR_111;
   KOI8R;
   KOI8U;
  ]

(*
let cyrillic_russian =
  [
   CP866;
  ]

let cyrillic_ukrainian =
  [
   KOI8U;
  ]
*)

let georgian =
  [
   GEORGIAN_ACADEMYE;
  ]

let greek =
  [
   WINDOWS_1253;
   ISO_8859_7;
  ]

let hebrew =
  [
   WINDOWS_1255;
   ISO_8859_8;
   IBM862;
  ]

let japanese =
  [
   EUC_JP;
   ISO_2022_JP;
   SHIFT_JIS;
  ]

let korean =
  [
   UHC;
   JOHAB;
   EUC_KR;
   ISO_2022_KR;
  ]

let nordic =
  [
   ISO_8859_10;
  ]

let romanian =
  [
   ISO_8859_16;
  ]

let south_european =
  [
   ISO_8859_3;
  ]

let tajik =
  [
   KOI8_T;
  ]

let thai =
  [
   TIS_620;
  ]

let turkish =
  [
   WINDOWS_1254;
   ISO_8859_9;
   IBM857;
  ]

let unicode =
  [
   UCS_2;
  ]

(*
   UTF_8;
   UTF_7;
   UTF_16;
   UTF_32;
   UCS_4;
*)

let vietnamese =
  [
   WINDOWS_1258;
   VISCII;
   TCVN;
  ]

let western_european =
  [
   WINDOWS_1252;
   ISO_8859_15;
   IBM850;
   ISO_8859_1;
  ]

let codeset_list_from_region region =
  match region with
    Default -> List.map (fun s -> string_to_codeset s) !enc_list
  | Arabic -> arabic
  | Armenian -> armenian
  | Baltic -> baltic
  | Celtic -> celtic
  | CentralEuropean -> central_european
  | ChineseSimplified -> chinese_simplified
  | ChineseTraditional -> chinese_traditional
  | Cyrillic -> cyrillic
  | Georgian -> georgian
  | Greek -> greek
  | Hebrew -> hebrew
  | Japanese -> japanese
  | Korean -> korean
  | Nordic -> nordic
  | Romanian -> romanian
  | SouthEuropean -> south_european
  | Tajik -> tajik
  | Thai -> thai
  | Turkish -> turkish
  | Unicode -> unicode
  | Vietnamese -> vietnamese
  | WesternEuropean -> western_european

let codeset_list_from_lang lang =
  let li = ref [] in
  li := unicode :: !li;
  let _ =
    match lang with
        AR -> li := arabic :: !li
      | HY -> li := armenian :: !li
      | LT 
      | LV 
      | MI -> li := baltic :: !li
      | CY -> li := celtic :: western_european :: !li
      | BS  
      | CS 
      | HR 
      | HU 
      | PL  
      | SK  
      | SL -> li := central_european :: !li
      | SH  
      | SR -> li := central_european :: cyrillic ::!li
      | ZH -> li := chinese_traditional :: chinese_simplified :: !li
      | BE 
      | BG  
      | MK  
      | RU  
      | UK -> li := cyrillic :: !li
      | KA -> li := georgian :: !li
      | EL -> li := greek :: !li
      | YI
      | HE 
      | IW -> li := hebrew :: !li
      | JA -> li := japanese :: !li
      | KO-> li := korean :: !li
      | RO -> li := romanian :: central_european :: !li
      | MT -> li := south_european :: !li
      | TG -> li := tajik :: !li
      | TH -> li := thai :: !li
      | TR -> li := turkish :: !li
      | VI -> li := vietnamese :: !li
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
      | WA -> li := western_european :: !li
      | _ -> ()
  in
  List.flatten !li


(*
RFC 3066 - Tags for the Identification of Languages
http://www.faqs.org/rfcs/rfc3066.html
A locale is a string with a form as "<LANG>-<COUNTRY>-..." where 
<LANG> is a 2-letter ISO 639 language code, 
<COUNTRY> is a 2-letter ISO 3166 country code.
*)

let set_default_codeset_list lang =
  enc_list := List.map (fun c -> codeset_to_string c ) (codeset_list_from_lang lang);
  enc_list := List.filter (fun enc -> enc <> charset) !enc_list;
  enc_list := if (charset <> "UTF-8") then charset::!enc_list else !enc_list;
  nenc := List.length !enc_list

let last_chance_to_encode s =
  let us = ref "" in
  let slen = String.length s in
  for i = 0 to (slen - 1) do
    try
      us := !us ^ (Glib.Convert.locale_to_utf8 (String.sub s i 1))
    with _ ->
      us := !us ^ (Glib.Utf8.from_unichar 2)
  done;
  !us


let encode_to_utf8 s =
  let rec iter i n =
     if i = n 
       then last_chance_to_encode s
       else
         try
           let from_codeset = List.nth !enc_list i in
           Glib.Convert.convert s ~to_codeset:"UTF-8" ~from_codeset
         with _ -> iter (i + 1) !nenc
  in iter 0 !nenc

let private_encode_to_utf8 s enc_list =
  let nenc = List.length enc_list in
  let rec iter i n =
     if i = n 
       then last_chance_to_encode s
       else
         try
           let from_codeset = codeset_to_string (List.nth enc_list i) in
           Glib.Convert.convert s ~to_codeset:"UTF-8" ~from_codeset
         with _ -> iter (i + 1) nenc
  in iter 0 nenc

let is_utf8 s = (Glib.Utf8.validate s)

let utf8_of s =
  if is_utf8 s then s
    else encode_to_utf8 s

let private_utf8_of s enc_list =
  if is_utf8 s then s
    else private_encode_to_utf8 s enc_list

let simple_encode_to_utf8 s =
  try
    Glib.Convert.locale_to_utf8 s
  with _ ->
    (
     let slen = String.length s in
     String.make slen (Glib.Utf8.from_unichar 2).[0]
    )

let simple_utf8_of s =
  if is_utf8 s then s
    else simple_encode_to_utf8 s

let all_regions =
  [
   Default;
   Arabic;
   Armenian;
   Baltic;
   Celtic;
   CentralEuropean;
   ChineseSimplified;
   ChineseTraditional;
   Cyrillic;
   Georgian;
   Greek;
   Hebrew;
   Japanese;
   Korean;
   Nordic;
   Romanian;
   SouthEuropean;
   Tajik;
   Thai;
   Turkish;
   Vietnamese;
   WesternEuropean;
  ]

let default_codeset_list () = !enc_list

(* names are utf8 encoded! *)

open Options

let assoc_lang_languages =
  [
EN, "English";
(*
 * TODO : Create files mlnet_strings.<language>
 *

SQ, "Shqip";
AR, "Arabic";
HY, "Armenian";
EU, "Euskara";
BE, "Беларускаямова";
BG, "Български";
BS, "Bosanski";
CA, "Català";
ZH, "中文";
HR, "Hrvatski";
CS, "čeština";
DA, "dansk";
NL, "Nederlands";
ET, "Eesti";
FI, "Suomi";
FR, "Français";
GL, "Galego";
DE, "Deutsch";
EL, "Ελληνικά";
HE, "עברית";
HU, "Magyar";
IS, "Icelandic";
ID, "BahasaIndonesia";
GA, "Gaeilge";
IT, "Italiano";
JA, "日本語";
KO, "한국어";
LV, "Latviešu";
LT, "Lietuvių";
MS, "BahasaMelayu";
NO, "Norsk";
PL, "Polski";
PT, "Português";
RO, "Română";
RU, "русский";
SR, "српски";
SK, "Slovenský";
SL, "Slovenščina";
ES, "Español";
SV, "Svenska";
TH, "ไทย";
TR, "Türkçe";
UK, "Українська";
VI, "ViệtNam";
WA, "Walon";
CY, "Cymraeg";
YI, "ייִדיש";
*)
  ]

let languages = List.map (fun (_, s) -> s) assoc_lang_languages
let languages_rev = List.map (fun (lang,s) -> (s, lang)) assoc_lang_languages

let string_to_language s =
  try
    List.assoc s languages_rev
  with _ -> EN

let language_to_string lang =
  try
    List.assoc lang assoc_lang_languages
  with _ -> "English"

let value_to_lang v =
  match v with
      StringValue s -> string_to_language s
    | _ -> EN

let lang_to_value lang =
    StringValue (language_to_string lang)

let (language_option : lang option_class) =
    define_option_class "Language"
    value_to_lang lang_to_value
