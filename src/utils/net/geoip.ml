(* Copyright 2006 z *)
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
(* This product includes GeoIP data created by MaxMind, available from http://maxmind.com/ *)

open Int64ops
open Printf2
open Gettext
open Bigarray

let _s x = _s "GeoIp" x
let _b x = _b "GeoIp" x

let verbose = ref false

let log_prefix = "[Geo]"

let lprintf_nl fmt =
  lprintf_nl2 log_prefix fmt

let country_begin = 16776960
let state_begin_rev0 = 16700000
let state_begin_rev1 = 16000000
let structure_info_max_size = 20

let segment_record_length = 3
let standard_record_length = 3
let org_record_length = 4
let max_record_length = 4

type database_type =
    DatabaseInfo_UNKNOWN
  | DatabaseInfo_COUNTRY_EDITION
  | DatabaseInfo_REGION_EDITION_REV0
  | DatabaseInfo_REGION_EDITION_REV1
  | DatabaseInfo_CITY_EDITION_REV0
  | DatabaseInfo_CITY_EDITION_REV1
  | DatabaseInfo_ORG_EDITION
  | DatabaseInfo_ISP_EDITION
  | DatabaseInfo_PROXY_EDITION
  | DatabaseInfo_ASNUM_EDITION
  | DatabaseInfo_NETSPEED_EDITION
  | DatabaseInfo_DOMAIN_EDITION

let database_type_of_int =
  [| DatabaseInfo_UNKNOWN;
     DatabaseInfo_COUNTRY_EDITION;
     DatabaseInfo_CITY_EDITION_REV1;
     DatabaseInfo_REGION_EDITION_REV1;
     DatabaseInfo_ISP_EDITION;
     DatabaseInfo_ORG_EDITION;
     DatabaseInfo_CITY_EDITION_REV0;
     DatabaseInfo_REGION_EDITION_REV0;
     DatabaseInfo_PROXY_EDITION;
     DatabaseInfo_ASNUM_EDITION;
     DatabaseInfo_NETSPEED_EDITION;
     DatabaseInfo_DOMAIN_EDITION |]

let database_name dbtype =
  match dbtype with
  | DatabaseInfo_COUNTRY_EDITION -> "country edition"
  | DatabaseInfo_REGION_EDITION_REV0 -> "region edition v0"
  | DatabaseInfo_REGION_EDITION_REV1 -> "region edition v1"
  | DatabaseInfo_CITY_EDITION_REV0 -> "city edition v0"
  | DatabaseInfo_CITY_EDITION_REV1 -> "city edition v1"
  | DatabaseInfo_ORG_EDITION -> "org edition"
  | DatabaseInfo_ISP_EDITION -> "isp edition"
  | DatabaseInfo_PROXY_EDITION -> "proxy edition"
  | DatabaseInfo_ASNUM_EDITION -> "asnum edition"
  | DatabaseInfo_NETSPEED_EDITION -> "netspeed edition"
  | DatabaseInfo_DOMAIN_EDITION -> "domain edition"
  | DatabaseInfo_UNKNOWN -> "unknown edition"

let country_code_array = [|
  "--";"AP";"EU";"AD";"AE";"AF";"AG";"AI";"AL";"AM";"AN";"AO";"AQ";"AR";
  "AS";"AT";"AU";"AW";"AZ";"BA";"BB";"BD";"BE";"BF";"BG";"BH";"BI";"BJ";
  "BM";"BN";"BO";"BR";"BS";"BT";"BV";"BW";"BY";"BZ";"CA";"CC";"CD";"CF";
  "CG";"CH";"CI";"CK";"CL";"CM";"CN";"CO";"CR";"CU";"CV";"CX";"CY";"CZ";
  "DE";"DJ";"DK";"DM";"DO";"DZ";"EC";"EE";"EG";"EH";"ER";"ES";"ET";"FI";
  "FJ";"FK";"FM";"FO";"FR";"FX";"GA";"GB";"GD";"GE";"GF";"GH";"GI";"GL";
  "GM";"GN";"GP";"GQ";"GR";"GS";"GT";"GU";"GW";"GY";"HK";"HM";"HN";"HR";
  "HT";"HU";"ID";"IE";"IL";"IN";"IO";"IQ";"IR";"IS";"IT";"JM";"JO";"JP";
  "KE";"KG";"KH";"KI";"KM";"KN";"KP";"KR";"KW";"KY";"KZ";"LA";"LB";"LC";
  "LI";"LK";"LR";"LS";"LT";"LU";"LV";"LY";"MA";"MC";"MD";"MG";"MH";"MK";
  "ML";"MM";"MN";"MO";"MP";"MQ";"MR";"MS";"MT";"MU";"MV";"MW";"MX";"MY";
  "MZ";"NA";"NC";"NE";"NF";"NG";"NI";"NL";"NO";"NP";"NR";"NU";"NZ";"OM";
  "PA";"PE";"PF";"PG";"PH";"PK";"PL";"PM";"PN";"PR";"PS";"PT";"PW";"PY";
  "QA";"RE";"RO";"RU";"RW";"SA";"SB";"SC";"SD";"SE";"SG";"SH";"SI";"SJ";
  "SK";"SL";"SM";"SN";"SO";"SR";"ST";"SV";"SY";"SZ";"TC";"TD";"TF";"TG";
  "TH";"TJ";"TK";"TM";"TN";"TO";"TL";"TR";"TT";"TV";"TW";"TZ";"UA";"UG";
  "UM";"US";"UY";"UZ";"VA";"VC";"VE";"VG";"VI";"VN";"VU";"WF";"WS";"YE";
  "YT";"RS";"ZA";"ZM";"ME";"ZW";"A1";"A2";"O1";"AX";"GG";"IM";"JE";"BL";
  "MF"
|]

let country_name_array = [|
  "N/A";"Asia/Pacific Region";"Europe";"Andorra";"United Arab Emirates";
  "Afghanistan";"Antigua and Barbuda";"Anguilla";"Albania";"Armenia";
  "Netherlands Antilles";"Angola";"Antarctica";"Argentina";"American Samoa";
  "Austria";"Australia";"Aruba";"Azerbaijan";"Bosnia and Herzegovina";
  "Barbados";"Bangladesh";"Belgium";"Burkina Faso";"Bulgaria";"Bahrain";
  "Burundi";"Benin";"Bermuda";"Brunei Darussalam";"Bolivia";"Brazil";"Bahamas";
  "Bhutan";"Bouvet Island";"Botswana";"Belarus";"Belize";"Canada";
  "Cocos (Keeling) Islands";"Congo; The Democratic Republic of the";
  "Central African Republic";"Congo";"Switzerland";"Cote D'Ivoire";
  "Cook Islands";"Chile";"Cameroon";"China";"Colombia";"Costa Rica";"Cuba";
  "Cape Verde";"Christmas Island";"Cyprus";"Czech Republic";"Germany";
  "Djibouti";"Denmark";"Dominica";"Dominican Republic";"Algeria";"Ecuador";
  "Estonia";"Egypt";"Western Sahara";"Eritrea";"Spain";"Ethiopia";"Finland";
  "Fiji";"Falkland Islands (Malvinas)";"Micronesia; Federated States of";
  "Faroe Islands";"France";"France; Metropolitan";"Gabon";"United Kingdom";
  "Grenada";"Georgia";"French Guiana";"Ghana";"Gibraltar";"Greenland";"Gambia";
  "Guinea";"Guadeloupe";"Equatorial Guinea";"Greece";
  "South Georgia and the South Sandwich Islands";"Guatemala";"Guam";
  "Guinea-Bissau";"Guyana";"Hong Kong";"Heard Island and McDonald Islands";
  "Honduras";"Croatia";"Haiti";"Hungary";"Indonesia";"Ireland";"Israel";"India";
  "British Indian Ocean Territory";"Iraq";"Iran; Islamic Republic of";
  "Iceland";"Italy";"Jamaica";"Jordan";"Japan";"Kenya";"Kyrgyzstan";"Cambodia";
  "Kiribati";"Comoros";"Saint Kitts and Nevis";
  "Korea; Democratic People's Republic of";"Korea; Republic of";"Kuwait";
  "Cayman Islands";"Kazakstan";"Lao People's Democratic Republic";"Lebanon";
  "Saint Lucia";"Liechtenstein";"Sri Lanka";"Liberia";"Lesotho";"Lithuania";
  "Luxembourg";"Latvia";"Libyan Arab Jamahiriya";"Morocco";"Monaco";
  "Moldova; Republic of";"Madagascar";"Marshall Islands";
  "Macedonia";"Mali";"Myanmar";"Mongolia";
  "Macau";"Northern Mariana Islands";"Martinique";"Mauritania";"Montserrat";
  "Malta";"Mauritius";"Maldives";"Malawi";"Mexico";"Malaysia";"Mozambique";
  "Namibia";"New Caledonia";"Niger";"Norfolk Island";"Nigeria";"Nicaragua";
  "Netherlands";"Norway";"Nepal";"Nauru";"Niue";"New Zealand";"Oman";"Panama";
  "Peru";"French Polynesia";"Papua New Guinea";"Philippines";"Pakistan";
  "Poland";"Saint Pierre and Miquelon";"Pitcairn Islands";"Puerto Rico";
  "Palestinian Territory; Occupied";"Portugal";"Palau";"Paraguay";"Qatar";
  "Reunion";"Romania";"Russian Federation";"Rwanda";"Saudi Arabia";
  "Solomon Islands";"Seychelles";"Sudan";"Sweden";"Singapore";"Saint Helena";
  "Slovenia";"Svalbard and Jan Mayen";"Slovakia";"Sierra Leone";"San Marino";
  "Senegal";"Somalia";"Suriname";"Sao Tome and Principe";"El Salvador";
  "Syrian Arab Republic";"Swaziland";"Turks and Caicos Islands";"Chad";
  "French Southern Territories";"Togo";"Thailand";"Tajikistan";"Tokelau";
  "Turkmenistan";"Tunisia";"Tonga";"Timor-Leste";"Turkey";"Trinidad and Tobago";
  "Tuvalu";"Taiwan";"Tanzania; United Republic of";"Ukraine";"Uganda";
  "United States Minor Outlying Islands";"United States";"Uruguay";"Uzbekistan";
  "Holy See (Vatican City State)";"Saint Vincent and the Grenadines";
  "Venezuela";"Virgin Islands; British";"Virgin Islands; U.S.";"Vietnam";
  "Vanuatu";"Wallis and Futuna";"Samoa";"Yemen";"Mayotte";"Serbia";
  "South Africa";"Zambia";"Montenegro";"Zimbabwe";"Anonymous Proxy";
  "Satellite Provider";"Other";"Aland Islands";"Guernsey";"Isle of Man";"Jersey";
  "Saint Barthelemy";"Saint Martin"
|]

let country_continent_code_array = [| "--";
  "AS";"EU";"EU";"AS";"AS";"SA";"SA";"EU";"AS";"SA";
  "AF";"AN";"SA";"OC";"EU";"OC";"SA";"AS";"EU";"SA";
  "AS";"EU";"AF";"EU";"AS";"AF";"AF";"SA";"AS";"SA";
  "SA";"SA";"AS";"AF";"AF";"EU";"SA";"NA";"AS";"AF";
  "AF";"AF";"EU";"AF";"OC";"SA";"AF";"AS";"SA";"SA";
  "SA";"AF";"AS";"AS";"EU";"EU";"AF";"EU";"SA";"SA";
  "AF";"SA";"EU";"AF";"AF";"AF";"EU";"AF";"EU";"OC";
  "SA";"OC";"EU";"EU";"EU";"AF";"EU";"SA";"AS";"SA";
  "AF";"EU";"SA";"AF";"AF";"SA";"AF";"EU";"SA";"SA";
  "OC";"AF";"SA";"AS";"AF";"SA";"EU";"SA";"EU";"AS";
  "EU";"AS";"AS";"AS";"AS";"AS";"EU";"EU";"SA";"AS";
  "AS";"AF";"AS";"AS";"OC";"AF";"SA";"AS";"AS";"AS";
  "SA";"AS";"AS";"AS";"SA";"EU";"AS";"AF";"AF";"EU";
  "EU";"EU";"AF";"AF";"EU";"EU";"AF";"OC";"EU";"AF";
  "AS";"AS";"AS";"OC";"SA";"AF";"SA";"EU";"AF";"AS";
  "AF";"NA";"AS";"AF";"AF";"OC";"AF";"OC";"AF";"SA";
  "EU";"EU";"AS";"OC";"OC";"OC";"AS";"SA";"SA";"OC";
  "OC";"AS";"AS";"EU";"SA";"OC";"SA";"AS";"EU";"OC";
  "SA";"AS";"AF";"EU";"EU";"AF";"AS";"OC";"AF";"AF";
  "EU";"AS";"AF";"EU";"EU";"EU";"AF";"EU";"AF";"AF";
  "SA";"AF";"SA";"AS";"AF";"SA";"AF";"AF";"AF";"AS";
  "AS";"OC";"AS";"AF";"OC";"AS";"AS";"SA";"OC";"AS";
  "AF";"EU";"AF";"OC";"NA";"SA";"AS";"EU";"SA";"SA";
  "SA";"SA";"AS";"OC";"OC";"OC";"AS";"AF";"EU";"AF";
  "AF";"EU";"AF";"--";"--";"--";"EU";"EU";"EU";"EU";
  "SA";"SA"
|]

let country_continent_name_array =
  Array.make (Array.length country_continent_code_array) "N/A"

let country_index = Hashtbl.create 250
let () =
  Array.iteri (fun i cc -> 
    Hashtbl.add country_index cc i
  ) country_code_array;
  Array.iteri (fun i ccc ->
    country_continent_name_array.(i) <- (
      match ccc with
      | "AF" -> "Africa"
      | "AN" -> "Antarctica"
      | "AS" -> "Asia"
      | "EU" -> "Europe"
      | "NA" -> "North America"
      | "OC" -> "Oceania"
      | "SA" -> "South America"
      | _    -> "N/A"
    )) country_continent_code_array

let unknown_country = ("--", "N/A")

type geoip_database = {
  file: in_channel;
  dbtype: database_type;
  segments: int;
  record_length: int;
  map: (int, int8_unsigned_elt, c_layout) Array1.t;
}

let unpack filename =
  let ext = String.lowercase (Filename2.extension filename) in
  let last_ext = String.lowercase (Filename2.last_extension filename) in
  let real_ext = if last_ext = ".zip" then last_ext else ext in
  match real_ext with
  | ".zip" ->
      (try
         let file =
           Unix2.tryopen_read_zip filename (fun ic ->
             try
               Zip.find_entry ic "GeoIP.dat"
             with e ->
               lprintf_nl "Exception %s while extracting geoip.dat"
                 (Printexc2.to_string e);
               raise e) in
         try
           ignore(Misc.archive_extract filename "zip");
           let geo_file = Filename.concat "web_infos" "GeoIP.dat" in
           (try Sys.remove geo_file with _ -> ());
           Unix2.rename file.Zip.filename geo_file;
           geo_file
         with e ->
           lprintf_nl "Exception %s while extracting geoip.dat"
             (Printexc2.to_string e);
           raise e
       with e ->
         lprintf_nl "Exception %s while opening %s"
           (Printexc2.to_string e) filename;
         raise Not_found)

  | ".dat.gz" | ".dat.bz2" | ".gz" | ".bz2" ->
      let filetype =
        if ext = ".bz2" || ext = ".dat.bz2" then "bz2" else "gz" in
      (try
         let geo_file = Filename.concat "web_infos" "GeoIP.dat" in
         let s = Misc.archive_extract filename filetype in
         (try Sys.remove geo_file with _ -> ());
         Unix2.rename s geo_file;
         geo_file
       with e ->
         lprintf_nl "Exception %s while extracting"
           (Printexc2.to_string e);
         raise Not_found)
(* if file is not a supported archive type try loading that file anyway *)
    | _ -> filename

let close_geoip_db geoip_db =
  close_in geoip_db.file

let open_geoip_db filename =
  try
    let f = open_in filename in
    let size = in_channel_length f in
    let map = Misc2.map_file f in

    let new_database ?offset dbtype =
      let read_segment_size () =
        match offset with
        | None -> failwith "Can't read variable segment size without a signature"
        | Some offset ->
            let result = ref 0 in
            for j = 0 to segment_record_length - 1 do
              let k = map.{offset + j} lsl (j * 8) in
              result := !result + k
            done;
            !result in

      match dbtype with
      | DatabaseInfo_UNKNOWN -> assert false
      | DatabaseInfo_DOMAIN_EDITION ->
          None; (* Missing in previous implementation! *)
      | _ ->
          Some {
            file = f;
            dbtype = dbtype;
            segments = 
              (match dbtype with
               | DatabaseInfo_COUNTRY_EDITION 
               | DatabaseInfo_PROXY_EDITION
               | DatabaseInfo_NETSPEED_EDITION -> 
                   country_begin
               | DatabaseInfo_REGION_EDITION_REV0 -> 
                   state_begin_rev0
               | DatabaseInfo_REGION_EDITION_REV1 -> 
                   state_begin_rev1
               | _ -> 
                   read_segment_size ());
            record_length =
              (match dbtype with
               | DatabaseInfo_ORG_EDITION 
               | DatabaseInfo_ISP_EDITION 
               | DatabaseInfo_ASNUM_EDITION -> 
                   org_record_length
               | _ -> 
                   standard_record_length);
            map = map;
          } in

    let rec setup_types i =
      if i >= structure_info_max_size then
        new_database DatabaseInfo_COUNTRY_EDITION
      else
        let offset = size - 3 - i in
        if map.{offset} <> 255 ||
          map.{offset + 1} <> 255 ||
          map.{offset + 2} <> 255 then setup_types (i + 1)
        else
          let type_byte = map.{offset + 3} in
          let type_byte = if type_byte >= 106 then
            type_byte - 105 else type_byte in
          if type_byte < 1 || type_byte >= Array.length database_type_of_int 
          then setup_types (i + 1)
          else 
            let dbtype = database_type_of_int.(type_byte) in
            new_database ~offset:(offset + 4) dbtype
    in
    setup_types 0
  with e -> 
    lprintf_nl "Exception %s while opening"
      (Printexc2.to_string e);
    None

let seek_country db ip =
  let ip_long = Ip.to_int64 ip in

  let rec dive depth offset = 
    if depth < 0 then 0 else
      let update i =
        match db.record_length with
        | 3 -> (* specialized code for common case *)
          let offset = 6 * offset + 3 * i in
          db.map.{offset} +
          (db.map.{offset + 1} lsl 8) +
          (db.map.{offset + 2} lsl 16)
        | _ ->
          let offset = (2 * offset + i) * db.record_length in
          let tmp = ref 0 in
          for j = 0 to db.record_length - 1 do
            let y = db.map.{offset + j} in
            tmp := !tmp + (y lsl (j * 8));
          done;
          !tmp in
          
      let swim i = 
        if i >= db.segments then i - country_begin
        else dive (depth - 1) i in

      let bit = if (and64 ip_long (left64 1L depth)) = 0L then 0 else 1 in
      swim (update bit) in
        
  dive 31 0

let current_db = ref (None: geoip_database option)

let active () =
  !current_db <> None

let close () =
  match !current_db with
  | None -> ()
  | Some db ->
      close_geoip_db db;
      current_db := None

let init filename = 
  close ();
  current_db := open_geoip_db filename;
  (match !current_db with
   | None -> lprintf_nl (_b "database not loaded")
   | Some db -> lprintf_nl (_b "%s database loaded") (database_name db.dbtype))

let get_country_code ip =
  if !verbose then lprintf_nl "get_country_code %s" (Ip.to_string ip);
  if ip = Ip.null then 0
  else
    match !current_db with
    | None -> 0
    | Some db ->
        try seek_country db ip
        with _ -> 0

let get_country_code_option ip =
  if !verbose then lprintf_nl "get_country_code_option %s" (Ip.to_string ip);
  if ip = Ip.null then None
  else 
    match !current_db with
    | None -> None
    | Some db ->
        try
          let cc = seek_country db ip in
          if cc = 0 then None else Some cc
        with _ -> None

let get_country ip = 
  if !verbose then lprintf_nl "get_country %s" (Ip.to_string ip);
  if ip = Ip.null then unknown_country
  else
    match !current_db with
    | None -> unknown_country
    | Some db ->
        try 
          let ret = seek_country db ip in
          if ret = 0 then unknown_country 
          else country_code_array.(ret), country_name_array.(ret);
        with _ -> unknown_country

let get_country_code_name cc =
  match cc with
  | Some cc ->
      country_code_array.(cc),
      country_name_array.(cc)
  | None -> unknown_country

let _ =
  Heap.add_memstat "GeoIp" (fun level buf ->
    match !current_db with
    | Some db ->
        Printf.bprintf buf "  countries: %d\n" (Array.length country_code_array);
        Printf.bprintf buf "  database_type: %s\n" (database_name db.dbtype);
        Printf.bprintf buf "  map size: %d\n" (Array1.dim db.map);
    | None -> Printf.bprintf buf "  module not active\n"
  )
