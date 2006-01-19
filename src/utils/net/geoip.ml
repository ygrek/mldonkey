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

open Unix
open Ip
open Int64ops
open Printf2

let country_begin = 16776960
let state_begin_rev0 = 16700000
let state_begin_rev1 = 16000000
let structure_info_max_size = 20

let segment_record_length = 3
let standard_record_length = 3
let org_record_length = 4
let max_record_length = 4

let databaseInfo_COUNTRY_EDITION = 1
let databaseInfo_REGION_EDITION_REV0 = 7
let databaseInfo_REGION_EDITION_REV1 = 3
let databaseInfo_CITY_EDITION_REV0 = 6
let databaseInfo_CITY_EDITION_REV1 = 2
let databaseInfo_ORG_EDITION = 5
let databaseInfo_ISP_EDITION = 4

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
  "TH";"TJ";"TK";"TM";"TN";"TO";"TP";"TR";"TT";"TV";"TW";"TZ";"UA";"UG";
  "UM";"US";"UY";"UZ";"VA";"VC";"VE";"VG";"VI";"VN";"VU";"WF";"WS";"YE";
  "YT";"YU";"ZA";"ZM";"ZR";"ZW";"A1";"A2";"O1";
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
  "Turkmenistan";"Tunisia";"Tonga";"East Timor";"Turkey";"Trinidad and Tobago";
  "Tuvalu";"Taiwan";"Tanzania; United Republic of";"Ukraine";"Uganda";
  "United States Minor Outlying Islands";"United States";"Uruguay";"Uzbekistan";
  "Holy See (Vatican City State)";"Saint Vincent and the Grenadines";
  "Venezuela";"Virgin Islands; British";"Virgin Islands; U.S.";"Vietnam";
  "Vanuatu";"Wallis and Futuna";"Samoa";"Yemen";"Mayotte";"Yugoslavia";
  "South Africa";"Zambia";"Zaire";"Zimbabwe";"Anonymous Proxy";
  "Satellite Provider";"Other";
|]

let unknown_country = ref ("--", "N/A")
let file = ref (Obj.magic 0)
let active = ref false
let database_type = ref databaseInfo_COUNTRY_EDITION 
let database_segments = ref 0
let record_length = ref 0

let unpack filename =
  let ext = String.lowercase (Filename2.extension filename) in
    let last_ext = String.lowercase (Filename2.last_extension filename) in
    let real_ext = if last_ext = ".zip" then
      last_ext
    else
      ext
    in
    match real_ext with
      ".zip" ->
	begin
	  try
	    let ic = Zip.open_in filename in
	      try
		let file = Zip.find_entry ic "GeoIP.dat" in
		  Zip.close_in ic;
		ignore(Misc.archive_extract filename "zip");
		(try Sys.remove "GeoIP.dat" with _ -> ());
		Unix2.rename file.Zip.filename "GeoIP.dat";
		"GeoIP.dat"
	      with e ->
		Zip.close_in ic;
		lprintf_nl "Exception %s while extracting geoip.dat"
		  (Printexc2.to_string e);
		raise Not_found
	  with e ->
	    lprintf_nl "Exception %s while opening %s"
	      (Printexc2.to_string e) filename;
	    raise Not_found
	end
    | ".dat.gz" | ".dat.bz2" | ".gz" | ".bz2" ->
	begin
	  let filetype =
	    if ext = ".bz2" || ext = ".dat.bz2" then
	      "bz2"
	    else
	      "gz"
	  in try
	    let s = Misc.archive_extract filename filetype in
	    (try Sys.remove "GeoIP.dat" with _ -> ());
	    Unix2.rename s "GeoIP.dat";
	    "GeoIP.dat"
          with e ->
            lprintf_nl "Exception %s while extracting"
	      (Printexc2.to_string e);
	    raise Not_found
        end
(* if file is not a supported archive type try loading that file anyway *)
    | _ -> filename

let close () =
  try 
    if !active then close_in !file;
    active := false
  with _ -> ()

let init filename =
  try
    close ();
    file := open_in filename;

    let size = in_channel_length !file in
    ignore( seek_in !file (size-3) );
 
    let rec setup_types i =
      if i < structure_info_max_size then begin
 
        let delim_len = 3 in
        let delim = String.create delim_len in
        ignore( input !file delim 0 delim_len );
  
        if delim = "\255\255\255" then begin
  
          database_type := input_byte !file;
  
          if !database_type >= 106 then
            database_type := !database_type - 105;
        
          if !database_type = databaseInfo_REGION_EDITION_REV0 then begin
            database_segments := state_begin_rev0;
            record_length := standard_record_length;
          end
          else if !database_type = databaseInfo_REGION_EDITION_REV1 then begin
            database_segments := state_begin_rev1;
            record_length := standard_record_length;
          end
          else if !database_type = databaseInfo_CITY_EDITION_REV0 
                 || !database_type = databaseInfo_CITY_EDITION_REV1
                 || !database_type = databaseInfo_ORG_EDITION 
                 || !database_type = databaseInfo_ISP_EDITION
                  then begin
             
            database_segments := 0;
  
            if !database_type = databaseInfo_CITY_EDITION_REV0 
              || !database_type = databaseInfo_CITY_EDITION_REV1 then begin
                record_length := standard_record_length;
            end else begin
                record_length := org_record_length;
            end;
        
            let buf = String.create segment_record_length in
            ignore( input !file buf 0 segment_record_length );
        
            for j = 0 to segment_record_length - 1 do
              let k = (int_of_char buf.[j] land 0xff) lsl (j * 8) in
              database_segments := !database_segments + k;
            done;
          end;
  
        end
        else begin
          let pos = pos_in !file in
          ignore(seek_in !file (pos - 4));
          setup_types (i+1);
        end;
      end
    in

    setup_types 0;

    if !database_type = databaseInfo_COUNTRY_EDITION then begin
      database_segments := country_begin;
      record_length := standard_record_length;
    end;

    active := true; 
    lprintf_nl "[GeoIP] database loaded"
  with _ -> 
    active := false

let seek_country ip =
    let ip_long = Ip.to_int64 ip in
    let buf = String.create (2 * max_record_length) in

    let rec dive depth offset = 

      if depth < 0 then 0 else begin

        ignore( seek_in !file (2 * !record_length * offset) );
        ignore( input !file buf 0 (2 * max_record_length) );
        
        let update i =
          let tmp = ref 0 in
          for j = 0 to !record_length - 1 do
            let y = ref (int_of_char buf.[i * !record_length + j]) in
            if !y < 0 then y := !y + 256;
            tmp := !tmp + (!y lsl (j * 8));
          done;
          !tmp
        in
          
        let swim i = 
          if i >= !database_segments then i else dive (depth-1) i
        in

        if (and64 ip_long (left64 1L depth)) > 0L 
          then swim (update 1)
          else swim (update 0)
      
      end

    in
    dive 31 0

let get_country_code ip =
  if not !active then 0
  else begin
    try   
      (seek_country ip) - country_begin 
    with _ -> 0
  end

let get_country ip = 
  if not !active then !unknown_country
  else begin
    try 
      let ret = (seek_country ip) - country_begin in
      if ret = 0 then !unknown_country 
        else country_code_array.(ret), country_name_array.(ret);
    with _ -> 
      !unknown_country
  end

