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

open AnyEndian
open LittleEndian
open Printf2
open Options
open BasicSocket (* last_time *)
open GuiTypes
open CommonOptions 
open CommonTypes
open CommonGlobals
open CommonNetwork
open CommonMessages
open CommonInteractive
  
open DonkeyOptions
open DonkeyTypes
open DonkeyGlobals
open DonkeyComplexOptions


let brand_to_int b =
  match b with
    Brand_unknown -> 0
  | Brand_edonkey -> 1
  | Brand_mldonkey1 -> 2
  | Brand_mldonkey2 -> 3
  | Brand_overnet -> 4
  | Brand_newemule -> 5
  | Brand_server -> 6
  | Brand_mldonkey3 -> 7
  | Brand_cdonkey -> 8
  | Brand_lmule -> 9
  | Brand_shareaza -> 10
  | Brand_amule -> 11
  | Brand_lphant -> 12
      
let brand_of_int b =
  match b with
    0 -> Brand_unknown
  | 1 -> Brand_edonkey
  | 2 -> Brand_mldonkey1
  | 3 -> Brand_mldonkey2
  | 4 -> Brand_overnet
  | 5 -> Brand_newemule
  | 6 -> Brand_server
  | 7 -> Brand_mldonkey3
  | 8 -> Brand_cdonkey
  | 9 -> Brand_lmule
  | 10 -> Brand_shareaza
  | 11 -> Brand_amule
  | 12 -> Brand_lphant
  | _ -> raise Not_found
      
let gbrand_to_string b =
  match b with
    Brand_unknown -> "unk"
  | Brand_edonkey -> "eDK"
  | Brand_cdonkey -> "cDK"
  | Brand_mldonkey1 -> "oML"
  | Brand_mldonkey2 -> "nML"
  | Brand_mldonkey3 -> "tML"
  | Brand_overnet -> "OVR"
  | Brand_newemule -> "eMU"
  | Brand_lmule -> "xMU"
  | Brand_shareaza -> "sZA"
  | Brand_server -> "SER"
  | Brand_amule -> "aMU"
  | Brand_lphant -> "lPH"

let brand_mod_to_int b =
  match b with
    Brand_mod_unknown -> 0
  | Brand_mod_extasy -> 1
  | Brand_mod_hunter -> 2
  | Brand_mod_sivka -> 3
  | Brand_mod_ice -> 4
  | Brand_mod_plus -> 5
  | Brand_mod_lsd -> 6
  | Brand_mod_maella -> 7
  | Brand_mod_pille -> 8
  | Brand_mod_morphkad -> 9
  | Brand_mod_efmod -> 10
  | Brand_mod_xtreme -> 11
  | Brand_mod_bionic -> 12
  | Brand_mod_pawcio -> 13
  | Brand_mod_zzul -> 14
  | Brand_mod_blackhand -> 15
  | Brand_mod_lovelace -> 16
  | Brand_mod_morphnext -> 17
  | Brand_mod_fincan -> 18
  | Brand_mod_ewombat -> 19
  | Brand_mod_morph -> 20
  | Brand_mod_mortillo -> 21
  | Brand_mod_lh -> 22
  | Brand_mod_emulespana -> 23
  | Brand_mod_blackrat -> 24
  | Brand_mod_enkeydev -> 25
  | Brand_mod_gnaddelwarz -> 26
  | Brand_mod_phoenixkad -> 27
  | Brand_mod_koizo -> 28
  | Brand_mod_ed2kfiles -> 29
  | Brand_mod_athlazan -> 30
  | Brand_mod_cryptum -> 31
  | Brand_mod_lamerzchoice -> 32
  | Brand_mod_notdead -> 33
  | Brand_mod_peace -> 34
  | Brand_mod_goldicryptum -> 35
  | Brand_mod_eastshare -> 36
  | Brand_mod_mfck -> 37
  | Brand_mod_echanblard -> 38
  | Brand_mod_sp4rk -> 39
  | Brand_mod_powermule -> 40
  | Brand_mod_bloodymad -> 41
  | Brand_mod_roman2k -> 42
  | Brand_mod_gammaoh -> 43
  | Brand_mod_elfenwombat -> 44
  | Brand_mod_o2 -> 45
  | Brand_mod_dm -> 46
  | Brand_mod_sfiom -> 47
  | Brand_mod_magic_elseve -> 48
  | Brand_mod_schlumpmule -> 49
  | Brand_mod_lc -> 50
  | Brand_mod_noamson -> 51
  | Brand_mod_stormit -> 52
  | Brand_mod_omax -> 53
  | Brand_mod_mison -> 54
  | Brand_mod_phoenix -> 55
  | Brand_mod_spiders -> 56
  | Brand_mod_iberica -> 57
  | Brand_mod_mortimer -> 58
  | Brand_mod_stonehenge -> 59
  | Brand_mod_xlillo -> 60
  | Brand_mod_imperator -> 61
  | Brand_mod_raziboom -> 62
  | Brand_mod_khaos -> 63
  | Brand_mod_hardmule -> 64
  | Brand_mod_sc -> 65
  | Brand_mod_cy4n1d -> 66
  | Brand_mod_dmx -> 67
  | Brand_mod_ketamine -> 68
  | Brand_mod_blackmule -> 69
  | Brand_mod_morphxt -> 70
  | Brand_mod_ngdonkey -> 71
  | Brand_mod_hawkstar -> 72
  | Brand_mod_neomule -> 73
  | Brand_mod_cyrex -> 74
  | Brand_mod_aldo -> 75
  | Brand_mod_emulede -> 76
  | Brand_mod_zx -> 77
  | Brand_mod_ibericaxt -> 78
  | Brand_mod_candymule -> 79
  | Brand_mod_ackronic -> 80
  | Brand_mod_rappis -> 81
  | Brand_mod_overdose -> 82
  | Brand_mod_hebmule -> 83
  | Brand_mod_senfei -> 84
  | Brand_mod_spoofmod -> 85
  | Brand_mod_fusspilz -> 86
  | Brand_mod_rocket -> 87
  | Brand_mod_warezfaw -> 88
  | Brand_mod_emusicmule -> 89
  | Brand_mod_aideadsl -> 90
  | Brand_mod_epo -> 91
  | Brand_mod_kalitsch -> 92
  | Brand_mod_raynz -> 93
  | Brand_mod_serverclient -> 94
  | Brand_mod_bl4ckbird -> 95
  | Brand_mod_bl4ckf0x -> 96
  | Brand_mod_rt -> 97

let brand_mod_of_int b =
  match b with
    0 -> Brand_mod_unknown
  | 1 -> Brand_mod_extasy
  | 2 -> Brand_mod_hunter
  | 3 -> Brand_mod_sivka
  | 4 -> Brand_mod_ice
  | 5 -> Brand_mod_plus
  | 6 -> Brand_mod_lsd
  | 7 -> Brand_mod_maella
  | 8 -> Brand_mod_pille
  | 9 -> Brand_mod_morphkad
  | 10 -> Brand_mod_efmod
  | 11 -> Brand_mod_xtreme
  | 12 -> Brand_mod_bionic
  | 13 -> Brand_mod_pawcio
  | 14 -> Brand_mod_zzul
  | 15 -> Brand_mod_blackhand
  | 16 -> Brand_mod_lovelace
  | 17 -> Brand_mod_morphnext
  | 18 -> Brand_mod_fincan
  | 19 -> Brand_mod_ewombat
  | 20 -> Brand_mod_morph
  | 21 -> Brand_mod_mortillo
  | 22 -> Brand_mod_lh
  | 23 -> Brand_mod_emulespana
  | 24 -> Brand_mod_blackrat
  | 25 -> Brand_mod_enkeydev
  | 26 -> Brand_mod_gnaddelwarz
  | 27 -> Brand_mod_phoenixkad
  | 28 -> Brand_mod_koizo
  | 29 -> Brand_mod_ed2kfiles
  | 30 -> Brand_mod_athlazan
  | 31 -> Brand_mod_cryptum
  | 32 -> Brand_mod_lamerzchoice
  | 33 -> Brand_mod_notdead
  | 34 -> Brand_mod_peace
  | 35 -> Brand_mod_goldicryptum
  | 36 -> Brand_mod_eastshare
  | 37 -> Brand_mod_mfck
  | 38 -> Brand_mod_echanblard
  | 39 -> Brand_mod_sp4rk
  | 40 -> Brand_mod_powermule
  | 41 -> Brand_mod_bloodymad
  | 42 -> Brand_mod_roman2k
  | 43 -> Brand_mod_gammaoh
  | 44 -> Brand_mod_elfenwombat
  | 45 -> Brand_mod_o2
  | 46 -> Brand_mod_dm
  | 47 -> Brand_mod_sfiom
  | 48 -> Brand_mod_magic_elseve
  | 49 -> Brand_mod_schlumpmule
  | 50 -> Brand_mod_lc
  | 51 -> Brand_mod_noamson
  | 52 -> Brand_mod_stormit
  | 53 -> Brand_mod_omax
  | 54 -> Brand_mod_mison
  | 55 -> Brand_mod_phoenix
  | 56 -> Brand_mod_spiders
  | 57 -> Brand_mod_iberica
  | 58 -> Brand_mod_mortimer
  | 59 -> Brand_mod_stonehenge
  | 60 -> Brand_mod_xlillo
  | 61 -> Brand_mod_imperator
  | 62 -> Brand_mod_raziboom
  | 63 -> Brand_mod_khaos
  | 64 -> Brand_mod_hardmule
  | 65 -> Brand_mod_sc
  | 66 -> Brand_mod_cy4n1d
  | 67 -> Brand_mod_dmx
  | 68 -> Brand_mod_ketamine
  | 69 -> Brand_mod_blackmule
  | 70 -> Brand_mod_morphxt
  | 71 -> Brand_mod_ngdonkey
  | 72 -> Brand_mod_hawkstar
  | 73 -> Brand_mod_neomule
  | 74 -> Brand_mod_cyrex
  | 75 -> Brand_mod_aldo
  | 76 -> Brand_mod_emulede
  | 77 -> Brand_mod_zx
  | 78 -> Brand_mod_ibericaxt
  | 79 -> Brand_mod_candymule
  | 80 -> Brand_mod_ackronic
  | 81 -> Brand_mod_rappis
  | 82 -> Brand_mod_overdose
  | 83 -> Brand_mod_hebmule
  | 84 -> Brand_mod_senfei
  | 85 -> Brand_mod_spoofmod
  | 86 -> Brand_mod_fusspilz
  | 87 -> Brand_mod_rocket
  | 88 -> Brand_mod_warezfaw
  | 89 -> Brand_mod_emusicmule
  | 90 -> Brand_mod_aideadsl
  | 91 -> Brand_mod_epo
  | 92 -> Brand_mod_kalitsch
  | 93 -> Brand_mod_raynz
  | 94 -> Brand_mod_serverclient
  | 95 -> Brand_mod_bl4ckbird
  | 96 -> Brand_mod_bl4ckf0x
  | 97 -> Brand_mod_rt
  | _ -> raise Not_found
      
let gbrand_mod_to_string b =
  match b with
    Brand_mod_unknown -> ""
  | Brand_mod_extasy -> "ext"
  | Brand_mod_hunter -> "hun"
  | Brand_mod_sivka -> "siv"
  | Brand_mod_ice -> "ice"
  | Brand_mod_plus -> "plu"
  | Brand_mod_lsd -> "lsd"
  | Brand_mod_maella -> "mae"
  | Brand_mod_pille -> "pil"
  | Brand_mod_morphkad -> "mo1"
  | Brand_mod_efmod -> "efm"
  | Brand_mod_xtreme -> "xtr"
  | Brand_mod_bionic -> "bio"
  | Brand_mod_pawcio -> "paw"
  | Brand_mod_zzul -> "zzu"
  | Brand_mod_blackhand -> "bla"
  | Brand_mod_lovelace -> "lov"
  | Brand_mod_morphnext -> "mo2"
  | Brand_mod_fincan -> "fin"
  | Brand_mod_ewombat -> "ewo"
  | Brand_mod_morph -> "mo3"
  | Brand_mod_mortillo -> "mot"
  | Brand_mod_lh -> "lh"
  | Brand_mod_emulespana -> "esp"
  | Brand_mod_blackrat -> "blr"
  | Brand_mod_enkeydev -> "ekd"
  | Brand_mod_gnaddelwarz -> "gna"
  | Brand_mod_phoenixkad -> "pkd"
  | Brand_mod_koizo -> "koi"
  | Brand_mod_ed2kfiles -> "edf"
  | Brand_mod_athlazan -> "ath"
  | Brand_mod_cryptum -> "cry"
  | Brand_mod_lamerzchoice -> "lam"
  | Brand_mod_notdead -> "nod"
  | Brand_mod_peace -> "pea"
  | Brand_mod_goldicryptum -> "gcr"
  | Brand_mod_eastshare -> "eas"
  | Brand_mod_mfck -> "mfc"
  | Brand_mod_echanblard -> "ech"
  | Brand_mod_sp4rk -> "sp4"
  | Brand_mod_powermule -> "pow"
  | Brand_mod_bloodymad -> "blo"
  | Brand_mod_roman2k -> "rom"
  | Brand_mod_gammaoh -> "gam"
  | Brand_mod_elfenwombat -> "elf"
  | Brand_mod_o2 -> "o2"
  | Brand_mod_dm -> "DM"
  | Brand_mod_sfiom -> "SFI"
  | Brand_mod_magic_elseve -> "MEl"
  | Brand_mod_schlumpmule -> "sch"
  | Brand_mod_lc -> "LC"
  | Brand_mod_noamson -> "NoS"
  | Brand_mod_stormit -> "Sto"
  | Brand_mod_omax -> "OMX"
  | Brand_mod_mison -> "Mis"
  | Brand_mod_phoenix -> "pPho"
  | Brand_mod_spiders -> "spi"
  | Brand_mod_iberica -> "Ib"
  | Brand_mod_mortimer -> "mor"
  | Brand_mod_stonehenge -> "sto"
  | Brand_mod_xlillo -> "Xli"
  | Brand_mod_imperator -> "Imp"
  | Brand_mod_raziboom -> "Raz"
  | Brand_mod_khaos -> "Kha"
  | Brand_mod_hardmule -> "Har"
  | Brand_mod_sc -> "SC"
  | Brand_mod_cy4n1d -> "Cy4"
  | Brand_mod_dmx -> "DMX"
  | Brand_mod_ketamine -> "Ket"
  | Brand_mod_blackmule -> "blm"
  | Brand_mod_morphxt -> "Mxt"
  | Brand_mod_ngdonkey -> "ngd"
  | Brand_mod_hawkstar -> "haw"
  | Brand_mod_neomule -> "neo"
  | Brand_mod_cyrex -> "cyr"
  | Brand_mod_aldo -> "ald"
  | Brand_mod_emulede -> "ede"
  | Brand_mod_zx -> "zx"
  | Brand_mod_ibericaxt -> "iBx"
  | Brand_mod_candymule -> "can"
  | Brand_mod_ackronic -> "ack"
  | Brand_mod_rappis -> "rap"
  | Brand_mod_overdose -> "ove"
  | Brand_mod_hebmule -> "heb"
  | Brand_mod_senfei -> "sen"
  | Brand_mod_spoofmod -> "spo"
  | Brand_mod_fusspilz -> "fus"
  | Brand_mod_rocket -> "roc"
  | Brand_mod_warezfaw -> "war"
  | Brand_mod_emusicmule -> "emm"
  | Brand_mod_aideadsl -> "aid"
  | Brand_mod_epo -> "epo"
  | Brand_mod_kalitsch -> "kal"
  | Brand_mod_raynz -> "ray"
  | Brand_mod_serverclient -> "sc"
  | Brand_mod_bl4ckbird -> "b4b"
  | Brand_mod_bl4ckf0x -> "b4f"
  | Brand_mod_rt -> "rt"

let stats_all = dummy_stats 
let mod_stats_all = dummy_mod_stats 
let stats_by_brand = Array.init brand_count (fun _ ->
  { dummy_stats with brand_seen = 0 }
  )
  
let stats_by_brand_mod = Array.init brand_mod_count (fun _ ->
  { dummy_mod_stats with brand_mod_seen = 0 }
  )

let count_seen c =
  stats_all.brand_seen <- stats_all.brand_seen + 1;
  if !!emule_mods_count && c.client_mod_brand != Brand_mod_unknown then mod_stats_all.brand_mod_seen <- mod_stats_all.brand_mod_seen + 1;
  (match c.client_brand with
      Brand_unknown -> () (* be careful, raising an exception here will
abort all other operations after that point for this client...*)
    | b ->
      stats_by_brand.(brand_to_int b).brand_seen <-
        stats_by_brand.(brand_to_int b).brand_seen + 1;
      !!gstats_by_brand.(brand_to_int b).brand_seen <-
        !!gstats_by_brand.(brand_to_int b).brand_seen + 1);
  if !!emule_mods_count then begin
  (match c.client_mod_brand with
      Brand_mod_unknown -> () (* be careful, raising an exception here will
abort all other operations after that point for this client...*)
    | b ->
      stats_by_brand_mod.(brand_mod_to_int b).brand_mod_seen <-
        stats_by_brand_mod.(brand_mod_to_int b).brand_mod_seen + 1;
      !!gstats_by_brand_mod.(brand_mod_to_int b).brand_mod_seen <-
        !!gstats_by_brand_mod.(brand_mod_to_int b).brand_mod_seen + 1)
  end

let count_banned c =
  stats_all.brand_banned <- stats_all.brand_banned + 1;
  if !!emule_mods_count && c.client_mod_brand != Brand_mod_unknown then mod_stats_all.brand_mod_banned <- mod_stats_all.brand_mod_banned + 1;
  (match c.client_brand with
      Brand_unknown -> () 
    | b ->
      stats_by_brand.(brand_to_int b).brand_banned <-
        stats_by_brand.(brand_to_int b).brand_banned + 1;
      !!gstats_by_brand.(brand_to_int b).brand_banned <-
        !!gstats_by_brand.(brand_to_int b).brand_banned + 1);
  if !!emule_mods_count then begin
  (match c.client_mod_brand with
      Brand_mod_unknown -> () 
    | b ->
      stats_by_brand_mod.(brand_mod_to_int b).brand_mod_banned <-
        stats_by_brand_mod.(brand_mod_to_int b).brand_mod_banned + 1;
      !!gstats_by_brand_mod.(brand_mod_to_int b).brand_mod_banned <-
        !!gstats_by_brand_mod.(brand_mod_to_int b).brand_mod_banned + 1)
  end

let count_filerequest c =
  stats_all.brand_filerequest <- stats_all.brand_filerequest + 1;
  if !!emule_mods_count && c.client_mod_brand != Brand_mod_unknown then mod_stats_all.brand_mod_filerequest <- mod_stats_all.brand_mod_filerequest + 1;
  (match c.client_brand with
      Brand_unknown -> ()
    | b ->
      stats_by_brand.(brand_to_int b).brand_filerequest <-
        stats_by_brand.(brand_to_int b).brand_filerequest + 1;
      !!gstats_by_brand.(brand_to_int b).brand_filerequest <-
	!!gstats_by_brand.(brand_to_int b).brand_filerequest + 1);
  if !!emule_mods_count then begin
  (match c.client_mod_brand with
      Brand_mod_unknown -> ()
    | b ->
      stats_by_brand_mod.(brand_mod_to_int b).brand_mod_filerequest <-
        stats_by_brand_mod.(brand_mod_to_int b).brand_mod_filerequest + 1;
      !!gstats_by_brand_mod.(brand_mod_to_int b).brand_mod_filerequest <-
	!!gstats_by_brand_mod.(brand_mod_to_int b).brand_mod_filerequest + 1)
  end

let count_download c f v =
  download_counter := Int64.add !download_counter v;
  c.client_downloaded <- Int64.add c.client_downloaded v;
  stats_all.brand_download <- Int64.add stats_all.brand_download v;
  if !!emule_mods_count && c.client_mod_brand != Brand_mod_unknown then mod_stats_all.brand_mod_download <- Int64.add mod_stats_all.brand_mod_download v;
  (match c.client_brand with
      Brand_unknown -> ()
    | b ->
      stats_by_brand.(brand_to_int b).brand_download <-
        Int64.add stats_by_brand.(brand_to_int b).brand_download v;
      !!gstats_by_brand.(brand_to_int b).brand_download <-
        Int64.add !!gstats_by_brand.(brand_to_int b).brand_download v);
  if !!emule_mods_count then begin
  (match c.client_mod_brand with
      Brand_mod_unknown -> ()
    | b ->
      stats_by_brand_mod.(brand_mod_to_int b).brand_mod_download <-
        Int64.add stats_by_brand_mod.(brand_mod_to_int b).brand_mod_download v;
      !!gstats_by_brand_mod.(brand_mod_to_int b).brand_mod_download <-
        Int64.add !!gstats_by_brand_mod.(brand_mod_to_int b).brand_mod_download v)
  end

let count_upload c f v =
  upload_counter := Int64.add !upload_counter v;
  c.client_uploaded <- Int64.add c.client_uploaded v;
  stats_all.brand_upload <- Int64.add stats_all.brand_upload v;
  if !!emule_mods_count && c.client_mod_brand != Brand_mod_unknown then mod_stats_all.brand_mod_upload <- Int64.add mod_stats_all.brand_mod_upload v;
  (match c.client_brand with
      Brand_unknown -> ()
    | b ->
      stats_by_brand.(brand_to_int b).brand_upload <-
        Int64.add stats_by_brand.(brand_to_int b).brand_upload v;
      !!gstats_by_brand.(brand_to_int b).brand_upload <-
        Int64.add !!gstats_by_brand.(brand_to_int b).brand_upload v);
  if !!emule_mods_count then begin
  (match c.client_mod_brand with
      Brand_mod_unknown -> ()
    | b ->
      stats_by_brand_mod.(brand_mod_to_int b).brand_mod_upload <-
        Int64.add stats_by_brand_mod.(brand_mod_to_int b).brand_mod_upload v;
      !!gstats_by_brand_mod.(brand_mod_to_int b).brand_mod_upload <-
        Int64.add !!gstats_by_brand_mod.(brand_mod_to_int b).brand_mod_upload v)
  end

let percent_of_ints x y = 
  if y <> 0 then 100. *. (float_of_int x /. float_of_int y)
  else 0.

let percent_of_int64s x y = 
  if y <> Int64.zero then 100. *. (Int64.to_float x /. Int64.to_float y)
  else 0.
      
let print_stats buf =
  let one_minute = 60 in
  let one_hour = 3600 in
  let one_day = 86400 in
  let uptime = last_time () - start_time in
  let days = uptime / one_day in
  let rem = uptime - days * one_day in
  let hours = rem / one_hour in
  let rem = rem - hours * one_hour in
  let mins = rem / one_minute in
    Printf.bprintf buf "Uptime: %d seconds (%d+%02d:%02d)\n" uptime days hours mins;


  if stats_all.brand_seen = 0 then
    Printf.bprintf buf "You haven't connected to any client yet\n"
  else begin
    Printf.bprintf buf "\n     Successful Connections: %18d\n" stats_all.brand_seen;
    for i=1 to brand_count-1 do
      Printf.bprintf buf "%27s: %18d (%5.1f %%)\n" 
	(brand_to_string (brand_of_int i)) 
	stats_by_brand.(i).brand_seen 
	(percent_of_ints stats_by_brand.(i).brand_seen stats_all.brand_seen)
    done
  end;

  if stats_all.brand_filerequest = 0 then
    Printf.bprintf buf "You weren't asked for any file yet\n"
  else begin
    Printf.bprintf buf "\nTotal filerequests received: %18d\n" stats_all.brand_filerequest;
    for i=1 to brand_count-1 do
      Printf.bprintf buf "%27s: %18d (%5.1f %%)\n" 
	(brand_to_string (brand_of_int i))
	stats_by_brand.(i).brand_filerequest 
	(percent_of_ints stats_by_brand.(i).brand_filerequest stats_all.brand_filerequest)
    done
  end;

  if stats_all.brand_download = Int64.zero then
    Printf.bprintf buf "You didn't download anything yet\n"
  else begin
      Printf.bprintf buf "\n            Total downloads: %18s (%5.1f KB/s)\n"
      (Int64.to_string stats_all.brand_download) 
      ((Int64.to_float stats_all.brand_download) /. (float_of_int uptime) /. 1024.0);
    for i=1 to brand_count-1 do
      Printf.bprintf buf "%27s: %18s (%5.1f %%)\n" 
	(brand_to_string (brand_of_int i))
	(Int64.to_string stats_by_brand.(i).brand_download) 
	(percent_of_int64s stats_by_brand.(i).brand_download stats_all.brand_download)
    done
  end;

  if stats_all.brand_upload = Int64.zero then
    Printf.bprintf buf "You didn't upload anything yet\n"
  else begin
      Printf.bprintf buf "\n              Total uploads: %18s (%5.1f KB/s)\n"
      (Int64.to_string stats_all.brand_upload)
      ((Int64.to_float stats_all.brand_upload) /. (float_of_int uptime) /. 1024.0);
    for i=1 to brand_count-1 do
      Printf.bprintf buf "%27s: %18s (%5.1f %%)\n" 
	(brand_to_string (brand_of_int i))
	(Int64.to_string stats_by_brand.(i).brand_upload) 
	(percent_of_int64s stats_by_brand.(i).brand_upload stats_all.brand_upload)
    done
  end;
  
  if stats_all.brand_banned = 0 then
    Printf.bprintf buf "You didn't ban any client yet\n"
  else begin
      Printf.bprintf buf "\n                 Total bans: %18d\n" stats_all.brand_banned;
    for i=1 to brand_count-1 do
      Printf.bprintf buf "%27s: %18d (%5.1f %%)\n" 
	(brand_to_string (brand_of_int i)) 
	stats_by_brand.(i).brand_banned 
	(percent_of_ints stats_by_brand.(i).brand_banned stats_all.brand_banned)
    done
  end
  

let stats_html_header buf = 
  html_mods_table_header buf "csTable" "cs" [ 
   ( "0", "srh", "Client brand", "Brand" ) ; 
   ( "0", "srh", "Separator", ":" ) ; 
   ( "1", "srh ar", "Successful connections", "Seen" ) ; 
   ( "1", "srh", "Successful connections percent", "%" ) ; 
   ( "0", "srh", "Separator", "|" ) ; 
   ( "1", "srh ar", "File requests received", "Reqs" ) ; 
   ( "1", "srh", "File requests received percent", "%" ) ; 
   ( "0", "srh", "Separator", "|" ) ; 
   ( "1", "srh ar", "Total bans", "B" ) ; 
   ( "1", "srh", "Total bans percent", "%" ) ; 
   ( "0", "srh", "Separator", "|" ) ; 
   ( "1", "srh ar", "Total uploads", "UL" ) ; 
   ( "1", "srh", "Total uploads percent", "%" ) ; 
   ( "1", "srh ar", "Total uploads average KB/s", "KB/s" ) ; 
   ( "0", "srh", "Separator", "|" ) ; 
   ( "1", "srh ar", "Total downloads", "DL" ) ; 
   ( "1", "srh", "Total downloads percent", "%" ) ; 
   ( "1", "srh ar", "Total downloads average KB/s", "KB/s" ) ; 
   ( "0", "srh", "Separator", "|" ) ; 
   ( "1", "srh", "Total uploads:downloads ratio", "U:DL" ) ]

let new_print_stats buf o =
  let one_minute = 60 in
  let one_hour = 3600 in
  let one_day = 86400 in
  let uptime = last_time () - start_time in
  let days = uptime / one_day in
  let rem = maxi 1 (uptime - days * one_day) in
  
  let hours = rem / one_hour in
  let rem = rem - hours * one_hour in
  let mins = rem / one_minute in
  
  let sstats_all = 
    let stat = {
        brand_seen = 0;
        brand_banned = 0;
        brand_filerequest = 0;
        brand_download = Int64.zero;
        brand_upload = Int64.zero
      }
    in stat in

  for i=0 to brand_count-1 do
    if ( brand_of_int i != Brand_server ) then begin
        sstats_all.brand_seen <- sstats_all.brand_seen + stats_by_brand.(i).brand_seen;
        sstats_all.brand_filerequest <- sstats_all.brand_filerequest + stats_by_brand.(i).brand_filerequest;
        sstats_all.brand_download <- Int64.add sstats_all.brand_download stats_by_brand.(i).brand_download ;
        sstats_all.brand_upload <- Int64.add sstats_all.brand_upload stats_by_brand.(i).brand_upload;
        sstats_all.brand_banned <- sstats_all.brand_banned + stats_by_brand.(i).brand_banned;
    end;
  done; 

  let gstats_all = 
    let stat = {
        brand_seen = 0;
        brand_banned = 0;
        brand_filerequest = 0;
        brand_download = Int64.zero;
        brand_upload = Int64.zero
      }
    in stat in
  
  for i=0 to brand_count-1 do
    gstats_all.brand_seen <- gstats_all.brand_seen + !!gstats_by_brand.(i).brand_seen;
    gstats_all.brand_filerequest <- gstats_all.brand_filerequest + !!gstats_by_brand.(i).brand_filerequest;
    gstats_all.brand_download <- Int64.add gstats_all.brand_download !!gstats_by_brand.(i).brand_download ;
    gstats_all.brand_upload <- Int64.add gstats_all.brand_upload !!gstats_by_brand.(i).brand_upload;
    gstats_all.brand_banned <- gstats_all.brand_banned + !!gstats_by_brand.(i).brand_banned;
  done; 
  
  if use_html_mods o then
    begin
      Printf.bprintf buf "\\<div class=\\\"cs\\\"\\>Session Uptime: %d seconds (%d+%02d:%02d)\\</div\\>" uptime days hours mins;
      stats_html_header buf;
      
      let counter = ref 0 in
      let showTotal = ref false in

      for i=1 to brand_count do
        if i=brand_count then showTotal := true;
        if !showTotal || ( brand_of_int i != Brand_server && stats_by_brand.(i).brand_seen > 0 ) then begin
          incr counter;
          Printf.bprintf buf "\\<tr class=\\\"%s\\\"\\>" (if (!counter mod 2 == 0) then "dl-1" else "dl-2");
          Printf.bprintf buf "
\\<td class=\\\"sr\\\"\\>%s\\</td\\>
\\<td class=\\\"sr \\\"\\>:\\</td\\>
\\<td class=\\\"sr ar\\\"\\>%d\\</td\\>
\\<td class=\\\"srp \\\"\\>(%.f%%)\\</td\\>
\\<td class=\\\"sr \\\"\\>|\\</td\\>

\\<td class=\\\"sr ar\\\"\\>%d\\</td\\>
\\<td class=\\\"srp \\\"\\>(%.f%%)\\</td\\>
\\<td class=\\\"sr \\\"\\>|\\</td\\>

\\<td class=\\\"sr ar\\\"\\>%d\\</td\\>
\\<td class=\\\"srp\\\"\\>(%.0f%%)\\</td\\>
\\<td class=\\\"sr \\\"\\>|\\</td\\>

\\<td class=\\\"sr ar\\\"\\>%s\\</td\\>
\\<td class=\\\"srp \\\"\\>(%.0f%%)\\</td\\>
\\<td class=\\\"sr ar\\\"\\>%.1f\\</td\\>
\\<td class=\\\"sr \\\"\\>|\\</td\\>

\\<td class=\\\"sr ar\\\"\\>%s\\</td\\>
\\<td class=\\\"srp \\\"\\>(%.0f%%)\\</td\\>
\\<td class=\\\"sr ar \\\"\\>%.1f\\</td\\>
\\<td class=\\\"sr \\\"\\>|\\</td\\>

\\<td class=\\\"sr \\\"\\>1:%.2f\\</td\\>
\\</tr\\>\n"
            
            (if !showTotal then "Total" else (brand_to_string (brand_of_int i))) 
            
            (if !showTotal then sstats_all.brand_seen else
                    stats_by_brand.(i).brand_seen) 
                    
            (if !showTotal then 100.0 else (percent_of_ints
            stats_by_brand.(i).brand_seen stats_all.brand_seen))
          
            (if !showTotal then sstats_all.brand_filerequest else stats_by_brand.(i).brand_filerequest)

            (if !showTotal then 100.0 else (percent_of_ints stats_by_brand.(i).brand_filerequest stats_all.brand_filerequest))

            (if !showTotal then sstats_all.brand_banned else
                    stats_by_brand.(i).brand_banned)
                    
            (max 0.0 (if !showTotal then (percent_of_ints sstats_all.brand_banned sstats_all.brand_seen) 
             else (percent_of_ints stats_by_brand.(i).brand_banned stats_all.brand_banned)))

            (size_of_int64 (if !showTotal then sstats_all.brand_upload else
                    stats_by_brand.(i).brand_upload)) 
          
            (max 0.0 (if !showTotal then 100.0 else (percent_of_int64s
            stats_by_brand.(i).brand_upload stats_all.brand_upload)))

            (if !showTotal then ((Int64.to_float sstats_all.brand_upload) /. (float_of_int uptime) /. 1024.0)
            else ((Int64.to_float stats_by_brand.(i).brand_upload) /.  (float_of_int uptime) /. 1024.0))
          
            (size_of_int64 (if !showTotal then sstats_all.brand_download else
                    stats_by_brand.(i).brand_download)) 

            (max 0.0 (if !showTotal then 100.0 else (percent_of_int64s
            stats_by_brand.(i).brand_download stats_all.brand_download)))

            (if !showTotal then ((Int64.to_float sstats_all.brand_download) /. (float_of_int uptime) /. 1024.0)
            else ((Int64.to_float stats_by_brand.(i).brand_download) /.  (float_of_int uptime) /. 1024.0))

            (if !showTotal then 
             (if sstats_all.brand_upload = Int64.zero then 0.0 else 
	    	  ( (Int64.to_float sstats_all.brand_download) /.  (Int64.to_float sstats_all.brand_upload) ))
            else 
             (if stats_by_brand.(i).brand_upload = Int64.zero then 0.0 else 
			 ( (Int64.to_float stats_by_brand.(i).brand_download) /.
             (Int64.to_float stats_by_brand.(i).brand_upload) )));
        end
      done;
      Printf.bprintf buf "\\</table\\>\\</div\\>\n";
      
      let gdays = (guptime () + uptime) / one_day in
      let grem = maxi 1 ((guptime () + uptime) - gdays * one_day) in
      
      let ghours = grem / one_hour in
      let grem = grem - ghours * one_hour in
      let gmins = grem / one_minute in
      
      Printf.bprintf buf "\n\\<div class=\\\"cs\\\"\\>Total Uptime: %d seconds (%d+%02d:%02d)\\</div\\>" (guptime() + uptime) gdays ghours gmins;
      stats_html_header buf;
      
      showTotal := false;
      for i=1 to brand_count do
       if i=brand_count then showTotal := true;
       if !showTotal || ( brand_of_int i != Brand_server && !!gstats_by_brand.(i).brand_seen > 0 ) then begin
          incr counter;
          Printf.bprintf buf "\\<tr class=\\\"%s\\\"\\>" (if (!counter mod 2 == 0) then "dl-1" else "dl-2");
          Printf.bprintf buf "
\\<td class=\\\"sr\\\"\\>%s\\</td\\>
\\<td class=\\\"sr \\\"\\>:\\</td\\>
\\<td class=\\\"sr ar\\\"\\>%d\\</td\\>
\\<td class=\\\"srp \\\"\\>(%.f%%)\\</td\\>
\\<td class=\\\"sr \\\"\\>|\\</td\\>

\\<td class=\\\"sr ar\\\"\\>%d\\</td\\>
\\<td class=\\\"srp \\\"\\>(%.f%%)\\</td\\>
\\<td class=\\\"sr \\\"\\>|\\</td\\>

\\<td class=\\\"sr ar\\\"\\>%d\\</td\\>
\\<td class=\\\"srp\\\"\\>(%.0f%%)\\</td\\>
\\<td class=\\\"sr \\\"\\>|\\</td\\>

\\<td class=\\\"sr ar\\\"\\>%s\\</td\\>
\\<td class=\\\"srp \\\"\\>(%.0f%%)\\</td\\>
\\<td class=\\\"sr ar\\\"\\>%.1f\\</td\\>
\\<td class=\\\"sr \\\"\\>|\\</td\\>

\\<td class=\\\"sr ar\\\"\\>%s\\</td\\>
\\<td class=\\\"srp \\\"\\>(%.0f%%)\\</td\\>
\\<td class=\\\"sr ar \\\"\\>%.1f\\</td\\>
\\<td class=\\\"sr \\\"\\>|\\</td\\>

\\<td class=\\\"sr \\\"\\>1:%.2f\\</td\\>

\\</tr\\>\n"

          (if !showTotal then "Total" else (brand_to_string (brand_of_int i)) )

          (if !showTotal then gstats_all.brand_seen else !!gstats_by_brand.(i).brand_seen) 

          (if !showTotal then 100. else (percent_of_ints (!!gstats_by_brand.(i).brand_seen) gstats_all.brand_seen))
          
          (if !showTotal then gstats_all.brand_filerequest else !!gstats_by_brand.(i).brand_filerequest)

          (if !showTotal then 100. else (percent_of_ints (!!gstats_by_brand.(i).brand_filerequest) gstats_all.brand_filerequest))
          
          (if !showTotal then gstats_all.brand_banned else !!gstats_by_brand.(i).brand_banned) 

          (max 0.0 (if !showTotal then (percent_of_ints gstats_all.brand_banned
          gstats_all.brand_seen) else (percent_of_ints (!!gstats_by_brand.(i).brand_banned) gstats_all.brand_banned)))

          (size_of_int64 (if !showTotal then gstats_all.brand_upload else !!gstats_by_brand.(i).brand_upload))
          
          (if !showTotal then 100. else (max 0.0 (percent_of_int64s
          (!!gstats_by_brand.(i).brand_upload) gstats_all.brand_upload)))

          (if !showTotal then ((Int64.to_float gstats_all.brand_upload) /. (float_of_int (guptime() + uptime)) /. 1024.0)
          else ((Int64.to_float (!!gstats_by_brand.(i).brand_upload)) /.  (float_of_int (guptime() + uptime)) /. 1024.0))

          (size_of_int64 (if !showTotal then gstats_all.brand_download else !!gstats_by_brand.(i).brand_download))
          
          (if !showTotal then 100. else (max 0.0 (percent_of_int64s
          (!!gstats_by_brand.(i).brand_download) gstats_all.brand_download)))

          (if !showTotal then ((Int64.to_float gstats_all.brand_download) /. (float_of_int (guptime() + uptime)) /. 1024.0)
          else ((Int64.to_float (!!gstats_by_brand.(i).brand_download)) /.  (float_of_int (guptime() + uptime)) /. 1024.0))

         (if !showTotal then (if gstats_all.brand_upload = Int64.zero then 0.0 else 
		  ((Int64.to_float gstats_all.brand_download) /.  (Int64.to_float gstats_all.brand_upload) )) else  
          (if !!gstats_by_brand.(i).brand_upload = Int64.zero then 0.0 else 
			( (Int64.to_float !!gstats_by_brand.(i).brand_download) /.
            (Int64.to_float !!gstats_by_brand.(i).brand_upload) )))
        end
      done;
      Printf.bprintf buf "\\</table\\>\\</div\\>\n";
    end
  else
    begin
      Printf.bprintf buf "Session Uptime: %d seconds (%d days %d hours %d minutes)\n" uptime days hours mins;
      Printf.bprintf buf "Client Brand|    seen      |     Downloads      |      Uploads       |   Banned   |  Requests\n";
      Printf.bprintf buf "------------+--------------+--------------------+--------------------+------------+--------------\n";
      
      for i=1 to brand_count-1 do
        if brand_of_int i != Brand_server && stats_by_brand.(i).brand_seen > 0 then (* dont print server stats *)
          let brandstr = 
            if brand_of_int i = Brand_mldonkey3 then 
              "trusted mld"
            else
              brand_to_string (brand_of_int i) in
          
          Printf.bprintf buf "%-12s|%9d %3.f%%|%9.1f %5.1f %3.0f%%|%9.1f %5.1f %3.0f%%|%7d %3.0f%%|%9d %3.0f%%\n"
            (brandstr)
            stats_by_brand.(i).brand_seen 
              (percent_of_ints stats_by_brand.(i).brand_seen stats_all.brand_seen)
            ((Int64.to_float stats_by_brand.(i).brand_download) /. 1024.0 /. 1024.0)
              ((Int64.to_float stats_by_brand.(i).brand_download) /. (float_of_int uptime) /. 1024.0)
              (percent_of_int64s stats_by_brand.(i).brand_download stats_all.brand_download)
            ((Int64.to_float stats_by_brand.(i).brand_upload) /. 1024.0 /. 1024.0)
              ((Int64.to_float stats_by_brand.(i).brand_upload) /. (float_of_int uptime) /. 1024.0)
              (percent_of_int64s stats_by_brand.(i).brand_upload stats_all.brand_upload)
            stats_by_brand.(i).brand_banned 
              (percent_of_ints stats_by_brand.(i).brand_banned stats_all.brand_banned)
            stats_by_brand.(i).brand_filerequest
              (percent_of_ints stats_by_brand.(i).brand_filerequest stats_all.brand_filerequest)
      done;

      Printf.bprintf buf "------------+--------------+--------------------+--------------------+------------+--------------\n";
      Printf.bprintf buf "%-12s|%9d     |%9.1f %5.1f     |%9.1f %5.1f     |%7d     |%9d\n"
        "Total"
        sstats_all.brand_seen
        ((Int64.to_float sstats_all.brand_download) /. 1024.0 /. 1024.0)
          ((Int64.to_float sstats_all.brand_download) /. (float_of_int uptime) /. 1024.0)
        ((Int64.to_float sstats_all.brand_upload) /. 1024.0 /. 1024.0)
          ((Int64.to_float sstats_all.brand_upload) /. (float_of_int uptime) /. 1024.0)
        sstats_all.brand_banned 
        sstats_all.brand_filerequest;

      let gdays = (guptime () + uptime) / one_day in
      let grem = maxi 1 ((guptime () + uptime) - gdays * one_day) in
      let ghours = grem / one_hour in
      let grem = grem - ghours * one_hour in
      let gmins = grem / one_minute in
      Printf.bprintf buf "\nTotal Uptime: %d seconds (%d days %d hours %d minutes)\n" (guptime() + uptime) gdays ghours gmins;
      Printf.bprintf buf "Client Brand|    seen      |     Downloads      |      Uploads       |   Banned   |  Requests\n";
      Printf.bprintf buf "------------+--------------+--------------------+--------------------+------------+--------------\n";
      
      for i=1 to brand_count-1 do
        if brand_of_int i != Brand_server && !!gstats_by_brand.(i).brand_seen > 0 then (* dont print server stats *)
          let brandstr = 
            if brand_of_int i = Brand_mldonkey3 then 
              "trusted mld"
            else
              brand_to_string (brand_of_int i) in
          
          Printf.bprintf buf "%-12s|%9d %3.f%%|%9.1f %5.1f %3.0f%%|%9.1f %5.1f %3.0f%%|%7d %3.0f%%|%9d %3.0f%%\n"
            (brandstr)
            !!gstats_by_brand.(i).brand_seen 
              (percent_of_ints !!gstats_by_brand.(i).brand_seen gstats_all.brand_seen)
            ((Int64.to_float !!gstats_by_brand.(i).brand_download) /. 1024.0 /. 1024.0)
              ((Int64.to_float !!gstats_by_brand.(i).brand_download) /. (float_of_int (guptime() + uptime)) /. 1024.0)
              (percent_of_int64s !!gstats_by_brand.(i).brand_download gstats_all.brand_download)
            ((Int64.to_float !!gstats_by_brand.(i).brand_upload) /. 1024.0 /. 1024.0)
              ((Int64.to_float !!gstats_by_brand.(i).brand_upload) /. (float_of_int (guptime() + uptime)) /. 1024.0)
              (percent_of_int64s !!gstats_by_brand.(i).brand_upload gstats_all.brand_upload)
            !!gstats_by_brand.(i).brand_banned 
              (percent_of_ints !!gstats_by_brand.(i).brand_banned gstats_all.brand_banned)
            !!gstats_by_brand.(i).brand_filerequest
              (percent_of_ints !!gstats_by_brand.(i).brand_filerequest gstats_all.brand_filerequest)
      done;

      Printf.bprintf buf "------------+--------------+--------------------+--------------------+------------+--------------\n";
      Printf.bprintf buf "%-12s|%9d     |%9.1f %5.1f     |%9.1f %5.1f     |%7d     |%9d\n"
      "Total"
      gstats_all.brand_seen
      ((Int64.to_float gstats_all.brand_download) /. 1024.0 /. 1024.0)
        ((Int64.to_float gstats_all.brand_download) /. (float_of_int (guptime() + uptime)) /. 1024.0)
      ((Int64.to_float gstats_all.brand_upload) /. 1024.0 /. 1024.0)
        ((Int64.to_float gstats_all.brand_upload) /. (float_of_int (guptime() + uptime)) /. 1024.0)
      gstats_all.brand_banned 
      gstats_all.brand_filerequest;
      
    end

let new_print_mod_stats buf o =
  let one_minute = 60 in
  let one_hour = 3600 in
  let one_day = 86400 in
  let uptime = last_time () - start_time in
  let days = uptime / one_day in
  let rem = maxi 1 (uptime - days * one_day) in
  
  let hours = rem / one_hour in
  let rem = rem - hours * one_hour in
  let mins = rem / one_minute in

  if !!emule_mods_count then
  
  if use_html_mods o then
    begin
      Printf.bprintf buf "\\<div class=\\\"cs\\\"\\>Session Uptime: %d seconds (%d+%02d:%02d)\\</div\\>" uptime days hours mins;
      stats_html_header buf;
      
      let counter = ref 0 in
      let showTotal = ref false in

      for i=1 to brand_mod_count do
        if i=brand_mod_count then showTotal := true;
        if !showTotal || ( !!emule_mods_showall || stats_by_brand_mod.(i).brand_mod_seen > 0 ) then begin
          incr counter;
          Printf.bprintf buf "\\<tr class=\\\"%s\\\"\\>" (if (!counter mod 2 == 0) then "dl-1" else "dl-2");
          Printf.bprintf buf "
\\<td class=\\\"sr\\\"\\>%s\\</td\\>
\\<td class=\\\"sr \\\"\\>:\\</td\\>
\\<td class=\\\"sr ar\\\"\\>%d\\</td\\>
\\<td class=\\\"srp \\\"\\>(%.f%%)\\</td\\>
\\<td class=\\\"sr \\\"\\>|\\</td\\>

\\<td class=\\\"sr ar\\\"\\>%d\\</td\\>
\\<td class=\\\"srp \\\"\\>(%.f%%)\\</td\\>
\\<td class=\\\"sr \\\"\\>|\\</td\\>

\\<td class=\\\"sr ar\\\"\\>%d\\</td\\>
\\<td class=\\\"srp\\\"\\>(%.0f%%)\\</td\\>
\\<td class=\\\"sr \\\"\\>|\\</td\\>

\\<td class=\\\"sr ar\\\"\\>%s\\</td\\>
\\<td class=\\\"srp \\\"\\>(%.0f%%)\\</td\\>
\\<td class=\\\"sr ar\\\"\\>%.1f\\</td\\>
\\<td class=\\\"sr \\\"\\>|\\</td\\>

\\<td class=\\\"sr ar\\\"\\>%s\\</td\\>
\\<td class=\\\"srp \\\"\\>(%.0f%%)\\</td\\>
\\<td class=\\\"sr ar \\\"\\>%.1f\\</td\\>
\\<td class=\\\"sr \\\"\\>|\\</td\\>

\\<td class=\\\"sr \\\"\\>1:%.2f\\</td\\>
\\</tr\\>\n"
            
            (if !showTotal then "Total" else (brand_mod_to_string (brand_mod_of_int i))) 
            
            (if !showTotal then mod_stats_all.brand_mod_seen else
                    stats_by_brand_mod.(i).brand_mod_seen) 
                    
            (if !showTotal then 100.0 else (percent_of_ints
            stats_by_brand_mod.(i).brand_mod_seen mod_stats_all.brand_mod_seen))
          
            (if !showTotal then mod_stats_all.brand_mod_filerequest else stats_by_brand_mod.(i).brand_mod_filerequest)

            (if !showTotal then 100.0 else (percent_of_ints stats_by_brand_mod.(i).brand_mod_filerequest mod_stats_all.brand_mod_filerequest))

            (if !showTotal then mod_stats_all.brand_mod_banned else
                    stats_by_brand_mod.(i).brand_mod_banned)
                    
            (max 0.0 (if !showTotal then (percent_of_ints mod_stats_all.brand_mod_banned mod_stats_all.brand_mod_seen) 
             else (percent_of_ints stats_by_brand_mod.(i).brand_mod_banned mod_stats_all.brand_mod_banned)))

            (size_of_int64 (if !showTotal then mod_stats_all.brand_mod_upload else
                    stats_by_brand_mod.(i).brand_mod_upload)) 
          
            (max 0.0 (if !showTotal then 100.0 else (percent_of_int64s
            stats_by_brand_mod.(i).brand_mod_upload mod_stats_all.brand_mod_upload)))

            (if !showTotal then ((Int64.to_float mod_stats_all.brand_mod_upload) /. (float_of_int uptime) /. 1024.0)
            else ((Int64.to_float stats_by_brand_mod.(i).brand_mod_upload) /.  (float_of_int uptime) /. 1024.0))
          
            (size_of_int64 (if !showTotal then mod_stats_all.brand_mod_download else
                    stats_by_brand_mod.(i).brand_mod_download)) 

            (max 0.0 (if !showTotal then 100.0 else (percent_of_int64s
            stats_by_brand_mod.(i).brand_mod_download mod_stats_all.brand_mod_download)))

            (if !showTotal then ((Int64.to_float mod_stats_all.brand_mod_download) /. (float_of_int uptime) /. 1024.0)
            else ((Int64.to_float stats_by_brand_mod.(i).brand_mod_download) /.  (float_of_int uptime) /. 1024.0))

            (if !showTotal then 
             (if mod_stats_all.brand_mod_upload = Int64.zero then 0.0 else 
	    	  ( (Int64.to_float mod_stats_all.brand_mod_download) /.  (Int64.to_float mod_stats_all.brand_mod_upload) ))
            else 
             (if stats_by_brand_mod.(i).brand_mod_upload = Int64.zero then 0.0 else 
			 ( (Int64.to_float stats_by_brand_mod.(i).brand_mod_download) /.
             (Int64.to_float stats_by_brand_mod.(i).brand_mod_upload) )));
        end
      done;
      Printf.bprintf buf "\\</table\\>\\</div\\>\n";
      
      let gstats_all = 
        let stat = {
            brand_mod_seen = 0;
            brand_mod_banned = 0;
            brand_mod_filerequest = 0;
            brand_mod_download = Int64.zero;
            brand_mod_upload = Int64.zero
          }
        in stat in
      
      for i=0 to brand_mod_count-1 do
        
        gstats_all.brand_mod_seen <- gstats_all.brand_mod_seen + !!gstats_by_brand_mod.(i).brand_mod_seen;
        gstats_all.brand_mod_filerequest <- gstats_all.brand_mod_filerequest + !!gstats_by_brand_mod.(i).brand_mod_filerequest;
        gstats_all.brand_mod_download <- Int64.add gstats_all.brand_mod_download !!gstats_by_brand_mod.(i).brand_mod_download ;
        gstats_all.brand_mod_upload <- Int64.add gstats_all.brand_mod_upload !!gstats_by_brand_mod.(i).brand_mod_upload;
        gstats_all.brand_mod_banned <- gstats_all.brand_mod_banned + !!gstats_by_brand_mod.(i).brand_mod_banned;
      
      done; 
      let gdays = (guptime () + uptime) / one_day in
      let grem = maxi 1 ((guptime () + uptime) - gdays * one_day) in
      
      let ghours = grem / one_hour in
      let grem = grem - ghours * one_hour in
      let gmins = grem / one_minute in
      
      Printf.bprintf buf "\n\\<div class=\\\"cs\\\"\\>Total Uptime: %d seconds (%d+%02d:%02d)\\</div\\>" (guptime() + uptime) gdays ghours gmins;
      stats_html_header buf;
      
      showTotal := false;
      for i=1 to brand_mod_count do
       if i=brand_mod_count then showTotal := true;
       if !showTotal || ( !!emule_mods_showall || !!gstats_by_brand_mod.(i).brand_mod_seen > 0 ) then begin
          incr counter;
          Printf.bprintf buf "\\<tr class=\\\"%s\\\"\\>" (if (!counter mod 2 == 0) then "dl-1" else "dl-2");
          Printf.bprintf buf "
\\<td class=\\\"sr\\\"\\>%s\\</td\\>
\\<td class=\\\"sr \\\"\\>:\\</td\\>
\\<td class=\\\"sr ar\\\"\\>%d\\</td\\>
\\<td class=\\\"srp \\\"\\>(%.f%%)\\</td\\>
\\<td class=\\\"sr \\\"\\>|\\</td\\>

\\<td class=\\\"sr ar\\\"\\>%d\\</td\\>
\\<td class=\\\"srp \\\"\\>(%.f%%)\\</td\\>
\\<td class=\\\"sr \\\"\\>|\\</td\\>

\\<td class=\\\"sr ar\\\"\\>%d\\</td\\>
\\<td class=\\\"srp\\\"\\>(%.0f%%)\\</td\\>
\\<td class=\\\"sr \\\"\\>|\\</td\\>

\\<td class=\\\"sr ar\\\"\\>%s\\</td\\>
\\<td class=\\\"srp \\\"\\>(%.0f%%)\\</td\\>
\\<td class=\\\"sr ar\\\"\\>%.1f\\</td\\>
\\<td class=\\\"sr \\\"\\>|\\</td\\>

\\<td class=\\\"sr ar\\\"\\>%s\\</td\\>
\\<td class=\\\"srp \\\"\\>(%.0f%%)\\</td\\>
\\<td class=\\\"sr ar \\\"\\>%.1f\\</td\\>
\\<td class=\\\"sr \\\"\\>|\\</td\\>

\\<td class=\\\"sr \\\"\\>1:%.2f\\</td\\>

\\</tr\\>\n"

          (if !showTotal then "Total" else (brand_mod_to_string (brand_mod_of_int i)) )

          (if !showTotal then gstats_all.brand_mod_seen else !!gstats_by_brand_mod.(i).brand_mod_seen) 

          (if !showTotal then 100. else (percent_of_ints (!!gstats_by_brand_mod.(i).brand_mod_seen) gstats_all.brand_mod_seen))
          
          (if !showTotal then gstats_all.brand_mod_filerequest else !!gstats_by_brand_mod.(i).brand_mod_filerequest)

          (if !showTotal then 100. else (percent_of_ints (!!gstats_by_brand_mod.(i).brand_mod_filerequest) gstats_all.brand_mod_filerequest))
          
          (if !showTotal then gstats_all.brand_mod_banned else !!gstats_by_brand_mod.(i).brand_mod_banned) 

          (max 0.0 (if !showTotal then (percent_of_ints gstats_all.brand_mod_banned
          gstats_all.brand_mod_seen) else (percent_of_ints (!!gstats_by_brand_mod.(i).brand_mod_banned) gstats_all.brand_mod_banned)))

          (size_of_int64 (if !showTotal then gstats_all.brand_mod_upload else !!gstats_by_brand_mod.(i).brand_mod_upload))
          
          (if !showTotal then 100. else (max 0.0 (percent_of_int64s
          (!!gstats_by_brand_mod.(i).brand_mod_upload) gstats_all.brand_mod_upload)))

          (if !showTotal then ((Int64.to_float gstats_all.brand_mod_upload) /. (float_of_int (guptime() + uptime)) /. 1024.0)
          else ((Int64.to_float (!!gstats_by_brand_mod.(i).brand_mod_upload)) /.  (float_of_int (guptime() + uptime)) /. 1024.0))

          (size_of_int64 (if !showTotal then gstats_all.brand_mod_download else !!gstats_by_brand_mod.(i).brand_mod_download))
          
          (if !showTotal then 100. else (max 0.0 (percent_of_int64s
          (!!gstats_by_brand_mod.(i).brand_mod_download) gstats_all.brand_mod_download)))

          (if !showTotal then ((Int64.to_float gstats_all.brand_mod_download) /. (float_of_int (guptime() + uptime)) /. 1024.0)
          else ((Int64.to_float (!!gstats_by_brand_mod.(i).brand_mod_download)) /.  (float_of_int (guptime() + uptime)) /. 1024.0))

         (if !showTotal then (if gstats_all.brand_mod_upload = Int64.zero then 0.0 else 
		  ((Int64.to_float gstats_all.brand_mod_download) /.  (Int64.to_float gstats_all.brand_mod_upload) )) else  
          (if !!gstats_by_brand_mod.(i).brand_mod_upload = Int64.zero then 0.0 else 
			( (Int64.to_float !!gstats_by_brand_mod.(i).brand_mod_download) /.
            (Int64.to_float !!gstats_by_brand_mod.(i).brand_mod_upload) )))
        end
      done;
      Printf.bprintf buf "\\</table\\>\\</div\\>\n";
    end
  else
    begin
      Printf.bprintf buf "Uptime: %d seconds (%d+%02d:%02d)\n" uptime days hours mins;
      Printf.bprintf buf "         MOD| seen      |  Downloads       |  Uploads         |  Banned\n";
      Printf.bprintf buf "------------+-----------+------------------+------------------+----------\n";
      Printf.bprintf buf "%-12s|%6d     |%7.1f %5.1f     |%7.1f %5.1f     |%5d %3.0f%%\n"
        
        "Total"
        mod_stats_all.brand_mod_seen
        ((Int64.to_float mod_stats_all.brand_mod_download) /. 1024.0 /. 1024.0)
      ((Int64.to_float mod_stats_all.brand_mod_download) /. (float_of_int uptime) /. 1024.0)
      ((Int64.to_float mod_stats_all.brand_mod_upload) /. 1024.0 /. 1024.0)
      ((Int64.to_float mod_stats_all.brand_mod_upload) /. (float_of_int uptime) /. 1024.0)
      mod_stats_all.brand_mod_banned 
        (percent_of_ints mod_stats_all.brand_mod_banned mod_stats_all.brand_mod_seen);
      
      for i=1 to brand_mod_count-1 do
          let brandstr = 
              brand_mod_to_string (brand_mod_of_int i) in
          
          Printf.bprintf buf "%-12s|%6d %3.f%%|%7.1f %5.1f %3.0f%%|%7.1f %5.1f %3.0f%%|%5d %3.0f%%\n"
            (brandstr)
          stats_by_brand_mod.(i).brand_mod_seen 
            (percent_of_ints stats_by_brand_mod.(i).brand_mod_seen mod_stats_all.brand_mod_seen)
          ((Int64.to_float stats_by_brand_mod.(i).brand_mod_download) /. 1024.0 /. 1024.0)
          ((Int64.to_float stats_by_brand_mod.(i).brand_mod_download) /. (float_of_int uptime) /. 1024.0)
          (percent_of_int64s stats_by_brand_mod.(i).brand_mod_download mod_stats_all.brand_mod_download)
          ((Int64.to_float stats_by_brand_mod.(i).brand_mod_upload) /. 1024.0 /. 1024.0)
          ((Int64.to_float stats_by_brand_mod.(i).brand_mod_upload) /. (float_of_int uptime) /. 1024.0)
          (percent_of_int64s stats_by_brand_mod.(i).brand_mod_upload mod_stats_all.brand_mod_upload)
          stats_by_brand_mod.(i).brand_mod_banned 
            (percent_of_ints stats_by_brand_mod.(i).brand_mod_banned mod_stats_all.brand_mod_banned)
      done
    end
  else
      Printf.bprintf buf "eMule mods statistics are disabled, to activate set emule_mods_count true \n"

let append_out name =
  open_out_gen [Open_wronly; Open_creat; Open_append; Open_text] 0o666 name


  (*
let save_download_history file = 
  
  let buf = Buffer.create 100 in

(* Some opcode for edonkey downloads *)
  buf_int8 buf 153;         (* opcode = stats *)
  buf_md4 buf !!client_md4; (* client md4, and NOT IP *)

  let time = Unix.time () in
  let time = Unix.localtime time in
  (* date on 5 bytes *)
  buf_int8 buf time.Unix.tm_hour;
  buf_int8 buf time.Unix.tm_mday;
  buf_int8 buf time.Unix.tm_mon;
  buf_int16 buf time.Unix.tm_year;
(* ANONYMISED Informations on the downloads: *)
(*   Send the SHA1 hash of the MD4+size, so that they cannot be recovered, 
          but they can be used to compare downloads. *)
  let m = Printf.sprintf "%s%Ld" 
      (Md4.Md4.direct_to_string file.file_md4) (file_size file) in
  let m = Md4.Sha1.string m in (* compute SHA1 of the string *)
(* SENT: 20 bytes *)
  Buffer.add_string buf (Md4.Sha1.direct_to_string m);

(* Send the magnitude of the size (power of 10) *)
  let size = ref (file_size file) in
  let m = ref 0 in
  while !size <> Int64.zero do
    incr m;
    size := !size // (Int64.of_int 10)
  done;
(* SENT: 1 byte *)
  buf_int8 buf !m;
  
  let current = ref [] in
  
  Intmap.iter (fun _ c ->
      let location =
        match c.client_kind with
          Indirect_location (name, md4) -> 
            Printf.sprintf "%s%s" name 
              (Md4.Md4.direct_to_string md4)
        | Known_location (ip,port) ->
            Printf.sprintf "%s%d"
              (Ip.to_string ip) port
      in

(* ANONYMISATION of the source: we compute the Sha1 digest of the source,
  which cannot be recovered from this information *)
      let location = Md4.Sha1.string location in (* compute SHA1 of the string *)
      current := location :: !current;
  ) file.file_locations;
  
  buf_list (fun buf s ->
      Buffer.add_string buf (Md4.Sha1.direct_to_string s)
  ) buf !current;

    
  let file_history = Filename.concat file_basedir "downloads.stats" in
  let oc = append_out file_history in
  output_string oc (Buffer.contents buf);
  close_out oc
*)  
    
let _ =
  register_commands 
    [
    "client_stats", "Network/Donkey",Arg_none (fun o ->
        let buf = o.conn_buf in
        print_stats buf;
        ""
    ), ":\t\t\t\tshow breakdown of download/upload by clients brand";
    
    "cs", "Network/Donkey",Arg_none (fun o ->
        let buf = o.conn_buf in
        new_print_stats buf o;
        ""
    ), ":\t\t\t\t\tshow table of download/upload by clients brand";
  
    "csm", "Network/Donkey",Arg_none (fun o ->
        let buf = o.conn_buf in
        new_print_mod_stats buf o;
        ""
    ), ":\t\t\t\t\tshow table of download/upload by eMule MODs";
  ]
