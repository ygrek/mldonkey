(* Copyright 2006 pango *)
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

open Printf2

(* list of functions to call when banning changes *)
let update_hooks = ref []

let add_update_hook f =
  update_hooks := f :: !update_hooks

let ip_blocking_list = ref Ip_set.bl_empty
let web_ip_blocking_list = ref Ip_set.bl_empty
let country_blocking_block = ref false
let country_blocking_list = 
  Array.create (Array.length Geoip.country_code_array) false

let ip_set_hit bl ip =
  match Ip_set.match_ip bl ip with
    | None -> None
    | Some br -> Some br.Ip_set.blocking_description

let geoip_hit cbl ip =
  let index = Geoip.get_country_code ip in
  if not !Geoip.active then None
  else if cbl.(index) then
    Some (Printf.sprintf "IPs from %s are currently blocked"
      Geoip.country_name_array.(index))
  else None

let update_bans () =
  Ip.banned :=
    (fun ip ->
      if Ip.local_ip ip then None else
      let block = ip_set_hit !web_ip_blocking_list ip in
      if block <> None then block else
      let block = ip_set_hit !ip_blocking_list ip in
      if block <> None then block else
      let block = geoip_hit country_blocking_list ip in
      if block <> None then block else 
      None
    );
  List.iter (fun f -> f ()) !update_hooks

let set_ip_blocking_list filename =
  ip_blocking_list :=
    if filename = "" then
      Ip_set.bl_empty
    else
      Ip_set.load filename;
  update_bans ()

let set_geoip_dat filename =
  Geoip.init (Geoip.unpack filename);
  update_bans ()

let set_ip_blocking_countries cl =
  let cl = List.map String.uppercase cl in
  Array.fill country_blocking_list 0 
    (Array.length country_blocking_list) !country_blocking_block;
  List.iter (fun cc ->
    if cc = "UNKNOWN" then 
      country_blocking_list.(0) <- (not !country_blocking_block)
    else
      try
	let index = Hashtbl.find Geoip.country_index cc in
	  country_blocking_list.(index) <- (not !country_blocking_block)
      with Not_found ->
	lprintf_nl "Country code %s not found" cc
  ) cl;
  update_bans ()

let set_ip_blocking_countries_block v =
  country_blocking_block := v;
  update_bans ()

let _ =
  CommonWeb.add_web_kind "guarding.p2p"
    "IP blocking lists (ipfilter and guardian v2 formats)"
    (fun _ filename ->
    try
      web_ip_blocking_list :=
	if filename = "" then
	  Ip_set.bl_empty
	else
	  Ip_set.load filename;
      update_bans ()
    with _ -> ());
  CommonWeb.add_web_kind "geoip.dat" "IP to country mapping database"
    (fun _ filename ->
    Geoip.init (Geoip.unpack filename);
    update_bans ());

  Heap.add_memstat "CommonBlocking" (fun level buf ->
      Printf.bprintf buf "  local ranges: %d\n" 
	(Ip_set.bl_length !ip_blocking_list);
      Printf.bprintf buf "  web ranges: %d\n" 
	(Ip_set.bl_length !web_ip_blocking_list)
   )
