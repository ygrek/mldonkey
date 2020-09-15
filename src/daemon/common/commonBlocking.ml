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
  Array.make (Array.length Geoip.country_code_array) false

(* Keep a copy of valid values from !!ip_blocking_countries to
   check if geoip_hit needs really to compute the country code.
   If this list is empty, do not call GeoIP *)
let country_blocking_string_list_copy = ref []

let ip_set_hit bl ip =
  match Ip_set.match_blocking_range bl ip with
    | None -> None
    | Some br -> Some br.Ip_set.blocking_description

let geoip_hit cbl ip cc =
  let index =
    match cc with
    | None -> Geoip.get_country_code ip
    | Some cc -> if cc = 0 then Geoip.get_country_code ip else cc
  in
  if not (Geoip.active ()) || !country_blocking_string_list_copy = [] then None
  else if cbl.(index) then
    Some (Printf.sprintf "IPs from country %s are currently blocked"
      Geoip.country_name_array.(index))
  else None

let update_bans () =
  Ip.banned :=
    (fun (ip, cc) ->
      if Ip.local_ip ip then None else
      let block = ip_set_hit !web_ip_blocking_list ip in
      if block <> None then block else
      let block = ip_set_hit !ip_blocking_list ip in
      if block <> None then block else
      let block = geoip_hit country_blocking_list ip cc in
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
  if filename <> "" then Geoip.init (Geoip.unpack filename)
  else Geoip.close ();
  update_bans ()

let set_ip_blocking_countries cl =
  let temp_list = ref [] in
  let cl = List.map String.uppercase cl in
  Array.fill country_blocking_list 0 
    (Array.length country_blocking_list) !country_blocking_block;
  List.iter (fun cc ->
    if cc = "UNKNOWN" then 
      begin
        temp_list := cc :: !temp_list;
        country_blocking_list.(0) <- (not !country_blocking_block)
      end
    else
      try
        let index = Hashtbl.find Geoip.country_index cc in
          temp_list := cc :: !temp_list;
          country_blocking_list.(index) <- (not !country_blocking_block)
      with Not_found ->
        lprintf_nl "Country code %s not found" cc
  ) cl;
  country_blocking_string_list_copy := !temp_list;
  update_bans ()

let set_ip_blocking_countries_block v =
  country_blocking_block := v;
  update_bans ()

let _ =
  CommonWeb.add_web_kind "guarding.p2p"
    "IP blocking lists (ipfilter and guardian v2 formats)"
    (fun url filename ->
      web_ip_blocking_list :=
        if filename = "" then
          Ip_set.bl_empty
        else
          Ip_set.load filename;
      update_bans ()
  );
  CommonWeb.add_web_kind "geoip.dat" "IP to country mapping database"
    (fun url filename ->
    Geoip.init (Geoip.unpack filename);
    update_bans ()
  );

  Heap.add_memstat "CommonBlocking" (fun level buf ->
      Printf.bprintf buf "  local ranges: %d\n" 
        (Ip_set.bl_length !ip_blocking_list);
      Printf.bprintf buf "  web ranges: %d\n" 
        (Ip_set.bl_length !web_ip_blocking_list)
   )
