(* Copyright 2003 pango *)
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
   Detect clients switching to another client_hash, already is use by
   someone else (almost certainly a theft).
*)

open Printf2

open BasicSocket
open CommonTypes
open CommonNetwork
open Md4
open GuiTypes
open TcpBufferedSocket
open DonkeyTypes
open DonkeyGlobals

let (client_hashes : (Ip.t, (Md4.t * int)) Hashtbl.t) = Hashtbl.create 16383
let (hashes_usage : (Md4.t, (int * int)) Hashtbl.t) = Hashtbl.create 16383

let register_client_hash ip hash =
  let usage =
    try
      let old_usage, _ = Hashtbl.find hashes_usage hash in
      old_usage
    with Not_found -> 0 in
  try
    let old_hash, _ = Hashtbl.find client_hashes ip in
    Hashtbl.replace client_hashes ip (hash, last_time ());
    if hash = old_hash then begin
      (* No change, all is fine *)
      if usage = 0 then
	(* should not normally happen *)
	Hashtbl.add hashes_usage hash (1, last_time ())
      else
	(* refresh timestamp *)
	Hashtbl.replace hashes_usage hash (usage, last_time ());
      true 
    end else begin
      Hashtbl.replace hashes_usage hash (usage + 1, last_time ());
      lprintf "client hash change %s: %s -> %s" (Ip.to_string ip) (Md4.to_string old_hash) (Md4.to_string hash);
      lprint_newline ();
      if usage = 1 then 
	(* The new hash is original, that's acceptable *)
	true
      else begin
	lprintf "That hash was already used somewhere else, that's certainly a theft!";
        lprint_newline ();
	false
      end
    end
  with Not_found ->
    (* No hash was known for that IP, that's acceptable.
       We do not check if the hash was already used elsewhere, because it
       could just be a client reconnecting with a new address *)
    Hashtbl.add client_hashes ip (hash, last_time ());
    Hashtbl.replace hashes_usage hash (usage + 1, last_time ());
    true

let clean_thieves () =
  let timelimit = last_time () - 3 * 3600 in
  let obsolete_ips = Hashtbl.fold (fun ip (_, time) l ->
				     if time < timelimit then
				       ip :: l
				     else l) client_hashes [] in
  List.iter (fun ip -> Hashtbl.remove client_hashes ip) obsolete_ips;
  let obsolete_hashes = Hashtbl.fold (fun hash (_, time) l ->
					if time < timelimit then
					  hash :: l
					else l) hashes_usage [] in
  List.iter (fun hash -> Hashtbl.remove hashes_usage hash) obsolete_hashes

module Marshal = struct

    let to_string v _ =
      let v = Marshal.to_string v [] in
(* The header depends on Ocaml version. Remove it. *)
      let v = String.sub v (Marshal.header_size) 
        (String.length v - Marshal.header_size) in
      v

  end  
