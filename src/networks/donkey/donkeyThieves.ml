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

let hash_of_md4 md4 = Hashtbl.hash (Md4.direct_to_string md4)

module Md4HashType =
  struct type t = Md4.t
  let equal = Md4.equal
  let hash = hash_of_md4
end

module Md4_hashtbl = Hashtbl.Make ( Md4HashType )

let (client_hashes : (Ip.t, (Md4.t * int) ref) Hashtbl.t) = Hashtbl.create 16383
let hashes_usage = Md4_hashtbl.create 16383

let register_client_hash ip hash =
  let usageref =
    try
      Md4_hashtbl.find hashes_usage hash
    with Not_found ->
      let newusage = ref (0, 0) in
      Md4_hashtbl.add hashes_usage hash newusage;
      newusage in
  match !usageref with usage, _ ->
  try
    let hashref = Hashtbl.find client_hashes ip in
    match !hashref with old_hash, _ ->
    hashref := (hash, last_time ());
    if Md4.equal hash old_hash then begin
      (* No change, all is fine *)
      (* just refresh timestamp *)
      usageref := (usage, last_time ());
      true 
    end else begin
      usageref := (usage + 1, last_time ());
      lprintf "client hash change %s: %s -> %s" (Ip.to_string ip) (Md4.to_string old_hash) (Md4.to_string hash);
      lprint_newline ();
      if usage = 0 then 
	(* The new hash is original, that's acceptable *)
	true
      else begin
(*	lprintf "That hash was already used somewhere else, that's certainly a theft!";
        lprint_newline (); *)
	false
      end
    end
  with Not_found ->
    (* No hash was known for that IP, that's acceptable.
       We do not check if the hash was already used elsewhere, because it
       could just be a client reconnecting with a new address *)
    Hashtbl.add client_hashes ip (ref (hash, last_time ()));
    usageref := (usage + 1, last_time ());
    true

let clean_thieves () =
  let timelimit = last_time () - 3 * 3600 in
  let obsolete_ips = Hashtbl.fold (fun ip r l ->
				     match !r with _, time ->
				       if time < timelimit then
				         ip :: l
				       else l) client_hashes [] in
  List.iter (fun ip -> Hashtbl.remove client_hashes ip) obsolete_ips;
  let obsolete_hashes = Md4_hashtbl.fold (fun hash r l ->
					    match !r with _, time ->
					      if time < timelimit then
					        hash :: l
					      else l) hashes_usage [] in
  List.iter (fun hash -> Md4_hashtbl.remove hashes_usage hash) obsolete_hashes

module Marshal = struct

    let to_string v _ =
      let v = Marshal.to_string v [] in
(* The header depends on Ocaml version. Remove it. *)
      let v = String.sub v (Marshal.header_size) 
        (String.length v - Marshal.header_size) in
      v

  end  
