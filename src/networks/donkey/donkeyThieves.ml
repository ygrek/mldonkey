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
open CommonOptions
open Md4


let log_prefix = "[eDThieves]"

let lprintf_nl fmt =
  lprintf_nl2 log_prefix fmt

type hash_history = Ip.t * Md4.t * int

(* index by client IP *)
let (client_hashes : (Ip.t, hash_history ref) Hashtbl.t) = Hashtbl.create 1023

(* index by client hash *)
let hash_of_md4 md4 = Hashtbl.hash (Md4.direct_to_string md4)

module Md4HashType =
  struct type t = Md4.t
  let equal = Md4.equal
  let hash = hash_of_md4
end

module Md4_hashtbl = Hashtbl.Make ( Md4HashType )

let hashes_usage = Md4_hashtbl.create 1023

;;
let register_client_hash ip hash =
  let find_by_ip ip =
    try
      Hashtbl.find client_hashes ip
    with Not_found ->
      let new_record = ref (ip, Md4.null, 0) in
      Hashtbl.add client_hashes ip new_record;
      new_record in

  let find_by_hash hash =
    try
      Md4_hashtbl.find hashes_usage hash
    with Not_found ->
      let new_record = ref (Ip.null, hash, 0) in
      Md4_hashtbl.add hashes_usage hash new_record;
      new_record in

  let by_ip = find_by_ip ip in
  let by_hash = find_by_hash hash in
  let new_record = (ip, hash, last_time ()) in
  match !by_ip with
    _, _, 0 ->
      (* no hash was previously known for that IP. We do not check if the
         hash was previously used somewhere else, as it could be legitimate
         (client switching IP address). *)
      by_ip := new_record;
      by_hash := new_record;
      true
  | _, previous_hash, _ ->
      if Md4.equal previous_hash hash then begin
        (* no change, all is fine; just update timestamps *)
        by_ip := new_record;
        by_hash := new_record;
        true
      end else
      (* peer changed in hash, what's happening ? *)
      match !by_hash with
          _, _, 0 ->
            (* that hash is original, all is fine *)
            (* forget old hash *)
            Md4_hashtbl.remove hashes_usage previous_hash;
            by_ip := new_record;
            by_hash := new_record;
            true
        | _, _, _ ->
            (* it switched to a hash that's used somewhere else,
               that's certainly a theft. *)
            if !verbose then
        lprintf_nl "client_md4 %s (ip:%s) was already used somewhere else, that's certainly a theft!"
                (Md4.to_string hash) (Ip.to_string ip);
            false
;;
let clean_thieves () =
  let timelimit = last_time () - 3 * 3600 in
  let obsolete_ips = Hashtbl.fold (fun ip r l ->
                                     match !r with _, _, time ->
                                       if time < timelimit then
                                         ip :: l
                                       else l) client_hashes [] in
  List.iter (fun ip -> Hashtbl.remove client_hashes ip) obsolete_ips;
  let obsolete_hashes = Md4_hashtbl.fold (fun hash r l ->
                                            match !r with _, _, time ->
                                              if time < timelimit then
                                                hash :: l
                                              else l) hashes_usage [] in
  List.iter (fun hash -> Md4_hashtbl.remove hashes_usage hash) obsolete_hashes

module Marshal = struct

    let to_string v _ =
      let v = Marshal.to_string v [] in
(* The header depends on OCaml version. Remove it. *)
      let v = String.sub v (Marshal.header_size)
        (String.length v - Marshal.header_size) in
      v

  end

(* test code *)
(*
let dump () =
  lprintf "Hashes history by IP:\n";
  Hashtbl.iter (fun ip r -> match !r with (ip, md4, time) -> lprintf "%s: %s (%d)\n" (Ip.to_string ip) (Md4.to_string md4) time) client_hashes;
  lprintf "Hashes history by hash:\n";
  Md4_hashtbl.iter (fun md4 r -> match !r with (ip, md4, time) -> lprintf "%s: %s (%d)\n" (Md4.to_string md4) (Ip.to_string ip) time) hashes_usage;
  lprintf "\n"

let testcode () =
  let peer1 = Ip.of_string "1.2.3.4" in
  let peer2 = Ip.of_string "5.6.7.8" in
  let peer3 = Ip.of_string "9.10.11.12" in
  lprintf "Add a peer...\n";
  assert(register_client_hash peer1 (Md4.of_string "11111111111111111111111111111111"));
  dump ();
  lprintf "Another peer...\n";
  assert(register_client_hash peer2 (Md4.of_string "22222222222222222222222222222222"));
  dump ();
  lprintf "First peer changes of hash, ok...\n";
  assert(register_client_hash peer1 (Md4.of_string "33333333333333333333333333333333"));
  dump ();
  lprintf "A third peer appears with the same hash as the first, ok (could be in fact first one switching IP)...\n";
  assert(register_client_hash peer3 (Md4.of_string "33333333333333333333333333333333"));
  dump ();
  lprintf "Second peer takes the hash of the first, wrong!...\n";
  assert(not (register_client_hash peer2 (Md4.of_string "33333333333333333333333333333333")));
  dump ();
  lprintf "Third peer takes a hash that *was* first's hash, ok (?)...\n";
  assert(register_client_hash peer3 (Md4.of_string "11111111111111111111111111111111"));
  dump ();

  exit 2

let _ =
  testcode ()
*)
