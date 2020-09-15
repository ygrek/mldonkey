(* Copyright 2001, 2002 pango *)
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

(* The idea:

   Contributors can be neutral, reliable, of suspicious.
   Contributors of different reliability should not be mixed, as much
   as possible.
   Suspicious contributors of level (n) can be used in groups that contain
   at most n non reliable contributors.

   When a chunk is validated, all contributors become reliable.
   When a chunk is found broken, all contributors become suspicious, of
   level (number of contributors/2).

   Suspicious of level 0 = banned.


Let's suppose a chunk has been downloaded using n neutral contributors,
      r reliable contributors and s suspicious contributors;

If the chunk is validated =>
   all contributors become reliable

If the chunk is broken =>
   if there's non reliable contributors (n+s>0), remove reliable contributors
   from the list (we trust reliable contributors to be really reliable)
   all (remaining) contributors become suspicious "number of contributors/2"

From that, we see than it's acceptable to sometimes mix reliable contributors
with others and still detect suspects (but the chunk may be unvalidated,
resulting in a poor use of reliable contributors).

Neutrals can be mixed with suspicious, but should be accounted for when
checking contributors list limit of suspicious members, so that dichotomia
works. But this should be avoided in the general case, if we believe that
most neutrals are in fact reliable (otherwise p2p networks don't work).
While keeping them separate, the probability that the chunk is validated
and that they all become reliable stays high.

level 1:
reliables go with reliables
neutrals go with neutrals
suspicious go with suspicious

level 2:
reliables go with reliables and neutrals
neutrals go with neutrals
suspicious go with suspicious

level 3:
reliables go with all
neutrals go with neutrals
suspicious go with suspicious

level 4:
reliables go with all
neutrals go with neutrals and suspicious
suspicious go with suspicious

level 5 (or max_int):
no level 5.

levels:       past contributor
               r       n        s

incoming   r   1       2        3
contrib    n   5       1        4
           s   5       5        1

with several past contributors kinds, take the max of levels.

Legacy blocks (containing legacy data at the beginning of the session)
should be handled as if it was downloaded by another virtual neutral
contributor.

From several choices, prefer the one of lowest level.
 *)
open Printf2

open CommonTypes
open CommonNetwork
open DonkeyTypes

let (reliable_ips : (Ip.t, reliability) Hashtbl.t) = Hashtbl.create 1023

let ip_reliability ip =
  try
    Hashtbl.find reliable_ips ip
  with Not_found -> Reliability_neutral

      (*
let socket_reliability sock =
  ip_reliability (peer_ip sock)
  *)

let client_reliability c =
  ip_reliability c.client_ip
(*  match c.client_sock with
      None -> ip_reliability c.client_ip
    | Some s ->
        try
          let ip = peer_ip s in
          if ip <> c.client_ip then begin
            lprintf "[WARNING] client_ip doesn't match peer_ip for client %d" (client_num c);
            lprint_newline ()
          end;
          ip_reliability ip
        with _ ->
          lprintf "[WARNING] client disconnected, using saved client_ip";
          lprint_newline ();
          ip_reliability c.client_ip *)


  (*
let block_reliability b =
  let rec aux ips r n s l =
    match ips with
        [] -> (r, n, s, l)
      | ip :: q ->
          match ip_reliability ip with
              Reliability_reliable -> aux q (r + 1) n s l
            | Reliability_neutral -> aux q r (n + 1) s l
            | Reliability_suspicious ipl ->
                aux q r n (s + 1) (min l ipl) in
    aux b.block_contributors 0 0 0 max_int
    *)

let print_reliability ip =
  lprintf "%s is %s\n" (Ip.to_string ip)
  (match ip_reliability ip with
      Reliability_neutral -> "neutral"
    | Reliability_reliable -> "reliable"
    | Reliability_suspicious 0 -> "has sent corrupted data"
    | Reliability_suspicious s -> Printf.sprintf "suspicious of level %d" s)

let bprint_reliability buf ip =
  Printf.bprintf buf "%s is %s\n" (Ip.to_string ip)
  (match ip_reliability ip with
       Reliability_neutral -> "neutral"
     | Reliability_reliable -> "reliable"
     | Reliability_suspicious 0 -> "has sent corrupted data"
     | Reliability_suspicious s -> Printf.sprintf "suspicious of level %d" s)

let bprint_reliability_table buf =
  let sorted_ips = Array.of_list (Hashtbl.fold (fun ip _ l -> ip :: l) reliable_ips []) in
  Array.sort Ip.compare sorted_ips;
  Array.iter (fun ip -> bprint_reliability buf ip) sorted_ips

let set_ip_reliability ip reliability =
  Hashtbl.replace reliable_ips ip reliability

(*
let valid_block_detected b =
  List.iter (fun ip ->
    set_ip_reliability ip Reliability_reliable;
    print_reliability ip
  ) b.block_contributors

let corrupted_block_detected b =
  let r, n, s, l = block_reliability b in
  let n = if b.block_legacy then n + 1 else n in
  let r, contributors = if n + s > 0 then
    0, List.filter (fun ip -> ip_reliability ip <> Reliability_reliable) b.block_contributors
  else r, b.block_contributors in
  let max_neighbors = (r + n + s) / 2 in
          List.iter (fun ip ->
            set_ip_reliability ip (Reliability_suspicious max_neighbors);
            print_reliability ip;
            if max_neighbors = 0 then
              let message = Printf.sprintf "IP %s SHOULD BE BANNED (CORRUPTED DATA)\n" (Ip.to_string ip) in
              CommonEvent.add_event (Console_message_event message)
          ) contributors

let allowed_by_reliability b c =
  let r, n, s, l = block_reliability b in
  let n = if b.block_legacy then n + 1 else n in
  match client_reliability c with
      Reliability_reliable ->
        if s > 0 then 3
        else if n > 0 then 2
        else 1
    | Reliability_neutral ->
        if r > 0 then max_int
        else if n + s + 1 > l then max_int
        else if s > 0 then 4
        else 1
    | Reliability_suspicious crl ->
        if r > 0 || n > 0 then max_int
        else if n + s + 1 > min l crl then max_int
        else 1
          *)

let _ =
  register_commands
    [
      "dump_reliability", "Network/Donkey", Arg_none (fun o ->
        let buf = o.conn_buf in
        bprint_reliability_table buf;
        ""
      ), ":\t\t\tdisplay the reliability of sources";
    ]

open Md4
(* TODO: add timers to remove old entries *)

let (client_hashes : (Ip.t, Md4.t) Hashtbl.t) = Hashtbl.create 1023

let register_client_hash ip hash =
  try
    let old_hash = Hashtbl.find client_hashes ip in
    let hashes_match = (hash = old_hash) in
    if not hashes_match then begin
        lprintf "client hash clash %s: %s/%s\n"
        (Ip.to_string ip) (Md4.to_string old_hash) (Md4.to_string hash);
      Hashtbl.replace client_hashes ip hash;
    end;
    hashes_match
  with Not_found ->
    Hashtbl.add client_hashes ip hash;
    true

module Marshal = struct

    let to_string v _ =
      let v = Marshal.to_string v [] in
(* The header depends on OCaml version. Remove it. *)
      let v = String.sub v (Marshal.header_size)
        (String.length v - Marshal.header_size) in
      v

  end
