(* The idea:

   Contributors can be neutral, trusted, of suspicious.
   Contributors of different trust kind should not be mixed. 
   "Suspicious contributors of level n" can be used in groups of at 
   most n suspicious contributors.

   When a chunk is validated, all contributors become trusted.
   When a chunk is found broken, all contributors become suspicious, of
   level (number of contributors/2).

   Suspicious of level 0 = banned. *)

open CommonTypes
open DonkeyTypes
open TcpBufferedSocket

let (trusted_ips : (Ip.t, trust) Hashtbl.t) = Hashtbl.create 1023

let ip_trust ip =
  try
    Hashtbl.find trusted_ips ip
  with Not_found -> Trust_neutral

let socket_trust sock =
  ip_trust (peer_ip sock)

let client_trust c =
  match c.client_sock with
      None -> assert false
    | Some s ->
	socket_trust s

let print_trust ip =
  Printf.printf "%s is %s" (Ip.to_string ip)
  (match ip_trust ip with
       Trust_neutral -> "neutral"
     | Trust_trusted -> "trusted"
     | Trust_suspicious 0 -> "banned (corrupted data)"
     | Trust_suspicious s -> Printf.sprintf "suspicious of level %d" s);
  print_newline ()

let set_ip_trust ip t =
  let oldt = ip_trust ip in
  match oldt, t with
      Trust_neutral, Trust_neutral -> ()
    | Trust_neutral, _ -> Hashtbl.add trusted_ips ip t
    | _, Trust_neutral -> Hashtbl.remove trusted_ips ip
    | _, _ -> 
	Hashtbl.remove trusted_ips ip;
	Hashtbl.add trusted_ips ip t

let valid_block_detected b =
  List.iter (fun ip -> 
    set_ip_trust ip Trust_trusted;
    print_trust ip
  ) b.block_contributors

let corrupted_block_detected b =
  if not b.block_unknown_origin then
    match b.block_contributors with
	[] -> () (* uh ? *)
      | l ->
	  let max_neighbors = (List.length l) / 2 in
	  List.iter (fun ip -> 
            set_ip_trust ip (Trust_suspicious max_neighbors);
	    print_trust ip;
	    if max_neighbors = 0 then
	      let message = Printf.sprintf "IP %s BANNED (CORRUPTED DATA)\n" (Ip.to_string ip) in
	      CommonEvent.add_event (Console_message_event message)
	  ) l

(* when forced, allows "less suspicious" contributors to take over a
   stalled chunk completion *)

let allowed_by_trust b c force =
  b.block_contributors = [] || 
  if not force then
  (match b.block_trust, client_trust c with
      Trust_neutral, Trust_neutral | Trust_trusted, Trust_trusted -> true
    | Trust_suspicious bt, Trust_suspicious ct ->
	(List.length b.block_contributors) + 1 <= min bt ct
    | _ -> false)
  else
  b.block_nclients = 0 && (b.block_unknown_origin ||
   match b.block_trust, client_trust c with
      _, Trust_trusted -> true
    | Trust_suspicious _, Trust_neutral -> true
    | Trust_suspicious bt, Trust_suspicious ct ->
	(List.length b.block_contributors) + 1 <= min bt ct
    | _ -> false)

let add_client_trust b c =
  b.block_trust <- (match b.block_trust, client_trust c with
      Trust_suspicious bt, Trust_suspicious ct -> Trust_suspicious (min bt ct)
    | _, ct -> ct)

