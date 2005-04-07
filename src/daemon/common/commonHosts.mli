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

type host_kind = Peer | Ultrapeer | IndexServer

type ('a, 'c, 'd) host = {
  host_num : int;
  mutable host_server : 'a option;
  mutable host_on_remove : unit -> unit;
  host_addr : 'd;
  host_port : int;
  mutable host_obsolete : int;
  mutable host_requests : ('c * int) list;
  mutable host_kind : host_kind;
  mutable host_queues : ('a, 'c, 'd) host Queues.Queue.t list;
} 
module Make :
  functor
  (M : sig
      type server
      and request
      and ip
      val requests :
        (request *
          (int *
            (host_kind ->
            (server, request, ip) host Queues.Queue.t list)))
        list
      val default_requests : host_kind -> (request * int) list
      val max_peers : int Options.option_record
      val max_ultrapeers : int Options.option_record
    end) ->
  sig
    val workflow :
      (M.server, M.request, M.ip) host Queues.Queue.t
    val host_queue_add :
      ('a, 'c, 'd) host Queues.Queue.t ->
      ('a, 'c, 'd) host -> int -> unit
    val host_queue_take :
      ('a, 'c, 'd) host Queues.Queue.t -> ('a, 'c, 'd) host
    val hosts_by_key :
      (M.ip * int, (M.server, M.request, M.ip) host) Hashtbl.t
    val new_host :
      M.ip ->
      int -> host_kind -> (M.server, M.request, M.ip) host
    val set_request : ('a, 'c, 'd) host -> 'c -> unit
    val manage_hosts : unit -> unit
    val try_connect : ('a, 'c, 'd) host -> unit
    val connected : ('a, 'c, 'd) host -> unit
  end
  
  
