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

type ('a, 'b, 'c, 'd) host = {
  host_num : int;
  mutable host_server : 'a option;
  host_addr : 'd;
  host_port : int;
  mutable host_age : int;
  mutable host_connected : int;
  mutable host_requests : ('c * int) list;
  mutable host_kind : 'b;
  mutable host_queues : ('a, 'b, 'c, 'd) host Queues.Queue.t list;
} 
module Make :
  functor
  (M : sig
      type server
      and host_kind
      and request
      and ip
      val requests :
        (request *
          (int *
            (host_kind ->
            (server, host_kind, request, ip) host Queues.Queue.t list)))
        list
      val default_requests : host_kind -> (request * int) list
      val max_hosts : int Options.option_record
    end) ->
  sig
    val workflow :
      (M.server, M.host_kind, M.request, M.ip) host Queues.Queue.t
    val host_queue_add :
      ('a, 'b, 'c, 'd) host Queues.Queue.t ->
      ('a, 'b, 'c, 'd) host -> int -> unit
    val host_queue_take :
      ('a, 'b, 'c, 'd) host Queues.Queue.t -> ('a, 'b, 'c, 'd) host
    val hosts_by_key :
      (M.ip * int, (M.server, M.host_kind, M.request, M.ip) host) Hashtbl.t
    val hosts_counter : int ref
    val new_host :
      M.ip ->
      int -> M.host_kind -> (M.server, M.host_kind, M.request, M.ip) host
    val set_request : ('a, 'b, 'c, 'd) host -> 'c -> unit
    val manage_hosts : unit -> unit
  end
  
  