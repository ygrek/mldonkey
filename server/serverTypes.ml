(* Copyright 2002 b8_bavard, b8_fee_carabine, INRIA *)
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

open Unix
open TcpBufferedSocket
open DonkeyMftp
open Options
open Mftp_comm
  
type replies =  {
    mutable docs : Store.index array;
    mutable next_doc : int;
  }

type location = {
    mutable loc_ip : Ip.t;
    mutable loc_port : int;
    mutable loc_expired : float;
  }

  (*
type file = {
    mutable file_names : string list;
    file_md4 : Md4.t;
    file_size : int32;
    mutable file_avail : int; 
    file_tags : tag list;
    mutable file_clients : client list;
  }
    *)

and client = {
    mutable client_id : Ip.t; 
    mutable client_conn_ip : Ip.t;
    mutable client_md4 : Md4.t;
    mutable client_sock: Mftp_comm.server_sock option;
    mutable client_kind : client_kind;
    mutable client_files : Md4.t list;
    mutable client_tags: CommonTypes.tag list;
    mutable client_location : location;
    mutable client_results : replies;
  }

and client_kind =
  Firewalled_client
| KnownLocation of Ip.t * int
  
type server = {
    server_ip : Ip.t;
    server_port : int;
}

type peer = {
    mutable peer_ip : Ip.t;
    mutable peer_port : int;
    mutable peer_sock : TcpBufferedSocket.t option;
    mutable peer_tags : CommonTypes.tag list; 
  }
  
open CommonNetwork
  
let network = CommonNetwork.new_network "Donkey:server"


(*

Il ne faut pas utiliser les memes noms de label dans deux records definis
dans le meme module (client_md4 est aussi defini au dessus dans client.
Il faut choisir d'autres noms ou placer le type dans un autre module (ou
  un sous module).
  
type stand_by_log =
 {  
    client_md4 : Md4.t; 
    client_ip : Ip.t;
    time : float;
    msg : Mftp_server.t;
    reply : Mftp_server.t;
 } 
    *)

(*
type kLocation = {
    mutable ip : Ip.t;
    mutable port : int;
}  
 
type fLocation = {
    mutable ip_s : Ip.t;
    mutable port_s : int;
    mutable id_client : Ip.t;
    }
  
and global = 
     Firewalled_location of fLocation 
   | Knowed_location of kLocation

type where = {
        mutable loc : global;
        mutable expired : float;
}
*)
