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

open CommonOptions
open CommonTypes
open Unix
open TcpBufferedSocket
open DonkeyMftp
open Options
open Mftp_comm

  
let (store : tagged_file Store.t) = 
  Store.create (Filename.concat file_basedir "server_store")
  
module Document = struct
    type t = Store.index
      
    let num t = Store.index t
    let filtered t = Store.get_attrib store t
    let filter t bool = Store.set_attrib store t bool
  end

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
    mutable client_mldonkey : int;
    mutable client_sock: Mftp_comm.server_sock option;
    mutable client_kind : client_kind;
    mutable client_files : Md4.t list;
    mutable client_tags: CommonTypes.tag list;
    mutable client_location : location;
    mutable client_results : replies;
    mutable client_subscriptions : subscription list;
  }

and client_kind =
  Firewalled_client
| KnownLocation of Ip.t * int

  (*
type known_server = {
    known_server_ip : Ip.t;
    known_server_port : int;
    last_message : float;
}
    *)

and subscription = {
    notif_client : client;
    notif_num : int;
    notif_query : Store.index Indexer.query;
    mutable notif_docs : Document.t Intmap.t;
  }

type notification = {
  add : bool;
  md4 : Md4.t;
  source_ip : Ip.t;
  source_port : int;
}

type server = { 
    mutable server_id : int;
    mutable server_group_id : Md4.t;
    mutable server_master : bool;
    mutable server_md4 : Md4.t;
    mutable server_ip : Ip.t;
    mutable server_port : int;
    mutable server_need_recovery : bool;
    mutable server_last_message_send : int * int;
    mutable server_last_message_received_id : int;
    mutable server_last_message_received_time : float;
    mutable server_sock : TcpBufferedSocket.t option;
    mutable server_notifications : notification list;
    mutable server_clients : (Ip.t,Ip.t) Hashtbl.t;
    mutable server_tags : CommonTypes.tag list;
  }
  
open CommonNetwork
  
let network = CommonNetwork.new_network "Donkey:server"

type remote_client = {
    mutable remote_client_local_id : Ip.t; 
    mutable remote_client_server: int;
    mutable remote_client_md4 : Md4.t;
    mutable remote_client_kind : client_kind;
    mutable remote_client_files : Md4.t list;
}



type global_client = 
  LocalClient of client
| RemoteClient of remote_client



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
