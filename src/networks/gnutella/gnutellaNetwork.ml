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

open Int64ops
open BasicSocket
open AnyEndian
open Printf2
open Options
open Md4
open TcpBufferedSocket

open CommonGlobals
open CommonTypes
open CommonOptions
open CommonHosts


type cipher
type ciphers

type query_key =
  NoUdpSupport
| GuessSupport
| UdpSupport of Md4.t
| UdpQueryKey of int64

type search_extension = string
type search_uid = Md4.t
type file_uid = Uid.t
  
type file_uri =
  FileByIndex of int * string
| FileByUrl of string
  
let port = 6346
let config_file = "gnutella.ini"
let redirectors =   [
    "public.bearshare.net"; 
    "gnotella.fileflash.com";
    "gnutella-again.hostscache.com";
    "connect1.bearshare.net"; 
    "connect1.gnutellanet.com";
    "gnutella.hostscache.com";
    "connect2.gnutellanet.com";
    "connect3.gnutellanet.com"; 
    "router4.gnutella.com";
  ]
let options_prefix = "GNUT-"
let has_accept = false
let accept_header = "application/x-gnutella-packets"
let max_known_peers_default = 20
  
let accept_ed2kuid = false
let accept_bitprint = true
let accept_md5ext = false

let max_queued_ranges = 1
    
let value_to_index tail =
  match tail with
    [index; name] ->
      FileByIndex (value_to_int index, value_to_string name)
  | _ -> failwith "Bad source"

let index_to_value uri =
  match uri with
    FileByIndex (i,n) -> 
      [ int_to_value i; string_to_value n]
  | FileByUrl s -> 
      [ string_to_value s] 
      
let apply_cipher _ _ _ _ = ()
  