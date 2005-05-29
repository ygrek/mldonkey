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

let port = 6347
let config_file = "gnutella2.ini"
let redirectors = [
    "http://dlaikar.de/cgi-bin/gcache2-cgi/gcache.cgi";
    "http://fast.papajema.com/cgi-bin/gcache.cgi";
    "http://gwc.fspn.cryptnet.net/gcache.cgi";
    "http://gwc.gnewsgroups.com/cgi-bin/gcache.cgi";
    "http://gwc.mamarazzi.net/";
    "http://gwebcache.jonatkins.com/cgi-bin/gwebcache.cgi";
    "http://g2cache.theg2.net/gwcache/lynnx.asp";
    "http://user1.7host.com/dgwc2/lynnx.asp";
    "http://www20.brinkster.com/dgc2/lynnx.asp";
    "http://ptzldd1.ath.cx/perlgcache.cgi";
    "http://www25.brinkster.com/dcache/dcache.asp";
    "http://gwebcache4.jonatkins.com/cgi-bin/perlgcache.cgi";
    "http://gwebcache2.jonatkins.com/cgi-bin/gwebcache.cgi";
    "http://gwebcache5.jonatkins.com/cgi-bin/perlgcache.cgi";
  ]

let options_prefix = "G2-"
let has_accept = true
let accept_header = "application/x-gnutella2"
let max_known_peers_default = 20

(* All false, so that there is only one network per uri type
   until an better url and result handler is in place and
   the thing with non-primary downloads works ( gives an
   assertion failure if we add an ed2k file in
   CommonSwarming2 at line 609 "assert (not tt.t_primary);"
   ) *)
let accept_ed2kuid = false
let accept_bitprint = false
let accept_md5ext = false
  
  
let max_queued_ranges = 6
  
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
  
