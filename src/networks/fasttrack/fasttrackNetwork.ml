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

open Md4

open CommonTypes


(* any = 0 *)
let name_of_tag =
  [
    Field_KNOWN "any", 0;
    Field_KNOWN "year", 1;
    Field_Filename, 2;
    Field_Uid, 3;
    Field_Title, 4;
    Field_KNOWN "time", 5;
    Field_Artist, 6;
    Field_Album, 8;
    Field_KNOWN "language", 0x0A;
    Field_KNOWN "keywords", 0x0C;
    Field_KNOWN "resolution", 0x0D;
    Field_KNOWN "genre", 0x0E;
    Field_KNOWN "OS", 0x10;
    Field_KNOWN "bitdepth", 0x11;
    Field_Type, 0x12;
    Field_KNOWN "quality", 0x15;
    Field_KNOWN "version", 0x18;
    Field_KNOWN "comment", 0x1A;
    Field_Codec, 0x1C; (* "divx" *)
    Field_KNOWN "rating", 0x1D;
    Field_Size, 0x21;
    Field_Type, 0x22; (* "movie", "video clip",... *)
    Field_KNOWN "49", 49;
    Field_KNOWN "53", 53;
  ]

type cipher

type ciphers = {
    in_cipher : cipher;
    out_cipher : cipher;

    mutable in_xinu : int64;
    mutable out_xinu : int64;
  }

type query_key = unit

type query_operator =
| Equals
| Approx
| AtMost
| AtLeast
| Substring

type query_term = query_operator * tag

type search_extension =  string * query_term list
type search_uid = int
type file_uid = Md5Ext.t
type file_uri = string

external create_cipher : unit -> cipher = "ml_create_cipher"
external apply_cipher : cipher -> bytes -> int -> int -> unit
  = "ml_apply_cipher"
external init_cipher : cipher -> unit = "ml_init_cipher"
external set_cipher : cipher -> int32 -> int -> unit = "ml_set_cipher"
external get_cipher_from_packet : string -> int -> cipher -> unit
  = "ml_cipher_packet_get"
external xor_ciphers : cipher -> cipher -> unit = "ml_xor_ciphers"
external xor_ciphers2 : cipher -> cipher -> unit = "ml_xor_ciphers2"

external cipher_packet_set : cipher -> bytes -> int -> unit
  = "ml_cipher_packet_set"
external cipher_packet_set_xored : cipher -> string -> int -> cipher -> unit
  = "ml_cipher_packet_set_xored"
external cipher_free : cipher -> unit = "ml_cipher_free"
external cipher_enc_type : cipher -> int = "ml_cipher_enc_type"

let network_name = "KaZaA"

let port = 1214
let config_file = "fasttrack.ini"
let options_prefix = "FT-"
let max_known_peers_default = 20

(* Useless *)
let redirectors =   []
let has_accept = false
let accept_header = ""

let accept_ed2kuid = false
let accept_bitprint = false
let accept_md5ext = true

  let max_queued_ranges = 1

(* This is the typical reply of a busy FT client.
ascii:[
HTTP/1.0 503 Service Unavailable
Retry-After: 300
X-Kazaa-Username: K++_www.kazaaKPP.com
X-Kazaa-Network: KaZaA
X-Kazaa-IP: 80.56.???.???:3223
X-Kazaa-SupernodeIP: 80.57.???.???:1070
*)

let value_to_index _ = ()
let index_to_value _ = []
