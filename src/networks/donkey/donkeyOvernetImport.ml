(* Copyright 2001, 2002 b8_bavard, b8_fee_carabine, INRIA *)
(* Copyright 2005 beedauchon *)
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
open AnyEndian
open Printf2
open Md4
open LittleEndian

let dump_file filename =
  Unix2.tryopen_read_bin filename (fun ic ->
  let s = Bytes.create 20 in
  try
    lprintf "file: %s\n" filename; 
    let pos = ref 0 in
    while true do 
      let n = input ic s 0 20 in
      lprintf "pos = %d\n" !pos; 
      if n = 0 then raise Exit;
      dump (Bytes.sub_string s 0 n);
      pos := !pos + n;
    done
  with End_of_file -> ())
     
module Peer = struct
    
    type peer = {
        md4 : Md4.t;
        ip : Ip.t;
        port : int;
        kind : int;
      }
    
    type t = peer list

    let get_md4 s pos =
      get_md4 s pos
    
    let get_port s pos =
      get_int16 s pos

    let get_kind s pos =
      get_uint8 s pos
           
    let rec read_peers s pos left =
      if pos + 22 >= String.length s then List.rev left else
      match
        try
          let md4 = get_md4 s pos in
          let ip = get_ip s (pos+16) in
          let port = get_port s (pos+20) in
          let kind = get_kind s (pos+22) in
          (* debug
          lprintf "{ pos = %d " pos;
          lprintf "md4 = %s " (Md4.to_string md4);
          lprintf "ip = %s " (Ip.to_string ip);
          lprintf "port = %d " port;
          lprintf "type = %d}\n" kind;
          *)
          let pos = pos + 23 in
          Some ({
            md4 = md4;
            ip = ip;
            port = port;
            kind = kind;
            }, pos)
        with e -> 
            (*
            let len = String.length s - pos in
            lprintf "Error while reading peer %s (left %d)\n"
              (Printexc2.to_string e) len; 
dump (String.sub s pos len);      
*)
            None
      with
        None -> List.rev left
      | Some (peer, pos) ->
          read_peers s pos (peer :: left)
      
    let read s =
      read_peers s 4  []
(* unused     
    let write buf t =
      buf_int8 buf 14;
      buf_int buf (List.length t);
      List.iter (fun s ->
          buf_ip buf s.ip;
          buf_port buf s.port
      ) t
*)
    let print t =
      lprintf "contact.dat: %d peers found\n" (List.length t); 
      List.iter (fun s ->
          lprintf "  peer %s:%d\n" (Ip.to_string s.ip) s.port;
      ) t;

  end
  
