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
open Unix

let simple_bind port =
  let fds = socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  setsockopt fds Unix.SO_REUSEADDR true;
  bind fds (Unix.ADDR_INET (Unix.inet_addr_any, port));
  listen fds 5;
  fds
  
let simple_server port handler =
  let fds = simple_bind port in
  while true do
    let s = accept fds in
    handler s
  done
    
let channel_relay ic oc =  
  let s = String.create 8192 in
  let rec iter () =
    let nread = input ic s 0 8192 in
    if nread > 0 then begin
        output_string oc (String.sub s 0 nread);
        iter ()
      end
  in
  iter ();
  flush oc
  
let simple_relay in_fd out_fd =  
  let s = String.create 8192 in
  let rec iter_r () =
    let nread = Unix.read in_fd s 0 8192 in
    if nread > 0 then 
      let rec iter_w pos n =
        let nw = Unix.write out_fd s pos n in
        if nw < 1 then raise End_of_file;
        if nw < n then
          iter_w (pos+nw) (n-nw)
        else
          iter_r ()
      in
      iter_w 0 nread 
  in
  iter_r ()
  
let spy_relay f in_fd out_fd =  
  let s = String.create 8192 in
  let rec iter_r () =
    let nread = Unix.read in_fd s 0 8192 in
    if nread > 0 then 
      let rec iter_w pos n =
        let nw = Unix.write out_fd s pos n in
        f s pos nw;
        if nw < 1 then raise End_of_file;
        if nw < n then
          iter_w (pos+nw) (n-nw)
        else
          iter_r ()
      in
      iter_w 0 nread 
  in
  iter_r ()

let simple_connect hostname port =
  let s = socket PF_INET SOCK_STREAM 0 in
  let h = Unix.gethostbyname hostname in
  let addr = h.h_addr_list.(0) in
  try
    Unix.connect s (ADDR_INET(addr,port));
    s
  with e -> close s; raise e

let channel_connect addr port =
  let s = socket PF_INET SOCK_STREAM 0 in
  try
    Unix.connect s (ADDR_INET(addr,port));
    let ic = in_channel_of_descr s in
    let oc = out_channel_of_descr s in
    (ic,oc)
  with e -> close s; raise e
  

  
