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
    type t = {
    mutable name : string;
    mutable ip : Unix.inet_addr array;
  }

let local_ip = Unix.inet_addr_of_string "127.0.0.1"

let local_host = {
    name = "localhost";
    ip = [| local_ip |]      
  }

let from_name name = {
    name =  name;
    ip = [||];
  }

let from_ints a1 a2 a3 a4 =
  let name = Printf.sprintf "%d.%d.%d.%d" a1 a2 a3 a4 in
  let ip = Unix.inet_addr_of_string name in
  { name = name; ip = [| ip |] }

let from_ip ip =
  { name = Unix.string_of_inet_addr ip; 
    ip = [|ip|];
  }

let name t = t.name
let ip t = 
  if Array.length t.ip = 0 then
    let h = Unix.gethostbyname t.name in
    t.ip <- h.Unix.h_addr_list;
    t.ip.(0)
  else
    t.ip.(0)
