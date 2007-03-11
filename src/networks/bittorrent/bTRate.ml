(* Copyright 2003, Denis Fortin
   Heavily based on file CurrentRateMeasure.py
   from BitTorrent 3.3 by Bram Cohen
 *)
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
open BasicSocket

module Rate = struct

type t = {
  mutable ratesince : int;
  mutable lasttime : int;
  mutable rate : int;
}

let update ?(amount=0) r  =
  let t =  last_time () in
    try 
      r.rate <- (r.rate * (r.lasttime - r.ratesince) + amount) / (t - r.ratesince)
    with Division_by_zero ->
      r.rate <- 0;
    r.lasttime <- t;
    if r.ratesince < (t - 20) then
      r.ratesince <- (t - 20)

let update_no_change r =
  let t = last_time () in
    r.ratesince <- t;
    r.lasttime <- t

let ratesince r =
  r.ratesince

let new_rate () =
  let t = last_time() in
  {
    ratesince = t - 1;
    lasttime = t;
    rate = 0;
  }

let get_rate r =
  r.rate

let compare r1 r2 =
  compare r1.rate r2.rate

end
