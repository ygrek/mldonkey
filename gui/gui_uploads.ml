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

(** The box with uploads info *)

open CommonTypes
open GuiTypes
open GuiProto


module M = Gui_messages
module P = Gpattern
module O = Gui_options

let (!!) = Options.(!!)



class box () =
  object (self)
    inherit [string] Gpattern.plist `SINGLE
	[ M.filename ; M.requests ; M.blocks ]
	true as pl
    inherit Gui_uploads_base.box () as box

    method box = wf_upstats#coerce

    method compare_by_col col t1 t2 = compare t1 t2

    method compare t1 t2 =
      let abs = if current_sort >= 0 then current_sort else - current_sort in
      let res = self#compare_by_col abs t1 t2 in
      current_sort * res

    method content t = ([P.String t], None)
      
    method clear = self#update_data []

    initializer
      wf_upstats#add pl#box;

  end

class upstats_box () =
  let upstats = new box () in
  object (self)
    inherit Gui_uploads_base.upstats_box () as upsb

    method box = upsb#vbox

    method clear = upstats#clear

    initializer
      vbox#pack ~expand: true ~padding: 2 upstats#box
  end
