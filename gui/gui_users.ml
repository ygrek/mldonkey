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

(** GUI for the lists of files. *)

open CommonTypes
open Gui_proto

module M = Gui_messages
module P = Gpattern
module O = Gui_options

let (!!) = Options.(!!)

class box () =
  let titles = [ M.kind ; M.name ; ]
  in
  object (self)
    inherit [Gui_proto.user_info] Gpattern.plist `EXTENDED titles true as pl
    inherit Gui_users_base.box () as box

    method compare u1 u2 =
      let res = match current_sort with
      |	1 | -1 -> compare (Ip.valid u1.user_ip) (Ip.valid u2.user_ip)
      |	2 | -2 -> compare u1.user_name u2.user_name
      |	_ -> 0
      in
      res * current_sort

    method content u =
      let s_kind = 
	if Ip.valid u.user_ip then 
          M.direct
	else ""
      in
      let s_name = u.user_name in
      [ P.String s_kind ; P.String s_name ; ], None

    method add_to_friends () =
      List.iter 
	(fun u -> Gui_com.send (Gui_proto.AddUserFriend u.user_num))
	self#selection

    method menu =
      match self#selection with
	[] -> []
      |	_ -> [ `I (M.add_to_friends, self#add_to_friends) ]

    method set_tb_style = wtool#set_style

    initializer
      box#vbox#pack ~expand: true pl#box ;
      ignore
	(wtool#insert_button 
	   ~text: M.add_to_friends
	   ~tooltip: M.add_to_friends
	   ~icon: (Gui_icons.pixmap M.o_xpm_add_to_friends)#coerce
	   ~callback: self#add_to_friends
	   ()
	);
  end

class box_users () =
  object (self)
    inherit box ()

    initializer
      ()
  end
