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
open Gui_columns
  
module M = Gui_messages
module P = Gpattern
module O = Gui_options

let (!!) = Options.(!!)
(* [ M.kind ; M.name ; ] *)
class box columns () =
  let titles = List.map Gui_columns.string_of_user_column columns in 
  object (self)
    inherit [Gui_proto.user_info] Gpattern.plist `EXTENDED titles true as pl
      inherit Gui_users_base.box () as box
    
    val mutable columns = columns
    method set_columns l =
      columns <- l;
      self#set_titles (List.map Gui_columns.string_of_user_column columns);
      self#update
    
    
    method compare_by_col col f1 f2 =
      match col with
        Col_user_name -> compare f1.user_name f2.user_name
      | Col_user_kind -> compare (Ip.valid f1.user_ip) (Ip.valid f2.user_ip)
      | Col_user_tags -> 0
          
          
    method compare f1 f2 =
      let abs = if current_sort >= 0 then current_sort else - current_sort in
      let col = 
	try List.nth columns (abs - 1) 
	with _ -> Col_user_name
      in
      let res = self#compare_by_col col f1 f2 in
      res * current_sort

    method content_by_col f col =
      match col with
        Col_user_name -> f.user_name
      | Col_user_kind -> 
          if Ip.valid f.user_ip then "Direct" else ""
      | Col_user_tags -> CommonGlobals.string_of_tags f.user_tags

    method content f =
      let strings = List.map 
	  (fun col -> P.String (self#content_by_col f col))
	  columns 
      in
      let col_opt = Some `BLACK      in
      (strings, col_opt)

    (*
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
*)
    
    method add_to_friends () =
      List.iter 
	(fun u -> Gui_com.send (Gui_proto.AddUserFriend u.user_num))
	self#selection
    
    method browse_files () =
      List.iter 
        (fun u -> Gui_com.send (Gui_proto.BrowseUser u.user_num))
      self#selection
      
    method menu =
      match self#selection with
	[] -> []
      |	_ -> [ 
            `I (M.add_to_friends, self#add_to_friends);
            `I (M.browse_files, self#browse_files)
          ]

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
    inherit box !!O.users_columns ()

    initializer
      ()
  end
