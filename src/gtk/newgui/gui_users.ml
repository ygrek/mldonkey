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

open Options
open Gettext
open CommonTypes
open GuiTypes
open Gui_columns
  
module M = Gui_messages
module P = Gpattern
module O = Gui_options

let (!!) = Options.(!!)


class box columns () =
  let titles = List.map Gui_columns.User.string_of_column !!columns in 
  object (self)
    inherit [GuiTypes.user_info] Gpattern.plist `EXTENDED titles true (fun s -> s.user_num) as pl
      inherit Gui_users_base.box () as box
    
    val mutable columns = columns

    method set_list_bg bg font =
      let wlist = self#wlist in
      let style = wlist#misc#style#copy in
      style#set_base [ (`NORMAL, bg)];
      style#set_font font;
      wlist#misc#set_style style;
      wlist#set_row_height 18;
      wlist#columns_autosize ()

    method set_columns l =
      columns <- l;
      self#set_titles (List.map Gui_columns.User.string_of_column !!columns);
      self#update;
      self#set_list_bg (`NAME !!O.color_list_bg) (Gdk.Font.load_fontset !!O.font_list)
    
    method column_menu  i = 
      [
        `I (M.mAutosize, fun _ -> self#wlist#columns_autosize ());
        `I (M.mSort, self#resort_column i);
        `I (M.mRemove_column,
          (fun _ -> 
              match !!columns with
                _ :: _ :: _ ->
                  (let l = !!columns in
                    match List2.cut i l with
                      l1, _ :: l2 ->
                        columns =:= l1 @ l2;
                        self#set_columns columns
                    | _ -> ())
              | _ -> ()
          )
        );
        `M (M.mAdd_column_after, (
            List.map (fun (c,s,_) ->
                (`I (s, (fun _ -> 
                        let c1, c2 = List2.cut (i+1) !!columns in
                        columns =:= c1 @ [c] @ c2;
                        self#set_columns columns
                    )))
            ) Gui_columns.User.column_strings));
        `M (M.mAdd_column_before, (
            List.map (fun (c,s,_) ->
                (`I (s, (fun _ -> 
                        let c1, c2 = List2.cut i !!columns in
                        columns =:= c1 @ [c] @ c2;
                        self#set_columns columns
                    )))
            ) Gui_columns.User.column_strings));
      ]
    
    
    method compare_by_col col f1 f2 =
      match col with
        Col_user_name -> compare f1.user_name f2.user_name
      | Col_user_kind -> compare (Ip.valid f1.user_ip) (Ip.valid f2.user_ip)
      | Col_user_tags -> 0
          
          
    method compare f1 f2 =
      let abs = if current_sort >= 0 then current_sort else - current_sort in
      let col = 
        try List.nth !!columns (abs - 1) 
        with _ -> Col_user_name
      in
      let res = self#compare_by_col col f1 f2 in
      res * current_sort

    method content_by_col f col =
      match col with
        Col_user_name -> f.user_name
      | Col_user_kind -> 
          if Ip.valid f.user_ip then (M.rT_tx_direct) else ""
      | Col_user_tags -> CommonGlobals.string_of_tags f.user_tags

    method content f =
      let strings = List.map 
          (fun col -> P.String (self#content_by_col f col))
          !!columns 
      in
      let col_opt = Some `BLACK      in
      (strings, col_opt)
    
    method add_to_friends () =
      List.iter 
        (fun u -> Gui_com.send (GuiProto.AddUserFriend u.user_num))
        self#selection
    
    method browse_files () =
      List.iter 
        (fun u -> Gui_com.send (GuiProto.BrowseUser u.user_num))
      self#selection
      
    method menu =
      match self#selection with
        [] -> []
      |	_ -> [ 
            `I (M.rT_me_add_to_friends, self#add_to_friends);
            `I (M.rT_me_browse_files, self#browse_files)
          ]

    method set_tb_style tb = 
        if Options.(!!) Gui_options.mini_toolbars then
          (wtool1#misc#hide (); wtool2#misc#show ()) else
          (wtool2#misc#hide (); wtool1#misc#show ());
      wtool1#set_style tb;
      wtool2#set_style tb

    method find_user num = self#find num

    initializer
      box#vbox#pack ~expand: true pl#box ;
      (*
      Gui_misc.insert_buttons wtool1 wtool2
           ~text: (gettext M.add_to_friends)
           ~tooltip: (gettext M.add_to_friends)
           ~icon: (M.o_xpm_add_to_friends)
           ~callback: self#add_to_friends ()
           *)
  end

class box_users () =
  object (self)
    inherit box O.users_columns ()

    initializer
    
      let style = evbox1#misc#style#copy in
      style#set_bg [ (`NORMAL, (`NAME "#494949"))];
      evbox1#misc#set_style style;
      let style = label_users#misc#style#copy in
      style#set_fg [ (`NORMAL, `WHITE)];
      label_users#misc#set_style style


  end
