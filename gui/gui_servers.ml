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

(** GUI for the list of servers. *)

open Gettext
open CommonGlobals
open CommonTypes
open GuiTypes
open Gui_columns

module M = Gui_messages
module P = Gpattern
module O = Gui_options
module G = Gui_global
module Mi = Gui_misc

let (!!) = Options.(!!)

let string_color_of_state = Gui_friends.string_color_of_state

let server_key s = s.server_num

let filter_disconnected_servers = ref true

let is_filtered s =
  (!filter_disconnected_servers && s.server_state = NotConnected) ||
  List.memq s.server_network !G.networks_filtered

class box columns users wl_status =
  let titles = List.map Gui_columns.Server.string_of_column columns in 
  object (self)
    inherit [server_info] Gpattern.plist `EXTENDED titles true as pl
      inherit Gui_servers_base.box () as box
    
    val mutable filtered_data = []
    
    val mutable columns = columns
    method set_columns l =
      columns <- l;
      self#set_titles (List.map Gui_columns.Server.string_of_column columns);
      self#update
    
    method box = box#coerce
    
    method compare_by_col col s1 s2 =
      match col with
        Col_server_address -> compare s1.server_addr s2.server_addr
      |	Col_server_state -> compare s1.server_state s2.server_state
      |	Col_server_users -> compare s1.server_nusers s2.server_nusers
      |	Col_server_files -> compare s1.server_nfiles s2.server_nfiles
      |	Col_server_desc -> compare s1.server_description s2.server_description
      | Col_server_network -> compare s1.server_network s2.server_network
      | Col_server_name -> compare s1.server_name s2.server_name
    
    method compare s1 s2 =
      let abs = if current_sort >= 0 then current_sort else - current_sort in
      let col = 
        try List.nth columns (abs - 1) 
        with _ -> Col_server_address
      in
      let res = self#compare_by_col col s1 s2 in
      current_sort * res
    
    method update_wl_status =
      wl_status#set_text
        (Printf.sprintf !!Gui_messages.connected_to_servers 
	   !G.nconnected_servers !G.nservers)

    method content_by_col s col =
      match col with
        Col_server_address -> 
          Printf.sprintf "%16s : %-5d"
            (string_of_addr s.server_addr) s.server_port
      |	Col_server_state -> fst (string_color_of_state s.server_state)
      |	Col_server_users ->
          if s.server_nusers = 0 then "" 
          else Printf.sprintf "%5d" s.server_nusers
      |	Col_server_files ->
          if s.server_nfiles = 0 then "" 
          else Printf.sprintf "%7d"  s.server_nfiles
      |	Col_server_desc ->
          if s.server_name = "" then "" 
          else Printf.sprintf "%s [%s]" s.server_name s.server_description
      | Col_server_network ->
          Gui_global.network_name s.server_network
      | Col_server_name -> String2.shorten 30 s.server_name
    
    method content s =
      let strings = List.map 
          (fun col -> P.String (self#content_by_col s col))
        columns
      in
      let col_opt = 
        match snd (string_color_of_state s.server_state) with
          None -> Some `BLACK
        | Some c -> Some (`NAME c)
      in
      (strings, col_opt)
    
    method remove () =
      List.iter
        (fun s -> Gui_com.send (GuiProto.RemoveServer_query (s.server_num)))
      self#selection
    
    method connect () =
      List.iter
        (fun s -> Gui_com.send (GuiProto.ConnectServer s.server_num))
      self#selection
    
    method disconnect () =
      List.iter
        (fun s -> Gui_com.send (GuiProto.DisconnectServer s.server_num))
      self#selection
    
    method view_users () =
      List.iter
        (fun s -> Gui_com.send (GuiProto.ViewUsers s.server_num))
      self#selection
    
    method connect_more_servers () =
      Gui_com.send GuiProto.ConnectMore_query
    
    method remove_old_servers () =
      Gui_com.send GuiProto.CleanOldServers
    
    method on_select s =
      match s.server_users with
        None -> 
          users#update_data [] ;
          Gui_com.send (GuiProto.GetServer_users s.server_num);
      |	Some l -> 
          let list = ref [] in
          List.iter (fun u ->
              try
                let user_info = Hashtbl.find G.users u in
                list := user_info :: !list
              with _ ->
                  Gui_com.send (GuiProto.GetUser_info u);
          ) l;
          users#update_data !list
    
    method add_server () =
      let (server_ip, server_port) =
        let server = we_ip#text in
        try
          let pos = String.index server ':' in
          String.sub server 0 pos, String.sub server (pos+1) (
            String.length server - pos - 1)
        with _ ->
            server, we_port#text
      in
      Gui_com.send 
        (GuiProto.Url (
          Printf.sprintf "ed2k://|server|%s|%s||"
            server_ip
            server_port)
      );
      we_ip#set_text "";
      we_port#set_text ""
    
    method on_deselect s = 
      users#update_data []
    
    method menu =
      (match self#selection with
          [] -> []
        |	_ ->
            [ `I ((gettext M.connect), self#connect) ;
              `I ((gettext M.disconnect), self#disconnect) ;
              `I ((gettext M.view_users), self#view_users) ;
              `I ((gettext M.remove), self#remove) ;
              `S ]
      ) @
        [ `I ((gettext M.connect_more_servers), self#connect_more_servers) ;
        `I ((gettext M.remove_old_servers), self#remove_old_servers)
      ]	
    
    method set_tb_style = wtool#set_style

    method clear = self#update_data []

(** {2 Handling core messages} *)
    
    method update_server s s_new row =
      s.server_score <- s_new.server_score ;
      s.server_tags <- s_new.server_tags ;
      s.server_nusers <- s_new.server_nusers ;
      s.server_nfiles <- s_new.server_nfiles ;
      s.server_state <- s_new.server_state ;
      s.server_name <- s_new.server_name ;
      s.server_description <- s_new.server_description ;
      if s.server_state = RemovedHost then
        if row >= 0 then 
          (
           data <- List.filter (fun s2 -> s2 <> s) data;
           self#wlist#remove row;
           decr G.nservers;
	   self#update_wl_status
          )
        else
	  (
           filtered_data <- List.filter (fun s2 -> s2 <> s) filtered_data;
           decr G.nservers;
	   self#update_wl_status
          )
      else 
      if row >= 0 then
        self#update_row s row;
      
      (if s.server_users != s_new.server_users then begin
            s.server_users <- s_new.server_users ;
            List.iter (fun ss ->
                if s == ss then
                  self#on_select s
            ) self#selection;
          end
      )
    
    
    method find_server num =
      let rec iter n l =
        match l with
          [] -> 
            if n >= 0 then
              iter min_int filtered_data
            else raise Not_found
        | s :: q ->
            if s.server_num = num then
              (n, s)
            else
              iter (n+1) q
      in
      iter 0 data
    
    method h_server_info s = 
      try
        let (row, serv) = self#find_server s.server_num in
        (
          match Mi.is_connected s.server_state, Mi.is_connected serv.server_state with
            true, false -> incr G.nconnected_servers ; self#update_wl_status
          | false, true -> decr G.nconnected_servers ; self#update_wl_status
          | _ -> ()
        );
        let row = 
          if is_filtered s <> (row < 0) then 
            if row < 0 then begin
(* serv was filtered, but not s *)
                data  <- data @ [serv];
                filtered_data <- List2.removeq serv filtered_data;
                let row = self#wlist#rows    in
                self#insert ~row: row s;
                row
              end
            else begin
                filtered_data  <- filtered_data @ [serv];
                data <- List2.removeq serv data;
                self#wlist#remove row;
                -1
              end else row
        in
        self#update_server serv s row;
      with
        Not_found ->
          if s.server_state <> RemovedHost then
            (
              if is_filtered s then 
		begin
                  filtered_data <- filtered_data @ [s];
                  incr G.nservers ;       
		  self#update_wl_status
                end 
	      else 
		begin
                  data <- data @ [s];
                  incr G.nservers;
                  self#insert ~row: self#wlist#rows s;
                  if Mi.is_connected s.server_state then
                    incr G.nconnected_servers;
		  self#update_wl_status
                end
            )
          else
            ()
    
    method h_server_filter_networks =
      let data = data @ filtered_data in
      let rec iter filtered not_filtered data =
        match data with
          [] -> List.rev filtered, List.rev not_filtered
        | s :: tail ->
            if is_filtered s then
              iter (s :: filtered) not_filtered tail
            else
              iter filtered (s :: not_filtered) tail
      in
      let (filtered, not_filtered) = iter [] [] data in
      filtered_data <- filtered;
      self#update_data not_filtered
    
    method toggle_display_all_servers () =
      filter_disconnected_servers := not !filter_disconnected_servers;
      self#h_server_filter_networks
    
    method h_server_state num state =
      try
        let (row, serv) = self#find_server num in
        self#h_server_info { serv with server_state = state }
        
      with
        Not_found -> Gui_com.send (GuiProto.GetServer_info num)
          
    
    method h_server_busy num nusers nfiles =
      try
        let (row, serv) = self#find_server num in
        self#update_server
          serv 
          { serv with 
          server_nusers = nusers ;
          server_nfiles = nfiles ;
        } 
          row
      with
        Not_found -> ()
    
    method h_server_user num user_num =
      try
        let (row, serv) = self#find_server num in
        let users = match serv.server_users with
            None -> []
          | Some users -> users in
        
        if not (List.memq user_num users) then
          self#update_server
            serv { serv with server_users = Some (user_num :: users) } row
      with
        Not_found ->
          if num <> 0 then begin
              Gui_com.send (GuiProto.GetServer_info num);
              Gui_com.send (GuiProto.GetServer_users num)
            end
            
    initializer
      box#vbox#pack ~expand: true pl#box ;

      ignore (wb_add#connect#clicked self#add_server);

      ignore
	(wtool#insert_button 
	   ~text: (gettext M.remove)
	   ~tooltip: (gettext M.remove)
	   ~icon: (Gui_icons.pixmap M.o_xpm_remove)#coerce
	   ~callback: self#remove
	   ()
	);

      ignore
	(wtool#insert_button 
	   ~text: (gettext M.connect)
	   ~tooltip: (gettext M.connect)
	   ~icon: (Gui_icons.pixmap M.o_xpm_connect)#coerce
	   ~callback: self#connect
	   ()
	);

      ignore
	(wtool#insert_button 
	   ~text: (gettext M.disconnect)
	   ~tooltip: (gettext M.disconnect)
	   ~icon: (Gui_icons.pixmap M.o_xpm_disconnect)#coerce
	   ~callback: self#disconnect
	   ()
	);

      ignore
	(wtool#insert_button 
	   ~text: (gettext M.view_users)
	   ~tooltip: (gettext M.view_users)
	   ~icon: (Gui_icons.pixmap M.o_xpm_view_users)#coerce
	   ~callback: self#view_users
	   ()
	);

      ignore
	(wtool#insert_button 
	   ~text: (gettext M.connect_more_servers)
	   ~tooltip: (gettext M.connect_more_servers)
	   ~icon: (Gui_icons.pixmap M.o_xpm_connect_more_servers)#coerce
	   ~callback: self#connect_more_servers
	   ()
	);

      ignore
	(wtool#insert_button 
	   ~text: (gettext M.remove_old_servers)
	   ~tooltip: (gettext M.remove_old_servers)
	   ~icon: (Gui_icons.pixmap M.o_xpm_remove_old_servers)#coerce
	   ~callback: self#remove_old_servers
	   ()
	);

      ignore
	(wtool#insert_button 
	   ~text: (gettext M.toggle_display_all_servers)
	   ~tooltip: (gettext M.toggle_display_all_servers)
	   ~icon: (Gui_icons.pixmap M.o_xpm_toggle_display_all_servers)#coerce
	   ~callback: self#toggle_display_all_servers
	   ()
	);
  end

class pane_servers () =
  let users = new Gui_users.box_users () in
  let wl_status = GMisc.label ~text: "" ~show: true () in
  let servers = new box !!O.servers_columns users wl_status in
  object (self)
    inherit Gui_servers_base.paned ()

    method wl_status = wl_status
    method box_servers = servers
    method box_users = users
    method hpaned = wpane

    method set_tb_style st = 
      users#set_tb_style st ;
      servers#set_tb_style st

    method clear =
      wl_status#set_text "";
      servers#clear ;
      users#clear ;

    (** {2 Handling core messages} *)

    method h_server_filter_networks = servers#h_server_filter_networks
    method h_server_info = servers#h_server_info
    method h_server_state = servers#h_server_state
    method h_server_busy = servers#h_server_busy
    method h_server_user = servers#h_server_user
      

    initializer
      wpane#add1 servers#coerce;
      wpane#add2 users#coerce 
  end

