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

open Options
open GToolbox
  
open Gettext
open CommonGlobals
open CommonTypes
open GuiTypes
open Gui_columns
open Gui_global
  
module M = Gui_messages
module P = Gpattern
module O = Gui_options
module G = Gui_global
module Mi = Gui_misc

let mOk = "Ok"
let mCancel = "Cancel"
  
let input_widget2 ~widget1 ~widget2 ~get_text ~bind_ok ~title message =
  let retour = ref None in
  let window = GWindow.dialog ~title ~modal:true () in
  ignore (window#connect#destroy ~callback: GMain.Main.quit);
  let main_box = window#vbox in
  let hbox_boutons = window#action_area in
  
  let vbox_saisie = GPack.vbox ~packing: main_box#pack () in
  let hbox_saisie = GPack.hbox ~packing: vbox_saisie#pack () in
  
  let wl_invite = GMisc.label
      ~text: message
      ~packing: (hbox_saisie#pack ~padding: 3)
    ()
  in
  
  hbox_saisie#pack widget1 ~padding: 3;
  hbox_saisie#pack widget2 ~padding: 3;
  
  let wb_ok = GButton.button ~label: "OK"
      ~packing: (hbox_boutons#pack ~expand: true ~padding: 3) () in
  wb_ok#grab_default ();
  let wb_cancel = GButton.button ~label: "Cancel"
      ~packing: (hbox_boutons#pack ~expand: true ~padding: 3) () in
  let f_ok () =
    retour := Some (get_text ()) ;
    window#destroy ()
  in
  let f_cancel () = 
    retour := None;
    window#destroy () 
  in
  ignore (wb_ok#connect#clicked f_ok);
  ignore (wb_cancel#connect#clicked f_cancel);
  
  widget1#misc#grab_focus ();
  window#show ();
  GMain.Main.main ();
  
  !retour


  
  
let (!!) = Options.(!!)

let string_color_of_state = Gui_friends.string_color_of_state

let server_key s = s.server_num

let filter_disconnected_servers = ref true

let is_filtered s =
  (!filter_disconnected_servers && (match s.server_state with
        NotConnected _ 
      | NewHost -> true | _ -> false)) ||
  List.memq s.server_network !G.networks_filtered

class box columns users wl_status =
  let titles = List.map Gui_columns.Server.string_of_column !!columns in 
  object (self)
    inherit [server_info] Gpattern.filtered_plist `EXTENDED titles true (fun s -> s.server_num) as pl
      inherit Gui_servers_base.box () as box
    
    val mutable filtered_data = []
    
    val mutable columns = columns
    method set_columns l =
      columns <- l;
      self#set_titles (List.map Gui_columns.Server.string_of_column !!columns);
      self#update
    
    
    
    method column_menu  i = 
      [
        `I ("Autosize", fun _ -> self#wlist#columns_autosize ());
        `I ("Sort", self#resort_column i);
        `I ("Remove Column",
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
        `M ("Add Column After", (
            List.map (fun (c,s) ->
                (`I (s, (fun _ -> 
                        let c1, c2 = List2.cut (i+1) !!columns in
                        columns =:= c1 @ [c] @ c2;
                        self#set_columns columns
                    )))
            ) Gui_columns.Server.column_strings));
        `M ("Add Column Before", (
            List.map (fun (c,s) ->
                (`I (s, (fun _ -> 
                        let c1, c2 = List2.cut i !!columns in
                        columns =:= c1 @ [c] @ c2;
                        self#set_columns columns
                    )))
            ) Gui_columns.Server.column_strings));
      ]
    
    
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
        try List.nth !!columns (abs - 1) 
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
            (Ip.string_of_addr s.server_addr) s.server_port
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
        !!columns
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
    
    method add_server () = 
      
      let text = "Address(ip:port) :" in
      let title = "Add Server by Address" in
      let nets,wcombo = networks_combo false in
      let we_chaine = GEdit.entry ~width: 200 ~text:"" () in
      match
        input_widget2  ~widget1:we_chaine#coerce ~widget2:wcombo#coerce
        ~get_text:(fun () -> wcombo#entry#text, we_chaine#text) ~bind_ok:true
        ~title  text
      with
        None -> ()
      | Some (net, addr) ->
          let net = List.assoc net nets in
          let (ip,port) = String2.cut_at addr ':' in
          let ip = Ip.of_string ip in
          let port = int_of_string port in
          Gui_com.send (GuiProto.AddServer_query (net, ip, port))

    method connect_more_servers () =
      Gui_com.send GuiProto.ConnectMore_query
    
    method remove_old_servers () =
      Gui_com.send GuiProto.CleanOldServers
    
    val mutable delayed_users_update = None 
    
    method on_select s =
      match s.server_users with
        None -> 
          users#clear;
          Gui_com.send (GuiProto.GetServer_users s.server_num);
      |	Some l -> 
          if delayed_users_update = None then
            BasicSocket.add_timer 0.1 (fun _ ->
                match delayed_users_update with
                  Some l ->
                    delayed_users_update <- None;
                    let list = ref [] in
                    List.iter (fun u ->
                        try
                          let user_info = Hashtbl.find G.users u in
                          list := user_info :: !list
                        with _ ->
                            Gui_com.send (GuiProto.GetUser_info u);
                    ) l;
                    users#reset_data !list
                | _ -> ()
            );
          delayed_users_update <- Some l

(*
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
*)
    
    method on_deselect s = users#clear
    
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
        [ `I ((gettext M.connect_more_servers_text), self#connect_more_servers) ;
        `I ((gettext M.remove_old_servers_text), self#remove_old_servers)
      ]	
    
    method set_tb_style st = 
      if Options.(!!) Gui_options.mini_toolbars then
        (wtool1#misc#hide (); wtool2#misc#show ()) else
        (wtool2#misc#hide (); wtool1#misc#show ());
      wtool1#set_style st;
      wtool2#set_style st;

(** {2 Handling core messages} *)
    
    method filter = is_filtered
    
    method update_server s s_new row =
      s.server_score <- s_new.server_score ;
      s.server_tags <- s_new.server_tags ;
      s.server_nusers <- s_new.server_nusers ;
      s.server_nfiles <- s_new.server_nfiles ;
      s.server_state <- s_new.server_state ;
      s.server_name <- s_new.server_name ;
      s.server_description <- s_new.server_description ;
      if s.server_state = RemovedHost then 
        self#remove_item row s
      else begin
          self#refresh_item row s
        end;
      if !G.nservers <> self#size then begin
          G.nservers := self#size;
          self#update_wl_status
        end;
      
      (if s.server_users != s_new.server_users then begin
            s.server_users <- s_new.server_users ;
            List.iter (fun ss ->
                if s == ss then
                  self#on_select s
            ) self#selection;
          end
      )
    
    
    method find_server num = self#find num
    
    method h_server_info s = 
      try
        let (row, serv) = self#find_server s.server_num in
        (
          match Mi.is_connected s.server_state, Mi.is_connected serv.server_state with
            true, false -> incr G.nconnected_servers ; self#update_wl_status
          | false, true -> decr G.nconnected_servers ; self#update_wl_status
          | _ -> ()
        );
        self#update_server serv s row;
      with
        Not_found ->
          if s.server_state <> RemovedHost then
            (
              self#add_item s;
              if !G.nservers <> self#size then begin
                  G.nservers := self#size;
                  self#update_wl_status
                end;
              if Mi.is_connected s.server_state then begin
                  incr G.nconnected_servers;
                  self#update_wl_status
                end
            )
    
    method h_server_filter_networks = self#refresh_filter
    
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
    
    method clean_table list = 
      let data = ref [] in
      List.iter (fun (s_num :int) ->
          try
            let row, s = self#find_server s_num in
            data := s :: !data
          with _ -> ()
      ) list;
      self#reset_data !data
    
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

(*      ignore (wb_add#connect#clicked self#add_server); *)
      
      Gui_misc.insert_buttons wtool1 wtool2 
        ~text: (gettext M.remove)
      ~tooltip: (gettext M.remove)
      ~icon: (M.o_xpm_remove)
      ~callback: self#remove
        ();
      
      Gui_misc.insert_buttons wtool1 wtool2 
        ~text: (gettext M.connect)
      ~tooltip: (gettext M.connect)
      ~icon: (M.o_xpm_connect)
      ~callback: self#connect
        ();
      
      Gui_misc.insert_buttons wtool1 wtool2 
        ~text: (gettext M.disconnect)
      ~tooltip: (gettext M.disconnect)
      ~icon: (M.o_xpm_disconnect)
      ~callback: self#disconnect
        ();
      
      Gui_misc.insert_buttons wtool1 wtool2 
        ~text: (gettext M.view_users)
      ~tooltip: (gettext M.view_users)
      ~icon: (M.o_xpm_view_users)
      ~callback: self#view_users
        ();
      
      Gui_misc.insert_buttons wtool1 wtool2 
        ~text: (gettext M.connect_more_servers_text)
      ~tooltip: (gettext M.connect_more_servers_tips)
      ~icon: (M.o_xpm_connect_more_servers)
      ~callback: self#connect_more_servers
        ();
      
      Gui_misc.insert_buttons wtool1 wtool2 
        ~text: (gettext M.remove_old_servers_text)
      ~tooltip: (gettext M.remove_old_servers_tips)
      ~icon: (M.o_xpm_remove_old_servers)
      ~callback: self#remove_old_servers
        ();
      
      Gui_misc.insert_buttons wtool1 wtool2 
        ~text: (gettext M.toggle_display_all_servers_text)
      ~tooltip: (gettext M.toggle_display_all_servers_tips)
      ~icon: (M.o_xpm_toggle_display_all_servers)
      ~callback: self#toggle_display_all_servers
        ();
      
      Gui_misc.insert_buttons wtool1 wtool2
        ~text: "Add Server"
        ~tooltip: "Add Server by Address"
        ~icon: (M.o_xpm_find_friend)
      ~callback: self#add_server
        ();
      
  end

class pane_servers () =
  let box_users = new Gui_users.box_users () in
  let wl_status = GMisc.label ~text: "" ~show: true () in
  let box_servers = new box O.servers_columns box_users wl_status in
  let box_queries = new Gui_queries.paned () in

  object (self)
    inherit Gui_servers_base.pane ()

    method wl_status = wl_status
    method box_servers = box_servers
    method box_users = box_users
    method hpaned = hpaned_servers

    method set_tb_style st = 
      box_users#set_tb_style st ;
      box_servers#set_tb_style st

    method clear =
      wl_status#set_text "";
      box_servers#clear ;
      box_users#clear ;
      box_queries#clear

    method tab_queries = box_queries
      
    (** {2 Handling core messages} *)

    method h_server_filter_networks = box_servers#h_server_filter_networks
    method h_server_info = box_servers#h_server_info
    method h_server_state = box_servers#h_server_state
    method h_server_busy = box_servers#h_server_busy
    method h_server_user = box_servers#h_server_user

    method clean_table servers = box_servers#clean_table servers
      
    initializer
      servers_frame#add box_servers#coerce;
      users_frame#add box_users#coerce;
      queries_frame#add box_queries#coerce;
  end

