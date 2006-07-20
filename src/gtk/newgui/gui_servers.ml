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

open Int64ops
open Options
open GToolbox
  
open Gettext
open CommonGlobals
open CommonTypes
open GuiTypes
open Gui_types
open Gui_columns
open Gui_global
  
module M = Gui_messages
module P = Gpattern
module O = Gui_options
module G = Gui_global
module Mi = Gui_misc

let (!!) = Options.(!!)
  
let input_widget2 ~widget1 ~widget2 ~get_text ~bind_ok ~title message1 message2 =
  let retour = ref None in
  let window = GWindow.dialog ~position:`CENTER ~title ~modal:true () in
  ignore (window#connect#destroy ~callback: GMain.Main.quit);
  let main_box = window#vbox in
  let hbox_boutons = window#action_area in
  
  let vbox_saisie =
    GPack.vbox ~homogeneous:false
      ~spacing:5 ~packing:(main_box#add) () in
  let pixmap = O.gdk_pix M.o_xpm_toggle_display_all_servers in
  ignore (GMisc.pixmap pixmap
    ~packing:(vbox_saisie#pack ~padding:3) ());
  let hbox2 =
    GPack.hbox ~homogeneous:false
      ~spacing:5 ~packing:(vbox_saisie#pack ~expand:false ~fill:false) () in
  let wl_invite2 = GMisc.label
      ~justify:`LEFT
      ~line_wrap:true
      ~text: message2
      ~packing:(hbox2#pack ~expand:false ~fill:true ~padding: 3)
    ()
  in
  vbox_saisie#pack widget2 ~padding: 3;
  let hbox1 =
    GPack.hbox ~homogeneous:false
      ~spacing:5 ~packing:(vbox_saisie#pack ~expand:false ~fill:false) () in
  let wl_invite = GMisc.label
      ~justify:`LEFT
      ~line_wrap:true
      ~text: message1
      ~packing:(hbox1#pack ~expand:false ~fill:true ~padding: 3)
    ()
  in
  vbox_saisie#pack widget1 ~padding: 3;
  let wb_ok = GButton.button ~label:(M.pW_lb_ok)
      ~packing: (hbox_boutons#pack ~expand: true ~padding: 3) () in
  wb_ok#grab_default ();
  let wb_cancel = GButton.button ~label:(M.pW_lb_cancel)
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

let filter_disconnected_servers = ref true

let is_filtered s =
  (!filter_disconnected_servers && (match s.gserver_state with
        NotConnected _ 
      | NewHost -> true | _ -> false)) ||
  List.memq s.gserver_network !G.networks_filtered

let state_pixmap state =
    match state with
        Connected_downloading _ -> O.gdk_pix M.o_xpm_downloading
      | Connected (-1) -> O.gdk_pix M.o_xpm_server_c_low
      | Connected (-2) -> O.gdk_pix M.o_xpm_server_c_high
      | Connecting  -> O.gdk_pix M.o_xpm_server_ci
      | NewHost -> O.gdk_pix M.o_xpm_server_nc
      | Connected_initiating -> O.gdk_pix M.o_xpm_server_ci
      | Connected 0 -> O.gdk_pix M.o_xpm_server_c_low
      | Connected n -> O.gdk_pix M.o_xpm_server_c_low
      | NotConnected (_,n) -> O.gdk_pix M.o_xpm_server_nc
      | RemovedHost -> O.gdk_pix M.o_xpm_removedhost
      | BlackListedHost -> O.gdk_pix M.o_xpm_blacklistedhost

let get_pix state =
  let pix = state_pixmap state in
  pix


class box columns users wl_status =
  let titles = List.map Gui_columns.Server.string_of_column !!columns in 
  object (self)
    inherit [gui_server_info] Gpattern.filtered_plist `EXTENDED titles true (fun s -> s.gserver_num) as pl
      inherit Gui_servers_base.box () as box
    
    val mutable filtered_data = []
    
    val mutable columns = columns

    val mutable icons_are_used = (!!O.use_icons : bool)

    method set_list_bg bg font =
      let wlist = self#wlist in
      let style = wlist#misc#style#copy in
      style#set_base [ (`NORMAL, bg)];
      style#set_font font;
      wlist#misc#set_style style;
      wlist#set_row_height 18; (* we need to fix it because of the pixmaps *)
      wlist#columns_autosize ()

    method set_columns l =
      columns <- l;
      self#set_titles (List.map Gui_columns.Server.string_of_column !!columns);
      self#update;
      self#set_list_bg (`NAME !!O.color_list_bg) (Gdk.Font.load_fontset !!O.font_list)
    
    method column_menu  i = 
      [
        `I (M.mAutosize, fun _ -> GToolbox.autosize_clist self#wlist);
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
            ) Gui_columns.Server.column_strings));
        `M (M.mAdd_column_before, (
            List.map (fun (c,s,_) ->
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
         Col_server_address -> compare s1.gserver_addr s2.gserver_addr
      |	Col_server_state -> compare s1.gserver_state s2.gserver_state
      |	Col_server_users -> compare s1.gserver_nusers s2.gserver_nusers
      |	Col_server_files -> compare s1.gserver_nfiles s2.gserver_nfiles
      |	Col_server_desc -> compare s1.gserver_description s2.gserver_description
      | Col_server_network -> compare s1.gserver_network s2.gserver_network
      | Col_server_name -> compare s1.gserver_name s2.gserver_name
    
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
        (Printf.sprintf Gui_messages.mW_sb_connected_to_servers 
          !G.nconnected_servers !G.nservers)

    method content_by_col s col =
      match col with
        Col_server_address -> 
          Printf.sprintf "%16s : %-5d"
            (Ip.string_of_addr s.gserver_addr) s.gserver_port
      |	Col_server_state -> fst (string_color_of_state s.gserver_state)
      |	Col_server_users ->
          if s.gserver_nusers = zero then ""
          else Printf.sprintf "%5Ld" s.gserver_nusers
      |	Col_server_files ->
          if s.gserver_nfiles = zero then ""
          else Printf.sprintf "%7Ld"  s.gserver_nfiles
      |	Col_server_desc ->
          if s.gserver_name = "" then ""
          else Printf.sprintf "%s [%s]" s.gserver_name s.gserver_description
      | Col_server_network ->
          Gui_global.network_name s.gserver_network
      | Col_server_name -> if (shorten s.gserver_name 30) = ""
                             then "<unknown>"
                             else shorten s.gserver_name 30
    
    method content s =
      let strings = List.map 
          (fun col -> match col with
               Col_server_name ->
                 (match s.gserver_pixmap with
                     Some pixmap -> P.Pixtext (self#content_by_col s col, pixmap)
                   | _ -> P.String (self#content_by_col s col))
             | Col_server_network ->
                 (match s.gserver_net_pixmap with
                     Some pixmap -> P.Pixmap (pixmap)
                   | _ -> P.String (self#content_by_col s col))
             | _ -> P.String (self#content_by_col s col))
        !!columns
      in
      let col_opt = 
        match snd (string_color_of_state s.gserver_state) with
          None -> Some `BLACK
        | Some c -> Some (`NAME c)
      in
      (strings, col_opt)
    
    method remove () =
      List.iter
        (fun s -> Gui_com.send (GuiProto.RemoveServer_query (s.gserver_num)))
      self#selection
    
    method connect () =
      List.iter
        (fun s -> Gui_com.send (GuiProto.ConnectServer s.gserver_num))
      self#selection
    
    method disconnect () =
      List.iter
        (fun s -> Gui_com.send (GuiProto.DisconnectServer s.gserver_num))
      self#selection
    
    method view_users () =
      List.iter
        (fun s -> Gui_com.send (GuiProto.ViewUsers s.gserver_num))
      self#selection
    
    method add_server () =
      let text1 = M.sT_lb_add_server in
      let text2 = M.sT_lb_network in
      let title = M.sT_wt_add_server in
      let nets,wcombo = Gui_global.networks_combo false in
      let we_chaine = GEdit.entry ~width: 200 ~text:"" () in
      match
        input_widget2  ~widget1:we_chaine#coerce ~widget2:wcombo#coerce
        ~get_text:(fun () -> wcombo#entry#text, we_chaine#text) ~bind_ok:true
        ~title  text1 text2
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
    (* Same comment than for friends tab.
    We will display only the first one selected instead of the last one
    (like this there is only display the first one and not display the
    first one and then the last one).
    Furthermore this loads the CPU with the pixmaps and can
    block everything => see with All the networks selected *)
      if s = List.hd (List.rev self#selection) then
      match s.gserver_users with
        None -> 
          users#clear;
          Gui_com.send (GuiProto.GetServer_users s.gserver_num);
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

    method on_deselect s = users#clear
    
    method menu =
      (match self#selection with
          [] -> []
        |	_ ->
            [ `I ((M.sT_me_connect), self#connect) ;
              `I ((M.sT_me_disconnect), self#disconnect) ;
              `I ((M.sT_me_view_users), self#view_users) ;
              `I ((M.sT_me_remove), self#remove) ;
              `S ]
      ) @
        [ `I ((M.sT_me_connect_more_servers), self#connect_more_servers) ;
        `I ((M.sT_me_remove_old_servers), self#remove_old_servers)
      ]	
    
    method set_tb_style st = 
      if Options.(!!) Gui_options.mini_toolbars then
        (wtool1#misc#hide (); wtool2#misc#show ()) else
        (wtool2#misc#hide (); wtool1#misc#show ());
      wtool1#set_style st;
      wtool2#set_style st;

(** {2 Handling core messages} *)
    
    method filter = is_filtered
    
    method core_to_gui_server s =
      {
       gserver_num = s.server_num;
       gserver_network = s.server_network;
       gserver_addr = s.server_addr;
       gserver_port = s.server_port;
       gserver_score = s.server_score;
       gserver_tags = s.server_tags;
       gserver_nusers = s.server_nusers;
       gserver_nfiles = s.server_nfiles;
       gserver_state = s.server_state;
       gserver_name = s.server_name;
       gserver_description = s.server_description;
       gserver_users = s.server_users;
       gserver_banner = s.server_banner;
       gserver_pixmap =
         if icons_are_used then
           Some (get_pix s.server_state)
           else None;
       gserver_net_pixmap =
         if icons_are_used then
           Some (Gui_options.network_pix
                   (Gui_global.network_name s.server_network))
           else None;
      }

    method update_server s s_new row =
      s.gserver_score <- s_new.server_score ;
      s.gserver_tags <- s_new.server_tags ;
      s.gserver_nusers <- s_new.server_nusers ;
      s.gserver_nfiles <- s_new.server_nfiles ;
      s.gserver_name <- s_new.server_name ;
      s.gserver_description <- s_new.server_description ;
      if !G.nservers <> self#size then begin
          G.nservers := self#size;
          self#update_wl_status
        end;
      (if s.gserver_users != s_new.server_users then begin
         s.gserver_users <- s_new.server_users ;
            List.iter (fun ss ->
                if s == ss then
                  self#on_select s
            ) self#selection;
          end
        (* this not satisfactory but we have to find a way to disconnect
        the servers GUI list updating from the servers list updating.
        This limits the flickering and high CPU usage especially in case we
        receive a lot of users *)
         else if s_new.server_state = RemovedHost then
           self#remove_item row s
           else begin
             s.gserver_state <- s_new.server_state ;
             s.gserver_pixmap <-
               if icons_are_used then
                 Some (get_pix s_new.server_state)
                 else None;
             self#refresh_item row s
             end
      )
    
    
    method find_server num = self#find num
    
    method h_server_info s = 
      try
        let (row, serv) = self#find_server s.server_num in
        (
          match Mi.is_connected s.server_state, Mi.is_connected serv.gserver_state with
            true, false -> incr G.nconnected_servers ; self#update_wl_status
          | false, true -> decr G.nconnected_servers ; self#update_wl_status
          | _ -> ()
        );
        self#update_server serv s row;
      with
        Not_found ->
          if s.server_state <> RemovedHost then
            (
              let si = self#core_to_gui_server s in
              self#add_item si;
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
        (
          match Mi.is_connected state, Mi.is_connected serv.gserver_state with
            true, false -> incr G.nconnected_servers ; self#update_wl_status
          | false, true -> decr G.nconnected_servers ; self#update_wl_status
          | _ -> ()
        );
        if state = RemovedHost then
          begin
            self#remove_item row serv;
            if !G.nservers <> self#size then begin
              G.nservers := self#size;
              self#update_wl_status
              end     
          end else
             begin
            serv.gserver_state <- state ;
            serv.gserver_pixmap <-
              if icons_are_used then
                Some (get_pix state)
                else None;
            self#refresh_item row serv
            end
      with
        Not_found -> Gui_com.send (GuiProto.GetServer_info num)
    
    
    method h_server_busy num nusers nfiles =
      try
        let (row, serv) = self#find_server num in
        serv.gserver_nusers <- nusers ;
        serv.gserver_nfiles <- nfiles ;
        self#refresh_item row serv
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
        let users = match serv.gserver_users with
            None -> []
          | Some users -> users in
        if not (List.memq user_num users) then
          serv.gserver_users <- Some (user_num :: users);
          self#refresh_item row serv
      with
        Not_found ->
          if num <> 0 then begin
              Gui_com.send (GuiProto.GetServer_info num);
              Gui_com.send (GuiProto.GetServer_users num)
            end
    
    method update_icons b =
      icons_are_used <- b;
      let (f, label, step) =
        if b then
          ((fun s ->
          s.gserver_net_pixmap <-
            Some (Gui_options.network_pix
                    (Gui_global.network_name s.gserver_network));
          s.gserver_pixmap <- Some (get_pix s.gserver_state)
          ), M.pW_lb_servers_add_icons, 1)
          else
            ((fun s ->
            s.gserver_net_pixmap <- None;
            s.gserver_pixmap <- None
            ), M.pW_lb_servers_remove_icons, 1)
      in
      Gui_options.generate_with_progress label self#get_all_items f step


    initializer
      box#vbox1#pack ~expand: true pl#box ;

      let style = evbox1#misc#style#copy in
      style#set_bg [ (`NORMAL, (`NAME "#494949"))];
      evbox1#misc#set_style style;
      let style = label_servers#misc#style#copy in
      style#set_fg [ (`NORMAL, `WHITE)];
      label_servers#misc#set_style style;
(*      ignore (wb_add#connect#clicked self#add_server); *)
(*
      Gui_misc.insert_buttons wtool1 wtool2 
        ~text: (M.remove)
      ~tooltip: (M.remove)
      ~icon: (M.o_xpm_remove)
      ~callback: self#remove
        ();
      
      Gui_misc.insert_buttons wtool1 wtool2 
        ~text: (M.connect)
      ~tooltip: (M.connect)
      ~icon: (M.o_xpm_connect)
      ~callback: self#connect
        ();
      
      Gui_misc.insert_buttons wtool1 wtool2 
        ~text: (M.disconnect)
      ~tooltip: (M.disconnect)
      ~icon: (M.o_xpm_disconnect)
      ~callback: self#disconnect
        ();
      
      Gui_misc.insert_buttons wtool1 wtool2 
        ~text: (M.view_users)
      ~tooltip: (M.view_users)
      ~icon: (M.o_xpm_view_users)
      ~callback: self#view_users
        ();
      
      Gui_misc.insert_buttons wtool1 wtool2 
        ~text: (M.connect_more_servers_text)
      ~tooltip: (M.connect_more_servers_tips)
      ~icon: (M.o_xpm_connect_more_servers)
      ~callback: self#connect_more_servers
        ();
      
      Gui_misc.insert_buttons wtool1 wtool2 
        ~text: (M.remove_old_servers_text)
      ~tooltip: (M.remove_old_servers_tips)
      ~icon: (M.o_xpm_remove_old_servers)
      ~callback: self#remove_old_servers
        ();
*)
      Gui_misc.insert_buttons wtool1 wtool2 
        ~text: (M.sT_lb_display_all_servers)
        ~tooltip: (M.sT_ti_display_all_servers)
        ~icon: (M.o_xpm_toggle_display_all_servers)
        ~callback: self#toggle_display_all_servers
        ();
      
      Gui_misc.insert_buttons wtool1 wtool2
        ~text: (M.sT_lb_add_server)
        ~tooltip: (M.sT_ti_add_server)
        ~icon: (M.o_xpm_add_server)
        ~callback: self#add_server
        ()
      
  end

class pane_servers () =
  let box_users = new Gui_users.box_users () in
  let wl_status = GMisc.label ~text: "" ~show: true () in
  let box_servers = new box O.servers_columns box_users wl_status in
  (* we moved the queries box to the Search tab that now includs the
  queries box and the results box *)
(*
  let box_queries = new Gui_queries.paned () in
*)

  object (self)
    inherit Gui_servers_base.pane ()

    method wl_status = wl_status
    method box_servers = box_servers
    method box_users = box_users
    method vpaned = vpaned_servers

    method set_tb_style st = 
      box_users#set_tb_style st ;
      box_servers#set_tb_style st

    method set_list_bg bg font =
      box_users#set_list_bg bg font;
      box_servers#set_list_bg bg font

    method clear =
      wl_status#set_text "";
      box_servers#clear ;
      box_users#clear
(*
      box_queries#clear

    method tab_queries = box_queries
*)
      
    (** {2 Handling core messages} *)

    method h_server_filter_networks = box_servers#h_server_filter_networks
    method h_server_info = box_servers#h_server_info
    method h_server_state = box_servers#h_server_state
    method h_server_busy = box_servers#h_server_busy
    method h_server_user = box_servers#h_server_user

    method clean_table servers = box_servers#clean_table servers
      
    method c_update_icons b =
      box_servers#update_icons b

    initializer
      vpaned_servers#add1 box_servers#coerce;
      vpaned_servers#add2 box_users#coerce;
      
      box_users#label_users#set_text ( M.sT_lb_users);

(*
      queries_frame#add box_queries#coerce;
*)
  end

