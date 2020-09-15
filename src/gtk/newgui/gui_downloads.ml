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
open Md4
open GMain
open GtkBase
open Gtk
open Gpattern
open Printf2
  
open Gettext
open Gui_global
open CommonTypes
open GuiTypes
open Gui_types
open GuiProto
open Gui_columns

module M = Gui_messages
module P = Gpattern
module O = Gui_options
module G = Gui_global
module VB = VerificationBitmap

let use_interested_in_sources = ref false
let interested_in_sources = ref false
let interested_in_sources_version = 27
  
let preview file () = Gui_com.send (Preview (List.hd file.data.gfile_num))

let get_priority_pixmap p =
  match p with
      -10 -> Some (O.gdk_pix M.o_xpm_priority_0)
    | 0 -> Some (O.gdk_pix M.o_xpm_priority_1)
    | 10 -> Some (O.gdk_pix M.o_xpm_priority_2)
    | _ -> None

let tree_pixmap b =
  if b then
    O.gdk_pix M.o_xpm_tree_opened
    else O.gdk_pix M.o_xpm_tree_closed

let the_col_width = ref 70

let fake_tree_pixmap (pixmap :  GDraw.pixmap) =
  pixmap#set_foreground `BLACK;
  for i = 0 to 3 do
    pixmap#point ~x:7 ~y:(2 * i);
    pixmap#point ~x:7 ~y:(12 + 2 * i)
  done;
  for i = 0 to 3 do
    pixmap#point ~x:(9 + 2 * i) ~y:9
  done;
  pixmap#arc ~x:5 ~y:7 ~width:4 ~height:4 ();
  pixmap

let pix_with_mask (pixmap : GDraw.pixmap) (pix1 : GDraw.pixmap) =
  let mask = match pixmap#mask with Some m -> m | None -> assert false in
  let wmask = new GDraw.drawable mask in
  let _ = match  pix1#mask with
              Some m ->
                  let image = Gdk.Image.get m ~x:0 ~y:0 ~width:16 ~height:16 in
                  let pixel = Gdk.Color.pixel (GDraw.color `BLACK) in
                  for i = 0 to 15 do
                    for j = 0 to 15 do
                      let col =
                        if Gdk.Image.get_pixel image ~x:i ~y:j  = pixel then
                          `BLACK
                          else `WHITE
                      in
                      wmask#set_foreground col;
                      wmask#point ~x:(i + 16) ~y:j
                    done
                  done
            | None -> ()
  in
  pixmap#put_pixmap ~x:16 ~y:0 ~xsrc:0 ~ysrc:0 ~width:16 ~height:16 pix1#pixmap;
  pixmap

let friend_source =
  let pixmap = GDraw.pixmap ~width:32 ~height:16 ~mask:true
    ~colormap:(Gdk.Color.get_system_colormap ()) ()
  in
  let pixmap = fake_tree_pixmap pixmap in
  let pix1 = Gui_friends.type_pix client_friend_tag in
  pix_with_mask pixmap pix1

let contact_source =
  let pixmap = GDraw.pixmap ~width:32 ~height:16 ~mask:true
    ~colormap:(Gdk.Color.get_system_colormap ()) ()
  in
  let pixmap = fake_tree_pixmap pixmap in
  let pix1 = Gui_friends.type_pix client_contact_tag in
  pix_with_mask pixmap pix1

let normal_source =
  let pixmap = GDraw.pixmap ~width:32 ~height:16 ~mask:true
    ~colormap:(Gdk.Color.get_system_colormap ()) ()
  in
  let pixmap = fake_tree_pixmap pixmap in
  let pix1 = Gui_friends.type_pix 0 in
  pix_with_mask pixmap pix1

let get_source_pix client_type =
  if client_friend_tag land client_type <> 0 then friend_source else
  if client_contact_tag land client_type <> 0 then contact_source else
    normal_source


  
let save_menu_items file =
  List.map
    (fun name ->
      `I (name, 
        (fun _ -> 
            match file.data.gfile_state with
              FDownloaded  -> Gui_com.send (GuiProto.SaveFile (List.hd (file.data.gfile_num), name))
            | _  ->  Gui_com.send (GuiProto.RenameFile (List.hd (file.data.gfile_num), name))
        )
      )
  ) 
  file.data.gfile_names
  
let file_first_name b f = 
  if b then f.data.gfile_name
    else String.sub f.data.gfile_name 5 ((String.length f.data.gfile_name) - 5)

let save_as file b () = 
  let file_opt = GToolbox.input_string ~title:(M.dT_wt_save_as)
    ~text:(file_first_name b file)
    (M.dT_lb_save_as) in
  match file_opt with
    None -> ()
  | Some name -> 
          match file.data.gfile_state with
              FDownloaded  -> Gui_com.send (GuiProto.SaveFile (List.hd (file.data.gfile_num), name))
            | _  ->  Gui_com.send (GuiProto.RenameFile (List.hd (file.data.gfile_num), name))
      
  
let (!!) = Options.(!!)



let file_to_general_state state =
  match state with
      FileDownloading -> FDownloading
    | FileCancelled -> FCancelled
    | FileQueued -> FQueued
    | FilePaused -> FPaused
    | FileDownloaded -> FDownloaded
    | FileShared  -> FShared
    | FileNew -> FNew
    | FileAborted s -> FAborted s

let client_to_general_state state file_num =
  match state with
    | Connected_downloading n ->
        if n = (-1) 
          then CConnected_downloading
          else if file_num = n 
            then CConnected_downloading
            else CConnected (-1)
    | Connected n -> CConnected n
    | Connecting  -> CConnecting
    | NewHost -> CNewHost
    | Connected_initiating -> CConnected_initiating
    | ServerFull -> CNotConnected (BasicSocket.Closed_connect_failed,0)
    | NotConnected (p,n) -> CNotConnected (p,n)
    | RemovedHost -> CRemovedHost
    | BlackListedHost -> CBlackListedHost

let string_of_file_state f =
  match f.data.gfile_state with
  | FDownloading -> if f.data.gfile_download_rate > 0.
                        then M.dT_tx_downloading
                        else M.dT_tx_waiting
  | FCancelled -> M.dT_tx_cancelled
  | FQueued -> M.dT_tx_queued
  | FPaused -> M.dT_tx_paused
  | FDownloaded -> M.dT_tx_complete
  | FShared  -> M.dT_tx_dl_done
  | FNew -> assert false
  | FAborted s -> Printf.sprintf Gui_messages.dT_tx_dl_aborted s
  | CConnected_downloading -> M.dT_tx_downloading
  | CConnected (-1) -> M.dT_tx_connected
  | CConnecting  -> M.dT_tx_connecting
  | CNewHost -> M.dT_tx_new_host
  | CConnected_initiating -> M.dT_tx_initiating
  | CConnected 0 -> M.dT_tx_queued
  | CConnected n -> Printf.sprintf Gui_messages.dT_tx_ranked n
  | CNotConnected (_,n) ->
      if n = -1 then
        ""
      else
      if n = 0 then
        (M.dT_tx_queued_out)
      else
      if n > 0 then
         Printf.sprintf Gui_messages.dT_tx_ranked_out n
      else
         Printf.sprintf Gui_messages.dT_tx_failed (- n - 1)
       
  | CRemovedHost -> M.dT_tx_removed
  | CBlackListedHost -> M.dT_tx_black_listed

      
let some_is_available f =
  match f.data.gfile_availability with
    (_,avail) :: _ ->
      
      if !!Gui_options.use_relative_availability
      then
        match f.data.gfile_chunks with
        | None -> false
        | Some chunks ->
            String2.existsi (fun i a ->
              CommonGlobals.partial_chunk (VB.get chunks i) &&
                a <> (char_of_int 0)
            ) avail
      else
        String2.exists ((<>) (char_of_int 0)) avail
  | _ -> false
      
let color_opt_of_file f =
  if f.data.gfile_download_rate > 0. then
    Some !!O.color_downloading
  else if some_is_available f then
    Some !!O.color_available
  else if f.data.gfile_state = FDownloaded then
    Some !!O.color_downloaded
  else
    Some !!O.color_not_available

let float_avail s = 
  try float_of_string s
  with _ -> 0.0

let file_availability f =
  match f.data.gfile_availability with
    (_,avail) :: _ ->
      
      (match f.data.gfile_chunks with
      | None -> "---"
      | Some chunks ->
          let rec loop i p n =
            if i < 0 then
              if n = 0 then "---"
              else Printf.sprintf "%5.1f" ((float p) /. (float n) *. 100.0)
            else 
              loop (i - 1)
                (if CommonGlobals.partial_chunk (VB.get chunks i) then p + 1 
                else p)
                (if avail.[i] <> (char_of_int 0) then n + 1 else n) in
      loop ((String.length avail) - 1) 0 0)
  | _ -> "---"
      
let string_availability s =
  match s with
    (_,s) :: _ ->
      
      let len = String.length s in
      let p = ref 0 in
      for i = 0 to len - 1 do
        if s.[i] <> '0' then begin
            incr p
          end
      done;
      if len = 0 then "" else 
        Printf.sprintf "%5.1f" (float_of_int !p /. float_of_int len *. 100.)
  | _ -> ""
      
let string_of_format format =
  match format with
    AVI f ->
      Printf.sprintf "AVI: %s %dx%d %g fps %d bpf"
        f.avi_codec f.avi_width f.avi_height 
        (float_of_int(f.avi_fps) *. 0.001) f.avi_rate
  | MP3 (tag, _) ->
      let module M = Mp3tag.Id3v1 in
      Printf.sprintf "MP3: %s - %s (%d): %s"
        tag.M.artist tag.M.album 
        tag.M.tracknum tag.M.title
  | _ -> (M.dT_tx_unknown)

let max_last_seen = 100 * 24 * 3600

let time_to_string time =
  if time = max_last_seen 
    then "--"
    else Date.time_to_string time "long"

let max_eta = 1000.0 *. 60.0 *. 60.0 *. 24.0
    
let calc_file_eta f =
  let size = Int64.to_float f.data.gfile_size in
  let downloaded = Int64.to_float f.data.gfile_downloaded in
  let missing = size -. downloaded in
  let rate =
      let age = f.data.gfile_age in
      if age > 0
      then downloaded /. (float_of_int age)
      else 0.
  in
  let eta = 
    if rate = 0.0 then max_eta else
    let eta = missing /. rate in
    if eta < 0. || eta > max_eta then max_eta else
      eta
  in
  int_of_float eta


class box columns sel_mode () =
  let titles = List.map Gui_columns.File.string_of_column !!columns in
  object (self)
    inherit [gui_file_info Gpattern.ptree] Gpattern.filtered_ptree sel_mode titles true
            (fun f -> f.data.gfile_num) as pl
      inherit Gui_downloads_base.box () as box
    
    method filter = (fun _ -> false)
    
    val mutable columns = columns
    
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
      self#set_titles (List.map Gui_columns.File.string_of_column !!columns);
      self#update;
      self#set_list_bg (`NAME !!O.color_list_bg)
        (Gdk.Font.load_fontset !!O.font_list)

    
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
            ) Gui_columns.file_column_strings));
        `M (M.mAdd_column_before, (
            List.map (fun (c,s,_) ->
                (`I (s, (fun _ -> 
                        let c1, c2 = List2.cut i !!columns in
                        columns =:= c1 @ [c] @ c2;
                        self#set_columns columns
                    )))
            ) Gui_columns.file_column_strings));
      ]
    
    
    method has_changed_width l =
      (* Printf.printf "Gui_downloads has_changed_width\n";
      flush stdout;*)
      List.iter ( fun (col, width) ->
        if ((self#wlist#column_title col) = (M.c_avail)) && (!the_col_width <> width)
          then begin
            (* Printf.printf "Column No %d Width %d Width_ref %d\n" col width !the_col_width;
            flush stdout;*)
            the_col_width := width;
            self#resize_all_avail_pixmap
          end
      ) l

    method resize_all_avail_pixmap = ()

    method box = box#coerce
    method vbox = box#vbox
    
    method compare_by_col col f1 f2 =
      match col with
      | Col_file_name -> compare f1.data.gfile_name f2.data.gfile_name
      | Col_file_size -> compare f1.data.gfile_size f2.data.gfile_size
      | Col_file_downloaded -> compare f1.data.gfile_downloaded f2.data.gfile_downloaded
      |	Col_file_percent -> compare 
            (Int64.to_float f1.data.gfile_downloaded /. Int64.to_float f1.data.gfile_size)
          (Int64.to_float f2.data.gfile_downloaded /. Int64.to_float f2.data.gfile_size)
      | Col_file_rate-> compare f1.data.gfile_download_rate f2.data.gfile_download_rate
      | Col_file_state -> compare f1.data.gfile_state f2.data.gfile_state
      |	Col_file_availability ->
          if (List.length f1.data.gfile_num) = 1 && (List.length f2.data.gfile_num) = 1 then
            compare f1.data.gfile_downloaded f2.data.gfile_downloaded
            else compare (string_availability f1.data.gfile_availability)
                         (string_availability f2.data.gfile_availability)
        (* let fn =
            if !!Gui_options.use_relative_availability
            then file_availability
            else fun f -> string_availability f.file_availability
          in
          compare (float_avail (fn f1)) (float_avail (fn f2))*)
      | Col_file_md4 -> compare (Md4.to_string f1.data.gfile_md4) (Md4.to_string f2.data.gfile_md4)
      | Col_file_format -> compare f1.data.gfile_format f2.data.gfile_format
      | Col_file_network -> compare f1.data.gfile_network f2.data.gfile_network
      | Col_file_status -> compare f1.data.gfile_age f2.data.gfile_age
      | Col_file_eta -> compare (calc_file_eta f1) (calc_file_eta f2)
      | Col_file_priority -> compare f1.data.gfile_priority f2.data.gfile_priority
    
    method compare f1 f2 =
      let abs = if current_sort >= 0 then current_sort else - current_sort in
      let col = 
        try List.nth !!columns (abs - 1) 
        with _ -> Col_file_name
      in
      let res = self#compare_by_col col f1 f2 in
      current_sort * res
    
    method content_by_col f col =
      match col with
        Col_file_name -> Gui_misc.short_name f.data.gfile_name
      |	Col_file_size ->
          if (List.length f.data.gfile_num) = 1 then
            Gui_misc.size_of_int64 f.data.gfile_size ^ " ("
              ^ string_of_int (List.length f.children) ^ ")"
            else "UL = " ^ Gui_misc.size_of_int64 f.data.gfile_size
      |	Col_file_downloaded ->
          if (List.length f.data.gfile_num) = 1 then
            Gui_misc.size_of_int64 f.data.gfile_downloaded
            else "DL = " ^ Gui_misc.size_of_int64 f.data.gfile_downloaded
      |	Col_file_percent ->
          if (List.length f.data.gfile_num) = 1 then
            if Int64.to_float f.data.gfile_size <> 0. then
              Printf.sprintf "%5.1f" 
                 (Int64.to_float f.data.gfile_downloaded /. Int64.to_float f.data.gfile_size *. 100.)
              else ""
            else List.hd f.data.gfile_names
      |	Col_file_rate ->
          if (List.length f.data.gfile_num) = 1 then
            if f.data.gfile_download_rate > 0. then
              Printf.sprintf "%5.1f" (f.data.gfile_download_rate /. 1024.)
              else ""
          else ""
      |	Col_file_state ->
          string_of_file_state f
      |	Col_file_availability ->
          if (List.length f.data.gfile_num) = 1 then (
          if !!Gui_options.use_relative_availability
          then file_availability f
              else string_availability f.data.gfile_availability)
            else string_availability f.data.gfile_availability
      | Col_file_md4 ->
          if (List.length f.data.gfile_num) = 1 then
            Md4.to_string f.data.gfile_md4
            else ""
      | Col_file_format ->
          if (List.length f.data.gfile_num) = 1 then
            string_of_format f.data.gfile_format
            else ""
      | Col_file_network -> Gui_global.network_name f.data.gfile_network
      |	Col_file_status ->
          if (List.length f.data.gfile_num) = 1 
            then begin
              let age = time_to_string f.data.gfile_age in
              let last = 
                if f.data.gfile_last_seen > 0
                  then time_to_string f.data.gfile_last_seen
                  else "--"
              in
              Printf.sprintf "%s / %s" last age
            end else ""
      | Col_file_eta ->
          if (List.length f.data.gfile_num) = 1 then
          let eta = calc_file_eta f in
          if eta >= 1000 * 60 * 60 * 24 then
              Printf.sprintf "---"
              else time_to_string eta
            else ""
      |	Col_file_priority ->
          if (List.length f.data.gfile_num) = 1 then
            (match f.data.gfile_priority with
                 -20 -> M.dT_tx_priority_verylow
               | -10 -> M.dT_tx_priority_low
               | 0 -> M.dT_tx_priority_normal
               | 10 -> M.dT_tx_priority_high
               | 20 -> M.dT_tx_priority_veryhigh
               | _ -> Printf.sprintf "%d" f.data.gfile_priority)
            else ""
    
    method content f =
      let strings = List.map
          (fun col -> match col with
               Col_file_name ->
                 (match f.data.gfile_pixmap with
                     Some pixmap -> P.Pixtext (self#content_by_col f col, pixmap)
                   | _ -> P.String (self#content_by_col f col))
             | Col_file_network ->
                 (match f.data.gfile_net_pixmap with
                     Some pixmap -> P.Pixmap (pixmap)
                   | _ -> P.String (self#content_by_col f col))
             | Col_file_priority ->
                 (match f.data.gfile_priority_pixmap with
                     Some pixmap -> P.Pixmap (pixmap)
                   | _ -> P.String (self#content_by_col f col))
             | Col_file_availability ->
                 (match f.data.gfile_avail_pixmap with
                     Some pixmap -> P.Pixmap (pixmap)
                   | _ -> P.String (self#content_by_col f col))
             | _ -> P.String (self#content_by_col f col))

        !!columns 
      in
      let col_opt = 
        match color_opt_of_file f with
          None -> Some `BLACK
        | Some c -> Some (`NAME c)
      in
      (strings, col_opt)
    
    method find_file num = self#find num
    
    method remove_file f row = 
      self#remove_item row f
    
    method set_tb_style tb = 
      if Options.(!!) Gui_options.mini_toolbars then
        (wtool1#misc#hide (); wtool2#misc#show ()) else
        (wtool2#misc#hide (); wtool1#misc#show ());
      wtool2#set_style tb;
      wtool1#set_style tb
    
    initializer

      box#vbox#pack ~expand: true pl#box;
      ask_clients#misc#hide ()

end


(* make a simple function to give a 3D effect *)
let highlight range i =
  if i < 8
    then 256 * (2 * i * range / 16)
    else 256 * (range - (i * range / 2 / 16))

(* define the colors we will use to display the availability *)
let color_red = GDraw.pixmap ~width:1 ~height:16
                  ~colormap:(Gdk.Color.get_system_colormap ()) ()
let _ =
  for i = 0 to 15 do
    let r = highlight 255 i in
    color_red#set_foreground (`RGB (r, 0, 0));
    color_red#point ~x:0 ~y:i
  done
    
let color_green = GDraw.pixmap ~width:1 ~height:16
                    ~colormap:(Gdk.Color.get_system_colormap ()) ()
let _ =
  for i = 0 to 15 do
    let g = highlight 255 i in
    color_green#set_foreground (`RGB (0, g, 0));
    color_green#point ~x:0 ~y:i
  done
    
let color_black = GDraw.pixmap ~width:1 ~height:16
                    ~colormap:(Gdk.Color.get_system_colormap ()) ()
let _ =
  for i = 0 to 15 do
    let r = highlight 128 i in
    color_black#set_foreground (`RGB (r, r, r));
    color_black#point ~x:0 ~y:i
  done
    
let color_orange = GDraw.pixmap ~width:1 ~height:16 ~colormap:(Gdk.Color.get_system_colormap ()) ()
let _ =
  for i = 0 to 15 do
    let r = highlight 255 i in
    let g = 178 * r / 255 in
    color_orange#set_foreground (`RGB (r, g, 0));
    color_orange#point ~x:0 ~y:i
  done
      
let color_blue_relative = ref [||]
let _ =
  for i = 0 to (!!O.availability_max - 1) do
    let pixmap = GDraw.pixmap ~width:1 ~height:16
                   ~colormap:(Gdk.Color.get_system_colormap ()) () in
    let col_step = i * 255 / (!!O.availability_max - 1) in
    for j = 0 to 15 do
      let b = highlight 255 j in
      let g = highlight col_step j in
      pixmap#set_foreground (`RGB (0, g, b));
      pixmap#point ~x:0 ~y:j
    done;
    color_blue_relative := Array.append !color_blue_relative [|pixmap|]
  done
      
let color_grey = GDraw.pixmap ~width:1 ~height:16 ~colormap:(Gdk.Color.get_system_colormap ()) ()
let _ =
  for i = 0 to 15 do
    let r = highlight 255 i in
    color_grey#set_foreground (`RGB (r, r, r));
    color_grey#point ~x:0 ~y:i
  done

let get_avail_pixmap avail chunks is_file =
  let (width, height) = (!the_col_width - 3, 16) in (* clist height has previously been fixed *)
  let pixmap = GDraw.pixmap ~width:width ~height:height
      ~colormap:(Gdk.Color.get_system_colormap ()) ()
  in
  match chunks with
  | None -> pixmap
  | Some chunks ->
      let nchunks = VB.length chunks in
      try 
        match avail with
        | (_,avail) :: _ ->
        
            for i = 0 to (width - 1) do
              let ind = i * (nchunks - 1) / (width - 1) in
              pixmap#put_pixmap
                ~x:i ~y:0 ~xsrc:0 ~ysrc:0 ~width:1 ~height:height
                (if is_file then
                  match int_of_char(avail.[ind]), VB.get chunks ind with
                  | _, (VB.State_complete | VB.State_verified) -> 
                      color_green#pixmap

                  | 0, VB.State_missing -> color_red#pixmap

                  | 0, VB.State_partial -> color_orange#pixmap

                  | h, (VB.State_missing | VB.State_partial) (* h > 0 *) ->
                      let h = 
                        if h >= !!O.availability_max then 0
                        else (!!O.availability_max - h) in
                      let color_blue = !color_blue_relative.(h) in
                      color_blue#pixmap
                else
                  match int_of_char(avail.[ind]), VB.get chunks ind with
                  | 0, VB.State_verified -> color_orange#pixmap

                  | 0, (VB.State_missing | VB.State_partial |
                        VB.State_complete) -> 
                      color_red#pixmap

                  | _, (VB.State_complete | VB.State_verified) ->
                      (* h > 0 *) 
                      color_black#pixmap

                  | _, (VB.State_missing | VB.State_partial) ->
                      (* h > 0 *)
                      color_green#pixmap)
            done;
            pixmap
        | _ -> raise Not_found  
  with _ ->
      begin
        for i = 0 to (width - 1) do
          pixmap#put_pixmap
            ~x:i ~y:0 ~xsrc:0 ~ysrc:0 ~width:1 ~height:height
            color_grey#pixmap
        done;
        pixmap
      end

let refresh_timerID =
  ref (Timeout.add ~ms:2000
      ~callback:(fun _ -> true))

class box_downloads wl_status () =
  
  let label_file_info = GMisc.label () in
  object (self)
    inherit box O.downloads_columns `EXTENDED ()
    
    val mutable use_avail_pixmap  = (!!O.use_graphical_availability : bool)
    val mutable icons_are_used = (!!O.use_icons : bool)
    val mutable is_visible = (false : bool)
    
    
    method update_wl_status : unit =
      wl_status#set_text 
        (Printf.sprintf Gui_messages.mW_sb_downloaded_files 
          !G.ndownloaded !G.ndownloads)
    
    method cancel () =
      let s = ref (M.dT_lb_ask_cancel_download_files) in
      List.iter (fun f -> 
          s := !s ^ (file_first_name icons_are_used f) ^ "\n" 
      ) self#selection;
      match GToolbox.question_box (M.dT_wt_cancel)
        [ M.pW_lb_ok ; M.pW_lb_cancel] !s 
      with
        1 ->
          List.iter
            (fun f ->
              Gui_com.send (RemoveDownload_query (List.hd f.data.gfile_num)))
          self#selection
      |	_ ->
          ()
    
    method retry_connect () =
      List.iter
        (fun f ->
          Gui_com.send (ConnectAll (List.hd f.data.gfile_num)))
      self#selection
    
    method pause_resume () =
      List.iter
        (fun f ->
          Gui_com.send (SwitchDownload (List.hd f.data.gfile_num,
              match f.data.gfile_state with
                FPaused | FAborted _ -> true
              | _ -> false
            )))
      self#selection
    
    method verify_chunks () =
      List.iter
        (fun f ->
          Gui_com.send (VerifyAllChunks (List.hd f.data.gfile_num)))
      self#selection
    
    method get_format () =
      List.iter
        (fun f ->
          Gui_com.send (QueryFormat (List.hd f.data.gfile_num)))
      self#selection
    
    method set_priority prio () =
      List.iter
        (fun f ->
          Gui_com.send (SetFilePriority (List.hd f.data.gfile_num, prio)))
      self#selection
    
    method show_hide_sources file =
      let check_first =
(* Wouldn't it be easier to just store a bool in the file entry ? *)
        List.mem_assoc file.data.gfile_num self#is_expanded
      in
      try
        if check_first then
          self#collapse_file file
        else self#expand_file file
      with e ->
          lprintf "show_hide_sources: exception %s\n"
            (Printexc2.to_string e)

    initializer 
      BasicSocket.add_infinite_timer 30. (fun _ ->

          if !use_interested_in_sources &&
            !interested_in_sources && List.length self#is_expanded = 0 then 
            begin
(*              lprintf "not interested in sources !!!!\n"; *)
              interested_in_sources := false;
              Gui_com.send (InterestedInSources false);
            end
            
      )
          
    method collapse_file file =
(*      lprintf "collapse_file\n"; *)

      ignore (self#collapse file);
      let (row, fi) = self#find_file file.data.gfile_num in
      if icons_are_used then
        file.data.gfile_pixmap <- Some (tree_pixmap false)
        else
          begin
            let s = String.sub file.data.gfile_name 5 ((String.length file.data.gfile_name) - 5) in
            file.data.gfile_name <- "(+)- " ^ s
          end;
      self#update_row file row;
      (* we just empty the pixmaps *)
      List.iter (fun child ->
        child.data.gfile_pixmap <- None;
        child.data.gfile_net_pixmap <- None;
        child.data.gfile_priority_pixmap <- None;
        child.data.gfile_avail_pixmap <- None
      ) fi.children

      
    method expand_file file =
(*      lprintf "expand_file\n"; *)
            
      if !use_interested_in_sources && not !interested_in_sources then begin
(*          lprintf "interested in sources !!!!\n"; *)
          interested_in_sources := true;
          Gui_com.send (InterestedInSources true);
        end;

      let (row, fi) = self#find_file file.data.gfile_num in
      if icons_are_used then
        begin
          file.data.gfile_pixmap <- Some (tree_pixmap true);
          List.iter (fun child ->
            child.data.gfile_pixmap <- 
              Some (get_source_pix child.data.gfile_type);
            child.data.gfile_net_pixmap <- 
              Some (Gui_options.network_pix 
                     (Gui_global.network_name child.data.gfile_network));
            child.data.gfile_avail_pixmap <-
              if use_avail_pixmap then
                Some (get_avail_pixmap child.data.gfile_availability
                                       fi.data.gfile_chunks
                                       false)
                else None;
          ) fi.children
        end else
          begin
            let s = 
              String.sub file.data.gfile_name 5 
                ((String.length file.data.gfile_name) - 5) 
            in
            file.data.gfile_name <- "(=)- " ^ s
          end;
      self#update_row file row;
      ignore (self#expand file)


    method add_to_friends () =
      List.iter
        (fun c ->
           let num = List.hd (List.rev c.data.gfile_num) in
           Gui_com.send (GuiProto.AddClientFriend num))
      self#selection
    
    method save_all () = 
      self#iter	(fun f ->
          match f.data.gfile_state with
              FDownloaded -> 
                Gui_com.send (GuiProto.SaveFile (List.hd f.data.gfile_num, file_first_name icons_are_used f))
            | _ -> () 
      )
    
    method preview () =
      match self#selection with
        [] -> ()
      | file :: _ -> preview file ()
    
    method edit_mp3_tags () = 
      match self#selection with
        file :: _ ->
          (
            match file.data.gfile_format with
              MP3 (tag,_) ->
                Mp3_ui.edit_tag_v1 (M.dT_wt_edit_mp3) tag ;
                Gui_com.send (GuiProto.ModifyMp3Tags (List.hd file.data.gfile_num, tag))
            | _ ->
                ()
          )
      |	_ ->
          ()
    
    method menu =
      match self#selection with
        [] -> []
      |	file :: tail ->
          if List.length file.data.gfile_num = 1 then
            (if tail = [] then
                [
                  `I ((M.dT_me_view_sources), fun _ -> self#show_hide_sources file) ;
                  `I ((M.dT_me_preview), preview file) ;
                  `S ;
                ]
              else  [])@
          `I ((M.dT_me_pause_resume_dl), self#pause_resume) ::
          `I ((M.dT_me_retry_connect), self#retry_connect) ::
          `I ((M.dT_me_cancel), self#cancel) ::
            `S ::
          `I ((M.dT_me_verify_chunks), self#verify_chunks) ::
            `M ((M.dT_me_set_priority), [
                `I ((M.dT_me_set_priority_veryhigh), self#set_priority 20);
                `I ((M.dT_me_set_priority_high), self#set_priority 10);
                `I ((M.dT_me_set_priority_normal), self#set_priority 0);
                `I ((M.dT_me_set_priority_low), self#set_priority (-10));
                `I ((M.dT_me_set_priority_verylow), self#set_priority (-20));
            ]) ::
          `I ((M.dT_me_get_format), self#get_format) ::
            `S ::
          (match (file.data.gfile_state, file.data.gfile_format) with
               (FDownloaded, MP3 _) -> [`I ((M.dT_me_edit_mp3), self#edit_mp3_tags)]
             | _ -> []) @
          `I ((M.dT_me_save_all), self#save_all) ::           
          (if tail = [] then
              [
                `I ((M.dT_me_save_as), save_as file icons_are_used) ;
                  `M ((M.dT_me_save), save_menu_items file) ;
              ]
            else  [])
            else
            [ `I (M.dT_me_add_to_friends, self#add_to_friends) ]

    method resize_all_avail_pixmap =
      if use_avail_pixmap then
        (* Printf.printf "Gui_downloads resize_all_avail_pixmap %b\n"
          use_avail_pixmap;
        flush stdout; *)
        List.iter (fun f ->
           (if (List.length f.data.gfile_num) = 1 then
            f.data.gfile_avail_pixmap <-
              Some (get_avail_pixmap f.data.gfile_availability
                                     f.data.gfile_chunks
                                     true)
            else
            f.data.gfile_avail_pixmap <-
              Some (get_avail_pixmap f.data.gfile_availability
                                     f.data.gfile_chunks
                                     false));
            let (row, _) = self#find_file f.data.gfile_num in
            if row <> (-1) then self#update_row f row
        ) (List.rev self#get_all_items)
    
    val mutable label_shown = false
      
    method on_select file =
    (* same comment than for the friends tab but without
       consequence here. Just to be coherent ;-) *)
      if file = List.hd (List.rev self#selection) then
      if not label_shown then begin
          label_shown <- true;
          self#vbox#pack ~expand: false ~fill: true label_file_info#coerce
          
        end;
      label_file_info#set_text 
        (
        Printf.sprintf "NAME: %s SIZE: %s FORMAT: %s" 
          (file.data.gfile_name)
        (Int64.to_string file.data.gfile_size)
        (string_of_format file.data.gfile_format)
        ;
      )

(** {2 Handling core messages} *)

     method update_file f f_new =
      f.data.gfile_md4 <- f_new.file_md4 ;
      f.data.gfile_name <-
        if icons_are_used then
          f_new.file_name
          else begin
            let s = String.sub f.data.gfile_name 0 5 in
            (s ^ f_new.file_name)
          end;
      f.data.gfile_names <- f_new.file_names ;
      f.data.gfile_size <- f_new.file_size ;
      f.data.gfile_downloaded <- f_new.file_downloaded ;
      f.data.gfile_all_sources <- f_new.file_all_sources ;
      f.data.gfile_active_sources <- f_new.file_active_sources ;
      f.data.gfile_state <- (file_to_general_state f_new.file_state) ;
      f.data.gfile_download_rate <- f_new.file_download_rate ;
      f.data.gfile_format <- f_new.file_format;
      f.data.gfile_age <- (BasicSocket.last_time () - f_new.file_age);
      f.data.gfile_last_seen <- (BasicSocket.last_time () - f_new.file_last_seen);
      if f.data.gfile_priority <> f_new.file_priority then
           begin
             f.data.gfile_priority <- f_new.file_priority;
             f.data.gfile_priority_pixmap <-
               if icons_are_used then
                 (get_priority_pixmap f.data.gfile_priority)
                 else None
           end;
      if ((f.data.gfile_availability <> f_new.file_availability) ||
          f.data.gfile_chunks <> f_new.file_chunks ) then
           begin
             f.data.gfile_availability <- f_new.file_availability ;
             f.data.gfile_chunks <- f_new.file_chunks ;
             f.data.gfile_avail_pixmap <-
               if use_avail_pixmap then
                 Some (get_avail_pixmap f.data.gfile_availability
                                        f.data.gfile_chunks
                                        true)
                 else None
           end

    (* as now we do not update the GUI downloads list every time,
    we need to refresh it when it is visible, i.e. when the
    downloads tab is selected. In the mean time it is not necessary
    to update rows that are not visible *)

    method refresh () =
      let nrows = self#wlist#rows - 1 in
      self#wlist#freeze ();
      for r = 0 to nrows do
        let test_visibility = self#wlist#row_is_visible r in
        let f = self#get_data r in
        match test_visibility with
            `NONE -> ()
          | _ -> self#update_row f r
      done;
      self#wlist#thaw ()
        
    method is_visible b =
      if b then begin
        self#refresh ();
        refresh_timerID :=
          (Timeout.add ~ms:2000
             ~callback:(fun _ ->
               self#refresh ();
               true))
        end else
          Timeout.remove (!refresh_timerID)
    
    (* to convert a file_info into a gui_file_info *)
    method file_to_gui_file f =
      {
        gfile_num = [f.file_num];
        gfile_network = f.file_network;
        gfile_name =
          if icons_are_used then
            f.file_name
            else "(+)- " ^ f.file_name;
        gfile_names = f.file_names;
        gfile_md4 = f.file_md4;
        gfile_size = f.file_size;
        gfile_downloaded = f.file_downloaded;
        gfile_all_sources = f.file_all_sources;
        gfile_active_sources = f.file_active_sources;
        gfile_state  = file_to_general_state f.file_state;
        gfile_chunks = f.file_chunks;
        gfile_availability = f.file_availability;
        gfile_download_rate = f.file_download_rate;
        gfile_format = f.file_format;
        (* file_chunks_age is useless => ignored *)
        gfile_age = (BasicSocket.last_time () - f.file_age);
        gfile_last_seen = (BasicSocket.last_time () - f.file_last_seen);
        gfile_priority = f.file_priority;
        gfile_type = 0;
        gfile_pixmap =
          if icons_are_used then
            Some (tree_pixmap false)
            else None;
        gfile_net_pixmap =
          if icons_are_used then
            Some (Gui_options.network_pix (Gui_global.network_name f.file_network))
            else None;
        gfile_priority_pixmap =
          if icons_are_used then
            (get_priority_pixmap f.file_priority)
            else None;
        gfile_avail_pixmap =
          if use_avail_pixmap then
            Some (get_avail_pixmap f.file_availability f.file_chunks true)
            else None;
      }

    method h_paused f =
      try
        let (_, fi) = self#find_file [f.file_num] in
        self#update_file fi f
      with
        Not_found ->
          incr ndownloads;
          self#update_wl_status ;
          let fi = {data = self#file_to_gui_file f; children = []} in
          self#add_item fi
    
    method h_cancelled num =
      try
        let (row, fi) = self#find_file [num] in
        decr ndownloads;
        if fi.data.gfile_state = FDownloaded then
          decr ndownloaded;
        self#update_wl_status ;
        self#remove_file fi row;
      with
        Not_found ->
          ()
    
    method h_downloaded f =
      try
        let (_, fi) = self#find_file [f.file_num] in
        match fi.data.gfile_state with
            FDownloaded -> ()
          | _ -> incr ndownloaded;
        self#update_wl_status ;
        self#update_file fi f
      with
        Not_found ->
          incr ndownloads;
          incr ndownloaded;
          self#update_wl_status ;
          let fi = {data = self#file_to_gui_file f; children = []} in
          self#add_item fi
         

    method h_removed f =
      try
        let (row, fi) = self#find_file [f.file_num] in
        decr ndownloaded;
        decr ndownloads;
        self#update_wl_status ;
        self#remove_file fi row
      with
        Not_found ->
          ()
    
    method h_downloading f = self#h_paused f

    method h_file_downloaded num dled rate last =
      try
        let (row, f) = self#find_file [num] in
        f.data.gfile_downloaded <- dled;
        f.data.gfile_download_rate <- rate;
        f.data.gfile_last_seen <- (BasicSocket.last_time () - last)
      with Not_found -> ()
    
    method h_file_age num age =
      try
        let (_, f) = self#find_file [num] in
        f.data.gfile_age <- (BasicSocket.last_time () - age)
      with Not_found -> ()
        
    method make_child file client_num avail =
      [{
        data =
          {
            gfile_num = [List.hd file.data.gfile_num; client_num];
            gfile_network = file.data.gfile_network;
            gfile_name =
              if icons_are_used then
                Printf.sprintf "Client %d" client_num
                else "   |-- " ^ (Printf.sprintf "Client %d" client_num);
            gfile_names = [""];
            gfile_md4 = file.data.gfile_md4;
            gfile_size = Int64.of_string "0";
            gfile_downloaded = Int64.of_string "0";
            gfile_all_sources = 0;
            gfile_active_sources = 0;
            gfile_state  = CNewHost;
            gfile_chunks = file.data.gfile_chunks;
            gfile_availability = avail;
            gfile_download_rate = 0.;
            gfile_format = FormatUnknown;
            gfile_age = 0;
            gfile_last_seen = 0;
            gfile_priority = 1;
            gfile_type = 0;
            gfile_pixmap = None;
            gfile_net_pixmap = None;
            gfile_priority_pixmap = None;
            gfile_avail_pixmap = None;
          };
        children = []
      }]

    (* Is it useful to update the row of a client (file expanded)
       in case of an avail change ?
       I think no *)
    method h_file_availability file_num client_num avail =      
      try
        let (row, fi) = self#find_file [file_num] in
        fi.children <-
            (match fi.children with
                [] ->
                   Gui_com.send (GuiProto.GetClient_info client_num);
                   self#make_child fi client_num [0,avail]
              | l ->
                   let length = List.length l in
                   let rec iter i n =
                      if i = n then
                        begin
                          Gui_com.send (GuiProto.GetClient_info client_num);
                          l@(self#make_child fi client_num [0,avail])
                        end else
                          begin
                            let child = List.nth l i in
                            let cnum = List.hd (List.rev child.data.gfile_num) in
                            if cnum = client_num then
                              begin
                                child.data.gfile_availability <- [0,avail];
                                l
                              end else iter (i + 1) n
                          end
                   in iter 0 length
             )

      with _ -> ()
    

    method h_file_location num src =
      try
        let (row, fi) = self#find_file [num] in
        fi.children <-
            (match fi.children with
                [] ->
                   Gui_com.send (GuiProto.GetClient_info src);
                   self#make_child fi src [0,""]
              | l ->
                   let length = List.length l in
                   let rec iter i n =
                      if i = n then begin
                        Gui_com.send (GuiProto.GetClient_info src);
                        l@(self#make_child fi src [0,""])
                        end else
                          begin
                            let child = List.nth l i in
                            let cnum = List.hd (List.rev child.data.gfile_num) in
                            if cnum = src then
                              l
                              else iter (i + 1) n
                          end
                   in iter 0 length
             )

      with _ -> ()
    
    method h_file_remove_location (num:int) (src:int) = 
      try
(*        lprintf "Source %d for %d" src num;  lprint_newline (); *)
        let (row , fi) = self#find_file [num] in
        fi.children <-
            (match fi.children with
                [] ->
                   []
              | l ->
                   let length = List.length l in
                   let rec iter i n =
                      if i = n then
                        l
                        else
                          begin
                            let child = List.nth l i in
                            let cnum = List.hd (List.rev child.data.gfile_num) in
                            if cnum = src then
                              begin
                                let c_array = Array.of_list l in
                                Array.blit c_array (i + 1) c_array i (length - i - 1);
                                Array.to_list (Array.sub c_array 0 (length - 1))
                              end
                              else iter (i + 1) n
                          end
                   in iter 0 length
            )

      with Not_found -> 
(* some sources are sent for shared files in eDonkey. have to fix that *)
(*          lprintf "No such file %d" num; lprint_newline () *)
          ()
    
    method clean_table clients =
      (* Printf.printf "Gui_downloads Clean Table\n";
      flush stdout; *)
      let nrows = self#wlist#rows in
      let array_of_clients = Array.of_list clients in
      let length = Array.length array_of_clients in
      let array_of_clients = Array.of_list clients in
      for r = 0 to (nrows - 1) do
        let fi = self#get_data r in
        if (List.length fi.data.gfile_num) = 1 then
          let lr = ref [] in
          List.iter (fun child ->
            let num = List.hd (List.rev child.data.gfile_num) in
            let rec iter i n =
              if i < n then
                if array_of_clients.(i) = num then
                  lr := child::!lr
                  else iter (i + 1) n
            in iter 0 length
          ) fi.children;
          fi.children <- !lr
      done
    
    (* to update sources in case a file is expanded *)
    method update_client_state (num , state) =
      List.iter (fun f ->
        let length = List.length f.children in
        if (List.length f.data.gfile_num) = 1
          then begin
            let rec iter i n =
              if i = n then begin
                (* Printf.printf "Gui_downloads update_client_state Failed to find SOURCE\n";
                 flush stdout; *)
                 ()
                end else begin
                  let child = List.nth f.children i in
                  let cnum = List.hd (List.rev child.data.gfile_num) in
                  if cnum = num 
                    then if state = RemovedHost 
                      then begin
                        let c_array = Array.of_list f.children in
                        Array.blit c_array (i + 1) c_array i (length - i - 1);
                        f.children <- Array.to_list (Array.sub c_array 0 (length - 1))
                      end else child.data.gfile_state <- client_to_general_state state (List.hd f.data.gfile_num)
                    else iter (i + 1) n
                end
            in iter 0 length

          end else match f.data.gfile_num with
            [file_num; n] ->
              if n = num 
                then f.data.gfile_state <- client_to_general_state state file_num

          | _ -> ()

      ) self#get_all_items

    method update_client_type (num , friend_kind) =
      List.iter (fun f ->
        let length = List.length f.children in
        if (List.length f.data.gfile_num) = 1
          then begin
            let rec iter i n =
              if i = n then begin
                (* Printf.printf "Gui_downloads update_client_type Failed to find SOURCE\n";
                 flush stdout; *)
                 ()
                end else begin
                  let child = List.nth f.children i in
                  let cnum = List.hd (List.rev child.data.gfile_num) in
                  if cnum = num 
                    then f.data.gfile_type <- friend_kind
                    else iter (i + 1) n
                end
            in iter 0 length

          end else match f.data.gfile_num with
            [ _; n] ->
              if n = num then
                begin
                  f.data.gfile_type <- friend_kind;
                  f.data.gfile_pixmap <-
                    if icons_are_used then
                      Some (get_source_pix friend_kind)
                      else None
                end

          | _ -> ()

      ) self#get_all_items

    method update_client c =
      List.iter (fun f ->
        let length = List.length f.children in
        if (List.length f.data.gfile_num) = 1
          then begin
            let rec iter i n =
              if i = n then begin
                (* Printf.printf "Gui_downloads update_client Failed to find SOURCE\n";
                 flush stdout; *)
                 ()
                end else begin
                  let child = List.nth f.children i in
                  let cnum = List.hd (List.rev child.data.gfile_num) in
                  if cnum = c.client_num 
                    then if c.client_state = RemovedHost 
                      then begin
                        let c_array = Array.of_list f.children in
                        Array.blit c_array (i + 1) c_array i (length - i - 1);
                        f.children <- Array.to_list (Array.sub c_array 0 (length - 1))
                      end else begin
                        child.data.gfile_names <- [(if c.client_emulemod = ""
                                                     then c.client_software
                                                     else (c.client_software ^ 
                                                           " - " ^
                                                           c.client_emulemod))];
                        child.data.gfile_size <- c.client_total_uploaded;
                        child.data.gfile_downloaded <- c.client_total_downloaded;
                        child.data.gfile_state  <- client_to_general_state c.client_state (List.hd f.data.gfile_num);
                        child.data.gfile_chunks <- f.data.gfile_chunks;
                        child.data.gfile_name <- 
                          if icons_are_used
                            then c.client_name
                            else "   |-- " ^ c.client_name;
                        child.data.gfile_type <- c.client_type;
                        child.data.gfile_network <- c.client_network;
                      end
                    else iter (i + 1) n
                end
            in iter 0 length

          end else match f.data.gfile_num with
            [file_num; n] ->
              if n = c.client_num then
                begin
                  f.data.gfile_names <- [c.client_software ^ 
                                         " - " ^
                                         c.client_emulemod];
                  f.data.gfile_state <- client_to_general_state c.client_state file_num;
                  f.data.gfile_size <- c.client_total_uploaded;
                  f.data.gfile_downloaded <- c.client_total_downloaded;
                  if f.data.gfile_type <> c.client_type then
                    begin
                      f.data.gfile_type <- c.client_type;                 
                      f.data.gfile_pixmap <-
                        if icons_are_used then
                          Some (get_source_pix c.client_type)
                          else None
                    end
                end

          | _ -> ()

      ) self#get_all_items

    (* options for icons *)
    method update_availability_column b =
      use_avail_pixmap <- b;
      if b then begin
        self#resize_all_avail_pixmap
        end else begin
          List.iter (fun f ->
            f.data.gfile_avail_pixmap <- None
          ) self#get_all_items;
          self#update;
          self#wlist#columns_autosize ()
        end

    method update_icons b =
      List.iter (fun f ->
        let f_expanded =
          List.mem_assoc f.data.gfile_num self#is_expanded
        in
        if f_expanded then self#collapse_file f
      ) self#get_all_items;
      icons_are_used <- b;
      let (f, label, step) =
        if b then
          ((fun f ->
          f.data.gfile_name <- 
            String.sub f.data.gfile_name 5 
             ((String.length f.data.gfile_name) - 5);
          f.data.gfile_pixmap <- 
            Some (tree_pixmap false);
          f.data.gfile_net_pixmap <- 
            Some (Gui_options.network_pix 
             (Gui_global.network_name f.data.gfile_network));
          f.data.gfile_priority_pixmap <- 
            (get_priority_pixmap f.data.gfile_priority);
          List.iter (fun child ->
            child.data.gfile_name <- 
              String.sub child.data.gfile_name 7 
                ((String.length child.data.gfile_name) - 7);
          ) f.children
          ), M.pW_lb_downloads_add_icons, 1)
          else
            ((fun f ->
            f.data.gfile_name <- "(+)- " ^ f.data.gfile_name;
            f.data.gfile_pixmap <- None;
            f.data.gfile_net_pixmap <- None;
            f.data.gfile_priority_pixmap <- None;
            List.iter (fun child ->
              child.data.gfile_name <- "   |-- " ^ child.data.gfile_name;
            ) f.children
            ), M.pW_lb_downloads_remove_icons, 1)
      in
      Gui_options.generate_with_progress label self#get_all_items f step
  
end



class pane_downloads () =
  let wl_status = GMisc.label ~text: "" ~show: true () in
  let dls = new box_downloads wl_status ()
  in
  object (self)
    inherit Gui_downloads_base.paned ()

    method wl_status = wl_status
    method box_downloads = dls

    method set_tb_style st =
      dls#set_tb_style st

    method set_list_bg bg font =
      dls#set_list_bg bg font

    method clear =
      wl_status#set_text "";
      dls#clear

    (** {2 Handling core messages} *)

    method h_file_info f = 
      match f.file_state with
        FileNew -> assert false
      | FileCancelled -> 
          dls#h_cancelled f.file_num
      |	FileDownloaded ->
         dls#h_downloaded f
      |	FileShared ->
         dls#h_removed f
      |	FilePaused | FileQueued | FileAborted _ -> 
          dls#h_paused f
      | FileDownloading ->
          dls#h_downloading f

    method h_file_availability = dls#h_file_availability
    method h_file_age = dls#h_file_age
    method h_file_downloaded = dls#h_file_downloaded

    method h_file_location = dls#h_file_location

    method h_file_remove_location = dls#h_file_remove_location

    method is_visible b = dls#is_visible b

    method h_update_client_state (num , state) =
      dls#update_client_state (num , state)

    method h_update_client_type (num , friend_kind) =
      dls#update_client_type (num , friend_kind)

    method h_update_client c =
      dls#update_client c

    method c_update_availability_column b =
      dls#update_availability_column b

    method c_update_icons b =
      dls#update_icons b

    method clean_table clients = dls#clean_table clients
      
    method on_entry_return () =
      match entry_ed2k_url#text with
        "" -> ()
      | s ->
          Gui_com.send (GuiProto.Url s);
          entry_ed2k_url#set_text ""

    initializer

      Okey.add entry_ed2k_url
        ~mods: []
        GdkKeysyms._Return
        self#on_entry_return;

      box#add dls#coerce ;

      let style = evbox1#misc#style#copy in
      style#set_bg [ (`NORMAL, (`NAME "#494949"))];
      evbox1#misc#set_style style;
      let style = label_entry_ed2k_url#misc#style#copy in
      style#set_fg [ (`NORMAL, `WHITE)];
      label_entry_ed2k_url#misc#set_style style;
      
  end
