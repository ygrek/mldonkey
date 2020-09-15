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

open Printf2
open Options
open Md4

open Gettext
open Gui_global
open CommonTypes
open GuiTypes
open GuiProto
open Gui_columns

module M = Gui_messages
module P = Gpattern
module O = Gui_options
module G = Gui_global

module VB = VerificationBitmap
      
let preview file () =  Gui_com.send (Preview file.file_num)

  
let save_menu_items file =
  List.map
    (fun name ->
      `I (name, 
        (fun _ -> 
            Gui_com.send (GuiProto.SaveFile (file.file_num, name))
        )
      )
  ) 
  file.file_names


let save_as file () = 
  let file_opt = GToolbox.input_string ~title: (gettext M.save) 
    (gettext M.save) in
  match file_opt with
    None -> ()
  | Some name -> 
      Gui_com.send (GuiProto.SaveFile (file.file_num, name))
      
  
let (!!) = Options.(!!)

let file_first_name f = f.file_name

let string_of_file_state state =
  match state with
  | FileDownloading -> (gettext M.downloading)
  | FileCancelled -> (gettext M.cancelled)
  | FileQueued -> (gettext M.queued)
  | FilePaused -> (gettext M.paused)
  | FileDownloaded
  | FileShared  -> (gettext M.dl_done)
  | FileNew -> assert false
  | FileAborted s -> Printf.sprintf "Aborted: %s" s

      
      
let some_is_available f =
  let b = ref false in
  
  List.iter (fun (_, avail) ->
(* We cannot use anymore relative availability as we cannot relate the 
  partial chunks with positions in the availability...
      if !!Gui_options.use_relative_availability
      then
        for i = 0 to String.length avail - 1 do
          if CommonGlobals.partial_chunk f.file_chunks.[i] &&
            f.file_availability.[i] <> '\000'
          then true
          else loop (i - 1)
        in
        loop ((String.length f.file_availability) - 1)
else
  *)
      let len = String.length avail in
      for i = 0 to len - 1 do
        b := !b || avail.[i] <> '\000'
      done;
  ) f.file_availability;
        !b

  
let color_opt_of_file f =
  if f.file_download_rate > 0. then
    Some !!O.color_downloading
  else if some_is_available f then
    Some !!O.color_available
  else
    Some !!O.color_not_available

let float_avail s = 
  try float_of_string s
  with _ -> 0.0

let file_availability f =
  match f.file_chunks with
  | None -> "---"
  | Some chunks ->
      match f.file_availability with
        (_,avail) :: _ ->
          let rec loop i p n =
            if i < 0 then
              if n = 0 then "---"
              else Printf.sprintf "%5.1f" ((float p) /. (float n) *. 100.0)
            else 
              loop (i - 1)
                (if CommonGlobals.partial_chunk (VerificationBitmap.get chunks i) then p + 1 else p)
                (if avail.[i] <> (char_of_int 0) then n + 1 else n)
          in
          loop ((String.length avail) - 1) 0 0
      | _ -> "---"

let string_availability f =
  let maxi = ref 0. in
  List.iter (fun (_, avail) ->
      let len = String.length avail in
      let p = ref 0 in
      for i = 0 to len - 1 do
        if avail.[i] <> '\000' then begin
            incr p
          end
      done;
      maxi := max !maxi (float_of_int !p /. float_of_int len *. 100.)
  ) f.file_availability;
  Printf.sprintf "%5.1f" !maxi

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
  | _ -> (gettext M.unknown)

let max_eta = 1000.0 *. 60.0 *. 60.0 *. 24.0
    
let calc_file_eta f =
  let size = Int64.to_float f.file_size in
  let downloaded = Int64.to_float f.file_downloaded in
  let missing = size -. downloaded in
  let rate = f.file_download_rate in
  let rate =
    if rate = 0.
    then
      let time = BasicSocket.last_time () in
      let age = time - f.file_age in
      if age > 0
      then downloaded /. (float_of_int age)
      else 0.
    else rate
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
    inherit [file_info] Gpattern.plist sel_mode titles true (fun f -> f.file_num) as pl
      inherit Gui_downloads_base.box () as box
    
    
    val mutable columns = columns
    
    method set_columns l =
      columns <- l;
      self#set_titles (List.map Gui_columns.File.string_of_column !!columns);
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
            ) Gui_columns.file_column_strings));
        `M ("Add Column Before", (
            List.map (fun (c,s) ->
                (`I (s, (fun _ -> 
                        let c1, c2 = List2.cut i !!columns in
                        columns =:= c1 @ [c] @ c2;
                        self#set_columns columns
                    )))
            ) Gui_columns.file_column_strings));
      ]
    
    
    method box = box#coerce
    method vbox = box#vbox
    
    method compare_by_col col f1 f2 =
      match col with
      | Col_file_name -> compare f1.file_name f2.file_name
      |	Col_file_size -> compare f1.file_size f2.file_size
      |	Col_file_downloaded -> compare f1.file_downloaded f2.file_downloaded
      |	Col_file_percent -> compare 
            (Int64.to_float f1.file_downloaded /. Int64.to_float f1.file_size) 
          (Int64.to_float f2.file_downloaded /. Int64.to_float f2.file_size) 
      |	Col_file_rate-> compare f1.file_download_rate f2.file_download_rate
      |	Col_file_state -> compare f1.file_state f2.file_state
      |	Col_file_availability ->
          let fn =
            if !!Gui_options.use_relative_availability
            then file_availability
            else string_availability
          in
          compare (float_avail (fn f1)) (float_avail (fn f2))
      |	Col_file_md4 -> compare (Md4.to_string f1.file_md4) (Md4.to_string f2.file_md4)
      |	Col_file_format -> compare f1.file_format f2.file_format
      | Col_file_network -> compare f1.file_network f2.file_network
      |	Col_file_age -> compare f1.file_age f2.file_age
      |	Col_file_last_seen -> compare f1.file_last_seen f2.file_last_seen
      | Col_file_eta -> compare (calc_file_eta f1) (calc_file_eta f2)
      |	Col_file_priority -> compare f1.file_priority f2.file_priority
    
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
        Col_file_name -> 
          let s_file = Gui_misc.short_name f.file_name in
          s_file
      |	Col_file_size ->
          Gui_misc.size_of_int64 f.file_size 
      |	Col_file_downloaded ->
          Gui_misc.size_of_int64 f.file_downloaded
      |	Col_file_percent ->
          Printf.sprintf "%5.1f" 
            (Int64.to_float f.file_downloaded /. Int64.to_float f.file_size *. 100.)
      |	Col_file_rate ->
          if f.file_download_rate > 0. then
            Printf.sprintf "%5.1f" (f.file_download_rate /. 1024.)
          else ""
      |	Col_file_state ->
          string_of_file_state f.file_state 
      |	Col_file_availability ->
          if !!Gui_options.use_relative_availability
          then file_availability f
          else string_availability f
      | Col_file_md4 -> Md4.to_string f.file_md4
      | Col_file_format -> string_of_format f.file_format
      | Col_file_network -> Gui_global.network_name f.file_network
      |	Col_file_age ->
          let age = (BasicSocket.last_time ()) - f.file_age in
          Date.time_to_string age "long"
      |	Col_file_last_seen ->
          if f.file_last_seen > 0
          then let last = (BasicSocket.last_time ())
              - f.file_last_seen in
            Date.time_to_string last "long"
          else Printf.sprintf "---"
      | Col_file_eta ->
          let eta = calc_file_eta f in
          if eta >= 1000 * 60 * 60 * 24 then
            Printf.sprintf "---"
          else Date.time_to_string eta "long"
      |	Col_file_priority ->
          Printf.sprintf "%3d" f.file_priority
    
    method content f =
      let strings = List.map 
          (fun col -> P.String (self#content_by_col f col))
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
      self#remove_item row f;
      selection <- List.filter (fun fi -> fi.file_num <> f.file_num) selection
    
    method set_tb_style tb = 
      if Options.(!!) Gui_options.mini_toolbars then
        (wtool1#misc#hide (); wtool2#misc#show ()) else
        (wtool2#misc#hide (); wtool1#misc#show ());
      wtool2#set_style tb;
      wtool1#set_style tb
    
    initializer
      box#vbox#pack ~expand: true pl#box;


end

class box_downloaded wl_status () =
  object (self)
    inherit box O.downloaded_columns `SINGLE () as super
    
    method update_wl_status : unit =
      wl_status#set_text 
        (Printf.sprintf !!Gui_messages.downloaded_files !G.ndownloaded !G.ndownloads)
    
    method content f = 
      (fst (super#content f), Some (`NAME !!O.color_downloaded))
    
    method save ev =
      match self#selection with
        f :: _ ->
          let items = save_menu_items f in
          GAutoconf.popup_menu ~entries: items ~button: 1 ~time: 0
      |	_ ->
          ()
    
    method save_all () = 
      self#iter	(fun f -> 
          Gui_com.send (GuiProto.SaveFile (f.file_num, file_first_name f)))
    
    method edit_mp3_tags () = 
      match self#selection with
        file :: _ ->
          (
            match file.file_format with
              MP3 (tag,_) ->
                Mp3_ui.edit_tag_v1 (gettext M.edit_mp3) tag ;
                Gui_com.send (GuiProto.ModifyMp3Tags (file.file_num, tag))
            | _ ->
                ()
          )
      |	_ ->
          ()
    
    method menu =
      match self#selection with
        [] -> [ `I ((gettext M.save_all), self#save_all) ]
      |	file :: _ ->
          [ 
            `I ((gettext M.save_as), save_as file) ;
            `M ((gettext M.save), save_menu_items file) ;
            `I ((gettext M.preview), preview file) ;
          ] @
            (match file.file_format with
              MP3 _ -> [`I ((gettext M.edit_mp3), self#edit_mp3_tags)]
            | _ -> []) @
            [ `S ;
            `I ((gettext M.save_all), self#save_all) 
          ]

(** {2 Handling core messages} *)
    
    method h_downloaded f =
      try
        ignore (self#find_file f.file_num)
      with
        Not_found ->
          incr ndownloaded;
          self#update_wl_status ;
          self#add_item f
    
    method h_removed f =
      try
        let (row, _) = self#find_file f.file_num in
        decr ndownloaded;
        self#update_wl_status ;
        self#remove_file f row
      with
        Not_found ->
          ()
    
    method preview () =
      match self#selection with
        [] -> ()
      | file :: _ -> preview file ()
    
    method save_as () = 
      match self#selection with 
      | [] -> ()
      | file :: _ -> save_as file ()
    
    initializer
      Gui_misc.insert_buttons wtool1 wtool2 
        ~text: (gettext M.save)
      ~tooltip: (gettext M.save)
      ~icon: (M.o_xpm_save)
      ~callback: self#save
        ();
      
      Gui_misc.insert_buttons wtool1 wtool2 
        ~text: (gettext M.save_as)
      ~tooltip: (gettext M.save_as)
      ~icon: (M.o_xpm_save_as)
      ~callback: self#save_as
        ();
      
      Gui_misc.insert_buttons wtool1 wtool2 
        ~text: (gettext M.save_all)
      ~tooltip: (gettext M.save_all)
      ~icon: (M.o_xpm_save_all)
      ~callback: self#save_all
        ();
      
      Gui_misc.insert_buttons wtool1 wtool2 
        ~text: (gettext M.edit_mp3)
      ~tooltip: (gettext M.edit_mp3)
      ~icon: (M.o_xpm_edit_mp3)
      ~callback: self#edit_mp3_tags
        ();
      
      
      Gui_misc.insert_buttons wtool1 wtool2 
        ~text: (gettext M.preview)
      ~tooltip: (gettext M.preview)
      ~icon: (M.o_xpm_preview)
      ~callback: self#preview
        ();
      
      
end

let colorGreen = `NAME "green"
let colorRed   = `NAME "red"
let colorBlue  = `NAME "blue"
let colorGray  = `NAME "gray"
let colorWhite =`WHITE
let colorBlack = `BLACK
let colorCyan = `NAME "cyan"

let drawing = ref (None :   [ `window] GDraw.drawable option)

  
let draw_chunks (drawing : unit GDraw.drawable) file =
  match file.file_chunks with
  | None -> ()
  | Some chunks ->
      let wx, wy = drawing#size in
      drawing#set_foreground colorWhite;
      drawing#rectangle ~filled: true ~x:0 ~y:0 ~width:wx ~height:wy ();
      let nchunks = VerificationBitmap.length chunks in
      let dx = min !!O.chunk_width (wx / nchunks) in
  
      if wx > nchunks*dx && dx > 0 then
    
        let offset = (wx - nchunks * dx) / 2 in
        let offset = if offset < 0 then 0 else offset in
        let dx2 = if dx <= 2 then dx else dx - 1 in
        for i = 0 to nchunks - 1 do
          drawing#set_foreground (
            match VerificationBitmap.get chunks i with
            | VerificationBitmap.State_missing -> colorRed
            | VerificationBitmap.State_partial -> colorBlue
            | VerificationBitmap.State_complete -> colorBlack
            | VerificationBitmap.State_verified -> colorGreen);
          drawing#rectangle ~filled: true
            ~x:(offset + i*dx) ~y: 0 
            ~width: dx2 ~height:wy ()
        done          
      else
  
        let group = (nchunks+wx-1) / wx in
        let chunk n =
          let p = n * group in
          let get i = 
            if i < nchunks then VerificationBitmap.get chunks i
            else VerificationBitmap.State_complete
          in
          let current = ref 0 in
          for i = p to p+group-1 do
            match get i with
            | VB.State_missing -> ()
            | VB.State_partial -> if !current = 0 then current := 1
            | VB.State_complete | VB.State_verified ->
                current := 2
          done;
          String.make 1 (char_of_int !current)
        in
        let dx = 1 in
        let nchunks = nchunks / group in  
  
        let offset = (wx - nchunks * dx) / 2 in
        let offset = if offset < 0 then 0 else offset in
        let dx2 = if dx <= 2 then dx else dx - 1 in
        for i = 0 to nchunks - 1 do
          let chunk = chunk i in
          drawing#set_foreground (
            match VerificationBitmap.get chunks i with
            | VerificationBitmap.State_missing -> colorRed
            | VerificationBitmap.State_partial -> colorBlue
            | VerificationBitmap.State_complete -> colorBlack
            | VerificationBitmap.State_verified -> colorGreen);
          drawing#rectangle ~filled: true
            ~x:(offset + i*dx) ~y: 0 
            ~width: dx2 ~height:wy ()
        done
  
let draw_availability (drawing: unit GDraw.drawable) availability =
  let wx, wy = drawing#size in
  drawing#set_foreground colorWhite;
  drawing#rectangle ~filled: true ~x:0 ~y:0 ~width:wx ~height:wy ();
  let nchunks = String.length availability in
  let dx = min !!O.chunk_width (wx / nchunks) in
  
  if wx > nchunks*dx && dx > 0 then
    
    let offset = (wx - nchunks * dx) / 2 in
    let offset = if offset < 0 then 0 else offset in
    let dx2 = if dx <= 2 then dx else dx - 1 in
    for i = 0 to nchunks - 1 do
      if !!Gui_options.use_availability_height
      then begin
          let h = int_of_char (availability.[i]) 
          in
          if h = 0
          then
            begin
              drawing#set_foreground colorRed;
              drawing#rectangle ~filled: true
              ~x:(offset + i*dx) ~y: 0
                ~width: dx2 ~height:wy ();
            end
          else begin
              let h = min ((wy * h) / !!Gui_options.availability_max) wy in
              drawing#set_foreground colorGray;
              drawing#rectangle ~filled: true
              ~x:(offset + i*dx) ~y: 0
                ~width: dx2 ~height: (wy - h) ();
              
              drawing#set_foreground colorBlue;
              drawing#rectangle ~filled: true
              ~x:(offset + i*dx) ~y: (wy - h)
              ~width: dx2 ~height:h ();
            end
        end else begin
          drawing#set_foreground (
            match int_of_char availability.[i]  with
              0 -> colorRed
            | 1 -> colorBlue
            | _ -> colorBlack);
          drawing#rectangle ~filled: true
          ~x:(offset + i*dx) ~y: 0 
            ~width: dx2 ~height:wy ()
        end
    done          
  else
  
  let group = (nchunks+wx-1) / wx in
  let avail i =
    let p = i * group in
    let get i = 
      if i < nchunks then availability.[i] else '\200'
    in
    let current = ref (get p) in
    for i = p+1 to p+group-1 do
      current := min (get i) !current
    done;
    !current
  in
  let dx = 1 in
  let nchunks = nchunks / group in  
  
  let offset = (wx - nchunks * dx) / 2 in
  let offset = if offset < 0 then 0 else offset in
  let dx2 = if dx <= 2 then dx else dx - 1 in
  for i = 0 to nchunks - 1 do
    let avail = avail i in
    if !!Gui_options.use_availability_height
    then begin
        let h = int_of_char avail
        in
        if h = 0
        then
          begin
            drawing#set_foreground colorRed;
            drawing#rectangle ~filled: true
            ~x:(offset + i*dx) ~y: 0
              ~width: dx2 ~height:wy ();
          end
        else begin
            let h = min ((wy * h) / !!Gui_options.availability_max) wy in
            drawing#set_foreground colorGray;
            drawing#rectangle ~filled: true
            ~x:(offset + i*dx) ~y: 0
              ~width: dx2 ~height: (wy - h) ();
            
            drawing#set_foreground colorBlue;
            drawing#rectangle ~filled: true
            ~x:(offset + i*dx) ~y: (wy - h)
            ~width: dx2 ~height:h ();
          end
      end else begin
        drawing#set_foreground (
          match int_of_char avail with
            0 -> colorRed
          | 1 -> colorBlue
          | _ -> colorBlack);
        drawing#rectangle ~filled: true
        ~x:(offset + i*dx) ~y: 0 
          ~width: dx2 ~height:wy ()
      end
  done
  
let redraw_chunks display_caption draw_avail view_availability file =
  let (drawing : unit GDraw.drawable) = match !drawing with
      None ->         
        let w = draw_avail#misc#window in
        let d = new GDraw.drawable w in
        draw_avail#misc#show ();
        drawing := Some d;
        d        
    | Some d -> d
  in
  try
    if view_availability = 0 then begin
        display_caption#set_text "Downloaded Chunks";
        draw_chunks drawing file
      end else
    let (net, a) = 
      List.nth file.file_availability (view_availability-1) in
    display_caption#set_text (Printf.sprintf "Availability on %s" 
      (try (Hashtbl.find networks net).net_name with _ -> "??"));
    draw_availability drawing a
  with e ->
      Printf2.lprintf "Exception %s during drawing\n"
        (Printexc2.to_string e)

class box_downloads box_locs wl_status () =
  let draw_availability =
    GMisc.drawing_area ~height:20
      ()
  in
  let display_caption =
    GMisc.label ~text:"Current chunks" ~justify:`LEFT ~line_wrap:true
    ~xalign:(-1.0) ~yalign:(-1.0) ()
  in
  
  let label_file_info = GMisc.label () in
  object (self)
    inherit box O.downloads_columns `EXTENDED ()
    
    method update_wl_status : unit =
      wl_status#set_text 
        (Printf.sprintf !!Gui_messages.downloaded_files !G.ndownloaded !G.ndownloads)
    
    method cancel () =
      let s = Gui_messages.ask_cancel_download_files
          (List.map (fun f -> file_first_name f) self#selection)
      in
      match GToolbox.question_box (gettext Gui_messages.cancel)
        [ gettext Gui_messages.yes ; gettext Gui_messages.no] s 
      with
        1 ->
          List.iter
            (fun f -> Gui_com.send (RemoveDownload_query f.file_num))
          self#selection
      |	_ ->
          ()
    
    method retry_connect () =
      List.iter
        (fun f -> Gui_com.send (ConnectAll f.file_num))
      self#selection
    
    method pause_resume () =
      List.iter
        (fun f -> Gui_com.send (SwitchDownload (f.file_num,
              match f.file_state with
                FilePaused | FileAborted _ -> true
              | _ -> false
            )))
      self#selection
    
    method verify_chunks () =
      List.iter
        (fun f -> Gui_com.send (VerifyAllChunks f.file_num))
      self#selection
    
    method get_format () =
      List.iter
        (fun f -> Gui_com.send (QueryFormat f.file_num))
      self#selection
    
    method set_priority prio () =
      List.iter
        (fun f -> Gui_com.send (SetFilePriority (f.file_num, prio)))
      self#selection
    
    method menu =
      match self#selection with
        [] -> []
      |	file :: tail ->
          `I ((gettext M.pause_resume_dl), self#pause_resume) ::
          `I ((gettext M.retry_connect), self#retry_connect) ::
          `I ((gettext M.cancel), self#cancel) ::
          `I ((gettext M.verify_chunks), self#verify_chunks) ::
          `M ("Set priority", [
              `I ("High", self#set_priority 10);
              `I ("Normal", self#set_priority 0);
              `I ("Low", self#set_priority (-10));
            
            ]) ::
          `I ((gettext M.get_format), self#get_format) ::
          (if tail = [] then
              [
                `I ((gettext M.preview), preview file) ;
                `M ((gettext M.save), save_menu_items file) ;
                `I ((gettext M.save_as), save_as file) ;
              ]
            else  [])

(* 0 = current chunks, 1-n availability *)
    val mutable view_availability = 0
    val mutable last_displayed_file = None
    val mutable label_shown = false
    
    method on_select file =
      if not label_shown then begin
          label_shown <- true;
          let hbox =  GPack.hbox ~homogeneous:false () in
          let button =  GButton.button () in
          self#vbox#pack ~expand: false ~fill: true label_file_info#coerce ;
          self#vbox#pack ~expand:false ~fill:true hbox#coerce;
          hbox#pack ~expand:false ~fill:false button#coerce;
          hbox#add draw_availability#coerce;
          button#add display_caption#coerce;
          ignore (button#connect#clicked (fun _ ->
              match last_displayed_file with
                Some file ->
                    let n = view_availability + 1 in
                    (* lprintf "view_availability %d/%d\n" n
                      (List.length file.file_availability); *)
                    view_availability <- (if
                      n > List.length file.file_availability 
                    then 0 else n);
                  redraw_chunks display_caption draw_availability view_availability file;
              | _ -> ()));
        end;
      label_file_info#set_text 
        (
        Printf.sprintf "NAME: %s SIZE: %s FORMAT: %s" 
          (file.file_name)
        (Int64.to_string file.file_size) 
        (string_of_format file.file_format)
        ;
      );
      view_availability <- 0;
      redraw_chunks display_caption draw_availability view_availability file;
      last_displayed_file <- Some file;
      box_locs#update_data_by_file (Some file)
    
    method on_deselect _ =
      box_locs#update_data_by_file None



(** {2 Handling core messages} *)
    
    method update_file f f_new row =
      f.file_md4 <- f_new.file_md4 ;
      f.file_name <- f_new.file_name ;
      f.file_names <- f_new.file_names ;
      f.file_size <- f_new.file_size ;
      f.file_downloaded <- f_new.file_downloaded ;
      f.file_all_sources <- f_new.file_all_sources ;
      f.file_active_sources <- f_new.file_active_sources ;
      f.file_state <- f_new.file_state ;
      let must_redraw = 
        (f.file_chunks <> f_new.file_chunks) || 
        (f.file_availability <> f_new.file_availability)
      in
      f.file_chunks <- f_new.file_chunks ;
      f.file_availability <- f_new.file_availability ;
      f.file_priority <- f_new.file_priority;
      if f_new.file_sources <> None then
        f.file_sources <- f_new.file_sources ;
      f.file_download_rate <- f_new.file_download_rate ;
      f.file_format <- f_new.file_format;
      f.file_age <- f_new.file_age;
      f.file_last_seen <- f_new.file_last_seen;
      self#update_row f row;
      if must_redraw then 
        match last_displayed_file with 
        | Some file when f == file ->
            redraw_chunks display_caption draw_availability view_availability file;
        | _ -> ()
    
    method h_file_downloaded num dled rate =
      try
        let (row, f) = self#find_file num in
        self#update_file f
          { f with file_downloaded = dled ; file_download_rate = rate } 
          row
      with
        Not_found ->
          ()
    
    method h_paused f =
      try
        let (row, fi) = self#find_file f.file_num in
        self#update_file fi f row
      with
        Not_found ->
          incr ndownloads;
          self#update_wl_status ;
          self#add_item f
    
    method h_cancelled f = 
      try
        let (row, fi) = self#find_file f.file_num in
        decr ndownloads;
        self#update_wl_status ;
        self#remove_file fi row
      with
        Not_found ->
          ()
    
    method h_downloaded = self#h_cancelled 
    method h_downloading f = self#h_paused f
    
    method h_file_availability file_num client_num avail =      
      try
        let files = Intmap.find client_num !availabilities in
        try
          let (s,file) = Intmap.find file_num !files in
          String.blit avail 0 s 0 (String.length avail);
        with _ ->
            let _, file = self#find_file file_num in
            files := Intmap.add file_num (avail,file) !files
      with _ ->
          let _, file = self#find_file file_num in          
          availabilities := Intmap.add client_num
            (ref (Intmap.add file_num (avail,file) Intmap.empty)) !availabilities
(*
      try
        let (row, f) = self#find_file file_num in
        self#update_file f 
          { f with file_chunks = chunks ; file_availability = avail } 
          row
      with Not_found -> ()
*)
(*
    method remove_client c =
      List.iter (fun file ->
          match file.file_sources with
            None -> ()
          | Some sources ->
              if List.memq c sources then
                let (row, file) = self#find_file file.file_num in
(*                lprintf "Removing client from sources";
                lprint_newline (); *)
                self#update_file file { file with file_sources = Some (
                    List.filter (fun cc -> cc != c) sources) } 
                row
      ) data
*)
    
    method h_file_age num age =
      try
        let (row, f) = self#find_file num in
        self#update_file f 
          { f with file_age = age } 
          row
      with Not_found -> ()
    
    method h_file_last_seen num last =
      try
        let (row, f) = self#find_file num in
        self#update_file f 
          { f with file_last_seen = last } 
          row
      with Not_found -> ()
    
    method h_file_location num src =
      try
(*        lprintf "Source %d for %d" src num;  lprint_newline (); *)
        let (row, f) = self#find_file num in
        self#update_file f 
          { f with 
          file_sources = Some (
            (match f.file_sources with
                None -> [src]
              | Some list -> if not (List.mem src list) then
                    list@[src] else list ))
        } 
          row
      with Not_found -> 
(* some sources are sent for shared files in eDonkey. have to fix that *)
(*          lprintf "No such file %d" num; lprint_newline () *)
          ()
    
    method h_file_remove_location (num:int) (src:int) = 
      try
(*        lprintf "Source %d for %d" src num;  lprint_newline (); *)
        let (row, f) = self#find_file num in
        match f.file_sources with
          None -> ()
        | Some list ->
            if List.memq src list then
              self#update_file f { f with 
                file_sources = Some (List.filter 
                    (fun s -> s<>src)
                list) 
        } 
          row
      with Not_found -> 
(* some sources are sent for shared files in eDonkey. have to fix that *)
(*          lprintf "No such file %d" num; lprint_newline () *)
          ()
    
    method clean_table list = 
      let set = ref Intset.empty in
      List.iter (fun c_num ->
          set := Intset.add c_num !set
      ) list;
      self#iter (fun file ->
          match file.file_sources with
            None -> ()
          | Some sources ->
              file.file_sources <- Some (List.filter (fun c ->
                  Intset.mem c !set) sources)
      );
      let all = Hashtbl2.to_list locations in 
      Hashtbl.clear locations;
      List.iter (fun c ->
          if Intset.mem c.client_num !set then
            Hashtbl.add locations c.client_num c
      ) all
    
    method preview () =
      match self#selection with
        [] -> ()
      | file :: _ -> preview file ()
    
    initializer

        Gui_misc.insert_buttons wtool1 wtool2 
          ~text: (gettext M.cancel)
        ~tooltip: (gettext M.cancel)
      ~icon: (M.o_xpm_cancel)
          ~callback: self#cancel
          ();

      Gui_misc.insert_buttons wtool1 wtool2 
          ~text: (gettext M.retry_connect)
        ~tooltip: (gettext M.retry_connect)
        ~icon: (M.o_xpm_retry_connect)
          ~callback: self#retry_connect
          ();

        Gui_misc.insert_buttons wtool1 wtool2 
          ~text: (gettext M.pause_resume_dl)
        ~tooltip: (gettext M.pause_resume_dl)
        ~icon: (M.o_xpm_pause_resume)
          ~callback: self#pause_resume
          ();

        Gui_misc.insert_buttons wtool1 wtool2 
          ~text: (gettext M.verify_chunks)
        ~tooltip: (gettext M.verify_chunks)
      ~icon: (M.o_xpm_verify_chunks)
          ~callback: self#verify_chunks
          ();

        Gui_misc.insert_buttons wtool1 wtool2 
          ~text: (gettext M.preview)
        ~tooltip: (gettext M.preview)
        ~icon: (M.o_xpm_preview)
          ~callback: self#preview
          ();

        Gui_misc.insert_buttons wtool1 wtool2 
          ~text: (gettext M.get_format)
        ~tooltip: (gettext M.get_format)
        ~icon: (M.o_xpm_get_format)
          ~callback: self#get_format
          ();

      ignore (draw_availability#event#connect#expose (fun _ ->
            match last_displayed_file with
              None -> true
            | Some file -> 
                redraw_chunks display_caption draw_availability view_availability file; true
        ));

end

class pane_downloads () =
  let wl_status = GMisc.label ~text: "" ~show: true () in
  let scroll_box = GPack.vbox () in
  let scroll = GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~placement:`TOP_LEFT ~packing:scroll_box#add () in
  let client_info_box = GPack.vbox ~packing:scroll#add_with_viewport () in
  let locs = new Gui_friends.box_list client_info_box false in
  let dled = new box_downloaded wl_status () in
  let dls = new box_downloads locs wl_status () in
  let downloads_frame =
    GBin.frame ~border_width:5 ~label:"Running Downloads" ~label_xalign:(-1.0)
    ~label_yalign:(-1.0) ~shadow_type:`ETCHED_OUT ()
  in
  let downloaded_frame =
    GBin.frame ~border_width:5 ~label:"Completed Downloads"
      ~label_xalign:(-1.0) ~label_yalign:(-1.0) ~shadow_type:`ETCHED_OUT
      ()
  in
  
  object (self)
    inherit Gui_downloads_base.paned ()

    method wl_status = wl_status
    method box_downloads = dls
    method box_downloaded = dled
    method box_locations = locs

    method set_tb_style st =
      locs#set_tb_style st;
      dled#set_tb_style st;
      dls#set_tb_style st

    method clear =
      wl_status#set_text "";
      locs#clear;
      dled#clear;
      dls#clear

    (** {2 Handling core messages} *)

    method h_file_info f = 
      match f.file_state with
        FileNew -> assert false
      | FileCancelled -> 
          dls#h_cancelled f
      |	FileDownloaded ->
          dls#h_cancelled f;
          dled#h_downloaded f
      |	FileShared -> 
          dled#h_removed f
      |	FilePaused | FileQueued | FileAborted _ -> 
          dls#h_paused f
      | FileDownloading ->
          dls#h_downloading f

    method h_file_availability = dls#h_file_availability
    method h_file_age = dls#h_file_age
    method h_file_last_seen = dls#h_file_last_seen
    method h_file_downloaded = dls#h_file_downloaded
    method h_file_location = dls#h_file_location
    method h_file_remove_location = dls#h_file_remove_location

    method h_update_location c_new =
      locs#h_update_location c_new

(*    method h_remove_client c = dls#remove_client c *)

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


      
      clients_wpane#add1 locs#coerce;
      clients_wpane#add2 scroll_box#coerce;
      downloaded_frame#add dled#coerce; 
      downloads_frame#add dls#coerce ;
      if !!Gui_options.downloads_up then  begin
          vpaned#add1 downloads_frame#coerce;
          vpaned#add2 downloaded_frame#coerce; 
        end else begin
          vpaned#add2 downloads_frame#coerce;
          vpaned#add1 downloaded_frame#coerce;
        end
  end
